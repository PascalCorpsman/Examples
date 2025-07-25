Unit delforengine;

(*
 * Modified by Corpsman, to Support Lazarus Linux. 01.09.2009
 * Modified by Corpsman, to Support Class, Recordd, Type Helper 07.10.2020
 * Modified by Corpsman, add rudimentary support for C-Operands 04.04.2021
 * Modified by Corpsman, Fix Crash, when Expression is nil 04.04.2021
 * Modified by Corpsman, to Support Identifier Caseing 21.04.2021
 * Modified by Corpsman, to Support "deprecated" as Key word 21.09.2021
 * Modified by Corpsman, Fix Crash, when prefix spaces are to long 16.11.2021
 *)

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

Interface

Uses SysUtils, oobjects, Classes, delfortypes;

(*
WISHLIST:
- suppress read-only file message
- Extra indent in procedures (brr)
- extra indent in implementation part (brr)
- read capitalization from var const type blocks
- sorting methods
- Is it possible to insert a "user customisable" line or group of lines before each
function/procedure, to allow the developer to comment it. Ex :

{------------Name of the proc------------------------}  (Simple)

{***************************
 * ...Comment ...
 * ...Comment ...
 ***************************/ (A few lines)}

 *)

Type

  TPascalWord = Class
  private
    Procedure SetCase(ACase: TCase); virtual;
    Function GetCase: TCase; virtual;
    Procedure SetExpression(AExpression: PChar); virtual;
    Function GetExpression: PChar; virtual;
  public
    Constructor Create;
    Function WordType: TWordType; virtual;
    Procedure GetLength(Var Length: Integer); virtual;
    Function Space(SpaceBefore: TSpaceBefore): Boolean; virtual;
    Function ReservedType: TReservedType; virtual;
    Procedure SetSpace(SpaceBefore: TSpaceBefore; State: Boolean); virtual;
    Procedure SetReservedType(AReservedType: TReservedType); virtual;
    Function GetEString(Dest: PChar): PChar; virtual; abstract;
    Function ChangeComment(commchar: char): boolean;
    Property Expression: PChar read GetExpression write SetExpression;
    Property ExpressionCase: TCase read GetCase write SetCase;
  End;

  TLineFeed = Class(TPascalWord)
  public
    nSpaces: Integer;
    oldnSpaces: Integer;
    wrapped: Boolean;
    Constructor Create(AOldnSpaces: Integer);
    Procedure SetIndent(n: Integer);
    Procedure IncIndent(n: Integer);
    Procedure GetLength(Var Length: Integer); override;
    Function ReservedType: TReservedType; override;
    Function GetEString(Dest: PChar): PChar; override;
  End;

  { TExpression }

  TExpression = Class(TPascalWord)
  private
    Procedure SetCase(ACase: TCase); override;
    Function GetCase: TCase; override;
    Procedure SetExpression(AExpression: PChar); override;
  public
    FExpression: PChar;
    FWordType: TWordType;
    FFormatType: Byte;
    FReservedType: TReservedType;
    Constructor Create(AType: TWordType; AExpression: PChar);
    Procedure CheckReserved;
    Procedure SetSpace(SpaceBefore: TSpaceBefore; State: Boolean); override;
    Procedure SetReservedType(AReservedType: TReservedType); override;
    Function Space(SpaceBefore: TSpaceBefore): Boolean; override;
    Function GetEString(Dest: PChar): PChar; override;
    Procedure GetLength(Var Length: Integer); override;
    Function GetExpression: PChar; override;
    Function WordType: TWordType; override;
    Function ReservedType: TReservedType; override;
    Destructor Destroy; override;
  End;

  TAlignExpression = Class(TExpression)
  public
    AlignPos: Byte;
    nSpaces: Byte;
    Constructor Create(Like: TExpression; Pos: Byte);
    Procedure GetLength(Var Length: Integer); override;
    Function GetEString(Dest: PChar): PChar; override;
  End;
Type

  TDelforParser = Class(TObject)
  private
    FSettings: TSettings;
    FileText: TOCollection;
    FCurrentText: PChar;
    FCapNames: TKeywordColl;
    nIndent: Integer;
    ProcLevel: Integer;
    ReadingAsm: Boolean;
    AsmComment: TWordType;
    prev: TPascalWord;
    PrevLine: TLineFeed;
    prevType: TWordType;
    FOnProgress: TProgressEvent;
    HasAligned: Boolean;
    LeftPointBracket: Integer;
    Procedure SetFillNewWords(AFillNewWords: TFillMode);
    Function AlignExpression(I: Integer; aPos: Integer): TPascalWord;
    Procedure checkPrintable(P: PChar);
    Procedure ReadAsm(Var Buff: PChar);
    Function ReadHalfComment(Dest: PChar; Var Source: PChar): TWordType;
    Function ReadWord(Dest: PChar; Var Source: PChar): TWordType;
    Procedure SetTextStr(AText: PChar);
    Function GetTextStr: PChar;
    Procedure CheckWrapping;
    Function GetString(Dest: PChar; Var I: Integer): PChar;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure LoadFromFile(AFileName: PChar);
    Procedure LoadFromList(AList: TStringList);
    Procedure LoadCapFile(ACapFile: PChar);
    Procedure SaveCapFile(ACapFile: PChar);
    Function Parse: Boolean;
    Procedure Clear;
    Procedure WriteToFile(AFileName: PChar);
    Procedure Add(Buff: PChar);
    Property Text: PChar read GetTextStr write SetTextStr;
    Property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    Property CapNames: TKeywordColl read FCapNames write FCapNames;
    Property Settings: TSettings read FSettings write FSettings;
    Property SpacePerIndent: Integer read FSettings.SpacePerIndent write
      FSettings.SpacePerIndent;
    Property SpaceOperators: TSpaceBefore read FSettings.SpaceOperators write
      FSettings.SpaceOperators;
    Property SpaceEqualOper: TSpaceBefore read FSettings.SpaceEqualOper write
      FSettings.SpaceEqualOper;
    Property SpaceColon: TSpaceBefore read FSettings.SpaceColon write
      FSettings.SpaceColon;
    Property SpaceComma: TSpaceBefore read FSettings.SpaceComma write
      FSettings.SpaceComma;
    Property SpaceSemiColon: TSpaceBefore read FSettings.SpaceSemiColon write
      FSettings.SpaceSemiColon;
    Property SpaceLeftBr: TSpaceBefore read FSettings.SpaceLeftBr write
      FSettings.SpaceLeftBr;
    Property SpaceRightBr: TSpaceBefore read FSettings.SpaceRightBr write
      FSettings.SpaceRightBr;
    Property SpaceLeftHook: TSpaceBefore read FSettings.SpaceLeftHook write
      FSettings.SpaceLeftHook;
    Property SpaceRightHook: TSpaceBefore read FSettings.SpaceRightHook write
      FSettings.SpaceRightHook;
    Property UpperCompDirectives: Boolean read FSettings.UpperCompDirectives
      write FSettings.UpperCompDirectives;
    Property UpperNumbers: Boolean read FSettings.UpperNumbers write
      FSettings.UpperNumbers;
    Property ReservedCase: TCase read FSettings.ReservedCase write
      FSettings.ReservedCase;
    Property StandDirectivesCase: TCase read FSettings.StandDirectivesCase write
      FSettings.StandDirectivesCase;
    Property ChangeIndent: Boolean read FSettings.ChangeIndent write
      FSettings.ChangeIndent;
    Property indentBegin: Boolean read FSettings.indentBegin write
      FSettings.indentBegin;
    Property NoIndentElseIf: Boolean read FSettings.NoIndentElseIf write
      FSettings.NoIndentElseIf;
    Property IndentComments: Boolean read FSettings.IndentComments write
      FSettings.IndentComments;
    Property IndentCompDirectives: Boolean read FSettings.IndentCompDirectives
      write FSettings.IndentCompDirectives;
    Property IndentTry: Boolean read FSettings.IndentTry write
      FSettings.IndentTry;
    Property IndentTryElse: Boolean read FSettings.IndentTryElse write
      FSettings.IndentTryElse;
    Property IndentCaseElse: Boolean read FSettings.IndentCaseElse write
      FSettings.IndentCaseElse;
    Property BlankProc: Boolean read FSettings.BlankProc write
      FSettings.BlankProc;
    Property RemoveDoubleBlank: Boolean read FSettings.RemoveDoubleBlank write
      FSettings.RemoveDoubleBlank;
    Property FeedRoundBegin: TFeedBegin read FSettings.FeedRoundBegin write
      FSettings.FeedRoundBegin;
    Property FeedAfterThen: Boolean read FSettings.FeedAfterThen write
      FSettings.FeedAfterThen;
    Property ExceptSingle: Boolean read FSettings.FeedAfterThen write
      FSettings.ExceptSingle;
    Property NoFeedBeforeThen: Boolean read FSettings.NoFeedBeforeThen write
      FSettings.NoFeedBeforeThen;
    Property FeedElseIf: Boolean read FSettings.FeedElseIf write
      FSettings.FeedElseIf;
    Property FeedEachUnit: Boolean read FSettings.FeedEachUnit write
      FSettings.FeedEachUnit;
    Property FeedAfterVar: Boolean read FSettings.FeedAfterVar write
      FSettings.FeedAfterVar;
    Property WrapLines: Boolean read FSettings.WrapLines write
      FSettings.WrapLines;
    Property WrapPosition: Byte read FSettings.WrapPosition write
      FSettings.WrapPosition;
    Property AlignCommentPos: Byte read FSettings.AlignCommentPos write
      FSettings.AlignCommentPos;
    Property AlignComments: Boolean read FSettings.AlignComments write
      FSettings.AlignComments;
    Property AlignVarPos: Byte read FSettings.AlignVarPos write
      FSettings.AlignVarPos;
    Property AlignVar: Boolean read FSettings.AlignVar write
      FSettings.AlignVar;
    Property FeedBeforeEnd: Boolean read FSettings.FeedBeforeEnd write
      FSettings.FeedBeforeEnd;
    Property FillNewWords: TFillMode read FSettings.FillNewWords write
      SetFillNewWords;
    Property FeedAfterSemiColon: Boolean read FSettings.FeedAfterSemiColon write
      FSettings.FeedAfterSemiColon;
    Property BlankSubProc: Boolean read FSettings.BlankSubProc write
      FSettings.BlankSubProc;
    //property CommentFunction: Boolean read FSettings.CommentFunction write
    //  FSettings.CommentFunction;
    //property CommentUnit: Boolean read FSettings.CommentUnit write
    //  FSettings.CommentUnit;
    Property StartCommentOut: TCommentArray read FSettings.StartCommentOut
      write FSettings.StartCommentOut;
    Property EndCommentOut: TCommentArray read FSettings.EndCommentOut write
      FSettings.EndCommentOut;
    Property SupportCOperands: Boolean read Fsettings.SupportCOperands
      write fsettings.SupportCOperands;
    Property IdentifierCaseing: TIdentifierCase read Fsettings.IdentifierCaseing
      write fsettings.IdentifierCaseing;
  End;
Var
  DelforParser: TDelforParser = Nil;

Implementation

Constructor TDelforParser.Create;
Begin
  DelforParser := Self;
  CapNames := TKeywordColl.Create(10);
  Clear;
End;

Function TDelforParser.AlignExpression(I: Integer; aPos: Integer): TPascalWord;
Var
  OldExpr: TExpression;
Begin
  HasAligned := True;
  With FileText Do Begin
    OldExpr := TExpression(Items[I]);
    Result := TAlignExpression.Create(OldExpr, aPos);
    Items[I] := Result;
    OldExpr.Free;
  End;
End;

Procedure TDelforParser.Clear;
Begin
  HasAligned := False;
  LeftPointBracket := 0;
  nIndent := 0;
  ReadingAsm := False;
  PrevLine := Nil;
  prev := Nil;
  prevType := wtNothing;
  If FileText = Nil Then
    FileText := TOCollection.Create(500)
  Else
    FileText.FreeAll;
End;

Procedure Nop();
Begin

End;

Procedure TDelforParser.Add(Buff: PChar);
Var
  AWord: Array[0..Maxline] Of Char;
Begin
  PrevLine := TLineFeed.Create(0); {New(TLineFeed, Create(-1));}
  FileText.Add(PrevLine);
  If ReadingAsm Then
    ReadAsm(Buff);
  While (Buff^ <> #0) Do Begin
    Case prevType Of
      wtHalfComment, wtHalfStarComment,
        wtHalfOutComment: prevType := ReadHalfComment(AWord, Buff);
    Else
      prevType := ReadWord(AWord, Buff);
    End;
    If Not (prevType = wtSpaces) Then Begin
      FileText.Add(TExpression.Create(prevType, AWord));
      If ReadingAsm And (Buff^ <> #0) Then
        ReadAsm(Buff);
    End
    Else If (PrevLine <> Nil) And (PrevLine.nSpaces = 0) Then Begin
      PrevLine.nSpaces := StrLen(AWord);
      PrevLine.oldnSpaces := StrLen(AWord);
    End;
  End;
End;

Procedure TDelforParser.LoadFromFile(AFileName: PChar);
Var
  InFile: TextFile;
  Buff: Array[0..Maxline] Of Char;
Begin
  If Assigned(OnProgress) Then
    OnProgress(Self, 0);
  PrevLine := Nil;
  ReadingAsm := False;
  AssignFile(InFile, AFileName);
  Try
    Reset(InFile);
    Try
      While Not Eof(InFile) And (FileText.Count < MaxCollectionSize - 100) Do Begin
        Readln(InFile, Buff);
        Add(Buff);
      End;
      If FileText.Count >= MaxCollectionSize - 100 Then
        Raise Exception.Create('File to large to reformat')
    Finally
      CloseFile(InFile);
    End;
  Finally
  End;
  If Assigned(OnProgress) Then
    OnProgress(Self, 33);
End;

Procedure TDelforParser.LoadFromList(AList: TStringList);
Var
  Buff: Array[0..Maxline] Of Char;
  I, k: Integer;
Begin
  If Assigned(OnProgress) Then
    OnProgress(Self, 0);
  PrevLine := Nil;
  k := 0;
  ReadingAsm := False;
  With AList Do
    If Count = 0 Then
      Self.Add(StrCopy(Buff, ''))
    Else
      For I := 0 To Count - 1 Do Begin
        StrCopy(Buff, PChar(Strings[I]));
        Self.Add(Buff);
        If Assigned(OnProgress) Then Begin
          inc(k);
          If k = 20 Then Begin
            k := 0;
            OnProgress(Self, Round(I / Count * 34));
          End;
        End;
      End;
End;

Procedure TDelforParser.ReadAsm(Var Buff: PChar);
Var
  P, P1: PChar;
Begin
  P := Buff;
  P1 := Buff;
  While P1^ = ' ' Do
    inc(P1);
  Repeat
    checkPrintable(P);
    Case AsmComment Of
      wtHalfComment: Begin
          If P^ = '}' Then
            AsmComment := wtWord;
          inc(P);
        End;
      wtHalfStarComment: Begin
          If strLComp(P, '*)', 2) = 0 Then Begin
            AsmComment := wtWord;
            inc(P);
          End;
          inc(P);
        End;
    Else
      If (StrLIComp(P, 'end', 3) = 0) And ((P = Buff) Or ((P - 1)^ In [' ',
        Tab])
          And ((P + 3)^ In [#0, ';', ' ', Tab])) Then Begin
        ReadingAsm := False;
        If P1 <> P Then Begin
          Dec(P);
          P^ := #0;
          FileText.Add(TExpression.Create(wtAsm, P1));
          P^ := ' ';
          inc(P);
        End;
        Buff := P;
        Exit;
      End
      Else If P^ = '{' Then Begin
        While (P^ <> '}') And (P^ <> #0) Do Begin
          inc(P);
          checkPrintable(P);
        End;
        If (P^ <> '}') Then
          AsmComment := wtHalfComment;
      End
      Else If strLComp(P, '(*', 2) = 0 Then Begin
        While (strLComp(P, '*)', 2) <> 0) And (P^ <> #0) Do Begin
          inc(P);
          checkPrintable(P);
        End;
        If strLComp(P, '*)', 2) <> 0 Then
          AsmComment := wtHalfStarComment;
      End
      Else If strLComp(P, '//', 2) = 0 Then
        While P^ <> #0 Do Begin
          checkPrintable(P);
          inc(P);
        End
      Else
        inc(P);
    End;
  Until (P^ = #0);
  FileText.Add(TExpression.Create(wtAsm, Buff));
  Buff := P;
End;

Procedure TDelforParser.checkPrintable(P: PChar);
Begin
  If (P <> Nil) And (P^ In NotPrintable) Then Begin
    While (P^ In NotPrintable) And Not (strLComp(P, #13#10, 2) = 0) Do Begin
      P^ := ' ';
      inc(P);
    End;
  End;
End;

Function TDelforParser.ReadWord(Dest: PChar; Var Source: PChar): TWordType;
Const
  operators = '+-*/=<>[].,():;{}@^';
  AllOper = operators + ' {}'''#9;
Var
  P: PChar;
  Len: Integer;

  Procedure ReadString;

    Procedure readQuotes;
    Begin
      While (P^ = '''') And ((P + 1)^ = '''') Do
        inc(P, 2);
    End;
  Begin
    Repeat
      inc(P);
      checkPrintable(P);
      If (P^ = '''') Then Begin
        readQuotes;
        If ((P + 1)^ = '#') Then Begin
          inc(P);
          While P^ In ['0'..'9', 'A'..'F', 'a'..'f', '$', '#', '^'] Do
            inc(P);
          If P^ = '''' Then
            inc(P)
          Else Begin
            Dec(P);
            Exit;
          End;
          readQuotes;
        End
        Else If ((P + 1)^ = '^') Then Begin
          inc(P);
          While P^ In ['0'..'9', 'A'..'Z', 'a'..'z', '$', '#', '^'] Do
            inc(P);
          If P^ = '''' Then
            inc(P)
          Else Begin
            Dec(P);
            Exit;
          End;
          readQuotes;
        End
          {else
            readQuotes;}
      End;
    Until (P^ = '''') Or (P^ = #0);
  End;

  Procedure ReadIdentifier;
  Begin
    Result := wtWord;
    While (StrScan(AllOper, P^) = Nil) And (P^ <> #0) Do
      inc(P);
    Dec(P);
  End;
Begin
  P := Source;
  checkPrintable(P);
  If P^ In [Tab, ' '] Then Begin
    Result := wtSpaces;
    While (P^ In [Tab, ' ']) Do
      inc(P);
    Dec(P);
  End
  Else If (Settings.StartCommentOut[0] <> #0) And (Settings.EndCommentOut[0] <>
    #0) And
    (StrLIComp(P, Settings.StartCommentOut, StrLen(Settings.StartCommentOut)) =
    0) Then Begin
    Result := wtHalfOutComment;
    inc(P, StrLen(Settings.StartCommentOut));
    Len := StrLen(Settings.EndCommentOut);
    While (StrLIComp(P, Settings.EndCommentOut, Len) <> 0) And (P^ <> #0) Do Begin
      inc(P);
      checkPrintable(P);
    End;
    If StrLIComp(P, Settings.EndCommentOut, Len) = 0 Then Begin
      inc(P, Len - 1);
      Result := wtFullOutComment;
    End;
  End
  Else If P^ = '{' Then Begin
    Result := wtHalfComment;
    While (P^ <> '}') And (P^ <> #0) Do Begin
      inc(P);
      checkPrintable(P);
    End;
    If (P^ = '}') Then Begin
      Result := wtFullComment;
      If (Source + 1)^ = '$' Then
        Result := wtCompDirective;
    End;
  End
  Else If strLComp(P, '(*', 2) = 0 Then Begin
    Result := wtHalfStarComment;
    While (strLComp(P, '*)', 2) <> 0) And (P^ <> #0) Do Begin
      inc(P);
      checkPrintable(P);
    End;
    If strLComp(P, '*)', 2) = 0 Then Begin
      inc(P);
      Result := wtFullComment;
    End;
  End
  Else If strLComp(P, '//', 2) = 0 Then Begin
    Result := wtFullComment;
    While P^ <> #0 Do Begin
      checkPrintable(P);
      inc(P);
    End
  End
  Else If P^ = '''' Then Begin
    Result := wtString;
    ReadString;
    If (P^ = #0) Then
      Result := wtErrorString;
  End
  Else If P^ = '^' Then {string starting with ^A or so} Begin
    If ((P + 1)^ In ['a'..'z', 'A'..'Z']) And ((P + 2)^ In ['''', '^', '#']) Then Begin
      Result := wtString;
      While P^ In ['0'..'9', 'A'..'Z', 'a'..'z', '$', '#', '^'] Do
        inc(P);
      If P^ = '''' Then
        ReadString;
    End
    Else
      Result := wtOperator;
  End
  Else If StrScan(operators, P^) <> Nil Then Begin
    Result := wtOperator;
    If strLComp(P, '<=', 2) = 0 Then
      inc(P);
    If strLComp(P, '>=', 2) = 0 Then
      inc(P);
    If strLComp(P, '<>', 2) = 0 Then
      inc(P);
    If strLComp(P, ':=', 2) = 0 Then
      inc(P);
    If SupportCOperands Then Begin
      If strLComp(P, '+=', 2) = 0 Then
        inc(P);
      If strLComp(P, '-=', 2) = 0 Then
        inc(P);
      If strLComp(P, '*=', 2) = 0 Then
        inc(P);
      If strLComp(P, '/=', 2) = 0 Then
        inc(P);
    End;
    If strLComp(P, '..', 2) = 0 Then
      inc(P);
    If strLComp(P, '(.', 2) = 0 Then Begin
      inc(LeftPointBracket);
      inc(P);
    End;
    If strLComp(P, '.)', 2) = 0 Then Begin
      Dec(LeftPointBracket);
      inc(P);
    End;
  End
  Else If P^ = '$' Then Begin
    Result := wtHexNumber;
    inc(P);
    While UpCase(P^) In ['0'..'9', 'A'..'F'] Do
      inc(P);
    Dec(P);
  End
  Else If P^ = '#' Then Begin
    Result := wtNumber;
    While P^ In ['0'..'9', 'A'..'F', 'a'..'f', '$', '#', '^'] Do
      inc(P);
    If P^ = '''' Then Begin
      Result := wtString;
      ReadString;
    End
    Else
      Dec(P);
  End
  Else If P^ In ['0'..'9'] Then Begin
    Result := wtNumber;
    While (P^ In ['0'..'9', '.']) And Not (strLComp(P, '..', 2) = 0)
      And Not ((LeftPointBracket > 0) And (strLComp(P, '.)', 2) = 0)) Do
      inc(P);
    If UpCase(P^) = 'E' Then
      If (P + 1)^ In ['0'..'9', '-', '+'] Then Begin
        inc(P, 2);
        While (P^ In ['0'..'9']) Do
          inc(P);
      End;
    Dec(P);
  End
  Else
    ReadIdentifier;
  StrLCopy(Dest, Source, P - Source + 1);
  If (StrIComp(Dest, 'asm') = 0) Then Begin
    ReadingAsm := True;
    AsmComment := wtWord;
  End;
  If (P^ = #0) Then
    Source := P
  Else Begin
    If ((P + 1)^ In [Tab, ' ']) Then
      inc(P);
    Source := P + 1;
  End;
  {  Readword := Result;}
End;

Function TDelforParser.ReadHalfComment(Dest: PChar; Var Source: PChar):
  TWordType;
Var
  Len: Integer;
  P: PChar;
Begin
  P := Source;
  While P^ In [Tab, ' '] Do
    inc(P);
  If (PrevLine <> Nil) And (PrevLine.nSpaces = 0) Then Begin
    PrevLine.nSpaces := P - Source;
    PrevLine.oldnSpaces := P - Source;
    P := StrCopy(Source, P);
  End;
  ReadHalfComment := prevType;
  If prevType = wtHalfComment Then Begin
    While (P^ <> '}') And (P^ <> #0) Do
      inc(P);
    If (P^ = '}') Then Begin
      ReadHalfComment := wtFullComment;
      inc(P);
    End;
  End
  Else If prevType = wtHalfStarComment Then Begin
    While (strLComp(P, '*)', 2) <> 0) And (P^ <> #0) Do
      inc(P);
    If strLComp(P, '*)', 2) = 0 Then Begin
      ReadHalfComment := wtFullComment;
      inc(P, 2);
    End;
  End
  Else Begin
    Len := StrLen(Settings.EndCommentOut);
    While (StrLIComp(P, Settings.EndCommentOut, Len) <> 0) And (P^ <> #0) Do
      inc(P);
    If StrLIComp(P, Settings.EndCommentOut, Len) = 0 Then Begin
      ReadHalfComment := wtFullOutComment;
      inc(P, Len);
    End;
  End;
  StrLCopy(Dest, Source, P - Source);
  If P^ = #0 Then
    Source := P
  Else Begin
    If (P^ In [Tab, ' ']) Then
      inc(P);
    Source := P;
  End;
End;

Function TDelforParser.Parse: Boolean;
Type
  TRec = Record
    RT: TReservedType;
    nInd: Integer;
  End;
Const
  MaxStack = 150;
Type
  TStackArray = Array[0..MaxStack] Of TRec;
  TStackStackRec = Record
    stackptr: Integer;
    ProcLevel: Integer;
    stack: TStackArray;
    nIndent: Integer;
  End;

Var
  Prev1: TPascalWord;
  OldWrapIndent: Boolean;
  PrevPrevLine: TLineFeed;
  stack: TStackArray;
  stackptr: Integer;
  StackStack: Array[0..2] Of TStackStackRec;
  StackStackPtr: Integer;
  WrapIndent: Boolean;
  interfacePart: Boolean;
  NTmp: Integer;
  PrevOldNspaces: Integer;
  I, J: Integer;

  Function GetStackTop: TReservedType;
  Begin
    If stackptr >= 0 Then
      GetStackTop := stack[stackptr].RT
    Else
      GetStackTop := rtNothing;
  End;

  Procedure SetSpacing(PascalWord, prev: TPascalWord; I: Integer);
  Var
    Prev2: TPascalWord;
    rtype: TReservedType;
    wType: TWordType;
    k: Integer;
    S: Array[0..Maxline] Of Char;
    Found: Boolean;
  Begin
    If PascalWord <> Nil Then Begin
      rtype := PascalWord.ReservedType;
      wType := PascalWord.WordType;
      {if (rType = rtPrivate) and (prev <> nil) and
        (prev.ReservedType <> rtLineFeed) then
      begin
        PascalWord.SetReservedType(rtNothing);
        rType := rtNothing;
      end;}
      If Not (rtype In NoReservedTypes) Then
        PascalWord.ExpressionCase := ReservedCase
      Else If rtype In StandardDirectives Then
        PascalWord.ExpressionCase := StandDirectivesCase
      Else Begin
        PascalWord.ExpressionCase := rfUnchanged;
        If (wType = wtWord) And (PascalWord.Expression <> Nil) Then Begin
          Found := False;
          If (FillNewWords In [fmAddNewWord, fmAddUse, fmAddUseExcept])
            And (rtype In
            (NoReservedTypes - StandardDirectives)) Then Begin
            Found := CapNames.Search(PascalWord.Expression, I);
            If Not Found Then Begin
              StrCopy(S, '*');
              StrCat(S, PascalWord.Expression); {comment out}
              If Not CapNames.Search(@S, J) Then
                CapNames.Insert(I, StrNew(PascalWord.Expression));
            End;
          End;
          If (FillNewWords In [fmUse, fmAddUse])
            Or ((FillNewWords In [fmExceptDirect, fmAddUseExcept]) And
            Not (rtype In StandardDirectives)) Then Begin
            If Not Found Then
              Found := CapNames.Search(PascalWord.Expression, I);
            If Found Then Begin
              PascalWord.Expression := CapNames.Items[I];
              PascalWord.ExpressionCase := rfUnchanged;
            End;
          End;
        End;
        (*       else
                 if (FillNewWords = fmAddNewWord) and (rType in
                   (NoReservedTypes - StandardDirectives)) then
                 begin
                   StrCopy(S, '*');
                   StrCat(S, PascalWord.Expression); {comment out}
                   if not CapNames.Search(@S, I) then
                     CapNames.Insert(I, (StrNew(PascalWord.Expression));
                 end; *)
      End;

      Case rtype Of
        rtHelper,
          rtThen, rtOf, rtElse, rtDo, rtAsm: PascalWord.SetSpace(spBoth, True);
        rtEnd, rtFuncDirective: PascalWord.SetSpace(spBefore, True);
        rtIf, rtUntil, rtWhile, rtCase, rtRecord:
          PascalWord.SetSpace(spAfter, True);
        rtOper, rtMathOper, rtPlus, rtMinus, rtLogOper, rtEquals:
          PascalWord.SetSpace(SpaceOperators, True);
        rtEqualOper: PascalWord.SetSpace(SpaceEqualOper, True);
        rtColon: PascalWord.SetSpace(SpaceColon, True);
        rtSemiColon: PascalWord.SetSpace(SpaceSemiColon, True);
        rtComma: PascalWord.SetSpace(SpaceComma, True);
        rtLeftBr: Begin
            PascalWord.SetSpace(SpaceLeftBr, True);
            If prev.ReservedType = rtLeftBr Then
              PascalWord.SetSpace(spBefore, False);
          End;
        rtLeftHook: Begin
            PascalWord.SetSpace(SpaceLeftHook, True);
            If prev.ReservedType = rtLeftHook Then
              PascalWord.SetSpace(spBefore, False);
          End;
        rtRightBr:
          PascalWord.SetSpace(SpaceRightBr, True);
        rtRightHook:
          PascalWord.SetSpace(SpaceRightHook, True);
      End;
      {append space after : , ;}
      If (wType In [wtNumber, wtHexNumber]) And UpperNumbers Then
        PascalWord.SetCase(rfUpperCase);
      {delimiter between 2 words (necesary)}
      If (prev <> Nil) Then Begin
        If (SpaceOperators In [spBoth, spBefore, spAfter]) And
          (wType In [wtString, wtFullComment,
          wtHalfComment, wtHalfStarComment]) And
          Not (prev.ReservedType In [rtDotDot, rtLineFeed]) Then
          PascalWord.SetSpace(spBefore, True);
        If (rtype In [rtMinus, rtPlus]) Then Begin
          Prev2 := prev;
          k := 0;
          While (Prev2 <> Nil) And (Prev2.ReservedType In [rtComment,
            rtLineFeed]) Do Begin
            inc(k);
            If k > I Then
              Prev2 := Nil
            Else
              Prev2 := TPascalword(FileText.Items[I - k]); // -----------------------------------------------------------
          End;
          If (Prev2 <> Nil) And (Prev2.ReservedType In [rtOper,
            rtMathOper, rtPlus, rtMinus, rtSemiColon, rtOf,
              rtLogOper, rtEquals, rtEqualOper, rtLeftBr,
              rtLeftHook, rtComma, rtDefault]) Then
            PascalWord.SetSpace(spAfter, False); {sign operator}
        End;
        If (rtype = rtLeftHook) Then Begin
          If Not (prev.ReservedType In [rtReserved, rtNothing, rtRightBr,
            rtRightHook]) Then
            //           PascalWord.SetSpace(spBefore, False) {array}
            //         else
            PascalWord.SetSpace(spBefore, True);
        End;
        If PascalWord.Space(spBefore) And
          (prev.ReservedType In [rtLeftBr, rtLeftHook,
          rtLineFeed]) Then
          PascalWord.SetSpace(spBefore, False);
        If (prev.WordType In [wtWord, wtNumber, wtHexNumber, wtString]) And
          (wType In [wtWord, wtNumber, wtHexNumber]) Then
          PascalWord.SetSpace(spBefore, True);
        If (prev.ReservedType = rtComment) And
          (wType In [wtWord, wtNumber, wtHexNumber]) Then
          prev.SetSpace(spAfter, True);
        If PascalWord.Space(spBefore) And prev.Space(spAfter) Then
          prev.SetSpace(spAfter, False); {avoid double spaces}
      End;
    End;
  End;

  Procedure SetPrevIndent;
  Begin
    If PrevLine <> Nil Then
      PrevLine.SetIndent(nIndent + NTmp + ProcLevel);
  End;

  Procedure Push(R: TReservedType; n, ninc: Integer);
  Begin
    inc(stackptr);
    If stackptr > MaxStack Then
      Raise EFormatException.Create('Stack overflow');
    With stack[stackptr] Do Begin
      RT := R;
      nInd := n;
      nIndent := n + ninc;
    End;
  End;

  Function HasType(AType: TReservedType): Boolean;
  Var
    I: Integer;
  Begin
    HasType := False;
    For I := 0 To stackptr Do
      If stack[I].RT = AType Then Begin
        HasType := True;
        Exit;
      End;
  End;

  Function Pop: TReservedType;
  Begin
    If stackptr >= 0 Then Begin
      nIndent := stack[stackptr].nInd;
      If (stack[stackptr].RT = rtProcedure) And (ProcLevel > 0) Then
        Dec(ProcLevel);
      Pop := stack[stackptr].RT;
      Dec(stackptr);
    End
    Else Begin
      nIndent := 0;
      ProcLevel := 0;
      Pop := rtNothing;
    End;
  End;

  Function GetWord(I: Integer): TPascalWord;
  Begin
    With FileText Do
      If (I < Count) And (I >= 0) Then
        GetWord := TPascalWord(Items[I])
      Else
        GetWord := Nil;
  End;

  Function GetNextNoComment(Var I, k: Integer): TPascalWord;
  Begin
    k := 0;
    Repeat
      inc(k);
      Result := GetWord(I + k);
    Until (Result = Nil) Or (Result.ReservedType <> rtComment);
  End;

  Function InsertBlankLines(atIndex, NLines: Integer): TLineFeed;
  Var
    J: Integer;
    AfterWord: TPascalWord;
  Begin
    Result := PrevLine;
    For J := 0 To NLines - 1 Do Begin
      Result := TLineFeed.Create(0);
      Result.SetIndent(nIndent);
      AfterWord := TPascalWord(FileText.Items[atIndex]);
      If AfterWord.Space(spBefore) Then
        AfterWord.SetSpace(spBefore, False);
      FileText.Insert(atIndex, Result);
      SetSpacing(AfterWord, Result, atIndex);
    End;
    If atIndex <= I Then
      inc(I, NLines);
  End;

  Procedure CheckBlankProc;
  Var
    k: Integer;
    Prev2: TPascalWord;
  Begin
    If (prev <> Nil) Then Begin
      k := 1;
      If prev.ReservedType = rtClass Then Begin
        k := 2;
        Prev2 := GetWord(I - 2);
      End
      Else
        Prev2 := prev;
      If (Prev2 <> Nil) And (Prev2.ReservedType <> rtLineFeed) Then Begin
        PrevLine := InsertBlankLines(I - k, 2);
        prev := PrevLine;
      End
      Else Begin
        inc(k);
        Prev2 := GetWord(I - k);
        If (Prev2 <> Nil) And (Prev2.ReservedType <> rtLineFeed) Then Begin
          PrevLine := InsertBlankLines(I - k + 1, 1);
          prev := PrevLine;
        End;
      End;
    End;
  End;

  Procedure PutCommentBefore(aComment: PChar);
  Var
    J: Integer;
    P: TPascalWord;
  Begin
    J := I - 2;
    P := GetWord(J);
    If P.ReservedType = rtComment Then
      P.Expression := aComment
    Else Begin
      P := TExpression.Create(wtWord, aComment);
      P.SetReservedType(rtComment);
      FileText.Insert(I, P);
      inc(I);
      P := TLineFeed.Create(0);
      TLineFeed(P).SetIndent(nIndent);
      FileText.Insert(I, P);
      inc(I);
    End;
  End;

  Function MakeLineFeed(k, J: Integer): TLineFeed;
  Var
    next: TPascalWord;
  Begin
    next := GetNextNoComment(k, J);
    If (next <> Nil) And (next.ReservedType <> rtLineFeed) Then
      Result := InsertBlankLines(k + J, 1)
    Else
      Result := PrevLine;
  End;

  Procedure SetTopIndent;
  Begin
    If stackptr >= 0 Then Begin
      nIndent := stack[stackptr].nInd;
      SetPrevIndent;
    End;
  End;

  Procedure DecPrevIndent;
  Begin
    If PrevLine <> Nil Then
      PrevLine.IncIndent(-1);
  End;

  Procedure CheckIndent(PascalWord: TPascalWord);
  Var
    next: TPascalWord;
    lastPop: TReservedType;
    k: Integer;
    P, P2: PChar;
    Top: TReservedType;
    functdeclare, NoBlankLine: Boolean;
    rtype: TReservedType;
    wType: TWordType;
    Procedure CheckSlashComment;
    Var
      PasWord: TPascalWord;
      PrevPasWord: TPascalWord;
      Buff: Array[0..200] Of Char;
    Begin
      prev := GetWord(I - 1);
      If (prev <> Nil) And (prev.ReservedType = rtComment)
        And (prev.Expression^ = '/') Then {fix for situation with a // comment
        on prev line: begin becomes part of the comment}
        If Not prev.ChangeComment('{') Then Begin
          {FileText.Delete(I - 1);}
          k := 0;
          PasWord := Nil;
          Repeat
            PrevPasWord := PasWord;
            PasWord := GetWord(I + k);
            inc(k);
          Until (PasWord = Nil) Or (PasWord.ReservedType = rtLineFeed);
          Dec(k);
          If ((PrevPasWord.ReservedType = rtComment) And (PrevPasWord.Expression^
            =
            '/')) Then Begin
            prev.Expression := StrCat(StrCat(StrCopy(Buff, '{'), prev.Expression
              +
              2), '}');
            Exit;
          End
          Else
            FileText.Delete(I - 1);
          FileText.Insert(I + k, prev);
          prev := GetWord(I - 1);
          SetSpacing(prev, GetWord(I - 2), I - 1);
          PascalWord := GetWord(I);
        End;
      PrevLine := PrevPrevLine;
    End;

    Function NoBeginTryIndent: Boolean;
    Begin
      Result := Not ((indentBegin And (rtype = rtBegin)) Or
        (IndentTry And (rtype = rtTry))) And
        (GetStackTop In [rtDo, rtThen, rtIfElse]);
    End;

    Procedure CheckShortLine;
    Begin
      k := 1;
      next := GetWord(I + k);
      If (next <> Nil) And (next.ReservedType = rtLineFeed) Then Begin
        While Not ((next = Nil) Or (next.ReservedType In
          [rtSemiColon, rtBegin, rtElse, rtDo, rtWhile, rtOn, rtThen, rtCase])
          Or ((k > 1) And (next.ReservedType = rtLineFeed))) Do Begin
          inc(k);
          next := GetWord(I + k);
        End;
        If (next <> Nil) And (next.ReservedType = rtSemiColon) Then Begin
          FileText.AtFree(I + 1);
        End;
      End;
    End;

    Procedure ComplexIfElse;
    Begin
      While (stackptr >= 0) And (lastPop <> rtThen) Do Begin
        lastPop := Pop;
        If lastPop = rtIfElse Then
          ComplexIfElse;
      End;
      SetPrevIndent;
    End;
  Begin
    If PascalWord <> Nil Then Begin
      rtype := PascalWord.ReservedType;
      wType := PascalWord.WordType;
      If (rtype In [rtWhile, rtEnd, rtRepeat, rtBegin, rtUses, rtTry,
        rtProgram, rtType, rtvar,
          rtIf, rtThen, rtElse] + standardDirectives) And (prev <> Nil) And
        (prev.ReservedType = rtDot) Then Begin
        PascalWord.SetReservedType(rtNothing);
        rtype := rtNothing;
      End;
      {SetSpacing;}
      If rtype <> rtNothing Then Begin
        Case rtype Of
          rtIf: Begin
              If FeedAfterThen And Not FeedElseIf And (GetStackTop =
                rtIfElse) And (prev = PrevLine) Then Begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              End
              Else Begin
                If FeedElseIf And (prev <> PrevLine) Then Begin
                  PrevLine := MakeLineFeed(I - 1, 0);
                  prev := PrevLine;
                End;
              End;
              If ((prev <> Nil) And (prev.ReservedType = rtElse)) Or
                (NoIndentElseIf And (GetStackTop = rtIfElse)) Then Begin
                Pop;
                If GetStackTop = rtThen Then
                  Pop;
                WrapIndent := True;
                Push(rtIfElse, nIndent, 0);
              End
              Else
                Push(rtype, nIndent, 0);
            End;
          rtThen:
            If GetStackTop In [rtIf, rtIfElse] Then Begin
              WrapIndent := False;
              lastPop := Pop;
              If NoFeedBeforeThen And ((prev = PrevLine) And
                (TPascalWord(FileText.Items[I - 1]).ReservedType <>
                rtComment)) Then Begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              End;
              If FeedAfterThen Then Begin
                If MakeLineFeed(I, 1) <> PrevLine Then Begin
                  If (lastPop = rtIf) And Settings.ExceptSingle Then
                    CheckShortLine;
                End;
              End;
              Push(rtype, nIndent, 1);
            End;
          rtColon:
            If GetStackTop = rtOf Then Begin
              Push(rtype, nIndent, 1);
              If FeedAfterThen Then Begin
                If (GetNextNoComment(I, k).ReservedType = rtBegin) And
                  (MakeLineFeed(I, 1) <> PrevLine) Then
                  CheckShortLine;
              End;
              WrapIndent := False;
            End
            Else If GetStackTop = rtClassDecl Then Begin
              Pop;
              Push(rtClass, nIndent, 1);
            End
            Else If AlignVar And (GetStackTop = rtVar) Then
              PascalWord := AlignExpression(I, AlignVarPos)
            Else If Not (GetStackTop In [rtProcedure, rtProcDeclare]) Then
              //label????
              WrapIndent := False;
          rtElse: Begin
              lastPop := rtNothing;
              While (stackptr >= 0) And Not (GetStackTop In [rtThen,
                rtOf, rtTry]) Do
                lastPop := Pop;
              If lastPop = rtIfElse Then
                ComplexIfElse;
              If FeedAfterThen Then Begin
                If (prev <> Nil) And (GetWord(I - 1).ReservedType <>
                  rtLineFeed) Then Begin
                  PrevLine := MakeLineFeed(I - 1, 0);
                  prev := PrevLine;
                End;
                If GetNextNoComment(I, k).ReservedType <> rtIf Then
                  MakeLineFeed(I, 1);
              End;
              If stackptr >= 0 Then
                nIndent := stack[stackptr].nInd;
              If prev = PrevLine Then
                SetPrevIndent;
              If IndentTryElse And (GetStackTop = rtTry) Then Begin
                inc(nIndent);
                SetPrevIndent;
              End
              Else If IndentCaseElse And (GetStackTop = rtOf) Then Begin
                inc(nIndent);
                SetPrevIndent;
              End;
              If GetStackTop = rtThen Then
                Push(rtIfElse, nIndent, 1)
              Else
                Push(rtype, nIndent, 1);
              WrapIndent := False;
            End;
          rtRepeat, rtRecord: Begin
              Push(rtype, nIndent, 1);
              WrapIndent := False;
            End;
          rtClass: Begin
              next := GetNextNoComment(I, k);
              If Not ((next <> Nil) And (next.ReservedType In [rtProcedure,
                rtProcDeclare, rtOf])) Then
              {not a "class function" or "class of" declaration} Begin
                WrapIndent := False;
                Push(rtClassDecl, nIndent, 1);
                {first assume that it is a class declaration
                 the first procedure replaces it with rtClass}
              End
              Else
                PascalWord.SetSpace(spAfter, True);
            End;
          rtUntil: Begin
              Repeat
                lastPop := Pop;
              Until (lastPop = rtRepeat) Or (stackptr < 0);
              SetPrevIndent;
            End;
          rtLeftBr:
            If (GetStackTop = rtLeftBr) Then
              Push(rtype, nIndent, 0)
            Else Begin
              OldWrapIndent := WrapIndent;
              If (ProcLevel <= 0) Or (GetStackTop <> rtProcedure) Then
                {niet erg netjes}
                Push(rtype, nIndent, 1)
              Else Begin
                k := 1;
                next := GetWord(I - k);
                While (I > k) And ((next <> Nil) And (next.ReservedType
                  In
                  [rtDot, rtNothing])) Do Begin
                  inc(k);
                  next := GetWord(I - k);
                End;
                If (next <> Nil) And (next.ReservedType = rtProcedure) Then
                  Push(rtype, nIndent, 0)
                Else
                  Push(rtype, nIndent, 1);
              End;
              WrapIndent := False;
            End;
          rtLeftHook, rtWhile, rtOn:
            Push(rtype, nIndent, 0);
          rtRightBr: Begin
              Repeat
                lastPop := Pop;
              Until (lastPop = rtLeftBr) Or (stackptr < 0);
              If Not (GetStackTop = rtLeftBr) Then
                WrapIndent := OldWrapIndent;
            End;
          rtRightHook: Begin
              Repeat
                lastPop := Pop;
              Until (lastPop = rtLeftHook) Or (stackptr < 0);
              If GetStackTop = rtClassDecl {Interface} Then
                WrapIndent := False;
            End;
          rtExcept: Begin
              While (stackptr >= 0) And (GetStackTop <> rtTry) Do
                Pop;
              SetTopIndent;
              inc(nIndent);
              WrapIndent := False;
            End;
          rtPrivate:
            If Not (GetStackTop In [rtClass, rtClassDecl]) Then
              PascalWord.SetReservedType(rtNothing)
            Else If prev.ReservedType = rtLineFeed Then Begin
              DecPrevIndent;
              WrapIndent := False;
            End;
          rtOf: Begin
              Case GetStackTop Of
                rtCase: Begin
                    Push(rtype, nIndent, 1);
                    If FeedAfterThen Then
                      MakeLineFeed(I, 1);
                    WrapIndent := False;
                  End;
                rtRecord: WrapIndent := False;
              End;
            End;
          rtLineFeed: Begin
              If stackptr = -1 Then
                WrapIndent := False;
              If RemoveDoubleBlank And (I >= 2) And (prev <> Nil)
                And (prev = PrevLine) And
                (TLineFeed(FileText.Items[I - 2]) = PrevPrevLine) Then {// -----------------------------------------------------------} Begin
                FileText.AtFree(I - 2);
                Dec(I);
              End;
              next := GetNextNoComment(I, k);
              If (next <> Nil) Then Begin
                If (next.ReservedType In [rtElse, rtIfElse, rtBegin,
                  rtEnd, rtUntil, rtExcept]) Then
                  WrapIndent := False;
                {TLineFeed(PascalWord).Wrapped:=WrapIndent;}
                If WrapIndent Then
                  NTmp := 1
                Else
                  NTmp := 0;
                Top := GetStackTop;
                WrapIndent := True;
                If (next.ReservedType In [rtLineFeed])
                  Or (Top In [rtUses, rtLeftBr]) Then
                  WrapIndent := False;
              End;
              PrevPrevLine := PrevLine;
              PrevLine := TLineFeed(PascalWord);
              SetPrevIndent;
            End;
          rtAsm: Begin
              While GetStackTop In [rtVar, rtType] Do
                Pop;
              If GetStackTop = rtProcedure Then Begin
                Pop;
                DecPrevIndent;
              End;
              Push(rtype, nIndent, 0);
              With FileText Do Begin
                PascalWord := TPascalWord(Items[I]);
                While (I < Count - 1) And (PascalWord.ReservedType <>
                  rtEnd) Do Begin
                  If PascalWord.ReservedType = rtLineFeed Then Begin
                    PrevLine := TLineFeed(PascalWord);
                    With PrevLine Do
                      nSpaces := oldnSpaces;
                  End;
                  SetSpacing(PascalWord, prev, I);
                  inc(I);
                  prev := PascalWord;
                  PascalWord := TPascalWord(Items[I]);
                End;
                If I < Count Then
                  SetPrevIndent;
                Dec(I);
                Exit;
              End;
            End;
          rtComma:
            If FeedEachUnit And (GetStackTop = rtUses) Then Begin
              next := GetNextNoComment(I, k);
              If next.ReservedType <> rtLineFeed Then Begin
                MakeLineFeed(I, 0);

              End;
            End;
          rtProgram, rtUses, rtInitialization:
            If GetStackTop <> rtLeftBr Then Begin
              next := GetNextNoComment(I, k);
              If (rtype = rtUses) And (GetStackTop In [rtProcedure, rtProcDeclare, rtClass]) Then
                PascalWord.SetReservedType(rtNothing)
              Else Begin
                DecPrevIndent;
                stackptr := -1;
                nIndent := 0;
                Push(rtype, 0, 1);
                WrapIndent := False;
              End;
              {nIndent := 1;}
            End;
          rtAbsolute:
            If Not (GetStackTop In [rtVar, rtType]) Then
              PascalWord.SetReservedType(rtNothing)
            Else Begin
              next := GetNextNoComment(I, k);
              If next.ReservedType = rtColon Then Begin
                DecPrevIndent;
                PascalWord.SetReservedType(rtNothing);
              End;
            End;
          rtFuncDirective, rtDefault: Begin
              next := GetNextNoComment(I, k);
              If (next.ReservedType = rtColon) Or
                Not (GetStackTop In [rtProcedure, rtProcDeclare, rtClass])
                Or (prev.ReservedType In [rtProcedure, rtProcDeclare, rtDot]) Then
                PascalWord.SetReservedType(rtNothing);
            End;
          rtForward: Begin
              If GetStackTop In [rtProcedure, rtProcDeclare] Then
                Pop
              Else
                PascalWord.SetReservedType(rtNothing);
            End;
          rtProcedure: Begin
              If GetStackTop = rtClassDecl Then Begin
                Pop;
                Push(rtClass, nIndent, 1);
              End;
              Prev1 := prev;
              J := I;
              If Prev1 <> Nil Then Begin
                While (J > 0) And (Prev1.ReservedType In [rtComment,
                  rtLineFeed]) Do Begin
                  Dec(J);
                  Prev1 := Tpascalword(FileText.Items[J]); // -----------------------------------------------------------
                End;
                functdeclare := (Prev1 <> Nil) And (Prev1.ReservedType In
                  [rtEquals, rtColon]);
              End
              Else
                functdeclare := False;
              NoBlankLine := False;
              If Not functdeclare Then Begin
                k := 0;
                Repeat
                  inc(k);
                  next := GetWord(I + k);
                  If (next <> Nil) And (next.ReservedType = rtLeftBr) Then
                    Repeat
                      inc(k);
                      next := GetWord(I + k);
                    Until (next = Nil) Or (next.ReservedType = rtRightBr);
                Until (next = Nil) Or (next.ReservedType = rtSemiColon);
                If next <> Nil Then Begin
                  Repeat
                    inc(k);
                    next := GetWord(I + k);
                  Until (next = Nil) Or Not (next.ReservedType In
                    [rtLineFeed,
                    rtComment]);
                  If (next <> Nil) And (next.ReservedType = rtForward) Then
                    NoBlankLine := True;
                End;
              End;
              If Not (functdeclare Or interfacePart Or
                (GetStackTop = rtClass)) Then Begin
                If Not HasType(rtProcedure) Then Begin
                  If (nIndent > 0) Then Begin
                    nIndent := 0;
                    SetPrevIndent;
                  End;
                  ProcLevel := 0;
                  If BlankProc And Not NoBlankLine Then
                    CheckBlankProc;
                  (*
                  Removed by Corpsman seems to be Useless.
                  *)
                  //if CommentFunction then
                  //  PutCommentBefore('{ procedure }');
                End
                Else Begin
                  If BlankSubProc And Not NoBlankLine Then
                    CheckBlankProc;
                  inc(ProcLevel);
                  If nIndent = 0 Then Begin
                    SetPrevIndent;
                    inc(nIndent);
                  End;
                End;
                {if indentProcedure then  inc(nIndent);}
                Push(rtProcedure, nIndent, 0);
              End
              Else Begin
                If (Not functdeclare) And (Not (GetStackTop = rtClass)) Then Begin
                  nIndent := 0;
                  SetPrevIndent;
                End;
                Push(rtProcDeclare, nIndent, 0);
              End;
            End;
          rtInterface: Begin
              If (prev.ReservedType = rtEquals) Then Begin
                {declaration of a OLE object
                   IClass = interface
                     [' dfgsgdf']}
                Push(rtClassDecl, nIndent, 1);
              End
              Else Begin
                interfacePart := True;
                DecPrevIndent;
              End;
              WrapIndent := False;
            End;
          rtImplementation: Begin
              stackptr := -1;
              nIndent := 0;
              interfacePart := False;
              WrapIndent := False;
              {DecPrevIndent;}
              nIndent := 0;
              SetPrevIndent;
            End;
          rtBegin, rtTry: Begin
              If GetStackTop In [rtVar, rtType] Then
                Pop;
              If GetStackTop In [rtProcedure, rtProgram] Then
                Pop;
              If stackptr = -1 Then
                nIndent := 0;
              If NoBeginTryIndent Then
                Dec(nIndent);
              Case FeedRoundBegin Of
                Hanging: Begin
                    If (GetStackTop In [rtDo, rtThen, rtIfElse, rtElse,
                      rtColon])
                      And (prev <> Nil) And (GetWord(I - 1) = PrevLine) Then Begin
                      FileText.AtFree(I - 1);
                      Dec(I);
                      CheckSlashComment;
                    End;
                    MakeLineFeed(I, 1);
                  End;
                NewLine: Begin
                    If (prev <> Nil) And (GetWord(I - 1).ReservedType <>
                      rtLineFeed) Then Begin
                      PrevLine := MakeLineFeed(I - 1, 0);
                      prev := PrevLine;
                    End;
                    MakeLineFeed(I, 1);
                  End;
              End;
              Push(rtype, nIndent, 1);
              If prev = PrevLine Then Begin
                SetPrevIndent;
                DecPrevIndent;
              End;
              WrapIndent := False;
            End;
          rtEquals:
            If AlignVar And (GetStackTop = rtVar) Then
              PascalWord := AlignExpression(I, AlignVarPos);
          rtVar, rtType:
            If Not (GetStackTop In [rtLeftBr, rtLeftHook]) Then Begin
              WrapIndent := False;
              If nIndent < 1 Then
                nIndent := 1;
              {in classes.pas I found
               t =  type string}
              If (GetStackTop In [rtVar, rtType]) Then
                Pop;
              Push(rtype, nIndent, 0);
              If (Not ((prev <> Nil) And
                (prev.ReservedType = rtEquals))) Then Begin
                DecPrevIndent;
                If FeedAfterVar Then
                  MakeLineFeed(I, 1);
              End;
            End;
          rtCase:
            If Not (GetStackTop In [rtRecord, rtLeftBr]) Then
              Push(rtype, nIndent, 0)
            Else Begin
              WrapIndent := False;
              Push(rtRecCase, nIndent, 1);
            End;
          rtDo:
            If GetStackTop In [rtWhile, rtOn] Then Begin
              lastPop := GetStackTop;
              Push(rtype, nIndent, 1);
              WrapIndent := False;
              If NoFeedBeforeThen And (prev = PrevLine) Then Begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              End;
              If FeedAfterThen Then Begin
                If MakeLineFeed(I, 1) <> PrevLine Then Begin
                  If (lastPop = rtOn) And Settings.ExceptSingle Then
                    CheckShortLine;
                End;
              End;
            End;
          rtEnd: Begin
              WrapIndent := False;
              Repeat
                lastPop := Pop;
              Until (stackptr < 0) Or (lastPop In [rtClass, rtClassDecl,
                rtRecord, rtTry, rtCase, rtBegin, rtAsm]);
              If stackptr = -1 Then
                nIndent := 0;
              If FeedBeforeEnd And (prev <> Nil) And
                (GetWord(I - 1).ReservedType <> rtLineFeed) Then Begin
                PrevLine := MakeLineFeed(I - 1, 0);
                prev := PrevLine;
              End;
              If (prev = PrevLine) Then
                SetPrevIndent;
              If NoBeginTryIndent Then
                inc(nIndent);
            End;
          rtComment: Begin
              If IndentComments Then
                WrapIndent := False;
              If (stackptr = -1) And (nIndent > 0) Then Begin
                nIndent := 0;
                SetPrevIndent;
              End;
              SetSpacing(GetWord(I + 1), PascalWord, I + 1);
              If ((PrevLine <> Nil) And (PrevLine = prev)) Then Begin
                If Not IndentComments Or
                  (PascalWord.WordType In [wtFullOutComment,
                  wtHalfOutComment]) Then
                  PrevLine.nSpaces := PrevLine.oldnSpaces
                Else Begin
                  If PrevOldNspaces >= 0 Then
                    PrevLine.nSpaces := PrevLine.nSpaces +
                      (PrevLine.oldnSpaces - PrevOldNspaces)
                  Else
                    PrevOldNspaces := PrevLine.oldnSpaces;
                End;
              End
              Else If AlignComments And (PascalWord.WordType = wtFullComment) Then Begin
                next := GetWord(I + 1);
                If (next <> Nil) And (next.ReservedType = rtLineFeed) Then
                  PascalWord := AlignExpression(I, AlignCommentPos);
              End;
            End;
          rtSemiColon:
            If Not (GetStackTop In [rtLeftBr, rtLeftHook]) Then Begin
              While (stackptr >= 0) And (GetStackTop In [rtDo, rtWhile,
                rtProcDeclare, rtThen,
                  rtProgram, rtUses, rtColon, rtClassDecl]) Or (GetStackTop
                = rtIfElse) Do
                Pop;
              WrapIndent := False;
              k := 0;
              Repeat
                inc(k);
                next := GetWord(I + k);
              Until (next = Nil) Or (Not (next.ReservedType In [{rtComment,}
                rtLineFeed]));
              If (next <> Nil) Then Begin
                If (next.ReservedType = rtAbsolute)
                  Or ((GetStackTop In [rtProcedure, rtProcDeclare,
                  rtClass]) And (next.ReservedType In [rtFuncDirective,
                    rtForward])
                    And (ProcLevel = 0)) Then
                  WrapIndent := True
                Else If FeedAfterSemiColon And Not (next.ReservedType In
                  [rtForward, rtFuncDirective, rtDefault, rtDeprecated]) Then
                  MakeLineFeed(I, 0);
              End;
            End;
          rtCompIf: Begin
              inc(StackStackPtr);
              If (StackStackPtr <= 2) Then Begin
                StackStack[StackStackPtr].stack := stack;
                StackStack[StackStackPtr].stackptr := stackptr;
                StackStack[StackStackPtr].ProcLevel := ProcLevel;
                StackStack[StackStackPtr].nIndent := nIndent;
              End;
            End;
          rtCompElse: Begin
              If (StackStackPtr >= 0) And (StackStackPtr <= 2) Then Begin
                stack := StackStack[StackStackPtr].stack;
                stackptr := StackStack[StackStackPtr].stackptr;
                ProcLevel := StackStack[StackStackPtr].ProcLevel;
                nIndent := StackStack[StackStackPtr].nIndent;
              End;
            End;
          rtCompEndif: Begin
              If StackStackPtr >= 0 Then
                Dec(StackStackPtr);
            End;
        End;
      End;
      SetSpacing(PascalWord, prev, I);
      If Not (rtype In [rtLineFeed, rtComment]) Then
        PrevOldNspaces := -1;
      If wType = wtCompDirective Then Begin
        WrapIndent := False;
        If Not IndentCompDirectives And (prev <> Nil) And
          (prev.ReservedType = rtLineFeed) Then Begin
          NTmp := -nIndent;
          SetPrevIndent;
        End;
        If UpperCompDirectives Then Begin
          P := PascalWord.Expression;
          P2 := P + 1;
          While Not (P2^ In [' ', Tab, #0]) Do
            inc(P2);
          While P < P2 Do Begin
            inc(P);
            P^ := UpCase(P^);
          End;
        End;
      End;
      If Not (PascalWord.ReservedType = rtComment) Then
        prev := PascalWord;
    End;
  End;
Begin {procedure TPascalParser.Parse;}
  Result := True;
  Try
    If Assigned(OnProgress) Then
      OnProgress(Self, 33);
    If ChangeIndent Then Begin
      {LastPop := rtNothing;}
      PrevLine := Nil;
      PrevPrevLine := Nil;
      prev := Nil;
      WrapIndent := True;
      interfacePart := False;
      nIndent := 0;
      ProcLevel := 0;
      NTmp := 0;
      PrevOldNspaces := -1;
      stackptr := -1;
      StackStackPtr := -1;
      I := 0;
      With FileText Do
        While I < Count Do Begin
          CheckIndent(TPascalWord(Items[I]));
          inc(I);
          If (I Mod 1024 * 4 = 0) And Assigned(OnProgress) Then
            OnProgress(Self, Round(33 + I * 33 / FileText.Count));
        End;
    End;
    With FileText Do Begin
      (*
      DEBUGG
      *)
      //if Count  = 0 then exit; // Debugg Ende
      I := Count - 1;
      While (TPascalWord(Items[I]).ReservedType = rtLineFeed) And (I > 0) Do Begin
        AtFree(I);
        Dec(I);
      End;
      If WrapLines Or HasAligned Then
        CheckWrapping;
    End;
    If Assigned(OnProgress) Then
      OnProgress(Self, 66);
  Except
    On E: Exception Do Begin
      Clear;
      Result := False;
      {ShowMessage('Error occurred, cannot format');}
    End;
  End;
End;

Procedure TDelforParser.CheckWrapping;
Var
  I, J, k: Integer;
  K2, lineLen: Integer;
  PrevPrevLine: TLineFeed;
  PascalWord: TPascalWord;
  Found: Boolean;

  Procedure InsertLine(k: Integer);
  Begin
    PrevPrevLine := PrevLine;
    PrevLine := TLineFeed.Create(PrevPrevLine.oldnSpaces);
    PrevLine.nSpaces := PrevPrevLine.nSpaces;
    PrevLine.wrapped := True;
    TPascalWord(FileText.Items[k]).SetSpace(spBefore, False);
    FileText.Insert(k, PrevLine);
    Found := True;
  End;

Begin
  With FileText Do Begin
    lineLen := 0;
    PrevLine := Nil;
    J := 0;
    I := 0;
    While I < Count Do Begin
      PascalWord := TPascalWord(Items[I]);
      PascalWord.GetLength(lineLen);
      If WrapLines And
        (PascalWord Is TAlignExpression) And (lineLen > WrapPosition) Then
        With TAlignExpression(PascalWord) Do Begin
          k := nSpaces - lineLen - WrapPosition;
          If k < 1 Then
            nSpaces := 1
          Else
            nSpaces := k;
          lineLen := WrapPosition;
        End;
      If PascalWord.ReservedType = rtLineFeed Then Begin
        PrevLine := TLineFeed(PascalWord);
        If (lineLen > WrapPosition) Then
          lineLen := 0;
        J := I;
      End;
      If WrapLines And (lineLen > WrapPosition) And (I > J + 3) Then Begin
        k := I - 1;
        K2 := 0;
        Found := False;
        While (k >= J) And Not Found Do Begin
          If (TPascalWord(Items[k]).ReservedType In [rtThen, rtDo]) Or
            ((TPascalWord(Items[k]).ReservedType = rtElse) And
            (TPascalWord(Items[k + 1]).ReservedType <> rtIf)) Then Begin
            InsertLine(k + 1);
            I := J;
          End;
          If (K2 = 0) And (TPascalWord(Items[k]).Space(spAfter) Or
            TPascalWord(Items[k + 1]).Space(spBefore)) Then
            K2 := k + 1;
          Dec(k);
        End;
        If Not Found And (K2 <> 0) And (K2 > J) Then Begin
          InsertLine(K2);
          I := J;
        End;
        lineLen := 0;
      End;
      inc(I);
    End;
  End;
End;

Function TDelforParser.GetString(Dest: PChar; Var I: Integer): PChar;
Var
  PasWord: TPascalWord;
  P: PChar;
Begin
  GetString := Dest;
  Dest^ := #0;
  P := Dest;
  If (I < FileText.Count) And (I >= 0) Then Begin
    PasWord := TPascalWord(FileText.Items[I]);
    Repeat
      P := PasWord.GetEString(P);
      inc(I);
      If I < FileText.Count Then Begin
        PasWord := TPascalWord(FileText.Items[I]);
      End;
    Until (I >= FileText.Count) Or (PasWord.ReservedType = rtLineFeed);
  End;
  P := StrEnd(Dest);
  If P - Dest > 1 Then
    Dec(P);
  While (P >= Dest) And (P^ In [' ', Tab]) Do Begin
    P^ := #0;
    Dec(P);
  End;
End;

Procedure TDelforParser.WriteToFile(AFileName: PChar);
Var
  outFile: TextFile;
  I: Integer;
  A: Array[0..Maxline] Of Char;
Begin
  Assign(outFile, AFileName);
  Try
    Rewrite(outFile);
    I := 0;
    With FileText Do
      While I < Count Do
        Writeln(outFile, GetString(A, I));
  Finally
    Close(outFile);
  End;
End;

Procedure TDelforParser.SetTextStr(AText: PChar);

Var
  sl: TStringList;

  Function DefineRedifineIdentifier(Value: String): String;
  Var
    index: integer;
  Begin
    If sl.Find(value, index) Then Begin
      result := sl[index];
    End
    Else Begin
      sl.Add(Value);
      result := value;
    End;
  End;

Var
  P1, P2: PChar;
  Total, Curr, I, j, k: Integer;
  e: TExpression;
  s: String;
Begin
  P1 := AText;
  Total := StrLen(AText);
  Curr := 0;
  P2 := StrScan(P1, New_Line);
  k := 0;
  While P2 <> Nil Do Begin
    inc(k);
    I := P2 - P1;
    P2^ := #0;
    Add(P1);
    P2^ := New_Line;
    {strLCopy(buff, p1, i);
    add(buff);             }
    p1 := p2 + CRLF_LEN; // StrLen(CRLF);
    P2 := StrScan(P1, New_Line);
    Curr := Curr + I + CRLF_LEN; // StrLen(CRLF);
    If (k Mod 50 = 0) And Assigned(OnProgress) Then
      OnProgress(Self, (Curr * 34) Div Total);
  End;
  Add(P1);

  (*
   * Hack Eingefügt by Corpsman, der das Schlüsselwort "For" umbiegt
   * zu einem Reserved, wenn es nach "Helper" kommt -> Damit die Einrückung stimmt
   * Die Schlüsselworte "Type" und "Record" for "Helper" werden zu einer "Class" Umgebogen -> Damit die Einrückung stimmt
   *)
  sl := TStringList.Create;
  sl.Clear;
  sl.CaseSensitive := false;
  sl.Sorted := true;
  For i := 0 To FileText.Count - 1 Do Begin
    If (TPascalWord(FileText[i]) Is TExpression) And (TExpression(FileText[i]).FReservedType = rtHelper) Then Begin
      // for -> rtReserved
      For j := i + 1 To FileText.Count - 1 Do Begin
        If (TPascalWord(FileText[j]) Is TExpression) And (lowercase(TExpression(FileText[j]).FExpression) = 'for') Then Begin
          e := TExpression(FileText[j]);
          e.FReservedType := rtReserved; // Umschreiben, das das For nicht wie eine While verwendet wird.
          break;
        End
        Else
          break;
      End;
      // "Type", "Record" -> rtClass
      For j := i - 1 Downto 0 Do Begin
        If (TPascalWord(FileText[j]) Is TExpression) And
          ((lowercase(TExpression(FileText[j]).FExpression) = 'type') Or (lowercase(TExpression(FileText[j]).FExpression) = 'record')) Then Begin
          e := TExpression(FileText[j]);
          e.FReservedType := rtClass; // Umschreiben, das das For nicht wie eine While verwendet wird.
          break;
        End
        Else
          break;
      End;
    End;
    (*
     * Hack eingefügt by Corpsman, der alle Bezeichner "Gleich" Formatiert
     *)
    If IdentifierCaseing <> idUnchanged Then Begin
      If (TPascalWord(FileText[i]) Is TExpression) And (TExpression(FileText[i]).FReservedType = rtNothing) Then Begin
        s := TExpression(FileText[i]).GetExpression;
        Case IdentifierCaseing Of
          idSameAsFirstOccurence: Begin
              s := DefineRedifineIdentifier(s);
            End;
          idLowerCase: Begin
              s := lowercase(s);
            End;
          idUpperCase: Begin
              s := UpperCase(s);
            End;
          idFirstUp: Begin
              s := lowercase(s);
              If length(s) > 0 Then Begin
                s[1] := uppercase(s[1])[1];
              End;
            End;
        End;
        TExpression(FileText[i]).SetExpression(pchar(s));
      End;
    End;
  End;
  sl.free;
End;

Function TDelforParser.GetTextStr: PChar;
Const
  Increment = $FFFF;
Var
  CurSize, CurCap: Integer;
  Buff: Array[0..Maxline] Of Char;
  P, P2, Pres: PChar;
  I: Integer;
Begin
  CurCap := Increment;
  StrDispose(FCurrentText);
  Pres := StrAlloc(CurCap + 10);
  P := Pres;
  P^ := #0;
  I := 0;
  CurSize := 0;
  With FileText Do
    While I < Count Do Begin
      GetString(Buff, I);
      CurSize := CurSize + Integer(StrLen(Buff)) + 2;
      While CurSize > CurCap Do Begin
        inc(CurCap, Increment);
        P2 := Pres;
        Pres := StrAlloc(CurCap + 10);
        P := strECopy(Pres, P2);
        StrDispose(P2);
      End;
      P := strECopy(P, Buff);
      P := strECopy(P, CRLF);
      If (I Mod 50 = 0) And Assigned(OnProgress) Then
        OnProgress(Self, 66 + I * 34 Div Count);
    End;
  Result := Pres;
  FCurrentText := Pres;
End;

Destructor TDelforParser.Destroy;
Begin
  Inherited Destroy;
  StrDispose(FCurrentText);
  CapNames.free;
  FileText.Free;
End;

Constructor TPascalWord.Create;
Begin
  Inherited Create;
End;

Function TPascalWord.GetExpression: PChar;
Begin
  GetExpression := Nil;
End;

Procedure TPascalWord.GetLength(Var Length: Integer);
Begin
End;

Function TPascalWord.WordType: TWordType;
Begin
  WordType := wtLineFeed;
End;

Function TPascalWord.Space(SpaceBefore: TSpaceBefore): Boolean;
Begin
  Space := False;
End;

Procedure TPascalWord.SetExpression(AExpression: PChar);
Begin
End;

Procedure TPascalWord.SetCase(ACase: TCase);
Begin
End;

Function TPascalWord.GetCase: TCase;
Begin
  Result := rfUnchanged;
End;

Procedure TPascalWord.SetSpace(SpaceBefore: TSpaceBefore; State: Boolean);
Begin
End;

Function TPascalWord.ReservedType: TReservedType;
Begin
  ReservedType := rtNothing;
End;

Procedure TPascalWord.SetReservedType(AReservedType: TReservedType);
Begin
End;

Constructor TExpression.Create(AType: TWordType; AExpression: PChar);
Begin
  FWordType := AType;
  FFormatType := ftNothing;
  FReservedType := rtNothing;
  FExpression := Nil;
  SetExpression(AExpression);
  SetSpace(spBoth, False);
  CheckReserved;
End;

Procedure TExpression.CheckReserved;
Var
  L, H, C, I: Integer;
  P: PChar;
  Buf: Array[0..Maxline] Of Char;
Begin
  SetReservedType(rtNothing);
  Case WordType Of
    wtCompDirective: Begin
        P := StrLower(StrCopy(Buf, Expression));
        If P <> Nil Then Begin
          If StrLIComp(P, '{$ifdef', 7) = 0 Then
            SetReservedType(rtCompIf)
          Else If StrLIComp(P, '{$ifndef', 8) = 0 Then
            SetReservedType(rtCompIf)
          Else If StrLIComp(P, '{$else', 6) = 0 Then
            SetReservedType(rtCompElse)
          Else If StrLIComp(P, '{$endif', 7) = 0 Then
            SetReservedType(rtCompEndif);
        End;
      End;
    wtFullComment, wtFullOutComment, wtHalfStarComment,
      wtHalfOutComment, wtHalfComment: SetReservedType(rtComment);
    wtWord: Begin
        P := StrLower(StrCopy(Buf, Expression));
        If P <> Nil Then Begin
          L := 0;
          H := NReservedWords - 1; {fast search method}
          While L <= H Do Begin
            I := (L + H) Shr 1;
            C := StrComp(ReservedArray[I].Words, P);
            If C < 0 Then
              L := I + 1
            Else Begin
              H := I - 1;
              If C = 0 Then
                With ReservedArray[I] Do Begin
                  SetReservedType(ReservedType);
                  Exit;
                End;
            End;
          End;
        End;
      End;
    wtOperator: Begin
        P := Expression;
        If StrLen(P) = 1 Then Begin
          Case P^ Of
            ':': SetReservedType(rtColon);
            '.': SetReservedType(rtDot);
            ';': SetReservedType(rtSemiColon);
            ',': SetReservedType(rtComma);
            ')': SetReservedType(rtRightBr);
            '(': SetReservedType(rtLeftBr);
            ']': SetReservedType(rtRightHook);
            '[': SetReservedType(rtLeftHook);
            '-': SetReservedType(rtMinus);
            '+': SetReservedType(rtPlus);
            '=': SetReservedType(rtEquals);
            '/', '*': SetReservedType(rtMathOper);
            '<', '>': SetReservedType(rtLogOper);
          End;
        End
        Else If StrLen(P) = 2 Then Begin
          If (StrComp(P, '.)') = 0) Then
            SetReservedType(rtRightHook)
          Else If (StrComp(P, '(.') = 0) Then
            SetReservedType(rtLeftHook)
          Else If (StrComp(P, '..') = 0) Then
            SetReservedType(rtDotDot)
          Else If (StrComp(P, ':=') = 0) Then
            SetReservedType(rtEqualOper)
          Else Begin
            // TODO: Check if all C-Operands are listed.
            If (StrComp(P, '+=') = 0) Or
              (StrComp(P, '-=') = 0) Or
              (StrComp(P, '*=') = 0) Or
              (StrComp(P, '/=') = 0)
              Then Begin
              SetReservedType(rtEqualOper);
            End
            Else Begin
              If (P^ = '<') Or (P^ = '>') Then
                {if p in > < <> >=  <= =}
                SetReservedType(rtLogOper);
            End;
          End;
        End;
      End;
  End;
End;

Procedure TExpression.SetExpression(AExpression: PChar);
Begin
  StrDispose(FExpression);
  FExpression := StrNew(AExpression);
End;

Function TExpression.WordType: TWordType;
Begin
  WordType := FWordType;
End;

Function TExpression.GetExpression: PChar;
Begin
  Result := FExpression;
End;

Function TExpression.Space(SpaceBefore: TSpaceBefore): Boolean;
Begin
  Case SpaceBefore Of
    spBefore: Space := FFormatType And ftSpaceBefore > 0;
    spAfter: Space := FFormatType And ftSpaceAfter > 0;
    spBoth: Space := FFormatType And (ftSpaceAfter Or ftSpaceBefore) > 0;
  Else
    Space := False;
  End;
End;

Function TExpression.ReservedType: TReservedType;
Begin
  ReservedType := FReservedType;
End;

Destructor TExpression.Destroy;
Begin
  SetExpression(Nil);
  Inherited Destroy;
End;

Procedure TExpression.SetSpace(SpaceBefore: TSpaceBefore; State: Boolean);
Var
  B: Byte;
Begin
  Case SpaceBefore Of
    spBefore: B := ftSpaceBefore;
    spAfter: B := ftSpaceAfter;
    spBoth: B := ftSpaceAfter Or ftSpaceBefore;
  Else
    B := 0;
  End;
  If State Then
    FFormatType := FFormatType Or B
  Else
    FFormatType := FFormatType And Not B;
End;

Procedure TExpression.SetCase(ACase: TCase);
Begin
  FFormatType := FFormatType And Not (ftLowerCase + ftUpperCase);
  Case ACase Of
    rfUpperCase: FFormatType := FFormatType Or ftUpperCase;
    rfLowerCase: FFormatType := FFormatType Or ftLowerCase;
    rfFirstUp: FFormatType := FFormatType Or ftFirstUp;
  End;
End;

Function TExpression.GetCase: TCase;
Begin
  If (FFormatType And ftUpperCase) > 0 Then
    Result := rfUpperCase
  Else If (FFormatType And ftLowerCase) > 0 Then
    Result := rfLowerCase
  Else If (FFormatType And ftFirstUp) > 0 Then
    Result := rfFirstUp
  Else
    Result := rfUnchanged;
End;

Procedure TExpression.SetReservedType(AReservedType: TReservedType);
Begin
  FReservedType := AReservedType;
End;

Function TExpression.GetEString(Dest: PChar): PChar;
Var
  e: PChar;
Begin
  If Space(spBefore) Then
    Result := strECopy(Dest, ' ')
  Else
    Result := Dest;
  e := Expression;
  If assigned(e) Then
    Result := strECopy(Result, e);
  StrCase(Dest, ExpressionCase);
  If Space(spAfter) Then
    Result := strECopy(Result, ' ');
End;

Procedure TExpression.GetLength(Var Length: Integer);
Begin
  If Space(spBefore) Then
    inc(Length);
  If Space(spAfter) Then
    inc(Length);
  If Expression <> Nil Then
    inc(Length, StrLen(Expression));
End;

Function strSpace(Dest: PChar; n: Integer): PChar;
Var
  I: Integer;
Begin
  strSpace := Dest;
  (*
   * Modified by Corpsman: prevent Buffer Overflow
   *)
  If n >= Maxline Div 2 Then Begin
    n := 0;
  End;
  For I := 0 To n - 1 Do Begin
    Dest^ := ' ';
    inc(Dest);
  End;
  Dest^ := #0;
End;

Function TLineFeed.GetEString(Dest: PChar): PChar;
Var
  Len: Integer;
Begin
  If wrapped And (DelforParser <> Nil) Then
    Len := nSpaces + DelforParser.SpacePerIndent
  Else
    Len := nSpaces;
  If (Len > 0) Then
    GetEString := StrEnd(strSpace(Dest, Len))
  Else
    GetEString := Dest;
End;

Procedure TLineFeed.GetLength(Var Length: Integer);
Begin
  If nSpaces > 0 Then
    Length := nSpaces
  Else
    Length := 0;
End;

Function TLineFeed.ReservedType: TReservedType;
Begin
  ReservedType := rtLineFeed;
End;

Constructor TLineFeed.Create(AOldnSpaces: Integer);
Begin
  Inherited Create;
  oldnSpaces := AOldnSpaces;
  wrapped := False;
  nSpaces := AOldnSpaces; {default not changed indent}
End;

Procedure TLineFeed.IncIndent(n: Integer);
Begin
  If DelforParser <> Nil Then
    inc(nSpaces, n * DelforParser.SpacePerIndent);
End;

Procedure TLineFeed.SetIndent(n: Integer);
Begin
  If DelforParser <> Nil Then
    nSpaces := n * DelforParser.SpacePerIndent;
End;
{ TAlignExpression }

Constructor TAlignExpression.Create(Like: TExpression; Pos: Byte);
Begin
  AlignPos := Pos;
  nSpaces := 0;
  FExpression := StrNew(Like.FExpression);
  FWordType := Like.FWordType;
  FFormatType := Like.FFormatType;
  FReservedType := Like.FReservedType;
End;

Procedure TAlignExpression.GetLength(Var Length: Integer);
Begin
  If Length < AlignPos Then Begin
    nSpaces := AlignPos - Length;
    Length := AlignPos;
    SetSpace(spBefore, False);
  End
  Else
    nSpaces := 0;
  Inherited GetLength(Length);
End;

Function TAlignExpression.GetEString(Dest: PChar): PChar;
Begin
  If (nSpaces > 0) Then
    Result := StrEnd(strSpace(Dest, nSpaces))
  Else
    Result := Dest;
  Result := Inherited GetEString(Result);
End;

Procedure TDelforParser.SetFillNewWords(AFillNewWords: TFillMode);
Begin
  FSettings.FillNewWords := AFillNewWords;
End;

Procedure TDelforParser.LoadCapFile(ACapFile: PChar);
Var
  InFile: TextFile;
  S: Array[0..400] Of Char;
  I: Integer;
Begin
  If CapNames <> Nil Then Begin
    CapNames.FreeAll;
    If (ACapFile <> Nil) And FileExists(ACapFile^) Then Begin
      AssignFile(InFile, ACapFile^);
      Try
        Reset(InFile);
        While Not Eof(InFile) Do Begin
          Readln(InFile, S);
          i := 0; // Shut up Compiler warning, i is out parameter of "CapNames.Search".
          If Not CapNames.Search(@S, I) Then
            CapNames.Insert(I, StrNew(S));
        End;
      Finally
        CloseFile(InFile);
      End;
    End
  End;
End;

Procedure TDelforParser.SaveCapFile(ACapFile: PChar);
Var
  outFile: TextFile;
  I: Integer;
Begin
  If (ACapFile <> Nil) And (ACapFile^ <> #0) Then Begin
    Assign(outFile, ACapFile);
    Try
      Rewrite(outFile);
      For I := 0 To CapNames.Count - 1 Do Begin
        Write(outFile, PChar(CapNames.Items[I]));
        Writeln(outFile);
      End;
    Finally
      Close(outFile);
    End;
  End;
End;

Function TPascalWord.ChangeComment(commchar: char): boolean;
Var
  Buff, buff1: Array[0..Maxline] Of Char;
Begin
  Result := False;
  If ReservedType = rtComment Then Begin
    strCopy(buff, expression);
    strCopy(buff1, expression);
    If strScan(buff, commChar) = Nil Then
      Case commchar Of
        '{':
          If strScan(buff, '}') = Nil Then Begin
            StrCopy(buff, '{');
            strCat(buff, buff1);
            strCat(buff, '}');
            Result := True;
          End;
        '(':
          If strPos(buff, '*)') = Nil Then Begin
            StrCopy(buff, '(*');
            strCat(buff, buff1);
            strCat(buff, '*)');
            Result := True;
          End;
        '/':
          If StrPos(buff, '//') <> @buff[0] Then Begin
            strCat(StrCopy(buff, '//'), buff1);
            Result := True;
          End;
      End;
    expression := buff;
  End;
End;

End.

