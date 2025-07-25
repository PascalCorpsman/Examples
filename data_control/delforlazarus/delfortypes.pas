{|----------------------------------------------------------------------
 | Unit:        DelforTypes
 |
 | Author:      Egbert van Nes
 |
 | Description: Simple types used in DelFor(Exp)
 |
 | Copyright (c) 2000  Egbert van Nes
 |   All rights reserved
 |   Disclaimer and licence notes: see license.txt
 |
 |----------------------------------------------------------------------
}
(*
 * Modified by Corpsman, to Support Lazarus Linux. 01.09.2009
 *)
Unit delfortypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

Interface

Uses SysUtils, oobjects;

Const
  (*
  Increased Version Number, because, the modifying was to much
  *)
  CurrentDllVersion = 2;
Type
  TWordType = (wtLineFeed, wtSpaces, wtHalfComment, wtHalfStarComment,
    wtHalfOutComment, wtFullComment, wtFullOutComment, wtString, wtErrorString,
    wtOperator, wtWord, wtNumber, wtHexNumber, wtNothing, wtAsm, wtCompDirective);
  EFormatException = Class(Exception);

  TProgressEvent = Procedure(Sender: TObject;
    Progress: Integer) Of Object;
Const
  Maxline = 1024; // the maximum line length of the editor
{$IFDEF WINDOWS}
  CRLF = #13#10#0;
  CRLF_LEN = 2;
  New_Line = #13;
{$ELSE}
  CRLF = #10#0;
  CRLF_LEN = 1;
  New_Line = #10;
{$ENDIF}
  NotPrintable = [#1..#8, #10..#14, #16..#19, #22..#31]; {not printable chars}
  Tab = #9;
  ftNothing = 0;
  ftSpaceBefore = $01;
  ftSpaceAfter = $02;
  ftSpaceBoth = $03;
  ftUpperCase = $04;
  ftLowerCase = $08;
  ftFirstUp = $10;
Type
  TSpaceBefore = (spNone, spBefore, spAfter, spBoth);
  TReservedType = (rtNothing, rtReserved, rtOper, rtDirective, rtHelper,
    rtIf, rtDo, rtWhile, rtOn, rtVar, rtType, rtProcedure, rtAsm, rtTry,
    rtExcept,
    rtEnd, rtBegin, rtCase, rtOf, rtLineFeed, rtColon, rtSemiColon,
    rtThen, rtClass, rtClassDecl, rtProgram, rtRepeat, rtUntil, rtRecord,
    rtPrivate, rtElse, rtIfElse, rtInterface, rtImplementation,
    rtLeftBr, rtRightBr, rtLeftHook, rtRightHook, rtMathOper, rtEqualOper,
    rtMinus, rtPlus,
    rtLogOper, rtEquals, rtForward, rtDeprecated, rtDefault, rtInitialization, rtComma,
    rtUses, rtProcDeclare, rtFuncDirective, rtAbsolute, rtComment, rtRecCase, rtDot,
    rtCompIf, rtDotDot,
    rtCompElse, rtCompEndif);
Const
  NoReservedTypes = [rtNothing, rtComma, rtColon, rtLineFeed, rtDefault,
    rtFuncDirective, rtAbsolute, rtComment, rtLeftBr, rtRightBr, rtForward,
    rtCompIf, rtCompElse, rtCompEndif, rtPrivate];
  StandardDirectives = [rtDefault, rtPrivate, rtFuncDirective,
    rtAbsolute, rtForward];
Type
  TIdentifierCase = (idUnchanged, idSameAsFirstOccurence, idLowerCase, idUpperCase, idFirstUp);
  TCase = (rfLowerCase, rfUpperCase, rfFirstUp, rfUnchanged);
  TReservedRec = Record
    ReservedType: TReservedType;
    Words: PChar;
  End;

Type
  TFeedBegin = (Unchanged, Hanging, NewLine);
  TFillMode = (fmUnchanged, fmAddNewWord, fmUse, fmExceptDirect, fmAddUse,
    fmAddUseExcept);

  TCommentArray = Array[0..20] Of Char; {easier to save}

  PSettings = ^TSettings;
  TSettings = Packed Record // !! Attention if you add a new field, always add at the end, otherwise the load function will do strange things !
    ShortCut: Word;
    SpaceOperators: TSpaceBefore;
    SpaceColon: TSpaceBefore;
    SpaceSemiColon: TSpaceBefore;
    SpaceComma: TSpaceBefore;
    SpaceLeftBr: TSpaceBefore;
    SpaceRightBr: TSpaceBefore;
    SpaceLeftHook: TSpaceBefore;
    SpaceRightHook: TSpaceBefore;
    SpaceEqualOper: TSpaceBefore;
    UpperCompDirectives: Boolean;
    UpperNumbers: Boolean;
    ReservedCase: TCase;
    StandDirectivesCase: TCase;
    ChangeIndent: Boolean;
    NoIndentElseIf: Boolean;
    indentBegin: Boolean;
    IndentTry: Boolean;
    IndentTryElse: Boolean;
    IndentCaseElse: Boolean;
    IndentComments: Boolean;
    IndentCompDirectives: Boolean;
    BlankProc: Boolean;
    BlankSubProc: Boolean;
    RemoveDoubleBlank: Boolean;
    SpacePerIndent: Integer;
    FeedRoundBegin: TFeedBegin;
    FeedBeforeEnd: Boolean;
    FeedAfterThen: Boolean;
    ExceptSingle: Boolean;
    FeedAfterVar: Boolean;
    FeedEachUnit: Boolean;
    NoFeedBeforeThen: Boolean;
    FeedElseIf: Boolean;
    FillNewWords: TFillMode;
    FeedAfterSemiColon: Boolean;
    StartCommentOut: TCommentArray;
    EndCommentOut: TCommentArray;
    (*
    Removed by Corpsman, seems to be useless
    *)
    //CommentFunction: Boolean;
    //CommentUnit: Boolean;
    WrapLines: Boolean;
    WrapPosition: Byte;
    AlignCommentPos: Byte;
    AlignComments: Boolean;
    AlignVarPos: Byte;
    AlignVar: Boolean;
    SupportCOperands: Boolean; // Support für += , -= , *=, /=
    IdentifierCaseing: TIdentifierCase;
  End;

Const
  NReservedWords = 108;
  {IndentProcedure = True;}
Type
  TReservedArray = Array[0..NReservedWords - 1] Of TReservedRec;
Const
  {must be ordered perfectly on words!!}

  ReservedArray: TReservedArray = (
    (ReservedType: rtAbsolute; Words: 'absolute'),
    (ReservedType: rtFuncDirective; Words: 'abstract'),
    (ReservedType: rtOper; Words: 'and'),
    (ReservedType: rtReserved; Words: 'array'),
    (ReservedType: rtOper; Words: 'as'),
    (ReservedType: rtAsm; Words: 'asm'),
    (ReservedType: rtFuncDirective; Words: 'assembler'),
    (ReservedType: rtPrivate; Words: 'automated'),
    (ReservedType: rtBegin; Words: 'begin'),
    (ReservedType: rtCase; Words: 'case'),
    (ReservedType: rtFuncDirective; Words: 'cdecl'),
    (ReservedType: rtClass; Words: 'class'),
    (ReservedType: rtVar; Words: 'const'),
    (ReservedType: rtProcedure; Words: 'constructor'),
    (ReservedType: rtUses; Words: 'contains'),
    (ReservedType: rtDefault; Words: 'default'),
    (ReservedType: rtDeprecated; Words: 'deprecated'),
    (ReservedType: rtProcedure; Words: 'destructor'),
    (ReservedType: rtFuncDirective; Words: 'dispid'),
    (ReservedType: rtInterface; Words: 'dispinterface'),
    (ReservedType: rtOper; Words: 'div'),
    (ReservedType: rtDo; Words: 'do'),
    (ReservedType: rtOper; Words: 'downto'),
    (ReservedType: rtFuncDirective; Words: 'dynamic'),
    (ReservedType: rtElse; Words: 'else'),
    (ReservedType: rtEnd; Words: 'end'),
    (ReservedType: rtExcept; Words: 'except'),
    (ReservedType: rtFuncDirective; Words: 'export'),
    (ReservedType: rtUses; Words: 'exports'),
    (ReservedType: rtForward; Words: 'external'),
    (ReservedType: rtFuncDirective; Words: 'far'),
    (ReservedType: rtReserved; Words: 'file'),
    (ReservedType: rtInitialization; Words: 'finalization'),
    (ReservedType: rtExcept; Words: 'finally'),
    (ReservedType: rtWhile; Words: 'for'),
    (ReservedType: rtForward; Words: 'forward'),
    (ReservedType: rtProcedure; Words: 'function'),
    (ReservedType: rtReserved; Words: 'generic'),
    (ReservedType: rtReserved; Words: 'goto'),
    (ReservedType: rtHelper; Words: 'helper'),
    (ReservedType: rtIf; Words: 'if'),
    (ReservedType: rtImplementation; Words: 'implementation'),
    (ReservedType: rtFuncDirective; Words: 'implements'),
    (ReservedType: rtOper; Words: 'in'),
    (ReservedType: rtFuncDirective; Words: 'index'),
    (ReservedType: rtReserved; Words: 'inherited'),
    (ReservedType: rtInitialization; Words: 'initialization'),
    (ReservedType: rtDirective; Words: 'inline'),
    (ReservedType: rtInterface; Words: 'interface'),
    (ReservedType: rtOper; Words: 'is'),
    (ReservedType: rtVar; Words: 'label'),
    (ReservedType: rtProgram; Words: 'library'),
    (ReservedType: rtFuncDirective; Words: 'message'),
    (ReservedType: rtOper; Words: 'mod'),
    (ReservedType: rtFuncDirective; Words: 'name'),
    (ReservedType: rtFuncDirective; Words: 'near'),
    (ReservedType: rtReserved; Words: 'nil'),
    (ReservedType: rtFuncDirective; Words: 'nodefault'),
    (ReservedType: rtOper; Words: 'not'),
    (ReservedType: rtClass; Words: 'object'),
    (ReservedType: rtOf; Words: 'of'),
    (ReservedType: rtOn; Words: 'on'),
    (ReservedType: rtProcedure; Words: 'operator'), // FPC, supports Operator Overloading
    (ReservedType: rtOper; Words: 'or'),
    (ReservedType: rtReserved; Words: 'out'),
    (ReservedType: rtFuncDirective; Words: 'overload'),
    (ReservedType: rtFuncDirective; Words: 'override'),
    (ReservedType: rtReserved; Words: 'packed'),
    (ReservedType: rtFuncDirective; Words: 'pascal'),
    (ReservedType: rtPrivate; Words: 'private'),
    (ReservedType: rtProcedure; Words: 'procedure'),
    (ReservedType: rtProgram; Words: 'program'),
    (ReservedType: rtProcedure; Words: 'property'),
    (ReservedType: rtPrivate; Words: 'protected'),
    (ReservedType: rtPrivate; Words: 'public'),
    (ReservedType: rtPrivate; Words: 'published'),
    (ReservedType: rtReserved; Words: 'raise'),
    (ReservedType: rtFuncDirective; Words: 'read'),
    (ReservedType: rtFuncDirective; Words: 'readonly'),
    (ReservedType: rtRecord; Words: 'record'),
    (ReservedType: rtFuncDirective; Words: 'register'),
    (ReservedType: rtFuncDirective; Words: 'reintroduce'),
    (ReservedType: rtRepeat; Words: 'repeat'),
    (ReservedType: rtUses; Words: 'requires'),
    (ReservedType: rtFuncDirective; Words: 'resident'),
    (ReservedType: rtVar; Words: 'resourcestring'),
    (ReservedType: rtFuncDirective; Words: 'safecall'),
    (ReservedType: rtReserved; Words: 'set'),
    (ReservedType: rtOper; Words: 'shl'),
    (ReservedType: rtOper; Words: 'shr'),
    (ReservedType: rtFuncDirective; Words: 'stdcall'),
    (ReservedType: rtFuncDirective; Words: 'stored'),
    (ReservedType: rtReserved; Words: 'string'),
    (ReservedType: rtThen; Words: 'then'),
    (ReservedType: rtVar; Words: 'threadvar'),
    (ReservedType: rtOper; Words: 'to'),
    (ReservedType: rtTry; Words: 'try'),
    (ReservedType: rtType; Words: 'type'),
    (ReservedType: rtProgram; Words: 'unit'),
    (ReservedType: rtUntil; Words: 'until'),
    (ReservedType: rtUses; Words: 'uses'),
    (ReservedType: rtVar; Words: 'var'),
    (ReservedType: rtFuncDirective; Words: 'virtual'),
    (ReservedType: rtWhile; Words: 'while'),
    (ReservedType: rtWhile; Words: 'with'),
    (ReservedType: rtFuncDirective; Words: 'write'),
    (ReservedType: rtFuncDirective; Words: 'writeonly'),
    (ReservedType: rtOper; Words: 'xor')
    );
Type
  TKeywordColl = Class(TStrCollection)
  public
    Function Compare(Key1, Key2: Pointer): Integer; override;
  End;

Function StrCase(Source: PChar;
  ACase: TCase): PChar;

Implementation

Function TKeywordColl.Compare(Key1, Key2: Pointer): Integer;
Begin
  Result := StrIComp(Key1, Key2);
End;

Function StrCase(Source: PChar; ACase: TCase): PChar;
Begin
  Case ACase Of
    rfUpperCase: Result := StrUpper(Source);
    rfLowerCase: Result := StrLower(Source);
    rfFirstUp: Begin
        Result := StrLower(Source);
        While Source^ In [' ', Tab] Do
          inc(Source);
        Source^ := UpCase(Char(Source^));
      End;
  Else
    Result := Source;
  End;
End;

End.

