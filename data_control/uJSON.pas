(******************************************************************************)
(* uJSON.pas                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.17                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Own implementation of a JSON-Parser                          *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: The parser does not enforce all JSON syntax rules.           *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 = In Array's erlauben, dass anstatt eines Nodes auch    *)
(*                      einfach nur ein String steht                          *)
(*               0.03 = TJSONArray.clear                                      *)
(*               0.04 = Fix Parsing Error, Array of Array                     *)
(*               0.05 = FindPath                                              *)
(*               0.06 = Abstieg in ArrayElemente in Findpath ging nicht       *)
(*               0.07 = Optional // und /* */ Kommentare wie in JSON5         *)
(*               0.08 = Uncommenter unterstützt nun StringLiterale            *)
(*                      -> Besserer Support für JSON5 Kommentare              *)
(*               0.09 = Array Parsing erlaubte "zu viel" ungültiges           *)
(*                      -> Parser Verschärft                                  *)
(*               0.10 = Einführen des Feldes "Tag"                            *)
(*               0.11 = TJSONComment -> nur im "erstellen" mode sinnvoll      *)
(*               0.12 = .Clone Function                                       *)
(*               0.13 = Erste Versuche eine Zeilennummer aus zu geben, wenn   *)
(*                      der JSON Text falsch ist..                            *)
(*               0.14 = Ignore \u tags instead of throwing an exception       *)
(*               0.15 = ADD IJSONAddobjInterface                              *)
(*                      Parent Property                                       *)
(*                      Validy Checks on TJSONArray.addobj                    *)
(*                      FIX: linebreak on TJSONNodeObj.ToString               *)
(*               0.16 = Multi Dimensional JSON Array's                        *)
(*                      Compact feature for ToString                          *)
(*               0.17 = Free partial trees on syntax errors, require object   *)
(*                      separators, and decode Unicode escapes as UTF-8       *)
(*                      Preserve scalar JSON values and escape control bytes; *)
(*                      reject negative array indices in FindPath             *)
(*                      Preserve literal quotes when cloning array strings    *)
(*                      Reject comments outside strings when disabled         *)
(*                      Validate unquoted values and reject invalid strings   *)
(*                                                                            *)
(******************************************************************************)
Unit uJSON;

{$MODE objfpc}{$H+}

{$INTERFACES CORBA}

Interface

(*
 * Den untenstehenden Text auf
 *
 * http://plantuml.com/
 *
 * einfügen und man bekommt ein Klassendiagram

 @startuml

 abstract class TJSONobj {
 {abstract} +String ToString()
 {abstract} +TJSONobj FindPath(String)
 {abstract} +TJSONobj Clone()
 }

 class TJSONComment {
 + String Comment
 + String ToString()
 }

 class TJSONArray {
 + integer ObjCount
 + TJSONobj Obj[Integer]
 + AddObj(TJSONobj)
 + Clear()
 + String ToString()
 }

 class TJSONNode {
 + integer ObjCount
 + TJSONobj Obj[Integer]
 + AddObj(TJSONobj)
 + String ToString()
 }

 class TJSONNodeObj{
 +String Name
 +TJSONobj Value
 +String ToString()
 }

 class TJSONValue{
 +String Name
 +String Value
 +String ToString()
 }

 class TJSONTerminal{
 +String Value
 +String ToString()
 }

 class TJSONParser{
 +jsonobj Parse(String)
 }

 '\Ableitungshierarchieen \'

 TJSONComment --|> TJSONobj
 TJSONArray --|> TJSONobj
 TJSONNode --|> TJSONobj
 TJSONNodeObj--|> TJSONobj
 TJSONValue --|> TJSONobj
 TJSONTerminal--|> TJSONobj

 '\Die ganzen enthält Beziehungen \'

 TJSONArray *-- TJSONobj
 TJSONNode *-- TJSONobj
 TJSONNodeObj *-- TJSONobj

 TJSONParser*-- TJSONobj

 @enduml

 *)

Const
  SpaceIdent = '  '; // Einrückung pro Schachtelungstiefe

Type

  { TJSONObj }

  TJSONObj = Class // Basisklasse für alle JSON Objekte
  private
    fName: String; // Wird in Findpath benötigt
    fobjs: Array Of TJSONObj; // Das ist damit TJSONObj das Findpath bereitstellen kann, sonst müssten das die Kindklassen alle Redundant implementieren
    fParent: TJSONObj; // das Eltern JSON Element, wenn es eines gibt, sonst NIL
  protected
    Procedure Clear; virtual;
  public
    Tag: PtrInt; // For the User, is not needed by the JSON Library = 0 on Create
    Property Parent: TJSONObj read fParent;
    Constructor Create; virtual;
    Destructor Destroy; override;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; virtual; reintroduce; // Abstract;
    Function FindPath(APath: String): TJSONObj; virtual; // Abstract;

    Function Clone: TJSONObj; virtual; // Abstract; // Das Object "Clont" sich selbst und wird als neue Instanz zurück gegeben
  End;

  IJSONChildobjInterface = Interface
    ['{121D69AC-9738-455E-B2EA-C80C409D588F}'] // Created with Lazarus IDE, using CTRL+SHIFT+G
    Procedure AddObj(JSONObj: TJSONObj);
    Function RemoveObj(JSONObj: TJSONObj): Boolean;
  End;
  { TJSONComment }

  TJSONComment = Class(TJSONObj) // Falls man einen JSON 5.0 Kommantar erzeugen will, der Parser wirft alle Kommentare weg
  private

  public
    Property Comment: String read fName write fname;
    Constructor Create; override;
    Constructor Create(aComment: String); virtual;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;
    Function FindPath(APath: String): TJSONObj; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONArray }

  TJSONArray = Class(TJSONObj, IJSONChildobjInterface)
  private
    Function getObjCount: integer;
    Function getObj(index: integer): TJSONObj;

  public

    Property ObjCount: integer read getObjCount;
    Property Obj[Index: integer]: TJSONObj read getObj; default;

    Constructor Create; override;

    Procedure AddObj(JSONObj: TJSONObj);
    Function RemoveObj(JSONObj: TJSONObj): Boolean;
    Procedure Clear; override;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONNode }

  TJSONNode = Class(TJSONObj, IJSONChildobjInterface)
  private
    Function getObjCount: integer;
    Function getObj(index: integer): TJSONObj;
  public
    Property ObjCount: integer read getObjCount;
    Property Obj[Index: integer]: TJSONObj read getObj;

    Constructor Create; override;

    Procedure AddObj(JSONObj: TJSONObj);
    Function RemoveObj(JSONObj: TJSONObj): Boolean;
    Procedure Clear; override;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONNodeObj }

  TJSONNodeObj = Class(TJSONObj)
  private
    fvalue: TJSONObj;
  protected
    Procedure Clear; override;
  public
    Property Name: String read fName;
    Property Value: TJSONObj read fvalue;

    Constructor Create(aName: String; aValue: TJSONObj); reintroduce;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;

    Function FindPath(APath: String): TJSONObj; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONValue }

  TJSONValue = Class(TJSONObj)
  private
    fvalue: String;
    fvalueIsString: Boolean;
  public
    Property Name: String read fName;
    Property Value: String read fvalue write fvalue;

    Constructor Create(aName, aValue: String; ValueisString: Boolean); reintroduce;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONTerminal }

  TJSONTerminal = Class(TJSONObj)
  private
    fIsString: Boolean;
  public
    Property Value: String read fName write fName;
    Property IsString: Boolean read fIsString write fIsString;

    Constructor Create(aValue: String); reintroduce;

    Function ToString(FrontSpace: String = ''; Compact: Boolean = false): String; override;

    Function Clone: TJSONObj; override;
  End;

  { TJSONParser }

  TJSONParser = Class
  private
    faLine: Integer; // Der Versuch die Aktuelle Zeile bei Fehlermeldungen aus zu geben. Das Problem ist, dass Kommentare vorher gelöscht werden und damit stimmt die Zeilennummer dann nicht mehr :(
    fparsedata: String;
    fparsedataptr: integer;

    Function NextToken(EatToken: Boolean): String;
    Procedure Eat(Token: String);
    Function ReadUnicodeEscape(Var Index: Integer): UTF8String;

    Function ParseHelper(): TJSONObj;
    Function ParseNode(): TJSONNode;
    Function ParseString(): TJSONTerminal;
    Function ParseArray(): TJSONArray;

  public
    SupportJSON5Comments: Boolean; // Wenn True [default=false], dann wird JSON5 Like ein Unkommenting vor dem Parsen gemacht.
    Constructor Create;
    Destructor Destroy; override;

    (*
     * Parst den Text gemäß:
     *   https://www.json.org/json-en.html
     *  Zusätzlich unterstützt der Parser die aus JSON5 bekannten Commentar Strukturen
     *  // -- Einzeilen Kommentare
     *  /* -- Multi Zeilen Kommentare */
     *)
    Function Parse(Data: String): TJSONObj;
  End;

Function StringToJsonString(Data: String): String;

Implementation

Uses sysutils, uncommenter;

Procedure Nop;
Begin

End;

Function StringToJsonString(Data: String): String;
Var
  Builder: TStringBuilder;
  I: SizeInt;
  C: Char;
Begin
  Builder := TStringBuilder.Create(Length(Data) + 2);
  Try
    Builder.Append('"');
    For I := 1 To Length(Data) Do Begin
      C := Data[I];
      Case C Of
        '"', '\', '/': Builder.Append('\').Append(C);
        #8: Builder.Append('\b');
        #9: Builder.Append('\t');
        #10: Builder.Append('\n');
        #12: Builder.Append('\f');
        #13: Builder.Append('\r');
        Else
          If C < #32 Then
            Builder.Append('\u00').Append(IntToHex(Ord(C), 2))
          Else
            Builder.Append(C);
      End;
    End;
    Builder.Append('"');
    Result := Builder.ToString;
  Finally
    Builder.Free;
  End;
End;

{ TJSONComment }

Constructor TJSONComment.Create;
Begin
  Inherited Create;
  Comment := '';
End;

Constructor TJSONComment.Create(aComment: String);
Begin
  Inherited Create;
  Comment := aComment;
End;

Function TJSONComment.ToString(FrontSpace: String; Compact: Boolean): String;
Var
  s: String;
Begin
  s := StringToJsonString(Comment);
  If compact Then Begin
    result := '/*' + copy(s, 2, length(s) - 2) + '*/';
  End
  Else Begin
    result := FrontSpace + '/*' + copy(s, 2, length(s) - 2) + '*/';
  End;
End;

Function TJSONComment.FindPath(APath: String): TJSONObj;
Begin
  Result := Nil;
End;

Function TJSONComment.Clone: TJSONObj;
Begin
  Result := TJSONComment.Create(Comment);
End;

{ TJSONTerminal }

Constructor TJSONTerminal.Create(aValue: String);
Var
  i: Integer;
Begin
  Inherited create;
  fName := aValue;
  If length(aValue) > 1 Then Begin
    If (avalue[1] = '"') And (avalue[length(aValue)] = '"') Then Begin
      fName := copy(aValue, 2, length(aValue) - 2);
    End;
  End;
  fIsString := false;
  For i := 1 To length(fName) Do Begin
    // TODO: Das ist eigentlich falsch, weil es einen String wie 1.1.1 auch als gültige Zahl erkennen würde ..
    If Not (fName[i] In ['0'..'9', DefaultFormatSettings.DecimalSeparator]) Then Begin
      fIsString := true;
      break;
    End;
  End;
End;

Function TJSONTerminal.ToString(FrontSpace: String; Compact: Boolean): String;
Begin
  If Compact Then Begin
    If fIsString Then Begin
      result := StringToJsonString(fName);
    End
    Else Begin
      result := fName;
    End;
  End
  Else Begin
    If fIsString Then Begin
      result := FrontSpace + StringToJsonString(fName);
    End
    Else Begin
      result := FrontSpace + fName;
    End;
  End;
End;

Function TJSONTerminal.Clone: TJSONObj;
Var
  Terminal: TJSONTerminal;
Begin
  Terminal := TJSONTerminal.Create('');
  Terminal.Value := Value;
  Terminal.IsString := IsString;
  Result := Terminal;
End;

{ TJSONObj }

Procedure TJSONObj.Clear;
Var
  i: integer;
Begin
  For i := 0 To high(fobjs) Do Begin
    fobjs[i].Free;
  End;
  setlength(fobjs, 0);
  fobjs := Nil;
End;

Constructor TJSONObj.Create;
Begin
  Inherited Create;
  Tag := 0;
  fName := '';
  fobjs := Nil;
  fParent := Nil;
End;

Destructor TJSONObj.Destroy;
Begin
  Clear;
End;

Function TJSONObj.ToString(FrontSpace: String; Compact: Boolean): String;
Begin
  result := '';
  Raise Exception.Create('ToString not implemented in ' + ClassName);
End;

Function TJSONObj.FindPath(APath: String): TJSONObj;
Var
  elem: String;
  i: Integer;
Begin
  result := Nil;
  If apath = '' Then Begin
    If self Is TJSONComment Then Begin
      result := Nil; // Kommentare werden nicht gefunden, normalerweise nicht mal erstellt
    End
    Else Begin
      result := self;
    End;
    exit;
  End;
  elem := APath;
  APath := '';
  For i := 1 To length(elem) Do Begin
    If elem[i] = '.' Then Begin // Weiter Absteigen
      APath := copy(elem, i + 1, length(elem));
      elem := copy(elem, 1, i - 1);
      break;
    End;
    If elem[i] = '[' Then Begin // Zugriff auf Array Elemente
      APath := copy(elem, i, length(elem));
      elem := copy(elem, 1, i - 1);
      break;
    End;
  End;
  elem := LowerCase(elem);
  If (elem = '') And (pos('[', APath) = 1) Then Begin
    i := strtointdef(copy(APath, 2, pos(']', APath) - 2), -1);
    If (i >= 0) And (i <= high(fobjs)) Then Begin
      elem := copy(APath, pos(']', APath) + 1, length(APath));
      If (elem <> '') And (elem[1] = '.') Then Begin
        // Eigentlich muss beim Zugriff nach [] zwingend ein Punkt kommen,
        // so wäre er nur Optional, wenn man da mehr haben will..
        elem := copy(elem, 2, length(elem));
      End;
      result := fobjs[i].FindPath(elem);
      exit;
    End;
  End;
  For i := 0 To high(fobjs) Do Begin
    If lowercase(fobjs[i].fName) = elem Then Begin
      result := fobjs[i].FindPath(APath);
      exit;
    End;
  End;
End;

Function TJSONObj.Clone: TJSONObj;
Begin
  result := Nil;
  Raise Exception.Create('Clone not implemented in ' + ClassName);
End;

{ TJSONArray }

Constructor TJSONArray.Create;
Begin
  Inherited create;
  fobjs := Nil;
End;

Function TJSONArray.getObj(index: integer): TJSONObj;
Begin
  result := fobjs[Index];
End;

Function TJSONArray.getObjCount: integer;
Begin
  result := length(fobjs);
End;

Function TJSONArray.ToString(FrontSpace: String; Compact: Boolean): String;
Var
  res: String;
  i: integer;
Begin
  If Compact Then Begin
    res := '[';
    For i := 0 To High(fobjs) Do Begin
      If (i > 0) Then Begin
        If (fobjs[i - 1] Is TJSONComment) Then Begin
        End
        Else Begin
          res := res + ',';
        End;
      End;
      res := res + fobjs[i].ToString(FrontSpace + SpaceIdent, Compact);
    End;
    result := res + ']';
  End
  Else Begin
    res := LineEnding + FrontSpace + '[' + LineEnding;
    For i := 0 To High(fobjs) Do Begin
      If (i > 0) Then Begin
        If (fobjs[i - 1] Is TJSONComment) Then Begin
          res := res + LineEnding;
        End
        Else Begin
          res := res + ',' + LineEnding;
        End;
      End;
      res := res + fobjs[i].ToString(FrontSpace + SpaceIdent, Compact);
    End;
    result := res + LineEnding + FrontSpace + ']';
  End;
End;

Function TJSONArray.Clone: TJSONObj;
Var
  i: Integer;
Begin
  Result := TJSONArray.Create;
  setlength(result.fobjs, length(Self.fobjs));
  For i := 0 To high(fobjs) Do Begin
    result.fobjs[i] := Self.fobjs[i].Clone;
    result.fobjs[i].fParent := result;
  End;
End;

Procedure TJSONArray.AddObj(JSONObj: TJSONObj);
Begin
  If (JSONObj Is TJSONTerminal) Or
    (JSONObj Is TJSONNode) Or
    (JSONObj Is TJSONArray) Then Begin
    setlength(fobjs, High(fobjs) + 2);
    fobjs[High(fobjs)] := JSONObj;
    JSONObj.fParent := self;
  End
  Else Begin
    Raise exception.create('Error: ' + JSONObj.ClassName + ' not allowed as element for ' + ClassName);
  End;
End;

Function TJSONArray.RemoveObj(JSONObj: TJSONObj): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To high(fobjs) Do Begin
    If fobjs[i] = JSONObj Then Begin
      result := true;
      fobjs[i].Free;
      For j := i To high(fobjs) - 1 Do Begin
        fobjs[j] := fobjs[j + 1];
      End;
      setlength(fobjs, high(fobjs));
      exit;
    End;
  End;
End;

Procedure TJSONArray.Clear;
Begin
  Inherited Clear;
End;

{ TJSONNode }

Constructor TJSONNode.Create;
Begin
  Inherited create;
  fobjs := Nil;
End;

Function TJSONNode.getObj(index: integer): TJSONObj;
Begin
  result := fobjs[Index];
End;

Function TJSONNode.getObjCount: integer;
Begin
  result := length(fobjs);
End;

Function TJSONNode.ToString(FrontSpace: String; Compact: Boolean): String;
Var
  res: String;
  i: integer;
Begin
  If Compact Then Begin

    res := '{';
    For i := 0 To High(fobjs) Do Begin
      If (i > 0) Then Begin
        If (fobjs[i - 1] Is TJSONComment) Then Begin
        End
        Else Begin
          res := res + ',';
        End;
      End;
      res := res + fobjs[i].ToString(FrontSpace + SpaceIdent, Compact);
    End;
    result := res + '}';
  End
  Else Begin
    res := FrontSpace + '{' + LineEnding;
    For i := 0 To High(fobjs) Do Begin
      If (i > 0) Then Begin
        If (fobjs[i - 1] Is TJSONComment) Then Begin
          res := res + LineEnding;
        End
        Else Begin
          res := res + ',' + LineEnding;
        End;
      End;
      res := res + fobjs[i].ToString(FrontSpace + SpaceIdent, Compact);
    End;
    result := res + LineEnding + FrontSpace + '}';
  End;
End;

Function TJSONNode.Clone: TJSONObj;
Var
  i: Integer;
Begin
  Result := TJSONNode.Create;
  setlength(result.fobjs, length(Self.fobjs));
  For i := 0 To high(fobjs) Do Begin
    result.fobjs[i] := Self.fobjs[i].Clone;
    result.fobjs[i].fParent := result;
  End;
End;

Procedure TJSONNode.AddObj(JSONObj: TJSONObj);
Begin
  // TODO: darf hier wirklich alles hinzugefügt werden ?
  setlength(fobjs, High(fobjs) + 2);
  fobjs[High(fobjs)] := JSONObj;
  JSONObj.fParent := self;
End;

Function TJSONNode.RemoveObj(JSONObj: TJSONObj): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To high(fobjs) Do Begin
    If fobjs[i] = JSONObj Then Begin
      result := true;
      fobjs[i].Free;
      For j := i To high(fobjs) - 1 Do Begin
        fobjs[j] := fobjs[j + 1];
      End;
      setlength(fobjs, high(fobjs));
      exit;
    End;
  End;
End;

Procedure TJSONNode.Clear;
Begin
  Inherited Clear;
End;

{ TJSONParser }

Constructor TJSONParser.Create;
Begin
  SupportJSON5Comments := false;
  Inherited Create;
End;

Destructor TJSONParser.Destroy;
Begin
  // Nichts
End;

Procedure TJSONParser.Eat(Token: String);
Var
  atoken: String;
Begin
  atoken := NextToken(true);
  atoken := LowerCase(atoken);
  Token := LowerCase(Token);
  If (atoken <> Token) Or (fparsedataptr = 0) Or
    (fparsedata[fparsedataptr] = '"') Then Begin
    nop;
    Raise Exception.Create('Expected token : "' + Token + '" got "' + atoken + '" [Line: ' + inttostr(faLine) + ']');
  End;
End;

Function TJSONParser.ReadUnicodeEscape(Var Index: Integer): UTF8String;
Var
  Units: UnicodeString;

  Procedure InvalidEscape;
  Begin
    Raise Exception.Create('Invalid Unicode escape [Line: ' + IntToStr(faLine) + ']');
  End;

  Function ReadCodeUnit: WideChar;
  Var
    I, Value: Integer;
    Digit: Char;
  Begin
    If Index > Length(fparsedata) - 3 Then InvalidEscape;
    Value := 0;
    For I := 1 To 4 Do Begin
      Digit := fparsedata[Index];
      Inc(Index);
      Value := Value * 16;
      Case Digit Of
        '0'..'9': Inc(Value, Ord(Digit) - Ord('0'));
        'a'..'f': Inc(Value, Ord(Digit) - Ord('a') + 10);
        'A'..'F': Inc(Value, Ord(Digit) - Ord('A') + 10);
        Else InvalidEscape;
      End;
    End;
    Result := WideChar(Value);
  End;

Begin
  Units := ReadCodeUnit;
  If (Units[1] >= #$D800) And (Units[1] <= #$DBFF) Then Begin
    If (Index >= Length(fparsedata)) Or
      (fparsedata[Index] <> '\') Or (fparsedata[Index + 1] <> 'u') Then
      InvalidEscape;
    Inc(Index, 2);
    SetLength(Units, 2);
    Units[2] := ReadCodeUnit;
    If (Units[2] < #$DC00) Or (Units[2] > #$DFFF) Then InvalidEscape;
  End
  Else If (Units[1] >= #$DC00) And (Units[1] <= #$DFFF) Then InvalidEscape;
  Result := UTF8Encode(Units);
End;

Function TJSONParser.NextToken(EatToken: Boolean): String;
(*
 * Der JSON Lexer, der einen String nach JSON Tokens separiert whitespace
 * zwischen den Tokens wird ignoriert.
 *)
Const
  tokens: Set Of char = ['[', ']', '{', '}', ':', ',']; // In JSON sind alle separierenden Tokens zum Glück 1 Byte groß ;)
Var
  aindex, I: integer;
  achar: char;
  inString, skipnext, ignorenext, interpretnext: Boolean;
  res: String;
  Encoded: UTF8String;
Begin
  aindex := fparsedataptr + 1;
  If (aindex > length(fparsedata)) Then Begin // Wir haben über das Ende der Datei hinausgelesen
    result := '';
    exit;
  End;
  achar := fparsedata[aindex];
  If EatToken And (achar = #10) Then inc(faLine); // TODO: unter MAC wäre das ein Problem ..
  skipnext := false; // Das nächste Zeichen wird nicht in res aufgenommen
  ignorenext := false; // Das nächste Zeichen wird nicht für die String Auswertung berücksichtigt.
  interpretnext := false; // Das nächste zeichen muss gemäß der \ regel ausgewertet werden.
  res := '';
  inString := false;
  // Wenn das erste zu lesende Zeichen direkt ein Separierendes Token ist
  If (achar In tokens) Then Begin
    If (EatToken) Then Begin
      fparsedataptr := aindex;
    End;
    result := achar;
    exit;
  End;
  // Weitersuchen, so lange bis wir einen Token finden oder der String zuende ist.
  // Ausnahme in Strings, werden Tokens ignoriert!
  While (aindex <= Length(fparsedata)) And ((Not (achar In tokens)) Or inString) Do Begin
    If (Not SupportJSON5Comments) And (Not inString) And
      (achar = '/') And (aindex < Length(fparsedata)) And
      (fparsedata[aindex + 1] In ['/', '*']) Then
      Raise Exception.Create('Comments are disabled [Line: ' + IntToStr(faLine) + ']');
    If inString And (achar < #32) Then
      Raise Exception.Create('Unescaped control character in string [Line: ' + IntToStr(faLine) + ']');
    If ((achar = '"') And (Not ignorenext)) Then Begin
      inString := Not inString;
      skipnext := true;
      If (Not inString) Then Begin // Der String ist fertig, also ist das Token auch abgeschlossen
        If (EatToken) Then Begin
          fparsedataptr := aindex;
          result := res;
        End
        Else Begin
          // A quoted token is never a structural delimiter.
          result := '"';
        End;
        exit;
      End;
    End;
    ignorenext := false;
    If (inString) Then Begin
      If (Not interpretnext) Then Begin
        If (achar = '\') Then Begin
          ignorenext := true;
          skipnext := true; // Das aktuell gelesene Zeichen ist ja ein Steuerzeichen, also nicht in den String übernehmen
          interpretnext := true;
        End;
      End
      Else Begin
        Case achar Of
          '"', '\', '/': ;
          'b': achar := #8;
          'f': achar := #12;
          'n': achar := #10;
          'r': achar := #13;
          't': achar := #9;
          'u': Begin
            Inc(aindex);
            Encoded := ReadUnicodeEscape(aindex);
            // Append UTF-8 bytes without converting to the system code page.
            For I := 1 To Length(Encoded) Do
              res := res + Encoded[I];
            Dec(aindex);
            skipnext := true;
          End;
          Else
            Raise Exception.Create('Invalid string escape [Line: ' + IntToStr(faLine) + ']');
        End;
        interpretnext := false;
      End;
      If (Not skipnext) Then Begin
        res := res + achar;
      End;
    End
    Else Begin
      If (ord(achar) > 32) Then Begin
        res := res + achar;
      End;
    End;
    skipnext := false;
    aindex := aindex + 1;
    If aindex > Length(fparsedata) Then Break;
    achar := fparsedata[aindex];
    If (Not inString) And (res <> '') And ((achar <= #32) Or (achar = '"')) Then Break;
    If EatToken And (achar = #10) Then inc(faLine); // TODO: unter MAC wäre das ein Problem ..
  End;
  If aindex > Length(fparsedata) Then Begin
    If inString Then
      Raise Exception.Create('Unterminated string [Line: ' + IntToStr(faLine) + ']');
    If EatToken Then fparsedataptr := Length(fparsedata);
    Result := res;
    Exit;
  End;
  If (res = '') Then Begin // Wir haben nur Leerzeichen überlesen und eigentlich direkt wieder ein Token gelesen.
    If (EatToken) Then Begin
      fparsedataptr := aindex;
    End;
    result := achar;
    exit;
  End;
  If (EatToken) Then Begin // Wir haben ein Token gelesen und sind ja auf dem nächsten Stehen geblieben also eins weniger
    fparsedataptr := aindex - 1;
  End;
  result := res;
End;

Function TJSONParser.Parse(Data: String): TJSONObj;
Var
  Obj: TJSONObj;
  only_Space: Boolean;
  i: integer;
  uc: TUnCommenter;
Begin
  result := Nil;
  faLine := 1;
  // TODO: Der TUnCommenter muss wieder raus und von "Hand" in Nexttoken implementiert werden -> dann stimmen die faLines wieder !
  If SupportJSON5Comments Then Begin
    uc := TUnCommenter.Create;
    Try
      uc.AddRule('//', '', true);
      uc.AddRule('/*', '*/', False);
      uc.StringLiteral := '"';
{$IFDEF Windows}
      // Theoretisch könnte es sein das das Programm unter Windows läuft aber einen Linux CRT Bekommt,
      // Dann würde Lineweis nicht funktionieren, dafür bräuchte es dann ein Stringreplace .. das #10 in #13#10 umbaut
      // Unter Linux ist es egal, weil der JSON Parser alles < #32 eh weg wirft. und #10 in #13#10 enthalten ist.
      data := StringReplace(data, #10, LineEnding, [rfReplaceAll]);
{$ENDIF}
      data := uc.Uncomment(data);
    Finally
      uc.Free;
    End;
  End;
  fparsedata := Data;
  fparsedataptr := 0;
  Obj := ParseHelper();
  // Wenn nicht alles geparst wurde, dann schauen wir ob es sich nur noch um
  // Leerzeichen handelt die Fehlen oder ob echt Content fehlt.
  If (fparsedataptr <> length(fparsedata)) Then Begin
    only_Space := true;
    For i := fparsedataptr + 1 To length(fparsedata) Do Begin
      If (ord(fparsedata[i]) > 32) Then Begin
        only_Space := false;
        break;
      End;
    End;
    If (Not only_Space) Then Begin
      If assigned(obj) Then obj.free;
      Raise Exception.Create('not all data where parsed, missing "' + copy(fparsedata, fparsedataptr + 1, length(fparsedata)) + '"');
    End;
  End;
  result := Obj;
End;

Function TJSONParser.ParseArray(): TJSONArray;
Var
  Obj: TJSONArray;
  ntoken: String;
Begin
  Obj := TJSONArray.Create;
  Try
    Eat('[');
    ntoken := NextToken(false);
    While ((ntoken <> '') And (ntoken <> ']')) Do Begin
      ntoken := NextToken(false);
      If ntoken = '{' Then Begin
        Obj.AddObj(ParseNode());
      End
      Else Begin
        If ntoken = '[' Then Begin
          obj.addobj(ParseArray());
        End
        Else Begin
          Obj.AddObj(ParseString());
        End;
      End;
      ntoken := NextToken(false);
      If (ntoken <> ']') Then Begin
        Eat(',');
      End;
    End;
    Eat(']');
  Except
    Obj.Free;
    Raise;
  End;
  result := Obj;
End;

Function TJSONParser.ParseHelper(): TJSONObj;
Var
  Obj: TJSONObj;
  atoken: String;
Begin
  Obj := Nil;
  atoken := NextToken(false);
  If (atoken = '{') Then Begin
    Obj := ParseNode();
  End;
  If (atoken = '[') Then Begin
    Obj := ParseArray();
  End;
  result := Obj;
End;

Function IsJSONNumber(Const Token: String): Boolean;
Var
  I: SizeInt;

  Function ConsumeDigits: Boolean;
  Var
    Start: SizeInt;
  Begin
    Start := I;
    While (I <= Length(Token)) And (Token[I] In ['0'..'9']) Do Inc(I);
    Result := I > Start;
  End;

Begin
  I := 1;
  If (I <= Length(Token)) And (Token[I] = '-') Then Inc(I);
  If (I <= Length(Token)) And (Token[I] = '0') Then
    Inc(I)
  Else If Not ConsumeDigits Then Exit(False);
  If (I <= Length(Token)) And (Token[I] = '.') Then Begin
    Inc(I);
    If Not ConsumeDigits Then Exit(False);
  End;
  If (I <= Length(Token)) And (Token[I] In ['e', 'E']) Then Begin
    Inc(I);
    If (I <= Length(Token)) And (Token[I] In ['+', '-']) Then Inc(I);
    If Not ConsumeDigits Then Exit(False);
  End;
  Result := I > Length(Token);
End;

Procedure RequireJSONLiteral(Const Token: String; Line: Integer);
Begin
  If (Token <> 'true') And (Token <> 'false') And (Token <> 'null') And
    (Not IsJSONNumber(Token)) Then
    Raise Exception.Create('Invalid JSON value [Line: ' + IntToStr(Line) + ']');
End;

Function TJSONParser.ParseNode(): TJSONNode;
Var
  Obj: TJSONNode;
  valuename, ntoken: String;
  isstring: Boolean;
  Value: TJSONValue;
Begin
  Obj := TJSONNode.Create;
  Try
    Eat('{');
    ntoken := NextToken(false);
    While (ntoken <> '') And (ntoken <> '}') Do Begin
      valuename := NextToken(true);
      Eat(':');
      ntoken := NextToken(false);
      If (ntoken = '[') Then Begin // Der Wert des Nodes ist ein Array von Nodes
        Obj.AddObj(TJSONNodeObj.Create(valuename, ParseArray()));
      End
      Else Begin
        If (ntoken = '{') Then Begin // Der Wert des Nodes ist ein Node
          Obj.AddObj(TJSONNodeObj.Create(valuename, ParseNode()));
        End
        Else Begin
          // Der Wert ist einfach nur wieder ein String / Integer / ..
          ntoken := NextToken(true);
          isstring := (fparsedataptr > 0) And (fparsedata[fparsedataptr] = '"');
          If Not isstring Then Begin
            RequireJSONLiteral(ntoken, faLine);
            If ntoken = 'null' Then ntoken := '';
          End;
          Value := TJSONValue.Create(valuename, ntoken, isstring);
          Obj.AddObj(Value);
        End;
      End;
      ntoken := NextToken(false);
      If (ntoken <> '') And (ntoken <> '}') Then Begin
        Eat(',');
      End;
    End;
    Eat('}');
  Except
    Obj.Free;
    Raise;
  End;
  result := Obj;
End;

Function TJSONParser.ParseString(): TJSONTerminal;
Var
  Token: String;
  IsString: Boolean;
Begin
  Token := NextToken(true);
  IsString := (fparsedataptr > 0) And (fparsedata[fparsedataptr] = '"');
  If Not IsString Then RequireJSONLiteral(Token, faLine);
  Result := TJSONTerminal.Create('');
  Result.Value := Token;
  Result.IsString := IsString;
End;

{ TJSONValue }

Constructor TJSONValue.Create(aName, aValue: String; ValueisString: Boolean);
Begin
  Inherited Create;
  fName := aName;
  fvalue := aValue;
  fvalueIsString := ValueisString;
End;

Function TJSONValue.ToString(FrontSpace: String; Compact: Boolean): String;
Begin
  Result := StringToJsonString(fName) + ':';
  If fvalueIsString Then
    Result := Result + StringToJsonString(fvalue)
  Else If fvalue = '' Then
    Result := Result + 'null'
  Else
    Result := Result + fvalue;
  If Not Compact Then Result := FrontSpace + Result;
End;

Function TJSONValue.Clone: TJSONObj;
Begin
  Result := TJSONValue.Create(name, value, fvalueIsString);
End;

{ TJSONNodeObj }

Constructor TJSONNodeObj.Create(aName: String; aValue: TJSONObj);
Begin
  Inherited Create;
  fName := aName;
  fvalue := aValue;
  fvalue.fParent := self;
End;

Function TJSONNodeObj.ToString(FrontSpace: String; Compact: Boolean): String;
Var
  maybeLE: String;
Begin
  If Compact Then Begin
    If assigned(fvalue) Then Begin
      result := StringToJsonString(fName) + ':' + fvalue.ToString(FrontSpace, Compact);
    End
    Else Begin
      result := StringToJsonString(fName) + ':""';
    End;
  End
  Else Begin
    If assigned(fvalue) Then Begin
      maybeLE := '';
      If fvalue Is TJSONNode Then maybeLE := LineEnding;
      result := FrontSpace + StringToJsonString(fName) + ':' + maybeLE + fvalue.ToString(FrontSpace, Compact);
    End
    Else Begin
      result := FrontSpace + StringToJsonString(fName) + ':""';
    End;
  End;
End;

Procedure TJSONNodeObj.Clear;
Begin
  If assigned(fvalue) Then Begin
    fvalue.free;
  End;
  fvalue := Nil;
  Inherited Clear;
End;

Function TJSONNodeObj.FindPath(APath: String): TJSONObj;
Begin
  If apath = '' Then Begin
    result := fvalue;
  End
  Else Begin
    Result := Inherited FindPath(APath);
    If (Not assigned(result)) And assigned(fvalue) Then Begin
      result := fvalue.FindPath(APath);
    End;
  End;
End;

Function TJSONNodeObj.Clone: TJSONObj;
Begin
  If assigned(fvalue) Then Begin
    Result := TJSONNodeObj.Create(Name, fvalue.Clone);
  End
  Else Begin
    Result := TJSONNodeObj.Create(Name, Nil);
  End;
End;

End.

