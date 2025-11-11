{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJSON.pas, released 2015-01-14.
The Initial Author of this file is Christian-W. Budde.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
}

Unit SynHighlighterJSON;

{$I SynDefines.inc}

Interface

Uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  Classes;


Type
  TtkTokenKind = (tkString, tkReserved, tkNull, tkNumber, tkSpace,
    tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsAttribute, rsObjectValue, rsArrayValue);

Type

  { TSynJSONSyn }

  TSynJSONSyn = Class(TSynCustomHighLighter)
  private
    FRange: TRangeState;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FReservedAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FValueAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    Run: LongInt;
    fLineRef: String;
    fLine: PChar;
    fLineNumber: Integer;
    fLineLen: Integer;

    Procedure CloseArrayProc;
    Procedure CloseObjectProc;
    Procedure ColonProc;
    Procedure CommaProc;
    Procedure CRProc;
    Function IsLineEnd(_Run: Integer): Boolean;
    Procedure LFProc;
    Procedure NullProc;
    Procedure NumberProc;
    Procedure OpenArrayProc;
    Procedure OpenObjectProc;
    Procedure ReservedWordProc;
    Procedure SpaceProc;
    Procedure StringProc;
    Procedure SymbolProc;
    Procedure UnknownProc;
  protected
    Function GetSampleSource: String; override;
    Function IsFilterStored: Boolean; override;
    Procedure SetLine(Const NewValue: String; LineNumber: Integer); override;
  public
    Class Function GetLanguageName: String; override;
    Class Function GetFriendlyLanguageName: String;
  public
    Constructor Create(AOwner: TComponent); override;
    Function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    Function GetEol: Boolean; override;
    Function GetRange: Pointer; override;
    Function GetToken: String; override;
    Function GetTokenID: TtkTokenKind;
    Function GetTokenAttribute: TSynHighlighterAttributes; override;
    Function GetTokenKind: Integer; override;
    Procedure Next; override;
    Procedure SetRange(Value: Pointer); override;
    Procedure ResetRange; override;
    Procedure GetTokenEx(Out TokenStart: PChar; Out TokenLength: integer); override;
    Function GetTokenPos: Integer; override;
  published
    Property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
    Property ReservedAttri: TSynHighlighterAttributes read FReservedAttri
      write FReservedAttri;
    Property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    Property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    Property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    Property ValueAttri: TSynHighlighterAttributes read FValueAttri
      write FValueAttri;
  End;

Implementation

Uses
  SynEditStrConst,
  SynEditStrConstExtra;


{ TSynJSONSyn }

Function TSynJSONSyn.IsLineEnd(_Run: Integer): Boolean;
Begin
  Result := (_Run >= FLineLen) Or (FLine[_Run] = #10) Or (FLine[_Run] = #13);
End;

Constructor TSynJSONSyn.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  //FCaseSensitive := True;

  // Attribute
  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttributeName);
  FAttributeAttri.Foreground := clNavy;
  AddAttribute(FAttributeAttri);

  // reserved words ("true", "false", "null")
  FReservedAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  FReservedAttri.Style := [fsBold];
  AddAttribute(FReservedAttri);

  // numbers
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);

  // spaces
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);

  // symbols
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  FSymbolAttri.Foreground := clGreen;
  AddAttribute(FSymbolAttri);

  // Value
  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  FValueAttri.Foreground := clBlue;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := SYNS_FilterJSON;
  FRange := rsUnknown;
End;

Procedure TSynJSONSyn.CloseArrayProc;
Begin
  SymbolProc;
  FRange := rsUnknown;
End;

Procedure TSynJSONSyn.CloseObjectProc;
Begin
  SymbolProc;
  FRange := rsUnknown;
End;

Procedure TSynJSONSyn.ColonProc;
Begin
  SymbolProc;
  FRange := rsObjectValue;
End;

Procedure TSynJSONSyn.CommaProc;
Begin
  SymbolProc;
  If FRange = rsObjectValue Then
    FRange := rsAttribute;
End;

Procedure TSynJSONSyn.CRProc;
Begin
  FTokenID := tkSpace;
  Inc(Run);
  If FLine[Run] = #10 Then Inc(Run);
End;

Procedure TSynJSONSyn.LFProc;
Begin
  FTokenID := tkSpace;
  Inc(Run);
End;

Procedure TSynJSONSyn.NullProc;
Begin
  FTokenID := tkNull;
  Inc(Run);
End;

Procedure TSynJSONSyn.NumberProc;

  Function ExpectDigit: Boolean;
  Begin
    Result := FLine[Run] In ['0'..'9'];
    While FLine[Run] In ['0'..'9'] Do
      Inc(Run);
  End;

Begin
  FTokenID := tkNumber;

  If FLine[Run] = '-' Then
    Inc(Run);

  // ensure that a zero is followed by a dot
  If FLine[Run] = '0' Then
    If FLine[Run + 1] <> '.' Then Begin
      FTokenID := tkUnknown;
      While (FLine[Run] <> #32) And Not IsLineEnd(Run) Do
        Inc(Run);
      Exit;
    End;

  // at least any digit must appear here
  If Not ExpectDigit Then Begin
    FTokenID := tkUnknown;
    While (FLine[Run] <> #32) And Not IsLineEnd(Run) Do
      Inc(Run);
    Exit;
  End;

  // check for dot
  If FLine[Run] = '.' Then Begin
    // advance
    Inc(Run);

    // at least any digit must appear after a dot!
    If Not ExpectDigit Then Begin
      FTokenID := tkUnknown;
      While (FLine[Run] <> #32) And Not IsLineEnd(Run) Do
        Inc(Run);
      Exit;
    End;
  End;

  // check for an exponent
  If FLine[Run] In ['e', 'E'] Then Begin
    Inc(Run);

    // allow +/- here
    If (FLine[Run] In ['+', '-']) Then
      Inc(Run);

    // at least any digit must appear here
    If Not ExpectDigit Then Begin
      FTokenID := tkUnknown;
      While (FLine[Run] <> #32) And Not IsLineEnd(Run) Do
        Inc(Run);
      Exit;
    End;
  End;
End;

Procedure TSynJSONSyn.OpenArrayProc;
Begin
  SymbolProc;
  FRange := rsArrayValue;
End;

Procedure TSynJSONSyn.OpenObjectProc;
Begin
  SymbolProc;
  FRange := rsAttribute;
End;

Procedure TSynJSONSyn.ReservedWordProc;

  Procedure SkipToken;
  Begin
    While (FLine[Run] <> #32) And (FLine[Run] <> ',') And Not IsLineEnd(Run) Do
      Inc(Run);
  End;

Begin
  FTokenID := tkUnknown;
  Case FLine[Run] Of
    'n':
      If (FLine[Run + 1] = 'u') And
        (FLine[Run + 2] = 'l') And
        (FLine[Run + 3] = 'l') Then Begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      End
      Else
        SkipToken;
    't':
      If (FLine[Run + 1] = 'r') And
        (FLine[Run + 2] = 'u') And
        (FLine[Run + 3] = 'e') Then Begin
        FTokenID := tkReserved;
        Inc(Run, 4);
      End
      Else
        SkipToken;
    'f':
      If (FLine[Run + 1] = 'a') And
        (FLine[Run + 2] = 'l') And
        (FLine[Run + 3] = 's') And
        (FLine[Run + 4] = 'e') Then Begin
        FTokenID := tkReserved;
        Inc(Run, 5);
      End
      Else
        SkipToken;
  Else
    SkipToken;
  End;
End;

Procedure TSynJSONSyn.SpaceProc;
Begin
  Inc(Run);
  FTokenID := tkSpace;
  While (FLine[Run] <= #32) And Not IsLineEnd(Run) Do
    Inc(Run);
End;

Procedure TSynJSONSyn.StringProc;

  Function IsHex(Digit: AnsiChar): Boolean; overload;
  Begin
    Result := (Digit In ['0'..'9', 'A'..'F', 'a'..'f']);
  End;

  Function IsHex(Digit: WideChar): Boolean; overload;
  Begin
    Result := (Digit In ['0'..'9', 'A'..'F', 'a'..'f']);
  End;

Begin
  FTokenID := tkString;

  Repeat
    Inc(Run);
    Case FLine[Run] Of
      '"': Begin
          Inc(Run);
          Break;
        End;
      '\':
        Case FLine[Run + 1] Of
          '"', '/', '\', 'b', 'f', 'n', 'r', 't':
            Inc(Run);
          'u': Begin
              Inc(Run);
              If Not (IsHex(FLine[Run + 1]) And IsHex(FLine[Run + 2]) And
                IsHex(FLine[Run + 3]) And IsHex(FLine[Run + 4])) Then Begin
                // a 4 hex digit is expected
                FTokenID := tkUnknown;
                While Not (FLine[Run] In [#32, '"']) And Not IsLineEnd(Run) Do
                  Inc(Run);
                Exit;
              End;
              Inc(Run, 4);
            End;
        End;
    End;
  Until IsLineEnd(Run);
End;

Procedure TSynJSONSyn.SymbolProc;
Begin
  Inc(Run);
  FTokenID := tkSymbol;
End;

Procedure TSynJSONSyn.UnknownProc;
Begin
  Inc(Run);
  FTokenID := tkUnknown;
End;

Procedure TSynJSONSyn.SetLine(Const NewValue: String; LineNumber: Integer);
Begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  fLineLen := Length(NewValue);
  Run := 0;
  fLineNumber := LineNumber;

  Next;
End;

Procedure TSynJSONSyn.Next;
Begin
  FTokenPos := Run;
  Case FLine[Run] Of
    #0: NullProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #10: LFProc;
    #13: CRProc;
    '0'..'9', '-': NumberProc;
    't',
      'f',
      'n': ReservedWordProc;
    '"': StringProc;
    ':': ColonProc;
    '{': OpenObjectProc;
    '[': OpenArrayProc;
    '}': CloseObjectProc;
    ']': CloseArrayProc;
    ',': CommaProc;
  Else
    UnknownProc;
  End;

End;

Function TSynJSONSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
Begin
  Case Index Of
    SYN_ATTR_KEYWORD: Result := FReservedAttri;
    SYN_ATTR_IDENTIFIER: Result := FAttributeAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_STRING: Result := FValueAttri;
  Else
    Result := Nil;
  End;
End;

Function TSynJSONSyn.GetEol: Boolean;
Begin
  Result := Run = FLineLen + 1;
End;

Function TSynJSONSyn.GetRange: Pointer;
Begin
  Result := Pointer(PtrUInt(FRange));
End;

Function TSynJSONSyn.GetTokenID: TtkTokenKind;
Begin
  Result := FTokenID;
End;

Function TSynJSONSyn.GetTokenAttribute: TSynHighlighterAttributes;
Begin
  Case GetTokenID Of
    tkString:
      If FRange In [rsObjectValue, rsArrayValue] Then
        Result := FValueAttri
      Else
        Result := FAttributeAttri;
    tkReserved: Result := FReservedAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FAttributeAttri;
  Else
    Result := Nil;
  End;
End;

Function TSynJSONSyn.GetTokenKind: Integer;
Begin
  Result := Ord(FTokenID);
End;

Procedure TSynJSONSyn.ResetRange;
Begin
  FRange := rsUnknown;
End;

Procedure TSynJSONSyn.SetRange(Value: Pointer);
Begin
  FRange := TRangeState(UIntPtr(Value));
End;

Procedure TSynJSONSyn.GetTokenEx(Out TokenStart: PChar; Out TokenLength: integer);
Begin
  TokenLength := Run - fTokenPos;
  TokenStart := FLine + fTokenPos;
End;

Function TSynJSONSyn.GetTokenPos: Integer;
Begin
  Result := fTokenPos;
End;

Function TSynJSONSyn.GetToken: String;
Var
  Len: LongInt;
Begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
End;


Function TSynJSONSyn.IsFilterStored: Boolean;
Begin
  Result := FDefaultFilter <> SYNS_FilterJSON;
End;

Class Function TSynJSONSyn.GetLanguageName: String;
Begin
  Result := SYNS_LangJSON;
End;

Function TSynJSONSyn.GetSampleSource: String;
Begin
  Result :=
    '{'#13#10 +
    '  "firstName": "John",'#13#10 +
    '  "lastName": "Smith",'#13#10 +
    '  "isAlive": true,'#13#10 +
    '  "age": 25,'#13#10 +
    '  "height_cm": 167.6,'#13#10 +
    '  "address": {'#13#10 +
    '    "streetAddress": "21 2nd Street",'#13#10 +
    '    "city": "New York",'#13#10 +
    '    "state": "NY",'#13#10 +
    '    "postalCode": "10021-3100"'#13#10 +
    '  },'#13#10 +
    '  "phoneNumbers": ['#13#10 +
    '    {'#13#10 +
    '      "type": "home",'#13#10 +
    '      "number": "212 555-1234"'#13#10 +
    '    },'#13#10 +
    '    {'#13#10 +
    '      "type": "office",'#13#10 +
    '      "number": "646 555-4567"'#13#10 +
    '    }'#13#10 +
    '  ],'#13#10 +
    '  "face": "\uD83D\uDE02",'#13#10 +
    '  "children": [],'#13#10 +
    '  "spouse": null'#13#10 +
    '}';
End;

Class Function TSynJSONSyn.GetFriendlyLanguageName: String;
Begin
  Result := SYNS_LangJSON;
End;

Initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynJSONSyn);
{$ENDIF}
End.

