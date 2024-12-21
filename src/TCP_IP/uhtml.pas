(******************************************************************************)
(* uhtml.pas                                                       04.03.2021 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Autor       : Corpsman                                                     *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is a HTML DOM Parser it will parse the content of a     *)
(*               HTML-File into a THTMLDocument, which itself can than        *)
(*               be used for further works                                    *)
(*                                                                            *)
(* Usage:       THTMLParser.Create                                            *)
(*              doc := THTMLParser.Parse()                                    *)
(*                                                                            *)
(* License     : This component is postcardware for non commercial use only.  *)
(*               If you like the component, send me a postcard:               *)
(*                                                                            *)
(*                    Uwe Schächterle                                         *)
(*                    Buhlstraße 85                                           *)
(*                    71384 Weinstadt - Germany                               *)
(*                                                                            *)
(*               It is not allowed to change or remove this license from any  *)
(*               source file of the project.                                  *)
(*                                                                            *)
(*                                                                            *)
(* Warranty    : There is no warranty, use at your own risk.                  *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(* Known Bugs  : none                                                         *)
(*                                                                            *)
(******************************************************************************)

Unit uhtml;

{$MODE objfpc}{$H+}

Interface

(*
 * Verwendete Quellen: https://www.w3schools.com/tags/tag_doctype.asp
 *                     https://www.w3.org/TR/html52/introduction.html#a-quick-introduction-to-html
 *                     https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
 *)

Uses
  Classes, SysUtils;

Type

  TAttribut = Record
    Name: String;
    Value: String;
  End;

  { THTMLElement }

  THTMLElement = Class // Der BasisTag von dem Alles Ableitet
  private
    fName: String;
    fAttributes: Array Of TAttribut;
    fContent: Array Of THTMLElement;
    Function getAttribute(Index: integer): TAttribut;
    Function getAttributeCount: integer;
    Function getContent(Index: integer): THTMLElement;
    Function getContentCount: integer;
    Procedure parse(); virtual;
    Function parseAttributes(): Boolean; virtual;
    Function AttribsToString(): String;
  public
    Property Name: String read fName;
    Property AttributeCount: integer read getAttributeCount;
    Property Attribute[Index: integer]: TAttribut read getAttribute;
    Property ContentCount: integer read getContentCount;
    Property Content[Index: integer]: THTMLElement read getContent;
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = false): String; virtual;
  End;

  { THTMLComment }

  THTMLComment = Class(THTMLElement) // <!--Text-->
  private
    fComment: String;
    Procedure parse(); override;
  public
    Property Comment: String read fComment;
    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = false): String; override;
  End;

  { TDocumentType }

  TDocumentType = Class(THTMLElement) // <!DOCTYPE html Attribute >
  private
    Procedure parse(); override;
  public
    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = false): String; override;
  End;

  { TSingleTagElement }

  TSingleTagElement = Class(THTMLElement) // <br>, <hr> ...
  private
    Procedure parse(); override;
  public
    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = false): String; override;
  End;

  { TTextElement }

  TTextElement = Class(THTMLElement)
  private
    fText: String;
    Procedure parse(); override;
  public
    Property Text: String read fText;
    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = false): String; override;
  End;

  { THTMLDocument }

  THTMLDocument = Class(tobject)
  private
    fDocumentType: TDocumentType;
    fContent: THTMLElement;
    Procedure parse();
  public
    Property Document: THTMLElement read fContent;
    Property DocumentType: TDocumentType read fDocumentType;
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function ToString(FrontSpace: String = ''; IncludeComments: Boolean = False): String;
  End;

  { THTMLParser }

  THTMLParser = Class
  private
    fLastError: String;
    fLastWarning: String;
  public
    Property LastError: String read fLastError;
    Property LastWarning: String read fLastWarning;
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Function Parse(Content: String): THTMLDocument;
  End;

Var
  SpaceIdent: String = '  ';

Implementation

Const
  // Quelle: https://www.lifewire.com/html-singleton-tags-3468620
  HTMLSingleTonElements: Array[0..3] Of String =
  (
    //, 'area'
    //, 'base'
    'br'
    //, 'col'
    //, 'command'
    //, 'embed'
    , 'hr'
    , 'img'
    //, 'input'
    //, 'keygen'
    //, 'link'
    , 'meta'
    //, 'param'
    //, 'source'
    //, 'track'
    //, 'wbr'
    );

Procedure Nop();
Begin

End;

Function IsSingleTon(Value: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  value := lowercase(Value);
  For i := 0 To high(HTMLSingleTonElements) Do Begin
    If HTMLSingleTonElements[i] = Value Then Begin
      result := true;
      break;
    End;
  End;
End;

(*
 * !! Achtung, diese Variablen sorgen dafür das die Unit nicht Thread Safe ist !!
 *)
Var
  LError, LWarning: String;
  ALine, Aindex: Integer;
  aContent: String;

  (*
   * Dieser Lexer Zerlegt nach folgenden Mustern
   * Seperatoren = [#0 .. #32] Trennen Tokens werden aber übersprungen
   * Operatoren =
   * [<, >, =, <!--, -->]
   *
   *)

Function PopToken(): String;
Var
  i: Integer;
  inString: Boolean;
Begin
  result := '';
  // 1. Überspringen aller Separatoren
  While (aIndex < length(aContent)) And (aContent[aIndex] <= ' ') Do Begin
    If aContent[aIndex] = #10 Then inc(aline);
    inc(aIndex);
  End;
  // Lesen wir ein ", dann handelt es sich um einen String, dieser wird wieder durch ein "
  // Beendet
  inString := aContent[aIndex] = '"';
  If inString Then Begin
    For i := aIndex + 1 To length(aContent) Do Begin
      If aContent[aIndex] = #10 Then inc(aline);
      If aContent[i] = '"' Then Begin
        result := copy(aContent, aIndex, i - aIndex + 1);
        aIndex := i + 1;
        break;
      End;
    End;
  End
  Else Begin
    // Wir Lesen bis ein Separator oder ein Operator kommt
    For i := aIndex To length(aContent) Do Begin
      If (aContent[i] <= ' ') Or (aContent[i] In ['<', '>', '=', '-']) Then Begin
        (*
         * Hier gibt es 2 Fälle
         * 1. Das Ergebnis ist direht ein Separator
         * 2. Das Ergebnis ist ein "unbekanntes Token" gefolgt von einem Separator
         *)
        If i = aIndex Then Begin
          Case aContent[i] Of
            '-': Begin
                If (aContent[i + 1] = '-') And (aContent[i + 2] = '>') Then Begin
                  result := '-->';
                  Aindex := Aindex + 3;
                  break;
                End;
              End;
            '<': Begin
                If (aContent[i + 1] = '!') And (aContent[i + 2] = '-') And (aContent[i + 3] = '-') Then Begin
                  result := '<!--';
                  Aindex := Aindex + 4;
                End
                Else Begin
                  result := '<';
                  Aindex := Aindex + 1;
                End;
                break;
              End;
          Else Begin
              result := aContent[i];
              Aindex := Aindex + 1;
              break;
            End;
          End;
        End
        Else Begin
          If aContent[i] <> '-' Then Begin
            result := copy(aContent, aIndex, i - aIndex);
            aIndex := aIndex + length(result);
            break;
          End;
        End;
      End;
    End;
  End;
End;

Function PopText(): String;
Var
  oline, i: integer;
  instring, block: Boolean;
  //dbg: String; // Debug
Begin
  oline := ALine;
  result := '';
  If LError <> '' Then exit;
  //dbg := '';
  instring := false;
  block := false;
  For i := aIndex To length(aContent) Do Begin
    //dbg := dbg + aContent[i];
    If (aContent[i] = '/') And (aContent[i + 1] = '/') Then Begin
      block := true;
    End;
    If aContent[i] = '''' Then Begin
      instring := Not instring;
    End;
    If aContent[i] = #10 Then Begin
      inc(aline);
      block := false;
    End;
    If (aContent[i] = '<') And (aContent[i + 1] = '/') // Ein Text wird beendet durch den start eines neuen Elements
    Or ((Not instring) And (Not block) And ((aContent[i] = '<')))
      Then Begin
      result := copy(aContent, Aindex, i - Aindex);
      Aindex := Aindex + length(result);
      exit;
    End;
  End;
  LError := 'Error, reached end of text, while parsing in line [' + inttostr(oline) + ']';
End;

Function PopComment(): String;
Var
  oline, i: integer;
Begin
  oline := ALine;
  result := '';
  If LError <> '' Then exit;
  For i := aIndex To length(aContent) - 2 Do Begin
    If aContent[i] = #10 Then inc(aline);
    If (aContent[i] = '-') And (aContent[i + 1] = '-') And (aContent[i + 2] = '>') Then Begin // --> beendet einen Kommentar
      result := copy(aContent, Aindex, i - Aindex);
      Aindex := Aindex + length(result);
      exit;
    End;
  End;
  LError := 'Error, reached end of text, while parsing in line [' + inttostr(oline) + ']';
End;

Function TopToken(LookAhead: Integer = 0): String;
Var
  i: Integer;
  oaline, oaindex: integer;
Begin
  // Bakup der Alten Werte
  oaline := ALine;
  oaindex := Aindex;
  // Weglesen des LookAhead
  For i := 0 To LookAhead - 1 Do Begin
    PopToken();
  End;
  // Das gewünschte Element Lesen
  result := PopToken();
  // Wieder Herstellen der Index, da ja Top und nicht Pop
  ALine := oaline;
  Aindex := oaindex;
End;

Function Eat(Token: String): Boolean;
Var
  s: String;
Begin
  If LError <> '' Then Begin
    result := false;
    exit;
  End;
  s := PopToken();
  If lowercase(s) = LowerCase(Token) Then Begin
    result := True;
  End
  Else Begin
    LError := 'Error, expected: "' + Token + '" got "' + s + '" [Line: ' + inttostr(aline) + ']';
    result := false;
  End;
End;

{ TTextElement }

Procedure TTextElement.parse();
Begin
  fText := PopText();
End;

Function TTextElement.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Begin
  Result := trim(fText);
End;

{ TSingleTagElement }

Procedure TSingleTagElement.parse();
Begin
  If Not Eat('<') Then exit;
  fName := PopToken();
  If Not parseAttributes() Then exit;
  If TopToken() = '/' Then Begin // Eigentlich sollten die Single Tag Elemente kein /> Closing Tag haben, aber manche haben es wohl doch *facepalm*
    If Not Eat('/') Then exit;
  End;
  If Not Eat('>') Then exit;
End;

Function TSingleTagElement.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Begin
  result := FrontSpace + '<' + Name + attribsToString() + '>';
End;

{ TDocumentType }

Procedure TDocumentType.parse();
Begin
  If Not Eat('<') Then exit;
  If Not Eat('!DOCTYPE') Then exit;
  If Not Eat('html') Then exit;
  fName := 'HTML';
  If Not parseAttributes() Then exit;
  If Not Eat('>') Then exit;
End;

Function TDocumentType.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Begin
  result := FrontSpace + '<!DOCTYPE ' + Name + attribsToString() + '>';
End;

{ THTMLDocument }

Constructor THTMLDocument.Create();
Begin
  fDocumentType := TDocumentType.Create();
  fContent := THTMLElement.Create();
End;

Destructor THTMLDocument.Destroy();
Begin
  fDocumentType.free;
  fContent.Free;
End;

Function THTMLDocument.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Begin
  result :=
    fDocumentType.ToString(FrontSpace) + LineEnding +
    fContent.ToString(FrontSpace, IncludeComments);
End;

Procedure THTMLDocument.parse();
Begin
  If LError <> '' Then exit;
  fDocumentType.Parse;
  If LError <> '' Then exit;
  fContent.parse();
End;

{ TComment }

Procedure THTMLComment.parse();
Begin
  If Not eat('<!--') Then exit;
  fComment := PopComment();
  If Not eat('-->') Then exit;
End;

Function THTMLComment.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Begin
  // Kommentare fliegen raus !
  If IncludeComments Then Begin
    Result := '<!--' + fComment + '-->';
  End
  Else Begin
    Result := '';
  End;
End;

{ THTMLElement }

Procedure THTMLElement.parse();
Var
  aToken: THTMLElement;
  lname, l1, l2: String;
  ln: integer; // Debug
Begin
  If Not eat('<') Then exit;
  fName := PopToken();
  ln := ALine;
  If Pos('/', fName) = 1 Then Begin
    (*
     * Kommt dieser Fehler, dann hat den Parser etwas durcheinander gebracht was im übergeordneten
     * Element geparst wurde. Raussteppen und dann nach sehen !
     *)
    If LError = '' Then Begin
      LError := 'Error, invalid tag name "' + fName + '" in line [' + inttostr(ALine) + ']';
    End;
    exit;
  End;
  lname := LowerCase(Name);
  fContent := Nil;
  If Not parseAttributes() Then exit;
  l1 := TopToken();
  If l1 = '/' Then Begin // Das Element definiert nur Attribute und endet hier bereits mit "/>"
    If Not eat('/') Then exit;
    If Not eat('>') Then exit;
    exit;
  End;
  If Not eat('>') Then exit;
  l1 := TopToken();
  l2 := lowercase(TopToken(1));
  While (LError = '') And (Not ((l1 = '<') And (l2 = '/' + lName))) Do Begin
    // Wenn hier ein schließendes Token Kommt, das uns nicht geöffnet hat
    If (l1 = '<') And (pos('/', l2) = 1) Then Begin
      If LWarning <> '' Then Begin
        LWarning := LWarning + LineEnding;
      End;
      LWarning := LWarning + 'missing: "</' + lName + '>" in line [' + inttostr(ALine) + '] for <' + fName + '> from line ' + inttostr(ln);
      exit;
      //  If Not Eat('<') Then exit;
      //  If Not Eat(l2) Then exit;
      //  If Not Eat('>') Then exit;
      //  l1 := TopToken();
      //  l2 := lowercase(TopToken(1));
      //  Continue;
    End;

    //If (l1 = '<') And (pos('/', l2) = 1) Then Begin
    //  If LWarning <> '' Then Begin
    //    LWarning := LWarning + LineEnding;
    //  End;
    //  LWarning := LWarning + 'Invalid: "<' + l2 + '" in line [' + inttostr(ALine) + ']';
    //  If Not Eat('<') Then exit;
    //  If Not Eat(l2) Then exit;
    //  If Not Eat('>') Then exit;
    //  l1 := TopToken();
    //  l2 := lowercase(TopToken(1));
    //  Continue;
    //End;
    Case l1 Of
      '<': Begin
          If isSingleTon(l2) Then Begin
            atoken := TSingleTagElement.Create();
          End
          Else Begin
            atoken := THTMLElement.Create();
          End;
        End;
      '<!--': Begin
          aToken := THTMLComment.Create();
        End;
    Else Begin
        aToken := TTextElement.Create();
      End;
    End;
    setlength(fContent, high(fContent) + 2);
    fContent[high(fContent)] := aToken;
    aToken.Parse();
    l1 := TopToken();
    l2 := lowercase(TopToken(1));
    // Hier muss eine Prüfung Rein ob es überhaupt noch was zu lesen gibt, oder ist die schon drin ?
  End;
  If Not eat('<') Then exit;
  If Not eat('/' + lName) Then exit;
  If Not eat('>') Then exit;
End;

Function THTMLElement.getContentCount: integer;
Begin
  result := Length(fContent);
End;

Function THTMLElement.getContent(Index: integer): THTMLElement;
Begin
  result := fContent[index];
End;

Function THTMLElement.getAttribute(Index: integer): TAttribut;
Begin
  result := fAttributes[index];
End;

Function THTMLElement.getAttributeCount: integer;
Begin
  result := Length(fAttributes);
End;

Function THTMLElement.parseAttributes(): Boolean;
Var
  l1: String;
Begin
  result := false;
  setlength(fAttributes, 0);
  l1 := TopToken();
  (*
   * Normale Attribute enden auf >
   * Wenn aber nach den Attributen das Element direkt beendet werden soll, dann kann auch /> stehen
   *)
  While (l1 <> '>') And (l1 <> '/') Do Begin
    (*
     * Liest Attribute eines Elementes ein, da gibt es 2 Formen
     * Leere Attribute, Attribute mit Wert
     *)
    SetLength(fAttributes, high(fAttributes) + 2);
    fAttributes[high(fAttributes)].Name := l1;
    PopToken();
    fAttributes[high(fAttributes)].Value := '';
    l1 := TopToken();
    If l1 = '=' Then Begin
      If Not eat('=') Then exit;
      fAttributes[high(fAttributes)].Value := PopToken();
      l1 := TopToken();
    End;
  End;
  result := true;
End;

Function THTMLElement.AttribsToString(): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To high(fAttributes) Do Begin
    result := result + ' ' + fAttributes[i].Name;
    If fAttributes[i].Value <> '' Then Begin
      result := result + '=' + fAttributes[i].Value;
    End;
  End;
End;

Constructor THTMLElement.Create();
Begin
  fName := '';
  fAttributes := Nil;
  fContent := Nil;
End;

Destructor THTMLElement.Destroy();
Var
  i: Integer;
Begin
  For i := 0 To high(fContent) Do Begin
    fContent[i].Free;
  End;
  setlength(fAttributes, 0);
End;

Function THTMLElement.ToString(FrontSpace: String; IncludeComments: Boolean): String;
Var
  i: Integer;
Begin
  result := FrontSpace + '<' + Name + attribsToString() + '>';
  For i := 0 To high(fContent) Do Begin
    //If (Not (fContent[i] Is TTextElement)) Then Begin
    result := result + LineEnding;
    //End;
    result := result + fContent[i].ToString(FrontSpace + SpaceIdent, IncludeComments);
  End;
  If length(fContent) <> 0 Then Begin
    //If (Not (fContent[high(fContent)] Is TTextElement)) Then Begin
    result := result + LineEnding + FrontSpace;
    //End;
  End;
  result := result + '</' + Name + '>';
End;

{ THTMLParser }

Constructor THTMLParser.Create();
Begin
  Inherited Create;
End;

Destructor THTMLParser.Destroy();
Begin

End;

Function THTMLParser.Parse(Content: String): THTMLDocument;
Begin
  result := THTMLDocument.Create;
  fLastError := '';
  LError := '';
  LWarning := '';
  aindex := 1;
  aLine := 1;
  aContent := Content;
  result.parse();
  If LError <> '' Then Begin
    fLastError := LError;
    Result.Free;
    result := Nil;
  End;
  fLastWarning := LWarning;
End;

End.

