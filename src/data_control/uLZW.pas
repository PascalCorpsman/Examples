(******************************************************************************)
(* uLZW.pas                                                       28.03.2009  *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : TLZW is an implementation of my interpretation of the LZW    *)
(*               algorithm. This must not be the general used version.        *)
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
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)

Unit uLZW;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes,
  sysutils,
  ubitstream;

Type
  ELZWException = Class(Exception);
  EInvalidSourceStream = Class(ELZWException);
  EInvalidDestStream = Class(ELZWException);
  EInvalidInputData = Class(ELZWException);

  // Die Elemente mit deren Hilfe wir kommunizieren
  TElement = Array Of Byte;

  (*
    Ein einfaches Dictionary welches folgende Laufzeiten hat.

    Find                O(N)
    AddElement          O(1)
    Get                 O(1)
    Count               O(1)
    SpeicherKomplexität O(N^N)

  *)
  (*
   !! ACHTUNG !!
   TDictionary_Easy wird nicht benutzt.
   Ist allerdings in der Funktion identisch zu TDictionary_Better.
   Zum besseren Verständnis sollte man sich TDictionary_Easy ansehen.
  *)
  TDictionary_Easy = Class
  private
    FDict: Array Of TElement;
  public
    Procedure Clear; // Löschen aller Elemente im Dictionary
    Procedure AddElement(Value: TElement); // 1 Element hinzufügen
    Function Find(Value: TElement): Integer; // -1 wenn Element nicht im Dictionary enthalten, sonst der Index
    Function Get(Index: Integer): TElement; // gibt das Element zum Index zurück
    Function Count: cardinal; // Die Gesamtanzahl der Elemente im Dictionary
    Constructor Create;
    Destructor Destroy; override;
  End;

  // Die Datentypen, welche für das optimierte Dictionary benötigt werden.
  PDictElement = ^TDictElement;
  TDictElement = Record
    Parent: PDictElement;
    Child: PDictElement;
    Next: PDictElement;
    Value: Byte;
    Index: Integer;
  End;

  (*
    Das optimierte Dictionary

    Find                O(Log(N))
    AddElement          O(1)
    Get                 O(1)
    Count               O(1)
    Speicherkomplexität O(N)

  *)
  TDictionary_Better = Class
  private
    FAnker: PDictElement;
    FCounter: Cardinal;
    FElementList: Array Of PDictElement;
    Procedure RecursivFree(Value: PDictElement);
  public
    Procedure Clear; // Löschen aller Elemente im Dictionary
    Procedure AddElement(Value: TElement); // 1 Element hinzufügen
    Function Find(Value: TElement): Integer; // -1 wenn Element nicht im Dictionary enthalten, sonst der Index
    Function Get(Index: Integer): TElement; // gibt das Element zum Index zurück
    Function Count: cardinal; // Die Gesamtanzahl der Elemente im Dictionary
    Constructor create;
    Destructor Destroy; override;
  End;

  (*
  Die eigentliche LZW Klasse, welche den Algorithmus umsetzt.

  Die hier implementierte Variante erzeugt die Dateien mit maximal
  5 Byte overhead zum Optimum

  4 Byte aufgrund des künstlich eingefügten Headers
  1 Byte aufgrund der TBitStream Komponente
  *)
  TLZW = Class
  private
    FDictionary: TDictionary_Better;
    Procedure InitDict;
    Function CreateMask(value: Cardinal): integer;
    Function BitCount(value: Cardinal): integer;
  public
    Name: String;
    Constructor create;
    Destructor Destroy; override;
    (*
    Compress
    *)
    Procedure Compress(Const Source, Dest: TStream); overload;
    Procedure Compress(Const Source: TStream; Dest: String); overload;
    Procedure Compress(Source: String; Const Dest: TStream); overload;
    Procedure Compress(Source, Dest: String); overload;
    (*
    Decompress
    *)
    Procedure DeCompress(Const Source, Dest: TStream); overload;
    Procedure DeCompress(Const Source: TStream; Dest: String); overload;
    Procedure DeCompress(Source: String; Const Dest: TStream); overload;
    Procedure DeCompress(Source, Dest: String); overload;
  End;

Implementation

{ TDictionary_Easy }

Constructor TDictionary_Easy.create;
Begin
  Inherited;
  setlength(Fdict, 0);
End;

Destructor TDictionary_Easy.Destroy;
Begin
  clear;
End;

Procedure TDictionary_Easy.AddElement(Value: TElement);
Var
  i: Integer;
Begin
  setlength(Fdict, high(Fdict) + 2);
  setlength(Fdict[high(Fdict)], high(Value) + 1);
  For i := 0 To High(Value) Do
    Fdict[high(Fdict), i] := Value[i];
End;

Procedure TDictionary_Easy.Clear;
Var
  i: integer;
Begin
  For i := 0 To High(FDict) Do
    setlength(Fdict[i], 0);
  setlength(Fdict, 0);
End;

Function TDictionary_Easy.Find(Value: TElement): Integer;
Var
  i, j: integer;
  b: Boolean;
Begin
  result := -1;
  For i := 0 To High(FDict) Do
    If High(Value) = High(Fdict[i]) Then Begin
      b := True;
      For j := 0 To High(Value) Do
        If Value[j] <> Fdict[i, j] Then Begin
          b := false;
          break;
        End;
      If b Then Begin
        result := i;
        exit;
      End;
    End;
End;

Function TDictionary_Easy.Count: cardinal;
Begin
  result := High(Fdict) + 1;
End;

Function TDictionary_Easy.Get(Index: Integer): TElement;
Var
  i: Integer;
Begin
  result := Nil;
  If index > high(fdict) Then
    Raise exception.CreateFmt('Error Invalid Index Value. (Instance: %0:s)', [ClassName]);
  setlength(result, high(fdict[index]) + 1);
  For i := 0 To high(result) Do
    result[i] := Fdict[index, i];
End;

{ TDictionary_Better }

Constructor TDictionary_Better.create;
Begin
  Inherited;
  fanker := Nil;
  FCounter := 0;
  setlength(FElementList, 0);
End;

Destructor TDictionary_Better.Destroy;
Begin
  Clear;
End;

Procedure TDictionary_Better.RecursivFree(Value: PDictElement);
Begin
  If assigned(Value) Then Begin
    RecursivFree(Value^.Child);
    RecursivFree(Value^.Next);
    dispose(Value);
  End;
End;

Procedure TDictionary_Better.Clear;
Var
  i: Integer;
Begin
  RecursivFree(Fanker);
  fanker := Nil;
  FCounter := 0;
  // Blockweises allokieren spart Rechenzeit auf Kosten von Speicher.
  setlength(FElementList, 10000);
  For i := 0 To high(FElementList) Do
    FElementList[i] := Nil;
End;

Procedure TDictionary_Better.AddElement(Value: TElement);
Var
  Bevor, parent, p: PDictElement;
  i: Integer;
Begin
  // Suchen der Stelle wo unser Element hinein gehört.
  If Assigned(Fanker) Then Begin
    p := Fanker;
    parent := Nil;
    Bevor := Fanker;
    i := 0;
    // Suchen der Stelle wo wir unser Element einfügen müssen
    While assigned(p) Do Begin
      If p^.value = value[i] Then Begin
        Parent := p;
        Bevor := Nil;
        p := p^.Child;
        // Der Clou ist, dass i niemals Größer wird als High(Value) bzw immer bei High Value stehen bleibt
        inc(i);
      End
      Else Begin
        Bevor := p;
        p := p^.Next;
      End;
    End;
    // OP zeigt nun auf das Parent
    new(p);
    p^.Parent := parent;
    If assigned(parent) Then
      If Not assigned(parent^.Child) Then parent^.Child := p;
    p^.Child := Nil;
    p^.Next := Nil;
    p^.value := value[high(Value)];
    p^.Index := FCounter;
    If Fcounter > high(felementlist) Then Begin
      // Blockweises allokieren spart Rechenzeit auf Kosten von Speicher.
      setlength(felementlist, high(felementlist) + 10001);
    End;
    felementlist[Fcounter] := p;
    If assigned(bevor) Then
      bevor^.Next := p;
    inc(FCounter);
  End
  Else Begin // Beim Einfügen des allerersten Elementes machen wir alles von Hand
    new(FAnker);
    fanker^.Parent := Nil;
    fanker^.Child := Nil;
    fanker^.Next := Nil;
    fanker^.value := Value[0];
    fanker^.Index := 0;
    FElementList[0] := Fanker;
    FCounter := 1;
  End;
End;

Function TDictionary_Better.Count: cardinal;
Begin
  result := Fcounter;
End;

Function TDictionary_Better.Find(Value: TElement): Integer;
Var
  p: PDictElement;
  i: integer;
Begin
  result := -1;
  p := Fanker;
  i := 0;
  // Suchen des Elementes
  While assigned(p) Do Begin
    If p^.value = value[i] Then Begin
      inc(i);
      // Element gefunden, nichts wie raus.
      If i > High(value) Then Begin
        result := p^.Index;
        exit;
      End;
      p := p^.Child;
    End
    Else Begin
      p := p^.Next;
    End;
  End;
End;

Function TDictionary_Better.Get(Index: Integer): TElement;
Var
  p: PDictElement;
  c: Integer;
Begin
  result := Nil;
  // Dank felementlist ist die Get Funktion ebenfalls rasend schnell
  // Dies kostet uns allerdings auch ein klein wenig Speicher...
  p := felementlist[Index];
  // Zählen der Größe von Result
  c := 1;
  While p^.Parent <> Nil Do Begin
    inc(c);
    p := p^.parent;
  End;
  // Das eigentliche Auslesen
  setlength(result, c);
  dec(c);
  p := felementlist[Index];
  While p^.Parent <> Nil Do Begin
    result[c] := p^.value;
    p := p^.parent;
    dec(c);
  End;
  result[c] := p^.value;
End;

{ TLZW }

Constructor TLZW.create;
Begin
  Inherited;
  Name := 'TLZW';
  FDictionary := Nil;
End;

Destructor TLZW.Destroy;
Begin
  If assigned(FDictionary) Then Begin
    FDictionary.free;
  End;
End;

Procedure TLZW.InitDict;
Var
  i: Integer;
  e: TElement;
Begin
  e := Nil;
  If Not assigned(FDictionary) Then
    FDictionary := TDictionary_Better.create;
  FDictionary.clear;
  // Wir initialisieren das Wörterbuch mit allen Werten die es in Byte gibt.
  Setlength(e, 1);
  For i := 0 To 255 Do Begin
    e[0] := i;
    FDictionary.AddElement(e);
  End;
End;

(*
Berechnen der Bitmaske die mit dem Index "And" genommen werden muss, um die
einzelnen Bits zu bestimmen
*)

Function TLZW.CreateMask(value: Cardinal): integer;
Var
  tmp: Cardinal;
Begin
  result := 1;
  tmp := 1;
  While tmp < value Do Begin
    result := result Shl 1;
    tmp := (tmp Shl 1) Or 1;
  End;
End;

(*
Berechnen wie viele Bits notwendig sind um Value Zahlenwerte zu speichern
*)

Function TLZW.BitCount(Value: Cardinal): integer;
Var
  tmp: Cardinal;
Begin
  result := 1;
  tmp := 1;
  While tmp < value Do Begin
    result := result + 1;
    tmp := (tmp Shl 1) Or 1;
  End;
End;

(*
Der eigentliche Komprimierungsschritt
*)

Procedure TLZW.Compress(Const Source, Dest: TStream);
Var
  k, i, mask, size: Integer;
  buf, tmp: Telement;
  b: Byte;
  mapper: TBitStream;
  lastindex, index: Integer;
Begin
  buf := Nil;
  tmp := Nil;
  If Not Assigned(source) Then
    Raise EInvalidSourceStream.CreateFmt('Compress was called with an nil source stream. (Instance: %0:s)', [ClassName]);
  If Not Assigned(Dest) Then
    Raise EInvalidDestStream.CreateFmt('Compress was called with an nil dest stream. (Instance: %0:s)', [ClassName]);
  // Das Dictionary initialisieren
  InitDict;
  // Als Erstes müssen wir uns merken wie viele Bytes im ursprünglichen Stream vorhanden waren.
  // Dieser Schritt kostet uns 4 Bytes, es ginge auch ohne, allerdings müsste dann
  // beim dekomprimieren das Dateiende mittels Exception gefunden werden => nicht sehr schön
  // TODO: Das kann auch so umgeschrieben werden, dass man die 4 Bytes nicht braucht !
  size := source.Size - source.position;
  Dest.Write(size, sizeof(size));
  // Der Mapper der uns das bitweise schreiben ermöglicht
  mapper := TBitStream.create();
  // Init der benötigten Variablen
  setlength(buf, 0);
  index := 0;
  LastIndex := 0;
  setlength(tmp, 0);
  b := 0; // Prevent Compiler Warnings
  // Und los gehts
  While source.Position <> source.size Do Begin
    // Aufbau des Puffers
    While Index <> -1 Do Begin
      setlength(buf, high(buf) + 2);
      source.read(b, sizeof(b));
      buf[high(buf)] := b;
      lastindex := Index;
      Index := FDictionary.Find(buf);
      // Das Ende der Datei erfordert besondere Aufmerksamkeit
      If Source.position = source.size Then Begin
        // Wenn das allerletzte Element nicht im Dictionary drin ist.
        If Index = -1 Then Begin
          setlength(tmp, high(buf));
          For k := 0 To high(tmp) Do
            tmp[k] := buf[k];
          lastindex := FDictionary.Find(tmp);
          setlength(tmp, 1);
          tmp[0] := buf[high(buf)];
        End
        Else Begin
          // Wenn es drin ist
          lastindex := Index;
          index := -1;
        End;
      End;
    End;
    // Wir haben ein Element das nicht mehr im Dictionary ist.
    // Also fügen wir es ein
    FDictionary.AddElement(buf);
    // Ausgabe unseres Indexes
    mask := CreateMask(FDictionary.count - 2);
    For i := 1 To BitCount(FDictionary.count - 2) Do Begin
      mapper.WriteBool((lastindex And mask) <> 0);
      mask := mask Shr 1;
    End;
    Buf[0] := Buf[high(Buf)];
    setlength(Buf, 1);
    index := FDictionary.Find(buf);
  End;
  // Sollte das letze Element nicht gefunden worden sein, so
  // Muss nun noch dieses eine hinzugefügt werden.
  If high(tmp) <> -1 Then Begin
    lastindex := FDictionary.find(tmp);
    // Ausgabe unseres Indexes
    mask := CreateMask(FDictionary.count - 2);
    For i := 1 To BitCount(FDictionary.count - 2) Do Begin
      mapper.WriteBool((lastindex And mask) <> 0);
      mask := mask Shr 1;
    End;
  End;
  // Übernehmen der Comprimierten Daten nach Dest
  mapper.SaveTo(dest);
  mapper.free;
  setlength(buf, 0);
  FDictionary.Clear;
End;

Procedure TLZW.Compress(Const Source: TStream; Dest: String);
Var
  f: TFilestream;
Begin
  f := Tfilestream.create(Dest, FMCreate Or FMOpenWrite);
  Compress(Source, f);
  f.free;
End;

Procedure TLZW.Compress(Source: String; Const Dest: TStream);
Var
  f: TFilestream;
Begin
  f := Tfilestream.create(Source, FMOpenRead);
  Compress(f, Dest);
  f.free;
End;

Procedure TLZW.Compress(Source, Dest: String);
Var
  f1: TFilestream;
  f2: TFilestream;
Begin
  f1 := Tfilestream.create(Source, FMOpenRead);
  f2 := Tfilestream.create(Dest, FMCreate Or FMOpenWrite);
  Compress(f1, f2);
  f1.free;
  f2.Free;
End;

Procedure TLZW.DeCompress(Const Source, Dest: TStream);
Var
  k, i, index, counter, Size: Integer;
  b: Byte;
  mapper: TBitStream;
  Buf, element: TElement;
Begin
  buf := Nil;
  If Not Assigned(source) Then
    Raise EInvalidSourceStream.CreateFmt('DeCompress was called with an nil source stream. (Instance: %0:s)', [ClassName]);
  If Not Assigned(Dest) Then
    Raise EInvalidDestStream.CreateFmt('DeCompress was called with an nil dest stream. (Instance: %0:s)', [ClassName]);
  // Das Dictionary initialisieren
  InitDict;
  // Auslesen der Gesamtgröße der zu entpackenden Datei
  size := 0; // Prevent Compiler Warnings
  Source.read(size, sizeof(size));
  counter := Dest.Position + Size;
  // Der Mapper ermöglicht bitweisen Zugriff
  mapper := TBitStream.create();
  mapper.CopyFrom(source, source.Size - source.Position);
  setlength(buf, 0);
  // Vorspiel
  // Einlesen des 1. Index
  index := 0;
  For i := 1 To BitCount(FDictionary.count - 1) Do Begin
    index := index Shl 1;
    If mapper.ReadBool Then
      index := index Or 1;
  End;
  // Auslesen des Elementes
  element := FDictionary.Get(Index);
  // Initialisieren des Puffers
  setlength(buf, 1);
  buf[0] := element[0];
  b := buf[0];
  // Schreiben des Ersten entpackten Elementes, dieses ist immer der Länge 1
  Dest.Write(b, sizeof(b));
  // Hauptspiel
  While Dest.position <> counter Do Begin
    // Einlesen des Index
    index := 0;
    For i := 1 To BitCount(FDictionary.count) Do Begin
      index := index Shl 1;
      If mapper.ReadBool Then
        index := index Or 1;
    End;
    If Index >= FDictionary.count Then Begin
      If Index > FDictionary.count Then Begin
        // Diese Exception kommt natürlich nie, sie würde evtl. nur kommen, wenn die Quelldatei Fehlerhaft ist.
        // Und da wir im Dictionary sehr viele Pointer geschichten haben, ist das hier ein Schutz
        Raise EInvalidInputData.CreateFmt('DeCompress was called withInvalid Input Data. (Instance: %0:s)', [ClassName]);
      End
      Else Begin
        // Der Fall, dass wir auf ein Element zu greifen, das erst jetzt ins Dictionary aufgenommen wird
        setlength(buf, high(buf) + 2);
        buf[high(buf)] := Buf[0];
        Setlength(Element, high(buf) + 1);
        For k := 0 To high(buf) Do
          Element[k] := Buf[k];
      End;
    End
    Else Begin
      // Auslesen des Elementes
      element := FDictionary.Get(Index);
      setlength(buf, high(buf) + 2);
      buf[high(buf)] := element[0];
    End;
    // Das Element ins Wörterbuch aufnehmen
    FDictionary.AddElement(buf);
    setlength(buf, 0);
    buf := Element;
    // Rausschreiben des Elementes
    For k := 0 To High(buf) Do Begin
      b := element[k];
      Dest.Write(b, sizeof(b));
    End;
  End;
  setlength(buf, 0);
  mapper.free;
  FDictionary.clear;
End;

Procedure TLZW.DeCompress(Const Source: TStream; Dest: String);
Var
  f: TFilestream;
Begin
  f := Tfilestream.create(Dest, FMCreate Or FMOpenWrite);
  Compress(Source, f);
  f.free;
End;

Procedure TLZW.DeCompress(Source, Dest: String);
Var
  f1: TFilestream;
  f2: TFilestream;
Begin
  f1 := Tfilestream.create(Source, FMOpenRead);
  f2 := Tfilestream.create(Dest, FMCreate Or FMOpenWrite);
  DeCompress(f1, f2);
  f1.free;
  f2.Free;
End;

Procedure TLZW.DeCompress(Source: String; Const Dest: TStream);
Var
  f: TFilestream;
Begin
  f := Tfilestream.create(Source, FMOpenRead);
  DECompress(f, Dest);
  f.free;
End;

End.

