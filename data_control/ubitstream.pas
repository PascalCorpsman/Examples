(******************************************************************************)
(* ubitstream.pas                                                 27.03.2009  *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Stream that allows bit wise storing of data without "gaps"   *)
(*                         7 Boolean, Filesize = 1 Byte                       *)
(*                         9 Boolean, Filesize = 2 Byte                       *)
(*                                                                            *)
(*               The class supports a lot of Integer types, so that you can   *)
(*               store e.g. 2 Boolean 1 Integer 3 Boolean in only 5 Byte      *)
(*               Filesize.                                                    *)
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
Unit ubitstream;

{$MODE ObjFPC}{$H+}

Interface

Uses classes, sysutils;

Type

  TMode = (mRead, mWrite);

  TBitStream = Class
  private
    Fmode: TMode;
    FBuffer: byte;
    FIndex: Integer;
    Procedure Check(Mode: TMode);
    Procedure FWriteBool(Value: Boolean);
    Function FReadBool: Boolean;
  public
    Name: String;
    // TODO: Make Stream Private -> Needs modification in uLZW.pas !
    Stream: TStream; // Do not access this stream directly, if you do not know exactly what you are doing
    Constructor Create(Mode: TMode);
    Destructor destroy; override;
    (*
    Write Operations
    *)
    Procedure WriteBool(Value: Boolean);
    Procedure WriteByte(Value: Byte);
    Procedure WriteWord(Value: Word);
    Procedure WriteInteger(Value: Integer);
    (*
    Close stellt sicher das der Schreibvorgang noch offen stehende Bits einfügt (der Rest wird mit 0 aufgefüllt)
    *)
    Procedure Close;
    (*
    Read Operations
    *)
    Function ReadBool: Boolean;
    Function ReadByte: Byte;
    Function ReadWord: Word;
    Function ReadInteger: Integer;
  End;

Implementation

{ TBitStream }

Constructor TBitStream.Create(Mode: TMode);
Begin
  Inherited Create;
  name := 'TBitStream';
  fmode := Mode;
  FIndex := 0;
  FBuffer := 0;
  Stream := Nil;
End;

Destructor TBitStream.destroy;
Begin
  Close;
End;

Procedure TBitStream.Check(Mode: TMode);
Begin
  If Not Assigned(Stream) Then
    Raise exception.create('Error ' + Name + ' Stream is NIL.');
  Case Mode Of
    mRead: If fmode = MWrite Then
        Raise exception.create('Error ' + Name + ' was created in write mode and you tried to read.');
    mWrite: If fmode = MRead Then
        Raise exception.create('Error ' + Name + ' was created in read mode and you tried to read.');
  End;
End;

Procedure TBitStream.WriteBool(Value: Boolean);
Begin
  Check(mWrite);
  FWriteBool(Value);
End;

Procedure TBitStream.WriteByte(Value: Byte);
Var
  i: Integer;
  m: Byte;
Begin
  Check(mWrite);
  m := 1 Shl 7;
  For i := 0 To 7 Do Begin
    FWriteBool((Value And m) <> 0);
    m := m Shr 1;
  End;
End;

Procedure TBitStream.WriteWord(Value: Word);
Var
  i: Integer;
  m: word;
Begin
  Check(mWrite);
  m := 1 Shl 15;
  For i := 0 To 15 Do Begin
    FWriteBool((Value And m) <> 0);
    m := m Shr 1;
  End;
End;

Procedure TBitStream.WriteInteger(Value: Integer);
Var
  i: Integer;
  m: Integer;
Begin
  Check(mWrite);
  m := integer(1 Shl 31);
  For i := 0 To 31 Do Begin
    FWriteBool((Value And m) <> 0);
    m := m Shr 1;
  End;
End;

Procedure TBitStream.FWriteBool(Value: Boolean);
Begin
  If value Then
    Fbuffer := (fbuffer Shl 1) Or 1
  Else
    Fbuffer := fbuffer Shl 1;
  inc(Findex);
  If Findex = 8 Then Begin
    Findex := 0;
    stream.write(Fbuffer, sizeof(fbuffer));
  End;
End;

Function TBitStream.ReadBool: Boolean;
Begin
  Check(mRead);
  result := FReadBool;
End;

Function TBitStream.ReadByte: Byte;
Var
  i: Integer;
  erg: Byte;
Begin
  Check(mRead);
  erg := 0;
  For i := 0 To 7 Do Begin
    erg := erg Shl 1;
    If FReadBool Then
      erg := erg Or 1;
  End;
  Result := erg;
End;

Function TBitStream.ReadWord: Word;
Var
  i: Integer;
  erg: Word;
Begin
  Check(mRead);
  erg := 0;
  For i := 0 To 15 Do Begin
    erg := erg Shl 1;
    If FReadBool Then
      erg := erg Or 1;
  End;
  Result := erg;
End;

Function TBitStream.ReadInteger: Integer;
Var
  i: Integer;
  erg: Integer;
Begin
  Check(mRead);
  erg := 0;
  For i := 0 To 31 Do Begin
    erg := erg Shl 1;
    If FReadBool Then
      erg := erg Or 1;
  End;
  Result := erg;
End;

Function TBitStream.FReadBool: Boolean;
Var
  m: Byte;
Begin
  result := false;
  Case Findex Of
    0: Begin
        m := 1 Shl 7;
        If Stream.Position = Stream.size Then
          Raise exception.create('Error ' + Name + ' "end of file" reached.');
        Stream.read(FBuffer, sizeof(FBuffer));
        result := (m And FBuffer <> 0);
        inc(Findex);
      End;
    1..7: Begin
        m := 1 Shl (7 - Findex);
        result := (m And FBuffer <> 0);
        findex := (findex + 1) Mod 8;
      End;
  End;
End;

Procedure TBitStream.Close;
Begin
  Case Fmode Of
    mWrite: Begin
        While Findex <> 0 Do
          FWriteBool(false);
      End;
  End;
End;

End.

