(******************************************************************************)
(* ubitstream.pas                                                 27.03.2009  *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
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
(*               0.02 - rework the whole interface to be more encapsulated    *)
(*                                                                            *)
(******************************************************************************)
Unit ubitstream;

{$MODE ObjFPC}{$H+}

Interface

Uses classes, sysutils;

Type

  TMode = (mRead, mWrite);

  { TBitStream }

  TBitStream = Class
  private
    fStream: TStream;
    Fmode: TMode;
    FBuffer: byte;
    FIndex: Integer;
    Procedure Check(Mode: TMode); Inline;
    Procedure FWriteBool(Value: Boolean);
    Function FReadBool: Boolean;

    (*
     * Finish writes the "last" byte that is actually not written to the
     * stream (will be filled up with "0")
     *)
    Procedure Finish;
  public
    (*
     * By Default the Stream is in Write Mode
     *)
    Constructor Create(); virtual;
    Destructor Destroy; override;

    (*
     * Resets all Internal datastructure as if the Stream was created
     * -> Switch Back to Write mode and start with a empty stream
     *)
    Procedure Clear;

    (*
     * This is how you "Load" the Bitstream from a other Stream
     *
     * After that you can "Read" the bits with all Read* funktions
     *
     * Usage: Create, CopyFrom, Read*, Free
     *)
    Procedure CopyFrom(Source: TStream; Count: int64);

    (*
     * This is how you "store" the Bitstream to a other Stream
     * Result is the number of bytes that where written
     *
     * Usage: Create, Write*, SaveTo, Free
     *)
    Function SaveTo(Dest: TStream): int64;

    (*
     * Write Operations
     *)
    Procedure WriteBool(Value: Boolean);
    Procedure WriteByte(Value: Byte);
    Procedure WriteWord(Value: Word);
    Procedure WriteInteger(Value: Integer);

    (*
     * Read Operations
     *)
    Function ReadBool: Boolean;
    Function ReadByte: Byte;
    Function ReadWord: Word;
    Function ReadInteger: Integer;
  End;

Implementation

{ TBitStream }

Constructor TBitStream.Create;
Begin
  Inherited Create;
  fmode := mWrite; // By Default the Stream is Empty and wants to be filled through "write" commands
  FIndex := 0;
  FBuffer := 0;
  fStream := TMemoryStream.Create;
End;

Destructor TBitStream.Destroy;
Begin
  fStream.Free;
End;

Procedure TBitStream.Clear;
Begin
  Fmode := mWrite;
  fStream.free;
  fstream := TMemoryStream.Create;
End;

Procedure TBitStream.Check(Mode: TMode);
Begin
  If Fmode <> Mode Then Begin
    Case Fmode Of
      mRead: Raise exception.Create('Error, in read mode is write forbidden.');
      mWrite: Raise exception.Create('Error, in write mode is read forbidden.');
    End;
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
    fstream.write(Fbuffer, sizeof(fbuffer));
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

Procedure TBitStream.CopyFrom(Source: TStream; Count: int64);
Begin
  // Reset old data
  Clear;
  fStream.CopyFrom(Source, Count);
  // Init for Reading ;)
  fStream.Position := 0;
  Fmode := mRead;
End;

Function TBitStream.SaveTo(Dest: TStream): int64;
Begin
  Check(mWrite);
  Finish;
  fStream.Position := 0;
  result := dest.CopyFrom(fStream, fStream.Size);
End;

Function TBitStream.FReadBool: Boolean;
Var
  m: Byte;
Begin
  result := false;
  Case Findex Of
    0: Begin
        m := 1 Shl 7;
        If fStream.Position = fStream.size Then
          Raise exception.create('Error "end of file" reached.');
        fStream.read(FBuffer, sizeof(FBuffer));
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

Procedure TBitStream.Finish;
Begin
  Case Fmode Of
    mWrite: Begin
        While Findex <> 0 Do
          FWriteBool(false);
      End;
  End;
End;

End.

