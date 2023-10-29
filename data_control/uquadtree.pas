(******************************************************************************)
(* uquadtree.pas                                                   15.04.2010 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Quadtree Coding with Toleranz for grayscale Bitmaps          *)
(*                                                                            *)
(*               This Unit shows a simple Quadtree Compression vor            *)
(*               grayscale Bitmaps.                                           *)
(*               If you input a coloured Image it will be automatically       *)
(*               reduced to grayscale.                                        *)
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
Unit uquadtree;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ubitstream,
  IntfGraphics, fpImage, Graphics, LCLType, math;

Type

  { TQuadtree }

  TQuadtree = Class
  private
    FSafeData: Array Of Array Of Byte;
    Procedure QuadPart(Const Stream: TBitStream; x, y, w, Tol: Integer);
    Procedure UnQuadPart(Const Stream: TBitStream; x, y, w: integer);
  public
    Constructor create;
    Destructor destroy; override;
    Procedure SaveBitmap(Const Bitmap: TBitmap; Filename: String; Toleranz: Integer = 0);
    Function LoadBitmap(Filename: String): TBitmap;
  End;

Implementation

// Convert Color to Grayscale

Function ColorToLuminance(Color: TFPColor): Byte;
Begin
  result := max(0, min(255, round(
    0.30 * (color.red Shr 8) + 0.59 * (color.green Shr 8) + 0.11 * (color.blue Shr 8)
    )));
End;

{ TQuadtree }

Constructor TQuadtree.create;
Begin
  Inherited create;
  setlength(FSafeData, 0, 0);
End;

Destructor TQuadtree.destroy;
Begin
  setlength(FSafeData, 0, 0);
End;

// Do the Compression

Procedure TQuadtree.QuadPart(Const Stream: TBitStream; x, y, w, tol: Integer);
Var
  b: Boolean;
  i, j: Integer;
Begin
  b := true;
  i := 0;
  While (i < w) And b Do Begin
    For j := 0 To w - 1 Do
      If abs(FSafeData[x, y] - FSafeData[x + i, y + j]) > Tol Then Begin
        b := false;
        break;
      End;
    inc(i);
  End;
  If b Then Begin
    stream.WriteBool(true);
    stream.WriteByte(FSafeData[x + w Div 2, y + w Div 2]);
  End
  Else Begin
    stream.WriteBool(false);
    w := w Div 2;
    QuadPart(stream, x, y, w, tol);
    QuadPart(stream, x + w, y, w, tol);
    QuadPart(stream, x, y + w, w, tol);
    QuadPart(stream, x + w, y + w, w, tol);
  End;
End;

// Undo the Compression

Procedure TQuadtree.UnQuadPart(Const Stream: TBitStream; x, y, w: integer);
Var
  i, j: Integer;
  s: Boolean;
  b: Byte;
Begin
  s := Stream.ReadBool;
  If Not s Then Begin
    w := w Div 2;
    UnQuadPart(Stream, x, y, w);
    UnQuadPart(Stream, x + w, y, w);
    UnQuadPart(Stream, x, y + w, w);
    UnQuadPart(Stream, x + w, y + w, w);
  End
  Else Begin
    b := Stream.ReadByte;
    For i := 0 To w - 1 Do
      For j := 0 To w - 1 Do
        FSafeData[x + i, y + j] := b;
  End;
End;

// Save A TBitmap in a Compressed File

Procedure TQuadtree.SaveBitmap(Const Bitmap: TBitmap; Filename: String;
  Toleranz: Integer);
Var
  TempIntfImg: TLazIntfImage;
  n, w, h, i, j: Integer;
  f: TFileStream;
  FBitstream: TBitStream;
Begin
  w := 1;
  While w < bitmap.width Do
    w := w * 2;
  h := 1;
  While h < bitmap.Height Do
    h := h * 2;
  n := max(w, h);
  setlength(FSafeData, n, n);
  bitmap.pixelformat := pf24bit;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
  For i := 0 To n - 1 Do
    For j := 0 To n - 1 Do Begin
      If (i < bitmap.Width) And (j < bitmap.height) Then Begin
        FSafeData[i, j] := ColorToLuminance(TempIntfImg.Colors[i, j]);
      End
      Else Begin
        // Irgend einen Default Wert muss man nehmen, und dieser ist wenigstens im Bild Enthalten
        FSafeData[i, j] := ColorToLuminance(TempIntfImg.Colors[bitmap.Width - 1, 0]);
      End;
    End;
  TempIntfImg.free;
  f := TFileStream.create(filename, fmcreate Or fmopenwrite);
  FBitstream := TBitStream.create();
  // Store the Orig Dimensions
  FBitstream.WriteInteger(bitmap.Width);
  FBitstream.WriteInteger(bitmap.height);
  // Store the Compressed Data
  QuadPart(FBitstream, 0, 0, n, Toleranz);
  FBitstream.SaveTo(f);
  FBitstream.Free;
  f.free;
  setlength(FSafeData, 0, 0);
End;

// Load a TBitmap from a Compressed File

Function TQuadtree.LoadBitmap(Filename: String): TBitmap;
Var
  f: TFilestream;
  FBitstream: TBitStream;
  n, w, h, x, y, i, j: Integer;
  CurColor: TFPColor;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  FBitstream := TBitStream.create();
  f := TFileStream.create(Filename, fmopenread);
  FBitstream.CopyFrom(f, f.Size);
  // Load Orig Dimension
  x := FBitstream.ReadInteger;
  y := FBitstream.ReadInteger;
  // Calculate the Temp Dimension
  w := 1;
  While w < x Do
    w := w * 2;
  h := 1;
  While h < y Do
    h := h * 2;
  n := max(w, h);
  setlength(FSafeData, n, n);
  // Unpack the Stream
  UnQuadPart(FBitstream, 0, 0, n);
  // copy the Unpacked Stream into the result
  result := Tbitmap.create;
  result.Width := x;
  result.height := y;
  result.PixelFormat := pf24bit;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(result.Handle, result.MaskHandle);
  For i := 0 To x - 1 Do
    For j := 0 To y - 1 Do Begin
      CurColor.red := FSafeData[i, j] Shl 8;
      CurColor.green := FSafeData[i, j] Shl 8;
      CurColor.blue := FSafeData[i, j] Shl 8;
      TempIntfImg.Colors[i, j] := CurColor;
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  result.Handle := ImgHandle;
  result.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
  setlength(FSafeData, 0, 0);
End;

End.

