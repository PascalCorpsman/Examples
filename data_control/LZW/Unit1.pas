(******************************************************************************)
(* Lempel-Zip-Welch Demo                                          28.03.2009  *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo application to show how to use ulzw.pas                 *)
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
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, uLZW, LCLIntf, LCLType;

Type
  TForm1 = Class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    GroupBox2: TGroupBox;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    OpenDialog2: TOpenDialog;
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form1: TForm1;
  LZW: TLZW;

Implementation

{$R *.lfm}

Procedure WriteString(Const Stream: TStream; Value: String);
Var
  b: Byte;
  i: Integer;
Begin
  i := length(Value);
  stream.write(i, sizeof(i));
  For i := 1 To Length(value) Do Begin
    b := ord(value[i]);
    stream.write(b, sizeof(b));
  End;
End;

Function ReadString(Const Stream: Tstream): String;
Var
  i: integer;
  b: Byte;
Begin
  result := '';
  i := 0; // Prevent Compiler Warning
  b := 0; // Prevent Compiler Warning
  stream.read(i, sizeof(i));
  setlength(result, i);
  For i := 1 To length(result) Do Begin
    stream.read(b, sizeof(b));
    result[i] := chr(b);
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *           0.02 = Rework code internals, and Update help
   *)
  caption := 'LZW ver. 0.02 by Corpsman';
  lzw := TLZW.create;
  OpenDialog1.initialdir := ExtractFilePath(paramstr(0));
  saveDialog1.initialdir := ExtractFilePath(paramstr(0));
  OpenDialog2.initialdir := ExtractFilePath(paramstr(0));
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  lzw.free;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  Showmessage(caption + LineEnding + LineEnding +
    'Support : www.Corpsman.de' + LineEnding + LineEnding +
    'This demo is based on the Lempel-Ziv-Welch algorithmus.' + LineEnding +
    'It show''s how easy it is to implement a compression Application.'
    );
End;

Procedure TForm1.Button1Click(Sender: TObject);
{.$DEFINE EXACKTINFO}
Var
{$IFDEF EXACKTINFO}
  SizeBefore, SizeAfter,
{$ENDIF EXACKTINFO}
  d, d2: cardinal;
  f1, f2: TFilestream;
  m1, m2: TMemoryStream;
Begin
  If Opendialog1.execute Then Begin
    OpenDialog1.initialdir := ExtractFilePath(Opendialog1.Filename);
    If Savedialog1.execute Then Begin
      button1.enabled := false;
      saveDialog1.initialdir := ExtractFilePath(savedialog1.filename);
      f1 := TFileStream.create(Opendialog1.Filename, fmopenread);
      m1 := TMemoryStream.create;
      m1.CopyFrom(f1, f1.size);
{$IFDEF EXACKTINFO}
      SizeBefore := f1.size;
{$ENDIF EXACKTINFO}
      m1.position := 0;
      f1.free;
      m2 := TMemoryStream.create;
      WriteString(m2, ExtractFileName(Opendialog1.Filename));
      d := GetTickCount;
      lzw.Compress(m1, m2);
      d2 := GetTickCount;
{$IFDEF EXACKTINFO}
      SizeAfter := m2.Size;
{$ENDIF EXACKTINFO}
      f2 := TFileStream.create(savedialog1.filename, fmcreate Or fmopenwrite);
      m2.position := 0;
      f2.CopyFrom(m2, m2.size);
      m2.free;
      f2.free;
      showmessage(
        'Ready ' + LineEnding
        + 'Time :' + FloattostrF((d2 - d) / 1000, FFFixed, 7, 3)
{$IFDEF EXACKTINFO}
        + LineEnding + 'Compression Rate :' + FloattostrF((SizeBefore / SizeAfter) * 100, FFFixed, 7, 2) + '%' + LineEnding +
        'M/sec :' + floattostrf(SizeBefore / (d2 - d), fffixed, 7, 3)
{$ENDIF EXACKTINFO}
        );
      button1.enabled := True;
    End;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  d, d2: cardinal;
  s, path: String;
  f1, f2: TFilestream;
  m1, m2: TMemorystream;
Begin
  If Opendialog2.execute Then Begin
    OpenDialog2.initialdir := ExtractFilePath(Opendialog2.Filename);
    SelectDirectory('Select directory', '', Path);
    If Path <> '' Then Begin
      button3.enabled := false;
      f1 := TFileStream.create(Opendialog2.Filename, fmopenread);
      m1 := TMemoryStream.create;
      m1.CopyFrom(f1, f1.size);
      m1.position := 0;
      f1.free;
      s := ReadString(m1);
      m2 := TMemoryStream.create;
      d := GetTickCount;
      LZW.DeCompress(m1, m2);
      d2 := GetTickCount;
      m2.position := 0;
      f2 := TFileStream.create(IncludeTrailingBackslash(Path) + s, fmcreate Or fmopenwrite);
      f2.CopyFrom(m2, m2.size);
      f2.free;
      showmessage('Ready [ Time :' + FloattostrF((d2 - d) / 1000, FFFixed, 7, 3) + ' ].');
      button3.enabled := True;
    End;
  End;
End;

End.

