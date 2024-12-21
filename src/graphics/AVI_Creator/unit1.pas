(******************************************************************************)
(* AVI_Creator                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo that shows the usage of ugwavi.pas                      *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses FileUtil, ugwavi;

Function LoadImg(Filename: String): TJPEGImage;
Var
  bmp: Tbitmap;
  png: TPortableNetworkGraphic;
Begin
  result := TJPEGImage.Create;
  Case lowercase(ExtractFileExt(Filename)) Of
    '.bmp': Begin
        bmp := TBitmap.Create;
        bmp.LoadFromFile(Filename);
        result.Assign(bmp);
        bmp.free;
      End;
    '.jpg': Begin
        result.LoadFromFile(Filename);
      End;
    '.png': Begin
        png := TPortableNetworkGraphic.Create;
        png.LoadFromFile(Filename);
        result.Assign(png);
        png.free;
      End;
  Else Begin
      Raise exception.create('Error unknown filetype:' + Filename);
    End;
  End;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  Dir: String;
  sl: TStringList;
  avi: tgwavi;
  jp: TJPEGImage;
  m: TMemoryStream;
  i: Integer;
Begin
  If Not SelectDirectory(Dir, [], 0) Then Begin
    exit;
  End;
  sl := findallFiles(dir, '*.bmp;*.png;*.jpg', false);
  If sl.count = 0 Then Begin
    showmessage('Error, no files found.');
    sl.free;
    exit;
  End;
  showmessage(inttostr(sl.count) + ' found.');
  If Not SaveDialog1.Execute Then Begin
    sl.free;
    exit;
  End;
  sl.Sort;
  jp := LoadImg(sl[0]);
  avi := tgwavi.Create();
  avi.Open(SaveDialog1.FileName, jp.Width, jp.Height, 'MJPG', strtointdef(Edit1.Text, 25), Nil);
  m := TMemoryStream.Create;
  jp.SaveToStream(m);
  jp.free;
  m.Position := 0;
  avi.Add_Frame(m);
  m.free;
  For i := 1 To sl.Count - 1 Do Begin
    jp := LoadImg(sl[i]);
    m := TMemoryStream.Create;
    jp.SaveToStream(m);
    jp.free;
    m.Position := 0;
    avi.Add_Frame(m);
    m.free;
  End;
  avi.Close;
  avi.free;
  sl.free;
  showmessage('Done.');
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  edit1.text := '25';
  caption := 'Avi creater ver. 0.01';
End;

End.

