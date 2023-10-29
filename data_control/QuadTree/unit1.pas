(******************************************************************************)
(* Demo for uQuadtree.pas                                          15.04.2010 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Quadtree Coding with Toleranz for grayscale Bitmaps          *)
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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uquadtree, math;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  QT: tquadtree;

Implementation

{$R *.lfm}

Uses LazUTF8;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  If OpenDialog1.execute Then
    If SaveDialog1.execute Then Begin
      b := Tbitmap.create;
      b.loadfromfile(OpenDialog1.FileName);
      qt.SaveBitmap(b, SaveDialog1.FileName, max(min(255, strtoint(edit1.text)), 0));
      b.free;
    End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  b: Tbitmap;
Begin
  If opendialog2.execute Then
    If SaveDialog2.execute Then Begin
      b := QT.LoadBitmap(opendialog2.FileName);
      b.SaveToFile(SaveDialog2.FileName);
      b.free;
    End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  showmessage(
    'Quadtree Compression version 0.01' + LineEnding +
    'by Corpsman' + LineEnding +
    'Support : www.Corpsman.de' + LineEnding + LineEnding +
    'Compression:' + LineEnding +
    'Open the Bitmap File with the Komprimieren Button' + LineEnding +
    'Then give the Filename for the stored data.' + LineEnding +
    'By giving the Toleranz > 0 you get a losy Compression' + LineEnding +
    'max Toleranz = 255' + LineEnding + LineEnding +
    'Uncomression:' + LineEnding +
    'Open the Compressed Data with the Dekomprimieren Button' + LineEnding +
    'Then give the Filename for the stored data.'
    );
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  qt.free;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  qt := TQuadtree.create;
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  OpenDialog2.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog2.InitialDir := ExtractFilePath(ParamStrUTF8(0));
End;

End.

