(******************************************************************************)
(* Complex Demo                                                    21.11.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo for ucomplex.pas                                        *)
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ucomplex, ugraphics, math;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PaintBox1: TPaintBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioGroup1: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Const
  mx = 128;
  my = 128;
  Grid = 50;

Var
  Form1: TForm1;
  z1, z2: TComplex;
  selected: integer = -1;

Implementation

{$R *.lfm}

{ TForm1 }

Function Collide(x, y: integer; im: TComplex): boolean;
Begin
  x := x - mx;
  y := -(y - my);
  result := sqrt(sqr(im.re * Grid - x) + sqr(im.im * Grid - y)) <= 10;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Begin
  // Einlesen
  If RadioButton1.Checked Then Begin
    z1 := Complex(strtofloatdef(edit1.text, 0), strtofloatdef(edit2.text, 0));
  End
  Else Begin
    z1 := Complex2(strtofloatdef(edit3.text, 0), degtorad(strtofloatdef(edit4.text, 0)));
  End;
  If RadioButton3.Checked Then Begin
    z2 := Complex(strtofloatdef(edit5.text, 0), strtofloatdef(edit6.text, 0));
  End
  Else Begin
    z2 := Complex2(strtofloatdef(edit7.text, 0), degtorad(strtofloatdef(edit8.text, 0)));
  End;
  // Gitter
  PaintBox1.Canvas.Brush.Color := clwhite;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Rectangle(-1, -1, 257, 257);
  PaintBox1.Canvas.Pen.Color := clblack;
  PaintBox1.Canvas.moveto(28, my);
  PaintBox1.Canvas.lineto(256 - 28, my);
  PaintBox1.Canvas.moveto(mx, 28);
  PaintBox1.Canvas.lineto(mx, 256 - 28);
  // Die 1 Markierung
  PaintBox1.Canvas.moveto(mx + Grid, my - 5);
  PaintBox1.Canvas.Lineto(mx + Grid, my + 5);
  PaintBox1.Canvas.moveto(mx - Grid, my - 5);
  PaintBox1.Canvas.Lineto(mx - Grid, my + 5);
  PaintBox1.Canvas.moveto(mx - 5, my - grid);
  PaintBox1.Canvas.Lineto(mx + 5, my - grid);
  PaintBox1.Canvas.moveto(mx - 5, my + grid);
  PaintBox1.Canvas.Lineto(mx + 5, my + grid);
  PaintBox1.Canvas.Pen.Style := psDash;
  PaintBox1.Canvas.Pen.Color := clGray;
  PaintBox1.Canvas.Brush.Style := bsClear;
  PaintBox1.Canvas.Ellipse(mx - grid, my - grid, mx + grid, my + grid);
  PaintBox1.Canvas.Pen.Style := psSolid;
  // Rendern der Komplexen Zahl
  PaintBox1.Canvas.Pen.Color := clred;
  RenderArrow(PaintBox1.Canvas, point(mx, my), point(mx + round(Grid * z1.re), my - round(Grid * z1.im)));
  PaintBox1.Canvas.Pen.Color := clgreen;
  RenderArrow(PaintBox1.Canvas, point(mx, my), point(mx + round(Grid * z2.re), my - round(Grid * z2.im)));
  // Rückausgabe
  If Not RadioButton1.Checked Then Begin
    Edit1.text := floattostr(z1.re);
    Edit2.text := floattostr(z1.im);
  End
  Else Begin
    Edit3.text := floattostr(GetR(z1));
    Edit4.text := floattostr(radtodeg(GetPhi(z1)));
  End;
  If Not RadioButton3.Checked Then Begin
    Edit5.text := floattostr(z2.re);
    Edit6.text := floattostr(z2.im);
  End
  Else Begin
    Edit7.text := floattostr(GetR(z2));
    Edit8.text := floattostr(radtodeg(GetPhi(z2)));
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'uComplex.pas Demo, www.Corpsman.de';
  Edit3.text := '1';
  Edit4.text := '45';
  Edit7.text := '1';
  Edit8.text := '135';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  selected := -1;
  If Collide(x, y, z1) Then selected := 1;
  If Collide(x, y, z2) Then selected := 2;
End;

Procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  If (ssleft In shift) And (selected <> -1) Then Begin
    If selected = 1 Then Begin
      RadioButton1.Checked := true;
      Edit1.text := floattostr((x - mx) / Grid);
      Edit2.text := floattostr(-(y - my) / Grid);
    End
    Else Begin
      RadioButton3.Checked := true;
      Edit5.text := floattostr((x - mx) / Grid);
      Edit6.text := floattostr(-(y - my) / Grid);
    End;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  selected := -1;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  PaintBox1.Invalidate; // Einlesen der Zahlen
  Case RadioGroup1.ItemIndex Of
    0: z1 := z1 + z2;
    1: z1 := z1 - z2;
    2: z1 := z1 * z2;
    3: z1 := z1 / z2;
    4: Z1 := KonjugateC(Z1);
    //    5:
    6: z1 := sqrC(z1);
    7: z1 := sqrtC(z1);
  End;
  // Rückausgabe von Z1
  Edit1.text := floattostr(z1.re);
  Edit2.text := floattostr(z1.im);
  Edit3.text := floattostr(GetR(z1));
  Edit4.text := floattostr(radtodeg(GetPhi(z1)));
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

End.

