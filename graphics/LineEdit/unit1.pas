(******************************************************************************)
(* Demo for uLinedit.pas                                           15.04.2025 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Visual component to edit a function with multiple base points*)
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
(*               0.02 - made Ux more intuitive                                *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, uLineEdit;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    SpeedButton1: TToggleBox;
    SpeedButton2: TToggleBox;
    SpeedButton3: TToggleBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Procedure OnLineEdit1PointChange(Sender: TObject);
    Function Scale(value: Word): Word;
  public
    LineEdit1: TLineEdit;

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses
  IntfGraphics, // TLazIntfImage
  fpImage, // TFPColor
  math;

(*
 * in x in [0..1], a in [-1 .. 1]
 * => result in [0..1]
 *)

Function f(x, a: Single): Single;
Begin
  //  Result := x + a * (1 - 2 * x) * x * (1 - x); // Eine "Andere" Art das Exp via ein Polynom an zu nähern..
  //  exit;
  x := x * 2 - 1; // x in [0..1] -> x in [-1 .. 1]
  If x < 0 Then Begin
    result := -power(-x, 1 + a);
  End
  Else Begin
    result := power(x, 1 + a);
  End;
  result := (result + 1) / 2; // result in [-1 ..1 ] -> [0 .. 1]
End;

(*
 * in x in [0..1], a in [-0.035 .. 0.035]
 * => result.x in [0..1]
 *    result.y in [0..1]
 *)

Function f2(x, a: Single): TPercentPoint;
Var
  vx, vy, s, xx, yy: Single;
Begin
  // Quell X transformieren
  xx := (x - 0.5) * sqrt(2); // [0..1] --> [-sqrt(2)/2 .. sqrt(2)/2]
  // Berechnen der Parabel
  yy := a * (1 - 2 * sqr(xx)); // [xx, yy]
  // Rücktransformation in Ursprungsraum [-0.5 .. 0.5]
  s := sqrt(2) / 2;
  vx := s * xx - s * yy;
  vy := s * xx + s * yy;
  // Translation, auf [0..1]
  result.x := vx + 0.5;
  result.y := vy + 0.5;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'LineEdit demo ver. 0.02';
  LineEdit1 := TLineEdit.Create(self);
  LineEdit1.Name := 'LineEdit1';
  LineEdit1.Parent := self;
  LineEdit1.Top := 10;
  LineEdit1.Left := 10;
  LineEdit1.Width := Scale96ToForm(256);
  LineEdit1.Height := Scale96ToForm(256);
  LineEdit1.BrushColor := clBlack;
  LineEdit1.PenColor := clYellow;
  LineEdit1.GridColor := clYellow Div 3;
  LineEdit1.GridPercent := 25;
  LineEdit1.LineColor := clWhite;
  LineEdit1.PointBoxColor := clBlue;
  LineEdit1.OnPointsChange := @OnLineEdit1PointChange;
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Const
  steps = 10;
Var
  i: integer;
Begin
  If LineEdit1.PointCount <> steps Then Begin
    LineEdit1.Reset;
    For i := 1 To steps - 2 Do Begin
      LineEdit1.AddPoint(PercentPoint(100 * i / (steps - 1), 0));
    End;
  End;
  For i := 0 To steps - 1 Do Begin
    LineEdit1.SetPointXValue(i, 100 * i / (steps - 1));
    LineEdit1.SetPointYValue(i, 100 * f(i / (steps - 1), ScrollBar1.Position / 100));
  End;
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Const
  steps = 10;
Var
  i: integer;
  p: TPercentPoint;
Begin
  LineEdit1.Reset;
  For i := 1 To steps - 2 Do Begin
    p := f2(i / (steps - 1), ScrollBar2.Position / 100);
    p.x := p.x * 100;
    p.Y := p.Y * 100;
    LineEdit1.AddPoint(p);
  End;
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  LineEdit1.Reset;
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  LineEdit1.Reset;
  LineEdit1.SetPointYValue(0, 100);
  LineEdit1.SetPointYValue(1, 0);
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  d: Single;
Begin
  // Reduce Colors 5
  d := 100 / LineEdit1.Width;
  LineEdit1.Reset;
  // first point 0,0 already exists
  LineEdit1.AddPoint(PercentPoint(20, 0));
  LineEdit1.AddPoint(PercentPoint(20 + d, 25));
  LineEdit1.AddPoint(PercentPoint(40, 25));
  LineEdit1.AddPoint(PercentPoint(40 + d, 50));
  LineEdit1.AddPoint(PercentPoint(60, 50));
  LineEdit1.AddPoint(PercentPoint(60 + d, 75));
  LineEdit1.AddPoint(PercentPoint(80, 75));
  LineEdit1.AddPoint(PercentPoint(80 + d, 100));
  // last point 100, 100 already exists
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  d: Single;
Begin
  // Reduce Colors 3
  d := 100 / LineEdit1.Width;
  LineEdit1.Reset;
  // first point 0,0 already exists
  LineEdit1.AddPoint(PercentPoint(33, 0));
  LineEdit1.AddPoint(PercentPoint(33 + d, 50));
  LineEdit1.AddPoint(PercentPoint(66, 50));
  LineEdit1.AddPoint(PercentPoint(66 + d, 100));
  // last point 100, 100 already exists
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  d: Single;
Begin
  // Binarize
  d := 100 / LineEdit1.Width;
  LineEdit1.Reset;
  // first point 0,0 already exists
  LineEdit1.AddPoint(PercentPoint(50, 0));
  LineEdit1.AddPoint(PercentPoint(50 + d, 100));
  // last point 100, 100 already exists
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Rainbow 1
  LineEdit1.Reset;
  LineEdit1.SetPointYValue(0, 100);
  LineEdit1.AddPoint(PercentPoint(25, 0));
  LineEdit1.AddPoint(PercentPoint(50, 100));
  LineEdit1.AddPoint(PercentPoint(75, 0));
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  // Rainbow 2
  LineEdit1.Reset;
  LineEdit1.AddPoint(PercentPoint(25, 100));
  LineEdit1.AddPoint(PercentPoint(50, 0));
  LineEdit1.AddPoint(PercentPoint(75, 100));
  LineEdit1.SetPointYValue(100, 0);
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Begin

  OnLineEdit1PointChange(Nil);
End;

procedure TForm1.SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  If TSpeedButton(sender).Down Then
  TSpeedButton(sender).Down := false;
end;

Function TForm1.Scale(value: Word): Word;
Var
  s: Single;
Begin
  // 1. Farbwert -> 0..100
  s := (value Shr 8) / 2.55;
  // 2. Umwandeln durch LineEdit1
  s := LineEdit1.F(s);
  // 3. 0..100 -> Farbwert
  result := min(255, max(0, round(s * 2.55))) Shl 8;
End;

Procedure TForm1.OnLineEdit1PointChange(Sender: TObject);
Var
  Target: TBitmap;
  SourceIntf: TLazIntfImage;
  i, j: Integer;
  col: TFPColor;
Begin
  SourceIntf := Image1.Picture.Bitmap.CreateIntfImage;
  // Anwenden der Filter
  For i := 0 To SourceIntf.Width - 1 Do Begin
    For j := 0 To SourceIntf.Height - 1 Do Begin
      col := SourceIntf.Colors[i, j];
      If SpeedButton1.State = cbChecked Then Begin
        col.Red := Scale(col.Red);
      End;
      If SpeedButton2.State = cbChecked Then Begin
        col.Green := Scale(col.Green);
      End;
      If SpeedButton3.State = cbChecked Then Begin
        col.Blue := Scale(col.Blue);
      End;
      SourceIntf.Colors[i, j] := col;
    End;
  End;
  Target := TBitmap.Create;
  Target.LoadFromIntfImage(SourceIntf);
  image2.Picture.Assign(Target);
  Target.Free;
  SourceIntf.free;
End;

End.


