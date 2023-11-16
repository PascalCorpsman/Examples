(******************************************************************************)
(* SimpleChart demo                                                15.11.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Corpsman                                                     *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : The primitive reimplementation of a TAChart similar component*)
(*               especially for console applications. TSimpleChart supports   *)
(*               as many TSeries with their own Y-achsis scalings as needed.  *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, usimplechart;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    PaintBox1: TPaintBox;
    RadioGroup1: TRadioGroup;
    Shape1: TShape;
    Shape2: TShape;
    SpinEdit1: TSpinEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure CheckBox5Click(Sender: TObject);
    Procedure Edit10Change(Sender: TObject);
    Procedure Edit11Change(Sender: TObject);
    Procedure Edit12Change(Sender: TObject);
    Procedure Edit13Change(Sender: TObject);
    Procedure Edit14Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure OnShapeClick(sender: TObject);
  private
    fChart: TSimpleChart;
    Procedure LCLToSeries(Var s: TSeries);
    Procedure SeriesToLCL(Const s: TSeries);
  public

    Procedure InitialPopulate();

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'TSimplechart demo ver. 0.01 by Corpsman';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Randomize;
  fChart := TSimpleChart.Create();
  Edit1.text := 'Series1';
  Shape1.brush.Color := clBlack;
  Shape2.brush.Color := clGray;
  edit2.text := '';
  Edit3.text := '0';
  Edit4.text := '100';
  edit5.text := '%f';
  edit6.text := '0';
  edit7.text := '100';
  edit9.text := '-15';
  edit8.text := '50';
  edit10.text := 'Demo diagram';
  edit11.text := '';
  edit12.text := '0';
  edit13.text := '100';
  edit14.text := '%f';
  Shape1.OnClick := @OnShapeClick;
  Shape2.OnClick := @OnShapeClick;
  InitialPopulate();
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Var
  s: TSeries;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    s := fChart.Series[ListBox1.ItemIndex];
    SeriesToLCL(s);
  End;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  p: TPortableNetworkGraphic;
Begin
  If Not assigned(fChart) Then exit;
  p := fChart.SaveToPngImage(PaintBox1.Width, PaintBox1.Height);
  PaintBox1.Canvas.Draw(0, 0, p);
  p.free;
End;

Procedure TForm1.OnShapeClick(sender: TObject);
Begin
  ColorDialog1.Color := TShape(Sender).brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(Sender).brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.LCLToSeries(Var s: TSeries);
Begin
  s.SeriesCaption := edit1.text;
  s.SeriesWidth := SpinEdit1.Value;
  s.SeriesColor := Shape1.Brush.Color;
  s.GridColor := Shape2.Brush.Color;
  Case RadioGroup1.ItemIndex Of
    0: s.YAxis.Pos := apLeft;
    1: s.YAxis.Pos := apRight;
    2: s.YAxis.Pos := apNone;
  End;
  s.YAxis.AxisUnit := edit2.text;
  s.YAxis.UseMinVal := CheckBox1.Checked;
  s.YAxis.MinVal := strtofloatdef(edit3.text, 0);
  s.YAxis.UseMaxVal := CheckBox2.Checked;
  s.YAxis.MaxVal := strtofloatdef(edit4.text, 100);
  s.YAxis.MarkFormat := edit5.text;
End;

Procedure TForm1.SeriesToLCL(Const s: TSeries);
Begin
  edit1.text := s.SeriesCaption;
  SpinEdit1.Value := s.SeriesWidth;
  Shape1.Brush.Color := s.SeriesColor;
  Shape2.Brush.Color := s.GridColor;
  Case s.YAxis.Pos Of
    apLeft: RadioGroup1.ItemIndex := 0;
    apRight: RadioGroup1.ItemIndex := 1;
    apNone: RadioGroup1.ItemIndex := 2;
  End;
  edit2.text := s.YAxis.AxisUnit;
  CheckBox1.Checked := s.YAxis.UseMinVal;
  edit3.text := floattostr(s.YAxis.MinVal);
  CheckBox2.Checked := s.YAxis.UseMaxVal;
  edit4.text := floattostr(s.YAxis.MaxVal);
  edit5.text := s.YAxis.MarkFormat;
End;

Procedure TForm1.InitialPopulate();
Begin
  // Die 1. Kurve Definiert auch die Linke Y-Achse
  CheckBox1.Checked := true;
  CheckBox2.Checked := true;
  Edit3.text := '-15';
  Edit2.text := ' V';
  Edit5.text := '%.0f';
  Button1.Click;
  // Die 2. Kurve Teil sich die Y-Achse mit der 1. Kurve
  edit1.text := 'Series2';
  Edit5.text := '%f';
  Edit2.text := '';
  RadioGroup1.ItemIndex := 2;
  Shape1.Brush.Color := clred;
  Edit9.text := '50';
  Edit8.text := '100';
  SpinEdit1.Value := 2;
  Button1.Click;
  // Die 3. Kurve hat eine Eigene Achse Rechts
  edit1.text := 'Series3';
  SpinEdit1.Value := 1;
  Shape1.Brush.Color := clBlue;
  RadioGroup1.ItemIndex := 1;
  Edit3.text := '-10';
  Edit4.text := '20';
  Edit9.text := '0';
  Edit8.text := '10';
  Button1.Click;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  fChart.free;
  fChart := Nil;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  s: TSeries;
Begin
  // Update Series
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Nothing selected.');
    exit;
  End;
  s := fChart.Series[ListBox1.ItemIndex];
  LCLToSeries(s);
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Del Series
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Nothing selected.');
    exit;
  End;
  fChart.Series[ListBox1.ItemIndex].Free;
  ListBox1.Items.Delete(ListBox1.ItemIndex);
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Begin
  fChart.ShowLegend := CheckBox3.Checked;
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox4Click(Sender: TObject);
Begin
  fChart.XAXis.UseMinVal := CheckBox4.Checked;
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox5Click(Sender: TObject);
Begin
  fChart.XAXis.UseMaxVal := CheckBox5.Checked;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit10Change(Sender: TObject);
Begin
  fChart.Title := Edit10.Text;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit11Change(Sender: TObject);
Begin
  fChart.XAXis.AxisUnit := Edit11.Text;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit12Change(Sender: TObject);
Begin
  fChart.XAXis.MinVal := StrToFloatDef(edit12.text, 0);
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit13Change(Sender: TObject);
Begin
  fChart.XAXis.MaxVal := StrToFloatDef(edit13.text, 100);
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit14Change(Sender: TObject);
Begin
  fChart.XAXis.MarkFormat := edit14.text;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  Offset, range, i, y: Integer;
  s: TSeries;
Begin
  // Add Series
  s := TSeries.Create();
  LCLToSeries(s);

  // Populate Series with data
  Offset := strtointdef(edit9.text, 0);
  range := strtointdef(edit8.text, 100) - offset;
  For i := strtointdef(edit6.text, 0) To strtointdef(edit7.text, 100) Do Begin
    y := random(range) + Offset;
    s.AddDataPoint(i, y);
  End;

  fChart.AddSeries(s);
  ListBox1.Items.Add(edit1.text);
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;

  PaintBox1.Invalidate;
End;

End.

