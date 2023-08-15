(******************************************************************************)
(* Circlepack_demo                                                 18.08.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo application for ucirclepackchart.pas                    *)
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
  Menus, ucirclepackchart;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Shape1: TShape;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure PackedCircleChartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OnShapeClick(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private

  public
    aChart: TPackedCircleChart;
    aSelected: PCircle;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LCLType, Unit2;

Const
  bColors: Array[0..7] Of TColor =
  (
    clRed, clgreen, clBlue, clwhite, clGray,
    clmaroon, cllime, clyellow
    );

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
  c: TCircle;
Begin
  caption := 'Packed circle demo ver. 0.01';
  Edit1.text := '4';
  Edit2.text := '50';
  Shape1.OnClick := @OnShapeClick;
  aChart := TPackedCircleChart.Create(self);
  aChart.Name := 'TPackedCircleChart1';
  aChart.Parent := Self;
  aChart.Top := 0;
  aChart.Left := 0;
  aChart.width := 600;
  aChart.Height := 600;
  aChart.Anchors := [akLeft, akTop, akRight, akBottom];
  aChart.OnMouseDown := @PackedCircleChartMouseDown;
  aChart.PopupMenu := PopupMenu1;
  aSelected := Nil;
  For i := 0 To 49 Do Begin
    c := DefaultCircle();
    c.Value := random(40) + 10;
    c.Color.BrushColor := bColors[random(length(bColors))];
    c.Caption := inttostr(i);
    aChart.AddCircle(c);
  End;
  aChart.Invalidate;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Add Circle
  form2.LoadCircleDataToLCL(DefaultCircle());
  form2.caption := 'Add element';
  If form2.ShowModal = mrOK Then Begin
    aChart.AddCircle(form2.GetCircleDataFromLCL());
    aChart.DeselectAll;
    aSelected := Nil;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  oldValue: Integer;
Begin
  // Edit Circle
  If assigned(aSelected) Then Begin
    form2.LoadCircleDataToLCL(aSelected^);
    form2.caption := 'Edit element';
    If form2.ShowModal = mrOK Then Begin
      oldValue := aSelected^.Value;
      aSelected^ := form2.GetCircleDataFromLCL();
      aChart.DeselectAll;
      If oldValue <> aSelected^.Value Then Begin
        aChart.NeedReEvaluation;
      End;
      aChart.Invalidate;
      aselected := Nil;
    End;
  End
  Else Begin
    showmessage('Error, no element selected, please select a element first.');
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Del Element
  If assigned(aSelected) Then Begin
    If aChart.DelCircle(aSelected) Then Begin
      achart.DeselectAll;
      aselected := Nil;
      aChart.Invalidate;
    End;
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Clear
  aChart.Clear;
  aChart.Invalidate;
End;

Procedure TForm1.PackedCircleChartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  aChart.DeselectAll;
  If aChart.GetCircleAtPos(x, y, aSelected) Then Begin
    aSelected^.Selected := true;
  End
  Else Begin
    aSelected := Nil;
  End;
  Invalidate;
End;

Procedure TForm1.OnShapeClick(Sender: TObject);
Begin
  ColorDialog1.Color := TShape(Sender).Brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(Sender).Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  c: TCircle;
Begin
  If aChart.CircleCount >= StrToIntDef(edit2.text, 50) Then Begin
    Timer1.Enabled := false;
  End
  Else Begin
    c := DefaultCircle();
    c.Value := random(40) + 10;
    c.Color.BrushColor := bColors[random(length(bColors))];
    c.Caption := inttostr(aChart.CircleCount);
    aChart.AddCircle(c);
    aChart.Invalidate;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Load from File
  If OpenDialog1.Execute Then Begin
    If aChart.LoadFromFile(OpenDialog1.FileName) Then Begin
      aChart.Invalidate;
    End
    Else Begin
      showmessage('Error, during load.');
    End;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Save to File
  If SaveDialog1.Execute Then Begin
    If Not aChart.SaveToFile(SaveDialog1.FileName) Then Begin
      showmessage('Error, during save.');
    End;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Demo of iterater pattern set something for all Elements.
  aChart.IterFirst;
  While assigned(achart.Iterator) Do Begin
    achart.Iterator^.Color.PenWitdh := strtointdef(edit1.text, 1);
    achart.Iterator^.Color.BrushColor := Shape1.Brush.Color;
    aChart.IterNext;
  End;
  aChart.Invalidate;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  aChart.Clear;
  aChart.Invalidate;
  Timer1.Enabled := True;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If aChart.Changed Then Begin
    If id_no = application.MessageBox('Close without save ?', 'Question', mb_yesno Or mb_iconquestion) Then canclose := false;
  End;
End;

End.

