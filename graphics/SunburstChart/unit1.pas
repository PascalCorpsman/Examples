(******************************************************************************)
(* Sunburst_demo                                                   30.07.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo application for usunburstchart.pas                      *)
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
  Menus, usunburstchart;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    Shape1: TShape;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
  private
    Chart: TSunburstChart;
    aSelected: PSunBurstChartElement;

    Procedure OnChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnChartResize(Sender: Tobject);
    Procedure OnShapeClick(Sender: TObject);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math, unit2;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
  Function Child(bc: TColor; c: String; v: integer): TSunBurstChartElement;
  Begin
    result := DefaultSunBurstChartElement();
    result.Color.BrushColor := bc;
    result.Caption := c;
    result.Value := v;
  End;

Var
  n, r, b, f: PSunBurstChartElement;
Begin
  caption := 'sunburst demo ver 0.01';
  panel1.caption := '';
  Shape1.OnClick := @OnShapeClick;
  edit1.text := '4';
  (*
   * Create TSunburstChart by hand, this prevents us from the need to install the component into the IDE ;)
   *)
  Chart := TSunburstChart.Create(self);
  Chart.Name := 'THierarchicalPieChart1';
  chart.Parent := self;

  Chart.Left := 0;
  Chart.Top := 0;
  Chart.Width := 500;
  Chart.Height := 500;

  chart.Anchors := [akBottom, akLeft, akRight, akTop];
  chart.PopupMenu := PopupMenu1;

  Chart.OnMouseDown := @OnChartMouseDown;
  Chart.OnResize := @OnChartResize;
  OnChartResize(Nil);
  aSelected := Nil;

  // Initial "Fillings" ;)
  // Ring 1
  r := Chart.AddChildElement(Nil, Child(clred, 'Red', 3));
  Chart.AddChildElement(Nil, Child(clGreen, 'Green', 1));
  b := Chart.AddChildElement(Nil, Child(clBlue, 'Blue', 1));

  // Ring 2
  chart.AddChildElement(r, child(clLime, 'Lime', 2));
  f := chart.AddChildElement(r, child(clfuchsia, 'Fuchsia', 1));
  chart.AddChildElement(b, child(clSilver, 'Silver', 1));

  // Ring 3
  n := Chart.AddChildElement(f, child(clNavy, 'Navy', 1));
  n^.Color.FontColor := clWhite;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  w: integer;
  c: TColor;
  //p: PSunBurstChartElement; // Debug Only
Begin
  // Demo of iterater pattern set something for all Elements.
  w := max(1, strtointdef(edit1.text, 1));
  c := Shape1.Brush.Color;
  Chart.IterFirst;
  While assigned(Chart.Iterator) Do Begin
    //p := Chart.Iterator;
    Chart.Iterator^.Color.PenWitdh := w;
    Chart.Iterator^.Color.PenColor := c;
    chart.IterNext;
  End;
  chart.Invalidate;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Load from file
  If OpenDialog1.Execute Then Begin
    If Chart.LoadFromFile(OpenDialog1.FileName) Then Begin
      OnChartResize(Nil);
      ScrollBar1.Position := round(radtodeg(Chart.InitialArc));
      ScrollBar2.Position := round(radtodeg(Chart.AngleOffset));
      ScrollBar3.Position := Chart.LevelMargin;
    End
    Else Begin
      showmessage('Error, during load.');
    End;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Save to file
  If SaveDialog1.Execute Then Begin
    If Not Chart.SaveToFile(SaveDialog1.FileName) Then Begin
      showmessage('Error, during save.');
    End;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Add Element
  form2.LoadChildDataToLCL(DefaultSunBurstChartElement());
  form2.caption := 'Add element';
  If form2.ShowModal = mrOK Then Begin
    Chart.AddChildElement(aSelected, form2.GetChildDataFromLCL());
    Chart.DeselectAll;
    aSelected := Nil;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Clear
  Chart.Clear;
  Chart.Invalidate;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Edit Element
  If assigned(aSelected) Then Begin
    form2.LoadChildDataToLCL(aSelected^);
    form2.caption := 'Edit element';
    If form2.ShowModal = mrOK Then Begin
      aSelected^ := form2.GetChildDataFromLCL();
      Chart.DeselectAll;
      aselected := Nil;
    End;
  End
  Else Begin
    showmessage('Error, no element selected, please select a element first.');
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Del Element
  If assigned(aSelected) Then Begin
    If Chart.DelElement(aSelected) Then Begin
      chart.DeselectAll;
      aselected := Nil;
      Chart.Invalidate;
    End;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Add Sibling
  If assigned(aSelected) Then Begin
    form2.LoadChildDataToLCL(DefaultSunBurstChartElement());
    form2.caption := 'Add sibling';
    If form2.ShowModal = mrOK Then Begin
      Chart.AddSiblingElement(aSelected, form2.GetChildDataFromLCL());
      Chart.DeselectAll;
      aselected := Nil;
    End;
  End
  Else Begin
    showmessage('Error, no element selected, please select a element first.');
  End;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  Chart.InitialArc := DegToRad(ScrollBar1.Position);
  Chart.Invalidate;
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  Chart.AngleOffset := DegToRad(ScrollBar2.Position);
  Chart.Invalidate;
End;

Procedure TForm1.ScrollBar3Change(Sender: TObject);
Begin
  Chart.LevelMargin := ScrollBar3.Position;
  chart.Invalidate;
End;

Procedure TForm1.OnChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  segment: PSunBurstChartElement;
Begin
  Chart.DeselectAll;
  If Chart.GetSegmentAtPos(x, y, segment) Then Begin
    segment^.Selected := true;
    aSelected := segment;
  End
  Else Begin
    aSelected := Nil;
  End;
  Chart.Invalidate;
End;

Procedure TForm1.OnChartResize(Sender: Tobject);
Begin
  Chart.PieCenter := point(Chart.Width Div 2, Chart.Height Div 2);
  chart.PieRadius := min(Chart.Width / 2, Chart.Height / 2) - 10;
  chart.Invalidate;
End;

Procedure TForm1.OnShapeClick(Sender: TObject);
Begin
  ColorDialog1.Color := TShape(Sender).brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(Sender).brush.Color := ColorDialog1.Color;
  End;
End;

End.

