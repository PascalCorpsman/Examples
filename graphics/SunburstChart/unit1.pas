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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
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
    aSelected: PChild;

    Procedure OnChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnChartResize(Sender: Tobject);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math, unit2;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'sunburst demo ver 0.01';
  panel1.caption := '';
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

 //{
(* Vollkreis
setlength(Chart.Root, 1);
Chart.Root[0] := DefaultChild;
Chart.Root[0].Color.BrushColor := clRed;
Chart.Root[0].Caption := 'Level1';
// *)

// (* Gefüllt mit innen ausgefüllt
  setlength(Chart.Root, 3);
  Chart.Root[0] := DefaultChild;
  Chart.Root[0].Color.BrushColor := clRed;
  Chart.Root[0].Caption := 'Red';
  Chart.Root[0].value := 3;

  Chart.Root[1] := DefaultChild;
  Chart.Root[1].Color.BrushColor := clGreen;
  Chart.Root[1].Caption := 'Green';

  Chart.Root[2] := DefaultChild;
  Chart.Root[2].Color.BrushColor := clBlue;
  Chart.Root[2].Caption := 'Blue';

  setlength(Chart.Root[0].Childrens, 2);

  Chart.Root[0].Childrens[0] := DefaultChild();
  Chart.Root[0].Childrens[0].Color.BrushColor := clLime;
  Chart.Root[0].Childrens[0].Value := 2;
  Chart.Root[0].Childrens[0].Caption := 'Lime';

  Chart.Root[0].Childrens[1] := DefaultChild();
  Chart.Root[0].Childrens[1].Color.BrushColor := clFuchsia;
  Chart.Root[0].Childrens[1].Caption := 'Fuchsia';


  setlength(Chart.Root[2].Childrens, 1);
  Chart.Root[2].Childrens[0] := DefaultChild();
  Chart.Root[2].Childrens[0].Color.BrushColor := clSilver;
  Chart.Root[2].Childrens[0].Caption := 'Silver';

  setlength(Chart.Root[0].Childrens[1].Childrens, 1);
  Chart.Root[0].Childrens[1].Childrens[0] := DefaultChild();
  Chart.Root[0].Childrens[1].Childrens[0].Color.BrushColor := clNavy;
  Chart.Root[0].Childrens[1].Childrens[0].Caption := 'Navy';
  Chart.Root[0].Childrens[1].Childrens[0].Color.FontColor := clWhite;
  // *)

  (*// Ein "Ring" -> Das Innerste Element Leer
  SetLength(chart.Root, 1);
  chart.Root[0] := DefaultChild();
  chart.Root[0].Color.BrushColor := clWhite;
  chart.Root[0].Color.PenColor := clWhite;

  setlength(chart.Root[0].Childrens, 3);

  chart.Root[0].Childrens[0] := DefaultChild();
  chart.Root[0].Childrens[0].Color.BrushColor := clRed;

  chart.Root[0].Childrens[1] := DefaultChild();
  chart.Root[0].Childrens[1].Color.BrushColor := clGreen;

  chart.Root[0].Childrens[2] := DefaultChild();
  chart.Root[0].Childrens[2].Color.BrushColor := clBlue;

  setlength(chart.Root[0].Childrens[1].Childrens, 2);
  chart.Root[0].Childrens[1].Childrens[0] := DefaultChild();
  chart.Root[0].Childrens[1].Childrens[0].Color.BrushColor := clNavy;
  chart.Root[0].Childrens[1].Childrens[1] := DefaultChild();
  chart.Root[0].Childrens[1].Childrens[1].Color.BrushColor := clLime;
  // *)
  //}
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Load from file
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Save to file
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Add Element
  form2.LoadChildDataToLCL(DefaultChild());
  form2.caption := 'Add element';
  If form2.ShowModal = mrOK Then Begin
    Chart.AddChild(aSelected, form2.GetChildDataFromLCL());
    Chart.Deselect;
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
      Chart.Deselect;
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
    If Chart.DelChild(aSelected) Then Begin
      chart.Deselect;
      aselected := Nil;
    End;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Add Sibling
  If assigned(aSelected) Then Begin
    form2.LoadChildDataToLCL(DefaultChild());
    form2.caption := 'Add sibling';
    If form2.ShowModal = mrOK Then Begin
      Chart.AddSibling(aSelected, form2.GetChildDataFromLCL());
      Chart.Deselect;
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
  Chart.StageMargin := ScrollBar3.Position;
  chart.Invalidate;
End;

Procedure TForm1.OnChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  segment: PChild;
Begin
  Chart.Deselect;
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

End.

