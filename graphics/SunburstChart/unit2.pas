(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sunburst_demo                                         *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  usunburstchart;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
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
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Procedure FormCreate(Sender: TObject);
  private
    fPrivateChild: TSunBurstChartElement;
    Procedure OnShapeClick(Sender: TObject);
  public
    Function GetChildDataFromLCL(): TSunBurstChartElement;
    Procedure LoadChildDataToLCL(Const aElement: TSunBurstChartElement);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Shape1.OnClick := @OnShapeClick;
End;

Procedure TForm2.OnShapeClick(Sender: TObject);
Begin
  ColorDialog1.Color := TShape(sender).Brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(sender).Brush.Color := ColorDialog1.Color;
  End;
End;

Function TForm2.GetChildDataFromLCL: TSunBurstChartElement;
Begin
  result := fPrivateChild;
  result.caption := edit1.text;
  result.Value := strtoint(edit4.text);
  // Color
  result.Color.BrushColor := Shape1.Brush.Color;
  result.Color.FontColor := Shape2.Brush.Color;
  result.Color.PenColor := Shape3.Brush.Color;
  result.Color.PenWitdh := strtoint(edit2.text);
  // Selected
  result.SelectedColor.BrushColor := Shape4.Brush.Color;
  result.SelectedColor.FontColor := Shape5.Brush.Color;
  result.SelectedColor.PenColor := Shape6.Brush.Color;
  result.SelectedColor.PenWitdh := strtoint(edit3.text);
End;

Procedure TForm2.LoadChildDataToLCL(Const aElement: TSunBurstChartElement);
Begin
  fPrivateChild := aElement;
  edit1.text := aElement.Caption;
  edit4.text := inttostr(aElement.Value);

  // Color
  Shape1.Brush.Color := aElement.Color.BrushColor;
  Shape2.Brush.Color := aElement.Color.FontColor;
  Shape3.Brush.Color := aElement.Color.PenColor;
  edit2.text := inttostr(aElement.Color.PenWitdh);
  // Selected
  Shape4.Brush.Color := aElement.SelectedColor.BrushColor;
  Shape5.Brush.Color := aElement.SelectedColor.FontColor;
  Shape6.Brush.Color := aElement.SelectedColor.PenColor;
  edit3.text := inttostr(aElement.SelectedColor.PenWitdh);
End;

End.

