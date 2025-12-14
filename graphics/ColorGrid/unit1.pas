(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of TColorGrid                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uColorGrid;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    ColorGrid: TColorGrid;
  public
    Procedure OnForeGroundColorChange(Sender: TObject);
    Procedure OnBackGroundColorChange(Sender: TObject);
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'TColorGrid demo ver. 0.01';
  ColorGrid := TColorGrid.Create(self);
  ColorGrid.Name := 'ColorGrid1';
  ColorGrid.Parent := self;
  ColorGrid.Top := 10;
  ColorGrid.Left := 10;
  ColorGrid.Width := 150;
  ColorGrid.Height := 150;
  ColorGrid.OnBackGroundColorChange := @OnBackGroundColorChange;
  ColorGrid.OnForeGroundColorChange := @OnForeGroundColorChange;
  OnForeGroundColorChange(ColorGrid);
  OnBackGroundColorChange(ColorGrid);
End;

Procedure TForm1.OnForeGroundColorChange(Sender: TObject);
Begin
  label1.caption := ColorToString(TColorGrid(sender).ForeGroundColor);
End;

Procedure TForm1.OnBackGroundColorChange(Sender: TObject);
Begin
  label2.caption := ColorToString(TColorGrid(sender).BackGroundColor);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  Close;
End;

End.

