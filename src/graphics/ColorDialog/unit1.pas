(******************************************************************************)
(* colordialog Demo                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : alternative colordialog oriented at the KDE Color-Picker     *)
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
  ExtCtrls, ucolordialog;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    C1: TColorDialog;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *)
  caption := 'Colordialog demo ver. 0.01';
  c1 := TColorDialog.Create(self);
  label1.caption := '';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If c1.Execute Then Begin
    label1.Caption := ColorToString(c1.Color);
  End
  Else Begin
    label1.Caption := '';
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  c1.ShowSimplifiedColors := CheckBox1.Checked;
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
Begin
  If CheckBox2.Checked Then Begin
    c1.WheelType := wtScience;
  End
  Else Begin
    c1.WheelType := wtArtists;
  End;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  c1.free;
  c1 := Nil;
End;

End.

