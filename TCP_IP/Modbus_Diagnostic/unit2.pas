(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Modbus_diagnostic                                     *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, synaser;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    Procedure Button3Click(Sender: TObject);
    Procedure ComboBox3KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Refresh_Gui_Elements();
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.ComboBox3KeyPress(Sender: TObject; Var Key: char);
Begin
  key := #0;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm2.RadioGroup1Click(Sender: TObject);
Begin
  Refresh_Gui_Elements();
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  ComboBox2.Items.CommaText := GetSerialPortNames();
End;

Procedure TForm2.Refresh_Gui_Elements;
Begin
  label1.visible := false; // Seriell
  label2.visible := false; // Seriell
  label3.visible := false; // Seriell
  label4.visible := false; // Ethernet oder RTU Ethernet
  label5.visible := false; // Ethernet oder RTU Ethernet
  Edit1.Visible := false; // Ethernet
  Edit2.Visible := false; // Ethernet
  Edit3.Visible := false; // RTU Ethernet
  Edit4.Visible := false; // RTU Ethernet
  Button3.Visible := false; // Seriell
  ComboBox1.Visible := false; // Seriell
  ComboBox2.Visible := false; // Seriell
  ComboBox3.Visible := false; // Seriell
  Case RadioGroup1.ItemIndex Of
    0: Begin // Ethernet
        label4.visible := True;
        label5.visible := True;
        Edit1.Visible := True;
        Edit2.Visible := True;
      End;
    1: Begin // RTU Ethernet
        label4.visible := True;
        label5.visible := True;
        Edit3.Visible := True;
        Edit4.Visible := True;
      End;
    2: Begin // Seriell
        label1.visible := True;
        label2.visible := True;
        label3.visible := True;
        Button3.Visible := true;
        ComboBox1.Visible := true;
        ComboBox2.Visible := true;
        ComboBox3.Visible := true;
      End;
  End;
End;

End.

