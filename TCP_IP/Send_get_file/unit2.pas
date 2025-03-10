(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Send_get_file                                         *)
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Const
  FileOverwrite = 0;
  FileContinue = 1;

Var
  Form2: TForm2;
  Form2Option: Integer;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm2.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  CanClose := Form2Option <> -1;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  Form2Option := FileOverwrite;
  close;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  Form2Option := FileContinue;
  close;
End;

End.

