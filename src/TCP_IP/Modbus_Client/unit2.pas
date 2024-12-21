(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Modbus Client                                         *)
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
  Grids, ExtCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      Const Value: String);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

Uses unit1, math;

{$R *.lfm}

Function hextointdef(HexString: String; DefaultValue: integer): integer;
Const
  Chars = '0123456789ABCDEF';
Var
  j, i: Integer;

Begin
  Try
    result := 0;
    HexString := UpperCase(HexString);
    For i := 1 To length(HexString) Do Begin
      j := pos(HexString[i], Chars);
      If j = 0 Then Begin
        result := DefaultValue;
        exit;
      End;
      result := result * 16;
      result := result + (j - 1);
    End;
  Except
    result := DefaultValue;
  End;
End;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Register overview';
  Edit1.text := '0';
  Edit2.text := '10';
  StringGrid1.cells[0, 0] := 'Register Nr';
  StringGrid1.cells[1, 0] := 'Hex';
  StringGrid1.cells[2, 0] := 'Decimal';
  StringGrid1.cells[3, 0] := 'Char';
  StringGrid1.ColWidths[0] := 100;
End;

Procedure TForm2.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  Const Value: String);
Var
  val, i: integer;
Begin
  Case acol Of
    1: Begin // Edit als Hex Zahl
        val := hextointdef(value, 0);
        val := val And $FFFF;
        Registers[strtointdef(edit1.text, 0) + arow - 1] := val;
      End;
    2: Begin // Edit als dezimal Zahl
        val := StrToIntDef(value, 0);
        If val < 0 Then val := 65536 + val;
        val := val And $FFFF;
        Registers[strtointdef(edit1.text, 0) + arow - 1] := val;
      End;
    3: Begin // Edit als Char
        // Das ist noch nicht Optimal, aber geht schon mal rudimentär
        val := 0;
        For i := 1 To min(length(Value), 2) Do Begin
          val := val * 256;
          val := val + ord(Value[i]);
        End;
        Registers[strtointdef(edit1.text, 0) + arow - 1] := val;
      End;
  End;
  Button2Click(Nil);
End;

Procedure TForm2.Timer1Timer(Sender: TObject);
Begin
  Button2.Click;
End;

Procedure TForm2.CheckBox1Change(Sender: TObject);
Begin
  timer1.enabled := CheckBox1.Checked;
End;

Procedure TForm2.Edit1Change(Sender: TObject);
Begin
  Button2.Click;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Var
  firstReg, Count, j: integer;
  s: String;
  b1, b2: byte;
Begin
  firstReg := StrToIntDef(Edit1.Text, 0);
  Count := StrToIntDef(Edit2.Text, 10);
  If (count <= 0) Or (firstReg < 0) Or (firstReg > 65535) Then exit;
  If firstReg + Count > 65535 Then Begin
    count := 65535 - firstReg;
  End;
  StringGrid1.RowCount := count + 1;
  For j := firstReg To firstReg + Count - 1 Do Begin
    StringGrid1.Cells[0, j - firstReg + 1] := 'Register: ' + IntToStr(j);
    StringGrid1.Cells[1, j - firstReg + 1] := format('%0.4X', [Registers[j]]);
    StringGrid1.Cells[2, j - firstReg + 1] := format('%d', [Registers[j]]);
    b1 := (Registers[j] Shr 8) And $FF;
    b2 := Registers[j] And $FF;
    s := '';
    If b1 In [32..255] Then Begin
      s := chr(b1);
    End
    Else Begin
      s := '';
    End;
    If b2 In [32..255] Then Begin
      s := s + chr(b2);
    End
    Else Begin
      s := s + '';
    End;
    StringGrid1.Cells[3, j - firstReg + 1] := s;
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  Close;
End;

End.

