(******************************************************************************)
(* YearGrid demo                                                   10.11.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : LCL-Component to visualise a year overview                   *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, uYearGrid;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    ColorDialog1: TColorDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    YearGrid1: TYearGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    Procedure FormCreate(Sender: TObject);
    Procedure YearGrid1DayClick(Sender: TObject; Year, Month, Day: integer);
    Procedure YearGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    Procedure SetInvalidColor(Sender: TObject);
    Procedure SetWeekendColor(Sender: TObject);
    Procedure SetTodayColor(Sender: TObject);
    Procedure SetWorkColor(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  aYear, aMonth, aDay: word;
Begin
  caption := 'Yeargrid demo ver. 0.01 by Corpsman';
  YearGrid1 := TYearGrid.Create(self);
  YearGrid1.Name := 'YearGrid1';
  YearGrid1.Parent := self;
  YearGrid1.Top := 8;
  YearGrid1.Left := 8;
  YearGrid1.Width := GroupBox1.Left - 16;
  YearGrid1.Height := ClientHeight - 16;
  YearGrid1.OnDayClick := @YearGrid1DayClick;
  YearGrid1.OnPrepareCanvas := @YearGrid1PrepareCanvas;
  YearGrid1.Anchors := [akBottom, akLeft, akRight, akTop];
  DecodeDate(now(), aYear, aMonth, aDay);
  label1.caption := IntToStr(aYear);
  CheckGroup1.Checked[0] := true;
  CheckGroup1.Checked[1] := true;
  CheckGroup1.Checked[2] := true;
  CheckGroup1.Checked[3] := true;
  CheckGroup1.Checked[4] := true;
  Constraints.MinHeight := 350;
  Constraints.MinWidth := 600;

  Shape1.OnClick := @SetInvalidColor;
  Shape1.Brush.Color := YearGrid1.NoValidDateColor;
  Shape2.OnClick := @SetWeekendColor;
  Shape2.Brush.Color := YearGrid1.WeekendColor;
  Shape3.OnClick := @SetTodayColor;
  Shape3.Brush.Color := YearGrid1.TodayColor;
  Shape4.OnClick := @SetWorkColor;
  Shape4.Brush.Color := YearGrid1.WorkColor;
End;

Procedure TForm1.YearGrid1DayClick(Sender: TObject; Year, Month, Day: integer);
Begin
  showmessage(format('%d.%d.%d', [Year, Month, Day]));
End;

Procedure TForm1.YearGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Begin
  (*
   * Change Brush.color depending on what ever your application needs to be
   *)
  If CheckBox1.Checked Then Begin
    If (aCol + aRow) Mod 2 = 0 Then Begin
      YearGrid1.Canvas.Brush.Color := clred;
    End;
  End;
End;

Procedure TForm1.SetInvalidColor(Sender: TObject);
Begin
  ColorDialog1.Color := YearGrid1.NoValidDateColor;
  If ColorDialog1.Execute Then Begin
    YearGrid1.NoValidDateColor := ColorDialog1.Color;
    YearGrid1.Invalidate;
    shape1.Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.SetWeekendColor(Sender: TObject);
Begin
  ColorDialog1.Color := YearGrid1.WeekendColor;
  If ColorDialog1.Execute Then Begin
    YearGrid1.WeekendColor := ColorDialog1.Color;
    YearGrid1.Invalidate;
    shape2.Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.SetTodayColor(Sender: TObject);
Begin
  ColorDialog1.Color := YearGrid1.TodayColor;
  If ColorDialog1.Execute Then Begin
    YearGrid1.TodayColor := ColorDialog1.Color;
    YearGrid1.Invalidate;
    shape3.Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.SetWorkColor(Sender: TObject);
Begin
  ColorDialog1.Color := YearGrid1.WorkColor;
  If ColorDialog1.Execute Then Begin
    YearGrid1.WorkColor := ColorDialog1.Color;
    YearGrid1.Invalidate;
    shape4.Brush.Color := ColorDialog1.Color;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  aYear: integer;
Begin
  ayear := strtoint(label1.Caption);
  aYear := aYear + 1;
  YearGrid1.LoadYear(aYear);
  label1.Caption := inttostr(aYear);
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  YearGrid1.Invalidate;
End;

Procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
Var
  wd, i: integer;
Begin
  YearGrid1.WeekendDays := [];
  For i := 0 To 6 Do Begin
    (*
     * The Listbox is Monday(0) -> Sunday(6)
     * YearGrid1 expects Sunday (1) -> Monday(2) -> Saturday (7)
     *)
    wd := ((i + 1) Mod 7) + 1;
    If Not CheckGroup1.Checked[i] Then YearGrid1.WeekendDays := YearGrid1.WeekendDays + [wd];
  End;
  YearGrid1.Invalidate;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  aYear: integer;
Begin
  ayear := strtoint(label1.Caption);
  aYear := aYear - 1;
  YearGrid1.LoadYear(aYear);
  label1.Caption := inttostr(aYear);
End;

End.

