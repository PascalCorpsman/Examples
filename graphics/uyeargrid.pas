(******************************************************************************)
(* uYearGrid                                                       ??.??.???? *)
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


Unit uYearGrid;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Grids, Graphics;

Type

  TOnDayClickEvent = Procedure(Sender: TObject; Year, Month, Day: integer) Of Object;

  { TYearGrid }

  TYearGrid = Class(TStringGrid)
  private
    fYear: integer;
    fUserPrepareCanvas: TOnPrepareCanvasEvent;
    Function getPrepareCanvas: TOnPrepareCanvasEvent;
    Procedure setPrepareCanvas(AValue: TOnPrepareCanvasEvent);
    Procedure OnPrepareCanvasEvent(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);

    Function DayExistsInYear(Month, Day: Integer): Boolean;

  protected
    Procedure Click; override;
    Procedure DoOnResize; override;

  public
    // TODO: All die hier müssen noch mit settern ausgestatet werden und dann ein Invalidate in die Setter
    WeekendDays: Set Of byte; // 1 = Sunday, 2 = Monday, .. 7 = Saturday
    NoValidDateColor: TColor;
    WeekendColor: TColor; // Disable by setting "WeekendDays" to []
    WorkColor: TColor;
    TodayColor: TColor; // if = clnone = not displayed.

    OnDayClick: TOnDayClickEvent; // TODO: Den Onclick auch so schützen wie den Prepare Canvas

    Property OnPrepareCanvas: TOnPrepareCanvasEvent read getPrepareCanvas write setPrepareCanvas;
    Property Year: integer read fYear;

    Constructor Create(AOwner: TComponent); override;

    Function IsWeekendDay(Value: TDateTime): Boolean;

    Procedure LoadYear(aYear: Integer);
  End;


Implementation

Uses StdCtrls, forms;

{ TYearGrid }

Constructor TYearGrid.Create(AOwner: TComponent);
Var
  d, m, y: word;
Begin
  Inherited Create(AOwner);
  Options := Options - [goRangeSelect];
  fUserPrepareCanvas := Nil;
  DecodeDate(now, y, m, d);
  LoadYear(y);
  WeekendDays := [1, 7];
  NoValidDateColor := clGray;
  WeekendColor := TColor($F0F0F0);
  WorkColor := clWhite;
  TodayColor := clBlue;
  OnDayClick := Nil;
  Inherited OnPrepareCanvas := @OnPrepareCanvasEvent;
  ScrollBars := ssNone;
End;

Function TYearGrid.IsWeekendDay(Value: TDateTime): Boolean;
Begin
  result := DayOfWeek(Value) In WeekendDays;
End;

Function TYearGrid.getPrepareCanvas: TOnPrepareCanvasEvent;
Begin
  result := fUserPrepareCanvas;
End;

Procedure TYearGrid.setPrepareCanvas(AValue: TOnPrepareCanvasEvent);
Begin
  fUserPrepareCanvas := AValue;
End;

Procedure TYearGrid.OnPrepareCanvasEvent(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Var
  m, d: Integer;
  ad, am, ay: word;
Begin
  If (aCol <= 0) Or (aRow <= 0) Then exit; // Die Fixed Sachen kommen erst gar net rein
  m := aRow;
  d := aCol;
  If DayExistsInYear(m, d) Then Begin
    // Dem User "Vor" auswählen was es so gibt
    // Markieren der Wochenenden
    If IsWeekendDay(EncodeDate(fYear, m, d)) Then Begin
      Canvas.Brush.Color := WeekendColor;
    End
    Else Begin
      Canvas.Brush.Color := WorkColor;
    End;
    // Heute
    If TodayColor <> clNone Then Begin
      DecodeDate(now, ay, am, ad);
      If (fYear = ay) And (m = am) And (d = ad) Then Begin
        Canvas.Brush.Color := TodayColor;
      End;
    End;
    // Let the user overrule all decisions
    If assigned(fUserPrepareCanvas) Then Begin
      fUserPrepareCanvas(sender, aCol, aRow, aState);
    End;
  End
  Else Begin
    Canvas.Brush.Color := NoValidDateColor;
    Canvas.Pen.Color := NoValidDateColor; // TODO: warum geht das nicht ??
  End;
End;

Function TYearGrid.DayExistsInYear(Month, Day: Integer): Boolean;
Var
  Leap: Boolean;
Begin
  result := false;
  If (Not (Month In [1..12])) Then exit;
  If Day < 1 Then exit;
  leap := IsLeapYear(fYear);
  result := MonthDays[leap][Month] >= day;
End;

Procedure TYearGrid.Click;
Var
  m, d: LongInt;
Begin
  Inherited Click;
  If assigned(OnDayClick) Then Begin
    m := Selection.top;
    d := Selection.Left;
    If DayExistsInYear(m, d) Then Begin
      OnDayClick(self, fYear, m, d);
    End;
  End;
End;

Procedure TYearGrid.DoOnResize;
Var
  i, w, h, aborder: Integer;
Begin
  Inherited DoOnResize;
  If GetParentDesignControl(self) = Nil Then Begin
    aborder := 5;
  End
  Else Begin
    aborder := Scale96ToForm(5);
  End;
  h := ClientHeight - aborder;
  For i := 0 To RowCount - 1 Do Begin
    RowHeights[i] := h Div 13;
  End;
  w := ClientWidth - ColWidths[0] - aborder;
  For i := 1 To ColCount - 1 Do Begin
    ColWidths[i] := w Div 31;
  End;
End;

Procedure TYearGrid.LoadYear(aYear: Integer);
Var
  i: Integer;
Begin
  fYear := aYear;
  RowCount := 13;
  ColCount := 32;
  FixedCols := 1;
  FixedRows := 1;
  // TODO: make this configurable
  Cells[0, 1] := 'Jan';
  Cells[0, 2] := 'Feb';
  Cells[0, 3] := 'Mar';
  Cells[0, 4] := 'Apr';
  Cells[0, 5] := 'May';
  Cells[0, 6] := 'Jun';
  Cells[0, 7] := 'Jul';
  Cells[0, 8] := 'Aug';
  Cells[0, 9] := 'Sep';
  Cells[0, 10] := 'Oct';
  Cells[0, 11] := 'Nov';
  Cells[0, 12] := 'Dec';
  For i := 1 To 31 Do Begin
    Cells[i, 0] := inttostr(i);
  End;
  DoOnResize;
  Invalidate;
End;

End.

