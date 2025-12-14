(******************************************************************************)
(* TColorGrid                                                      14.12.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Reimplementation of the TColorGrid component from Delphi5    *)
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
Unit uColorGrid;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics;

Type

  { TColorGrid }

  TColorGrid = Class(TWinControl)
  private
    fBackgroundIndex: Integer;
    fForegroundIndex: Integer;
    Grids: Array[0..15] Of TBevel;

    Procedure BevelPaint(Sender: TObject);
    Procedure BevelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure BevelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Function GetBackGroundColor: TColor;
    Function GetForeGroundColor: TColor;

    Procedure SetBackGroundColor(AValue: TColor);
    Procedure SetBackgroundIndex(AValue: Integer);
    Procedure SetForeGroundColor(AValue: TColor);
    Procedure SetForegroundIndex(AValue: Integer);

    Procedure UpdateColorCaptions;
  protected
    Procedure DoOnResize; override;

  public
    OnForeGroundColorChange: TNotifyEvent;
    OnBackGroundColorChange: TNotifyEvent;

    Property OnMouseUp;

    Property ForeGroundColor: TColor read GetForeGroundColor write SetForeGroundColor;
    Property BackGroundColor: TColor read GetBackGroundColor write SetBackGroundColor;

    Property ForegroundIndex: Integer read fForegroundIndex write SetForegroundIndex;
    Property BackgroundIndex: Integer read fBackgroundIndex write SetBackgroundIndex;

    Constructor Create(TheOwner: TComponent); override;

    Function ColorToIndex(Const aColor: TColor): Integer;

  End;

  // TODO: Add a LCL Register Function ;)

Implementation

Const
  BevelColors: Array[0..15] Of TColor = (
    clblack, clMaroon, clGreen, clOlive,
    clNavy, clPurple, clTeal, clSilver,
    clGray, clRed, clLime, clYellow,
    clBlue, clFuchsia, clAqua, clWhite
    );

  BevelFontColors: Array[0..15] Of TColor = (
    clWhite, clAqua, clFuchsia, clBlue,
    clYellow, clLime, clRed, clblack,
    clblack, clAqua, clFuchsia, clBlue,
    clWhite, clLime, clRed, clblack
    );

  { TColorGrid }

Constructor TColorGrid.Create(TheOwner: TComponent);
Var
  i: Integer;
Begin
  Inherited Create(TheOwner);
  OnForeGroundColorChange := Nil;
  OnBackGroundColorChange := Nil;
  BorderWidth := 3;
  For i := 0 To 15 Do Begin
    Grids[i] := TBevel.Create(self);
    Grids[i].Name := 'Bevel' + inttostr(i);
    Grids[i].Parent := self;
    Grids[i].OnPaint := @BevelPaint;
    Grids[i].OnMouseDown := @BevelMouseDown;
    Grids[i].OnMouseUp := @BevelMouseUp;
    Grids[i].Caption := '';
    Grids[i].Color := BevelColors[i];
    Grids[i].Font.Color := BevelFontColors[i];
  End;
  fForegroundIndex := 0;
  fBackgroundIndex := 0;
  UpdateColorCaptions();
End;

Procedure TColorGrid.BevelPaint(Sender: TObject);
Var
  b: TBevel;
Begin
  b := TBevel(Sender);

  b.canvas.Brush.Color := b.Color;
  b.canvas.Pen.Color := b.Color;

  b.canvas.Rectangle(
    1, 1,
    b.Width - 1, b.Height - 1
    );

  If b.Caption <> '' Then Begin
    b.canvas.TextOut(
      (b.Width - b.canvas.TextWidth(b.caption)) Div 2,
      (b.Height - b.canvas.TextHeight(b.caption)) Div 2,
      b.Caption
      );
  End;
End;

Procedure TColorGrid.BevelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    SetForeGroundColor(TBevel(Sender).Color);
  End;
  If ssRight In shift Then Begin
    SetBackGroundColor(TBevel(Sender).Color);
  End;
End;

Procedure TColorGrid.BevelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(OnMouseUp) Then Begin
    OnMouseUp(self, Button, Shift, TBevel(sender).left + x, TBevel(sender).Top + y);
  End;
End;

Function TColorGrid.GetBackGroundColor: TColor;
Begin
  If (fBackgroundIndex >= 0) And (fBackgroundIndex <= high(BevelColors)) Then Begin
    result := BevelColors[fBackgroundIndex];
  End
  Else Begin
    result := clNone;
  End;
End;

Function TColorGrid.GetForeGroundColor: TColor;
Begin
  If (fForegroundIndex >= 0) And (fForegroundIndex <= high(BevelColors)) Then Begin
    result := BevelColors[fForegroundIndex];
  End
  Else Begin
    result := clNone;
  End;
End;

Procedure TColorGrid.SetBackGroundColor(AValue: TColor);
Var
  index: Integer;
Begin
  index := ColorToIndex(AValue);
  If (index = -1) And (index <> fBackgroundIndex) Then Exit;
  SetBackgroundIndex(index);
End;

Procedure TColorGrid.SetBackgroundIndex(AValue: Integer);
Begin
  If fBackgroundIndex = AValue Then Exit;
  fBackgroundIndex := AValue;
  UpdateColorCaptions;
  If assigned(OnBackGroundColorChange) Then
    OnBackGroundColorChange(self);
  Invalidate;
End;

Procedure TColorGrid.SetForeGroundColor(AValue: TColor);
Var
  index: Integer;
Begin
  index := ColorToIndex(AValue);
  If (index = -1) And (index <> fForegroundIndex) Then Exit;
  SetForegroundIndex(Index);
End;

Procedure TColorGrid.SetForegroundIndex(AValue: Integer);
Begin
  If fForegroundIndex = AValue Then Exit;
  fForegroundIndex := AValue;
  UpdateColorCaptions;
  If assigned(OnBackGroundColorChange) Then
    OnBackGroundColorChange(self);
  Invalidate;
End;

Function TColorGrid.ColorToIndex(Const aColor: TColor): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(BevelColors) Do Begin
    If aColor = BevelColors[i] Then Begin
      result := i;
      break;
    End;
  End;
End;

Procedure TColorGrid.UpdateColorCaptions;
Var
  i: Integer;
Begin
  For i := 0 To high(Grids) Do Begin
    If i = fForegroundIndex Then Begin
      grids[i].Caption := 'FG';
    End
    Else Begin
      If i = fBackgroundIndex Then Begin
        grids[i].Caption := 'BG';
      End
      Else Begin
        grids[i].Caption := '';
      End;
    End;
  End;
End;

Procedure TColorGrid.DoOnResize;
Var
  w, h, i, tw, th: Integer;
Begin
  Inherited DoOnResize;
  grids[0].Canvas.Font.Size := 1;
  tw := 1;
  th := 1;
  grids[0].Canvas.GetTextSize('VG', tw, th);
  w := (Width - 3 * BorderWidth) Div 4;
  h := (Height - 3 * Borderwidth) Div 4;
  While (tw < W - BorderWidth) And (th < H - BorderWidth) Do Begin
    grids[0].Canvas.Font.Size := grids[0].Canvas.Font.Size + 1;
    grids[0].Canvas.GetTextSize('VG', tw, th);
  End;
  For i := 0 To 15 Do Begin
    Grids[i].Left := (w + BorderWidth) * (i Mod 4);
    Grids[i].Top := (h + BorderWidth) * (i Div 4);
    Grids[i].Width := w;
    Grids[i].Height := h;
    If i <> 0 Then Grids[i].Canvas.Font.Size := Grids[0].Canvas.Font.Size;
  End;
End;

End.

