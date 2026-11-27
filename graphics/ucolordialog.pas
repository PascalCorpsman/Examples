(******************************************************************************)
(* ucolordialog.pas                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
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
(*               0.02 - optional alpha, RGB/HSV synchronization, cancel restore*)
(*                                                                            *)
(******************************************************************************)
Unit ucolordialog;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, types, forms, graphics, controls, extctrls, StdCtrls, spin
  , uvectormath;

Type

  TWheelType = (wtArtists, wtScience);

  { TColorDialog }

  TColorDialog = Class(TForm)
  published
    PaintBox1: TPaintBox; // Anzeigen des Kreisdiagramms
    PaintBox2: TPaintBox; // Anzeigen der aktuellen Farbe
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    SpinEdit1: TspinEdit;
    SpinEdit2: TspinEdit;
    SpinEdit3: TspinEdit;
    SpinEdit4: TspinEdit;
    SpinEdit5: TspinEdit;
    SpinEdit6: TspinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelAlpha: TLabel;
    SpinEditAlpha: TSpinEdit;
    ScrollBarAlpha: TScrollBar;
    Procedure AlphaChange(Sender: TObject);
    Procedure ColorNameEditingDone(Sender: TObject);
    Procedure HSV_Change(Sender: TObject);
    Procedure RGB_Change(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2Paint(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FInWeights: Boolean;
    FInCircle: Boolean;
    FUpdatingControls: Boolean;
    FAlphaEnabled: Boolean;
    FAlpha: Byte;
    FColor: TColor;
    FWheelType: TWheelType;
    FAngle: Single;
    FWeights: TVector3;
    FAngle_old: Single;
    FShowSimplifiedColors: Boolean;
    Gradient: Array Of TColor;
    PaintBox1Buffer: TBitmap;
    Procedure SetAlphaEnabled(AValue: Boolean);
    Procedure SetAlpha(AValue: Byte);
    Procedure SetShowSimplifiedColors(AValue: Boolean);
    Procedure UpdateControls;
    Procedure UpdateColorFromSelection;
    Function Interpol(value: TVector3; c0, c1, c2: TColor): TColor;
    Function GradientColor(Value: Single): TColor;
    Function GetColor: TColor;
    Procedure SetColor(AValue: TColor); reintroduce;
    Procedure SetArtistGradient;
    Procedure SetScienceGradient;
    Procedure SetWheelType(Value: TWheelType);
    Procedure Apply_RGB_Color(r, g, b: Byte);
  public
    AlwaysCenterDialogOnCurrentScreen: Boolean; // Wenn True, wird der Dialog immer auf dem Monitor angezeigt, wo sich der Mauscursor gerade befindet.
    Property ShowSimplifiedColors: Boolean read FShowSimplifiedColors write SetShowSimplifiedColors;
    Property WheelType: TWheelType read FWheelType write SetWheelType; // Wissenschaftliche Farbenbasis, oder "Heutistische" Farbenbasis
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    Property Color: TColor read GetColor write SetColor;
    // Alpha is straight opacity: 0 = transparent, 255 = opaque.
    // Disabling the UI preserves Alpha; callers use opaque color in that mode.
    Property AlphaEnabled: Boolean read FAlphaEnabled write SetAlphaEnabled;
    Property Alpha: Byte read FAlpha write SetAlpha;
    Function Execute: Boolean;
  End;

Implementation

Uses math, FPImage, IntfGraphics, GraphType, ugraphics;

{$R *.lfm}

Const
  rad_120_degree = 2 * pi / 3; // 120° im Bogenmaß

  SelectColorBoxDim = 170; // Die Breite des Farbauswahlbereichs
  SelectColorWheelWidth = 17; // Die Breite des Rings im Farbauswahlbereich

  Artists_ColorWheel: Array[0..12] Of TColor = // Artists Color Wheel (see: ISBN: 978-1-119-99895-2 ( Design for Hackers) , Page 248)
  (
    $0000FF,
    $4000C0,
    $800080,
    $C00040,
    $FF0000,
    $808000,
    $00FF00,
    $00FF80,
    $00FFFF,
    $00C0FF,
    $0080FF,
    $0040FF,
    $0000FF
    );

  Science_ColorWheel: Array[0..16] Of TColor = // Colorwheel developed by the analysis of cons and roods. See : http://blog.asmartbear.com/color-wheels.html
  (
    $0000FF,
    $4000C0, // Nur für die Optik, wird eigentlich nicht benötigt.
    $800080, // Nur für die Optik, wird eigentlich nicht benötigt.
    $C00040, // Nur für die Optik, wird eigentlich nicht benötigt.
    $FF0000,
    $C04000, // Nur für die Optik, wird eigentlich nicht benötigt.
    $808000, // Nur für die Optik, wird eigentlich nicht benötigt.
    $40C000, // Nur für die Optik, wird eigentlich nicht benötigt.
    $00FF00,
    $00FF40, // Nur für die Optik, wird eigentlich nicht benötigt.
    $00FF80, // Nur für die Optik, wird eigentlich nicht benötigt.
    $00FFC0, // Nur für die Optik, wird eigentlich nicht benötigt.
    $00FFFF,
    $00C0FF, // Nur für die Optik, wird eigentlich nicht benötigt.
    $0080FF, // Nur für die Optik, wird eigentlich nicht benötigt.
    $0040FF, // Nur für die Optik, wird eigentlich nicht benötigt.
    $0000FF
    );

Function WeightsInTriangle(P, A, B, C: TVector2): TVector3;
Var
  a0, a1, a2, a3: Double;
Begin
  If PointInTriangle(p, a, b, c) Then Begin
    a0 := CalculateTriangleArea(a, b, c);
    a1 := CalculateTriangleArea(p, b, c);
    a2 := CalculateTriangleArea(a, p, c);
    a3 := CalculateTriangleArea(a, b, p);
    result.x := min(a1 / a0, 1);
    result.y := min(a2 / a0, 1);
    result.z := min(a3 / a0, 1);
  End
  Else Begin
    result.x := 0;
    result.y := 0;
    result.z := 0;
  End;
End;

(*
 * Rechnet
 *
 * R,G,B in [0..255]
 *
 * um in
 *
 * H in [0..360(
 * S, V in [0..100]
 *
 * Vorlage : http: //www.rapidtables.com/convert/color/rgb-to-hsv.htm
 *)

Procedure RGB_To_HSV(R, G, B: Byte; Out H, S, V: Integer);
Var
  tmp, delta, cc_max, cc_min, rr, gg, bb: Double;
Begin
  //
  rr := r / 255;
  gg := g / 255;
  Bb := b / 255;
  cc_max := max(max(rr, gg), bb);
  cc_min := min(min(rr, gg), bb);
  delta := cc_max - cc_min;
  If delta = 0 Then Begin
    h := 0;
    s := 0;
  End
  Else Begin
    If cc_max = rr Then Begin
      tmp := (gg - bb) / delta;
      While tmp >= 6 Do
        tmp := tmp - 6;
      h := round(60 * tmp);
    End
    Else Begin
      If cc_max = gg Then Begin
        h := round(60 * ((bb - rr) / delta + 2));
      End
      Else Begin
        h := round(60 * ((rr - gg) / delta + 4));
      End;
    End;
    s := round((delta / cc_max) * 100);
    h := (h Mod 360 + 360) Mod 360;
    s := min(100, max(0, s));
  End;
  v := round(cc_max * 100);
  v := min(100, max(0, v));
End;

(*
 * Rechnet
 *
 * H in [0..360(
 * S, V in [0..100]
 *
 * um in
 *
 * R,G,B in [0..255]
 *
 * Vorlage : http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
 *)

Procedure HSV_to_RGB(Const H, S, V: integer; Out R, G, B: Byte);
Var
  h_i: Integer;
  ss, vv, rr, gg, bb, f: Double;
  p: Double;
  q: Double;
  t: Double;
Begin
  h_i := (H Mod 360 + 360) Mod 360;
  f := Frac(h_i / 60);
  h_i := h_i Div 60;
  ss := s / 100;
  vv := v / 100;
  p := Vv * (1 - sS);
  q := Vv * (1 - sS * f);
  t := Vv * (1 - sS * (1 - f));
  Case h_i Mod 6 Of
    0: Begin
        rR := Vv;
        gG := t;
        bB := p;
      End;
    1: Begin
        rR := q;
        gG := Vv;
        bB := p;
      End;
    2: Begin
        rR := p;
        gG := Vv;
        bB := t;
      End;
    3: Begin
        rR := p;
        gG := q;
        bB := Vv;
      End;
    4: Begin
        rR := t;
        gG := p;
        bB := Vv;
      End;
    5: Begin
        rR := Vv;
        gG := p;
        bB := q;
      End;
  End;
  r := min(255, max(0, round(rr * 255)));
  g := min(255, max(0, round(gg * 255)));
  b := min(255, max(0, round(bb * 255)));
End;

Function PointInRect(P: TPoint; {classes.} R: TRect): boolean;
Var
  t: Integer;
Begin
  If r.left > r.right Then Begin
    t := r.left;
    r.left := r.right;
    r.right := t;
  End;
  If r.top > r.bottom Then Begin
    t := r.Bottom;
    r.bottom := r.top;
    r.top := t;
  End;
  result := (r.left <= p.x) And (r.right >= p.x) And
    (r.top <= p.y) And (r.bottom >= p.y);
End;

{ TColorDialog }

Function TColorDialog.GetColor: TColor;
Begin
  result := FColor;
End;

Procedure TColorDialog.SetColor(AValue: TColor);
Var
  r, g, b: Byte;
Begin
  RedGreenBlue(Graphics.ColorToRGB(avalue), r, g, b);
  Apply_RGB_Color(r, g, b);
End;

Function TColorDialog.Interpol(value: TVector3; c0, c1, c2: TColor): TColor;
Var
  r, g, b: Array[0..3] Of Byte;
Begin
  RedGreenBlue(c0, r[0], g[0], b[0]);
  RedGreenBlue(c1, r[1], g[1], b[1]);
  RedGreenBlue(c2, r[2], g[2], b[2]);
  result := Graphics.RGBToColor(
    round(value.x * r[0] + value.y * r[1] + value.z * r[2]),
    round(value.x * g[0] + value.y * g[1] + value.z * g[2]),
    round(value.x * b[0] + value.y * b[1] + value.z * b[2])
    );
End;

Function TColorDialog.GradientColor(Value: Single): TColor;
  Function interpol_(v: single; c1, c2: TColor): TColor;
  Var
    R1, r2, g1, g2, b1, b2: Byte;
    vm1: Single;
  Begin
    RedGreenBlue(c1, r1, g1, b1);
    RedGreenBlue(c2, r2, g2, b2);
    vm1 := 1 - v;
    result := Graphics.RGBToColor(round(vm1 * r1 + v * r2), round(vm1 * g1 + v * g2), round(vm1 * b1 + v * b2));
  End;
Var
  i: integer;
Begin
  // Wrap the seam, including 2*pi rounded to Single, before indexing.
  value := value - Floor(value);
  value := value * (length(Gradient) - 1);
  i := min(trunc(value), length(Gradient) - 2);
  value := value - i;
  result := interpol_(value, Gradient[i], Gradient[i + 1]);
End;

Procedure TColorDialog.PaintBox1Paint(Sender: TObject);
Var
  x0, y0: integer;
  S, c: Array[0..2] Of {$IFDEF Win32 }Double{$ELSE}Extended{$ENDIF};

  Procedure RenderTriangleColored(c0, c1, c2: TColor);
  Var
    dimx, dimy: TPoint;
    i, j: Integer;
    t: TVector3;
  Begin
    dimx.x := trunc(c[0]);
    dimx.y := ceil(c[0]);
    dimy.x := trunc(s[0]);
    dimy.y := ceil(s[0]);
    For i := 1 To 2 Do Begin
      dimx.x := min(dimx.x, trunc(c[i]));
      dimx.y := max(dimx.y, ceil(c[i]));
      dimy.x := min(dimy.x, trunc(s[i]));
      dimy.y := max(dimy.y, ceil(s[i]));
    End;
    For i := dimx.x To dimx.y Do Begin
      For j := dimy.x To dimy.y Do Begin
        If PointInTriangle(v2(-i, -j), v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2])) Then Begin
          t := WeightsInTriangle(v2(-i, -j), v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
          PaintBox1Buffer.canvas.pixels[x0 - i, y0 - j] := Interpol(t, c0, c1, c2);
        End;
      End;
    End;
  End;

  Function GradientColor2(Value: Single): TColor;
  Var
    step: Single;
    i: integer;
  Begin
    step := 1 / (length(Gradient) - 1);
    i := round(value / step);
    result := Gradient[i];
  End;

Var
  i, j: integer;
  R1, R2: integer;
  ss, cc, r: Double;
Begin
  // Das Auswahldreieck
  sincos(fAngle, s[0], c[0]);
  sincos(fAngle + rad_120_degree, s[1], c[1]);
  sincos(fAngle - rad_120_degree, s[2], c[2]);
  x0 := SelectColorBoxDim Div 2;
  y0 := SelectColorBoxDim Div 2;
  r2 := SelectColorBoxDim Div 2 - 1;
  ss := s[0] * r2;
  cc := c[0] * r2;
  For i := 0 To 2 Do Begin
    s[i] := s[i] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
    c[i] := c[i] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
  End;
  If (FAngle <> FAngle_old) Then Begin // Den Puffer nur bei Bedarf neu Rendern
    FAngle_old := FAngle;
    // Hintergrund
    PaintBox1Buffer.canvas.Brush.Color := clBtnFace;
    PaintBox1Buffer.canvas.Brush.Style := bsSolid;
    PaintBox1Buffer.Canvas.FillRect(0, 0, SelectColorBoxDim, SelectColorBoxDim);
    // Der Ring
    r1 := SelectColorBoxDim Div 2 - SelectColorWheelWidth - 1;
    For i := -r2 To r2 Do Begin
      For j := -r2 To r2 Do Begin
        // Liegt der Punkt Im Kreis Ring ?
        If (sqr(r1) <= sqr(i) + sqr(j)) And (sqr(r2) >= sqr(i) + sqr(j)) Then Begin
          r := arctan2(j, i) + pi; // Winkel in [0.. 2* pi(
          r := r / (2 * pi); // Winkel in [0..1(
          If ShowSimplifiedColors Then Begin
            If (sqr((r2 + r1) / 2) >= sqr(i) + sqr(j)) Then Begin
              PaintBox1Buffer.canvas.Pixels[x0 + i, y0 + j] := GradientColor2(r);
            End
            Else Begin
              PaintBox1Buffer.canvas.Pixels[x0 + i, y0 + j] := GradientColor(r);
            End;
          End
          Else Begin
            PaintBox1Buffer.canvas.Pixels[x0 + i, y0 + j] := GradientColor(r);
          End;
        End;
      End;
    End;
    // Das Farbdreieck
    RenderTriangleColored(GradientColor(fAngle / (2 * pi)), clblack, clwhite);
    // Anzeige des Spekrtums
    PaintBox1Buffer.canvas.Pen.Color := clwhite;
    PaintBox1Buffer.canvas.Line(
      round(x0 - c[0]),
      round(y0 - s[0]),
      round(x0 - cc),
      round(y0 - ss)
      );
    PaintBox1Buffer.Canvas.Brush.Color := clBlack;
    PaintBox1Buffer.Canvas.FrameRect(0, 0, SelectColorBoxDim, SelectColorBoxDim);
  End;
  // Alles Raus
  PaintBox1.Canvas.Draw(0, 0, PaintBox1Buffer);
  // Anzeige des "Gewichtes"
  i := -round(FWeights.x * c[0] + FWeights.y * c[1] + FWeights.z * c[2]);
  j := -round(FWeights.x * s[0] + FWeights.y * s[1] + FWeights.z * s[2]);
  PaintBox1.canvas.Pen.Color := clwhite;
  PaintBox1.canvas.Brush.Style := bsClear;
  PaintBox1.canvas.Ellipse(x0 + i - 3, y0 + j - 3, x0 + i + 3, y0 + j + 3);
End;

Procedure TColorDialog.PaintBox2Paint(Sender: TObject);
Var
  Checker, Foreground, Preview: TBitmap;
  Layers: Array[0..1] Of TBitmapLayer;
  Image: TLazIntfImage;
  LightTile, DarkTile, Pixel: TFPColor;
  x, y: Integer;
Begin
  PaintBox2.Canvas.Brush.Style := bsSolid;
  If FAlphaEnabled Then Begin
    Checker := Nil;
    Foreground := Nil;
    Preview := Nil;
    Try
      Checker := TBitmap.Create;
      Foreground := TBitmap.Create;
      Preview := TBitmap.Create;
      Preview.SetSize(PaintBox2.Width, PaintBox2.Height);
      Image := TLazIntfImage.Create(Preview.Width, Preview.Height, [riqfRGB, riqfAlpha]);
      Try
        LightTile := ColorToFPColor(clWhite);
        DarkTile := ColorToFPColor(clSilver);
        For y := 0 To Image.Height - 1 Do
          For x := 0 To Image.Width - 1 Do
            If ((x Div 8 + y Div 8) Mod 2) = 0 Then
              Image.Colors[x, y] := LightTile
            Else
              Image.Colors[x, y] := DarkTile;
        Checker.LoadFromIntfImage(Image);

        Pixel := ColorToFPColor(Graphics.ColorToRGB(FColor));
        Pixel.Alpha := FAlpha * 257;
        Image.FillPixels(Pixel);
        Foreground.LoadFromIntfImage(Image);
      Finally
        Image.Free;
      End;
      Layers[0].Bitmap := Checker;
      Layers[0].Position := Point(0, 0);
      Layers[1].Bitmap := Foreground;
      Layers[1].Position := Point(0, 0);
      ComposeBitmapLayers(Preview, clWhite, Layers);
      PaintBox2.Canvas.Draw(0, 0, Preview);
    Finally
      Preview.Free;
      Foreground.Free;
      Checker.Free;
    End;
    PaintBox2.Canvas.Brush.Style := bsClear;
  End
  Else
    PaintBox2.Canvas.Brush.Color := FColor;
  PaintBox2.Canvas.Pen.Color := clBlack;
  PaintBox2.Canvas.Rectangle(0, 0, PaintBox2.Width, PaintBox2.Height);
End;

Procedure TColorDialog.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  r1, r2, i, j, k: integer;
  S, c: Array[0..2] Of {$IFDEF Win32 }Double{$ELSE}Extended{$ENDIF};
Begin
  If Button <> mbLeft Then Exit;
  i := x - SelectColorBoxDim Div 2;
  j := y - SelectColorBoxDim Div 2;
  r1 := SelectColorBoxDim Div 2 - SelectColorWheelWidth;
  r2 := SelectColorBoxDim Div 2 - 1;
  // Liegt der Punkt Im Kreis Ring ?
  FInCircle := (sqr(r1) <= sqr(i) + sqr(j)) And (sqr(r2) >= sqr(i) + sqr(j));
  // Liegt der Punkt im Anzeige Dreieck ?
  sincos(fAngle, s[0], c[0]);
  sincos(fAngle + rad_120_degree, s[1], c[1]);
  sincos(fAngle - rad_120_degree, s[2], c[2]);
  For k := 0 To 2 Do Begin
    s[k] := s[k] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
    c[k] := c[k] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
  End;
  FInWeights := PointInTriangle(v2(i, j), v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
  PaintBox1MouseMove(sender, shift, x, y);
End;

Procedure TColorDialog.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Var
  i, j, k: integer;
  S, c: Array[0..2] Of {$IFDEF Win32 }Double{$ELSE}Extended{$ENDIF};
  p, a, b, cc, m: TVector2;
Begin
  If ssleft In shift Then Begin
    i := x - SelectColorBoxDim Div 2;
    j := y - SelectColorBoxDim Div 2;
    // Liegt der Punkt Im Kreis Ring ?
    If FInCircle Then Begin
      FAngle := arctan2(j, i) + pi;
      UpdateColorFromSelection;
    End;
    // Liegt der Punkt im Anzeige Dreieck ?
    If FInWeights Then Begin
      sincos(fAngle, s[0], c[0]);
      sincos(fAngle + rad_120_degree, s[1], c[1]);
      sincos(fAngle - rad_120_degree, s[2], c[2]);
      For k := 0 To 2 Do Begin
        s[k] := s[k] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
        c[k] := c[k] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
      End;
      If PointInTriangle(v2(i, j), v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2])) Then Begin
        FWeights := WeightsInTriangle(v2(i, j), v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
      End
      Else Begin
        // Todo : Wenn der Punkt nicht im Dreieck Liegt, muss er auf die Passende Kante Projiziert werden !!
        //        Das hier geht zwar schon ist aber nicht optimal / könnte noch besser sein.
        a := v2(-c[0], -s[0]);
        b := v2(-c[1], -s[1]);
        cc := v2(-c[2], -s[2]);
        m := v2(0, 0); //1 / 3 * (a + b + cc);
        p := v2(i, j);
        If IntersectLine_segments(m, p, a, b, p) Then Begin
          p := 0.9999 * p; // Ein Klein Wenig ins Dreieck Rein Hohlen sonst klappt die Gewichtsberechnung nicht
          FWeights := WeightsInTriangle(p, v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
        End;
        If IntersectLine_segments(m, p, a, Cc, p) Then Begin
          p := 0.9999 * p; // Ein Klein Wenig ins Dreieck Rein Hohlen sonst klappt die Gewichtsberechnung nicht
          FWeights := WeightsInTriangle(p, v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
        End;
        If IntersectLine_segments(m, p, cc, b, p) Then Begin
          p := 0.9999 * p; // Ein Klein Wenig ins Dreieck Rein Hohlen sonst klappt die Gewichtsberechnung nicht
          FWeights := WeightsInTriangle(p, v2(-c[0], -s[0]), v2(-c[1], -s[1]), v2(-c[2], -s[2]));
        End;
      End;
      UpdateColorFromSelection;
    End;
  End;
End;

Procedure TColorDialog.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  FInWeights := false;
  FInCircle := false;
End;

Procedure TColorDialog.SetArtistGradient;
Var
  i: Integer;
Begin
  setlength(Gradient, length(Artists_ColorWheel));
  For i := low(Artists_ColorWheel) To high(Artists_ColorWheel) Do Begin
    Gradient[i] := Artists_ColorWheel[i];
  End;
End;

Procedure TColorDialog.SetScienceGradient;
Var
  i: Integer;
Begin
  setlength(Gradient, length(Science_ColorWheel));
  For i := low(Science_ColorWheel) To high(Science_ColorWheel) Do Begin
    Gradient[i] := Science_ColorWheel[i];
  End;
End;

Procedure TColorDialog.SetWheelType(Value: TWheelType);
Var
  r, g, b: Byte;
Begin
  If Value <> FWheelType Then Begin
    Case value Of
      wtArtists: SetArtistGradient;
      wtScience: SetScienceGradient;
    End;
    FWheelType := value;
    RedGreenBlue(FColor, r, g, b);
    Apply_RGB_Color(r, g, b);
  End;
End;

Procedure TColorDialog.Apply_RGB_Color(r, g, b: Byte);
Var
  Target, c0, c1, Direction, Candidate: TVector3;
  LowChannel, HighChannel, Span0, Span1, Delta, t, RawT: Double;
  Error, BestError, Denominator, ColorWeight: Double;
  i: Integer;
  cr, cg, cb: Byte;
Begin
  // Keep exact RGB independently: some artist-wheel hues have limited brightness.
  FColor := Graphics.RGBToColor(r, g, b);
  LowChannel := min(r, min(g, b));
  HighChannel := max(r, max(g, b));
  Delta := HighChannel - LowChannel;
  If Delta = 0 Then
    FWeights := v3(0, 1 - HighChannel / 255, LowChannel / 255)
  Else Begin
    Target := v3((r - LowChannel) / Delta,
      (g - LowChannel) / Delta, (b - LowChannel) / Delta);
    BestError := MaxDouble;
    For i := 0 To length(Gradient) - 2 Do Begin
      RedGreenBlue(Gradient[i], cr, cg, cb);
      Span0 := max(cr, max(cg, cb));
      c0 := v3(cr / Span0, cg / Span0, cb / Span0);
      RedGreenBlue(Gradient[i + 1], cr, cg, cb);
      Span1 := max(cr, max(cg, cb));
      c1 := v3(cr / Span1, cg / Span1, cb / Span1);
      Direction := c1 - c0;
      Denominator := DotV3(Direction, Direction);
      If Denominator = 0 Then t := 0
      Else t := max(0, min(1, DotV3(Target - c0, Direction) / Denominator));
      Candidate := c0 + t * Direction;
      Error := DotV3(Target - Candidate, Target - Candidate);
      If Error < BestError Then Begin
        BestError := Error;
        // Convert interpolation between normalized endpoints back to the raw ring.
        RawT := t * Span0 / (Span1 * (1 - t) + t * Span0);
        FAngle := 2 * pi * (i + RawT) / (length(Gradient) - 1);
      End;
    End;
    RedGreenBlue(GradientColor(FAngle / (2 * pi)), cr, cg, cb);
    ColorWeight := min(1 - LowChannel / 255, Delta / max(cr, max(cg, cb)));
    FWeights := v3(ColorWeight, max(0, 1 - LowChannel / 255 - ColorWeight),
      LowChannel / 255);
  End;
  FAngle_old := FAngle + 1;
  UpdateControls;
  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
End;

Procedure TColorDialog.UpdateColorFromSelection;
Begin
  FColor := Interpol(FWeights, GradientColor(FAngle / (2 * pi)), clBlack, clWhite);
  UpdateControls;
  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
End;

Procedure TColorDialog.UpdateControls;
Var
  r, g, b: Byte;
  h, s, v: Integer;
Begin
  If FUpdatingControls Then Exit;
  FUpdatingControls := True;
  Try
    RedGreenBlue(FColor, r, g, b);
    RGB_To_HSV(r, g, b, h, s, v);
    SpinEdit1.Value := h;
    SpinEdit2.Value := s;
    SpinEdit3.Value := v;
    SpinEdit4.Value := r;
    SpinEdit5.Value := g;
    SpinEdit6.Value := b;
    SpinEditAlpha.Value := FAlpha;
    ScrollBarAlpha.Position := FAlpha;
    If FAlphaEnabled Then
      Edit1.Text := format('#%.2X%.2X%.2X%.2X', [FAlpha, r, g, b])
    Else
      Edit1.Text := format('#%.2X%.2X%.2X', [r, g, b]);
  Finally
    FUpdatingControls := False;
  End;
End;

Procedure TColorDialog.SetAlphaEnabled(AValue: Boolean);
Begin
  FAlphaEnabled := AValue;
  LabelAlpha.Visible := AValue;
  SpinEditAlpha.Visible := AValue;
  ScrollBarAlpha.Visible := AValue;
  If AValue Then Edit1.MaxLength := 9 Else Edit1.MaxLength := 7;
  UpdateControls;
  PaintBox2.Invalidate;
End;

Procedure TColorDialog.SetAlpha(AValue: Byte);
Begin
  FAlpha := AValue;
  UpdateControls;
  PaintBox2.Invalidate;
End;

Procedure TColorDialog.SetShowSimplifiedColors(AValue: Boolean);
Begin
  FShowSimplifiedColors := AValue;
  FAngle_old := FAngle + 1;
  PaintBox1.Invalidate;
End;

Procedure TColorDialog.AlphaChange(Sender: TObject);
Begin
  If FUpdatingControls Or Not FAlphaEnabled Then Exit;
  If Sender = ScrollBarAlpha Then SetAlpha(ScrollBarAlpha.Position)
  Else SetAlpha(SpinEditAlpha.Value);
End;

Procedure TColorDialog.ColorNameEditingDone(Sender: TObject);
Var
  ColorText: String;
  Value: LongWord;
Begin
  If FUpdatingControls Then Exit;
  ColorText := Trim(Edit1.Text);
  If Not ((length(ColorText) = 7) Or
    (FAlphaEnabled And (length(ColorText) = 9))) Then Begin
    UpdateControls;
    Exit;
  End;
  If (ColorText[1] = '#') And
    TryStrToDWord('$' + Copy(ColorText, 2, length(ColorText)), Value) Then Begin
    If length(ColorText) = 9 Then FAlpha := Value Shr 24;
    Apply_RGB_Color((Value Shr 16) And $FF, (Value Shr 8) And $FF, Value And $FF);
  End
  Else
    UpdateControls;
End;

Procedure TColorDialog.HSV_Change(Sender: TObject);
Var
  r, g, b: Byte;
Begin
  If FUpdatingControls Then Exit;
  // Jemand hat die HSV-Werte Geändert
  HSV_to_RGB(SpinEdit1.Value, SpinEdit2.Value, SpinEdit3.Value, r, g, b);
  Apply_RGB_Color(r, g, b);
End;

Procedure TColorDialog.RGB_Change(Sender: TObject);
Begin
  If FUpdatingControls Then Exit;
  // Jemand hat die RGB-Werte Geändert
  Apply_RGB_Color(SpinEdit4.Value, SpinEdit5.Value, SpinEdit6.Value);
End;

Constructor TColorDialog.Create(TheOwner: TComponent);
Begin
  // Suppress OnChange while the static controls are streamed from the resource.
  FUpdatingControls := True;
  Inherited Create(TheOwner);
  AlwaysCenterDialogOnCurrentScreen := true;
  FInWeights := false;
  FInCircle := false;
  FAlpha := 255;
  FColor := clBlack;
  left := (Screen.Width - self.Width) Div 2;
  top := (Screen.Height - self.Height) Div 2;
  PaintBox1Buffer := TBitmap.create;
  PaintBox1Buffer.width := SelectColorBoxDim;
  PaintBox1Buffer.Height := SelectColorBoxDim;
  FWheelType := wtArtists;
  SetArtistGradient;
  FAngle := 0;
  FAngle_old := 1;
  FWeights := v3(0, 1, 0);
  FUpdatingControls := False;
  SetAlphaEnabled(False);
End;

Destructor TColorDialog.Destroy;
Begin
  If Assigned(PaintBox1) Then PaintBox1.OnPaint := Nil;
  PaintBox1Buffer.free;
  Inherited Destroy;
End;

Function TColorDialog.Execute: Boolean;
Var
  i: Integer;
  r: TRect;
  SavedColor: TColor;
  SavedAlpha: Byte;
  SavedAngle: Single;
  SavedWeights: TVector3;
Begin
  SavedColor := FColor;
  SavedAlpha := FAlpha;
  SavedAngle := FAngle;
  SavedWeights := FWeights;
  FInWeights := False;
  FInCircle := False;
  UpdateControls;
  FAngle_old := FAngle - 1; // Erzwingen, dass ein Repaint gemacht wird.
  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
  ModalResult := mrnone;
  If AlwaysCenterDialogOnCurrentScreen Then Begin
    If screen.MonitorCount <> 1 Then Begin
      For i := 0 To screen.MonitorCount - 1 Do Begin
        r := screen.Monitors[i].BoundsRect;
        If PointInRect(Mouse.CursorPos, r) Then Begin
          left := (screen.Monitors[i].width - self.width) Div 2 + screen.Monitors[i].BoundsRect.left;
          top := (screen.Monitors[i].height - self.height) Div 2 + screen.Monitors[i].BoundsRect.top;
          break;
        End;
      End;
    End
    Else Begin
      left := (screen.width - self.width) Div 2;
      top := (screen.height - self.height) Div 2;
    End;
  End;
  result := False;
  Try
    result := ShowModal = mrok;
  Finally
    FInWeights := False;
    FInCircle := False;
    If Not result Then Begin
      FColor := SavedColor;
      FAlpha := SavedAlpha;
      FAngle := SavedAngle;
      FWeights := SavedWeights;
      FAngle_old := FAngle + 1;
      UpdateControls;
      PaintBox1.Invalidate;
      PaintBox2.Invalidate;
    End;
  End;
End;

End.

