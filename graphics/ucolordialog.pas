(******************************************************************************)
(* ucolordialog.pas                                                ??.??.???? *)
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
  private
    FInWeights: Boolean;
    FInCircle: BOolean;
    FWheelType: TWheelType;
    FAngle: Single;
    FWeights: TVector3;
    FAngle_old: Single; // Der Alte Winkel, spart Rechenzeit wenn sich der Cursor nur im Dreieck bewegt.
    Gradient: Array Of TColor;
    PaintBox1Buffer: TBitmap;
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
    Function Interpol(value: TVector3; c0, c1, c2: TColor): TColor;
    Function GradientColor(Value: Single): TColor;
    Function GetColor: TColor;
    Procedure SetColor(AValue: TColor); reintroduce;
    Procedure SetArtistGradient;
    Procedure SetScienceGradient;
    Procedure SetWheelType(Value: TWheelType);
    Procedure Apply_RGB_Color(r, g, b: Byte);
    Procedure HSV_Change(Sender: TObject);
    Procedure RGB_Change(Sender: TObject);
    (*
     * Events für LCL-Componenten
     *)
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2Paint(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    ShowSimplifiedColors: Boolean; // Zeigt im Farbring die nicht Interpolierten Farben mit an
    AlwaysCenterDialogOnCurrentScreen: Boolean; // Wenn True, wird der Dialog immer auf dem Monitor angezeigt, wo sich der Mauscursor gerade befindet.
    Property WheelType: TWheelType read FWheelType write SetWheelType; // Wissenschaftliche Farbenbasis, oder "Heutistische" Farbenbasis
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    Property Color: TColor read GetColor write SetColor;
    Function Execute: Boolean;
  End;

Implementation

Uses math;

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
    h := h Mod 360;
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
  h_i := Trunc(H / 60);
  f := Frac(H / 60);
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
Var
  c: TColor;
Begin
  c := GradientColor(fAngle / (2 * pi));
  result := Interpol(FWeights, c, clblack, clwhite);
End;

Procedure TColorDialog.SetColor(AValue: TColor);
Var
  r, g, b: Byte;
Begin
  // Besser wäre hier der Code aus RGB_Change
  RedGreenBlue(avalue, r, g, b);
  Apply_RGB_Color(r, g, b);
End;

Function TColorDialog.Interpol(value: TVector3; c0, c1, c2: TColor): TColor;
Var
  r, g, b: Array[0..3] Of Byte;
Begin
  RedGreenBlue(c0, r[0], g[0], b[0]);
  RedGreenBlue(c1, r[1], g[1], b[1]);
  RedGreenBlue(c2, r[2], g[2], b[2]);
  result := RGBToColor(
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
    result := RGBToColor(round(vm1 * r1 + v * r2), round(vm1 * g1 + v * g2), round(vm1 * b1 + v * b2));
  End;
Var
  i: integer;
Begin
  value := value * (length(Gradient) - 1);
  i := trunc(value);
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
  If (FAngle <> FAngle_old) Then Begin // Den Puffer nur bei Bedarf neu Rendern
    FAngle_old := FAngle;
    // Hintergrund
    PaintBox1Buffer.canvas.Brush.Color := clBtnFace;
    PaintBox1Buffer.canvas.Brush.Style := bsSolid;
    PaintBox1Buffer.canvas.Pen.Color := clblack;
    PaintBox1Buffer.canvas.Pen.Style := psDot;
    PaintBox1Buffer.Canvas.Rectangle(0, 0, SelectColorBoxDim, SelectColorBoxDim);
    PaintBox1Buffer.canvas.Pen.Style := psSolid;
    // Der Ring
    r1 := SelectColorBoxDim Div 2 - SelectColorWheelWidth - 1;
    r2 := SelectColorBoxDim Div 2 - 1;
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
    ss := s[0] * (((SelectColorBoxDim - 1) / 2));
    cc := c[0] * (((SelectColorBoxDim - 1) / 2));
    For i := 0 To 2 Do Begin
      s[i] := s[i] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
      c[i] := c[i] * ((SelectColorBoxDim / 2) - 1 - SelectColorWheelWidth);
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
  End;
  // Alles Raus
  PaintBox1.Canvas.Draw(0, 0, PaintBox1Buffer);
  // Anzeige des "Gewichtes"
  i := -round(FWeights.x * c[0] + FWeights.y * c[1] + FWeights.z * c[2]);
  j := -round(FWeights.x * s[0] + FWeights.y * s[1] + FWeights.z * s[2]);
  PaintBox1.canvas.Pen.Color := clwhite;
  PaintBox1.canvas.Brush.Style := bsClear;
  PaintBox1.canvas.Ellipse(x0 + i - 3, y0 + j - 3, x0 + i + 3, y0 + j + 3);
  // Die Varbsachen noch Aktualisieren
  PaintBox2Paint(self);
End;

Procedure TColorDialog.PaintBox2Paint(Sender: TObject);
Var
  c, cc: TColor;
  a, r, g, b: byte;
  h, s, v: integer;
Begin
  c := GradientColor(fAngle / (2 * pi));
  cc := Interpol(FWeights, c, clblack, clwhite);
  PaintBox2.Canvas.Brush.Color := cc;
  PaintBox2.Canvas.Pen.Color := clBlack;
  PaintBox2.Canvas.Rectangle(0, 0, PaintBox2.Width, PaintBox2.Height);
  RedGreenBlue(cc, r, g, b);
  // Anzeige an den Kontrollelementen
  a := 0; // Alpha Kanal
  edit1.text := format('#%0.2X%0.2X%0.2X%0.2X', [a, r, g, b]);
  RGB_To_HSV(r, g, b, h, s, v);
  SpinEdit1.Value := h;
  SpinEdit2.Value := s;
  SpinEdit3.Value := v;
  SpinEdit4.Value := r;
  SpinEdit5.Value := g;
  SpinEdit6.Value := b;
End;

Procedure TColorDialog.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  r1, r2, i, j, k: integer;
  S, c: Array[0..2] Of {$IFDEF Win32 }Double{$ELSE}Extended{$ENDIF};
Begin
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
      PaintBox1Paint(Nil);
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
      PaintBox1Paint(Nil);
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
Begin
  If Value <> FWheelType Then Begin
    Case value Of
      wtArtists: SetArtistGradient;
      wtScience: SetScienceGradient;
    End;
    FWheelType := value;
  End;
End;

Procedure TColorDialog.Apply_RGB_Color(r, g, b: Byte);
Begin
  // Todo : Konvertierung von RGB nach Weight / Angle
  FAngle_old := FAngle + 1; // Erzwingen, dass das Bild für Paintbox1 neu erzeugt wird.
  Paintbox1.Invalidate;
  //Paintbox2.Invalidate; --\  So werden die SpinEdits in "echtzeit" aktualisiert
  //                        v
  Paintbox2.OnPaint(Nil); // Setze alle SpinEdits..
End;

Procedure TColorDialog.HSV_Change(Sender: TObject);
Var
  r, g, b: Byte;
Begin
  // Jemand hat die HSV-Werte Geändert
  HSV_to_RGB(SpinEdit4.Value, SpinEdit5.Value, SpinEdit6.Value, r, g, b);
  Apply_RGB_Color(r, g, b);
End;

Procedure TColorDialog.RGB_Change(Sender: TObject);
Begin
  // Jemand hat die RGB-Werte Geändert
  Apply_RGB_Color(SpinEdit4.Value, SpinEdit5.Value, SpinEdit6.Value);
End;

Constructor TColorDialog.Create(TheOwner: TComponent);

  Function SpinCreate(l, t: integer): TSpinEdit;
  Begin
    result := TspinEdit.create(self);
    result.Parent := self;
    result.left := l;
    result.top := t;
  End;

  Function LCreate(l, t: integer; c: String): TLabel;
  Begin
    result := TLabel.create(self);
    result.parent := self;
    result.left := l;
    result.Top := t;
    result.Caption := c;
  End;

Begin
  CreateNew(TheOwner);
  AlwaysCenterDialogOnCurrentScreen := true;
  FInWeights := false;
  FInCircle := false;
  ShowSimplifiedColors := false;
  caption := 'Select color';
  self.width := 490;
  self.height := 290;
  left := (Screen.Width - self.Width) Div 2;
  top := (Screen.Height - self.Height) Div 2;
  BorderIcons := [biSystemMenu];
  PaintBox1 := TPaintBox.Create(self);
  PaintBox1.Parent := Self;
  PaintBox1.Width := SelectColorBoxDim;
  PaintBox1.Height := SelectColorBoxDim;
  PaintBox1.Left := 10;
  PaintBox1.Top := 10;
  PaintBox1Buffer := TBitmap.create;
  PaintBox1Buffer.width := SelectColorBoxDim;
  PaintBox1Buffer.Height := SelectColorBoxDim;
  PaintBox1.OnPaint := @PaintBox1Paint;
  PaintBox1.OnMouseDown := @PaintBox1MouseDown;
  PaintBox1.OnMouseUp := @PaintBox1MouseUp;
  PaintBox1.OnMouseMove := @PaintBox1MouseMove;
  // Init des Gradienten
  FWheelType := wtScience;
  SetWheelType(wtArtists);
  FAngle := 0;
  FAngle_old := 1; // Hauptsache Anders
  FWeights := v3(0, 1, 0);
  PaintBox2 := TPaintBox.Create(self);
  PaintBox2.Parent := self;
  PaintBox2.Height := 40;
  PaintBox2.Width := SelectColorBoxDim;
  PaintBox2.Left := 10;
  PaintBox2.Top := PaintBox1.Top + SelectColorBoxDim + 10;
  PaintBox2.OnPaint := @PaintBox2Paint;
  edit1 := tedit.create(self);
  edit1.parent := self;
  edit1.left := 272;
  edit1.Top := 152;
  Button1 := TButton.Create(self);
  button1.parent := self;
  button1.left := 320;
  button1.Top := 248;
  button1.Caption := 'Cancel';
  button1.ModalResult := mrCancel;
  Button2 := TButton.Create(self);
  button2.parent := self;
  button2.left := 405;
  button2.Top := 248;
  button2.Caption := 'OK';
  button2.ModalResult := mrOK;
  // HSV
  SpinEdit1 := SpinCreate(272, 23);
  SpinEdit1.MaxValue := 360;
  SpinEdit1.OnChange := @HSV_Change;
  SpinEdit2 := SpinCreate(272, 56);
  SpinEdit2.MaxValue := 100;
  SpinEdit2.OnChange := @HSV_Change;
  SpinEdit3 := SpinCreate(272, 88);
  SpinEdit2.MaxValue := 100;
  SpinEdit3.OnChange := @HSV_Change;
  // RGB
  SpinEdit4 := SpinCreate(430, 23);
  SpinEdit4.MaxValue := 255;
  SpinEdit4.OnChange := @RGB_Change;
  SpinEdit5 := SpinCreate(430, 56);
  SpinEdit5.MaxValue := 255;
  SpinEdit5.OnChange := @RGB_Change;
  SpinEdit6 := SpinCreate(430, 88);
  SpinEdit6.MaxValue := 255;
  SpinEdit6.OnChange := @RGB_Change;
  Label1 := LCreate(194, 31, 'Hue:');
  Label2 := LCreate(194, 64, 'Saturation:');
  Label3 := LCreate(194, 96, 'Value:');
  Label4 := LCreate(352, 31, 'Red:');
  Label5 := LCreate(352, 64, 'Green:');
  Label6 := LCreate(352, 96, 'Blue:');
  Label7 := LCreate(194, 160, 'Color name:');
{$WARNING Es fehlt noch der Scrollbar fuer die Opazitaet}
End;

Destructor TColorDialog.Destroy;
Begin
  PaintBox1.OnPaint := Nil;
  PaintBox1Buffer.free;
  Inherited Destroy;
End;

Function TColorDialog.Execute: Boolean;
Var
  i: Integer;
  r: TRect;
Begin
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
  result := ShowModal = mrok;
End;

End.

