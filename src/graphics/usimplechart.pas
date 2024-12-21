(******************************************************************************)
(* TSimpleChart                                                    01.12.2022 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Corpsman                                                     *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : The primitive reimplementation of a TAChart similar component*)
(*               especially for console applications. TSimpleChart supports   *)
(*               as many TSeries with their own Y-achsis scalings as needed.  *)
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
(*               0.02 - Add ability to "work" with series                     *)
(*                      FIX: Achsis unit was not rendered                     *)
(*                                                                            *)
(******************************************************************************)
Unit usimplechart;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, graphics, uvectormath;

Type
  TDimension = Record
    MinX, MaxX, MinY, MaxY: TBaseType;
  End;

  TYAxesPos = (apLeft, apRight, apNone);

  TSimpleChart = Class;

  TXInterval = Record
    MinX, MaxX: TBaseType;
  End;

  TYAxis = Record
    Pos: TYAxesPos;
    AxisUnit: String;
    UseMinVal: Boolean;
    MinVal: TBaseType;
    UseMaxVal: Boolean;
    MaxVal: TBaseType;
    MarkFormat: String;
  End;

  TXAxis = Record
    AxisUnit: String;
    UseMinVal: Boolean;
    MinVal: TBaseType;
    UseMaxVal: Boolean;
    MaxVal: TBaseType;
    GridColor: TColor;
    MarkFormat: String;
  End;

  { TSeries }

  TSeries = Class
  private
    fOwner: TSimpleChart;
    fDim: TDimension; // !! Do not access directly to this value, always use GetDimension !!
    fDataPoints: Array Of TVector2;
    Function RenderLeftYAchsis(Const Canvas: TCanvas; Rect: Trect): integer; // Es werden von Rect nur Top, Bottom und Left verwendet!
    Function RenderRightYAchsis(Const Canvas: TCanvas; Rect: TRect): integer; // Es werden von Rect nur Top, Bottom und Left verwendet!
    Procedure RenderToRect(Const Canvas: TCanvas; Rect: TRect; XInterval: TXInterval); // zeichnet die "Kurve" in das entsprechende Rechteck
  public
    SeriesWidth: integer;
    SeriesCaption: String;
    SeriesColor: TColor;

    GridColor: TColor;
    YAxis: TYAxis;

    Constructor Create();
    Destructor Destroy(); override;

    Procedure AddDataPoint(x, y: Single);
    Procedure Clear();

    Function GetDimension(): TDimension;
  End;

  { TSimpleChart }

  TSimpleChart = Class
  private
    fSeries: Array Of TSeries;
    Function GetSeries(Index: integer): TSeries;
    Procedure SetSeries(Index: integer; AValue: TSeries);
  public
    Title: String;
    ShowLegend: Boolean;
    BackGroundColor: TColor;
    XAXis: TXAxis;

    Property Series[Index: integer]: TSeries read GetSeries write SetSeries;

    Constructor Create();
    Destructor Destroy(); override;

    Procedure AddSeries(aSeries: TSeries);
    Procedure Clear();

    Function SaveToPngImage(Width, Height: integer): TPortableNetworkGraphic;
  End;

Implementation

Uses Math;

Const
  Margin = 5;
  TickLen = 3;

Function Convert(Const Dim: TDimension; Rect: Trect; V: TVector2): TPoint;
Begin
  result.x := round(ConvertDimension(Dim.MinX, Dim.MaxX, v.x, rect.Left, Rect.Right));
  result.Y := round(ConvertDimension(Dim.MaxY, Dim.MinY, v.y, rect.Top, Rect.Bottom)); // Y-Achse ist gedreht ;)
End;

{ TSeries }

Constructor TSeries.Create;
Begin
  fDataPoints := Nil;
  SeriesCaption := '';
  SeriesColor := clBlack;
  YAxis.Pos := apLeft;
  YAxis.AxisUnit := '';
  YAxis.UseMinVal := false;
  YAxis.MinVal := 0;
  YAxis.UseMaxVal := false;
  YAxis.MaxVal := 0;
  YAxis.MarkFormat := '%f';
  fOwner := Nil;
  SeriesWidth := 1;
  GridColor := clGray;
End;

Destructor TSeries.Destroy;
Var
  i, j: Integer;
Begin
  Clear;
  // Remove Self from Owner
  If assigned(fOwner) Then Begin
    For i := 0 To high(fOwner.fSeries) Do Begin
      If fOwner.fSeries[i] = Self Then Begin
        For j := i To high(fOwner.fSeries) - 1 Do Begin
          fOwner.fSeries[j] := fOwner.fSeries[j + 1];
        End;
        setlength(fOwner.fSeries, High(fOwner.fSeries));
        break;
      End;
    End;
  End;
End;

Function TSeries.RenderLeftYAchsis(Const Canvas: TCanvas; Rect: Trect): integer;
Var
  dim: TDimension;
  dy: TBaseType;
  h, i: Integer;
  p: TPoint;
Begin
  result := 0;
  Case YAxis.Pos Of
    apNone, apRight: Begin
        // Nix zu tun
      End;
    apLeft: Begin
        dim := GetDimension();
        result := max(Canvas.TextWidth(format(YAxis.MarkFormat, [dim.MinY])), Canvas.TextWidth(format(YAxis.MarkFormat, [dim.MaxY])));
        If YAxis.AxisUnit <> '' Then Begin
          result := result + Margin + Canvas.TextWidth(YAxis.AxisUnit);
        End;
        result := result + Margin;
        dy := (Dim.MaxY - dim.MinY) / 10;
        h := Canvas.TextHeight('8') Div 2;
        For i := 0 To 10 Do Begin
          p := Convert(dim, rect, v2(0, Dim.MinY + dy * i));
          canvas.font.Color := SeriesColor;
          canvas.TextOut(rect.Left, p.Y - h, format(YAxis.MarkFormat, [Dim.MinY + dy * i]) + YAxis.AxisUnit);
        End;
      End;
  End;
End;

Function TSeries.RenderRightYAchsis(Const Canvas: TCanvas; Rect: TRect
  ): integer;
Var
  dim: TDimension;
  dy: TBaseType;
  p: TPoint;
  h, i: Integer;
Begin
  result := 0;
  Case YAxis.Pos Of
    apNone, apLeft: Begin
        // Nix zu tun
      End;
    apRight: Begin
        dim := GetDimension();
        result := max(Canvas.TextWidth(format(YAxis.MarkFormat, [dim.MinY])), Canvas.TextWidth(format(YAxis.MarkFormat, [dim.MaxY])));
        If YAxis.AxisUnit <> '' Then Begin
          result := result + Margin + Canvas.TextWidth(YAxis.AxisUnit);
        End;
        result := result + Margin;
        dy := (Dim.MaxY - dim.MinY) / 10;
        h := Canvas.TextHeight('8') Div 2;
        For i := 0 To 10 Do Begin
          p := Convert(dim, rect, v2(0, Dim.MinY + dy * i));
          canvas.font.Color := SeriesColor;
          canvas.TextOut(rect.Right - result + Margin, p.Y - h, format(YAxis.MarkFormat, [Dim.MinY + dy * i]) + YAxis.AxisUnit);
        End;
      End;
  End;
End;

Procedure TSeries.RenderToRect(Const Canvas: TCanvas; Rect: TRect;
  XInterval: TXInterval);
Var
  dim: TDimension;
  tickx, i: Integer;
  p: TPoint;
  dy: TBaseType;
Begin
  dim := GetDimension();
  dim.MinX := min(XInterval.MinX, dim.MinX);
  dim.MaxX := max(XInterval.MaxX, dim.MaxX);
  // Die Y-Ticks
  If YAxis.Pos <> apNone Then Begin
    If YAxis.Pos = apLeft Then Begin
      tickx := Rect.Left;
    End
    Else Begin
      tickx := Rect.Right - TickLen;
    End;
    // Y-Ticks Rendern immer von TickX bis TickX + TickLen;
    canvas.Pen.Color := SeriesColor;
    canvas.Pen.Width := 1;
    dy := (Dim.MaxY - dim.MinY) / 10;
    For i := 1 To 9 Do Begin
      p := Convert(dim, rect, v2(0, Dim.MinY + dy * i));
      Canvas.MoveTo(tickx, p.Y);
      Canvas.LineTo(tickx + TickLen, p.Y);
      If GridColor <> clNone Then Begin
        canvas.pen.Width := 1;
        canvas.pen.Color := GridColor;
        Canvas.Pen.Style := psDash;
        canvas.MoveTo(rect.left, p.y);
        canvas.LineTo(rect.Right, p.y);
        Canvas.Pen.Style := psSolid;
      End;
    End;
  End;
  // Die eigentliche Kurve
  canvas.Pen.Color := SeriesColor;
  canvas.Pen.Width := SeriesWidth;
  canvas.MoveTo(Convert(dim, rect, fDataPoints[0]));
  For i := 1 To high(fDataPoints) Do Begin
    canvas.LineTo(Convert(dim, rect, fDataPoints[i]));
  End;
End;

Procedure TSeries.AddDataPoint(x, y: Single);
Begin
  setlength(fDataPoints, high(fDataPoints) + 2);
  fDataPoints[high(fDataPoints)] := V2(x, y);
  If length(fDataPoints) = 1 Then Begin
    fDim.MinX := x;
    fDim.MaxX := x;
    fDim.MinY := y;
    fDim.MaxY := y;
  End
  Else Begin
    fDim.MinX := min(fDim.MinX, x);
    fDim.MaxX := max(fDim.MaxX, x);
    fDim.Miny := min(fDim.MinY, y);
    fDim.Maxy := max(fDim.MaxY, y);
  End;
End;

Procedure TSeries.Clear;
Begin
  setlength(fDataPoints, 0);
End;

Function TSeries.GetDimension: TDimension;
Begin
  If Not assigned(fDataPoints) Then Begin
    Raise exception.Create('TSeries.GetDimension: no datapoints to get dimension of.');
  End;
  result := fDim;
  // TODO: Which version is correkt ?
  If YAxis.UseMinVal Then Begin
    result.MinY := min(result.MinY, YAxis.MinVal);
    //    result.MinY := YAxis.MinVal;
  End;
  If YAxis.UseMaxVal Then Begin
    result.MaxY := max(result.MaxY, YAxis.MaxVal);
    //    result.MaxY := YAxis.MaxVal;
  End;
End;

{ TSimpleChart }

Function TSimpleChart.GetSeries(Index: integer): TSeries;
Begin
  result := fSeries[Index];
End;

Procedure TSimpleChart.SetSeries(Index: integer; AValue: TSeries);
Begin
  fSeries[Index] := AValue;
End;

Constructor TSimpleChart.Create;
Begin
  Inherited Create;
  Title := '';
  fSeries := Nil;
  ShowLegend := true;
  BackGroundColor := clWhite;
  XAXis.GridColor := clGray;
  XAXis.AxisUnit := '';
  XAXis.UseMinVal := false;
  XAXis.UseMaxVal := false;
  XAXis.MinVal := 0;
  XAXis.MaxVal := 0;
  XAXis.MarkFormat := '%f';
End;

Destructor TSimpleChart.Destroy;
Begin
  Clear;
End;

Function TSimpleChart.SaveToPngImage(Width, Height: integer
  ): TPortableNetworkGraphic;

  Procedure RenderNumOnPos(P: TPoint; Value: Single; Const Canvas: TCanvas);
  Var
    t: String;
    tw: Integer;
  Begin
    t := format(XAXis.MarkFormat, [Value]) + XAXis.AxisUnit;
    tw := Canvas.TextWidth(t);
    canvas.Font.Color := clBlack;
    canvas.TextOut(p.x - tw Div 2, p.y + Margin, t);
  End;

Var
  b: TBitmap;
  dim, dim2: TDimension;
  Legendwidth, i: Integer;
  RenderRect: TRect;
  th, tw, t: integer;
  XInterval: TXInterval;
  dx: TBaseType;
  p: TPoint;
Begin
  result := TPortableNetworkGraphic.Create;
  b := TBitmap.Create;
  b.Width := Width;
  b.Height := Height;
  b.canvas.brush.color := BackGroundColor;
  b.canvas.Rectangle(-1, -1, Width + 1, Height + 1);
  If Not assigned(fSeries) Then Begin
    result.Assign(b);
    b.free;
    exit;
  End;
  // 1. Bestimmen der Maximalen Dimension in "EchtKoordinaten" -> Zur Bestimmung der Scalierung der X-Achse
  dim := fSeries[0].GetDimension();
  For i := 1 To high(fSeries) Do Begin
    dim2 := fSeries[i].GetDimension();
    dim.MinX := min(dim.MinX, dim2.MinX);
    dim.MaxX := max(dim.MaxX, dim2.MaxX);
    dim.Miny := min(dim.Miny, dim2.MinY);
    dim.MaxY := max(dim.Maxy, dim2.MaxY);
  End;
  // TODO: Which version is correkt ?
  If XAXis.UseMinVal Then Begin
    dim.MinX := min(dim.MinX, XAXis.MinVal);
    //    dim.MinX := XAXis.MinVal;
  End;
  If XAXis.UseMaxVal Then Begin
    dim.MaxX := max(dim.MaxX, XAXis.MaxVal);
    //    dim.MaxX := XAXis.MaxVal;
  End;
  XInterval.MinX := dim.MinX;
  XInterval.MaxX := dim.MaxX;
  Legendwidth := 0;
  If ShowLegend Then Begin
    th := b.canvas.TextHeight('h');
    For i := 0 To high(fSeries) Do Begin
      tw := b.Canvas.TextWidth(fSeries[i].SeriesCaption);
      Legendwidth := max(Legendwidth, tw);
      b.canvas.Font.Color := clBlack;
      b.canvas.TextOut(Margin, Margin + th * (i + 1), fSeries[i].SeriesCaption);
    End;
    Legendwidth := Legendwidth + Margin + 50 + 2 * Margin; // 5 = Abstand , 50 = Länge des "Vorschau" Striches, 2*5 = Abstand Links und Rechts
    For i := 0 To high(fSeries) Do Begin
      b.canvas.Pen.Color := fSeries[i].SeriesColor;
      b.canvas.Pen.Width := fSeries[i].SeriesWidth;
      b.canvas.MoveTo(Legendwidth - Margin - 50, Margin + th * (i + 1) + th Div 2);
      b.canvas.LineTo(Legendwidth - Margin, Margin + th * (i + 1) + th Div 2);
    End;
  End;

  // Bestimmen des Bereiches in den die Kurven gemalt werden, dieser Bereich wird
  // Links und Rechts durch die Legenden und Achsenbeschriftungen beschränkt
  // Oben und Unten durch den Title und die x-Achsenbeschriftung

  If Title <> '' Then Begin
    RenderRect.Top := b.canvas.TextHeight(Title) + 2 * Margin;
  End
  Else Begin
    RenderRect.Top := Margin;
  End;
  RenderRect.Bottom := b.Height - b.canvas.TextHeight('Blub') - 2 * Margin;
  RenderRect.left := Legendwidth;
  RenderRect.Right := b.Width - margin;
  For i := 0 To high(fSeries) Do Begin
    t := fSeries[i].RenderLeftYAchsis(b.canvas, RenderRect);
    RenderRect.left := RenderRect.left + t;
    t := fSeries[i].RenderRightYAchsis(b.canvas, RenderRect);
    RenderRect.Right := RenderRect.Right - t;
  End;
  If Title <> '' Then Begin
    b.canvas.Font.Color := clBlack;
    b.canvas.TextOut((RenderRect.Left + RenderRect.Right) Div 2 - b.Canvas.TextWidth(Title) Div 2, margin, Title);
  End;
  // Den Rahmen um alle Kurven drum rum machen
  b.canvas.pen.Width := 1;
  b.canvas.pen.Color := clblack;
  b.canvas.Rectangle(RenderRect);
  dx := (dim.MaxX - dim.MinX) / 10;

  // Die X-Achse
  For i := 0 To 9 Do Begin
    p := Convert(dim, RenderRect, v2(i * dx, dim.miny));
    If i <> 0 Then Begin
      b.Canvas.Pen.Width := 1;
      b.Canvas.Pen.Color := clBlack;
      b.Canvas.MoveTo(p.x, RenderRect.Bottom - 1);
      b.Canvas.LineTo(p.x, RenderRect.Bottom - 1 - TickLen);
      b.Canvas.MoveTo(p.x, RenderRect.Top);
      b.Canvas.LineTo(p.x, RenderRect.Top + TickLen);
      If XAXis.GridColor <> clNone Then Begin
        b.Canvas.Pen.Color := XAXis.GridColor;
        b.Canvas.Pen.Style := psDash;
        b.Canvas.MoveTo(p.x, RenderRect.Bottom - 1);
        b.Canvas.LineTo(p.x, RenderRect.Top);
        b.Canvas.Pen.Style := psSolid;
      End;
    End;
    RenderNumOnPos(p, i * dx, b.Canvas);
  End;
  tw := b.Canvas.TextWidth(Format(XAXis.MarkFormat, [dim.MaxX]));
  p := Convert(dim, RenderRect, v2(dim.MaxX, dim.miny));
  If p.x + tw Div 2 <= Width Then Begin
    RenderNumOnPos(p, dim.MaxX, b.Canvas);
  End;

  // Die Eigentlichen Kurven Rendern
  For i := 0 To high(fSeries) Do Begin
    fSeries[i].RenderToRect(b.canvas, RenderRect, XInterval);
  End;
  result.Assign(b);
  b.free;
End;

Procedure TSimpleChart.AddSeries(aSeries: TSeries);
Var
  i: Integer;
Begin
  For i := 0 To high(fSeries) Do Begin
    If fSeries[i] = aSeries Then Begin
      Raise exception.create('Error, already added.');
    End;
  End;
  setlength(fSeries, high(fSeries) + 2);
  fSeries[high(fSeries)] := aSeries;
  fSeries[high(fSeries)].fOwner := Self;
End;

Procedure TSimpleChart.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(fSeries) Do Begin
    fSeries[i].fOwner := Nil; // Otherwise we get a Loop with "freeing"
    fSeries[i].Free;
  End;
  setlength(fSeries, 0);
End;

End.

