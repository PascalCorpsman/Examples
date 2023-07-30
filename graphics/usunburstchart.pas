(******************************************************************************)
(* uSunburstChart                                                  28.07.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Realises a SunBurstchart that is configurable as much as     *)
(*               possible.                                                    *)
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
Unit usunburstchart;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics;

Type

  TPointArray = Array Of TPoint;

  TColorSet = Record
    BrushColor: TColor;
    PenColor: TColor;
    PenWitdh: integer;
    FontColor: TColor;
  End;

  TChild = Record
    // Everything that a user is allowed to access to
    Caption: String;
    Color: TColorSet;
    SelectedColor: TColorSet;
    Value: Integer;
    UserData: Pointer;
    Childrens: Array Of TChild;
    Selected: Boolean; // If True, then Selected Brush / Selected will be used.

    // Everything that the user is not allowed to access (can and will be overwriten by TSunburstChart)
    AbsStartAngle: Single;
    AbsEndAngle: Single;
    Stage: Integer;
    StageRadius: Single;
  End;

  (*
   * Needed to give write access to Selected elements ..
   *)
  PChild = ^TChild;

  { TSunburstChart }

  TSunburstChart = Class(TGraphicControl)
  private
    fSelectedStack: Array Of PChild; // unly used during Paint
    fSelectedStackCnt: integer;
    FAngleOffset: Single;
    fInitialArc: Single;
    fPieCenter: Tpoint;
    fPieRadius: Single;
    fStageMargin: integer;
    Procedure setAngleOffset(AValue: Single);
    Procedure SetInitialArc(AValue: Single);
    Procedure SetPieCenter(AValue: Tpoint);

    Function CalcMaxStageCount(): Integer;
    Procedure SetPieRadius(AValue: Integer);

    Procedure RenderStageSegment(Var aElement: TChild; RenderAll: Boolean);
    Procedure SetPieRadius(AValue: Single);
    Procedure SetStageMargin(AValue: integer);
    Procedure CalcAllMetaData(); // Calc Stage, Angle informations for each Element accessable by Root

    Function SearchParent(Const aPotentialParent, aChild: PChild): Pchild;
  protected
    Procedure Paint; override;
  public
    Root: Array Of TChild; // TODO: Find a better way to access to this elements !

    Property StageMargin: integer read fStageMargin write SetStageMargin;

    Property OnMouseDown;
    Property OnResize;
    Property Color;

    Property InitialArc: Single read fInitialArc write SetInitialArc;
    Property AngleOffset: Single read FAngleOffset write setAngleOffset;
    Property PieCenter: Tpoint read fPieCenter write SetPieCenter;
    Property PieRadius: Single read fPieRadius write SetPieRadius;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy(); override;

    (*
     * Following some helper Routines to work with TChild / PChild
     * true on succeed
     *)
    Function AddChild(Const aElement: PChild; Child: TChild): Boolean; // if aElement = nil, child will always be added to root
    Function AddSibling(Const aElement: PChild; Sibling: TChild): Boolean;

    Procedure Deselect; // Iterates through all Childs and "desectes" them

    // Gives the Segment which collides with x,y
    // false if none exist
    // Only valid after the first "rendering"
    // Returns a pointer to give write access to the Element!
    Function GetSegmentAtPos(x, y: integer; Out aElement: PChild): Boolean;

    (*
     * Iterator Pattern to walk all Segments
     *)
    Function Iterator: PChild; // TODO: implement
    Function IterFirst: Pchild; // TODO: implement
    Function IterNext: PChild; // TODO: implement
  End;

Function DefaultChild(): TChild;

Implementation

Uses
  math;

Const
  Epsilon = 0.0001;

Procedure Nop();
Begin
End;

Function DefaultChild(): TChild;
Begin
  // The values the user is allowed to access to
  result.Caption := '';
  result.Color.BrushColor := clGray;
  result.Color.PenColor := clwhite;
  result.Color.PenWitdh := 1;
  result.Color.FontColor := clBlack;
  result.SelectedColor.BrushColor := clNavy;
  result.SelectedColor.PenColor := clred;
  result.SelectedColor.PenWitdh := 4;
  result.SelectedColor.FontColor := clWhite;
  result.Selected := false;
  result.Value := 1;
  result.UserData := Nil;
  result.Childrens := Nil;

  // The values the user shall not change / or edit, will be calculated through TSunburstChart
  result.AbsStartAngle := 0;
  result.AbsEndAngle := 0;
  result.Stage := 0;
  result.StageRadius := 0;
End;

{ TSunburstChart }

Constructor TSunburstChart.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Width := 300;
  Height := 300;
  PieRadius := 150;
  Color := clwhite;
  PieCenter := point(Width Div 2, height Div 2);
  InitialArc := 2 * pi;
  AngleOffset := 0;
  StageMargin := 0;
  Root := Nil;
  fSelectedStack := Nil;
  fSelectedStackCnt := 0;
End;

Destructor TSunburstChart.Destroy;
Begin
  Root := Nil;
  setlength(fSelectedStack, 0);
  Inherited Destroy;
End;

Function TSunburstChart.AddChild(Const aElement: PChild; Child: TChild
  ): Boolean;
Begin
  If aElement = Nil Then Begin
    setlength(Root, high(root) + 2);
    root[high(root)] := Child;
  End
  Else Begin
    setlength(aElement^.Childrens, high(aElement^.Childrens) + 2);
    aElement^.Childrens[high(aElement^.Childrens)] := Child;
  End;
  result := true;
End;

Function TSunburstChart.AddSibling(Const aElement: PChild; Sibling: TChild
  ): Boolean;

Var
  i: Integer;
  pa: PChild;
Begin
  result := false;
  // To be Able to add a Sibling we need to find the Parent of the aElement
  // case 1: aElement is Child of Root
  For i := 0 To high(Root) Do Begin
    If @root[i] = aElement Then Begin
      result := AddChild(Nil, Sibling);
    End;
    // case 2: aElement is child of a roots child
    pa := SearchParent(@root[i], aElement);
    If assigned(pa) Then Begin
      result := AddChild(pa, Sibling);
      exit;
    End;
  End;
End;

Procedure TSunburstChart.Deselect;
  Procedure DeSel(Var aElement: TChild);
  Var
    i: Integer;
  Begin
    aElement.Selected := false;
    For i := 0 To high(aElement.Childrens) Do Begin
      DeSel(aElement.Childrens[i]);
    End;
  End;

Var
  i: Integer;
Begin
  For i := 0 To high(Root) Do Begin
    DeSel(root[i]);
  End;
  Invalidate;
End;

Function TSunburstChart.GetSegmentAtPos(x, y: integer; Out aElement: PChild
  ): Boolean;

Var
  Stage: Integer;
  angle: Single;

  Function Search(Const asElement: TChild): Boolean;
  Var
    i: Integer;
  Begin
    result := (asElement.Stage = Stage) And
      (angle >= asElement.AbsStartAngle) And
      (angle <= asElement.AbsEndAngle);
    If result Then Begin
      aElement := @asElement;
    End
    Else Begin
      For i := 0 To high(asElement.Childrens) Do Begin
        result := Search(asElement.Childrens[i]);
        If result Then exit;
      End;
    End;
  End;

Var
  tx, ty, i: integer;
Begin
  result := false;
  If Not assigned(Root) Then exit;
  // 0. Alle Elemente initialisieren, sollten diese noch nie gerendert worden sein
  CalcAllMetaData();
  // 1. Bestimmen der Polarkoordinaten von X,Y
  tx := x - PieCenter.x;
  ty := PieCenter.y - y; // Man Bedenke die Y-Achse ist invertiert damit der Mathematische Drehsinn stimmt ;)
  // TODO: Abfangen Div by 0, wenn Root[0].StageRadius + StageMargin = 0 !
  Stage := trunc(sqrt(sqr(tx) + sqr(ty)) / (Root[0].StageRadius + StageMargin)); // Berechnen des Aktuellen Ringes
  angle := ArcTan2(ty, tx); // -pi .. pi
  If angle < 0 Then angle := angle + 2 * pi; // 0 .. 2 * pi (so wie alle internen Koordiaten abgespeichert sind !
  // 2. Suchen ob es ein Segment gibt, welches in diesen Koordinaten liegt !
  For i := 0 To high(root) Do Begin
    result := Search(root[i]);
    If result Then exit;
  End;
End;

Function TSunburstChart.Iterator: PChild;
Begin
  result := Nil;
End;

Function TSunburstChart.IterFirst: Pchild;
Begin
  result := Nil;
End;

Function TSunburstChart.IterNext: PChild;
Begin
  result := Nil;
End;

Procedure TSunburstChart.Paint;
Var
  i: integer;
Begin
  Inherited Paint; // Call the fOnPaint, ifneeded

  // Erase Background
  Canvas.Brush.Color := Color;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(-1, -1, Width + 1, Height + 1);

  If Not assigned(root) Then exit;
  CalcAllMetaData;

  fSelectedStackCnt := 0;
  // Umstellen auf Iterator pattern , aber nur Testweise !
  For i := 0 To high(Root) Do Begin
    RenderStageSegment(root[i], True);
  End;

  // Re Render all Selected Parts so that their border can overpaint the non selected areas
  For i := 0 To fSelectedStackCnt - 1 Do Begin
    RenderStageSegment(fSelectedStack[i]^, false);
  End;
End;

Procedure TSunburstChart.RenderStageSegment(Var aElement: TChild;
  RenderAll: Boolean);

  Procedure PlotTextAtPos(p: Tpoint);
  Begin
    canvas.TextOut(
      p.x - Canvas.TextWidth(aElement.Caption) Div 2,
      p.Y - Canvas.TextHeight(aElement.Caption) Div 2,
      aElement.Caption
      );
  End;

Var
  c, s, a: extended;
  cnt, i: Integer;
  PolyPoints: Array Of TPoint;
  InnerRadius, OuterRadius: Single;
Begin
  PolyPoints := Nil;
  // 1. Rendern des Segmentes
  If aElement.Selected Then Begin
    Canvas.Brush.Color := aElement.SelectedColor.BrushColor;
    canvas.Pen.Color := aElement.SelectedColor.PenColor;
    canvas.Pen.Width := aElement.SelectedColor.PenWitdh;
    canvas.Font.Color := aElement.SelectedColor.FontColor;
    If RenderAll Then Begin
      inc(fSelectedStackCnt);
      If fSelectedStackCnt > high(fSelectedStack) Then Begin
        // In case where typically only 1 element is selected
        // This is totally fine. In case with lots and lots selected
        // elements, this is really high cost at the very first rendering ..
        setlength(fSelectedStack, fSelectedStackCnt);
        fSelectedStack[fSelectedStackCnt - 1] := @aElement;
      End;
    End;
  End
  Else Begin
    Canvas.Brush.Color := aElement.Color.BrushColor;
    canvas.Pen.Color := aElement.Color.PenColor;
    canvas.Pen.Width := aElement.Color.PenWitdh;
    canvas.Font.Color := aElement.Color.FontColor;
  End;

  InnerRadius := aElement.Stage * (aElement.StageRadius + StageMargin);
  OuterRadius := (aElement.Stage + 1) * (aElement.StageRadius);
  If InnerRadius = 0 Then Begin
    If abs(aElement.AbsEndAngle - aElement.AbsStartAngle - 2 * pi) <= Epsilon Then Begin
      // Ein Vollkreis
      Canvas.Ellipse(
        round(PieCenter.x - OuterRadius),
        round(PieCenter.Y - OuterRadius),
        round(PieCenter.x + OuterRadius),
        round(PieCenter.Y + OuterRadius)
        );
      PlotTextAtPos(PieCenter);
    End
    Else Begin
      // Ein "Torten" Element
      cnt := max(3, round(((aElement.AbsEndAngle - aElement.AbsStartAngle) * OuterRadius) / 10));
      setlength(PolyPoints, cnt + 2);
      PolyPoints[0] := PieCenter;
      For i := 0 To cnt Do Begin
        a := (aElement.AbsEndAngle - aElement.AbsStartAngle) * i / cnt + aElement.AbsStartAngle;
        SinCos(a, s, c);
        PolyPoints[i + 1] := point(round(PieCenter.x + OuterRadius * c), round(PieCenter.Y - OuterRadius * s));
      End;
      Canvas.Polygon(PolyPoints);
      a := (aElement.AbsEndAngle + aElement.AbsStartAngle) / 2;
      SinCos(a, s, c);
      PlotTextAtPos(point(round(PieCenter.x + OuterRadius * c / 2), round(PieCenter.Y - OuterRadius * s / 2)));
    End;
  End
  Else Begin
    // Ein Segment "außen" also als Ring
    cnt := max(3, round(((aElement.AbsEndAngle - aElement.AbsStartAngle) * OuterRadius) / 10));
    setlength(PolyPoints, 2 * (cnt + 1));
    For i := 0 To cnt Do Begin
      a := (aElement.AbsEndAngle - aElement.AbsStartAngle) * i / cnt + aElement.AbsStartAngle;
      SinCos(a, s, c);
      PolyPoints[i] := point(round(PieCenter.x + InnerRadius * c), round(PieCenter.Y - InnerRadius * s));
      PolyPoints[2 * (cnt + 1) - 1 - i] := point(round(PieCenter.x + OuterRadius * c), round(PieCenter.Y - OuterRadius * s));
    End;
    Canvas.Polygon(PolyPoints);
    a := (aElement.AbsEndAngle + aElement.AbsStartAngle) / 2;
    SinCos(a, s, c);
    PlotTextAtPos(point(round(PieCenter.x + (InnerRadius + OuterRadius) * c / 2), round(PieCenter.Y - (InnerRadius + OuterRadius) * s / 2)));
  End;
  // 2. Rekursiver Abstieg
  If RenderAll Then Begin
    For i := 0 To high(aElement.Childrens) Do Begin
      RenderStageSegment(aElement.Childrens[i], true);
    End;
  End;
End;

Procedure TSunburstChart.SetPieCenter(AValue: Tpoint);
Begin
  If fPieCenter = AValue Then Exit;
  fPieCenter := AValue;
  Invalidate;
End;

Function TSunburstChart.CalcMaxStageCount: Integer;
  Function GetDepthOf(aElement: TChild): integer;
  Var
    i: Integer;
  Begin
    result := 1;
    For i := 0 To high(aElement.Childrens) Do Begin
      result := max(result, 1 + GetDepthOf(aElement.Childrens[i]));
    End;
  End;

Var
  i: Integer;
Begin
  If assigned(Root) Then Begin
    result := 1;
    For i := 0 To high(Root) Do Begin
      result := max(result, GetDepthOf(Root[i]));
    End;
  End
  Else Begin
    Result := 0;
  End;
End;

Procedure TSunburstChart.SetPieRadius(AValue: Integer);
Begin
  If fPieRadius = AValue Then Exit;
  fPieRadius := AValue;
  Invalidate;
End;

Procedure TSunburstChart.SetPieRadius(AValue: Single);
Begin
  If fPieRadius = AValue Then Exit;
  fPieRadius := AValue;
  Invalidate;
End;

Procedure TSunburstChart.SetStageMargin(AValue: integer);
Begin
  If fStageMargin = AValue Then Exit;
  fStageMargin := AValue;
  Invalidate;
End;

Procedure TSunburstChart.CalcAllMetaData;
Var
  StageRadius: Single;
  Procedure CalcAllMetaDataSub(Var aElement: TChild);
  Var
    ElementSum, i: integer;
    AngleDist, ChildStartAngle, ChildAngleDist: Single;
  Begin
    ElementSum := 0;
    For i := 0 To high(aElement.Childrens) Do Begin
      ElementSum := ElementSum + aElement.Childrens[i].Value;
    End;
    If ElementSum = 0 Then ElementSum := 1;
    AngleDist := aElement.AbsEndAngle - aElement.AbsStartAngle;
    ChildStartAngle := aElement.AbsStartAngle;

    For i := 0 To high(aElement.Childrens) Do Begin
      ChildAngleDist := AngleDist * aElement.Childrens[i].Value / ElementSum;
      aElement.Childrens[i].StageRadius := aElement.StageRadius;
      aElement.Childrens[i].Stage := aElement.Stage + 1;
      aElement.Childrens[i].AbsStartAngle := ChildStartAngle;
      aElement.Childrens[i].AbsEndAngle := ChildStartAngle + ChildAngleDist;
      CalcAllMetaDataSub(aElement.Childrens[i]);
      ChildStartAngle := ChildStartAngle + ChildAngleDist;
    End;
  End;

Var
  StageCount, i, ElementSum: integer;
  AngleDist, ChildStartAngle, ChildAngleDist: Single;
Begin
  StageCount := CalcMaxStageCount();
  If StageCount = 0 Then exit;

  StageRadius := (PieRadius - StageMargin * (StageCount - 1)) / StageCount;

  ElementSum := 0;
  For i := 0 To high(Root) Do Begin
    ElementSum := ElementSum + Root[i].Value;
  End;
  If ElementSum = 0 Then ElementSum := 1;

  AngleDist := InitialArc;
  ChildStartAngle := AngleOffset;

  For i := 0 To high(Root) Do Begin
    ChildAngleDist := AngleDist * Root[i].Value / ElementSum;
    root[i].StageRadius := StageRadius;
    root[i].Stage := 0;
    root[i].AbsStartAngle := ChildStartAngle;
    root[i].AbsEndAngle := ChildStartAngle + ChildAngleDist;
    CalcAllMetaDataSub(root[i]);
    ChildStartAngle := ChildStartAngle + ChildAngleDist;
  End;
End;

Function TSunburstChart.SearchParent(Const aPotentialParent, aChild: PChild
  ): Pchild;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(aPotentialParent^.Childrens) Do Begin
    If @aPotentialParent^.Childrens[i] = aChild Then Begin
      result := aPotentialParent;
      exit;
    End;
    result := SearchParent(@aPotentialParent^.Childrens[i], aChild);
    If assigned(result) Then exit;
  End;
End;

Procedure TSunburstChart.SetInitialArc(AValue: Single);
Begin
  If fInitialArc = AValue Then Exit;
  fInitialArc := AValue;
  Invalidate;
End;

Procedure TSunburstChart.setAngleOffset(AValue: Single);
Begin
  If FAngleOffset = AValue Then Exit;
  FAngleOffset := AValue;
  Invalidate;
End;

End.

