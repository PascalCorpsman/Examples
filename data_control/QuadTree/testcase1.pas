(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of uquadtree unittests                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uquadtree;

Const
  SimulationRect: TQuadRect = (
    TopLeft: (x: - 10; y: - 10);
    BottomRight: (x: 10; y: 10)
    );
  SimulationCapacity = 5;
  TopRightRect: TQuadRect = (
    TopLeft: (x: 0; y: - 10);
    BottomRight: (x: 10; y: 0)
    );

Type

  TIntQuadTree = specialize TQuadTree < integer > ;

  { TTestCase1 }

  TTestCase1 = Class(TTestCase)
  private
    dut: TIntQuadTree;
  protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  published
    Procedure EmptyAfterCreate;
    Procedure UnableToAddOutOfRangePoint;
    Procedure AbleToAddInRangePoint;
    Procedure AddAndQueryAPoint;
    Procedure QueryARangeThatHoldsNoPoints;
    Procedure AddPointsUpUntilFirstSubdivide;
    Procedure RandomMassTest;
    Procedure RandomPartialMassTest;
  End;

Implementation

{$I uquadtree.inc}

{$IFDEF UseVectormath}
Uses uvectormath;
{$ENDIF}

Procedure TTestCase1.EmptyAfterCreate;
Var
  res: TIntQuadTree.TQuadTreeElementArray;
Begin
  res := dut.Query(SimulationRect);
  AssertTrue(res = Nil);
End;

Procedure TTestCase1.UnableToAddOutOfRangePoint;
Var
  e: TIntQuadTree.TQuadTreeElement;
Begin
  e.Data := 1;
  e.Position := v2(SimulationRect.TopLeft.x - 1, SimulationRect.TopLeft.y - 1);
  AssertFalse(dut.Add(e));
End;

Procedure TTestCase1.AbleToAddInRangePoint;
Var
  e: TIntQuadTree.TQuadTreeElement;
Begin
  e.Data := 1;
  e.Position := v2(
    (SimulationRect.TopLeft.x + SimulationRect.BottomRight.x) / 2
    , (SimulationRect.TopLeft.y + SimulationRect.BottomRight.y) / 2);
  AssertTrue(dut.Add(e));
End;

Procedure TTestCase1.AddAndQueryAPoint;
Var
  e: TIntQuadTree.TQuadTreeElement;
  res: TIntQuadTree.TQuadTreeElementArray;
Begin
  e.Data := 1;
  e.Position := v2(
    (SimulationRect.TopLeft.x + SimulationRect.BottomRight.x) / 2
    , (SimulationRect.TopLeft.y + SimulationRect.BottomRight.y) / 2);
  dut.Add(e);
  res := dut.Query(SimulationRect);
  AssertTrue(assigned(res));
  AssertTrue(res[0].Data = e.Data);
End;

Procedure TTestCase1.QueryARangeThatHoldsNoPoints;
Var
  e: TIntQuadTree.TQuadTreeElement;
  res: TIntQuadTree.TQuadTreeElementArray;
Begin
  e.Data := 1;
  e.Position := SimulationRect.TopLeft;
  AssertTrue(dut.Add(e));
  res := dut.Query(
    QuadRect(v2(0, 0), v2(10, 10))
    );
  AssertTrue(res = Nil);
End;

Procedure TTestCase1.AddPointsUpUntilFirstSubdivide;
Var
  e: TIntQuadTree.TQuadTreeElement;
  res: TIntQuadTree.TQuadTreeElementArray;
  i: Integer;
Begin
  (*
   * es Gibt 4 Teilbereiche
   *  -10/-10 .. 0/0 , 0/-10 .. 10/0
   *  -10/10 .. 0/10 , 0/10 .. 10/10
   *)
  For i := 1 To SimulationCapacity + 1 Do Begin
    e.Data := i;
    e.Position := SimulationRect.TopLeft;
    AssertTrue('Error, could not add element.', dut.Add(e));
  End;
  (*
   * Und auch gleich Prüfen ob die Elemente auch wieder Auslesbar sind
   *)
  res := dut.Query(SimulationRect);
  AssertEquals(SimulationCapacity + 1, length(res));
  // TODO: hier könnte ggf. noch geprüft werden ob e.Data auch alles stimmt
End;

Procedure TTestCase1.RandomMassTest;
Const
  Cnt = 1000;
Var
  e: TIntQuadTree.TQuadTreeElement;
  res: TIntQuadTree.TQuadTreeElementArray;
  i: Integer;
Begin
  RandSeed := 42;
  For i := 0 To Cnt - 1 Do Begin
    e.Data := i;
    e.Position := v2(
      random(20) - 10,
      random(20) - 10
      );
    AssertTrue('Error, could not add element.', dut.Add(e));
  End;
  (*
   * Und auch gleich Prüfen ob die Elemente auch wieder Auslesbar sind
   *)
  res := dut.Query(SimulationRect);
  AssertEquals(Cnt, length(res));
  // TODO: hier könnte ggf. noch geprüft werden ob e.Data auch alles stimmt
End;

Procedure TTestCase1.RandomPartialMassTest;
Const
  Cnt = 1000;
Var
  e: TIntQuadTree.TQuadTreeElement;
  res: TIntQuadTree.TQuadTreeElementArray;
  i: Integer;
  ptsCnt: integer;
Begin
  RandSeed := 43;
  ptsCnt := 0;
  For i := 0 To Cnt - 1 Do Begin
    e.Data := i;
    e.Position := v2(
      random(20) - 10,
      random(20) - 10
      );
    If QuadRectContainsPoint(TopRightRect, e.Position) Then Begin
      inc(ptsCnt);
    End;
    AssertTrue('Error, could not add element.', dut.Add(e));
  End;
  (*
   * Und auch gleich Prüfen ob die Elemente auch wieder Auslesbar sind
   *)
  res := dut.Query(TopRightRect);
  AssertEquals(ptsCnt, length(res));
  // TODO: hier könnte ggf. noch geprüft werden ob e.Data auch alles stimmt

End;

Procedure TTestCase1.SetUp;
Begin
  dut := TIntQuadTree.Create(SimulationRect, SimulationCapacity);
End;

Procedure TTestCase1.TearDown;
Begin
  dut.free;
  dut := Nil;
End;

Initialization

  RegisterTest(TTestCase1);
End.

