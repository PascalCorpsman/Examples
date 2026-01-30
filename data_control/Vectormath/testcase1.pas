Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testregistry, uvectormath;

Type

  { TGeneralTests }

  TGeneralTests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
  published
    Procedure RectIsInRect;
    Procedure RectIsNotInRect;
  End;

  { TVector2Tests }

  TVector2Tests = Class(TTestCase)
  protected
  published
    Procedure Lerp;
  End;

  { TVector3Tests }

  TVector3Tests = Class(TTestCase)
  protected
  published
    Procedure LinearDependent;
  End;

  { TMatrix2x2Tests }

  TMatrix2x2Tests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
    Procedure InvertMatrixThatRaisesAnException;
  published
    Procedure MarixMultiplication;
    Procedure OperatorEqual;
    Procedure InvertMatrix;
    Procedure InvertMatrix2;
  End;

  { TMatrix3x3Tests }

  TMatrix3x3Tests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
    Procedure InvertMatrixThatRaisesAnException;
  published
    Procedure MarixMultiplication;
    Procedure OperatorEqual;
    Procedure InvertMatrix;
    Procedure InvertMatrix2;
  End;

  { TMatrix4x4Tests }

  TMatrix4x4Tests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
    Procedure InvertMatrixThatRaisesAnException;
  published
    Procedure MarixMultiplication;
    Procedure OperatorEqual;
    Procedure InvertMatrix;
    Procedure InvertMatrix2;
  End;

  { TMatrixNxMTests }

  TMatrixNxMTests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
  published
    Procedure InitEmpty;
    Procedure InitByElements;
    Procedure MatrixToVector;
    Procedure MarixMultiplication;
    Procedure OperatorEqual;
    Procedure LoadFromSaveToStream;
  End;

Implementation

{ TGeneralTests }

Procedure TGeneralTests.RectIsInRect;
Var
  TL1, BR1, TL2, BR2: TVector2;
Begin
  (*
   * Overlapping Edges
   *)
  TL1 := v2(-1, 1);
  BR1 := v2(1, -1);
  TL2 := v2(0.5, 2);
  BR2 := v2(2, 0.5);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(0.5, -2);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(-2, -0.5);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-0.5, 2);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  (*
   * Overlapping Sides
   *)
  TL2 := v2(0.5, 0.5);
  BR2 := v2(2, -0.5);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(-0.5, -2);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-2, 0.5);
  BR2 := v2(-0.5, -0.5);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(0.5, 2);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  (*
   * complete in each other
   *)
  TL2 := v2(-2, 2);
  BR2 := v2(2, -2);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-0.5, 0.5);
  BR2 := v2(0.5, -0.5);
  AssertTrue(RectIntersectRect(TL1, BR1, TL2, BR2));
End;

Procedure TGeneralTests.RectIsNotInRect;
Var
  TL1, BR1, TL2, BR2: TVector2;
Begin
  // Above
  TL1 := v2(-1, 1);
  BR1 := v2(1, -1);
  TL2 := v2(-2.5, 3.5);
  BR2 := v2(-1.5, 1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-0.5, 3.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(0.5, 1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(1.5, 3.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(2.5, 1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  // Side by side
  TL2 := v2(-2.5, 3.5); // left
  BR2 := v2(-1.5, 1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-2.5, 0.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(-1.5, -0.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-2.5, -1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(-1.5, -2.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(2.5, 3.5); // right
  BR2 := v2(1.5, 1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(2.5, 0.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(1.5, -0.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(2.5, -1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(1.5, -2.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  // Below
  TL2 := v2(-2.5, -3.5);
  BR2 := v2(-1.5, -1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(-0.5, -3.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(0.5, -1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  TL2 := v2(1.5, -3.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
  BR2 := v2(2.5, -1.5);
  AssertFalse(RectIntersectRect(TL1, BR1, TL2, BR2));
End;

{ TVector2Tests }

Procedure TVector2Tests.Lerp;
Var
  v_1, v_2: TVector2;
Begin
  v_1 := v2(0, 0);
  v_2 := v2(10, 0);
  // x-Component
  v_1.Lerp(v_2, 0); // Amp = 0 -> stay the same
  AssertTrue('got: ' + v_1.ToString + ' expected (0 0)', Equal(v_1, v2(0, 0)));
  v_1.Lerp(v_2, 1); // Amp = 100 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (10 0)', Equal(v_1, v2(10, 0)));
  v_1 := v2(0, 0);
  v_1.Lerp(v_2, 0.5); // Amp = 50 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (5 0)', Equal(v_1, v2(5, 0)));
  v_1 := v2(0, 0);
  v_1.Lerp(v_2, 0.1); // Amp = 50 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (1 0)', Equal(v_1, v2(1, 0)));
  // y-Component
  v_2 := v2(0, 10);
  v_1 := v2(0, 0);
  v_1.Lerp(v_2, 0); // Amp = 0 -> stay the same
  AssertTrue('got: ' + v_1.ToString + ' expected (0 0)', Equal(v_1, v2(0, 0)));
  v_1.Lerp(v_2, 1); // Amp = 100 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (0 10)', Equal(v_1, v2(0, 10)));
  v_1 := v2(0, 0);
  v_1.Lerp(v_2, 0.5); // Amp = 50 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (0 5)', Equal(v_1, v2(0, 5)));
  v_1 := v2(0, 0);
  v_1.Lerp(v_2, 0.1); // Amp = 50 % = the other vector
  AssertTrue('got: ' + v_1.ToString + ' expected (0 1)', Equal(v_1, v2(0, 1)));
End;

{ TVector3Tests }

Procedure TVector3Tests.LinearDependent;
Var
  x1, x2, x3, sx1, sx2, sx3, a, b: TVector3;
Begin
  x1 := v3(1, 0, 0);
  x2 := v3(0, 1, 0);
  x3 := v3(0, 0, 1);
  sx1 := v3(2, 0, 0);
  sx2 := v3(0, 3, 0);
  sx3 := v3(0, 0, 4);
  // Trivial Cases
  // Not Dependent
  AssertFalse('Expected false, got true for (' + print(x1) + '/' + print(x2) + ')', IsLinearDependent(x1, x2));
  AssertFalse('Expected false, got true for (' + print(x1) + '/' + print(x3) + ')', IsLinearDependent(x1, x3));
  AssertFalse('Expected false, got true for (' + print(x2) + '/' + print(x1) + ')', IsLinearDependent(x2, x1));
  AssertFalse('Expected false, got true for (' + print(x2) + '/' + print(x3) + ')', IsLinearDependent(x2, x3));
  AssertFalse('Expected false, got true for (' + print(x3) + '/' + print(x1) + ')', IsLinearDependent(x3, x1));
  AssertFalse('Expected false, got true for (' + print(x3) + '/' + print(x2) + ')', IsLinearDependent(x3, x2));
  AssertFalse('Expected false, got true for (' + print(x2) + '/' + print(x1) + ')', IsLinearDependent(x2, x1));
  AssertFalse('Expected false, got true for (' + print(x3) + '/' + print(x1) + ')', IsLinearDependent(x3, x1));
  AssertFalse('Expected false, got true for (' + print(x1) + '/' + print(x2) + ')', IsLinearDependent(x1, x2));
  AssertFalse('Expected false, got true for (' + print(x3) + '/' + print(x2) + ')', IsLinearDependent(x3, x2));
  AssertFalse('Expected false, got true for (' + print(x1) + '/' + print(x3) + ')', IsLinearDependent(x1, x3));
  AssertFalse('Expected false, got true for (' + print(x2) + '/' + print(x3) + ')', IsLinearDependent(x2, x3));
  // Dependent
  AssertTrue(IsLinearDependent(x1, -x1));
  AssertTrue(IsLinearDependent(x2, -x2));
  AssertTrue(IsLinearDependent(x3, -x3));
  AssertTrue(IsLinearDependent(-x1, x1));
  AssertTrue(IsLinearDependent(-x2, x2));
  AssertTrue(IsLinearDependent(-x3, x3));
  AssertTrue(IsLinearDependent(x1, sx1));
  AssertTrue(IsLinearDependent(x2, sx2));
  AssertTrue(IsLinearDependent(x3, sx3));
  AssertTrue(IsLinearDependent(sx1, x1));
  AssertTrue(IsLinearDependent(sx2, x2));
  AssertTrue(IsLinearDependent(sx3, x3));
  a := v3(1, 2, 3);
  AssertTrue(IsLinearDependent(a, a * 2));
  AssertTrue(IsLinearDependent(2 * a, a));
  b := v3(1, 2, 4);
  AssertFalse(IsLinearDependent(a, b));
  AssertFalse(IsLinearDependent(b, a));
  AssertFalse(IsLinearDependent(2 * a, b));
  AssertFalse(IsLinearDependent(a, 2 * b));
  b := v3(2, 2, 3);
  AssertFalse(IsLinearDependent(a, b));
  AssertFalse(IsLinearDependent(b, a));
  AssertFalse(IsLinearDependent(2 * a, b));
  AssertFalse(IsLinearDependent(a, 2 * b));
End;


{ TMatrix2x2Tests }

Procedure TMatrix2x2Tests.InvertMatrixThatRaisesAnException;
Var
  m, mi: TMatrix2x2;
Begin
  m := M2x2(1, 3, 2, 6);
  mi := uvectormath.InvertMatrix(m);
  // Die Nachfolgende Zeile sorgt nur dafür das Mi und die Erzeugung nicht wegoptimiert werden.
  assertfalse('Das darf nicht passieren.', mi.Equal(mi));
End;

Procedure TMatrix2x2Tests.MarixMultiplication;
Var
  a, b, c, er: TMatrix2x2;
Begin
  a := M2x2(1, 3, 2, 5);
  b := M2x2(6, 8, 7, 10);
  c := MulMatrix(a, b);
  er := M2x2(22, 58, 27, 71);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
  c := MulMatrix(b, a);
  er := M2x2(27, 38, 47, 66);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
End;

Procedure TMatrix2x2Tests.OperatorEqual;
Var
  a, b, c: TMatrix2x2;
Begin
  a := M2x2(1, 3, 2, 5);
  b := M2x2(6, 9, 7, 10);
  c := M2x2(1, 3, 2, 5);
  AssertFalse(a.Equal(b));
  Asserttrue(a.Equal(a));
  Asserttrue(a.Equal(c));
End;

Procedure TMatrix2x2Tests.InvertMatrix;
Var
  zero, id, r, m, mi: TMatrix2x2;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix2x2;
  zero := Zero2x2();
  m := M2x2(1, 3, 2, 5);
  mi := uvectormath.InvertMatrix(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  // Nicht erfolgreicher Versuch
  AssertException(Exception, @InvertMatrixThatRaisesAnException, '');
End;

Procedure TMatrix2x2Tests.InvertMatrix2;
Var
  zero, id, r, m, mi: TMatrix2x2;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix2x2;
  zero := Zero2x2();
  m := M2x2(1, 3, 2, 5);
  mi := uvectormath.InvertMatrix2(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));

  // Nicht invertierbare Matrix -> NullMatrix
  m := M2x2(1, 3, 2, 6);
  mi := uvectormath.InvertMatrix2(m); //
  AssertTrue(Print(zero) + ' = ' + Print(mi), zero.Equal(mi));
End;


{ TMatrix3x3 }

Procedure TMatrix3x3Tests.InvertMatrixThatRaisesAnException;
Var
  m, mi: TMatrix3x3;
Begin
  m := M3x3(1, 4, 7, 2, 5, 8, 3, 6, 9);
  mi := uvectormath.InvertMatrix(m);
  // Die Nachfolgende Zeile sorgt nur dafür das Mi und die Erzeugung nicht wegoptimiert werden.
  assertfalse('Das darf nicht passieren.', mi.Equal(mi));
End;

Procedure TMatrix3x3Tests.MarixMultiplication;
Var
  a, b, c, er: TMatrix3x3;
Begin
  a := M3x3(1, 4, 7, 2, 5, 8, 3, 6, 10);
  b := M3x3(11, 14, 17, 12, 15, 18, 13, 16, 20);
  c := MulMatrix(a, b);
  er := M3x3(90, 216, 359, 96, 231, 384, 105, 252, 419);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
  c := MulMatrix(b, a);
  er := M3x3(150, 186, 229, 186, 231, 284, 235, 292, 359);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
End;

Procedure TMatrix3x3Tests.OperatorEqual;
Var
  a, b, c: TMatrix3x3;
Begin
  a := M3x3(1, 4, 7, 2, 5, 8, 3, 6, 10);
  b := M3x3(11, 14, 17, 12, 15, 18, 13, 16, 20);
  c := M3x3(1, 4, 7, 2, 5, 8, 3, 6, 10);
  AssertFalse(a.Equal(b));
  Asserttrue(a.Equal(a));
  Asserttrue(a.Equal(c));
End;

Procedure TMatrix3x3Tests.InvertMatrix;
Var
  zero, id, r, m, mi: TMatrix3x3;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix3x3;
  zero := Zero3x3();
  m := M3x3(1, 5, 9, 2, -6, 10, 3, 7, 11);
  mi := uvectormath.InvertMatrix(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  // Nicht erfolgreicher Versuch
  AssertException(Exception, @InvertMatrixThatRaisesAnException, '');
End;

Procedure TMatrix3x3Tests.InvertMatrix2;
Var
  zero, id, r, m, mi: TMatrix3x3;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix3x3;
  zero := Zero3x3();
  m := M3x3(1, 5, 9, 2, -6, 10, 3, 7, 11);
  mi := uvectormath.InvertMatrix2(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));

  // Nicht invertierbare Matrix -> NullMatrix
  m := M3x3(1, 4, 7, 2, 5, 8, 3, 6, 9);
  mi := uvectormath.InvertMatrix2(m); //
  AssertTrue(Print(zero) + ' = ' + Print(mi), zero.Equal(mi));
End;

{ TMatrix4x4Tests }

Procedure TMatrix4x4Tests.MarixMultiplication;
Var
  a, b, c, er: TMatrix4x4;
Begin
  a := M4x4(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  b := M4x4(18, 22, 26, 30, 19, 23, 27, 31, 20, 24, 28, 32, 21, 25, 29, 34);
  c := MulMatrix(a, b);
  er := M4x4(260, 644, 1028, 1442, 270, 670, 1070, 1501, 280, 696, 1112, 1560, 294, 730, 1166, 1636);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
  c := MulMatrix(b, a);
  er := M4x4(566, 678, 790, 915, 644, 772, 900, 1042, 722, 866, 1010, 1169, 821, 985, 1149, 1330);
  AssertTrue(Print(er) + ' = ' + Print(c), er.Equal(c));
End;

Procedure TMatrix4x4Tests.OperatorEqual;
Var
  a, b, c: TMatrix4x4;
Begin
  a := M4x4(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  b := M4x4(18, 22, 26, 30, 19, 23, 27, 31, 20, 24, 28, 32, 21, 25, 29, 34);
  c := M4x4(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  AssertFalse(a.Equal(b));
  Asserttrue(a.Equal(a));
  Asserttrue(a.Equal(c));
End;

Procedure TMatrix4x4Tests.InvertMatrix;
Var
  zero, id, r, m, mi: TMatrix4x4;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix4x4;
  zero := Zero4x4();
  m := M4x4(1, 5, 9, 13, 2, -6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  mi := uvectormath.InvertMatrix(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  // Nicht erfolgreicher Versuch
  AssertException(Exception, @InvertMatrixThatRaisesAnException, '');
End;

Procedure TMatrix4x4Tests.InvertMatrix2;
Var
  zero, id, r, m, mi: TMatrix4x4;
Begin
  // Erfolgreicher Versuch
  id := IdentityMatrix4x4;
  zero := Zero4x4();
  m := M4x4(1, 5, 9, 13, 2, -6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  mi := uvectormath.InvertMatrix2(m);
  AssertTrue(Print(zero) + ' <> ' + Print(mi), Not zero.Equal(mi));
  r := MulMatrix(m, mi); // m * mi
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));
  r := MulMatrix(m, mi); // mi * m
  AssertTrue(Print(id) + ' = ' + Print(r), id.Equal(r));

  // Nicht invertierbare Matrix -> NullMatrix
  m := M4x4(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  mi := uvectormath.InvertMatrix2(m); //
  AssertTrue(Print(zero) + ' = ' + Print(mi), zero.Equal(mi));
End;

Procedure TMatrix4x4Tests.InvertMatrixThatRaisesAnException;
Var
  m, mi: TMatrix4x4;
Begin
  m := M4x4(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 17);
  mi := uvectormath.InvertMatrix(m);
  // Die Nachfolgende Zeile sorgt nur dafür das Mi und die Erzeugung nicht wegoptimiert werden.
  assertfalse('Das darf nicht passieren.', mi.Equal(mi));
End;


Procedure TMatrixNxMTests.InitEmpty;
Var
  m: TMatrixNxM;
  i: Integer;
  rows, cols, x, y: Integer;
Begin
  For i := 0 To 100 Do Begin
    rows := random(10) + 1;
    Cols := random(10) + 1;
    m := ZeroNxM(Cols, Rows);
    For x := 0 To Cols - 1 Do Begin
      For y := 0 To rows - 1 Do Begin
        AssertEquals(0, m[x, y]);
      End;
    End;
  End;
End;

Procedure TMatrixNxMTests.InitByElements;
Var
  v: TVectorN;
  m: TMatrixNxM;
  s: String;
Begin
  // 1. Via Vektor
  v := VN([0, 1, 2, 3, 4, 5]);
  m := VNToNxM(v);
  s :=
    '(0.00)' + LineEnding +
    '(1.00)' + LineEnding +
    '(2.00)' + LineEnding +
    '(3.00)' + LineEnding +
    '(4.00)' + LineEnding +
    '(5.00)';
  AssertEquals(s, Print(m));
  // 2. Dim + Via Vektor
  m := MNxM(2, v);
  s :=
    '(0.00 1.00)' + LineEnding +
    '(2.00 3.00)' + LineEnding +
    '(4.00 5.00)';
  AssertEquals(s, Print(m));
  // 3. Via Dim + Elements
  m := MNxM(3, [0, 1, 2, 3, 4, 5]);
  s :=
    '(0.00 1.00 2.00)' + LineEnding +
    '(3.00 4.00 5.00)';
  AssertEquals(s, Print(m));
End;

Procedure TMatrixNxMTests.MatrixToVector;
Var
  m: TMatrixNxM;
  vv, v: TVectorN;
Begin
  m := MNxM(3, [0, 1, 2, 3, 4, 5]);
  vv := NxMToVN(m);
  v := VN([0, 1, 2, 3, 4, 5]);
  AssertTrue(Print(vv) + ' = ' + Print(v), Equal(vv, v));
End;

Procedure TMatrixNxMTests.MarixMultiplication;
Var
  a, b, c, er: TMatrixNxM;
Begin
  // dim 2x3 * 3x4 = 2x4
  a := MNxM(3, vn([1, 2, 3, 4, 5, 7]));
  b := MNxM(4, vn([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13]));
  c := a * b;
  er := MNxM(4, vn([38, 44, 50, 59, 92, 108, 124, 147]));
  AssertTrue(Print(c) + ' = ' + Print(er), Equal(c, er));
  // dim 2x2 * 2x2
  a := MNxM(2, vn([1, 2, 3, 5]));
  b := MNxM(2, vn([6, 7, 8, 10]));
  c := a * b;
  er := MNxM(2, vn([22, 27, 58, 71]));
  AssertTrue(Print(c) + ' = ' + Print(er), Equal(c, er));
  // Reverted Multiplikation
  c := b * a;
  er := MNxM(2, vn([27, 47, 38, 66]));
  AssertTrue(Print(c) + ' = ' + Print(er), Equal(c, er));
End;

Procedure TMatrixNxMTests.OperatorEqual;
Var
  a, b, c, d: TMatrixNxM;
  i, j: Integer;
Begin
  a := ZeroNxM(5, 4);
  b := ZeroNxM(5, 4);
  RandomizeNxM(a);
  RandomizeNxM(b);
  a[0, 0] := b[0, 0] + 1; // Sicherstellen, das die definitiv nie zufällig gleich sind !
  c := ZeroNxM(5, 4);
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[0]) Do Begin
      c[i, j] := a[i, j];
    End;
  End;
  d := ZeroNxM(4, 5);
  AssertFalse(Print(a) + ' = ' + Print(b), Equal(a, b));
  AssertFalse(Print(a) + ' = ' + Print(d), Equal(a, d));
  Asserttrue(Print(a) + ' = ' + Print(a), Equal(a, a));
  Asserttrue(Print(a) + ' = ' + Print(c), Equal(a, c));
End;

Procedure TMatrixNxMTests.LoadFromSaveToStream;
Var
  m: TMemoryStream;
  b, a: TMatrixNxM;
Begin
  m := TMemoryStream.Create;
  a := ZeroNxM(4, 4);
  RandomizeNxM(a);
  SaveMNxMToStream(m, a);
  m.Position := 0;
  b := LoadMNxMFromStream(m);
  AssertTrue(Print(a) + ' = ' + Print(b), Equal(a, b));
  AssertTrue(m.position = m.size);
  m.free;
End;

Initialization
  Randomize;

  DefaultFormatSettings.DecimalSeparator := '.';

  RegisterTest(TGeneralTests);
  RegisterTest(TVector2Tests);
  RegisterTest(TVector3Tests);
  RegisterTest(TMatrix2x2Tests);
  RegisterTest(TMatrix3x3Tests);
  RegisterTest(TMatrix4x4Tests);
  RegisterTest(TMatrixNxMTests);
End.

