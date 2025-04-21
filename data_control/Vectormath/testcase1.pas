Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testregistry, uvectormath;

Type

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

  { TOtherTests }

  TOtherTests = Class(TTestCase)
  protected
  published
    Procedure Convolve1D;
    Procedure ConvolveAVG;
  End;

Implementation

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

{ TOtherTests }

Procedure TOtherTests.Convolve1D;
Var
  a, b, c: TVectorN;
Begin
  (*
   * Falten mit "nicht symmetrischem" filter kern
   *)
  a := VN([1, 2, 3]);
  b := VN([4, 5, 6]);
  c := Convolve(a, b);
  AssertEquals('Invalid len', 5, length(c));
  AssertEquals('Invalid result [0]', 4, c[0]);
  AssertEquals('Invalid result [1]', 13, c[1]);
  AssertEquals('Invalid result [2]', 28, c[2]);
  AssertEquals('Invalid result [3]', 27, c[3]);
  AssertEquals('Invalid result [4]', 18, c[4]);
End;

Procedure TOtherTests.ConvolveAVG;
Var
  a, b, c: TVectorN;
Begin
  (*
   * Faltung als Mittelwert filter
   *)
  a := vn([0, 0, 10, 10, 10, 0, 0]);
  b := vn([0.5, 0.5]);
  c := Convolve(a, b);
  AssertEquals('Invalid len', 8, length(c));
  AssertEquals('Invalid result [0]', 0, c[0]);
  AssertEquals('Invalid result [1]', 0, c[1]);
  AssertEquals('Invalid result [2]', 5, c[2]);
  AssertEquals('Invalid result [3]', 10, c[3]);
  AssertEquals('Invalid result [4]', 10, c[4]);
  AssertEquals('Invalid result [5]', 5, c[5]);
  AssertEquals('Invalid result [6]', 0, c[6]);
  AssertEquals('Invalid result [7]', 0, c[7]);
End;

Initialization
  Randomize;
  DefaultFormatSettings.DecimalSeparator := '.';

  RegisterTest(TMatrix2x2Tests);
  RegisterTest(TMatrix3x3Tests);
  RegisterTest(TMatrix4x4Tests);
  RegisterTest(TMatrixNxMTests);
  RegisterTest(TOtherTests);

End.

