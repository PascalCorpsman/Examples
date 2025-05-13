Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testregistry;

Type

  { TQuaternionTests }

  TQuaternionTests = Class(TTestCase)
  protected
    //Procedure SetUp; override;
    //Procedure TearDown; override;
  published
    Procedure Add();
    Procedure AddScalar();
    Procedure Sub();
    Procedure SubScalar();
    Procedure Mul();
    Procedure MulScalar();
    Procedure IsEqual();
    Procedure Conj();
  End;

Implementation

Uses uquaternion;

Procedure TQuaternionTests.Add;
Var
  q1, q2, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q2 := q(9, 8, 7, 6);
  q3 := q1 + q2;
  AssertTrue(Equal(q3, q(10, 10, 10, 10)));
  q3 := q2 + q1;
  AssertTrue(Equal(q3, q(10, 10, 10, 10)));
End;

Procedure TQuaternionTests.AddScalar;
Var
  q1, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q3 := q1 + 5;
  AssertTrue(Equal(q3, q(6, 2, 3, 4)));
End;

Procedure TQuaternionTests.Sub;
Var
  q1, q2, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q2 := q(9, 8, 7, 6);
  q3 := q1 - q2;
  AssertTrue(Equal(q3, q(-8, -6, -4, -2)));
  q3 := q2 - q1;
  AssertTrue(Equal(q3, q(8, 6, 4, 2)));
End;

Procedure TQuaternionTests.SubScalar;
Var
  q1, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q3 := q1 - 5;
  AssertTrue(Equal(q3, q(-4, 2, 3, 4)));
End;

Procedure TQuaternionTests.Mul;
Var
  q1, q2, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q2 := q(9, 8, 7, 6);
  q3 := q1 * q2;
  AssertTrue(Equal(q3, q(-52, 16, 54, 32)));
  q3 := q2 * q1;
  AssertTrue(Equal(q3, q(-52, 36, 14, 52)));
End;

Procedure TQuaternionTests.MulScalar;
Var
  q1, q3, q4: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q3 := q1 * 5;
  q4 := 5 * q1;
  AssertTrue(Equal(q3, q(5, 10, 15, 20)));
  AssertTrue(Equal(q4, q(5, 10, 15, 20)));
  AssertTrue(Equal(q3, q4));
End;

Procedure TQuaternionTests.IsEqual;
Var
  q1, q2, q3, q4: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q2 := q(9, 8, 7, 6);
  q3 := q1 * q2;
  q4 := q2 * q1;
  AssertFalse(Equal(q3, q4));
  AssertTrue(Equal(q3, q3));
  AssertTrue(Equal(q4, q4));
End;

Procedure TQuaternionTests.Conj();
Var
  q1, q3: TQuaternion;
Begin
  q1 := q(1, 2, 3, 4);
  q3 := uquaternion.Conj(q1);
  AssertTrue(Equal(q3, q(1, -2, -3, -4)));
  AssertTrue(Equal(q1, uquaternion.Conj(q3)));
  AssertTrue(Equal(q1 * uquaternion.Conj(q1), uquaternion.Conj(q1) * q1));
End;

//Procedure TQuaternionTests.SetUp;
//Begin

//End;

//Procedure TQuaternionTests.TearDown;
//Begin

//End;

Initialization

  RegisterTest(TQuaternionTests);
End.

