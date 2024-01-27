Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ugraphics, Graphics, IntfGraphics;

Type

  { TTestCase1 }

  TTestCase1 = Class(TTestCase)
  private
    Lena1, Lena2: TBitmap;
    Lena1_intf, Lena2_intf: TLazIntfImage;
    Procedure CheckLenaEqual;
  protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  published
    Procedure TeadUpDown_Valid;
    Procedure Rotate_90_Grad;
    Procedure Rotate_Counter_90_Grad;
    Procedure Rotate_90_for_and_back;
    Procedure Rotate_90_for_and_back_2;
    Procedure Rotate_180;
    Procedure Rotate_180_2;
    Procedure UpDown;
    Procedure LeftRight;
    Procedure Flip_Rotate;
  End;

Implementation

Procedure TTestCase1.Rotate_90_Grad;
Begin

  RotateClockWise90Degrees(Lena1);
  RotateClockWise90Degrees(Lena1);
  RotateClockWise90Degrees(Lena1);
  RotateClockWise90Degrees(Lena1);

  CheckLenaEqual();
End;

Procedure TTestCase1.Rotate_Counter_90_Grad;
Begin
  RotateCounterClockWise90Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena1);
  CheckLenaEqual();
End;

Procedure TTestCase1.Rotate_90_for_and_back;
Begin
  RotateClockWise90Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena1);
  CheckLenaEqual();
End;

Procedure TTestCase1.Rotate_90_for_and_back_2;
Begin
  RotateClockWise90Degrees(Lena1);
  RotateClockWise90Degrees(Lena1);
  RotateClockWise90Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena2);
  CheckLenaEqual();
End;

Procedure TTestCase1.Rotate_180;
Begin
  Rotate180Degrees(Lena1);
  Rotate180Degrees(Lena1);
  CheckLenaEqual();
End;

Procedure TTestCase1.Rotate_180_2;
Begin
  Rotate180Degrees(Lena1);
  RotateCounterClockWise90Degrees(Lena2);
  RotateCounterClockWise90Degrees(Lena2);
  CheckLenaEqual();
End;

Procedure TTestCase1.UpDown;
Begin
  UpSideDown(Lena1);
  UpSideDown(Lena1);
  CheckLenaEqual();
End;

Procedure TTestCase1.LeftRight;
Begin
  LeftToRight(Lena1);
  LeftToRight(Lena1);
  CheckLenaEqual();
End;

Procedure TTestCase1.Flip_Rotate;
Begin
  UpSideDown(Lena1);
  LeftToRight(Lena1);
  Rotate180Degrees(Lena2);
  CheckLenaEqual();
End;

Procedure TTestCase1.CheckLenaEqual;
Var
  i, j: Integer;
Begin
  AssertEquals('Error, dimension after rotate wrong.', Lena1.Width, Lena2.Width);
  AssertEquals('Error, dimension after rotate wrong.', Lena1.Height, Lena2.Height);
  Lena1_intf := TLazIntfImage.Create(0, 0);
  Lena1_intf.LoadFromBitmap(Lena1.Handle, Lena1.MaskHandle);
  Lena2_intf := TLazIntfImage.Create(0, 0);
  Lena2_intf.LoadFromBitmap(Lena2.Handle, Lena2.MaskHandle);

  For i := 0 To Lena1.Width - 1 Do
    For j := 0 To Lena1.Height - 1 Do Begin
      AssertTrue('Eror, pixel data invalid', FPColorToColor(Lena1_intf.Colors[i, j]) = FPColorToColor(Lena2_intf.Colors[i, j]))
    End;
End;

Procedure TTestCase1.SetUp;
Begin
  Lena1 := TBitmap.Create;
  If Not FileExists('..' + PathDelim + 'Lena.bmp') Then Begin
    AssertTrue('..' + PathDelim + 'Lena.bmp does not exist.', false);
  End;
  Lena1.LoadFromFile('..' + PathDelim + 'Lena.bmp');
  lena2 := TBitmap.Create;
  lena2.Assign(lena1);
End;

Procedure TTestCase1.TearDown;
Begin
  Lena1_intf.Free;
  Lena2_intf.Free;
  Lena1.Free;
  Lena2.Free;
End;

Procedure TTestCase1.TeadUpDown_Valid;
Begin
  // Nichts, testet nur ob Setup und Teardown alles richtig machen.
  CheckLenaEqual();
End;

Initialization

  RegisterTest(TTestCase1);
End.

