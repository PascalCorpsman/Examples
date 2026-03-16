(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of uStableIndexVector                                    *)
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
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uStableIndexVector;

Type

  TCharContainer = specialize TStableIndexVector < Char > ;

  { TTestCase1 }

  TTestCase1 = Class(TTestCase)
  protected
    fDut: TCharContainer;
    Procedure SetUp; override;
    Procedure TearDown; override;
  published
    Procedure EmptyCreate;
    Procedure AddAndAccessElements;
    Procedure AddAndDeleteElements;
    Procedure DelAlreadyDeletedElement;
    Procedure ExchangeValueOfID;
    Procedure IterateAllElements;
    Procedure DelIteratedElement;
  End;

Implementation

Procedure TTestCase1.EmptyCreate;
Begin
  AssertEquals('List not empty after creation', 0, fdut.Count);
End;

Procedure TTestCase1.AddAndAccessElements;
Var
  i: Integer;
Begin
  // Add in ascending order
  For i := 0 To 10 Do Begin
    fDut.Add(chr(ord('A') + i));
  End;
  For i := 0 To 10 Do Begin
    AssertTrue('Written and read value do not match', chr(ord('A') + i) = fdut[i]);
  End;
End;

Procedure TTestCase1.AddAndDeleteElements;
Var
  i: Integer;
Begin
  // Add in ascending order
  For i := 0 To 6 Do Begin
    fDut.Add(chr(ord('A') + i));
  End;
  fDut.Del(4); // Del E
  fDut.Del(1); // Del B
  fDut.Del(5); // Del F
  fDut.Del(0); // Del A
  fDut.Del(3); // Del D
  fDut.Del(2); // Del C
  fDut.Del(6); // Del G
  AssertEquals('not empty', 0, fDut.Count);
End;

Procedure TTestCase1.DelAlreadyDeletedElement;
Var
  i: Integer;
Begin
  // Add in ascending order
  For i := 0 To 6 Do Begin
    fDut.Add(chr(ord('A') + i));
  End;
  AssertTrue('Could not delete existing element', fDut.Del(4)); // Del E
  AssertFalse('Deleted not existing element', fDut.Del(4)); // Del E
  AssertEquals('Deleted to much', 6, fDut.Count);
End;

Procedure TTestCase1.ExchangeValueOfID;
Var
  i: Integer;
Begin
  For i := 0 To 6 Do Begin
    fDut.Add(chr(ord('A') + i));
  End;
  fdut[2] := 'Z'; // C -> Z
  For i := 0 To 6 Do Begin
    If i = 2 Then Begin
      AssertTrue('Z' = fDut[i]);
    End
    Else Begin
      AssertTrue(chr(ord('A') + i) = fDut[i]);
    End;
  End;
End;

Procedure TTestCase1.IterateAllElements;
Var
  i: Integer;
  elem: pChar;
  data: Array['A'..'G'] Of boolean;
Begin
  // Fill and store not visited
  For i := 0 To 6 Do Begin
    fDut.Add(chr(ord('A') + i));
    data[chr(ord('A') + i)] := false;
  End;
  // Visit all elements and store visited
  assertTrue(fDut.IterReset(elem));
  data[elem^] := true;
  While fdut.IterNext(elem) Do Begin
    asserttrue(elem^ In ['A'..'G']);
    data[elem^] := true;
  End;
  // Check all visited
  For i := 0 To 6 Do Begin
    asserttrue(data[chr(ord('A') + i)]);
  End;
End;

Procedure TTestCase1.DelIteratedElement;
Var
  id: TID;
  i: Integer;
  elem: pChar;
  data: Array['A'..'G'] Of boolean;
Begin
  // Fill and store not visited
  For i := 0 To 6 Do Begin
    fDut.Add(chr(ord('A') + i));
    data[chr(ord('A') + i)] := false;
  End;
  // Visit all elements and delete a element in the middle
  assertTrue(fDut.IterReset(elem));
  data[elem^] := true;
  While fdut.IterNext(elem) Do Begin
    asserttrue(elem^ In ['A'..'G']);
    If elem^ = 'C' Then Begin
      i := fDut.PTToIndex(elem);
      assertfalse(i = -1);
      asserttrue(fDut.IndexToID(i, id));
      asserttrue(fDut.Del(id));
    End;
  End;
  // Check all visited
  For i := 0 To fdut.Count - 1 Do Begin
    data[fdut.DataElement[i]] := true;
  End;
  For i := 0 To 6 Do Begin
    If i = 2 Then Begin
      assertfalse(data[chr(ord('A') + i)]);
    End
    Else Begin
      asserttrue(data[chr(ord('A') + i)]);
    End;
  End;
End;

Procedure TTestCase1.SetUp;
Begin
  fDut := TCharContainer.Create(16);
End;

Procedure TTestCase1.TearDown;
Begin
  fDut.free;
End;

Initialization

  RegisterTest(TTestCase1);
End.

