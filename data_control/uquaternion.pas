(******************************************************************************)
(* uquaternion.pas                                                 13.05.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit extends uvectormat.pas by quaternion math          *)
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
(* Known Issues:                                                              *)
(*                                                                            *)
(* History     : 0.01                                                         *)
(*                                                                            *)
(* References:                                                                *)
(* https://danceswithcode.net/engineeringnotes/quaternions/quaternions.html   *)
(* https://de.mathworks.com/help/nav/ref/quaternion.html#mw_5e8d12c0-3db1-4db9-8399-4ed59877ec05 *)
(*                                                                            *)
(******************************************************************************)
Unit uquaternion;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uvectormath;

Type

  (*
   * If you get a compiler error with missing file
   * please see uvectormath.pas for how to create uvectormath.inc
   *)
{$I uvectormath.inc}

  // q = q0 + iq1 + jq2 + kq3, with i^2 = j^2 = k^2 = ijk = −1.
  TQuaternion = Record
    Case byte Of
      0: (q0, q1, q2, q3: TBaseType);
      1: (w, x, y, z: TBaseType);
      2: (data: Array[0..3] Of TBaseType);
  End;

{$IFDEF UseOperandOverloading}


Operator + (Const a, b: TQuaternion): TQuaternion;
Operator + (Const a: TQuaternion; Const b: TBaseType): TQuaternion;

Operator - (Const a, b: TQuaternion): TQuaternion;
Operator - (Const a: TQuaternion; Const b: TBaseType): TQuaternion;

Operator * (Const a, b: TQuaternion): TQuaternion;
Operator * (Const a: TQuaternion; Const b: TBaseType): TQuaternion;
Operator * (Const a: TBaseType; Const b: TQuaternion): TQuaternion;

{$ENDIF}

// Konstruktoren
Function Q(q0, q1, q2, q3: TBaseType): TQuaternion;
Function V3ToQ(Const V: TVector3; Const Angle: TBaseType): TQuaternion; // Angle im Gradmaß
Function QToM4x4(Const Q: TQuaternion): TMatrix4x4;
Function M3x3ToQ(Const M3x3: TMatrix3x3): TQuaternion; // Asuming M3x3 is a Rotation Matrix !

// Operatoren
Function Equal(Const a, b: TQuaternion): Boolean; overload;

Function AddQ(Const q1, q2: TQuaternion): TQuaternion;
Function AddScalarToQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;

Function SubQ(Const q1, q2: TQuaternion): TQuaternion;
Function SubScalarfromQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;

Function MulQ(Const q1, q2: TQuaternion): TQuaternion;
Function MulScalarToQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;

Function Conj(Const q1: TQuaternion): TQuaternion;

// Debugg Operationen
Function Print(Const Q: TQuaternion): String;

Implementation

Uses math;

{$IFDEF UseOperandOverloading}

Operator * (Const a, b: TQuaternion): TQuaternion;
Begin
  result := MulQ(a, b);
End;

Operator * (Const a: TQuaternion; Const b: TBaseType): TQuaternion;
Begin
  result := MulScalarToQ(a, b);
End;

Operator * (Const a: TBaseType; Const b: TQuaternion): TQuaternion;
Begin
  result := MulScalarToQ(b, a);
End;

Operator + (Const a, b: TQuaternion): TQuaternion;
Begin
  result := AddQ(a, b);
End;

Operator + (Const a: TQuaternion; Const b: TBaseType): TQuaternion;
Begin
  result := AddScalarToQ(a, b);
End;

Operator - (Const a, b: TQuaternion): TQuaternion;
Begin
  result := SubQ(a, b);
End;

Operator - (Const a: TQuaternion; Const b: TBaseType): TQuaternion;
Begin
  result := SubScalarFromQ(a, b);
End;

{$ENDIF}

Function Q(q0, q1, q2, q3: TBaseType): TQuaternion;
Begin
  result.q0 := q0;
  result.q1 := q1;
  result.q2 := q2;
  result.q3 := q3;
End;

Function V3ToQ(Const V: TVector3; Const Angle: TBaseType): TQuaternion;
Var
  halfAngle, s, c: TBaseType;
Begin
  halfAngle := DegToRad(Angle / 2);
  SinCos(halfAngle, s, c);
  Result.w := c;
  Result.x := v.x * s;
  Result.y := v.y * s;
  Result.z := v.z * s;
End;

Function QToM4x4(Const Q: TQuaternion): TMatrix4x4;
Begin
  Result[0, 0] := 1 - 2 * (q.y * q.y + q.z * q.z);
  Result[0, 1] := 2 * (q.x * q.y - q.z * q.w);
  Result[0, 2] := 2 * (q.x * q.z + q.y * q.w);
  Result[0, 3] := 0;

  Result[1, 0] := 2 * (q.x * q.y + q.z * q.w);
  Result[1, 1] := 1 - 2 * (q.x * q.x + q.z * q.z);
  Result[1, 2] := 2 * (q.y * q.z - q.x * q.w);
  Result[1, 3] := 0;

  Result[2, 0] := 2 * (q.x * q.z - q.y * q.w);
  Result[2, 1] := 2 * (q.y * q.z + q.x * q.w);
  Result[2, 2] := 1 - 2 * (q.x * q.x + q.y * q.y);
  Result[2, 3] := 0;

  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  Result[3, 3] := 1;
End;

// https://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
Function M3x3ToQ(Const M3x3: TMatrix3x3): TQuaternion;
Var
  trace, s: TBaseType;
Begin
  trace := m3x3[0][0] + m3x3[1][1] + m3x3[2][2];
  If trace > 0 Then Begin
    s := 0.5 / sqrt(trace + 1.0);
    Result.w := 0.25 / s;
    Result.x := (m3x3[2][1] - m3x3[1][2]) * s;
    Result.y := (m3x3[0][2] - m3x3[2][0]) * s;
    Result.z := (m3x3[1][0] - m3x3[0][1]) * s;
  End
  Else Begin
    If (m3x3[0][0] > m3x3[1][1]) And (m3x3[0][0] > m3x3[2][2]) Then Begin
      s := 2.0 * sqrt(1.0 + m3x3[0][0] - m3x3[1][1] - m3x3[2][2]);
      Result.w := (m3x3[2][1] - m3x3[1][2]) / s;
      Result.x := 0.25 * s;
      Result.y := (m3x3[0][1] + m3x3[1][0]) / s;
      Result.z := (m3x3[0][2] + m3x3[2][0]) / s;
    End
    Else If m3x3[1][1] > m3x3[2][2] Then Begin
      s := 2.0 * sqrt(1.0 + m3x3[1][1] - m3x3[0][0] - m3x3[2][2]);
      Result.w := (m3x3[0][2] - m3x3[2][0]) / s;
      Result.x := (m3x3[0][1] + m3x3[1][0]) / s;
      Result.y := 0.25 * s;
      Result.z := (m3x3[1][2] + m3x3[2][1]) / s;
    End
    Else Begin
      s := 2.0 * sqrt(1.0 + m3x3[2][2] - m3x3[0][0] - m3x3[1][1]);
      Result.w := (m3x3[1][0] - m3x3[0][1]) / s;
      Result.x := (m3x3[0][2] + m3x3[2][0]) / s;
      Result.y := (m3x3[1][2] + m3x3[2][1]) / s;
      Result.z := 0.25 * s;
    End;
  End;
End;

Function Equal(Const a, b: TQuaternion): Boolean;
Begin
  // TODO: das kann nicht so einfach sein, ...
  result :=
    (abs(a.q0 - b.q0) <= Epsilon) And
    (abs(a.q1 - b.q1) <= Epsilon) And
    (abs(a.q2 - b.q2) <= Epsilon) And
    (abs(a.q3 - b.q3) <= Epsilon);
End;

Function AddQ(Const q1, q2: TQuaternion): TQuaternion;
Begin
  result.q0 := q1.q0 + q2.q0;
  result.q1 := q1.q1 + q2.q1;
  result.q2 := q1.q2 + q2.q2;
  result.q3 := q1.q3 + q2.q3;
End;

Function AddScalarToQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;
Begin
  result := q1;
  result.q0 := result.q0 + s;
End;

Function SubQ(Const q1, q2: TQuaternion): TQuaternion;
Begin
  result.q0 := q1.q0 - q2.q0;
  result.q1 := q1.q1 - q2.q1;
  result.q2 := q1.q2 - q2.q2;
  result.q3 := q1.q3 - q2.q3;
End;

Function SubScalarfromQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;
Begin
  result := q1;
  result.q0 := result.q0 - s;
End;

Function MulQ(Const q1, q2: TQuaternion): TQuaternion;
Begin
  Result.w := q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z;
  Result.x := q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y;
  Result.y := q1.w * q2.y - q1.x * q2.z + q1.y * q2.w + q1.z * q2.x;
  Result.z := q1.w * q2.z + q1.x * q2.y - q1.y * q2.x + q1.z * q2.w;
End;

Function MulScalarToQ(Const q1: TQuaternion; Const S: TBaseType): TQuaternion;
Begin
  result.q0 := q1.q0 * s;
  result.q1 := q1.q1 * s;
  result.q2 := q1.q2 * s;
  result.q3 := q1.q3 * s;
End;

Function Conj(Const q1: TQuaternion): TQuaternion;
Begin
  result.q0 := q1.q0;
  result.q1 := -q1.q1;
  result.q2 := -q1.q2;
  result.q3 := -q1.q3;
End;

Function Print(Const Q: TQuaternion): String;
Begin
  result := format('%0.2f %s%0.2fi %s%0.2fj %s%0.2fk', [
    q.q0,
      BoolToStr(q.q1 > 0, '+', '-'), abs(q.q1),
      BoolToStr(q.q2 > 0, '+', '-'), abs(q.q2),
      BoolToStr(q.q3 > 0, '+', '-'), abs(q.q3)
      ]);
End;

End.

