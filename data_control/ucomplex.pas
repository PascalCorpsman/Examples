(******************************************************************************)
(* ucomplex.pas                                                    21.06.2011 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit gives you the ability to use complex numbers       *)
(*               like any other numer in source your code.                    *)
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
(* Version     : 0.01 = initialversion                                        *)
(*               0.02 = Einfügen := für TPoint                                *)
(*                                                                            *)
(******************************************************************************)

Unit ucomplex;

{$MODE objfpc}{$H+}

(*
 * Mit diesem Schalter kann das Überladen der Standart Operatoren Aktiviert
 * werden.
 *)
{$DEFINE UseOperandOverloading}

(*
 * Die Komplexen Zahlen sind im Prinzip ja der R2 mit diesem Compilerswitch
 * werden diverse Konvertierungsroutinen zu uvectormath freigeschaltet.
 *)

{$DEFINE SupportuVectormath}

Interface

Uses
  Classes, SysUtils, math
{$IFDEF SupportuVectormath}
  , uvectormath
{$ENDIF}
  ;

Type

{$IFNDEF SupportuVectormath}
  TBaseType = Single;
{$ENDIF}
  TComplex = Record
    re: TBaseType;
    im: TBaseType;
  End;

Const
  Epsilon: TBaseType = 0.00001; // Alle Werte die sich um weniger als Epsilon unterscheiden werden, als gleich betrachtet

{$IFDEF UseOperandOverloading}
  Operator + (a, b: TComplex): TComplex;
  Operator - (a, b: TComplex): TComplex;
  Operator * (a, b: TComplex): TComplex;
  Operator / (a, b: TComplex): TComplex;

  Operator := (a: TBaseType): TComplex;
  Operator := (a: Tpoint): TComplex;
  Operator := (a: TComplex): Tpoint;
  Operator * (a: TBaseType; b: TComplex): TComplex;
  Operator * (b: TComplex; a: TBaseType): TComplex;

{$IFDEF SupportuVectormath}
  Operator := (a: TVector2): TComplex;
  Operator := (a: TComplex): TVector2;
{$ENDIF}

{$ENDIF}

  // Konvertierungsfunktionen
Function Complex(Re, Im: TBaseType): TComplex; // Konvertieren a + bI in eine Komplexe Zahl
Function Complex2(R, Phi: TBaseType): TComplex; // Konvertieren r * e^(phiI) in eine Komplexe Zahl ( Phi im Bogenmas )
Function GetR(C: TComplex): TBaseType; // Radius der Zahl
Function GetPhi(C: TComplex): TBaseType; // Winkel im Bogenmas
Procedure GetR_Phi(C: TComplex; Out R: TBaseType; Out Phi: TBaseType); // Braucht man R und Phi, so ist diese Routine Schneller
{$IFDEF SupportuVectormath}
Function toV2(c: TComplex): TVector2;
Function toComplex(a: TVector2): TComplex;
{$ENDIF}

// Todo : Rechenoperationen der 3. Stufe  siehe : http://de.wikipedia.org/wiki/Komplexe_Zahl

// Grundrechenarten
Function KonjugateC(Const C: TComplex): TComplex;
Function SqrC(Const C: TComplex): TComplex;
Function SqrtC(Const C: TComplex): TComplex;
Function AddC(Const C1, C2: TComplex): TComplex;
Function SubC(Const C1, C2: TComplex): TComplex;
Function MulC(Const C1, C2: TComplex): TComplex;
Function DivC(Const C1, C2: TComplex): TComplex;
Function ScaleC(Const S: TBaseType; C: TComplex): TComplex;

Implementation

{$IFDEF UseOperandOverloading}

Operator + (a, b: TComplex): TComplex;
Begin
  result := AddC(a, b);
End;

Operator - (a, b: TComplex): TComplex;
Begin
  result := SubC(a, b);
End;

Operator * (a, b: TComplex): TComplex;
Begin
  result := MulC(a, b);
End;

Operator / (a, b: TComplex): TComplex;
Begin
  result := DivC(a, b);
End;

Operator := (a: TBaseType): TComplex;
Begin
  result.re := a;
  result.im := 0;
End;

Operator := (a: TPoint): TComplex;
Begin
  result := Complex(a.x, a.y);
End;

Operator := (a: TComplex): Tpoint;
Begin
  result.x := round(a.re);
  result.y := round(a.im);
End;

Operator * (a: TBaseType; b: TComplex): TComplex;
Begin
  result := ScaleC(a, b);
End;

Operator * (b: TComplex; a: TBaseType): TComplex;
Begin
  result := ScaleC(a, b);
End;

{$IFDEF SupportuVectormath}
Operator := (a: TVector2): TComplex;
Begin
  result := toComplex(a);
End;

Operator := (a: TComplex): TVector2;
Begin
  result := toV2(a);
End;
{$ENDIF}

{$ENDIF}

Function Complex(Re, Im: TBaseType): TComplex;
Begin
  result.re := re;
  result.im := im;
End;

Function Complex2(R, Phi: TBaseType): TComplex;
Begin
  result.re := cos(phi) * r;
  result.im := sin(phi) * r;
End;

Function GetR(C: TComplex): TBaseType;
Begin
  result := sqrt(sqr(c.im) + sqr(c.re));
End;

Function GetPhi(C: TComplex): TBaseType;
Begin
  If GetR(c) < Epsilon Then Begin
    result := 1 / 0; // Kein Winkel bestimmbar, wir Lösen eine DivByZero Exception aus.
  End;
  result := arctan2(c.im, c.re);
  If result < 0 Then result := result + 2 * pi;
End;

Procedure GetR_Phi(C: TComplex; Out R: TBaseType; Out Phi: TBaseType);
Begin
  r := GetR(c);
  If r < Epsilon Then Begin
    r := 1 / 0; // Kein Winkel bestimmbar, wir Lösen eine DivByZero Exception aus.
  End;
  phi := arctan2(c.im, c.re);
  If phi < 0 Then phi := phi + 2 * pi;
End;

{$IFDEF SupportuVectormath}

Function toV2(c: TComplex): TVector2;
Begin
  result.x := c.re;
  result.y := c.im;
End;

Function toComplex(a: TVector2): TComplex;
Begin
  result.re := a.x;
  result.im := a.y;
End;
{$ENDIF}

Function KonjugateC(Const C: TComplex): TComplex;
Begin
  result.re := c.re;
  result.im := -c.im;
End;

Function SqrC(Const C: TComplex): TComplex;
Begin
  result.re := sqr(c.re) - sqr(c.im);
  result.im := 2 * c.re * c.im;
End;

Function SqrtC(Const C: TComplex): TComplex;
Var
  r, Phi: TBaseType;
Begin
  (*
   * Die Komplexe Quadratwurzel ist definiert als
   *
   *  sqrt(c) = sqrt(r) * e^( ( i phi + 2kpi) / 2 ) mit k = 0,1
   *
   * Hat also 2 Mögliche Lösungen, hier ist die Variante für k = 0 gewählt.
   *)
  GetR_Phi(c, r, phi);
  result := Complex2(sqrt(r), (phi {+ 2 * k * pi}) / 2);
End;

Function AddC(Const C1, C2: TComplex): TComplex;
Begin
  result.re := c1.re + c2.re;
  result.im := c1.im + c2.im;
End;

Function SubC(Const C1, C2: TComplex): TComplex;
Begin
  result.re := c1.re - c2.re;
  result.im := c1.im - c2.im;
End;

Function MulC(Const C1, C2: TComplex): TComplex;
Begin
  result.re := c1.re * c2.re - c1.im * c2.im;
  result.im := c1.re * c2.im + c1.im * c2.re;
End;

Function DivC(Const C1, C2: TComplex): TComplex;
Var
  nenner: TBaseType;
Begin
  nenner := sqr(c2.re) + sqr(c2.im);
  If Nenner < Epsilon Then Begin
    result.re := 1 / 0; // Wir Lösen eine DivByZero Exception aus.
  End;
  result.re := (c1.re * c2.re + c1.im * c2.im) / Nenner;
  result.im := (c1.im * c2.re - c1.re * c2.im) / Nenner;
End;

Function ScaleC(Const S: TBaseType; C: TComplex): TComplex;
Begin
  result.re := s * c.re;
  result.im := s * c.im;
End;

End.

