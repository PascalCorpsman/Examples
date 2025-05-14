(******************************************************************************)
(* uopengl_camera.pas                                              13.05.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implements a OpenGL, 3D camera class                         *)
(*               By defining a Pos, Target and Up Vector, you could afterwords*)
(*               Translate / Rotate the CAM freely.                           *)
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
Unit uopengl_camera;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, dglOpenGL
  , uvectormath;

Type

  { TOpenGLCamera }

  TOpenGLCamera = Class
  private
    fDefPos, fDefTarget, fDefUp: TVector3;
    fPos, fTarget, fup: TVector3;
  public

    Property Target: TVector3 read fTarget;
    Property Pos: TVector3 read fPos;

    Constructor Create(aPos, aTarget, aUp: TVector3);

    (*
     * Call this function right after you cleared all OpenGL Buffers and
     * glLoadIdentity in the OnRender Routine, it will initialize the
     * ModelView Matrix, so that you can directly render the szene.
     *)
    Procedure SetCam;

    (*
     * Resets, Camera Position, Up and Target to the values given on creation
     *)
    Procedure Reset;

    (*
     * Translate Camera and Target, no change in Direction
     *
     * So ganz Sauber sind die beiden Routinen nicht, da sie beide
     * die Kamera Richtung berücksichtigen.
     * Im Zweifel dürfte "TranslateByWorld" die gefühlt Richtigere sein
     *)
    Procedure TranslateByView(x, y, z: Single);
    Procedure TranslateByWorld(x, y, z: Single);

    (*
     * Roates Camera, without Gimbal Lock
     *)
    Procedure Rotate(dx, dy, dz: Single); // Attention: atm, dz is ignored !

    (*
     * Zoom In / Out
     * Effects Cam - Target distance
     * aZoomValue: 1 = No Zoom , < 1 = Zoom in, > 1 = Zoom Out
     * Example: If you zoomed in with 1.1 you need to zoom out with 1/1.1
     *)
    Procedure Zoom(aZoomValue: Single);

    (*
     * Renders a small little gizmo in the right bottom corner ;)
     * If used, call directly after SetCam (
     *)
    Procedure RenderGizmo(aBorder, aWidth, aHeight: Integer; aSize: Single); // Size (= Size) -> -2 = Big, -9 = tiny
  End;

Implementation

Uses uquaternion;

{ TOpenGLCamera }

Procedure TOpenGLCamera.RenderGizmo(aBorder, aWidth, aHeight: Integer; aSize: Single);
Var
  vp: Array[0..3] Of GLint;
  mv: TMatrix4x4;
  lw: GLfloat;
Begin
  // Auslesen der Modelview (wie sie von SetCam gesetzt wurde)
  glGetFloatv(GL_MODELVIEW_MATRIX, @mv);

  // Anpassen des ViewPort für den Gizmo
  glGetIntegerv(GL_VIEWPORT, @vp);
  glViewport(vp[2] - aBorder - aWidth, aBorder, aWidth, aHeight);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  gluPerspective(45.0, 1.0, 0.1, 10.0);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;

  // Rendern des Gizmo

  // 1. Übernehmen der SetCam Rotation Matrix, aber mit "eigener" Position
  mv[3, 0] := 0;
  mv[3, 1] := 0;
  mv[3, 2] := -aSize;
  glMultMatrixf(@mv);

  // Eigentlichen Rendern des Gizmo
  glGetFloatv(GL_LINE_WIDTH, @lw);
  glLineWidth(2.0);

  glBegin(GL_LINES);
  // X – Rot
  glColor3f(1, 0, 0);
  glVertex3f(0, 0, 0);
  glVertex3f(1, 0, 0);
  // Y – Grün
  glColor3f(0, 1, 0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 1, 0);
  // Z – Blau
  glColor3f(0, 0, 1);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, 1);
  glEnd;

  // Zurücksetzen Aller Werte auf Zustand vor Aufruf RenderGizmo
  glLineWidth(lw);
  glPopMatrix;
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glViewport(vp[0], vp[1], vp[2], vp[3]);
End;

Constructor TOpenGLCamera.Create(aPos, aTarget, aUp: TVector3);
Begin
  fDefPos := aPos;
  fDefTarget := aTarget;
  fDefUp := aUp;
End;

Procedure TOpenGLCamera.Reset;
Begin
  fPos := fDefPos;
  fTarget := fDefTarget;
  fup := fDefUp;
End;

Procedure TOpenGLCamera.SetCam;
Begin
  // Hauptkamera setzen
  gluLookAt(
    FPos.x, FPos.y, FPos.z,
    FTarget.x, FTarget.y, FTarget.z,
    fUp.x, fUp.y, fUp.z
    );
End;

Procedure TOpenGLCamera.TranslateByView(x, y, z: Single);
Var
  forward, right, up, move: TVector3;
Begin
  forward := NormV3(fTarget - fPos);
  right := NormV3(CrossV3(forward, fUp));
  up := CrossV3(right, forward);
  move := right * x + up * y + forward * z;
  fPos := fPos + move;
  fTarget := fTarget + move;
End;

Procedure TOpenGLCamera.TranslateByWorld(x, y, z: Single);
Var
  forward, right, up, move: TVector3;
Begin
  forward := NormV3(fTarget - fPos);
  right := NormV3(CrossV3(forward, fUp));
  up := CrossV3(right, forward);
  forward := NormV3(CrossV3(right, fDefUp));
  up := NormV3(fDefUp);
  move := right * x + up * y + forward * z;
  fPos := fPos + move;
  fTarget := fTarget + move;
End;

Procedure TOpenGLCamera.Rotate(dx, dy, dz: Single);
Var
  offset: TVector3;
  pitchAxis, yawAxis: TVector3;
  qPitch, qYaw: TQuaternion;
  rotation: TMatrix4x4;
  forward, right: TVector3;
Begin
  // Richtung vom Ziel zur Kamera
  offset := fPos - fTarget;

  // forward-Vektor (Blickrichtung)
  forward := NormV3(fTarget - fPos);

  // yaw: immer um Welt-Up (z. B. Y-Achse)
  yawAxis := fDefUp;

  // pitch: um lokale X-Achse = Right-Vektor
  right := NormV3(CrossV3(forward, fDefUp));
  pitchAxis := right;

  // Erzeuge Quaternionen aus dynamischen Achsen
  qPitch := V3ToQ(pitchAxis, dx); // Kamera-X (lokal)
  qYaw := V3ToQ(yawAxis, dy); // Welt-Y (global)

  // Kombinierte Rotation
  rotation := QToM4x4(qYaw * qPitch);

  // Versetze Kamera um rotierten Vektor
  fPos := rotation * v4(offset, 0) + fTarget;

  // Optional: Roll unterdrücken durch Up-Neuberechnung
  forward := NormV3(fTarget - fPos);
  right := NormV3(CrossV3(forward, fDefUp));
  fUp := CrossV3(right, forward); // neue Up-Richtung (orthonormal)
End;

Procedure TOpenGLCamera.Zoom(aZoomValue: Single);
Var
  offset: TVector3;
Begin
  aZoomValue := Clamp(aZoomValue, 0.1, 10.0);
  // Richtung vom Ziel zur Kamera
  offset := fPos - fTarget;
  // Skaliere den Offset-Vektor
  offset := offset * aZoomValue;
  // Neue Position
  fPos := fTarget + offset;
End;

End.

