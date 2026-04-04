(******************************************************************************)
(* uopengl_shaderprimitives.pas                                    03.04.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Helper functions to port "old" OpenGl legacy code to new     *)
(*               OpenGL Shaded code, use together with                        *)
(*                 uOpenGLGraphikEngine.pas                                   *)
(*               ! Attention !                                                *)
(*               This unit tries to reduce porting effort and is not          *)
(*               "performant" or how you should use shaders in new projects   *)
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

Unit uopengl_shaderprimitives;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, dglOpenGL, uvectormath;

(*
 * Call once during Make Current
 *)
Procedure OpenGL_ShaderPrimitives_InitializeShaderSystem;

(*
 * Call once during destroy
 *)
Procedure OpenGL_ShaderPrimitives_FinalizeShaderSystem;

(*
 * replacement for:
 *
 *  glColor3f(..); // -- Optional, but usually done..
 *  glBegin(aMode);
 *  glvertex(..);
 *  glEnd;
 *
 * How to use:
 *
 * Case 1:
 *  UseColorShader();
 *  SetShaderColor(..); // -- Optional, but usually done..
 *  glShaderBegin(aMode);
 *  glShaderRender([
 *   v3(), ...
 *   ]);
 *  glShaderEnd();
 *  UseTextureShader; // Switch back to Render Textures
 *
 * Case 2:
 *  UseColorShader();
 *  SetShaderColor(..); // -- Optional, but usually done..
 *  glShaderBegin(aMode);
 *  glShaderVertex(..); // As often as needed
 *  glShaderEnd();
 *  UseTextureShader; // Switch back to Render Textures
 *)

Procedure glShaderBegin(aMode: GLenum);
Procedure glShaderRender(Const aData: Array Of TVector3);
Procedure glShaderVertex(Const aData: TVector3);
Procedure glShaderEnd();

(*
 * The following function shall help porting legacy code to new OpenGL v3.3
 * by calling this function during MakeCurrent you register a callback that is
 * called, whenever a "old" legacy function is called.
 * When placing a breakpoint in the callback you will be able to stacktrace
 * the source of the call.
 *)

Procedure RegisterLegacyCheckerCallback(Const aCallback: TNotifyEvent);

Implementation

Const
  VertexBufferBlockSize = 1024;

Var
  ShaderVBO: GLint;
  aShaderMode: GLenum;
  RenderVertexBuffer: Array Of TVector3;
  RenderVertexBufferCnt: integer;

Procedure OpenGL_ShaderPrimitives_InitializeShaderSystem;
Begin
  If ShaderVBO = 0 Then
    glGenBuffers(1, @ShaderVBO);
End;

Procedure OpenGL_ShaderPrimitives_FinalizeShaderSystem;
Begin
  If ShaderVBO <> 0 Then
    glDeleteBuffers(1, @ShaderVBO);
  ShaderVBO := 0;
End;

Procedure glShaderBegin(aMode: GLenum);
Begin
  aShaderMode := aMode;
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, Nil);
  RenderVertexBufferCnt := 0;
End;

Procedure glShaderRender(Const aData: Array Of TVector3);
Begin
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * length(aData), @aData[0], GL_DYNAMIC_DRAW);
  glDrawArrays(aShaderMode, 0, length(aData));
End;

Procedure glShaderVertex(Const aData: TVector3);
Begin
  RenderVertexBuffer[RenderVertexBufferCnt] := aData;
  inc(RenderVertexBufferCnt);
  If RenderVertexBufferCnt > high(RenderVertexBuffer) Then Begin
    setlength(RenderVertexBuffer, length(RenderVertexBuffer) + VertexBufferBlockSize);
  End;
End;

Procedure glShaderEnd();
Begin
  // Die glShaderRender routine wurde nicht benutzt, also hier den VertexBuffer übergeben
  If RenderVertexBufferCnt <> 0 Then Begin
    glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * RenderVertexBufferCnt, @RenderVertexBuffer[0], GL_DYNAMIC_DRAW);
    glDrawArrays(aShaderMode, 0, RenderVertexBufferCnt);
    RenderVertexBufferCnt := 0;
  End;
  glDisableVertexAttribArray(0);
End;

Var
  fCallback: TNotifyEvent = Nil;
  glBegincapture: TglBegin;
  glEndcapture: TglEnd;
  glVertex2fcapture: TglVertex2f;
  glVertex3fcapture: TglVertex3f;
  glVertex4fcapture: TglVertex4f;
  glColor3fcapture: TglColor3f;
  glColor4fcapture: TglColor4f;
  glNormal3fcapture: TglNormal3f;
  glTexCoord2fcapture: TglTexCoord2f;
  glPushMatrixcapture: TglPushMatrix;
  glPopMatrixcapture: TglPopMatrix;
  glPushAttribcapture: TglPushAttrib;
  glPopAttribcapture: TglPopAttrib;
  glMatrixModecapture: TglMatrixMode;
  glLoadIdentitycapture: TglLoadIdentity;
  glLoadMatrixfcapture: TglLoadMatrixf;
  glMultMatrixfcapture: TglMultMatrixf;
  glRotatefcapture: TglRotatef;
  glTranslatefcapture: TglTranslatef;
  glScalefcapture: TglScalef;
  glOrthocapture: TglOrtho;
  glFrustumcapture: TglFrustum;

Procedure glBeginWatcher(mode: GLenum); cdecl;
Begin
  fCallback(Nil);
  glBegincapture(mode);
End;

Procedure glEndWatcher(); cdecl;
Begin
  fCallback(Nil);
  glEndcapture();
End;

Procedure glVertex2fWatcher(x: GLfloat; y: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex2fcapture(x, y);
End;

Procedure glVertex3fWatcher(x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex3fcapture(x, y, z);
End;

Procedure glVertex4fWatcher(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex4fcapture(x, y, z, w);
End;

Procedure glColor3fWatcher(red: GLfloat; green: GLfloat; blue: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glColor3fcapture(red, green, blue);
End;

Procedure glColor4fWatcher(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glColor4fcapture(red, green, blue, alpha);
End;

Procedure glNormal3fWatcher(nx: GLfloat; ny: GLfloat; nz: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glNormal3fcapture(nx, ny, nz);
End;

Procedure glTexCoord2fWatcher(s: GLfloat; t: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2fcapture(s, t);
End;

Procedure glPushMatrixWatcher(); cdecl;
Begin
  fCallback(Nil);
  glPushMatrixcapture();
End;

Procedure glPopMatrixWatcher(); cdecl;
Begin
  fCallback(Nil);
  glPopMatrixcapture();
End;

Procedure glPushAttribWatcher(mask: GLbitfield); cdecl;
Begin
  fCallback(Nil);
  glPushAttribcapture(mask);
End;

Procedure glPopAttribWatcher(); cdecl;
Begin
  fCallback(Nil);
  glPopAttribcapture();
End;

Procedure glMatrixModeWatcher(mode: GLenum); cdecl;
Begin
  fCallback(Nil);
  glMatrixModecapture(mode);
End;

Procedure glLoadIdentityWatcher(); cdecl;
Begin
  fCallback(Nil);
  glLoadIdentitycapture();
End;

Procedure glLoadMatrixfWatcher(Const m: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glLoadMatrixfcapture(m);
End;

Procedure glMultMatrixfWatcher(Const m: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glMultMatrixfcapture(m);
End;

Procedure glRotatefWatcher(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glRotatefcapture(angle, x, y, z);
End;

Procedure glTranslatefWatcher(x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glTranslatefcapture(x, y, z);
End;

Procedure glScalefWatcher(x: GLfloat; y: GLfloat; z: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glScalefcapture(x, y, z);
End;

Procedure glOrthoWatcher(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glOrthocapture(left, right, bottom, top, zNear, zFar);
End;

Procedure glFrustumWatcher(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glFrustumcapture(left, right, bottom, top, zNear, zFar);
End;

Procedure RegisterLegacyCheckerCallback(Const aCallback: TNotifyEvent);
Begin
  fCallback := aCallback;
  // Here is a list of hopefully "All" gl* calls which are not allowed when not
  // using OpenGL ver 3.3 and up
  glBegincapture := glBegin;
  glBegin := @glBeginWatcher;
  glEndcapture := glEnd;
  glEnd := @glEndWatcher;
  glVertex2fcapture := glVertex2f;
  glVertex2f := @glVertex2fWatcher;
  glVertex3fcapture := glVertex3f;
  glVertex3f := @glVertex3fWatcher;
  glVertex4fcapture := glVertex4f;
  glVertex4f := @glVertex4fWatcher;
  glColor3fcapture := glColor3f;
  glColor3f := @glColor3fWatcher;
  glColor4fcapture := glColor4f;
  glColor4f := @glColor4fWatcher;
  glNormal3fcapture := glNormal3f;
  glNormal3f := @glNormal3fWatcher;
  glTexCoord2fcapture := glTexCoord2f;
  glTexCoord2f := @glTexCoord2fWatcher;
  glPushMatrixcapture := glPushMatrix;
  glPushMatrix := @glPushMatrixWatcher;
  glPopMatrixcapture := glPopMatrix;
  glPopMatrix := @glPopMatrixWatcher;
  glPushAttribcapture := glPushAttrib;
  glPushAttrib := @glPushAttribWatcher;
  glPopAttribcapture := glPopAttrib;
  glPopAttrib := @glPopAttribWatcher;
  glMatrixModecapture := glMatrixMode;
  glMatrixMode := @glMatrixModeWatcher;
  glLoadIdentitycapture := glLoadIdentity;
  glLoadIdentity := @glLoadIdentityWatcher;
  glLoadMatrixfcapture := glLoadMatrixf;
  glLoadMatrixf := @glLoadMatrixfWatcher;
  glMultMatrixfcapture := glMultMatrixf;
  glMultMatrixf := @glMultMatrixfWatcher;
  glRotatefcapture := glRotatef;
  glRotatef := @glRotatefWatcher;
  glTranslatefcapture := glTranslatef;
  glTranslatef := @glTranslatefWatcher;
  glScalefcapture := glScalef;
  glScalef := @glScalefWatcher;
  glOrthocapture := glOrtho;
  glOrtho := @glOrthoWatcher;
  glFrustumcapture := glFrustum;
  glFrustum := @glFrustumWatcher;
End;

Initialization
  RenderVertexBuffer := Nil;
  setlength(RenderVertexBuffer, VertexBufferBlockSize);
  RenderVertexBufferCnt := 0;

Finalization
  setlength(RenderVertexBuffer, 0);

End.

