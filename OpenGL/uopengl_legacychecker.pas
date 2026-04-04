(******************************************************************************)
(* uopengl_legacychecker.pas                                       03.04.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Gives a helper function to more easy check if a application  *)
(*               uses old legacy OpenGL functions.                            *)
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
Unit uopengl_legacychecker;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

(*
 * The following function shall help porting legacy code to new OpenGL v3.3
 * by calling this function during MakeCurrent you register a callback that is
 * called, whenever a "old" legacy function is called.
 * When placing a breakpoint in the callback you will be able to stacktrace
 * the source of the call.
 *)

Procedure RegisterLegacyCheckerCallback(Const aCallback: TNotifyEvent);

Implementation

Uses dglOpenGL;

Var
  fCallback: TNotifyEvent = Nil; // Storage for the registered Callback ;)

  glVertex2dcapture: TglVertex2d;
  glVertex2fcapture: TglVertex2f;
  glVertex2icapture: TglVertex2i;
  glVertex2ivcapture: TglVertex2iv;
  glVertex2dvcapture: TglVertex2dv;
  glVertex2fvcapture: TglVertex2fv;

  glVertex3fcapture: TglVertex3f;
  glVertex3dcapture: TglVertex3d;
  glVertex3icapture: TglVertex3i;
  glVertex3fvcapture: TglVertex3fv;
  glVertex3ivcapture: TglVertex3iv;
  glVertex3dvcapture: TglVertex3dv;

  glVertex4fcapture: TglVertex4f;
  glVertex4icapture: TglVertex4i;
  glVertex4dcapture: TglVertex4d;
  glVertex4fvcapture: TglVertex4fv;
  glVertex4ivcapture: TglVertex4iv;
  glVertex4dvcapture: TglVertex4dv;

  glColor3fcapture: TglColor3f;
  glColor3icapture: TglColor3i;
  glColor3ubcapture: TglColor3ub;
  glColor3fvcapture: TglColor3fv;
  glColor3ivcapture: TglColor3iv;
  glColor3ubvcapture: TglColor3ubv;

  glColor4fcapture: TglColor4f;
  glColor4ubcapture: TglColor4ub;
  glColor4icapture: TglColor4i;
  glColor4fvcapture: TglColor4fv;
  glColor4ivcapture: TglColor4iv;
  glColor4ubvcapture: TglColor4ubv;

  glTexCoord2dcapture: TglTexCoord2d;
  glTexCoord2icapture: TglTexCoord2i;
  glTexCoord2fcapture: TglTexCoord2f;
  glTexCoord2fvcapture: TglTexCoord2fv;
  glTexCoord2ivcapture: TglTexCoord2iv;
  glTexCoord2dvcapture: TglTexCoord2dv;

  glNormal3dcapture: TglNormal3d;
  glNormal3icapture: TglNormal3i;
  glNormal3fcapture: TglNormal3f;
  glNormal3fvcapture: TglNormal3fv;
  glNormal3ivcapture: TglNormal3iv;
  glNormal3dvcapture: TglNormal3dv;

  glBegincapture: TglBegin;
  glEndcapture: TglEnd;
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

  glEnableClientStatecapture: TglEnableClientState;
  glDisableClientStatecapture: TglDisableClientState;
  glVertexPointercapture: TglVertexPointer;
  glColorPointercapture: TglColorPointer;
  glNormalPointercapture: TglNormalPointer;
  glTexCoordPointercapture: TglTexCoordPointer;

  glNewListcapture: TglNewList;
  glEndListcapture: TglEndList;
  glCallListcapture: TglCallList;
  glGenListscapture: TglGenLists;
  glDeleteListscapture: TglDeleteLists;

  glTexEnvcapture: TglTexEnvf;

Procedure glVertex2fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex2fvcapture(v);
End;

Procedure glVertex3fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex3fvcapture(v);
End;

Procedure glVertex4fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glVertex4fvcapture(v);
End;

Procedure glVertex2ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glVertex2ivcapture(v);
End;

Procedure glVertex3ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glVertex3ivcapture(v);
End;

Procedure glVertex4ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glVertex4ivcapture(v);
End;

Procedure glVertex2dvWatcher(Const v: PGLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex2dvcapture(v);
End;

Procedure glVertex3dvWatcher(Const v: PGLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex3dvcapture(v);
End;

Procedure glVertex4dvWatcher(Const v: PGLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex4dvcapture(v);
End;


Procedure glColor3fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glColor3fvcapture(v);
End;

Procedure glColor4fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glColor4fvcapture(v);
End;

Procedure glColor3ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glColor3ivcapture(v);
End;

Procedure glColor4ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glColor4ivcapture(v);
End;

Procedure glColor3ubvWatcher(Const v: PGLubyte); cdecl;
Begin
  fCallback(Nil);
  glColor3ubvcapture(v);
End;

Procedure glColor4ubvWatcher(Const v: PGLubyte); cdecl;
Begin
  fCallback(Nil);
  glColor4ubvcapture(v);
End;

Procedure glTexCoord2fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2fvcapture(v);
End;

Procedure glTexCoord2ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2ivcapture(v);
End;

Procedure glTexCoord2dvWatcher(Const v: PGLdouble); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2dvcapture(v);
End;

Procedure glNormal3fvWatcher(Const v: PGLfloat); cdecl;
Begin
  fCallback(Nil);
  glNormal3fvcapture(v);
End;

Procedure glNormal3ivWatcher(Const v: PGLint); cdecl;
Begin
  fCallback(Nil);
  glNormal3ivcapture(v);
End;

Procedure glNormal3dvWatcher(Const v: PGLdouble); cdecl;
Begin
  fCallback(Nil);
  glNormal3dvcapture(v);
End;

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

Procedure glEnableClientStateWatcher(array_: GLenum); cdecl;
Begin
  fCallback(Nil);
  glEnableClientStatecapture(array_);
End;

Procedure glDisableClientStateWatcher(array_: GLenum); cdecl;
Begin
  fCallback(Nil);
  glDisableClientStatecapture(array_);
End;

Procedure glVertexPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid); cdecl;
Begin
  fCallback(Nil);
  glVertexPointercapture(size, _type, stride, _pointer);
End;

Procedure glColorPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid); cdecl;
Begin
  fCallback(Nil);
  glColorPointercapture(size, _type, stride, _pointer);
End;

Procedure glNormalPointerWatcher(_type: GLenum; stride: GLsizei; Const _pointer: PGLvoid); cdecl;
Begin
  fCallback(Nil);
  glNormalPointercapture(_type, stride, _pointer);
End;

Procedure glTexCoordPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid); cdecl;
Begin
  fCallback(Nil);
  glTexCoordPointercapture(size, _type, stride, _pointer);
End;

Procedure glNewListWatcher(list: GLuint; mode: GLenum); cdecl;
Begin
  fCallback(Nil);
  glNewListcapture(list, mode);
End;

Procedure glEndListWatcher(); cdecl;
Begin
  fCallback(Nil);
  glEndListcapture();
End;

Procedure glCallListWatcher(list: GLuint); cdecl;
Begin
  fCallback(Nil);
  glCallListcapture(list);
End;

Function glGenListsWatcher(range: GLsizei): GLuint; cdecl;
Begin
  fCallback(Nil);
  result := glGenListscapture(range);
End;

Procedure glDeleteListsWatcher(list: GLuint; range: GLsizei); cdecl;
Begin
  fCallback(Nil);
  glDeleteListscapture(list, range);
End;

Procedure glTexEnvfWatcher(target: GLenum; pname: GLenum; param: GLfloat); cdecl;
Begin
  fCallback(Nil);
  glTexEnvcapture(target, pname, param);
End;

Procedure glVertex2iWatcher(x: GLint; y: GLint); cdecl;
Begin
  fCallback(Nil);
  glVertex2icapture(x, y);
End;

Procedure glVertex3iWatcher(x: GLint; y: GLint; z: GLint); cdecl;
Begin
  fCallback(Nil);
  glVertex3icapture(x, y, z);
End;

Procedure glVertex4iWatcher(x: GLint; y: GLint; z: GLint; w: GLint); cdecl;
Begin
  fCallback(Nil);
  glVertex4icapture(x, y, z, w);
End;

Procedure glVertex2dWatcher(x: GLdouble; y: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex2dcapture(x, y);
End;

Procedure glVertex3dWatcher(x: GLdouble; y: GLdouble; z: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex3dcapture(x, y, z);
End;

Procedure glVertex4dWatcher(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glVertex4dcapture(x, y, z, w);
End;

Procedure glColor3ubWatcher(r: GLubyte; g: GLubyte; b: GLubyte); cdecl;
Begin
  fCallback(Nil);
  glColor3ubcapture(r, g, b);
End;

Procedure glColor4ubWatcher(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte); cdecl;
Begin
  fCallback(Nil);
  glColor4ubcapture(r, g, b, a);
End;

Procedure glColor3iWatcher(r: GLint; g: GLint; b: GLint); cdecl;
Begin
  fCallback(Nil);
  glColor3icapture(r, g, b);
End;

Procedure glColor4iWatcher(r: GLint; g: GLint; b: GLint; a: GLint); cdecl;
Begin
  fCallback(Nil);
  glColor4icapture(r, g, b, a);
End;

Procedure glTexCoord2dWatcher(s: GLdouble; t: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2dcapture(s, t);
End;

Procedure glTexCoord2iWatcher(s: GLint; t: GLint); cdecl;
Begin
  fCallback(Nil);
  glTexCoord2icapture(s, t);
End;

Procedure glNormal3dWatcher(nx: GLdouble; ny: GLdouble; nz: GLdouble); cdecl;
Begin
  fCallback(Nil);
  glNormal3dcapture(nx, ny, nz);
End;

Procedure glNormal3iWatcher(nx: GLint; ny: GLint; nz: GLint); cdecl;
Begin
  fCallback(Nil);
  glNormal3icapture(nx, ny, nz);
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
  glEnableClientStatecapture := glEnableClientState;
  glEnableClientState := @glEnableClientStateWatcher;
  glDisableClientStatecapture := glDisableClientState;
  glDisableClientState := @glDisableClientStateWatcher;
  glVertexPointercapture := glVertexPointer;
  glVertexPointer := @glVertexPointerWatcher;
  glColorPointercapture := glColorPointer;
  glColorPointer := @glColorPointerWatcher;
  glNormalPointercapture := glNormalPointer;
  glNormalPointer := @glNormalPointerWatcher;
  glTexCoordPointercapture := glTexCoordPointer;
  glTexCoordPointer := @glTexCoordPointerWatcher;
  glNewListcapture := glNewList;
  glNewList := @glNewListWatcher;
  glEndListcapture := glEndList;
  glEndList := @glEndListWatcher;
  glCallListcapture := glCallList;
  glCallList := @glCallListWatcher;
  glGenListscapture := glGenLists;
  glGenLists := @glGenListsWatcher;
  glDeleteListscapture := glDeleteLists;
  glDeleteLists := @glDeleteListsWatcher;
  glTexEnvcapture := glTexEnvf;
  glTexEnvf := @glTexEnvfWatcher;
  glVertex2icapture := glVertex2i;
  glVertex2i := @glVertex2iWatcher;
  glVertex3icapture := glVertex3i;
  glVertex3i := @glVertex3iWatcher;
  glVertex4icapture := glVertex4i;
  glVertex4i := @glVertex4iWatcher;
  glVertex2dcapture := glVertex2d;
  glVertex2d := @glVertex2dWatcher;
  glVertex3dcapture := glVertex3d;
  glVertex3d := @glVertex3dWatcher;
  glVertex4dcapture := glVertex4d;
  glVertex4d := @glVertex4dWatcher;
  glColor3ubcapture := glColor3ub;
  glColor3ub := @glColor3ubWatcher;
  glColor4ubcapture := glColor4ub;
  glColor4ub := @glColor4ubWatcher;
  glColor3icapture := glColor3i;
  glColor3i := @glColor3iWatcher;
  glColor4icapture := glColor4i;
  glColor4i := @glColor4iWatcher;
  glTexCoord2dcapture := glTexCoord2d;
  glTexCoord2d := @glTexCoord2dWatcher;
  glTexCoord2icapture := glTexCoord2i;
  glTexCoord2i := @glTexCoord2iWatcher;
  glNormal3dcapture := glNormal3d;
  glNormal3d := @glNormal3dWatcher;
  glNormal3icapture := glNormal3i;
  glNormal3i := @glNormal3iWatcher;

  glVertex2fvcapture := glVertex2fv;
  glVertex2fv := @glVertex2fvWatcher;
  glVertex3fvcapture := glVertex3fv;
  glVertex3fv := @glVertex3fvWatcher;
  glVertex4fvcapture := glVertex4fv;
  glVertex4fv := @glVertex4fvWatcher;

  glVertex2ivcapture := glVertex2iv;
  glVertex2iv := @glVertex2ivWatcher;
  glVertex3ivcapture := glVertex3iv;
  glVertex3iv := @glVertex3ivWatcher;
  glVertex4ivcapture := glVertex4iv;
  glVertex4iv := @glVertex4ivWatcher;

  glVertex2dvcapture := glVertex2dv;
  glVertex2dv := @glVertex2dvWatcher;
  glVertex3dvcapture := glVertex3dv;
  glVertex3dv := @glVertex3dvWatcher;
  glVertex4dvcapture := glVertex4dv;
  glVertex4dv := @glVertex4dvWatcher;

  glColor3fvcapture := glColor3fv;
  glColor3fv := @glColor3fvWatcher;
  glColor4fvcapture := glColor4fv;
  glColor4fv := @glColor4fvWatcher;

  glColor3ivcapture := glColor3iv;
  glColor3iv := @glColor3ivWatcher;
  glColor4ivcapture := glColor4iv;
  glColor4iv := @glColor4ivWatcher;

  glColor3ubvcapture := glColor3ubv;
  glColor3ubv := @glColor3ubvWatcher;
  glColor4ubvcapture := glColor4ubv;
  glColor4ubv := @glColor4ubvWatcher;

  glTexCoord2fvcapture := glTexCoord2fv;
  glTexCoord2fv := @glTexCoord2fvWatcher;
  glTexCoord2ivcapture := glTexCoord2iv;
  glTexCoord2iv := @glTexCoord2ivWatcher;
  glTexCoord2dvcapture := glTexCoord2dv;
  glTexCoord2dv := @glTexCoord2dvWatcher;

  glNormal3fvcapture := glNormal3fv;
  glNormal3fv := @glNormal3fvWatcher;
  glNormal3ivcapture := glNormal3iv;
  glNormal3iv := @glNormal3ivWatcher;
  glNormal3dvcapture := glNormal3dv;
  glNormal3dv := @glNormal3dvWatcher;
End;

End.

