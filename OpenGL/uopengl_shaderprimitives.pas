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
Procedure glShaderVertex(Const aData: TVector3); overload;
Procedure glShaderVertex(x, y, z: Single); overload;
Procedure glShaderEnd();

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

Procedure glShaderVertex(x, y, z: Single);
Begin
  glShaderVertex(v3(x, y, z));
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

Initialization
  RenderVertexBuffer := Nil;
  setlength(RenderVertexBuffer, VertexBufferBlockSize);
  RenderVertexBufferCnt := 0;

Finalization
  setlength(RenderVertexBuffer, 0);

End.

