(******************************************************************************)
(* OpenGL Clear Engine                                             ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : All thats needed to start a OpenGL Application               *)
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
(*               0.02 - ADD shader example (needed for GTK3)                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}
{$DEFINE DebuggMode}

(*
 * Activate OpenGL Legacy mode
 *)
{.$DEFINE LEGACYMODE}

{$IFDEF LEGACYMODE}
{$IFDEF LCLGTK3}
{$WARNING OpenGL Legacymode will not work when compiled for GTK3}
{$ENDIF}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  OpenGlcontext,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  //, uopengl_graphikengine // Die OpenGLGraphikengine ist eine Eigenproduktion von www.Corpsman.de, und kann getrennt auf https://github.com/PascalCorpsman/Examples/tree/master/OpenGL geladen werden.
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
{$IFNDEF LEGACYMODE}
  ShaderProgram: GLuint;
  VAO: GLuint; // Vertex Array Object
  VBO: GLuint; // Vertex Buffer Object
{$ENDIF}

Implementation

{$R *.lfm}

{ TForm1 }

Procedure Tform1.Go2d();
{$IFNDEF LEGACYMODE}
Var
  LocRes: GLint;
{$ENDIF}
Begin
{$IFDEF LEGACYMODE}
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
{$ELSE}
  glUseProgram(ShaderProgram);
  LocRes := glGetUniformLocation(ShaderProgram, 'uResolution');
  If LocRes >= 0 Then
    glUniform2f(LocRes, OpenGLControl1.Width, OpenGLControl1.Height);
  glBindVertexArray(VAO);
{$ENDIF}
End;

Procedure Tform1.Exit2d();
Begin
{$IFDEF LEGACYMODE}
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
{$ELSE}
  glBindVertexArray(0);
  glUseProgram(0);
{$ENDIF}
End;

{$IFNDEF LEGACYMODE}
Const
  VertexSrc: PChar =
  '#version 330 core'#10 +
    'layout(location = 0) in vec2 aPos;'#10 +
    'uniform vec2 uResolution;'#10 +
    'void main() {'#10 +
    '  vec2 ndc = vec2((aPos.x / uResolution.x) * 2.0 - 1.0, 1.0 - (aPos.y / uResolution.y) * 2.0);'#10 +
    '  gl_Position = vec4(ndc, 0.0, 1.0);'#10 +
    '}';

  FragmentSrc: PChar =
  '#version 330 core'#10 +
    'out vec4 FragColor;'#10 +
    'void main() {'#10 +
    '  FragColor = vec4(1,0,0,1);'#10 +
    '}';

Function CompileShader(Src: PChar; ShaderType: GLenum): GLuint;
Var
  S: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  result := 0;
  S := glCreateShader(ShaderType);
  glShaderSource(S, 1, @Src, Nil);
  glCompileShader(S);

  glGetShaderiv(S, GL_COMPILE_STATUS, @status);
  If status = 0 Then Begin
    glGetShaderInfoLog(S, 1024, Nil, @Log);
    Raise Exception.Create('Shader Fehler: ' + Log);
  End;

  Result := S;
End;

Function CreateShaderProgram: GLuint;
Var
  vs, fs: GLuint;
  prog: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  vs := CompileShader(VertexSrc, GL_VERTEX_SHADER);
  fs := CompileShader(FragmentSrc, GL_FRAGMENT_SHADER);

  prog := glCreateProgram();
  glAttachShader(prog, vs);
  glAttachShader(prog, fs);
  glLinkProgram(prog);

  glGetProgramiv(prog, GL_LINK_STATUS, @status);
  If status = 0 Then Begin
    glGetProgramInfoLog(prog, 1024, Nil, @Log);
    Raise Exception.Create('Link Fehler: ' + Log);
  End;

  glDeleteShader(vs);
  glDeleteShader(fs);

  Result := prog;
End;
{$ENDIF}

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    {
    OpenGL_GraphikEngine.clear;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);
    }

{$IFNDEF LEGACYMODE}
    If Not Assigned(glCreateShader) Then Begin
      // On Windows it seems that you need to "reload" the core functions for proper function
      ReadExtensions;
      ReadImplementationProperties;
      // if still not available, then halt
      If Not Assigned(glCreateShader) Then Begin
        showmessage('glCreateShader not available, use legacy mode..');
        halt;
      End;
    End;
    ShaderProgram := CreateShaderProgram;
    glGenVertexArrays(1, @VAO);
    glGenBuffers(1, @VBO);
{$ENDIF}

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
{$IFNDEF LEGACYMODE}
Var
  vertices: Array[0..3] Of GLfloat;
{$ENDIF}
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  // { Render etwas ---
{$IFDEF LEGACYMODE}

  glLoadIdentity();
  go2d;
  glcolor3f(1, 0, 0);
  glbegin(gl_lines);
  glvertex3f(10, 10, 0);
  glvertex3f(100, 100, 0);
  glend;
  exit2d;
{$ELSE}
  Go2d;
  vertices[0] := 10.0;
  vertices[1] := 10.0;
  vertices[2] := 100.0;
  vertices[3] := 100.0;
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, Nil);
  glLineWidth(1);
  glDrawArrays(GL_LINES, 0, 2);
  Exit2d;
{$ENDIF}
  //}
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
{$IFDEF LEGACYMODE}
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
{$ELSE}
    If OpenGLControl1.MakeCurrent Then
      glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    OpenGLControl1.Invalidate;
{$ENDIF}
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
{$IFDEF LEGACYMODE}
  caption := 'Legacy mode';
{$ELSE}
  caption := 'Shader mode';
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  VAO := 0;
  VBO := 0;
{$ENDIF}
  Timer1.Interval := 17;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
{$IFNDEF LEGACYMODE}
  If Initialized And OpenGLControl1.MakeCurrent Then Begin
    If ShaderProgram <> 0 Then
      glDeleteProgram(ShaderProgram);
    If VAO <> 0 Then
      glDeleteVertexArrays(1, @VAO);
    If VBO <> 0 Then
      glDeleteBuffers(1, @VBO);
  End;
{$ENDIF}
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
    OpenGLControl1.Invalidate;
{$IFDEF DebuggMode}
    i := glGetError();
    If i <> 0 Then Begin
      Timer1.Enabled := false;
      p := gluErrorString(i);
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + LineEnding + LineEnding +
        'OpenGL Message : "' + p + '"' + LineEnding + LineEnding +
        'Applikation will be terminated.');
      close;
    End;
{$ENDIF}
  End;
End;

End.

