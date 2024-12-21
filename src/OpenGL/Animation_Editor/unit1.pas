(******************************************************************************)
(* Animation Editor                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.09                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Graphikal frontend to create and use TAnimation              *)
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
(*               0.02 - Unterstützen FrameOffset                              *)
(*                      Kleinere Optische Korrekturen                         *)
(*               0.03 - Optische Preview der Winkel                           *)
(*                      FIX AV on Close                                       *)
(*               0.04 - Anzeigen Preview Image                                *)
(*               0.05 - Automatisches vorschlagen Renderwidth / Height        *)
(*                      Automatisches übernehmen von Abgeleiteten Graphik-    *)
(*                      daten                                                 *)
(*               0.06 - FIX: Name der Sprites wurde bei Änderung nicht        *)
(*                           gespeichert                                      *)
(*                      FIX: Rendering unter Linux kaputt                     *)
(*                      FIX: div by 0 Error                                   *)
(*               0.07 - FIX: off by 1 error by defining Framecount via mouse  *)
(*                      Remove unused subimages                               *)
(*               0.08 - ADD: drop .ani files on app                           *)
(*               0.09 - FIX: Preview image was not correct                    *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}
{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, FPImage, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus, ExtDlgs, OpenGlcontext,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // see https://github.com/PascalCorpsman/Examples#dependencies
  , uopengl_graphikengine // see https://github.com/PascalCorpsman/Examples/tree/master/OpenGL
  , uopengl_animation // see https://github.com/PascalCorpsman/Examples/tree/master/OpenGL
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    OpenPictureDialog1: TOpenPictureDialog;
    PaintBox1: TPaintBox;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Shape1: TShape;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure Edit10Change(Sender: TObject);
    Procedure Edit11Change(Sender: TObject);
    Procedure Edit12Change(Sender: TObject);
    Procedure Edit13Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit5KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit7Change(Sender: TObject);
    Procedure Edit8Change(Sender: TObject);
    Procedure Edit9Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ListBox1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Ani: TOpenGL_Animation;
    fFilename: String;
    Procedure AniToLCL();
    Procedure GuessZoom;
    Procedure LoadAniFile(Const Filename: String);
  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

Var
  defcaption: String;
  Form1ShowOnce: Boolean = true;
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

Uses math, uopengl_spriteengine, LCLType, IntfGraphics, ugraphics, GraphType, Unit2, uvectormath;

{ TForm1 }

Procedure TForm1.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure TForm1.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

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

    OpenGL_GraphikEngine.clear;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);

    Ani := TOpenGL_Animation.Create;
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
    MenuItem2Click(Nil); // New

    If Form1ShowOnce Then Begin
      Form1ShowOnce := false;
      If ParamCount = 1 Then Begin
        If FileExists(ParamStr(1)) Then Begin
          LoadAniFile(ParamStr(1));
        End;
      End;
    End;
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  s: Single;
  c: TRGB;
Begin
  If Not Initialized Then Exit;
  // Render Szene
  c := ColorToRGB(Shape1.Brush.Color);
  glClearColor(c.r / 255, c.g / 255, c.b / 255, 0.0);
  //glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  go2d;
  glPushMatrix();
  s := ScrollBar2.Position / 10;
  glScalef(s, s, 1);
  glTranslatef(10 / s, 10 / s, 0);
  Ani.Render(ScrollBar1.Position);
  //If CheckBox3.Checked Then Begin
  //  If Ani.Sprite[0].SpriteIndex <> -1 Then Begin
  //    RenderAlphaQuad(point(32, 32), 64, -64, 0,
  //      OpenGL_SpriteEngine.Sprite[Ani.Sprite[0].SpriteIndex].Image
  //      );
  //  End;
  //End;
  glPopMatrix();
  exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
  End;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  ep: TPoint;
  c, s: extended;
Begin
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Rectangle(-1, -1, 33, 33);
  PaintBox1.Canvas.Pen.Color := clRed;
  sincos(degtorad(-ScrollBar1.Position {- strtofloatdef(edit2.text, 0.0)}), s, c);
  ep.x := round(16 + 16 * c);
  ep.Y := round(16 + 16 * s);
  RenderArrow(PaintBox1.Canvas, point(16, 16), ep);
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
  Function mod2(value: Single): Single;
  Begin
    result := value;
    While result > 360 Do
      result := result - 360;
    While result < 0 Do
      result := result + 360;
  End;
Begin
  //  label3.caption := format('= %d', [ScrollBar1.Position + round(strtofloatdef(edit2.text, 0.0))]);
  If assigned(ani) Then Ani.AngleOffset := strtofloatdef(edit2.text, 0.0);
  label3.caption := format('= %d -> %0.1f = ', [ScrollBar1.Position, mod2(ScrollBar1.Position + strtofloatdef(edit2.text, 0.0))]);
  PaintBox1.Invalidate;
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  label16.caption := format('= %0.1f', [ScrollBar2.Position / 10]);
End;

Procedure TForm1.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If button = mbleft Then Begin
    If ColorDialog1.Execute Then Begin
      Shape1.Brush.Color := ColorDialog1.Color;
    End;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  defcaption := 'Animation Editor ver. 0.09 by Corpsman';
  caption := defcaption;
  Application.Title := Caption;
  edit2.text := format('%0.1f', [0.0]);
  Ani := Nil;
  ScrollBar1Change(Nil); // Init Label Angle
  ScrollBar2Change(Nil); // Init Label Scale
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
  Timer1.Interval := 17;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxHeight := Height;
  Constraints.MaxWidth := Width;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Var
  i: Integer;
Begin
  For i := 0 To high(FileNames) Do Begin
    If lowercase(ExtractFileExt(FileNames[i])) = '.ani' Then Begin
      If Ani.Changed Then Begin
        If ID_YES = Application.MessageBox('Animation was changed, but not saved, do you want to save now ?', 'Warning', MB_YESNO Or MB_ICONQUESTION) Then Begin
          MenuItem3Click(Nil);
        End;
      End;
      LoadAniFile(FileNames[i]);
    End;
  End;
End;

Procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  s: TAniSprite;
  index, w, h: integer;
  xs, ys: integer;
  scaleinv, scale: Extended;
Begin
  (*
   * Beim Click auf das Bild wird der entsprechende FrameOffset Berechnet und ins passende Edit Feld geschrieben
   *)
  If ListBox1.ItemIndex = -1 Then exit;
  s := Ani.Sprite[ListBox1.ItemIndex];
  (*
   * Umrechnen in Sprite Koordinaten
   *
   * das ist ein bisschen auffwendiger, da das Bild ja Propertional gestretched wurde ;)
   *)
  w := (s.Rect.Right - s.Rect.Left);
  h := (s.Rect.Bottom - s.Rect.Top);
  If (h = 0) Or (w = 0) Then exit;
  If w / h > Image1.Width / Image1.Height Then Begin
    // Waagrecht gestretched
    scale := Image1.Width / w;
    scaleinv := w / Image1.Width;
    xs := round(ConvertDimension(0, Image1.Width, x, s.Rect.Left, s.Rect.Right));
    ys := round((y - (image1.Height - round(h * scale)) Div 2) * scaleinv);
  End
  Else Begin
    // Senkrecht Gestretched
    scale := Image1.Height / h;
    scaleinv := h / Image1.Height;
    xs := round((x - (image1.Width - round(w * scale)) Div 2) * scaleinv);
    ys := round(ConvertDimension(0, Image1.Height, y, s.Rect.Top, s.Rect.Bottom));
  End;
  // Umrechnen der X,Y-Koordinaten in den passenden Image Index
  w := (s.Rect.Right - s.Rect.Left) Div strtoint(edit11.text);
  h := (s.Rect.Bottom - s.Rect.Top) Div strtoint(edit10.text);
  If (h = 0) Or (w = 0) Then exit;
  index := xs Div w + (ys Div h) * strtoint(edit11.text);
  If ssleft In shift Then Begin
    edit13.text := inttostr(index);
  End
  Else Begin
    edit9.text := inttostr(index - strtointdef(edit13.text, 0) + 1); // +1 weil das ja ein Count ist und kein Index !
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // Wird die Anwendung Geschlossen, bevor der Rendering Context erstellt werden kann Knallts hier sonst..
  Initialized := false;
  If assigned(ani) Then Begin
    ani.free;
    ani := Nil;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Add Range
  Ani.AddRange();
  // LCL schon mal hin Faken, damit das nachher passt ;)
  ListBox1.Items.add('');
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  AniToLCL();
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Remove Range
  If Ani.SpriteCount = 1 Then Begin
    showmessage('Last Range, can not be deleted.');
    exit;
  End;
  If ListBox1.ItemIndex <> -1 Then Begin
    Ani.DeleteSprite(ListBox1.ItemIndex);
    ListBox1.ItemIndex := min(ListBox1.Items.Count - 2, ListBox1.ItemIndex);
    AniToLCL();
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  p: TPortableNetworkGraphic;
  s: TAniSprite;
  b, b2: TBitmap;
  Source, Dest: TLazIntfImage;
  oi, i, j: Integer;
  DestColor, CurColor: TFPColor;
  newRect, oldRect: TRect;
Begin
  // Load Image for Sprite
  If ListBox1.ItemIndex = -1 Then exit;
  If OpenPictureDialog1.Execute Then Begin
    oldrect := rect(0, 0, 0, 0);
    newRect := Rect(0, 0, 0, 0);
    s := ani.Sprite[ListBox1.ItemIndex];
    Case lowercase(extractfileext(OpenPictureDialog1.FileName)) Of
      '.bmp': Begin
          If assigned(s.AlphaMask) Then s.AlphaMask.free;
          s.AlphaMask := Nil;
          If Not assigned(s.Bitmap) Then s.Bitmap := TBitmap.Create;
          s.Bitmap.LoadFromFile(OpenPictureDialog1.FileName);
          oldRect := s.rect;
          s.Rect := Rect(0, 0, s.Bitmap.Width, s.Bitmap.Height);
          newRect := s.Rect;
          ani.Sprite[ListBox1.ItemIndex] := s;
        End;
      '.png': Begin
          // laden des PNG und aufsplitten des Bildes in "Bild" und "Alphamaske"
          p := TPortableNetworkGraphic.Create;
          p.LoadFromFile(OpenPictureDialog1.FileName);
          b := TBitmap.Create;
          b.Assign(p);
          b2 := TBitmap.Create;
          b2.Width := b.Width;
          b2.Height := b.Height;
          Source := TLazIntfImage.Create(0, 0);
          Source.LoadFromBitmap(b.Handle, b.MaskHandle);
          Dest := TLazIntfImage.Create(0, 0);
          Dest.LoadFromBitmap(b2.Handle, b2.MaskHandle);
          For i := 0 To b.Width - 1 Do Begin
            For j := 0 To b.Height - 1 Do Begin
              // Separieren des Alpha Kanals
              CurColor := Source.Colors[i, j];
              (*
               * So Drehen dass Schwarz = Transparent
               *                Weiß = Opak
               *)
              DestColor.red := (255 - CurColor.alpha Div 256) * 256;
              DestColor.green := (255 - CurColor.alpha Div 256) * 256;
              DestColor.blue := (255 - CurColor.alpha Div 256) * 256;
              DestColor.alpha := 255 * 256;
              Dest.Colors[i, j] := DestColor;
              // Reset des Alpha Kanals in der Quelle
              CurColor.alpha := 255 * 256;
              Source.Colors[i, j] := CurColor;
            End;
          End;
          b.LoadFromIntfImage(source);
          b2.LoadFromIntfImage(dest);
          Source.Free;
          Dest.free;
          s.Bitmap := b;
          s.AlphaMask := b2;
          oldRect := s.rect;
          s.Rect := Rect(0, 0, s.Bitmap.Width, s.Bitmap.Height);
          newRect := s.Rect;
          s.AlphaImage := true;
          ani.Sprite[ListBox1.ItemIndex] := s;
        End;
    Else Begin
        showmessage('Error, filetype not jet supported.');
      End;
    End;
    If ((oldRect.Right <> newRect.Right) Or
      (oldRect.Bottom <> newRect.Bottom)) And (Ani.SpriteCount > 1) Then Begin
      If ID_YES = Application.MessageBox('The image dimension changed, would you like to apply this dimension to all its derivatives?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then Begin
        oi := ListBox1.ItemIndex;
        AniToLCL();
        Button4.Click; // übernehmen der Dimensionen
        Button5.Click; // Textur neu erstellen
        For i := ListBox1.ItemIndex + 1 To ani.SpriteCount - 1 Do Begin
          s := ani.Sprite[i];
          If s.Derived Then Begin
            s.Rect := newRect;
            ani.Sprite[i] := s;
            ListBox1.ItemIndex := i;
            edit3.text := inttostr(newRect.Top);
            edit4.text := inttostr(newRect.Right);
            edit5.text := inttostr(newRect.Bottom);
            edit6.text := inttostr(newRect.Left);
            Button4.Click; // übernehmen der Dimensionen
            Button5.Click; // Textur neu erstellen
          End
          Else Begin
            // Keine weiteren Abgeleiteten mehr -> Raus
            break;
          End;
        End;
        ListBox1.ItemIndex := oi;
      End;
    End;
    AniToLCL();
    Button4.Click; // übernehmen der Dimensionen
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  s: TAniSprite;
  b: TBitmap;
Begin
  // Set Rect
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ani.Sprite[ListBox1.ItemIndex];
    b := ani.GetBitmapOf(ListBox1.ItemIndex);
    If Not assigned(b) Then exit;
    s.Rect.Left := strtointdef(edit6.text, 0);
    s.Rect.Top := strtointdef(edit3.text, 0);
    s.Rect.Right := strtointdef(edit4.text, b.Width);
    s.Rect.Bottom := strtointdef(edit5.text, b.Height);
    If s.FramesPerCol <> 0 Then Begin
      s.Height := (s.Rect.Bottom - s.Rect.Top) Div s.FramesPerCol;
    End
    Else Begin
      s.Height := (s.Rect.Bottom - s.Rect.Top);
    End;
    If s.FramesPerRow <> 0 Then Begin
      s.Width := (s.Rect.Right - s.Rect.Left) Div s.FramesPerRow;
    End
    Else Begin
      s.Width := (s.Rect.Right - s.Rect.Left);
    End;
    ani.Sprite[ListBox1.ItemIndex] := s;
    AniToLCL();
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  ani.RemoveOpenGLData(ListBox1.ItemIndex, true); // Löschen der OpenGL-Textur, damit diese bei Änderung ggf neu geladen werden kann.
  If Not ani.CreateOpenGLData(ListBox1.ItemIndex) Then Begin
    showmessage('Error could not create OpenGL preview:' + LineEnding + ani.LastError);
    exit;
  End;
  GuessZoom;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  ScrollBar2.Position := 10;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  a, b, d: TBitmap;
  p: TPortableNetworkGraphic;
  Source, Alpha, Dest: TLazIntfImage;
  SColor, AColor: TFPColor;
  i, j: Integer;
Begin
  i := ani.GetDerivedIndexOf(ListBox1.ItemIndex);
  If assigned(ani.Sprite[i].Bitmap) Then Begin
    If assigned(ani.Sprite[i].AlphaMask) Then Begin
      // -> Ziel ist ein PNG, es werden Alpha und Bitmap gemerged und als PNG gespeichert.
      b := TBitmap.Create;
      b.Assign(ani.Sprite[i].Bitmap); // Das Quellbild ohne Alpha
      a := TBitmap.Create;
      a.Assign(ani.Sprite[i].AlphaMask); // Der Alphakanal des Quellbildes
      If (a.Width <> b.Width) Or (a.Height <> b.Height) Then Begin
        ShowMessage('Error alphamask and normal image differ in size.');
        a.free;
        b.Free;
        exit;
      End;
      d := TBitmap.Create;
      d.Width := a.Width;
      d.Height := a.Height;

      source := TLazIntfImage.Create(0, 0);
      Source.LoadFromBitmap(b.Handle, b.MaskHandle);

      Alpha := TLazIntfImage.Create(0, 0);
      Alpha.LoadFromBitmap(a.Handle, a.MaskHandle);

      Dest := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
      Dest.SetSize(a.Width, a.Height);

      For i := 0 To a.Width - 1 Do Begin
        For j := 0 To a.Height - 1 Do Begin
          SColor := Source.Colors[i, j];
          AColor := Alpha.Colors[i, j];
          SColor.alpha := (255 - AColor.red Shr 8) Shl 8; // Egal ist ja eh Graustufen
          Dest.Colors[i, j] := SColor;
        End;
      End;
      d.LoadFromIntfImage(Dest);
      p := TPortableNetworkGraphic.Create;
      p.Assign(d);
      SaveDialog2.Filter := 'Portable Network File|*.png|All Files|*.*';
      SaveDialog2.DefaultExt := '.png';
      SaveDialog2.FileName := '';
      If SaveDialog2.Execute Then Begin
        p.SaveToFile(SaveDialog2.FileName);
      End;
      p.free;
      d.free;
      a.Free;
      b.Free;
      source.free;
      Alpha.free;
      Dest.free;
    End
    Else Begin
      // -> Ziel ist ein BMP
      SaveDialog2.Filter := 'Bitmap File|*.bmp|All Files|*.*';
      SaveDialog2.DefaultExt := '.bmp';
      SaveDialog2.FileName := '';
      If SaveDialog2.Execute Then Begin
        ani.Sprite[i].Bitmap.SaveToFile(SaveDialog2.FileName);
      End;
    End;
  End
  Else Begin
    showmessage('Error no image to export found.');
  End;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  b := Ani.GetFirstBitmap();
  form2.LoadPreview(b);
  b.free;
  form2.ShowModal;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  If ID_YES = application.MessageBox('Removing unused subimages will delete content of all images, this can not be undone. Do this only at the very end of the creating process and have a bakup of the images in case you want to extend your animation at a later point. Are you shure you want to continue ?', 'Warning', MB_YESNO Or MB_ICONEXCLAMATION) Then Begin
    If Ani.RemoveUnusedSubImagex() Then Begin
      AniToLCL();
      ListBox1.ItemIndex := 0;
      ListBox1Click(Nil);
      GuessZoom();
    End;
  End;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Var
  s: TAniSprite;
  b: TBitmap;
  di: integer;
Begin
  If ListBox1.ItemIndex > 0 Then Begin
    s := ani.Sprite[ListBox1.ItemIndex];
    If CheckBox1.Checked Then Begin
      // Wenn Abgeleitet wird, dann Geben wir ein evtl geladenes Bild wieder frei
      If assigned(s.Bitmap) Then s.bitmap.free;
      If assigned(s.AlphaMask) Then s.AlphaMask.free;
      s.Bitmap := Nil;
      s.AlphaMask := Nil;
      If Not s.Derived Then Begin
        // Das Bild war bisher noch nicht "Abgeleitet" -> Rect neu Initialisieren
        s.Derived := CheckBox1.Checked;
        ani.Sprite[ListBox1.ItemIndex] := s;
        b := ani.GetBitmapOf(ListBox1.ItemIndex);
        If assigned(b) Then Begin
          s.Rect := Rect(0, 0, b.Width, b.Height);
          di := ani.GetDerivedIndexOf(ListBox1.ItemIndex);
          s.AlphaImage := ani.Sprite[di].AlphaImage;
          s.Width := ani.Sprite[di].Width;
          s.Height := ani.Sprite[di].Height;
          s.FrameCount := ani.Sprite[di].FrameCount;
          s.FramesPerCol := ani.Sprite[di].FramesPerCol;
          s.FramesPerRow := ani.Sprite[di].FramesPerRow;
          s.TimePerFrame := ani.Sprite[di].TimePerFrame;
        End
        Else Begin
          s.rect := rect(0, 0, 0, 0);
        End;
      End;
    End;
    // Übernehmen des "Derived" damit es beim GetBitmap Of schon zur Verfügung steht
    s.Derived := CheckBox1.Checked;
    ani.Sprite[ListBox1.ItemIndex] := s;
  End;
  AniToLCL();
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Var
  sa: TAniSprite;
Begin
  // Set Alpha Image
  If ListBox1.ItemIndex = -1 Then exit;
  sa := ani.Sprite[ListBox1.ItemIndex];
  If sa.AlphaImage <> CheckBox2.Checked Then Begin
    sa.AlphaImage := CheckBox2.Checked;
    ani.Sprite[ListBox1.ItemIndex] := sa;
  End;
End;

Procedure TForm1.Edit10Change(Sender: TObject);
Var
  s: TAniSprite;
  st: String;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.FramesPerCol := strtointdef(Tedit(Sender).Text, s.FramesPerCol);
  Ani.Sprite[ListBox1.ItemIndex] := s;
  If s.FramesPerCol <> 0 Then Begin
    st := inttostr((s.Rect.Bottom - s.Rect.Top) Div s.FramesPerCol);
    If st <> '0' Then
      edit8.text := st;
  End;
End;

Procedure TForm1.Edit11Change(Sender: TObject);
Var
  s: TAniSprite;
  st: String;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.FramesPerRow := strtointdef(Tedit(Sender).Text, s.FramesPerRow);
  Ani.Sprite[ListBox1.ItemIndex] := s;
  If s.FramesPerRow <> 0 Then Begin
    st := inttostr((s.Rect.Right - s.Rect.Left) Div s.FramesPerRow);
    If st <> '0' Then
      edit7.text := st;
  End;
End;

Procedure TForm1.Edit12Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.TimePerFrame := strtointdef(Tedit(Sender).Text, s.TimePerFrame);
  Ani.Sprite[ListBox1.ItemIndex] := s;
End;

Procedure TForm1.Edit13Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.FrameOffset := strtointdef(Tedit(Sender).Text, s.FrameOffset);
  Ani.Sprite[ListBox1.ItemIndex] := s;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.Name := Edit1.text;
  Ani.Sprite[ListBox1.ItemIndex] := s;
  ListBox1.Items[ListBox1.ItemIndex] := edit1.text;
End;

Procedure TForm1.Edit5KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then button4.Click;
End;

Procedure TForm1.Edit7Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.Width := strtointdef(Tedit(Sender).Text, s.Width);
  Ani.Sprite[ListBox1.ItemIndex] := s;
End;

Procedure TForm1.Edit8Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.Height := strtointdef(Tedit(Sender).Text, s.Height);
  Ani.Sprite[ListBox1.ItemIndex] := s;
End;

Procedure TForm1.Edit9Change(Sender: TObject);
Var
  s: TAniSprite;
Begin
  s := Ani.Sprite[ListBox1.ItemIndex];
  s.FrameCount := strtointdef(Tedit(Sender).Text, s.FrameCount);
  Ani.Sprite[ListBox1.ItemIndex] := s;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // New
  ani.free;
  ani := TOpenGL_Animation.Create;
  fFilename := '';
  AniToLCL;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Save
  If fFilename = '' Then Begin
    MenuItem4Click(Nil);
    exit;
  End;
  If Not Ani.SaveToFile(fFileName) Then Begin
    ShowMessage('Error, could not store: ' + fFileName);
  End
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Save As
  If SaveDialog1.Execute Then Begin
    If Ani.SaveToFile(SaveDialog1.FileName) Then Begin
      fFilename := SaveDialog1.FileName;
      caption := defcaption + ': ' + ExtractFileName(fFilename);
    End
    Else Begin
      ShowMessage('Error, could not store: ' + SaveDialog1.FileName);
    End;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Load
  If Ani.Changed Then Begin
    If ID_YES = Application.MessageBox('Animation was changed, but not saved, do you want to save now ?', 'Warning', MB_YESNO Or MB_ICONQUESTION) Then Begin
      MenuItem3Click(Nil);
    End;
  End;
  If OpenDialog1.Execute Then Begin
    LoadAniFile(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  // Quit
  Close;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
{$IFDEF Windows}
    OpenGLControl1.Invalidate;
{$ELSE}
    // Why the heck does invalidate not work under Linux ?
    OpenGLControl1.DoOnPaint;
{$ENDIF}
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

Procedure TForm1.AniToLCL();
Var
  liIndex, i: integer;
Begin
  liIndex := ListBox1.ItemIndex;
  ListBox1.Items.Clear;
  For i := 0 To ani.SpriteCount - 1 Do Begin
    ListBox1.Items.add(ani.Sprite[i].Name);
  End;
  If (liIndex >= 0) And (liIndex < ListBox1.Items.Count) Then Begin
    ListBox1.ItemIndex := liIndex;
  End
  Else Begin
    ListBox1.ItemIndex := 0;
  End;
  ListBox1.Click; // Refresh des angezeigten Sprites
  Edit2.Text := format('%0.3f', [ani.AngleOffset]);
End;

Procedure TForm1.GuessZoom;
Var
  f: Single;
  h: integer;
  s: TAniSprite;
Begin
  // Versuch den Srollbar so zu skallieren, dass "alles" Sichtbar ist.
  s := ani.Sprite[ListBox1.ItemIndex];
  h := s.Height;
  f := ((OpenGLControl1.Height - 10) * 10) / h;
  ScrollBar2.Position := round(f);
End;

Procedure TForm1.LoadAniFile(Const Filename: String);
Begin
  If ani.LoadFromFile(FileName) Then Begin
    fFilename := FileName;
    caption := defcaption + ': ' + ExtractFileName(fFilename);
    AniToLCL();
    ListBox1.ItemIndex := 0;
    ListBox1Click(Nil);
    GuessZoom();
  End
  Else Begin
    showmessage('Error could not open file: ' + FileName + LineEnding + ani.LastError);
  End;
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Var
  sb, b, c: TBitmap;
  Sprite: TAniSprite;
  Derivedindex: integer;
Begin
  // Laden eines Sprites in die Vorschau
  If ListBox1.ItemIndex <> -1 Then Begin
    CheckBox1.Visible := ListBox1.ItemIndex > 0; // Show derived "option" if image is not the first in list
    Sprite := Ani.Sprite[ListBox1.ItemIndex];
    CheckBox1.Checked := Sprite.Derived;
    CheckBox2.Checked := Sprite.AlphaImage;
    edit7.text := inttostr(Sprite.Width);
    edit8.text := inttostr(Sprite.Height);
    edit9.text := inttostr(Sprite.FrameCount);
    edit10.text := inttostr(Sprite.FramesPerCol);
    edit11.text := inttostr(Sprite.FramesPerRow);
    edit12.text := inttostr(Sprite.TimePerFrame);
    edit13.text := inttostr(Sprite.FrameOffset);

    edit1.text := Sprite.Name;
    label6.caption := format('[%d..%d]', [Sprite.StartAngle, Sprite.EndAngle]);
    // Raus Suchen des "Abgeleiteten" Bildes
    sb := ani.GetBitmapOf(ListBox1.ItemIndex);
    If assigned(sb) Then Begin
      b := TBitmap.Create;
      b.Width := Sprite.Rect.Right - Sprite.Rect.Left;
      b.Height := Sprite.Rect.Bottom - Sprite.Rect.Top;
      Derivedindex := ani.GetDerivedIndexOf(ListBox1.ItemIndex);
      If assigned(Ani.Sprite[Derivedindex].AlphaMask) Then Begin
        c := MulImage(sb, Ani.Sprite[Derivedindex].AlphaMask, true);
        b.Canvas.Draw(-Sprite.Rect.Left, -Sprite.Rect.Top, c);
        c.free;
      End
      Else Begin
        b.Canvas.Draw(-Sprite.Rect.Left, -Sprite.Rect.Top, sb);
      End;
      Image1.Picture.Assign(b);
      b.free;
      edit3.text := inttostr(Sprite.Rect.Top);
      edit6.text := inttostr(Sprite.Rect.Left);
      edit4.text := inttostr(Sprite.Rect.Right);
      edit5.text := inttostr(Sprite.Rect.Bottom);
    End
    Else Begin
      image1.Picture.Clear;
      edit3.text := '0';
      edit4.text := '0';
      edit5.text := '0';
      edit6.text := '0';
    End;
  End;
End;

End.

