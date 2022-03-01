Unit Unit1;

{$MODE objfpc}{$H+}
{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, OpenGLContext
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  , dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , uopengl_graphikengine
  , umapviewer
  , Types;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenGLControl1: TOpenGLControl;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure CheckBox3Change(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure Edit5Change(Sender: TObject);
    Procedure Edit6Change(Sender: TObject);
    Procedure Edit7Change(Sender: TObject);
    Procedure Edit8Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure OpenGLControl1DblClick(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
  private
    fPointImage: integer;
    fMV: tMapViewer;
    Procedure Exit2d();
    Procedure Go2d();
  public

  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  engFormat: TFormatSettings;

Implementation

{$R *.lfm}

{ TForm1 }

Uses
  uvectormath,
  ugraphics,
  uOpenGL_ASCII_Font,
  LazFileUtils,
  FileUtil;

Var
  allowcnt: Integer = 0;

Function PrettyKoordToString(Coord: Double; Prefix: String): String;
Var
  p, a, b, c: String;
Begin
  DecodeKoordinate(coord, a, b, c);
  p := Prefix[1];
  If strtoint(a) < 0 Then
    p := Prefix[2];
  result := p + format(' %d° %0.2d.%0.3d', [abs(strtoint(a)), abs(strtoint(b)), abs(strtoint(c))]);
End;

Procedure Tform1.Go2d();
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

Procedure Tform1.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Var
  bm: TBitmap;
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
    // glEnable(GL_DEPTH_TEST); // Tiefentest
    // glDepthFunc(gl_less);
    Create_ASCII_Font();
    If assigned(fmv) Then fmv.free;
    fmv := tMapViewer.Create(OpenGLControl1);
    fmv.ScrollGrid := 1;
    fmv.MapLocalization := 'de';
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
    OpenGLControl1.Invalidate;
    bm := TBitmap.Create;
    bm.Width := 32;
    bm.Height := 32;
    bm.Canvas.Brush.Color := clFuchsia;
    bm.Canvas.Rectangle(-1, -1, 33, 33);
    ImageList1.Draw(bm.Canvas, 0, 0, 0);
    fPointImage := OpenGL_GraphikEngine.LoadAlphaColorGraphik(bm, 'CustomPointImage1', ColorToRGB(clFuchsia));
    bm.free;
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Var
  p: TVector2;
Begin
  If Not assigned(fMV) Then exit;
  If ssleft In shift Then Begin
    OpenGLControl1.Invalidate;
  End;
  p := fmv.GetMouseMapLongLat(x, y);

  caption :=
    format('%s %s = [%0.6f,%0.6f], Zoom: %d', [
    PrettyKoordToString(p.y, 'NS'),
      PrettyKoordToString(p.x, 'EW'),
      p.y, p.x,
      fMV.Zoom], engFormat);
End;

Procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
Begin
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  engFormat := DefaultFormatSettings;
  engFormat.DecimalSeparator := '.';
  fMV := Nil;
  OpenGLControl1.Invalidate;
  edit1.text := format('%0.6f', [52.520813]);
  edit2.text := format('%0.6f', [13.409409]);
  edit3.text := format('%0.6f', [52.520813]);
  edit4.text := format('%0.6f', [13.409409]);

  edit5.text := '';
  edit6.text := '';
  edit7.text := '';
  edit8.text := '';
End;

Procedure TForm1.OpenGLControl1DblClick(Sender: TObject);
Begin
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Initialized := false;
  If assigned(fMV) Then
    fmv.free;
  fMV := Nil;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  fMV.ReEnableDownloading;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Den OpenGL-Cache Löschen
  fMV.EmptyTileCache();
  // Alle Dateien auf der Platte Löschen
  sl := findallfiles(IncludeTrailingPathDelimiter(fMV.CacheFolder));
  For i := 0 To sl.Count - 1 Do Begin
    If Not DeleteFileutf8(sl[i]) Then Begin
      showmessage('Error could not delete: ' + sl[i]);
    End;
  End;
  sl.free;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // When Jump you have to set Zoom as first, then do the centerLonLat
  fMV.Zoom := 12;
  fMV.CenterLongLat(strtofloatdef(edit2.text, 13.409409), StrToFloatDef(edit1.text, 52.520813));
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Add a UserImage at
  fMV.AddImageOnCoord(strtofloatdef(edit4.text, 13.409409), StrToFloatDef(edit3.text, 52.520813), fPointImage, 32, 32, -16, -16, 'Userpoint', 'Metainfo');
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  fMV.ShowScale := CheckBox1.Checked;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
Begin
  fMV.ShowPointLabels := CheckBox2.Checked;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.CheckBox3Change(Sender: TObject);
Begin
  fMV.Show161Ranges := CheckBox3.Checked;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Begin
  Case ComboBox1.ItemIndex Of
    0: fMV.Source := msGoogleHybrid;
    1: fMV.Source := msGoogleNormal;
    2: fMV.Source := msGoogleSatellite;
    3: fMV.Source := msGoogleTerrain;
  End;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.ComboBox2Change(Sender: TObject);
Begin
  // Switch Language
  fMV.MapLocalization := ComboBox2.Text;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.Edit5Change(Sender: TObject);
Begin
  If assigned(fMV) Then
    fMV.ProxyHost := Edit5.Text;
End;

Procedure TForm1.Edit6Change(Sender: TObject);
Begin
  If assigned(fMV) Then
    fMV.ProxyPass := Edit6.Text;
End;

Procedure TForm1.Edit7Change(Sender: TObject);
Begin
  If assigned(fMV) Then
    fMV.ProxyPort := Edit7.Text;
End;

Procedure TForm1.Edit8Change(Sender: TObject);
Begin
  If assigned(fMV) Then
    fMV.ProxyUser := Edit8.Text;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  fMV.Render();

  OpenGLControl1.SwapBuffers;
{$IFDEF DebuggMode}
  i := glGetError();
  If i <> 0 Then Begin
    p := gluErrorString(i);
    showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + #13#13 +
      'OpenGL Message : "' + p + '"'#13#13 +
      'Applikation will be terminated.');
    close;
  End;
{$ENDIF}
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  // Fixed, aber dennoch ..
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
  End;
End;

End.

