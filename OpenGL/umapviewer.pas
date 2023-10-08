(******************************************************************************)
(* umapviewer.pas                                                  ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : make Google maps accessable for FPC / OpenGL                 *)
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
(* Inspired by: https://github.com/maciejkaczkowski/mapviewer (GPLv2)         *)
(*                                                                            *)
(* Modifications against the Original one:                                    *)
(*                            - Merge into one file                           *)
(*                            - removed threads                               *)
(*                            - Render in OpenGL (instead of LCL-Component)   *)
(*                            - Only Google Maps supported                    *)
(*                            - Added Images shown on locations               *)
(*                            - only using synapse as downloadmanager         *)
(*                            - enable proxy downloading                      *)
(*                            - removed as much variables as possible         *)
(*                                                                            *)
(* Missing Features:                                                          *)
(*                       - Cleanup, Doku                                      *)
(*                       - Download via Thread for smoother rendering ?       *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)

Unit umapviewer;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, OpenGlcontext, uvectormath;

Type

  TMapSource = (
    msNone,
    msGoogleHybrid,
    msGoogleNormal,
    msGoogleSatellite,
    msGoogleTerrain
    );

  TProxySettings = Record
    ProxyHost: String;
    ProxyPass: String;
    ProxyPort: String;
    ProxyUser: String;
  End;

  TImageInfoRecord = Record
    x, y: Extended;
    ImageIndex: integer;
    W, H, OffX, OffY: integer;
    Label_: String;
    MetaInfo: String; // Wird nicht gerendert einfach nur so zur info --> TODO: das sollte ein PTRInt werden !
  End;

  { tMapViewer }

  tMapViewer = Class
  private
    fCacheFolder: String;
    fOpenGLControl: TOpenGLControl; // Das OpenGLControl in dem Gerendert wird, dieses liefert die OnMouse* Events

    fShowScale: Boolean; // Wenn True, dann wird unten Rechts ein kleiner Maßstab eingeblendet

    FX: Int64; // Die auf die Tile Karte Umgerechnete X-Koordinate
    FY: Int64; // Die auf die Tile Karte Umgerechnete X-Koordinate
    FOffsetX: Int64; // Delta X um das die Karte verschoben wurde
    FOffsetY: Int64; // Delta Y um das die Karte verschoben wurde

    FSource: TMapSource; // Auswahl, welche Karte gerade angezeigt werden soll

    DownX, DownY: integer; // x,y Position an derer die Maus beim OnMouseDown war

    FZoom: integer; // Aktuelles Zoom Level

    FMouseDown: Boolean; // True, zwischen MouseDown und MouseUp

    fMouseDownCapture: TMouseEvent;
    fMouseMoveCapture: TMouseMoveEvent;
    fMouseUpCapture: TMouseEvent;

    fMouseDblClickCapture: TNotifyEvent;
    fMouseWheelDownCapture: TMouseWheelUpDownEvent;
    fMouseWheelUpCapture: TMouseWheelUpDownEvent;
    fStopDownloading: Boolean;
    fScrollCount: integer; // Zählt die Scroll Aufrufe
    fScrollGrid: integer; // Führt den OnScroll Code nur alle X mal aus
    fProxy: TProxySettings; // Die Userdaten für den Proxy Server

    fPoints: Array Of TImageInfoRecord; // Container für die Zusätzlich an zu zeigenden Punkte

    Function GetImage(index: integer): TImageInfoRecord;
    Function GetImageCount: integer;
    Procedure PaintRectangle(AX, AY, X, Y, Z: Int64); // Rendert ein Tile (lädt ggf auch nach)

    Procedure DblClick(Sender: TObject); // Doppelklick oder ZoomIn

    Procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
    Procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);

    Procedure MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
    Procedure MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);

    Function DownloadTile(X, Y, Z: Integer): integer; // Runter laden eines Tiles

    Procedure Go2d();
    Procedure Exit2d();
    Procedure SetCacheFolder(AValue: String);

    Procedure SetCenterLongLat(Const AValue: TVector2); // Zentriert auf das Sichtfenster auf der Lat / Lon Position
    Procedure SetImage(index: integer; AValue: TImageInfoRecord);
    Procedure SetScrolGrid(AValue: integer);
    Procedure SetShowscale(AValue: Boolean);

    Procedure ZoomTo(x, y: Integer); // Zoomt an den Pixel Positionen x, y rein
    Procedure UnZoomTo(x, y: Integer); // Zoomt an den Pixel positionen x, y raus

    Function getProxyHost: String;
    Function getProxyPass: String;
    Function getProxyPort: String;
    Function getProxyUser: String;

    Procedure SetProxyHost(AValue: String);
    Procedure SetProxyPass(AValue: String);
    Procedure SetProxyPort(AValue: String);
    Procedure SetProxyUser(AValue: String);
  public
    ShowPointLabels: Boolean;
    Show161Ranges: Boolean;
    MapLocalization: String; // de = Deutsch, en = Englisch, ja = Japan ...
    Property CacheFolder: String read fCacheFolder write SetCacheFolder; // Das Verzeichnis in welchem die herunter geladenen Tiles gespeichert werden (Default paramstr(0)+\temp

    Property ProxyHost: String read getProxyHost write SetProxyHost;
    Property ProxyPass: String read getProxyPass write SetProxyPass;
    Property ProxyPort: String read getProxyPort write SetProxyPort;
    Property ProxyUser: String read getProxyUser write SetProxyUser;

    Property Zoom: integer read FZoom write FZoom;
    Property Source: TMapSource read FSource write FSource;
    Property ScrollGrid: integer read fScrollGrid write SetScrolGrid; // Used for mous scrolling, typically 1 (or on some systems 3)
    Property ShowScale: Boolean read fShowScale write SetShowscale;

    Property ImageCount: integer read GetImageCount;
    Property Image[index: integer]: TImageInfoRecord read GetImage write SetImage;

    Constructor Create(OpenGLControl: TOpenGLControl);
    Destructor Destroy; override;

    Procedure Render();
    Procedure EmptyTileCache(); // Löscht alle bisher gepufferten Teile aus dem OpenGL-Cache

    (*
     * Zentriert die Koordinate Lon, Lat innerhalb des Sichtbaren Bereiches
     *)
    Procedure CenterLongLat(Lon, Lat: Double); // !! ACHTUNG !!, Der Zoom muss vorher gesetzt werden.
    (*
     * Rückgabe des Lat und Long Wert als Übergabe eine Koordinate auf dem Canvas des Renderingkontextes (in Pixel)
     *)
    Function GetMouseMapLongLat(X, Y: Integer): TVector2;
    Function GetMouseMapLongLatRev(p: TVector2): TPoint; // Umkehrfunktion zu GetMouseMapLongLat

    Procedure Reset; // Reset der Kameraeinstellungen

    (*
     * Zeigt ein Bild Zentriert über der Coordinate x, y an.
     * ImageIndex aus OpenGL_GraphikEngine
     * mit w, h wird die Bildgröße in Pixel angegeben
     * offx und offy verschieben das Bild nach Wunsch
     * Label_ beschriftet das Bild wenn gewünscht.
     *)
    Function AddImageOnCoord(lon, lat: Extended; ImageIndex, w, h, offx,
      offy: integer; Label_, MetaInfo: String): boolean;
    Procedure DelImageWith(Label_, MetaInfo: String);
    Procedure ClearImagesOnCoords;

    Procedure ReEnableDownloading; // Kann ein Tile nicht geladen werden (weil z.B. eine unsichere Verbindung besteht und das Teil nicht gecached vorliegt, dann wird das Runterladen blockiert, diese Routine hebt die Blockierung wieder auf).

    Procedure ZoomInAtXY(x, y: integer); // Zoomt an der X, Y Koordinate rein (x,y sind in Canvas Pixel Koordinaten gegeben, nicht in GPS Koordinaten !!)
    //Todo: Das so umschreiben das GetImageAtXY nen Integer zurückgibt und man den dann bei Image[] nutzen kann, damit kann dann auch der Operator und UpdateImage raus
    Function GetImageAtXY(x, y: Integer; Out Img: TImageInfoRecord): Boolean; // Gibt an ob unter x,y (als Pixel) sich ein Bild aus AddImageOnCoord befindet, wenn ja ist es in Img zu finden
    Function UpdateImage(OldImg: TImageInfoRecord; NewImg: TImageInfoRecord): Boolean; // Sucht nach OldImage und ersetzt dieses durch NewImg, True bei Erfolg
  End;

  (*
   * Umwandlungsroutinen Float nach DE Format
   *
   * Im Prinzip Passiert folgendes
   * FloatZahl = Vorkomma.Nachkomma
   * WinkelZal = Vorkomma° Nachkomma * 60 / 100
   *
   *)
Procedure EncodeKoordinate(Out Coord: Double; a, b, c: String);
Procedure DecodeKoordinate(Coord: Double; Out a: String; Out b: String; Out c: String);
Function ZoomLevelToGroundResolution(Level: integer): Extended; // Liefert zum Zoom Level die Streck in Meter die 256 Pixel (1-Tile) überdecken.

Operator = (a, b: TImageInfoRecord): boolean;

Implementation

Uses
  dglOpenGL,
  uopengl_graphikengine,
  uopengl_ascii_font,
  httpsend,
  Graphics,
  LazUTF8, LazFileUtils, math, Dialogs;

Const
  TILE_SIZE: int64 = 256;
  EARTH_RADIUS = 6378137;
  SHIFT = {2 *} pi * EARTH_RADIUS {/ 2.0};

Function PointInRect(p: Tpoint; r: Trect): boolean;
Begin
  result := (p.x >= min(r.Left, r.Right)) And (p.x <= max(r.Left, r.Right)) And
    (p.y >= min(r.Top, r.Bottom)) And (p.y <= max(r.Top, r.Bottom));
End;

Function ZoomLevelToGroundResolution(Level: integer): Extended;
Begin
  // Table taken from : https://msdn.microsoft.com/en-us/library/bb259689.aspx
  result := 0;
  Case level Of
    1: result := 782715170;
    2: result := 391357585;
    3: result := 195678792;
    4: result := 97839396;
    5: result := 48919698;
    6: result := 24459849;
    7: result := 12229925;
    8: result := 6114962;
    9: result := 3057481;
    10: result := 1528741;
    11: result := 764370;
    12: result := 382185;
    13: result := 191093;
    14: result := 95546;
    15: result := 47773;
    16: result := 23887;
    17: result := 11943;
    18: result := 5972;
    19: result := 2986;
    20: result := 1493;
    21: result := 746;
    22: result := 373;
    23: result := 187;
  End;
  result := result / (16 * 4); // Where the hell comes this factor from ?
End;

Operator = (a, b: TImageInfoRecord): boolean;
Begin
  result :=
    (a.x = b.x) And
    (a.y = b.y) And
    (a.ImageIndex = b.ImageIndex) And
    (a.W = b.W) And
    (a.H = b.H) And
    (a.OffX = b.OffX) And
    (a.OffY = b.OffY) And
    (a.Label_ = b.Label_) And
    (a.MetaInfo = b.MetaInfo);
End;

Function DownLoadFile(URL, Filename: String; Proxy: TProxySettings): Boolean;
Const
  MaxTries = 10;
Var
  f: TFileStream;
  http: THTTPSend;
  tries: integer;
Begin
  result := false;
  http := THTTPSend.Create;
  If Proxy.ProxyHost <> '' Then Begin
    HTTP.ProxyHost := Proxy.ProxyHost;
    HTTP.ProxyPass := Proxy.ProxyPass;
    HTTP.ProxyPort := Proxy.ProxyPort;
    HTTP.ProxyUser := Proxy.ProxyUser;
  End;

  tries := 0;
  Repeat
    If Not Http.HTTPMethod('GET', url) Then Begin
      http.free;
      exit;
    End;
    If http.ResultCode = 301 Then Begin
      http.Headers.NameValueSeparator := ':';
      url := http.Headers.Values['Location'];
      If url = '' Then Begin
        http.free;
        exit;
      End;
      http.Clear;
    End;
    If (tries >= Maxtries) Then Begin
      http.free;
      exit;
    End;
    inc(tries);
  Until http.ResultCode = 200;
  If http.Document.Size <> 0 Then Begin
    f := TFileStream.Create(Filename, fmOpenWrite Or fmCreate);
    f.CopyFrom(http.Document, http.Document.Size);
    f.Free;
    result := true;
  End;
  http.free;
End;

Function MapSourceToString(Value: TMapSource): String;
Begin
  result := 'None';
  Case Value Of
    msNone: result := 'None';
    msGoogleNormal: result := 'GoogleNormal';
    msGoogleSatellite: result := 'GoogleSatellite';
    msGoogleTerrain: result := 'GoogleTerrain';
    msGoogleHybrid: result := 'GoogleHybrid';
  Else Begin
      Raise Exception.Create('Todo : MapSourceToString');
    End;
  End;
End;

Function IsValidPNG(Filename: String): Boolean;
Var
  s: String;
  f: TFileStream;
Begin
  result := false;
  If FileExistsUTF8(Filename) Then Begin
    f := TFileStream.Create(Filename, fmOpenRead);
    s := '';
    SetLength(s, 3);
    f.Position := 1;
    f.Read(s[1], 3);
    Result := s = 'PNG';
    f.free;
  End;
End;

Function IsValidJPEG(Filename: String): Boolean;
Var
  s: String;
  f: TFileStream;
Begin
  result := false;
  If FileExistsUTF8(Filename) Then Begin
    f := TFileStream.Create(Filename, fmOpenRead);
    s := '';
    SetLength(s, 4);
    f.Position := 6;
    f.Read(s[1], 4);
    Result := (s = 'JFIF') Or (s = 'Exif');
    f.free;
  End;
End;

Procedure DecodeKoordinate(Coord: Double; Out a: String; Out b: String; Out
  c: String);
Var
  v, n: Double;
  i: int64;
  c_: integer;
  DefFormat: TFormatSettings;
Begin
  DefFormat := FormatSettings;
  DefFormat.DecimalSeparator := '.';
  v := trunc(coord);
  n := coord - v;
  n := n * 1000000;
  i := trunc(n);
  i := i * 60;
  a := floattostr(v, DefFormat);
  c_ := round((i Mod 1000000) / 1000);
  If abs(c_) >= 1000 Then Begin
    If c_ < 0 Then Begin
      c := format('%.3d', [((c_ + 1000))], DefFormat);
      b := format('%.2d', [(trunc(i / 1000000) + c_ Div 1000)], DefFormat);
    End
    Else Begin
      c := format('%.3d', [((c_ - 1000))], DefFormat);
      b := format('%.2d', [(trunc(i / 1000000) + c_ Div 1000)], DefFormat);
    End;
  End
  Else Begin
    b := format('%.2d', [(trunc(i / 1000000))], DefFormat);
    c := format('%.3d', [(c_)], DefFormat);
  End;
End;

Procedure EncodeKoordinate(Out Coord: Double; a, b, c: String);
  Function Clear(val: String): String;
  Var
    i: Integer;
  Begin
    result := '';
    For i := 1 To length(val) Do
      If val[i] In ['0'..'9', '-'] Then Begin
        result := result + val[i];
      End;
  End;
Var
  i: int64;
  n: Double;
Begin
  Try
    a := Clear(a);
    b := Clear(b);
    c := Clear(c);
    Coord := strtoint(a);
    i := StrToInt64(b) * 1000000 + strtoint64(c) * 1000;
    n := trunc(i / 60);
    n := n / 1000000;
    Coord := Coord + n;
  Except
    coord := -1;
  End;
End;

{ tMapViewer }

Constructor tMapViewer.Create(OpenGLControl: TOpenGLControl);
Begin
  fProxy.ProxyHost := '';
  fProxy.ProxyPass := '';
  fProxy.ProxyPort := '';
  fProxy.ProxyUser := '';

  fOpenGLControl := OpenGLControl;
  fShowScale := true;
  Show161Ranges := false;
  fScrollGrid := 3;
  fScrollCount := 0;
  fMouseWheelDownCapture := fOpenGLControl.OnMouseWheelDown;
  fMouseWheelUpCapture := fOpenGLControl.OnMouseWheelUp;

  fMouseDownCapture := fOpenGLControl.OnMouseDown;
  fMouseMoveCapture := fOpenGLControl.OnMouseMove;
  fMouseUpCapture := fOpenGLControl.OnMouseUp;
  fMouseDblClickCapture := fOpenGLControl.OnDblClick;

  fOpenGLControl.OnMouseWheelDown := @MouseWheelDown;
  fOpenGLControl.OnMouseWheelUp := @MouseWheelUp;

  fOpenGLControl.OnMouseDown := @MouseDown;
  fOpenGLControl.OnMouseMove := @MouseMove;
  fOpenGLControl.OnMouseUp := @MouseUp;
  fOpenGLControl.OnDblClick := @DblClick;

  FSource := msGoogleNormal;
  FX := 0;
  FY := 0;
  FZoom := 0;
  FMouseDown := false;
  fStopDownloading := false;
  ShowPointLabels := false;
  fCacheFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrutf8(0))) + 'cache' + PathDelim;
End;

Destructor tMapViewer.Destroy;
Begin
  ClearImagesOnCoords;
  Inherited Destroy;
End;

Procedure tMapViewer.Go2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, fOpenGLControl.Width, fOpenGLControl.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure tMapViewer.Exit2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Procedure tMapViewer.SetCacheFolder(AValue: String);
Begin
  If fCacheFolder = AValue Then Exit;
  fCacheFolder := IncludeTrailingPathDelimiter(AValue);
End;

Function tMapViewer.GetMouseMapLongLat(X, Y: Integer): TVector2;
Var
  tiles: Int64;
  circumference: Int64;
  lat: Extended;
  res: Extended;
  mPoint: TPoint;
Begin

  tiles := 1 Shl fZoom;
  circumference := tiles * TILE_SIZE;

  mPoint.x := X - FX + FOffsetX;
  mPoint.y := Y - FY + FOffsetY;

  Result.X := ((mPoint.X * 360.0) / circumference) - 180.0;

  res := (2 * pi * EARTH_RADIUS) / circumference;
  lat := ((mPoint.Y * res - SHIFT) / SHIFT) * 180.0;

  lat := radtodeg(2 * arctan(exp(lat * pi / 180.0)) - pi / 2.0);
  Result.Y := -lat;
End;

Function tMapViewer.GetMouseMapLongLatRev(p: TVector2): TPoint;
Var
  mx, my: Extended;
  res: Extended;
Begin
  mx := p.X * (shift / 180.0);
  my := ln(tan((90 - p.Y) * pi / 360.0)) / (pi / 180.0);
  my := my * (shift / 180.0);
  res := (2 * pi * EARTH_RADIUS) / (TILE_SIZE * (1 Shl FZoom));
  Result.X := Round((mx + Shift) / res) - FOffsetX + FX;
  Result.Y := Round((my + Shift) / res) - FOffsetY + FY;
End;

Procedure tMapViewer.Reset;
Begin
  FZoom := 0;
  CenterLongLat(0, 0);
End;

Function tMapViewer.AddImageOnCoord(lon, lat: Extended; ImageIndex, w, h, offx,
  offy: integer; Label_, MetaInfo: String): boolean;
Begin
  result := true;
  Setlength(fPoints, high(fPoints) + 2);
  fPoints[high(fPoints)].x := lon;
  fPoints[high(fPoints)].y := lat;
  fPoints[high(fPoints)].W := w;
  fPoints[high(fPoints)].H := h;
  fPoints[high(fPoints)].OffX := offx;
  fPoints[high(fPoints)].Offy := offy;
  fPoints[high(fPoints)].ImageIndex := ImageIndex;
  fPoints[high(fPoints)].Label_ := Label_;
  fPoints[high(fPoints)].MetaInfo := MetaInfo;
End;

Procedure tMapViewer.DelImageWith(Label_, MetaInfo: String);
Var
  i, j: Integer;
Begin
  For i := 0 To high(fPoints) Do Begin
    If (fPoints[i].Label_ = Label_) And (fPoints[i].MetaInfo = MetaInfo) Then Begin
      For j := i To high(fPoints) - 1 Do Begin
        fPoints[j] := fPoints[j + 1];
      End;
      SetLength(fPoints, high(fPoints));
      break;
    End;
  End;
End;

Procedure tMapViewer.ClearImagesOnCoords;
Begin
  setlength(fPoints, 0);
End;

Procedure tMapViewer.ReEnableDownloading;
Begin
  fStopDownloading := false;
End;

Procedure tMapViewer.ZoomInAtXY(x, y: integer);
Begin
  ZoomTo(x, y);
End;

Function tMapViewer.GetImageAtXY(x, y: Integer; Out Img: TImageInfoRecord
  ): Boolean;
Var
  TopLeft, BottomRight: TVector2;
  i, xx, yy: Integer;
  pp, p: Tpoint;
  r: Trect;
Begin
  result := false;
  Topleft := GetMouseMapLongLat(0, 0);
  BottomRight := GetMouseMapLongLat(fOpenGLControl.Width, fOpenGLControl.Height);
  p := point(x, y);
  For i := 0 To high(fPoints) Do Begin
    If (fPoints[i].x >= min(TopLeft.X, BottomRight.X)) And (fPoints[i].x <= max(TopLeft.X, BottomRight.X)) And
      (fPoints[i].y >= min(TopLeft.y, BottomRight.y)) And (fPoints[i].y <= max(TopLeft.y, BottomRight.y)) Then Begin
      pp := GetMouseMapLongLatRev(V2(fPoints[i].x, fPoints[i].y));
      xx := pp.x + fPoints[i].OffX;
      yy := pp.y + fPoints[i].Offy;
      r.Left := xx;
      r.Top := yy;
      r.Right := r.Left + fPoints[i].W;
      r.Bottom := r.Top + fPoints[i].H;
      If PointInRect(p, r) Then Begin
        img := fPoints[i];
        result := true;
        exit;
      End;
    End;
  End;
End;

Function tMapViewer.UpdateImage(OldImg: TImageInfoRecord;
  NewImg: TImageInfoRecord): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(fPoints) Do Begin
    If fPoints[i] = OldImg Then Begin
      fPoints[i] := NewImg;
      result := true;
    End;
  End;
End;

Procedure tMapViewer.SetCenterLongLat(Const AValue: TVector2);
Var
  p: TPoint;
Begin
  p := GetMouseMapLongLatRev(avalue);
  FX := fOpenGLControl.Width Div 2 - p.x + FX - FOffsetX;
  FY := fOpenGLControl.Height Div 2 - p.y + FY - FOffsetY;
  FOffsetX := 0; // Rein oder Raus ?
  FOffsetY := 0; // Rein oder Raus ?
End;

Procedure tMapViewer.SetImage(index: integer; AValue: TImageInfoRecord);
Begin
  fPoints[index] := AValue;
End;

Procedure tMapViewer.SetProxyHost(AValue: String);
Begin
  fProxy.ProxyHost := AValue;
End;

Procedure tMapViewer.SetProxyPass(AValue: String);
Begin
  fProxy.ProxyPass := AValue;
End;

Procedure tMapViewer.SetProxyPort(AValue: String);
Begin
  fProxy.ProxyPort := AValue;
End;

Procedure tMapViewer.SetProxyUser(AValue: String);
Begin
  fProxy.ProxyUser := AValue;
End;

Procedure tMapViewer.SetScrolGrid(AValue: integer);
Begin
  If fScrollGrid = AValue Then Exit;
  fScrollGrid := max(1, AValue);
End;

Procedure tMapViewer.SetShowscale(AValue: Boolean);
Begin
  If fShowScale = AValue Then Exit;
  fShowScale := AValue;
  render;
End;

Procedure tMapViewer.ZoomTo(x, y: Integer);
Var
  r: TVector2;
Begin
  // Umrechnen der Mausposition in die Tileposition
  r := GetMouseMapLongLat(x, y);
  FZoom := min(23, FZoom + 1);
  SetCenterLongLat(r);
End;

Procedure tMapViewer.UnZoomTo(x, y: Integer);
Var
  r: TVector2;
Begin
  // Umrechnen der Mausposition in die Tileposition
  r := GetMouseMapLongLat(x, y);
  FZoom := max(0, FZoom - 1);
  SetCenterLongLat(r);
End;

Procedure tMapViewer.Render;
  Function PrettyScale(value: Extended): String;
  Var
    s: String;
  Var
    DefFormat: TFormatSettings;
  Begin
    DefFormat := FormatSettings;
    DefFormat.DecimalSeparator := '.';
    s := 'm';
    If value > 1000 Then Begin
      s := 'km';
      value := value / 1000;
    End;
    result := format('%0.1f', [value], DefFormat) + s;
  End;

Var
  b, dt: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  AX, AY, FMaxX, FMaxY: Int64;
  startX, startY: Integer;
  i, j, x, y: Integer;

  TopLeft, BottomRight: TVector2;
  s: String;
  si, co, d: Extended;
  p: Tpoint;
Begin
  Go2d();
  dt := glIsEnabled(GL_DEPTH_TEST);
  If (dt{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glDisable(GL_DEPTH_TEST);

  glBindTexture(GL_TEXTURE_2D, 0);
  glColor3f(1, 1, 1);

  (*
   * Rendern der Eigentlichen Karte
   *)
  AX := -FOffsetX;
  AY := -FOffsetY;
  startX := (-(FX + AX)) Div TILE_SIZE;
  startY := (-(FY + AY)) Div TILE_SIZE;
  FMaxX := (fOpenGLControl.Width Div TILE_SIZE) + 1;
  FMaxY := (fOpenGLControl.Height Div TILE_SIZE) + 1;

  For i := startX To startX + FMaxX Do Begin
    For j := startY To startY + FMaxY Do Begin
      PaintRectangle(AX, AY, i, j, FZoom);
    End;
  End;
  (*
   * Die Points
   *)
  Topleft := GetMouseMapLongLat(0, 0);
  BottomRight := GetMouseMapLongLat(fOpenGLControl.Width, fOpenGLControl.Height);
  glPushMatrix;
  For i := 0 To high(fPoints) Do Begin
    If (fPoints[i].x >= min(TopLeft.X, BottomRight.X)) And (fPoints[i].x <= max(TopLeft.X, BottomRight.X)) And
      (fPoints[i].y >= min(TopLeft.y, BottomRight.y)) And (fPoints[i].y <= max(TopLeft.y, BottomRight.y)) Then Begin
      p := GetMouseMapLongLatRev(V2(fPoints[i].x, fPoints[i].y));
      x := p.x + fPoints[i].OffX;
      y := p.y + fPoints[i].Offy;
      B := glIsEnabled(gl_Blend);
      If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glenable(gl_Blend);
      glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
      glColor3d(1, 1, 1);
      glBindTexture(GL_TEXTURE_2D, fPoints[i].ImageIndex);
      glbegin(GL_QUADS);
      glTexCoord2f(0, 1);
      glVertex2f(X, Y + fPoints[i].h);
      glTexCoord2f(1, 1);
      glVertex2f(X + fPoints[i].W, Y + fPoints[i].h);
      glTexCoord2f(1, 0);
      glVertex2f(X + fPoints[i].W, Y);
      glTexCoord2f(0, 0);
      glVertex2f(X, Y);
      glend;
      If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(gl_blend);
      If ShowPointLabels Then Begin
        glBindTexture(GL_TEXTURE_2D, 0);
        OpenGL_ASCII_Font.Color := clblack;
        OpenGL_ASCII_Font.Textout(x - round(OpenGL_ASCII_Font.TextWidth(fPoints[i].Label_) / 2), y + fPoints[i].H, fPoints[i].Label_);
        glColor3f(1, 1, 1);
      End;
      If Show161Ranges Then Begin
        d := ZoomLevelToGroundResolution(FZoom); // 256 Pixel sind nun diese Level
        d := (2 * 161 * 256) / d;
        If d > 5 Then Begin
          glPushMatrix;
          glTranslatef(x - fPoints[i].OffX, y - fPoints[i].OffY, 0);
          glBindTexture(GL_TEXTURE_2D, 0);
          glColor3d(0, 0, 0);
          glbegin(GL_LINE_LOOP);
          For j := 0 To 23 Do Begin
            sincos(pi * 2 * j / 24, si, co);
            glVertex2f(-co * d / 2, si * d / 2);
          End;
          glend;
          glPopMatrix;
        End;
      End;
    End;
  End;
  glPopMatrix;
  If fShowScale Then Begin
    OpenGL_ASCII_Font.Color := clblack;
    d := ZoomLevelToGroundResolution(FZoom); // 256 Pixel sind nun diese Level
    d := d * 100 / 256; // Umrechen in 100 Pixel entsprechen.
    s := PrettyScale(d);
    glBindTexture(GL_TEXTURE_2D, 0);
    OpenGL_ASCII_Font.Textout(fOpenGLControl.Width - round(OpenGL_ASCII_Font.TextWidth(s)) - 50, fOpenGLControl.height - 50, s);
    glColor3f(0, 0, 0);
    glBegin(GL_LINES);
    glVertex2f(fOpenGLControl.Width - 50 - 100, fOpenGLControl.height - 50 - round(OpenGL_ASCII_Font.TextHeight('S')) - 10);
    glVertex2f(fOpenGLControl.Width - 50, fOpenGLControl.height - 50 - round(OpenGL_ASCII_Font.TextHeight('S')) - 10);
    glend();
  End;
  If (dt{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(GL_DEPTH_TEST);
  Exit2d();
End;

Procedure tMapViewer.EmptyTileCache;
Begin
  OpenGL_GraphikEngine.Clear;
End;

Procedure tMapViewer.CenterLongLat(Lon, Lat: Double);
Begin
  SetCenterLongLat(V2(lon, lat));
End;

Procedure tMapViewer.PaintRectangle(AX, AY, X, Y, Z: Int64);
Var
  XD, YD: Int64;
  XB, YB: Int64;
  maxOfZ: Int64;
  imageIndex: integer;
Begin
  maxOfZ := 1 Shl Z;
  If Not ((Z = 0) And (X = 0) And (Y = 0)) Then
    If (X < 0) Or (Y < 0) Or (X > maxOfZ - 1) Or (Y > maxOfZ - 1) Then
      Exit;
  XD := X * TILE_SIZE;
  YD := Y * TILE_SIZE;
  XB := FX + AX + XD; // begin of X
  YB := FY + AY + YD; // begin of Y
  imageIndex := DownloadTile(X, Y, Z);
  If (imageIndex = 0) And (Not fStopDownloading) Then Begin
    fStopDownloading := true;
    showmessage('Error could not download imageIndex data. Stop downloading now.');
  End;
  glBindTexture(GL_TEXTURE_2D, imageIndex);
  glbegin(GL_QUADS);
  glTexCoord2f(0, 1);
  glVertex2f(XB, YB + 256);
  glTexCoord2f(1, 1);
  glVertex2f(XB + 256, YB + 256);
  glTexCoord2f(1, 0);
  glVertex2f(XB + 256, YB);
  glTexCoord2f(0, 0);
  glVertex2f(XB, YB);
  glend;
End;

Function tMapViewer.GetImageCount: integer;
Begin
  result := length(fPoints);
End;

Function tMapViewer.GetImage(index: integer): TImageInfoRecord;
Begin
  result := fPoints[index];
End;

Function tMapViewer.getProxyHost: String;
Begin
  result := fProxy.ProxyHost;
End;

Function tMapViewer.getProxyPass: String;
Begin
  result := fProxy.ProxyPass;
End;

Function tMapViewer.getProxyPort: String;
Begin
  result := fProxy.ProxyPort;
End;

Function tMapViewer.getProxyUser: String;
Begin
  result := fProxy.ProxyUser;
End;

Procedure tMapViewer.DblClick(Sender: TObject);
Begin
  If assigned(fMouseDblClickCapture) Then Begin
    fMouseDblClickCapture(self);
    FMouseDown := false;
  End
  Else Begin
    ZoomTo(DownX, DownY);
  End;
End;

Procedure tMapViewer.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
Begin
  FMouseDown := false;
  If assigned(fMouseUpCapture) Then Begin
    fMouseUpCapture(sender, button, shift, x, y);
  End;
End;

Procedure tMapViewer.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  If FMouseDown Then Begin
    FOffsetX := FOffsetX + (DownX - x);
    FOffsety := FOffsety + (Downy - y);
    DownX := x;
    DownY := y;
  End;
  If assigned(fMouseMoveCapture) Then Begin
    fMouseMoveCapture(sender, shift, x, y);
  End;
End;

Procedure tMapViewer.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
Begin
  DownX := x;
  DownY := y;
  If ssleft In shift Then Begin
    FMouseDown := True;
  End;
  If assigned(fMouseDownCapture) Then Begin
    fMouseDownCapture(sender, button, shift, x, y);
  End;
End;

Procedure tMapViewer.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  inc(fScrollCount);
  If fScrollCount Mod fScrollGrid = 0 Then Begin
    fScrollCount := 0;
    UnZoomTo(MousePos.x, MousePos.y);
  End;
  If assigned(fMouseWheelDownCapture) Then Begin
    fMouseWheelDownCapture(sender, shift, MousePos, Handled);
  End;
End;

Procedure tMapViewer.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  // Der Wheel Event kommt häufiger wie gescrollt werden soll
  inc(fScrollCount, fScrollGrid - 1);
  If fScrollCount Mod fScrollGrid = 0 Then Begin
    fScrollCount := 0;
    ZoomTo(MousePos.x, MousePos.y);
  End;
  If assigned(fMouseWheelUpCapture) Then Begin
    fMouseWheelUpCapture(sender, shift, MousePos, Handled);
  End;
End;

Function tMapViewer.DownloadTile(X, Y, Z: Integer): integer;
Var
  url, f, s: String;
  img: TGraphic;
  b: TBitmap;
  DefFormat: TFormatSettings;
Begin
  If length(MapLocalization) <> 2 Then // Init
    MapLocalization := 'en';
  DefFormat := FormatSettings;
  DefFormat.DecimalSeparator := '.';
  // Haben wir die Datei schon geladen ?
  s := format('%s_%s_%d_%d_%d', [MapSourceToString(FSource), lowercase(MapLocalization), x, y, z], DefFormat);
  result := OpenGL_GraphikEngine.Find(s, false);
  If result > 0 Then exit;
  // Nein, haben wir sie auf dem HDD Cache ?
  f := fCacheFolder + s;
  Case FSource Of
    msNone: ;
    (*
       Quelle : https://stackoverflow.com/questions/23017766/google-maps-tile-url-for-hybrid-maptype-tiles

       For anyone who wants to save some time while looking for specific tile types:
           h = roads only
           m = standard roadmap
           p = terrain
           r = somehow altered roadmap
           s = satellite only
           t = terrain only
           y = hybrid
    *)
    //                            Es geht um diesen Buchstaben  --\
    //                                                            v
    //                                                             ,traffic   <- und man bekommt das "traffic" overlay, siehe: https://www.spatialmanager.com/traffic-and-transit-in-google-maps/
    msGoogleNormal: url := Format('http://mt%d.google.com/vt/lyrs=m&x=%d&y=%d&z=%d&hl=%s', [random(4), X, Y, z, MapLocalization], DefFormat);
    msGoogleHybrid: url := Format('http://mt%d.google.com/vt/lyrs=y&x=%d&y=%d&z=%d&hl=%s', [random(4), X, Y, z, MapLocalization], DefFormat);
    msGoogleTerrain: url := Format('http://mt%d.google.com/vt/lyrs=p&x=%d&y=%d&z=%d&hl=%s', [random(4), X, Y, z, MapLocalization], DefFormat);
    msGoogleSatellite: url := Format('http://mt%d.google.com/vt/lyrs=s&x=%d&y=%d&z=%d&hl=%s', [random(4), X, Y, z, MapLocalization], DefFormat);
    (*
      Weitere Quellen: https://stackoverflow.com/questions/29692737/customizing-google-map-tile-server-url
     *)
  End;
  If Not DirectoryExistsutf8(ExtractFilePath(f)) Then Begin
    If Not CreateDirUTF8(ExtractFilePath(f)) Then Begin
      exit;
    End;
  End;
  If FileExistsUTF8(f) Or ((Not fStopDownloading) And DownLoadFile(url, f, fProxy)) Then Begin
    // Die Datei konnte geladen werden, nun aber ab damit in die Graphic Engine
    img := Nil;
    If IsValidJPEG(f) Then Begin
      img := TJPEGImage.Create;
      img.LoadFromFile(f);
    End
    Else Begin
      If IsValidPNG(f) Then Begin
        img := TPortableNetworkGraphic.Create;
        img.LoadFromFile(f);
      End;
    End;
    If Not assigned(img) Then exit;
    b := TBitmap.Create;
    b.Assign(img);
    img.free;
    // Wat ein Glück, dass all diese Tiles alle 256x256 Groß sind ;)
    result := OpenGL_GraphikEngine.LoadGraphik(b, s);
    b.free;
  End;
End;

End.

