(******************************************************************************)
(* uradialtreemenu.pas                                             22.11.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Radial popup tree menu derived from TPopupMenu. Root items   *)
(*               form a circle, child items open as outer ring segments by    *)
(*               hovering. See TRadialTreeMenu for the usage contract.        *)
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
(* Platforms   : Tested on Linux/X11 (MATE, Marco compositor) with GTK2,      *)
(*               GTK3, Qt5 and Qt6. The win64 build was tested only in Wine.  *)
(*               Untested: native Windows, Wayland, macOS.                    *)
(*                                                                            *)
(* Known Issues: - GTK3 needs an RGBA visual and a compositing manager.       *)
(*               - GTK2, Qt and Windows use a bitmap window shape; only       *)
(*                 GTK3 has an antialiased outline.                           *)
(*               - GTK2/Qt: Marco/Metacity draw a rectangular shadow around   *)
(*                 the shaped window. RefreshCompositorShadow works around    *)
(*                 it; other compositors may still show such a shadow.        *)
(*               - On Qt5 and Qt6, clMenuHighlight can be gray depending on   *)
(*                 the active theme.                                          *)
(*               - With Lazarus older than 4.0 the inherited PopupPoint       *)
(*                 property is not updated.                                   *)
(*                                                                            *)
(* History     : 0.01 - initial version                                       *)
(*                                                                            *)
(******************************************************************************)

Unit uradialtreemenu;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, Menus, Forms;

Type
  TRadialTreeMenuItem = Class(TMenuItem)
  public
    Function Add(Const ACaption: String): TRadialTreeMenuItem; reintroduce; overload;
    Procedure Release(Item: TMenuItem);
  End;

  { Add creates menu-owned roots; Item.Add creates parent-owned children.
    Release destroys an attached element and its subtree; native Remove/Delete
    only detach. References to released elements become invalid.
    Mutate on the LCL thread; changes cancel an open popup, with native changes
    detected at the next available idle pass.
    PopUp uses screen coordinates centered on the menu; before Lazarus 4.0
    the inherited PopupPoint property cannot be updated. The host is modal;
    outside clicks are consumed. Left-click closes only enabled leaf sectors,
    at any ring level; clicks on branches leave the popup unchanged.
    Tested platforms and known issues are listed in the unit header.
    Rendering assumes stable DPI. Sector images persist between popups;
    native item changes are compared rather than relying on custom dirty flags.
    A leaf click closes the popup, fires OnClose and then the item's standard
    Click (OnClick, Action, AutoCheck). The click is dropped if the item or
    menu is destroyed in OnClose or the tree changed while the popup was open.
    Opening a branch by hover calls its Click once, as native submenus do.
    Handlers that modify the tree close the popup; fill dynamic trees in OnPopup.
    Hover/preview callbacks are not implemented yet. }
  TRadialTreeMenu = Class(TPopupMenu)
  private
    Type
      TItemState = Record
        Item: TMenuItem;
        Caption: String;
        Count: Integer;
        Enabled, Visible: Boolean;
      End;
    Var
    FWindow: TForm;
    FItemStates: Array Of TItemState;
    FRenderCache: TObject;
    Function GetCachedSectorRenderCount: QWord;
    Function GetCachedSectorCount: Integer;
    Procedure CancelForMutation;
    Procedure WatchItems(AParent: TMenuItem);
    Procedure UnwatchItems;
    Function ItemsUnchanged(AParent: TMenuItem; Var Index: Integer): Boolean;
    Procedure CheckForMutation(Sender: TObject; Var Done: Boolean);
  protected
    Procedure DoClose; override;
    Procedure DoChange(Source: TMenuItem; Rebuild: Boolean); override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Destructor Destroy; override;
    Function Add(Const ACaption: String): TRadialTreeMenuItem;
    Procedure Release(Item: TMenuItem);
    Procedure Clear;
    Procedure PopUp(X, Y: Integer); override;
    // Counts prepared sectors (each with both color states), not hover paints.
    Property CachedSectorRenderCount: QWord Read GetCachedSectorRenderCount;
    // Sector images currently held. Images of removed or changed items are
    // released when the next popup is prepared, or with the menu.
    Property CachedSectorCount: Integer Read GetCachedSectorCount;
  End;

Implementation

Uses
  SysUtils, Controls, Graphics, uvectormath, ugraphics, LCLType, LCLIntf, LCLVersion,
  InterfaceBase, LCLPlatformDef
  {$IFDEF LCLGTK3}
  , Gtk3Widgets, Gtk3Objects, LazGtk3, LazGdk3, LazCairo1,
  LazGLib2, LazGObject2, LazGdkPixbuf2
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , Gtk2, Gdk2, Gtk2Proc
  {$ENDIF}
  ;

Const
  RenderScale = 4;

Type
  TRadialSectorImage = Class;

  TRadialMenuSector = Record
    Polygon: TVector2Array;
    Caption: String;
    Level, Index, ParentIndex: Integer;
    Enabled: Boolean;
    LabelPosition: TPoint;
    InnerRadius, OuterRadius: Integer;
    StartAngle, Sweep: Double;
    Image: TRadialSectorImage;
  End;

  TRadialSectorImage = Class
  public
    Key: TRadialMenuSector;
    Bounds: TRect;
    Bitmaps: Array[Boolean] Of TBitmap;
    Used: Boolean;
    Destructor Destroy; override;
    Function Matches(Const Sector: TRadialMenuSector): Boolean;
  End;

  TRadialRenderCache = Class
  public
    Sectors: TList;
    Font: TFont;
    Scale: Double;
    MenuColor, TextColor, SelectedColor, SelectedText, DisabledText, BorderColor: TColor;
    RenderCount: QWord;
    Constructor Create;
    Destructor Destroy; override;
    Procedure BeginUpdate(AScale: Double);
    Procedure EndUpdate;
    Function Find(Const Sector: TRadialMenuSector): TRadialSectorImage;
  End;

  TRadialMenuForm = Class(TForm)
  private
    FSectors: Array Of TRadialMenuSector;
    FAllSectors: Array Of TRadialMenuSector;
    // Parallel to FAllSectors; only valid while the menu tree is unchanged.
    FItems: Array Of TMenuItem;
    FSelectedItem: TMenuItem;
    FPath: Array Of Integer;
    FCenter: Integer;
    FScale: Double;
    FMask, FImage: TBitmap;
    FCache: TRadialRenderCache;
    {$IFNDEF LCLGTK3}
    FShadowRefreshed: Boolean;
    {$ENDIF}
    {$IFDEF LCLGTK3}
    FSeat: PGdkSeat;
    FDrawSignal: gulong;
    Procedure GrabPointer(Data: PtrInt);
    Procedure ReleasePointer;
    Procedure ApplyInputShape;
    Class Function DrawNative(Widget: PGtkWidget; Context: Pcairo_t;
      Data: gpointer): gboolean; cdecl; static;
    {$ENDIF}
    {$IF not defined(LCLGTK2) and not defined(LCLGTK3)}
    FGrabQueued: Boolean;
    Procedure GrabPointer(Data: PtrInt);
    Procedure ReleasePointer;
    {$ENDIF}
    {$IFDEF LCLGTK2}
    FPointerGrabbed: Boolean;
    Procedure GrabPointer(Data: PtrInt);
    Procedure ReleasePointer;
    Class Function InputMapped(Widget: PGtkWidget; Event: PGdkEvent;
      Data: Pointer): LongInt; cdecl; static;
    {$ENDIF}
    Function AddSector(AInnerRadius, AOuterRadius: Integer;
      AStartAngle, ASweep: Double; Const ACaption: String;
      ALevel, AParentIndex: Integer; AEnabled: Boolean): Integer;
    Procedure BuildMenu(AParent: TMenuItem; StartAngle, Sweep: Double;
      AParentIndex, ALevel: Integer);
    Procedure Rebuild;
    Procedure PrepareImages;
    Procedure RenderSector(Image: TRadialSectorImage; Selected: Boolean);
    Procedure DrawCaption(Const Sector: TRadialMenuSector; Dest: TBitmap;
      AOffset: TPoint);
    Function HitTest(X, Y: Integer): Integer;
    Function IsBranch(SectorIndex: Integer): Boolean;
    {$IFNDEF LCLGTK3}
    Procedure RefreshCompositorShadow;
    {$ENDIF}
    Procedure WindowShown(Sender: TObject);
    Procedure WindowHidden(Sender: TObject);
    Procedure WindowDeactivated(Sender: TObject);
  protected
    {$IF defined(LCLGTK3) or defined(LCLGTK2)}
    Procedure CreateWnd; override;
    {$ENDIF}
    Procedure Paint; override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IF not defined(LCLGTK2) and not defined(LCLGTK3)}
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$ENDIF}
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
  public
    {$IF defined(LCLGTK3) or defined(LCLGTK2)}
    Procedure DestroyWnd; override;
    {$ENDIF}
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function HasItems: Boolean;
    Property SelectedItem: TMenuItem Read FSelectedItem;
  End;

  // Outlives OnClose handlers that destroy the clicked item or its menu.
  TClickGuard = Class(TComponent)
  protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Item: TMenuItem;
  End;

Procedure TClickGuard.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (Operation = opRemove) And (AComponent = Item) Then Item := nil;
  inherited Notification(AComponent, Operation);
End;

Destructor TRadialSectorImage.Destroy;
Begin
  Bitmaps[False].Free;
  Bitmaps[True].Free;
  inherited Destroy;
End;

Function TRadialSectorImage.Matches(Const Sector: TRadialMenuSector): Boolean;
Begin
  Result := (Key.Caption = Sector.Caption) And (Key.Enabled = Sector.Enabled) And
    (Key.InnerRadius = Sector.InnerRadius) And (Key.OuterRadius = Sector.OuterRadius) And
    (Key.StartAngle = Sector.StartAngle) And (Key.Sweep = Sector.Sweep);
End;

Constructor TRadialRenderCache.Create;
Begin
  inherited Create;
  Sectors := TList.Create;
  Font := TFont.Create;
End;

Destructor TRadialRenderCache.Destroy;
Var
  I: Integer;
Begin
  If Sectors <> nil Then
    For I := 0 To Sectors.Count - 1 Do TObject(Sectors[I]).Free;
  Sectors.Free;
  Font.Free;
  inherited Destroy;
End;

Procedure TRadialRenderCache.BeginUpdate(AScale: Double);
Var
  I: Integer;
  CurrentFont: TFont;
  MenuRGB, TextRGB, BorderRGB: TRGB;
Begin
  CurrentFont := Screen.MenuFont;
  If (Scale <> AScale) Or Not Font.IsEqual(CurrentFont) Or
    (MenuColor <> Graphics.ColorToRGB(clMenu)) Or
    (TextColor <> Graphics.ColorToRGB(clMenuText)) Or
    (SelectedColor <> Graphics.ColorToRGB(clMenuHighlight)) Or
    (SelectedText <> Graphics.ColorToRGB(clHighlightText)) Or
    (DisabledText <> Graphics.ColorToRGB(clGrayText)) Then Begin
    For I := 0 To Sectors.Count - 1 Do TObject(Sectors[I]).Free;
    Sectors.Clear;
  End;
  Scale := AScale;
  Font.Assign(CurrentFont);
  MenuColor := Graphics.ColorToRGB(clMenu);
  TextColor := Graphics.ColorToRGB(clMenuText);
  SelectedColor := Graphics.ColorToRGB(clMenuHighlight);
  SelectedText := Graphics.ColorToRGB(clHighlightText);
  DisabledText := Graphics.ColorToRGB(clGrayText);
  MenuRGB := ugraphics.ColorToRGB(MenuColor);
  TextRGB := ugraphics.ColorToRGB(TextColor);
  BorderRGB.R := (3 * MenuRGB.R + TextRGB.R) Div 4;
  BorderRGB.G := (3 * MenuRGB.G + TextRGB.G) Div 4;
  BorderRGB.B := (3 * MenuRGB.B + TextRGB.B) Div 4;
  BorderColor := RGBToColor(BorderRGB);
  For I := 0 To Sectors.Count - 1 Do TRadialSectorImage(Sectors[I]).Used := False;
End;

Procedure TRadialRenderCache.EndUpdate;
Var
  I: Integer;
Begin
  For I := Sectors.Count - 1 Downto 0 Do
    If Not TRadialSectorImage(Sectors[I]).Used Then Begin
      TObject(Sectors[I]).Free;
      Sectors.Delete(I);
    End;
End;

Function TRadialRenderCache.Find(Const Sector: TRadialMenuSector): TRadialSectorImage;
Var
  I: Integer;
Begin
  For I := 0 To Sectors.Count - 1 Do Begin
    Result := TRadialSectorImage(Sectors[I]);
    If Result.Matches(Sector) Then Begin
      Result.Used := True;
      Exit;
    End;
  End;
  Result := nil;
End;

Function IsSectorItem(Item: TMenuItem): Boolean;
Begin
  Result := Item.Visible And Not Item.IsLine;
End;

Function VisibleDepth(AParent: TMenuItem): Integer;
Var
  I, Depth: Integer;
Begin
  Result := 0;
  For I := 0 To AParent.Count - 1 Do
    If IsSectorItem(AParent[I]) Then Begin
      Depth := 1;
      If AParent[I].Enabled Then
        Inc(Depth, VisibleDepth(AParent[I]));
      If Depth > Result Then Result := Depth;
    End;
End;

Constructor TRadialMenuForm.Create(AOwner: TComponent);
Var
  Depth: Integer;
Begin
  inherited CreateNew(AOwner);
  Caption := 'RadialTreeMenu';
  BorderStyle := bsNone;
  Position := poDesigned;
  ShowInTaskBar := stNever;
  KeyPreview := True;
  ControlStyle := ControlStyle - [csCaptureMouse];
  // Geometry is scaled below; LCL must not scale the form a second time.
  Scaled := False;
  FScale := Screen.PixelsPerInch / 96;
  Depth := VisibleDepth(TRadialTreeMenu(AOwner).Items);
  If Depth = 0 Then Depth := 1;
  FCenter := Round((72 + (Depth - 1) * 50) * FScale);
  SetBounds(0, 0, 2 * FCenter, 2 * FCenter);
  FMask := TBitmap.Create;
  FImage := TBitmap.Create;
  FMask.Monochrome := True;
  FMask.SetSize(Width, Height);
  FImage.SetSize(Width, Height);
  OnShow := @WindowShown;
  OnHide := @WindowHidden;
  OnDeactivate := @WindowDeactivated;
  BuildMenu(TRadialTreeMenu(AOwner).Items, -Pi / 2, 2 * Pi, -1, 0);
  If TRadialTreeMenu(AOwner).FRenderCache = nil Then
    TRadialTreeMenu(AOwner).FRenderCache := TRadialRenderCache.Create;
  FCache := TRadialRenderCache(TRadialTreeMenu(AOwner).FRenderCache);
  PrepareImages;
  Rebuild;
End;

Function TRadialMenuForm.HasItems: Boolean;
Begin
  Result := Length(FAllSectors) <> 0;
End;

Destructor TRadialMenuForm.Destroy;
Begin
  Application.RemoveAsyncCalls(Self);
  ReleasePointer;
  {$IF defined(LCLGTK3) or defined(LCLGTK2)}
  If HandleAllocated Then DestroyHandle;
  {$ENDIF}
  FMask.Free;
  FImage.Free;
  inherited Destroy;
End;

{$IFDEF LCLGTK3}
Procedure TRadialMenuForm.CreateWnd;
Var
  NativeWidget: PGtkWidget;
  NativeWindow: PGdkWindow;
  Visual: PGdkVisual;
Begin
  inherited CreateWnd;
  NativeWidget := gtk_widget_get_toplevel(TGtk3Widget(Handle).Widget);
  Visual := gdk_screen_get_rgba_visual(gtk_widget_get_screen(NativeWidget));
  If (Visual = nil) Or
    (Not gdk_screen_is_composited(gtk_widget_get_screen(NativeWidget))) Then
    Raise EInvalidOperation.Create('GTK3 radial popup requires RGBA compositing');
  // LCL realizes the widget during handle creation. Change its visual before
  // the first mapping, using GTK's unrealize/set_visual/realize lifecycle.
  gtk_widget_unrealize(NativeWidget);
  gtk_widget_set_visual(NativeWidget, Visual);
  gtk_widget_set_app_paintable(NativeWidget, True);
  gtk_window_set_type_hint(PGtkWindow(NativeWidget),
    GDK_WINDOW_TYPE_HINT_POPUP_MENU);
  gtk_widget_realize(NativeWidget);
  NativeWindow := gtk_widget_get_window(NativeWidget);
  gdk_window_set_override_redirect(NativeWindow, True);
  FDrawSignal := g_signal_connect_data(NativeWidget, 'draw',
    TGCallback(@DrawNative), Self, nil, G_CONNECT_DEFAULT);
End;

Procedure TRadialMenuForm.DestroyWnd;
Var
  NativeWidget: PGtkWidget;
Begin
  Application.RemoveAsyncCalls(Self);
  ReleasePointer;
  If FDrawSignal <> 0 Then Begin
    NativeWidget := gtk_widget_get_toplevel(TGtk3Widget(Handle).Widget);
    g_signal_handler_disconnect(NativeWidget, FDrawSignal);
    FDrawSignal := 0;
  End;
  inherited DestroyWnd;
End;

Class Function TRadialMenuForm.DrawNative(Widget: PGtkWidget;
  Context: Pcairo_t; Data: gpointer): gboolean; cdecl;
Var
  Window: TRadialMenuForm;
  I, J: Integer;
  Image: TGtk3Image;
Begin
  Window := TRadialMenuForm(Data);
  cairo_save(Context);
  cairo_set_operator(Context, CAIRO_OPERATOR_SOURCE);
  cairo_set_source_rgba(Context, 0, 0, 0, 0);
  cairo_paint(Context);
  cairo_new_path(Context);
  For I := 0 To High(Window.FSectors) Do Begin
    With Window.FSectors[I] Do Begin
      cairo_move_to(Context, Polygon[0].x, Polygon[0].y);
      For J := 1 To High(Polygon) Do
        cairo_line_to(Context, Polygon[J].x, Polygon[J].y);
      cairo_close_path(Context);
    End;
  End;
  cairo_clip(Context);
  Image := TGtk3Image(Window.FImage.Handle);
  Image.UpdatePixbufFromSurface;
  gdk_cairo_set_source_pixbuf(Context, Image.Handle, 0, 0);
  cairo_paint(Context);
  cairo_restore(Context);
  Result := True;
End;

Procedure TRadialMenuForm.ApplyInputShape;
Var
  NativeWidget: PGtkWidget;
  Mask: TGtk3Image;
  AlphaMask: PGdkPixbuf;
  Surface: Pcairo_surface_t;
  Region: Pcairo_region_t;
Begin
  NativeWidget := gtk_widget_get_toplevel(TGtk3Widget(Handle).Widget);
  Mask := TGtk3Image(FMask.Handle);
  Mask.UpdatePixbufFromSurface;
  AlphaMask := Mask.Handle^.add_alpha(True, 0, 0, 0);
  Surface := gdk_cairo_surface_create_from_pixbuf(AlphaMask, 1,
    gtk_widget_get_window(NativeWidget));
  AlphaMask^.unref;
  Region := gdk_cairo_region_create_from_surface(Surface);
  gdk_window_input_shape_combine_region(gtk_widget_get_window(NativeWidget),
    Region, 0, 0);
  cairo_region_destroy(Region);
  cairo_surface_destroy(Surface);
End;

Procedure TRadialMenuForm.GrabPointer(Data: PtrInt);
Var
  NativeWidget: PGtkWidget;
  NativeWindow: PGdkWindow;
  Seat: PGdkSeat;
  Status: TGdkGrabStatus;
Begin
  If (Not Visible) Or (ModalResult <> 0) Then Exit;
  ApplyInputShape;
  // LCL ignores motion on the outer GtkWindow; grab its paint/input client.
  NativeWidget := TGtk3Widget(Handle).GetContainerWidget;
  NativeWindow := gtk_widget_get_window(NativeWidget);
  Seat := gdk_display_get_default_seat(gtk_widget_get_display(NativeWidget));
  If (NativeWindow = nil) Or (Seat = nil) Then Begin
    ModalResult := mrCancel;
    Raise EInvalidOperation.Create('GTK3 popup has no native window or input seat');
  End;
  Status := gdk_seat_grab(Seat, NativeWindow,
    [GDK_SEAT_CAPABILITY_POINTER, GDK_SEAT_CAPABILITY_KEYBOARD],
    False, nil, nil, nil, nil);
  If Status <> GDK_GRAB_SUCCESS Then Begin
    ModalResult := mrCancel;
    Raise EInvalidOperation.CreateFmt('GTK3 popup input grab failed (status %d)',
      [Ord(Status)]);
  End;
  FSeat := Seat;
End;

Procedure TRadialMenuForm.ReleasePointer;
Begin
  If FSeat <> nil Then Begin
    gdk_seat_ungrab(FSeat);
    FSeat := nil;
  End;
End;
{$ENDIF}

{$IFDEF LCLGTK2}
Procedure TRadialMenuForm.CreateWnd;
Var
  NativeWidget: PGtkWidget;
Begin
  inherited CreateWnd;
  NativeWidget := gtk_widget_get_toplevel(PGtkWidget(Handle));
  If NativeWidget = nil Then
    Raise EInvalidOperation.Create('GTK2 popup has no input client');
  ConnectSignalAfter(PGtkObject(NativeWidget), 'map-event', @InputMapped,
    Self, GDK_STRUCTURE_MASK);
End;

Class Function TRadialMenuForm.InputMapped(Widget: PGtkWidget; Event: PGdkEvent;
  Data: Pointer): LongInt; cdecl;
Var
  Window: TRadialMenuForm;
Begin
  Window := TRadialMenuForm(Data);
  If Window.Visible And (Window.ModalResult = mrNone) And
    Not Window.FPointerGrabbed Then
    Application.QueueAsyncCall(@Window.GrabPointer, 0);
  Result := 0;
End;

Procedure TRadialMenuForm.GrabPointer(Data: PtrInt);
Var
  NativeWidget: PGtkWidget;
  NativeWindow: PGdkWindow;
  Status: TGdkGrabStatus;
Begin
  If (Not Visible) Or (ModalResult <> 0) Or FPointerGrabbed Then Exit;
  NativeWidget := GetDefaultMouseCaptureWidget(PGtkWidget(Handle));
  If NativeWidget = nil Then Begin
    ModalResult := mrCancel;
    Raise EInvalidOperation.Create('GTK2 popup has no input client');
  End;
  NativeWindow := NativeWidget^.window;
  If NativeWindow = nil Then Begin
    ModalResult := mrCancel;
    Raise EInvalidOperation.Create('GTK2 popup has no native input window');
  End;
  Status := gdk_pointer_grab(NativeWindow, False,
    GDK_POINTER_MOTION_MASK Or GDK_BUTTON_PRESS_MASK Or GDK_BUTTON_RELEASE_MASK,
    nil, nil, GDK_CURRENT_TIME);
  If Status <> GDK_GRAB_SUCCESS Then Begin
    ModalResult := mrCancel;
    Raise EInvalidOperation.CreateFmt('GTK2 popup pointer grab failed (status %d)',
      [Ord(Status)]);
  End;
  Status := gdk_keyboard_grab(NativeWindow, False, GDK_CURRENT_TIME);
  If Status <> GDK_GRAB_SUCCESS Then Begin
    gdk_pointer_ungrab(GDK_CURRENT_TIME);
    ModalResult := mrCancel;
    Raise EInvalidOperation.CreateFmt('GTK2 popup keyboard grab failed (status %d)',
      [Ord(Status)]);
  End;
  FPointerGrabbed := True;
  RefreshCompositorShadow;
  ActivePopupMenu := TRadialTreeMenu(Owner);
End;

Procedure TRadialMenuForm.ReleasePointer;
Begin
  If FPointerGrabbed Then Begin
    gdk_pointer_ungrab(GDK_CURRENT_TIME);
    gdk_keyboard_ungrab(GDK_CURRENT_TIME);
    FPointerGrabbed := False;
  End;
End;

Procedure TRadialMenuForm.DestroyWnd;
Begin
  Application.RemoveAsyncCalls(Self);
  ReleasePointer;
  inherited DestroyWnd;
End;
{$ENDIF}

{$IF not defined(LCLGTK2) and not defined(LCLGTK3)}
Procedure TRadialMenuForm.GrabPointer(Data: PtrInt);
Begin
  If (Not Visible) Or (ModalResult <> mrNone) Then Exit;
  // A widgetset may have dropped the capture without telling the LCL;
  // only a release makes the LCL request it again.
  MouseCapture := False;
  MouseCapture := True;
  RefreshCompositorShadow;
End;

Procedure TRadialMenuForm.ReleasePointer;
Begin
  MouseCapture := False;
End;

Procedure TRadialMenuForm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
  inherited MouseUp(Button, Shift, X, Y);
  // Qt releases every capture when a button is released on a form.
  Application.QueueAsyncCall(@GrabPointer, 0);
End;
{$ENDIF}

Function TRadialMenuForm.AddSector(AInnerRadius, AOuterRadius: Integer;
  AStartAngle, ASweep: Double; Const ACaption: String;
  ALevel, AParentIndex: Integer; AEnabled: Boolean): Integer;
Const
  Steps = 60;
Var
  N, I: Integer;
  Angle, Radius: Double;
Begin
  N := Length(FAllSectors);
  SetLength(FAllSectors, N + 1);
  With FAllSectors[N] Do Begin
    Caption := ACaption;
    Level := ALevel;
    Index := N;
    ParentIndex := AParentIndex;
    Enabled := AEnabled;
    InnerRadius := AInnerRadius;
    OuterRadius := AOuterRadius;
    StartAngle := AStartAngle;
    Sweep := ASweep;
    SetLength(Polygon, 2 * (Steps + 1));
    For I := 0 To Steps Do Begin
      Angle := StartAngle + Sweep * I / Steps;
      Polygon[I].x := FCenter + Cos(Angle) * OuterRadius * FScale;
      Polygon[I].y := FCenter + Sin(Angle) * OuterRadius * FScale;
      Angle := StartAngle + Sweep * (Steps - I) / Steps;
      Polygon[Steps + 1 + I].x := FCenter + Cos(Angle) * InnerRadius * FScale;
      Polygon[Steps + 1 + I].y := FCenter + Sin(Angle) * InnerRadius * FScale;
    End;
    Angle := StartAngle + Sweep / 2;
    Radius := (InnerRadius + OuterRadius) / 2 * FScale;
    LabelPosition := Point(Round(FCenter + Cos(Angle) * Radius),
      Round(FCenter + Sin(Angle) * Radius));
  End;
  Result := N;
End;

Procedure TRadialMenuForm.BuildMenu(AParent: TMenuItem; StartAngle, Sweep: Double;
  AParentIndex, ALevel: Integer);
Var
  I, Count, ItemPosition, SectorIndex, InnerRadius, OuterRadius: Integer;
  Item: TMenuItem;
  Angle, ItemSweep: Double;
Begin
  Count := 0;
  For I := 0 To AParent.Count - 1 Do
    If IsSectorItem(AParent[I]) Then Inc(Count);
  If Count = 0 Then Exit;
  ItemSweep := Sweep / Count;
  ItemPosition := 0;
  If ALevel = 0 Then
    InnerRadius := 0
  Else
    InnerRadius := 70 + (ALevel - 1) * 50;
  OuterRadius := 70 + ALevel * 50;
  For I := 0 To AParent.Count - 1 Do Begin
    Item := AParent[I];
    If Not IsSectorItem(Item) Then Continue;
    Angle := StartAngle + ItemPosition * ItemSweep;
    SectorIndex := AddSector(InnerRadius, OuterRadius, Angle, ItemSweep,
      Item.Caption, ALevel, AParentIndex, Item.Enabled);
    SetLength(FItems, SectorIndex + 1);
    FItems[SectorIndex] := Item;
    If Item.Enabled Then
      BuildMenu(Item, Angle, ItemSweep, SectorIndex, ALevel + 1);
    Inc(ItemPosition);
  End;
End;

Procedure TRadialMenuForm.PrepareImages;
Var
  I, J, Margin, X, Y: Integer;
  Image: TRadialSectorImage;
Begin
  FCache.BeginUpdate(FScale);
  Margin := Round(FScale) + 2;
  For I := 0 To High(FAllSectors) Do Begin
    Image := FCache.Find(FAllSectors[I]);
    If Image = nil Then Begin
      Image := TRadialSectorImage.Create;
      Try
        Image.Key := FAllSectors[I];
        With FAllSectors[I].Polygon[0] Do
          Image.Bounds := Rect(Round(X) - FCenter, Round(Y) - FCenter,
            Round(X) - FCenter, Round(Y) - FCenter);
        For J := 1 To High(FAllSectors[I].Polygon) Do Begin
          X := Round(FAllSectors[I].Polygon[J].X) - FCenter;
          Y := Round(FAllSectors[I].Polygon[J].Y) - FCenter;
          If X < Image.Bounds.Left Then Image.Bounds.Left := X;
          If X > Image.Bounds.Right Then Image.Bounds.Right := X;
          If Y < Image.Bounds.Top Then Image.Bounds.Top := Y;
          If Y > Image.Bounds.Bottom Then Image.Bounds.Bottom := Y;
        End;
        InflateRect(Image.Bounds, Margin, Margin);
        Inc(Image.Bounds.Right);
        Inc(Image.Bounds.Bottom);
        RenderSector(Image, False);
        RenderSector(Image, True);
        Image.Used := True;
        FCache.Sectors.Add(Image);
        Inc(FCache.RenderCount);
      Except
        Image.Free;
        Raise;
      End;
    End;
    FAllSectors[I].Image := Image;
  End;
  FCache.EndUpdate;
End;

Procedure TRadialMenuForm.RenderSector(Image: TRadialSectorImage; Selected: Boolean);
Var
  Source, Mask: TBitmap;
  Points: Array Of TPoint;
  Offset: TPoint;
  I: Integer;
Begin
  Points := nil;
  Offset := Point(FCenter + Image.Bounds.Left, FCenter + Image.Bounds.Top);
  Source := TBitmap.Create;
  Try
    Mask := TBitmap.Create;
    Try
      Source.PixelFormat := pf24Bit;
      Source.SetSize((Image.Bounds.Right - Image.Bounds.Left) * RenderScale,
        (Image.Bounds.Bottom - Image.Bounds.Top) * RenderScale);
      Mask.SetSize(Source.Width, Source.Height);
      // Uncovered samples share the outline color, so averaging them into
      // edge pixels cannot tint the outline.
      Source.Canvas.Brush.Color := FCache.BorderColor;
      Source.Canvas.FillRect(Rect(0, 0, Source.Width, Source.Height));
      Mask.Canvas.Brush.Color := clBlack;
      Mask.Canvas.FillRect(Rect(0, 0, Mask.Width, Mask.Height));
      SetLength(Points, Length(Image.Key.Polygon));
      For I := 0 To High(Points) Do Begin
        Points[I].X := Round((Image.Key.Polygon[I].X - Offset.X) * RenderScale);
        Points[I].Y := Round((Image.Key.Polygon[I].Y - Offset.Y) * RenderScale);
      End;
      Source.Canvas.Pen.Color := FCache.BorderColor;
      Source.Canvas.Pen.Width := Round(2 * FScale * RenderScale);
      Source.Canvas.Brush.Color := FCache.MenuColor;
      If Selected Then Source.Canvas.Brush.Color := FCache.SelectedColor;
      Source.Canvas.Polygon(Points);
      Mask.Canvas.Pen.Color := clWhite;
      Mask.Canvas.Brush.Color := clWhite;
      Mask.Canvas.Pen.Width := Source.Canvas.Pen.Width;
      Mask.Canvas.Polygon(Points);
      // Windows GDI text output clears the alpha channel. The caption is
      // therefore drawn on the opaque image; coverage becomes alpha afterwards.
      DownsampleOpaque(Source, Source, RenderScale);
      DownsampleOpaque(Mask, Mask, RenderScale);
      With Source.Canvas Do Begin
        Font.Assign(FCache.Font);
        Font.Color := FCache.TextColor;
        If Selected Then Font.Color := FCache.SelectedText;
        If Not Image.Key.Enabled Then Font.Color := FCache.DisabledText;
        Brush.Style := bsClear;
      End;
      DrawCaption(Image.Key, Source, Offset);
      Image.Bitmaps[Selected] := TBitmap.Create;
      DownsampleMasked(Image.Bitmaps[Selected], Source, Mask, 1);
    Finally
      Mask.Free;
    End;
  Finally
    Source.Free;
  End;
End;

Procedure TRadialMenuForm.Rebuild;
Var
  I, J, N: Integer;
  SectorVisible, ShapeChanged, Selected: Boolean;
  PreviousSectors: Array Of Integer;
  Points: Array Of TPoint;
  Layers: Array Of TBitmapLayer;
Begin
  PreviousSectors := nil;
  Points := nil;
  Layers := nil;
  SetLength(PreviousSectors, Length(FSectors));
  For I := 0 To High(FSectors) Do PreviousSectors[I] := FSectors[I].Index;
  FSectors := nil;
  For I := 0 To High(FAllSectors) Do Begin
    SectorVisible := FAllSectors[I].Level = 0;
    If Not SectorVisible And (FAllSectors[I].Level <= Length(FPath)) Then
      SectorVisible := FAllSectors[I].ParentIndex = FPath[FAllSectors[I].Level - 1];
    If SectorVisible Then Begin
      N := Length(FSectors);
      SetLength(FSectors, N + 1);
      FSectors[N] := FAllSectors[I];
    End;
  End;
  ShapeChanged := Length(PreviousSectors) <> Length(FSectors);
  If Not ShapeChanged Then
    For I := 0 To High(FSectors) Do
      If PreviousSectors[I] <> FSectors[I].Index Then ShapeChanged := True;
  If ShapeChanged Then Begin
    FMask.Canvas.Brush.Color := clBlack;
    FMask.Canvas.FillRect(Rect(0, 0, Width, Height));
  End;
  SetLength(Layers, Length(FSectors));
  For I := 0 To High(FSectors) Do Begin
    If ShapeChanged Then Begin
      SetLength(Points, Length(FSectors[I].Polygon));
      For J := 0 To High(Points) Do Points[J] := FSectors[I].Polygon[J];
      FMask.Canvas.Brush.Color := clWhite;
      FMask.Canvas.Pen.Color := clWhite;
      FMask.Canvas.Polygon(Points);
    End;
    Selected := (FSectors[I].Level < Length(FPath)) And
      (FPath[FSectors[I].Level] = FSectors[I].Index);
    Layers[I].Bitmap := FSectors[I].Image.Bitmaps[Selected];
    Layers[I].Position := Point(FCenter + FSectors[I].Image.Bounds.Left,
      FCenter + FSectors[I].Image.Bounds.Top);
  End;
  ComposeBitmapLayers(FImage, FCache.MenuColor, Layers);
  If HandleAllocated Then Begin
    {$IFDEF LCLGTK3}
    If ShapeChanged Then ApplyInputShape;
    gtk_widget_queue_draw(TGtk3Widget(Handle).Widget);
    {$ELSE}
    If ShapeChanged Then SetShape(FMask);
    {$ENDIF}
  End;
  Invalidate;
End;

Procedure TRadialMenuForm.DrawCaption(Const Sector: TRadialMenuSector;
  Dest: TBitmap; AOffset: TPoint);
Var
  Points: Array Of TPoint;
  I: Integer;
  ClipRegion: HRGN;
  Style: TTextStyle;
Begin
  Points := nil;
  SetLength(Points, Length(Sector.Polygon));
  For I := 0 To High(Points) Do Begin
    Points[I].X := Round(Sector.Polygon[I].X) - AOffset.X;
    Points[I].Y := Round(Sector.Polygon[I].Y) - AOffset.Y;
  End;
  ClipRegion := CreatePolygonRgn(@Points[0], Length(Points), WINDING);
  If ClipRegion = 0 Then
    Raise EInvalidOperation.Create('Cannot create the caption clip region');
  Try
    Dest.Canvas.SaveHandleState;
    Try
      If SelectClipRgn(Dest.Canvas.Handle, ClipRegion) = ERROR Then
        Raise EInvalidOperation.Create('Cannot apply the caption clip region');
      Style := Dest.Canvas.TextStyle;
      Style.Alignment := taCenter;
      Style.Layout := tlCenter;
      Style.SingleLine := False;
      Style.WordBreak := False;
      Style.EndEllipsis := False;
      Style.ShowPrefix := False;
      Style.Opaque := False;
      // Keep the polygon clip; TextRect must not replace it with a rectangle.
      Style.Clipping := False;
      With Sector.LabelPosition Do
        Dest.Canvas.TextRect(Rect(X - AOffset.X - Width, Y - AOffset.Y - Height,
          X - AOffset.X + Width, Y - AOffset.Y + Height),
          0, 0, Sector.Caption, Style);
    Finally
      Dest.Canvas.RestoreHandleState;
    End;
  Finally
    DeleteObject(ClipRegion);
  End;
End;

Function TRadialMenuForm.HitTest(X, Y: Integer): Integer;
Var
  I: Integer;
  P: TVector2;
Begin
  P.x := X;
  P.y := Y;
  For I := High(FSectors) Downto 0 Do
    If PointInPolygon(P, FSectors[I].Polygon) Then Exit(I);
  Result := -1;
End;

Procedure TRadialMenuForm.Paint;
Begin
  {$IFNDEF LCLGTK3}
  Canvas.Draw(0, 0, FImage);
  {$ENDIF}
  {$IF not defined(LCLGTK2) and not defined(LCLGTK3)}
  // Capture needs a window that is on screen; OnShow fires before that. Qt
  // would otherwise capture a widget that drops pointer motion.
  If Not FGrabQueued Then Begin
    FGrabQueued := True;
    Application.QueueAsyncCall(@GrabPointer, 0);
  End;
  {$ENDIF}
End;

{$IFNDEF LCLGTK3}
// Marco/Metacity give shaped windows no shadow, but decide when the window is
// created, before its shape exists, and then keep a rectangular shadow. Call
// this once the window is on screen.
Procedure TRadialMenuForm.RefreshCompositorShadow;
Var
  Hole: TRect;
Begin
  If FShadowRefreshed Or
    Not (WidgetSet.LCLPlatform In [lpGtk2, lpQt, lpQt5, lpQt6]) Then Exit;
  FShadowRefreshed := True;
  // Shape changes are only reported for windows that are on screen, and an
  // unchanged shape may be skipped: punch a one-pixel hole and close it again.
  Hole := Rect(FCenter, FCenter, FCenter + 1, FCenter + 1);
  FMask.Canvas.Brush.Color := clBlack;
  FMask.Canvas.FillRect(Hole);
  SetShape(FMask);
  FMask.Canvas.Brush.Color := clWhite;
  FMask.Canvas.FillRect(Hole);
  SetShape(FMask);
  // An opacity change is one of the few events that make the compositor
  // decide again, now knowing the shape.
  AlphaBlendValue := 254;
  AlphaBlend := True;
  AlphaBlend := False;
End;
{$ENDIF}

Function TRadialMenuForm.IsBranch(SectorIndex: Integer): Boolean;
Var
  I: Integer;
Begin
  For I := 0 To High(FAllSectors) Do
    If FAllSectors[I].ParentIndex = SectorIndex Then Exit(True);
  Result := False;
End;

Procedure TRadialMenuForm.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  Hit, Level, Index: Integer;
  Opened: Boolean;
Begin
  inherited MouseMove(Shift, X, Y);
  // A pending close must not open branches or notify their items.
  If ModalResult <> mrNone Then Exit;
  Hit := HitTest(X, Y);
  If Hit < 0 Then Exit;
  If Not FSectors[Hit].Enabled Then Exit;
  Level := FSectors[Hit].Level;
  Index := FSectors[Hit].Index;
  If (Length(FPath) = Level + 1) And (FPath[Level] = Index) Then Exit;
  // Returning from a descendant keeps the branch open; that is not a new opening.
  Opened := (Length(FPath) <= Level) Or (FPath[Level] <> Index);
  SetLength(FPath, Level + 1);
  FPath[Level] := Index;
  Rebuild;
  // Must stay the last statement: the handler may change the tree or close the menu.
  If Opened And IsBranch(Index) Then FItems[Index].Click;
End;

Procedure TRadialMenuForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  Hit: Integer;
Begin
  inherited MouseDown(Button, Shift, X, Y);
  // A pending close, e.g. after a tree mutation, must not become a selection.
  If ModalResult <> mrNone Then Exit;
  Hit := HitTest(X, Y);
  If (Button = mbRight) Or (Hit < 0) Then
    ModalResult := mrCancel
  Else If (Button = mbLeft) And FSectors[Hit].Enabled And
    Not IsBranch(FSectors[Hit].Index) Then Begin
    FSelectedItem := FItems[FSectors[Hit].Index];
    ModalResult := mrOK;
  End;
End;

Procedure TRadialMenuForm.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  inherited KeyDown(Key, Shift);
  If Key = VK_ESCAPE Then Begin
    Key := 0;
    ModalResult := mrCancel;
  End;
End;

Procedure TRadialMenuForm.WindowShown(Sender: TObject);
Begin
  {$IFNDEF LCLGTK2}
  ActivePopupMenu := TRadialTreeMenu(Owner);
  {$ENDIF}
  {$IFDEF LCLGTK3}
  ApplyInputShape;
  Application.QueueAsyncCall(@GrabPointer, 0);
  {$ELSE}
  SetShape(FMask);
  {$ENDIF}
End;

Procedure TRadialMenuForm.WindowHidden(Sender: TObject);
Begin
  Application.RemoveAsyncCalls(Self);
  ReleasePointer;
End;

Procedure TRadialMenuForm.WindowDeactivated(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TRadialTreeMenu.DoClose;
Begin
  If Assigned(FWindow) Then
    FWindow.ModalResult := mrCancel
  Else
    inherited DoClose;
End;

Destructor TRadialTreeMenu.Destroy;
Begin
  FRenderCache.Free;
  inherited Destroy;
End;

Function TRadialTreeMenu.GetCachedSectorRenderCount: QWord;
Begin
  Result := 0;
  If FRenderCache <> nil Then Result := TRadialRenderCache(FRenderCache).RenderCount;
End;

Function TRadialTreeMenu.GetCachedSectorCount: Integer;
Begin
  Result := 0;
  If FRenderCache <> nil Then Result := TRadialRenderCache(FRenderCache).Sectors.Count;
End;

Function CreateRadialItem(AParent: TMenuItem; AOwner: TComponent;
  Const ACaption: String): TRadialTreeMenuItem;
Begin
  Result := TRadialTreeMenuItem.Create(AOwner);
  Try
    Result.Caption := ACaption;
    AParent.Add(Result);
  Except
    Result.Free;
    Raise;
  End;
End;

Function TRadialTreeMenuItem.Add(Const ACaption: String): TRadialTreeMenuItem;
Begin
  Result := CreateRadialItem(Self, Self, ACaption);
End;

Procedure TRadialTreeMenuItem.Release(Item: TMenuItem);
Begin
  If (Item = nil) Or (IndexOf(Item) < 0) Then
    Raise EMenuError.Create('Release requires a direct child of this item');
  Remove(Item);
  Item.Free;
End;

Function TRadialTreeMenu.Add(Const ACaption: String): TRadialTreeMenuItem;
Begin
  Result := CreateRadialItem(Items, Self, ACaption);
End;

Procedure TRadialTreeMenu.Release(Item: TMenuItem);
Begin
  If (Item = nil) Or (Item = Items) Then
    Raise EMenuError.Create('Release requires an attached menu element');
  If Item.GetParentMenu <> Self Then
    Raise EMenuError.Create('Cannot release an element outside this menu');
  CancelForMutation;
  Item.Parent.Remove(Item);
  Item.Free;
End;

Procedure TRadialTreeMenu.Clear;
Begin
  CancelForMutation;
  Items.Clear;
End;

Procedure TRadialTreeMenu.CancelForMutation;
Begin
  If Assigned(FWindow) Then FWindow.ModalResult := mrCancel;
End;

Procedure TRadialTreeMenu.DoChange(Source: TMenuItem; Rebuild: Boolean);
Begin
  CancelForMutation;
  inherited DoChange(Source, Rebuild);
End;

Procedure TRadialTreeMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
Var
  I: Integer;
Begin
  If Operation = opRemove Then
    For I := 0 To High(FItemStates) Do
      If FItemStates[I].Item = AComponent Then Begin
        FItemStates[I].Item := nil;
        CancelForMutation;
      End;
  inherited Notification(AComponent, Operation);
End;

Procedure TRadialTreeMenu.WatchItems(AParent: TMenuItem);
Var
  I, Index: Integer;
Begin
  For I := 0 To AParent.Count - 1 Do Begin
    Index := Length(FItemStates);
    SetLength(FItemStates, Index + 1);
    With FItemStates[Index] Do Begin
      Item := AParent[I];
      Caption := Item.Caption;
      Count := Item.Count;
      Enabled := Item.Enabled;
      Visible := Item.Visible;
    End;
    AParent[I].FreeNotification(Self);
    WatchItems(AParent[I]);
  End;
End;

Procedure TRadialTreeMenu.UnwatchItems;
Var
  I: Integer;
Begin
  For I := 0 To High(FItemStates) Do
    If FItemStates[I].Item <> nil Then
      FItemStates[I].Item.RemoveFreeNotification(Self);
  SetLength(FItemStates, 0);
End;

Function TRadialTreeMenu.ItemsUnchanged(AParent: TMenuItem;
  Var Index: Integer): Boolean;
Var
  I: Integer;
  CurrentItem: TMenuItem;
Begin
  Result := False;
  For I := 0 To AParent.Count - 1 Do Begin
    If Index >= Length(FItemStates) Then Exit;
    CurrentItem := AParent[I];
    With FItemStates[Index] Do
      If (Item <> CurrentItem) Or (Caption <> CurrentItem.Caption) Or
        (Count <> CurrentItem.Count) Or (Enabled <> CurrentItem.Enabled) Or
        (Visible <> CurrentItem.Visible) Then Exit;
    Inc(Index);
    If Not ItemsUnchanged(CurrentItem, Index) Then Exit;
  End;
  Result := True;
End;

Procedure TRadialTreeMenu.CheckForMutation(Sender: TObject; Var Done: Boolean);
Var
  Index: Integer;
Begin
  If (FWindow = nil) Or (FWindow.ModalResult <> mrNone) Then Exit;
  Index := 0;
  If Not ItemsUnchanged(Items, Index) Or (Index <> Length(FItemStates)) Then Begin
    CancelForMutation;
    Done := False;
  End;
End;

Procedure TRadialTreeMenu.PopUp(X, Y: Integer);
Var
  Index: Integer;
  Clicked: TMenuItem;
  Guard: TClickGuard;
Begin
  If Assigned(FWindow) Then Exit;
  Clicked := nil;
  If Assigned(ActivePopupMenu) Then ActivePopupMenu.Close;
  // PopupPoint is writable for descendants only since Lazarus 4.0.
  {$IF LCL_FULLVERSION >= 4000000}
  SetPopupPoint(Point(X, Y));
  {$ENDIF}
  DoPopup(Self);
  FWindow := TRadialMenuForm.Create(Self);
  Try
    If Not TRadialMenuForm(FWindow).HasItems Then Exit;
    WatchItems(Items);
    Application.AddOnIdleHandler(@CheckForMutation, False);
    FWindow.PopupMode := pmExplicit;
    If PopupComponent Is TControl Then
      FWindow.PopupParent := GetParentForm(TControl(PopupComponent))
    Else If Parent Is TCustomForm Then
      FWindow.PopupParent := TCustomForm(Parent);
    FWindow.Left := X - FWindow.Width Div 2;
    FWindow.Top := Y - FWindow.Height Div 2;
    If FWindow.ShowModal = mrOK Then Begin
      // Drop the click if a native change was not detected before it.
      Index := 0;
      If ItemsUnchanged(Items, Index) And (Index = Length(FItemStates)) Then
        Clicked := TRadialMenuForm(FWindow).SelectedItem;
    End;
  Finally
    Application.RemoveOnIdleHandler(@CheckForMutation);
    UnwatchItems;
    If ActivePopupMenu = Self Then ActivePopupMenu := nil;
    FreeAndNil(FWindow);
  End;
  If Clicked = nil Then Begin
    DoClose;
    Exit;
  End;
  // OnClose may destroy the item or this menu; only the guard is used afterwards.
  Guard := TClickGuard.Create(nil);
  Try
    Guard.Item := Clicked;
    Clicked.FreeNotification(Guard);
    DoClose;
    If Guard.Item <> nil Then Guard.Item.Click;
  Finally
    Guard.Free;
  End;
End;

End.

