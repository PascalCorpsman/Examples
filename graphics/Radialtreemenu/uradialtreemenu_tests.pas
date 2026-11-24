Unit uradialtreemenu_tests;

{$MODE objfpc}{$H+}

Interface

Function RunRadialMenuItemTests: Integer;
Procedure ConfigureTestDPI;
Procedure ConfigureCaptionTest;
Procedure ConfigureClickTest;
Procedure ConfigureFullRingTest;

Implementation

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Menus, Graphics, LCLIntf, LMessages,
  IntfGraphics, FPImage, ugraphics, uradialtreemenu
  {$IFDEF LCLGTK3}, LazGdk3{$ENDIF}
  {$IFDEF LCLGTK2}, Gdk2{$ENDIF}
  ;

{$IFDEF LCLGTK2}
Procedure gdk_screen_set_resolution(Screen: PGdkScreen; DPI: Double); cdecl; external gdklib;
{$ENDIF}

Type
  TDestroyObserver = Class(TComponent)
  public
    Destroyed: Integer;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  End;

  TPopupMutation = (pmAdd, pmRelease, pmClear, pmCaption, pmFree, pmDetach,
    pmChildAdd, pmChildCaption, pmEnabled, pmVisible, pmNativeClear);

  TPopupMutationTest = Class
  private
    FMenu: TRadialTreeMenu;
    FRoot: TRadialTreeMenuItem;
    FChild: TMenuItem;
    FMutation: TPopupMutation;
    FExecuted: Boolean;
    FError: String;
    Procedure MutateOnIdle(Sender: TObject; Var Done: Boolean);
  public
    Procedure Run;
  End;

  TPopupCacheTest = Class
  private
    FMenu: TRadialTreeMenu;
    FClosed: Boolean;
    Procedure CloseOnIdle(Sender: TObject; Var Done: Boolean);
    Procedure Open;
    Procedure BuildAndReleaseTree;
  public
    Procedure Run;
    Procedure RunLifecycle;
  End;

  TPopupRenderBenchmark = Class
  private
    FMenu: TRadialTreeMenu;
    FExecuted: Boolean;
    Procedure MeasureOnIdle(Sender: TObject; Var Done: Boolean);
  public
    Procedure Run;
  End;

  TClickScenario = (ckLeaf, ckBranch, ckBranchHover, ckBranchMutates, ckRightClick,
    ckChangeAfterClick, ckFreeItemOnClose, ckFreeMenuOnClose, ckSingleRoot, ckCenter);

  TPopupClickTest = Class
  private
    FMenu: TRadialTreeMenu;
    FBranch: TRadialTreeMenuItem;
    FLeaf: TMenuItem;
    FScenario: TClickScenario;
    FExecuted: Boolean;
    FClicks, FBranchClicks, FCloses: Integer;
    FError: String;
    Procedure ClickOnIdle(Sender: TObject; Var Done: Boolean);
    Procedure MenuClosed(Sender: TObject);
    Procedure ItemClicked(Sender: TObject);
    Procedure BranchClicked(Sender: TObject);
  public
    Procedure Run;
  End;

  // Reports lines appended to the demo memo, for the X11 integration test.
  TMemoLogger = Class(TComponent)
  private
    FLogged: Integer;
    Procedure LogOnIdle(Sender: TObject; Var Done: Boolean);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  End;

  // Reports the owning control that opens the radial menu, in client
  // coordinates of the main form, and the form caption for the X11
  // integration test. Waits until the shown form has a realized layout.
  THostReporter = Class(TComponent)
  private
    Procedure ReportOnIdle(Sender: TObject; Var Done: Boolean);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  End;

Procedure Check(Condition: Boolean; Const Message: String);
Begin
  If Not Condition Then Raise Exception.Create(Message);
End;

Procedure ConfigureTestDPI;
Var
  I, DPI: Integer;
  Argument: String;
Begin
  For I := 1 To ParamCount Do Begin
    Argument := ParamStr(I);
    If Copy(Argument, 1, 10) <> '--test-dpi' Then Continue;
    Application.CaptureExceptions := False;
    If (Copy(Argument, 1, 11) <> '--test-dpi=') Or
      Not TryStrToInt(Copy(Argument, 12, Length(Argument)), DPI) Or (DPI <= 0) Then
      Raise EArgumentException.Create('Test DPI must be a positive integer');
    {$IF defined(LCLGTK2) or defined(LCLGTK3)}
    gdk_screen_set_resolution(gdk_screen_get_default, DPI);
    {$ELSE}
    Raise EInvalidOperation.Create('Isolated test DPI configuration requires GTK');
    {$ENDIF}
    ScreenInfo.PixelsPerInchX := DPI;
    ScreenInfo.PixelsPerInchY := DPI;
    Screen.UpdateScreen;
    Writeln('Test DPI: ', Screen.PixelsPerInch);
    Flush(Output);
  End;
End;

Function FindRadialMenuHost(Control: TControl): TControl;
Var
  I: Integer;
Begin
  If Control.PopupMenu Is TRadialTreeMenu Then Exit(Control);
  If Control Is TWinControl Then
    For I := 0 To TWinControl(Control).ControlCount - 1 Do Begin
      Result := FindRadialMenuHost(TWinControl(Control).Controls[I]);
      If Result <> nil Then Exit;
    End;
  Result := nil;
End;

// The demo assigns its radial menu to a control on the main form.
Function DemoMenu(Out Host: TControl): TRadialTreeMenu;
Begin
  Host := FindRadialMenuHost(Application.MainForm);
  Check(Host <> nil, 'Demo tests require a control with the radial popup menu');
  Result := TRadialTreeMenu(Host.PopupMenu);
  Check(Result.Items.Count = 3, 'Demo tests require the three-root demo tree');
End;

Procedure ConfigureCaptionTest;
Var
  I: Integer;
  Menu: TRadialTreeMenu;
  Host: TControl;

  Procedure ClearCaptions(Item: TMenuItem);
  Var
    J: Integer;
  Begin
    Item.Caption := '';
    For J := 0 To Item.Count - 1 Do ClearCaptions(Item[J]);
  End;

Begin
  For I := 1 To ParamCount Do
    If ParamStr(I) = '--test-captions' Then Begin
      Menu := DemoMenu(Host);
      ClearCaptions(Menu.Items);
      Menu.Items[0].Caption := StringOfChar('M', 32) + LineEnding + StringOfChar('W', 32);
      Application.MainForm.Position := poScreenCenter;
      Application.CaptureExceptions := False;
      Exit;
    End;
End;

Constructor TMemoLogger.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FLogged := TCustomMemo(AOwner).Lines.Count;
  Application.AddOnIdleHandler(@LogOnIdle, False);
End;

Destructor TMemoLogger.Destroy;
Begin
  Application.RemoveOnIdleHandler(@LogOnIdle);
  inherited Destroy;
End;

Procedure TMemoLogger.LogOnIdle(Sender: TObject; Var Done: Boolean);
Var
  Memo: TCustomMemo;
Begin
  Memo := TCustomMemo(Owner);
  If FLogged >= Memo.Lines.Count Then Exit;
  While FLogged < Memo.Lines.Count Do Begin
    Writeln('MEMO_LINE ', Memo.Lines[FLogged]);
    Inc(FLogged);
  End;
  Flush(Output);
End;

Constructor THostReporter.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  Application.AddOnIdleHandler(@ReportOnIdle, False);
End;

Destructor THostReporter.Destroy;
Begin
  Application.RemoveOnIdleHandler(@ReportOnIdle);
  inherited Destroy;
End;

Procedure THostReporter.ReportOnIdle(Sender: TObject; Var Done: Boolean);
Var
  Host: TControl;
  Form: TForm;
  Origin: TPoint;
Begin
  Host := TControl(Owner);
  Form := Application.MainForm;
  If Not Form.Visible Then Exit;
  Application.RemoveOnIdleHandler(@ReportOnIdle);
  If Host = Form Then
    Origin := Point(0, 0)
  Else
    Origin := Host.ClientToParent(Point(0, 0), Form);
  Writeln('RADIAL_HOST ', Origin.X, ' ', Origin.Y, ' ', Host.ClientWidth, ' ',
    Host.ClientHeight, ' ', Form.Caption);
  Flush(Output);
End;

Procedure ConfigureClickTest;
Var
  I, J: Integer;
  Form: TForm;
  Host: TControl;
  Menu: TRadialTreeMenu;
  HasMemo: Boolean;
Begin
  For I := 1 To ParamCount Do
    If ParamStr(I) = '--test-clicks' Then Begin
      Form := Application.MainForm;
      Menu := DemoMenu(Host);
      Menu.Items[1].Clear;
      HasMemo := False;
      For J := 0 To Form.ComponentCount - 1 Do
        If Form.Components[J] Is TCustomMemo Then Begin
          TMemoLogger.Create(Form.Components[J]);
          HasMemo := True;
        End;
      Check(HasMemo, 'Click tests require the demo memo');
      THostReporter.Create(Host);
      Application.CaptureExceptions := False;
      Exit;
    End;
End;

// Replaces the demo tree by the chain A -> B -> (C1, C2) for the X11
// integration test: B then forms a full ring around the root disc.
Procedure ConfigureFullRingTest;
Var
  I: Integer;
  Host: TControl;
  Menu: TRadialTreeMenu;
  Ring: TRadialTreeMenuItem;
Begin
  For I := 1 To ParamCount Do
    If ParamStr(I) = '--test-full-ring' Then Begin
      Menu := DemoMenu(Host);
      Menu.Clear;
      Ring := Menu.Add('A').Add('B');
      Ring.Add('C1');
      Ring.Add('C2');
      Application.CaptureExceptions := False;
      Exit;
    End;
End;

Procedure TDestroyObserver.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  If Operation = opRemove Then Inc(Destroyed);
  inherited Notification(AComponent, Operation);
End;

Procedure ExpectReleaseError(Menu: TRadialTreeMenu; Item: TMenuItem);
Begin
  Try
    Menu.Release(Item);
  Except
    On E: EMenuError Do Exit;
  End;
  Raise Exception.Create('Invalid Release did not raise EMenuError');
End;

Procedure TestAddAndRelease;
Var
  Menu: TRadialTreeMenu;
  Observer: TDestroyObserver;
  Root, OtherRoot, Child, Leaf: TRadialTreeMenuItem;
Begin
  Observer := TDestroyObserver.Create(nil);
  Menu := TRadialTreeMenu.Create(nil);
  Try
    Root := Menu.Add('Duplicate');
    OtherRoot := Menu.Add('Duplicate');
    Child := Root.Add('Child');
    Leaf := Child.Add('Leaf');
    Root.Tag := 17;
    OtherRoot.Tag := 18;
    Check((Root <> OtherRoot) And (Root.Caption = OtherRoot.Caption),
      'Duplicate captions must not merge elements');
    Check((Root.Owner = Menu) And (Child.Owner = Root) And (Leaf.Owner = Child),
      'Add did not establish hierarchical component ownership');
    Check((Menu.Items.Count = 2) And (Root.Count = 1) And
      (Child.GetParentMenu = Menu) And (Leaf.GetParentMenu = Menu),
      'Added elements were not attached to the native menu tree');
    Root.FreeNotification(Observer);
    Child.FreeNotification(Observer);
    Leaf.FreeNotification(Observer);
    Root.Release(Child);
    Check((Root.Count = 0) And (Observer.Destroyed = 2),
      'Item.Release did not destroy the complete child subtree');
    Menu.Release(Root);
    Check((Menu.Items.Count = 1) And (Menu.Items[0] = OtherRoot) And
      (OtherRoot.Tag = 18) And (Observer.Destroyed = 3),
      'Menu.Release damaged a sibling or did not destroy its root');
    Child := OtherRoot.Add('Child');
    Leaf := Child.Add('Leaf');
    Child.FreeNotification(Observer);
    Leaf.FreeNotification(Observer);
    Menu.Release(Leaf);
    Check((Child.Count = 0) And (Observer.Destroyed = 4),
      'Menu.Release must also support an attached descendant');
    OtherRoot.FreeNotification(Observer);
    Menu.Clear;
    Check((Menu.Items.Count = 0) And (Observer.Destroyed = 6),
      'Clear did not destroy the remaining attached tree');
    Menu.Clear;
    Menu.PopUp(100, 100);
    Check(ActivePopupMenu <> Menu, 'Empty menu opened a native popup');
    Menu.Add('Hidden').Visible := False;
    Menu.Items.AddSeparator;
    Menu.PopUp(100, 100);
    Check(ActivePopupMenu <> Menu, 'Hidden items/separators opened a popup');
    Menu.Clear;
  Finally
    Menu.Free;
    Observer.Free;
  End;
End;

Procedure TestDownsample;
Var
  Source, Dest: TBitmap;
  Color: TRGB;
Begin
  Source := TBitmap.Create;
  Dest := TBitmap.Create;
  Try
    Source.SetSize(8, 4);
    Source.Canvas.Brush.Color := clBlack;
    Source.Canvas.FillRect(Rect(0, 0, 4, 4));
    Source.Canvas.Brush.Color := clWhite;
    Source.Canvas.FillRect(Rect(0, 0, 4, 2));
    Source.Canvas.Brush.Color := RGBToColor(RGB(36, 68, 100));
    Source.Canvas.FillRect(Rect(4, 0, 8, 4));
    DownsampleOpaque(Dest, Source, 4);
    Check((Dest.Width = 2) And (Dest.Height = 1), 'Downsampling produced the wrong dimensions');
    Color := ugraphics.ColorToRGB(Dest.Canvas.Pixels[0, 0]);
    Check((Abs(Color.R - 128) <= 1) And (Abs(Color.G - 128) <= 1) And
      (Abs(Color.B - 128) <= 1), 'Downsampling did not average the full pixel block');
    Check(Dest.Canvas.Pixels[1, 0] = RGBToColor(RGB(36, 68, 100)),
      'Downsampling shifted or mixed adjacent pixel blocks');
    DownsampleOpaque(Source, Source, 4);
    Check(Source.Canvas.Pixels[0, 0] = Dest.Canvas.Pixels[0, 0],
      'In-place downsampling lost the source pixels');
    Try
      DownsampleOpaque(Dest, Source, 0);
      Raise Exception.Create('Downsampling accepted a zero factor');
    Except
      On E: EArgumentException Do ;
    End;
    Try
      DownsampleOpaque(Dest, Source, 3);
      Raise Exception.Create('Downsampling accepted indivisible dimensions');
    Except
      On E: EArgumentException Do ;
    End;
  Finally
    Dest.Free;
    Source.Free;
  End;
End;

Procedure TestAlphaLayers;
Var
  Source, Mask, Tile, Dest: TBitmap;
  Image: TLazIntfImage;
  Color: TFPColor;
  Layers: Array[0..0] Of TBitmapLayer;
  RGBColor: TRGB;
Begin
  Source := TBitmap.Create;
  Mask := TBitmap.Create;
  Tile := TBitmap.Create;
  Dest := TBitmap.Create;
  Try
    Source.SetSize(8, 4);
    Mask.SetSize(8, 4);
    Source.Canvas.Brush.Color := clBlue;
    Source.Canvas.FillRect(Rect(0, 0, 8, 4));
    Source.Canvas.Brush.Color := clRed;
    Source.Canvas.FillRect(Rect(0, 0, 2, 4));
    Mask.Canvas.Brush.Color := clBlack;
    Mask.Canvas.FillRect(Rect(0, 0, 8, 4));
    Mask.Canvas.Brush.Color := clWhite;
    Mask.Canvas.FillRect(Rect(0, 0, 2, 4));
    DownsampleMasked(Tile, Source, Mask, 4);
    Image := Tile.CreateIntfImage;
    Try
      Color := Image.Colors[0, 0];
      Check((Color.Red = High(Word)) And (Color.Blue = 0) And
        (Abs(Integer(Color.Alpha) - 32768) <= 256),
        'Masked averaging mixed uncovered blue samples into the red edge');
      Check(Image.Colors[1, 0].Alpha = 0, 'Uncovered output must be transparent');
    Finally
      Image.Free;
    End;
    Dest.SetSize(3, 2);
    Layers[0].Bitmap := Tile;
    Layers[0].Position := Point(1, 0);
    ComposeBitmapLayers(Dest, clLime, Layers);
    RGBColor := ugraphics.ColorToRGB(Dest.Canvas.Pixels[1, 0]);
    Check((Abs(RGBColor.R - 128) <= 2) And (Abs(RGBColor.G - 127) <= 2) And
      (RGBColor.B = 0), 'Layer alpha was not blended over the background');
    Check(Dest.Canvas.Pixels[2, 0] = clLime, 'Transparent layer overwrote its background');
    Layers[0].Position := Point(-1, 1);
    ComposeBitmapLayers(Dest, clLime, Layers);
    Check(Dest.Canvas.Pixels[0, 1] = clLime, 'Clipped layer sampled the wrong source pixel');
    Layers[0].Bitmap := Dest;
    Try
      ComposeBitmapLayers(Dest, clLime, Layers);
      Raise Exception.Create('Composition accepted an aliased destination');
    Except
      On E: EArgumentException Do ;
    End;
    Try
      DownsampleMasked(Tile, Source, Mask, 0);
      Raise Exception.Create('Masked downsampling accepted a zero factor');
    Except
      On E: EArgumentException Do ;
    End;
    DownsampleMasked(Source, Source, Mask, 4);
    Check((Source.Width = 2) And (Source.Height = 1),
      'In-place masked downsampling lost source dimensions');
    Try
      DownsampleMasked(Tile, Source, Mask, 1);
      Raise Exception.Create('Masked downsampling accepted mismatched dimensions');
    Except
      On E: EArgumentException Do ;
    End;
  Finally
    Dest.Free;
    Tile.Free;
    Mask.Free;
    Source.Free;
  End;
End;

Procedure TestNativeCompatibility;
Var
  Menu: TRadialTreeMenu;
  Observer: TDestroyObserver;
  Root: TRadialTreeMenuItem;
  Native, Second, Third, NativeRoot, NativeChild: TMenuItem;
Begin
  Menu := TRadialTreeMenu.Create(nil);
  Observer := TDestroyObserver.Create(nil);
  Try
    Root := Menu.Add('Root');
    Native := TMenuItem.Create(Root);
    Second := TMenuItem.Create(Root);
    Third := TMenuItem.Create(Root);
    Native.FreeNotification(Observer);
    Root.Add(Native);
    Root.Add([Second, Third]);
    Check(Root.Count = 3, 'Native Add overloads are not available');
    Root.Delete(0);
    Check((Root.Count = 2) And (Native.Parent = nil) And
      (Native.Owner = Root) And (Observer.Destroyed = 0),
      'Native Delete must detach without destroying or changing ownership');
    Root.Add(Native);
    Root.Remove(Native);
    Check((Native.Parent = nil) And (Observer.Destroyed = 0),
      'Native Remove must not destroy the element');
    Root.Add(Native);
    Root.Release(Native);
    Check((Root.Count = 2) And (Observer.Destroyed = 1),
      'Release did not support an ordinary TMenuItem');
    NativeRoot := TMenuItem.Create(Menu);
    NativeChild := TMenuItem.Create(NativeRoot);
    NativeChild.FreeNotification(Observer);
    NativeRoot.Add(NativeChild);
    Menu.Items.Add(NativeRoot);
    Menu.Release(NativeRoot);
    Check((Menu.Items.Count = 1) And (Observer.Destroyed = 2),
      'Native roots and their descendants were not released');
    Second.FreeNotification(Observer);
    Third.FreeNotification(Observer);
    Root.Clear;
    Check((Root.Count = 0) And (Observer.Destroyed = 4),
      'Inherited Item.Clear did not destroy ordinary menu children');
    Native := Root.Add('Detached child');
    Native.FreeNotification(Observer);
    Root.Remove(Native);
    Menu.Release(Root);
    Check((Menu.Items.Count = 0) And (Observer.Destroyed = 5),
      'Releasing an owner did not destroy its detached, parent-owned child');
  Finally
    Menu.Free;
    Observer.Free;
  End;
End;

Procedure TestInvalidRelease;
Var
  Menu, OtherMenu: TRadialTreeMenu;
  Root, OtherRoot, Child: TRadialTreeMenuItem;
Begin
  Menu := TRadialTreeMenu.Create(nil);
  OtherMenu := TRadialTreeMenu.Create(nil);
  Try
    Root := Menu.Add('Root');
    OtherRoot := OtherMenu.Add('Other');
    Child := Root.Add('Child');
    ExpectReleaseError(Menu, nil);
    ExpectReleaseError(Menu, Menu.Items);
    ExpectReleaseError(Menu, OtherRoot);
    Root.Remove(Child);
    ExpectReleaseError(Menu, Child);
    Root.Add(Child);
    Try
      Root.Release(OtherRoot);
      Raise Exception.Create('Item.Release accepted a foreign child');
    Except
      On E: EMenuError Do ;
    End;
    Check((Menu.Items.Count = 1) And (Root.Count = 1) And
      (OtherMenu.Items.Count = 1), 'Invalid Release modified a menu');
  Finally
    OtherMenu.Free;
    Menu.Free;
  End;
End;

Procedure TPopupMutationTest.MutateOnIdle(Sender: TObject; Var Done: Boolean);
Begin
  If ActivePopupMenu <> FMenu Then Exit;
  Application.RemoveOnIdleHandler(@MutateOnIdle);
  FExecuted := True;
  Done := False;
  Try
    Case FMutation Of
      pmAdd: FMenu.Add('Added while open');
      pmRelease: FMenu.Release(FChild);
      pmClear: FMenu.Clear;
      pmCaption: FRoot.Caption := 'Changed while open';
      pmFree: FChild.Free;
      pmDetach: FRoot.Remove(FChild);
      pmChildAdd: FChild.Add(TMenuItem.Create(FChild));
      pmChildCaption: FChild.Caption := 'Changed native child';
      pmEnabled: FChild.Enabled := False;
      pmVisible: FChild.Visible := False;
      pmNativeClear: FRoot.Clear;
    End;
  Except
    On E: Exception Do Begin
      FError := E.ClassName + ': ' + E.Message;
      FMenu.Close;
    End;
  End;
End;

// Popups close when their application is deactivated, so a late activation of
// the host must not arrive while a test popup is open. Wait for a stable active
// host. Window managers may refuse programmatic activation without user input
// (seen with Qt5 under Marco); then no late activation follows either.
Procedure ActivateTestHost(Host: TForm);
Var
  Deadline, StableSince, Tick: QWord;
Begin
  Host.Show;
  SetForegroundWindow(Host.Handle);
  Deadline := GetTickCount64 + 2000;
  StableSince := 0;
  Repeat
    Application.ProcessMessages;
    Tick := GetTickCount64;
    If Not Host.Active Then StableSince := 0
    Else If StableSince = 0 Then StableSince := Tick;
    Sleep(1);
  Until ((StableSince <> 0) And (Tick - StableSince >= 50)) Or (Tick >= Deadline);
End;

Procedure TPopupCacheTest.CloseOnIdle(Sender: TObject; Var Done: Boolean);
Begin
  If ActivePopupMenu <> FMenu Then Exit;
  Application.RemoveOnIdleHandler(@CloseOnIdle);
  FClosed := True;
  FMenu.Close;
  Done := False;
End;

Procedure TPopupCacheTest.Open;
Begin
  ActivateTestHost(Application.MainForm);
  FClosed := False;
  Application.AddOnIdleHandler(@CloseOnIdle);
  Try
    FMenu.PopUp(260, 220);
    Check(FClosed, 'Cache popup closed before its idle check');
  Finally
    Application.RemoveOnIdleHandler(@CloseOnIdle);
  End;
End;

Procedure TPopupCacheTest.Run;
Var
  Root: TRadialTreeMenuItem;
  Native: TMenuItem;
  Count: QWord;
Begin
  FMenu := TRadialTreeMenu.Create(nil);
  Try
    FMenu.Parent := Application.MainForm;
    Root := FMenu.Add('Root');
    FMenu.Add('Other root');
    Native := TMenuItem.Create(Root);
    Native.Caption := 'Native child';
    Root.Add(Native);
    Open;
    Count := FMenu.CachedSectorRenderCount;
    Open;
    Check(FMenu.CachedSectorRenderCount = Count, 'Unchanged popup rebuilt cached sectors');
    Native.Caption := 'Changed native child';
    Open;
    Inc(Count);
    Check(FMenu.CachedSectorRenderCount = Count, 'Caption change rebuilt unrelated sectors or was missed');
    Root.Add('Sibling');
    Open;
    Inc(Count, 2);
    Check(FMenu.CachedSectorRenderCount = Count, 'Sibling insertion did not invalidate the affected angles only');
    Native.Add(TMenuItem.Create(Native));
    Native[0].Caption := 'Leaf';
    Open;
    Inc(Count);
    Check(FMenu.CachedSectorRenderCount = Count, 'Depth change unnecessarily invalidated translated sectors');
    Native.Enabled := False;
    Open;
    Inc(Count);
    Check(FMenu.CachedSectorRenderCount = Count, 'Enabled change did not update only the affected sector');
    Native.Visible := False;
    Open;
    Inc(Count);
    Check(FMenu.CachedSectorRenderCount = Count, 'Hiding a child did not update its visible sibling angle only');
    Native.Visible := True;
    Open;
    Inc(Count, 2);
    Check(FMenu.CachedSectorRenderCount = Count, 'Showing a child did not restore both child sectors');
    Native.Free;
    Open;
    Inc(Count);
    Check(FMenu.CachedSectorRenderCount = Count, 'Destroyed native child left stale cache geometry');
    FMenu.Clear;
    FMenu.PopUp(260, 220);
    Check(FMenu.CachedSectorRenderCount = Count, 'Clearing the cache prepared an empty menu');
  Finally
    FMenu.Free;
  End;
End;

Procedure TPopupCacheTest.BuildAndReleaseTree;
Var
  Observer: TDestroyObserver;
  Renders: QWord;

  // Eight items in three levels, including one ordinary TMenuItem.
  Procedure BuildTree;
  Var
    Root, Child: TRadialTreeMenuItem;
    Native: TMenuItem;
  Begin
    Root := FMenu.Add('Root 1');
    Child := Root.Add('Child 1.1');
    Child.Add('Leaf 1.1.1');
    Native := TMenuItem.Create(Child);
    Native.Caption := 'Native leaf 1.1.2';
    Child.Add(Native);
    Root.Add('Child 1.2');
    FMenu.Add('Root 2').Add('Child 2.1');
    FMenu.Add('Root 3');
  End;

  Procedure Watch(AParent: TMenuItem);
  Var
    I: Integer;
  Begin
    For I := 0 To AParent.Count - 1 Do Begin
      AParent[I].FreeNotification(Observer);
      Watch(AParent[I]);
    End;
  End;

  Procedure CheckState(Destroyed, Sectors: Integer; Const Step: String);
  Begin
    Check(Observer.Destroyed = Destroyed, Step + ': ' + IntToStr(Observer.Destroyed) +
      ' items destroyed instead of ' + IntToStr(Destroyed));
    Check(FMenu.CachedSectorCount = Sectors, Step + ': ' +
      IntToStr(FMenu.CachedSectorCount) + ' sector images held instead of ' +
      IntToStr(Sectors));
  End;

Begin
  Observer := TDestroyObserver.Create(nil);
  Try
    FMenu := TRadialTreeMenu.Create(nil);
    Try
      FMenu.Parent := Application.MainForm;
      BuildTree;
      Watch(FMenu.Items);
      Open;
      CheckState(0, 8, 'First tree');
      FMenu.Clear;
      Check(FMenu.Items.Count = 0, 'Clear left attached items');
      FMenu.PopUp(260, 220);
      Check(ActivePopupMenu <> FMenu, 'Cleared menu opened a popup');
      CheckState(8, 0, 'Cleared tree');
      Renders := FMenu.CachedSectorRenderCount;
      BuildTree;
      Watch(FMenu.Items);
      Open;
      CheckState(8, 8, 'Rebuilt tree');
      Check(FMenu.CachedSectorRenderCount = Renders + 8,
        'Rebuilt tree was not rendered completely');
      // LCL style: freeing an item releases it and its whole subtree.
      FMenu.Items[0][0].Free;
      Check(FMenu.Items[0].Count = 1, 'Freed branch is still attached');
      Open;
      CheckState(11, 5, 'Freed branch with two leaves');
      FMenu.Items[1].Free;
      Check(FMenu.Items.Count = 2, 'Freed root is still attached');
      Open;
      CheckState(13, 3, 'Freed root with one child');
      (FMenu.Items[1] As TRadialTreeMenuItem).Add('Child 3.1').Add('Leaf 3.1.1');
      Watch(FMenu.Items);
      Open;
      CheckState(13, 5, 'Extended remaining root');
    Finally
      FreeAndNil(FMenu);
    End;
    Check(Observer.Destroyed = 18, 'Destroying the menu left items alive');
  Finally
    Observer.Free;
  End;
End;

Procedure TPopupCacheTest.RunLifecycle;
Const
  // Enough passes to fill bounded caches, e.g. the 100 unused fonts of the
  // LCL GTK2 font cache.
  MaxPasses = 20;
Var
  Before, After: PtrUInt;
  Pass: Integer;
Begin
  // The first pass also creates lazily allocated LCL/widgetset resources.
  BuildAndReleaseTree;
  // Repaints of the visible demo form may fill bounded widgetset caches
  // during a pass. Such caches stop growing when full; a leak grows the heap
  // in every pass.
  For Pass := 1 To MaxPasses Do Begin
    Before := GetFPCHeapStatus.CurrHeapUsed;
    BuildAndReleaseTree;
    // Read before building the message; its temporary string is heap memory too.
    After := GetFPCHeapStatus.CurrHeapUsed;
    If After = Before Then Exit;
  End;
  Check(False, 'Tree lifecycle changed the used heap in each of ' +
    IntToStr(MaxPasses) + ' passes, last by ' +
    IntToStr(Int64(After) - Int64(Before)) + ' bytes');
End;

Function PopupHost(Menu: TRadialTreeMenu): TCustomForm;
Var
  I: Integer;
Begin
  Result := nil;
  For I := 0 To Screen.CustomFormCount - 1 Do
    If Screen.CustomForms[I].Owner = Menu Then Result := Screen.CustomForms[I];
  Check(Result <> nil, 'Popup test has no popup host');
End;

Procedure SendPopupMouse(Window: TCustomForm; Message: Cardinal; Radius: Integer;
  Degrees: Double);
Var
  X, Y: Integer;
  Scale: Double;
Begin
  Scale := Screen.PixelsPerInch / 96;
  X := Window.Width Div 2 + Round(Radius * Scale * Cos(Degrees * Pi / 180));
  Y := Window.Height Div 2 + Round(Radius * Scale * Sin(Degrees * Pi / 180));
  Window.Perform(Message, 0, X Or (Y Shl 16));
End;

Procedure TPopupClickTest.ClickOnIdle(Sender: TObject; Var Done: Boolean);
Var
  Window: TCustomForm;
Begin
  If ActivePopupMenu <> FMenu Then Exit;
  Application.RemoveOnIdleHandler(@ClickOnIdle);
  FExecuted := True;
  Done := False;
  Try
    Window := PopupHost(FMenu);
    Case FScenario Of
      ckSingleRoot: Begin
          // The only root opened with the popup: its leaf ring is reachable
          // without pointer motion.
          Check(FBranchClicks = 1, 'The single root did not open with the popup');
          SendPopupMouse(Window, LM_LBUTTONDOWN, 95, 0);
          Check(Window.ModalResult <> mrNone, 'The leaf of the single root is not reachable');
          Exit;
        End;
      ckCenter: Begin
          // Both roots meet at the center, which selects neither of them.
          SendPopupMouse(Window, LM_MOUSEMOVE, 0, 0);
          SendPopupMouse(Window, LM_LBUTTONDOWN, 0, 0);
          Check((FBranchClicks = 0) And (Window.ModalResult = mrNone),
            'The center selected a root or closed the popup');
        End;
    End;
    SendPopupMouse(Window, LM_MOUSEMOVE, 40, 0);
    Case FScenario Of
      ckBranch: Begin
          SendPopupMouse(Window, LM_LBUTTONDOWN, 40, 0);
          Check(Window.ModalResult = mrNone, 'Branch click closed the popup');
          SendPopupMouse(Window, LM_RBUTTONDOWN, 40, 0);
          Exit;
        End;
      ckBranchHover: Begin
          // Within the branch, to its leaf and back inward: it stays open.
          SendPopupMouse(Window, LM_MOUSEMOVE, 45, 10);
          SendPopupMouse(Window, LM_MOUSEMOVE, 95, 0);
          SendPopupMouse(Window, LM_MOUSEMOVE, 40, 0);
          Check(FBranchClicks = 1, 'Moving within an open branch clicked it again');
          // Over the root-level leaf and back: the branch opens a second time.
          SendPopupMouse(Window, LM_MOUSEMOVE, 40, 180);
          SendPopupMouse(Window, LM_MOUSEMOVE, 40, 0);
          SendPopupMouse(Window, LM_RBUTTONDOWN, 40, 0);
          Exit;
        End;
      // The handler changed the tree; the menu's idle check must close the popup.
      ckBranchMutates: Exit;
    End;
    SendPopupMouse(Window, LM_MOUSEMOVE, 95, 0);
    If FScenario = ckRightClick Then
      SendPopupMouse(Window, LM_RBUTTONDOWN, 95, 0)
    Else
      SendPopupMouse(Window, LM_LBUTTONDOWN, 95, 0);
    Check(Window.ModalResult <> mrNone, 'Leaf click did not close the popup');
    If FScenario = ckChangeAfterClick Then FLeaf.Caption := 'Changed after click';
  Except
    On E: Exception Do Begin
      FError := E.ClassName + ': ' + E.Message;
      FMenu.Close;
    End;
  End;
End;

Procedure TPopupClickTest.MenuClosed(Sender: TObject);
Begin
  Inc(FCloses);
  Case FScenario Of
    ckFreeItemOnClose: FreeAndNil(FLeaf);
    ckFreeMenuOnClose: FreeAndNil(FMenu);
  End;
End;

Procedure TPopupClickTest.ItemClicked(Sender: TObject);
Begin
  Check(Sender = FLeaf, 'OnClick fired for a branch or a wrong item');
  Check(FCloses = 1, 'OnClick fired before OnClose');
  Check(ActivePopupMenu <> FMenu, 'OnClick fired while the popup was active');
  Inc(FClicks);
End;

Procedure TPopupClickTest.BranchClicked(Sender: TObject);
Begin
  Check(Sender = FBranch, 'Branch OnClick has the wrong sender');
  // GTK2 sets ActivePopupMenu only after its input grabs, i.e. after OnShow.
  Check((FCloses = 0) And PopupHost(FMenu).Visible,
    'Branch OnClick fired outside the open popup');
  Inc(FBranchClicks);
  If FScenario = ckBranchMutates Then FBranch.Add('Added in OnClick');
End;

Procedure TPopupClickTest.Run;
Var
  Scenario: TClickScenario;
Begin
  For Scenario := Low(TClickScenario) To High(TClickScenario) Do Begin
    ActivateTestHost(Application.MainForm);
    FMenu := TRadialTreeMenu.Create(nil);
    Try
      FMenu.Parent := Application.MainForm;
      FBranch := FMenu.Add('Branch');
      FLeaf := FBranch.Add('Leaf');
      If Scenario <> ckSingleRoot Then FMenu.Add('Root leaf');
      FBranch.OnClick := @BranchClicked;
      FLeaf.OnClick := @ItemClicked;
      FMenu.OnClose := @MenuClosed;
      FScenario := Scenario;
      FExecuted := False;
      FClicks := 0;
      FBranchClicks := 0;
      FCloses := 0;
      FError := '';
      Application.AddOnIdleHandler(@ClickOnIdle);
      Try
        FMenu.PopUp(260, 220);
      Finally
        Application.RemoveOnIdleHandler(@ClickOnIdle);
      End;
      Check(FExecuted, 'Popup closed before click scenario ' + IntToStr(Ord(Scenario)));
      Check(FError = '', FError);
      Check(FCloses = 1, 'OnClose did not fire exactly once in click scenario ' +
        IntToStr(Ord(Scenario)));
      Check(FClicks = Ord(Scenario In [ckLeaf, ckSingleRoot, ckCenter]),
        'Wrong OnClick count in click scenario ' + IntToStr(Ord(Scenario)));
      Check(FBranchClicks = 1 + Ord(Scenario = ckBranchHover),
        'Wrong branch OnClick count in click scenario ' + IntToStr(Ord(Scenario)));
      If Scenario = ckBranchMutates Then
        Check(FBranch.Count = 2, 'Tree change in a branch OnClick was lost');
      If Scenario = ckFreeItemOnClose Then
        Check(FBranch.Count = 0, 'Item destroyed in OnClose is still attached');
    Finally
      FreeAndNil(FMenu);
    End;
  End;
End;

Procedure TPopupRenderBenchmark.MeasureOnIdle(Sender: TObject; Var Done: Boolean);
Const
  BaselineIterations = 3;
  HoverIterations = 50;
Var
  Window: TCustomForm;
  Source, Dest: TBitmap;
  I: Integer;
  Started, BaselineTime, HoverTime, RenderCount: QWord;
  Scale: Double;

  Procedure MoveTo(Radius: Integer; Degrees: Double);
  Var
    X, Y: Integer;
  Begin
    X := Window.Width Div 2 + Round(Radius * Scale * Cos(Degrees * Pi / 180));
    Y := Window.Height Div 2 + Round(Radius * Scale * Sin(Degrees * Pi / 180));
    Window.Perform(LM_MOUSEMOVE, 0, X Or (Y Shl 16));
  End;

Begin
  If ActivePopupMenu <> FMenu Then Exit;
  Application.RemoveOnIdleHandler(@MeasureOnIdle);
  Window := PopupHost(FMenu);
  Scale := Screen.PixelsPerInch / 96;
  Source := TBitmap.Create;
  Try
    Dest := TBitmap.Create;
    Try
      Source.PixelFormat := pf24Bit;
      Source.SetSize(Window.Width * 4, Window.Height * 4);
      Source.Canvas.Brush.Color := clMenu;
      Source.Canvas.FillRect(Rect(0, 0, Source.Width, Source.Height));
      Started := GetTickCount64;
      For I := 1 To BaselineIterations Do DownsampleOpaque(Dest, Source, 4);
      BaselineTime := GetTickCount64 - Started;
      MoveTo(45, -30);
      MoveTo(95, -60);
      RenderCount := FMenu.CachedSectorRenderCount;
      Started := GetTickCount64;
      For I := 0 To HoverIterations - 1 Do
        If Odd(I) Then MoveTo(145, -45) Else MoveTo(145, -75);
      HoverTime := GetTickCount64 - Started;
      Check(FMenu.CachedSectorRenderCount = RenderCount, 'Hover prepared new sector images');
      Check(HoverTime * BaselineIterations * 2 < BaselineTime * HoverIterations,
        'Cached hover did not beat full-frame downsampling by at least two times');
      Writeln('RENDER_METRICS dpi=', Screen.PixelsPerInch,
        ' baseline_ms=', (BaselineTime / BaselineIterations):0:2,
        ' cached_hover_ms=', (HoverTime / HoverIterations):0:2,
        ' sectors_prepared_during_hover=', FMenu.CachedSectorRenderCount - RenderCount);
      Flush(Output);
      FExecuted := True;
      FMenu.Close;
      Done := False;
    Finally
      Dest.Free;
    End;
  Finally
    Source.Free;
  End;
End;

Procedure TPopupRenderBenchmark.Run;
Var
  Root, Child: TRadialTreeMenuItem;
Begin
  FMenu := TRadialTreeMenu.Create(nil);
  Try
    FMenu.Parent := Application.MainForm;
    Root := FMenu.Add('Root');
    Child := Root.Add('Child');
    Child.Add('Leaf one');
    Child.Add('Leaf two');
    Root.Add('Other child');
    FMenu.Add('Other root');
    FMenu.Add('Third root');
    ActivateTestHost(Application.MainForm);
    Application.AddOnIdleHandler(@MeasureOnIdle);
    Try
      FMenu.PopUp(260, 220);
      Check(FExecuted, 'Popup closed before render benchmark');
    Finally
      Application.RemoveOnIdleHandler(@MeasureOnIdle);
    End;
  Finally
    FMenu.Free;
  End;
End;

Procedure TPopupMutationTest.Run;
Var
  Host: TForm;
  Mutation: TPopupMutation;
Begin
  Host := Application.MainForm;
  Check(Host <> nil, 'Popup tests require the existing test host as main form');
  Begin
    Host.Show;
    For Mutation := Low(TPopupMutation) To High(TPopupMutation) Do Begin
      ActivateTestHost(Host);
      FMenu := TRadialTreeMenu.Create(Host);
      Try
        FMenu.Parent := Host;
        FRoot := FMenu.Add('Root');
        FChild := TMenuItem.Create(FRoot);
        FChild.Caption := 'Native child';
        FRoot.Add(FChild);
        FMutation := Mutation;
        FExecuted := False;
        FError := '';
        Application.AddOnIdleHandler(@MutateOnIdle);
        Writeln('Running popup mutation ', Ord(Mutation));
        Flush(Output);
        FMenu.PopUp(260, 220);
        Check(FExecuted, 'Popup closed before mutation test executed');
        Check(FError = '', FError);
        Check(ActivePopupMenu <> FMenu, 'Mutated popup remained active');
        Case Mutation Of
          pmAdd: Check(FMenu.Items.Count = 2, 'Addition was lost after popup closure');
          pmRelease, pmFree, pmDetach, pmNativeClear:
            Check(FRoot.Count = 0, 'Child mutation did not update the tree');
          pmClear: Check(FMenu.Items.Count = 0, 'Clear left attached roots');
          pmCaption:
            Check(FRoot.Caption = 'Changed while open', 'Caption change was lost');
          pmChildAdd: Check(FChild.Count = 1, 'Native child addition was lost');
          pmChildCaption:
            Check(FChild.Caption = 'Changed native child', 'Native caption change was lost');
          pmEnabled: Check(Not FChild.Enabled, 'Enabled change was lost');
          pmVisible: Check(Not FChild.Visible, 'Visible change was lost');
        End;
      Finally
        Application.RemoveOnIdleHandler(@MutateOnIdle);
        FreeAndNil(FMenu);
      End;
    End;
  End;
End;

Function RunRadialMenuItemTests: Integer;
Var
  PopupTest: TPopupMutationTest;
  CacheTest: TPopupCacheTest;
  Benchmark: TPopupRenderBenchmark;
  ClickTest: TPopupClickTest;
Begin
  Result := 1;
  Try
    TestDownsample;
    Writeln('PASS: downsampling averages aligned blocks and rejects invalid factors');
    Flush(Output);
    TestAlphaLayers;
    Writeln('PASS: masked averaging and clipped alpha composition avoid color fringes');
    Flush(Output);
    TestAddAndRelease;
    Writeln('PASS: Add, subtree Release, Clear, duplicate captions and empty popup');
    Flush(Output);
    TestNativeCompatibility;
    Writeln('PASS: native TMenuItem Add/Remove/Delete/Clear compatibility');
    Flush(Output);
    TestInvalidRelease;
    Writeln('PASS: invalid Release reports errors without modifying trees');
    Flush(Output);
    CacheTest := TPopupCacheTest.Create;
    Try
      CacheTest.Run;
    Finally
      CacheTest.Free;
    End;
    Writeln('PASS: popup cache reuse and selective native-item invalidation');
    Flush(Output);
    CacheTest := TPopupCacheTest.Create;
    Try
      CacheTest.RunLifecycle;
    Finally
      CacheTest.Free;
    End;
    Writeln('PASS: build, clear, rebuild and partial Free release items, images and heap');
    Flush(Output);
    Benchmark := TPopupRenderBenchmark.Create;
    Try
      Benchmark.Run;
    Finally
      Benchmark.Free;
    End;
    ClickTest := TPopupClickTest.Create;
    Try
      ClickTest.Run;
    Finally
      ClickTest.Free;
    End;
    Writeln('PASS: leaf clicks fire OnClick once after OnClose; opened branches once per ' +
      'opening; a single root opens with the popup; the center of several roots is neutral');
    Flush(Output);
    PopupTest := TPopupMutationTest.Create;
    Try
      PopupTest.Run;
    Finally
      PopupTest.Free;
    End;
    Writeln('PASS: mutations and native item destruction close open popups');
    Result := 0;
  Except
    On E: Exception Do
      Writeln(StdErr, 'FAIL: ', E.ClassName, ': ', E.Message);
  End;
End;

End.
