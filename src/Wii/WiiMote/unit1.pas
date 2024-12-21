(******************************************************************************)
(* Wiimote demo                                                    ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uxwiimote, ugraphics;

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
    CheckBox4: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure CheckBox3Change(Sender: TObject);
    Procedure CheckBox4Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    XWiiMonitor: TXWiiMonitor;
    XWiiMote: TXWiiMote;
    MotorRumble: Boolean;
    calibCounter, accelOffx, accelOffy, accelOffz: integer;
    InCalibration: Boolean;
    Procedure OnWiiMoteKeyEvent(Sender: TObject; Key: Integer; State: integer);
    Procedure OnWiiMoteIREvent(Sender: Tobject; Valid: byte; pt1, pt2, pt3, pt4: TPoint);
    Procedure OnWiiMoteAccelEvent(Sender: TObject; x, y, z: Int32);

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ulibxwiimote; // To get access to the const values

Procedure Nop();
Begin

End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Wiimote demo ver. 0.01';
  XWiiMonitor := TXWiiMonitor.Create();
  XWiiMote := Nil;
  accelOffx := 0;
  accelOffy := 0;
  accelOffz := 0;
  label1.caption := ''; // Name
  label2.caption := ''; // Battery
  Label3.Caption := '1';
  Label4.Caption := '2';
  Label5.Caption := '3';
  Label6.Caption := '4';
  Label7.Caption := ''; // Available ifaces
  Label8.Caption := ''; // Opened ifaces
  Label9.Caption := ''; // Button Preview
  Label10.Caption := ''; // Button State
  Label11.Caption := ''; // Button State

  label3.Top := -30;
  label4.Top := -30;
  label5.Top := -30;
  label6.Top := -30;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  i: integer;
Begin
  // Refresh Battery state :-)
  If assigned(XWiiMote) Then Begin
    i := XWiiMote.Battery();
    If i <> -1 Then Begin
      label2.caption := 'Battery is ' + inttostr(i) + '%';
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(XWiiMote) Then XWiiMote.free;
  XWiiMonitor.Free;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Create a instance for the first WiiMote in List
  sl := XWiiMonitor.GetDeviceList();

  If sl.Count = 0 Then Begin
    showmessage('Error no device found.');
    sl.free;
    exit;
  End;

  // 3. Wenn Devices gefunden wurden versuchen wir diese zu erstellen
  XWiiMote := XWiiMonitor.CreateXWiiMote(sl[0]);
  label1.caption := sl[0] + LineEnding +
    'Devtype: ' + XWiiMote.get_devtype() + LineEnding +
    'Extension: ' + XWiiMote.get_extension();
  sl.free;
  If Not assigned(XWiiMote) Then Begin
    showmessage('Error, unable to create device.');
    exit;
  End;
  InCalibration := false;
  accelOffx := 0;
  accelOffy := 0;
  accelOffz := 0;

  (*
   * Assign Callbacks ;)
   *)
  XWiiMote.OnKeyEvent := @OnWiiMoteKeyEvent;
  XWiiMote.OnIREvent := @OnWiiMoteIREvent;
  XWiiMote.OnAccelEvent := @OnWiiMoteAccelEvent;

  // 4. Das Interface wird geöffnet mit Allem was geht ;)
  i := XWiiMote.Available();
  label7.caption := 'Available features: ' + format('%0.6X', [i]);
  i := i Or XWII_IFACE_WRITABLE;
  If Not XWiiMote.Open_interfaces(i) Then Begin
    // Das Interface konnte nicht mit allen Dingen geöffnet werden, wir versuchen es mit den Core Sachen
    i := XWII_IFACE_CORE
      Or XWII_IFACE_ACCEL
      Or XWII_IFACE_WRITABLE;
    If XWiiMote.Open_interfaces(i) Then Begin
      Showmessage('! Attention was unable to access to all features, please restart as root.');
    End
    Else Begin
      XWiiMote.free;
      XWiiMote := Nil;
      showmessage('Error, unable to open interfaces.');
      exit;
    End;
  End;

  label8.caption := 'Opened features: ' + format('%0.6X', [XWiiMote.Opened_interfaces()]);

  (*
   * Read the LED-State into the Checkboxes -> This is actually broken
   *)
  MotorRumble := false;

  button1.Enabled := false;
  button2.Enabled := true;
  button3.Enabled := true;
  button4.Enabled := true;
  CheckBox1.Enabled := true;
  CheckBox2.Enabled := true;
  CheckBox3.Enabled := true;
  CheckBox4.Enabled := true;
  label3.Top := -30;
  label4.Top := -30;
  label5.Top := -30;
  label6.Top := -30;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Free
  XWiiMote.free;
  XWiiMote := Nil;

  label1.caption := ''; // Name
  label2.caption := ''; // Battery
  Label7.Caption := ''; // Available ifaces
  Label8.Caption := ''; // Opened ifaces
  Label9.Caption := ''; // Button Preview
  Label10.Caption := ''; // Button State

  button1.Enabled := true;
  button2.Enabled := false;
  button3.Enabled := false;
  button4.Enabled := false;

  CheckBox1.Enabled := false;
  CheckBox2.Enabled := false;
  CheckBox3.Enabled := false;
  CheckBox4.Enabled := false;

  label3.Top := -30;
  label4.Top := -30;
  label5.Top := -30;
  label6.Top := -30;
  ;

End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  MotorRumble := Not MotorRumble;
  If Not XWiiMote.Rumble(MotorRumble) Then Begin
    ShowMessage('Unable to switch motor, did you open with: XWII_IFACE_CORE Or XWII_IFACE_WRITABLE ?');
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  t: QWord;
Begin
  showmessage('Do not move WiiMote during Calibration (take 5 to 10s)!.');
  t := GetTickCount64();
  InCalibration := true;
  accelOffx := 0;
  accelOffy := 0;
  accelOffz := 0;
  calibCounter := 0;
  While GetTickCount64() - t < 5000 Do Begin
    CheckSynchronize(1);
  End;
  InCalibration := false;
  If calibCounter = 0 Then Begin
    showmessage('Error, did not get any accel events, did you forget to enable its interface ?');
    exit;
  End;
  accelOffx := accelOffx Div calibCounter;
  accelOffy := accelOffy Div calibCounter;
  accelOffz := accelOffz Div calibCounter;
  showmessage('Done.');
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  If Not XWiiMote.LED(wmLED1, CheckBox1.Checked) Then Begin
    ShowMessage('Unable to switch led, did you start the application with root rights ?');
  End;
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
Begin
  If Not XWiiMote.LED(wmLED2, CheckBox2.Checked) Then Begin
    ShowMessage('Unable to switch led, did you start the application with root rights ?');
  End;
End;

Procedure TForm1.CheckBox3Change(Sender: TObject);
Begin
  If Not XWiiMote.LED(wmLED3, CheckBox3.Checked) Then Begin
    ShowMessage('Unable to switch led, did you start the application with root rights ?');
  End;
End;

Procedure TForm1.CheckBox4Change(Sender: TObject);
Begin
  If Not XWiiMote.LED(wmLED4, CheckBox4.Checked) Then Begin
    ShowMessage('Unable to switch led, did you start the application with root rights ?');
  End;
End;

Procedure TForm1.OnWiiMoteKeyEvent(Sender: TObject; Key: Integer; State: integer
  );
Begin
  label9.caption := XWIIKeyToString(Key);
  label10.caption := XWIIKeyStateToString(State);
End;

Procedure TForm1.OnWiiMoteIREvent(Sender: Tobject; Valid: byte; pt1, pt2, pt3,
  pt4: TPoint);
Begin
  If valid >= 1 Then Begin
    Label3.Left := round((Panel1.Width * pt1.x) / 1024);
    Label3.Top := round((Panel1.Height * pt1.y) / 768);
  End;
  If valid >= 2 Then Begin
    Label4.Left := round((Panel1.Width * pt2.x) / 1024);
    Label4.Top := round((Panel1.Height * pt2.y) / 768);
  End;
  If valid >= 3 Then Begin
    Label5.Left := round((Panel1.Width * pt3.x) / 1024);
    Label5.Top := round((Panel1.Height * pt3.y) / 768);
  End;
  If valid >= 4 Then Begin
    Label6.Left := round((Panel1.Width * pt4.x) / 1024);
    Label6.Top := round((Panel1.Height * pt4.y) / 768);
  End;
End;

Procedure TForm1.OnWiiMoteAccelEvent(Sender: TObject; x, y, z: Int32);
Begin
  If InCalibration Then Begin
    accelOffx := accelOffx + x;
    accelOffy := accelOffy + y;
    accelOffz := accelOffz + z;
    calibCounter := calibCounter + 1;
    exit;
  End;

  x := x - accelOffx;
  y := y - accelOffy;
  z := z - accelOffz;

  label11.caption := format(
    'X = %d' + LineEnding + 'Y = %d' + LineEnding + 'Z = %d' + LineEnding +
    'Kalibrationoffset:' + LineEnding +
    'cX = %d' + LineEnding + 'cY = %d' + LineEnding + 'cZ = %d'
    , [x, y, z, accelOffx, accelOffy, accelOffz]);

  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Rectangle(-1, -1, 201, 201);

  RenderArrow(PaintBox1.Canvas, point(100, 100), point(100 + x, 100));
  RenderArrow(PaintBox1.Canvas, point(100, 100), point(100, 100 + y));
  RenderArrow(PaintBox1.Canvas, point(100, 100), point(100 + z, 100 - z));
End;

End.

