(******************************************************************************)
(* uxwiimote.pas                                                   2021.10.18 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is the Lazarus Wrapper Class to easily access to        *)
(*               ulibxwiimote.                                                *)
(* Usage:                                                                     *)
(*                                                                            *)
(*        Creation / Initialization:                                          *)
(*        - TXWiiMonitor.Create                                               *)
(*        - TXWiiMonitor.GetDeviceList                                        *)
(*        - TXWiiMonitor.CreateXWiiMote -> TXWiiMote                          *)
(*        - TXWiiMote.OnKeyEvent := ?? [Optional]                             *)
(*        - TXWiiMote.OnIREvent := ??  [Optional]                             *)
(*        - TXWiiMote.OnAccelEvent := ??  [Optional]                          *)
(*        - TXWiiMote.Open( ?? )                                              *)
(*                                                                            *)
(*        Usage:                                                              *)
(*        - TXWiiMote.Rumble / .Battery / .LED                                *)
(*                                                                            *)
(*        Cleanup:                                                            *)
(*        - TXWiiMonitor.free                                                 *)
(*        - TXWiiMote.free                                                    *)
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
Unit uxwiimote;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ulibxwiimote, ufifo
{$IFDEF Linux}
  , BaseUnix
{$ENDIF}
  ;

Type

  (*
   * Die hier deklarierten Event Prototypen müssen zu den "Byte" einträgen von
   * TWiiCallBackData passen, sonst macht es keinen Sinn.
   *)
  TWiiMoteKeyEvent = Procedure(Sender: TObject; Key: Integer; State: integer) Of Object;
  TWiiMoteAccelEvent = Procedure(Sender: TObject; x, y, z: Int32) Of Object;
  TWiiMoteIREvent = Procedure(Sender: Tobject; Valid: byte; pt1, pt2, pt3, pt4: TPoint) Of Object;

  TWiiMoteLED = (wmLED1, wmLED2, wmLED3, wmLED4);

  TXWiiMonitor = Class;
  TXWiiMote = Class;

  TWiiCallBackData = Record
    CallbackType: integer;
    Sender: TXWiiMote;
    Case Byte Of
      0: (KeyCode: Byte; StateCode: Byte);
      1: (x, y, z: Int32);
      2: (Valid: Byte; pt1: TPoint; pt2: TPoint; pt3: TPoint; pt4: TPoint);
  End;

  TWiiCallBackFifo = specialize TFifo < TWiiCallBackData > ;

  (*
   * !! Attention !!
   *
   * Do not directly create TXWiiMote, use TXWiiMonitor to do this !
   *)
  { TXWiiMote }

  TXWiiMote = Class
  private
    fIsOpened: Boolean;
    fwii_iface: xwii_iface;
    fOwner: TXWiiMonitor;
    fwii_FileDesciptor: ptrint; // TODO: Geeigneten Datentyp finden
    fDevicePath: String;

    Procedure Poll();

  public
    OnKeyEvent: TWiiMoteKeyEvent;
    OnAccelEvent: TWiiMoteAccelEvent;
    OnIREvent: TWiiMoteIREvent;

    Property DevicePath: String read fDevicePath; // Just for fun, does not really have any use

    Constructor Create(aOwner: TXWiiMonitor; aDevicePath: String); virtual;
    Destructor Destroy(); override;

    (*
     * Interfaces
     *)
    Function Available(): UInt32; // Returns the Available interfaces see "ulibxwiimote.pas" ( XWII_IFACE_CORE ...)
    Function Open_interfaces(ifaces: Uint32): Boolean; // Open given interfaces see "ulibxwiimote.pas" ( XWII_IFACE_CORE ...)
    Function Opened_interfaces(): UInt32; // Gives all Opened interfaces see "ulibxwiimote.pas" ( XWII_IFACE_CORE ...)
    Procedure Close_Interfaces(ifaces: Uint32); // Close given interfaces see "ulibxwiimote.pas" ( XWII_IFACE_CORE ...)

    (*
     * Access to the Device
     *)
    Function Rumble(aValue: Boolean): Boolean;
    Function Battery(): Int8; // Returns 0%..100%, -1 on Failure
    Function LED(aLed: TWiiMoteLED; aValue: Boolean): Boolean;

    (*
     * Info's
     *)
    Function get_devtype(): String;
    Function get_extension(): String;
  End;

  { TXWiiMonitor }

  TXWiiMonitor = Class(TThread)
  private
    fWiiCallBackFifo: TWiiCallBackFifo;
    fxwiiMotes: Array Of TXWiiMote;

    FException: Exception;

    fCreateXWiiMote_Flag: Boolean;
    fCreateXWiiMote_DevicePath: String;
    fCreateXWiiMote_WiiMote: TXWiiMote;

    Procedure DoCreateXWiiMote;

    Procedure Setup(); // OnCreate
    Procedure Teardown(); // OnDestroy

    Procedure RegisterWiiMote(XWiiMote: TXWiiMote);
    Procedure UnRegisterWiiMote(XWiiMote: TXWiiMote);

    Procedure HandleException; // Exception Handling within Thread
    Procedure DoHandleException; // Exception Handling within Thread

    Procedure ProceedWiiCallBackFifo();
  protected
    Procedure Execute; override;
  public
    Constructor Create(); reintroduce;
    Destructor Destroy(); override;

    Function GetDeviceList(): TStringList;

    Function CreateXWiiMote(DevicePath: String): TXWiiMote;
  End;

Function XWIIKeyToString(Value: Integer): String;
Function XWIIKeyStateToString(Value: Integer): String;

Implementation

Function XWIIKeyToString(Value: Integer): String;
Begin
  result := 'Unknown';
  Case Value Of
    XWII_KEY_LEFT: result := 'Left';
    XWII_KEY_RIGHT: result := 'Right';
    XWII_KEY_UP: result := 'Up';
    XWII_KEY_DOWN: result := 'Down';
    XWII_KEY_A: result := 'A';
    XWII_KEY_B: result := 'B';
    XWII_KEY_PLUS: result := '+';
    XWII_KEY_MINUS: result := '-';
    XWII_KEY_HOME: result := 'Home';
    XWII_KEY_ONE: result := '1';
    XWII_KEY_TWO: result := '2';
    XWII_KEY_X: result := 'X';
    XWII_KEY_Y: result := 'Y';
    XWII_KEY_TL: result := 'TL';
    XWII_KEY_TR: result := 'TR';
    XWII_KEY_ZL: result := 'ZL';
    XWII_KEY_ZR: result := 'ZR';
  End;
End;

Function XWIIKeyStateToString(Value: Integer): String;
Begin
  result := 'Unknown';
  Case Value Of
    XWII_KEY_STATE_UP: result := 'up';
    XWII_KEY_STATE_DOWN: result := 'down';
    XWII_KEY_STATE_AUTO_REPEAT: result := 'auto-repeat';
  End;
End;

Procedure Nop();
Begin

End;

{ TXWiiMonitor }

Procedure TXWiiMonitor.Setup();
Begin
  fCreateXWiiMote_Flag := false;
  fxwiiMotes := Nil;
  fWiiCallBackFifo := TWiiCallBackFifo.create;
End;

Procedure TXWiiMonitor.Teardown();
Begin
  fWiiCallBackFifo.free;
  fWiiCallBackFifo := Nil;
End;

Procedure TXWiiMonitor.RegisterWiiMote(XWiiMote: TXWiiMote);
Var
  i: Integer;
Begin
  For i := 0 To high(fxwiiMotes) Do Begin
    If fxwiiMotes[i] = XWiiMote Then exit;
  End;
  setlength(fxwiiMotes, high(fxwiiMotes) + 2);
  fxwiiMotes[High(fxwiiMotes)] := XWiiMote;
End;

Procedure TXWiiMonitor.UnRegisterWiiMote(XWiiMote: TXWiiMote);
Var
  i, j: Integer;
Begin
  For i := 0 To high(fxwiiMotes) Do Begin
    If fxwiiMotes[i] = XWiiMote Then Begin
      For j := i To high(fxwiiMotes) - 1 Do Begin
        fxwiiMotes[j] := fxwiiMotes[j + 1];
      End;
      setlength(fxwiiMotes, high(fxwiiMotes));
    End;
  End;
End;

Procedure TXWiiMonitor.DoCreateXWiiMote;
Begin
  Try
    fCreateXWiiMote_WiiMote := TXWiiMote.Create(self, fCreateXWiiMote_DevicePath);
  Except
    fCreateXWiiMote_WiiMote := Nil;
  End;
End;

Procedure TXWiiMonitor.HandleException;
Begin
  FException := Exception(ExceptObject);
  Try
    Synchronize(@DoHandleException);
  Finally
    FException := Nil;
  End;
End;

Procedure TXWiiMonitor.DoHandleException;
Begin
  Raise FException;
End;

Procedure TXWiiMonitor.ProceedWiiCallBackFifo();
Var
  Event: TWiiCallBackData;
Begin
  While fWiiCallBackFifo.Count <> 0 Do Begin
    Event := fWiiCallBackFifo.Pop;
    Case Event.CallbackType Of
      XWII_EVENT_KEY: Begin
          If assigned(event.sender.OnKeyEvent) Then Begin
            event.sender.OnKeyEvent(event.sender, event.KeyCode, event.StateCode);
          End;
        End;
      XWII_EVENT_ACCEL: Begin
          If assigned(Event.Sender.OnAccelEvent) Then Begin
            Event.Sender.OnAccelEvent(Event.Sender, Event.x, Event.y, Event.z);
          End;
        End;
      XWII_EVENT_IR: Begin
          If assigned(Event.Sender.OnIREvent) Then Begin
            event.Sender.OnIREvent(Event.Sender, Event.Valid, event.pt1, event.pt2, event.pt3, event.pt4);
          End;
        End;
      // Todo: Implementieren der Restlichen Events..
    End;
  End;
End;

Procedure TXWiiMonitor.Execute;
Var
  i: Integer;
Begin
  Try
    Setup();
    While Not Terminated Do Begin
      (*
       * Abarbeiten der ganzen Asynchronen Requets durch die Hauptanwendung
       *)
      If fCreateXWiiMote_Flag Then Begin
        // Vorbedingung: fCreateXWiiMote_DevicePath befüllt
        DoCreateXWiiMote;
        // Nachbedingung: fCreateXWiiMote_WiiMote gesetzt oder NIL
        fCreateXWiiMote_Flag := false;
      End;
      // Alle WiiMotes auf Events Abklappern
      For i := 0 To high(fxwiiMotes) Do Begin
        fxwiiMotes[i].Poll();
      End;
      If fWiiCallBackFifo.Count <> 0 Then Begin
        Synchronize(@ProceedWiiCallBackFifo);
      End;
      sleep(1);
    End;
    Teardown();
  Except
    HandleException;
  End;
End;

Constructor TXWiiMonitor.Create();
Begin
  Inherited Create(true);
  FreeOnTerminate := false;
  Start;
End;

Destructor TXWiiMonitor.Destroy();
Begin
  Terminate;
  While Not Finished Do Begin
    CheckSynchronize(1);
  End;
  Inherited Destroy();
End;

Function TXWiiMonitor.GetDeviceList(): TStringList;
Var
  p: PChar;
  wii_Monitor: xwii_monitor;
Begin
  result := TStringList.Create;
  wii_Monitor := xwii_monitor_new(false, false);
  If Not assigned(wii_Monitor) Then Begin
    Raise Exception.Create('Error, could not create monitor instance.');
  End;
  Repeat
    p := xwii_monitor_poll(wii_Monitor);
    If assigned(p) Then Begin
      result.add(p);
    End;
  Until Not assigned(p);
  xwii_monitor_unref(wii_Monitor);
  wii_Monitor := Nil;
End;

Function TXWiiMonitor.CreateXWiiMote(DevicePath: String): TXWiiMote;
Begin
  result := Nil;
  fCreateXWiiMote_DevicePath := DevicePath;
  fCreateXWiiMote_Flag := true;
  While fCreateXWiiMote_Flag Do Begin
    CheckSynchronize(1);
  End;
  result := fCreateXWiiMote_WiiMote;
End;

{ TXWiiMote }

Constructor TXWiiMote.Create(aOwner: TXWiiMonitor; aDevicePath: String);
Begin
  Inherited Create;
  fOwner := Nil;
  OnKeyEvent := Nil;
  OnAccelEvent := Nil;
  OnIREvent := Nil;
  If Not assigned(aOwner) Then Begin
    Raise Exception.Create('Error invalid owner.');
  End;
  If xwii_iface_new(fwii_iface, pchar(aDevicePath)) <> 0 Then Begin
    Raise Exception.Create('Can not open: ' + aDevicePath);
  End;
  If Not assigned(fwii_iface) Then Begin
    Raise Exception.Create('Can not open: ' + aDevicePath);
  End;
  fwii_FileDesciptor := xwii_iface_get_fd(fwii_iface);
  If fwii_FileDesciptor = 0 Then Begin
    Raise exception.create('Error unable to get filedescriptor for: ' + aDevicePath);
  End;
  fDevicePath := aDevicePath;
  fIsOpened := false;
  fOwner := aOwner;
  fOwner.RegisterWiiMote(Self);
End;

Destructor TXWiiMote.Destroy();
Begin
  fIsOpened := false;
  If assigned(fOwner) Then Begin
    fOwner.UnRegisterWiiMote(Self);
  End;
  fOwner := Nil;
  xwii_iface_unref(fwii_iface);
  fwii_iface := Nil;
  fwii_FileDesciptor := 0;
End;

Procedure TXWiiMote.Poll();
Var
  buffer: Array[0..255] Of Byte; // Der Puffer muss mindestens 20 + 128 = 148 Bytes groß sein !!
  i: cint;
  Event: TWiiCallBackData;
  Info: stat;
  x, y, z: Int32;
Begin
  If Not fIsOpened Then exit;
  info.st_atime := 0; // -- Compiler beruhigen
  If FpFStat(fwii_FileDesciptor, info) <> 0 Then Begin // Ob das Sinn macht und überhaupt funktioniert ist Fragwürdig, es schadet aber zum Glück auch nicht ;)
    exit; // -- Ist die Datei generell Lesbar
  End;
  Repeat
    buffer[0] := 0; // -- Kill Compiler Warning
    i := xwii_iface_dispatch(fwii_iface, buffer, sizeof(buffer));
    If i = 0 Then Begin
      // Bytes 0 .. 15 stellen irgend einen Komischen Zeitstempel dar.
      Event.Sender := self;
      Event.CallbackType := buffer[16]; // Eigentlich Bytes 16 - 19
      Case Event.CallbackType Of
        XWII_EVENT_KEY: Begin
            (**
             * Core-interface key event
             *
             * The payload of such events is struct xwii_event_key. Valid
             * key-events include all the events reported by the core-interface,
             * which is normally only LEFT, RIGHT, UP, DOWN, A, B, PLUS, MINUS,
             * HOME, ONE, TWO.
             *)
            Event.KeyCode := buffer[20]; // Eigentlich Bytes 20 - 23
            Event.StateCode := buffer[24]; // Eigentlich Bytes 24 - 27
          End;
        XWII_EVENT_ACCEL: Begin
            (**
             * Accelerometer event
             *
             * Provides accelerometer data. Payload is struct xwii_event_abs
             * and only the first element in the abs-array is used. The x, y and z
             * fields contain the accelerometer data.
             * Note that the accelerometer reports acceleration data, not speed
             * data!
             *
             * ein ABS Datensatz besteht aus 12 Byte
             * immer 4 Byte sind eine vorzeichen Behaftete Zahl
             *)
            x := 0;
            y := 0;
            z := 0;
            For i := 0 To 3 Do Begin
              x := x Shl 8;
              x := x Or buffer[23 - i];
              y := y Shl 8;
              y := y Or buffer[27 - i];
              z := z Shl 8;
              z := z Or buffer[31 - i];
            End;
            Event.x := x;
            Event.y := y;
            Event.z := z;
          End;
        XWII_EVENT_IR: Begin
            (**
             * IR-Camera event
             *
             * Provides IR-camera events. The camera can track up two four IR
             * sources. As long as a single source is tracked, it stays at it's
             * pre-allocated slot. The four available slots are reported as
             * struct xwii_event_abs
             * payload. The x and y fields contain the position of each slot.
             *
             *)
            Event.Valid := 0;
            For i := 0 To 3 Do Begin
              x := buffer[20 + i * 12] Or (buffer[21 + i * 12] Shl 8); // Eigentlich Bytes 20 - 23
              y := buffer[24 + i * 12] Or (buffer[25 + i * 12] Shl 8); // Eigentlich Bytes 24 - 27
              // Bytes 28 - 31 sind die 3. Koodinate des ABS-Events diese ist aber immer 0 und wird weg geworfen
              If (x <> 1023) Or (y <> 1023) Then Begin
                inc(event.Valid);
                Case Event.Valid Of
                  1: Event.pt1 := point(x, y);
                  2: Event.pt2 := point(x, y);
                  3: Event.pt3 := point(x, y);
                  4: Event.pt4 := point(x, y);
                End;
              End;
            End;
          End;
        // Todo: Implementieren der Restlichen Events..
      End;
      fOwner.fWiiCallBackFifo.Push(Event);
    End;
  Until i <> 0;
End;

Function TXWiiMote.Available(): UInt32;
Begin
  result := xwii_iface_available(fwii_iface);
End;

Function TXWiiMote.Opened_interfaces(): UInt32;
Begin
  result := xwii_iface_opened(fwii_iface);
End;

Function TXWiiMote.Open_interfaces(ifaces: Uint32): Boolean;
Var
  ri, oi, i: cuint;
Begin
  result := false;
  // Schauen ob nicht eh schon alles geöffnet ist was gewünscht ist.
  oi := Opened_interfaces();
  i := oi And ifaces;
  If i = ifaces Then Begin
    result := true;
    exit;
  End;
  ri := oi Or ifaces;
  If xwii_iface_open(fwii_iface, ri) <> XWII_RETURN_OK Then exit;
  i := Opened_interfaces();
  // Das Writeable Bit wird nicht über Opened Interfaces zurück gegeben deswegen muss es an dieser Stelle wieder raus..
  ri := ri And (Not XWII_IFACE_WRITABLE);
  result := ri = i;
  fIsOpened := i <> 0;
End;

Procedure TXWiiMote.Close_Interfaces(ifaces: Uint32);
Begin
  xwii_iface_close(fwii_iface, ifaces);
End;

Function TXWiiMote.Rumble(aValue: Boolean): Boolean;
Begin
  result := xwii_iface_rumble(fwii_iface, aValue) = XWII_RETURN_OK;
End;

Function TXWiiMote.Battery(): Int8;
Var
  val: cuint8;
Begin
  result := -1;
  If xwii_iface_get_battery(fwii_iface, val) = 0 Then Begin
    result := val;
  End;
End;

Function TXWiiMote.LED(aLed: TWiiMoteLED; aValue: Boolean): Boolean;
Var
  l: integer;
Begin
  result := false;
  l := XWII_LED1;
  Case aLed Of
    wmLED1: l := XWII_LED1;
    wmLED2: l := XWII_LED2;
    wmLED3: l := XWII_LED3;
    wmLED4: l := XWII_LED4;
  End;
  result := xwii_iface_set_led(fwii_iface, l, avalue) = XWII_RETURN_OK;
End;

Function TXWiiMote.get_devtype(): String;
Var
  p: PChar;
Begin
  result := 'Can not extract devtype';
  p := Nil;
  If xwii_iface_get_devtype(fwii_iface, p) = 0 Then Begin
    result := String(p);
  End;
End;

Function TXWiiMote.get_extension(): String;
Var
  p: PChar;
Begin
  result := 'Can not extract extension';
  p := Nil;
  If xwii_iface_get_extension(fwii_iface, p) = 0 Then Begin
    result := String(p);
  End;
End;

End.

