(******************************************************************************)
(* uuart.pas                                                       01.02.2021 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Uart implementation within a TThread (using synaser)         *)
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
(*               0.02 - do not kill uart connection on a error                *)
(*               0.03 - Add: "Purge"                                          *)
(*                                                                            *)
(******************************************************************************)

(*
 * Usage:
 *
 * Init and start:
 *     Instance := TUart.Create(true);
 *     Instance.OnReceive := @OnReceive;  // <-- Set the receive Event handler
 *     Instance.FreeOnTerminate := false;
 *     Instance.start;
 *     ..
 *
 *     While Not Instance.IsRunning Do Begin // This is only needed, if you connect right after start
 *      sleep(1); // This is only needed, if you connect right after start
 *     End; // This is only needed, if you connect right after start
 *     Instance.Connect(..);
 *
 * Use:
 *    Instance.SendBytes .. Instance.SendString
 *
 * Free:
 *    Instance.Terminate;
 *    While Instance.IsRunning Do Begin
 *      CheckSynchronize(1);
 *    End;
 *    Instance.Free;
 *    Instance := Nil;
 *)

Unit uuart;

{$MODE objfpc}{$H+}

(*
 * Soll die uuart.pas als Library genutzt werden, dann sollte die uuart.inc
 * aus dem Verzeichnis der uuart.pas gelöscht werden. Für jedes Projekt, welches
 * uuart.pas einbindet. Muss dann im Projektordner eine seperate uuart.inc
 * angelegt werden.
 *
 * Folgende Defines können/sollten definiert werden:
 *
 * UseLCL : Wenn die uuart Klasse unter der LCL läuft (ausgabe von Fehlern via Dialog)
 * UseConsole : Wenn die uuart Klasse fehler auf die Konsole loggen soll.
 *
 *)
{$I uuart.inc}

Interface

Uses
  Classes, SysUtils, synaser, ufifo
{$IFDEF UseLCL}
  , dialogs //debug
{$ENDIF}
  ;

Const
  SB1 = synaser.SB1;
  SB1andHalf = synaser.SB1andHalf;
  SB2 = synaser.SB2;

Type

  TOnReceive = Procedure(Sender: TObject; Data: TBytes) Of Object; // TODO: das Muss konfigurierbar auf ohne Object sein !!

  TBytesFifo = specialize TFifo < TBytes > ;

  { TUart }

  TUart = Class(TThread)
  private
    fLastError: integer;
    freceivebuffer: integer;

    FOnReceive: TOnReceive;
    fNeedDisconnect: Boolean;
    fNeedPurge: Boolean;

    fCom: TBlockSerial; // Handle auf die COM-Schnittstelle

    fReceived: TBytesFifo; // Fifo aller Empfangener Nachrichten, die an den Hauptthread sollen
    fSend: TBytesFifo; // Fifo aller Nachrichten die gesendet werden sollen

    fIsRunning: Boolean; // True, wenn wir in der Thread.execute sind

    Function GetReceivebuffer: integer;
    Procedure SetOnReceive(AValue: TOnReceive);
    Function GetConnected(): Boolean;

    Procedure Init(); // der Interne Constructor
    Procedure setReceifebuffer(AValue: integer);
    Procedure Shutdown(); // der Interne Destructor

    Procedure ThreadReceive(); // Wird aus Thread.Execute heraus aufgerufen und Spricht mit MainThread
  protected
    Procedure Execute; override;

  public
    Property LastError: integer read fLastError;

    Property SizeRecvBuffer: integer read GetReceivebuffer write setReceifebuffer;

    Property IsRunning: Boolean read fIsRunning;
    Property IsConnected: Boolean read GetConnected;

    Property OnReceive: TOnReceive read FOnReceive write SetOnReceive;

    Function Connect(aPort: String; aBaudrate, aBits: Integer; aParity: Char; aStop: Integer; asoftflow, ahardflow: boolean): Boolean;
    Procedure Disconnect();

    Procedure Purge;

    Function SendBytes(Value: TBytes): Boolean;
    Function SendString(Value: String): Boolean;

    Constructor Create(CreateSuspended: Boolean;
      Const StackSize: SizeUInt = DefaultStackSize);
  End;

Function StringToBytes(Value: String): TBytes;
Function BytesToString(Value: TBytes): String;

Function GetSerialPortNames(): String;

Implementation

Function GetSerialPortNames(): String;
{$IFDEF WINDOWS}
Begin
  result := synaser.GetSerialPortNames();
{$ELSE}
Var
  sl: TStringlist;
Var
  Info: TSearchRec;
  hdl: THandle;
  b: Boolean;
Begin
  sl := TStringlist.create;
  If FindFirst('/dev/tty*', faSysFile, Info) = 0 Then Begin
    Repeat
      b := true;
      Try
        hdl := FileOpen('/dev/' + info.Name, fmOpenReadWrite);
        If hdl = -1 Then Begin
          b := false;
        End;
      Except
        b := false;
      End;
      If hdl >= 0 Then Begin
        FileClose(hdl);
      End;
      If b Then Begin
        sl.Add('/dev/' + info.Name);
      End;
    Until FindNext(info) <> 0;
  End;
  FindClose(Info);
  result := sl.CommaText;
  sl.free;
{$ENDIF}
End;

Function ErrorCodeToString(ErrorCode: integer): String;
Begin
  // Die Bedeutung der Errorcodes ist hier :
  // http://synapse.ararat.cz/doc/help/synaser.html
  // bzw. hier :
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms681382%28v=vs.85%29.aspx
  (*
   * Generell bei Fehlern unter Linux, prüfen ob der User in der "tty" bzw. "dialout" gruppe ist !
   *)
  Case ErrorCode Of
    // Es sind beileibe nicht alle aufgeführt, nur die die bisher mal auftraten ...
    0: result := 'ERROR_SUCCESS';
    2: result := '2 = ERROR_FILE_NOT_FOUND';
    5: result := '5 = ERROR_ACCESS_DENIED';
    13: result := '13 = The data is invalid.';
    87: result := '87 = ERROR_INVALID_PARAMETER';
    9991: Begin
        result := '9991 = ERROR_ALREADY_OWNED';
{$IFDEF LINUX}
        result := result + LineEnding;
        result := result + 'See in "' + LockfileDirectory + '" for lockfile informations.';
{$ENDIF}
      End;
    9992: result := '9992 = ERROR_ALREADY_IN_USE';
    9993: result := '9993 = ERROR_WRONG_PARAMETER';
    9994: result := '9994 = ERROR_PORT_NOT_OPEN';
    9995: result := '9995 = ERROR_NO_DEVICE_ANSWER';
    9996: result := '9996 = ERROR_MAX_BUFFER';
    9997: result := '9997 = ERROR_TIMEOUT';
    9998: result := '9998 = ERROR_NOT_READ';
    9999: result := '9999 = ERROR_FRAME';
    10000: result := '10000 = ERROR_OVERRUN';
    10001: result := '10001 = ERROR_RX_OVER';
    10002: result := '10002 = ERROR_RX_PARITY';
    10003: result := '10003 = ERROR_TX_FULL';
  Else
    result := inttostr(ErrorCode);
  End;
End;

Function StringToBytes(Value: String): TBytes;
Var
  i: integer;
Begin
  result := Nil;
  setlength(result, length(value));
  For i := 1 To length(value) Do Begin
    result[i - 1] := ord(value[i]);
  End;
End;

Function BytesToString(Value: TBytes): String;
Var
  i: integer;
Begin
  result := '';
  setlength(result, length(Value));
  For i := 0 To high(Value) Do Begin
    result[i + 1] := chr(value[i]);
  End;
End;

{ TUart }

Procedure TUart.Init();
Begin
  fCom := Nil;
  freceived := TBytesFifo.create;
  fSend := TBytesFifo.create;
  fNeedDisconnect := false;
  fNeedPurge := false;
End;

Procedure TUart.Shutdown();
Begin
  If assigned(fCom) Then Begin
    fCom.Free;
  End;
  fCom := Nil;
  freceived.free;
  freceived := Nil;
  fSend.Free;
  fSend := Nil;
End;

Procedure TUart.SetOnReceive(AValue: TOnReceive);
Begin
  If FOnReceive = AValue Then Exit;
  FOnReceive := AValue;
End;

Function TUart.GetReceivebuffer: integer;
Begin
  result := freceivebuffer;
End;

Procedure TUart.setReceifebuffer(AValue: integer);
Begin
{$IFDEF LINUX}
{$IFDEF UseLCL}
  showmessage('SizeRecvBuffer only valid under Windows');
{$ENDIF}
{$IFDEF UseConsole}
  writeln('SizeRecvBuffer only valid under Windows');
{$ENDIF}
{$ELSE}
  If assigned(fCom) Then Begin
    Raise Exception.Create('Error set receivebuffer while connected.');
  End
  Else Begin
    freceivebuffer := AValue;
  End;
{$ENDIF}
End;


Function TUart.GetConnected(): Boolean;
Begin
  result := assigned(fCom);
End;

Procedure TUart.ThreadReceive();
Var
  s: TBytes;
Begin
  If Not freceived.isempty Then Begin
    s := freceived.Pop;
    If assigned(OnReceive) Then Begin
      OnReceive(self, s);
    End;
  End;
End;

Function TUart.Connect(aPort: String; aBaudrate, aBits: Integer; aParity: Char; aStop: Integer; asoftflow, ahardflow: boolean): Boolean;
Begin
  result := Not IsConnected;
  If Not result Then exit;
  fCom := TBlockSerial.Create;
{$IFDEF Windows}
  fCom.SizeRecvBuffer := freceivebuffer;
{$ENDIF}
  fCom.Connect(aPort);
  sleep(100);
  If fCom.LastError <> 0 Then Begin
{$IFDEF UseLCL}
    showmessage('Error, could not connect to device error number : ' + ErrorCodeToString(fCom.LastError));
{$ENDIF}
{$IFDEF UseConsole}
    writeln('Error, could not connect to device error number : ' + ErrorCodeToString(fCom.LastError));
{$ENDIF}
    fCom.Free;
    fcom := Nil;
    result := false;
    exit;
  End;
  fCom.Config(aBaudrate, aBits, aParity, aStop, asoftflow, ahardflow);
  sleep(100);
  If fCom.LastError <> 0 Then Begin
{$IFDEF UseLCL}
    showmessage('Error, could not config device error number : ' + ErrorCodeToString(fCom.LastError));
{$ENDIF}
{$IFDEF UseConsole}
    writeln('Error, could not config device error number : ' + ErrorCodeToString(fCom.LastError));
{$ENDIF}
    fCom.Free;
    fcom := Nil;
    result := false;
    exit;
  End;
  result := true;
End;

Procedure TUart.Disconnect();
Begin
  fNeedDisconnect := true;
End;

Procedure TUart.Purge();
Begin
  fNeedPurge := true;
End;

Function TUart.SendBytes(Value: TBytes): Boolean;
Begin
  result := IsConnected;
  If Not result Then exit;
  fSend.Push(value);
End;

Function TUart.SendString(Value: String): Boolean;
Var
  bt: TBytes;
Begin
  bt := StringToBytes(Value);
  result := SendBytes(bt);
End;

Constructor TUart.Create(CreateSuspended: Boolean; Const StackSize: SizeUInt);
Begin
  Inherited Create(CreateSuspended, StackSize);
  freceivebuffer := 4096; // Default value from Synaser
End;

Procedure TUart.Execute;
  Function CheckComState(ForceDisconnect: Boolean = false): Boolean;
  Begin
    result := false;
    fLastError := fCom.LastError;
    If (fLastError <> sOK) Or (ForceDisconnect) Then Begin
      // Im Falle eines Fehlers die COM einfach ab zu schießen macht keinen Sinn und sorgt für mehr Probleme als es löst
      If ForceDisconnect Then Begin
        fcom.free;
        fcom := Nil;
      End;
      fSend.Clear;
      fReceived.Clear;
      result := true;
    End;
  End;

Var
  bt: TBytes;
  i: integer;
Begin
  Init();
  fIsRunning := true;
  While Not Terminated Do Begin
    If fNeedPurge And assigned(fCom) Then Begin
      fCom.Purge;
      fNeedPurge := false;
    End;
    If fNeedDisconnect And assigned(fCom) Then Begin
      fNeedDisconnect := false;
      CheckComState(true);
    End;
    If assigned(fCom) Then Begin
      While Not fSend.isempty Do Begin
        bt := fSend.Pop;
        For i := 0 To high(bt) Do Begin
          fCom.SendByte(bt[i]);
          If CheckComState(false) Then break;
        End;
      End;
      If assigned(fcom) And (fCom.WaitingData <> 0) Then Begin
        setlength(bt, fCom.WaitingData);
        For i := 0 To high(bt) Do Begin
          bt[i] := fcom.RecvByte(0);
          If CheckComState(false) Then break;
        End;
        If assigned(fcom) Then Begin
          freceived.Push(bt);
        End;
      End;
      While Not freceived.isempty Do Begin
        Synchronize(@ThreadReceive);
      End;
    End;
    Sleep(1); // Prevent 100% CPU-Load
  End;
  Shutdown();
  fIsRunning := false;
End;

End.

