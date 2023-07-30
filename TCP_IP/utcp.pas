(******************************************************************************)
(* UTCP                                                            23.04.2014 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Unit implements a Control class for the L-Net TLTCP     *)
(*               Component                                                    *)
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
(*               0.02 - Added LCL Support, Write                              *)
(*               0.04 - Added WriteByteArr                                    *)
(*               0.03 - Added utcp.inc                                        *)
(*               0.05 - Bugfix WriteByteArr                                   *)
(*               0.06 - added Flush                                           *)
(*                                                                            *)
(******************************************************************************)
Unit utcp;

{$MODE objfpc}{$H+}

(*
 * Soll die utcp.pas als Library genutzt werden, dann sollte die utcp.inc
 * aus dem Verzeichnis der utcp.pas gelöscht werden. Für jedes Projekt, welches
 * utcp.pas einbindet. Muss dann im Projektordner eine seperate utcp.inc
 * angelegt werden.
 *)
{$I utcp.inc}

Interface

Uses
  Classes, SysUtils, lnet
{$IFDEF UseLCL}
  , forms, lclintf
{$ENDIF}
  ;

Type

  TLSocketEvent = lnet.TLSocketEvent;
  TLSocket = lnet.TLSocket;

  { TTCP }

  TTCP = Class
  private
    receivebuffer: Array Of Byte; // Die Empfangenen Daten
    FCon: TLTcp; // the connection
    Procedure OnDs(aSocket: TLSocket);
    Procedure OnRe(aSocket: TLSocket);
    Procedure OnEr(Const msg: String; aSocket: TLSocket);
{$IFDEF UseLCL}
    Procedure OnIdle(Sender: TObject; Var Done: Boolean);
{$ENDIF}
  public
    OnDisConnect: TLSocketEvent;

    Constructor create(); virtual;
    Destructor destroy(); override;

    Function Connect(IP_Address: String; Port: Word): Boolean;
    Function Connected(): Boolean;

    Procedure Disconnect();

    Function RecvByte(Out Error: Boolean; Timeout: Integer = 10): Byte;
    Procedure Write(Const Value: String);
    Procedure WriteByteArr(Const Data: TBytes); // Sendet einen Byte Datanstrom
    Procedure Flush();
  End;

Implementation

{$IFNDEF UseLCL}

{$IFDEF use_own_gettickcount}

Function GetTickCount64(): QWord;
Begin
  result := round(now * 1000 * 60 * 60 * 24);
End;

{$ELSE}
Uses lazutf8sysutils;

Function GetTickCount64(): QWord;
Begin
  Result := lazutf8sysutils.GetTickCount64();
End;
{$ENDIF}
{$ENDIF}

{ TTCP }

Procedure TTCP.OnDs(aSocket: TLSocket);
Begin
{$IFDEF UseLCL}
  // Todo : Ausgabe einer Fehlermeldung
{$ENDIF}
{$IFDEF UseConsole}
  Writeln('Lost connection');
{$ENDIF}
  If assigned(OnDisConnect) Then Begin
    OnDisConnect(aSocket);
  End;
End;

Procedure TTCP.OnRe(aSocket: TLSocket);
Var
  i, asize: integer;
  buf: Array[0..2047] Of Byte;
  j: Integer;
Begin
  asize := aSocket.Get(buf[0], 2048);
  While asize > 0 Do Begin
    i := high(receivebuffer);
    setlength(receivebuffer, i + 1 + asize);
    For j := 0 To asize - 1 Do Begin
      receivebuffer[i + 1 + j] := buf[j];
    End;
    If asize = 2048 Then Begin
      asize := aSocket.Get(buf[0], 2048);
    End
    Else Begin
      asize := 0;
    End;
  End;
End;

Procedure TTCP.OnEr(Const msg: String; aSocket: TLSocket);
Begin
{$IFDEF UseLCL}
  // Todo : Ausgabe einer Fehlermeldung
{$ENDIF}
{$IFDEF UseConsole}
  Writeln('Error : ' + msg); // if error occured, write it
{$ENDIF}
End;

{$IFDEF UseLCL}

Procedure TTCP.OnIdle(Sender: TObject; Var Done: Boolean);
Begin
  FCon.CallAction;
  Done := false; // Evtl. muss hier auch immer False stehen ??
End;
{$ENDIF}

Constructor TTCP.create;
Begin
  Inherited create;
  OnDisConnect := Nil;
  FCon := TLTCP.Create(Nil); // create new TCP connection with no parent component
  FCon.OnError := @OnEr; // assign callbacks
  FCon.OnReceive := @OnRe;
  FCOn.OnDisconnect := @OnDs;
  FCon.Timeout := 1;
  setlength(receivebuffer, 0);
{$IFDEF UseLCL}
  Application.AddOnIdleHandler(@OnIdle, true);
{$ENDIF}
End;

Destructor TTCP.destroy;
Begin
{$IFDEF UseLCL}
  Application.RemoveOnIdleHandler(@OnIdle);
{$ENDIF}
  FCon.Free; // free the connection
  setlength(receivebuffer, 0);
End;

Function TTCP.Connect(IP_Address: String; Port: Word): Boolean;
Var
  t: qword;
  FQuit: Boolean;
Begin
{$IFDEF DEBUG_CONSOLE}
  writeln('Connecting to : ' + IP_Address + ' : ' + inttostr(port));
{$ENDIF}
  FQuit := False;
  t := GetTickCount64;
  If FCon.Connect(IP_Address, Port) Then Begin // if connect went ok
    Repeat
      FCon.CallAction; // wait for "OnConnect"
      If GetTickCount64 > t + 2000 Then
        FQuit := true;
    Until FCon.Connected Or FQuit;
  End
  Else Begin
    FQuit := true;
  End;
  If FQuit Then Begin
{$IFDEF UseLCL}
    // Todo : Ausgabe einer Fehlermeldung
{$ENDIF}
{$IFDEF UseConsole}
    writeln('Error could not establish connection.');
{$ENDIF}
  End;
  result := Not FQuit;
End;

Function TTCP.Connected: Boolean;
Begin
  result := FCon.Connected;
End;

Procedure TTCP.Disconnect();
Begin
  FCon.Disconnect(true);
End;

Function TTCP.RecvByte(Out Error: Boolean; Timeout: Integer): Byte;
Var
  t: QWord;
  i: Integer;
Begin
  t := GetTickCount64;
  While true Do Begin
    // Abbruch bei Erfolg
    If high(receivebuffer) > -1 Then Begin
      error := false;
      // Das hier ist höchst ineffizient, da ja ständig die Größe des Puffers geändert wird.
      // Aber es geht, so wat..
      result := receivebuffer[0];
      For i := 1 To high(receivebuffer) Do Begin
        receivebuffer[i - 1] := receivebuffer[i];
      End;
      SetLength(receivebuffer, high(receivebuffer));
      exit;
    End;
    // Abbruch bei nicht Erfolg
    If t + Timeout < GetTickCount64() Then Begin
      result := 0;
      error := true;
      exit;
    End;
    FCon.CallAction;
  End;
End;

Procedure TTCP.Write(Const Value: String);
Begin
  fcon.SendMessage(value);
  FCon.CallAction;
End;

Procedure TTCP.WriteByteArr(Const Data: TBytes);
Var
{$IFDEF DEBUG_CONSOLE}
  s: String;
{$ENDIF}
  c, i, j: integer;
  buf_: Array[0..4095] Of byte;
Begin
{$IFDEF DEBUG_CONSOLE}
  s := 'Send:';
  For i := 0 To high(Data) Do Begin
    s := s + format(' %0.2X', [data[i]]);
  End;
  writeln(s);
{$ENDIF}
  // Es gehen nur Statische Arrays, also müssen wir als 4kb-Blöcke senden
  For i := 0 To length(data) Div (4096) Do Begin
    c := 0;
    For j := 0 To 4095 Do Begin
      If i * 4096 + j > high(data) Then break;
      buf_[j] := data[i * 4096 + j];
      inc(c);
    End;
    FCon.send(buf_, c);
    FCon.CallAction;
  End;
End;

Procedure TTCP.Flush;
Begin
  setlength(receivebuffer, 0);
End;

End.

