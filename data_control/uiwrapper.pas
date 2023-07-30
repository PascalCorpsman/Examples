(******************************************************************************)
(* UIWrapper                                                       23.04.2014 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Unit implements a wrapper to transparent switch         *)
(*               between TUart and TTCP                                       *)
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
(*               0.02 - Einfügen von Write für TInput und abgeleitete Klassen *)
(*               0.03 - Einfügen WriteBytes                                   *)
(*               0.04 - Einfügen Flush                                        *)
(*               0.05 - Forwarding der Stopbit konstanten                     *)
(*                                                                            *)
(******************************************************************************)
Unit uiwrapper;

{$MODE objfpc}{$H+}

Interface

(*
 * Defines or not defines the following "defines"

{$define useUUart}
{$define useTCP}
 *)
{$I uiwrapper.inc}

Uses
  Classes, SysUtils
{$IFDEF useUUart}
  , uuart_deprecated
{$ENDIF}
{$IFDEF useTCP}
  , utcp
{$ENDIF}
  ;
{$IFDEF useUUart}
// TUartInput Stopbit const forwarding
Const
  SB1 = uuart_deprecated.SB1;
  SB1andHalf = uuart_deprecated.SB1andHalf;
  SB2 = uuart_deprecated.SB2;
{$ENDIF}

Type

  { TInput }

  TInput = Class // Wrapper Klasse, welche die unabhängigen Methoden zur Verfügung stellt
  private
  public
    Constructor create(); virtual;
    Destructor destroy; override;
    (*
     * Eigentlich würde man diese Methoden als "Abstract" implementieren.
     * Dann bekommt man aber nur einen "Abstracten" Fehler, wenn die Kindklasse
     * die Methode nicht überschreibt.
     *)
    Function RecvByte(Out Error: Boolean; Timeout: Integer = 10): Byte; virtual; // Empfängt ein Byte, Error = True => Keine Daten innerhalb von Timeout empfangen
    Procedure Write(Const Value: String); virtual; // Schreibt einen String
    Procedure WriteBytes(Const data: Array Of Byte); virtual; // Schreibt beliebig viele Bytes
    Procedure Flush(); virtual; // Löscht den Empfangspuffer

    Function Connected(): Boolean; virtual; // Abstract
    Procedure Disconnect(); virtual; // Abstract
  End;

{$IFDEF useUUart}
  { TUartInput }

  TUartInput = Class(TInput) // Zugriff auf die Serielle Schnittstelle
  private
    FUart: TUart;
    fSizeRecvBuffer: integer;
    Function fGetReceivebuffer(): integer;
    Procedure setReceifebuffer(AValue: integer);
  public
    Property SizeRecvBuffer: integer read fGetReceivebuffer write setReceifebuffer;

    Constructor create(); override;
    Destructor destroy(); override;

    Function RecvByte(Out Error: Boolean; Timeout: Integer = 10): Byte; override;
    Procedure Write(Const Value: String); override; // Schreibt einen String
    Procedure WriteBytes(Const data: Array Of Byte); override; // Schreibt beliebig viele Bytes
    Procedure Flush(); override;

    Function connect(Port: String; Baudrate, Bits: Integer; Parity: Char; Stop: Integer; softflow, hardflow: boolean): Boolean;
    Function Connected(): Boolean; override;

    Procedure Disconnect(); override;
  End;
{$ENDIF}

{$IFDEF useTCP}
  { TTCPInput }

  TTCPInput = Class(TInput) // Zugriff auf die TCP-IP Schnittstelle, fia LNet
  private
    FTCP: TTCP;
    Procedure OnDisconnectEvent(aSocket: TLSocket);
  public
    OnDisconnect: TLSocketEvent;
    Constructor create(); override;
    Destructor destroy(); override;

    Function RecvByte(Out Error: Boolean; Timeout: Integer = 10): Byte; override;
    Procedure Write(Const Value: String); override; // Schreibt einen String
    Procedure WriteBytes(Const data: Array Of Byte); override; // Schreibt beliebig viele Bytes
    Procedure Flush(); override;

    Function Connect(IP_Address: String; Port: Word): Boolean;
    Function Connected(): Boolean; override;
    Procedure Disconnect(); override;

  End;
{$ENDIF}

Implementation
{$IFDEF useTCP}
{ TTCPInput }

Procedure TTCPInput.OnDisconnectEvent(aSocket: TLSocket);
Begin
  If assigned(OnDisconnect) Then Begin
    OnDisconnect(aSocket);
  End;
End;

Constructor TTCPInput.create;
Begin
  Inherited create;
  OnDisconnect := Nil;
  FTCP := TTCP.Create;
  ftcp.OnDisConnect := @OnDisconnectEvent;
End;

Destructor TTCPInput.destroy;
Begin
  FTCP.free;
  Inherited destroy;
End;

Function TTCPInput.RecvByte(Out Error: Boolean; Timeout: Integer): Byte;
Begin
  Result := FTCP.RecvByte(Error, Timeout);
End;

Procedure TTCPInput.Write(Const Value: String);
Begin
  FTCP.Write(value);
End;

Procedure TTCPInput.WriteBytes(Const data: Array Of Byte);
Var
  i: integer;
  dat: TBytes;
Begin
  dat := Nil;
  setlength(dat, length(data));
  For i := 0 To high(data) Do
    dat[i] := data[i];
  FTCP.WriteByteArr(dat);
  //FTCP.WriteByteArr(TBytes(@data)); -- Das geht wohl nicht immer
End;

Procedure TTCPInput.Flush;
Begin
  FTCP.Flush();
End;

Function TTCPInput.Connect(IP_Address: String; Port: Word): Boolean;
Begin
  result := FTCP.Connect(IP_Address, Port);
End;

Function TTCPInput.Connected: Boolean;
Begin
  result := FTCP.Connected();
End;

Procedure TTCPInput.Disconnect;
Begin
  FTCP.Disconnect();
End;

{$ENDIF}

{$IFDEF useUUart}
{ TUartInput }

Function TUartInput.fGetReceivebuffer: integer;
Begin
  result := fSizeRecvBuffer;
End;

Procedure TUartInput.setReceifebuffer(AValue: integer);
Begin
  fSizeRecvBuffer := AValue;
End;

Function TUartInput.connect(Port: String; Baudrate, Bits: Integer;
  Parity: Char; Stop: Integer; softflow, hardflow: boolean): Boolean;
Begin
{$IFDEF Windows}
  FUart.SizeRecvBuffer := fSizeRecvBuffer;
{$ENDIF}
  result := FUart.connect(Port, Baudrate, Bits, Parity, Stop, softflow, hardflow);
End;

Function TUartInput.Connected: Boolean;
Begin
  result := FUart.Connected;
End;

Procedure TUartInput.Disconnect;
Begin
  FUart.disconnect();
End;

Constructor TUartInput.create;
Begin
  Inherited create;
  FUart := TUart.create;
  fSizeRecvBuffer := FUart.SizeRecvBuffer;
End;

Destructor TUartInput.destroy;
Begin
  FUart.Free;
  Inherited destroy;
End;

Function TUartInput.RecvByte(Out Error: Boolean; Timeout: Integer): Byte;
Begin
  Result := FUart.RecvByte(Error, Timeout);
End;

Procedure TUartInput.Write(Const Value: String);
Begin
  FUart.Write(value);
End;

Procedure TUartInput.WriteBytes(Const data: Array Of Byte);
Var
  i: integer;
  dat: TBytes;
Begin
  dat := Nil;
  setlength(dat, length(data));
  For i := 0 To high(data) Do
    dat[i] := data[i];
  FUart.WriteByteArr(dat);
  //FUart.WriteByteArr(TBytes(@data)); -- Das geht wohl nicht immer
End;

Procedure TUartInput.Flush;
Begin
  FUart.Flush();
End;
{$ENDIF}

{ TInput }

Constructor TInput.create;
Begin
  Inherited create;
End;

Destructor TInput.destroy;
Begin
End;

Function TInput.RecvByte(Out Error: Boolean; Timeout: Integer): Byte;
Begin
  result := 0;
  error := true;
  Raise exception.create('Error virtual method "' + ClassName + '.RecvByte" is not declared.');
End;

Procedure TInput.Write(Const Value: String);
Begin
  Raise exception.create('Error virtual method "' + ClassName + '.Write" is not declared.');
End;

Procedure TInput.WriteBytes(Const data: Array Of Byte);
Begin
  Raise exception.create('Error virtual method "' + ClassName + '.WriteBytes" is not declared.');
End;

Procedure TInput.Flush;
Begin
  Raise exception.create('Error virtual method "' + ClassName + '.Flush" is not declared.');
End;

Function TInput.Connected: Boolean;
Begin
  Raise exception.create('Error virtual method "' + ClassName + '.Connected" is not declared.');
End;

Procedure TInput.Disconnect();
Begin
  Raise exception.create('Error virtual method "' + ClassName + '.Disconnect" is not declared.');
End;

End.

