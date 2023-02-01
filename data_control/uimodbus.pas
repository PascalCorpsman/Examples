(******************************************************************************)
(* uimodbus                                                        27.11.2014 *)
(*                                                                            *)
(* Version     : 0.08                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Unit implements a abstract modbus implementation        *)
(*               for Tinput (uiwrapper.pas)                                   *)
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
(*               0.02 hinzufügen WriteMultipleRegisters                       *)
(*               0.03 Umstellen auf ReceiveRawBytesCnt                        *)
(*               0.04 Die Rohdaten Routinen sind nun Virtual, so können       *)
(*                    Abgeleitete Klassen diese noch "Beeinflussen" oder Lesen*)
(*               0.05 Einfügen ReadDiscreteInputs                             *)
(*               0.06 Anpassen Sichtbarkeiten der Debugg Routinen             *)
(*               0.07 Support für Modbus BroadCast                            *)
(*               0.08 Read Input Registers                                    *)
(*                                                                            *)
(* Echt gute Doku: https://www.simplymodbus.ca/FC04.htm                       *)
(*                                                                            *)
(******************************************************************************)
Unit uimodbus;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, uiwrapper
  , ucrc // http://corpsman.de/index.php?doc=komponenten/crc
  ;

Const
  Modbus_BroadCast_ID = 0;

Type

  tBools = Array Of Boolean;
  tWords = Array Of Word; // Der Modbus überträgt in 16-Bit Worten Pro Register
  TModbusMode = (mmRTU, mmTCP {, mmASCII, mmTCPASCII});

  { TUIModbusServer }

  TUIModbusServer = Class
  private
    fSendingDevice: TInput;
    fcrc: TCRC_Calculator;
    ftcptn: Word; // die TCP-Transaktionsnummer
  protected
    Function HandleRegisterEvent(FC: integer; SlaveID: Byte; Address: Word; Quantity: Byte; TimeOut: integer = 10): tWords;
    (*
     * !! ACHTUNG !!
     * Die Nachfolgenden Routinen geben direkten Zugriff auf das
     * Übertragungsmedium, das hat nichts mit Modbus zu tun.
     *)
    // Löscht den Empfangspuffer
    Procedure Flush;
    // Sendet die Daten direkt (ohne weitere Verarbeitungsschritte) an das SendingDevice
    Procedure WriteRawBytes(Const data: Array Of Byte); virtual; // ACHTUNG : das hier hat nichts mit Modbus zu tun !!
    // Liest so lange Daten bis Timeout Lange nichts mehr kommt
    Function ReceiveRawBytes(TimeOut: integer = 10): TBytes; virtual; // ACHTUNG : das hier hat nichts mit Modbus zu tun !!
    // Liest genau ByteCount Bytes und Bricht dann ab. Das Erspart das warten auf Timeout, wenn die Anzahl der zu empfangenen Bytes bekannt ist.
    Function ReceiveRawBytesCnt(ByteCount: integer; TimeOut: integer = 10): TBytes; virtual; // ACHTUNG : das hier hat nichts mit Modbus zu tun !!
  public
    Mode: TModbusMode;
    Constructor Create(Const SendingDevice: TInput);
    Destructor destroy; override;
    (*
     * Modbus Kommunikationsroutinen
     *)
    Function ReadDiscreteInput(SlaveID: Byte; Address: Word; Quantity: integer; TimeOut: integer = 10): tBools;
    Function ReadHoldingRegisters(SlaveID: Byte; Address: Word; Quantity: Byte; TimeOut: integer = 10): tWords;
    Function ReadInputRegisters(SlaveID: Byte; Address: Word; Quantity: Byte; TimeOut: integer = 10): tWords;
    Function WriteSingleRegister(SlaveID: Byte; Address: Word; Data: Word; TimeOut: integer = 10): boolean;
    Function WriteMultipleRegisters(SlaveID: Byte; Address: Word; Data: tWords; TimeOut: integer = 10): boolean;
  End;

Implementation

{ TUIModbus }

Constructor TUIModbusServer.Create(Const SendingDevice: TInput);
Begin
  fSendingDevice := SendingDevice;
  fcrc := TCRC_Calculator.create();
  Mode := mmRTU;
  ftcptn := 1;
End;

Destructor TUIModbusServer.destroy;
Begin
  fcrc.Free;
End;

Function TUIModbusServer.ReadDiscreteInput(SlaveID: Byte; Address: Word;
  Quantity: integer; TimeOut: integer): tBools;
Var
  b: TBytes;
  crc: word;
  c, i, j: Integer;
  rescnt: integer;
Begin
  rescnt := 0;
  result := Nil;
  If SlaveID = Modbus_BroadCast_ID Then exit;
  fSendingDevice.Flush();
  // Todo : Einbaun von Checks wie Quantity / 8 <= 125 ...
  Case mode Of
    mmTCP: Begin
        b := Nil;
        setlength(b, 12);
        b[0] := (ftcptn Shr 8) And $FF;
        b[1] := (ftcptn Shr 0) And $FF;
        b[2] := 0; // Protokollzeichen Immer 0
        b[3] := 0; // Protokollzeichen Immer 0
        b[4] := 0; // Anzahl der folgenden Bytes
        b[5] := 6; // Anzahl der folgenden Bytes
        b[6] := SlaveID;
        b[7] := 02;
        b[8] := (Address Shr 8) And $FF;
        b[9] := (Address Shr 0) And $FF;
        b[10] := 0;
        b[11] := Quantity;
        rescnt := 9 + ((Quantity - 1) Div 8 + 1);
      End;
    mmrtu: Begin
        b := Nil;
        setlength(b, 8);
        b[0] := SlaveID;
        b[1] := 02;
        b[2] := (Address Shr 8) And $FF;
        b[3] := (Address Shr 0) And $FF;
        b[4] := Quantity Div 256;
        b[5] := Quantity;
        crc := fcrc.CalculateCRClen(b, 6);
        b[6] := (crc Shr 0) And $FF;
        b[7] := (crc Shr 8) And $FF;
        rescnt := 5 + ((Quantity - 1) Div 8 + 1);
      End;
  End;
  WriteRawBytes(b);
  If rescnt = 0 Then Begin
    b := ReceiveRawBytes(TimeOut);
  End
  Else Begin
    b := ReceiveRawBytesCnt(rescnt, TimeOut);
  End;
  // Auswerten der Antwort
  If assigned(b) Then Begin
    Case mode Of
      mmTCP: Begin
          // Transaktionsnummer OK
          If (b[0] = (ftcptn Shr 8) And $FF) And
            (b[1] = (ftcptn Shr 0) And $FF) And
            // Modbus Antwort OK
          (b[7] = 02) Then Begin
            setlength(result, Quantity);
            c := 0;
            For i := 0 To ((Quantity - 1) Div 8) Do Begin
              For j := 0 To 7 Do Begin
                If c < Quantity Then Begin
                  result[c] := (b[9 + i] And (1 Shl j)) <> 0;
                End;
                inc(c);
              End;
            End;
          End;
          ftcptn := ftcptn + 1;
        End;
      mmrtu: Begin
          // CRC-OK
          If (fcrc.CalculateCRC(b) = 0)
            // Modbus Antwort OK
          And (b[1] = 02) Then Begin
            setlength(result, Quantity);
            c := 0;
            For i := 0 To ((Quantity - 1) Div 8) Do Begin
              For j := 0 To 7 Do Begin
                If c < Quantity Then Begin
                  result[c] := (b[3 + i] And (1 Shl j)) <> 0;
                End;
                inc(c);
              End;
            End;
          End;
        End;
    End;
  End;
End;

Function TUIModbusServer.ReadHoldingRegisters(SlaveID: Byte; Address: Word;
  Quantity: Byte; TimeOut: integer): tWords;
Begin
  result := HandleRegisterEvent($03, SlaveID, Address, Quantity, TimeOut);
End;

Function TUIModbusServer.ReadInputRegisters(SlaveID: Byte; Address: Word;
  Quantity: Byte; TimeOut: integer): tWords;
Begin
  result := HandleRegisterEvent($04, SlaveID, Address, Quantity, TimeOut);
End;

Function TUIModbusServer.WriteSingleRegister(SlaveID: Byte; Address: Word;
  Data: Word; TimeOut: integer): boolean;
Var
  dat: TBytes;
  crc: word;
  rescnt: integer;
Begin
  fSendingDevice.Flush(); // Löschen alter Daten
  rescnt := 0;
  // Todo : Einbaun von Checks wie Quantity <= 125 ...
  Case mode Of
    mmTCP: Begin
        dat := Nil;
        setlength(dat, 12);
        dat[0] := (ftcptn Shr 8) And $FF;
        dat[1] := (ftcptn Shr 0) And $FF;
        dat[2] := 0; // Protokollzeichen Immer 0
        dat[3] := 0; // Protokollzeichen Immer 0
        dat[4] := 0; // Anzahl der folgenden Bytes
        dat[5] := 6; // Anzahl der folgenden Bytes
        dat[6] := SlaveID;
        dat[7] := $06;
        dat[8] := (Address Shr 8) And $FF;
        dat[9] := (Address Shr 0) And $FF;
        dat[10] := (data Shr 8) And $FF;
        dat[11] := (data Shr 0) And $FF;
        rescnt := 12; // Sind immer 12 Byte (Transaktionsnummer[2], Protokollkennzeichen[2], Anzahl der noch fehlenden Bytes[2], SlaveID[1], FunktionsCode[1], Startaddresse[2], Anzahl Register[2])
      End;
    mmRTU: Begin
        setlength(dat, 8);
        dat[0] := SlaveID;
        dat[1] := $06;
        dat[2] := (Address Shr 8) And $FF;
        dat[3] := (Address Shr 0) And $FF;
        dat[4] := (data Shr 8) And $FF;
        dat[5] := (data Shr 0) And $FF;
        crc := fcrc.CalculateCRClen(dat, 6);
        dat[6] := (crc Shr 0) And $FF;
        dat[7] := (crc Shr 8) And $FF;
        rescnt := 8; // Sind immer 8 Byte (SlaveID[1], FunktionsCode[1], Startaddresse[2], Anzahl Register[2], CRC[2])
      End;
  End;
  WriteRawBytes(dat);
  // Broadcast nachrichten werden nicht beantwortet
  If SlaveID = Modbus_BroadCast_ID Then Begin
    result := true;
    exit;
  End;
  If rescnt = 0 Then Begin
    dat := ReceiveRawBytes(TimeOut); // Keine Ahnung wie viele Bytes empfangen werden sollen ?
  End
  Else Begin
    dat := ReceiveRawBytesCnt(rescnt, TimeOut);
  End;
  result := assigned(dat);
  If result Then Begin // Wir haben was empfangen ist es auch Akzeptiert worden ?
    Case mode Of
      mmTCP: Begin
          result := dat[7] = $06;
          If result Then Begin
            ftcptn := ftcptn + 1;
          End;
        End;
      mmRTU: Begin
          // Theoretisch könnte man hier noch mehr testen ...
          result := (dat[1] = $06) And (fcrc.CalculateCRC(dat) = 0);
        End;
    End;
  End;
End;

Function TUIModbusServer.WriteMultipleRegisters(SlaveID: Byte; Address: Word;
  Data: tWords; TimeOut: integer): boolean;
Var
  dat: TBytes;
  i: Integer;
  crc: word;
  bc: word;
  rescnt: integer;
Begin
  fSendingDevice.Flush(); // Löschen alter Daten
  rescnt := 0;
  // Todo : Einbaun von Checks wie Quantity <= 125 ...
  assert(length(data) <= 125);
  Case mode Of
    mmTCP: Begin
        dat := Nil;
        bc := 2 * Length(data) + 7;
        setlength(dat, bc + 6);
        dat[0] := (ftcptn Shr 8) And $FF;
        dat[1] := (ftcptn Shr 0) And $FF;
        dat[2] := 0; // Protokollzeichen Immer 0
        dat[3] := 0; // Protokollzeichen Immer 0
        dat[4] := (bc Shr 8) And $FF; // Anzahl der folgenden Bytes
        dat[5] := (bc Shr 0) And $FF; // Anzahl der folgenden Bytes
        dat[6] := SlaveID;
        dat[7] := $10;
        dat[8] := (Address Shr 8) And $FF;
        dat[9] := (Address Shr 0) And $FF;
        dat[10] := 0;
        dat[11] := length(data);
        dat[12] := 2 * length(data);
        For i := 0 To high(data) Do Begin
          dat[13 + i * 2] := (data[i] Shr 8) And $FF;
          dat[14 + i * 2] := (data[i] Shr 0) And $FF;
        End;
        rescnt := 12; // Sind immer 12 Byte (Transaktionsnummer[2], Protokollkennzeichen[2], Anzahl der noch fehlenden Bytes[2], SlaveID[1], FunktionsCode[1], Startaddresse[2], Anzahl Register[2])
      End;
    mmRTU: Begin
        setlength(dat, 2 * Length(data) + 9);
        dat[0] := SlaveID;
        dat[1] := $10;
        dat[2] := (Address Shr 8) And $FF;
        dat[3] := (Address Shr 0) And $FF;
        dat[4] := 0;
        dat[5] := length(data);
        dat[6] := length(data) * 2;
        For i := 0 To high(data) Do Begin
          dat[7 + i * 2] := (data[i] Shr 8) And $FF;
          dat[8 + i * 2] := (data[i] Shr 0) And $FF;
        End;
        crc := fcrc.CalculateCRClen(dat, 2 * Length(data) + 7);
        dat[2 * Length(data) + 7] := (crc Shr 0) And $FF;
        dat[2 * Length(data) + 8] := (crc Shr 8) And $FF;
        rescnt := 8; // Sind immer 8 Byte (SlaveID[1], FunktionsCode[1], Startaddresse[2], Anzahl Register[2], CRC[2])
      End;
  End;
  WriteRawBytes(dat);
  // Broadcast nachrichten werden nicht beantwortet
  If SlaveID = Modbus_BroadCast_ID Then Begin
    result := true;
    exit;
  End;
  If rescnt = 0 Then Begin
    dat := ReceiveRawBytes(TimeOut); // Keine Ahnung wie viele Bytes empfangen werden sollen ?
  End
  Else Begin
    dat := ReceiveRawBytesCnt(rescnt, TimeOut);
  End;
  result := assigned(dat);
  If result Then Begin // Wir haben was empfangen ist es auch Akzeptiert worden ?
    Case mode Of
      mmTCP: Begin
          result := dat[7] = $10;
          If result Then Begin
            ftcptn := ftcptn + 1;
          End;
        End;
      mmRTU: Begin
          result := (dat[1] = $10) And (fcrc.CalculateCRC(dat) = 0);
        End;
    End;
  End;
End;

Function TUIModbusServer.HandleRegisterEvent(FC: integer; SlaveID: Byte;
  Address: Word; Quantity: Byte; TimeOut: integer): tWords;
Var
  b: TBytes;
  crc: word;
  i: Integer;
  rescnt: integer;
Begin
  result := Nil;
  If SlaveID = Modbus_BroadCast_ID Then exit;
  rescnt := 0;
  fSendingDevice.Flush();
  // Todo : Einbaun von Checks wie Quantity <= 125 ...
  assert(Quantity <= 125);
  Case mode Of
    mmTCP: Begin
        b := Nil;
        setlength(b, 12);
        b[0] := (ftcptn Shr 8) And $FF;
        b[1] := (ftcptn Shr 0) And $FF;
        b[2] := 0; // Protokollzeichen Immer 0
        b[3] := 0; // Protokollzeichen Immer 0
        b[4] := 0; // Anzahl der folgenden Bytes
        b[5] := 6; // Anzahl der folgenden Bytes
        b[6] := SlaveID;
        b[7] := FC;
        b[8] := (Address Shr 8) And $FF;
        b[9] := (Address Shr 0) And $FF;
        b[10] := 0;
        b[11] := Quantity;
        rescnt := 9 + 2 * Quantity;
      End;
    mmrtu: Begin
        b := Nil;
        setlength(b, 8);
        b[0] := SlaveID;
        b[1] := FC;
        b[2] := (Address Shr 8) And $FF;
        b[3] := (Address Shr 0) And $FF;
        b[4] := 0;
        b[5] := Quantity;
        crc := fcrc.CalculateCRClen(b, 6);
        b[6] := (crc Shr 0) And $FF;
        b[7] := (crc Shr 8) And $FF;
        rescnt := 5 + 2 * Quantity;
      End;
  End;
  WriteRawBytes(b);
  If rescnt = 0 Then Begin
    b := ReceiveRawBytes(TimeOut);
  End
  Else Begin
    b := ReceiveRawBytesCnt(rescnt, TimeOut);
  End;
  // Auswerten der Antwort
  If assigned(b) Then Begin
    Case mode Of
      mmTCP: Begin
          // Transaktionsnummer OK
          If (b[0] = (ftcptn Shr 8) And $FF) And
            (b[1] = (ftcptn Shr 0) And $FF) And
            // Modbus Antwort OK
          (b[7] = FC) Then Begin
            setlength(result, Quantity);
            For i := 0 To Quantity - 1 Do Begin
              result[i] := (b[9 + 2 * i] Shl 8) Or (b[10 + 2 * i] Shl 0);
            End;
          End;
          ftcptn := ftcptn + 1;
        End;
      mmrtu: Begin
          // CRC-OK
          If (fcrc.CalculateCRC(b) = 0)
            // Modbus Antwort OK
          And (b[1] = FC) Then Begin
            setlength(result, Quantity);
            For i := 0 To Quantity - 1 Do Begin
              result[i] := (b[3 + 2 * i] Shl 8) Or (b[4 + 2 * i] Shl 0);
            End;
          End;
        End;
    End;
  End;
End;

Procedure TUIModbusServer.Flush;
Begin
  fSendingDevice.Flush();
End;

Procedure TUIModbusServer.WriteRawBytes(Const data: Array Of Byte);
Begin
  fSendingDevice.WriteBytes(data);
End;

Function TUIModbusServer.ReceiveRawBytes(TimeOut: integer): TBytes;
Var
  err: Boolean;
  value: Byte;
  l: integer;
Begin
  result := Nil;
  setlength(result, 1024);
  l := 0;
  err := false;
  While Not err Do Begin
    value := fSendingDevice.RecvByte(err, TimeOut);
    If Not err Then Begin
      result[l] := value;
      inc(l);
      If l > high(result) Then setlength(result, high(result) + 1025);
    End;
  End;
  setlength(result, l);
End;

Function TUIModbusServer.ReceiveRawBytesCnt(ByteCount: integer; TimeOut: integer
  ): TBytes;
Var
  i: integer;
  value: Byte;
  err: Boolean;
Begin
  result := Nil;
  setlength(result, ByteCount);
  err := false;
  For i := 0 To ByteCount - 1 Do Begin
    value := fSendingDevice.RecvByte(err, TimeOut);
    If err Then Begin
      setlength(result, 0);
      exit;
    End;
    result[i] := value;
  End;
End;

End.

