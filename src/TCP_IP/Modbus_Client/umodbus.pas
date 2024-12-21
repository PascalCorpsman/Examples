(******************************************************************************)
(* umodbus.pas                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a ModBUS Client component either Serial or *)
(*               TCP IP                                                       *)
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


Unit umodbus;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, lNetComponents, lnet, uuart, ucrc;

Const
  MODBUS_CONNECTION_TIMEOUT = 150; // Zeit in mS, nach derer eingehende Nachrichten Definitiv als Neues Frame Erkannt werden (eigentlich müsste das im 1-3mS Bereich liegen)
  MODBUS_BROADCAST_ID = 0; // Nachrichten deren Broadcast ID = MODBUS_BROADCAST_ID werden nicht beantwortet !

Type

  (*
   * ID = Slave ID
   * FunctionCode = Modbus Funktionscode
   * StartAddress = Addresse ab derer die Daten geschrieben werden
   * Data = Empfangene Nutzdaten
   *)
  TReceivedData = Procedure(Sender: TObject; ID, FunctionCode, StartAddress: Integer; Const Data: Array Of Word) Of Object;

  (*
   * ID = Slave ID
   * FunctionCode = Modbus Funktionscode
   * StartAddress = Addresse ab derer die Daten geschrieben werden
   * Count = Anzahl der Daten (könnte man auch mittels Length(Data) haben, da Data Vor initialisiert wurde)
   * Data = Nutzdaten
   *)
  TRequestData = Procedure(Sender: TObject; ID, FunctionCode, StartAddress, Count: Integer; Var Data: Array Of Word) Of Object;

  TReceiveRecord = Record // Struktur zum Verwalten und Verarbeiten der Empfangenen Datenpackete
    (*
     * 0 = Empfange TCP Header
     * 1 = Auslesen Modbus ID
     * 2 = Auslesen Funktionscode
     * 3 = Auswerten Functionscode
     * 4 = Lesen Wortweise, RegCount oft
     * 5 = Empfange RTU CRC
     *)
    State: Integer; // s.o.
    Socket: TLSocket; // Der Socket an den dieser Record gebunden ist.
    ipHeader: Array[0..5] Of byte; // Der Empfangene IP-Header wird für die Quitierung benötigt
    RTUCRC: uint16; // Die Modbus RTU-empfangene CRC
    Counter: Integer; // Hilfsvariable zur für State
    Counter2: Integer; // Hilfsvariable zur für State
    ID: Integer; // Slave ID, an die die Nachricht ging
    (*
     * $02 =  2 = Read Discrete Inputs -- Not implemented yet
     * $03 =  3 = Read Holding Register
     * $06 =  6 = Write Single Register
     * $10 = 16 = Write Multiple Registers
     *)
    FC: Integer; // Funktionscode
    StartAddress: Integer; // Startaddresse
    RegCount: Integer; // Anzahl der Register
    ByteCount: Integer; // Anzahl der Bytes
    ResultingData: Array Of Word; // Empfangspuffer der Daten
    fLastReceiveTimeStamp: int64; // Zeitpunkt der Letzten Empfangenen Nachricht
  End;

  PReceiveRecord = ^TReceiveRecord;

  { TModbusClient }

  TModbusClient = Class
  private
    fReceiveList: Array Of TReceiveRecord; // Je verbundenem Client die Datenstruktur 1 mal vor halten

    fTCPConnection: TLTCPComponent; // TCP-Server

    fUartConnection: TUart; // RTU-Server
    fcrc: TCRC_Calculator; // CRC Calculator für Modbus RTU

    FHandledPackets: Integer;
    FDroppedPackets: Integer;
    // TCP Dinger
    Procedure OnAcceptTCPEvent(aSocket: TLSocket);
    Procedure OnLostTCPConnectionEvent(aSocket: TLSocket);
    Procedure OnReceiveTCPDataEvent(aSocket: TLSocket); // Callback zum Auswerten der Empfangenen Daten

    Function ResetPacket(Socket: TLSocket): PReceiveRecord; // Setzt die Interne Datenstruktur zum Empfangen eines Neuen Frames zurück
    Function GetReceiveRecord(Socket: TLSocket): PReceiveRecord;

    // Uart Dinger
    Procedure OnReceiveUartDataEvent(Sender: TObject; Data: TBytes);

    Procedure HandleReceiveRecord(Const RR: PReceiveRecord; Const Data: TBytes);
    Procedure HandleSendData(Const RR: PReceiveRecord; Const Data: TBytes);
  public
    OnReceiveData: TReceivedData; // Callback zur Verarbeitung der Empfangenen Frames
    OnRequestData: TRequestData; // Callback zur Verarbeitung zu Sendender Frames
    OnAccept: TLSocketEvent; // Callback zur Benachrichtigung, dass ein Client verbunden hat
    OnDisconnect: TLSocketEvent; // Callback zur Benachrichtigung, dass ein Client die Verbindung beendet hat

    Property DroppedFrames: Integer read FDroppedPackets; // Counter der verworfenen Frames
    Property HandledFrames: Integer read FHandledPackets; // Counter der Erfolgreich Empfangenen Frames

    Constructor Create(Connection: TLTCPComponent); overload;
    Constructor Create(Connection: TUart); overload;
    Destructor Destroy; override;

    Function TCPListen(Port: Integer): Boolean; // Öffnet den Port und Lauscht nach Modbus Frames
    Function UartConnect(Port: String; BaudRate: integer; Parity: Char): Boolean;

    Procedure Disconnect(); // Lauschen Beenden
  End;

Implementation

{ TModbusClient }

Constructor TModbusClient.Create(Connection: TLTCPComponent);
Begin
  Inherited create;
  fUartConnection := Nil;
  fTCPConnection := Connection;
  fTCPConnection.OnReceive := @OnReceiveTCPDataEvent;
  fTCPConnection.OnDisconnect := @OnLostTCPConnectionEvent;
  fTCPConnection.OnAccept := @OnAcceptTCPEvent;
  OnDisconnect := Nil;
  OnReceiveData := Nil;
  OnRequestData := Nil;
  OnAccept := Nil;
  fReceiveList := Nil;
  fcrc := Nil;
End;

Constructor TModbusClient.Create(Connection: TUart);
Begin
  Inherited create;
  fTCPConnection := Nil;
  fUartConnection := Connection;
  fUartConnection.OnReceive := @OnReceiveUartDataEvent;
  OnDisconnect := Nil;
  OnReceiveData := Nil;
  OnRequestData := Nil;
  OnAccept := Nil;
  fReceiveList := Nil;
  fcrc := TCRC_Calculator.create();
End;

Destructor TModbusClient.Destroy;
Var
  i: Integer;
Begin
  fTCPConnection := Nil;
  fUartConnection := Nil;
  If assigned(fcrc) Then fcrc.free;
  fcrc := Nil;
  For i := 0 To High(fReceiveList) Do Begin
    setlength(fReceiveList[i].ResultingData, 0);
  End;
  setlength(fReceiveList, 0);
  Inherited Destroy;
End;

Procedure TModbusClient.OnAcceptTCPEvent(aSocket: TLSocket);
Begin
  If assigned(OnAccept) Then Begin
    OnAccept(aSocket);
  End;
End;

Procedure TModbusClient.OnLostTCPConnectionEvent(aSocket: TLSocket);
Var
  i, j: Integer;
Begin
  If assigned(OnDisconnect) Then Begin
    OnDisconnect(aSocket);
  End;
  // Alles was mit diesem Socket zu tun hat wieder entfernen
  For i := 0 To High(fReceiveList) Do Begin
    If fReceiveList[i].Socket = aSocket Then Begin
      setlength(fReceiveList[i].ResultingData, 0);
      For j := i To High(fReceiveList) - 1 Do Begin
        fReceiveList[j] := fReceiveList[j + 1];
      End;
      setlength(fReceiveList, High(fReceiveList));
      exit;
    End;
  End;
End;

Procedure TModbusClient.OnReceiveTCPDataEvent(aSocket: TLSocket);
Var
  i: Integer;
  RR: PReceiveRecord;
  data: TBytes;
Begin
  (*
   * Hohlen des für den jeweiligen Socket passenden Empfangspuffer
   *)
  RR := GetReceiveRecord(aSocket);
  // Es gibt 2 Gründe die Datenstruktur fürs empfangen zu resetten
  // 1. Es gibt sie noch gar nicht
  // 2. Es ist schon mehr als 150 ms her, seit das letzte mal Daten über diesen Kanal eingetroffen sind.
  If (Not assigned(RR)) Or (GetTickCount64 - RR^.fLastReceiveTimeStamp > MODBUS_CONNECTION_TIMEOUT) Then Begin
    If (assigned(RR)) And // Drop Package nur, wenn es die Datenstruktur überhaupt gibt und etwas empfangen wurde.
    ((RR^.State <> 0) Or (RR^.Counter <> 0)) Then Begin
      inc(FDroppedPackets);
    End;
    RR := ResetPacket(aSocket); // Reset oder initiales Anlegen der Datenstruktur
  End;
  data := Nil;
  setlength(data, 4 * 1024);
  i := asocket.Get(data[0], 4 * 1024);
  setlength(data, i);
  HandleReceiveRecord(rr, data);
End;

Procedure TModbusClient.OnReceiveUartDataEvent(Sender: TObject; Data: TBytes);
Var
  RR: PReceiveRecord;
Begin
  (*
   * Hohlen des für den jeweiligen Socket passenden Empfangspuffer
   *)
  RR := GetReceiveRecord(Nil);
  // Es gibt 2 Gründe die Datenstruktur fürs empfangen zu resetten
  // 1. Es gibt sie noch gar nicht
  // 2. Es ist schon mehr als 150 ms her, seit das letzte mal Daten über diesen Kanal eingetroffen sind.
  If (Not assigned(RR)) Or (GetTickCount64 - RR^.fLastReceiveTimeStamp > MODBUS_CONNECTION_TIMEOUT) Then Begin
    If (assigned(RR)) And // Drop Package nur, wenn es die Datenstruktur überhaupt gibt und etwas empfangen wurde.
    ((RR^.State <> 1) Or (RR^.Counter <> 0)) Then Begin
      inc(FDroppedPackets);
    End;
    RR := ResetPacket(Nil); // Reset oder initiales Anlegen der Datenstruktur
  End;
  HandleReceiveRecord(rr, data);
End;

Procedure TModbusClient.HandleReceiveRecord(Const RR: PReceiveRecord;
  Const Data: TBytes);
Var
  ol, i, j: Integer;
  outbuf: TBytes;
  OutwBuf: Array Of Word;
  b1, b2: uInt8;
  s: String; //-- Only for debugging
Begin
  (*
   * Beispiel einer Nachricht mit selbem Inhalt als Modbus TCP / RTU

Modbus TCP:
     <TCP Header     > ID FC <ADD> <CNT>
In : 00 04 00 00 00 06 2A 03 10 00 00 04
     <TCP Header     > ID FC LEN<data>
Out: 00 04 00 00 00 0B 2A 03 08 48 61 67 65 72 00 00 00

Modbus RTU:
                       ID FC <ADD> <CNT> <CRC>
In :                   2A 03 10 00 00 04 46 D2
                       ID FC LEN<data                 > <CRC>
Out:                   2A 03 08 48 61 67 65 72 00 00 00 31 04
   *)
  (*
   * Byteweise auslesen der Daten, quasi als Stream, wäre auch Egal ob
   * die daten via mehrerer Packete ankommen
   *)
  outbuf := Nil;
  For i := 0 To High(Data) Do Begin
    Case RR^.State Of
      0: Begin // Abschneiden TCP-Header
          RR^.ipHeader[RR^.Counter] := Data[i];
          inc(RR^.Counter);
          If RR^.Counter = 6 Then Begin
            RR^.State := 1;
          End;
        End;
      1: Begin // Lesen ID
          RR^.ID := Data[i];
          RR^.State := 2;
        End;
      2: Begin // Lesen Funktionscode
          RR^.FC := Data[i];
          RR^.State := 3;
          RR^.Counter := 0;
          RR^.StartAddress := 0;
          RR^.RegCount := 0;
          RR^.ByteCount := 0;
        End;
      3: Begin // Lesen Funktionscode Header
          Case RR^.FC Of
            // Alle Formate der Form : ID, FC, 2Byte StartAddresse, 2Byte Anz. Register, 1Byte Anzahl Bytes und die die Rein gehackt wurden ;)
            3, 6, 16: Begin // Multiple Registers
                Case RR^.Counter Of
                  0..1: Begin
                      RR^.StartAddress := RR^.StartAddress Shl 8;
                      RR^.StartAddress := RR^.StartAddress Or Data[i];
                    End;
                  2..3: Begin
                      RR^.RegCount := RR^.RegCount Shl 8;
                      RR^.RegCount := RR^.RegCount Or Data[i];
                    End;
                  4: Begin
                      RR^.ByteCount := Data[i];
                    End;
                End;
                inc(RR^.Counter, 1);
                If (RR^.Counter = 5) Then Begin // wirkt nur bei FC = 16 -> Umschalten in Read Databytes
                  RR^.State := 4;
                  RR^.Counter := 0;
                  RR^.Counter2 := 0;
                  setlength(RR^.ResultingData, RR^.RegCount);
                End;
                // Funktionscode 6 hat kein RegCount
                If (RR^.FC = 6) And (RR^.Counter = 2) Then Begin // Wirkt nur bei FC = 6 -> Umschalten in Read Databytes
                  RR^.ByteCount := 0;
                  RR^.State := 4;
                  RR^.Counter := 0;
                  RR^.Counter2 := 0;
                  RR^.RegCount := 1;
                  setlength(RR^.ResultingData, RR^.RegCount);
                End;
                // Kommt eine Anfrage mittels FC 3 Rein, wird sie hier beantwortet
                If ((RR^.FC = 3) And (RR^.Counter = 4)) Then Begin // Wirkt nur bei FC = 3 -> Senden Antwort
                  If (assigned(rr^.Socket)) Then Begin
                    If rr^.ID <> MODBUS_BROADCAST_ID Then Begin // --FC 3, Broadcasts are not existing -> Ignore
                      inc(FHandledPackets);
                      setlength(outbuf, RR^.RegCount * 2 + 9);
                      outbuf[0] := RR^.ipHeader[0]; // Transaction identifier (high byte)
                      outbuf[1] := RR^.ipHeader[1]; // Transaction identifier (low byte)
                      outbuf[2] := RR^.ipHeader[2]; // Protocol Identifier (high byte)
                      outbuf[3] := RR^.ipHeader[3]; // Protocol Identifier (low byte)
                      outbuf[4] := byte((3 + RR^.RegCount * 2) Shr 8); // Länge der Nachfolgenden Daten in Byte (high byte)
                      outbuf[5] := byte(3 + RR^.RegCount * 2); // Länge der Nachfolgenden Daten in Byte (low byte)
                      outbuf[6] := byte(RR^.ID);
                      outbuf[7] := byte(RR^.FC);
                      outbuf[8] := byte(RR^.RegCount * 2);
                      If assigned(OnRequestData) Then Begin
                        OutwBuf := Nil;
                        setlength(OutwBuf, RR^.RegCount);
                        OnRequestData(self, RR^.ID, RR^.FC, RR^.StartAddress, RR^.RegCount, OutwBuf);
                        For j := 0 To High(OutwBuf) Do Begin
                          outbuf[j * 2 + 9] := byte(OutwBuf[j] Shr 8);
                          outbuf[j * 2 + 10] := byte(OutwBuf[j]);
                        End;
                      End;
                      HandleSendData(rr, outbuf);
                    End
                    Else Begin
                      inc(FDroppedPackets);
                    End;
                    ResetPacket(rr^.Socket); // TODO: Das ist etwas "umständlich"
                  End
                  Else Begin
                    rr^.State := 5;
                    RR^.Counter := 0;
                    RR^.RTUCRC := 0;
                  End;
                End;
              End;
          End;
        End;
      4: Begin // Lesen N-Worte als 2 Bytewerte
          Case RR^.Counter Of
            0: Begin // Lesen High Word
                RR^.ResultingData[RR^.Counter2] := Data[i] Shl 8;
                RR^.Counter := 1;
              End;
            1: Begin // lesen Low Word
                RR^.Counter := 0;
                RR^.ResultingData[RR^.Counter2] := RR^.ResultingData[RR^.Counter2] Or Data[i];
                inc(RR^.Counter2, 1); // Zählen der Empfangenen Worte
                If RR^.Counter2 = RR^.RegCount Then Begin
                  If assigned(rr^.Socket) Then Begin
                    inc(FHandledPackets);
                    // Der Gegenstelle sagen, dass wir alles Empfangen haben
                    Try
                      b1 := 0;
                      b2 := 0;
                      If RR^.FC = 6 Then Begin
                        b1 := byte(RR^.ResultingData[0] Shr 8);
                        b2 := byte(RR^.ResultingData[0]);
                      End;
                      If RR^.FC = 16 Then Begin
                        b1 := byte(RR^.RegCount Shr 8);
                        b2 := byte(RR^.RegCount Shr 0);
                      End;
                      setlength(outbuf, 12);
                      outbuf[0] := RR^.ipHeader[0]; // Transaction identifier (high byte)
                      outbuf[1] := RR^.ipHeader[1]; // Transaction identifier (low byte)
                      outbuf[2] := RR^.ipHeader[2]; // Protocol Identifier (high byte)
                      outbuf[3] := RR^.ipHeader[3]; // Protocol Identifier (low byte)
                      outbuf[4] := 0; // Länge der Nachfolgenden Daten in Byte (high byte)
                      outbuf[5] := 6; // Länge der Nachfolgenden Daten in Byte (low byte)
                      outbuf[6] := byte(RR^.ID);
                      outbuf[7] := byte(RR^.FC);
                      outbuf[8] := byte(RR^.StartAddress Shr 8);
                      outbuf[9] := byte(RR^.StartAddress Shr 0);
                      outbuf[10] := b1;
                      outbuf[11] := b2;
                      HandleSendData(rr, outbuf);
                    Except
                      On av: exception Do Begin
                        //log('TTCPModbusHandler.OnReceiveDataEvent :' + av.Message, llfatal);
                      End;
                    End;
                    // Empfangene Nachricht an die Anwendung übergeben
                    If assigned(OnReceiveData) Then Begin
                      (*
                       * Wenn der Client auf die Pakete antworten soll und nicht wie oben das
                       * hier in place gemacht wird. Dann muss die Socketnummer mit übergeben werden.
                       * Ebenso müsste dann wahrscheinlich auch ein Sendepuffer angelegt werden ...
                       *)
                      OnReceiveData(self, RR^.ID, RR^.FC, RR^.StartAddress, RR^.ResultingData);
                    End;
                    // Nächste Frame bitte
                    ResetPacket(rr^.Socket); // TODO: Das ist etwas "umständlich"
                  End
                  Else Begin
                    RR^.State := 5;
                    RR^.Counter := 0;
                    RR^.RTUCRC := 0;
                  End;
                End;
              End;
          End;
        End;
      5: Begin // Empfange Modbus RTU CRC
          RR^.RTUCRC := RR^.RTUCRC Shl 8;
          RR^.RTUCRC := RR^.RTUCRC Or Data[i];
          inc(RR^.Counter);
          If RR^.Counter = 2 Then Begin
            // Prüfen ob die CRC stimmt, wenn nicht -> Ignorieren...
            outbuf := Nil;
            setlength(outbuf,
              1 // ID
              + 1 // FC
              + 2 // Startaddress
              + 2 // Count
              );
            outbuf[0] := RR^.ID;
            outbuf[1] := RR^.FC;
            outbuf[2] := rr^.StartAddress Shr 8;
            outbuf[3] := rr^.StartAddress And $FF;
            outbuf[4] := rr^.RegCount Shr 8;
            outbuf[5] := rr^.RegCount And $FF;
            Case rr^.FC Of
              3: Begin
                  // Nichts zu tun stimmt alles schon ...
                End;
              6: Begin
                  If length(rr^.ResultingData) = 1 Then Begin
                    setlength(outbuf, 4); // FC 6 wiederhohlt das 1-Wort Payload anstatt der Quantity
                  End
                  Else Begin
                    // Fehler der FC 6 muss Payload 1 Wort haben !
                    inc(FDroppedPackets);
                    ResetPacket(rr^.Socket); // TODO: Das ist etwas "umständlich"
                    exit;
                  End;
                End;
              16: Begin
                  setlength(outbuf,
                    length(outbuf) + 1);
                  outbuf[high(outbuf)] := rr^.ByteCount;
                End;
            End;
            // Anhängen der Resulting Daten
            ol := length(outbuf);
            setlength(outbuf,
              ol +
              length(RR^.ResultingData) * 2
              + 2 // CRC
              );
            For j := 0 To high(RR^.ResultingData) Do Begin
              outbuf[j * 2 + ol] := RR^.ResultingData[j] Shr 8;
              outbuf[j * 2 + 1 + ol] := RR^.ResultingData[j] And $FF;
            End;
            outbuf[ol + length(RR^.ResultingData) * 2] := RR^.RTUCRC Shr 8;
            outbuf[ol + length(RR^.ResultingData) * 2 + 1] := RR^.RTUCRC And $FF;
            s := '';
            For j := 0 To high(outbuf) Do Begin
              s := s + format('%0.2X ', [outbuf[j]]);
            End;
            If fcrc.CalculateCRC(outbuf) = 0 Then Begin
              Case rr^.FC Of
                3: Begin // Read Holding Registers
                    If rr^.ID <> MODBUS_BROADCAST_ID Then Begin // --FC 3, Broadcasts are not existing -> Ignore
                      inc(FHandledPackets);
                      setlength(outbuf, RR^.RegCount * 2 + 3);
                      outbuf[0] := RR^.ID;
                      outbuf[1] := rr^.FC;
                      outbuf[2] := RR^.RegCount * 2; // Länge der Nachfolgenden Daten in Byte
                      If assigned(OnRequestData) Then Begin
                        setlength(OutwBuf, RR^.RegCount);
                        OnRequestData(self, RR^.ID, RR^.FC, RR^.StartAddress, RR^.RegCount, OutwBuf);
                        For j := 0 To High(OutwBuf) Do Begin
                          outbuf[j * 2 + 3] := byte(OutwBuf[j] Shr 8);
                          outbuf[j * 2 + 4] := byte(OutwBuf[j]);
                        End;
                      End;
                      HandleSendData(rr, outbuf);
                    End
                    Else Begin
                      inc(FDroppedPackets);
                    End;
                    ResetPacket(rr^.Socket); // TODO: Das ist etwas "umständlich"
                  End;
                6, 16: Begin // Write Holding Registers
                    inc(FHandledPackets);
                    // Der Gegenstelle sagen, dass wir alles Empfangen haben
                    Try
                      b1 := 0;
                      b2 := 0;
                      If RR^.FC = 6 Then Begin
                        b1 := byte(RR^.ResultingData[0] Shr 8);
                        b2 := byte(RR^.ResultingData[0]);
                      End;
                      If RR^.FC = 16 Then Begin
                        b1 := byte(RR^.RegCount Shr 8);
                        b2 := byte(RR^.RegCount Shr 0);
                      End;
                      setlength(outbuf, 6);
                      outbuf[0] := byte(RR^.ID);
                      outbuf[1] := byte(RR^.FC);
                      outbuf[2] := byte(RR^.StartAddress Shr 8);
                      outbuf[3] := byte(RR^.StartAddress Shr 0);
                      outbuf[4] := b1;
                      outbuf[5] := b2;
                      HandleSendData(rr, outbuf);
                    Except
                      On av: exception Do Begin
                        //log('TTCPModbusHandler.OnReceiveDataEvent :' + av.Message, llfatal);
                      End;
                    End;
                    // Empfangene Nachricht an die Anwendung übergeben
                    If assigned(OnReceiveData) Then Begin
                      (*
                       * Wenn der Client auf die Pakete antworten soll und nicht wie oben das
                       * hier in place gemacht wird. Dann muss die Socketnummer mit übergeben werden.
                       * Ebenso müsste dann wahrscheinlich auch ein Sendepuffer angelegt werden ...
                       *)
                      OnReceiveData(self, RR^.ID, RR^.FC, RR^.StartAddress, RR^.ResultingData);
                    End;
                    // Nächste Frame bitte
                    ResetPacket(rr^.Socket); // TODO: Das ist etwas "umständlich"
                  End;
              End;
            End
            Else Begin
              inc(FDroppedPackets);
            End;
          End;
        End;
    End;
  End;
End;

Procedure TModbusClient.HandleSendData(Const RR: PReceiveRecord;
  Const Data: TBytes);
Var
  crc: uint16;
Begin
  (*
   * Do not answer Broadcast Messages !
   *)
  If rr^.ID = MODBUS_BROADCAST_ID Then exit;
  If assigned(rr^.Socket) Then Begin
    rr^.Socket.Send(data[0], length(Data));
  End
  Else Begin
    // Modubus RTU -> Anhängen der CRC und dann Raus
    If Not assigned(fUartConnection) Then exit;
    crc := fcrc.CalculateCRC(Data);
    fUartConnection.SendBytes(data);
    fUartConnection.SendBytes([crc And $FF, crc Shr 8]);
  End;
End;

Function TModbusClient.ResetPacket(Socket: TLSocket): PReceiveRecord;
Begin
  result := GetReceiveRecord(Socket); // Hohlen der Bisherigen Datenstruktur
  If Not assigned(result) Then Begin // Der Socket existiert nicht, also legen wir ihn an ;) -- Das könnte man eigentlich auch in OnAccept machen *g*
    setlength(fReceiveList, High(fReceiveList) + 2);
    result := @fReceiveList[High(fReceiveList)];
    result^.Socket := Socket;
  End;
  // Setzt alle Empfangspuffer relevanten Daten auf 0 zurück
  If assigned(Socket) Then Begin
    result^.State := 0;
  End
  Else Begin
    result^.State := 1; // im RTU Modus gibt es keinen TCP-Header ;)
  End;
  result^.Counter := 0;
  setlength(result^.ResultingData, 0);
  result^.fLastReceiveTimeStamp := GetTickCount64;
End;

Function TModbusClient.GetReceiveRecord(Socket: TLSocket): PReceiveRecord;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To High(fReceiveList) Do Begin
    If fReceiveList[i].Socket = Socket Then Begin
      result := @fReceiveList[i];
      exit;
    End;
  End;
End;

Function TModbusClient.TCPListen(Port: Integer): Boolean;
Begin
  result := false;
  If Not assigned(fTCPConnection) Then exit;
  If fTCPConnection.Connected Then fTCPConnection.Disconnect(true);
  result := fTCPConnection.Listen(Port);
  FHandledPackets := 0;
  FDroppedPackets := 0;
End;

Function TModbusClient.UartConnect(Port: String; BaudRate: integer; Parity: Char
  ): Boolean;
Begin
  result := false;
  If Not assigned(fUartConnection) Then exit;
  If fUartConnection.IsConnected Then fUartConnection.Disconnect();
  result := fUartConnection.Connect(Port, BaudRate, 8, Parity, SB1, false, false);
  FHandledPackets := 0;
  FDroppedPackets := 0;
End;

Procedure TModbusClient.Disconnect;
Begin
  If assigned(fTCPConnection) Then Begin
    If fTCPConnection.Connected Then Begin
      fTCPConnection.Disconnect(true);
    End;
  End;
  If assigned(fUartConnection) Then Begin
    If fUartConnection.IsConnected Then fUartConnection.Disconnect();
  End;
End;

End.

