(******************************************************************************)
(* uMQTTbroker.pas                                                 13.10.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is a really small MQTT broker for use with the Lnet     *)
(*               component.                                                   *)
(*                                                                            *)
(*               !! Attention !!                                              *)
(*                                                                            *)
(*               This is a work in Progress not all MQTT features are         *)
(*               implemented, nor fully understood.                           *)
(*                                                                            *)
(*               Whats working:                                               *)
(*                -listen on a port                                           *)
(*                -accept incomming connections (no checking, no logging)     *)
(*                -call callbacks when client send:                           *)
(*                 * CONNECT                                                  *)
(*                 * PUBLISH                                                  *)
(*                 * SUBSCRIBE (only informing, no implementation of features)*)
(*                 * PINGREQ                                                  *)
(*                                                                            *)
(*               So this component is only usefull if a client is directly    *)
(*               publishing to the broker and the brokers callbacks are used  *)
(*               to evaluate the published values.                            *)
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
(* Known Issues: it seems that the socket is not freeed correctly             *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)

(*
 * Documentation: http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html
 *)

Unit uMQTTbroker;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, lNet;

// Table 2.1 - Control packet types
Const
  CPT_CONNECT = 1;
  CPT_CONNACK = 2;
  CPT_PUBLISH = 3;
  CPT_PUBACK = 4;
  CPT_PUBREC = 5;
  CPT_PUBREL = 6;
  CPT_PUBCOMP = 7;
  CPT_SUBSCRIBE = 8;
  CPT_SUBACK = 9;
  CPT_UNSUBSCRIBE = 10;
  CPT_UNSUBACK = 11;
  CPT_PINGREQ = 12;
  CPT_PINGRESP = 13;
  CPT_DISCONNECT = 14;
  //  CPT_RESERVED = 15;

Type
  (*
   * For each connection a separate client
   *)
  TClients = Record
    ID: Integer; // For External Reverences, always increasing number > 0
    Socket: TLSocket;
    RecvBytes: Array Of Byte;
    MQTTConnected: Boolean;
    // TODO: Add list of Subsribes and identifier
  End;

  // Figure 2.1
  TMQTTPacket = Record
    ControlPacketType: byte;
    Flags: byte;
    Payload: Array Of Byte;
  End;

  // Table 3.4
  TQoS = (QoS0, QoS1, QoS2);

  // Figure 3.27
  TReturn = (rQoS0, rQoS1, rQoS2, rFailure);

  TOnLog = Procedure(Sender: TObject; ClientID: integer; LogText: String) Of Object; // 0 = from Broker
  TOnSubscribeRequest = Function(Sender: TObject; ClientID: integer; Subscription: String): TReturn Of Object;
  TOnPublishRequest = Procedure(Sender: TObject; ClientID: integer; aName, aPayload: String; DUP, Retain: Boolean) Of Object;
  TOnClientEvent = Procedure(Sender: TObject; ClientID: integer) Of Object;

  { TMQTTBroker }

  TMQTTBroker = Class
  private
    fClientIDs: integer;
    fconnection: TLTcp;
    fClients: Array Of TClients;

    Function GetListening: Boolean;
    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnReceive(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Function SocketToindex(Const aSocket: TLSocket): integer;

    (*
     * Versucht aus den bereits empfangenen Daten ein "komplettes" zu extrahieren
     *)
    Function TryExtractaPacket(Clientindex: Integer; Out aPacket: TMQTTPacket): Boolean;

    (*
     * Behandelt alle Packete außer das Connect Packet
     *)
    Procedure HandlePacket(Clientindex: Integer; Const aPacket: TMQTTPacket);

    (*
     * Jedes Packet hat seinen eigenen Event Handler, damit das besser "Maintainbar" ist.
     *)
    Procedure HandleConnectPacket(Clientindex: Integer; Const aPacket: TMQTTPacket);
    Procedure HandleSubscribePacket(Clientindex: Integer; Const aPacket: TMQTTPacket);
    Procedure HandlePublishPacket(Clientindex: Integer; Const aPacket: TMQTTPacket);
    Procedure HandlePingPacket(Clientindex: Integer; Const aPacket: TMQTTPacket);

    Procedure Log(ClientID: integer; Const aValue: String);
  public
    Port: integer; // Default 1883

    OnLog: TOnLog; // Callback for internal "logging" informations

    OnAcceptMQTTClient: TOnClientEvent;
    OnSubscribeRequest: TOnSubscribeRequest;
    OnPublishRequest: TOnPublishRequest;
    OnPingEvent: TOnClientEvent;

    Property Listening: Boolean read GetListening;

    Constructor Create(Const aConnection: TLTcp); virtual;
    Destructor Destroy(); override;

    Function Listen: Boolean;
    Procedure Disconnect;

    Procedure CallAction(); // Only needed if TLTcp is not LCL-Component (console mode applications)
  End;

Implementation

Procedure Nop;
Begin

End;

{ TMQTTBroker }

Constructor TMQTTBroker.Create(Const aConnection: TLTcp);
Begin
  Inherited create();
  fconnection := aConnection;
  Port := 1883;

  fconnection.OnAccept := @OnAccept;
  fconnection.OnDisconnect := @OnDisconnect;
  fconnection.OnReceive := @OnReceive;
  fconnection.OnError := @OnError;

  OnLog := Nil;

  OnAcceptMQTTClient := Nil;
  OnSubscribeRequest := Nil;
  OnPublishRequest := Nil;
  OnPingEvent := Nil;

  fClients := Nil;
End;

Destructor TMQTTBroker.Destroy;
Begin
  If fconnection.Connected Then fconnection.Disconnect(true);
  // Connection is not owned -> so no fconnection.free; !
End;

Function TMQTTBroker.Listen: Boolean;
Begin
  Disconnect;
  setlength(fClients, 0);
  result := fconnection.Listen(Port);
  If result Then Begin
    fClientIDs := 0;
    log(0, 'Start listening on port: ' + inttostr(Port));
  End;
End;

Procedure TMQTTBroker.Disconnect;
Begin
  If fconnection.Connected Then Begin
    fconnection.Disconnect(true);
    CallAction;
  End;
End;

Procedure TMQTTBroker.CallAction;
Begin
  fconnection.CallAction;
End;

Procedure TMQTTBroker.OnAccept(aSocket: TLSocket);
Begin
  setlength(fClients, high(fClients) + 2);
  inc(fClientIDs);
  fClients[high(fClients)].Socket := aSocket;
  fClients[high(fClients)].ID := fClientIDs;
  fClients[high(fClients)].RecvBytes := Nil;
  fClients[high(fClients)].MQTTConnected := false;
  log(fClientIDs, 'Accepted connection');
End;

Function TMQTTBroker.GetListening: Boolean;
Begin
  result := fconnection.Connected;
End;

Procedure TMQTTBroker.OnDisconnect(aSocket: TLSocket);
Var
  ci, i, j: Integer;
Begin
  // Den Client aus der Liste der Bekannten werfen
  For i := 0 To high(fClients) Do Begin
    If fClients[i].Socket = aSocket Then Begin
      ci := fClients[i].ID;
      For j := i To high(fClients) - 1 Do Begin
        fClients[j] := fClients[j + 1];
      End;
      setlength(fClients, high(fClients));
      log(ci, 'Client disconnected');
      exit;
    End;
  End;
  log(0, 'Lost unknown connection');
End;

Procedure TMQTTBroker.OnReceive(aSocket: TLSocket);
Const
  BuffSize = 2048;
Var
  index: integer;
  Buffer: Array[0..2047] Of byte;
  BytesCount, i: Integer;
  aIndex: SizeInt;
  aPacket: TMQTTPacket;
Begin
  index := SocketToindex(aSocket);
  If index < 0 Then Begin // Wir kennen den Socket nicht -> Platt machen
    aSocket.Disconnect(true);
    exit;
  End;
  // Wir müssen immer lesen, was der Socket gelesen hat, sonst wirds ganz unschön..
  BytesCount := aSocket.Get(Buffer[0], BuffSize);
  While BytesCount <> 0 Do Begin
    // TODO: Das könnte man Performanter implementieren ...
    // Die Daten an den Aktuellen Lese Speicher anfügen
    aIndex := length(fClients[index].RecvBytes);
    setlength(fClients[index].RecvBytes, aIndex + BytesCount);
    For i := 0 To BytesCount - 1 Do Begin
      fClients[index].RecvBytes[aIndex + i] := Buffer[i];
    End;
    BytesCount := aSocket.Get(Buffer[0], BuffSize);
    // Nach jedem Lesen Versuchen wir ein MQTT Packet zu empfangen.
    If TryExtractAPacket(index, aPacket) Then Begin
      If fClients[high(fClients)].MQTTConnected Then Begin
        HandlePacket(index, aPacket);
      End
      Else Begin
        HandleConnectPacket(index, aPacket);
      End;
    End;
  End;
End;

Procedure TMQTTBroker.OnError(Const msg: String; aSocket: TLSocket);
Var
  index: integer;
Begin
  index := SocketToindex(aSocket);
  If index >= 0 Then Begin
    Log(fClients[index].ID, 'Error: ' + msg);
  End
  Else Begin
    Log(0, 'Error: ' + msg);
  End;
  // OnDisconnect(aSocket); -- This causes a endlessloop we have to trust L-net to call the "Disconnect" event !
End;

Function TMQTTBroker.SocketToindex(Const aSocket: TLSocket): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fClients) Do Begin
    If fClients[i].Socket = aSocket Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function TMQTTBroker.TryExtractaPacket(Clientindex: Integer; Out
  aPacket: TMQTTPacket): Boolean;
Var
  RemainingLength, i, Multiplier, aPayloadStartIndex: Integer;
  EncodedByte: uint8;
Begin
  result := false;
  If Clientindex > high(fClients) Then exit; // Der Client wurde mittlerweile frei gegeben ..
  // Der Fixed Header hat auf jeden Fall 2 Byte
  If length(fClients[Clientindex].RecvBytes) < 2 Then exit;
  // Decoding the Remaining Length
  aPayloadStartIndex := 1;
  Multiplier := 1;
  RemainingLength := 0;
  Repeat
    If aPayloadStartIndex > high(fClients[Clientindex].RecvBytes) Then exit; // Die Länge der Remaining Bytes wurde noch nicht Vollständig eingelesen -> Raus
    EncodedByte := fClients[Clientindex].RecvBytes[aPayloadStartIndex];
    RemainingLength := RemainingLength + (EncodedByte And $7F) * Multiplier;
    Multiplier := Multiplier * 128;
    If (Multiplier > 128 * 128 * 128) Then Begin
      // TODO: ggf. könnte man hier auch einfach nur den ClientIndex Platt machen und alles Resetten ?
      Raise exception.create('Error invalid multiplier.');
    End;
    inc(aPayloadStartIndex); // Switch to next Byte
  Until ((EncodedByte And $80) = 0);
  // Nachricht vollständig empfangen ?
  If length(fClients[Clientindex].RecvBytes) < RemainingLength + 2 Then exit;
  // Nachricht ist Vollständig -> Auslesen und den Empfangspuffer entsprechend verkürzen
  result := true;
  aPacket.ControlPacketType := (fClients[Clientindex].RecvBytes[0] Shr 4) And $F;
  aPacket.Flags := (fClients[Clientindex].RecvBytes[0]) And $F;
  setlength(aPacket.Payload, RemainingLength);
  For i := 0 To RemainingLength - 1 Do Begin
    aPacket.Payload[i] := fClients[Clientindex].RecvBytes[i + aPayloadStartIndex];
  End;
  For i := RemainingLength + aPayloadStartIndex To length(fClients[Clientindex].RecvBytes) - 1 Do Begin
    fClients[Clientindex].RecvBytes[i - RemainingLength - aPayloadStartIndex] := fClients[Clientindex].RecvBytes[i];
  End;
  setlength(fClients[Clientindex].RecvBytes, length(fClients[Clientindex].RecvBytes) - aPayloadStartIndex - RemainingLength);
End;

Procedure TMQTTBroker.HandleConnectPacket(Clientindex: Integer;
  Const aPacket: TMQTTPacket);

  Procedure SendCONN_ACK(ReturnCode: uint8); // Antwort auf das Connect Packet
  Var
    a: Array Of byte;
  Begin
    a := Nil;
    setlength(a, 4);
    a[0] := CPT_CONNACK Shl 4;
    a[1] := 2; // Remaining Length = 2
    a[2] := 0; // Connect Acknowledge Flags: SP = 0
    a[3] := ReturnCode; // Connect Return Code: = 0
    fClients[Clientindex].Socket.Send(a[0], length(a));
  End;

Const
  ConnAccepted = 0;
  ConnUnacceptableProtocolVersion = 1;
  ConnIdentifierRejected = 2;
  ConnServerUnavailable = 3;
  ConnBadUsernameOrPassword = 4;
  ConnNotauthorized = 5;

Var
  KeepAlive: UInt16;
  ConnectFlags: uint8;
Begin
  (*
   * Der Aufbau des Connect Packetes ist vorgeschrieben als:
   * <Protocol Name><Protocol Level><Connect Flags><Keep Alive>
   *)
  //       | -- Payload beginnt mit [0] !
  // 10 3E 00 04 4D 51 54 54 04 26 02 EE 00 0A 77 61 74 65 72 6D 65 74 65 72 00 15 77 61 74 65 72 6D 65 74 65 72 2F 63 6F 6E 6E 65 63 74 69 6F 6E 00 0F 63 6F 6E 6E 65 63 74 69 6F 6E 20 6C 6F 73 74
  // +++++ =Header
  //       +++++++++++++++++ =<4>MQTT
  //                         ++ Protokol Level =<4>
  //                            ++ Connect Flags = (Will Retain, Will Flag, Clean Session)
  //                               +++++ =Keep Alive
  If (Not aPacket.ControlPacketType = CPT_CONNECT) Or
    (length(aPacket.Payload) < 10) Then Begin // Die Länge bis zu den Connect Flags schon mal vor Prüfen
    SendCONN_ACK(ConnNotauthorized);
    log(fClients[Clientindex].ID, 'Login rejected');
    exit;
  End;
  (*
   * Prüfung <Protocol Name> => 00 04 "MQTT"
   *)
  If (aPacket.Payload[0] <> 0) Or
    (aPacket.Payload[1] <> 4) Or
    (aPacket.Payload[2] <> ord('M')) Or
    (aPacket.Payload[3] <> ord('Q')) Or
    (aPacket.Payload[4] <> ord('T')) Or
    (aPacket.Payload[5] <> ord('T')) Then Begin
    SendCONN_ACK(ConnUnacceptableProtocolVersion);
    log(fClients[Clientindex].ID, 'Login rejected');
    exit;
  End;
  // Prüfen Protokol Level = 4
  If (aPacket.Payload[6] <> 4) Then Begin
    SendCONN_ACK(ConnUnacceptableProtocolVersion);
    log(fClients[Clientindex].ID, 'Login rejected');
    exit;
  End;
  // Prüfen ob das Reserved Flag = 0
  ConnectFlags := aPacket.Payload[7];
  If (ConnectFlags And $01 <> 0) Then Begin
    SendCONN_ACK(ConnNotauthorized);
    log(fClients[Clientindex].ID, 'Login rejected');
    exit;
  End;
  KeepAlive := (aPacket.Payload[8] Shl 8) Or (aPacket.Payload[9]); // in S
  // 3.1.3 Payload
  // The payload of the CONNECT Packet contains one or more length-prefixed fields, whose presence is determined by the flags in the variable header. These fields, if present, MUST appear in the order Client Identifier, Will Topic, Will Message, User Name, Password [MQTT-3.1.3-1].
  // TODO: die Payload könnte man auch auswerten ...
  SendCONN_ACK(ConnAccepted);
  fClients[high(fClients)].MQTTConnected := true;
  If assigned(OnAcceptMQTTClient) Then OnAcceptMQTTClient(self, fClients[high(fClients)].ID);
End;

Procedure TMQTTBroker.HandleSubscribePacket(Clientindex: Integer;
  Const aPacket: TMQTTPacket);
Var
  IdentifierLen, PackageIdentifier: uint16;

  Identifier: String;
  i: Integer;
  a: Array Of Byte;
  q: TReturn;
Begin
  //                            +++++= Package Identifier
  // Unknown package: 8 2 [31]: 00 0A 00 1A 77 61 74 65 72 6D 65 74 65 72 2F 63 74 72 6C 2F 66 6C 6F 77 5F 73 74 61 72 74 00
  // Unknown package: 8 2 [33]: 00 0B 00 1C 77 61 74 65 72 6D 65 74 65 72 2F 63 74 72 6C 2F 73 65 74 5F 70 72 65 76 61 6C 75 65 00
  PackageIdentifier := aPacket.Payload[0] Shl 8 Or aPacket.Payload[1];
  IdentifierLen := aPacket.Payload[2] Shl 8 Or aPacket.Payload[3];
  Identifier := '';
  For i := 0 To IdentifierLen - 1 Do Begin
    Identifier := Identifier + chr(aPacket.Payload[4 + i]);
  End;
  //  log(format('Subsribe: %0.4X : %s', [PackageIdentifier, Identifier]));
  // Subsribe: 0011 : watermeter/ctrl/flow_start
  // Subsribe: 0012 : watermeter/ctrl/set_prevalue

  // Da gibt es ganz viel : http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Topic_wildcards

  // das "/" ist ein Topic level separator -> quasi wie ein Verzeichnis Separator
  // das "#" ist ein Wildcard -> damit kann dann quasi gefilter werden.
  // das "+" ist ein Wildcard -> damit kann dann quasi gefilter werden aber nur 1 Level, was auch immer das heist ...

  If Not assigned(OnSubscribeRequest) Then Begin
    Raise exception.create('Error, a client want to subsribe, but no callback is defined.');
  End;
  q := OnSubscribeRequest(self, fClients[Clientindex].ID, Identifier);

  // --> Verlangte Antwort SUBACK
  a := Nil;
  setlength(a, 5);
  a[0] := CPT_SUBACK Shl 4;
  a[1] := 3; // Länge der Payload
  a[2] := (PackageIdentifier Shr 8) And $FF; // Repeat Package identifier
  a[3] := PackageIdentifier And $FF; // Repeat Package identifier
  // Add Result from Application
  Case q Of
    rQoS0: a[4] := 0;
    rQoS1: a[4] := 1;
    rQoS2: a[4] := 2;
    rFailure: a[4] := $80;
  End;
  fClients[Clientindex].Socket.Send(a[0], length(a));
End;

Procedure TMQTTBroker.HandlePublishPacket(Clientindex: Integer;
  Const aPacket: TMQTTPacket);
Var
  PackageIdentifier, NameLen: uint16;
  QoSLevel: byte;
  Retain, DUP: Boolean;
  Payload, Name: String;
  i: Integer;
  a: Array Of Byte;
Begin
  a := Nil;
  dup := (aPacket.Flags And $08 = $08); // Wenn True, dann wurde das Packet wiederhohlt gesendet
  QoSLevel := (aPacket.Flags And $06) Shr 1; // 0, 1, 2 = valid, 3 = Fehler -> Abbruch der Verbindung !
  Retain := (aPacket.Flags And $01 = $01); // if true, than the server should store this value !
  // Payload is: Topic Name, Packet Identifier
  NameLen := (aPacket.Payload[0] Shl 8) Or aPacket.Payload[1];
  Name := '';
  For i := 0 To NameLen - 1 Do Begin
    Name := name + chr(aPacket.Payload[2 + i]);
  End;
  //  Unknown package: 3 2 [31]: 00 11 77 61 74 65 72 6D 65 74 65 72 2F 73 74 61 74 75 73 00 21 54 61 6B 65 20 49 6D 61 67 65
  PackageIdentifier := 0;
  Payload := '';
  If QoSLevel In [1, 2] Then Begin
    PackageIdentifier := (aPacket.Payload[NameLen + 2] Shl 8) Or aPacket.Payload[NameLen + 3];
    For i := NameLen + 4 To high(aPacket.Payload) Do Begin
      Payload := Payload + chr(aPacket.Payload[i]);
    End;
  End;
  If Not assigned(OnPublishRequest) Then Begin
    Raise exception.create('Error, a client published a value, but no callback is defined.');
  End;
  OnPublishRequest(self, fClients[Clientindex].ID, Name, Payload, DUP, Retain);
  // Berechnen der Antwort
  Case QoSLevel Of
    0: Begin
        // Nothing, QoS0 will not be replied to.
      End;
    1: Begin // PUBACK
        setlength(a, 4);
        a[0] := CPT_PUBACK Shl 4;
        a[1] := 2;
        a[2] := (PackageIdentifier Shr 8) And $FF;
        a[3] := (PackageIdentifier) And $FF;
        fClients[Clientindex].Socket.Send(a[0], length(a));
      End;
    2: Begin // PUBREC
        log(fClients[Clientindex].ID, 'Nicht sicher ob das so richtig ist ..');
        setlength(a, 4);
        a[0] := CPT_PUBREC Shl 4;
        a[1] := 2;
        a[2] := (PackageIdentifier Shr 8) And $FF;
        a[3] := (PackageIdentifier) And $FF;
        fClients[Clientindex].Socket.Send(a[0], length(a));
      End;
  End;
End;

Procedure TMQTTBroker.HandlePingPacket(Clientindex: Integer;
  Const aPacket: TMQTTPacket);
Var
  a: Array Of byte;
Begin
  // 2023.10.13 19:06:10 Unknown package: 12 0 [0]:
  a := Nil;
  setlength(a, 2);
  a[0] := CPT_PINGRESP Shl 4;
  a[1] := 0;
  fClients[Clientindex].Socket.Send(a[0], length(a));
  If Assigned(OnPingEvent) Then Begin
    OnPingEvent(self, fClients[high(fClients)].ID);
  End;
End;

Procedure TMQTTBroker.HandlePacket(Clientindex: Integer;
  Const aPacket: TMQTTPacket);
Var
  s: String;
  i: Integer;
Begin
  Case aPacket.ControlPacketType Of
    CPT_CONNECT: Begin
        // Ein erneutes Connect Packet -> Fehler Abbruch und Raus
        Log(fClients[Clientindex].ID, 'Drop client due to invalid second connect request');
        fClients[Clientindex].Socket.Disconnect(true);
        exit;
      End;
    //    CPT_CONNACK: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    CPT_PUBLISH: HandlePublishPacket(Clientindex, aPacket);
    //    CPT_PUBACK: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    //    CPT_PUBREC: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    //    CPT_PUBREL: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    //    CPT_PUBCOMP:
    CPT_SUBSCRIBE: HandleSubscribePacket(Clientindex, aPacket);
    //    CPT_SUBACK: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    //    CPT_UNSUBSCRIBE:
    //    CPT_UNSUBACK: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    CPT_PINGREQ: HandlePingPacket(Clientindex, aPacket);
    //    CPT_PINGRESP: -- Das Gibts auf dem Server gar nicht ist ja eine Antwort
    //    CPT_DISCONNECT:
    //  CPT_RESERVED = 15;
  Else Begin
      // Alles was wir nicht kennen wird raus geloggt
      s := '';
      For i := 0 To high(aPacket.Payload) Do Begin
        s := s + format(' %0.2X', [aPacket.Payload[i]]);
      End;
      Log(fClients[Clientindex].ID, 'Unknown package: ' + format('%d %d [%d]:' + s, [aPacket.ControlPacketType, aPacket.Flags, length(aPacket.Payload)]));
    End;
  End;
End;

Procedure TMQTTBroker.Log(ClientID: integer; Const aValue: String);
Begin
  If assigned(OnLog) Then Begin
    OnLog(self, ClientID, aValue);
  End;
End;

End.

