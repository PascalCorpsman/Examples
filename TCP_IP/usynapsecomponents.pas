(******************************************************************************)
(* uSynapseComponents.pas                                          27.12.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Wrapper class, that makes Synapse use like lNet              *)
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
(* Known Issues: -Reuse Feature does not work like expected                   *)
(*               -OnCanSend not implemented yet                               *)
(*               -when using SSL, the client stalls until the server sends its*)
(*                first byte :/                                               *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit uSynapseComponents;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, blcksock, synsock;

Type

  TSocketState = (
    //    ssServerSocket, ssBlocking, ssReuseAddress, ssCanSend,
    //                   ssCanReceive, ssSSLActive,
    ssNoDelay
    );

  TMode = (mServer, mClient, mUndefined);

  TAcceptThread = Class;
  TClientThread = Class;
  TSynapseComponent = Class;

  // Externe Repräsentation einer Verbindung

  { TSynapseConnection }

  TSynapseConnection = Class
  private
    FClientThreadID: Int64; // Referenz auf den ClientThread
    FOwner: TSynapseComponent; // Referenz auf Owner
    FSocket: TTCPBlockSocket; // Referenz auf den Socket des ClientThread
  public
    Property ID: int64 read FClientThreadID;

    Constructor Create(AOwner: TSynapseComponent; aSocket: TTCPBlockSocket; aFClientThreadID: Int64); virtual;
    Destructor Destroy(); override;

    Function Get(Data: TMemory; DataSize: integer): integer;
    Function GetMessage(Out DataString: String): integer;

    Function Send(Const Data: TMemory; DataSize: Integer): integer;
    Function SendMessage(Const msg: String): Integer;

    Function SetState(Const aState: TSocketState; Const TurnOn: Boolean = True): Boolean;
  End;

  // Connection Event
  TSocketEvent = Procedure(Connection: TSynapseConnection) Of Object;

  // Error Event, connection can be Nil
  TSocketErrorEvent = Procedure(Const ErrorMsg: String; Connection: TSynapseConnection) Of Object;

  { TSynapseComponent }

  TSynapseComponent = Class
  private
    // All needed by both, server and client
    fMode: TMode;
    fPort: Integer;
    FClients: Array Of TClientThread;
    fDeadClients: Array Of TClientThread;

    // All needed to be a server
    FNextID: Int64;
    fAcceptThread: TAcceptThread;
    FTimeout: Integer;
    FReuseAddress: Boolean;
    FIterator: TSynapseConnection;

    // All needed to be a client
    fIP: String; // IP-Address to which the client is connected to

    Procedure SetReuseAddress(AValue: Boolean);

    Procedure InternalAddClient(Const Client: TTCPBlockSocket);
    Procedure InternalRemoveClient(Client: TSynapseConnection);
  public

    OnAccept: TSocketEvent;
    OnConnect: TSocketEvent;
    OnDisconnect: TSocketEvent;
    OnError: TSocketErrorEvent;
    OnReceive: TSocketEvent;
    //    OnCanSend: TSocketEvent;

    UseSSL: Boolean; // Set to true to use encrypted connections (need to be done before Listen / Connect)
    SSLCertFile: String; // server.crt
    SSLKeyFile: String; // server.key
    SSLCAFile: String; // ca.crt (only needed if SSLVerify is true
    SSLVerify: Boolean; // important for clients set to true

    Property Iterator: TSynapseConnection read FIterator;

    Constructor Create;
    Destructor Destroy; override;
    //
    Property Timeout: Integer read FTimeout write FTimeout;
    Property ReuseAddress: Boolean read FReuseAddress write SetReuseAddress;

    // Client Interface
    Function Connect(AIP: String; APort: Integer): Boolean;

    Function Send(Const Data: TMemory; DataSize: Integer; Connection: TSynapseConnection = Nil): integer;
    Function SendMessage(Const msg: String; Connection: TSynapseConnection = Nil): Integer;

    // Server Interface
    Function Listen(APort: Integer): Boolean;
    Procedure IterReset;
    Function IterNext: Boolean;

    // Server / Client API
    Function Connected(): Boolean;
    Procedure Disconnect(force: Boolean);
  End;

  { TAcceptThread }

  TAcceptThread = Class(TThread)
  private
    // Für Execute
    FListenSocket: TTCPBlockSocket;
    FOwner: TSynapseComponent;

    //Zur Übergabe an den Owner
    fNewClient: TTCPBlockSocket;
    Procedure HandNewCLientOverToOwner;

  protected
    Procedure Execute; override;
  End;

  { TClientThread }

  TClientThread = Class(TThread)
  private
    fID: Int64;
    FSocket: TTCPBlockSocket;
    FConnection: TSynapseConnection;
    FOwner: TSynapseComponent;
    FWasConnected: Boolean; // True, if Authentification was successfull,
    Procedure AcceptClient();
    Procedure ConnectClient();
    Procedure ErrorClient();
    Procedure SSLErrorClient();
    Procedure RemoveClient();
    Procedure ReadData();
  protected
    Procedure Execute; override;
  End;

Implementation

Uses math, ssl_openssl3;

Const
  NoSocket = -1;
  PollTimeoutms = 10;

  { TAcceptThread }

Procedure TAcceptThread.HandNewCLientOverToOwner;
Begin
  FOwner.InternalAddClient(fNewClient);
  fNewClient := Nil;
End;

Procedure TAcceptThread.Execute;
Var
  aSock: TSocket;
Begin
  // Setup
  FListenSocket := TTCPBlockSocket.Create;
  FListenSocket.SetLinger(True, FOwner.FTimeout);
  FListenSocket.SetTimeout(PollTimeoutms);
  FListenSocket.Bind('0.0.0.0', IntToStr(FOwner.fPort));
  FListenSocket.EnableReuse(FOwner.FReuseAddress);
  FListenSocket.Listen;
  // Execute
  While Not Terminated Do Begin
    aSock := FListenSocket.Accept;
    If aSock <> NoSocket Then Begin
      // Neues TTCPBlockSocket für den Client
      fNewClient := TTCPBlockSocket.Create;
      fNewClient.Socket := aSock;
      Synchronize(@HandNewCLientOverToOwner);
    End;
  End;
  // Teardown
  FListenSocket.CloseSocket;
  FListenSocket.Free;
  FListenSocket := Nil;
End;

{ TClientThread }

Procedure TClientThread.AcceptClient;
Begin
  If assigned(FOwner) And assigned(FOwner.OnAccept) Then Begin
    FOwner.OnAccept(FConnection);
  End;
End;

Procedure TClientThread.ConnectClient;
Begin
  If assigned(FOwner) And assigned(FOwner.OnConnect) Then Begin
    FOwner.OnConnect(FConnection);
  End;
End;

Procedure TClientThread.ErrorClient;
Begin
  If assigned(FOwner) And assigned(FOwner.OnError) Then Begin
    FOwner.OnError(FSocket.LastErrorDesc, FConnection);
  End;
End;

Procedure TClientThread.SSLErrorClient();
Begin
  If assigned(FOwner) And assigned(FOwner.OnError) Then Begin
    FOwner.OnError(format('Error[%d]: %s', [FSocket.ssl.LastError, FSocket.ssl.LastErrorDesc]), FConnection);
  End;
End;

Procedure TClientThread.RemoveClient;
Begin
  If assigned(FOwner) Then Begin
    FOwner.InternalRemoveClient(FConnection);
  End;
End;

Procedure TClientThread.ReadData;
Begin
  If assigned(FOwner) And assigned(FOwner.OnReceive) Then Begin
    FOwner.OnReceive(FConnection);
  End;
End;

Procedure TClientThread.Execute;
Var
  i: Integer;
Begin
  // Setup:
  //  Nichts zu tun der Erzeuger dieses Threads hat
  //  FOwner, FSocket bereits korrekt initialisiert ;)
  FConnection := TSynapseConnection.Create(FOwner, FSocket, fID);
  fWasConnected := false;
  // Optional: SSL-Handschake
  Case FOwner.fMode Of
    mServer: Begin
        If FOwner.UseSSL Then Begin
          FSocket.SSL.CertificateFile := FOwner.SSLCertFile;
          FSocket.SSL.PrivateKeyFile := FOwner.SSLKeyFile;
          // Die SSL Verbindung aufbauen
          If Not FSocket.SSLAcceptConnection Then Begin
            // Wenns nicht geklappt hat werfen wie den Client wieder raus befor es überhaupt erst los gehen kann
            Synchronize(@RemoveClient);
            exit;
          End;
        End;
      End;
    mClient: Begin
        If FOwner.SSLVerify Then Begin
          FSocket.SSL.CertCAFile := FOwner.SSLCAFile; // optional
          FSocket.SSLDoConnect(); // Der Client will verbinden
        End;
      End;
  End;

  If FSocket.SSL.LastError <> 0 Then Begin
    Synchronize(@SSLErrorClient);
    Terminate;
    exit;
  End;
  If FSocket.LastError <> 0 Then Begin
    Synchronize(@ErrorClient);
    Terminate;
    exit;
  End;

  fWasConnected := true;
  // Der Server hat einen neuen Client akzeptiert
  If FOwner.fMode = mServer Then Begin
    If FOwner.UseSSL Then Begin
      // Ohne dieses Byte hängt der Client nach dem verbinden bis der Server endlich etwas sendet :(
      // Aber mit, empfängt jeder andere client (z.B. L-Net) der Funktioniert ein Byte mit dem er nichts anfangen kann :/
      //i := 0;
      //FSocket.SendBuffer(@i, 1);
    End;
    Synchronize(@AcceptClient);
  End;
  // Wir sind erfolgreich mit dem Server verbunden
  If FOwner.fMode = mClient Then Begin
    Synchronize(@ConnectClient);
  End;

  // Execute
  While Not Terminated Do Begin
    If (FSocket <> Nil) And FSocket.CanRead(1) Then Begin
      If (FSocket = Nil) Or (FSocket.PeekBuffer(@i, 1) = 0) Then Begin
        Synchronize(@RemoveClient);
      End
      Else Begin
        Synchronize(@ReadData);
      End;
    End;
    If assigned(FSocket) And (FSocket.LastError <> 0) Then Begin
      Synchronize(@ErrorClient);
      Terminate;
    End;
  End;
  // Teardown
  If assigned(FOwner) Then Begin
    Synchronize(@RemoveClient);
  End;
End;

{ TSynapseComponent }

Procedure TSynapseComponent.InternalAddClient(Const Client: TTCPBlockSocket);
Var
  newClientThread: TClientThread;
Begin
  newClientThread := TClientThread.Create(true);
  newClientThread.FOwner := self;
  newClientThread.FreeOnTerminate := false;
  newClientThread.FSocket := Client;
  newClientThread.fID := FNextID;
  inc(FNextID);
  newClientThread.Start;
  setlength(FClients, high(FClients) + 2);
  FClients[high(FClients)] := newClientThread;
End;

Procedure TSynapseComponent.InternalRemoveClient(Client: TSynapseConnection);
Var
  i, j: Integer;
  ClientThreadToBeFreed: TClientThread;
  SocketToBeFreed: TTCPBlockSocket;
Begin
  If Not assigned(Client) Then exit; // Diese Verbindung wurde bereits freigegeben
  If Not Assigned(Client.FOwner) Then exit; // Diese Verbindung wurde bereits freigegeben
  Client.FOwner := Nil;
  Client.FSocket := Nil;
  // Wir Suchen den Client, und wenn es ihn noch gibt, dann geben wir ihn frei
  For i := high(FClients) Downto 0 Do Begin
    If FClients[i].fID = Client.ID Then Begin
      If (FClients[i].fWasConnected) And assigned(OnDisconnect) Then Begin
        OnDisconnect(client);
      End;
      client.free;
      // 1. dem Thread mitteilen, dass Schluss ist, aber ohne das Internal Remove erneut zu triggern
      ClientThreadToBeFreed := FClients[i];
      SocketToBeFreed := ClientThreadToBeFreed.FSocket;
      ClientThreadToBeFreed.FOwner := Nil; // Dem Thread die Refence weg nehmen -> Keine endless Austragung ;)
      ClientThreadToBeFreed.FConnection := Nil;
      ClientThreadToBeFreed.FSocket := Nil;
      ClientThreadToBeFreed.Terminate;
      For j := i To high(FClients) - 1 Do Begin
        FClients[j] := FClients[j + 1];
      End;
      setlength(FClients, high(FClients));
      SocketToBeFreed.CloseSocket;
      SocketToBeFreed.Free;
      // Den Thread frei geben
      setlength(fDeadClients, high(fDeadClients) + 2);
      fDeadClients[high(fDeadClients)] := ClientThreadToBeFreed;
      break;
    End;
  End;
End;

Procedure TSynapseComponent.SetReuseAddress(AValue: Boolean);
Begin
  If FReuseAddress = AValue Then Exit;
  FReuseAddress := AValue;
  // TODO: Prüfen ob wir schon verbunden sind, wenn ja ne exception werfen..
End;

Constructor TSynapseComponent.Create;
Begin
  Inherited Create;
  fMode := mUndefined;
  FTimeout := 5000;
  fAcceptThread := Nil;
  FClients := Nil;
  FNextID := 1;
  FReuseAddress := false;
  OnAccept := Nil;
  OnConnect := Nil;
  OnDisconnect := Nil;
  OnError := Nil;
  fDeadClients := Nil;
End;

Destructor TSynapseComponent.Destroy;
Begin
  // Threads beenden und Clients freigeben
  Disconnect(true);
End;

Function TSynapseComponent.Listen(APort: Integer): Boolean;
Begin
  result := false;
  Disconnect(true);

  If UseSSL Then Begin
    If (SSLCertFile = '') Or (SSLKeyFile = '') Or
      (Not FileExists(SSLCertFile)) Or
      (Not FileExists(SSLKeyFile)) Then
      Raise Exception.Create('SSL active, but invalid certificate / key.');
  End;
  FNextID := 1;
  fMode := mServer;
  fPort := APort;
  fAcceptThread := TAcceptThread.Create(true);
  fAcceptThread.FOwner := self;
  fAcceptThread.FreeOnTerminate := false;
  fAcceptThread.Start;

  result := true;
End;

Procedure TSynapseComponent.IterReset;
Begin
  fIterator := Nil;
  If Connected() Then fIterator := FClients[0].FConnection;
End;

Function TSynapseComponent.IterNext: Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(FClients) - 1 Do Begin
    If FClients[i].FConnection = FIterator Then Begin
      FIterator := FClients[i + 1].FConnection;
      exit;
    End;
  End;
  FIterator := Nil;
End;

Function TSynapseComponent.Connect(AIP: String; APort: Integer): Boolean;
Var
  aSocket: TTCPBlockSocket;
Begin
  result := false;
  Disconnect(true);
  If SSLVerify Then Begin
    If (SSLCAFile = '') Or
      (Not FileExists(SSLCAFile)) Then Begin
      Raise Exception.Create('SSL Verify active, but invalid certificate.');
    End;
  End;
  fMode := mClient;
  fIP := AIP;
  fPort := APort;
  aSocket := TTCPBlockSocket.Create;
  aSocket.SetLinger(True, FTimeout);
  aSocket.Connect(fIP, inttostr(fPort));
  If aSocket.LastError <> 0 Then Begin
    If assigned(OnError) Then Begin
      OnError(aSocket.LastErrorDesc, Nil);
    End;
    aSocket.free;
    exit;
  End;
  InternalAddClient(aSocket);
  Result := true;
End;

Function TSynapseComponent.Connected: Boolean;
Begin
  result := false;
  Case fMode Of
    mClient: result := assigned(FClients);
    mServer: result := assigned(fAcceptThread);
  End;
End;

Procedure TSynapseComponent.Disconnect(force: Boolean);
Var
  i: Integer;
Begin
  fMode := mUndefined; // Das, macht das Connected platt ;)
  // Shutdown Accept Thread in Server mode..
  If assigned(fAcceptThread) Then Begin
    fAcceptThread.Terminate;
    While Not fAcceptThread.Finished Do Begin
      sleep(1);
    End;
    fAcceptThread.free;
  End;
  fAcceptThread := Nil;
  // 1. allen Threads sagen Sie sollen Schluss machen sollen
  For i := high(FClients) Downto 0 Do Begin
    InternalRemoveClient(FClients[i].FConnection);
  End;
  setlength(FClients, 0);
  For i := 0 To high(fDeadClients) Do Begin
    fDeadClients[i].Free;
  End;
  setlength(fDeadClients, 0);
End;

Function TSynapseComponent.Send(Const Data: TMemory; DataSize: Integer;
  Connection: TSynapseConnection): integer;
Begin
  result := 0;
  If Not Connected() Then exit;
  Case fMode Of
    mClient: Begin
        result := FClients[0].FConnection.Send(data, DataSize);
      End;
    mServer: Begin
        If assigned(Connection) Then Begin
          result := Connection.Send(data, DataSize);
        End
        Else Begin
          // Broadcast ?
        End;
      End;
  End;
End;

Function TSynapseComponent.SendMessage(Const msg: String;
  Connection: TSynapseConnection): Integer;
Begin
  result := 0;
  If msg <> '' Then Begin
    result := send(@msg[1], length(msg), Connection);
  End;
End;

{ TSynapseConnection }

Constructor TSynapseConnection.Create(AOwner: TSynapseComponent;
  aSocket: TTCPBlockSocket; aFClientThreadID: Int64);
Begin
  Inherited Create;
  FClientThreadID := aFClientThreadID;
  FOwner := AOwner;
  FSocket := aSocket;
End;

Destructor TSynapseConnection.Destroy;
Begin
  If assigned(FOwner) Then Begin
    FOwner.InternalRemoveClient(self);
    FOwner := Nil;
  End;
End;

Function TSynapseConnection.Get(Data: TMemory; DataSize: integer): integer;
Var
  WaitingData: integer;
Begin
  result := 0;
  If Not assigned(FSocket) Then exit; // Irgendwas ist falsch, wir haben keinen Gültigen Socket mehr ..
  // TODO: sollte senden und empfangen nicht über den ClientThread laufen ?
  WaitingData := FSocket.WaitingData;
  If WaitingData <> 0 Then Begin
    result := FSocket.RecvBuffer(data, min(WaitingData, DataSize));
  End;
End;

Function TSynapseConnection.GetMessage(Out DataString: String): integer;
Var
  WaitingData: Integer;
Begin
  Result := 0;
  If Not assigned(FSocket) Then exit;
  // TODO: sollte senden und empfangen nicht über den ClientThread laufen ?
  WaitingData := FSocket.WaitingData;
  DataString := '';
  If WaitingData <> 0 Then Begin
    setlength(DataString, WaitingData);
    result := FSocket.RecvBuffer(@DataString[1], WaitingData);
    If WaitingData <> result Then // Why ever in SSL mode waiting data is sometimes more then result, so trim the final result if needed
      setlength(DataString, result);
  End;
End;

Function TSynapseConnection.Send(Const Data: TMemory; DataSize: Integer
  ): integer;
Begin
  result := 0;
  If Not Assigned(FSocket) Then exit;
  // TODO: sollte senden und empfangen nicht über den ClientThread laufen ?
  result := FSocket.SendBuffer(data, DataSize);
  If result <> DataSize Then Begin
    Raise exception.create('Fehler, CanSend noch nicht implementiert, wäre nun aber nötig gewesen ..');
  End;
End;

Function TSynapseConnection.SendMessage(Const msg: String): Integer;
Begin
  result := 0;
  If msg <> '' Then Begin
    result := send(@msg[1], length(msg));
  End;
End;

Function TSynapseConnection.SetState(Const aState: TSocketState;
  Const TurnOn: Boolean): Boolean;
Var
  NoDelay: Boolean;
Begin
  result := false;
  If Not assigned(FSocket) Then exit;
  Case aState Of
    ssNoDelay: Begin
        NoDelay := TurnOn;
        setsockopt(FSocket.Socket, IPPROTO_TCP, TCP_NODELAY, @NoDelay, SizeOf(NoDelay));
      End;
  End;
End;

End.

