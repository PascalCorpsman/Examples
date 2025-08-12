(******************************************************************************)
(* uRest.pas                                                       11.08.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a REST server  and client                  *)
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
(* History     : 0.01 - Initial version (server)                              *)
(*                                                                            *)
(******************************************************************************)

Unit urest;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, lnet, uJSON;

Type

  TOnReceiveHTTPDocument = Procedure(Sender: TObject; Const Header: TStrings; Body: TStrings) Of Object;

  (*
   * Theoretisch könnte man die HTTP Header mit durchreichen, das gäbe dann die Möglichkeit "Cookies" zu setzen
   * -> Dann wäre das ganze nicht Stateless, aber die bisherige Anforderung hat dies nicht benötigt.
   *)
  TGetHandler = Function(Sender: TObject; Const aPath: String): TJSONobj Of Object;
  TPostHandler = Function(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): Boolean Of Object;

  THTTPReceivingState = (
    rsHeader,
    rsBody
    );

  { THTTPReceiver }

  THTTPReceiver = Class // Simple HTTP Decoder Class
  private
    fHTTPReceivingState: THTTPReceivingState;
    fHeader: TStringList;
    fBody: TStringList; // TODO: Das wäre als TMemoryStream wahrscheinlich Sinnvoller !
    fBodyByteCount: integer;
    fBodyBuffer: String;
    fActualReceivedLine, fLastReceivedLine: String;
    Procedure Reset;
    Procedure EvaluateReceivedHeader;
  public
    UserData: Pointer;
    OnReceiveHTTPDocument: TOnReceiveHTTPDocument;

    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure ReceiveByte(aData: Byte);
  End;

  (*
   * Daten, welche an den Socket gebunden werden
   *)
  TUserData = Record
    HTTPReceiver: THTTPReceiver; // Aktueller Empfangspuffer
    Socket: TLSocket; // Pointer auf den Besitzer Socket
    // Sende / Out Buffer
    OutBuffer: TMemoryStream;
    OutBuffPos: Int64;
  End;

  PUserData = ^TUserData;

  TGetPath = Record
    Path: String;
    Handler: TGetHandler;
  End;

  TPostPath = Record
    Path: String;
    Handler: TPostHandler;
  End;

  { TRestServer }

  TRestServer = Class
  private
    fTCPConnection: TLTcp;
    (*
     * Captured Events
     *)
    FOnAccept_Captured: TLSocketEvent;
    FOnDisconnect_Captured: TLSocketEvent;
    FOnError_Captured: TLSocketErrorEvent;

    fGetPaths: Array Of TGetPath;
    fPostPaths: Array Of TPostPath;

    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Procedure OnCanSend(aSocket: TLSocket);
    Procedure OnReceive(aSocket: TLSocket);

    Procedure OnReceiveHTTPDocument(Sender: TObject; Const Header: TStrings; Body: TStrings);

    Procedure RegisterSocket(aSocket: TLSocket);
    Procedure UnRegisterSocket(aSocket: TLSocket);

    Procedure HandleGetCommand(Const Header: TStrings; Const aSocket: TLSocket);
    Procedure HandlePostCommand(Const Header, Body: TStrings; Const aSocket: TLSocket);

    Procedure SendResponceString(Const aData: String; Const aSocket: TLSocket);
  public

    (*
     * OnAccept, OnDisconnect, OnError, dürfen von "außen" genutzt werden (indem sie vorher initialisiert werden)
     * aTCPConnection wird nicht freigegeben !
     *)
    Constructor Create(Const aTCPConnection: TLTcp); virtual;
    Destructor Destroy(); override;

    Procedure CallAction; // Ruft die Callaction der TCP-Komponente auf (nur notwendig, wenn keine LCL variante verwendet wird.)

    Procedure RegisterGetHandler(Const aGetPath: String; Const aHandler: TGetHandler);
    Procedure RegisterPostHandler(Const aPostPath: String; Const aHandler: TPostHandler);

    Function Listen(aPort: integer): Boolean;
    Procedure DisConnect(Const Forced: Boolean);
  End;

  { TRestClient }

  TRestClient = Class
  private
    fTCPConnection: TLTcp;
  public
    Constructor Create(Const aTCPConnection: TLTcp); virtual;
    Destructor Destroy(); override;

    Function Connect(IP: String; Port: integer): Boolean;
    Procedure DisConnect(Const Forced: Boolean);
  End;

Implementation

Uses math;

Const
  CRT = #13#10;

Procedure Nop; // Debug Only ..
Begin

End;

Type
  TUriParameter = Record
    Name: String;
    Value: String;
  End;

  TUriParameters = Array Of TUriParameter;

Function DecodeURLElement(Const S: String): String; // Created by Microsoft Copilot
Var
  I: Integer;
  Hex: String;
  Code: Integer;
Begin
  Result := '';
  I := 1;
  While I <= Length(S) Do Begin
    Case S[I] Of
      '+':
        Result := Result + ' '; // plus becomes space
      '%': Begin
          If (I + 2 <= Length(S)) Then Begin
            Hex := Copy(S, I + 1, 2);
            If TryStrToInt('$' + Hex, Code) Then
              Result := Result + Chr(Code)
            Else
              Result := Result + '%' + Hex; // invalid hex, keep as-is
            Inc(I, 2); // skip hex chars
          End
          Else
            Result := Result + '%'; // lone '%' at end
        End;
    Else
      Result := Result + S[I];
    End;
    Inc(I);
  End;
End;

Function ParseQueryParams(Const Query: String): TUriParameters;
Var
  Pair, Pairs: TStringArray;
  i: Integer;
Begin
  result := Nil;
  pairs := query.Split('&');
  setlength(result, length(Pairs));
  For i := 0 To high(Pairs) Do Begin
    pair := pairs[i].Split('=');
    result[i].Name := DecodeURLElement(pair[0]);
    If length(pair) > 1 Then Begin
      result[i].Value := DecodeURLElement(pair[1]);
    End
    Else Begin
      result[i].Value := '';
    End;
  End;
End;

{ THTTPReceiver }

Constructor THTTPReceiver.Create;
Begin
  Inherited Create;
  OnReceiveHTTPDocument := Nil;
  fHeader := TStringList.create;
  fBody := TStringList.create;
  UserData := Nil;
  Reset;
End;

Destructor THTTPReceiver.Destroy;
Begin
  fHeader.Free;
  fBody.Free;
End;

Procedure THTTPReceiver.Reset;
Begin
  fHTTPReceivingState := rsHeader;
  fHeader.Clear;
  fBody.Clear;
  fLastReceivedLine := '';
  fActualReceivedLine := '';
  fBodyByteCount := 0;
End;

Procedure THTTPReceiver.EvaluateReceivedHeader;
Var
  i: Integer;
  s: String;
Begin
  // Suchen nach dem Content-Length Eintrag, wenn Vorhanden, dann weiter, sonst Fertig.
  fBodyByteCount := 0;
  For i := 0 To fHeader.Count - 1 Do Begin
    If pos('content-length', LowerCase(fHeader[i])) <> 0 Then Begin
      s := copy(fHeader[i], pos(':', fHeader[i]) + 1, length(fHeader[i]));
      fBodyByteCount := strtointdef(trim(s), 0);
      break;
    End;
  End;
  If fBodyByteCount = 0 Then Begin
    If assigned(OnReceiveHTTPDocument) Then Begin
      OnReceiveHTTPDocument(self, fHeader, fBody);
    End;
    reset;
  End
  Else Begin
    fHTTPReceivingState := rsBody;
    fBodyBuffer := '';
  End;
End;

Procedure THTTPReceiver.ReceiveByte(aData: Byte);
Begin
  Case fHTTPReceivingState Of
    rsHeader: Begin
        fActualReceivedLine := fActualReceivedLine + chr(aData);
        If length(fActualReceivedLine) >= 2 Then Begin
          // Gemäß Spec ist der Header Vollständig empfangen, sobald eine "Leerzeile" empfangen wurde
          If (trim(fActualReceivedLine) = '') Then Begin
            EvaluateReceivedHeader;
            exit;
          End;
          // Jede Empfangene Zeile wird dem Header angefügt
          If (fActualReceivedLine[length(fActualReceivedLine) - 1] = #13) And
          (fActualReceivedLine[length(fActualReceivedLine)] = #10) Then Begin
            fHeader.Add(trim(fActualReceivedLine));
            fLastReceivedLine := fActualReceivedLine;
            fActualReceivedLine := '';
          End;
        End;
      End;
    rsBody: Begin
        fBodyBuffer := fBodyBuffer + chr(aData);
        dec(fBodyByteCount);
        If fBodyByteCount <= 0 Then Begin
          If assigned(OnReceiveHTTPDocument) Then Begin
            fBody.Text := fBodyBuffer;
            OnReceiveHTTPDocument(self, fHeader, fBody);
          End;
          reset;
        End;
      End;
  End;
End;

{ TRestServer }

Constructor TRestServer.Create(Const aTCPConnection: TLTcp);
Begin
  fTCPConnection := aTCPConnection;
  // Retten der alten Events
  FOnAccept_Captured := fTCPConnection.OnAccept;
  FOnDisconnect_Captured := fTCPConnection.OnDisconnect;
  FOnError_Captured := fTCPConnection.OnError;

  // Einhängen unserer Komponente
  fTCPConnection.OnAccept := @OnAccept;
  fTCPConnection.OnDisconnect := @OnDisconnect;
  fTCPConnection.OnReceive := @OnReceive;
  fTCPConnection.OnCanSend := @OnCanSend;
  fTCPConnection.OnError := @OnError;
  fGetPaths := Nil;
  fPostPaths := Nil;
End;

Destructor TRestServer.Destroy;
Begin
  DisConnect(true);

  fTCPConnection.OnAccept := FOnAccept_Captured;
  fTCPConnection.OnDisconnect := FOnDisconnect_Captured;
  fTCPConnection.OnError := FOnError_Captured;
  fTCPConnection := Nil;
End;

Procedure TRestServer.OnAccept(aSocket: TLSocket);
Begin
  RegisterSocket(aSocket);
  If assigned(FOnAccept_Captured) Then FOnAccept_Captured(aSocket);
End;

Procedure TRestServer.OnDisconnect(aSocket: TLSocket);
Begin
  UnRegisterSocket(aSocket);
  If assigned(FOnDisconnect_Captured) Then FOnDisconnect_Captured(aSocket);
End;

Procedure TRestServer.OnError(Const msg: String; aSocket: TLSocket);
Begin
  UnRegisterSocket(aSocket);
  If assigned(FOnError_Captured) Then FOnError_Captured(msg, aSocket);
End;

Procedure TRestServer.OnCanSend(aSocket: TLSocket);
Var
  pu: PUserData;
  Data: Array[0..1023] Of byte;
  send, cnt: Integer;
Begin
  If Not (aSocket.ConnectionStatus = scConnected) Then Begin
    // TODO: hier ein Unregister ?
    exit;
  End;
  data[0] := 0; // Totaler Quatsch, aber beruhigt den Compiler ;)
  pu := aSocket.UserData;
  If pu^.OutBuffPos < pu^.OutBuffer.Size Then Begin
    pu^.OutBuffer.Position := pu^.OutBuffPos;
    Repeat
      cnt := pu^.OutBuffer.Read(data, min(1024, pu^.OutBuffer.Size - pu^.OutBuffPos));
      If cnt = 0 Then Begin
        send := 0;
      End
      Else Begin
        send := fTCPConnection.Send(data, cnt, aSocket);
      End;
      pu^.OutBuffPos := pu^.OutBuffPos + send;
    Until (send = 0);
  End;
End;

Procedure TRestServer.OnReceive(aSocket: TLSocket);
Var
  buffer: Array[0..1023] Of byte;
  pu: PUserData;
  cnt, i: Integer;
Begin
  If Not assigned(aSocket.UserData) Then Begin
    OnError('Error, no userdata pointer set.', aSocket);
    aSocket.Disconnect(true);
    exit;
  End;
  pu := aSocket.UserData;
  // Weiter Reichen der Empfangenen Daten an den HTTP Empfänger ..
  cnt := aSocket.Get(buffer[0], length(buffer));
  While cnt <> 0 Do Begin
    For i := 0 To cnt - 1 Do Begin
      pu^.HTTPReceiver.ReceiveByte(buffer[i]);
    End;
    cnt := aSocket.Get(buffer[0], length(buffer));
  End;
End;

Procedure TRestServer.OnReceiveHTTPDocument(Sender: TObject;
  Const Header: TStrings; Body: TStrings);
Var
  Pu: PUserData;
  js, CMD: String;
Begin
  pu := THTTPReceiver(Sender).UserData;
  // Auslesen des "Commands" aus der 1. Header Zeile
  cmd := uppercase(copy(Header[0], 1, pos(' ', header[0]) - 1));
  Case cmd Of
    'GET': HandleGetCommand(Header, pu^.Socket);
    'POST': HandlePostCommand(Header, Body, pu^.Socket);
  Else Begin
      // Error, unknown CMD
      js := '{"error":"Method ' + cmd + ' is not supported for this resource"}';
      SendResponceString(
        // HTTP Header
        'HTTP/1.1 405 Method Not Allowed' + CRT +
        'Content-Type: application/json' + CRT +
        'Content-Length: ' + inttostr(length(js)) + CRT +
        // TODO: hier müssen mit weiteren Kommas, die 'Cases' von oben mit angefügt werden ;)
        'Allow: GET, POST' + CRT +
        CRT +
        // HTTP Body
        js
        , pu^.Socket);
    End;
  End;
End;

Procedure TRestServer.RegisterSocket(aSocket: TLSocket);
Var
  pu: PUserData;
Begin
  new(pu);
  pu^.Socket := aSocket;
  pu^.HTTPReceiver := THTTPReceiver.Create;
  pu^.HTTPReceiver.UserData := pu;
  pu^.HTTPReceiver.OnReceiveHTTPDocument := @OnReceiveHTTPDocument;
  pu^.OutBuffer := TMemoryStream.Create;
  aSocket.UserData := pu;
End;

Procedure TRestServer.UnRegisterSocket(aSocket: TLSocket);
Var
  pu: PUserData;
Begin
  If assigned(aSocket.UserData) Then Begin
    pu := aSocket.UserData;
    pu^.HTTPReceiver.Free;
    pu^.OutBuffer.free;
    Dispose(pu);
    aSocket.UserData := Nil;
  End;
End;

Procedure TRestServer.HandleGetCommand(Const Header: TStrings;
  Const aSocket: TLSocket);
Var
  Path, js: String;
  i: Integer;
  j: TJSONobj;
Begin
  //pu := aSocket.UserData;
  // 1. Extrahieren der Path
  Path := Uppercase(header[0]);
  Path := trim(copy(Path, 4, length(Path))); // "Get " abschneiden
  If pos(' ', Path) <> 0 Then Begin // einen ggf Suffix abschneiden (typisch "HTTP/1.1")
    Path := copy(Path, 1, pos(' ', Path) - 1);
  End;
  // 2. Suchen ob der Pfad registriert ist
  For i := 0 To high(fGetPaths) Do Begin
    If fGetPaths[i].path = Path Then Begin
      j := fGetPaths[i].Handler(self, Path);
      js := j.ToString();
      SendResponceString(
        // HTTP Header
        'HTTP/1.1 200 OK' + CRT +
        'Content-Type: application/json' + CRT +
        'Content-Length: ' + inttostr(length(js)) + CRT +
        CRT +
        // HTTP Body
        js
        , aSocket);
      j.free;
      exit;
    End;
  End;
  // Fehler code unknown Path ..
  js := '{"error":"Path ' + Path + ' not found"}';
  SendResponceString(
    // HTTP Header
    'HTTP/1.1 404 Not Found' + CRT +
    'Content-Type: application/json' + CRT +
    'Content-Length: ' + inttostr(length(js)) + CRT +
    CRT +
    // HTTP Body
    js
    , aSocket);
End;

Procedure TRestServer.HandlePostCommand(Const Header, Body: TStrings;
  Const aSocket: TLSocket);
Var
  jp: TJSONParser;
  j: TJSONObj;
  jn: TJSONNode;
  s, Path, URI: String;
  i: Integer;
  IsURLEncoded: Boolean;
  params: TUriParameters;
Begin
  // 1. Extrahieren der Path
  Path := Uppercase(header[0]);
  Path := trim(copy(Path, 5, length(Path))); // "POST " abschneiden
  If pos(' ', Path) <> 0 Then Begin // einen ggf. Suffix abschneiden (typisch "HTTP/1.1")
    Path := copy(Path, 1, pos(' ', Path) - 1);
  End;
  If pos('?', path) <> 0 Then Begin
    IsURLEncoded := true;
    // Die URI ist Case Sensitive -> Deswegen wird sie hier noch mal neu vom Header gelesen..
    URI := copy(header[0], pos('?', header[0]) + 1, length(header[0]));
    If pos(' ', URI) <> 0 Then Begin // einen ggf. Suffix abschneiden (typisch "HTTP/1.1")
      URI := copy(URI, 1, pos(' ', URI) - 1);
    End;
    path := copy(path, 1, pos('?', Path) - 1);
  End
  Else Begin
    IsURLEncoded := false;
  End;
  // Erstellen der Parameter als JSON
  If IsURLEncoded Then Begin
    params := ParseQueryParams(URI);
    // Umwandeln der URi in ein Json Ding, so dass das nach außen Einheitlich ist ;)
    jn := TJSONNode.Create;
    For i := 0 To high(params) Do Begin
      jn.AddObj(TJSONValue.Create(params[i].Name, params[i].Value, true));
    End;
    j := jn
  End
  Else Begin
    jp := TJSONParser.Create;
    jp.SupportJSON5Comments := true; // Ka ob das Sinnvoll ist, aber so ist es auf jeden Fall "Robuster"
    j := Nil;
    Try
      j := jp.Parse(Body.Text);
      If Not assigned(j) Then Raise exception.create('Blub'); // Wir wollen einfach in den Fehlerhandler ;)
    Except
      // Fehler Anfrage nicht Parsbar
      s := '{"error":"Post ' + Path + ' without data"}';
      SendResponceString(
        'HTTP/1.1 400 Bad Request' + CRT +
        'Content-Type: application/json' + CRT +
        'Content-Length: ' + inttostr(length(s)) + CRT +
        CRT +
        // HTTP Body
        s
        , aSocket);
    End;
    jp.Free;
    If Not assigned(j) Then exit;
  End;
  // Suchen eines Passenden Handlers für den Pfad
  For i := 0 To high(fPostPaths) Do Begin
    If fPostPaths[i].Path = path Then Begin
      If fPostPaths[i].Handler(self, Path, j) Then Begin
        // Aktzeptiert
        SendResponceString(
          'HTTP/1.1 204 No Content' + CRT
          + CRT, aSocket);
      End
      Else Begin
        // Anfrage nicht Akzeptiert
        SendResponceString(
          'HTTP/1.1 422 Path with value Not accepted' + CRT
          + CRT, aSocket);
      End;
      j.free;
      exit;
    End;
  End;
  j.free;
  // Fehler kein Handler definiert
  s := '{"error":"No handler for path ' + Path + '"}';
  SendResponceString(
    'HTTP/1.1 501 Not Implemented' + CRT +
    'Content-Type: application/json' + CRT +
    'Content-Length: ' + inttostr(length(s)) + CRT +
    CRT +
    // HTTP Body
    s
    , aSocket);
End;

Procedure TRestServer.SendResponceString(Const aData: String;
  Const aSocket: TLSocket);
Var
  Pu: PUserData;
Begin
  pu := aSocket.UserData;
  pu^.OutBuffer.Clear;
  pu^.OutBuffer.Write(aData[1], length(aData));
  pu^.OutBuffPos := 0;
  OnCanSend(aSocket);
End;

Procedure TRestServer.CallAction;
Begin
  fTCPConnection.CallAction;
End;

Procedure TRestServer.RegisterGetHandler(Const aGetPath: String;
  Const aHandler: TGetHandler);
Begin
  // TODO: Doppelte Pfade via exception verhindern
  setlength(fGetPaths, high(fGetPaths) + 2);
  fGetPaths[high(fGetPaths)].path := Uppercase(aGetPath);
  fGetPaths[high(fGetPaths)].Handler := aHandler;
End;

Procedure TRestServer.RegisterPostHandler(Const aPostPath: String;
  Const aHandler: TPostHandler);
Begin
  // TODO: Doppelte Pfade via exception verhindern
  setlength(fPostPaths, high(fPostPaths) + 2);
  fPostPaths[high(fPostPaths)].path := Uppercase(aPostPath);
  fPostPaths[high(fPostPaths)].Handler := aHandler;
End;

Function TRestServer.Listen(aPort: integer): Boolean;
Begin
  result := fTCPConnection.Listen(aPort);
End;

Procedure TRestServer.DisConnect(Const Forced: Boolean);
Begin
  fTCPConnection.IterReset;
  While fTCPConnection.IterNext Do Begin
    If assigned(fTCPConnection.Iterator) Then Begin
      OnDisconnect(fTCPConnection.Iterator);
    End;
  End;
  fTCPConnection.Disconnect(Forced);
End;

{ TRestClient }

Constructor TRestClient.Create(Const aTCPConnection: TLTcp);
Begin
  Inherited create;
  fTCPConnection := aTCPConnection;
End;

Destructor TRestClient.Destroy;
Begin
  DisConnect(true);
  fTCPConnection := Nil;
End;

Function TRestClient.Connect(IP: String; Port: integer): Boolean;
Begin
  result := fTCPConnection.Connect(IP, Port);
End;

Procedure TRestClient.DisConnect(Const Forced: Boolean);
Begin
  fTCPConnection.Disconnect(Forced);
End;

End.

