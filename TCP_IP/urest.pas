(******************************************************************************)
(* uRest.pas                                                       11.08.2025 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
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
(*               0.02 - Initial version (client)                              *)
(*               0.03 - ADD HTTPHeader to getHandler                          *)
(*               0.04 - support more detailed post results                    *)
(*               0.05 - support post with no params                           *)
(*               0.06 - connected property for TRestClient                    *)
(*                                                                            *)
(******************************************************************************)

Unit urest;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, lnet, uJSON;

Type

  TPostResult = Record
    HTTPCode: Integer; // Supported are all in "CodetoHTTPString", if you need more you need to adjust that function !
    Content: TJSONObj; // If Nil HTTP-Code needs to be 204 or 304 !
  End;

  TOnReceiveHTTPDocument = Procedure(Sender: TObject; Const Header: TStrings; Body: TStrings) Of Object;

  (*
   * Theoretisch könnte man die HTTP Header mit durchreichen, das gäbe dann die Möglichkeit "Cookies" zu setzen
   * -> Dann wäre das ganze nicht Stateless, aber die bisherige Anforderung hat dies nicht benötigt.
   *)
  TGetHandler = Function(Sender: TObject; Const aPath: String; Const HTTPHeader: tstrings): TJSONobj Of Object;
  TPostHandler = Function(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): TPostResult Of Object;

  TOnGetResultCallback = Procedure(Sender: TObject; Const aPath: String; Const aContent: TJSONObj) Of Object;
  TOnPostResultCallback = Procedure(Sender: TObject; Const aPath: String; aResult: TJSONObj) Of Object;

  THTTPReceivingState = (
    rsHeader,
    rsBody
    );

  { THTTPReceiver }

  THTTPReceiver = Class // Simple HTTP Decoder Class // TODO: Auslagern in eine Eigene File
  private
    fHTTPReceivingState: THTTPReceivingState;
    fHeader: TStringList;
    fBody: TStringList; // TODO: Das wäre als TMemoryStream wahrscheinlich Sinnvoller !
    fBodyByteCount: integer;
    fBodyBuffer: String;
    fActualReceivedLine: String;
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
   * Daten, welche an den Socket gebunden werden (REST Server)
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

    Procedure SendResponce(HTTPCode: Integer; Const Body: String; Const aSocket: TLSocket);
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

  TStatus = (sIdle, sGet, sPost); // Für den Client, könnte auch Private sein ..

  { TRestClient }

  TRestClient = Class
  private
    fConnected: Boolean;
    fhttpReceiver: THTTPReceiver;
    fStatus: TStatus;
    fTCPConnection: TLTcp;

    FOnConnect_Captured: TLSocketEvent;
    FOnDisconnect_Captured: TLSocketEvent;
    FOnError_Captured: TLSocketErrorEvent;

    Procedure OnConnect(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);
    //Procedure OnCanSend(aSocket: TLSocket); -- Eigentlich können wir uns das sparen, weil die Lnet Komponente einen 64K Puffer hat und der Reichen müsste, sonst muss das rein wie beim Server
    Procedure OnReceive(aSocket: TLSocket);

    Procedure OnReceiveHTTPDocument(Sender: TObject; Const Header: TStrings; Body: TStrings);
    // Schaut ob noch Callbacks offen sind und Sendet an diese die msg
    Procedure HandleMessage(Const msg: String);
  private
    (*
     * Get
     *)
    fGetCallback: TOnGetResultCallback;
    fGetPath: String;
    Procedure HandleGetCommand(Const Header, Body: TStrings);
  private
    (*
     * Post
     *)
    fPostCallback: TOnPostResultCallback;
    fPostPath: String;
    Procedure HandlePostCommand(Const Header, Body: TStrings);
  public
    Property Connected: Boolean read fConnected;
    (*
     * OnConnect, OnDisconnect, OnError, dürfen von "außen" genutzt werden (indem sie vorher initialisiert werden)
     * aTCPConnection wird nicht freigegeben !
     *)
    Constructor Create(Const aTCPConnection: TLTcp); virtual;
    Destructor Destroy(); override;

    Procedure CallAction; // Ruft die Callaction der TCP-Komponente auf (nur notwendig, wenn keine LCL variante verwendet wird.)

    Function Get(Const Path: String; Const Callback: TOnGetResultCallback): Boolean;
    Function Post(Const Path: String; Const Data: TJSONObj; Const Callback: TOnPostResultCallback): Boolean;

    Function Connect(IP: String; Port: integer): Boolean;
    Procedure DisConnect(Const Forced: Boolean);
  End;

  TUriParameter = Record
    Name: String;
    Value: String;
  End;

  TUriParameters = Array Of TUriParameter;

  (*
   * In: "param1=value&param2=Value%20with%20Spaces
   *
   * Out:
   * Array of parsed Uri Parameters
   *)
Function ParseQueryParams(Const Query: String): TUriParameters;

Implementation

Uses math;

Const
  CRT = #13#10;

Procedure Nop; // Debug Only ..
Begin

End;

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

Function CodetoHTTPString(code: Integer): String;
Begin
  result := '';
  Case code Of
    200: result := '200 OK';
    201: result := '201 Created';
    204: result := '204 No Content';

    304: result := '304 Not Modified';

    400: result := '400 Bad Request';
    401: result := '401 Unauthorized';
    403: result := '403 Forbidden';
    404: result := '404 Not Found';
    405: result := '405 Method Not Allowed';
    409: result := '409 Conflict';
    422: result := '422 Unprocessable Entity';

    500: result := '500 Internal Server Error';
    501: result := '501 Not Implemented';
  Else Begin
      Raise exception.create('CodetoHTTP: Error, unknown code: ' + inttostr(code));
    End;
  End;
End;

(*
 * Prüft ab, ob der zugehörige HTTPCpde pasend zum "Content" ist, ..
 *)

Function CheckHTTPCodeAndResultContent(HTTPCode: Integer; Const Content: Boolean): Boolean;
Begin
  result := true;
  (*
   * Liste aller HTTP-Codes die keinen Content haben dürfen!
   *)
  If (
    (HTTPCode = 204)
    Or (HTTPCode = 304)
    ) And (Content) Then Begin
    result := false;
  End;
  (*
   * Liste aller HTTP-Codes die einen Content haben müssen !
   * Gemäß HTTP-Spec. sind die eigentlich alle Optional, aber
   * der hier liegende Code wurde mit VS-Code Thunder Client
   * gegen geprüft und zumindest Thunder Client, hat bei den
   * folgenden einen Body "verlangt".
   *)
  If (
    (HTTPCode = 200)
    Or (HTTPCode = 400)
    Or (HTTPCode = 404)
    Or (HTTPCode = 405)
    Or (HTTPCode = 422)
    Or (HTTPCode = 501)
    ) And (Not Content) Then Begin
    result := false;
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
    // Vor Allokieren des Empfangspuffers ;)
    setlength(fBodyBuffer, fBodyByteCount);
    fBodyByteCount := 1;
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
            fActualReceivedLine := '';
          End;
        End;
      End;
    rsBody: Begin
        fBodyBuffer[fBodyByteCount] := chr(aData);
        inc(fBodyByteCount);
        If fBodyByteCount > length(fBodyBuffer) Then Begin
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
  CMD: String;
Begin
  pu := THTTPReceiver(Sender).UserData;
  // Auslesen des "Commands" aus der 1. Header Zeile
  cmd := uppercase(copy(Header[0], 1, pos(' ', header[0]) - 1));
  Case cmd Of
    'GET': HandleGetCommand(Header, pu^.Socket);
    'POST': HandlePostCommand(Header, Body, pu^.Socket);
  Else Begin
      // Error, unknown CMD
      // TODO: der Allow Tag ist nun verloren gegangen :/
      SendResponce(
        405,
        '{"error": ' + StringToJsonString('Method ' + cmd + ' is not supported for this resource') + '}',
        pu^.Socket
        );
      //SendResponceString(
      //  // HTTP Header
      //  'HTTP/1.1 ' + CodetoHTTP(405) + CRT +
      //  'Content-Type: application/json' + CRT +
      //  'Content-Length: ' + inttostr(length(js)) + CRT +
      //  // TODO: hier müssen mit weiteren Kommas, die 'Cases' von oben mit angefügt werden ;)
      //  'Allow: GET, POST' + CRT +
      //  CRT +
      //  // HTTP Body
      //  js
      //  , pu^.Socket);
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
  Path: String;
  i: Integer;
  j: TJSONobj;
Begin
  // 1. Extrahieren der Path
  Path := Uppercase(header[0]);
  Path := trim(copy(Path, 4, length(Path))); // "Get " abschneiden
  If pos(' ', Path) <> 0 Then Begin // einen ggf. Suffix abschneiden (typisch "HTTP/1.1")
    Path := copy(Path, 1, pos(' ', Path) - 1);
  End;
  // Abschneiden aller ggf mit übergebenen Parameter
  If pos('?', path) <> 0 Then Begin
    path := copy(path, 1, pos('?', path) - 1);
  End;
  // 2. Suchen ob der Pfad registriert ist
  For i := 0 To high(fGetPaths) Do Begin
    If fGetPaths[i].path = Path Then Begin
      j := fGetPaths[i].Handler(self, Path, Header);
      SendResponce(200, j.ToString(), aSocket);
      j.free;
      exit;
    End;
  End;
  // Fehler code unknown Path ..
  SendResponce(
    404,
    '{"error":' + StringToJsonString('Path ' + Path + ' not found') + '}',
    aSocket
    );
End;

Procedure TRestServer.HandlePostCommand(Const Header, Body: TStrings;
  Const aSocket: TLSocket);
Var
  jp: TJSONParser;
  j: TJSONObj;
  jn: TJSONNode;
  Path, URI: String;
  i: Integer;
  IsURLEncoded: Boolean;
  params: TUriParameters;
  PostRes: TPostResult;
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
    j := jn;
  End
  Else Begin
    jp := TJSONParser.Create;
    jp.SupportJSON5Comments := true; // Ka ob das Sinnvoll ist, aber so ist es auf jeden Fall "Robuster"
    j := Nil;
    Try
      j := jp.Parse(Body.Text);
    Except
      // Fehler Anfrage nicht Parsbar
      SendResponce(
        400, '{"error":' + StringToJsonString('Post ' + Path + ' without valid data') + '}',
        aSocket
        );
      exit;
    End;
    jp.Free;
  End;
  // Suchen eines Passenden Handlers für den Pfad
  For i := 0 To high(fPostPaths) Do Begin
    If fPostPaths[i].Path = path Then Begin
      PostRes := fPostPaths[i].Handler(self, Path, j);
      If assigned(PostRes.Content) Then Begin
        SendResponce(PostRes.HTTPCode, PostRes.Content.ToString(), aSocket);
        PostRes.Content.Free;
      End
      Else Begin
        SendResponce(PostRes.HTTPCode, '', aSocket);
      End;
      If assigned(j) Then j.free;
      exit;
    End;
  End;
  If assigned(j) Then j.free;
  // Fehler kein Handler definiert
  SendResponce(
    501,
    '{"error":' + StringToJsonString('No handler for path ' + Path) + '}',
    aSocket
    );
End;

Procedure TRestServer.SendResponce(HTTPCode: Integer; Const Body: String;
  Const aSocket: TLSocket);
Var
  Pu: PUserData;
  Header, res: String;
Begin
  Header := 'HTTP/1.1 ' + CodetoHTTPString(HTTPCode) + CRT;
  If Not CheckHTTPCodeAndResultContent(HTTPCode, Body <> '') Then Begin
    Raise exception.create('Error, HTTPCode and body are not compatible, see "CheckHTTPCodeAndResultContent" for more details.');
  End;
  If Body <> '' Then Begin
    Header := Header +
      'Content-Type: application/json' + CRT +
      'Content-Length: ' + inttostr(length(Body)) + CRT;
    res :=
      Header + CRT +
      Body
      ;
  End
  Else Begin
    // Bodyless responce
    res :=
      Header + CRT
      ;
  End;
  pu := aSocket.UserData;
  pu^.OutBuffer.Clear;
  pu^.OutBuffer.Write(res[1], length(res));
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
  result := false;
  If (fPostPaths = Nil) And (fGetPaths = Nil) Then Begin
    Raise Exception.Create('Error, no path handler set.');
  End;
  result := fTCPConnection.Listen(aPort);
End;

Procedure TRestServer.DisConnect(Const Forced: Boolean);
Begin
  If Not fTCPConnection.Connected Then exit;
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
  fConnected := false;
  fTCPConnection := aTCPConnection;

  FOnConnect_Captured := fTCPConnection.OnConnect;
  FOnDisconnect_Captured := fTCPConnection.OnDisconnect;
  FOnError_Captured := fTCPConnection.OnError;

  // Einhängen unserer Komponente
  fTCPConnection.OnConnect := @OnConnect;
  fTCPConnection.OnDisconnect := @OnDisconnect;
  fTCPConnection.OnReceive := @OnReceive;
  //fTCPConnection.OnCanSend := @OnCanSend;
  fTCPConnection.OnError := @OnError;

  fStatus := sIdle;
  fhttpReceiver := THTTPReceiver.Create;
  fhttpReceiver.OnReceiveHTTPDocument := @OnReceiveHTTPDocument;
End;

Destructor TRestClient.Destroy;
Begin
  DisConnect(true);

  fTCPConnection.OnConnect := FOnConnect_Captured;
  fTCPConnection.OnDisconnect := FOnDisconnect_Captured;
  fTCPConnection.OnError := FOnError_Captured;

  fhttpReceiver.free;
  fhttpReceiver := Nil;
  fTCPConnection := Nil;
End;

Procedure TRestClient.CallAction;
Begin
  fTCPConnection.CallAction;
End;

Procedure TRestClient.OnReceiveHTTPDocument(Sender: TObject;
  Const Header: TStrings; Body: TStrings);
Begin
  Case fStatus Of
    sGet: HandleGetCommand(header, body);
    sPost: HandlePostCommand(header, body);
  End;
  fStatus := sIdle;
End;

Procedure TRestClient.HandleMessage(Const msg: String);
Var
  jn: TJSONNode;
Begin
  jn := TJSONNode.Create;
  jn.AddObj(TJSONValue.Create('msg', msg, true));
  If assigned(fGetCallback) Then Begin
    fGetCallback(self, fGetPath, jn);
  End;
  fGetCallback := Nil;
  If assigned(fPostCallback) Then Begin
    fPostCallback(self, fPostPath, jn);
  End;
  fPostCallback := Nil;
  jn.free;
  fStatus := sIdle;
End;

Procedure TRestClient.OnConnect(aSocket: TLSocket);
Begin
  fStatus := sIdle;
  fConnected := true;
  If assigned(FOnConnect_Captured) Then FOnConnect_Captured(aSocket);
End;

Procedure TRestClient.OnDisconnect(aSocket: TLSocket);
Begin
  fConnected := false;
  HandleMessage('lost connection');
  If assigned(FOnDisconnect_Captured) Then FOnDisconnect_Captured(aSocket);
End;

Procedure TRestClient.OnError(Const msg: String; aSocket: TLSocket);
Begin
  fConnected := false;
  HandleMessage(msg);
  If assigned(FOnError_Captured) Then FOnError_Captured(msg, aSocket);
End;

Procedure TRestClient.OnReceive(aSocket: TLSocket);
Var
  buffer: Array[0..1023] Of byte;
  cnt, i: Integer;
Begin
  // Weiter Reichen der Empfangenen Daten an den HTTP Empfänger ..
  cnt := aSocket.Get(buffer[0], length(buffer));
  While cnt <> 0 Do Begin
    For i := 0 To cnt - 1 Do Begin
      fHTTPReceiver.ReceiveByte(buffer[i]);
    End;
    cnt := aSocket.Get(buffer[0], length(buffer));
  End;
End;

Procedure TRestClient.HandleGetCommand(Const Header, Body: TStrings);
Var
  j: TJSONObj;
  jp: TJSONParser;
Begin
  jp := TJSONParser.Create;
  Try
    j := jp.Parse(Body.Text);
  Except
    j := Nil;
  End;
  jp.free;
  If Not assigned(j) Then Begin
    HandleMessage('Error, got invalid data.');
    exit;
  End;
  fGetCallback(self, fGetPath, j);
  fGetCallback := Nil;
  j.free;
End;

Procedure TRestClient.HandlePostCommand(Const Header, Body: TStrings);
Var
  j: TJSONObj;
  jp: TJSONParser;
  sa: TStringArray;
Begin
  // Read HTTP Status , wenn <> 204 -> Body lesen
  sa := Header[0].Split(' ');
  If (sa[1] = '204') Then Begin
    HandleMessage('OK');
    exit;
  End
  Else Begin
    If (sa[1] = '304') Then Begin
      HandleMessage('Not Modified');
      exit;
    End
    Else Begin
      jp := TJSONParser.Create;
      Try
        j := jp.Parse(Body.Text);
      Except
        j := Nil;
      End;
      jp.free;
    End;
  End;
  If Not assigned(j) Then Begin
    HandleMessage('Error, got invalid data.');
    exit;
  End;
  fPostCallback(self, fPostPath, j);
  fPostCallback := Nil;
  j.free;
End;

Function TRestClient.Get(Const Path: String;
  Const Callback: TOnGetResultCallback): Boolean;
Begin
  result := false;
  If (Not fTCPConnection.Connected) Or
    (fStatus <> sIdle) Or
    (Not Assigned(Callback)) Then exit;
  fStatus := sGet;
  fGetCallback := Callback;
  fGetPath := Path;
  // Sollte der User Parameter mit gegeben haben, schneiden wir diese mal ab !
  If pos('?', fGetPath) <> 0 Then Begin
    fGetPath := copy(fGetPath, 1, pos('?', fGetPath) - 1);
  End;
  fhttpReceiver.Reset;
  fTCPConnection.SendMessage(
    'GET ' + Path + ' HTTP/1.1' + CRT +
    'Content-Type: application/json' + CRT +
    CRT
    );
  result := true;
End;

Function TRestClient.Post(Const Path: String; Const Data: TJSONObj;
  Const Callback: TOnPostResultCallback): Boolean;
Var
  s: String;
Begin
  result := false;
  If (Not fTCPConnection.Connected) Or
    (fStatus <> sIdle) Or
    (Not assigned(Data)) Or
    (Not Assigned(Callback)) Then exit;
  fStatus := sPost;
  fPostCallback := Callback;
  fPostPath := Path;
  // Sollte der User Parameter mit gegeben haben, schneiden wir diese mal ab !
  If pos('?', fPostPath) <> 0 Then Begin
    fPostPath := copy(fPostPath, 1, pos('?', fPostPath) - 1);
  End;
  fhttpReceiver.Reset;
  s := Data.ToString();
  fTCPConnection.SendMessage(
    'POST ' + Path + ' HTTP/1.1' + CRT +
    'Content-Type: application/json' + CRT +
    'Content-Length: ' + inttostr(length(s)) + CRT +
    CRT +
    // HTTP Body
    s);
  result := true;
End;

Function TRestClient.Connect(IP: String; Port: integer): Boolean;
Begin
  result := fTCPConnection.Connect(IP, Port);
  fhttpReceiver.Reset;
  fStatus := sIdle;
  fGetCallback := Nil;
  fPostCallback := Nil;
End;

Procedure TRestClient.DisConnect(Const Forced: Boolean);
Begin
  fTCPConnection.Disconnect(Forced);
End;

End.

