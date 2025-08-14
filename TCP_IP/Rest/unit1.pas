(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of REST_Demo                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  lNetComponents, urest, lNet, uJSON;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
  private
    (*
     * For Client
     *)
    Procedure OnGetResultCallback(Sender: TObject; Const aPath: String; Const aContent: TJSONObj);
    Procedure OnPostResultCallback(Sender: TObject; Const aPath: String; aResult: TJSONObj);

    (*
     * For Server
     *)
    // Dummy Path Handler for demonstration ;)
    Function GetStatus(Sender: TObject; Const aPath: String; Const HTTPHeader: TStrings): TJSONobj;
    // Dummy Path post handler
    Function PostCMD(Sender: TObject; Const aPath: String; Const aContent: TJSONObj): Boolean;
  public
    RestServer: TRestServer;
    RestClient: TRestClient;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // As Client the VS-Code Plugin "Thunder Client" can be uses ;)
  (*
   * History: 0.01 = Initialversion (Server)
   *          0.02 = More infos in Server mode
   *                 Initialversion (CLient)
   *
   *)
  caption := 'Rest Api, Demo ver. 0.02 by Corpsman';
  edit1.text := '8080'; // weitere "Typische" 3000, 5000, 8888 – ebenfalls gängig für lokale Tests
  edit2.text := '127.0.0.1';
  edit3.text := '8080'; // weitere "Typische" 3000, 5000, 8888 – ebenfalls gängig für lokale Tests
  label7.caption := '0';
  RestServer := Nil;
  RestClient := Nil;
  Memo1.Clear;
  PageControl1.PageIndex := 0; // Reset auf Server Page, egal wie's im designer gestanden hat ;)
  memo2.Text :=
    '{' + LineEnding +
    ' "TestName": "TestValue"' + LineEnding +
    '}';
  ComboBox1Change(ComboBox1);
  edit4.text := '/api/status';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  label3.caption :=
    'Demo supports:' + LineEnding + LineEnding +
    'GET path "/api/status"' + LineEnding +
    'POST path "/api/cmd"' + LineEnding;
End;


Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  If assigned(RestClient) Then RestClient.free;
  RestClient := Nil;
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin
  // Der Server Akzeptiert eine Verbindung
  label7.caption := inttostr(strtoint(label7.caption) + 1);
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Begin
  // Der Client hat es geschafft sich zu verbinden
  If assigned(RestClient) Then
    button3.caption := 'Disconnect';
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  // Der Server hat eine Verbindung verloren
  If assigned(RestServer) Then Begin
    label7.caption := inttostr(strtoint(label7.caption) - 1);
  End;
  // Der Client wurde getrennt
  If assigned(RestClient) Then Begin
    button3.caption := 'Connect to server';
  End;
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  // Egal wer, bei einem Fehler wird alles Platt gemacht.
  showmessage(msg);
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  If assigned(RestClient) Then RestClient.free;
  RestClient := Nil;
  button1.caption := 'Start Server';
  button3.caption := 'Connect to server';
End;

Procedure TForm1.OnGetResultCallback(Sender: TObject; Const aPath: String;
  Const aContent: TJSONObj);
Begin
  Memo1.Append('OnGetStatusResult: ' + aPath +
    LineEnding + aContent.ToString());
End;

Procedure TForm1.OnPostResultCallback(Sender: TObject; Const aPath: String;
  aResult: TJSONObj);
Begin
  Memo1.Append('OnPostStatusResult: ' + aPath +
    LineEnding + aResult.ToString());
End;

Function TForm1.GetStatus(Sender: TObject; Const aPath: String; Const HTTPHeader: TStrings): TJSONobj;
Var
  jn: TJSONNode;
  jnv: TJSONValue;
Begin
  jn := TJSONNode.Create;
  jnv := TJSONValue.Create('status', 'ok', true);
  jn.AddObj(jnv);
  result := jn;
  Memo1.Append('GetStatus');
End;

Function TForm1.PostCMD(Sender: TObject; Const aPath: String;
  Const aContent: TJSONObj): Boolean;
Begin
  Memo1.Append(
    'PostCMD: ' + aPath + LineEnding +
    aContent.ToString('')
    );
  result := true; // Accepted ;)
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Start Server
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  If assigned(RestClient) Then RestClient.free;
  RestClient := Nil;

  If button1.caption = 'Disconnect' Then Begin
    button1.caption := 'Start Server';
  End
  Else Begin
    RestServer := TRestServer.Create(LTCPComponent1);
    (*
     * registrieren aller pfade für die entsprechenden Methoden..
     *)
    RestServer.RegisterGetHandler('/api/status', @GetStatus);
    RestServer.RegisterPostHandler('/api/cmd', @PostCMD);
    Memo1.Clear;
    If RestServer.Listen(strtointdef(edit1.text, 8080)) Then Begin
      button1.caption := 'Disconnect';
    End
    Else Begin
      ShowMessage('Error, during listen');
    End;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  Memo1.Clear;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Connect to Server
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  If assigned(RestClient) Then RestClient.free;
  RestClient := Nil;
  If button3.caption = 'Disconnect' Then Begin
    button3.caption := 'Connect to server';
  End
  Else Begin
    RestClient := TRestClient.Create(LTCPComponent1);
    Memo1.Clear;
    If RestClient.Connect(Edit2.text, strtointdef(edit3.text, 8080)) Then Begin
      // Nichts, das macht der "OnConnect" handler
    End
    Else Begin
      ShowMessage('Error, during connect');
    End;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  j: TJSONObj;
  jp: TJSONParser;
Begin
  If Not assigned(RestClient) Then Begin
    ShowMessage('Error, not connected as client');
    exit;
  End;
  // Send as Client
  Case ComboBox1.ItemIndex Of
    0: Begin // Get
        If Not RestClient.Get(edit4.text, @OnGetResultCallback) Then Begin
          showmessage('Error, not connected, or busy.');
        End;
      End;
    1: Begin // Post
        jp := TJSONParser.Create;
        jp.SupportJSON5Comments := true;
        j := jp.Parse(memo2.Text);
        jp.free;
        If Not assigned(j) Then Begin
          showmessage('Error, invalid JSON content.');
          exit;
        End;
        If Not RestClient.post(edit4.text, j, @OnPostResultCallback) Then Begin
          showmessage('Error, not connected, or busy.');
        End;
        j.free;
      End;
  End;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Begin
  label9.Visible := ComboBox1.ItemIndex = 1;
  Memo2.Visible := ComboBox1.ItemIndex = 1;
End;

End.

