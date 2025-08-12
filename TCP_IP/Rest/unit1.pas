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
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
  private
    // Dummy Path Handler for demonstration ;)
    Function GetStatus(Sender: TObject; Const aPath: String): TJSONobj;

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
  caption := 'Rest Api, Demo ver. 0.01 by Corpsman';
  edit1.text := '8080'; // weitere "Typische" 3000, 5000, 8888 – ebenfalls gängig für lokale Tests
  edit2.text := '127.0.0.1';
  edit3.text := '8080'; // weitere "Typische" 3000, 5000, 8888 – ebenfalls gängig für lokale Tests
  RestServer := Nil;
  RestClient := Nil;
  Memo1.Clear;
  PageControl1.PageIndex := 0; // Reset auf Server Page, egal wie's im designer gestanden hat ;)
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin

End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  showmessage(msg);
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  button1.caption := 'Start Server';
End;

Function TForm1.GetStatus(Sender: TObject; Const aPath: String): TJSONobj;
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
    RestServer.RegisterPostHandler('/api/CMD', @PostCMD);
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
      button3.caption := 'Disconnect';
    End
    Else Begin
      ShowMessage('Error, during connect');
    End;
  End;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  label3.caption := 'Test server using: "127.0.0.1:' + edit1.text + '/api/status" with get';
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(RestServer) Then RestServer.free;
  RestServer := Nil;
  If assigned(RestClient) Then RestClient.free;
  RestClient := Nil;
End;

End.

