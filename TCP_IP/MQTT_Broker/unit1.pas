Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lNetComponents, uMQTTbroker;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    fServer: TMQTTBroker;
    Procedure OnLog(Sender: TObject; ClientID: integer; aValue: String);
    Function OnSubscribeRequest(Sender: TObject; ClientID: integer; Subscription: String): Treturn;
    Procedure OnPublishRequest(Sender: TObject; ClientID: integer; aName, aPayload: String; DUP, Retain: Boolean);
    Procedure OnPing(Sender: TObject; ClientID: integer);
    Procedure OnAcceptMQTTClient(Sender: TObject; ClientID: integer);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If fServer.Listen() Then Begin
    Button1.Enabled := false;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  fServer.Disconnect;
  button1.enabled := true;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  Memo1.Clear;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  fServer.Disconnect;
  fServer.CallAction();
  fServer.free;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  fServer := TMQTTBroker.Create(LTCPComponent1);
  fserver.OnLog := @OnLog;
  fserver.OnSubscribeRequest := @OnSubscribeRequest;
  fserver.OnPublishRequest := @OnPublishRequest;
  fserver.OnPingEvent := @OnPing;
  fserver.OnAcceptMQTTClient := @OnAcceptMQTTClient;
  memo1.Clear;
End;

Procedure TForm1.OnLog(Sender: TObject; ClientID: integer; aValue: String);
Var
  s: String;
Begin
  If ClientID <> 0 Then Begin
    aValue := '[' + inttostr(ClientID) + '] ' + aValue;
  End;
  If CheckBox1.Checked Then Begin
    s := FormatDateTime('YYYY.MM.DD HH:NN:SS', now) + ' ' + aValue;
  End
  Else Begin
    s := aValue;
  End;
  Memo1.Lines.Add(s);
End;

Function TForm1.OnSubscribeRequest(Sender: TObject; ClientID: integer;
  Subscription: String): Treturn;
Begin
  Onlog(self, ClientID, 'Request subscription for: ' + Subscription);
  result := rQoS0;
End;

Procedure TForm1.OnPublishRequest(Sender: TObject; ClientID: integer; aName,
  aPayload: String; DUP, Retain: Boolean);
Begin
  OnLog(self, ClientID, format('Publish: %s = %s', [aName, aPayload]));
End;

Procedure TForm1.OnPing(Sender: TObject; ClientID: integer);
Begin
  Onlog(self, ClientID, 'Ping from client');
End;

Procedure TForm1.OnAcceptMQTTClient(Sender: TObject; ClientID: integer);
Begin
  Onlog(self, ClientID, 'Accept client');
End;

End.

