(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of uSynapseComponents.pas                                *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uSynapseComponents;

Type

  TConncection = Record
    Socket: TSynapseConnection;
    Num: integer; // Zur Anzeige, dass der User die Sockets auch unterscheiden kann
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure ComboBox1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    SynapseComponent1: TSynapseComponent;

    FIsServer: Boolean;
    fClients: Array Of TConncection;
    fClientCounter: integer;

    Procedure OnAccept(Connection: TSynapseConnection); // Server accepts a connection
    Procedure OnConnect(Connection: TSynapseConnection); // Client sucessfully connected to server
    Procedure OnReceive(Connection: TSynapseConnection); // Connection hat Daten empfangen

    Procedure OnDisconnect(Connection: TSynapseConnection); // Client wurde getrennt, oder Server hat einen Client verloren
    Procedure OnError(Const ErrorMsg: String; Connection: TSynapseConnection);
    Procedure Disconnect();
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Function HextoByte(Value: String): Cardinal;
Var
  m: cardinal;
  c: byte;
Begin
  value := uppercase(value);
  result := 0;
  m := 1;
  While length(value) <> 0 Do Begin
    c := ord(value[length(value)]);
    delete(value, length(value), 1);
    If c In [48..57] Then
      result := result + (c - 48) * m;
    If c In [65..70] Then
      result := result + (c - 55) * m;
    m := m * 16;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin

  //  Testcases:
  //  - Verschlüsselt verbinden, client "erkennt" es erst, wenn Server 1 byte gesendet hat
  //  - Verschlüsselt verbinden, Server Schließen ohne Client zu disconnecten -> chrash auf server


    (*
     * SSL Docu: https://synapse.readthedocs.io/en/latest/run/ssl/
     *)
  caption := 'Synapse Socket ver. 0.01';
  SynapseComponent1 := TSynapseComponent.Create;
  SynapseComponent1.ReuseAddress := true;
  SynapseComponent1.OnAccept := @OnAccept;
  SynapseComponent1.OnConnect := @OnConnect;
  SynapseComponent1.OnDisconnect := @OnDisconnect;
  SynapseComponent1.OnError := @OnError;
  SynapseComponent1.OnReceive := @OnReceive;

  Edit1.text := '127.0.0.1';
  Edit2.text := '1234';
  Edit3.text := 'server.crt';
  Edit4.text := 'server.key';
  Edit5.text := 'server.crt';

  ComboBox1.text := '';
  memo1.Clear;
  fClients := Nil;
End;

Procedure TForm1.OnAccept(Connection: TSynapseConnection);
Begin
  // Nur im Servermodus Akzeptieren eines neuen Clients
//  Connection.SetState(ssNoDelay);
  SetLength(fClients, high(fClients) + 2);
  fClients[high(fClients)].Num := fClientCounter;
  fClients[high(fClients)].Socket := Connection;
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Accept[' + inttostr(fClientCounter) + ']');
  End;
  ComboBox2.Items.Add(inttostr(fClients[high(fClients)].Num));
  fClientCounter := fClientCounter + 1;
End;

Procedure TForm1.OnConnect(Connection: TSynapseConnection);
Begin
  //  Connection.SetState(ssNoDelay);
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Connect.');
  End;
End;

Procedure TForm1.OnDisconnect(Connection: TSynapseConnection);
Var
  i, j: integer;
Begin
  If FIsServer Then Begin
    For i := high(fClients) Downto 0 Do Begin
      If fClients[i].Socket = Connection Then Begin
        If CheckBox4.Checked Then Begin
          memo1.Lines.Add('Lost client[' + inttostr(fClients[i].Num) + ']');
        End;
        For j := 0 To ComboBox2.Items.Count - 1 Do Begin
          If ComboBox2.Items[j] = inttostr(fClients[i].Num) Then Begin
            ComboBox2.Items.Delete(j);
            break;
          End;
        End;
        For j := i To high(fClients) - 1 Do Begin
          fClients[j] := fClients[j + 1];
        End;
        SetLength(fClients, high(fClients));
        exit;
      End;
    End;
    // Das hier tritt hoffentlich nie auf, sonst knallts gehörig
    If CheckBox4.Checked Then Begin
      memo1.Lines.Add('Lost unknown client');
    End;
    exit;
  End;
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Disconnect');
  End;
  caption := 'not connected';
  Button1.Enabled := true;
  Button6.Enabled := true;
  Button2.Enabled := false;
End;

Procedure TForm1.OnError(Const ErrorMsg: String; Connection: TSynapseConnection
  );
Begin
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Error:' + ErrorMsg);
  End;
  caption := 'not connected';
  Button1.Enabled := true;
  Button6.Enabled := true;
  Button2.Enabled := false;
End;

Procedure TForm1.Disconnect();
Begin

End;

Procedure TForm1.OnReceive(Connection: TSynapseConnection);
Const
  BufferSize = 2048;
Var
  s: String;
  a: Array Of Byte;
  i, j: Integer;
  found: boolean;
Begin
  If CheckBox1.Checked Then Begin
    a := Nil;
    setlength(a, BufferSize);
    If FIsServer Then Begin
      found := false;
      For i := 0 To high(fClients) Do Begin
        If fClients[i].Socket = Connection Then Begin
          memo1.Text := memo1.Text + LineEnding + '[' + inttostr(fClients[i].Num) + ']';
          found := true;
          break;
        End;
      End;
      If Not found Then Begin
        memo1.Text := memo1.Text + '[unknown]';
      End;
    End;
    i := Connection.Get(@a[0], BufferSize);
    While i > 0 Do Begin
      For j := 0 To i - 1 Do Begin
        memo1.Text := memo1.Text + format('%0.2X ', [a[j]]);
      End;
      If i = BufferSize Then Begin
        i := Connection.Get(@a[0], BufferSize);
      End
      Else Begin
        i := 0;
      End;
    End;
  End
  Else Begin
    If Connection.GetMessage(s) > 0 Then Begin
      (*
       * Wenn Nicht "Druckbare" Zeichen empfangen werden.
       *)
      For i := 1 To length(s) Do Begin
        If Not (s[i] In ([#01..#127])) Then Begin
          s[i] := '?';
        End;
      End;
      If FIsServer Then Begin
        For i := 0 To high(fClients) Do Begin
          If fClients[i].Socket = Connection Then Begin
            Memo1.Lines.Add('[' + inttostr(fClients[i].Num) + ']' + s);
            exit;
          End;
        End;
        Memo1.Lines.Add('[unknown]' + s);
      End
      Else Begin
        Memo1.Lines.Add(s);
      End;
    End;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Client Connect
  SynapseComponent1.UseSSL := CheckBox2.Checked;
  SynapseComponent1.SSLCertFile := edit3.text;
  SynapseComponent1.SSLKeyFile := edit4.text;
  SynapseComponent1.SSLVerify := CheckBox6.Checked;
  SynapseComponent1.SSLCAFile := edit5.text;

  If SynapseComponent1.Connect(edit1.text, strtoint(Edit2.Text)) Then Begin
    caption := 'Connected';
    Button1.Enabled := false;
    Button6.Enabled := false;
    Button2.Enabled := true;
    FIsServer := false;
    application.ProcessMessages;
  End
  Else Begin
    //Button1.Enabled := true;
    //Button2.Enabled := false;
    caption := 'not connected';
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  SynapseComponent1.Disconnect(true);
  caption := 'not connected';
  Button1.Enabled := true;
  Button6.Enabled := true;
  Button2.Enabled := false;
  fClients := Nil;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  Memo1.Clear;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  a: Array Of Byte;
  t, s: String;
  i, j: integer;
  b: Boolean;
Begin
  If SynapseComponent1.Connected Then Begin
    s := ComboBox1.Text;
    // Historie mit speichern
    b := true;
    For i := 0 To ComboBox1.Items.Count - 1 Do Begin
      If ComboBox1.Items[i] = s Then Begin
        b := false;
        break;
      End;
    End;
    If b Then ComboBox1.Items.Add(s);
    // Sendepuffer Erzeugen
    setlength(a, length(s));
    i := 1;
    j := 0;
    While i <= length(s) Do Begin
      If s[i] = '#' Then Begin // Eingabe ist als Hex oder decimalzahl
        If s[i + 1] = '#' Then Begin
          a[j] := ord('#');
          inc(i, 2);
          inc(j);
        End
        Else Begin
          inc(i);
          t := '';
          While s[i] <> ' ' Do Begin
            t := t + s[i];
            inc(i);
          End;
          inc(i);
          If pos('h', lowercase(t)) = 1 Then Begin
            a[j] := HextoByte(copy(t, 2, length(t)));
          End
          Else Begin
            a[j] := strtointdef(t, 0);
          End;
          inc(j);
        End;
      End
      Else Begin // Eingabe 1:1
        a[j] := ord(s[i]);
        inc(i);
        inc(j);
      End;
    End;
    If j > 0 Then Begin
      If FIsServer Then Begin
        If ComboBox2.ItemIndex = 0 Then Begin
          For i := 0 To high(fClients) Do Begin
            SynapseComponent1.Send(@a[0], j, fClients[i].Socket);
          End;
        End
        Else Begin
          For i := 0 To high(fClients) Do Begin
            If fClients[i].Num = strtointdef(ComboBox2.Text, -1) Then Begin
              SynapseComponent1.Send(@a[0], j, fClients[i].Socket);
              break;
            End;
          End;
        End;
      End
      Else Begin
        SynapseComponent1.Send(@a[0], j);
      End;
      setlength(a, 0);
    End;
    If CheckBox3.Checked Then ComboBox1.text := '';
  End
  Else Begin
    showmessage('you have to connect first.');
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  // Server Listen
  SynapseComponent1.UseSSL := CheckBox2.Checked;
  SynapseComponent1.SSLCertFile := edit3.text;
  SynapseComponent1.SSLKeyFile := edit4.text;
  SynapseComponent1.SSLVerify := CheckBox6.Checked;
  SynapseComponent1.SSLCAFile := edit5.text;

  If SynapseComponent1.Listen(strtoint(edit2.text)) Then Begin
    caption := 'Listen';
    Button1.Enabled := false;
    Button6.Enabled := false;
    Button2.Enabled := true;
    FIsServer := True;
    fClients := Nil;
    fClientCounter := 0;
    ComboBox2.Items.Clear;
    ComboBox2.Items.add('All');
    ComboBox2.ItemIndex := 0;
    ComboBox2.Visible := true;
  End
  Else Begin
    //Button1.Enabled := true;
    //Button2.Enabled := false;
    caption := 'not connected';
  End;
End;

Procedure TForm1.ComboBox1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button4.OnClick(Nil);
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If SynapseComponent1.Connected Then
    SynapseComponent1.Disconnect(true);
  SynapseComponent1.free;
  SynapseComponent1 := Nil;
End;

End.

