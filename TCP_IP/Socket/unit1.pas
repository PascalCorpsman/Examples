(******************************************************************************)
(* Socket Demo                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Demo application demonstrates how to estabish a TCP-IP  *)
(*               connection using L-Net                                       *)
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
(*               0.02 - Umbauen, dass mittels "#" in den String auch          *)
(*                      Decimalzahlen / Hexzahlen eingebaut werden können.    *)
(*                      Automatisches Speichern einer Historie                *)
(*               0.03 - Im Server Modus zwischen den Clients unterscheiden und*)
(*                      nicht immer nur den 1. nehmen.                        *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, StdCtrls, lNet;

Type

  TConncection = Record
    Socket: TLSocket;
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
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Edit3KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent1Receive(aSocket: TLSocket);
  private
    { private declarations }
    FIsServer: Boolean;
    fClients: Array Of TConncection;
    fClientCounter: integer;
  public
    { public declarations }
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
  caption := 'Socket ver 0.03';
  edit1.text := '127.0.0.1';
  edit2.text := '1234';
  ComboBox1.text := '';
  memo1.Clear;
  fClients := Nil;
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin
  // Nur im Servermodus Akzeptieren eines neuen Clients
  aSocket.SetState(ssNoDelay);
  SetLength(fClients, high(fClients) + 2);
  fClients[high(fClients)].Num := fClientCounter;
  fClients[high(fClients)].Socket := aSocket;
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Accept[' + inttostr(fClientCounter) + ']');
  End;
  ComboBox2.Items.Add(inttostr(fClients[high(fClients)].Num));
  fClientCounter := fClientCounter + 1;
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Begin
  aSocket.SetState(ssNoDelay);
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Connect.');
  End;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Var
  i, j: integer;
Begin
  If FIsServer Then Begin
    For i := high(fClients) Downto 0 Do Begin
      If fClients[i].Socket = aSocket Then Begin
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

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  If CheckBox4.Checked Then Begin
    memo1.Lines.Add('Error:' + msg);
  End;
  caption := 'not connected';
  Button1.Enabled := true;
  Button6.Enabled := true;
  Button2.Enabled := false;
End;

Procedure TForm1.LTCPComponent1Receive(aSocket: TLSocket);
Const
  BufferSize = 16; //2048;
Var
  s: String;
  a: Array Of Byte;
  i, j: Integer;
  found: boolean;
Begin
  If CheckBox1.Checked Then Begin
    setlength(a, BufferSize);
    If FIsServer Then Begin
      found := false;
      For i := 0 To high(fClients) Do Begin
        If fClients[i].Socket = aSocket Then Begin
          memo1.Text := memo1.Text + LineEnding + '[' + inttostr(fClients[i].Num) + ']';
          found := true;
          break;
        End;
      End;
      If Not found Then Begin
        memo1.Text := memo1.Text + '[unknown]';
      End;
    End;
    i := aSocket.Get(a[0], BufferSize);
    While i > 0 Do Begin
      For j := 0 To i - 1 Do Begin
        memo1.Text := memo1.Text + format('%0.2X ', [a[j]]);
      End;
      If i = BufferSize Then Begin
        i := aSocket.Get(a[0], BufferSize);
      End
      Else Begin
        i := 0;
      End;
    End;
  End
  Else Begin
    If aSocket.GetMessage(s) > 0 Then Begin
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
          If fClients[i].Socket = aSocket Then Begin
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
  //LTCPComponent1.Host:=edit1.text;
  If LTCPComponent1.Connect(edit1.text, strtoint(Edit2.Text)) Then Begin
    caption := 'Connected';
    Button1.Enabled := false;
    Button6.Enabled := false;
    Button2.Enabled := true;
    FIsServer := false;
  End
  Else Begin
    //Button1.Enabled := true;
    //Button2.Enabled := false;
    caption := 'not connected';
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  LTCPComponent1.Disconnect(true);
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
  If LTCPComponent1.Connected Then Begin
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
            LTCPComponent1.Send(a[0], j, fClients[i].Socket);
          End;
        End
        Else Begin
          For i := 0 To high(fClients) Do Begin
            If fClients[i].Num = strtointdef(ComboBox2.Text, -1) Then Begin
              LTCPComponent1.Send(a[0], j, fClients[i].Socket);
              break;
            End;
          End;
        End;
      End
      Else Begin
        LTCPComponent1.Send(a[0], j);
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
  If LTCPComponent1.Listen(strtoint(edit2.text)) Then Begin
    caption := 'Connected';
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

Procedure TForm1.Edit3KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button4.OnClick(Nil);
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If LTCPComponent1.Connected Then
    LTCPComponent1.Disconnect(true);
End;

End.

