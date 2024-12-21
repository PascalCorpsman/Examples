Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  lNetComponents, lNet, uMQTTbroker;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
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
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent1Receive(aSocket: TLSocket);
  private
    fMQTTConnected: Boolean;
    rBuffer: Array Of Byte;
    Function TryExtractaPacket(Out aPacket: TMQTTPacket): Boolean;

    Procedure HandlePacket(Const aPacket: TMQTTPacket);

    Procedure HandleConnAckPacket(Const aPacket: TMQTTPacket);
    Procedure HandlePingResp(Const aPacket: TMQTTPacket);
    Procedure HandlePuBack(Const aPacket: TMQTTPacket);
    Procedure HandlePUBREC(Const aPacket: TMQTTPacket);


    Procedure ResetLCL;
  public
    Procedure Log(aText: String);

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *)
  caption := 'MQTT Publisher ver. 0.01';
  edit1.text := '127.0.0.1';
  edit2.text := '1883';

  edit3.text := 'watermeter/main/value';
  edit4.text := '1234';
  edit5.text := '37.1234';
  Memo1.Clear;
  ResetLCL();
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  ResetLCL;
  Log('Lost connection');
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  Log(msg);
  ResetLCL;
End;

Procedure TForm1.LTCPComponent1Receive(aSocket: TLSocket);
Const
  BuffSize = 2048;
Var
  Buffer: Array[0..2047] Of byte;
  BytesCount, i: Integer;
  aIndex: SizeInt;
  aPacket: TMQTTPacket;
Begin
  // Wir müssen immer lesen, was der Socket gelesen hat, sonst wirds ganz unschön..
  BytesCount := aSocket.Get(Buffer[0], BuffSize);
  While BytesCount <> 0 Do Begin
    // TODO: Das könnte man Performanter implementieren ...
    // Die Daten an den Aktuellen Lese Speicher anfügen
    aIndex := length(rBuffer);
    setlength(rBuffer, aIndex + BytesCount);
    For i := 0 To BytesCount - 1 Do Begin
      rBuffer[aIndex + i] := Buffer[i];
    End;
    BytesCount := aSocket.Get(Buffer[0], BuffSize);
    // Nach jedem Lesen Versuchen wir ein MQTT Packet zu empfangen.
    If TryExtractAPacket(aPacket) Then Begin
      HandlePacket(aPacket);
    End;
  End;
End;

Function TForm1.TryExtractaPacket(Out aPacket: TMQTTPacket): Boolean;
Var
  RemainingLength, i, Multiplier, aPayloadStartIndex: Integer;
  EncodedByte: uint8;
Begin
  result := false;
  // Der Fixed Header hat auf jeden Fall 2 Byte + 1 Byte Payload Länge
  If length(rBuffer) < 2 Then exit;
  // Decoding the Remaining Length
  aPayloadStartIndex := 1;
  Multiplier := 1;
  RemainingLength := 0;
  Repeat
    If aPayloadStartIndex > high(rBuffer) Then exit; // Die Länge der Remaining Bytes wurde noch nicht Vollständig eingelesen -> Raus
    EncodedByte := rBuffer[aPayloadStartIndex];
    RemainingLength := RemainingLength + (EncodedByte And $7F) * Multiplier;
    Multiplier := Multiplier * 128;
    If (Multiplier > 128 * 128 * 128) Then Begin
      // TODO: ggf. könnte man hier auch einfach nur den ClientIndex Platt machen und alles Resetten ?
      Raise exception.create('Error invalid multiplier.');
    End;
    inc(aPayloadStartIndex); // Switch to next Byte
  Until ((EncodedByte And $80) = 0);
  // Nachricht vollständig empfangen ?
  If length(rBuffer) < RemainingLength + 2 Then exit;
  // Nachricht ist Vollständig -> Auslesen und den Empfangspuffer entsprechend verkürzen
  result := true;
  aPacket.ControlPacketType := (rBuffer[0] Shr 4) And $F;
  aPacket.Flags := (rBuffer[0]) And $F;
  setlength(aPacket.Payload, RemainingLength);
  For i := 0 To RemainingLength - 1 Do Begin
    aPacket.Payload[i] := rBuffer[i + aPayloadStartIndex];
  End;
  For i := RemainingLength + aPayloadStartIndex To length(rBuffer) - 1 Do Begin
    rBuffer[i - RemainingLength - aPayloadStartIndex] := rBuffer[i];
  End;
  setlength(rBuffer, length(rBuffer) - aPayloadStartIndex - RemainingLength);
End;

Procedure TForm1.HandlePacket(Const aPacket: TMQTTPacket);
Var
  s: String;
  i: Integer;
Begin
  Case aPacket.ControlPacketType Of
    CPT_CONNACK: HandleConnAckPacket(aPacket);
    CPT_PINGRESP: HandlePingResp(aPacket);
    CPT_PUBACK: HandlePuBack(aPacket);
    CPT_PUBREC: HandlePUBREC(aPacket);
  Else Begin
      // Alles was wir nicht kennen wird raus geloggt
      s := '';
      For i := 0 To high(aPacket.Payload) Do Begin
        s := s + format(' %0.2X', [aPacket.Payload[i]]);
      End;
      Log('Unknown package: ' + format('%d %d [%d]:' + s, [aPacket.ControlPacketType, aPacket.Flags, length(aPacket.Payload)]));
    End;
  End;
End;

Procedure TForm1.HandleConnAckPacket(Const aPacket: TMQTTPacket);
Begin
  // Login Accepted
  If aPacket.Payload[1] = $00 Then Begin
    Button2.Enabled := false;
    Button4.Enabled := true;
    Button7.Enabled := true;
    log('Log in, accepted');
  End
  Else Begin
    Log('Error: regret to log in, reason: ' + inttostr(aPacket.Payload[1]));
  End;
End;

Procedure TForm1.HandlePingResp(Const aPacket: TMQTTPacket);
Begin
  log('Got Ping Resp');
End;

Procedure TForm1.HandlePuBack(Const aPacket: TMQTTPacket);
Var
  id: uint16;
Begin
  id := (aPacket.Payload[0] Shl 8) Or aPacket.Payload[1];
  log('Got PuBack for ' + inttostr(id));
End;

Procedure TForm1.HandlePUBREC(Const aPacket: TMQTTPacket);
Var
  id: uint16;
Begin
  id := (aPacket.Payload[0] Shl 8) Or aPacket.Payload[1];
  log('Got PubRec for ' + inttostr(id));
End;

Procedure TForm1.ResetLCL;
Begin
  button1.Enabled := true;
  fMQTTConnected := false;
  button2.Enabled := false;
  Button4.Enabled := false;
  Button5.Enabled := false;
  Button7.Enabled := false;
End;

Procedure TForm1.Log(aText: String);
Begin
  If CheckBox3.Checked Then Begin
    Memo1.Lines.Add(FormatDateTime('HH:NN:SS', Now) + ' ' + aText);
  End
  Else Begin
    Memo1.Lines.Add(aText);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If LTCPComponent1.Connected Then Begin
    LTCPComponent1.Disconnect(true);
  End;
  If LTCPComponent1.Connect(Edit1.Text, StrToIntDef(edit2.text, 1883)) Then Begin
    fMQTTConnected := false;
    button1.Enabled := false;
    button2.Enabled := true;
    Button5.Enabled := true;
    rBuffer := Nil;
  End
  Else Begin
    showmessage('Error could not connect.');
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  a: Array Of Byte;
Begin
  If Not LTCPComponent1.Connected Then ResetLCL;
  a := Nil;
  setlength(a, 12);
  a[0] := CPT_CONNECT Shl 4;
  a[1] := length(a) - 2; // Das Geht nur, wenn length(a) < 130 ist !
  a[2] := 0; // String of Len 4
  a[3] := 4; // String of Len 4
  a[4] := ORD('M');
  a[5] := ORD('Q');
  a[6] := ORD('T');
  a[7] := ORD('T');
  a[8] := 4; // Protocol Version
  a[9] := 2; // Connect Flags [Clean Session]
  a[10] := $7F; // Keep Alive
  a[11] := $FF; // Keep Alive
  LTCPComponent1.Send(a[0], length(a));
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  memo1.clear;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  a: Array Of Byte;
  l, i: Integer;
  id: Integer;

Begin
  // Publish
  If Not LTCPComponent1.Connected Then exit;
  a := Nil;
  Case RadioGroup1.ItemIndex Of
    0: setlength(a, 2 + 2 + length(edit3.text) + 2);
    1, 2: setlength(a, 2 + 2 + length(edit3.text) + 2 + length(Edit5.Text));
  End;
  // Header + Flags bauen
  a[0] := CPT_PUBLISH Shl 4;
  If CheckBox1.Checked Then inc(a[0], 8);
  inc(a[0], RadioGroup1.ItemIndex * 2);
  If CheckBox2.Checked Then inc(a[0], 1);
  If length(a) >= 130 Then Begin
    showmessage('Error, content to "Long", do not send due to missing implementation!');
    exit;
  End;
  a[1] := length(a) - 2; // Das Geht nur, wenn length(a) < 130 ist !
  l := length(edit3.text);
  a[2] := (l Shr 8) And $FF;
  a[3] := l And $FF;
  For i := 1 To l Do Begin
    a[3 + i] := ord(edit3.text[i]);
  End;
  id := strtointdef(edit4.text, 0);
  id := min(65535, id);
  id := max(0, id);
  a[4 + l] := (id Shr 8) And $FF;
  a[4 + l + 1] := id And $FF;
  If RadioGroup1.ItemIndex > 0 Then Begin
    For i := 1 To length(Edit5.Text) Do Begin
      a[4 + l + 1 + i] := ord(edit5.text[i]);
    End;
  End;
  LTCPComponent1.Send(a[0], length(a));
  Case RadioGroup1.ItemIndex Of
    0: log('Publish: ' + Edit3.Text);
    1, 2: log('Publish: ' + Edit3.Text + ' = ' + Edit5.text);
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  If LTCPComponent1.Connected Then Begin
    LTCPComponent1.Disconnect(true);
  End;
  ResetLCL;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  a: Array Of Byte;
Begin
  // Send Ping
  If Not LTCPComponent1.Connected Then exit;
  a := Nil;
  setlength(a, 2);
  a[0] := CPT_PINGREQ Shl 4;
  a[1] := 0;
  LTCPComponent1.Send(a[0], length(a));
  Log('Send Ping');
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If LTCPComponent1.Connected Then Begin
    LTCPComponent1.Disconnect(true);
  End;
End;

End.

