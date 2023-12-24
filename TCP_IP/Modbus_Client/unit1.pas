(******************************************************************************)
(* Modbus Client                                                   ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
(*               0.02 - Support für Modbus RTU                                *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, IniPropStorage, umodbus, uuart, lNet;

Type

  TRegisters = Array[0..65535] Of Word;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Edit1: TEdit;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LTCPComponent1: TLTCPComponent;
    Memo1: TMemo;
    Mode: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fdefCaption: String;
    fUart: TUart;
    fModbus: TModbusClient;
    Procedure OnReceivedData(Sender: TObject; Id, FunctionCode,
      StartAddress: Integer; Const Data: Array Of Word);
    Procedure OnRequestData(Sender: TObject; Id, FunctionCode, StartAddress,
      Count: Integer; Var Data: Array Of Word);
    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);

    Procedure RecreateAsTCP();
    Procedure RecreateAsUart();
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  Registers: TRegisters;
  formShowOnce: Boolean = true;

Implementation

Uses unit2;

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: integer;
Begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  edit1.text := '502';
  fdefcaption := 'Modbus slave ver. 0.02';
  caption := fdefCaption;
  fModbus := Nil;
  fUart := Nil;
  RecreateAsTCP();
  For i := Low(Registers) To High(Registers) Do Begin
    Registers[i] := 0;
  End;
  Button2.Enabled := false;
  Memo1.Clear;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If formShowOnce Then Begin
    formShowOnce := false;
    // die Session Property sachen, die Abhängig sind müssen hier von hand gemacht werden, weil sie im OnCreate noch nicht zur Verfügung stehen
    ComboBox1Change(Nil);
    Button9.Click;
    ComboBox2.Text := IniPropStorage1.ReadString('ComboBox2_Text', ComboBox2.Text);
  End;
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  Memo1.Append('Error: ' + msg);
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  // Sonst kriegen wir nicht mit, wenn Frames verworfen werden
  If assigned(fModbus) And (Button2.Enabled) Then Begin
    Caption := format('Handled %d, Dropped %d', [fModbus.HandledFrames, fModbus.DroppedFrames]);
  End;
End;

Procedure TForm1.RecreateAsTCP;
Begin
  If assigned(fModbus) Then fModbus.Free;
  If assigned(fUart) Then Begin
    fUart.Terminate;
    While fUart.IsRunning Do Begin
      CheckSynchronize(1);
    End;
    fUart.free;
    fUart := Nil;
  End;
  If LTCPComponent1.Connected Then LTCPComponent1.Disconnect();
  fModbus := TModbusClient.Create(LTCPComponent1);
  fModbus.OnReceiveData := @OnReceivedData;
  fModbus.OnRequestData := @OnRequestData;
  fModbus.OnAccept := @OnAccept;
  fModbus.OnDisconnect := @OnDisconnect;
End;

Procedure TForm1.RecreateAsUart;
Begin
  If assigned(fModbus) Then fModbus.Free;

  If assigned(fUart) Then Begin
    fUart.Terminate;
    While fUart.IsRunning Do Begin
      CheckSynchronize(1);
    End;
    fUart.free;
    fUart := Nil;
  End;

  fUart := TUart.Create(true);
  fUart.FreeOnTerminate := false;
  fModbus := TModbusClient.Create(fUart); // Setzt den OnReceive Event Handler
  fUart.Start;
  While Not fUart.IsRunning Do Begin // Warten bis der Thread Empfangsbereit ist.
    sleep(1);
  End;
  fModbus.OnReceiveData := @OnReceivedData;
  fModbus.OnRequestData := @OnRequestData;
  Button9.Click;
End;

Procedure TForm1.OnReceivedData(Sender: TObject; Id, FunctionCode,
  StartAddress: Integer; Const Data: Array Of Word);
Var
  s: String;
  i: Integer;
Begin
  // TODO: Die ID wird überhaupt nicht ausgewertet
  Caption := format('Handled %d, Dropped %d', [fModbus.HandledFrames, fModbus.DroppedFrames]);
  Case FunctionCode Of
    6, 16: Begin
        s := 'FC: ' + inttostr(FunctionCode) + ', Addr: ' + inttostr(StartAddress) + ' Data:';
        For i := 0 To High(Data) Do Begin
          s := s + ' ' + format('%0.4X', [Data[i]]);
          Registers[StartAddress + i] := Data[i];
        End;
        Memo1.Append(s);
      End;
  End;
End;

Procedure TForm1.OnRequestData(Sender: TObject; Id, FunctionCode, StartAddress,
  Count: Integer; Var Data: Array Of Word);
Var
  i: Integer;
  s: String;
Begin
  // TODO: Die ID wird überhaupt nicht ausgewertet
  s := 'FC: ' + inttostr(FunctionCode) + ', Addr: ' + inttostr(StartAddress) + ' Data:';
  Caption := format('Handled %d, Dropped %d', [fModbus.HandledFrames, fModbus.DroppedFrames]);
  For i := 0 To Count - 1 Do Begin
    Data[i] := Registers[StartAddress + i];
    s := s + ' ' + format('%0.4X', [Data[i]]);
  End;
  Memo1.Append(s);
End;

Procedure TForm1.OnAccept(aSocket: TLSocket);
Begin
  Memo1.Append('Accept incomming connection.');
End;

Procedure TForm1.OnDisconnect(aSocket: TLSocket);
Begin
  Memo1.Append('Lost connection.');
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  info: String;
Begin
  info := 'not connected.';
  Memo1.Clear;
  Case ComboBox1.ItemIndex Of
    0: Begin // Modbus TCP
        If fModbus.TCPListen(strtointdef(Edit1.Text, 502)) Then Begin
          info := 'connected';
          Button1.Enabled := false;
          Button2.Enabled := true;
        End;
      End;
    1: Begin // Modbus RTU
        If ComboBox2.Text <> '' Then Begin
          If fModbus.UartConnect(ComboBox2.Text, strtoint(ComboBox3.Text), ComboBox4.Text[1]) Then Begin
            info := 'connected';
            Button1.Enabled := false;
            Button2.Enabled := true;
          End;
        End;
      End;
  End;
  form1.caption := fdefCaption + ' ' + Info;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  Button1.Enabled := true;
  Button2.Enabled := false;
  fModbus.disconnect;
  Caption := fdefCaption;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  j, i: Integer;
  f: TFileStream;
Begin
  // Save Registerdump
  If SaveDialog1.Execute Then Begin
    f := TFileStream.Create(SaveDialog1.FileName, fmCreate Or fmOpenWrite);
    j := 1; // Version
    f.Write(j, sizeof(j));
    // Inhalt der Register
    For i := Low(Registers) To High(Registers) Do Begin
      f.Write(Registers[i], sizeof(Registers[i]));
    End;
    f.Free;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  l, j, i: Integer;
  f: TFileStream;
  s: String;
Begin
  // Load Registerdump
  If OpenDialog1.Execute Then Begin
    Memo1.Clear;
    f := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    j := 0; // Version
    f.read(j, sizeof(j));
    // Inhalt der Register
    l := -1;
    s := '';
    For i := Low(Registers) To High(Registers) Do Begin
      Registers[i] := 0;
      f.read(Registers[i], sizeof(Registers[i]));
      If Registers[i] <> 0 Then Begin
        If l = -1 Then Begin
          s := s + ' Registers [' + inttostr(i) + '..';
        End;
        l := i;
      End
      Else Begin
        If l <> -1 Then Begin
          s := s + inttostr(l) + ']'#13#10;
          l := -1;
        End;
      End;
    End;
    f.Free;
    Memo1.Append('Data in : '#13#10 + s);
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  memo1.clear;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  showmessage(
    'This application provides a modbus slave holding 65536 Registers' + LineEnding +
    'Use e.g. Modbus Diagnostic tool to read and/or write registers.'
    );
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  form2.show;
  Form2.Button2Click(Nil);
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  ComboBox2.Items.CommaText := GetSerialPortNames();
  If ComboBox2.Items.Count <> 0 Then Begin
    ComboBox2.ItemIndex := 0;
  End;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Begin
  Button2.Click; // Disconnect
  // Modbus TCP
  Edit1.Visible := ComboBox1.ItemIndex = 0;

  // Modbus RTU
  ComboBox2.Visible := ComboBox1.ItemIndex = 1;
  Button9.Visible := ComboBox1.ItemIndex = 1;
  label2.Visible := ComboBox1.ItemIndex = 1;
  ComboBox3.Visible := ComboBox1.ItemIndex = 1;
  label3.Visible := ComboBox1.ItemIndex = 1;
  ComboBox4.Visible := ComboBox1.ItemIndex = 1;

  Case ComboBox1.ItemIndex Of
    0: RecreateAsTCP();
    1: RecreateAsUart();
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(fModbus) Then fModbus.Free;
  fModbus := Nil;
  If assigned(fUart) Then Begin
    fUart.Terminate;
    While fUart.IsRunning Do Begin
      CheckSynchronize(1);
    End;
    fUart.free;
    fUart := Nil;
  End;
End;

End.

