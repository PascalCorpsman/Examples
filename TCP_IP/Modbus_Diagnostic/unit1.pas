(******************************************************************************)
(* Modbus_diagnostic                                               ??.??.???? *)
(*                                                                            *)
(* Version     : 0.09                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Application to test MOdbus TCP / RTU connections and         *)
(*               endianes                                                     *)
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
(*               0.02 - Einfügen von uShort, uLong in der Anzeige             *)
(*               0.03 - Fixed Endianes, Forsiert Komma = Punkt                *)
(*               0.04 - Bugfix, anzeige von Daten, welche bei schreibbefehlen *)
(*                      empfangen werden.                                     *)
(*               0.05 - Erkennen, wenn eine TCP-Verbindung abbricht und       *)
(*                      entsprechend anzeigen.                                *)
(*               0.06 - Verhindern von Senden, wenn nicht verbunden.          *)
(*               0.07 - Kleinere Bugfixes im RTU-Protokoll                    *)
(*                      Support Broadcast Schreiben                           *)
(*               0.08 - Anzeigen aller Empfangenen Bytes auch wenn diese      *)
(*                       "Falsch" waren                                       *)
(*               0.09 - Umstellen auf TLTCPComponent                          *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, IniFiles, uimodbus, uiwrapper, lNet, lNetComponents;

Type

  { TMyModbus }

  TMyModbus = Class(TUIModbusServer)
  private
  public
    Procedure WriteRawBytes(Const data: Array Of Byte); override;
    Function ReceiveRawBytes(TimeOut: integer): TBytes; override;
    Function ReceiveRawBytesCnt(ByteCount: integer; TimeOut: integer = 10): TBytes; override;
  End;

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
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit23: TEdit;
    Edit24: TEdit;
    Edit25: TEdit;
    Edit26: TEdit;
    Edit27: TEdit;
    Edit28: TEdit;
    Edit29: TEdit;
    Edit3: TEdit;
    Edit30: TEdit;
    Edit31: TEdit;
    Edit32: TEdit;
    Edit33: TEdit;
    Edit34: TEdit;
    Edit35: TEdit;
    Edit36: TEdit;
    Edit37: TEdit;
    Edit38: TEdit;
    Edit39: TEdit;
    Edit4: TEdit;
    Edit40: TEdit;
    Edit41: TEdit;
    Edit42: TEdit;
    Edit43: TEdit;
    Edit44: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LTCPComponent1: TLTCPComponent;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Edit44KeyPress(Sender: TObject; Var Key: char);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
    Procedure OnTCPDisconnect(aSocket: TLSocket);
    Procedure OnDroppedFrame(Sender: TObject);
  public
    { public declarations }
    Procedure ReLoadModbusDevice();
    Procedure BlockLCL();
    Procedure UnBlockLCL();
    Procedure Visualize(StartEdit: integer; Const Data: TWords);
  End;

Var
  Form1: TForm1;
  ini: Tinifile;
  Modbus: TMyModbus = Nil;
  Connection: TInput = Nil;

Implementation

{$R *.lfm}

Uses synaser, unit2, lazutf8;

{ TMyModbus }

Procedure TMyModbus.WriteRawBytes(Const data: Array Of Byte);
Var
  s: String;
  i: Integer;
Begin
  s := 'Out: ';
  For i := 0 To high(data) Do Begin
    s := s + format('%0.2X ', [data[i]]);
  End;
  Inherited WriteRawBytes(data);
  form1.memo1.lines.add(s);
  Application.ProcessMessages;
End;

Function TMyModbus.ReceiveRawBytes(TimeOut: integer): TBytes;
Var
  s: String;
  err: Boolean;
  value: Byte;
  l: integer;
Begin
  s := 'In : ';
  result := Nil;
  setlength(result, 1024);
  l := 0;
  err := false;
  While Not err Do Begin
    value := fSendingDevice.RecvByte(err, TimeOut);
    If Not err Then Begin
      s := s + format('%0.2X ', [value]);
      result[l] := value;
      inc(l);
      If l > high(result) Then setlength(result, high(result) + 1025);
    End;
  End;
  setlength(result, l);
  form1.memo1.Append(trim(s));
End;

Function TMyModbus.ReceiveRawBytesCnt(ByteCount: integer; TimeOut: integer): TBytes;
Var
  s: String;
  i: integer;
  value: Byte;
  err: Boolean;
Begin
  result := Nil;
  s := 'In : ';
  setlength(result, ByteCount);
  err := false;
  For i := 0 To ByteCount - 1 Do Begin
    value := fSendingDevice.RecvByte(err, TimeOut);
    If err Then Begin
      setlength(result, 0);
      break;
    End
    Else Begin
      s := s + format('%0.2X ', [value]);
      result[i] := value;
    End;
  End;
  form1.memo1.Append(trim(s));
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  s: String;
Begin
  FormatSettings.DecimalSeparator := '.';
  Caption := 'Modbus Diagnostic ver. 0.09, by Corpsman, www.Corpsman.de';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;

  ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'md_settings.ini');
  edit1.text := ini.ReadString('General', 'Client', '50');
  edit2.text := ini.ReadString('General', 'Register', '0');
  s := ini.ReadString('General', 'Size', '2 = 1 Register');
  If pos(s, ComboBox1.Items.CommaText) = 0 Then
    ComboBox1.Items.Add(s);
  ComboBox1.Text := s;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  showmessage(Caption);
End;

Procedure TForm1.OnTCPDisconnect(aSocket: TLSocket);
Begin
  (*
   * Verbindungsverlust
   *)
  If assigned(Modbus) Then
    Modbus.free;
  Modbus := Nil;
  //  if assigned(Connection) then -- Mit denen Zeilen gibts ne AV. ohne gehts aber auch *g*
  //    Connection.free;
  //  Connection := Nil;
  edit3.text := 'not connected.';
End;

Procedure TForm1.OnDroppedFrame(Sender: TObject);
Begin
  Memo1.Append('Error, invalid responce.');
End;

Procedure TForm1.ReLoadModbusDevice;
Var
  p: Char;
  sb: integer;
  s, info: String;
  sl: TStringList;
Begin
  Memo1.Clear;
  Visualize(4, Nil);
  Visualize(14, Nil);
  Visualize(24, Nil);
  Visualize(34, Nil);
  If assigned(Modbus) Then
    Modbus.free;
  Modbus := Nil;
  If assigned(Connection) Then
    Connection.free;
  Connection := Nil;
  info := form2.RadioGroup1.Items[ini.ReadInteger('General', 'Protocol', 2)] + ' ';
  Case ini.ReadInteger('General', 'Protocol', 2) Of
    0: Begin // TCP
        Connection := TTCPInput.create(LTCPComponent1);
        TTCPInput(Connection).OnDisconnect := @OnTCPDisconnect;
        If TTCPInput(connection).Connect(Ini.ReadString('ModbusTCP', 'Host', '127.0.0.1'), Ini.ReadInteger('ModbusTCP', 'Port', 502)) Then Begin
          Modbus := TMyModbus.Create(Connection);
          Modbus.Mode := mmTCP;
          info := info + format('(%s %d)', [Ini.ReadString('ModbusTCP', 'Host', '127.0.0.1'), Ini.ReadInteger('ModbusTCP', 'Port', 502)]);
        End
        Else Begin
          Connection.free;
          Connection := Nil;
          info := 'not connected.';
        End;
      End;
    1: Begin // RTU over TCP
        Connection := TTCPInput.create(LTCPComponent1);
        TTCPInput(Connection).OnDisconnect := @OnTCPDisconnect;
        If TTCPInput(connection).Connect(Ini.ReadString('ModbusTCPRTU', 'Host', '127.0.0.1'), Ini.ReadInteger('ModbusTCPRTU', 'Port', 8000)) Then Begin
          Modbus := TMyModbus.Create(Connection);
          Modbus.Mode := mmRTU;
          info := info + format('(%s %d)', [Ini.ReadString('ModbusTCPRTU', 'Host', '127.0.0.1'), Ini.ReadInteger('ModbusTCPRTU', 'Port', 8000)]);
        End
        Else Begin
          Connection.free;
          Connection := Nil;
          info := 'not connected.';
        End;
      End;
    2: Begin // RTU over Seriel
        P := ini.ReadString('Seriel', 'Parity', 'N')[1];
        Case p Of
          'N': sb := SB2;
          'O', 'E': sb := SB1;
        End;
        Connection := TUartInput.create();
        sl := TStringList.Create;
        sl.CommaText := GetSerialPortNames;
        s := '';
        If sl.Count <> 0 Then s := sl[0];
        sl.free;
        If TUartInput(Connection).connect(ini.ReadString('Seriel', 'Port', s), ini.ReadInteger('Seriel', 'Baudrate', 19200), 8, P, SB, false, false) Then Begin
          Modbus := TMyModbus.Create(Connection);
          Modbus.Mode := mmRTU;
          info := info + format('(%s %d %s)', [ini.ReadString('Seriel', 'Port', s), ini.ReadInteger('Seriel', 'Baudrate', 19200), p]);
        End
        Else Begin
          Connection.free;
          Connection := Nil;
          info := 'not connected.';
        End;
      End;
  End;
  If assigned(Modbus) Then Begin
    Modbus.OnDroppedFrame := @OnDroppedFrame;
  End;
  edit3.text := info;
End;

Procedure TForm1.BlockLCL;
Begin
  Edit1.Enabled := false;
  Edit2.Enabled := false;
  ComboBox1.Enabled := false;
  Button1.Enabled := false;
  Button2.Enabled := false;
  Button3.Enabled := false;
  Button4.Enabled := false;
  Button5.Enabled := false;
  Button6.Enabled := false;
  Button7.Enabled := false;
End;

Procedure TForm1.UnBlockLCL;
Begin
  Edit1.Enabled := true;
  Edit2.Enabled := true;
  ComboBox1.Enabled := true;
  Button1.Enabled := true;
  Button2.Enabled := true;
  Button3.Enabled := true;
  Button4.Enabled := true;
  Button5.Enabled := true;
  Button6.Enabled := true;
  Button7.Enabled := true;
End;

Procedure TForm1.Visualize(StartEdit: integer; Const Data: TWords);
  Function validChar(value: Byte): Boolean;
  Begin
    result := (value >= 32) And (value < 128);
  End;
Var
  e: TEdit;
  i: Integer;
  t4: uint32;
  t8: int64;
  tu8: uint64;
  b: Byte;
  w: ^word;
  si: ^Smallint;
  ip: ^LongInt;
  lp: ^LongWord;
  fp: ^Single;
  ii64: ^int64;
  ui64: ^uint64;
  dp: ^Double;
Begin
  // Erst mal alles Löschen
  For i := 0 To 9 Do Begin
    e := TEdit(FindComponent('Edit' + inttostr(StartEdit + i)));
    e.text := '';
  End;
  If High(data) = -1 Then Begin // Ungültige Daten Raus
    exit;
  End;

  // Die Daten beliebiger Länge
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit)));
  // Hex View
  For i := 0 To high(data) Do Begin
    e.text := e.text + Format('%0.4X ', [data[i]]);
  End;
  // String
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 9)));
  For i := 0 To high(data) Do Begin
    b := (data[i] Shr 8) And $FF;
    If validChar(b) Then Begin
      e.text := e.text + chr(b);
    End
    Else Begin
      e.text := e.text + ' ';
    End;
    b := (data[i] Shr 0) And $FF;
    If validChar(b) Then Begin
      e.text := e.text + chr(b);
    End
    Else Begin
      e.text := e.text + ' ';
    End;
  End;

  // 2-Byte Typen
  // Short
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 1)));
  For i := 0 To high(data) Do Begin
    si := @data[i];
    e.text := e.text + inttostr(si^) + ' ';
  End;
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 2)));
  For i := 0 To high(data) Do Begin
    w := @data[i];
    e.text := e.text + inttostr(w^) + ' ';
  End;

  // 4-Byte Typen
  If High(data) < 1 Then Begin
    exit;
  End;
  // Signed 4-Byte  int
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 3)));
  For i := 0 To high(data) Div 2 Do Begin
    ip := @data[i * 2];
    e.text := e.text + inttostr(((ip^ And $FFFF) Shl 16) Or ((ip^ Shr 16) And $FFFF)) + ' ';
  End;
  // Unsigned 4-Byte int
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 4)));
  For i := 0 To high(data) Div 2 Do Begin
    lp := @data[i * 2];
    e.text := e.text + inttostr(((lp^ And $FFFF) Shl 16) Or ((lp^ Shr 16) And $FFFF)) + ' ';
  End;
  // float 4-Byte
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 7)));
  For i := 0 To high(data) Div 2 Do Begin
    ip := @data[i * 2];
    t4 := ip^;
    t4 := ((t4 And $FFFF) Shl 16) Or ((t4 Shr 16) And $FFFF);
    fp := @t4;
    e.text := e.text + floattostr(fp^) + ' ';
  End;

  // 8-Byte Typen
  If High(data) < 3 Then Begin
    exit;
  End;
  // Signed 8-Byte int
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 5)));
  For i := 0 To high(data) Div 4 Do Begin
    ii64 := @data[i * 4];
    t8 := ii64^;
    t8 := ((t8 And $FFFF) Shl 48) Or
      ((t8 And $FFFF0000) Shl 16) Or
      ((t8 And $FFFF00000000) Shr 16) Or
      ((t8 And $FFFF000000000000) Shr 48);
    e.text := e.text + inttostr(t8) + ' ';
  End;
  // Unsigned Signed 8-Byte int
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 6)));
  For i := 0 To high(data) Div 4 Do Begin
    ui64 := @data[i * 4];
    tu8 := ui64^;
    tu8 := ((tu8 And $FFFF) Shl 48) Or
      ((tu8 And $FFFF0000) Shl 16) Or
      ((tu8 And $FFFF00000000) Shr 16) Or
      ((tu8 And $FFFF000000000000) Shr 48);
    e.text := e.text + inttostr(tu8) + ' ';
  End;
  // Double
  e := TEdit(FindComponent('Edit' + inttostr(StartEdit + 8)));
  For i := 0 To high(data) Div 4 Do Begin
    ui64 := @data[i * 4];
    tu8 := ui64^;
    tu8 := ((tu8 And $FFFF) Shl 48) Or
      ((tu8 And $FFFF0000) Shl 16) Or
      ((tu8 And $FFFF00000000) Shr 16) Or
      ((tu8 And $FFFF000000000000) Shr 48);
    dp := @tu8;
    e.text := e.text + floattostr(dp^) + ' ';
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  ini.WriteString('General', 'Client', edit1.text);
  ini.WriteString('General', 'Register', edit2.text);
  ini.WriteString('General', 'Size', ComboBox1.Text);
  ini.free;
  ini := Nil;
  If assigned(Modbus) Then
    Modbus.free;
  Modbus := Nil;
  If assigned(Connection) Then
    Connection.free;
  Connection := Nil;
End;

Procedure TForm1.FormActivate(Sender: TObject);
Begin
  Edit44.text := '';
  ReLoadModbusDevice();
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  w: tWords;

  Function WordSwapIndex(value: integer): integer;
  Begin
    result := value + 1 - (value Mod 2) * 2;
    If result > high(w) Then Begin
      result := high(w);
    End;
  End;

Var
  wt: tWords;
  cnt: integer;
  i: Integer;
Begin
  If assigned(Modbus) Then Begin
    BlockLCL();
    memo1.clear;
    cnt := 1 Shl (ComboBox1.ItemIndex + 1);
    If strtointdef(edit1.text, 50) = 0 Then Begin
      showmessage('Error broadcast not allowed on read.');
      UnBlockLCL();
      exit;
    End;
    w := modbus.ReadHoldingRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), cnt Div 2, ini.ReadInteger('General', 'Timeout', 5000));
    If assigned(w) Then Begin
      wt := Nil;
      //w[0] := 65535;
      // 1:1 Mapping
      Visualize(4, w);
      setlength(wt, length(w));
      // Wortweise Vertauscht
      For i := 0 To high(w) Do Begin
        wt[i] := w[WordSwapIndex(i)];
      End;
      Visualize(14, wt);
      // Byteweise Vertauscht
      For i := 0 To high(w) Do Begin
        wt[i] := ((w[i] Shr 8) And $FF) Or ((w[i] Shl 8) And $FF00);
      End;
      Visualize(24, wt);
      // Wortweise und Byteweise Vertauscht
      For i := 0 To high(w) Do Begin
        wt[i] := ((w[WordSwapIndex(i)] Shr 8) And $FF) Or ((w[WordSwapIndex(i)] Shl 8) And $FF00);
      End;
      Visualize(34, wt);
    End;
    UnBlockLCL();
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  sl: TStringList;
  s: String;
Begin
  form2.ModalResult := mrNone;
  // ini to LCL
  form2.RadioGroup1.ItemIndex := ini.ReadInteger('General', 'Protocol', 2);
  form2.ComboBox3.Text := ini.ReadString('Seriel', 'Parity', 'None');
  form2.Button3Click(Nil);
  sl := Tstringlist.create;
  sl.CommaText := GetSerialPortNames;
  s := '';
  If sl.Count <> 0 Then s := sl[0];
  s := ini.ReadString('Seriel', 'Port', s);
  If sl.Count <> 0 Then Begin
    If pos(s, sl.CommaText) = 0 Then
      s := sl[0];
  End;
  sl.free;
  form2.ComboBox2.Text := s;
  s := inttostr(ini.ReadInteger('Seriel', 'Baudrate', 19200));
  If pos(s, form2.ComboBox1.Items.CommaText) = 0 Then Begin
    form2.ComboBox1.Items.Add(s);
  End;
  form2.ComboBox1.text := s;
  form2.Edit1.text := Ini.ReadString('ModbusTCP', 'Host', '127.0.0.1');
  form2.Edit2.text := inttostr(Ini.ReadInteger('ModbusTCP', 'Port', 502));
  form2.Edit3.text := Ini.ReadString('ModbusTCPRTU', 'Host', '127.0.0.1');
  form2.Edit4.text := inttostr(Ini.ReadInteger('ModbusTCPRTU', 'Port', 8000));

  form2.Edit5.text := inttostr(ini.ReadInteger('General', 'Timeout', 5000));
  // Den Dialog Starten
  form2.Refresh_Gui_Elements;
  form2.ShowModal;
  If form2.ModalResult = mrOK Then Begin
    // LCL to Ini
    ini.WriteInteger('General', 'Protocol', form2.RadioGroup1.ItemIndex);
    ini.WriteInteger('General', 'Timeout', strtoint(form2.Edit5.text));
    // Seriell
    ini.WriteString('Seriel', 'Parity', form2.ComboBox3.Text);
    ini.WriteString('Seriel', 'Port', form2.ComboBox2.Text);
    ini.WriteInteger('Seriel', 'Baudrate', strtoint(form2.ComboBox1.Text));
    // Modbus TCP
    Ini.WriteString('ModbusTCP', 'Host', form2.Edit1.text);
    Ini.WriteInteger('ModbusTCP', 'Port', strtoint(form2.Edit2.text));
    // Modbus TCP RTU
    Ini.WriteString('ModbusTCPRTU', 'Host', form2.Edit3.text);
    Ini.WriteInteger('ModbusTCPRTU', 'Port', strtoint(form2.Edit4.text));
    ReLoadModbusDevice();
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  i: word;
  w: tWords;
Begin
  If Not assigned(Modbus) Then Begin
    showmessage('Connect first.');
    exit;
  End;
  BlockLCL();
  memo1.clear;
  i := strtoint(Edit44.text);
  w := Nil;
  setlength(w, 1);
  move(i, w[0], 2);
  If Not Modbus.WriteMultipleRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), w, ini.ReadInteger('General', 'Timeout', 5000)) Then Begin
    showmessage('Some, error occoured please check communication log.');
  End;
  UnBlockLCL();
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  i: int32;
  w: tWords;
Begin
  If Not assigned(Modbus) Then Begin
    showmessage('Connect first.');
    exit;
  End;
  BlockLCL();
  memo1.clear;
  i := strtoint(Edit44.text);
  w := Nil;
  setlength(w, 2);
  w[0] := ((i Shr 16) And $FFFF);
  w[1] := (i And $FFFF);
  If Not Modbus.WriteMultipleRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), w, ini.ReadInteger('General', 'Timeout', 5000)) Then Begin
    showmessage('Some, error occoured please check communication log.');
  End;
  UnBlockLCL();
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  i: uint32;
  w: tWords;
Begin
  If Not assigned(Modbus) Then Begin
    showmessage('Connect first.');
    exit;
  End;
  BlockLCL();
  memo1.clear;
  i := strtoint(Edit44.text);
  w := Nil;
  setlength(w, 2);
  w[0] := ((i Shr 16) And $FFFF);
  w[1] := (i And $FFFF);
  If Not Modbus.WriteMultipleRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), w, ini.ReadInteger('General', 'Timeout', 5000)) Then Begin
    showmessage('Some, error occoured please check communication log.');
  End;
  UnBlockLCL();
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  i: Smallint;
  w: tWords;
Begin
  If Not assigned(Modbus) Then Begin
    showmessage('Connect first.');
    exit;
  End;
  BlockLCL();
  memo1.clear;
  i := strtoint(Edit44.text);
  w := Nil;
  setlength(w, 1);
  move(i, w[0], 2);
  If Not Modbus.WriteMultipleRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), w, ini.ReadInteger('General', 'Timeout', 5000)) Then Begin
    showmessage('Some, error occoured please check communication log.');
  End;
  UnBlockLCL();
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  i: Single;
  j: uint32 absolute i;
  w: tWords;
Begin
  If Not assigned(Modbus) Then Begin
    showmessage('Connect first.');
    exit;
  End;
  BlockLCL();
  memo1.clear;
  i := strtofloat(Edit44.text);
  w := Nil;
  setlength(w, 2);
  w[0] := ((j Shr 16) And $FFFF);
  w[1] := (j And $FFFF);
  If Not Modbus.WriteMultipleRegisters(strtointdef(edit1.text, 50), strtointdef(edit2.text, 0), w, ini.ReadInteger('General', 'Timeout', 5000)) Then Begin
    showmessage('Some, error occoured please check communication log.');
  End;
  UnBlockLCL();
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Edit44KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = ',' Then key := '.';
End;

End.

