(******************************************************************************)
(* Send_get_file demo                                              ??.??.???? *)
(*                                                                            *)
(* Version     : 0.08                                                         *)
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
(*               0.02 - 0.05 : ??                                             *)
(*               0.06 - Beim Lesen des Letzten Chunks gabs ne AV, weil über   *)
(*                      die Streamsize gelesen wurde..                        *)
(*               0.07 - Neues Feature : Shutdown on transmit end              *)
(*                      Bugfix : Refresh Bug von label9 (Fortschrittsanzeige) *)
(*                               unter Linux                                  *)
(*                      Neues Feature : Detailreichere Fortschrittsanzeige    *)
(*                                      (immer eine Nachkomma Einheit mit)    *)
(*               0.08 - publish reenable SSL - Linux Only                     *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, lNetComponents, lcommon, lNet, lclintf, lHTTPUtil, lhttp
{$IFDEF Linux}
  , lNetSSL, openssl
{$ENDIF}
  ;

(*
 * LNet von : http://lnet.wordpress.com/download/
 *)

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LHTTPClientComponent1: TLHTTPClientComponent;
    LTCPComponent1: TLTCPComponent;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
{$IFDEF Linux}
    LSSLSessionComponent1: TLSSLSessionComponent;
    LSSLSessionComponent2: TLSSLSessionComponent;
{$ENDIF}
    Procedure Button10Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox4Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Function LHTTPClientComponent1Input(ASocket: TLHTTPClientSocket;
      ABuffer: PChar; ASize: integer): integer;
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1CanSend(aSocket: TLSocket);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent1Receive(aSocket: TLSocket);
    Procedure RadioGroup1Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

  TProgramState = (
    sSWaitForFileInfo, // Der Empfänger wartet auf die Datei Informationen
    sCWaitForFileAcknowledge, // Der Sender Wartet darauf mit der Übertragung beginnen zu dürfen
    sSReceiveData, // Der Empfänger empfängt bis die Datei vollständig ist und beendet dann mit der "Fin" Nachricht.
    sCSendData // Der Sender Sendet was er hat, und wartet dann auf die "Fin" Nachricht
    );

  TTransmitFile = Record
    Filename: String; // Dateiname, der zu sendenden / empfangenden Datei
    Filesize: int64; // Größe in Bytes der zu sendenden / empfangenden Datei
    Stream: TFileStream; // Filestream der zu sendenden / empfangenden Datei
    LastTime: DWord; // Der Letzte Zeitpunkt an dem die Dateiübertragung an die LCL visualisiert wurde
    LastPos: int64; // Die File Position am Letzten Zeitpunkt an dem die Dateiübertragung an die LCL visualisiert wurde
  End;

Const
  JunkSize = 1024 * 20; // = 20 KB Junk Size ( muss kleiner 65 KB sein, der Rest ist eigentlich Egal )
  SimplePassWd = $AA; // Invertiere jedes 2. Bit, nicht weil es sinnvoll ist, sondern eher weil es skriptkids abhalten soll.

Var
  Form1: TForm1;
  FIsserver: boolean = False; // Wenn True, dann ist die Anwendung Server
  ProgramState: TProgramState; // Zum Abspeichern des Aktuellen Programmstatus
  TransmitFile: TTransmitFile; // Alle Daten zur aktuell zu übertragenden Datei
  Transmitbuffer: TMemoryStream = Nil; // Der Puffer für die zu übertragende Datei
  isclosing: Boolean = false; // Wird beim beenden auf True gesetzt, und soll ein ShutdownLinux verhindern.

Implementation

Uses
  lazutf8, lazfileutils,
  unit2, process;

{$R *.lfm}

{ TForm1 }

Procedure ShutdownPC;
Var
  p: TProcess;
Begin
{$IFDEF LINUX}
  If FileExists('/usr/bin/dbus-send') Then Begin
    p := TProcess.Create(Nil);
    p.Options := [poStderrToOutPut];
    writeLn('Shutting down Linux...');
    p.CommandLine := 'dbus-send --system --print-reply --dest=org.freedesktop.ConsoleKit /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop';
    p.Execute;
    p.Free;
    halt(0);
  End
  Else
    writeLn('DBus command line utility not found!');
{$ENDIF}
{$IFDEF WINDOWS}
  p := TProcess.Create(Nil);
  p.CommandLine := 'shutdown -s -t 00 -f';
  p.Options := [];
  p.Execute;
  p.free;
  halt(0);
{$ENDIF}
End;

(*
 * Formatiert eine Zeitangabe in eine mit passendem Präfix
 *)

Function pretty(Value: int64): String;
Var
  suf: String;
Begin
  // 15sec
  suf := 'sec';
  If Value > 60 Then Begin
    // 10min 42sec
    suf := 'min ' + inttostr(value Mod 60) + 'sec';
    Value := Value Div 60;
  End;
  If Value > 60 Then Begin
    // 2h 30m
    suf := 'h ' + inttostr(value Mod 60) + 'm';
    Value := Value Div 60;
  End;
  If Value > 24 Then Begin
    If value Div 24 > 1 Then Begin
      // 2days 7h
      suf := 'days ' + inttostr(value Mod 24) + 'h';
    End
    Else Begin
      // 1day 14h
      suf := 'day ' + inttostr(value Mod 24) + 'h';
    End;
    Value := Value Div 24;
  End;
  Result := IntToStr(Value) + suf;
End;

(*
 * Liest aus Transmit File den Nächsten Junk aus.
 *)

Procedure Get_Next_Chunk();
Begin
  If Not assigned(TransmitFile.Stream) Then Begin
    exit;
  End;
  Transmitbuffer.Clear;
  Transmitbuffer.Position := 0;
  If (TransmitFile.Stream.Position < TransmitFile.Stream.Size) Then Begin
    If TransmitFile.Stream.Position + JunkSize <= TransmitFile.Stream.Size Then Begin
      Transmitbuffer.CopyFrom(TransmitFile.Stream, JunkSize);
    End
    Else Begin
      Transmitbuffer.CopyFrom(TransmitFile.Stream, TransmitFile.Stream.Size - TransmitFile.Stream.Position);
    End;
  End
  Else Begin
  End;
  Transmitbuffer.Position := 0;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Caption := 'Send Get File ver. 0.08, by Corpsman, support : www.Corpsman.de';
  TForm(self).Constraints.MaxHeight := TForm(self).Height;
  TForm(self).Constraints.MinHeight := TForm(self).Height;
  TForm(self).Constraints.Maxwidth := TForm(self).Width;
  TForm(self).Constraints.Minwidth := TForm(self).Width;
  edit1.Text := '9876';
  edit2.Text := '9876';
  edit3.Text := '127.0.0.1';
  edit4.Text := '';
  edit5.Text := '';
  edit6.Text := '';
  edit7.Text := '';
  edit8.Text := '';
  label3.Caption := 'not connected';
  label6.Caption := 'not connected';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  OpenDialog2.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  label10.Caption := '';
  Transmitbuffer := TMemoryStream.Create;
  Transmitbuffer.Clear;
  Transmitbuffer.Position := 0;
  TransmitFile.Stream := Nil;
  TransmitFile.Filename := '';
  TransmitFile.Filesize := 0;
{$IFDEF Windows}
  GroupBox3.Enabled := false;
  label17.caption := 'No ssl support for Windows systems, sorry ..';
{$ENDIF}
{$IFDEF Linux}
  InitSSLInterface;
  LSSLSessionComponent1 := TLSSLSessionComponent.create(self);
  LSSLSessionComponent1.Method := msTLS;
  LSSLSessionComponent2 := TLSSLSessionComponent.create(self);
  LSSLSessionComponent2.Method := msTLS;
  LSSLSessionComponent2.SSLActive := False;
  LHTTPClientComponent1.Session := LSSLSessionComponent2;
  LTCPComponent1.Session;
{$ENDIF}
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Var
  s: String;
Begin
  // On Drop file
  If high(filenames) <> -1 Then Begin
    // Warum auch immer in Edit4 nicht immer das steht was da stehen soll...
    s := filenames[0];
    Edit4.Text := S;
  End;
End;

Function TForm1.LHTTPClientComponent1Input(ASocket: TLHTTPClientSocket;
  ABuffer: PChar; ASize: integer): integer;
Var
  s: String;
Begin
  s := '';
  setlength(s, ASize);
  move(ABuffer^, s[1], ASize);
  edit5.Text := trim(s);
  Result := 0;
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin
  label10.Caption := 'Client connected';
  button7.Enabled := True;
End;

Procedure TForm1.LTCPComponent1CanSend(aSocket: TLSocket);
Var
  Data: Array[0..JunkSize - 1] Of byte;
  Sent: integer;
  oPos: int64;
  j, i: integer;
  b: byte;
  d2, d: int64;
  t: String;
Begin
  If ProgramState <> sCSendData Then
    exit;
  Repeat
    //    if not assigned(Transmitbuffer) then exit;
    If (Transmitbuffer.Size = 0) Or (Transmitbuffer.Position >= Transmitbuffer.Size) Then Begin
      Get_Next_Chunk();
    End;
    // Wenn die Datei Komplett gesendet wurde.
    If (Transmitbuffer.Size = 0) Then
      exit;
    oPos := Transmitbuffer.Position;
    j := 0;
    b := 0;
    For i := opos To Transmitbuffer.Size - 1 Do Begin
      Transmitbuffer.Read(b, SizeOf(b));
      Data[i - opos] := b Xor SimplePassWd;
      Inc(j);
    End;
    sent := LTCPComponent1.Send(Data, j, aSocket);
    Transmitbuffer.Position := opos + sent;
    If TransmitFile.LastTime + 1000 < GetTickCount Then Begin
      ProgressBar1.Position := round((TransmitFile.Stream.Position * 100) / TransmitFile.filesize);
      ProgressBar1.Hint := inttostr(ProgressBar1.Position) + '%';
      d := TransmitFile.Stream.Position - TransmitFile.LastPos;
      If d > 0 Then Begin
        t := IntToStr(d Div 1024) + ' KB / Sec';
        If d <> 0 Then Begin
          d2 := (TransmitFile.filesize - TransmitFile.Stream.Position);
          d := d2 Div d; // Das Geht aus irgend einem Grund nicht, aber warum ???
          t := t + '     Time : ' + pretty(d);
        End;
        label9.Caption := t;
      End;
      TransmitFile.lastpos := TransmitFile.Stream.Position;
      TransmitFile.LastTime := GetTickCount;
{$IFDEF LINUX}
      form1.Refresh;
{$ENDIF}
      Application.ProcessMessages;
    End;
  Until (Sent = 0);
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Begin
  // Hier ist immer nur der Sender Code
  label10.Caption := '';
  label6.Caption := 'connected';
  GroupBox2.Enabled := False;
  GroupBox3.Enabled := False;
  CheckBox3.Enabled := False;
  edit3.Enabled := False;
  edit2.Enabled := False;
  button7.Enabled := True;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  If FIsServer Then Begin
    label10.Caption := '';
  End
  Else Begin
    Button4.OnClick(Nil);
  End;
  // Wenn die Gegenstelle Unerwartet beendet, wird auch runter gefahren.
  If CheckBox5.Checked Then Begin
    // Todo : Hier sollte evtl. eine Verzögerung von x-Sekunden angezeigt werden (So wie beim Wecker, mit möglichkeit ab zu brechen).
    sleep(5000);
    Application.ProcessMessages;
    Sleep(1000);
    If isclosing Then exit; // Der versuch den Benutzer "Abbrechen" zu lassen.
    ShutdownPC;
  End;
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  ShowMessage(msg);
End;

Procedure TForm1.LTCPComponent1Receive(aSocket: TLSocket);
Var
  s, t: String;
  j, i: integer;
  filename: String;
  Data: Array[0..JunkSize - 1] Of byte;
  d2, d: int64;
Begin
  Case ProgramState Of
    sSWaitForFileInfo: Begin // Der Empfänger Wartet auf die Dateiinformationen
        // Auspacken Dateinamen und Dateigröße
        If aSocket.GetMessage(s) > 0 Then Begin
          TransmitFile.filesize := 0;
          For i := length(s) Downto 1 Do Begin
            If s[i] = '~' Then Begin
              TransmitFile.filesize := StrToInt64Def(copy(s, i + 1, length(s)), 0);
              filename := copy(s, 1, i - 1);
              break;
            End;
          End;
          If TransmitFile.filesize <> 0 Then Begin
            SaveDialog1.FileName := filename;
            If SaveDialog1.Execute Then Begin
              TransmitFile.filename := SaveDialog1.FileName;
              If assigned(TransmitFile.Stream) Then
                TransmitFile.Stream.Free;
              TransmitFile.Stream := Nil;
              Form2Option := FileOverwrite;
              If FileExistsUTF8(TransmitFile.filename) Then Begin
                Form2Option := -1;
                form2.ShowModal;
              End;
              ProgramState := sSReceiveData;
              TransmitFile.LastTime := GetTickCount;
              label9.Visible := True;
              label13.Visible := True;
              ProgressBar1.Visible := True;
              ProgressBar1.Position := 0;
              Button7.Enabled := False;
              Case (Form2Option) Of
                FileOverwrite: Begin
                    TransmitFile.Stream := TFileStream.Create(utf8tosys(TransmitFile.filename), fmcreate Or fmopenwrite);
                    asocket.SendMessage('OK');
                  End;
                FileContinue: Begin
                    TransmitFile.Stream := TFileStream.Create(utf8tosys(TransmitFile.filename), fmopenwrite);
                    TransmitFile.Stream.Position := TransmitFile.Stream.Size;
                    asocket.SendMessage('CONTINUE~' + inttostr(TransmitFile.Stream.Size));
                  End;
              End;
            End;
          End
          Else Begin
            // Ungültige daten Übertragen ..
          End;
        End;
      End;
    sCWaitForFileAcknowledge: Begin // Der Sender Wartet auf die Starte Senden Nachricht
        If aSocket.GetMessage(s) > 0 Then Begin
          If s = 'OK' Then Begin
            // Start des Filetransfer
            If assigned(TransmitFile.Stream) Then
              TransmitFile.Stream.Free;
            TransmitFile.Stream := TFileStream.Create(utf8tosys(TransmitFile.filename), fmopenread);
            ProgramState := sCSendData;
            ProgressBar1.Position := 0;
            ProgressBar1.Visible := True;
            Label9.Visible := True;
            Label13.Visible := True;
            Label9.Caption := '';
            Transmitbuffer.Clear; // Löschen von allem was bisher war
            TransmitFile.LastTime := GetTickCount;
            // Starten des Senden Vorgangs
            LTCPComponent1.OnCanSend(aSocket);
          End
          Else If pos('CONTINUE', s) = 1 Then Begin
            d := strtoint64(copy(s, pos('~', s) + 1, length(s)));
            // Start des Filetransfer
            If assigned(TransmitFile.Stream) Then
              TransmitFile.Stream.Free;
            TransmitFile.Stream := TFileStream.Create(utf8tosys(TransmitFile.filename), fmopenread);
            TransmitFile.Stream.Position := d;
            ProgramState := sCSendData;
            ProgressBar1.Visible := True;
            Label9.Visible := True;
            Label13.Visible := True;
            Label9.Caption := '';
            Transmitbuffer.Clear; // Löschen von allem was bisher war
            TransmitFile.LastTime := GetTickCount;
            // Starten des Senden Vorgangs
            LTCPComponent1.OnCanSend(aSocket);
          End
          Else Begin
            ShowMessage(s);
          End;
        End;
      End;
    sSReceiveData: Begin // Der Empfänger Empfängt Daten
        j := aSocket.Get(Data, JunkSize);
        If TransmitFile.Stream.position + j <= TransmitFile.filesize Then Begin
          For i := 0 To j - 1 Do Begin
            Data[i] := Data[i] Xor SimplePassWD;
          End;
        End
        Else Begin
          j := TransmitFile.filesize - TransmitFile.Stream.position;
          For i := 0 To j - 1 Do Begin
            Data[i] := Data[i] Xor SimplePassWD;
          End;
        End;
        TransmitFile.stream.Write(Data, j);
        If TransmitFile.Stream.position >= TransmitFile.filesize Then Begin
          // Die Übertragung ist beendet.
          TransmitFile.Stream.Free;
          TransmitFile.Stream := Nil;
          ProgramState := sSWaitForFileInfo;
          aSocket.SendMessage('FIN');
          Label9.Visible := False;
          Label13.Visible := False;
          ProgressBar1.Visible := False;
          Button7.Enabled := True;
          If CheckBox5.Checked Then Begin
            sleep(1000); // Warten, das die "Fin" Nachricht auch sicher raus geht.
            Button2Click(Nil); // Wir trennen die Verbindung
            sleep(1000); // Warten, das die "Fin" Nachricht auch sicher raus geht.
            ShutdownPC(); // Linux Runterfahren
            halt; // Sollten wir dann noch Leben, beenden wir uns selbst.
          End
          Else Begin
            ShowMessage('Finished.');
            exit;
          End;
        End;
        If TransmitFile.LastTime + 1000 < GetTickCount Then Begin
          ProgressBar1.Position := round((TransmitFile.Stream.Position * 100) / TransmitFile.filesize);
          ProgressBar1.Hint := inttostr(ProgressBar1.Position) + '%';
          d := TransmitFile.Stream.Position - TransmitFile.LastPos;
          If d > 0 Then Begin
            t := IntToStr(d Div 1024) + ' KB / Sec';
            If d <> 0 Then Begin
              d2 := (TransmitFile.filesize - TransmitFile.Stream.Position);
              d := d2 Div d;
              t := t + '     Time : ' + pretty(d);
            End;
            label9.Caption := t;
          End;
          TransmitFile.lastpos := TransmitFile.Stream.Position;
          TransmitFile.LastTime := GetTickCount;
{$IFDEF LINUX}
          form1.Refresh;
{$ENDIF}
          Application.ProcessMessages;
        End;
      End;
    sCSendData: Begin // Der Sender Hat Gesendet und wartet auf das "Fin" des Empfängers.
        If aSocket.GetMessage(s) > 0 Then Begin
          If s = 'FIN' Then Begin
            TransmitFile.Stream.Free;
            TransmitFile.Stream := Nil;
            ProgramState := sSWaitForFileInfo;
            Label9.Visible := False;
            Label13.Visible := False;
            ProgressBar1.Visible := False;
            Button7.Enabled := True;
            If CheckBox5.Checked Then Begin
              sleep(1000);
              Button4Click(Nil); // Wir Trennen die Verbindung.
              sleep(1000);
              ShutdownPC(); // Linux Runterfahren
              halt; // Sollten wir dann noch Leben, beenden wir uns selbst.
            End
            Else Begin
              ShowMessage('Finished.');
            End;
          End;
        End;
      End;
  End;
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Begin
{$IFDEF Linux}
  Case RadioGroup1.ItemIndex Of
    0: LSSLSessionComponent2.Method := msTLS;
    1: LSSLSessionComponent2.Method := msTLSv1_2;
  End;
{$ENDIF}
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // receiver Connect
  Button4.OnClick(Nil);
{$IFDEF Linux}
  If CheckBox1.Checked Then Begin
    LSSLSessionComponent2.CAFile := edit6.Text;
    LSSLSessionComponent2.KeyFile := edit7.Text;
    LSSLSessionComponent2.Password := edit8.Text;
  End;
  LSSLSessionComponent2.SSLActive := CheckBox1.Checked;
{$ENDIF}
  If CheckBox2.Checked Then Begin
    LTCPComponent1.SocketNet := LAF_INET6;
  End
  Else Begin
    LTCPComponent1.SocketNet := LAF_INET;
  End;
  If LTCPComponent1.Listen(StrToInt(Edit1.Text)) Then Begin
    label3.Caption := 'connected';
    GroupBox1.Enabled := False;
    GroupBox3.Enabled := False;
    ProgramState := sSWaitForFileInfo;
    CheckBox2.Enabled := False;
    edit1.Enabled := False;
    FIsServer := True;
  End;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Begin
  If OpenDialog2.Execute Then
    edit7.Text := systoutf8(OpenDialog2.FileName);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Receiver Disconnect
  LTCPComponent1.Disconnect(True);
  If assigned(TransmitFile.Stream) Then
    TransmitFile.Stream.Free;
  TransmitFile.Stream := Nil;
  label3.Caption := 'not connected';
  GroupBox1.Enabled := True;
  GroupBox3.Enabled := True;
  label10.Caption := '';
  CheckBox2.Enabled := True;
  edit1.Enabled := True;
  Button7.Enabled := False;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Sender Connect
  Button2.OnClick(Nil);
{$IFDEF Linux}
  If CheckBox1.Checked Then Begin
    LSSLSessionComponent2.CAFile := edit6.Text;
    LSSLSessionComponent2.KeyFile := edit7.Text;
    LSSLSessionComponent2.Password := edit8.Text;
  End;
  LSSLSessionComponent2.SSLActive := CheckBox1.Checked;
{$ENDIF}
  If CheckBox3.Checked Then Begin
    LTCPComponent1.SocketNet := LAF_INET6;
  End
  Else Begin
    LTCPComponent1.SocketNet := LAF_INET;
  End;
  If LTCPComponent1.Connect(Edit3.Text, StrToInt(Edit2.Text)) Then Begin
    FIsServer := False;
    ProgramState := sSWaitForFileInfo;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Sender Disconnect
  label6.Caption := 'not connected';
  LTCPComponent1.Disconnect(True);
  If assigned(TransmitFile.Stream) Then
    TransmitFile.Stream.Free;
  TransmitFile.Stream := Nil;
  GroupBox2.Enabled := True;
  GroupBox3.Enabled := True;
  label10.Caption := '';
  CheckBox3.Enabled := True;
  edit3.Enabled := True;
  edit2.Enabled := True;
  Button7.Enabled := False;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  // Sender Load Filename
  If OpenDialog1.Execute Then Begin
    edit4.Text := {systoutf8}(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  If FileExistsUTF8(edit4.Text) Then Begin
    ProgramState := sCWaitForFileAcknowledge;
    LTCPComponent1.SendMessage(ExtractFileName(Edit4.Text) + '~' + IntToStr(FileSize(utf8tosys(Edit4.Text))));
    TransmitFile.filename := Edit4.Text;
    TransmitFile.filesize := FileSize(utf8tosys(Edit4.Text));
    Button7.Enabled := False;
  End
  Else Begin
    Showmessage(format('Error could not open ' + LineEnding + '%s', [edit4.text]));
  End;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  aHost, aURI: String;
  aPort: word;
  Url: String;
Begin
  (*
   Auf www.Corpsman.de ist eine PHP Datei die macht folgendes :

    <?php
    echo $_SERVER['REMOTE_ADDR'];
    ?>

   *)
  url := 'https://corpsman.de/ip_info.php';
  DecomposeURL(URL, aHost, aURI, aPort);
  LHTTPClientComponent1.Host := aHost;
  LHTTPClientComponent1.URI := aURI;
  LHTTPClientComponent1.Port := aPort;
  LHTTPClientComponent1.SendRequest;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  If OpenDialog2.Execute Then
    edit6.Text := systoutf8(OpenDialog2.FileName);
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
{$IFDEF Linux}
  LSSLSessionComponent2.SSLActive := CheckBox1.Checked;
{$ENDIF}
End;

Procedure TForm1.CheckBox4Change(Sender: TObject);
Begin
  If CheckBox4.Checked Then
    edit8.PasswordChar := #0
  Else
    edit8.PasswordChar := '*';
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  CloseAction := caFree;
  If LTCPComponent1.Connected Then Begin
    CloseAction := caNone; // make sure we quit gracefuly
    LTCPComponent1.Disconnect; // call disconnect (soft)
    Timer1.Enabled := True; // if time runs out, quit ungracefully
  End;
  If CloseAction <> canone Then Begin
    If assigned(Transmitbuffer) Then
      Transmitbuffer.Free;
    Transmitbuffer := Nil;
    If assigned(TransmitFile.Stream) Then
      TransmitFile.Stream.Free;
    TransmitFile.Stream := Nil;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  isclosing := true;
End;

End.

