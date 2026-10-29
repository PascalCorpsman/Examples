(******************************************************************************)
(* Usage :                                                                    *)
(*        Just Bind ulanguage.pas to your Project and in every unit you want  *)
(*        to use a Resourcestring.                                            *)
(*                                                                            *)
(*       Edit ressourcestrings.include as needed ( or clean it, when no       *)
(*         Ressource Strings needed, do not delete it !! )                    *)
(*                                                                            *)
(*       Language.CurrentLanguage := xy;  setzt the application to Language xy*)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ulanguage, lclintf, lcltype;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure MemoxKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    Changed_: Boolean;
    Procedure AddListDataToListbox(Const List: TStringlist);
    Procedure StoreString(Const List: TStringlist; Item: String; Value: String);
    Function LoadString(Const List: TStringlist; Item: String): String;
  End;

Var
  Form1: TForm1;
  OpenFiles: Array Of TStringList;
  OpenFileNames: Array Of String;
  oldindex: integer;

Implementation

{$R *.lfm}

Uses LazFileUtils, lazutf8;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i: integer;
  t: TTabsheet;
  m: TMemo;
Begin
  If OpenDialog1.Execute Then Begin
    For i := PageControl1.PageCount - 1 Downto 0 Do Begin
      OpenFiles[i].free;
      openfilenames[i] := '';
      PageControl1.Pages[i].Free;
    End;
    ListBox1.Items.Clear;
    setlength(OpenFiles, OpenDialog1.Files.Count);
    setlength(openfilenames, OpenDialog1.Files.Count);
    For i := 0 To OpenDialog1.Files.Count - 1 Do Begin
      openfilenames[i] := OpenDialog1.Files[i];
      OpenFiles[i] := TStringList.create;
      OpenFiles[i].LoadFromFile(OpenDialog1.Files[i]);
      t := PageControl1.AddTabSheet;
      m := TMemo.Create(t);
      m.Parent := t;
      m.Align := alClient;
      m.Name := 'Memo' + IntToStr(i);
      m.Text := '';
      m.OnKeyUp := @MemoxKeyUp;
      t.Caption := ExtractFileNameOnly(OpenDialog1.Files[i]);
      AddListDataToListbox(OpenFiles[i]);
    End;
    PageControl1.ActivePageIndex := 0;
    oldindex := -1;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  i: integer;
  m: Tmemo;
Begin
  (* Erst mal alle Aktuell Editierten Texte Speichern *)
  If Listbox1.ItemIndex <> -1 Then Begin
    For i := 0 To PageControl1.PageCount - 1 Do Begin
      m := Tmemo(PageControl1.Pages[i].FindComponent('Memo' + inttostr(i)));
      StoreString(OpenFiles[i], ListBox1.Items[Listbox1.ItemIndex], m.Text);
    End;
  End;
  (* Speichern aller Language Files *)
  For i := 0 To high(OpenFiles) Do Begin
    OpenFiles[i].SaveToFile(OpenFileNames[i]);
  End;
  Changed_ := false;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Var
  filename: String;
Begin
  filename := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + ExtractFileNameOnly(ParamStrUTF8(0)) + '_' + ComboBox1.Text + '.lang';
  If Not FileExistsUTF8(filename) Then Begin
    ShowMessage(Error_Language_not_exists);
  End;
  Language.CurrentLanguage := filename;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If (Changed_) Then Begin
    canclose := messagebox(form1.Handle, pchar(Warning_not_Saved_yet), pchar(Warning_text), MB_YESNO Or MB_ICONWARNING) = ID_YES;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Die Default Sprachen, sollen es mehr sein, hier adden.
   *)
  ComboBox1.Items.Add('en');
  ComboBox1.Items.Add('de');
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  s: String;
  i: integer;
  b: Boolean;
Begin
  (*
   * Das Setzen der Sprache, darf nicht in OnCreate gemacht werden, da hier noch nicht
   * alle Formulare bei der Application registriert sind. In OnShow/ OnActivate ist dies der Fall.
   *)
  (*
   * Hat mam im Quelltext den Inhalt der Bezeichner geändert
   * und will, dass diese in die Haupt SprachDatei übernommen werden.
   * so kann man diesen Switch Aktivieren.
   * Zu Bedenken ist aber, dass es nur Sinn macht für die Sprache in der der Source ist !!
    *)// Zur Aktivierung den Kommentar hier schließen
  //Language.AskNameChanges := true; // nur zum Aktualisieren, sollte std. nicht an sein !!
  ComboBox1.Text := 'en';
  ComboBox1Change(Nil); // init der Sprache
  exit; // Da wir hier die Sprache aus dem Source neu Auslesen, macht es keinen Sinn den unteren Teil aus zu führen.
  // *)
  (*
   * So oder Ähnlich könnte man das Programm auf die Systemsprache einstellen..
   *)
  s := Get_System_Default_Language();
  ComboBox1.Text := 'de';
  // Gibts die Sprach Datei, wird sie geladen, gibt es diese nicht, dann
  // Wird sie mit der Englischen Version Erstellt, und der user kann sie
  // Editieren *g*
  If length(s) >= 2 Then Begin
    ComboBox1.Text := lowercase(copy(s, 1, 2));
    b := false;
    For i := 0 To ComboBox1.Items.Count - 1 Do Begin
      If lowercase(ComboBox1.Items[i]) = ComboBox1.Text Then Begin
        b := true;
      End;
    End;
    If Not b Then Begin
      ComboBox1.Items.Add(ComboBox1.Text);
    End;
  End;
  ComboBox1Change(Nil); // init der Sprache

End;

Procedure TForm1.ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  i: integer;
  m: Tmemo;
Begin
  If oldindex = ListBox1.ItemIndex Then exit;
  If oldindex <> -1 Then Begin
    For i := 0 To PageControl1.PageCount - 1 Do Begin
      m := Tmemo(PageControl1.Pages[i].FindComponent('Memo' + inttostr(i)));
      StoreString(OpenFiles[i], ListBox1.Items[oldindex], m.Text);
    End;
  End;
  oldindex := ListBox1.ItemIndex;
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    m := Tmemo(PageControl1.Pages[i].FindComponent('Memo' + inttostr(i)));
    m.Text := LoadString(OpenFiles[i], ListBox1.Items[oldindex]);
  End;
End;

Procedure TForm1.MemoxKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  Changed_ := true;
End;

Procedure TForm1.AddListDataToListbox(Const List: TStringlist);
Var
  i, j: integer;
  b: Boolean;
  pre: String;
Begin
  For i := 0 To List.Count - 1 Do Begin
    If pos('=', list[i]) <> 0 Then Begin
      pre := copy(list[i], 1, pos('=', list[i]) - 1);
      b := false;
      For j := 0 To ListBox1.Count - 1 Do Begin
        If lowercase(ListBox1.Items[j]) = lowercase(pre) Then Begin
          b := true;
          break;
        End;
      End;
      If Not b Then Begin
        ListBox1.Items.Add(pre);
      End;
    End;
  End;
End;

Procedure TForm1.StoreString(Const List: TStringlist; Item: String;
  Value: String);
Var
  i: integer;
Begin
  For i := 0 To list.Count - 1 Do Begin
    If pos(lowercase(item), lowercase(list[i])) = 1 Then Begin
      list[i] := item + '=' + encode(value);
      exit;
    End;
  End;
  list.add(item + '=' + encode(value));
End;

Function TForm1.LoadString(Const List: TStringlist; Item: String): String;
Var
  i: integer;
  suf: String;
Begin
  For i := 0 To list.Count - 1 Do Begin
    If pos(lowercase(item), lowercase(list[i])) = 1 Then Begin
      suf := copy(list[i], pos('=', list[i]) + 1, length(list[i]));
      result := decode(copy(suf, 2, length(suf) - 2));
      exit;
    End;
  End;
  result := '';
End;

End.

