Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, SynEdit, SynHighlighterPas, delfortypes, delforinterf;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    GroupBox1: TGroupBox;
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
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
  private
    Procedure SettingsToLCL(Const Settings: TSettings);
    Function LCLToSettings(): TSettings;
  public

  End;

Var
  Form1: TForm1;
  Settings: TSettings;

Implementation

{$R *.lfm}

Procedure SetDefault(Var Settings: TSettings);
Begin
  With settings Do Begin
    // Lines
    WrapLines := False;
    WrapPosition := 81;
    RemoveDoubleBlank := true;
    BlankProc := True;
    BlankSubProc := False;
    FeedRoundBegin := UnChanged;
    FeedEachUnit := False;
    FeedAfterVar := False;
    FeedAfterThen := False;
    NoFeedBeforeThen := False;
    FeedElseIf := False;
    FeedBeforeEnd := true;
    FeedAfterSemiColon := False;

    // Indentation
    ChangeIndent := True;
    SpacePerIndent := 2;
    indentBegin := False;
    IndentComments := False;
    IndentCompDirectives := False;
    NoIndentElseIf := False;
    IndentTryElse := False;
    IndentCaseElse := False;
    IndentTry := False;

    // Space
    SpaceOperators := spBoth;
    SpaceEqualOper := spBoth;
    SpaceColon := spAfter;
    SpaceSemiColon := spAfter;
    SpaceComma := spAfter;
    SpaceLeftBr := spNone;
    SpaceRightBr := spNone;
    SpaceLeftHook := spNone;
    SpaceRightHook := spNone;

    // Case
    ReservedCase := rfFirstUp;
    StandDirectivesCase := rfFirstUp;
    UpperCompDirectives := True;
    UpperNumbers := True;
    IdentifierCaseing := idUnchanged;

    // Misc
    AlignComments := False;
    AlignCommentPos := 40;
    AlignVar := False;
    AlignVarPos := 20;
    SupportCOperands := false;

    // Was die folgenden Parameter machen ist nicht wirklich verstanden
    ExceptSingle := False;
    FillNewWords := fmUnchanged;
    StrCopy(StartCommentOut, '{(*}');
    StrCopy(EndCommentOut, '{*)}');
  End;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *           0.02 = Settings ebenfalls mit Splitter
   *           0.03 = erster Support für C-operanden
   *                  Speicherndialog eingefügt
   *                  Load Source From Disk repariert
   *           0.04 = Formatierungen für Identifier eingefügt (default aus)
   *)
  caption := 'Formater Settings viewer, ver. 0.04, by Corpsman';
  Formatter_Create;
  panel1.caption := '';
  GroupBox1.Align := alLeft;
  Splitter2.Align := alleft;
  Panel1.Align := alClient;
  SynEdit1.Align := alLeft;
  Splitter1.Align := alLeft;
  SynEdit2.Align := alClient;
  SetDefault(settings);
  SettingsToLCL(settings);
  SynEdit1Change(Nil);
  PageControl1.ActivePageIndex := 0;
  // Zum Create Zeitpunkt sind die Breiten der oben via Alignment definierten Elemente noch nicht "Berechnet"
  // Deswegen kann hier nicht Panel1.width div2 genommen werden !
  SynEdit1.Width := (form1.Width - GroupBox1.Width) Div 2;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  settings := LCLToSettings();
  // Wenn das nicht Doppelt ist, dann wird das Casing der Identifier nicht korrekt übernommen, nur warum ist noch nicht verstanden
  SynEdit1Change(Nil);
  SynEdit1Change(Nil);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  f: TFileStream;
Begin
  // Load Settings
  If OpenDialog2.Execute Then Begin
    f := TFileStream.Create(OpenDialog2.FileName, fmOpenRead);
    FillChar(Settings, sizeof(Settings), 0);
    f.Read(Settings, sizeof(Settings));
    f.free;
    SettingsToLCL(Settings);
    Button1.Click;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  f: TFileStream;
  s: TSettings;
Begin
  // Save Settings
  If SaveDialog2.Execute Then Begin
    s := LCLToSettings();
    f := TFileStream.Create(SaveDialog2.FileName, fmCreate Or fmOpenWrite);
    f.Write(s, sizeof(s));
    f.free;
  End;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  Formatter_Destroy;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Lade zu formatierende Datei
  If OpenDialog1.Execute Then Begin
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
    Button1.Click;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Speichere formatierte Datei
  If SaveDialog1.Execute Then Begin
    SynEdit2.Lines.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.SynEdit1Change(Sender: TObject);
Var
  Pt: PChar;
  CursorPos: TPoint;
  TopLine: integer;
Begin
  CursorPos := synedit2.CaretXY;
  topline := SynEdit2.TopLine;
  // Do Code Format.
  Formatter_Clear;
  Pt := @synedit1.Lines.Text[1];
  Try
    Formatter_SetTextStr(Pt);
    // Formatieren des Textes
    Formatter_Parse(@Settings, sizeof(Tsettings));
    // Zurückschreiben des Textes
    Pt := Formatter_GetTextStr;
    synedit2.Lines.Text := Pt;
  Except
    On av: Exception Do Begin
      ShowMessage(av.Message);
    End;
  End;
  synedit2.CaretXY := CursorPos;
  SynEdit2.TopLine := topline;
End;

Procedure TForm1.SettingsToLCL(Const Settings: TSettings);
  Function SpaceBeforeToIndex(value: TSpaceBefore): integer;
  Begin
    result := 0;
    Case value Of
      spNone: result := 0;
      spBefore: result := 1;
      spAfter: result := 2;
      spBoth: result := 3;
    End;
  End;

  Function CaseToIndex(Value: TCase): integer;
  Begin
    result := 0;
    Case value Of
      rfLowerCase: result := 0;
      rfUpperCase: result := 1;
      rfFirstUp: result := 2;
      rfUnchanged: result := 3;
    End;
  End;

  Function IdentifierCaseToIndex(Value: TIdentifierCase): integer;
  Begin
    result := 0;
    Case value Of
      idSameAsFirstOccurence: result := 0;
      idLowerCase: result := 1;
      idUpperCase: result := 2;
      idFirstUp: result := 3;
      idUnchanged: result := 4;
    End;
  End;

Begin
  // Lines
  CheckBox1.Checked := settings.WrapLines;
  edit1.text := inttostr(settings.WrapPosition);
  CheckBox5.Checked := Settings.RemoveDoubleBlank;
  CheckBox4.Checked := Settings.BlankProc;
  CheckBox6.Checked := Settings.BlankSubProc;
  Case settings.FeedRoundBegin Of
    Unchanged: ComboBox1.ItemIndex := 0;
    Hanging: ComboBox1.ItemIndex := 1;
    NewLine: ComboBox1.ItemIndex := 2;
  End;
  CheckBox16.Checked := settings.FeedEachUnit;
  CheckBox21.Checked := settings.FeedAfterVar;
  CheckBox14.Checked := settings.FeedAfterThen;
  CheckBox19.Checked := settings.NoFeedBeforeThen;
  CheckBox17.Checked := settings.FeedElseIf;
  CheckBox18.Checked := settings.FeedBeforeEnd;
  CheckBox20.Checked := settings.FeedAfterSemiColon;

  // Indentation
  CheckBox7.Checked := settings.ChangeIndent;
  edit4.text := inttostr(settings.SpacePerIndent);
  CheckBox8.Checked := settings.indentBegin;
  CheckBox9.Checked := settings.IndentComments;
  CheckBox10.Checked := settings.IndentCompDirectives;
  CheckBox15.Checked := settings.NoIndentElseIf;
  CheckBox11.Checked := settings.IndentTryElse;
  CheckBox12.Checked := settings.IndentCaseElse;
  CheckBox13.Checked := settings.IndentTry;

  // Space
  ComboBox2.ItemIndex := SpaceBeforeToIndex(settings.SpaceOperators);
  ComboBox3.ItemIndex := SpaceBeforeToIndex(settings.SpaceEqualOper);
  ComboBox4.ItemIndex := SpaceBeforeToIndex(settings.SpaceColon);
  ComboBox5.ItemIndex := SpaceBeforeToIndex(settings.SpaceSemiColon);
  ComboBox6.ItemIndex := SpaceBeforeToIndex(settings.SpaceComma);
  ComboBox7.ItemIndex := SpaceBeforeToIndex(settings.SpaceLeftBr);
  ComboBox8.ItemIndex := SpaceBeforeToIndex(settings.SpaceRightBr);
  ComboBox9.ItemIndex := SpaceBeforeToIndex(settings.SpaceLeftHook);
  ComboBox10.ItemIndex := SpaceBeforeToIndex(settings.SpaceRightHook);

  // Case
  ComboBox11.ItemIndex := CaseToIndex(settings.ReservedCase);
  ComboBox12.ItemIndex := CaseToIndex(settings.StandDirectivesCase);
  CheckBox22.Checked := settings.UpperCompDirectives;
  CheckBox23.Checked := settings.UpperNumbers;
  ComboBox14.ItemIndex := IdentifierCaseToIndex(Settings.IdentifierCaseing);

  // Misc
  CheckBox2.Checked := settings.AlignComments;
  edit2.text := inttostr(settings.AlignCommentPos);
  CheckBox3.Checked := Settings.AlignVar;
  edit3.text := inttostr(Settings.AlignVarPos);
  // Was die folgenden Parameter machen ist nicht wirklich verstanden
  CheckBox24.Checked := Settings.ExceptSingle;
  Case settings.FillNewWords Of
    fmUnchanged: ComboBox13.ItemIndex := 0;
    fmAddNewWord: ComboBox13.ItemIndex := 1;
    fmUse: ComboBox13.ItemIndex := 2;
    fmExceptDirect: ComboBox13.ItemIndex := 3;
    fmAddUse: ComboBox13.ItemIndex := 4;
    fmAddUseExcept: ComboBox13.ItemIndex := 5;
  End;
  edit5.text := settings.StartCommentOut;
  edit6.text := settings.EndCommentOut;

  CheckBox25.Checked := settings.SupportCOperands;
End;

Function TForm1.LCLToSettings(): TSettings;
  Function IndexToSpaceBefore(value: integer): TSpaceBefore;
  Begin
    result := spNone;
    Case value Of
      0: result := spNone;
      1: result := spBefore;
      2: result := spAfter;
      3: result := spBoth;
    End;
  End;

  Function IndexToCase(Value: integer): TCase;
  Begin
    result := rfLowerCase;
    Case value Of
      0: result := rfLowerCase;
      1: result := rfUpperCase;
      2: result := rfFirstUp;
      3: result := rfUnchanged;
    End;
  End;

  Function IndexToIdentifierCase(Value: integer): TIdentifierCase;
  Begin
    result := idSameAsFirstOccurence;
    Case value Of
      0: result := idSameAsFirstOccurence;
      1: result := idLowerCase;
      2: result := idUpperCase;
      3: result := idFirstUp;
      4: result := idUnchanged;
    End;

  End;

Begin
  // Lines
  result.WrapLines := CheckBox1.Checked;
  result.WrapPosition := strtointdef(edit1.text, 81);
  result.RemoveDoubleBlank := CheckBox5.Checked;
  result.BlankProc := CheckBox4.Checked;
  result.BlankSubProc := CheckBox6.Checked;
  Case ComboBox1.ItemIndex Of
    0: result.FeedRoundBegin := Unchanged;
    1: result.FeedRoundBegin := Hanging;
    2: result.FeedRoundBegin := NewLine;
  End;
  result.FeedEachUnit := CheckBox16.Checked;
  result.FeedAfterVar := CheckBox21.Checked;
  result.FeedAfterThen := CheckBox14.Checked;
  result.NoFeedBeforeThen := CheckBox19.Checked;
  result.FeedElseIf := CheckBox17.Checked;
  result.FeedBeforeEnd := CheckBox18.Checked;
  result.FeedAfterSemiColon := CheckBox20.Checked;

  // Indentation
  result.ChangeIndent := CheckBox7.Checked;
  result.SpacePerIndent := strtointdef(edit4.text, 2);
  result.indentBegin := CheckBox8.Checked;
  result.IndentComments := CheckBox9.Checked;
  result.IndentCompDirectives := CheckBox10.Checked;
  result.NoIndentElseIf := CheckBox15.Checked;
  result.IndentTryElse := CheckBox11.Checked;
  result.IndentCaseElse := CheckBox12.Checked;
  result.IndentTry := CheckBox13.Checked;

  // Space
  result.SpaceOperators := IndexToSpaceBefore(ComboBox2.ItemIndex);
  result.SpaceEqualOper := IndexToSpaceBefore(ComboBox3.ItemIndex);
  result.SpaceColon := IndexToSpaceBefore(ComboBox4.ItemIndex);
  result.SpaceSemiColon := IndexToSpaceBefore(ComboBox5.ItemIndex);
  result.SpaceComma := IndexToSpaceBefore(ComboBox6.ItemIndex);
  result.SpaceLeftBr := IndexToSpaceBefore(ComboBox7.ItemIndex);
  result.SpaceRightBr := IndexToSpaceBefore(ComboBox8.ItemIndex);
  result.SpaceLeftHook := IndexToSpaceBefore(ComboBox9.ItemIndex);
  result.SpaceRightHook := IndexToSpaceBefore(ComboBox10.ItemIndex);

  // Case
  result.ReservedCase := IndexToCase(ComboBox11.ItemIndex);
  result.StandDirectivesCase := IndexToCase(ComboBox12.ItemIndex);
  result.UpperCompDirectives := CheckBox22.Checked;
  result.UpperNumbers := CheckBox23.Checked;
  result.IdentifierCaseing := IndexToIdentifierCase(ComboBox14.ItemIndex);

  // Misc
  result.AlignComments := CheckBox2.Checked;
  result.AlignCommentPos := strtointdef(edit2.text, 40);
  result.AlignVar := CheckBox3.Checked;
  result.AlignVarPos := strtointdef(edit3.text, 20);
  // Was die folgenden Parameter machen ist nicht wirklich verstanden
  result.ExceptSingle := CheckBox24.Checked;
  Case ComboBox13.ItemIndex Of
    0: result.FillNewWords := fmUnchanged;
    1: result.FillNewWords := fmAddNewWord;
    2: result.FillNewWords := fmUse;
    3: result.FillNewWords := fmExceptDirect;
    4: result.FillNewWords := fmAddUse;
    5: result.FillNewWords := fmAddUseExcept;
  End;
  result.StartCommentOut := edit5.text;
  result.EndCommentOut := edit6.text;
  result.SupportCOperands := CheckBox25.Checked;
End;

End.

