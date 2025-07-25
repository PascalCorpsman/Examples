(*
This Package is oriented to the PrettyFormat Package.

But Imports the delforex library aviable for Delphi 5 - X

Import Written by : Corpsman
Support : www.Corpsman.de
*)

Unit delforsource;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LCLtype, delfortypes;

Procedure PrettyPrintFile(Sender: TObject);
Function DefaultSettings: TSettings;

Procedure Register;

Implementation

Uses LazIDEIntf, menuintf, idecommands, srceditorintf, synedit, delforinterf, dialogs // Ausgabe der Fehlermeldung
  , Fileutil, LazFileUtils, lazutf8
  ;

Const
  SCmdPFFile = 'PrettyFormatFile';
  SCatFormatting = 'Formatting';

Resourcestring
  SDescrPFFile = 'Pretty-Format File';
  SDescrFormatting = 'Formatting commands';

Var
  CmdFormatFile: TIDECommand;

Procedure Register;
Var
  Key: TIDEShortCut;
  (*
  In der Hoffnung das ich es noch hinbekomme, soll heisen das es hier irgendwann evtl. den "Option" dialog geben soll
  *)
  Cat: TIDECommandCategory;
Begin
  Cat := IDECommandList.CreateCategory(Nil,
    SCatFormatting,
    SDescrFormatting,
    IDECmdScopeSrcEditOnly);
  Key := IDEShortCut(VK_D, [SSctrl], VK_UNKNOWN, []);
  CmdFormatFile := RegisterIDECommand(Cat,
    SCmdPFFile,
    SDescrPFFile,
    Key, Nil, @PrettyPrintFile);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor,
    SCmdPFFile,
    SDescrPFFile,
    Nil, Nil, CmdFormatFile);
  RegisterIDEMenuCommand(itmSourceBlockActions,
    SCmdPFFile,
    SDescrPFFile,
    Nil, Nil, CmdFormatFile);
End;

Procedure LoadSettins(Const Settings: PSettings);
Var
  s: String;
  F: Tfilestream;
Begin
  s := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'delforlaz.cfg';
  If FileExistsutf8(s) Then Begin
    f := TFilestream.create(utf8tosys(s), fmopenread);
    f.read(Settings^, sizeof(TSettings));
    f.free;
  End
  Else Begin
    showmessage('You are using delforlaz for the first time.'#13 +
      'The component loads default values and will save them to :'#13#13 +
      s + #13#13 +
      'Please use the setup dialog to create your own config file.');
    Settings^ := DefaultSettings;
    f := TFilestream.create(utf8tosys(s), fmOpenWrite Or fmCreate);
    f.Write(Settings^, sizeof(TSettings));
    f.free;
  End;
End;

Procedure PrettyPrintFile(Sender: TObject);
Var
  E: TSourceEditorInterface;
  st: TStringStream;
  cursors: Array Of Record // Da Fenster auch Geklont werden können, speichern wir alle Cursorpos und Topline
    Cursorpos: TPoint;
    TopLine: Integer;
  End;
  i: Integer;
  SettingsP: PSettings;
  Text: PChar;
Begin
  (* what the hack ?*)
  If Sender = Nil Then exit;
  If (SourceEditorManagerIntf = Nil) Then Exit;
  E := SourceEditorManagerIntf.ActiveEditor;
  If (E = Nil) Then Exit;
  // Speichern des Aktuellen Cursors
  i := SourceEditorManagerIntf.SourceEditorCount;
  setlength(cursors, i);
  For i := 0 To High(cursors) Do Begin
    cursors[i].Cursorpos := SourceEditorManagerIntf.SourceEditors[i].CursorTextXY;
    cursors[i].TopLine := SourceEditorManagerIntf.SourceEditors[i].TopLine;
  End;
  // Init Codeformater
  Formatter_Create;
  New(SettingsP);
  LoadSettins(SettingsP);
  Try
    Formatter_Clear;
    //Laden des Textes
    Text := @e.Lines.Text[1];
    Formatter_SetTextStr(Text);
    // Formatieren des Textes
    Formatter_Parse(SettingsP, sizeof(Tsettings));
    // Zurückschreiben des Textes
    Text := Formatter_GetTextStr;
    (*
    Das Mus so "Bescheuert" gemacht werden, damit die IDE mit bekommt das sich
    der source geändert hat.
    *)
    st := TStringStream.Create(text);
    st.Position := 0;
    E.ReplaceLines(0, E.LineCount, St.DataString, true);
    st.free;
  Finally
    Formatter_Destroy;
  End;
  // Setzen der Cursor Pos
  For i := 0 To High(cursors) Do Begin
    SourceEditorManagerIntf.SourceEditors[i].CursorTextXY := cursors[i].Cursorpos;
    SourceEditorManagerIntf.SourceEditors[i].TopLine := cursors[i].TopLine;
  End;
  // Freigeben der benötigten Variablen
  setlength(cursors, 0);
  Dispose(SettingsP);
End;

Function DefaultSettings: TSettings;
Begin
  With result Do Begin
    WrapLines := False;
    WrapPosition := 81;
    AlignCommentPos := 40;
    AlignComments := False;
    AlignVarPos := 20;
    AlignVar := False;
    SpaceEqualOper := spBoth;
    SpaceOperators := spBoth;
    SpaceColon := spAfter;
    SpaceComma := spAfter;
    SpaceSemiColon := spAfter;
    SpaceLeftBr := spNone;
    SpaceRightBr := spNone;
    SpaceLeftHook := spNone;
    SpaceRightHook := spNone;
    ReservedCase := rfLowerCase;
    StandDirectivesCase := rfLowerCase;
    ChangeIndent := True;
    indentBegin := False;
    IndentComments := False;
    IndentCompDirectives := False;
    IndentTryElse := False;
    IndentCaseElse := False;
    FeedAfterThen := False;
    ExceptSingle := False;
    FeedElseIf := False;
    FeedEachUnit := False;
    NoFeedBeforeThen := False;
    NoIndentElseIf := False;
    FeedAfterVar := False;
    FeedBeforeEnd := False;
    FeedRoundBegin := UnChanged;
    FeedAfterSemiColon := False;
    FillNewWords := fmUnchanged;
    IndentTry := False;
    UpperCompDirectives := True;
    UpperNumbers := True;
    SpacePerIndent := 2;
    BlankProc := True;
    RemoveDoubleBlank := False;
    BlankSubProc := False;
    StrCopy(StartCommentOut, '{(*}');
    StrCopy(EndCommentOut, '{*)}');
  End;
End;

End.

