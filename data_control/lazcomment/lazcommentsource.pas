(******************************************************************************)
(* Made by Corpsman                                                08.09.2009 *)
(* Support : www.Corpsman.de                                                  *)
(* Warranty : This unit goes as freeware for non comercial use only.          *)
(*            there is no warranty in hard or software damages.               *)
(*            Feel free to use or change the code.                            *)
(******************************************************************************)
Unit lazcommentsource;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, LCLtype;

Procedure CommentInFile(Sender: TObject);
Procedure Register;

Implementation

Uses menuintf, IDECommands, srceditorintf, synedit,
  ulazcomment
  ;
Const
  SCmdPFFile = 'CodeCommenter';
  SCatFormatting = 'Commenting';

Resourcestring
  SDescrPFFile = 'CodeCommenter';
  SDescrFormatting = 'CodeCommenter is runnning';

Var
  CmdFormatFile: TIDECommand;

Procedure Register;
Var
  Key: TIDEShortCut;
  (*
  In der Hoffnung das ich es noch hinbekomme, soll heisen das es hier irgendwann evtl den "Option" dialog geben soll
  *)
  Cat: TIDECommandCategory;
Begin
  Cat := IDECommandList.CreateCategory(Nil,
    SCatFormatting,
    SDescrFormatting,
    IDECmdScopeSrcEditOnly);
  Key := IDEShortCut(
    222 // Das neue # Zeichen, ermittelt mittels Codeviewer OnKeyDown !!
    {192}{ das  #  Zeichen }, [SSctrl], VK_UNKNOWN, []);
  CmdFormatFile := RegisterIDECommand(Cat,
    SCmdPFFile,
    SDescrPFFile,
    Key, Nil, @CommentInFile);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor,
    SCmdPFFile,
    SDescrPFFile,
    Nil, Nil, CmdFormatFile);
  (*
  SVN 29721 in MenuIntf
  itmEditBlockIndentation -> itmSourceBlockIndentation
  SVN 29862 in MenuIntf
  itmSourceBlockIndentation -> itmSourceBlockActions
  *)
  RegisterIDEMenuCommand(itmSourceBlockActions,
    SCmdPFFile,
    SDescrPFFile,
    Nil, Nil, CmdFormatFile);
End;

{Function CursorPosToIndex(Const Text: TStrings; Pos: TPoint): integer;
Var
  i, j: Integer;
Begin
  j := pos.y - 1;
  For i := 0 To pos.y - 1 Do
    j := j + length(text[i]);
  CursorPosToIndex := j;
End;}

Function min(a, b: integer): integer;
Begin
  If a < b Then
    min := a
  Else
    min := b;
End;

Function CursorPosToIndex(Const Text: TStrings; Pos: TPoint): integer;
Const
{$IFDEF WINDOWS}
  crtlen = 2;
{$ELSE}
  crtlen = 1;
{$ENDIF}
Var
  i: Integer;
Begin
  result := min(length(text[pos.y - 1]), pos.x);
  For i := 0 To pos.y - 2 Do
    result := result + length(text[i]) + crtlen;
End;

Procedure CommentInFile(Sender: TObject);
Var
  E: TSourceEditorInterface;
  cursors: Array Of Record // Da Fenster auch Geklont werden können, speichern wir alle Cursorpos und Topline
    Cursorpos: TPoint;
    TopLine: Integer;
    Bookmarks: Array[0..9] Of TPoint; // Die Bookmarks
  End;
  i, j: Integer;
  cursorpos: TPoint;
  st: TStringstream;
  selstart, selend: Integer;
  // Zum Speichern der Bookmarks
  sy: TSynedit;
Begin
  (* what the hack ?*)
  If Sender = Nil Then exit;
  // Hohlen des Source Fensters
  (*
  SVN 30122 srceditorintf.pas
  *)
  //E := SourceEditorWindow.ActiveEditor;
  If (SourceEditorManagerIntf = Nil) Then
    Exit;
  E := SourceEditorManagerIntf.ActiveEditor;
  If (E = Nil) Then
    Exit;
  // Speichern des Aktuellen Cursors
  i := SourceEditorManagerIntf.SourceEditorCount;
  setlength(cursors, i);
  cursorpos := E.CursorTextXY;
  For i := 0 To High(cursors) Do Begin
    cursors[i].Cursorpos := SourceEditorManagerIntf.SourceEditors[i].CursorTextXY;
    cursors[i].TopLine := SourceEditorManagerIntf.SourceEditors[i].TopLine;
    sy := TSynEdit(SourceEditorManagerIntf.SourceEditors[i].EditorControl);
    For j := 0 To 9 Do Begin
      cursors[i].Bookmarks[j].x := 0; // Beruhigt den Compiler
      cursors[i].Bookmarks[j].y := 0; // Beruhigt den Compiler
      sy.GetBookMark(j, cursors[i].Bookmarks[j].x, cursors[i].Bookmarks[j].y);
    End;
  End;
  (*
  In Der Hoffnung, das diese if Bedingung nur kommt, wenn der Cursor hinter einer
  Zeile Steht, und nichts selektiert wurde ..
  *)
  selstart := e.selstart;
  selend := e.selend;
  If (cursorpos.x > length(e.CurrentLineText)) And (length(e.Selection) = 0) Then Begin
    (*
    Dieser Code setzt Selstart so um, das es auf dem Letzten zeichen der Zeile steht.
    *)
    selstart := CursorPosToIndex(e.lines, Cursorpos);
    selend := selstart;
  End;
  // Das Kommentieren ist ausgelagert, so kann man es an anderen stellen auch nutzen ;)
  comment(e.lines, selstart, selend);
  (*
  Das Mus so "Bescheuert" gemacht werden, damit die IDE mit bekommt das sich
  der source geändert hat.
  *)
  st := TStringStream.Create(e.lines.text);
  st.Position := 0;
  E.ReplaceLines(0, E.LineCount, St.DataString);
  st.free;
  // Setzen der Cursor Pos
//    e.CursorTextXY := cursorpos;  -- ALT
//    E.TopLine := cursorTopLine; -- ALT
  For i := 0 To High(cursors) Do Begin
    SourceEditorManagerIntf.SourceEditors[i].CursorTextXY := cursors[i].Cursorpos;
    SourceEditorManagerIntf.SourceEditors[i].TopLine := cursors[i].TopLine;
    sy := TSynEdit(SourceEditorManagerIntf.SourceEditors[i].EditorControl);
    For j := 0 To 9 Do Begin
      sy.SetBookMark(j, cursors[i].Bookmarks[j].x, cursors[i].Bookmarks[j].y);
    End;
  End;
  setlength(cursors, 0);
End;

End.











