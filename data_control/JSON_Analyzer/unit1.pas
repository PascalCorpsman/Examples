(******************************************************************************)
(* JSON-Analyser                                                   ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo that shows how to use uJSON.pas and also implements a   *)
(*               JSON Prettyfier.                                             *)
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
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, uJSON;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    Procedure CreateTree(N: TJSONObj; Root: TTreeNode);

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  jo: TJSONObj;
  p: TJSONParser;
Begin
  p := TJSONParser.Create;
  p.SupportJSON5Comments := CheckBox1.Checked;
  Try
    jo := p.Parse(memo1.Text);
  Except
    On av: Exception Do Begin
      showmessage(av.Message);
      jo := Nil;
    End;
  End;
  p.free;
  TreeView1.Items.Clear;
  If Not assigned(jo) Then Begin
    showmessage('Invalid JSON input');
    exit;
  End;
  If assigned(jo) Then Begin
    memo1.Text := trim(jo.ToString('')); // Pretty printed
    // Generieren des TTreeview
    CreateTree(jo, Nil);
    jo.free;
    TreeView1.FullExpand;
  End
  Else Begin
    TreeView1.Items.Clear;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  jpo, jo: TJSONObj;
  p: TJSONParser;
Begin
  // Findpath
  p := TJSONParser.Create;
  p.SupportJSON5Comments := CheckBox1.Checked;
  Try
    jo := p.Parse(memo1.Text);
  Except
    On av: Exception Do Begin
      showmessage(av.Message);
    End;
  End;
  p.free;
  If Not assigned(jo) Then Begin
    showmessage('Invalid JSON input');
    exit;
  End;
  jpo := jo.FindPath(ComboBox1.text);
  If jpo <> Nil Then Begin
    If jpo Is TJSONValue Then Begin
      showmessage((jpo As TJSONValue).Value);
      jo.free;
      exit;
    End;
    If jpo Is TJSONTerminal Then Begin
      showmessage((jpo As TJSONTerminal).Value);
      jo.free;
      exit;
    End;
    showmessage('Findpath gave result ' + jpo.ClassName + ' but there is no showing implementation in this demo for it.');
  End
  Else Begin
    showmessage('Could not find path.');
  End;
  jo.free;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * History: 0.01 = initial version
   *          0.02 = Demo für Findpath integriert
   *          0.03 = Fix AV-wenn Code nicht Parsebar
   *                 Schriftart Courier New
   *          0.04 = Support für JSON5 Kommentare
   *          0.05 = Update Uncommenter für besseren Support der JSON5 Kommentare
   *)
  caption := 'JSON-Class Analyzer ver. 0.05';
  Application.Title := caption;

  ComboBox1.items.add('User:info.Username');
  ComboBox1.items.add('User:info.Roles');
  ComboBox1.items.add('User:info.Roles[0]');
  ComboBox1.items.add('User:info.Roles[1]');
  ComboBox1.items.add('app:options.localregion');
  ComboBox1.itemindex := 0;

  // memo1.Text := '{"box":[[1.2,3.4],[5.6,7.8]]}';
End;

Procedure TForm1.CreateTree(N: TJSONObj; Root: TTreeNode);
Var
  t: TTreeNode;
  //jt: TJSONString;
  ja: TJSONArray;
  jv: TJSONValue;
  jn: TJSONNode;
  jno: TJSONNodeObj;
  i: Integer;
Begin
  If n Is TJSONNode Then Begin
    jn := n As TJSONNode;
    t := TreeView1.Items.AddChild(root, 'TJSONNode');
    For i := 0 To jn.ObjCount - 1 Do Begin
      CreateTree(jn.Obj[i], t);
    End;
    exit;
  End;
  If n Is TJSONNodeObj Then Begin
    jno := n As TJSONNodeObj;
    t := TreeView1.Items.AddChild(root, 'TJSONNodeObj: ' + jno.Name);
    CreateTree(jno.Value, t);
    exit;
  End;
  If n Is TJSONValue Then Begin
    jv := n As TJSONValue;
    t := TreeView1.Items.AddChild(root, 'TJSONValue: ' + jv.Name);
    exit;
  End;
  If n Is TJSONArray Then Begin
    ja := n As TJSONArray;
    t := TreeView1.Items.AddChild(root, 'TJSONArray (' + inttostr(ja.ObjCount) + ' elements)');
    For i := 0 To ja.ObjCount - 1 Do Begin
      CreateTree(ja.Obj[i], t);
    End;
    exit;
  End;
  If n Is TJSONTerminal Then Begin
    //jt := n As TJSONTerminal;
    t := TreeView1.Items.AddChild(root, 'TJSONTerminal');
    exit;
  End;

  Raise exception.create('Error not implemented class: ' + n.ClassName);
End;

End.

