Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ComCtrls, ExtCtrls, uradialtreemenu;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TRadialTreeMenu;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    Procedure AssignLeafHandlers(AParent: TMenuItem; aEvent: TNotifyEvent);
    Procedure CopyToNativeMenu(AParent: TMenuItem; ANode: TTreeNode);
    Procedure CopyToRadialMenu(AParent: TRadialTreeMenuItem; ANode: TTreeNode);
    Procedure LeafClick(Sender: TObject);
    Procedure TreeLeafClick(Sender: TObject);
    Procedure RebuildMenus;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  Item1, Item2, Item3: TTreeNode;
Begin
  Caption := 'RadialTreeMenu demo ver. 0.01, by Corpsman, www.Corpsman.de';
  Edit1.text := 'Nodename';
  Memo1.Clear;
  PopupMenu2 := TRadialTreeMenu.Create(Panel2);
  PopupMenu2.Name := 'PopupMenu2';
  PopupMenu2.Parent := Panel2;
  Panel1.Caption := 'Right click to open Menu';
  Panel2.Caption := 'Right click to open TreeMenu';

  TreeView1.Items.BeginUpdate;
  Try
    TreeView1.Items.Clear;

    Item1 := TreeView1.Items.Add(Nil, 'Item1');
    TreeView1.Items.AddChild(Item1, 'Subitem1.1');
    TreeView1.Items.AddChild(Item1, 'Subitem1.2');

    Item2 := TreeView1.Items.Add(Nil, 'Item2');
    TreeView1.Items.AddChild(Item2, 'Subitem2.1');
    TreeView1.Items.AddChild(Item2, 'Subitem2.2');

    Item3 := TreeView1.Items.Add(Nil, 'Item3' + LineEnding + 'Test');
    TreeView1.Items.AddChild(Item3, 'Subitem3.1');
    Item3 := TreeView1.Items.AddChild(Item3, 'Subitem3.2');
    TreeView1.Items.AddChild(Item3, 'Subitem3.2.1');
  Finally
    TreeView1.Items.EndUpdate;
  End;

  Panel1.PopupMenu := PopupMenu1;
  Panel2.PopupMenu := PopupMenu2;
  RebuildMenus;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  TextValue: String;
  SelectedNode: TTreeNode;
Begin
  TextValue := Trim(Edit1.Text);
  If TextValue = '' Then
    Exit;

  SelectedNode := TreeView1.Selected;
  If SelectedNode = Nil Then Begin
    SelectedNode := TreeView1.Items.Add(Nil, TextValue);
  End
  Else Begin
    SelectedNode := TreeView1.Items.AddChild(SelectedNode, TextValue);
    SelectedNode.Parent.Expanded := True;
  End;
  SelectedNode.Selected := True;

  RebuildMenus;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If TreeView1.Selected <> Nil Then Begin
    TreeView1.Selected.Free;
    RebuildMenus;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  TreeView1.Items.Clear;
  RebuildMenus;
End;

Procedure TForm1.CopyToNativeMenu(AParent: TMenuItem; ANode: TTreeNode);
Var
  Item: TMenuItem;
Begin
  While ANode <> Nil Do Begin
    Item := TMenuItem.Create(PopupMenu1);
    Item.Caption := ANode.Text;
    AParent.Add(Item);
    CopyToNativeMenu(Item, ANode.GetFirstChild);
    ANode := ANode.GetNextSibling;
  End;
End;

Procedure TForm1.CopyToRadialMenu(AParent: TRadialTreeMenuItem;
  ANode: TTreeNode);
Var
  Item: TRadialTreeMenuItem;
Begin
  While ANode <> Nil Do Begin
    Item := AParent.Add(ANode.Text);
    CopyToRadialMenu(Item, ANode.GetFirstChild);
    ANode := ANode.GetNextSibling;
  End;
End;

Procedure TForm1.AssignLeafHandlers(AParent: TMenuItem; aEvent: TNotifyEvent);
Var
  I: Integer;
Begin
  For I := 0 To AParent.Count - 1 Do
    If AParent[I].Count = 0 Then
      AParent[I].OnClick := aEvent
    Else
      AssignLeafHandlers(AParent[I], aEvent);
End;

Procedure TForm1.LeafClick(Sender: TObject);
Begin
  Memo1.Append((Sender As TMenuItem).Caption + ' clicked');
End;

Procedure TForm1.TreeLeafClick(Sender: TObject);
Begin
  Memo1.Append('Tree ' + (Sender As TMenuItem).Caption + ' clicked');
End;

Procedure TForm1.RebuildMenus;
Var
  Node: TTreeNode;
  RadialItem: TRadialTreeMenuItem;
Begin
  PopupMenu1.Items.Clear;
  PopupMenu2.Clear;

  CopyToNativeMenu(PopupMenu1.Items, TreeView1.Items.GetFirstNode);

  Node := TreeView1.Items.GetFirstNode;
  While Node <> Nil Do Begin
    RadialItem := PopupMenu2.Add(Node.Text);
    CopyToRadialMenu(RadialItem, Node.GetFirstChild);
    Node := Node.GetNextSibling;
  End;

  AssignLeafHandlers(PopupMenu1.Items, @LeafClick);
  AssignLeafHandlers(PopupMenu2.Items, @TreeLeafClick);
  TreeView1.FullExpand;
End;

End.

