(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of playingcard demo                                      *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  SysUtils, Forms, Dialogs, StdCtrls, ComCtrls,
  Graphics, uplayingcard;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure ComboBox3Change(Sender: TObject);
    Procedure ComboBox4Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure PlayingCard1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PlayingCard1: TPlayingCard; // by defining the component by hand, you do not need to register it into the IDE
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // by defining the component by hand, you do not need to register it into the IDE
  PlayingCard1 := TPlayingCard.create(Self);
  PlayingCard1.Parent := self;
  PlayingCard1.Visible := true;
  PlayingCard1.Left := 216;
  PlayingCard1.Top := 64;
  PlayingCard1.OnClick := @PlayingCard1Click;
  // END - by defining the component by hand, you do not need to register it into the IDE
  caption := 'Playingcard Demo ver. 0.01 by Corpsman, support : www.Corpsman.de';
  label1.caption := 'This Sample demonstrates all capacities of the component : TPlayingcard';
  // Reset Aller Einstellungen
  PageControl1.TabIndex := 1;
  ComboBox1.ItemIndex := 0;
  ComboBox1Change(Nil);
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  b: Tbitmap;
Begin
  If OpenDialog1.Execute Then Begin
    b := tbitmap.create;
    b.LoadFromFile(OpenDialog1.FileName);
    If (b.Width <> CardWidth) Or
      (b.Height <> CardHeight) Then Begin
      showmessage('Error, bitmap resolution should be : ' + inttostr(CardWidth) + 'x' + inttostr(CardHeight) + LineEnding +
        'Card will not be added.');
      b.Free;
    End
    Else Begin
      AddCardTool(b);
      // b.Free; -- No Free, this will be done by uplayingcard.pas !!
      ComboBox4.Items.add(inttostr(ComboBox4.Items.Count + 1));
      ComboBox4.ItemIndex := ComboBox4.Items.Count - 1;
      ComboBox4Change(Nil);
    End;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  b: Tbitmap;
Begin
  If OpenDialog1.Execute Then Begin
    b := tbitmap.create;
    b.LoadFromFile(OpenDialog1.FileName);
    If (b.Width <> CardWidth) Or
      (b.Height <> CardHeight) Then Begin
      showmessage('Error, bitmap resolution should be : ' + inttostr(CardWidth) + 'x' + inttostr(CardHeight) + LineEnding +
        'Card will not be added.');
        b.Free;
    End
    Else Begin
      AddCardBack(b);
      // b.Free; -- No Free, this will be done by uplayingcard.pas !!
      ComboBox3.Items.add(inttostr(ComboBox3.Items.Count + 1));
      ComboBox3.ItemIndex := ComboBox3.Items.Count - 1;
      ComboBox3Change(Nil);
    End;
  End;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  PlayingCard1.Selected := CheckBox1.Checked;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Var
  i: integer;
Begin
  Case ComboBox1.ItemIndex Of
    0..3: Begin
        PlayingCard1.Suit := ComboBox1.ItemIndex;
        If ComboBox2.Items.Count <> 13 Then Begin
          ComboBox2.Clear;
          For i := 1 To 13 Do
            ComboBox2.Items.add(inttostr(i));
          ComboBox2.ItemIndex := 0;
        End;
      End;
    4: Begin
        If ComboBox2.Items.Count <> 2 Then Begin
          ComboBox2.Clear;
          For i := 1 To 2 Do
            ComboBox2.Items.add(inttostr(i));
          ComboBox2.ItemIndex := 0;
          PlayingCard1.FaceIndex := 0;
        End;
      End;
  End;
  ComboBox2Change(Nil);
End;

Procedure TForm1.ComboBox2Change(Sender: TObject);
Begin
  Case ComboBox1.ItemIndex Of
    0..3: Begin
        PlayingCard1.FaceIndex := 1 + ComboBox2.ItemIndex + 13 * ComboBox1.ItemIndex;
      End;
    4: Begin
        PlayingCard1.Suit := ComboBox2.ItemIndex;
      End;
  End;
  Label7.caption := inttostr(PlayingCard1.Value);
End;

Procedure TForm1.ComboBox3Change(Sender: TObject);
Begin
  PlayingCard1.BackIndex := ComboBox3.ItemIndex;
End;

Procedure TForm1.ComboBox4Change(Sender: TObject);
Begin
  PlayingCard1.ToolIndex := ComboBox4.ItemIndex;
End;

Procedure TForm1.PageControl1Change(Sender: TObject);
Begin
  If PageControl1.ActivePage.Caption = 'Back' Then
    PlayingCard1.Show := soBack
  Else If PageControl1.ActivePage.Caption = 'Face' Then
    PlayingCard1.Show := soface
  Else
    PlayingCard1.Show := soTool;
End;

Procedure TForm1.PlayingCard1Click(Sender: TObject);
Begin
  showmessage('You clicked on the card.');
End;

End.

