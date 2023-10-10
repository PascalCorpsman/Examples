Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Image1: TImage;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure LoadPreview(Const Image: TBitmap);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses math;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Preview';
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.LoadPreview(Const Image: TBitmap);
Begin
  Image1.Picture.Assign(Image);
  width := max(110, Image.Width + 20);
  Height := max(150, Image.Height + 60);
End;

End.

