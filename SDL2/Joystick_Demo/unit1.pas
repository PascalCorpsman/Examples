(******************************************************************************)
(* SDL2 Joystick Demo                                              20.11.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo to show using of usdl_joystick.pas                      *)
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

(*
 * sudo aptitude install libsdl2-dev
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

{$I sdl2_cfg.inc}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls
  , sdl2
  , usdl_joystick;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    fsdlJoyStick: TSDL_Joystick;
    pb: Array Of TProgressBar;
    sp: Array Of TShape;
    Procedure CreateJoyStickLCL();
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If assigned(fsdlJoyStick) Then fsdlJoyStick.free;
  SDL_Quit();
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If ComboBox1.ItemIndex <> -1 Then Begin
    Try
      fsdlJoyStick := TSDL_Joystick.Create(ComboBox1.ItemIndex);
    Except
      fsdlJoyStick.free;
      fsdlJoyStick := Nil;
      showmessage('Error could not init joystick.');
      exit;
    End;
    button1.Enabled := false;
    ComboBox1.Enabled := false;
    CreateJoyStickLCL();
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  ver: TSDL_Version;
  i: Integer;
Begin
{$IFDEF SDL_DYNAMIC_LINKING}
  If Not SDL_LoadLib('') Then Begin
    ShowMessage('Error, unable to load sdl lib');
    halt;
  End;
{$ENDIF}
  caption := 'SDL-Joystick Demo ver. 0.01';
  If SDL_Init(SDL_INIT_JOYSTICK) <> 0 Then Begin
    showmessage('Error could not init SDL');
    halt;
  End;
  SDL_GetVersion(@ver);
  If SDL_VERSIONNUM(ver.major, ver.minor, ver.patch) < SDL_COMPILEDVERSION Then Begin
    showmessage(format('The version of sdl.dll (%d.%d.%d) is to low you have to have at least %d.%d.%d', [ver.major, ver.minor, ver.patch, SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL]));
    halt;
  End;
  ComboBox1.Clear;
  For i := 0 To SDL_NumJoysticks() - 1 Do Begin
    ComboBox1.Items.Add(SDL_JoystickNameForIndex(i));
  End;
  If ComboBox1.Items.Count <> 0 Then Begin
    ComboBox1.ItemIndex := 0;
  End;
  fsdlJoyStick := Nil;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  event: TSDL_Event;
Begin
  While SDL_PollEvent(@event) <> 0 Do Begin
    Case event.type_ Of
      SDL_JOYAXISMOTION: Begin // Eine Joystick Achse wurde geändert, diese überbehmen wir
          pb[event.jaxis.axis].Position := fsdlJoyStick.Axis[event.jaxis.axis];
        End;
      SDL_JOYBUTTONUP,
        SDL_JOYBUTTONDOWN: Begin
          If fsdlJoyStick.Button[event.jbutton.button]
            Then Begin
            sp[event.jbutton.button].Brush.Color := clRed;
          End
          Else Begin
            sp[event.jbutton.button].Brush.Color := clWhite;
          End;
        End;
    End;
  End;
End;

Procedure TForm1.CreateJoyStickLCL();
Var
  i: Integer;
Begin
  setlength(pb, fsdlJoyStick.AxisCount);
  For i := 0 To fsdlJoyStick.AxisCount - 1 Do Begin
    pb[i] := TProgressBar.Create(Form1);
    pb[i].Name := 'Progressbar' + inttostr(i + 1);
    pb[i].Parent := Form1;
    pb[i].Min := -32768;
    pb[i].Max := 32767;
    pb[i].Orientation := pbVertical;
    pb[i].Top := 48;
    pb[i].Left := 16 + i * (44 - 16);
    pb[i].Width := 20;
    pb[i].Height := 180;
    pb[i].Position := fsdlJoyStick.Axis[i];
  End;
  setlength(sp, fsdlJoyStick.ButtonCount);
  For i := 0 To fsdlJoyStick.ButtonCount - 1 Do Begin
    sp[i] := TShape.Create(form1);
    sp[i].name := 'Shape' + IntToStr(i + 1);
    sp[i].Parent := Form1;
    sp[i].Shape := stCircle;
    sp[i].Width := 20;
    sp[i].Height := 20;
    sp[i].Top := 48 + 180 + 10;
    sp[i].Left := 16 + i * (20 + 5);
    If fsdlJoyStick.Button[i] Then Begin
      sp[i].Brush.Color := clRed;
    End
    Else Begin
      sp[i].Brush.Color := clWhite;
    End;
  End;
  timer1.Enabled := true;
End;

End.

