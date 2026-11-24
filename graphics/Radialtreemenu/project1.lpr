program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uradialtreemenu_tests, ugraphics, uvectormath
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  ConfigureTestDPI;
  Application.CreateForm(TForm1, Form1);
  ConfigureCaptionTest;
  ConfigureClickTest;
  ConfigureFullRingTest;
  if (ParamCount > 0) and (ParamStr(1) = '--test-items') then
  begin
    Application.CaptureExceptions := False;
    try
      ExitCode := RunRadialMenuItemTests;
    finally
      Form1.Free;
    end;
    Exit;
  end;
  Application.Run;
end.

