program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testcase1, ugraphics;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

