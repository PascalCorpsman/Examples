Program fpcunitproject1;

{$MODE objfpc}{$H+}

Uses
  Interfaces, Forms, GuiTestRunner, TestCase1;

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
End.

