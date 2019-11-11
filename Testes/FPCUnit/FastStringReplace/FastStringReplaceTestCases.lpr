program FastStringReplaceTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, TestuStrReplaceTestClass, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

