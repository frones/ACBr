program ACBrTCPTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrSocketTest, ACBrTests.Util;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

