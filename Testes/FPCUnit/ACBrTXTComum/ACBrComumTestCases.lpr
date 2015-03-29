program ACBrComumTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrTXTClassTest, acbrtxtclasstest2, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

