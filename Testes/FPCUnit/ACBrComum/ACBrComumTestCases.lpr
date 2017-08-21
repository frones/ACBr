program ACBrComumTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrUtilTest, GuiTestRunner, ACBrCompress;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

