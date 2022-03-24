program ACBrComumTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrUtilTest, ACBrUtil.StringsTests, ACBrTests.Util,
  GuiTestRunner, ACBrCompress;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

