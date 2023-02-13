program ACBrReinfTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrTests.Util, GuiTestRunner, ACBrReinfRetornoSoapTests;


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

