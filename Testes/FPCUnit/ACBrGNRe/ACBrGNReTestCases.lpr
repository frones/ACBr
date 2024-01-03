program ACBrGNReTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrTests.Util, GuiTestRunner,
pgnreRetConsResLoteGNRETestCases, ACBrGNReGuiasRetornoTestCases,
ACBrGNReTestConsts;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

