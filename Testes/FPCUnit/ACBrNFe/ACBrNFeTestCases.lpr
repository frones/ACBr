program ACBrNFeTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrNFeTests, ACBrTests.Util, GuiTestRunner,
  ACBrNFeProvedorABRASFv1Tests, ACBrNFeProvedorABRASFv2Tests,
  ACBrNFeProvedorAgiliTests,
  ACBrNFeProvedorAssessorPublicoTests,
  ACBrNFeProvedorBauhausTests,
  ACBrNFeProvedorEquiplanoTests,
  ACBrNFeProvedorIPMTests,
  ACBrNFeProvedorISSBarueriTests,
  ACBrNFeProvedorPadraoNacionalTests,
  ACBrNFeProvedorSigISSTests,
  ACBrNFeProvedorSigISSWebTests,
  ACBrNFeProvedorSoftPlanTests,
  ACBrNFeProvedorWebFiscoTests,
  ACBrNFeRetornoSoapTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

