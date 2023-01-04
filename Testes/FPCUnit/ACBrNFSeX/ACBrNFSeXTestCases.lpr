program ACBrNFSeXTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrNFSeXTests, ACBrTests.Util, GuiTestRunner,
  ACBrNFSeXProvedorABRASFv1Tests, ACBrNFSeXProvedorABRASFv2Tests,
  ACBrNFSeXProvedorAgiliTests,
  ACBrNFSeXProvedorAssessorPublicoTests,
  ACBrNFSeXProvedorBauhausTests,
  ACBrNFSeXProvedorEquiplanoTests,
  ACBrNFSeXProvedorIPMTests,
  ACBrNFSeXProvedorPadraoNacionalTests,
  ACBrNFSeXProvedorSigISSTests, 
  ACBrNFSeXProvedorSoftPlanTests, 
  ACBrNFSeXProvedorWebFiscoTests,
  ACBrNFSeXRetornoSoapTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

