program ACBrNFSeXTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrNFSeXTests, ACBrTests.Util, GuiTestRunner,
  ACBrNFSeXProvedorABRASFv1Tests, ACBrNFSeXProvedorABRASFv2Tests,
  ACBrNFSeXProvedorAssessorPublicoTests,
  ACBrNFSeXProvedorBauhausTests,
  ACBrNFSeXProvedorPadraoNacionalTests,
  ACBrNFSeXProvedorSigISSTests, 
  ACBrNFSeXProvedorSoftPlanTests, 
  ACBrNFSeXProvedorWebFiscoTests,
  ACBrNFSeXProvedorAgiliTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

