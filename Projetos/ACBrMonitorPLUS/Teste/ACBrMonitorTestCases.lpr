program ACBrMonitorTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibResposta, ACBrLibNFeRespostas, ACBrLibNFSeRespostas,
  ACBrLibCTeRespostas, ACBrLibBPeRespostas, ACBrLibBoletoRespostas,
  ACBrLibeSocialRespostas, ACBrLibGNReRespostas, ACBrLibMDFeRespostas,
  ACBrLibReinfConsts, ACBrLibCEPRespostas, ACBrLibIBGERespostas,
  ACBrLibGTINRespostas, ACBrLibNCMsRespostas, ACBrLibSedexRespostas,
  ACBrLibSATRespostas, ACBrLibConsultaCNPJRespostas,
  ACBrLibConsultaCPFRespostas, CmdUnit, ACBrMonitorConfig, DoACBrNFSeUnit,
  ACBrTests.Util, ACBrMonitorTests, GuiTestRunner, DoACBrNFSeTests, 
ACBrMonitorTestConsts;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

