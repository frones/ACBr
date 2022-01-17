program AcbrLerTxt;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestAcBrLerTxt in 'TestAcBrLerTxt.pas',
  pcnNFe in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnNFe.pas',
  pcnNFeW in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnNFeW.pas',
  pcnNFeR in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnNFeR.pas',
  pcnAuxiliar in '..\..\..\Fontes\PCNComum\pcnAuxiliar.pas',
  ACBrValidador in '..\..\..\Fontes\ACBrDiversos\ACBrValidador.pas',
  ACBrUtil in '..\..\..\Fontes\ACBrComum\ACBrUtil.pas',
  AcbrLerTxtIni in 'Fontes\AcbrLerTxtIni.pas',
  IniPersist in 'Fontes\IniPersist.pas',
  pcnConversaoNFe in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnConversaoNFe.pas',
  pcnConversao in '..\..\..\Fontes\PCNComum\pcnConversao.pas',
  pcnNFeRTXT in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnNFeRTXT.pas',
  pcnLayoutTXT in '..\..\..\Fontes\ACBrDFe\ACBrNFe\PCNNFe\pcnLayoutTXT.pas';

{$R *.RES}

begin
  Application.Initialize;
    ReportMemoryLeaksOnShutdown := True;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

