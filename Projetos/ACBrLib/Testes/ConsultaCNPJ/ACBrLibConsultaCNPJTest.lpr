program ACBrLibConsultaCNPJTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibConsultaCNPJStaticImportMT, ACBrLibConsultaCNPJStaticImportST,
  ACBrLibConsts, GuiTestRunner, fpcunittestrunner, ACBrLibConsultaCNPJTestCase;

{$R *.res}
begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

