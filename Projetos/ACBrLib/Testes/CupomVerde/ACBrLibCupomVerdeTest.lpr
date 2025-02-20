program ACBrLibCupomVerdeTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibCupomVerdeStaticImportMT, GuiTestRunner,
  ACBrLibConsts, ACBrLibCupomVerdeTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

