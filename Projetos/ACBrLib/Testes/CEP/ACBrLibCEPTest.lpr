program ACBrLibCEPTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibCEPStaticImportMT, ACBrLibConsts, GuiTestRunner,
  ACBrLibCEPTestCase;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

