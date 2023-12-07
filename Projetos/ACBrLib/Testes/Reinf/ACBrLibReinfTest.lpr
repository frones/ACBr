program ACBrLibReinfTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibReinfStaticImportMT, ACBrLibConsts, GuiTestRunner,
  ACBrLibReinfTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

