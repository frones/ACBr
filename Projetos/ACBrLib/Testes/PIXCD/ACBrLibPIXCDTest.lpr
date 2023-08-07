program ACBrLibPIXCDTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibPIXCDStaticImportMT, GuiTestRunner,
  ACBrLibConsts, ACBrLibPIXCDTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

