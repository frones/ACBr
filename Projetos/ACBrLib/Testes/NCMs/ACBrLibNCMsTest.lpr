program ACBrLibNCMsTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibNCMsTestCase, ACBrLibConsts,
  ACBrLibNCMsStaticImportMT, GuiTestRunner;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

