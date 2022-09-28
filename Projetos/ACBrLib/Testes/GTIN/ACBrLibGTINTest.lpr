program ACBrLibGTINTest;


{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibGTINStaticImportMT, ACBrLibConsts, GuiTestRunner,
  fpcunittestrunner, ACBrLibGTINTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

