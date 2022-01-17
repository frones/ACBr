program ACBrBoletoTestCase;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrrBoletoTest, ACBrTests.Util, GuiTestRunner,
  fpcunittestrunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

