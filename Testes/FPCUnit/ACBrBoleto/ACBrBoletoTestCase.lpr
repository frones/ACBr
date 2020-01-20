program ACBrBoletoTestCase;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrrBoletoTest, GuiTestRunner, fpcunittestrunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

