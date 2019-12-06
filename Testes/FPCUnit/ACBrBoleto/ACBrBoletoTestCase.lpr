program ACBrBoletoTestCase;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, acbrboletotest, GuiTestRunner, fpcunittestrunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

