program ACBrPCNTesteCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, pcnGeradorTestsUnit,
  ACBrTests.Util;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

