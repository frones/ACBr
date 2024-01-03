program ACBrGNReTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrTests.Util, GuiTestRunner,
pgnreRetConsResLoteGNRETestCases;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

