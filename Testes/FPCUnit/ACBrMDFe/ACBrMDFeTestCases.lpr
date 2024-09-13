program ACBrMDFeTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, ACBrTests.Util,
  ACBrMDFeTestConsts;

{$R *.res}

begin
  Application.Initialize;
  SetHeapTraceOutput('heaptrace.log');
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

