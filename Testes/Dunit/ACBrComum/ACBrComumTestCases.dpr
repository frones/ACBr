program ACBrComumTestCases;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  Forms,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ENDIF }
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  acbrutiltest in '..\..\FPCUnit\ACBrComum\acbrutiltest.pas';

{$R *.RES}

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnit.RunRegisteredTests;
  exit;
{$ENDIF}
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

