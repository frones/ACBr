program ACBrComumTestCases;

{
  Esse é um projeto de testes do projeto ACBr para Delphi.
  -------------------------
  Este projeto deve funcionar tanto em DUnit/DUnitX/TestInsight
  Por padrão ele irá utilizar DUnit e Interface (GUI)

  Para mudar o comportamento, adicione os seguintes "conditional defines" nas
    opções do projeto (project->options):
    * "CONSOLE_TESTRUNNER" - Transforma os testes em uma aplicação CONSOLE
    * "DUNITX"      - Passa a usar a DUnitX ao invés da Dunit
    * "TESTINSIGHT" - Passa a usar o TestInsight
    * "CI"          - Caso use integração continua (por exemplo com o Continua CI ou Jenkins)
                    --/ Geralmente usado em conjunto com CONSOLE_TESTRUNNER
    * "FMX"         - Para usar Firemonkey (FMX) ao invés de VCL. (Testado apenas com DUnitX)

  ATENÇÃO: OS defines PRECISAM estar nas opções do projeto. Não basta definir nesta unit.
}

{$I ACBr.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF DUNITX}
  {$STRONGLINKTYPES ON}
{$ENDIF}
uses
  SysUtils,
{$IFDEF TESTINSIGHT}
  {$IFDEF DUNITX}
  TestInsight.DUnitX,
  {$ELSE}
  TestInsight.DUnit,
  {$ENDIF}
{$ENDIF }

{$IFNDEF CONSOLE_TESTRUNNER}
  {$IFDEF FMX}
  FMX.Forms,
  {$ELSE}
    {$IFDEF USE_NAMESPACES}
  Vcl.Forms,
    {$ELSE}
  Forms,
    {$ENDIF }
  {$ENDIF }
{$ENDIF }

{$IFDEF DUNITX}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$IFNDEF CONSOLE_TESTRUNNER}
    {$IFDEF FMX}
  DUnitX.Loggers.GUIX,
    {$ELSE}
  DUnitX.Loggers.GUI.VCL,
    {$ENDIF}
  {$ENDIF }
  DUnitX.TestFramework,
  DUnitX.DUnitCompatibility,
{$ELSE}
  TestFramework,
  GUITestRunner,
  TextTestRunner,
{$ENDIF}
  acbrutiltest in '..\..\FPCUnit\ACBrComum\acbrutiltest.pas';

{$IFDEF TESTINSIGHT}
  function IsTestInsightRunning: Boolean;
  var
    TestInsightClient: ITestInsightClient;
  begin
    TestInsightClient := TTestInsightRestClient.Create;
    TestInsightClient.StartedTesting(0);
    Result := not TestInsightClient.HasError;
  end;
{$ELSE}
  {$IFDEF CONSOLE_TESTRUNNER}
  procedure PausaSeNaoTiverCI;
  begin
    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
    {$ENDIF}
  end;

    {$IFDEF DUNITX}
    procedure ConsoleDUnitX;
    var
      runner: ITestRunner;
      results: IRunResults;
      logger: ITestLogger;
      nunitLogger : ITestLogger;
    begin
      try
        //Check command line options, will exit if invalid
        TDUnitX.CheckCommandLine;
        //Create the test runner
        runner := TDUnitX.CreateRunner;
        //Tell the runner to use RTTI to find Fixtures
        runner.UseRTTI := True;
        //When true, Assertions must be made during tests;
        runner.FailsOnNoAsserts := False;

        //tell the runner how we will log things
        //Log to the console window if desired
        if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
        begin
          logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
          runner.AddLogger(logger);
        end;
        //Generate an NUnit compatible XML File
        nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
        runner.AddLogger(nunitLogger);

        //Run tests
        results := runner.Execute;
        if not results.AllPassed then
          System.ExitCode := EXIT_ERRORS;

        PausaSeNaoTiverCI;
      except
        on E: Exception do
          System.Writeln(E.ClassName, ': ', E.Message);
      end;
    end;
    {$ELSE}
    procedure ConsoleDUnit;
    begin
      with TextTestRunner.RunRegisteredTests do
        Free;
      PausaSeNaoTiverCI
    end;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
  if IsTestInsightRunning then
  {$IFDEF DUNITX}
    TestInsight.DUnitX.RunRegisteredTests;
  {$ELSE}
    TestInsight.DUnit.RunRegisteredTests;
  {$ENDIF}
    Exit;
{$ELSE}
  {$IFDEF DUNITX}
    {$IFDEF CONSOLE_TESTRUNNER}
      ConsoleDUnitX;
      Exit;
    {$ELSE}
      {$IFDEF FMX}
//      TDUnitX.CheckCommandLine;

      Application.Initialize;
      Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
      Application.Run;
      {$ELSE}
      DUnitX.Loggers.GUI.VCL.Run;
      {$ENDIF}
      Exit;
    {$ENDIF}
  {$ELSE}
    {$IFDEF CONSOLE_TESTRUNNER}
      ConsoleDUnit;
      Exit;
    {$ELSE}
      Application.Initialize;
      GUITestRunner.RunRegisteredTests;
      Exit;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end.
