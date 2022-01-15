program TestesTACrTXTClass;

{
  Esse é um projeto de testes do projeto ACBr para Delphi.
  -------------------------
  Este projeto deve funcionar tanto em DUnit/DUnitX/TestInsight
  Por padrão ele irá utilizar DUnit e Interface (GUI)

  Para mudar o comportamento, adicione os seguintes "conditional defines" nas
    opções do projeto (project->options):
    * "NOGUI"       - Transforma os testes em uma aplicação CONSOLE
    * "DUNITX"      - Passa a usar a DUnitX ao invés da Dunit
    * "TESTINSIGHT" - Passa a usar o TestInsight
    * "CI"          - Caso use integração continua (por exemplo com o Continua CI ou Jenkins)
                    --/ Geralmente usado em conjunto com NOGUI
    * "FMX"         - Para usar Firemonkey (FMX) ao invés de VCL. (Testado apenas com DUnitX)

  ATENÇÃO: OS defines PRECISAM estar nas opções do projeto. Não basta definir nesta unit.
  Veja o arquivo: ACBrTests.Runner.pas
}

{$I ACBr.inc}

{$IFDEF NOGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF DUNITX}
  {$STRONGLINKTYPES ON}
{$ENDIF}
uses
  ACBrTests.Util in '..\..\ACBrTests.Util.pas',
  ACBrTests.Runner in '..\..\ACBrTests.Runner.pas',
  acbrtxtclasstest in '..\..\FPCUnit\ACBrTXTComum\acbrtxtclasstest.pas',
  acbrtxtclasstest2 in '..\..\FPCUnit\ACBrTXTComum\acbrtxtclasstest2.pas';

{$R *.RES}

begin
  ACBrRunTests;
end.
