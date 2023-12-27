program ACBrDFeTestCases;

{
  Esse é um projeto de testes com a ajuda da ACBrTests.Runner.pas
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

  ATENÇÃO: 1) OS defines PRECISAM estar nas opções do projeto. Não basta definir no arquivo de projeto.
           2) Faça um Build sempre que fizer alterações de Defines.
  Para mais informações veja o arquivo: ACBrTests.Runner.pas
}

{$I ACBr.inc}

{$IFDEF NOGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF DUNITX}
  {$STRONGLINKTYPES ON}
{$ENDIF}

{$R *.RES}

uses
  ACBrTests.Util in '..\..\ACBrTests.Util.pas',
  ACBrTests.Runner in '..\..\ACBrTests.Runner.pas',
  ACBrDFeUtilTest in '..\..\FPCUnit\ACBrDFe\ACBrDFeUtilTest.pas',
  LibXml2TestCases in '..\..\FPCUnit\ACBrDFe\LibXml2TestCases.pas',
  ACBrLibXml2TestClass in '..\..\FPCUnit\ACBrDFe\ACBrLibXml2TestClass.pas',
  ACBrLibXml2TestAnsiConsts in '..\..\FPCUnit\ACBrDFe\ACBrLibXml2TestAnsiConsts.pas',
  ACBrLibXml2TestUTF8Consts in '..\..\FPCUnit\ACBrDFe\ACBrLibXml2TestUTF8Consts.pas';

{$R *.RES}

begin
  ACBrRunTests
end.

