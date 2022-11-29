{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrTests.Util;

{
  Esse Unit possui métodos úteis utilizados nos Testes Unitários do ACBr.
  -------------------------
  Esses métodos tem objetivo de permitir o máximo de compatibilidade entre os frameworks
    DUnitX/DUnit/FPCunit, FMX/VCL, CONSOLE/GUI/TestInsight
  No momento os testes precisam ser desenvolvidos como se estivessem no Dunit ou na FPCUnit.
  Assim, com ajuda dessa unit, consguimos manter a compatibilidade mencionada.

}

{$I ACBr.inc}

interface

uses
 SysUtils,
  {$ifdef FPC}
  fpcunit,{testutils,} testregistry
  {$else}
   {$IFDEF DUNITX}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility
   {$ELSE}
    TestFramework
   {$ENDIF}
  {$endif};

type
  {$ifdef FPC}
  TTestCase = fpcunit.TTestCase;
  {$else}
   {$IFDEF DUNITX}
  TTestCase = DUnitX.DUnitCompatibility.TTestCase;
   {$ELSE}
  TTestCase = TestFramework.TTestCase;
   {$ENDIF}
  {$endif}

    { Registra uma classe de testes. Use para manter compatibilidade entre DUnitX/DUnit/FPCunit.
      @param ATesteName é um nome para o Teste. Pode ser passado vazio.
      @param ATestClass é a classe de teste }
 procedure _RegisterTest(ATesteName: String; ATestClass: TClass);


implementation

procedure _RegisterTest(ATesteName: String; ATestClass: TClass);
begin
  {$IfDef DUNITX}
   TDUnitX.RegisterTestFixture(ATestClass, ATesteName + ATestClass.ClassName);
  {$ELSE}
   RegisterTest(ATesteName, TTestCaseClass(ATestClass){$IfNDef FPC}.Suite{$EndIf} );
  {$EndIf}
end;



end.
