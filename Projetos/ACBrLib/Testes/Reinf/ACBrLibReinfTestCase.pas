{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibReinfTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrReinfLib }

  TTestACBrReinfLib = class(TTestCase)
  published
    procedure Test_Reinf_Inicializar_Com_DiretorioInvalido;
    procedure Test_Reinf_Inicializar;
    procedure Test_Reinf_Inicializar_Ja_Inicializado;
    procedure Test_Reinf_Finalizar;
    procedure Test_Reinf_Finalizar_Ja_Finalizado;
    procedure Test_Reinf_Nome_Obtendo_LenBuffer;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_Reinf_Versao;
    procedure Test_Reinf_ConfigLerValor;
    procedure Test_Reinf_ConfigGravarValor;

    procedure Test_Reinf_LerArqIni;
    procedure Test_Reinf_Enviar;
    procedure Test_Reinf_Consultar;
  end;

implementation

uses
  ACBrLibReinfStaticImport, ACBrLibReinfConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, Reinf_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar;
begin
  AssertEquals(ErrOk, Reinf_Inicializar('',''));
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, Reinf_Inicializar('',''));
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar;
begin
  AssertEquals(ErrOk, Reinf_Finalizar());
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, Reinf_Finalizar());
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, Reinf_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibReinfNome), Bufflen);
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibReinfNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibReinfNome), Bufflen);
  AssertEquals(CLibReinfNome, AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibReinfNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibReinfNome), Bufflen);
  AssertEquals(CLibReinfNome, AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibReinfNome,1,4), AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, Reinf_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibReinfVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibReinfVersao), Bufflen);
  AssertEquals(CLibReinfVersao, AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_ConfigLerValor(CSessaoVersao, CLibReinfNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibReinfVersao, AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, Reinf_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrReinfLib.Test_Reinf_LerArqIni;
begin
  // Lendo o arquivo INI
  AssertEquals('Erro ao ler o arquivo INI', ErrOk,
       Reinf_LerArqIni('C:\ACBr\trunk2\Projetos\ACBrLib\Testes\Reinf\bin\R1000.ini'));
end;

procedure TTestACBrReinfLib.Test_Reinf_Enviar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando o Envio
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Enviar', ErrOk, Reinf_Enviar(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrReinfLib.Test_Reinf_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar', ErrOk, Reinf_Consultar('123456789', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

initialization
  RegisterTest(TTestACBrReinfLib);

end.

