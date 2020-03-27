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

unit ACBrLibeSocialTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBreSocialLib }

  TTestACBreSocialLib = class(TTestCase)
  published
    procedure Test_eSocial_Inicializar_Com_DiretorioInvalido;
    procedure Test_eSocial_Inicializar;
    procedure Test_eSocial_Inicializar_Ja_Inicializado;
    procedure Test_eSocial_Finalizar;
    procedure Test_eSocial_Finalizar_Ja_Finalizado;
    procedure Test_eSocial_Nome_Obtendo_LenBuffer;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_eSocial_Versao;
    procedure Test_eSocial_ConfigLerValor;
    procedure Test_eSocial_ConfigGravarValor;

    procedure Test_eSocial_LerArqIni;
    procedure Test_eSocial_Enviar;
    procedure Test_eSocial_Consultar;
  end;

implementation

uses
  ACBrLibeSocialStaticImport, ACBrLibeSocialConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, eSocial_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar;
begin
  AssertEquals(ErrOk, eSocial_Inicializar('',''));
end;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, eSocial_Inicializar('',''));
end;

procedure TTestACBreSocialLib.Test_eSocial_Finalizar;
begin
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, eSocial_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibeSocialNome), Bufflen);
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibeSocialNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibeSocialNome), Bufflen);
  AssertEquals(CLibeSocialNome, AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibeSocialNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibeSocialNome), Bufflen);
  AssertEquals(CLibeSocialNome, AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibeSocialNome,1,4), AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, eSocial_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibeSocialVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibeSocialVersao), Bufflen);
  AssertEquals(CLibeSocialVersao, AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_ConfigLerValor(CSessaoVersao, CLibeSocialNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibeSocialVersao, AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, eSocial_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBreSocialLib.Test_eSocial_LerArqIni;
begin
  // Lendo o arquivo INI
  AssertEquals('Erro ao ler o arquivo INI', ErrOk,
       eSocial_LerArqIni('C:\ACBr\trunk2\Projetos\ACBrLib\Testes\eSocial\bin\S1000.ini'));
end;

procedure TTestACBreSocialLib.Test_eSocial_Enviar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando o Envio
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Enviar', ErrOk, eSocial_Enviar(1, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBreSocialLib.Test_eSocial_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar', ErrOk, eSocial_Consultar('123456789', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

initialization
  RegisterTest(TTestACBreSocialLib);

end.

