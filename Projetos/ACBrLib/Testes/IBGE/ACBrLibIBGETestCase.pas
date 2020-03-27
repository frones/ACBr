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

unit ACBrLibIBGETestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrIBGELib }

  TTestACBrIBGELib = class(TTestCase)
  published
    procedure Test_IBGE_Inicializar_Com_DiretorioInvalido;
    procedure Test_IBGE_Inicializar;
    procedure Test_IBGE_Inicializar_Ja_Inicializado;
    procedure Test_IBGE_Finalizar;
    procedure Test_IBGE_Finalizar_Ja_Finalizado;
    procedure Test_IBGE_Nome_Obtendo_LenBuffer;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_IBGE_Versao;
    procedure Test_IBGE_ConfigLerValor;
    procedure Test_IBGE_ConfigGravarValor;

    procedure Test_IBGE_BuscarPorCodigo;
    procedure Test_IBGE_BuscarPorNome;
  end;

implementation

uses
  ACBrLibIBGEStaticImport, ACBrLibIBGEConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, IBGE_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar;
begin
  AssertEquals(ErrOk, IBGE_Inicializar('',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, IBGE_Inicializar('',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar;
begin
  AssertEquals(ErrOk, IBGE_Finalizar());
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, IBGE_Finalizar());
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, IBGE_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibIBGENome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibIBGENome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibIBGENome,1,4), AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, IBGE_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibIBGEVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGEVersao), Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(CSessaoVersao, CLibIBGENome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, IBGE_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorCodigo;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por código
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Buscar Por Código', ErrOk,
                              IBGE_BuscarPorCodigo(3554003, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorNome;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por nome
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Buscar Por Nome', ErrOk,
                     IBGE_BuscarPorNome('Araraquara', 'SP', False, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

initialization
  RegisterTest(TTestACBrIBGELib);

end.

