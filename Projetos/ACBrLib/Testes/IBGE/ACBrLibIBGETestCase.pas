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

const
  CLibIBGENome = 'ACBrLibIBGE';

type

  { TTestACBrIBGELib }

  TTestACBrIBGELib = class(TTestCase)
  published
    procedure Test_IBGE_Inicializar_Com_DiretorioInvalido;
    procedure Test_IBGE_Inicializar;
    procedure Test_IBGE_Inicializar_Ja_Inicializado;
    procedure Test_IBGE_Finalizar;
    procedure Test_IBGE_Finalizar_Ja_Finalizado;
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
  ACBrLibIBGEStaticImportMT, ACBrLibIBGEConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Com_DiretorioInvalido;
var
  Handle: longint;
begin
  AssertEquals(ErrDiretorioNaoExiste, IBGE_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar;
var
  Handle: longint;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Ja_Inicializado;
var
  Handle: longint;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar;
var
  Handle: longint;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar_Ja_Finalizado;
var
  Handle: longint;
begin

  Handle:=0;
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));

  AssertEquals(ErrOk, IBGE_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));

  try
    IBGE_Finalizar(Handle)
  except
    On E : Exception do
    if not (E is EAccessViolation) then
    Fail('Erro ao finalizar a Lib.');
  end;
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  Bufflen := Length(CLibIBGENome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  Bufflen := Length(CLibIBGENome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(copy(CLibIBGENome,1,4), AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_Versao;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  Bufflen := 0;
  AStr := '';

  AssertEquals(ErrOk, IBGE_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  AssertEquals(Length(CLibIBGEVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert (AStr <> '');
  AssertEquals(Length(CLibIBGEVersao), Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigLerValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(Handle, CSessaoVersao, CLibIBGENome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigGravarValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, IBGE_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorCodigo;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por código
  Resposta := '';
  Tamanho := 0;
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Buscar Por Código', 1, IBGE_BuscarPorCodigo(Handle, 3554003, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorNome;
var
  Handle: longint;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por nome
  Resposta := '';
  Tamanho := 0;
  AssertEquals(ErrOK, IBGE_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Buscar Por Nome', 1, IBGE_BuscarPorNome(Handle, PChar('Tatuí'), PChar('SP'), False, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOk, IBGE_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrIBGELib);

end.

