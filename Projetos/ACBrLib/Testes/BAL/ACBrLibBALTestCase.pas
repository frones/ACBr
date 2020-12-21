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

unit ACBrLibBALTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibBALNome = 'ACBrLibBAL';

type

  { TTestACBrBALLib }

  TTestACBrBALLib = class(TTestCase)
  published
    procedure Test_BAL_Inicializar_Com_DiretorioInvalido;
    procedure Test_BAL_Inicializar;
    procedure Test_BAL_Inicializar_Ja_Inicializado;
    procedure Test_BAL_Finalizar;
    procedure Test_BAL_Finalizar_Ja_Finalizado;
    procedure Test_BAL_Nome_Obtendo_LenBuffer;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_BAL_Versao;
    procedure Test_BAL_ConfigLerValor;
    procedure Test_BAL_ConfigGravarValor;

    procedure Test_BAL_Ativar;
    procedure Test_BAL_SolicitarPeso;
    procedure Test_BAL_LePeso;
    procedure Test_BAL_InterpretarRespostaPeso;
    procedure Test_BAL_Desativar;
  end;

implementation

uses
  ACBrLibBALStaticImportMT, ACBrLibBALConsts, ACBrLibConsts, ACBrUtil, Dialogs;

procedure TTestACBrBALLib.Test_BAL_Inicializar_Com_DiretorioInvalido;
var
  Handle: longint;
begin

  try
    BAL_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, BAL_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrBALLib.Test_BAL_Inicializar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Inicializar_Ja_Inicializado;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Finalizar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Finalizar_Ja_Finalizado;
var
  Handle: longint;
begin

  try
    AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, BAL_Finalizar(Handle));
    //AssertEquals(ErrOk, BAL_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrBALLib.Test_BAL_Nome_Obtendo_LenBuffer;
var
  Handle: longint;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := 0;
  AssertEquals(ErrOk, BAL_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := Length(CLibBALNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(CLibBALNome, AStr);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := Length(CLibBALNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(CLibBALNome, AStr);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(copy(CLibBALNome,1,4), AStr);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Versao;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := 0;
  AssertEquals(ErrOk, BAL_Versao(Handle, Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_ConfigLerValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_ConfigLerValor(Handle, CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_ConfigGravarValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, BAL_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Ativar;
var
  Handle: longint;
begin
  // Ativando a Balança
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Ativar a Balança', ErrOk, BAL_Ativar(Handle));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_Desativar;
var
  Handle: longint;
begin
  // Desativando a Balança
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals('Erro ao Desativar a Balança', ErrOk, BAL_Desativar(Handle));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_LePeso;
var
  Handle: longint;
  Peso: Double;
  sPeso: String;
begin

  try
    // Iniciando a leitura do peso
    AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
    AssertEquals('Erro ao Iniciar a leitura do peso', ErrOk, BAL_LePeso(Handle, 3000, Peso));
    AssertEquals('Peso= ', FloatToStr(Peso));
    AssertEquals(ErrOk, BAL_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

procedure TTestACBrBALLib.Test_BAL_SolicitarPeso;
var
  Handle: longint;
begin
  // Solicitar o peso
  AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
  AssertEquals('Erro ao solicitar o peso', ErrOk, BAL_SolicitarPeso(Handle));
  AssertEquals(ErrOk, BAL_Finalizar(Handle));
end;

procedure TTestACBrBALLib.Test_BAL_InterpretarRespostaPeso;
var
  Handle: longint;
  Peso: Double;
  sPeso: String;
begin

  try
    // Interpretando a resposta
    AssertEquals(ErrOk, BAL_Inicializar(Handle,'',''));
    AssertEquals('Erro ao interpretar a resposta', ErrOk,
               BAL_InterpretarRespostaPeso(Handle, '125', Peso));
    sPeso := FloatToStr(Peso);
    AssertEquals('Peso= ', sPeso);
    AssertEquals(ErrOk, BAL_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end

end;

initialization
  RegisterTest(TTestACBrBALLib);

end.

