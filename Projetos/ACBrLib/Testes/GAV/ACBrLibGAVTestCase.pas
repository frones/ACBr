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

unit ACBrLibGAVTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrGAVLib }

  TTestACBrGAVLib = class(TTestCase)
  published
    procedure Test_GAV_Inicializar_Com_DiretorioInvalido;
    procedure Test_GAV_Inicializar;
    procedure Test_GAV_Inicializar_Ja_Inicializado;
    procedure Test_GAV_Finalizar;
    procedure Test_GAV_Finalizar_Ja_Finalizado;
    procedure Test_GAV_Nome_Obtendo_LenBuffer;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_GAV_Versao;
    procedure Test_GAV_ConfigLerValor;
    procedure Test_GAV_ConfigGravarValor;

    procedure Test_GAV_Ativar;
    procedure Test_GAV_AbreGaveta;
    procedure Test_GAV_Desativar;
  end;

implementation

uses
  ACBrLibGAVStaticImport, ACBrLibGAVConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrGAVLib.Test_GAV_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, GAV_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Inicializar;
begin
  AssertEquals(ErrOk, GAV_Inicializar('',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, GAV_Inicializar('',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Finalizar;
begin
  AssertEquals(ErrOk, GAV_Finalizar());
end;

procedure TTestACBrGAVLib.Test_GAV_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, GAV_Finalizar());
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GAV_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibGAVNome), Bufflen);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGAVNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGAVNome), Bufflen);
  AssertEquals(CLibGAVNome, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGAVNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibGAVNome), Bufflen);
  AssertEquals(CLibGAVNome, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibGAVNome,1,4), AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GAV_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibGAVVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGAVVersao), Bufflen);
  AssertEquals(CLibGAVVersao, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_ConfigLerValor(CSessaoVersao, CLibGAVNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibGAVVersao, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, GAV_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Ativar;
begin
  // Ativando a Gaveta
  AssertEquals('Erro ao Ativar a Gaveta', ErrOk, GAV_Ativar);
end;

procedure TTestACBrGAVLib.Test_GAV_Desativar;
begin
  // Desativando a Gaveta
  AssertEquals('Erro ao Desativar a Gaveta', ErrOk, GAV_Desativar);
end;

procedure TTestACBrGAVLib.Test_GAV_AbreGaveta;
begin
  // Abrindo a Gaveta
  AssertEquals('Erro ao abrir a Gaveta', ErrOk, GAV_AbreGaveta);
end;

initialization
  RegisterTest(TTestACBrGAVLib);

end.

