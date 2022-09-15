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

unit ACBrLibNCMsTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibNCMsNome = 'ACBrLibNCMs';

type

  { TTestACBrNCMsLib }

  TTestACBrNCMsLib = class(TTestCase)
  published
    procedure Test_NCMs_Inicializar_Com_DiretorioInvalido;
    procedure Test_NCMs_Inicializar;
    procedure Test_NCMs_Inicializar_Ja_Inicializado;
    procedure Test_NCMs_Finalizar;
    procedure Test_NCMs_Finalizar_Ja_Finalizado;
    procedure Test_NCMs_Nome_Obtendo_LenBuffer;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_NCMs_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_NCMs_Versao;
    procedure Test_NCMs_ConfigLerValor;
    procedure Test_NCMs_ConfigGravarValor;

    //procedure Test_NCM_BuscarPorCEP;
    //procedure Test_NCM_BuscarPorLogradouro;
    //
    //procedure Test_NCM_BuscarPorCEPJSON;
    //procedure Test_NCM_BuscarPorCEPXML;

  end;

implementation

uses
  Dialogs, ACBrUtil.Strings, ACBrLibConsts, ACBrLibNCMsStaticImportMT;

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar_Com_DiretorioInvalido;
var
  Handle: TLibHandle;
begin
    AssertEquals(ErrDiretorioNaoExiste, NCM_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar;
var
  Handle: TLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Inicializar_Ja_Inicializado;
var
  Handle: TLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Finalizar;
var
  Handle: TLibHandle;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Finalizar_Ja_Finalizado;
var
  Handle: TLibHandle;
begin

  Handle:=0;
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, NCM_Finalizar(Handle));

  try
    AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, NCM_Finalizar(Handle));
    //AssertEquals(ErrOk, NCM_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Obtendo_LenBuffer;
var
  Handle: TLibHandle;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, NCM_Nome(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: TLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := Length(CLibNCMsNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: TLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := Length(CLibNCMsNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: TLibHandle;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNCMsNome), Bufflen);
  AssertEquals(copy(CLibNCMsNome,1,4), AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_Versao;
var
  Handle: TLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, NCM_Versao(Handle, Nil, Bufflen));
  Assert(Bufflen > 0);
  //AssertEquals(Length(CLibCEPVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_Versao(Handle, PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert (AStr <> '');
  //AssertEquals(Length(CLibCEPVersao), Bufflen);
  //AssertEquals(CLibCEPVersao, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_ConfigLerValor;
var
  Handle: TLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_ConfigLerValor(Handle, CSessaoVersao, CLibNCMsNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibNCMsNome, AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

procedure TTestACBrNCMsLib.Test_NCMs_ConfigGravarValor;
var
  Handle: TLibHandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NCM_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);

  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, NCM_Finalizar(Handle));
end;

//procedure TTestACBrNCMsLib.Test_NCM_BuscarPorCEP;
//var
//  Handle: TLibHandle;
//  Resposta: String;
//  Tamanho: Longint;
//begin
//  // Buscando o Endereço por CEP
//  Resposta := space(10240);
//  Tamanho := 10240;
//
//  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
//  AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoCEP, CChaveWebService, '10'));  // Via Cep
//  AssertEquals('Erro ao buscar o endereço por CEP', ErrOk,
//    NCM_BuscarPorCEP(Handle, '18272230', PChar(Resposta), Tamanho));
//
//  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
//  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
//  AssertEquals(ErrOk, NCM_Finalizar(Handle));
//end;
//
//procedure TTestACBrNCMsLib.Test_NCM_BuscarPorLogradouro;
//var
//  Handle: TLibHandle;
//  Resposta: String;
//  Tamanho: Longint;
//begin
//  // Buscando o CEP por Logradouro
//  Resposta := space(10240);
//  Tamanho := 10240;
//
//  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
//  AssertEquals('Erro ao Mudar configuração', ErrOk, NCM_ConfigGravarValor(Handle, CSessaoCEP, CChaveWebService, '10'));  // Via Cep
//  AssertEquals('Erro ao buscar o CEP por Logradouro', ErrOk,
//    NCM_BuscarPorLogradouro(Handle, 'Tatuí', 'Rua', 'Caridade Terceira', 'SP', '',
//                            PChar(Resposta), Tamanho));
//
//  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
//  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
//  AssertEquals(ErrOk, NCM_Finalizar(Handle));
//end;
//
//procedure TTestACBrNCMsLib.Test_NCM_BuscarPorCEPJSON;
//var
//  Handle: TLibHandle;
//  Qtde: Integer;
//  Resposta: string;
//  Tamanho: Longint;
//  Retorno: PChar;
//  Tamanho2: Longint;
//begin
//  // Buscando o Endereço por CEP
//  Resposta := space(10240);
//  Tamanho := 10240;
//
//  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
//
//  NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveTipoResposta, '2');  // 2 -json
//  NCM_ConfigGravarValor(Handle, 'CEP', 'WebService', '11');
//
//  Qtde := NCM_BuscarPorCEP(Handle, '08717220', pchar(Resposta), Tamanho);
//
//  if Qtde <0 then
//  begin
//    NCM_UltimoRetorno(Handle, Retorno, Tamanho2 );
//  end;
//  AssertEquals('Erro ao buscar o endereço por CEP', ErrOk, Qtde);
//
//
//  if Qtde > 0 then
//  begin
//    AssertEquals('Qtde = ' + IntToStr(Qtde), '', '');
//    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
//    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
//  end;
//  AssertEquals(ErrOk, NCM_Finalizar(Handle));
//end;
//
//procedure TTestACBrNCMsLib.Test_NCM_BuscarPorCEPXML;
//var
//  Handle: TLibHandle;
//  Qtde: Integer;
//  Resposta: string;
//  Tamanho: Longint;
//  Retorno: PChar;
//  Tamanho2: Longint;
//begin
//  // Buscando o Endereço por CEP
//  Resposta := space(10240);
//  Tamanho := 10240;
//
//  AssertEquals(ErrOk, NCM_Inicializar(Handle, '',''));
//
//  NCM_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveTipoResposta, '1');  // 2 -json
//  NCM_ConfigGravarValor(Handle, 'CEP', 'WebService', '11');
//
//  Qtde := NCM_BuscarPorCEP(Handle, '08717220', pchar(Resposta), Tamanho);
//
//  if Qtde <0 then
//  begin
//    NCM_UltimoRetorno(Handle, Retorno, Tamanho2 );
//  end;
//  AssertEquals('Erro ao buscar o endereço por CEP', ErrOk, Qtde);
//
//
//  if Qtde > 0 then
//  begin
//    AssertEquals('Qtde = ' + IntToStr(Qtde), '', '');
//    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
//    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
//  end;
//  AssertEquals(ErrOk, NCM_Finalizar(Handle));
//end;

initialization
  RegisterTest(TTestACBrNCMsLib);

end.

