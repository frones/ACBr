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

unit ACBrLibCEPTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrCEPLib }

  TTestACBrCEPLib = class(TTestCase)
  published
    procedure Test_CEP_Inicializar_Com_DiretorioInvalido;
    procedure Test_CEP_Inicializar;
    procedure Test_CEP_Inicializar_Ja_Inicializado;
    procedure Test_CEP_Finalizar;
    procedure Test_CEP_Finalizar_Ja_Finalizado;
    procedure Test_CEP_Nome_Obtendo_LenBuffer;
    procedure Test_CEP_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_CEP_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_CEP_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_CEP_Versao;
    procedure Test_CEP_ConfigLerValor;
    procedure Test_CEP_ConfigGravarValor;

    procedure Test_CEP_BuscarPorCEP;
    procedure Test_CEP_BuscarPorLogradouro;
  end;

implementation

uses
  ACBrLibCEPStaticImport, ACBrLibCEPConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrCEPLib.Test_CEP_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, CEP_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrCEPLib.Test_CEP_Inicializar;
begin
  AssertEquals(ErrOk, CEP_Inicializar('',''));
end;

procedure TTestACBrCEPLib.Test_CEP_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, CEP_Inicializar('',''));
end;

procedure TTestACBrCEPLib.Test_CEP_Finalizar;
begin
  AssertEquals(ErrOk, CEP_Finalizar());
end;

procedure TTestACBrCEPLib.Test_CEP_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, CEP_Finalizar());
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, CEP_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibCEPNome), Bufflen);
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibCEPNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCEPNome), Bufflen);
  AssertEquals(CLibCEPNome, AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibCEPNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibCEPNome), Bufflen);
  AssertEquals(CLibCEPNome, AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibCEPNome,1,4), AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, CEP_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibCEPVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCEPVersao), Bufflen);
  AssertEquals(CLibCEPVersao, AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_ConfigLerValor(CSessaoVersao, CLibCEPNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibCEPVersao, AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, CEP_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrCEPLib.Test_CEP_BuscarPorCEP;
var
  Qtde: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Buscando o Endereço por CEP
  Qtde := 0;
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao buscar o endereço por CEP', ErrOk,
    CEP_BuscarPorCEP('14802-406', Qtde, Resposta, Tamanho));

  if Qtde > 0 then
  begin
    AssertEquals('Qtde= ', '', IntToStr(Qtde));
    AssertEquals('Tamanho= ', '', IntToStr(Tamanho));
    AssertEquals('Resposta= ', '', AnsiString(Resposta));
  end;
end;

procedure TTestACBrCEPLib.Test_CEP_BuscarPorLogradouro;
var
  Qtde: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Buscando o CEP por Logradouro
  Qtde := 0;
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao buscar o CEP por Logradouro', ErrOk,
    CEP_BuscarPorLogradouro('araraquara', 'rua', 'italia', 'sp',
         '', Qtde, Resposta, Tamanho));

  if Qtde > 0 then
  begin
    AssertEquals('Qtde= ', '', IntToStr(Qtde));
    AssertEquals('Tamanho= ', '', IntToStr(Tamanho));
    AssertEquals('Resposta= ', '', AnsiString(Resposta));
  end;
end;

initialization
  RegisterTest(TTestACBrCEPLib);

end.

