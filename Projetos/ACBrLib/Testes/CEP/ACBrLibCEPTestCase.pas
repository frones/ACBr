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

const
  CLibCEPNome = 'ACBrLibCEP';

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
  ACBrLibCEPStaticImportMT, ACBrLibCEPConsts, ACBrLibConsts, ACBrUtil, Dialogs;

procedure TTestACBrCEPLib.Test_CEP_Inicializar_Com_DiretorioInvalido;
var
  Handle: longint;
begin
    AssertEquals(ErrDiretorioNaoExiste, CEP_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrCEPLib.Test_CEP_Inicializar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Inicializar_Ja_Inicializado;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Finalizar;
var
  Handle: longint;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Finalizar_Ja_Finalizado;
var
  Handle: longint;
begin
  try
    AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
    AssertEquals(ErrOk, CEP_Finalizar(Handle));
    //AssertEquals(ErrOk, CEP_Finalizar(Handle));
  except
  on E: Exception do
     ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Obtendo_LenBuffer;
var
  Handle: longint;
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  Bufflen := 0;
  AssertEquals(ErrOk, CEP_Nome(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibCEPNome), Bufflen);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Identico;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  Bufflen := Length(CLibCEPNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCEPNome), Bufflen);
  AssertEquals(CLibCEPNome, AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Maior;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  Bufflen := Length(CLibCEPNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibCEPNome), Bufflen);
  AssertEquals(CLibCEPNome, AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Nome_Lendo_Buffer_Tamanho_Menor;
var
  Handle: longint;
  AStr: String;
  Bufflen: Integer;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibCEPNome,1,4), AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_Versao;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, CEP_Versao(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibCEPVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCEPVersao), Bufflen);
  AssertEquals(CLibCEPVersao, AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_ConfigLerValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_ConfigLerValor(Handle, CSessaoVersao, CLibCEPNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibCEPVersao, AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_ConfigGravarValor;
var
  Handle: longint;
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, CEP_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CEP_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_BuscarPorCEP;
var
  Handle: longint;
  Qtde: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Buscando o Endereço por CEP
  Qtde := 0;
  Resposta := '';
  Tamanho := 0;

  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals('Erro ao buscar o endereço por CEP', ErrOk,
    CEP_BuscarPorCEP(Handle, '14802-406', Qtde, Resposta, Tamanho));

  if Qtde > 0 then
  begin
    AssertEquals('Qtde= ', '', IntToStr(Qtde));
    AssertEquals('Tamanho= ', '', IntToStr(Tamanho));
    AssertEquals('Resposta= ', '', AnsiString(Resposta));
  end;
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

procedure TTestACBrCEPLib.Test_CEP_BuscarPorLogradouro;
var
  Handle: longint;
  Qtde: Integer;
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Buscando o CEP por Logradouro
  Qtde := 0;
  Resposta := '';
  Tamanho := 0;

  AssertEquals(ErrOk, CEP_Inicializar(Handle, '',''));
  AssertEquals('Erro ao buscar o CEP por Logradouro', ErrOk,
    CEP_BuscarPorLogradouro(Handle, 'araraquara', 'rua', 'italia', 'sp',
         '', Qtde, Resposta, Tamanho));

  if Qtde > 0 then
  begin
    AssertEquals('Qtde= ', '', IntToStr(Qtde));
    AssertEquals('Tamanho= ', '', IntToStr(Tamanho));
    AssertEquals('Resposta= ', '', AnsiString(Resposta));
  end;
  AssertEquals(ErrOk, CEP_Finalizar(Handle));
end;

initialization
  RegisterTest(TTestACBrCEPLib);

end.

