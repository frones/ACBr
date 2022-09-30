{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibGTINTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Dialogs;

type

  { TTestACBrGTINLib }

  TTestACBrGTINLib = class(TTestCase)
  published
    procedure Test_GTIN_Inicializar_Com_DiretorioInvalido;
    procedure Test_GTIN_Inicializar;
    procedure Test_GTIN_Inicializar_Ja_Inicializado;
    procedure Test_GTIN_Finalizar;
    procedure Test_GTIN_Finalizar_Ja_Finalizado;
    procedure Test_GTIN_Nome_Obtendo_LenBuffer;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_GTIN_Versao;
    procedure Test_GTIN_ConfigLerValor;
    procedure Test_GTIN_ConfigGravarValor;

    procedure Test_GTIN_Consultar;

  end;

implementation

uses
  ACBrLibGTINStaticImportST, ACBrLibGTINConsts, ACBrLibConsts, ACBrUtil.Strings;

procedure TTestACBrGTINLib.Test_GTIN_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrDiretorioNaoExiste, GTIN_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: exception do
    ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrGTINLib.Test_GTIN_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar('',''));
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar('', ''));
  AssertEquals(ErrOK, GTIN_Inicializar('',''));
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  AssertEquals(ErrOk, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
  try
   AssertEquals(ErrOk, GTIN_Inicializar( '', ''));
   AssertEquals(ErrOk, GTIN_Finalizar());
   AssertEquals(ErrOk, GTIN_Finalizar());
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrGTINLib.Test_GTIN_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, GTIN_Nome(  Nil, Bufflen));
  AssertEquals(Length(CLibGTINNome), Bufflen);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar( '',''));
  Bufflen := Length(CLibGTINNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GTIN_Nome( PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGTINNome), Bufflen);
  AssertEquals(CLibGTINNome, AStr);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar( '', ''));
  Bufflen := Length(CLibGTINNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GTIN_Nome(  PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibGTINNome), Bufflen);
  AssertEquals(CLibGTINNome, AStr);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  Bufflen := 11;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GTIN_Nome(  PChar(AStr), Bufflen));
  AssertEquals(11, Bufflen);
  AssertEquals(copy(CLibGTINNome,1,11), AStr);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Versao;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, GTIN_Versao(  Nil, Bufflen));
  AssertEquals(Length(CLibGTINVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GTIN_Versao(  PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGTINVersao), Bufflen);
  AssertEquals(CLibGTINVersao, AStr);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOK, GTIN_ConfigLerValor(CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  GTIN_ConfigGravarValor('DFe','DadosPFX', '');
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, GTIN_Inicializar(  '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, GTIN_ConfigGravarValor(  CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GTIN_ConfigLerValor(  CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

procedure TTestACBrGTINLib.Test_GTIN_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  //Iniciando a consulta
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(ErrOK, GTIN_Inicializar( '',''));
  AssertEquals('Erro ao consultar', ErrExecutandoMetodo, GTIN_Consultar( '7897169400313', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOK, GTIN_Finalizar());
end;

initialization
  RegisterTest(TTestACBrGTINLib);

end.
