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

unit ACBrLibPIXCDTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Dialogs;

type

  { TTestACBrPIXCDLib }

  TTestACBrPIXCDLib= class(TTestCase)
  published
    procedure Test_PIXCD_Inicializar_Com_DiretorioInvalido;
    procedure Test_PIXCD_Inicializar;
    procedure Test_PIXCD_Inicializar_Ja_Inicializado;
    procedure Test_PIXCD_Finalizar;
    procedure Test_PIXCD_Finalizar_Ja_Finalizado;
    procedure Test_PIXCD_Nome_Obtendo_LenBuffer;
    procedure Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_PIXCD_Versao;
    procedure Test_PIXCD_ConfigLerValor;
    procedure Test_PIXCD_ConfigGravarValor;

  end;

implementation

uses
  ACBrLibPIXCDStaticImportMT, ACBrLibPIXCDConsts, ACBrLibConsts;

procedure TTestACBrPIXCDLib.Test_PIXCD_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrDiretorioNaoExiste, PIXCD_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: exception do
    ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Inicializar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, PIXCD_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));
   AssertEquals(ErrOk, PIXCD_Finalizar(Handle));
   //AssertEquals(ErrOk, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, PIXCD_Nome(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibPIXCDNome), Bufflen);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '',''));
  Bufflen := Length(CLibPIXCDNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPIXCDNome), Bufflen);
  AssertEquals(CLibPIXCDNome, AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Bufflen := Length(CLibPIXCDNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibPIXCDNome), Bufflen);
  AssertEquals(CLibPIXCDNome, AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Bufflen := 12;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(12, Bufflen);
  AssertEquals(copy(CLibPIXCDNome,1,19), AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Versao;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, PIXCD_Versao(Handle, Nil, Bufflen));
  AssertEquals(Length(CLibPIXCDVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPIXCDVersao), Bufflen);
  AssertEquals(CLibPIXCDVersao, AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  PIXCD_ConfigGravarValor(Handle, 'DFe','DadosPFX', '');
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, PIXCD_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrPIXCDLib);
end.

