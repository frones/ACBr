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

unit ACBrLibConsultaCNPJTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Dialogs;

type

  { TTestACBrConsultaCNPJLib }

  TTestACBrConsultaCNPJLib= class(TTestCase)
  published
    procedure Test_ConsultaCNPJ_Inicializar_Com_DiretorioInvalido;
    procedure Test_ConsultaCNPJ_Inicializar;
    procedure Test_ConsultaCNPJ_Inicializar_Ja_Inicializado;
    procedure Test_ConsultaCNPJ_Finalizar;
    procedure Test_ConsultaCNPJ_Finalizar_Ja_Finalizado;
    procedure Test_ConsultaCNPJ_Nome_Obtendo_LenBuffer;
    procedure Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_ConsultaCNPJ_Versao;
    procedure Test_ConsultaCNPJ_ConfigLerValor;
    procedure Test_ConsultaCNPJ_ConfigGravarValor;

    procedure Test_ConsultaCNPJ_ConsultarCaptha;
    procedure Test_ConsultaCNPJ_Consultar;

  end;

implementation

uses
  ACBrLibConsultaCNPJStaticImportST, ACBrLibConsultaCNPJConsts, ACBrLibConsts, ACBrUtil.Strings;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrDiretorioNaoExiste, CNPJ_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: exception do
    ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar('',''));
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar('', ''));
  AssertEquals(ErrOK, CNPJ_Inicializar('',''));
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar('', ''));
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
  try
   AssertEquals(ErrOk, CNPJ_Inicializar( '', ''));
   AssertEquals(ErrOk, CNPJ_Finalizar());
   AssertEquals(ErrOk, CNPJ_Finalizar());
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, CNPJ_Inicializar(  '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, CNPJ_Nome(  Nil, Bufflen));
  AssertEquals(Length(CLibConsultaCNPJNome), Bufflen);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar( '',''));
  Bufflen := Length(CLibConsultaCNPJNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CNPJ_Nome( PChar(AStr), Bufflen));
  AssertEquals(Length(CLibConsultaCNPJNome), Bufflen);
  AssertEquals(CLibConsultaCNPJNome, AStr);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar( '', ''));
  Bufflen := Length(CLibConsultaCNPJNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CNPJ_Nome(  PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibConsultaCNPJNome), Bufflen);
  AssertEquals(CLibConsultaCNPJNome, AStr);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar(  '', ''));
  Bufflen := 19;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CNPJ_Nome(  PChar(AStr), Bufflen));
  AssertEquals(19, Bufflen);
  AssertEquals(copy(CLibConsultaCNPJNome,1,19), AStr);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Versao;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, CNPJ_Inicializar(  '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, CNPJ_Versao(  Nil, Bufflen));
  AssertEquals(Length(CLibConsultaCNPJVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CNPJ_Versao(  PChar(AStr), Bufflen));
  AssertEquals(Length(CLibConsultaCNPJVersao), Bufflen);
  AssertEquals(CLibConsultaCNPJVersao, AStr);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, CNPJ_Inicializar(  '', ''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOK, CNPJ_ConfigLerValor(CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
  CNPJ_ConfigGravarValor('DFe','DadosPFX', '');
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, CNPJ_Inicializar(  '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, CNPJ_ConfigGravarValor(  CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CNPJ_ConfigLerValor(  CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_ConsultarCaptha;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOK, CNPJ_Inicializar('',''));

  Bufflen:= 255;
  AStr:= Space(Bufflen);

  AssertEquals('Erro ao tentar criar Captcha', ErrOk, CNPJ_ConsultarCaptcha('..\bin', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr:= Space(Bufflen);
    AssertEquals(ErrOK, CNPJ_UltimoRetorno(PChar(AStr), Bufflen));
  end;
  AssertEquals(ErrOK, CNPJ_Finalizar());
end;

procedure TTestACBrConsultaCNPJLib.Test_ConsultaCNPJ_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(ErrOK, CNPJ_Inicializar('',''));
  AssertEquals('Erro ao consultar', ErrOk, CNPJ_Consultar('38251233000130','hv2zwd', Resposta, Tamanho));

  AssertEquals('Resposta =' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOK, CNPJ_Finalizar);
end;

initialization

  RegisterTest(TTestACBrConsultaCNPJLib);
end.

