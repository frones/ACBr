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

unit ACBrLibReinfTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrReinfLib }

  TTestACBrReinfLib = class(TTestCase)
  published
    procedure Test_Reinf_Inicializar_Com_DiretorioInvalido;
    procedure Test_Reinf_Inicializar;
    procedure Test_Reinf_Inicializar_Ja_Inicializado;
    procedure Test_Reinf_Finalizar;
    procedure Test_Reinf_Finalizar_Ja_Finalizado;
    procedure Test_Reinf_Nome_Obtendo_LenBuffer;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_Reinf_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_Reinf_Versao;
    procedure Test_Reinf_ConfigLerValor;
    procedure Test_Reinf_ConfigGravarValor;
    procedure Test_Reinf_CriarEventoReinf;
    procedure Test_Reinf_EnviarReinf;
    procedure Test_Reinf_ConsultarReinf;
    procedure Test_Reinf_ConsultarReciboReinf;
    procedure Test_Reinf_CriarEnviarReinf;
    procedure Test_Reinf_LimparReinf;
    procedure Test_Reinf_CarregarXMLEventoReinf;
    procedure Test_Reinf_SetIDContribuinte;
    procedure Test_Reinf_SetIDTransmissor;
    procedure Test_Reinf_SetTipoContribuinte;
    procedure Test_Reinf_SetVersaoDF;
    procedure Test_Reinf_ObterCertificados;
    procedure Test_Reinf_Validar;

  end;

implementation

uses
  ACBrLibReinfStaticImportMT, ACBrLibReinfConsts, ACBrLibConsts, Dialogs;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //Reinf_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, Reinf_Inicializar(Handle,'C:\NAOEXISTE\ACBrLib.ini',''));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
    //AssertEquals(ErrOk, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, Reinf_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibReinfNome), Bufflen);

  AssertEquals(ErrOk, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, Reinf_Inicializar(Handle, '',''));

  Bufflen := Length(CLibReinfNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibReinfNome), Bufflen);
  AssertEquals(CLibReinfNome, AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibReinfNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibReinfNome), Bufflen);
  AssertEquals(CLibReinfNome, AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, Reinf_Inicializar(Handle,  '', ''));

  Bufflen := 12;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(12, Bufflen);
  AssertEquals(copy(CLibReinfNome,1,12), AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, Reinf_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, Reinf_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibReinfVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibReinfVersao), Bufflen);
  AssertEquals(CLibReinfVersao, AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_ConfigLerValor(Handle, CSessaoVersao, CLibReinfNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibReinfVersao, AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, Reinf_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Reinf_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, Reinf_Finalizar(Handle));
end;

procedure TTestACBrReinfLib.Test_Reinf_CriarEventoReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_EnviarReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_ConsultarReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_ConsultarReciboReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_CriarEnviarReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_LimparReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_CarregarXMLEventoReinf;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_SetIDContribuinte;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_SetIDTransmissor;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_SetTipoContribuinte;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_SetVersaoDF;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_ObterCertificados;
begin

end;

procedure TTestACBrReinfLib.Test_Reinf_Validar;
begin

end;

initialization
  RegisterTest(TTestACBrReinfLib);

end.

