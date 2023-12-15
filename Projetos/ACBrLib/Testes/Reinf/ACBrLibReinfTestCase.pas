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
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestACBrReinfLib }

  TTestACBrReinfLib = class(TTestCase)
  private
    fCaminhoExec: string;
  public
    procedure SetUp; override;
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
  ACBrLibReinfStaticImportMT, ACBrLibReinfConsts, ACBrLibConsts, Dialogs, ACBrUtil.DateTime;

procedure TTestACBrReinfLib.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

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
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  try
    // Obtendo o Tamanho //
    AssertEquals(ErrOk, Reinf_Inicializar(Handle,'',''));

    Bufflen := 0;
    AssertEquals(ErrOk, Reinf_Nome(Handle,Nil, Bufflen));
    AssertEquals(Length(CLibReinfNome), Bufflen);

    AssertEquals(ErrOk, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '',''));

    Bufflen := Length(CLibReinfNome);
    AStr := Space(Bufflen);
    AssertEquals(ErrOk, Reinf_Nome(Handle, PChar(AStr), Bufflen));
    AssertEquals(Length(CLibReinfNome), Bufflen);
    AssertEquals(CLibReinfNome, AStr);

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    Bufflen := Length(CLibReinfNome)*2;
    AStr := Space(Bufflen);
    AssertEquals(ErrOk, Reinf_Nome(Handle, PChar(AStr), Bufflen));
    AStr := copy(AStr, 1, Bufflen);
    AssertEquals(Length(CLibReinfNome), Bufflen);
    AssertEquals(CLibReinfNome, AStr);

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle,  '', ''));

    Bufflen := 12;
    AStr := Space(Bufflen);
    AssertEquals(ErrOk, Reinf_Nome(Handle,  PChar(AStr), Bufflen));
    AssertEquals(12, Bufflen);
    AssertEquals(copy(CLibReinfNome,1,12), AStr);

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
    // Obtendo o Tamanho //
    AssertEquals(ErrOK, Reinf_Inicializar(Handle,  '', ''));

    Bufflen := 0;
    AssertEquals(ErrOk, Reinf_Versao(Handle, Nil, Bufflen));
    AssertEquals(Length(CLibReinfVersao), Bufflen);

    // Lendo a resposta //
    AStr := Space(Bufflen);
    AssertEquals(ErrOk, Reinf_Versao(Handle, PChar(AStr), Bufflen));
    AssertEquals(Length(CLibReinfVersao), Bufflen);
    AssertEquals(CLibReinfVersao, AStr);

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
    // Obtendo o Tamanho //
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    Bufflen := 255;
    AStr := Space(Bufflen);
    AssertEquals(ErrOk, Reinf_ConfigLerValor(Handle, CSessaoVersao, CLibReinfNome, PChar(AStr), Bufflen));
    AStr := copy(AStr,1,Bufflen);
    AssertEquals(CLibReinfVersao, AStr);

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  try
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
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_CriarEventoReinf;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Criar Evento do Reinf', ErrOK,
      Reinf_CriarEventoReinf(Handle, PChar(fCaminhoExec+'\evento.ini')));

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_EnviarReinf;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho := 0;

    AssertEquals('Erro ao Enviar Evento Reinf', ErrOK,
      Reinf_EnviarReinf(Handle, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_ConsultarReinf;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    Resposta := '';
    Tamanho := 0;

    AssertEquals('Erro ao Consultar Protocolo', ErrOK,
      Reinf_ConsultarReinf(Handle, '', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_ConsultarReciboReinf;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    Resposta := '';
    Tamanho := 0;

    AssertEquals('Erro ao Consultar Recibo', ErrOK,
      Reinf_ConsultarReciboReinf(Handle, '', 0, '', '', '', '', '', '', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_CriarEnviarReinf;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    Resposta := '';
    Tamanho := 0;

    AssertEquals('Erro ao Criar e Enviar Evento Reinf', ErrOK,
      Reinf_CriarEnviarReinf(Handle, PChar(fCaminhoExec+'\evento.ini'), Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_LimparReinf;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Limpar Reinf', ErrOK,
      Reinf_LimparReinf(Handle));

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_CarregarXMLEventoReinf;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Carregar XML do Reinf', ErrOK,
      Reinf_CarregarXMLEventoReinf(Handle, PChar(fCaminhoExec+'\evento.xml')));

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_SetIDContribuinte;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    AssertEquals(ErrOk, Reinf_SetIDContribuinte(Handle, '1234'));
    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_SetIDTransmissor;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    AssertEquals(ErrOk, Reinf_SetIDTransmissor(Handle, '1234'));
    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_SetTipoContribuinte;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    AssertEquals(ErrOk, Reinf_SetTipoContribuinte(Handle, 1));
    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_SetVersaoDF;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    AssertEquals(ErrOk, Reinf_SetVersaoDF(Handle, '2_01_02'));
    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_ObterCertificados;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Obter Certificados', ErrOK,
      Reinf_ObterCertificados(Handle, Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrReinfLib.Test_Reinf_Validar;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrOK, Reinf_Inicializar(Handle, '', ''));

    AssertEquals('Erro ao Validar Reinf', ErrOK,
      Reinf_Validar(Handle));

    AssertEquals(ErrOK, Reinf_Finalizar(Handle));
  except
    on E: Exception do
      ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

initialization
  RegisterTest(TTestACBrReinfLib);

end.

