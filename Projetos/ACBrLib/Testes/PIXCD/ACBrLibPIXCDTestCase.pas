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
  private
    fCaminhoExec: String;
  public
    procedure SetUp; override;
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
    procedure Test_PIXCD_ConfigGravarValor_ScopesBancoBrasil;
    procedure Test_PIXCD_ConfigGravarValor_ScopesInvalidos;
    procedure Test_PIXCD_ConfigGravarValor_ScopeEmBranco;
    procedure Test_PIXCD_TestePSP;
    procedure Test_PIXCD_GerarQRCodeEstatico;
    procedure Test_PIXCD_ConsultarPIX;
    procedure Test_PIXCD_ConsultarPixRecebidos;
    procedure Test_PIXCD_ConsultarDevolucaoPix;
    procedure Test_PIXCD_ConsultarCobrancaImediata;
    procedure Test_PIXCD_ConsultarCobranca;
    procedure Test_PIXCD_SolicitarDevolucaoPix;
    procedure Test_PIXCD_RevisarCobrancaImediata;
    procedure Test_PIXCD_RevisarCobranca;
    procedure Test_PIXCD_CriarCobrancaImediata;
    procedure Test_PIXCD_CriarCobranca;
    procedure Test_PIXCD_CancelarCobrancaImediata;
    procedure Test_PIXCD_CancelarCobranca;
    procedure Test_PIXCD_ConsultarCobrancasCob;
    procedure Test_PIXCD_ConsultarCobrancasCobV;
    procedure Test_PIXCD_ConfigPSPSicoob;

    //Matera
    procedure Test_PIXCD_Matera_IncluirConta;
    procedure Test_PIXCD_Matera_ConsultarConta;
    procedure Test_PIXCD_Matera_InativarConta;
    procedure Test_PIXCD_Matera_IncluirChavePix;
    procedure Test_PIXCD_Matera_ConsultarChavePix;
    procedure Test_PIXCD_Matera_ExcluirChavePix;
    procedure Test_PIXCD_Matera_GerarQRCode_Normal;
    procedure Test_PIXCD_Matera_GerarQRCode_Vencimento;
    procedure Test_PIXCD_Matera_ConsultarTransacao;
    procedure Test_PIXCD_Matera_ConsultarSaldoEC;
    procedure Test_PIXCD_Matera_ConsultarExtratoEC;
    procedure Test_PIXCD_Matera_ConsultarMotivosDevolucao;
    procedure Test_PIXCD_Matera_MateraSolicitarDevolucao;
    procedure Test_PIXCD_Matera_MateraConsultarAliasRetirada;
    procedure Test_PIXCD_Matera_MateraSolicitarRetirada;
  end;

implementation

uses
  ACBrLibPIXCDStaticImportMT, ACBrLibPIXCDConsts, ACBrLibConsts;

procedure TTestACBrPIXCDLib.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

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
  PIXCD_ConfigGravarValor(Handle, 'Principal','LogNivel', '4');
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

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigGravarValor_ScopesBancoBrasil;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk,
                     PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDBancoBrasilConfig, CChaveScopes, '[scCobWrite,scCobRead,scCobVWrite,scCobVRead,scPixWrite,scPixRead]'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDBancoBrasilConfig, CChaveScopes, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '[scCobWrite,scCobRead,scCobVWrite,scCobVRead,scPixWrite,scPixRead]', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigGravarValor_ScopesInvalidos;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk,
                     PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDBradescoConfig, CChaveScopes, 'ScopeInvalido'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDBradescoConfig, CChaveScopes, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '[]', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigGravarValor_ScopeEmBranco;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk,
                     PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDInterConfig, CChaveScopes, ''));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDInterConfig, CChaveScopes, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '[]', AStr);
  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_TestePSP;
var
  AStr: String;
  Handle: THandle;
  Bufflen: Integer;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '0'));  //Bradesco

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '1'));  //Itaú

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '2')); //Banco do Brasil

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '3')); //Santander

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '4')); //Shipay

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '5')); //Sicredi

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '6')); //Sicoob

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '7')); //PagSeguro

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '8')); //GerenciaNet

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '9')); //PixPDV

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '10')); //Inter

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '11')); //Ailos

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '13')); //Cielo

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '14')); //MercadoPago

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '15')); //Gate2All

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '16')); //Banrisul

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_ConfigLerValor(Handle, 'PIXCD', 'PSP', PChar(AStr), Bufflen));

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '17')); //C6Bank

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AStr := copy(AStr,1,Bufflen);

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_GerarQRCodeEstatico;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '3')); //Santander

  AssertEquals('Erro ao Informar Tipo de Chave', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'TipoChave', '1'));

  AssertEquals('Erro ao Informar Chave', ErrOk, PIXCD_ConfigGravarValor(Handle, 'Santander', 'ChavePIX', 'teste.email@email.com'));

  AssertEquals('Erro ao Informar Nome do Recebedor', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'NomeRecebedor', 'Nome'));

  AssertEquals('Erro ao Informar Cidade do Recebedor', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'CidadeRecebedor', 'Cidade'));

  AssertEquals('Erro ao Informar UF do Recebedor', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'UFRecebedor', 'UF'));

  AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

  AssertEquals(ErrOK, PIXCD_GerarQRCodeEstatico(Handle, 1, '', '', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarPIX;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  try
   AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
   Bufflen := 255;
   AStr := Space(Bufflen);
   AssertEquals(ErrOK, PIXCD_ConsultarPix(Handle, 'teste', PChar(AStr), Bufflen));
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarPixRecebidos;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
  DataInicio, DataFim: TDateTime;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  DataInicio:= EncodeDate(2023, 10, 18);
  DataFim:= EncodeDate(2023, 10, 19);

  AssertEquals(ErrOK, PIXCD_ConsultarPixRecebidos(Handle, DataInicio, DataFim, 'teste', 'teste', 1, 10, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarDevolucaoPix;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_ConsultarDevolucaoPix(Handle, 'teste', 'teste', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarCobrancaImediata;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_ConsultarCobrancaImediata(Handle, 'teste', 1, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarCobranca;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_ConsultarCobranca(Handle, 'teste', 1, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_SolicitarDevolucaoPix;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));
   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Solicitar Devolução', ErrOK,
   PIXCD_SolicitarDevolucaoPix(Handle, PChar(fCaminhoExec +'\DevolucaoSolicitada.ini'), 'teste', '1', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_RevisarCobrancaImediata;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
   try
    AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Revisar Cobrança Imediata', ErrOK,
    PIXCD_RevisarCobrancaImediata(Handle, PChar(fCaminhoExec +'\CobRevisada.ini'), 'teste', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
   except
     on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
   end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_RevisarCobranca;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
   try
    AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Revisar Cobrança', ErrOK,
    PIXCD_RevisarCobranca(Handle, PChar(fCaminhoExec +'\CobVRevisada.ini'), 'teste', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
   except
     on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
   end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_CriarCobrancaImediata;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
   try
    AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Criar Cobrança Imediata', ErrOK,
    PIXCD_CriarCobrancaImediata(Handle, PChar(fCaminhoExec +'\CobSolicitada.ini'), 'teste', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
   except
     on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
   end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_CriarCobranca;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
   try
    AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));
    Resposta:= '';
    Tamanho:= 0;

    AssertEquals('Erro ao Criar Cobrança', ErrOK,
    PIXCD_CriarCobranca(Handle, PChar(fCaminhoExec +'\CobVSolicitada.ini'), 'teste', Resposta, Tamanho));
    AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
    AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

    AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
   except
     on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
   end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_CancelarCobrancaImediata;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_CancelarCobrancaImediata(Handle, 'teste', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_CancelarCobranca;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals(ErrOK, PIXCD_CancelarCobranca(Handle, 'teste', PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarCobrancasCob;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
  DataInicio, DataFim: TDateTime;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  DataInicio:= EncodeDate(2024, 07, 01);
  DataFim:= EncodeDate(2024, 07, 22);

  AssertEquals(ErrOK, PIXCD_ConsultarCobrancasCob(Handle, DataInicio, DataFim, '', False, 2, 1, 10, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConsultarCobrancasCobV;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
  DataInicio, DataFim: TDateTime;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  DataInicio:= EncodeDate(2024, 07, 01);
  DataFim:= EncodeDate(2024, 07, 22);

  AssertEquals(ErrOK, PIXCD_ConsultarCobrancasCobV(Handle, DataInicio, DataFim, '', False, 2, 1, 10, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_ConfigPSPSicoob;
var
  Bufflen: Integer;
  AStr, Arq: String;
  Handle: THandle;
begin
  AssertEquals(ErrOK, PIXCD_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao definir PSP', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDConfig, CChavePSP, '6'));
  AssertEquals('Erro ao definir ChavePIX', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChavePIXSicoob, 'meuemail@mail.com'));
  AssertEquals('Erro ao definir ClientID', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, 'testeClientID'));
  AssertEquals('Erro ao definir TokenSandBox', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'));

  Arq := fCaminhoExec+'\Arq.key';
  AssertEquals('Erro ao definir ArqChavePrivada', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, PChar(Arq)));

  Arq := fCaminhoExec+'\Arq.cert';
  AssertEquals('Erro ao definir ArqCertificado', ErrOK, PIXCD_ConfigGravarValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, PChar(Arq)));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChavePIXSicoob, PChar(AStr), Bufflen));
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveClientIDSicoob, PChar(AStr), Bufflen));
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveTokenSandboxSicoob, PChar(AStr), Bufflen));
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqChavePrivadaSicoob, PChar(AStr), Bufflen));
  AssertEquals(ErrOk, PIXCD_ConfigLerValor(Handle, CSessaoPIXCDSicoobConfig, CChaveArqCertificadoSicoob, PChar(AStr), Bufflen));

  AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_IncluirConta;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Incluir Conta', ErrOK,
   PIXCD_Matera_IncluirConta(Handle, PChar(fCaminhoExec +'\IncluirConta.ini'), Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarConta;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Conta', ErrOK,
   PIXCD_Matera_ConsultarConta(Handle, 'accountid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_InativarConta;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Inativar Conta', ErrOK,
   PIXCD_Matera_InativarConta(Handle, 'accountid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_IncluirChavePix;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Incluir Chave PIX', ErrOK,
   PIXCD_Matera_IncluirChavePix(Handle, 'accountid', 'externalid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarChavePix;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Chave Pix', ErrOK,
   PIXCD_Matera_ConsultarChavePix(Handle, 'accountid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ExcluirChavePix;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Excluir Chave PIX', ErrOK,
   PIXCD_Matera_ExcluirChavePix(Handle, 'accountid', 'chavepix', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_GerarQRCode_Normal;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Gerar QRCode', ErrOK,
   PIXCD_Matera_GerarQRCode(Handle, PChar(fCaminhoExec +'\IncluirQRCode_Normal.ini'), Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_GerarQRCode_Vencimento;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Gerar QRCode', ErrOK,
   PIXCD_Matera_GerarQRCode(Handle, PChar(fCaminhoExec +'\IncluirQRCode_Vencto.ini'), Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarTransacao;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Transação', ErrOK,
   PIXCD_Matera_ConsultarTransacao(Handle, 'accountid', 'transactionid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarSaldoEC;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Saldo EC', ErrOK,
   PIXCD_Matera_ConsultarSaldoEC(Handle, 'accountid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarExtratoEC;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
  dataInicial, dataFinal: TDateTime;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   dataInicial:= EncodeDate(2024, 07, 12);
   dataFinal:= EncodeDate(2024, 07, 12);

   AssertEquals('Erro ao Consultar Extrato EC', ErrOK,
   PIXCD_Matera_ConsultarExtratoEC(Handle, 'accountid', dataInicial, dataFinal, Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_ConsultarMotivosDevolucao;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Motivos Devolução', ErrOK,
   PIXCD_Matera_ConsultarMotivosDevolucao(Handle, Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_MateraSolicitarDevolucao;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Solicitar Devolução', ErrOK,
   PIXCD_Matera_SolicitarDevolucao(Handle, PChar(fCaminhoExec +'\SolicitarDevolucao.ini'), 'accountid', 'transactionid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_MateraConsultarAliasRetirada;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Consultar Alias Retirada', ErrOK,
   PIXCD_Matera_ConsultarAliasRetirada(Handle, 'accountid', 'alias', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrPIXCDLib.Test_PIXCD_Matera_MateraSolicitarRetirada;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  try
   AssertEquals(ErrOk, PIXCD_Inicializar(Handle, '', ''));

   AssertEquals('Erro ao Mudar PSP', ErrOk, PIXCD_ConfigGravarValor(Handle, 'PIXCD', 'PSP', '12')); //Matera
   AssertEquals(ErrOK, PIXCD_ConfigGravar(Handle,'ACBrLib.ini'));

   Resposta:= '';
   Tamanho:= 0;

   AssertEquals('Erro ao Solicitar Retirada', ErrOK,
   PIXCD_Matera_SolicitarRetirada(Handle, PChar(fCaminhoExec +'\SolicitarRetirada.ini'), 'accountid', Resposta, Tamanho));
   AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
   AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
   AssertEquals(ErrOK, PIXCD_Finalizar(Handle));
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

initialization

  RegisterTest(TTestACBrPIXCDLib);
end.

