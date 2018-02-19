{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira Moraes                          }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATMFe_integrador ;

interface

uses
  Classes, SysUtils,
  pcnGerador, pcnVFPeW, pcnVFPe,
  ACBrSATClass, ACBrIntegrador, ACBrBase;

const
  cNomeComponente = 'MF-e';

type

   { TACBrSATMFe_integrador_XML }

   TACBrSATMFe_integrador_XML = class( TACBrSATClass )
   private
     FIntegrador: TACBrIntegrador;
     FOldOnGetNumeroSessao: TACBrIntegradorGetNumeroSessao;
     FOldOnGravarLog: TACBrGravarLog;

     function AjustaComando(const Comando: String): String;

     procedure OnGetNumeroSessaoIntegrador(var ANumeroSessao: Integer);
     procedure OnGravarLogIntegrador(const ALogLine: String; var Tratado: Boolean);
   protected
     procedure LoadDLLFunctions ; override;
     procedure UnLoadDLLFunctions; override;

   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

     function AssociarAssinatura( CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; override;
     function AtivarSAT( subComando : Integer; CNPJ: AnsiString; cUF : Integer )
       : String ; override;
     function AtualizarSoftwareSAT : String ; override;
     function BloquearSAT : String ; override;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; override;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ; override;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString ) :
       String ; override;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
       override;
     function ConsultarSAT : String ; override ;
     function ConsultarStatusOperacional : String ; override;
     function DesbloquearSAT : String ; override;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; override;
     function ExtrairLogs : String ; override;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; override;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; override;

     function EnviarPagamento(Pagamento: TEnviarPagamento): TRespostaPagamento;
     function EnviarStatusPagamento(StatusPagamento: TStatusPagamento):
       TRespostaStatusPagamento;
     function VerificarStatusValidador(AVerificarStatusValidador: TVerificarStatusValidador):
       TRespostaVerificarStatusValidador;
     function RespostaFiscal(ARespostaFiscal: TRespostaFiscal): TRetornoRespostaFiscal;

   end;

implementation

Uses
  dateutils,
  pcnConversao,
  ACBrUtil, ACBrSAT;

constructor TACBrSATMFe_integrador_XML.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  FIntegrador := TACBrSAT(AOwner).Integrador;
  if not Assigned(FIntegrador) then
    raise EACBrSATErro.Create( cACBrSATSemIntegrador );

  FOldOnGetNumeroSessao := FIntegrador.OnGetNumeroSessao;
  FOldOnGravarLog := FIntegrador.OnGravarLog;

  FIntegrador.OnGetNumeroSessao := OnGetNumeroSessaoIntegrador;
  FIntegrador.OnGravarLog := OnGravarLogIntegrador;

  fpModeloStr := 'MFe_Integrador_XML' ;
end ;

destructor TACBrSATMFe_integrador_XML.Destroy;
begin
  FIntegrador.OnGetNumeroSessao := FOldOnGetNumeroSessao;
  FIntegrador.OnGravarLog := FOldOnGravarLog;

  inherited Destroy;
end;

procedure TACBrSATMFe_integrador_XML.LoadDLLFunctions;
begin
  //Não faz nada
end;

procedure TACBrSATMFe_integrador_XML.UnLoadDLLFunctions;
begin
  //Não faz nada
end;

procedure TACBrSATMFe_integrador_XML.OnGetNumeroSessaoIntegrador(
  var ANumeroSessao: Integer);
begin
  ANumeroSessao := Self.numeroSessao;
end;

procedure TACBrSATMFe_integrador_XML.OnGravarLogIntegrador(
  const ALogLine: String; var Tratado: Boolean);
begin
  TACBrSAT(Owner).DoLog( ALogLine );
  Tratado := True;
end;

function TACBrSATMFe_integrador_XML.AjustaComando(const Comando: String
  ): String;
begin
  Result := ChangeLineBreak(Comando,'');

  while pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);

  Result := StringReplace(Result, '> <', '><', [rfReplaceAll]);;
end;

function TACBrSATMFe_integrador_XML.AssociarAssinatura(CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'AssociarAssinatura';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['cnpjValue']        := CNPJvalue;
  FIntegrador.Parametros.Values['assinaturaCNPJs']  := assinaturaCNPJs;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.AtivarSAT(subComando : Integer ;
  CNPJ : AnsiString; cUF : Integer) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'AtivarMFe';
  FIntegrador.Parametros.Values['subComando']       := IntToStr(subComando);
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['CNPJ']             := CNPJ;
  FIntegrador.Parametros.Values['cUF']              := IntToStr(cUF);
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.AtualizarSoftwareSAT : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'AtualizarSoftwareMFe';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.BloquearSAT : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'BloquearMFe';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.CancelarUltimaVenda(chave,
  dadosCancelamento : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'CancelarUltimaVenda';
  FIntegrador.Parametros.Values['codigoDeAtivacao']  := codigoDeAtivacao;
  FIntegrador.Parametros.Values['chave']             := OnlyNumber(chave);
  FIntegrador.Parametros.Values['dadosCancelamento'] := '<![CDATA[' +AjustaComando(dadosCancelamento)+ ']]>';
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ComunicarCertificadoICPBRASIL(
  certificado : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ComunicarCertificadoICPBRASIL';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['certificado']      := certificado;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ConfigurarInterfaceDeRedeMFE';
  FIntegrador.Parametros.Values['codigoDeAtivacao']  := codigoDeAtivacao;
  FIntegrador.Parametros.Values['dadosConfiguracao'] := '<![CDATA[' +AjustaComando(dadosConfiguracao)+ ']]>';
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ConsultarNumeroSessao';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['cNumeroDeSessao']  := IntToStr(cNumeroDeSessao);
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ConsultarSAT : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ConsultarMFe';
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ConsultarStatusOperacional : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ConsultarStatusOperacional';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.DesbloquearSAT : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'DesbloquearMFe';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'EnviarDadosVenda';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['dadosVenda']       := '<![CDATA[' +AjustaComando(dadosVenda)+ ']]>';
  FIntegrador.Parametros.Values['nrDocumento']      := IntToStr(numeroSessao);
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.ExtrairLogs : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'ExtrairLogs';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.TesteFimAFim(dadosVenda : AnsiString) : String ;
begin
  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'TesteFimAFim';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['dadosVenda']       := '<![CDATA[' +AjustaComando(dadosVenda)+ ']]>';
  //FIntegrador.Parametros.Values['nrDocumento']      := IntToStr(numeroSessao);
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer; novoCodigo: AnsiString
  ): String;
begin
  if codigoDeAtivacaoOuEmergencia = '' then
    codigoDeAtivacaoOuEmergencia := codigoDeAtivacao;

  FIntegrador.Clear;
  FIntegrador.NomeComponente := cNomeComponente;
  FIntegrador.NomeMetodo := 'TrocarCodigoDeAtivacao';
  FIntegrador.Parametros.Values['codigoDeAtivacao'] := codigoDeAtivacao;
  FIntegrador.Parametros.Values['opcao']            := IntToStr(opcao);
  FIntegrador.Parametros.Values['novoCodigo']       := novoCodigo;
  FIntegrador.Parametros.Values['confNovoCodigo']   := novoCodigo;
  Result := FIntegrador.Enviar;
end ;

function TACBrSATMFe_integrador_XML.EnviarPagamento(Pagamento: TEnviarPagamento
  ): TRespostaPagamento;
begin
  TACBrSAT(Owner).DoLog('EnviarPagamento');

  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    Result := FIntegrador.EnviarPagamento(Pagamento);
  finally
    TACBrSAT(Owner).FinalizaComando( Result.XML );
  end;
end;

function TACBrSATMFe_integrador_XML.EnviarStatusPagamento(
  StatusPagamento: TStatusPagamento): TRespostaStatusPagamento;
begin
  TACBrSAT(Owner).DoLog('EnviarStatusPagamento');

  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    Result := FIntegrador.EnviarStatusPagamento(StatusPagamento);
  finally
    TACBrSAT(Owner).FinalizaComando( Result.XML );
  end;
end;

function TACBrSATMFe_integrador_XML.VerificarStatusValidador(
  AVerificarStatusValidador: TVerificarStatusValidador
  ): TRespostaVerificarStatusValidador;
begin
  TACBrSAT(Owner).DoLog('VerificarStatusValidador');

  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    Result := FIntegrador.VerificarStatusValidador(AVerificarStatusValidador);
  finally
    TACBrSAT(Owner).FinalizaComando( Result.XML );
  end;
end;

function TACBrSATMFe_integrador_XML.RespostaFiscal(
  ARespostaFiscal: TRespostaFiscal): TRetornoRespostaFiscal;
begin
  TACBrSAT(Owner).DoLog('RespostaFiscal');

  Result := Nil;
  TACBrSAT(Owner).IniciaComando;
  try
    Result := FIntegrador.RespostaFiscal(ARespostaFiscal);
  finally
    TACBrSAT(Owner).FinalizaComando( Result.XML );
  end;
end;

end.

