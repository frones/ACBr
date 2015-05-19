{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

unit ACBrProvedorGovBR;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorGovBR }

 TProvedorGovBR = class(TProvedorClass)
  protected
   { protected }
  private
   { private }
  public
   { public }
   Constructor Create;

   function GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade; OverRide;
   function GetConfigSchema(ACodCidade: Integer): TConfigSchema; OverRide;
   function GetConfigURL(ACodCidade: Integer): TConfigURL; OverRide;
   function GetURI(URI: String): String; OverRide;
   function GetAssinarXML(Acao: TnfseAcao): Boolean; OverRide;
   function GetValidarLote: Boolean; OverRide;

   function Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4, NameSpaceDad, Identificador, URI: String): AnsiString; OverRide;
   function Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados, NameSpaceCab: String; ACodCidade: Integer): AnsiString; OverRide;
   function Gera_DadosSenha(CNPJ, Senha: String): AnsiString; OverRide;
   function Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString; OverRide;

   function GeraEnvelopeRecepcionarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarSituacaoLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSeporRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeGerarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;
   function GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; OverRide;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; OverRide;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; OverRide;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; OverRide;
  end;

implementation

{ TProvedorGovBR }

constructor TProvedorGovBR.Create;
begin
 {----}
end;

function TProvedorGovBR.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := 'tc:'; // Alterado por Italo em 05/02/2014
  ConfigCidade.Identificador := 'id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org';

//  ConfigCidade.AssinaRPS  := False;
//  ConfigCidade.AssinaLote := False;

// Alterado por Luiz Baião em 10/12/2014
// Segundo: http://www.contagem.mg.gov.br/arquivos/downloads/modelo_integracao_nfs-e.pdf
// Página: 12 - Item: 2.2.5 Uso de Assinatura com Certificado Digital
// Abaixo segue as informações que deverão ser assinadas e quem deverá fazê-lo em cada momento:
// - O RPS, pelo contribuinte, antes do envio do mesmo através do Lote de RPS;
// - O Lote de RPS, pelo contribuinte, antes do envio do mesmo;
// - A NFS-e:
// 	  * Pela prefeitura e pelo contribuinte, quando gerada pela Aplicação On Line;
//	  * Pela prefeitura nos demais casos;
// - O Pedido de cancelamento da NFS-e, pelo contribuinte;
// - A Confirmação de cancelamento da NFS-e, pela prefeitura;

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorGovBR.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1.00';
  ConfigSchema.VersaoDados           := '1.00';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://tempuri.org/';
  ConfigSchema.Cabecalho             := '';
  ConfigSchema.ServicoEnviar         := 'servico_enviar_lote_rps_envio.xsd';
  ConfigSchema.ServicoConSit         := 'servico_consultar_situacao_lote_rps_envio.xsd';
  ConfigSchema.ServicoConLot         := 'servico_consultar_lote_rps_envio.xsd';
  ConfigSchema.ServicoConRps         := 'servico_consultar_nfse_rps_envio';
  ConfigSchema.ServicoConNfse        := 'servico_consultar_nfse_envio';
  ConfigSchema.ServicoCancelar       := 'servico_cancelar_nfse_envio';
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := 'tipos_complexos.xsd'; // Alterado por Italo em 05/02/2014

  Result := ConfigSchema;
end;

function TProvedorGovBR.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
 case ACodCidade of
  3541505: begin // Presidente Venceslau/SP
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://mail.presidentevenceslau.sp.gov.br/NFSEWSTESTE/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://mail.presidentevenceslau.sp.gov.br/NFSEWS/Services.svc';
           end;
  4101408: begin // Apucarana/PR
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := '';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://cetil.apucarana.pr.gov.br/NFSEWS/Services.svc';
           end;
  4309407: begin // Guapore/RS
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://177.20.255.245/NFSEWSTESTE/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://177.20.255.244/NFSEWS/Services.svc';
           end;
  4321709: begin // Tres Coroas/RS
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://nfseteste.pmtcoroas.com.br/nfsewsteste/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.pmtcoroas.com.br/nfsews/Services.svc';
           end;
 end;

  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProGerarNFSe          := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProRecepcaoSincrono   := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProSubstituiNFSe      := ConfigURL.ProRecepcaoLoteRPS;

  Result := ConfigURL;
end;

function TProvedorGovBR.GetURI(URI: String): String;
begin
 // No provedor GovBR a URI não é informada.
 Result := '';
end;

function TProvedorGovBR.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := False;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := False;
   acCancelar:    Result := False;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
   acConsSecRps:  Result := False;
 end;
end;

function TProvedorGovBR.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorGovBR.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio' + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorGovBR.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorGovBR.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorGovBR.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorGovBR.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<RecepcionarLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</RecepcionarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarSituacaoLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarSituacaoLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNfsePorRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarNfsePorRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNfse xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<CancelarNfse xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                DadosMsg +
              '</xmlEnvio>' +
             '</CancelarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovBR.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorGovBR.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorGovBR.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorGovBR.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://tempuri.org/INFSEGeracao/RecepcionarLoteRps';
   acConsSit:     Result := 'http://tempuri.org/INFSEConsultas/ConsultarSituacaoLoteRps';
   acConsLote:    Result := 'http://tempuri.org/INFSEConsultas/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://tempuri.org/INFSEConsultas/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://tempuri.org/INFSEConsultas/ConsultarNfse';
   acCancelar:    Result := 'http://tempuri.org/INFSEGeracao/CancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorGovBR.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'RecepcionarLoteRpsResult' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRpsResult' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'ConsultarLoteRpsResult' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ConsultarNfsePorRpsResult' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ConsultarNfseResult' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'CancelarNfseResult' );
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorGovBR.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorGovBR.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
