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

unit ACBrProvedorVirtual;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorVirtual }

 TProvedorVirtual = class(TProvedorClass)
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

{ TProvedorVirtual }

constructor TProvedorVirtual.Create;
begin
  {----}
end;

function TProvedorVirtual.GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
 	ConfigCidade.VersaoSoap        := '1.1';
 	ConfigCidade.Prefixo2          := '';
 	ConfigCidade.Prefixo3          := '';
 	ConfigCidade.Prefixo4          := '';
 	ConfigCidade.Identificador     := 'Id';
  ConfigCidade.QuebradeLinha     := ';';
	ConfigCidade.NameSpaceEnvelope := 'http://www.abrasf.org.br';

 	ConfigCidade.AssinaRPS   := True;
 	ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

 	Result := ConfigCidade;
end;

function TProvedorVirtual.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '2.02';
  ConfigSchema.VersaoDados           := '1.00';
  ConfigSchema.VersaoXML             := '2';
  ConfigSchema.NameSpaceXML          := 'http://www.abrasf.org.br/';
  ConfigSchema.Cabecalho             := 'nfse.xsd';
  ConfigSchema.ServicoEnviar         := 'nfse.xsd';
  ConfigSchema.ServicoConSit         := 'nfse.xsd';
  ConfigSchema.ServicoConLot         := 'nfse.xsd';
  ConfigSchema.ServicoConRps         := 'nfse.xsd';
  ConfigSchema.ServicoConNfse        := 'nfse.xsd';
  ConfigSchema.ServicoCancelar       := 'nfse.xsd';
  ConfigSchema.ServicoGerar          := 'nfse.xsd';
  ConfigSchema.ServicoEnviarSincrono := 'nfse.xsd';
  ConfigSchema.ServicoSubstituir     := 'nfse.xsd';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorVirtual.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 	ConfigURL: TConfigURL;
begin
 	ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomConsultaLoteRPS    := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomConsultaNFSeRPS    := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomConsultaSitLoteRPS := '';
  ConfigURL.HomConsultaNFSe       := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomCancelaNFSe        := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomGerarNFSe          := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomRecepcaoSincrono   := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';
  ConfigURL.HomSubstituiNFSe      := 'http://servidor1.virtualtechnologia.com.br:81/WebServiceSCEMJavaEnvironment/servlet/awsnfsebarradogarcas';

 	ConfigURL.ProNomeCidade         := '';
  ConfigURL.ProRecepcaoLoteRPS    := '';
  ConfigURL.ProConsultaLoteRPS    := '';
  ConfigURL.ProConsultaNFSeRPS    := '';
  ConfigURL.ProConsultaSitLoteRPS := '';
  ConfigURL.ProConsultaNFSe       := '';
  ConfigURL.ProCancelaNFSe        := 'https://financas2.barradogarcas.mt.gov.br:8080/SCEM/servlet/acancelarnfse_barradogarcas'; // ?wsdl';
  ConfigURL.ProGerarNFSe          := 'https://financas2.barradogarcas.mt.gov.br:8080/SCEM/servlet/agerarnfse_barradogarcas'; //?wsdl';
  ConfigURL.ProRecepcaoSincrono   := '';
  ConfigURL.ProSubstituiNFSe      := '';

 	Result := ConfigURL;
end;

function TProvedorVirtual.GetURI(URI: String): String;
begin
  Result := URI;
end;

function TProvedorVirtual.GetAssinarXML(Acao: TnfseAcao): Boolean;
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
  end;
end;

function TProvedorVirtual.GetValidarLote: Boolean;
begin
  Result := False;
end;

function TProvedorVirtual.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRps' + NameSpaceDad;
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

function TProvedorVirtual.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
             '<versaoDados>' + VersaoDados + '</versaoDados>'+
            '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorVirtual.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
  Result := '';
end;

function TProvedorVirtual.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRps>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorVirtual.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := SeparaDados( DadosMsg, 'EnviarLoteRpsEnvio' );
  result := '<?xml version="1.0" encoding="utf-8"?>' +
            '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
                               'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" ' +
                               'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                               'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
             '<SOAP-ENV:Body>' +
              '<WSNfseBarraDoGarcas.RECEPCIONARLOTERPS>' +
               '<Entrada xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 '<nfseCabecMsg>' +
                   CabMsg +
                 '</nfseCabecMsg>' +
                 '<nfseDadosMsg>' +
                   DadosMsg +
                 '</nfseDadosMsg>' +
               '</Entrada>' +
              '</WSNfseBarraDoGarcas.RECEPCIONARLOTERPS>' +
             '</SOAP-ENV:Body>' +
            '</SOAP-ENV:Envelope>';
end;

function TProvedorVirtual.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorVirtual.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorVirtual.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorVirtual.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorVirtual.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := SeparaDados( DadosMsg, 'CancelarNfseEnvio' );
  result := '<?xml version="1.0" encoding="utf-8"?>' +
            '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
                               'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" ' +
                               'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                               'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
             '<SOAP-ENV:Body>' +
              '<cancelarnfse_barradogarcas.Execute>' +
               '<Entrada xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 '<nfseCabecMsg>' +
                   CabMsg +
                 '</nfseCabecMsg>' +
                 '<nfseDadosMsg>' +
                   DadosMsg +
                 '</nfseDadosMsg>' +
               '</Entrada>' +
              '</cancelarnfse_barradogarcas.Execute>' +
             '</SOAP-ENV:Body>' +
            '</SOAP-ENV:Envelope>';
end;

function TProvedorVirtual.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := SeparaDados( DadosMsg, 'GerarNfseEnvio' );
  result := '<?xml version="1.0" encoding="utf-8"?>' +
            '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
                               'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" ' +
                               'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                               'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
             '<SOAP-ENV:Body>' +
              '<gerarnfse_barradogarcas.Execute>' +
               '<Entrada xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 '<nfseCabecMsg>' +
                   CabMsg +
                 '</nfseCabecMsg>' +
                 '<nfseDadosMsg>' +
                   DadosMsg +
                 '</nfseDadosMsg>' +
               '</Entrada>' +
              '</gerarnfse_barradogarcas.Execute>' +
             '</SOAP-ENV:Body>' +
            '</SOAP-ENV:Envelope>';
end;

function TProvedorVirtual.GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorVirtual.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorVirtual.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
  case Acao of
   acRecepcionar: Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.RECEPCIONARLOTERPS';
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.CONSULTARLOTERPS';
   acConsNFSeRps: Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.CONSULTARNFSEPORRPS';
   acConsNFSe:    Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.CONSULTARNFSEFAIXA';
   acCancelar:    Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.CANCELARNFSE';
   acGerar:       Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.GERARNFSE';
   acRecSincrono: Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.RECEPCIONARLOTERPSSINCRONO';
   acSubstituir:  Result := 'http://nfse.abrasf.org.braction/AWSNFSEBARRADOGARCAS.SUBSTITUIRNFSE';
  end;
end;

function TProvedorVirtual.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := RetornoWS;
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := RetornoWS;
   acConsNFSeRps: Result := RetornoWS;
   acConsNFSe:    Result := RetornoWS;
   acCancelar:    Result := RetornoWS; // SeparaDados( RetornoWS, 'nfse_barradogarcas.CANCELARNFSEResponse' );
   acGerar:       Result := RetornoWS; // SeparaDados( RetornoWS, 'Saida' );
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
  end;
end;

function TProvedorVirtual.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNFSe +
            '</CompNfse>';
end;

function TProvedorVirtual.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  // Não implementada
  Result := ''; 
end;

end.
