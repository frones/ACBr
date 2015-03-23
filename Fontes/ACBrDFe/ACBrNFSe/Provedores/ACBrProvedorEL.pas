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

unit ACBrProvedorEL;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorEL }

 TProvedorEL = class(TProvedorClass)
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
   function FinalizaSessao(hashIdent: String): AnsiString;

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

{ TProvedorEL }

constructor TProvedorEL.Create;
begin
 {----}
end;

function TProvedorEL.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  ConfigCidade.NameSpaceEnvelope := 'http://des36.el.com.br:8080/el-issonline/';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := False;

  Result := ConfigCidade;
end;

function TProvedorEL.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1.00';
  ConfigSchema.VersaoDados           := '1.00';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://www.el.com.br/nfse/xsd/';
  ConfigSchema.Cabecalho             := 'el-nfse.xsd';
  ConfigSchema.ServicoEnviar         := 'el-nfse.xsd';
  ConfigSchema.ServicoConSit         := 'el-nfse.xsd';
  ConfigSchema.ServicoConLot         := 'el-nfse.xsd';
  ConfigSchema.ServicoConRps         := 'el-nfse.xsd';
  ConfigSchema.ServicoConNfse        := 'el-nfse.xsd';
  ConfigSchema.ServicoCancelar       := 'el-nfse.xsd';
  ConfigSchema.ServicoGerar          := 'el-nfse.xsd';
  ConfigSchema.ServicoEnviarSincrono := 'el-nfse.xsd';
  ConfigSchema.ServicoSubstituir     := 'el-nfse.xsd';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorEL.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
  nfse: String;
begin
  
     case ACodCidade of
          3203205: begin // Linhares/ES
                        ConfigURL.HomNomeCidade := 'notafiscal.linhares.es';
                        ConfigURL.ProNomeCidade := 'notafiscal.linhares.es';
                        nfse                    := '/el-nfse';

                        ConfigURL.HomRecepcaoLoteRPS    := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.HomConsultaLoteRPS    := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.HomConsultaNFSeRPS    := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.HomConsultaSitLoteRPS := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.HomConsultaNFSe       := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.HomCancelaNFSe        := 'https://' + ConfigURL.HomNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';

                        ConfigURL.ProRecepcaoLoteRPS    := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.ProConsultaLoteRPS    := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.ProConsultaNFSeRPS    := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.ProConsultaSitLoteRPS := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.ProConsultaNFSe       := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                        ConfigURL.ProCancelaNFSe        := 'https://' + ConfigURL.ProNomeCidade + '.gov.br' + nfse + '/WSNacional/nfse.asmx';
                   end;
          3202603: begin
                        ConfigURL.HomNomeCidade := 'nf-iconha-es';
                        ConfigURL.ProNomeCidade := 'nf-iconha-es';          
                        nfse                    := '.el.com.br';

                        ConfigURL.HomRecepcaoLoteRPS    := 'https://' + ConfigURL.HomNomeCidade + nfse + '/RpsServiceService?wsdl';
                        ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;

                        ConfigURL.ProRecepcaoLoteRPS    := 'https://' + ConfigURL.ProNomeCidade + nfse + '/RpsServiceService?wsdl';
                        ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
                   end;
          3128006: begin // Guanhães/MG
                        ConfigURL.HomNomeCidade := 'nfse00.el.com.br:17080/';
                        ConfigURL.ProNomeCidade := 'nfse00.el.com.br:17080/';
                        nfse                    := 'nfse_mg_guanhaes';

                        ConfigURL.HomRecepcaoLoteRPS    := 'http://' + ConfigURL.HomNomeCidade + nfse + '/RpsServiceService?wsdl';
                        ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
                        ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;

                        ConfigURL.ProRecepcaoLoteRPS    := 'http://' + ConfigURL.ProNomeCidade + nfse + '/RpsServiceService?wsdl';
                        ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
                        ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
                   end;
     end;
     Result := ConfigURL;

  Result := ConfigURL;
end;

function TProvedorEL.GetURI(URI: String): String;
begin
     Result := URI;
end;

function TProvedorEL.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
  case Acao of
   acRecepcionar: Result := False;
   acConsSit    : Result := False;
   acConsLote   : Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe   : Result := False;
   acCancelar   : Result := False;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
  end;
end;

function TProvedorEL.GetValidarLote: Boolean;
begin
  Result := False;
end;

function TProvedorEL.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<wsn:' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit    : Result := '<wsn:' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote   : Result := '<wsn:' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '<wsn:' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe   : Result := '<wsn:' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar   : Result := '<wsn:' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
  end;
end;

function TProvedorEL.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</wsn:' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit    : Result := '</wsn:' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote   : Result := '</wsn:' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</wsn:' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe   : Result := '</wsn:' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar   : Result := '</wsn:' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
  end;
end;

function TProvedorEL.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '';
{     Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
                '<versaoDados>' + VersaoDados + '</versaoDados>'+
               '</' + Prefixo2 + 'cabecalho>';}
end;

function TProvedorEL.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
   result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                               'xmlns:wsn="http://des36.el.com.br:8080/el-issonline/">' +
             '<soapenv:Header/>' +
              '<soapenv:Body>' +
               '<wsn:autenticarContribuinte>' +
                '<identificacaoPrestador>' + CNPJ + '</identificacaoPrestador>' +
                '<senha>' + Senha + '</senha>' +
               '</wsn:autenticarContribuinte>' +
              '</soapenv:Body>' +
             '</soapenv:Envelope>';
end;

function TProvedorEL.FinalizaSessao(hashIdent: String): AnsiString;
begin
   result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                               'xmlns:wsn="http://des36.el.com.br:8080/el-issonline/">' +
             '<soapenv:Header/>' +
              '<soapenv:Body>' +
               '<wsn:finalizarSessao>' +
                '<hashIdentificador>' + hashIdent + '</hashIdentificador>' +
               '</wsn:finalizarSessao>' +
              '</soapenv:Body>' +
             '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                              'xmlns:wsn="' + URLNS + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
                 DadosMsg +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                              'xmlns:wsn="' + URLNS + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
                 DadosMsg +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                              'xmlns:wsn="' + URLNS + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
                 DadosMsg +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                              'xmlns:wsn="' + URLNS + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
                 DadosMsg +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
   result := '';
end;

function TProvedorEL.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                              'xmlns:wsn="' + URLNS + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
                 DadosMsg +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorEL.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEL.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEL.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEL.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
  {case Acao of
   acRecepcionar: Result := 'https://nf-iconha-es.el.com.br/RpsService/RecepcionarLoteRps';
   acConsSit    : Result := 'https://nf-iconha-es.el.com.br/RpsService/ConsultarSituacaoLoteRps';
   acConsLote   : Result := 'https://nf-iconha-es.el.com.br/RpsService/ConsultarLoteRps';
   acConsNFSeRps: Result := 'https://nf-iconha-es.el.com.br/RpsService/ConsultarNfsePorRps';
   acConsNFSe   : Result := 'https://nf-iconha-es.el.com.br/RpsService/ConsultarNfse';
   acCancelar   : Result := 'https://nf-iconha-es.el.com.br/RpsService/CancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
  end;}
  Result := 'https://nf-iconha-es.el.com.br/RpsService?xsd=1';
end;

function TProvedorEL.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := RetornoWS;
   acConsSit    : Result := RetornoWS;
   acConsLote   : Result := RetornoWS;
   acConsNFSeRps: Result := RetornoWS;
   acConsNFSe   : Result := RetornoWS;
   acCancelar   : Result := RetornoWS;
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := SeparaDados( RetornoWS, 'return' ); // Usado para autenticarContribuinte
   acSubstituir:  Result := RetornoWS;
  end;
end;

function TProvedorEL.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
  Result := '';
end;

function TProvedorEL.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
