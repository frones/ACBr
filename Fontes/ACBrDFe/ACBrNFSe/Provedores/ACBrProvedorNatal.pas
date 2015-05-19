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

unit ACBrProvedorNatal;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorNatal }

 TProvedorNatal = class(TProvedorClass)
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

{ TProvedorNatal }

constructor TProvedorNatal.Create;
begin
 {----}
end;

function TProvedorNatal.GetConfigCidade(ACodCidade,
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

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'https://wsnfsev1.natal.rn.gov.br:8444'
  else
    ConfigCidade.NameSpaceEnvelope := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443';

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorNatal.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1';
  ConfigSchema.VersaoDados           := '1';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://www.abrasf.org.br/ABRASF/arquivos/';
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

function TProvedorNatal.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := 'homologacao';
  ConfigURL.HomRecepcaoLoteRPS    := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/NfseWSServiceV1/';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProNomeCidade         := '';
  ConfigURL.ProRecepcaoLoteRPS    := 'https://wsnfsev1.natal.rn.gov.br:8444/axis2/services/NfseWSServiceV1/';
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

function TProvedorNatal.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorNatal.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := False;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorNatal.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorNatal.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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

function TProvedorNatal.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                                      'versao="' + VersaoLayOut + '" ' +
                                      'xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorNatal.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorNatal.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorNatal.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:RecepcionarLoteRpsRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:RecepcionarLoteRpsRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:ConsultarSituacaoLoteRpsRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:ConsultarSituacaoLoteRpsRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">'+
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:ConsultarLoteRpsRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:ConsultarLoteRpsRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:ConsultarNfsePorRpsRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:ConsultarNfsePorRpsRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:ConsultarNfseServicoPrestadoRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:ConsultarNfseServicoPrestadoRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';

end;

function TProvedorNatal.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:CancelarNfseRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:CancelarNfseRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:GerarNfseRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:GerarNfseRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:wsn="' + URLNs + '">' +
            '<soapenv:Header/>' +
            '<soapenv:Body>' +
             '<wsn:RecepcionarLoteRpsSincronoRequest>' +
              '<nfseCabecMsg>'+
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>'+
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</nfseDadosMsg>' +
             '</wsn:RecepcionarLoteRpsSincronoRequest>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNatal.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorNatal.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
var
 Porta: String;
begin
  {
 case Acao of
   acRecepcionar: Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/RecepcionarLoteRps';
   acConsSit:     Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/ConsultarSituacaoLoteRps';
   acConsLote:    Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/ConsultarLoteRps';
   acConsNFSeRps: Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/ConsultarNfse';
   acCancelar:    Result := 'https://wsnfsev1homologacao.natal.rn.gov.br:8443/axis2/services/CancelarNfse';
   acGerar:       Result := '';
 end;
 }
 if NomeCidade = '' then
   Porta := '8444'
 else
   Porta := '8443';

 case Acao of
   acRecepcionar: Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/RecepcionarLoteRps';
   acConsSit:     Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/ConsultarSituacaoLoteRps';
   acConsLote:    Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/ConsultarLoteRps';
   acConsNFSeRps: Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/ConsultarNfse';
   acCancelar:    Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/CancelarNfse';
   acGerar:       Result := 'https://wsnfsev1' + NomeCidade + '.natal.rn.gov.br:' + Porta +'/axis2/services/GerarNfse';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorNatal.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
var
 RetWS: AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorNatal.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := 
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorNatal.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.

