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

unit ACBrProvedorGovDigital;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorGovDigital }

 TProvedorGovDigital = class(TProvedorClass)
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

{ TProvedorGovDigital }

constructor TProvedorGovDigital.Create;
begin
 {----}
end;

function TProvedorGovDigital.GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
 	ConfigCidade.VersaoSoap    := '1.1';
 	ConfigCidade.Prefixo2      := '';
 	ConfigCidade.Prefixo3      := '';
 	ConfigCidade.Prefixo4      := '';
 	ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

(*
  case ACodCidade of
   3122306: begin // Divinopolis/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/div'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/div';
            end;
   3132404: begin // Itajubá/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/itj'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/itj';
            end;
   3138203: begin // Lavras/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/lavr'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/lavr';
            end;
   3147006: begin // Paracatu/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/pctu'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/pctu';
            end;
   3151800: begin // Poços de Caldas/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/pocos'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/pocos';
            end;
   3162955: begin // Sao Jose Da Lapa/MG
             if AAmbiente = 1
              then ConfigCidade.NameSpaceEnvelope := 'https://ws.govdigital.com.br/ws/sjl'
              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/sjl';
            end;

//Prata
//Produção: https://ws.govdigital.com.br/ws/prata
//Homologação: https://homolog.govd...com.br/ws/prata

// A Cidade de Itapetininga/SP trocou o provedor de GovDigital para ISSNet
//   3522307: begin // Itapetininga/SP
//             if AAmbiente = 1
//              then ConfigCidade.NameSpaceEnvelope := 'https://www.govdigital.com.br/ws/itapetininga'
//              else ConfigCidade.NameSpaceEnvelope := 'https://homolog.govdigital.com.br/ws/itapetininga';
//            end;
  end;
*)

  ConfigCidade.NameSpaceEnvelope := 'http://nfse.abrasf.org.br';

 	ConfigCidade.AssinaRPS   := True;
 	ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

 	Result := ConfigCidade;
end;

function TProvedorGovDigital.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '2.00';
  ConfigSchema.VersaoDados           := '2.00';
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

function TProvedorGovDigital.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 	ConfigURL: TConfigURL;
  Porta: String;
begin
  Porta := '';

  case ACodCidade of
   3122306: begin // Divinopolis/MG
              ConfigURL.HomNomeCidade := 'div';
              ConfigURL.ProNomeCidade := 'div';
              Porta := ':443';
            end;
   3132404: begin
              ConfigURL.HomNomeCidade := 'itj';
              ConfigURL.ProNomeCidade := 'itj';
//              Porta := ':443';
            end;
   3138203: begin // Lavras/MG
              ConfigURL.HomNomeCidade := 'lavr';
              ConfigURL.ProNomeCidade := 'lavr';
//              Porta := ':443';
            end;
   3147006: begin // Paracatu/MG
              ConfigURL.HomNomeCidade := 'pctu';
              ConfigURL.ProNomeCidade := 'pctu';
//              Porta := ':443';
            end;
   3151800: begin // Poços de Caldas/MG
              ConfigURL.HomNomeCidade := 'pocos';
              ConfigURL.ProNomeCidade := 'pocos';
//              Porta := ':443';
            end;
   3162955: begin // Sao Jose Da Lapa/MG
              ConfigURL.HomNomeCidade := 'sjl';
              ConfigURL.ProNomeCidade := 'sjl';
//              Porta := ':443';
            end;


//   3522307: begin // Itapetininga/SP
//             ConfigURL.HomNomeCidade := 'itapetininga';
//             ConfigURL.ProNomeCidade := 'itapetininga';
//            end;
  end;

 	ConfigURL.HomRecepcaoLoteRPS    := 'https://homolog.govdigital.com.br' + Porta + '/ws/' + ConfigURL.HomNomeCidade;
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

 	ConfigURL.ProRecepcaoLoteRPS    := 'https://ws.govdigital.com.br' + Porta + '/ws/' + ConfigURL.ProNomeCidade;
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

function TProvedorGovDigital.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorGovDigital.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := False;
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

function TProvedorGovDigital.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorGovDigital.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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
                                 SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorGovDigital.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<![CDATA[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
           '<ns2:cabecalho xmlns:ns2="http://www.abrasf.org.br/nfse.xsd" versao="' + VersaoLayOut + '">' +
            '<ns2:versaoDados>' + VersaoDados + '</ns2:versaoDados>'+
           '</ns2:cabecalho>]]>';

end;

function TProvedorGovDigital.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorGovDigital.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorGovDigital.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<RecepcionarLoteRpsRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</RecepcionarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorGovDigital.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ConsultarSituacaoRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</ConsultarSituacaoRequest>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovDigital.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ConsultarLoteRpsRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</ConsultarLoteRpsRequest>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovDigital.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ConsultarNfsePorRpsRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</ConsultarNfsePorRpsRequest>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovDigital.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<s:Body>' +
             '<ConsultarNfsePorFaixaRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</ConsultarNfsePorFaixaRequest>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorGovDigital.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := '<![CDATA[<?xml version="1.0" encoding="UTF-8"?>' + DadosMsg+']]>';

  Result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nfse="http://nfse.abrasf.org.br">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<nfse:CancelarNfseRequest>' +
                     '<nfseCabecMsg>'+CabMsg+'</nfseCabecMsg>' +
                     '<nfseDadosMsg>'+DadosMsg+'</nfseDadosMsg>' +
                 '</nfse:CancelarNfseRequest>' +
              '</soapenv:Body>' +
            '</soapenv:Envelope>';

end;

function TProvedorGovDigital.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := '<![CDATA[<?xml version="1.0" encoding="UTF-8"?>' + DadosMsg+']]>';

  Result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nfse="http://nfse.abrasf.org.br">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<nfse:GerarNfseRequest>' +
                     '<nfseCabecMsg>'+CabMsg+'</nfseCabecMsg>' +
                     '<nfseDadosMsg>'+DadosMsg+'</nfseDadosMsg>' +
                 '</nfse:GerarNfseRequest>' +
              '</soapenv:Body>' +
            '</soapenv:Envelope>';

end;

function TProvedorGovDigital.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  DadosMsg := '<![CDATA[<?xml version="1.0" encoding="UTF-8"?>' + DadosMsg+']]>';

  Result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nfse="http://nfse.abrasf.org.br">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<nfse:RecepcionarLoteRpsSincronoRequest>' +
                    '<nfseCabecMsg>'+CabMsg+'</nfseCabecMsg>' +
                    '<nfseDadosMsg>'+DadosMsg+'</nfseDadosMsg>' +
                 '</nfse:RecepcionarLoteRpsSincronoRequest>' +
              '</soapenv:Body>' +
            '</soapenv:Envelope>';

end;

function TProvedorGovDigital.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<SubstituirNfseRequest xmlns="' + URLNS + '">' +
                DadosMsg +
             '</SubstituirNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorGovDigital.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://nfse.abrasf.org.br/RecepcionarLoteRps';
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://nfse.abrasf.org.br/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://nfse.abrasf.org.br/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://nfse.abrasf.org.br/ConsultarNfsePorFaixa';
   acCancelar:    Result := 'http://nfse.abrasf.org.br/CancelarNfse';
   acGerar:       Result := 'http://nfse.abrasf.org.br/GerarNfse';
   acRecSincrono: Result := 'http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono';
   acSubstituir:  Result := 'http://nfse.abrasf.org.br/SubstituirNfse';
 end;
end;

function TProvedorGovDigital.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
var
 RetWS: AnsiString;
begin
 case Acao of
   acRecepcionar: begin
                   RetWS := SeparaDados( RetornoWS, 'RecepcionarLoteRpsResponse' );
                   RetWS := RetWS + '</RecepcionarLoteRpsResponse>';
                   Result := RetWS;
                  end;
   acConsSit:     Result := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRpsResponse' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'ConsultarLoteRpsResponse' );
   acConsNFSeRps: begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarNfsePorRpsResponse>' );
                   RetWS := RetWS + '</ConsultarNfsePorRpsResponse>';
                   Result := RetWS;
                  end;
   acConsNFSe:    begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarNfsePorFaixaResponse' );
                   RetWS := RetWS + '</ConsultarNfsePorFaixaResponse>';
                   Result := RetWS;
                  end;
   acCancelar:    begin
                   RetWS := SeparaDados( RetornoWS, 'CancelarNfseResponse' );
                   RetWS := RetWS + '</CancelarNfseResponse>';
                   Result := RetWS;
                  end;
   acGerar:       begin
                   RetWS := SeparaDados( RetornoWS, 'GerarNfseResponse' );
                   RetWS := RetWS + '</GerarNfseResponse>';
                   Result := RetWS;
                  end;
   acRecSincrono:  begin
                   RetWS := SeparaDados( RetornoWS, 'RecepcionarLoteRpsSincronoResponse' );
                   RetWS := RetWS + '</RecepcionarLoteRpsSincronoResponse>';
                   Result := RetWS;
                  end;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorGovDigital.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
            RetNFSe +
           '</CompNfse>';
end;

function TProvedorGovDigital.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
