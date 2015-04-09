{$I ACBr.inc}

unit ACBrProvedorNFSEBrasil;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorNFSEBrasil }

 TProvedorNFSEBrasil = class(TProvedorClass)
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

{ TProvedorNFSEBrasil }

constructor TProvedorNFSEBrasil.Create;
begin
 {----}
end;

function TProvedorNFSEBrasil.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'Id';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://nfsebrasil.net.br'    //'http://tempuri.org'         - Produção
  else
   ConfigCidade.NameSpaceEnvelope := 'http://web1.memory.com.br:81';    // 'http://tempuri.org';  - Homologação

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorNFSEBrasil.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1';
  ConfigSchema.VersaoDados           := '1';
  ConfigSchema.VersaoXML             := '2';
  ConfigSchema.NameSpaceXML          := ''; //'http://www.abrasf.org.br/ABRASF/arquivos/';
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

function TProvedorNFSEBrasil.GetConfigURL(ACodCidade: Integer): TConfigURL;
{As Cidades comentadas são as que não foram utilizadas até o momento
mas utilizam o mesmo provedor

http://www.projetoacbr.com.br/forum/index.php?/topic/18150-nfs-e-vespasiano/
http://www.projetoacbr.com.br/forum/index.php?/topic/17981-implementa%C3%A7%C3%A3o-do-provedor-memory/
}

var
 ConfigURL: TConfigURL;
begin
 case ACodCidade of
 3171204: begin //  Vespasiano/MG
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://web1.memory.com.br:81/nfse/ws/lote_rps_service.php';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://nfsebrasil.net.br/nfse/ws/lote_rps_service.php?wsdl';
           end;
  3118304: begin //  Conselheiro Lafaiete/MG
  
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://web1.memory.com.br:81/nfse/rps/xsd/rps.xsd';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://www.nfsebrasil.net.br/nfse/rps/xsd/rps.xsd';
           end;
  3120904: begin //  Curvelo/MG
            {
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://nfseteste.guarapari.es.gov.br/NFSEWSTESTE/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://nfes.guarapari.es.gov.br/NFSEws/Services.svc';
            }
           end;
   3169356: begin //  Três Marias/MG
            {
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://nfseteste.guarapari.es.gov.br/NFSEWSTESTE/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://nfes.guarapari.es.gov.br/NFSEws/Services.svc';
            }
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

function TProvedorNFSEBrasil.GetURI(URI: String): String;
begin
 // No provedor memory a URI não é informada.
 Result := '';
end;

function TProvedorNFSEBrasil.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := False;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := False;
   acConsNFSe:    Result := False;
   acCancelar:    Result := False;
   acGerar:       Result := True;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorNFSEBrasil.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorNFSEBrasil.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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

function TProvedorNFSEBrasil.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
             '<versaoDados>' + VersaoDados + '</versaoDados>'+
            '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorNFSEBrasil.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
  // Contribuição EricMartins

  Result := '<cnpjPrestador xsi:type="xsd:string">'+CNPJ+'</cnpjPrestador>'+
            '<hashValidador xsi:type="xsd:string">'+Senha+'</hashValidador>';
end;

function TProvedorNFSEBrasil.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorNFSEBrasil.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
// Contribuição EricMartins

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.importarLoteRPS ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                    '<xml xsi:type="xsd:string">'+
                     DadosMsg+
                    '</xml>' +
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.importarLoteRPS>' +
              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
  protocolo: String;
begin
 protocolo := NotaUtil.RetornarConteudoEntre(DadosMsg,'<Protocolo>','</Protocolo>');

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.consultarLoteRPS ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                     '<Protocolo>'+protocolo+'</Protocolo>'+
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.consultarLoteRPS>' +

              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
  protocolo: String;
begin
 protocolo := NotaUtil.RetornarConteudoEntre(DadosMsg,'<Protocolo>','</Protocolo>');

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.consultarLoteRPS ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                     '<Protocolo>'+protocolo+'</Protocolo>'+
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.consultarLoteRPS>' +

              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.importarLoteRPS ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                    '<xml xsi:type="xsd:string">'+
                     DadosMsg+
                    '</xml>' +
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.importarLoteRPS>' +
              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.importarLoteRPS ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                    '<xml xsi:type="xsd:string">'+
                     DadosMsg+
                    '</xml>' +
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.importarLoteRPS>' +
              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
var
  NumRPS:string;
begin

 NumRPS := NotaUtil.RetornarConteudoEntre(DadosMsg,'<Numero>','</Numero>');

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
                             'xmlns:urn="urn:loterpswsdl">' +
              '<soapenv:Header/>' +
              '<soapenv:Body>' +
                 '<urn:tm_lote_rps_service.cancelarNFSE ' +
                  'soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +

                     '<numeroRPS>'+NumRPS+'</numeroRPS>'+
                     CabMsg +
                     DadosSenha +
                 '</urn:tm_lote_rps_service.cancelarNFSE>' +
              '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorNFSEBrasil.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorNFSEBrasil.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorNFSEBrasil.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorNFSEBrasil.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
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

function TProvedorNFSEBrasil.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
(*
 case Acao of
   acRecepcionar: Result := SeparaDados_NFSEBrasil( RetornoWS, 'tm_lote_rps_service.importarLoteRPSResponse' );
   acConsSit:     Result := SeparaDados_NFSEBrasil( RetornoWS, 'tm_lote_rps_service.consultarLoteRPS' );
   acConsLote:    Result := SeparaDados_NFSEBrasil( RetornoWS, 'tm_lote_rps_service.consultarLoteRPS' );
   acConsNFSeRps: Result := SeparaDados_NFSEBrasil( RetornoWS, '' );
   acConsNFSe:    Result := SeparaDados_NFSEBrasil( RetornoWS, '' );
   acCancelar:    Result := SeparaDados_NFSEBrasil( RetornoWS, 'tm_lote_rps_service.cancelarNFSE' );
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
*) 
(*
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
*)
end;

function TProvedorNFSEBrasil.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorNFSEBrasil.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
