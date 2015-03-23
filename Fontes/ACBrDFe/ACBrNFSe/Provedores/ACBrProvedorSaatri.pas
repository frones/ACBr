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

unit ACBrProvedorSaatri;

interface

uses
  Classes, SysUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorSaatri }

 TProvedorSaatri = class(TProvedorClass)
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

{ TProvedorSaatri }

constructor TProvedorSaatri.Create;
begin
 {----}
end;

function TProvedorSaatri.GetConfigCidade(ACodCidade,
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
    ConfigCidade.NameSpaceEnvelope := 'http://nfse.abrasf.org.br'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://nfse.abrasf.org.br';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorSaatri.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '2.01';
  ConfigSchema.VersaoDados           := '2.01';
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

function TProvedorSaatri.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
 case ACodCidade of
  1400100: begin // Boa Vista/RR
            ConfigURL.HomNomeCidade := 'boavista';
            ConfigURL.ProNomeCidade := 'boavista';
           end;
  1400209: begin // Caracarai/RR
            ConfigURL.HomNomeCidade := 'caracarai';
            ConfigURL.ProNomeCidade := 'caracarai';
           end;
  2900801: begin // Alcobaca/BA
            ConfigURL.HomNomeCidade := 'alcobaca';
            ConfigURL.ProNomeCidade := 'alcobaca';
           end;
  2902708: begin // Barra/BA
            ConfigURL.HomNomeCidade := 'barra';
            ConfigURL.ProNomeCidade := 'barra';
           end;
  2903201: begin // Barreiras/BA
            ConfigURL.HomNomeCidade := 'barreiras';
            ConfigURL.ProNomeCidade := 'barreiras';
           end;
  2903904: begin // Bom Jesus Da Lapa/BA
            ConfigURL.HomNomeCidade := 'bjlapa';
            ConfigURL.ProNomeCidade := 'bjlapa';
           end;
  2905602: begin // Camacan/BA
            ConfigURL.HomNomeCidade := 'camacan';
            ConfigURL.ProNomeCidade := 'camacan';
           end;
  2906006: begin // Campo Formoso/BA
            ConfigURL.HomNomeCidade := 'campoformoso';
            ConfigURL.ProNomeCidade := 'campoformoso';
           end;
  2907202: begin // Casa Nova/BA
            ConfigURL.HomNomeCidade := 'casanova';
            ConfigURL.ProNomeCidade := 'casanova';
           end;
  2909307: begin // Correntina/BA
            ConfigURL.HomNomeCidade := 'correntina';
            ConfigURL.ProNomeCidade := 'correntina';
           end;
  2910057: begin // Dias D Avila/BA
            ConfigURL.HomNomeCidade := 'diasdavila';
            ConfigURL.ProNomeCidade := 'diasdavila';
           end;
  2910602: begin // Esplanada/BA
            ConfigURL.HomNomeCidade := 'esplanada';
            ConfigURL.ProNomeCidade := 'esplanada';
           end;
  2913200: begin // Ibotirama/BA
            ConfigURL.HomNomeCidade := 'ibotirama';
            ConfigURL.ProNomeCidade := 'ibotirama';
           end;
  2913903: begin // Ipiau/BA
            ConfigURL.HomNomeCidade := 'ipiau';
            ConfigURL.ProNomeCidade := 'ipiau';
           end;
  2914505: begin // Irara/BA
            ConfigURL.HomNomeCidade := 'irara';
            ConfigURL.ProNomeCidade := 'irara';
           end;
  2914653: begin // Itabela/BA
            ConfigURL.HomNomeCidade := 'itabela';
            ConfigURL.ProNomeCidade := 'itabela';
           end;
  2914703: begin // Itaberaba/BA
            ConfigURL.HomNomeCidade := 'itaberaba';
            ConfigURL.ProNomeCidade := 'itaberaba';
           end;
  2917003: begin // Itiuba/BA
            ConfigURL.HomNomeCidade := 'itiuba';
            ConfigURL.ProNomeCidade := 'itiuba';
           end;
  2917508: begin // Jacobina/BA
            ConfigURL.HomNomeCidade := 'jacobina';
            ConfigURL.ProNomeCidade := 'jacobina';
           end;
  2917706: begin // Jaguarari/BA
            ConfigURL.HomNomeCidade := 'jaguarari';
            ConfigURL.ProNomeCidade := 'jaguarari';
           end;
  2919157: begin // Lapao/BA
            ConfigURL.HomNomeCidade := 'lapao';
            ConfigURL.ProNomeCidade := 'lapao';
           end;
  2919926: begin // Madre De Deus/BA
            ConfigURL.HomNomeCidade := 'madre';
            ConfigURL.ProNomeCidade := 'madre';
           end;
  2922656: begin // Nordestina/BA
            ConfigURL.HomNomeCidade := 'nordestina';
            ConfigURL.ProNomeCidade := 'nordestina';
           end;
  2922003: begin // Mucuri/BA
            ConfigURL.HomNomeCidade := 'mucuri';
            ConfigURL.ProNomeCidade := 'mucuri';
           end;
  2927705: begin // Santa Cruz Cabralia/BA
            ConfigURL.HomNomeCidade := 'santacruzcabralia';
            ConfigURL.ProNomeCidade := 'santacruzcabralia';
           end;
  2928901: begin // Sao Desiderio/BA
            ConfigURL.HomNomeCidade := 'saodesiderio';
            ConfigURL.ProNomeCidade := 'saodesiderio';
           end;
  2931350: begin // Teixeira De Freitas/BA
            ConfigURL.HomNomeCidade := 'teixeiradefreitas';
            ConfigURL.ProNomeCidade := 'teixeiradefreitas';
           end;
  2932804: begin // Utinga/BA
            ConfigURL.HomNomeCidade := 'utinga';
            ConfigURL.ProNomeCidade := 'utinga';
           end;
  2933208: begin // Vera Cruz/BA
            ConfigURL.HomNomeCidade := 'veracruz';
            ConfigURL.ProNomeCidade := 'veracruz';
           end;
  4201653: begin // Arvoredo/SC
            ConfigURL.HomNomeCidade := 'arvoredo';
            ConfigURL.ProNomeCidade := 'arvoredo';
           end;
  4204558: begin // Correia Pinto/SC
            ConfigURL.HomNomeCidade := 'correiapinto';
            ConfigURL.ProNomeCidade := 'correiapinto';
           end;
 end;

  ConfigURL.HomRecepcaoLoteRPS    := 'https://homologa-' + ConfigURL.HomNomeCidade + '.saatri.com.br/servicos/nfse.svc';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProRecepcaoLoteRPS    := 'https://' + ConfigURL.ProNomeCidade + '.saatri.com.br/servicos/nfse.svc';
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

function TProvedorSaatri.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorSaatri.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorSaatri.GetValidarLote: Boolean;
begin
 Result := False;
end;

function TProvedorSaatri.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
var
  xmlns: String;
begin
  xmlns := ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"';

  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + xmlns + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + xmlns + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + xmlns + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + xmlns + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio' + xmlns + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + xmlns + NameSpaceDad +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + xmlns + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + xmlns + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + xmlns + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  SeSenao(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorSaatri.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho' +
            ' versao="'  + VersaoLayOut + '"' +
            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
            ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorSaatri.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '<wsse:Security S:mustUnderstand="1"' +
               ' xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"' +
               ' xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">' +
            '<wsse:UsernameToken wsu:Id="UsernameToken-18">' +
             '<wsse:Username>' +
               CNPJ +
             '</wsse:Username>' +
             '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">' +
              Senha +
             '</wsse:Password>' +
            '</wsse:UsernameToken>' +
           '</wsse:Security>';

end;

function TProvedorSaatri.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorSaatri.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:RecepcionarLoteRpsRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:RecepcionarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
 raise Exception.Create( 'Opção não implementada para este provedor.' );
end;

function TProvedorSaatri.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:ConsultarLoteRpsRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:ConsultarNfsePorRpsRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarNfsePorRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:ConsultarNfseServicoPrestadoRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarNfseServicoPrestadoRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:CancelarNfseRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:CancelarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Header>' +
              DadosSenha +
            '</S:Header>' +
            '<S:Body>' +
             '<nfse:GerarNfseRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</nfse:GerarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorSaatri.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSaatri.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSaatri.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://nfse.abrasf.org.br/Infse/RecepcionarLoteRps';
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://nfse.abrasf.org.br/Infse/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://nfse.abrasf.org.br/Infse/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://nfse.abrasf.org.br/Infse/ConsultarNfseServicoPrestado';
   acCancelar:    Result := 'http://nfse.abrasf.org.br/Infse/CancelarNfse';
   acGerar:       Result := 'http://nfse.abrasf.org.br/Infse/GerarNfse';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorSaatri.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'outputXML' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'outputXML' );
   acGerar:       Result := StringReplace( RetornoWS, ' xml:lang="pt-BR"','', [rfReplaceAll] );
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSaatri.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorSaatri.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 if AAmbiente = 1
  then begin
   case ACodMunicipio of
    1400100: Result := 'https://boavista.saatri.com.br/VisualizarNotaFiscal?numero=' +
                       IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    1400209: Result := 'https://caracarai.saatri.com.br/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2902708: Result := 'https://barra.saatri.com.br/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2903201: Result := 'https://barreiras.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
                       IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2903904: Result := 'https://bjlapa.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2905602: Result := 'https://camacan.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2906006: Result := 'https://campoformoso.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2907202: Result := 'https://casanova.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2909307: Result := 'https://correntina.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2910057: Result := 'https://diasdavila.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
                       IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2910602: Result := 'https://esplanada.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2913200: Result := 'https://ibotirama.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2913903: Result := 'https://ipiau.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
  	                   IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2914505: Result := 'https://irara.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2914653: Result := 'https://itabela.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2914703: Result := 'https://itaberaba.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
 	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2917003: Result := 'https://itiuba.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2917508: Result := 'https://jacobina.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
    2917706: Result := 'https://jaguarari.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
  	2919157: Result := 'https://lapao.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2919926: Result := 'https://madre.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2922656: Result := 'https://nordestina.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
  	                   IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2922003: Result := 'https://mucuri.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2927705: Result := 'https://santacruzcabralia.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2928901: Result := 'https://saodesiderio.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2931350: Result := 'https://teixeiradefreitas.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2932804: Result := 'https://utinga.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  2933208: Result := 'https://veracruz.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  4201653: Result := 'https://arvoredo.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
	  4204558: Result := 'https://correiapinto.saatri.com.br/Relatorio/VisualizarNotaFiscal?numero=' +
	                     IntToStr(ANumeroNFSe) + '&codigoVerificacao=' + ACodVerificacao;
   else Result := '';
   end;
  end
  else Result := '';
end;

end.
