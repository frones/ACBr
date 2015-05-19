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

unit ACBrProvedorSpeedGov;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorSpeedGov }

 TProvedorSpeedGov = class(TProvedorClass)
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

{ TProvedorSpeedGov }

constructor TProvedorSpeedGov.Create;
begin
 {----}
end;

function TProvedorSpeedGov.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := 'p:';
  ConfigCidade.Prefixo3      := 'p:';
  ConfigCidade.Prefixo4      := 'p1:';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://ws.speedgov.com.br'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://ws.speedgov.com.br';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorSpeedGov.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1';
  ConfigSchema.VersaoDados           := '1';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://ws.speedgov.com.br/';
  ConfigSchema.Cabecalho             := 'cabecalho_v1.xsd';
  ConfigSchema.ServicoEnviar         := 'enviar_lote_rps_envio_v1.xsd';
  ConfigSchema.ServicoConSit         := 'consultar_situacao_lote_rps_envio_v1.xsd';
  ConfigSchema.ServicoConLot         := 'consultar_lote_rps_envio_v1.xsd';
  ConfigSchema.ServicoConRps         := 'consultar_nfse_rps_envio_v1.xsd';
  ConfigSchema.ServicoConNfse        := 'consultar_nfse_envio_v1.xsd';
  ConfigSchema.ServicoCancelar       := 'cancelar_nfse_envio_v1.xsd';
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := 'tipos_v1.xsd';

  Result := ConfigSchema;
end;

function TProvedorSpeedGov.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
 URL: String;
begin
 case ACodCidade of
  2301000: begin // Aquiraz/CE
            ConfigURL.HomNomeCidade := 'aqz';
            ConfigURL.ProNomeCidade := 'aqz';
           end;
  2611101: begin // Petrolina
            ConfigURL.HomNomeCidade := 'pet';
            ConfigURL.ProNomeCidade := 'pet';
           end;
 end;

  ConfigURL.HomRecepcaoLoteRPS    := 'http://speedgov.com.br/wsmod/Nfes';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProRecepcaoLoteRPS    := 'http://www.speedgov.com.br/ws' + ConfigURL.ProNomeCidade + '/Nfes';
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

function TProvedorSpeedGov.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorSpeedGov.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
  Result := false;
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

function TProvedorSpeedGov.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorSpeedGov.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio' + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + NameSpaceDad +
                             '<' + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorSpeedGov.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<p:cabecalho versao="1"' +
            ' xmlns:ds="http://www.w3.org/2000/09/xmldsig#"' +
            ' xmlns:p="http://ws.speedgov.com.br/cabecalho_v1.xsd"' +
            ' xmlns:p1="http://ws.speedgov.com.br/tipos_v1.xsd"' +
            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
            ' xsi:schemaLocation="http://ws.speedgov.com.br/cabecalho_v1.xsd cabecalho_v1.xsd ">' +
            '<versaoDados>1</versaoDados>' +
           '</p:cabecalho>';
end;

function TProvedorSpeedGov.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorSpeedGov.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio>';
   acCancelar:    Result := '</' + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorSpeedGov.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:RecepcionarLoteRps>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:RecepcionarLoteRps>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:ConsultarSituacaoLoteRps>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:ConsultarSituacaoLoteRps>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:ConsultarLoteRps>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:ConsultarLoteRps>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:ConsultarNfsePorRps>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:ConsultarNfsePorRps>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:ConsultarNfse>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:ConsultarNfse>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + CabMsg;
 DadosMsg := '<?xml version="1.0" encoding="UTF-8" ?>' + DadosMsg;

 CabMsg :=StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]);
 CabMsg :=StringReplace(CabMsg, '>', '&gt;', [rfReplaceAll]);

 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                            ' xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
            '<soapenv:Header />' +
            '<soapenv:Body>' +
             '<nfse:CancelarNfse>' +
              '<header>' + CabMsg + '</header>' +
              '<parameters>' + DadosMsg + '</parameters>' +
             '</nfse:CancelarNfse>' +
            '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorSpeedGov.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorSpeedGov.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorSpeedGov.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSpeedGov.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := '';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;;
end;

function TProvedorSpeedGov.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'return' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'return' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'return' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'return' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'return' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'return' );
   acGerar:       Result := SeparaDados( RetornoWS, 'return' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'return' );
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSpeedGov.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorSpeedGov.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
