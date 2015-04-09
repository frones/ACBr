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

unit ACBrProvedorIssCuritiba;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorIssCuritiba }

 TProvedorIssCuritiba = class(TProvedorClass)
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

{ TProvedorIssCuritiba }

constructor TProvedorIssCuritiba.Create;
begin
 {----}
end;

function TProvedorIssCuritiba.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.2';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'id';
  ConfigCidade.QuebradeLinha := ';';

// if AAmbiente = 1
//  then ConfigCidade.NameSpaceEnvelope := 'https://isscuritiba.curitiba.pr.gov.br'
//  else ConfigCidade.NameSpaceEnvelope := 'http://200.189.192.82/pilotonota_iss/';

  ConfigCidade.NameSpaceEnvelope := 'http://www.e-governeapps2.com.br/';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorIssCuritiba.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '';
  ConfigSchema.VersaoDados           := '';
  ConfigSchema.VersaoXML             := '1';
//  ConfigSchema.NameSpaceXML          := 'http://www.e-governeapps2.com.br/';
  ConfigSchema.NameSpaceXML          := 'http://isscuritiba.curitiba.pr.gov.br/iss/';
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

function TProvedorIssCuritiba.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
 (*
 ConfigURL.HomRecepcaoLoteRPS    := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 ConfigURL.HomConsultaLoteRPS    := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 ConfigURL.HomConsultaNFSeRPS    := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 ConfigURL.HomConsultaSitLoteRPS := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 ConfigURL.HomConsultaNFSe       := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 ConfigURL.HomCancelaNFSe        := 'http://200.189.192.82/pilotonota_webservice/nfsews.asmx';
 *)
  ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := 'https://pilotoisscuritiba.curitiba.pr.gov.br/nfse_ws/NfseWs.asmx';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProNomeCidade         := '';
  ConfigURL.ProRecepcaoLoteRPS    := 'https://isscuritiba.curitiba.pr.gov.br/Iss.NfseWebService/Nfsews.asmx';
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

function TProvedorIssCuritiba.GetURI(URI: String): String;
begin
 Result := '';
end;

function TProvedorIssCuritiba.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := True;
   acConsLote:    Result := True;
   acConsNFSeRps: Result := True;
   acConsNFSe:    Result := True;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorIssCuritiba.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorIssCuritiba.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio' + NameSpaceDad;
   acCancelar:    Result := '<CancelarNfseEnvio' + NameSpaceDad + '<Pedido>'; // Alterado por Akai - L. Massao Aihara 31/10/2013
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorIssCuritiba.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '';
           {'<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';}
end;

function TProvedorIssCuritiba.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorIssCuritiba.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</Pedido></CancelarNfseEnvio>'; // Alterado por Akai - L. Massao Aihara 31/10/2013
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorIssCuritiba.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
            '<soap12:Body>' +
             '<RecepcionarLoteRps xmlns="http://www.e-governeapps2.com.br/">' +
               DadosMsg +
             '</RecepcionarLoteRps>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 rESULT := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
           '<soap12:Body>' +
            '<ConsultarSituacaoLoteRps xmlns="http://www.e-governeapps2.com.br/">' +
              DadosMsg +
            '</ConsultarSituacaoLoteRps>' +
           '</soap12:Body>' +
          '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
            '<soap12:Body>' +
             '<ConsultarLoteRps xmlns="http://www.e-governeapps2.com.br/">' +
               DadosMsg +
             '</ConsultarLoteRps>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
            '<soap12:Body>' +
             '<ConsultarNfsePorRps xmlns="http://www.e-governeapps2.com.br/">' +
               DadosMsg +
             '</ConsultarNfsePorRps>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
           '<soap12:Body>' +
            '<ConsultarNfse xmlns="http://www.e-governeapps2.com.br/">' +
              DadosMsg +
            '</ConsultarNfse>'+
           '</soap12:Body>' +
          '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
            '<soap12:Body>' +
             '<CancelarNfse xmlns="http://www.e-governeapps2.com.br/">' +
               DadosMsg +
             '</CancelarNfse>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorIssCuritiba.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorIssCuritiba.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorIssCuritiba.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorIssCuritiba.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://www.e-governeapps2.com.br/RecepcionarLoteRps';
   acConsSit:     Result := 'http://www.e-governeapps2.com.br/ConsultarSituacaoLoteRps';
   acConsLote:    Result := 'http://www.e-governeapps2.com.br/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://www.e-governeapps2.com.br/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://www.e-governeapps2.com.br/ConsultarNfse';
   acCancelar:    Result := 'http://www.e-governeapps2.com.br/CancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorIssCuritiba.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 // Alterado por Akai - L. Massao Aihara 31/10/2013
 // para manter as chaves...
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'RecepcionarLoteRpsResult', True );
   acConsSit:     Result := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRpsResult', True );
   acConsLote:    Result := SeparaDados( RetornoWS, 'ConsultarLoteRpsResult', True );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ConsultarNfsePorRpsResult', True );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ConsultarNfseResult', True );
   acCancelar:    Result := SeparaDados( RetornoWS, 'CancelarNfseResult', True );
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorIssCuritiba.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns:ns4="http://www.e-governeapps2.com.br/nfse.xsd">' +
            RetNFSe +
           '</CompNfse>';
end;

function TProvedorIssCuritiba.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := 'https://isscuritiba.curitiba.pr.gov.br/portalnfse/Default.aspx?doc=' +
           AInscricaoM + '&num=' + IntToStr(ANumeroNFSe) + '&cod=' + ACodVerificacao;
end;

end.
