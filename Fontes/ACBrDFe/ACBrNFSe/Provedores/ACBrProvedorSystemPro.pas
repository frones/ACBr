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

unit ACBrProvedorSystemPro;

interface

uses
  Classes, SysUtils, Forms, strUtils,
  pnfsConversao, pcnAuxiliar, // IniFiles,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

const
  fHttp:string='https://';

type
  { TACBrProvedorSystemPro }

 TProvedorSystemPro = class(TProvedorClass)
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

{ TProvedorSystemPro }

constructor TProvedorSystemPro.Create;
begin
 {----}
end;

function TProvedorSystemPro.GetConfigCidade(ACodCidade,
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

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorSystemPro.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
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

function TProvedorSystemPro.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 Endereco: string;
 ConfigURL: TConfigURL;
begin
 case ACodCidade of
  4307005: begin // Erechim/RS
            Endereco := 'www.nfse.erechim.rs.gov.br:8182';
            ConfigURL.HomNomeCidade := Endereco + '/NfseService_Homolog/NfseService_Homolog';
            ConfigURL.ProNomeCidade := Endereco + '/NfseService/NfseService';
           end;
  4304408: begin // Canela/RS
            Endereco := 'www.nfse.canela.rs.gov.br:8182';
            ConfigURL.HomNomeCidade := Endereco + '/NfseService_Homolog/NfseService_Homolog';
            ConfigURL.ProNomeCidade := Endereco + '/NfseService/NfseService';
           end;
 end;

 ConfigURL.HomRecepcaoLoteRPS    := '';
 ConfigURL.HomConsultaLoteRPS    := '';
 ConfigURL.HomConsultaNFSeRPS    := '';
 ConfigURL.HomConsultaSitLoteRPS := '';
 ConfigURL.HomConsultaNFSe       := fHttp+ConfigURL.HomNomeCidade;
 ConfigURL.HomCancelaNFSe        := fHttp+ConfigURL.HomNomeCidade;
 ConfigURL.HomGerarNFSe          := fHttp+ConfigURL.HomNomeCidade;
 ConfigURL.HomRecepcaoSincrono   := '';
  ConfigURL.HomSubstituiNFSe      := '';

 ConfigURL.ProRecepcaoLoteRPS    := '';
 ConfigURL.ProConsultaLoteRPS    := '';
 ConfigURL.ProConsultaNFSeRPS    := '';
 ConfigURL.ProConsultaSitLoteRPS := '';
 ConfigURL.ProConsultaNFSe       := fHttp+ConfigURL.ProNomeCidade;
 ConfigURL.ProCancelaNFSe        := fHttp+ConfigURL.ProNomeCidade;
 ConfigURL.ProGerarNFSe          := fHttp+ConfigURL.ProNomeCidade;
 ConfigURL.ProRecepcaoSincrono   := '';
  ConfigURL.ProSubstituiNFSe      := '';

 Result := ConfigURL;
end;

function TProvedorSystemPro.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorSystemPro.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorSystemPro.GetValidarLote: Boolean;
begin
 Result := False;
end;

function TProvedorSystemPro.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
var
  xmlns: String;
begin
  xmlns := ' xmlns="http://www.abrasf.org.br/nfse.xsd">';

  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + xmlns;
   acConsSit:     Result := '<' + Prefixo3 + '' + xmlns;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + xmlns;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + xmlns;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseFaixaEnvio' + xmlns;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + xmlns +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + xmlns;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + xmlns;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + xmlns +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
 
end;

function TProvedorSystemPro.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho' +
            ' versao="'  + VersaoLayOut + '"' +
            ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorSystemPro.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorSystemPro.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseFaixaEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorSystemPro.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
 raise Exception.Create( 'Opção não implementada para este provedor (GeraEnvelopeRecepcionarLoteRPS).' );
end;

function TProvedorSystemPro.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
 raise Exception.Create( 'Opção não implementada para este provedor (GeraEnvelopeConsultarSituacaoLoteRPS).' );
end;

function TProvedorSystemPro.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
 raise Exception.Create( 'Opção não implementada para este provedor (GeraEnvelopeConsultarLoteRPS).' );
end;

function TProvedorSystemPro.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
  raise Exception.Create( 'Opção não implementada para este provedor (GeraEnvelopeConsultarNFSeporRPS).' );
end;

function TProvedorSystemPro.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>'
           +'<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'
           +'<SOAP-ENV:Header/>'
           +'<S:Body>'
           +'<ns2:ConsultarNfseFaixa xmlns:ns2="http://NFSe.wsservices.systempro.com.br/">'

           + '<nfseCabecMsg><![CDATA[<?xml version=''1.0'' encoding=''UTF-8''?>'
           + '<cabecalho xmlns="http://www.abrasf.org.br/nfse.xsd" '
           + 'versao="0.01">'
           + '<versaoDados>2.01</versaoDados>'
           + '</cabecalho>]]></nfseCabecMsg>'

           +'<nfseDadosMsg><![CDATA[' + DadosMsg + ']]></nfseDadosMsg>'

           +'</ns2:ConsultarNfseFaixa>'
           +'</S:Body>'
           +'</S:Envelope>';
end;

function TProvedorSystemPro.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>'
           +'<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'
           +'<SOAP-ENV:Header/>'
           +'<S:Body>'
           +'<ns2:CancelarNfse xmlns:ns2="http://NFSe.wsservices.systempro.com.br/">'

           + '<nfseCabecMsg><![CDATA[<?xml version=''1.0'' encoding=''UTF-8''?>'
           + '<cabecalho xmlns="http://www.abrasf.org.br/nfse.xsd" '
           + 'versao="0.01">'
           + '<versaoDados>2.01</versaoDados>'
           + '</cabecalho>]]></nfseCabecMsg>'

           +'<nfseDadosMsg><![CDATA[' + DadosMsg + ']]></nfseDadosMsg>'

           +'</ns2:CancelarNfse>'
           +'</S:Body>'
           +'</S:Envelope>';
end;

function TProvedorSystemPro.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                        'xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">' +
             '<SOAP-ENV:Header/>' +
             '<S:Body>' +
              '<ns2:GerarNfse xmlns:ns2="http://NFSe.wsservices.systempro.com.br/">' +
               '<nfseCabecMsg><![CDATA[<?xml version=''1.0'' encoding=''UTF-8''?>' +
                '<cabecalho xmlns="http://www.abrasf.org.br/nfse.xsd" versao="0.01">' +
                '<versaoDados>2.01</versaoDados>' +
                '</cabecalho>]]>' +
               '</nfseCabecMsg>' +
               '<nfseDadosMsg>' +
                '<![CDATA[' + DadosMsg + ']]>' +
               '</nfseDadosMsg>' +
              '</ns2:GerarNfse>' +
             '</S:Body>' +
            '</S:Envelope>';

end;

function TProvedorSystemPro.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
  raise Exception.Create( 'Opção não implementada para este provedor (GeraEnvelopeRecepcionarSincrono).' );
end;

function TProvedorSystemPro.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSystemPro.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := fHttp+NomeCidade+'/#ConsultarNfseFaixa';
   acCancelar:    Result := fHttp+NomeCidade+'/#CancelarNfse';
   acGerar:       Result := fHttp+NomeCidade+'/#GerarNfse';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorSystemPro.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'return' );
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'return' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'return' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'return' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'return' );
   acGerar:       Result := SeparaDados( RetornoWS, 'return' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'return' );
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSystemPro.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorSystemPro.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 if AAmbiente = 1
  then begin
   case ACodMunicipio of
     4307005: Result := '';
   else
     Result := '';
   end;
  end
  else
    Result := '';
end;

end.

