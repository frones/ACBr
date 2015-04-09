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

unit ACBrProvedorEgoverneiss;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorEgoverneISS }

 TProvedorEgoverneISS = class(TProvedorClass)
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

{ TProvedorEgoverneISS }

constructor TProvedorEgoverneISS.Create;
begin
end;

function TProvedorEgoverneISS.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := 'tem:';
  ConfigCidade.Prefixo4      := 'rgm:';
  ConfigCidade.Identificador := '';
  ConfigCidade.QuebradeLinha := ';';

  ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org';
  
  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorEgoverneISS.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '';
  ConfigSchema.VersaoDados           := '';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := '';
  ConfigSchema.Cabecalho             := '';
  ConfigSchema.ServicoEnviar         := '';
  ConfigSchema.ServicoConSit         := '';
  ConfigSchema.ServicoConLot         := '';
  ConfigSchema.ServicoConRps         := '';
  ConfigSchema.ServicoConNfse        := '';
  ConfigSchema.ServicoCancelar       := '';
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorEgoverneISS.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := '';
  ConfigURL.HomConsultaLoteRPS    := '';
  ConfigURL.HomConsultaNFSeRPS    := '';
  ConfigURL.HomConsultaSitLoteRPS := '';
  ConfigURL.HomConsultaNFSe       := '';
  ConfigURL.HomCancelaNFSe        := 'https://www.nfeosasco.com.br/EissnfeWebServices/NotaFiscalEletronica.svc';
  ConfigURL.HomGerarNFSe          := 'https://www.nfeosasco.com.br/EissnfeWebServices/NotaFiscalEletronica.svc';
  ConfigURL.HomRecepcaoSincrono   := '';
  ConfigURL.HomSubstituiNFSe      := '';

  ConfigURL.ProNomeCidade         := '';
  ConfigURL.ProRecepcaoLoteRPS    := '';
  ConfigURL.ProConsultaLoteRPS    := '';
  ConfigURL.ProConsultaNFSeRPS    := '';
  ConfigURL.ProConsultaSitLoteRPS := '';
  ConfigURL.ProConsultaNFSe       := '';
  ConfigURL.ProCancelaNFSe        := 'https://www.nfeosasco.com.br/EissnfeWebServices/NotaFiscalEletronica.svc';
  ConfigURL.ProGerarNFSe          := 'https://www.nfeosasco.com.br/EissnfeWebServices/NotaFiscalEletronica.svc';
  ConfigURL.ProRecepcaoSincrono   := '';
  ConfigURL.ProSubstituiNFSe      := '';

  Result := ConfigURL;
end;

function TProvedorEgoverneISS.GetURI(URI: String): String;
begin
 Result := '';
end;

function TProvedorEgoverneISS.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorEgoverneISS.GetValidarLote: Boolean;
begin
  Result := False;
end;

function TProvedorEgoverneISS.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
var
 xmlns: String;
begin
 xmlns := NameSpaceDad;

 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := '<' + Prefixo3 + 'request' + xmlns;
   acGerar:       Result := '<' + Prefixo3 + 'request' + xmlns;
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorEgoverneISS.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '';
end;

function TProvedorEgoverneISS.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorEgoverneISS.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := '</' + Prefixo3 + 'request>';
   acGerar:       Result := '</' + Prefixo3 + 'request>';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorEgoverneISS.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="utf-8"?>' +
         '<soap12:Envelope xmlns:soap12="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tem="http://tempuri.org/" ' +
         'xmlns:rgm="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Negocio.WebServices.Mensagem" ' +
         'xmlns:rgm1="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Dominio.DataTransferObject.Prestador" ' +
         'xmlns:rgm2="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Dominio.DataTransferObject.Contribuinte">' +
            '<soap12:Header/>' +
            '<soap12:Body>' +
             '<tem:Cancelar>' +
               DadosMsg +
             '</tem:Cancelar>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorEgoverneISS.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="utf-8"?>' +
         '<soap12:Envelope xmlns:soap12="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tem="http://tempuri.org/" ' +
         'xmlns:rgm="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Negocio.WebServices.Mensagem" ' +
         'xmlns:rgm1="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Dominio.DataTransferObject.Prestador" ' +
         'xmlns:rgm2="http://schemas.datacontract.org/2004/07/Rgm.Eissnfe.Dominio.DataTransferObject.Contribuinte">' +
            '<soap12:Header/>' +
            '<soap12:Body>' +
             '<tem:Emitir>' +
               DadosMsg +
             '</tem:Emitir>' +
            '</soap12:Body>' +
           '</soap12:Envelope>';
end;

function TProvedorEgoverneISS.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEgoverneISS.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorEgoverneISS.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := '';
   acConsSit:     Result := '';
   acConsLote:    Result := '';
   acConsNFSeRps: Result := '';
   acConsNFSe:    Result := '';
   acCancelar:    Result := 'http://tempuri.org/INotaFiscalEletronicaServico/Cancelar';
   acGerar:       Result := 'http://tempuri.org/INotaFiscalEletronicaServico/Emitir';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorEgoverneISS.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, '', True );
   acConsSit:     Result := SeparaDados( RetornoWS, '', True );
   acConsLote:    Result := SeparaDados( RetornoWS, '', True );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, '', True );
   acConsNFSe:    Result := SeparaDados( RetornoWS, '', True );
   acCancelar:    Result := RetornoWS;
   acGerar:       Result := RetornoWS;

//   acCancelar:    Result := SeparaDados( RetornoWS, 'http://tempuri.org/INotaFiscalEletronicaServico/CancelarResponse', True );
//   acGerar:       Result := SeparaDados( RetornoWS, 'http://tempuri.org/INotaFiscalEletronicaServico/EmitirResponse', True );
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorEgoverneISS.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse>' +
            RetNFSe +
           '</CompNfse>';
end;

function TProvedorEgoverneISS.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
