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

unit ACBrProvedorLexsom;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorLexsom }

 TProvedorLexsom = class(TProvedorClass)
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

{ TProvedorLexsom }

constructor TProvedorLexsom.Create;
begin
 {----}
end;

function TProvedorLexsom.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap        := '1.1';
  ConfigCidade.Prefixo2          := '';
  ConfigCidade.Prefixo3          := '';
  ConfigCidade.Prefixo4          := '';
  ConfigCidade.Identificador     := 'Id'; // Dever ser trocado depois para id
  ConfigCidade.QuebradeLinha     := ';';
  ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorLexsom.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1.00';
  ConfigSchema.VersaoDados           := '1.00';
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

function TProvedorLexsom.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  case ACodCidade of
    4101804: begin // Araucaria/PR
               ConfigURL.HomNomeCidade := 'araucaria.pr';
               ConfigURL.ProNomeCidade := 'araucaria.pr';
             end;
    4108304: begin // Foz Do Iguacu/PR
               ConfigURL.HomNomeCidade := 'pmfi.pr';
               ConfigURL.ProNomeCidade := 'pmfi.pr';
             end;
  end;

  ConfigURL.HomRecepcaoLoteRPS    := 'http://homologa.nfse.'+ ConfigURL.HomNomeCidade +'.gov.br/nfsews/nfse.asmx';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.'+ ConfigURL.ProNomeCidade +'.gov.br/nfsews/nfse.asmx';
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

function TProvedorLexsom.GetURI(URI: String): String;
begin
  Result := URI;
end;

function TProvedorLexsom.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorLexsom.GetValidarLote: Boolean;
begin
  Result := True;
end;

function TProvedorLexsom.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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

function TProvedorLexsom.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
  Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
             '<versaoDados>' + VersaoDados + '</versaoDados>'+
            '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorLexsom.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
  Result := '';
end;

function TProvedorLexsom.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorLexsom.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<RecebeLoteRPS xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</RecebeLoteRPS>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<ConsultarSituacaoLoteRPS xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</ConsultarSituacaoLoteRPS>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<ConsultarLoteRPS xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</ConsultarLoteRPS>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<ConsultarNFSEPorRPS xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</ConsultarNFSEPorRPS>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<ConsultaNFSE xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</ConsultaNFSE>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                             'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                             'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">' +
             '<soap12:Body>' +
              '<CancelamentoNFSE xmlns="' + URLNS + '/">' +
               '<xml>' +
                 DadosMsg +
               '</xml>' +
              '</CancelamentoNFSE>' +
             '</soap12:Body>' +
            '</soap12:Envelope>';
end;

function TProvedorLexsom.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
   Result := '';
end;

function TProvedorLexsom.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
   Result := '';
end;

function TProvedorLexsom.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorLexsom.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://tempuri.org/RecebeLoteRPS';
   acConsSit:     Result := 'http://tempuri.org/ConsultarSituacaoLoteRPS';
   acConsLote:    Result := 'http://tempuri.org/ConsultarLoteRPS';
   acConsNFSeRps: Result := 'http://tempuri.org/ConsultarNFSEPorRPS';
   acConsNFSe:    Result := 'http://tempuri.org/ConsultaNFSE';
   acCancelar:    Result := 'http://tempuri.org/CancelamentoNFSE';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorLexsom.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
  case Acao of
    acRecepcionar: begin
                     Result := SeparaDados( RetornoWS, 'RecebeLoteRPSResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
    acConsSit:     begin
                     Result := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRPSResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
    acConsLote:    begin
                     Result := SeparaDados( RetornoWS, 'ConsultarLoteRPSResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
    acConsNFSeRps: begin
                     Result := SeparaDados( RetornoWS, 'ConsultarNFSEPorRPSResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
    acConsNFSe:    begin
                     Result := SeparaDados( RetornoWS, 'ConsultaNFSEResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
    acCancelar:    begin
                     Result := SeparaDados( RetornoWS, 'CancelamentoNFSEResponse' );
                     if Result = '' then
                       Result := SeparaDados( RetornoWS, 'soap12:Body' );
                   end;
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
  end;
end;

function TProvedorLexsom.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
            '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">' +
              RetNfse +
            '</' + Prefixo + 'CompNfse>';
end;

function TProvedorLexsom.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
