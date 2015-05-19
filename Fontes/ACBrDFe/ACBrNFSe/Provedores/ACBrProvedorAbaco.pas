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

unit ACBrProvedorAbaco;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorAbaco }

 TProvedorAbaco = class(TProvedorClass)
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

{ TProvedorAbaco }

constructor TProvedorAbaco.Create;
begin
 {----}
end;

function TProvedorAbaco.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap        := '1.1';
  ConfigCidade.Prefixo2          := '';
  ConfigCidade.Prefixo3          := '';
  ConfigCidade.Prefixo4          := '';
  ConfigCidade.Identificador     := 'id';
  ConfigCidade.QuebradeLinha     := ';';
  ConfigCidade.NameSpaceEnvelope := 'http://www.e-nfs.com.br';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorAbaco.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '201001';
  ConfigSchema.VersaoDados           := 'V2010';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://www.e-nfs.com.br';
  ConfigSchema.Cabecalho             := 'nfse_v2010.xsd';
  ConfigSchema.ServicoEnviar         := 'nfse_v2010.xsd';
  ConfigSchema.ServicoConSit         := 'nfse_v2010.xsd';
  ConfigSchema.ServicoConLot         := 'nfse_v2010.xsd';
  ConfigSchema.ServicoConRps         := 'nfse_v2010.xsd';
  ConfigSchema.ServicoConNfse        := 'nfse_v2010.xsd';
  ConfigSchema.ServicoCancelar       := 'nfse_v2010.xsd';
  ConfigSchema.ServicoGerar          := 'nfse_v2010.xsd';
  ConfigSchema.ServicoEnviarSincrono := 'nfse_v2010.xsd';
  ConfigSchema.ServicoSubstituir     := 'nfse_v2010.xsd';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorAbaco.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  case ACodCidade of
   3201209: begin // Cachoeiro De Itapemirim/ES
              ConfigURL.HomNomeCidade := 'cachoeiro';
              ConfigURL.ProNomeCidade := 'cachoeiro';
            end;
   4304606: begin // Canoas/RS
              ConfigURL.HomNomeCidade := 'canoas';
              ConfigURL.ProNomeCidade := 'e-nfs_canoas';
            end;
   5107040: begin // Primavera do Leste/MT
              ConfigURL.HomNomeCidade := 'primavera';
              ConfigURL.ProNomeCidade := 'e-nfs_primavera';
            end;
   5107602: begin // Rondonopolis/MT
              ConfigURL.HomNomeCidade := 'rondonopolis';
              ConfigURL.ProNomeCidade := 'e-nfs_roo';
            end;
  end;

 (*
 ConfigURL.HomRecepcaoLoteRPS    := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/arecepcionarloterps?wsdl';
 ConfigURL.HomConsultaLoteRPS    := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/aconsultarloterps?wsdl';
 ConfigURL.HomConsultaNFSeRPS    := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/aconsultarnfseporrps?wsdl';
 ConfigURL.HomConsultaSitLoteRPS := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/aconsultarsituacaoloterps?wsdl';
 ConfigURL.HomConsultaNFSe       := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/aconsultarnfse?wsdl';
 ConfigURL.HomCancelaNFSe        := 'http://186.233.149.102/'+ ConfigURL.HomNomeCidade +'_homologa/servlet/acancelarnfse?wsdl';
 *)

  ConfigURL.HomRecepcaoLoteRPS    := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/arecepcionarloterps?wsdl';
  ConfigURL.HomConsultaLoteRPS    := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/aconsultarloterps?wsdl';
  ConfigURL.HomConsultaNFSeRPS    := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/aconsultarnfseporrps?wsdl';
  ConfigURL.HomConsultaSitLoteRPS := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/aconsultarsituacaoloterps?wsdl';
  ConfigURL.HomConsultaNFSe       := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/aconsultarnfse?wsdl';
  ConfigURL.HomCancelaNFSe        := 'http://homologa.e-nfs.com.br/'+ ConfigURL.HomNomeCidade +'/servlet/acancelarnfse?wsdl';
  ConfigURL.HomGerarNFSe          := '';
  ConfigURL.HomRecepcaoSincrono   := '';
  ConfigURL.HomSubstituiNFSe      := '';

  ConfigURL.ProRecepcaoLoteRPS    := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/arecepcionarloterps?wsdl';
  ConfigURL.ProConsultaLoteRPS    := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/aconsultarloterps?wsdl';
  ConfigURL.ProConsultaNFSeRPS    := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/aconsultarnfseporrps?wsdl';
  ConfigURL.ProConsultaSitLoteRPS := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/aconsultarsituacaoloterps?wsdl';
  ConfigURL.ProConsultaNFSe       := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/aconsultarnfse?wsdl';
  ConfigURL.ProCancelaNFSe        := 'https://www.e-nfs.com.br/'+ ConfigURL.ProNomeCidade +'/servlet/acancelarnfse?wsdl';
  ConfigURL.ProGerarNFSe          := '';
  ConfigURL.ProRecepcaoSincrono   := '';
  ConfigURL.ProSubstituiNFSe      := '';

  Result := ConfigURL;
end;

function TProvedorAbaco.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorAbaco.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorAbaco.GetValidarLote: Boolean;
begin
 Result := False;
end;

function TProvedorAbaco.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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

function TProvedorAbaco.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorAbaco.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorAbaco.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorAbaco.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:RecepcionarLoteRps.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:RecepcionarLoteRps.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:ConsultarSituacaoLoteRps.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:ConsultarSituacaoLoteRps.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:ConsultarLoteRps.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:ConsultarLoteRps.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:ConsultarNfsePorRps.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:ConsultarNfsePorRps.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:ConsultarNfse.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:ConsultarNfse.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:e="' + URLNS + '">' +
            '<S:Header/>' +
             '<S:Body>' +
              '<e:CancelarNfse.Execute>' +

               '<e:Nfsecabecmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsecabecmsg>' +
               '<e:Nfsedadosmsg>' +
                 '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                 StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
               '</e:Nfsedadosmsg>' +

              '</e:CancelarNfse.Execute>' +
             '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorAbaco.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorAbaco.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorAbaco.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorAbaco.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://www.e-nfs.com.braction/ARECEPCIONARLOTERPS.Execute';
   acConsSit:     Result := 'http://www.e-nfs.com.braction/ACONSULTARSITUACAOLOTERPS.Execute';
   acConsLote:    Result := 'http://www.e-nfs.com.braction/ACONSULTARLOTERPS.Execute';
   acConsNFSeRps: Result := 'http://www.e-nfs.com.braction/ACONSULTARNFSEPORRPS.Execute';
   acConsNFSe:    Result := 'http://www.e-nfs.com.braction/ACONSULTARNFSE.Execute';
   acCancelar:    Result := 'http://www.e-nfs.com.braction/ACANCELARNFSE.Execute';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorAbaco.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'Outputxml' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'Outputxml' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'Outputxml' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'Outputxml' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'Outputxml' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'Outputxml' );
   acGerar:       Result := SeparaDados( RetornoWS, 'Outputxml' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'Outputxml' );
   acSubstituir:  Result := SeparaDados( RetornoWS, 'Outputxml' );
 end;
end;

function TProvedorAbaco.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.e-nfs.com.br">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorAbaco.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
