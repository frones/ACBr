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

unit ACBrProvedorSimplISS;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorSimplISS }

 TProvedorSimplISS = class(TProvedorClass)
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

{ TProvedorSimplISS }

constructor TProvedorSimplISS.Create;
begin
 {----}
end;

function TProvedorSimplISS.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://www.sistema.com.br/Sistema.Ws.Nfse'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://www.sistema.com.br/Sistema.Ws.Nfse';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorSimplISS.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1.00';
  ConfigSchema.VersaoDados           := '1.00';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://www.sistema.com.br/Nfse/arquivos/';
  ConfigSchema.Cabecalho             := 'nfse_3.xsd';
  ConfigSchema.ServicoEnviar         := 'nfse_3.xsd';
  ConfigSchema.ServicoConSit         := 'nfse_3.xsd';
  ConfigSchema.ServicoConLot         := 'nfse_3.xsd';
  ConfigSchema.ServicoConRps         := 'nfse_3.xsd';
  ConfigSchema.ServicoConNfse        := 'nfse_3.xsd';
  ConfigSchema.ServicoCancelar       := 'nfse_3.xsd';
  ConfigSchema.ServicoGerar          := 'nfse_3.xsd';
  ConfigSchema.ServicoEnviarSincrono := 'nfse_3.xsd';
  ConfigSchema.ServicoSubstituir     := 'nfse_3.xsd';
  ConfigSchema.DefTipos              := '';

  Result := ConfigSchema;
end;

function TProvedorSimplISS.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
 if ACodCidade = 3503307
  then begin
   ConfigURL.HomNomeCidade      := '';
   ConfigURL.HomRecepcaoLoteRPS := 'http://200.144.16.82:8080/ws_homologacao/nfseservice.svc';
  end
  else begin
   ConfigURL.HomNomeCidade      := '';
   ConfigURL.HomRecepcaoLoteRPS := 'http://187.45.245.217/ws_nfse/nfseservice.svc';
  end;

 case ACodCidade of
  3103504: begin
            ConfigURL.ProNomeCidade      := 'araguari';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3105103: begin
            ConfigURL.ProNomeCidade      := 'bambui';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3137205: begin
            ConfigURL.ProNomeCidade      := 'lagoa_prata';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3148103: begin // Patrocinio/MG
            ConfigURL.ProNomeCidade      := 'patrocinio';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3503307: begin // Araras/SP
            ConfigURL.ProNomeCidade      := 'araras';
            ConfigURL.ProRecepcaoLoteRPS := 'http://189.56.68.34:8080/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3511508: begin // Cerquilho/SP
            ConfigURL.ProNomeCidade      := 'cerquilho';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3514106: begin // Dois Corregos/SP
            ConfigURL.ProNomeCidade      := 'dois_corregos';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_nfse_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3520608: begin // Indiana/SP
            ConfigURL.ProNomeCidade      := 'indiana';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3530201: begin // Mirante Do Paranapanema/SP
            ConfigURL.ProNomeCidade      := 'mirantedoparanapanema';
            ConfigURL.ProRecepcaoLoteRPS := 'http://wsmirantedoparanapanema.simplissweb.com.br/nfseservice.svc';
           end;
  3524808: begin // Jales/SP
            ConfigURL.ProNomeCidade      := 'jales';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3534609: begin // Osvaldo Cruz/SP
            ConfigURL.ProNomeCidade      := 'nfse_osvaldo_cruz';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3538709: begin // Piracicaba/SP
            ConfigURL.ProNomeCidade      := 'piracicaba';
            ConfigURL.ProRecepcaoLoteRPS := 'http://www.sistemas.piracicaba.sp.gov.br/semfi/simpliss/ws_nfse/nfseservice.svc';
           end;
  3541406: begin // Presidente Prudente/SP
            ConfigURL.ProNomeCidade      := 'presidente_prudente';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_nfse_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3549102: begin
            ConfigURL.ProNomeCidade      := 'sao_joao_boa_vista';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_nfse_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3549706: begin
            ConfigURL.ProNomeCidade      := 'sao_jose_rio_pardo';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  3556404: begin
            ConfigURL.ProNomeCidade      := 'vargem_grande_sul';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_nfse_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
           end;
  4202008: begin
            ConfigURL.ProNomeCidade      := 'balneario_camboriu';
            ConfigURL.ProRecepcaoLoteRPS := 'http://187.45.245.217/ws_' + ConfigURL.ProNomeCidade + '/nfseservice.svc';
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

function TProvedorSimplISS.GetURI(URI: String): String;
begin
 // No provedor SimplISS a URI não é informada.
 Result := '';
end;

function TProvedorSimplISS.GetAssinarXML(Acao: TnfseAcao): Boolean;
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
   acConsSecRps:  Result := False;
 end;
end;

function TProvedorSimplISS.GetValidarLote: Boolean;
begin
 Result :=False;
end;

function TProvedorSimplISS.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio>' +
                             '<' + Prefixo3 + 'Pedido ' + NameSpaceDad +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio>' +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorSimplISS.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorSimplISS.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '<P1 xmlns="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn">' +
             OnlyNumber(CNPJ) +
           '</P1>' +
           '<P2 xmlns="http://www.sistema.com.br/Sistema.Ws.Nfse.Cn">' +
             Senha +
           '</P2>';
end;

function TProvedorSimplISS.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorSimplISS.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<RecepcionarLoteRps xmlns="' + URLNS + '">' +
                DadosMsg +
              '<pParam>' +
                DadosSenha +
              '</pParam>' +
             '</RecepcionarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarSituacaoLoteRps xmlns="' + URLNS + '">' +
               DadosMsg +
             '<pParam>' +
               DadosSenha +
             '</pParam>' +
            '</ConsultarSituacaoLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarLoteRps xmlns="' + URLNS + '">' +
               DadosMsg +
             '<pParam>' +
               DadosSenha +
             '</pParam>' +
            '</ConsultarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNfsePorRps xmlns="' + URLNS + '">' +
               DadosMsg +
             '<pParam>' +
               DadosSenha +
             '</pParam>' +
            '</ConsultarNfsePorRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNfse xmlns="' + URLNS + '">' +
               DadosMsg +
             '<pParam>' +
               DadosSenha +
             '</pParam>' +
            '</ConsultarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<CancelarNfse xmlns="' + URLNS + '">' +
               DadosMsg +
             '<pParam>' +
               DadosSenha +
             '</pParam>' +
            '</CancelarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorSimplISS.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSimplISS.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSimplISS.GeraEnvelopeSubstituirNFSe(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSimplISS.GetSoapAction(Acao: TnfseAcao;
  NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/RecepcionarLoteRps';
   acConsSit:     Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarSituacaoLoteRps';
   acConsLote:    Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/ConsultarNfse';
   acCancelar:    Result := 'http://www.sistema.com.br/Sistema.Ws.Nfse/INfseService/CancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorSimplISS.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
var
 RetWS: AnsiString;
begin
 case Acao of
   acRecepcionar: begin
                   RetWS := SeparaDados( RetornoWS, 'RecepcionarLoteRpsResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<EnviarLoteRpsResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</EnviarLoteRpsResposta>';
                   Result := RetWS;
                  end;
   acConsSit:     begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarSituacaoLoteRpsResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<ConsultarSituacaoLoteRpsResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</ConsultarSituacaoLoteRpsResposta>';
                   Result := RetWS;
                  end;
   acConsLote:    begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarLoteRpsResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<ConsultarLoteRpsResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</ConsultarLoteRpsResposta>';
                   Result := RetWS;
                  end;
   acConsNFSeRps: begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarNfsePorRpsResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<ConsultarNfseRpsResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</ConsultarNfseRpsResposta>';
                   Result := RetWS;
                  end;
   acConsNFSe:    begin
                   RetWS := SeparaDados( RetornoWS, 'ConsultarNfseResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<ConsultarNfseResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</ConsultarNfseResposta>';
                   Result := RetWS;
                  end;
   acCancelar:    begin
                   RetWS := SeparaDados( RetornoWS, 'CancelarNfseResult' );
                   RetWS := StringReplace(RetWS, ' xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd"', '', [rfReplaceAll]);
                   RetWS := '<CancelarNfseResposta xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
                              RetWS +
                            '</CancelarNfseResposta>';
                   Result := RetWS;
                  end;
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSimplISS.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorSimplISS.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
