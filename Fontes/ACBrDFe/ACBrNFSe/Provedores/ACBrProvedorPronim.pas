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

unit ACBrProvedorPronim;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorPronim }

 TProvedorPronim = class(TProvedorClass)
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

{ TProvedorPronim }

constructor TProvedorPronim.Create;
begin
 {----}
end;

function TProvedorPronim.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := 'Id'; // Alterado para poder Assinar no Schema é id
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaGerar := True;

  case ACodCidade of
   3118601: ConfigCidade.AssinaLote := True; // Contagem/MG
   3504008: ConfigCidade.AssinaLote := True; // Assis/SP
   3530300: ConfigCidade.AssinaLote := True; // Mirassol/SP
   4216602: ConfigCidade.AssinaLote := True; // São José/SC
   4309407: ConfigCidade.AssinaLote := True; // Guapore/RS
   4310207: ConfigCidade.AssinaLote := True; // Ijuí/RS
   4320800: ConfigCidade.AssinaLote := True; // Soledade/RS
   3545407: ConfigCidade.AssinaLote := True; // Salto Grando/SP
  else      begin
             ConfigCidade.Identificador := 'id';
             ConfigCidade.AssinaLote    := False;
             ConfigCidade.AssinaGerar   := False;
            end;
  end;

  Result := ConfigCidade;
end;

function TProvedorPronim.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
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

function TProvedorPronim.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
 case ACodCidade of
  3202405: begin // Guarapari/ES
            ConfigURL.HomNomeCidade         := '';
            ConfigURL.HomRecepcaoLoteRPS    := 'http://nfseteste.guarapari.es.gov.br/NFSEWSTESTE/Services.svc';

            ConfigURL.ProNomeCidade         := '';
            ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.guarapari.es.gov.br/NFSEws/Services.svc';
           end;

  3118601: begin // Contagem/MG
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://teste.contagem.mg.gov.br/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.contagem.mg.gov.br/NFSEWS/Services.svc';
           end;
  3143302: begin // Montes Claros/MG
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfeteste.montesclaros.mg.gov.br:8081/nfsewsteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfe.montesclaros.mg.gov.br:8082/NFSEws/Services.svc';
           end;
  3304706: begin // Santo Antonio de Padua/RJ
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://177.67.128.86/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://177.67.128.86/NFSEWS/Services.svc';
           end;
  3504008: begin // Assis/SP
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfseteste.assis.sp.gov.br/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://189.57.228.200/NFSEWS/Services.svc';
           end;
  3505609: begin // Barrinha/SP
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://186.232.87.226:90/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://186.232.87.226:90/NFSEWS/Services.svc';
           end;
  3511102: begin // Catanduva/SP
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfse.catanduva.sp.gov.br/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.catanduva.sp.gov.br/NFSEWS/Services.svc';
           end;
  3530300: begin // Mirassol/SP
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfse.mirassol.sp.gov.br:5555/NFSEWSteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.mirassol.sp.gov.br:5555/NFSEWS/Services.svc';
           end;
  4102000: begin // Assis Chateaubriand/PR
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://177.38.165.34:8184/nfsewsteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://177.38.165.34:8184/nfsews/Services.svc';
           end;
  4118501: begin // Pato Branco/PR
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfse.patobranco.pr.gov.br/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.patobranco.pr.gov.br/NFSEws/Services.svc';
           end;
  4122404: begin // Rolandia/PR
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := '';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.rolandia.pr.gov.br/NFSEws/Services.svc';
           end;
  4201307: begin // Araquari/SC
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://201.14.131.162:8288/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://201.14.131.162:8288/NFSEWS/Services.svc';
           end;
  4210506: begin // Maravilha/SC
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://187.45.102.245:8090/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://187.45.102.245:8090/NFSEWS/Services.svc';
           end;
  4216602: begin // São José/SC
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://nfse.pmsj.sc.gov.br:90/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://nfse.pmsj.sc.gov.br:90/NFSEWS/Services.svc';
           end;
  4303509: begin // Camaqua/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://portal.camaqua.rs.gov.br:8181/nfsewsteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://portal.camaqua.rs.gov.br/nfsews/Services.svc';
           end;
  4308102: begin // Feliz/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://187.84.56.69:8082/nfsewsteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://187.84.56.68:8081/nfsews/Services.svc';
           end;
  4309407: begin // Guapore/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://177.20.255.245/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://177.20.255.244/NFSEWS/Services.svc';
           end;
  4310207: begin // Ijui/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://ambienteteste.ijui.rs.gov.br/nfsewsteste/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://server21.ijui.rs.gov.br/nfsews/Services.svc';
           end;
  4320800: begin // Soledade/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := '';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://177.101.230.30/nfsews/services.svc';
           end;
  4322400: begin // Uruguaiana/RS
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://dueto-web.uruguaiana.rs.gov.br:7778/NFSEWSTESTE/Services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://dueto-web.uruguaiana.rs.gov.br:7778/NFSEWS/Services.svc';
           end;
  3545407: begin // Salto Grande/SP
             ConfigURL.HomNomeCidade         := '';
             ConfigURL.HomRecepcaoLoteRPS    := 'http://200.192.244.89/nfsewsteste/services.svc';

             ConfigURL.ProNomeCidade         := '';
             ConfigURL.ProRecepcaoLoteRPS    := 'http://200.192.244.89/nfsews/services.svc';
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

function TProvedorPronim.GetURI(URI: String): String;
begin
 // No provedor Pronim a URI não é informada.
 Result := '';
end;

function TProvedorPronim.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorPronim.GetValidarLote: Boolean;
begin
 Result := False; {Dalvan}
end;

function TProvedorPronim.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
//   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfsePorRpsEnvio' + NameSpaceDad;
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

function TProvedorPronim.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorPronim.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorPronim.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorPronim.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<RecepcionarLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</RecepcionarLoteRps>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<ConsultarSituacaoLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarSituacaoLoteRps>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<ConsultarLoteRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarLoteRps>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<ConsultarNfsePorRps xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarNfsePorRps>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<ConsultarNfse xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                DadosMsg +
              '</xmlEnvio>' +
             '</ConsultarNfse>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 DadosMsg :=StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, '>', '&gt;', [rfReplaceAll]);
 DadosMsg :=StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                       'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">' +
            '<soap:Body>' +
             '<CancelarNfse xmlns="' + URLNS + '/">' +
              '<xmlEnvio>' +
                DadosMsg +
              '</xmlEnvio>' +
             '</CancelarNfse>' +
            '</soap:Body>' +
           '</soap:Envelope>';
end;

function TProvedorPronim.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorPronim.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorPronim.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorPronim.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
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

function TProvedorPronim.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
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
end;

function TProvedorPronim.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorPronim.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
var
 sCNPJ: String;
begin

 sCNPJ := ''; // Incluido somente para poder compilar, deve ser feita uma alteração nessa
              // function contemplando a passagem do parametro CNPJ

 case ACodMunicipio of
  4310207: begin // Ijui/RS
            Result := 'http://server21.ijui.rs.gov.br/nfse/VisualizarXMLdaNota.aspx?Prestador=' + sCNPJ +
                      '&Numero=' + IntToStr(ANumeroNFSe) +
                      '&Codigo=' + ACodVerificacao +
                      '&page=default.aspx&origin=ConAut&pdf=true'
           end;
  else Result := '';
 end;
end;

end.
