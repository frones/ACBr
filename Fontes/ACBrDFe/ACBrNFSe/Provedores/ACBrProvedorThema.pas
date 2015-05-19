{$WARNINGS OFF}
{$HINTS OFF}
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

unit ACBrProvedorThema;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorThema }

 TProvedorThema = class(TProvedorClass)
  protected
   { protected }
  private
   { private }
   FMetodoRecepcionar: String;
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

uses
  pcnLeitor, pcnConversao;

{ TProvedorThema }

constructor TProvedorThema.Create;
begin
 FMetodoRecepcionar := 'recepcionarLoteRps';
end;

function TProvedorThema.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := '';
  ConfigCidade.Identificador := '';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://server.nfse.thema.inf.br'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://server.nfse.thema.inf.br';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorThema.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
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

function TProvedorThema.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
 sHTTPHom, sHTTPPro: String;
begin
 sHTTPHom := 'http://';
 sHTTPPro := 'https://';

 case ACodCidade of
  4205902: begin // Gaspar/SC
            ConfigURL.HomNomeCidade := 'nfsehml.gaspar.sc.gov.br';
            ConfigURL.ProNomeCidade := 'nfse.gaspar.sc.gov.br/nfse';
           end;
  4303103: begin // Cachoeirinha/RS
            ConfigURL.HomNomeCidade := 'nfsehomologacao.cachoeirinha.rs.gov.br/nfse';
            ConfigURL.ProNomeCidade := 'nfse.cachoeirinha.rs.gov.br/nfse';
            sHTTPPro := 'http://';
           end;
  4307708: begin // Esteio/Rs
            ConfigURL.HomNomeCidade := 'grp.esteio.rs.gov.br/nfsehml';
			ConfigURL.ProNomeCidade := 'grp.esteio.rs.gov.br/nfse';
			sHTTPPro := 'http://';
           end; 		
  4311403: begin // Lajeado/RS
            ConfigURL.HomNomeCidade := 'nfsehml.lajeado.rs.gov.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfse.lajeado.rs.gov.br/thema-nfse';
            sHTTPPro := 'http://';
           end;
  4312401: begin // Montenegro/RS
            ConfigURL.HomNomeCidade := 'nfsehml.montenegro.rs.gov.br/nfsehml';
            ConfigURL.ProNomeCidade := 'nfe.montenegro.rs.gov.br/thema-nfse';
            sHTTPHom := 'https://';
           end;
  4312658: begin // Nao-Me-Toque/RS
            ConfigURL.HomNomeCidade := 'nfsehml.naometoquers.com.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfse.naometoquers.com.br/thema-nfse';
            sHTTPPro := 'http://';
           end;
  4314100: begin // Passo Fundo/RS
            ConfigURL.HomNomeCidade := 'nfsehomologacao.pmpf.rs.gov.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfse.pmpf.rs.gov.br/thema-nfse';
            sHTTPPro := 'http://';
           end;
  4316808: begin // Santa Cruz Do Sul/RS
            ConfigURL.HomNomeCidade := 'grphml.santacruz.rs.gov.br/thema-nfse-hml';
            ConfigURL.ProNomeCidade := 'nfse.santacruz.rs.gov.br/thema-nfse';
//            ConfigURL.HomNomeCidade := 'grphml.santacruz.rs.gov.br/thema-nfse-hml';
//            ConfigURL.ProNomeCidade := 'pmsc.server.01.solidit.com.br:7082/thema-nfse';
            sHTTPPro := 'http://';
           end;
  4318705: begin // Sao Leopoldo/RS
            ConfigURL.HomNomeCidade := 'nfehomologacao.saoleopoldo.rs.gov.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfe.saoleopoldo.rs.gov.br/thema-nfse';
           end;
  4321204: begin // Taquara/RS
            ConfigURL.HomNomeCidade := 'nfsehomologacao.taquara.rs.gov.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfse.taquara.rs.gov.br/thema-nfse';
            sHTTPPro := 'http://';
           end;
  4322608: begin // Venancio Aires/RS
            ConfigURL.HomNomeCidade := 'nfehml.venancioaires.rs.gov.br/thema-nfse';
            ConfigURL.ProNomeCidade := 'nfe.venancioaires.rs.gov.br/thema-nfse';
            sHTTPPro := 'http://';
           end;
 end;

 ConfigURL.HomRecepcaoLoteRPS    := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEremessa';
 ConfigURL.HomConsultaLoteRPS    := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEconsulta';
 ConfigURL.HomConsultaNFSeRPS    := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEconsulta';
 ConfigURL.HomConsultaSitLoteRPS := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEconsulta';
 ConfigURL.HomConsultaNFSe       := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEconsulta';
 ConfigURL.HomCancelaNFSe        := sHTTPHom + ConfigURL.HomNomeCidade + '/services/NFSEcancelamento';
 ConfigURL.HomGerarNFSe          := '';
 ConfigURL.HomRecepcaoSincrono   := '';
 ConfigURL.HomSubstituiNFSe      := '';

 ConfigURL.ProRecepcaoLoteRPS    := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEremessa';
 ConfigURL.ProConsultaLoteRPS    := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEconsulta';
 ConfigURL.ProConsultaNFSeRPS    := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEconsulta';
 ConfigURL.ProConsultaSitLoteRPS := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEconsulta';
 ConfigURL.ProConsultaNFSe       := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEconsulta';
 ConfigURL.ProCancelaNFSe        := sHTTPPro + ConfigURL.ProNomeCidade + '/services/NFSEcancelamento';
 ConfigURL.ProGerarNFSe          := '';
 ConfigURL.ProRecepcaoSincrono   := '';
 ConfigURL.ProSubstituiNFSe      := '';

 Result := ConfigURL;
end;

function TProvedorThema.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorThema.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorThema.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorThema.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
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
                              '<' + Prefixo4 + 'InfPedidoCancelamento Id' + '="' + URI + '">';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorThema.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorThema.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorThema.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
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

function TProvedorThema.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
var
   LoteRps      : string;
   Leitor       : TLeitor;
   QuantidadeRps: Integer;
begin
{ - Incluído por Márcio Teixeira em 05/03/2013 -
    O provedor permite o envio de até 3 rps por
    lote no modo síncrono, então verifico a
    quantidade de Rps no lote e valido qual
    médoto posso utilizar. Coloco numa variável
    privada porque na ação ele também deve ir
    o mesmo nome.
  }
Leitor := TLeitor.Create;

try
 Leitor.Arquivo := DadosMsg;
 Leitor.Grupo   := Leitor.Arquivo;
 LoteRps        := Leitor.rExtrai(1, 'LoteRps');
 QuantidadeRps  := Leitor.rCampo(tcInt, 'QuantidadeRps');

 if QuantidadeRps <= 3 then
  FMetodoRecepcionar := 'recepcionarLoteRpsLimitado'
 else
  FMetodoRecepcionar := 'recepcionarLoteRps';
finally
 FreeAndNil(Leitor);
end;

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<' + FMetodoRecepcionar + ' xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</' + FMetodoRecepcionar + '>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<consultarSituacaoLoteRps xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</consultarSituacaoLoteRps>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<consultarLoteRps xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</consultarLoteRps>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<consultarNfsePorRps xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</consultarNfsePorRps>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<consultarNfse xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</consultarNfse>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<cancelarNfse xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</cancelarNfse>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorThema.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorThema.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorThema.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorThema.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'urn:' + FMetodoRecepcionar;
   acConsSit:     Result := 'urn:consultarSituacaoLoteRps';
   acConsLote:    Result := 'urn:consultarLoteRps';
   acConsNFSeRps: Result := 'urn:consultarNfsePorRps';
   acConsNFSe:    Result := 'urn:consultarNfse';
   acCancelar:    Result := 'urn:cancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorThema.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'ns:return' );
   acConsSit:     Result := SeparaDados( RetornoWS, 'ns:return' );
   acConsLote:    Result := SeparaDados( RetornoWS, 'ns:return' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ns:return' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ns:return' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'ns:return' );
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorThema.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorThema.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
{$HINTS ON}
{$WARNINGS ON}
