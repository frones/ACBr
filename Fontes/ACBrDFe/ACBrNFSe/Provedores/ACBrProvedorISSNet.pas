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

unit ACBrProvedorISSNet;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorISSNet }

 TProvedorISSNet = class(TProvedorClass)
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

{ TProvedorISSNet }

constructor TProvedorISSNet.Create;
begin
 {----}
end;

function TProvedorISSNet.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := '';
  ConfigCidade.Prefixo4      := 'tc:';
  ConfigCidade.Identificador := '';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://www.issnetonline.com.br/webservice/nfd'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://www.issnetonline.com.br/webservice/nfd';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorISSNet.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '1.00';
  ConfigSchema.VersaoDados           := '1.00';
  ConfigSchema.VersaoXML             := '1';
  ConfigSchema.NameSpaceXML          := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/';
  ConfigSchema.Cabecalho             := '';
  ConfigSchema.ServicoEnviar         := 'servico_enviar_lote_rps_envio.xsd';
  ConfigSchema.ServicoConSit         := 'servico_consultar_situacao_lote_rps_envio.xsd';
  ConfigSchema.ServicoConLot         := 'servico_consultar_lote_rps_envio.xsd';
  ConfigSchema.ServicoConRps         := 'servico_consultar_nfse_rps_envio.xsd';
  ConfigSchema.ServicoConNfse        := 'servico_consultar_nfse_envio.xsd';
  ConfigSchema.ServicoCancelar       := 'servico_cancelar_nfse_envio.xsd';
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := 'tipos_complexos.xsd';

  Result := ConfigSchema;
end;

function TProvedorISSNet.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade := 'homologacao';

  case ACodCidade of
       999: ConfigURL.ProNomeCidade := 'homologacao';
   1702109: ConfigURL.ProNomeCidade := 'araguaina';          // Araguaína/TO
   3139607: ConfigURL.ProNomeCidade := 'mantena';            // Mantena/MG
   3169307: ConfigURL.ProNomeCidade := 'trescoracoes';       // Três Corações/MG
   3502101: ConfigURL.ProNomeCidade := 'andradina';          // Andradina/SP
   3502507: ConfigURL.ProNomeCidade := 'aparecida';          // Aparecida/SP
   3522307: ConfigURL.ProNomeCidade := 'itapetininga';       // Itapetininga/SP
   3524402: ConfigURL.ProNomeCidade := 'jacarei';            // Jacareí/SP
   3524709: ConfigURL.ProNomeCidade := 'jaguariuna';         // Jaguariúna/SP
   3527207: ConfigURL.ProNomeCidade := 'lorena';             // Lorena/SP
   3530607: ConfigURL.ProNomeCidade := 'mogidascruzes';      // Mogi das Cruzes/SP
   3541000: ConfigURL.ProNomeCidade := 'praiagrande';        // Praia Grande/SP
   3551009: ConfigURL.ProNomeCidade := 'saovicente';         // São Vicente/SP
   3551504: ConfigURL.ProNomeCidade := 'serrana';            // Serrana/SP
   4104808: ConfigURL.ProNomeCidade := 'cascavel';           // Cascavel/PR
//   4219507: ConfigURL.ProNomeCidade := 'xanxere';            // Xanxerê/SC passou para o provedor Betha
   4313409: ConfigURL.ProNomeCidade := 'novohamburgo';       // Novo Hamburgo/RS
   4316907: ConfigURL.ProNomeCidade := 'santamaria';         // Santa Maria/RS
   5002209: ConfigURL.ProNomeCidade := 'bonito';             // Bonito/MS
   5003702: ConfigURL.ProNomeCidade := 'dourados';           // Dourados/MS
   5100250: ConfigURL.ProNomeCidade := 'altafloresta';       // Alta Floresta/MT
   5103403: ConfigURL.ProNomeCidade := 'cuiaba';             // Cuiaba/MT
   5105101: ConfigURL.ProNomeCidade := 'juara';              // Juara/MT
   5105903: ConfigURL.ProNomeCidade := 'nobres';             // Nobres/MT
   5106232: ConfigURL.ProNomeCidade := 'novaolimpia';        // Nova Olimpia/MT
   5107925: ConfigURL.ProNomeCidade := 'sorriso';            // Sorriso/MT
   5108402: ConfigURL.ProNomeCidade := 'varzeagrande';       // Varzea Grande/MT
   5201108: ConfigURL.ProNomeCidade := 'anapolis';           // Anapolis/GO
   5201405: ConfigURL.ProNomeCidade := 'aparecidadegoiania'; // Aparecida de Goiania/GO
  end;

  ConfigURL.HomRecepcaoLoteRPS    := 'http://www.issnetonline.com.br/webserviceabrasf/homologacao/servicos.asmx';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProRecepcaoLoteRPS    := 'http://www.issnetonline.com.br/webserviceabrasf/' + ConfigURL.ProNomeCidade + '/servicos.asmx';
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

function TProvedorISSNet.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorISSNet.GetAssinarXML(Acao: TnfseAcao): Boolean;
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
   acConsSecRps:  Result := False;
 end;
end;

function TProvedorISSNet.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorISSNet.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + NameSpaceDad;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + NameSpaceDad;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseEnvio' + NameSpaceDad;
   acCancelar:    Result := '<p1:CancelarNfseEnvio' +
                              ' xmlns:p1="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd"' +
                              ' xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"' +
                              ' xmlns:ts="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_simples.xsd">' +
//   NameSpaceDad +
                             '<' + Prefixo3 + 'Pedido>' +
                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                 ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
//   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + NameSpaceDad +
//                             '<' + Prefixo3 + 'Pedido>' +
//                              '<' + Prefixo4 + 'InfPedidoCancelamento' +
//                                 ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
   acGerar:       Result := '<' + Prefixo3 + 'GerarNfseEnvio' + NameSpaceDad;
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + NameSpaceDad;
   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + NameSpaceDad +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorISSNet.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorISSNet.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorISSNet.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</p1:CancelarNfseEnvio>';
//                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorISSNet.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<RecepcionarLoteRps xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</RecepcionarLoteRps>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultaSituacaoLoteRPS xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</ConsultaSituacaoLoteRPS>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarLoteRps xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</ConsultarLoteRps>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNFSePorRPS xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</ConsultarNFSePorRPS>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<ConsultarNfse xmlns="' + URLNS + '">' +
              '<xml>' +
                '&lt;?xml version="1.0" encoding="UTF-8"?&gt;' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</xml>' +
             '</ConsultarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
var
 Texto: AnsiString;
begin
 Texto := '<?xml version="1.0" encoding="UTF-8"?>' +
          DadosMsg;

 Texto := StringReplace(StringReplace(Texto, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<s:Body>' +
             '<CancelarNfse xmlns="' + URLNS + '">' +
              '<xml>' +
                Texto +
              '</xml>' +
             '</CancelarNfse>' +
            '</s:Body>' +
           '</s:Envelope>';
end;

function TProvedorISSNet.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorISSNet.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorISSNet.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorISSNet.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://www.issnetonline.com.br/webservice/nfd/RecepcionarLoteRps';
   acConsSit:     Result := 'http://www.issnetonline.com.br/webservice/nfd/ConsultaSituacaoLoteRPS';
   acConsLote:    Result := 'http://www.issnetonline.com.br/webservice/nfd/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://www.issnetonline.com.br/webservice/nfd/ConsultarNFSePorRPS';
   acConsNFSe:    Result := 'http://www.issnetonline.com.br/webservice/nfd/ConsultarNfse';
   acCancelar:    Result := 'http://www.issnetonline.com.br/webservice/nfd/CancelarNfse';
   acGerar:       Result := '';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorISSNet.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
var
 RetWS: AnsiString;
begin
 case Acao of
   acRecepcionar: begin
                   RetWS  := SeparaDados( RetornoWS, 'RecepcionarLoteRpsResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_enviar_lote_rps_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result := RetWS;
                  end;
   acConsSit:     begin
                   RetWS  := SeparaDados( RetornoWS, 'ConsultaSituacaoLoteRPSResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_situacao_lote_rps_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result := RetWS;
                  end;
   acConsLote:    begin
                   RetWS  := SeparaDados( RetornoWS, 'ConsultarLoteRpsResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_lote_rps_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result := RetWS;
                  end;
   acConsNFSeRps: begin
                   RetWS  := SeparaDados( RetornoWS, 'ConsultarNFSePorRPSResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_rps_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result := RetWS;
                  end;
   acConsNFSe:    begin
                   RetWS  := SeparaDados( RetornoWS, 'ConsultarNfseResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result := RetWS;
                  end;
   acCancelar:    begin
                   RetWS  := SeparaDados( RetornoWS, 'CancelarNfseResult' );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_resposta.xsd"', '', [rfReplaceAll] );
                   RetWS  := StringReplace( RetWS, ' xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"', '', [rfReplaceAll] );
                   Result :=RetWS;
                  end;
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorISSNet.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd">' +
             RetNfse +
           '</CompNfse>';
end;

function TProvedorISSNet.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
