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

unit ACBrProvedorTecnos;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorTecnos }

 TProvedorTecnos = class(TProvedorClass)
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

{ TProvedorTecnos }

constructor TProvedorTecnos.Create;
begin
 FMetodoRecepcionar := 'mEnvioLoteRPSSincrono';
end;

function TProvedorTecnos.GetConfigCidade(ACodCidade,
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

  ConfigCidade.NameSpaceEnvelope := 'http://tempuri.org/';
  {
 if AAmbiente = 1 then
   ConfigCidade.AssinaRPS  := false
 else
   ConfigCidade.AssinaRPS  := true;
}
  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := False;
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorTecnos.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '20.01';
  ConfigSchema.VersaoDados           := '20.01';
  ConfigSchema.VersaoXML             := '2';
  ConfigSchema.NameSpaceXML          := 'http://www.nfse-tecnos.com.br';
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

function TProvedorTecnos.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
 sHTTPPro: String;
begin
 sHTTPPro := 'https://';

 case ACodCidade of
  4306403: begin //Dois Irmãos/RS
            ConfigURL.HomNomeCidade := 'homologadoi.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'dois.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4306809: begin // Encantado/RS
            ConfigURL.HomNomeCidade := 'homologaencan.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'encantado.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4307609: begin // Estância Velha/RS
            ConfigURL.HomNomeCidade := 'homologaestan.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'estanciavelha.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4307807: begin // Estrela/RS
            ConfigURL.HomNomeCidade := 'homologaest.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'estrela.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4308201: begin // Flores da Cunha/RS
            ConfigURL.HomNomeCidade := 'homologaflo.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'flores.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4310801: begin // Ivoti/RS
            ConfigURL.HomNomeCidade := 'homologaivo.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'ivoti.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4313300: begin // Nova Prata/RS
            ConfigURL.HomNomeCidade := 'homologaprata.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'novaprata.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4314803: begin // Portao/RS
            ConfigURL.HomNomeCidade := 'homologapor.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'portao.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4320404: begin // Serafina Correa/RS
            ConfigURL.HomNomeCidade := 'homologasera.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'serafina.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;
  4322806: begin // Veranopolis/RS
            ConfigURL.HomNomeCidade := 'homologaver.nfse-tecnos.com.br';
            ConfigURL.ProNomeCidade := 'veranopolis.nfse-tecnos.com.br';
            sHTTPPro := 'http://';
           end;

 end;

 ConfigURL.HomRecepcaoLoteRPS    := 'http://' + ConfigURL.HomNomeCidade + ':9091';
 ConfigURL.HomConsultaLoteRPS    := 'http://' + ConfigURL.HomNomeCidade + ':9097';
 ConfigURL.HomConsultaNFSeRPS    := 'http://' + ConfigURL.HomNomeCidade + ':9095';
 ConfigURL.HomConsultaSitLoteRPS := '';
 ConfigURL.HomConsultaNFSe       := 'http://' + ConfigURL.HomNomeCidade + ':9094';
 ConfigURL.HomCancelaNFSe        := 'http://' + ConfigURL.HomNomeCidade + ':9098';
 ConfigURL.HomRecepcaoSincrono   := 'http://' + ConfigURL.HomNomeCidade + ':9091';
  ConfigURL.HomSubstituiNFSe      := '';

 ConfigURL.ProRecepcaoLoteRPS    := sHTTPPro + ConfigURL.ProNomeCidade + ':9091';
 ConfigURL.ProConsultaLoteRPS    := sHTTPPro + ConfigURL.ProNomeCidade + ':9097';
 ConfigURL.ProConsultaNFSeRPS    := sHTTPPro + ConfigURL.ProNomeCidade + ':9095';
 ConfigURL.ProConsultaSitLoteRPS := '';
 ConfigURL.ProConsultaNFSe       := sHTTPPro + ConfigURL.ProNomeCidade + ':9094';
 ConfigURL.ProCancelaNFSe        := sHTTPPro + ConfigURL.ProNomeCidade + ':9098';
 ConfigURL.ProRecepcaoSincrono   := sHTTPPro + ConfigURL.ProNomeCidade + ':9091';
  ConfigURL.ProSubstituiNFSe      := '';

 Result := ConfigURL;
end;

function TProvedorTecnos.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorTecnos.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorTecnos.GetValidarLote: Boolean;
begin
 Result := False;
end;

function TProvedorTecnos.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
var
  xmlns: String;
begin
  xmlns := ' xmlns="http://www.abrasf.org.br/nfse.xsd">';
  
  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + xmlns;
   acConsSit:     Result := '';
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + xmlns;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + xmlns;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseFaixaEnvio' + xmlns;
   acCancelar:    Result := '<' + Prefixo3 + 'CancelarNfseEnvio' + xmlns+
                             '<' + Prefixo3 + 'Pedido>' +
                             '<' + Prefixo4 + 'InfPedidoCancelamento ' + Identificador + '="' + URI + '"' + xmlns + '>';
   acGerar:       Result := '';
   acRecSincrono: Result := '<' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio' + xmlns;

   acSubstituir:  Result := '<' + Prefixo3 + 'SubstituirNfseEnvio' + xmlns +
                             '<' + Prefixo3 + 'SubstituicaoNfse>' +
                              '<' + Prefixo3 + 'Pedido>' +
                               '<' + Prefixo4 + 'InfPedidoCancelamento' +
                                  ifThen(Identificador <> '', ' ' + Identificador + '="' + URI + '"', '') + '>';
  end;
end;

function TProvedorTecnos.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho versao="'  + VersaoLayOut + '"' + NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorTecnos.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorTecnos.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acConsSit:     Result := '';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseFaixaEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorTecnos.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<' + FMetodoRecepcionar + ' xmlns="' + URLNS + '">' +
              '<remessa>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</remessa>' +
             '</' + FMetodoRecepcionar + '>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorTecnos.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedorTecnos.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<mConsultaLoteRPS xmlns="' + URLNS + '">' +
              '<remessa>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</remessa>' +
             '</mConsultaLoteRPS>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorTecnos.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<mConsultaNFSePorRPS xmlns="' + URLNS + '">' +
              '<remessa>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</remessa>' +
             '</mConsultaNFSePorRPS>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorTecnos.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
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

function TProvedorTecnos.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<mCancelamentoNFSe xmlns="' + URLNS + '">' +
              '<remessa>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</remessa>' +
             '</mCancelamentoNFSe>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorTecnos.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '';
end;

function TProvedorTecnos.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Body>' +
             '<' + FMetodoRecepcionar + ' xmlns="' + URLNS + '">' +
              '<remessa>' +
                StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
              '</remessa>' +
             '</' + FMetodoRecepcionar + '>' +
            '</S:Body>' +
           '</S:Envelope>';
end;

function TProvedorTecnos.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorTecnos.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://tempuri.org/' + FMetodoRecepcionar;
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://tempuri.org/mConsultaLoteRPS';
   acConsNFSeRps: Result := 'http://tempuri.org/mConsultaNFSePorRPS';
   acConsNFSe:    Result := 'http://tempuri.org/mConsultaNFSePorFaixa';
   acCancelar:    Result := 'http://tempuri.org/mCancelamentoNFSe';
   acGerar:       Result := '';
   acRecSincrono: Result := 'http://tempuri.org/' + FMetodoRecepcionar;
   acSubstituir:  Result := '';
 end;
end;

function TProvedorTecnos.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'mEnvioLoteRPSSincronoResult');
   acConsSit:     Result := SeparaDados( RetornoWS, '');
   acConsLote:    Result := SeparaDados( RetornoWS, 'mConsultaLoteRPSResult');
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'mConsultaNFSePorRPSResult');
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'mConsultaNFSePorFaixaResult');
   acCancelar:    Result := SeparaDados( RetornoWS, 'mCancelamentoNFSeResult');
   acGerar:       Result := RetornoWS;
   acRecSincrono: Result := SeparaDados( RetornoWS, 'mEnvioLoteRPSSincronoResult');
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorTecnos.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.sistema.com.br/Nfse/arquivos/nfse_3.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
end;

function TProvedorTecnos.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
  Result := '';
end;

end.
