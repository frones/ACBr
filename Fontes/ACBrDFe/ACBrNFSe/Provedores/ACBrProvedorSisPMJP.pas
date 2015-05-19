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

unit ACBrProvedorSisPMJP;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorISSe }

 TProvedorSisPMJP = class(TProvedorClass)
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

{ TProvedorSisPMJP }

constructor TProvedorSisPMJP.Create;
begin
 {----}
end;

function TProvedorSisPMJP.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.1';
  ConfigCidade.Prefixo2      := ''; //nfse: Alterado por Carlos Henrique
  ConfigCidade.Prefixo3      := ''; //nfse: Alterado por Carlos Henrique
  ConfigCidade.Prefixo4      := ''; //nfse: Alterado por Carlos Henrique
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1 then
    ConfigCidade.NameSpaceEnvelope := 'http://nfse.abrasf.org.br'
  else
    ConfigCidade.NameSpaceEnvelope := 'http://nfse.abrasf.org.br';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := True; //False; Alterado por Carlos Henrique
  ConfigCidade.AssinaGerar := False;

  Result := ConfigCidade;
end;

function TProvedorSisPMJP.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
  ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho       := '2.02';
  ConfigSchema.VersaoDados           := '2.02';
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

function TProvedorSisPMJP.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
  ConfigURL: TConfigURL;
begin
  ConfigURL.HomNomeCidade         := 'joaopessoa';
  ConfigURL.HomRecepcaoLoteRPS    := 'https://nfsehomolog.joaopessoa.pb.gov.br:8443/sispmjp/NfseWSService';
  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProNomeCidade         := 'joaopessoa';
  ConfigURL.ProRecepcaoLoteRPS    := 'https://sispmjp.joaopessoa.pb.gov.br:8443/sispmjp/NfseWSService';
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

function TProvedorSisPMJP.GetURI(URI: String): String;
begin
 Result := URI;
end;

function TProvedorSisPMJP.GetAssinarXML(Acao: TnfseAcao): Boolean;
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

function TProvedorSisPMJP.GetValidarLote: Boolean;
begin
 Result := True; //False Alterado por Carlos Henrique
end;

function TProvedorSisPMJP.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
var
  xmlns: String;
begin
(*
 xmlns := ' xmlns:ds="http://www.w3.org/2000/09/xmldsig#"' +
          ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
          ' xsi:schemaLocation="http://www.abrasf.org.br/nfse.xsd nfse_v2.01.xsd"' +
          NameSpaceDad;
*)
  xmlns := NameSpaceDad;

  case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'EnviarLoteRpsEnvio' + xmlns;
   acConsSit:     Result := '<' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + xmlns;
   acConsLote:    Result := '<' + Prefixo3 + 'ConsultarLoteRpsEnvio' + xmlns;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ConsultarNfseRpsEnvio' + xmlns;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio' + xmlns;
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

function TProvedorSisPMJP.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '<' + Prefixo2 + 'cabecalho' +
            ' versao="'  + VersaoLayOut + '"' +
//            ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
//            ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
            NameSpaceCab +
            '<versaoDados>' + VersaoDados + '</versaoDados>'+
           '</' + Prefixo2 + 'cabecalho>';
end;

function TProvedorSisPMJP.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorSisPMJP.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
  case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'EnviarLoteRpsEnvio>';
   acConsSit:     Result := '</' + Prefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';
   acConsLote:    Result := '</' + Prefixo3 + 'ConsultarLoteRpsEnvio>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ConsultarNfseRpsEnvio>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ConsultarNfseServicoPrestadoEnvio>';
   acCancelar:    Result := '</' + Prefixo3 + 'Pedido>' +
                            '</' + Prefixo3 + 'CancelarNfseEnvio>';
   acGerar:       Result := '</' + Prefixo3 + 'GerarNfseEnvio>';
   acRecSincrono: Result := '</' + Prefixo3 + 'EnviarLoteRpsSincronoEnvio>';
   acSubstituir:  Result := '</' + Prefixo3 + 'SubstituicaoNfse>' +
                            '</' + Prefixo3 + 'SubstituirNfseEnvio>';
  end;
end;

function TProvedorSisPMJP.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:RecepcionarLoteRpsRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:RecepcionarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<RecepcionarLoteRpsRequest>'+
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</RecepcionarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
 raise Exception.Create( 'Opção não implementada para este provedor.' );
end;

function TProvedorSisPMJP.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:ConsultarLoteRpsRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<ConsultarLoteRpsRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</ConsultarLoteRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:ConsultarNfsePorRpsRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarNfsePorRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<ConsultarNfsePorRpsRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</ConsultarNfsePorRpsRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:ConsultarNfsePorFaixaRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:ConsultarNfsePorFaixaRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<ConsultarNfsePorFaixaRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</ConsultarNfsePorFaixaRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:CancelarNfseRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:CancelarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<CancelarNfseRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</CancelarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:GerarNfseRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:GerarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<GerarNfseRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</GerarNfseRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeRecepcionarSincrono(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 CabMsg   := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
 DadosMsg := StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>' +DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/" ' +
                       'xmlns:nfse="http://nfse.abrasf.org.br" ' +
                       'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                       'xmlns:xsd="http://www.w3.org/2001/XMLSchema">' +
            '<S:Header />' +
            '<S:Body>' +
             '<nfse:RecepcionarLoteRpsSincronoRequest xmlns="http://nfse.abrasf.org.br">' +
              '<nfseCabecMsg xmlns="">' +
                CabMsg +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg xmlns="">' +
                DadosMsg +
              '</nfseDadosMsg>' +
             '</nfse:RecepcionarLoteRpsSincronoRequest>' +
            '</S:Body>' +
           '</S:Envelope>';

{
 result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/"' +
                      ' xmlns:nfse="http://nfse.abrasf.org.br">' +
            '<S:Body>' +
             '<RecepcionarLoteRpsSincronoRequest>' +
              '<nfseCabecMsg>' +
               '<![CDATA[' + CabMsg + ']]>' +
              '</nfseCabecMsg>' +
              '<nfseDadosMsg>' +
               '<![CDATA[' + DadosMsg + ']]>' +
              '</nfseDadosMsg>' +
             '</RecepcionarLoteRpsSincronoRequest>' +
            '</S:Body>' +
           '</S:Envelope>';
}
end;

function TProvedorSisPMJP.GeraEnvelopeSubstituirNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  Result := '';
end;

function TProvedorSisPMJP.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'http://nfse.abrasf.org.br/RecepcionarLoteRps';
   acConsSit:     Result := '';
   acConsLote:    Result := 'http://nfse.abrasf.org.br/ConsultarLoteRps';
   acConsNFSeRps: Result := 'http://nfse.abrasf.org.br/ConsultarNfsePorRps';
   acConsNFSe:    Result := 'http://nfse.abrasf.org.br/ConsultarNfsePorFaixa';
   acCancelar:    Result := 'http://nfse.abrasf.org.br/CancelarNfse';
   acGerar:       Result := 'http://nfse.abrasf.org.br/GerarNfse';
   acRecSincrono: Result := 'http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorSisPMJP.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'ws:RecepcionarLoteRpsResponse' );
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'ws:ConsultarLoteRpsResponse' );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ws:ConsultarNfsePorRpsResponse' );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ConsultarNfsePorFaixaResponse' );
   acCancelar:    Result := SeparaDados( RetornoWS, 'ws:CancelarNfseResponse' );
   acGerar:       Result := SeparaDados( RetornoWS, 'ws:GerarNfseResponse' );
   acRecSincrono: Result := SeparaDados( RetornoWS, 'ws:RecepcionarLoteRpsSincronoResponse' );
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorSisPMJP.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/ABRASF/arquivos/">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';

{
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<' + Prefixo + 'CompNfse xmlns="http://www.abrasf.org.br/nfse.xsd">' +
             RetNfse +
           '</' + Prefixo + 'CompNfse>';
}
end;

function TProvedorSisPMJP.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
