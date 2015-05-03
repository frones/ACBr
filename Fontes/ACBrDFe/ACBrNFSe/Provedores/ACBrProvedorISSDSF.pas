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

unit ACBrProvedorISSDSF;

interface

uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar,
  ACBrNFSeConfiguracoes, ACBrNFSeUtil, ACBrUtil, ACBrDFeUtil,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF};

type
  { TACBrProvedorIssDSF }

 TProvedorIssDSF = class(TProvedorClass)
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
   function GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; OverRide;

   function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; OverRide;
   function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; OverRide;

   function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; OverRide;
   function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; OverRide;
  end;

implementation

{ TProvedorIssDSF }

constructor TProvedorIssDSF.Create;
begin
 {----}
end;

function TProvedorIssDSF.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
var
  ConfigCidade: TConfigCidade;
begin
  ConfigCidade.VersaoSoap    := '1.2';
  ConfigCidade.Prefixo2      := '';
  ConfigCidade.Prefixo3      := 'ns1:';
  ConfigCidade.Prefixo4      := 'tipos:';
  ConfigCidade.Identificador := 'Id';
  ConfigCidade.QuebradeLinha := ';';

  if AAmbiente = 1
   then case ACodCidade of
      3509502: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://issdigital.campinas.sp.gov.br/WsNFe2/lote'; // Campinas/SP
      3170206: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://udigital.uberlandia.mg.gov.br/WsNFe2/lote'; // Uberlandia/MG
      1501402: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://www.issdigitalbel.com.br/WsNFe2/lote';      // Belem/PA
      5002704: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://issdigital.pmcg.ms.gov.br/WsNFe2/lote';     // Campo Grande/MS
      3303500: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://www.issmaisfacil.com.br/WsNFe2/lote';       // Nova Iguacu/RJ
      2211001: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://www.issdigitalthe.com.br/WsNFe2/lote';      // Teresina/PI
      2111300: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://www.issdigitalslz.com.br/WsNFe2/lote';      // Sao Luis/MA
      3552205: ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';  // 'http://www.issdigitalsod.com.br/WsNFe2/lote';      // Sorocaba/SP
   end
  else
    ConfigCidade.NameSpaceEnvelope := 'http://localhost:8080/WsNFe2/lote';

  ConfigCidade.AssinaRPS   := False;
  ConfigCidade.AssinaLote  := (AAmbiente = 1);
  ConfigCidade.AssinaGerar := True;

  Result := ConfigCidade;
end;

function TProvedorIssDSF.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
var
 ConfigSchema: TConfigSchema;
begin
  ConfigSchema.VersaoCabecalho := '';
  ConfigSchema.VersaoDados     := '';
  ConfigSchema.VersaoXML       := '1';
  case ACodCidade of                                                      //tentar estes se não der certo 
    3509502: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://issdigital.campinas.sp.gov.br/WsNFe2'; // Campinas/SP
    3170206: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://udigital.uberlandia.mg.gov.br/WsNFe2'; // Uberlandia/MG
    1501402: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://www.issdigitalbel.com.br/WsNFe2';      // Belem/PA
    5002704: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://issdigital.pmcg.ms.gov.br/WsNFe2';     // Campo Grande/MS
    3303500: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://www.issmaisfacil.com.br/WsNFe2';       // Nova Iguacu/RJ
    2211001: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://www.issdigitalthe.com.br/WsNFe2';      // Teresina/PI
    2111300: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://www.issdigitalslz.com.br/WsNFe2';      // Sao Luis/MA
    3552205: ConfigSchema.NameSpaceXML := 'http://localhost:8080/WsNFe2/lote'; //'http://www.issdigitalsod.com.br/WsNFe2';      // Sorocaba/SP
  end;
                                   //tentar este se não der certo http://localhost:8080/WsNFe2/xsd/...
  ConfigSchema.Cabecalho             := '';
  ConfigSchema.ServicoEnviar         := 'ReqEnvioLoteRPS.xsd';
  ConfigSchema.ServicoConSit         := 'ConsultaSeqRps.xsd';      //verificar esta opção esta errado pra campo grande
  ConfigSchema.ServicoConLot         := 'ReqConsultaLote.xsd';
  ConfigSchema.ServicoConRps         := 'ReqConsultaNFSeRPS.xsd';
  ConfigSchema.ServicoConNfse        := 'ReqConsultaNotas.xsd';
  ConfigSchema.ServicoCancelar       := 'ReqCancelamentoNFSe.xsd';
  ConfigSchema.ServicoConSeqRps      := 'consultarSequencialRps.xsd'; // Incluido por Ailton Branco 16/07/2014
  ConfigSchema.ServicoGerar          := '';
  ConfigSchema.ServicoEnviarSincrono := '';
  ConfigSchema.ServicoSubstituir     := '';
  ConfigSchema.DefTipos              := ''; // 'Tipos.xsd';

 Result := ConfigSchema;
end;

function TProvedorIssDSF.GetConfigURL(ACodCidade: Integer): TConfigURL;
var
 ConfigURL: TConfigURL;
begin
  {ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaLoteRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaNFSeRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaSitLoteRPS := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaNFSe       := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomCancelaNFSe        := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  }

  ConfigURL.HomNomeCidade         := '';

  ConfigURL.ProNomeCidade         := '';

   case ACodCidade of
    3509502:
     begin // Campinas/SP
       ConfigURL.HomRecepcaoLoteRPS    := 'http://issdigital.campinas.sp.gov.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://issdigital.campinas.sp.gov.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    3170206: // Uberlandia/MG
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://udigital.uberlandia.mg.gov.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://udigital.uberlandia.mg.gov.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    1501402: // Belem/PA
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://www.issdigitalbel.com.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://www.issdigitalbel.com.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    5002704: // Campo Grande/MS
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://issdigital.pmcg.ms.gov.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://issdigital.pmcg.ms.gov.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    3303500: // Nova Iguacu/RJ
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://www.notamaisfacil.novaiguacu.rj.gov.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://www.notamaisfacil.novaiguacu.rj.gov.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    2211001: // Teresina/PI
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://www.issdigitalthe.com.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://www.issdigitalthe.com.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    2111300: // Sao Luis/MA
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://sistemas.semfaz.saoluis.ma.gov.br/WsNFe2/LoteRps.jws?wsdl';
     end;
    3552205: // Sorocaba/SP
     begin
       ConfigURL.HomRecepcaoLoteRPS    := 'http://www.issdigitalsod.com.br/WsNFe2/LoteRps.jws?wsdl';

       ConfigURL.ProRecepcaoLoteRPS    := 'http://www.issdigitalsod.com.br/WsNFe2/LoteRps.jws?wsdl';
     end;
  end;

  ConfigURL.HomConsultaLoteRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSeRPS    := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSitLoteRPS := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaNFSe       := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomCancelaNFSe        := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomConsultaSeqRPS     := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomGerarNFSe          := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomRecepcaoSincrono   := ConfigURL.HomRecepcaoLoteRPS;
  ConfigURL.HomSubstituiNFSe      := ConfigURL.HomRecepcaoLoteRPS;

  ConfigURL.ProConsultaLoteRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSeRPS    := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSitLoteRPS := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaNFSe       := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProCancelaNFSe        := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProConsultaSeqRPS     := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProGerarNFSe          := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProRecepcaoSincrono   := ConfigURL.ProRecepcaoLoteRPS;
  ConfigURL.ProSubstituiNFSe      := ConfigURL.ProRecepcaoLoteRPS;

  Result := ConfigURL;
end;

function TProvedorIssDSF.GetURI(URI: String): String;
begin
 Result := '';
end;

function TProvedorIssDSF.GetAssinarXML(Acao: TnfseAcao): Boolean;
begin
 case Acao of
   acRecepcionar: Result := True;
   acConsSit:     Result := False;
   acConsLote:    Result := False;
   acConsNFSeRps: Result := True;
   acConsNFSe:    Result := True;
   acCancelar:    Result := True;
   acGerar:       Result := False;
   acConsSecRps:  Result := True;
   acRecSincrono: Result := False;
   acSubstituir:  Result := False;
 end;
end;

function TProvedorIssDSF.GetValidarLote: Boolean;
begin
 Result := True;
end;

function TProvedorIssDSF.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '<' + Prefixo3 + 'ReqEnvioLoteRPS' + NameSpaceDad;
   acConsSit:     Result := '';
   acConsLote:    Result := '<' + Prefixo3 + 'ReqConsultaLote' + NameSpaceDad;
   acConsNFSeRps: Result := '<' + Prefixo3 + 'ReqConsultaNFSeRPS' + NameSpaceDad;
   acConsNFSe:    Result := '<' + Prefixo3 + 'ReqConsultaNotas' + NameSpaceDad;
   acCancelar:    Result := '<' + Prefixo3 + 'ReqCancelamentoNFSe' + NameSpaceDad;
   acGerar:       Result := '<' + Prefixo3 + 'ReqEnvioLoteRPS' + NameSpaceDad;
   acConsSecRps:  Result := '<' + Prefixo3 + 'ConsultaSeqRps' + NameSpaceDad;
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorIssDSF.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
begin
 Result := '';
end;

function TProvedorIssDSF.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
begin
 Result := '';
end;

function TProvedorIssDSF.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := '</' + Prefixo3 + 'ReqEnvioLoteRPS>';
   acConsSit:     Result := '';
   acConsLote:    Result := '</' + Prefixo3 + 'ReqConsultaLote>';
   acConsNFSeRps: Result := '</' + Prefixo3 + 'ReqConsultaNFSeRPS>';
   acConsNFSe:    Result := '</' + Prefixo3 + 'ReqConsultaNotas>';
   acCancelar:    Result := '</' + Prefixo3 + 'ReqCancelamentoNFSe>';
   acGerar:       Result := '</' + Prefixo3 + 'ReqEnvioLoteRPS>';
   acConsSecRps:  Result := '</' + Prefixo3 + 'ConsultaSeqRps>';
   acRecSincrono: Result := '';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorIssDSF.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:enviar soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string">' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</dsf:enviar>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '';
end;

function TProvedorIssDSF.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:consultarLote soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string">' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</dsf:consultarLote>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:consultarNFSeRps soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string"><![CDATA[' + DadosMsg +
                 ']]></mensagemXml>' +
               '</dsf:consultarNFSeRps>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:consultarNota soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string">' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</dsf:consultarNota>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:cancelar soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string">' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</dsf:cancelar>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '';
end;

function TProvedorIssDSF.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
 Result := '';
end;

function TProvedorIssDSF.GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
begin
  //consultar sequencial RPS
 Result := '<?xml version="1.0" encoding="utf-8"?>' +
           '<soapenv:Envelope xmlns:dsf="http://dsfnet.com.br"' +
                           ' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"' +
                           ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                           ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
             '<soapenv:Body>' +
               '<dsf:consultarSequencialRps soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
                 '<mensagemXml xsi:type="xsd:string">' +
                   StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                 '</mensagemXml>' +
               '</dsf:consultarSequencialRps>' +
             '</soapenv:Body>' +
           '</soapenv:Envelope>';
end;

function TProvedorIssDSF.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
begin
 case Acao of
   acRecepcionar: Result := 'enviar';
   acConsSit:     Result := '';
   acConsLote:    Result := 'consultarLote';
   acConsNFSeRps: Result := 'consultarNFSeRps';
   acConsNFSe:    Result := 'consultarNota';
   acCancelar:    Result := 'cancelar';
   acGerar:       Result := '';
   acRecSincrono: Result := 'enviarSincrono';
   acConsSecRps:  Result := 'consultarSequencialRps';
   acSubstituir:  Result := '';
 end;
end;

function TProvedorIssDSF.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
begin
 case Acao of
   acRecepcionar: Result := SeparaDados( RetornoWS, 'ns1:RetornoEnvioLoteRPS', True );
   acConsSit:     Result := RetornoWS;
   acConsLote:    Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaLote', True );
   acConsNFSeRps: Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaNFSeRPS', True );
   acConsNFSe:    Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaNotas', True );
   acCancelar:    Result := SeparaDados( RetornoWS, 'ns1:RetornoCancelamentoNFSe', True );
   acGerar:       Result := RetornoWS;
   acConsSecRps:  Result := SeparaDados( RetornoWS, 'ns1:RetornoConsultaSeqRps', True );
   acRecSincrono: Result := RetornoWS;
   acSubstituir:  Result := RetornoWS;
 end;
end;

function TProvedorIssDSF.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
begin
 Result := '<?xml version="1.0" encoding="UTF-8"?>' +
           '<CompNfse xmlns:ns4="http://www.e-governeapps2.com.br/nfse.xsd">' +
             RetNFSe +
           '</CompNfse>';
end;

function TProvedorIssDSF.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
begin
 Result := '';
end;

end.
