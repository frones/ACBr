{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

Unit ACBrProvedorSMARAPD;

Interface

Uses
  Classes, SysUtils, strUtils,
  pnfsConversao, pcnAuxiliar, // synacode,
  ACBrNFSeConfiguracoes, ACBrUtil, ACBrDFeUtil,
{$IFDEF COMPILER6_UP}DateUtils{$ELSE}ACBrD5, FileCtrl{$ENDIF};

Type
  { TACBrProvedorIssDSF }

  TProvedorSMARAPD = Class(TProvedorClass)
  Protected
    { protected }
    FACodCidade: Integer;
  Private
    { private }

  Public
    { public }
    Constructor Create;

    Function GetConfigCidade(ACodCidade, AAmbiente: Integer): TConfigCidade; Override;
    Function GetConfigSchema(ACodCidade: Integer): TConfigSchema; Override;
    Function GetConfigURL(ACodCidade: Integer): TConfigURL; Override;
    Function GetURI(URI: String): String; Override;
    Function GetAssinarXML(Acao: TnfseAcao): Boolean; Override;
    Function GetValidarLote: Boolean; Override;

    Function Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4, NameSpaceDad, Identificador, URI: String): AnsiString; Override;
    Function Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados, NameSpaceCab: String; ACodCidade: Integer): AnsiString; Override;
    Function Gera_DadosSenha(CNPJ, Senha: String): AnsiString; Overload; Override;
    Function Gera_DadosSenha(CNPJ, Senha: String; CodigoMunicipio: String): AnsiString; Overload; Override;
    Function Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString; Override;

    Function GeraEnvelopeRecepcionarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeConsultarSituacaoLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Overload; Override;
    Function GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha, XmlRecibo : AnsiString): AnsiString; Overload;Override;
    Function GeraEnvelopeConsultarNFSeporRPS(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeGerarNFSe(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeRecepcionarSincrono(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;
    Function GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString; Override;

    Function GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String; Override;
    Function GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString; Override;

    Function GeraRetornoNFSe(Prefixo: String; RetNFSe: AnsiString; NomeCidade: String): AnsiString; Override;
    Function GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String; Override;
  End;

Implementation

{ TProvedorSMARAPD }

Constructor TProvedorSMARAPD.Create;
Begin
  {----}
End;

Function TProvedorSMARAPD.GetConfigCidade(ACodCidade,
  AAmbiente: Integer): TConfigCidade;
Var
  ConfigCidade: TConfigCidade;
Begin
  ConfigCidade.VersaoSoap := '';
  ConfigCidade.Prefixo2 := '';
  ConfigCidade.Prefixo3 := '';
  ConfigCidade.Prefixo4 := '';
  ConfigCidade.Identificador := '';
  ConfigCidade.QuebradeLinha := '';

  if AAmbiente = 1 then
    case ACodCidade of
      3205002: ConfigCidade.NameSpaceEnvelope := 'http://apps.serra.es.gov.br:8080/tbw/services/WSEntrada';
    end
  else
    ConfigCidade.NameSpaceEnvelope := '';

  ConfigCidade.AssinaRPS   := True;
  ConfigCidade.AssinaLote  := (AAmbiente = 1);
  ConfigCidade.AssinaGerar := True;

  FACodCidade := ACodCidade;

  Result := ConfigCidade;
End;

Function TProvedorSMARAPD.GetConfigSchema(ACodCidade: Integer): TConfigSchema;
Var
  ConfigSchema: TConfigSchema;
Begin
  ConfigSchema.VersaoCabecalho := '';
  ConfigSchema.VersaoDados := '';
  ConfigSchema.VersaoXML := '1';
  Case ACodCidade Of //tentar estes se não der certo
    3205002: ConfigSchema.NameSpaceXML := 'http://apps.serra.es.gov.br:8080/tbw/services/WSEntrada';
  End;
  //tentar este se não der certo http://localhost:8080/WsNFe2/xsd/...
  ConfigSchema.Cabecalho := '';
  ConfigSchema.ServicoGerar := 'WSEntradaNfd.xsd';
  ConfigSchema.ServicoEnviar := 'WSEntradaNfd.xsd';
  ConfigSchema.ServicoConSit := ''; //verificar esta opção esta errado pra campo grande
  ConfigSchema.ServicoConLot := '';
  ConfigSchema.ServicoConRps := '';
  ConfigSchema.ServicoConNfse := '';
  ConfigSchema.ServicoCancelar := '';
  ConfigSchema.ServicoConSeqRps := ''; // Incluido por Ailton Branco 16/07/2014
  ConfigSchema.DefTipos := ''; // 'Tipos.xsd';

  Result := ConfigSchema;
End;

Function TProvedorSMARAPD.GetConfigURL(ACodCidade: Integer): TConfigURL;
Var
  ConfigURL: TConfigURL;
Begin
  {ConfigURL.HomNomeCidade         := '';
  ConfigURL.HomRecepcaoLoteRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaLoteRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaNFSeRPS    := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaSitLoteRPS := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomConsultaNFSe       := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  ConfigURL.HomCancelaNFSe        := 'http://treinamento.dsfweb.com.br/WsNFe2/LoteRps.jws?wsdl';
  }

  ConfigURL.HomNomeCidade := '';
  ConfigURL.HomRecepcaoLoteRPS := '';
  ConfigURL.HomConsultaLoteRPS := '';
  ConfigURL.HomConsultaNFSeRPS := '';
  ConfigURL.HomConsultaSitLoteRPS := '';
  ConfigURL.HomConsultaNFSe := '';
  ConfigURL.HomCancelaNFSe := '';

  ConfigURL.ProNomeCidade := '';

  Case ACodCidade Of
    3205002:
      Begin // SERRA/ES
        ConfigURL.HomRecepcaoLoteRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSEntrada?wsdl';
        ConfigURL.HomGerarNFSe := 'http://apps.serra.es.gov.br:8080/tbw/services/WSEntrada?wsdl';
        ConfigURL.ProGerarNFSe := 'http://apps.serra.es.gov.br:8080/tbw/services/WSEntrada?wsdl';
        ConfigURL.HomConsultaLoteRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.HomConsultaNFSeRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.HomConsultaSitLoteRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.HomConsultaNFSe := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.HomCancelaNFSe := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.HomConsultaSeqRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';

        ConfigURL.ProRecepcaoLoteRPS := '';
        ConfigURL.ProConsultaLoteRPS := 'http://apps.serra.es.gov.br:8080/tbw/services/WSSaida?wsdl';
        ConfigURL.ProConsultaNFSeRPS := '';
        ConfigURL.ProConsultaSitLoteRPS := '';
        ConfigURL.ProConsultaNFSe := '';
        ConfigURL.ProCancelaNFSe := '';
        ConfigURL.ProConsultaSeqRPS := '';
      End;
  End;
  Result := ConfigURL;
End;

Function TProvedorSMARAPD.GetURI(URI: String): String;
Begin
  Result := '';
End;

Function TProvedorSMARAPD.GetAssinarXML(Acao: TnfseAcao): Boolean;
Begin
  Case Acao Of
    acRecepcionar: Result := True;
    acConsSit: Result := False;
    acConsLote: Result := False;
    acConsNFSeRps: Result := True;
    acConsNFSe: Result := True;
    acCancelar: Result := True;
    acGerar: Result := False;
    acConsSecRps: Result := True;
  Else Result := False;
  End;
End;

Function TProvedorSMARAPD.GetValidarLote: Boolean;
Begin
  Result := False;
End;

Function TProvedorSMARAPD.Gera_TagI(Acao: TnfseAcao; Prefixo3, Prefixo4,
  NameSpaceDad, Identificador, URI: String): AnsiString;
Begin
  Case Acao Of
    acRecepcionar: Result := '<' + Prefixo3 + 'ReqEnvioLoteRPS' + NameSpaceDad;
    acConsSit: Result := '';
    acConsLote: Result := '';
    acConsNFSeRps: Result := '<' + Prefixo3 + 'ReqConsultaNFSeRPS' + NameSpaceDad;
    acConsNFSe: Result := '<' + Prefixo3 + 'ReqConsultaNotas' + NameSpaceDad;
    acCancelar: Result := '<' + Prefixo3 + 'ReqCancelamentoNFSe' + NameSpaceDad;
    acGerar: Result := '';
    acConsSecRps: Result := '<' + Prefixo3 + 'ConsultaSeqRps' + NameSpaceDad;
  End;
End;

Function TProvedorSMARAPD.Gera_CabMsg(Prefixo2, VersaoLayOut, VersaoDados,
  NameSpaceCab: String; ACodCidade: Integer): AnsiString;
Begin
  Result := '';
End;

Function TProvedorSMARAPD.Gera_DadosSenha(CNPJ, Senha,
  CodigoMunicipio: String): AnsiString;
var
  Hash: String;
Begin
  Hash := EncodeBase64( SHA1( Senha ) );
  If CodigoMunicipio = '3205002' Then
    Result := '<cpfUsuario>' + CNPJ + '</cpfUsuario><hashSenha>' + Hash +
              '</hashSenha><codigoMunicipio>3</codigoMunicipio>'
  Else
    Raise Exception.Create('Somente a cidade da serra foi implementada, para implementar outra cidade troque a tag "codigoMunicipio"');
End;

Function TProvedorSMARAPD.Gera_DadosSenha(CNPJ, Senha: String): AnsiString;
Begin
  Raise Exception.Create('Para esse provedor Utilize o método "Gera_DadosSenha(CNPJ, Senha, CodigoMunicipio: String)"');
  Result := '';
End;

Function TProvedorSMARAPD.Gera_TagF(Acao: TnfseAcao; Prefixo3: String): AnsiString;
Begin
  Case Acao Of
    acRecepcionar: Result := '</' + Prefixo3 + 'ReqEnvioLoteRPS>';
    acConsSit: Result := '';
    acConsLote: Result := '';
    acConsNFSeRps: Result := '</' + Prefixo3 + 'ReqConsultaNFSeRPS>';
    acConsNFSe: Result := '</' + Prefixo3 + 'ReqConsultaNotas>';
    acCancelar: Result := '</' + Prefixo3 + 'ReqCancelamentoNFSe>';
    acGerar: Result := '';
    acConsSecRps: Result := '</' + Prefixo3 + 'ConsultaSeqRps>';
  End;
End;

Function TProvedorSMARAPD.GeraEnvelopeRecepcionarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

Function TProvedorSMARAPD.GeraEnvelopeConsultarSituacaoLoteRPS(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
  result := '';
End;

Function TProvedorSMARAPD.GeraEnvelopeConsultarLoteRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

Function TProvedorSMARAPD.GeraEnvelopeConsultarNFSeporRPS(URLNS: String;
  CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

function TProvedorSMARAPD.GeraEnvelopeConsultarLoteRPS(URLNS: String; CabMsg,
  DadosMsg, DadosSenha, XmlRecibo: AnsiString): AnsiString;
var
  XmlExtraido : string;
begin
  XmlRecibo := Trim(Copy(XmlRecibo,Pos('<?xml ',XmlRecibo),Pos('</nfd>',XmlRecibo)-Pos('<?xml ',XmlRecibo)+6));
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">' +
    '<soapenv:Body>' +
    '<sil:nfdSaida xmlns:sil="http://webservices.sil.com/">' +
    DadosSenha +
    '<recibo>' +
    StringReplace(StringReplace(
        XmlRecibo, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
    '</recibo>' +
    '</sil:nfdSaida></soapenv:Body></soapenv:Envelope>'

end;

Function TProvedorSMARAPD.GeraEnvelopeConsultarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

Function TProvedorSMARAPD.GeraEnvelopeCancelarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

Function TProvedorSMARAPD.GeraEnvelopeGerarNFSe(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">' +
    '<soapenv:Body>' +
    '<sil:nfdEntrada xmlns:sil="http://webservices.sil.com/">' +
    DadosSenha +
    '<nfd>' +
    StringReplace(StringReplace('<?xml version="1.0" encoding="UTF-8"?>'+
        DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
    '</nfd>' +
    '</sil:nfdEntrada></soapenv:Body></soapenv:Envelope>'
End;

Function TProvedorSMARAPD.GeraEnvelopeRecepcionarSincrono(
  URLNS: String; CabMsg, DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
  Result := '';
End;

Function TProvedorSMARAPD.GeraEnvelopeConsultarSequencialRps(URLNS: String; CabMsg,
  DadosMsg, DadosSenha: AnsiString): AnsiString;
Begin
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
End;

Function TProvedorSMARAPD.GetSoapAction(Acao: TnfseAcao; NomeCidade: String): String;
Begin
  Case Acao Of
    acRecepcionar: Result := 'enviar';
    acConsSit: Result := '';
    acConsLote: Result := 'NfdSaida';
    acConsNFSeRps: Result := 'consultarNFSeRps';
    acConsNFSe: Result := 'consultarNota';
    acCancelar: Result := 'cancelar';
    acGerar: Result := 'NfdEntrada';
    acRecSincrono: Result := 'enviarSincrono';
    acConsSecRps: Result := 'consultarSequencialRps';
  End;
End;

Function TProvedorSMARAPD.GetRetornoWS(Acao: TnfseAcao; RetornoWS: AnsiString): AnsiString;
Begin
  Case Acao Of
    acRecepcionar: Result := SeparaDados(RetornoWS, 'ns1:RetornoEnvioLoteRPS', True);
    acConsSit: Result := '';
    acConsLote: Result := SeparaDados(RetornoWS, 'ns2:nfdSaidaResponse', True);
    acConsNFSeRps: Result := SeparaDados(RetornoWS, 'ns1:RetornoConsultaNFSeRPS', True);
    acConsNFSe: Result := SeparaDados(RetornoWS, 'ns1:RetornoConsultaNotas', True);
    acCancelar: Result := SeparaDados(RetornoWS, 'ns2:nfdEntradaResponse', True);
    acGerar: Result := SeparaDados(RetornoWS, 'ns2:nfdEntradaResponse', True);
    acConsSecRps: Result := SeparaDados(RetornoWS, 'ns1:RetornoConsultaSeqRps', True);
  End;
End;

Function TProvedorSMARAPD.GeraRetornoNFSe(Prefixo: String;
  RetNFSe: AnsiString; NomeCidade: String): AnsiString;
Begin
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
    '<CompNfse xmlns:ns4="http://www.e-governeapps2.com.br/nfse.xsd">' +
    RetNFSe +
    '</CompNfse>';
End;

Function TProvedorSMARAPD.GetLinkNFSe(ACodMunicipio, ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String; AAmbiente: Integer): String;
Begin
  Result := 'http://apps.serra.es.gov.br:8080/tbw/services/WSUtil?wsdl';
End;

End.

