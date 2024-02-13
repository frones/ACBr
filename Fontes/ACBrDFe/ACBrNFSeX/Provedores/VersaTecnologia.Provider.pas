{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit VersaTecnologia.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceVersaTecnologia200 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetURL: string;
    function GetNameSpace: string;
    function GetSoapAction: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property URL: string read GetURL;
    property NameSpace: string read GetNameSpace;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderVersaTecnologia200 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

  TACBrNFSeProviderVersaTecnologia201 = class (TACBrNFSeProviderVersaTecnologia200)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
  public
    function GetSchemaPath: string; override;
  end;

  TACBrNFSeProviderVersaTecnologia202 = class (TACBrNFSeProviderVersaTecnologia200)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
  public
    function GetSchemaPath: string; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais,
  VersaTecnologia.GravarXml, VersaTecnologia.LerXml;

{ TACBrNFSeProviderVersaTecnologia200 }

procedure TACBrNFSeProviderVersaTecnologia200.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;
end;

function TACBrNFSeProviderVersaTecnologia200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_VersaTecnologia200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderVersaTecnologia200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_VersaTecnologia200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderVersaTecnologia200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceVersaTecnologia200.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderVersaTecnologia200.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml, xNameSpace1, xNameSpace2: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  xXml := Response.ArquivoEnvio;

  xNameSpace1 := 'xmlns="' + ConfigWebServices.Producao.XMLNameSpace + '"';

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    xNameSpace2 := 'xmlns="' + ConfigWebServices.Producao.NameSpace + '"'
  else
    xNameSpace2 := 'xmlns="' + ConfigWebServices.Homologacao.NameSpace + '"';

  if ConfigWebServices.VersaoDados = '2.01' then
    xNameSpace2 := StringReplace(xNameSpace2, '/webservice', '', []);

  case aMetodo of
    tmRecepcionar:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<EnviarLoteRpsEnvio ' + xNameSpace1 + '>',
          '</EnviarLoteRpsEnvio>', False);

        xXml := '<EnviarLoteRpsEnvio ' + xNameSpace2 + '>' + xXml + '</EnviarLoteRpsEnvio>';
      end;

    tmConsultarLote:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarLoteRpsEnvio ' + xNameSpace1 + '>',
          '</ConsultarLoteRpsEnvio>', False);

        xXml := '<ConsultarLoteRpsEnvio ' + xNameSpace2 + '>' + xXml + '</ConsultarLoteRpsEnvio>';
      end;

    tmConsultarNFSePorRps:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseRpsEnvio ' + xNameSpace1 + '>',
          '</ConsultarNfseRpsEnvio>', False);

        xXml := '<ConsultarNfseRpsEnvio ' + xNameSpace2 + '>' + xXml + '</ConsultarNfseRpsEnvio>';
      end;

    tmConsultarNFSePorFaixa:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseFaixaEnvio ' + xNameSpace1 + '>',
          '</ConsultarNfseFaixaEnvio>', False);

        xXml := '<ConsultarNfseFaixaEnvio ' + xNameSpace2 + '>' + xXml + '</ConsultarNfseFaixaEnvio>';
      end;

    tmConsultarNFSeServicoPrestado:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseServicoPrestadoEnvio ' + xNameSpace1 + '>',
          '</ConsultarNfseServicoPrestadoEnvio>', False);

        xXml := '<ConsultarNfseServicoPrestadoEnvio ' + xNameSpace2 + '>' + xXml + '</ConsultarNfseServicoPrestadoEnvio>';
      end;

    tmConsultarNFSeServicoTomado:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseServicoTomadoEnvio ' + xNameSpace1 + '>',
          '</ConsultarNfseServicoTomadoEnvio>', False);

        xXml := '<ConsultarNfseServicoTomadoEnvio ' + xNameSpace2 + '>' + xXml + '</ConsultarNfseServicoTomadoEnvio>';
      end;

    tmCancelarNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<CancelarNfseEnvio ' + xNameSpace1 + '>',
          '</CancelarNfseEnvio>', False);

        xXml := '<CancelarNfseEnvio ' + xNameSpace2 + '>' + xXml + '</CancelarNfseEnvio>';
      end;

    tmGerar:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<GerarNfseEnvio ' + xNameSpace1 + '>',
          '</GerarNfseEnvio>', False);

        xXml := '<GerarNfseEnvio '+ xNameSpace2 + '>' + xXml + '</GerarNfseEnvio>';
      end;

     tmRecepcionarSincrono:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<EnviarLoteRpsSincronoEnvio ' + xNameSpace1 + '>',
          '</EnviarLoteRpsSincronoEnvio>', False);

        xXml := '<EnviarLoteRpsSincronoEnvio ' + xNameSpace2 + '>' + xXml + '</EnviarLoteRpsSincronoEnvio>';
      end;

    tmSubstituirNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<SubstituirNfseEnvio ' + xNameSpace1 + '>',
          '</SubstituirNfseEnvio>', False);

        xXml := '<SubstituirNfseEnvio ' + xNameSpace2 + '>' + xXml + '</SubstituirNfseEnvio>';
      end;
  else
    Response.ArquivoEnvio := xXml;
  end;

  Response.ArquivoEnvio := xXml;
end;

{ TACBrNFSeXWebserviceVersaTecnologia200 }

function TACBrNFSeXWebserviceVersaTecnologia200.GetURL: string;
var
  xURL: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    xURL := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params.ValorParametro('URLProducao')
  else
    xURL := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params.ValorParametro('URLHomologacao');

  if TACBrNFSeX(FPDFeOwner).Configuracoes.Geral.Versao = ve201 then
    xURL := xURL + '/webservice'
  else
    xURL := xURL + '/webservices/2.02';

  Result := xURL;
end;

function TACBrNFSeXWebserviceVersaTecnologia200.GetNameSpace: string;
begin
  Result := 'xmlns:ns1="http://' + URL + '/nfse.wsdl"';
end;

function TACBrNFSeXWebserviceVersaTecnologia200.GetSoapAction: string;
begin
  Result := 'http://' + URL + '/servicos#';
end;

function TACBrNFSeXWebserviceVersaTecnologia200.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:RecepcionarLoteRpsRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:GerarNfseRequest>';

  Result := Executar(SoapAction + 'GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:ConsultarLoteRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:ConsultarNfsePorFaixaRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:ConsultarNfsePorRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:ConsultarNfseServicoTomadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:CancelarNfseRequest>';

  Result := Executar(SoapAction + 'CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ns1:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ns1:SubstituirNfseRequest>';

  Result := Executar(SoapAction + 'SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceVersaTecnologia200.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

{ TACBrNFSeProviderVersaTecnologia201 }

procedure TACBrNFSeProviderVersaTecnologia201.Configuracao;
var
  FpURL: string;
begin
  inherited Configuracao;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    FpURL := ConfigWebServices.Producao.NameSpace
  else
    FpURL := ConfigWebServices.Homologacao.NameSpace;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '201';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho(FpURL);

  SetXmlNameSpace(ConfigWebServices.Producao.XMLNameSpace);

  SetNomeXSD('nfse_v201.xsd');
end;

function TACBrNFSeProviderVersaTecnologia201.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_VersaTecnologia201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderVersaTecnologia201.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_VersaTecnologia201.Create(Self);
  Result.NFSe := ANFSe;
end;

procedure TACBrNFSeProviderVersaTecnologia201.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                           '<' + Prefixo2 + 'Pedido>' +
                             '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + '>' +
                               '<' + Prefixo2 + 'IdentificacaoNfse>' +
                                 '<' + Prefixo2 + 'Numero>' +
                                    InfoCanc.NumeroNFSe +
                                 '</' + Prefixo2 + 'Numero>' +
                                 Serie +
                                 '<' + Prefixo2 + 'CpfCnpj>' +
                                   GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                                 '</' + Prefixo2 + 'CpfCnpj>' +
                                 GetInscMunic(Emitente.InscMun, Prefixo2) +
                                 '<' + Prefixo2 + 'CodigoMunicipio>' +
                                    IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                                 '</' + Prefixo2 + 'CodigoMunicipio>' +
                                 CodigoVerificacao +
                               '</' + Prefixo2 + 'IdentificacaoNfse>' +
                               '<' + Prefixo2 + 'CodigoCancelamento>' +
                                  InfoCanc.CodCancelamento +
                               '</' + Prefixo2 + 'CodigoCancelamento>' +
                               Motivo +
                             '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                           '</' + Prefixo2 + 'Pedido>' +
                         '</' + Prefixo + 'CancelarNfseEnvio>';
  end;
end;

function TACBrNFSeProviderVersaTecnologia201.GetSchemaPath: string;
begin
  Result := inherited GetSchemaPath;

  Result := Result + ConfigGeral.CodIBGE + '\';
end;

{ TACBrNFSeProviderVersaTecnologia202 }

procedure TACBrNFSeProviderVersaTecnologia202.Configuracao;
var
  FpURL: string;
begin
  inherited Configuracao;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    FpURL := ConfigWebServices.Producao.NameSpace
  else
    FpURL := ConfigWebServices.Homologacao.NameSpace;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '202';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho(FpURL);

  SetXmlNameSpace(ConfigWebServices.Producao.XMLNameSpace);

  SetNomeXSD('nfse_v202.xsd');
end;

function TACBrNFSeProviderVersaTecnologia202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_VersaTecnologia202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderVersaTecnologia202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_VersaTecnologia202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderVersaTecnologia202.GetSchemaPath: string;
begin
  Result := inherited GetSchemaPath;

  Result := Result + ConfigGeral.CodIBGE + '\';
end;

end.
