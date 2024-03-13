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

unit AEG.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceAEG202 = class(TACBrNFSeXWebserviceSoap11)
  private

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;
  end;

  TACBrNFSeProviderAEG202 = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Resultado'); override;

  end;

implementation

uses
  ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais,
  AEG.GravarXml, AEG.LerXml;

{ TACBrNFSeXWebserviceAEG202 }

function TACBrNFSeXWebserviceAEG202.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:RecepcionarLoteRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:RecepcionarLoteRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#RecepcionarLoteRps',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:RecepcionarLoteRpsSincrono xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:RecepcionarLoteRpsSincrono>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#RecepcionarLoteRpsSincrono',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarLoteRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarLoteRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarLoteRps',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfsePorRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfsePorRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfsePorRps',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfsePorFaixa xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfsePorFaixa>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfsePorFaixa',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfseServicoPrestado xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfseServicoPrestado>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfseServicoPrestado',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfseServicoTomado xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfseServicoTomado>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfseServicoTomado',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.GerarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:GerarNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:GerarNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#GerarNfse',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:CancelarNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:CancelarNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#CancelarNfse',
                     Request, ['return', 'DocumentElement'], []);
end;

function TACBrNFSeXWebserviceAEG202.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:SubstituirNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:SubstituirNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#SubstituirNfse',
                     Request, ['return', 'DocumentElement'], []);
end;

{ TACBrNFSeProviderAEG202 }

procedure TACBrNFSeProviderAEG202.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  ConfigGeral.Autenticacao.RequerCertificado := False;
  ConfigGeral.Autenticacao.RequerChaveAcesso := True;
  ConfigGeral.Autenticacao.RequerChaveAutorizacao := True;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.GerarNSLoteRps := True;
end;

function TACBrNFSeProviderAEG202.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_AEG202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAEG202.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_AEG202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAEG202.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAEG202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderAEG202.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANodeArray := RootNode.Childrens.FindAllAnyNs('Resultado');

  if Assigned(ANodeArray) then
  begin
    for I := Low(ANodeArray) to High(ANodeArray) do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ResultadoCodigo'), tcStr);
      AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ResultadoErro'), tcStr);
      AErro.Correcao := '';
    end;
  end;
end;

procedure TACBrNFSeProviderAEG202.ValidarSchema(Response: TNFSeWebserviceResponse;
  aMetodo: TMetodo);
var
  Seguranca: string;
  aXml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  with TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente do
  begin
    Seguranca := '<Seguranca>' +
                   '<ChaveAcesso>' + WSChaveAcesso + '</ChaveAcesso>' +
                   '<ChaveAutorizacao>' + WSChaveAutoriz + '</ChaveAutorizacao>' +
                 '</Seguranca>';
  end;

  aXml := Response.ArquivoEnvio;

  case aMetodo of
    tmRecepcionar:
      aXml := '<EnviarLoteRpsEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'EnviarLoteRpsEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>EnviarLoteRpsEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</EnviarLoteRpsEnvio>';

    tmConsultarLote:
      aXml := '<ConsultarLoteRpsEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'ConsultarLoteRpsEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>ConsultarLoteRpsEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</ConsultarLoteRpsEnvio>';

    tmConsultarNFSePorRps:
      aXml := '<ConsultarNfseRpsEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'ConsultarNfseRpsEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>ConsultarNfseRpsEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</ConsultarNfseRpsEnvio>';

    tmConsultarNFSePorFaixa:
      aXml := '<ConsultarNfsePorFaixaEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'ConsultarNfsePorFaixaEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>ConsultarNfsePorFaixaEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</ConsultarNfsePorFaixaEnvio>';

    tmConsultarNFSeServicoPrestado:
      aXml := '<ConsultarNfseServicoPrestadoEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'ConsultarNfseServicoPrestadoEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>ConsultarNfseServicoPrestadoEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</ConsultarNfseServicoPrestadoEnvio>';

    tmConsultarNFSeServicoTomado:
      aXml := '<ConsultarNfseServicoTomadoEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'ConsultarNfseServicoTomadoEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>ConsultarNfseServicoTomadoEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</ConsultarNfseServicoTomadoEnvio>';

    tmCancelarNFSe:
      aXml := '<CancelarNfseEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'CancelarNfseEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>CancelarNfseEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</CancelarNfseEnvio>';

    tmGerar:
      aXml := '<GerarNfseEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'GerarNfseEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>GerarNfseEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</GerarNfseEnvio>';

    tmRecepcionarSincrono:
      aXml := '<EnviarLoteRpsSincronoEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'EnviarLoteRpsSincronoEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>EnviarLoteRpsSincronoEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</EnviarLoteRpsSincronoEnvio>';

    tmSubstituirNFSe:
      aXml := '<SubstituirNfseEnvio xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                 SeparaDados(aXml, 'SubstituirNfseEnvio') +
                 '<MetodoInfo>' +
                   Seguranca +
                   '<Versao>' +
                     '<VersaoNumero>2.02</VersaoNumero>' +
                     '<VersaoMetodo>SubstituirNfseEnvio</VersaoMetodo>' +
                   '</Versao>' +
                 '</MetodoInfo>' +
              '</SubstituirNfseEnvio>';
  else
    Response.ArquivoEnvio := aXml;
  end;

  Response.ArquivoEnvio := aXml;
end;

end.
