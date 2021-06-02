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
  TACBrNFSeXWebserviceAEG = class(TACBrNFSeXWebserviceSoap11)
  private

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
  end;

  TACBrNFSeProviderAEG = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais,
  AEG.GravarXml, AEG.LerXml;

{ TACBrNFSeXWebserviceAEG }

function TACBrNFSeXWebserviceAEG.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:RecepcionarLoteRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:RecepcionarLoteRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#RecepcionarLoteRps',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:RecepcionarLoteRpsSincrono xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:RecepcionarLoteRpsSincrono>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#RecepcionarLoteRpsSincrono',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarLoteRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarLoteRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarLoteRps',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfsePorRps xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfsePorRps>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfsePorRps',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfsePorFaixa xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfsePorFaixa>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfsePorFaixa',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfseServicoPrestado xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfseServicoPrestado>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfseServicoPrestado',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:ConsultarNfseServicoTomado xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:ConsultarNfseServicoTomado>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#ConsultarNfseServicoTomado',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.GerarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:GerarNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:GerarNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#GerarNfse',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:CancelarNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:CancelarNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#CancelarNfse',
                     Request, ['return', 'DocumentElement'], ['']);
end;

function TACBrNFSeXWebserviceAEG.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:SubstituirNfse xmlns:urn="urn:uWSPortalInteg-IWSPortalInteg">';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</urn:SubstituirNfse>';

  Result := Executar('urn:uWSPortalInteg-IWSPortalInteg#SubstituirNfse',
                     Request, ['return', 'DocumentElement'], ['']);
end;

{ TACBrNFSeProviderAEG }

procedure TACBrNFSeProviderAEG.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.GerarNSLoteRps := True;
end;

function TACBrNFSeProviderAEG.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_AEG.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAEG.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_AEG.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAEG.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceAEG.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderAEG.ValidarSchema(Response: TNFSeWebserviceResponse;
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

  aXml := Response.XmlEnvio;

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
    Response.XmlEnvio := aXml;
  end;

  Response.XmlEnvio := aXml;
end;

end.
