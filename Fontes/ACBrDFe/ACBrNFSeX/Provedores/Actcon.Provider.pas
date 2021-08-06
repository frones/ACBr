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

unit Actcon.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceActconv2 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;
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

    property Namespace: string read GetNamespace;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderActconv201 = class(TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeProviderActconv202 = class(TACBrNFSeProviderActconv201)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;

  end;

implementation

uses
  ACBrDFeException, ACBrNFSeX, Actcon.GravarXml, Actcon.LerXml;

{ TACBrNFSeXWebserviceActconv2 }

function TACBrNFSeXWebserviceActconv2.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.NameSpace
  else
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.NameSpace;

  Result := 'xmlns:nfse="' + Result + '"';
end;

function TACBrNFSeXWebserviceActconv2.GetSoapAction: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.SoapAction
  else
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.SoapAction;
end;

function TACBrNFSeXWebserviceActconv2.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRps',
                     Request,
                     ['outputXML', 'EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincronoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRpsSincrono',
                     Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarLoteRps',
                     Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRpsRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorRps',
                     Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixaRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorFaixa',
                     Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoPrestado',
                     Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseServicoTomado',
                     Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.GerarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar(SoapAction + 'GerarNfse',
                     Request,
                     ['outputXML', 'GerarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar(SoapAction + 'CancelarNfse',
                     Request,
                     ['outputXML', 'CancelarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceActconv2.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest ' + NameSpace + '>';

  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar(SoapAction + 'SubstituirNfse',
                     Request,
                     ['outputXML', 'SubstituirNfseResposta'], []);
end;

{ TACBrNFSeProviderActconv201 }

procedure TACBrNFSeProviderActconv201.Configuracao;
var
  NameSpace: string;
begin
  inherited Configuracao;

  ConfigGeral.ModoEnvio := meLoteAssincrono;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    // Esta ocorrendo erro de assinatura ao enviar o SubstituirNFSe
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '1.00';
  end;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
    NameSpace := ConfigWebServices.Homologacao.XMLNameSpace
  else
    NameSpace := ConfigWebServices.Producao.XMLNameSpace;

  SetXmlNameSpace(NameSpace);

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v201.xsd');
end;

function TACBrNFSeProviderActconv201.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Actconv201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderActconv201.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Actconv201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderActconv201.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceActconv2.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeProviderActconv202 }

procedure TACBrNFSeProviderActconv202.Configuracao;
begin
  inherited Configuracao;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '1.00';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v202.xsd');
end;

function TACBrNFSeProviderActconv202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Actconv202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderActconv202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Actconv202.Create(Self);
  Result.NFSe := ANFSe;
end;

end.
