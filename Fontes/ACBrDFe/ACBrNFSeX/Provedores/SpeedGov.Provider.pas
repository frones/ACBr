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

unit SpeedGov.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSpeedGov = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSpeedGov = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, SpeedGov.GravarXml, SpeedGov.LerXml;

{ TACBrNFSeXWebserviceSpeedGov }

function TACBrNFSeXWebserviceSpeedGov.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'RecepcionarLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarSituacaoLoteRps>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:ConsultarSituacaoLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfse>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:ConsultarNfse>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + '<header>' + XmlToStr(ACabecalho) + '</header>';
  Request := Request + '<parameters>' + XmlToStr(AMSG) + '</parameters>';
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceSpeedGov.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
end;

{ TACBrNFSeProviderSpeedGov }

procedure TACBrNFSeProviderSpeedGov.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigMsgDados do
  begin
    Prefixo := 'p';
    PrefixoTS := 'p1';

    XmlRps.xmlns := 'http://ws.speedgov.com.br/tipos_v1.xsd';

    LoteRps.xmlns := 'http://ws.speedgov.com.br/enviar_lote_rps_envio_v1.xsd';

    ConsultarSituacao.xmlns := 'http://ws.speedgov.com.br/consultar_situacao_lote_rps_envio_v1.xsd';

    ConsultarLote.xmlns := 'http://ws.speedgov.com.br/consultar_lote_rps_envio_v1.xsd';

    ConsultarNfseRps.xmlns := 'http://ws.speedgov.com.br/consultar_nfse_rps_envio_v1.xsd';

    ConsultarNfse.xmlns := 'http://ws.speedgov.com.br/consultar_nfse_envio_v1.xsd';

    CancelarNfse.xmlns := 'http://ws.speedgov.com.br/cancelar_nfse_envio_v1.xsd';

    DadosCabecalho := '<p:cabecalho versao="1" xmlns:p="http://ws.speedgov.com.br/cabecalho_v1.xsd">' +
                      '<versaoDados>1</versaoDados>' +
                      '</p:cabecalho>';
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'enviar_lote_rps_envio_v1.xsd';
    ConsultarSituacao := 'consultar_situacao_lote_rps_envio_v1.xsd';
    ConsultarLote := 'consultar_lote_rps_envio_v1.xsd';
    ConsultarNFSeRps := 'consultar_nfse_rps_envio_v1.xsd';
    ConsultarNFSe := 'consultar_nfse_envio_v1.xsd';
    CancelarNFSe := 'cancelar_nfse_envio_v1.xsd';
  end;
end;

function TACBrNFSeProviderSpeedGov.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SpeedGov.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSpeedGov.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SpeedGov.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSpeedGov.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSpeedGov.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

end.
