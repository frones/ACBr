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

{
  Só funcionou usando o HttpWinNet
}

{$I ACBr.inc}

unit ISSNet.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSNet = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeUrl(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderISSNet = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrDFeException,
  ISSNet.GravarXml, ISSNet.LerXml;

{ TACBrNFSeProviderISSNet }

procedure TACBrNFSeProviderISSNet.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := '';

  with ConfigMsgDados do
  begin
    PrefixoTS := 'tc';

    XmlRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd';

    LoteRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_enviar_lote_rps_envio.xsd';

    ConsultarSituacao.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_situacao_lote_rps_envio.xsd';

    ConsultarLote.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_lote_rps_envio.xsd';

    ConsultarNFSeRps.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_rps_envio.xsd';

    ConsultarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_nfse_envio.xsd';

    CancelarNFSe.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd';

    ConsultarNFSeURL.xmlns := 'http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_consultar_url_visualizacao_nfse_envio.xsd';
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    CancelarNFSe := True;
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'servico_enviar_lote_rps_envio.xsd';
    ConsultarSituacao := 'servico_consultar_situacao_lote_rps_envio.xsd';
    ConsultarLote := 'servico_consultar_lote_rps_envio.xsd';
    ConsultarNFSeRps := 'servico_consultar_nfse_rps_envio.xsd';
    ConsultarNFSe := 'servico_consultar_nfse_envio.xsd';
    ConsultarNFSeURL := 'servico_consultar_url_visualizacao_nfse_envio.xsd';
    CancelarNFSe := 'servico_cancelar_nfse_envio.xsd';
  end;
end;

function TACBrNFSeProviderISSNet.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSNet.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSNet.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSNet.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_SEM_URL);
end;

procedure TACBrNFSeProviderISSNet.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
  i, j: Integer;
begin
  if aMetodo <> tmCancelarNFSe then
  begin
    xXml := Response.XmlEnvio;

    i := Pos('<tc:Cnpj>', xXml) -1;
    j := Pos('<tc:InscricaoMunicipal>', xXml);

    xXml := Copy(xXml, 1, i) +
            '<tc:CpfCnpj>' + Copy(xXml, i+1, j-i-1) + '</tc:CpfCnpj>' +
            Copy(xXml, j, Length(xXml));

    Response.XmlEnvio := xXml;
  end;

  inherited ValidarSchema(Response, aMetodo);
end;

{ TACBrNFSeXWebserviceISSNet }

function TACBrNFSeXWebserviceISSNet.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:RecepcionarLoteRps>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:RecepcionarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/RecepcionarLoteRps',
                     Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:ConsultarLoteRps>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:ConsultarLoteRps>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarLoteRps',
                     Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:ConsultaSituacaoLoteRPS>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:ConsultaSituacaoLoteRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultaSituacaoLoteRPS',
                     Request,
                     ['ConsultaSituacaoLoteRPSResult', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:ConsultarNFSePorRPS>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:ConsultarNFSePorRPS>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNFSePorRPS',
                     Request,
                     ['ConsultarNFSePorRPSResult', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:ConsultarNfse>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:ConsultarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarNfse',
                     Request,
                     ['ConsultarNfseResult', 'ConsultarNfseResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:CancelarNfse>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:CancelarNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/CancelarNfse',
                     Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

function TACBrNFSeXWebserviceISSNet.ConsultarNFSeUrl(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfd:ConsultarUrlVisualizacaoNfse>';
  Request := Request + '<nfd:xml>' + XmlToStr(AMSG) + '</nfd:xml>';
  Request := Request + '</nfd:ConsultarUrlVisualizacaoNfse>';

  Result := Executar('http://www.issnetonline.com.br/webservice/nfd/ConsultarUrlVisualizacaoNfse',
                     Request,
                     ['ConsultarUrlVisualizacaoNfseResult', 'ConsultarUrlVisualizacaoNfseResposta'],
                     ['xmlns:nfd="http://www.issnetonline.com.br/webservice/nfd"']);
end;

end.
