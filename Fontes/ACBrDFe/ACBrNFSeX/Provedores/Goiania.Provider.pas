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

unit Goiania.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSGoiania200 = class(TACBrNFSeXWebserviceSoap11)
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderISSGoiania200 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Goiania.GravarXml, Goiania.LerXml;

{ TACBrNFSeProviderISSGoiania200 }

procedure TACBrNFSeProviderISSGoiania200.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '\s\n';
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
  end;

  with ConfigAssinar do
  begin
    LoteGerarNFSe := True;
    IncluirURI := False;
  end;

  SetXmlNameSpace('http://nfse.goiania.go.gov.br/xsd/nfse_gyn_v02.xsd');

  SetNomeXSD('nfse_gyn_v02.xsd');
  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderISSGoiania200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSGoiania200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSGoiania200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSGoiania200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSGoiania200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSGoiania200.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_SEM_URL);
end;

procedure TACBrNFSeProviderISSGoiania200.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xAux, xXml, xXmlEnvio: string;
  i: Integer;
begin
  xXml := Response.XmlEnvio;

  if aMetodo = tmGerar then
  begin
    xAux := RetornarConteudoEntre(xXml, '<Signature', '</Signature>', True);

    xXmlEnvio := StringReplace(xXml, xAux, '', [rfReplaceAll]);

    i := Pos('</InfDeclaracaoPrestacaoServico>', xXmlEnvio);

    xXmlEnvio := Copy(xXmlEnvio, 1, i + 31) + xAux + '</Rps>' +
                        '</GerarNfseEnvio>';
  end
  else
    xXmlEnvio := xXml;

  Response.XmlEnvio := xXmlEnvio;

  inherited ValidarSchema(Response, aMetodo);
end;

{ TACBrNFSeXWebserviceISSGoiania200 }

function TACBrNFSeXWebserviceISSGoiania200.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:GerarNfse>';
  Request := Request + '<ws:ArquivoXML>' + XmlToStr(AMSG) + '</ws:ArquivoXML>';
  Request := Request + '</ws:GerarNfse>';

  Result := Executar('http://nfse.goiania.go.gov.br/ws/GerarNfse', Request,
                     ['GerarNfseResult', 'GerarNfseResposta'],
                     ['xmlns:ws="http://nfse.goiania.go.gov.br/ws/"']);
end;

function TACBrNFSeXWebserviceISSGoiania200.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultarNfseRps>';
  Request := Request + '<ws:ArquivoXML>' + XmlToStr(AMSG) + '</ws:ArquivoXML>';
  Request := Request + '</ws:ConsultarNfseRps>';

  Result := Executar('http://nfse.goiania.go.gov.br/ws/ConsultarNfseRps', Request,
                     ['ConsultarNfseRpsResult', 'ConsultarNfseRpsResposta'],
                     ['xmlns:ws="http://nfse.goiania.go.gov.br/ws/"']);
end;

end.
