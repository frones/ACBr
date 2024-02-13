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

unit ModernizacaoPublica.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceModernizacaoPublica202 = class(TACBrNFSeXWebserviceSoap11)
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
  end;

  TACBrNFSeProviderModernizacaoPublica202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'Erro'); override;
  end;

implementation

uses
  ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, ModernizacaoPublica.GravarXml, ModernizacaoPublica.LerXml;

{ TACBrNFSeProviderModernizacaoPublica202 }

procedure TACBrNFSeProviderModernizacaoPublica202.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    CancPreencherMotivo := True;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');

  SetNomeXSD('nfse_v202.xsd');
end;

function TACBrNFSeProviderModernizacaoPublica202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ModernizacaoPublica202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderModernizacaoPublica202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ModernizacaoPublica202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderModernizacaoPublica202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceModernizacaoPublica202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderModernizacaoPublica202.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs('Erro');
  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ErroID'), tcStr);
    AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ErroMensagem'), tcStr);
    AErro.Correcao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ErroSolucao'), tcStr);
  end;
end;

{ TACBrNFSeXWebserviceModernizacaoPublica202 }

function TACBrNFSeXWebserviceModernizacaoPublica202.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:RecepcionarLoteRps>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:RecepcionarLoteRps>';

  Result := Executar('', Request,
                     ['RecepcionarLoteRpsReturn', 'EnviarLoteRpsResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:RecepcionarLoteRpsSincrono>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:RecepcionarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['RecepcionarLoteRpsSincronoReturn', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:GerarNfse>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:GerarNfse>';

  Result := Executar('', Request,
                     ['GerarNfseReturn', 'GerarNfseResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:ConsultarLoteRps>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:ConsultarLoteRps>';

  Result := Executar('', Request,
                     ['ConsultarLoteRpsReturn', 'ConsultarLoteRpsResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:ConsultarNfsePorFaixa>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:ConsultarNfsePorFaixa>';

  Result := Executar('', Request,
                     ['ConsultarNfsePorFaixaReturn', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:ConsultarNfsePorRps>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:ConsultarNfsePorRps>';

  Result := Executar('', Request,
                     ['ConsultarNfsePorRpsReturn', 'ConsultarNfseRpsResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:ConsultarNfseServicoPrestado>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:ConsultarNfseServicoPrestado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoPrestadoReturn', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:ConsultarNfseServicoTomado>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:ConsultarNfseServicoTomado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoTomadoReturn', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:CancelarNfse>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:CancelarNfse>';

  Result := Executar('', Request,
                     ['CancelarNfseReturn', 'CancelarNfseResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<def:SubstituirNfse>';
  Request := Request + '<Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</Nfsecabecmsg>';
  Request := Request + '<Nfsedadosmsg>' + XmlToStr(AMSG) + '</Nfsedadosmsg>';
  Request := Request + '</def:SubstituirNfse>';

  Result := Executar('', Request,
                     ['SubstituirNfseReturn', 'SubstituirNfseResposta'],
                     ['xmlns:def="http://DefaultNamespace"']);
end;

function TACBrNFSeXWebserviceModernizacaoPublica202.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result));
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
