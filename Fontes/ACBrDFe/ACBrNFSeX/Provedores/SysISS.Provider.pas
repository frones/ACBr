{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit SysISS.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSysISS202 = class(TACBrNFSeXWebserviceSoap11)
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

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSysISS202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrDFeException,
  ACBrUtil.XMLHTML,
  SysISS.GravarXml, SysISS.LerXml;

{ TACBrNFSeProviderSysISS202 }

procedure TACBrNFSeProviderSysISS202.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal := True;

  ConfigAssinar.LoteRps := True;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
    AtribVerLote := 'versao';
  end;
end;

function TACBrNFSeProviderSysISS202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SysISS202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSysISS202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SysISS202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSysISS202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSysISS202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceSysISS202 }

function TACBrNFSeXWebserviceSysISS202.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.ENVIARLOTERPS>';
  Request := Request + '<ws:Enviarloterpsin>' + XmlToStr(AMSG) + '</ws:Enviarloterpsin>';
  Request := Request + '</ws:ws.ENVIARLOTERPS>';

  Result := Executar('wsaction/AWS.ENVIARLOTERPS', Request,
                     ['Enviarloterpsout', 'EnviarLoteRpsResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.ENVIARLOTERPSSINCRONO>';
  Request := Request + '<ws:Enviarloterpssincronoin>' + XmlToStr(AMSG) + '</ws:Enviarloterpssincronoin>';
  Request := Request + '</ws:ws.ENVIARLOTERPSSINCRONO>';

  Result := Executar('wsaction/AWS.ENVIARLOTERPSSINCRONO', Request,
                     ['Enviarloterpssincronoout', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.GERARNFSE>';
  Request := Request + '<ws:Gerarnfsein>' + XmlToStr(AMSG) + '</ws:Gerarnfsein>';
  Request := Request + '</ws:ws.GERARNFSE>';

  Result := Executar('wsaction/AWS.GERARNFSE', Request,
                     ['Gerarnfseout', 'GerarNfseResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CONSULTARLOTERPS>';
  Request := Request + '<ws:Consultarloterpsin>' + XmlToStr(AMSG) + '</ws:Consultarloterpsin>';
  Request := Request + '</ws:ws.CONSULTARLOTERPS>';

  Result := Executar('wsaction/AWS.CONSULTARLOTERPS', Request,
                     ['Consultarloterpsout', 'ConsultarLoteRpsResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CONSULTARNFSEFAIXA>';
  Request := Request + '<ws:Consultarnfsefaixain>' + XmlToStr(AMSG) + '</ws:Consultarnfsefaixain>';
  Request := Request + '</ws:ws.CONSULTARNFSEFAIXA>';

  Result := Executar('wsaction/AWS.CONSULTARNFSEFAIXA', Request,
                     ['Consultarnfsefaixaout', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CONSULTARNFSERPS>';
  Request := Request + '<ws:Consultarnfserpsin>' + XmlToStr(AMSG) + '</ws:Consultarnfserpsin>';
  Request := Request + '</ws:ws.CONSULTARNFSERPS>';

  Result := Executar('wsaction/AWS.CONSULTARNFSERPS', Request,
                     ['Consultarnfserpsout', 'ConsultarNfseRpsResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CONSULTARNFSESERVICOPRESTADO>';
  Request := Request + '<ws:Consultarnfseservicoprestadoin>' + XmlToStr(AMSG) + '</ws:Consultarnfseservicoprestadoin>';
  Request := Request + '</ws:ws.CONSULTARNFSESERVICOPRESTADO>';

  Result := Executar('wsaction/AWS.CONSULTARNFSESERVICOPRESTADO', Request,
                     ['Consultarnfseservicoprestadoout', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CONSULTARNFSESERVICOTOMADO>';
  Request := Request + '<ws:Consultarnfseservicotomadoin>' + XmlToStr(AMSG) + '</ws:Consultarnfseservicotomadoin>';
  Request := Request + '</ws:ws.CONSULTARNFSESERVICOTOMADO>';

  Result := Executar('wsaction/AWS.CONSULTARNFSESERVICOTOMADO', Request,
                     ['Consultarnfseservicotomadoout', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.CANCELARNFSE>';
  Request := Request + '<ws:Cancelarnfsein>' + XmlToStr(AMSG) + '</ws:Cancelarnfsein>';
  Request := Request + '</ws:ws.CANCELARNFSE>';

  Result := Executar('wsaction/AWS.CANCELARNFSE', Request,
                     ['Cancelarnfseout', 'CancelarNfseResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ws.SUBSTITUIRNFSE>';
  Request := Request + '<ws:Substituirnfsein>' + XmlToStr(AMSG) + '</ws:Substituirnfsein>';
  Request := Request + '</ws:ws.SUBSTITUIRNFSE>';

  Result := Executar('wsaction/AWS.SUBSTITUIRNFSE', Request,
                     ['Substituirnfseout', 'SubstituirNfseResposta'],
                     ['xmlns:ws="ws"']);
end;

function TACBrNFSeXWebserviceSysISS202.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := Trim(StringReplace(Result, '&', '&amp;', [rfReplaceAll]));
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
