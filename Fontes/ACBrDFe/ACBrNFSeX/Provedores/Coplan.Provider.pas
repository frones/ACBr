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

unit Coplan.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceCoplan201 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;

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

    property Namespace: string read GetNamespace;
  end;

  TACBrNFSeProviderCoplan201 = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, Coplan.GravarXml, Coplan.LerXml;

{ TACBrNFSeProviderCoplan201 }

procedure TACBrNFSeProviderCoplan201.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal := True;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '2.01';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderCoplan201.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Coplan201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCoplan201.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Coplan201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCoplan201.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceCoplan201.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceCoplan201 }

function TACBrNFSeXWebserviceCoplan201.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'Tributario_PRODUCAO_FULL'
  else
    Result := 'TributarioGx16New';

  Result := 'xmlns:trib1="' + Result + '"';
end;

function TACBrNFSeXWebserviceCoplan201.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.RECEPCIONARLOTERPS>';
  Request := Request + '<trib:Recepcionarloterpsrequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Recepcionarloterpsrequest>';
  Request := Request + '</trib:nfse_web_service.RECEPCIONARLOTERPS>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.RECEPCIONARLOTERPS', Request,
                     ['Recepcionarloterpsresponse', 'outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.RECEPCIONARLOTERPSSINCRONO>';
  Request := Request + '<trib:Recepcionarloterpssincronorequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Recepcionarloterpssincronorequest>';
  Request := Request + '</trib:nfse_web_service.RECEPCIONARLOTERPSSINCRONO>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.RECEPCIONARLOTERPSSINCRONO', Request,
                     ['Recepcionarloterpssincronoresponse', 'outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.GERARNFSE>';
  Request := Request + '<trib:Gerarnfserequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Gerarnfserequest>';
  Request := Request + '</trib:nfse_web_service.GERARNFSE>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.GERARNFSE', Request,
                     ['Gerarnfseresponse', 'outputXML', 'GerarNfseResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CONSULTARLOTERPS>';
  Request := Request + '<trib:Consultarloterpsrequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Consultarloterpsrequest>';
  Request := Request + '</trib:nfse_web_service.CONSULTARLOTERPS>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CONSULTARLOTERPS', Request,
                     ['Consultarloterpsresponse', 'outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CONSULTARNFSEFAIXA>';
  Request := Request + '<trib:Consultarnfseporfaixarequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Consultarnfseporfaixarequest>';
  Request := Request + '</trib:nfse_web_service.CONSULTARNFSEFAIXA>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CONSULTARNFSEFAIXA', Request,
                     ['Consultarnfseporfaixaresponse', 'outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
  {
    os campos <Codigo>, <Mensagem> e <Correcao> estão dentro do grupo:
       <ConsultarNfseFaixaResposta> em vez de <MensagemRetorno>
    Vai ser necessário estudar a melhor forma de resolver esse problema
  }
end;

function TACBrNFSeXWebserviceCoplan201.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CONSULTARNFSEPORRPS>';
  Request := Request + '<trib:Consultarnfseporrpsrequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Consultarnfseporrpsrequest>';
  Request := Request + '</trib:nfse_web_service.CONSULTARNFSEPORRPS>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CONSULTARNFSEPORRPS', Request,
                     ['Consultarnfseporrpsresponse', 'outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CONSULTARNFSESERVICOPRESTADO>';
  Request := Request + '<trib:Consultarnfseservicoprestadorequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Consultarnfseservicoprestadorequest>';
  Request := Request + '</trib:nfse_web_service.CONSULTARNFSESERVICOPRESTADO>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CONSULTARNFSESERVICOPRESTADO', Request,
                     ['Consultarnfseservicoprestadoresponse', 'outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CONSULTARNFSESERVICOTOMADO>';
  Request := Request + '<trib:Consultarnfseservicotomadorequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Consultarnfseservicotomadorequest>';
  Request := Request + '</trib:nfse_web_service.CONSULTARNFSESERVICOTOMADO>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CONSULTARNFSESERVICOTOMADO', Request,
                     ['Consultarnfseservicotomadoresponse', 'outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.CANCELARNFSE>';
  Request := Request + '<trib:Cancelarnfserequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Cancelarnfserequest>';
  Request := Request + '</trib:nfse_web_service.CANCELARNFSE>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.CANCELARNFSE', Request,
                     ['Cancelarnfseresponse', 'outputXML', 'CancelarNfseResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<trib:nfse_web_service.SUBSTITUIRNFSE>';
  Request := Request + '<trib:Substituirnfserequest>';
  Request := Request + '<trib1:nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</trib1:nfseCabecMsg>';
  Request := Request + '<trib1:nfseDadosMsg>' + IncluirCDATA(AMSG) + '</trib1:nfseDadosMsg>';
  Request := Request + '</trib:Substituirnfserequest>';
  Request := Request + '</trib:nfse_web_service.SUBSTITUIRNFSE>';

  Result := Executar('Tributarioaction/ANFSE_WEB_SERVICE.SUBSTITUIRNFSE', Request,
                     ['Substituirnfseresponse', 'outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:trib="Tributario"', NameSpace]);
end;

function TACBrNFSeXWebserviceCoplan201.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCDATA(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
