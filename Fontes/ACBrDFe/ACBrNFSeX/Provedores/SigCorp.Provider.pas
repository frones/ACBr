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

unit SigCorp.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSigCorp203 = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProviderSigCorp203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, SigCorp.GravarXml, SigCorp.LerXml;

{ TACBrNFSeProviderSigCorp203 }

procedure TACBrNFSeProviderSigCorp203.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigAssinar do
  begin
    Rps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderSigCorp203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SigCorp203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSigCorp203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SigCorp203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSigCorp203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSigCorp203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceSigCorp203 }

function TACBrNFSeXWebserviceSigCorp203.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteRps>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:RecepcionarLoteRps>';

  Result := Executar('http://tempuri.org/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteRpsSincrono>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:RecepcionarLoteRpsSincrono>';

  Result := Executar('http://tempuri.org/RecepcionarLoteRpsSincrono', Request,
                     ['RecepcionarLoteRpsSincronoResult', 'EnviarLoteRpsResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:GerarNfse>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:GerarNfse>';

  Result := Executar('http://tempuri.org/GerarNfse', Request,
                     ['GerarNfseResult', 'GerarNfseResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarLoteRps>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:ConsultarLoteRps>';

  Result := Executar('http://tempuri.org/ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarNfsePorFaixa>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:ConsultarNfsePorFaixa>';

  Result := Executar('http://tempuri.org/ConsultarNfsePorFaixa', Request,
                     ['ConsultarNfsePorFaixaResult', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarNfsePorRps>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:ConsultarNfsePorRps>';

  Result := Executar('http://tempuri.org/ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResult', 'ConsultarNfseRpsResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarNfseServicoPrestado>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:ConsultarNfseServicoPrestado>';

  Result := Executar('http://tempuri.org/ConsultarNfseServicoPrestado', Request,
                     ['ConsultarNfseServicoPrestadoResult', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarNfseServicoTomado>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:ConsultarNfseServicoTomado>';

  Result := Executar('http://tempuri.org/ConsultarNfseServicoTomado', Request,
                     ['ConsultarNfseServicoTomadoResult', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:CancelaNota>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:CancelaNota>';

  Result := Executar('http://tempuri.org/CancelaNota', Request,
                     ['CancelaNotaResult', 'CancelarNfseResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:SubstituirNfse>';
  Request := Request + '<tem:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</tem:nfseCabecMsg>';
  Request := Request + '<tem:nfseDadosMsg>' + XmlToStr(AMSG) + '</tem:nfseDadosMsg>';
  Request := Request + '</tem:SubstituirNfse>';

  Result := Executar('http://tempuri.org/SubstituirNfse', Request,
                     ['SubstituirNfseResult', 'SubstituirNfseResposta'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceSigCorp203.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, False);
  Result := RemoverDeclaracaoXML(Result);
end;

end.
