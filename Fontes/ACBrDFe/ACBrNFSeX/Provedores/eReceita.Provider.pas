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

unit eReceita.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceeReceita202 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetSoapAction: string;
    function GetNameSpace: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property SoapAction: string read GetSoapAction;
    property NameSpace: string read GetNameSpace;
  end;

  TACBrNFSeProvidereReceita202 = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, eReceita.GravarXml, eReceita.LerXml;

{ TACBrNFSeProvidereReceita202 }

procedure TACBrNFSeProvidereReceita202.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProvidereReceita202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_eReceita202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereReceita202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_eReceita202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereReceita202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceeReceita202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceeReceita202 }

function TACBrNFSeXWebserviceeReceita202.GetNameSpace: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'xmlns:nfs="http://webservice.ereceita.net.br/soap/NfseWebService"'
  else
    Result := 'xmlns:nfs="http://www3.ereceita.net.br/soap/NfseWebService"';
end;

function TACBrNFSeXWebserviceeReceita202.GetSoapAction: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'https://www.ereceita.net.br/'
  else
    Result := 'https://www3.ereceita.net.br/';

  // comentado a linha abaixo por conta da cidade São Gotardo/MG em 08/09/2023
  // por: Italo
//    Result := 'https://www3.ereceita.net.br/ereceita/rpp/';
end;

function TACBrNFSeXWebserviceeReceita202.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:RecepcionarLoteRpsRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar(SoapAction + 'RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:GerarNfseRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:GerarNfseRequest>';

  Result := Executar(SoapAction + 'GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:ConsultarLoteRpsRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:ConsultarLoteRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:ConsultarNfseFaixaRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:ConsultarNfseFaixaRequest>';

  Result := Executar(SoapAction + 'ConsultarNfseFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:ConsultarNfsePorRpsRequest>';

  Result := Executar(SoapAction + 'ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:CancelarNfseRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:CancelarNfseRequest>';

  Result := Executar(SoapAction + 'CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfs:SubstituirNfseRequest>';
  Request := Request + '<nfs:nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfs:nfseCabecMsg>';
  Request := Request + '<nfs:nfseDadosMsg>' + XmlToStr(AMSG) + '</nfs:nfseDadosMsg>';
  Request := Request + '</nfs:SubstituirNfseRequest>';

  Result := Executar(SoapAction + 'SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'], [NameSpace]);
end;

function TACBrNFSeXWebserviceeReceita202.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

end.
