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

unit iiBrasil.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceiiBrasil204 = class(TACBrNFSeXWebserviceSoap11)
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

  TACBrNFSeProvideriiBrasil204 = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, iiBrasil.GravarXml, iiBrasil.LerXml;

{ TACBrNFSeProvideriiBrasil204 }

procedure TACBrNFSeProvideriiBrasil204.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    CancPreencherSerieNfse := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
  end;

  with ConfigMsgDados do
  begin
    GerarPrestadorLoteRps := True;
    DadosCabecalho := GetCabecalho('');
  end;
end;

function TACBrNFSeProvideriiBrasil204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_iiBrasil204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvideriiBrasil204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_iiBrasil204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvideriiBrasil204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceiiBrasil204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProvideriiBrasil204.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml, Integridade: string;
  i: Integer;
begin
  xXml := Response.ArquivoEnvio;

  // Precisa verificar o que deve ser utilizado para gerar o valor da Integridade
  // para o provedor iiBrasil
  Integridade := TACBrNFSeX(FAOwner).GerarIntegridade(xXml);
  Integridade := '<Integridade>' + Integridade + '</Integridade>';

  case aMetodo of
    tmGerar:
      begin
        i := Pos('</GerarNfseEnvio>', xXml);

        xXml := Copy(xXml, 1, i -1) + Integridade + '</GerarNfseEnvio>';
        Response.ArquivoEnvio := xXml;
      end;

    tmConsultarNFSePorRps:
      begin
        i := Pos('</ConsultarNfseRpsEnvio>', xXml);

        xXml := Copy(xXml, 1, i -1) + Integridade + '</ConsultarNfseRpsEnvio>';
        Response.ArquivoEnvio := xXml;
      end;

    tmCancelarNFSe:
      begin
        i := Pos('</CancelarNfseEnvio>', xXml);

        xXml := Copy(xXml, 1, i -1) + Integridade + '</CancelarNfseEnvio>';
        Response.ArquivoEnvio := xXml;
      end;

    tmSubstituirNFSe:
      begin
        i := Pos('</SubstituirNfseEnvio>', xXml);

        xXml := Copy(xXml, 1, i -1) + Integridade + '</SubstituirNfseEnvio>';
        Response.ArquivoEnvio := xXml;
      end;
  else
    Response.ArquivoEnvio := xXml;
  end;

  inherited ValidarSchema(Response, aMetodo);
end;

{ TACBrNFSeXWebserviceiiBrasil204 }

function TACBrNFSeXWebserviceiiBrasil204.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorFaixa', Request,
                     ['outputXML', 'ConsultarNfsePorFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceiiBrasil204.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, False);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
