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

unit SafeWeb.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceSafeWeb200 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSafeWeb200 = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, SafeWeb.GravarXml, SafeWeb.LerXml;

{ TACBrNFSeProviderSafeWeb200 }

procedure TACBrNFSeProviderSafeWeb200.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ModoEnvio := meLoteAssincrono;

  ConfigGeral.Autenticacao.RequerLogin := True;

  with ConfigGeral.ServicosDisponibilizados do
  begin
    EnviarLoteSincrono := False;
    EnviarUnitario := False;
    ConsultarServicoPrestado := False;
    ConsultarServicoTomado := False;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigMsgDados do
  begin
    DadosCabecalho := '<CabecalhoEnvio versao="2" xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                        '<versaoDados>2</versaoDados>' +
                        // verificar se o valor 1 é produção
                        '<TpAmb>' +
                           IntToStr(TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo) +
                        '</TpAmb>' +
                        '<Cnpj>' +
                           TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente.WSUser +
                        '</Cnpj>' +
                        '<CodigoIbge>' +
                           IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                        '</CodigoIbge>' +
                      '</CabecalhoEnvio>';
  end;
end;

function TACBrNFSeProviderSafeWeb200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SafeWeb200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSafeWeb200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SafeWeb200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSafeWeb200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSafeWeb200.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceSafeWeb200 }

function TACBrNFSeXWebserviceSafeWeb200.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<enviarLoteRpsEnvio>' + XmlToStr(AMSG) + '</enviarLoteRpsEnvio>';
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('http://tempuri.org/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult', 'EnviarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<consultarLoteRpsEnvio>' + XmlToStr(AMSG) + '</consultarLoteRpsEnvio>';
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('http://tempuri.org/ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResult', 'ConsultarLoteRpsResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseFaixa xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<consultarNfseFaixaEnvio>' + XmlToStr(AMSG) + '</consultarNfseFaixaEnvio>';
  Request := Request + '</ConsultarNfseFaixa>';

  Result := Executar('http://tempuri.org/ConsultarNfseFaixa', Request,
                     ['ConsultarNfseFaixaResult', 'ConsultarNfseFaixaResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfsePorRps xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<consultarNfseRpsEnvio>' + XmlToStr(AMSG) + '</consultarNfseRpsEnvio>';
  Request := Request + '</ConsultarNfsePorRps>';

  Result := Executar('http://tempuri.org/ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResult', 'ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<cancelarNfseEnvio>' + XmlToStr(AMSG) + '</cancelarNfseEnvio>';
  Request := Request + '</CancelarNfse>';

  Result := Executar('http://tempuri.org/CancelarNfse', Request,
                     ['CancelarNfseResult', 'CancelarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<SubstituirNfse xmlns="http://tempuri.org/">';
  Request := Request + '<cabecalho>' + XmlToStr(ACabecalho) + '</cabecalho>';
  Request := Request + '<substituirNfseEnvio>' + XmlToStr(AMSG) + '</substituirNfseEnvio>';
  Request := Request + '</SubstituirNfse>';

  Result := Executar('http://tempuri.org/SubstituirNfse', Request,
                     ['SubstituirNfseResult', 'SubstituirNfseResposta'], []);
end;

function TACBrNFSeXWebserviceSafeWeb200.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

end.
