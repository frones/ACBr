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

unit TcheInfo.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceTcheInfo204 = class(TACBrNFSeXWebserviceSoap11)
  public
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderTcheInfo204 = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, TcheInfo.GravarXml, TcheInfo.LerXml;

{ TACBrNFSeProviderTcheInfo204 }

procedure TACBrNFSeProviderTcheInfo204.Configuracao;
var
  CodigoIBGE: string;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;

    Autenticacao.RequerLogin := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := False;
      EnviarLoteSincrono := False;
      ConsultarLote := False;
      ConsultarFaixaNfse := False;
      ConsultarServicoPrestado := False;
      ConsultarServicoTomado := False;
      SubstituirNfse := False;
    end;
  end;

  with ConfigAssinar do
  begin
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
  end;

  with ConfigMsgDados do
  begin
    GerarPrestadorLoteRps := True;

    with TACBrNFSeX(FAOwner).Configuracoes.Geral do
    begin
      if TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
        CodigoIBGE := IntToStr(CodigoMunicipio)
      else
        CodigoIBGE := '9999999';

      DadosCabecalho := '<cabecalho versao="1.00" xmlns="http://www.abrasf.org.br/nfse.xsd">' +
                        '<versaoDados>2.04</versaoDados>' +
                        '<CodigoIBGE>' + CodigoIBGE + '</CodigoIBGE>' +
                        '<CpfCnpj>' + Emitente.WSUser + '</CpfCnpj>' +
                        '<Token>' + Emitente.WSSenha + '</Token>' +
                        '</cabecalho>';
    end;
  end;
end;

function TACBrNFSeProviderTcheInfo204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_TcheInfo204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderTcheInfo204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_TcheInfo204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderTcheInfo204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceTcheInfo204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceTcheInfo204 }

function TACBrNFSeXWebserviceTcheInfo204.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:NfseWebService.GERARNFSE>';
  Request := Request + '<nfse:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</nfse:Nfsecabecmsg>';
  Request := Request + '<nfse:Nfsedadosmsg>' + XmlToStr(AMSG) + '</nfse:Nfsedadosmsg>';
  Request := Request + '</nfse:NfseWebService.GERARNFSE>';

  Result := Executar('http://www.abrasf.org.br/nfse.xsdaction/ANFSEWEBSERVICE.GERARNFSE',
                     Request,
                     ['Outputxml', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceTcheInfo204.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

function TACBrNFSeXWebserviceTcheInfo204.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:NfseWebService.CONSULTARNFSERPS>';
  Request := Request + '<nfse:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</nfse:Nfsecabecmsg>';
  Request := Request + '<nfse:Nfsedadosmsg>' + XmlToStr(AMSG) + '</nfse:Nfsedadosmsg>';
  Request := Request + '</nfse:NfseWebService.CONSULTARNFSERPS>';

  Result := Executar('http://www.abrasf.org.br/nfse.xsdaction/ANFSEWEBSERVICE.CONSULTARNFSERPS',
                     Request,
                     ['Outputxml', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceTcheInfo204.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:NfseWebService.CANCELARNFSE>';
  Request := Request + '<nfse:Nfsecabecmsg>' + XmlToStr(ACabecalho) + '</nfse:Nfsecabecmsg>';
  Request := Request + '<nfse:Nfsedadosmsg>' + XmlToStr(AMSG) + '</nfse:Nfsedadosmsg>';
  Request := Request + '</nfse:NfseWebService.CANCELARNFSE>';

  Result := Executar('http://www.abrasf.org.br/nfse.xsdaction/ANFSEWEBSERVICE.CANCELARNFSE',
                     Request,
                     ['Outputxml', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

end.
