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

unit Centi.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceCenti202 = class(TACBrNFSeXWebserviceRest)
  private
    function GetDadosUsuario: string;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderCenti202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  public
    function SituacaoTributariaToStr(const t: TnfseSituacaoTributaria): string; override;
    function StrToSituacaoTributaria(out ok: boolean; const s: string): TnfseSituacaoTributaria; override;
    function SituacaoTributariaDescricao(const t: TnfseSituacaoTributaria): string; override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Centi.GravarXml, Centi.LerXml;

{ TACBrNFSeProviderCenti202 }

procedure TACBrNFSeProviderCenti202.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    RpsGerarNFSe := True;
    CancelarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  SetXmlNameSpace('http://www.centi.com.br/files/nfse.xsd');
end;

function TACBrNFSeProviderCenti202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Centi202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCenti202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Centi202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCenti202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceCenti202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderCenti202.SituacaoTributariaToStr(
  const t: TnfseSituacaoTributaria): string;
begin
  Result := EnumeradoToStr(t,
                             ['0', '1', '2'],
                             [stRetencao, stNormal, stSubstituicao]);
end;

function TACBrNFSeProviderCenti202.StrToSituacaoTributaria(out ok: boolean;
  const s: string): TnfseSituacaoTributaria;
begin
  Result := StrToEnumerado(ok, s,
                             ['0', '1', '2'],
                             [stNormal, stRetencao, stSubstituicao]);
end;

function TACBrNFSeProviderCenti202.SituacaoTributariaDescricao(
  const t: TnfseSituacaoTributaria): string;
begin
  case t of
    stNormal       : Result := '0 - Não' ;
    stRetencao     : Result := '1 - Sim' ;
    stSubstituicao : Result := '2 - Substituição' ;
  else
    Result := '';
  end;
end;

{ TACBrNFSeXWebserviceCenti202 }

function TACBrNFSeXWebserviceCenti202.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<aUsuario>' + Emitente.WSUser + '</aUsuario>' +
              '<aSenha>' + Emitente.WSSenha + '</aSenha>';
  end;
end;

function TACBrNFSeXWebserviceCenti202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request, Operacao: string;
begin
  FPMsgOrig := AMSG;

  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Operacao := 'Homologacao'
  else
    Operacao := '';

  Request := '<GerarNfse' + Operacao +' xmlns="http://tempuri.org/">';
  Request := Request + '<aXml>' + XmlToStr(AMSG) + '</aXml>';
  Request := Request + DadosUsuario;
  Request := Request + '</GerarNfse>';

  Result := Executar('http://tempuri.org/IServiceNfse/GerarNfse' + Operacao, Request,
                            ['return', 'outputXML', 'GerarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceCenti202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseRps xmlns="http://tempuri.org/">';
  Request := Request + '<aXml>' + XmlToStr(AMSG) + '</aXml>';
  Request := Request + DadosUsuario;
  Request := Request + '</ConsultarNfseRps>';

  Result := Executar('http://tempuri.org/IServiceNfse/ConsultarNfseRps', Request,
                     ['return', 'outputXML', 'ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceCenti202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse xmlns="http://tempuri.org/">';
  Request := Request + '<aXml>' + XmlToStr(AMSG) + '</aXml>';
  Request := Request + DadosUsuario;
  Request := Request + '</CancelarNfse>';

  Result := Executar('http://tempuri.org/IServiceNfse/CancelarNfse', Request,
                         ['return', 'outputXML', 'CancelarNfseResposta'], []);
end;

end.
