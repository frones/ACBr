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

unit ISSJoinville.Provider;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSJoinville = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;
    function GetSoapAction: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property Namespace: string read GetNamespace;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderISSJoinville = class (TACBrNFSeProviderABRASFv2)
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
  ACBrNFSeXNotasFiscais, ISSJoinville.GravarXml, ISSJoinville.LerXml;

{ TACBrNFSeProviderISSJoinville }

procedure TACBrNFSeProviderISSJoinville.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '\n';
    ModoEnvio := meLoteAssincrono;
    FormatoItemListaServico := filsComFormatacaoSemZeroEsquerda;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
  end;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    SetXmlNameSpace('http://nfemws.joinville.sc.gov.br')
  else
    SetXmlNameSpace('http://nfemwshomologacao.joinville.sc.gov.br');

  with ConfigMsgDados do
  begin
    Prefixo := 'nfem';
    PrefixoTS := 'nfem';

    GerarPrestadorLoteRps := True;
  end;

  SetNomeXSD('nfse_v2-04.xsd');
end;

function TACBrNFSeProviderISSJoinville.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSJoinville.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSJoinville.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSJoinville.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSJoinville.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, CancelarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceISSJoinville.Create(FAOwner, AMetodo, CancelarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderISSJoinville.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml, FpNameSpace: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  xXml := Response.XmlEnvio;

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    FpNameSpace := 'xmlns:nfem="http://nfemws.joinville.sc.gov.br"'
  else
    FpNameSpace := 'xmlns:nfem="http://nfemwshomologacao.joinville.sc.gov.br"';

  case aMetodo of
    tmRecepcionar:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfem:EnviarLoteRpsEnvio ' + FpNameSpace + '>',
          '</nfem:EnviarLoteRpsEnvio>', False);

        xXml := '<nfem:EnviarLoteRpsEnvio>' + xXml + '</nfem:EnviarLoteRpsEnvio>';
      end;

    tmConsultarLote:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfem:ConsultarLoteRpsEnvio ' + FpNameSpace + '>',
          '</nfem:ConsultarLoteRpsEnvio>', False);

        xXml := '<nfem:ConsultarLoteRpsEnvio>' + xXml + '</nfem:ConsultarLoteRpsEnvio>';
      end;

    tmConsultarNFSePorRps:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfem:ConsultarNfseRpsEnvio ' + FpNameSpace + '>',
          '</nfem:ConsultarNfseRpsEnvio>', False);

        xXml := '<nfem:ConsultarNfseRpsEnvio>' + xXml + '</nfem:ConsultarNfseRpsEnvio>';
      end;

    tmCancelarNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<nfem:CancelarNfseEnvio ' + FpNameSpace + '>',
          '</nfem:CancelarNfseEnvio>', False);

        xXml := '<nfem:CancelarNfseEnvio>' + xXml + '</nfem:CancelarNfseEnvio>';
      end;
  else
    Response.XmlEnvio := xXml;
  end;

  Response.XmlEnvio := xXml;
end;

{ TACBrNFSeXWebserviceISSJoinville }

function TACBrNFSeXWebserviceISSJoinville.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'xmlns:nfem="https://nfemws.joinville.sc.gov.br"'
  else
    Result := 'xmlns:nfem="https://nfemwshomologacao.joinville.sc.gov.br"';
end;

function TACBrNFSeXWebserviceISSJoinville.GetSoapAction: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'https://nfemws.joinville.sc.gov.br/'
  else
    Result := 'https://nfemwshomologacao.joinville.sc.gov.br/';
end;

function TACBrNFSeXWebserviceISSJoinville.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar(SoapAction + 'EnviarLoteRpsEnvio', AMSG,
                     ['EnviarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSJoinville.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar(SoapAction + 'ConsultarLoteRpsEnvio', AMSG,
                     ['ConsultarLoteRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSJoinville.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar(SoapAction + 'ConsultarNfseRpsEnvio', AMSG,
                     ['ConsultarNfseRpsResposta'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSJoinville.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar(SoapAction + 'CancelarNfseEnvio', AMSG,
                     ['CancelarNfseResposta'],
                     [NameSpace]);
end;

end.
