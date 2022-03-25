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

unit DBSeller.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXProviderABRASFv1,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceDBSeller = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNamespace: string;

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property Namespace: string read GetNamespace;
  end;

  TACBrNFSeProviderDBSeller = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); override;
  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrXmlBase, ACBrDFeException,
  DBSeller.GravarXml, DBSeller.LerXml;

{ TACBrNFSeXWebserviceDBSeller }

function TACBrNFSeXWebserviceDBSeller.GetNamespace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'producao'
  else
    Result := 'homologacao';

  Result := 'xmlns:e="' + BaseUrl + '/webservice/index/' + Result + '"';
end;

function TACBrNFSeXWebserviceDBSeller.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:RecepcionarLoteRps>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:RecepcionarLoteRps>';

  Result := Executar('', Request,
                     ['return'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarLoteRps>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:ConsultarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarSituacaoLoteRps>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:ConsultarSituacaoLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarSituacaoLoteRpsResposta'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfsePorRps>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:ConsultarNfsePorRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfsePorRpsResposta'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:ConsultarNfse>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:ConsultarNfse>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseResposta'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<e:CancelarNfse>';
  Request := Request + '<xml><![CDATA[' + AMSG + ']]></xml>';
  Request := Request + '</e:CancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'], [Namespace]);
end;

function TACBrNFSeXWebserviceDBSeller.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, False);
  Result := RemoverDeclaracaoXML(Result);
end;

{ TACBrNFSeProviderDBSeller }

procedure TACBrNFSeProviderDBSeller.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := 'Id';
    UseCertificateHTTP := False;
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    IncluirURI := False;
  end;
end;

function TACBrNFSeProviderDBSeller.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DBSeller.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDBSeller.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DBSeller.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDBSeller.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceDBSeller.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderDBSeller.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  ANode: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  Mensagem: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
  begin
    ANode := RootNode.Childrens.FindAnyNs('ErroWebServiceResposta');

    Mensagem := ObterConteudoTag(ANode.Childrens.FindAnyNs('MensagemErro'), tcStr);

    if Mensagem <> '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoErro'), tcStr);
      AErro.Descricao := Mensagem;
      AErro.Correcao := '';
    end;
  end
  else
    inherited ProcessarMensagemErros(RootNode, Response, AListTag, AMessageTag);
end;

(*
   Retorno ao enviar o Rps:

<ii:ErroWebServiceResposta xmlns:ii="urn:DBSeller">
	<ii:CodigoErro>E157</ii:CodigoErro>
	<ii:MensagemErro>Usuário contribuinte não existe!</ii:MensagemErro>
	<ii:ListaMensagemRetorno/>
</ii:ErroWebServiceResposta>

*)
end.
