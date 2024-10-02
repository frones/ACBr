{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit NFEletronica.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceNFEletronica = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetToken: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property Token: string read GetToken;
  end;

  TACBrNFSeProviderNFEletronica = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;
  end;

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrDFeException,
  ACBrXmlDocument,
  ACBrNFSeXConfiguracoes,
  ACBrNFSeXConsts,
  NFEletronica.GravarXml, NFEletronica.LerXml;

{ TACBrNFSeProviderNFEletronica }

procedure TACBrNFSeProviderNFEletronica.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;
    NumMaxRpsGerar  := 1;
    NumMaxRpsEnviar := 50;

    TabServicosExt := False;
    Identificador := 'Id';
    QuebradeLinha := ';';

    ConsultaSitLote := True;
    ConsultaLote := True;
    ConsultaNFSe := True;
    ConsultaPorFaixa := False;
    CancPreencherMotivo := False;
    CancPreencherSerieNfse := False;
    CancPreencherCodVerificacao := False;
  end;

  SetXmlNameSpace('http://www.nf-eletronica.com.br/nfse');
(*
  // Inicializa os parâmetros de configuração: Mensagem de Dados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';

    UsarNumLoteConsLote := False;

    XmlRps.xmlns := '';
    XmlRps.InfElemento := 'InfRps';
    XmlRps.DocElemento := 'Rps';

    // Usado para geração do Envio do Lote em modo assíncrono
    LoteRps.xmlns := '';
    LoteRps.InfElemento := 'LoteRps';
    LoteRps.DocElemento := 'EnviarLoteRpsEnvio';

    // Usado para geração da Consulta a Situação do Lote
    ConsultarSituacao.xmlns := '';
    ConsultarSituacao.InfElemento := '';
    ConsultarSituacao.DocElemento := 'ConsultarSituacaoLoteRpsEnvio';

    // Usado para geração da Consulta do Lote
    ConsultarLote.xmlns := '';
    ConsultarLote.InfElemento := '';
    ConsultarLote.DocElemento := 'ConsultarLoteRpsEnvio';

    // Usado para geração da Consulta da NFSe por RPS
    ConsultarNFSeRps.xmlns := '';
    ConsultarNFSeRps.InfElemento := '';
    ConsultarNFSeRps.DocElemento := 'ConsultarNfseRpsEnvio';

    // Usado para geração da Consulta da NFSe
    ConsultarNFSe.xmlns := '';
    ConsultarNFSe.InfElemento := '';
    ConsultarNFSe.DocElemento := 'ConsultarNfseEnvio';

    // Usado para geração do Cancelamento
    CancelarNFSe.xmlns := '';
    CancelarNFSe.InfElemento := 'InfPedidoCancelamento';
    CancelarNFSe.DocElemento := 'Pedido';
  end;
*)
end;

function TACBrNFSeProviderNFEletronica.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_NFEletronica.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFEletronica.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_NFEletronica.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFEletronica.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceNFEletronica.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  strRetorno: string;
begin
  Document := TACBrXmlDocument.Create;
  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      strRetorno := ObterConteudoTag(ANode.Childrens.FindAnyNs('UploadArquivoResult'), tcStr);

      if not StringIsXML(strRetorno) then
      begin
        Response.ArquivoRetorno := '<UploadArquivoResult>' +
                                     '<ListaMensagemRetorno>' +
                                       '<MensagemRetorno>' +
                                         '<Codigo>' + '</Codigo>' +
                                         '<Mensagem>' + strRetorno + '</Mensagem>' +
                                         '<Correcao>' + '</Correcao>' +
                                       '</MensagemRetorno>' +
                                     '</ListaMensagemRetorno>' +
                                   '</UploadArquivoResult>';

        Document.Clear;
        Document.LoadFromXml(Response.ArquivoRetorno);

        ANode := Document.Root;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), FpFormatoDataRecebimento);
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceNFEletronica }

function TACBrNFSeXWebserviceNFEletronica.GetToken: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
    Result := '<ws:token_nick>' + WSChaveAcesso + '</ws:token_nick>' +
              '<ws:token_pass>' + WSChaveAutoriz + '</ws:token_pass>';
end;

function TACBrNFSeXWebserviceNFEletronica.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:UploadArquivo>';
  Request := Request + '<ws:doc>' + AMSG + '</ws:doc>';
  Request := Request + Token;
  Request := Request + '</ws:UploadArquivo>';

  Result := Executar('http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx/UploadArquivo',
                     Request, [],
                     ['xmlns:ws="http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx"']);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarLoteRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarSituacaoLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarSituacaoLoteRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarSituacaoLoteRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarNfsePorRpsRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     []);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:ConsultarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:ConsultarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'ConsultarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceNFEletronica.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<wsn:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</wsn:CancelarNfseRequest>';

  Result := Executar('', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     []);
end;

function TACBrNFSeXWebserviceNFEletronica.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
end;

end.
