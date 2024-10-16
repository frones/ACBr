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
//    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
//    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLinkNFSe(const ACabecalho, AMSG: String): string; override;
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

    function DefinirIDLote(const ID: string): string; override;

    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;
    {
    procedure GerarMsgDadosConsultaSituacao(Response: TNFSeConsultaSituacaoResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    }

    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure GerarMsgDadosConsultaNFSe(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
    procedure GerarMsgDadosConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
  end;

implementation

uses
  StrUtils,
  ACBrUtil.Base,
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
    UseCertificateHTTP := False;
    {
    UseAuthorizationHeader := False;
    NumMaxRpsGerar  := 1;
    NumMaxRpsEnviar := 50;

    TabServicosExt := False;
    Identificador := 'Id';
    QuebradeLinha := ';';
    }
    ConsultaSitLote := False;
    ConsultaLote := False;
    ConsultaNFSe := False;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;
    Autenticacao.RequerChaveAutorizacao := True;

    ServicosDisponibilizados.ConsultarSituacao := False;
    ServicosDisponibilizados.ConsultarLote := False;
    ServicosDisponibilizados.ConsultarLinkNfse := True;
  end;

  SetXmlNameSpace('http://www.nf-eletronica.com.br/nfse');

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'nfse.xsd';
    ConsultarNFSeRps := 'nfse.xsd';
    ConsultarNFSe := 'nfse.xsd';
    ConsultarLinkNFSe := 'nfse.xsd';
    CancelarNFSe := 'nfse.xsd';

    Validar := False;
  end;
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

function TACBrNFSeProviderNFEletronica.DefinirIDLote(const ID: string): string;
begin
  Result := ' ' + ConfigGeral.Identificador + '="' + ID + '"'
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  strRetorno: string;
  NumeroNota: Integer;
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
      NumeroNota := StrToIntDef(strRetorno, 0);

      if (not StringIsXML(strRetorno)) and (NumeroNota <= 0) then
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

      Response.NumeroNota := IntToStr(NumeroNota);
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

procedure TACBrNFSeProviderNFEletronica.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := '<ws:referencia>' +
                              Response.NumeroRps +
                           '</ws:referencia>';
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
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

      strRetorno := ObterConteudoTag(ANode.Childrens.FindAnyNs('Consulta_Ref_DetalheResult'), tcStr);

      if not StringIsXML(strRetorno) then
      begin
        Response.ArquivoRetorno := '<Consulta_Ref_DetalheResult>' +
                                     '<ListaMensagemRetorno>' +
                                       '<MensagemRetorno>' +
                                         '<Codigo>' + '</Codigo>' +
                                         '<Mensagem>' + strRetorno + '</Mensagem>' +
                                         '<Correcao>' + '</Correcao>' +
                                       '</MensagemRetorno>' +
                                     '</ListaMensagemRetorno>' +
                                   '</Consulta_Ref_DetalheResult>';

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

procedure TACBrNFSeProviderNFEletronica.GerarMsgDadosConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  InfConsulta: TInfConsultaNFSe;
begin
  InfConsulta := Response.InfConsultaNFSe;

  Response.ArquivoEnvio := '<ws:numNota>' +
                              OnlyNumber(InfConsulta.NumeroIniNFSe) +
                           '</ws:numNota>';
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
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

      strRetorno := ObterConteudoTag(ANode.Childrens.FindAnyNs('ConsultaXmlNotaResult'), tcStr);

      if not StringIsXML(strRetorno) then
      begin
        Response.ArquivoRetorno := '<ConsultaXmlNotaResult>' +
                                     '<ListaMensagemRetorno>' +
                                       '<MensagemRetorno>' +
                                         '<Codigo>' + '</Codigo>' +
                                         '<Mensagem>' + strRetorno + '</Mensagem>' +
                                         '<Correcao>' + '</Correcao>' +
                                       '</MensagemRetorno>' +
                                     '</ListaMensagemRetorno>' +
                                   '</ConsultaXmlNotaResult>';

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

procedure TACBrNFSeProviderNFEletronica.PrepararConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  aParams: TNFSeParamsResponse;
begin
  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;

    GerarMsgDadosConsultaLinkNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderNFEletronica.GerarMsgDadosConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse; Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := IfThen(Response.InfConsultaLinkNFSe.NumeroRps <> 0,
                             '<ws:referencia>' +
                                IntToStr(Response.InfConsultaLinkNFSe.NumeroRps) +
                             '</ws:referencia>', '') +
                           IfThen(not EstaVazio(Response.InfConsultaLinkNFSe.NumeroNFSe),
                             '<ws:num_NF>' +
                                Response.InfConsultaLinkNFSe.NumeroNFSe +
                             '</ws:num_NF>', '');
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
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

      strRetorno := ObterConteudoTag(ANode.Childrens.FindAnyNs('Consulta_LinkResult'), tcStr);

      if not StringIsXML(strRetorno) then
      begin
        Response.ArquivoRetorno := '<Consulta_LinkResult>' +
                                     '<ListaMensagemRetorno>' +
                                       '<MensagemRetorno>' +
                                         '<Codigo>' + '</Codigo>' +
                                         '<Mensagem>' + strRetorno + '</Mensagem>' +
                                         '<Correcao>' + '</Correcao>' +
                                       '</MensagemRetorno>' +
                                     '</ListaMensagemRetorno>' +
                                   '</Consulta_LinkResult>';

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

procedure TACBrNFSeProviderNFEletronica.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  InfoCanc: TInfCancelamento;
begin
  InfoCanc := Response.InfCancelamento;

  Response.ArquivoEnvio := '<ws:nNF>' +
                              InfoCanc.NumeroNFSe +
                           '</ws:nNF>' +
                           '<ws:motivo>' +
                              InfoCanc.MotCancelamento +
                           '</ws:motivo>';
end;

procedure TACBrNFSeProviderNFEletronica.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
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

      strRetorno := ObterConteudoTag(ANode.Childrens.FindAnyNs('Cancela_NFeResult'), tcStr);

      if not StringIsXML(strRetorno) then
      begin
        Response.ArquivoRetorno := '<Cancela_NFeResult>' +
                                     '<ListaMensagemRetorno>' +
                                       '<MensagemRetorno>' +
                                         '<Codigo>' + '</Codigo>' +
                                         '<Mensagem>' + strRetorno + '</Mensagem>' +
                                         '<Correcao>' + '</Correcao>' +
                                       '</MensagemRetorno>' +
                                     '</ListaMensagemRetorno>' +
                                   '</Cancela_NFeResult>';

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
{
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
}
function TACBrNFSeXWebserviceNFEletronica.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:Consulta_Ref_Detalhe>';
  Request := Request + AMSG;
  Request := Request + Token;
  Request := Request + '</ws:Consulta_Ref_Detalhe>';

  Result := Executar('http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx/Consulta_Ref_Detalhe',
                     Request, [],
                     ['xmlns:ws="http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx"']);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultaXmlNota>';
  Request := Request + Token;
  Request := Request + AMSG;
  Request := Request + '</ws:ConsultaXmlNota>';

  Result := Executar('http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx/ConsultaXmlNota',
                     Request, [],
                     ['xmlns:ws="http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx"']);
end;

function TACBrNFSeXWebserviceNFEletronica.ConsultarLinkNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:Consulta_Link>';
  Request := Request + AMSG;
  Request := Request + Token;
  Request := Request + '</ws:Consulta_Link>';

  Result := Executar('http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx/Consulta_Link',
                     Request, [],
                     ['xmlns:ws="http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx"']);
end;

function TACBrNFSeXWebserviceNFEletronica.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:Cancela_NFe>';
  Request := Request + AMSG;
  Request := Request + Token;
  Request := Request + '</ws:Cancela_NFe>';

  Result := Executar('http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx/Cancela_NFe',
                     Request, [],
                     ['xmlns:ws="http://www.nf-eletronica.com.br/ws_nf/WS_NF_Serv.asmx"']);
end;

function TACBrNFSeXWebserviceNFEletronica.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
end;

end.
