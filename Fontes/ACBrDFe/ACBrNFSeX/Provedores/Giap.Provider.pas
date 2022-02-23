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

unit Giap.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrBase, ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceGiap = class(TACBrNFSeXWebserviceNoSoap)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderGiap = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, synacode,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Giap.GravarXml, Giap.LerXml;

{ TACBrNFSeProviderGiap }

procedure TACBrNFSeProviderGiap.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    QuebradeLinha := '\\';
    UseCertificateHTTP := False;
    UseAuthorizationHeader := True;
    ModoEnvio := meLoteAssincrono;
    ConsultaLote := False;
    ConsultaNFSe := False;
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'notaFiscal';
      DocElemento := 'nfe';
    end;

    with LoteRps do
    begin
      InfElemento := 'notaFiscal';
      DocElemento := 'nfe';
    end;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderGiap.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Giap.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGiap.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Giap.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGiap.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceGiap.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderGiap.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode.Childrens.FindAnyNs('ListaMensagemRetorno');

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then
    ANodeArray := ANode.Childrens.FindAllAnyNs('MensagemRetorno');

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Erro'), tcStr);
    AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Status'), tcStr);

    if AErro.Descricao = '' then
      AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

    AErro.Correcao := '';
  end;
end;

function TACBrNFSeProviderGiap.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderGiap.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := '<nfe>' + Params.Xml + '</nfe>';
end;

procedure TACBrNFSeProviderGiap.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANodeArray: TACBrXmlNodeArray;
  ANode: TACBrXmlNode;
  i: Integer;
  NumRps: String;
  ANota: TNotaFiscal;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'Msg');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      {
      ANode := ANode.Childrens.FindAnyNs('nfeResposta');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;
      }
      ANodeArray := ANode.Childrens.FindAllAnyNs('notaFiscal');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];

        with Response do
        begin
          NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numeroNota'), tcStr);
          CodVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('codigoVerificacao'), tcStr);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('statusEmissao'), tcStr);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('link'), tcStr);
        end;

        NumRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('numeroRps'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XmlNfse := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderGiap.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := Desc117;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<consulta>' +
                          '<inscricaoMunicipal>' +
                            OnlyNumber(Emitente.InscMun) +
                          '</inscricaoMunicipal>' +
                          '<codigoVerificacao>' +
                            Response.CodVerificacao +
                          '</codigoVerificacao>' +
                       '</consulta>';
end;

procedure TACBrNFSeProviderGiap.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  {
  ANodeArray: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
  }
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'Msg');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('notaExiste'), tcStr);
        Response.NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numeroNota'), tcStr);
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderGiap.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := Desc109;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Response.ArquivoEnvio := '<nfe>' +
                         '<cancelaNota>' +
                           '<codigoMotivo>' +
                              Response.InfCancelamento.CodCancelamento +
                           '</codigoMotivo>' +
                           '<numeroNota>' +
                              Response.InfCancelamento.NumeroNFSe +
                           '</numeroNota>' +
                         '</cancelaNota>' +
                       '</nfe>';
end;

procedure TACBrNFSeProviderGiap.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  I: Integer;
  NumRps: String;
  ANota: TNotaFiscal;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'Msg');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      Response.Lote := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroLote'), tcStr);

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');
        AuxNode := AuxNode.Childrens.FindAnyNs('NumeroRps');

        if AuxNode <> nil then
          NumRps := ObterConteudoTag(AuxNode, tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XmlNfse := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceGiap }

procedure TACBrNFSeXWebserviceGiap.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth, Token: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
  begin
    Token := WSChaveAutoriz;
    Auth := InscMun + '-' + Token;
//    Auth := InscMun + '-' + UpperCase(EncodeBase64(Token));
  end;

  aHeaderReq.AddHeader('Authorization', Auth);
  aHeaderReq.AddHeader('postman-token', Token);
end;

function TACBrNFSeXWebserviceGiap.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceGiap.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceGiap.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

end.
