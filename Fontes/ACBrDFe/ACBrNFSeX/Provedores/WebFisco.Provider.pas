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

unit WebFisco.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceWebFisco = class(TACBrNFSeXWebserviceSoap11)
  protected
    function GetSoapActionURL: string; virtual;

  private
    function GetSoapAction: string;
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property SoapActionURL: string read GetSoapActionURL;
    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderWebFisco = class (TACBrNFSeProviderProprio)
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

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrDFeException,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  WebFisco.GravarXml, WebFisco.LerXml;

{ TACBrNFSeProviderWebFisco }

procedure TACBrNFSeProviderWebFisco.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    ModoEnvio := meUnitario;
    UseCertificateHTTP := False;
    DetalharServico := True;

    Autenticacao.RequerLogin := True;

    with ServicosDisponibilizados do
    begin
      EnviarUnitario := True;
      ConsultarNfse := True;
      ConsultarRps := True;
      CancelarNfse := True;
    end;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderWebFisco.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_WebFisco.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderWebFisco.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_WebFisco.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderWebFisco.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceWebFisco.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderWebFisco.ProcessarMensagemErros(
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
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr));
    AErro.Correcao := '';

    if AErro.Descricao = '' then
      AErro.Descricao := ACBrStr(ANodeArray[I].AsString);
  end;
end;

function TACBrNFSeProviderWebFisco.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderWebFisco.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
begin
  with Params do
    Response.ArquivoEnvio := Xml;
end;

procedure TACBrNFSeProviderWebFisco.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
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

      //ProcessarMensagemErros(ANode, Response, '', 'okk');

      //Response.Sucesso := (Response.Erros.Count = 0);

      with Response do
      begin
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('okk'), tcStr);
      end;

      Response.Sucesso := (Response.Situacao = 'OK');

      if not Response.Sucesso then
        ProcessarMensagemErros(ANode, Response, '', 'okk');

      AuxNode := ANode.Childrens.FindAnyNs('okk');

      if AuxNode <> nil then
      begin
        {
        AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveRPS');

        if (AuxNodeChave <> nil) then
        begin
          with Response do
          begin
            InscricaoPrestador := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            SerieRPS := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('SerieRPS'), tcStr);
            NumeroRPS := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('NumeroRPS'), tcStr);
          end;
        end;
        }
      end;
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

procedure TACBrNFSeProviderWebFisco.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.CNPJ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod130;
    AErro.Descricao := ACBrStr(Desc130);
    Exit;
  end;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;

  if EstaVazio(TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod133;
    AErro.Descricao := ACBrStr(Desc133);
    Exit;
  end;

  Response.ArquivoEnvio := '<ConsultaNfe>' +
                             '<usuario xsi:type="xsd:string">' +
                               Trim(Emitente.WSUser) +
                             '</usuario>' +
                             '<pass xsi:type="xsd:string">' +
                               Trim(Emitente.WSSenha) +
                             '</pass>' +
                             '<prf xsi:type="xsd:string">' +
                               TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura +
                             '</prf>' +
                             '<usr xsi:type="xsd:string">' +
                               Trim(Emitente.CNPJ) +
                             '</usr>' +
                             '<ctr xsi:type="xsd:string">' +
                               Response.NumeroRps +
                             '</ctr>' +
                             '<tipo xsi:type="xsd:string">' +
                               '2' +
                             '</tipo>' +
                           '</ConsultaNfe>';
end;

procedure TACBrNFSeProviderWebFisco.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANota: TNotaFiscal;
  xStatus: string;
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

      with Response do
      begin
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('okk'), tcStr);
        NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfenumerorps'), tcStr);
        NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfenumero'), tcStr);
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfedata'), tcDat);
        Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('nfehora'), tcHor);
        CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfeautenticacao'), tcStr);
        Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfelink'), tcStr);
        Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
        xStatus := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfestatus'), tcStr);

        if UpperCase(xStatus) = 'SIM' then
          DescSituacao := 'NFSe Cancelada';
      end;

      Response.Sucesso := (Response.Situacao = 'OK');

      if not Response.Sucesso then
        ProcessarMensagemErros(ANode, Response, '', 'okk');

      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroRps);

      if ANota = nil then
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroNota);

      ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
      SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderWebFisco.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.CNPJ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod130;
    AErro.Descricao := ACBrStr(Desc130);
    Exit;
  end;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;

  if EstaVazio(TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod133;
    AErro.Descricao := ACBrStr(Desc133);
    Exit;
  end;

  Response.Metodo := tmConsultarNFSe;

  Response.ArquivoEnvio := '<ConsultaNfe>' +
                             '<usuario xsi:type="xsd:string">' +
                               Trim(Emitente.WSUser) +
                             '</usuario>' +
                             '<pass xsi:type="xsd:string">' +
                               Trim(Emitente.WSSenha) +
                             '</pass>' +
                             '<prf xsi:type="xsd:string">' +
                               TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura +
                             '</prf>' +
                             '<usr xsi:type="xsd:string">' +
                               Trim(Emitente.CNPJ) +
                             '</usr>' +
                             '<ctr xsi:type="xsd:string">' +
                               Response.InfConsultaNFSe.NumeroIniNFSe +
                             '</ctr>' +
                             '<tipo xsi:type="xsd:string">' +
                               '1' +
                             '</tipo>' +
                           '</ConsultaNfe>';
end;

procedure TACBrNFSeProviderWebFisco.TratarRetornoConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANota: TNotaFiscal;
  xStatus: string;
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

//      ProcessarMensagemErros(ANode, Response, '', 'okk');

      with Response do
      begin
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('okk'), tcStr);
        NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfenumerorps'), tcStr);
        NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfenumero'), tcStr);
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfedata'), tcDat);
        Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('nfehora'), tcHor);
        CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfeautenticacao'), tcStr);
        Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfelink'), tcStr);
        Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
        xStatus := ObterConteudoTag(ANode.Childrens.FindAnyNs('nfestatus'), tcStr);

        if UpperCase(xStatus) = 'SIM' then
          DescSituacao := 'NFSe Cancelada';
      end;

      Response.Sucesso := (Response.Situacao = 'OK');

      if not Response.Sucesso then
        ProcessarMensagemErros(ANode, Response, '', 'okk');

      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroRps);

      if ANota = nil then
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroNota);

      ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
      SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderWebFisco.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.CNPJ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod130;
    AErro.Descricao := ACBrStr(Desc130);
    Exit;
  end;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;

  if EstaVazio(TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod133;
    AErro.Descricao := ACBrStr(Desc133);
    Exit;
  end;

  Response.ArquivoEnvio := '<CancelaNfe>' +
                             '<usuario xsi:type="xsd:string">' +
                               Trim(Emitente.WSUser) +
                             '</usuario>' +
                             '<pass xsi:type="xsd:string">' +
                               Trim(Emitente.WSSenha) +
                             '</pass>' +
                             '<prf xsi:type="xsd:string">' +
                               TACBrNFSeX(FAOwner).Configuracoes.Geral.CNPJPrefeitura +
                             '</prf>' +
                             '<usr xsi:type="xsd:string">' +
                               Trim(Emitente.CNPJ) +
                             '</usr>' +
                             '<ctr xsi:type="xsd:string">' +
                               Response.InfCancelamento.NumeroNFSe +
                             '</ctr>' +
                             '<tipo xsi:type="xsd:string">' +
                               '1' +
                             '</tipo>' +
                             '<obs xsi:type="xsd:string">' +
                               Response.InfCancelamento.MotCancelamento +
                             '</obs>' +
                           '</CancelaNfe>';
end;

procedure TACBrNFSeProviderWebFisco.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode{, AuxNode}: TACBrXmlNode;
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

      //ProcessarMensagemErros(ANode, Response, '', 'okk');

      //Response.Sucesso := (Response.Erros.Count = 0);

      Response.RetCancelamento.MsgCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('okk'), tcStr);
      Response.RetCancelamento.Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('okk'), tcStr);
      Response.RetCancelamento.Link := StringReplace(Response.RetCancelamento.Link, '&amp;', '&', [rfReplaceAll]);

      if (Copy(Response.RetCancelamento.Link, 1, 5) = 'https') or
         (Copy(Response.RetCancelamento.Link, 1, 5) = 'http:') or
         (Copy(Response.RetCancelamento.Link, 1, 4) = 'www.') then
      begin
        Response.Sucesso := True;
        Response.RetCancelamento.Sucesso := 'SIM';
      end
      else
      begin
        Response.Sucesso := False;
        Response.RetCancelamento.Sucesso := 'NÃO';
        Response.RetCancelamento.Link := '';
      end;

      {
      AuxNode := ANode.Childrens.FindAnyNs('okk');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          Sucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
        end;
      end;
      }
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

{ TACBrNFSeXWebserviceWebFisco }

function TACBrNFSeXWebserviceWebFisco.GetSoapActionURL: string;
begin
  Result := 'https://www.webfiscotecnologia.com.br/issqn/wservice/';
end;

function TACBrNFSeXWebserviceWebFisco.GetSoapAction: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := 'wsnfeenvia.php/EnvNfe'
  else
    Result := 'wsnfe_teste_homologacao.php/EnvNfe';
end;

function TACBrNFSeXWebserviceWebFisco.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar(SoapActionURL + SoapAction,
                     Request,
                     ['return', 'item'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"']);
end;

function TACBrNFSeXWebserviceWebFisco.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar(SoapActionURL + 'wsnfeconsultaxml.php/ConsultaNfe',
                     Request,
                     ['return', 'item'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"']);
end;

function TACBrNFSeXWebserviceWebFisco.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar(SoapActionURL + 'wsnfeconsultaxml.php/ConsultaNfe',
                     Request,
                     ['return', 'item'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"']);
end;

function TACBrNFSeXWebserviceWebFisco.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar(SoapActionURL + 'wsnfecancela.php/CancelaNfe',
                     Request,
                     ['return', 'item'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"']);
end;

end.
