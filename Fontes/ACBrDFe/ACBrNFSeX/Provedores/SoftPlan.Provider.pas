{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit SoftPlan.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrJSON,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceSoftPlan = class(TACBrNFSeXWebserviceRest)
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderSoftPlan = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
    FpMimeType: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'message';
                                     const AMessageTag: string = ''); override;

    procedure ProcessarMensagemDeErros(LJson: TACBrJSONObject;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'erros');

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

  end;

implementation

uses
  ACBrDFeException,  ACBrUtil.FilesIO,
  ACBrNFSeX, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  SoftPlan.GravarXml, SoftPlan.LerXml;

{ TACBrNFSeProviderSoftPlan }

procedure TACBrNFSeProviderSoftPlan.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
    Identificador := '';
  end;

  with ConfigAssinar do
  begin
    RpsGerarNFSe := True;
    CancelarNFSe := True;
  end;

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'xmlProcessamentoNfpse';
      DocElemento := 'xmlProcessamentoNfpse';
    end;

    with CancelarNFSe do
    begin
      InfElemento := 'xmlCancelamentoNfpse';
      DocElemento := 'xmlCancelamentoNfpse';
    end;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderSoftPlan.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SoftPlan.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSoftPlan.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SoftPlan.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSoftPlan.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebserviceSoftPlan.Create(FAOwner, AMetodo, URL,
      FpMethod, FpMimeType);
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderSoftPlan.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse; const AListTag,
  AMessageTag: string);
var
//  I: Integer;
  ANode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if ANode <> nil then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ANode.AsString;
    AErro.Correcao := '';
  end;
  {
  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ERRO'), tcStr);
    AErro.Correcao := '';
  end;
  }
end;

procedure TACBrNFSeProviderSoftPlan.ProcessarMensagemDeErros(
  LJson: TACBrJSONObject; Response: TNFSeWebserviceResponse;
  const AListTag: string);
var
  Codigo, Descricao: string;
  AErro: TNFSeEventoCollectionItem;
begin
  Codigo := '';
  Descricao := '';
  LJson.Value('error', Codigo);
  LJson.Value('error_description', Descricao);

  AErro := Response.Erros.New;
  AErro.Codigo := Codigo;
  AErro.Descricao := Descricao;
  AErro.Correcao := '';
end;

procedure TACBrNFSeProviderSoftPlan.PrepararEmitir(
  Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Nota: TNotaFiscal;
  IdAttr, ListaRps: string;
  I: Integer;
begin
  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := Desc002;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := 'Conjunto de DPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' DPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count);
  end;

  if Response.Erros.Count > 0 then Exit;

  ListaRps := '';

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    Nota.GerarXML;

    Nota.XmlRps := AplicarXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := AplicarLineBreak(Nota.XmlRps, '');

    if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono])) or
       (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);

      Response.ArquivoEnvio := Nota.XmlRps;
    end;

    SalvarXmlRps(Nota);

    ListaRps := ListaRps + Nota.XmlRps;
  end;

  Response.ArquivoEnvio := ListaRps;

  FpMimeType := 'application/xml';
  FpPath := '/processamento/notas/processa';
  FpMethod := 'POST';
end;

procedure TACBrNFSeProviderSoftPlan.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := Desc201;
    Exit
  end;

  if StringIsXml(Response.ArquivoRetorno) then
  begin
    DocumentXml := TACBrXmlDocument.Create;

    try
      try
        if Response.ArquivoRetorno = '' then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit
        end;

        DocumentXml.LoadFromXml(Response.ArquivoRetorno);

        ANode := DocumentXml.Root;

        ProcessarMensagemErros(ANode, Response);
        Response.Sucesso := (Response.Erros.Count = 0);

        if Response.Sucesso then
        begin
          with Response do
          begin
            CodVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('codigoVerificacao'), tcStr);
            Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dataEmissao'), tcDat);
            DataCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('dataCancelamento'), tcDat);
            DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('motivoCancelamento'), tcStr);
            NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('identificacao'), tcStr);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroNota);

          ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
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
      FreeAndNil(DocumentXml);
    end;
  end
  else
  begin
    Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

    try
      try
        ProcessarMensagemDeErros(Document, Response);
        Response.Sucesso := (Response.Erros.Count = 0);
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
end;

{ TACBrNFSeXWebserviceSoftPlan }

function TACBrNFSeXWebserviceSoftPlan.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceSoftPlan.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceSoftPlan.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

end.
