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
  private
    function GetSoapAction: string;
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property SoapAction: string read GetSoapAction;
  end;

  TACBrNFSeProviderWebFisco = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
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

    {
    TagRaizNFSe := 'Nfe'; // Verificar
    TagRaizRps  := 'EnvNfe';
    }
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    with ConsultarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'ConsultaNfe';
    end;

    with CancelarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'CancelaNfe';
    end;

    with GerarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'GerarCancelaNfe';
    end;
  end;

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
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmGerar: URL := GerarNFSe;
        tmConsultarNFSe: URL := ConsultarNFSe;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmGerar: URL := GerarNFSe;
        tmConsultarNFSe: URL := ConsultarNFSe;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end;

  if URL <> '' then
    Result := TACBrNFSeXWebserviceWebFisco.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderWebFisco.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
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
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr);
    AErro.Correcao := '';

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;
  end;
end;

procedure TACBrNFSeProviderWebFisco.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Nota: NotaFiscal;
  IdAttr, ListaRps, xRps: string;
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
    AErro.Descricao := 'Conjunto de RPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' RPS)' +
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

    if EstaVazio(Nota.XMLAssinado) then
    begin
      Nota.GerarXML;
      if ConfigAssinar.Rps or ConfigAssinar.RpsGerarNFSe then
      begin
        Nota.XMLOriginal := FAOwner.SSL.Assinar(ConverteXMLtoUTF8(Nota.XMLOriginal), ConfigMsgDados.XmlRps.DocElemento,
                                                ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
      end;
    end;

    if FAOwner.Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(Nota.NomeArqRps) then
        TACBrNFSeX(FAOwner).Gravar(Nota.NomeArqRps, Nota.XMLOriginal)
      else
      begin
        Nota.NomeArqRps := Nota.CalcularNomeArquivoCompleto(Nota.NomeArqRps, '');
        TACBrNFSeX(FAOwner).Gravar(Nota.NomeArqRps, Nota.XMLOriginal);
      end;
    end;

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);

    ListaRps := ListaRps + xRps;
  end;

  Response.XmlEnvio := ListaRps;
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'okk');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.Find('okk');

      if AuxNode <> nil then
      begin
        {
        AuxNodeChave := AuxNode.Childrens.Find('ChaveRPS');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('InscricaoPrestador'), tcStr);
            SerieRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('SerieRPS'), tcStr);
            NumeroRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('NumeroRPS'), tcStr);
          end;
        end;
        }
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderWebFisco.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.Tipo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod114;
    AErro.Descricao := Desc114;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  Response.XmlEnvio := '<ConsultaNfe>' +
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
                           Response.InfConsultaNFSe.Tipo +
                         '</tipo>' +
                       '</ConsultaNfe>';
end;

procedure TACBrNFSeProviderWebFisco.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode{, AuxNode}: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
//  i: Integer;
//  NumRps: String;
//  ANota: NotaFiscal;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'okk');

      Response.Sucesso := (Response.Erros.Count = 0);

      {
      AuxNode := ANode.Childrens.Find('okk');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
        end;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFe');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];
        AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');
        NumRps := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end;
      }
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<CancelaNfe>' +
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
                           Response.InfCancelamento.SerieNFSe +
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'okk');

      Response.Sucesso := (Response.Erros.Count = 0);

      {
      AuxNode := ANode.Childrens.Find('okk');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
        end;
      end;
      }
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceWebFisco }

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

  Result := Executar('https://www.webfiscotecnologia.com.br/issqn/wservice/' + SoapAction,
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

  Result := Executar('https://www.webfiscotecnologia.com.br/issqn/wservice/wsnfeconsultaxml.php/ConsultaNfe',
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

  Result := Executar('https://www.webfiscotecnologia.com.br/issqn/wservice/wsnfecancela.php/CancelaNfe',
                     Request,
                     ['return', 'item'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"']);
end;

end.
