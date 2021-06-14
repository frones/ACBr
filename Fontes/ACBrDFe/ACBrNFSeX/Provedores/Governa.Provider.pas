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

unit Governa.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceGoverna = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderGoverna = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

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
  Governa.GravarXml, Governa.LerXml;

{ TACBrNFSeProviderGoverna }

procedure TACBrNFSeProviderGoverna.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';

    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    {
    TagRaizNFSe := 'tcRps';  // Verificar.
    TagRaizRps  := 'tcRps';
    }
  end;

  SetXmlNameSpace('http://tempuri.org/');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderGoverna.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Governa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGoverna.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Governa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGoverna.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar: URL := Recepcionar;
        tmConsultarNFSePorRps: URL := ConsultarNFSeRps;
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
        tmRecepcionar: URL := Recepcionar;
        tmConsultarNFSePorRps: URL := ConsultarNFSeRps;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end;

  if URL <> '' then
    Result := TACBrNFSeXWebserviceGoverna.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderGoverna.ProcessarMensagemErros(
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
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('tsFlgEtt'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('tsDesOco'), tcStr);
    AErro.Correcao := '';

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;
  end;
end;

procedure TACBrNFSeProviderGoverna.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
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

    xRps := '<tcRps>' + SeparaDados(xRps, 'tcRps') + '</tcRps>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<tcLoteRps>' +
                          '<tsCodCadBic>' +
                            OnlyNumber(Emitente.InscMun) +
                          '</tsCodCadBic>' +
                          '<tsVrsArq>' +
                            ConfigGeral.Params1 +
                          '</tsVrsArq>' +
                          '<tsChvAcs>' +
                            OnlyNumber(Emitente.WSChaveAcesso) +
                          '</tsChvAcs>' +
                          ListaRps +
                       '</tcLoteRps>';
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
//  AuxNode, AuxNodeChave: TACBrXmlNode;
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaLoteRps');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);

          with InformacoesLote do
          begin
            NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.Find('NumeroLote'), tcStr);
            InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

            CPFCNPJRemetente := ProcessarConteudoXml(AuxNode.Childrens.Find('CPFCNPJRemetente'), tcStr);

            DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.Find('DataEnvioLote'), tcDatHor);
            QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.Find('QtdNotasProcessadas'), tcInt);
            TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.Find('TempoProcessamento'), tcInt);
            ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.Find('ValorTotalServico'), tcDe2);
          end;
        end;
      end;

      AuxNode := ANode.Childrens.Find('ChavesNFeRPS');

      if AuxNode <> nil then
      begin
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

        AuxNodeChave := AuxNode.Childrens.Find('ChaveNFe');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('InscricaoPrestador'), tcStr);
            Numero := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('NumeroNFe'), tcStr);
            CodigoVerificacao := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('CodigoVerificacao'), tcStr);
          end;
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

procedure TACBrNFSeProviderGoverna.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumRPS) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := Desc102;
    Exit;
  end;

  if EstaVazio(Response.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := Desc117;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<tcConsultaRPS>' +
                          '<tsCodCadBic>' +
                            OnlyNumber(Emitente.InscMun) +
                          '</tsCodCadBic>' +
                          '<tsVrsArq>' +
                            ConfigGeral.Params1 +
                          '</tsVrsArq>' +
                          '<tsChvAcs>' +
                            OnlyNumber(Emitente.WSChaveAcesso) +
                          '</tsChvAcs>' +
                          '<tcInfConsultaRPS>' +
                            '<tsNumRPS>' + Response.NumRPS + '</tsNumRPS>' +
                            '<tsCodVer>' + Response.CodVerificacao + '</tsCodVer>' +
                          '</tcInfConsultaRPS>' +
                       '</tcConsultaRPS>';
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaConsultaRPS');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'Erros', 'Erro');

        Response.Sucesso := (Response.Erros.Count = 0);

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


procedure TACBrNFSeProviderGoverna.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := Desc117;
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

  Response.XmlEnvio := '<tcLoteCancelamento>' +
                          '<tsCodCadBic>' +
                            OnlyNumber(Emitente.InscMun) +
                          '</tsCodCadBic>' +
                          '<tsVrsArq>' +
                            ConfigGeral.Params1 +
                          '</tsVrsArq>' +
                          '<tsChvAcs>' +
                            OnlyNumber(Emitente.WSChaveAcesso) +
                          '</tsChvAcs>' +
                          '<tcNotCan>' +
                            '<tsNumNot>' +
                              Response.InfCancelamento.NumeroNFSe +
                            '</tsNumNot>' +
                            '<tsCodVer>' +
                              Response.InfCancelamento.CodVerificacao +
                            '</tsCodVer>' +
                            '<tsDesMotCan>' +
                              Response.InfCancelamento.MotCancelamento +
                            '</tsDesMotCan>' +
                          '</tcNotCan>' +
                       '</tcLoteCancelamento>';
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoCancelaNFSe(
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaLoteCancelamento');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      AuxNode := ANode.Childrens.Find('Cabecalho');

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

{ TACBrNFSeXWebserviceGoverna }

function TACBrNFSeXWebserviceGoverna.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteRps>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarLoteRps>';

  Result := Executar('http://tempuri.org/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult', 'tcRetornoLoteRps'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceGoverna.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarConsultaRPS>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarConsultaRPS>';

  Result := Executar('http://tempuri.org/RecepcionarConsultaRPS', Request,
                     ['RecepcionarConsultaRPSResult', 'tcRetornoConsultaRPS'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceGoverna.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteNotasCanceladas>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarLoteNotasCanceladas>';

  Result := Executar('http://tempuri.org/RecepcionarLoteNotasCanceladas', Request,
                     ['RecepcionarLoteNotasCanceladasResult', 'tcRetornoLoteCancelamento'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

end.
