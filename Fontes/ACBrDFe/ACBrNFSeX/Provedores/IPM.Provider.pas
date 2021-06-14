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

unit IPM.Provider;

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
  TACBrNFSeXWebserviceIPM = class(TACBrNFSeXWebserviceRest)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function TesteEnvio(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderIPM = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

  TACBrNFSeXWebserviceIPMV110 = class(TACBrNFSeXWebserviceIPM)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  end;

  TACBrNFSeProviderIPMV110 = class (TACBrNFSeProviderIPM)
  protected
    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeXWebserviceIPMa = class(TACBrNFSeXWebserviceIPM)
  public

  end;

  TACBrNFSeProviderIPMa = class (TACBrNFSeProviderIPM)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, synacode,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  IPM.GravarXml, IPM.LerXml;

{ TACBrNFSeProviderIPM }

procedure TACBrNFSeProviderIPM.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    {
    TagRaizNFSe := 'nfse';
    TagRaizRps  := 'nfse';
    }
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'nfse';
      DocElemento := 'nfse';
    end;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderIPM.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPM.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPM.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM.CriarServiceClient(
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
        tmTeste: URL := TesteEnvio;
        tmConsultarLote: URL := ConsultarLote;
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
        tmTeste: URL := TesteEnvio;
        tmConsultarLote: URL := ConsultarLote;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end;

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPM.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderIPM.ProcessarMensagemErros(
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

procedure TACBrNFSeProviderIPM.PrepararEmitir(Response: TNFSeEmiteResponse);
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

procedure TACBrNFSeProviderIPM.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode, AuxNodeChave: TACBrXmlNode;
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

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'Erros', 'Erro');

        Response.Sucesso := (Response.Erros.Count = 0);

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

procedure TACBrNFSeProviderIPM.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Response.XmlEnvio := '<nfse>' +
                         '<pesquisa>' +
                           '<codigo_autenticidade>' +
                             Response.Protocolo +
                           '</codigo_autenticidade>' +
                           '<numero>' +
                             '' +
                           '</numero>' +
                           '<serie>' +
                             '' +
                           '</serie>' +
                           '<cadastro>' +
                             '' +
                           '</cadastro>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        Response.InfRetorno.Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('ListaNFSe');
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

        if AuxNode <> nil then
        begin
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

procedure TACBrNFSeProviderIPM.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<nfse>' +
                         '<nf>' +
                           '<numero>' +
                             Response.InfCancelamento.NumeroNFSe +
                           '</numero>' +
                           '<situacao>' +
                             'C' +
                           '</situacao>' +
                           '<observacao>' +
                             Response.InfCancelamento.MotCancelamento +
                           '</observacao>' +
                         '</nf>' +
                         '<Prestador>' +
                           '<cpfcnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</cpfcnpj>' +
                           '<cidade>' +
                             CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                           '</cidade>' +
                         '</Prestador>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
        end;
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

{ TACBrNFSeXWebserviceIPM }

function TACBrNFSeXWebserviceIPM.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request,
                     ['enviarReturn', 'ReqEnvioLoteRPS'],
                     ['']);
end;

function TACBrNFSeXWebserviceIPM.TesteEnvio(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request,
                     ['testeEnviarReturn', 'RetornoEnvioLoteRPS'],
                     ['']);
end;

function TACBrNFSeXWebserviceIPM.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request,
                     ['consultarLoteReturn', 'RetornoConsultaLote'],
                     ['']);
end;

function TACBrNFSeXWebserviceIPM.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request,
                     ['cancelarReturn', 'RetornoCancelamentoNFSe'],
                     ['']);
end;

{ TACBrNFSeProviderIPMa }

procedure TACBrNFSeProviderIPMa.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
  end;
end;

function TACBrNFSeProviderIPMa.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPMa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMa.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPMa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMa.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, RecepcionarSincrono);
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
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceIPMa.Create(FAOwner, AMetodo, RecepcionarSincrono);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceIPMV110 }

procedure TACBrNFSeXWebserviceIPMV110.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
    Auth := 'Basic ' + string(EncodeBase64(AnsiString(WSUser + ':' + WSSenha)));

  aHeaderReq.AddHeader('Authorization', Auth);
end;

{ TACBrNFSeProviderIPMV110 }

function TACBrNFSeProviderIPMV110.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPMV110.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV110.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPMV110.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV110.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, RecepcionarSincrono);
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
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, RecepcionarSincrono);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

end.
