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

unit SmarAPD.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceSmarAPD = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderSmarAPD = class (TACBrNFSeProviderProprio)
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

  TACBrNFSeXWebserviceSmarAPDv203 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderSmarAPDv203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeXWebserviceSmarAPDv204 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderSmarAPDv204 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, SmarAPD.GravarXml, SmarAPD.LerXml;

{ TACBrNFSeProviderSmarAPD }

procedure TACBrNFSeProviderSmarAPD.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := 'id';
    ModoEnvio := meLoteAssincrono;
    {italo
    TagRaizNFSe := 'nfdok'; // Verificar
    TagRaizRps  := 'nfd';
    }
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    IncluirURI := False;
  end;

  SetXmlNameSpace('http://webservices.sil.com/');

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
      InfElemento := 'tbnfd';
      DocElemento := 'nfd';
    end;
  end;

  // Apesar de existir os nomes dos schemas até o momento não temos eles.
  with ConfigSchemas do
  begin
    Recepcionar := 'WSEntradaNfd.xsd';
    ConsultarLote := 'WSSaidaNfd.xsd';
    CancelarNFSe := 'WSEntradaCancelar.xsd';

    Validar := False;
  end;
end;

function TACBrNFSeProviderSmarAPD.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SmarAPD.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPD.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SmarAPD.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPD.CriarServiceClient(
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
        tmConsultarLote: URL := ConsultarLote;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end;

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSmarAPD.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderSmarAPD.ProcessarMensagemErros(
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

procedure TACBrNFSeProviderSmarAPD.PrepararEmitir(Response: TNFSeEmiteResponse);
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

    xRps := '<nfd>' + SeparaDados(xRps, 'nfd') + '</nfd>';

    ListaRps := ListaRps + xRps;
  end;

  Response.XmlEnvio := '<nfd>' + '<tbnfd>' + ListaRps + '</tbnfd>' + '</nfd>';
end;

procedure TACBrNFSeProviderSmarAPD.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode: TACBrXmlNode;
//  AuxNode, AuxNodeCPFCNPJ, AuxNodeChave: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'return');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.Find('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.Find('NumeroLote'), tcStr);
              InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.Find('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CPF'), tcStr);
              end;

              DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.Find('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.Find('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.Find('TempoProcessamento'), tcInt);
              ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.Find('ValorTotalServico'), tcDe2);
            end;
          end;
        end;
      end;

      AuxNode := ANode.Childrens.Find('ChaveNFeRPS');

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

procedure TACBrNFSeProviderSmarAPD.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Response.XmlEnvio := '<recibo>' + Response.Protocolo + '</recibo>';
end;

procedure TACBrNFSeProviderSmarAPD.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'return');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

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

procedure TACBrNFSeProviderSmarAPD.PrepararCancelaNFSe(
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

  Response.XmlEnvio := '<inscricaomunicipalemissor>' +
                         OnlyNumber(Emitente.InscMun) +
                       '</inscricaomunicipalemissor>' +
                       '<numeronf>' +
                         Response.InfCancelamento.NumeroNFSe +
                       '</numeronf>' +
                       '<motivocancelamento>' +
                         Response.InfCancelamento.MotCancelamento +
                       '</motivocancelamento>' +
                       '<datacancelamento>' +
                         FormatDateTime('dd/mm/yyyy', now) +
                       '</datacancelamento>';
end;

procedure TACBrNFSeProviderSmarAPD.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'return');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

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

{ TACBrNFSeXWebserviceSmarAPD }

function TACBrNFSeXWebserviceSmarAPD.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<cpfUsuario>' + Emitente.WSUser + '</cpfUsuario>' +
              '<hashSenha>' + Emitente.WSSenha + '</hashSenha>';
  end;
end;

function TACBrNFSeXWebserviceSmarAPD.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request, CodMun: string;
begin
  FPMsgOrig := AMSG;

  CodMun := IntToStr(TACBrNFSeX(FPDFeOwner).Configuracoes.Geral.CodigoMunicipio);

  Request := '<web:nfdEntrada>';
  Request := Request + DadosUsuario;
  Request := Request + '<codigoMunicipio>' + CodMun + '</codigoMunicipio>';
  Request := Request + AMSG;
  Request := Request + '</web:nfdEntrada>';

  Result := Executar('', Request,
                     ['return'],
                     ['xmlns:web="http://webservices.sil.com/"']);
end;

function TACBrNFSeXWebserviceSmarAPD.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request, IM: string;
begin
  FPMsgOrig := AMSG;

  IM := TACBrNFSeX(FPDFeOwner).Configuracoes.Geral.Emitente.InscMun;

  Request := '<web:nfdSaida>';
  Request := Request + DadosUsuario;
  Request := Request + '<inscricaoMunicipal>' + IM + '</inscricaoMunicipal>';
  Request := Request + AMSG;
  Request := Request + '</web:nfdSaida>';

  Result := Executar('', Request,
                     ['return'],
                     ['xmlns:web="http://webservices.sil.com/"']);
end;

function TACBrNFSeXWebserviceSmarAPD.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<web:nfdEntradaCancelar>';
  Request := Request + DadosUsuario;
  Request := Request + '<nfd>' + AMSG + '</nfd>';
  Request := Request + '</web:nfdEntradaCancelar>';

  Result := Executar('', Request,
                     ['return', 'outputXML'],
                     ['xmlns:web="http://webservices.sil.com/"']);
end;

{ TACBrNFSeProviderSmarAPDv203 }

procedure TACBrNFSeProviderSmarAPDv203.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;

    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;
end;

function TACBrNFSeProviderSmarAPDv203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SmarAPDv203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPDv203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SmarAPDv203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPDv203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, SubstituirNFSe);
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
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv203.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceSmarAPDv203 }

function TACBrNFSeXWebserviceSmarAPDv203.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:recepcionarLoteRps>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:recepcionarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:recepcionarLoteRpsSincrono>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:recepcionarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['return', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:gerarNfse>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:gerarNfse>';

  Result := Executar('', Request,
                     ['return', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarLoteRps>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarLoteRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfsePorFaixa>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfsePorFaixa>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfsePorRps>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfsePorRps>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfseServicoPrestado>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfseServicoPrestado>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:consultarNfseServicoTomado>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:consultarNfseServicoTomado>';

  Result := Executar('', Request,
                     ['return', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:cancelarNfse>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:cancelarNfse>';

  Result := Executar('', Request,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv203.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:substituirNfse>';
  Request := Request + '<xml>' + IncluirCDATA(AMSG) + '</xml>';
  Request := Request + '</nfse:substituirNfse>';

  Result := Executar('', Request,
                     ['return', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

{ TACBrNFSeProviderSmarAPDv204 }

procedure TACBrNFSeProviderSmarAPDv204.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
  end;

  with ConfigMsgDados do
  begin
    GerarPrestadorLoteRps := True;

    DadosCabecalho := GetCabecalho('');
  end;
end;

function TACBrNFSeProviderSmarAPDv204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SmarAPDv204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPDv204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SmarAPDv204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmarAPDv204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, SubstituirNFSe);
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
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, GerarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceSmarAPDv204.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceSmarAPDv204 }

function TACBrNFSeXWebserviceSmarAPDv204.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', Request,
                     ['outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorFaixa', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRpsRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', Request,
                     ['outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', Request,
                     ['outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceSmarAPDv204.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', Request,
                     ['outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

end.
