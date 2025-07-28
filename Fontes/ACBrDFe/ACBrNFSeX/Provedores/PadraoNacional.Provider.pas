{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit PadraoNacional.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrJSON, ACBrDFeSSL,
  ACBrXmlBase, 
  ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebservicePadraoNacional = class(TACBrNFSeXWebserviceRest)
  public
    function GerarNFSe(const ACabecalho, AMSG: string): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: string): string; override;
    function ConsultarNFSePorChave(const ACabecalho, AMSG: string): string; override;
    function EnviarEvento(const ACabecalho, AMSG: string): string; override;
    function ConsultarEvento(const ACabecalho, AMSG: string): string; override;
    function ConsultarDFe(const ACabecalho, AMSG: string): string; override;
    function ConsultarParam(const ACabecalho, AMSG: string): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderPadraoNacional = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
    FpChave: string;
    FpTipoConsultaEvento: Integer;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSeporChave(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporChave(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararEnviarEvento(Response: TNFSeEnviarEventoResponse); override;
    procedure TratarRetornoEnviarEvento(Response: TNFSeEnviarEventoResponse); override;

    procedure PrepararConsultarEvento(Response: TNFSeConsultarEventoResponse); override;
    procedure TratarRetornoConsultarEvento(Response: TNFSeConsultarEventoResponse); override;

    procedure PrepararConsultarDFe(Response: TNFSeConsultarDFeResponse); override;
    procedure TratarRetornoConsultarDFe(Response: TNFSeConsultarDFeResponse); override;

    procedure PrepararConsultarParam(Response: TNFSeConsultarParamResponse); override;
    procedure TratarRetornoConsultarParam(Response: TNFSeConsultarParamResponse); override;

    procedure ProcessarMensagemDeErros(LJson: TACBrJSONObject;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'Erros');

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  public
    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; override;
  end;

implementation

uses
  synacode,
  ACBrDFeException, ACBrCompress,
  ACBrUtil.DateTime,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrNFSeX, ACBrNFSeXConsts, ACBrNFSeXConfiguracoes,
  PadraoNacional.GravarXml, PadraoNacional.LerXml;

{ TACBrNFSeProviderPadraoNacional }

procedure TACBrNFSeProviderPadraoNacional.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meUnitario;
    ConsultaLote := False;
    FormatoArqEnvio := tfaJson;
    FormatoArqRetorno := tfaJson;
    FormatoArqEnvioSoap := tfaJson;
    FormatoArqRetornoSoap := tfaJson;

    ServicosDisponibilizados.EnviarUnitario := True;
    ServicosDisponibilizados.ConsultarNfseChave := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.EnviarEvento := True;
    ServicosDisponibilizados.ConsultarEvento := True;
    ServicosDisponibilizados.ConsultarDFe := True;
    ServicosDisponibilizados.ConsultarParam := True;

    Particularidades.AtendeReformaTributaria := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace('http://www.sped.fazenda.gov.br/nfse');

  with ConfigMsgDados do
  begin
    UsarNumLoteConsLote := False;

    DadosCabecalho := GetCabecalho('');

    XmlRps.InfElemento := 'infDPS';
    XmlRps.DocElemento := 'DPS';

    EnviarEvento.InfElemento := 'infPedReg';
    EnviarEvento.DocElemento := 'pedRegEvento';
  end;

  with ConfigAssinar do
  begin
    RpsGerarNFSe := True;
    EnviarEvento := True;
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    GerarNFSe := 'DPS_v1.00.xsd';
    ConsultarNFSe := 'DPS_v1.00.xsd';
    ConsultarNFSeRps := 'DPS_v1.00.xsd';
    EnviarEvento := 'pedRegEvento_v1.00.xsd';
    ConsultarEvento := 'DPS_v1.00.xsd';
  end;
end;

function TACBrNFSeProviderPadraoNacional.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_PadraoNacional.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderPadraoNacional.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_PadraoNacional.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderPadraoNacional.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebservicePadraoNacional.Create(FAOwner, AMetodo, URL, FpMethod);
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderPadraoNacional.ProcessarMensagemDeErros(
  LJson: TACBrJSONObject; Response: TNFSeWebserviceResponse;
  const AListTag: string);
var
  JSonLista: TACBrJSONArray;
  JSon: TACBrJSONObject;

  procedure AdicionaCollectionItem(JSonItem: TACBrJSONObject; Collection: TNFSeEventoCollection);
  var
    AItem: TNFSeEventoCollectionItem;
    Codigo: string;
  begin
    Codigo := JSonItem.AsString['Codigo'];

    if Codigo <> '' then
    begin
      AItem := Collection.New;
      AItem.Codigo := Codigo;
      AItem.Descricao := JSonItem.AsString['Descricao'];
      AItem.Correcao := JSonItem.AsString['Complemento'];
    end
    else
    begin
      Codigo := JSonItem.AsString['codigo'];

      if Codigo <> '' then
      begin
        AItem := Collection.New;
        AItem.Codigo := Codigo;
        AItem.Descricao := JSonItem.AsString['descricao'];
        AItem.Correcao := JSonItem.AsString['complemento'];
      end;
    end;
  end;

  procedure LerListaErrosAlertas(jsLista: TACBrJSONArray; Collection: TNFSeEventoCollection);
  var
    i: Integer;
  begin
    for i := 0 to jsLista.Count-1 do
    begin
      JSon := jsLista.ItemAsJSONObject[i];

      AdicionaCollectionItem(JSon, Collection);
    end;
  end;

  procedure VerificaSeObjetoOuArray(aNome: string; Collection: TNFSeEventoCollection);
  begin
    // Verifica se no retorno contem um objeto ou array
    if LJson.IsJSONArray(aNome) then
    begin
      JSonLista := LJson.AsJSONArray[aNome];

      if JSonLista.Count > 0 then
        LerListaErrosAlertas(JSonLista, Collection);
    end
    else
    begin
      JSon := LJson.AsJSONObject[aNome];

      if JSon <> nil then
        AdicionaCollectionItem(JSon, Collection);
    end;
  end;
begin
  // Verifica se no retorno contem a lista de Erros
  VerificaSeObjetoOuArray(AListTag, Response.Erros);
  // Verifica se no retorno contem a lista de erros
  VerificaSeObjetoOuArray('erros', Response.Erros);
  // Verifica se no retorno contem a lista de erro
  VerificaSeObjetoOuArray('erro', Response.Erros);
  // Verifica se no retorno contem a lista de Alertas
  VerificaSeObjetoOuArray('Alertas', Response.Alertas);
  // Verifica se no retorno contem a lista de Alertas
  VerificaSeObjetoOuArray('alertas', Response.Alertas);
end;

procedure TACBrNFSeProviderPadraoNacional.PrepararEmitir(
  Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Nota: TNotaFiscal;
  IdAttr, ListaDps: string;
  I: Integer;
begin
  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := ACBrStr(Desc002);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := ACBrStr('Conjunto de DPS transmitidos (m�ximo de ' +
                       IntToStr(Response.MaxRps) + ' DPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
  end;

  if Response.Erros.Count > 0 then Exit;

  ListaDps := '';

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono])) or
       (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);

      Response.ArquivoEnvio := Nota.XmlRps;
    end;

    SalvarXmlRps(Nota);

    ListaDps := ListaDps + Nota.XmlRps;
  end;

  Response.ArquivoEnvio := ListaDps;
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NFSeXml: string;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  NumNFSe, NumDps, CodVerif: string;
  DataAut: TDateTime;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['dataHoraProcessamento'];
      Response.idNota := Document.AsString['idDPS'];

      if Response.idNota = '' then
        Response.idNota := Document.AsString['idDps'];

      Response.Link := Document.AsString['chaveAcesso'];
      NFSeXml := Document.AsString['nfseXmlGZipB64'];

      if NFSeXml <> '' then
        NFSeXml := DeCompress(DecodeBase64(NFSeXml));

      DocumentXml := TACBrXmlDocument.Create;

      try
        try
          if NFSeXml = '' then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit
          end;

          DocumentXml.LoadFromXml(NFSeXml);

          ANode := DocumentXml.Root.Childrens.FindAnyNs('infNFSe');

          CodVerif := OnlyNumber(ObterConteudoTag(ANode.Attributes.Items['Id']));
          NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('nNFSe'), tcStr);
          DataAut := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhProc'), tcDatHor);

          ANode := ANode.Childrens.FindAnyNs('DPS');
          ANode := ANode.Childrens.FindAnyNs('infDPS');
          NumDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDPS'), tcStr);

          with Response do
          begin
            NumeroNota := NumNFSe;
            Data := DataAut;
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumDps);

          ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
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
        FreeAndNil(DocumentXml);
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod126;
    AErro.Descricao := ACBrStr(Desc126);
    Exit;
  end;

  FpPath := '/dps/' + Response.NumeroRps;
  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['dataHoraProcessamento'];
      Response.idNota := Document.AsString['chaveAcesso'];
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultaNFSeporChave(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.InfConsultaNFSe.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := ACBrStr(Desc118);
    Exit;
  end;

  Response.Metodo := tmConsultarNFSePorChave;

  if Response.InfConsultaNFSe.tpRetorno = trXml then
    FpPath := '/nfse/' + Response.InfConsultaNFSe.ChaveNFSe
  else
    FpPath := '/danfse/' + Response.InfConsultaNFSe.ChaveNFSe;

  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultaNFSeporChave(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NFSeXml: string;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  NumNFSe, NumDps: string;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  if Response.InfConsultaNFSe.tpRetorno = trXml then
  begin
    Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

    try
      try
        ProcessarMensagemDeErros(Document, Response);
        Response.Sucesso := (Response.Erros.Count = 0);

        Response.Data := Document.AsISODateTime['dataHoraProcessamento'];

        NFSeXml := Document.AsString['nfseXmlGZipB64'];

        if NFSeXml <> '' then
          NFSeXml := DeCompress(DecodeBase64(NFSeXml));

        DocumentXml := TACBrXmlDocument.Create;

        try
          try
            if NFSeXml = '' then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := ACBrStr(Desc203);
              Exit
            end;

            DocumentXml.LoadFromXml(NFSeXml);

            ANode := DocumentXml.Root.Childrens.FindAnyNs('infNFSe');

            NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('nNFSe'), tcStr);
            ANode := ANode.Childrens.FindAnyNs('DPS');
            ANode := ANode.Childrens.FindAnyNs('infDPS');
            NumDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDPS'), tcStr);

            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumDps);

            ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
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
          FreeAndNil(DocumentXml);
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
  end
  else
  begin
    SalvarPDFNfse(Response.InfConsultaNFSe.ChaveNFSe, Response.ArquivoRetorno);
  end;
end;

procedure TACBrNFSeProviderPadraoNacional.PrepararEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  xEvento, xUF, xAutorEvento, IdAttr, xCamposEvento: string;
begin
  with Response.InfEvento.pedRegEvento do
  begin
    if chNFSe = '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod004;
      AErro.Descricao := ACBrStr(Desc004);
    end;

    if Response.Erros.Count > 0 then Exit;

    xUF := TACBrNFSeX(FAOwner).Configuracoes.WebServices.UF;

    if Length(TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente.CNPJ) < 14 then
    begin
      xAutorEvento := '<CPFAutor>' +
                        TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente.CNPJ +
                      '</CPFAutor>';
    end
    else
    begin
      xAutorEvento := '<CNPJAutor>' +
                        TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente.CNPJ +
                      '</CNPJAutor>';
    end;

    ID := chNFSe + OnlyNumber(tpEventoToStr(tpEvento)) +
              FormatFloat('000', nPedRegEvento);

    IdAttr := 'Id="' + 'PRE' + ID + '"';

    case tpEvento of
      teCancelamento:
        xCamposEvento := '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                         '<xMotivo>' + xMotivo + '</xMotivo>';

      teCancelamentoSubstituicao:
        xCamposEvento := '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                         '<xMotivo>' + xMotivo + '</xMotivo>' +
                         '<chSubstituta>' + chSubstituta + '</chSubstituta>';

      teAnaliseParaCancelamento:
        xCamposEvento := '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                         '<xMotivo>' + xMotivo + '</xMotivo>';

      teRejeicaoPrestador:
        xCamposEvento := '<infRej>' +
                           '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                           '<xMotivo>' + xMotivo + '</xMotivo>' +
                         '</infRej>';

      teRejeicaoTomador:
        xCamposEvento := '<infRej>' +
                           '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                           '<xMotivo>' + xMotivo + '</xMotivo>' +
                         '</infRej>';

      teRejeicaoIntermediario:
        xCamposEvento := '<infRej>' +
                           '<cMotivo>' + IntToStr(cMotivo) + '</cMotivo>' +
                           '<xMotivo>' + xMotivo + '</xMotivo>' +
                         '</infRej>';
    else
      // teConfirmacaoPrestador, teConfirmacaoTomador,
      // ConfirmacaoIntermediario
      xCamposEvento := '';
    end;

    xEvento := '<pedRegEvento xmlns="' + ConfigMsgDados.EnviarEvento.xmlns +
                           '" versao="' + ConfigWebServices.VersaoAtrib + '">' +
                 '<infPedReg ' + IdAttr + '>' +
                   '<tpAmb>' + IntToStr(tpAmb) + '</tpAmb>' +
                   '<verAplic>' + verAplic + '</verAplic>' +
                   '<dhEvento>' +
                     FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', dhEvento) +
                     GetUTC(xUF, dhEvento) +
                   '</dhEvento>' +
                   xAutorEvento +
                   '<chNFSe>' + chNFSe + '</chNFSe>' +
                   '<nPedRegEvento>' +
                     FormatFloat('000', nPedRegEvento) +
                   '</nPedRegEvento>' +
                   '<' + tpEventoToStr(tpEvento) + '>' +
                     '<xDesc>' + tpEventoToDesc(tpEvento) + '</xDesc>' +
                     xCamposEvento +
                   '</' + tpEventoToStr(tpEvento) + '>' +
                 '</infPedReg>' +
               '</pedRegEvento>';

    xEvento := ConverteXMLtoUTF8(xEvento);
    xEvento := ChangeLineBreak(xEvento, '');

    Response.ArquivoEnvio := xEvento;
    FpChave := chNFSe;

    SalvarXmlEvento(ID + '-pedRegEvento', Response.ArquivoEnvio, Response.PathNome);
  end;
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  EventoXml, IDEvento: string;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  Ok: Boolean;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['dataHoraProcessamento'];

      EventoXml := Document.AsString['eventoXmlGZipB64'];

      if EventoXml <> '' then
      begin
        EventoXml := DeCompress(DecodeBase64(EventoXml));

        DocumentXml := TACBrXmlDocument.Create;

        try
          try
            if EventoXml = '' then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod211;
              AErro.Descricao := ACBrStr(Desc211);
              Exit
            end;

            DocumentXml.LoadFromXml(EventoXml);

            ANode := DocumentXml.Root.Childrens.FindAnyNs('infEvento');

            IDEvento := OnlyNumber(ObterConteudoTag(ANode.Attributes.Items['Id']));

            Response.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
            Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhProc'), tcDatHor);
            Response.idEvento := IDEvento;
            Response.tpEvento := StrTotpEvento(Ok, Copy(IDEvento, 51, 6));

            case Response.tpEvento of
              teCancelamento:
                begin
                  Response.SucessoCanc := True;
                  Response.DescSituacao := 'Nota Cancelada';
                end
            else
              begin
                Response.SucessoCanc := False;
                Response.DescSituacao := '';
              end;
            end;

            ANode := ANode.Childrens.FindAnyNs('pedRegEvento');
            ANode := ANode.Childrens.FindAnyNs('infPedReg');

            Response.idNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFSe'), tcStr);

            SalvarXmlEvento(IDEvento + '-procEveNFSe', EventoXml, Response.PathNome);
          except
            on E:Exception do
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod999;
              AErro.Descricao := ACBrStr(Desc999 + E.Message);
            end;
          end;
        finally
          FreeAndNil(DocumentXml);
        end;
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := ACBrStr(Desc118);
    Exit;
  end;

  if Response.nSeqEvento = 0 then
  begin
    if Response.tpEvento = teNenhum then
    begin
      FpTipoConsultaEvento := 1;
      FpPath := '/nfse/' + Response.ChaveNFSe + '/eventos';
    end
    else
    begin
      FpTipoConsultaEvento := 2;
      FpPath := '/nfse/' + Response.ChaveNFSe + '/eventos/' +
                OnlyNumber(tpEventoToStr(Response.tpEvento));
    end;
  end
  else
  begin
    FpTipoConsultaEvento := 3;
    FpPath := '/nfse/' + Response.ChaveNFSe + '/eventos/' +
              OnlyNumber(tpEventoToStr(Response.tpEvento)) + '/' +
              FormatFloat('000', Response.nSeqEvento);
  end;

  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
var
  Document, JSon: TACBrJSONObject;
  JSonLoteEventos: TACBrJSONArray;
  i: Integer;
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  IDEvento, TipoEvento, ArquivoXml: string;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  Ok: Boolean;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['dataHoraProcessamento'];

      JSonLoteEventos := Document.AsJSONArray['eventos'];

      for i := 0 to JSonLoteEventos.Count-1 do
      begin
        JSon := JSonLoteEventos.ItemAsJSONObject[i];

        AResumo := Response.Resumos.New;
        AResumo.ChaveDFe := JSon.AsString['chaveAcesso'];
        TipoEvento := 'e' + JSon.AsString['tipoEvento'];
        AResumo.TipoDoc := 'Evento de ' +
                           tpEventoToDesc(StrTotpEvento(Ok, TipoEvento));

        ArquivoXml := JSon.AsString['arquivoXml'];

        if FpTipoConsultaEvento = 3 then
          ArquivoXml := DeCompress(DecodeBase64(DecodeBase64(ArquivoXml)))
        else
          ArquivoXml := DeCompress(DecodeBase64(ArquivoXml));

        if ArquivoXml = '' then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit
        end;

        DocumentXml := TACBrXmlDocument.Create;

        try
          try
            DocumentXml.LoadFromXml(ArquivoXml);

            ANode := DocumentXml.Root.Childrens.FindAnyNs('infEvento');

            IDEvento := OnlyNumber(ObterConteudoTag(ANode.Attributes.Items['Id']));

            Response.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
            Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhProc'), tcDatHor);
            Response.idEvento := IDEvento;
            Response.tpEvento := StrTotpEvento(Ok, Copy(IDEvento, 51, 6));

            ANode := ANode.Childrens.FindAnyNs('pedRegEvento');
            ANode := ANode.Childrens.FindAnyNs('infPedReg');

            Response.idNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFSe'), tcStr);

            SalvarXmlEvento(IDEvento + '-procEveNFSe', ArquivoXml, Response.PathNome);
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  if Response.ChaveNFSe <> '' then
    FpPath := '/NFSe/' + Response.ChaveNFSe + '/Eventos'
  else
    FpPath := '/DFe/' + IntToStr(Response.NSU);

  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  Document, JSon: TACBrJSONObject;
  JSonLoteDFe: TACBrJSONArray;
  i: Integer;
  CnpjCpfDps, SerieDps, TipoDoc, ArquivoXml, NumNFSe, NumDps, IDEvento: string;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  ANota: TNotaFiscal;
  Ok: Boolean;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response, 'Erros');
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['DataHoraProcessamento'];
      Response.Situacao := Document.AsString['StatusProcessamento'];

      if Response.Situacao = 'DOCUMENTOS_LOCALIZADOS' then
      begin

        JSonLoteDFe := Document.AsJSONArray['LoteDFe'];

        for i := 0 to JSonLoteDFe.Count-1 do
        begin
          JSon := JSonLoteDFe.ItemAsJSONObject[i];

          AResumo := Response.Resumos.New;
          AResumo.NSU := JSon.AsInteger['NSU'];
          AResumo.ChaveDFe := JSon.AsString['ChaveAcesso'];
          TipoDoc := JSon.AsString['TipoDocumento'];
          AResumo.TipoDoc := TipoDoc;
          AResumo.TipoEvento := JSon.AsString['TipoEvento'];

          ArquivoXml := JSon.AsString['ArquivoXml'];
          ArquivoXml := DeCompress(DecodeBase64(ArquivoXml));

          if ArquivoXml = '' then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit
          end;

          if TipoDoc = 'NFSE' then
          begin
            DocumentXml := TACBrXmlDocument.Create;

            try
              try
                DocumentXml.LoadFromXml(ArquivoXml);

                ANode := DocumentXml.Root.Childrens.FindAnyNs('infNFSe');

                NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('nNFSe'), tcStr);
                ANode := ANode.Childrens.FindAnyNs('DPS');
                ANode := ANode.Childrens.FindAnyNs('infDPS');
                NumDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDPS'), tcStr);
                SerieDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie'), tcStr);

                ANode := ANode.Childrens.FindAnyNs('prest');

                CnpjCpfDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);

                if CnpjCpfDps = '' then
                  CnpjCpfDps := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByCnpjCpfSerieRps(CnpjCpfDps, SerieDps, NumDps);

                ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
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
              FreeAndNil(DocumentXml);
            end;
          end
          else
          begin
            DocumentXml := TACBrXmlDocument.Create;

            try
              try
                DocumentXml.LoadFromXml(ArquivoXml);

                ANode := DocumentXml.Root.Childrens.FindAnyNs('infEvento');

                IDEvento := OnlyNumber(ObterConteudoTag(ANode.Attributes.Items['Id']));

                Response.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
                Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhProc'), tcDatHor);
                Response.idEvento := IDEvento;
                Response.tpEvento := StrTotpEvento(Ok, Copy(IDEvento, 51, 6));

                ANode := ANode.Childrens.FindAnyNs('pedRegEvento');
                ANode := ANode.Childrens.FindAnyNs('infPedReg');

                Response.idNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFSe'), tcStr);

                SalvarXmlEvento(IDEvento + '-procEveNFSe', ArquivoXml, Response.PathNome);
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
          end;
        end;
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultarParam(
  Response: TNFSeConsultarParamResponse);
var
  CodSer, Compet, NumBenef: string;
begin
  FpPath := '/parametros_municipais/' + IntToStr(Response.CodigoMunicipio);

  CodSer := OnlyNumber(Response.CodigoServico);
  CodSer := Copy(CodSer, 1, 2) + '.' + Copy(CodSer, 3, 2) + '.' +
            Copy(CodSer, 5, 2) + '.' + Copy(CodSer, 7, 3);
  Compet := FormatDateTime('MM-DD-YYYY', Response.Competencia);
  NumBenef := Response.NumeroBeneficio;

  case Response.tpParamMunic of
    pmAliquota:
      FpPath := FpPath + '/' + CodSer + '/' + Compet + '/aliquota';
    pmHistoricoAliquota:
      FpPath := FpPath + '/' + CodSer + '/historicoaliquotas';
    pmConvenio:
      FpPath := FpPath + '/convenio';
    pmRegimesEspeciais:
      FpPath := FpPath + '/' + CodSer + '/' + Compet + '/regimes_especiais';
    pmRetencoes:
      FpPath := FpPath + '/' + Compet + '/retencoes';
    pmBeneficios:
      FpPath := FpPath + '/' + NumBenef + '/' + Compet + '/beneficio';
  else
    FpPath := '';
  end;

  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultarParam(
  Response: TNFSeConsultarParamResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Document, JSon, JsonE, JsonI: TACBrJSONObject;
  JSonItem, JSonRetMun: TACBrJSONArray;
  i, j: Integer;
  xCodServ: string;

  procedure LerHistorico(Json: TACBrJSONObject);
  var
    JSonItem: TACBrJSONArray;
    i: Integer;
  begin
    JSonItem := Json.AsJSONArray['hist'];

    for i := 0 to JSonItem.Count-1 do
    begin
      JsonI := JSonItem.ItemAsJSONObject[i];

      Response.Parametros.Add('Data Inicial: ' +
        DateTimeToStr(JsonI.AsISODate['dtIni']));

      Response.Parametros.Add('Data Final: ' +
        DateTimeToStr(JsonI.AsISODate['dtFim']));
    end;
  end;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response, 'Erros');
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsDateTimeBr['dataHoraProcessamento'];

      Response.Parametros.Clear;
      Response.Parametros.Add('Mensagem: ' + Document.AsString['mensagem']);

      // Retorno da Consulta Par�metros Municipais - Aliquotas
      // Retorno da Consulta Par�metros Municipais - Hist�rico de Aliquotas
      JSon := Document.AsJSONObject['aliquotas'];

      if Json <> nil then
      begin
        xCodServ := OnlyNumber(Response.CodigoServico);
        xCodServ := Copy(xCodServ, 1, 2) + '.' + Copy(xCodServ, 3, 2) + '.' +
                    Copy(xCodServ, 5, 2) + '.' + Copy(xCodServ, 7, 3);

        JSonItem := Json.AsJSONArray[xCodServ];

        for i := 0 to JSonItem.Count-1 do
        begin
          JsonI := JSonItem.ItemAsJSONObject[i];

          Response.Parametros.Add(ACBrStr('Al�quota: ' +
            JsonI.AsString['aliq']));

          Response.Parametros.Add('Data Inicial: ' +
            DateTimeToStr(JsonI.AsISODate['dtIni']));
        end;
      end;

      // Retorno da Consulta Par�metros Municipais - Convenio
      JSon := Document.AsJSONObject['parametrosConvenio'];

      if Json <> nil then
      begin
        Response.Parametros.Add('Aderente ao Ambiente Nacional: ' +
          JSon.AsString['aderenteAmbienteNacional']);

        Response.Parametros.Add('Aderente ao Emissor Nacional: ' +
          JSon.AsString['aderenteEmissorNacional']);

        Response.Parametros.Add('Aderente ao MAN: ' +
          JSon.AsString['aderenteMAN']);

        Response.Parametros.Add('orig Cad: ' +
          JSon.AsString['origCad']);

        Response.Parametros.Add(ACBrStr('Permite Aproveitameto de Cr�ditos: ' +
          JSon.AsString['permiteAproveitametoDeCreditos']));
      end;

      // Retorno da Consulta Par�metros Municipais - Reten��es
      JSon := Document.AsJSONObject['retencoes'];

      if Json <> nil then
      begin
        JsonE := JSon.AsJSONObject['art6'];

        Response.Parametros.Add('Habilitado: ' +
          JsonE.AsString['habilitado']);

        LerHistorico(JsonE);

        JSonRetMun := JSon.AsJSONArray['retMun'];

        for i := 0 to JSonRetMun.Count-1 do
        begin
          JsonI := JSonRetMun.ItemAsJSONObject[i];

          Response.Parametros.Add(ACBrStr('Descri��o: ' +
            JsonI.AsString['desc']));

          Response.Parametros.Add('Data Inicial: ' +
            DateTimeToStr(JsonI.AsISODate['dtIni']));

          Response.Parametros.Add('Data Final: ' +
            DateTimeToStr(JsonI.AsISODate['dtFim']));

          // Falta ler o tpRet

          JSonItem := JsonI.AsJSONArray['serv'];

          for j := 0 to JSonItem.Count-1 do
          begin
            JsonI := JSonItem.ItemAsJSONObject[j];

            Response.Parametros.Add(ACBrStr('C�digo: ' +
              JsonI.AsString['codigo']));

            LerHistorico(JsonI);
          end;
        end;

        JSonItem := Json.AsJSONArray['respTrib'];

        for i := 0 to JSonItem.Count-1 do
        begin
          JsonI := JSonItem.ItemAsJSONObject[i];

          Response.Parametros.Add(ACBrStr('Tipo Inscri��o: ' +
            JsonI.AsString['tpInsc']));

          Response.Parametros.Add(ACBrStr('Inscri��o: ' +
            JsonI.AsString['insc']));

          LerHistorico(JsonI);
        end;
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

procedure TACBrNFSeProviderPadraoNacional.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
begin
  if aMetodo in [tmGerar, tmEnviarEvento] then
  begin
    inherited ValidarSchema(Response, aMetodo);

    Response.ArquivoEnvio := ChangeLineBreak(Response.ArquivoEnvio, '');
    Response.ArquivoEnvio := EncodeBase64(GZipCompress(Response.ArquivoEnvio));

    case aMetodo of
      tmGerar:
        begin
          Response.ArquivoEnvio := '{"dpsXmlGZipB64":"' + Response.ArquivoEnvio + '"}';
          FpPath := '/nfse';
        end;

      tmEnviarEvento:
        begin
          Response.ArquivoEnvio := '{"pedidoRegistroEventoXmlGZipB64":"' + Response.ArquivoEnvio + '"}';
          FpPath := '/nfse/' + FpChave + '/eventos';
        end;
    else
      begin
        Response.ArquivoEnvio := '';
        FpPath := '';
      end;
    end;

    FpMethod := 'POST';
  end;
end;

function TACBrNFSeProviderPadraoNacional.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  Result := EnumeradoToStr(t,
                         ['0', '1', '2', '3', '4', '5', '6'],
                         [retNenhum, retCooperativa, retEstimativa,
                         retMicroempresaMunicipal, retNotarioRegistrador,
                         retISSQNAutonomos, retSociedadeProfissionais]);
end;

function TACBrNFSeProviderPadraoNacional.StrToRegimeEspecialTributacao(
  out ok: boolean; const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                        ['0', '1', '2', '3', '4', '5', '6'],
                        [retNenhum, retCooperativa, retEstimativa,
                         retMicroempresaMunicipal, retNotarioRegistrador,
                         retISSQNAutonomos, retSociedadeProfissionais]);
end;

function TACBrNFSeProviderPadraoNacional.RegimeEspecialTributacaoDescricao(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  case t of
    retNenhum:                 Result := '0 - Nenhum';
    retCooperativa:            Result := '1 - Cooperativa';
    retEstimativa:             Result := '2 - Estimativa';
    retMicroempresaMunicipal:  Result := '3 - Microempresa Municipal';
    retNotarioRegistrador:     Result := '4 - Not�rio ou Registrador';
    retISSQNAutonomos:         Result := '5 - Profissional Aut�nomo';
    retSociedadeProfissionais: Result := '6 - Sociedade de Profissionais';
  else
    Result := '';
  end;
end;

{ TACBrNFSeXWebservicePadraoNacional }

function TACBrNFSeXWebservicePadraoNacional.GerarNFSe(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSePorChave(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSePorRps(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.EnviarEvento(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarEvento(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarDFe(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarParam(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePadraoNacional.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  if not StringIsPDF(Result) then
    Result := UTF8Decode(Result);
end;

end.
