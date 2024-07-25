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

unit SigISSWeb.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrBase,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceSigISSWeb = class(TACBrNFSeXWebserviceRest)
  private
    FAjustaSetHeader:Boolean;

  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarToken(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSigISSWeb = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
    FpMimeType: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); override;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'erros';
                                     const AMessageTag: string = 'erro'); override;

  public
    function TributacaoToStr(const t: TTributacao): string; override;
    function StrToTributacao(out ok: boolean; const s: string): TTributacao; override;
    function TributacaoDescricao(const t: TTributacao): String; override;
  end;

var
  xToken: string;

implementation

uses
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXNotasFiscais, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  SigISSWeb.GravarXml, SigISSWeb.LerXml;

{ TACBrNFSeProviderSigISSWeb }

procedure TACBrNFSeProviderSigISSWeb.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '|';
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    ConsultaLote := False;
    ConsultaNFSe := False;
    CancPreencherMotivo := True;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;

    ServicosDisponibilizados.EnviarUnitario := True;
    ServicosDisponibilizados.GerarToken := True;
    ServicosDisponibilizados.CancelarNfse := True;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderSigISSWeb.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SigISSWeb.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSigISSWeb.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SigISSWeb.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSigISSWeb.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebserviceSigISSWeb.Create(FAOwner, AMetodo, URL,
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

procedure TACBrNFSeProviderSigISSWeb.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  Descricao: String;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('descricao'), tcStr);

    if Descricao <> 'Operação concluída com sucesso' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('codigo'), tcStr);
      AErro.Descricao := Descricao;
      AErro.Correcao := '';
    end;  
  end;
end;

function TACBrNFSeProviderSigISSWeb.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := SeparaDados(aXml, 'notafiscal');
end;

procedure TACBrNFSeProviderSigISSWeb.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  Response.Clear;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

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

  // Atenção: Neste xml todos os "Ws_" do início das tags devem ter o primeiro "W" em maiúsculo
  Response.ArquivoEnvio := '{"login":"' +
                           OnlyNumber(Emitente.WSUser) + '","senha":"' +
                           Emitente.WSSenha + '"}';

  FpPath := 'rest/login';
  FpMethod := 'POST';
  FpMimeType := 'application/json';
end;

procedure TACBrNFSeProviderSigISSWeb.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
begin
  xToken := SeparaDados(Response.ArquivoRetorno, 'descricao');

  if Pos('Bearer', xToken) = 0 then
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

        ProcessarMensagemErros(ANode, Response);

        Response.Sucesso := (Response.Erros.Count = 0);
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
    Response.Token := xToken;
end;

procedure TACBrNFSeProviderSigISSWeb.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
//  Emitente: TEmitenteConfNFSe;
//  CodMun: Integer;
begin
  if Response.InfCancelamento.NumeroNFSe = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if Response.InfCancelamento.SerieNFSe = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := ACBrStr(Desc112);
    Exit;
  end;

  if Response.InfCancelamento.MotCancelamento = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;

//  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
//  CodMun := TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio;


  FpPath := 'rest/nfes/cancela/' +
            Response.InfCancelamento.NumeroNFSe +
            '/serie/' +
            Response.InfCancelamento.SerieNFSe +
            '/motivo/' +
            StringReplace(Response.InfCancelamento.MotCancelamento,' ','%20',[rfReplaceAll]);
  FpMethod := 'GET';
  FpMimeType := 'application/json';
  {
  Response.ArquivoEnvio := '<cancelamentoNfseLote xmlns="http://www.SigISSWeb.com/nfse">' +
                             '<codigoMunicipio>' +
                                CodIBGEToCodTOM(CodMun) +
                             '</codigoMunicipio>' +
                             '<dtEmissao>' +
                                FormatDateTime('YYYY-MM-DD', Response.InfCancelamento.DataEmissaoNFSe) +
                                'T' +
                                FormatDateTime('HH:NN:SS', Response.InfCancelamento.DataEmissaoNFSe) +
                             '</dtEmissao>' +
                             '<autenticacao>' +
                               '<token>' +
                                  Emitente.WSChaveAutoriz +
                               '</token>' +
                             '</autenticacao>' +
                             '<numeroNota>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</numeroNota>' +
                             '<chaveSeguranca>' +
                                Response.InfCancelamento.ChaveNFSe +
                             '</chaveSeguranca>' +
                           '</cancelamentoNfseLote>';
  }
end;

procedure TACBrNFSeProviderSigISSWeb.PrepararEmitir(
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
    AErro.Descricao := ACBrStr(Desc002);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
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

    SalvarXmlRps(Nota);

    ListaRps := ListaRps + Nota.XmlRps;
  end;

  Response.ArquivoEnvio := RemoverDeclaracaoXML(ListaRps);
  Response.ArquivoEnvio := '<?xml version="1.0" encoding="ISO-8859-1"?>' +
                           Response.ArquivoEnvio;

  FpPath := 'rest/nfes';
  FpMethod := 'POST';
  FpMimeType := 'text/xml';
end;

procedure TACBrNFSeProviderSigISSWeb.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANota: TNotaFiscal;
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

      Response.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('codigo'), tcStr);
      Response.NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numero_nf'), tcStr);
      Response.SerieNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie'), tcStr);
      Response.NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('rps'), tcStr);
      Response.SerieRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie_rps'), tcStr);
      Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('data_emissao'), tcDatVcto);

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if Response.Sucesso then
      begin
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(Response.NumeroRps);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderSigISSWeb.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('protocolo'), tcStr);
      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('codigoStatus'), tcStr);

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      // Precisamos de um retorno sem erros para terminar a implementação da
      // leitura do retorno
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

function TACBrNFSeProviderSigISSWeb.StrToTributacao(out ok: boolean;
  const s: string): TTributacao;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '9'],
                           [ttIsentaISS, ttImune, ttExigibilidadeSusp,
                            ttTributavel, ttNaoIncidencianoMunic,
                            ttTributavelSN, ttTributavelFixo, ttNaoTributavel,
                            ttMEI]);
end;

function TACBrNFSeProviderSigISSWeb.TributacaoDescricao(
  const t: TTributacao): String;
begin
  case t of
    ttIsentaISS           : Result := '1 - Isenta de ISS';
    ttImune               : Result := '2 - Imune';
    ttExigibilidadeSusp   : Result := '3 - Exigibilidade Susp.Dec.J/Proc.A';
    ttTributavel          : Result := '4 - Tributável';
    ttNaoIncidencianoMunic: Result := '5 - Não Incidência no Município';
    ttTributavelSN        : Result := '6 - Tributável S.N.';
    ttTributavelFixo      : Result := '7 - Tributável Fixo';
    ttNaoTributavel       : Result := '8 - Não Tributável';
    ttMEI                 : Result := '9 - Micro Empreendedor Individual(MEI)';
  else
    Result := '';
  end;
end;

function TACBrNFSeProviderSigISSWeb.TributacaoToStr(
  const t: TTributacao): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '9'],
                           [ttIsentaISS, ttImune, ttExigibilidadeSusp,
                            ttTributavel, ttNaoIncidencianoMunic,
                            ttTributavelSN, ttTributavelFixo, ttNaoTributavel,
                            ttMEI]);
end;

{ TACBrNFSeXWebserviceSigISSWeb }

procedure TACBrNFSeXWebserviceSigISSWeb.SetHeaders(aHeaderReq: THTTPHeader);
begin
  if FAjustaSetHeader then
    aHeaderReq.AddHeader('Authorization', xToken);
end;

function TACBrNFSeXWebserviceSigISSWeb.GerarToken(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FAjustaSetHeader := False;
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceSigISSWeb.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FAjustaSetHeader := True;
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceSigISSWeb.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FAjustaSetHeader := True;
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceSigISSWeb.TratarXmlRetornado(
  const aXML: string): string;
var
  Xml: string;
begin
  Xml := ConverteANSIparaUTF8(aXML);
  Xml := RemoverDeclaracaoXML(Xml);

  if StringIsXML(Xml) then
  begin
    Result := inherited TratarXmlRetornado(Xml);

    Result := ParseText(Result);
//    Result := RemoverDeclaracaoXML(Result);
    Result := RemoverIdentacao(Result);
    Result := RemoverCaracteresDesnecessarios(Result);
    Result := RemoverPrefixosDesnecessarios(Result);
    Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  end
  else
  begin
    Result := '<a>' +
                '<erros>' +
                  '<erro>' +
                    '<codigo>' + '</codigo>' +
                    '<descricao>' + Xml + '</descricao>' +
                    '<correcao>' + '</correcao>' +
                  '</erro>' +
                '</erros>' +
              '</a>';

    Result := ParseText(Result);
  end;
end;

end.
