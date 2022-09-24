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

unit PadraoNacional.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrJSON, ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebservicePadraoNacional = class(TACBrNFSeXWebserviceRest)
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

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderPadraoNacional = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure ProcessarMensagemDeErros(LJson: TACBrJSONObject;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'erros'); //override;
  public
    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; override;
  end;

implementation

uses
  synacode,
  ACBrDFeException, ACBrCompress,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrNFSeX, ACBrNFSeXConsts, ACBrNFSeXConfiguracoes,
  PadraoNacional.GravarXml, PadraoNacional.LerXml;

{ TACBrNFSeProviderPadraoNacional }

procedure TACBrNFSeProviderPadraoNacional.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meUnitario;
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

    with XmlRps do
    begin
      // Define o NameSpace do XML do Rps, sobrepõe a definição global: SetXmlNameSpace
      xmlns := '';
      InfElemento := 'infDPS';
      DocElemento := 'DPS';
    end;

    (*
    // Usado para geração do Xml do Rps
    // Usado para geração do Envio do Lote em modo assíncrono
    with LoteRps do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsEnvio';
    end;

    // Usado para geração do Envio do Lote em modo Sincrono
    with LoteRpsSincrono do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsSincronoEnvio';
    end;

    // Usado para geração da Consulta a Situação do Lote
    with ConsultarSituacao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarSituacaoLoteRpsEnvio';
    end;

    // Usado para geração da Consulta do Lote
    with ConsultarLote do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarLoteRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe por RPS
    with ConsultarNFSeRps do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe
    with ConsultarNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseEnvio';
    end;

    // Usado para geração do Cancelamento
    with CancelarNFSe do
    begin
      xmlns := '';
      InfElemento := 'InfPedidoCancelamento';
      DocElemento := 'Pedido';
    end;

    // Usado para geração do Substituir
    with SubstituirNFSe do
    begin
      xmlns := '';
      InfElemento := 'SubstituicaoNfse';
      DocElemento := 'SubstituirNfseEnvio';
    end;
    *)
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    RpsGerarNFSe := True;
    {
    ConsultarSituacao := False;
    ConsultarLote     := False;
    ConsultarNFSeRps  := False;
    ConsultarNFSe     := False;
    CancelarNFSe      := False;
    LoteGerarNFSe     := False;
    RpsSubstituirNFSe := False;
    SubstituirNFSe    := False;
    }
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'DPS_v1.00.xsd';
    GerarNFSe := 'DPS_v1.00.xsd';
    {
    ConsultarSituacao := 'nfse.xsd';
    ConsultarLote := 'nfse.xsd';
    ConsultarNFSeRps := 'nfse.xsd';
    ConsultarNFSe := 'nfse.xsd';
    ConsultarNFSePorFaixa := 'nfse.xsd';
    ConsultarNFSeServicoPrestado := 'nfse.xsd';
    ConsultarNFSeServicoTomado := 'nfse.xsd';
    CancelarNFSe := 'nfse.xsd';
    RecepcionarSincrono := 'nfse.xsd';
    SubstituirNFSe := 'nfse.xsd';
    }
    Validar := False;
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
  I: Integer;
  JSonErros: TACBrJSONArray;
  JsonErro, JSon: TACBrJSONObject;
  Codigo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  JSonErros := LJson.AsJSONArray['erros'];

  for I := 0 to JSonErros.Count-1 do
  begin
    JSon := JSonErros.ItemAsJSONObject[i];

    Codigo := JSon.AsString['Codigo'];

    if Codigo <> '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Codigo;
      AErro.Descricao := JSon.AsString['Descricao'];
      AErro.Correcao := JSon.AsString['Complemento'];
    end;
  end;

  JSonErro := LJson.AsJSONObject['erro'];

  if JsonErro <> nil then
  begin
    Codigo := JSonErro.AsString['codigo'];

    if Codigo <> '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Codigo;
      AErro.Descricao := JSonErro.AsString['descricao'];
      AErro.Correcao := JSonErro.AsString['complemento'];
    end;
  end;
end;

procedure TACBrNFSeProviderPadraoNacional.PrepararEmitir(
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
      // Verificar se o envio vai ser em lote também.
      ValidarSchema(Response, tmGerar);

      if (EmiteResponse.Erros.Count > 0) then
      begin
        TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
        Exit;
      end;
    end;

    SalvarXmlRps(Nota);

    ListaRps := ListaRps + Nota.XmlRps;
  end;

  ListaRps := AplicarLineBreak(ListaRps, '');
  ListaRps := EncodeBase64(GZipCompress(ListaRps));
  ListaRps := AplicarLineBreak(ListaRps, '');

  Response.ArquivoEnvio := '{"dpsXmlGZipB64":"' + ListaRps + '"}';
  FpPath := '/nfse';
  FpMethod := 'POST';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NFSeXml: String;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  NumNFSe, NumRps: String;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := Desc201;
    Exit
  end;

  Document := TACBrJsonObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(Document, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Data := Document.AsISODateTime['dataHoraProcessamento'];
      Response.idNota := Document.AsString['idDPS'];
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
            AErro.Descricao := Desc203;
            Exit
          end;

          DocumentXml.LoadFromXml(NFSeXml);

          ANode := DocumentXml.Root.Childrens.FindAnyNs('infNFSe');

          NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);
          ANode := ANode.Childrens.FindAnyNs('DPS');
          ANode := ANode.Childrens.FindAnyNs('infDPS');
          NumRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDPS'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
          SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderPadraoNacional.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.InfConsultaNFSe.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := Desc118;
    Exit;
  end;

  Response.Metodo := tmConsultarNFSePorFaixa;

  Response.ArquivoEnvio := '';
  FpPath := '/nfse/' + Response.InfConsultaNFSe.ChaveNFSe;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderPadraoNacional.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NFSeXml: String;
  DocumentXml: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  NumNFSe, NumRps: String;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := Desc201;
    Exit
  end;

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
            AErro.Descricao := Desc203;
            Exit
          end;

          DocumentXml.LoadFromXml(NFSeXml);

          ANode := DocumentXml.Root.Childrens.FindAnyNs('infNFSe');

          NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);
          ANode := ANode.Childrens.FindAnyNs('DPS');
          ANode := ANode.Childrens.FindAnyNs('infDPS');
          NumRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDPS'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, DocumentXml.Root.OuterXml);
          SalvarXmlNfse(ANota);
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
    retNotarioRegistrador:     Result := '4 - Notário ou Registrador';
    retISSQNAutonomos:         Result := '5 - Profissional Autônomo';
    retSociedadeProfissionais: Result := '6 - Sociedade de Profissionais';
  else
    Result := '';
  end;
end;

{ TACBrNFSeXWebservicePadraoNacional }

function TACBrNFSeXWebservicePadraoNacional.Recepcionar(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.RecepcionarSincrono(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('',
                     Request, ['GerarNfseResposta'], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('',
                     Request, ['GerarNfseResposta'], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('',
                     Request, ['GerarNfseResposta'], []);
end;

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSePorRps(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSeServicoPrestado(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.ConsultarNFSeServicoTomado(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.Cancelar(ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebservicePadraoNacional.SubstituirNFSe(ACabecalho,
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

function TACBrNFSeXWebservicePadraoNacional.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
