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

unit GeisWeb.Provider;

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
  TACBrNFSeXWebserviceGeisWeb = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
    function GetSoapAction: string;
    function GetAliasCidade: string;
  public
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property NameSpace: string read GetNameSpace;
    property SoapAction: string read GetSoapAction;
    property AliasCidade: string read GetAliasCidade;
  end;

  TACBrNFSeProviderGeisWeb = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Msg'); override;
  public
    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  GeisWeb.GravarXml, GeisWeb.LerXml;

{ TACBrNFSeProviderGeisWeb }

procedure TACBrNFSeProviderGeisWeb.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    ModoEnvio := meLoteSincrono;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := True;
      ConsultarLote := True;
      ConsultarNfse := True;
      CancelarNfse := True;
    end;
  end;

  ConfigAssinar.Rps := True;

  SetXmlNameSpace('http://www.geisweb.net.br/xsd/envio_lote_rps.xsd');

  with ConfigMsgDados do
  begin
    UsarNumLoteConsLote := True;

    with XmlRps do
    begin
      DocElemento := 'Rps';
      InfElemento := 'Rps';
    end;
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    RecepcionarSincrono := 'envio_lote_rps.xsd';
    ConsultarLote := 'consulta_lote_rps.xsd';
    ConsultarNFSe := 'consulta_nfse.xsd';
    CancelarNFSe := 'cancela_nfse.xsd';
  end;
end;

function TACBrNFSeProviderGeisWeb.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_GeisWeb.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGeisWeb.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_GeisWeb.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGeisWeb.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceGeisWeb.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderGeisWeb.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Erro'), tcStr);
    AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Status'), tcStr);
    AErro.Correcao := '';
  end;
end;

function TACBrNFSeProviderGeisWeb.RegimeEspecialTributacaoDescricao(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  case t of
    retSimplesNacional           : Result := '1 - Simples Nacional';
    retMicroempresarioIndividual : Result := '2 - Microempresário Individual (MEI)';
    retImune                     : Result := '4 - Imune';
    retOutros                    : Result := '6 - Outros/Sem Vinculo';
  else
    Result := '';
  end;
end;

function TACBrNFSeProviderGeisWeb.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '4', '6'],
                           [retSimplesNacional, retMicroempresarioIndividual,
                            retImune, retOutros]);
end;

function TACBrNFSeProviderGeisWeb.StrToRegimeEspecialTributacao(out ok: boolean;
  const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                          ['1', '2', '4', '6'],
                          [retSimplesNacional, retMicroempresarioIndividual,
                           retImune, retOutros]);
end;

function TACBrNFSeProviderGeisWeb.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<Rps xmlns="http://www.geisweb.net.br/xsd/envio_lote_rps.xsd">' +
            SeparaDados(aXml, 'Rps') + '</Rps>';
end;

procedure TACBrNFSeProviderGeisWeb.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<EnviaLoteRps' + NameSpace + '>' +
                           '<CnpjCpf>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</CnpjCpf>' +
                           '<NumeroLote>' +
                             Response.NumeroLote +
                           '</NumeroLote>' +
                           Xml +
                         '</EnviaLoteRps>';
  end;
end;

procedure TACBrNFSeProviderGeisWeb.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANodeArray: TACBrXmlNodeArray;
  ANode, AuxNode: TACBrXmlNode;
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
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');

      if not Assigned(ANodeArray) then
      begin
        Response.Sucesso := False;
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];

        if ANode =  nil then
        begin
          Response.Sucesso := False;
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        with Response do
        begin
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataLancamento'), tcDatVcto);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('LinkConsulta'), tcStr);
          Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
        end;

        AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

        if AuxNode <> nil then
        begin
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);

          with Response do
          begin
            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroNfse'), tcStr);
            CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);

          ANota.NFSe.Numero := Response.NumeroNota;
          ANota.NFSe.CodigoVerificacao := Response.CodigoVerificacao;

          SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderGeisWeb.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<ConsultaLoteRps>' +
                         '<CnpjCpf>' +
                           OnlyNumber(Emitente.CNPJ) +
                         '</CnpjCpf>' +
                         '<Consulta>' +
                           '<CnpjCpfPrestador>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</CnpjCpfPrestador>' +
                           '<NumeroLote>' +
                             Response.NumeroLote +
                           '</NumeroLote>' +
                         '</Consulta>' +
                       '</ConsultaLoteRps>';
end;

procedure TACBrNFSeProviderGeisWeb.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANodeArray: TACBrXmlNodeArray;
  ANode, AuxNode: TACBrXmlNode;
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
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANodeArray := ANode.Childrens.FindAllAnyNs('Rps');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod208;
        AErro.Descricao := ACBrStr(Desc208);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

        if AuxNode <> nil then
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);

        if NumRps = '' then
        begin
          AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

          if AuxNode <> nil then
            NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

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

procedure TACBrNFSeProviderGeisWeb.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  if OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) =
     OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) then
    Response.InfConsultaNFSe.NumeroFinNFSe := '';

  XmlConsulta := '';

  if (OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '') and
     (OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) = '') and
     (Response.InfConsultaNFSe.DataInicial = 0) and
     (Response.InfConsultaNFSe.DataFinal = 0) then
    XmlConsulta := '<NumeroNfse>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</NumeroNfse>' +
                   '<DtInicial/>' +
                   '<DtFinal/>' +
                   '<NumeroInicial/>' +
                   '<NumeroFinal/>';

  if (OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '') and
     (OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) <> '') and
     (Response.InfConsultaNFSe.DataInicial = 0) and
     (Response.InfConsultaNFSe.DataFinal = 0) then
    XmlConsulta := '<NumeroNfse/>' +
                   '<DtInicial/>' +
                   '<DtFinal/>' +
                   '<NumeroInicial>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</NumeroInicial>' +
                   '<NumeroFinal>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) +
                   '</NumeroFinal>';

  if (OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) = '') and
     (OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) = '') and
     (Response.InfConsultaNFSe.DataInicial > 0) and
     (Response.InfConsultaNFSe.DataFinal > 0) then
    XmlConsulta := '<NumeroNfse/>' +
                   '<DtInicial>' +
                      FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataInicial) +
                   '</DtInicial>' +
                   '<DtFinal>' +
                      FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataFinal) +
                   '</DtFinal>' +
                   '<NumeroInicial/>' +
                   '<NumeroFinal/>';

  Response.ArquivoEnvio := '<ConsultaNfse>' +
                             '<CnpjCpf>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CnpjCpf>' +
                             '<Consulta>' +
                               '<CnpjCpfPrestador>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</CnpjCpfPrestador>' +
                               XmlConsulta +
                               '<Pagina>' +
                                  IntToStr(Response.InfConsultaNFSe.Pagina) +
                               '</Pagina>' +
                             '</Consulta>' +
                           '</ConsultaNfse>';
end;

procedure TACBrNFSeProviderGeisWeb.TratarRetornoConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANodeArray: TACBrXmlNodeArray;
  ANode, AuxNode: TACBrXmlNode;
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
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

        if AuxNode <> nil then
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

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

procedure TACBrNFSeProviderGeisWeb.PrepararCancelaNFSe(
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

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<CancelaNfse>' +
                             '<CnpjCpf>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CnpjCpf>' +
                             '<Cancela>' +
                               '<CnpjCpfPrestador>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</CnpjCpfPrestador>' +
                               '<NumeroNfse>' +
                                  Response.InfCancelamento.NumeroNFSe +
                               '</NumeroNfse>' +
                             '</Cancela>' +
                           '</CancelaNfse>';
end;

procedure TACBrNFSeProviderGeisWeb.TratarRetornoCancelaNFSe(
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
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      Response.NumeroLote := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroLote'), tcStr);

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

        if AuxNode <> nil then
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

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

{ TACBrNFSeXWebserviceGeisWeb }

function TACBrNFSeXWebserviceGeisWeb.GetAliasCidade: string;
begin
  Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params.ValorParametro('AliasCidade');
end;

function TACBrNFSeXWebserviceGeisWeb.GetNameSpace: string;
var
  ambiente: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    ambiente := 'producao/' + AliasCidade
  else
    ambiente := 'homologacao/modelo';

  Result := 'xmlns:geis="urn:https://www.geisweb.net.br/' + ambiente +
            '/webservice/GeisWebServiceImpl.php"';
end;

function TACBrNFSeXWebserviceGeisWeb.GetSoapAction: string;
var
  ambiente: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    ambiente := 'producao/' + AliasCidade
  else
    ambiente := 'homologacao/modelo';

  Result := 'urn:https://www.geisweb.net.br/' + ambiente +
            '/webservice/GeisWebServiceImpl.php#';
end;

function TACBrNFSeXWebserviceGeisWeb.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<geis:EnviaLoteRps soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<EnviaLoteRps xsi:type="xsd:string">' +
                          XmlToStr(AMSG) +
                       '</EnviaLoteRps>';
  Request := Request + '</geis:EnviaLoteRps>';

  Result := Executar(SoapAction + 'EnviaLoteRps', Request,
                     ['EnviaLoteRpsResposta', 'EnviaLoteRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
                      NameSpace]);
end;

function TACBrNFSeXWebserviceGeisWeb.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<geis:ConsultaLoteRps soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<ConsultaLoteRps xsi:type="xsd:string">' +
                          XmlToStr(AMSG) +
                       '</ConsultaLoteRps>';
  Request := Request + '</geis:ConsultaLoteRps>';

  Result := Executar(SoapAction + 'ConsultaLoteRps', Request,
                     ['ConsultaLoteRpsResposta', 'ConsultaLoteRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
                      NameSpace]);
end;

function TACBrNFSeXWebserviceGeisWeb.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<geis:ConsultaNfse soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<ConsultaNfse xsi:type="xsd:string">' +
                          XmlToStr(AMSG) +
                       '</ConsultaNfse>';
  Request := Request + '</geis:ConsultaNfse>';

  Result := Executar(SoapAction + 'ConsultaNfse', Request,
                     ['ConsultaNfseResposta', 'ConsultaNfseResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
                      NameSpace]);
end;

function TACBrNFSeXWebserviceGeisWeb.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<geis:CancelaNfse soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<CancelaNfse xsi:type="xsd:string">' +
                          XmlToStr(AMSG) +
                       '</CancelaNfse>';
  Request := Request + '</geis:CancelaNfse>';

  Result := Executar(SoapAction + 'CancelaNfse', Request,
                     ['CancelaNfseResposta', 'CancelaNfseResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:xsd="http://www.w3.org/2001/XMLSchema"',
                      NameSpace]);
end;

function TACBrNFSeXWebserviceGeisWeb.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := Trim(StringReplace(Result, '&', '&amp;', [rfReplaceAll]));
end;

end.
