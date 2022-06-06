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

  TACBrNFSeXWebserviceIPM = class(TACBrNFSeXWebserviceMulti1)
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderIPM = class (TACBrNFSeProviderProprio)
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

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'mensagem'); override;

    function AjustarRetorno(const Retorno: string): string;

  public
    function SimNaoToStr(const t: TnfseSimNao): string; override;
    function StrToSimNao(out ok: boolean; const s: string): TnfseSimNao; override;
  end;

  TACBrNFSeXWebserviceIPM101 = class(TACBrNFSeXWebserviceMulti2)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderIPM101 = class (TACBrNFSeProviderIPM)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  synacode,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
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
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
    DetalharServico := True;
  end;

  with ConfigAssinar do
  begin
    RpsGerarNFSe := ConfigGeral.Params.ParamTemValor('Assinar', 'AssRpsGerarNfse');
    CancelarNFSe := ConfigGeral.Params.ParamTemValor('Assinar', 'AssCancelarNfse');
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'nfse';
      DocElemento := 'nfse';
    end;

    with CancelarNFSe do
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
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPM.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderIPM.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I{, j, k}: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  aMsg, Codigo: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    aMsg := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('codigo'), tcStr);

    Codigo := Copy(aMsg, 1, 5);

    {
     Codigo = 00001 significa que o processamento ocorreu com sucesso, logo não
     tem erros.
    }
    if Codigo <> '00001' then
    begin
      AErro := Response.Erros.New;

      AErro.Codigo := Codigo;
      AErro.Descricao := Copy(aMsg, 9, Length(aMsg));
      AErro.Correcao := '';
    end;

    (*
    if Codigo <> '' then
    begin
      aMsg := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('MensagemRetorno'), tcStr);

      if aMsg = '' then
      begin
        aMsg := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

        if aMsg <> '' then
        begin
          AErro := Response.Erros.New;

          AErro.Codigo := Codigo;
          AErro.Descricao := aMsg;
          AErro.Correcao := '';
        end;
      end
      else
      begin
        j := Pos('code', aMsg);

        if j > 0 then
         Codigo := Copy(aMsg, j + 6, 3);


        j := Pos('msg', aMsg);

        if j > 0 then
        begin
          AErro := Response.Erros.New;

          AErro.Codigo := Codigo;
          AErro.Descricao := Copy(aMsg, j + 6, Length(aMsg));
          k := Pos(',', AErro.Descricao);
          AErro.Descricao := Copy(AErro.Descricao, 1, k - 2);

          AErro.Correcao := '';
        end;
      end;
    end;
    *)
  end;
end;

function TACBrNFSeProviderIPM.SimNaoToStr(const t: TnfseSimNao): string;
begin
  Result := EnumeradoToStr(t, ['0', '1'], [snNao, snSim]);
end;

function TACBrNFSeProviderIPM.StrToSimNao(out ok: boolean;
  const s: string): TnfseSimNao;
begin
  Result := StrToEnumerado(ok, s,
                           ['0', '1', 'N', 'S'],
                           [snNao, snSim, snNao, snSim]);
end;

function TACBrNFSeProviderIPM.AjustarRetorno(const Retorno: string): string;
var
  i: Integer;
begin
  i := Pos('<codigo_html>', Retorno);

  if i > 0 then
    Result := Copy(Retorno, 1, i -1) + '</retorno>'
  else
    Result := Retorno;

  Result := Trim(StringReplace(Result, '&', '&amp;', [rfReplaceAll]));
end;

function TACBrNFSeProviderIPM.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderIPM.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := Params.Xml;
end;

procedure TACBrNFSeProviderIPM.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
  I: Integer;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.ArquivoRetorno := AjustarRetorno(Response.ArquivoRetorno);

      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');

          NumRps := '';
          if AuxNode <> nil then
            NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          if Assigned(ANota) then
            ANota.XmlNfse := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
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

procedure TACBrNFSeProviderIPM.PrepararConsultaLoteRps(
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

  Response.ArquivoEnvio := '<nfse>' +
                         '<pesquisa>' +
                           '<codigo_autenticidade>' +
                             Response.Protocolo +
                           '</codigo_autenticidade>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  NumRps: String;
  ANota: TNotaFiscal;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.ArquivoRetorno := AjustarRetorno(Response.ArquivoRetorno);

      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);
      ProcessarMensagemErros(ANode, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('rps');
        NumRps := '';

        if AuxNode <>  nil then
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);


        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('nf');

          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
        end;

        if NumRps <> '' then
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
        else
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

        if Assigned(ANota) then
          ANota.XmlNfse := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
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

procedure TACBrNFSeProviderIPM.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.NumRPS) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := Desc102;
    Exit;
  end;

  if EstaVazio(Response.Serie) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := Desc103;
    Exit;
  end;

  Response.ArquivoEnvio := '<consulta_rps>' +
                         '<cidade>' +
                           CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                         '</cidade>' +
                         '<serie_rps>' +
                           OnlyNumber(Response.Serie) +
                         '</serie_rps>' +
                         '<numero_rps>' +
                           OnlyNumber(Response.NumRPS) +
                         '</numero_rps>' +
                       '</consulta_rps>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  NumRps: String;
  ANota: TNotaFiscal;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.ArquivoRetorno := AjustarRetorno(Response.ArquivoRetorno);

      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('rps');
        NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('nf');

          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XmlNfse := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end
      else
      begin
        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('rps');

          if AuxNode = nil then
            AuxNode := ANode;

          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          if NumeroNota = '' then
            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfe'), tcStr);

          SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          if SerieNota = '' then
            SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfe'), tcStr);

          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);

          if Data = 0 then
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_hora_conversao'), tcDatVcto);

          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);

          DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          if DescSituacao = '' then
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);

          Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);

          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          if Protocolo = '' then
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('codigo_autenticidade'), tcStr);
        end;
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

procedure TACBrNFSeProviderIPM.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  TagSerie: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.CadEconomico) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if ConfigGeral.Versao = ve101 then
    TagSerie := 'serie_nfse'
  else
    TagSerie := 'serie';

  Response.ArquivoEnvio := '<nfse>' +
                         '<pesquisa>' +
                           '<numero>' +
                             OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                           '</numero>' +
                           '<' + TagSerie + '>' +
                             OnlyNumber(Response.InfConsultaNFSe.SerieNFSe) +
                           '</' + TagSerie + '>' +
                           '<cadastro>' +
                             OnlyNumber(Response.InfConsultaNFSe.CadEconomico) +
                           '</cadastro>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
//  I: Integer;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.ArquivoRetorno := AjustarRetorno(Response.ArquivoRetorno);

      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('rps');

        if AuxNode <> nil then
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('nf');

          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XmlNfse := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
        {
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if Assigned(ANota) then
            ANota.XmlNfse := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
        }
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
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

procedure TACBrNFSeProviderIPM.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  xSerie, IdAttr, xSubstituta: string;
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

  if ConfigGeral.Versao = ve101 then
    xSerie := '<serie_nfse>' +
                Response.InfCancelamento.SerieNFSe +
              '</serie_nfse>'
  else
    xSerie := '';

  if ConfigGeral.Params.TemParametro('SolicitarCancelamento') then
  begin
    xSubstituta := '';
    if Response.InfCancelamento.NumeroNFSeSubst <> '' then
      xSubstituta := '<substituta>' +
                       '<numero>' +
                         Response.InfCancelamento.NumeroNFSeSubst +
                       '</numero>' +
                       '<serie>' +
                         Response.InfCancelamento.SerieNFSeSubst +
                       '</serie>' +
                     '</substituta>';

    Response.ArquivoEnvio := '<solicitacao_cancelamento>' +
                           '<prestador>' +
                             '<cpfcnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</cpfcnpj>' +
                             '<cidade>' +
                               CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                             '</cidade>' +
                           '</prestador>' +
                           '<documentos>' +
                             '<nfse>' +
                               '<numero>' +
                                 Response.InfCancelamento.NumeroNFSe +
                               '</numero>' +
                               '<serie>' +
                                 Response.InfCancelamento.SerieNFSe +
                               '</serie>' +
                               '<observacao>' +
                                 Response.InfCancelamento.MotCancelamento +
                               '</observacao>' +
                               xSubstituta +
                             '</nfse>' +
                           '</documentos>' +
                         '</solicitacao_cancelamento>';
  end
  else
  begin
    if ConfigAssinar.CancelarNFSe then
      IdAttr := ' Id="nota"'
    else
      IdAttr := '';

    Response.ArquivoEnvio := '<nfse' + IdAttr + '>' +
                           '<nf>' +
                             '<numero>' +
                               Response.InfCancelamento.NumeroNFSe +
                             '</numero>' +
                             xSerie +
                             '<situacao>' +
                               'C' +
                             '</situacao>' +
                             '<observacao>' +
                               Response.InfCancelamento.MotCancelamento +
                             '</observacao>' +
                           '</nf>' +
                           '<prestador>' +
                             '<cpfcnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</cpfcnpj>' +
                             '<cidade>' +
                               CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                             '</cidade>' +
                           '</prestador>' +
                         '</nfse>';
  end;
end;

procedure TACBrNFSeProviderIPM.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
  I: Integer;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.ArquivoRetorno := AjustarRetorno(Response.ArquivoRetorno);

      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');

          if AuxNode <> nil then
            NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr)
          else
            NumRps := '';

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ObterConteudoTag(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          if Assigned(ANota) then
            ANota.XmlNfse := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ObterConteudoTag(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
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

{ TACBrNFSeXWebserviceIPM }

function TACBrNFSeXWebserviceIPM.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM.TratarXmlRetornado(const aXML: string): string;
begin
  if Pos('</', aXML) = 0 then
  begin
    Result := '<a>' +
              '<mensagem>' +
                '<codigo>' + '</codigo>' +
                '<Mensagem>' + aXML + '</Mensagem>' +
                '<Correcao>' + '</Correcao>' +
              '</mensagem>' +
            '</a>';

    Result := ParseText(AnsiString(Result), True, False);
    Result := String(NativeStringToUTF8(Result));
  end
  else
  begin
    Result := inherited TratarXmlRetornado(aXML);

    Result := ParseText(AnsiString(Result), True, False);
    Result := RemoverDeclaracaoXML(Result);
    Result := RemoverIdentacao(Result);
    Result := RemoverCaracteresDesnecessarios(Result);
  end;
end;

function TACBrNFSeXWebserviceIPM.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

{ TACBrNFSeXWebserviceIPM101 }

procedure TACBrNFSeXWebserviceIPM101.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
    Auth := 'Basic ' + string(EncodeBase64(AnsiString(WSUser + ':' +
      ParseText(AnsiString(WSSenha), False))));

  aHeaderReq.AddHeader('Authorization', Auth);
end;

function TACBrNFSeXWebserviceIPM101.TratarXmlRetornado(
  const aXML: string): string;
var
  j, k: Integer;
  Codigo, Mensagem: string;
begin
  if Pos('</', aXML) = 0 then
  begin
    j := Pos('code', aXML);

    if j > 0 then
     Codigo := Copy(aXML, j + 6, 3);

    j := Pos('msg', aXML);

    if j > 0 then
    begin
      Mensagem := Copy(aXML, j + 6, Length(aXML));
      k := Pos(',', Mensagem);
      Mensagem := Copy(Mensagem, 1, k - 2);
    end;

    Result := '<a>' +
              '<mensagem>' +
                '<codigo>' + Codigo + '</codigo>' +
                '<Mensagem>' + Mensagem + '</Mensagem>' +
                '<Correcao>' + '</Correcao>' +
              '</mensagem>' +
            '</a>';

    Result := ParseText(AnsiString(Result), True, False);
    Result := String(NativeStringToUTF8(Result));
  end
  else
  begin
    Result := inherited TratarXmlRetornado(aXML);

    Result := ParseText(AnsiString(Result), True, False);
    Result := RemoverDeclaracaoXML(Result);
    Result := RemoverIdentacao(Result);
    Result := RemoverCaracteresDesnecessarios(Result);
  end;
end;

function TACBrNFSeXWebserviceIPM101.GerarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM101.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM101.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceIPM101.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

{ TACBrNFSeProviderIPM101 }

procedure TACBrNFSeProviderIPM101.Configuracao;
begin
  inherited Configuracao;

end;

function TACBrNFSeProviderIPM101.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPM101.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM101.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPM101.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM101.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPM101.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

end.
