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

unit Bauhaus.Provider;

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

  TACBrNFSeXWebserviceBauhaus = class(TACBrNFSeXWebserviceRest)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderBauhaus = class (TACBrNFSeProviderProprio)
  private
    FpBaseURL: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

    function AjustarRetorno(const Retorno: string): string;
  end;

implementation

uses
  ACBrUtil, synacode,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Bauhaus.GravarJson, Bauhaus.LerJson;

{ TACBrNFSeProviderBauhaus }

procedure TACBrNFSeProviderBauhaus.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderBauhaus.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Bauhaus.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBauhaus.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Bauhaus.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBauhaus.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL, FpMethod: string;
begin
  URL := GetWebServiceURL(AMetodo);
  FpBaseURL := URL;
  FpMethod := 'POST';

  if AMetodo = tmConsultarNFSe then
    FpMethod := 'GET';

  if URL <> '' then
    Result := TACBrNFSeXWebserviceBauhaus.Create(FAOwner, AMetodo, URL, FpMethod)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderBauhaus.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I, j, k: Integer;
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

    if aMsg = '' then
    begin
      aMsg := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('MensagemRetorno'), tcStr);

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
    end
    else
    begin
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
    end;
  end;
end;

function TACBrNFSeProviderBauhaus.AjustarRetorno(const Retorno: string): string;
var
  i: Integer;
begin
  i := Pos('<codigo_html>', Retorno);

  if i > 0 then
    Result := Copy(Retorno, 1, i -1) + '</retorno>'
  else
    Result := Retorno;

  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

function TACBrNFSeProviderBauhaus.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderBauhaus.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := Params.Xml;
end;

procedure TACBrNFSeProviderBauhaus.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
  I: Integer;
  NotaCompleta: Boolean;
  AcessoNegado: Boolean;
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

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

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

procedure TACBrNFSeProviderBauhaus.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  TagSerie: string;
begin
(*
CONSULTA
------------------------------------------------------------
case TipoAmbiente of
  taHomologacao : FRESTClient.BaseURL := '(...)?NumeroNfse='+ANroNota.ToString;
  taProducao    : FRESTClient.BaseURL := '(...)?NumeroNfse='+ANroNota.ToString;
end;

FRESTRequest.Method := rmGet;
FRESTRequest.Params.Clear;
FRESTRequest.Params.AddHeader('Authorization', FChaveAutorizacao);
FRESTRequest.Execute;
*)
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Response.ArquivoEnvio := FpBaseURL + '?NumeroNfse=' +
                       Response.InfConsultaNFSe.NumeroIniNFSe;
end;

procedure TACBrNFSeProviderBauhaus.TratarRetornoConsultaNFSe(
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

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

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

procedure TACBrNFSeProviderBauhaus.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
(*
CANCELAMENTO
case TipoAmbiente of
  taHomologacao : FRESTClient.BaseURL := '(...)/cancelar';
  taProducao    : FRESTClient.BaseURL := '(...)/cancelar';
end;

FRESTRequest.Method := rmPost;
FRESTRequest.Params.Clear;
FRESTRequest.Params.AddHeader('Authorization', FChaveAutorizacao);

vObjetoJSON := TJson.ObjectToJsonObject(FNotaCancelar);
FRESTRequest.AddBody(vObjetoJSON);
FRESTRequest.Execute;
*)
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

  with Response.InfCancelamento do
  begin
    Response.ArquivoEnvio := '{"DadosNota":' +
                           '{"Numero":' + NumeroNFSe + ',' +
                            '"Cancelamento":' +
                            '{"Motivo":"' + MotCancelamento + '"},' +
                            '"Prestador":' +
                            '{"InscricaoMunicipal":' + OnlyNumber(Emitente.InscMun) + '}}}';
  end;
end;

procedure TACBrNFSeProviderBauhaus.TratarRetornoCancelaNFSe(
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

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

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

procedure TACBrNFSeProviderBauhaus.PrepararSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
begin
  inherited;

end;

procedure TACBrNFSeProviderBauhaus.TratarRetornoSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
begin
  inherited;

end;

{ TACBrNFSeXWebserviceBauhaus }

procedure TACBrNFSeXWebserviceBauhaus.SetHeaders(aHeaderReq: THTTPHeader);
begin
  aHeaderReq.AddHeader('Authorization',
             TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAutoriz);
end;

function TACBrNFSeXWebserviceBauhaus.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceBauhaus.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceBauhaus.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceBauhaus.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

end.
