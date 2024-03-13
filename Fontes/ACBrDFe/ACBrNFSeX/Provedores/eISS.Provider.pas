{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit eISS.Provider;

interface

uses
  SysUtils, Classes, Variants, StrUtils,
  ACBrBase, ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrJSON,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  TACBrNFSeXWebserviceeISS = class(TACBrNFSeXWebserviceRest)
  private
    FpMetodo: TMetodo;
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarToken(const ACabecalho, AMSG: String): string; override;
    function Recepcionar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProvidereISS = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
    FpMimeType: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); override;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure ProcessarMensagemDeErros(LJson: TACBrJSONObject;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'erros');
  end;

implementation

uses
  synacode,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrCompress,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  eISS.GravarJson, eISS.LerJson;

{ TACBrNFSeProvidereISS }

procedure TACBrNFSeProvidereISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    ConsultaNFSe := False;
    FormatoArqEnvio := tfaJson;
    FormatoArqRetorno := tfaJson;
    FormatoArqEnvioSoap := tfaJson;
    FormatoArqRetornoSoap := tfaJson;
    FormatoArqRecibo := tfaJson;
    FormatoArqNota := tfaJson;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := True;
      GerarToken := True;
    end;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProvidereISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_eISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_eISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebserviceeISS.Create(FAOwner, AMetodo, URL,
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

procedure TACBrNFSeProvidereISS.ProcessarMensagemDeErros(
  LJson: TACBrJSONObject; Response: TNFSeWebserviceResponse;
  const AListTag: string);
var
  JSonErro: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  i: Integer;
  xDetailError: String;
begin
  JSonErro := LJson.AsJSONObject['Message'];

  if not Assigned(JSonErro) then Exit;

  for i := 0 to JSonErro.AsJSONArray['DetailError'].Count - 1 do
    xDetailError := JSonErro.AsJSONArray['DetailError'].Items[i] + #13 + xDetailError;

  AErro := Response.Erros.New;
  AErro.Codigo := Trim(Copy(JSonErro.AsString['Code'], 6, 3));
  AErro.Descricao := TrimRight(JSonErro.AsString['Message'] + #13 +
                               JSonErro.AsString['MessageDev']);
  AErro.Correcao :=  TrimRight(JSonErro.AsString['Detail'] + #13 +
                               JSonErro.AsString['DetailDev'] + #13 +
                               xDetailError);
end;

procedure TACBrNFSeProvidereISS.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  with TACBrNFSeX(FAOwner).Configuracoes.Geral do
  begin
    Response.ArquivoEnvio := '{' +
                              '"AccessKey":"' + Emitente.WSChaveAcesso + '",' +
                              '"Documento":"' + Emitente.CNPJ + '",' +
                              '"CodigoCidade":"' + IntToStr(CodigoMunicipio) + '"' +
                             '}';
  end;

  FpPath := '/Api/Integracao/Token';
  FpMimeType := 'application/json';
  FpMethod := 'POST';
end;

procedure TACBrNFSeProvidereISS.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  AErro: TNFSeEventoCollectionItem;
  json: TACBrJsonObject;
begin
  try
    if (Copy(Response.ArquivoRetorno, 1, 1) = '{') then
    begin
      json := TACBrJsonObject.Parse(Response.ArquivoRetorno);
      try
      {
        if (json.AsString['message'] <> '') then
        begin
          Response.Token := json.AsString['access_token'];
        end;
        }
        if (json.AsString['message'] <> '') then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod999;
          AErro.Descricao := ACBrStr(json.AsString['message']);
          AErro.Correcao := '';
        end;
      finally
        json.Free;
      end;
    end
    else
    begin
      Response.Token := Response.ArquivoRetorno;
      {
      AErro := Response.Erros.New;
      AErro.Codigo := Cod212;
      AErro.Descricao := ACBrStr(Desc212);
      AErro.Correcao := ACBrStr(Response.ArquivoRetorno);
      }
    end;
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod999;
      AErro.Descricao := ACBrStr(Desc999 + E.Message);
    end;
  end;

  Response.Sucesso := (Response.Erros.Count = 0);
end;

function TACBrNFSeProvidereISS.PrepararRpsParaLote(const aXml: string): string;
begin
{
  Result := Copy(aXml, 5, Length(aXml));
  Result := Copy(Result, 1, Length(Result) - 1) + ',';
  }
  Result := aXml + ',';
end;

procedure TACBrNFSeProvidereISS.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
var
  Json: string;
begin
  Json := '[' + Copy(Params.Xml, 1, Length(Params.Xml) -1) + ']';
  Json := '{"Notas":' + Json + '}';
  Json := AplicarLineBreak(Json, '');

  Json := EncodeBase64(Json);
//  Json := EncodeBase64(GZipCompress(Json));

  Response.ArquivoEnvio := Json;

//  FpPath := '/Api/NotaFiscal/Prestador';
  FpPath := '/Api/Importacao/NotaFiscal/Prestador';
  FpMimeType := 'application/json';
  FpMethod := 'POST';
end;

procedure TACBrNFSeProvidereISS.TratarRetornoEmitir
  (Response: TNFSeEmiteResponse);
var
//  jDocument, jNfse: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  json: TACBrJsonObject;
//  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

  try
    if (Copy(Response.ArquivoRetorno, 1, 1) = '{') then
    begin
      json := TACBrJsonObject.Parse(Response.ArquivoRetorno);
      try
      {
        if (json.AsString['message'] <> '') then
        begin
          Response.Token := json.AsString['access_token'];
        end;
        }
        if (json.AsString['message'] <> '') then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod999;
          AErro.Descricao := ACBrStr(json.AsString['message']);
          AErro.Correcao := '';
        end;
      finally
        json.Free;
      end;
    end
    else
    begin
      {
      Response.Token := Response.ArquivoRetorno;
      AErro := Response.Erros.New;
      AErro.Codigo := Cod212;
      AErro.Descricao := ACBrStr(Desc212);
      AErro.Correcao := ACBrStr(Response.ArquivoRetorno);
      }
    end;
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod999;
      AErro.Descricao := ACBrStr(Desc999 + E.Message);
    end;
  end;

  Response.Sucesso := (Response.Erros.Count = 0);
(*
  jDocument := TACBrJSONObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(jDocument, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      if Response.Sucesso then
      begin
        jNfse := jDocument.AsJSONObject['DadosNfse'];

        if Assigned(jNfse) then
        begin
          with Response do
          begin
            NumeroNota := jNfse.AsString['Numero'];

            if AnsiPos('AMBIENTE DE TESTE', jNfse.AsString['Mensagem'] ) > 0 then
            begin
              Sucesso := false;
              NumeroNota := '';
              DescSituacao := 'ATENÇÃO! AMBIENTE DE TESTE PARA VALIDAÇÃO DE INTEGRAÇÃO.';

              AErro := Response.Erros.New;
              AErro.Codigo := Cod999;
              AErro.Descricao := ACBrStr(jNfse.AsString['Mensagem']);
              AErro.Correcao :=  'ENTRAR EM CONTATO COM A PREFEITURA PARA PEDIR A MUDANÇA PARA AMBIENTE EM PRODUÇÃO DO CNPJ DO EMISSOR';
            end;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        Response.Sucesso := False;
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(jDocument);
  end;
*)
end;

{ TACBrNFSeXWebserviceeISS }

procedure TACBrNFSeXWebserviceeISS.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth: string;
begin
  if (FpMetodo <> tmGerarToken) then
  begin
    Auth := 'Bearer ' + TACBrNFSeX(FPDFeOwner).WebService.GerarToken.Token;

    aHeaderReq.AddHeader('Authorization', Auth);
    aHeaderReq.AddHeader('Connection', 'keep-alive');
    aHeaderReq.AddHeader('Accept', '*/*');
  end;
end;

function TACBrNFSeXWebserviceeISS.GerarToken(const ACabecalho, AMSG: String): string;
begin
  FpMetodo := tmGerarToken;
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceeISS.Recepcionar(const ACabecalho, AMSG: String): string;
begin
  FpMetodo := tmRecepcionar;
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceeISS.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := UTF8Decode(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := StringReplace(Result, '"}"}', '"}}', [rfReplaceAll]);
end;

end.
