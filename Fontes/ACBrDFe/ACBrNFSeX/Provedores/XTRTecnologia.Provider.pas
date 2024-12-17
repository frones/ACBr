{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit XTRTecnologia.Provider;

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

  TACBrNFSeXWebserviceXTRTecnologia = class(TACBrNFSeXWebserviceRest)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderXTRTecnologia = class (TACBrNFSeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemDeErros(LJson: TACBrJSONObject;
                                     Response: TNFSeWebserviceResponse);
  public
    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string; override;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS; override;
  end;

implementation

uses
  synacode,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  XTRTecnologia.GravarJson, XTRTecnologia.LerJson;

{ TACBrNFSeProviderXTRTecnologia }

procedure TACBrNFSeProviderXTRTecnologia.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    DetalharServico := True;
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
    FormatoArqEnvio := tfaJson;
    FormatoArqRetorno := tfaJson;
    FormatoArqEnvioSoap := tfaJson;
    FormatoArqRetornoSoap := tfaJson;
    FormatoArqRecibo := tfaJson;
    FormatoArqNota := tfaJson;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;

    ServicosDisponibilizados.EnviarUnitario := True;
//    ServicosDisponibilizados.ConsultarRps := True;
//    ServicosDisponibilizados.ConsultarNfse := True;
//    ServicosDisponibilizados.CancelarNfse := True;

    Particularidades.PermiteMaisDeUmServico := True;
    Particularidades.PermiteTagOutrasInformacoes := True;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderXTRTecnologia.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_XTRTecnologia.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderXTRTecnologia.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_XTRTecnologia.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderXTRTecnologia.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebserviceXTRTecnologia.Create(FAOwner, AMetodo, URL, FpMethod);
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderXTRTecnologia.ProcessarMensagemDeErros(
  LJson: TACBrJSONObject; Response: TNFSeWebserviceResponse);
var
//  JSonErro: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
//  i: Integer;
//  xDetailError: String;
begin
  if not Assigned(LJson) then Exit;

  AErro := Response.Erros.New;
  AErro.Codigo := Trim(LJson.AsString['message']);
  AErro.Descricao := Trim(LJson.AsString['dados']);
  AErro.Correcao := '';
end;

function TACBrNFSeProviderXTRTecnologia.TipoTributacaoRPSToStr(
  const t: TTipoTributacaoRPS): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4'],
                           [ttTribnoMun, ttTribforaMun,
                            ttTribnoMunIsento, ttTribforaMunIsento]);
end;

function TACBrNFSeProviderXTRTecnologia.StrToTipoTributacaoRPS(out ok: boolean;
  const s: string): TTipoTributacaoRPS;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4'],
                           [ttTribnoMun, ttTribforaMun,
                            ttTribnoMunIsento, ttTribforaMunIsento]);
end;

function TACBrNFSeProviderXTRTecnologia.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderXTRTecnologia.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := Params.Xml;
  FpMethod := 'POST';
  FpPath := '';
end;

procedure TACBrNFSeProviderXTRTecnologia.TratarRetornoEmitir
  (Response: TNFSeEmiteResponse);
var
  jDocument, jNfse: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
//  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

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
end;

procedure TACBrNFSeProviderXTRTecnologia.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.NumeroRps) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod102;
      AErro.Descricao := ACBrStr(Desc102);
      Exit;
    end;

  Response.ArquivoEnvio := '?NumeroRps=' +
    Response.NumeroRps;

  FpPath := Response.ArquivoEnvio;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderXTRTecnologia.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  jDocument, jRps, jNfse, jCancel: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NumRps: string;
  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod201;
      AErro.Descricao := ACBrStr(Desc201);
      Exit
    end;

  jDocument := TACBrJSONObject.Parse(Response.ArquivoRetorno);

  try
    try
      ProcessarMensagemDeErros(jDocument, Response);
      Response.Sucesso := (Response.Erros.Count = 0);

      if Response.Sucesso then
        begin
          jNfse := jDocument.AsJSONObject['DadosNfse'];
          jRps := jNfse.AsJSONObject['Rps'];
          NumRps := jRps.AsString['Numero'];

          if Assigned(jNfse) then
            begin
              with Response do
                begin
                  NumeroNota := jNfse.AsString['NumeroNfse'];
                  SerieNota := jRps.AsString['Serie'];
                  Data := jNfse.AsISODateTime['DataEmissao'];
                  Link := jNfse.AsString['LinkNfse'];
                  Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
                  Protocolo := jNfse.AsString['CodigoValidacao'];
                  Situacao := jNfse.AsString['SituacaoNfse'];
                  NumeroLote := NumRps;

                  if Situacao = '2' then
                    begin
                      DescSituacao := 'Cancelada';
                      jCancel := jNfse.AsJSONObject['Cancelamento'];
                      DataCanc := jCancel.AsISODate['Data'];
                    end
                  else
                    DescSituacao := 'Normal';
                end;

              if NumRps <> '' then
                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
              else
                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

              ANota := CarregarXmlNfse(ANota, Response.ArquivoRetorno);
              SalvarXmlNfse(ANota);
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
end;

procedure TACBrNFSeProviderXTRTecnologia.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Response.ArquivoEnvio := '?NumeroNfse=' +
    Response.InfConsultaNFSe.NumeroIniNFSe;

  FpPath := Response.ArquivoEnvio;
  FpMethod := 'GET';
end;

procedure TACBrNFSeProviderXTRTecnologia.TratarRetornoConsultaNFSeporNumero
  (Response: TNFSeConsultaNFSeResponse);
var
  jDocument, jRps, jNfse, jCancel: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
  NumRps: string;
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
    jDocument := TACBrJSONObject.Parse(Response.ArquivoRetorno);

    try
      try
        ProcessarMensagemDeErros(jDocument, Response);
        Response.Sucesso := (Response.Erros.Count = 0);

        if Response.Sucesso then
        begin
          jNfse := jDocument.AsJSONObject['DadosNfse'];
          jRps := jNfse.AsJSONObject['Rps'];
          NumRps := jRps.AsString['Numero'];

          if Assigned(jNfse) then
          begin
            with Response do
            begin
              NumeroNota := jNfse.AsString['NumeroNfse'];
              SerieNota := jRps.AsString['Serie'];
              Data := jNfse.AsISODateTime['DataEmissao'];
              Link := jNfse.AsString['LinkNfse'];
              Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
              Protocolo := jNfse.AsString['CodigoValidacao'];
              Situacao := jNfse.AsString['SituacaoNfse'];
              NumeroLote := NumRps;

              if Situacao = '2' then
              begin
                DescSituacao := 'Cancelada';
                jCancel := jNfse.AsJSONObject['Cancelamento'];
                DataCanc := jCancel.AsISODate['Data'];
              end
              else
                DescSituacao := 'Normal';
            end;

            if NumRps <> '' then
              ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
            else
              ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

            ANota := CarregarXmlNfse(ANota, Response.ArquivoRetorno);
            SalvarXmlNfse(ANota);
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
  end;
end;

procedure TACBrNFSeProviderXTRTecnologia.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  jo: TACBrJSONObject;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  jo := TACBrJSONObject.Create;
  try
    with Response.InfCancelamento do
      jo
        .AddPairJSONObject('DadosNota', EmptyStr)
        .AsJSONObject['DadosNota']
          .AddPair('Numero', StrToIntDef(NumeroNFSe, 0))
          .AddPair('Cancelamento', TACBrJSONObject.Create
                                     .AddPair('Motivo', MotCancelamento))
          .AddPair('Prestador', TACBrJSONObject.Create
                                  .AddPair('InscricaoMunicipal', StrToIntDef(OnlyNumber(Emitente.InscMun), 0)));
    Response.ArquivoEnvio := jo.ToJSON;
  finally
    jo.Free;
  end;

  FpMethod := 'POST';
  FpPath := '';
end;

procedure TACBrNFSeProviderXTRTecnologia.TratarRetornoCancelaNFSe
  (Response: TNFSeCancelaNFSeResponse);
var
  jDocument, jNfse: TACBrJSONObject;
  AErro: TNFSeEventoCollectionItem;
//  ANota: TNotaFiscal;
begin
  if Response.ArquivoRetorno = '' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod201;
    AErro.Descricao := ACBrStr(Desc201);
    Exit
  end;

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
            DataCanc := jNfse.AsISODate['DataCancelamento'];
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
end;

{ TACBrNFSeXWebserviceXTRTecnologia }

procedure TACBrNFSeXWebserviceXTRTecnologia.SetHeaders(aHeaderReq: THTTPHeader);
begin
  aHeaderReq.AddHeader('Authorization',
             TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAutoriz);
end;

function TACBrNFSeXWebserviceXTRTecnologia.GerarNFSe(const ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceXTRTecnologia.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceXTRTecnologia.ConsultarNFSe(const ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceXTRTecnologia.Cancelar(const ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceXTRTecnologia.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverDeclaracaoXML(Result);
  Result := StringReplace(Result, '"}"}', '"}}', [rfReplaceAll]);
end;

end.
