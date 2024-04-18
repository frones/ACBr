{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

unit Siappa.Provider;

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
  TACBrNFSeXWebserviceSiappa = class(TACBrNFSeXWebserviceSoap11)
  public
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function GerarToken(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderSiappa = class (TACBrNFSeProviderProprio)
  private
    function GetDadosOpcExecucao: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); override;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); override;

    function AplicarLineBreak(const AXMLRps: String; const ABreak: String): String; override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = ''); override;
  public
    property DadosOpcExecucao: string read GetDadosOpcExecucao;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Siappa.GravarXml, Siappa.LerXml, ACBrNFSeXProviderBase;

{ TACBrNFSeProviderSiappa }

function TACBrNFSeProviderSiappa.GetDadosOpcExecucao: string;
begin
  // (T)estes ou (D)efinitivo
  if ConfigGeral.Ambiente = taHomologacao then
    Result := 'T'
  else
    Result := 'D';
end;

procedure TACBrNFSeProviderSiappa.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meLoteSincrono;

    Autenticacao.RequerLogin := True;
    Autenticacao.RequerChaveAutorizacao := True;

    ServicosDisponibilizados.EnviarLoteAssincrono := True;
    ServicosDisponibilizados.ConsultarNfse := True;
    ServicosDisponibilizados.CancelarNfse := True;
    ServicosDisponibilizados.GerarToken := True;

    Particularidades.PermiteTagOutrasInformacoes := True;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderSiappa.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Siappa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSiappa.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Siappa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSiappa.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSiappa.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderSiappa.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  vDescricao: string;
  vRetorno: string;
begin
  ANodeArray := RootNode.Childrens.FindAllAnyNs(AListTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    // O AMessageTag recebe o prefixo das tags de retorno, pois eles variam entre os métodos
    vDescricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs(AMessageTag + '_out_msg_retorno'), tcStr);

    vRetorno := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs(AMessageTag + '_out_status_retorno'), tcStr);

    // Se o retorno estiver em branco, verifica se é o token foi gerado
    if vRetorno = '' then
    begin
      vRetorno := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs(AMessageTag + '_out_token'), tcStr);

      if vRetorno = '' then
        vRetorno := 'N';
    end;

    if (vRetorno = 'N') then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs(AMessageTag + '_out_codigo_retorno'), tcStr);
      AErro.Descricao := vDescricao;
      AErro.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderSiappa.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderSiappa.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := Params.Xml;
end;

procedure TACBrNFSeProviderSiappa.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
  xSucesso: string;
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

      ProcessarMensagemErros(Document.Root, Response, 'Sdt_ws_001_out_gera_nfse_token', 'ws_001');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Sdt_ws_001_out_gera_nfse_token');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          xSucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_001_out_status_retorno'), tcStr);
          Sucesso := (xSucesso = 'S');

          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_001_out_nfse_numero'), tcStr);
          Data := EncodeDataHora( ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_001_out_nfse_data_hora'), tcStr),
                                  'DD/MM/YYYY HH:NN:SS' );
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_001_out_nfse_cod_validacao'), tcStr);
          Link := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_001_out_nfse_url_emissao'), tcStr);
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

procedure TACBrNFSeProviderSiappa.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if Response.InfConsultaNFSe.DataInicial = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod115;
    AErro.Descricao := ACBrStr(Desc115);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.CodServ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod123;
    AErro.Descricao := ACBrStr(Desc123);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := ACBrStr(Desc117);
    Exit;
  end;

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

  Response.Metodo := tmConsultarNFSe;

  // Atenção: Neste xml apenas o "Sdt_" da tag raiz deve ter o primeiro "S" em maiúsculo
  Response.ArquivoEnvio := '<Sdt_ws_003_in_cons_nfse_token>' +
                           '<ws_003_in_prest_insc_seq>' +
                           Emitente.WSUser +
                           '</ws_003_in_prest_insc_seq>' +
                           '<ws_003_in_prest_cnpj>' +
                           OnlyNumber(Emitente.CNPJ) +
                           '</ws_003_in_prest_cnpj>' +
                           '<ws_003_in_prest_ws_senha>' +
                           Emitente.WSSenha +
                           '</ws_003_in_prest_ws_senha>' +
                           '<ws_003_in_prest_ws_token>' +
                           Emitente.WSChaveAutoriz +
                           '</ws_003_in_prest_ws_token>' +
                           '<ws_003_in_nfse_ano>' +
                           FormatDateTime('YYYY', Response.InfConsultaNFSe.DataInicial) +
                           '</ws_003_in_nfse_ano>' +
                           '<ws_003_in_nfse_mes>' +
                           FormatDateTime('MM', Response.InfConsultaNFSe.DataInicial) +
                           '</ws_003_in_nfse_mes>' +
                           '<ws_003_in_nfse_numero>' +
                           Response.InfConsultaNFSe.NumeroIniNFSe +
                           '</ws_003_in_nfse_numero>' +
                           '<ws_003_in_nfse_cod_especie>' +
                           '10' +
                           '</ws_003_in_nfse_cod_especie>' +
                           '<ws_003_in_nfse_cod_atividade>' +
                           Response.InfConsultaNFSe.CodServ +
                           '</ws_003_in_nfse_cod_atividade>' +
                           '<ws_003_in_nfse_cod_validacao>' +
                           Response.InfConsultaNFSe.CodVerificacao +
                           '</ws_003_in_nfse_cod_validacao>' +
                           '<ws_003_in_nfse_opcao_envio_e_mail>' +
                           'N' +
                           '</ws_003_in_nfse_opcao_envio_e_mail>' +
                           '<ws_003_in_opcao_execucao>' +
                           DadosOpcExecucao +
                           '</ws_003_in_opcao_execucao>' +
                           '</Sdt_ws_003_in_cons_nfse_token>';
end;

procedure TACBrNFSeProviderSiappa.TratarRetornoConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumNFSe: String;
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

      ProcessarMensagemErros(Document.Root, Response, 'Sdt_ws_003_out_cons_nfse_token', 'ws_003');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      ANodeArray := nil;
      
      if Assigned(ANode) then
        ANodeArray := ANode.Childrens.FindAllAnyNs('Sdt_ws_003_out_cons_nfse_token');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + 'Webservice não retornou informações');
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];

        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('ws_003_out_nfse_numero'), tcStr);
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);

        with Response do
        begin
          NumeroNota := NumNFSe;
          Data := EncodeDataHora( ObterConteudoTag(ANode.Childrens.FindAnyNs('ws_003_out_nfse_data_hora'), tcStr),
                                  'DD/MM/YYYY HH:NN:SS' );
          Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('ws_003_out_nfse_url_emissao'), tcStr);
          Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
        end;

        if Assigned(ANota) then
        begin
          if ANota.NFSe.Numero = '' then
            ANota.NFSe.Numero := Response.NumeroNota;

          if ANota.NFSe.Link = '' then
            ANota.NFSe.Link := Response.Link;

          if ANota.NFSe.CodigoVerificacao = '' then
            ANota.NFSe.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('ws_003_out_nfse_cod_validacao'), tcStr);

          ANota.NFSe.StatusRps := srNormal;
          ANota.NFSe.SituacaoNfse := snNormal;

          if Pos('nfs-e de teste está cancelada', ObterConteudoTag(ANode.Childrens.FindAnyNs('ws_003_out_msg_retorno'), tcStr)) > 0 then
          begin
            ANota.NFSe.StatusRps := srCancelado;
            ANota.NFSe.SituacaoNfse := snCancelado;
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

procedure TACBrNFSeProviderSiappa.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if Response.InfCancelamento.DataEmissaoNFSe = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod122;
    AErro.Descricao := ACBrStr(Desc122);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodServ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod123;
    AErro.Descricao := ACBrStr(Desc123);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := ACBrStr(Desc117);
    Exit;
  end;

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

  // Atenção: Neste xml apenas o "Sdt_" da tag raiz deve ter o primeiro "S" em maiúsculo
  Response.ArquivoEnvio := '<Sdt_ws_002_in_canc_nfse_token>' +
                           '<ws_002_in_prest_insc_seq>' +
                           Emitente.WSUser +
                           '</ws_002_in_prest_insc_seq>' +
                           '<ws_002_in_prest_cnpj>' +
                           OnlyNumber(Emitente.CNPJ) +
                           '</ws_002_in_prest_cnpj>' +
                           '<ws_002_in_prest_ws_senha>' +
                           Emitente.WSSenha +
                           '</ws_002_in_prest_ws_senha>' +
                           '<ws_002_in_prest_ws_token>' +
                           Emitente.WSChaveAutoriz +
                           '</ws_002_in_prest_ws_token>' +
                           '<ws_002_in_nfse_ano>' +
                           FormatDateTime('YYYY', Response.InfCancelamento.DataEmissaoNFSe) +
                           '</ws_002_in_nfse_ano>' +
                           '<ws_002_in_nfse_mes>' +
                           FormatDateTime('MM', Response.InfCancelamento.DataEmissaoNFSe) +
                           '</ws_002_in_nfse_mes>' +
                           '<ws_002_in_nfse_numero>' +
                           Response.InfCancelamento.NumeroNFSe +
                           '</ws_002_in_nfse_numero>' +
                           '<ws_002_in_nfse_cod_especie>' +
                           '10' +
                           '</ws_002_in_nfse_cod_especie>' +
                           '<ws_002_in_nfse_cod_atividade>' +
                           Response.InfCancelamento.CodServ +
                           '</ws_002_in_nfse_cod_atividade>' +
                           '<ws_002_in_nfse_cod_validacao>' +
                           Response.InfCancelamento.CodVerificacao +
                           '</ws_002_in_nfse_cod_validacao>' +
                           '<ws_002_in_opcao_execucao>' +
                           DadosOpcExecucao +
                           '</ws_002_in_opcao_execucao>' +
                           '</Sdt_ws_002_in_canc_nfse_token>';
end;

procedure TACBrNFSeProviderSiappa.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
  xSucesso: string;
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

      ProcessarMensagemErros(ANode, Response, 'Sdt_ws_002_out_canc_nfse_token', 'ws_002');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Sdt_ws_002_out_canc_nfse_token');

      if not Assigned(AuxNode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + 'Webservice não retornou informações');
        Exit;
      end;

      if AuxNode <> nil then
      begin
        with Response do
        begin
          xSucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_002_out_status_retorno'), tcStr);
          Sucesso := (xSucesso = 'S');

          RetCancelamento.MsgCanc := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ws_002_out_msg_retorno'), tcStr);

          if Sucesso then
            RetCancelamento.DataHora := Date;
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

procedure TACBrNFSeProviderSiappa.PrepararGerarToken(
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
  Response.ArquivoEnvio := '<Ws_000_in_prest_insc_seq>' +
                           Emitente.WSUser +
                           '</Ws_000_in_prest_insc_seq>' +
                           '<Ws_000_in_prest_cnpj>' +
                           OnlyNumber(Emitente.CNPJ) +
                           '</Ws_000_in_prest_cnpj>' +
                           '<Ws_000_in_prest_ws_senha>' +
                           Emitente.WSSenha +
                           '</Ws_000_in_prest_ws_senha>' +
                           '<Ws_000_in_opc_execucao>' +
                           DadosOpcExecucao +
                           '</Ws_000_in_opc_execucao>';
end;

procedure TACBrNFSeProviderSiappa.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      Document.LoadFromXml('<retorno>' +
                           Response.ArquivoRetorno +
                           '</retorno>');

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'ws_gera_token.ExecuteResponse', 'Ws_000');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('ws_gera_token.ExecuteResponse');

      if not Assigned(AuxNode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      if AuxNode <> nil then
      begin
        with Response do
        begin
          Token := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Ws_000_out_token'), tcStr);

          DataExpiracao := EncodeDataHora( ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Ws_000_out_data_expiracao'), tcStr),
                                           'DD/MM/YYYY HH:NN' );

          Sucesso := (Token <> '');
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

function TACBrNFSeProviderSiappa.AplicarLineBreak(const AXMLRps: String;
  const ABreak: String): String;
begin
  Result := AXMLRps;
  if Trim(Result) <> '' then
  begin
    Result := ChangeLineBreak(AXMLRps, '&#10;');
    Result := StringReplace(Result, '&amp;#10;', '&#10;', [rfReplaceAll]);
  end;
end;

{ TACBrNFSeXWebserviceSiappa }

function TACBrNFSeXWebserviceSiappa.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws_gera_nfse_token.Execute xmlns="issqnwebev3v2">';
  Request := Request + AMSG;
  Request := Request + '</ws_gera_nfse_token.Execute>';

  Result := Executar('', Request, ['ws_gera_nfse_token.ExecuteResponse'], []);
end;

function TACBrNFSeXWebserviceSiappa.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws_consulta_nfse_token.Execute xmlns="issqnwebev3v2">';
  Request := Request + AMSG;
  Request := Request + '</ws_consulta_nfse_token.Execute>';

  Result := Executar('', Request, ['ws_consulta_nfse_token.ExecuteResponse'], []);
end;

function TACBrNFSeXWebserviceSiappa.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws_cancela_nfse_token.Execute xmlns="issqnwebev3v2">';
  Request := Request + AMSG;
  Request := Request + '</ws_cancela_nfse_token.Execute>';

  Result := Executar('', Request, ['ws_cancela_nfse_token.ExecuteResponse'], []);
end;

function TACBrNFSeXWebserviceSiappa.GerarToken(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws_gera_token.Execute xmlns="issqnwebev3v2">';
  Request := Request + AMSG;
  Request := Request + '</ws_gera_token.Execute>';

  Result := Executar('', Request, ['ws_gera_token.ExecuteResponse'], []);
end;

function TACBrNFSeXWebserviceSiappa.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
