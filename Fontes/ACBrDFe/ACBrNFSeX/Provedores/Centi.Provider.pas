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

unit Centi.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase,
  ACBrNFSeXClass, ACBrNFSeXConversao, ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceCenti202 = class(TACBrNFSeXWebserviceRest2)
  private
    function GetOperacao: string;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property Operacao: string read GetOperacao;
  end;

  TACBrNFSeProviderCenti202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
  public
    function SituacaoTributariaToStr(const t: TnfseSituacaoTributaria): string; override;
    function StrToSituacaoTributaria(out ok: boolean; const s: string): TnfseSituacaoTributaria; override;
    function SituacaoTributariaDescricao(const t: TnfseSituacaoTributaria): string; override;
  end;

implementation

uses
  ACBrDFeException, ACBrUtil.Strings,
  ACBrXmlDocument,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  Centi.GravarXml, Centi.LerXml;

{ TACBrNFSeProviderCenti202 }

procedure TACBrNFSeProviderCenti202.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
    CancPreencherCodVerificacao := True;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    RpsGerarNFSe := True;
    CancelarNFSe := True;
    RpsSubstituirNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  SetXmlNameSpace('http://www.centi.com.br/files/nfse.xsd');
end;

function TACBrNFSeProviderCenti202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Centi202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCenti202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Centi202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCenti202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceCenti202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderCenti202.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    if InfoCanc.MotCancelamento <> '' then
    begin
      Motivo := '<DescricaoCancelamento>' +
                   Trim(InfoCanc.MotCancelamento) +
                 '</DescricaoCancelamento>';
    end
    else
      Motivo := '';

    Response.ArquivoEnvio := '<CancelarNfseEnvio' + NameSpace + '>' +
                               '<Pedido>' +
                                 '<InfPedidoCancelamento' + '>' +
                                   '<IdentificacaoNfse>' +
                                     '<Numero>' +
                                        InfoCanc.NumeroNFSe +
                                     '</Numero>' +
                                     '<CpfCnpj>' +
                                        GetCpfCnpj(Emitente.CNPJ, '') +
                                     '</CpfCnpj>' +
                                     GetInscMunic(Emitente.InscMun, '') +
                                     '<CodigoMunicipio>' +
                                        IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                                     '</CodigoMunicipio>' +
                                     CodigoVerificacao +
                                     Motivo +
                                     '<Id>' +
                                        InfoCanc.ChaveNFSe +
                                     '</Id>' +
                                   '</IdentificacaoNfse>' +
                                   '<CodigoCancelamento>' +
                                      InfoCanc.CodCancelamento +
                                   '</CodigoCancelamento>' +
                                 '</InfPedidoCancelamento>' +
                               '</Pedido>' +
                             '</CancelarNfseEnvio>';
  end;
end;

function TACBrNFSeProviderCenti202.SituacaoTributariaToStr(
  const t: TnfseSituacaoTributaria): string;
begin
  Result := EnumeradoToStr(t,
                             ['1', '2'],
                             [stRetencao, stNormal]);
end;

function TACBrNFSeProviderCenti202.StrToSituacaoTributaria(out ok: boolean;
  const s: string): TnfseSituacaoTributaria;
begin
  Result := StrToEnumerado(ok, s,
                             ['0', '1', '2', ''],
                             [stNormal, stRetencao, stSubstituicao, stNenhum]);
end;

procedure TACBrNFSeProviderCenti202.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
  I: Integer;
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

      with Response do
      begin
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), FpFormatoDataRecebimento);
        Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
      end;

      if Response.ModoEnvio in [meLoteSincrono, meUnitario] then
      begin
        // Retorno do EnviarLoteRpsSincrono e GerarNfse
        ANode := ANode.Childrens.FindAnyNs('ListaNfse');

        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod202;
          AErro.Descricao := ACBrStr(Desc202);
          Exit;
        end;

        ProcessarMensagemErros(ANode, Response);

        ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

        if not Assigned(ANodeArray) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('Nfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');

          with Response do
          begin
            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
            CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          end;

          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('IdentificacaoRps'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
        end;
      end;

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
end;

procedure TACBrNFSeProviderCenti202.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  Status: string;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
begin
  Status := '';
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      ANode := ANode.Childrens.FindAnyNs('ListaNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('CompNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('Nfse');
      if not Assigned(ANode) or (ANode = nil) then Exit;

      ANode := ANode.Childrens.FindAnyNs('InfNfse');
      if not Assigned(ANode) or (ANode = nil) then Exit;

      Status := ObterConteudoTag(ANode.Childrens.FindAnyNs('Status'), tcStr);

      if Status <> '3' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := ACBrStr(Desc204);
        Exit;
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

procedure TACBrNFSeProviderCenti202.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode, AuxNodeConf: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumNFSe, NumRps: String;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('CompNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('NfseCancelamento');

      if AuxNode <> nil then
      begin
        AuxNodeConf := AuxNode.Childrens.FindAnyNs('Confirmacao');

        if AuxNodeConf = nil then
          AuxNodeConf := AuxNode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

        Response.DataCanc := ObterConteudoTag(AuxNodeConf.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);
        Response.DescSituacao := '';

        if Response.DataCanc > 0 then
          Response.DescSituacao := 'Nota Cancelada';
      end;

      AuxNode := ANode.Childrens.FindAnyNs('Nfse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

        with Response do
        begin
          NumeroNota := NumNFSe;
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if ANota = nil then
        begin
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('IdentificacaoRps'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);
        end;

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
      end
      else
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
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

function TACBrNFSeProviderCenti202.SituacaoTributariaDescricao(
  const t: TnfseSituacaoTributaria): string;
begin
  case t of
    stNormal       : Result := '0 - Não' ;
    stRetencao     : Result := '1 - Sim' ;
    stSubstituicao : Result := '2 - Substituição' ;
  else
    Result := '';
  end;
end;

{ TACBrNFSeXWebserviceCenti202 }

function TACBrNFSeXWebserviceCenti202.GetOperacao: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 2 then
    Result := 'Homologacao'
  else
    Result := '';
end;

function TACBrNFSeXWebserviceCenti202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('http://tempuri.org/IServiceNfse/GerarNfse' + Operacao,
                     Request, ['GerarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceCenti202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('http://tempuri.org/IServiceNfse/ConsultarNfseRps' + Operacao,
                     Request, ['ConsultarNfseRpsResposta'], []);
end;

function TACBrNFSeXWebserviceCenti202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('http://tempuri.org/IServiceNfse/CancelarNfse' + Operacao,
                     Request, ['CancelarNfseResposta'], []);
end;

end.
