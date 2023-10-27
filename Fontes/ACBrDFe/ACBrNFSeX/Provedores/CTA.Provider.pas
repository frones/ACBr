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

unit CTA.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceCTA200 = class(TACBrNFSeXWebserviceMulti2)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    {
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;
    }

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderCTA200 = class (TACBrNFSeProviderProprio)
  private
    FpMethod: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function DefinirIDLote(const ID: string): string; override;
    function PrepararRpsParaLote(const aXml: string): string; override;

//    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    (*
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    *)

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'a';
                                     const AMessageTag: string = 'mensagem'); override;
  public
    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string; override;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS; override;
  end;

implementation

uses
  DateUtils, synacode,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException, ACBrDFeSSL,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  CTA.GravarXml, CTA.LerXml;

{ TACBrNFSeProviderCTA200 }

procedure TACBrNFSeProviderCTA200.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;

    ModoEnvio := meLoteAssincrono;
    DetalharServico := True;

    ConsultaLote := False;
    ConsultaNFSe := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.00';
    VersaoAtrib := '2.00';
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderCTA200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_CTA200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCTA200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_CTA200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderCTA200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL, xMimeType: string;
begin
  xMimeType := 'text/xml';
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceCTA200.Create(FAOwner, AMetodo, URL, FpMethod,
      xMimeType)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderCTA200.ProcessarMensagemErros(RootNode: TACBrXmlNode;
  Response: TNFSeWebserviceResponse; const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  Mensagem: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode.Childrens.FindAnyNs('Rps');

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if ANodeArray = nil then
    ANodeArray := ANode.Childrens.FindAllAnyNs('ListadeErros');

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Mensagem := ANodeArray[I].AsString;

    if Mensagem <> '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := '';
      AErro.Descricao := ACBrStr(Mensagem);
      AErro.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderCTA200.DefinirIDLote(const ID: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="' + ID + '"'
  else
    Result := '';
end;

function TACBrNFSeProviderCTA200.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<Rps>' + SeparaDados(aXml, 'Rps') + '</Rps>';
end;
{
procedure TACBrNFSeProviderCTA200.PrepararEmitir(Response: TNFSeEmiteResponse);
begin
  inherited;

end;
}
procedure TACBrNFSeProviderCTA200.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  ChaveSeguranca, DataEmissao, Producao: string;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: word;
begin
  FpMethod := 'POST';

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  DecodeDateTime(Now, wAno, wMes, wDia, wHor, wMin, wSeg, wMse);

  DataEmissao :=  FormatFloat('0000', wAno) + '-' +
    FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia) +
    'T' + FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) +
    ':' + FormatFloat('00', wSeg);

  ChaveSeguranca := Emitente.CNPJ + Emitente.WSChaveAcesso + DataEmissao;

  ChaveSeguranca := LowerCase(TACBrNFSeX(FAOwner).SSL.CalcHash(ChaveSeguranca,
                      dgstSHA256, outHexa, False));
  ChaveSeguranca := EncodeBase64(ChaveSeguranca);

  if ConfigGeral.Ambiente = taProducao then
    Producao := '2'
  else
    Producao := '1';

  with Params do
  begin
    Versao := ' versao="' + ConfigWebServices.VersaoAtrib + '"';

    Response.ArquivoEnvio := '<RpsNfse' +  Versao + IdAttr + '>' +
                               '<ChaveSeguranca>' +
                                  ChaveSeguranca +
                               '</ChaveSeguranca>' +
                               '<Producao>' +
                                  Producao +
                               '</Producao>' +
                               '<Prestador>' +
                                 '<DataEmissao>' +
                                    DataEmissao +
                                 '</DataEmissao>' +
                                 '<Cnpj>' +
                                    Emitente.CNPJ +
                                 '</Cnpj>' +
                                 '<InscricaoEstadual>' +
                                    TACBrNFSeX(FAOwner).NotasFiscais.Items[0].NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual +
                                 '</InscricaoEstadual>' +
                                 '<InscricaoMunicipal>' +
                                    Emitente.InscMun +
                                 '</InscricaoMunicipal>' +
                                 '<OptanteSimplesNacional>' +
                                    SimNaoToStr(TACBrNFSeX(FAOwner).NotasFiscais.Items[0].NFSe.OptanteSimplesNacional) +
                                 '</OptanteSimplesNacional>' +
                                 '<IncentivoFiscal>' +
                                    SimNaoToStr(TACBrNFSeX(FAOwner).NotasFiscais.Items[0].NFSe.IncentivadorCultural) +
                                 '</IncentivoFiscal>' +
                               '</Prestador>' +
                               '<ListaRps>' +
                                 Xml +
                               '</ListaRps>' +
                             '</RpsNfse>';
  end;

  Response.ArquivoEnvio := InserirDeclaracaoXMLSeNecessario(Response.ArquivoEnvio);
end;

procedure TACBrNFSeProviderCTA200.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
//  NumRps: String;
//  ANota: TNotaFiscal;
//  I: Integer;
//  NotaCompleta: Boolean;
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

//      NotaCompleta := (Pos('<nfse>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);
      (*
      if False then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
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
            Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := Response.NumeroNota;
          AResumo.SerieNota := Response.SerieNota;
          AResumo.Data := Response.Data;
          AResumo.Link := Response.Link;
          AResumo.Protocolo := Response.Protocolo;
          AResumo.Situacao := Response.Situacao;
          AResumo.DescSituacao := Response.DescSituacao;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
        end;
      end
      else
      begin
      *)
        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('Rps');

          NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
          DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('EstadoDoRPS'), tcStr);
        end;

        AResumo := Response.Resumos.New;
        AResumo.NumeroRps := Response.NumeroRps;
        AResumo.DescSituacao := Response.DescSituacao;
//      end;
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

procedure TACBrNFSeProviderCTA200.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  ChaveSeguranca, DataEmissao, Producao, Versao, IdAttr: string;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: word;
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

  DecodeDateTime(Now, wAno, wMes, wDia, wHor, wMin, wSeg, wMse);

  DataEmissao :=  FormatFloat('0000', wAno) + '-' +
    FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia) +
    'T' + FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) +
    ':' + FormatFloat('00', wSeg);

  ChaveSeguranca := Emitente.CNPJ + Emitente.WSChaveAcesso + DataEmissao;

  ChaveSeguranca := EncodeBase64(SHA1(ChaveSeguranca));

  if ConfigGeral.Ambiente = taProducao then
    Producao := '2'
  else
    Producao := '1';

  Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
            ConfigWebServices.VersaoDados + '"';

  IdAttr := ' Id="' + Response.InfCancelamento.NumeroNFSe + '"';

  Response.ArquivoEnvio := '<CancelaNfse' +  Versao + IdAttr + '>' +
                             '<ChaveSeguranca>' +
                                ChaveSeguranca +
                             '</ChaveSeguranca>' +
                             '<Producao>' +
                                Producao +
                             '</Producao>' +
                             '<Prestador>' +
                               '<DataEmissao>' + DataEmissao + '</DataEmissao>' +
                               '<Cnpj>' + Emitente.CNPJ + '</Cnpj>' +
                             '</Prestador>' +
                             '<Nfse>' +
                               '<IdentificacaoNfse>' +
                                 '<Numero>' +
                                   Response.InfCancelamento.NumeroNFSe +
                                 '</Numero>' +
                                 '<Justificativa>' +
                                   Response.InfCancelamento.MotCancelamento +
                                 '</Justificativa>' +
                               '</IdentificacaoNfse>' +
                             '</Nfse>' +
                           '</CancelaNfse>';
end;

procedure TACBrNFSeProviderCTA200.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  AResumo: TNFSeResumoCollectionItem;
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
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

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
          AErro.Descricao := ACBrStr(Desc203);
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
            Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
            Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := Response.NumeroNota;
          AResumo.SerieNota := Response.SerieNota;
          AResumo.Data := Response.Data;
          AResumo.Link := Response.Link;
          AResumo.Protocolo := Response.Protocolo;
          AResumo.Situacao := Response.Situacao;
          AResumo.DescSituacao := Response.DescSituacao;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
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
          Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;

        AResumo := Response.Resumos.New;
        AResumo.NumeroNota := Response.NumeroNota;
        AResumo.SerieNota := Response.SerieNota;
        AResumo.Data := Response.Data;
        AResumo.Link := Response.Link;
        AResumo.Protocolo := Response.Protocolo;
        AResumo.Situacao := Response.Situacao;
        AResumo.DescSituacao := Response.DescSituacao;
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

function TACBrNFSeProviderCTA200.StrToTipoTributacaoRPS(out ok: boolean;
  const s: string): TTipoTributacaoRPS;
begin
  Result := StrToEnumerado(Ok, s,
                      ['0', '1', '2', '2', '2', '2', '3', '3', '4', '5'],
                      [ttTribnoMun, ttTribforaMun, ttTribnoMunIsento,
                        ttTribforaMunIsento, ttTribnoMunImune, ttTribforaMunImune,
                        ttTribnoMunSuspensa, ttTribforaMunSuspensa,
                        ttSimplesNacional, ttRetidonoMun]);
end;

function TACBrNFSeProviderCTA200.TipoTributacaoRPSToStr(
  const t: TTipoTributacaoRPS): string;
begin
  Result := EnumeradoToStr(t,
                      ['0', '1', '2', '2', '2', '2', '3', '3', '4', '5'],
                      [ttTribnoMun, ttTribforaMun, ttTribnoMunIsento,
                        ttTribforaMunIsento, ttTribnoMunImune, ttTribforaMunImune,
                        ttTribnoMunSuspensa, ttTribforaMunSuspensa,
                        ttSimplesNacional, ttRetidonoMun]);

end;

{ TACBrNFSeXWebserviceCTA200 }

function TACBrNFSeXWebserviceCTA200.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, ['Informacoes'], []);
end;

function TACBrNFSeXWebserviceCTA200.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, ['GerarNfseResposta'], []);
end;

function TACBrNFSeXWebserviceCTA200.TratarXmlRetornado(
  const aXML: string): string;
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

    Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  end
  else
  begin
    Result := inherited TratarXmlRetornado(aXML);

    Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
    Result := RemoverDeclaracaoXML(Result);
    Result := RemoverIdentacao(Result);
    Result := RemoverCaracteresDesnecessarios(Result);
//    Result := SeparaDados(Result, 'Informacoes', True);
  end;
end;

end.
