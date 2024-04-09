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

unit Simple.Provider;

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
  TACBrNFSeXWebserviceSimple = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
  public
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderSimple = class (TACBrNFSeProviderProprio)
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

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = ''); override;
  public
    function TipoTributacaoRPSToStr(const t: TTipoTributacaoRPS): string; override;
    function StrToTipoTributacaoRPS(out ok: boolean; const s: string): TTipoTributacaoRPS; override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Simple.GravarXml, Simple.LerXml;

{ TACBrNFSeProviderSimple }

procedure TACBrNFSeProviderSimple.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meLoteSincrono;
    DetalharServico := True;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;

    ServicosDisponibilizados.EnviarLoteSincrono := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.ConsultarNfse := True;
    ServicosDisponibilizados.ConsultarFaixaNfse := True;
    ServicosDisponibilizados.CancelarNfse := True;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderSimple.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Simple.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimple.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Simple.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSimple.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSimple.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderSimple.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  vDescricao: string;
begin
  ANodeArray := RootNode.Childrens.FindAllAnyNs('Nota');

  if ANodeArray = nil then
    ANodeArray := RootNode.Childrens.FindAllAnyNs('CancelamentoNota');

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    vDescricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('sRetorno'), tcStr);

    if vDescricao = '' then
      vDescricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('sRetornoCanc'), tcStr);

    if (vDescricao <> 'Atualizado OK') and (vDescricao <> 'Nota Cancelada') then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := '';
      AErro.Descricao := vDescricao;
      AErro.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderSimple.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderSimple.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
begin
  Response.ArquivoEnvio := '<tNota>' +
                          Params.Xml +
                       '</tNota>';
end;

procedure TACBrNFSeProviderSimple.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'Nota');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Nota');

      if AuxNode <> nil then
      begin
        {
        with Response do
        begin
          Sucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.FindAnyNs('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
              InscricaoPrestador := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ObterConteudoTag(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ObterConteudoTag(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CPF'), tcStr);
              end;

              DataEnvioLote := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
              ValorTotalServico := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ValorTotalServicos'), tcDe2);
            end;
          end;
        end;
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
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderSimple.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<iRPS>' + Response.NumeroRps + '</iRPS>' +
                       '<sCPFCNPJ>' + OnlyNumber(Emitente.CNPJ) + '</sCPFCNPJ>' +
                       '<dDataRecibo>' + '</dDataRecibo>';
end;

procedure TACBrNFSeProviderSimple.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
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

      ProcessarMensagemErros(Document.Root, Response, 'Nota');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];
        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('iNota'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

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

procedure TACBrNFSeProviderSimple.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: PrepararConsultaNFSeporFaixa(Response);
  else
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

      Response.ArquivoEnvio := '<iNota>' + Response.InfConsultaNFSe.NumeroIniNFSe + '</iNota>' +
                           '<sCPFCNPJ>' + OnlyNumber(Emitente.CNPJ) + '</sCPFCNPJ>';
    end;
  end;
end;

procedure TACBrNFSeProviderSimple.TratarRetornoConsultaNFSeporNumero(
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
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: TratarRetornoConsultaNFSeporFaixa(Response);
  else
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

          ProcessarMensagemErros(Document.Root, Response, 'Nota');

          Response.Sucesso := (Response.Erros.Count = 0);

          ANode := Document.Root;

          ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');

          if not Assigned(ANodeArray) then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit;
          end;

          for i := Low(ANodeArray) to High(ANodeArray) do
          begin
            ANode := ANodeArray[i];
            NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('iNota'), tcStr);

            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

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
  end;
end;

procedure TACBrNFSeProviderSimple.PrepararConsultaNFSeporFaixa(
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

  if Response.InfConsultaNFSe.DataFinal = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod116;
    AErro.Descricao := ACBrStr(Desc116);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorFaixa;

  Response.ArquivoEnvio := '<dDataInicial>' +
                         FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataInicial) +
                       '</dDataInicial>' +
                       '<dDataFinal>' +
                         FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataFinal) +
                       '</dDataFinal>' +
                       '<sCPFCNPJ>' + OnlyNumber(Emitente.CNPJ) + '</sCPFCNPJ>';
end;

procedure TACBrNFSeProviderSimple.TratarRetornoConsultaNFSeporFaixa(
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

      ProcessarMensagemErros(Document.Root, Response, 'Nota');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];
        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('iNota'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

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

procedure TACBrNFSeProviderSimple.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := ACBrStr(Desc112);
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

  Response.ArquivoEnvio := '<tCancelamentoNota>' +
                         '<CancelamentoNota>' +
                           '<sRetornoCanc>' + '</sRetornoCanc>' +
                           '<sContribuinteCanc>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</sContribuinteCanc>' +
                           '<iNotaCanc>' +
                             Response.InfCancelamento.NumeroNFSe +
                           '</iNotaCanc>' +
                           '<sSerieCanc>' +
                             Response.InfCancelamento.SerieNFSe +
                           '</sSerieCanc>' +
                           '<dDataCancelamento>' +
                             FormatDateTime('YYYY-MM-DD', Date) +
                           '</dDataCancelamento>' +
                           '<sMotivoCanc>' +
                             Response.InfCancelamento.MotCancelamento +
                           '</sMotivoCanc>' +
                         '</CancelamentoNota>' +
                       '</tCancelamentoNota>';
end;

procedure TACBrNFSeProviderSimple.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'CancelamentoNota');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          xSucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
          Sucesso := not (xSucesso = 'N');
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

function TACBrNFSeProviderSimple.TipoTributacaoRPSToStr(
  const t: TTipoTributacaoRPS): string;
begin
  Result := EnumeradoToStr(t, ['N', 'S', 'I', 'R', 'P', 'T'],
            [ttTribnoMun, ttSimplesNacional, ttTribnoMunIsento, ttRetidonoMun,
            ttTribforaMun, ttExpServicos]);
end;

function TACBrNFSeProviderSimple.StrToTipoTributacaoRPS(out ok: boolean;
  const s: string): TTipoTributacaoRPS;
begin
  Result := StrToEnumerado(Ok, s, ['N', 'S', 'I', 'R', 'P', 'T'],
            [ttTribnoMun, ttSimplesNacional, ttTribnoMunIsento, ttRetidonoMun,
            ttTribforaMun, ttExpServicos]);
end;

{ TACBrNFSeXWebserviceSimple }

function TACBrNFSeXWebserviceSimple.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<iCMC>' + Emitente.InscMun + '</iCMC>' +
              '<sLogin>' + Emitente.WSUser + '</sLogin>' +
              '<sSenha>' + Emitente.WSSenha + '</sSenha>';
  end;
end;

function TACBrNFSeXWebserviceSimple.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:LeRPSeGravaNota xmlns:tem="http://tempuri.org">';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</tem:LeRPSeGravaNota>';

  Result := Executar('', Request, ['LeRPSeGravaNotaResult'], []);
end;

function TACBrNFSeXWebserviceSimple.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultaNotaporRPS xmlns:tem="http://tempuri.org">';
  Request := Request + DadosUsuario;
  Request := Request + AMSG;
  Request := Request + '</tem:ConsultaNotaporRPS>';

  Result := Executar('', Request, ['ConsultaNotaporRPSResult'], []);
end;

function TACBrNFSeXWebserviceSimple.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultaNota xmlns:tem="http://tempuri.org">';
  Request := Request + DadosUsuario;
  Request := Request + AMSG;
  Request := Request + '</tem:ConsultaNota>';

  Result := Executar('', Request, ['ConsultaNotaResult'], []);
end;

function TACBrNFSeXWebserviceSimple.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ListarNotas xmlns:tem="http://tempuri.org">';
  Request := Request + DadosUsuario;
  Request := Request + AMSG;
  Request := Request + '</tem:ListarNotas>';

  Result := Executar('', Request, ['ListarNotasResult'], []);
end;

function TACBrNFSeXWebserviceSimple.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:CancelarNota xmlns:tem="http://tempuri.org">';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</tem:CancelarNota>';

  Result := Executar('', Request, ['CancelarNotaResult'], []);
end;

function TACBrNFSeXWebserviceSimple.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
end;

end.
