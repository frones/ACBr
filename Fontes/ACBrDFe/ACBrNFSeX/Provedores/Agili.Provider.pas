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

unit Agili.Provider;

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
  TACBrNFSeXWebserviceAgili = class(TACBrNFSeXWebserviceNoSoap)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderAgili = class (TACBrNFSeProviderProprio)
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

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  public
    function SimNaoToStr(const t: TnfseSimNao): string; override;
    function StrToSimNao(out ok: boolean; const s: string): TnfseSimNao; override;

    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;
    function RegimeEspecialTributacaoDescricao(const t: TnfseRegimeEspecialTributacao): string; override;

    function ResponsavelRetencaoToStr(const t: TnfseResponsavelRetencao): string; override;
    function StrToResponsavelRetencao(out ok: boolean; const s: string): TnfseResponsavelRetencao; override;

    function ExigibilidadeISSToStr(const t: TnfseExigibilidadeISS): string; override;
    function StrToExigibilidadeISS(out ok: boolean; const s: string): TnfseExigibilidadeISS; override;

    function TipoRPSToStr(const t:TTipoRPS): string; override;
    function StrToTipoRPS(out ok: boolean; const s: string): TTipoRPS; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Agili.GravarXml, Agili.LerXml;

{ TACBrNFSeProviderAgili }

procedure TACBrNFSeProviderAgili.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    DetalharServico := True;
    NumMinRpsEnviar := 2;
  end;

  SetXmlNameSpace('http://www.agili.com.br/nfse_v_1.00.xsd');

  SetNomeXSD('nfse_v_1.00.xsd');
end;

function TACBrNFSeProviderAgili.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Agili.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAgili.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Agili.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAgili.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderAgili.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr));
    AErro.Correcao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr));
  end;
end;

function TACBrNFSeProviderAgili.RegimeEspecialTributacaoDescricao(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  case t of
    retEstimativa                : Result := '2 - Estimativa';
    retCooperativa               : Result := '4 - Cooperativa';
    retMicroempresarioIndividual : Result := '5 - Microempresário Individual (MEI)';
    retMicroempresarioEmpresaPP  : Result := '6 - Microempresário e Empresa de Pequeno Porte (ME EPP)';
  else
    Result := '';
  end;
end;

function TACBrNFSeProviderAgili.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  result := EnumeradoToStr(t, ['', '-2', '-4', '-5', '-6'],
            [retNenhum, retEstimativa, retCooperativa,
             retMicroempresarioIndividual, retMicroempresarioEmpresaPP]);
end;

function TACBrNFSeProviderAgili.StrToRegimeEspecialTributacao(out ok: boolean;
  const s: string): TnfseRegimeEspecialTributacao;
begin
  result := StrToEnumerado(Ok, s, ['-1', '-2', '-4', '-5', '-6'],
               [retNenhum, retEstimativa, retCooperativa,
                retMicroempresarioIndividual, retMicroempresarioEmpresaPP]);
end;

function TACBrNFSeProviderAgili.ResponsavelRetencaoToStr(
  const t: TnfseResponsavelRetencao): string;
begin
  result := EnumeradoToStr(t, ['-1', '-2', '-3'],
                           [rtTomador, rtIntermediario, rtPrestador]);
end;

function TACBrNFSeProviderAgili.StrToResponsavelRetencao(out ok: boolean;
  const s: string): TnfseResponsavelRetencao;
begin
  result := StrToEnumerado(Ok, s, ['-1', '-2', '-3'],
                                     [rtTomador, rtIntermediario, rtPrestador]);
end;

function TACBrNFSeProviderAgili.SimNaoToStr(const t: TnfseSimNao): string;
begin
  Result := EnumeradoToStr(t, ['0', '1'], [snNao, snSim]);
end;

function TACBrNFSeProviderAgili.StrToSimNao(out ok: boolean;
  const s: string): TnfseSimNao;
begin
  Result := StrToEnumerado(ok, s,
                           ['0', '1'],
                           [snNao, snSim]);
end;

function TACBrNFSeProviderAgili.ExigibilidadeISSToStr(
  const t: TnfseExigibilidadeISS): string;
begin
  result := EnumeradoToStr(t, ['-1', '-2', '-3', '-4', '-5', '-6', '-7', '-8'],
              [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao,
               exiImunidade, exiSuspensaDecisaoJudicial,
               exiSuspensaProcessoAdministrativo, exiISSFixo]);
end;

function TACBrNFSeProviderAgili.StrToExigibilidadeISS(out ok: boolean;
  const s: string): TnfseExigibilidadeISS;
begin
  result := StrToEnumerado(Ok, s, ['-1', '-2', '-3', '-4', '-5', '-6', '-7'],
    [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
     exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
end;

function TACBrNFSeProviderAgili.StrToTipoRPS(out ok: boolean;
  const s: string): TTipoRPS;
begin
  result := StrToEnumerado(ok, s, ['-2','-4','-5'],
                           [trRPS, trNFConjugada, trCupom]);
end;

function TACBrNFSeProviderAgili.TipoRPSToStr(const t: TTipoRPS): string;
begin
  result := EnumeradoToStr(t, ['-2', '-4', '-5'],
                           [trRPS, trNFConjugada, trCupom]);
end;

function TACBrNFSeProviderAgili.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := '<DeclaracaoPrestacaoServico>' +
               SeparaDados(aXml, 'InfDeclaracaoPrestacaoServico') +
            '</DeclaracaoPrestacaoServico>';
end;

procedure TACBrNFSeProviderAgili.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    case Response.ModoEnvio of
      meUnitario:
      begin
        TagEnvio := 'GerarNfseEnvio';

        if EstaVazio(ConfigMsgDados.GerarNFSe.xmlns) then
          NameSpace := ''
        else
          NameSpace := ' xmlns="' + ConfigMsgDados.GerarNFSe.xmlns + '"';
      end;
    else
      begin
        TagEnvio := 'EnviarLoteRpsEnvio';

        Xml := '<LoteRps>' +
                 '<NumeroLote>' +
                    Response.NumeroLote +
                 '</NumeroLote>' +
                 '<IdentificacaoPrestador>' +
                   '<ChaveDigital>' +
                      Emitente.WSChaveAcesso +
                   '</ChaveDigital>' +
                   '<CpfCnpj>' +
                      GetCpfCnpj(Emitente.Cnpj) +
                   '</CpfCnpj>' +
                   '<InscricaoMunicipal>' +
                      Emitente.InscMun +
                   '</InscricaoMunicipal>' +
                 '</IdentificacaoPrestador>' +
                 '<QuantidadeRps>' +
                    IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                 '</QuantidadeRps>' +
                 '<ListaRps>' +
                   Xml +
                 '</ListaRps>' +
               '</LoteRps>' +
               '<Versao>1.00</Versao>';

        if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
          NameSpace := ''
        else
          NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"';
      end;
    end;

    Response.ArquivoEnvio := '<' + TagEnvio + NameSpace + '>' +
                           '<UnidadeGestora>' +
                              TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                           '</UnidadeGestora>' +
                           Xml +
                         '</' + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderAgili.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANota: TNotaFiscal;
  NumRps: String;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), tcDatHor);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
        end;
      end;

      if Response.ModoEnvio = meUnitario then
      begin
        ANode := ANode.Childrens.FindAnyNs('Nfse');

        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        AuxNode := ANode;
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

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

procedure TACBrNFSeProviderAgili.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarLote.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarLote.xmlns + '"';

  Response.ArquivoEnvio := '<ConsultarLoteRpsEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<Protocolo>' +
                            Response.Protocolo +
                         '</Protocolo>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Situacao'), tcStr);

          AuxNode := ANode.Childrens.FindAnyNs('ListaNfse');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
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
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');

              if AuxNode <> nil then
              begin
                NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

                ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
                SalvarXmlNfse(ANota);
              end;
            end;
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

procedure TACBrNFSeProviderAgili.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.SerieRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := ACBrStr(Desc103);
    Exit;
  end;

  if EstaVazio(Response.TipoRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod104;
    AErro.Descricao := ACBrStr(Desc104);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarNFSeRps.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';

  Response.ArquivoEnvio := '<ConsultarNfseRpsEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoRps>' +
                           '<Numero>' +
                              Response.NumeroRps +
                           '</Numero>' +
                           '<Serie>' +
                              Response.SerieRps +
                           '</Serie>' +
                           '<Tipo>' +
                              Response.TipoRps +
                           '</Tipo>' +
                         '</IdentificacaoRps>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarNfseRpsEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');
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
            AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
            if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

            AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
            if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

            AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');

            if AuxNode <> nil then
            begin
              NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

              ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumeroRps);

              ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
              SalvarXmlNfse(ANota);
            end;
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

procedure TACBrNFSeProviderAgili.PrepararConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod105;
    AErro.Descricao := ACBrStr(Desc105);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.NumeroFinNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod106;
    AErro.Descricao := ACBrStr(Desc106);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorFaixa;

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.ArquivoEnvio := '<ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<NumeroNfseInicial>' +
                            Response.InfConsultaNFSe.NumeroIniNFSe +
                         '</NumeroNfseInicial>' +
                         '<NumeroNfseFinal>' +
                            Response.InfConsultaNFSe.NumeroFinNFSe +
                         '</NumeroNfseFinal>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarNfseFaixaEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('ListaNfse');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
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
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');

              if AuxNode <> nil then
              begin
                NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

                ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
                SalvarXmlNfse(ANota);
              end;
            end;
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

procedure TACBrNFSeProviderAgili.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := ACBrStr(Desc109);
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

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';

  Response.ArquivoEnvio := '<CancelarNfseEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<PedidoCancelamento>' +
                           '<IdentificacaoNfse>' +
                             '<Numero>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</Numero>' +
                             '<IdentificacaoPrestador>' +
                               '<ChaveDigital>' +
                                  Emitente.WSChaveAcesso +
                               '</ChaveDigital>' +
                               '<CpfCnpj>' +
                                  GetCpfCnpj(Emitente.Cnpj) +
                               '</CpfCnpj>' +
                               '<InscricaoMunicipal>' +
                                  Emitente.InscMun +
                               '</InscricaoMunicipal>' +
                             '</IdentificacaoPrestador>' +
                           '</IdentificacaoNfse>' +
                           '<CodigoCancelamento>' +
                              Response.InfCancelamento.CodCancelamento +
                           '</CodigoCancelamento>' +
                           '<JustificativaCancelamento>' +
                              Response.InfCancelamento.MotCancelamento +
                           '</JustificativaCancelamento>' +
                           '<Versao>1.00</Versao>' +
                         '</PedidoCancelamento>' +
                       '</CancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('ProtocoloRequerimentoCancelamento'), tcStr);
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

procedure TACBrNFSeProviderAgili.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
begin
  with Params do
  begin
    Response.ArquivoEnvio := '<SubstituirNfseEnvio' + NameSpace + '>' +
                           Response.PedCanc +
                           Xml +
                         '</SubstituirNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderAgili.TratarRetornoSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode, AuxNode2: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumRps: String;
  i: Integer;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('RetSubstituicao');

        if AuxNode <> nil then
        begin
          AuxNode2 := AuxNode.Childrens.FindAnyNs('NfseSubstituida');

          if AuxNode2 <> nil then
          begin
            with Response.RetCancelamento do
             DataHora := ObterConteudoTag(AuxNode2.Childrens.FindAnyNs('DataHora'), tcDatHor);
          end;

          AuxNode := AuxNode.Childrens.FindAnyNs('NfseSubstituidora');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
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
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');

              if AuxNode <> nil then
              begin
                NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

                ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

                ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
                SalvarXmlNfse(ANota);
              end;
            end;
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

{ TACBrNFSeXWebserviceAgili }

function TACBrNFSeXWebserviceAgili.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebserviceAgili.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

end.
