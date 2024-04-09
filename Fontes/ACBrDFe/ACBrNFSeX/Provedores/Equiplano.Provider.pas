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

unit Equiplano.Provider;

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
  TACBrNFSeXWebserviceEquiplano = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderEquiplano = class (TACBrNFSeProviderProprio)
  private
    FpCodigoCidade: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    function GerarXmlNota(const aXmlRps, aXmlRetorno: string): string;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'listaErros';
                                     const AMessageTag: string = 'erro'); override;
  public
    function SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string; override;
    function StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps; override;
    function SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Equiplano.GravarXml, Equiplano.LerXml;

{ TACBrNFSeProviderEquiplano }

procedure TACBrNFSeProviderEquiplano.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    ModoEnvio := meLoteAssincrono;
    FpCodigoCidade := Params.ValorParametro('CodigoCidade');
    DetalharServico := True;

    ServicosDisponibilizados.EnviarLoteAssincrono := True;
    ServicosDisponibilizados.ConsultarSituacao := True;
    ServicosDisponibilizados.ConsultarLote := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.ConsultarNfse := True;
    ServicosDisponibilizados.CancelarNfse := True;
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarSituacao := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSe := True;
    CancelarNFSe := True;
  end;

  SetXmlNameSpace('http://www.equiplano.com.br/esnfs');

  with ConfigMsgDados do
  begin
    Prefixo := 'es';
    UsarNumLoteConsLote := True;

    LoteRps.InfElemento := 'lote';
    LoteRps.DocElemento := 'enviarLoteRpsEnvio';

    ConsultarSituacao.InfElemento := 'prestador';
    ConsultarSituacao.DocElemento := 'esConsultarSituacaoLoteRpsEnvio';

    ConsultarLote.InfElemento := 'prestador';
    ConsultarLote.DocElemento := 'esConsultarLoteRpsEnvio';

    ConsultarNFSeRps.InfElemento := 'rps';
    ConsultarNFSeRps.DocElemento := 'esConsultarNfsePorRpsEnvio';

    ConsultarNFSe.InfElemento := 'prestador';
     ConsultarNFSe.DocElemento := 'esConsultarNfseEnvio';

    CancelarNFSe.InfElemento := 'prestador';
    CancelarNFSe.DocElemento := 'esCancelarNfseEnvio';

    DadosCabecalho := '1';
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'esRecepcionarLoteRpsEnvio_v01.xsd';
    ConsultarSituacao := 'esConsultarSituacaoLoteRpsEnvio_v01.xsd';
    ConsultarLote := 'esConsultarLoteRpsEnvio_v01.xsd';
    ConsultarNFSeRps := 'esConsultarNfsePorRpsEnvio_v01.xsd';
    ConsultarNFSe := 'esConsultarNfseEnvio_v01.xsd';
    CancelarNFSe := 'esCancelarNfseEnvio_v01.xsd';
  end;
end;

function TACBrNFSeProviderEquiplano.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Equiplano.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEquiplano.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Equiplano.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEquiplano.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEquiplano.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderEquiplano.ProcessarMensagemErros(
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
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('cdMensagem'), tcStr);
    AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('dsMensagem'), tcStr);
    AErro.Correcao := '';
  end;
end;

function TACBrNFSeProviderEquiplano.SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4'],
                           [sLoteNaoProcessado, sLoteProcessadoErro,
                            sLoteProcessadoSucesso, sLoteProcessadoAviso]);
end;

function TACBrNFSeProviderEquiplano.StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4'],
                           [sLoteNaoProcessado, sLoteProcessadoErro,
                            sLoteProcessadoSucesso, sLoteProcessadoAviso]);
end;

function TACBrNFSeProviderEquiplano.SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['Lote Não Processado', 'Lote Processado com Erro',
                            'Lote Processado com Aviso', 'Lote Processado com Sucesso'],
                           [sLoteNaoProcessado, sLoteProcessadoErro,
                            sLoteProcessadoSucesso, sLoteProcessadoAviso]);
end;

function TACBrNFSeProviderEquiplano.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<rps>' + SeparaDados(aXml, 'rps') + '</rps>';
end;

procedure TACBrNFSeProviderEquiplano.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
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

  with Params do
  begin
    Response.ArquivoEnvio := '<es:enviarLoteRpsEnvio' + NameSpace + '>' +
                               '<lote>' +
                                 '<nrLote>' +
                                    Response.NumeroLote +
                                 '</nrLote>' +
                                 '<qtRps>' +
                                    IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                                 '</qtRps>' +
                                 '<nrVersaoXml>' +
                                    '1' +
                                 '</nrVersaoXml>' +
                                 '<prestador>' +
                                   '<nrCnpj>' +
                                      OnlyNumber(Emitente.CNPJ) +
                                   '</nrCnpj>' +
                                   '<nrInscricaoMunicipal>' +
                                      OnlyNumber(Emitente.InscMun) +
                                   '</nrInscricaoMunicipal>' +
                                   '<isOptanteSimplesNacional>' +
                                      SimNaoToStr(TACBrNFSeX(FAOwner).NotasFiscais.items[0].NFSe.OptanteSimplesNacional) +
                                   '</isOptanteSimplesNacional>' +
                                   '<idEntidade>' +
                                      FpCodigoCidade +
                                   '</idEntidade>' +
                                 '</prestador>' +
                                 '<listaRps>' +
                                    Xml +
                                 '</listaRps>' +
                               '</lote>' +
                             '</es:enviarLoteRpsEnvio>';
  end;
end;

function TACBrNFSeProviderEquiplano.GerarXmlNota(const aXmlRps,
  aXmlRetorno: string): string;
var
  aRPS, aNFSE: string;
  i: Integer;
begin
  aRPS  := SeparaDados(aXmlRps, 'rps', False);
  aNFSE := SeparaDados(aXmlRetorno, 'nfse', False);

  i := Pos('<nrEmissorRps', aNFSE);
  if i > 0 then
    aNFSE := Copy(aNFSE, 1, i-1);

  i := Pos('<nrRps', aNFSE);
  if i > 0 then
    aNFSE := Copy(aNFSE, 1, i-1);

  Result := '<compNfse xmlns="http://www.equiplano.com.br/esnfs">' +
              '<nfse>' +
                aNFSE +
                aRPS +
              '</nfse>' +
            '</compNfse>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('protocolo');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtRecebimento'), tcDatHor);
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrProtocolo'), tcStr);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, xConsulta: string;
begin
  if EstaVazio(Response.Protocolo) and EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);

    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarSituacao.xmlns + '"';

  if Response.Protocolo <> '' then
    xConsulta := '<nrProtocolo>' +
                   Response.Protocolo +
                 '</nrProtocolo>'
  else
    xConsulta := '<nrLoteRps>' +
                   Response.NumeroLote +
                 '</nrLoteRps>';

  Response.ArquivoEnvio := '<es:esConsultarSituacaoLoteRpsEnvio' + NameSpace + '>' +
                             '<prestador>' +
                               '<nrInscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</nrInscricaoMunicipal>' +
                               '<cnpj>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</cnpj>' +
                               '<idEntidade>' +
                                  FpCodigoCidade +
                               '</idEntidade>' +
                             '</prestador>' +
                             xConsulta +
                           '</es:esConsultarSituacaoLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  Ok: Boolean;
  Situacao: TSituacaoLoteRps;
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

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response do
      begin
        NumeroLote := ObterConteudoTag(ANode.Childrens.FindAnyNs('nrLoteRps'), tcStr);
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('stLote'), tcStr);
      end;

      Situacao := TACBrNFSeX(FAOwner).Provider.StrToSituacaoLoteRps(Ok, Response.Situacao);
      Response.DescSituacao := TACBrNFSeX(FAOwner).Provider.SituacaoLoteRpsToDescr(Situacao);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, xConsulta: string;
begin
  if EstaVazio(Response.Protocolo) and EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);

    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarLote.xmlns + '"';

  if Response.Protocolo <> '' then
    xConsulta := '<nrProtocolo>' +
                   Response.Protocolo +
                 '</nrProtocolo>'
  else
    xConsulta := '<nrLoteRps>' +
                   Response.NumeroLote +
                 '</nrLoteRps>';

  Response.ArquivoEnvio := '<es:esConsultarLoteRpsEnvio' + NameSpace + '>' +
                             '<prestador>' +
                               '<nrInscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</nrInscricaoMunicipal>' +
                               '<cnpj>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</cnpj>' +
                               '<idEntidade>' +
                                  FpCodigoCidade +
                               '</idEntidade>' +
                             '</prestador>' +
                             xConsulta +
                           '</es:esConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AResumo: TNFSeResumoCollectionItem;
  ANumRps, ACodVer, ANumNfse: String;
  ADataHora: TDateTime;
  i, j, k: Integer;
  aXmlNota, aXmlRetorno: string;
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

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('nfse');

      j := TACBrNFSeX(FAOwner).NotasFiscais.Count;

      if AuxNode <> nil then
      begin
        Response.NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrNfse'), tcStr);
        Response.CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);
        Response.Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtEmissaoNfs'), tcDat);
        Response.NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrRps'), tcStr);

        if j > 0 then
        begin
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[j-1];

          if ANota.NFSe.IdentificacaoRps.Numero = Response.NumeroRps  then
          begin
            if ANota.XmlRps = '' then
              aXmlNota := GerarXmlNota(ANota.XmlNfse, Response.ArquivoRetorno)
            else
              aXmlNota := GerarXmlNota(ANota.XmlRps, Response.ArquivoRetorno);

            ANota.XmlNfse := aXmlNota;

            SalvarXmlNfse(ANota);
          end;
        end;
      end
      else
      begin
        AuxNode := ANode.Childrens.FindAnyNs('listaNfse');

        if AuxNode <> nil then
        begin
          ANodeArray := AuxNode.Childrens.FindAllAnyNs('nfse');
          if not Assigned(ANodeArray) then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit;
          end;

          for i := Low(ANodeArray) to High(ANodeArray) do
          begin
            AuxNode := ANodeArray[i];

            if AuxNode <> nil then
            begin
              ANumRps:= ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrRps'), tcStr);
              ACodVer := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);
              ANumNfse := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrNfse'), tcStr);
              ADataHora := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtEmissaoNfs'), tcDat);

              AResumo := Response.Resumos.New;
              AResumo.NumeroNota := ANumNfse;
              AResumo.CodigoVerificacao := ACodVer;
              AResumo.NumeroRps := ANumRps;
              AResumo.Data := ADataHora;

              if j > 0 then
              begin
                aXmlRetorno := AuxNode.OuterXml;

                for k := 0 to j-1 do
                begin
                  ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[k];

                  if ANota.NFSe.IdentificacaoRps.Numero = ANumRps  then
                  begin
                    if ANota.XmlRps = '' then
                      aXmlNota := GerarXmlNota(ANota.XmlNfse, aXmlRetorno)
                    else
                      aXmlNota := GerarXmlNota(ANota.XmlRps, aXmlRetorno);

                    ANota.XmlNfse := aXmlNota;

                    SalvarXmlNfse(ANota);
                    Exit;
                  end;
                end;
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaNFSeporRps(
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

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';

  Response.ArquivoEnvio := '<es:esConsultarNfsePorRpsEnvio' + NameSpace + '>' +
                             '<rps>' +
                               '<nrRps>' +
                                  Response.NumeroRps +
                               '</nrRps>' +
                               '<nrEmissorRps>' +
                                  '1' +
                               '</nrEmissorRps>' +
                             '</rps>' +
                             '<prestador>' +
                               '<nrInscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</nrInscricaoMunicipal>' +
                               '<cnpj>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</cnpj>' +
                               '<idEntidade>' +
                                  FpCodigoCidade +
                               '</idEntidade>' +
                             '</prestador>' +
                           '</es:esConsultarNfsePorRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANota: TNotaFiscal;
  aXmlNota: string;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('nfse');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrNfse'), tcStr);
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtEmissaoNfs'), tcDatHor);
          NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nrRps'), tcStr);
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('cancelamento');

        if AuxNode <> nil then
        begin
          Response.Cancelamento.DataHora := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtCancelamento'), tcDatHor);
          Response.Cancelamento.Motivo := ObterConteudoTag(AuxNode.Childrens.FindAnyns('dsCancelamento'), tcStr);
        end;

        i := TACBrNFSeX(FAOwner).NotasFiscais.Count;

        if i > 0 then
        begin
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[i-1];

          if ANota.NFSe.IdentificacaoRps.Numero = Response.NumeroRps  then
          begin
            if ANota.XmlRps = '' then
              aXmlNota := GerarXmlNota(ANota.XmlNfse, Response.ArquivoRetorno)
            else
              aXmlNota := GerarXmlNota(ANota.XmlRps, Response.ArquivoRetorno);

            ANota.XmlNfse := aXmlNota;

            SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
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

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.ArquivoEnvio := '<es:esConsultarNfseEnvio' + NameSpace + '>' +
                             '<prestador>' +
                               '<nrInscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</nrInscricaoMunicipal>' +
                               '<cnpj>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</cnpj>' +
                               '<idEntidade>' +
                                  FpCodigoCidade +
                               '</idEntidade>' +
                             '</prestador>' +
                             '<nrNfse>' +
                                Response.InfConsultaNFSe.NumeroIniNFSe +
                             '</nrNfse>' +
                           '</es:esConsultarNfseEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaNFSeporNumero(
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('listaNfse');

      if AuxNode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
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
          AuxNode := ANode.Childrens.FindAnyNs('nfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('nrRps');

          if AuxNode <> nil then
          begin
            NumRps := AuxNode.AsString;

            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

            ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
            SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderEquiplano.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.ArquivoEnvio := '<es:esCancelarNfseEnvio' + NameSpace + '>' +
                             '<prestador>' +
                               '<nrInscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</nrInscricaoMunicipal>' +
                               '<cnpj>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</cnpj>' +
                               '<idEntidade>' +
                                  FpCodigoCidade +
                               '</idEntidade>' +
                             '</prestador>' +
                             '<nrNfse>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</nrNfse>' +
                             '<dsMotivoCancelamento>' +
                                Response.InfCancelamento.MotCancelamento +
                             '</dsMotivoCancelamento>' +
                           '</es:esCancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
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

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
        ProcessarMensagemErros(AuxNode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response.RetCancelamento do
      begin
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('sucesso'), tcStr);
        DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('dtCancelamento'), tcDatHor);
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

{ TACBrNFSeXWebserviceSP }

function TACBrNFSeXWebserviceEquiplano.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esRecepcionarLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esRecepcionarLoteRps>';

  Result := Executar('urn:esRecepcionarLoteRps', Request,
                     ['return', 'esEnviarLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarSituacao(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarSituacaoLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarSituacaoLoteRps>';

  Result := Executar('urn:esConsultarSituacaoLoteRps', Request,
                     ['return', 'esConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarLoteRps>';

  Result := Executar('urn:esConsultarLoteRps', Request,
                     ['return', 'esConsultarLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarNfsePorRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarNfsePorRps>';

  Result := Executar('urn:esConsultarNfsePorRps', Request,
                     ['return', 'esConsultarNfsePorRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarNfse>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarNfse>';

  Result := Executar('urn:esConsultarNfse', Request,
                     ['return', 'esConsultarNfseResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esCancelarNfse>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esCancelarNfse>';

  Result := Executar('urn:esCancelarNfse', Request,
                     ['return', 'esCancelarNfseResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
end;

end.
