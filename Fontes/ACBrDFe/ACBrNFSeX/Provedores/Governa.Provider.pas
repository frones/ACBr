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

unit Governa.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceGoverna = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderGoverna = class (TACBrNFSeProviderProprio)
  private
    FpVersaoArquivo: string;
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

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Governa.GravarXml, Governa.LerXml;

{ TACBrNFSeProviderGoverna }

procedure TACBrNFSeProviderGoverna.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    ConsultaLote := False;
    ConsultaNFSe := False;
    DetalharServico := True;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := True;
      ConsultarRps := True;
      CancelarNfse := True;
    end;

    FpVersaoArquivo := Params.ValorParametro('VersaoArquivo');
  end;

  SetXmlNameSpace('http://tempuri.org/');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderGoverna.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Governa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGoverna.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Governa.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderGoverna.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceGoverna.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderGoverna.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  Codigo: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('tsFlgEtt'), tcStr);

    if Codigo <> 'V' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Codigo;
      AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('tsDesOco'), tcStr);
      AErro.Correcao := '';

      if AErro.Descricao = '' then
        AErro.Descricao := ANodeArray[I].AsString;
    end;
  end;
end;

function TACBrNFSeProviderGoverna.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<tcRps>' + SeparaDados(aXml, 'tcRps') + '</tcRps>';
end;

procedure TACBrNFSeProviderGoverna.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<tcLoteRps>' +
                            '<tsCodCadBic>' +
                               OnlyNumber(Emitente.InscMun) +
                            '</tsCodCadBic>' +
                            '<tsVrsArq>' +
                               FpVersaoArquivo +
                            '</tsVrsArq>' +
                            '<tsChvAcs>' +
                               Emitente.WSChaveAcesso +
                            '</tsChvAcs>' +
                            Xml +
                         '</tcLoteRps>';
  end;
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  AResumo: TNFSeResumoCollectionItem;
  Codigo: String;
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaLoteRps');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('tcRetRps');

      if AuxNode <> nil then
      begin
        ANodeArray := AuxNode.Childrens.FindAllAnyNs('tcInfRetRps');

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

          Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsFlgRet'), tcStr);

          if Codigo <> 'V' then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Codigo;
            AErro.Descricao := ACBrStr(ObterConteudoTag(ANode.Childrens.FindAnyNs('tsDesOco'), tcStr));
            AErro.Correcao := '';

            if AErro.Descricao = '' then
              AErro.Descricao := ACBrStr(ANode.AsString);
          end;

          AResumo := Response.Resumos.New;
          AResumo.NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumRps'), tcStr);
          AResumo.NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumNot'), tcStr);
          AResumo.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsCodVer'), tcStr);
          AResumo.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsFlgRet'), tcStr);
          AResumo.DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsDesOco'), tcStr);

          with Response do
          begin
            NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumRps'), tcStr);
            NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumNot'), tcStr);
            CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsCodVer'), tcStr);
            Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsFlgRet'), tcStr);
            DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsDesOco'), tcStr);
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

procedure TACBrNFSeProviderGoverna.PrepararConsultaNFSeporRps(
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

  if EstaVazio(Response.CodigoVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := ACBrStr(Desc117);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<tcConsultaRPS>' +
                          '<tsCodCadBic>' +
                             OnlyNumber(Emitente.InscMun) +
                          '</tsCodCadBic>' +
                          '<tsVrsArq>' +
                             FpVersaoArquivo +
                          '</tsVrsArq>' +
                          '<tsChvAcs>' +
                             Emitente.WSChaveAcesso +
                          '</tsChvAcs>' +
                          '<tcInfConsultaRPS>' +
                            '<tsNumRPS>' + Response.NumeroRps + '</tsNumRPS>' +
                            '<tsCodVer>' + Response.CodigoVerificacao + '</tsCodVer>' +
                          '</tcInfConsultaRPS>' +
                       '</tcConsultaRPS>';
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
//  i: Integer;
//  NumRps: String;
//  ANota: NotaFiscal;
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaConsultaRPS');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('tcRetConsultaRPS');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('tcInfRetConsultaRPS');

        if AuxNode <> nil then
        begin
          with Response do
          begin
            NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tsNumRps'), tcStr);
            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tsNumNot'), tcStr);
            CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tsCodVer'), tcStr);
            Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tsFlgRet'), tcStr);
            DescSituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tsDesOco'), tcStr);
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


procedure TACBrNFSeProviderGoverna.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := ACBrStr(Desc117);
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

  Response.ArquivoEnvio := '<tcLoteCancelamento>' +
                             '<tsCodCadBic>' +
                               OnlyNumber(Emitente.InscMun) +
                             '</tsCodCadBic>' +
                             '<tsVrsArq>' +
                               FpVersaoArquivo +
                             '</tsVrsArq>' +
                             '<tsChvAcs>' +
                               Emitente.WSChaveAcesso +
                             '</tsChvAcs>' +
                             '<tcNotCan>' +
                               '<tcInfNotCan>' +
                                 '<tsNumNot>' +
                                   Response.InfCancelamento.NumeroNFSe +
                                 '</tsNumNot>' +
                                 '<tsCodVer>' +
                                   Response.InfCancelamento.CodVerificacao +
                                 '</tsCodVer>' +
                                 '<tsDesMotCan>' +
                                   Response.InfCancelamento.MotCancelamento +
                                 '</tsDesMotCan>' +
                               '</tcInfNotCan>' +
                             '</tcNotCan>' +
                           '</tcLoteCancelamento>';
end;

procedure TACBrNFSeProviderGoverna.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  AResumo: TNFSeResumoCollectionItem;
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

      ProcessarMensagemErros(ANode, Response, '', 'tcValidaLoteCancelamento');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('tcRetNotCan');

      if AuxNode <> nil then
      begin
        ANodeArray := AuxNode.Childrens.FindAllAnyNs('tcInfRetNotCan');

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

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumNot'), tcStr);
          AResumo.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsCodVer'), tcStr);
          AResumo.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsFlgRet'), tcStr);
          AResumo.DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsDesOco'), tcStr);

          with Response do
          begin
            NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsNumNot'), tcStr);
            CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsCodVer'), tcStr);
            Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsFlgRet'), tcStr);
            DescSituacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('tsDesOco'), tcStr);
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

{ TACBrNFSeXWebserviceGoverna }

function TACBrNFSeXWebserviceGoverna.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteRps>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarLoteRps>';

  Result := Executar('http://tempuri.org/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResult', 'tcRetornoLoteRps'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceGoverna.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarConsultaRPS>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarConsultaRPS>';

  Result := Executar('http://tempuri.org/RecepcionarConsultaRPS', Request,
                     ['RecepcionarConsultaRPSResult', 'tcRetornoConsultaRPS'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceGoverna.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:RecepcionarLoteNotasCanceladas>';
  Request := Request + '<tem:pArquivoXML>' + XmlToStr(AMSG) + '</tem:pArquivoXML>';
  Request := Request + '</tem:RecepcionarLoteNotasCanceladas>';

  Result := Executar('http://tempuri.org/RecepcionarLoteNotasCanceladas', Request,
                     ['RecepcionarLoteNotasCanceladasResult', 'tcRetornoLoteCancelamento'],
                     ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrNFSeXWebserviceGoverna.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
end;

end.
