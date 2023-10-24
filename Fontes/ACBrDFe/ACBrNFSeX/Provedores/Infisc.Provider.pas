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

unit Infisc.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  TACBrNFSeXWebserviceInfisc = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorChave(ACabecalho, AMSG: string): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderInfisc = class (TACBrNFSeProviderProprio)
  private
    FpSituacao: string;
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

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaNFSeporChave(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporChave(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'motivos';
                                     const AMessageTag: string = 'mot'); override;

  public
    function SimNaoToStr(const t: TnfseSimNao): string; override;
    function StrToSimNao(out ok: boolean; const s: string): TnfseSimNao; override;
  end;

  TACBrNFSeProviderInfisc101 = class (TACBrNFSeProviderInfisc)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
  end;

  TACBrNFSeXWebserviceInfisc201 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderInfisc201 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

  TACBrNFSeXWebserviceInfisc203 = class(TACBrNFSeXWebserviceInfisc201)
  public

  end;

  TACBrNFSeProviderInfisc203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, Infisc.GravarXml, Infisc.LerXml;

{ TACBrNFSeProviderInfisc }

procedure TACBrNFSeProviderInfisc.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    ModoEnvio := meLoteAssincrono;
    ConsultaPorFaixa := True;
    DetalharServico := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.0';
    AtribVerLote := 'versao';
  end;

  ConfigMsgDados.UsarNumLoteConsLote := True;

  SetXmlNameSpace('http://ws.pc.gif.com.br/');
end;

function TACBrNFSeProviderInfisc.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infisc.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceInfisc.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderInfisc.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<NFS-e>' + SeparaDados(aXml, 'NFS-e') + '</NFS-e>';
end;

procedure TACBrNFSeProviderInfisc.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoAtrib + '"';

    Response.ArquivoEnvio := '<envioLote' + Versao + '>' +
                               '<CNPJ>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</CNPJ>' +
                               '<dhTrans>' +
                                  FormatDateTime('yyyy-mm-dd hh:mm:ss', Now) +
                               '</dhTrans>' +
                                Xml +
                             '</envioLote>';
  end;
end;

procedure TACBrNFSeProviderInfisc.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
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

      ANode := Document.Root;

      with Response do
      begin
        NumeroLote := ObterConteudoTag(ANode.Childrens.FindAnyNs('cLote'), tcStr);
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('sit'), tcStr);
      end;

      ProcessarMensagemErros(ANode, Response);

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

procedure TACBrNFSeProviderInfisc.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
begin
  if EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
            ConfigWebServices.VersaoAtrib + '"';

  Response.ArquivoEnvio := '<pedidoStatusLote' + Versao + '>' +
                             '<CNPJ>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                             '<cLote>' +
                                Response.NumeroLote +
                             '</cLote>' +
                           '</pedidoStatusLote>';
end;

procedure TACBrNFSeProviderInfisc.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      with Response do
      begin
        NumeroLote := ObterConteudoTag(ANode.Childrens.FindAnyNs('cLote'), tcStr);
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('sit'), tcStr);
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFS-e');

      if ANodeArray = nil then
        ANodeArray := ANode.Childrens.FindAllAnyNs('NFSe');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];

        Response.idNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('chvAcessoNFSe'), tcStr);
        Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('sit'), tcStr);
        FpSituacao := Response.Situacao;

        AResumo := Response.Resumos.New;
        AResumo.ChaveDFe := Response.idNota;
        AResumo.Situacao := Response.Situacao;

        ProcessarMensagemErros(ANode, Response);

        Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProviderInfisc.PrepararConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse);
var
  Emitente: TEmitenteConfNFSe;
  Versao: string;
  xConsulta: string;
begin
  if ConfigGeral.Versao = ve101 then
  begin
    xConsulta := '<notaInicial>' +
                   Response.InfConsultaNFSe.NumeroIniNFSe +
                 '</notaInicial>' +
                 '<notaFinal>' +
                   Response.InfConsultaNFSe.NumeroFinNFSe +
                 '</notaFinal>';

    if (Response.InfConsultaNFSe.DataInicial <> 0) then
      xConsulta := xConsulta + '<emissaoInicial>' +
                   FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataInicial) +
                 '</emissaoInicial>';

    if (Response.InfConsultaNFSe.DataFinal <> 0) then
      xConsulta := xConsulta + '<emissaoFinal>' +
                   FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataFinal) +
                 '</emissaoFinal>';

    if Response.InfConsultaNFSe.SerieNFSe <> '' then
      xConsulta := xConsulta + '<serieNotaFiscal>' +
                                 Response.InfConsultaNFSe.SerieNFSe +
                               '</serieNotaFiscal>';
  end
  else
  begin
    if (Response.InfConsultaNFSe.NumeroIniNFSe <> '') and
       (Response.InfConsultaNFSe.NumeroFinNFSe <> '') then
    begin
      xConsulta := '<notaInicial>' +
                     Response.InfConsultaNFSe.NumeroIniNFSe +
                   '</notaInicial>' +
                   '<notaFinal>' +
                     Response.InfConsultaNFSe.NumeroFinNFSe +
                   '</notaFinal>';
    end
    else
    begin
      xConsulta := '<emissaoInicial>' +
                     FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataInicial) +
                   '</emissaoInicial>' +
                   '<emissaoFinal>' +
                     FormatDateTime('YYYY-MM-DD', Response.InfConsultaNFSe.DataFinal) +
                   '</emissaoFinal>';
    end;

    if Response.InfConsultaNFSe.SerieNFSe <> '' then
      xConsulta := xConsulta + '<serieNotaFiscal>' +
                                 Response.InfConsultaNFSe.SerieNFSe +
                               '</serieNotaFiscal>';
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorFaixa;

  Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
            ConfigWebServices.VersaoAtrib + '"';

  Response.ArquivoEnvio := '<pedidoLoteNFSe' + Versao + '>' +
                             '<CNPJ>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                             xConsulta +
                           '</pedidoLoteNFSe>';
end;

procedure TACBrNFSeProviderInfisc.TratarRetornoConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  AuxNode: TACBrXmlNode;
  NumNota: String;
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

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFS-e');

      if ANodeArray = nil then
        ANodeArray := ANode.Childrens.FindAllAnyNs('NFSe');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];
        AuxNode := ANode.Childrens.FindAnyNs('infNFSe');

        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Id');

        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        NumNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nNFS-e'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNota);

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

procedure TACBrNFSeProviderInfisc.PrepararConsultaNFSeporChave(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := ACBrStr(Desc118);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorChave;

  Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
            ConfigWebServices.VersaoAtrib + '"';

  Response.ArquivoEnvio := '<pedidoNFSe' + Versao + '>' +
                             '<CNPJ>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                             '<chvAcessoNFS-e>' +
                                Response.InfConsultaNFSe.ChaveNFSe +
                             '</chvAcessoNFS-e>' +
                           '</pedidoNFSe>';
end;

procedure TACBrNFSeProviderInfisc.TratarRetornoConsultaNFSeporChave(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  AuxNode: TACBrXmlNode;
  NumNota: String;
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

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFS-e');

      if ANodeArray = nil then
        ANodeArray := ANode.Childrens.FindAllAnyNs('NFSe');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for i := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[i];
        AuxNode := ANode.Childrens.FindAnyNs('infNFSe');

        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Id');

        if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

        NumNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nNFS-e'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNota);

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

procedure TACBrNFSeProviderInfisc.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
begin
  if EstaVazio(Response.InfCancelamento.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := ACBrStr(Desc118);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := ACBrStr(Desc109);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
            ConfigWebServices.VersaoAtrib + '"';

  Response.ArquivoEnvio := '<pedCancelaNFSe' + Versao + '>' +
                             '<CNPJ>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                             '<chvAcessoNFS-e>' +
                                Response.InfCancelamento.ChaveNFSe +
                             '</chvAcessoNFS-e>' +
                             '<motivo>' +
                                Response.InfCancelamento.CodCancelamento +
                             '</motivo>' +
                           '</pedCancelaNFSe>';
end;

procedure TACBrNFSeProviderInfisc.TratarRetornoCancelaNFSe(
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response do
      begin
        DataCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
        Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);
        Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('sit'), tcStr);

        if Situacao = '100' then
          DescSituacao := 'NFS-e Cancelada';
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

procedure TACBrNFSeProviderInfisc.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  AAlerta: TNFSeEventoCollectionItem;
  Descricao: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('mot'), tcStr);

    if Descricao = '' then
      Descricao := ANodeArray[I].AsString;

    if (Descricao <> '') and (FpSituacao <> '100') then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := '';
      AErro.Descricao := ACBrStr(Descricao);
      AErro.Correcao := '';
    end;

    if (Descricao <> '') and (FpSituacao = '100') then
    begin
      AAlerta := Response.Alertas.New;
      AAlerta.Codigo := '';
      AAlerta.Descricao := ACBrStr(Descricao);
      AAlerta.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderInfisc.SimNaoToStr(const t: TnfseSimNao): string;
begin
  Result := EnumeradoToStr(t, ['N', 'S'], [snNao, snSim]);
end;

function TACBrNFSeProviderInfisc.StrToSimNao(out ok: boolean;
  const s: string): TnfseSimNao;
begin
  Result := StrToEnumerado(ok, s, ['N', 'S'], [snNao, snSim]);
end;

{ TACBrNFSeProviderInfisc101 }

procedure TACBrNFSeProviderInfisc101.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarLote := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSePorChave := True;
    CancelarNFSe := True;

    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.1';
    VersaoAtrib := '1.0';
    AtribVerLote := 'versao';
  end;

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
      InfElemento := 'envioLote';
      DocElemento := 'envioLote';
    end;

    with ConsultarLote do
    begin
      InfElemento := 'pedidoStatusLote';
      DocElemento := 'pedidoStatusLote';
    end;

    with ConsultarNFSePorFaixa do
    begin
      InfElemento := 'pedidoLoteNFSe';
      DocElemento := 'pedidoLoteNFSe';
    end;

    with ConsultarNFSePorChave do
    begin
      InfElemento := 'pedidoNFSe';
      DocElemento := 'pedidoNFSe';
    end;

    with CancelarNFSe do
    begin
      InfElemento := 'pedCancelaNFSe';
      DocElemento := 'pedCancelaNFSe';
    end;
  end;
end;

function TACBrNFSeProviderInfisc101.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infisc101.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc101.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc.Create(Self);
  Result.NFSe := ANFSe;
end;

{ TACBrNFSeProviderInfisc201 }

procedure TACBrNFSeProviderInfisc201.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    CancelarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '2.01';
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    DadosCabecalho := '<cabecalho versao="2.01">' +
                        '<versaoDados>2.01</versaoDados>' +
                      '</cabecalho>';
  end;
end;

function TACBrNFSeProviderInfisc201.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infisc201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc201.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc201.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc201.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceInfisc201.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceInfisc }

function TACBrNFSeXWebserviceInfisc.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<enviarLoteNotasRequest>';
  Request := Request + AMSG;
  Request := Request + '</enviarLoteNotasRequest>';

  Result := Executar('', Request, ['confirmaLote'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<obterCriticaLoteRequest>';
  Request := Request + AMSG;
  Request := Request + '</obterCriticaLoteRequest>';

  Result := Executar('', Request, ['resultadoLote'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<obterLoteNotaFiscalRequest>';
  Request := Request + AMSG;
  Request := Request + '</obterLoteNotaFiscalRequest>';

  Result := Executar('', Request, ['resPedidoLoteNFSe'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.ConsultarNFSePorChave(ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<obterNotaFiscalRequest>';
  Request := Request + AMSG;
  Request := Request + '</obterNotaFiscalRequest>';

  Result := Executar('', Request, ['resPedidoNFSe'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<cancelarNotaFiscalRequest>';
  Request := Request + AMSG;
  Request := Request + '</cancelarNotaFiscalRequest>';

  Result := Executar('', Request, ['resCancelaNFSe'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
end;

{ TACBrNFSeXWebserviceInfisc201 }

function TACBrNFSeXWebserviceInfisc201.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('', Request, ['EnviarLoteRpsResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</RecepcionarLoteRpsSincrono>';

  Result := Executar('', Request, ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</GerarNfse>';

  Result := Executar('', Request, ['GerarNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('', Request, ['ConsultarLoteRpsResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseFaixa>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseFaixa>';

  Result := Executar('', Request, ['ConsultarNfseFaixaResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfsePorRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfsePorRps>';

  Result := Executar('', Request, ['ConsultarNfseRpsResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseServicoPrestado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseServicoPrestado>';

  Result := Executar('', Request, ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseServicoTomado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseServicoTomado>';

  Result := Executar('', Request, ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</CancelarNfse>';

  Result := Executar('', Request, ['CancelarNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</SubstituirNfse>';

  Result := Executar('', Request, ['SubstituirNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfisc201.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := StringReplace(Result, '<Signature>)', '[Signature])', [rfReplaceAll]);

  // Correção dos retornos
  Result := StringReplace(Result,
    '<ConsultarNfseFaixaResposta></ConsultarNfseFaixaResponse>',
    '</ConsultarNfseFaixaResposta></ConsultarNfseFaixaResponse>', [rfReplaceAll]);
  Result := StringReplace(Result,
    '<ConsultarNfseServicoTomadoResposta></ConsultarNfseServicoTomadoResponse>',
    '</ConsultarNfseServicoTomadoResposta></ConsultarNfseServicoTomadoResponse>', [rfReplaceAll]);
end;

{ TACBrNFSeProviderInfisc203 }

procedure TACBrNFSeProviderInfisc203.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  SetXmlNameSpace('http://nfse.abrasf.org.br');

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderInfisc203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infisc203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfisc203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceInfisc203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

end.
