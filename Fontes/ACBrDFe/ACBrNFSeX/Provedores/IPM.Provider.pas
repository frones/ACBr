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

unit IPM.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrBase, ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  TACBrNFSeXWebserviceIPM = class(TACBrNFSeXWebserviceMulti1)
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderIPM = class (TACBrNFSeProviderProprio)
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

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

    function AjustarRetorno(const Retorno: string): string;
  end;

  TACBrNFSeXWebserviceIPM101 = class(TACBrNFSeXWebserviceMulti2)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderIPM101 = class (TACBrNFSeProviderIPM)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrUtil, synacode,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  IPM.GravarXml, IPM.LerXml;

{ TACBrNFSeProviderIPM }

procedure TACBrNFSeProviderIPM.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    ConsultaNFSe := False;
  end;

  ConfigAssinar.Rps := (ConfigGeral.Params2 = 'AssinarRps');

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'nfse';
      DocElemento := 'nfse';
    end;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderIPM.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPM.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPM.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPM.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_SEM_URL);
end;

procedure TACBrNFSeProviderIPM.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I, j, k: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  aMsg, Codigo: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    aMsg := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('codigo'), tcStr);

    if aMsg = '' then
    begin
      aMsg := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('MensagemRetorno'), tcStr);

      j := Pos('code', aMsg);

      if j > 0 then
       Codigo := Copy(aMsg, j + 6, 3);


      j := Pos('msg', aMsg);

      if j > 0 then
      begin
        AErro := Response.Erros.New;

        AErro.Codigo := Codigo;
        AErro.Descricao := Copy(aMsg, j + 6, Length(aMsg));
        k := Pos(',', AErro.Descricao);
        AErro.Descricao := Copy(AErro.Descricao, 1, k - 2);

        AErro.Correcao := '';
      end;
    end
    else
    begin
      Codigo := Copy(aMsg, 1, 5);

      {
       Codigo = 00001 significa que o processamento ocorreu com sucesso, logo não
       tem erros.
      }
      if Codigo <> '00001' then
      begin
        AErro := Response.Erros.New;

        AErro.Codigo := Codigo;
        AErro.Descricao := Copy(aMsg, 9, Length(aMsg));
        AErro.Correcao := '';
      end;
    end;
  end;
end;

function TACBrNFSeProviderIPM.AjustarRetorno(const Retorno: string): string;
var
  i: Integer;
begin
  i := Pos('<codigo_html>', Retorno);

  if i > 0 then
    Result := Copy(Retorno, 1, i -1) + '</retorno>'
  else
    Result := Retorno;

  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

function TACBrNFSeProviderIPM.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderIPM.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
begin
  Response.XmlEnvio := Params.Xml;
end;

procedure TACBrNFSeProviderIPM.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: NotaFiscal;
  I: Integer;
  NotaCompleta: Boolean;
  AcessoNegado: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.XmlRetorno := AjustarRetorno(Response.XmlRetorno);

      NotaCompleta := (Pos('<nfse>', Response.XmlRetorno) > 0);

      AcessoNegado := (Pos('Acesso Negado', Response.XmlRetorno) > 0);

      if AcessoNegado then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod211;
        AErro.Descricao := Desc211;
        Exit;
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');
          NumRps := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if Assigned(ANota) then
            ANota.XML := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderIPM.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Response.XmlEnvio := '<nfse>' +
                         '<pesquisa>' +
                           '<codigo_autenticidade>' +
                             Response.Protocolo +
                           '</codigo_autenticidade>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  NumRps: String;
  ANota: NotaFiscal;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.XmlRetorno := AjustarRetorno(Response.XmlRetorno);

      NotaCompleta := (Pos('<nfse>', Response.XmlRetorno) > 0);

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('rps');
        NumRps := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('nf');

          NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Link := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderIPM.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  TagSerie: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.CadEconomico) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if ConfigGeral.Versao = ve101 then
    TagSerie := 'serie_nfse'
  else
    TagSerie := 'serie';

  Response.XmlEnvio := '<nfse>' +
                         '<pesquisa>' +
                           '<numero>' +
                             OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                           '</numero>' +
                           '<' + TagSerie + '>' +
                             OnlyNumber(Response.InfConsultaNFSe.SerieNFSe) +
                           '</' + TagSerie + '>' +
                           '<cadastro>' +
                             OnlyNumber(Response.InfConsultaNFSe.CadEconomico) +
                           '</cadastro>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: NotaFiscal;
//  I: Integer;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.XmlRetorno := AjustarRetorno(Response.XmlRetorno);

      NotaCompleta := (Pos('<nfse>', Response.XmlRetorno) > 0);

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        AuxNode := ANode.Childrens.FindAnyNs('rps');
        NumRps := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

        with Response do
        begin
          AuxNode := ANode.Childrens.FindAnyNs('nf');

          NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Link := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
        {
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');
          NumRps := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if Assigned(ANota) then
            ANota.XML := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
        }
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderIPM.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  xSerie: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := Desc112;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigGeral.Versao = ve101 then
    xSerie := '<serie_nfse>' +
                Response.InfCancelamento.SerieNFSe +
              '</serie_nfse>'
  else
    xSerie := '';

  if ConfigGeral.Params1 = 'SolicitarCancelamento' then
  begin
    Response.XmlEnvio := '<solicitacao_cancelamento>' +
                           '<prestador>' +
                             '<cpfcnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</cpfcnpj>' +
                             '<cidade>' +
                               CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                             '</cidade>' +
                           '</prestador>' +
                           '<documentos>' +
                             '<nfse>' +
                               '<numero>' +
                                 Response.InfCancelamento.NumeroNFSe +
                               '</numero>' +
                               '<serie>' +
                                 Response.InfCancelamento.SerieNFSe +
                               '</serie>' +
                               '<observacao>' +
                                 Response.InfCancelamento.MotCancelamento +
                               '</observacao>' +
                             '</nfse>' +
                           '</documentos>' +
                         '</solicitacao_cancelamento>';
  end
  else
  begin
    Response.XmlEnvio := '<nfse>' +
                           '<nf>' +
                             '<numero>' +
                               Response.InfCancelamento.NumeroNFSe +
                             '</numero>' +
                             xSerie +
                             '<situacao>' +
                               'C' +
                             '</situacao>' +
                             '<observacao>' +
                               Response.InfCancelamento.MotCancelamento +
                             '</observacao>' +
                           '</nf>' +
                           '<prestador>' +
                             '<cpfcnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</cpfcnpj>' +
                             '<cidade>' +
                               CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                             '</cidade>' +
                           '</prestador>' +
                         '</nfse>';
  end;
end;

procedure TACBrNFSeProviderIPM.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: NotaFiscal;
  I: Integer;
  NotaCompleta: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.XmlRetorno := AjustarRetorno(Response.XmlRetorno);

      NotaCompleta := (Pos('<nfse>', Response.XmlRetorno) > 0);

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      if NotaCompleta then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
        if not Assigned(ANodeArray) and (Response.Sucesso) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        for I := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[I];
          AuxNode := ANode.Childrens.FindAnyNs('rps');

          if AuxNode <> nil then
            NumRps := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr)
          else
            NumRps := '';

          with Response do
          begin
            AuxNode := ANode.Childrens.FindAnyNs('nf');

            NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
            SerieNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);
            Data := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
            Data := Data + ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcHor);
            Link := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
            Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
            Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
            DescSituacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          end;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          if Assigned(ANota) then
            ANota.XML := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);
        end;
      end
      else
      begin
        with Response do
        begin
          NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcStr);
          SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcStr);
          Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Data := Data + ProcessarConteudoXml(ANode.Childrens.FindAnyNs('hora_nfse'), tcHor);
          Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);
          DescSituacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_descricao_nfse'), tcStr);
          Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceIPM }

function TACBrNFSeXWebserviceIPM.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

{ TACBrNFSeXWebserviceIPM101 }

procedure TACBrNFSeXWebserviceIPM101.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
    Auth := 'Basic ' + string(EncodeBase64(AnsiString(WSUser + ':' +
      ParseText(WSSenha, False))));

  aHeaderReq.AddHeader('Authorization', Auth);
end;

function TACBrNFSeXWebserviceIPM101.GerarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM101.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM101.ConsultarNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM101.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

{ TACBrNFSeProviderIPM101 }

procedure TACBrNFSeProviderIPM101.Configuracao;
begin
  inherited Configuracao;

end;

function TACBrNFSeProviderIPM101.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPM101.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM101.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPM101.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPM101.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPM101.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_SEM_URL);
end;

end.
