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
  // Rps não é assinado
  TACBrNFSeXWebserviceIPM = class(TACBrNFSeXWebserviceMulti)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function TesteEnvio(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
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

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

    function AjustarRetorno(const Retorno: string): string;
  end;

  // Rps não é assinado
  TACBrNFSeXWebserviceIPMV110 = class(TACBrNFSeXWebserviceRest)
  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;

  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function TesteEnvio(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderIPMV110 = class (TACBrNFSeProviderIPM)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

  TACBrNFSeXWebserviceIPMV120 = class(TACBrNFSeXWebserviceIPMV110)
  public

  end;

  // Rps é assinado
  TACBrNFSeProviderIPMV120 = class (TACBrNFSeProviderIPM)
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
    ModoEnvio := meLoteAssincrono;
    ConsultaNFSe := False;
  end;

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
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderIPM.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I: Integer;
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
  ANode: TACBrXmlNode;
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

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response do
      begin
        NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcInt);
//        SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcInt);
        Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
        Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
        Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);

        //Código da situação da NFS-e (1-Emitida, 2-Cancelada)
        if (Situacao = '1') or (Situacao = '2') then
          Situacao := '4'
        else
          Situacao := '3';
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
//                           '<numero>' +
//                             '' +
//                           '</numero>' +
//                           '<serie>' +
//                             '' +
//                           '</serie>' +
//                           '<cadastro>' +
//                             '' +
//                           '</cadastro>' +
                         '</pesquisa>' +
                       '</nfse>';
end;

procedure TACBrNFSeProviderIPM.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('nf');

      if ANode <> nil then
      begin
        with Response do
        begin
          NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcInt);
  //        SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcInt);
          Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
          Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
          Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);

          //Código da situação da NFS-e (1-Emitida, 2-Cancelada)
          if (Situacao = '1') or (Situacao = '2') then
            Situacao := '4'
          else
            Situacao := '3';
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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

  Response.XmlEnvio := '<nfse>' +
                         '<nf>' +
                           '<numero>' +
                             Response.InfCancelamento.NumeroNFSe +
                           '</numero>' +
                           '<serie_nfse>' +
                             Response.InfCancelamento.SerieNFSe +
                           '</serie_nfse>' +
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

procedure TACBrNFSeProviderIPM.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      Document.LoadFromXml(Response.XmlRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', 'mensagem');

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response do
      begin
        NumeroNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('numero_nfse'), tcInt);
//        SerieNota := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('serie_nfse'), tcInt);
        Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('data_nfse'), tcDatVcto);
        Link := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('link_nfse'), tcStr);
        Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);
        Situacao := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr);

        //Código da situação da NFS-e (1-Emitida, 2-Cancelada)
        if (Situacao = '1') or (Situacao = '2') then
          Situacao := '4'
        else
          Situacao := '3';
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceIPM }

function TACBrNFSeXWebserviceIPM.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPM.TesteEnvio(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request,
                     ['testeEnviarReturn', 'RetornoEnvioLoteRPS'],
                     []);
end;

function TACBrNFSeXWebserviceIPM.ConsultarLote(ACabecalho,
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

{ TACBrNFSeProviderIPMV120 }

procedure TACBrNFSeProviderIPMV120.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
  end;
end;

function TACBrNFSeProviderIPMV120.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPMV120.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV120.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPMV120.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV120.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPMV120.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceIPMV110 }

procedure TACBrNFSeXWebserviceIPMV110.SetHeaders(aHeaderReq: THTTPHeader);
var
  Auth: string;
begin
  with TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente do
    Auth := 'Basic ' + string(EncodeBase64(AnsiString(WSUser + ':' +
      ParseText(WSSenha, False))));

  aHeaderReq.AddHeader('Authorization', Auth);
end;

function TACBrNFSeXWebserviceIPMV110.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPMV110.TesteEnvio(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPMV110.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

function TACBrNFSeXWebserviceIPMV110.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], []);
end;

{ TACBrNFSeProviderIPMV110 }

procedure TACBrNFSeProviderIPMV110.Configuracao;
begin
  inherited Configuracao;

end;

function TACBrNFSeProviderIPMV110.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_IPMV110.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV110.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_IPMV110.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderIPMV110.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceIPMV110.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

end.
