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

unit eGoverneISS.Provider;

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
  TACBrNFSeXWebserviceeGoverneISS = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProvidereGoverneISS = class (TACBrNFSeProviderProprio)
  private
    FPCodigoLote: string;
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

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  eGoverneISS.GravarXml, eGoverneISS.LerXml;

{ TACBrNFSeProvidereGoverneISS }

procedure TACBrNFSeProvidereGoverneISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    Identificador := '';
    ModoEnvio := meLoteAssincrono;
    ConsultaNFSe := False;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerChaveAcesso := True;

    ServicosDisponibilizados.EnviarLoteAssincrono := True;
    ServicosDisponibilizados.EnviarUnitario := True;
    ServicosDisponibilizados.ConsultarLote := True;
    ServicosDisponibilizados.CancelarNfse := True;

    Particularidades.PermiteTagOutrasInformacoes := True;
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    Prefixo := 'eis';
    UsarNumLoteConsLote := True;
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProvidereGoverneISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_eGoverneISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereGoverneISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_eGoverneISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereGoverneISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceeGoverneISS.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProvidereGoverneISS.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<eis:NotaFiscal>' +
                SeparaDados(aXml, 'eis:NotaFiscal') +
            '</eis:NotaFiscal>';
end;

procedure TACBrNFSeProvidereGoverneISS.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  AAlerta: TNFSeEventoCollectionItem;
  Mensagem, Codigo: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Erro'), tcStr);

    if Codigo = 'false' then
    begin
      Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

      if Mensagem <> '' then
      begin
        AAlerta := Response.Alertas.New;
        AAlerta.Codigo := '';
        AAlerta.Descricao := ACBrStr(Mensagem);
        AAlerta.Correcao := '';
      end;
    end
    else
    begin
      Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('MensagemErro'), tcStr);

      if Mensagem = '' then
      begin
        Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

        if Mensagem = '' then
          Mensagem := 'Ocorreu um erro, sem retorno do provedor';
      end;

      if Mensagem <> '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '';
        AErro.Descricao := ACBrStr(Mensagem);
        AErro.Correcao := '';
      end;
    end;
  end;
end;

procedure TACBrNFSeProvidereGoverneISS.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.ModoEnvio = meLoteAssincrono then
    begin
      Xml := StringReplace(Xml, 'eis:NotaFiscal', 'eis1:NotaFiscalLoteDTO', [rfReplaceAll]);

      Response.ArquivoEnvio := '<eis:Notas>' +
                                 '<eis1:ChaveAutenticacao>' +
                                    Emitente.WSChaveAcesso +
                                 '</eis1:ChaveAutenticacao>' +
                                 '<eis1:EmailContato>' +
                                    Emitente.DadosEmitente.Email +
                                 '</eis1:EmailContato>' +
                                 '<eis1:Notas>' +
                                    Xml +
                                 '</eis1:Notas>' +
                               '</eis:Notas>';
    end
    else
      Response.ArquivoEnvio := Xml;
  end;
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  ANode: TACBrXmlNode;
  I: Integer;
  AMessageTag: string;
  ANodeArray: TACBrXmlNodeArray;
begin
  FPCodigoLote := '';
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

      if Response.ModoEnvio = meLoteAssincrono then
        AMessageTag := 'EmitirEmLoteResult'
      else
        AMessageTag := 'EmitirResult';

      ProcessarMensagemErros(Document.Root, Response, '', AMessageTag);

      Response.Sucesso := (Response.Erros.Count = 0);

      if Response.Alertas.Count > 0 then
      begin
        Response.NumeroLote := OnlyNumber(RightStrNativeString(Response.Alertas[0].Descricao, 20));
        FPCodigoLote := Response.NumeroLote;
      end;

      ANode := Document.Root.Childrens.FindAnyNs(AMessageTag);

      if ANode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('NotaFiscalGerada');

        if Assigned(ANodeArray) then
        begin
          for I := Low(ANodeArray) to High(ANodeArray) do
          begin
            ANode := ANodeArray[I];

            with Response do
            begin
              NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);
              CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Autenticador'), tcStr);
              Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('Link'), tcStr);
              Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
            end;

            AResumo := Response.Resumos.New;
            AResumo.NumeroNota := Response.NumeroNota;
            AResumo.CodigoVerificacao := Response.CodigoVerificacao;
            AResumo.Link := Response.Link;
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

procedure TACBrNFSeProvidereGoverneISS.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(FPCodigoLote) then
    FPCodigoLote := Response.NumeroLote;

  if EstaVazio(FPCodigoLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<eis:ChaveAutenticacao>' +
                              Emitente.WSChaveAcesso +
                           '</eis:ChaveAutenticacao>' +
                           '<eis:CodigoLote>' +
                              FPCodigoLote +
                           '</eis:CodigoLote>';
  FPCodigoLote := '';
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  ANode: TACBrXmlNode;
  I: Integer;
  ANodeArray: TACBrXmlNodeArray;
  xMensagemErro: string;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'ConsultarLoteResult');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('ConsultarLoteResult');

      if ANode <> nil then
      begin
        ANode := ANode.Childrens.FindAnyNs('NotasGeradas');

        if ANode <> nil then
        begin
          ANodeArray := ANode.Childrens.FindAllAnyNs('NotaFiscalLoteGeradaDTO');

          if Assigned(ANodeArray) then
          begin
            for I := Low(ANodeArray) to High(ANodeArray) do
            begin
              ANode := ANodeArray[I];

              xMensagemErro := ObterConteudoTag(ANode.Childrens.FindAnyNs('MensagemErro'), tcStr);

              if xMensagemErro <> '' then
              begin
                AErro := Response.Erros.New;
                AErro.Codigo := '';
                AErro.Descricao := ACBrStr(xMensagemErro);
              end;

              with Response do
              begin
                NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);

                CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Autenticador'), tcStr);

                Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('Link'), tcStr);
                Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
              end;

              AResumo := Response.Resumos.New;
              AResumo.NumeroNota := Response.NumeroNota;
              AResumo.CodigoVerificacao := Response.CodigoVerificacao;
              AResumo.Link := Response.Link;
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

procedure TACBrNFSeProvidereGoverneISS.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Transacao: Boolean;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  // Se Transacao for True o Ambiente é de Homologação
  Transacao := (TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 2);

  Response.ArquivoEnvio := '<eis:ChaveAutenticacao>' +
                              Emitente.WSChaveAcesso +
                           '</eis:ChaveAutenticacao>' +
                           '<eis:Homologacao>' +
                             LowerCase(BoolToStr(Transacao, True)) +
                           '</eis:Homologacao>' +
                           '<eis:NumeroNota>' +
                              Response.InfCancelamento.NumeroNFSe +
                           '</eis:NumeroNota>';
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'CancelarResult');

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

{ TACBrNFSeXWebserviceeGoverneISS }

function TACBrNFSeXWebserviceeGoverneISS.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:EmitirEmLote>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:EmitirEmLote>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/EmitirEmLote', Request,
                     [],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:Emitir>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:Emitir>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/Emitir', Request,
                     [],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarLote>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:ConsultarLote>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/ConsultarLote', Request,
                     [],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:Cancelar>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:Cancelar>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/Cancelar', Request,
                     [],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverPrefixosDesnecessarios(Result);
end;

end.
