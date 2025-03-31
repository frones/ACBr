{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit Smart4.Provider;

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
  TACBrNFSeXWebserviceSmart4 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
  public
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderSmart4 = class (TACBrNFSeProviderProprio)
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

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  SynaCode,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Math, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Smart4.GravarXml, Smart4.LerXml;

{ TACBrNFSeProviderSmart4 }

procedure TACBrNFSeProviderSmart4.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meLoteSincrono;
    DetalharServico := True;
    QuebradeLinha := sLineBreak;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;

    ServicosDisponibilizados.EnviarLoteSincrono := True;
    ServicosDisponibilizados.ConsultarLote := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.CancelarNfse := True;

    Particularidades.PermiteMaisDeUmServico := True;
  end;

  ConfigMsgDados.UsarNumLoteConsLote := True;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderSmart4.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Smart4.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmart4.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Smart4.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSmart4.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSmart4.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderSmart4.ProcessarMensagemErros(
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

  if not Assigned(ANodeArray) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ObterConteudoTag(RootNode.Childrens.FindAnyNs('NFSE'), tcStr);
    AErro.Correcao := '';
  end
  else
  begin
    for I := Low(ANodeArray) to High(ANodeArray) do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := '';
      AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('ERRO'), tcStr);
      AErro.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderSmart4.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<NOTA>' + SeparaDados(aXml, 'NOTA') + '</NOTA>';
end;

procedure TACBrNFSeProviderSmart4.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  xData: string;
  Mes, Ano: Integer;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  xData := SeparaDados(Params.Xml, 'DATAEMISSAO', False);

  Mes := StrToInt(Copy(xData, 4, 2));
  Ano := StrToInt(Copy(xData, 7, 4));

  with Params do
  begin
    Response.ArquivoEnvio := '<NFSE>' +
                               '<IDENTIFICACAO>' +
                                 '<MESCOMP>' +
                                    FormatFloat('00', Mes) +
                                 '</MESCOMP>' +
                                 '<ANOCOMP>' +
                                    FormatFloat('0000', Ano) +
                                 '</ANOCOMP>' +
                                 '<INSCRICAO>' +
                                    Emitente.InscMun +
                                 '</INSCRICAO>' +
                                 '<VERSAO>1.00</VERSAO>' +
                               '</IDENTIFICACAO>' +
                               '<NOTAS>' +
                                 Xml +
                               '</NOTAS>' +
                             '</NFSE>';
  end;
end;

procedure TACBrNFSeProviderSmart4.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  Inconsistencia: Boolean;
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

      Inconsistencia := (Pos('<INCONSISTENCIA>', Response.ArquivoRetorno) > 0);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      if Inconsistencia then
      begin
        ANode := ANode.Childrens.FindAnyNs('Mensagem');

        ProcessarMensagemErros(ANode, Response, 'NFSE', 'INCONSISTENCIA');
      end
      else
        Response.Protocolo := Trim(ObterConteudoTag(ANode.Childrens.FindAnyNs('Mensagem'), tcStr));

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

procedure TACBrNFSeProviderSmart4.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
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

  Response.ArquivoEnvio := '<NFSE>' +
                             '<IDENTIFICACAO>' +
                               '<INSCRICAO>' +
                                  Emitente.InscMun +
                               '</INSCRICAO>' +
                               '<LOTE>' +
                                  Response.NumeroLote +
                               '</LOTE>' +
                             '</IDENTIFICACAO>' +
                           '</NFSE>';
end;

procedure TACBrNFSeProviderSmart4.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
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

      ANode := Document.Root;

      ANode := ANode.Childrens.FindAnyNs('Mensagem');

      ProcessarMensagemErros(ANode, Response, 'NFSE', 'INCONSISTENCIA');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('NFSE');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NOTA');

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

        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('COD'), tcStr);

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

procedure TACBrNFSeProviderSmart4.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
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

  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

//  Response.Metodo := tmConsultarNFSe;

  Response.ArquivoEnvio := '<NFSE>' +
                             '<IDENTIFICACAO>' +
                               '<INSCRICAO>' +
                                  Emitente.InscMun +
                               '</INSCRICAO>' +
                               '<LOTE>' +
                                  Response.NumeroLote +
                               '</LOTE>' +
                               '<RPS>' +
                                  Response.NumeroRps +
                               '</RPS>' +
                             '</IDENTIFICACAO>' +
                           '</NFSE>';
end;

procedure TACBrNFSeProviderSmart4.TratarRetornoConsultaNFSeporRps(
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

      ANode := Document.Root;

      ANode := ANode.Childrens.FindAnyNs('Mensagem');

      ProcessarMensagemErros(ANode, Response, 'NFSE', 'INCONSISTENCIA');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('NFSE');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NOTA');

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

        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('COD'), tcStr);

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

procedure TACBrNFSeProviderSmart4.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if Response.InfCancelamento.NumeroRps = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;
{
  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;
}
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<NFSE>' +
                             '<IDENTIFICACAO>' +
                               '<INSCRICAO>' +
                                  Emitente.InscMun +
                               '</INSCRICAO>' +
                               '<LOTE>' +
                                  Response.InfCancelamento.NumeroLote +
                               '</LOTE>' +
                               '<RPS>' +
                                  IntToStr(Response.InfCancelamento.NumeroRps) +
                               '</RPS>' +
//                               '<OBSERVACAO>' +
//                                  Response.InfCancelamento.MotCancelamento +
//                               '</OBSERVACAO>' +
                             '</IDENTIFICACAO>' +
                           '</NFSE>';
end;

procedure TACBrNFSeProviderSmart4.TratarRetornoCancelaNFSe(
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

      ANode := Document.Root.Childrens.FindAnyNs('Mensagem');
{
      if ANode <> nil then
        ANode := ANode.Childrens.FindAnyNs('NFSE')
      else
        ANode := Document.Root.Childrens.FindAnyNs('NFSE');
}
      if ANode <> nil then
      begin
        ProcessarMensagemErros(ANode, Response, 'NFSE', 'INCONSISTENCIA');

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

{ TACBrNFSeXWebserviceSmart4 }

function TACBrNFSeXWebserviceSmart4.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<nfse:Usuario>' + Emitente.WSUser + '</nfse:Usuario>' +
              '<nfse:Senha>' +
                LowerCase(AsciiToHex(MD5(AnsiString(Emitente.WSSenha)))) +
              '</nfse:Senha>';
  end;
end;

function TACBrNFSeXWebserviceSmart4.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ExecuteRequest>';
  Request := Request + '<nfse:Operacao>1</nfse:Operacao>';
  Request := Request + DadosUsuario;
  Request := Request + '<nfse:Webxml>' + XmlToStr(AMSG) + '</nfse:Webxml>';
  Request := Request + '</nfse:ExecuteRequest>';

  Result := Executar('', Request, [], ['xmlns:nfse="http://com/mbsolutions/webservices/nfse"']);
end;

function TACBrNFSeXWebserviceSmart4.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ExecuteRequest>';
  Request := Request + '<nfse:Operacao>3</nfse:Operacao>';
  Request := Request + DadosUsuario;
  Request := Request + '<nfse:Webxml>' + XmlToStr(AMSG) + '</nfse:Webxml>';
  Request := Request + '</nfse:ExecuteRequest>';

  Result := Executar('', Request, [], ['xmlns:nfse="http://com/mbsolutions/webservices/nfse"']);
end;

function TACBrNFSeXWebserviceSmart4.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ExecuteRequest>';
  Request := Request + '<nfse:Operacao>3</nfse:Operacao>';
  Request := Request + DadosUsuario;
  Request := Request + '<nfse:Webxml>' + XmlToStr(AMSG) + '</nfse:Webxml>';
  Request := Request + '</nfse:ExecuteRequest>';

  Result := Executar('', Request, [], ['xmlns:nfse="http://com/mbsolutions/webservices/nfse"']);
end;

function TACBrNFSeXWebserviceSmart4.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ExecuteRequest>';
  Request := Request + '<nfse:Operacao>2</nfse:Operacao>';
  Request := Request + DadosUsuario;
  Request := Request + '<nfse:Webxml>' + XmlToStr(AMSG) + '</nfse:Webxml>';
  Request := Request + '</nfse:ExecuteRequest>';

  Result := Executar('', Request, [], ['xmlns:nfse="http://com/mbsolutions/webservices/nfse"']);
end;

function TACBrNFSeXWebserviceSmart4.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
