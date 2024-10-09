{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrANe.WebServicesBase;

interface

uses
  Classes, SysUtils,
  {$IFNDEF NOGUI}
   {$IFDEF CLX}
     QDialogs,
   {$ELSE}
     {$IFDEF FMX}
       FMX.Dialogs,
     {$ELSE}
       Dialogs,
     {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  ACBrBase, ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeSSL,
  ACBrXmlDocument, ACBrANe.Conversao;

resourcestring
  ERR_NAO_IMP = 'Serviço não implementado para esta seguradora.';
  ERR_SEM_URL_PRO = 'Não informado a URL de Produção, favor entrar em contato com a Seguradora.';
  ERR_SEM_URL_HOM = 'Não informado a URL de Homologação, favor entrar em contato com a Seguradora.';

type

  TACBrANeWebservice = class
  private
    FPrefixo: string;
    FPath: string;
    FHtmlRetorno: string;

    function GetBaseUrl: string;

  protected
    FHttpClient: TDFeSSLHttpClass;
    FPMethod: string;

    FPFaultNode: string;
    FPFaultCodeNode: string;
    FPFaultMsgNode: string;

    FPConfiguracoes: TConfiguracoes;
    FPDFeOwner: TACBrDFe;
    FPURL: string;
    FPMimeType: string;
    FPEnvio: string;
    FPRetorno: string;
    FUseOuterXml: Boolean;

    FPArqEnv: string;
    FPArqResp: string;
    FPMsgOrig: string;

    procedure FazerLog(const Msg: string; Exibir: Boolean = False); virtual;
    procedure GerarException(const Msg: string; E: Exception = nil); virtual;
    function GetSoapBody(const Response: string): string; virtual;

    function GerarPrefixoArquivo: string; virtual;

    function GravarJSON(NomeArquivo: string; ConteudoXML: string;
      const aPath: string = ''): Boolean;

    procedure SalvarEnvio(ADadosSoap, ADadosMsg: string); virtual;
    procedure SalvarRetornoWebService(ADadosSoap: string); virtual;
    procedure SalvarRetornoDadosMsg(ADadosMsg: string); virtual;

    procedure SetHeaders(aHeaderReq: THTTPHeader); virtual;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; virtual; abstract;

    function ExtrairRetorno(const ARetorno: string; responseTag: array of string): string; virtual;
    function TratarXmlRetornado(const aXML: string): string; virtual;
    function RetornaHTMLNota(const Retorno: string): string;

    procedure VerificarErroNoRetorno(const ADocument: TACBrXmlDocument); virtual;
    procedure UsarCertificado; virtual;
    procedure EnviarDados(const SoapAction: string); virtual;
    procedure EnvioInterno(var CodigoErro, CodigoInterno: Integer); virtual;
    procedure ConfigurarHttpClient; virtual;
    procedure LevantarExcecaoHttp; virtual;

    function Executar(const SoapAction, Message, responseTag: string): string; overload;
    function Executar(const SoapAction, Message, responseTag, namespace: string): string; overload;
    function Executar(const SoapAction, Message, responseTag: string;
                                  namespace: array of string): string; overload;
    function Executar(const SoapAction, Message: string;
                      responseTag, namespace: array of string): string; overload;
    function Executar(const SoapAction, Message, SoapHeader: string;
                      responseTag, namespace: array of string): string; overload;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST');

    function Enviar(const ACabecalho, AMSG: string): string; virtual;
    function Consultar(const ACabecalho, AMSG: string): string; virtual;

    property URL: string read FPURL;
    property BaseURL: string read GetBaseUrl;
    property MimeType: string read FPMimeType;
    property Envio: string read FPEnvio;
    property Retorno: string read FPRetorno;
    property Prefixo: string read FPrefixo write FPrefixo;
    property Method: string read FPMethod;
    property Path: string read FPath write FPath;
    property HtmlRetorno: string read FHtmlRetorno;

  end;

  TACBrANeWebserviceSoap11 = class(TACBrANeWebservice)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

  TACBrANeWebserviceSoap12 = class(TACBrANeWebservice)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/soap+xml');

  end;

  TACBrANeWebserviceNoSoap = class(TACBrANeWebservice)
  protected
    function GetSoapBody(const Response: string): string; override;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/xml');

  end;

  TACBrANeWebserviceRest = class(TACBrANeWebserviceNoSoap)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/json');

  end;

  TACBrANeWebserviceRest2 = class(TACBrANeWebserviceNoSoap)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/json');

  end;

  TACBrANeWebserviceMulti1 = class(TACBrANeWebserviceNoSoap)
  protected
    FPBound: string;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

  TACBrANeWebserviceMulti2 = class(TACBrANeWebserviceNoSoap)
  protected
    FPBound: string;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

implementation

uses
  IniFiles, StrUtils, synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrConsts, ACBrDFeException, ACBrXmlBase,
  ACBrANe, ACBrANeConfiguracoes;

{ TACBrANeWebservice }

constructor TACBrANeWebservice.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string);
begin
  FPDFeOwner := AOwner;
  if Assigned(AOwner) then
    FPConfiguracoes := AOwner.Configuracoes;

  FUseOuterXml := True;

  FPFaultNode := 'Fault';
  FPFaultCodeNode := 'faultcode';
  FPFaultMsgNode := 'faultstring';
  Path := '';

  case AMetodo of
    tmEnviar:
      begin
        FPArqEnv := 'ped-ANe';
        FPArqResp := 'res-ANe';
      end;

    tmConsultar:
      begin
        FPArqEnv := 'con-sit';
        FPArqResp := 'sit';
      end;
  end;

  FPURL := AURL;
  FPMethod := AMethod;
end;

procedure TACBrANeWebservice.FazerLog(const Msg: string; Exibir: Boolean);
var
  Tratado: Boolean;
begin
  if (Msg <> '') then
  begin
    FPDFeOwner.FazerLog(Msg, Tratado);

    if Tratado then Exit;

    {$IFNDEF NOGUI}
    if Exibir and FPConfiguracoes.WebServices.Visualizar then
      ShowMessage(ACBrStr(Msg));
    {$ENDIF}
  end;
end;

procedure TACBrANeWebservice.GerarException(const Msg: string; E: Exception);
begin
  FPDFeOwner.GerarException(ACBrStr(Msg), E);
end;

function TACBrANeWebservice.GetBaseUrl: string;
var
  i:Integer;
begin
  Result := '';

  if EstaVazio(Url) then Exit;

  i := Pos('//', Url);

  if i>0 then
    i := PosEx('/', Url, i+2)
  else
    i := Pos('/', Url);

  if i=0 then
    i := Length(Url);

  Result := Copy(Url, 1, i);
end;

function TACBrANeWebservice.GerarPrefixoArquivo: string;
begin
  if FPrefixo = '' then
    Result := FormatDateTime('yyyymmddhhnnss', Now)
  else
    Result := TiraPontos(FPrefixo);
end;

function TACBrANeWebservice.GravarJSON(NomeArquivo, ConteudoXML: string;
  const aPath: string): Boolean;
var
  SoNome, SoPath: string;
begin
  Result := False;
  try
    SoNome := ExtractFileName(NomeArquivo);
    if EstaVazio(SoNome) then
      raise EACBrDFeException.Create('Nome de arquivo não informado');

    SoPath := ExtractFilePath(NomeArquivo);
    if EstaVazio(SoPath) then
      SoPath := aPath;
    if EstaVazio(SoPath) then
      SoPath := FPConfiguracoes.Arquivos.PathSalvar;

    SoPath := PathWithDelim(SoPath);

    ConteudoXML := StringReplace(ConteudoXML, '<-><->', '', [rfReplaceAll]);

    if not DirectoryExists(SoPath) then
      ForceDirectories(SoPath);

    NomeArquivo := SoPath + SoNome;

    WriteToTXT(NomeArquivo, ConteudoXML, False, False);
    Result := True;
  except
    on E: Exception do
      GerarException('Erro ao salvar.', E);
  end;
end;

procedure TACBrANeWebservice.SalvarEnvio(ADadosSoap, ADadosMsg: string);
var
  Prefixo, ArqEnv, Extensao, ExtensaoSoap: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqEnv = '' then Exit;

  Extensao := '.xml';

  ExtensaoSoap := '.xml';

  ConteudoEhXml := True;

  Prefixo := GerarPrefixoArquivo;

  if FPConfiguracoes.Geral.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqEnv;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosMsg) then
        ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);

      FPDFeOwner.Gravar(ArqEnv + Extensao, ADadosMsg, Path);
    end
    else
      GravarJSON(ArqEnv + Extensao, ADadosMsg, Path);
  end;

  if FPConfiguracoes.WebServices.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqEnv;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosSoap) then
        ADadosSoap := RemoverDeclaracaoXML(ADadosSoap);

      FPDFeOwner.Gravar(ArqEnv + '-soap' + ExtensaoSoap, ADadosSoap, Path);
    end
    else
      GravarJSON(ArqEnv + '-soap' + ExtensaoSoap, ADadosSoap, Path);
  end;
end;

procedure TACBrANeWebservice.SalvarRetornoDadosMsg(ADadosMsg: string);
var
  Prefixo, ArqEnv, Extensao: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqResp = '' then Exit;

  Extensao := '.xml';

  ConteudoEhXml := True;

  Prefixo := GerarPrefixoArquivo;

  if FPDFeOwner.Configuracoes.Geral.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqResp;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosMsg) then
        ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);

      FPDFeOwner.Gravar(ArqEnv + Extensao, ADadosMsg, Path);
    end
    else
      GravarJSON(ArqEnv + Extensao, ADadosMsg, Path);
  end;
end;

procedure TACBrANeWebservice.SalvarRetornoWebService(ADadosSoap: string);
var
  Prefixo, ArqEnv, Extensao: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqResp = '' then Exit;

  Extensao := '.xml';
    ConteudoEhXml := True;

  Prefixo := GerarPrefixoArquivo;

  if FPDFeOwner.Configuracoes.WebServices.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqResp;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosSoap) then
        ADadosSoap := RemoverDeclaracaoXML(ADadosSoap);

      FPDFeOwner.Gravar(ArqEnv + '-soap' + Extensao, ADadosSoap, Path);
    end
    else
      GravarJSON(ArqEnv + '-soap' + Extensao, ADadosSoap, Path);
  end;
end;

procedure TACBrANeWebservice.SetHeaders(aHeaderReq: THTTPHeader);
begin
  if TACBrANe(FPDFeOwner).Provider.ConfigGeral.UseAuthorizationHeader then
    aHeaderReq.AddHeader('Authorization', TConfiguracoesANe(FPConfiguracoes).Geral.CNPJEmitente);
end;

function TACBrANeWebservice.GetSoapBody(const Response: string): string;
begin
  Result := SeparaDados(Response, 'Body');

  if Result = '' then
    Result := Response;
end;

procedure TACBrANeWebservice.LevantarExcecaoHttp;
var
  aRetorno: TACBrXmlDocument;
begin
  // Verifica se o ResultCode é: 200 OK; 201 Created; 202 Accepted
  // https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

//  if not (HttpClient.HTTPResultCode in [200..202]) then
//    raise EACBrDFeException.Create('Erro de Conexão.');

  if not XmlEhUTF8(FPRetorno) then
    Exit;

  if not (FHttpClient.HTTPResultCode in [200..202]) then
  begin
    aRetorno := TACBrXmlDocument.Create;
    try
      aRetorno.LoadFromXml(FPRetorno);
      VerificarErroNoRetorno(aRetorno);
    finally
      aRetorno.Free;
    end;
  end;
end;

procedure TACBrANeWebservice.VerificarErroNoRetorno(const ADocument: TACBrXmlDocument);
var
  ANode: TACBrXmlNode;
  aMsg, xFaultCodeNode, xFaultMsgNode, xCode, xReason, xDetail: string;
begin
  if ADocument.Root.LocalName = FPFaultNode then
    ANode := ADocument.Root
  else
    ANode := ADocument.Root.Childrens.FindAnyNs(FPFaultNode);

  {
    Se o ANode for igual a nil significa que não foi retornado nenhum
    Grupo/Elemento de erro no Soap, logo não tem erro Soap a ser apresentado.
  }

  if ANode = nil then
    Exit;

  xFaultCodeNode := ObterConteudoTag(ANode.Childrens.FindAnyNs(FPFaultCodeNode), tcStr);
  xFaultMsgNode := ObterConteudoTag(ANode.Childrens.FindAnyNs(FPFaultMsgNode), tcStr);

  xCode := ObterConteudoTag(ANode.Childrens.FindAnyNs('Code'), tcStr);
  xReason := ObterConteudoTag(ANode.Childrens.FindAnyNs('Reason'), tcStr);
  xDetail := ObterConteudoTag(ANode.Childrens.FindAnyNs('Detail'), tcStr);

  if (xFaultCodeNode <> '') or (xFaultMsgNode <> '') then
    aMsg := IfThen(xFaultCodeNode = '', '999', xFaultCodeNode) + ' - ' +
            IfThen(xFaultMsgNode = '', 'Erro Desconhecido.', xFaultMsgNode)
  else
    aMsg := xCode + ' - ' + xReason + ' - ' + xDetail;

  raise EACBrDFeException.Create(aMsg);
end;

function TACBrANeWebservice.ExtrairRetorno(const ARetorno: string;
  responseTag: array of string): string;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  I: Integer;
  xRetorno: string;
begin
  Result := '';

  xRetorno := TratarXmlRetornado(ARetorno);

  if xRetorno = '' then
    Exit;

  if not StringIsXML(xRetorno) then
  begin
    Result := xRetorno;
    Exit;
  end;

  if (Length(responseTag) = 0) then
  begin
    Result := xRetorno;
    Exit;
  end;

  Document := TACBrXmlDocument.Create;
  try
    Document.LoadFromXml(xRetorno);

    VerificarErroNoRetorno(Document);
    ANode := Document.Root;

    if ANode.Name <> 'a' then
    begin
      for I := Low(responseTag) to High(responseTag) do
      begin
        if ANode <> nil then
          ANode := ANode.Childrens.FindAnyNs(responseTag[I]);
      end;

      if ANode = nil then
        ANode := Document.Root.Childrens.FindAnyNs(responseTag[0]);

      if ANode = nil then
        ANode := Document.Root;
    end;

    if ANode <> nil then
    begin
      if FUseOuterXml then
        Result := ANode.OuterXml
      else
        Result := ANode.Content;
    end;
  finally
    Document.Free;
  end;
end;

function TACBrANeWebservice.TratarXmlRetornado(const aXML: string): string;
begin
  // Reescrever na Unit Provider do Provedor se necessário;
  Result := GetSoapBody(aXML);
end;

procedure TACBrANeWebservice.UsarCertificado;
var
  TemCertificadoConfigurado: Boolean;
begin
  FPDFeOwner.SSL.UseCertificateHTTP := TACBrANe(FPDFeOwner).Provider.ConfigGeral.UseCertificateHTTP;

  if FPDFeOwner.SSL.UseCertificateHTTP then
  begin
    TemCertificadoConfigurado := (FPConfiguracoes.Certificados.NumeroSerie <> '') or
                                 (FPConfiguracoes.Certificados.DadosPFX <> '') or
                                 (FPConfiguracoes.Certificados.ArquivoPFX <> '');

    if TemCertificadoConfigurado then
      if FPConfiguracoes.Certificados.VerificarValidade then
        if (FPDFeOwner.SSL.CertDataVenc < Now) then
          raise EACBrDFeException.Create('Data de Validade do Certificado já expirou: '+
                                            FormatDateBr(FPDFeOwner.SSL.CertDataVenc));
  end;
end;

function TACBrANeWebservice.Executar(const SoapAction, Message, responseTag: string): string;
begin
  Result := Executar(SoapAction, Message, '', [], []);
end;

function TACBrANeWebservice.Executar(const SoapAction, Message, responseTag, namespace: string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], [namespace]);
end;

function TACBrANeWebservice.Executar(const SoapAction, Message, responseTag: string;
  namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], namespace);
end;

function TACBrANeWebservice.Executar(const SoapAction, Message: string;
  responseTag, namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', responseTag, namespace);
end;

procedure TACBrANeWebservice.EnviarDados(const SoapAction: string);
var
  Tentar, Tratado: Boolean;
  HTTPResultCode: Integer;
  InternalErrorCode: Integer;
begin
  Tentar := True;

  while Tentar do
  begin
    Tentar  := False;
    Tratado := False;
    FPRetorno := '';
    HTTPResultCode := 0;
    InternalErrorCode := 0;

    try
      // Envio por Evento... Aplicação cuidará do envio
      if Assigned(FPDFeOwner.OnTransmit) then
      begin
        FPDFeOwner.OnTransmit(FPEnvio, FPURL, SoapAction,
                              FPMimeType, FPRetorno,
                              HTTPResultCode, InternalErrorCode);

        if (InternalErrorCode <> 0) then
          raise EACBrDFeException.Create('Erro ao Transmitir');
      end
      else   // Envio interno, por TDFeSSL
      begin
        EnvioInterno(HTTPResultCode, InternalErrorCode);
      end;
    except
      if Assigned(FPDFeOwner.OnTransmitError) then
        FPDFeOwner.OnTransmitError(HTTPResultCode, InternalErrorCode,
                                   FPURL, FPEnvio, SoapAction,
                                   Tentar, Tratado);

      if not (Tentar or Tratado) then raise;
    end;
  end;
end;

procedure TACBrANeWebservice.ConfigurarHttpClient;
begin
  FHttpClient.URL := FPURL;
  FHttpClient.Method := FPMethod;
  FHttpClient.MimeType := FPMimeType;

  SetHeaders(FHttpClient.HeaderReq);

  if FPMethod = 'POST' then
    WriteStrToStream(FHttpClient.DataReq, AnsiString(FPEnvio));
end;

procedure TACBrANeWebservice.EnvioInterno(var CodigoErro, CodigoInterno: Integer);
begin
  ConfigurarHttpClient;

  try
    try
      FHttpClient.Execute;
    finally
      CodigoErro := FHttpClient.HTTPResultCode;
      CodigoInterno := FHttpClient.InternalErrorCode;
    end;

    FHttpClient.DataResp.Position := 0;

    FPRetorno := ReadStrFromStream(FHttpClient.DataResp, FHttpClient.DataResp.Size);

    if FPRetorno = '' then
      raise EACBrDFeException.Create('WebService retornou um XML vazio.');

    if StringIsXML(FPRetorno) then
      LevantarExcecaoHttp;
  except
    on E:Exception do
    begin
      raise EACBrDFeException.CreateDef(Format(ACBrStr(cACBrDFeSSLEnviarException),
        [FHttpClient.InternalErrorCode, FHttpClient.HTTPResultCode, FHttpClient.URL])
        + sLineBreak + FHttpClient.LastErrorDesc+ sLineBreak + E.Message);
    end;
  end;
end;

function TACBrANeWebservice.Executar(const SoapAction, Message, SoapHeader: string;
  responseTag, namespace: array of string): string;
begin
  FPEnvio := DefinirMsgEnvio(Message, SoapAction, SoapHeader, namespace);
  SalvarEnvio(FPEnvio, FPMsgOrig);

  UsarCertificado;

  EnviarDados(SoapAction);
  SalvarRetornoWebService(FPRetorno);

  FHtmlRetorno := RetornaHTMLNota(FPRetorno);

  Result := ExtrairRetorno(FPRetorno, responseTag);
  SalvarRetornoDadosMsg(Result);
end;

function TACBrANeWebservice.Enviar(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrANeWebservice.Consultar(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrANeWebservice.RetornaHTMLNota(const Retorno: string): string;
var pInicio, pFim: Integer;
begin
  Result := EmptyStr;

  pInicio := Pos('<codigo_html>', Retorno);
  pFim    := Pos('</codigo_html>', Retorno);

  if pInicio > 0 then
  begin
    Result := Copy(Retorno, pInicio, pFim -1) + '</codigo_html>';
    Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
    Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
    Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  end;
end;

{ TACBrANeWebserviceSoap11 }

constructor TACBrANeWebserviceSoap11.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrANeWebserviceSoap11.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  i: Integer;
  ns: string;
begin
  Result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"';

  for i := Low(namespace) to High(namespace) do
  begin
    ns := namespace[i];
    Result := Result + ' ' + ns;
  end;

  Result := Result + '>';

  if NaoEstaVazio(SoapHeader) then
    Result := Result + '<soapenv:Header>' + soapHeader + '</soapenv:Header>'
  else
    Result := Result + '<soapenv:Header/>';

  Result := Result + '<soapenv:Body>' + Message + '</soapenv:Body></soapenv:Envelope>';
  Result := string(NativeStringToUTF8(Result));

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
  FHttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

{ TACBrANeWebserviceSoap12 }

constructor TACBrANeWebserviceSoap12.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrANeWebserviceSoap12.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  i: Integer;
  ns: string;
begin
  Result := '<soapenv:Envelope xmlns:soapenv="http://www.w3.org/2003/05/soap-envelope"';

  for i := Low(namespace) to High(namespace) do
  begin
    ns := namespace[i];
    Result := Result + ' ' + ns;
  end;

  Result := Result + '>';

  if NaoEstaVazio(SoapHeader) then
    Result := Result + '<soapenv:Header>' + soapHeader + '</soapenv:Header>'
  else
    Result := Result + '<soapenv:Header/>';

  Result := Result + '<soapenv:Body>' + message + '</soapenv:Body></soapenv:Envelope>';
  Result := string(NativeStringToUTF8(Result));

  FPMimeType := FPMimeType + ';action="' + SoapAction + '"';

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
end;

{ TACBrANeWebserviceNoSoap }

constructor TACBrANeWebserviceNoSoap.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrANeWebserviceNoSoap.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
begin
  Result := Message;

  Result := string(NativeStringToUTF8(Result));

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
  FHttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

function TACBrANeWebserviceNoSoap.GetSoapBody(const Response: string): string;
begin
  Result := Response;
end;

{ TACBrANeWebserviceRest }

constructor TACBrANeWebserviceRest.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL, AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrANeWebserviceRest.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
begin
  Result := Message;

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
end;

{ TACBrANeWebserviceRest2 }

constructor TACBrANeWebserviceRest2.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrANeWebserviceRest2.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb, Texto: string;
begin
  UsuarioWeb := Trim(TConfiguracoesANe(FPConfiguracoes).Geral.Usuario);

//  if UsuarioWeb = '' then
//    GerarException(ACBrStr('O Seguradora ' + TConfiguracoesANe(FPConfiguracoes).Geral.xSeguradora +
//      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSUser seja informada.'));

  SenhaWeb := Trim(TConfiguracoesANe(FPConfiguracoes).Geral.Senha);

//  if SenhaWeb = '' then
//    GerarException(ACBrStr('O Seguradora ' + TConfiguracoesANe(FPConfiguracoes).Geral.xSeguradora +
//      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSSenha seja informada.'));

  Texto := StringReplace(Message, '"', '\"', [rfReplaceAll]);
  Texto := StringReplace(Texto, #10, '', [rfReplaceAll]);
  Texto := StringReplace(Texto, #13, '', [rfReplaceAll]);

  Result := Format('{"xml": "%s", "usuario": "%s", "senha": "%s"}', [Texto, UsuarioWeb, SenhaWeb]);

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
end;

{ TACBrANeWebserviceMulti1 }

constructor TACBrANeWebserviceMulti1.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPBound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr(FPBound, '"');
end;

function TACBrANeWebserviceMulti1.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb: string;
begin
  UsuarioWeb := Trim(TConfiguracoesANe(FPConfiguracoes).Geral.Usuario);

//  if UsuarioWeb = '' then
//    GerarException(ACBrStr('O Seguradora ' + TConfiguracoesANe(FPConfiguracoes).Geral.xSeguradora +
//      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSUser seja informada.'));

  SenhaWeb := Trim(TConfiguracoesANe(FPConfiguracoes).Geral.Senha);

//  if SenhaWeb = '' then
//    GerarException(ACBrStr('O Seguradora ' + TConfiguracoesANe(FPConfiguracoes).Geral.xSeguradora +
//      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSSenha seja informada.'));

  Result := '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr( 'login', '"') + sLineBreak + sLineBreak + UsuarioWeb + sLineBreak +
            '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr( 'senha', '"') + sLineBreak + sLineBreak + SenhaWeb + sLineBreak +
            '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr('f1', '"' ) + '; ' + 'filename=' +
            AnsiQuotedStr(GerarPrefixoArquivo + '-' + FPArqEnv + '.xml', '"') + sLineBreak +
            'Content-Type: text/xml' + sLineBreak + sLineBreak + Message + sLineBreak +
            '--' + FPBound + '--' + sLineBreak;

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
end;

{ TACBrANeWebserviceMulti2 }

constructor TACBrANeWebserviceMulti2.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPBound := '----=_Part_3_' + IntToHex(Random(MaxInt), 8);
  FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr(FPBound, '"');
end;

function TACBrANeWebserviceMulti2.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  NomeArq: string;
begin
  NomeArq := GerarPrefixoArquivo + '-' + FPArqEnv + '.xml';

  Result := '--' + FPBound + sLineBreak +
            'Content-Type: text/xml; charset=Cp1252; name=' +
            NomeArq + sLineBreak +
            'Content-Transfer-Encoding: binary' + sLineBreak +
            'Content-Disposition: form-data; name=' + AnsiQuotedStr(NomeArq, '"') +
            '; filename=' + AnsiQuotedStr(NomeArq, '"') + sLineBreak +
            sLineBreak +
            Message + sLineBreak +
            '--' + FPBound + '--' + sLineBreak;

  FHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FHttpClient.Clear;
end;

end.

