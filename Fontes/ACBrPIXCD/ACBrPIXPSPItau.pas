{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

(*

  Documentação
  https://developer.itau.com.br/

*)

{$I ACBr.inc}

unit ACBrPIXPSPItau;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD, ACBrOpenSSLUtils;

const
  cItauURLSandbox = 'https://api.itau.com.br/sandbox';
  cItauURLProducao = 'https://secure.api.itau';
  cItauPathAPIPix = '/pix_recebimentos/v2';
  cItauURLAuthTeste = cItauURLSandbox+'/api/oauth/token';
  cItauURLAuthProducao = 'https://sts.itau.com.br';
  cItauPathAuthToken = '/as/token.oauth2';
  cItauPathCertificado = '/seguranca/v1/certificado';
  cItauPathCertificadoSolicitacao = '/solicitacao';
  cItauPathCertificadoRenovacao = '/renovacao';

type

  { TACBrPSPItau }

  TACBrPSPItau = class(TACBrPSP)
  private
    fSandboxStatusCode: String;
    fxCorrelationID: String;
    fSSLUtils: TACBrOpenSSLUtils;
    fArquivoCertificado: String;
    fArquivoChavePrivada: String;
    fQuandoNecessitarCredenciais: TACBrQuandoNecessitarCredencial;

    function ObterChavePrivada: String;
    procedure SetArquivoCertificado(AValue: String);
    procedure SetArquivoChavePrivada(AValue: String);
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
    procedure ConfigurarHeaders(const Method, AURL: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Autenticar; override;

    function SolicitarCertificado(const TokenTemporario: String): String;
    function GerarCertificadoCSR: String;
  published
    property APIVersion;
    property ClientID;
    property ClientSecret;

    property ArquivoChavePrivada: String read fArquivoChavePrivada write SetArquivoChavePrivada;
    property ArquivoCertificado: String read fArquivoCertificado write SetArquivoCertificado;

    property QuandoNecessitarCredenciais: TACBrQuandoNecessitarCredencial
      read fQuandoNecessitarCredenciais write fQuandoNecessitarCredenciais;

    property xCorrelationID: String read fxCorrelationID write fxCorrelationID;
    property SandboxStatusCode: String read fSandboxStatusCode write fSandboxStatusCode;
  end;

implementation

uses
  synautil,
  ACBrUtil,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  DateUtils;

{ TACBrPSPItau }

constructor TACBrPSPItau.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSandboxStatusCode := '';
  fxCorrelationID := '';
  fArquivoCertificado := '';
  fArquivoChavePrivada := '';
  fQuandoNecessitarCredenciais := Nil;
  fSSLUtils := TACBrOpenSSLUtils.Create(Self);  // Self irá destruir ele...
end;

procedure TACBrPSPItau.Autenticar;
var
  AURL, Body: String;
  RespostaHttp: AnsiString;
  ResultCode, sec: Integer;
  js: TJsonObject;
  qp: TACBrQueryParams;
begin
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    AURL := cItauURLAuthProducao + cItauPathAuthToken
  else
    AURL := cItauURLAuthTeste;

  qp := TACBrQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['client_id'] := ClientID;
    qp.Values['client_secret'] := ClientSecret;
    Body := qp.AsURL;
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
  begin
   {$IfDef USE_JSONDATAOBJECTS_UNIT}
    js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
    try
      fpToken := js.S['access_token'];
      sec := js.I['expires_in'];
      fpRefereshToken := js.S['refresh_token'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      fpToken := js['access_token'].AsString;
      sec := js['expires_in'].AsInteger;
      fpRefereshToken := js['refresh_token'].AsString;
    finally
      js.Free;
    end;
   {$EndIf}

   if (Trim(fpToken) = '') then
     ACBrPixCD.DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

   fpValidadeToken := IncSecond(Now, sec);
   fpAutenticado := True;
  end
  else
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
end;

function TACBrPSPItau.SolicitarCertificado(const TokenTemporario: String): String;
var
  Body, AURL, Token: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  VerificarPIXCDAtribuido;

  if (ACBrPixCD.Ambiente = ambProducao) then
    AURL := cItauURLAuthProducao + cItauPathCertificado + cItauPathCertificadoSolicitacao
  else
  begin
    VerificarAutenticacao;
    AURL := cItauURLSandbox + cItauPathCertificado + cItauPathCertificadoSolicitacao;
  end;

  Body := GerarCertificadoCSR;

  LimparHTTP;
  Token := IfEmptyThen(TokenTemporario, fpToken);
  if (Token <> '') then
    Http.Headers.Insert(0, ChttpHeaderAuthorization + ChttpAuthorizationBearer+' '+Token);

  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeTextPlain;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);

  Result := '';
  if (ResultCode = HTTP_OK) then
    Result := StreamToAnsiString(Http.OutputStream)
  else
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
end;

function TACBrPSPItau.GerarCertificadoCSR: String;
begin
  VerificarPIXCDAtribuido;
  ObterChavePrivada;

  if (Trim(ClientID) = '') then
    raise EACBrPSPException.CreateFmt( ACBrStr(sErroPropriedadeNaoDefinida),
                                       ['ClientID']);
  if (Trim(ACBrPixCD.Recebedor.Nome) = '') then
    raise EACBrPSPException.CreateFmt( ACBrStr(sErroPropriedadeNaoDefinida),
                                       ['ACBrPixCD.Recebedor.Nome']);
  if (Trim(ACBrPixCD.Recebedor.Cidade) = '') then
    raise EACBrPSPException.CreateFmt( ACBrStr(sErroPropriedadeNaoDefinida),
                                       ['ACBrPixCD.Recebedor.Cidade']);
  if (Trim(ACBrPixCD.Recebedor.UF) = '') then
    raise EACBrPSPException.CreateFmt( ACBrStr(sErroPropriedadeNaoDefinida),
                                       ['ACBrPixCD.Recebedor.UF']);
  if (Trim(ACBrPixCD.DadosAutomacao.NomeAplicacao) = '') then
    raise EACBrPSPException.CreateFmt( ACBrStr(sErroPropriedadeNaoDefinida),
                                       ['ACBrPixCD.DadosAutomacao.NomeAplicacao']);

  Result := fSSLUtils.CreateCertificateSignRequest( ClientID,
                                                    ACBrPixCD.Recebedor.Nome,
                                                    ACBrPixCD.DadosAutomacao.NomeAplicacao,
                                                    ACBrPixCD.Recebedor.Cidade,
                                                    ACBrPixCD.Recebedor.UF,
                                                    'BR');
end;

function TACBrPSPItau.ObterChavePrivada: String;
var
  Resposta: AnsiString;
begin
  if (fArquivoChavePrivada <> '') and FileExists(fArquivoChavePrivada) then
    fSSLUtils.LoadPrivateKeyFromFile(fArquivoChavePrivada)
  else
  begin
    Resposta := '';
    if Assigned(fQuandoNecessitarCredenciais) then
      fQuandoNecessitarCredenciais(crePrivKey, Resposta);

    if (Trim(Resposta) <> '') then
      fSSLUtils.LoadCertificateFromString(Resposta);
  end;

  Result := fSSLUtils.PrivateKeyAsString;
end;

procedure TACBrPSPItau.SetArquivoCertificado(AValue: String);
begin
  if fArquivoCertificado = AValue then Exit;
  fArquivoCertificado := Trim(AValue);
end;

procedure TACBrPSPItau.SetArquivoChavePrivada(AValue: String);
begin
  if fArquivoChavePrivada = AValue then Exit;
  fArquivoChavePrivada := (AValue);
end;

function TACBrPSPItau.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cItauURLProducao
  else
    Result := cItauURLSandbox;

  Result := Result + cItauPathAPIPix;
end;

procedure TACBrPSPItau.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  inherited ConfigurarQueryParameters(Method, EndPoint);

  with URLQueryParams do
  begin
    if (fSandboxStatusCode <> '') then
      Values['status_code'] := fSandboxStatusCode;
  end;
end;

procedure TACBrPSPItau.ConfigurarHeaders(const Method, AURL: String);
var
  guid: TGUID;
  s: String;
begin
  inherited ConfigurarHeaders(Method, AURL);

  s := Trim(fxCorrelationID);
  if (s = '') then
  begin
    if (CreateGUID(guid) = 0) then
      s := GUIDToString(guid);
  end;

  if (s <> '') then
    Http.Headers.Add('x-correlationID: ' + s);
end;

end.


