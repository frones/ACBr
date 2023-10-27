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
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrOpenSSLUtils;

const
  cItauURLSandbox = 'https://devportal.itau.com.br/sandboxapi';
  cItauURLProducao = 'https://secure.api.itau';
  cItauPathAPIPix = '/pix_recebimentos/v2';
  cItauPathAPIPixSandbox = '/pix_recebimentos_ext_v2/v2';
  cItauURLAuthTeste = 'https://devportal.itau.com.br/api/jwt';
  cItauURLAuthProducao = 'https://sts.itau.com.br';
  cItauPathAuthToken = '/as/token.oauth2';
  cItauPathCertificado = '/seguranca/v1/certificado';
  cItauPathCertificadoSolicitacao = '/solicitacao';
  cItauPathCertificadoRenovacao = '/renovacao';

type

  { TACBrPSPItau }
  
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPItau = class(TACBrPSPCertificate)
  private
    fSandboxStatusCode: String;
    fxCorrelationID: String;
    fSSLUtils: TACBrOpenSSLUtils;
    fQuandoNecessitarCredenciais: TACBrQuandoNecessitarCredencial;

    function ObterChavePrivada: String;
  protected
    function VerificarSeIncluiCertificado(const Method, AURL: String): Boolean; override;
    function VerificarSeIncluiChavePrivada(const Method, AURL: String): Boolean; override;
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
    procedure ConfigurarHeaders(const Method, AURL: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Autenticar; override;

    function SolicitarCertificado(const TokenTemporario: String): String;
    function RenovarCertificado: String;
    function GerarCertificadoCSR: String;
  published
    property APIVersion;
    property ClientID;
    property ClientSecret;

    property QuandoNecessitarCredenciais: TACBrQuandoNecessitarCredencial
      read fQuandoNecessitarCredenciais write fQuandoNecessitarCredenciais;

    property xCorrelationID: String read fxCorrelationID write fxCorrelationID;
    property SandboxStatusCode: String read fSandboxStatusCode write fSandboxStatusCode;
  end;

implementation

uses
  synautil, DateUtils,
  ACBrUtil.Strings, ACBrUtil.Base, ACBrJSON;

{ TACBrPSPItau }

constructor TACBrPSPItau.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSandboxStatusCode := '';
  fxCorrelationID := '';
  fQuandoNecessitarCredenciais := Nil;
  fSSLUtils := TACBrOpenSSLUtils.Create(Self);  // Self irá destruir ele...
end;

procedure TACBrPSPItau.Autenticar;
var
  AURL, Body: String;
  RespostaHttp: AnsiString;
  ResultCode, sec: Integer;
  js: TACBrJSONObject;
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
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      fpToken := js.AsString['access_token'];
      sec := js.AsInteger['expires_in'];
      fpRefreshToken := js.AsString['refresh_token'];
    finally
      js.Free;
    end;

    if (Trim(fpToken) = '') then
      DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpValidadeToken := IncSecond(Now, sec);
    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, AURL]));
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
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
      [Http.ResultCode, ChttpMethodPOST, AURL]));
end;

function TACBrPSPItau.RenovarCertificado: String;
var
  Body, AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  Result := EmptyStr;
  VerificarPIXCDAtribuido;

  if (ACBrPixCD.Ambiente = ambProducao) then
    AURL := cItauURLAuthProducao + cItauPathCertificado + cItauPathCertificadoRenovacao
  else
  begin
    VerificarAutenticacao;
    AURL := cItauURLSandbox + cItauPathCertificado + cItauPathCertificadoRenovacao;
  end;

  Body := GerarCertificadoCSR;

  LimparHTTP;
  PrepararHTTP;
  ConfigurarAutenticacao(ChttpMethodPOST, cItauPathCertificadoRenovacao);

  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeTextPlain;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
    Result := StreamToAnsiString(Http.OutputStream)
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, AURL]));
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
  if NaoEstaVazio(ArquivoChavePrivada) and FileExists(ArquivoChavePrivada) then
    fSSLUtils.LoadPrivateKeyFromFile(ArquivoChavePrivada)
  else if NaoEstaVazio(ChavePrivada) then
    fSSLUtils.LoadPrivateKeyFromString(ChavePrivada)
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

function TACBrPSPItau.VerificarSeIncluiCertificado(const Method, AURL: String): Boolean;
begin
  Result := inherited VerificarSeIncluiCertificado(Method, AURL) and
    (ACBrPixCD.Ambiente = ambProducao) and (Pos(cItauPathCertificadoSolicitacao, AURL) <= 0);
end;

function TACBrPSPItau.VerificarSeIncluiChavePrivada(const Method, AURL: String): Boolean;
begin
  Result := inherited VerificarSeIncluiChavePrivada(Method, AURL) and
    (ACBrPixCD.Ambiente = ambProducao) and (Pos(cItauPathCertificadoSolicitacao, AURL) <= 0);
end;

function TACBrPSPItau.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cItauURLProducao + cItauPathAPIPix
  else
    Result := cItauURLSandbox + cItauPathAPIPixSandbox;
end;

procedure TACBrPSPItau.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  inherited ConfigurarQueryParameters(Method, EndPoint);

  with URLQueryParams do
  begin
    if NaoEstaVazio(fSandboxStatusCode) then
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
  if EstaVazio(s) then
  begin
    if (CreateGUID(guid) = 0) then
      s := GUIDToString(guid);
  end;

  if NaoEstaVazio(s) then
    Http.Headers.Add('x-correlationID: ' + s);

  if (ACBrPixCD.Ambiente = ambTeste) and (fpToken <> '') then
    Http.Headers.Add('x-sandbox-token: ' + fpToken);
end;

end.


