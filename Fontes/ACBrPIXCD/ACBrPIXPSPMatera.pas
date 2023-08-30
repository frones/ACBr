{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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
  https://doc-api.matera.com/mp_server.html

*)

unit ACBrPIXPSPMatera;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD, ACBrOpenSSLUtils, ACBrSchemasMatera;

const
  cMateraURLSandbox       = 'https://mtls-mp.hml.flagship.maas.link';
  cMateraURLProducao      = '';
  cMateraPathAuthToken    = '/auth/realms/Matera/protocol/openid-connect/token';
  cMateraURLAuthTeste     = cMateraURLSandbox+cMateraPathAuthToken;
  cMateraURLAuthProducao  = cMateraURLProducao+cMateraPathAuthToken;
  cMateraEndPointAccounts = '/v1/accounts';
  cMateraEndPointAccountsv2 = '/v2/accounts';
  cMateraEndPointPayments = '/v1/payments';
  cMateraEndPointAliases  = '/aliases';
  cMateraEndPointWithdraw = '/withdraw';
  cMateraEndPointDeposits = '/deposits';
  cMateraEndPointWallet   = '/v1/wallet';
  cMateraEndPointCashin   = '/cashin';
  cMateraEndPointTransactions = '/transactions';

type

  { TACBrPSPMatera }

  TACBrPSPMatera = class(TACBrPSPCertificate)
  private
    fChavesPIXResposta: TMateraAliasArray;
    fChavePIXResposta: TMateraRegisterAliasResponse;
    fChavePIXSolicitacao: TMateraAliasRequest;
    fContaResposta: TMateraAccountResponse;
    fContaSolicitacao: TMateraCreateAccountTransactionRequest;
    fErroResposta: TMateraError;
    fQRCodeSolicitacao: TMateraQRCodeRequest;
    fQRCodeResposta: TMateraQRCodeResponse;
    fTransacoesResposta: TMateraTransactionResponseArray;
    fSecretKey: String;
    function GetChavePIXResposta: TMateraRegisterAliasResponse;
    function GetChavePIXSolicitacao: TMateraAliasRequest;
    function GetChavesPIXResposta: TMateraAliasArray;
    function GetContaSolicitacao: TMateraCreateAccountTransactionRequest;
    function GetContaResposta: TMateraAccountResponse;
    function GetErroResposta: TMateraError;
    function GetQRCodeSolicitacao: TMateraQRCodeRequest;
    function GetQRCodeResposta: TMateraQRCodeResponse;
    function GetTransacoesResposta: TMateraTransactionResponseArray;
    function RemoverResponseData(aJson: String): String;
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
    procedure ConfigurarHeaders(const Method, AURL: String); override;
  public
    destructor Destroy; override;

    procedure Clear; override;
    procedure Autenticar; override;

    function ContaIncluir: Boolean;
    function ContaInativar(aAccountId: String): Boolean;
    procedure ContaConsultar(aAccountId: String);

    function ChavePIXIncluir(aAccountId: String): Boolean;
    function ChavePIXExcluir(aAccountId, aChavePIX: String): Boolean;
    procedure ChavesPIXConsultar(aAccountId: String);

    property ContaResposta: TMateraAccountResponse read GetContaResposta;
    property ContaSolicitacao: TMateraCreateAccountTransactionRequest read GetContaSolicitacao;

    property ChavePIXSolicitacao: TMateraAliasRequest read GetChavePIXSolicitacao;
    property ChavePIXResposta: TMateraRegisterAliasResponse read GetChavePIXResposta;
    property ChavesPIXResposta: TMateraAliasArray read GetChavesPIXResposta;

    property ErroResposta: TMateraError read GetErroResposta;

    property QRCodeSolicitacao: TMateraQRCodeRequest read GetQRCodeSolicitacao;
    property QRCodeResposta: TMateraQRCodeResponse read GetQRCodeResposta;
    function GerarQRCode: Boolean;

    property TransacoesResposta: TMateraTransactionResponseArray read GetTransacoesResposta;
    procedure ConsultarTransacao(aAccountId, aTransactionID: String);

  published
    property ClientID;
    property ClientSecret;
    property SecretKey: String read fSecretKey write fSecretKey;
  end;

implementation

uses
  synautil, DateUtils, ACBrJSON, ACBrPIXBase,
  ACBrUtil.Strings, ACBrUtil.Base;


{ TACBrPSPMatera }

procedure TACBrPSPMatera.Autenticar;
var
  wURL, Body: String;
  wRespostaHttp: AnsiString;
  wResultCode, sec: Integer;
  js: TACBrJSONObject;
  qp: TACBrQueryParams;
begin
  VerificarPIXCDAtribuido;
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    wURL := cMateraURLAuthProducao
  else
    wURL := cMateraURLAuthTeste;


  qp := TACBrQueryParams.Create;
  try  
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['client_id'] := ClientID;
    qp.Values['client_secret'] := ClientSecret;
    Body := qp.AsURL;
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
    Http.Protocol := '1.1';
  finally
    qp.Free;
  end;

  TransmitirHttp(ChttpMethodPOST, wURL, wResultCode, wRespostaHttp);

  if (wResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(wRespostaHttp);
    try
      fpToken := js.AsString['access_token'];
      sec := js.AsInteger['expires_in'];
    finally
      js.Free;
    end;

    if (Trim(fpToken) = EmptyStr) then
      DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpValidadeToken := IncSecond(Now, sec);
    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
end;

function TACBrPSPMatera.ContaIncluir: Boolean;
var                           
  wOpenSSL: TACBrOpenSSLUtils;
  wBody, wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  if (not Assigned(fContaSolicitacao)) or fContaSolicitacao.IsEmpty then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['ContaSolicitada']));

  wBody := Trim(ContaSolicitacao.AsJSON);
  ContaResposta.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(
               ContaSolicitacao.externalIdentifier +
               ContaSolicitacao.client.taxIdentifier.taxId, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  Result := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointAccounts, wResultCode, wRespHttp);
  Result := Result and (wResultCode = HTTP_OK);

  if Result then
    ContaResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPMatera.ContaConsultar(aAccountId: String);
var
  wOK: Boolean;
  wURL, wHash: String;
  wResultCode: Integer; 
  wRespHttp: AnsiString;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(aAccountId);

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(aAccountId, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccounts, wResultCode, wRespHttp);
  wOK := (wResultCode = HTTP_OK);

  if wOK then
    ContaResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
  end;
end;

function TACBrPSPMatera.ChavePIXIncluir(aAccountId: String): Boolean;
var
  wOpenSSL: TACBrOpenSSLUtils;
  wBody, wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  if (not Assigned(fChavePIXSolicitacao)) or fChavePIXSolicitacao.IsEmpty then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['ChavePIXSolicitacao']));

  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));

  wBody := Trim(ChavePIXSolicitacao.AsJSON);
  ChavePIXResposta.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(LowerCase(ChttpMethodPOST) +
               ':' + cMateraEndPointAccounts +
               '/' + aAccountId + cMateraEndPointAliases +
               ':', SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  Result := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointAccounts + '/' +
              aAccountId + cMateraEndPointAliases, wResultCode, wRespHttp);
  Result := Result and (wResultCode = HTTP_ACCEPTED);

  if Result then
    ChavePIXResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

function TACBrPSPMatera.ChavePIXExcluir(aAccountId, aChavePIX: String): Boolean;
var
  wURL, wHash: String;
  wResultCode: Integer;
  wRespHttp: AnsiString;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  Result := False;
  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));
  if EstaVazio(aChavePIX) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aChavePIX']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(aChavePIX);

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(LowerCase(ChttpMethodDELETE) +
               ':' + cMateraEndPointAccounts +
               '/' + aAccountId + cMateraEndPointAliases +
               '/' + aChavePIX, SecretKey, algSHA256);
    Http.Headers.Add('Content-Type: ' + CContentTypeApplicationJSon);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  Result := AcessarEndPoint(ChttpMethodDELETE, cMateraEndPointAccounts + '/' +
              aAccountId + cMateraEndPointAliases, wResultCode, wRespHttp);
  Result := (wResultCode in [HTTP_OK, HTTP_ACCEPTED]);

  if (not Result) then
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodDELETE, cMateraEndPointAliases);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodDELETE, wURL]));
  end;
end;

procedure TACBrPSPMatera.ChavesPIXConsultar(aAccountId: String);
var
  wOK: Boolean;
  wURL: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));

  ChavesPIXResposta.Clear;
  PrepararHTTP;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccounts + '/' +
              aAccountId + cMateraEndPointAliases, wResultCode, wRespHttp);
  wOk := wOK and (wResultCode = HTTP_OK);

  if wOk then
    ChavesPIXResposta.AsJSON := RemoverResponseData(String(wRespHttp))
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
  end;
end;

function TACBrPSPMatera.GerarQRCode: Boolean;
var
  wOpenSSL: TACBrOpenSSLUtils;
  wBody, wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  if (not Assigned(fQRCodeSolicitacao)) or fQRCodeSolicitacao.IsEmpty then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['QRCodeSolicitacao']));

  wBody := Trim(QRCodeSolicitacao.AsJSON);

  QRCodeResposta.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(
                 QRCodeSolicitacao.paymentInfo.instantPayment.alias_.name +
                 CurrToStr(QRCodeSolicitacao.totalAmount)+
                 QRCodeSolicitacao.recipients[0].account.accountID+
                 CurrToStr(QRCodeSolicitacao.recipients[0].amount),
               SecretKey,
               algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  Result := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointPayments, wResultCode, wRespHttp);
  Result := Result and (wResultCode = HTTP_OK);

  if Result then
    QrCodeResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPMatera.ConsultarTransacao(aAccountId, aTransactionID: String);
var
  wOpenSSL: TACBrOpenSSLUtils;
  wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) or EstaVazio(aTransactionID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId/aTransactionID']));

  TransacoesResposta.Clear;
  PrepararHTTP;
  URLPathParams.Add(aTransactionID);

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := 'get:' + cMateraEndPointAccountsv2 + '/' + aAccountId + cMateraEndPointTransactions + '/' + aTransactionID;
    wHash := wOpenSSL.HMACFromString(wHash, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccountsv2 + '/' +
              aAccountId + cMateraEndPointTransactions, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    TransacoesResposta.AsJSON := RemoverResponseData(String(wRespHttp))
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

function TACBrPSPMatera.ContaInativar(aAccountId: String): Boolean;
var
  wURL, wHash: String;
  wResultCode: Integer;
  wRespHttp: AnsiString;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  Result := False;
  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(aAccountId);

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(aAccountId, SecretKey, algSHA256);
    Http.Headers.Add('Content-Type: ' + CContentTypeApplicationJSon);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  AcessarEndPoint(ChttpMethodDELETE, cMateraEndPointAccounts, wResultCode, wRespHttp);
  Result := (wResultCode = HTTP_OK);

  if (not Result) then
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodDELETE, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodDELETE, wURL]));
  end;
end;

function TACBrPSPMatera.GetContaSolicitacao: TMateraCreateAccountTransactionRequest;
begin
  if (not Assigned(fContaSolicitacao)) then
    fContaSolicitacao := TMateraCreateAccountTransactionRequest.Create;

  Result := fContaSolicitacao;
end;

function TACBrPSPMatera.GetChavePIXSolicitacao: TMateraAliasRequest;
begin
  if (not Assigned(fChavePIXSolicitacao)) then
    fChavePIXSolicitacao := TMateraAliasRequest.Create;
  Result := fChavePIXSolicitacao;
end;

function TACBrPSPMatera.GetChavesPIXResposta: TMateraAliasArray;
begin
  if (not Assigned(fChavesPIXResposta)) then
    fChavesPIXResposta := TMateraAliasArray.Create('aliases');
  Result := fChavesPIXResposta;
end;

function TACBrPSPMatera.GetChavePIXResposta: TMateraRegisterAliasResponse;
begin
  if (not Assigned(fChavePIXResposta)) then
    fChavePIXResposta := TMateraRegisterAliasResponse.Create('data');
  Result := fChavePIXResposta;
end;

function TACBrPSPMatera.GetContaResposta: TMateraAccountResponse;
begin
  if (not Assigned(fContaResposta)) then
    fContaResposta := TMateraAccountResponse.Create('data');

  Result := fContaResposta;
end;

function TACBrPSPMatera.GetErroResposta: TMateraError;
begin
  if (not Assigned(fErroResposta)) then
    fErroResposta := TMateraError.Create('error');

  Result := fErroResposta;
end;

function TACBrPSPMatera.GetQRCodeSolicitacao: TMateraQRCodeRequest;
begin
  if (not Assigned(fQRCodeSolicitacao)) then
    fQRCodeSolicitacao := TMateraQRCodeRequest.Create;
  Result := fQRCodeSolicitacao;

end;

function TACBrPSPMatera.GetQRCodeResposta: TMateraQRCodeResponse;
begin
  if (not Assigned(fQRCodeResposta)) then
    fQRCodeResposta := TMateraQRCodeResponse.Create('data');

  Result := fQRCodeResposta;

end;

function TACBrPSPMatera.GetTransacoesResposta: TMateraTransactionResponseArray;
begin
  if (not Assigned(fTransacoesResposta)) then
    fTransacoesResposta := TMateraTransactionResponseArray.Create('transactions');
  Result := fTransacoesResposta;
end;

function TACBrPSPMatera.RemoverResponseData(aJson: String): String;
var
  wJO: TACBrJSONObject;
begin
  wJO := TACBrJSONObject.Parse(aJson);
  try
    Result := wJO.AsJSONObject['data'].ToJSON;
  except
    Result := aJson;
  end;
end;

function TACBrPSPMatera.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cMateraURLProducao
  else
    Result := cMateraURLSandbox;
end;

procedure TACBrPSPMatera.ConfigurarHeaders(const Method, AURL: String);
//var
  //wHash: String;
  //wStrStream: TStringStream;
  //wOpenSSL: TACBrOpenSSLUtils;
begin
  inherited ConfigurarHeaders(Method, AURL);

  //Http.Headers.Add(cPixPDVHeaderPoweredBy);

  {if (Http.Document.Size <= 0) then
    Exit;

  wStrStream := TStringStream.Create('');
  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wStrStream.CopyFrom(Http.Document, 0);
    wHash := wOpenSSL.HMACFromString(wStrStream.DataString, ClientSecret, algSHA256);

    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
    wStrStream.Free;
  end;}
end;

destructor TACBrPSPMatera.Destroy;
begin
  if Assigned(fErroResposta) then
    fErroResposta.Free;

  if Assigned(fContaSolicitacao) then
    fContaSolicitacao.Free;

  if Assigned(fContaResposta) then
    fContaResposta.Free;

  If Assigned(fQRCodeSolicitacao) then
    fQRCodeSolicitacao.Free;

  If Assigned(fQRCodeResposta) then
    fQRCodeResposta.Free;

  inherited Destroy;
end;

procedure TACBrPSPMatera.Clear;
begin
  inherited Clear;

  if Assigned(fContaSolicitacao) then
    fContaSolicitacao.Clear;

  if Assigned(fContaResposta) then
    fContaResposta.Clear;

  if Assigned(fErroResposta) then
    fErroResposta.Clear;

  If Assigned(fQRCodeSolicitacao) then
    fQRCodeSolicitacao.Clear;

  If Assigned(fQRCodeResposta) then
    fQRCodeResposta.Clear;
end;

end.
