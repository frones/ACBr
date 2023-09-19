{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         } 
{ - Alexandre de Paula                                                         }
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
  ACBrPIXCD, ACBrPIXBase, ACBrOpenSSLUtils, ACBrSchemasMatera;

const
  cMateraURLSandbox       = 'https://mtls-mp.hml.flagship.maas.link';
  cMateraURLProducao      = '';
  cMateraPathAuthToken    = '/auth/realms/Matera/protocol/openid-connect/token';
  cMateraURLAuthTeste     = cMateraURLSandbox+cMateraPathAuthToken;
  cMateraURLAuthProducao  = cMateraURLProducao+cMateraPathAuthToken;
  cMateraEndPointAccounts = '/v1/accounts';
  cMateraEndPointAccountsv2 = '/v2/accounts';
  cMateraEndPointPayments = '/v1/payments';
  cMateraEndPointInstantPayments = '/v1/instant-payments';
  cMateraEndPointAliases  = '/aliases';
  cMateraEndPointWithdraw = '/withdraw';
  cMateraEndPointDeposits = '/deposits';
  cMateraEndPointWallet   = '/v1/wallet';
  cMateraEndPointCashin   = '/cashin';
  cMateraEndPointTransactions = '/transactions';
  cMateraEndPointReturnCodes = '/return-codes';
  cMateraEndPointInstant_Payments = '/instant-payments';
  cMateraEndPointReturns = '/returns';
  cMateraEndPointCountry = 'BRA';
  cMateraMsgInstantPayment = 'InstantPayment';

type

  { TACBrPSPMatera }

  TACBrPSPMatera = class(TACBrPSPCertificate)
  private
    fAccountId: String;
    fChavesPIXResposta: TMateraAliasArray;
    fChavePIXResposta: TMateraRegisterAliasResponse;
    fChavePIXSolicitacao: TMateraAliasRequest;
    fContaResposta: TMateraAccountResponse;
    fContaSolicitacao: TMateraCreateAccountTransactionRequest;
    fErroResposta: TMateraError;
    fQRCodeSolicitacao: TMateraQRCodeRequest;
    fQRCodeResposta: TMateraQRCodeResponse;
    fRetiradaSolicitacao: TMateraRetiradaRequest;
    fRetiradaResposta: TMateraRetiradaResponse;
    fDevolucaoSolicitacao: TMateraDevolucaoRequest;
    fDevolucaoResposta: TMateraDevolucaoResponse;
    fMotivosDevolucoes: TMateraReturnCodesQueryResponse;
    fTransacoesResposta: TMateraTransactionResponseArray;
    fAliasResposta: TMateraAliasResponse;
    fSecretKey: String;
    function GetAliasResposta: TMateraAliasResponse;
    function GetChavePIXResposta: TMateraRegisterAliasResponse;
    function GetChavePIXSolicitacao: TMateraAliasRequest;
    function GetChavesPIXResposta: TMateraAliasArray;
    function GetContaSolicitacao: TMateraCreateAccountTransactionRequest;
    function GetContaResposta: TMateraAccountResponse;
    function GetDevolucaoResposta: TMateraDevolucaoResponse;
    function GetDevolucaoSolicitacao: TMateraDevolucaoRequest;
    function GetErroResposta: TMateraError;
    function GetQRCodeSolicitacao: TMateraQRCodeRequest;
    function GetQRCodeResposta: TMateraQRCodeResponse;
    function GetRetiradaResposta: TMateraRetiradaResponse;
    function GetRetiradaSolicitacao: TMateraRetiradaRequest;
    function GetReturnCodesQueryResponse: TMateraReturnCodesQueryResponse;
    function GetTransacoesResposta: TMateraTransactionResponseArray;
    function RemoverResponseData(aJson: String): String;

    function CobSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
    function QRCodeRespostaToCobGerada(const aJsonQRCodeResposta: String): String;

    function MateraStatusToCobStatus(aMateraStatus: TMateraTransactionStatus): TACBrPIXStatusCobranca;

    procedure AdicionarTransactionHashPOSTPayments;
    procedure AdicionarTransactionHashGETTransaction(aTransactionID: String);
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
    function CalcularEndPointPath(const aMethod, aEndPoint: String): String; override;

    procedure ConfigurarHeaders(const aMethod, aURL: String); override;
    procedure ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String); override;
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

    property AliasRetiradaResposta: TMateraAliasResponse read GetAliasResposta;
    procedure ConsultarAliasRetirada(aAccountId, aAlias: String);

    property DevolucaoMotivos: TMateraReturnCodesQueryResponse read GetReturnCodesQueryResponse;
    property DevolucaoSolicitacao: TMateraDevolucaoRequest read GetDevolucaoSolicitacao;
    property DevolucaoResposta: TMateraDevolucaoResponse read GetDevolucaoResposta;
    procedure DevolucaoSolicitar(aAccountId, aTransactionID: String);
    procedure DevolucaoConsultarMotivos;

    property RetiradaSolicitacao: TMateraRetiradaRequest read GetRetiradaSolicitacao;
    property RetiradaResposta: TMateraRetiradaResponse read GetRetiradaResposta;
    function Retirada(aAccountId: String): Boolean;
  published
    property ClientID;
    property ClientSecret;
    property SecretKey: String read fSecretKey write fSecretKey;
    property AccountId: String read fAccountId write fAccountId;
  end;

implementation

uses
  synautil, DateUtils, StrUtils, ACBrJSON, ACBrPIXSchemasCob, ACBrPIXUtil,
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
  wBody, wURL: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  if (not Assigned(fQRCodeSolicitacao)) or fQRCodeSolicitacao.IsEmpty then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['QRCodeSolicitacao']));

  if (QRCodeSolicitacao.recipients.Count <= 0) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['recipients']));

  if EstaVazio(QRCodeSolicitacao.recipients[0].account.accountID) and NaoEstaVazio(fAccountId) then
    QRCodeSolicitacao.recipients[0].account.accountID := fAccountId;

  wBody := Trim(QRCodeSolicitacao.AsJSON);

  QRCodeResposta.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

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
  wURL: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) or EstaVazio(aTransactionID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId/aTransactionID']));

  TransacoesResposta.Clear;
  PrepararHTTP;
  URLPathParams.Add(aTransactionID);

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

procedure TACBrPSPMatera.ConsultarAliasRetirada(aAccountId, aAlias: String);
var
  wURL: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) or EstaVazio(aAlias) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId/aAlias']));

  AliasRetiradaResposta.Clear;
  PrepararHTTP;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccounts + '/' +
              aAccountId +
              cMateraEndPointAliases + '/' +
              cMateraEndPointCountry + '/' +
              aAlias, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    AliasRetiradaResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPMatera.DevolucaoSolicitar(aAccountId, aTransactionID: String);
var
  wOpenSSL: TACBrOpenSSLUtils;
  wURL, wHash, wBody: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) or EstaVazio(aTransactionID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId/aTransactionID']));

  DevolucaoResposta.Clear;
  PrepararHTTP;

  wBody := Trim(DevolucaoSolicitacao.AsJSON);
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := IntToStr(Trunc(DevolucaoSolicitacao.amount)) +
      aAccountId +
      aTransactionID +
      DevolucaoSolicitacao.returnReasonCode;
    wHash := wOpenSSL.HMACFromString(wHash, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  wOk := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointAccounts + '/' +
           aAccountId + cMateraEndPointInstant_Payments + '/' + aTransactionID +
           cMateraEndPointReturns, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode in [HTTP_OK, HTTP_ACCEPTED]);

  if wOk then
    DevolucaoResposta.AsJSON := RemoverResponseData(String(wRespHttp))
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

function TACBrPSPMatera.Retirada(aAccountId: String): Boolean;
var
  wOpenSSL: TACBrOpenSSLUtils;
  wBody, wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
//  if (not Assigned(fChavePIXSolicitacao)) or fChavePIXSolicitacao.IsEmpty then
//    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['ChavePIXSolicitacao']));

  if EstaVazio(aAccountID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['accountId']));

  wBody := Trim(RetiradaSolicitacao.AsJSON);

  RetiradaResposta.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := IntToStr(Trunc(RetiradaSolicitacao.totalAmount)) +
      aAccountId +
      RetiradaSolicitacao.withdrawInfo.instantPayment.recipient.pspid +
      RetiradaSolicitacao.withdrawInfo.instantPayment.recipient.TaxIdentifierRequest.taxId;
    wHash := wOpenSSL.HMACFromString(wHash, SecretKey, algSHA256);

    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  Result := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointAccounts + '/' +
              aAccountId + cMateraEndPointWithdraw, wResultCode, wRespHttp);
  Result := Result and (wResultCode = HTTP_OK);

  if Result then
    RetiradaResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cMateraEndPointAccounts);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPMatera.DevolucaoConsultarMotivos;
var
  wOk: Boolean;
  wURL: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
  DevolucaoMotivos.Clear;
  PrepararHTTP;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointInstantPayments + '/' +
              cMateraEndPointCountry + cMateraEndPointReturnCodes, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    DevolucaoMotivos.AsJSON := String(wRespHttp)
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

function TACBrPSPMatera.GetAliasResposta: TMateraAliasResponse;
begin
  if (not Assigned(fAliasResposta)) then
    fAliasResposta := TMateraAliasResponse.Create('data');
  Result := fAliasResposta;
end;

function TACBrPSPMatera.GetContaResposta: TMateraAccountResponse;
begin
  if (not Assigned(fContaResposta)) then
    fContaResposta := TMateraAccountResponse.Create('data');
  Result := fContaResposta;
end;

function TACBrPSPMatera.GetDevolucaoResposta: TMateraDevolucaoResponse;
begin
  if (not Assigned(fDevolucaoResposta)) then
    fDevolucaoResposta := TMateraDevolucaoResponse.Create('data');
  Result := fDevolucaoResposta;
end;

function TACBrPSPMatera.GetDevolucaoSolicitacao: TMateraDevolucaoRequest;
begin
  if (not Assigned(fDevolucaoSolicitacao)) then
    fDevolucaoSolicitacao := TMateraDevolucaoRequest.Create('');
  Result := fDevolucaoSolicitacao;
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

function TACBrPSPMatera.GetRetiradaResposta: TMateraRetiradaResponse;
begin
  if (not Assigned(fRetiradaResposta)) then
    fRetiradaResposta := TMateraRetiradaResponse.Create('data');
  Result := fRetiradaResposta;
end;

function TACBrPSPMatera.GetRetiradaSolicitacao: TMateraRetiradaRequest;
begin
  if (not Assigned(fRetiradaSolicitacao)) then
    fRetiradaSolicitacao := TMateraRetiradaRequest.Create;
  Result := fRetiradaSolicitacao;
end;

function TACBrPSPMatera.GetReturnCodesQueryResponse: TMateraReturnCodesQueryResponse;
begin
  if (not Assigned(fMotivosDevolucoes)) then
    fMotivosDevolucoes := TMateraReturnCodesQueryResponse.Create('data');
  Result := fMotivosDevolucoes;
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

function TACBrPSPMatera.CobSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
var
  wCob: TACBrPIXCobSolicitada;
begin
  QRCodeSolicitacao.Clear;
  wCob := TACBrPIXCobSolicitada.Create('');
  try
    wCob.AsJSON := aJsonCobSolicitada;

    QRCodeSolicitacao.totalAmount := wCob.valor.original;
    QRCodeSolicitacao.currency := 'BRL';
    QRCodeSolicitacao.externalIdentifier := CriarTxId;

    QRCodeSolicitacao.paymentInfo.transactionType := cMateraMsgInstantPayment;
    //QRCodeSolicitacao.paymentInfo.instantPayment.alias_.type_ := malEVP;
    QRCodeSolicitacao.paymentInfo.instantPayment.alias_ := wCob.chave;
    QRCodeSolicitacao.paymentInfo.instantPayment.expiration := wCob.calendario.expiracao;

    with QRCodeSolicitacao.paymentInfo.instantPayment.qrCodeImageGenerationSpecification do
    begin
      errorCorrectionLevel := 'M';
      imageWidth := 400;
      generateImageRendering := False;
    end;

    QRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation.Clear;
    with QRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation.New do
    begin
      if NaoEstaVazio(wCob.devedor.nome) then
      begin
        name := wCob.devedor.nome;
        content := IfThen(EstaVazio(wCob.devedor.cpf), wCob.devedor.cnpj, wCob.devedor.cpf);
        showToPlayer := True;
      end
      else
      begin
        name := 'Type';
        content := cMateraMsgInstantPayment;
        showToPlayer := False;
      end;
    end;

    QRCodeSolicitacao.recipients.clear;
    with QRCodeSolicitacao.recipients.New do
    begin
      account.accountID := fAccountId;
      amount := wCob.valor.original;
      currency := 'BRL';
      recipientComment := wCob.solicitacaoPagador;
    end;

    Result := QRCodeSolicitacao.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMatera.QRCodeRespostaToCobGerada(const aJsonQRCodeResposta: String): String;
var
  wCob: TACBrPIXCobGerada;
begin
  fQRCodeResposta.AsJSON := aJsonQRCodeResposta;
  wCob := TACBrPIXCobGerada.Create('');
  try
    wCob.calendario.criacao := fQRCodeResposta.transactionDate;
    wCob.txId := fQRCodeResposta.transactionId;
    wCob.status := MateraStatusToCobStatus(fQRCodeResposta.financialStatement.status);
    wCob.pixCopiaECola := fQRCodeResposta.instantPayment.textContent;

    // Copiando informações que não constam na resposta, do Objeto de Requisição //
    wCob.chave := fQRCodeSolicitacao.paymentInfo.instantPayment.alias_;

    if (fQRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation. Count > 0) then
    with fQRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation[0] do
    begin
      wCob.devedor.nome := name;

      if (Length(content) > 11) then
        wCob.devedor.cnpj := content
      else
        wCob.devedor.cpf := content;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMatera.MateraStatusToCobStatus(aMateraStatus: TMateraTransactionStatus): TACBrPIXStatusCobranca;
begin
  case aMateraStatus of
    mtsCreated, mtsPartial, mtsCanceling: Result := stcATIVA;
    mtsApproved: Result := stcCONCLUIDA;
    mtsCanceled: Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
    mtsRejected, mtsExpired: Result := stcREMOVIDA_PELO_PSP
  else
    Result := stcNENHUM;
  end;
end;

procedure TACBrPSPMatera.AdicionarTransactionHashPOSTPayments;
var
  wHash: String;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(
               QRCodeSolicitacao.paymentInfo.instantPayment.alias_ +
               CurrToStr(QRCodeSolicitacao.totalAmount) +
               QRCodeSolicitacao.recipients[0].account.accountID +
               CurrToStr(QRCodeSolicitacao.recipients[0].amount), fSecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;
end;

procedure TACBrPSPMatera.AdicionarTransactionHashGETTransaction(
  aTransactionID: String);
var
  wHash: String;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := 'get:' + cMateraEndPointAccountsv2 + '/' + fAccountId +
      cMateraEndPointTransactions + '/' + aTransactionID;
    wHash := wOpenSSL.HMACFromString(wHash, fSecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;
end;

function TACBrPSPMatera.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cMateraURLProducao
  else
    Result := cMateraURLSandbox;
end;

function TACBrPSPMatera.CalcularEndPointPath(const aMethod, aEndPoint: String): String;
begin
  Result := Trim(aEndPoint);

  if (aEndPoint = cEndPointCob) then
  begin
    if (UpperCase(aMethod) = ChttpMethodGET) then
      Result := cMateraEndPointAccountsv2 + '/' + fAccountId + cMateraEndPointTransactions
    else
      Result := cMateraEndPointPayments;
  end;
end;

procedure TACBrPSPMatera.ConfigurarHeaders(const aMethod, aURL: String);
begin
  inherited ConfigurarHeaders(aMethod, aURL);

  if (UpperCase(aMethod) = ChttpMethodPOST) and (Pos(cMateraEndPointPayments, aURL) > 0) then
    AdicionarTransactionHashPOSTPayments;

  if (UpperCase(aMethod) = ChttpMethodGET) and (Pos(cMateraEndPointTransactions, aURL) > 0) and
     (URLPathParams.Count = 1) then
    AdicionarTransactionHashGETTransaction(URLPathParams[0]);
end;

procedure TACBrPSPMatera.ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String);
begin
  if (aEndPoint = cEndPointCob) and (aMethod = ChttpMethodPOST) then
    aBody := CobSolicitadaToQRCodeSolicitacao(aBody);
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
