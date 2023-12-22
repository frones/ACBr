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

{$I ACBr.inc}

unit ACBrPIXPSPMatera;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrPIXBase, ACBrOpenSSLUtils, ACBrSchemasMatera, ACBrPIXSchemasProblema;

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
  cMateraEndPointStatement = '/statement';
  cMateraEndPointBalance = '/balance';
  cMateraMsgInstantPayment = 'InstantPayment';
  cMateraMsgBillingDueDate = 'BillingDueDate';

type

  { TACBrPSPMatera }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPMatera = class(TACBrPSPCertificate)
  private
    fAccountId: String;
    fChavesPIXResposta: TMateraAliasArray;
    fChavePIXResposta: TMateraRegisterAliasResponse;
    fChavePIXSolicitacao: TMateraAliasRequest;
    fContaResposta: TMateraAccountResponse;
    fContaSolicitacao: TMateraCreateAccountTransactionRequest;
    fErroResposta: TMateraError;
    fMediatorFee: Currency;
    fQRCodeSolicitacao: TMateraQRCodeRequest;
    fQRCodeResposta: TMateraQRCodeResponse;
    fRetiradaSolicitacao: TMateraRetiradaRequest;
    fRetiradaResposta: TMateraRetiradaResponse;
    fDevolucaoSolicitacao: TMateraDevolucaoRequest;
    fDevolucaoResposta: TMateraDevolucaoResponse;
    fMotivosDevolucoes: TMateraReturnCodesQueryResponse;
    fTransacoesResposta: TMateraTransactionResponseArray;
    fAliasResposta: TMateraAliasResponse;
    fExtratoECResposta: TMaterastatementResponse;
    fSaldoECResposta: TMateraBalanceResponse;
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
    function GetExtratoECResposta: TMaterastatementResponse;
    function GetQRCodeSolicitacao: TMateraQRCodeRequest;
    function GetQRCodeResposta: TMateraQRCodeResponse;
    function GetRetiradaResposta: TMateraRetiradaResponse;
    function GetRetiradaSolicitacao: TMateraRetiradaRequest;
    function GetReturnCodesQueryResponse: TMateraReturnCodesQueryResponse;
    function GetSaldoECResposta: TMateraBalanceResponse;
    function GetTransacoesResposta: TMateraTransactionResponseArray;
    procedure SetMediatorFee(aValue: Currency);
    function TratarTransactionID(aTransactionID: String): String;
    function RemoverResponseData(aJson: String): String;

    function CobToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
    function CobSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
    function CobVSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
    function QRCodeRespostaToCobGerada(const aJsonQRCodeResposta: String): String;
    function TransactionResposeToCobCompleta(const aJsonTransactionResponse: String): String;
    function TransactionsResponseToCobsConsultadas(const aJsonTransactionsResponse: String): String;
    function DevolucaoSolicitadaToInstantPaymentReturns(const aJsonPixDevolucao: String): String;
    function InstantPaymentReturnsToDevolucaoSolicitada(const aJsonReturns: String): String;

    function MateraStatusToCobStatus(aMateraStatus: TMateraTransactionStatus): TACBrPIXStatusCobranca;
    function CobStatusToMateraStatus(aCobStatus: TACBrPIXStatusCobranca): TMateraTransactionStatus;
    
    procedure AdicionarTransactionHashPOSTReturns;
    procedure AdicionarTransactionHashPOSTPayments;
    procedure AdicionarTransactionHashGETTransaction(aTransactionID: String = '');

    procedure DoQuandoAcessarEndPoint(const aEndPoint: String; var aURL: String; var aMethod: String);
    procedure DoQuandoReceberRespostaEndPoint(const aEndPoint, aURL, aMethod: String; var aResultCode: Integer; var aRespostaHttp: AnsiString);
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
    function CalcularEndPointPath(const aMethod, aEndPoint: String): String; override;

    procedure ConfigurarHeaders(const aMethod, aURL: String); override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
    procedure ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String); override;
    procedure TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString; Problema: TACBrPIXProblema); override;
  public
    constructor Create(AOwner: TComponent); override;
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

    property QRCodeSolicitacao: TMateraQRCodeRequest read GetQRCodeSolicitacao;
    property QRCodeResposta: TMateraQRCodeResponse read GetQRCodeResposta;
    function GerarQRCode: Boolean;

    property TransacoesResposta: TMateraTransactionResponseArray read GetTransacoesResposta;
    procedure ConsultarTransacao(aAccountId, aTransactionID: String);
    procedure ConsultarTransacoes(aAccountId: String; aInicio: TDateTime = 0;
      aFim: TDateTime = 0; aStatus: TMateraTransactionStatus = mtsNone;
      aEndToEndId: String = ''; aHashNextPage: String = ''; aPageLimit: Integer = 0);

    property ExtratoECResposta: TMaterastatementResponse read GetExtratoECResposta;
    property SaldoECResposta: TMateraBalanceResponse read GetSaldoECResposta;
    procedure ConsultarExtratoEC(aAccountId: String; aStart: TDateTime = 0;
      aEnding: TDateTime = 0);
    procedure ConsultarSaldoEC(aAccountId: String);

    property AliasRetiradaResposta: TMateraAliasResponse read GetAliasResposta;
    procedure ConsultarAliasRetirada(aAccountId, aAlias: String);

    property DevolucaoMotivos: TMateraReturnCodesQueryResponse read GetReturnCodesQueryResponse;
    property DevolucaoSolicitacao: TMateraDevolucaoRequest read GetDevolucaoSolicitacao;
    property DevolucaoResposta: TMateraDevolucaoResponse read GetDevolucaoResposta;
    function DevolucaoSolicitar(aAccountId, aTransactionID: String): Boolean;
    procedure DevolucaoConsultarMotivos;

    property RetiradaSolicitacao: TMateraRetiradaRequest read GetRetiradaSolicitacao;
    property RetiradaResposta: TMateraRetiradaResponse read GetRetiradaResposta;
    function RetiradaSolicitar(aAccountId: String): Boolean;

    property ErroResposta: TMateraError read GetErroResposta;
  published
    property ClientID;
    property ClientSecret;
    property SecretKey: String read fSecretKey write fSecretKey;
    property AccountId: String read fAccountId write fAccountId;
    property MediatorFee: Currency read fMediatorFee write SetMediatorFee;
  end;

implementation

uses
  synautil, DateUtils, StrUtils, ACBrJSON, ACBrPIXUtil, ACBrPIXSchemasCobV,
  ACBrPIXSchemasCob, ACBrPIXSchemasCobsConsultadas, ACBrPIXSchemasDevolucao,
  ACBrUtil.Strings, ACBrUtil.Base, ACBrUtil.DateTime;


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
  if EstaVazio(aAccountId) then
    aAccountId := fAccountId;

  if EstaVazio(aAccountId) or EstaVazio(aTransactionID) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId/aTransactionID']));

  fAccountId := aAccountId;
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
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointTransactions);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPMatera.ConsultarTransacoes(aAccountId: String;
  aInicio: TDateTime; aFim: TDateTime; aStatus: TMateraTransactionStatus;
  aEndToEndId: String; aHashNextPage: String; aPageLimit: Integer);
var
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
  wURL: String;
begin
  if EstaVazio(aAccountId) then
    aAccountId := fAccountId;

  if EstaVazio(aAccountId) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId']));

  if (NivelLog > 1) then
  begin
    RegistrarLog('ConsultarTransacoes( ' +
      aAccountId +', '+
      FormatDateTimeBr(aInicio) +', '+
      FormatDateTimeBr(aFim)+', '+
      MateraTransactionStatusToString(aStatus) +', '+
      aEndToEndId +', '+
      aHashNextPage +', '+
      IntToStr(aPageLimit) +' )');
  end;

  Clear;
  PrepararHTTP;

  if (aInicio > 0) then
    URLQueryParams.Values['begin'] := FormatDateBr(aInicio, 'YYYY-MM-DD');

  if (aFim > 0) then
    URLQueryParams.Values['end'] := FormatDateBr(aFim, 'YYYY-MM-DD');

  if (aStatus <> mtsNone) then
    URLQueryParams.Values['status'] := MateraTransactionStatusToString(aStatus);

  if NaoEstaVazio(aEndToEndId) then
    URLQueryParams.Values['endToEndId'] := aEndToEndId;

  if NaoEstaVazio(aHashNextPage) then
    URLQueryParams.Values['hashNextPage'] := aHashNextPage;

  if NaoEstaZerado(aPageLimit) then
    URLQueryParams.Values['pageLimit'] := IntToStr(aPageLimit);

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccountsv2 + '/' +
              aAccountId + cMateraEndPointTransactions, wResultCode, wRespostaHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    TransacoesResposta.AsJSON := RemoverResponseData(String(wRespostaHttp))
  else
  begin
    ErroResposta.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointTransactions);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPMatera.ConsultarExtratoEC(aAccountId: String; aStart: TDateTime = 0; aEnding: TDateTime = 0);
var
  wOpenSSL: TACBrOpenSSLUtils;
  wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) then
    aAccountId := fAccountId;

  if EstaVazio(aAccountId) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId']));

  ExtratoECResposta.Clear;
  PrepararHTTP;

  if (aStart > 0) then
    URLQueryParams.Values['start'] := FormatDateBr(aStart, 'YYYY-MM-DD');

  if (aEnding > 0) then
    URLQueryParams.Values['ending'] := FormatDateBr(aEnding, 'YYYY-MM-DD');

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(aAccountId, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccounts + '/' +
              aAccountId + cMateraEndPointStatement, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    ExtratoECResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointTransactions);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPMatera.ConsultarSaldoEC(aAccountId: String);
var
  wOpenSSL: TACBrOpenSSLUtils;
  wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(aAccountId) then
    aAccountId := fAccountId;

  if EstaVazio(aAccountId) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['aAccountId']));

  SaldoECResposta.Clear;
  PrepararHTTP;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(aAccountId, SecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;

  wOk := AcessarEndPoint(ChttpMethodGET, cMateraEndPointAccountsv2 + '/' +
              aAccountId + cMateraEndPointBalance, wResultCode, wRespHttp);
  wOk := wOk and (wResultCode = HTTP_OK);

  if wOk then
    SaldoECResposta.AsJSON := String(wRespHttp)
  else
  begin
    ErroResposta.AsJSON := String(wRespHttp);
    wURL := CalcularURLEndPoint(ChttpMethodGET, cMateraEndPointTransactions);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodGET, wURL]));
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

function TACBrPSPMatera.DevolucaoSolicitar(aAccountId, aTransactionID: String): Boolean;
var
  wOpenSSL: TACBrOpenSSLUtils;
  wURL, wHash, wBody: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
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

  Result := AcessarEndPoint(ChttpMethodPOST, cMateraEndPointAccounts + '/' +
           aAccountId + cMateraEndPointInstant_Payments + '/' + aTransactionID +
           cMateraEndPointReturns, wResultCode, wRespHttp);
  Result := Result and (wResultCode in [HTTP_OK, HTTP_ACCEPTED]);

  if Result then
    DevolucaoResposta.AsJSON := String(wRespHttp)
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

function TACBrPSPMatera.RetiradaSolicitar(aAccountId: String): Boolean;
var
  wOpenSSL: TACBrOpenSSLUtils;
  wBody, wURL, wHash: String;
  wRespHttp: AnsiString;
  wResultCode: Integer;
begin
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

function TACBrPSPMatera.GetExtratoECResposta: TMaterastatementResponse;
begin
  if (not Assigned(fExtratoECResposta)) then
    fExtratoECResposta := TMaterastatementResponse.Create('data');
  Result := fExtratoECResposta;
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

function TACBrPSPMatera.GetSaldoECResposta: TMateraBalanceResponse;
begin
  if (not Assigned(fSaldoECResposta)) then
    fSaldoECResposta := TMateraBalanceResponse.Create('data');
  Result := fSaldoECResposta;

end;

function TACBrPSPMatera.GetTransacoesResposta: TMateraTransactionResponseArray;
begin
  if (not Assigned(fTransacoesResposta)) then
    fTransacoesResposta := TMateraTransactionResponseArray.Create('transactions');
  Result := fTransacoesResposta;
end;

procedure TACBrPSPMatera.SetMediatorFee(aValue: Currency);
begin
  if fMediatorFee = aValue then Exit;
  fMediatorFee := aValue;
end;

function TACBrPSPMatera.TratarTransactionID(aTransactionID: String): String;
begin
  Result := aTransactionID;

  if (Pos('-', aTransactionID) <= 0) then
    Result :=
      Copy(aTransactionID, 1, 8) + '-' +
      Copy(aTransactionID, 9, 4) + '-' +
      Copy(aTransactionID, 13, 4) + '-' +
      Copy(aTransactionID, 17, 4) + '-' +
      Copy(aTransactionID, 21, Length(aTransactionID));
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

function TACBrPSPMatera.CobToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
const
  cDataVencto = 'dataDeVencimento';
begin
  QRCodeSolicitacao.Clear;
  
  URLPathParams.Clear;
  if (Pos(cDataVencto, aJsonCobSolicitada) <= 0) then
    Result := CobSolicitadaToQRCodeSolicitacao(aJsonCobSolicitada)
  else
    Result := CobVSolicitadaToQRCodeSolicitacao(aJsonCobSolicitada);
end;

function TACBrPSPMatera.CobSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
var
  wCob: TACBrPIXCobSolicitada;
begin
  wCob := TACBrPIXCobSolicitada.Create('');
  try
    wCob.AsJSON := aJsonCobSolicitada;

    QRCodeSolicitacao.totalAmount := wCob.valor.original;
    QRCodeSolicitacao.currency := 'BRL';
    QRCodeSolicitacao.externalIdentifier := CriarTxId;

    QRCodeSolicitacao.paymentInfo.transactionType := cMateraMsgInstantPayment;
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
      mediatorfee := fMediatorFee;
    end;

    Result := QRCodeSolicitacao.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMatera.CobVSolicitadaToQRCodeSolicitacao(const aJsonCobSolicitada: String): String;
var
  wCobV: TACBrPIXCobVSolicitada;
  I: Integer;
begin
  wCobV := TACBrPIXCobVSolicitada.Create;
  try
    wCobV.AsJSON := aJsonCobSolicitada;

    QRCodeSolicitacao.totalAmount := wCobV.valor.original;
    QRCodeSolicitacao.currency := 'BRL';
    QRCodeSolicitacao.externalIdentifier := CriarTxId;
    QRCodeSolicitacao.paymentInfo.transactionType := cMateraMsgInstantPayment;
    QRCodeSolicitacao.paymentInfo.instantPayment.alias_ := wCobV.chave;
    QRCodeSolicitacao.paymentInfo.instantPayment.expiration := 0;
    QRCodeSolicitacao.paymentInfo.instantPayment.dynamicQRCodeType := mqtBillingDueDate;

    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.dueDate := wCobV.calendario.dataDeVencimento;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.daysAfterDueDate := wCobV.calendario.validadeAposVencimento;

    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.name := wCobV.devedor.nome;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.cpfCnpj := IfThen(EstaVazio(wCobV.devedor.cpf), wCobV.devedor.cnpj, wCobV.devedor.cpf);
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.email := wCobV.devedor.email;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.addressing.city := wCobV.devedor.cidade;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.addressing.uf := wCobV.devedor.uf;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.addressing.cep := wCobV.devedor.cep;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.payerInformation.addressing.street := wCobV.devedor.logradouro;

    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.fines.valuePerc := wCobV.valor.multa.valorPerc;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.fines.modality := Ord(wCobV.valor.multa.modalidade);
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.reduction.valuePerc := wCobV.valor.abatimento.valorPerc;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.reduction.modality := Ord(wCobV.valor.abatimento.modalidade);
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.interests.valuePerc := wCobV.valor.juros.valorPerc;
    QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.interests.modality := Ord(wCobV.valor.juros.modalidade);

    if NaoEstaZerado(wCobV.valor.desconto.valorPerc) then
    begin
      QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.discounts.uniqueDiscount.uniqueValuePercDiscount := wCobV.valor.desconto.valorPerc;
      QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.discounts.uniqueDiscount.modality := Ord(wCobV.valor.desconto.modalidade);
    end
    else if (wCobV.valor.desconto.descontosDataFixa.Count > 0) then
    begin
      QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.discounts.fixedDateDiscountList.modality := Ord(wCobV.valor.desconto.modalidade);
      for I := 0 to wCobV.valor.desconto.descontosDataFixa.Count - 1 do
        with QRCodeSolicitacao.paymentInfo.instantPayment.billingDueDate.discounts.fixedDateDiscountList.fixedDateDiscounts.New do
        begin
          date := wCobV.valor.desconto.descontosDataFixa[I].data;
          valuePerc := wCobV.valor.desconto.descontosDataFixa[I].valorPerc;
        end;
    end;

    with QRCodeSolicitacao.paymentInfo.instantPayment.qrCodeImageGenerationSpecification do
    begin
      errorCorrectionLevel := 'M';
      imageWidth := 400;
      generateImageRendering := False;
    end;

    QRCodeSolicitacao.recipients.Clear;
    with QRCodeSolicitacao.recipients.New do
    begin
      account.accountID := fAccountId;
      amount := wCobV.valor.original;
      currency := 'BRL';
      recipientComment := IfEmptyThen(wCobV.solicitacaoPagador, 'Solicitacao');
      mediatorfee := fMediatorFee;
    end;

    for I := 0 to wCobV.infoAdicionais.Count - 1 do
      with QRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation.New do
      begin
        name := wCobV.infoAdicionais[I].nome;
        content := wCobV.infoAdicionais[I].valor;
        showToPlayer := True;
      end;

    Result := QRCodeSolicitacao.AsJSON;
  finally
    wCobV.Free;
  end;
end;

function TACBrPSPMatera.QRCodeRespostaToCobGerada(const aJsonQRCodeResposta: String): String;
var
  wCob: TACBrPIXCobGerada;
begin
  QRCodeResposta.AsJSON := aJsonQRCodeResposta;
  wCob := TACBrPIXCobGerada.Create('');
  try
    wCob.calendario.criacao := QRCodeResposta.transactionDate;
    wCob.txId := RemoveString('-', QRCodeResposta.transactionId);
    wCob.status := MateraStatusToCobStatus(QRCodeResposta.financialStatement.status);
    wCob.pixCopiaECola := QRCodeResposta.instantPayment.textContent;
    wCob.valor.original := QRCodeResposta.totalAmount;

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

function TACBrPSPMatera.TransactionResposeToCobCompleta(const aJsonTransactionResponse: String): String;
var
  wCob: TACBrPIXCobCompleta;
  I: Integer;
  wTS: Int64;
begin
  TransacoesResposta.AsJSON := aJsonTransactionResponse;
  if (TransacoesResposta.Count <= 0) then
    Exit;

  wCob := TACBrPIXCobCompleta.Create('');
  try
    wCob.calendario.criacao := TransacoesResposta[0].transactionDate;
    wCob.calendario.expiracao := 3600;
    wCob.valor.original := TransacoesResposta[0].totalAmount;
    wCob.txId := StringReplace(TransacoesResposta[0].transactionId, '-', '', [rfReplaceAll]);
    wCob.status := MateraStatusToCobStatus(TransacoesResposta[0].transactionStatus);
    with wCob.infoAdicionais.New do
    begin
      nome := 'accountId';
      valor := TransacoesResposta[0].accountId;
    end;
    with wCob.infoAdicionais.New do
    begin
      nome := 'accountHolderId';
      valor := TransacoesResposta[0].accountHolderId;
    end;
    with wCob.infoAdicionais.New do
    begin
      nome := 'transactionType';
      valor := TransacoesResposta[0].transactionType;
    end;

    for I := 0 to TransacoesResposta[0].instantPayment.paymentReceived.Count - 1 do
    begin
      with wCob.pix.New do
      begin
        valor := TransacoesResposta[0].instantPayment.paymentReceived[I].receivedAmount;
        endToEndId := TransacoesResposta[0].instantPayment.paymentReceived[I].endToEndId;
        infoPagador := TransacoesResposta[0].instantPayment.paymentReceived[I].sender.name;

        wTS := StrToInt64Def(TransacoesResposta[0].instantPayment.paymentReceived[I].transactionTimestamp, 0);
        wTS := (wTS div 1000);
        horario := IncSecond(EncodeDate(1970,1,1), wTS);
      end;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMatera.TransactionsResponseToCobsConsultadas(const aJsonTransactionsResponse: String): String;
var
  wCobs: TACBrPIXCobsConsultadas;
  I, J: Integer;
  wTS: Int64;
begin
  TransacoesResposta.AsJSON := aJsonTransactionsResponse;
  if (TransacoesResposta.Count <= 0) then
    Exit;

  wCobs := TACBrPIXCobsConsultadas.Create('');
  try
    for I := 0 to TransacoesResposta.count - 1 do
    with wCobs.cobs.New do
    begin
      calendario.criacao := TransacoesResposta[I].transactionDate;
      calendario.expiracao := 3600;
      valor.original := TransacoesResposta[I].totalAmount;
      txId := StringReplace(TransacoesResposta[I].transactionId, '-', '', [rfReplaceAll]);
      status := MateraStatusToCobStatus(TransacoesResposta[I].transactionStatus);
      with infoAdicionais.New do
      begin
        nome := 'accountId';
        valor := TransacoesResposta[I].accountId;
      end;
      with infoAdicionais.New do
      begin
        nome := 'accountHolderId';
        valor := TransacoesResposta[I].accountHolderId;
      end;
      with infoAdicionais.New do
      begin
        nome := 'transactionType';
        valor := TransacoesResposta[I].transactionType;
      end;

      for J := 0 to TransacoesResposta[I].instantPayment.paymentReceived.Count - 1 do
      begin
        with pix.New do
        begin
          valor := TransacoesResposta[I].instantPayment.paymentReceived[J].receivedAmount;
          endToEndId := TransacoesResposta[I].instantPayment.paymentReceived[J].endToEndId;
          infoPagador := TransacoesResposta[I].instantPayment.paymentReceived[J].sender.name;

          wTS := StrToInt64Def(TransacoesResposta[I].instantPayment.paymentReceived[J].transactionTimestamp, 0);
          wTS := (wTS div 1000);
          horario := IncSecond(EncodeDate(1970,1,1), wTS);
        end;
      end;
    end;

    Result := wCobs.AsJSON;
  finally
    wCobs.Free;
  end;
end;

function TACBrPSPMatera.DevolucaoSolicitadaToInstantPaymentReturns(
  const aJsonPixDevolucao: String): String;
var
  wDev: TACBrPIXDevolucaoSolicitada;
begin
  wDev := TACBrPIXDevolucaoSolicitada.Create('');
  try
    wDev.AsJSON := aJsonPixDevolucao;

    DevolucaoSolicitacao.amount := wDev.valor;
    DevolucaoSolicitacao.returnReasonCode := 'MD06';
    DevolucaoSolicitacao.mediatorFee := fMediatorFee;
    DevolucaoSolicitacao.externalIdentifier := URLPathParams[2];

    Result := DevolucaoSolicitacao.AsJSON;
  finally
    wDev.Free;
  end;
end;

function TACBrPSPMatera.InstantPaymentReturnsToDevolucaoSolicitada(const aJsonReturns: String): String;
var
  wDev: TACBrPIXDevolucao;
begin
  DevolucaoResposta.AsJSON := aJsonReturns;
  wDev := TACBrPIXDevolucao.Create('');
  try
    wDev.id := RemoveString('-', DevolucaoResposta.transactionId);
    Result := wDev.AsJSON;
  finally
    wDev.Free;
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

function TACBrPSPMatera.CobStatusToMateraStatus(aCobStatus: TACBrPIXStatusCobranca): TMateraTransactionStatus;
begin
  case aCobStatus of
    stcATIVA: Result := mtsCreated;
    stcCONCLUIDA: Result := mtsApproved;
    stcREMOVIDA_PELO_PSP: Result := mtsExpired;
    stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := mtsCanceled;
  else
    Result := mtsNone;
  end;
end;

procedure TACBrPSPMatera.AdicionarTransactionHashPOSTReturns;
var
  wHash, wTransactionID: String;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  if (Pos('Transaction-Hash', Http.Headers.Text) > 0) then
    Exit;

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wTransactionID := TratarTransactionID(URLPathParams[0]);
    wHash := IntToStr(Trunc(DevolucaoSolicitacao.amount)) +
      fAccountId +
      wTransactionID +
      DevolucaoSolicitacao.returnReasonCode;

    wHash := wOpenSSL.HMACFromString(wHash, fSecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;
end;

procedure TACBrPSPMatera.AdicionarTransactionHashPOSTPayments;
var
  wHash: String;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  if EstaZerado(QRCodeSolicitacao.recipients.Count) then
      raise EACBrPixException.Create(Format(sErroObjetoNaoPrenchido, ['recipients']));

  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wHash := wOpenSSL.HMACFromString(
               QRCodeSolicitacao.paymentInfo.instantPayment.alias_ +
               CurrToStr(Trunc(QRCodeSolicitacao.totalAmount)) +
               QRCodeSolicitacao.recipients[0].account.accountID +
               CurrToStr(Trunc(QRCodeSolicitacao.recipients[0].amount)), fSecretKey, algSHA256);
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
      cMateraEndPointTransactions;
    if NaoEstaVazio(aTransactionID) then
      wHash := wHash + '/' + aTransactionID;
    wHash := wOpenSSL.HMACFromString(wHash, fSecretKey, algSHA256);
    Http.Headers.Add('Transaction-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
  end;
end;

procedure TACBrPSPMatera.DoQuandoAcessarEndPoint(const aEndPoint: String;
  var aURL: String; var aMethod: String);
begin
  if ((aEndPoint = cEndPointCob) or (aEndPoint = cEndPointCobV)) and (aMethod = ChttpMethodPUT) then
    aMethod := ChttpMethodPOST;

  if (aEndPoint = cEndPointPix) then
  begin
    if (aMethod = ChttpMethodPUT) then
      aMethod := ChttpMethodPOST
    else
      raise EACBrPixException.Create(sErroEndpointNaoImplementado);
  end;
end;

procedure TACBrPSPMatera.DoQuandoReceberRespostaEndPoint(const aEndPoint, aURL,
  aMethod: String; var aResultCode: Integer; var aRespostaHttp: AnsiString);
var
  wResp: String;
begin
  if (AEndPoint = cEndPointCob) or (AEndPoint = cEndPointCobV) then
  begin
    if (aMethod = ChttpMethodGET) then
    begin
      if (aResultCode in [HTTP_OK, HTTP_ACCEPTED, HTTP_CREATED]) then
        wResp := RemoverResponseData(aRespostaHttp);

      if (URLQueryParams.Count > 0) then
        aRespostaHttp := TransactionsResponseToCobsConsultadas(wResp)
      else
        aRespostaHttp := TransactionResposeToCobCompleta(wResp);
    end;

    if (aMethod = ChttpMethodPOST) and (aResultCode = HTTP_OK) then
      aResultCode := HTTP_CREATED;

    if (aMethod = ChttpMethodPOST) and (aResultCode = HTTP_CREATED) then
      aRespostaHttp := QRCodeRespostaToCobGerada(aRespostaHttp);
  end;

  if (AEndPoint = cEndPointPix) and (aMethod = ChttpMethodPOST) and
     (aResultCode = HTTP_ACCEPTED) then
  begin
    aResultCode := HTTP_CREATED;
    aRespostaHttp := InstantPaymentReturnsToDevolucaoSolicitada(aRespostaHttp);
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
var
  wTransactionID: String;
begin
  Result := Trim(aEndPoint);

  if ((aEndPoint = cEndPointCob) or (aEndPoint = cEndPointCobV)) then
  begin
    if (aMethod = ChttpMethodPATCH) then
      raise EACBrPixException.Create(sErroEndpointNaoImplementado);

    if (UpperCase(aMethod) = ChttpMethodGET) then
    begin
      if (URLPathParams.Count = 1) then
      begin
        wTransactionID := TratarTransactionID(URLPathParams[0]);
        URLPathParams.Clear;
        URLPathParams.Add(wTransactionID);
      end;

      Result := cMateraEndPointAccountsv2 + '/' + fAccountId + cMateraEndPointTransactions
    end
    else
      Result := cMateraEndPointPayments;
  end;

  if (aEndPoint = cEndPointPix) and (UpperCase(aMethod) = ChttpMethodPUT) then
  begin
    if (URLPathParams.Count > 1) then
    begin
      AdicionarTransactionHashPOSTReturns;
      wTransactionID := TratarTransactionID(URLPathParams[0]);
      Result := cMateraEndPointAccounts + '/' + fAccountId +
        cMateraEndPointInstant_Payments + '/' + wTransactionID +
        cMateraEndPointReturns;
      URLPathParams.Clear;
    end;
  end;
end;

procedure TACBrPSPMatera.ConfigurarHeaders(const aMethod, aURL: String);
begin
  inherited ConfigurarHeaders(aMethod, aURL);

  if (aMethod = ChttpMethodPOST) and (Pos(cMateraEndPointPayments, aURL) > 0) then
    AdicionarTransactionHashPOSTPayments;

  if (aMethod = ChttpMethodGET) and (Pos(cMateraEndPointTransactions, aURL) > 0) then
    if (URLPathParams.Count = 1) then
      AdicionarTransactionHashGETTransaction(URLPathParams[0])
    else
      AdicionarTransactionHashGETTransaction;
end;

procedure TACBrPSPMatera.ConfigurarQueryParameters(const Method, EndPoint: String);
const
  SDateFormat: string = 'yyyy''-''mm''-''dd';
var
  I: Integer;
  wSL: TStringList;
  wName, wValue: String;
begin
  if (URLQueryParams.Count <= 0) then
    Exit;

  wSL := TStringList.Create;
  try
    for I := 0 to URLQueryParams.Count - 1 do
    begin
      wName := LowerCase(URLQueryParams.Names[I]);
      wValue:= URLQueryParams.Values[wName];

      // Ignora parâmetros não utilizados pela Shipay
      if (Pos(wName, 'cpf,cnpj,locationPresente,paginacao.paginaatual') > 0) then
        Continue;

      if (wName = 'inicio') then
        wSL.Values['begin'] := FormatDateTime(SDateFormat, Iso8601ToDateTime(wValue))
      else if (wName = 'fim') then
        wSL.Values['end'] := FormatDateTime(SDateFormat, Iso8601ToDateTime(wValue))  
      else if (wName = 'status') then
        wSL.Values['status'] := MateraTransactionStatusToString(CobStatusToMateraStatus(StringToPIXStatusCobranca(wValue)))
      else if (wName = 'paginacao.itensporpagina') then
        wSL.Values['pageLimit'] := wValue
      else
        Continue;

      RegistrarLog('Parametro(Query) alterado: ' + URLQueryParams[I] + ' => ' + wSL[wSL.Count-1]);
    end;

    if (wSL.Count > 0) then
      URLQueryParams.Text := wSL.Text;
  finally
    wSL.Free;
  end;
end;

procedure TACBrPSPMatera.ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String);
begin
  if ((aEndPoint = cEndPointCob) or (aEndPoint = cEndPointCobV)) and
     ((aMethod = ChttpMethodPOST) or (aMethod = ChttpMethodPUT)) then
    aBody := CobToQRCodeSolicitacao(aBody);

  if (aEndPoint = cEndPointPix) and (aMethod = ChttpMethodPUT) then
    aBody := DevolucaoSolicitadaToInstantPaymentReturns(aBody);
end;

procedure TACBrPSPMatera.TratarRetornoComErro(ResultCode: Integer;
  const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
var
  js: TACBrJSONObject;
begin
  // Verifica se erro está no formato próprio da Matera
  if (pos('"error"', RespostaHttp) > 0) then
  begin
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      Problema.title := 'Error: ' + js.AsJSONObject['error'].AsString['code'];
      Problema.detail := js.AsJSONObject['error'].AsString['description'];
    finally
      js.Free;
    end;
  end
  else
    inherited TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

constructor TACBrPSPMatera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpQuandoAcessarEndPoint := DoQuandoAcessarEndPoint;
  fpQuandoReceberRespostaEndPoint := DoQuandoReceberRespostaEndPoint;
  fSecretKey := EmptyStr;
  fAccountId := EmptyStr;
  fMediatorFee := 0;
  Clear;
end;

destructor TACBrPSPMatera.Destroy;
begin
  if Assigned(fErroResposta) then
    fErroResposta.Free;

  if Assigned(fContaSolicitacao) then
    fContaSolicitacao.Free;

  if Assigned(fContaResposta) then
    fContaResposta.Free;

  if Assigned(fChavePIXSolicitacao) then
    fChavePIXSolicitacao.Free;

  if Assigned(fChavePIXResposta) then
    fChavePIXResposta.Free;

  if Assigned(fChavesPIXResposta) then
    fChavesPIXResposta.Free;

  if Assigned(fQRCodeSolicitacao) then
    fQRCodeSolicitacao.Free;

  if Assigned(fQRCodeResposta) then
    fQRCodeResposta.Free;

  if Assigned(fTransacoesResposta) then
    fTransacoesResposta.Free;

  if Assigned(fAliasResposta) then
    fAliasResposta.Free;

  if Assigned(fMotivosDevolucoes) then
    fMotivosDevolucoes.Free;

  if Assigned(fDevolucaoSolicitacao) then
    fDevolucaoSolicitacao.Free;

  if Assigned(fDevolucaoResposta) then
    fDevolucaoResposta.Free;

  if Assigned(fRetiradaSolicitacao) then
    fRetiradaSolicitacao.Free;

  if Assigned(fRetiradaResposta) then
    fRetiradaResposta.Free;

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

  if Assigned(fChavePIXSolicitacao) then
    fChavePIXSolicitacao.Clear;

  if Assigned(fChavePIXResposta) then
    fChavePIXResposta.Clear;

  if Assigned(fChavesPIXResposta) then
    fChavesPIXResposta.Clear;

  if Assigned(fQRCodeSolicitacao) then
    fQRCodeSolicitacao.Clear;

  if Assigned(fQRCodeResposta) then
    fQRCodeResposta.Clear;

  if Assigned(fTransacoesResposta) then
    fTransacoesResposta.Clear;

  if Assigned(fAliasResposta) then
    fAliasResposta.Clear;

  if Assigned(fMotivosDevolucoes) then
    fMotivosDevolucoes.Clear;

  if Assigned(fDevolucaoSolicitacao) then
    fDevolucaoSolicitacao.Clear;

  if Assigned(fDevolucaoResposta) then
    fDevolucaoResposta.Clear;

  if Assigned(fRetiradaSolicitacao) then
    fRetiradaSolicitacao.Clear;

  if Assigned(fRetiradaResposta) then
    fRetiradaResposta.Clear;
end;

end.
