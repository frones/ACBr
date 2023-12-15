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
  https://api-staging.shipay.com.br/docs.html

*)

{$I ACBr.inc}

unit ACBrPIXPSPShipay;

interface

uses
  Classes, SysUtils, 
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrShipaySchemas, ACBrPIXBase, ACBrPIXSchemasProblema;

const
  cShipayURLStaging = 'https://api-staging.shipay.com.br';
  cShipayURLProducao = 'https://api.shipay.com.br';
  cShipayEndPointAuth = '/pdvauth';
  cShipayEndPointRefreshToken = '/refresh-token';
  cShipayEndPointWallets = '/v1/wallets';
  cShipayEndPointOrder = '/order';
  cShipayEndPointOrdersList = '/orders/list';
  cShipayEndPointOrderDueDate = '/v1/bacen/order-due-date';
  cShipayEndPointOrderV = '/orderv';
  cShipayPathRefund = 'refund';
  cShipayWalletPix = 'pix';
  cShipayWalletPagador = 'shipay-pagador';
  cShipayHeaderOrderType = 'x-shipay-order-type';
  cShipayEOrder = 'e-order';
  cItemTitleNotInformed = 'Item Vendido';
  cShipayEndPointPix = '/reconciliation/v2/pix-any-bank';
  

resourcestring
  sErrOrderIdDifferent = 'order_id diferente do informado';
  sErrOrderIdInvalid = 'order_id inválida: %s';
  sErrOrderRefNotInformed = '"order_ref" ou "txid" não informado';
  sErrNoWallet = 'Nenhuma Carteira associada a essa conta';

type

  TShipayQuandoEnviarOrder = procedure(ShipayOrder: TShipayOrder) of object;

  { TACBrPSPShipay }
  
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPShipay = class(TACBrPSP)
  private
    fAccessKey: String;
    fOrder: TShipayOrder;
    fOrderCreated: TShipayOrderCreated;
    fOrderDueDate: TShipayOrderDueDate;
    fOrderError: TShipayOrderError;
    fOrderInfo: TShipayOrderInfo;
    fOrderList: TShipayOrdersList;
    fQuandoEnviarOrder: TShipayQuandoEnviarOrder;
    fURLProducao: String;
    fURLSandBox: String;
    fWallets: TShipayWalletArray;
    function GetSecretKey: String;
    function GetURLProducao: String;
    function GetURLSandBox: String;
    procedure SetSecretKey(AValue: String);

    procedure DoPostOrder(aEndPoint: String; IsEOrder: Boolean);
    procedure DoGetOrderInfo(aEndPoint, order_id: String);

    procedure ProcessarAutenticacao(const AURL: String; ResultCode: Integer;
      const RespostaHttp: AnsiString);
    procedure QuandoAcessarEndPoint(const AEndPoint: String;
      var AURL: String; var AMethod: String);
    procedure QuandoReceberRespostaEndPoint(const AEndPoint, AURL, AMethod: String;
      var AResultCode: Integer; var RespostaHttp: AnsiString);

    function ConverterJSONCobSolicitadaParaShipayOrder(const CobSolicitadaJSON: String): String;
    function ConverterJSONOrderCreatedParaCobGerada(const OrderCreatedJSON: String): String;
    function ConverterJSONOrderInfoParaCobCompleta(const OrderInfoJSON: String): String;
    function ConverterJSONOrdersListParaCobsConsultadas(const OrdersList: String): String;

    function ShiPayStatusToCobStatus(AShipayStatus: TShipayOrderStatus): TACBrPIXStatusCobranca;
  protected
    function CalcularEndPointPath(const aMethod, aEndPoint: String): String; override;
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    
    procedure ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String); override;
    procedure ConfigurarPathParameters(const aMethod, aEndPoint: String); override;
    procedure ConfigurarQueryParameters(const aMethod, aEndPoint: String); override;
    procedure TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString;
      Problema: TACBrPIXProblema); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    procedure Autenticar; override;
    procedure RenovarToken; override;

    procedure PostOrder(IsEOrder: Boolean = False);
    procedure PostOrderV(IsEOrder: Boolean = False);
    procedure GetWallets;

    function DeleteOrder(const order_id: String): Boolean;
    function RefundOrder(const order_id: String; ValorReembolso: Currency): Boolean;

    procedure GetOrderInfo(const order_id: String);
    procedure GetOrderVInfo(const order_id: String);
    procedure GetOrdersList(start_date: TDateTime; end_date: TDateTime = 0;
      offset: Integer = 0; limit: Integer = 0);

    procedure PostOrderDueDate;
    function PatchOrderDueDate(const order_id: String): Boolean;   // Cancelar OrderDueDate
    function DeleteOrderDueDate(const order_id: String): Boolean;  // Estornar OrderDueDate

    property Wallets: TShipayWalletArray read fWallets;
    property Order: TShipayOrder read fOrder;
    property OrderDueDate: TShipayOrderDueDate read fOrderDueDate;
    property OrderCreated: TShipayOrderCreated read fOrderCreated;
    property OrderInfo: TShipayOrderInfo read fOrderInfo;
    property OrderList: TShipayOrdersList read fOrderList;
    property OrderError: TShipayOrderError read fOrderError;
  published
    property ClientID;
    property SecretKey: String read GetSecretKey write SetSecretKey;
    property AccessKey: String read fAccessKey write fAccessKey;
    property URLSandBox: String read GetURLSandBox write fURLSandBox;
    property URLProducao: String read GetURLProducao write fURLProducao;

    property QuandoEnviarOrder: TShipayQuandoEnviarOrder read fQuandoEnviarOrder
      write fQuandoEnviarOrder;
  end;

implementation

uses
  StrUtils, synautil, DateUtils, ACBrJSON,
  ACBrUtil.DateTime, ACBrUtil.Strings, ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrPIXUtil, ACBrPIXSchemasCob, ACBrPIXBRCode, ACBrPIXSchemasCobsConsultadas,
  ACBrPIXSchemasPix;

{ TACBrPSPShipay }

constructor TACBrPSPShipay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWallets := TShipayWalletArray.Create('wallets');
  fOrder := TShipayOrder.Create;
  fOrderDueDate := TShipayOrderDueDate.Create;
  fOrderCreated := TShipayOrderCreated.Create;
  fOrderInfo := TShipayOrderInfo.Create;
  fOrderList := TShipayOrdersList.Create;
  fOrderError := TShipayOrderError.Create;
  fAccessKey := EmptyStr;
  fURLSandBox := EmptyStr;
  fURLProducao := EmptyStr;
  fQuandoEnviarOrder := Nil;
  fpQuandoAcessarEndPoint := QuandoAcessarEndPoint;
  fpQuandoReceberRespostaEndPoint := QuandoReceberRespostaEndPoint;
end;

destructor TACBrPSPShipay.Destroy;
begin
  fWallets.Free;
  fOrder.Free;
  fOrderDueDate.Free;
  fOrderCreated.Free;
  fOrderInfo.Free;
  fOrderList.Free;
  fOrderError.Free;
  inherited Destroy;
end;

procedure TACBrPSPShipay.Clear;
begin
  inherited Clear;
  fOrder.Clear;
  fOrderDueDate.Clear;
  fOrderCreated.Clear;
  fOrderInfo.Clear;
  fOrderList.Clear;
  fOrderError.Clear;
  fWallets.Clear;
end;

function TACBrPSPShipay.GetSecretKey: String;
begin
   Result := ClientSecret;
end;

function TACBrPSPShipay.GetURLProducao: String;
begin
  Result := fURLProducao;
  if EstaVazio(fURLProducao) then
    Result := cShipayURLProducao;
end;

function TACBrPSPShipay.GetURLSandBox: String;
begin
  Result := fURLSandBox;
  if EstaVazio(fURLSandBox) then
    Result := cShipayURLStaging;
end;

procedure TACBrPSPShipay.SetSecretKey(AValue: String);
begin
  ClientSecret := AValue;
end;

procedure TACBrPSPShipay.DoPostOrder(aEndPoint: String; IsEOrder: Boolean);
var
  Body, aURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  Ok: Boolean;
begin
  Body := Trim(fOrder.AsJSON);
  if EstaVazio(Body) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido),
      [PathWithoutDelim(aEndPoint)]));

  fOrderCreated.Clear;
  PrepararHTTP;
  if IsEOrder then
    Http.Headers.Add(cShipayHeaderOrderType+' '+cShipayEOrder);
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  Ok := AcessarEndPoint(ChttpMethodPOST, aEndPoint, ResultCode, RespostaHttp);
  Ok := Ok and ((ResultCode = HTTP_OK) or (ResultCode = HTTP_CREATED));

  if Ok then
    fOrderCreated.AsJSON := String(RespostaHttp)
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    aURL := CalcularURLEndPoint(ChttpMethodPOST, aEndPoint);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, aURL]));
  end;
end;

procedure TACBrPSPShipay.DoGetOrderInfo(aEndPoint, order_id: String);
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  OK: Boolean;

begin
  if EstaVazio(Trim(order_id)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  AcessarEndPoint(ChttpMethodGET, aEndPoint, ResultCode, RespostaHttp);
  OK := (ResultCode = HTTP_OK);

  if OK then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
      DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));
  end
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodGET, aEndPoint);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodGET, AURL]));
  end;
end;

procedure TACBrPSPShipay.Autenticar;
var
  AURL, Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  js: TACBrJSONObject;
begin
  LimparHTTP;
  AURL := ObterURLAmbiente( ACBrPixCD.Ambiente ) + cShipayEndPointAuth;

   js := TACBrJSONObject.Create;
   try
     js.AddPair('access_key', AccessKey);
     js.AddPair('secret_key', SecretKey);
     js.AddPair('client_id', ClientID);
     Body := js.ToJSON;
   finally
     js.Free;
   end;

  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  ProcessarAutenticacao(AURL, ResultCode, RespostaHttp);
end;

procedure TACBrPSPShipay.RenovarToken;
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  LimparHTTP;
  AURL := ObterURLAmbiente( ACBrPixCD.Ambiente ) + cShipayEndPointRefreshToken;

  Http.Headers.Insert(0, ChttpHeaderAuthorization + ChttpAuthorizationBearer+' '+fpRefreshToken);
  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  ProcessarAutenticacao(AURL, ResultCode, RespostaHttp);
end;

procedure TACBrPSPShipay.PostOrder(IsEOrder: Boolean);
begin
  DoPostOrder(cShipayEndPointOrder, IsEOrder);
end;

procedure TACBrPSPShipay.PostOrderV(IsEOrder: Boolean);
begin
  if (fOrder.expiration <= 0) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroPropriedadeNaoDefinida),['expiration']));

  DoPostOrder(cShipayEndPointOrderV, IsEOrder);
end;

procedure TACBrPSPShipay.GetWallets;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  AURL: String;
  Ok: Boolean;
begin
  fWallets.Clear;
  PrepararHTTP;
  Ok := AcessarEndPoint(ChttpMethodGET, cShipayEndPointWallets, ResultCode, RespostaHttp);
  Ok := Ok and (ResultCode = HTTP_OK);

  if Ok then
  begin
    RespostaHttp := '{"wallets":'+RespostaHttp+'}';   // Transforma Array em Object
    fWallets.AsJSON := String(RespostaHttp);

    if (fWallets.Count < 1) then
      DispararExcecao(EACBrPixHttpException.Create(sErrNoWallet));
  end
  else
  begin
    AURL := CalcularURLEndPoint(ChttpMethodGET, cShipayEndPointWallets);
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodPOST, AURL]));
  end;
end;

// Retorna Verdadeiro se a Order foi cancelada com sucesso //
function TACBrPSPShipay.DeleteOrder(const order_id: String): Boolean;
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (Trim(order_id) = '') then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  AcessarEndPoint(ChttpMethodDELETE, cShipayEndPointOrder, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
      DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));

    Result := (fOrderInfo.status in [spsCancelled, spsRefunded]);
  end
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodDELETE, cShipayEndPointOrder);
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodDELETE, AURL]));
  end;
end;

function TACBrPSPShipay.RefundOrder(const order_id: String;
  ValorReembolso: Currency): Boolean;
var
  AURL, Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  js: TACBrJSONObject;
begin
  if (Trim(order_id) = '') then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  if (ValorReembolso <= 0) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['ValorReembolso']));

  js := TACBrJSONObject.Create;
  try
    js.AddPair('amount', ValorReembolso);
    Body := js.ToJSON;
  finally
    js.Free;
  end;

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  URLPathParams.Add(cShipayPathRefund);
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodDELETE, cShipayEndPointOrder, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
      DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));

    Result := (fOrderInfo.status in [spsCancelled, spsRefunded, spsRefundPending]);
  end
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodDELETE, cShipayEndPointOrder);
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodDELETE, AURL]));
  end;
end;

procedure TACBrPSPShipay.GetOrderInfo(const order_id: String);
begin
  DoGetOrderInfo(cShipayEndPointOrder, order_id);
end;

procedure TACBrPSPShipay.GetOrderVInfo(const order_id: String);
begin
  DoGetOrderInfo(cShipayEndPointOrderV, order_id);
end;

procedure TACBrPSPShipay.GetOrdersList(start_date: TDateTime;
  end_date: TDateTime; offset: Integer; limit: Integer);
var
  AURL, wStartDate, wEndDate: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  OK: Boolean;
begin
  Clear;
  PrepararHTTP;

  wStartDate := DateTimeToIso8601(start_date);
  SetLength(wStartDate, Length(wStartDate) - 5);
  URLQueryParams.Values['start_date'] := wStartDate;

  if (end_date <> 0) then
  begin
    wEndDate := DateTimeToIso8601(end_date);
    SetLength(wEndDate, Length(wEndDate) - 5);
    URLQueryParams.Values['end_date'] := wEndDate;
  end;

  if (limit <> 0) then
    URLQueryParams.Values['limit'] := IntToStr(limit);
  if (offset <> 0) then
    URLQueryParams.Values['offset'] := IntToStr(offset);

  AcessarEndPoint(ChttpMethodGET, cShipayEndPointOrdersList, ResultCode, RespostaHttp);
  OK := (ResultCode = HTTP_OK);

  if OK then
    fOrderList.AsJSON := String(RespostaHttp)
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodGET, cShipayEndPointOrdersList);
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodGET, AURL]));
  end;
end;

procedure TACBrPSPShipay.PostOrderDueDate;
var
  Body, AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  Ok: Boolean;
begin
  Body := Trim(OrderDueDate.AsJSON);
  if (Body = '') then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['OrderDueDate']));

  fOrderCreated.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  Ok := AcessarEndPoint(ChttpMethodPOST, cShipayEndPointOrderDueDate, ResultCode, RespostaHttp);
  Ok := Ok and (ResultCode = HTTP_CREATED);

  if Ok then
    fOrderCreated.AsJSON := String(RespostaHttp)
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodPOST, cShipayEndPointOrderDueDate);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, AURL]));
  end;
end;

function TACBrPSPShipay.PatchOrderDueDate(const order_id: String): Boolean;
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (Trim(order_id) = '') then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  AcessarEndPoint(ChttpMethodPATCH,cShipayEndPointOrderDueDate, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
      DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));

    Result := (fOrderInfo.status in [spsCancelled, spsRefunded]);
  end
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodPATCH, cShipayEndPointOrderDueDate);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPATCH, AURL]));
  end;
end;

function TACBrPSPShipay.DeleteOrderDueDate(const order_id: String): Boolean;
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (Trim(order_id) = '') then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  AcessarEndPoint(ChttpMethodDELETE, cShipayEndPointOrderDueDate, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
      DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));

    Result := (fOrderInfo.status in [spsCancelled, spsRefunded, spsRefundPending]);
  end
  else
  begin
    OrderError.AsJSON := String(RespostaHttp);
    AURL := CalcularURLEndPoint(ChttpMethodDELETE, cShipayEndPointOrderDueDate);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodDELETE, AURL]));
  end;
end;

procedure TACBrPSPShipay.ProcessarAutenticacao(const AURL: String;
  ResultCode: Integer; const RespostaHttp: AnsiString);
var
  js: TACBrJSONObject;
begin
  if (ResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      fpToken := js.AsString['access_token'];
      fpRefreshToken := js.AsString['refresh_token'];
    finally
      js.Free;
    end;

    if (Trim(fpToken) = '') then
      DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpValidadeToken := IncHour(Now, 4);
    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodPOST, AURL]));
end;

procedure TACBrPSPShipay.QuandoAcessarEndPoint(const AEndPoint: String;
  var AURL: String; var AMethod: String);
begin
  if (AEndPoint = cEndPointCob) then
  begin
    if (Pos(aMethod, ChttpMethodPOST+','+ChttpMethodPUT) > 0) then
      AMethod := ChttpMethodPOST;

    if (AMethod = ChttpMethodPATCH) then
      AMethod := ChttpMethodDELETE;
  end;
end;

procedure TACBrPSPShipay.QuandoReceberRespostaEndPoint(const AEndPoint, AURL,
  AMethod: String; var AResultCode: Integer; var RespostaHttp: AnsiString);
var
  NovaURL, uMethod: String;
begin
  uMethod := UpperCase(AMethod);
  if (AEndPoint = cEndPointCob) then
  begin
    if (uMethod = ChttpMethodPOST) then
    begin
      if (AResultCode = HTTP_OK) or (AResultCode = HTTP_CREATED) then
      begin
        RespostaHttp := ConverterJSONOrderCreatedParaCobGerada( RespostaHttp );
        AResultCode := HTTP_CREATED;
      end;
    end

    else if (uMethod = ChttpMethodGET) then
    begin
      if (pos(cShipayEndPointOrdersList, AURL) = 0) then
      begin
        if (AResultCode = HTTP_BAD_REQUEST) then   // não achou em /order, veja em /orderv
        begin
          epCob.Clear;
          PrepararHTTP;
          NovaURL := StringReplace(AURL, cShipayEndPointOrder, cShipayEndPointOrderV, [rfReplaceAll]);
          TransmitirHttp(AMethod, NovaURL, AResultCode, RespostaHttp);
        end;

        if (AResultCode = HTTP_OK) then
          RespostaHttp := ConverterJSONOrderInfoParaCobCompleta(RespostaHttp);
      end
      else if (AResultCode = HTTP_OK) then
        RespostaHttp := ConverterJSONOrdersListParaCobsConsultadas(RespostaHttp);
    end;
  end
end;

function TACBrPSPShipay.ConverterJSONCobSolicitadaParaShipayOrder(const CobSolicitadaJSON: String): String;
const
  cACBrOrderRefSufixo = '-acbr';
var
  Cob: TACBrPIXCobSolicitada;
  ia: TACBrPIXInfoAdicional;
  i: Integer;
  item: TShipayItem;
  s: String;
begin
  fOrder.Clear;
  Cob := TACBrPIXCobSolicitada.Create('');
  try
    Cob.AsJSON := CobSolicitadaJSON;

    fOrder.buyer.name := Cob.devedor.nome;
    fOrder.buyer.cpf_cnpj := IfEmptyThen(Cob.devedor.cnpj, Cob.devedor.cpf);
    fOrder.total := Cob.valor.original;
    fOrder.pix_dict_key := Cob.chave;
    fOrder.expiration := Cob.calendario.expiracao;

    ia := Cob.infoAdicionais.Find('order_ref');
    if (ia <> Nil) then
      fOrder.order_ref := ia.valor;

    ia := Cob.infoAdicionais.Find('wallet');
    if (ia <> Nil) then
      fOrder.wallet := ia.valor;

    i := 1;
    repeat
      s := 'item_'+IntToStr(i);
      ia := Cob.infoAdicionais.Find(s);
      if (ia <> Nil) then
      begin
        item := fOrder.items.New;
        try
          item.AsJSON := ia.valor;
        except
          On E: Exception do
          begin
            ACBrPixCD.RegistrarLog(Format('Erro na sintaxe de %s',[s]));
            ACBrPixCD.RegistrarLog(E.ClassName + ': ' + E.Message);
            fOrder.items.Remove(item);
          end;
        end;
        Inc(i);
      end;
    until (ia = Nil) ;

    // Chama Evento, para permitir ao usuário informar OrderRef, Wallet e Items
    if Assigned(fQuandoEnviarOrder) then
      fQuandoEnviarOrder(fOrder);

    if (Trim(fOrder.order_ref) = '') then
      DispararExcecao(EACBrPixException.Create(ACBrStr(sErrOrderRefNotInformed)));

    if (pos(cACBrOrderRefSufixo, LowerCase(fOrder.order_ref)) = 0) then
       fOrder.order_ref := fOrder.order_ref + cACBrOrderRefSufixo;

    if (Trim(fOrder.wallet) = '') then
    begin
      if (Wallets.Count <= 0) then
      begin
        GetWallets;
        LimparHTTP;
      end;

      // Não especificou Wallet, usando a única Wallet retornada ou "pix"
      if (Wallets.Count = 1) then
        fOrder.wallet := Wallets[0].wallet
      else
      begin
        for i := 0 to Wallets.Count-1 do
        begin
          if (Wallets[i].wallet = cShipayWalletPix) then  // Tem Pix ?
          begin
            fOrder.wallet := cShipayWalletPix;
            Break;
          end;
        end;
        if (Trim(fOrder.wallet) = '') then  // Não tem PIX, pegue a primeira da Lista
          fOrder.wallet := Wallets[0].wallet;
      end;
    end;

    if (fOrder.items.Count = 0) then
    begin
      // Não especificou Item em "QuandoEnviarOrder", criando um Item padrão
      with fOrder.items.New do
      begin
        sku := '00001';
        ean := '2'+ sku + '000000';  // 2 = in-store, Zeros = Total
        ean := ean + EAN13_DV(ean);
        item_title := cItemTitleNotInformed;
        quantity := 1;
        unit_price := fOrder.total;
      end;
    end;

    if (fOrder.wallet <> cShipayWalletPix) then
      fOrder.expiration := 0
    else
    begin
      if (fOrder.expiration <= 0) then
        fOrder.expiration := 3600
    end;

    Result := fOrder.AsJSON;
  finally
    Cob.Free;
  end;
end;

function TACBrPSPShipay.ConverterJSONOrderCreatedParaCobGerada(
  const OrderCreatedJSON: String): String;
var
  Cob: TACBrPIXCobGerada;
  qrd: TACBrPIXQRCodeDinamico;
begin
  fOrderCreated.AsJSON := OrderCreatedJSON;
  Cob := TACBrPIXCobGerada.Create('');
  try
    Cob.calendario.criacao := Now;
    if (fOrderCreated.expiration_date = 0) then
      fOrderCreated.expiration_date := IncMinute(Cob.calendario.criacao, 60);
    Cob.calendario.expiracao := SecondsBetween(Cob.calendario.criacao, fOrderCreated.expiration_date);
    Cob.txId := StringReplace(fOrderCreated.order_id, '-', '', [rfReplaceAll]);
    Cob.status := ShiPayStatusToCobStatus(fOrderCreated.status);
    Cob.pixCopiaECola := fOrderCreated.qr_code_text;
    Cob.chave := fOrderCreated.pix_dict_key;
    with Cob.infoAdicionais.New do
    begin
      nome := 'order_id';
      valor := fOrderCreated.order_id;
    end;
    with Cob.infoAdicionais.New do
    begin
      nome := 'wallet';
      valor := fOrderCreated.wallet;
    end;
    //with Cob.infoAdicionais.New do
    //begin
    //  nome := 'qr_code';
    //  valor := fOrderCreated.qr_code;   // infoAdicional.valor só aceita 200 chars;
    //end;

    if (fOrderCreated.wallet = cShipayWalletPix) then
    begin
      Cob.chave := fOrderCreated.pix_dict_key;
      with Cob.infoAdicionais.New do
      begin
        nome := 'pix_psp';
        valor := fOrderCreated.pix_psp;
      end;

      qrd := TACBrPIXQRCodeDinamico.Create;
      try
        qrd.AsString := fOrderCreated.qr_code_text;
        Cob.location := qrd.URL;
        Cob.loc.tipoCob := tcoCob;
        Cob.loc.location := qrd.URL;
      finally
        qrd.Free;
      end;
    end;

    // Copiando informações que não constam na resposta, do Objeto de Requisição //
    Cob.devedor.nome := fOrder.buyer.name;
    if (Length(fOrder.buyer.cpf_cnpj) > 11) then
      Cob.devedor.cnpj := fOrder.buyer.cpf_cnpj
    else
      Cob.devedor.cpf := fOrder.buyer.cpf_cnpj;
    Cob.valor.original := fOrder.total;
    with Cob.infoAdicionais.New do
    begin
      nome := 'order_ref';
      valor := fOrder.order_ref;
    end;

    Result := Cob.AsJSON;
  finally
    Cob.Free;
  end;
end;

function TACBrPSPShipay.ConverterJSONOrderInfoParaCobCompleta(
  const OrderInfoJSON: String): String;
var
  Cob: TACBrPIXCobCompleta;
  pix: TACBrPIX;
begin
  fOrderInfo.AsJSON := OrderInfoJSON;
  Cob := TACBrPIXCobCompleta.Create('');
  try
    Cob.calendario.criacao := fOrderInfo.created_at;
    if (fOrderInfo.expiration_date = 0) then
      fOrderInfo.expiration_date := IncMinute(fOrderInfo.created_at, 60);
    Cob.calendario.expiracao := SecondsBetween(Cob.calendario.criacao, fOrderInfo.expiration_date);
    Cob.valor.original := fOrderInfo.total_order;
    Cob.txId := StringReplace(fOrderInfo.order_id, '-', '', [rfReplaceAll]);
    Cob.status := ShiPayStatusToCobStatus(fOrderInfo.status);
    with Cob.infoAdicionais.New do
    begin
      nome := 'order_id';
      valor := fOrderInfo.order_id;
    end;
    with Cob.infoAdicionais.New do
    begin
      nome := 'wallet';
      valor := fOrderInfo.wallet;
    end;
    if (fOrderInfo.wallet_payment_id <> '') then
    begin
      with Cob.infoAdicionais.New do
      begin
        nome := 'wallet_payment_id';
        valor := fOrderInfo.wallet_payment_id;
      end;
    end;
    if (fOrderInfo.wallet = cShipayWalletPix) then
    begin
      with Cob.infoAdicionais.New do
      begin
        nome := 'pix_psp';
        valor := fOrderInfo.pix_psp;
      end;
    end;

    // Cria objeto pix com as informação de pagamento, caso existam
    if NaoEstaVazio(fOrderInfo.wallet_payment_id) then
    with cob.pix.New do
    begin
      endToEndId := fOrderInfo.wallet_payment_id;
      txid := fOrderInfo.order_id;
      componentesValor.original.valor := fOrderInfo.total_order;
      valor := fOrderInfo.paid_amount;
      horario := fOrderInfo.payment_date;
    end;

    Result := Cob.AsJSON;
  finally
    Cob.Free;
  end;
end;

function TACBrPSPShipay.ConverterJSONOrdersListParaCobsConsultadas(
  const OrdersList: String): String;
var
  wCobs: TACBrPIXCobsConsultadas;
  I: Integer;
begin
  fOrderList.AsJSON := OrdersList;
  wCobs := TACBrPIXCobsConsultadas.Create('');
  try
    for I := 0 to fOrderList.count - 1 do
    with wCobs.cobs.New do
    begin
      calendario.criacao := fOrderList.data[I].order_created_at;
      if (fOrderList.data[I].order_expiration_date = 0) then
        fOrderList.data[I].order_expiration_date := IncMinute(fOrderList.data[I].order_created_at, 60);
      calendario.expiracao := SecondsBetween(calendario.criacao, fOrderList.data[I].order_expiration_date);
      valor.original := fOrderList.data[I].total_order;
      txId := StringReplace(fOrderList.data[I].order_id, '-', '', [rfReplaceAll]);
      status := ShiPayStatusToCobStatus(fOrderList.data[I].status);
      with infoAdicionais.New do
      begin
        nome := 'order_id';
        valor := fOrderList.data[I].order_id;
      end;
      if NaoEstaVazio(fOrderList.data[I].wallet_payment_id) then
      begin
        with infoAdicionais.New do
        begin
          nome := 'wallet_payment_id';
          valor := fOrderList.data[I].wallet_payment_id;
        end;
      end;
    end;

    Result := wCobs.AsJSON;
  finally
    wCobs.Free;
  end;
end;

function TACBrPSPShipay.ShiPayStatusToCobStatus(
  AShipayStatus: TShipayOrderStatus): TACBrPIXStatusCobranca;
begin
  case AShipayStatus of
    spsPending, spsPendingV:
      Result := stcATIVA;
    spsApproved, spsRefunded, spsRefundPending:
      Result := stcCONCLUIDA;
    spsCancelled, spsExpired:
      Result := stcREMOVIDA_PELO_PSP
  else
    Result := stcNENHUM;
  end;
end;

function TACBrPSPShipay.CalcularEndPointPath(const aMethod, aEndPoint: String): String;
begin
  Result := Trim(aEndPoint);

  if (aEndPoint = cEndPointCob) then
  begin
    // Possui mais de um parâmetro de query?  ...Então é consulta por período
    if (URLQueryParams.Count > 1) then
      Result := cShipayEndPointOrdersList
    else if ((aMethod = ChttpMethodPATCH) or (aMethod = ChttpMethodDELETE)) or
            (fOrder.wallet = cShipayWalletPagador) then
      Result := cShipayEndPointOrder
    else
      Result := cShipayEndPointOrderV;
  end
  else if (aEndPoint = cEndPointPix) then
    Result := cShipayEndPointPix;
end;

function TACBrPSPShipay.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := URLProducao
  else
    Result := URLSandBox;
end;

procedure TACBrPSPShipay.ConfigurarBody(const aMethod, aEndPoint: String;
  var aBody: String);
begin
  if (aEndPoint = cEndPointCob) then
  begin
    if ((aMethod = ChttpMethodPUT) or (aMethod = ChttpMethodPOST)) then
      aBody := ConverterJSONCobSolicitadaParaShipayOrder(aBody);

    // Shipay não possui Body ao cancelar ordem
    if ((aMethod = ChttpMethodDELETE) or (aMethod = ChttpMethodPATCH)) and
       (Pos(PIXStatusCobrancaToString(stcREMOVIDA_PELO_USUARIO_RECEBEDOR), aBody) > 0) then
      aBody := EmptyStr;
  end;
end;

procedure TACBrPSPShipay.ConfigurarPathParameters(const aMethod,
  aEndPoint: String);
var
  wP, wName: String;
  wSL: TStringList;
  I: Integer;
begin
  if (URLPathParams.Count <= 0) then
    Exit;

  // Shipay não utiliza parâmetros de Path para métodos POST/PUT
  if (aMethod = ChttpMethodPOST) or (aMethod = ChttpMethodPUT) then
  begin
    URLPathParams.Clear;
    Exit;
  end;

  wSL := TStringList.Create;
  try
    for I := 0 to URLPathParams.Count - 1 do
    begin
      wName := URLPathParams.Names[I];

      // Ignora parâmetros não utilizados pela Shipay
      if (Pos(wName, 'revisao') > 0) then
        Continue;

      if EstaVazio(wName) then
        wP := URLPathParams[I]
      else
        wP := URLPathParams.Values[wName];

      // É order_id sem formatação?  ... Insere caracteres '-'
      if (aEndPoint = cEndPointCob) and (Length(wP) = 32) then
        wP := FormatarGUID(wP);
        
      if EstaVazio(wName) then
        wSL.Add(WP)
      else
        wSL.Values[wName] := wP;

      if (NivelLog > 1) and (wP <> URLPathParams[I]) then
        RegistrarLog('Parametro(Path) alterado: ' + URLPathParams[I] + ' => ' + wP);
    end;

    URLPathParams.Text := wSL.Text;
  finally
    wSL.Free;
  end;
end;

procedure TACBrPSPShipay.ConfigurarQueryParameters(const aMethod,
  aEndPoint: String);
const
  SDateFormat: string = 'yyyy''-''mm''-''dd''T''hh'':''nn'':''ss';
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
      if (Pos(wName, 'cpf,cnpj,locationPresente,status') > 0) then
        Continue;

      if (wName = 'inicio') then
        wSL.Values['inicio'] := FormatDateTime(SDateFormat, Iso8601ToDateTime(wValue))
      else if (wName = 'fim') then
        wSL.Values['fim'] := FormatDateTime(SDateFormat, Iso8601ToDateTime(wValue))
      else if (wName = 'paginacao.paginaatual') then
        wSL.Values['paginacao.paginaAtual'] := wValue
      else if (wName = 'paginacao.itensporpagina') then
        wSL.Values['paginacao.itensPorPagina'] := wValue
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

procedure TACBrPSPShipay.TratarRetornoComErro(ResultCode: Integer;
  const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
var
  js: TACBrJSONObject;
begin
  if (pos('"message"', RespostaHttp) > 0) then   // Erro no formato próprio da ShiPay
  begin
     (* Exemplo de Retorno
       {"code":404,"message":"Order not Found"}
     *)
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      Problema.status := js.AsInteger['code'];
      Problema.detail := js.AsString['message'];
    finally
      js.Free;
    end;
  end
  else
    inherited TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

end.


