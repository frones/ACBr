{$I ACBr.inc}

unit ACBrOpenDelivery;

interface

uses
  ACBrBase,
  ACBrOpenDeliveryMarketPlace,
  ACBrOpenDeliverySchemaClasses,
  ACBrOpenDeliveryException,
  ACBrOpenDeliveryWebService,
  ACBrOpenDeliveryHTTP,
  ACBrOpenDeliveryEvents,
  Classes,
  SysUtils;

type
  TACBrOpenDeliveryProxy = class;
  TACBrOpenDeliveryHTTPLogEnvio = ACBrOpenDeliveryHTTP.TACBrOpenDeliveryHTTPLogEnvio;
  TACBrOpenDeliveryHTTPLogResposta = ACBrOpenDeliveryHTTP.TACBrOpenDeliveryHTTPLogResposta;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrOpenDelivery = class(TACBrComponent)
  private
    FMarketPlace: TACBrOpenDeliveryMarketPlace;
    FTimeOut: Integer;
    FProxy: TACBrOpenDeliveryProxy;
    FWebServices: TACBrOpenDeliveryWebServices;
    FOnGerarLog: TACBrGravarLog;
    FOnHTTPEnviar: TACBrOpenDeliveryOnHTTPEnviar;
    FOnHTTPRetornar: TACBrOpenDeliveryOnHTTPRetornar;
    FOnTokenGet: TOnTokenGet;
    FOnTokenSave: TOnTokenSave;
    FOnEventConfirmed: TOnEventStatus;
    FOnEventDispatched: TOnEventStatus;
    FOnEventReadyForPickup: TOnEventStatus;
    FOnEventPickupAreaAssigned: TOnEventStatus;
    FOnEventConcluded: TOnEventStatus;
    FOnEventCancellationRequested: TOnEventStatus;
    FOnEventCancellationRequestDenied: TOnEventStatus;
    FOnEventCancelled: TOnEventStatus;
    FOnEventOrderCancellationRequest: TOnEventStatus;
    FOnEventOrderPlaced: TOnEventOrder;
    FOnPollingEnd: TOnPollingEnd;
    function GetWebServices: TACBrOpenDeliveryWebServices;
    function GetOrder: TACBrOpenDeliverySchemaOrder;
    function GetEvents: TACBrOpenDeliverySchemaEventCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TACBrOpenDelivery); reintroduce;

    function GetToken: string;
    procedure FazerLog(const AMsg: String; out ATratado: Boolean);
    procedure GerarException(const AMsg: String; AErro: Exception = nil);

    property Events: TACBrOpenDeliverySchemaEventCollection read GetEvents;
    property Order: TACBrOpenDeliverySchemaOrder read GetOrder;
    property WebServices: TACBrOpenDeliveryWebServices read GetWebServices;
  published
    property MarketPlace: TACBrOpenDeliveryMarketPlace read FMarketPlace write FMarketPlace;
    property Proxy: TACBrOpenDeliveryProxy read FProxy write FProxy;
    property TimeOut: Integer read FTimeOut write FTimeOut;

    property OnEventOrderPlaced: TOnEventOrder read FOnEventOrderPlaced write FOnEventOrderPlaced;
    property OnEventConfirmed: TOnEventStatus read FOnEventConfirmed write FOnEventConfirmed;
    property OnEventDispatched: TOnEventStatus read FOnEventDispatched write FOnEventDispatched;
    property OnEventReadyForPickup: TOnEventStatus read FOnEventReadyForPickup write FOnEventReadyForPickup;
    property OnEventPickupAreaAssigned: TOnEventStatus read FOnEventPickupAreaAssigned write FOnEventPickupAreaAssigned;
    property OnEventConcluded: TOnEventStatus read FOnEventConcluded write FOnEventConcluded;
    property OnEventCancellationRequested: TOnEventStatus read FOnEventCancellationRequested write FOnEventCancellationRequested;
    property OnEventCancellationRequestDenied: TOnEventStatus read FOnEventCancellationRequestDenied write FOnEventCancellationRequestDenied;
    property OnEventCancelled: TOnEventStatus read FOnEventCancelled write FOnEventCancelled;
    property OnEventOrderCancellationRequest: TOnEventStatus read FOnEventOrderCancellationRequest write FOnEventOrderCancellationRequest;
    property OnPollingEnd: TOnPollingEnd read FOnPollingEnd write FOnPollingEnd;

    property OnGerarLog: TACBrGravarLog read FOnGerarLog write FOnGerarLog;
    property OnHTTPEnviar: TACBrOpenDeliveryOnHTTPEnviar read FOnHTTPEnviar write FOnHTTPEnviar;
    property OnHTTPRetornar: TACBrOpenDeliveryOnHTTPRetornar read FOnHTTPRetornar write FOnHTTPRetornar;
    property OnTokenGet: TOnTokenGet read FOnTokenGet write FOnTokenGet;
    property OnTokenSave: TOnTokenSave read FOnTokenSave write FOnTokenSave;
  end;

  TACBrOpenDeliveryProxy = class(TPersistent)
  private
    FPort: string;
    FPass: string;
    FHost: string;
    FUser: string;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(ASource: TACBrOpenDeliveryProxy); reintroduce;
  published
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property User: string read FUser write FUser;
    property Pass: string read FPass write FPass;
  end;

implementation

const
  CHttpTimeOutDef = 90000;
  CEndPointAuthentication = 'oauth/token';
  CEndPointMerchantUpdate = 'merchantUpdate';
  CEndPointMerchantStatus = 'merchantStatus';
  CEndPointEventPolling = 'events:polling';
  CEndPointEventAcknowledgment = 'events/acknowledgment';
  CEndPointOrderDetails = 'orders/{orderId}';
  CEndPointOrderConfirm = 'orders/{orderId}/confirm';
  CEndPointOrderReadyForPickup = 'orders/{orderId}/readyForPickup';
  CEndPointOrderDispatch = 'orders/{orderId}/dispatch';
  CEndPointOrderRequestCancellation = 'orders/{orderId}/requestCancellation';
  CEndPointOrderAcceptCancellation = 'orders/{orderId}/acceptCancellation';
  CEndPointOrderDenyCancellation = 'orders/{orderId}/denyCancellation';

{ TACBrOpenDeliveryProxy }

procedure TACBrOpenDeliveryProxy.Assign(ASource: TACBrOpenDeliveryProxy);
begin
  FHost := ASource.Host;
  FPass := ASource.Pass;
  FPort := ASource.Port;
  FUser := ASource.User;
end;

procedure TACBrOpenDeliveryProxy.Clear;
begin
  FHost := '';
  FPass := '';
  FPort := '';
  FUser := '';
end;

constructor TACBrOpenDeliveryProxy.Create;
begin
  inherited Create;
  Clear;
end;

{ TACBrOpenDelivery }

procedure TACBrOpenDelivery.Assign(ASource: TACBrOpenDelivery);
begin
  FTimeOut := ASource.TimeOut;
  FMarketPlace.Assign(ASource.MarketPlace);
  FProxy.Assign(ASource.Proxy);
end;

constructor TACBrOpenDelivery.Create(AOwner: TComponent);
begin
  inherited;
  FMarketPlace := TACBrOpenDeliveryMarketPlace.Create(Self);
  FProxy := TACBrOpenDeliveryProxy.Create;
  FTimeOut := CHttpTimeOutDef;
end;

destructor TACBrOpenDelivery.Destroy;
begin
  FMarketPlace.Free;
  FProxy.Free;
  FWebServices.Free;
  inherited;
end;

procedure TACBrOpenDelivery.FazerLog(const AMsg: String; out ATratado: Boolean);
begin
  ATratado := False;
  if (AMsg <> '') then
  begin
    if Assigned(OnGerarLog) then
      OnGerarLog(AMsg, ATratado);
  end;
end;

procedure TACBrOpenDelivery.GerarException(const AMsg: String; AErro: Exception);
var
  LTratado: Boolean;
  LMsgErro: string;
begin
  LMsgErro := AMsg;
  if Assigned(AErro) then
    LMsgErro := LMsgErro + sLineBreak + AErro.Message;

  LTratado := False;
  FazerLog('ERRO: ' + LMsgErro, LTratado);
  if not LTratado then
    raise EACBrOpenDeliveryException.CreateDef(LMsgErro);
end;

function TACBrOpenDelivery.GetEvents: TACBrOpenDeliverySchemaEventCollection;
begin
  Result := WebServices.Polling.Events;
end;

function TACBrOpenDelivery.GetOrder: TACBrOpenDeliverySchemaOrder;
begin
  Result := WebServices.OrderDetails.Order;
end;

function TACBrOpenDelivery.GetToken: string;
var
  LToken: string;
  LExpiresAt: TDateTime;
  LIsValidToken: Boolean;
begin
  LToken := '';
  LExpiresAt := 0;
  LIsValidToken := WebServices.Auth.AccessToken.IsValid;
  FMarketPlace.Credenciais.Validar;
  if not LIsValidToken then
  begin
    if Assigned(FOnTokenGet) then
    begin
      FOnTokenGet(FMarketPlace.Credenciais.ClientId, LToken, LExpiresAt);
      WebServices.Auth.AccessToken.AccessToken := LToken;
      WebServices.Auth.AccessToken.ExpiresAt := LExpiresAt;
      LIsValidToken := WebServices.Auth.AccessToken.IsValid;
    end;
  end;

  if not LIsValidToken then
  begin
    WebServices.Auth.Executar;
    LToken := WebServices.Auth.AccessToken.AccessToken;
    LExpiresAt := WebServices.Auth.AccessToken.ExpiresAt;
    if Assigned(FOnTokenSave) then
      FOnTokenSave(FMarketPlace.Credenciais.ClientId, LToken, LExpiresAt);
  end;

  Result := WebServices.Auth.AccessToken.AccessToken;
end;

function TACBrOpenDelivery.GetWebServices: TACBrOpenDeliveryWebServices;
begin
  if not Assigned(FWebServices) then
    FWebServices := TACBrOpenDeliveryWebServices.Create(Self);
  Result := FWebServices;
end;

end.
