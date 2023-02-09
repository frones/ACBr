{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Gabriel Baltazar                               }
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

unit ACBrOpenDeliveryWebService;

interface

uses
  ACBrBase,
  ACBrJSON,
  ACBrOpenDeliverySchemaClasses,
  ACBrOpenDeliveryEvents,
  ACBrOpenDeliveryException,
  ACBrOpenDeliveryHTTP,
  ACBrUtil.Strings,
  DateUtils,
  pcnConversaoOD,
  SysUtils;

type
  TACBrOpenDeliveryAuth = class;
  TACBrOpenDeliveryPolling = class;
  TACBrOpenDeliveryAcknowledgment = class;
  TACBrOpenDeliveryMerchantUpdate = class;
  TACBrOpenDeliveryOrderDetails = class;
  TACBrOpenDeliveryOrderConfirm = class;
  TACBrOpenDeliveryOrderReadyForPickup = class;
  TACBrOpenDeliveryOrderDispatch = class;
  TACBrOpenDeliveryOrderDelivered = class;
  TACBrOpenDeliveryOrderRequestCancellation = class;
  TACBrOpenDeliveryOrderAcceptCancellation = class;
  TACBrOpenDeliveryOrderDenyCancellation = class;

  { TACBrOpenDeliveryWebServices }

  TACBrOpenDeliveryWebServices = class
  private
    FOwner: TACBrComponent;
    FAuth: TACBrOpenDeliveryAuth;
    FAcknowledgment: TACBrOpenDeliveryAcknowledgment;
    FPolling: TACBrOpenDeliveryPolling;
    FMerchantUpdate: TACBrOpenDeliveryMerchantUpdate;
    FOrderDetails: TACBrOpenDeliveryOrderDetails;
    FOrderConfirm: TACBrOpenDeliveryOrderConfirm;
    FOrderReadyForPickup: TACBrOpenDeliveryOrderReadyForPickup;
    FOrderDispatch: TACBrOpenDeliveryOrderDispatch;
    FOrderDelivered: TACBrOpenDeliveryOrderDelivered;
    FOrderRequestCancellation: TACBrOpenDeliveryOrderRequestCancellation;
    FOrderAcceptCancellation: TACBrOpenDeliveryOrderAcceptCancellation;
    FOrderDenyCancellation: TACBrOpenDeliveryOrderDenyCancellation;
    function GetAuth: TACBrOpenDeliveryAuth;
    function GetPolling: TACBrOpenDeliveryPolling;
    function GetAcknowledgment: TACBrOpenDeliveryAcknowledgment;
    function GetOrderDetails: TACBrOpenDeliveryOrderDetails;
    function GetOrderConfirm: TACBrOpenDeliveryOrderConfirm;
    function GetOrderDispatch: TACBrOpenDeliveryOrderDispatch;
    function GetOrderDelivered: tacbropendeliveryorderDelivered;
    function GetOrderReadyForPickup: TACBrOpenDeliveryOrderReadyForPickup;
    function GetOrderAcceptCancellation: TACBrOpenDeliveryOrderAcceptCancellation;
    function GetOrderDenyCancellation: TACBrOpenDeliveryOrderDenyCancellation;
    function GetOrderRequestCancellation: TACBrOpenDeliveryOrderRequestCancellation;
    function GetMerchantUpdate: TACBrOpenDeliveryMerchantUpdate;
  public
    constructor Create(AOwner: TACBrComponent);
    destructor Destroy; override;

    property Auth: TACBrOpenDeliveryAuth read GetAuth;
    property Acknowledgment: TACBrOpenDeliveryAcknowledgment read GetAcknowledgment;
    property MerchantUpdate: TACBrOpenDeliveryMerchantUpdate read GetMerchantUpdate;
    property OrderDetails: TACBrOpenDeliveryOrderDetails read GetOrderDetails;
    property OrderConfirm: TACBrOpenDeliveryOrderConfirm read GetOrderConfirm;
    property OrderReadyForPickup: TACBrOpenDeliveryOrderReadyForPickup read GetOrderReadyForPickup;
    property OrderDispatch: TACBrOpenDeliveryOrderDispatch read GetOrderDispatch;
    property OrderDelivered: TACBrOpenDeliveryOrderDelivered read GetOrderDelivered;
    property OrderRequestCancellation: TACBrOpenDeliveryOrderRequestCancellation read GetOrderRequestCancellation;
    property OrderAcceptCancellation: TACBrOpenDeliveryOrderAcceptCancellation read GetOrderAcceptCancellation write FOrderAcceptCancellation;
    property OrderDenyCancellation: TACBrOpenDeliveryOrderDenyCancellation read GetOrderDenyCancellation write FOrderDenyCancellation;
    property Polling: TACBrOpenDeliveryPolling read GetPolling;
  end;

  TACBrOpenDeliveryWebService = class
  protected
    FOwner: TACBrComponent;
    FRequest: TACBrOpenDeliveryHTTPRequest;
    FResponse: TACBrOpenDeliveryHTTPResponse;
    FLogEnvio: TACBrOpenDeliveryHTTPLogEnvio;
    FLogResposta: TACBrOpenDeliveryHTTPLogResposta;
    FUseAuth: Boolean;
    FStatusCode: Integer;
    FStatusText: string;

    procedure SetLogEnvio(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio);
    procedure SetLogResposta(ALogResposta: TACBrOpenDeliveryHTTPLogResposta);

    procedure InicializarServico;
    procedure FinalizarServico;
    procedure DefinirDadosMsg; virtual;
    procedure DefinirRecurso; virtual;
    procedure SalvarEnvio;
    procedure EnviarDados;
    function TratarResposta: Boolean; virtual;
    procedure SalvarResposta;

    procedure FazerLog(const AMsg: string); virtual;
    procedure GerarException(const AMsg: string; AErro: Exception = nil); virtual;
    function GerarMsgLog: string; virtual;
    function GerarMsgErro(AErro: Exception): string; virtual;
  public
    constructor Create(AOwner: TACBrComponent); virtual;
    destructor Destroy; override;
    function Executar: Boolean;
    procedure Clear; virtual;

    property StatusCode: Integer read FStatusCode;
    property StatusTitle: string read FStatusText;
  end;

  { TACBrOpenDeliveryAuth }

  TACBrOpenDeliveryAuth = class(TACBrOpenDeliveryWebService)
  private
    FAccessToken: TACBrOpenDeliverySchemaAccessToken;
    FIssuedAt: TDateTime;
    function GetAccessToken: TACBrOpenDeliverySchemaAccessToken;
  protected
    procedure DefinirDadosMsg; override;
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;

  public
    constructor Create(AOwner: TACBrComponent); override;
    destructor Destroy; override;
    property AccessToken: TACBrOpenDeliverySchemaAccessToken read GetAccessToken;
  end;

  { TACBrOpenDeliveryMerchantUpdate }

  TACBrOpenDeliveryMerchantUpdate = class(TACBrOpenDeliveryWebService)
  private
    FMerchant: TACBrOpenDeliverySchemaMerchant;
    FUpdateType: TACBrODMerchantUpdateType;
    FEntityType: TACBrODMerchantUpdateEntity;

    function GetBody: TACBrJSONObject;
  protected
    procedure DefinirDadosMsg; override;
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    constructor Create(AOwner: TACBrComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    property Merchant: TACBrOpenDeliverySchemaMerchant read FMerchant write FMerchant;
    property UpdateType: TACBrODMerchantUpdateType read FUpdateType write FUpdateType;
    property EntityType: TACBrODMerchantUpdateEntity read FEntityType write FEntityType;
  end;

  { TACBrOpenDeliveryPolling }

  TACBrOpenDeliveryPolling = class(TACBrOpenDeliveryWebService)
  private
    FEventType: TACBrODEventTypeArray;
    FXPollingMerchants: TSplitResult;
    FEvents: TACBrOpenDeliverySchemaEventCollection;

    procedure InvokeEvents;
    procedure InvokeOrderPlaced(AEvent: TACBrOpenDeliverySchemaEvent; var AAck: Boolean);
    procedure Acknowledgment(AEvents: array of TACBrOpenDeliverySchemaEvent);
  protected
    procedure DefinirDadosMsg; override;
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    constructor Create(AOwner: TACBrComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function AddEventType(const AValue: TACBrODEventType): TACBrOpenDeliveryPolling;
    function AddMerchantId(const AValue: string): TACBrOpenDeliveryPolling;

    property EventType: TACBrODEventTypeArray read FEventType write FEventType;
    property XPollingMerchants: TSplitResult read FXPollingMerchants write FXPollingMerchants;
    property Events: TACBrOpenDeliverySchemaEventCollection read FEvents;
  end;

  { TACBrOpenDeliveryAcknowledgment }

  TACBrOpenDeliveryAcknowledgment = class(TACBrOpenDeliveryWebService)
  private
    FEvents: TACBrOpenDeliverySchemaAcknowledgmentCollection;
  protected
    procedure DefinirDadosMsg; override;
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    constructor Create(AOwner: TACBrComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function AddEventId(const AValue: string): TACBrOpenDeliveryAcknowledgment;

    property Events: TACBrOpenDeliverySchemaAcknowledgmentCollection read FEvents;
  end;

  { TACBrOpenDeliveryDetails }

  TACBrOpenDeliveryOrderDetails = class(TACBrOpenDeliveryWebService)
  private
    FOrder: TACBrOpenDeliverySchemaOrder;
    FOrderId: string;

    function GetOrder: TACBrOpenDeliverySchemaOrder;
  protected
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
    property Order: TACBrOpenDeliverySchemaOrder read GetOrder;
  end;

  { TACBrOpenDeliveryOrderConfirm }

  TACBrOpenDeliveryOrderConfirm = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
    FReason: string;
    FCreatedAt: TDateTime;
    FOrderExternalCode: string;
  protected
    procedure DefinirRecurso; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
    property Reason: string read FReason write FReason;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property OrderExternalCode: string read FOrderExternalCode write FOrderExternalCode;
  end;

  { TACBrOpenDeliveryOrderDispatch }

  TACBrOpenDeliveryOrderDispatch = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
  protected
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
  end;

  { TACBrOpenDeliveryOrderDelivered }

  TACBrOpenDeliveryOrderDelivered = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
  protected
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
  end;

  { TACBrOpenDeliveryOrderReadyForPickup }

  TACBrOpenDeliveryOrderReadyForPickup = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
  protected
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
  end;

  { TACBrOpenDeliveryOrderRequestCancellation }

  TACBrOpenDeliveryOrderRequestCancellation = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
    FReason: string;
    FCode: TACBrODCancelRequestCode;
    FMode: TACBrODCancelRequestMode;
    FOutOfStockItems: TSplitResult;
    FInvalidItems: TSplitResult;
  protected
    procedure DefinirRecurso; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
    property Reason: string read FReason write FReason;
    property Code: TACBrODCancelRequestCode read FCode write FCode;
    property Mode: TACBrODCancelRequestMode read FMode write FMode;
    property OutOfStockItems: TSplitResult read FOutOfStockItems write FOutOfStockItems;
    property InvalidItems: TSplitResult read FInvalidItems write FInvalidItems;

    function AddOutOfStockItems(const AValue: string): TACBrOpenDeliveryOrderRequestCancellation;
    function AddInvalidItems(const AValue: string): TACBrOpenDeliveryOrderRequestCancellation;
  end;

  { TACBrOpenDeliveryOrderAcceptCancellation }

  TACBrOpenDeliveryOrderAcceptCancellation = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
  protected
    procedure DefinirRecurso; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
  end;

  { TACBrOpenDeliveryOrderDenyCancellation }

  TACBrOpenDeliveryOrderDenyCancellation = class(TACBrOpenDeliveryWebService)
  private
    FOrderId: string;
    FReason: string;
    FCode: TACBrODDenyCancelCode;
  protected
    procedure DefinirRecurso; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  public
    procedure Clear; override;

    property OrderId: string read FOrderId write FOrderId;
    property Reason: string read FReason write FReason;
    property Code: TACBrODDenyCancelCode read FCode write FCode;
  end;

implementation

uses
  ACBrOpenDelivery;

function GetACBrOpenDelivery(AACBrComponent: TACBrComponent): TACBrOpenDelivery;
begin
  Result := TACBrOpenDelivery(AACBrComponent);
end;

function GetToken(AACBrComponent: TACBrComponent): string;
begin
  Result := GetACBrOpenDelivery(AACBrComponent).GetToken;
end;

{ TACBrOpenDeliveryOrderDelivered }

procedure TACBrOpenDeliveryOrderDelivered.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetOrderDelivered(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderDelivered.TratarResposta: Boolean;
begin
  Result := True;
end;

procedure TACBrOpenDeliveryOrderDelivered.Clear;
begin
  inherited;
  FOrderId := '';
end;

{ TACBrOpenDeliveryOrderDetails }

procedure TACBrOpenDeliveryOrderDetails.Clear;
begin
  inherited;
  FOrderId := '';
end;

procedure TACBrOpenDeliveryOrderDetails.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources.GetOrderDetails(FOrderId, '');
  FRequest.GET.Resource(LResource);
end;

destructor TACBrOpenDeliveryOrderDetails.Destroy;
begin
  FOrder.Free;
  inherited;
end;

function TACBrOpenDeliveryOrderDetails.GetOrder: TACBrOpenDeliverySchemaOrder;
begin
  if not Assigned(FOrder) then
    FOrder := TACBrOpenDeliverySchemaOrder.Create;
  Result := FOrder;
end;

function TACBrOpenDeliveryOrderDetails.TratarResposta: Boolean;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := FResponse.GetJSONObject;
  Order.Clear;
  Order.AsJSON := LJSON.ToJSON;
  Result := True;
end;

{ TACBrOpenDeliveryWebService }

procedure TACBrOpenDeliveryWebService.Clear;
begin

end;

constructor TACBrOpenDeliveryWebService.Create(AOwner: TACBrComponent);
begin
  FOwner := AOwner;
  FUseAuth := True;
end;

procedure TACBrOpenDeliveryWebService.DefinirDadosMsg;
begin
end;

procedure TACBrOpenDeliveryWebService.DefinirRecurso;
begin

end;

destructor TACBrOpenDeliveryWebService.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  inherited;
end;

function TACBrOpenDeliveryWebService.Executar: Boolean;
var
  LMsgErro: string;
begin
  FazerLog('Inicio ' + ClassName);
  InicializarServico;
  try
    DefinirRecurso;
    DefinirDadosMsg;
    SalvarEnvio;
    try
      EnviarDados;
      try
        Result := TratarResposta;
        Clear;
      finally
        FazerLog(GerarMsgLog);
        SalvarResposta;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        LMsgErro := GerarMsgErro(E);
        GerarException(LMsgErro, E);
      end;
    end;
  finally
    FinalizarServico;
  end;
end;

procedure TACBrOpenDeliveryWebService.EnviarDados;
var
  LError: TACBrOpenDeliverySchemaError;
  LJSONObject: TACBrJSONObject;
  LStatus: Integer;
  LTitle: string;
  LContent: string;
begin
  FreeAndNil(FResponse);
  FLogEnvio := nil;
  FLogResposta := nil;
  FResponse := FRequest.Send;
  FStatusText := FResponse.StatusText;
  FStatusCode := FResponse.StatusCode;

  LStatus := FStatusCode;
  LTitle := FStatusText;
  LContent := FResponse.GetContent;
  if FStatusCode >= 400 then
  begin
    LJSONObject := FResponse.GetJSONObject;
    if Assigned(LJSONObject) then
    begin
      LError := TACBrOpenDeliverySchemaError.Create;
      try
        LError.AsJSON := LJSONObject.ToJSON;
        LStatus := LError.Status;
        LTitle := LError.Title;
      finally
        LError.Free;
      end;
    end;

    raise EACBrOpenDeliveryHTTPException.Create(LStatus, LTitle, LContent);
  end;
end;

procedure TACBrOpenDeliveryWebService.FazerLog(const AMsg: string);
var
  LTratado: Boolean;
begin
  if (AMsg <> '') then
    GetACBrOpenDelivery(FOwner).FazerLog(AMsg, LTratado);
end;

procedure TACBrOpenDeliveryWebService.FinalizarServico;
begin
end;

procedure TACBrOpenDeliveryWebService.GerarException(const AMsg: string; AErro: Exception);
var
  LTratado: Boolean;
begin
  LTratado := False;
  if Assigned(GetACBrOpenDelivery(FOwner).OnHTTPError) then
  begin
    if AErro is EACBrOpenDeliveryHTTPException then
      GetACBrOpenDelivery(FOwner).OnHTTPError(FLogEnvio, FLogResposta,
        EACBrOpenDeliveryHTTPException(AErro), LTratado);
  end;
  if not LTratado then
    GetACBrOpenDelivery(FOwner).GerarException(AMsg, AErro);
end;

function TACBrOpenDeliveryWebService.GerarMsgErro(AErro: Exception): string;
begin
  Result := '';
end;

function TACBrOpenDeliveryWebService.GerarMsgLog: string;
begin
  Result := '';
end;

procedure TACBrOpenDeliveryWebService.InicializarServico;
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  if not Assigned(FRequest) then
    FRequest := TACBrOpenDeliveryHTTPRequest.New;
  FRequest
    .OnHTTPEnvio(SetLogEnvio)
    .OnHTTPResposta(SetLogResposta)
    .BaseURL(LComponent.MarketPlace.BaseUrl)
    .TimeOut(LComponent.TimeOut)
    .ProxyHost(LComponent.Proxy.Host)
    .ProxyPort(LComponent.Proxy.Port)
    .ProxyUser(LComponent.Proxy.User)
    .ProxyPass(LComponent.Proxy.Pass);

  if FUseAuth then
    FRequest.Token(LComponent.GetToken);
end;

procedure TACBrOpenDeliveryWebService.SalvarEnvio;
begin

end;

procedure TACBrOpenDeliveryWebService.SalvarResposta;
begin

end;

procedure TACBrOpenDeliveryWebService.SetLogEnvio(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio);
begin
  FLogEnvio := ALogEnvio;
  if Assigned(GetACBrOpenDelivery(FOwner).OnHTTPEnviar) then
    GetACBrOpenDelivery(FOwner).OnHTTPEnviar(ALogEnvio);
end;

procedure TACBrOpenDeliveryWebService.SetLogResposta(ALogResposta: TACBrOpenDeliveryHTTPLogResposta);
begin
  FLogResposta := ALogResposta;
  if Assigned(GetACBrOpenDelivery(FOwner).OnHTTPRetornar) then
    GetACBrOpenDelivery(FOwner).OnHTTPRetornar(ALogResposta);
end;

function TACBrOpenDeliveryWebService.TratarResposta: Boolean;
begin
  raise Exception.Create('Not Implemented.');
end;

{ TACBrOpenDeliveryAuth }

constructor TACBrOpenDeliveryAuth.Create(AOwner: TACBrComponent);
begin
  inherited Create(AOwner);
  FUseAuth := False;
end;

procedure TACBrOpenDeliveryAuth.DefinirDadosMsg;
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  FIssuedAt := Now;
  FRequest
    .AddOrSetUrlEncoded('grant_type', 'client_credentials')
    .AddOrSetUrlEncoded('client_id', LComponent.MarketPlace.Credenciais.ClientId)
    .AddOrSetUrlEncoded('client_secret', LComponent.MarketPlace.Credenciais.ClientSecret);
end;

procedure TACBrOpenDeliveryAuth.DefinirRecurso;
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  FRequest
    .POST
    .Resource(LComponent.MarketPlace.Resources.GetAuthentication(''));
end;

destructor TACBrOpenDeliveryAuth.Destroy;
begin
  FAccessToken.Free;
  inherited;
end;

function TACBrOpenDeliveryAuth.GetAccessToken: TACBrOpenDeliverySchemaAccessToken;
begin
  if not Assigned(FAccessToken) then
    FAccessToken := TACBrOpenDeliverySchemaAccessToken.Create;
  Result := FAccessToken;
end;

function TACBrOpenDeliveryAuth.TratarResposta: Boolean;
var
  LJSON: TACBrJSONObject;
begin
  AccessToken.Clear;
  LJSON := FResponse.GetJSONObject;
  AccessToken.AsJSON := LJSON.ToJSON;
  AccessToken.expiresAt := IncSecond(FIssuedAt, AccessToken.expiresIn);
  Result := True;
end;

{ TACBrOpenDeliveryWebServices }

constructor TACBrOpenDeliveryWebServices.Create(AOwner: TACBrComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TACBrOpenDeliveryWebServices.Destroy;
begin
  FAuth.Free;
  FAcknowledgment.Free;
  FPolling.Free;
  FMerchantUpdate.Free;
  FOrderDetails.Free;
  FOrderConfirm.Free;
  FOrderDispatch.Free;
  FOrderDelivered.Free;
  FOrderReadyForPickup.Free;
  FOrderRequestCancellation.Free;
  FOrderAcceptCancellation.Free;
  FOrderDenyCancellation.Free;
  inherited;
end;

function TACBrOpenDeliveryWebServices.GetAcknowledgment: TACBrOpenDeliveryAcknowledgment;
begin
  if not Assigned(FAcknowledgment) then
    FAcknowledgment := TACBrOpenDeliveryAcknowledgment.Create(FOwner);
  Result := FAcknowledgment;
end;

function TACBrOpenDeliveryWebServices.GetAuth: TACBrOpenDeliveryAuth;
begin
  if not Assigned(FAuth) then
    FAuth := TACBrOpenDeliveryAuth.Create(FOwner);
  Result := FAuth;
end;

function TACBrOpenDeliveryWebServices.GetMerchantUpdate: TACBrOpenDeliveryMerchantUpdate;
begin
  if not Assigned(FMerchantUpdate) then
    FMerchantUpdate := TACBrOpenDeliveryMerchantUpdate.Create(FOwner);
  Result := FMerchantUpdate;
end;

function TACBrOpenDeliveryWebServices.GetOrderAcceptCancellation: TACBrOpenDeliveryOrderAcceptCancellation;
begin
  if not Assigned(FOrderAcceptCancellation) then
    FOrderAcceptCancellation := TACBrOpenDeliveryOrderAcceptCancellation.Create(FOwner);
  Result := FOrderAcceptCancellation;
end;

function TACBrOpenDeliveryWebServices.GetOrderConfirm: TACBrOpenDeliveryOrderConfirm;
begin
  if not Assigned(FOrderConfirm) then
    FOrderConfirm := TACBrOpenDeliveryOrderConfirm.Create(FOwner);
  Result := FOrderConfirm;
end;

function TACBrOpenDeliveryWebServices.GetOrderDenyCancellation: TACBrOpenDeliveryOrderDenyCancellation;
begin
  if not Assigned(FOrderDenyCancellation) then
    FOrderDenyCancellation := TACBrOpenDeliveryOrderDenyCancellation.Create(FOwner);
  Result := FOrderDenyCancellation;
end;

function TACBrOpenDeliveryWebServices.GetOrderDetails: TACBrOpenDeliveryOrderDetails;
begin
  if not Assigned(FOrderDetails) then
    FOrderDetails := TACBrOpenDeliveryOrderDetails.Create(FOwner);
  Result := FOrderDetails;
end;

function TACBrOpenDeliveryWebServices.GetOrderDispatch: TACBrOpenDeliveryOrderDispatch;
begin
  if not Assigned(FOrderDispatch) then
    FOrderDispatch := TACBrOpenDeliveryOrderDispatch.Create(FOwner);
  Result := FOrderDispatch;
end;

function TACBrOpenDeliveryWebServices.GetOrderDelivered: tacbropendeliveryorderDelivered;
begin
  if not Assigned(FOrderDelivered) then
    FOrderDelivered := TACBrOpenDeliveryOrderDelivered.Create(FOwner);
  Result := FOrderDelivered;
end;

function TACBrOpenDeliveryWebServices.GetOrderReadyForPickup: TACBrOpenDeliveryOrderReadyForPickup;
begin
  if not Assigned(FOrderReadyForPickup) then
    FOrderReadyForPickup := TACBrOpenDeliveryOrderReadyForPickup.Create(FOwner);
  Result := FOrderReadyForPickup;
end;

function TACBrOpenDeliveryWebServices.GetOrderRequestCancellation: TACBrOpenDeliveryOrderRequestCancellation;
begin
  if not Assigned(FOrderRequestCancellation) then
    FOrderRequestCancellation := TACBrOpenDeliveryOrderRequestCancellation.Create(FOwner);
  Result := FOrderRequestCancellation;
end;

function TACBrOpenDeliveryWebServices.GetPolling: TACBrOpenDeliveryPolling;
begin
  if not Assigned(FPolling) then
    FPolling := TACBrOpenDeliveryPolling.Create(FOwner);
  Result := FPolling;
end;

{ TACBrOpenDeliveryPolling }

procedure TACBrOpenDeliveryPolling.Acknowledgment(AEvents: array of TACBrOpenDeliverySchemaEvent);
var
  LWebService: TACBrOpenDeliveryAcknowledgment;
  I: Integer;
begin
  if Length(AEvents) > 0 then
  begin
    LWebService := TACBrOpenDeliveryAcknowledgment.Create(FOwner);
    try
      for I := 0 to Pred(Length(AEvents)) do
      begin
        LWebService.Events.New;
        LWebService.Events[I].Id := AEvents[I].EventId;
        LWebService.Events[I].OrderId := AEvents[I].OrderId;
        LWebService.Events[I].EventType := AEvents[I].EventType;
      end;

      LWebService.Executar;
    finally
      LWebService.Free;
    end;
  end;
end;

function TACBrOpenDeliveryPolling.AddEventType(const AValue: TACBrODEventType): TACBrOpenDeliveryPolling;
begin
  Result := Self;
  SetLength(FEventType, Length(FEventType) + 1);
  FEventType[Length(FEventType) - 1] := AValue;
end;

function TACBrOpenDeliveryPolling.AddMerchantId(const AValue: string): TACBrOpenDeliveryPolling;
begin
  Result := Self;
  SetLength(FXPollingMerchants, Length(XPollingMerchants) + 1);
  FXPollingMerchants[Length(XPollingMerchants) - 1] := AValue;
end;

procedure TACBrOpenDeliveryPolling.Clear;
begin
  inherited;
  SetLength(FXPollingMerchants, 0);
  SetLength(FEventType, 0);
end;

constructor TACBrOpenDeliveryPolling.Create(AOwner: TACBrComponent);
begin
  inherited Create(AOwner);
  FEvents := TACBrOpenDeliverySchemaEventCollection.Create('');
end;

procedure TACBrOpenDeliveryPolling.DefinirDadosMsg;
var
  LStrParam: string;
  I: Integer;
begin
  LStrParam := '';
  for I := 0 to Pred(Length(FXPollingMerchants)) do
  begin
    if I > 0 then
      LStrParam := LStrParam + ',';
    LStrParam := LStrParam + FXPollingMerchants[I];
  end;

  if LStrParam <> '' then
    FRequest.AddOrSetHeader('x-polling-merchants', LStrParam);

  LStrParam := '';
  for I := 0 to Pred(Length(FEventType)) do
  begin
    if I > 0 then
      LStrParam := LStrParam + ',';
    LStrParam := LStrParam + EventTypeToStr(FEventType[I]);
  end;

  if LStrParam <> '' then
    FRequest.AddOrSetQuery('eventType', LStrParam);
end;

procedure TACBrOpenDeliveryPolling.DefinirRecurso;
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  FRequest
    .GET
    .Resource(LComponent.MarketPlace.Resources.GetEventPolling(''));
end;

destructor TACBrOpenDeliveryPolling.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure TACBrOpenDeliveryPolling.InvokeEvents;
var
  I: Integer;
  LComponent: TACBrOpenDelivery;
  LEvent: TACBrOpenDeliverySchemaEvent;
  LEventStatus: TOnEventStatus;
  LPollingEnd: TOnPollingEnd;
  LEventsToAck: array of TACBrOpenDeliverySchemaEvent;
  LAck: Boolean;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LPollingEnd := LComponent.OnPollingEnd;
  if Assigned(LPollingEnd) then
    LPollingEnd(Now, FEvents);

  for I := 0 to Pred(FEvents.Count) do
  begin
    LAck := False;
    LEventStatus := nil;
    LEvent := FEvents[I];
    case LEvent.EventType of
      etCreated: InvokeOrderPlaced(LEvent, LAck);
      etConfirmed: LEventStatus := LComponent.OnEventConfirmed;
      etDispatched: LEventStatus := LComponent.OnEventDispatched;
      etReadyForPickup: LEventStatus := LComponent.OnEventReadyForPickup;
      etPickupAreaAssigned: LEventStatus := LComponent.OnEventPickupAreaAssigned;
      etDelivered: LEventStatus := LComponent.OnEventDelivered;
      etConcluded: LEventStatus := LComponent.OnEventConcluded;
      etCancellationRequested: LEventStatus := LComponent.OnEventCancellationRequested;
      etCancellationRequestDenied: LEventStatus := LComponent.OnEventCancellationRequestDenied;
      etCancelled: LEventStatus := LComponent.OnEventCancelled;
      etOrderCancellationRequest: LEventStatus := LComponent.OnEventOrderCancellationRequest;
    end;

    if Assigned(LEventStatus) then
    begin
      LAck := False;
      LEventStatus(LEvent, LAck);
    end;

    if LAck then
    begin
      SetLength(LEventsToAck, Length(LEventsToAck) + 1);
      LEventsToAck[Length(LEventsToAck) - 1] := LEvent;
    end;
  end;
  Acknowledgment(LEventsToAck);
end;

procedure TACBrOpenDeliveryPolling.InvokeOrderPlaced(AEvent: TACBrOpenDeliverySchemaEvent; var AAck: Boolean);
var
  LComponent: TACBrOpenDelivery;
  LEvent: TOnEventOrder;
  LWebService: TACBrOpenDeliveryOrderDetails;
  LOrder: TACBrOpenDeliverySchemaOrder;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LEvent := LComponent.OnEventOrderPlaced;
  if Assigned(LEvent) then
  begin
    LWebService := TACBrOpenDeliveryOrderDetails.Create(FOwner);
    try
      LWebService.OrderId := AEvent.OrderId;
      if LWebService.Executar then
      begin
        LOrder := LWebService.Order;
        LEvent(AEvent, LOrder, AAck);
      end;
    finally
      LWebService.Free;
    end;
  end;
end;

function TACBrOpenDeliveryPolling.TratarResposta: Boolean;
var
  LJSON: TACBrJSONArray;
begin
  Result := True;
  Events.Clear;
  LJSON := FResponse.GetJSONArray;
  if Assigned(LJSON) then
  begin
    Events.AsJSON := LJSON.ToJSON;
  end;
  InvokeEvents;
end;

{ TACBrOpenDeliveryAcknowledgment }

function TACBrOpenDeliveryAcknowledgment.AddEventId(const AValue: string): TACBrOpenDeliveryAcknowledgment;
begin
  Result := Self;
  FEvents.New.Id := AValue;
end;

procedure TACBrOpenDeliveryAcknowledgment.Clear;
begin
  inherited;
  FEvents.Clear;
end;

constructor TACBrOpenDeliveryAcknowledgment.Create(AOwner: TACBrComponent);
begin
  inherited Create(AOwner);
  FEvents := TACBrOpenDeliverySchemaAcknowledgmentCollection.Create('');
end;

procedure TACBrOpenDeliveryAcknowledgment.DefinirDadosMsg;
var
  LJSON: TACBrJSONArray;
begin
  LJSON := FEvents.ToJSonArray;
  try
    FRequest.Body(LJSON, False);
  finally
    LJSON.Free;
  end;
end;

procedure TACBrOpenDeliveryAcknowledgment.DefinirRecurso;
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  FRequest
    .POST
    .Resource(LComponent.MarketPlace.Resources.GetEventAcknowledgment(''));
end;

destructor TACBrOpenDeliveryAcknowledgment.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TACBrOpenDeliveryAcknowledgment.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderConfirm }

procedure TACBrOpenDeliveryOrderConfirm.Clear;
begin
  inherited;
  FOrderId := '';
  FReason := '';
  FCreatedAt := 0;
  FOrderExternalCode := '';
end;

procedure TACBrOpenDeliveryOrderConfirm.DefinirDadosMsg;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := TACBrJSONObject.Create;
  try
    LJSON
      .AddPair('reason', FReason)
      .AddPairISODateTime('createdAt', FCreatedAt, False)
      .AddPair('orderExternalCode', FOrderExternalCode);

    FRequest.Body(LJSON, False);
  finally
    LJSON.Free;
  end;
end;

procedure TACBrOpenDeliveryOrderConfirm.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources.GetOrderConfirm(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderConfirm.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderReadyForPickup }

procedure TACBrOpenDeliveryOrderReadyForPickup.Clear;
begin
  inherited;
  FOrderId := '';
end;

procedure TACBrOpenDeliveryOrderReadyForPickup.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources.GetOrderReadyForPickup(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderReadyForPickup.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderDispatch }

procedure TACBrOpenDeliveryOrderDispatch.Clear;
begin
  inherited;
  FOrderId := '';
end;

procedure TACBrOpenDeliveryOrderDispatch.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetOrderDispatch(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderDispatch.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderRequestCancellation }

function TACBrOpenDeliveryOrderRequestCancellation.AddInvalidItems(const AValue: string): TACBrOpenDeliveryOrderRequestCancellation;
begin
  Result := Self;
  SetLength(FInvalidItems, Length(FInvalidItems) + 1);
  FInvalidItems[Length(FInvalidItems) - 1] := AValue;
end;

function TACBrOpenDeliveryOrderRequestCancellation.AddOutOfStockItems(const AValue: string): TACBrOpenDeliveryOrderRequestCancellation;
begin
  Result := Self;
  SetLength(FOutOfStockItems, Length(FOutOfStockItems) + 1);
  FOutOfStockItems[Length(FOutOfStockItems) - 1] := AValue;
end;

procedure TACBrOpenDeliveryOrderRequestCancellation.Clear;
begin
  inherited;
  FOrderId := '';
  FReason := '';
  FCode := crcSystemicIssues;
  FMode := crmAuto;
  SetLength(FInvalidItems, 0);
  SetLength(FOutOfStockItems, 0);
end;

procedure TACBrOpenDeliveryOrderRequestCancellation.DefinirDadosMsg;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := TACBrJSONObject.Create;
  try
    LJSON
      .AddPair('reason', FReason)
      .AddPair('code', CancelRequestCodeToStr(FCode))
      .AddPair('mode', CancelRequestModeToStr(FMode))
      .AddPair('outOfStockItems', FOutOfStockItems)
      .AddPair('invalidItems', FInvalidItems);

    FRequest.Body(LJSON, False);
  finally
    LJSON.Free;
  end;
end;

procedure TACBrOpenDeliveryOrderRequestCancellation.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetOrderRequestCancellation(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderRequestCancellation.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderAcceptCancellation }

procedure TACBrOpenDeliveryOrderAcceptCancellation.Clear;
begin
  inherited;
  FOrderId := '';
end;

procedure TACBrOpenDeliveryOrderAcceptCancellation.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetOrderAcceptCancellation(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderAcceptCancellation.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryOrderDenyCancellation }

procedure TACBrOpenDeliveryOrderDenyCancellation.Clear;
begin
  inherited;
  FOrderId := '';
  FReason := '';
  FCode := dccDishAlreadyDone;
end;

procedure TACBrOpenDeliveryOrderDenyCancellation.DefinirDadosMsg;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := TACBrJSONObject.Create;
  try
    LJSON
      .AddPair('reason', FReason)
      .AddPair('code', DenyCancelCodeToStr(FCode));

    FRequest.Body(LJSON, False);
  finally
    LJSON.Free;
  end;
end;

procedure TACBrOpenDeliveryOrderDenyCancellation.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetOrderDenyCancellation(FOrderId, '');
  FRequest.POST.Resource(LResource);
end;

function TACBrOpenDeliveryOrderDenyCancellation.TratarResposta: Boolean;
begin
  Result := True;
end;

{ TACBrOpenDeliveryMerchantUpdate }

procedure TACBrOpenDeliveryMerchantUpdate.Clear;
begin
  inherited;
  FEntityType := mueService;
  FUpdateType := mutEmptyBody;
end;

constructor TACBrOpenDeliveryMerchantUpdate.Create(AOwner: TACBrComponent);
begin
  inherited;
  FMerchant := TACBrOpenDeliverySchemaMerchant.Create;
end;

procedure TACBrOpenDeliveryMerchantUpdate.DefinirDadosMsg;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := GetBody;
  try
    if Assigned(LJSON) then
      FRequest.Body(LJSON, False);
  finally
    LJSON.Free;
  end;
end;

procedure TACBrOpenDeliveryMerchantUpdate.DefinirRecurso;
var
  LResource: string;
  LComponent: TACBrOpenDelivery;
begin
  LComponent := GetACBrOpenDelivery(FOwner);
  LResource := LComponent.MarketPlace.Resources
    .GetMerchantUpdate(FMerchant.id);
  FRequest.POST.Resource(LResource);
end;

destructor TACBrOpenDeliveryMerchantUpdate.Destroy;
begin
  FMerchant.Free;
  inherited;
end;

function TACBrOpenDeliveryMerchantUpdate.GetBody: TACBrJSONObject;
var
  LJSONArray: TACBrJSONArray;
begin
  // https://abrasel-nacional.github.io/docs/#tag/merchantUpdate/operation/menuUpdated
  Result := TACBrJSONObject.Create;
  try
    if FUpdateType in [mutOnlyStatus, mutStatusEntityType] then
      Result.AddPair('merchantStatus', StatusToStr(FMerchant.status));

    if FUpdateType in [mutEntityType, mutStatusEntityType] then
    begin
      LJSONArray := nil;
      case FEntityType of
        mueService: LJSONArray := FMerchant.services.ToJSonArray;
        mueMenu: LJSONArray := FMerchant.menus.ToJSonArray;
        mueCategory: LJSONArray := FMerchant.categories.ToJSonArray;
        mueItem: LJSONArray := FMerchant.items.ToJSonArray;
        mueItemOffer: LJSONArray := FMerchant.itemOffers.ToJSonArray;
        mueOptionGroup: LJSONArray := FMerchant.optionGroups.ToJSonArray;
        mueAvailability: LJSONArray := FMerchant.availabilities.ToJSonArray;
      end;

      if Assigned(LJSONArray) then
        Result.AddPair('updateObjects', LJSONArray);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TACBrOpenDeliveryMerchantUpdate.TratarResposta: Boolean;
begin
  Result := True;
end;

end.
