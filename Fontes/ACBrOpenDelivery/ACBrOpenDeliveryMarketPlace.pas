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

unit ACBrOpenDeliveryMarketPlace;

interface

uses
  ACBrBase,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  Classes,
  SysUtils;

type
  TACBrODMarketPlace = (mpOutro, mpHubDelivery);

  TACBrOpenDeliveryCredential = class;
  TACBrOpenDeliveryResources = class;

  TACBrOpenDeliveryMarketPlace = class(TPersistent)
  private
    FOwner: TACBrComponent;
    FName: TACBrODMarketPlace;
    FDescription: string;
    FBaseUrl: string;
    FCredenciais: TACBrOpenDeliveryCredential;
    FResources: TACBrOpenDeliveryResources;
    procedure SetName(const Value: TACBrODMarketPlace);
  public
    constructor Create(AOwner: TACBrComponent);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ASource: TACBrOpenDeliveryMarketPlace); reintroduce;
  published
    property Name: TACBrODMarketPlace read FName write SetName;
    property Description: string read FDescription write FDescription;
    property BaseUrl: string read FBaseUrl write FBaseUrl;
    property Credenciais: TACBrOpenDeliveryCredential read FCredenciais write FCredenciais;
    property Resources: TACBrOpenDeliveryResources read FResources write FResources;
  end;

  TACBrOpenDeliveryCredential = class(TPersistent)
  private
    FOwner: TACBrComponent;
    FClientId: string;
    FClientSecret: string;
  public
    constructor Create(AOwner: TACBrComponent);
    procedure Clear;
    procedure Assign(ASource: TACBrOpenDeliveryCredential); reintroduce;
    procedure Validar;
  published
    property ClientId: string read FClientId write FClientId;
    property ClientSecret: string read FClientSecret write FClientSecret;
  end;

  TACBrOpenDeliveryResources = class(TPersistent)
  private
    FOwner: TACBrComponent;
    FAuthentication: string;
    FMerchantUpdate: string;
    FMerchantStatus: string;
    FEventPolling: string;
    FEventAcknowledgment: string;
    FOrderDetails: string;
    FOrderConfirm: string;
    FOrderReadyForPickup: string;
    FOrderDispatch: string;
    FOrderRequestCancellation: string;
    FOrderAcceptCancellation: string;
    FOrderDenyCancellation: string;

    function ReplaceResource(const AResource, AOrderId, AMerchantId: string): string;
  public
    constructor Create(AOwner: TACBrComponent);
    procedure Clear;
    procedure Assign(ASource: TACBrOpenDeliveryResources); reintroduce;

    function GetAuthentication(const AMerchantId: string): string;
    function GetMerchantUpdate(const AMerchantId: string): string;
    function GetMerchantStatus(const AMerchantId: string): string;
    function GetEventPolling(const AMerchantId: string): string;
    function GetEventAcknowledgment(const AMerchantId: string): string;
    function GetOrderDetails(const AOrderId, AMerchantId: string): string;
    function GetOrderConfirm(const AOrderId, AMerchantId: string): string;
    function GetOrderReadyForPickup(const AOrderId, AMerchantId: string): string;
    function GetOrderDispatch(const AOrderId, AMerchantId: string): string;
    function GetOrderRequestCancellation(const AOrderId, AMerchantId: string): string;
    function GetOrderAcceptCancellation(const AOrderId, AMerchantId: string): string;
    function GetOrderDenyCancellation(const AOrderId, AMerchantId: string): string;
  published
    property Authentication: string read FAuthentication write FAuthentication;
    property MerchantUpdate: string read FMerchantUpdate write FMerchantUpdate;
    property MerchantStatus: string read FMerchantStatus write FMerchantStatus;
    property EventPolling: string read FEventPolling write FEventPolling;
    property EventAcknowledgment: string read FEventAcknowledgment write FEventAcknowledgment;
    property OrderDetails: string read FOrderDetails write FOrderDetails;
    property OrderConfirm: string read FOrderConfirm write FOrderConfirm;
    property OrderReadyForPickup: string read FOrderReadyForPickup write FOrderReadyForPickup;
    property OrderDispatch: string read FOrderDispatch write FOrderDispatch;
    property OrderRequestCancellation: string read FOrderRequestCancellation write FOrderRequestCancellation;
    property OrderAcceptCancellation: string read FOrderAcceptCancellation write FOrderAcceptCancellation;
    property OrderDenyCancellation: string read FOrderDenyCancellation write FOrderDenyCancellation;
  end;

implementation

uses
  ACBrOpenDelivery;

const
// 'OPENDELIVERY'
  CMarketPlaceDescription = 'Open Delivery';
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

// 'HUBDELIVERY'
  CHubDeliveryMarketPlaceDescription = 'Hub Delivery';
  CHubDeliveryMarketBaseUrl = 'https://sandbox.myhubdelivery.io';
  CHubDeliveryEndPointAuthentication = 'license-manager/api/v1/oauth/token';
  CHubDeliveryEndPointMerchantUpdate = 'merchants/api/v1/{merchantId}/merchantUpdate';
  CHubDeliveryEndPointMerchantStatus = 'merchants/api/v1/{merchantId}/merchantStatus';
  CHubDeliveryEndPointEventPolling = 'orders/api/v1/events:polling';
  CHubDeliveryEndPointEventAcknowledgment = 'orders/api/v1/events/acknowledgment';
  CHubDeliveryEndPointOrderDetails = 'orders/api/v1/{orderId}';
  CHubDeliveryEndPointOrderConfirm = 'orders/api/v1/{orderId}/confirm';
  CHubDeliveryEndPointOrderReadyForPickup = 'orders/api/v1/{orderId}/readyForPickup';
  CHubDeliveryEndPointOrderDispatch = 'orders/api/v1/{orderId}/dispatch';
  CHubDeliveryEndPointOrderRequestCancellation = 'orders/api/v1/{orderId}/requestCancellation';
  CHubDeliveryEndPointOrderAcceptCancellation = 'orders/api/v1/{orderId}/acceptCancellation';
  CHubDeliveryEndPointOrderDenyCancellation = 'orders/api/v1/{orderId}/denyCancellation';

procedure GerarException(AOwner: TACBrComponent; const AMsg: string);
var
  LComponent: TACBrOpenDelivery;
begin
  LComponent := TACBrOpenDelivery(AOwner);
  LComponent.GerarException(ACBrStr('ERRO: ' + AMsg));
end;

function GetMarketPlaceDescription(const AName: TACBrODMarketPlace): string;
begin
  Result := CMarketPlaceDescription;
  case AName of
    mpHubDelivery: Result := CHubDeliveryMarketPlaceDescription;
  end;
end;

procedure ConfigOpenDelivery(AMarketPlace: TACBrOpenDeliveryMarketPlace);
begin
  AMarketPlace.Description := CMarketPlaceDescription;
  AMarketPlace.BaseUrl := '';
  AMarketPlace.Resources.Authentication := CEndPointAuthentication;
  AMarketPlace.Resources.MerchantUpdate := CEndPointMerchantUpdate;
  AMarketPlace.Resources.MerchantStatus := CEndPointMerchantStatus;
  AMarketPlace.Resources.EventPolling := CEndPointEventPolling;
  AMarketPlace.Resources.EventAcknowledgment := CEndPointEventAcknowledgment;
  AMarketPlace.Resources.OrderDetails := CEndPointOrderDetails;
  AMarketPlace.Resources.OrderConfirm := CEndPointOrderConfirm;
  AMarketPlace.Resources.OrderDispatch := CEndPointOrderDispatch;
  AMarketPlace.Resources.OrderReadyForPickup := CEndPointOrderReadyForPickup;
  AMarketPlace.Resources.OrderRequestCancellation := CEndPointOrderRequestCancellation;
  AMarketPlace.Resources.OrderAcceptCancellation := CEndPointOrderAcceptCancellation;
  AMarketPlace.Resources.OrderDenyCancellation := CEndPointOrderDenyCancellation;
end;

procedure ConfigHubDelivery(AMarketPlace: TACBrOpenDeliveryMarketPlace);
begin
  AMarketPlace.Description := CHubDeliveryMarketPlaceDescription;
  AMarketPlace.BaseUrl := CHubDeliveryMarketBaseUrl;
  AMarketPlace.Resources.Authentication := CHubDeliveryEndPointAuthentication;
  AMarketPlace.Resources.MerchantUpdate := CHubDeliveryEndPointMerchantUpdate;
  AMarketPlace.Resources.MerchantStatus := CHubDeliveryEndPointMerchantStatus;
  AMarketPlace.Resources.EventPolling := CHubDeliveryEndPointEventPolling;
  AMarketPlace.Resources.EventAcknowledgment := CHubDeliveryEndPointEventAcknowledgment;
  AMarketPlace.Resources.OrderDetails := CHubDeliveryEndPointOrderDetails;
  AMarketPlace.Resources.OrderConfirm := CHubDeliveryEndPointOrderConfirm;
  AMarketPlace.Resources.OrderDispatch := CHubDeliveryEndPointOrderDispatch;
  AMarketPlace.Resources.OrderReadyForPickup := CHubDeliveryEndPointOrderReadyForPickup;
  AMarketPlace.Resources.OrderRequestCancellation := CHubDeliveryEndPointOrderRequestCancellation;
  AMarketPlace.Resources.OrderAcceptCancellation := CHubDeliveryEndPointOrderAcceptCancellation;
  AMarketPlace.Resources.OrderDenyCancellation := CHubDeliveryEndPointOrderDenyCancellation;
end;

{ TACBrOpenDeliveryMarketPlace }

procedure TACBrOpenDeliveryMarketPlace.Assign(ASource: TACBrOpenDeliveryMarketPlace);
begin
  FName := ASource.Name;
  FDescription := ASource.Description;
  FBaseUrl := ASource.BaseUrl;
  FCredenciais.Assign(ASource.Credenciais);
  FResources.Assign(ASource.Resources);
end;

procedure TACBrOpenDeliveryMarketPlace.Clear;
begin
  FName := mpOutro;
  FDescription := '';
  FBaseUrl := '';
  FCredenciais.Clear;
  FResources.Clear;
end;

constructor TACBrOpenDeliveryMarketPlace.Create(AOwner: TACBrComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FName := mpOutro;
  FCredenciais := TACBrOpenDeliveryCredential.Create(FOwner);
  FResources := TACBrOpenDeliveryResources.Create(FOwner);
end;

destructor TACBrOpenDeliveryMarketPlace.Destroy;
begin
  FCredenciais.Free;
  FResources.Free;
  inherited;
end;

procedure TACBrOpenDeliveryMarketPlace.SetName(const Value: TACBrODMarketPlace);
begin
  if FName <> Value then
  begin
    FName := Value;
    case FName of
      mpOutro: ConfigOpenDelivery(Self);
      mpHubDelivery: ConfigHubDelivery(Self);
    end;
  end;
end;

{ TACBrOpenDeliveryResources }

procedure TACBrOpenDeliveryResources.Assign(ASource: TACBrOpenDeliveryResources);
begin
  FAuthentication := ASource.Authentication;
  FMerchantUpdate := ASource.MerchantUpdate;
  FMerchantStatus := ASource.MerchantStatus;
  FEventPolling := ASource.EventPolling;
  FEventAcknowledgment := ASource.EventAcknowledgment;
  FOrderDetails := ASource.OrderDetails;
  FOrderConfirm := ASource.OrderConfirm;
  FOrderDispatch := ASource.OrderDispatch;
  FOrderReadyForPickup := ASource.OrderReadyForPickup;
  FOrderDenyCancellation := ASource.OrderDenyCancellation;
  FOrderAcceptCancellation := ASource.OrderAcceptCancellation;
  FOrderRequestCancellation := ASource.OrderRequestCancellation;
end;

procedure TACBrOpenDeliveryResources.Clear;
begin
  FAuthentication := '';
  FMerchantUpdate := '';
  FMerchantStatus := '';
  FEventPolling := '';
  FEventAcknowledgment := '';
  FOrderDetails := '';
  FOrderConfirm := '';
  FOrderDispatch := '';
  FOrderReadyForPickup := '';
  FOrderDenyCancellation := '';
  FOrderAcceptCancellation := '';
  FOrderRequestCancellation := '';
end;

constructor TACBrOpenDeliveryResources.Create(AOwner: TACBrComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FAuthentication := CEndPointAuthentication;
  FMerchantUpdate := CEndPointMerchantUpdate;
  FMerchantStatus := CEndPointMerchantStatus;
  FEventPolling := CEndPointEventPolling;
  FEventAcknowledgment := CEndPointEventAcknowledgment;
  FOrderDetails := CEndPointOrderDetails;
  FOrderConfirm := CEndPointOrderConfirm;
  FOrderDispatch := CEndPointOrderDispatch;
  FOrderReadyForPickup := CEndPointOrderReadyForPickup;
  FOrderRequestCancellation := CEndPointOrderRequestCancellation;
  FOrderAcceptCancellation := CEndPointOrderAcceptCancellation;
  FOrderDenyCancellation := CEndPointOrderDenyCancellation;
end;

function TACBrOpenDeliveryResources.GetAuthentication(const AMerchantId: string): string;
begin
  Result := ReplaceResource(FAuthentication, '', AMerchantId);
end;

function TACBrOpenDeliveryResources.GetEventAcknowledgment(const AMerchantId: string): string;
begin
  Result := ReplaceResource(FEventAcknowledgment, '', AMerchantId);
end;

function TACBrOpenDeliveryResources.GetEventPolling(const AMerchantId: string): string;
begin
  Result := ReplaceResource(FEventPolling, '', AMerchantId);
end;

function TACBrOpenDeliveryResources.GetMerchantStatus(const AMerchantId: string): string;
begin
  Result := ReplaceResource(FMerchantStatus, '', AMerchantId);
end;

function TACBrOpenDeliveryResources.GetMerchantUpdate(const AMerchantId: string): string;
begin
  Result := ReplaceResource(FMerchantUpdate, '', AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderAcceptCancellation(const AOrderId, AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderAcceptCancellation, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderConfirm(const AOrderId,
  AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderConfirm, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderDenyCancellation(
  const AOrderId, AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderDenyCancellation, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderDetails(const AOrderId, AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderDetails, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderDispatch(const AOrderId,
  AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderDispatch, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderReadyForPickup(const AOrderId,
  AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderReadyForPickup, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.GetOrderRequestCancellation(
  const AOrderId, AMerchantId: string): string;
begin
  Result := ReplaceResource(FOrderRequestCancellation, AOrderId, AMerchantId);
end;

function TACBrOpenDeliveryResources.ReplaceResource(const AResource, AOrderId, AMerchantId: string): string;
begin
  Result := StringReplace(AResource, '{orderId}', AOrderId, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{merchantId}', AMerchantId, [rfReplaceAll, rfIgnoreCase]);
end;

{ TACBrOpenDeliveryCredential }

procedure TACBrOpenDeliveryCredential.Assign(ASource: TACBrOpenDeliveryCredential);
begin
  FClientId := ASource.ClientId;
  FClientSecret := ASource.ClientSecret;
end;

procedure TACBrOpenDeliveryCredential.Clear;
begin
  FClientId := '';
  FClientSecret := '';
end;

constructor TACBrOpenDeliveryCredential.Create(AOwner: TACBrComponent);
begin
  inherited Create;
  FOwner := AOwner;
  Clear;
end;

procedure TACBrOpenDeliveryCredential.Validar;
begin
  if EstaVazio(FClientId) then
    GerarException(FOwner,
      'ClientId não informado. Informe na propriedade MarketPlace.Credenciais...');

  if EstaVazio(FClientSecret) then
    GerarException(FOwner,
      'ClientSecret não informado. Informe na propriedade MarketPlace.Credenciais...');
end;

end.
