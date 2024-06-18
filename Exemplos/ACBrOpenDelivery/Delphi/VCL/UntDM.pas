unit UntDM;

interface

uses
  SysUtils,
  Classes,
  DB,
  DBClient;

type
  TDM = class(TDataModule)
    cdsPolling: TClientDataSet;
    dtsPolling: TDataSource;
    cdsPollingeventID: TStringField;
    cdsPollingeventType: TStringField;
    cdsPollingorderID: TStringField;
    cdsPollingorderURL: TStringField;
    cdsPollingcreatedAt: TDateTimeField;
    cdsPollingsourceAppID: TStringField;
    cdsOrder: TClientDataSet;
    dtsOrder: TDataSource;
    cdsOrderuniqueID: TStringField;
    cdsOrderID: TStringField;
    cdsOrdertype: TStringField;
    cdsOrderdisplayID: TStringField;
    cdsOrdercreatedAt: TDateTimeField;
    cdsOrderorderTiming: TStringField;
    cdsOrdermerchantID: TStringField;
    cdsOrdermerchantName: TStringField;
    cdsCustomer: TClientDataSet;
    dtsCustomer: TDataSource;
    cdsCustomerID: TStringField;
    cdsCustomerphoneNumber: TStringField;
    cdsCustomerphoneExtention: TStringField;
    cdsCustomerdocumentNumber: TStringField;
    cdsCustomername: TStringField;
    cdsItems: TClientDataSet;
    dtsItems: TDataSource;
    cdsItemsID: TStringField;
    cdsItemsIndex: TIntegerField;
    cdsItemsName: TStringField;
    cdsItemsexternalCode: TStringField;
    cdsItemsUnit: TStringField;
    cdsItemsspecialInstructions: TStringField;
    cdsItemsunitPriceValue: TFloatField;
    cdsItemsoptionsPrice: TFloatField;
    cdsItemstotalPriceValue: TFloatField;
    cdsItemstotalPriceCurrency: TStringField;
    cdsItemsunitPriceCurrency: TStringField;
    cdsItemsQuantity: TFloatField;
    cdsOptions: TClientDataSet;
    dtsOptions: TDataSource;
    cdsOptionsID: TStringField;
    cdsOptionsName: TStringField;
    cdsOptionsexternalCode: TStringField;
    cdsOptionsUnit: TStringField;
    cdsOptionsQuantity: TFloatField;
    cdsOptionsunitPriceValue: TFloatField;
    cdsOptionsunitPriceCurrency: TStringField;
    cdsOptionstotalPriceValue: TFloatField;
    cdsOptionstotalPriceCurrency: TStringField;
    cdsOptionsspecialInstructions: TStringField;
    cdsPayments: TClientDataSet;
    dtsPayments: TDataSource;
    cdsPaymentsprepaid: TFloatField;
    cdsPaymentspending: TFloatField;
    cdsPaymentsmethodsValue: TFloatField;
    cdsPaymentsmethodsCurrency: TStringField;
    cdsPaymentsmethodsMethod: TStringField;
    cdsPaymentsmethodsType: TStringField;
    cdsPaymentsmethodsChangeFor: TFloatField;
    cdsPaymentsmethodsMethodInfo: TStringField;
    cdsPaymentsmethodsChangeValue: TCurrencyField;
    private
      { Private declarations }
    public
      { Public declarations }
      procedure ResetClientDataSet(AClientDataSet: TClientDataSet);
  end;

var
  DM: TDM;

implementation


{$R *.dfm}

{ TDM }

procedure TDM.ResetClientDataSet(AClientDataSet: TClientDataSet);
begin
  if not AClientDataSet.Active then
    AClientDataSet.CreateDataSet;
  if not (AClientDataSet.Active) then
    AClientDataSet.Active := True;
  if not (AClientDataSet.IsEmpty) then
    AClientDataSet.EmptyDataSet;
end;

end.
