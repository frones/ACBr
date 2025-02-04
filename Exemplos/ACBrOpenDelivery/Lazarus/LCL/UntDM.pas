unit UntDM;

interface

uses
  SysUtils,
  Classes,
  DB,
  BufDataset;

type
  TDM = class(TDataModule)
    cdsPolling: TBufDataset;
    dtsPolling: TDataSource;
    cdsPollingeventID: TStringField;
    cdsPollingeventType: TStringField;
    cdsPollingorderID: TStringField;
    cdsPollingorderURL: TStringField;
    cdsPollingcreatedAt: TDateTimeField;
    cdsPollingsourceAppID: TStringField;
    cdsOrder: TBufDataset;
    dtsOrder: TDataSource;
    cdsOrderuniqueID: TStringField;
    cdsOrderID: TStringField;
    cdsOrdertype: TStringField;
    cdsOrderdisplayID: TStringField;
    cdsOrdercreatedAt: TDateTimeField;
    cdsOrderorderTiming: TStringField;
    cdsOrdermerchantID: TStringField;
    cdsOrdermerchantName: TStringField;
    cdsCustomer: TBufDataset;
    dtsCustomer: TDataSource;
    cdsCustomerID: TStringField;
    cdsCustomerphoneNumber: TStringField;
    cdsCustomerphoneExtention: TStringField;
    cdsCustomerdocumentNumber: TStringField;
    cdsCustomername: TStringField;
    cdsItems: TBufDataset;
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
    cdsOptions: TBufDataset;
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
    cdsPayments: TBufDataset;
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
      procedure ResetClientDataSet(AClientDataSet: TBufDataset);
  end;

var
  DM: TDM;

implementation


{$R *.lfm}

{ TDM }

procedure TDM.ResetClientDataSet(AClientDataSet: TBufDataset);
begin
  if not AClientDataSet.Active then
    AClientDataSet.CreateDataSet;
  if not (AClientDataSet.Active) then
    AClientDataSet.Active := True;
  if not (AClientDataSet.IsEmpty) then
    AClientDataSet.Close;
end;

end.
