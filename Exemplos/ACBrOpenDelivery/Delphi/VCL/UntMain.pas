unit UntMain;

interface

uses
  ACBrBase,
  ACBrOpenDelivery,
  ACBrOpenDeliveryHTTP,
  ACBrOpenDeliverySchemaClasses,
  pcnConversaoOD,
  Classes,
  SysUtils,
  Variants,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Menus,
  StdCtrls,
  Messages,
  ShellAPI,
  Windows,
  Grids, DB, DBGrids, DBCtrls, Mask;

type
  TFMain = class(TForm)
    pnlTop: TPanel;
    pnlGeral: TPanel;
    tabGeral: TPageControl;
    tbiConfiguracoes: TTabSheet;
    tbiPolling: TTabSheet;
    tbiOrder: TTabSheet;
    tbiMerchant: TTabSheet;
    tbiLog: TTabSheet;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    pmLog: TPopupMenu;
    Label2: TLabel;
    edtBaseUrl: TEdit;
    Label48: TLabel;
    edtClientId: TEdit;
    Label3: TLabel;
    edtClientSecret: TEdit;
    Panel1: TPanel;
    Label9: TLabel;
    Label4: TLabel;
    edtPollingMerchantId: TEdit;
    btnPollingAddMerchantId: TButton;
    btnPolling: TButton;
    Label5: TLabel;
    edtPollingAddEventId: TEdit;
    btnPollingAck: TButton;
    Label6: TLabel;
    edtPollingOrderId: TEdit;
    Panel2: TPanel;
    Label10: TLabel;
    Label7: TLabel;
    edtOrderOrderId: TEdit;
    btnOrderGetDetails: TButton;
    btnOrderConfirm: TButton;
    btnOrderDispatch: TButton;
    btnOrderDelivered: TButton;
    btnOrderReadyForPickup: TButton;
    Label8: TLabel;
    edtOrderReason: TEdit;
    btnOrderRequestCancellation: TButton;
    btnOrderAcceptCancellation: TButton;
    btnOrderDenyCancellation: TButton;
    pnlLink: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    edtMerchantUpdateId: TEdit;
    chkMerchantStatus: TCheckBox;
    btnMerchantUpdate: TButton;
    rgUpdateType: TRadioGroup;
    rgUpdateEntity: TRadioGroup;
    mmoLogRequest: TMemo;
    Clear1: TMenuItem;
    ACBrOpenDelivery1: TACBrOpenDelivery;
    pgPolling: TPageControl;
    tabPolling: TTabSheet;
    tbJSONPolling: TTabSheet;
    mmoPolling: TMemo;
    DBGrid1: TDBGrid;
    pgOrder: TPageControl;
    tbItems: TTabSheet;
    tbOptions: TTabSheet;
    tbPayments: TTabSheet;
    TabSheet4: TTabSheet;
    tbJSONOrder: TTabSheet;
    mmoOrder: TMemo;
    pnlCustomer: TPanel;
    Shape1: TShape;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lblCusomerName: TDBText;
    lblCusomerDocument: TDBText;
    lblCusomerPhone: TDBText;
    Panel3: TPanel;
    Shape2: TShape;
    Label17: TLabel;
    lblItemsPriceValue: TDBText;
    lblItemsPriceCurr: TDBText;
    Panel4: TPanel;
    Shape3: TShape;
    Label18: TLabel;
    lblothersFeesValue: TDBText;
    lblothersFeesCurr: TDBText;
    Panel5: TPanel;
    Shape4: TShape;
    Label19: TLabel;
    lblDiscountValue: TDBText;
    lblDiscountCurr: TDBText;
    Panel6: TPanel;
    Shape5: TShape;
    Label20: TLabel;
    lblorderAmountValue: TDBText;
    lblorderAmountCurr: TDBText;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    edtMerchantIDOrder: TDBEdit;
    edtMerchantNameOrder: TDBEdit;
    edtID: TDBEdit;
    edtType: TDBEdit;
    edtDisplayId: TDBEdit;
    edtCreateAt: TDBEdit;
    edtOrderTiming: TDBEdit;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    lblDeliveryStreet: TLabel;
    lblDeliveryNumber: TLabel;
    lblDeliveryCity: TLabel;
    lblDeliveryPostalCode: TLabel;
    lblDeliveryDistrict: TLabel;
    lblDeliveryState: TLabel;
    lblDeliveryComplement: TLabel;
    Label35: TLabel;
    lblPaymentsPending: TLabel;
    procedure btnMerchantUpdateClick(Sender: TObject);
    procedure btnOrderAcceptCancellationClick(Sender: TObject);
    procedure btnOrderConfirmClick(Sender: TObject);
    procedure btnOrderDenyCancellationClick(Sender: TObject);
    procedure btnOrderDispatchClick(Sender: TObject);
    procedure btnOrderDeliveredClick(Sender: TObject);
    procedure btnOrderGetDetailsClick(Sender: TObject);
    procedure btnOrderReadyForPickupClick(Sender: TObject);
    procedure btnOrderRequestCancellationClick(Sender: TObject);
    procedure btnPollingAckClick(Sender: TObject);
    procedure btnPollingAddMerchantIdClick(Sender: TObject);
    procedure btnPollingClick(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure ACBrOpenDelivery1HTTPEnviar(
      ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio);
    procedure ACBrOpenDelivery1HTTPRetornar(
      ALogResposta: TACBrOpenDeliveryHTTPLogResposta);
    procedure ACBrOpenDelivery1HTTPError(
      ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio;
      ALogResposta: TACBrOpenDeliveryHTTPLogResposta;
      AErro: EACBrOpenDeliveryHTTPException; var ATratado: Boolean);
    private
      { Private declarations }
    public
      { Public declarations }
      procedure OpenLink(ALabel: TLabel);
      procedure ConfigurarComponente;
  end;

var
  FMain: TFMain;

implementation

uses
  UntDM;

{$R *.dfm}


procedure TFMain.btnMerchantUpdateClick(Sender: TObject);
var
  LUpdateType: TACBrODMerchantUpdateType;
  LUpdateEntity: TACBrODMerchantUpdateEntity;
  LStatus: TACBrODStatus;
begin
  ConfigurarComponente;
  LUpdateType := TACBrODMerchantUpdateType(rgUpdateType.ItemIndex);
  LUpdateEntity := TACBrODMerchantUpdateEntity(rgUpdateEntity.ItemIndex);
  LStatus := sAvailable;
  if not chkMerchantStatus.Checked then
    LStatus := sUnavailable;

  ACBrOpenDelivery1.WebServices.MerchantUpdate.UpdateType := LUpdateType;
  ACBrOpenDelivery1.WebServices.MerchantUpdate.EntityType := LUpdateEntity;
  ACBrOpenDelivery1.WebServices.MerchantUpdate.Merchant.Id := edtMerchantUpdateId.Text;
  ACBrOpenDelivery1.WebServices.MerchantUpdate.Merchant.Status := LStatus;
  ACBrOpenDelivery1.WebServices.MerchantUpdate.Executar;

  ShowMessage('Atualizado!');
end;

procedure TFMain.btnOrderAcceptCancellationClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderAcceptCancellation.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderAcceptCancellation.Executar;
end;

procedure TFMain.btnOrderConfirmClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderConfirm.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderConfirm.Reason := 'Campo livre para mais informacoes sobre a confirmacao do pedido';
  ACBrOpenDelivery1.WebServices.OrderConfirm.OrderExternalCode := '';
  ACBrOpenDelivery1.WebServices.OrderConfirm.CreatedAt := Now;

  ACBrOpenDelivery1.WebServices.OrderConfirm.Executar;
end;

procedure TFMain.btnOrderDeliveredClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderDelivered.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderDelivered.Executar;
end;

procedure TFMain.btnOrderDenyCancellationClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderDenyCancellation.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderDenyCancellation.Reason := edtOrderReason.Text;
  ACBrOpenDelivery1.WebServices.OrderDenyCancellation.Code := dccOutForDelivery;
  ACBrOpenDelivery1.WebServices.OrderDenyCancellation.Executar;
end;

procedure TFMain.btnOrderDispatchClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderDispatch.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderDispatch.Executar;
end;

procedure TFMain.btnOrderGetDetailsClick(Sender: TObject);
var
  I: Integer;
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderDetails.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderDetails.Executar;

  DM.ResetClientDataSet(DM.cdsOrder);
  DM.ResetClientDataSet(DM.cdsCustomer);
  DM.ResetClientDataSet(DM.cdsItems);
  DM.ResetClientDataSet(DM.cdsOptions);
  DM.ResetClientDataSet(DM.cdsPayments);

  mmoOrder.Lines.Clear;
  mmoOrder.Lines.Add('DisplayId: ' + ACBrOpenDelivery1.Order.displayId);
  mmoOrder.Lines.Add('Customer Name: ' + ACBrOpenDelivery1.Order.customer.name);
  mmoOrder.Lines.Add('Total: ' + CurrToStr(ACBrOpenDelivery1.Order.total.orderAmount.value));

  DM.cdsOrder.Append;
  DM.cdsOrderID.AsString := ACBrOpenDelivery1.Order.merchant.id;
  DM.cdsOrdermerchantID.AsString := ACBrOpenDelivery1.Order.merchant.id;
  DM.cdsOrdermerchantName.AsString := ACBrOpenDelivery1.Order.merchant.name;
  DM.cdsOrdertype.AsString := ServiceTypeToStr(ACBrOpenDelivery1.Order._type);
  DM.cdsOrderdisplayID.AsString := ACBrOpenDelivery1.Order.displayId;
  DM.cdsOrdercreatedAt.AsDateTime := ACBrOpenDelivery1.Order.createdAt;
  DM.cdsOrderorderTiming.AsString := OrderTimingToStr(ACBrOpenDelivery1.Order.orderTiming);

  //Customer
  DM.cdsCustomer.Append;
  DM.cdsCustomerID.AsString := ACBrOpenDelivery1.Order.customer.id;
  DM.cdsCustomerphoneNumber.AsString := ACBrOpenDelivery1.Order.customer.phone.number;
  DM.cdsCustomerdocumentNumber.AsString := ACBrOpenDelivery1.Order.customer.documentNumber;
  DM.cdsCustomername.AsString := ACBrOpenDelivery1.Order.customer.name;
  DM.cdsCustomer.Post;

  DM.cdsOrder.Post;
  //Total
  lblItemsPriceValue.Caption := CurrToStr(ACBrOpenDelivery1.Order.total.itemsPrice.value);
  lblItemsPriceCurr.Caption := ACBrOpenDelivery1.Order.total.itemsPrice.currency;

  lblothersFeesValue.Caption := CurrToStr(ACBrOpenDelivery1.Order.total.otherFees.value);
  lblothersFeesCurr.Caption := ACBrOpenDelivery1.Order.total.otherFees.currency;

  lblDiscountValue.Caption := CurrToStr(ACBrOpenDelivery1.Order.total.discount.value);
  lblDiscountCurr.Caption := ACBrOpenDelivery1.Order.total.discount.currency;

  lblorderAmountValue.Caption := CurrToStr(ACBrOpenDelivery1.Order.total.orderAmount.value);
  lblorderAmountCurr.Caption := ACBrOpenDelivery1.Order.total.orderAmount.currency;

  //Delivery
  lblDeliveryStreet.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.street;
  lblDeliveryNumber.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.number;
  lblDeliveryCity.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.city;
  lblDeliveryPostalCode.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.postalCode;
  lblDeliveryDistrict.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.district;
  lblDeliveryState.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.state;
  lblDeliveryComplement.Caption := ACBrOpenDelivery1.Order.delivery.deliveryAddress.complement;

  //Fill Itens
  for I := 0 to Pred(ACBrOpenDelivery1.Order.items.Count) do
  begin
    DM.cdsItems.Append;

    DM.cdsItemsID.AsString := ACBrOpenDelivery1.Order.items[I].id;
    DM.cdsItemsIndex.AsInteger := ACBrOpenDelivery1.Order.items[I].index;
    DM.cdsItemsName.AsString := ACBrOpenDelivery1.Order.items[I].name;
    DM.cdsItemsexternalCode.AsString := ACBrOpenDelivery1.Order.items[I].externalCode;
    DM.cdsItemsUnit.AsString := UnitToStr(ACBrOpenDelivery1.Order.items[I]._unit);
    DM.cdsItemsQuantity.AsFloat := ACBrOpenDelivery1.Order.items[I].quantity;
    DM.cdsItemsspecialInstructions.AsString := ACBrOpenDelivery1.Order.items[I].specialInstructions;
    DM.cdsItemsunitPriceValue.AsCurrency := ACBrOpenDelivery1.Order.items[I].unitPrice.value;
    DM.cdsItemsunitPriceCurrency.AsString := ACBrOpenDelivery1.Order.items[I].unitPrice.currency;
    DM.cdsItemsoptionsPrice.AsCurrency := ACBrOpenDelivery1.Order.items[I].optionsPrice.value;
    DM.cdsItemstotalPriceValue.AsCurrency := ACBrOpenDelivery1.Order.items[I].totalPrice.value;
    DM.cdsItemstotalPriceCurrency.AsString := ACBrOpenDelivery1.Order.items[I].totalPrice.currency;

    DM.cdsItems.Post;
  end;

  //Fill Options
  for I := 0 to Pred(ACBrOpenDelivery1.Order.items[0].options.Count) do
  begin
    DM.cdsOptions.Append;

    DM.cdsOptionsID.AsString := ACBrOpenDelivery1.Order.items[0].options[I].id;
    DM.cdsOptionsName.AsString := ACBrOpenDelivery1.Order.items[0].options[I].name;
    DM.cdsOptionsexternalCode.AsString := ACBrOpenDelivery1.Order.items[0].options[I].externalCode;
    DM.cdsOptionsUnit.AsString := ACBrOpenDelivery1.Order.items[0].options[I]._unit;
    DM.cdsOptionsQuantity.AsFloat := ACBrOpenDelivery1.Order.items[0].options[I].quantity;
    DM.cdsOptionsunitPriceValue.AsCurrency := ACBrOpenDelivery1.Order.items[0].options[I].unitPrice.value;
    DM.cdsOptionsunitPriceCurrency.AsString := ACBrOpenDelivery1.Order.items[0].options[I].unitPrice.currency;
    DM.cdsOptionstotalPriceValue.AsCurrency := ACBrOpenDelivery1.Order.items[0].options[I].totalPrice.value;
    DM.cdsOptionstotalPriceCurrency.AsString := ACBrOpenDelivery1.Order.items[0].options[I].totalPrice.currency;
    DM.cdsOptionsspecialInstructions.AsString := ACBrOpenDelivery1.Order.items[0].options[I].specialInstructions;

    DM.cdsOptions.Post;
  end;

  //Paymentsd
  lblPaymentsPending.Caption := CurrToStr(ACBrOpenDelivery1.Order.payments.prepaid);
  lblPaymentsPending.Caption := CurrToStr(ACBrOpenDelivery1.Order.payments.pending);
  for I := 0 to Pred(ACBrOpenDelivery1.Order.payments.methods.Count) do
  begin
    DM.cdsPayments.Append;
    DM.cdsPaymentsprepaid.AsCurrency := ACBrOpenDelivery1.Order.payments.prepaid;
    DM.cdsPaymentspending.AsCurrency := ACBrOpenDelivery1.Order.payments.pending;
    DM.cdsPaymentsmethodsValue.AsCurrency := ACBrOpenDelivery1.Order.payments.methods[I].value;
    DM.cdsPaymentsmethodsCurrency.AsString := ACBrOpenDelivery1.Order.payments.methods[I].currency;
    DM.cdsPaymentsmethodsMethod.AsString := PaymentMethodToStr(ACBrOpenDelivery1.Order.payments.methods[I].method);
    DM.cdsPaymentsmethodsMethodInfo.AsString := ACBrOpenDelivery1.Order.payments.methods[I].methodInfo;
    DM.cdsPaymentsmethodsType.AsString := PaymentTypeToStr(ACBrOpenDelivery1.Order.payments.methods[I]._type);
    DM.cdsPaymentsmethodsChangeFor.AsCurrency := ACBrOpenDelivery1.Order.payments.methods[I].changeFor;
    DM.cdsPaymentsmethodsChangeValue.AsCurrency := ACBrOpenDelivery1.Order.payments.methods[I].changeValue;

    DM.cdsPayments.Post;
  end;

end;

procedure TFMain.btnOrderReadyForPickupClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderReadyForPickup.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderReadyForPickup.Executar;
end;

procedure TFMain.btnOrderRequestCancellationClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderRequestCancellation.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderRequestCancellation.Reason := edtOrderReason.Text;
  ACBrOpenDelivery1.WebServices.OrderRequestCancellation.Code := crcUnavailableItem;
  ACBrOpenDelivery1.WebServices.OrderRequestCancellation.Mode := crmAuto;
  ACBrOpenDelivery1.WebServices.OrderRequestCancellation.Executar;
end;

procedure TFMain.btnPollingAckClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.Acknowledgment.Events.New;
  ACBrOpenDelivery1.WebServices.Acknowledgment.Events[0].Id := edtPollingAddEventId.Text;
  ACBrOpenDelivery1.WebServices.Acknowledgment.Events[0].OrderId := edtPollingOrderId.Text;

  ACBrOpenDelivery1.WebServices.Acknowledgment.Executar;
  ShowMessage(IntToStr(ACBrOpenDelivery1.WebServices.Acknowledgment.StatusCode));
end;

procedure TFMain.btnPollingAddMerchantIdClick(Sender: TObject);
begin
  ACBrOpenDelivery1.WebServices.Polling
    .AddMerchantId(edtPollingMerchantId.Text);
end;

procedure TFMain.btnPollingClick(Sender: TObject);
var
  I: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    ConfigurarComponente;
    DM.ResetClientDataSet(DM.cdsPolling);
    ACBrOpenDelivery1.WebServices.Polling.Executar;

    if ACBrOpenDelivery1.Events.Count > 0 then
    begin
      edtPollingAddEventId.Text := ACBrOpenDelivery1.Events[0].EventId;
      edtPollingOrderId.Text := ACBrOpenDelivery1.Events[0].OrderId;
      edtOrderOrderId.Text := ACBrOpenDelivery1.Events[0].OrderId;

      mmoPolling.Lines.Text := ACBrOpenDelivery1.Events.AsJSON;

      for I := 0 to Pred(ACBrOpenDelivery1.Events.Count) do
      begin
        DM.cdsPolling.Append;
        DM.cdsPollingeventID.AsString := ACBrOpenDelivery1.Events[I].EventId;
        DM.cdsPollingeventType.AsString := EventTypeToStr(ACBrOpenDelivery1.Events[I].EventType);
        DM.cdsPollingorderID.AsString := ACBrOpenDelivery1.Events[I].OrderId;
        DM.cdsPollingorderURL.AsString := ACBrOpenDelivery1.Events[I].OrderURL;
        DM.cdsPollingcreatedAt.AsDateTime := ACBrOpenDelivery1.Events[I].CreatedAt;
        DM.cdsPollingsourceAppID.AsString := ACBrOpenDelivery1.Events[I].SourceAppId;
        DM.cdsPolling.Post;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFMain.Clear1Click(Sender: TObject);
begin
  mmoLogRequest.Lines.Clear;
end;

{ TFMain }

procedure TFMain.ConfigurarComponente;
begin
  if (edtClientId.Text = '') or (edtClientSecret.Text = '') then
  begin
    tabGeral.ActivePage := tbiConfiguracoes;
    raise Exception.Create('Informe as credenciais de ClientId e ClientSecret...');
  end
  else
  begin
    ACBrOpenDelivery1.MarketPlace.BaseUrl := edtBaseUrl.Text;
    ACBrOpenDelivery1.MarketPlace.Credenciais.ClientId := edtClientId.Text;
    ACBrOpenDelivery1.MarketPlace.Credenciais.ClientSecret := edtClientSecret.Text;
  end;
end;

procedure TFMain.Label10Click(Sender: TObject);
begin
  OpenLink(Sender as TLabel);
end;

procedure TFMain.Label11Click(Sender: TObject);
begin
  OpenLink(Sender as TLabel);
end;

procedure TFMain.Label9Click(Sender: TObject);
begin
  OpenLink(Sender as TLabel);
end;

procedure TFMain.OpenLink(ALabel: TLabel);
begin
//  ShellExecute(Handle, 'open', PAnsiChar(ALabel.Caption), nil, nil, SW_SHOWMAXIMIZED);
end;

procedure TFMain.ACBrOpenDelivery1HTTPEnviar(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio);
begin
  mmoLogRequest.Lines.Add('------ INICIO REQUEST -------');
  mmoLogRequest.Lines.Add('Id: ' + ALogEnvio.Id);
  mmoLogRequest.Lines.Add('Start: ' + FormatDateTime('hh:mm:ss', ALogEnvio.Data));
  mmoLogRequest.Lines.Add('Url: ' + ALogEnvio.URL);
  mmoLogRequest.Lines.Add('Method: ' + ALogEnvio.Method);
  mmoLogRequest.Lines.Add('Headers: ' + ALogEnvio.Headers.Text);
  mmoLogRequest.Lines.Add('Body: ' + ALogEnvio.Body);
  mmoLogRequest.Lines.Add('------ FIM REQUEST -------');
end;

procedure TFMain.ACBrOpenDelivery1HTTPError(
  ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio;
  ALogResposta: TACBrOpenDeliveryHTTPLogResposta;
  AErro: EACBrOpenDeliveryHTTPException; var ATratado: Boolean);
begin
  ShowMessage(AErro.Content);
  // Setar essa variável para True caso queira que o componente não lance
  // a exceção e siga o fluxo normal
  ATratado := True;
end;

procedure TFMain.ACBrOpenDelivery1HTTPRetornar(ALogResposta: TACBrOpenDeliveryHTTPLogResposta);
begin
  mmoLogRequest.Lines.Add('------ INICIO RESPONSE -------');
  mmoLogRequest.Lines.Add('Id: ' + ALogResposta.Id);
  mmoLogRequest.Lines.Add('Start: ' + FormatDateTime('hh:mm:ss', ALogResposta.Data));
  mmoLogRequest.Lines.Add('Url: ' + ALogResposta.URL);
  mmoLogRequest.Lines.Add('Status: ' + IntToStr(ALogResposta.Status));
  mmoLogRequest.Lines.Add('Headers: ' + ALogResposta.Headers.Text);
  mmoLogRequest.Lines.Add('Body: ' + ALogResposta.Body);
  mmoLogRequest.Lines.Add('------ FIM RESPONSE -------');
end;

end.
