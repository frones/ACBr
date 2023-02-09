unit UMain;

interface

uses
  ACBrBase,
  ACBrOpenDelivery,
  ACBrOpenDeliveryHTTP,
  ACBrOpenDeliverySchemaClasses,
  IniFiles,
  pcnConversaoOD,

  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Forms,
  FMX.Graphics,
  FMX.Layouts,
  FMX.Memo,
  FMX.Memo.Types,
  FMX.Menus,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.StdCtrls,
  FMX.TabControl,
  FMX.Types,

  REST.JSON,

  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,

  Winapi.ShellAPI,
  Winapi.Windows, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TFMain = class(TForm)
    lytTop: TLayout;
    recBackground: TRectangle;
    lytBackground: TLayout;
    Image1: TImage;
    Image2: TImage;
    tabGeral: TTabControl;
    tbiConfiguracoes: TTabItem;
    tbiPolling: TTabItem;
    tbiOrder: TTabItem;
    tbiMerchant: TTabItem;
    tbiLog: TTabItem;
    ACBrOpenDelivery1: TACBrOpenDelivery;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtBaseUrl: TEdit;
    edtClientId: TEdit;
    edtClientSecret: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtPollingMerchantId: TEdit;
    edtPollingAddEventId: TEdit;
    edtPollingOrderId: TEdit;
    btnPollingAddMerchantId: TButton;
    btnPolling: TButton;
    btnPollingAck: TButton;
    mmoPolling: TMemo;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtOrderOrderId: TEdit;
    edtOrderReason: TEdit;
    btnOrderGetDetails: TButton;
    btnOrderConfirm: TButton;
    btnOrderDispatch: TButton;
    btnOrderDelivered: TButton;
    btnOrderReadyForPickup: TButton;
    btnOrderRequestCancellation: TButton;
    btnOrderAcceptCancellation: TButton;
    btnOrderDenyCancellation: TButton;
    Label12: TLabel;
    Label13: TLabel;
    edtMerchantUpdateId: TEdit;
    rgUpdateType: TGroupBox;
    rgUpdateEntity: TGroupBox;
    chkMerchantStatus: TCheckBox;
    btnMerchantUpdate: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    mmoLogRequest: TMemo;
    mmoLogResponse: TMemo;
    pmLog: TPopupMenu;
    MenuItem1: TMenuItem;
    edtMerchantIDOrder: TEdit;
    edtMerchantNameOrder: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    gridItens: TStringGrid;
    edtID: TEdit;
    edtType: TEdit;
    edtCreateAt: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    gridPolling: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    edtDisplayId: TEdit;
    Label17: TLabel;
    edtOrderTiming: TEdit;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    StringColumn16: TStringColumn;
    gridOptions: TStringGrid;
    StringColumn17: TStringColumn;
    StringColumn19: TStringColumn;
    StringColumn20: TStringColumn;
    StringColumn21: TStringColumn;
    StringColumn22: TStringColumn;
    StringColumn23: TStringColumn;
    StringColumn24: TStringColumn;
    StringColumn18: TStringColumn;
    StringColumn25: TStringColumn;
    StringColumn26: TStringColumn;
    StringColumn27: TStringColumn;
    StringColumn28: TStringColumn;
    Label23: TLabel;
    lytItemsPrice: TLayout;
    Rectangle1: TRectangle;
    lblItemsPriceCurr: TLabel;
    lblItemsPriceValue: TLabel;
    lytothersFees: TLayout;
    Rectangle2: TRectangle;
    Label24: TLabel;
    lblothersFeesCurr: TLabel;
    lblothersFeesValue: TLabel;
    lytDiscount: TLayout;
    Rectangle3: TRectangle;
    Label25: TLabel;
    lblDiscountCurr: TLabel;
    lblDiscountValue: TLabel;
    lytOtherAmount: TLayout;
    Rectangle4: TRectangle;
    Label26: TLabel;
    lblorderAmountCurr: TLabel;
    lblorderAmountValue: TLabel;
    tabOrderDetails: TTabControl;
    tbiItems: TTabItem;
    tbiOptions: TTabItem;
    tbiPayments: TTabItem;
    Layout1: TLayout;
    Rectangle5: TRectangle;
    Label16: TLabel;
    lblCusomerName: TLabel;
    lblTitCusomerName: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    Label22: TLabel;
    lblCusomerPhone: TLabel;
    Layout4: TLayout;
    Label28: TLabel;
    lblCusomerDocument: TLabel;
    tbiCustomerAddress: TTabItem;
    Label27: TLabel;
    lblDeliveryStreet: TLabel;
    Label30: TLabel;
    lblDeliveryNumber: TLabel;
    Label32: TLabel;
    lblDeliveryCity: TLabel;
    Label34: TLabel;
    lblDeliveryPostalCode: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    lblDeliveryDistrict: TLabel;
    lblDeliveryState: TLabel;
    Label40: TLabel;
    lblDeliveryComplement: TLabel;
    Label29: TLabel;
    lblPaymentsPending: TLabel;
    gridPayments: TStringGrid;
    StringColumn29: TStringColumn;
    StringColumn30: TStringColumn;
    StringColumn31: TStringColumn;
    StringColumn32: TStringColumn;
    StringColumn33: TStringColumn;
    StringColumn34: TStringColumn;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    mmoOrder: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ACBrOpenDelivery1HTTPEnviar(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio);
    procedure ACBrOpenDelivery1HTTPRetornar(ALogResposta:
        TACBrOpenDeliveryHTTPLogResposta);
    procedure ACBrOpenDelivery1TokenGet(AClientId: string; var AToken: string; var
        AExpiresAt: TDateTime);
    procedure ACBrOpenDelivery1TokenSave(AClientId, AToken: string; AExpiresAt:
        TDateTime);
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
    procedure gridPollingCellClick(const Column: TColumn; const Row: Integer);
    procedure MenuItem1Click(Sender: TObject);
    procedure ACBrOpenDelivery1EventOrderPlaced(
      AEvent: TACBrOpenDeliverySchemaEvent;
      AOrder: TACBrOpenDeliverySchemaOrder; var Ack: Boolean);
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

{$R *.fmx}


procedure TFMain.FormCreate(Sender: TObject);
begin
  tabGeral.ActiveTab := tbiConfiguracoes;
end;

procedure TFMain.ACBrOpenDelivery1EventOrderPlaced(
  AEvent: TACBrOpenDeliverySchemaEvent;
  AOrder: TACBrOpenDeliverySchemaOrder; var Ack: Boolean);
begin
  // Para Integrar e Confirmar automaticamente
//  try
//    ShowMessage(AOrder.customer.name);
//    ACBrOpenDelivery1.WebServices.OrderConfirm.OrderId := AOrder.id;
//    ACBrOpenDelivery1.WebServices.OrderConfirm.Reason := 'Aceito com sucesso.';
//    if ACBrOpenDelivery1.WebServices.OrderConfirm.Executar then
//      Ack := True;
//  except
//    Ack := False;
//  end;
end;

procedure TFMain.ACBrOpenDelivery1HTTPEnviar(ALogEnvio:
  TACBrOpenDeliveryHTTPLogEnvio);
begin
  mmoLogRequest.Lines.Add('Id: ' + ALogEnvio.Id);
  mmoLogRequest.Lines.Add('Start: ' + FormatDateTime('hh:mm:ss', ALogEnvio.Data));
  mmoLogRequest.Lines.Add('Url: ' + ALogEnvio.URL);
  mmoLogRequest.Lines.Add('Method: ' + ALogEnvio.Method);
  mmoLogRequest.Lines.Add('Headers: ' + ALogEnvio.Headers.Text);
  mmoLogRequest.Lines.Add('Body: ' + ALogEnvio.Body);
end;

procedure TFMain.ACBrOpenDelivery1HTTPRetornar(ALogResposta:
  TACBrOpenDeliveryHTTPLogResposta);
begin
  mmoLogResponse.Lines.Add('Id: ' + ALogResposta.Id);
  mmoLogResponse.Lines.Add('Start: ' + FormatDateTime('hh:mm:ss', ALogResposta.Data));
  mmoLogResponse.Lines.Add('Url: ' + ALogResposta.URL);
  mmoLogResponse.Lines.Add('Status: ' + IntToStr(ALogResposta.Status));
  mmoLogResponse.Lines.Add('Headers: ' + ALogResposta.Headers.Text);
  mmoLogResponse.Lines.Add('Body: ' + ALogResposta.Body);
end;

procedure TFMain.ACBrOpenDelivery1TokenGet(AClientId: string; var AToken:
  string; var AExpiresAt: TDateTime);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'OpenDeliveryDemo.ini');
  try
    AToken := LIniFile.ReadString(AClientId, 'TOKEN', '');
    AExpiresAt := StrToDateTimeDef(LIniFile.ReadString(AClientId, 'EXPIRES_AT', ''), 0);
  finally
    LIniFile.Free;
  end;
end;

procedure TFMain.ACBrOpenDelivery1TokenSave(AClientId, AToken: string;
  AExpiresAt: TDateTime);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'OpenDeliveryDemo.ini');
  try
    LIniFile.WriteString(AClientId, 'TOKEN', AToken);
    LIniFile.WriteString(AClientId, 'EXPIRES_AT', DateTimeToStr(AExpiresAt));
  finally
    LIniFile.Free;
  end;
end;

procedure TFMain.btnMerchantUpdateClick(Sender: TObject);
var
  LUpdateType: TACBrODMerchantUpdateType;
  LUpdateEntity: TACBrODMerchantUpdateEntity;
  LStatus: TACBrODStatus;
begin
  ConfigurarComponente;
//ToDo: Compatibilizar aqui com o FMX
//  LUpdateType := TACBrODMerchantUpdateType(rgUpdateType.ItemIndex);
//  LUpdateEntity := TACBrODMerchantUpdateEntity(rgUpdateEntity.ItemIndex);
  LStatus := sAvailable;
  if not chkMerchantStatus.IsChecked then
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
  ACBrOpenDelivery1.WebServices.OrderConfirm.Reason := 'Campo livre para informacoes da confirmacao do pedido';
  ACBrOpenDelivery1.WebServices.OrderConfirm.OrderExternalCode := '';

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
    {$REGION 'Cabecalho Pedido'}
    (*
    {
        "uniqueId": "f0269d26-6872-4c2a-9e8a-f3d6f5e55aaa",
        "id": "f0269d26-6872-4c2a-9e8a-f3d6f5e55aaa",
        "type": "DELIVERY",
        "displayId": "0808221308",
        "createdAt": "2022-08-08T13:08:01.119423Z",
        "orderTiming": "INSTANT",
        "merchant": {
            "id": "14417282000131-7cf42549-aff0-4bfd-a33b-83f4c08ff441",
            "name": "GET IT Restaurante e Pizzaria"
        },
    *)
    {$ENDREGION}
    {$REGION 'Items'}
    (*
        "items": [
            {
                "id": "ea45a60e-14c3-4fb0-8713-093b02014579",
                "index": 0,
                "name": "X-Burger",
                "externalCode": "0101",
                "unit": "UNIT",
                "quantity": 0,
                "specialInstructions": "Remover mostarda",
                "unitPrice": {
                    "value": 0.0,
                    "currency": null
                },
                "optionsPrice": null,
                "totalPrice": {
                    "value": 10.0,
                    "currency": "BRL"
                },
                "options": [
                    {
                        "id": "02a3c52f-3fd4-456b-a2c6-157ae38df7b6",
                        "name": "Coca-Cola",
                        "externalCode": "COC",
                        "unit": "UNIT",
                        "quantity": 0,
                        "unitPrice": {
                            "value": 0.0,
                            "currency": null
                        },
                        "totalPrice": {
                            "value": 0.0,
                            "currency": "BRL"
                        },
                        "specialInstructions": ""
                    }
                ]
            }
        ],
    *)
    {$ENDREGION}
    {$REGION 'otherFees'}
    (*
        "otherFees": [
            {
                "name": "MARKETPLACE",
                "type": "SERVICE_FEE",
                "receivedBy": "MARKETPLACE",
                "receiverDocument": "95320052000151",
                "price": {
                    "value": 3.0,
                    "currency": "BRL"
                },
                "observation": "Order 0808221308 Fee"
            }
        ],
    *)
    {$ENDREGION'}
    {$REGION 'discounts'}
    (*
        "discounts": [],
    *)
    {$ENDREGION}
    {$REGION 'total'}
    (*
        "total": {
            "itemsPrice": {
                "value": 10.0,
                "currency": "BRL"
            },
            "otherFees": {
                "value": 3.0,
                "currency": "BRL"
            },
            "discount": {
                "value": 0.0,
                "currency": "BRL"
            },
            "orderAmount": {
                "value": 10.0,
                "currency": "BRL"
            }
        },
    *)
    {$ENDREGION}
    {$REGION 'payments'}
    (*
        "payments": {
            "prepaid": 10.0,
            "pending": 0.0,
            "methods": [
                {
                    "value": 10.0,
                    "currency": "BRL",
                    "method": "CREDIT",
                    "methodInfo": "VISA",
                    "type": "PREPAID",
                    "changeFor": 0.0
                }
            ]
        },
    *)
    {$ENDREGION}
    {$REGION 'customer'}
    (*
        "customer": {
            "id": "7cf42549-aff0-4bfd-a33b-83f4c08ff441",
            "phone": {
                "number": "11910588280",
                "extension": "0101"
            },
            "documentNumber": "28896497094",
            "name": "GET IT Restaurante e Pizzaria",
            "ordersCountOnMerchant": 8
        },
    *)
    {$ENDREGION}
    {$REGION 'delivery'}
    (*
        "delivery": {
            "deliveredBy": "MARKETPLACE",
            "deliveryAddress": {
                "country": "BR",
                "street": "Rua Cel. Irineu de Castro",
                "formattedAddress": null,
                "number": "43",
                "city": "S?o Paulo",
                "postalCode": "03333-050",
                "coordinates": {
                    "latitude": -23.5481,
                    "longitude": -23.5481
                },
                "district": "Vila Carrao",
                "state": "SP",
                "complement": "S?o Paulo",
                "deliveryDateTime": "0001-01-01T00:00:00"
            },
            "estimatedDeliveryDateTime": "2022-08-08T13:38:01.2791544Z"
        },
        "takeout": {
            "mode": null,
            "takeoutDateTime": null
        },
        "schedule": {
            "scheduleDateTime": "0001-01-01T00:00:00"
        },
        "indoor": {
            "mode": null,
            "indoorDeliveryDateTime": "0001-01-01T00:00:00",
            "table": null
        },
        "extraInfo": "Pedido Teste"
    }
    *)
    {$ENDREGION}

  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.OrderDetails.OrderId := edtOrderOrderId.Text;
  ACBrOpenDelivery1.WebServices.OrderDetails.Executar;

  mmoOrder.Lines.Clear;
  mmoOrder.Lines.Add(TJson.Format(TJSONObject.ParseJSONValue(ACBrOpenDelivery1.WebServices.OrderDetails.Order.AsJSON) as TJSONValue));
  with ACBrOpenDelivery1.WebServices.OrderDetails do
  begin
    mmoOrder.Lines.Add('DisplayId: ' + Order.displayId);
    mmoOrder.Lines.Add('Customer Name: ' + Order.customer.name);
    mmoOrder.Lines.Add('Total: ' + CurrToStr(Order.total.orderAmount.value));

    edtMerchantIDOrder.Text  := Order.merchant.id;
    edtMerchantNameOrder.Text := Order.merchant.name;
    edtMerchantUpdateId.Text := Order.merchant.id;
    edtID.Text := Order.id;

    edtType.Text := ServiceTypeToStr(Order._type);

    edtDisplayId.Text := Order.displayId;
    edtCreateAt.Text := FormatDateTime('YYYY-MM-DD hh:mm:ss', Order.createdAt);
    edtOrderTiming.Text := OrderTimingToStr(Order.orderTiming);

    //Total
    lblItemsPriceValue.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.itemsPrice.value.ToString;
    lblItemsPriceCurr.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.itemsPrice.currency;

    lblothersFeesValue.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.otherFees.value.ToString;
    lblothersFeesCurr.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.otherFees.currency;

    lblDiscountValue.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.discount.value.ToString;
    lblDiscountCurr.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.discount.currency;

    lblorderAmountValue.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.orderAmount.value.ToString;
    lblorderAmountCurr.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.total.orderAmount.currency;

    //Customer
    lblCusomerName.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.customer.name;
    lblCusomerDocument.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.customer.documentNumber;
    lblCusomerPhone.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.customer.phone.number;

    //Delivery
    lblDeliveryStreet.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.street;
    lblDeliveryNumber.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.number;
    lblDeliveryCity.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.city;
    lblDeliveryPostalCode.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.postalCode;
    lblDeliveryDistrict.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.district;
    lblDeliveryState.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.state;
    lblDeliveryComplement.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.delivery.deliveryAddress.complement;

    //Fill Itens
    for I := 0 to Pred(ACBrOpenDelivery1.WebServices.OrderDetails.Order.items.Count) do
    begin
      gridItens.Cells[0 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.items[I].id;
      gridItens.Cells[1 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].index.ToString;
      gridItens.Cells[2 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].name;
      gridItens.Cells[3 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].externalCode;
      gridItens.Cells[4 , I] := UnitToStr(ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I]._unit);
      gridItens.Cells[5 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].quantity.ToString;
      gridItens.Cells[6 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].specialInstructions;
      gridItens.Cells[7 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].unitPrice.value.ToString;
      gridItens.Cells[8 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].unitPrice.currency;
      gridItens.Cells[9 , I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].optionsPrice.value.ToString;
      gridItens.Cells[10, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].totalPrice.value.ToString;
      gridItens.Cells[11, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].totalPrice.currency;
    end;

    //Fill Options
    for I := 0 to Pred(ACBrOpenDelivery1.WebServices.OrderDetails.Order.items[0].options.Count) do
    begin
      gridOptions.Cells[0, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.items[I].options[I].id;
      gridOptions.Cells[1, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].name;
      gridOptions.Cells[2, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].externalCode;
      gridOptions.Cells[3, I] := UnitToStr(ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I]._unit);
      gridOptions.Cells[4, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].quantity.ToString;
      gridOptions.Cells[5, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].unitPrice.value.ToString;
      gridOptions.Cells[6, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].unitPrice.currency;
      gridOptions.Cells[7, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].totalPrice.value.ToString;
      gridOptions.Cells[8, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].totalPrice.currency;
      gridOptions.Cells[9, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.Items[I].specialInstructions;
    end;

    //Paymentsd
    lblPaymentsPending.Text := ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.pending.ToString;
    for I := 0 to Pred(ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods.Count) do
    begin
      gridPayments.Cells[0, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I].value.ToString;
      gridPayments.Cells[1, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I].currency;
      gridPayments.Cells[2, I] := PaymentMethodToStr(ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I].method);
      gridPayments.Cells[3, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I].methodInfo;
      gridPayments.Cells[4, I] := PaymentTypeToStr(ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I]._type);
      gridPayments.Cells[5, I] := ACBrOpenDelivery1.WebServices.OrderDetails.Order.payments.methods[I].changeFor.ToString;
    end;
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
  LStrEvent: string;
begin
  {$REGION '<JSON Exemplo Retorno EVENTS>'}
  (*
    [
        {
            "eventId": "db37e591-f9e9-4f8b-8503-e4a0a8409e67",
            "eventType": "CREATED",
            "orderId": "f0269d26-6872-4c2a-9e8a-f3d6f5e55aaa",
            "orderURL": "https:\/\/sandbox.myhubdelivery.io\/orders\/api\/v1\/f0269d26-6872-4c2a-9e8a-f3d6f5e55aaa",
            "createdAt": "2022-08-08T13:08:03.000Z",
            "sourceAppId": "0babcbcf2d5c4c69a9d1b6677cb4f593"
        }
    ]
  *)
  {$ENDREGION}

  ConfigurarComponente;
  ACBrOpenDelivery1.WebServices.Polling.Executar;

  edtPollingAddEventId.Text := ACBrOpenDelivery1.WebServices.Polling.Events[0].EventId;
  edtPollingOrderId.Text := ACBrOpenDelivery1.WebServices.Polling.Events[0].OrderId;
  edtOrderOrderId.Text := ACBrOpenDelivery1.WebServices.Polling.Events[0].OrderId;

  mmoPolling.Lines.Text := TJson.Format(TJSONObject.ParseJSONValue(ACBrOpenDelivery1.WebServices.Polling.Events.AsJSON) as TJSONValue);

  for I := 0 to Pred(ACBrOpenDelivery1.WebServices.Polling.Events.Count) do
  begin
    LStrEvent := EventTypeToStr(ACBrOpenDelivery1.WebServices.Polling.Events[I].EventType);
    gridPolling.Cells[0, I] := ACBrOpenDelivery1.WebServices.Polling.Events[I].EventId;
    gridPolling.Cells[1, I] := LStrEvent;
    gridPolling.Cells[2, I] := ACBrOpenDelivery1.WebServices.Polling.Events[I].OrderId;
    gridPolling.Cells[3, I] := ACBrOpenDelivery1.WebServices.Polling.Events[I].OrderURL;
    gridPolling.Cells[4, I] := FormatDateTime('YYYY-MM-DD hh:mm:ss', ACBrOpenDelivery1.WebServices.Polling.Events[I].CreatedAt);
    gridPolling.Cells[5, I] := ACBrOpenDelivery1.WebServices.Polling.Events[I].SourceAppId;
  end;
end;

procedure TFMain.ConfigurarComponente;
begin
  ACBrOpenDelivery1.MarketPlace.BaseUrl := edtBaseUrl.Text;
  ACBrOpenDelivery1.MarketPlace.Credenciais.ClientId := edtClientId.Text;
  ACBrOpenDelivery1.MarketPlace.Credenciais.ClientSecret := edtClientSecret.Text;
end;

procedure TFMain.gridPollingCellClick(const Column: TColumn; const Row:
  Integer);
begin
  edtOrderOrderId.Text := gridPolling.Cells[2, Row];
  edtPollingOrderId.Text := gridPolling.Cells[2, Row];
end;

procedure TFMain.MenuItem1Click(Sender: TObject);
begin
  mmoLogRequest.Lines.Clear;
  mmoLogResponse.Lines.Clear;
end;

procedure TFMain.OpenLink(ALabel: TLabel);
begin
  ShellExecute(0, 'open', PWideChar(ALabel.Text), nil, nil, SW_SHOWMAXIMIZED);
end;

end.
