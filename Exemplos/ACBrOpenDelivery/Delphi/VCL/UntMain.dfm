object FMain: TFMain
  Left = 547
  Top = 279
  Width = 1080
  Height = 808
  Caption = 'ACBrOpenDelivery - Demo VCL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1064
    Height = 90
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 90
      Height = 90
      Align = alLeft
      Stretch = True
    end
    object Image2: TImage
      Left = 974
      Top = 0
      Width = 90
      Height = 90
      Align = alRight
      Stretch = True
    end
    object Label1: TLabel
      Left = 90
      Top = 0
      Width = 884
      Height = 90
      Align = alClient
      Caption = ' DEMO VCL'
      Color = clHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -40
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
  end
  object pnlGeral: TPanel
    Left = 0
    Top = 90
    Width = 1064
    Height = 679
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object tabGeral: TPageControl
      Left = 0
      Top = 0
      Width = 1064
      Height = 679
      ActivePage = tbiMerchant
      Align = alClient
      TabOrder = 0
      object tbiPolling: TTabSheet
        Caption = 'Polling'
        ImageIndex = 1
        object Label4: TLabel
          Left = 3
          Top = 40
          Width = 70
          Height = 15
          Caption = 'Merchant ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 3
          Top = 88
          Width = 48
          Height = 15
          Caption = 'Event ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 3
          Top = 144
          Width = 49
          Height = 15
          Caption = 'Order ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 1056
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label9: TLabel
            Left = 0
            Top = 0
            Width = 321
            Height = 15
            Cursor = crHandPoint
            Align = alClient
            Caption = 'https://abrasel-nacional.github.io/docs/#tag/ordersPolling'
            DragCursor = crHandPoint
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            OnClick = Label9Click
          end
        end
        object edtPollingMerchantId: TEdit
          Left = 3
          Top = 61
          Width = 278
          Height = 23
          TabOrder = 1
        end
        object btnPollingAddMerchantId: TButton
          Left = 298
          Top = 60
          Width = 130
          Height = 25
          Caption = 'Add Merchant Id'
          TabOrder = 2
          OnClick = btnPollingAddMerchantIdClick
        end
        object btnPolling: TButton
          Left = 482
          Top = 60
          Width = 130
          Height = 25
          Caption = 'Polling'
          TabOrder = 3
          OnClick = btnPollingClick
        end
        object edtPollingAddEventId: TEdit
          Left = 3
          Top = 109
          Width = 278
          Height = 23
          TabOrder = 4
        end
        object btnPollingAck: TButton
          Left = 298
          Top = 108
          Width = 130
          Height = 25
          Caption = 'Acknowledgment'
          TabOrder = 5
          OnClick = btnPollingAckClick
        end
        object edtPollingOrderId: TEdit
          Left = 3
          Top = 165
          Width = 278
          Height = 23
          TabOrder = 6
        end
        object pgPolling: TPageControl
          Left = 0
          Top = 208
          Width = 1056
          Height = 441
          ActivePage = tbJSONPolling
          Align = alBottom
          TabOrder = 7
          object tabPolling: TTabSheet
            Caption = 'Polling'
            object DBGrid1: TDBGrid
              Left = 0
              Top = 0
              Width = 1048
              Height = 411
              Align = alClient
              DataSource = DM.dtsPolling
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -12
              TitleFont.Name = 'Segoe UI'
              TitleFont.Style = []
              Columns = <
                item
                  Expanded = False
                  FieldName = 'eventID'
                  Width = 180
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'eventType'
                  Width = 180
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'orderID'
                  Width = 180
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'orderURL'
                  Width = 180
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'createdAt'
                  Width = 100
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'sourceAppID'
                  Width = 180
                  Visible = True
                end>
            end
          end
          object tbJSONPolling: TTabSheet
            Caption = 'JSON'
            ImageIndex = 1
            object mmoPolling: TMemo
              Left = 0
              Top = 0
              Width = 1048
              Height = 411
              Align = alClient
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
      end
      object tbiOrder: TTabSheet
        Caption = 'Order (Pedidos)'
        ImageIndex = 2
        object Label7: TLabel
          Left = 8
          Top = 40
          Width = 49
          Height = 15
          Caption = 'Order ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 8
          Top = 88
          Width = 40
          Height = 15
          Caption = 'Reason'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label21: TLabel
          Left = 8
          Top = 144
          Width = 189
          Height = 15
          Caption = 'Merchant ID (ID Estabelecimento)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label22: TLabel
          Left = 328
          Top = 144
          Width = 230
          Height = 15
          Caption = 'Merchant Name (Nome Estabelecimento)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label23: TLabel
          Left = 11
          Top = 200
          Width = 13
          Height = 15
          Caption = 'ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label24: TLabel
          Left = 328
          Top = 200
          Width = 107
          Height = 15
          Caption = 'Type (Tipo Entrega)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label25: TLabel
          Left = 456
          Top = 200
          Width = 50
          Height = 15
          Caption = 'displayID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label26: TLabel
          Left = 584
          Top = 200
          Width = 142
          Height = 15
          Caption = 'createdAt (Dt. do Pedido)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label27: TLabel
          Left = 792
          Top = 200
          Width = 163
          Height = 15
          Caption = 'orderTiming (Agendado para)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 1056
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label10: TLabel
            Left = 0
            Top = 0
            Width = 319
            Height = 15
            Cursor = crHandPoint
            Align = alClient
            Caption = 'https://abrasel-nacional.github.io/docs/#tag/ordersStatus'
            DragCursor = crHandPoint
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            OnClick = Label10Click
          end
        end
        object edtOrderOrderId: TEdit
          Left = 8
          Top = 64
          Width = 300
          Height = 23
          TabOrder = 1
        end
        object btnOrderGetDetails: TButton
          Left = 328
          Top = 64
          Width = 122
          Height = 25
          Caption = 'Get Order Details'
          TabOrder = 2
          OnClick = btnOrderGetDetailsClick
        end
        object btnOrderConfirm: TButton
          Left = 456
          Top = 64
          Width = 130
          Height = 25
          Caption = 'Confirm'
          TabOrder = 3
          OnClick = btnOrderConfirmClick
        end
        object btnOrderDispatch: TButton
          Left = 592
          Top = 64
          Width = 130
          Height = 25
          Caption = 'Dispatch'
          TabOrder = 4
          OnClick = btnOrderDispatchClick
        end
        object btnOrderReadyForPickup: TButton
          Left = 728
          Top = 64
          Width = 130
          Height = 25
          Caption = 'Ready For Pickup'
          TabOrder = 5
          OnClick = btnOrderReadyForPickupClick
        end
        object edtOrderReason: TEdit
          Left = 8
          Top = 112
          Width = 249
          Height = 23
          TabOrder = 6
          Text = 'Texto livre indicando motivo da opera'#231#227'o'
        end
        object btnOrderRequestCancellation: TButton
          Left = 456
          Top = 112
          Width = 130
          Height = 25
          Caption = 'Request Cancellation'
          TabOrder = 7
          OnClick = btnOrderRequestCancellationClick
        end
        object btnOrderAcceptCancellation: TButton
          Left = 592
          Top = 112
          Width = 130
          Height = 25
          Caption = 'Accept Cancellation'
          TabOrder = 8
          OnClick = btnOrderAcceptCancellationClick
        end
        object btnOrderDenyCancellation: TButton
          Left = 728
          Top = 112
          Width = 130
          Height = 25
          Caption = 'Deny Cancellation'
          TabOrder = 9
          OnClick = btnOrderDenyCancellationClick
        end
        object pgOrder: TPageControl
          Left = 0
          Top = 384
          Width = 1056
          Height = 265
          ActivePage = tbPayments
          Align = alBottom
          TabOrder = 10
          object tbItems: TTabSheet
            Caption = 'Items (Itens do Pedido)'
            object DBGrid2: TDBGrid
              Left = 0
              Top = 0
              Width = 1048
              Height = 235
              Align = alClient
              DataSource = DM.dtsItems
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -12
              TitleFont.Name = 'Segoe UI'
              TitleFont.Style = []
              Columns = <
                item
                  Expanded = False
                  FieldName = 'ID'
                  Width = 150
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Index'
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Name'
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'externalCode'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Unit'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'specialInstructions'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'unitPriceValue'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'optionsPrice'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'totalPriceValue'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'totalPriceCurrency'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'unitPriceCurrency'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Quantity'
                  Visible = True
                end>
            end
          end
          object tbOptions: TTabSheet
            Caption = 'Options (Op'#231#245'es do Pedido)'
            ImageIndex = 1
            object DBGrid3: TDBGrid
              Left = 0
              Top = 0
              Width = 1048
              Height = 235
              Align = alClient
              DataSource = DM.dtsOptions
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -12
              TitleFont.Name = 'Segoe UI'
              TitleFont.Style = []
              Columns = <
                item
                  Expanded = False
                  FieldName = 'ID'
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Name'
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'externalCode'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Unit'
                  Width = 150
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'Quantity'
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'unitPriceValue'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'unitPriceCurrency'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'totalPriceValue'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'totalPriceCurrency'
                  Width = 64
                  Visible = True
                end
                item
                  Expanded = False
                  FieldName = 'specialInstructions'
                  Width = 200
                  Visible = True
                end>
            end
          end
          object tbPayments: TTabSheet
            Caption = 'Payments (Op'#231#245'es de Pagamento)'
            ImageIndex = 2
            object Label35: TLabel
              Left = 8
              Top = 8
              Width = 48
              Height = 15
              Caption = 'pending:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object lblPaymentsPending: TLabel
              Left = 88
              Top = 8
              Width = 109
              Height = 15
              Caption = 'lblPaymentsPending'
            end
            object DBGrid4: TDBGrid
              Left = 0
              Top = 32
              Width = 1048
              Height = 203
              Align = alBottom
              DataSource = DM.dtsPayments
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -12
              TitleFont.Name = 'Segoe UI'
              TitleFont.Style = []
            end
          end
          object TabSheet4: TTabSheet
            Caption = 'Customer Address (Endere'#231'o do Cliente)'
            ImageIndex = 3
            object Label28: TLabel
              Left = 8
              Top = 8
              Width = 37
              Height = 15
              Caption = 'street:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label29: TLabel
              Left = 8
              Top = 27
              Width = 47
              Height = 15
              Caption = 'number:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label30: TLabel
              Left = 8
              Top = 46
              Width = 23
              Height = 15
              Caption = 'city:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label31: TLabel
              Left = 8
              Top = 65
              Width = 55
              Height = 15
              Caption = 'postCode:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label32: TLabel
              Left = 8
              Top = 84
              Width = 42
              Height = 15
              Caption = 'district:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label33: TLabel
              Left = 8
              Top = 103
              Width = 31
              Height = 15
              Caption = 'state:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label34: TLabel
              Left = 8
              Top = 122
              Width = 74
              Height = 15
              Caption = 'complement:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object lblDeliveryStreet: TLabel
              Left = 128
              Top = 8
              Width = 85
              Height = 15
              Caption = 'lblDeliveryStreet'
            end
            object lblDeliveryNumber: TLabel
              Left = 128
              Top = 27
              Width = 99
              Height = 15
              Caption = 'lblDeliveryNumber'
            end
            object lblDeliveryCity: TLabel
              Left = 128
              Top = 46
              Width = 76
              Height = 15
              Caption = 'lblDeliveryCity'
            end
            object lblDeliveryPostalCode: TLabel
              Left = 128
              Top = 65
              Width = 115
              Height = 15
              Caption = 'lblDeliveryPostalCode'
            end
            object lblDeliveryDistrict: TLabel
              Left = 128
              Top = 84
              Width = 92
              Height = 15
              Caption = 'lblDeliveryDistrict'
            end
            object lblDeliveryState: TLabel
              Left = 128
              Top = 103
              Width = 81
              Height = 15
              Caption = 'lblDeliveryState'
            end
            object lblDeliveryComplement: TLabel
              Left = 128
              Top = 122
              Width = 125
              Height = 15
              Caption = 'lblDeliveryComplement'
            end
          end
          object tbJSONOrder: TTabSheet
            Caption = 'JSON'
            ImageIndex = 4
            object mmoOrder: TMemo
              Left = 0
              Top = 0
              Width = 1048
              Height = 235
              Align = alClient
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
        object pnlCustomer: TPanel
          Left = 3
          Top = 256
          Width = 447
          Height = 120
          BevelOuter = bvNone
          TabOrder = 11
          object Shape1: TShape
            Left = 0
            Top = 0
            Width = 447
            Height = 120
            Align = alClient
            Brush.Color = 15329769
            Pen.Color = clGray
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object Label13: TLabel
            Left = 9
            Top = 8
            Width = 424
            Height = 15
            Alignment = taCenter
            AutoSize = False
            Caption = 'Customer (Cliente)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label14: TLabel
            Left = 9
            Top = 50
            Width = 31
            Height = 15
            Caption = 'name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label15: TLabel
            Left = 9
            Top = 69
            Width = 57
            Height = 15
            Caption = 'document'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label16: TLabel
            Left = 9
            Top = 88
            Width = 35
            Height = 15
            Caption = 'phone'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblCusomerName: TDBText
            Left = 72
            Top = 50
            Width = 340
            Height = 17
            DataField = 'name'
            DataSource = DM.dtsCustomer
          end
          object lblCusomerDocument: TDBText
            Left = 72
            Top = 71
            Width = 340
            Height = 17
            DataField = 'documentNumber'
            DataSource = DM.dtsCustomer
          end
          object lblCusomerPhone: TDBText
            Left = 72
            Top = 92
            Width = 340
            Height = 17
            DataField = 'phoneNumber'
            DataSource = DM.dtsCustomer
          end
        end
        object Panel3: TPanel
          Left = 457
          Top = 256
          Width = 140
          Height = 120
          BevelOuter = bvNone
          TabOrder = 12
          object Shape2: TShape
            Left = 0
            Top = 0
            Width = 140
            Height = 120
            Align = alClient
            Brush.Color = 15329769
            Pen.Color = clGray
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object Label17: TLabel
            Left = 8
            Top = 8
            Width = 120
            Height = 39
            Alignment = taCenter
            AutoSize = False
            Caption = 'itemsPrice'#13#10'(Soma dos Itens)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblItemsPriceValue: TDBText
            Left = 8
            Top = 50
            Width = 120
            Height = 17
            Alignment = taCenter
          end
          object lblItemsPriceCurr: TDBText
            Left = 8
            Top = 71
            Width = 120
            Height = 17
            Alignment = taCenter
          end
        end
        object Panel4: TPanel
          Left = 603
          Top = 256
          Width = 140
          Height = 120
          BevelOuter = bvNone
          TabOrder = 13
          object Shape3: TShape
            Left = 0
            Top = 0
            Width = 140
            Height = 120
            Align = alClient
            Brush.Color = 15329769
            Pen.Color = clGray
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object Label18: TLabel
            Left = 8
            Top = 8
            Width = 120
            Height = 39
            Alignment = taCenter
            AutoSize = False
            Caption = 'otherPrice'#13#10'(Outras Taxas)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblothersFeesValue: TDBText
            Left = 8
            Top = 50
            Width = 120
            Height = 17
            Alignment = taCenter
          end
          object lblothersFeesCurr: TDBText
            Left = 8
            Top = 71
            Width = 120
            Height = 17
            Alignment = taCenter
          end
        end
        object Panel5: TPanel
          Left = 755
          Top = 256
          Width = 140
          Height = 120
          BevelOuter = bvNone
          TabOrder = 14
          object Shape4: TShape
            Left = 0
            Top = 0
            Width = 140
            Height = 120
            Align = alClient
            Brush.Color = 15329769
            Pen.Color = clGray
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object Label19: TLabel
            Left = 8
            Top = 8
            Width = 120
            Height = 39
            Alignment = taCenter
            AutoSize = False
            Caption = 'Discount'#13#10'(Descontos)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblDiscountValue: TDBText
            Left = 8
            Top = 50
            Width = 120
            Height = 17
            Alignment = taCenter
          end
          object lblDiscountCurr: TDBText
            Left = 8
            Top = 71
            Width = 120
            Height = 17
            Alignment = taCenter
          end
        end
        object Panel6: TPanel
          Left = 907
          Top = 256
          Width = 140
          Height = 120
          BevelOuter = bvNone
          TabOrder = 15
          object Shape5: TShape
            Left = 0
            Top = 0
            Width = 140
            Height = 120
            Align = alClient
            Brush.Color = 15329769
            Pen.Color = clGray
            Pen.Style = psClear
            Shape = stRoundRect
          end
          object Label20: TLabel
            Left = 8
            Top = 8
            Width = 120
            Height = 39
            Alignment = taCenter
            AutoSize = False
            Caption = 'oderAmount'#13#10'(Valor Total)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblorderAmountValue: TDBText
            Left = 8
            Top = 50
            Width = 120
            Height = 17
            Alignment = taCenter
          end
          object lblorderAmountCurr: TDBText
            Left = 8
            Top = 71
            Width = 120
            Height = 17
            Alignment = taCenter
          end
        end
        object edtMerchantIDOrder: TDBEdit
          Left = 8
          Top = 168
          Width = 300
          Height = 23
          DataField = 'merchantID'
          DataSource = DM.dtsOrder
          TabOrder = 16
        end
        object edtMerchantNameOrder: TDBEdit
          Left = 328
          Top = 168
          Width = 258
          Height = 23
          DataField = 'merchantName'
          DataSource = DM.dtsOrder
          TabOrder = 17
        end
        object edtID: TDBEdit
          Left = 8
          Top = 224
          Width = 300
          Height = 23
          DataField = 'ID'
          DataSource = DM.dtsOrder
          TabOrder = 18
        end
        object edtType: TDBEdit
          Left = 328
          Top = 224
          Width = 121
          Height = 23
          DataField = 'type'
          DataSource = DM.dtsOrder
          TabOrder = 19
        end
        object edtDisplayId: TDBEdit
          Left = 456
          Top = 224
          Width = 121
          Height = 23
          DataField = 'displayID'
          DataSource = DM.dtsOrder
          TabOrder = 20
        end
        object edtCreateAt: TDBEdit
          Left = 584
          Top = 224
          Width = 202
          Height = 23
          DataField = 'createdAt'
          DataSource = DM.dtsOrder
          TabOrder = 21
        end
        object edtOrderTiming: TDBEdit
          Left = 792
          Top = 224
          Width = 121
          Height = 23
          DataField = 'orderTiming'
          DataSource = DM.dtsOrder
          TabOrder = 22
        end
        object btnOrderDelivered: TButton
          Left = 872
          Top = 64
          Width = 130
          Height = 25
          Caption = 'Delivered'
          TabOrder = 23
          OnClick = btnOrderDeliveredClick
        end
      end
      object tbiMerchant: TTabSheet
        Caption = 'Merchant (Estabelecimento)'
        ImageIndex = 3
        object Label12: TLabel
          Left = 24
          Top = 80
          Width = 64
          Height = 15
          Caption = 'Merchant Id'
        end
        object pnlLink: TPanel
          Left = 0
          Top = 0
          Width = 1056
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label11: TLabel
            Left = 0
            Top = 0
            Width = 1056
            Height = 41
            Cursor = crHandPoint
            Align = alClient
            Caption = 
              'https://abrasel-nacional.github.io/docs/#tag/merchantUpdate/oper' +
              'ation/menuUpdated'
            DragCursor = crHandPoint
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
            OnClick = Label11Click
          end
        end
        object edtMerchantUpdateId: TEdit
          Left = 24
          Top = 101
          Width = 249
          Height = 23
          TabOrder = 1
          Text = 'X2V8QrBKZy'
        end
        object chkMerchantStatus: TCheckBox
          Left = 304
          Top = 96
          Width = 89
          Height = 17
          Caption = 'Available'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object btnMerchantUpdate: TButton
          Left = 432
          Top = 92
          Width = 289
          Height = 25
          Caption = 'Merchant Update (Atualizar Estabelecimento)'
          TabOrder = 3
          OnClick = btnMerchantUpdateClick
        end
        object rgUpdateType: TRadioGroup
          Left = 16
          Top = 136
          Width = 249
          Height = 161
          Caption = 'Update Type'
          ItemIndex = 0
          Items.Strings = (
            'Empty Body'
            'Only Status'
            'Entity Type'
            'Status and Entity Type')
          TabOrder = 4
        end
        object rgUpdateEntity: TRadioGroup
          Left = 16
          Top = 303
          Width = 249
          Height = 201
          Caption = 'Update Entity'
          ItemIndex = 0
          Items.Strings = (
            'Service'
            'Menu'
            'Category'
            'Item'
            'Item Offer'
            'Option Group'
            'Availability')
          TabOrder = 5
        end
      end
      object tbiLog: TTabSheet
        Caption = 'Log'
        ImageIndex = 4
        object mmoLogRequest: TMemo
          Left = 0
          Top = 0
          Width = 1056
          Height = 649
          Align = alClient
          PopupMenu = pmLog
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tbiConfiguracoes: TTabSheet
        Caption = 'Configura'#231#245'es'
        object Label2: TLabel
          Left = 16
          Top = 16
          Width = 52
          Height = 15
          Caption = 'Base URL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label48: TLabel
          Left = 16
          Top = 72
          Width = 48
          Height = 15
          Caption = 'Client ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 335
          Top = 72
          Width = 72
          Height = 15
          Caption = 'Client Secret'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtBaseUrl: TEdit
          Left = 16
          Top = 37
          Width = 406
          Height = 23
          TabOrder = 0
          Text = 'https://sandbox.myhubdelivery.io'
        end
        object edtClientId: TEdit
          Left = 16
          Top = 93
          Width = 313
          Height = 23
          TabOrder = 1
        end
        object edtClientSecret: TEdit
          Left = 335
          Top = 93
          Width = 313
          Height = 23
          PasswordChar = '*'
          TabOrder = 2
        end
      end
    end
  end
  object pmLog: TPopupMenu
    Left = 728
    Top = 72
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
  end
  object ACBrOpenDelivery1: TACBrOpenDelivery
    MarketPlace.Name = mpHubDelivery
    MarketPlace.Description = 'Hub Delivery'
    MarketPlace.BaseUrl = 'https://sandbox.myhubdelivery.io'
    MarketPlace.Resources.Authentication = 'license-manager/api/v1/oauth/token'
    MarketPlace.Resources.MerchantUpdate = 'merchants/api/v1/{merchantId}/merchantUpdate'
    MarketPlace.Resources.MerchantStatus = 'merchants/api/v1/{merchantId}/merchantStatus'
    MarketPlace.Resources.EventPolling = 'orders/api/v1/events:polling'
    MarketPlace.Resources.EventAcknowledgment = 'orders/api/v1/events/acknowledgment'
    MarketPlace.Resources.OrderDetails = 'orders/api/v1/{orderId}'
    MarketPlace.Resources.OrderConfirm = 'orders/api/v1/{orderId}/confirm'
    MarketPlace.Resources.OrderReadyForPickup = 'orders/api/v1/{orderId}/readyForPickup'
    MarketPlace.Resources.OrderDispatch = 'orders/api/v1/{orderId}/dispatch'
    MarketPlace.Resources.OrderDelivered = 'orders/api/v1/{orderId}/delivered'
    MarketPlace.Resources.OrderRequestCancellation = 'orders/api/v1/{orderId}/requestCancellation'
    MarketPlace.Resources.OrderAcceptCancellation = 'orders/api/v1/{orderId}/acceptCancellation'
    MarketPlace.Resources.OrderDenyCancellation = 'orders/api/v1/{orderId}/denyCancellation'
    TimeOut = 90000
    OnHTTPEnviar = ACBrOpenDelivery1HTTPEnviar
    OnHTTPRetornar = ACBrOpenDelivery1HTTPRetornar
    OnHTTPError = ACBrOpenDelivery1HTTPError
    Left = 728
    Top = 16
  end
end
