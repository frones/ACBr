object DM: TDM
  OldCreateOrder = True
  Height = 480
  Width = 640
  object cdsPolling: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 56
    Top = 24
    object cdsPollingeventID: TStringField
      FieldName = 'eventID'
      Size = 50
    end
    object cdsPollingeventType: TStringField
      FieldName = 'eventType'
      Size = 50
    end
    object cdsPollingorderID: TStringField
      FieldName = 'orderID'
      Size = 50
    end
    object cdsPollingorderURL: TStringField
      FieldName = 'orderURL'
      Size = 255
    end
    object cdsPollingcreatedAt: TDateTimeField
      FieldName = 'createdAt'
    end
    object cdsPollingsourceAppID: TStringField
      FieldName = 'sourceAppID'
      Size = 50
    end
  end
  object dtsPolling: TDataSource
    DataSet = cdsPolling
    Left = 56
    Top = 80
  end
  object cdsOrder: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 152
    Top = 24
    object cdsOrderuniqueID: TStringField
      FieldName = 'uniqueID'
      Size = 50
    end
    object cdsOrderID: TStringField
      FieldName = 'ID'
      Size = 50
    end
    object cdsOrdertype: TStringField
      FieldName = 'type'
      Size = 50
    end
    object cdsOrderdisplayID: TStringField
      FieldName = 'displayID'
      Size = 50
    end
    object cdsOrdercreatedAt: TDateTimeField
      FieldName = 'createdAt'
    end
    object cdsOrderorderTiming: TStringField
      FieldName = 'orderTiming'
      Size = 50
    end
    object cdsOrdermerchantID: TStringField
      DisplayWidth = 100
      FieldName = 'merchantID'
      Size = 50
    end
    object cdsOrdermerchantName: TStringField
      FieldName = 'merchantName'
      Size = 50
    end
  end
  object dtsOrder: TDataSource
    DataSet = cdsOrder
    Left = 152
    Top = 80
  end
  object cdsCustomer: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 240
    Top = 24
    object cdsCustomerID: TStringField
      FieldName = 'ID'
      Size = 100
    end
    object cdsCustomerphoneNumber: TStringField
      FieldName = 'phoneNumber'
      Size = 50
    end
    object cdsCustomerphoneExtention: TStringField
      FieldName = 'phoneExtention'
      Size = 10
    end
    object cdsCustomerdocumentNumber: TStringField
      FieldName = 'documentNumber'
      Size = 50
    end
    object cdsCustomername: TStringField
      FieldName = 'name'
      Size = 255
    end
  end
  object dtsCustomer: TDataSource
    DataSet = cdsCustomer
    Left = 240
    Top = 80
  end
  object cdsItems: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 56
    Top = 192
    object cdsItemsID: TStringField
      FieldName = 'ID'
      Size = 100
    end
    object cdsItemsIndex: TIntegerField
      FieldName = 'Index'
    end
    object cdsItemsName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object cdsItemsexternalCode: TStringField
      FieldName = 'externalCode'
      Size = 30
    end
    object cdsItemsUnit: TStringField
      FieldName = 'Unit'
      Size = 30
    end
    object cdsItemsspecialInstructions: TStringField
      FieldName = 'specialInstructions'
      Size = 50
    end
    object cdsItemsunitPriceValue: TFloatField
      FieldName = 'unitPriceValue'
    end
    object cdsItemsoptionsPrice: TFloatField
      FieldName = 'optionsPrice'
    end
    object cdsItemstotalPriceValue: TFloatField
      FieldName = 'totalPriceValue'
    end
    object cdsItemstotalPriceCurrency: TStringField
      FieldName = 'totalPriceCurrency'
      Size = 30
    end
    object cdsItemsunitPriceCurrency: TStringField
      FieldName = 'unitPriceCurrency'
      Size = 30
    end
    object cdsItemsQuantity: TFloatField
      FieldName = 'Quantity'
    end
  end
  object dtsItems: TDataSource
    DataSet = cdsItems
    Left = 56
    Top = 248
  end
  object cdsOptions: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 144
    Top = 192
    object cdsOptionsID: TStringField
      FieldName = 'ID'
      Size = 10
    end
    object cdsOptionsName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object cdsOptionsexternalCode: TStringField
      FieldName = 'externalCode'
      Size = 30
    end
    object cdsOptionsUnit: TStringField
      FieldName = 'Unit'
      Size = 300
    end
    object cdsOptionsQuantity: TFloatField
      FieldName = 'Quantity'
    end
    object cdsOptionsunitPriceValue: TFloatField
      FieldName = 'unitPriceValue'
    end
    object cdsOptionsunitPriceCurrency: TStringField
      FieldName = 'unitPriceCurrency'
      Size = 30
    end
    object cdsOptionstotalPriceValue: TFloatField
      FieldName = 'totalPriceValue'
    end
    object cdsOptionstotalPriceCurrency: TStringField
      FieldName = 'totalPriceCurrency'
      Size = 30
    end
    object cdsOptionsspecialInstructions: TStringField
      FieldName = 'specialInstructions'
      Size = 255
    end
  end
  object dtsOptions: TDataSource
    DataSet = cdsOptions
    Left = 144
    Top = 248
  end
  object cdsPayments: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 232
    Top = 192
    object cdsPaymentsprepaid: TFloatField
      FieldName = 'prepaid'
    end
    object cdsPaymentspending: TFloatField
      FieldName = 'pending'
    end
    object cdsPaymentsmethodsValue: TFloatField
      FieldName = 'methodsValue'
    end
    object cdsPaymentsmethodsCurrency: TStringField
      FieldName = 'methodsCurrency'
      Size = 30
    end
    object cdsPaymentsmethodsMethod: TStringField
      FieldName = 'methodsMethod'
      Size = 30
    end
    object cdsPaymentsmethodsType: TStringField
      FieldName = 'methodsType'
      Size = 30
    end
    object cdsPaymentsmethodsChangeFor: TFloatField
      FieldName = 'methodsChangeFor'
    end
    object cdsPaymentsmethodsMethodInfo: TStringField
      FieldName = 'methodsMethodInfo'
      Size = 30
    end
    object cdsPaymentsmethodsChangeValue: TCurrencyField
      FieldName = 'methodsChangeValue'
    end
  end
  object dtsPayments: TDataSource
    DataSet = cdsPayments
    Left = 232
    Top = 248
  end
end
