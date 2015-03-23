object dmACBrCTeFR: TdmACBrCTeFR
  OldCreateOrder = False
  Height = 525
  Width = 848
  object frxPDFExport: TfrxPDFExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    PrintOptimized = True
    Outline = False
    Background = True
    HTMLTags = True
    Author = 'FastReport'
    Subject = 'Exportando DANFE para PDF'
    ProtectionFlags = [ePrint, eModify, eCopy, eAnnot]
    HideToolbar = False
    HideMenubar = False
    HideWindowUI = False
    FitWindow = False
    CenterWindow = False
    PrintScaling = False
    Left = 48
    Top = 92
  end
  object cdsIdentificacao: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 12
  end
  object cdsEmitente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 116
  end
  object cdsDestinatario: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 227
  end
  object cdsDadosNotasFiscais: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 404
  end
  object cdsParametros: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 297
    Top = 68
  end
  object cdsInformacoesAdicionais: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 204
  end
  object cdsVolumes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 12
  end
  object frxIdentificacao: TfrxDBDataset
    UserName = 'Identificacao'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsIdentificacao
    BCDToCurrency = False
    Left = 172
    Top = 12
  end
  object frxEmitente: TfrxDBDataset
    UserName = 'Emitente'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEmitente
    BCDToCurrency = False
    Left = 172
    Top = 116
  end
  object frxDestinatario: TfrxDBDataset
    UserName = 'Destinatario'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsDestinatario
    BCDToCurrency = False
    Left = 172
    Top = 227
  end
  object frxDadosNotasFiscais: TfrxDBDataset
    UserName = 'DadosNotasFiscais'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsDadosNotasFiscais
    BCDToCurrency = False
    Left = 172
    Top = 404
  end
  object frxParametros: TfrxDBDataset
    UserName = 'Parametros'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsParametros
    BCDToCurrency = False
    Left = 329
    Top = 68
  end
  object frxVolumes: TfrxDBDataset
    UserName = 'Volumes'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsVolumes
    BCDToCurrency = False
    Left = 328
    Top = 12
  end
  object frxInformacoesAdicionais: TfrxDBDataset
    UserName = 'InformacoesAdicionais'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsInformacoesAdicionais
    BCDToCurrency = False
    Left = 328
    Top = 204
  end
  object frxBarCodeObject: TfrxBarCodeObject
    Left = 48
    Top = 148
  end
  object frxReport: TfrxReport
    Tag = 1
    Version = '4.12.2'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.AllowEdit = False
    PreviewOptions.Buttons = [pbPrint, pbSave, pbZoom, pbFind, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 41606.360400451400000000
    ReportOptions.LastChange = 42002.608125000000000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnBeforePrint = frxReportBeforePrint
    OnReportPrint = 'frxReportOnReportPrint'
    Left = 48
    Top = 36
  end
  object cdsTomador: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 62
  end
  object frxTomador: TfrxDBDataset
    UserName = 'Tomador'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsTomador
    BCDToCurrency = False
    Left = 172
    Top = 62
  end
  object cdsExpedidor: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 284
  end
  object frxExpedidor: TfrxDBDataset
    UserName = 'Expedidor'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsExpedidor
    BCDToCurrency = False
    Left = 172
    Top = 284
  end
  object cdsRecebedor: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 348
  end
  object frxRecebedor: TfrxDBDataset
    UserName = 'Recebedor'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsRecebedor
    BCDToCurrency = False
    Left = 172
    Top = 348
  end
  object cdsRemetente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 171
  end
  object frxRemetente: TfrxDBDataset
    UserName = 'Remetente'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsRemetente
    BCDToCurrency = False
    Left = 172
    Top = 171
  end
  object cdsCalculoImposto: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 132
  end
  object frxCalculoImposto: TfrxDBDataset
    UserName = 'LocalEntrega'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsCalculoImposto
    BCDToCurrency = False
    Left = 328
    Top = 132
  end
  object cdsComponentesPrestacao: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 276
  end
  object frxComponentesPrestacao: TfrxDBDataset
    UserName = 'ComponentesPrestacao'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsComponentesPrestacao
    BCDToCurrency = False
    Left = 328
    Top = 276
  end
  object cdsSeguro: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 348
  end
  object frxSeguro: TfrxDBDataset
    UserName = 'Seguro'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsSeguro
    BCDToCurrency = False
    Left = 328
    Top = 348
  end
  object cdsModalRodoviario: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 420
  end
  object frxModalRodoviario: TfrxDBDataset
    UserName = 'ModalRodoviario'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsModalRodoviario
    BCDToCurrency = False
    Left = 328
    Top = 420
  end
  object cdsRodoVeiculos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 12
  end
  object frxRodoVeiculos: TfrxDBDataset
    UserName = 'Veiculos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsRodoVeiculos
    BCDToCurrency = False
    Left = 480
    Top = 12
  end
  object frxRodoValePedagio: TfrxDBDataset
    UserName = 'ValePedagio'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsRodoValePedagio
    BCDToCurrency = False
    Left = 480
    Top = 68
  end
  object cdsRodoValePedagio: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 68
  end
  object cdsRodoMotorista: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 124
  end
  object frxRodoMotorista: TfrxDBDataset
    UserName = 'Motorista'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsRodoMotorista
    BCDToCurrency = False
    Left = 480
    Top = 124
  end
  object frxDocAnterior: TfrxDBDataset
    UserName = 'DocAnterior'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsDocAnterior
    BCDToCurrency = False
    Left = 484
    Top = 188
  end
  object cdsDocAnterior: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 188
  end
  object cdsAnuladoComple: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 448
    Top = 272
  end
  object frxcdsAnuladoComple: TfrxDBDataset
    UserName = 'AnuladoComple'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsAnuladoComple
    BCDToCurrency = False
    Left = 484
    Top = 272
  end
  object cdsEventos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 444
    Top = 340
  end
  object frxEventos: TfrxDBDataset
    UserName = 'Eventos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEventos
    BCDToCurrency = False
    Left = 516
    Top = 336
  end
  object cdsModalAereo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 143
    Top = 458
  end
  object frxModalAereo: TfrxDBDataset
    UserName = 'ModalAereo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsModalAereo
    BCDToCurrency = False
    Left = 173
    Top = 459
  end
end
