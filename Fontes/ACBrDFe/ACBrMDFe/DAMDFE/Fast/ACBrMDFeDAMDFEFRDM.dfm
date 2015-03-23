object DMACBrMDFeDAMDFEFR: TDMACBrMDFeDAMDFEFR
  OldCreateOrder = False
  Height = 382
  Width = 555
  object frxReport: TfrxReport
    Version = '4.15.11'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.AllowEdit = False
    PreviewOptions.Buttons = [pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 41606.360400451400000000
    ReportOptions.LastChange = 41606.413323935190000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnGetValue = frxReportGetValue
    Left = 48
    Top = 36
  end
  object frxPDFExport: TfrxPDFExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    PrintOptimized = True
    Outline = False
    Background = True
    HTMLTags = True
    Quality = 95
    Author = 'FastReport'
    Subject = 'Exportando DANFE para PDF'
    ProtectionFlags = [ePrint, eModify, eCopy, eAnnot]
    HideToolbar = False
    HideMenubar = False
    HideWindowUI = False
    FitWindow = False
    CenterWindow = False
    PrintScaling = False
    CheckboxAsShape = False
    Left = 48
    Top = 92
  end
  object frxBarCodeObject: TfrxBarCodeObject
    Left = 48
    Top = 148
  end
  object cdsIdentificacao: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 135
    Top = 12
  end
  object frxIdentificacao: TfrxDBDataset
    UserName = 'Identificacao'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsIdentificacao
    BCDToCurrency = False
    Left = 212
    Top = 12
  end
  object cdsEmitente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 71
  end
  object frxEmitente: TfrxDBDataset
    UserName = 'Emitente'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEmitente
    BCDToCurrency = False
    Left = 212
    Top = 71
  end
  object cdsModalRodo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 300
    Top = 11
  end
  object frxModalRodo: TfrxDBDataset
    UserName = 'ModalRodo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsModalRodo
    BCDToCurrency = False
    Left = 377
    Top = 11
  end
  object frxModalAereo: TfrxDBDataset
    UserName = 'ModalAereo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalAereo
    BCDToCurrency = False
    Left = 217
    Top = 181
  end
  object CDSModalAereo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 181
  end
  object CDSModalAqua: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 236
  end
  object frxModalAqua: TfrxDBDataset
    UserName = 'ModalAqua'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalAqua
    BCDToCurrency = False
    Left = 222
    Top = 236
  end
  object frxModalFerrov: TfrxDBDataset
    UserName = 'ModalFerrov'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalFerrov
    BCDToCurrency = False
    Left = 396
    Top = 182
  end
  object CDSModalFerrov: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 310
    Top = 178
  end
  object cdsParametros: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 297
    Top = 68
  end
  object frxParametros: TfrxDBDataset
    UserName = 'Parametros'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsParametros
    BCDToCurrency = False
    Left = 369
    Top = 68
  end
  object CDSModalFerrovVagoes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 311
    Top = 234
  end
  object frxModalFerrovVagoes: TfrxDBDataset
    UserName = 'ModalFerrovVagoes'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalFerrovVagoes
    BCDToCurrency = False
    Left = 405
    Top = 230
  end
  object CDSDocumentos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 291
  end
  object frxDocumentos: TfrxDBDataset
    UserName = 'Documentos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSDocumentos
    BCDToCurrency = False
    Left = 222
    Top = 291
  end
  object cdsEventos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 316
    Top = 308
  end
  object frxEventos: TfrxDBDataset
    UserName = 'Eventos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEventos
    BCDToCurrency = False
    Left = 388
    Top = 304
  end
  object cdsMunCarrega: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 139
    Top = 128
  end
  object frxMunCarrega: TfrxDBDataset
    UserName = 'MunCarrega'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsMunCarrega
    BCDToCurrency = False
    Left = 216
    Top = 128
  end
  object cdsPercurso: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 299
    Top = 128
  end
  object frxPercurso: TfrxDBDataset
    UserName = 'Percurso'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsPercurso
    BCDToCurrency = False
    Left = 376
    Top = 128
  end
end
