object dmACBrBoletoFCFR: TdmACBrBoletoFCFR
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 341
  Width = 539
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
    Left = 48
    Top = 92
  end
  object cdsTitulo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 36
  end
  object frxTitulo: TfrxDBDataset
    UserName = 'Titulo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsTitulo
    BCDToCurrency = False
    Left = 192
    Top = 38
  end
  object frxBarCodeObject: TfrxBarCodeObject
    Left = 44
    Top = 200
  end
  object frxReport: TfrxReport
    Version = '5.2.3'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.AllowEdit = False
    PreviewOptions.Buttons = [pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 40401.475989294000000000
    ReportOptions.LastChange = 42257.626775173600000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnProgressStart = frxReportProgressStart
    Left = 50
    Top = 36
  end
  object frxHTMLExport: TfrxHTMLExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    FixedWidth = True
    Background = False
    Centered = False
    EmptyLines = True
    Print = False
    PictureType = gpPNG
    Left = 46
    Top = 148
  end
  object cdsCedente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 98
  end
  object frxCedente: TfrxDBDataset
    UserName = 'Cedente'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsCedente
    BCDToCurrency = False
    Left = 192
    Top = 98
  end
  object cdsBanco: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 160
  end
  object frxBanco: TfrxDBDataset
    UserName = 'Banco'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsBanco
    BCDToCurrency = False
    Left = 192
    Top = 160
  end
  object frxJPEGExport: TfrxJPEGExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    Left = 40
    Top = 248
  end
end
