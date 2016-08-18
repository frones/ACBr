object dmACBrGNREFR: TdmACBrGNREFR
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 260
  Width = 689
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
  object cdsGuia: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 12
  end
  object frxGuia: TfrxDBDataset
    UserName = 'Guia'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsGuia
    BCDToCurrency = False
    Left = 172
    Top = 12
  end
  object frxBarCodeObject: TfrxBarCodeObject
    Left = 48
    Top = 148
  end
  object frxReport: TfrxReport
    Version = '5.3.16'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.AllowEdit = False
    PreviewOptions.Buttons = [pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 40401.475989294000000000
    ReportOptions.LastChange = 41075.662367303240000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    Left = 48
    Top = 36
  end
end
