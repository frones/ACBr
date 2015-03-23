object DANFSeDM: TDANFSeDM
  OldCreateOrder = False
  Left = 352
  Top = 108
  Height = 314
  Width = 464
  object RvSystem1: TRvSystem
    TitleSetup = 'Configura'#231#227'o do Relat'#243'rio'
    TitleStatus = 'Report Status'
    TitlePreview = 'Report Preview'
    SystemOptions = [soAllowPrintFromPreview, soAllowSaveFromPreview, soPreviewModal]
    SystemFiler.StatusFormat = 'Generating page %p'
    SystemPreview.FormState = wsMaximized
    SystemPreview.MarginPercent = 2.500000000000000000
    SystemPreview.ShadowDepth = 5
    SystemPreview.ZoomFactor = 100.000000000000000000
    SystemPrinter.ScaleX = 100.000000000000000000
    SystemPrinter.ScaleY = 100.000000000000000000
    SystemPrinter.StatusFormat = 'Printing page %p'
    SystemPrinter.Title = 'ReportPrinter Report'
    SystemPrinter.UnitsFactor = 1.000000000000000000
    Left = 139
    Top = 16
  end
  object RvProject: TRvProject
    Engine = RvSystem1
    ProjectFile = 'C:\oficina\NFSE\DANFENFSE.rav'
    Left = 40
    Top = 16
  end
  object RvRenderPDF1: TRvRenderPDF
    Active = False
    DisplayName = 'Adobe Acrobat (PDF)'
    FileExtension = '*.pdf'
    EmbedFonts = False
    ImageQuality = 90
    MetafileDPI = 300
    FontEncoding = feWinAnsiEncoding
    DocInfo.Creator = 'Rave (http://www.nevrona.com/rave)'
    DocInfo.Producer = 'Nevrona Designs'
    BufferDocument = True
    DisableHyperlinks = False
    Left = 224
    Top = 16
  end
  object DadosNFSE: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = DadosNFSEGetCols
    OnGetRow = DadosNFSEGetRow
    OnOpen = DadosNFSEOpen
    Left = 136
    Top = 80
  end
  object DadosPrestador: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = DadosPrestadorGetCols
    OnGetRow = DadosPrestadorGetRow
    OnOpen = DadosPrestadorOpen
    Left = 232
    Top = 80
  end
  object DadosServico: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = DadosServicoGetCols
    OnGetRow = DadosServicoGetRow
    OnOpen = DadosServicoOpen
    Left = 336
    Top = 80
  end
  object DadosTomador: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = DadosTomadorGetCols
    OnGetRow = DadosTomadorGetRow
    OnOpen = DadosTomadorOpen
    Left = 136
    Top = 144
  end
  object DadosPrefeitura: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = DadosPrefeituraGetCols
    OnGetRow = DadosPrefeituraGetRow
    OnOpen = DadosPrefeituraOpen
    Left = 232
    Top = 144
  end
  object NFSeCancelada: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = NFSeCanceladaGetCols
    OnGetRow = NFSeCanceladaGetRow
    OnOpen = NFSeCanceladaOpen
    Left = 336
    Top = 144
  end
end
