object dmACBrNFe: TdmACBrNFe
  Left = 396
  Top = 136
  Width = 743
  Height = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CustomDestinatarioCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomDestinatarioCXNGetCols
    OnGetRow = CustomDestinatarioCXNGetRow
    OnOpen = CustomDestinatarioCXNOpen
    Left = 208
    Top = 176
  end
  object RvSystem1: TRvSystem
    TitleSetup = 'Configura'#231#227'o do Relat'#243'rio'
    TitleStatus = 'Report Status'
    TitlePreview = 'Report Preview'
    DefaultDest = rdPrinter
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
    OnBeforePrint = RvSystem1BeforePrint
    Left = 107
    Top = 80
  end
  object RvProject: TRvProject
    Engine = RvSystem1
    ProjectFile = '.\Report\NotaFiscalEletronica.rav'
    Left = 40
    Top = 80
  end
  object CustomEmitenteCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomEmitenteCXNGetCols
    OnGetRow = CustomEmitenteCXNGetRow
    OnOpen = CustomEmitenteCXNOpen
    Left = 207
    Top = 129
  end
  object CustomDadosProdutosCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomDadosProdutosCXNGetCols
    OnGetRow = CustomDadosProdutosCXNGetRow
    OnOpen = CustomDadosProdutosCXNOpen
    Left = 208
    Top = 224
  end
  object CustomTransportadorCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomTransportadorCXNGetCols
    OnGetRow = CustomTransportadorCXNGetRow
    OnOpen = CustomTransportadorCXNOpen
    Left = 374
    Top = 176
  end
  object CustomCalculoImpostoCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomCalculoImpostoCXNGetCols
    OnGetRow = CustomCalculoImpostoCXNGetRow
    OnOpen = CustomCalculoImpostoCXNOpen
    Left = 375
    Top = 128
  end
  object CustomParametrosCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomParametrosCXNGetCols
    OnGetRow = CustomParametrosCXNGetRow
    OnOpen = CustomParametrosCXNOpen
    Left = 206
    Top = 280
  end
  object CustomDuplicatasCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomDuplicatasCXNGetCols
    OnGetRow = CustomDuplicatasCXNGetRow
    OnOpen = CustomDuplicatasCXNOpen
    Left = 374
    Top = 79
  end
  object CustomIdentificacaoCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomIdentificacaoCXNGetCols
    OnGetRow = CustomIdentificacaoCXNGetRow
    OnOpen = CustomIdentificacaoCXNOpen
    Left = 208
    Top = 80
  end
  object CustomVeiculoCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomVeiculoCXNGetCols
    OnGetRow = CustomVeiculoCXNGetRow
    OnOpen = CustomVeiculoCXNOpen
    Left = 375
    Top = 224
  end
  object CustomVolumesCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomVolumesCXNGetCols
    OnGetRow = CustomVolumesCXNGetRow
    OnOpen = CustomVolumesCXNOpen
    Left = 375
    Top = 280
  end
  object CustomInformacoesAdicionaisCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomInformacoesAdicionaisCXNGetCols
    OnGetRow = CustomInformacoesAdicionaisCXNGetRow
    OnOpen = CustomInformacoesAdicionaisCXNOpen
    Left = 206
    Top = 333
  end
  object CustomObservacaoFiscoCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomObservacaoFiscoCXNGetCols
    OnGetRow = CustomObservacaoFiscoCXNGetRow
    OnOpen = CustomObservacaoFiscoCXNOpen
    Left = 374
    Top = 333
  end
  object CustomISSQNCXN: TRvCustomConnection
    RuntimeVisibility = rtDeveloper
    OnGetCols = CustomISSQNCXNGetCols
    OnGetRow = CustomISSQNCXNGetRow
    OnOpen = CustomISSQNCXNOpen
    Left = 520
    Top = 80
  end
  object RvRenderPDF1: TRvRenderPDF
    DisplayName = 'Adobe Acrobat (PDF)'
    FileExtension = '*.pdf'
    EmbedFonts = False
    ImageQuality = 90
    MetafileDPI = 300
    FontEncoding = feWinAnsiEncoding
    DocInfo.Creator = 'Rave (http://www.nevrona.com/rave)'
    DocInfo.Producer = 'Nevrona Designs'
    Left = 40
    Top = 136
  end
end
