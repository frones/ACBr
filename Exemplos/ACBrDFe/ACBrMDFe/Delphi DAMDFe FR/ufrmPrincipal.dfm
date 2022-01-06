object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Demo DAMDF-e em Fast Report'
  ClientHeight = 536
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object imgLogo: TImage
    Left = 0
    Top = 0
    Width = 358
    Height = 110
    Align = alTop
    AutoSize = True
  end
  object pnlbotoes: TPanel
    Left = 0
    Top = 444
    Width = 358
    Height = 92
    Align = alBottom
    TabOrder = 0
    object Image1: TImage
      Left = 172
      Top = 37
      Width = 176
      Height = 46
      Stretch = True
    end
    object btnImprimir: TButton
      Left = 273
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Imprimir'
      TabOrder = 1
      OnClick = btnImprimirClick
    end
    object btncarregar: TButton
      Left = 9
      Top = 6
      Width = 124
      Height = 25
      Caption = 'Carregar XML MDFe'
      TabOrder = 0
      OnClick = btncarregarClick
    end
    object btnCarregarEvento: TButton
      Left = 9
      Top = 33
      Width = 124
      Height = 25
      Caption = 'Carregar XML Evento'
      TabOrder = 2
      OnClick = btnCarregarEventoClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 110
    Width = 358
    Height = 334
    ActivePage = TabArquivos
    Align = alClient
    TabOrder = 1
    object TabArquivos: TTabSheet
      Caption = 'Arquivos *Fr3'
      object lstbxFR3: TListBox
        Left = 0
        Top = 0
        Width = 350
        Height = 306
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabCustomizacao: TTabSheet
      Caption = 'Customiza'#231#227'o'
      ImageIndex = 1
      object Label9: TLabel
        Left = 113
        Top = 150
        Width = 62
        Height = 13
        Caption = 'ImprimeValor'
      end
      object Label10: TLabel
        Left = 113
        Top = 177
        Width = 91
        Height = 13
        Caption = 'PosCanhotoLayout'
      end
      object Label11: TLabel
        Left = 113
        Top = 227
        Width = 126
        Height = 13
        Caption = 'ExibeCampoDePagamento'
      end
      object RbCanhoto: TRadioGroup
        Left = 0
        Top = 0
        Width = 350
        Height = 46
        Align = alTop
        Caption = 'Canhoto'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Cabe'#231'alho '
          'Rodap'#233
          'Esquerda')
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 46
        Width = 105
        Height = 260
        Align = alLeft
        Caption = 'Margem'
        TabOrder = 1
        object Label1: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 26
          Width = 40
          Height = 13
          Caption = 'Superior'
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 14
          Top = 51
          Width = 36
          Height = 13
          Caption = 'Inferior'
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 19
          Top = 105
          Width = 31
          Height = 13
          Caption = 'Direita'
        end
        object Label4: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 78
          Width = 45
          Height = 13
          Caption = 'Esquerda'
        end
        object EditMargemEsquerda: TEdit
          Left = 57
          Top = 74
          Width = 33
          Height = 21
          TabOrder = 2
        end
        object EditMargemSuperior: TEdit
          Left = 57
          Top = 20
          Width = 33
          Height = 21
          TabOrder = 0
        end
        object EditMargemDireita: TEdit
          Left = 57
          Top = 102
          Width = 33
          Height = 21
          TabOrder = 3
        end
        object EditMargemInferior: TEdit
          Left = 57
          Top = 47
          Width = 33
          Height = 21
          TabOrder = 1
        end
      end
      object rbTarjaCancelada: TCheckBox
        Left = 113
        Top = 104
        Width = 164
        Height = 17
        Caption = 'Mostra a Tarja de Cancelada'
        TabOrder = 4
      end
      object CBImprimirUndQtVlComercial: TComboBox
        Left = 206
        Top = 146
        Width = 141
        Height = 21
        AutoCloseUp = True
        ItemIndex = 0
        TabOrder = 5
        Text = 'iuComercial'
        Items.Strings = (
          'iuComercial'
          'iuTributavel'
          'iuComercialETributavel')
      end
      object rbImprimirDadosDocReferenciados: TCheckBox
        Left = 113
        Top = 81
        Width = 190
        Height = 17
        Caption = 'Imprimir documentos referenciados'
        TabOrder = 3
      end
      object ckImprimeCodigoEan: TCheckBox
        Left = 113
        Top = 58
        Width = 190
        Height = 17
        Caption = 'ImprimeCodigoEan'
        TabOrder = 2
      end
      object ChkQuebraLinhaEmDetalhamentos: TCheckBox
        Left = 113
        Top = 200
        Width = 184
        Height = 17
        Caption = 'Quebra Linha Em Detalhamentos'
        TabOrder = 6
      end
      object cbPosCanhotoLayout: TComboBox
        Left = 210
        Top = 173
        Width = 136
        Height = 21
        AutoCloseUp = True
        TabOrder = 7
        Text = 'prlPadrao'
        Items.Strings = (
          'prlPadrao'
          'prlBarra')
      end
      object cbExibeCampoDePagamento: TComboBox
        Left = 245
        Top = 223
        Width = 101
        Height = 21
        AutoCloseUp = True
        ItemIndex = 0
        TabOrder = 8
        Text = 'eipNenhum'
        Items.Strings = (
          'eipNenhum'
          'eipAdicionais'
          'eipQuadro')
      end
    end
    object Decimais: TTabSheet
      Caption = 'Decimais'
      ImageIndex = 2
      object RgTipodedecimais: TRadioGroup
        Left = 0
        Top = 0
        Width = 350
        Height = 49
        Align = alTop
        Caption = 'Tipo '
        Columns = 2
        Items.Strings = (
          'tdetInteger'
          'tdetMascara')
        TabOrder = 0
      end
      object PageControl2: TPageControl
        Left = 0
        Top = 49
        Width = 350
        Height = 257
        ActivePage = TabtdetInteger
        Align = alClient
        TabOrder = 1
        object TabtdetInteger: TTabSheet
          Caption = 'tdetInteger'
          ImageIndex = 1
          object Label5: TLabel
            Left = 16
            Top = 24
            Width = 56
            Height = 13
            Caption = 'Quantidade'
          end
          object Label6: TLabel
            Left = 184
            Top = 24
            Width = 24
            Height = 13
            Caption = 'Valor'
          end
          object cbtdetInteger_qtd: TComboBox
            Left = 16
            Top = 48
            Width = 56
            Height = 21
            AutoCloseUp = True
            TabOrder = 0
            Items.Strings = (
              '0'
              '1'
              '2'
              '3'
              '4'
              '5'
              '6'
              '7'
              '8'
              '9'
              '10')
          end
          object cbtdetInteger_Vrl: TComboBox
            Left = 184
            Top = 48
            Width = 56
            Height = 21
            AutoCloseUp = True
            TabOrder = 1
            Items.Strings = (
              '0'
              '1'
              '2'
              '3'
              '4'
              '5'
              '6'
              '7'
              '8'
              '9'
              '10')
          end
        end
        object TabtdetMascara: TTabSheet
          Caption = 'tdetMascara'
          ImageIndex = 2
          object Label7: TLabel
            Left = 5
            Top = 5
            Width = 56
            Height = 13
            Caption = 'Quantidade'
          end
          object Label8: TLabel
            Left = 5
            Top = 58
            Width = 24
            Height = 13
            Caption = 'Valor'
          end
          object cbtdetMascara_qtd: TComboBox
            Left = 5
            Top = 29
            Width = 185
            Height = 21
            AutoCloseUp = True
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = '#,###,##0.##########'
            Items.Strings = (
              '#,###,##0.##########'
              '#,###,##0.0#########'
              '#,###,##0.00########'
              '#,###,##0.000#######'
              '#,###,##0.0000######'
              '#,###,##0.00000#####'
              '#,###,##0.000000####'
              '#,###,##0.0000000###'
              '#,###,##0.00000000##'
              '#,###,##0.000000000#'
              '#,###,##0.0000000000')
          end
          object cbtdetMascara_Vrl: TComboBox
            Left = 5
            Top = 77
            Width = 185
            Height = 21
            AutoCloseUp = True
            Style = csDropDownList
            TabOrder = 1
            Items.Strings = (
              '#,###,##0.##########'
              '#,###,##0.0#########'
              '#,###,##0.00########'
              '#,###,##0.000#######'
              '#,###,##0.0000######'
              '#,###,##0.00000#####'
              '#,###,##0.000000####'
              '#,###,##0.0000000###'
              '#,###,##0.00000000##'
              '#,###,##0.000000000#'
              '#,###,##0.0000000000')
          end
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'xml|*.xml'
    Left = 154
    Top = 20
  end
  object frxReport1: TfrxReport
    Tag = 1
    Version = '2021.3.4'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 40401.475989294000000000
    ReportOptions.LastChange = 43838.690446203710000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnReportPrint = 'frxReportOnReportPrint'
    Left = 307
    Top = 30
  end
  object ACBrMDFe1: TACBrMDFe
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Configuracoes.RespTec.IdCSRT = 0
    DAMDFE = ACBrMDFeDAMDFEFR1
    Left = 40
    Top = 16
  end
  object ACBrMDFeDAMDFEFR1: TACBrMDFeDAMDFEFR
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 8.000000000000000000
    MargemSuperior = 8.000000000000000000
    MargemEsquerda = 6.000000000000000000
    MargemDireita = 5.100000000000000000
    ExpandeLogoMarcaConfig.Altura = 0
    ExpandeLogoMarcaConfig.Esquerda = 0
    ExpandeLogoMarcaConfig.Topo = 0
    ExpandeLogoMarcaConfig.Largura = 0
    ExpandeLogoMarcaConfig.Dimensionar = False
    ExpandeLogoMarcaConfig.Esticar = True
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    ACBrMDFe = ACBrMDFe1
    ImprimeHoraSaida = False
    TipoDAMDFe = tiSemGeracao
    TamanhoPapel = tpA4
    Cancelada = False
    Encerrado = False
    ImprimeDadosExtras = [deValorTotal, deRelacaoDFe]
    SelecionaImpressora = False
    EspessuraBorda = 1
    Left = 52
    Top = 84
  end
  object frxPDFExport1: TfrxPDFExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    EmbedFontsIfProtected = False
    InteractiveFormsFontSubset = 'A-Z,a-z,0-9,#43-#47 '
    OpenAfterExport = False
    PrintOptimized = False
    Outline = False
    Background = False
    HTMLTags = True
    Quality = 95
    Author = 'FastReport'
    Subject = 'FastReport PDF export'
    Creator = 'FastReport'
    ProtectionFlags = [ePrint, eModify, eCopy, eAnnot]
    HideToolbar = False
    HideMenubar = False
    HideWindowUI = False
    FitWindow = False
    CenterWindow = False
    PrintScaling = False
    PdfA = False
    PDFStandard = psNone
    PDFVersion = pv17
    Left = 212
    Top = 270
  end
end
