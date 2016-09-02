object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Demo DANF-e em Fast Report'
  ClientHeight = 413
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Top = 321
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
      Width = 106
      Height = 25
      Caption = 'Carregar XML NFe'
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
    object btncarregarinutilizacao: TButton
      Left = 9
      Top = 60
      Width = 145
      Height = 25
      Caption = 'Carregar XML Inutiliza'#231#227'o'
      TabOrder = 3
      OnClick = btncarregarinutilizacaoClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 110
    Width = 358
    Height = 211
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Arquivos *Fr3'
      object lstbxFR3: TListBox
        Left = 0
        Top = 0
        Width = 350
        Height = 183
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Customiza'#231#227'o'
      ImageIndex = 1
      object RbCanhoto: TRadioGroup
        Left = 0
        Top = 0
        Width = 350
        Height = 46
        Align = alTop
        Caption = 'Canhoto'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Cabe'#231'alho '
          'Rodap'#233)
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 46
        Width = 350
        Height = 59
        Align = alTop
        Caption = 'Margem'
        TabOrder = 1
        object Label1: TLabel
          Left = 15
          Top = 13
          Width = 40
          Height = 13
          Caption = 'Superior'
        end
        object Label2: TLabel
          Left = 75
          Top = 13
          Width = 36
          Height = 13
          Caption = 'Inferior'
        end
        object Label3: TLabel
          Left = 254
          Top = 13
          Width = 31
          Height = 13
          Caption = 'Direita'
        end
        object Label4: TLabel
          Left = 194
          Top = 13
          Width = 45
          Height = 13
          Caption = 'Esquerda'
        end
        object EditMargemEsquerda: TEdit
          Left = 194
          Top = 31
          Width = 33
          Height = 21
          TabOrder = 0
        end
        object EditMargemSuperior: TEdit
          Left = 15
          Top = 31
          Width = 30
          Height = 21
          TabOrder = 1
        end
        object EditMargemDireita: TEdit
          Left = 254
          Top = 31
          Width = 33
          Height = 21
          TabOrder = 2
        end
        object EditMargemInferior: TEdit
          Left = 75
          Top = 31
          Width = 33
          Height = 21
          TabOrder = 3
        end
      end
    end
  end
  object ACBrNFe1: TACBrNFe
    Configuracoes.Geral.SSLLib = libCapicomDelphiSoap
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.IncluirQRCodeXMLNFCe = False
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    DANFE = ACBrNFeDANFEFR1
    Left = 213
    Top = 214
  end
  object ACBrNFeDANFEFR1: TACBrNFeDANFEFR
    ACBrNFe = ACBrNFe1
    MostrarPreview = True
    MostrarStatus = True
    TipoDANFE = tiSemGeracao
    NumCopias = 1
    ImprimeNomeFantasia = False
    ImprimirDescPorc = False
    ImprimirTotalLiquido = True
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais._qCom = 2
    CasasDecimais._vUnCom = 2
    CasasDecimais._Mask_qCom = '###,###,###,##0.00'
    CasasDecimais._Mask_vUnCom = '###,###,###,##0.00'
    ExibirResumoCanhoto = False
    FormularioContinuo = False
    TamanhoFonte_DemaisCampos = 10
    ProdutosPorPagina = 0
    ImprimirDetalhamentoEspecifico = True
    NFeCancelada = False
    ImprimirItens = True
    ViaConsumidor = True
    TamanhoLogoHeight = 0
    TamanhoLogoWidth = 0
    RecuoEndereco = 0
    RecuoEmpresa = 0
    LogoemCima = False
    TamanhoFonteEndereco = 0
    RecuoLogo = 0
    TributosSeparadamente = False
    EspessuraBorda = 1
    ExibirTotalTributosItem = False
    ExibeCampoFatura = True
    TributosPercentual = ptValorProdutos
    ImprimirUnQtVlComercial = False
    Detalhado = False
    DescricaoViaEstabelec = 'Via do Consumidor'
    ExpandirDadosAdicionaisAuto = False
    ImprimirDadosArma = True
    QuebraLinhaEmDetalhamentoEspecifico = True
    IncorporarBackgroundPdf = True
    IncorporarFontesPdf = True
    Left = 120
    Top = 214
  end
  object OpenDialog1: TOpenDialog
    Filter = 'xml|*.xml'
    Left = 32
    Top = 214
  end
  object frxReport1: TfrxReport
    Version = '5.5'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 42426.048066701390000000
    ReportOptions.LastChange = 42426.048066701390000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      ''
      'begin'
      ''
      'end.')
    OnReportPrint = 'frxReportOnReportPrint'
    Left = 299
    Top = 214
    Datasets = <>
    Variables = <>
    Style = <>
    object Data: TfrxDataPage
      Height = 1000.000000000000000000
      Width = 1000.000000000000000000
    end
    object Page1: TfrxReportPage
      PaperWidth = 210.000000000000000000
      PaperHeight = 297.000000000000000000
      PaperSize = 9
      LeftMargin = 10.000000000000000000
      RightMargin = 10.000000000000000000
      TopMargin = 10.000000000000000000
      BottomMargin = 10.000000000000000000
      object ReportTitle1: TfrxReportTitle
        FillType = ftBrush
        Height = 22.677180000000000000
        Top = 18.897650000000000000
        Width = 718.110700000000000000
      end
      object MasterData1: TfrxMasterData
        FillType = ftBrush
        Height = 22.677180000000000000
        Top = 102.047310000000000000
        Width = 718.110700000000000000
        RowCount = 0
      end
      object PageFooter1: TfrxPageFooter
        FillType = ftBrush
        Height = 22.677180000000000000
        Top = 185.196970000000000000
        Width = 718.110700000000000000
      end
      object Memo1: TfrxMemoView
        Left = 642.520100000000000000
        Top = 185.196970000000000000
        Width = 75.590600000000000000
        Height = 18.897650000000000000
        HAlign = haRight
        Memo.UTF8W = (
          '[Page#]')
      end
    end
  end
end
