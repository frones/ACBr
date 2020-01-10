object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Demo DANF-e em Fast Report'
  ClientHeight = 536
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
    Top = 149
    Width = 358
    Height = 295
    ActivePage = TabArquivos
    Align = alClient
    TabOrder = 1
    object TabArquivos: TTabSheet
      Caption = 'Arquivos *Fr3'
      object lstbxFR3: TListBox
        Left = 0
        Top = 0
        Width = 350
        Height = 267
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
        Height = 221
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
      object rbTarjaNfeCancelada: TCheckBox
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
        TabOrder = 6
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
      object ckImprimeItens: TCheckBox
        Left = 113
        Top = 127
        Width = 97
        Height = 17
        Caption = 'Imprime itens'
        TabOrder = 5
      end
      object ChkQuebraLinhaEmDetalhamentos: TCheckBox
        Left = 113
        Top = 200
        Width = 184
        Height = 17
        Caption = 'Quebra Linha Em Detalhamentos'
        TabOrder = 7
      end
      object cbPosCanhotoLayout: TComboBox
        Left = 210
        Top = 173
        Width = 136
        Height = 21
        AutoCloseUp = True
        TabOrder = 8
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
        TabOrder = 9
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
        Height = 218
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
  object rgModelo: TRadioGroup
    Left = 0
    Top = 110
    Width = 358
    Height = 39
    Align = alTop
    Caption = 'Modelo'
    Columns = 2
    Items.Strings = (
      'NFe (ACBrNFeDANFeFR)'
      'NFCe (ACBrNFeDANFCEFR)')
    TabOrder = 2
    OnClick = rgModeloClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 'xml|*.xml'
    Left = 136
    Top = 22
  end
  object frxReport1: TfrxReport
    Tag = 1
    Version = '6.3.8'
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
  object ACBrNFeDANFEFR1: TACBrNFeDANFEFR
    MostraSetup = True
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
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
    ACBrNFe = ACBrNFe1
    TipoDANFE = tiSemGeracao
    ExibeTotalTributosItem = True
    EspessuraBorda = 1
    BorderIcon = [biSystemMenu, biMinimize, biMaximize]
    Left = 48
    Top = 22
  end
  object ACBrNFeDANFCEFR1: TACBrNFeDANFCEFR
    MostraSetup = True
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
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
    BorderIcon = [biSystemMenu, biMinimize, biMaximize]
    Left = 52
    Top = 76
  end
  object ACBrNFe1: TACBrNFe
    Configuracoes.Geral.SSLLib = libCapicomDelphiSoap
    Configuracoes.Geral.SSLCryptLib = cryCapicom
    Configuracoes.Geral.SSLHttpLib = httpIndy
    Configuracoes.Geral.SSLXmlSignLib = xsMsXmlCapicom
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.VersaoQRCode = veqr000
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Configuracoes.RespTec.IdCSRT = 0
    DANFE = ACBrNFeDANFEFR1
    Left = 221
    Top = 30
  end
end
