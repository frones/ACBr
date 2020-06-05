object frmPrincipal: TfrmPrincipal
  Left = 420
  Top = 211
  BorderStyle = bsToolWindow
  Caption = 'Demo DANFS-e em Fast Report'
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
    TabOrder = 2
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
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 110
    Width = 358
    Height = 160
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Arquivos *Fr3'
      object lstbxFR3: TListBox
        Left = 0
        Top = 0
        Width = 350
        Height = 132
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
      object rbTipoAmbiente: TRadioGroup
        Left = 0
        Top = 46
        Width = 350
        Height = 46
        Align = alTop
        Caption = 'Ambiente'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Produ'#231#227'o'
          'Homologa'#231#227'o')
        TabOrder = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 270
    Width = 358
    Height = 51
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      358
      51)
    object Label22: TLabel
      Left = 219
      Top = 6
      Width = 13
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'UF'
    end
    object Label20: TLabel
      Left = 141
      Top = 6
      Width = 59
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'C'#243'd. Cidade'
    end
    object Label21: TLabel
      Left = 4
      Top = 6
      Width = 33
      Height = 13
      Caption = 'Cidade'
    end
    object Label1: TLabel
      Left = 260
      Top = 6
      Width = 44
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Provedor'
    end
    object edtEmitUF: TEdit
      Left = 219
      Top = 22
      Width = 35
      Height = 21
      TabStop = False
      Anchors = [akTop, akRight]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
    end
    object edtCodCidade: TEdit
      Left = 141
      Top = 22
      Width = 72
      Height = 21
      TabStop = False
      Anchors = [akTop, akRight]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      OnChange = edtCodCidadeChange
    end
    object cbCidades: TComboBox
      Left = 4
      Top = 22
      Width = 131
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Text = 'Selecione uma Cidade'
      OnChange = cbCidadesChange
    end
    object edtProvedor: TEdit
      Left = 260
      Top = 22
      Width = 94
      Height = 21
      TabStop = False
      Anchors = [akTop, akRight]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 3
      OnChange = edtCodCidadeChange
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'xml|*.xml'
    Left = 32
    Top = 214
  end
  object frxReport1: TfrxReport
    Tag = 1
    Version = '5.6'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Padr'#227'o'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 41401.601407893500000000
    ReportOptions.LastChange = 43787.472053842600000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnReportPrint = 'frxReportOnReportPrint'
    Left = 299
    Top = 214
  end
  object ACBrNFSe1: TACBrNFSe
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.CodigoMunicipio = 0
    Configuracoes.Geral.ConsultaLoteAposEnvio = False
    Configuracoes.Geral.PathIniCidades = '..\ArqIni'
    Configuracoes.Geral.PathIniProvedor = '..\ArqIni'
    Configuracoes.Geral.Emitente.DadosSenhaParams = <>
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.Ambiente = taProducao
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    DANFSE = ACBrNFSeDANFSeFR1
    Left = 96
    Top = 180
  end
  object ACBrNFSeDANFSeFR1: TACBrNFSeDANFSeFR
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 8.000000000000000000
    MargemSuperior = 8.000000000000000000
    MargemEsquerda = 6.000000000000000000
    MargemDireita = 5.099999999999999000
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
    ACBrNFSe = ACBrNFSe1
    Cancelada = False
    ImprimeCanhoto = True
    Provedor = proNenhum
    TamanhoFonte = 6
    FormatarNumeroDocumentoNFSe = True
    EspessuraBorda = 1
    Left = 188
    Top = 192
  end
end
