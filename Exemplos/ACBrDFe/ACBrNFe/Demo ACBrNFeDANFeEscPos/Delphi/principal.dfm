object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  Caption = 'Demo Impress'#227'o DANFCe'
  ClientHeight = 571
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 171
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Porta'
  end
  object Label2: TLabel
    Left = 20
    Top = 20
    Width = 34
    Height = 13
    Caption = 'Modelo'
  end
  object Label3: TLabel
    Left = 252
    Top = 20
    Width = 51
    Height = 13
    Caption = 'Velocidade'
  end
  object Label4: TLabel
    Left = 403
    Top = 20
    Width = 96
    Height = 13
    Caption = 'Linhas entre cupons'
  end
  object Label5: TLabel
    Left = 20
    Top = 63
    Width = 52
    Height = 13
    Caption = 'Logomarca'
  end
  object btnProcurarLogomarca: TSpeedButton
    Left = 483
    Top = 78
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnProcurarLogomarcaClick
  end
  object Label6: TLabel
    Left = 20
    Top = 106
    Width = 42
    Height = 13
    Caption = 'Id Token'
  end
  object Label7: TLabel
    Left = 112
    Top = 106
    Width = 60
    Height = 13
    Caption = 'CSC (Token)'
  end
  object cbxPorta: TComboBox
    Left = 171
    Top = 36
    Width = 75
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbxModelo: TComboBox
    Left = 20
    Top = 36
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object cbxVelocidade: TComboBox
    Left = 252
    Top = 36
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    Items.Strings = (
      '9600'
      '115200')
  end
  object edtLinhasEntreCupom: TSpinEdit
    Left = 403
    Top = 36
    Width = 103
    Height = 22
    MaxValue = 50
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
  object edtLogomarca: TEdit
    Left = 20
    Top = 79
    Width = 457
    Height = 21
    ReadOnly = True
    TabOrder = 4
    Text = 'edtLogomarca'
  end
  object btnNFCeImprimirDANFE: TButton
    Left = 268
    Top = 527
    Width = 116
    Height = 25
    Caption = 'Imprimir XML NFC-e'
    TabOrder = 5
    OnClick = btnNFCeImprimirDANFEClick
  end
  object txtMemo: TMemo
    Left = 20
    Top = 270
    Width = 486
    Height = 251
    Lines.Strings = (
      'TEXTO LIVRE'
      'At'#233' 600 caracteres'
      #193#201#218#205#211#218#225#233#237#243#250#199#231#195#213#227#245
      ''
      '</linha_dupla>'
      ''
      '<CE>*** TIPOS DE FONTE ***</CE>'
      '<FN>TEXTO TEXTO TEXTO TEXTO</fn>'
      '<fp>TEXTO TEXTO TEXTO TEXTO</fp>'
      ''
      '<CE>*** TAGS DE FORMATA'#199#195'O ***</CE>'
      '<e>EXPANDIDO</e>'
      '<N>Negrito</n>'
      '<S>Sublinhado</s>'
      '<C>CONDENSADO</C>'
      '<i>ITALICO</I>'
      '</linha_simples>'
      ''
      '<CE>*** TAGS DE C'#211'DIGO DE BARRAS ***</CE>'
      'EAN 8:'
      '<ean8>1234567</ean8>'
      'EAN13:'
      '<ean13>123456789012</ean13>'
      'INT25:'
      '<inter>1234567890</inter>'
      'CODE39: '
      '<code39>ABCDE12345</code39>'
      'CODE93:'
      '<code93>ABC123abc</code93>'
      'CODE128:'
      '<code128>$-=+ABC123abc</code128>'
      'UPCA:'
      '<upca>12345678901</upca>'
      'CODABAR :'
      '<codabar>$12345</codabar>'
      '</linha_simples>'
      ''
      '<CE>*** TAGS DE FORMATA'#199#195'O ***</CE>'
      '<ae>Alinhado esquerda</Ae>'
      '<ce>NO CENTRO</CE>'
      '<AD>A Direira</ad>'
      '</linha_simples>'
      ''
      '<CE>*** TESTE DE TAGS INV'#193'LIDAS ***</CE>'
      '<ce> <>tags inv'#225'lidas no texto">">><<</CE>'
      '<AD><da><ec></</A Direira</ad>'
      ''
      '</linha_dupla>')
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object btnImprimirRelatorio: TButton
    Left = 20
    Top = 527
    Width = 116
    Height = 25
    Caption = 'Imprimir Relat'#243'rio'
    TabOrder = 7
    OnClick = btnImprimirRelatorioClick
  end
  object chkImprimirItem1Linha: TCheckBox
    Left = 20
    Top = 149
    Width = 186
    Height = 17
    Caption = 'Imprimir itens somente em 1 linha'
    TabOrder = 8
  end
  object chkDanfeResumido: TCheckBox
    Left = 20
    Top = 172
    Width = 186
    Height = 17
    Caption = 'DANFC-e resumido'
    TabOrder = 9
  end
  object chkIgnorarTagsFormatacao: TCheckBox
    Left = 291
    Top = 149
    Width = 186
    Height = 17
    Caption = 'Ignorar tags de Formata'#231#227'o'
    TabOrder = 10
  end
  object chkImprimirDescAcresItem: TCheckBox
    Left = 291
    Top = 172
    Width = 215
    Height = 17
    Caption = 'Imprime desconto/acr'#233'scimo por item'
    TabOrder = 11
  end
  object chkViaConsumidor: TCheckBox
    Left = 291
    Top = 195
    Width = 215
    Height = 17
    Caption = 'Via consumidor'
    Checked = True
    State = cbChecked
    TabOrder = 12
  end
  object btnNFCeImprimirEvento: TButton
    Left = 390
    Top = 527
    Width = 116
    Height = 25
    Caption = 'Imprimir Evento'
    TabOrder = 13
    OnClick = btnNFCeImprimirEventoClick
  end
  object edtCSCId: TEdit
    Left = 20
    Top = 122
    Width = 86
    Height = 21
    TabOrder = 14
    Text = 'edtLogomarca'
  end
  object edtCSCNumero: TEdit
    Left = 112
    Top = 122
    Width = 365
    Height = 21
    TabOrder = 15
    Text = 'edtLogomarca'
  end
  object chkAbrirGaveta: TCheckBox
    Left = 291
    Top = 218
    Width = 215
    Height = 17
    Caption = 'Abrir Gaveta'
    TabOrder = 16
  end
  object ACBrNFe: TACBrNFe
    Configuracoes.Geral.SSLLib = libCapicom
    Configuracoes.Geral.SSLCryptLib = cryCapicom
    Configuracoes.Geral.SSLHttpLib = httpWinINet
    Configuracoes.Geral.SSLXmlSignLib = xsMsXmlCapicom
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.ValidarDigest = False
    Configuracoes.Geral.VersaoDF = ve200
    Configuracoes.Geral.VersaoQRCode = veqr000
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    DANFE = ACBrNFeDANFeESCPOS
    Left = 140
    Top = 405
  end
  object ACBrNFeDANFeESCPOS: TACBrNFeDANFeESCPOS
    PathPDF = '.\pdf\'
    Sistema = 'Teste de Impress'#227'o ACBRr EscPOS'
    Site = 'http://www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    ACBrNFe = ACBrNFe
    TipoDANFE = tiNFCe
    ImprimeDescAcrescItem = False
    PosPrinter = ACBrPosPrinter1
    Left = 235
    Top = 405
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'Arquivos XML|*.xml'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Abrir XML'
    Left = 335
    Top = 405
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 0
    CortaPapel = False
    ControlePorta = True
    Left = 235
    Top = 455
  end
end
