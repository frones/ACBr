object FrmSPEDFiscal: TFrmSPEDFiscal
  Left = 356
  Top = 153
  Caption = 'ACBrSpedFiscal - Demo'
  ClientHeight = 595
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    747
    595)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 201
    Width = 65
    Height = 13
    Caption = 'Lista de erros'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 8
    Top = 311
    Width = 75
    Height = 13
    Caption = 'Arquivo Gerado'
    Color = clBtnFace
    ParentColor = False
  end
  object Label7: TLabel
    Left = 25
    Top = 499
    Width = 63
    Height = 13
    Caption = 'Buffer Linhas'
    Color = clBtnFace
    ParentColor = False
  end
  object Label8: TLabel
    Left = 130
    Top = 499
    Width = 61
    Height = 13
    Caption = 'Buffer Notas'
    Color = clBtnFace
    ParentColor = False
  end
  object memoError: TMemo
    Left = 8
    Top = 218
    Width = 731
    Height = 83
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object memoTXT: TMemo
    Left = 8
    Top = 328
    Width = 731
    Height = 221
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
    WantReturns = False
    WordWrap = False
  end
  object btnError: TButton
    Left = 535
    Top = 562
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Gerar TXT Error'
    Enabled = False
    TabOrder = 7
    OnClick = btnErrorClick
  end
  object btnTXT: TButton
    Left = 641
    Top = 562
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Gerar TXT'
    TabOrder = 8
    OnClick = btnTXTClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 747
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
    DesignSize = (
      743
      54)
    object Label1: TLabel
      Left = 22
      Top = 14
      Width = 82
      Height = 13
      Caption = 'Nome do Arquivo'
      Color = clBtnFace
      ParentColor = False
    end
    object Label5: TLabel
      Left = 0
      Top = 0
      Width = 743
      Height = 16
      Align = alTop
      Alignment = taCenter
      Caption = '1 - Informe o Nome do Arquivo e m'#233'todo de Gera'#231#227'o'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 341
    end
    object Label6: TLabel
      Left = 526
      Top = 14
      Width = 71
      Height = 13
      Caption = 'Num.Notas (C)'
      Color = clBtnFace
      ParentColor = False
    end
    object Label9: TLabel
      Left = 622
      Top = 14
      Width = 74
      Height = 13
      Caption = 'Ano Refer'#234'ncia'
      Color = clBtnFace
      ParentColor = False
    end
    object edtFile: TEdit
      Left = 22
      Top = 28
      Width = 269
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'ACBrSpedFiscal.txt'
      OnChange = edtFileChange
      OnExit = edtFileChange
    end
    object cbConcomitante: TCheckBox
      Left = 315
      Top = 30
      Width = 134
      Height = 19
      Hint = 
        'Grava os Registros a medida que s'#227'o alimentados'#13#10'Economizando me' +
        'm'#243'ria. '#13#10#218'til para evitar erros em arquivos Enormes'
      Anchors = [akTop, akRight]
      Caption = 'Gerar Concomitante'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = cbConcomitanteClick
    end
    object edNotas: TEdit
      Left = 526
      Top = 28
      Width = 32
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = '2'
    end
    object DtRef: TDateTimePicker
      Left = 622
      Top = 28
      Width = 90
      Height = 21
      Date = 43124.498320694450000000
      Time = 43124.498320694450000000
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 58
    Width = 747
    Height = 126
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 1
    object Label4: TLabel
      Left = 1
      Top = 1
      Width = 741
      Height = 16
      Align = alTop
      Alignment = taCenter
      Caption = 
        '2 - Clique em cada bot'#227'o dos Blocos e em seguida no bot'#227'o Gerar ' +
        'TXT'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitWidth = 450
    end
    object btnB_0: TButton
      Left = 16
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco 0'
      TabOrder = 1
      OnClick = btnB_0Click
    end
    object btnB_1: TButton
      Left = 17
      Top = 85
      Width = 100
      Height = 25
      Caption = 'Registros Bloco 1'
      Enabled = False
      TabOrder = 7
      OnClick = btnB_1Click
    end
    object btnB_C: TButton
      Left = 118
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco C'
      Enabled = False
      TabOrder = 2
      OnClick = btnB_CClick
    end
    object btnB_D: TButton
      Left = 220
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco D'
      Enabled = False
      TabOrder = 3
      OnClick = btnB_DClick
    end
    object btnB_E: TButton
      Left = 323
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco E'
      Enabled = False
      TabOrder = 4
      OnClick = btnB_EClick
    end
    object btnB_H: TButton
      Left = 531
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco H'
      Enabled = False
      TabOrder = 6
      OnClick = btnB_HClick
    end
    object btnB_G: TButton
      Left = 427
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco G'
      Enabled = False
      TabOrder = 5
      OnClick = btnB_GClick
    end
    object btnB_Completo: TButton
      Left = 16
      Top = 23
      Width = 719
      Height = 25
      Caption = 'Gerar o arquivo do SPED Fiscal completo'
      TabOrder = 0
      OnClick = btnB_CompletoClick
    end
    object btnB_K: TButton
      Left = 635
      Top = 54
      Width = 100
      Height = 25
      Caption = 'Registros Bloco K'
      Enabled = False
      TabOrder = 8
      OnClick = btnB_KClick
    end
  end
  object btnB_9: TButton
    Left = 429
    Top = 562
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Gravar Bloco 9'
    Enabled = False
    TabOrder = 6
    OnClick = btnB_9Click
  end
  object edBufLinhas: TEdit
    Left = 8
    Top = 564
    Width = 80
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Text = '1000'
  end
  object edBufNotas: TEdit
    Left = 113
    Top = 564
    Width = 80
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 5
    Text = '1000'
  end
  object ProgressBar1: TProgressBar
    Left = 79
    Top = 192
    Width = 348
    Height = 20
    TabOrder = 9
    Visible = False
  end
  object btnCancelaGeracao: TButton
    Left = 480
    Top = 190
    Width = 102
    Height = 25
    Caption = 'Cancela Gera'#231#227'o'
    TabOrder = 10
    OnClick = btnCancelaGeracaoClick
  end
  object ACBrSPEDFiscal1: TACBrSPEDFiscal
    Path = '.\'
    Delimitador = '|'
    TrimString = True
    CurMascara = '#0.00'
    OnError = ACBrSPEDFiscal1Error
    Left = 44
    Top = 441
  end
end
