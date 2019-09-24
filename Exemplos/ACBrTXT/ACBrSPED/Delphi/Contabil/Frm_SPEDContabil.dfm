object FrmSPEDContabil: TFrmSPEDContabil
  Left = 356
  Top = 153
  Caption = 'ACBrSpedContabil - Demo'
  ClientHeight = 508
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    670
    508)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 65
    Height = 13
    Caption = 'Lista de erros'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 8
    Top = 232
    Width = 75
    Height = 13
    Caption = 'Arquivo Gerado'
    Color = clBtnFace
    ParentColor = False
  end
  object memoError: TMemo
    Left = 8
    Top = 145
    Width = 637
    Height = 83
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object memoTXT: TMemo
    Left = 8
    Top = 248
    Width = 637
    Height = 208
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WantReturns = False
    WordWrap = False
  end
  object btnError: TButton
    Left = 395
    Top = 472
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Gerar TXT Error'
    TabOrder = 2
    OnClick = btnErrorClick
  end
  object btnTXT: TButton
    Left = 521
    Top = 472
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Gerar TXT'
    TabOrder = 3
    OnClick = btnTXTClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 670
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 4
    DesignSize = (
      666
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
      Width = 666
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
    end
    object edtFile: TEdit
      Left = 22
      Top = 28
      Width = 288
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'ACBrSpedContabil.txt'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 58
    Width = 670
    Height = 62
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    TabOrder = 5
    object Label4: TLabel
      Left = 1
      Top = 1
      Width = 664
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
    end
    object btnB_0: TButton
      Left = 15
      Top = 22
      Width = 100
      Height = 25
      Caption = 'Registros Bloco 0'
      TabOrder = 0
      OnClick = btnB_0Click
    end
    object btnB_I: TButton
      Left = 118
      Top = 22
      Width = 100
      Height = 25
      Caption = 'Registros Bloco I'
      TabOrder = 1
      OnClick = btnB_IClick
    end
    object btnB_J: TButton
      Left = 220
      Top = 22
      Width = 100
      Height = 25
      Caption = 'Registros Bloco J'
      TabOrder = 2
      OnClick = btnB_JClick
    end
  end
  object ACBrSPEDContabil1: TACBrSPEDContabil
    Path = '.\'
    Delimitador = '|'
    TrimString = True
    CurMascara = '#0.00'
    OnError = ACBrSPEDContabil1Error
    Left = 302
    Top = 302
  end
end
