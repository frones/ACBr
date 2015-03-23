object Form1: TForm1
  Left = 248
  Top = 141
  Width = 813
  Height = 379
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 200
    Top = 49
    Width = 100
    Height = 16
    Caption = 'Ultima Resposta'
  end
  object Label3: TLabel
    Left = 200
    Top = 4
    Width = 105
    Height = 16
    Caption = 'Ultimo Peso Lido:'
  end
  object Label9: TLabel
    Left = 601
    Top = 4
    Width = 51
    Height = 16
    Caption = 'TimeOut'
  end
  object Label10: TLabel
    Left = 200
    Top = 270
    Width = 68
    Height = 16
    Caption = 'Mensagem'
  end
  object sttPeso: TStaticText
    Left = 200
    Top = 20
    Width = 393
    Height = 24
    AutoSize = False
    BevelKind = bkTile
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object sttResposta: TStaticText
    Left = 200
    Top = 66
    Width = 462
    Height = 199
    AutoSize = False
    BevelKind = bkTile
    TabOrder = 2
  end
  object edtTimeOut: TEdit
    Left = 601
    Top = 20
    Width = 61
    Height = 24
    TabOrder = 0
    Text = '5000'
    OnKeyPress = edtTimeOutKeyPress
  end
  object Memo1: TMemo
    Left = 200
    Top = 288
    Width = 461
    Height = 42
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 177
    Height = 321
    BevelOuter = bvLowered
    TabOrder = 4
    object Label1: TLabel
      Left = 16
      Top = 6
      Width = 50
      Height = 16
      Caption = 'Balanca'
    end
    object Label4: TLabel
      Left = 16
      Top = 49
      Width = 70
      Height = 16
      Caption = 'Porta Serial'
    end
    object Label5: TLabel
      Left = 16
      Top = 92
      Width = 58
      Height = 16
      Caption = 'Baud rate'
    end
    object Label6: TLabel
      Left = 16
      Top = 137
      Width = 54
      Height = 16
      Caption = 'Data Bits'
    end
    object Label7: TLabel
      Left = 16
      Top = 181
      Width = 34
      Height = 16
      Caption = 'Parity'
    end
    object Label8: TLabel
      Left = 16
      Top = 271
      Width = 80
      Height = 16
      Caption = 'Handshaking'
    end
    object Label11: TLabel
      Left = 16
      Top = 224
      Width = 53
      Height = 16
      Caption = 'Stop Bits'
    end
    object cmbBalanca: TComboBox
      Left = 16
      Top = 22
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 0
      Items.Strings = (
        'Nenhuma'
        'Filizola'
        'Toledo'
        'Toledo2180'
        'Urano'
        'LucasTec'
        'Magna'
        'Digitron'
        'Magellan'
        'UranoPOP'
        'Lider')
    end
    object cmbPortaSerial: TComboBox
      Left = 16
      Top = 65
      Width = 145
      Height = 24
      ItemHeight = 16
      TabOrder = 1
      Text = 'COM1'
      Items.Strings = (
        'COM1'
        'COM2'
        'COM3'
        'COM4'
        'COM5'
        'COM6'
        'COM7'
        'COM8')
    end
    object cmbBaudRate: TComboBox
      Left = 16
      Top = 110
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 6
      TabOrder = 2
      Text = '9600'
      Items.Strings = (
        '110'
        '300'
        '600'
        '1200'
        '2400'
        '4800'
        '9600'
        '14400'
        '19200'
        '38400'
        '56000'
        '57600')
    end
    object cmbDataBits: TComboBox
      Left = 16
      Top = 153
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 3
      TabOrder = 3
      Text = '8'
      Items.Strings = (
        '5'
        '6'
        '7'
        '8')
    end
    object cmbHandShaking: TComboBox
      Left = 16
      Top = 289
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 0
      TabOrder = 6
      Text = 'Nenhum'
      Items.Strings = (
        'Nenhum'
        'XON/XOFF'
        'RTS/CTS'
        'DTR/DSR')
    end
    object cmbParity: TComboBox
      Left = 16
      Top = 198
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 0
      TabOrder = 4
      Text = 'none'
      Items.Strings = (
        'none'
        'odd'
        'even'
        'mark'
        'space')
    end
    object cmbStopBits: TComboBox
      Left = 16
      Top = 242
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 0
      TabOrder = 5
      Text = 's1'
      Items.Strings = (
        's1'
        's1,5'
        's2'
        '')
    end
  end
  object Panel2: TPanel
    Left = 674
    Top = 0
    Width = 131
    Height = 345
    Align = alRight
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 5
    object btnConectar: TButton
      Left = 16
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Ativar'
      TabOrder = 0
      OnClick = btnConectarClick
    end
    object btnDesconectar: TButton
      Left = 16
      Top = 45
      Width = 105
      Height = 25
      Caption = 'Desativar'
      Enabled = False
      TabOrder = 1
      OnClick = btnDesconectarClick
    end
    object btnLerPeso: TButton
      Left = 16
      Top = 80
      Width = 105
      Height = 25
      Caption = 'Ler Peso'
      Enabled = False
      TabOrder = 2
      OnClick = btnLerPesoClick
    end
    object chbMonitorar: TCheckBox
      Left = 17
      Top = 115
      Width = 103
      Height = 42
      Caption = 'Monitorar Balan'#231'a'
      TabOrder = 3
      WordWrap = True
      OnClick = chbMonitorarClick
    end
    object btnLimpar: TButton
      Left = 16
      Top = 208
      Width = 105
      Height = 25
      Caption = 'Limpar'
      TabOrder = 4
      OnClick = btnLimparClick
    end
    object btnSair: TButton
      Left = 16
      Top = 243
      Width = 105
      Height = 25
      Caption = 'Sair'
      TabOrder = 5
      OnClick = btnSairClick
    end
  end
  object ACBrBAL1: TACBrBAL
    Porta = 'COM1'
    OnLePeso = ACBrBAL1LePeso
    Left = 696
    Top = 160
  end
end
