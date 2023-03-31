object FormSinaleiraDemo: TFormSinaleiraDemo
  Left = 0
  Top = 0
  Caption = 'Sinaleira Teste'
  ClientHeight = 361
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 8
    Top = 17
    Width = 177
    Height = 328
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 6
      Width = 88
      Height = 15
      Caption = 'Modelo Sinaleira'
      Color = clBtnFace
      ParentColor = False
    end
    object Label4: TLabel
      Left = 16
      Top = 49
      Width = 59
      Height = 15
      Caption = 'Porta Serial'
      Color = clBtnFace
      ParentColor = False
    end
    object Label5: TLabel
      Left = 16
      Top = 92
      Width = 50
      Height = 15
      Caption = 'Baud rate'
      Color = clBtnFace
      ParentColor = False
    end
    object Label6: TLabel
      Left = 16
      Top = 137
      Width = 46
      Height = 15
      Caption = 'Data Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object Label7: TLabel
      Left = 16
      Top = 181
      Width = 30
      Height = 15
      Caption = 'Parity'
      Color = clBtnFace
      ParentColor = False
    end
    object Label8: TLabel
      Left = 16
      Top = 271
      Width = 70
      Height = 15
      Caption = 'Handshaking'
      Color = clBtnFace
      ParentColor = False
    end
    object Label11: TLabel
      Left = 16
      Top = 224
      Width = 46
      Height = 15
      Caption = 'Stop Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object cmbSinaleira: TComboBox
      Left = 16
      Top = 22
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Nenhuma'
      Items.Strings = (
        'Nenhuma'
        'Laurenti')
    end
    object cmbPortaSerial: TComboBox
      Left = 16
      Top = 65
      Width = 145
      Height = 23
      ItemIndex = 0
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
      Height = 23
      Style = csDropDownList
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
      Height = 23
      Style = csDropDownList
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
      Height = 23
      Style = csDropDownList
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
      Height = 23
      Style = csDropDownList
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
      Height = 23
      Style = csDropDownList
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
  object ButtonAtivar: TButton
    Left = 191
    Top = 17
    Width = 122
    Height = 41
    Caption = 'Ativar/Conectar'
    TabOrder = 1
    OnClick = ButtonAtivarClick
  end
  object ButtonDesativar: TButton
    Left = 337
    Top = 17
    Width = 122
    Height = 41
    Caption = 'Desativar'
    TabOrder = 2
    OnClick = ButtonDesativarClick
  end
  object GroupBox1: TGroupBox
    Left = 191
    Top = 82
    Width = 272
    Height = 119
    Caption = 'Define Led'
    TabOrder = 3
    object ButtonRed: TButton
      Left = 181
      Top = 27
      Width = 86
      Height = 82
      Caption = 'Red'
      TabOrder = 0
      OnClick = ButtonRedClick
    end
    object ButtonYellow: TButton
      Left = 93
      Top = 27
      Width = 86
      Height = 82
      Caption = 'Yellow'
      TabOrder = 1
      OnClick = ButtonYellowClick
    end
    object ButtonGreen: TButton
      Left = 5
      Top = 27
      Width = 86
      Height = 82
      Caption = 'Green'
      TabOrder = 2
      OnClick = ButtonGreenClick
    end
  end
  object ACBrSIN1: TACBrSIN
    Modelo = sinLaurenti
    Porta = 'COM1'
    Left = 152
    Top = 40
  end
end
