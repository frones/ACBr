object frConfiguraSerial: TfrConfiguraSerial
  Left = 322
  Top = 155
  Width = 200
  Height = 391
  HorzScrollBar.Range = 177
  VertScrollBar.Range = 337
  ActiveControl = cmbPortaSerial
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Porta Serial'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 180
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 52
    Width = 136
    Height = 13
    Caption = '&Baud rate (Bits por Segundo)'
    FocusControl = cmbBaudRate
  end
  object Label6: TLabel
    Left = 8
    Top = 97
    Width = 118
    Height = 13
    Caption = '&Data Bits (Bits de Dados)'
    FocusControl = cmbDataBits
  end
  object Label7: TLabel
    Left = 8
    Top = 141
    Width = 77
    Height = 13
    Caption = '&Parity (Paridade)'
    FocusControl = cmbParity
  end
  object Label11: TLabel
    Left = 8
    Top = 184
    Width = 120
    Height = 13
    Caption = '&Stop Bits (Bits de Parada)'
    FocusControl = cmbStopBits
  end
  object Label8: TLabel
    Left = 8
    Top = 231
    Width = 154
    Height = 13
    Caption = '&Handshaking (Controle de Fluxo)'
    FocusControl = cmbHandShaking
  end
  object Label4: TLabel
    Left = 8
    Top = 11
    Width = 54
    Height = 13
    Caption = '&Porta Serial'
    FocusControl = cmbPortaSerial
  end
  object cmbBaudRate: TComboBox
    Left = 8
    Top = 70
    Width = 161
    Height = 21
    TabOrder = 1
    Text = '9600'
    OnChange = cmbBaudRateChange
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
      '57600'
      '115200')
  end
  object cmbDataBits: TComboBox
    Left = 8
    Top = 113
    Width = 161
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = cmbDataBitsChange
    Items.Strings = (
      '5'
      '6'
      '7'
      '8')
  end
  object cmbParity: TComboBox
    Left = 8
    Top = 158
    Width = 161
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = cmbParityChange
    Items.Strings = (
      'None'
      'Odd'
      'Even'
      'Mark'
      'Space')
  end
  object cmbStopBits: TComboBox
    Left = 8
    Top = 202
    Width = 161
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = cmbStopBitsChange
    Items.Strings = (
      '1'
      '1,5'
      '2')
  end
  object cmbHandShaking: TComboBox
    Left = 8
    Top = 249
    Width = 161
    Height = 21
    Style = csDropDownList
    TabOrder = 5
    OnChange = cmbHandShakingChange
    Items.Strings = (
      'Nenhum'
      'XON/XOFF'
      'RTS/CTS'
      'DTR/DSR')
  end
  object cmbPortaSerial: TComboBox
    Left = 8
    Top = 27
    Width = 161
    Height = 21
    DropDownCount = 10
    TabOrder = 0
    Text = 'COM1'
    OnChange = cmbPortaSerialChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'COM6'
      'COM7'
      'COM8'
      'COM9')
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 312
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
  end
  object BitBtn2: TBitBtn
    Left = 94
    Top = 312
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object chHardFlow: TCheckBox
    Left = 8
    Top = 280
    Width = 89
    Height = 17
    Caption = 'HardFlow'
    TabOrder = 8
    OnClick = chHardFlowClick
  end
  object chSoftFlow: TCheckBox
    Left = 96
    Top = 280
    Width = 81
    Height = 17
    Caption = 'SoftFlow'
    TabOrder = 9
    OnClick = chSoftFlowClick
  end
end
