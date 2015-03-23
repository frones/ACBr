object frConfiguraSerial: TfrConfiguraSerial
  Left = 322
  Height = 350
  Top = 155
  Width = 187
  ActiveControl = cmbPortaSerial
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Porta Serial'
  ClientHeight = 350
  ClientWidth = 187
  Constraints.MinHeight = 350
  Constraints.MinWidth = 180
  Font.CharSet = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  ParentFont = False
  Position = poOwnerFormCenter
  LCLVersion = '0.9.26'
  object Label5: TLabel
    Left = 8
    Height = 14
    Top = 52
    Width = 137
    Caption = '&Baud rate (Bits por Segundo)'
    FocusControl = cmbBaudRate
    ParentColor = False
  end
  object Label6: TLabel
    Left = 8
    Height = 14
    Top = 97
    Width = 119
    Caption = '&Data Bits (Bits de Dados)'
    FocusControl = cmbDataBits
    ParentColor = False
  end
  object Label7: TLabel
    Left = 8
    Height = 14
    Top = 141
    Width = 78
    Caption = '&Parity (Paridade)'
    FocusControl = cmbParity
    ParentColor = False
  end
  object Label11: TLabel
    Left = 8
    Height = 14
    Top = 184
    Width = 121
    Caption = '&Stop Bits (Bits de Parada)'
    FocusControl = cmbStopBits
    ParentColor = False
  end
  object Label8: TLabel
    Left = 8
    Height = 14
    Top = 231
    Width = 155
    Caption = '&Handshaking (Controle de Fluxo)'
    FocusControl = cmbHandShaking
    ParentColor = False
  end
  object Label4: TLabel
    Left = 8
    Height = 14
    Top = 11
    Width = 55
    Caption = '&Porta Serial'
    FocusControl = cmbPortaSerial
    ParentColor = False
  end
  object cmbBaudRate: TComboBox
    Left = 8
    Height = 21
    Top = 70
    Width = 161
    ItemHeight = 13
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
      '115200'
    )
    MaxLength = -1
    OnChange = cmbBaudRateChange
    TabOrder = 1
  end
  object cmbDataBits: TComboBox
    Left = 8
    Height = 21
    Top = 113
    Width = 161
    ItemHeight = 13
    ItemIndex = 3
    Items.Strings = (
      '5'
      '6'
      '7'
      '8'
    )
    MaxLength = -1
    OnChange = cmbDataBitsChange
    Style = csDropDownList
    TabOrder = 2
    Text = '8'
  end
  object cmbParity: TComboBox
    Left = 8
    Height = 21
    Top = 158
    Width = 161
    ItemHeight = 13
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Odd'
      'Even'
      'Mark'
      'Space'
    )
    MaxLength = -1
    OnChange = cmbParityChange
    Style = csDropDownList
    TabOrder = 3
    Text = 'None'
  end
  object cmbStopBits: TComboBox
    Left = 8
    Height = 21
    Top = 202
    Width = 161
    ItemHeight = 13
    ItemIndex = 0
    Items.Strings = (
      '1'
      '1,5'
      '2'
    )
    MaxLength = -1
    OnChange = cmbStopBitsChange
    Style = csDropDownList
    TabOrder = 4
    Text = '1'
  end
  object cmbHandShaking: TComboBox
    Left = 8
    Height = 21
    Top = 249
    Width = 161
    ItemHeight = 13
    ItemIndex = 0
    Items.Strings = (
      'Nenhum'
      'XON/XOFF'
      'RTS/CTS'
      'DTR/DSR'
    )
    MaxLength = -1
    OnChange = cmbHandShakingChange
    Style = csDropDownList
    TabOrder = 5
    Text = 'Nenhum'
  end
  object cmbPortaSerial: TComboBox
    Left = 8
    Height = 21
    Top = 27
    Width = 161
    DropDownCount = 10
    ItemHeight = 13
    ItemIndex = 0
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'COM6'
      'COM7'
      'COM8'
      'COM9'
    )
    MaxLength = -1
    OnChange = cmbPortaSerialChange
    TabOrder = 0
    Text = 'COM1'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Height = 25
    Top = 312
    Width = 75
    Caption = '&OK'
    Default = True
    Kind = bkOK
    ModalResult = 1
    NumGlyphs = 0
    TabOrder = 6
  end
  object BitBtn2: TBitBtn
    Left = 94
    Height = 25
    Top = 312
    Width = 75
    Cancel = True
    Caption = 'Cancelar'
    Kind = bkCancel
    ModalResult = 2
    NumGlyphs = 0
    TabOrder = 7
  end
  object chHardFlow: TCheckBox
    Left = 8
    Height = 19
    Top = 280
    Width = 71
    Caption = 'HardFlow'
    OnClick = chHardFlowClick
    TabOrder = 8
  end
  object chSoftFlow: TCheckBox
    Left = 96
    Height = 19
    Top = 280
    Width = 67
    Caption = 'SoftFlow'
    OnClick = chSoftFlowClick
    TabOrder = 9
  end
end
