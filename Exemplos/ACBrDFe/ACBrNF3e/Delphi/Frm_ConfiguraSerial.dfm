object frmConfiguraSerial: TfrmConfiguraSerial
  Left = 322
  Top = 155
  ActiveControl = cmbPortaSerial
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Porta Serial'
  ClientHeight = 350
  ClientWidth = 187
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 180
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 52
    Width = 136
    Height = 13
    Caption = '&Baud rate (Bits por Segundo)'
    Color = clBtnFace
    FocusControl = cmbBaudRate
    ParentColor = False
  end
  object Label6: TLabel
    Left = 8
    Top = 97
    Width = 118
    Height = 13
    Caption = '&Data Bits (Bits de Dados)'
    Color = clBtnFace
    FocusControl = cmbDataBits
    ParentColor = False
  end
  object Label7: TLabel
    Left = 8
    Top = 141
    Width = 77
    Height = 13
    Caption = '&Parity (Paridade)'
    Color = clBtnFace
    FocusControl = cmbParity
    ParentColor = False
  end
  object Label11: TLabel
    Left = 8
    Top = 184
    Width = 120
    Height = 13
    Caption = '&Stop Bits (Bits de Parada)'
    Color = clBtnFace
    FocusControl = cmbStopBits
    ParentColor = False
  end
  object Label8: TLabel
    Left = 8
    Top = 231
    Width = 154
    Height = 13
    Caption = '&Handshaking (Controle de Fluxo)'
    Color = clBtnFace
    FocusControl = cmbHandShaking
    ParentColor = False
  end
  object Label4: TLabel
    Left = 8
    Top = 11
    Width = 54
    Height = 13
    Caption = '&Porta Serial'
    Color = clBtnFace
    FocusControl = cmbPortaSerial
    ParentColor = False
  end
  object cmbBaudRate: TComboBox
    Left = 8
    Top = 70
    Width = 161
    Height = 21
    TabOrder = 1
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
    ItemIndex = 3
    TabOrder = 2
    Text = '8'
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
    ItemIndex = 0
    TabOrder = 3
    Text = 'None'
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
    ItemIndex = 0
    TabOrder = 4
    Text = '1'
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
    ItemIndex = 0
    TabOrder = 5
    Text = 'Nenhum'
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
    ItemIndex = 0
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
    Caption = '&OK'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
  end
  object BitBtn2: TBitBtn
    Left = 94
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object chHardFlow: TCheckBox
    Left = 8
    Top = 280
    Width = 71
    Height = 19
    Caption = 'HardFlow'
    TabOrder = 8
    OnClick = chHardFlowClick
  end
  object chSoftFlow: TCheckBox
    Left = 96
    Top = 280
    Width = 67
    Height = 19
    Caption = 'SoftFlow'
    TabOrder = 9
    OnClick = chSoftFlowClick
  end
end
