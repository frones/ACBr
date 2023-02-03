object Form1: TForm1
  Left = 270
  Top = 172
  ActiveControl = btnConectar
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label2: TLabel
    Left = 208
    Top = 135
    Width = 77
    Height = 13
    Caption = 'Ultima Resposta'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 208
    Top = 90
    Width = 81
    Height = 13
    Caption = 'Ultimo Peso Lido:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label9: TLabel
    Left = 208
    Top = 288
    Width = 40
    Height = 13
    Caption = 'TimeOut'
    Color = clBtnFace
    ParentColor = False
  end
  object Label10: TLabel
    Left = 208
    Top = 188
    Width = 51
    Height = 13
    Caption = 'Mensagem'
    Color = clBtnFace
    ParentColor = False
  end
  object Label12: TLabel
    Left = 297
    Top = 288
    Width = 42
    Height = 13
    Alignment = taRightJustify
    Caption = 'Arq.Log:'
    Color = clBtnFace
    ParentColor = False
  end
  object SbArqLog: TSpeedButton
    Left = 417
    Top = 304
    Width = 24
    Height = 22
    Caption = '...'
    OnClick = SbArqLogClick
  end
  object btnDesconectar: TButton
    Left = 203
    Top = 16
    Width = 110
    Height = 25
    Caption = 'Desativar'
    Enabled = False
    TabOrder = 1
    Visible = False
    OnClick = btnDesconectarClick
  end
  object btnConectar: TButton
    Left = 203
    Top = 16
    Width = 110
    Height = 25
    Caption = 'Ativar'
    TabOrder = 0
    OnClick = btnConectarClick
  end
  object btnLerPeso: TButton
    Left = 203
    Top = 48
    Width = 110
    Height = 25
    Caption = 'Ler Peso'
    Enabled = False
    TabOrder = 2
    OnClick = btnLerPesoClick
  end
  object sttPeso: TStaticText
    Left = 208
    Top = 105
    Width = 233
    Height = 24
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
  end
  object sttResposta: TStaticText
    Left = 208
    Top = 152
    Width = 233
    Height = 32
    AutoSize = False
    TabOrder = 8
  end
  object edtTimeOut: TEdit
    Left = 208
    Top = 304
    Width = 73
    Height = 21
    TabOrder = 3
    Text = '2000'
    OnKeyPress = edtTimeOutKeyPress
  end
  object chbMonitorar: TCheckBox
    Left = 208
    Top = 259
    Width = 126
    Height = 19
    Caption = 'Monitorar a Balan'#231'a'
    TabOrder = 4
    OnClick = chbMonitorarClick
  end
  object Memo1: TMemo
    Left = 208
    Top = 208
    Width = 233
    Height = 42
    TabOrder = 5
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 177
    Height = 321
    BevelOuter = bvLowered
    TabOrder = 6
    object Label1: TLabel
      Left = 16
      Top = 6
      Width = 37
      Height = 13
      Caption = 'Balanca'
      Color = clBtnFace
      ParentColor = False
    end
    object Label4: TLabel
      Left = 16
      Top = 49
      Width = 55
      Height = 13
      Caption = 'Porta Serial'
      Color = clBtnFace
      ParentColor = False
    end
    object Label5: TLabel
      Left = 16
      Top = 92
      Width = 47
      Height = 13
      Caption = 'Baud rate'
      Color = clBtnFace
      ParentColor = False
    end
    object Label6: TLabel
      Left = 16
      Top = 137
      Width = 43
      Height = 13
      Caption = 'Data Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object Label7: TLabel
      Left = 16
      Top = 181
      Width = 28
      Height = 13
      Caption = 'Parity'
      Color = clBtnFace
      ParentColor = False
    end
    object Label8: TLabel
      Left = 16
      Top = 271
      Width = 61
      Height = 13
      Caption = 'Handshaking'
      Color = clBtnFace
      ParentColor = False
    end
    object Label11: TLabel
      Left = 16
      Top = 224
      Width = 42
      Height = 13
      Caption = 'Stop Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object btSearchPorts: TSpeedButton
      Left = 131
      Top = 65
      Width = 30
      Height = 21
      Hint = 'Procurar Impressoras USB'
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000005000000A6000000EF00000031000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000005000000AC000000FF000000FF00000053000000000000
        000000000000000000010000003F0000008E000000B0000000AA0000007B0000
        002100000005000000AC000000FF000000FF0000008C00000001000000000000
        000000000019000000BB000000FF000000FF000000FF000000FF000000FF0000
        00FB000000CF000000FF000000FF0000008C0000000100000000000000000000
        0013000000DB000000FF000000FF000000D20000009C000000A5000000E90000
        00FF000000FF000000FF0000008C000000010000000000000000000000000000
        009F000000FF000000FB0000005E0000000100000000000000000000000A0000
        009D000000FF000000FF00000053000000000000000000000000000000180000
        00FC000000FF0000007100000000000000000000000000000000000000000000
        0002000000C0000000FF000000C8000000000000000000000000000000580000
        00FF000000F70000000900000000000000000000000000000000000000000000
        000000000051000000FF000000FF0000000D00000000000000000000006F0000
        00FF000000DB0000000000000000000000000000000000000000000000000000
        00000000002C000000FF000000FF0000002200000000000000000000005E0000
        00FF000000F20000000400000000000000000000000000000000000000000000
        000000000046000000FF000000FF000000110000000000000000000000220000
        00FF000000FF0000005800000000000000000000000000000000000000000000
        0000000000A8000000FF000000D4000000000000000000000000000000000000
        00B5000000FF000000F000000039000000000000000000000000000000010000
        0075000000FF000000FF00000068000000000000000000000000000000000000
        0022000000EE000000FF000000FC000000AA000000730000007B000000C80000
        00FF000000FF000000BF00000003000000000000000000000000000000000000
        00000000002F000000DC000000FF000000FF000000FF000000FF000000FF0000
        00FF000000B00000000E00000000000000000000000000000000000000000000
        0000000000000000000900000068000000B7000000D9000000D4000000A50000
        0046000000010000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      ParentShowHint = False
      ShowHint = True
      OnClick = btSearchPortsClick
    end
    object cmbBalanca: TComboBox
      Left = 16
      Top = 22
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Nenhuma'
      Items.Strings = (
        'Nenhuma'
        'Filizola'
        'Toledo')
    end
    object cmbPortaSerial: TComboBox
      Left = 16
      Top = 65
      Width = 113
      Height = 21
      TabOrder = 1
    end
    object cmbBaudRate: TComboBox
      Left = 16
      Top = 110
      Width = 145
      Height = 21
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
      Height = 21
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
      Height = 21
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
      Height = 21
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
      Height = 21
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
  object edLog: TEdit
    Left = 294
    Top = 304
    Width = 122
    Height = 21
    Cursor = crIBeam
    TabOrder = 9
    Text = 'BalLog.txt'
  end
  object edPrecoKg: TEdit
    Left = 331
    Top = 16
    Width = 110
    Height = 21
    Enabled = False
    TabOrder = 10
    Text = '9,99'
  end
  object btEnviarPrecoKg: TButton
    Left = 331
    Top = 48
    Width = 110
    Height = 25
    Caption = 'Enviar Pre'#231'o/Kg'
    Enabled = False
    TabOrder = 11
    OnClick = btEnviarPrecoKgClick
  end
  object ACBrBAL1: TACBrBAL
    Porta = 'COM1'
    OnLePeso = ACBrBAL1LePeso
    Left = 416
    Top = 56
  end
end
