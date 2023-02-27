object Form1: TForm1
  Left = 369
  Top = 172
  Width = 604
  Height = 382
  AutoSize = True
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnConfig: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 343
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 0
    object Label8: TLabel
      Left = 21
      Top = 282
      Width = 61
      Height = 13
      Caption = 'Handshaking'
      Color = clBtnFace
      ParentColor = False
    end
    object Label7: TLabel
      Left = 21
      Top = 192
      Width = 28
      Height = 13
      Caption = 'Parity'
      Color = clBtnFace
      ParentColor = False
    end
    object Label6: TLabel
      Left = 21
      Top = 148
      Width = 43
      Height = 13
      Caption = 'Data Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object Label5: TLabel
      Left = 21
      Top = 103
      Width = 47
      Height = 13
      Caption = 'Baud rate'
      Color = clBtnFace
      ParentColor = False
    end
    object Label4: TLabel
      Left = 21
      Top = 60
      Width = 55
      Height = 13
      Caption = 'Porta Serial'
      Color = clBtnFace
      ParentColor = False
    end
    object Label11: TLabel
      Left = 21
      Top = 235
      Width = 42
      Height = 13
      Caption = 'Stop Bits'
      Color = clBtnFace
      ParentColor = False
    end
    object Label1: TLabel
      Left = 21
      Top = 17
      Width = 37
      Height = 13
      Caption = 'Balanca'
      Color = clBtnFace
      ParentColor = False
    end
    object btSearchPorts: TSpeedButton
      Left = 133
      Top = 75
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
    object cmbStopBits: TComboBox
      Left = 21
      Top = 253
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 's1'
      Items.Strings = (
        's1'
        's1,5'
        's2'
        '')
    end
    object cmbPortaSerial: TComboBox
      Left = 21
      Top = 76
      Width = 111
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'COM1'
        'COM2'
        'COM3'
        'COM4'
        'TCP:192.168.0.10:9600')
    end
    object cmbParity: TComboBox
      Left = 21
      Top = 209
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'none'
      Items.Strings = (
        'none'
        'odd'
        'even'
        'mark'
        'space')
    end
    object cmbHandShaking: TComboBox
      Left = 21
      Top = 300
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'Nenhum'
      Items.Strings = (
        'Nenhum'
        'XON/XOFF'
        'RTS/CTS'
        'DTR/DSR')
    end
    object cmbDataBits: TComboBox
      Left = 21
      Top = 164
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 4
      Text = '8'
      Items.Strings = (
        '5'
        '6'
        '7'
        '8')
    end
    object cmbBalanca: TComboBox
      Left = 21
      Top = 33
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = 'Nenhuma'
      Items.Strings = (
        'Nenhuma'
        'Filizola'
        'Toledo')
    end
    object cmbBaudRate: TComboBox
      Left = 21
      Top = 121
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 6
      TabOrder = 6
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
  end
  object pnOpcoes: TPanel
    Left = 185
    Top = 0
    Width = 273
    Height = 343
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 20
      Top = 138
      Width = 77
      Height = 13
      Caption = 'Ultima Resposta'
      Color = clBtnFace
      ParentColor = False
    end
    object Label3: TLabel
      Left = 20
      Top = 93
      Width = 81
      Height = 13
      Caption = 'Ultimo Peso Lido:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label9: TLabel
      Left = 20
      Top = 291
      Width = 40
      Height = 13
      Caption = 'TimeOut'
      Color = clBtnFace
      ParentColor = False
    end
    object Label10: TLabel
      Left = 20
      Top = 191
      Width = 51
      Height = 13
      Caption = 'Mensagem'
      Color = clBtnFace
      ParentColor = False
    end
    object Label12: TLabel
      Left = 109
      Top = 291
      Width = 42
      Height = 13
      Alignment = taRightJustify
      Caption = 'Arq.Log:'
      Color = clBtnFace
      ParentColor = False
    end
    object SbArqLog: TSpeedButton
      Left = 229
      Top = 307
      Width = 24
      Height = 22
      Caption = '...'
      OnClick = SbArqLogClick
    end
    object btnDesconectar: TButton
      Left = 19
      Top = 24
      Width = 110
      Height = 25
      Caption = 'Desativar'
      Enabled = False
      TabOrder = 0
      Visible = False
      OnClick = btnDesconectarClick
    end
    object btnConectar: TButton
      Left = 19
      Top = 24
      Width = 110
      Height = 25
      Caption = 'Ativar'
      TabOrder = 1
      OnClick = btnConectarClick
    end
    object btnLerPeso: TButton
      Left = 19
      Top = 56
      Width = 110
      Height = 25
      Caption = 'Ler Peso'
      Enabled = False
      TabOrder = 2
      OnClick = btnLerPesoClick
    end
    object edPrecoKg: TEdit
      Left = 147
      Top = 24
      Width = 110
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '9,99'
    end
    object btEnviarPrecoKg: TButton
      Left = 147
      Top = 56
      Width = 110
      Height = 25
      Caption = 'Enviar Pre'#231'o/Kg'
      Enabled = False
      TabOrder = 4
      OnClick = btEnviarPrecoKgClick
    end
    object sttPeso: TStaticText
      Left = 20
      Top = 108
      Width = 233
      Height = 24
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
    end
    object sttResposta: TStaticText
      Left = 20
      Top = 155
      Width = 233
      Height = 32
      AutoSize = False
      TabOrder = 6
    end
    object edtTimeOut: TEdit
      Left = 20
      Top = 307
      Width = 73
      Height = 21
      TabOrder = 7
      Text = '2000'
      OnKeyPress = edtTimeOutKeyPress
    end
    object chbMonitorar: TCheckBox
      Left = 20
      Top = 262
      Width = 126
      Height = 19
      Caption = 'Monitorar a Balan'#231'a'
      TabOrder = 8
      OnClick = chbMonitorarClick
    end
    object Memo1: TMemo
      Left = 20
      Top = 211
      Width = 233
      Height = 42
      TabOrder = 9
    end
    object edLog: TEdit
      Left = 106
      Top = 307
      Width = 122
      Height = 21
      Cursor = crIBeam
      TabOrder = 10
      Text = 'BalLog.txt'
    end
  end
  object pnOpcoesAdicionais: TPanel
    Left = 458
    Top = 0
    Width = 130
    Height = 343
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object btAtivarTara: TButton
      Left = 7
      Top = 22
      Width = 112
      Height = 25
      Caption = 'Ativar Tara'
      TabOrder = 0
      OnClick = btAtivarTaraClick
    end
    object btZerarDispositivo: TButton
      Left = 7
      Top = 91
      Width = 112
      Height = 25
      Caption = 'Zerar Dispositivo'
      TabOrder = 1
      OnClick = btZerarDispositivoClick
    end
    object btLigarDisplay: TButton
      Left = 7
      Top = 125
      Width = 112
      Height = 25
      Caption = 'Ligar Display'
      TabOrder = 2
      OnClick = btLigarDisplayClick
    end
    object btDesativarTara: TButton
      Left = 7
      Top = 56
      Width = 112
      Height = 25
      Caption = 'Desativar Tara'
      TabOrder = 3
      OnClick = btDesativarTaraClick
    end
    object btDesligarDisplay: TButton
      Left = 7
      Top = 158
      Width = 112
      Height = 25
      Caption = 'Desligar Display'
      TabOrder = 4
      OnClick = btDesligarDisplayClick
    end
  end
  object ACBrBAL1: TACBrBAL
    Porta = 'COM1'
    OnLePeso = ACBrBAL1LePeso
    Left = 104
    Top = 16
  end
end
