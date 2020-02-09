object Form1: TForm1
  Left = 306
  Top = 296
  Width = 570
  Height = 337
  HorzScrollBar.Range = 505
  VertScrollBar.Range = 241
  ActiveControl = mProdutos
  AutoScroll = False
  Caption = 'Teste de Leitor de C'#243'digo de Barras Serial'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 67
    Height = 13
    Caption = #218'ltima Leitura:'
  end
  object Label3: TLabel
    Left = 222
    Top = 199
    Width = 62
    Height = 13
    Caption = 'Interv.Leitura'
  end
  object Label4: TLabel
    Left = 8
    Top = 199
    Width = 25
    Height = 13
    Caption = 'Porta'
  end
  object Label5: TLabel
    Left = 152
    Top = 199
    Width = 29
    Height = 13
    Caption = 'Sufixo'
  end
  object Label6: TLabel
    Left = 352
    Top = 32
    Width = 80
    Height = 13
    Caption = 'Conteudo da Fila'
  end
  object Label2: TLabel
    Left = 374
    Top = 199
    Width = 132
    Height = 13
    Caption = 'Simular Atraso na Aplica'#231#227'o'
  end
  object lUltimaLeitura: TLabel
    Left = 80
    Top = 8
    Width = 449
    Height = 20
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 80
    Top = 199
    Width = 25
    Height = 13
    Caption = 'Baud'
  end
  object Label8: TLabel
    Left = 8
    Top = 247
    Width = 23
    Height = 13
    Caption = 'Data'
  end
  object Label9: TLabel
    Left = 249
    Top = 247
    Width = 57
    Height = 13
    Caption = 'HandShake'
  end
  object Label10: TLabel
    Left = 72
    Top = 247
    Width = 26
    Height = 13
    Caption = 'Parity'
  end
  object Label11: TLabel
    Left = 352
    Top = 248
    Width = 22
    Height = 13
    Caption = 'Stop'
  end
  object mProdutos: TMemo
    Left = 8
    Top = 40
    Width = 337
    Height = 145
    TabStop = False
    Lines.Strings = (
      'SE NAO LEU O C'#211'DIGO DE BARRAS:'
      ''
      '1 - Verifique se a Porta est'#225' correta'
      '2 - Verifique se o Baud est'#225' correto'
      '3 - Experimente deixar o SUFIXO vazio. Ap'#243's uma leitura '
      'verifique o Sufixo correto no final de "Ultima Leitura" '
      'IMPORTANTE: Informar o sufixo correto para o '
      'componente '#233' importante para ler corretamente um c'#243'digo de '
      'cada vez.')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object cbFila: TCheckBox
    Left = 295
    Top = 203
    Width = 70
    Height = 17
    Caption = 'Usar Fila'
    TabOrder = 5
    OnClick = cbFilaClick
  end
  object edIntervalo: TEdit
    Left = 224
    Top = 215
    Width = 57
    Height = 21
    Cursor = crIBeam
    TabOrder = 4
    OnChange = edIntervaloChange
  end
  object cbxPorta: TComboBox
    Left = 8
    Top = 215
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'COM1'
    OnChange = cbxPortaChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'LPT1'
      'LPT2'
      'LPT3')
  end
  object edSufixo: TEdit
    Left = 152
    Top = 215
    Width = 65
    Height = 21
    Cursor = crIBeam
    TabOrder = 3
    OnChange = cbxPortaChange
  end
  object mFila: TMemo
    Left = 352
    Top = 48
    Width = 185
    Height = 113
    ScrollBars = ssVertical
    TabOrder = 18
  end
  object bEmulador: TButton
    Left = 488
    Top = 216
    Width = 57
    Height = 25
    Caption = 'Emulador'
    TabOrder = 15
    OnClick = bEmuladorClick
  end
  object cbExcluirSufixo: TCheckBox
    Left = 295
    Top = 224
    Width = 98
    Height = 17
    Caption = 'Excluir Sufixo'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = cbExcluirSufixoClick
  end
  object edAtraso: TEdit
    Left = 414
    Top = 215
    Width = 57
    Height = 21
    Cursor = crIBeam
    TabOrder = 14
    OnChange = edIntervaloChange
  end
  object pAtraso: TPanel
    Left = 188
    Top = 102
    Width = 185
    Height = 41
    Caption = 'Simulando Atraso (Impress'#227'o)'
    TabOrder = 19
    Visible = False
  end
  object bApagarFila: TButton
    Left = 456
    Top = 166
    Width = 51
    Height = 25
    Caption = 'Apagar'
    TabOrder = 17
    OnClick = bApagarFilaClick
  end
  object bLerFila: TButton
    Left = 392
    Top = 166
    Width = 51
    Height = 25
    Caption = 'Ler Fila'
    TabOrder = 16
    OnClick = bLerFilaClick
  end
  object cbxBaud: TComboBox
    Left = 80
    Top = 215
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = '9600'
    OnChange = cbxPortaChange
    Items.Strings = (
      '1200'
      '2400'
      '3600'
      '4800'
      '9600'
      '19200')
  end
  object EditData: TEdit
    Left = 8
    Top = 263
    Width = 49
    Height = 21
    Cursor = crIBeam
    TabOrder = 7
    Text = '8'
    OnChange = cbxPortaChange
  end
  object chbHard: TCheckBox
    Left = 167
    Top = 270
    Width = 68
    Height = 17
    Caption = 'HardFlow'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = chbHardClick
  end
  object chbSoft: TCheckBox
    Left = 167
    Top = 253
    Width = 67
    Height = 20
    Caption = 'SoftFlow'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = chbSoftClick
  end
  object cbxParidade: TComboBox
    Left = 72
    Top = 263
    Width = 81
    Height = 21
    ItemHeight = 13
    TabOrder = 8
    Text = 'pNone'
    OnChange = cbxPortaChange
    Items.Strings = (
      'pNone'
      'pOdd'
      'pEven'
      'pMark'
      'pSpace')
  end
  object cbxHandShake: TComboBox
    Left = 245
    Top = 263
    Width = 92
    Height = 21
    ItemHeight = 13
    TabOrder = 11
    Text = 'hsNenhum'
    OnChange = cbxHandShakeChange
    Items.Strings = (
      'hsNenhum'
      'hsXON_XOFF'
      'hsRTS_CTS'
      'hsDTR_DSR')
  end
  object cbxStop: TComboBox
    Left = 349
    Top = 263
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 12
    Text = 's1'
    OnChange = cbxPortaChange
    Items.Strings = (
      's1'
      's1eMeio'
      's2')
  end
  object bAtivar: TButton
    Left = 440
    Top = 250
    Width = 88
    Height = 33
    Caption = '&Ativar'
    TabOrder = 13
    OnClick = bAtivarClick
  end
  object ACBrLCB1: TACBrLCB
    Porta = 'COM2'
    Sufixo = '#13'
    Intervalo = 300
    OnLeCodigo = ACBrLCB1LeCodigo
    OnLeFila = ACBrLCB1LeFila
    Left = 48
    Top = 144
  end
end
