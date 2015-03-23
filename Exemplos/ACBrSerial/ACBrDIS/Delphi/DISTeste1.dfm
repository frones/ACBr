object Form1: TForm1
  Left = 328
  Top = 132
  Width = 463
  Height = 332
  HorzScrollBar.Range = 428
  VertScrollBar.Range = 274
  ActiveControl = edLinha1
  AutoScroll = False
  Caption = 'Teste de Display Cliente'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 236
    Width = 25
    Height = 13
    Caption = '&Porta'
    FocusControl = cbxPorta
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 88
    Top = 236
    Width = 35
    Height = 13
    Caption = '&Modelo'
    FocusControl = cbxModelo
  end
  object Label3: TLabel
    Left = 240
    Top = 236
    Width = 41
    Height = 13
    Caption = '&Intervalo'
    FocusControl = edIntervalo
  end
  object Label4: TLabel
    Left = 296
    Top = 236
    Width = 34
    Height = 13
    Caption = 'P&assos'
    FocusControl = edPassos
  end
  object Label5: TLabel
    Left = 359
    Top = 245
    Width = 34
    Height = 13
    Caption = 'Linhas:'
  end
  object Label6: TLabel
    Left = 355
    Top = 261
    Width = 38
    Height = 13
    Caption = 'Colunas'
  end
  object lLinhas: TLabel
    Left = 404
    Top = 245
    Width = 8
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lColunas: TLabel
    Left = 404
    Top = 261
    Width = 8
    Height = 13
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lLinha1: TLabel
    Left = 24
    Top = 80
    Width = 385
    Height = 15
    AutoSize = False
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Fixedsys'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lLinha2: TLabel
    Left = 24
    Top = 104
    Width = 385
    Height = 15
    AutoSize = False
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Fixedsys'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label9: TLabel
    Left = 8
    Top = 144
    Width = 58
    Height = 13
    Caption = '&Alinhamento'
    FocusControl = cbxAlinhamento
  end
  object Label10: TLabel
    Left = 134
    Top = 144
    Width = 55
    Height = 13
    Caption = 'Efeito Exibir'
  end
  object Label11: TLabel
    Left = 288
    Top = 144
    Width = 55
    Height = 13
    Caption = 'Efeito Rolar'
  end
  object Label7: TLabel
    Left = 24
    Top = 119
    Width = 385
    Height = 15
    AutoSize = False
    Caption = '....+....1....+....2....+....3....+....4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 13
    Font.Name = 'Fixedsys'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edLinha1: TEdit
    Left = 24
    Top = 16
    Width = 241
    Height = 21
    Cursor = crIBeam
    TabOrder = 1
    Text = 'DANIEL SIMOES DE ALMEIDA'
  end
  object edLinha2: TEdit
    Left = 24
    Top = 48
    Width = 241
    Height = 21
    Cursor = crIBeam
    TabOrder = 3
    Text = 'D.J. SYSTEM'
  end
  object cbxPorta: TComboBox
    Left = 16
    Top = 252
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 14
    Text = 'COM1'
    OnChange = cbxPortaChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5')
  end
  object cbxModelo: TComboBox
    Left = 88
    Top = 252
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 15
    OnChange = cbxModeloChange
    Items.Strings = (
      'disNenhum'
      'disGertecSerial'
      'disGertecTeclado'
      'disKeytecTeclado')
  end
  object edIntervalo: TEdit
    Left = 237
    Top = 252
    Width = 49
    Height = 21
    Cursor = crIBeam
    TabOrder = 16
    OnChange = edIntervaloChange
  end
  object edPassos: TEdit
    Left = 296
    Top = 252
    Width = 41
    Height = 21
    Cursor = crIBeam
    TabOrder = 17
    OnChange = edPassosChange
  end
  object bDemo: TButton
    Left = 272
    Top = 12
    Width = 89
    Height = 25
    Caption = '&Demonstra'#231#227'o'
    TabOrder = 4
    OnClick = bDemoClick
  end
  object bLimpar: TButton
    Left = 272
    Top = 44
    Width = 89
    Height = 25
    Caption = '&Limpar Display'
    TabOrder = 5
    OnClick = bLimparClick
  end
  object bParar: TButton
    Left = 368
    Top = 12
    Width = 57
    Height = 25
    Caption = 'Para&r'
    TabOrder = 6
    OnClick = bPararClick
  end
  object bContinuar: TButton
    Left = 368
    Top = 44
    Width = 57
    Height = 25
    Caption = '&Continuar'
    TabOrder = 7
    OnClick = bContinuarClick
  end
  object cbxAlinhamento: TComboBox
    Left = 8
    Top = 160
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 8
    OnChange = cbxAlinhamentoChange
    Items.Strings = (
      'Esquerda'
      'Direita'
      'Centro'
      'Justificado')
  end
  object cbxExibirEfeito: TComboBox
    Left = 134
    Top = 160
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 10
    Items.Strings = (
      'Esquerda para Direita'
      'Direita para  Esquerda ')
  end
  object cbxRolarEfeito: TComboBox
    Left = 288
    Top = 160
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
    Items.Strings = (
      'rolParaEsquerda_Inicio'
      'rolParaEsquerda_Sempre'
      'rolParaDireita_Inicio'
      'rolParaDireita_Sempre'
      'rolVai_e_Volta')
  end
  object cbLinha1: TCheckBox
    Left = 8
    Top = 14
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbLinha1Click
  end
  object cbLinha2: TCheckBox
    Left = 8
    Top = 46
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = cbLinha1Click
  end
  object bExibir: TButton
    Left = 29
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Exibir Normal'
    TabOrder = 9
    OnClick = bExibirClick
  end
  object bExibirEfeito: TButton
    Left = 163
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Exibir Efeito'
    TabOrder = 11
    OnClick = bExibirEfeitoClick
  end
  object bRolar: TButton
    Left = 320
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Rolar'
    TabOrder = 13
    OnClick = bRolarClick
  end
  object ACBrDIS1: TACBrDIS
    Porta = 'COM1'
    Intervalo = 200
    OnAtualiza = ACBrDIS1Atualiza
    Left = 200
    Top = 88
  end
end
