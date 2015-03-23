object frExtenso: TfrExtenso
  Left = 238
  Top = 107
  Width = 249
  Height = 228
  HorzScrollBar.Range = 225
  VertScrollBar.Range = 121
  ActiveControl = edValor
  AutoScroll = False
  Caption = 'Numeros por Extenso'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Digite o Valor'
  end
  object Label2: TLabel
    Left = 48
    Top = 168
    Width = 41
    Height = 13
    Caption = 'Formato:'
  end
  object edValor: TEdit
    Left = 8
    Top = 24
    Width = 101
    Height = 21
    TabOrder = 0
    Text = '123456,12'
    OnKeyPress = edValorKeyPress
  end
  object bExtenso: TButton
    Left = 144
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Extenso'
    Default = True
    TabOrder = 1
    OnClick = bExtensoClick
  end
  object mExtenso: TMemo
    Left = 8
    Top = 64
    Width = 217
    Height = 57
    ReadOnly = True
    TabOrder = 2
  end
  object cbZeroAEsquerda: TCheckBox
    Left = 8
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Zero a Esquerda'
    TabOrder = 3
    OnClick = cbZeroAEsquerdaClick
  end
  object ComboBox1: TComboBox
    Left = 96
    Top = 160
    Width = 129
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = ComboBox1Change
    Items.Strings = (
      'extPadrao'
      'extDolar')
  end
  object ACBrExtenso1: TACBrExtenso
    StrMoeda = 'Real'
    StrMoedas = 'Reais'
    StrCentavo = 'Centavo'
    StrCentavos = 'Centavos'
    Left = 48
    Top = 80
  end
end
