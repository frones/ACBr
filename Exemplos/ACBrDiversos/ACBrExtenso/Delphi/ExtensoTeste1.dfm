object frExtenso: TfrExtenso
  Left = 545
  Top = 267
  Width = 353
  Height = 384
  HorzScrollBar.Range = 225
  VertScrollBar.Range = 121
  Caption = 'Numeros por Extenso'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    337
    345)
  TextHeight = 13
  object lbValor: TLabel
    Left = 15
    Top = 155
    Width = 63
    Height = 13
    Caption = 'Digite o Valor'
  end
  object btExtenso: TButton
    Left = 176
    Top = 168
    Width = 145
    Height = 25
    Caption = 'Extenso'
    Default = True
    TabOrder = 0
    OnClick = btExtensoClick
  end
  object mExtenso: TMemo
    Left = 15
    Top = 218
    Width = 306
    Height = 112
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 1
  end
  object pnCabecalho: TPanel
    Left = 0
    Top = 0
    Width = 337
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lbFormato: TLabel
      Left = 175
      Top = 16
      Width = 41
      Height = 13
      Caption = 'Formato:'
    end
    object lbIdioma: TLabel
      Left = 15
      Top = 16
      Width = 31
      Height = 13
      Caption = 'Idioma'
    end
    object cbFormato: TComboBox
      Left = 175
      Top = 30
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbFormatoChange
    end
    object cbIdioma: TComboBox
      Left = 15
      Top = 30
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbIdiomaChange
    end
  end
  object edValor: TEdit
    Left = 16
    Top = 170
    Width = 145
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '123456,12'
    OnKeyPress = edValorKeyPress
  end
  object cbZeroAEsquerda: TCheckBox
    Left = 16
    Top = 192
    Width = 99
    Height = 19
    Caption = 'Zero a Esquerda'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbZeroAEsquerdaClick
  end
  object pnCustomStr: TPanel
    Left = 0
    Top = 51
    Width = 337
    Height = 92
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    object lbInteiroSingular: TLabel
      Left = 15
      Top = 12
      Width = 70
      Height = 13
      Caption = 'Inteiro Singular'
    end
    object lbInteiroPlural: TLabel
      Left = 175
      Top = 12
      Width = 58
      Height = 13
      Caption = 'Inteiro Plural'
    end
    object lbDecimalSingular: TLabel
      Left = 15
      Top = 56
      Width = 79
      Height = 13
      Caption = 'Decimal Singular'
    end
    object lbDecimalPlural: TLabel
      Left = 175
      Top = 56
      Width = 67
      Height = 13
      Caption = 'Decimal Plural'
    end
    object edInteiroSingular: TEdit
      Left = 15
      Top = 27
      Width = 145
      Height = 21
      TabOrder = 0
    end
    object edInteiroPlural: TEdit
      Left = 175
      Top = 27
      Width = 145
      Height = 21
      TabOrder = 1
    end
    object edDecimalSingular: TEdit
      Left = 15
      Top = 71
      Width = 145
      Height = 21
      TabOrder = 2
    end
    object edDecimalPlural: TEdit
      Left = 175
      Top = 71
      Width = 145
      Height = 21
      TabOrder = 3
    end
  end
  object ACBrExtenso1: TACBrExtenso
    Left = 40
    Top = 240
  end
end
