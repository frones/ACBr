object frVendeItem: TfrVendeItem
  Left = 304
  Top = 154
  Width = 466
  Height = 407
  HorzScrollBar.Range = 329
  VertScrollBar.Range = 225
  BorderStyle = bsSingle
  Caption = 'Vende Item'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 19
    Width = 33
    Height = 13
    Alignment = taRightJustify
    Caption = '&C'#243'digo'
    FocusControl = edCodigo
  end
  object Label2: TLabel
    Left = 27
    Top = 46
    Width = 48
    Height = 13
    Alignment = taRightJustify
    Caption = '&Descri'#231#227'o'
    FocusControl = edDescricao
  end
  object Label3: TLabel
    Left = 53
    Top = 73
    Width = 17
    Height = 13
    Alignment = taRightJustify
    Caption = '&Qtd'
  end
  object Label4: TLabel
    Left = 12
    Top = 100
    Width = 67
    Height = 13
    Alignment = taRightJustify
    Caption = '&Pre'#231'o Unit'#225'rio'
    FocusControl = edPrecoUnita
  end
  object Label5: TLabel
    Left = 275
    Top = 159
    Width = 76
    Height = 13
    Alignment = taRightJustify
    Caption = '&Valor Aliq. ICMS'
    FocusControl = edICMS
  end
  object Label6: TLabel
    Left = 93
    Top = 159
    Width = 46
    Height = 13
    Alignment = taRightJustify
    Caption = '&Desconto'
    FocusControl = edDesconto
  end
  object Label7: TLabel
    Left = 33
    Top = 127
    Width = 40
    Height = 13
    Alignment = taRightJustify
    Caption = '&Unidade'
    FocusControl = edUN
  end
  object Bevel1: TBevel
    Left = 82
    Top = 146
    Width = 169
    Height = 76
  end
  object Bevel2: TBevel
    Left = 257
    Top = 146
    Width = 193
    Height = 142
  end
  object Label8: TLabel
    Left = 273
    Top = 182
    Width = 47
    Height = 13
    Caption = 'II = Isento'
  end
  object Label9: TLabel
    Left = 273
    Top = 198
    Width = 100
    Height = 13
    Caption = 'NN = Nao Incidencia'
  end
  object Label10: TLabel
    Left = 273
    Top = 214
    Width = 129
    Height = 13
    Caption = 'FF = Substitui'#231'ao Tribut'#225'ria'
  end
  object Label11: TLabel
    Left = 273
    Top = 230
    Width = 87
    Height = 13
    Caption = 'SI = Isento ISSQN'
  end
  object Label12: TLabel
    Left = 273
    Top = 246
    Width = 135
    Height = 13
    Caption = 'SN = Nao Incidencia ISSQN'
  end
  object Label13: TLabel
    Left = 273
    Top = 262
    Width = 135
    Height = 13
    Caption = 'SF = Subst.Tribut'#225'ria ISSQN'
  end
  object edCodigo: TEdit
    Left = 82
    Top = 11
    Width = 101
    Height = 21
    Cursor = crIBeam
    TabOrder = 0
    Text = '111222333'
  end
  object edDescricao: TEdit
    Left = 82
    Top = 38
    Width = 369
    Height = 21
    Cursor = crIBeam
    TabOrder = 1
    Text = 'TESTE DE PRODUTO'
  end
  object edPrecoUnita: TEdit
    Left = 83
    Top = 92
    Width = 89
    Height = 21
    Cursor = crIBeam
    TabOrder = 3
    Text = '1'
    OnKeyPress = edQtdKeyPress
  end
  object edICMS: TEdit
    Left = 356
    Top = 151
    Width = 89
    Height = 21
    Cursor = crIBeam
    TabOrder = 9
    Text = 'NN'
    OnKeyPress = edQtdKeyPress
  end
  object edDesconto: TEdit
    Left = 146
    Top = 151
    Width = 81
    Height = 21
    Cursor = crIBeam
    TabOrder = 6
    Text = '0'
    OnKeyPress = edQtdKeyPress
  end
  object edUN: TEdit
    Left = 83
    Top = 119
    Width = 89
    Height = 21
    Cursor = crIBeam
    TabOrder = 4
    Text = 'UN'
  end
  object Button1: TButton
    Left = 143
    Top = 328
    Width = 75
    Height = 25
    Caption = '&Imprimir'
    Default = True
    TabOrder = 10
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 226
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Fechar'
    ModalResult = 2
    TabOrder = 11
    OnClick = Button2Click
  end
  object edQtd: TEdit
    Left = 82
    Top = 65
    Width = 90
    Height = 21
    Cursor = crIBeam
    TabOrder = 2
    Text = '1'
    OnKeyPress = edQtdKeyPress
  end
  object rbPercentagem: TRadioButton
    Left = 122
    Top = 183
    Width = 105
    Height = 17
    Caption = 'Percentagem'
    Checked = True
    TabOrder = 7
    TabStop = True
  end
  object rbValor: TRadioButton
    Left = 122
    Top = 201
    Width = 105
    Height = 17
    Caption = 'Valor'
    TabOrder = 8
  end
  object ckInmetro: TCheckBox
    Left = 178
    Top = 123
    Width = 130
    Height = 17
    Caption = 'utilizar legenda inmetro'
    TabOrder = 5
  end
end
