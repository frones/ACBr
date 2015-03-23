object Form1: TForm1
  Left = 311
  Top = 155
  Width = 352
  Height = 329
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Valor'
  end
  object Label2: TLabel
    Left = 16
    Top = 88
    Width = 62
    Height = 13
    Caption = 'Texto a Falar'
  end
  object sbDir: TSpeedButton
    Left = 288
    Top = 240
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5555555555555555555555555555555555555555555555555555555555555555
      555555555555555555555555555555555555555FFFFFFFFFF555550000000000
      55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
      B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
      000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
      555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
      55555575FFF75555555555700007555555555557777555555555555555555555
      5555555555555555555555555555555555555555555555555555}
    NumGlyphs = 2
    OnClick = sbDirClick
  end
  object SpeedButton1: TSpeedButton
    Left = 288
    Top = 208
    Width = 23
    Height = 22
    Hint = 'Como usar o ACBrFala ?'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333F797F3333333333F737373FF333333BFB999BFB
      33333337737773773F3333BFBF797FBFB33333733337333373F33BFBFBFBFBFB
      FB3337F33333F33337F33FBFBFB9BFBFBF3337333337F333373FFBFBFBF97BFB
      FBF37F333337FF33337FBFBFBFB99FBFBFB37F3333377FF3337FFBFBFBFB99FB
      FBF37F33333377FF337FBFBF77BF799FBFB37F333FF3377F337FFBFB99FB799B
      FBF373F377F3377F33733FBF997F799FBF3337F377FFF77337F33BFBF99999FB
      FB33373F37777733373333BFBF999FBFB3333373FF77733F7333333BFBFBFBFB
      3333333773FFFF77333333333FBFBF3333333333377777333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object edValor: TEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = edValorChange
    OnExit = edValorExit
    OnKeyPress = edValorKeyPress
  end
  object mFalar: TMemo
    Left = 16
    Top = 104
    Width = 289
    Height = 89
    TabOrder = 1
  end
  object bFalar: TButton
    Left = 240
    Top = 24
    Width = 59
    Height = 25
    Caption = 'Falar'
    TabOrder = 2
    OnClick = bFalarClick
  end
  object chPromo: TCheckBox
    Left = 16
    Top = 56
    Width = 97
    Height = 17
    Caption = 'Promo'#231#227'o'
    TabOrder = 3
    OnClick = chPromoClick
  end
  object chNaoDisp: TCheckBox
    Left = 152
    Top = 56
    Width = 153
    Height = 17
    Caption = 'Pre'#231'o n'#227'o disponivel'
    TabOrder = 4
    OnClick = chNaoDispClick
  end
  object rbDir: TRadioButton
    Left = 24
    Top = 208
    Width = 113
    Height = 17
    Caption = 'Diret'#243'rio'
    TabOrder = 5
    OnClick = rbDirClick
  end
  object rbResource: TRadioButton
    Left = 160
    Top = 208
    Width = 113
    Height = 17
    Caption = 'Resource'
    TabOrder = 6
    OnClick = rbDirClick
  end
  object edDir: TEdit
    Left = 24
    Top = 240
    Width = 257
    Height = 21
    TabOrder = 7
    OnExit = edDirExit
  end
  object chDinheiro: TCheckBox
    Left = 152
    Top = 26
    Width = 81
    Height = 20
    Caption = 'Dinheiro'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chDinheiroClick
  end
  object ACBrFala1: TACBrFala
    OrigemArquivos = '..\..\..\Fontes\ACBrDiversos\ACBrFalaWaves'
    LocalSons = lsRecurso
    ExtensaoSons = '.wav'
    CharsSeparadores = ' ,;|'
    LinuxComando = 'play'
    Left = 136
    Top = 128
  end
end
