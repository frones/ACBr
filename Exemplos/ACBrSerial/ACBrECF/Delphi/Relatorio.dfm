object frRelatorio: TfrRelatorio
  Left = 191
  Top = 107
  Width = 311
  Height = 363
  HorzScrollBar.Range = 298
  VertScrollBar.Range = 329
  ActiveControl = mRelat
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Relat'#243'rio Gerencial'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sbFPG: TSpeedButton
    Left = 128
    Top = 26
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33033333333333333F8F3333333333333000333333333333F888333333333333
      000333333333333F888333333333333000333333333333F88833333333333300
      033333333FFF3F888333333000003B803333333F8883F8883333330EEEEE00B3
      3333338833388883333330EEEEEEE033333338F3333338F333330EEEEEEEEE03
      33333833F333383F33330EFEEEEEEE0333338F33F333338F33330EFEEEEEEE03
      33338F333F33338F33330EEFEEEEEE03333383F333FF338333330EEEFFEEEE03
      333338F3333338F3333330EEEEEEE0333333383FF333F8333333330EEEEE0333
      333333883FF88333333333300000333333333333888333333333}
    NumGlyphs = 2
    Visible = False
    OnClick = sbFPGClick
  end
  object sbCNF: TSpeedButton
    Left = 192
    Top = 26
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33033333333333333F8F3333333333333000333333333333F888333333333333
      000333333333333F888333333333333000333333333333F88833333333333300
      033333333FFF3F888333333000003B803333333F8883F8883333330EEEEE00B3
      3333338833388883333330EEEEEEE033333338F3333338F333330EEEEEEEEE03
      33333833F333383F33330EFEEEEEEE0333338F33F333338F33330EFEEEEEEE03
      33338F333F33338F33330EEFEEEEEE03333383F333FF338333330EEEFFEEEE03
      333338F3333338F3333330EEEEEEE0333333383FF333F8333333330EEEEE0333
      333333883FF88333333333300000333333333333888333333333}
    NumGlyphs = 2
    Visible = False
    OnClick = sbCNFClick
  end
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 23
    Height = 13
    Caption = '&Vias:'
    FocusControl = edVias
  end
  object lCodFPG: TLabel
    Left = 96
    Top = 5
    Width = 54
    Height = 13
    Caption = '&Form.Pagto'
    FocusControl = lCodFPG
    Visible = False
  end
  object lValor: TLabel
    Left = 224
    Top = 5
    Width = 27
    Height = 13
    Caption = '&Valor:'
    FocusControl = edValor
    Visible = False
  end
  object lCupom: TLabel
    Left = 40
    Top = 5
    Width = 33
    Height = 13
    Caption = '&Cupom'
    FocusControl = edCupom
  end
  object lCodCNF: TLabel
    Left = 160
    Top = 5
    Width = 21
    Height = 13
    Caption = 'C&NF'
    FocusControl = lCodCNF
    Visible = False
  end
  object mRelat: TMemo
    Left = 1
    Top = 56
    Width = 297
    Height = 241
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      '1...+....2....+....3....+....4....+...'
      '      TESTE DE RELATORIO'
      ''
      'O relatorio deve respeitar a '
      'propriedade COLUNAS do componente '
      'ACBrECF.'
      ''
      'A maioria dos ECF'#39's trabalham com 40 '
      'colunas, alguns com 48 colunas.'
      ''
      'O Relatorio Gerencial e usado pelas '
      'rotinas de TEF para imprirmir '
      'comprovantes, quando a impressao do '
      'Cupom NAO Fiscal vinculado falhar.')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Button1: TButton
    Left = 173
    Top = 301
    Width = 75
    Height = 25
    Caption = '&Ler TXT'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 53
    Top = 301
    Width = 75
    Height = 25
    Caption = '&Imprimir'
    TabOrder = 5
    OnClick = Button2Click
  end
  object edVias: TEdit
    Left = 8
    Top = 26
    Width = 25
    Height = 21
    TabOrder = 0
    Text = '3'
  end
  object edFPG: TEdit
    Left = 96
    Top = 26
    Width = 33
    Height = 21
    TabOrder = 2
    Text = '01'
    Visible = False
  end
  object edValor: TEdit
    Left = 224
    Top = 26
    Width = 65
    Height = 21
    TabOrder = 3
    Text = '1'
    Visible = False
    OnKeyPress = edValorKeyPress
  end
  object edCupom: TEdit
    Left = 40
    Top = 26
    Width = 49
    Height = 21
    TabOrder = 1
  end
  object edCNF: TEdit
    Left = 160
    Top = 26
    Width = 33
    Height = 21
    TabOrder = 7
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Arquivos Texto|*.TXT'
    FilterIndex = 0
    Left = 264
    Top = 56
  end
end
