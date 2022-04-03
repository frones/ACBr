object FPrincipal: TFPrincipal
  Left = 621
  Top = 301
  Width = 756
  Height = 390
  Caption = 'Impress'#227'o de Etiquetas'
  Color = clBtnFace
  Constraints.MinHeight = 340
  Constraints.MinWidth = 740
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbConfiguracao: TGroupBox
    Left = 0
    Top = 0
    Width = 246
    Height = 351
    Align = alLeft
    Caption = 'Configura'#231#245'es da Impressora'
    TabOrder = 0
    DesignSize = (
      246
      351)
    object lbModelo: TLabel
      Left = 136
      Top = 21
      Width = 38
      Height = 13
      Caption = 'Modelo:'
    end
    object lbPorta: TLabel
      Left = 10
      Top = 21
      Width = 28
      Height = 13
      Caption = 'Porta:'
    end
    object lbAvanco: TLabel
      Left = 10
      Top = 201
      Width = 59
      Height = 13
      Caption = 'Avan'#231'o Etq:'
    end
    object lbDPI: TLabel
      Left = 136
      Top = 66
      Width = 75
      Height = 13
      Caption = 'DPI Impressora:'
    end
    object lbTemperatura: TLabel
      Left = 10
      Top = 66
      Width = 63
      Height = 13
      Caption = 'Temperatura:'
    end
    object lbBackFeed: TLabel
      Left = 136
      Top = 157
      Width = 52
      Height = 13
      Caption = 'BackFeed:'
    end
    object lbMargem: TLabel
      Left = 10
      Top = 111
      Width = 89
      Height = 13
      Hint = 'Utilize '#39'-1'#39' para usar velocidade padr'#227'o da impressora'
      Caption = 'Margem Esquerda:'
    end
    object lbTemperatura2: TLabel
      Left = 136
      Top = 111
      Width = 56
      Height = 13
      Hint = 'Utilize '#39'-1'#39' para usar velocidade padr'#227'o da impressora'
      Caption = 'Velocidade:'
    end
    object lbBackFeed1: TLabel
      Left = 136
      Top = 201
      Width = 36
      Height = 13
      Caption = 'Origem:'
    end
    object Label5: TLabel
      Left = 10
      Top = 157
      Width = 54
      Height = 13
      Caption = 'Pag.codigo'
    end
    object cbModelo: TComboBox
      Left = 136
      Top = 34
      Width = 94
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbModeloChange
    end
    object cbPorta: TComboBox
      Left = 10
      Top = 34
      Width = 113
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = '\\LOCALHOST\L42'
      Items.Strings = (
        'LPT1'
        'LPT2'
        'COM1'
        'COM2'
        'COM3'
        '\\localhost\ZEBRA'
        '\\127.0.0.1\ARGOX'
        'Digite a porta')
    end
    object edAvanco: TEdit
      Left = 10
      Top = 214
      Width = 113
      Height = 21
      TabOrder = 6
      OnKeyPress = eOnlyNumberKeyPress
    end
    object cbDPI: TComboBox
      Left = 136
      Top = 79
      Width = 94
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object edTemperatura: TEdit
      Left = 10
      Top = 79
      Width = 113
      Height = 21
      TabOrder = 2
      Text = '10'
      OnKeyPress = eOnlyNumberKeyPress
    end
    object cbBackFeed: TComboBox
      Left = 136
      Top = 170
      Width = 94
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
    end
    object edMargemEsquerda: TEdit
      Left = 10
      Top = 124
      Width = 113
      Height = 21
      TabOrder = 4
      Text = '10'
      OnKeyPress = eOnlyNumberKeyPress
    end
    object ckMemoria: TCheckBox
      Left = 10
      Top = 246
      Width = 94
      Height = 19
      Caption = 'Limpar Mem'#243'ria'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object edVelocidade: TEdit
      Left = 136
      Top = 124
      Width = 94
      Height = 21
      Hint = 'Utilize '#39'-1'#39' para usar velocidade padr'#227'o da impressora'
      TabOrder = 5
      Text = '-1'
      OnKeyPress = eOnlyNumberKeyPress
    end
    object cbOrigem: TComboBox
      Left = 136
      Top = 214
      Width = 94
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
    end
    object cbPagCodigo: TComboBox
      Left = 10
      Top = 170
      Width = 113
      Height = 21
      Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
    end
    object bConfSalvar: TButton
      Left = 14
      Top = 286
      Width = 94
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Salvar'
      TabOrder = 11
      OnClick = bConfSalvarClick
    end
    object bConfLer: TButton
      Left = 134
      Top = 286
      Width = 94
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Ler'
      TabOrder = 12
      OnClick = bConfLerClick
    end
  end
  object gbImagem: TGroupBox
    Left = 246
    Top = 0
    Width = 309
    Height = 351
    Align = alClient
    Caption = 'Carregar Imagem'
    TabOrder = 1
    DesignSize = (
      309
      351)
    object Image1: TImage
      Left = 2
      Top = 15
      Width = 305
      Height = 211
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      Center = True
      Picture.Data = {
        07544269746D6170DE0D0000424DDE0D0000000000003E00000028000000FA00
        00006D0000000100010000000000A00D00000000000000000000020000000000
        000000000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FE00000000000000000000000F00F000000000000000
        00000000000000003FC0FC00000000000000000000000F00F800000000000000
        00000000000000000FC0F800000000000000000000000F007C00000000000000
        000000000000000007C0E000000000000000000000000F003E00000000000000
        200000000000000001C0C000000000000000000000000F001F00000000000001
        C00000000000000001C0C000000000000000000000000F000F8000000000001F
        800000000000000000C0800000000000000000000380040007C000000000007F
        8000000000000000004080000000000000000000008000000100000000000000
        00000000000000000040000000000C3BE39E7367E3CF67807C38CCCF9C3CCFCC
        0FCC3B3CEE0000000000000000000C37E7BF7367E7EFEFC0FE7CCCCFDC7ECFCC
        0FEC7776EE0000000000000000000FF666737367660EECE0C6C6CCDFDCE0CECC
        0C6C771EEE000000000000000000066666737363EE07ECE1C0C6CCDFDCE0C7CC
        0CEE1F7EEE00000000000000000007E6663377606660ECC1C0E6EECCDCE6C4CC
        0FCE0760EE00000000000000000007C66F9E7EE7E7C7C781C07CFFCF9F7CC7CC
        0CEFBF3EEE00000000000000000003C00600000000000000EE0000000000000C
        0CE000000E00000000008000000003C00200000000078000FC0000300000C00C
        0FC00000EE00000000408000000000000000000000000F003803E1FFE00003C0
        00000000000000000040C000000000000000000000000F000001FFFE000007C0
        000000000000000000C0E000000000000000000000000F000000FFF000000780
        000000000000000001C0F000000000000000000000000F0000007FC000000700
        000000000000000003C0F800000000000000000000000F0000003C0000000E00
        000000000000000007C0FE00000000000000000000000F000000000000001E00
        00000000000000001FC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFF8003FFFFF01FFFFFFF800FFFFFFFFFF80
        7FFFF807FFFFFFFFFFC0FFFFFFE000780078001E03F80001FC00F00000007FE0
        78007807FFFFFFFFFFC0FFFFFFE000380078001E07E000007E00F000000007F0
        78007807FFFFFFFFFFC0FFFFFFF000380078001E0F8000001F00F000000001F0
        78007807FFFFFFFFFFC0FFFFFFF0003C0070001C3E0000000780F0000000007C
        78007807FFFFFFFFFFC0FFFFFFF8003FFFF0003C3E00000007C0F0000000003C
        78007807FFFFFFFFFFC0FFFFFFF8001FFFF000387800000001E0F0000000001E
        78007807FFFFFFFFFFC0FFFFFFFC001FFFF00078F800000001E0F0000000001E
        78007807FFFFFFFFFFC0FFFFFFFC001FFFE00078F000000000F0F0000000000E
        78007807FFFFFFFFFFC0FFFFFFFC0000000000F1E000000000F8F0000000000F
        7800780FFFFFFFFFFFC0FFFFFFFC0000000000F1C00000000078F0000000000F
        7800780FFFFFFFFFFFC0FFFFFFFE0000000000E3C0003FC00038F0003FF80007
        7800780FFFFFFFFFFFC0FFFFFFFE0000000000E3C0007FE00038F0003FFC0007
        F800780FFFFFFFFFFFC0FFFFFFFF0000000001EF8000FFF0003CF0003FFE0007
        F800781FFFFFFFFFFFC0FFFFFFFF0000000001FF8001F0F8003CF00039FE0007
        F800781FFFFFFFFFFFC0FFFFFFFF0000000003FF0001E07C001CF0003BCE0007
        F800781FFFFFFFFFFFC0FFFFFFFF8000000003FF0003C03C00FEF0003FFE0007
        7800783FFFFFFFFFFFC0FFFFFFFF8000000003E70003801C07FEF0003FFE000F
        7800783FFFFFFFFFFFC0FFFFFFFFC001FF00078F0003801C1FFEF0003FFC000F
        7800383FFFFFFFFFFFC0FFFFFFFFC001FF00070F0003801FFFC0F0003FE0000E
        7800383FFFFFFFFFFFC0FFFFFFFFC001FE000F0F0007801FFF00F0000000001E
        78003C7FFFFFFFFFFFC0FFFFFFFFE001FE000F0F0007800FE000F0000000003E
        78001FFFFFFFFFFFFFC0FFFFFFFFE001FE000F0F0007800F8000F0000000007C
        78001FFFFFFFFFFFFFC0FFFFFFFFE000FE001FEF0007800C0000F000000000F8
        78001FFFFFFFFFFFFFC0FFFFFFFFF000FC001FFF000780000000F000000007F0
        7800073FFFFFFFFFFFC0FFFFFFFFF0007C001FFF000780000000F000000007E0
        7800001FFFFFFFFFFFC0FFFFFFFFF8007C003C7F0007800E0000F000000001F0
        7800000FFFFFFFFFFFC0FFFFFFFFF80078003C0F0003801FE000F000000000F0
        7800000FFFFFFFFFFFC0FFFFFFFFF8003800780F0003801FFE00F00000000078
        7800000FFFFFFFFFFFC0FFFFFFFFFC00300078070003801FFFF8F0003FF0003C
        78008007FFFFFFFFFFC0FFFFFFFFFC00300070070003C01CFFFEF0003FF8003C
        78008007FFFFFFFFFFC0FFFFFFFFFE003000F0070001FC7C03FEF0003FF8003C
        7800C003FFFFFFFFFFC0FFFFFFFFFE001000E0078001FFF8003CF0003838001C
        7800E003FFFFFFFFFFC0FFFFFFFFFE001000E0078000FFF8003CF0003CF8001E
        7800F007FFFFFFFFFFC0FFFFFFFFFF000001E00380007FE00038F0003FF8001E
        7FFFFFFFFFFFFFFFFFC0FFFFFFFFFF000001E003C0003FC00038F0003FF8001E
        7FFFFFFFFFFFFFFFFFC0FFFFFFFFFF000003C001C00000000078F0003FC0001E
        7FFFFFFFFFFFFFFFFFC0FFFFFFFFFF800003C001E000000000F0F0000000001C
        00007FFFFFFFFFFFFFC0FFFFFFFFFF8000038000E000000000F0F0000000003C
        00007FFFFFFFFFFFFFC0FFFFFFFFFFC000078000F800000001E0F0000000003C
        0000FFFFFFFFFFFFFFC0FFFFFFFFFFC0000780007800000003C0F00000000038
        0001FFFFFFFFFFFFFFC0FFFFFFFFFFC0000F00003E00000007C0F000000000FC
        0003FFFFFFFFFFFFFFC0FFFFFFFFFFC0000F00003E0000000F80F000000000FE
        0003FFFFFFFFFFFFFFC0FFFFFFFFFFE0000F00001F8000001F00F000000001FE
        0007FFFFFFFFFFFFFFC0FFFFFFFFFFE0000E000007E000007E00F000000007CF
        000FFFFFFFFFFFFFFFC0FFFFFFFFFFF0001E000003F80001FC00F00000003FCF
        800FFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFC000001FFFFFFF000FFFFFFFFFF07
        C01FFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFC0000007FFFFFE000FFFFFFFFFFF3
        C03FFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFE0000001FFFFF8001FFFFFFFFFFFF
        E07FFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFF00000000FFF00001E000000007FF
        F0FFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFF8000000000700003C0000000007F
        F9FFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFC000000000700007C00000000001
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFE000000000700007800000000000
        1FFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFF00000000070000F000000000000
        0FFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFF80000000070001E000000000000
        1FFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFC0000000070003E000000000000
        3FFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFF00000000700078000000000000
        7FFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFF000000007800F8000000000000
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFF800000007800F0000000000001
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFE00000007801E0000000000007
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFF00000007803C000000000000F
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFC0000007807C000000000003F
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFE00000078078000000000007F
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFF000000380F000000000000FF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFC00000381F000000000003FF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFF00000381E00000000000FFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFC00003C7C00000000003FFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFF00003C780000000000FFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFC0003CF80000000001FFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFF0003DE0000000000FFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFE003FE0000000003FFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFC03FC000000003FFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFF01F800000000FFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFE1F000000007FFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFC00003FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFC0}
      Proportional = True
      Stretch = True
    end
    object lbNomeImg: TLabel
      Left = 15
      Top = 238
      Width = 111
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Nome Imagem Mem'#243'ria'
    end
    object edNomeImg: TEdit
      Left = 15
      Top = 251
      Width = 258
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 0
      Text = 'ACBR'
    end
    object bCarregarImg: TButton
      Left = 15
      Top = 281
      Width = 152
      Height = 32
      Anchors = [akLeft, akBottom]
      Caption = 'Carregar Imagem'
      TabOrder = 1
      OnClick = bCarregarImgClick
    end
    object rbStream: TRadioButton
      Left = 183
      Top = 298
      Width = 87
      Height = 19
      Anchors = [akLeft, akBottom]
      Caption = 'De um Stream'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object rbArquivo: TRadioButton
      Left = 183
      Top = 281
      Width = 90
      Height = 19
      Anchors = [akLeft, akBottom]
      Caption = 'De um Arquivo'
      TabOrder = 2
    end
  end
  object gbImpressao: TGroupBox
    Left = 555
    Top = 0
    Width = 185
    Height = 351
    Align = alRight
    Caption = 'Impress'#227'o'
    TabOrder = 2
    DesignSize = (
      185
      351)
    object lbCopias: TLabel
      Left = 16
      Top = 32
      Width = 47
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'N'#186' Copias'
    end
    object eCopias: TEdit
      Left = 16
      Top = 34
      Width = 150
      Height = 21
      TabOrder = 0
      Text = '1'
      OnKeyPress = eOnlyNumberKeyPress
    end
    object bEtqSimples: TButton
      Left = 16
      Top = 66
      Width = 150
      Height = 32
      Caption = 'Etiqueta Simples'
      TabOrder = 1
      OnClick = bEtqSimplesClick
    end
    object bEtqCarreiras: TButton
      Left = 16
      Top = 106
      Width = 150
      Height = 32
      Caption = 'Etiqueta 3 Colunas'
      TabOrder = 2
      OnClick = bEtqCarreirasClick
    end
    object bImprimirImagem: TButton
      Left = 16
      Top = 186
      Width = 150
      Height = 32
      Caption = 'Imprimir Imagem'
      TabOrder = 4
      OnClick = bImprimirImagemClick
    end
    object bEtqBloco: TButton
      Left = 16
      Top = 146
      Width = 150
      Height = 32
      Caption = 'Bloco de Etiquetas'
      TabOrder = 3
      OnClick = bEtqBlocoClick
    end
    object bQRCode: TButton
      Left = 16
      Top = 226
      Width = 150
      Height = 32
      Caption = 'Imprimir QRCode'
      TabOrder = 5
      OnClick = bQRCodeClick
    end
  end
  object ACBrETQ: TACBrETQ
    Origem = ogTop
    ArqLOG = 'etq.txt'
    Porta = 'LPT1'
    Ativo = False
    Left = 400
    Top = 48
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'BMP MonoCrom'#225'tico|*.bmp|PCX|*.pcx|IMG|*.img'
    Left = 304
    Top = 48
  end
end
