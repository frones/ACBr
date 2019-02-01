object FrPosPrinterTeste: TFrPosPrinterTeste
  Left = 536
  Top = 240
  Width = 854
  Height = 561
  ActiveControl = bTagFormtacaoCaracter
  Caption = 'ACBrPosPrinter - Teste'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 256
    Top = 0
    Width = 582
    Height = 522
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 348
      Width = 580
      Height = 173
      Align = alBottom
      TabOrder = 0
      object bTagFormtacaoCaracter: TButton
        Left = 8
        Top = 44
        Width = 183
        Height = 25
        Caption = 'Tags de Format. de Caracter'
        TabOrder = 0
        OnClick = bTagFormtacaoCaracterClick
      end
      object bTagsAlinhamento: TButton
        Left = 8
        Top = 68
        Width = 183
        Height = 25
        Caption = 'Tags de Alinhamento'
        TabOrder = 1
        OnClick = bTagsAlinhamentoClick
      end
      object bTagQRCode: TButton
        Left = 200
        Top = 68
        Width = 183
        Height = 25
        Caption = 'Tags de QRCode'
        TabOrder = 6
        OnClick = bTagQRCodeClick
      end
      object bTagsCodBarras: TButton
        Left = 200
        Top = 44
        Width = 183
        Height = 25
        Caption = 'Tags de Codigo de Barras'
        TabOrder = 5
        OnClick = bTagsCodBarrasClick
      end
      object bTagsTesteInvalidas: TButton
        Left = 8
        Top = 92
        Width = 183
        Height = 25
        Caption = 'Teste de Tags Invalidas'
        TabOrder = 2
        OnClick = bTagsTesteInvalidasClick
      end
      object bTagsTestePagCodigo: TButton
        Left = 8
        Top = 116
        Width = 183
        Height = 25
        Caption = 'Teste de P'#225'gina de C'#243'digo'
        TabOrder = 3
        OnClick = bTagsTestePagCodigoClick
      end
      object bImpLinhaALinha: TButton
        Left = 200
        Top = 116
        Width = 183
        Height = 25
        Caption = 'Teste Impress'#227'o Linha a Linha'
        TabOrder = 7
        OnClick = bImpLinhaALinhaClick
      end
      object bImpTagsValidas: TButton
        Left = 392
        Top = 44
        Width = 183
        Height = 25
        Caption = 'Ajuda - Tags Validas'
        TabOrder = 9
        OnClick = bImpTagsValidasClick
      end
      object bLerStatus: TButton
        Left = 392
        Top = 116
        Width = 183
        Height = 25
        Caption = 'Leitura de Status'
        TabOrder = 12
        OnClick = bLerStatusClick
      end
      object bLerInfo: TButton
        Left = 392
        Top = 92
        Width = 183
        Height = 25
        Caption = 'Leitura de Informa'#231#245'es'
        TabOrder = 11
        OnClick = bLerInfoClick
      end
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 578
        Height = 39
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 13
        DesignSize = (
          578
          39)
        object bLimpar: TBitBtn
          Left = 389
          Top = 7
          Width = 83
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'Limpar'
          TabOrder = 1
          OnClick = bLimparClick
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00101073FF000029FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00000431FF10106BFFFF00FF00FF00FF00FF00FF000000
            8CFF3134F7FF0808CEFF000031FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000039FF080CCEFF3130EFFF00007BFFFF00FF00000063FF0808
            E7FF4A49F7FF4A4DF7FF0000C6FF000039FFFF00FF00FF00FF00FF00FF00FF00
            FF0000004AFF0004CEFF5A59EFFF5A59F7FF0808DEFF00004AFF000484FF2120
            FFFF3134FFFF5A59FFFF4A49EFFF0004C6FF000042FFFF00FF00FF00FF000000
            52FF0808CEFF5A59EFFF7371FFFF5255FFFF3134FFFF00046BFF000473FF181C
            FFFF2928FFFF3938FFFF5255FFFF4245EFFF0004C6FF00004AFF00005AFF0808
            CEFF5255EFFF6B69FFFF5251FFFF4241FFFF3130FFFF080C52FFFF00FF000808
            7BFF181CFFFF2928FFFF3134FFFF4A49FFFF3134EFFF0000BDFF0004C6FF4245
            EFFF5A59FFFF4245FFFF393CFFFF292CFFFF080C63FFFF00FF00FF00FF00FF00
            FF0008086BFF1818FFFF292CFFFF393CFFFF4A4DFFFF3130EFFF3134EFFF4A49
            FFFF3938FFFF3130FFFF2124F7FF08084AFFFF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000452FF393CEFFF6361FFFF6361FFFF6361FFFF6361FFFF6361
            FFFF5A59FFFF2928DEFF000439FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0000048CFF393CF7FF6361FFFF6361FFFF6365FFFF6365
            FFFF3134EFFF000463FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000005AFF0000A5FF3130F7FF6B69FFFF6B69FFFF6B69FFFF6B69
            FFFF2120E7FF00009CFF000042FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000052FF0000A5FF3134F7FF7B79FFFF7B79FFFF7B79FFFF7B79FFFF7B79
            FFFF7375FFFF2124E7FF000094FF000039FFFF00FF00FF00FF00FF00FF000000
            52FF00009CFF4241F7FF8C8AFFFF8C8AFFFF8C8AFFFF3130C6FF4A49DEFF8C8E
            FFFF8C8AFFFF8486FFFF2928E7FF00008CFF000039FFFF00FF00000018FF0000
            9CFF4A49F7FF9C9AFFFF9C9AFFFF9C9AFFFF3134BDFFFF00FF00000021FF5255
            E7FF9C9EFFFF9C9AFFFF9496FFFF292CE7FF00007BFFFF00FF00000029FF5255
            FFFFADAEFFFFADAAFFFFADAAFFFF393CBDFFFF00FF00FF00FF00FF00FF000000
            29FF5A5DE7FFADAEFFFFADAAFFFFADAAFFFF3130DEFFFF00FF00FF00FF004245
            BDFFBDBAFFFFBDBAFFFF393CBDFFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000029FF6B69E7FFCECBFFFFA5A6FFFF21248CFFFF00FF00FF00FF00FF00
            FF00393CBDFF4241C6FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000029FF5A59E7FF181C7BFFFF00FF00FF00FF00}
        end
        object bImprimir: TBitBtn
          Left = 484
          Top = 7
          Width = 92
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'Imprimir'
          TabOrder = 2
          OnClick = bImprimirClick
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000064000000640000000000000000000000000000000000
            00000000000000000000000000007F7F7FFF4F4F4FFF5F5F5FFFBFBFBFFF0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000007F7F7FFF5F5F5FFFBFBFBFFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F
            5FFFBFBFBFFF0000000000000000000000000000000000000000000000007F7F
            7FFF5F5F5FFFBFBFBFFF9F9F9FFF8F8F8FFF9F9F9FFF5F5F5FFF3F3F3FFF5F5F
            5FFF5F5F5FFF5F5F5FFFBFBFBFFF0000000000000000000000009F9F9FFFBFBF
            BFFF9F9F9FFF9F9F9FFFBFBFBFFF9F9F9FFF9F9F9FFF7F7F7FFF7F7F7FFF5F5F
            5FFF3F3F3FFF5F5F5FFF5F5F5FFF5F5F5FFF00000000000000009F9F9FFF8F8F
            8FFFBFBFBFFFBFBFBFFFBFBFBFFF9F9F9FFF9F9F9FFF7F7F7FFF7F7F7FFF7F7F
            7FFF7F7F7FFF5F5F5FFF3F3F3FFF1F1F1FFF0000000000000000BFBFBFFF9F9F
            9FFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFF8F8F8FFF7F7F7FFF7F7F
            7FFF7F7F7FFF7F7F7FFF7F7F7FFF5F5F5FFF3F3F3FFF00000000BFBFBFFF9F9F
            9FFFBFBFBFFFDFDFDFFFDFDFDFFFBFBFBFFFAFAFAFFF9F9F9FFF9F9F9FFF8F8F
            8FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF4F4F4FFF5F5F5FFFBFBFBFFFAFAF
            AFFFCFCFCFFF8FCF8FFF5F9F9FFF7F7FBFFF9F9F9FFFAFAFAFFFAFAFAFFF6F6F
            6FFF9F9F9FFF8F8F8FFF7F7F7FFF7F7F7FFF4F4F4FFFBFBFBFFFBFBFBFFFAFAF
            AFFFBFBFBFFF9F9F9FFF8F8F8FFF9F9F9FFFBFBFBFFF5F5F5FFF7F7F7FFFEFEF
            EFFF4F4F4FFF9F9F9FFF9F9F9FFF8F8F8FFF7F7F7FFF00000000000000000000
            0000BFBFBFFFAFAFAFFFBFBFBFFF8F8F8FFF5F5F5FFFFFBFBFFFFF7F7FFFFFBF
            BFFF6F6F6FFF8F8F8FFFAFAFAFFFDFDFDFFF0000000000000000000000000000
            00000000000000000000BFBFBFFFAFAFAFFFAFAFAFFFFFFFFFFFFF7F7FFFFF7F
            7FFFEFEFEFFF6F6F6FFF00000000000000000000000000000000000000000000
            000000000000000000000000000000000000BFBFBFFFBFBFBFFFFFBFBFFFFF7F
            7FFFFFBFBFFFEFEFEFFF6F6F6FFF000000000000000000000000000000000000
            00000000000000000000000000000000000000000000DFDFDFFFDFDFDFFFFFBF
            BFFFFF7F7FFFFFBFBFFFEFEFEFFF3F3F3FFF0000000000000000000000000000
            0000000000000000000000000000000000000000000000000000DFDFDFFFDFDF
            DFFFFFFFFFFFBFBFBFFFBFBFBFFF000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000DFDF
            DFFFBFBFBFFF0000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000}
        end
        object cbxLimparTexto: TCheckBox
          Left = 16
          Top = 8
          Width = 217
          Height = 19
          Caption = 'Limpar texto a cada teste selecionado'
          TabOrder = 0
        end
      end
      object bTagGaveta: TButton
        Left = 392
        Top = 68
        Width = 183
        Height = 25
        Caption = 'Tag de Gaveta'
        TabOrder = 10
        OnClick = bTagGavetaClick
      end
      object bTagsTestePageMode: TButton
        Left = 8
        Top = 140
        Width = 183
        Height = 25
        Caption = 'Teste de Page Mode'
        TabOrder = 4
        OnClick = bTagsTestePageModeClick
      end
      object Button1: TButton
        Left = 200
        Top = 140
        Width = 183
        Height = 25
        Caption = 'Senha'
        TabOrder = 8
        OnClick = Button1Click
      end
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 580
      Height = 347
      ActivePage = tsImagens
      Align = alClient
      TabHeight = 40
      TabOrder = 1
      object tsImprimir: TTabSheet
        Caption = 'Texto a Imprimir'
        object mImp: TMemo
          Left = 0
          Top = 0
          Width = 572
          Height = 297
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
      object tsLog: TTabSheet
        Caption = 'Eventos Log'
        object mLog: TMemo
          Left = 0
          Top = 0
          Width = 572
          Height = 297
          Align = alClient
          TabOrder = 0
        end
      end
      object tsImagens: TTabSheet
        Caption = 'Imagens e Logo'
        object Splitter1: TSplitter
          Left = 0
          Top = 137
          Width = 572
          Height = 5
          Cursor = crVSplit
          Align = alTop
        end
        object gbCodBarrasConfig2: TGroupBox
          Left = 0
          Top = 208
          Width = 572
          Height = 88
          Align = alTop
          Caption = 'LogoTipo'
          TabOrder = 0
          object Label14: TLabel
            Left = 16
            Top = 16
            Width = 20
            Height = 13
            Caption = 'KC1'
            Color = clBtnFace
            ParentColor = False
          end
          object Label15: TLabel
            Left = 72
            Top = 16
            Width = 20
            Height = 13
            Caption = 'KC2'
            Color = clBtnFace
            ParentColor = False
          end
          object Label16: TLabel
            Left = 128
            Top = 16
            Width = 31
            Height = 13
            Caption = 'FatorX'
            Color = clBtnFace
            ParentColor = False
          end
          object Label17: TLabel
            Left = 185
            Top = 16
            Width = 31
            Height = 13
            Caption = 'FatorY'
            Color = clBtnFace
            ParentColor = False
          end
          object seLogoKC1: TSpinEdit
            Left = 16
            Top = 32
            Width = 41
            Height = 22
            MaxValue = 126
            MinValue = 0
            TabOrder = 0
            Value = 32
            OnChange = seLogoKC1Change
          end
          object seLogoKC2: TSpinEdit
            Left = 72
            Top = 32
            Width = 41
            Height = 22
            MaxValue = 126
            MinValue = 0
            TabOrder = 1
            Value = 32
            OnChange = seLogoKC2Change
          end
          object seLogoFatorX: TSpinEdit
            Left = 129
            Top = 32
            Width = 41
            Height = 22
            MaxValue = 4
            MinValue = 1
            TabOrder = 2
            Value = 4
            OnChange = seLogoFatorXChange
          end
          object seLogoFatorY: TSpinEdit
            Left = 184
            Top = 32
            Width = 41
            Height = 22
            MaxValue = 4
            MinValue = 1
            TabOrder = 3
            Value = 4
            OnChange = seLogoFatorYChange
          end
          object bGravarLogo: TButton
            Left = 344
            Top = 24
            Width = 90
            Height = 25
            Caption = 'Gravar Logo'
            TabOrder = 4
            OnClick = bGravarLogoClick
          end
          object bTagLogo: TButton
            Left = 240
            Top = 56
            Width = 118
            Height = 25
            Caption = 'Tags de Logotipo'
            TabOrder = 5
            OnClick = bTagLogoClick
          end
          object bImprimirLogo: TButton
            Left = 240
            Top = 24
            Width = 72
            Height = 25
            Caption = 'Imprimir'
            TabOrder = 6
            OnClick = bImprimirLogoClick
          end
          object bApagarLogo: TButton
            Left = 464
            Top = 24
            Width = 94
            Height = 25
            Caption = 'Apagar Logo'
            TabOrder = 7
            OnClick = bApagarLogoClick
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 142
          Width = 572
          Height = 66
          Align = alTop
          BevelInner = bvRaised
          BevelOuter = bvLowered
          TabOrder = 1
          object bCaregarImagem: TButton
            Left = 8
            Top = 7
            Width = 118
            Height = 25
            Caption = 'Carregar Imagem'
            TabOrder = 0
            OnClick = bCaregarImagemClick
          end
          object bImprimirImagem: TButton
            Left = 232
            Top = 35
            Width = 72
            Height = 25
            Caption = 'Imprimir'
            TabOrder = 2
            OnClick = bImprimirImagemClick
          end
          object bTagBMP: TButton
            Left = 394
            Top = 35
            Width = 168
            Height = 25
            Caption = 'Tags de Impress'#227'o de BMP'
            TabOrder = 3
            OnClick = bTagBMPClick
          end
          object edImagem: TEdit
            Left = 131
            Top = 7
            Width = 431
            Height = 21
            TabOrder = 1
          end
          object rbArquivo: TRadioButton
            Left = 121
            Top = 41
            Width = 100
            Height = 19
            Caption = 'De um Arquivo'
            TabOrder = 4
          end
          object rbStream: TRadioButton
            Left = 16
            Top = 41
            Width = 95
            Height = 19
            Caption = 'De um Stream'
            Checked = True
            TabOrder = 5
            TabStop = True
          end
          object bConverter: TButton
            Left = 310
            Top = 35
            Width = 78
            Height = 25
            Caption = 'Converter'
            TabOrder = 6
            OnClick = bConverterClick
          end
        end
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 0
          Width = 572
          Height = 137
          Align = alTop
          TabOrder = 2
          object Image1: TImage
            Left = 0
            Top = 0
            Width = 568
            Height = 126
            Align = alTop
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
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 256
    Height = 522
    Align = alLeft
    TabOrder = 1
    object gbConfiguracao: TGroupBox
      Left = 1
      Top = 1
      Width = 254
      Height = 247
      Align = alTop
      Caption = 'Configura'#231#227'o'
      TabOrder = 0
      DesignSize = (
        254
        247)
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 35
        Height = 13
        Caption = 'Modelo'
        Color = clBtnFace
        ParentColor = False
      end
      object Label4: TLabel
        Left = 16
        Top = 56
        Width = 25
        Height = 13
        Caption = 'Porta'
        Color = clBtnFace
        ParentColor = False
      end
      object Label2: TLabel
        Left = 15
        Top = 104
        Width = 38
        Height = 13
        Caption = 'Colunas'
        Color = clBtnFace
        ParentColor = False
      end
      object Label3: TLabel
        Left = 72
        Top = 104
        Width = 41
        Height = 13
        Caption = 'Espa'#231'os'
        Color = clBtnFace
        ParentColor = False
      end
      object Label5: TLabel
        Left = 127
        Top = 194
        Width = 54
        Height = 13
        Caption = 'Pag.codigo'
        Color = clBtnFace
        ParentColor = False
      end
      object btSerial: TSpeedButton
        Left = 220
        Top = 72
        Width = 23
        Height = 22
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF323232
          3232323E3E3E565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF503200FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          565656565656FFFFFFFFFFFF3232322626262626262626262626265032005032
          000000504873FFFFFFFFFFFFFFFFFFFF6E6E6EFFFFFFFFFFFFFFFFFFFFFFFF6E
          6E6E32323232323232323232323250320000005025AAFFFFFFFFFFFFFF565656
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5656563232323232326E6E6E5032005032
          008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFF50320050320056
          56564A4A4A5050003232325032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
          FFFFFF5656563E3E3E2626265032006262625656565050003232325032005032
          008FFF6B8ED4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5050005050006E
          6E6E5656565050003250005032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8686865656565656563250005032005032
          008FFF6B48B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3232323E
          3E3EA4A0A08686866E6E6E565656503200C0C0C02557FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF5050004A4A4A3232323232323232323232325032
          00FFFFFF6B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        OnClick = btSerialClick
      end
      object Label6: TLabel
        Left = 131
        Top = 104
        Width = 28
        Height = 13
        Caption = 'Buffer'
        Color = clBtnFace
        ParentColor = False
      end
      object Label9: TLabel
        Left = 132
        Top = 152
        Width = 40
        Height = 13
        Alignment = taRightJustify
        Caption = 'Arq.Log:'
        Color = clBtnFace
        ParentColor = False
      end
      object SbArqLog: TSpeedButton
        Left = 220
        Top = 168
        Width = 24
        Height = 22
        Hint = 'Abre Arquivo de Log'
        Caption = '...'
        OnClick = SbArqLogClick
      end
      object Label7: TLabel
        Left = 178
        Top = 104
        Width = 67
        Height = 13
        Caption = 'Linhas '#224' Pular'
        Color = clBtnFace
        ParentColor = False
      end
      object cbTraduzirTags: TCheckBox
        Left = 15
        Top = 197
        Width = 88
        Height = 19
        Hint = 'Traduz as Tags de Formata'#231#227'o e Cod.Barras'
        Caption = 'TraduzirTags'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object cbIgnorarTags: TCheckBox
        Left = 15
        Top = 216
        Width = 83
        Height = 19
        Hint = 'N'#227'o processa as Tags, imprime-as como texto normal'
        Caption = 'IgnorarTags'
        TabOrder = 1
      end
      object cbxModelo: TComboBox
        Left = 16
        Top = 32
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = cbxModeloChange
      end
      object cbxPorta: TComboBox
        Left = 16
        Top = 72
        Width = 192
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        OnChange = cbxPortaChange
      end
      object seColunas: TSpinEdit
        Left = 15
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 999
        MinValue = 1
        TabOrder = 4
        Value = 48
        OnChange = seColunasChange
      end
      object cbControlePorta: TCheckBox
        Left = 15
        Top = 158
        Width = 97
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Controle Porta'
        TabOrder = 5
      end
      object seEspLinhas: TSpinEdit
        Left = 72
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnChange = seEspLinhasChange
      end
      object cbxPagCodigo: TComboBox
        Left = 127
        Top = 210
        Width = 117
        Height = 21
        Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
        OnChange = cbxPagCodigoChange
      end
      object seLinhasBuffer: TSpinEdit
        Left = 131
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 8
        Value = 0
        OnChange = seLinhasBufferChange
      end
      object edLog: TEdit
        Left = 127
        Top = 168
        Width = 84
        Height = 21
        Cursor = crIBeam
        Hint = 'Arquivo de Log para gravar os comandos enviados e recebidos'
        TabOrder = 9
        Text = 'PosPrinter.log'
      end
      object bAtivar: TBitBtn
        Left = 175
        Top = 16
        Width = 67
        Height = 43
        Anchors = [akTop, akRight]
        Caption = 'Ativar'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ModalResult = 1
        ParentFont = False
        TabOrder = 10
        OnClick = bAtivarClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF0808391818A51818A51818A51818A51818A518
          18A51818A51818A51818A51818A51818A51818A5101042FFFFFFFFFFFF1818AD
          0000C60000C60000CE0000CE0000DE1818E71818EF0808EF0008EF1821EF2131
          F73142FF2929BDFFFFFFFFFFFF1010AD0000B50000BD0000C66B6BE7DEDEFFFF
          FFFFFFFFFFE7E7FF848CF71821EF1829EF2939EF2129C6FFFFFFFFFFFF1010AD
          0000B50000BD8C8CE7FFFFFFDEDEF79494EF8C8CEFD6D6F7FFFFFFADB5FF2129
          EF2939EF2129C6FFFFFFFFFFFF1010AD0000B56B6BD6FFFFFFADADEF0808CE00
          00D60000DE0000E79494EFFFFFFF949CF71829EF2129C6FFFFFFFFFFFF1010A5
          1010BDD6D6F7F7F7FF3131CE1818D61818DE1010DE0000E70008E7E7E7FFEFEF
          FF2931EF2129C6FFFFFFFFFFFF1818A53131BDEFEFFFDEDEF72929CE3131D631
          31DE3131DE3939E71010E7B5B5F7FFFFFF3139EF1821BDFFFFFFFFFFFF1818A5
          3939BDE7E7F7EFEFFF4242CE3939D69C9CEF9C9CEF4A4AE74A4AEFDEDEFFEFF7
          FF1821EF1818BDFFFFFFFFFFFF2121A54242BDADADDEFFFFFF9C9CE74A4AD6EF
          EFFFF7F7FF5252E79494EFFFFFFFC6C6F70810E71010BDFFFFFFFFFFFF2929A5
          5A5AC66363C6DEDEEFFFFFFF9494E7E7E7F7E7E7FFA5A5EFFFFFFFEFEFF78484
          E76363EF1010BDFFFFFFFFFFFF2929A56B6BC66363C67373C6ADADD68484D6E7
          E7FFEFEFFFA5A5E7D6D6EF8484DE7B7BE78C8CEF3131BDFFFFFFFFFFFF3131A5
          7B7BC67373C67373CE7373C67B7BCEF7F7FFF7F7FF8484DE8484D68C8CE78C8C
          E79494E74242BDFFFFFFFFFFFF313194ADADDEADADDEADADDEADADE7B5B5E7B5
          B5E7B5B5E7BDBDEFBDBDEFC6C6EFC6C6F7D6D6F78C8CB5FFFFFFFFFFFF181894
          21218C3131943131943131943131943131943131943131943131943131943131
          94292994181884FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Layout = blGlyphTop
      end
      object seLinhasPular: TSpinEdit
        Left = 192
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 11
        Value = 0
        OnChange = seLinhasPularChange
      end
      object cbCortarPapel: TCheckBox
        Left = 15
        Top = 178
        Width = 85
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Cortar Papel'
        Checked = True
        State = cbChecked
        TabOrder = 12
      end
    end
    object gbCodBarrasConfig: TGroupBox
      Left = 1
      Top = 248
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'Cod.Barras'
      TabOrder = 1
      object Label8: TLabel
        Left = 16
        Top = 16
        Width = 36
        Height = 13
        Caption = 'Largura'
        Color = clBtnFace
        ParentColor = False
      end
      object Label10: TLabel
        Left = 72
        Top = 16
        Width = 27
        Height = 13
        Caption = 'Altura'
        Color = clBtnFace
        ParentColor = False
      end
      object seBarrasLargura: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 5
        MinValue = 0
        TabOrder = 0
        Value = 1
        OnChange = seBarrasLarguraChange
      end
      object seBarrasAltura: TSpinEdit
        Left = 72
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = seBarrasAlturaChange
      end
      object cbHRI: TCheckBox
        Left = 136
        Top = 32
        Width = 94
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Exibe Numero'
        TabOrder = 2
      end
    end
    object gbCodBarrasConfig1: TGroupBox
      Left = 1
      Top = 312
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'QRCode'
      TabOrder = 2
      object Label11: TLabel
        Left = 16
        Top = 16
        Width = 21
        Height = 13
        Caption = 'Tipo'
        Color = clBtnFace
        ParentColor = False
      end
      object Label12: TLabel
        Left = 72
        Top = 16
        Width = 63
        Height = 13
        Caption = 'Largura Mod.'
        Color = clBtnFace
        ParentColor = False
      end
      object Label13: TLabel
        Left = 167
        Top = 16
        Width = 48
        Height = 13
        Caption = 'ErrorLevel'
        Color = clBtnFace
        ParentColor = False
      end
      object seQRCodeTipo: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 2
        MinValue = 1
        TabOrder = 0
        Value = 2
        OnChange = seQRCodeTipoChange
      end
      object seQRCodeLarguraModulo: TSpinEdit
        Left = 88
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 1
        Value = 4
        OnChange = seQRCodeLarguraModuloChange
      end
      object seQRCodeErrorLevel: TSpinEdit
        Left = 168
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 3
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = seQRCodeErrorLevelChange
      end
    end
    object gbGavetaConfig: TGroupBox
      Left = 1
      Top = 376
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'Gaveta'
      TabOrder = 3
      object Label18: TLabel
        Left = 63
        Top = 16
        Width = 16
        Height = 13
        Caption = 'ON'
        Color = clBtnFace
        ParentColor = False
      end
      object Label19: TLabel
        Left = 112
        Top = 16
        Width = 20
        Height = 13
        Caption = 'OFF'
        Color = clBtnFace
        ParentColor = False
      end
      object Label20: TLabel
        Left = 16
        Top = 16
        Width = 35
        Height = 13
        Caption = 'Gaveta'
        Color = clBtnFace
        ParentColor = False
      end
      object seGavetaTempoON: TSpinEdit
        Left = 63
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 250
        MinValue = 50
        TabOrder = 1
        Value = 50
        OnChange = seGavetaTempoONChange
      end
      object cbGavetaSinalInvertido: TCheckBox
        Left = 167
        Top = 32
        Width = 67
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Invertido'
        TabOrder = 3
      end
      object seGavetaTempoOFF: TSpinEdit
        Left = 112
        Top = 31
        Width = 41
        Height = 22
        MaxValue = 250
        MinValue = 50
        TabOrder = 2
        Value = 200
        OnChange = seGavetaTempoOFFChange
      end
      object seGavetaNum: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 2
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
    end
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    Modelo = ppEscPosEpson
    Device.NomeDocumento = 'ACBrPorPrinterDemo'
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 0
    CortaPapel = False
    VerificarImpressora = True
    OnGravarLog = ACBrPosPrinter1GravarLog
    Left = 120
    Top = 456
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 16
    Top = 448
  end
end
