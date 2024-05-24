inherited frlGuiaRLRetrato: TfrlGuiaRLRetrato
  Left = 0
  Top = 0
  Caption = 'ACBrGNREGuiaFR_Fortes'
  ClientHeight = 1044
  ClientWidth = 812
  Font.Name = 'Tahoma'
  TextHeight = 13
  object RLDraw41: TRLDraw [0]
    Left = 511
    Top = 411
    Width = 219
    Height = 27
  end
  object RLDraw43: TRLDraw [1]
    Left = 511
    Top = 463
    Width = 219
    Height = 27
  end
  object RLDraw42: TRLDraw [2]
    Left = 511
    Top = 437
    Width = 219
    Height = 27
  end
  inherited RLGNRe: TRLReport
    Left = 0
    Top = 0
    Margins.LeftMargin = 6.000000000000000000
    Margins.TopMargin = 6.000000000000000000
    Margins.RightMargin = 6.000000000000000000
    Margins.BottomMargin = 6.000000000000000000
    Font.Color = clBlack
    OnDataRecord = RLGNReDataRecord
    object subItens: TRLSubDetail
      Left = 23
      Top = 23
      Width = 748
      Height = 1060
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      InsideMargins.LeftMargin = 5.000000000000000000
      OnDataRecord = subItensDataRecord
      object RLBand1: TRLBand
        Left = 19
        Top = 0
        Width = 729
        Height = 1030
        BeforePrint = RLBand1BeforePrint
        object imgQrCodePIX: TRLImage
          Left = 524
          Top = 269
          Width = 52
          Height = 52
          Center = True
          Scaled = True
        end
        object imgQrCodePIX2: TRLImage
          Left = 524
          Top = 599
          Width = 52
          Height = 52
          Center = True
          Scaled = True
        end
        object imgQrCodePIX3: TRLImage
          Left = 524
          Top = 935
          Width = 52
          Height = 52
          Center = True
          Scaled = True
        end
        object RLMemo2: TRLMemo
          Left = 0
          Top = 0
          Width = 700
          Height = 261
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clSilver
          Font.Height = -67
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            ''
            'Guia Inv'#225'lida '
            'para Pagamento')
          ParentFont = False
        end
        object RLMemo5: TRLMemo
          Left = 8
          Top = 678
          Width = 700
          Height = 261
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clSilver
          Font.Height = -67
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            ''
            'Guia Inv'#225'lida '
            'para Pagamento')
          ParentFont = False
        end
        object RLDraw55: TRLDraw
          Left = 480
          Top = 744
          Width = 219
          Height = 27
        end
        object RLDraw53: TRLDraw
          Left = 480
          Top = 718
          Width = 219
          Height = 27
        end
        object RLDraw52: TRLDraw
          Left = 479
          Top = 692
          Width = 220
          Height = 27
        end
        object RLMemo3: TRLMemo
          Left = 8
          Top = 338
          Width = 700
          Height = 261
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clSilver
          Font.Height = -67
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            ''
            'Guia Inv'#225'lida '
            'para Pagamento')
          ParentFont = False
        end
        object RLDraw49: TRLDraw
          Left = 480
          Top = 900
          Width = 219
          Height = 27
        end
        object RLDraw48: TRLDraw
          Left = 480
          Top = 874
          Width = 219
          Height = 27
        end
        object RLDraw47: TRLDraw
          Left = 480
          Top = 848
          Width = 219
          Height = 27
        end
        object RLDraw46: TRLDraw
          Left = 480
          Top = 822
          Width = 219
          Height = 27
        end
        object RLDraw45: TRLDraw
          Left = 480
          Top = 796
          Width = 219
          Height = 27
        end
        object RLDraw44: TRLDraw
          Left = 480
          Top = 770
          Width = 111
          Height = 27
        end
        object RLDraw38: TRLDraw
          Left = 590
          Top = 770
          Width = 109
          Height = 27
        end
        object RLDraw51: TRLDraw
          Left = 1
          Top = 863
          Width = 480
          Height = 64
        end
        object RLDraw50: TRLDraw
          Left = 1
          Top = 811
          Width = 480
          Height = 53
        end
        object RLDraw36: TRLDraw
          Left = 1
          Top = 770
          Width = 480
          Height = 42
        end
        object RLDraw37: TRLDraw
          Left = 1
          Top = 692
          Width = 480
          Height = 79
        end
        object RLDraw1: TRLDraw
          Left = 0
          Top = 0
          Width = 481
          Height = 27
        end
        object RLDraw14: TRLDraw
          Left = 0
          Top = 104
          Width = 481
          Height = 42
        end
        object RLDraw2: TRLDraw
          Left = 0
          Top = 26
          Width = 481
          Height = 79
        end
        object RLDraw17: TRLDraw
          Left = 589
          Top = 104
          Width = 110
          Height = 27
        end
        object RLDraw3: TRLDraw
          Left = 480
          Top = 0
          Width = 89
          Height = 27
        end
        object RLDraw4: TRLDraw
          Left = 568
          Top = 0
          Width = 131
          Height = 27
        end
        object RLDraw5: TRLDraw
          Left = 480
          Top = 26
          Width = 219
          Height = 27
        end
        object RLDraw6: TRLDraw
          Left = 480
          Top = 52
          Width = 219
          Height = 27
        end
        object RLDraw7: TRLDraw
          Left = 480
          Top = 78
          Width = 219
          Height = 27
        end
        object RLDraw8: TRLDraw
          Left = 480
          Top = 104
          Width = 110
          Height = 27
        end
        object RLDraw9: TRLDraw
          Left = 480
          Top = 130
          Width = 219
          Height = 27
        end
        object RLDraw10: TRLDraw
          Left = 480
          Top = 156
          Width = 219
          Height = 27
        end
        object RLDraw11: TRLDraw
          Left = 480
          Top = 182
          Width = 219
          Height = 27
        end
        object RLDraw12: TRLDraw
          Left = 480
          Top = 208
          Width = 219
          Height = 27
        end
        object RLDraw13: TRLDraw
          Left = 480
          Top = 234
          Width = 219
          Height = 27
        end
        object RLDraw15: TRLDraw
          Left = 0
          Top = 145
          Width = 481
          Height = 53
        end
        object RLDraw16: TRLDraw
          Left = 0
          Top = 197
          Width = 481
          Height = 64
        end
        object RLLabel1: TRLLabel
          Left = 43
          Top = 5
          Width = 398
          Height = 16
          Alignment = taCenter
          Caption = 'Guia Nacional de Recolhimento de Tributos Estaduais - GNRE '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel2: TRLLabel
          Left = 3
          Top = 31
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Contribuinte Emitente'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel3: TRLLabel
          Left = 3
          Top = 41
          Width = 49
          Height = 10
          Caption = 'Raz'#227'o Social'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel4: TRLLabel
          Left = 376
          Top = 41
          Width = 79
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est.:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel5: TRLLabel
          Left = 3
          Top = 65
          Width = 40
          Height = 10
          Caption = 'Endere'#231'o:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel6: TRLLabel
          Left = 487
          Top = 3
          Width = 56
          Height = 10
          Caption = 'UF Favorecida'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel7: TRLLabel
          Left = 585
          Top = 3
          Width = 70
          Height = 10
          Caption = 'C'#243'digo da Receita'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel8: TRLLabel
          Left = 3
          Top = 78
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel9: TRLLabel
          Left = 376
          Top = 78
          Width = 16
          Height = 10
          Caption = 'UF:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel10: TRLLabel
          Left = 3
          Top = 91
          Width = 21
          Height = 10
          Caption = 'CEP:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel11: TRLLabel
          Left = 355
          Top = 91
          Width = 37
          Height = 10
          Caption = 'Telefone:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel12: TRLLabel
          Left = 487
          Top = 29
          Width = 57
          Height = 10
          Caption = 'N'#176' de Controle'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel13: TRLLabel
          Left = 487
          Top = 55
          Width = 76
          Height = 10
          Caption = 'Data de Vencimento'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel14: TRLLabel
          Left = 487
          Top = 81
          Width = 97
          Height = 10
          Caption = 'N'#176' Documento de Origem'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel15: TRLLabel
          Left = 3
          Top = 107
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Destinat'#225'rio'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel16: TRLLabel
          Left = 3
          Top = 118
          Width = 77
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel17: TRLLabel
          Left = 3
          Top = 131
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel18: TRLLabel
          Left = 86
          Top = 118
          Width = 389
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel19: TRLLabel
          Left = 3
          Top = 148
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Reservado '#224' Fiscaliza'#231#227'o'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel20: TRLLabel
          Left = 3
          Top = 166
          Width = 81
          Height = 10
          Caption = 'Conv'#234'nio/Protocolo:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel21: TRLLabel
          Left = 3
          Top = 179
          Width = 36
          Height = 10
          Caption = 'Produto:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel22: TRLLabel
          Left = 3
          Top = 200
          Width = 116
          Height = 10
          Caption = 'Informa'#231#245'es Complementares:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel23: TRLLabel
          Left = 3
          Top = 247
          Width = 149
          Height = 10
          Caption = 'Documento V'#225'lido para pagamento at'#233':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel24: TRLLabel
          Left = 487
          Top = 107
          Width = 83
          Height = 10
          Caption = 'Per'#237'odo de Refer'#234'ncia'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel25: TRLLabel
          Left = 594
          Top = 107
          Width = 28
          Height = 10
          Caption = 'Parcela'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel26: TRLLabel
          Left = 594
          Top = 118
          Width = 103
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel27: TRLLabel
          Left = 487
          Top = 133
          Width = 55
          Height = 10
          Caption = 'Valor Principal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel28: TRLLabel
          Left = 487
          Top = 142
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel30: TRLLabel
          Left = 487
          Top = 169
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel32: TRLLabel
          Left = 487
          Top = 194
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel33: TRLLabel
          Left = 487
          Top = 211
          Width = 23
          Height = 10
          Caption = 'Multa'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel34: TRLLabel
          Left = 487
          Top = 220
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel35: TRLLabel
          Left = 487
          Top = 237
          Width = 62
          Height = 10
          Caption = 'Total a Recolher'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel36: TRLLabel
          Left = 487
          Top = 246
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel37: TRLLabel
          Left = 650
          Top = 263
          Width = 47
          Height = 7
          Caption = '1'#170' via - Banco'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLMemo1: TRLMemo
          Left = 702
          Top = 177
          Width = 8
          Height = 84
          Alignment = taJustify
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'A'
            'u'
            't'
            'e'
            'n'
            't'
            'i'
            'c'
            'a'
            #231
            #227
            'o')
          ParentFont = False
        end
        object RLLabel29: TRLLabel
          Left = 487
          Top = 160
          Width = 83
          Height = 10
          Caption = 'Atualiza'#231#227'o Monet'#225'ria'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel31: TRLLabel
          Left = 487
          Top = 185
          Width = 23
          Height = 10
          Caption = 'Juros'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLDraw18: TRLDraw
          Left = 0
          Top = 330
          Width = 481
          Height = 27
        end
        object RLDraw19: TRLDraw
          Left = 0
          Top = 434
          Width = 481
          Height = 42
        end
        object RLDraw20: TRLDraw
          Left = 0
          Top = 356
          Width = 481
          Height = 79
        end
        object RLDraw21: TRLDraw
          Left = 589
          Top = 434
          Width = 110
          Height = 27
        end
        object RLDraw22: TRLDraw
          Left = 480
          Top = 330
          Width = 89
          Height = 27
        end
        object RLDraw23: TRLDraw
          Left = 568
          Top = 330
          Width = 131
          Height = 27
        end
        object RLDraw24: TRLDraw
          Left = 480
          Top = 356
          Width = 219
          Height = 27
        end
        object RLDraw25: TRLDraw
          Left = 480
          Top = 382
          Width = 219
          Height = 27
        end
        object RLDraw26: TRLDraw
          Left = 480
          Top = 408
          Width = 219
          Height = 27
        end
        object RLDraw27: TRLDraw
          Left = 480
          Top = 434
          Width = 110
          Height = 27
        end
        object RLDraw28: TRLDraw
          Left = 480
          Top = 460
          Width = 219
          Height = 27
        end
        object RLDraw29: TRLDraw
          Left = 480
          Top = 486
          Width = 219
          Height = 27
        end
        object RLDraw30: TRLDraw
          Left = 480
          Top = 512
          Width = 219
          Height = 27
        end
        object RLDraw31: TRLDraw
          Left = 480
          Top = 538
          Width = 219
          Height = 27
        end
        object RLDraw32: TRLDraw
          Left = 480
          Top = 564
          Width = 219
          Height = 27
        end
        object RLDraw33: TRLDraw
          Left = 0
          Top = 475
          Width = 481
          Height = 53
        end
        object RLDraw34: TRLDraw
          Left = 0
          Top = 527
          Width = 481
          Height = 64
        end
        object RLLabel38: TRLLabel
          Left = 43
          Top = 335
          Width = 398
          Height = 16
          Alignment = taCenter
          Caption = 'Guia Nacional de Recolhimento de Tributos Estaduais - GNRE '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel39: TRLLabel
          Left = 3
          Top = 361
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Contribuinte Emitente'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel40: TRLLabel
          Left = 3
          Top = 371
          Width = 49
          Height = 10
          Caption = 'Raz'#227'o Social'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel41: TRLLabel
          Left = 376
          Top = 371
          Width = 79
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est.:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel42: TRLLabel
          Left = 3
          Top = 395
          Width = 40
          Height = 10
          Caption = 'Endere'#231'o:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel43: TRLLabel
          Left = 487
          Top = 333
          Width = 56
          Height = 10
          Caption = 'UF Favorecida'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel44: TRLLabel
          Left = 585
          Top = 333
          Width = 70
          Height = 10
          Caption = 'C'#243'digo da Receita'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel45: TRLLabel
          Left = 3
          Top = 408
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel46: TRLLabel
          Left = 376
          Top = 408
          Width = 16
          Height = 10
          Caption = 'UF:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel47: TRLLabel
          Left = 3
          Top = 421
          Width = 21
          Height = 10
          Caption = 'CEP:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel48: TRLLabel
          Left = 355
          Top = 421
          Width = 37
          Height = 10
          Caption = 'Telefone:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel49: TRLLabel
          Left = 487
          Top = 359
          Width = 57
          Height = 10
          Caption = 'N'#176' de Controle'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel50: TRLLabel
          Left = 487
          Top = 385
          Width = 76
          Height = 10
          Caption = 'Data de Vencimento'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel51: TRLLabel
          Left = 487
          Top = 411
          Width = 97
          Height = 10
          Caption = 'N'#176' Documento de Origem'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel52: TRLLabel
          Left = 3
          Top = 437
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Destinat'#225'rio'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel53: TRLLabel
          Left = 3
          Top = 448
          Width = 77
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel54: TRLLabel
          Left = 3
          Top = 461
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel55: TRLLabel
          Left = 86
          Top = 448
          Width = 389
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel56: TRLLabel
          Left = 3
          Top = 478
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Reservado '#224' Fiscaliza'#231#227'o'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel57: TRLLabel
          Left = 3
          Top = 496
          Width = 81
          Height = 10
          Caption = 'Conv'#234'nio/Protocolo:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel58: TRLLabel
          Left = 3
          Top = 509
          Width = 36
          Height = 10
          Caption = 'Produto:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel59: TRLLabel
          Left = 3
          Top = 530
          Width = 116
          Height = 10
          Caption = 'Informa'#231#245'es Complementares:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel60: TRLLabel
          Left = 3
          Top = 577
          Width = 149
          Height = 10
          Caption = 'Documento V'#225'lido para pagamento at'#233':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel61: TRLLabel
          Left = 487
          Top = 437
          Width = 83
          Height = 10
          Caption = 'Per'#237'odo de Refer'#234'ncia'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel62: TRLLabel
          Left = 594
          Top = 437
          Width = 28
          Height = 10
          Caption = 'Parcela'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel63: TRLLabel
          Left = 594
          Top = 448
          Width = 103
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel64: TRLLabel
          Left = 487
          Top = 463
          Width = 55
          Height = 10
          Caption = 'Valor Principal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel65: TRLLabel
          Left = 487
          Top = 472
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel66: TRLLabel
          Left = 487
          Top = 499
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel67: TRLLabel
          Left = 487
          Top = 524
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel68: TRLLabel
          Left = 487
          Top = 541
          Width = 23
          Height = 10
          Caption = 'Multa'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel69: TRLLabel
          Left = 487
          Top = 550
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel70: TRLLabel
          Left = 487
          Top = 567
          Width = 62
          Height = 10
          Caption = 'Total a Recolher'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel71: TRLLabel
          Left = 487
          Top = 576
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel72: TRLLabel
          Left = 632
          Top = 593
          Width = 65
          Height = 7
          Caption = '2'#170' via - Contribuinte'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLMemo4: TRLMemo
          Left = 702
          Top = 507
          Width = 8
          Height = 84
          Alignment = taJustify
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'A'
            'u'
            't'
            'e'
            'n'
            't'
            'i'
            'c'
            'a'
            #231
            #227
            'o')
          ParentFont = False
        end
        object RLLabel73: TRLLabel
          Left = 487
          Top = 490
          Width = 83
          Height = 10
          Caption = 'Atualiza'#231#227'o Monet'#225'ria'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel74: TRLLabel
          Left = 487
          Top = 515
          Width = 23
          Height = 10
          Caption = 'Juros'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RazaoSocialEmitente: TRLLabel
          Left = 3
          Top = 52
          Width = 367
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DocEmitente: TRLLabel
          Left = 376
          Top = 52
          Width = 57
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object EnderecoEmitente: TRLLabel
          Left = 52
          Top = 65
          Width = 389
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object UFFavorecida: TRLLabel
          Left = 487
          Top = 14
          Width = 68
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CodReceita: TRLLabel
          Left = 585
          Top = 14
          Width = 52
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object MunicipioEmitente: TRLLabel
          Left = 52
          Top = 78
          Width = 318
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object UFEmitente: TRLLabel
          Left = 398
          Top = 78
          Width = 53
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CEPEmitente: TRLLabel
          Left = 52
          Top = 91
          Width = 59
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object TelefoneEmitente: TRLLabel
          Left = 398
          Top = 91
          Width = 77
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object NumeroControle: TRLLabel
          Left = 585
          Top = 37
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DataVencimento: TRLLabel
          Left = 585
          Top = 63
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object NumDocOrigem: TRLLabel
          Left = 585
          Top = 89
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object MunicipioDestinatario: TRLLabel
          Left = 52
          Top = 131
          Width = 423
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Convenio: TRLLabel
          Left = 90
          Top = 166
          Width = 385
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Produto: TRLLabel
          Left = 45
          Top = 179
          Width = 430
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DataLimitePagamento: TRLLabel
          Left = 167
          Top = 247
          Width = 307
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RepresentacaoNumerica: TRLLabel
          Left = 3
          Top = 263
          Width = 472
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RazaoSocialEmitente2: TRLLabel
          Left = 3
          Top = 382
          Width = 367
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DocEmitente2: TRLLabel
          Left = 376
          Top = 382
          Width = 57
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object EnderecoEmitente2: TRLLabel
          Left = 52
          Top = 395
          Width = 389
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object UFFavorecida2: TRLLabel
          Left = 487
          Top = 344
          Width = 68
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CodReceita2: TRLLabel
          Left = 585
          Top = 344
          Width = 52
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object MunicipioEmitente2: TRLLabel
          Left = 52
          Top = 408
          Width = 318
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object UFEmitente2: TRLLabel
          Left = 398
          Top = 408
          Width = 53
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CEPEmitente2: TRLLabel
          Left = 52
          Top = 421
          Width = 59
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object TelefoneEmitente2: TRLLabel
          Left = 398
          Top = 421
          Width = 77
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object NumeroControle2: TRLLabel
          Left = 585
          Top = 364
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DataVencimento2: TRLLabel
          Left = 585
          Top = 393
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object NumDocOrigem2: TRLLabel
          Left = 585
          Top = 419
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object MunicipioDestinatario2: TRLLabel
          Left = 52
          Top = 461
          Width = 423
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Convenio2: TRLLabel
          Left = 90
          Top = 496
          Width = 385
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Produto2: TRLLabel
          Left = 45
          Top = 509
          Width = 430
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object DataLimitePagamento2: TRLLabel
          Left = 167
          Top = 577
          Width = 308
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RepresentacaoNumerica2: TRLLabel
          Left = 2
          Top = 593
          Width = 473
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object PerMesAnoRef2: TRLLabel
          Left = 487
          Top = 448
          Width = 97
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object PerMesAnoRef: TRLLabel
          Left = 487
          Top = 118
          Width = 97
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CodigoBarras: TRLBarcode
          Left = 3
          Top = 279
          Width = 472
          Height = 45
          Margins.LeftMargin = 1.000000000000000000
          Margins.RightMargin = 1.000000000000000000
          Alignment = taCenter
          AutoSize = False
          Ratio = 3.000000000000000000
        end
        object CodigoBarras2: TRLBarcode
          Left = 3
          Top = 609
          Width = 472
          Height = 45
          Margins.LeftMargin = 1.000000000000000000
          Margins.RightMargin = 1.000000000000000000
          Alignment = taCenter
          AutoSize = False
          Ratio = 3.000000000000000000
        end
        object InfoComplementares: TRLMemo
          Left = 125
          Top = 200
          Width = 350
          Height = 41
          AutoSize = False
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object InfoComplementares2: TRLMemo
          Left = 125
          Top = 530
          Width = 350
          Height = 41
          AutoSize = False
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLDraw35: TRLDraw
          Left = 1
          Top = 666
          Width = 480
          Height = 27
        end
        object RLLabel75: TRLLabel
          Left = 44
          Top = 671
          Width = 398
          Height = 16
          Alignment = taCenter
          Caption = 'Guia Nacional de Recolhimento de Tributos Estaduais - GNRE '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLDraw39: TRLDraw
          Left = 480
          Top = 666
          Width = 89
          Height = 27
        end
        object RLDraw40: TRLDraw
          Left = 568
          Top = 666
          Width = 131
          Height = 27
        end
        object RLLabel80: TRLLabel
          Left = 487
          Top = 669
          Width = 56
          Height = 10
          Caption = 'UF Favorecida'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object UFFavorecida3: TRLLabel
          Left = 487
          Top = 680
          Width = 68
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CodReceita3: TRLLabel
          Left = 585
          Top = 680
          Width = 52
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel81: TRLLabel
          Left = 585
          Top = 669
          Width = 70
          Height = 10
          Caption = 'C'#243'digo da Receita'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel86: TRLLabel
          Left = 487
          Top = 695
          Width = 57
          Height = 10
          Caption = 'N'#176' de Controle'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object NumeroControle3: TRLLabel
          Left = 585
          Top = 703
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel87: TRLLabel
          Left = 487
          Top = 721
          Width = 76
          Height = 10
          Caption = 'Data de Vencimento'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object DataVencimento3: TRLLabel
          Left = 585
          Top = 729
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel88: TRLLabel
          Left = 487
          Top = 747
          Width = 97
          Height = 10
          Caption = 'N'#176' Documento de Origem'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object NumDocOrigem3: TRLLabel
          Left = 585
          Top = 755
          Width = 112
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel98: TRLLabel
          Left = 487
          Top = 773
          Width = 83
          Height = 10
          Caption = 'Per'#237'odo de Refer'#234'ncia'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object PerMesAnoRef3: TRLLabel
          Left = 487
          Top = 784
          Width = 97
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel99: TRLLabel
          Left = 594
          Top = 773
          Width = 28
          Height = 10
          Caption = 'Parcela'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel100: TRLLabel
          Left = 594
          Top = 784
          Width = 103
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel101: TRLLabel
          Left = 487
          Top = 799
          Width = 55
          Height = 10
          Caption = 'Valor Principal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel102: TRLLabel
          Left = 487
          Top = 808
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel110: TRLLabel
          Left = 487
          Top = 826
          Width = 83
          Height = 10
          Caption = 'Atualiza'#231#227'o Monet'#225'ria'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel103: TRLLabel
          Left = 487
          Top = 835
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel111: TRLLabel
          Left = 487
          Top = 851
          Width = 23
          Height = 10
          Caption = 'Juros'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel104: TRLLabel
          Left = 487
          Top = 860
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel105: TRLLabel
          Left = 487
          Top = 877
          Width = 23
          Height = 10
          Caption = 'Multa'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel106: TRLLabel
          Left = 487
          Top = 886
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel107: TRLLabel
          Left = 487
          Top = 903
          Width = 62
          Height = 10
          Caption = 'Total a Recolher'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel108: TRLLabel
          Left = 487
          Top = 912
          Width = 210
          Height = 12
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel109: TRLLabel
          Left = 612
          Top = 929
          Width = 85
          Height = 7
          Caption = '3'#170' via - Contribuinte/Fisco'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLMemo6: TRLMemo
          Left = 702
          Top = 843
          Width = 8
          Height = 84
          Alignment = taJustify
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'A'
            'u'
            't'
            'e'
            'n'
            't'
            'i'
            'c'
            'a'
            #231
            #227
            'o')
          ParentFont = False
        end
        object RLLabel76: TRLLabel
          Left = 4
          Top = 697
          Width = 471
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Contribuinte Emitente'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel77: TRLLabel
          Left = 4
          Top = 707
          Width = 49
          Height = 10
          Caption = 'Raz'#227'o Social'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel79: TRLLabel
          Left = 4
          Top = 731
          Width = 40
          Height = 10
          Caption = 'Endere'#231'o:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object EnderecoEmitente3: TRLLabel
          Left = 52
          Top = 731
          Width = 389
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel78: TRLLabel
          Left = 377
          Top = 707
          Width = 79
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est.:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object DocEmitente3: TRLLabel
          Left = 377
          Top = 721
          Width = 57
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RazaoSocialEmitente3: TRLLabel
          Left = 4
          Top = 718
          Width = 367
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel83: TRLLabel
          Left = 377
          Top = 744
          Width = 16
          Height = 10
          Caption = 'UF:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object UFEmitente3: TRLLabel
          Left = 399
          Top = 744
          Width = 53
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object TelefoneEmitente3: TRLLabel
          Left = 399
          Top = 757
          Width = 76
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel85: TRLLabel
          Left = 356
          Top = 757
          Width = 37
          Height = 10
          Caption = 'Telefone:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object MunicipioEmitente3: TRLLabel
          Left = 53
          Top = 744
          Width = 318
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel82: TRLLabel
          Left = 4
          Top = 744
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel84: TRLLabel
          Left = 4
          Top = 757
          Width = 21
          Height = 10
          Caption = 'CEP:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object CEPEmitente3: TRLLabel
          Left = 53
          Top = 757
          Width = 59
          Height = 12
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel89: TRLLabel
          Left = 4
          Top = 773
          Width = 471
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Dados do Destinat'#225'rio'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel90: TRLLabel
          Left = 4
          Top = 784
          Width = 77
          Height = 10
          Caption = 'CNPJ/CPF/Insc. Est:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel92: TRLLabel
          Left = 87
          Top = 784
          Width = 388
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object MunicipioDestinatario3: TRLLabel
          Left = 53
          Top = 797
          Width = 422
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel93: TRLLabel
          Left = 4
          Top = 814
          Width = 471
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Caption = 'Reservado '#224' Fiscaliza'#231#227'o'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel94: TRLLabel
          Left = 4
          Top = 832
          Width = 81
          Height = 10
          Caption = 'Conv'#234'nio/Protocolo:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Convenio3: TRLLabel
          Left = 91
          Top = 832
          Width = 384
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel95: TRLLabel
          Left = 4
          Top = 845
          Width = 36
          Height = 10
          Caption = 'Produto:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Produto3: TRLLabel
          Left = 46
          Top = 845
          Width = 429
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel91: TRLLabel
          Left = 4
          Top = 797
          Width = 42
          Height = 10
          Caption = 'Munic'#237'pio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLLabel96: TRLLabel
          Left = 4
          Top = 866
          Width = 116
          Height = 10
          Caption = 'Informa'#231#245'es Complementares:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object InfoComplementares3: TRLMemo
          Left = 126
          Top = 866
          Width = 349
          Height = 41
          AutoSize = False
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RLLabel97: TRLLabel
          Left = 4
          Top = 913
          Width = 149
          Height = 10
          Caption = 'Documento V'#225'lido para pagamento at'#233':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object DataLimitePagamento3: TRLLabel
          Left = 167
          Top = 913
          Width = 308
          Height = 12
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object RepresentacaoNumerica3: TRLLabel
          Left = 4
          Top = 929
          Width = 471
          Height = 10
          Alignment = taCenter
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object CodigoBarras3: TRLBarcode
          Left = 4
          Top = 945
          Width = 472
          Height = 45
          Margins.LeftMargin = 1.000000000000000000
          Margins.RightMargin = 1.000000000000000000
          Alignment = taCenter
          AutoSize = False
          Ratio = 3.000000000000000000
        end
        object pnlMsgPIX: TRLPanel
          Left = 524
          Top = 269
          Width = 67
          Height = 55
          Borders.Sides = sdCustom
          Borders.DrawLeft = True
          Borders.DrawTop = True
          Borders.DrawRight = True
          Borders.DrawBottom = True
          Visible = False
          object msgQrCodePIX: TRLMemo
            Left = 1
            Top = 1
            Width = 65
            Height = 53
            Align = faClient
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Layout = tlCenter
            Lines.Strings = (
              'UF n'#227'o emite QR Code PIX')
          end
        end
        object pnlMsgPIX2: TRLPanel
          Left = 525
          Top = 599
          Width = 67
          Height = 55
          Borders.Sides = sdCustom
          Borders.DrawLeft = True
          Borders.DrawTop = True
          Borders.DrawRight = True
          Borders.DrawBottom = True
          Visible = False
          object msgQrCodePIX2: TRLMemo
            Left = 1
            Top = 1
            Width = 65
            Height = 53
            Align = faClient
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Layout = tlCenter
            Lines.Strings = (
              'UF n'#227'o emite QR Code PIX')
          end
        end
        object pnlMsgPIX3: TRLPanel
          Left = 524
          Top = 935
          Width = 67
          Height = 55
          Borders.Sides = sdCustom
          Borders.DrawLeft = True
          Borders.DrawTop = True
          Borders.DrawRight = True
          Borders.DrawBottom = True
          Visible = False
          object msgQrCodePIX3: TRLMemo
            Left = 1
            Top = 1
            Width = 65
            Height = 53
            Align = faClient
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Layout = tlCenter
            Lines.Strings = (
              'UF n'#227'o emite QR Code PIX')
          end
        end
      end
    end
  end
  inherited RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#169' 1999-200' +
      '8 Fortes Inform'#225'tica'
    DisplayName = 'ACBrGNRE PDF - http://acbr.sf.net'
    Left = 128
    Top = 8
  end
end
