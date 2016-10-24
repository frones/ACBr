inherited frlGuiaRLRetrato: TfrlGuiaRLRetrato
  Left = 0
  Top = 0
  Caption = 'ACBrGNREGuiaFR_Fortes'
  ClientHeight = 364
  ClientWidth = 759
  Font.Name = 'Tahoma'
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inherited RLGNRe: TRLReport
    Left = 0
    Top = 0
    Margins.LeftMargin = 6.000000000000000000
    Margins.TopMargin = 6.000000000000000000
    Margins.RightMargin = 6.000000000000000000
    Margins.BottomMargin = 6.000000000000000000
    Font.Color = clBlack

    object RLBand1: TRLBand
      Left = 23
      Top = 23
      Width = 748
      Height = 995
      BeforePrint = RLBand1BeforePrint
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
      object RLDBText1: TRLDBText
        Left = 3
        Top = 52
        Width = 367
        Height = 12
        AutoSize = False
        DataField = 'RazaoSocialEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText2: TRLDBText
        Left = 376
        Top = 52
        Width = 57
        Height = 12
        DataField = 'DocEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText3: TRLDBText
        Left = 52
        Top = 65
        Width = 389
        Height = 12
        AutoSize = False
        DataField = 'EnderecoEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText4: TRLDBText
        Left = 487
        Top = 14
        Width = 63
        Height = 12
        DataField = 'UFFavorecida'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText5: TRLDBText
        Left = 585
        Top = 14
        Width = 52
        Height = 12
        DataField = 'CodReceita'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText6: TRLDBText
        Left = 52
        Top = 78
        Width = 318
        Height = 12
        AutoSize = False
        DataField = 'MunicipioEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText7: TRLDBText
        Left = 398
        Top = 78
        Width = 53
        Height = 12
        DataField = 'UFEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText8: TRLDBText
        Left = 52
        Top = 91
        Width = 59
        Height = 12
        DataField = 'CEPEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText9: TRLDBText
        Left = 398
        Top = 91
        Width = 77
        Height = 12
        DataField = 'TelefoneEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText10: TRLDBText
        Left = 585
        Top = 37
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'NumeroControle'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText11: TRLDBText
        Left = 585
        Top = 63
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'DataVencimento'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText12: TRLDBText
        Left = 585
        Top = 89
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'NumDocOrigem'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText14: TRLDBText
        Left = 52
        Top = 131
        Width = 423
        Height = 12
        AutoSize = False
        DataField = 'MunicipioDestinatario'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText13: TRLDBText
        Left = 90
        Top = 166
        Width = 385
        Height = 12
        AutoSize = False
        DataField = 'Convenio'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText15: TRLDBText
        Left = 45
        Top = 179
        Width = 430
        Height = 12
        AutoSize = False
        DataField = 'Produto'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBMemo1: TRLDBMemo
        Left = 125
        Top = 200
        Width = 350
        Height = 41
        AutoSize = False
        Behavior = [beSiteExpander]
        DataField = 'InfoComplementares'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText16: TRLDBText
        Left = 158
        Top = 247
        Width = 317
        Height = 12
        AutoSize = False
        DataField = 'DataLimitePagamento'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText17: TRLDBText
        Left = 3
        Top = 263
        Width = 472
        Height = 10
        Alignment = taCenter
        AutoSize = False
        DataField = 'RepresentacaoNumerica'
        DataSource = dsItens
        DisplayMask = '00000000000 0 00000000000 0 00000000000 0 00000000000 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBBarcode1: TRLDBBarcode
        Left = 3
        Top = 279
        Width = 472
        Height = 45
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        Alignment = taCenter
        AutoSize = False
        DataField = 'CodigoBarras'
        DataSource = dsItens
        Ratio = 3.000000000000000000
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
      object RLDBText18: TRLDBText
        Left = 487
        Top = 118
        Width = 97
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'PeriodoReferencia'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
        Left = 652
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
        Left = 703
        Top = 180
        Width = 8
        Height = 77
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
      object RLDBText19: TRLDBText
        Left = 3
        Top = 382
        Width = 367
        Height = 12
        AutoSize = False
        DataField = 'RazaoSocialEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText20: TRLDBText
        Left = 376
        Top = 382
        Width = 57
        Height = 12
        DataField = 'DocEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText21: TRLDBText
        Left = 52
        Top = 395
        Width = 389
        Height = 12
        AutoSize = False
        DataField = 'EnderecoEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText22: TRLDBText
        Left = 487
        Top = 344
        Width = 63
        Height = 12
        DataField = 'UFFavorecida'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText23: TRLDBText
        Left = 585
        Top = 344
        Width = 52
        Height = 12
        DataField = 'CodReceita'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText24: TRLDBText
        Left = 52
        Top = 408
        Width = 318
        Height = 12
        AutoSize = False
        DataField = 'MunicipioEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText25: TRLDBText
        Left = 398
        Top = 408
        Width = 53
        Height = 12
        DataField = 'UFEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText26: TRLDBText
        Left = 52
        Top = 421
        Width = 59
        Height = 12
        DataField = 'CEPEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText27: TRLDBText
        Left = 398
        Top = 421
        Width = 77
        Height = 12
        DataField = 'TelefoneEmitente'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText28: TRLDBText
        Left = 585
        Top = 367
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'NumeroControle'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText29: TRLDBText
        Left = 585
        Top = 393
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'DataVencimento'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText30: TRLDBText
        Left = 585
        Top = 419
        Width = 112
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'NumDocOrigem'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText31: TRLDBText
        Left = 52
        Top = 461
        Width = 423
        Height = 12
        AutoSize = False
        DataField = 'MunicipioDestinatario'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText32: TRLDBText
        Left = 90
        Top = 496
        Width = 385
        Height = 12
        AutoSize = False
        DataField = 'Convenio'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText33: TRLDBText
        Left = 45
        Top = 509
        Width = 430
        Height = 12
        AutoSize = False
        DataField = 'Produto'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBMemo2: TRLDBMemo
        Left = 125
        Top = 530
        Width = 350
        Height = 41
        AutoSize = False
        Behavior = [beSiteExpander]
        DataField = 'InfoComplementares'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
      object RLDBText34: TRLDBText
        Left = 158
        Top = 577
        Width = 317
        Height = 12
        AutoSize = False
        DataField = 'DataLimitePagamento'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText35: TRLDBText
        Left = 3
        Top = 593
        Width = 472
        Height = 10
        Alignment = taCenter
        AutoSize = False
        DataField = 'RepresentacaoNumerica'
        DataSource = dsItens
        DisplayMask = '00000000000 0 00000000000 0 00000000000 0 00000000000 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBBarcode2: TRLDBBarcode
        Left = 3
        Top = 609
        Width = 472
        Height = 45
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        Alignment = taCenter
        AutoSize = False
        DataField = 'CodigoBarras'
        DataSource = dsItens
        Ratio = 3.000000000000000000
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
      object RLDBText36: TRLDBText
        Left = 487
        Top = 448
        Width = 97
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        DataField = 'PeriodoReferencia'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
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
        Top = 510
        Width = 8
        Height = 77
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
    end
    object RLDraw35: TRLDraw
      Left = 38
      Top = 698
      Width = 481
      Height = 27
    end
    object RLDraw36: TRLDraw
      Left = 38
      Top = 802
      Width = 481
      Height = 42
    end
    object RLDraw37: TRLDraw
      Left = 38
      Top = 724
      Width = 481
      Height = 79
    end
    object RLDraw38: TRLDraw
      Left = 627
      Top = 802
      Width = 110
      Height = 27
    end
    object RLDraw39: TRLDraw
      Left = 518
      Top = 698
      Width = 89
      Height = 27
    end
    object RLDraw40: TRLDraw
      Left = 606
      Top = 698
      Width = 131
      Height = 27
    end
    object RLDraw41: TRLDraw
      Left = 518
      Top = 724
      Width = 219
      Height = 27
    end
    object RLDraw42: TRLDraw
      Left = 518
      Top = 750
      Width = 219
      Height = 27
    end
    object RLDraw43: TRLDraw
      Left = 518
      Top = 776
      Width = 219
      Height = 27
    end
    object RLDraw44: TRLDraw
      Left = 518
      Top = 802
      Width = 110
      Height = 27
    end
    object RLDraw45: TRLDraw
      Left = 518
      Top = 828
      Width = 219
      Height = 27
    end
    object RLDraw46: TRLDraw
      Left = 518
      Top = 854
      Width = 219
      Height = 27
    end
    object RLDraw47: TRLDraw
      Left = 518
      Top = 880
      Width = 219
      Height = 27
    end
    object RLDraw48: TRLDraw
      Left = 518
      Top = 906
      Width = 219
      Height = 27
    end
    object RLDraw49: TRLDraw
      Left = 518
      Top = 932
      Width = 219
      Height = 27
    end
    object RLDraw50: TRLDraw
      Left = 38
      Top = 843
      Width = 481
      Height = 53
    end
    object RLDraw51: TRLDraw
      Left = 38
      Top = 895
      Width = 481
      Height = 64
    end
    object RLMemo5: TRLMemo
      Left = 46
      Top = 706
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
    object RLLabel75: TRLLabel
      Left = 81
      Top = 703
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
    object RLLabel76: TRLLabel
      Left = 41
      Top = 729
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
    object RLLabel77: TRLLabel
      Left = 41
      Top = 739
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
    object RLDBText37: TRLDBText
      Left = 41
      Top = 750
      Width = 367
      Height = 12
      AutoSize = False
      DataField = 'RazaoSocialEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel78: TRLLabel
      Left = 414
      Top = 739
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
    object RLDBText38: TRLDBText
      Left = 414
      Top = 750
      Width = 57
      Height = 12
      DataField = 'DocEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel79: TRLLabel
      Left = 41
      Top = 763
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
    object RLDBText39: TRLDBText
      Left = 90
      Top = 763
      Width = 389
      Height = 12
      AutoSize = False
      DataField = 'EnderecoEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel80: TRLLabel
      Left = 525
      Top = 701
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
    object RLDBText40: TRLDBText
      Left = 525
      Top = 712
      Width = 63
      Height = 12
      DataField = 'UFFavorecida'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel81: TRLLabel
      Left = 623
      Top = 701
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
    object RLDBText41: TRLDBText
      Left = 623
      Top = 712
      Width = 52
      Height = 12
      DataField = 'CodReceita'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel82: TRLLabel
      Left = 41
      Top = 776
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
    object RLDBText42: TRLDBText
      Left = 90
      Top = 776
      Width = 318
      Height = 12
      AutoSize = False
      DataField = 'MunicipioEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel83: TRLLabel
      Left = 414
      Top = 776
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
    object RLDBText43: TRLDBText
      Left = 436
      Top = 776
      Width = 53
      Height = 12
      DataField = 'UFEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel84: TRLLabel
      Left = 41
      Top = 789
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
    object RLDBText44: TRLDBText
      Left = 90
      Top = 789
      Width = 59
      Height = 12
      DataField = 'CEPEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel85: TRLLabel
      Left = 393
      Top = 789
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
    object RLDBText45: TRLDBText
      Left = 436
      Top = 789
      Width = 77
      Height = 12
      DataField = 'TelefoneEmitente'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel86: TRLLabel
      Left = 525
      Top = 727
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
    object RLDBText46: TRLDBText
      Left = 623
      Top = 735
      Width = 112
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      DataField = 'NumeroControle'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel87: TRLLabel
      Left = 525
      Top = 753
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
    object RLDBText47: TRLDBText
      Left = 623
      Top = 761
      Width = 112
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      DataField = 'DataVencimento'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel88: TRLLabel
      Left = 525
      Top = 779
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
    object RLDBText48: TRLDBText
      Left = 623
      Top = 787
      Width = 112
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      DataField = 'NumDocOrigem'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel89: TRLLabel
      Left = 41
      Top = 805
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
    object RLLabel90: TRLLabel
      Left = 41
      Top = 816
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
    object RLLabel91: TRLLabel
      Left = 41
      Top = 829
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
    object RLDBText49: TRLDBText
      Left = 90
      Top = 829
      Width = 423
      Height = 12
      AutoSize = False
      DataField = 'MunicipioDestinatario'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel92: TRLLabel
      Left = 124
      Top = 816
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
    object RLLabel93: TRLLabel
      Left = 41
      Top = 846
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
    object RLLabel94: TRLLabel
      Left = 41
      Top = 864
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
    object RLLabel95: TRLLabel
      Left = 41
      Top = 877
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
    object RLDBText50: TRLDBText
      Left = 128
      Top = 864
      Width = 385
      Height = 12
      AutoSize = False
      DataField = 'Convenio'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLDBText51: TRLDBText
      Left = 83
      Top = 877
      Width = 430
      Height = 12
      AutoSize = False
      DataField = 'Produto'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel96: TRLLabel
      Left = 41
      Top = 898
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
    object RLDBMemo3: TRLDBMemo
      Left = 163
      Top = 898
      Width = 350
      Height = 41
      AutoSize = False
      Behavior = [beSiteExpander]
      DataField = 'InfoComplementares'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel97: TRLLabel
      Left = 41
      Top = 945
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
    object RLDBText52: TRLDBText
      Left = 196
      Top = 945
      Width = 317
      Height = 12
      AutoSize = False
      DataField = 'DataLimitePagamento'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLDBText53: TRLDBText
      Left = 41
      Top = 961
      Width = 472
      Height = 10
      Alignment = taCenter
      AutoSize = False
      DataField = 'RepresentacaoNumerica'
      DataSource = dsItens
      DisplayMask = '00000000000 0 00000000000 0 00000000000 0 00000000000 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLDBBarcode3: TRLDBBarcode
      Left = 41
      Top = 977
      Width = 472
      Height = 45
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      Alignment = taCenter
      AutoSize = False
      DataField = 'CodigoBarras'
      DataSource = dsItens
      Ratio = 3.000000000000000000
    end
    object RLLabel98: TRLLabel
      Left = 525
      Top = 805
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
    object RLDBText54: TRLDBText
      Left = 525
      Top = 816
      Width = 97
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      DataField = 'PeriodoReferencia'
      DataSource = dsItens
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RLLabel99: TRLLabel
      Left = 632
      Top = 805
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
      Left = 632
      Top = 816
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
      Left = 525
      Top = 831
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
      Left = 525
      Top = 840
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
    object RLLabel103: TRLLabel
      Left = 525
      Top = 867
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
    object RLLabel104: TRLLabel
      Left = 525
      Top = 892
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
      Left = 525
      Top = 909
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
      Left = 525
      Top = 918
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
      Left = 525
      Top = 935
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
      Left = 525
      Top = 944
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
      Left = 650
      Top = 961
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
      Left = 741
      Top = 878
      Width = 8
      Height = 77
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
        #231
        #227
        'o')
      ParentFont = False
    end
    object RLLabel110: TRLLabel
      Left = 525
      Top = 858
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
    object RLLabel111: TRLLabel
      Left = 525
      Top = 883
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
  end
  inherited RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#169' 1999-200' +
      '8 Fortes Inform'#225'tica'
    DisplayName = 'ACBrGNRE PDF - http://acbr.sf.net'
    Left = 128
    Top = 8
  end
  inherited dsItens: TDataSource
    Left = 124
    Top = 53
  end
end
