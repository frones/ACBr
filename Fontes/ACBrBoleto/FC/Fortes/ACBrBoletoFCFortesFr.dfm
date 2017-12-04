object ACBRBoletoFCFortesFr: TACBRBoletoFCFortesFr
  Left = 248
  Top = 138
  Width = 1628
  Height = 893
  Caption = 'ACBRBoletoFCFortesFr'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LayoutBoleto: TRLReport
    Left = 14
    Top = 11
    Width = 794
    Height = 1123
    Margins.LeftMargin = 4.000000000000000000
    Margins.TopMargin = 5.000000000000000000
    Margins.RightMargin = 3.000000000000000000
    Margins.BottomMargin = 5.000000000000000000
    ParentFont = True
    PreviewOptions.ShowModal = True
    RealBounds.UsedUnit = buMilimeters
    BeforePrint = LayoutBoletoBeforePrint
    OnDataCount = LayoutBoletoDataCount
    OnDataRecord = LayoutBoletoDataRecord
    object txtSwHouse: TRLAngleLabel
      Left = 3
      Top = 677
      Width = 12
      Height = 53
      Alignment = taCenter
      Angle = 90.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Pitch = fpFixed
      Font.Style = []
      Layout = tlCenter
      ParentFont = False
    end
    object RLBand4: TRLBand
      Left = 15
      Top = 19
      Width = 768
      Height = 203
      BeforePrint = RLBand4BeforePrint
      object RLDraw14: TRLDraw
        Left = 278
        Top = 3
        Width = 17
        Height = 126
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw8: TRLDraw
        Left = 248
        Top = 128
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw12: TRLDraw
        Left = 456
        Top = 44
        Width = 9
        Height = 113
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw6: TRLDraw
        Left = 224
        Top = 99
        Width = 8
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw9: TRLDraw
        Left = 69
        Top = 99
        Width = 17
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw7: TRLDraw
        Left = 162
        Top = 128
        Width = 8
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw4: TRLDraw
        Left = 0
        Top = 176
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw73: TRLDraw
        Left = 0
        Top = 186
        Width = 763
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLDraw13: TRLDraw
        Left = 0
        Top = 148
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw10: TRLDraw
        Left = 0
        Top = 63
        Width = 460
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw2: TRLDraw
        Left = 173
        Top = 3
        Width = 17
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw3: TRLDraw
        Left = 0
        Top = 120
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw5: TRLDraw
        Left = 0
        Top = 90
        Width = 460
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw11: TRLDraw
        Left = 0
        Top = 36
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel2: TRLLabel
        Left = 4
        Top = 46
        Width = 42
        Height = 10
        Caption = 'Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeCedente4: TRLLabel
        Left = 9
        Top = 57
        Width = 275
        Height = 13
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel6: TRLLabel
        Left = 289
        Top = 46
        Width = 103
        Height = 10
        Caption = 'Agencia / Codigo Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoCedente4: TRLLabel
        Left = 289
        Top = 57
        Width = 168
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Agencia / Codigo Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel8: TRLLabel
        Left = 169
        Top = 130
        Width = 18
        Height = 10
        Caption = 'Data'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel11: TRLLabel
        Left = 80
        Top = 100
        Width = 79
        Height = 10
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNumeroDocumento4: TRLLabel
        Left = 80
        Top = 113
        Width = 145
        Height = 13
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel14: TRLLabel
        Left = 231
        Top = 100
        Width = 29
        Height = 10
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecie4: TRLLabel
        Left = 231
        Top = 113
        Width = 53
        Height = 13
        AutoSize = False
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel23: TRLLabel
        Left = 289
        Top = 73
        Width = 53
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNossoNumero4: TRLLabel
        Left = 289
        Top = 84
        Width = 168
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel33: TRLLabel
        Left = 289
        Top = 100
        Width = 70
        Height = 10
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorDocumento4: TRLLabel
        Left = 289
        Top = 113
        Width = 168
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object imgBanco4: TRLImage
        Left = 1
        Top = -1
        Width = 179
        Height = 42
        Scaled = True
      end
      object txtNumeroBanco4: TRLLabel
        Left = 183
        Top = 12
        Width = 102
        Height = 27
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -30
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel3: TRLLabel
        Left = 289
        Top = 21
        Width = 465
        Height = 18
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Comprovante de Entrega'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel51: TRLLabel
        Left = 496
        Top = 46
        Width = 219
        Height = 10
        Caption = 'Motivo de nao entrega. (Para uso da empresa entregadora)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel52: TRLLabel
        Left = 4
        Top = 158
        Width = 70
        Height = 10
        Caption = 'Local de Pagamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblLocalPagto4: TRLLabel
        Left = 9
        Top = 169
        Width = 496
        Height = 15
        AutoSize = False
        Caption = 'Local de Pagamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel21: TRLLabel
        Left = 4
        Top = 100
        Width = 41
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataVencimento4: TRLLabel
        Left = 9
        Top = 113
        Width = 66
        Height = 13
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel18: TRLLabel
        Left = 4
        Top = 130
        Width = 89
        Height = 12
        Caption = 'Recebemos o Titulo '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel20: TRLLabel
        Left = 4
        Top = 143
        Width = 132
        Height = 11
        Caption = 'com as caracter'#237'sticas acima'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel25: TRLLabel
        Left = 4
        Top = 73
        Width = 31
        Height = 10
        Caption = 'Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeSacado4: TRLLabel
        Left = 9
        Top = 84
        Width = 275
        Height = 13
        AutoSize = False
        Caption = 'Nome do Sacado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel27: TRLLabel
        Left = 256
        Top = 130
        Width = 38
        Height = 10
        Caption = 'Assinatura'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel28: TRLLabel
        Left = 463
        Top = 130
        Width = 18
        Height = 10
        Caption = 'Data'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel30: TRLLabel
        Left = 553
        Top = 130
        Width = 38
        Height = 10
        Caption = 'Assinatura'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel29: TRLLabel
        Left = 466
        Top = 64
        Width = 48
        Height = 10
        Caption = '[   ] Mudou-se'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel31: TRLLabel
        Left = 466
        Top = 88
        Width = 48
        Height = 10
        Caption = '[   ] Recusado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel32: TRLLabel
        Left = 466
        Top = 112
        Width = 62
        Height = 10
        Caption = '[   ] Desconhecido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel35: TRLLabel
        Left = 560
        Top = 64
        Width = 41
        Height = 10
        Caption = '[   ] Ausente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel37: TRLLabel
        Left = 560
        Top = 88
        Width = 64
        Height = 10
        Caption = '[   ] N'#227'o procurado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel50: TRLLabel
        Left = 560
        Top = 112
        Width = 43
        Height = 10
        Caption = '[   ] Falecido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel54: TRLLabel
        Left = 655
        Top = 64
        Width = 90
        Height = 10
        Caption = '[   ] N'#227'o existe n'#186'. indicado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel55: TRLLabel
        Left = 655
        Top = 88
        Width = 86
        Height = 10
        Caption = '[   ] Endere'#231'o insuficiente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel56: TRLLabel
        Left = 655
        Top = 112
        Width = 96
        Height = 10
        Caption = '[   ] Outros (anotar no verso)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel57: TRLLabel
        Left = 0
        Top = 187
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw72: TRLDraw
        Left = 542
        Top = 128
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw15: TRLDraw
        Left = 635
        Top = 156
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel4: TRLLabel
        Left = 641
        Top = 158
        Width = 82
        Height = 10
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataProcessamento4: TRLLabel
        Left = 644
        Top = 169
        Width = 109
        Height = 15
        AutoSize = False
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object RLBand1: TRLBand
      Left = 15
      Top = 222
      Width = 768
      Height = 419
      BeforePrint = RLBand1BeforePrint
      object RLDraw28: TRLDraw
        Left = 562
        Top = 38
        Width = 17
        Height = 247
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw21: TRLDraw
        Left = 117
        Top = 101
        Width = 17
        Height = 56
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw24: TRLDraw
        Left = 241
        Top = 100
        Width = 17
        Height = 62
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw25: TRLDraw
        Left = 387
        Top = 100
        Width = 17
        Height = 56
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw29: TRLDraw
        Left = 5
        Top = 392
        Width = 763
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLDraw39: TRLDraw
        Left = 0
        Top = 341
        Width = 763
        Height = 8
        DrawKind = dkLine
      end
      object RLDraw38: TRLDraw
        Left = 0
        Top = 277
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw37: TRLDraw
        Left = 571
        Top = 226
        Width = 193
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw34: TRLDraw
        Left = 570
        Top = 200
        Width = 193
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw27: TRLDraw
        Left = 312
        Top = 102
        Width = 17
        Height = 25
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw26: TRLDraw
        Left = 179
        Top = 126
        Width = 17
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw23: TRLDraw
        Left = 0
        Top = 149
        Width = 763
        Height = 13
        DrawKind = dkLine
      end
      object RLDraw22: TRLDraw
        Left = 1
        Top = 117
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw20: TRLDraw
        Left = 0
        Top = 58
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw19: TRLDraw
        Left = 0
        Top = 92
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw18: TRLDraw
        Left = 0
        Top = 32
        Width = 763
        Height = 13
        DrawKind = dkLine
      end
      object RLDraw17: TRLDraw
        Left = 279
        Top = 3
        Width = 17
        Height = 34
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw16: TRLDraw
        Left = 173
        Top = 3
        Width = 17
        Height = 35
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object imgBanco2: TRLImage
        Left = 1
        Top = -6
        Width = 179
        Height = 42
        Scaled = True
      end
      object txtNumeroBanco2: TRLLabel
        Left = 184
        Top = 6
        Width = 102
        Height = 29
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -30
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel67: TRLLabel
        Left = 289
        Top = 15
        Width = 465
        Height = 18
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Recibo do Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel68: TRLLabel
        Left = 4
        Top = 40
        Width = 70
        Height = 10
        Caption = 'Local de Pagamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblLocalPagto: TRLMemo
        Left = 9
        Top = 49
        Width = 557
        Height = 17
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel69: TRLLabel
        Left = 4
        Top = 68
        Width = 44
        Height = 10
        Caption = 'Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeCedente2: TRLLabel
        Left = 49
        Top = 67
        Width = 519
        Height = 14
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
      object RLLabel70: TRLLabel
        Left = 4
        Top = 102
        Width = 68
        Height = 10
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataDocumento2: TRLLabel
        Left = 4
        Top = 111
        Width = 120
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel71: TRLLabel
        Left = 127
        Top = 102
        Width = 79
        Height = 10
        Caption = 'Numero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNumeroDocumento2: TRLLabel
        Left = 127
        Top = 112
        Width = 122
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Numero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel72: TRLLabel
        Left = 252
        Top = 102
        Width = 47
        Height = 10
        Caption = 'Especie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecieDoc2: TRLLabel
        Left = 252
        Top = 112
        Width = 68
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Especie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel73: TRLLabel
        Left = 322
        Top = 101
        Width = 23
        Height = 10
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtAceite2: TRLLabel
        Left = 322
        Top = 112
        Width = 66
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel74: TRLLabel
        Left = 399
        Top = 102
        Width = 82
        Height = 10
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataProcessamento2: TRLLabel
        Left = 399
        Top = 111
        Width = 124
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtUsoBanco2: TRLLabel
        Left = 4
        Top = 136
        Width = 120
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel75: TRLLabel
        Left = 4
        Top = 126
        Width = 49
        Height = 10
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCarteira2: TRLLabel
        Left = 127
        Top = 135
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel76: TRLLabel
        Left = 127
        Top = 126
        Width = 30
        Height = 10
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel77: TRLLabel
        Left = 188
        Top = 126
        Width = 29
        Height = 10
        Caption = 'Especie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecie2: TRLLabel
        Left = 190
        Top = 136
        Width = 58
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Especie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel78: TRLLabel
        Left = 252
        Top = 126
        Width = 40
        Height = 10
        Caption = 'Quantidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtQuantidade2: TRLLabel
        Left = 252
        Top = 136
        Width = 140
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel79: TRLLabel
        Left = 399
        Top = 130
        Width = 20
        Height = 10
        Caption = 'Valor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorMoeda2: TRLLabel
        Left = 399
        Top = 140
        Width = 124
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel80: TRLMemo
        Left = 4
        Top = 156
        Width = 461
        Height = 26
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Instru'#231#240'es (Texto de responsabilidade do benefici'#225'rio.)')
        ParentFont = False
      end
      object txtInstrucoes2: TRLMemo
        Left = 4
        Top = 177
        Width = 529
        Height = 83
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Linha 1'
          'Linha 2'
          'Linha 3'
          'Linha 4'
          'Linha 5'
          'Linha 6')
        ParentFont = False
      end
      object RLLabel81: TRLLabel
        Left = 574
        Top = 45
        Width = 41
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataVencimento2: TRLLabel
        Left = 575
        Top = 52
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel82: TRLLabel
        Left = 574
        Top = 69
        Width = 103
        Height = 10
        Caption = 'Ag'#234'ncia / C'#243'digo Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoCedente2: TRLLabel
        Left = 575
        Top = 82
        Width = 172
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Ag'#234'ncia / C'#243'digo Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel83: TRLLabel
        Left = 574
        Top = 101
        Width = 53
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNossoNumero2: TRLLabel
        Left = 575
        Top = 110
        Width = 172
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel84: TRLLabel
        Left = 574
        Top = 128
        Width = 87
        Height = 10
        Caption = '( = ) Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorDocumento2: TRLLabel
        Left = 575
        Top = 139
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel85: TRLLabel
        Left = 573
        Top = 157
        Width = 92
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDesconto2: TRLLabel
        Left = 575
        Top = 166
        Width = 172
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel86: TRLLabel
        Left = 574
        Top = 210
        Width = 87
        Height = 10
        Caption = '( + ) Mora / Multa / Juros'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtMoraMulta2: TRLLabel
        Left = 575
        Top = 220
        Width = 172
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel87: TRLLabel
        Left = 575
        Top = 263
        Width = 68
        Height = 10
        Caption = '( = ) Valor Cobrado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorCobrado2: TRLLabel
        Left = 575
        Top = 271
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblPagador2: TRLLabel
        Left = 3
        Top = 287
        Width = 46
        Height = 14
        Caption = 'Pagador:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomePagador2: TRLLabel
        Left = 53
        Top = 287
        Width = 514
        Height = 12
        AutoSize = False
        Caption = 'Nome do Pagador'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndPagador2: TRLLabel
        Left = 53
        Top = 301
        Width = 514
        Height = 12
        AutoSize = False
        Caption = 'Logradouro / N'#250'mero / Comp. / Bairro / Cidade / Estado / Cep'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel89: TRLLabel
        Left = 573
        Top = 287
        Width = 45
        Height = 10
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCpfCnpjPagador2: TRLLabel
        Left = 574
        Top = 299
        Width = 181
        Height = 14
        AutoSize = False
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel90: TRLLabel
        Left = 574
        Top = 318
        Width = 57
        Height = 10
        Caption = 'C'#243'digo de Baixa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoBaixa2: TRLLabel
        Left = 574
        Top = 329
        Width = 181
        Height = 14
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLMemo2: TRLMemo
        Left = 1
        Top = 346
        Width = 179
        Height = 45
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Recebimento atrav'#233's do cheque n'#250'mero'
          '                                           do banco.'
          'Esta quita'#231#227'o s'#243' ter'#225' validade ap'#243's o '
          'pagamento do cheque pelo banco sacado.')
        ParentFont = False
      end
      object RLLabel91: TRLLabel
        Left = 664
        Top = 346
        Width = 96
        Height = 10
        Caption = 'Autentica'#231#227'o Mec'#226'nica'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel98: TRLLabel
        Left = 0
        Top = 393
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
      end
      object lblSacador2a: TRLLabel
        Left = 3
        Top = 316
        Width = 47
        Height = 14
        Caption = 'Sacador/'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndSacadorAval2: TRLLabel
        Left = 53
        Top = 330
        Width = 514
        Height = 14
        AutoSize = False
        Caption = 'Logradouro / N'#250'mero / Comp. / Bairro / Cidade / Estado / Cep'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtReferencia2: TRLLabel
        Left = 638
        Top = 313
        Width = 116
        Height = 14
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel92: TRLLabel
        Left = 575
        Top = 183
        Width = 76
        Height = 10
        Caption = '( - ) Outras Deducoes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw74: TRLDraw
        Left = 572
        Top = 179
        Width = 193
        Height = 5
        DrawKind = dkLine
        Transparent = False
      end
      object RLLabel96: TRLLabel
        Left = 575
        Top = 237
        Width = 84
        Height = 10
        Caption = '( + ) Outros Acrescimos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw75: TRLDraw
        Left = 572
        Top = 257
        Width = 193
        Height = 5
        DrawKind = dkLine
        Transparent = False
      end
      object txtOrientacoesBanco: TRLMemo
        Left = 187
        Top = 346
        Width = 296
        Height = 49
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'line1'
          'line2'
          'line3'
          'line4')
        ParentFont = False
      end
      object RLDraw1: TRLDraw
        Left = 182
        Top = 345
        Width = 3
        Height = 53
        Angle = 80.000000000000000000
        DrawKind = dkLine
      end
      object rlBarraOrientbanco: TRLDraw
        Left = 482
        Top = 344
        Width = 10
        Height = 56
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object lblSacador2b: TRLLabel
        Left = 3
        Top = 330
        Width = 45
        Height = 14
        Caption = 'Avalista:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeSacadorAval2: TRLLabel
        Left = 53
        Top = 316
        Width = 514
        Height = 12
        AutoSize = False
        Caption = 'Nome do Sacador/Avalista - Cpf/Cnpj'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndCedente: TRLMemo
        Left = 51
        Top = 78
        Width = 503
        Height = 22
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
    end
    object RLBand2: TRLBand
      Left = 15
      Top = 641
      Width = 768
      Height = 472
      RealBounds.UsedUnit = buMilimeters
      BeforePrint = RLBand2BeforePrint
      object RLDraw76: TRLDraw
        Left = 570
        Top = 217
        Width = 193
        Height = 5
        DrawKind = dkLine
        Transparent = False
      end
      object RLDraw50: TRLDraw
        Left = 0
        Top = 337
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw49: TRLDraw
        Left = 0
        Top = 284
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw48: TRLDraw
        Left = 570
        Top = 241
        Width = 193
        Height = 5
        DrawKind = dkLine
      end
      object RLDraw47: TRLDraw
        Left = 570
        Top = 192
        Width = 193
        Height = 5
        DrawKind = dkLine
      end
      object RLDraw46: TRLDraw
        Left = 0
        Top = 165
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw45: TRLDraw
        Left = 183
        Top = 140
        Width = 17
        Height = 33
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw44: TRLDraw
        Left = 0
        Top = 133
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw43: TRLDraw
        Left = 391
        Top = 115
        Width = 17
        Height = 60
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw42: TRLDraw
        Left = 318
        Top = 116
        Width = 17
        Height = 25
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw41: TRLDraw
        Left = 245
        Top = 115
        Width = 17
        Height = 61
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw40: TRLDraw
        Left = 121
        Top = 115
        Width = 17
        Height = 59
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw36: TRLDraw
        Left = 0
        Top = 107
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw35: TRLDraw
        Left = 0
        Top = 45
        Width = 763
        Height = 16
        DrawKind = dkLine
      end
      object RLDraw33: TRLDraw
        Left = 0
        Top = 73
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object imgBanco3: TRLImage
        Left = 4
        Top = 11
        Width = 179
        Height = 38
        Scaled = True
      end
      object RLDraw81: TRLDraw
        Left = 177
        Top = 15
        Width = 17
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtLinhaDigitavel: TRLLabel
        Left = 270
        Top = 30
        Width = 488
        Height = 19
        AutoSize = False
        Caption = '00000.0000 00000.000000 00000.000000 0 00000000000000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw82: TRLDraw
        Left = 261
        Top = 16
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroBanco3: TRLLabel
        Left = 187
        Top = 22
        Width = 78
        Height = 28
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -30
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel145: TRLLabel
        Left = 4
        Top = 54
        Width = 70
        Height = 10
        Caption = 'Local de Pagamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtLocalPagamento3: TRLMemo
        Left = 4
        Top = 64
        Width = 533
        Height = 17
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataVencimento3: TRLLabel
        Left = 574
        Top = 67
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel146: TRLLabel
        Left = 574
        Top = 59
        Width = 41
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw83: TRLDraw
        Left = 562
        Top = 52
        Width = 17
        Height = 240
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel147: TRLLabel
        Left = 4
        Top = 83
        Width = 42
        Height = 10
        Caption = 'Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeCedente3: TRLLabel
        Left = 48
        Top = 82
        Width = 518
        Height = 13
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
      object RLLabel148: TRLLabel
        Left = 574
        Top = 85
        Width = 103
        Height = 10
        Caption = 'Ag'#234'ncia / C'#243'digo Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoCedente3: TRLLabel
        Left = 574
        Top = 97
        Width = 172
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Ag'#234'ncia / C'#243'digo Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel149: TRLLabel
        Left = 4
        Top = 117
        Width = 68
        Height = 10
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataDocumento3: TRLLabel
        Left = 4
        Top = 125
        Width = 124
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel150: TRLLabel
        Left = 131
        Top = 117
        Width = 79
        Height = 10
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNumeroDocumento3: TRLLabel
        Left = 131
        Top = 125
        Width = 122
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel151: TRLLabel
        Left = 256
        Top = 117
        Width = 47
        Height = 10
        Caption = 'Esp'#233'cie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecieDoc3: TRLLabel
        Left = 256
        Top = 125
        Width = 68
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel152: TRLLabel
        Left = 329
        Top = 116
        Width = 23
        Height = 10
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtAceite3: TRLLabel
        Left = 329
        Top = 125
        Width = 66
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel153: TRLLabel
        Left = 403
        Top = 116
        Width = 82
        Height = 10
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataProcessamento3: TRLLabel
        Left = 403
        Top = 125
        Width = 124
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel154: TRLLabel
        Left = 574
        Top = 117
        Width = 53
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNossoNumero3: TRLLabel
        Left = 574
        Top = 126
        Width = 172
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel155: TRLLabel
        Left = 4
        Top = 142
        Width = 49
        Height = 10
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtUsoBanco3: TRLLabel
        Left = 4
        Top = 152
        Width = 124
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel156: TRLLabel
        Left = 131
        Top = 142
        Width = 30
        Height = 10
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCarteira3: TRLLabel
        Left = 131
        Top = 153
        Width = 58
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel157: TRLLabel
        Left = 194
        Top = 142
        Width = 54
        Height = 10
        Caption = 'Esp'#233'cie Moeda'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecie3: TRLLabel
        Left = 194
        Top = 154
        Width = 58
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel158: TRLLabel
        Left = 256
        Top = 142
        Width = 40
        Height = 10
        Caption = 'Quantidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtQuantidade3: TRLLabel
        Left = 256
        Top = 159
        Width = 140
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel159: TRLLabel
        Left = 403
        Top = 148
        Width = 20
        Height = 10
        Caption = 'Valor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorMoeda3: TRLLabel
        Left = 403
        Top = 158
        Width = 124
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel160: TRLLabel
        Left = 574
        Top = 144
        Width = 87
        Height = 10
        Caption = '( = ) Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorDocumento3: TRLLabel
        Left = 574
        Top = 155
        Width = 172
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel161: TRLMemo
        Left = 4
        Top = 174
        Width = 491
        Height = 20
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Instru'#231#240'es (Texto de responsabilidade do benefici'#225'rio.)')
        ParentFont = False
      end
      object txtInstrucoes3: TRLMemo
        Left = 5
        Top = 197
        Width = 529
        Height = 82
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Linha 1'
          'Linha 2'
          'Linha 3'
          'Linha 4'
          'Linha 5'
          'Linha 6')
        ParentFont = False
      end
      object RLLabel162: TRLLabel
        Left = 574
        Top = 175
        Width = 92
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDesconto3: TRLLabel
        Left = 650
        Top = 182
        Width = 96
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel163: TRLLabel
        Left = 574
        Top = 221
        Width = 87
        Height = 10
        Caption = '( + ) Mora / Multa / Juros'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtMoraMulta3: TRLLabel
        Left = 574
        Top = 231
        Width = 172
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel164: TRLLabel
        Left = 574
        Top = 270
        Width = 68
        Height = 10
        Caption = '( = ) Valor Cobrado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorCobrado3: TRLLabel
        Left = 574
        Top = 279
        Width = 172
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblPagador3: TRLLabel
        Left = 3
        Top = 293
        Width = 43
        Height = 14
        Caption = 'Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomePagador3: TRLLabel
        Left = 53
        Top = 293
        Width = 514
        Height = 14
        AutoSize = False
        Caption = 'Nome do Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel166: TRLLabel
        Left = 573
        Top = 293
        Width = 45
        Height = 10
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCpfCnpjPagador3: TRLLabel
        Left = 573
        Top = 302
        Width = 187
        Height = 16
        AutoSize = False
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndPagador3: TRLLabel
        Left = 53
        Top = 305
        Width = 514
        Height = 14
        AutoSize = False
        Caption = 'Logradouro / N'#250'mero / Comp. / Bairro / Cidade / Estado / Cep'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel167: TRLLabel
        Left = 573
        Top = 319
        Width = 57
        Height = 10
        Caption = 'C'#243'digo de Baixa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeSacadorAval3: TRLLabel
        Left = 53
        Top = 318
        Width = 514
        Height = 14
        AutoSize = False
        Caption = 'Nome do Sacador/Avalista - Cpf/Cnpj'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoBaixa3: TRLLabel
        Left = 573
        Top = 329
        Width = 185
        Height = 16
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel168: TRLLabel
        Left = 594
        Top = 350
        Width = 131
        Height = 10
        Caption = 'Autentica'#231#227'o - Ficha de Compensa'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtReferencia3: TRLLabel
        Left = 638
        Top = 316
        Width = 121
        Height = 16
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object imgCodigoBarra: TRLBarcode
        Left = 0
        Top = 350
        Width = 414
        Height = 58
        Behavior = [beSiteExpander]
        Caption = '00000000000000000000000000000000000000000000'
        CheckSumMethod = cmNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        InvalidCode = icDrawAnyway
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        ParentFont = False
        Ratio = 3.000000000000000000
        RealBounds.UsedUnit = buMilimeters
        RealBounds.Width = 103.000000000000000000
        RealBounds.Height = 13.000000000000000000
      end
      object RLLabel169: TRLLabel
        Left = 584
        Top = 525
        Width = 76
        Height = 10
        Caption = '( - ) Outras Dedu'#231#240'es'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object txtDesconto5: TRLLabel
        Left = 584
        Top = 537
        Width = 172
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel171: TRLLabel
        Left = 584
        Top = 578
        Width = 84
        Height = 10
        Caption = '( + ) Outros Acrescimos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object txtMoraMulta4: TRLLabel
        Left = 600
        Top = 587
        Width = 172
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw77: TRLDraw
        Left = 579
        Top = 600
        Width = 223
        Height = 3
        DrawKind = dkLine
        Transparent = False
      end
      object RLLabel7: TRLLabel
        Left = 574
        Top = 198
        Width = 76
        Height = 10
        Caption = '( - ) Outras Deducoes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel9: TRLLabel
        Left = 574
        Top = 248
        Width = 84
        Height = 10
        Caption = '( + ) Outros Acrescimos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw78: TRLDraw
        Left = 570
        Top = 267
        Width = 193
        Height = 5
        DrawKind = dkLine
      end
      object txtEndSacadorAval3: TRLLabel
        Left = 53
        Top = 330
        Width = 514
        Height = 14
        AutoSize = False
        Caption = 'Logradouro / N'#250'mero / Comp. / Bairro / Cidade / Estado / Cep'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblSacador3a: TRLLabel
        Left = 3
        Top = 316
        Width = 47
        Height = 14
        Caption = 'Sacador/'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblSacador3b: TRLLabel
        Left = 3
        Top = 330
        Width = 45
        Height = 14
        Caption = 'Avalista:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndCedente1: TRLMemo
        Left = 50
        Top = 93
        Width = 504
        Height = 22
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object BoletoCarne: TRLReport
    Left = 850
    Top = 109
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 5.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 5.000000000000000000
    Margins.BottomMargin = 0.000000000000000000
    PreviewOptions.ShowModal = True
    BeforePrint = BoletoCarneBeforePrint
    OnDataCount = BoletoCarneDataCount
    OnDataRecord = BoletoCarneDataRecord
    object RLBand3: TRLBand
      Left = 19
      Top = 8
      Width = 756
      Height = 376
      BeforePrint = RLBand3BeforePrint
      object RLDraw30: TRLDraw
        Left = 150
        Top = 270
        Width = 599
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw31: TRLDraw
        Left = 2
        Top = 223
        Width = 756
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw32: TRLDraw
        Left = 567
        Top = 199
        Width = 220
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw51: TRLDraw
        Left = 567
        Top = 172
        Width = 220
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw52: TRLDraw
        Left = -3
        Top = 145
        Width = 759
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw53: TRLDraw
        Left = 296
        Top = 120
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw54: TRLDraw
        Left = -1
        Top = 120
        Width = 757
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw55: TRLDraw
        Left = 476
        Top = 94
        Width = 1
        Height = 52
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw56: TRLDraw
        Left = 415
        Top = 94
        Width = 1
        Height = 27
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw57: TRLDraw
        Left = 344
        Top = 94
        Width = 1
        Height = 52
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw58: TRLDraw
        Left = 245
        Top = 94
        Width = 1
        Height = 52
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw59: TRLDraw
        Left = -1
        Top = 94
        Width = 757
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw60: TRLDraw
        Left = 0
        Top = 34
        Width = 756
        Height = 16
        DrawKind = dkLine
      end
      object RLDraw61: TRLDraw
        Left = -1
        Top = 67
        Width = 757
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw62: TRLDraw
        Left = 0
        Top = 355
        Width = 756
        Height = 15
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object imgBancoCarne: TRLImage
        Left = 152
        Top = 0
        Width = 101
        Height = 41
        Center = True
        Scaled = True
      end
      object RLDraw63: TRLDraw
        Left = 253
        Top = 1
        Width = 1
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw64: TRLDraw
        Left = 325
        Top = 1
        Width = 1
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel1: TRLLabel
        Left = 154
        Top = 43
        Width = 70
        Height = 10
        Caption = 'Local de Pagamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel94: TRLLabel
        Left = 572
        Top = 43
        Width = 41
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw65: TRLDraw
        Left = 567
        Top = 41
        Width = 1
        Height = 183
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel95: TRLLabel
        Left = 154
        Top = 71
        Width = 42
        Height = 10
        Caption = 'Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel97: TRLLabel
        Left = 572
        Top = 71
        Width = 91
        Height = 10
        Caption = 'Ag'#234'ncia / C'#243'digo Beneficiario'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel99: TRLLabel
        Left = 152
        Top = 96
        Width = 68
        Height = 10
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel101: TRLLabel
        Left = 247
        Top = 96
        Width = 79
        Height = 10
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel103: TRLLabel
        Left = 347
        Top = 96
        Width = 47
        Height = 10
        Caption = 'Esp'#233'cie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel105: TRLLabel
        Left = 418
        Top = 96
        Width = 23
        Height = 10
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel107: TRLLabel
        Left = 479
        Top = 96
        Width = 82
        Height = 10
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel109: TRLLabel
        Left = 572
        Top = 96
        Width = 53
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel111: TRLLabel
        Left = 152
        Top = 121
        Width = 49
        Height = 10
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel113: TRLLabel
        Left = 247
        Top = 121
        Width = 30
        Height = 10
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel115: TRLLabel
        Left = 298
        Top = 121
        Width = 29
        Height = 10
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel5: TRLLabel
        Left = 298
        Top = 131
        Width = 44
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'R$'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel117: TRLLabel
        Left = 348
        Top = 121
        Width = 40
        Height = 10
        Caption = 'Quantidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel119: TRLLabel
        Left = 479
        Top = 121
        Width = 20
        Height = 10
        Caption = 'Valor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel121: TRLLabel
        Left = 572
        Top = 121
        Width = 87
        Height = 10
        Caption = '( = ) Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel123: TRLMemo
        Left = 152
        Top = 146
        Width = 410
        Height = 20
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Instru'#231#240'es (Texto de responsabilidade do benefici'#225'rio.)')
        ParentFont = False
      end
      object RLLabel124: TRLLabel
        Left = 572
        Top = 146
        Width = 92
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel126: TRLLabel
        Left = 572
        Top = 173
        Width = 62
        Height = 10
        Caption = '( + ) Mora / Multa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel128: TRLLabel
        Left = 572
        Top = 200
        Width = 68
        Height = 10
        Caption = '( = ) Valor Cobrado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel130: TRLLabel
        Left = 152
        Top = 228
        Width = 31
        Height = 10
        Caption = 'Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel132: TRLLabel
        Left = 571
        Top = 227
        Width = 82
        Height = 10
        Caption = 'CPF / CNPJ do Sacado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel135: TRLLabel
        Left = 571
        Top = 253
        Width = 57
        Height = 10
        Caption = 'C'#243'digo de Baixa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel138: TRLLabel
        Left = 643
        Top = 284
        Width = 80
        Height = 10
        Caption = 'Autentica'#231#227'o Mec'#226'nica'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel10: TRLLabel
        Left = 474
        Top = 132
        Width = 5
        Height = 10
        Caption = 'x'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel13: TRLLabel
        Left = 0
        Top = 356
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel15: TRLLabel
        Left = 150
        Top = 266
        Width = 88
        Height = 14
        Caption = 'Sacador/Avalista:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText17: TRLDBText
        Left = 240
        Top = 266
        Width = 324
        Height = 13
        AutoSize = False
        DataField = 'SacadorAvalista'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDBText18: TRLDBText
        Left = 570
        Top = 266
        Width = 204
        Height = 13
        AutoSize = False
        DataField = 'NossoNumero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel16: TRLLabel
        Left = 644
        Top = 310
        Width = 81
        Height = 10
        Caption = 'Ficha de Compensa'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel19: TRLLabel
        Left = 0
        Top = 43
        Width = 28
        Height = 10
        Caption = 'Parcela'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel36: TRLLabel
        Left = 66
        Top = 43
        Width = 41
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw66: TRLDraw
        Left = 56
        Top = 41
        Width = 1
        Height = 25
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel38: TRLLabel
        Left = 0
        Top = 71
        Width = 103
        Height = 10
        Caption = 'Ag'#234'ncia / C'#243'digo Benefici'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel39: TRLLabel
        Left = 0
        Top = 95
        Width = 29
        Height = 10
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel40: TRLLabel
        Left = 0
        Top = 105
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'R$'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel41: TRLLabel
        Left = 58
        Top = 95
        Width = 40
        Height = 10
        Caption = 'Quantidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw67: TRLDraw
        Left = 55
        Top = 94
        Width = 1
        Height = 27
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel42: TRLLabel
        Left = 0
        Top = 121
        Width = 87
        Height = 10
        Caption = '( = ) Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel43: TRLLabel
        Left = 0
        Top = 146
        Width = 92
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel44: TRLLabel
        Left = 0
        Top = 165
        Width = 62
        Height = 10
        Caption = '( + ) Mora / Multa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel45: TRLLabel
        Left = 0
        Top = 184
        Width = 68
        Height = 10
        Caption = '( = ) Valor Cobrado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw68: TRLDraw
        Left = 0
        Top = 164
        Width = 141
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw69: TRLDraw
        Left = 0
        Top = 183
        Width = 141
        Height = 1
        DrawKind = dkLine
      end
      object RLLabel46: TRLLabel
        Left = 0
        Top = 202
        Width = 53
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw70: TRLDraw
        Left = 0
        Top = 202
        Width = 141
        Height = 1
        DrawKind = dkLine
      end
      object RLLabel47: TRLLabel
        Left = -1
        Top = 276
        Width = 31
        Height = 10
        Caption = 'Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel48: TRLLabel
        Left = 1
        Top = 343
        Width = 61
        Height = 13
        Caption = 'Recibo do Pagador'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel49: TRLLabel
        Left = 71
        Top = 343
        Width = 61
        Height = 13
        Caption = 'Autenticar no Verso'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw71: TRLDraw
        Left = 140
        Top = 0
        Width = 11
        Height = 353
        Angle = 90.000000000000000000
        DrawKind = dkLine
        Pen.Style = psDot
        Transparent = False
      end
      object ImgLoja: TRLImage
        Left = 5
        Top = 0
        Width = 140
        Height = 41
        Center = True
        Scaled = True
      end
      object txtTotPar: TRLLabel
        Left = 29
        Top = 52
        Width = 20
        Height = 14
        Caption = '/X'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object mIntrucoes: TRLMemo
        Left = 152
        Top = 168
        Width = 410
        Height = 56
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtLinhaDigitavelCarne: TRLLabel
        Left = 326
        Top = 19
        Width = 418
        Height = 18
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtVencCanhoto: TRLLabel
        Left = 56
        Top = 52
        Width = 82
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtCodCedenteCarne: TRLLabel
        Left = 5
        Top = 79
        Width = 129
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorCarne: TRLLabel
        Left = 0
        Top = 131
        Width = 137
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtNossoNumeroCarne: TRLLabel
        Left = 4
        Top = 211
        Width = 136
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
      object txtCPF: TRLLabel
        Left = -1
        Top = 306
        Width = 141
        Height = 12
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
      end
      object txtLocal: TRLMemo
        Left = 154
        Top = 52
        Width = 412
        Height = 14
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeCedente: TRLLabel
        Left = 154
        Top = 80
        Width = 412
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataDocto: TRLLabel
        Left = 152
        Top = 106
        Width = 84
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNumeroDocto: TRLLabel
        Left = 247
        Top = 106
        Width = 94
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEspecieDoc: TRLLabel
        Left = 347
        Top = 106
        Width = 68
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtAceite: TRLLabel
        Left = 418
        Top = 106
        Width = 56
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataProces: TRLLabel
        Left = 479
        Top = 106
        Width = 86
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCarteira: TRLLabel
        Left = 247
        Top = 131
        Width = 50
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeSacado: TRLLabel
        Left = 188
        Top = 227
        Width = 377
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndSacado: TRLLabel
        Left = 188
        Top = 240
        Width = 377
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCidadeSacado: TRLLabel
        Left = 188
        Top = 253
        Width = 377
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtVencCarne2: TRLLabel
        Left = 572
        Top = 53
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtNossoNumCan: TRLLabel
        Left = 572
        Top = 106
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorCar: TRLLabel
        Left = 572
        Top = 131
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtCodCedenteCarne2: TRLLabel
        Left = 572
        Top = 80
        Width = 172
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCPFCarne2: TRLLabel
        Left = 570
        Top = 237
        Width = 204
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtParcela: TRLLabel
        Left = -26
        Top = 52
        Width = 51
        Height = 14
        Alignment = taRightJustify
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object imgBarrasCarne: TRLBarcode
        Left = 154
        Top = 282
        Width = 465
        Height = 66
        AutoSize = False
        Caption = '23790-3571000000100120'
        CheckSumMethod = cmNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        InvalidCode = icDrawAnyway
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        ParentFont = False
        Ratio = 3.000000000000000000
      end
      object txtNumeroBanco: TRLLabel
        Left = 254
        Top = 12
        Width = 72
        Height = 27
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -24
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtNomeSacadoCarne: TRLMemo
        Left = 0
        Top = 284
        Width = 141
        Height = 23
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
      end
      object txtEndCedenteCarne: TRLLabel
        Left = 202
        Top = 71
        Width = 364
        Height = 12
        AutoSize = False
        Caption = 'Endereco Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object memoEndCedenteCarne: TRLMemo
        Left = 0
        Top = 225
        Width = 139
        Height = 51
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        Layout = tlCenter
        Lines.Strings = (
          '1'
          '2'
          '3'
          '4')
        ParentFont = False
      end
      object txtOrientacoesBancoCarne: TRLMemo
        Left = 0
        Top = 319
        Width = 140
        Height = 23
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object RLHTMLFilter1: TRLHTMLFilter
    DocumentStyle = dsCSS2
    DisplayName = 'Filtro HTML'
    Left = 158
    Top = 8
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#174' 1999-200' +
      '8 Fortes Inform'#225'tica'
    DisplayName = 'Filtro PDF'
    Left = 128
    Top = 8
  end
end
