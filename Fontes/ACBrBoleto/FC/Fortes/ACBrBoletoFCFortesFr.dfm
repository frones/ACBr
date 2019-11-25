object ACBRBoletoFCFortesFr: TACBRBoletoFCFortesFr
  Left = 424
  Top = 133
  Width = 1403
  Height = 788
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
    Top = 3
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
      Top = 222
      Width = 768
      Height = 203
      BeforePrint = RLBand4BeforePrint
      object RLDraw14: TRLDraw
        Left = 279
        Top = 3
        Width = 13
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
        Font.Name = 'Arial'
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
      Top = 425
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
        Font.Name = 'Arial'
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
      Top = 844
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
    object rlbndComprovanteEntrega2: TRLBand
      Left = 15
      Top = 19
      Width = 768
      Height = 203
      BeforePrint = rlbndComprovanteEntrega2BeforePrint
      object RLDraw116: TRLDraw
        Left = 327
        Top = 45
        Width = 17
        Height = 112
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw117: TRLDraw
        Left = 240
        Top = 156
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw118: TRLDraw
        Left = 562
        Top = 44
        Width = 17
        Height = 141
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw120: TRLDraw
        Left = 69
        Top = 128
        Width = 17
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw121: TRLDraw
        Left = 165
        Top = 128
        Width = 8
        Height = 57
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw122: TRLDraw
        Left = 0
        Top = 176
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw123: TRLDraw
        Left = 0
        Top = 186
        Width = 763
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLDraw124: TRLDraw
        Left = 0
        Top = 148
        Width = 571
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw125: TRLDraw
        Left = 0
        Top = 73
        Width = 336
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw126: TRLDraw
        Left = 173
        Top = 3
        Width = 17
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw127: TRLDraw
        Left = 0
        Top = 120
        Width = 571
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel165: TRLLabel
        Left = 172
        Top = 158
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
      object RLLabel175: TRLLabel
        Left = 80
        Top = 130
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
      object txtNumeroDocumento5: TRLLabel
        Left = 79
        Top = 143
        Width = 88
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel183: TRLLabel
        Left = 172
        Top = 130
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
      object txtEspecie5: TRLLabel
        Left = 172
        Top = 143
        Width = 39
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object imgBanco5: TRLImage
        Left = 1
        Top = -1
        Width = 179
        Height = 42
        Scaled = True
      end
      object txtDataDocumento5: TRLLabel
        Left = 9
        Top = 143
        Width = 66
        Height = 13
        AutoSize = False
        Caption = 'Data Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel196: TRLLabel
        Left = 4
        Top = 158
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
      object RLLabel197: TRLLabel
        Left = 4
        Top = 171
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
      object RLLabel198: TRLLabel
        Left = 4
        Top = 83
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
      object RLLabel200: TRLLabel
        Left = 248
        Top = 158
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
      object RLLabel201: TRLLabel
        Left = 339
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
      object RLLabel202: TRLLabel
        Left = 406
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
      object RLLabel203: TRLLabel
        Left = 358
        Top = 59
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
      object RLLabel204: TRLLabel
        Left = 358
        Top = 72
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
      object RLLabel205: TRLLabel
        Left = 358
        Top = 85
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
      object RLLabel206: TRLLabel
        Left = 358
        Top = 98
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
      object RLLabel207: TRLLabel
        Left = 358
        Top = 111
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
      object RLLabel208: TRLLabel
        Left = 450
        Top = 59
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
      object RLLabel209: TRLLabel
        Left = 450
        Top = 72
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
      object RLLabel210: TRLLabel
        Left = 450
        Top = 85
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
      object RLLabel211: TRLLabel
        Left = 450
        Top = 98
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
      object RLLabel212: TRLLabel
        Left = 0
        Top = 187
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw130: TRLDraw
        Left = 398
        Top = 128
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw131: TRLDraw
        Left = 451
        Top = 156
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel213: TRLLabel
        Left = 457
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
      object txtDataProcessamento5: TRLLabel
        Left = 460
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
      object RLDraw129: TRLDraw
        Left = 0
        Top = 36
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel191: TRLLabel
        Left = 341
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
      object RLLabel66: TRLLabel
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
      object RLDraw132: TRLDraw
        Left = 279
        Top = 3
        Width = 13
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroBanco5: TRLLabel
        Left = 183
        Top = 12
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
      object RLLabel194: TRLLabel
        Left = 574
        Top = 46
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
      object RLLabel192: TRLLabel
        Left = 3
        Top = 130
        Width = 69
        Height = 10
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw133: TRLDraw
        Left = 570
        Top = 60
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object RLLabel185: TRLLabel
        Left = 574
        Top = 65
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
      object RLDraw134: TRLDraw
        Left = 570
        Top = 85
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object txtNossoNumero5: TRLLabel
        Left = 575
        Top = 74
        Width = 172
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
      object RLLabel187: TRLLabel
        Left = 574
        Top = 90
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
      object RLDraw135: TRLDraw
        Left = 570
        Top = 105
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object txtValorDocumento5: TRLLabel
        Left = 644
        Top = 93
        Width = 103
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel188: TRLLabel
        Left = 574
        Top = 110
        Width = 93
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw136: TRLDraw
        Left = 570
        Top = 162
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object RLLabel195: TRLLabel
        Left = 574
        Top = 167
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
      object txtValorCobrado4: TRLLabel
        Left = 648
        Top = 170
        Width = 99
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw137: TRLDraw
        Left = 570
        Top = 143
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object RLLabel215: TRLLabel
        Left = 574
        Top = 148
        Width = 85
        Height = 10
        Caption = '( + ) Outros Acrescimos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw138: TRLDraw
        Left = 570
        Top = 124
        Width = 194
        Height = 8
        DrawKind = dkLine
      end
      object RLLabel217: TRLLabel
        Left = 574
        Top = 129
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
      object txtDesconto4: TRLLabel
        Left = 667
        Top = 113
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtMoraMulta5: TRLLabel
        Left = 665
        Top = 132
        Width = 82
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw139: TRLDraw
        Left = 211
        Top = 128
        Width = 8
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtCodigoCedente5: TRLLabel
        Left = 218
        Top = 143
        Width = 103
        Height = 11
        Caption = 'Agencia/Cod. Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel102: TRLLabel
        Left = 218
        Top = 130
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
      object txtDataVencimento5: TRLLabel
        Left = 621
        Top = 48
        Width = 126
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rlmCedente5: TRLMemo
        Left = 48
        Top = 46
        Width = 286
        Height = 34
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rlmPagador5: TRLMemo
        Left = 37
        Top = 83
        Width = 297
        Height = 45
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel190: TRLLabel
        Left = 291
        Top = 3
        Width = 476
        Height = 18
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Comprovante de Entrega'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtLinhaDigitavelComprovanteRec: TRLLabel
        Left = 291
        Top = 23
        Width = 473
        Height = 19
        AutoSize = False
        Caption = '00000.0000 00000.000000 00000.000000 0 00000000000000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object LayoutFaturaDetal: TRLReport
    Left = 208
    Top = 193
    Width = 794
    Height = 1123
    Margins.LeftMargin = 4.000000000000000000
    Margins.TopMargin = 5.000000000000000000
    Margins.RightMargin = 3.000000000000000000
    Margins.BottomMargin = 5.000000000000000000
    ParentFont = True
    PreviewOptions.ShowModal = True
    RealBounds.UsedUnit = buMilimeters
    BeforePrint = LayoutFaturaDetalBeforePrint
    OnDataCount = LayoutFaturaDetalDataCount
    OnDataRecord = LayoutFaturaDetalDataRecord
    object txtSwHouseDet: TRLAngleLabel
      Left = 3
      Top = 840
      Width = 12
      Height = 218
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
    object RLBandReciboPagDetal: TRLBand
      Left = 15
      Top = 19
      Width = 768
      Height = 129
      BeforePrint = RLBandReciboPagDetalBeforePrint
      object txtNomeCedenteTopDet: TRLLabel
        Left = 189
        Top = 5
        Width = 576
        Height = 20
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw128: TRLDraw
        Left = 184
        Top = 3
        Width = 6
        Height = 50
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw140: TRLDraw
        Left = 0
        Top = 43
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object imgBancoTopDet: TRLImage
        Left = 1
        Top = 7
        Width = 179
        Height = 42
        Scaled = True
      end
      object txtEnderecoSacadoTopDet: TRLLabel
        Left = 42
        Top = 98
        Width = 516
        Height = 14
        AutoSize = False
        Caption = 'Rua / Numero / Complemento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel182: TRLLabel
        Left = 6
        Top = 54
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
      object RLLabel184: TRLLabel
        Left = 581
        Top = 54
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
      object RLDraw141: TRLDraw
        Left = 2
        Top = 120
        Width = 763
        Height = 13
        DrawKind = dkLine
      end
      object RLDraw142: TRLDraw
        Left = 568
        Top = 51
        Width = 8
        Height = 77
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtEnderecoCedenteTopDet: TRLLabel
        Left = 190
        Top = 22
        Width = 575
        Height = 14
        AutoSize = False
        Caption = 'Rua / Numero / Complemento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCidadeCedenteTopDet: TRLLabel
        Left = 190
        Top = 36
        Width = 575
        Height = 14
        AutoSize = False
        Caption = 'Cep / Bairro / Cidade / Estado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw181: TRLDraw
        Left = 0
        Top = 74
        Width = 763
        Height = 13
        DrawKind = dkLine
      end
      object RLDraw182: TRLDraw
        Left = 129
        Top = 51
        Width = 6
        Height = 30
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNomeSacadoTopDet: TRLLabel
        Left = 42
        Top = 83
        Width = 516
        Height = 13
        AutoSize = False
        Caption = 'Nome do Sacado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtDataDocumentoTopDet: TRLLabel
        Left = 6
        Top = 64
        Width = 108
        Height = 14
        AutoSize = False
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel235: TRLLabel
        Left = 142
        Top = 54
        Width = 47
        Height = 10
        Caption = 'Compet'#234'ncia'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCompetenciaTopDet: TRLLabel
        Left = 142
        Top = 64
        Width = 84
        Height = 14
        AutoSize = False
        Caption = 'Competencia'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw183: TRLDraw
        Left = 264
        Top = 51
        Width = 6
        Height = 30
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw184: TRLDraw
        Left = 425
        Top = 51
        Width = 6
        Height = 30
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroDocumentoTopDet: TRLLabel
        Left = 581
        Top = 64
        Width = 161
        Height = 14
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel247: TRLLabel
        Left = 582
        Top = 92
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
      object txtCodigoBaixaTopDet: TRLLabel
        Left = 582
        Top = 103
        Width = 161
        Height = 14
        AutoSize = False
        Caption = 'C'#243'digo de Baixa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel186: TRLLabel
        Left = 292
        Top = 52
        Width = 71
        Height = 10
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorDocumentoTopDet: TRLLabel
        Left = 279
        Top = 64
        Width = 132
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel193: TRLLabel
        Left = 451
        Top = 54
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
      object txtDataVencimentoTopDet: TRLLabel
        Left = 451
        Top = 65
        Width = 106
        Height = 14
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel239: TRLLabel
        Left = 6
        Top = 85
        Width = 30
        Height = 10
        Caption = 'Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCidadeSacadoTopDet: TRLLabel
        Left = 42
        Top = 111
        Width = 516
        Height = 14
        AutoSize = False
        Caption = 'Cep / Bairro / Cidade / Estado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object RLBand8: TRLBand
      Left = 15
      Top = 719
      Width = 768
      Height = 384
      AlignToBottom = True
      RealBounds.UsedUnit = buMilimeters
      BeforePrint = RLBand8BeforePrint
      object RLDraw186: TRLDraw
        Left = 540
        Top = 229
        Width = 224
        Height = 5
        DrawKind = dkLine
      end
      object RLDraw144: TRLDraw
        Left = 0
        Top = 301
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw145: TRLDraw
        Left = 0
        Top = 248
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw146: TRLDraw
        Left = 0
        Top = 146
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw147: TRLDraw
        Left = 183
        Top = 127
        Width = 17
        Height = 27
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw148: TRLDraw
        Left = 0
        Top = 119
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw149: TRLDraw
        Left = 391
        Top = 101
        Width = 17
        Height = 54
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw150: TRLDraw
        Left = 318
        Top = 101
        Width = 17
        Height = 25
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw151: TRLDraw
        Left = 245
        Top = 101
        Width = 17
        Height = 54
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw152: TRLDraw
        Left = 121
        Top = 101
        Width = 17
        Height = 54
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw153: TRLDraw
        Left = 0
        Top = 93
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw154: TRLDraw
        Left = 0
        Top = 33
        Width = 763
        Height = 9
        DrawKind = dkLine
      end
      object imgBancoDet: TRLImage
        Left = 1
        Top = -3
        Width = 179
        Height = 40
        Scaled = True
      end
      object RLDraw155: TRLDraw
        Left = 179
        Top = 1
        Width = 7
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtLinhaDigitavelDet: TRLLabel
        Left = 272
        Top = 16
        Width = 488
        Height = 19
        AutoSize = False
        Caption = '00000.0000 00000.000000 00000.000000 0 00000000000000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw156: TRLDraw
        Left = 263
        Top = 2
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroBancoDet: TRLLabel
        Left = 187
        Top = 7
        Width = 78
        Height = 28
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -29
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtCodigoCedenteDet: TRLLabel
        Left = 552
        Top = 86
        Width = 192
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Ag'#234'ncia / C'#243'digo Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel234: TRLLabel
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
      object txtDataDocumentoDet: TRLLabel
        Left = 4
        Top = 112
        Width = 124
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel236: TRLLabel
        Left = 131
        Top = 102
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
      object txtNumeroDocumentoDet: TRLLabel
        Left = 131
        Top = 112
        Width = 122
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel238: TRLLabel
        Left = 256
        Top = 102
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
      object txtEspecieDocDet: TRLLabel
        Left = 256
        Top = 112
        Width = 68
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie Doc.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel240: TRLLabel
        Left = 329
        Top = 102
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
      object txtAceiteDet: TRLLabel
        Left = 329
        Top = 112
        Width = 66
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Aceite'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel242: TRLLabel
        Left = 403
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
      object txtDataProcessamentoDet: TRLLabel
        Left = 403
        Top = 112
        Width = 124
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Data do Processamento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel244: TRLLabel
        Left = 552
        Top = 103
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
      object txtNossoNumeroDet: TRLLabel
        Left = 552
        Top = 112
        Width = 192
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel246: TRLLabel
        Left = 4
        Top = 130
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
      object txtUsoBancoDet: TRLLabel
        Left = 4
        Top = 139
        Width = 124
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel248: TRLLabel
        Left = 131
        Top = 130
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
      object txtCarteiraDet: TRLLabel
        Left = 131
        Top = 139
        Width = 58
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel250: TRLLabel
        Left = 194
        Top = 130
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
      object txtEspecieDet: TRLLabel
        Left = 194
        Top = 139
        Width = 58
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel252: TRLLabel
        Left = 256
        Top = 130
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
      object txtQuantidadeDet: TRLLabel
        Left = 256
        Top = 139
        Width = 140
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel254: TRLLabel
        Left = 403
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
      object txtValorMoedaDet: TRLLabel
        Left = 403
        Top = 139
        Width = 124
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel256: TRLLabel
        Left = 552
        Top = 129
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
      object txtValorDocumentoDet: TRLLabel
        Left = 552
        Top = 138
        Width = 192
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel258: TRLLabel
        Left = 4
        Top = 160
        Width = 171
        Height = 10
        Caption = 'Instru'#231#245'es - Texto de responsabilidade do cedente.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtInstrucoesDet: TRLMemo
        Left = 5
        Top = 173
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
      object RLLabel259: TRLLabel
        Left = 552
        Top = 156
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
      object txtDescontoDet: TRLLabel
        Left = 552
        Top = 165
        Width = 192
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel261: TRLLabel
        Left = 552
        Top = 180
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
      object txtMoraMultaDet: TRLLabel
        Left = 552
        Top = 189
        Width = 192
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel263: TRLLabel
        Left = 552
        Top = 233
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
      object txtValorCobradoDet: TRLLabel
        Left = 552
        Top = 242
        Width = 192
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel265: TRLLabel
        Left = 50
        Top = 257
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
      object txtNomeSacadoDet: TRLLabel
        Left = 108
        Top = 257
        Width = 428
        Height = 14
        AutoSize = False
        Caption = 'Nome do Sacado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel267: TRLLabel
        Left = 552
        Top = 257
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
      object txtCpfCnpjSacadoDet: TRLLabel
        Left = 552
        Top = 266
        Width = 204
        Height = 16
        AutoSize = False
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEnderecoSacadoDet: TRLLabel
        Left = 108
        Top = 270
        Width = 429
        Height = 14
        AutoSize = False
        Caption = 'Rua / Numero / Complemento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel270: TRLLabel
        Left = 552
        Top = 282
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
      object txtCidadeSacadodet: TRLLabel
        Left = 108
        Top = 283
        Width = 429
        Height = 14
        AutoSize = False
        Caption = 'Cep / Bairro / Cidade / Estado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoBaixaDet: TRLLabel
        Left = 552
        Top = 293
        Width = 204
        Height = 16
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel273: TRLLabel
        Left = 594
        Top = 314
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
      object RLLabel274: TRLLabel
        Left = 4
        Top = 294
        Width = 89
        Height = 14
        Caption = 'Sacador/Avalista:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtReferenciaDet: TRLLabel
        Left = 615
        Top = 280
        Width = 150
        Height = 16
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel276: TRLLabel
        Left = 584
        Top = 525
        Width = 76
        Height = 10
        Caption = '( - ) Outras Dedu'#231#245'es'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel277: TRLLabel
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
      object RLLabel278: TRLLabel
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
      object RLLabel279: TRLLabel
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
      object RLDraw157: TRLDraw
        Left = 579
        Top = 600
        Width = 223
        Height = 3
        DrawKind = dkLine
        Transparent = False
      end
      object RLDraw158: TRLDraw
        Left = 539
        Top = 176
        Width = 225
        Height = 5
        DrawKind = dkLine
        Transparent = False
      end
      object RLDraw159: TRLDraw
        Left = 540
        Top = 203
        Width = 224
        Height = 5
        DrawKind = dkLine
      end
      object txtEndCedenteDet: TRLLabel
        Left = 48
        Top = 87
        Width = 481
        Height = 14
        Caption = 'Endereco Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtSacadorAvalistaDet: TRLLabel
        Left = 108
        Top = 294
        Width = 429
        Height = 14
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw160: TRLDraw
        Left = 0
        Top = 67
        Width = 763
        Height = 9
        DrawKind = dkLine
      end
      object RLLabel227: TRLLabel
        Left = 4
        Top = 39
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
      object txtLocalPagamentoDet: TRLMemo
        Left = 4
        Top = 49
        Width = 533
        Height = 18
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel230: TRLLabel
        Left = 4
        Top = 73
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
      object txtNomeCedenteDet: TRLLabel
        Left = 48
        Top = 73
        Width = 481
        Height = 13
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel229: TRLLabel
        Left = 552
        Top = 48
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
      object txtDataVencimentoDet: TRLLabel
        Left = 552
        Top = 57
        Width = 192
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel232: TRLLabel
        Left = 552
        Top = 76
        Width = 91
        Height = 10
        Caption = 'Ag'#234'ncia / C'#243'digo Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw161: TRLDraw
        Left = 531
        Top = 38
        Width = 17
        Height = 219
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object imgCodigoBarraDet: TRLBarcode
        Left = 10
        Top = 315
        Width = 414
        Height = 58
        Behavior = [beSiteExpander]
        Caption = '00000000000000000000000000000000000000000000'
        CheckSumMethod = cmNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -10
        Font.Name = 'Arial'
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
      object RLLabel243: TRLLabel
        Left = 552
        Top = 208
        Width = 85
        Height = 10
        Caption = '( + ) Outros Acrescimos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtOutrosAcrescimosDet: TRLLabel
        Left = 552
        Top = 217
        Width = 192
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object txtSwHouseCentDet: TRLAngleLabel
      Left = 3
      Top = 564
      Width = 12
      Height = 168
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
    object RLBand6: TRLBand
      Left = 15
      Top = 516
      Width = 768
      Height = 203
      AlignToBottom = True
      BeforePrint = RLBand6BeforePrint
      object RLDraw187: TRLDraw
        Left = 531
        Top = 44
        Width = 17
        Height = 139
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw185: TRLDraw
        Left = 540
        Top = 87
        Width = 224
        Height = 8
        DrawKind = dkLine
      end
      object RLDraw188: TRLDraw
        Left = 82
        Top = 129
        Width = 17
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw189: TRLDraw
        Left = 216
        Top = 128
        Width = 8
        Height = 55
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw190: TRLDraw
        Left = 0
        Top = 174
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw191: TRLDraw
        Left = 0
        Top = 184
        Width = 763
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLDraw192: TRLDraw
        Left = 0
        Top = 148
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw193: TRLDraw
        Left = 0
        Top = 90
        Width = 539
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw194: TRLDraw
        Left = 179
        Top = 3
        Width = 7
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw195: TRLDraw
        Left = 0
        Top = 122
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel241: TRLLabel
        Left = 101
        Top = 131
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
      object txtNumeroDocRecPagDet: TRLLabel
        Left = 100
        Top = 144
        Width = 110
        Height = 11
        Alignment = taCenter
        AutoSize = False
        Caption = 'N'#250'mero do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel245: TRLLabel
        Left = 235
        Top = 131
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
      object txtEspecieDocRecPagDet: TRLLabel
        Left = 235
        Top = 144
        Width = 40
        Height = 11
        Alignment = taCenter
        AutoSize = False
        Caption = 'Esp'#233'cie'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object txtDataDocRecPagDet: TRLLabel
        Left = 9
        Top = 144
        Width = 66
        Height = 11
        AutoSize = False
        Caption = 'Data Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel257: TRLLabel
        Left = 4
        Top = 100
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
      object RLLabel262: TRLLabel
        Left = 306
        Top = 131
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
      object RLDraw196: TRLDraw
        Left = 372
        Top = 129
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw198: TRLDraw
        Left = 0
        Top = 36
        Width = 763
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel287: TRLLabel
        Left = 4
        Top = 69
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
      object RLDraw199: TRLDraw
        Left = 263
        Top = 3
        Width = 11
        Height = 41
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroBancoRecPagDet: TRLLabel
        Left = 186
        Top = 12
        Width = 78
        Height = 29
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -29
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel290: TRLLabel
        Left = 552
        Top = 46
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
      object RLLabel291: TRLLabel
        Left = 3
        Top = 131
        Width = 69
        Height = 10
        Caption = 'Data do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw200: TRLDraw
        Left = 0
        Top = 64
        Width = 763
        Height = 8
        DrawKind = dkLine
      end
      object txtNossoNumeroRecPagDet: TRLLabel
        Left = 553
        Top = 76
        Width = 191
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel294: TRLLabel
        Left = 552
        Top = 92
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
      object RLDraw202: TRLDraw
        Left = 539
        Top = 107
        Width = 224
        Height = 8
        DrawKind = dkLine
      end
      object txtValorDocumentoRecPagDet: TRLLabel
        Left = 622
        Top = 95
        Width = 122
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
      object RLLabel296: TRLLabel
        Left = 552
        Top = 112
        Width = 93
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel297: TRLLabel
        Left = 552
        Top = 158
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
      object txtValorCobradoRecPagDet: TRLLabel
        Left = 552
        Top = 168
        Width = 192
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = '00000'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel300: TRLLabel
        Left = 552
        Top = 131
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
      object txtDescontoRecPagDet: TRLLabel
        Left = 645
        Top = 115
        Width = 99
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
      object txtMoraMultaRecPagDet: TRLLabel
        Left = 552
        Top = 143
        Width = 192
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw206: TRLDraw
        Left = 297
        Top = 129
        Width = 8
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtCodigoCedenteRecPagDet: TRLLabel
        Left = 387
        Top = 144
        Width = 103
        Height = 11
        Caption = 'Agencia/Cod. Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel304: TRLLabel
        Left = 387
        Top = 131
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
      object txtDataVencimentoRecPagDet: TRLLabel
        Left = 599
        Top = 52
        Width = 145
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel139: TRLLabel
        Left = 618
        Top = 19
        Width = 129
        Height = 18
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Recibo do Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomeCedenteRecPagDet: TRLLabel
        Left = 48
        Top = 69
        Width = 489
        Height = 13
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndCedenteRecPagDet: TRLLabel
        Left = 48
        Top = 83
        Width = 489
        Height = 14
        AutoSize = False
        Caption = 'Endereco Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNomePagadorRecPagDet: TRLLabel
        Left = 48
        Top = 101
        Width = 489
        Height = 13
        AutoSize = False
        Caption = 'Nome do Pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndPagadorRecPagDet: TRLLabel
        Left = 48
        Top = 115
        Width = 489
        Height = 14
        AutoSize = False
        Caption = 'EnderecoPagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object imgBancoRecPagDet: TRLImage
        Left = 1
        Top = 2
        Width = 179
        Height = 40
        Scaled = True
      end
      object txtCarteiraRecPagDet: TRLLabel
        Left = 311
        Top = 144
        Width = 40
        Height = 11
        Alignment = taCenter
        Caption = 'Carteira'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel253: TRLLabel
        Left = 92
        Top = 159
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
      object RLLabel292: TRLLabel
        Left = 552
        Top = 68
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
      object RLLabel237: TRLLabel
        Left = 4
        Top = 47
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
      object txtLocalPagamentoRecPagDet: TRLMemo
        Left = 84
        Top = 47
        Width = 453
        Height = 18
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel283: TRLLabel
        Left = 0
        Top = 185
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object RLBandDetalhamento: TRLBand
      Left = 15
      Top = 148
      Width = 768
      Height = 368
      BeforePrint = RLBandDetalhamentoBeforePrint
      object RLDraw143: TRLDraw
        Left = 5
        Top = 595
        Width = 763
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLLabel218: TRLLabel
        Left = -1
        Top = 596
        Width = 21
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDetalhamento: TRLMemo
        Left = 21
        Top = 16
        Width = 724
        Height = 349
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Linha 1'
          'Linha 2'
          'Linha 3'
          'Linha 4'
          'Linha 5'
          'Linha 6'
          'Linha 7'
          'Linha 8'
          'Linha 9'
          'Linha 10'
          'Linha 11'
          'Linha 12'
          'Linha 13'
          'Linha 14'
          'Linha 15'
          'Linha 16'
          'Linha 17'
          'Linha 18'
          'Linha 19'
          'Linha 20'
          'Linha 21'
          'Linha 22'
          'Linha 23'
          'Linha 24'
          'Linha 25'
          'Linha 26'
          'Linha 27'
          'Linha 28'
          'Linha 29'
          'Linha 30')
        ParentFont = False
      end
      object RLLabel189: TRLLabel
        Left = 9
        Top = 5
        Width = 83
        Height = 10
        Caption = 'Detalhamento do Boleto:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlVerso: TRLBand
      Left = 15
      Top = 1103
      Width = 768
      Height = 1049
      object RLDraw162: TRLDraw
        Left = 19
        Top = 765
        Width = 719
        Height = 125
      end
      object RLDraw163: TRLDraw
        Left = 20
        Top = 918
        Width = 719
        Height = 124
      end
      object txtRemententeNome: TRLLabel
        Left = 48
        Top = 775
        Width = 486
        Height = 16
        AutoSize = False
        Caption = 'Nome do Cedente (Remetente)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtRemetenteEndereco: TRLLabel
        Left = 48
        Top = 796
        Width = 59
        Height = 16
        Caption = 'Endere'#231'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtRemetenteBairro: TRLLabel
        Left = 48
        Top = 820
        Width = 38
        Height = 16
        Caption = 'Bairro'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtRemetenteCidade: TRLLabel
        Left = 48
        Top = 844
        Width = 44
        Height = 16
        Caption = 'Cidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtRemetenteCep: TRLLabel
        Left = 48
        Top = 868
        Width = 31
        Height = 16
        Caption = 'CEP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel199: TRLLabel
        Left = 20
        Top = 740
        Width = 722
        Height = 22
        Alignment = taCenter
        AutoSize = False
        Caption = 'Remetente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel214: TRLLabel
        Left = 23
        Top = 895
        Width = 714
        Height = 22
        Alignment = taCenter
        AutoSize = False
        Caption = 'Para uso dos Correios'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw164: TRLDraw
        Left = 27
        Top = 929
        Width = 28
        Height = 18
      end
      object RLLabel216: TRLLabel
        Left = 61
        Top = 931
        Width = 51
        Height = 14
        Caption = 'Mudou-se'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel219: TRLLabel
        Left = 61
        Top = 952
        Width = 107
        Height = 14
        Caption = 'Endere'#231'o insuficiente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw165: TRLDraw
        Left = 27
        Top = 950
        Width = 28
        Height = 18
      end
      object RLDraw166: TRLDraw
        Left = 27
        Top = 992
        Width = 28
        Height = 18
      end
      object RLLabel220: TRLLabel
        Left = 61
        Top = 994
        Width = 72
        Height = 14
        Caption = 'Desconhecido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel221: TRLLabel
        Left = 61
        Top = 973
        Width = 120
        Height = 14
        Caption = 'N'#227'o existe o n'#186' indicado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw167: TRLDraw
        Left = 27
        Top = 971
        Width = 28
        Height = 18
      end
      object RLLabel222: TRLLabel
        Left = 61
        Top = 1015
        Width = 52
        Height = 14
        Caption = 'Recusado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw168: TRLDraw
        Left = 27
        Top = 1013
        Width = 28
        Height = 18
      end
      object RLLabel223: TRLLabel
        Left = 269
        Top = 931
        Width = 75
        Height = 14
        Caption = 'N'#227'o procurado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw169: TRLDraw
        Left = 235
        Top = 929
        Width = 28
        Height = 18
      end
      object RLDraw170: TRLDraw
        Left = 235
        Top = 950
        Width = 28
        Height = 18
      end
      object RLLabel224: TRLLabel
        Left = 269
        Top = 952
        Width = 44
        Height = 14
        Caption = 'Ausente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw171: TRLDraw
        Left = 235
        Top = 971
        Width = 28
        Height = 18
      end
      object RLLabel225: TRLLabel
        Left = 269
        Top = 973
        Width = 43
        Height = 14
        Caption = 'Falecido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw172: TRLDraw
        Left = 235
        Top = 992
        Width = 28
        Height = 18
      end
      object RLLabel226: TRLLabel
        Left = 269
        Top = 994
        Width = 160
        Height = 14
        Caption = 'Inf. escrita pelo porteiro / s'#237'ndico'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw173: TRLDraw
        Left = 235
        Top = 1013
        Width = 28
        Height = 18
      end
      object RLDraw174: TRLDraw
        Left = 471
        Top = 918
        Width = 268
        Height = 124
      end
      object RLLabel228: TRLLabel
        Left = 477
        Top = 925
        Width = 256
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = 'Reintegrado ao Servi'#231'o Postal em:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel231: TRLLabel
        Left = 580
        Top = 959
        Width = 48
        Height = 24
        Alignment = taCenter
        Caption = '/     /'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw175: TRLDraw
        Left = 16
        Top = 371
        Width = 728
        Height = 324
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
      end
      object RLDraw176: TRLDraw
        Left = 30
        Top = 480
        Width = 698
        Height = 134
      end
      object txtDestinatarioNome: TRLLabel
        Left = 56
        Top = 490
        Width = 486
        Height = 16
        AutoSize = False
        Caption = 'Nome do Destinatario'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDestinatarioEndereco: TRLLabel
        Left = 56
        Top = 511
        Width = 59
        Height = 16
        Caption = 'Endere'#231'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDestinatarioBairro: TRLLabel
        Left = 56
        Top = 535
        Width = 38
        Height = 16
        Caption = 'Bairro'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDestinatarioCidade: TRLLabel
        Left = 56
        Top = 559
        Width = 44
        Height = 16
        Caption = 'Cidade'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDestinatarioCEP: TRLLabel
        Left = 56
        Top = 583
        Width = 31
        Height = 16
        Caption = 'CEP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object imgVersoBancoTopDet2: TRLImage
        Left = 42
        Top = 382
        Width = 161
        Height = 42
        Scaled = True
      end
      object RLDraw177: TRLDraw
        Left = 18
        Top = 23
        Width = 728
        Height = 296
      end
      object imgVersoBancoTopDet: TRLImage
        Left = 42
        Top = 78
        Width = 161
        Height = 42
        Scaled = True
      end
      object RLDraw178: TRLDraw
        Left = 471
        Top = 985
        Width = 268
        Height = 57
      end
      object RLLabel233: TRLLabel
        Left = 478
        Top = 986
        Width = 254
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = 'Respons'#225'vel / Visto'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEmpNome2: TRLLabel
        Left = 208
        Top = 394
        Width = 520
        Height = 22
        AutoSize = False
        Caption = 'Empresa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEmpNome: TRLLabel
        Left = 208
        Top = 90
        Width = 520
        Height = 22
        AutoSize = False
        Caption = 'Empresa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw179: TRLDraw
        Left = -2
        Top = 720
        Width = 790
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLDraw180: TRLDraw
        Left = -2
        Top = 346
        Width = 790
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
    end
  end
  object BoletoReciboTopo: TRLReport
    Left = 182
    Top = 98
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 4.000000000000000000
    Margins.TopMargin = 5.000000000000000000
    Margins.RightMargin = 3.000000000000000000
    Margins.BottomMargin = 5.000000000000000000
    PreviewOptions.ShowModal = True
    BeforePrint = BoletoCarneBeforePrint
    OnDataCount = BoletoCarneDataCount
    OnDataRecord = BoletoCarneDataRecord
    object RLBand5: TRLBand
      Left = 15
      Top = 19
      Width = 768
      Height = 521
      BeforePrint = RLBand5BeforePrint
      object RLDraw79: TRLDraw
        Left = 333
        Top = 41
        Width = 17
        Height = 87
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw80: TRLDraw
        Left = 248
        Top = 127
        Width = 10
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw84: TRLDraw
        Left = 456
        Top = 41
        Width = 9
        Height = 115
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw85: TRLDraw
        Left = 195
        Top = 97
        Width = 8
        Height = 31
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw86: TRLDraw
        Left = 69
        Top = 97
        Width = 17
        Height = 31
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw87: TRLDraw
        Left = 162
        Top = 127
        Width = 8
        Height = 29
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw88: TRLDraw
        Left = -18
        Top = 147
        Width = 772
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw89: TRLDraw
        Left = 0
        Top = 62
        Width = 461
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw91: TRLDraw
        Left = -18
        Top = 119
        Width = 478
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw92: TRLDraw
        Left = 0
        Top = 89
        Width = 461
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw93: TRLDraw
        Left = -18
        Top = 33
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object RLLabel12: TRLLabel
        Left = 4
        Top = 45
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
      object txtNomeCedenteRecTop: TRLLabel
        Left = 6
        Top = 56
        Width = 325
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
      object RLLabel17: TRLLabel
        Left = 344
        Top = 45
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
      object txtCodigoCedenteRecTop: TRLLabel
        Left = 350
        Top = 56
        Width = 108
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
      object RLLabel22: TRLLabel
        Left = 169
        Top = 129
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
      object RLLabel24: TRLLabel
        Left = 201
        Top = 99
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
      object txtNumeroDocumentoRecTop: TRLLabel
        Left = 201
        Top = 112
        Width = 79
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
      object RLLabel26: TRLLabel
        Left = 290
        Top = 99
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
      object txtEspecieRecTop: TRLLabel
        Left = 290
        Top = 112
        Width = 44
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
      object RLLabel34: TRLLabel
        Left = 344
        Top = 72
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
      object txtNossoNumeroRecTop: TRLLabel
        Left = 350
        Top = 83
        Width = 108
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
      object RLLabel53: TRLLabel
        Left = 344
        Top = 99
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
      object txtValorDocumentoRecTop: TRLLabel
        Left = 350
        Top = 112
        Width = 108
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object imgBancoRecTop: TRLImage
        Left = 5
        Top = 6
        Width = 160
        Height = 33
        Scaled = True
      end
      object txtNumeroBancoRecTop: TRLLabel
        Left = 179
        Top = 6
        Width = 78
        Height = 29
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -29
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel58: TRLLabel
        Left = 655
        Top = 5
        Width = 94
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Recibo do pagador'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel59: TRLLabel
        Left = 496
        Top = 43
        Width = 219
        Height = 10
        Caption = 'Motivo de n'#227'o entrega. (Para uso da empresa entregadora)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel60: TRLLabel
        Left = 4
        Top = 99
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
      object txtDataVencimentoRecTop: TRLLabel
        Left = 6
        Top = 112
        Width = 69
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
      object RLLabel61: TRLLabel
        Left = 4
        Top = 129
        Width = 130
        Height = 12
        Caption = 'Recebemos o T'#237'tulo / Ausente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel62: TRLLabel
        Left = 6
        Top = 142
        Width = 132
        Height = 11
        Caption = 'com as caracterist'#237'cas acima'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel63: TRLLabel
        Left = 4
        Top = 72
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
      object txtNomeSacadoRecTop: TRLLabel
        Left = 6
        Top = 83
        Width = 325
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
      object RLLabel64: TRLLabel
        Left = 256
        Top = 129
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
      object RLLabel93: TRLLabel
        Left = 466
        Top = 55
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
      object RLLabel100: TRLLabel
        Left = 466
        Top = 66
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
      object RLLabel104: TRLLabel
        Left = 466
        Top = 77
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
      object RLLabel106: TRLLabel
        Left = 560
        Top = 55
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
      object RLLabel108: TRLLabel
        Left = 560
        Top = 66
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
      object RLLabel110: TRLLabel
        Left = 560
        Top = 77
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
      object RLLabel112: TRLLabel
        Left = 649
        Top = 55
        Width = 98
        Height = 10
        Caption = '[   ] N'#227'o existe n'#186'. indicado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel114: TRLLabel
        Left = 649
        Top = 66
        Width = 98
        Height = 10
        Caption = '[   ] Endere'#231'o insuficiente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel116: TRLLabel
        Left = 649
        Top = 77
        Width = 98
        Height = 10
        Caption = '[   ] Outros (anotar no verso)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw95: TRLDraw
        Left = 1
        Top = 158
        Width = 754
        Height = 16
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object RLLabel118: TRLLabel
        Left = 0
        Top = 159
        Width = 26
        Height = 14
        Caption = '"'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw96: TRLDraw
        Left = -18
        Top = 453
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw97: TRLDraw
        Left = -12
        Top = 398
        Width = 768
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw98: TRLDraw
        Left = 538
        Top = 378
        Width = 217
        Height = 12
        DrawKind = dkLine
      end
      object RLDraw99: TRLDraw
        Left = 538
        Top = 351
        Width = 217
        Height = 12
        DrawKind = dkLine
      end
      object RLDraw100: TRLDraw
        Left = -18
        Top = 324
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw101: TRLDraw
        Left = 183
        Top = 300
        Width = 17
        Height = 33
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw102: TRLDraw
        Left = -18
        Top = 292
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw103: TRLDraw
        Left = 391
        Top = 274
        Width = 17
        Height = 59
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw104: TRLDraw
        Left = 318
        Top = 274
        Width = 17
        Height = 27
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw105: TRLDraw
        Left = 245
        Top = 274
        Width = 17
        Height = 59
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw106: TRLDraw
        Left = 121
        Top = 274
        Width = 17
        Height = 59
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw107: TRLDraw
        Left = -18
        Top = 266
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object RLDraw108: TRLDraw
        Left = -18
        Top = 204
        Width = 773
        Height = 16
        DrawKind = dkLine
      end
      object RLDraw109: TRLDraw
        Left = -18
        Top = 234
        Width = 773
        Height = 17
        DrawKind = dkLine
      end
      object imgBancoRecTop1: TRLImage
        Left = 5
        Top = 174
        Width = 160
        Height = 33
        Scaled = True
      end
      object RLDraw110: TRLDraw
        Left = 165
        Top = 175
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtLinhaDigitavelRecTop: TRLLabel
        Left = 264
        Top = 188
        Width = 490
        Height = 19
        AutoSize = False
        Caption = '00000.0000 00000.000000 00000.000000 0 00000000000000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw111: TRLDraw
        Left = 256
        Top = 175
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object txtNumeroBancoRecTop1: TRLLabel
        Left = 179
        Top = 174
        Width = 78
        Height = 29
        Alignment = taCenter
        AutoSize = False
        Caption = '000-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -29
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel120: TRLLabel
        Left = 4
        Top = 213
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
      object txtLocalPagamentoRecTop1: TRLMemo
        Left = 4
        Top = 223
        Width = 533
        Height = 18
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDataVencimentoRecTop1: TRLLabel
        Left = 552
        Top = 228
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
      object RLLabel122: TRLLabel
        Left = 552
        Top = 218
        Width = 56
        Height = 10
        Caption = 'Vencimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw112: TRLDraw
        Left = 530
        Top = 211
        Width = 17
        Height = 250
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel125: TRLLabel
        Left = 4
        Top = 244
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
      object txtNomeCedenteRecTop1: TRLLabel
        Left = 48
        Top = 244
        Width = 486
        Height = 13
        AutoSize = False
        Caption = 'Nome do Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel127: TRLLabel
        Left = 552
        Top = 244
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
      object txtCodigoCedenteRecTop1: TRLLabel
        Left = 552
        Top = 255
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
      object RLLabel129: TRLLabel
        Left = 4
        Top = 276
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
      object txtDataDocumentoRecTop1: TRLLabel
        Left = 4
        Top = 284
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
      object RLLabel131: TRLLabel
        Left = 131
        Top = 276
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
      object txtNumeroDocumentoRecTop1: TRLLabel
        Left = 131
        Top = 284
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
      object RLLabel133: TRLLabel
        Left = 256
        Top = 276
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
      object txtEspecieDocRecTop1: TRLLabel
        Left = 256
        Top = 284
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
      object RLLabel134: TRLLabel
        Left = 329
        Top = 276
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
      object txtAceiteRecTop1: TRLLabel
        Left = 329
        Top = 284
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
      object RLLabel136: TRLLabel
        Left = 403
        Top = 276
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
      object txtDataProcessamentoRecTop1: TRLLabel
        Left = 403
        Top = 284
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
      object RLLabel137: TRLLabel
        Left = 552
        Top = 276
        Width = 68
        Height = 10
        Caption = 'Nosso N'#250'mero'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtNossoNumeroRecTop1: TRLLabel
        Left = 553
        Top = 284
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
      object txtUsoBancoRecTop1: TRLLabel
        Left = 4
        Top = 301
        Width = 124
        Height = 13
        AutoSize = False
        Caption = 'Uso do Banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object RLLabel140: TRLLabel
        Left = 131
        Top = 302
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
      object txtCarteiraRecTop1: TRLLabel
        Left = 131
        Top = 313
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
      object RLLabel141: TRLLabel
        Left = 194
        Top = 302
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
      object txtEspecieRecTop1: TRLLabel
        Left = 194
        Top = 313
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
      object RLLabel142: TRLLabel
        Left = 256
        Top = 302
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
      object txtQuantidadeRecTop1: TRLLabel
        Left = 256
        Top = 313
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
      object RLLabel143: TRLLabel
        Left = 403
        Top = 302
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
      object txtValorMoedaRecTop1: TRLLabel
        Left = 403
        Top = 313
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
      object RLLabel144: TRLLabel
        Left = 552
        Top = 302
        Width = 102
        Height = 10
        Caption = '( = ) Valor do Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorDocumentoRecTop1: TRLLabel
        Left = 551
        Top = 315
        Width = 175
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
      object RLLabel170: TRLLabel
        Left = 4
        Top = 333
        Width = 338
        Height = 10
        Caption = 
          'Instru'#231#245'es (Todas as informa'#231#245'es deste bloqueto s'#227'o de exclusiva' +
          ' responsabilidade do benefici'#225'rio.)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel172: TRLLabel
        Left = 552
        Top = 334
        Width = 107
        Height = 10
        Caption = '( - ) Desconto / Abatimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtDescontoRecTop1: TRLLabel
        Left = 551
        Top = 342
        Width = 175
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
      object RLLabel173: TRLLabel
        Left = 552
        Top = 358
        Width = 63
        Height = 10
        Caption = '( + ) Juros / Multa'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtMoraMultaRecTop1: TRLLabel
        Left = 551
        Top = 369
        Width = 175
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
      object RLLabel174: TRLLabel
        Left = 552
        Top = 384
        Width = 83
        Height = 10
        Caption = '( = ) Valor Cobrado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtValorCobradoRecTop1: TRLLabel
        Left = 551
        Top = 393
        Width = 175
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
      object RLLabel176: TRLLabel
        Left = 51
        Top = 409
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
      object txtNomeSacadoRecTop1: TRLLabel
        Left = 101
        Top = 409
        Width = 429
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
      object RLLabel177: TRLLabel
        Left = 552
        Top = 409
        Width = 60
        Height = 10
        Caption = 'CPF / CNPJ'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCpfCnpjSacadoRecTop1: TRLLabel
        Left = 550
        Top = 419
        Width = 204
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
      object txtEnderecoSacadoRecTop1: TRLLabel
        Left = 101
        Top = 422
        Width = 429
        Height = 14
        AutoSize = False
        Caption = 'Rua / Numero / Complemento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel178: TRLLabel
        Left = 551
        Top = 435
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
      object txtCidadeSacadoRecTop1: TRLLabel
        Left = 101
        Top = 435
        Width = 429
        Height = 14
        AutoSize = False
        Caption = 'Cep / Bairro / Cidade / Estado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtCodigoBaixaRecTop1: TRLLabel
        Left = 550
        Top = 445
        Width = 204
        Height = 16
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel179: TRLLabel
        Left = 572
        Top = 466
        Width = 166
        Height = 10
        Caption = 'Autentica'#231#227'o Mec'#226'nica - Ficha de Compensa'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel180: TRLLabel
        Left = 6
        Top = 446
        Width = 88
        Height = 14
        Caption = 'Pagador/Avalista:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtReferenciaRecTop1: TRLLabel
        Left = 550
        Top = 432
        Width = 204
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
      object txtEndCedenteRecTop1: TRLLabel
        Left = 48
        Top = 258
        Width = 106
        Height = 15
        Caption = 'Endereco Cedente'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtSacadorAvalistaRecTop1: TRLLabel
        Left = 101
        Top = 446
        Width = 428
        Height = 14
        AutoSize = False
        Caption = 'Nome do Pagador/Avalista'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object mIntrucoesRecTop1: TRLMemo
        Left = 8
        Top = 344
        Width = 521
        Height = 59
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object imgBarrasRecTop1: TRLBarcode
        Left = 4
        Top = 464
        Width = 421
        Height = 50
        AutoSize = False
        Caption = '00000000000000000000000000000000000000000000'
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
      object RLDraw114: TRLDraw
        Left = 256
        Top = 5
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw115: TRLDraw
        Left = 461
        Top = 87
        Width = 294
        Height = 10
        DrawKind = dkLine
      end
      object txtOrientacoesBancoRecTop1: TRLMemo
        Left = 466
        Top = 99
        Width = 286
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
      object RLDraw94: TRLDraw
        Left = 283
        Top = 97
        Width = 8
        Height = 31
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel65: TRLLabel
        Left = 82
        Top = 99
        Width = 64
        Height = 10
        Caption = 'Para uso do banco'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtLinhaDigitavelRecTopRecPag: TRLLabel
        Left = 264
        Top = 20
        Width = 490
        Height = 19
        AutoSize = False
        Caption = '00000.0000 00000.000000 00000.000000 0 00000000000000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw90: TRLDraw
        Left = 165
        Top = 5
        Width = 11
        Height = 36
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
    end
    object RLDraw113: TRLDraw
      Left = 21
      Top = 534
      Width = 754
      Height = 16
      DrawKind = dkLine
      Pen.Style = psDot
    end
    object RLLabel181: TRLLabel
      Left = 30
      Top = 535
      Width = 26
      Height = 14
      Caption = '"'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
  end
  object BoletoCarne: TRLReport
    Left = 98
    Top = 53
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
      object txtOrientacoesBancoCarne: TRLMemo
        Left = 0
        Top = 316
        Width = 142
        Height = 32
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -7
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
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
        Font.Name = 'Arial'
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
        Top = 159
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
        Top = 171
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
        Top = 157
        Width = 141
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw69: TRLDraw
        Left = 0
        Top = 169
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
      object RLLabel48: TRLLabel
        Left = 1
        Top = 349
        Width = 61
        Height = 12
        Caption = 'Recibo do Pagador'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel49: TRLLabel
        Left = 71
        Top = 349
        Width = 61
        Height = 12
        Caption = 'Autenticar no Verso'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
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
        Left = 0
        Top = 305
        Width = 142
        Height = 11
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
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
        Left = 202
        Top = 70
        Width = 359
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
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
        Top = 283
        Width = 142
        Height = 23
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object txtEndCedenteCarne: TRLLabel
        Left = 155
        Top = 81
        Width = 406
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
        Width = 142
        Height = 58
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        Lines.Strings = (
          '1'
          '2'
          '3'
          '4')
        ParentFont = False
      end
      object txtNomeSacadorAval4: TRLLabel
        Left = 240
        Top = 265
        Width = 325
        Height = 13
        AutoSize = False
        Caption = 'txtNomeSacadorAval'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel88: TRLLabel
        Left = 0
        Top = 182
        Width = 69
        Height = 10
        Caption = 'N'#250'mero Documento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw119: TRLDraw
        Left = 0
        Top = 181
        Width = 141
        Height = 1
        DrawKind = dkLine
      end
      object txtNumeroDocumentoCarne: TRLLabel
        Left = 3
        Top = 190
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
