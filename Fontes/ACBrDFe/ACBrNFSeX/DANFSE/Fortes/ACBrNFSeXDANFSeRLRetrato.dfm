inherited frlXDANFSeRLRetrato: TfrlXDANFSeRLRetrato
  Left = 709
  Top = 76
  Caption = 'frlXDANFSeRLRetrato'
  ClientHeight = 750
  TextHeight = 13
  inherited RLNFSe: TRLReport
    Left = 32
    Top = 0
    Margins.LeftMargin = 6.000000000000000000
    Margins.TopMargin = 8.000000000000000000
    Margins.RightMargin = 5.099999999999999000
    Margins.BottomMargin = 8.000000000000000000
    BeforePrint = RLNFSeBeforePrint
    object rlbCabecalho: TRLBand
      Left = 23
      Top = 30
      Width = 752
      Height = 219
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlbCabecalhoBeforePrint
      object RLDraw3: TRLDraw
        Left = 595
        Top = 1
        Width = 1
        Height = 180
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw2: TRLDraw
        Left = 598
        Top = 48
        Width = 154
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllNumNF0: TRLLabel
        Left = 605
        Top = 28
        Width = 140
        Height = 18
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel13: TRLLabel
        Left = 630
        Top = 4
        Width = 90
        Height = 15
        Alignment = taCenter
        Caption = 'N'#250'mero da Nota'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel12: TRLLabel
        Left = 610
        Top = 50
        Width = 130
        Height = 15
        Alignment = taCenter
        Caption = 'Data e Hora de Emiss'#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rliLogo: TRLImage
        Left = 9
        Top = 9
        Width = 120
        Height = 112
        Center = True
        Scaled = True
      end
      object rllEmissao: TRLLabel
        Left = 605
        Top = 72
        Width = 140
        Height = 18
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rllCodigoChave: TRLLabel
        Left = 428
        Top = 182
        Width = 118
        Height = 15
        Alignment = taCenter
        Caption = 'C'#243'digo de Verifica'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllCodVerificacao: TRLLabel
        Left = 424
        Top = 199
        Width = 324
        Height = 18
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw70: TRLDraw
        Left = 598
        Top = 92
        Width = 154
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw8: TRLDraw
        Left = 3
        Top = 180
        Width = 749
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel7: TRLLabel
        Left = 4
        Top = 182
        Width = 77
        Height = 15
        Caption = 'Compet'#234'ncia:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllCompetencia: TRLLabel
        Left = 2
        Top = 200
        Width = 86
        Height = 15
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw9: TRLDraw
        Left = 93
        Top = 180
        Width = 1
        Height = 40
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel18: TRLLabel
        Left = 97
        Top = 182
        Width = 94
        Height = 15
        Caption = 'Num. RPS/Ser.:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllNumeroRps: TRLLabel
        Left = 95
        Top = 200
        Width = 118
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw10: TRLDraw
        Left = 218
        Top = 180
        Width = 1
        Height = 39
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel20: TRLLabel
        Left = 601
        Top = 96
        Width = 147
        Height = 15
        Caption = 'N'#250'mero NFSe substitu'#237'da:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllNumNFSeSubstituida: TRLLabel
        Left = 605
        Top = 117
        Width = 135
        Height = 15
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rlmPrefeitura: TRLMemo
        Left = 134
        Top = 4
        Width = 458
        Height = 92
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel60: TRLLabel
        Left = 194
        Top = 154
        Width = 347
        Height = 22
        Caption = 'Nota Fiscal de Servi'#231'o Eletr'#244'nica - NFS-e'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -19
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLSystemInfo1: TRLSystemInfo
        Left = 653
        Top = 159
        Width = 27
        Height = 15
        Alignment = taRightJustify
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        Info = itPageNumber
        ParentFont = False
        Text = ''
      end
      object RLSystemInfo2: TRLSystemInfo
        Left = 692
        Top = 159
        Width = 27
        Height = 15
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        Info = itLastPageNumber
        ParentFont = False
        Text = ''
      end
      object RLLabel62: TRLLabel
        Left = 682
        Top = 159
        Width = 7
        Height = 15
        Caption = '/'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel63: TRLLabel
        Left = 663
        Top = 143
        Width = 41
        Height = 15
        Caption = 'P'#225'gina'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw11: TRLDraw
        Left = 420
        Top = 180
        Width = 1
        Height = 39
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel64: TRLLabel
        Left = 220
        Top = 182
        Width = 186
        Height = 15
        Caption = 'Munic'#237'pio de Presta'#231#227'o do Servi'#231'o:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllMunicipioPrestacaoServico: TRLLabel
        Left = 220
        Top = 200
        Width = 196
        Height = 15
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw12: TRLDraw
        Left = 596
        Top = 136
        Width = 154
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
    end
    object rlbPrestador: TRLBand
      Left = 23
      Top = 249
      Width = 752
      Height = 104
      BandType = btTitle
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlbPrestadorBeforePrint
      object RLLabel30: TRLLabel
        Left = 351
        Top = 36
        Width = 20
        Height = 14
        Caption = 'IM:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel32: TRLLabel
        Left = 135
        Top = 35
        Width = 55
        Height = 14
        Caption = 'CPF/CNPJ:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllPrestInscMunicipal: TRLLabel
        Left = 379
        Top = 35
        Width = 117
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllPrestEndereco: TRLMemo
        Left = 134
        Top = 51
        Width = 525
        Height = 47
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllPrestCNPJ: TRLLabel
        Left = 194
        Top = 35
        Width = 127
        Height = 13
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rliPrestLogo: TRLImage
        Left = 9
        Top = 4
        Width = 95
        Height = 93
        Center = True
        Scaled = True
      end
      object RLLabel2: TRLLabel
        Left = 293
        Top = 2
        Width = 166
        Height = 15
        Alignment = taCenter
        Caption = 'PRESTADOR DE SERVI'#199'OS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel1: TRLLabel
        Left = 135
        Top = 19
        Width = 95
        Height = 14
        Caption = 'Nome/Raz'#227'o Social:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllPrestNome: TRLLabel
        Left = 234
        Top = 19
        Width = 73
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel69: TRLLabel
        Left = 507
        Top = 34
        Width = 17
        Height = 14
        Caption = 'IE:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllPrestInscEstadual: TRLLabel
        Left = 529
        Top = 34
        Width = 108
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object rlbTomador: TRLBand
      Left = 23
      Top = 353
      Width = 752
      Height = 102
      BandType = btTitle
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlbTomadorBeforePrint
      object RLLabel4: TRLLabel
        Left = 299
        Top = 3
        Width = 156
        Height = 15
        Alignment = taCenter
        Caption = 'TOMADOR DE SERVI'#199'OS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object lbIdentificacao: TRLLabel
        Left = 15
        Top = 35
        Width = 55
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'CPFCNPJ:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaCNPJ: TRLLabel
        Left = 74
        Top = 35
        Width = 74
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel11: TRLLabel
        Left = 488
        Top = 35
        Width = 97
        Height = 14
        Caption = 'Inscri'#231#227'o Municipal:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaInscMunicipal: TRLLabel
        Left = 592
        Top = 35
        Width = 118
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel15: TRLLabel
        Left = 15
        Top = 19
        Width = 95
        Height = 14
        Caption = 'Nome/Raz'#227'o Social:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaNome: TRLLabel
        Left = 114
        Top = 19
        Width = 74
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel17: TRLLabel
        Left = 15
        Top = 51
        Width = 49
        Height = 14
        Caption = 'Endere'#231'o:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaEndereco: TRLLabel
        Left = 66
        Top = 51
        Width = 91
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel19: TRLLabel
        Left = 15
        Top = 83
        Width = 53
        Height = 14
        Caption = 'Munic'#237'pio:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaMunicipio: TRLLabel
        Left = 70
        Top = 83
        Width = 97
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel21: TRLLabel
        Left = 374
        Top = 83
        Width = 20
        Height = 14
        Caption = 'UF:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaUF: TRLLabel
        Left = 398
        Top = 83
        Width = 58
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel10: TRLLabel
        Left = 470
        Top = 83
        Width = 35
        Height = 14
        Caption = 'e-mail:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaEmail: TRLLabel
        Left = 510
        Top = 83
        Width = 74
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel25: TRLLabel
        Left = 15
        Top = 67
        Width = 72
        Height = 14
        Caption = 'Complemento:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaComplemento: TRLLabel
        Left = 94
        Top = 67
        Width = 118
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel27: TRLLabel
        Left = 430
        Top = 67
        Width = 48
        Height = 14
        Caption = 'Telefone:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaTelefone: TRLLabel
        Left = 486
        Top = 67
        Width = 89
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel61: TRLLabel
        Left = 252
        Top = 35
        Width = 89
        Height = 14
        Caption = 'Inscri'#231#227'o Estadual:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomaInscEstadual: TRLLabel
        Left = 356
        Top = 35
        Width = 109
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object rlbHeaderItens: TRLBand
      Left = 23
      Top = 455
      Width = 752
      Height = 23
      BandType = btColumnHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      object RLLabel14: TRLLabel
        Left = 274
        Top = 4
        Width = 204
        Height = 15
        Alignment = taCenter
        Caption = 'DISCRIMINA'#199#195'O DOS SERVI'#199'OS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object rlbItens: TRLBand
      Left = 23
      Top = 498
      Width = 752
      Height = 21
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlbItensBeforePrint
      object rlmDescricao: TRLMemo
        Left = 10
        Top = 6
        Width = 737
        Height = 14
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
    end
    object rlbISSQN: TRLBand
      Left = 23
      Top = 537
      Width = 752
      Height = 318
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlbISSQNBeforePrint
      object RLDraw52: TRLDraw
        Left = 1
        Top = 76
        Width = 751
        Height = 1
        DrawKind = dkLine
      end
      object RLDraw53: TRLDraw
        Left = 144
        Top = 94
        Width = 1
        Height = 42
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw54: TRLDraw
        Left = 436
        Top = 94
        Width = 1
        Height = 42
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLDraw55: TRLDraw
        Left = 608
        Top = 94
        Width = 1
        Height = 42
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object RLLabel137: TRLLabel
        Left = 297
        Top = 162
        Width = 176
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Natureza da Opera'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel138: TRLLabel
        Left = 488
        Top = 210
        Width = 153
        Height = 15
        AutoSize = False
        Caption = '(=) Base de C'#225'lculo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel139: TRLLabel
        Left = 488
        Top = 269
        Width = 153
        Height = 15
        AutoSize = False
        Caption = '(=) Valor ISS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllBaseCalc: TRLLabel
        Left = 648
        Top = 210
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorISS: TRLLabel
        Left = 648
        Top = 269
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLDraw4: TRLDraw
        Left = 0
        Top = 293
        Width = 752
        Height = 1
        DrawKind = dkLine
      end
      object rllValorTotal: TRLLabel
        Left = 300
        Top = 298
        Width = 150
        Height = 13
        Alignment = taCenter
        Caption = 'VALOR TOTAL DA NOTA = '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel16: TRLLabel
        Left = 4
        Top = 4
        Width = 89
        Height = 14
        Caption = 'C'#243'digo do Servi'#231'o:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rlmCodServico: TRLMemo
        Left = 100
        Top = 4
        Width = 645
        Height = 16
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel3: TRLLabel
        Left = 488
        Top = 226
        Width = 153
        Height = 15
        AutoSize = False
        Caption = '(x) Al'#237'quota (%)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllAliquota: TRLLabel
        Left = 648
        Top = 226
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw6: TRLDraw
        Left = 292
        Top = 94
        Width = 1
        Height = 42
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaH1: TRLDraw
        Left = 0
        Top = 56
        Width = 752
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllCodigoObra: TRLLabel
        Left = 432
        Top = 61
        Width = 76
        Height = 14
        Caption = 'C'#243'digo da Obra:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllCodObra: TRLLabel
        Left = 512
        Top = 61
        Width = 64
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllTituloConstCivil: TRLLabel
        Left = 6
        Top = 60
        Width = 334
        Height = 15
        Alignment = taCenter
        Caption = 'DETALHAMENTO ESPECIFICO DA CONSTRU'#199#195'O CIVIL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object rllCodigoArt: TRLLabel
        Left = 595
        Top = 61
        Width = 64
        Height = 14
        Caption = 'C'#243'digo ART:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllCodART: TRLLabel
        Left = 663
        Top = 61
        Width = 59
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel34: TRLLabel
        Left = 7
        Top = 98
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'PIS (R$)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorPIS: TRLLabel
        Left = 7
        Top = 116
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel36: TRLLabel
        Left = 152
        Top = 98
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'COFINS (R$)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorCOFINS: TRLLabel
        Left = 152
        Top = 116
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel38: TRLLabel
        Left = 300
        Top = 98
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'IR (R$)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorIR: TRLLabel
        Left = 300
        Top = 116
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel40: TRLLabel
        Left = 470
        Top = 98
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'INSS (R$)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorINSS: TRLLabel
        Left = 470
        Top = 116
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel42: TRLLabel
        Left = 614
        Top = 98
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'CSLL (R$)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorCSLL: TRLLabel
        Left = 614
        Top = 116
        Width = 130
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel44: TRLLabel
        Left = 311
        Top = 79
        Width = 130
        Height = 15
        Alignment = taCenter
        Caption = 'TRIBUTOS FEDERAIS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLDraw13: TRLDraw
        Left = 0
        Top = 94
        Width = 752
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw14: TRLDraw
        Left = 0
        Top = 135
        Width = 752
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel35: TRLLabel
        Left = 6
        Top = 139
        Width = 274
        Height = 15
        Alignment = taCenter
        Caption = 'Detalhamento de Valores - Prestador dos Servi'#231'os'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel37: TRLLabel
        Left = 488
        Top = 139
        Width = 258
        Height = 20
        Alignment = taCenter
        AutoSize = False
        Caption = 'C'#225'lculo do ISSQN devido no Munic'#237'pio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel39: TRLLabel
        Left = 297
        Top = 139
        Width = 176
        Height = 20
        Alignment = taCenter
        AutoSize = False
        Caption = 'Outras Informa'#231#245'es'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel41: TRLLabel
        Left = 488
        Top = 162
        Width = 153
        Height = 15
        AutoSize = False
        Caption = 'Valor dos Servi'#231'os'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel43: TRLLabel
        Left = 488
        Top = 178
        Width = 153
        Height = 15
        AutoSize = False
        Caption = '(-) Dedu'#231#245'es permitidas em Lei'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel45: TRLLabel
        Left = 488
        Top = 194
        Width = 153
        Height = 15
        AutoSize = False
        Caption = '(-) Desconto Incondicionado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel46: TRLLabel
        Left = 488
        Top = 242
        Width = 153
        Height = 15
        AutoSize = False
        Caption = 'ISS a reter:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel47: TRLLabel
        Left = 8
        Top = 162
        Width = 146
        Height = 15
        AutoSize = False
        Caption = 'Valor dos Servi'#231'os'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel48: TRLLabel
        Left = 8
        Top = 178
        Width = 146
        Height = 15
        AutoSize = False
        Caption = '(-) Desconto Incondicionado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel49: TRLLabel
        Left = 8
        Top = 194
        Width = 146
        Height = 15
        AutoSize = False
        Caption = '(-) Desconto Condicionado'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel50: TRLLabel
        Left = 8
        Top = 210
        Width = 146
        Height = 15
        AutoSize = False
        Caption = '(-) Reten'#231#245'es Federais'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel51: TRLLabel
        Left = 8
        Top = 226
        Width = 146
        Height = 15
        AutoSize = False
        Caption = '(-) Outras Reten'#231#245'es'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel52: TRLLabel
        Left = 8
        Top = 242
        Width = 146
        Height = 15
        AutoSize = False
        Caption = '(-) ISS Retido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel53: TRLLabel
        Left = 8
        Top = 269
        Width = 145
        Height = 15
        AutoSize = False
        Caption = '(=) Valor L'#237'quido'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel54: TRLLabel
        Left = 297
        Top = 218
        Width = 176
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Regime Especial de Tributa'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel55: TRLLabel
        Left = 297
        Top = 261
        Width = 120
        Height = 15
        AutoSize = False
        Caption = 'Op'#231#227'o Simples Nacional'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel56: TRLLabel
        Left = 297
        Top = 277
        Width = 104
        Height = 15
        AutoSize = False
        Caption = 'Incentivador Cultural'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw15: TRLDraw
        Left = 292
        Top = 135
        Width = 1
        Height = 158
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw16: TRLDraw
        Left = 480
        Top = 135
        Width = 1
        Height = 159
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllValorServicos1: TRLLabel
        Left = 159
        Top = 162
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorServicos2: TRLLabel
        Left = 648
        Top = 162
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllDescIncondicionado1: TRLLabel
        Left = 159
        Top = 178
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllDescIncondicionado2: TRLLabel
        Left = 648
        Top = 194
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllDescCondicionado: TRLLabel
        Left = 159
        Top = 194
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllRetencoesFederais: TRLLabel
        Left = 159
        Top = 210
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllOutrasRetencoes: TRLLabel
        Left = 159
        Top = 226
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorIssRetido: TRLLabel
        Left = 159
        Top = 242
        Width = 129
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorLiquido: TRLLabel
        Left = 160
        Top = 265
        Width = 121
        Height = 20
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLDraw17: TRLDraw
        Left = 0
        Top = 260
        Width = 752
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllIncentivador: TRLLabel
        Left = 424
        Top = 277
        Width = 49
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllNatOperacao: TRLMemo
        Left = 297
        Top = 178
        Width = 176
        Height = 38
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllValorDeducoes: TRLLabel
        Left = 648
        Top = 178
        Width = 97
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllRegimeEspecial: TRLLabel
        Left = 297
        Top = 235
        Width = 176
        Height = 23
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllOpcaoSimples: TRLLabel
        Left = 424
        Top = 261
        Width = 49
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllISSReter: TRLLabel
        Left = 648
        Top = 242
        Width = 98
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllMsgTeste: TRLLabel
        Left = 13
        Top = 45
        Width = 724
        Height = 31
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGA'#199#195'O - SEM VALOR FISCAL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -27
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rllCodTributacaoMunicipio: TRLLabel
        Left = 4
        Top = 22
        Width = 168
        Height = 14
        Caption = 'C'#243'digo de Tributa'#231#227'o do Munic'#237'pio:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rlmDescCodTributacaoMunicipio: TRLMemo
        Left = 178
        Top = 21
        Width = 565
        Height = 16
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object rbOutrasInformacoes: TRLBand
      Left = 23
      Top = 855
      Width = 752
      Height = 40
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rbOutrasInformacoesBeforePrint
      object rlmDadosAdicionais: TRLMemo
        Left = 5
        Top = 21
        Width = 743
        Height = 14
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Dados Adicionais....')
        ParentFont = False
      end
      object RLLabel6: TRLLabel
        Left = 299
        Top = 2
        Width = 152
        Height = 15
        Alignment = taCenter
        Caption = 'OUTRAS INFORMA'#199#213'ES'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
    object rlbCanhoto: TRLBand
      Left = 23
      Top = 913
      Width = 752
      Height = 73
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      BeforePrint = rbOutrasInformacoesBeforePrint
      object RLDraw7: TRLDraw
        Left = 1
        Top = 6
        Width = 751
        Height = 64
      end
      object RLLabel26: TRLLabel
        Left = 5
        Top = 7
        Width = 78
        Height = 14
        Caption = 'Recebi(emos) de'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object rllPrestNomeEnt: TRLLabel
        Left = 88
        Top = 7
        Width = 505
        Height = 15
        AutoSize = False
        Caption = '<Nome do Tomador>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLLabel28: TRLLabel
        Left = 6
        Top = 19
        Width = 351
        Height = 14
        Caption = 
          'os servi'#231'os constantes da Nota Fiscal Eletronica de Servi'#231'o  (NF' +
          'Se) ao lado.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw1: TRLDraw
        Left = 603
        Top = 6
        Width = 1
        Height = 64
        Angle = 90.000000000000000000
        DrawKind = dkLine
      end
      object rllNumNF0Ent: TRLLabel
        Left = 607
        Top = 42
        Width = 140
        Height = 18
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel57: TRLLabel
        Left = 633
        Top = 15
        Width = 90
        Height = 15
        Alignment = taCenter
        Caption = 'N'#250'mero da Nota'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
      object RLLabel33: TRLLabel
        Left = 202
        Top = 53
        Width = 137
        Height = 10
        Caption = 'Identifica'#231#227'o e Assinatura do Recebedor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLDraw5: TRLDraw
        Left = 345
        Top = 64
        Width = 248
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel58: TRLLabel
        Left = 12
        Top = 53
        Width = 25
        Height = 10
        Caption = 'DATA'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel59: TRLLabel
        Left = 54
        Top = 53
        Width = 136
        Height = 12
        Caption = '_______ / _______ / __________'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllTomadorNomeEnt: TRLLabel
        Left = 6
        Top = 32
        Width = 591
        Height = 15
        AutoSize = False
        Caption = '<Nome do Tomador>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object rlbHeaderItensDetalhado: TRLBand
      Left = 23
      Top = 478
      Width = 752
      Height = 20
      BandType = btColumnHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      object RLLabel65: TRLLabel
        Left = 460
        Top = 1
        Width = 25
        Height = 14
        Caption = 'Qtde'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel66: TRLLabel
        Left = 4
        Top = 1
        Width = 48
        Height = 14
        Caption = 'Descri'#231#227'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel67: TRLLabel
        Left = 710
        Top = 1
        Width = 17
        Height = 14
        Caption = 'ISS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel68: TRLLabel
        Left = 384
        Top = 1
        Width = 70
        Height = 14
        Caption = 'Valor Unit'#225'rio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel5: TRLLabel
        Left = 505
        Top = 1
        Width = 79
        Height = 14
        Caption = 'Valor do Servi'#231'o'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object RLLabel9: TRLLabel
        Left = 601
        Top = 1
        Width = 94
        Height = 14
        Caption = 'Base de C'#225'lculo (%)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
    end
    object subItens: TRLSubDetail
      Left = 23
      Top = 519
      Width = 752
      Height = 18
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      OnDataRecord = subItensDataRecord
      object rlbItensServico: TRLBand
        Left = 1
        Top = 0
        Width = 750
        Height = 16
        BeforePrint = rlbItensServicoBeforePrint
        object txtServicoQtde: TRLLabel
          Left = 459
          Top = 1
          Width = 36
          Height = 14
          Alignment = taCenter
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
        object rlmServicoDescricao: TRLMemo
          Left = 4
          Top = 1
          Width = 373
          Height = 14
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
        object txtServicoUnitario: TRLLabel
          Left = 383
          Top = 1
          Width = 70
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
        object txtServicoTotal: TRLLabel
          Left = 506
          Top = 1
          Width = 76
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
        object txtBaseCalculo: TRLLabel
          Left = 601
          Top = 1
          Width = 93
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
        object txtISS: TRLLabel
          Left = 709
          Top = 1
          Width = 35
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          Transparent = False
        end
      end
    end
    object RLBand1: TRLBand
      Left = 23
      Top = 895
      Width = 752
      Height = 18
      BandType = btSummary
      object rllDataHoraImpressao: TRLLabel
        Left = 2
        Top = 3
        Width = 76
        Height = 10
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
      object rllSistema: TRLLabel
        Left = 356
        Top = 2
        Width = 392
        Height = 11
        Alignment = taRightJustify
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Transparent = False
      end
    end
  end
  inherited RLPDFFilter1: TRLPDFFilter
    Left = 120
  end
end
