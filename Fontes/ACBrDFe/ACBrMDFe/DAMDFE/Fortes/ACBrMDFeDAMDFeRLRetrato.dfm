inherited frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato
  Left = 279
  Top = 68
  Caption = 'Manifesto - Retrato'
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  TextHeight = 10
  inherited RLMDFe: TRLReport
    Tag = 1
    Left = 26
    Top = 2
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    Background.Align = faBottom
    Borders.Sides = sdCustom
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = rlMDFeBeforePrint
    OnDataRecord = RLMDFeDataRecord
    object rlb_2_Rodo: TRLBand
      Left = 26
      Top = 341
      Width = 742
      Height = 208
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_2_RodoBeforePrint
      object RLPanel7: TRLPanel
        Left = 0
        Top = 8
        Width = 742
        Height = 200
        Align = faBottom
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLDraw7: TRLDraw
          Left = 0
          Top = 35
          Width = 316
          Height = 9
          Color = clWhite
          DrawKind = dkLine
          ParentColor = False
          Pen.Color = clSilver
          Pen.Width = 2
          Transparent = False
        end
        object rlLabel35: TRLLabel
          Left = 3
          Top = 4
          Width = 44
          Height = 14
          Caption = 'Ve'#237'culo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel9: TRLLabel
          Left = 331
          Top = 4
          Width = 55
          Height = 14
          Caption = 'Condutor'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel13: TRLLabel
          Left = 4
          Top = 24
          Width = 29
          Height = 14
          Caption = 'Placa'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel14: TRLLabel
          Left = 168
          Top = 24
          Width = 37
          Height = 14
          Caption = 'RNTRC'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel15: TRLLabel
          Left = 331
          Top = 24
          Width = 22
          Height = 14
          Caption = 'CPF'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel16: TRLLabel
          Left = 425
          Top = 24
          Width = 30
          Height = 14
          Caption = 'Nome'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlmCPF: TRLMemo
          Left = 330
          Top = 45
          Width = 85
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmCondutor: TRLMemo
          Left = 425
          Top = 45
          Width = 310
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmPlaca: TRLMemo
          Left = 4
          Top = 45
          Width = 151
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmRNTRC: TRLMemo
          Left = 168
          Top = 44
          Width = 141
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLLabel7: TRLLabel
          Left = 4
          Top = 106
          Width = 74
          Height = 14
          Caption = 'Vale Ped'#225'gio'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel19: TRLLabel
          Left = 4
          Top = 125
          Width = 89
          Height = 11
          Caption = 'Respons'#225'vel CNPJ'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel20: TRLLabel
          Left = 98
          Top = 125
          Width = 84
          Height = 11
          Caption = 'Fornecedor CNPJ'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel21: TRLLabel
          Left = 196
          Top = 125
          Width = 77
          Height = 11
          Caption = 'N. Comprovante'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlmRespCNPJ: TRLMemo
          Left = 4
          Top = 144
          Width = 86
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmFornCNPJ: TRLMemo
          Left = 100
          Top = 144
          Width = 86
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmNumComprovante: TRLMemo
          Left = 196
          Top = 144
          Width = 114
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLLabel28: TRLLabel
          Left = 327
          Top = 106
          Width = 154
          Height = 14
          Caption = 'Respons'#225'vel pelo Seguro - '
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object RLLabel29: TRLLabel
          Left = 327
          Top = 125
          Width = 99
          Height = 11
          Caption = 'Nome da Seguradora'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlmRespSeguradora: TRLMemo
          Left = 328
          Top = 144
          Width = 178
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLLabel30: TRLLabel
          Left = 547
          Top = 125
          Width = 87
          Height = 11
          Caption = 'N'#250'mero da Ap'#243'lice'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlmRespApolice: TRLMemo
          Left = 547
          Top = 144
          Width = 174
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmRespSeguro: TRLLabel
          Left = 489
          Top = 106
          Width = 52
          Height = 14
          Caption = 'Emitente'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object RLDraw1: TRLDraw
          Left = 325
          Top = 35
          Width = 410
          Height = 9
          DrawKind = dkLine
          Pen.Color = clSilver
          Pen.Width = 2
        end
        object RLDraw2: TRLDraw
          Left = 0
          Top = 136
          Width = 316
          Height = 9
          Color = clWhite
          DrawKind = dkLine
          ParentColor = False
          Pen.Color = clSilver
          Pen.Style = psDot
          Pen.Width = 2
          Transparent = False
        end
        object RLDraw3: TRLDraw
          Left = 326
          Top = 136
          Width = 410
          Height = 9
          Color = clWhite
          DrawKind = dkLine
          ParentColor = False
          Pen.Color = clSilver
          Pen.Width = 2
          Transparent = False
        end
      end
    end
    object rlb_3_Aereo: TRLBand
      Left = 26
      Top = 549
      Width = 742
      Height = 20
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_3_AereoBeforePrint
    end
    object rlb_4_Aquav: TRLBand
      Left = 26
      Top = 569
      Width = 742
      Height = 121
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_4_AquavBeforePrint
      object RLPanel8: TRLPanel
        Left = 0
        Top = 0
        Width = 742
        Height = 121
        Align = faClient
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLPanel9: TRLPanel
          Left = 0
          Top = 0
          Width = 742
          Height = 20
          Align = faTop
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = False
          object rlLabel24: TRLLabel
            Left = 3
            Top = 3
            Width = 127
            Height = 14
            Caption = 'C'#243'digo da Embarca'#231#227'o'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object rllCodEmbar: TRLLabel
            Left = 132
            Top = 2
            Width = 85
            Height = 16
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object rlLabel26: TRLLabel
            Left = 379
            Top = 3
            Width = 120
            Height = 14
            Caption = 'Nome da Embarca'#231#227'o'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object rllNomeEmbar: TRLLabel
            Left = 505
            Top = 2
            Width = 415
            Height = 16
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
        end
        object RLLabel8: TRLLabel
          Left = 2
          Top = 24
          Width = 36
          Height = 14
          Caption = 'C'#243'digo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel11: TRLLabel
          Left = 70
          Top = 24
          Width = 174
          Height = 14
          Caption = 'Nome do Terminal de Carregamento'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel18: TRLLabel
          Left = 379
          Top = 24
          Width = 36
          Height = 14
          Caption = 'C'#243'digo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel31: TRLLabel
          Left = 451
          Top = 24
          Width = 192
          Height = 14
          Caption = 'Nome do Terminal de Descarregamento'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlmCodCarreg: TRLMemo
          Left = 3
          Top = 45
          Width = 59
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha'
            '5 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmNomeCarreg: TRLMemo
          Left = 70
          Top = 45
          Width = 291
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmCodDescarreg: TRLMemo
          Left = 377
          Top = 45
          Width = 59
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlmNomeDescarreg: TRLMemo
          Left = 449
          Top = 45
          Width = 284
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '1 Linha'
            '2 Linha'
            '3 Linha'
            '4 Linha')
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLDraw4: TRLDraw
          Left = 0
          Top = 38
          Width = 369
          Height = 9
          Color = clWhite
          DrawKind = dkLine
          ParentColor = False
          Pen.Color = clSilver
          Pen.Width = 2
          Transparent = False
        end
        object RLDraw5: TRLDraw
          Left = 377
          Top = 38
          Width = 362
          Height = 9
          Color = clWhite
          DrawKind = dkLine
          ParentColor = False
          Pen.Color = clSilver
          Pen.Width = 2
          Transparent = False
        end
      end
    end
    object rlb_5_Ferrov: TRLBand
      Left = 26
      Top = 690
      Width = 742
      Height = 20
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_5_FerrovBeforePrint
    end
    object rlb_6_Observacao: TRLBand
      Left = 26
      Top = 842
      Width = 742
      Height = 152
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_6_ObservacaoBeforePrint
      object rlmObservacao: TRLMemo
        Left = 1
        Top = 25
        Width = 734
        Height = 108
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel22: TRLLabel
        Left = 4
        Top = 4
        Width = 68
        Height = 14
        Caption = 'Observa'#231#227'o'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllMsg1: TRLLabel
        Left = 14
        Top = 38
        Width = 718
        Height = 32
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGA'#199#195'O - SEM VALOR FISCAL'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllDataHoraImpressao: TRLLabel
        Left = 1
        Top = 139
        Width = 77
        Height = 10
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllSistema: TRLLabel
        Left = 352
        Top = 139
        Width = 387
        Height = 11
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllMsg2: TRLLabel
        Left = 322
        Top = 75
        Width = 104
        Height = 32
        Alignment = taCenter
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object RLDraw9: TRLDraw
        Left = 0
        Top = 16
        Width = 745
        Height = 9
        Color = clWhite
        DrawKind = dkLine
        ParentColor = False
        Pen.Color = clSilver
        Pen.Width = 2
        Transparent = False
      end
    end
    object rlb_7_Documentos_Titulos: TRLBand
      Left = 26
      Top = 710
      Width = 742
      Height = 46
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      IntegralHeight = False
      ParentColor = False
      BeforePrint = rlb_7_Documentos_TitulosBeforePrint
      object rlLabel141: TRLLabel
        Left = 252
        Top = 4
        Width = 248
        Height = 14
        Alignment = taCenter
        Caption = 'Rela'#231#227'o dos Documentos Fiscais Eletr'#244'nicos'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel91: TRLLabel
        Left = 5
        Top = 22
        Width = 43
        Height = 14
        Caption = 'Tp. Doc.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel92: TRLLabel
        Left = 88
        Top = 22
        Width = 93
        Height = 14
        Caption = 'CNPJ/CPF Emitente'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel96: TRLLabel
        Left = 192
        Top = 22
        Width = 108
        Height = 14
        Caption = 'S'#233'rie/Nro. Documento'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel109: TRLLabel
        Left = 381
        Top = 22
        Width = 40
        Height = 14
        Caption = 'Tp Doc.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel106: TRLLabel
        Left = 464
        Top = 22
        Width = 93
        Height = 14
        Caption = 'CNPJ/CPF Emitente'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel100: TRLLabel
        Left = 567
        Top = 22
        Width = 108
        Height = 14
        Caption = 'S'#233'rie/Nro. Documento'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw6: TRLDraw
        Left = 0
        Top = 36
        Width = 367
        Height = 9
        Color = clWhite
        DrawKind = dkLine
        ParentColor = False
        Pen.Color = clSilver
        Pen.Style = psDot
        Transparent = False
      end
      object RLDraw8: TRLDraw
        Left = 375
        Top = 36
        Width = 364
        Height = 9
        Color = clWhite
        DrawKind = dkLine
        ParentColor = False
        Pen.Color = clSilver
        Pen.Style = psDot
        Transparent = False
      end
    end
    object rlb_2: TRLBand
      Left = 26
      Top = 213
      Width = 742
      Height = 128
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      BeforePrint = rlb_3_AereoBeforePrint
      object RLPanel_Contingencia: TRLPanel
        Left = 2
        Top = 74
        Width = 326
        Height = 34
        Color = clBlack
        ParentColor = False
        Transparent = False
      end
      object RLPanel14: TRLPanel
        Left = 205
        Top = 20
        Width = 122
        Height = 30
        Color = cl3DLight
        ParentColor = False
        Transparent = False
      end
      object RLPanel13: TRLPanel
        Left = 106
        Top = 20
        Width = 96
        Height = 30
        Color = cl3DLight
        ParentColor = False
        Transparent = False
      end
      object RLPanel12: TRLPanel
        Left = 4
        Top = 20
        Width = 99
        Height = 30
        Color = cl3DLight
        ParentColor = False
        Transparent = False
      end
      object rlLabel12: TRLLabel
        Left = 206
        Top = 22
        Width = 79
        Height = 14
        Caption = 'PESO TOTAL (Kg)'
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllPesoTotal: TRLLabel
        Left = 206
        Top = 35
        Width = 88
        Height = 14
        AutoSize = False
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllqNFeMDFe: TRLLabel
        Left = 111
        Top = 35
        Width = 74
        Height = 14
        AutoSize = False
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllqCTe: TRLLabel
        Left = 7
        Top = 35
        Width = 78
        Height = 14
        AutoSize = False
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object lblQTDENFeMDFe: TRLLabel
        Left = 111
        Top = 22
        Width = 74
        Height = 14
        AutoSize = False
        Caption = 'QTDE MDF-e'
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel5: TRLLabel
        Left = 7
        Top = 22
        Width = 78
        Height = 14
        AutoSize = False
        Caption = 'QTDE CT-e'
        Color = cl3DLight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLPanel4: TRLPanel
        Left = 330
        Top = 0
        Width = 408
        Height = 72
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLBarcode1: TRLBarcode
          Left = 1
          Top = 18
          Width = 286
          Height = 54
          Margins.LeftMargin = 1.000000000000000000
          Margins.RightMargin = 1.000000000000000000
          Alignment = taJustify
          BarcodeType = bcCode128C
          Caption = '12345678901234567890123456789012345678901324'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlJustify
          ParentFont = False
        end
        object rlLabel74: TRLLabel
          Left = 2
          Top = 1
          Width = 100
          Height = 15
          Caption = 'Controle do Fisco'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object RLPanel3: TRLPanel
        Left = 330
        Top = 74
        Width = 407
        Height = 35
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object rllChave: TRLLabel
          Left = 3
          Top = 17
          Width = 350
          Height = 14
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel1: TRLLabel
          Left = 3
          Top = 2
          Width = 97
          Height = 14
          Caption = 'Chave de acesso'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object rllDescricao: TRLLabel
        Left = 4
        Top = 58
        Width = 180
        Height = 14
        Caption = 'Protocolo de autoriza'#231#227'o de uso'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllModal: TRLLabel
        Left = 1
        Top = 3
        Width = 326
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        Caption = 'Modal Rodovi'#225'rio de Carga'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllProtocolo: TRLMemo
        Left = 3
        Top = 75
        Width = 323
        Height = 14
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLLabel10: TRLLabel
        Left = 330
        Top = 112
        Width = 59
        Height = 12
        Caption = 'Consulte em:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLLabel23: TRLLabel
        Left = 394
        Top = 112
        Width = 249
        Height = 11
        Caption = 'https://dfe-portal.sefazvirtual.rs.gov.br/MDFe/consulta'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object subItens: TRLSubDetail
      Left = 26
      Top = 756
      Width = 742
      Height = 86
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      BeforePrint = subItensBeforePrint
      OnDataRecord = subItensDataRecord
      object rlbItens: TRLBand
        Left = 0
        Top = 0
        Width = 742
        Height = 38
        Background.Height = 487
        Background.Width = 865
        AfterPrint = rlbItensAfterPrint
        BeforePrint = rlbItensBeforePrint
        object rlmChave1: TRLMemo
          Left = 0
          Top = 14
          Width = 361
          Height = 16
          Align = faLeftTop
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'rlmChave1')
          ParentColor = False
          ParentFont = False
        end
        object rlmChave2: TRLMemo
          Left = 377
          Top = 14
          Width = 365
          Height = 16
          Align = faRightTop
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'rlmChave2')
          ParentColor = False
          ParentFont = False
        end
        object rlbMunicipio: TRLLabel
          Left = 0
          Top = 0
          Width = 742
          Height = 14
          Align = faTop
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
      end
    end
    object rlb_1_DadosManifesto: TRLBand
      Left = 26
      Top = 26
      Width = 742
      Height = 187
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_1_DadosManifestoBeforePrint
      object rliLogo: TRLImage
        Left = 8
        Top = 9
        Width = 121
        Height = 87
        Center = True
        Scaled = True
      end
      object rlmEmitente: TRLMemo
        Left = 134
        Top = 11
        Width = 431
        Height = 19
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmDadosEmitente: TRLMemo
        Left = 134
        Top = 35
        Width = 433
        Height = 89
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          '1 Linha - LOGRADOURO - COMPLEMENTO - BAIRRO'
          '2 Linha - CEP - MUNICIPIO - UF'
          '3 Linha - CNPJ INSCRICAO ESTADUAL'
          '4 Linha - TELEFONE'
          '5 Linha - URL')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel17: TRLLabel
        Left = 4
        Top = 127
        Width = 73
        Height = 19
        Caption = 'DAMDFE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object RLMemo1: TRLMemo
        Left = 83
        Top = 128
        Width = 486
        Height = 20
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Documento Auxiliar de Manifesto Eletr'#244'nico de Documentos Fiscais')
        ParentFont = False
      end
      object imgQRCode: TRLImage
        Left = 574
        Top = 12
        Width = 162
        Height = 136
        Center = True
        Scaled = True
      end
      object RLPanel6: TRLPanel
        Left = 0
        Top = 147
        Width = 569
        Height = 38
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLPanel1: TRLPanel
          Left = 3
          Top = 2
          Width = 202
          Height = 35
          Color = clSkyBlue
          ParentColor = False
          Transparent = False
        end
        object RLPanel11: TRLPanel
          Left = 469
          Top = 2
          Width = 68
          Height = 35
          Color = clSkyBlue
          ParentColor = False
          Transparent = False
        end
        object RLPanel10: TRLPanel
          Left = 261
          Top = 2
          Width = 131
          Height = 35
          Color = clSkyBlue
          ParentColor = False
          Transparent = False
        end
        object RLPanel5: TRLPanel
          Left = 396
          Top = 2
          Width = 69
          Height = 35
          Color = clSkyBlue
          ParentColor = False
          Transparent = False
        end
        object RLPanel2: TRLPanel
          Left = 209
          Top = 2
          Width = 48
          Height = 35
          Color = clSkyBlue
          ParentColor = False
          Transparent = False
        end
        object rlLabel2: TRLLabel
          Left = 20
          Top = 4
          Width = 36
          Height = 10
          Borders.Color = clWindow
          Caption = 'MODELO'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllModelo: TRLLabel
          Left = 20
          Top = 15
          Width = 30
          Height = 15
          AutoSize = False
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel3: TRLLabel
          Left = 69
          Top = 4
          Width = 26
          Height = 10
          Caption = 'S'#201'RIE'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllSerie: TRLLabel
          Left = 69
          Top = 15
          Width = 20
          Height = 15
          AutoSize = False
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel4: TRLLabel
          Left = 110
          Top = 4
          Width = 70
          Height = 9
          AutoSize = False
          Caption = 'N'#218'MERO'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllNumMDFe: TRLLabel
          Left = 110
          Top = 15
          Width = 63
          Height = 15
          Caption = '999.999.999'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel25: TRLLabel
          Left = 217
          Top = 4
          Width = 32
          Height = 9
          AutoSize = False
          Caption = 'FOLHA'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLSystemInfo1: TRLSystemInfo
          Left = 217
          Top = 15
          Width = 32
          Height = 15
          AutoSize = False
          Color = clSkyBlue
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Info = itPagePreview
          ParentColor = False
          ParentFont = False
          Text = '0#/0#'
          Transparent = False
        end
        object rlLabel33: TRLLabel
          Left = 273
          Top = 4
          Width = 104
          Height = 9
          Alignment = taCenter
          AutoSize = False
          Caption = 'DATA E HORA DE EMISS'#195'O'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllEmissao: TRLLabel
          Left = 274
          Top = 15
          Width = 104
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Caption = '99/99/9999 99:99:99'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel77: TRLLabel
          Left = 405
          Top = 3
          Width = 44
          Height = 10
          Caption = 'UF Carrega'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllUFCarrega: TRLLabel
          Left = 405
          Top = 15
          Width = 34
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLLabel6: TRLLabel
          Left = 476
          Top = 3
          Width = 42
          Height = 10
          Caption = 'UF Descar.'
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllUFDescarrega: TRLLabel
          Left = 476
          Top = 15
          Width = 34
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clSkyBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
    end
  end
  inherited RLPDFFilter1: TRLPDFFilter
    Left = 58
    Top = 39
  end
end
