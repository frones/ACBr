inherited frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato
  Left = 209
  Top = 84
  Caption = 'Manifesto - Retrato'
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  PixelsPerInch = 96
  TextHeight = 10
  inherited RLMDFe: TRLReport
    Tag = 1
    Left = 2
    Top = 2
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = rlMDFeBeforePrint
    OnDataRecord = RLMDFeDataRecord
    object rlb_2_Rodo: TRLBand
      Left = 26
      Top = 368
      Width = 742
      Height = 208
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_2_RodoBeforePrint
      object RLPanel7: TRLPanel
        Left = 1
        Top = 0
        Width = 740
        Height = 208
        Align = faBottom
        object RLDraw7: TRLDraw
          Left = 0
          Top = 36
          Width = 740
          Height = 9
          Align = faWidth
          DrawKind = dkLine
        end
        object rlShape10: TRLDraw
          Left = 0
          Top = 16
          Width = 740
          Height = 9
          Align = faWidth
          DrawKind = dkLine
        end
        object rlLabel35: TRLLabel
          Left = 3
          Top = 4
          Width = 37
          Height = 14
          Caption = 'Ve'#237'culo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel9: TRLLabel
          Left = 318
          Top = 4
          Width = 46
          Height = 14
          Caption = 'Condutor'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel13: TRLLabel
          Left = 4
          Top = 24
          Width = 28
          Height = 14
          Caption = 'Placa'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel14: TRLLabel
          Left = 168
          Top = 24
          Width = 40
          Height = 14
          Caption = 'RNTRC'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel15: TRLLabel
          Left = 318
          Top = 24
          Width = 23
          Height = 14
          Caption = 'CPF'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel16: TRLLabel
          Left = 412
          Top = 24
          Width = 31
          Height = 14
          Caption = 'Nome'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLDraw5: TRLDraw
          Left = 315
          Top = 0
          Width = 1
          Height = 208
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object RLDraw6: TRLDraw
          Left = 408
          Top = 20
          Width = 1
          Height = 82
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object rlmCPF: TRLMemo
          Left = 317
          Top = 45
          Width = 85
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 412
          Top = 45
          Width = 325
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
        object RLDraw8: TRLDraw
          Left = 161
          Top = 20
          Width = 1
          Height = 81
          Angle = 90.000000000000000000
          DrawKind = dkLine
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
          Font.Name = 'Times New Roman'
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
          Font.Name = 'Times New Roman'
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
        object RLDraw9: TRLDraw
          Left = 0
          Top = 101
          Width = 740
          Height = 1
          Align = faWidth
          DrawKind = dkLine
        end
        object RLLabel7: TRLLabel
          Left = 4
          Top = 103
          Width = 62
          Height = 14
          Caption = 'Vale Ped'#225'gio'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLDraw10: TRLDraw
          Left = 0
          Top = 120
          Width = 740
          Height = 1
          Align = faWidth
          DrawKind = dkLine
        end
        object rlLabel19: TRLLabel
          Left = 4
          Top = 123
          Width = 89
          Height = 14
          Caption = 'Respons'#225'vel CNPJ'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel20: TRLLabel
          Left = 98
          Top = 123
          Width = 84
          Height = 14
          Caption = 'Fornecedor CNPJ'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlLabel21: TRLLabel
          Left = 196
          Top = 123
          Width = 81
          Height = 14
          Caption = 'N. Comprovante'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlShape16: TRLDraw
          Left = 94
          Top = 120
          Width = 1
          Height = 88
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object rlShape17: TRLDraw
          Left = 192
          Top = 120
          Width = 1
          Height = 88
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object rlmRespCNPJ: TRLMemo
          Left = 4
          Top = 141
          Width = 86
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Top = 141
          Width = 86
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Top = 141
          Width = 114
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 319
          Top = 103
          Width = 126
          Height = 14
          Caption = 'Respons'#225'vel pelo Seguro - '
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel29: TRLLabel
          Left = 319
          Top = 123
          Width = 96
          Height = 14
          Caption = 'Nome da Seguradora'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlmRespSeguradora: TRLMemo
          Left = 319
          Top = 143
          Width = 214
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 539
          Top = 123
          Width = 92
          Height = 14
          Caption = 'N'#250'mero da Ap'#243'lice'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlmRespApolice: TRLMemo
          Left = 539
          Top = 141
          Width = 198
          Height = 52
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
        object RLDraw16: TRLDraw
          Left = 535
          Top = 120
          Width = 1
          Height = 88
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object rlmRespSeguro: TRLLabel
          Left = 443
          Top = 103
          Width = 46
          Height = 14
          Caption = 'Emitente'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
      end
    end
    object rlb_3_Aereo: TRLBand
      Left = 26
      Top = 576
      Width = 742
      Height = 54
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_3_AereoBeforePrint
    end
    object rlb_4_Aquav: TRLBand
      Left = 26
      Top = 630
      Width = 742
      Height = 121
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_4_AquavBeforePrint
      object RLPanel8: TRLPanel
        Left = 1
        Top = 1
        Width = 740
        Height = 119
        Align = faClient
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLPanel9: TRLPanel
          Left = 0
          Top = 0
          Width = 740
          Height = 20
          Align = faTop
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = True
          object rlLabel24: TRLLabel
            Left = 6
            Top = 3
            Width = 107
            Height = 14
            Caption = 'C'#243'digo da Embarca'#231#227'o'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object rllCodEmbar: TRLLabel
            Left = 116
            Top = 2
            Width = 85
            Height = 16
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object rlLabel26: TRLLabel
            Left = 214
            Top = 3
            Width = 103
            Height = 14
            Caption = 'Nome da Embarca'#231#227'o'
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object rllNomeEmbar: TRLLabel
            Left = 322
            Top = 2
            Width = 415
            Height = 16
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
        end
        object RLLabel8: TRLLabel
          Left = 6
          Top = 24
          Width = 35
          Height = 14
          Caption = 'C'#243'digo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel11: TRLLabel
          Left = 78
          Top = 24
          Width = 172
          Height = 14
          Caption = 'Nome do Terminal de Carregamento'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel18: TRLLabel
          Left = 382
          Top = 24
          Width = 35
          Height = 14
          Caption = 'C'#243'digo'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object RLLabel31: TRLLabel
          Left = 454
          Top = 24
          Width = 187
          Height = 14
          Caption = 'Nome do Terminal de Descarregamento'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object rlmCodCarreg: TRLMemo
          Left = 6
          Top = 45
          Width = 59
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 78
          Top = 45
          Width = 291
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 382
          Top = 45
          Width = 59
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
          Left = 454
          Top = 45
          Width = 284
          Height = 62
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
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
        object RLDraw11: TRLDraw
          Left = 0
          Top = 35
          Width = 740
          Height = 9
          Align = faWidth
          DrawKind = dkLine
        end
        object RLDraw12: TRLDraw
          Left = 72
          Top = 35
          Width = 1
          Height = 84
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object RLDraw13: TRLDraw
          Left = 375
          Top = 35
          Width = 8
          Height = 84
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
        object RLDraw14: TRLDraw
          Left = 450
          Top = 35
          Width = 8
          Height = 84
          Angle = 90.000000000000000000
          DrawKind = dkLine
        end
      end
    end
    object rlb_5_Ferrov: TRLBand
      Left = 26
      Top = 751
      Width = 742
      Height = 60
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_5_FerrovBeforePrint
    end
    object rlb_6_Observacao: TRLBand
      Left = 26
      Top = 929
      Width = 742
      Height = 152
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
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
        Font.Height = -9
        Font.Name = 'Times New Roman'
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
        Width = 56
        Height = 14
        Caption = 'Observa'#231#227'o'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMsg1: TRLLabel
        Left = 11
        Top = 38
        Width = 724
        Height = 31
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGA'#199#195'O - SEM VALOR FISCAL'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Times New Roman'
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
        Font.Name = 'Times New Roman'
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
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllMsg2: TRLLabel
        Left = 325
        Top = 75
        Width = 98
        Height = 31
        Alignment = taCenter
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_7_Documentos_Titulos: TRLBand
      Left = 26
      Top = 811
      Width = 742
      Height = 32
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      IntegralHeight = False
      ParentColor = False
      BeforePrint = rlb_7_Documentos_TitulosBeforePrint
      object rlLabel141: TRLLabel
        Left = 254
        Top = 4
        Width = 244
        Height = 12
        Alignment = taCenter
        Caption = 'RELA'#199#195'O DOS DOCUMENTOS FISCAIS ELETR'#212'NICOS'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel91: TRLLabel
        Left = 5
        Top = 22
        Width = 29
        Height = 8
        Caption = 'TP DOC.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel92: TRLLabel
        Left = 88
        Top = 22
        Width = 69
        Height = 8
        Caption = 'CNPJ/CPF EMITENTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel96: TRLLabel
        Left = 174
        Top = 22
        Width = 86
        Height = 8
        Caption = 'S'#201'RIE/NRO. DOCUMENTO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel109: TRLLabel
        Left = 381
        Top = 22
        Width = 29
        Height = 8
        Caption = 'TP DOC.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel106: TRLLabel
        Left = 464
        Top = 22
        Width = 69
        Height = 8
        Caption = 'CNPJ/CPF EMITENTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel100: TRLLabel
        Left = 550
        Top = 22
        Width = 86
        Height = 8
        Caption = 'S'#201'RIE/NRO. DOCUMENTO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
    end
    object rlb_2: TRLBand
      Left = 26
      Top = 256
      Width = 742
      Height = 112
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
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
      object RLDraw19: TRLDraw
        Left = 1
        Top = 17
        Width = 428
        Height = 9
        DrawKind = dkLine
      end
      object rlLabel12: TRLLabel
        Left = 150
        Top = 26
        Width = 120
        Height = 14
        Caption = 'PESO TOTAL (Kg)'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllPesoTotal: TRLLabel
        Left = 150
        Top = 42
        Width = 120
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllqNFeMDFe: TRLLabel
        Left = 72
        Top = 42
        Width = 74
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllqCTe: TRLLabel
        Left = 1
        Top = 42
        Width = 66
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lblQTDENFeMDFe: TRLLabel
        Left = 72
        Top = 26
        Width = 74
        Height = 15
        AutoSize = False
        Caption = 'QTDE MDF-e'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel5: TRLLabel
        Left = 1
        Top = 26
        Width = 66
        Height = 18
        AutoSize = False
        Caption = 'QTDE CT-e'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllTituloValorMerc: TRLLabel
        Left = 274
        Top = 26
        Width = 152
        Height = 14
        Caption = 'VALOR DA MERCADORIA R$'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllValorMercadoria: TRLLabel
        Left = 274
        Top = 42
        Width = 152
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object RLPanel4: TRLPanel
        Left = 432
        Top = 2
        Width = 306
        Height = 64
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object RLBarcode1: TRLBarcode
          Left = 4
          Top = 24
          Width = 298
          Height = 39
          Margins.LeftMargin = 1.000000000000000000
          Margins.RightMargin = 1.000000000000000000
          Alignment = taCenter
          AutoSize = False
          BarcodeType = bcCode128C
        end
        object rlLabel74: TRLLabel
          Left = 5
          Top = 4
          Width = 63
          Height = 11
          Caption = 'Controle do Fisco'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object RLPanel3: TRLPanel
        Left = 432
        Top = 68
        Width = 307
        Height = 42
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object rllChave: TRLLabel
          Left = 3
          Top = 22
          Width = 300
          Height = 14
          Alignment = taCenter
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel1: TRLLabel
          Left = 3
          Top = 5
          Width = 58
          Height = 11
          Caption = 'Chave de acesso'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
      end
      object rllDescricao: TRLLabel
        Left = 4
        Top = 72
        Width = 141
        Height = 8
        Caption = 'PROTOCOLO DE AUTORIZA'#199#195'O DE USO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw17: TRLDraw
        Left = 1
        Top = 60
        Width = 428
        Height = 9
        DrawKind = dkLine
      end
      object RLDraw2: TRLDraw
        Left = 70
        Top = 21
        Width = 1
        Height = 44
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw3: TRLDraw
        Left = 428
        Top = 21
        Width = 1
        Height = 44
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw4: TRLDraw
        Left = 148
        Top = 20
        Width = 1
        Height = 44
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw15: TRLDraw
        Left = 272
        Top = 21
        Width = 1
        Height = 44
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllModal: TRLLabel
        Left = 1
        Top = 3
        Width = 427
        Height = 15
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
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllProtocolo: TRLMemo
        Left = 12
        Top = 86
        Width = 404
        Height = 23
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object subItens: TRLSubDetail
      Left = 26
      Top = 843
      Width = 742
      Height = 86
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = False
      BeforePrint = subItensBeforePrint
      OnDataRecord = subItensDataRecord
      object rlbItens: TRLBand
        Left = 1
        Top = 0
        Width = 740
        Height = 49
        Background.Height = 487
        Background.Width = 865
        AfterPrint = rlbItensAfterPrint
        BeforePrint = rlbItensBeforePrint
        object LinhaQuantidade: TRLDraw
          Left = 374
          Top = 16
          Width = 1
          Height = 33
          Align = faHeight
          Angle = 90.000000000000000000
          DrawKind = dkLine
          HoldStyle = hsRelatively
        end
        object rlmChave1: TRLMemo
          Left = 3
          Top = 30
          Width = 354
          Height = 16
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          Lines.Strings = (
            'rlmChave1')
          ParentColor = False
          ParentFont = False
        end
        object rlmChave2: TRLMemo
          Left = 381
          Top = 30
          Width = 354
          Height = 16
          AutoSize = False
          Behavior = [beSiteExpander]
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = []
          Lines.Strings = (
            'rlmChave2')
          ParentColor = False
          ParentFont = False
        end
        object rlbNumcipio: TRLLabel
          Left = 0
          Top = 0
          Width = 740
          Height = 16
          Align = faTop
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = True
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
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
      Height = 230
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_1_DadosManifestoBeforePrint
      object rliLogo: TRLImage
        Left = 6
        Top = 57
        Width = 96
        Height = 96
        Center = True
      end
      object rlmEmitente: TRLMemo
        Left = 7
        Top = 10
        Width = 586
        Height = 39
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmDadosEmitente: TRLMemo
        Left = 109
        Top = 54
        Width = 484
        Height = 108
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
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
      object RLPanel6: TRLPanel
        Left = 1
        Top = 199
        Width = 740
        Height = 31
        Align = faBottom
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = True
        Borders.DrawRight = False
        Borders.DrawBottom = False
        object rllModelo: TRLLabel
          Left = 1
          Top = 16
          Width = 30
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllSerie: TRLLabel
          Left = 39
          Top = 16
          Width = 20
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllNumMDFe: TRLLabel
          Left = 64
          Top = 16
          Width = 70
          Height = 15
          Alignment = taRightJustify
          Caption = '999.999.999'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLSystemInfo1: TRLSystemInfo
          Left = 136
          Top = 17
          Width = 32
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          Info = itPagePreview
          ParentFont = False
          Text = '0#/0#'
        end
        object rllEmissao: TRLLabel
          Left = 172
          Top = 16
          Width = 104
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Caption = '99/99/9999 99:99:99'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rllUFCarrega: TRLLabel
          Left = 279
          Top = 16
          Width = 34
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rllUFDescarrega: TRLLabel
          Left = 317
          Top = 16
          Width = 34
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Times New Roman'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object rlLabel2: TRLLabel
          Left = 1
          Top = 7
          Width = 32
          Height = 8
          Alignment = taCenter
          Caption = 'MODELO'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel3: TRLLabel
          Left = 39
          Top = 7
          Width = 22
          Height = 8
          Alignment = taCenter
          Caption = 'S'#201'RIE'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel4: TRLLabel
          Left = 64
          Top = 7
          Width = 70
          Height = 9
          Alignment = taCenter
          AutoSize = False
          Caption = 'N'#218'MERO'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel25: TRLLabel
          Left = 136
          Top = 7
          Width = 32
          Height = 9
          Alignment = taCenter
          AutoSize = False
          Caption = 'FOLHA'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel33: TRLLabel
          Left = 172
          Top = 7
          Width = 104
          Height = 9
          Alignment = taCenter
          AutoSize = False
          Caption = 'DATA E HORA DE EMISS'#195'O'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlLabel77: TRLLabel
          Left = 279
          Top = 7
          Width = 35
          Height = 8
          Caption = 'UF Carrega'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object RLLabel6: TRLLabel
          Left = 317
          Top = 7
          Width = 34
          Height = 8
          Caption = 'UF Descar.'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -7
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object rlsLinhaV05: TRLDraw
          Left = 35
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
          HoldStyle = hsRelatively
        end
        object rlsLinhaV06: TRLDraw
          Left = 62
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object rlsLinhaV07: TRLDraw
          Left = 135
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object rlsLinhaV08: TRLDraw
          Left = 169
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object rlsLinhaV09: TRLDraw
          Left = 277
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
          HoldStyle = hsRelatively
        end
        object RLDraw1: TRLDraw
          Left = 315
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object RLDraw18: TRLDraw
          Left = 352
          Top = 0
          Width = 1
          Height = 31
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
      end
      object rlLabel17: TRLLabel
        Left = 4
        Top = 174
        Width = 89
        Height = 22
        Caption = 'DAMDFE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object RLMemo1: TRLMemo
        Left = 99
        Top = 178
        Width = 323
        Height = 14
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Documento Auxiliar de Manifesto Eletr'#244'nico de Documentos Fiscais')
        ParentFont = False
      end
      object imgQRCode: TRLImage
        Left = 617
        Top = 10
        Width = 111
        Height = 107
        Center = True
        Scaled = True
      end
    end
  end
  inherited RLPDFFilter1: TRLPDFFilter
    Left = 234
    Top = 39
  end
end
