object ACBrSATExtratoFortesFr: TACBrSATExtratoFortesFr
  Left = 449
  Top = 7
  Width = 805
  Height = 1067
  Caption = 'ACBrSATExtratoFortes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object rlVenda: TRLReport
    Left = 40
    Top = 0
    Width = 302
    Height = 1512
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 2.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 2.000000000000000000
    Margins.BottomMargin = 20.000000000000000000
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 80.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlVendaBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbRodape: TRLBand
      Left = 8
      Top = 608
      Width = 286
      Height = 316
      AutoSize = True
      BandType = btSummary
      object pSATSerieHora: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 32
        Align = faTop
        AutoExpand = True
        BeforePrint = pSATSerieHoraBeforePrint
        object lDataHora: TRLLabel
          Left = 0
          Top = 16
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '14/08/1971 - 08:00:00'
          Layout = tlCenter
        end
        object pNumSAT: TRLPanel
          Left = 0
          Top = 0
          Width = 286
          Height = 16
          Align = faTop
          AutoExpand = True
          AutoSize = True
          BeforePrint = pNumSATBeforePrint
          object lTitSAT: TRLLabel
            Left = 0
            Top = 0
            Width = 130
            Height = 16
            Align = faLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'SAT N'#176': '
            Layout = tlCenter
          end
          object lNumSAT: TRLLabel
            Left = 130
            Top = 0
            Width = 156
            Height = 16
            Align = faClient
            Caption = '900.000.102'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            Layout = tlCenter
            ParentFont = False
          end
        end
      end
      object lChaveAcesso: TRLMemo
        Left = 0
        Top = 32
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        Lines.Strings = (
          '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111')
        ParentFont = False
      end
      object bcChaveAcesso1: TRLBarcode
        Left = 0
        Top = 44
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '3514031111111111111159'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object bcChaveAcesso2: TRLBarcode
        Left = 0
        Top = 70
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '9000001020002235664805'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object pGap05: TRLPanel
        Left = 0
        Top = 96
        Width = 286
        Height = 9
        Align = faBottom
      end
      object pQRCode: TRLPanel
        Left = 0
        Top = 105
        Width = 286
        Height = 135
        Align = faBottom
        BeforePrint = pQRCodeBeforePrint
        object imgQRCode: TRLImage
          Left = 0
          Top = 0
          Width = 144
          Height = 135
          Align = faClient
          Center = True
          Scaled = True
        end
        object pTextoLateral: TRLPanel
          Left = 144
          Top = 0
          Width = 142
          Height = 135
          Align = faRight
          Margins.LeftMargin = 2.000000000000000000
          object lTitLei12744Lateral: TRLMemo
            Left = 8
            Top = 121
            Width = 134
            Height = 14
            Align = faBottom
            Behavior = [beSiteExpander]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -7
            Font.Name = 'Arial'
            Font.Style = []
            Lines.Strings = (
              '* Valor Aproximado dos Tributos dos Itens')
            ParentFont = False
          end
          object mMsgAppQRCodeLateral: TRLMemo
            Left = 8
            Top = 93
            Width = 134
            Height = 28
            Align = faBottom
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -7
            Font.Name = 'Arial'
            Font.Style = []
            Lines.Strings = (
              'Consulte o QR Code pelo aplicativo  "De olho na nota",'
              'dispon'#237'vel na AppStore (Apple) e PlayStore (Android)')
            ParentFont = False
          end
          object pConsumidorLateral: TRLPanel
            Left = 8
            Top = 0
            Width = 134
            Height = 28
            Align = faTop
            AutoExpand = True
            AutoSize = True
            BeforePrint = pConsumidorLateralBeforePrint
            object lTitConsumidorLateral: TRLLabel
              Left = 0
              Top = 0
              Width = 134
              Height = 14
              Align = faTop
              Alignment = taCenter
              Caption = 'Consumidor'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object mConsumidorLateral: TRLMemo
              Left = 0
              Top = 14
              Width = 134
              Height = 14
              Align = faTop
              Alignment = taCenter
              Behavior = [beSiteExpander]
            end
          end
          object pNumSATDataHoraLateral: TRLPanel
            Left = 8
            Top = 28
            Width = 134
            Height = 26
            Align = faTop
            AutoExpand = True
            AutoSize = True
            BeforePrint = pNumSATDataHoraLateralBeforePrint
            object pNumSATLateral: TRLPanel
              Left = 0
              Top = 0
              Width = 134
              Height = 12
              Align = faTop
              BeforePrint = pNumSATLateralBeforePrint
              object lTitSATLateral: TRLLabel
                Left = 0
                Top = 0
                Width = 72
                Height = 12
                Align = faLeftTop
                Alignment = taRightJustify
                AutoSize = False
                Caption = 'N'#176' S'#233'rie SAT '
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -9
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                Layout = tlCenter
                ParentFont = False
              end
              object lNumSATLateral: TRLLabel
                Left = 72
                Top = 0
                Width = 62
                Height = 12
                Align = faClientTop
                Caption = '900.000.102'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBlack
                Font.Height = -9
                Font.Name = 'Arial'
                Font.Style = []
                Layout = tlCenter
                ParentFont = False
              end
            end
            object lDataHoraLateral: TRLLabel
              Left = 0
              Top = 12
              Width = 134
              Height = 14
              Align = faTop
              Alignment = taCenter
              Caption = '14/08/1971 - 08:00:00'
              Layout = tlCenter
            end
          end
        end
      end
      object mMsgAppQRCode: TRLMemo
        Left = 0
        Top = 250
        Width = 286
        Height = 24
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Consulte o QR Code pelo aplicativo  "De olho na nota",'
          'dispon'#237'vel na AppStore (Apple) e PlayStore (Android)')
        ParentFont = False
      end
      object mSwHouseSite: TRLMemo
        Left = 0
        Top = 274
        Width = 286
        Height = 20
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Projeto ACBr'
          'http://www.projetoacbr.com.br')
        ParentFont = False
      end
      object pEspacoFinal: TRLPanel
        Left = 0
        Top = 294
        Width = 286
        Height = 22
        Align = faBottom
        Color = clWhite
        ParentColor = False
        Transparent = False
        object RLDraw10: TRLDraw
          Left = 0
          Top = 21
          Width = 286
          Height = 1
          Align = faBottom
          DrawKind = dkLine
          Pen.Style = psDot
        end
      end
      object RLDraw6: TRLDraw
        Left = 0
        Top = 240
        Width = 286
        Height = 10
        Align = faBottom
        DrawKind = dkLine
      end
    end
    object rlsbDetItem: TRLSubDetail
      Left = 8
      Top = 264
      Width = 286
      Height = 201
      AllowedBands = [btDetail, btSummary]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnDataRecord = rlsbDetItemDataRecord
      object rlbDetItem: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 24
        AutoSize = True
        BeforePrint = rlbDetItemBeforePrint
        object lTotalItem: TRLLabel
          Left = 242
          Top = 12
          Width = 44
          Height = 12
          Align = faRightBottom
          Alignment = taRightJustify
          Caption = '99.999,99'
          Layout = tlBottom
        end
        object lSequencia: TRLLabel
          Left = 0
          Top = 0
          Width = 18
          Height = 24
          Align = faLeft
          Caption = '001'
        end
        object mLinhaItem: TRLMemo
          Left = 18
          Top = 0
          Width = 224
          Height = 24
          Align = faClientBottom
          Behavior = [beSiteExpander]
          Lines.Strings = (
            '9999999999999 DESCRICAO DO PRODUTO 99,999 UN x 999,999 (99,99)')
        end
      end
      object rlbDescItem: TRLBand
        Left = 0
        Top = 24
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbDescItemBeforePrint
        object lTitDesItem: TRLLabel
          Left = 0
          Top = 0
          Width = 242
          Height = 12
          Align = faClientTop
          Caption = 'desconto sobre item'
        end
        object lDescItem: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbAcresItem: TRLBand
        Left = 0
        Top = 36
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbAcresItemBeforePrint
        object lTitAcrescItem: TRLLabel
          Left = 0
          Top = 0
          Width = 242
          Height = 12
          Align = faClientTop
          Caption = 'acr'#233'scimo sobre item'
        end
        object lAcrescItem: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbRatDescSubTot: TRLBand
        Left = 0
        Top = 48
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbRatDescSubTotBeforePrint
        object lTitRatDescSubtot: TRLLabel
          Left = 0
          Top = 0
          Width = 242
          Height = 12
          Align = faClientTop
          Caption = 'rateio de desconto sobre subtotal'
        end
        object lRatDescSubTot: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbRatAcresSubTot: TRLBand
        Left = 0
        Top = 60
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbRatAcresSubTotBeforePrint
        object lTitRatAcresSubtot: TRLLabel
          Left = 0
          Top = 0
          Width = 242
          Height = 12
          Align = faClientTop
          Caption = 'rateio de acr'#233'scimo sobre subtotal'
        end
        object lRatAcresSubTot: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbDeducISSQN: TRLBand
        Left = 0
        Top = 72
        Width = 286
        Height = 24
        AutoSize = True
        BeforePrint = rlbDeducISSQNBeforePrint
        object RLPanel3: TRLPanel
          Left = 222
          Top = 0
          Width = 64
          Height = 24
          Align = faRightTop
          AutoExpand = True
          AutoSize = True
          object lDeducISSQN: TRLLabel
            Left = 0
            Top = 0
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lBaseCalcISSQN: TRLLabel
            Left = 0
            Top = 12
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
        object RLPanel1: TRLPanel
          Left = 0
          Top = 0
          Width = 222
          Height = 24
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lTitBaseCalcISSQN: TRLLabel
            Left = 0
            Top = 12
            Width = 222
            Height = 12
            Align = faTop
            Caption = 'Base de c'#225'lculo ISSQN'
          end
          object lTitDeducISSQN: TRLLabel
            Left = 0
            Top = 0
            Width = 222
            Height = 12
            Align = faTop
            Caption = 'Dedu'#231#227'o para ISSQN'
          end
        end
      end
      object rlbTotalBruto: TRLBand
        Left = 0
        Top = 109
        Width = 286
        Height = 14
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbTotalBrutoBeforePrint
        object lTitTotalBruto: TRLLabel
          Left = 0
          Top = 0
          Width = 235
          Height = 14
          Align = faClientTop
          Caption = 'Total Bruto de Itens'
        end
        object lTotalBruto: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbDescontos: TRLBand
        Left = 0
        Top = 137
        Width = 286
        Height = 14
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbDescontosBeforePrint
        object lTitTotDescontos: TRLLabel
          Left = 0
          Top = 0
          Width = 235
          Height = 14
          Align = faClientTop
          Caption = 'Desconto sobre subtotal'
        end
        object lDescSubTot: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbAcrescimos: TRLBand
        Left = 0
        Top = 151
        Width = 286
        Height = 14
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbAcrescimosBeforePrint
        object lAcresSubTot: TRLLabel
          Left = 0
          Top = 0
          Width = 235
          Height = 14
          Align = faClientTop
          Caption = 'Acr'#233'scimo sobre subtotal'
        end
        object lTotAcrescimos: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbTotal: TRLBand
        Left = 0
        Top = 165
        Width = 286
        Height = 16
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        BeforePrint = rlbTotalBeforePrint
        object lTitTotal: TRLLabel
          Left = 0
          Top = 0
          Width = 225
          Height = 16
          Align = faClientTop
          Caption = 'TOTAL R$'
          Layout = tlCenter
        end
        object lTotal: TRLLabel
          Left = 225
          Top = 0
          Width = 61
          Height = 16
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Layout = tlCenter
        end
      end
      object rlbSubDescAcresItem: TRLBand
        Left = 0
        Top = 123
        Width = 286
        Height = 14
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbSubDescAcresItemBeforePrint
        object lTitTotDescAcresItem: TRLLabel
          Left = 0
          Top = 0
          Width = 235
          Height = 14
          Align = faClientTop
          Caption = 'Total de descontos / acr'#233'scimos sobre item'
        end
        object lTotDescAcresItem: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbGapDescAcres: TRLBand
        Left = 0
        Top = 96
        Width = 286
        Height = 6
        AutoExpand = False
        BeforePrint = rlbGapDescAcresBeforePrint
      end
      object rlbGapTotItens: TRLBand
        Left = 0
        Top = 102
        Width = 286
        Height = 7
        BandType = btSummary
        BeforePrint = rlbGapTotItensBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 8
      Top = 465
      Width = 286
      Height = 39
      OnDataRecord = rlsbPagamentosDataRecord
      object rlbGap1: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 6
        BandType = btHeader
      end
      object rlbPagamento: TRLBand
        Left = 0
        Top = 6
        Width = 286
        Height = 14
        AutoSize = True
        BeforePrint = rlbPagamentoBeforePrint
        object lPagamento: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
        object lMeioPagamento: TRLLabel
          Left = 0
          Top = 0
          Width = 235
          Height = 14
          Align = faClientTop
          Caption = 'Cart'#227'o de Cr'#233'dito'
        end
      end
      object rlbTroco: TRLBand
        Left = 0
        Top = 20
        Width = 286
        Height = 16
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbTrocoBeforePrint
        object lTitTroco: TRLLabel
          Left = 0
          Top = 0
          Width = 225
          Height = 16
          Align = faClientTop
          Caption = 'Troco R$'
        end
        object lTroco: TRLLabel
          Left = 225
          Top = 0
          Width = 61
          Height = 16
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
    end
    object rlsbObsFisco: TRLSubDetail
      Left = 8
      Top = 504
      Width = 286
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -8
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnDataRecord = rlsbObsFiscoDataRecord
      object rlbObsFisco: TRLBand
        Left = 0
        Top = 6
        Width = 286
        Height = 11
        AutoSize = True
        BeforePrint = rlbObsFiscoBeforePrint
        object mObsFisco: TRLMemo
          Left = 0
          Top = 0
          Width = 286
          Height = 11
          Align = faClient
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'Observa'#231#245'es do Fisco'
            'Linha 2')
          ParentFont = False
        end
      end
      object pGap1: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 6
        Align = faTop
      end
    end
    object rlDadosEntrega: TRLBand
      Left = 8
      Top = 524
      Width = 286
      Height = 18
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlDadosEntregaBeforePrint
      object pGap: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 6
        Align = faTop
      end
      object mEndEnt: TRLMemo
        Left = 0
        Top = 6
        Width = 286
        Height = 12
        Align = faTop
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'ENDERE'#199'O DE ENTREGA: <Logradouro, 99 - Bairro - Cidade>')
        ParentFont = False
      end
    end
    object rlObsContrib: TRLBand
      Left = 8
      Top = 542
      Width = 286
      Height = 66
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlObsContribBeforePrint
      object pGapObs: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 6
        Align = faTop
      end
      object lTitObsContrib: TRLMemo
        Left = 0
        Top = 6
        Width = 286
        Height = 12
        Align = faTop
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'OBSERVA'#199#213'ES DO CONTRIBUINTE')
        ParentFont = False
      end
      object mObsContrib: TRLMemo
        Left = 0
        Top = 18
        Width = 286
        Height = 12
        Align = faTop
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          '<observa'#231#245'es do contribuinte>')
        ParentFont = False
      end
      object pLei12741: TRLPanel
        Left = 0
        Top = 30
        Width = 286
        Height = 24
        Align = faTop
        AutoExpand = True
        AutoSize = True
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = pLei12741BeforePrint
        object lTitLei12741: TRLMemo
          Left = 0
          Top = 0
          Width = 242
          Height = 24
          Align = faClientTop
          Behavior = [beSiteExpander]
          Lines.Strings = (
            'Valor aproximado dos Tributos deste Cupom'
            '(Conforme Lei Fed. 12.741/2012)')
        end
        object lValLei12741: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object pAsterisco: TRLPanel
        Left = 0
        Top = 54
        Width = 286
        Height = 12
        Align = faTop
        AutoExpand = True
        AutoSize = True
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = pAsteriscoBeforePrint
        object lTitLei12743: TRLMemo
          Left = 0
          Top = 0
          Width = 286
          Height = 12
          Align = faTop
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '* Valor Aproximado dos Tributos dos Itens')
          ParentFont = False
        end
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 8
      Top = 8
      Width = 286
      Height = 256
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbNumExtrato: TRLBand
        Left = 0
        Top = 68
        Width = 286
        Height = 26
        AutoSize = True
        object pGap9: TRLPanel
          Left = 0
          Top = 0
          Width = 286
          Height = 7
          Align = faTop
        end
        object lNumeroExtrato: TRLMemo
          Left = 0
          Top = 7
          Width = 286
          Height = 12
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          Lines.Strings = (
            'EXTRATO N'#176' <NUMERO> do CUPOM FISCAL ELETR'#212'NICO - SAT')
          ParentFont = False
        end
        object pGap10: TRLPanel
          Left = 0
          Top = 19
          Width = 286
          Height = 7
          Align = faTop
        end
      end
      object rlbDadosCliche: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 68
        AutoSize = True
        BandType = btHeader
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        object lEmitCNPJ_IE_IM: TRLMemo
          Left = 4
          Top = 40
          Width = 278
          Height = 28
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlBottom
          Lines.Strings = (
            
              'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
              '3')
          ParentFont = False
        end
        object paLogoECliche: TRLPanel
          Left = 4
          Top = 0
          Width = 278
          Height = 40
          Align = faTop
          AutoExpand = True
          AutoSize = True
          object paCliche: TRLPanel
            Left = 40
            Top = 0
            Width = 238
            Height = 40
            Align = faClientTop
            AutoExpand = True
            AutoSize = True
            object lNomeFantasia: TRLMemo
              Left = 0
              Top = 0
              Width = 238
              Height = 17
              Align = faTop
              Alignment = taCenter
              Behavior = [beSiteExpander]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              Layout = tlCenter
              Lines.Strings = (
                'Nome Fantasia')
              ParentFont = False
            end
            object lRazaoSocial: TRLMemo
              Left = 0
              Top = 17
              Width = 238
              Height = 12
              Align = faTop
              Alignment = taCenter
              Behavior = [beSiteExpander]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -9
              Font.Name = 'Arial'
              Font.Style = []
              Lines.Strings = (
                'Raz'#227'o Social')
              ParentFont = False
            end
            object lEndereco: TRLMemo
              Left = 0
              Top = 29
              Width = 238
              Height = 11
              Align = faTop
              Alignment = taCenter
              Behavior = [beSiteExpander]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -9
              Font.Name = 'Arial'
              Font.Style = []
              Lines.Strings = (
                'Dados do Endere'#231'o')
              ParentFont = False
            end
          end
          object paLogo: TRLPanel
            Left = 0
            Top = 0
            Width = 40
            Height = 1
            Align = faLeftTop
            AutoExpand = True
            AutoSize = True
            object imgLogo: TRLImage
              Left = 0
              Top = 0
              Width = 40
              Height = 1
              Align = faClientTop
              AutoSize = True
              Center = True
              Scaled = True
              Transparent = False
            end
          end
        end
      end
      object rlbTeste: TRLBand
        Left = 0
        Top = 94
        Width = 286
        Height = 74
        AutoSize = True
        BeforePrint = rlbTesteBeforePrint
        object lTeste: TRLLabel
          Left = 0
          Top = 0
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Caption = '= T E S T E ='
          Layout = tlCenter
        end
        object pGap11: TRLPanel
          Left = 0
          Top = 14
          Width = 286
          Height = 9
          Align = faTop
        end
        object lFiller1: TRLLabel
          Left = 0
          Top = 23
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller2: TRLLabel
          Left = 0
          Top = 37
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller3: TRLLabel
          Left = 0
          Top = 51
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object pGap8: TRLPanel
          Left = 0
          Top = 65
          Width = 286
          Height = 9
          Align = faBottom
        end
      end
      object rlbConsumidor: TRLBand
        Left = 0
        Top = 168
        Width = 286
        Height = 52
        AutoSize = True
        BeforePrint = rlbConsumidorBeforePrint
        object lRazaoSocialNome: TRLMemo
          Left = 0
          Top = 26
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Lines.Strings = (
            'Raz'#227'o Social/Nome: <xNome>')
        end
        object lCPF_CNPJ: TRLMemo
          Left = 0
          Top = 12
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Lines.Strings = (
            'CPF/CNPJ Consumidor: <CPF_CNPJ>')
        end
        object RLDraw3: TRLDraw
          Left = 0
          Top = 0
          Width = 286
          Height = 12
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object RLDraw4: TRLDraw
          Left = 0
          Top = 40
          Width = 286
          Height = 12
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
          Transparent = False
        end
      end
      object rlbLegenda: TRLBand
        Left = 0
        Top = 220
        Width = 286
        Height = 20
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object RLDraw5: TRLDraw
          Left = 0
          Top = 12
          Width = 286
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object lCabItem: TRLMemo
          Left = 0
          Top = 0
          Width = 286
          Height = 12
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = []
          Lines.Strings = (
            '#|COD|DESC|QTD|UN| VL UN R$|(VLTR R$)*| VL ITEM R$')
          ParentFont = False
        end
      end
    end
  end
  object rlCancelamento: TRLReport
    Left = 360
    Top = 8
    Width = 302
    Height = 1134
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 2.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 2.000000000000000000
    Margins.BottomMargin = 20.000000000000000000
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 80.000000000000000000
    PageSetup.PaperHeight = 300.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlCancelamentoBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbCanRodape: TRLBand
      Left = 8
      Top = 318
      Width = 286
      Height = 589
      AutoSize = True
      BandType = btSummary
      object RLDraw8: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lDataHoraCan: TRLLabel
        Left = 0
        Top = 20
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '14/08/1971 - 08:00:00'
        Layout = tlCenter
      end
      object pNumSATCan: TRLPanel
        Left = 0
        Top = 8
        Width = 286
        Height = 12
        Align = faTop
        BeforePrint = pNumSATCanBeforePrint
        object lTitSATCan: TRLLabel
          Left = 0
          Top = 0
          Width = 136
          Height = 12
          Align = faLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'SAT N'#176':  '
          Layout = tlCenter
        end
        object lNumSATCan: TRLLabel
          Left = 136
          Top = 0
          Width = 150
          Height = 12
          Align = faClient
          Caption = '900000102'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object bcChaveAcessoCan1: TRLBarcode
        Left = 0
        Top = 46
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '3514031111111111111159'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object imgQRCodeCan: TRLImage
        Left = 0
        Top = 115
        Width = 286
        Height = 133
        Align = faTop
        Center = True
        Scaled = True
      end
      object pGap6: TRLPanel
        Left = 0
        Top = 98
        Width = 286
        Height = 17
        Align = faTop
      end
      object RLDraw9: TRLDraw
        Left = 0
        Top = 248
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object pNumSATCancl: TRLPanel
        Left = 0
        Top = 266
        Width = 286
        Height = 12
        Align = faTop
        BeforePrint = pNumSATCanclBeforePrint
        object lTitSATCanl: TRLLabel
          Left = 0
          Top = 0
          Width = 137
          Height = 12
          Align = faLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'SAT N'#176':  '
          Layout = tlCenter
        end
        object lNumSATCanl: TRLLabel
          Left = 137
          Top = 0
          Width = 149
          Height = 12
          Align = faClient
          Caption = '900000102'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object lDataHoraCanl: TRLLabel
        Left = 0
        Top = 278
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '14/08/1971 - 08:00:00'
        Layout = tlCenter
      end
      object bcChaveAcessoCanl1: TRLBarcode
        Left = 0
        Top = 304
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '3514031111111111111159'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object pGap7: TRLPanel
        Left = 0
        Top = 356
        Width = 286
        Height = 17
        Align = faTop
      end
      object imgQRCodeCanl: TRLImage
        Left = 0
        Top = 373
        Width = 286
        Height = 150
        Align = faTop
        Center = True
        Scaled = True
      end
      object bcChaveAcessoCan2: TRLBarcode
        Left = 0
        Top = 72
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '9000001020002235664805'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object bcChaveAcessoCanl2: TRLBarcode
        Left = 0
        Top = 330
        Width = 286
        Height = 26
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '9000001020002235664805'
        CheckSumMethod = cmNone
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
      object pEspacoFinalCan: TRLPanel
        Left = 0
        Top = 567
        Width = 286
        Height = 22
        Align = faBottom
        object RLDraw12: TRLDraw
          Left = 0
          Top = 21
          Width = 286
          Height = 1
          Align = faBottom
          DrawKind = dkLine
          Pen.Style = psDot
        end
      end
      object mMsgAppQRCodeCanc: TRLMemo
        Left = 0
        Top = 523
        Width = 286
        Height = 24
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Consulte o QR Code pelo aplicativo  "De olho na nota",'
          'dispon'#237'vel na AppStore (Apple) e PlayStore (Android)')
        ParentFont = False
      end
      object mSwHouseSiteCanc: TRLMemo
        Left = 0
        Top = 547
        Width = 286
        Height = 20
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Projeto ACBr'
          'http://www.projetoacbr.com.br')
        ParentFont = False
      end
      object lChaveAcessoCan: TRLMemo
        Left = 0
        Top = 34
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        Lines.Strings = (
          '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111')
        ParentFont = False
      end
      object lChaveAcessoCanl: TRLMemo
        Left = 0
        Top = 292
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        Lines.Strings = (
          '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111')
        ParentFont = False
      end
      object lTitCancelamento2: TRLMemo
        Left = 0
        Top = 256
        Width = 286
        Height = 10
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        Lines.Strings = (
          'DADOS DO CUPOM FISCAL ELETR'#212'NICO DE CANCELAMENTO')
        ParentFont = False
      end
    end
    object rlbCabecalhoCan: TRLBand
      Left = 8
      Top = 8
      Width = 286
      Height = 147
      AutoSize = True
      BandType = btHeader
      object RLDraw11: TRLDraw
        Left = 0
        Top = 97
        Width = 286
        Height = 8
        Align = faBottom
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lNumeroExtratoCan: TRLLabel
        Left = 0
        Top = 105
        Width = 286
        Height = 14
        Align = faBottom
        Alignment = taCenter
        Caption = 'Extrato N'#176': <NUMERO>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
      object lTitCancelamento: TRLLabel
        Left = 0
        Top = 133
        Width = 286
        Height = 14
        Align = faBottom
        Alignment = taCenter
        Caption = 'CANCELAMENTO'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
      object lNomeFantasiaCan: TRLMemo
        Left = 0
        Top = 1
        Width = 286
        Height = 20
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        Lines.Strings = (
          'Nome Fantasia')
        ParentFont = False
      end
      object lEnderecoCan: TRLMemo
        Left = 0
        Top = 45
        Width = 286
        Height = 24
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Endere'#231'o LINHA 1'
          'Endere'#231'o LINHA 2')
        ParentFont = False
      end
      object lRazaoSocialCan: TRLMemo
        Left = 0
        Top = 21
        Width = 286
        Height = 24
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Raz'#227'o Social Canc - Linha 1'
          'Raz'#227'o Social Canc - Linha 2')
        ParentFont = False
      end
      object lCupomFiscalEletronicoCan: TRLMemo
        Left = 0
        Top = 119
        Width = 286
        Height = 14
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        Lines.Strings = (
          'CUPOM FISCAL ELETR'#212'NICO - SAT')
        ParentFont = False
      end
      object lEmitCNPJ_IE_IMCan: TRLMemo
        Left = 0
        Top = 69
        Width = 286
        Height = 28
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Layout = tlBottom
        Lines.Strings = (
          
            'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
            '3')
      end
      object paLogoCanc: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 1
        Align = faTop
        AutoExpand = True
        AutoSize = True
        object imgLogoCanc: TRLImage
          Left = 0
          Top = 0
          Width = 286
          Height = 1
          Align = faClientTop
          AutoSize = True
          Center = True
          Scaled = True
          Transparent = False
        end
      end
    end
    object rlbDadosCupomCancelado: TRLBand
      Left = 8
      Top = 239
      Width = 286
      Height = 79
      object RLDraw13: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 10
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lRazaoSocialNomeCanc: TRLMemo
        Left = 0
        Top = 37
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Layout = tlBottom
        Lines.Strings = (
          'Raz'#227'o Social/Nome: <xNome>')
      end
      object pTotalCanc: TRLPanel
        Left = 0
        Top = 51
        Width = 286
        Height = 28
        Align = faClient
        BeforePrint = pTotalCancBeforePrint
        object lTotalCan: TRLLabel
          Left = 144
          Top = 0
          Width = 142
          Height = 28
          Align = faClient
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object lTitTotalCan: TRLLabel
          Left = 0
          Top = 0
          Width = 144
          Height = 28
          Align = faLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'TOTAL R$   '
          Layout = tlCenter
        end
      end
      object lCPF_CNPJCan: TRLMemo
        Left = 0
        Top = 22
        Width = 286
        Height = 15
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Layout = tlBottom
        Lines.Strings = (
          'CPF/CNPJ Consumidor: <CPF_CNPJ>')
      end
      object lTitCancelamento1: TRLMemo
        Left = 0
        Top = 10
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        Lines.Strings = (
          'DADOS DO CUPOM FISCAL ELETR'#212'NICO CANCELADO')
        ParentFont = False
      end
    end
    object rlbTesteCan: TRLBand
      Left = 8
      Top = 155
      Width = 286
      Height = 84
      AutoSize = True
      BandType = btHeader
      BeforePrint = rlbTesteBeforePrint
      object lFiller4: TRLLabel
        Left = 0
        Top = 42
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        Layout = tlBottom
      end
      object lFiller5: TRLLabel
        Left = 0
        Top = 56
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        Layout = tlBottom
      end
      object lFiller6: TRLLabel
        Left = 0
        Top = 70
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        Layout = tlBottom
      end
      object lTesteCan: TRLMemo
        Left = 0
        Top = 0
        Width = 286
        Height = 42
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Layout = tlCenter
        Lines.Strings = (
          ''
          '= T E S T E ='
          '')
      end
    end
  end
  object RLHTMLFilter1: TRLHTMLFilter
    DocumentStyle = dsCSS2
    DisplayName = 'ACBrBoleto - http://acbr.sf.net'
    Left = 8
    Top = 56
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#169' 1999-200' +
      '8 Fortes Inform'#225'tica'
    DisplayName = 'ACBrBoleto - http://acbr.sf.net'
    Left = 8
  end
end
