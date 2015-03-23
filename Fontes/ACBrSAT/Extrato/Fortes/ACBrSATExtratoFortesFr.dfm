object ACBrSATExtratoFortesFr: TACBrSATExtratoFortesFr
  Left = 622
  Top = 64
  Width = 771
  Height = 780
  Caption = 'ACBrSATExtratoFortes'
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
  object rlVenda: TRLReport
    Left = 8
    Top = -8
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
      Top = 620
      Width = 286
      Height = 265
      AutoSize = True
      BandType = btSummary
      object RLDraw2: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lDataHora: TRLLabel
        Left = 0
        Top = 20
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = '14/08/1971 - 08:00:00'
        Layout = tlCenter
      end
      object pNumSAT: TRLPanel
        Left = 0
        Top = 8
        Width = 286
        Height = 12
        Align = faTop
        object lTitSAT: TRLLabel
          Left = 92
          Top = -1
          Width = 41
          Height = 14
          Alignment = taRightJustify
          Caption = 'SAT N'#176':'
          Layout = tlCenter
        end
        object lNumSAT: TRLLabel
          Left = 136
          Top = -1
          Width = 57
          Height = 14
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
      object lChaveAcesso: TRLLabel
        Left = 0
        Top = 34
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
      end
      object bcChaveAcesso1: TRLBarcode
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
      object imgQRCode: TRLImage
        Left = 0
        Top = 115
        Width = 286
        Height = 150
        Align = faBottom
        Center = True
        Scaled = True
      end
      object pGap05: TRLPanel
        Left = 0
        Top = 98
        Width = 286
        Height = 17
        Align = faBottom
      end
      object bcChaveAcesso2: TRLBarcode
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
    end
    object rlsbDetItem: TRLSubDetail
      Left = 8
      Top = 256
      Width = 286
      Height = 172
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
          Top = 0
          Width = 44
          Height = 24
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
          Layout = tlBottom
        end
        object lSequencia: TRLLabel
          Left = 0
          Top = 0
          Width = 18
          Height = 12
          Caption = '001'
        end
        object mLinhaItem: TRLMemo
          Left = 18
          Top = 0
          Width = 214
          Height = 24
          Behavior = [beSiteExpander]
          Lines.Strings = (
            '9999999999999 DESCRICAO DO PRODUTO 99,999 UN x 999,999 (99,99)')
        end
      end
      object rlbDescItem: TRLBand
        Left = 0
        Top = 24
        Width = 286
        Height = 24
        BeforePrint = rlbDescItemBeforePrint
        object lTitDesconto: TRLLabel
          Left = 18
          Top = 1
          Width = 43
          Height = 12
          Caption = 'Desconto'
        end
        object lTitDescValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 57
          Height = 12
          Caption = 'Valor L'#237'quido'
        end
        object RLPanel1: TRLPanel
          Left = 216
          Top = 0
          Width = 70
          Height = 24
          Align = faRight
          object lDesconto: TRLLabel
            Left = 26
            Top = 0
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lDescValLiq: TRLLabel
            Left = 26
            Top = 12
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbOutroItem: TRLBand
        Left = 0
        Top = 48
        Width = 286
        Height = 24
        BeforePrint = rlbOutroItemBeforePrint
        object lTitAcrescimo: TRLLabel
          Left = 18
          Top = 1
          Width = 47
          Height = 12
          Caption = 'Acr'#233'scimo'
        end
        object lTitOutroValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 57
          Height = 12
          Caption = 'Valor L'#237'quido'
        end
        object RLPanel2: TRLPanel
          Left = 217
          Top = 0
          Width = 69
          Height = 24
          Align = faRight
          object lOutro: TRLLabel
            Left = 25
            Top = 1
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lOutroValLiq: TRLLabel
            Left = 25
            Top = 12
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbDeducISSQN: TRLBand
        Left = 0
        Top = 72
        Width = 286
        Height = 24
        BeforePrint = rlbDeducISSQNBeforePrint
        object lTitDeducISSQN: TRLLabel
          Left = 18
          Top = 1
          Width = 93
          Height = 12
          Caption = 'Dedu'#231#227'o para ISSQN'
        end
        object lTitBaseCalcISSQN: TRLLabel
          Left = 18
          Top = 13
          Width = 101
          Height = 12
          Caption = 'Base de c'#225'lculo ISSQN'
        end
        object RLPanel3: TRLPanel
          Left = 222
          Top = 0
          Width = 64
          Height = 24
          Align = faRight
          object lDeducISSQN: TRLLabel
            Left = 20
            Top = 1
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lBaseCalcISSQN: TRLLabel
            Left = 20
            Top = 12
            Width = 44
            Height = 12
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbSubTotal: TRLBand
        Left = 0
        Top = 106
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
        BeforePrint = rlbSubTotalBeforePrint
        object lTitSubTotal: TRLLabel
          Left = 0
          Top = 0
          Width = 42
          Height = 14
          Caption = 'Subtotal'
        end
        object lSubTotal: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbDescontos: TRLBand
        Left = 0
        Top = 120
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
          Width = 55
          Height = 14
          Caption = 'Descontos'
        end
        object lTotDescontos: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbAcrescimos: TRLBand
        Left = 0
        Top = 134
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
        object lTitTotAcrescimos: TRLLabel
          Left = 0
          Top = 0
          Width = 61
          Height = 14
          Caption = 'Acr'#233'scimos'
        end
        object lTotAcrescimos: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
      object rlbTotal: TRLBand
        Left = 0
        Top = 148
        Width = 286
        Height = 24
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
          Width = 67
          Height = 24
          Align = faLeft
          Caption = 'TOTAL R$'
          Layout = tlCenter
        end
        object lTotal: TRLLabel
          Left = 225
          Top = 0
          Width = 61
          Height = 24
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
          Layout = tlCenter
        end
      end
      object rlbGap: TRLBand
        Left = 0
        Top = 96
        Width = 286
        Height = 10
        BandType = btSummary
        BeforePrint = rlbGapBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 8
      Top = 428
      Width = 286
      Height = 42
      OnDataRecord = rlsbPagamentosDataRecord
      object rlbPagamento: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 14
        AutoSize = True
        BeforePrint = rlbPagamentoBeforePrint
        object lPagamento: TRLLabel
          Left = 235
          Top = 0
          Width = 51
          Height = 14
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
        object lMeioPagamento: TRLLabel
          Left = 0
          Top = 0
          Width = 87
          Height = 14
          Caption = 'Cart'#227'o de Cr'#233'dito'
        end
      end
      object rlbTroco: TRLBand
        Left = 0
        Top = 14
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
          Left = -2
          Top = 0
          Width = 56
          Height = 16
          Caption = 'Troco R$'
        end
        object lTroco: TRLLabel
          Left = 225
          Top = 0
          Width = 61
          Height = 16
          Align = faRight
          Alignment = taRightJustify
          Caption = '99.999,99'
        end
      end
    end
    object rlsbObsFisco: TRLSubDetail
      Left = 8
      Top = 470
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
        Top = 0
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
    end
    object rlDadosEntrega: TRLBand
      Left = 8
      Top = 490
      Width = 286
      Height = 46
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlDadosEntregaBeforePrint
      object RLDraw6: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lTitDadosParaEntrega: TRLLabel
        Left = 0
        Top = 8
        Width = 286
        Height = 14
        Align = faTop
        Caption = 'DADOS PARA ENTREGA'
      end
      object pEndDest: TRLPanel
        Left = 0
        Top = 22
        Width = 286
        Height = 12
        Align = faTop
        AutoExpand = True
        AutoSize = True
        object mEndEnt: TRLMemo
          Left = 58
          Top = 0
          Width = 212
          Height = 12
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '<Logradouro, 99 - Bairro - Cidade>')
          ParentFont = False
        end
        object lTitEndEnt: TRLLabel
          Left = 11
          Top = 0
          Width = 45
          Height = 12
          Alignment = taRightJustify
          Caption = 'Endere'#231'o:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
        end
      end
      object pDestEnt: TRLPanel
        Left = 0
        Top = 34
        Width = 286
        Height = 12
        Align = faTop
        AutoExpand = True
        AutoSize = True
        object lTitDestEnt: TRLLabel
          Left = 0
          Top = 0
          Width = 56
          Height = 12
          Alignment = taRightJustify
          Caption = 'Destinat'#225'rio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
        end
        object mDestEnt: TRLMemo
          Left = 58
          Top = 0
          Width = 212
          Height = 12
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '<Nome do Destinat'#225'rio>')
          ParentFont = False
        end
      end
    end
    object rlObsContrib: TRLBand
      Left = 8
      Top = 536
      Width = 286
      Height = 84
      BandType = btSummary
      BeforePrint = rlObsContribBeforePrint
      object RLDraw7: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lTitObsContrib: TRLLabel
        Left = 0
        Top = 8
        Width = 286
        Height = 14
        Align = faTop
        Caption = 'OBSERVA'#199#213'ES DO CONTRIBUINTE'
      end
      object mObsContrib: TRLMemo
        Left = 0
        Top = 22
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
        Top = 34
        Width = 286
        Height = 50
        Align = faTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = pLei12741BeforePrint
        object lTitLei12741: TRLLabel
          Left = 0
          Top = 8
          Width = 188
          Height = 12
          Caption = 'Valor aproximado dos Tributos deste Cupom'
        end
        object lValLei12741: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 32
          Align = faRight
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
        object lTitLei12742: TRLLabel
          Left = 1
          Top = 20
          Width = 142
          Height = 12
          Caption = '(Conforme Lei Fed. 12.741/2012)'
        end
        object pAsterisco: TRLPanel
          Left = 0
          Top = 32
          Width = 286
          Height = 18
          Align = faBottom
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          BeforePrint = pAsteriscoBeforePrint
          object lTitLei12743: TRLLabel
            Left = 0
            Top = 6
            Width = 286
            Height = 12
            Align = faBottom
            Caption = '* Valor Aproximado dos Tributos dos Itens'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
        end
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 8
      Top = 8
      Width = 286
      Height = 248
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbNumExtrato: TRLBand
        Left = 0
        Top = 70
        Width = 286
        Height = 28
        AutoSize = True
        object lNumeroExtrato: TRLLabel
          Left = 0
          Top = 0
          Width = 286
          Height = 14
          Align = faTop
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
        object lCupomFiscalEletronico: TRLLabel
          Left = 0
          Top = 14
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'CUPOM FISCAL ELETR'#212'NICO - SAT'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object rlbDadosCliche: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 70
        AutoSize = True
        object pCNPJ_IE_IM: TRLPanel
          Left = 0
          Top = 48
          Width = 286
          Height = 22
          Align = faBottom
          AutoSize = True
          object lEmitCNPJ_IE_IM: TRLLabel
            Left = 0
            Top = 0
            Width = 286
            Height = 14
            Align = faBottom
            Alignment = taCenter
            Caption = 
              'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
              '3'
            Layout = tlBottom
          end
          object RLDraw1: TRLDraw
            Left = 0
            Top = 14
            Width = 286
            Height = 8
            Align = faBottom
            DrawKind = dkLine
            Pen.Style = psDot
          end
        end
        object pLogoeCliche: TRLPanel
          Left = 0
          Top = 0
          Width = 286
          Height = 48
          Align = faTop
          AutoExpand = True
          AutoSize = True
          object imgLogo: TRLImage
            Left = 0
            Top = 0
            Width = 1
            Height = 48
            Align = faLeft
            AutoSize = True
            Center = True
            Scaled = True
          end
          object pCliche: TRLPanel
            Left = 1
            Top = 0
            Width = 285
            Height = 48
            Align = faClient
            AutoExpand = True
            AutoSize = True
            object lEndereco: TRLLabel
              Left = 0
              Top = 33
              Width = 285
              Height = 12
              Align = faTop
              Alignment = taCenter
              Caption = 'Endere'#231'o'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -9
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object lRazaoSocial: TRLLabel
              Left = 0
              Top = 19
              Width = 285
              Height = 14
              Align = faTop
              Alignment = taCenter
              Caption = 'Raz'#227'o Social'
            end
            object lNomeFantasia: TRLLabel
              Left = 0
              Top = 0
              Width = 285
              Height = 19
              Align = faTop
              Alignment = taCenter
              Caption = 'Nome Fantasia'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              Layout = tlCenter
              ParentFont = False
            end
          end
        end
      end
      object rlbTeste: TRLBand
        Left = 0
        Top = 98
        Width = 286
        Height = 84
        AutoSize = True
        BeforePrint = rlbTesteBeforePrint
        object lFiller1: TRLLabel
          Left = 0
          Top = 42
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller2: TRLLabel
          Left = 0
          Top = 56
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller3: TRLLabel
          Left = 0
          Top = 70
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lTeste: TRLMemo
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
      object rlbConsumidor: TRLBand
        Left = 0
        Top = 182
        Width = 286
        Height = 22
        AutoSize = True
        object RLDraw3: TRLDraw
          Left = 0
          Top = 0
          Width = 286
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object lCPF_CNPJ: TRLLabel
          Left = 0
          Top = 8
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'CPF/CNPJ Consumidor: <CPF_CNPJ>'
          Layout = tlBottom
          BeforePrint = lCPF_CNPJBeforePrint
        end
      end
      object rlbLegenda: TRLBand
        Left = 0
        Top = 204
        Width = 286
        Height = 28
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object RLDraw4: TRLDraw
          Left = 0
          Top = 0
          Width = 286
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object lCPF_CNPJ1: TRLLabel
          Left = 0
          Top = 8
          Width = 286
          Height = 12
          Align = faTop
          Alignment = taCenter
          Caption = '#|COD|DESC|QTD|UN| VL UN R$|(VLTR R$)*| VL ITEM R$'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = []
          Layout = tlBottom
          ParentFont = False
        end
        object RLDraw5: TRLDraw
          Left = 0
          Top = 20
          Width = 286
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
      end
    end
  end
  object rlCancelamento: TRLReport
    Left = 352
    Top = 0
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
      Top = 280
      Width = 286
      Height = 523
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
        object lTitSATCan: TRLLabel
          Left = 92
          Top = -1
          Width = 41
          Height = 14
          Alignment = taRightJustify
          Caption = 'SAT N'#176':'
          Layout = tlCenter
        end
        object lNumSATCan: TRLLabel
          Left = 136
          Top = -1
          Width = 57
          Height = 14
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
      object lChaveAcessoCan: TRLLabel
        Left = 0
        Top = 34
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
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
      object lTitCancelamento2: TRLLabel
        Left = 0
        Top = 256
        Width = 286
        Height = 10
        Align = faTop
        Alignment = taCenter
        Caption = 'DADOS DO CUMPOM FISCAL ELETR'#212'NICO DE CANCELAMENTO'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
      object pNumSATCancl: TRLPanel
        Left = 0
        Top = 266
        Width = 286
        Height = 12
        Align = faTop
        object lTitSATCanl: TRLLabel
          Left = 92
          Top = -1
          Width = 41
          Height = 14
          Alignment = taRightJustify
          Caption = 'SAT N'#176':'
          Layout = tlCenter
        end
        object lNumSATCanl: TRLLabel
          Left = 136
          Top = -1
          Width = 57
          Height = 14
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
      object lChaveAcessoCanl: TRLLabel
        Left = 0
        Top = 292
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = '1111  2222  3333  4444  5555  6666  7777  8888  9999  0000  1111'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
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
    end
    object rlbCabecalhoCan: TRLBand
      Left = 8
      Top = 8
      Width = 286
      Height = 112
      AutoSize = True
      BandType = btHeader
      object pLogoeClicheCan: TRLPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 48
        Align = faTop
        AutoExpand = True
        AutoSize = True
        object imgLogoCan: TRLImage
          Left = 0
          Top = 0
          Width = 1
          Height = 48
          Align = faLeft
          AutoSize = True
          Center = True
          Scaled = True
        end
        object pClicheCan: TRLPanel
          Left = 1
          Top = 0
          Width = 285
          Height = 48
          Align = faClient
          AutoExpand = True
          AutoSize = True
          object lEnderecoCan: TRLLabel
            Left = 0
            Top = 33
            Width = 285
            Height = 12
            Align = faTop
            Alignment = taCenter
            Caption = 'Endere'#231'o'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object lRazaoSocialCan: TRLLabel
            Left = 0
            Top = 19
            Width = 285
            Height = 14
            Align = faTop
            Alignment = taCenter
            Caption = 'Raz'#227'o Social'
          end
          object lNomeFantasiaCan: TRLLabel
            Left = 0
            Top = 0
            Width = 285
            Height = 19
            Align = faTop
            Alignment = taCenter
            Caption = 'Nome Fantasia'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            Layout = tlCenter
            ParentFont = False
          end
        end
      end
      object lEmitCNPJ_IE_IMCan: TRLLabel
        Left = 0
        Top = 48
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = 
          'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
          '3'
        Layout = tlBottom
      end
      object RLDraw11: TRLDraw
        Left = 0
        Top = 62
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lNumeroExtratoCan: TRLLabel
        Left = 0
        Top = 70
        Width = 286
        Height = 14
        Align = faTop
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
      object lCupomFiscalEletronicoCan: TRLLabel
        Left = 0
        Top = 84
        Width = 286
        Height = 14
        Align = faTop
        Alignment = taCenter
        Caption = 'CUPOM FISCAL ELETR'#212'NICO - SAT'
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
        Top = 98
        Width = 286
        Height = 14
        Align = faTop
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
    end
    object rlbDadosCupomCancelado: TRLBand
      Left = 8
      Top = 204
      Width = 286
      Height = 76
      object RLDraw13: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 10
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lTitCancelamento1: TRLLabel
        Left = 0
        Top = 10
        Width = 286
        Height = 11
        Align = faTop
        Alignment = taCenter
        Caption = 'DADOS DO CUMPOM FISCAL ELETR'#212'NICO CANCELADO'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
      object lCPF_CNPJCan: TRLLabel
        Left = 0
        Top = 21
        Width = 286
        Height = 15
        Align = faTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'CPF/CNPJ Consumidor: <CPF_CNPJ>'
        Layout = tlBottom
        BeforePrint = lCPF_CNPJBeforePrint
      end
      object lTitTotalCan: TRLLabel
        Left = 72
        Top = 52
        Width = 53
        Height = 14
        Caption = 'TOTAL R$'
      end
      object lTotalCan: TRLLabel
        Left = 162
        Top = 52
        Width = 51
        Height = 14
        Alignment = taRightJustify
        Caption = '99.999,99'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object rlbTesteCan: TRLBand
      Left = 8
      Top = 120
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
