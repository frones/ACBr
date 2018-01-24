object ACBrSATExtratoFortesFr: TACBrSATExtratoFortesFr
  Left = 513
  Top = 22
  Width = 805
  Height = 920
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
    Left = 32
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
      Top = 649
      Width = 286
      Height = 331
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
        BeforePrint = pNumSATBeforePrint
        object lTitSAT: TRLLabel
          Left = 0
          Top = 0
          Width = 130
          Height = 12
          Align = faLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'SAT N'#176':   '
          Layout = tlCenter
        end
        object lNumSAT: TRLLabel
          Left = 130
          Top = 0
          Width = 156
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
      object pEspacoFinal: TRLPanel
        Left = 0
        Top = 309
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
      object mMsgAppQRCode: TRLMemo
        Left = 0
        Top = 265
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
        Top = 289
        Width = 286
        Height = 20
        Align = faBottom
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Projeto ACBr'
          'http://www.projetoacbr.com.br')
        ParentFont = False
      end
      object lChaveAcesso: TRLMemo
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
    end
    object rlsbDetItem: TRLSubDetail
      Left = 8
      Top = 289
      Width = 286
      Height = 193
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
        Height = 12
        AutoSize = True
        BeforePrint = rlbDetItemBeforePrint
        object mLinhaItem: TRLMemo
          Left = 0
          Top = 0
          Width = 286
          Height = 12
          Align = faTop
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Courier New'
          Font.Style = [fsBold]
          Lines.Strings = (
            '999999999 DESCRICAO PRODUTO 99,999 UN x 999,999 (99,99)')
          ParentFont = False
        end
      end
      object rlbDescItem: TRLBand
        Left = 0
        Top = 12
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
        Top = 36
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
        Top = 60
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
        Top = 91
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
        Top = 105
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
        Top = 119
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
        Top = 133
        Width = 286
        Height = 24
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
        Top = 84
        Width = 286
        Height = 7
        BandType = btSummary
        BeforePrint = rlbGapBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 8
      Top = 482
      Width = 286
      Height = 32
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
      Top = 514
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
      Top = 534
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
      object mEndEnt: TRLMemo
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
          '<Logradouro, 99 - Bairro - Cidade>')
        ParentFont = False
      end
      object mDestEnt: TRLMemo
        Left = 0
        Top = 34
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
          '<Nome do Destinat'#225'rio>')
        ParentFont = False
      end
    end
    object rlObsContrib: TRLBand
      Left = 8
      Top = 580
      Width = 286
      Height = 69
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlObsContribBeforePrint
      object pAsterisco: TRLPanel
        Left = 0
        Top = 57
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
          Align = faClient
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
      object RLDraw7: TRLDraw
        Left = 0
        Top = 0
        Width = 286
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Style = psDot
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
        Height = 23
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
        object lValLei12741: TRLLabel
          Left = 242
          Top = 0
          Width = 44
          Height = 23
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
        object lTitLei12741: TRLMemo
          Left = 0
          Top = 0
          Width = 242
          Height = 23
          Align = faClient
          Behavior = [beSiteExpander]
          Lines.Strings = (
            'Valor aproximado dos Tributos deste Cupom'
            '(Conforme Lei Fed. 12.741/2012)')
        end
      end
      object lTitObsContrib: TRLMemo
        Left = 0
        Top = 8
        Width = 286
        Height = 14
        Align = faTop
        Behavior = [beSiteExpander]
        Lines.Strings = (
          'OBSERVA'#199#213'ES DO CONTRIBUINTE')
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 8
      Top = 9
      Width = 286
      Height = 280
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbNumExtrato: TRLBand
        Left = 0
        Top = 101
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
        object lCupomFiscalEletronico: TRLMemo
          Left = 0
          Top = 14
          Width = 286
          Height = 14
          Align = faTop
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
      end
      object rlbDadosCliche: TRLBand
        Left = 0
        Top = 0
        Width = 286
        Height = 101
        AutoSize = True
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        object lEndereco: TRLMemo
          Left = 4
          Top = 41
          Width = 278
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
        object lRazaoSocial: TRLMemo
          Left = 4
          Top = 17
          Width = 278
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
            'Raz'#227'o Social - Linha 1'
            'Raz'#227'o Social - Linha 2')
          ParentFont = False
        end
        object lNomeFantasia: TRLMemo
          Left = 4
          Top = 0
          Width = 278
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
        object RLDraw1: TRLDraw
          Left = 4
          Top = 93
          Width = 278
          Height = 8
          Align = faBottom
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object lEmitCNPJ_IE_IM: TRLMemo
          Left = 4
          Top = 65
          Width = 278
          Height = 28
          Align = faBottom
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Layout = tlBottom
          Lines.Strings = (
            
              'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
              '3')
        end
      end
      object rlbTeste: TRLBand
        Left = 0
        Top = 129
        Width = 286
        Height = 84
        AutoSize = True
        BeforePrint = rlbTesteBeforePrint
        object lFiller1: TRLLabel
          Left = 0
          Top = 70
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller2: TRLLabel
          Left = 0
          Top = 42
          Width = 286
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          Layout = tlBottom
        end
        object lFiller3: TRLLabel
          Left = 0
          Top = 56
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
        Top = 213
        Width = 286
        Height = 40
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
          BeforePrint = lRazaoSocialNomeBeforePrint
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
          BeforePrint = lCPF_CNPJBeforePrint
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
      end
      object rlbLegenda: TRLBand
        Left = 0
        Top = 253
        Width = 286
        Height = 28
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
        object RLDraw5: TRLDraw
          Left = 0
          Top = 20
          Width = 286
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Style = psDot
        end
        object lCabItem: TRLMemo
          Left = 0
          Top = 8
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
    object rlLogo: TRLBand
      Left = 8
      Top = 8
      Width = 286
      Height = 1
      BandType = btTitle
      Visible = False
      object imgLogo: TRLImage
        Left = 0
        Top = 0
        Width = 286
        Height = 1
        Align = faTop
        AutoSize = True
        Center = True
        Scaled = True
        Transparent = False
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
      Top = 320
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
        Font.Name = 'Arial'
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
      Height = 146
      AutoSize = True
      BandType = btHeader
      object RLDraw11: TRLDraw
        Left = 0
        Top = 96
        Width = 286
        Height = 8
        Align = faBottom
        DrawKind = dkLine
        Pen.Style = psDot
      end
      object lNumeroExtratoCan: TRLLabel
        Left = 0
        Top = 104
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
        Top = 132
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
        Top = 0
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
        Top = 20
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
        Top = 44
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
        Top = 118
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
        Top = 68
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
    end
    object rlbDadosCupomCancelado: TRLBand
      Left = 8
      Top = 241
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
        BeforePrint = lRazaoSocialNomeBeforePrint
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
        BeforePrint = lCPF_CNPJBeforePrint
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
      Top = 154
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
    object rlLogoCanc: TRLBand
      Left = 8
      Top = 238
      Width = 286
      Height = 3
      BandType = btTitle
      Visible = False
      object imgLogoCanc: TRLImage
        Left = 0
        Top = 0
        Width = 286
        Height = 1
        Align = faTop
        AutoSize = True
        Center = True
        Scaled = True
        Transparent = False
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
