object ACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr
  Left = 523
  Top = 163
  Caption = 'ACBrNFeDANFCeFortesFr'
  ClientHeight = 749
  ClientWidth = 797
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object rlVenda: TRLReport
    Left = 8
    Top = 8
    Width = 302
    Height = 1512
    Margins.LeftMargin = 1.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 1.000000000000000000
    Margins.BottomMargin = 20.000000000000000000
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 80.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlVendaBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbQRCode: TRLBand
      Left = 4
      Top = 463
      Width = 294
      Height = 137
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      object imgQRCode: TRLImage
        Left = 4
        Top = 0
        Width = 286
        Height = 137
        Align = faTop
        Center = True
        Scaled = True
      end
    end
    object rlsbDetItem: TRLSubDetail
      Left = 4
      Top = 112
      Width = 294
      Height = 104
      Margins.LeftMargin = 2.000000000000000000
      Margins.RightMargin = 2.000000000000000000
      AllowedBands = [btDetail, btSummary]
      OnDataRecord = rlsbDetItemDataRecord
      object rlbDetItem: TRLBand
        Left = 8
        Top = 0
        Width = 278
        Height = 24
        AutoSize = True
        BeforePrint = rlbDetItemBeforePrint
        object mLinhaItem: TRLMemo
          Left = 0
          Top = 0
          Width = 278
          Height = 12
          Align = faTop
          Behavior = [beSiteExpander]
          Lines.Strings = (
            '123456789012345678901234567890123456789012345678901234')
        end
        object mLinhaTotalItem: TRLMemo
          Left = 0
          Top = 12
          Width = 234
          Height = 12
          Align = faLeft
          Behavior = [beSiteExpander]
          Lines.Strings = (
            '123456789012345678901234567890123456789012345678901234')
        end
        object lTotalItem: TRLLabel
          Left = 234
          Top = 12
          Width = 44
          Height = 12
          Align = faRightBottom
          Alignment = taRightJustify
          Caption = '99.999,99'
          Layout = tlBottom
        end
      end
      object rlbDescItem: TRLBand
        Left = 8
        Top = 24
        Width = 278
        Height = 24
        AutoSize = True
        InsideMargins.LeftMargin = 3.000000000000000000
        BeforePrint = rlbDescItemBeforePrint
        object rlpDescItemTit: TRLPanel
          Left = 11
          Top = 0
          Width = 213
          Height = 24
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lTitDesconto: TRLLabel
            Left = 0
            Top = 0
            Width = 213
            Height = 12
            Align = faTop
            Caption = 'Desconto'
          end
          object lTitDescValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 213
            Height = 12
            Align = faTop
            Caption = 'Valor L'#237'quido'
          end
        end
        object rlpDescItemVal: TRLPanel
          Left = 224
          Top = 0
          Width = 54
          Height = 24
          Align = faRightTop
          AutoExpand = True
          AutoSize = True
          object lDesconto: TRLLabel
            Left = 0
            Top = 0
            Width = 54
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lDescValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 54
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbFreteItem: TRLBand
        Left = 8
        Top = 48
        Width = 278
        Height = 24
        AutoSize = True
        InsideMargins.LeftMargin = 3.000000000000000000
        BeforePrint = rlbFreteItemBeforePrint
        object rlpFreteItemTit: TRLPanel
          Left = 11
          Top = 0
          Width = 203
          Height = 24
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lTitFreteItem: TRLLabel
            Left = 0
            Top = 0
            Width = 203
            Height = 12
            Align = faTop
            Caption = 'Frete'
          end
          object lTitFreteItemValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 203
            Height = 12
            Align = faTop
            Caption = 'Valor L'#237'quido'
          end
        end
        object rlpFreteItemVal: TRLPanel
          Left = 214
          Top = 0
          Width = 64
          Height = 24
          Align = faRightTop
          AutoExpand = True
          AutoSize = True
          object lFreteItem: TRLLabel
            Left = 0
            Top = 0
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lFreteItemValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbOutroItem: TRLBand
        Left = 8
        Top = 72
        Width = 278
        Height = 24
        AutoSize = True
        InsideMargins.LeftMargin = 3.000000000000000000
        BeforePrint = rlbOutroItemBeforePrint
        object rlpAcresItemTit: TRLPanel
          Left = 11
          Top = 0
          Width = 203
          Height = 24
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lTitAcrescimo: TRLLabel
            Left = 0
            Top = 0
            Width = 203
            Height = 12
            Align = faTop
            Caption = 'Acr'#233'scimo'
          end
          object lTitOutroValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 203
            Height = 12
            Align = faTop
            Caption = 'Valor L'#237'quido'
          end
        end
        object rlpAcresItemVal: TRLPanel
          Left = 214
          Top = 0
          Width = 64
          Height = 24
          Align = faRightTop
          AutoExpand = True
          AutoSize = True
          object lOutro: TRLLabel
            Left = 0
            Top = 0
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
          object lOutroValLiq: TRLLabel
            Left = 0
            Top = 12
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
          end
        end
      end
      object rlbGap: TRLBand
        Left = 8
        Top = 96
        Width = 278
        Height = 2
        BandType = btSummary
        BeforePrint = rlbGapBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 4
      Top = 216
      Width = 294
      Height = 104
      Margins.LeftMargin = 2.000000000000000000
      Margins.RightMargin = 2.000000000000000000
      OnDataRecord = rlsbPagamentosDataRecord
      object rlbTotal: TRLBand
        Left = 8
        Top = 0
        Width = 278
        Height = 24
        BandType = btHeader
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        BeforePrint = rlbTotalBeforePrint
        object rlpTotalVal: TRLPanel
          Left = 214
          Top = 0
          Width = 64
          Height = 23
          Align = faRightTop
          AutoExpand = True
          AutoSize = True
          object lQtdTotalItensVal: TRLLabel
            Left = 0
            Top = 0
            Width = 64
            Height = 12
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lTotal: TRLLabel
            Left = 0
            Top = 12
            Width = 64
            Height = 11
            Align = faTop
            Alignment = taRightJustify
            Caption = '99.999,99'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object rlpTotTit: TRLPanel
          Left = 0
          Top = 0
          Width = 214
          Height = 24
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lQtdItens: TRLLabel
            Left = 0
            Top = 0
            Width = 214
            Height = 12
            Align = faTop
            Caption = 'QTD. TOTAL DE ITENS'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lTitTotal: TRLLabel
            Left = 0
            Top = 12
            Width = 214
            Height = 12
            Align = faTop
            Caption = 'VALOR TOTAL R$'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object rlbTotalDesconto: TRLBand
        Left = 8
        Top = 24
        Width = 278
        Height = 12
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalDescontoBeforePrint
        object lTitTotalDesconto: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 12
          Align = faClientTop
          Caption = 'Descontos R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTotalDesconto: TRLLabel
          Left = 234
          Top = 0
          Width = 44
          Height = 11
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbTotalFrete: TRLBand
        Left = 8
        Top = 36
        Width = 278
        Height = 12
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalFreteBeforePrint
        object lTitTotalFrete: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 12
          Align = faClientTop
          Caption = 'Frete R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTotalFrete: TRLLabel
          Left = 234
          Top = 0
          Width = 44
          Height = 11
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbTotalAcrescimo: TRLBand
        Left = 8
        Top = 48
        Width = 278
        Height = 12
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalAcrescimoBeforePrint
        object lTitTotalAcrescimo: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 12
          Align = faClientTop
          Caption = 'Acr'#233'scimos R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTotalAcrescimo: TRLLabel
          Left = 234
          Top = 0
          Width = 44
          Height = 11
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbTotalAPagar: TRLBand
        Left = 8
        Top = 60
        Width = 278
        Height = 11
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalAPagarBeforePrint
        object lTitTotalAPagar: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 11
          Align = faClientTop
          Caption = 'VALOR A PAGAR R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTotalAPagar: TRLLabel
          Left = 234
          Top = 0
          Width = 44
          Height = 11
          Align = faRightTop
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbPagamentoTitulo: TRLBand
        Left = 8
        Top = 71
        Width = 278
        Height = 12
        AutoSize = True
        BandType = btHeader
        object lTitFormaPagto: TRLLabel
          Left = 0
          Top = 0
          Width = 230
          Height = 12
          Align = faClientTop
          Caption = 'FORMA DE PAGAMENTO'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTitValorPago: TRLLabel
          Left = 230
          Top = 0
          Width = 48
          Height = 12
          Align = faRightTop
          Alignment = taRightJustify
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbPagamento: TRLBand
        Left = 8
        Top = 83
        Width = 278
        Height = 12
        AutoSize = True
        BeforePrint = rlbPagamentoBeforePrint
        object lMeioPagamento: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 12
          Align = faClientTop
          Caption = 'Cart'#227'o de Cr'#233'dito'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lPagamento: TRLLabel
          Left = 234
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
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbTroco: TRLBand
        Left = 8
        Top = 95
        Width = 278
        Height = 12
        AutoSize = True
        BandType = btFooter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbTrocoBeforePrint
        object lTitTroco: TRLLabel
          Left = 0
          Top = 0
          Width = 234
          Height = 12
          Align = faClientTop
          Caption = 'Troco R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTroco: TRLLabel
          Left = 234
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
          Font.Style = []
          ParentFont = False
        end
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 4
      Top = 8
      Width = 294
      Height = 104
      Margins.LeftMargin = 2.000000000000000000
      Margins.RightMargin = 2.000000000000000000
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbMsgDANFe: TRLBand
        Left = 8
        Top = 40
        Width = 278
        Height = 8
        AutoSize = True
        object lMsgDANFCe: TRLLabel
          Left = 0
          Top = 0
          Width = 278
          Height = 8
          Align = faTop
          Alignment = taCenter
          Caption = 'DOCUMENTO AUXILIAR DA NOTA FISCAL DE CONSUMIDOR ELETR'#212'NICA'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -7
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object rlbDadosCliche: TRLBand
        Left = 8
        Top = 0
        Width = 278
        Height = 40
        AutoSize = True
        object pCliche: TRLPanel
          Left = 0
          Top = 0
          Width = 278
          Height = 40
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lEndereco: TRLMemo
            Left = 0
            Top = 28
            Width = 278
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
              'Endere'#231'o')
            ParentFont = False
          end
          object lNomeFantasia: TRLMemo
            Left = 0
            Top = 0
            Width = 278
            Height = 16
            Align = faTop
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            Layout = tlCenter
            Lines.Strings = (
              'Nome Fantasia')
            ParentFont = False
          end
          object lRazaoSocial: TRLMemo
            Left = 0
            Top = 16
            Width = 278
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
        end
        object pLogo: TRLPanel
          Left = 0
          Top = 38
          Width = 64
          Height = 2
          Align = faLeftBottom
          Alignment = taCenter
          AutoExpand = True
          AutoSize = True
          Behavior = [beSiteExpander]
          Layout = tlCenter
          object imgLogo: TRLImage
            Left = 0
            Top = 1
            Width = 64
            Height = 1
            Align = faClientBottom
            AutoSize = True
            Behavior = [beSiteExpander]
            Center = True
            Scaled = True
            Transparent = False
          end
        end
      end
      object rlbLegenda: TRLBand
        Left = 8
        Top = 70
        Width = 278
        Height = 12
        AutoSize = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        BeforePrint = rlbLegendaBeforePrint
        object lLegendaItens: TRLLabel
          Left = 0
          Top = 0
          Width = 278
          Height = 12
          Align = faClientBottom
          Alignment = taRightJustify
          AutoSize = False
          Caption = '  #|C'#243'digo|Descri'#231#227'o|Qtde|Un|Valor unit.|Valor total'
          Layout = tlBottom
        end
      end
      object rlbMsgContingencia: TRLBand
        Left = 8
        Top = 48
        Width = 278
        Height = 22
        AutoSize = True
        Color = clSilver
        ParentColor = False
        Transparent = False
        BeforePrint = rlbMsgContingenciaBeforePrint
        object lMsgContingencia: TRLMemo
          Left = 0
          Top = 0
          Width = 278
          Height = 22
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          Layout = tlCenter
          Lines.Strings = (
            'EMITIDA EM CONTING'#202'NCIA'
            'Pendente de autoriza'#231#227'o')
          ParentFont = False
          Transparent = False
        end
      end
    end
    object rlbChaveDeAcesso: TRLBand
      Left = 4
      Top = 320
      Width = 294
      Height = 39
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      BeforePrint = rlbChaveDeAcessoBeforePrint
      object lTitConsulteChave: TRLMemo
        Left = 8
        Top = 0
        Width = 278
        Height = 11
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Consulte pela Chave de Acesso em')
        ParentFont = False
      end
      object lURLConsulta: TRLMemo
        Left = 8
        Top = 11
        Width = 278
        Height = 16
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'URL CONSULTA')
        ParentFont = False
      end
      object lChaveDeAcesso: TRLMemo
        Left = 8
        Top = 27
        Width = 278
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
          '9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999')
        ParentFont = False
      end
    end
    object rlbConsumidor: TRLBand
      Left = 4
      Top = 359
      Width = 294
      Height = 81
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      BeforePrint = rlbChaveDeAcessoBeforePrint
      object lProtocolo: TRLLabel
        Left = 8
        Top = 46
        Width = 278
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'Protocolo de Autoriza'#231#227'o: 999999999999999 '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
      end
      object lNumeroSerie: TRLLabel
        Left = 8
        Top = 35
        Width = 278
        Height = 11
        Align = faTop
        Alignment = taCenter
        Caption = 'N'#250'mero 999999999 S'#233'rie 999'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lDataAutorizacao: TRLLabel
        Left = 8
        Top = 58
        Width = 278
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'Data de Autoriza'#231'ao DD/MM/AAAA HH:MM:SS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
      end
      object lConsumidor: TRLMemo
        Left = 8
        Top = 0
        Width = 278
        Height = 11
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Lines.Strings = (
          'CNPJ/CPF/ID Estrangeiro - CCCCCCCCCCCCCCCCCCCC')
        ParentFont = False
        Transparent = False
      end
      object lEnderecoConsumidor: TRLMemo
        Left = 8
        Top = 23
        Width = 278
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
          'Endere'#231'o Consumidor (Logradouro, n'#186', bairro, Munic'#237#173'pio)')
        ParentFont = False
      end
      object lNomeConsumidor: TRLMemo
        Left = 8
        Top = 11
        Width = 278
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
          'Nome do Consumidor')
        ParentFont = False
      end
      object lContingencia: TRLMemo
        Left = 8
        Top = 70
        Width = 278
        Height = 11
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object rlbMensagemFiscal: TRLBand
      Left = 4
      Top = 440
      Width = 294
      Height = 23
      Margins.LeftMargin = 2.000000000000000000
      Margins.RightMargin = 2.000000000000000000
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbChaveDeAcessoBeforePrint
      object lCancelada: TRLLabel
        Left = 8
        Top = 0
        Width = 278
        Height = 11
        Align = faTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lMensagemFiscal: TRLMemo
        Left = 8
        Top = 11
        Width = 278
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
      end
    end
    object rlbRodape: TRLBand
      Left = 4
      Top = 720
      Width = 294
      Height = 72
      Margins.LeftMargin = 2.000000000000000000
      Margins.RightMargin = 2.000000000000000000
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      BeforePrint = rlbRodapeBeforePrint
      object pGap6: TRLPanel
        Left = 8
        Top = 0
        Width = 278
        Height = 34
        Align = faTop
        AutoExpand = True
        AutoSize = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        object lSistema: TRLLabel
          Left = 0
          Top = 24
          Width = 278
          Height = 10
          Align = faTop
          Alignment = taRightJustify
          Caption = 'Projeto ACBr  '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          Layout = tlBottom
          ParentFont = False
          BeforePrint = lSistemaBeforePrint
        end
        object lObservacoes: TRLMemo
          Left = 0
          Top = 0
          Width = 278
          Height = 12
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTitLei12741: TRLMemo
          Left = 0
          Top = 12
          Width = 278
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
            'Tributos Incidentes Lei Federal 12.741/12 - Total R$')
          ParentFont = False
        end
      end
      object pGapEspacofinalVenda: TRLPanel
        Left = 8
        Top = 34
        Width = 278
        Height = 38
        Align = faTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlbQRLateral: TRLBand
      Left = 4
      Top = 600
      Width = 294
      Height = 120
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      AutoSize = True
      BandType = btSummary
      Computable = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      InsideMargins.RightMargin = 1.000000000000000000
      ParentFont = False
      BeforePrint = rlbQRLateralBeforePrint
      object rlpDadosQRCodeLateral: TRLPanel
        Left = 148
        Top = 0
        Width = 138
        Height = 120
        Align = faClient
        AutoExpand = True
        AutoSize = True
        object lConsumidor1: TRLMemo
          Left = 0
          Top = 0
          Width = 138
          Height = 20
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Lines.Strings = (
            'CNPJ/CPF/ID Estrangeiro - CCCCCCCCCCCCCCCCCCCC')
          ParentFont = False
          Transparent = False
        end
        object lNomeConsumidor1: TRLMemo
          Left = 0
          Top = 20
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Nome do Consumidor')
          ParentFont = False
        end
        object lEnderecoConsumidor1: TRLMemo
          Left = 0
          Top = 30
          Width = 138
          Height = 20
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            'Endere'#231'o Consumidor (Logradouro, n'#186', bairro, Munic'#237#173'pio)')
          ParentFont = False
        end
        object lNumeroSerie1: TRLMemo
          Left = 0
          Top = 50
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Numero NFC-e')
          ParentFont = False
        end
        object lProtocolo1: TRLMemo
          Left = 0
          Top = 60
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlBottom
          ParentFont = False
        end
        object lDataAutorizacao1: TRLMemo
          Left = 0
          Top = 70
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlBottom
          ParentFont = False
        end
        object lMensagemFiscal1: TRLMemo
          Left = 0
          Top = 90
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lContingencia1: TRLMemo
          Left = 0
          Top = 80
          Width = 138
          Height = 10
          Align = faTop
          Alignment = taCenter
          Behavior = [beSiteExpander]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Emitida em Conting'#234'ncia')
          ParentFont = False
        end
      end
      object rlpImgQRCodeLateral: TRLPanel
        Left = 4
        Top = 0
        Width = 144
        Height = 120
        Align = faLeft
        object imgQRCodeLateral: TRLImage
          Left = 0
          Top = 0
          Width = 144
          Height = 120
          Align = faCenter
          Center = True
          Scaled = True
        end
      end
    end
  end
  object rlCancelamento: TRLReport
    Left = 473
    Top = 4
    Width = 280
    Height = 1512
    Margins.LeftMargin = 1.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 1.000000000000000000
    Margins.BottomMargin = 0.000000000000000000
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    InsideMargins.LeftMargin = 1.000000000000000000
    InsideMargins.RightMargin = 1.000000000000000000
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 74.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlCancelamentoBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbRodapeCanc: TRLBand
      Left = 8
      Top = 357
      Width = 264
      Height = 222
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      ParentFont = False
      object RLDraw9: TRLDraw
        Left = 4
        Top = 0
        Width = 256
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lConsultaQRCodeCanc: TRLLabel
        Left = 4
        Top = 8
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'Consulta via leitor de QR Code'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlBottom
        ParentFont = False
      end
      object imgQRCodeCanc: TRLImage
        Left = 4
        Top = 20
        Width = 256
        Height = 137
        Align = faTop
        Center = True
        Scaled = True
      end
      object RLPanel1: TRLPanel
        Left = 4
        Top = 167
        Width = 256
        Height = 17
        Align = faTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        object lSistemaCanc: TRLLabel
          Left = 0
          Top = 0
          Width = 256
          Height = 10
          Align = faTop
          Alignment = taRightJustify
          Caption = 'Projeto ACBr'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          Layout = tlBottom
          ParentFont = False
          BeforePrint = lSistemaBeforePrint
        end
      end
      object lProtocoloCanc: TRLLabel
        Left = 4
        Top = 157
        Width = 256
        Height = 10
        Align = faTop
        Alignment = taCenter
        Caption = 'Protocolo de Autoriza'#231#227'o: 999999999999999 DD/MM/AAAA HH:MM:SS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlBottom
        ParentFont = False
      end
      object pGapEspacofinalCancelamento: TRLPanel
        Left = 4
        Top = 184
        Width = 256
        Height = 38
        Align = faTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object RLSubDetail3: TRLSubDetail
      Left = 8
      Top = 8
      Width = 264
      Height = 154
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      OnDataRecord = rlbsCabecalhoDataRecord
      object RLBand10: TRLBand
        Left = 4
        Top = 63
        Width = 256
        Height = 42
        AutoSize = True
        object RLLabel26: TRLLabel
          Left = 0
          Top = 0
          Width = 256
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'DANFE NFC-e - Documento Auxiliar'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object RLLabel27: TRLLabel
          Left = 0
          Top = 28
          Width = 256
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'DOCUMENTO CANCELADO'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object RLLabel28: TRLLabel
          Left = 0
          Top = 14
          Width = 256
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'da Nota Fiscal Eletr'#244'nica para Consumidor Final'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object RLBand11: TRLBand
        Left = 4
        Top = 0
        Width = 256
        Height = 63
        AutoSize = True
        object RLPanel3: TRLPanel
          Left = 0
          Top = 0
          Width = 256
          Height = 63
          Align = faTop
          AutoExpand = True
          AutoSize = True
          object lEmitCNPJ_IE_IM_Camc: TRLLabel
            Left = 0
            Top = 31
            Width = 256
            Height = 12
            Align = faTop
            Alignment = taCenter
            Caption = 
              'CNPJ: 22.222.222/22222-22  IE:223.233.344.233 IM:2323.222.333.23' +
              '3'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = []
            Layout = tlBottom
            ParentFont = False
          end
          object lEnderecoCanc: TRLMemo
            Left = 0
            Top = 43
            Width = 256
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
              'Endere'#231'o')
            ParentFont = False
          end
          object RLDraw14: TRLDraw
            Left = 0
            Top = 55
            Width = 256
            Height = 8
            Align = faTop
            DrawKind = dkLine
            Pen.Width = 2
          end
          object RLImage2: TRLImage
            Left = 0
            Top = 0
            Width = 256
            Height = 1
            Align = faTop
            AutoSize = True
            Center = True
            Scaled = True
            Transparent = False
          end
          object lNomeFantasiaCanc: TRLMemo
            Left = 0
            Top = 1
            Width = 256
            Height = 18
            Align = faTop
            Alignment = taCenter
            Behavior = [beSiteExpander]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -15
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            Layout = tlCenter
            Lines.Strings = (
              'Nome Fantasia')
            ParentFont = False
          end
          object lRazaoSocialCanc: TRLMemo
            Left = 0
            Top = 19
            Width = 256
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
        end
      end
      object RLBand12: TRLBand
        Left = 4
        Top = 105
        Width = 256
        Height = 8
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object RLDraw15: TRLDraw
          Left = 0
          Top = 0
          Width = 256
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Width = 2
        end
      end
    end
    object rlbConsumidorCanc: TRLBand
      Left = 8
      Top = 192
      Width = 264
      Height = 55
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      BeforePrint = rlbConsumidorCancBeforePrint
      object RLDraw17: TRLDraw
        Left = 4
        Top = 0
        Width = 256
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lTitConsumidorCanc: TRLLabel
        Left = 4
        Top = 8
        Width = 256
        Height = 11
        Align = faTop
        Alignment = taCenter
        Caption = 'CONSUMIDOR'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lEnderecoConsumidorCanc: TRLMemo
        Left = 4
        Top = 43
        Width = 256
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
          'Endere'#231'o Consumidor (Logradouro, n'#186', bairro, Munic'#237#173'pio)')
        ParentFont = False
      end
      object lCPF_CNPJ_ID_Canc: TRLMemo
        Left = 4
        Top = 19
        Width = 256
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
          
            'CNPJ/CPF/ID Estrangeiro - CCCCCCCCCCCCCCCCCCCC NOME DO CONSUMIDO' +
            'R')
        ParentFont = False
      end
    end
    object rlbMensagemFiscalCanc: TRLBand
      Left = 8
      Top = 247
      Width = 264
      Height = 110
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      BeforePrint = rlbMensagemFiscalCancBeforePrint
      object RLDraw18: TRLDraw
        Left = 4
        Top = 0
        Width = 256
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lMensagemFiscalCanc: TRLLabel
        Left = 4
        Top = 8
        Width = 256
        Height = 10
        Align = faTop
        Alignment = taCenter
        Caption = #195#129'REA DE MENSAGEM FISCAL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lChaveDeAcessoCanc: TRLLabel
        Left = 4
        Top = 74
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = '9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lTitChaveAcessoCanc: TRLLabel
        Left = 4
        Top = 62
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'CHAVE DE ACESSO'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lNumeroSerieCanc: TRLLabel
        Left = 4
        Top = 18
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'N'#250'mero 999999999 S'#233'rie 999'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lTitConsulteChaveCanc: TRLMemo
        Left = 4
        Top = 50
        Width = 256
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
          'Consulte pela Chave de Acesso em www.')
        ParentFont = False
      end
      object lEmissaoViaCanc: TRLLabel
        Left = 4
        Top = 30
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Caption = 'Emiss'#227'o DD/MM/AAAA HH:MM:SS  - Via Estabelecimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw19: TRLDraw
        Left = 4
        Top = 42
        Width = 256
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lCanceladaCanc: TRLLabel
        Left = 4
        Top = 86
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rllFisco: TRLLabel
        Left = 4
        Top = 98
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlbMensagemContribuinteCanc: TRLBand
      Left = 8
      Top = 162
      Width = 264
      Height = 30
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      BeforePrint = rlbMensagemContribuinteCancBeforePrint
      object lMensagemContribuinteCamc: TRLLabel
        Left = 4
        Top = 8
        Width = 256
        Height = 10
        Align = faTop
        Alignment = taCenter
        Caption = 'AREA DE MENSAGEM DE INTERESSE DO CONTRIBUINTE'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -8
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLDraw20: TRLDraw
        Left = 4
        Top = 0
        Width = 256
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lObservacoesCanc: TRLMemo
        Left = 4
        Top = 18
        Width = 256
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
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
    DisplayName = 'HTML'
    Left = 400
    Top = 131
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 'Projeto ACBr'
    DisplayName = 'Documento PDF'
    Left = 370
    Top = 430
  end
end
