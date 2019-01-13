object ACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr
  Left = 511
  Top = 148
  Width = 796
  Height = 926
  Caption = 'ACBrNFeDANFCeFortesFr'
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
    Top = 8
    Width = 302
    Height = 1512
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 1.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 1.000000000000000000
    Margins.BottomMargin = 20.000000000000000000
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 80.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlVendaBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbQRCode: TRLBand
      Left = 4
      Top = 413
      Width = 294
      Height = 137
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
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
      Height = 62
      AllowedBands = [btDetail, btSummary]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      ParentFont = False
      OnDataRecord = rlsbDetItemDataRecord
      object rlbDetItem: TRLBand
        Left = 4
        Top = 0
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbDetItemBeforePrint
        object mLinhaItem: TRLMemo
          Left = 8
          Top = 0
          Width = 274
          Height = 12
          Anchors = [fkLeft, fkRight]
          Behavior = [beSiteExpander]
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Courier New'
          Font.Style = [fsBold]
          Lines.Strings = (
            '123456789012345678901234567890123456789012345678901234')
          ParentFont = False
        end
      end
      object rlbDescItem: TRLBand
        Left = 4
        Top = 12
        Width = 286
        Height = 24
        BeforePrint = rlbDescItemBeforePrint
        object lTitDesconto: TRLLabel
          Left = 18
          Top = 1
          Width = 38
          Height = 10
          Caption = 'Desconto'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTitDescValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 52
          Height = 10
          Caption = 'Valor L'#237'quido'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lDesconto: TRLLabel
          Left = 233
          Top = 1
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lDescValLiq: TRLLabel
          Left = 233
          Top = 13
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbOutroItem: TRLBand
        Left = 4
        Top = 36
        Width = 286
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        BeforePrint = rlbOutroItemBeforePrint
        object lTitAcrescimo: TRLLabel
          Left = 18
          Top = 1
          Width = 40
          Height = 10
          Caption = 'Acr'#233'scimo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTitOutroValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 52
          Height = 10
          Caption = 'Valor L'#237'quido'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lOutro: TRLLabel
          Left = 233
          Top = 1
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lOutroValLiq: TRLLabel
          Left = 233
          Top = 13
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbGap: TRLBand
        Left = 4
        Top = 60
        Width = 286
        Height = 2
        BandType = btSummary
        BeforePrint = rlbGapBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 4
      Top = 174
      Width = 294
      Height = 96
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      OnDataRecord = rlsbPagamentosDataRecord
      object rlbPagamento: TRLBand
        Left = 4
        Top = 71
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbPagamentoBeforePrint
        object lPagamento: TRLLabel
          Left = 231
          Top = 0
          Width = 44
          Height = 12
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lMeioPagamento: TRLLabel
          Left = 8
          Top = 0
          Width = 77
          Height = 12
          Caption = 'Cart'#227'o de Cr'#233'dito'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbTroco: TRLBand
        Left = 4
        Top = 83
        Width = 286
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
          Left = 8
          Top = 0
          Width = 41
          Height = 12
          Caption = 'Troco R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTroco: TRLLabel
          Left = 231
          Top = 0
          Width = 44
          Height = 12
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbTotal: TRLBand
        Left = 4
        Top = 0
        Width = 286
        Height = 24
        AutoSize = True
        BandType = btHeader
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        BeforePrint = rlbTotalBeforePrint
        object lTitTotal: TRLLabel
          Left = 8
          Top = 12
          Width = 80
          Height = 12
          Caption = 'VALOR TOTAL R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTotal: TRLLabel
          Left = 231
          Top = 12
          Width = 44
          Height = 11
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lQtdItens: TRLLabel
          Left = 8
          Top = 0
          Width = 102
          Height = 12
          Caption = 'QTD. TOTAL DE ITENS'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lQtdTotalItensVal: TRLLabel
          Left = 244
          Top = 0
          Width = 31
          Height = 12
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbPagamentoTitulo: TRLBand
        Left = 4
        Top = 59
        Width = 286
        Height = 12
        AutoSize = True
        BandType = btHeader
        object lTitFormaPagto: TRLLabel
          Left = 8
          Top = 0
          Width = 112
          Height = 12
          Caption = 'FORMA DE PAGAMENTO'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTitValorPago: TRLLabel
          Left = 227
          Top = 0
          Width = 48
          Height = 12
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbTotalAcrescimo: TRLBand
        Left = 4
        Top = 24
        Width = 286
        Height = 12
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalAcrescimoBeforePrint
        object lTitTotalAcrescimo: TRLLabel
          Left = 8
          Top = 0
          Width = 67
          Height = 12
          Caption = 'Acr'#233'scimos R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTotalAcrescimo: TRLLabel
          Left = 231
          Top = 0
          Width = 44
          Height = 11
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object rlbTotalDesconto: TRLBand
        Left = 4
        Top = 36
        Width = 286
        Height = 12
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalDescontoBeforePrint
        object lTitTotalDesconto: TRLLabel
          Left = 8
          Top = 0
          Width = 63
          Height = 12
          Caption = 'Descontos R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTotalDesconto: TRLLabel
          Left = 231
          Top = 0
          Width = 44
          Height = 11
          Alignment = taRightJustify
          Anchors = [fkRight]
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
        Left = 4
        Top = 48
        Width = 286
        Height = 11
        AutoSize = True
        BandType = btHeader
        BeforePrint = rlbTotalAPagarBeforePrint
        object lTitTotalAPagar: TRLLabel
          Left = 8
          Top = 0
          Width = 90
          Height = 11
          Caption = 'VALOR A PAGAR R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTotalAPagar: TRLLabel
          Left = 231
          Top = 0
          Width = 44
          Height = 11
          Alignment = taRightJustify
          Anchors = [fkRight]
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 4
      Top = 8
      Width = 294
      Height = 104
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbMsgDANFe: TRLBand
        Left = 4
        Top = 40
        Width = 286
        Height = 8
        AutoSize = True
        object lMsgDANFCe: TRLLabel
          Left = 0
          Top = 0
          Width = 286
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
        Left = 4
        Top = 0
        Width = 286
        Height = 40
        AutoSize = True
        object pCliche: TRLPanel
          Left = 64
          Top = 0
          Width = 222
          Height = 40
          Align = faClientTop
          AutoExpand = True
          AutoSize = True
          object lEndereco: TRLMemo
            Left = 0
            Top = 28
            Width = 222
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
            Width = 222
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
            Width = 222
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
          Top = 0
          Width = 64
          Height = 1
          Align = faLeftTop
          Alignment = taCenter
          AutoExpand = True
          AutoSize = True
          object imgLogo: TRLImage
            Left = 0
            Top = 0
            Width = 64
            Height = 1
            Align = faClientTop
            AutoSize = True
            Center = True
            Scaled = True
            Transparent = False
          end
        end
      end
      object rlbLegenda: TRLBand
        Left = 4
        Top = 70
        Width = 286
        Height = 12
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object lLegendaItens: TRLLabel
          Left = 8
          Top = 0
          Width = 270
          Height = 12
          Anchors = [fkLeft, fkRight]
          Caption = 
            '  #|C'#243'digo |Descri'#231#227'o                |Qtde|Un|Valor unit.|Valor ' +
            'total'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlBottom
          ParentFont = False
        end
      end
      object rlbMsgContingencia: TRLBand
        Left = 4
        Top = 48
        Width = 286
        Height = 22
        AutoSize = True
        Color = clSilver
        ParentColor = False
        Transparent = False
        BeforePrint = rlbMsgContingenciaBeforePrint
        object lMsgContingencia: TRLMemo
          Left = 0
          Top = 0
          Width = 286
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
      Top = 270
      Width = 294
      Height = 39
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
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
      Top = 309
      Width = 294
      Height = 81
      AutoSize = True
      BandType = btSummary
      InsideMargins.LeftMargin = 1.000000000000000000
      InsideMargins.RightMargin = 1.000000000000000000
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
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
      Top = 390
      Width = 294
      Height = 23
      AutoSize = True
      BandType = btSummary
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      BeforePrint = rlbChaveDeAcessoBeforePrint
      object lCancelada: TRLLabel
        Left = 4
        Top = 0
        Width = 286
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
        Left = 4
        Top = 11
        Width = 286
        Height = 12
        Align = faTop
        Alignment = taCenter
        Behavior = [beSiteExpander]
      end
    end
    object rlbRodape: TRLBand
      Left = 4
      Top = 670
      Width = 294
      Height = 72
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
      ParentFont = False
      BeforePrint = rlbRodapeBeforePrint
      object pGap6: TRLPanel
        Left = 4
        Top = 0
        Width = 286
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
          Width = 286
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
          ParentFont = False
        end
        object lTitLei12741: TRLMemo
          Left = 0
          Top = 12
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
          Lines.Strings = (
            'Tributos Incidentes Lei Federal 12.741/12 - Total R$')
          ParentFont = False
        end
      end
      object pGapEspacofinalVenda: TRLPanel
        Left = 4
        Top = 34
        Width = 286
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
      Top = 550
      Width = 294
      Height = 120
      AutoSize = True
      BandType = btSummary
      Computable = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      InsideMargins.RightMargin = 1.000000000000000000
      Margins.LeftMargin = 1.000000000000000000
      Margins.RightMargin = 1.000000000000000000
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
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    InsideMargins.LeftMargin = 1.000000000000000000
    InsideMargins.RightMargin = 1.000000000000000000
    Margins.LeftMargin = 1.000000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 1.000000000000000000
    Margins.BottomMargin = 0.000000000000000000
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 74.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlCancelamentoBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbRodapeCanc: TRLBand
      Left = 8
      Top = 345
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
      Height = 98
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
