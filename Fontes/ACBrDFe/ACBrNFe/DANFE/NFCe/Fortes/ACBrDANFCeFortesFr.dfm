object ACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr
  Left = 461
  Top = 84
  Caption = 'ACBrNFeDANFCeFortesFr'
  ClientHeight = 741
  ClientWidth = 772
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
    Top = 4
    Width = 280
    Height = 1512
    Margins.LeftMargin = 0.610000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 0.610000000000000000
    Margins.BottomMargin = 0.000000000000000000
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 74.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlVendaBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbRodape: TRLBand
      Left = 2
      Top = 518
      Width = 276
      Height = 222
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      object RLDraw2: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lConsultaQRCode: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
      object imgQRCode: TRLImage
        Left = 0
        Top = 20
        Width = 276
        Height = 137
        Align = faTop
        Center = True
        Scaled = True
      end
      object pGap05: TRLPanel
        Left = 0
        Top = 167
        Width = 276
        Height = 17
        Align = faTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        object lSistema: TRLLabel
          Left = 0
          Top = 0
          Width = 276
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
      object lProtocolo: TRLLabel
        Left = 0
        Top = 157
        Width = 276
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
      object pGap8: TRLPanel
        Left = 0
        Top = 184
        Width = 276
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
    object rlsbDetItem: TRLSubDetail
      Left = 2
      Top = 162
      Width = 276
      Height = 80
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
        Width = 276
        Height = 24
        AutoSize = True
        BeforePrint = rlbDetItemBeforePrint
        object mLinhaItem: TRLMemo
          Left = 0
          Top = 0
          Width = 276
          Height = 24
          Align = faTop
          Behavior = [beSiteExpander]
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Courier New'
          Font.Style = [fsBold]
          Lines.Strings = (
            '9999999999999 DESCRICAO DO PRODUTO 99,999 UN x 999,999 (99,99)')
          ParentFont = False
        end
      end
      object rlbDescItem: TRLBand
        Left = 0
        Top = 24
        Width = 276
        Height = 24
        BeforePrint = rlbDescItemBeforePrint
        object lTitDesconto: TRLLabel
          Left = 18
          Top = 1
          Width = 34
          Height = 10
          Caption = 'Desconto'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTitDescValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 47
          Height = 10
          Caption = 'Valor L'#237'quido'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lDesconto: TRLLabel
          Left = 227
          Top = 1
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lDescValLiq: TRLLabel
          Left = 227
          Top = 13
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbOutroItem: TRLBand
        Left = 0
        Top = 48
        Width = 276
        Height = 24
        BeforePrint = rlbOutroItemBeforePrint
        object lTitAcrescimo: TRLLabel
          Left = 18
          Top = 1
          Width = 38
          Height = 10
          Caption = 'Acr'#233'scimo'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lTitOutroValLiq: TRLLabel
          Left = 18
          Top = 13
          Width = 47
          Height = 10
          Caption = 'Valor L'#237'quido'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lOutro: TRLLabel
          Left = 227
          Top = 1
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lOutroValLiq: TRLLabel
          Left = 227
          Top = 13
          Width = 34
          Height = 10
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object rlbGap: TRLBand
        Left = 0
        Top = 72
        Width = 276
        Height = 2
        BandType = btSummary
        BeforePrint = rlbGapBeforePrint
      end
    end
    object rlsbPagamentos: TRLSubDetail
      Left = 2
      Top = 242
      Width = 276
      Height = 65
      OnDataRecord = rlsbPagamentosDataRecord
      object rlbPagamento: TRLBand
        Left = 0
        Top = 42
        Width = 276
        Height = 12
        AutoSize = True
        BeforePrint = rlbPagamentoBeforePrint
        object lPagamento: TRLLabel
          Left = 218
          Top = 0
          Width = 44
          Height = 12
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lMeioPagamento: TRLLabel
          Left = 0
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
        Left = 0
        Top = 54
        Width = 276
        Height = 12
        AutoSize = True
        BandType = btSummary
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbTrocoBeforePrint
        object lTitTroco: TRLLabel
          Left = -2
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
          Left = 218
          Top = 0
          Width = 44
          Height = 12
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
      object rlbTotal: TRLBand
        Left = 0
        Top = 0
        Width = 276
        Height = 42
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
          Left = -2
          Top = 18
          Width = 80
          Height = 11
          Caption = 'VALOR TOTAL R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTotal: TRLLabel
          Left = 217
          Top = 18
          Width = 44
          Height = 11
          Alignment = taRightJustify
          Caption = '99.999,99'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lQtdItens: TRLLabel
          Left = 0
          Top = 6
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
          Left = 230
          Top = 6
          Width = 31
          Height = 12
          Alignment = taRightJustify
          Caption = '99.999'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTitFormaPagto: TRLLabel
          Left = 0
          Top = 30
          Width = 112
          Height = 12
          Caption = 'FORMA DE PAGAMENTO'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lTitValorPago: TRLLabel
          Left = 213
          Top = 30
          Width = 48
          Height = 12
          Alignment = taRightJustify
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RLDraw7: TRLDraw
          Left = 0
          Top = 0
          Width = 276
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Width = 2
        end
      end
    end
    object rlbLei12741: TRLBand
      Left = 2
      Top = 307
      Width = 276
      Height = 28
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbLei12741BeforePrint
      object RLDraw6: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lTitLei12741: TRLLabel
        Left = 0
        Top = 6
        Width = 182
        Height = 12
        Caption = 'Informa'#231#227'o dos Tributos Totais Incidentes '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lTitLei12742: TRLLabel
        Left = 1
        Top = 16
        Width = 112
        Height = 12
        Caption = '(Lei Federal 12.741 /2012)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lValLei12741: TRLLabel
        Left = 218
        Top = 7
        Width = 44
        Height = 12
        Alignment = taRightJustify
        Caption = '99.999,99'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lFonteTributos: TRLLabel
        Left = 136
        Top = 16
        Width = 126
        Height = 12
        Alignment = taRightJustify
        Caption = 'Fonte IBPT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlbsCabecalho: TRLSubDetail
      Left = 2
      Top = 8
      Width = 276
      Height = 154
      OnDataRecord = rlbsCabecalhoDataRecord
      object rlbMsgDANFe: TRLBand
        Left = 0
        Top = 81
        Width = 276
        Height = 42
        AutoSize = True
        object lMsgDANFCe: TRLLabel
          Left = 0
          Top = 0
          Width = 276
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
        object lMsgDANFCe1: TRLLabel
          Left = 0
          Top = 28
          Width = 276
          Height = 14
          Align = faTop
          Alignment = taCenter
          Caption = 'N'#227'o permite aproveitamento de cr'#233'dito de ICMS'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object lMsgDANFCe2: TRLLabel
          Left = 0
          Top = 14
          Width = 276
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
      object rlbDadosCliche: TRLBand
        Left = 0
        Top = 0
        Width = 276
        Height = 81
        AutoSize = True
        object pLogoeCliche: TRLPanel
          Left = 0
          Top = 0
          Width = 276
          Height = 81
          Align = faTop
          AutoExpand = True
          AutoSize = True
          object lEmitCNPJ_IE_IM: TRLLabel
            Left = 0
            Top = 31
            Width = 276
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
          object lEndereco: TRLMemo
            Left = 0
            Top = 43
            Width = 276
            Height = 30
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
          object RLDraw1: TRLDraw
            Left = 0
            Top = 73
            Width = 276
            Height = 8
            Align = faTop
            DrawKind = dkLine
            Pen.Width = 2
          end
          object imgLogo: TRLImage
            Left = 0
            Top = 0
            Width = 276
            Height = 1
            Align = faTop
            AutoSize = True
            Center = True
            Scaled = True
            Transparent = False
          end
          object lNomeFantasia: TRLMemo
            Left = 0
            Top = 1
            Width = 276
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
          object lRazaoSocial: TRLMemo
            Left = 0
            Top = 19
            Width = 276
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
      object rlbLegenda: TRLBand
        Left = 0
        Top = 123
        Width = 276
        Height = 28
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object RLDraw4: TRLDraw
          Left = 0
          Top = 0
          Width = 276
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Width = 2
        end
        object lCPF_CNPJ1: TRLLabel
          Left = 0
          Top = 8
          Width = 276
          Height = 12
          Align = faTop
          Caption = '#|COD|DESC|QTD|UN| VL UN R$|(VLTR R$)*| VL ITEM R$'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlBottom
          ParentFont = False
        end
        object RLDraw5: TRLDraw
          Left = 0
          Top = 20
          Width = 276
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Width = 2
        end
      end
    end
    object rlbConsumidor: TRLBand
      Left = 2
      Top = 365
      Width = 276
      Height = 55
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbConsumidorBeforePrint
      object RLDraw10: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lTitConsumidor: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
      object lEnderecoConsumidor: TRLMemo
        Left = 0
        Top = 43
        Width = 276
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
      object lCPF_CNPJ_ID: TRLMemo
        Left = 0
        Top = 19
        Width = 276
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
    object rlbMensagemFiscal: TRLBand
      Left = 2
      Top = 420
      Width = 276
      Height = 98
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbMensagemFiscalBeforePrint
      object RLDraw12: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lMensagemFiscal: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
      object lChaveDeAcesso: TRLLabel
        Left = 0
        Top = 74
        Width = 276
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
      object lTitChaveAcesso: TRLLabel
        Left = 0
        Top = 62
        Width = 276
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
      object lNumeroSerie: TRLLabel
        Left = 0
        Top = 18
        Width = 276
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
      object lTitConsulteChave: TRLMemo
        Left = 0
        Top = 50
        Width = 276
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
      object lEmissaoVia: TRLLabel
        Left = 0
        Top = 30
        Width = 276
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
      object RLDraw8: TRLDraw
        Left = 0
        Top = 42
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lCancelada: TRLLabel
        Left = 0
        Top = 86
        Width = 276
        Height = 12
        Align = faTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlbMensagemContribuinte: TRLBand
      Left = 2
      Top = 335
      Width = 276
      Height = 30
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbMensagemContribuinteBeforePrint
      object lMensagemContribuinte: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
      object RLDraw3: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lObservacoes: TRLMemo
        Left = 0
        Top = 18
        Width = 276
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
  object rlCancelamento: TRLReport
    Left = 473
    Top = 4
    Width = 280
    Height = 1512
    Margins.LeftMargin = 0.610000000000000000
    Margins.TopMargin = 2.000000000000000000
    Margins.RightMargin = 0.610000000000000000
    Margins.BottomMargin = 0.000000000000000000
    AllowedBands = [btHeader, btDetail, btSummary, btFooter]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    PageSetup.PaperSize = fpCustom
    PageSetup.PaperWidth = 74.000000000000000000
    PageSetup.PaperHeight = 400.000000000000000000
    PrintDialog = False
    ShowProgress = False
    BeforePrint = rlCancelamentoBeforePrint
    OnDataRecord = rlVendaDataRecord
    object rlbRodapeCanc: TRLBand
      Left = 2
      Top = 345
      Width = 276
      Height = 222
      AutoSize = True
      BandType = btSummary
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      object RLDraw9: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lConsultaQRCodeCanc: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
        Left = 0
        Top = 20
        Width = 276
        Height = 137
        Align = faTop
        Center = True
        Scaled = True
      end
      object RLPanel1: TRLPanel
        Left = 0
        Top = 167
        Width = 276
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
          Width = 276
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
        Left = 0
        Top = 157
        Width = 276
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
      object RLPanel2: TRLPanel
        Left = 0
        Top = 184
        Width = 276
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
      Left = 2
      Top = 8
      Width = 276
      Height = 154
      OnDataRecord = rlbsCabecalhoDataRecord
      object RLBand10: TRLBand
        Left = 0
        Top = 63
        Width = 276
        Height = 42
        AutoSize = True
        object RLLabel26: TRLLabel
          Left = 0
          Top = 0
          Width = 276
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
          Width = 276
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
          Width = 276
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
        Left = 0
        Top = 0
        Width = 276
        Height = 63
        AutoSize = True
        object RLPanel3: TRLPanel
          Left = 0
          Top = 0
          Width = 276
          Height = 63
          Align = faTop
          AutoExpand = True
          AutoSize = True
          object lEmitCNPJ_IE_IM_Camc: TRLLabel
            Left = 0
            Top = 31
            Width = 276
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
            Width = 276
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
            Width = 276
            Height = 8
            Align = faTop
            DrawKind = dkLine
            Pen.Width = 2
          end
          object RLImage2: TRLImage
            Left = 0
            Top = 0
            Width = 276
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
            Width = 276
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
            Width = 276
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
        Left = 0
        Top = 105
        Width = 276
        Height = 8
        AutoSize = True
        BeforePrint = rlbLegendaBeforePrint
        object RLDraw15: TRLDraw
          Left = 0
          Top = 0
          Width = 276
          Height = 8
          Align = faTop
          DrawKind = dkLine
          Pen.Width = 2
        end
      end
    end
    object rlbConsumidorCanc: TRLBand
      Left = 2
      Top = 192
      Width = 276
      Height = 55
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbConsumidorCancBeforePrint
      object RLDraw17: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lTitConsumidorCanc: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
        Left = 0
        Top = 43
        Width = 276
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
        Left = 0
        Top = 19
        Width = 276
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
      Left = 2
      Top = 247
      Width = 276
      Height = 98
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbMensagemFiscalCancBeforePrint
      object RLDraw18: TRLDraw
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lMensagemFiscalCanc: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
        Left = 0
        Top = 74
        Width = 276
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
        Left = 0
        Top = 62
        Width = 276
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
        Left = 0
        Top = 18
        Width = 276
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
        Left = 0
        Top = 50
        Width = 276
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
        Left = 0
        Top = 30
        Width = 276
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
        Left = 0
        Top = 42
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lCanceladaCanc: TRLLabel
        Left = 0
        Top = 86
        Width = 276
        Height = 12
        Align = faTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object rlbMensagemContribuinteCanc: TRLBand
      Left = 2
      Top = 162
      Width = 276
      Height = 30
      AutoSize = True
      BandType = btSummary
      BeforePrint = rlbMensagemContribuinteCancBeforePrint
      object lMensagemContribuinteCamc: TRLLabel
        Left = 0
        Top = 8
        Width = 276
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
        Left = 0
        Top = 0
        Width = 276
        Height = 8
        Align = faTop
        DrawKind = dkLine
        Pen.Width = 2
      end
      object lObservacoesCanc: TRLMemo
        Left = 0
        Top = 18
        Width = 276
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
