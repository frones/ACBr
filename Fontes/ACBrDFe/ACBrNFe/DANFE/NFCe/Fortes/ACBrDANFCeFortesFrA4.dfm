object frmACBrDANFCeFortesFrA4: TfrmACBrDANFCeFortesFrA4
  Left = 0
  Top = 0
  Caption = 'frmACBrDANFCeFortesFrA4'
  ClientHeight = 773
  ClientWidth = 978
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object rlReportA4: TRLReport
    Left = 8
    Top = 0
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    OnDataRecord = rlReportA4DataRecord
    object RLBand1: TRLBand
      Left = 38
      Top = 38
      Width = 718
      Height = 77
      BandType = btTitle
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      object imgLogo: TRLImage
        Left = 4
        Top = 4
        Width = 70
        Height = 70
        Center = True
        Scaled = True
        BeforePrint = imgLogoBeforePrint
      end
      object lNomeFantasia: TRLLabel
        Left = 80
        Top = 8
        Width = 387
        Height = 18
        Caption = 'Nome Fantasia'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
        BeforePrint = lNomeFantasiaBeforePrint
      end
      object RLLabel1: TRLLabel
        Left = 80
        Top = 24
        Width = 110
        Height = 16
        Caption = 'Raz'#227'o Social Ltda'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel1BeforePrint
      end
      object RLLabel2: TRLLabel
        Left = 80
        Top = 40
        Width = 159
        Height = 16
        Caption = 'CNPJ: 12.345.789/0001.12'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel2BeforePrint
      end
      object RLLabel4: TRLLabel
        Left = 488
        Top = 24
        Width = 221
        Height = 16
        Alignment = taRightJustify
        Caption = 'Inscri'#231#227'o Municipal: 12345678900123'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel4BeforePrint
      end
      object RLLabel5: TRLLabel
        Left = 492
        Top = 40
        Width = 217
        Height = 16
        Alignment = taRightJustify
        Caption = 'Inscri'#231#227'o Estadual: 12345678900123'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel5BeforePrint
      end
      object RLMemo3: TRLMemo
        Left = 80
        Top = 56
        Width = 629
        Height = 16
        Behavior = [beSiteExpander]
        BeforePrint = RLMemo3BeforePrint
      end
    end
    object RLBand2: TRLBand
      Left = 38
      Top = 115
      Width = 718
      Height = 36
      BandType = btTitle
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      object RLLabel6: TRLLabel
        Left = 112
        Top = 0
        Width = 489
        Height = 16
        Caption = 
          'DANFE NFC-e - Documento Auxiliar da Nota Fiscal de Consumidor El' +
          'etr'#244'nica'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel7: TRLLabel
        Left = 200
        Top = 16
        Width = 311
        Height = 16
        Caption = 'N'#227'o permite aproveitamento de cr'#233'dito de ICMS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
    end
    object RLBand3: TRLBand
      Left = 38
      Top = 151
      Width = 718
      Height = 18
      BandType = btColumnHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      object RLLabel8: TRLLabel
        Left = 0
        Top = 0
        Width = 105
        Height = 16
        Caption = 'Produto.............:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel9: TRLLabel
        Left = 112
        Top = 0
        Width = 129
        Height = 16
        Caption = 'Descri'#231#227'o do Produto'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel10: TRLLabel
        Left = 320
        Top = 0
        Width = 74
        Height = 16
        Caption = 'Quantidade:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel11: TRLLabel
        Left = 400
        Top = 0
        Width = 72
        Height = 16
        Caption = 'Vlr.Unit'#225'rio:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel12: TRLLabel
        Left = 648
        Top = 0
        Width = 70
        Height = 16
        Caption = 'Valor Total:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel14: TRLLabel
        Left = 480
        Top = 0
        Width = 63
        Height = 16
        Caption = 'Desconto:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel15: TRLLabel
        Left = 552
        Top = 0
        Width = 70
        Height = 16
        Caption = 'Acr'#233'scimo:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
    end
    object subItens: TRLSubDetail
      Left = 38
      Top = 169
      Width = 718
      Height = 58
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      OnDataRecord = subItensDataRecord
      object RLBand4: TRLBand
        Left = 1
        Top = 0
        Width = 716
        Height = 18
        GreenBarPrint = True
        object RLLabel13: TRLLabel
          Left = 0
          Top = 0
          Width = 106
          Height = 16
          AutoSize = False
          Caption = '1234567890123'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel13BeforePrint
        end
        object RLMemo1: TRLMemo
          Left = 112
          Top = 0
          Width = 202
          Height = 16
          Behavior = [beSiteExpander]
          BeforePrint = RLMemo1BeforePrint
        end
        object RLLabel16: TRLLabel
          Left = 320
          Top = 0
          Width = 74
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Quantidade:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel16BeforePrint
        end
        object RLLabel17: TRLLabel
          Left = 400
          Top = 0
          Width = 72
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Vlr.Unit'#225'rio:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel17BeforePrint
        end
        object RLLabel18: TRLLabel
          Left = 480
          Top = 0
          Width = 63
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Desconto:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel18BeforePrint
        end
        object RLLabel19: TRLLabel
          Left = 552
          Top = 0
          Width = 70
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Acr'#233'scimo:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel19BeforePrint
        end
        object RLLabel20: TRLLabel
          Left = 648
          Top = 0
          Width = 70
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Valor Total:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel20BeforePrint
        end
      end
    end
    object RLBand5: TRLBand
      Left = 38
      Top = 227
      Width = 718
      Height = 18
      BandType = btSummary
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = False
      object RLLabel21: TRLLabel
        Left = 0
        Top = 0
        Width = 156
        Height = 16
        Caption = 'Quantidade Total de Itens:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
      end
      object RLLabel22: TRLLabel
        Left = 159
        Top = 0
        Width = 32
        Height = 16
        Caption = '0000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel22BeforePrint
      end
      object RLLabel23: TRLLabel
        Left = 560
        Top = 0
        Width = 154
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
        BeforePrint = RLLabel23BeforePrint
      end
      object RLLabel24: TRLLabel
        Left = 478
        Top = 0
        Width = 78
        Height = 16
        Caption = 'Valor Total:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Layout = tlCenter
        ParentFont = False
      end
    end
    object RLSubDetail1: TRLSubDetail
      Left = 38
      Top = 245
      Width = 718
      Height = 78
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = True
      Positioning = btSummary
      OnDataRecord = RLSubDetail1DataRecord
      object RLBand6: TRLBand
        Left = 0
        Top = 0
        Width = 718
        Height = 19
        BandType = btTitle
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = False
        object RLLabel25: TRLLabel
          Left = 423
          Top = -1
          Width = 133
          Height = 18
          Alignment = taRightJustify
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = True
          Caption = 'Forma de Pagamento:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
        end
        object RLLabel26: TRLLabel
          Left = 559
          Top = 1
          Width = 155
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Borders.Sides = sdCustom
          Borders.DrawLeft = False
          Borders.DrawTop = False
          Borders.DrawRight = False
          Borders.DrawBottom = True
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
        end
      end
      object RLBand7: TRLBand
        Left = 0
        Top = 19
        Width = 718
        Height = 18
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = False
        object RLLabel27: TRLLabel
          Left = 422
          Top = 0
          Width = 133
          Height = 16
          Alignment = taRightJustify
          Caption = 'Forma de Pagamento:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel27BeforePrint
        end
        object RLLabel28: TRLLabel
          Left = 559
          Top = 0
          Width = 155
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel28BeforePrint
        end
      end
      object RLBand8: TRLBand
        Left = 0
        Top = 37
        Width = 718
        Height = 18
        BandType = btSummary
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = False
        BeforePrint = RLBand8BeforePrint
        object RLLabel29: TRLLabel
          Left = 512
          Top = 0
          Width = 44
          Height = 16
          Alignment = taRightJustify
          Caption = 'Troco:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object RLLabel30: TRLLabel
          Left = 559
          Top = 0
          Width = 155
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Valor Pago'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel30BeforePrint
        end
      end
    end
    object RLSubDetail2: TRLSubDetail
      Left = 38
      Top = 323
      Width = 718
      Height = 446
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Positioning = btSummary
      OnDataRecord = RLSubDetail2DataRecord
      object RLBand9: TRLBand
        Left = 0
        Top = 0
        Width = 718
        Height = 17
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = True
        object RLLabel31: TRLLabel
          Left = 312
          Top = 0
          Width = 399
          Height = 16
          Alignment = taRightJustify
          Caption = 
            'Informa'#231#227'o dos Tributos Totais Incidentes (Lei Federal 12.741/20' +
            '12):'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel31BeforePrint
        end
      end
      object RLBand10: TRLBand
        Left = 0
        Top = 17
        Width = 718
        Height = 35
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = True
        BeforePrint = RLBand10BeforePrint
        object RLMemo2: TRLMemo
          Left = 1
          Top = 16
          Width = 714
          Height = 16
          Behavior = [beSiteExpander]
          BeforePrint = RLMemo2BeforePrint
        end
        object RLLabel34: TRLLabel
          Left = 1
          Top = 0
          Width = 716
          Height = 12
          Align = faTop
          Alignment = taCenter
          Caption = #193'REA DE MENSAGEM DE INTERESSE DO CONTRIBUINTE'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
      end
      object RLBand11: TRLBand
        Left = 0
        Top = 52
        Width = 718
        Height = 73
        AutoSize = True
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = True
        object RLLabel32: TRLLabel
          Left = 1
          Top = 0
          Width = 716
          Height = 12
          Align = faTop
          Alignment = taCenter
          Caption = #193'REA DE MENSAGEM FISCAL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel32BeforePrint
        end
        object RLLabel33: TRLLabel
          Left = 1
          Top = 12
          Width = 716
          Height = 16
          Align = faTop
          Alignment = taCenter
          Caption = 'Numero'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel33BeforePrint
        end
        object RLLabel35: TRLLabel
          Left = 1
          Top = 28
          Width = 716
          Height = 16
          Align = faTop
          Alignment = taCenter
          Caption = 'Consulte pela chave de acesso em http://www.fazenda.pr.gov.br/'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel35BeforePrint
        end
        object RLLabel36: TRLLabel
          Left = 1
          Top = 44
          Width = 716
          Height = 12
          Align = faTop
          Alignment = taCenter
          Caption = 'CHAVE DE ACESSO'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Layout = tlCenter
          ParentFont = False
        end
        object RLLabel37: TRLLabel
          Left = 1
          Top = 56
          Width = 716
          Height = 16
          Align = faTop
          Alignment = taCenter
          Caption = '432432423'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Layout = tlCenter
          ParentFont = False
          BeforePrint = RLLabel37BeforePrint
        end
      end
      object rlbConsumidor: TRLBand
        Left = 0
        Top = 125
        Width = 718
        Height = 44
        AutoSize = True
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = True
        BeforePrint = rlbConsumidorBeforePrint
        object lTitConsumidor: TRLLabel
          Left = 1
          Top = 0
          Width = 716
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
          Left = 1
          Top = 27
          Width = 716
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
            'Endere'#231'o Consumidor (Logradouro, n'#186', bairro, Munic'#237#173'pio)')
          ParentFont = False
        end
        object lCPF_CNPJ_ID: TRLMemo
          Left = 1
          Top = 11
          Width = 716
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
            
              'CNPJ/CPF/ID Estrangeiro - CCCCCCCCCCCCCCCCCCCC NOME DO CONSUMIDO' +
              'R')
          ParentFont = False
        end
      end
      object rlbRodape: TRLBand
        Left = 0
        Top = 169
        Width = 718
        Height = 211
        AutoSize = True
        BandType = btSummary
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        BeforePrint = rlbRodapeBeforePrint
        object lConsultaQRCode: TRLLabel
          Left = 1
          Top = 0
          Width = 716
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
          Left = 1
          Top = 12
          Width = 716
          Height = 133
          Align = faTop
          Center = True
          Scaled = True
        end
        object pGap05: TRLPanel
          Left = 1
          Top = 155
          Width = 716
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
            Width = 716
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
          Left = 1
          Top = 145
          Width = 716
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
      end
    end
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 'Projeto ACBr'
    DisplayName = 'Documento PDF'
    Left = 818
    Top = 79
  end
  object RLHTMLFilter1: TRLHTMLFilter
    DocumentStyle = dsCSS2
    DisplayName = 'HTML'
    Left = 821
    Top = 131
  end
end
