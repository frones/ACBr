inherited frmMDFeDAEventoRLRetrato: TfrmMDFeDAEventoRLRetrato
  Left = 267
  Top = 209
  Caption = 'Evento - Retrato'
  ClientHeight = 749
  ClientWidth = 763
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  TextHeight = 10
  inherited RLMDFeEvento: TRLReport
    Left = 0
    Top = 0
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    DataSource = Datasource1
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = rlEventoBeforePrint
    object rlb_09_Itens: TRLBand
      Left = 26
      Top = 1021
      Width = 742
      Height = 13
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_09_ItensBeforePrint
      object rldbtTpDoc2: TRLDBText
        Left = 373
        Top = 1
        Width = 76
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rldbtTpDoc1: TRLDBText
        Left = 5
        Top = 1
        Width = 76
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rldbtDocumento2: TRLDBText
        Left = 542
        Top = 1
        Width = 195
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rldbtDocumento1: TRLDBText
        Left = 174
        Top = 1
        Width = 195
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rldbtCnpjEmitente2: TRLDBText
        Left = 456
        Top = 1
        Width = 51
        Height = 12
        Color = clWhite
        DataField = 'CNPJCPF_2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rldbtCnpjEmitente1: TRLDBText
        Left = 88
        Top = 1
        Width = 51
        Height = 12
        Color = clWhite
        DataField = 'CNPJCPF_1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
    end
    object rlb_01_Titulo: TRLBand
      Left = 26
      Top = 26
      Width = 742
      Height = 73
      BandType = btTitle
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_01_TituloBeforePrint
      object rlShape46: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 67
        Brush.Style = bsClear
      end
      object rllLinha1: TRLLabel
        Left = 302
        Top = 2
        Width = 140
        Height = 19
        Alignment = taCenter
        Caption = 'CANCELAMENTO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllLinha2: TRLLabel
        Left = 154
        Top = 24
        Width = 432
        Height = 15
        Alignment = taCenter
        Caption = 
          'N'#227'o possui valor fiscal, simples representa'#231#227'o do Cancelamento i' +
          'ndicada abaixo.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllLinha3: TRLLabel
        Left = 102
        Top = 45
        Width = 536
        Height = 15
        Alignment = taCenter
        Caption = 
          'CONSULTE A AUTENTICIDADE DO CANCELAMENTO NO SITE DA SEFAZ AUTORI' +
          'ZADORA.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
    end
    object rlb_08_HeaderItens: TRLBand
      Left = 26
      Top = 1053
      Width = 742
      Height = 16
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_08_HeaderItensBeforePrint
    end
    object rlb_10_Sistema: TRLBand
      Left = 26
      Top = 1034
      Width = 742
      Height = 19
      AlignToBottom = True
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_10_SistemaBeforePrint
      object rlLabel15: TRLLabel
        Left = 2
        Top = 0
        Width = 140
        Height = 12
        Caption = 'DATA E HORA DA IMPRESS'#195'O: '
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlSysData1: TRLSystemInfo
        Left = 143
        Top = 0
        Width = 26
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
        Transparent = False
      end
      object rllblSistema: TRLLabel
        Left = 352
        Top = 0
        Width = 387
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Desenvolvido por Projeto ACBr - http://acbr.sourceforge.net/'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_05_Evento: TRLBand
      Left = 26
      Top = 429
      Width = 742
      Height = 120
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_05_EventoBeforePrint
      object rlsQuadro01: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 113
        Brush.Style = bsClear
      end
      object rlsLinhaV01: TRLDraw
        Left = 68
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV09: TRLDraw
        Left = 590
        Top = 15
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV10: TRLDraw
        Left = 43
        Top = 15
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaH04: TRLDraw
        Left = 0
        Top = 48
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlLabel2: TRLLabel
        Left = 4
        Top = 18
        Width = 27
        Height = 8
        Caption = 'ORG'#195#402'O'
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
      object rllOrgao: TRLLabel
        Left = 4
        Top = 28
        Width = 38
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel78: TRLLabel
        Left = 74
        Top = 51
        Width = 83
        Height = 8
        Caption = 'DESCRI'#199#195'O DO EVENTO'
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
      object rllDescricaoEvento: TRLLabel
        Left = 74
        Top = 62
        Width = 92
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllDescricao: TRLLabel
        Left = 414
        Top = 83
        Width = 54
        Height = 8
        Caption = 'N'#186' PROTOCOLO'
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
      object rllProtocolo: TRLLabel
        Left = 414
        Top = 96
        Width = 322
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape88: TRLDraw
        Left = 0
        Top = 80
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rllTituloEvento: TRLLabel
        Left = 6
        Top = 2
        Width = 98
        Height = 13
        Caption = 'CANCELAMENTO'
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
      object rlShape48: TRLDraw
        Left = 0
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel9: TRLLabel
        Left = 50
        Top = 18
        Width = 37
        Height = 8
        Caption = 'AMBIENTE'
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
      object rllTipoAmbiente: TRLLabel
        Left = 50
        Top = 28
        Width = 89
        Height = 13
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rlLabel6: TRLLabel
        Left = 598
        Top = 18
        Width = 133
        Height = 8
        Caption = 'DATA E HORA DE EMISS'#195'O DO EVENTO'
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
      object rllEmissaoEvento: TRLLabel
        Left = 598
        Top = 28
        Width = 85
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel28: TRLLabel
        Left = 4
        Top = 51
        Width = 29
        Height = 8
        Caption = 'EVENTO'
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
      object rllTipoEvento: TRLLabel
        Left = 4
        Top = 62
        Width = 62
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel17: TRLLabel
        Left = 598
        Top = 50
        Width = 82
        Height = 8
        Caption = 'SEQU'#202'NCIA DO EVENTO'
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
      object rllSeqEvento: TRLLabel
        Left = 598
        Top = 60
        Width = 62
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlShape49: TRLDraw
        Left = 590
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape50: TRLDraw
        Left = 410
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlLabel18: TRLLabel
        Left = 4
        Top = 83
        Width = 28
        Height = 8
        Caption = 'STATUS'
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
      object rllStatus: TRLLabel
        Left = 4
        Top = 94
        Width = 40
        Height = 14
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
    object rlb_03_Emitente: TRLBand
      Left = 26
      Top = 189
      Width = 742
      Height = 120
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_03_EmitenteBeforePrint
      object rlsQuadro02: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 113
        Brush.Style = bsClear
      end
      object rlsLinhaH07: TRLDraw
        Left = 1
        Top = 80
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlsLinhaH06: TRLDraw
        Left = 1
        Top = 48
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rllRazaoEmitente: TRLLabel
        Left = 4
        Top = 30
        Width = 84
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMunEmitente: TRLLabel
        Left = 4
        Top = 94
        Width = 77
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllInscEstEmitente: TRLLabel
        Left = 624
        Top = 94
        Width = 90
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoEmitente: TRLLabel
        Left = 4
        Top = 62
        Width = 99
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCNPJEmitente: TRLLabel
        Left = 608
        Top = 30
        Width = 82
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCEPEmitente: TRLLabel
        Left = 669
        Top = 62
        Width = 64
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel98: TRLLabel
        Left = 669
        Top = 52
        Width = 15
        Height = 8
        Caption = 'CEP'
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
      object rlLabel93: TRLLabel
        Left = 624
        Top = 84
        Width = 78
        Height = 8
        Caption = 'INSCRI'#199#195'O ESTADUAL'
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
      object rlLabel24: TRLLabel
        Left = 608
        Top = 19
        Width = 19
        Height = 8
        Caption = 'CNPJ'
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
      object rlLabel22: TRLLabel
        Left = 4
        Top = 84
        Width = 38
        Height = 8
        Caption = 'MUNIC'#205'PIO'
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
      object rlLabel16: TRLLabel
        Left = 4
        Top = 52
        Width = 39
        Height = 8
        Caption = 'ENDERE'#199'O'
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
      object rlLabel13: TRLLabel
        Left = 4
        Top = 19
        Width = 79
        Height = 8
        Caption = 'NOME / RAZ'#195'O SOCIAL'
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
      object rlLabel12: TRLLabel
        Left = 6
        Top = 2
        Width = 55
        Height = 13
        Caption = 'EMITENTE'
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
      object rlShape51: TRLDraw
        Left = 0
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlShape53: TRLDraw
        Left = 604
        Top = 15
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape82: TRLDraw
        Left = 665
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape99: TRLDraw
        Left = 448
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlLabel4: TRLLabel
        Left = 452
        Top = 52
        Width = 29
        Height = 8
        Caption = 'BAIRRO'
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
      object rllBairroEmitente: TRLLabel
        Left = 452
        Top = 62
        Width = 85
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape108: TRLDraw
        Left = 620
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlLabel5: TRLLabel
        Left = 452
        Top = 84
        Width = 40
        Height = 8
        Caption = 'FONE / FAX'
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
      object rllFoneEmitente: TRLLabel
        Left = 452
        Top = 94
        Width = 79
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape109: TRLDraw
        Left = 448
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
    end
    object rlb_04_Tomador: TRLBand
      Left = 26
      Top = 309
      Width = 742
      Height = 120
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_04_TomadorBeforePrint
      object rlsQuadro03: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 113
        Brush.Style = bsClear
      end
      object rlLabel1: TRLLabel
        Left = 6
        Top = 2
        Width = 138
        Height = 13
        Caption = 'TOMADOR DO SERVI'#199'O'
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
      object rlShape52: TRLDraw
        Left = 0
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel14: TRLLabel
        Left = 4
        Top = 19
        Width = 79
        Height = 8
        Caption = 'NOME / RAZ'#195'O SOCIAL'
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
      object rllRazaoTomador: TRLLabel
        Left = 4
        Top = 30
        Width = 84
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel25: TRLLabel
        Left = 4
        Top = 52
        Width = 39
        Height = 8
        Caption = 'ENDERE'#199'O'
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
      object rllEnderecoTomador: TRLLabel
        Left = 4
        Top = 62
        Width = 99
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel27: TRLLabel
        Left = 4
        Top = 84
        Width = 38
        Height = 8
        Caption = 'MUNIC'#205'PIO'
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
      object rllMunTomador: TRLLabel
        Left = 4
        Top = 94
        Width = 77
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel30: TRLLabel
        Left = 608
        Top = 19
        Width = 38
        Height = 8
        Caption = 'CNPJ / CPF'
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
      object rllCNPJTomador: TRLLabel
        Left = 608
        Top = 30
        Width = 82
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel32: TRLLabel
        Left = 452
        Top = 52
        Width = 29
        Height = 8
        Caption = 'BAIRRO'
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
      object rllBairroTomador: TRLLabel
        Left = 452
        Top = 62
        Width = 85
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel35: TRLLabel
        Left = 669
        Top = 52
        Width = 15
        Height = 8
        Caption = 'CEP'
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
      object rllCEPTomador: TRLLabel
        Left = 669
        Top = 62
        Width = 64
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel37: TRLLabel
        Left = 452
        Top = 84
        Width = 40
        Height = 8
        Caption = 'FONE / FAX'
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
      object rllFoneTomador: TRLLabel
        Left = 452
        Top = 94
        Width = 79
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel40: TRLLabel
        Left = 624
        Top = 84
        Width = 78
        Height = 8
        Caption = 'INSCRI'#199#195'O ESTADUAL'
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
      object rllInscEstTomador: TRLLabel
        Left = 624
        Top = 94
        Width = 90
        Height = 14
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape7: TRLDraw
        Left = 604
        Top = 15
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape8: TRLDraw
        Left = 1
        Top = 48
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlShape9: TRLDraw
        Left = 1
        Top = 80
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlShape55: TRLDraw
        Left = 448
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape56: TRLDraw
        Left = 448
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape58: TRLDraw
        Left = 665
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape59: TRLDraw
        Left = 620
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
    end
    object rlb_06_Descricao: TRLBand
      Left = 26
      Top = 549
      Width = 742
      Height = 112
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_DescricaoBeforePrint
      object rlsQuadro04: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 105
        Brush.Style = bsClear
      end
      object rlmDescricao: TRLMemo
        Left = 5
        Top = 19
        Width = 732
        Height = 78
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        Lines.Strings = (
          'rlmCondicoes')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel59: TRLLabel
        Left = 6
        Top = 2
        Width = 71
        Height = 13
        Caption = 'DESCRI'#199#195'O'
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
      object rllMsgTeste: TRLLabel
        Left = 11
        Top = 43
        Width = 718
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
      object rlShape5: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
    end
    object rlb_07_Correcao: TRLBand
      Left = 26
      Top = 661
      Width = 742
      Height = 360
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_07_CorrecaoBeforePrint
      object rlsQuadro05: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 353
        Brush.Style = bsClear
      end
      object rlShape18: TRLDraw
        Left = 160
        Top = 15
        Width = 1
        Height = 338
        Brush.Style = bsClear
      end
      object rlShape19: TRLDraw
        Left = 270
        Top = 15
        Width = 1
        Height = 338
        Brush.Style = bsClear
      end
      object rlShape15: TRLDraw
        Left = 50
        Top = 15
        Width = 1
        Height = 338
        Brush.Style = bsClear
      end
      object rlmNumItemAlterado: TRLMemo
        Left = 8
        Top = 27
        Width = 40
        Height = 318
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Item 1'
          'Item 2'
          'Item 3'
          'Item 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmCampoAlterado: TRLMemo
        Left = 164
        Top = 27
        Width = 100
        Height = 318
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Campo 1'
          'Campo 2'
          'Campo 3'
          'Campo 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmValorAlterado: TRLMemo
        Left = 274
        Top = 27
        Width = 463
        Height = 318
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Valor 1'
          'Valor 2'
          'Valor 3'
          'Valor 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmGrupoAlterado: TRLMemo
        Left = 54
        Top = 27
        Width = 100
        Height = 318
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Grupo 1'
          'Grupo 2'
          'Grupo 3'
          'Grupo 4')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel46: TRLLabel
        Left = 164
        Top = 18
        Width = 27
        Height = 8
        Caption = 'CAMPO'
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
      object rlLabel45: TRLLabel
        Left = 30
        Top = 18
        Width = 18
        Height = 8
        Caption = 'ITEM'
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
      object rlLabel44: TRLLabel
        Left = 54
        Top = 18
        Width = 26
        Height = 8
        Caption = 'GRUPO'
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
      object rlLabel42: TRLLabel
        Left = 274
        Top = 18
        Width = 26
        Height = 8
        Caption = 'VALOR'
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
      object rlLabel38: TRLLabel
        Left = 6
        Top = 2
        Width = 70
        Height = 13
        Caption = 'CORRE'#199#195'O'
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
      object rlShape17: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
    end
    object rlb_02_Documento: TRLBand
      Left = 26
      Top = 99
      Width = 742
      Height = 90
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_02_DocumentoBeforePrint
      object rlShape10: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 81
        Brush.Style = bsClear
      end
      object rlLabel65: TRLLabel
        Left = 6
        Top = 2
        Width = 335
        Height = 13
        Caption = 'MANIFESTO ELETR'#212'NICO DE DOCUMENTOS FISCAIS- MDF-e'
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
      object rlShape81: TRLDraw
        Left = 1
        Top = 44
        Width = 368
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel8: TRLLabel
        Left = 5
        Top = 19
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
      object rllModelo: TRLLabel
        Left = 6
        Top = 27
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
      object rlLabel21: TRLLabel
        Left = 59
        Top = 19
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
      object rllSerie: TRLLabel
        Left = 60
        Top = 27
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
      object rlLabel23: TRLLabel
        Left = 112
        Top = 19
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
      object rllNumMDFe: TRLLabel
        Left = 110
        Top = 27
        Width = 72
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
      object rlsLinhaV05: TRLDraw
        Left = 50
        Top = 15
        Width = 1
        Height = 30
        Brush.Style = bsClear
      end
      object rlsLinhaV06: TRLDraw
        Left = 100
        Top = 15
        Width = 1
        Height = 30
        Brush.Style = bsClear
      end
      object rlsLinhaV08: TRLDraw
        Left = 236
        Top = 15
        Width = 1
        Height = 30
        Brush.Style = bsClear
      end
      object rlLabel33: TRLLabel
        Left = 246
        Top = 19
        Width = 95
        Height = 9
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
      object rllEmissao: TRLLabel
        Left = 246
        Top = 27
        Width = 58
        Height = 13
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
      object rlsLinhaV07: TRLDraw
        Left = 368
        Top = 15
        Width = 1
        Height = 66
        Brush.Style = bsClear
      end
      object rlLabel74: TRLLabel
        Left = 6
        Top = 46
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
      object rllChave: TRLLabel
        Left = 6
        Top = 60
        Width = 356
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
      object rlShape2: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLBarcode1: TRLBarcode
        Left = 375
        Top = 25
        Width = 87
        Height = 49
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
      end
    end
  end
end
