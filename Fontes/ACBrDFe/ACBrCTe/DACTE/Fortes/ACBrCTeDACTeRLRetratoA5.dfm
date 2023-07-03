inherited frmDACTeRLRetratoA5: TfrmDACTeRLRetratoA5
  Left = 312
  Top = 167
  Caption = 'DACTe -  Retrato A5'
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  TextHeight = 10
  inherited RLCTe: TRLReport
    Left = 16
    Top = 8
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    DataSource = Datasource1
    Font.Height = -8
    Font.Name = 'Courier New'
    Title = 'DACTe Retrato A5'
    BeforePrint = RLCTeBeforePrint
    object rlb_08_Itens: TRLBand
      Left = 26
      Top = 762
      Width = 742
      Height = 16
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      AfterPrint = rlb_08_ItensAfterPrint
      BeforePrint = rlb_08_ItensBeforePrint
      object RLDraw29: TRLDraw
        Left = 370
        Top = 0
        Width = 1
        Height = 13
        Brush.Style = bsClear
      end
      object RLDraw28: TRLDraw
        Left = 0
        Top = 0
        Width = 1
        Height = 14
        Brush.Style = bsClear
      end
      object RLDraw35: TRLDraw
        Left = 740
        Top = 0
        Width = 1
        Height = 14
        Brush.Style = bsClear
      end
      object rldbtTpDoc2: TRLDBText
        Left = 373
        Top = 1
        Width = 74
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_2'
        DataSource = Datasource1
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
        Width = 74
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_1'
        DataSource = Datasource1
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
        Left = 582
        Top = 1
        Width = 156
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_2'
        DataSource = Datasource1
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
        Left = 206
        Top = 1
        Width = 162
        Height = 13
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_1'
        DataSource = Datasource1
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
        Left = 449
        Top = 1
        Width = 128
        Height = 12
        Color = clWhite
        DataField = 'CNPJCPF_2'
        DataSource = Datasource1
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
        Left = 81
        Top = 1
        Width = 290
        Height = 12
        Color = clWhite
        DataField = 'CNPJCPF_1'
        DataSource = Datasource1
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
      object rlsFimItens: TRLDraw
        Left = 0
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
    end
    object rlb_01_Recibo: TRLBand
      Left = 26
      Top = 26
      Width = 742
      Height = 56
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_01_ReciboBeforePrint
      object RLDraw46: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 57
        Brush.Style = bsClear
      end
      object RLDraw49: TRLDraw
        Left = 1
        Top = 32
        Width = 201
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw50: TRLDraw
        Left = 593
        Top = 14
        Width = 1
        Height = 46
        Brush.Style = bsClear
      end
      object RLDraw51: TRLDraw
        Left = 473
        Top = 14
        Width = 1
        Height = 46
        Brush.Style = bsClear
      end
      object RLDraw52: TRLDraw
        Left = 202
        Top = 14
        Width = 1
        Height = 46
        Brush.Style = bsClear
      end
      object rllSerie2: TRLLabel
        Left = 655
        Top = 41
        Width = 50
        Height = 13
        Alignment = taCenter
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
      object rllNumCTe2: TRLLabel
        Left = 638
        Top = 27
        Width = 86
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
      object rlLabel3: TRLLabel
        Left = 480
        Top = 40
        Width = 108
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel143: TRLLabel
        Left = 480
        Top = 20
        Width = 108
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel140: TRLLabel
        Left = 647
        Top = 15
        Width = 28
        Height = 13
        Caption = 'CT-E'
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
      object rlLabel139: TRLLabel
        Left = 605
        Top = 27
        Width = 14
        Height = 12
        Caption = 'No '
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel138: TRLLabel
        Left = 605
        Top = 41
        Width = 30
        Height = 12
        Caption = 'S'#201'RIE:'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel137: TRLLabel
        Left = 6
        Top = 1
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO EST' +
          'ADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel136: TRLLabel
        Left = 6
        Top = 18
        Width = 30
        Height = 12
        Caption = 'NOME'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel135: TRLLabel
        Left = 480
        Top = 15
        Width = 108
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'CHEGADA DATA/HORA'
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
      object rlLabel134: TRLLabel
        Left = 480
        Top = 35
        Width = 108
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'SA'#205'DA DATA/HORA'
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
      object rlLabel133: TRLLabel
        Left = 207
        Top = 42
        Width = 262
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'ASSINATURA / CARIMBO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel132: TRLLabel
        Left = 6
        Top = 37
        Width = 19
        Height = 13
        AutoSize = False
        Caption = 'RG'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw101: TRLDraw
        Left = 0
        Top = 65
        Width = 756
        Height = 1
        Pen.Style = psDot
      end
      object RLDraw48: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
    end
    object rlb_07_HeaderItens: TRLBand
      Left = 26
      Top = 751
      Width = 742
      Height = 11
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_07_HeaderItensBeforePrint
      object rlsQuadro07: TRLDraw
        Left = 1
        Top = 0
        Width = 741
        Height = 11
        Brush.Style = bsClear
      end
      object RLDraw34: TRLDraw
        Left = 370
        Top = 0
        Width = 1
        Height = 11
        Brush.Style = bsClear
      end
      object rlLabel96: TRLLabel
        Left = 206
        Top = 1
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
      object rlLabel92: TRLLabel
        Left = 80
        Top = 1
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
      object rlLabel91: TRLLabel
        Left = 5
        Top = 1
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
      object rlLabel109: TRLLabel
        Left = 373
        Top = 1
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
        Left = 448
        Top = 1
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
        Left = 582
        Top = 1
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
    object rlb_09_Obs: TRLBand
      Left = 26
      Top = 695
      Width = 742
      Height = 56
      AlignToBottom = True
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_09_ObsBeforePrint
      object rlsQuadro08: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 56
        Brush.Style = bsClear
      end
      object rlmObs: TRLMemo
        Left = 304
        Top = 15
        Width = 241
        Height = 40
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2'
          'OBS LINHA 3'
          'OBS LINHA 4')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel10: TRLLabel
        Left = 304
        Top = 1
        Width = 234
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Observa'#231#245'es - Informa'#231#245'es Complementares'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel11: TRLLabel
        Left = 6
        Top = 1
        Width = 72
        Height = 8
        Caption = 'RNTRC DA EMPRESA'
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
      object lblCIOT: TRLLabel
        Left = 84
        Top = 1
        Width = 18
        Height = 8
        Caption = 'CIOT'
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
      object rlLabel83: TRLLabel
        Left = 154
        Top = 1
        Width = 35
        Height = 8
        Caption = 'LOTA'#199#195'O'
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
      object rlLabel84: TRLLabel
        Left = 196
        Top = 1
        Width = 101
        Height = 8
        Caption = 'DATA PREVISTA DE ENTREGA'
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
      object rllRntrcEmpresa: TRLLabel
        Left = 6
        Top = 8
        Width = 64
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCIOT: TRLLabel
        Left = 84
        Top = 8
        Width = 32
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllLotacao: TRLLabel
        Left = 154
        Top = 8
        Width = 34
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'SIM'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllDtPrevEntrega: TRLLabel
        Left = 196
        Top = 8
        Width = 69
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlsCIOT: TRLDraw
        Left = 80
        Top = 0
        Width = 1
        Height = 20
        Brush.Style = bsClear
      end
      object RLDraw36: TRLDraw
        Left = 150
        Top = 0
        Width = 1
        Height = 20
        Brush.Style = bsClear
      end
      object RLDraw37: TRLDraw
        Left = 192
        Top = 0
        Width = 1
        Height = 20
        Brush.Style = bsClear
      end
      object RLDraw38: TRLDraw
        Left = 300
        Top = 0
        Width = 1
        Height = 56
        Brush.Style = bsClear
      end
      object RLDraw24: TRLDraw
        Left = 1
        Top = 20
        Width = 300
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel85: TRLLabel
        Left = 6
        Top = 22
        Width = 292
        Height = 27
        Alignment = taCenter
        AutoSize = False
        Caption = 'ESSE CT-e DE TRANSP. ATENDE LEGISLA'#199#195'O DE TRANSP. RODO.EM VIGOR'
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
      object rlLabel20: TRLLabel
        Left = 552
        Top = 1
        Width = 186
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'RESERVADO AO FISCO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw5: TRLDraw
        Left = 548
        Top = 0
        Width = 1
        Height = 56
        Brush.Style = bsClear
      end
      object rlmObsFisco: TRLMemo
        Left = 552
        Top = 15
        Width = 185
        Height = 40
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2')
        ParentColor = False
        ParentFont = False
      end
      object rllMsgTeste: TRLLabel
        Left = 7
        Top = 14
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
      object RLDraw1: TRLDraw
        Left = 300
        Top = 14
        Width = 440
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
    end
    object rlb_02_Cabecalho: TRLBand
      Left = 26
      Top = 228
      Width = 742
      Height = 148
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_02_CabecalhoBeforePrint
      object RLBarcode1: TRLBarcode
        Left = 354
        Top = 117
        Width = 381
        Height = 28
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        AutoSize = False
        BarcodeType = bcCode128C
      end
      object rlsQuadro01: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 149
        Brush.Style = bsClear
      end
      object rlsLinhaH02: TRLDraw
        Left = 332
        Top = 54
        Width = 408
        Height = 1
        Brush.Style = bsClear
      end
      object rlsLinhaH03: TRLDraw
        Left = 332
        Top = 88
        Width = 408
        Height = 1
        Brush.Style = bsClear
      end
      object rlsLinhaV01: TRLDraw
        Left = 105
        Top = 114
        Width = 1
        Height = 35
        Brush.Style = bsClear
      end
      object rlsLinhaV04: TRLDraw
        Left = 332
        Top = 0
        Width = 1
        Height = 149
        Brush.Style = bsClear
      end
      object rlsLinhaV05: TRLDraw
        Left = 366
        Top = 29
        Width = 1
        Height = 26
        Brush.Style = bsClear
      end
      object rlsLinhaV06: TRLDraw
        Left = 390
        Top = 29
        Width = 1
        Height = 26
        Brush.Style = bsClear
      end
      object rlsLinhaV07: TRLDraw
        Left = 614
        Top = 29
        Width = 1
        Height = 28
        Brush.Style = bsClear
      end
      object rlsLinhaV08: TRLDraw
        Left = 464
        Top = 29
        Width = 1
        Height = 26
        Brush.Style = bsClear
      end
      object rlsLinhaV09: TRLDraw
        Left = 508
        Top = 29
        Width = 1
        Height = 26
        Brush.Style = bsClear
      end
      object rlsLinhaV10: TRLDraw
        Left = 627
        Top = 0
        Width = 1
        Height = 30
        Brush.Style = bsClear
      end
      object rliLogo: TRLImage
        Left = 7
        Top = 34
        Width = 94
        Height = 62
        Center = True
        Stretch = True
      end
      object rlsLinhaH04: TRLDraw
        Left = 0
        Top = 114
        Width = 332
        Height = 1
        Brush.Style = bsClear
      end
      object rlmEmitente: TRLMemo
        Left = 7
        Top = 1
        Width = 322
        Height = 24
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
        Left = 113
        Top = 26
        Width = 216
        Height = 83
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
      object rlLabel17: TRLLabel
        Left = 371
        Top = 1
        Width = 218
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'DACTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rlLabel18: TRLLabel
        Left = 344
        Top = 16
        Width = 278
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Documento Auxiliar do Conhecimento de Transporte Eletr'#244'nico'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rlLabel6: TRLLabel
        Left = 640
        Top = 1
        Width = 76
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = 'MODAL'
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
      object rllModal: TRLLabel
        Left = 633
        Top = 16
        Width = 96
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel8: TRLLabel
        Left = 333
        Top = 30
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
        Left = 334
        Top = 38
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
        Left = 367
        Top = 30
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
        Left = 368
        Top = 38
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
        Left = 392
        Top = 30
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
      object rllNumCte: TRLLabel
        Left = 392
        Top = 38
        Width = 70
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = '999.999.999'
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
      object rlLabel25: TRLLabel
        Left = 466
        Top = 30
        Width = 42
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
      object rllPageNumber: TRLLabel
        Left = 466
        Top = 38
        Width = 42
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = '00/00'
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
      object rlLabel33: TRLLabel
        Left = 510
        Top = 30
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
        Left = 510
        Top = 38
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
      object rlLabel74: TRLLabel
        Left = 334
        Top = 90
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
        Left = 336
        Top = 100
        Width = 402
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
      object rlLabel2: TRLLabel
        Left = 4
        Top = 116
        Width = 46
        Height = 8
        Caption = 'TIPO DO CT-E'
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
      object rllTipoCte: TRLLabel
        Left = 4
        Top = 124
        Width = 76
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel9: TRLLabel
        Left = 107
        Top = 116
        Width = 61
        Height = 8
        Caption = 'TIPO DO SERVI'#199'O'
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
      object rllTipoServico: TRLLabel
        Left = 107
        Top = 124
        Width = 77
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel28: TRLLabel
        Left = 212
        Top = 117
        Width = 81
        Height = 8
        Caption = 'TOMADOR DO SERVI'#199'O'
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
      object rllTomaServico: TRLLabel
        Left = 212
        Top = 126
        Width = 81
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllDescricao: TRLLabel
        Left = 334
        Top = 117
        Width = 56
        Height = 8
        Caption = 'N'#176' PROTOCOLO'
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
      object rllProtocolo: TRLLabel
        Left = 336
        Top = 126
        Width = 402
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
      end
      object rlLabel77: TRLLabel
        Left = 616
        Top = 30
        Width = 120
        Height = 8
        Caption = 'INSC. SUFRAMA DO DESTINAT'#193'RIO'
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
      object rllInscSuframa: TRLLabel
        Left = 616
        Top = 38
        Width = 56
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw88: TRLDraw
        Left = 332
        Top = 148
        Width = 408
        Height = 1
        Brush.Style = bsClear
      end
      object rlsLinhaH01: TRLDraw
        Left = 332
        Top = 29
        Width = 408
        Height = 1
        Brush.Style = bsClear
      end
      object RLDraw99: TRLDraw
        Left = 332
        Top = 114
        Width = 408
        Height = 1
        Brush.Style = bsClear
      end
      object RLDraw3: TRLDraw
        Left = 0
        Top = 148
        Width = 332
        Height = 1
        Brush.Style = bsClear
      end
      object rliBarCode: TRLBarcode
        Left = 337
        Top = 58
        Width = 398
        Height = 28
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        AutoSize = False
        BarcodeType = bcCode128C
        Caption = '1234569789'
      end
      object RLDraw12: TRLDraw
        Left = 209
        Top = 114
        Width = 1
        Height = 35
        Brush.Style = bsClear
      end
    end
    object rlb_03_DadosDACTe: TRLBand
      Left = 26
      Top = 376
      Width = 742
      Height = 81
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_03_DadosDACTeBeforePrint
      object rlsQuadro02: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 82
        Brush.Style = bsClear
      end
      object rlsLinhaV11: TRLDraw
        Left = 370
        Top = 24
        Width = 1
        Height = 58
        Brush.Style = bsClear
      end
      object rlsLinhaH05: TRLDraw
        Left = 1
        Top = 24
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rllRazaoRemet: TRLLabel
        Left = 48
        Top = 25
        Width = 318
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllRazaoDest: TRLLabel
        Left = 432
        Top = 25
        Width = 303
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllPaisRemet: TRLLabel
        Left = 48
        Top = 67
        Width = 209
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllOrigPrestacao: TRLLabel
        Left = 336
        Top = 9
        Width = 195
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllNatOperacao: TRLLabel
        Left = 4
        Top = 9
        Width = 325
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMunRemet: TRLLabel
        Left = 48
        Top = 49
        Width = 234
        Height = 19
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMunDest: TRLLabel
        Left = 432
        Top = 49
        Width = 225
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllInscEstRemet: TRLLabel
        Left = 256
        Top = 58
        Width = 109
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllInscEstDest: TRLLabel
        Left = 632
        Top = 58
        Width = 102
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllFoneRemet: TRLLabel
        Left = 288
        Top = 68
        Width = 77
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllFoneDest: TRLLabel
        Left = 664
        Top = 67
        Width = 70
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRemet2: TRLLabel
        Left = 48
        Top = 41
        Width = 318
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRemet1: TRLLabel
        Left = 48
        Top = 33
        Width = 318
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoDest1: TRLLabel
        Left = 432
        Top = 33
        Width = 303
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllDestPrestacao: TRLLabel
        Left = 542
        Top = 9
        Width = 195
        Height = 15
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCnpjRemet: TRLLabel
        Left = 48
        Top = 58
        Width = 124
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCEPRemet: TRLLabel
        Left = 301
        Top = 49
        Width = 64
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCEPDest: TRLLabel
        Left = 677
        Top = 49
        Width = 57
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel98: TRLLabel
        Left = 284
        Top = 49
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
      object rlLabel95: TRLLabel
        Left = 262
        Top = 68
        Width = 20
        Height = 8
        Caption = 'FONE'
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
        Left = 174
        Top = 58
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
      object rlLabel79: TRLLabel
        Left = 374
        Top = 67
        Width = 17
        Height = 8
        Caption = 'PAIS'
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
      object rlLabel32: TRLLabel
        Left = 374
        Top = 58
        Width = 34
        Height = 8
        Caption = 'CNPJ/CPF'
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
      object rlLabel31: TRLLabel
        Left = 374
        Top = 49
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
      object rlLabel30: TRLLabel
        Left = 374
        Top = 33
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
      object rlLabel29: TRLLabel
        Left = 4
        Top = 1
        Width = 115
        Height = 8
        Caption = 'CFOP - NATUREZA DA OPERA'#199#195'O'
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
      object rlLabel27: TRLLabel
        Left = 374
        Top = 25
        Width = 52
        Height = 8
        Caption = 'DESTINAT'#193'RIO'
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
      object rlLabel26: TRLLabel
        Left = 4
        Top = 67
        Width = 17
        Height = 8
        Caption = 'PAIS'
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
        Left = 4
        Top = 58
        Width = 34
        Height = 8
        Caption = 'CNPJ/CPF'
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
        Top = 49
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
        Top = 33
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
      object rlLabel14: TRLLabel
        Left = 542
        Top = 1
        Width = 86
        Height = 8
        Caption = 'DESTINO DA PRESTA'#199#195'O'
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
        Top = 25
        Width = 42
        Height = 8
        Caption = 'REMETENTE'
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
        Left = 336
        Top = 1
        Width = 84
        Height = 8
        Caption = 'ORIGEM DA PRESTA'#199#195'O'
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
      object rlLabel116: TRLLabel
        Left = 640
        Top = 67
        Width = 20
        Height = 8
        Caption = 'FONE'
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
      object rlLabel114: TRLLabel
        Left = 551
        Top = 58
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
      object RLDraw102: TRLDraw
        Left = 536
        Top = 0
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw103: TRLDraw
        Left = 332
        Top = 0
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object rllCnpjDest: TRLLabel
        Left = 432
        Top = 58
        Width = 115
        Height = 18
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllPaisDest: TRLLabel
        Left = 432
        Top = 67
        Width = 203
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel119: TRLLabel
        Left = 660
        Top = 49
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
      object rllEnderecoDest2: TRLLabel
        Left = 432
        Top = 41
        Width = 303
        Height = 13
        AutoSize = False
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
    object rlb_04_DadosNotaFiscal: TRLBand
      Left = 26
      Top = 522
      Width = 742
      Height = 65
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_04_DadosNotaFiscalBeforePrint
      object rlsQuadro03: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 66
        Brush.Style = bsClear
      end
      object RLDraw9: TRLDraw
        Left = 148
        Top = 0
        Width = 1
        Height = 23
        Brush.Style = bsClear
      end
      object RLDraw8: TRLDraw
        Left = 632
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw7: TRLDraw
        Left = 508
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw61: TRLDraw
        Left = 415
        Top = 0
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object RLDraw60: TRLDraw
        Left = 324
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw59: TRLDraw
        Left = 164
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw58: TRLDraw
        Left = 84
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw56: TRLDraw
        Left = 294
        Top = 0
        Width = 1
        Height = 23
        Brush.Style = bsClear
      end
      object RLDraw55: TRLDraw
        Left = 1
        Top = 23
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlmQtdUnidMedida5: TRLMemo
        Left = 328
        Top = 32
        Width = 84
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1')
        ParentColor = False
        ParentFont = False
      end
      object rlmQtdUnidMedida3: TRLMemo
        Left = 166
        Top = 32
        Width = 76
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1')
        ParentColor = False
        ParentFont = False
      end
      object rlmQtdUnidMedida2: TRLMemo
        Left = 86
        Top = 32
        Width = 76
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1')
        ParentColor = False
        ParentFont = False
      end
      object rlmQtdUnidMedida1: TRLMemo
        Left = 5
        Top = 32
        Width = 76
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1')
        ParentColor = False
        ParentFont = False
      end
      object rllVlrTotalMerc: TRLLabel
        Left = 298
        Top = 10
        Width = 110
        Height = 13
        Alignment = taRightJustify
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
      object rllRespSeguroMerc: TRLLabel
        Left = 418
        Top = 32
        Width = 87
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllProdPredominante: TRLLabel
        Left = 4
        Top = 10
        Width = 141
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllOutrasCaracCarga: TRLLabel
        Left = 152
        Top = 10
        Width = 139
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllNroAverbacao: TRLLabel
        Left = 634
        Top = 32
        Width = 102
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllNroApolice: TRLLabel
        Left = 510
        Top = 32
        Width = 122
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllNomeSeguradora: TRLLabel
        Left = 418
        Top = 10
        Width = 319
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel5: TRLLabel
        Left = 418
        Top = 1
        Width = 84
        Height = 8
        Caption = 'NOME DA SEGURADORA'
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
      object rlLabel43: TRLLabel
        Left = 328
        Top = 24
        Width = 84
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'QTDE. VOLUMES (Unid)'
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
      object rlLabel41: TRLLabel
        Left = 166
        Top = 24
        Width = 76
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO AFERIDO (Kg)'
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
      object rlLabel40: TRLLabel
        Left = 634
        Top = 24
        Width = 90
        Height = 8
        Caption = 'N'#218'MERO DA AVERBA'#199#195'O'
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
        Left = 152
        Top = 1
        Width = 135
        Height = 8
        Caption = 'OUTRAS CARACTER'#205'STICAS DA CARGA'
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
      object rlLabel39: TRLLabel
        Left = 510
        Top = 24
        Width = 75
        Height = 8
        Caption = 'N'#218'MERO DA AP'#211'LICE'
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
      object rlLabel37: TRLLabel
        Left = 418
        Top = 24
        Width = 51
        Height = 8
        Caption = 'RESPONS'#193'VEL'
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
      object rlLabel36: TRLLabel
        Left = 86
        Top = 24
        Width = 76
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO BASE C'#193'LC. (Kg)'
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
      object rlLabel35: TRLLabel
        Left = 5
        Top = 24
        Width = 76
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO BRUTO (Kg)'
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
      object rlLabel34: TRLLabel
        Left = 298
        Top = 1
        Width = 111
        Height = 8
        Caption = 'VALOR TOTAL DA MERCADORIA'
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
      object rlLabel1: TRLLabel
        Left = 4
        Top = 1
        Width = 91
        Height = 8
        Caption = 'PRODUTO PREDOMINANTE'
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
      object rlmQtdUnidMedida4: TRLMemo
        Left = 246
        Top = 32
        Width = 76
        Height = 12
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel73: TRLLabel
        Left = 246
        Top = 24
        Width = 76
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'CUBAGEM (M3)'
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
      object RLDraw100: TRLDraw
        Left = 244
        Top = 23
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object rlLabel52: TRLLabel
        Left = 3
        Top = 45
        Width = 81
        Height = 8
        Caption = 'SITUA'#199#195'O TRIBUT'#193'RIA'
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
      object rllSitTrib: TRLLabel
        Left = 3
        Top = 52
        Width = 340
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw22: TRLDraw
        Left = 346
        Top = 44
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object rlLabel55: TRLLabel
        Left = 350
        Top = 45
        Width = 66
        Height = 8
        Caption = 'BASE DE C'#193'LCULO'
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
      object rlLabel56: TRLLabel
        Left = 454
        Top = 45
        Width = 39
        Height = 8
        Caption = 'AL'#205'Q. ICMS'
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
      object rlLabel54: TRLLabel
        Left = 504
        Top = 45
        Width = 45
        Height = 8
        Caption = 'VALOR ICMS'
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
      object rlLabel53: TRLLabel
        Left = 590
        Top = 45
        Width = 59
        Height = 8
        Caption = '% RED.BC.CALC.'
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
      object rlLabel58: TRLLabel
        Left = 656
        Top = 45
        Width = 29
        Height = 8
        Caption = 'ICMS ST'
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
      object RLDraw20: TRLDraw
        Left = 448
        Top = 44
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw23: TRLDraw
        Left = 500
        Top = 44
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw25: TRLDraw
        Left = 586
        Top = 44
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw26: TRLDraw
        Left = 650
        Top = 44
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object rllBaseCalc: TRLLabel
        Left = 350
        Top = 52
        Width = 95
        Height = 13
        Alignment = taRightJustify
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
      object rllAliqICMS: TRLLabel
        Left = 454
        Top = 52
        Width = 41
        Height = 13
        Alignment = taRightJustify
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
      object rllVlrICMS: TRLLabel
        Left = 504
        Top = 52
        Width = 79
        Height = 13
        Alignment = taRightJustify
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
      object rllRedBaseCalc: TRLLabel
        Left = 590
        Top = 52
        Width = 57
        Height = 13
        Alignment = taRightJustify
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
      object rllICMS_ST: TRLLabel
        Left = 656
        Top = 52
        Width = 81
        Height = 13
        Alignment = taRightJustify
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
      object RLDraw62: TRLDraw
        Left = 0
        Top = 44
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
    end
    object rlb_05_Complemento: TRLBand
      Left = 26
      Top = 587
      Width = 742
      Height = 61
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_05_ComplementoBeforePrint
      object rlsQuadro04: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 60
        Brush.Style = bsClear
      end
      object RLDraw6: TRLDraw
        Left = 372
        Top = 0
        Width = 1
        Height = 60
        Brush.Style = bsClear
      end
      object rlmComplValor2: TRLMemo
        Left = 645
        Top = 11
        Width = 89
        Height = 46
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmComplValor1: TRLMemo
        Left = 280
        Top = 11
        Width = 89
        Height = 46
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmComplChave2: TRLMemo
        Left = 377
        Top = 11
        Width = 264
        Height = 46
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmComplChave1: TRLMemo
        Left = 5
        Top = 11
        Width = 269
        Height = 46
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlLabel64: TRLLabel
        Left = 645
        Top = 2
        Width = 90
        Height = 8
        Caption = 'VALOR COMPLEMENTADO'
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
      object rlLabel63: TRLLabel
        Left = 377
        Top = 2
        Width = 119
        Height = 8
        Caption = 'CHAVE DO CT-E COMPLEMENTADO'
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
      object rlLabel62: TRLLabel
        Left = 280
        Top = 2
        Width = 90
        Height = 8
        Caption = 'VALOR COMPLEMENTADO'
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
      object rlLabel61: TRLLabel
        Left = 5
        Top = 2
        Width = 119
        Height = 8
        Caption = 'CHAVE DO CT-E COMPLEMENTADO'
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
    object rlb_17_Sistema: TRLBand
      Left = 26
      Top = 1154
      Width = 742
      Height = 13
      AlignToBottom = True
      BandType = btSummary
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_17_SistemaBeforePrint
      object rlLabel15: TRLLabel
        Left = 1
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
      object rllblSistema: TRLLabel
        Left = 352
        Top = 1
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
    object rlb_06_ValorPrestacao: TRLBand
      Left = 26
      Top = 648
      Width = 742
      Height = 47
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_ValorPrestacaoBeforePrint
      object rlsQuadro05: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 47
        Brush.Style = bsClear
      end
      object RLDraw16: TRLDraw
        Left = 557
        Top = 21
        Width = 184
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw18: TRLDraw
        Left = 372
        Top = 0
        Width = 1
        Height = 47
        Brush.Style = bsClear
      end
      object RLDraw19: TRLDraw
        Left = 556
        Top = 0
        Width = 1
        Height = 47
        Brush.Style = bsClear
      end
      object RLDraw15: TRLDraw
        Left = 186
        Top = 0
        Width = 1
        Height = 47
        Brush.Style = bsClear
      end
      object rlmCompValor3: TRLMemo
        Left = 476
        Top = 10
        Width = 78
        Height = 36
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompValor2: TRLMemo
        Left = 290
        Top = 10
        Width = 78
        Height = 36
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompValor1: TRLMemo
        Left = 104
        Top = 10
        Width = 78
        Height = 36
        Alignment = taRightJustify
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompNome3: TRLMemo
        Left = 377
        Top = 10
        Width = 96
        Height = 36
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompNome2: TRLMemo
        Left = 190
        Top = 10
        Width = 96
        Height = 36
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompNome1: TRLMemo
        Left = 5
        Top = 10
        Width = 96
        Height = 36
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3')
        ParentColor = False
        ParentFont = False
      end
      object rllVlrTotServico: TRLLabel
        Left = 658
        Top = 6
        Width = 78
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
      object rllVlrTotReceber: TRLLabel
        Left = 658
        Top = 28
        Width = 78
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
      object rlLabel50: TRLLabel
        Left = 560
        Top = 23
        Width = 96
        Height = 9
        AutoSize = False
        Caption = 'VALOR A RECEBER'
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
      object rlLabel49: TRLLabel
        Left = 560
        Top = 1
        Width = 96
        Height = 9
        AutoSize = False
        Caption = 'VALOR TOTAL DO SERVI'#199'O'
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
      object rlLabel48: TRLLabel
        Left = 528
        Top = 1
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
      object rlLabel47: TRLLabel
        Left = 377
        Top = 1
        Width = 22
        Height = 8
        Caption = 'NOME'
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
      object rlLabel46: TRLLabel
        Left = 156
        Top = 1
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
      object rlLabel45: TRLLabel
        Left = 342
        Top = 1
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
      object rlLabel44: TRLLabel
        Left = 5
        Top = 1
        Width = 22
        Height = 8
        Caption = 'NOME'
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
        Left = 190
        Top = 1
        Width = 22
        Height = 8
        Caption = 'NOME'
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
    object rlb_12_ModAereo: TRLBand
      Left = 26
      Top = 881
      Width = 742
      Height = 97
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_12_ModAereoBeforePrint
      object RLDraw47: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 96
        Brush.Style = bsClear
      end
      object RLDraw66: TRLDraw
        Left = 68
        Top = 49
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw67: TRLDraw
        Left = 90
        Top = 49
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw68: TRLDraw
        Left = 154
        Top = 49
        Width = 1
        Height = 22
        Brush.Style = bsClear
      end
      object RLDraw69: TRLDraw
        Left = 540
        Top = 15
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw65: TRLDraw
        Left = 260
        Top = 15
        Width = 1
        Height = 56
        Brush.Style = bsClear
      end
      object RLDraw57: TRLDraw
        Left = 1
        Top = 38
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw72: TRLDraw
        Left = 596
        Top = 39
        Width = 1
        Height = 56
        Brush.Style = bsClear
      end
      object RLDraw70: TRLDraw
        Left = 34
        Top = 70
        Width = 1
        Height = 26
        Brush.Style = bsClear
      end
      object rllTrecho: TRLLabel
        Left = 2
        Top = 58
        Width = 37
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllTarifaValor: TRLLabel
        Left = 158
        Top = 58
        Width = 95
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0,00'
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
      object rllTarifaCodigo: TRLLabel
        Left = 95
        Top = 58
        Width = 22
        Height = 12
        Caption = '1234'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllTarifaCL: TRLLabel
        Left = 72
        Top = 58
        Width = 12
        Height = 12
        Caption = '12'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllRetira: TRLLabel
        Left = 2
        Top = 81
        Width = 26
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'SIM'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMinuta: TRLLabel
        Left = 672
        Top = 50
        Width = 65
        Height = 19
        AutoSize = False
        Caption = '123456789'
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
      object rllLojaAgenteEmissor: TRLLabel
        Left = 598
        Top = 81
        Width = 88
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllDadosRetira: TRLLabel
        Left = 39
        Top = 81
        Width = 554
        Height = 14
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllContaCorrente: TRLLabel
        Left = 262
        Top = 49
        Width = 67
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCaracAdTransporte: TRLLabel
        Left = 262
        Top = 25
        Width = 85
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCaracAdServico: TRLLabel
        Left = 6
        Top = 25
        Width = 73
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllAWB: TRLLabel
        Left = 632
        Top = 16
        Width = 105
        Height = 19
        AutoSize = False
        Caption = '000-0-000000000-0'
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
      object rlLabel157: TRLLabel
        Left = 598
        Top = 72
        Width = 92
        Height = 8
        Caption = 'LOJA OU AGENTE EMISSOR'
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
      object rlLabel156: TRLLabel
        Left = 262
        Top = 40
        Width = 65
        Height = 8
        Caption = 'CONTA CORRENTE'
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
      object rlLabel155: TRLLabel
        Left = 262
        Top = 16
        Width = 167
        Height = 8
        Caption = 'CARACTERISTICAS ADICIONAIS DO TRANSPORTE'
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
      object rlLabel154: TRLLabel
        Left = 6
        Top = 16
        Width = 152
        Height = 8
        Caption = 'CARACTERISTICAS ADICIONAIS DO SERVI'#199'O'
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
      object rlLabel153: TRLLabel
        Left = 8
        Top = 2
        Width = 730
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES ESPEC'#205'FICAS DO MODAL A'#201'REO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel150: TRLLabel
        Left = 39
        Top = 72
        Width = 149
        Height = 8
        Caption = 'DADOS RELATIVOS A RETIRADA DA CARGA'
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
      object rlLabel149: TRLLabel
        Left = 2
        Top = 72
        Width = 27
        Height = 8
        Caption = 'RETIRA'
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
      object rlLabel148: TRLLabel
        Left = 598
        Top = 40
        Width = 73
        Height = 8
        Caption = 'N'#218'MERO DA MINUTA'
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
      object rlLabel147: TRLLabel
        Left = 158
        Top = 50
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
      object rlLabel146: TRLLabel
        Left = 95
        Top = 50
        Width = 29
        Height = 8
        Caption = 'C'#211'DIGO'
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
      object rlLabel145: TRLLabel
        Left = 72
        Top = 50
        Width = 11
        Height = 8
        Caption = 'CL'
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
      object rlLabel144: TRLLabel
        Left = 2
        Top = 50
        Width = 30
        Height = 8
        Caption = 'TRECHO'
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
      object rlLabel142: TRLLabel
        Left = 8
        Top = 40
        Width = 250
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'DADOS DA TARIFA'
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
      object rlLabel141: TRLLabel
        Left = 543
        Top = 16
        Width = 83
        Height = 8
        Caption = 'N'#218'MERO OPERACIONAL'
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
      object RLDraw54: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw63: TRLDraw
        Left = 0
        Top = 70
        Width = 741
        Height = 1
        Brush.Style = bsClear
      end
      object RLDraw64: TRLDraw
        Left = 0
        Top = 48
        Width = 260
        Height = 1
        Brush.Style = bsClear
      end
    end
    object rlb_13_ModAquaviario: TRLBand
      Left = 26
      Top = 791
      Width = 742
      Height = 90
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_13_ModAquaviarioBeforePrint
      object RLDraw73: TRLDraw
        Left = 16
        Top = 0
        Width = 741
        Height = 89
        Brush.Style = bsClear
      end
      object rlLabel151: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'DADOS ESPEC'#205'FICOS DO MODAL AQUAVI'#193'RIO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw74: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel152: TRLLabel
        Left = 6
        Top = 16
        Width = 77
        Height = 8
        Caption = 'PORTO DE EMBARQUE'
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
      object rllPortoEmbarque: TRLLabel
        Left = 6
        Top = 25
        Width = 71
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel158: TRLLabel
        Left = 406
        Top = 16
        Width = 67
        Height = 8
        Caption = 'PORTO DE DESTINO'
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
      object rllPortoDestino: TRLLabel
        Left = 406
        Top = 25
        Width = 64
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw75: TRLDraw
        Left = 1
        Top = 38
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel159: TRLLabel
        Left = 6
        Top = 40
        Width = 141
        Height = 8
        Caption = 'IDENTIFICA'#199#195'O DO NAVIO / REBOCADOR'
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
      object rllIndNavioRebocador: TRLLabel
        Left = 6
        Top = 49
        Width = 89
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw76: TRLDraw
        Left = 1
        Top = 62
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel160: TRLLabel
        Left = 6
        Top = 64
        Width = 116
        Height = 8
        Caption = 'IDENTIFICA'#199#195'O DOS CONTEINERS'
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
      object rllIndConteiners: TRLLabel
        Left = 6
        Top = 73
        Width = 66
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw77: TRLDraw
        Left = 402
        Top = 15
        Width = 1
        Height = 48
        Brush.Style = bsClear
      end
      object rlLabel162: TRLLabel
        Left = 406
        Top = 40
        Width = 95
        Height = 8
        Caption = 'VR DA B. DE CALC. AFRMM'
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
      object rllBCAFRMM: TRLLabel
        Left = 406
        Top = 49
        Width = 57
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel164: TRLLabel
        Left = 518
        Top = 40
        Width = 56
        Height = 8
        Caption = 'VLR DO AFRMM'
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
      object rllValorAFRMM: TRLLabel
        Left = 518
        Top = 49
        Width = 66
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel166: TRLLabel
        Left = 614
        Top = 40
        Width = 74
        Height = 8
        Caption = 'TIPO DE NAVEGA'#199#195'O'
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
      object rllTipoNav: TRLLabel
        Left = 614
        Top = 49
        Width = 45
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel168: TRLLabel
        Left = 694
        Top = 40
        Width = 33
        Height = 8
        Caption = 'DIRE'#199#195'O'
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
      object rllDirecao: TRLLabel
        Left = 694
        Top = 49
        Width = 41
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw78: TRLDraw
        Left = 690
        Top = 39
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw79: TRLDraw
        Left = 610
        Top = 39
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw80: TRLDraw
        Left = 514
        Top = 39
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
    end
    object rlb_14_ModFerroviario: TRLBand
      Left = 26
      Top = 784
      Width = 742
      Height = 7
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_14_ModFerroviarioBeforePrint
    end
    object rlb_15_ModDutoviario: TRLBand
      Left = 26
      Top = 778
      Width = 742
      Height = 6
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_15_ModDutoviarioBeforePrint
    end
    object rlb_01_Recibo_Aereo: TRLBand
      Left = 26
      Top = 82
      Width = 742
      Height = 146
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_01_Recibo_AereoBeforePrint
      object RLDraw10: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 145
        Brush.Style = bsClear
      end
      object RLDraw53: TRLDraw
        Left = 367
        Top = 66
        Width = 1
        Height = 78
        Brush.Style = bsClear
      end
      object RLDraw2: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel70: TRLLabel
        Left = 6
        Top = 109
        Width = 15
        Height = 12
        Caption = 'RG'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel66: TRLLabel
        Left = 6
        Top = 80
        Width = 30
        Height = 12
        Caption = 'NOME'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel65: TRLLabel
        Left = 6
        Top = 2
        Width = 730
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO EST' +
          'ADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel19: TRLLabel
        Left = 6
        Top = 17
        Width = 730
        Height = 48
        AutoSize = False
        Caption = 
          'O Transporte coberto por este conhecimento se rege pelo c'#243'digo B' +
          'rasileiro de Aeron'#225'utica (Lei 7.565 de 19/12/1986), especificame' +
          'nte pelas regras relativas a responsabilidade Civil prevista nos' +
          ' artigos 193, 241, 244, 262 e 264, de cujo teor o Expedidor / Re' +
          'metente declara concordar e ter plena ci'#234'ncia. O Expedidor / Rem' +
          'etente aceita como corretas todas as especifica'#231#245'es impressas, m' +
          'anuscritas, datilografadas ou carimbadas neste conhecimento, cer' +
          'tificando que os artigos perigosos descritos pela regulamenta'#231#227'o' +
          ' da I.C.A.O. foram devidamente informados e acondicionados para ' +
          'transporte A'#233'reo.'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw81: TRLDraw
        Left = 1
        Top = 66
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw82: TRLDraw
        Left = 1
        Top = 77
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel57: TRLLabel
        Left = 121
        Top = 68
        Width = 88
        Height = 8
        Alignment = taCenter
        Caption = 'EXPEDIDOR / REMETENTE'
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
      object rlLabel60: TRLLabel
        Left = 508
        Top = 68
        Width = 100
        Height = 8
        Alignment = taCenter
        Caption = 'DESTINAT'#193'RIO / RECEBEDOR'
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
      object rlLabel69: TRLLabel
        Left = 206
        Top = 80
        Width = 62
        Height = 12
        Caption = 'DATA / HORA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel161: TRLLabel
        Left = 206
        Top = 109
        Width = 61
        Height = 12
        Caption = 'ASSINATURA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel67: TRLLabel
        Left = 374
        Top = 80
        Width = 30
        Height = 12
        Caption = 'NOME'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel68: TRLLabel
        Left = 374
        Top = 109
        Width = 15
        Height = 12
        Caption = 'RG'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel72: TRLLabel
        Left = 574
        Top = 80
        Width = 62
        Height = 12
        Caption = 'DATA / HORA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel163: TRLLabel
        Left = 574
        Top = 109
        Width = 61
        Height = 12
        Caption = 'ASSINATURA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
    end
    object rlb_11_ModRodLot104: TRLBand
      Left = 26
      Top = 978
      Width = 742
      Height = 108
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_11_ModRodLot104BeforePrint
      object RLDraw4: TRLDraw
        Left = 0
        Top = 0
        Width = 740
        Height = 107
        Brush.Style = bsClear
        Visible = False
      end
      object RLDraw30: TRLDraw
        Left = 207
        Top = 0
        Width = 1
        Height = 105
        Brush.Style = bsClear
      end
      object RLDraw83: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw84: TRLDraw
        Left = 42
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw85: TRLDraw
        Left = 100
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw86: TRLDraw
        Left = 122
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw87: TRLDraw
        Left = 1
        Top = 29
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw89: TRLDraw
        Left = 1
        Top = 82
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw90: TRLDraw
        Left = 345
        Top = 82
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw92: TRLDraw
        Left = 330
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object rlLabel167: TRLLabel
        Left = 2
        Top = 15
        Width = 23
        Height = 12
        Caption = 'TIPO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel169: TRLLabel
        Left = 214
        Top = 1
        Width = 524
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES REFERENTES AO VALE-PED'#193'GIO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel170: TRLLabel
        Left = 2
        Top = 1
        Width = 202
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'IDENTIFICA'#199#195'O DO CONJ. TRANSPORTADOR'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel171: TRLLabel
        Left = 351
        Top = 84
        Width = 148
        Height = 8
        Caption = 'IDENTIFICA'#199#195'O DOS LACRES EM TR'#194'NSITO'
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
      object rlLabel172: TRLLabel
        Left = 212
        Top = 84
        Width = 69
        Height = 8
        Caption = 'CPF DO MOTORISTA'
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
      object rlLabel173: TRLLabel
        Left = 4
        Top = 84
        Width = 76
        Height = 8
        Caption = 'NOME DO MOTORISTA'
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
      object rlLabel174: TRLLabel
        Left = 334
        Top = 15
        Width = 87
        Height = 8
        Caption = 'N'#218'MERO COMPROVANTE'
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
      object rlLabel177: TRLLabel
        Left = 618
        Top = 15
        Width = 70
        Height = 8
        Caption = 'CNPJ RESPONS'#193'VEL'
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
      object rlLabel179: TRLLabel
        Left = 210
        Top = 15
        Width = 68
        Height = 8
        Caption = 'CNPJ FORNECEDOR'
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
      object rlLabel181: TRLLabel
        Left = 124
        Top = 15
        Width = 32
        Height = 12
        Caption = 'RNTRC'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel182: TRLLabel
        Left = 102
        Top = 15
        Width = 14
        Height = 12
        Caption = 'UF'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel183: TRLLabel
        Left = 44
        Top = 15
        Width = 34
        Height = 12
        Caption = 'PLACA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmUF2: TRLMemo
        Left = 102
        Top = 32
        Width = 16
        Height = 50
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Uf1'
          'Uf2'
          'Uf3'
          'Uf4')
        ParentColor = False
        ParentFont = False
      end
      object rlmTipo2: TRLMemo
        Left = 2
        Top = 32
        Width = 36
        Height = 50
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Tipo 1'
          'Tipo 2'
          'Tipo 3'
          'Tipo 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmRNTRC2: TRLMemo
        Left = 124
        Top = 32
        Width = 77
        Height = 50
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'RNTRC 1'
          'RNTRC 2'
          'RNTRC 3'
          'RNTRC 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmPlaca2: TRLMemo
        Left = 44
        Top = 32
        Width = 53
        Height = 50
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Placa 1'
          'Placa 2'
          'Placa 3'
          'Placa 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmCNPJForn: TRLMemo
        Left = 210
        Top = 32
        Width = 117
        Height = 48
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Empresa 1'
          'Empresa 2'
          'Empresa 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmNumCompra: TRLMemo
        Left = 334
        Top = 32
        Width = 275
        Height = 48
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Transacao 1'
          'Transacao 2'
          'Transacao 3')
        ParentColor = False
        ParentFont = False
      end
      object rllNomeMotorista2: TRLLabel
        Left = 4
        Top = 93
        Width = 76
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllLacres2: TRLLabel
        Left = 351
        Top = 93
        Width = 41
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCPFMotorista2: TRLLabel
        Left = 212
        Top = 93
        Width = 71
        Height = 12
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLDraw98: TRLDraw
        Left = 614
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object rlmCNPJPg: TRLMemo
        Left = 618
        Top = 32
        Width = 117
        Height = 48
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Empresa 1'
          'Empresa 2'
          'Empresa 3')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_18_Recibo: TRLBand
      Left = 26
      Top = 1086
      Width = 742
      Height = 68
      BandType = btSummary
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_18_ReciboBeforePrint
      object RLDraw97: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 67
        Brush.Style = bsClear
      end
      object RLDraw91: TRLDraw
        Left = 202
        Top = 15
        Width = 1
        Height = 52
        Brush.Style = bsClear
      end
      object RLDraw93: TRLDraw
        Left = 473
        Top = 15
        Width = 1
        Height = 52
        Brush.Style = bsClear
      end
      object RLDraw94: TRLDraw
        Left = 593
        Top = 15
        Width = 1
        Height = 52
        Brush.Style = bsClear
      end
      object RLDraw95: TRLDraw
        Left = 1
        Top = 40
        Width = 201
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw96: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlLabel175: TRLLabel
        Left = 480
        Top = 49
        Width = 108
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel176: TRLLabel
        Left = 480
        Top = 25
        Width = 108
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel180: TRLLabel
        Left = 647
        Top = 17
        Width = 28
        Height = 13
        Caption = 'CT-E'
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
      object rlLabel184: TRLLabel
        Left = 605
        Top = 33
        Width = 14
        Height = 12
        Caption = 'N'#176' '
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel185: TRLLabel
        Left = 605
        Top = 47
        Width = 30
        Height = 12
        Caption = 'S'#201'RIE:'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel186: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO EST' +
          'ADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel187: TRLLabel
        Left = 6
        Top = 21
        Width = 30
        Height = 12
        Caption = 'NOME'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel188: TRLLabel
        Left = 480
        Top = 17
        Width = 108
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'CHEGADA DATA/HORA'
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
      object rlLabel189: TRLLabel
        Left = 480
        Top = 40
        Width = 108
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'SA'#205'DA DATA/HORA'
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
      object rlLabel190: TRLLabel
        Left = 207
        Top = 48
        Width = 262
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'ASSINATURA / CARIMBO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlLabel191: TRLLabel
        Left = 6
        Top = 45
        Width = 15
        Height = 12
        Caption = 'RG'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllSerie3: TRLLabel
        Left = 655
        Top = 47
        Width = 50
        Height = 13
        Alignment = taCenter
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
      object rllNumCTe3: TRLLabel
        Left = 638
        Top = 33
        Width = 86
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
    end
    object rlb_03_DadosRedespachoExpedidor: TRLBand
      Left = 26
      Top = 457
      Width = 742
      Height = 65
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_03_DadosRedespachoExpedidorBeforePrint
      object RLDraw11: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 66
        Brush.Style = bsClear
      end
      object RLDraw17: TRLDraw
        Left = 370
        Top = 0
        Width = 1
        Height = 64
        Brush.Style = bsClear
      end
      object RLLabel7: TRLLabel
        Left = 4
        Top = 1
        Width = 41
        Height = 8
        Caption = 'EXPEDIDOR'
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
      object rllRazaoResdes: TRLLabel
        Left = 48
        Top = 1
        Width = 318
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRedes1: TRLLabel
        Left = 48
        Top = 9
        Width = 318
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRedes2: TRLLabel
        Left = 48
        Top = 19
        Width = 318
        Height = 10
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMunRedes: TRLLabel
        Left = 48
        Top = 29
        Width = 234
        Height = 19
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCnpjRedes: TRLLabel
        Left = 48
        Top = 39
        Width = 124
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllPaisRedes: TRLLabel
        Left = 48
        Top = 50
        Width = 209
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLLabel81: TRLLabel
        Left = 4
        Top = 9
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
      object RLLabel82: TRLLabel
        Left = 4
        Top = 32
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
      object RLLabel86: TRLLabel
        Left = 4
        Top = 40
        Width = 34
        Height = 8
        Caption = 'CNPJ/CPF'
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
      object RLLabel87: TRLLabel
        Left = 5
        Top = 51
        Width = 17
        Height = 8
        Caption = 'PAIS'
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
      object RLLabel88: TRLLabel
        Left = 174
        Top = 40
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
      object RLLabel89: TRLLabel
        Left = 284
        Top = 29
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
      object rllCEPRedes: TRLLabel
        Left = 301
        Top = 27
        Width = 64
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllInscEstRedes: TRLLabel
        Left = 256
        Top = 37
        Width = 109
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLLabel97: TRLLabel
        Left = 262
        Top = 51
        Width = 20
        Height = 8
        Caption = 'FONE'
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
      object rllFoneRedes: TRLLabel
        Left = 286
        Top = 50
        Width = 77
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllRazaoReceb: TRLLabel
        Left = 432
        Top = 2
        Width = 307
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRecebe1: TRLLabel
        Left = 432
        Top = 10
        Width = 307
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllEnderecoRecebe2: TRLLabel
        Left = 432
        Top = 20
        Width = 307
        Height = 10
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllMunReceb: TRLLabel
        Left = 432
        Top = 30
        Width = 234
        Height = 19
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllCnpjReceb: TRLLabel
        Left = 432
        Top = 40
        Width = 124
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllPaisReceb: TRLLabel
        Left = 432
        Top = 48
        Width = 209
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLLabel80: TRLLabel
        Left = 373
        Top = 10
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
      object RLLabel90: TRLLabel
        Left = 373
        Top = 33
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
      object RLLabel94: TRLLabel
        Left = 373
        Top = 41
        Width = 34
        Height = 8
        Caption = 'CNPJ/CPF'
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
      object RLLabel99: TRLLabel
        Left = 374
        Top = 50
        Width = 17
        Height = 8
        Caption = 'PAIS'
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
      object RLLabel101: TRLLabel
        Left = 558
        Top = 41
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
      object RLLabel102: TRLLabel
        Left = 652
        Top = 30
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
      object rllCEPReceb: TRLLabel
        Left = 669
        Top = 28
        Width = 64
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllInscEstReceb: TRLLabel
        Left = 640
        Top = 38
        Width = 97
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLLabel105: TRLLabel
        Left = 646
        Top = 53
        Width = 20
        Height = 8
        Caption = 'FONE'
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
      object rllFoneReceb: TRLLabel
        Left = 670
        Top = 50
        Width = 68
        Height = 13
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object RLLabel108: TRLLabel
        Left = 373
        Top = 2
        Width = 44
        Height = 8
        Caption = 'RECEBEDOR'
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
  end
end
