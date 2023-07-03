inherited frmDACTeRLRetrato: TfrmDACTeRLRetrato
  Left = 256
  Top = 88
  Caption = 'DACTe - Retrato'
  ClientHeight = 1031
  ClientWidth = 802
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  TextHeight = 10
  inherited RLCTe: TRLReport
    Left = 8
    Top = 0
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    Background.Height = 96
    Background.Width = 175
    DataSource = Datasource1
    Font.Height = -8
    Font.Name = 'Courier New'
    PreviewOptions.FormStyle = fsStayOnTop
    PreviewOptions.ShowModal = True
    PreviewOptions.Caption = 'DACT-e '
    Title = 'DACT-e Retrato'
    BeforePrint = RLCTeBeforePrint
    object rlb_01_Recibo: TRLBand
      Left = 26
      Top = 103
      Width = 742
      Height = 72
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_01_ReciboBeforePrint
      object rllResumoCanhotoCTe: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'RESUMO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        Visible = False
      end
      object RLDraw49: TRLDraw
        Left = 1
        Top = 39
        Width = 201
        Height = 1
        HelpContext = 1
        DrawKind = dkLine
      end
      object RLDraw48: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        DrawKind = dkLine
      end
      object rllSerie2: TRLLabel
        Left = 636
        Top = 49
        Width = 50
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
      object rllNumCTe2: TRLLabel
        Left = 636
        Top = 35
        Width = 86
        Height = 16
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
        Left = 481
        Top = 51
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
        Left = 481
        Top = 27
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
        Top = 19
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
        Left = 616
        Top = 37
        Width = 14
        Height = 12
        Caption = 'N.'
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
        Left = 600
        Top = 50
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
      object rllRecebemosDe: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 10
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO EST' +
          'ADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
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
      object RLLabel136: TRLLabel
        Left = 6
        Top = 16
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
      object RLLabel135: TRLLabel
        Left = 481
        Top = 19
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
      object RLLabel134: TRLLabel
        Left = 481
        Top = 42
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
      object RLLabel133: TRLLabel
        Left = 207
        Top = 56
        Width = 262
        Height = 11
        Alignment = taCenter
        AutoSize = False
        Caption = 'ASSINATURA / CARIMBO'
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
      object RLLabel132: TRLLabel
        Left = 6
        Top = 44
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
      object RLDraw51: TRLDraw
        Left = 203
        Top = 14
        Width = 1
        Height = 56
        Angle = 90.000000000000000000
        Center = False
        DrawKind = dkLine
      end
      object RLDraw52: TRLDraw
        Left = 475
        Top = 14
        Width = 1
        Height = 56
        Angle = 90.000000000000000000
        Center = False
        DrawKind = dkLine
      end
      object RLDraw50: TRLDraw
        Left = 595
        Top = 14
        Width = 1
        Height = 56
        Angle = 90.000000000000000000
        Center = False
        DrawKind = dkLine
      end
      object RLDraw46: TRLDraw
        Left = 1
        Top = 70
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        DrawKind = dkLine
      end
    end
    object rlb_07_HeaderItens: TRLBand
      Left = 26
      Top = 1220
      Width = 742
      Height = 81
      AutoSize = True
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      AfterPrint = rlb_07_HeaderItensAfterPrint
      BeforePrint = rlb_07_HeaderItensBeforePrint
      object RLLabel20: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'DOCUMENTOS ORIGIN'#193'RIOS'
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
      object RLDraw32: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rld_07_headerItens: TRLDraw
        Left = 370
        Top = 14
        Width = 1
        Height = 66
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllTituloSerie1: TRLLabel
        Left = 174
        Top = 17
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
      object rllTituloCNPJ1: TRLLabel
        Left = 88
        Top = 17
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
      object RLLabel91: TRLLabel
        Left = 5
        Top = 17
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
      object RLLabel109: TRLLabel
        Left = 373
        Top = 17
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
      object rllTituloCNPJ2: TRLLabel
        Left = 456
        Top = 17
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
      object rllTituloSerie2: TRLLabel
        Left = 542
        Top = 17
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
      object rlDocOrig_tpDoc1: TRLMemo
        Left = 5
        Top = 27
        Width = 363
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
          
            'COMP 1                        COMP 1                          CO' +
            'MP 1'
          
            'COMP 2                        COMP 2                          CO' +
            'MP 2'
          
            'COMP 3                        COMP 3                          CO' +
            'MP 3'
          
            'COMP 4                        COMP 4                          CO' +
            'MP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlDocOrig_tpDoc2: TRLMemo
        Left = 373
        Top = 27
        Width = 363
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
          
            'COMP 1                        COMP 1                          CO' +
            'MP 1'
          
            'COMP 2                        COMP 2                          CO' +
            'MP 2'
          
            'COMP 3                        COMP 3                          CO' +
            'MP 3'
          
            'COMP 4                        COMP 4                          CO' +
            'MP 4')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_09_Obs: TRLBand
      Left = 26
      Top = 1426
      Width = 742
      Height = 70
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_09_ObsBeforePrint
      object RLDraw1: TRLDraw
        Left = 1
        Top = 18
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmObs: TRLMemo
        Left = 5
        Top = 19
        Width = 730
        Height = 45
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        IntegralHeight = True
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2'
          'OBS LINHA 3'
          'OBS LINHA 4')
        ParentColor = False
        ParentFont = False
      end
      object RLLabel10: TRLLabel
        Left = 6
        Top = 4
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'OBSERVA'#199#213'ES'
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
      object rllMsgTeste: TRLLabel
        Left = 50
        Top = 29
        Width = 640
        Height = 26
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGA'#199#195'O - SEM VALOR FISCAL'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -24
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_02_Cabecalho: TRLBand
      Left = 26
      Top = 291
      Width = 742
      Height = 184
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_02_CabecalhoBeforePrint
      object RLBarcode1: TRLBarcode
        Left = 318
        Top = 122
        Width = 298
        Height = 28
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        AutoSize = False
        BarcodeType = bcCode128C
      end
      object rlsQuadro01: TRLDraw
        Left = 0
        Top = 2
        Width = 741
        Height = 183
        Brush.Style = bsClear
        Visible = False
      end
      object rlsLinhaH02: TRLDraw
        Left = 313
        Top = 57
        Width = 428
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaH03: TRLDraw
        Left = 313
        Top = 88
        Width = 304
        Height = 8
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV01: TRLDraw
        Left = 176
        Top = 120
        Width = 1
        Height = 60
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV04: TRLDraw
        Left = 313
        Top = 2
        Width = 1
        Height = 182
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV05: TRLDraw
        Left = 350
        Top = 32
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV06: TRLDraw
        Left = 374
        Top = 32
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV08: TRLDraw
        Left = 464
        Top = 32
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV09: TRLDraw
        Left = 508
        Top = 32
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV10: TRLDraw
        Left = 616
        Top = 2
        Width = 1
        Height = 31
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rliLogo: TRLImage
        Left = 7
        Top = 34
        Width = 94
        Height = 62
        Center = True
        Scaled = True
      end
      object rlsLinhaH04: TRLDraw
        Left = -6
        Top = 120
        Width = 320
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmEmitente: TRLMemo
        Left = 7
        Top = 3
        Width = 305
        Height = 18
        Alignment = taCenter
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmDadosEmitente: TRLMemo
        Left = 106
        Top = 34
        Width = 206
        Height = 71
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
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
      object RLLabel17: TRLLabel
        Left = 317
        Top = 4
        Width = 298
        Height = 14
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
      object RLLabel18: TRLLabel
        Left = 317
        Top = 19
        Width = 298
        Height = 13
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
      object RLLabel6: TRLLabel
        Left = 640
        Top = 5
        Width = 76
        Height = 13
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
        Left = 627
        Top = 18
        Width = 104
        Height = 14
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
      object RLLabel8: TRLLabel
        Left = 314
        Top = 34
        Width = 32
        Height = 8
        Alignment = taCenter
        AutoSize = False
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
        Left = 315
        Top = 42
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
      object RLLabel21: TRLLabel
        Left = 351
        Top = 34
        Width = 21
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
        Left = 352
        Top = 42
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
      object RLLabel23: TRLLabel
        Left = 378
        Top = 34
        Width = 83
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
        Left = 378
        Top = 42
        Width = 83
        Height = 15
        AutoSize = False
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
      object RLLabel25: TRLLabel
        Left = 474
        Top = 34
        Width = 26
        Height = 8
        Alignment = taCenter
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
      object RLLabel33: TRLLabel
        Left = 510
        Top = 34
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
        Top = 42
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
      object RLLabel74: TRLLabel
        Left = 315
        Top = 92
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
        Left = 315
        Top = 104
        Width = 300
        Height = 14
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
      object rllTipoCte: TRLLabel
        Left = 4
        Top = 137
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
      object rllTipoServico: TRLLabel
        Left = 178
        Top = 137
        Width = 91
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
      object RLLabel28: TRLLabel
        Left = 4
        Top = 156
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
        Left = 4
        Top = 166
        Width = 170
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
      object RLLabel78: TRLLabel
        Left = 178
        Top = 156
        Width = 83
        Height = 8
        Caption = 'FORMA DE PAGAMENTO'
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
      object rllFormaPagamento: TRLLabel
        Left = 178
        Top = 166
        Width = 134
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
        Top = 156
        Width = 55
        Height = 8
        Caption = 'N. PROTOCOLO'
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
        Top = 166
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
      object RLLabel77: TRLLabel
        Left = 618
        Top = 34
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
        Left = 618
        Top = 42
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
        Left = 1
        Top = 154
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllVariavel1: TRLLabel
        Left = 316
        Top = 122
        Width = 298
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Consulta de autenticidade no portal nacional do CT-e, no site da' +
          ' Sefaz'
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
      object rlsLinhaH01: TRLDraw
        Left = 314
        Top = 32
        Width = 426
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw99: TRLDraw
        Left = 314
        Top = 120
        Width = 304
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlbCodigoBarras: TRLBarcode
        Left = 316
        Top = 62
        Width = 298
        Height = 26
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
        Transparent = False
      end
      object rllVariavel2: TRLLabel
        Left = 316
        Top = 134
        Width = 298
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Autorizadora, ou em http://www.cte.fazenda.gov.br/portal'
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
      object RLLabel199: TRLLabel
        Left = 178
        Top = 127
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
      object RLLabel200: TRLLabel
        Left = 4
        Top = 127
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
      object RLSystemInfo1: TRLSystemInfo
        Left = 468
        Top = 42
        Width = 38
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
      object rlsLinhaV07: TRLDraw
        Left = 616
        Top = 32
        Width = 1
        Height = 122
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object imgQRCode: TRLImage
        Left = 618
        Top = 59
        Width = 122
        Height = 94
        Center = True
        Scaled = True
      end
    end
    object rlb_10_ModRodFracionado: TRLBand
      Left = 26
      Top = 1603
      Width = 742
      Height = 44
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_10_ModRodFracionadoBeforePrint
      object rlsQuadro09: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 43
        Brush.Style = bsClear
        Visible = False
      end
      object RLDraw24: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw36: TRLDraw
        Left = 150
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw37: TRLDraw
        Left = 192
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw38: TRLDraw
        Left = 300
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllTituloLotacao: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'DADOS ESPEC'#205'FICOS DO MODAL RODOVI'#193'RIO - CARGA FRACIONADA'
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
      object rllRntrcEmpresa: TRLLabel
        Left = 6
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
      object rllLotacao: TRLLabel
        Left = 162
        Top = 25
        Width = 18
        Height = 12
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
        Top = 25
        Width = 72
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
      object RLLabel85: TRLLabel
        Left = 304
        Top = 26
        Width = 432
        Height = 11
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'ESTE CONHECIMENTO DE TRANSPORTE ATENDE '#192' LEGISLA'#199#195'O DE TRANSPORT' +
          'E RODOVI'#193'RIO EM VIGOR'
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
      object RLLabel84: TRLLabel
        Left = 196
        Top = 17
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
      object RLLabel83: TRLLabel
        Left = 154
        Top = 17
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
      object RLLabel11: TRLLabel
        Left = 6
        Top = 17
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
        Top = 17
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
      object rllCIOT: TRLLabel
        Left = 84
        Top = 25
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
      object rlsCIOT: TRLDraw
        Left = 80
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel216: TRLLabel
        Left = 302
        Top = 17
        Width = 90
        Height = 8
        Caption = 'MSG MODAL RODOVIARIO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        Visible = False
      end
    end
    object rlb_11_ModRodLot103: TRLBand
      Left = 26
      Top = 1647
      Width = 742
      Height = 108
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_11_ModRodLot103BeforePrint
      object RLDraw11: TRLDraw
        Left = 0
        Top = 0
        Width = 740
        Height = 107
        Brush.Style = bsClear
      end
      object RLDraw43: TRLDraw
        Left = 425
        Top = 29
        Width = 1
        Height = 53
        Brush.Style = bsClear
      end
      object RLDraw40: TRLDraw
        Left = 207
        Top = 42
        Width = 532
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw39: TRLDraw
        Left = 1
        Top = 29
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw12: TRLDraw
        Left = 207
        Top = 0
        Width = 1
        Height = 105
        Brush.Style = bsClear
      end
      object RLDraw13: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw14: TRLDraw
        Left = 42
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw31: TRLDraw
        Left = 100
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw33: TRLDraw
        Left = 122
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw42: TRLDraw
        Left = 345
        Top = 13
        Width = 1
        Height = 92
        Brush.Style = bsClear
      end
      object RLDraw44: TRLDraw
        Left = 585
        Top = 13
        Width = 1
        Height = 69
        Brush.Style = bsClear
      end
      object RLDraw41: TRLDraw
        Left = 1
        Top = 82
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object rlmVigencias: TRLMemo
        Left = 348
        Top = 45
        Width = 76
        Height = 35
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Vigencia 1'
          'Vigencia 2'
          'Vigencia 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmUF: TRLMemo
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
      object rlmTipo: TRLMemo
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
      object rlmRNTRC: TRLMemo
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
      object rlmPlaca: TRLMemo
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
      object rlmNumDispositivo: TRLMemo
        Left = 428
        Top = 45
        Width = 156
        Height = 35
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Dispositivo 1'
          'Dispositivo 2'
          'Dispositivo 3')
        ParentColor = False
        ParentFont = False
      end
      object rlmEmpresas: TRLMemo
        Left = 210
        Top = 45
        Width = 133
        Height = 35
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
      object rlmCodTransacao: TRLMemo
        Left = 588
        Top = 45
        Width = 149
        Height = 35
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
      object rllValorTotal: TRLLabel
        Left = 640
        Top = 15
        Width = 95
        Height = 13
        Alignment = taRightJustify
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
      object rllResponsavel: TRLLabel
        Left = 404
        Top = 15
        Width = 60
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
      object rllNumRegEsp: TRLLabel
        Left = 272
        Top = 15
        Width = 59
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
      object rllNomeMotorista: TRLLabel
        Left = 4
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
      object rllLacres: TRLLabel
        Left = 351
        Top = 93
        Width = 36
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
      object rllCPFMotorista: TRLLabel
        Left = 212
        Top = 93
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
      object RLLabel76: TRLLabel
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
      object RLLabel75: TRLLabel
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
      object RLLabel71: TRLLabel
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
      object RLLabel131: TRLLabel
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
      object RLLabel130: TRLLabel
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
      object RLLabel129: TRLLabel
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
      object RLLabel127: TRLLabel
        Left = 588
        Top = 31
        Width = 86
        Height = 8
        Caption = 'C'#211'DIGO DA TRANSA'#199#195'O'
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
      object RLLabel126: TRLLabel
        Left = 428
        Top = 31
        Width = 88
        Height = 8
        Caption = 'N'#218'MERO DO DISPOSITIVO'
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
      object RLLabel124: TRLLabel
        Left = 348
        Top = 31
        Width = 35
        Height = 8
        Caption = 'VIG'#202'NCIA'
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
      object RLLabel122: TRLLabel
        Left = 210
        Top = 31
        Width = 86
        Height = 8
        Caption = 'EMPRESA CREDENCIADA'
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
      object RLLabel121: TRLLabel
        Left = 588
        Top = 15
        Width = 50
        Height = 8
        Caption = 'VALOR TOTAL'
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
      object RLLabel120: TRLLabel
        Left = 348
        Top = 15
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
      object RLLabel118: TRLLabel
        Left = 210
        Top = 15
        Width = 53
        Height = 8
        Caption = 'NRO. REG. ESP.'
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
      object RLLabel117: TRLLabel
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
      object RLLabel115: TRLLabel
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
      object RLLabel112: TRLLabel
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
    end
    object rlb_03_DadosDACTe: TRLBand
      Left = 26
      Top = 475
      Width = 742
      Height = 202
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_03_DadosDACTeBeforePrint
      object rlsLinhaH08: TRLDraw
        Left = 1
        Top = 167
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV11: TRLDraw
        Left = 370
        Top = 27
        Width = 1
        Height = 141
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaH07: TRLDraw
        Left = 1
        Top = 109
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaH06: TRLDraw
        Left = 1
        Top = 51
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaH05: TRLDraw
        Left = 1
        Top = 26
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rllRazaoToma: TRLLabel
        Left = 88
        Top = 169
        Width = 280
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
      object rllRazaoRemet: TRLLabel
        Left = 48
        Top = 54
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
      object rllRazaoReceb: TRLLabel
        Left = 424
        Top = 111
        Width = 310
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
      object rllRazaoExped: TRLLabel
        Left = 48
        Top = 111
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
        Top = 54
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
      object rllPaisToma: TRLLabel
        Left = 520
        Top = 177
        Width = 214
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
        Top = 96
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
      object rllPaisReceb: TRLLabel
        Left = 424
        Top = 153
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
      object rllPaisExped: TRLLabel
        Left = 48
        Top = 153
        Width = 212
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
      object rllPaisDest: TRLLabel
        Left = 432
        Top = 96
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
      object rllOrigPrestacao: TRLLabel
        Left = 4
        Top = 36
        Width = 360
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
      object rllNatOperacao: TRLLabel
        Left = 4
        Top = 11
        Width = 733
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
      object rllMunToma: TRLLabel
        Left = 416
        Top = 169
        Width = 233
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
      object rllMunRemet: TRLLabel
        Left = 48
        Top = 78
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
      object rllMunReceb: TRLLabel
        Left = 424
        Top = 135
        Width = 226
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
      object rllMunExped: TRLLabel
        Left = 48
        Top = 135
        Width = 234
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
      object rllMunDest: TRLLabel
        Left = 432
        Top = 78
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
      object rllInscEstToma: TRLLabel
        Left = 256
        Top = 187
        Width = 111
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
        Top = 87
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
      object rllInscEstReceb: TRLLabel
        Left = 632
        Top = 144
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
      object rllInscEstExped: TRLLabel
        Left = 256
        Top = 144
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
        Top = 87
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
      object rllFoneToma: TRLLabel
        Left = 398
        Top = 187
        Width = 85
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
        Top = 96
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
      object rllFoneReceb: TRLLabel
        Left = 664
        Top = 153
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
      object rllFoneExped: TRLLabel
        Left = 288
        Top = 153
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
        Top = 96
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
      object rllEnderecoToma: TRLLabel
        Left = 48
        Top = 177
        Width = 445
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
        Top = 70
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
        Top = 62
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
      object rllEnderecoReceb2: TRLLabel
        Left = 424
        Top = 127
        Width = 310
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
      object rllEnderecoReceb1: TRLLabel
        Left = 424
        Top = 119
        Width = 310
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
      object rllEnderecoExped2: TRLLabel
        Left = 48
        Top = 127
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
      object rllEnderecoExped1: TRLLabel
        Left = 48
        Top = 119
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
      object rllEnderecoDest2: TRLLabel
        Left = 432
        Top = 70
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
      object rllEnderecoDest1: TRLLabel
        Left = 432
        Top = 62
        Width = 303
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
      object rllDestPrestacao: TRLLabel
        Left = 374
        Top = 36
        Width = 360
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
      object rllCnpjToma: TRLLabel
        Left = 41
        Top = 187
        Width = 130
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
      object rllCnpjRemet: TRLLabel
        Left = 48
        Top = 87
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
      object rllCnpjReceb: TRLLabel
        Left = 424
        Top = 144
        Width = 121
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
      object rllCnpjExped: TRLLabel
        Left = 48
        Top = 144
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
      object rllCnpjDest: TRLLabel
        Left = 432
        Top = 87
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
      object rllCEPToma: TRLLabel
        Left = 670
        Top = 169
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
      object rllCEPRemet: TRLLabel
        Left = 301
        Top = 78
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
      object rllCEPReceb: TRLLabel
        Left = 670
        Top = 135
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
      object rllCEPExped: TRLLabel
        Left = 301
        Top = 135
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
        Top = 78
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
      object RLLabel99: TRLLabel
        Left = 374
        Top = 111
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
      object RLLabel98: TRLLabel
        Left = 284
        Top = 78
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
      object RLLabel97: TRLLabel
        Left = 374
        Top = 170
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
      object RLLabel95: TRLLabel
        Left = 262
        Top = 96
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
      object RLLabel94: TRLLabel
        Left = 653
        Top = 170
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
      object RLLabel93: TRLLabel
        Left = 174
        Top = 87
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
      object RLLabel90: TRLLabel
        Left = 4
        Top = 153
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
      object RLLabel89: TRLLabel
        Left = 4
        Top = 144
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
      object RLLabel88: TRLLabel
        Left = 4
        Top = 135
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
      object RLLabel87: TRLLabel
        Left = 4
        Top = 119
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
      object RLLabel86: TRLLabel
        Left = 4
        Top = 111
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
      object RLLabel82: TRLLabel
        Left = 4
        Top = 187
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
      object RLLabel81: TRLLabel
        Left = 4
        Top = 178
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
      object RLLabel80: TRLLabel
        Left = 4
        Top = 170
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
      object RLLabel79: TRLLabel
        Left = 374
        Top = 96
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
      object RLLabel32: TRLLabel
        Left = 374
        Top = 87
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
      object RLLabel31: TRLLabel
        Left = 374
        Top = 78
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
      object RLLabel30: TRLLabel
        Left = 374
        Top = 62
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
      object RLLabel29: TRLLabel
        Left = 4
        Top = 2
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
      object RLLabel27: TRLLabel
        Left = 374
        Top = 54
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
      object RLLabel26: TRLLabel
        Left = 4
        Top = 96
        Width = 17
        Height = 8
        Caption = 'PA'#205'S'
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
      object RLLabel24: TRLLabel
        Left = 4
        Top = 87
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
      object RLLabel22: TRLLabel
        Left = 4
        Top = 78
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
      object RLLabel16: TRLLabel
        Left = 4
        Top = 62
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
      object RLLabel14: TRLLabel
        Left = 374
        Top = 28
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
      object RLLabel13: TRLLabel
        Left = 4
        Top = 54
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
      object RLLabel128: TRLLabel
        Left = 653
        Top = 135
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
      object RLLabel125: TRLLabel
        Left = 640
        Top = 153
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
      object RLLabel123: TRLLabel
        Left = 551
        Top = 144
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
      object RLLabel12: TRLLabel
        Left = 4
        Top = 28
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
      object RLLabel119: TRLLabel
        Left = 660
        Top = 78
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
      object RLLabel116: TRLLabel
        Left = 640
        Top = 96
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
      object RLLabel114: TRLLabel
        Left = 551
        Top = 87
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
      object RLLabel113: TRLLabel
        Left = 500
        Top = 178
        Width = 17
        Height = 8
        Caption = 'PA'#205'S'
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
      object RLLabel111: TRLLabel
        Left = 374
        Top = 187
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
      object RLLabel110: TRLLabel
        Left = 284
        Top = 135
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
      object RLLabel108: TRLLabel
        Left = 174
        Top = 187
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
      object RLLabel107: TRLLabel
        Left = 262
        Top = 153
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
      object RLLabel105: TRLLabel
        Left = 174
        Top = 144
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
      object RLLabel104: TRLLabel
        Left = 374
        Top = 153
        Width = 17
        Height = 8
        Caption = 'PA'#205'S'
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
      object RLLabel103: TRLLabel
        Left = 374
        Top = 144
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
      object RLLabel102: TRLLabel
        Left = 374
        Top = 135
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
      object RLLabel101: TRLLabel
        Left = 374
        Top = 119
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
    end
    object rlb_04_DadosNotaFiscal: TRLBand
      Left = 26
      Top = 746
      Width = 742
      Height = 90
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      IntegralHeight = False
      ParentColor = False
      BeforePrint = rlb_04_DadosNotaFiscalBeforePrint
      object RLLabel5: TRLLabel
        Left = 416
        Top = 29
        Width = 84
        Height = 8
        AutoSize = False
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
      object rlmNomeSeguradora: TRLMemo
        Left = 417
        Top = 40
        Width = 315
        Height = 19
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Seguradora 1')
        ParentColor = False
        ParentFont = False
      end
      object rlmRespSeguroMerc: TRLMemo
        Left = 416
        Top = 73
        Width = 105
        Height = 14
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Resp Seguro 1')
        ParentColor = False
        ParentFont = False
      end
      object RLLabel39: TRLLabel
        Left = 526
        Top = 64
        Width = 75
        Height = 8
        AutoSize = False
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
      object rlmNroApolice: TRLMemo
        Left = 528
        Top = 73
        Width = 99
        Height = 14
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Nro Apolice 1')
        ParentColor = False
        ParentFont = False
      end
      object RLLabel40: TRLLabel
        Left = 632
        Top = 64
        Width = 90
        Height = 8
        AutoSize = False
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
      object rlmNroAverbacao: TRLMemo
        Left = 634
        Top = 73
        Width = 99
        Height = 14
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Nro Averbacao 1')
        ParentColor = False
        ParentFont = False
      end
      object RLLabel37: TRLLabel
        Left = 419
        Top = 65
        Width = 51
        Height = 8
        AutoSize = False
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
      object RLDraw9: TRLDraw
        Left = 283
        Top = 1
        Width = 1
        Height = 25
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw8: TRLDraw
        Left = 632
        Top = 59
        Width = 1
        Height = 30
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw7: TRLDraw
        Left = 526
        Top = 59
        Width = 1
        Height = 30
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw62: TRLDraw
        Left = 414
        Top = 59
        Width = 326
        Height = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw61: TRLDraw
        Left = 414
        Top = 27
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw60: TRLDraw
        Left = 324
        Top = 27
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw59: TRLDraw
        Left = 164
        Top = 27
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw58: TRLDraw
        Left = 84
        Top = 27
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw56: TRLDraw
        Left = 540
        Top = 1
        Width = 1
        Height = 25
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw55: TRLDraw
        Left = 1
        Top = 26
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmQtdUnidMedida5: TRLMemo
        Left = 328
        Top = 38
        Width = 84
        Height = 15
        Alignment = taRightJustify
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
      object rlmQtdUnidMedida3: TRLMemo
        Left = 166
        Top = 38
        Width = 76
        Height = 15
        Alignment = taRightJustify
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
      object rlmQtdUnidMedida2: TRLMemo
        Left = 86
        Top = 38
        Width = 76
        Height = 15
        Alignment = taRightJustify
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
      object rlmQtdUnidMedida1: TRLMemo
        Left = 5
        Top = 38
        Width = 76
        Height = 15
        Alignment = taRightJustify
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
      object rllVlrTotalMerc: TRLLabel
        Left = 549
        Top = 13
        Width = 185
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
      object rllProdPredominante: TRLLabel
        Left = 4
        Top = 13
        Width = 275
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
        Left = 287
        Top = 13
        Width = 249
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
      object RLLabel43: TRLLabel
        Left = 328
        Top = 29
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
      object RLLabel41: TRLLabel
        Left = 166
        Top = 29
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
      object RLLabel4: TRLLabel
        Left = 286
        Top = 3
        Width = 135
        Height = 8
        AutoSize = False
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
      object RLLabel36: TRLLabel
        Left = 86
        Top = 29
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
      object RLLabel35: TRLLabel
        Left = 5
        Top = 29
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
      object RLLabel34: TRLLabel
        Left = 546
        Top = 3
        Width = 111
        Height = 8
        AutoSize = False
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
      object RLLabel1: TRLLabel
        Left = 4
        Top = 3
        Width = 91
        Height = 8
        AutoSize = False
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
        Top = 38
        Width = 76
        Height = 15
        Alignment = taRightJustify
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
      object RLLabel73: TRLLabel
        Left = 246
        Top = 29
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
        Top = 27
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
    end
    object rlb_05_Complemento: TRLBand
      Left = 26
      Top = 836
      Width = 742
      Height = 81
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_05_ComplementoBeforePrint
      object RLDraw6: TRLDraw
        Left = 372
        Top = 16
        Width = 1
        Height = 64
        Brush.Style = bsClear
      end
      object RLDraw5: TRLDraw
        Left = 1
        Top = 16
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel64: TRLLabel
        Left = 645
        Top = 19
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
      object RLLabel63: TRLLabel
        Left = 377
        Top = 19
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
      object RLLabel62: TRLLabel
        Left = 280
        Top = 19
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
      object RLLabel61: TRLLabel
        Left = 5
        Top = 19
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
      object RLLabel59: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'CT-e COMPLEMENTADO'
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
      object rlmComplChave1: TRLMemo
        Left = 5
        Top = 28
        Width = 269
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
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmComplValor1: TRLMemo
        Left = 280
        Top = 28
        Width = 89
        Height = 48
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
        Top = 28
        Width = 264
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
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmComplValor2: TRLMemo
        Left = 645
        Top = 28
        Width = 89
        Height = 48
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
    end
    object rlb_17_Sistema: TRLBand
      Left = 26
      Top = 2134
      Width = 742
      Height = 16
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_17_SistemaBeforePrint
      object RLLabel15: TRLLabel
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
    object rlb_16_DadosExcEmitente: TRLBand
      Left = 26
      Top = 2065
      Width = 742
      Height = 69
      AutoSize = True
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_16_DadosExcEmitenteBeforePrint
      object RLLabel165: TRLLabel
        Left = 566
        Top = 4
        Width = 102
        Height = 12
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
      object RLLabel7: TRLLabel
        Left = 142
        Top = 4
        Width = 171
        Height = 12
        Caption = 'USO EXCLUSIVO DO EMISSOR DO CT-E'
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
      object rlmObsExcEmitente: TRLMemo
        Left = 5
        Top = 17
        Width = 492
        Height = 49
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        IntegralHeight = True
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2')
        ParentColor = False
        ParentFont = False
      end
      object RLDraw27: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw3: TRLDraw
        Left = 500
        Top = 1
        Width = 1
        Height = 67
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmObsFisco: TRLMemo
        Left = 509
        Top = 17
        Width = 228
        Height = 49
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        IntegralHeight = True
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_06_ValorPrestacao: TRLBand
      Left = 26
      Top = 1077
      Width = 742
      Height = 143
      AutoExpand = False
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_ValorPrestacaoBeforePrint
      object RLDraw16: TRLDraw
        Left = 557
        Top = 47
        Width = 184
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw17: TRLDraw
        Left = 1
        Top = 16
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw18: TRLDraw
        Left = 372
        Top = 16
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw19: TRLDraw
        Left = 556
        Top = 16
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw20: TRLDraw
        Left = 448
        Top = 93
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw22: TRLDraw
        Left = 346
        Top = 93
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw23: TRLDraw
        Left = 500
        Top = 93
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw25: TRLDraw
        Left = 586
        Top = 93
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw26: TRLDraw
        Left = 650
        Top = 93
        Width = 1
        Height = 26
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw45: TRLDraw
        Left = 1
        Top = 78
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw15: TRLDraw
        Left = 186
        Top = 16
        Width = 1
        Height = 62
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw21: TRLDraw
        Left = 1
        Top = 93
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmCompValor3: TRLMemo
        Left = 476
        Top = 28
        Width = 78
        Height = 50
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
      object rlmCompValor2: TRLMemo
        Left = 290
        Top = 28
        Width = 78
        Height = 50
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
      object rlmCompValor1: TRLMemo
        Left = 104
        Top = 28
        Width = 78
        Height = 50
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
      object rlmCompNome3: TRLMemo
        Left = 377
        Top = 28
        Width = 96
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
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompNome2: TRLMemo
        Left = 190
        Top = 28
        Width = 96
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
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rlmCompNome1: TRLMemo
        Left = 5
        Top = 28
        Width = 96
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
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4')
        ParentColor = False
        ParentFont = False
      end
      object rllVlrTotServico: TRLLabel
        Left = 570
        Top = 29
        Width = 164
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
        Left = 570
        Top = 61
        Width = 164
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
      object rllVlrICMS: TRLLabel
        Left = 504
        Top = 102
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
      object rllSitTrib: TRLLabel
        Left = 3
        Top = 102
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
      object rllRedBaseCalc: TRLLabel
        Left = 590
        Top = 102
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
        Top = 102
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
      object rllBaseCalc: TRLLabel
        Left = 350
        Top = 102
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
        Top = 102
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
      object RLLabel58: TRLLabel
        Left = 656
        Top = 95
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
      object RLLabel56: TRLLabel
        Left = 454
        Top = 95
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
      object RLLabel55: TRLLabel
        Left = 350
        Top = 95
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
      object RLLabel54: TRLLabel
        Left = 504
        Top = 95
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
      object RLLabel53: TRLLabel
        Left = 590
        Top = 95
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
      object RLLabel52: TRLLabel
        Left = 3
        Top = 95
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
      object RLLabel51: TRLLabel
        Left = 8
        Top = 79
        Width = 728
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES RELATIVAS AO IMPOSTO'
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
      object RLLabel50: TRLLabel
        Left = 560
        Top = 49
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
      object RLLabel49: TRLLabel
        Left = 560
        Top = 19
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
      object RLLabel48: TRLLabel
        Left = 528
        Top = 19
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
      object RLLabel47: TRLLabel
        Left = 377
        Top = 19
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
      object RLLabel46: TRLLabel
        Left = 156
        Top = 19
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
      object RLLabel45: TRLLabel
        Left = 342
        Top = 19
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
      object RLLabel44: TRLLabel
        Left = 5
        Top = 21
        Width = 22
        Height = 6
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
      object RLLabel42: TRLLabel
        Left = 190
        Top = 19
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
      object RLLabel38: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'COMPONENTES DO VALOR DA PRESTA'#199#195'O DE SERVI'#199'O'
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
      object rlpnlTributosFederais: TRLPanel
        Left = 1
        Top = 117
        Width = 740
        Height = 25
        Align = faBottom
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = False
        object RLDraw101: TRLDraw
          Left = 150
          Top = 1
          Width = 4
          Height = 24
          Align = faHeight
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object RLDraw114: TRLDraw
          Left = 448
          Top = 1
          Width = 1
          Height = 24
          Align = faHeight
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object RLDraw115: TRLDraw
          Left = 300
          Top = 1
          Width = 4
          Height = 24
          Align = faHeight
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object RLDraw116: TRLDraw
          Left = 586
          Top = 1
          Width = 1
          Height = 24
          Align = faHeight
          Angle = 90.000000000000000000
          Brush.Style = bsClear
          DrawKind = dkLine
        end
        object RLLabel220: TRLLabel
          Left = 211
          Top = 2
          Width = 53
          Height = 8
          Caption = 'VALOR COFINS'
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
        object rlblVlrCOFINS: TRLLabel
          Left = 189
          Top = 10
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
        object RLLabel226: TRLLabel
          Left = 319
          Top = 2
          Width = 107
          Height = 8
          Caption = 'VALOR DO IMPOSTO DE RENDA'
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
        object rlblVlrIR: TRLLabel
          Left = 338
          Top = 10
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
        object RLLabel228: TRLLabel
          Left = 498
          Top = 2
          Width = 55
          Height = 8
          Caption = 'VALOR DO INSS'
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
        object rlblVlrINSS: TRLLabel
          Left = 476
          Top = 10
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
        object RLLabel232: TRLLabel
          Left = 650
          Top = 2
          Width = 57
          Height = 8
          Caption = 'VALOR DO CSLL'
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
        object rlblVlrCSLL: TRLLabel
          Left = 628
          Top = 10
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
        object RLLabel2: TRLLabel
          Left = 57
          Top = 2
          Width = 50
          Height = 8
          Caption = 'VALOR DO PIS'
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
        object rlblVlrPIS: TRLLabel
          Left = 35
          Top = 10
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
      end
    end
    object rlb_12_ModAereo: TRLBand
      Left = 26
      Top = 1862
      Width = 742
      Height = 97
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_12_ModAereoBeforePrint
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
        Align = faWidth
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
        Top = 80
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
      object RLLabel157: TRLLabel
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
      object RLLabel156: TRLLabel
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
      object RLLabel155: TRLLabel
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
      object RLLabel154: TRLLabel
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
      object RLLabel153: TRLLabel
        Left = 8
        Top = 1
        Width = 730
        Height = 11
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
      object RLLabel150: TRLLabel
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
      object RLLabel149: TRLLabel
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
      object RLLabel148: TRLLabel
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
      object RLLabel147: TRLLabel
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
      object RLLabel146: TRLLabel
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
      object RLLabel145: TRLLabel
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
      object RLLabel144: TRLLabel
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
      object RLLabel142: TRLLabel
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
      object RLLabel141: TRLLabel
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
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLDraw63: TRLDraw
        Left = 1
        Top = 70
        Width = 740
        Height = 1
        Align = faWidth
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
      Top = 1959
      Width = 742
      Height = 92
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_13_ModAquaviarioBeforePrint
      object RLLabel151: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 12
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
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel152: TRLLabel
        Left = 6
        Top = 17
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
        Top = 26
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
      object RLLabel158: TRLLabel
        Left = 406
        Top = 17
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
        Top = 26
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
        Top = 39
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel159: TRLLabel
        Left = 6
        Top = 41
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
        Top = 50
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
        Top = 63
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel160: TRLLabel
        Left = 6
        Top = 65
        Width = 108
        Height = 8
        Caption = 'IDENTIFICA'#199#195'O DA(S) BALSA(S)'
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
        Left = 406
        Top = 74
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
        Top = 16
        Width = 1
        Height = 73
        Brush.Style = bsClear
      end
      object RLLabel162: TRLLabel
        Left = 406
        Top = 41
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
        Top = 50
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
      object RLLabel164: TRLLabel
        Left = 518
        Top = 41
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
        Top = 50
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
      object RLLabel166: TRLLabel
        Left = 614
        Top = 41
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
        Top = 50
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
      object RLLabel168: TRLLabel
        Left = 694
        Top = 41
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
        Top = 50
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
        Top = 40
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw79: TRLDraw
        Left = 610
        Top = 40
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLDraw80: TRLDraw
        Left = 514
        Top = 40
        Width = 1
        Height = 24
        Brush.Style = bsClear
      end
      object RLLabel178: TRLLabel
        Left = 406
        Top = 65
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
      object rllIndBalsas: TRLLabel
        Left = 6
        Top = 74
        Width = 49
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
    end
    object rlb_14_ModFerroviario: TRLBand
      Left = 26
      Top = 2051
      Width = 742
      Height = 7
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_14_ModFerroviarioBeforePrint
    end
    object rlb_15_ModDutoviario: TRLBand
      Left = 26
      Top = 2058
      Width = 742
      Height = 7
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_15_ModDutoviarioBeforePrint
    end
    object rlb_01_Recibo_Aereo: TRLBand
      Left = 26
      Top = 175
      Width = 742
      Height = 116
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_01_Recibo_AereoBeforePrint
      object RLDraw81: TRLDraw
        Left = 1
        Top = 112
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw2: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel70: TRLLabel
        Left = 6
        Top = 94
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
      object RLLabel66: TRLLabel
        Left = 6
        Top = 62
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
      object RLLabel65: TRLLabel
        Left = 6
        Top = 2
        Width = 730
        Height = 11
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
      object RLDraw82: TRLDraw
        Left = 1
        Top = 61
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel57: TRLLabel
        Left = 121
        Top = 52
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
      object RLLabel60: TRLLabel
        Left = 508
        Top = 52
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
      object RLLabel69: TRLLabel
        Left = 206
        Top = 62
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
      object RLLabel161: TRLLabel
        Left = 206
        Top = 94
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
      object RLLabel67: TRLLabel
        Left = 374
        Top = 62
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
      object RLLabel68: TRLLabel
        Left = 374
        Top = 94
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
      object RLLabel72: TRLLabel
        Left = 574
        Top = 62
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
      object RLLabel163: TRLLabel
        Left = 574
        Top = 94
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
      object RLDraw53: TRLDraw
        Left = 367
        Top = 61
        Width = 1
        Height = 52
        HelpContext = 1
        Brush.Style = bsClear
        Color = clDefault
        DrawKind = dkLine
        ParentColor = False
        Transparent = False
      end
      object RLMemo1: TRLMemo
        Left = 2
        Top = 19
        Width = 736
        Height = 30
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          
            'O Transporte coberto por este conhecimento se rege pelo c'#243'digo B' +
            'rasileiro de Aeron'#225'utica (Lei 7.565 de 19/12/1986), especificame' +
            'nte pelas regras relativas a responsabilidade Civil prevista nos' +
            ' artigos 193, 241, 244, 262 e 264, de cujo teor o Expedidor / Re' +
            'metente declara concordar e ter plena ci'#234'ncia. O Expedidor / Rem' +
            'etente aceita como corretas todas as especifica'#231#245'es impressas, m' +
            'anuscritas, datilografadas ou carimbadas neste conhecimento, cer' +
            'tificando que os artigos perigosos descritos pela regulamenta'#231#227'o' +
            ' da I.C.A.O. foram devidamente informados e acondicionados para ' +
            'transporte A'#233'reo.')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLDraw10: TRLDraw
        Left = 1
        Top = 50
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
    end
    object rlb_11_ModRodLot104: TRLBand
      Left = 26
      Top = 1755
      Width = 742
      Height = 107
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      AfterPrint = rlb_11_ModRodLot104AfterPrint
      BeforePrint = rlb_11_ModRodLot104BeforePrint
      object RLDraw4: TRLDraw
        Left = 0
        Top = 1
        Width = 740
        Height = 104
        Brush.Style = bsClear
      end
      object RLDraw30: TRLDraw
        Left = 207
        Top = 1
        Width = 1
        Height = 79
        Brush.Style = bsClear
      end
      object RLDraw83: TRLDraw
        Left = 1
        Top = 15
        Width = 739
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw84: TRLDraw
        Left = 42
        Top = 14
        Width = 1
        Height = 66
        Brush.Style = bsClear
      end
      object RLDraw85: TRLDraw
        Left = 100
        Top = 14
        Width = 1
        Height = 66
        Brush.Style = bsClear
      end
      object RLDraw86: TRLDraw
        Left = 122
        Top = 14
        Width = 1
        Height = 66
        Brush.Style = bsClear
      end
      object RLDraw87: TRLDraw
        Left = 1
        Top = 26
        Width = 739
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw89: TRLDraw
        Left = 1
        Top = 79
        Width = 739
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLDraw90: TRLDraw
        Left = 345
        Top = 79
        Width = 1
        Height = 25
        Brush.Style = bsClear
      end
      object RLDraw92: TRLDraw
        Left = 330
        Top = 14
        Width = 1
        Height = 66
        Brush.Style = bsClear
      end
      object RLLabel167: TRLLabel
        Left = 2
        Top = 17
        Width = 17
        Height = 8
        Caption = 'TIPO'
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
      object RLLabel169: TRLLabel
        Left = 214
        Top = 2
        Width = 524
        Height = 12
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
      object RLLabel170: TRLLabel
        Left = 2
        Top = 2
        Width = 202
        Height = 12
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
      object RLLabel171: TRLLabel
        Left = 351
        Top = 82
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
      object RLLabel172: TRLLabel
        Left = 271
        Top = 82
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
      object RLLabel173: TRLLabel
        Left = 4
        Top = 82
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
      object RLLabel174: TRLLabel
        Left = 334
        Top = 17
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
      object RLLabel177: TRLLabel
        Left = 618
        Top = 17
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
      object RLLabel179: TRLLabel
        Left = 210
        Top = 17
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
      object RLLabel181: TRLLabel
        Left = 124
        Top = 17
        Width = 26
        Height = 8
        Caption = 'RNTRC'
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
      object RLLabel182: TRLLabel
        Left = 102
        Top = 17
        Width = 11
        Height = 8
        Caption = 'UF'
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
      object RLLabel183: TRLLabel
        Left = 44
        Top = 17
        Width = 25
        Height = 8
        Caption = 'PLACA'
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
      object rlmUF2: TRLMemo
        Left = 102
        Top = 28
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
        Top = 28
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
        Top = 28
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
        Top = 28
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
        Top = 28
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
        Top = 28
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
        Top = 91
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
        Top = 91
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
        Left = 272
        Top = 91
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
        Top = 15
        Width = 1
        Height = 65
        Brush.Style = bsClear
      end
      object rlmCNPJPg: TRLMemo
        Left = 618
        Top = 28
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
      object RLDraw28: TRLDraw
        Left = 267
        Top = 79
        Width = 1
        Height = 26
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        DrawKind = dkLine
      end
    end
    object rlb_06_ProdutosPerigosos: TRLBand
      Left = 26
      Top = 917
      Width = 742
      Height = 83
      AutoSize = True
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_ProdutosPerigososBeforePrint
      object RLLabel192: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES SOBRE OS PRODUTOS PERIGOSOS'
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
      object RLDraw102: TRLDraw
        Left = 1
        Top = 16
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel193: TRLLabel
        Left = 10
        Top = 19
        Width = 36
        Height = 8
        Caption = 'NRO. ONU'
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
      object RLLabel194: TRLLabel
        Left = 83
        Top = 19
        Width = 69
        Height = 8
        Caption = 'NOME APROPRIADO'
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
      object RLLabel195: TRLLabel
        Left = 310
        Top = 19
        Width = 145
        Height = 8
        Caption = 'CLASSE/SUBCLASSE E RISCO SUBSIDI'#193'RIO'
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
      object RLLabel196: TRLLabel
        Left = 510
        Top = 19
        Width = 83
        Height = 8
        Caption = 'GRUPO DE EMBALAGEM'
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
      object RLLabel197: TRLLabel
        Left = 625
        Top = 19
        Width = 79
        Height = 8
        Caption = 'QTDE TOTAL PRODUTO'
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
      object RLDraw103: TRLDraw
        Left = 80
        Top = 16
        Width = 1
        Height = 15
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw104: TRLDraw
        Left = 300
        Top = 16
        Width = 1
        Height = 14
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw105: TRLDraw
        Left = 500
        Top = 16
        Width = 1
        Height = 15
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw106: TRLDraw
        Left = 620
        Top = 16
        Width = 1
        Height = 15
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlmNumONU: TRLMemo
        Left = 0
        Top = 30
        Width = 81
        Height = 52
        Alignment = taCenter
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'NUM ONU'
          ''
          ''
          '')
        ParentColor = False
        ParentFont = False
      end
      object rlmNomeApropriado: TRLMemo
        Left = 80
        Top = 30
        Width = 221
        Height = 52
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Nome Apropriado'
          ''
          ''
          '')
        ParentColor = False
        ParentFont = False
      end
      object rlmClasse: TRLMemo
        Left = 300
        Top = 30
        Width = 201
        Height = 52
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Classe'
          ''
          ''
          '')
        ParentColor = False
        ParentFont = False
      end
      object rlmGrupoEmbalagem: TRLMemo
        Left = 500
        Top = 30
        Width = 121
        Height = 52
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Grupo de Embalagem'
          ''
          ''
          '')
        ParentColor = False
        ParentFont = False
      end
      object rlmQtdeProduto: TRLMemo
        Left = 620
        Top = 30
        Width = 122
        Height = 52
        Behavior = [beSiteExpander]
        Borders.Sides = sdCustom
        Borders.DrawLeft = True
        Borders.DrawTop = True
        Borders.DrawRight = True
        Borders.DrawBottom = True
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Quantidade'
          ''
          ''
          '')
        ParentColor = False
        ParentFont = False
      end
      object RLDraw107: TRLDraw
        Left = 1
        Top = 30
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
    end
    object rlb_06_VeiculosNovos: TRLBand
      Left = 26
      Top = 1496
      Width = 742
      Height = 63
      AutoSize = True
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_VeiculosNovosBeforePrint
      object RLLabel222: TRLLabel
        Left = 6
        Top = 3
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES SOBRE OS VE'#205'CULOS NOVOS TRANSPORTADOS'
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
      object RLDraw229: TRLDraw
        Left = 1
        Top = 16
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
      end
      object RLLabel229: TRLLabel
        Left = 5
        Top = 19
        Width = 27
        Height = 8
        Caption = 'CHASSI'
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
      object RLLabel231: TRLLabel
        Left = 128
        Top = 19
        Width = 17
        Height = 8
        Caption = 'COR'
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
      object RLLabel242: TRLLabel
        Left = 337
        Top = 19
        Width = 59
        Height = 8
        Caption = 'MARCA/MODELO'
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
      object RLLabel243: TRLLabel
        Left = 510
        Top = 19
        Width = 78
        Height = 8
        Caption = 'VR. UNIT. DO VE'#205'CULO'
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
      object RLLabel244: TRLLabel
        Left = 625
        Top = 19
        Width = 58
        Height = 8
        Caption = 'FRETE UNITARIO'
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
      object RLDraw324: TRLDraw
        Left = 123
        Top = 16
        Width = 1
        Height = 46
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw335: TRLDraw
        Left = 332
        Top = 16
        Width = 1
        Height = 46
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw310: TRLDraw
        Left = 500
        Top = 16
        Width = 1
        Height = 46
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw411: TRLDraw
        Left = 620
        Top = 16
        Width = 1
        Height = 46
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object CHASSI: TRLMemo
        Left = 5
        Top = 33
        Width = 116
        Height = 16
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'CHASSI')
        ParentColor = False
        ParentFont = False
      end
      object COR: TRLMemo
        Left = 128
        Top = 33
        Width = 201
        Height = 16
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'COR')
        ParentColor = False
        ParentFont = False
      end
      object MODELO: TRLMemo
        Left = 336
        Top = 33
        Width = 161
        Height = 16
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'MODELO')
        ParentColor = False
        ParentFont = False
      end
      object VUNIT: TRLMemo
        Left = 509
        Top = 33
        Width = 108
        Height = 16
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'VUNIT')
        ParentColor = False
        ParentFont = False
      end
      object VFRETE: TRLMemo
        Left = 625
        Top = 33
        Width = 112
        Height = 16
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'VFRETE')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_Fluxo_Carga: TRLBand
      Left = 26
      Top = 1382
      Width = 742
      Height = 44
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      BeforePrint = rlb_Fluxo_CargaBeforePrint
      object RLLabel201: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'PREVIS'#195'O DO FLUXO DA CARGA'
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
      object RLDraw34: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLLabel202: TRLLabel
        Left = 2
        Top = 17
        Width = 234
        Height = 8
        Caption = 'SIGLA/C'#211'D. INT. DA FILIAL/PORTO/ESTA'#199#195'O/AEROPORTO DE ORIGEM'
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object RLLabel203: TRLLabel
        Left = 246
        Top = 16
        Width = 244
        Height = 8
        Caption = 'SIGLA/C'#211'D. INT. DA FILIAL/PORTO/ESTA'#199#195'O/AEROPORTO DE PASSAGEM'
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
      object RLLabel204: TRLLabel
        Left = 496
        Top = 17
        Width = 236
        Height = 8
        Caption = 'SIGLA/C'#211'D. INT. DA FILIAL/PORTO/ESTA'#199#195'O/AEROPORTO DE DESTINO'
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
      object RLDraw110: TRLDraw
        Left = 494
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object RLDraw111: TRLDraw
        Left = 241
        Top = 15
        Width = 1
        Height = 27
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object rllSiglaOrigem: TRLLabel
        Left = 8
        Top = 26
        Width = 59
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
      object rllSiglaDestino: TRLLabel
        Left = 502
        Top = 26
        Width = 62
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
      object rllSiglaPassagem: TRLMemo
        Left = 248
        Top = 26
        Width = 188
        Height = 12
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Pass')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_03_DadosDACTe_OS: TRLBand
      Left = 26
      Top = 677
      Width = 742
      Height = 69
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_03_DadosDACTe_OSBeforePrint
      object rlsQuadro3: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 68
        Brush.Style = bsClear
        Visible = False
      end
      object rlsLinhaH7: TRLDraw
        Left = 1
        Top = 28
        Width = 740
        Height = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object RLLabel205: TRLLabel
        Left = 4
        Top = 3
        Width = 78
        Height = 8
        Caption = 'IN'#205'CIO DA PRESTA'#199#195'O'
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
      object RLLabel206: TRLLabel
        Left = 504
        Top = 3
        Width = 88
        Height = 8
        Caption = 'T'#201'RMINO DA PRESTA'#199#195'O'
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
      object RLLabel207: TRLLabel
        Left = 240
        Top = 3
        Width = 83
        Height = 8
        Caption = 'PERCURSO DO VE'#205'CULO'
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
      object rlsLinhaV12: TRLDraw
        Left = 235
        Top = 0
        Width = 1
        Height = 29
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlsLinhaV13: TRLDraw
        Left = 500
        Top = 0
        Width = 1
        Height = 29
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object rllOrigPrestacao1: TRLLabel
        Left = 3
        Top = 11
        Width = 228
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
      object rllDestPrestacao1: TRLLabel
        Left = 504
        Top = 11
        Width = 233
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
      object rllPercursoVeiculo: TRLLabel
        Left = 238
        Top = 11
        Width = 260
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
      object rllCEPToma1: TRLLabel
        Left = 673
        Top = 31
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
      object RLLabel208: TRLLabel
        Left = 653
        Top = 32
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
      object rllMunToma1: TRLLabel
        Left = 416
        Top = 31
        Width = 233
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
      object RLLabel209: TRLLabel
        Left = 374
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
      object RLLabel210: TRLLabel
        Left = 4
        Top = 32
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
      object rllRazaoToma1: TRLLabel
        Left = 89
        Top = 31
        Width = 280
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
      object rllPaisToma1: TRLLabel
        Left = 520
        Top = 40
        Width = 214
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
      object RLLabel211: TRLLabel
        Left = 500
        Top = 43
        Width = 17
        Height = 8
        Caption = 'PA'#205'S'
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
      object rllEnderecoToma1: TRLLabel
        Left = 48
        Top = 40
        Width = 445
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
      object RLLabel212: TRLLabel
        Left = 4
        Top = 43
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
      object rllFoneToma1: TRLLabel
        Left = 402
        Top = 50
        Width = 85
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
      object RLLabel213: TRLLabel
        Left = 378
        Top = 55
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
      object rllInscEstToma1: TRLLabel
        Left = 260
        Top = 50
        Width = 111
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
      object RLLabel214: TRLLabel
        Left = 178
        Top = 55
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
      object rllCnpjToma1: TRLLabel
        Left = 40
        Top = 50
        Width = 130
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
      object RLLabel215: TRLLabel
        Left = 3
        Top = 55
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
    end
    object rlb_Dados_Seguradora: TRLBand
      Left = 26
      Top = 1559
      Width = 742
      Height = 44
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_Dados_SeguradoraBeforePrint
      object RLDraw29: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object rllTituloSeguro: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'SEGURO DA VIAGEM'
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
      object RLDraw112: TRLDraw
        Left = 246
        Top = 15
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object RLDraw113: TRLDraw
        Left = 492
        Top = 16
        Width = 1
        Height = 28
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
        Transparent = False
      end
      object RLLabel217: TRLLabel
        Left = 8
        Top = 17
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
      object RLLabel218: TRLLabel
        Left = 252
        Top = 17
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
      object RLLabel219: TRLLabel
        Left = 500
        Top = 17
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
      object rllResponsavelSeguro: TRLMemo
        Left = 9
        Top = 25
        Width = 224
        Height = 12
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Resp Seguro')
        ParentColor = False
        ParentFont = False
      end
      object rllNomeSeguradora: TRLMemo
        Left = 252
        Top = 25
        Width = 224
        Height = 12
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Nome Seguradora')
        ParentColor = False
        ParentFont = False
      end
      object rllApolice: TRLMemo
        Left = 501
        Top = 25
        Width = 224
        Height = 12
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'Apolice')
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_CTeOS_PrestacaoServico: TRLBand
      Left = 26
      Top = 1000
      Width = 742
      Height = 77
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_CTeOS_PrestacaoServicoBeforePrint
      object rlDocOrig_tpDoc3: TRLMemo
        Left = 3
        Top = 26
        Width = 737
        Height = 47
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          'QTD 1                           DESC 1                          '
          'QTD 2                           DESC 2                     '
          'QTD 3                           DESC 3                         '
          'QTD 4                           DESC 4                          ')
        ParentColor = False
        ParentFont = False
      end
      object RLLabel221: TRLLabel
        Left = 5
        Top = 17
        Width = 47
        Height = 8
        Caption = 'QUANTIDADE'
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
      object RLLabel223: TRLLabel
        Left = 84
        Top = 17
        Width = 136
        Height = 8
        Caption = 'DESCRI'#199#195'O DOS SERVI'#199'OS PRESTADOS'
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
      object RLLabel224: TRLLabel
        Left = 7
        Top = 1
        Width = 732
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMA'#199#213'ES DA PRESTA'#199#195'O DO SERVI'#199'O'
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
      object RLDraw35: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
    end
    object rlb_Cte_Anulado_Substituido: TRLBand
      Left = 26
      Top = 1301
      Width = 742
      Height = 81
      AutoSize = True
      BandType = btHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = True
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_Cte_Anulado_SubstituidoBeforePrint
      object RLLabel9: TRLLabel
        Left = 6
        Top = 2
        Width = 732
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = 'CT-e  ANULADO / SUBSTITU'#205'DO'
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
      object RLDraw117: TRLDraw
        Left = 1
        Top = 14
        Width = 740
        Height = 1
        HelpContext = 1
        Align = faWidth
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object RLDraw118: TRLDraw
        Left = 370
        Top = 14
        Width = 1
        Height = 66
        Angle = 90.000000000000000000
        Brush.Style = bsClear
        DrawKind = dkLine
      end
      object rlblChaveCteSubstituido: TRLLabel
        Left = 5
        Top = 17
        Width = 90
        Height = 8
        Caption = 'CHAVE CT-E SUBSTITU'#205'DO'
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
      object rlblChaveCteAnulacao: TRLLabel
        Left = 373
        Top = 17
        Width = 84
        Height = 8
        Caption = 'CHAVE CT-E ANULA'#199#195'O'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        Visible = False
      end
      object rlChaveCteSerAnulSubst: TRLMemo
        Left = 5
        Top = 27
        Width = 363
        Height = 24
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlChaveCteAnulacao: TRLMemo
        Left = 373
        Top = 27
        Width = 363
        Height = 24
        AutoSize = False
        Behavior = [beSiteExpander]
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
    object rlb_01_ReciboBarra: TRLBand
      Left = 26
      Top = 38
      Width = 742
      Height = 65
      BandType = btHeader
      BeforePrint = rlb_01_ReciboBarraBeforePrint
      object rlBarraDataRecebimento: TRLLabel
        Tag = 10
        Left = 3
        Top = 32
        Width = 87
        Height = 7
        Caption = 'DATA DE RECEBIMENTO'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -7
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rlBarraiCanhoto: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 88
      end
      object rlBarraiCanhoto1: TRLDraw
        Left = 0
        Top = 25
        Width = 451
        Height = 1
        Borders.Sides = sdCustom
        Borders.DrawLeft = False
        Borders.DrawTop = False
        Borders.DrawRight = False
        Borders.DrawBottom = False
        Borders.Color = clWhite
        DrawKind = dkLine
        HoldStyle = hsHorizontally
      end
      object rlBarraiCanhoto2: TRLDraw
        Left = 102
        Top = 25
        Width = 1
        Height = 45
        Angle = 90.000000000000000000
        DrawKind = dkLine
        HoldStyle = hsVertically
      end
      object rlBarraiCanhoto3: TRLDraw
        Left = 451
        Top = 0
        Width = 1
        Height = 70
        Angle = 90.000000000000000000
        DrawKind = dkLine
        HoldStyle = hsVertically
      end
      object rlBarraIdentificacao: TRLLabel
        Tag = 10
        Left = 109
        Top = 31
        Width = 172
        Height = 7
        Caption = 'IDENTIFICA'#199#195'O E ASSINATURA DO RECEBEDOR'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -7
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rlBarramDadosAdicionaisAuxiliar: TRLMemo
        Left = 116
        Top = 45
        Width = 301
        Height = 12
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Courier New'
        Font.Style = []
        IntegralHeight = True
        ParentFont = False
        Visible = False
      end
      object rlBarraNumero: TRLLabel
        Left = 529
        Top = 3
        Width = 94
        Height = 16
        Alignment = taCenter
        Caption = 'N'#186' 000.000.000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RLBarraRecebemosDe: TRLMemo
        Tag = 20
        Left = 3
        Top = 1
        Width = 417
        Height = 12
        AutoSize = False
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Courier New'
        Font.Style = []
        IntegralHeight = True
        ParentFont = False
      end
      object RLBarraResumo: TRLMemo
        Tag = 20
        Left = 3
        Top = 10
        Width = 417
        Height = 12
        Behavior = [beSiteExpander]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -9
        Font.Name = 'Courier New'
        Font.Style = []
        IntegralHeight = True
        ParentFont = False
      end
      object rlBarraSERIE: TRLLabel
        Left = 639
        Top = 3
        Width = 68
        Height = 16
        Alignment = taCenter
        Caption = 'S'#201'RIE 000'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rllBarraCTe: TRLLabel
        Left = 458
        Top = 3
        Width = 45
        Height = 16
        AutoSize = False
        Caption = 'CT-e'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object RLBarraBarcode: TRLBarcode
        Left = 442
        Top = 31
        Width = 298
        Height = 38
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        BarcodeType = bcCode128C
      end
    end
    object rlb_DivisaoRecibo: TRLBand
      Left = 26
      Top = 26
      Width = 742
      Height = 12
      BandType = btHeader
      BeforePrint = rlb_DivisaoReciboBeforePrint
      object rliDivisao: TRLDraw
        Left = 0
        Top = 4
        Width = 741
        Height = 8
        DrawKind = dkLine
        HoldStyle = hsHorizontally
        Pen.Style = psDot
      end
    end
  end
  inherited RLPDFFilter1: TRLPDFFilter
    Left = 168
    Top = 16
  end
  inherited Datasource1: TDataSource
    Left = 336
    Top = 16
  end
end
