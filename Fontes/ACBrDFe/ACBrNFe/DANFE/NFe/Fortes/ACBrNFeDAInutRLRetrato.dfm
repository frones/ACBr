inherited frmNFeDAInutRLRetrato: TfrmNFeDAInutRLRetrato
  Left = 375
  Top = 175
  Caption = 'Inutiliza'#195#167#195#163'o - Retrato'
  ClientHeight = 485
  ClientWidth = 844
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  PixelsPerInch = 96
  TextHeight = 10
  inherited RLNFeInut: TRLReport
    Left = 1
    Top = 1
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = RLInutBeforePrint
    object rlb_01_Titulo: TRLBand
      Left = 38
      Top = 38
      Width = 718
      Height = 73
      Color = clWhite
      ParentColor = False
      object RLShape46: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 67
        Brush.Style = bsClear
      end
      object rllLinha1: TRLLabel
        Left = 211
        Top = 2
        Width = 322
        Height = 19
        Alignment = taCenter
        Caption = 'INUTILIZA'#195#8225#195#402'O DE NUMERA'#195#8225#195#402'O DA NF-E'
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
        Left = 180
        Top = 24
        Width = 380
        Height = 15
        Alignment = taCenter
        Caption = 
          'N'#195#163'o possui valor fiscal, simples representa'#195#167#195#163'o do fato indica' +
          'do abaixo.'
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
        Left = 166
        Top = 45
        Width = 408
        Height = 15
        Alignment = taCenter
        Caption = 'CONSULTE A AUTENTICIDADE NO SITE DA SEFAZ AUTORIZADORA.'
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
    object rlb_07_Rodape: TRLBand
      Left = 38
      Top = 430
      Width = 718
      Height = 32
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_07_RodapeBeforePrint
      object rlShape1: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 1
        Brush.Style = bsClear
      end
      object rllblSistema: TRLLabel
        Left = 448
        Top = 6
        Width = 291
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
      object rlLabel15: TRLLabel
        Left = 6
        Top = 6
        Width = 140
        Height = 12
        Caption = 'DATA E HORA DA IMPRESS'#195#402'O: '
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
    object rlb_03_Inutilizacao: TRLBand
      Left = 38
      Top = 231
      Width = 718
      Height = 154
      Color = clWhite
      ParentColor = False
      BeforePrint = RLb_03_InutilizacaoBeforePrint
      object rlsQuadro01: TRLDraw
        Left = 0
        Top = 0
        Width = 741
        Height = 145
        Brush.Style = bsClear
      end
      object rlsLinhaV01: TRLDraw
        Left = 70
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV09: TRLDraw
        Left = 138
        Top = 48
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
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel78: TRLLabel
        Left = 76
        Top = 51
        Width = 31
        Height = 8
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
        Left = 76
        Top = 62
        Width = 56
        Height = 15
        Alignment = taCenter
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
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
        Caption = 'N'#194#176' PROTOCOLO'
        Color = clWhite
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
        Top = 94
        Width = 322
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
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
        Width = 181
        Height = 13
        Caption = 'INUTILIZA'#195#8225#195#402'O DE NUMERA'#195#8225#195#402'O'
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
        Width = 93
        Height = 15
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rlLabel6: TRLLabel
        Left = 144
        Top = 50
        Width = 21
        Height = 8
        Caption = 'S'#195#8240'RIE'
        Color = clWhite
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
        Left = 144
        Top = 60
        Width = 40
        Height = 15
        Alignment = taCenter
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
      object rlLabel28: TRLLabel
        Left = 4
        Top = 51
        Width = 17
        Height = 8
        Caption = 'ANO'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllAno: TRLLabel
        Left = 4
        Top = 62
        Width = 62
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel17: TRLLabel
        Left = 194
        Top = 50
        Width = 92
        Height = 8
        Caption = 'N'#195#353'MERA'#195#8225#195#402'O INUTILIZADA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllNumeracao: TRLLabel
        Left = 194
        Top = 60
        Width = 73
        Height = 15
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
      object rlShape49: TRLDraw
        Left = 190
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
        Width = 47
        Height = 15
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlsLinhaV10: TRLDraw
        Left = 43
        Top = 15
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlShape2: TRLDraw
        Left = 0
        Top = 112
        Width = 740
        Height = 1
        Brush.Style = bsClear
      end
      object rlLabel1: TRLLabel
        Left = 4
        Top = 115
        Width = 52
        Height = 8
        Caption = 'JUSTIFICATIVA'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllJustificativa: TRLLabel
        Left = 4
        Top = 126
        Width = 77
        Height = 15
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object rlb_05_NaoUsado_Detalhe: TRLBand
      Left = 38
      Top = 400
      Width = 718
      Height = 15
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_05_NaoUsado_DetalheBeforePrint
    end
    object rlb_02_Emitente: TRLBand
      Left = 38
      Top = 111
      Width = 718
      Height = 120
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_02_EmitenteBeforePrint
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
        Caption = 'INSCRI'#195#8225#195#402'O ESTADUAL'
        Color = clWhite
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
        Caption = 'MUNIC'#195#141'PIO'
        Color = clWhite
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
        Top = 52
        Width = 39
        Height = 8
        Caption = 'ENDERE'#195#8225'O'
        Color = clWhite
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
        Top = 19
        Width = 79
        Height = 8
        Caption = 'NOME / RAZ'#195#402'O SOCIAL'
        Color = clWhite
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
      object RLShape51: TRLDraw
        Left = 1
        Top = 15
        Width = 740
        Height = 1
        HelpContext = 1
        Brush.Style = bsClear
      end
      object RLShape53: TRLDraw
        Left = 604
        Top = 16
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object RLShape82: TRLDraw
        Left = 665
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object RLShape99: TRLDraw
        Left = 448
        Top = 48
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object RLLabel4: TRLLabel
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
      object RLShape108: TRLDraw
        Left = 620
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object RLLabel5: TRLLabel
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
      object RLShape109: TRLDraw
        Left = 448
        Top = 80
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
    end
    object rlb_04_NaoUsado: TRLBand
      Left = 38
      Top = 385
      Width = 718
      Height = 15
      Color = clWhite
      ParentColor = False
    end
    object rlb_06_NaoUsado_Summary: TRLBand
      Left = 38
      Top = 415
      Width = 718
      Height = 15
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_06_NaoUsado_SummaryBeforePrint
    end
  end
end
