inherited frmNFeDAInutRLRetrato: TfrmNFeDAInutRLRetrato
  Left = 447
  Height = 834
  Top = 240
  Width = 844
  Caption = 'Inutilização - Retrato'
  ClientHeight = 834
  ClientWidth = 844
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  inherited RLNFeInut: TRLReport
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = RLInutBeforePrint
    object rlb_01_Titulo: TRLBand[0]
      Left = 38
      Height = 73
      Top = 38
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      object RLShape46: TRLDraw
        Left = 0
        Height = 67
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllLinha1: TRLLabel
        Left = 211
        Height = 19
        Top = 2
        Width = 322
        Alignment = taCenter
        Caption = 'INUTILIZAÇÃO DE NUMERAÇÃO DA NF-E'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllLinha2: TRLLabel
        Left = 180
        Height = 15
        Top = 24
        Width = 380
        Alignment = taCenter
        Caption = 'Não possui valor fiscal, simples representação do fato indicado abaixo.'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllLinha3: TRLLabel
        Left = 166
        Height = 15
        Top = 45
        Width = 408
        Alignment = taCenter
        Caption = 'CONSULTE A AUTENTICIDADE NO SITE DA SEFAZ AUTORIZADORA.'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
    end
    object rlb_07_Rodape: TRLBand[1]
      Left = 38
      Height = 32
      Top = 430
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_07_RodapeBeforePrint
      object rlShape1: TRLDraw
        Left = 0
        Height = 1
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllblSistema: TRLLabel
        Left = 448
        Height = 13
        Top = 6
        Width = 291
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Desenvolvido por Projeto ACBr - http://acbr.sourceforge.net/'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel15: TRLLabel
        Left = 6
        Height = 12
        Top = 6
        Width = 140
        Caption = 'DATA E HORA DA IMPRESSÃO: '
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_03_Inutilizacao: TRLBand[2]
      Left = 38
      Height = 154
      Top = 231
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = RLb_03_InutilizacaoBeforePrint
      object rlsQuadro01: TRLDraw
        Left = 0
        Height = 145
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        AfterPrint = rlsQuadro01AfterPrint
      end
      object rlsLinhaV01: TRLDraw
        Left = 70
        Height = 33
        Top = 48
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV09: TRLDraw
        Left = 138
        Height = 33
        Top = 48
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH04: TRLDraw
        Left = 0
        Height = 1
        Top = 48
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel2: TRLLabel
        Left = 4
        Height = 8
        Top = 18
        Width = 27
        Caption = 'ORGÃO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllOrgao: TRLLabel
        Left = 4
        Height = 16
        Top = 28
        Width = 38
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel78: TRLLabel
        Left = 76
        Height = 8
        Top = 51
        Width = 31
        Caption = 'MODELO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllModelo: TRLLabel
        Left = 76
        Height = 15
        Top = 62
        Width = 56
        Alignment = taCenter
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllDescricao: TRLLabel
        Left = 414
        Height = 8
        Top = 83
        Width = 54
        Caption = 'N° PROTOCOLO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllProtocolo: TRLLabel
        Left = 414
        Height = 16
        Top = 94
        Width = 322
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlShape88: TRLDraw
        Left = 0
        Height = 1
        Top = 80
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllTituloEvento: TRLLabel
        Left = 6
        Height = 13
        Top = 2
        Width = 181
        Caption = 'INUTILIZAÇÃO DE NUMERAÇÃO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlShape48: TRLDraw
        Left = 0
        Height = 1
        Top = 15
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel9: TRLLabel
        Left = 50
        Height = 8
        Top = 18
        Width = 37
        Caption = 'AMBIENTE'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllTipoAmbiente: TRLLabel
        Left = 50
        Height = 15
        Top = 28
        Width = 93
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel6: TRLLabel
        Left = 144
        Height = 8
        Top = 50
        Width = 21
        Caption = 'SÉRIE'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllSerie: TRLLabel
        Left = 144
        Height = 15
        Top = 60
        Width = 40
        Alignment = taCenter
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel28: TRLLabel
        Left = 4
        Height = 8
        Top = 51
        Width = 17
        Caption = 'ANO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllAno: TRLLabel
        Left = 4
        Height = 16
        Top = 62
        Width = 62
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel17: TRLLabel
        Left = 194
        Height = 8
        Top = 50
        Width = 92
        Caption = 'NÚMERAÇÃO INUTILIZADA'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllNumeracao: TRLLabel
        Left = 194
        Height = 15
        Top = 60
        Width = 73
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlShape49: TRLDraw
        Left = 190
        Height = 33
        Top = 48
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlShape50: TRLDraw
        Left = 410
        Height = 33
        Top = 80
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel18: TRLLabel
        Left = 4
        Height = 8
        Top = 83
        Width = 28
        Caption = 'STATUS'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllStatus: TRLLabel
        Left = 4
        Height = 15
        Top = 94
        Width = 47
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV10: TRLDraw
        Left = 43
        Height = 33
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlShape2: TRLDraw
        Left = 0
        Height = 1
        Top = 112
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel1: TRLLabel
        Left = 4
        Height = 8
        Top = 115
        Width = 52
        Caption = 'JUSTIFICATIVA'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllJustificativa: TRLLabel
        Left = 4
        Height = 15
        Top = 126
        Width = 77
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_05_NaoUsado_Detalhe: TRLBand[3]
      Left = 38
      Height = 15
      Top = 400
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_05_NaoUsado_DetalheBeforePrint
    end
    object rlb_02_Emitente: TRLBand[4]
      Left = 38
      Height = 120
      Top = 111
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_02_EmitenteBeforePrint
      object rlsQuadro02: TRLDraw
        Left = 0
        Height = 113
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH07: TRLDraw
        Left = 1
        Height = 1
        Top = 80
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH06: TRLDraw
        Left = 1
        Height = 1
        Top = 48
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllRazaoEmitente: TRLLabel
        Left = 4
        Height = 14
        Top = 30
        Width = 84
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllMunEmitente: TRLLabel
        Left = 4
        Height = 14
        Top = 94
        Width = 77
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllInscEstEmitente: TRLLabel
        Left = 624
        Height = 14
        Top = 94
        Width = 90
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllEnderecoEmitente: TRLLabel
        Left = 4
        Height = 14
        Top = 62
        Width = 99
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllCNPJEmitente: TRLLabel
        Left = 608
        Height = 14
        Top = 30
        Width = 82
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllCEPEmitente: TRLLabel
        Left = 669
        Height = 15
        Top = 62
        Width = 64
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel98: TRLLabel
        Left = 669
        Height = 8
        Top = 52
        Width = 15
        Caption = 'CEP'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel93: TRLLabel
        Left = 624
        Height = 8
        Top = 84
        Width = 78
        Caption = 'INSCRIÇÃO ESTADUAL'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel24: TRLLabel
        Left = 608
        Height = 8
        Top = 19
        Width = 19
        Caption = 'CNPJ'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel22: TRLLabel
        Left = 4
        Height = 8
        Top = 84
        Width = 38
        Caption = 'MUNICÍPIO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object RLLabel16: TRLLabel
        Left = 4
        Height = 8
        Top = 52
        Width = 39
        Caption = 'ENDEREÇO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object RLLabel13: TRLLabel
        Left = 4
        Height = 8
        Top = 19
        Width = 79
        Caption = 'NOME / RAZÃO SOCIAL'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object RLLabel12: TRLLabel
        Left = 6
        Height = 13
        Top = 2
        Width = 55
        Caption = 'EMITENTE'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object RLShape51: TRLDraw
        Left = 1
        Height = 1
        Top = 15
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLShape53: TRLDraw
        Left = 604
        Height = 33
        Top = 16
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLShape82: TRLDraw
        Left = 665
        Height = 33
        Top = 48
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLShape99: TRLDraw
        Left = 448
        Height = 33
        Top = 48
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLLabel4: TRLLabel
        Left = 452
        Height = 8
        Top = 52
        Width = 29
        Caption = 'BAIRRO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllBairroEmitente: TRLLabel
        Left = 452
        Height = 14
        Top = 62
        Width = 85
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLShape108: TRLDraw
        Left = 620
        Height = 33
        Top = 80
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLLabel5: TRLLabel
        Left = 452
        Height = 8
        Top = 84
        Width = 40
        Caption = 'FONE / FAX'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rllFoneEmitente: TRLLabel
        Left = 452
        Height = 14
        Top = 94
        Width = 79
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLShape109: TRLDraw
        Left = 448
        Height = 33
        Top = 80
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_04_NaoUsado: TRLBand[5]
      Left = 38
      Height = 15
      Top = 385
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
    end
    object rlb_06_NaoUsado_Summary: TRLBand[6]
      Left = 38
      Height = 15
      Top = 415
      Width = 718
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_06_NaoUsado_SummaryBeforePrint
    end
  end
end
