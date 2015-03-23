inherited frmDACTeRLRetratoA5: TfrmDACTeRLRetratoA5
  Left = 350
  Height = 629
  Top = 152
  Width = 835
  Caption = 'DACTe - Retrato A5'
  ClientHeight = 629
  ClientWidth = 835
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  inherited RLCTe: TRLReport
    Left = 24
    Top = 8
    Font.Height = -8
    Font.Name = 'Courier New'
    Margins.LeftMargin = 7
    Margins.TopMargin = 7
    Margins.RightMargin = 7
    Margins.BottomMargin = 7
    Title = 'DACTe Retrato A5'
    BeforePrint = RLCTeBeforePrint
    object rlb_08_Itens: TRLBand[0]
      Left = 26
      Height = 14
      Top = 679
      Width = 742
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_08_ItensBeforePrint
      object RLDraw29: TRLDraw
        Left = 370
        Height = 13
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw28: TRLDraw
        Left = 0
        Height = 14
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw35: TRLDraw
        Left = 740
        Height = 14
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rldbtTpDoc2: TRLDBText
        Left = 373
        Height = 13
        Top = 1
        Width = 46
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_2'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rldbtTpDoc1: TRLDBText
        Left = 5
        Height = 13
        Top = 1
        Width = 46
        AutoSize = False
        Color = clWhite
        DataField = 'TIPO_1'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rldbtDocumento2: TRLDBText
        Left = 542
        Height = 13
        Top = 1
        Width = 195
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_2'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rldbtDocumento1: TRLDBText
        Left = 174
        Height = 13
        Top = 1
        Width = 195
        AutoSize = False
        Color = clWhite
        DataField = 'DOCUMENTO_1'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rldbtCnpjEmitente2: TRLDBText
        Left = 424
        Height = 12
        Top = 1
        Width = 51
        Color = clWhite
        DataField = 'CNPJCPF_2'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rldbtCnpjEmitente1: TRLDBText
        Left = 56
        Height = 12
        Top = 1
        Width = 51
        Color = clWhite
        DataField = 'CNPJCPF_1'
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlsFimItens: TRLDraw
        Left = 0
        Height = 1
        Top = 14
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_01_Recibo: TRLBand[1]
      Left = 26
      Height = 69
      Top = 26
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_01_ReciboBeforePrint
      object RLDraw46: TRLDraw
        Left = 0
        Height = 64
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw49: TRLDraw
        Left = 1
        Height = 1
        Top = 40
        Width = 201
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw50: TRLDraw
        Left = 593
        Height = 50
        Top = 14
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw51: TRLDraw
        Left = 473
        Height = 50
        Top = 14
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw52: TRLDraw
        Left = 202
        Height = 50
        Top = 14
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllSerie2: TRLLabel
        Left = 655
        Height = 13
        Top = 47
        Width = 50
        Alignment = taCenter
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
      object rllNumCTe2: TRLLabel
        Left = 638
        Height = 16
        Top = 33
        Width = 86
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
        Transparent = False
      end
      object rlLabel3: TRLLabel
        Left = 480
        Height = 16
        Top = 46
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.CharSet = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel143: TRLLabel
        Left = 480
        Height = 16
        Top = 23
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.CharSet = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel140: TRLLabel
        Left = 647
        Height = 13
        Top = 17
        Width = 28
        Caption = 'CT-E'
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
      object rlLabel139: TRLLabel
        Left = 605
        Height = 12
        Top = 33
        Width = 14
        Caption = 'Nº '
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
        Transparent = False
      end
      object rlLabel138: TRLLabel
        Left = 605
        Height = 12
        Top = 47
        Width = 30
        Caption = 'SÉRIE:'
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
        Transparent = False
      end
      object rlLabel137: TRLLabel
        Left = 6
        Height = 13
        Top = 1
        Width = 732
        Alignment = taCenter
        AutoSize = False
        Caption = 'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO ESTADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
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
        Transparent = False
      end
      object rlLabel136: TRLLabel
        Left = 6
        Height = 12
        Top = 21
        Width = 30
        Caption = 'NOME'
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
        Transparent = False
      end
      object rlLabel135: TRLLabel
        Left = 480
        Height = 9
        Top = 15
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = 'CHEGADA DATA/HORA'
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
      object rlLabel134: TRLLabel
        Left = 480
        Height = 9
        Top = 38
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = 'SAÍDA DATA/HORA'
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
      object rlLabel133: TRLLabel
        Left = 207
        Height = 13
        Top = 48
        Width = 262
        Alignment = taCenter
        AutoSize = False
        Caption = 'ASSINATURA / CARIMBO'
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
        Transparent = False
      end
      object rlLabel132: TRLLabel
        Left = 6
        Height = 13
        Top = 45
        Width = 19
        AutoSize = False
        Caption = 'RG'
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
        Transparent = False
      end
      object RLDraw101: TRLDraw
        Left = 0
        Height = 1
        Top = 65
        Width = 756
        Angle = 0
        Pen.Style = psDot
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw48: TRLDraw
        Left = 1
        Height = 1
        Top = 14
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_07_HeaderItens: TRLBand[2]
      Left = 26
      Height = 11
      Top = 668
      Width = 742
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_07_HeaderItensBeforePrint
      object rlsQuadro07: TRLDraw
        Left = 0
        Height = 11
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw34: TRLDraw
        Left = 370
        Height = 11
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel96: TRLLabel
        Left = 174
        Height = 8
        Top = 1
        Width = 86
        Caption = 'SÉRIE/NRO. DOCUMENTO'
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
      object rlLabel92: TRLLabel
        Left = 56
        Height = 8
        Top = 1
        Width = 69
        Caption = 'CNPJ/CPF EMITENTE'
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
      object rlLabel91: TRLLabel
        Left = 5
        Height = 8
        Top = 1
        Width = 29
        Caption = 'TP DOC.'
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
      object rlLabel109: TRLLabel
        Left = 373
        Height = 8
        Top = 1
        Width = 29
        Caption = 'TP DOC.'
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
      object rlLabel106: TRLLabel
        Left = 424
        Height = 8
        Top = 1
        Width = 69
        Caption = 'CNPJ/CPF EMITENTE'
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
      object rlLabel100: TRLLabel
        Left = 542
        Height = 8
        Top = 1
        Width = 86
        Caption = 'SÉRIE/NRO. DOCUMENTO'
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
    end
    object rlb_09_Obs: TRLBand[3]
      Left = 26
      Height = 56
      Top = 693
      Width = 742
      AlignToBottom = True
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_09_ObsBeforePrint
      object rlsQuadro08: TRLDraw
        Left = 0
        Height = 56
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmObs: TRLMemo
        Left = 304
        Height = 40
        Top = 15
        Width = 241
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2'
          'OBS LINHA 3'
          'OBS LINHA 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel10: TRLLabel
        Left = 304
        Height = 13
        Top = 1
        Width = 234
        Alignment = taCenter
        AutoSize = False
        Caption = 'Observações - Informações Complementares'
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
        Transparent = False
      end
      object rlLabel11: TRLLabel
        Left = 6
        Height = 8
        Top = 1
        Width = 72
        Caption = 'RNTRC DA EMPRESA'
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
      object lblCIOT: TRLLabel
        Left = 84
        Height = 8
        Top = 1
        Width = 18
        Caption = 'CIOT'
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
      object rlLabel83: TRLLabel
        Left = 154
        Height = 8
        Top = 1
        Width = 35
        Caption = 'LOTAÇÃO'
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
      object rlLabel84: TRLLabel
        Left = 196
        Height = 8
        Top = 1
        Width = 101
        Caption = 'DATA PREVISTA DE ENTREGA'
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
      object rllRntrcEmpresa: TRLLabel
        Left = 6
        Height = 12
        Top = 8
        Width = 64
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
      object rllCIOT: TRLLabel
        Left = 84
        Height = 12
        Top = 8
        Width = 32
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
      object rllLotacao: TRLLabel
        Left = 154
        Height = 13
        Top = 8
        Width = 34
        Alignment = taCenter
        AutoSize = False
        Caption = 'SIM'
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
      object rllDtPrevEntrega: TRLLabel
        Left = 196
        Height = 12
        Top = 8
        Width = 69
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
        Transparent = False
      end
      object rlsCIOT: TRLDraw
        Left = 80
        Height = 20
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw36: TRLDraw
        Left = 150
        Height = 20
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw37: TRLDraw
        Left = 192
        Height = 20
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw38: TRLDraw
        Left = 300
        Height = 56
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw24: TRLDraw
        Left = 1
        Height = 1
        Top = 20
        Width = 300
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel85: TRLLabel
        Left = 6
        Height = 27
        Top = 22
        Width = 292
        Alignment = taCenter
        AutoSize = False
        Caption = 'ESSE CONHECIMENTO DE TRANSPORTE ATENDE À LEGISLAÇÃO DE TRANSPORTE RODOVIÁRIO EM VIGOR'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel20: TRLLabel
        Left = 552
        Height = 13
        Top = 1
        Width = 186
        Alignment = taCenter
        AutoSize = False
        Caption = 'RESERVADO AO FISCO'
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
        Transparent = False
      end
      object RLDraw5: TRLDraw
        Left = 548
        Height = 56
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmObsFisco: TRLMemo
        Left = 552
        Height = 40
        Top = 15
        Width = 185
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'OBS LINHA 1'
          'OBS LINHA 2'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllMsgTeste: TRLLabel
        Left = 11
        Height = 31
        Top = 22
        Width = 718
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'
        Color = clWhite
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw1: TRLDraw
        Left = 300
        Height = 1
        Top = 14
        Width = 440
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_02_Cabecalho: TRLBand[4]
      Left = 26
      Height = 173
      Top = 241
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_02_CabecalhoBeforePrint
      object rlsQuadro01: TRLDraw
        Left = 0
        Height = 174
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH02: TRLDraw
        Left = 332
        Height = 1
        Top = 54
        Width = 408
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH03: TRLDraw
        Left = 332
        Height = 1
        Top = 88
        Width = 408
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV01: TRLDraw
        Left = 174
        Height = 60
        Top = 114
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV04: TRLDraw
        Left = 332
        Height = 174
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV05: TRLDraw
        Left = 366
        Height = 26
        Top = 29
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV06: TRLDraw
        Left = 390
        Height = 26
        Top = 29
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV07: TRLDraw
        Left = 614
        Height = 28
        Top = 29
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV08: TRLDraw
        Left = 464
        Height = 26
        Top = 29
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV09: TRLDraw
        Left = 508
        Height = 26
        Top = 29
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV10: TRLDraw
        Left = 627
        Height = 30
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rliLogo: TRLImage
        Left = 7
        Height = 62
        Top = 32
        Width = 94
        Center = True
        Picture.Data = {
          0A544A706567496D616765D1080000FFD8FFE000104A46494600010101006000
          600000FFE1002245786966000049492A00080000000100005104000100000000
          00000000000000FFDB004300080606070605080707070909080A0C140D0C0B0B
          0C1912130F141D1A1F1E1D1A1C1C20242E2720222C231C1C2837292C30313434
          341F27393D38323C2E333432FFDB0043010909090C0B0C180D0D1832211C2132
          3232323232323232323232323232323232323232323232323232323232323232
          3232323232323232323232323232323232FFC00011080047005A030122000211
          01031101FFC4001F000001050101010101010000000000000000010203040506
          0708090A0BFFC400B5100002010303020403050504040000017D010203000411
          05122131410613516107227114328191A1082342B1C11552D1F0243362728209
          0A161718191A25262728292A3435363738393A434445464748494A5354555657
          58595A636465666768696A737475767778797A838485868788898A9293949596
          9798999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2
          D3D4D5D6D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F
          0100030101010101010101010000000000000102030405060708090A0BFFC400
          B511000201020404030407050404000102770001020311040521310612415107
          61711322328108144291A1B1C109233352F0156272D10A162434E125F1171819
          1A262728292A35363738393A434445464748494A535455565758595A63646566
          6768696A737475767778797A82838485868788898A92939495969798999AA2A3
          A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8
          D9DAE2E3E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C0301000211031100
          3F00F6900B1000C9352FD966C6768FA6697CF86C34E9EFA7388E246918FF00B2
          064FF2AF34D3FC63E28D7758FB569EA059C7264C247CAEB9E99C7A77AE75056B
          B2EE7A308DCBEC0BF37A53FECD3671E59FAE457112F8D751BAD6FECADA63D8B7
          96CBBC4BE6027B1FBA315B916A77DA67C3BBBD52F67692E92096446739C1E420
          FE5F9D38D34C4E425EDBEBD15F4ACB70B15A1C042550E3DBD69B697D2C17D3A5
          EDEE6358D4A07455C9EE4605705A5EABE2FD72DE291EECBDBEECFCD5DC38862B
          449AF767EE97259874ADDD28F298BA8D3B965BC43621D86642ABD5C21C67D2B0
          4F8BAE3ED52F94B1B4521C43E67CAA98EB93DCFB550BEBF92FDC298D96127F75
          6EBC17FF0069BDBDBFC9AD0E8F7F25D1B9D434E9E68A338B78A11919F538E95C
          CE9B92BC76FEBEE0552537D91A4FE20D4D9894BC04F5C45002A3F135A9A4789C
          CF38B4BF55495BEE48380DEC7D0D65B59EA31A2CB2C71C7001968E2E0A0FEB50
          EB31E9B2E929756773BE5475C1E8706B19D19D3576CA6D25CD17B1DEAB86E869
          D5CA4377343A025D4521930548E73DF9AD58B570F1236D3F32834A9B93BA96E9
          D8DD9B5782DF52D3EE34DBB9B60BA8DA2CE4024118E3DEA8F873428FC2DA5F91
          2DDC6C89D642BB723DE9752D3A0D52CDEDA750518570D2FC3BBB129587519442
          7F84B1E057A0626BDE789A0D675D934FD36554555C19550162724707F0A77C51
          B81A778161D3A3273712C7001DCAAFCC4FFE3A3F3AAB69E1CFF8460B4D6F6AD7
          3941B994F20E4E7F0C1ACCD6F41FEDED5ADE784BC8E06ED80600FA9AAB2B1177
          7373C336A2D342B74C63E5C9ACBD4F503A84E0202D6E8DB628FF00E7AB7F78FB
          0AB17F7FB34A86CE23B2490159307EE28EBFE156FC29A50BCBA5BA99311A8F91
          7D00A997BF2E5E8BF332B733B1A7A07872382137FA830DE46E258E303DFD2ACE
          9FE33F0E5E6AA34AB2B90D313B54843B18FA03567C51A48D774CFECCFED33631
          B9065D98DCEBFDDE7A0ACFD17C1BA3688D13C5323BC6721988CE6AAF737B5B62
          E6B89069FE54AC42C73C822DA7FBC413FD2B94F13F931D9DB411A2AB3C99181F
          C2064D2F8E3577D4BC4BA7E8968CAF1C0EB712B29CE1B0401F91359BA94C752D
          53119CA2E208F1DF1F78D454778A877FCBA99544AF6468697A7DC2E90BF65942
          8907CD1BF2BF51E86AB8F11CF6E3C96B693747F21C74C8E2B6E7BA8748D3E30C
          0B3F0B1C6BD5DBD05659F0C6A9707CE6B954693E72A3B13CE2B4718A7EA52726
          775451454161481557A003E8296A0BE95A0D3EE665FBC91330FA804D26ECAE07
          9EDF42BABEB13B42E62124C550A742ABDFF1AEC3C1D7AAD35E58BB2978182A11
          C6E5AE3F4D996DADA49CF3E5C785F6279FEB5B56F6C61F0D4B7F1318EED47991
          CABD41FF000AE6C2C9C937FD776251B453F98CF10FC3FB8D6BC5B71A8492B35B
          CC8BB70F8D840C63FAFE3506ABE07F0BE85A52CBA95EDC24CC70A44BCB9F40B5
          3EA7E3AD7ED14DAC5A1ABCBB702E04E719F5DBB7FAD72A74BBBBA9D755F115CC
          97370C711404F24FA01D8575B7CAAEC4E496A5DB7D3AC748B7DF60C5AE6E410A
          CDFC2BDD8FD2B4F40B1124BF682BFBA886D8F3DFDEA8DADA49777261182CDFEB
          597A22FF00747B574F3EDB0D34A44006036A0F5268A716DF3CBFA464BBB33B44
          B49355D664BEB962EB0B94407A2FD2BB4C0AA3A4D8AE9FA745081F3632E7D49E
          B57A866EB425F247FCF58BFEFAA3C91FF3D62FFBEAABD15CFED5F634E4458F24
          7FCF58BFEFAA8E7B559ADE488CB161D4A9F9BD4547451ED5F60E4479CD9DA194
          5DE9923AA4E094C13DC74FC2BB6D36CE39B41FB34F2468BB0A480B631597AFF8
          6C6A12FDAADCED9C7520E09AE33538358B3F92EA09E74ECCF2B11F957353F694
          9BE5574C8B492E5EC749A86B10E9974F05B5C477FB8001F3F2A37BB7422B2E24
          B8BFB9661279929FBF39E1507A2FA0AA5A46957FAE853332C3A7A36708A14311
          E807F3AEAEFED6DE2D39AC2153E64E362AA753FF00D6AEDA519C973D5F918B8A
          E6EE54B0B90ADF62D22013C80E1E67384FCFBD74369A3DCCD771DC5FCD6E045F
          72346C827D4D58D3EC21D3ED638628D576A807156EA5D76CDD5248B1E48FF9EB
          17FDF547923FE7AC5FF7D557A2A7DABEC57220A28A2B22828A28A002992451CA
          3122061EF4514018D75A549644DC69516493F35B6E0AADEE33D0D58D2B4F6881
          BCBB8C0BC93EF739D83FBA28A2A9CE4D728B955EE6A514515230A28A2803FFD9
        }
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Stretch = True
      end
      object rlsLinhaH04: TRLDraw
        Left = 0
        Height = 1
        Top = 114
        Width = 332
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmEmitente: TRLMemo
        Left = 7
        Height = 24
        Top = 1
        Width = 322
        Alignment = taCenter
        AutoSize = False
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
      object rlmDadosEmitente: TRLMemo
        Left = 113
        Height = 84
        Top = 26
        Width = 216
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          '1 Linha - LOGRADOURO - COMPLEMENTO - BAIRRO'
          '2 Linha - CEP - MUNICIPIO - UF'
          '3 Linha - CNPJ INSCRICAO ESTADUAL'
          '4 Linha - TELEFONE'
          '5 Linha - URL'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
        Transparent = False
      end
      object rlLabel17: TRLLabel
        Left = 371
        Height = 17
        Top = 1
        Width = 218
        Alignment = taCenter
        AutoSize = False
        Caption = 'DACTE'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel18: TRLLabel
        Left = 344
        Height = 14
        Top = 16
        Width = 278
        Alignment = taCenter
        AutoSize = False
        Caption = 'Documento Auxiliar do Conhecimento de Transporte Eletrônico'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -8
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
        Left = 640
        Height = 16
        Top = 1
        Width = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 'MODAL'
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
      object rllModal: TRLLabel
        Left = 633
        Height = 15
        Top = 16
        Width = 96
        Alignment = taCenter
        AutoSize = False
        Color = clWhite
        Font.CharSet = ANSI_CHARSET
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
      object rlLabel8: TRLLabel
        Left = 333
        Height = 8
        Top = 30
        Width = 32
        Alignment = taCenter
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
        Left = 334
        Height = 15
        Top = 38
        Width = 30
        Alignment = taCenter
        AutoSize = False
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
      object rlLabel21: TRLLabel
        Left = 367
        Height = 8
        Top = 30
        Width = 22
        Alignment = taCenter
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
        Left = 368
        Height = 15
        Top = 38
        Width = 20
        Alignment = taCenter
        AutoSize = False
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
      object rlLabel23: TRLLabel
        Left = 392
        Height = 9
        Top = 30
        Width = 70
        Alignment = taCenter
        AutoSize = False
        Caption = 'NÚMERO'
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
      object rllNumCte: TRLLabel
        Left = 392
        Height = 15
        Top = 38
        Width = 70
        Alignment = taCenter
        AutoSize = False
        Caption = '999.999.999'
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
      object rlLabel25: TRLLabel
        Left = 466
        Height = 9
        Top = 30
        Width = 42
        Alignment = taCenter
        AutoSize = False
        Caption = 'FOLHA'
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
      object rllPageNumber: TRLLabel
        Left = 466
        Height = 15
        Top = 38
        Width = 42
        Alignment = taCenter
        AutoSize = False
        Caption = '00/00'
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
      object rlLabel33: TRLLabel
        Left = 510
        Height = 9
        Top = 30
        Width = 95
        AutoSize = False
        Caption = 'DATA E HORA DE EMISSÃO'
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
      object rllEmissao: TRLLabel
        Left = 510
        Height = 13
        Top = 38
        Width = 58
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
      object rlLabel74: TRLLabel
        Left = 334
        Height = 11
        Top = 90
        Width = 58
        Caption = 'Chave de acesso'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -8
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
      object rllChave: TRLLabel
        Left = 336
        Height = 14
        Top = 100
        Width = 402
        Alignment = taCenter
        AutoSize = False
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
      object rlLabel2: TRLLabel
        Left = 4
        Height = 8
        Top = 116
        Width = 46
        Caption = 'TIPO DO CT-E'
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
      object rllTipoCte: TRLLabel
        Left = 4
        Height = 15
        Top = 124
        Width = 76
        AutoSize = False
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
      object rlLabel9: TRLLabel
        Left = 178
        Height = 8
        Top = 116
        Width = 61
        Caption = 'TIPO DO SERVIÇO'
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
      object rllTipoServico: TRLLabel
        Left = 178
        Height = 15
        Top = 124
        Width = 91
        AutoSize = False
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
      object rlLabel28: TRLLabel
        Left = 4
        Height = 8
        Top = 149
        Width = 81
        Caption = 'TOMADOR DO SERVIÇO'
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
      object rllTomaServico: TRLLabel
        Left = 4
        Height = 15
        Top = 158
        Width = 81
        AutoSize = False
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
      object rlLabel78: TRLLabel
        Left = 178
        Height = 8
        Top = 149
        Width = 83
        Caption = 'FORMA DE PAGAMENTO'
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
      object rllFormaPagamento: TRLLabel
        Left = 178
        Height = 15
        Top = 158
        Width = 73
        AutoSize = False
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
      object rllDescricao: TRLLabel
        Left = 334
        Height = 8
        Top = 149
        Width = 56
        Caption = 'N° PROTOCOLO'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -7
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
      object rllProtocolo: TRLLabel
        Left = 336
        Height = 15
        Top = 158
        Width = 402
        Alignment = taCenter
        AutoSize = False
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
      end
      object rlLabel77: TRLLabel
        Left = 616
        Height = 8
        Top = 30
        Width = 120
        Caption = 'INSC. SUFRAMA DO DESTINATÁRIO'
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
      object rllInscSuframa: TRLLabel
        Left = 616
        Height = 12
        Top = 38
        Width = 56
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
      object RLDraw88: TRLDraw
        Left = 332
        Height = 1
        Top = 148
        Width = 408
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllVariavel1: TRLLabel
        Left = 344
        Height = 29
        Top = 116
        Width = 386
        Alignment = taCenter
        AutoSize = False
        Caption = 'Consulta de autenticidade no portal nacional do CT-e, no site da Sefaz Autorizadora, ou em http://www.cte.fazenda.gov.br/portal'
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
      object rlsLinhaH01: TRLDraw
        Left = 332
        Height = 1
        Top = 29
        Width = 408
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw99: TRLDraw
        Left = 332
        Height = 1
        Top = 114
        Width = 408
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw3: TRLDraw
        Left = 0
        Height = 1
        Top = 148
        Width = 332
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rliBarCode: TRLBarcode
        Left = 337
        Height = 32
        Top = 56
        Width = 398
        AutoSize = False
        BarcodeType = bcCode128C
        Margins.LeftMargin = 1
        Margins.RightMargin = 1
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLBarcode1: TRLBarcode
        Left = 346
        Height = 28
        Top = 117
        Width = 381
        AutoSize = False
        BarcodeType = bcCode128C
        Margins.LeftMargin = 1
        Margins.RightMargin = 1
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_03_DadosDACTe: TRLBand[5]
      Left = 26
      Height = 81
      Top = 414
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_03_DadosDACTeBeforePrint
      object rllEnderecoDest2: TRLLabel
        Left = 432
        Height = 13
        Top = 41
        Width = 303
        AutoSize = False
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
      object rlsQuadro02: TRLDraw
        Left = 0
        Height = 82
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaV11: TRLDraw
        Left = 370
        Height = 58
        Top = 24
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlsLinhaH05: TRLDraw
        Left = 1
        Height = 1
        Top = 24
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllRazaoRemet: TRLLabel
        Left = 48
        Height = 13
        Top = 25
        Width = 318
        AutoSize = False
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
      object rllRazaoDest: TRLLabel
        Left = 432
        Height = 13
        Top = 25
        Width = 303
        AutoSize = False
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
      object rllPaisRemet: TRLLabel
        Left = 48
        Height = 13
        Top = 67
        Width = 209
        AutoSize = False
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
      object rllOrigPrestacao: TRLLabel
        Left = 336
        Height = 15
        Top = 9
        Width = 195
        AutoSize = False
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
      object rllNatOperacao: TRLLabel
        Left = 4
        Height = 15
        Top = 9
        Width = 325
        AutoSize = False
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
      object rllMunRemet: TRLLabel
        Left = 48
        Height = 19
        Top = 49
        Width = 234
        AutoSize = False
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
      object rllMunDest: TRLLabel
        Left = 432
        Height = 13
        Top = 49
        Width = 225
        AutoSize = False
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
      object rllInscEstRemet: TRLLabel
        Left = 256
        Height = 13
        Top = 58
        Width = 109
        AutoSize = False
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
      object rllInscEstDest: TRLLabel
        Left = 632
        Height = 13
        Top = 58
        Width = 102
        AutoSize = False
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
      object rllFoneRemet: TRLLabel
        Left = 288
        Height = 13
        Top = 68
        Width = 77
        AutoSize = False
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
      object rllFoneDest: TRLLabel
        Left = 664
        Height = 13
        Top = 67
        Width = 70
        AutoSize = False
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
      object rllEnderecoRemet2: TRLLabel
        Left = 48
        Height = 13
        Top = 41
        Width = 318
        AutoSize = False
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
      object rllEnderecoRemet1: TRLLabel
        Left = 48
        Height = 13
        Top = 33
        Width = 318
        AutoSize = False
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
      object rllEnderecoDest1: TRLLabel
        Left = 432
        Height = 13
        Top = 33
        Width = 303
        AutoSize = False
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
      object rllDestPrestacao: TRLLabel
        Left = 542
        Height = 15
        Top = 9
        Width = 195
        AutoSize = False
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
      object rllCnpjRemet: TRLLabel
        Left = 48
        Height = 13
        Top = 58
        Width = 124
        AutoSize = False
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
      object rllCEPRemet: TRLLabel
        Left = 301
        Height = 13
        Top = 49
        Width = 64
        AutoSize = False
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
      object rllCEPDest: TRLLabel
        Left = 677
        Height = 13
        Top = 49
        Width = 57
        AutoSize = False
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
      object rlLabel98: TRLLabel
        Left = 284
        Height = 8
        Top = 49
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
      object rlLabel95: TRLLabel
        Left = 262
        Height = 8
        Top = 68
        Width = 20
        Caption = 'FONE'
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
        Left = 174
        Height = 8
        Top = 58
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
      object rlLabel79: TRLLabel
        Left = 374
        Height = 8
        Top = 67
        Width = 17
        Caption = 'PAIS'
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
      object rlLabel32: TRLLabel
        Left = 374
        Height = 8
        Top = 58
        Width = 34
        Caption = 'CNPJ/CPF'
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
      object rlLabel31: TRLLabel
        Left = 374
        Height = 8
        Top = 49
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
      object rlLabel30: TRLLabel
        Left = 374
        Height = 8
        Top = 33
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
      object rlLabel29: TRLLabel
        Left = 4
        Height = 8
        Top = 1
        Width = 115
        Caption = 'CFOP - NATUREZA DA OPERAÇÃO'
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
      object rlLabel27: TRLLabel
        Left = 374
        Height = 8
        Top = 25
        Width = 52
        Caption = 'DESTINATÁRIO'
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
      object rlLabel26: TRLLabel
        Left = 4
        Height = 8
        Top = 67
        Width = 17
        Caption = 'PAIS'
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
        Left = 4
        Height = 8
        Top = 58
        Width = 34
        Caption = 'CNPJ/CPF'
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
        Top = 49
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
      object rlLabel16: TRLLabel
        Left = 4
        Height = 8
        Top = 33
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
      object rlLabel14: TRLLabel
        Left = 542
        Height = 8
        Top = 1
        Width = 86
        Caption = 'DESTINO DA PRESTAÇÃO'
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
      object rlLabel13: TRLLabel
        Left = 4
        Height = 8
        Top = 25
        Width = 42
        Caption = 'REMETENTE'
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
      object rlLabel12: TRLLabel
        Left = 336
        Height = 8
        Top = 1
        Width = 84
        Caption = 'ORIGEM DA PRESTAÇÃO'
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
      object rlLabel116: TRLLabel
        Left = 640
        Height = 8
        Top = 67
        Width = 20
        Caption = 'FONE'
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
      object rlLabel114: TRLLabel
        Left = 551
        Height = 8
        Top = 58
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
      object RLDraw102: TRLDraw
        Left = 536
        Height = 24
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw103: TRLDraw
        Left = 332
        Height = 24
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllCnpjDest: TRLLabel
        Left = 432
        Height = 18
        Top = 58
        Width = 115
        AutoSize = False
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
      object rllPaisDest: TRLLabel
        Left = 432
        Height = 13
        Top = 67
        Width = 203
        AutoSize = False
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
      object rlLabel119: TRLLabel
        Left = 660
        Height = 8
        Top = 49
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
    end
    object rlb_04_DadosNotaFiscal: TRLBand[6]
      Left = 26
      Height = 65
      Top = 495
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_04_DadosNotaFiscalBeforePrint
      object rlsQuadro03: TRLDraw
        Left = 0
        Height = 66
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw9: TRLDraw
        Left = 148
        Height = 23
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw8: TRLDraw
        Left = 632
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw7: TRLDraw
        Left = 526
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw61: TRLDraw
        Left = 415
        Height = 44
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw60: TRLDraw
        Left = 324
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw59: TRLDraw
        Left = 164
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw58: TRLDraw
        Left = 84
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw56: TRLDraw
        Left = 294
        Height = 23
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw55: TRLDraw
        Left = 1
        Height = 1
        Top = 23
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmQtdUnidMedida5: TRLMemo
        Left = 328
        Height = 12
        Top = 32
        Width = 84
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmQtdUnidMedida3: TRLMemo
        Left = 166
        Height = 12
        Top = 32
        Width = 76
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmQtdUnidMedida2: TRLMemo
        Left = 86
        Height = 12
        Top = 32
        Width = 76
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmQtdUnidMedida1: TRLMemo
        Left = 5
        Height = 12
        Top = 32
        Width = 76
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllVlrTotalMerc: TRLLabel
        Left = 298
        Height = 13
        Top = 10
        Width = 110
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllRespSeguroMerc: TRLLabel
        Left = 418
        Height = 13
        Top = 32
        Width = 106
        AutoSize = False
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
      object rllProdPredominante: TRLLabel
        Left = 4
        Height = 13
        Top = 10
        Width = 141
        AutoSize = False
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
      object rllOutrasCaracCarga: TRLLabel
        Left = 152
        Height = 13
        Top = 10
        Width = 139
        AutoSize = False
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
      object rllNroAverbacao: TRLLabel
        Left = 634
        Height = 13
        Top = 32
        Width = 102
        AutoSize = False
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
      object rllNroApolice: TRLLabel
        Left = 528
        Height = 13
        Top = 32
        Width = 102
        AutoSize = False
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
      object rllNomeSeguradora: TRLLabel
        Left = 418
        Height = 13
        Top = 10
        Width = 319
        AutoSize = False
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
        Transparent = False
      end
      object rlLabel5: TRLLabel
        Left = 418
        Height = 8
        Top = 1
        Width = 84
        Caption = 'NOME DA SEGURADORA'
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
      object rlLabel43: TRLLabel
        Left = 328
        Height = 9
        Top = 24
        Width = 84
        Alignment = taCenter
        AutoSize = False
        Caption = 'QTDE. VOLUMES (Unid)'
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
      object rlLabel41: TRLLabel
        Left = 166
        Height = 9
        Top = 24
        Width = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO AFERIDO (Kg)'
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
      object rlLabel40: TRLLabel
        Left = 634
        Height = 8
        Top = 24
        Width = 90
        Caption = 'NÚMERO DA AVERBAÇÃO'
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
      object rlLabel4: TRLLabel
        Left = 152
        Height = 8
        Top = 1
        Width = 135
        Caption = 'OUTRAS CARACTERÍSTICAS DA CARGA'
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
      object rlLabel39: TRLLabel
        Left = 528
        Height = 8
        Top = 24
        Width = 75
        Caption = 'NÚMERO DA APÓLICE'
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
      object rlLabel37: TRLLabel
        Left = 418
        Height = 8
        Top = 24
        Width = 51
        Caption = 'RESPONSÁVEL'
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
      object rlLabel36: TRLLabel
        Left = 86
        Height = 9
        Top = 24
        Width = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO BASE CÁLC. (Kg)'
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
      object rlLabel35: TRLLabel
        Left = 5
        Height = 9
        Top = 24
        Width = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 'PESO BRUTO (Kg)'
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
      object rlLabel34: TRLLabel
        Left = 298
        Height = 8
        Top = 1
        Width = 111
        Caption = 'VALOR TOTAL DA MERCADORIA'
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
      object rlLabel1: TRLLabel
        Left = 4
        Height = 8
        Top = 1
        Width = 91
        Caption = 'PRODUTO PREDOMINANTE'
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
      object rlmQtdUnidMedida4: TRLMemo
        Left = 246
        Height = 12
        Top = 32
        Width = 76
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel73: TRLLabel
        Left = 246
        Height = 9
        Top = 24
        Width = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 'CUBAGEM (M3)'
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
      object RLDraw100: TRLDraw
        Left = 244
        Height = 22
        Top = 23
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel52: TRLLabel
        Left = 3
        Height = 8
        Top = 45
        Width = 81
        Caption = 'SITUAÇÃO TRIBUTÁRIA'
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
      object rllSitTrib: TRLLabel
        Left = 3
        Height = 13
        Top = 52
        Width = 340
        AutoSize = False
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
      object RLDraw22: TRLDraw
        Left = 346
        Height = 22
        Top = 44
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel55: TRLLabel
        Left = 350
        Height = 8
        Top = 45
        Width = 66
        Caption = 'BASE DE CÁLCULO'
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
      object rlLabel56: TRLLabel
        Left = 454
        Height = 8
        Top = 45
        Width = 39
        Caption = 'ALÍQ. ICMS'
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
      object rlLabel54: TRLLabel
        Left = 504
        Height = 8
        Top = 45
        Width = 45
        Caption = 'VALOR ICMS'
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
      object rlLabel53: TRLLabel
        Left = 590
        Height = 8
        Top = 45
        Width = 59
        Caption = '% RED.BC.CALC.'
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
      object rlLabel58: TRLLabel
        Left = 656
        Height = 8
        Top = 45
        Width = 29
        Caption = 'ICMS ST'
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
      object RLDraw20: TRLDraw
        Left = 448
        Height = 22
        Top = 44
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw23: TRLDraw
        Left = 500
        Height = 22
        Top = 44
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw25: TRLDraw
        Left = 586
        Height = 22
        Top = 44
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw26: TRLDraw
        Left = 650
        Height = 22
        Top = 44
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllBaseCalc: TRLLabel
        Left = 350
        Height = 13
        Top = 52
        Width = 95
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllAliqICMS: TRLLabel
        Left = 454
        Height = 13
        Top = 52
        Width = 41
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllVlrICMS: TRLLabel
        Left = 504
        Height = 13
        Top = 52
        Width = 79
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllRedBaseCalc: TRLLabel
        Left = 590
        Height = 13
        Top = 52
        Width = 57
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllICMS_ST: TRLLabel
        Left = 656
        Height = 13
        Top = 52
        Width = 81
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw62: TRLDraw
        Left = 0
        Height = 1
        Top = 44
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_05_Complemento: TRLBand[7]
      Left = 26
      Height = 61
      Top = 560
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_05_ComplementoBeforePrint
      object rlsQuadro04: TRLDraw
        Left = 0
        Height = 60
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw6: TRLDraw
        Left = 372
        Height = 60
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmComplValor2: TRLMemo
        Left = 645
        Height = 46
        Top = 11
        Width = 89
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmComplValor1: TRLMemo
        Left = 280
        Height = 46
        Top = 11
        Width = 89
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmComplChave2: TRLMemo
        Left = 377
        Height = 46
        Top = 11
        Width = 264
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmComplChave1: TRLMemo
        Left = 5
        Height = 46
        Top = 11
        Width = 269
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
          'COMP 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel64: TRLLabel
        Left = 645
        Height = 8
        Top = 2
        Width = 90
        Caption = 'VALOR COMPLEMENTADO'
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
      object rlLabel63: TRLLabel
        Left = 377
        Height = 8
        Top = 2
        Width = 119
        Caption = 'CHAVE DO CT-E COMPLEMENTADO'
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
      object rlLabel62: TRLLabel
        Left = 280
        Height = 8
        Top = 2
        Width = 90
        Caption = 'VALOR COMPLEMENTADO'
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
      object rlLabel61: TRLLabel
        Left = 5
        Height = 8
        Top = 2
        Width = 119
        Caption = 'CHAVE DO CT-E COMPLEMENTADO'
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
    end
    object rlb_17_Sistema: TRLBand[8]
      Left = 26
      Height = 13
      Top = 1124
      Width = 742
      AlignToBottom = True
      BandType = btSummary
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_17_SistemaBeforePrint
      object rlLabel15: TRLLabel
        Left = 1
        Height = 12
        Top = 0
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
      object rllblSistema: TRLLabel
        Left = 352
        Height = 13
        Top = 1
        Width = 387
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
    end
    object rlb_06_ValorPrestacao: TRLBand[9]
      Left = 26
      Height = 47
      Top = 621
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_06_ValorPrestacaoBeforePrint
      object rlsQuadro05: TRLDraw
        Left = 0
        Height = 47
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw16: TRLDraw
        Left = 557
        Height = 1
        Top = 21
        Width = 184
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw18: TRLDraw
        Left = 372
        Height = 47
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw19: TRLDraw
        Left = 556
        Height = 47
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw15: TRLDraw
        Left = 186
        Height = 47
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompValor3: TRLMemo
        Left = 476
        Height = 36
        Top = 10
        Width = 78
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompValor2: TRLMemo
        Left = 290
        Height = 36
        Top = 10
        Width = 78
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompValor1: TRLMemo
        Left = 104
        Height = 36
        Top = 10
        Width = 78
        Alignment = taRightJustify
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompNome3: TRLMemo
        Left = 377
        Height = 36
        Top = 10
        Width = 96
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompNome2: TRLMemo
        Left = 190
        Height = 36
        Top = 10
        Width = 96
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCompNome1: TRLMemo
        Left = 5
        Height = 36
        Top = 10
        Width = 96
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'COMP 1'
          'COMP 2'
          'COMP 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllVlrTotServico: TRLLabel
        Left = 658
        Height = 14
        Top = 6
        Width = 78
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
      object rllVlrTotReceber: TRLLabel
        Left = 658
        Height = 14
        Top = 28
        Width = 78
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
      object rlLabel50: TRLLabel
        Left = 560
        Height = 9
        Top = 23
        Width = 96
        AutoSize = False
        Caption = 'VALOR A RECEBER'
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
      object rlLabel49: TRLLabel
        Left = 560
        Height = 9
        Top = 1
        Width = 96
        AutoSize = False
        Caption = 'VALOR TOTAL DO SERVIÇO'
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
      object rlLabel48: TRLLabel
        Left = 528
        Height = 8
        Top = 1
        Width = 26
        Caption = 'VALOR'
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
      object rlLabel47: TRLLabel
        Left = 377
        Height = 8
        Top = 1
        Width = 22
        Caption = 'NOME'
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
      object rlLabel46: TRLLabel
        Left = 156
        Height = 8
        Top = 1
        Width = 26
        Caption = 'VALOR'
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
      object rlLabel45: TRLLabel
        Left = 342
        Height = 8
        Top = 1
        Width = 26
        Caption = 'VALOR'
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
      object rlLabel44: TRLLabel
        Left = 5
        Height = 8
        Top = 1
        Width = 22
        Caption = 'NOME'
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
      object rlLabel42: TRLLabel
        Left = 190
        Height = 8
        Top = 1
        Width = 22
        Caption = 'NOME'
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
    end
    object rlb_12_ModAereo: TRLBand[10]
      Left = 26
      Height = 97
      Top = 851
      Width = 742
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_12_ModAereoBeforePrint
      object RLDraw47: TRLDraw
        Left = 0
        Height = 96
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw66: TRLDraw
        Left = 68
        Height = 22
        Top = 49
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw67: TRLDraw
        Left = 90
        Height = 22
        Top = 49
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw68: TRLDraw
        Left = 154
        Height = 22
        Top = 49
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw69: TRLDraw
        Left = 540
        Height = 24
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw65: TRLDraw
        Left = 260
        Height = 56
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw57: TRLDraw
        Left = 1
        Height = 1
        Top = 38
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw72: TRLDraw
        Left = 596
        Height = 56
        Top = 39
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw70: TRLDraw
        Left = 34
        Height = 26
        Top = 70
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllTrecho: TRLLabel
        Left = 2
        Height = 12
        Top = 58
        Width = 37
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
        Transparent = False
      end
      object rllTarifaValor: TRLLabel
        Left = 158
        Height = 13
        Top = 58
        Width = 95
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0,00'
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
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
      object rllTarifaCodigo: TRLLabel
        Left = 95
        Height = 12
        Top = 58
        Width = 22
        Caption = '1234'
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
        Transparent = False
      end
      object rllTarifaCL: TRLLabel
        Left = 72
        Height = 12
        Top = 58
        Width = 12
        Caption = '12'
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
        Transparent = False
      end
      object rllRetira: TRLLabel
        Left = 2
        Height = 13
        Top = 81
        Width = 26
        Alignment = taCenter
        AutoSize = False
        Caption = 'SIM'
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
      object rllMinuta: TRLLabel
        Left = 672
        Height = 19
        Top = 50
        Width = 65
        AutoSize = False
        Caption = '123456789'
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
      object rllLojaAgenteEmissor: TRLLabel
        Left = 598
        Height = 12
        Top = 81
        Width = 88
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
      object rllDadosRetira: TRLLabel
        Left = 39
        Height = 14
        Top = 81
        Width = 554
        AutoSize = False
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
        Transparent = False
      end
      object rllContaCorrente: TRLLabel
        Left = 262
        Height = 12
        Top = 49
        Width = 67
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
      object rllCaracAdTransporte: TRLLabel
        Left = 262
        Height = 12
        Top = 25
        Width = 85
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
      object rllCaracAdServico: TRLLabel
        Left = 6
        Height = 12
        Top = 25
        Width = 73
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
      object rllAWB: TRLLabel
        Left = 632
        Height = 19
        Top = 16
        Width = 105
        AutoSize = False
        Caption = '000-0-000000000-0'
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
      object rlLabel157: TRLLabel
        Left = 598
        Height = 8
        Top = 72
        Width = 92
        Caption = 'LOJA OU AGENTE EMISSOR'
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
      object rlLabel156: TRLLabel
        Left = 262
        Height = 8
        Top = 40
        Width = 65
        Caption = 'CONTA CORRENTE'
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
      object rlLabel155: TRLLabel
        Left = 262
        Height = 8
        Top = 16
        Width = 167
        Caption = 'CARACTERISTICAS ADICIONAIS DO TRANSPORTE'
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
      object rlLabel154: TRLLabel
        Left = 6
        Height = 8
        Top = 16
        Width = 152
        Caption = 'CARACTERISTICAS ADICIONAIS DO SERVIÇO'
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
      object rlLabel153: TRLLabel
        Left = 8
        Height = 13
        Top = 2
        Width = 730
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMAÇÕES ESPECÍFICAS DO MODAL AÉREO'
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
        Transparent = False
      end
      object rlLabel150: TRLLabel
        Left = 39
        Height = 8
        Top = 72
        Width = 149
        Caption = 'DADOS RELATIVOS A RETIRADA DA CARGA'
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
      object rlLabel149: TRLLabel
        Left = 2
        Height = 8
        Top = 72
        Width = 27
        Caption = 'RETIRA'
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
      object rlLabel148: TRLLabel
        Left = 598
        Height = 8
        Top = 40
        Width = 73
        Caption = 'NÚMERO DA MINUTA'
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
      object rlLabel147: TRLLabel
        Left = 158
        Height = 8
        Top = 50
        Width = 26
        Caption = 'VALOR'
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
      object rlLabel146: TRLLabel
        Left = 95
        Height = 8
        Top = 50
        Width = 29
        Caption = 'CÓDIGO'
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
      object rlLabel145: TRLLabel
        Left = 72
        Height = 8
        Top = 50
        Width = 11
        Caption = 'CL'
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
      object rlLabel144: TRLLabel
        Left = 2
        Height = 8
        Top = 50
        Width = 30
        Caption = 'TRECHO'
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
      object rlLabel142: TRLLabel
        Left = 8
        Height = 9
        Top = 40
        Width = 250
        Alignment = taCenter
        AutoSize = False
        Caption = 'DADOS DA TARIFA'
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
      object rlLabel141: TRLLabel
        Left = 543
        Height = 8
        Top = 16
        Width = 83
        Caption = 'NÚMERO OPERACIONAL'
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
      object RLDraw54: TRLDraw
        Left = 1
        Height = 1
        Top = 14
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw63: TRLDraw
        Left = 0
        Height = 1
        Top = 70
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw64: TRLDraw
        Left = 0
        Height = 1
        Top = 48
        Width = 260
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_13_ModAquaviario: TRLBand[11]
      Left = 26
      Height = 90
      Top = 761
      Width = 742
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_13_ModAquaviarioBeforePrint
      object RLDraw73: TRLDraw
        Left = 0
        Height = 89
        Top = 0
        Width = 741
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel151: TRLLabel
        Left = 6
        Height = 13
        Top = 3
        Width = 732
        Alignment = taCenter
        AutoSize = False
        Caption = 'DADOS ESPECÍFICOS DO MODAL AQUAVIÁRIO'
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
        Transparent = False
      end
      object RLDraw74: TRLDraw
        Left = 1
        Height = 1
        Top = 14
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel152: TRLLabel
        Left = 6
        Height = 8
        Top = 16
        Width = 77
        Caption = 'PORTO DE EMBARQUE'
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
      object rllPortoEmbarque: TRLLabel
        Left = 6
        Height = 12
        Top = 25
        Width = 71
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
      object rlLabel158: TRLLabel
        Left = 406
        Height = 8
        Top = 16
        Width = 67
        Caption = 'PORTO DE DESTINO'
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
      object rllPortoDestino: TRLLabel
        Left = 406
        Height = 12
        Top = 25
        Width = 64
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
      object RLDraw75: TRLDraw
        Left = 1
        Height = 1
        Top = 38
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel159: TRLLabel
        Left = 6
        Height = 8
        Top = 40
        Width = 141
        Caption = 'IDENTIFICAÇÃO DO NAVIO / REBOCADOR'
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
      object rllIndNavioRebocador: TRLLabel
        Left = 6
        Height = 12
        Top = 49
        Width = 89
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
      object RLDraw76: TRLDraw
        Left = 1
        Height = 1
        Top = 62
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel160: TRLLabel
        Left = 6
        Height = 8
        Top = 64
        Width = 116
        Caption = 'IDENTIFICAÇÃO DOS CONTEINERS'
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
      object rllIndConteiners: TRLLabel
        Left = 6
        Height = 12
        Top = 73
        Width = 66
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
      object RLDraw77: TRLDraw
        Left = 402
        Height = 48
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel162: TRLLabel
        Left = 406
        Height = 8
        Top = 40
        Width = 95
        Caption = 'VR DA B. DE CALC. AFRMM'
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
      object rllBCAFRMM: TRLLabel
        Left = 406
        Height = 12
        Top = 49
        Width = 57
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
      object rlLabel164: TRLLabel
        Left = 518
        Height = 8
        Top = 40
        Width = 56
        Caption = 'VLR DO AFRMM'
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
      object rllValorAFRMM: TRLLabel
        Left = 518
        Height = 12
        Top = 49
        Width = 66
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
      object rlLabel166: TRLLabel
        Left = 614
        Height = 8
        Top = 40
        Width = 74
        Caption = 'TIPO DE NAVEGAÇÃO'
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
      object rllTipoNav: TRLLabel
        Left = 614
        Height = 12
        Top = 49
        Width = 45
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
      object rlLabel168: TRLLabel
        Left = 694
        Height = 8
        Top = 40
        Width = 33
        Caption = 'DIREÇÃO'
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
      object rllDirecao: TRLLabel
        Left = 694
        Height = 12
        Top = 49
        Width = 41
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
      object RLDraw78: TRLDraw
        Left = 690
        Height = 24
        Top = 39
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw79: TRLDraw
        Left = 610
        Height = 24
        Top = 39
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw80: TRLDraw
        Left = 514
        Height = 24
        Top = 39
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_14_ModFerroviario: TRLBand[12]
      Left = 26
      Height = 6
      Top = 755
      Width = 742
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_14_ModFerroviarioBeforePrint
    end
    object rlb_15_ModDutoviario: TRLBand[13]
      Left = 26
      Height = 6
      Top = 749
      Width = 742
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_15_ModDutoviarioBeforePrint
    end
    object rlb_01_Recibo_Aereo: TRLBand[14]
      Left = 26
      Height = 146
      Top = 95
      Width = 742
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_01_Recibo_AereoBeforePrint
      object RLDraw10: TRLDraw
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
      end
      object RLDraw53: TRLDraw
        Left = 367
        Height = 78
        Top = 66
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw2: TRLDraw
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
      object rlLabel70: TRLLabel
        Left = 6
        Height = 12
        Top = 109
        Width = 15
        Caption = 'RG'
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
        Transparent = False
      end
      object rlLabel66: TRLLabel
        Left = 6
        Height = 12
        Top = 80
        Width = 30
        Caption = 'NOME'
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
        Transparent = False
      end
      object rlLabel65: TRLLabel
        Left = 6
        Height = 13
        Top = 2
        Width = 730
        Alignment = taCenter
        AutoSize = False
        Caption = 'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO ESTADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
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
        Transparent = False
      end
      object rlLabel19: TRLLabel
        Left = 6
        Height = 48
        Top = 17
        Width = 730
        AutoSize = False
        Caption = 'O Transporte coberto por este conhecimento se rege pelo código Brasileiro de Aeronáutica (Lei 7.565 de 19/12/1986), especificamente pelas regras relativas a responsabilidade Civil prevista nos artigos 193, 241, 244, 262 e 264, de cujo teor o Expedidor / Remetente declara concordar e ter plena ciência. O Expedidor / Remetente aceita como corretas todas as especificações impressas, manuscritas, datilografadas ou carimbadas neste conhecimento, certificando que os artigos perigosos descritos pela regulamentação da I.C.A.O. foram devidamente informados e acondicionados para transporte Aéreo.'
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
        Transparent = False
      end
      object RLDraw81: TRLDraw
        Left = 1
        Height = 1
        Top = 66
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw82: TRLDraw
        Left = 1
        Height = 1
        Top = 77
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel57: TRLLabel
        Left = 121
        Height = 8
        Top = 68
        Width = 88
        Alignment = taCenter
        Caption = 'EXPEDIDOR / REMETENTE'
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
      object rlLabel60: TRLLabel
        Left = 508
        Height = 8
        Top = 68
        Width = 100
        Alignment = taCenter
        Caption = 'DESTINATÁRIO / RECEBEDOR'
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
      object rlLabel69: TRLLabel
        Left = 206
        Height = 12
        Top = 80
        Width = 62
        Caption = 'DATA / HORA'
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
        Transparent = False
      end
      object rlLabel161: TRLLabel
        Left = 206
        Height = 12
        Top = 109
        Width = 61
        Caption = 'ASSINATURA'
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
        Transparent = False
      end
      object rlLabel67: TRLLabel
        Left = 374
        Height = 12
        Top = 80
        Width = 30
        Caption = 'NOME'
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
        Transparent = False
      end
      object rlLabel68: TRLLabel
        Left = 374
        Height = 12
        Top = 109
        Width = 15
        Caption = 'RG'
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
        Transparent = False
      end
      object rlLabel72: TRLLabel
        Left = 574
        Height = 12
        Top = 80
        Width = 62
        Caption = 'DATA / HORA'
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
        Transparent = False
      end
      object rlLabel163: TRLLabel
        Left = 574
        Height = 12
        Top = 109
        Width = 61
        Caption = 'ASSINATURA'
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
        Transparent = False
      end
    end
    object rlb_11_ModRodLot104: TRLBand[15]
      Left = 26
      Height = 108
      Top = 948
      Width = 742
      BandType = btColumnFooter
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_11_ModRodLot104BeforePrint
      object RLDraw4: TRLDraw
        Left = 0
        Height = 107
        Top = 0
        Width = 740
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw30: TRLDraw
        Left = 207
        Height = 105
        Top = 0
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw83: TRLDraw
        Left = 1
        Height = 1
        Top = 14
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw84: TRLDraw
        Left = 42
        Height = 69
        Top = 13
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw85: TRLDraw
        Left = 100
        Height = 69
        Top = 13
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw86: TRLDraw
        Left = 122
        Height = 69
        Top = 13
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw87: TRLDraw
        Left = 1
        Height = 1
        Top = 29
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw89: TRLDraw
        Left = 1
        Height = 1
        Top = 82
        Width = 740
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw90: TRLDraw
        Left = 345
        Height = 24
        Top = 82
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw92: TRLDraw
        Left = 330
        Height = 69
        Top = 13
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel167: TRLLabel
        Left = 2
        Height = 12
        Top = 15
        Width = 23
        Caption = 'TIPO'
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
        Transparent = False
      end
      object rlLabel169: TRLLabel
        Left = 214
        Height = 13
        Top = 1
        Width = 524
        Alignment = taCenter
        AutoSize = False
        Caption = 'INFORMAÇÕES REFERENTES AO VALE-PEDÁGIO'
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
        Transparent = False
      end
      object rlLabel170: TRLLabel
        Left = 2
        Height = 13
        Top = 1
        Width = 202
        Alignment = taCenter
        AutoSize = False
        Caption = 'IDENTIFICAÇÃO DO CONJ. TRANSPORTADOR'
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
        Transparent = False
      end
      object rlLabel171: TRLLabel
        Left = 351
        Height = 8
        Top = 84
        Width = 148
        Caption = 'IDENTIFICAÇÃO DOS LACRES EM TRÂNSITO'
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
      object rlLabel172: TRLLabel
        Left = 212
        Height = 8
        Top = 84
        Width = 69
        Caption = 'CPF DO MOTORISTA'
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
      object rlLabel173: TRLLabel
        Left = 4
        Height = 8
        Top = 84
        Width = 76
        Caption = 'NOME DO MOTORISTA'
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
      object rlLabel174: TRLLabel
        Left = 334
        Height = 8
        Top = 15
        Width = 87
        Caption = 'NÚMERO COMPROVANTE'
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
      object rlLabel177: TRLLabel
        Left = 618
        Height = 8
        Top = 15
        Width = 70
        Caption = 'CNPJ RESPONSÁVEL'
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
      object rlLabel179: TRLLabel
        Left = 210
        Height = 8
        Top = 15
        Width = 68
        Caption = 'CNPJ FORNECEDOR'
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
      object rlLabel181: TRLLabel
        Left = 124
        Height = 12
        Top = 15
        Width = 32
        Caption = 'RNTRC'
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
        Transparent = False
      end
      object rlLabel182: TRLLabel
        Left = 102
        Height = 12
        Top = 15
        Width = 14
        Caption = 'UF'
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
        Transparent = False
      end
      object rlLabel183: TRLLabel
        Left = 44
        Height = 12
        Top = 15
        Width = 34
        Caption = 'PLACA'
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
        Transparent = False
      end
      object rlmUF2: TRLMemo
        Left = 102
        Height = 50
        Top = 32
        Width = 16
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Uf1'
          'Uf2'
          'Uf3'
          'Uf4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmTipo2: TRLMemo
        Left = 2
        Height = 50
        Top = 32
        Width = 36
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Tipo 1'
          'Tipo 2'
          'Tipo 3'
          'Tipo 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmRNTRC2: TRLMemo
        Left = 124
        Height = 50
        Top = 32
        Width = 77
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'RNTRC 1'
          'RNTRC 2'
          'RNTRC 3'
          'RNTRC 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmPlaca2: TRLMemo
        Left = 44
        Height = 50
        Top = 32
        Width = 53
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Placa 1'
          'Placa 2'
          'Placa 3'
          'Placa 4'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCNPJForn: TRLMemo
        Left = 210
        Height = 48
        Top = 32
        Width = 117
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Empresa 1'
          'Empresa 2'
          'Empresa 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmNumCompra: TRLMemo
        Left = 334
        Height = 48
        Top = 32
        Width = 275
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Transacao 1'
          'Transacao 2'
          'Transacao 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rllNomeMotorista2: TRLLabel
        Left = 4
        Height = 12
        Top = 93
        Width = 76
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
      object rllLacres2: TRLLabel
        Left = 351
        Height = 12
        Top = 93
        Width = 41
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
      object rllCPFMotorista2: TRLLabel
        Left = 212
        Height = 12
        Top = 93
        Width = 71
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
      object RLDraw98: TRLDraw
        Left = 614
        Height = 69
        Top = 13
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlmCNPJPg: TRLMemo
        Left = 618
        Height = 48
        Top = 32
        Width = 117
        AutoSize = False
        Color = clWhite
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Lines.Strings = (
          'Empresa 1'
          'Empresa 2'
          'Empresa 3'
        )
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
    end
    object rlb_18_Recibo: TRLBand[16]
      Left = 26
      Height = 68
      Top = 1056
      Width = 742
      BandType = btSummary
      Color = clWhite
      ParentColor = False
      RealBounds.Left = 0
      RealBounds.Top = 0
      RealBounds.Width = 0
      RealBounds.Height = 0
      BeforePrint = rlb_18_ReciboBeforePrint
      object RLDraw97: TRLDraw
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
      object RLDraw91: TRLDraw
        Left = 202
        Height = 52
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw93: TRLDraw
        Left = 473
        Height = 52
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw94: TRLDraw
        Left = 593
        Height = 52
        Top = 15
        Width = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw95: TRLDraw
        Left = 1
        Height = 1
        Top = 40
        Width = 201
        HelpContext = 1
        Angle = 0
        Brush.Style = bsClear
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object RLDraw96: TRLDraw
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
      object rlLabel175: TRLLabel
        Left = 480
        Height = 16
        Top = 49
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.CharSet = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel176: TRLLabel
        Left = 480
        Height = 16
        Top = 25
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = '__/__/__    __:__'
        Color = clWhite
        Font.CharSet = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        ParentColor = False
        ParentFont = False
        RealBounds.Left = 0
        RealBounds.Top = 0
        RealBounds.Width = 0
        RealBounds.Height = 0
      end
      object rlLabel180: TRLLabel
        Left = 647
        Height = 13
        Top = 17
        Width = 28
        Caption = 'CT-E'
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
      object rlLabel184: TRLLabel
        Left = 605
        Height = 12
        Top = 33
        Width = 14
        Caption = 'Nº '
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
        Transparent = False
      end
      object rlLabel185: TRLLabel
        Left = 605
        Height = 12
        Top = 47
        Width = 30
        Caption = 'SÉRIE:'
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
        Transparent = False
      end
      object rlLabel186: TRLLabel
        Left = 6
        Height = 13
        Top = 2
        Width = 732
        Alignment = taCenter
        AutoSize = False
        Caption = 'DECLARO QUE RECEBI OS VOLUMES DESTE CONHECIMENTO EM PERFEITO ESTADO PELO QUE DOU POR CUMPRIDO O PRESENTE CONTRATO DE TRANSPORTE'
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
        Transparent = False
      end
      object rlLabel187: TRLLabel
        Left = 6
        Height = 12
        Top = 21
        Width = 30
        Caption = 'NOME'
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
        Transparent = False
      end
      object rlLabel188: TRLLabel
        Left = 480
        Height = 9
        Top = 17
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = 'CHEGADA DATA/HORA'
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
      object rlLabel189: TRLLabel
        Left = 480
        Height = 9
        Top = 40
        Width = 108
        Alignment = taCenter
        AutoSize = False
        Caption = 'SAÍDA DATA/HORA'
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
      object rlLabel190: TRLLabel
        Left = 207
        Height = 13
        Top = 48
        Width = 262
        Alignment = taCenter
        AutoSize = False
        Caption = 'ASSINATURA / CARIMBO'
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
        Transparent = False
      end
      object rlLabel191: TRLLabel
        Left = 6
        Height = 12
        Top = 45
        Width = 15
        Caption = 'RG'
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
        Transparent = False
      end
      object rllSerie3: TRLLabel
        Left = 655
        Height = 13
        Top = 47
        Width = 50
        Alignment = taCenter
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
      object rllNumCTe3: TRLLabel
        Left = 638
        Height = 16
        Top = 33
        Width = 86
        Alignment = taRightJustify
        AutoSize = False
        Caption = '999999999'
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
        Transparent = False
      end
    end
  end
end
