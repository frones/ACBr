inherited frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato
  Left = 238
  Top = 54
  Caption = 'Manifesto - Retrato'
  ClientHeight = 637
  ClientWidth = 838
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  PixelsPerInch = 96
  TextHeight = 10
  inherited RLMDFe: TRLReport
    Tag = 1
    Left = 24
    Top = -312
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    Font.Height = -8
    Font.Name = 'Courier New'
    BeforePrint = rlMDFeBeforePrint
    object rlb_1_DadosManifesto: TRLBand
      Left = 26
      Top = 26
      Width = 742
      Height = 267
      BandType = btTitle
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_1_DadosManifestoBeforePrint
      object rlsQuadro4: TRLDraw
        Left = 0
        Top = 200
        Width = 752
        Height = 62
        Brush.Style = bsClear
      end
      object rlsQuadro3: TRLDraw
        Left = 352
        Top = 0
        Width = 400
        Height = 169
        Brush.Style = bsClear
      end
      object rlsQuadro2: TRLDraw
        Left = 0
        Top = 168
        Width = 752
        Height = 33
        Brush.Style = bsClear
      end
      object rlsQuadro1: TRLDraw
        Left = 0
        Top = 0
        Width = 353
        Height = 169
        Brush.Style = bsClear
      end
      object rlsHorizontal1: TRLDraw
        Left = 0
        Top = 218
        Width = 752
        Height = 1
      end
      object rlLabel8: TRLLabel
        Left = 456
        Top = 4
        Width = 273
        Height = 29
        AutoSize = False
        Caption = 
          'Documento Auxiliar de Manifesto Eletr'#195#180'nico de Documentos Fiscai' +
          's'
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
        Left = 356
        Top = 6
        Width = 89
        Height = 22
        Caption = 'DAMDFE'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rliLogo: TRLImage
        Left = 9
        Top = 57
        Width = 96
        Height = 96
        Center = True
      end
      object rlmEmitente: TRLMemo
        Left = 7
        Top = 10
        Width = 338
        Height = 39
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
        Top = 53
        Width = 232
        Height = 108
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
      object rlShape1: TRLDraw
        Left = 352
        Top = 35
        Width = 400
        Height = 1
      end
      object rlLabel74: TRLLabel
        Left = 510
        Top = 38
        Width = 63
        Height = 11
        Caption = 'Controle do Fisco'
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
      object rlShape2: TRLDraw
        Left = 352
        Top = 120
        Width = 400
        Height = 1
      end
      object rlLabel1: TRLLabel
        Left = 358
        Top = 126
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
        Left = 358
        Top = 148
        Width = 386
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
        Left = 5
        Top = 171
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
        Top = 182
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
      object rlLabel3: TRLLabel
        Left = 39
        Top = 171
        Width = 22
        Height = 8
        Alignment = taCenter
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
        Left = 40
        Top = 182
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
      object rlLabel4: TRLLabel
        Left = 64
        Top = 171
        Width = 70
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'N'#195#353'MERO'
        Color = clWhite
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
        Left = 62
        Top = 182
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
      object rlLabel25: TRLLabel
        Left = 138
        Top = 171
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
        Left = 138
        Top = 182
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
        Left = 182
        Top = 171
        Width = 124
        Height = 9
        Alignment = taCenter
        AutoSize = False
        Caption = 'DATA E HORA DE EMISS'#195#402'O'
        Color = clWhite
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
        Left = 182
        Top = 182
        Width = 124
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
      object rlLabel77: TRLLabel
        Left = 312
        Top = 171
        Width = 35
        Height = 8
        Caption = 'UF Carrega'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -7
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rllUFCarrega: TRLLabel
        Left = 312
        Top = 182
        Width = 34
        Height = 15
        Alignment = taCenter
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
      object rlsLinhaV09: TRLDraw
        Left = 308
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV08: TRLDraw
        Left = 180
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV07: TRLDraw
        Left = 136
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV06: TRLDraw
        Left = 62
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV05: TRLDraw
        Left = 38
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV10: TRLDraw
        Left = 352
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rllDescricao: TRLLabel
        Left = 358
        Top = 171
        Width = 141
        Height = 8
        Caption = 'PROTOCOLO DE AUTORIZA'#195#8225#195#402'O DE USO'
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
        Left = 358
        Top = 180
        Width = 386
        Height = 19
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
      object rllModal: TRLLabel
        Left = 8
        Top = 202
        Width = 738
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Modal Rodovi'#195#161'rio de Carga'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel6: TRLLabel
        Left = 4
        Top = 222
        Width = 117
        Height = 15
        AutoSize = False
        Caption = 'CIOT'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel5: TRLLabel
        Left = 130
        Top = 222
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'QTDE CT-e'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel7: TRLLabel
        Left = 224
        Top = 222
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'QTDE CTRC'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel10: TRLLabel
        Left = 318
        Top = 222
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'QTDE NF-e'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel11: TRLLabel
        Left = 412
        Top = 222
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'QTDE NF'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel12: TRLLabel
        Left = 600
        Top = 222
        Width = 96
        Height = 14
        Caption = 'PESO TOTAL (Kg)'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape3: TRLDraw
        Left = 126
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rlShape4: TRLDraw
        Left = 220
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rlShape5: TRLDraw
        Left = 314
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rlShape6: TRLDraw
        Left = 408
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rlShape7: TRLDraw
        Left = 596
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rllCIOT: TRLLabel
        Left = 4
        Top = 240
        Width = 118
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
      object rllqCTe: TRLLabel
        Left = 130
        Top = 240
        Width = 88
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
      object rllqCT: TRLLabel
        Left = 224
        Top = 240
        Width = 88
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
      object rllqNFe: TRLLabel
        Left = 318
        Top = 240
        Width = 88
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
      object rllqNF: TRLLabel
        Left = 412
        Top = 240
        Width = 88
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
      object rllPesoTotal: TRLLabel
        Left = 600
        Top = 240
        Width = 146
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
      object rlLabel23: TRLLabel
        Left = 506
        Top = 222
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'QTDE MDF-e'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rllqMDFe: TRLLabel
        Left = 506
        Top = 240
        Width = 88
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
      object rlShape19: TRLDraw
        Left = 502
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object RLBarcode1: TRLBarcode
        Left = 360
        Top = 53
        Width = 99
        Height = 57
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        BarcodeType = bcCode128C
      end
    end
    object rlb_2_Rodo: TRLBand
      Left = 26
      Top = 293
      Width = 742
      Height = 208
      BandType = btTitle
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_2_RodoBeforePrint
      object rlShape8: TRLDraw
        Left = 1
        Top = 0
        Width = 752
        Height = 201
        Brush.Style = bsClear
      end
      object rlLabel35: TRLLabel
        Left = 4
        Top = 4
        Width = 37
        Height = 14
        Caption = 'Ve'#195#173'culo'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel9: TRLLabel
        Left = 318
        Top = 4
        Width = 46
        Height = 14
        Caption = 'Condutor'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape9: TRLDraw
        Left = 0
        Top = 20
        Width = 752
        Height = 1
      end
      object rlLabel13: TRLLabel
        Left = 4
        Top = 24
        Width = 28
        Height = 14
        Caption = 'Placa'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel14: TRLLabel
        Left = 124
        Top = 24
        Width = 40
        Height = 14
        Caption = 'RNTRC'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel15: TRLLabel
        Left = 318
        Top = 24
        Width = 23
        Height = 14
        Caption = 'CPF'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel16: TRLLabel
        Left = 412
        Top = 24
        Width = 31
        Height = 14
        Caption = 'Nome'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape10: TRLDraw
        Left = 0
        Top = 40
        Width = 752
        Height = 1
      end
      object rlShape11: TRLDraw
        Left = 314
        Top = 0
        Width = 1
        Height = 200
      end
      object rlShape12: TRLDraw
        Left = 120
        Top = 20
        Width = 1
        Height = 80
      end
      object rlmPlaca: TRLMemo
        Left = 4
        Top = 45
        Width = 109
        Height = 52
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmRNTRC: TRLMemo
        Left = 124
        Top = 45
        Width = 141
        Height = 52
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmCPF: TRLMemo
        Left = 318
        Top = 45
        Width = 85
        Height = 148
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlShape13: TRLDraw
        Left = 408
        Top = 20
        Width = 1
        Height = 180
      end
      object rlmCondutor: TRLMemo
        Left = 412
        Top = 45
        Width = 333
        Height = 148
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlShape14: TRLDraw
        Left = 0
        Top = 100
        Width = 314
        Height = 1
      end
      object rlLabel18: TRLLabel
        Left = 4
        Top = 104
        Width = 62
        Height = 14
        Caption = 'Vale Ped'#195#161'gio'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape15: TRLDraw
        Left = 0
        Top = 120
        Width = 314
        Height = 1
      end
      object rlLabel19: TRLLabel
        Left = 4
        Top = 122
        Width = 89
        Height = 14
        Caption = 'Respons'#195#161'vel CNPJ'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel20: TRLLabel
        Left = 98
        Top = 122
        Width = 84
        Height = 14
        Caption = 'Fornecedor CNPJ'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel21: TRLLabel
        Left = 196
        Top = 122
        Width = 81
        Height = 14
        Caption = 'N. Comprovante'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape16: TRLDraw
        Left = 94
        Top = 120
        Width = 1
        Height = 80
      end
      object rlShape17: TRLDraw
        Left = 192
        Top = 120
        Width = 1
        Height = 80
      end
      object rlmRespCNPJ: TRLMemo
        Left = 4
        Top = 141
        Width = 86
        Height = 52
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmFornCNPJ: TRLMemo
        Left = 100
        Top = 141
        Width = 86
        Height = 52
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmNumComprovante: TRLMemo
        Left = 196
        Top = 141
        Width = 114
        Height = 52
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
    end
    object rlb_3_Aereo: TRLBand
      Left = 26
      Top = 501
      Width = 742
      Height = 40
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_3_AereoBeforePrint
    end
    object rlb_4_Aquav: TRLBand
      Left = 26
      Top = 541
      Width = 742
      Height = 120
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_4_AquavBeforePrint
      object rlShape20: TRLDraw
        Left = 0
        Top = 0
        Width = 752
        Height = 113
        Brush.Style = bsClear
      end
      object rlLabel24: TRLLabel
        Left = 4
        Top = 4
        Width = 107
        Height = 14
        Caption = 'C'#195#179'digo da Embarca'#195#167#195#163'o'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel26: TRLLabel
        Left = 214
        Top = 4
        Width = 103
        Height = 14
        Caption = 'Nome da Embarca'#195#167#195#163'o'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlShape21: TRLDraw
        Left = 206
        Top = 0
        Width = 1
        Height = 20
      end
      object rlShape22: TRLDraw
        Left = 0
        Top = 20
        Width = 752
        Height = 1
      end
      object rllCodEmbar: TRLLabel
        Left = 116
        Top = 2
        Width = 85
        Height = 16
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
      object rllNomeEmbar: TRLLabel
        Left = 322
        Top = 2
        Width = 423
        Height = 16
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
      object rlShape23: TRLDraw
        Left = 376
        Top = 20
        Width = 1
        Height = 92
      end
      object rlShape24: TRLDraw
        Left = 70
        Top = 20
        Width = 1
        Height = 92
      end
      object rlShape25: TRLDraw
        Left = 446
        Top = 20
        Width = 1
        Height = 92
      end
      object rlShape26: TRLDraw
        Left = 0
        Top = 40
        Width = 752
        Height = 1
      end
      object rlLabel27: TRLLabel
        Left = 6
        Top = 24
        Width = 35
        Height = 14
        Caption = 'C'#195#179'digo'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel28: TRLLabel
        Left = 78
        Top = 24
        Width = 172
        Height = 14
        Caption = 'Nome do Terminal de Carregamento'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlLabel29: TRLLabel
        Left = 382
        Top = 24
        Width = 35
        Height = 14
        Caption = 'C'#195#179'digo'
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
        Left = 454
        Top = 24
        Width = 187
        Height = 14
        Caption = 'Nome do Terminal de Descarregamento'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlmCodCarreg: TRLMemo
        Left = 6
        Top = 45
        Width = 59
        Height = 62
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha'
          '5 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmCodDescarreg: TRLMemo
        Left = 382
        Top = 45
        Width = 59
        Height = 62
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmNomeCarreg: TRLMemo
        Left = 78
        Top = 45
        Width = 291
        Height = 62
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object rlmNomeDescarreg: TRLMemo
        Left = 454
        Top = 45
        Width = 291
        Height = 62
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
    end
    object rlb_5_Ferrov: TRLBand
      Left = 26
      Top = 661
      Width = 742
      Height = 40
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_5_FerrovBeforePrint
    end
    object rlb_6_Observacao: TRLBand
      Left = 26
      Top = 701
      Width = 742
      Height = 152
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_6_ObservacaoBeforePrint
      object rlShape18: TRLDraw
        Left = 0
        Top = 0
        Width = 752
        Height = 137
        Brush.Style = bsClear
      end
      object rlLabel22: TRLLabel
        Left = 4
        Top = 4
        Width = 56
        Height = 14
        Caption = 'Observa'#195#167#195#163'o'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object rlmObservacao: TRLMemo
        Left = 4
        Top = 21
        Width = 735
        Height = 108
        AutoSize = False
        Behavior = [beSiteExpander]
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        Lines.Strings = (
          '1 Linha'
          '2 Linha'
          '3 Linha'
          '4 Linha')
        ParentColor = False
        ParentFont = False
      end
      object rllMsgTeste: TRLLabel
        Left = 11
        Top = 38
        Width = 724
        Height = 31
        Alignment = taCenter
        Caption = 'AMBIENTE DE HOMOLOGA'#195#8225#195#402'O - SEM VALOR FISCAL'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -27
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object rllDataHoraImpressao: TRLLabel
        Left = 2
        Top = 138
        Width = 77
        Height = 10
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
      object rllSistema: TRLLabel
        Left = 352
        Top = 139
        Width = 387
        Height = 11
        Alignment = taRightJustify
        AutoSize = False
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
    end
    object rlb_8_Documentos_Lista: TRLBand
      Left = 26
      Top = 885
      Width = 742
      Height = 22
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Borders.FixedLeft = True
      Borders.FixedRight = True
      Borders.FixedBottom = True
      Color = clWhite
      ParentColor = False
      object rlmChave1: TRLDBText
        Left = 5
        Top = 2
        Width = 366
        Height = 16
        AutoSize = False
        Color = clWhite
        DataField = 'CHAVE1'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
      end
      object rls2: TRLDraw
        Left = 374
        Top = -1
        Width = 1
        Height = 22
      end
      object rlmChave2: TRLDBText
        Left = 384
        Top = 2
        Width = 354
        Height = 16
        AutoSize = False
        Color = clWhite
        DataField = 'CHAVE2'
        DataSource = dsItens
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Text = ''
      end
    end
    object rlb_7_Documentos_Titulos: TRLBand
      Left = 26
      Top = 853
      Width = 742
      Height = 32
      BandType = btColumnHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = True
      Borders.DrawTop = False
      Borders.DrawRight = True
      Borders.DrawBottom = True
      Color = clWhite
      ParentColor = False
      object rlsQuadrado5: TRLDraw
        Left = 1
        Top = 0
        Width = 751
        Height = 20
        Brush.Style = bsClear
      end
      object rlLabel141: TRLLabel
        Left = 254
        Top = 4
        Width = 244
        Height = 12
        Alignment = taCenter
        Caption = 'RELA'#195#8225#195#402'O DOS DOCUMENTOS FISCAIS ELETR'#195#8221'NICOS'
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
      object rlLabel91: TRLLabel
        Left = 5
        Top = 22
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
      object rlLabel92: TRLLabel
        Left = 88
        Top = 22
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
      object rlLabel96: TRLLabel
        Left = 174
        Top = 22
        Width = 86
        Height = 8
        Caption = 'S'#195#8240'RIE/NRO. DOCUMENTO'
        Color = clWhite
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
        Left = 381
        Top = 22
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
        Left = 464
        Top = 22
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
        Left = 550
        Top = 22
        Width = 86
        Height = 8
        Caption = 'S'#195#8240'RIE/NRO. DOCUMENTO'
        Color = clWhite
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
