inherited frlDAMDFeRLRetrato: TfrlDAMDFeRLRetrato
  Left = 244
  Top = 56
  Width = 838
  Height = 920
  AutoScroll = True
  Caption = 'Manifesto - Retrato'
  Font.Height = -8
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  PixelsPerInch = 96
  TextHeight = 10
  inherited RLMDFe: TRLReport
    Tag = 1
    Left = 3
    Top = 0
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
      BandType = btHeader
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_1_DadosManifestoBeforePrint
      object rlsQuadro4: TRLDraw
        Left = 0
        Top = 200
        Width = 742
        Height = 62
        Brush.Style = bsClear
      end
      object rlsQuadro3: TRLDraw
        Left = 351
        Top = -4
        Width = 390
        Height = 169
        Brush.Style = bsClear
      end
      object rlsQuadro2: TRLDraw
        Left = 1
        Top = 168
        Width = 740
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
        Width = 742
        Height = 1
      end
      object rlLabel8: TRLLabel
        Left = 455
        Top = 3
        Width = 281
        Height = 15
        AutoSize = False
        Caption = 'Documento Auxiliar de Manifesto Eletr'#244'nico de Documentos Fiscais'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
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
        Width = 390
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
        Width = 390
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
        Width = 380
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
        Left = 64
        Top = 182
        Width = 70
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
        Left = 136
        Top = 171
        Width = 32
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
      object rlLabel33: TRLLabel
        Left = 172
        Top = 171
        Width = 104
        Height = 9
        Alignment = taCenter
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
        Left = 172
        Top = 182
        Width = 104
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = '99/99/9999 99:99:99'
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
        Left = 279
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
        Left = 279
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
        Left = 277
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV08: TRLDraw
        Left = 169
        Top = 168
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rlsLinhaV07: TRLDraw
        Left = 135
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
        Caption = 'PROTOCOLO DE AUTORIZA'#199#195'O DE USO'
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
        Width = 380
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
        Width = 729
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Modal Rodovi'#225'rio de Carga'
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
        Left = 8
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
      object rlLabel10: TRLLabel
        Left = 210
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
      object rlLabel12: TRLLabel
        Left = 590
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
      object rlShape4: TRLDraw
        Left = 200
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rlShape7: TRLDraw
        Left = 580
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object rllqCTe: TRLLabel
        Left = 8
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
        Left = 210
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
        Left = 590
        Top = 240
        Width = 147
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
        Left = 410
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
        Left = 410
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
        Left = 400
        Top = 218
        Width = 1
        Height = 44
        Brush.Style = bsClear
      end
      object RLBarcode1: TRLBarcode
        Left = 360
        Top = 53
        Width = 370
        Height = 57
        Margins.LeftMargin = 1.000000000000000000
        Margins.RightMargin = 1.000000000000000000
        Alignment = taCenter
        AutoSize = False
        BarcodeType = bcCode128C
      end
      object RLLabel6: TRLLabel
        Left = 455
        Top = 19
        Width = 281
        Height = 15
        AutoSize = False
        Caption = 'Documentos Fiscais'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object RLDraw3: TRLDraw
        Left = 315
        Top = 167
        Width = 1
        Height = 33
        Brush.Style = bsClear
      end
      object rllUFDescarrega: TRLLabel
        Left = 317
        Top = 181
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
      object RLLabel11: TRLLabel
        Left = 317
        Top = 170
        Width = 34
        Height = 8
        Caption = 'UF Descar.'
        Color = clWhite
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
        Left = 136
        Top = 182
        Width = 32
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
        Top = 1
        Width = 741
        Height = 201
        Brush.Style = bsClear
      end
      object rlLabel35: TRLLabel
        Left = 4
        Top = 4
        Width = 37
        Height = 14
        Caption = 'Ve'#237'culo'
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
        Width = 742
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
        Width = 742
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
        Width = 325
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
        Caption = 'Vale Ped'#225'gio'
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
        Caption = 'Respons'#225'vel CNPJ'
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
      Height = 0
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_3_AereoBeforePrint
    end
    object rlb_4_Aquav: TRLBand
      Left = 26
      Top = 501
      Width = 742
      Height = 0
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_4_AquavBeforePrint
      object rlShape20: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 113
        Brush.Style = bsClear
      end
      object rlLabel24: TRLLabel
        Left = 4
        Top = 4
        Width = 107
        Height = 14
        Caption = 'C'#243'digo da Embarca'#231#227'o'
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
        Caption = 'Nome da Embarca'#231#227'o'
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
        Width = 742
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
        Width = 415
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
        Width = 742
        Height = 1
      end
      object rlLabel27: TRLLabel
        Left = 6
        Top = 24
        Width = 35
        Height = 14
        Caption = 'C'#243'digo'
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
        Caption = 'C'#243'digo'
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
        Width = 284
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
      Top = 501
      Width = 742
      Height = 0
      BandType = btColumnHeader
      Color = clWhite
      ParentColor = False
      Visible = False
      BeforePrint = rlb_5_FerrovBeforePrint
    end
    object rlb_6_Observacao: TRLBand
      Left = 26
      Top = 555
      Width = 742
      Height = 152
      BandType = btSummary
      Color = clWhite
      ParentColor = False
      BeforePrint = rlb_6_ObservacaoBeforePrint
      object rlShape18: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 137
        Brush.Style = bsClear
      end
      object rlLabel22: TRLLabel
        Left = 4
        Top = 4
        Width = 56
        Height = 14
        Caption = 'Observa'#231#227'o'
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
        Width = 734
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
      object rllDataHoraImpressao: TRLLabel
        Left = 1
        Top = 139
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
      Top = 533
      Width = 742
      Height = 22
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      object RLDraw1: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 22
        Brush.Style = bsClear
      end
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
      end
    end
    object rlb_7_Documentos_Titulos: TRLBand
      Left = 26
      Top = 501
      Width = 742
      Height = 32
      BandType = btColumnHeader
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = False
      Color = clWhite
      ParentColor = False
      object RLDraw2: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 33
        Brush.Style = bsClear
      end
      object rlsQuadrado5: TRLDraw
        Left = 0
        Top = 0
        Width = 742
        Height = 20
        Brush.Style = bsClear
      end
      object rlLabel141: TRLLabel
        Left = 254
        Top = 4
        Width = 244
        Height = 12
        Alignment = taCenter
        Caption = 'RELA'#199#195'O DOS DOCUMENTOS FISCAIS ELETR'#212'NICOS'
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
  end
  inherited RLPDFFilter1: TRLPDFFilter
    Left = 226
    Top = 0
  end
  inherited dsItens: TDataSource
    Left = 387
    Top = 2
  end
end
