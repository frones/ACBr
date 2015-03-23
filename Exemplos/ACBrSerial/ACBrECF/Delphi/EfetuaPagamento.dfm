object frPagamento: TfrPagamento
  Left = 275
  Top = 165
  Width = 421
  Height = 326
  HorzScrollBar.Range = 327
  VertScrollBar.Range = 245
  BorderStyle = bsSingle
  Caption = 'Efetuar Pagamento'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PgPagamento: TPageControl
    Left = 0
    Top = 0
    Width = 415
    Height = 298
    ActivePage = TabSheetPagamento
    Align = alClient
    TabOrder = 0
    object TabSheetPagamento: TTabSheet
      Caption = 'Pagamento '
      object SpeedButton1: TSpeedButton
        Left = 80
        Top = 75
        Width = 23
        Height = 22
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33033333333333333F8F3333333333333000333333333333F888333333333333
          000333333333333F888333333333333000333333333333F88833333333333300
          033333333FFF3F888333333000003B803333333F8883F8883333330EEEEE00B3
          3333338833388883333330EEEEEEE033333338F3333338F333330EEEEEEEEE03
          33333833F333383F33330EFEEEEEEE0333338F33F333338F33330EFEEEEEEE03
          33338F333F33338F33330EEFEEEEEE03333383F333FF338333330EEEFFEEEE03
          333338F3333338F3333330EEEEEEE0333333383FF333F8333333330EEEEE0333
          333333883FF88333333333300000333333333333888333333333}
        NumGlyphs = 2
        OnClick = SpeedButton1Click
      end
      object Label1: TLabel
        Left = 8
        Top = 83
        Width = 22
        Height = 13
        Caption = '&Cod.'
        FocusControl = edCod
      end
      object Label2: TLabel
        Left = 8
        Top = 115
        Width = 24
        Height = 13
        Caption = '&Valor'
        FocusControl = edValor
      end
      object Label3: TLabel
        Left = 160
        Top = 54
        Width = 106
        Height = 13
        Caption = 'Formas de Pagamento'
      end
      object Label4: TLabel
        Left = 136
        Top = 211
        Width = 99
        Height = 13
        Caption = 'TOTAL DO CUPOM:'
      end
      object Label5: TLabel
        Left = 160
        Top = 235
        Width = 68
        Height = 13
        Caption = 'TOTAL PAGO'
      end
      object lTotalAPAGAR: TLabel
        Left = 272
        Top = 211
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = '1000.00'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lTotalPago: TLabel
        Left = 272
        Top = 235
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = '1000.00'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label6: TLabel
        Left = 8
        Top = 7
        Width = 314
        Height = 13
        Caption = '- Entre com um ou v'#225'rios pagamentos at'#233' atingir o Total do Cupom'
        WordWrap = True
      end
      object Label7: TLabel
        Left = 8
        Top = 28
        Width = 313
        Height = 26
        AutoSize = False
        Caption = 
          '- Se o valor pago for superior ao Total do Cupom ser'#225' calculado ' +
          'o    TROCO'
        WordWrap = True
      end
      object Label8: TLabel
        Left = 8
        Top = 147
        Width = 22
        Height = 13
        Caption = '&Obs:'
        FocusControl = edObs
      end
      object mFormas: TMemo
        Left = 160
        Top = 69
        Width = 167
        Height = 128
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        OnEnter = mFormasEnter
      end
      object edCod: TEdit
        Left = 40
        Top = 75
        Width = 33
        Height = 21
        Cursor = crIBeam
        TabOrder = 1
      end
      object edValor: TEdit
        Left = 40
        Top = 107
        Width = 101
        Height = 21
        Cursor = crIBeam
        TabOrder = 2
        OnKeyPress = edValorKeyPress
      end
      object btImprimir: TButton
        Left = 32
        Top = 219
        Width = 75
        Height = 25
        Caption = '&Imprimir'
        TabOrder = 3
        OnClick = btImprimirClick
      end
      object cbVinc: TCheckBox
        Left = 16
        Top = 163
        Width = 129
        Height = 31
        Caption = 'Cupom &Vinculado'
        TabOrder = 4
      end
      object edObs: TEdit
        Left = 40
        Top = 139
        Width = 101
        Height = 21
        Cursor = crIBeam
        TabOrder = 5
      end
    end
    object TabSheetEstorno: TTabSheet
      Caption = 'Estorno'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object Label9: TLabel
        Left = 8
        Top = 57
        Width = 28
        Height = 13
        Caption = 'Canc.'
      end
      object Label10: TLabel
        Left = 9
        Top = 106
        Width = 26
        Height = 13
        Caption = 'Novo'
      end
      object Label11: TLabel
        Left = 11
        Top = 153
        Width = 24
        Height = 13
        Caption = 'Valor'
      end
      object Label12: TLabel
        Left = 43
        Top = 183
        Width = 76
        Height = 13
        Caption = 'Texto Adicional:'
      end
      object Label13: TLabel
        Left = 169
        Top = 44
        Width = 106
        Height = 13
        Caption = 'Formas de Pagamento'
      end
      object EdtTipoCanc: TEdit
        Left = 41
        Top = 53
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object EdtTipoNovo: TEdit
        Left = 41
        Top = 102
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object CbVincCancelado: TCheckBox
        Left = 44
        Top = 78
        Width = 105
        Height = 17
        Caption = 'Cupom Vinculado'
        TabOrder = 1
      end
      object CBVincNovo: TCheckBox
        Left = 42
        Top = 127
        Width = 106
        Height = 17
        Caption = 'Cupom Vinculado'
        TabOrder = 3
      end
      object btnEstornar: TButton
        Left = 48
        Top = 228
        Width = 75
        Height = 25
        Caption = 'Estornar'
        TabOrder = 7
        OnClick = btnEstornarClick
      end
      object EdtValor: TEdit
        Left = 41
        Top = 149
        Width = 121
        Height = 21
        TabOrder = 4
      end
      object EdtMsgPromocional: TEdit
        Left = 40
        Top = 197
        Width = 281
        Height = 21
        TabOrder = 5
      end
      object MFormasEst: TMemo
        Left = 169
        Top = 59
        Width = 167
        Height = 128
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 6
        OnEnter = MFormasEstEnter
      end
      object MemoInformacaoEstorno: TMemo
        Left = 16
        Top = 7
        Width = 323
        Height = 31
        Color = clCream
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Somente podem ser estornados pagamentos efetivados '
          'no '#250'ltimo CF ou CNF')
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 8
      end
    end
  end
end
