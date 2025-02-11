object frACBrCupomVerdeTeste: TfrACBrCupomVerdeTeste
  Left = 582
  Top = 186
  Width = 482
  Height = 505
  Caption = 'ACBrCupomVerdeTeste'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 466
    Height = 466
    ActivePage = tsConsultarCPF
    Align = alClient
    TabOrder = 0
    object tsConsultarCPF: TTabSheet
      Caption = 'Consultar CPF'
      object lbCPF: TLabel
        Left = 40
        Top = 29
        Width = 20
        Height = 13
        Caption = 'CPF'
        Color = clBtnFace
        ParentColor = False
      end
      object edCPF: TEdit
        Left = 40
        Top = 43
        Width = 216
        Height = 21
        TabOrder = 0
        Text = '40864390858'
      end
      object btConsultar: TButton
        Left = 40
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Consultar'
        TabOrder = 1
        OnClick = btConsultarClick
      end
      object mmLog: TMemo
        Left = 40
        Top = 128
        Width = 384
        Height = 272
        ReadOnly = True
        TabOrder = 2
      end
    end
    object tsEnviarXML: TTabSheet
      Caption = 'Enviar XML'
      DesignSize = (
        458
        438)
      object btAcharXML: TSpeedButton
        Left = 400
        Top = 48
        Width = 24
        Height = 23
        Anchors = [akTop, akRight]
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000064000000640000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000065000000140000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000083000000F6000000540000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000600000088000000F70000006B000000000000000000000000000000000000
          000000000000000000050000004B00000096000000A800000081000000260000
          0076000000F30000007100000000000000000000000000000000000000000000
          00000000000800000098000000F4000000C0000000B1000000DD000000E90000
          00CF000000660000000300000000000000000000000000000000000000000000
          00000000006D000000F90000004C000000070000000200000013000000A10000
          00E5000000220000000000000000000000000000000000000000000000000000
          000A000000CF0000009500000000000000000000000000000000000000120000
          00DE000000770000000000000000000000000000000000000000000000000000
          001A000000E20000005D00000000000000000000000000000000000000040000
          00B80000009A0000000000000000000000000000000000000000000000000000
          0011000000D800000076000000000000000000000000000000000000000D0000
          00D4000000850000000000000000000000000000000000000000000000000000
          000000000094000000E200000019000000000000000000000000000000670000
          00F30000003B0000000000000000000000000000000000000000000000000000
          000000000017000000CB000000EA0000008900000068000000A1000000F70000
          0083000000020000000000000000000000000000000000000000000000000000
          000000000000000000160000008D000000D1000000DA000000C30000005A0000
          0004000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000B0000001300000005000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btAcharXMLClick
      end
      object lbCPF1: TLabel
        Left = 32
        Top = 88
        Width = 20
        Height = 13
        Caption = 'CPF'
        Color = clBtnFace
        ParentColor = False
      end
      object lbCodOperador: TLabel
        Left = 168
        Top = 88
        Width = 80
        Height = 13
        Caption = 'C'#243'digo Operador'
        Color = clBtnFace
        ParentColor = False
      end
      object lbCodDocumento: TLabel
        Left = 304
        Top = 88
        Width = 91
        Height = 13
        Caption = 'C'#243'digo Documento'
        Color = clBtnFace
        ParentColor = False
      end
      object lbXML2: TLabel
        Left = 32
        Top = 34
        Width = 22
        Height = 13
        Caption = 'XML'
        Color = clBtnFace
        ParentColor = False
      end
      object edXML: TEdit
        Left = 32
        Top = 48
        Width = 368
        Height = 21
        TabOrder = 0
      end
      object edCPF1: TEdit
        Left = 32
        Top = 102
        Width = 120
        Height = 21
        TabOrder = 1
        Text = '40864390858'
      end
      object edCodOperador: TEdit
        Left = 168
        Top = 102
        Width = 120
        Height = 21
        TabOrder = 2
      end
      object edCodDocumento: TEdit
        Left = 304
        Top = 102
        Width = 120
        Height = 21
        TabOrder = 3
      end
      object gbComprovantes: TGroupBox
        Left = 16
        Top = 146
        Width = 425
        Height = 110
        Caption = 'Comprovantes'
        TabOrder = 4
        object lbDescricao: TLabel
          Left = 16
          Top = 5
          Width = 48
          Height = 13
          Caption = 'Descri'#231#227'o'
          Color = clBtnFace
          ParentColor = False
        end
        object lbParcelas: TLabel
          Left = 152
          Top = 5
          Width = 41
          Height = 13
          Caption = 'Parcelas'
          Color = clBtnFace
          ParentColor = False
        end
        object lbValorTotal: TLabel
          Left = 288
          Top = 5
          Width = 51
          Height = 13
          Caption = 'Valor Total'
          Color = clBtnFace
          ParentColor = False
        end
        object lbComprovantes: TLabel
          Left = 152
          Top = 57
          Width = 120
          Height = 20
          Caption = 'Comprovantes: 0'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object edDescricao: TEdit
          Left = 16
          Top = 21
          Width = 120
          Height = 21
          TabOrder = 0
        end
        object edParcelas: TEdit
          Left = 152
          Top = 21
          Width = 120
          Height = 21
          TabOrder = 1
        end
        object edValorTotal: TEdit
          Left = 285
          Top = 21
          Width = 120
          Height = 21
          TabOrder = 2
        end
        object btAdicionar: TButton
          Left = 16
          Top = 53
          Width = 120
          Height = 25
          Caption = 'Adicionar'
          TabOrder = 3
          OnClick = btAdicionarClick
        end
      end
      object btEnviar: TButton
        Left = 32
        Top = 272
        Width = 120
        Height = 25
        Caption = 'Enviar'
        TabOrder = 5
        OnClick = btEnviarClick
      end
      object btLimpar: TButton
        Left = 168
        Top = 272
        Width = 120
        Height = 25
        Caption = 'Limpar'
        TabOrder = 6
        OnClick = btLimparClick
      end
      object mmLogEnviar: TMemo
        Left = 32
        Top = 320
        Width = 392
        Height = 96
        ReadOnly = True
        TabOrder = 7
      end
    end
    object tsCancelarDocumento: TTabSheet
      Caption = 'Cancelar Documento'
      object lbChave: TLabel
        Left = 40
        Top = 29
        Width = 31
        Height = 13
        Caption = 'Chave'
        Color = clBtnFace
        ParentColor = False
      end
      object edChave: TEdit
        Left = 40
        Top = 43
        Width = 384
        Height = 23
        TabOrder = 0
      end
      object btCancelar: TButton
        Left = 40
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Cancelar'
        TabOrder = 1
        OnClick = btCancelarClick
      end
      object mmLogCancelar: TMemo
        Left = 40
        Top = 128
        Width = 384
        Height = 272
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object ACBrCupomVerde1: TACBrCupomVerde
    ProxyPort = '8080'
    ContentsEncodingCompress = []
    NivelLog = 0
    xApiKey = '12c392ad-ec0b-4ec5-bd3b-605044ea4d34'
    Ambiente = cvaHomologacao
    Left = 88
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    Left = 192
    Top = 40
  end
end
