object FrmTroco: TFrmTroco
  Left = 211
  Top = 223
  Width = 575
  Height = 367
  Caption = 'Troco'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 80
    Width = 48
    Height = 13
    Caption = 'Descricao'
  end
  object Label3: TLabel
    Left = 136
    Top = 80
    Width = 24
    Height = 13
    Caption = 'Valor'
  end
  object Bevel1: TBevel
    Left = 260
    Top = 8
    Width = 1
    Height = 321
    Shape = bsLeftLine
  end
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 218
    Height = 20
    Caption = 'Lista de Cédulas e Moedas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 328
    Top = 16
    Width = 46
    Height = 16
    Caption = 'Troco:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object eDescricao: TEdit
    Left = 8
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object eValor: TEdit
    Left = 136
    Top = 96
    Width = 113
    Height = 21
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 128
    Width = 241
    Height = 201
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button3: TButton
    Left = 136
    Top = 48
    Width = 49
    Height = 25
    Caption = 'Listar'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 48
    Width = 57
    Height = 25
    Caption = 'Adicionar'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 192
    Top = 48
    Width = 57
    Height = 25
    Caption = 'Limpar'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Memo2: TMemo
    Left = 272
    Top = 120
    Width = 281
    Height = 209
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object bTroco: TButton
    Left = 299
    Top = 48
    Width = 225
    Height = 25
    Caption = 'Calcular Troco Completo'
    TabOrder = 7
    OnClick = bTrocoClick
  end
  object bTrocoDetalhado: TButton
    Left = 299
    Top = 80
    Width = 225
    Height = 25
    Caption = 'Caclcular Troco Detalhado'
    TabOrder = 8
    OnClick = bTrocoDetalhadoClick
  end
  object Button2: TButton
    Left = 72
    Top = 48
    Width = 57
    Height = 25
    Caption = 'Remover'
    TabOrder = 9
    OnClick = Button2Click
  end
  object edTroco: TEdit
    Left = 384
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 10
    Text = '165,23'
  end
  object ACBrTroco1: TACBrTroco
    StrCedula = 'Cédula'
    StrMoeda = 'Moeda'
    Left = 392
    Top = 184
  end
end
