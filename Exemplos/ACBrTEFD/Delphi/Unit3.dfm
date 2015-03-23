object Form3: TForm3
  Left = 464
  Top = 194
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'CNF'
  ClientHeight = 174
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 15
    Width = 30
    Height = 13
    Caption = 'REDE'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 133
    Top = 15
    Width = 23
    Height = 13
    Caption = 'NSU'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 17
    Top = 72
    Width = 53
    Height = 13
    Caption = 'Finaliza'#231#227'o'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 198
    Top = 72
    Width = 24
    Height = 13
    Caption = 'Valor'
    Color = clBtnFace
    ParentColor = False
  end
  object cbxRede: TComboBox
    Left = 16
    Top = 36
    Width = 104
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'REDECARD'
      'VISANET'
      'AMEX')
  end
  object edNSU: TEdit
    Left = 133
    Top = 36
    Width = 163
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 128
    Top = 136
    Width = 75
    Height = 25
    Caption = 'CNF'
    ModalResult = 1
    TabOrder = 3
  end
  object edFinalizacao: TEdit
    Left = 17
    Top = 93
    Width = 163
    Height = 21
    TabOrder = 2
  end
  object edValor: TEdit
    Left = 199
    Top = 91
    Width = 97
    Height = 21
    TabOrder = 4
    Text = '0,00'
  end
end
