object Form2: TForm2
  Left = 464
  Top = 194
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Cancelamento'
  ClientHeight = 174
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
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
    Width = 77
    Height = 13
    Caption = 'Data Transa'#231#227'o'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 133
    Top = 72
    Width = 23
    Height = 13
    Caption = 'Hora'
    Color = clBtnFace
    ParentColor = False
  end
  object Label5: TLabel
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
    Caption = 'CNC'
    ModalResult = 1
    TabOrder = 2
  end
  object meHora: TMaskEdit
    Left = 128
    Top = 90
    Width = 60
    Height = 21
    EditMask = '00:00:00;1;_'
    MaxLength = 8
    TabOrder = 3
    Text = '  :  :  '
  end
  object edValor: TEdit
    Left = 199
    Top = 91
    Width = 97
    Height = 21
    TabOrder = 4
    Text = '0,00'
  end
  object edData: TMaskEdit
    Left = 16
    Top = 90
    Width = 87
    Height = 21
    EditMask = '00/00/0000;1;_'
    MaxLength = 10
    TabOrder = 5
    Text = '  /  /    '
  end
end
