object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 114
  ClientWidth = 165
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 12
    Width = 145
    Height = 45
    Caption = 'Gerar Conv'#234'nio 115'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 60
    Width = 145
    Height = 45
    Caption = 'Imprimir Nota Fiscal'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ACBrConvenio115: TACBrConvenio115
    Ano = 0
    Mes = 0
    Status = scv115Normal
    Left = 32
    Top = 12
  end
end
