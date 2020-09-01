object Form1: TForm1
  Left = 562
  Top = 179
  Caption = 'Form1'
  ClientHeight = 289
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Gerar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ACBrADRCST1: TACBrADRCST
    Path = 'C:\lazarus\'
    Delimitador = '|'
    TrimString = True
    CurMascara = '#0.00'
    Left = 368
    Top = 48
  end
end
