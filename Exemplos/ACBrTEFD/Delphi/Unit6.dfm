object Form6: TForm6
  Left = 412
  Top = 291
  BorderStyle = bsDialog
  Caption = 'Op'#231#245'es - Banese'
  ClientHeight = 148
  ClientWidth = 301
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
  object BitBtn1: TBitBtn
    Left = 107
    Top = 112
    Width = 75
    Height = 25
    TabOrder = 0
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object RadioButton1: TRadioButton
    Left = 40
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Reimpress'#227'o'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 40
    Top = 49
    Width = 113
    Height = 17
    Caption = 'Cancelamento'
    TabOrder = 2
  end
  object RadioButton3: TRadioButton
    Left = 40
    Top = 74
    Width = 113
    Height = 17
    Caption = 'Fechamento'
    TabOrder = 3
  end
end
