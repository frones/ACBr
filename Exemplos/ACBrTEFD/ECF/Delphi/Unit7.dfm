object Form7: TForm7
  Left = 459
  Top = 207
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'TEF Dire'#231#227'o'
  ClientHeight = 188
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 296
    Top = 0
    Width = 86
    Height = 152
    Align = alRight
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 6
      Top = 73
      Width = 75
      Height = 30
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 6
      Top = 113
      Width = 75
      Height = 30
      Caption = 'Cancelar'
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 296
    Height = 152
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object pnlInformacao: TPanel
    Left = 0
    Top = 152
    Width = 382
    Height = 36
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
  end
end
