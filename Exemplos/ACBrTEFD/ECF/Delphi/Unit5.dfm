object Form5: TForm5
  Left = 365
  Top = 160
  Width = 525
  Height = 203
  Caption = 'OnObtemCampo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    509
    165)
  PixelsPerInch = 96
  TextHeight = 24
  object Edit1: TEdit
    Left = 32
    Top = 72
    Width = 443
    Height = 32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 84
    Top = 119
    Width = 70
    Height = 35
    Anchors = [akTop]
    Caption = '&OK'
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 187
    Top = 119
    Width = 102
    Height = 35
    Anchors = [akTop]
    TabOrder = 4
    Kind = bkCancel
  end
  object BitBtn3: TBitBtn
    Left = 323
    Top = 119
    Width = 102
    Height = 35
    Anchors = [akTop]
    TabOrder = 2
    Kind = bkRetry
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 509
    Height = 50
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 3
  end
end
