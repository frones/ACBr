object FormObtemCampo: TFormObtemCampo
  Left = 634
  Top = 278
  Width = 612
  Height = 211
  Caption = 'OnObtemCampo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    596
    172)
  PixelsPerInch = 96
  TextHeight = 24
  object Edit1: TEdit
    Left = 32
    Top = 72
    Width = 513
    Height = 32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
  object BitBtn1: TBitBtn
    Left = 100
    Top = 119
    Width = 71
    Height = 35
    Anchors = [akTop]
    Caption = '&OK'
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 225
    Top = 119
    Width = 103
    Height = 35
    Anchors = [akTop]
    TabOrder = 2
    Kind = bkCancel
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 596
    Height = 50
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 3
  end
  object BitBtn3: TBitBtn
    Left = 386
    Top = 120
    Width = 89
    Height = 35
    Anchors = [akTop]
    TabOrder = 4
    Kind = bkRetry
  end
end
