object Form4: TForm4
  Left = 506
  Top = 178
  Width = 525
  Height = 399
  Caption = 'OnExibeMenu'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 237
    Top = 50
    Width = 5
    Height = 261
    Align = alRight
    Visible = False
  end
  object ListBox1: TListBox
    Left = 0
    Top = 50
    Width = 237
    Height = 261
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 24
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 509
    Height = 50
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 311
    Width = 509
    Height = 50
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      509
      50)
    object BitBtn1: TBitBtn
      Left = 78
      Top = 9
      Width = 70
      Height = 35
      Anchors = [akTop]
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 187
      Top = 9
      Width = 102
      Height = 35
      Anchors = [akTop]
      TabOrder = 2
      Kind = bkCancel
    end
    object BitBtn3: TBitBtn
      Left = 329
      Top = 9
      Width = 102
      Height = 35
      Anchors = [akTop]
      TabOrder = 1
      Kind = bkRetry
    end
  end
  object Memo1: TMemo
    Left = 242
    Top = 50
    Width = 267
    Height = 261
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
    Visible = False
    WordWrap = False
  end
end
