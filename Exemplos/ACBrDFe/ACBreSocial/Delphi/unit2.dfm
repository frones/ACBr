object frSelecionarCertificado: TfrSelecionarCertificado
  Left = 355
  Top = 229
  ActiveControl = StringGrid1
  Caption = 'frSelecionarCertificado'
  ClientHeight = 227
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 658
    Height = 182
    Align = alClient
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
    TabOrder = 0
    ExplicitWidth = 666
    ExplicitHeight = 200
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24)
  end
  object Panel1: TPanel
    Left = 0
    Top = 182
    Width = 658
    Height = 45
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 200
    ExplicitWidth = 666
    DesignSize = (
      658
      45)
    object BitBtn1: TBitBtn
      Left = 436
      Top = 5
      Width = 88
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 444
    end
    object BitBtn2: TBitBtn
      Left = 548
      Top = 5
      Width = 88
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 556
    end
  end
end
