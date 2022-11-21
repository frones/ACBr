object frmSelecionarCertificado: TfrmSelecionarCertificado
  Left = 355
  Top = 229
  ActiveControl = StringGrid1
  Caption = 'Selecionar Certificado'
  ClientHeight = 228
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 652
    Height = 183
    Align = alClient
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 183
    Width = 652
    Height = 45
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      652
      45)
    object BitBtn1: TBitBtn
      Left = 418
      Top = 5
      Width = 88
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      Left = 530
      Top = 5
      Width = 88
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
