object frlDANFeRL: TfrlDANFeRL
  Left = 944
  Top = 336
  Caption = 'frlDANFeLR'
  ClientHeight = 634
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object RLNFe: TRLReport
    Left = 8
    Top = -2
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    PreviewOptions.ShowModal = True
    PreviewOptions.Caption = 'DANFe'
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Author = 'FortesReport 3.23 - Copyright '#169' 1999-2009 Fortes Inform'#225'tica'
    DocumentInfo.Creator = 'Projeto ACBr (Componente NF-e)'
    DisplayName = 'Documento PDF'
    Left = 288
    Top = 88
  end
end
