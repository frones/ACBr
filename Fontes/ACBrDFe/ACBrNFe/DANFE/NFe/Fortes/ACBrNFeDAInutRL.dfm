object frmNFeDAInutRL: TfrmNFeDAInutRL
  Left = 196
  Height = 404
  Top = 397
  Width = 880
  Caption = 'frmNFeDAInutRL'
  ClientHeight = 404
  ClientWidth = 880
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  object RLNFeInut: TRLReport
    Left = 21
    Height = 1123
    Top = 51
    Width = 794
    DataSource = DataSource1
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    RealBounds.Left = 0
    RealBounds.Top = 0
    RealBounds.Width = 0
    RealBounds.Height = 0
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Author = 'FortesReport 3.23 - Copyright Â© 1999-2009 Fortes InformÃ¡tica'
    DocumentInfo.Creator = 'Projeto ACBr (Componente NF-e)'
    DocumentInfo.ModDate = 0
    ViewerOptions = []
    FontEncoding = feNoEncoding
    DisplayName = 'Documento PDF'
    left = 369
    top = 174
  end
  object DataSource1: TDataSource
    left = 428
    top = 180
  end
end
