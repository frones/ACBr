object frlDAMDFeRL: TfrlDAMDFeRL
  Left = 200
  Height = 330
  Top = 259
  Width = 641
  Caption = 'frlDAMDFeRL'
  ClientHeight = 330
  ClientWidth = 641
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  LCLVersion = '1.3'
  object RLMDFe: TRLReport
    Left = 16
    Height = 1123
    Top = 16
    Width = 794
    DataSource = dsItens
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Margins.LeftMargin = 6
    Margins.TopMargin = 6
    Margins.RightMargin = 6
    Margins.BottomMargin = 6
    RealBounds.Left = 0
    RealBounds.Top = 0
    RealBounds.Width = 0
    RealBounds.Height = 0
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 'FortesReport (Open Source) v3.24(B14)  \251 Copyright © 1999-2008 Fortes Informática'
    DocumentInfo.ModDate = 0
    ViewerOptions = []
    FontEncoding = feNoEncoding
    DisplayName = 'Documento PDF'
    left = 388
    top = 48
  end
  object dsItens: TDatasource
    left = 472
    top = 46
  end
end
