object frlDAMDFeRL: TfrlDAMDFeRL
  Left = 200
  Top = 259
  Caption = 'frlDAMDFeRL'
  ClientHeight = 330
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RLMDFe: TRLReport
    Left = 16
    Top = 16
    Width = 794
    Height = 1123
    Margins.LeftMargin = 6.000000000000000000
    Margins.TopMargin = 6.000000000000000000
    Margins.RightMargin = 6.000000000000000000
    Margins.BottomMargin = 6.000000000000000000
    DataSource = dsItens
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#194#169' 1999-20' +
      '08 Fortes Inform'#195#161'tica'
    DisplayName = 'Documento PDF'
    Left = 388
    Top = 48
  end
  object dsItens: TDataSource
    Left = 472
    Top = 46
  end
end
