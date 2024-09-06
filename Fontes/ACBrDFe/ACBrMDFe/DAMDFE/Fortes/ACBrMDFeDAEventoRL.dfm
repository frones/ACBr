object frmMDFeDAEventoRL: TfrmMDFeDAEventoRL
  Left = 338
  Top = 317
  Caption = 'frmMDFeDAEventoRL'
  ClientHeight = 294
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object RLMDFeEvento: TRLReport
    Left = 2
    Top = 2
    Width = 794
    Height = 1123
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ShowProgress = False
  end
  object Datasource1: TDataSource
    Left = 292
    Top = 80
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source) v3.24(B14)  \251 Copyright '#194#169' 1999-20' +
      '08 Fortes Inform'#195#161'tica'
    DisplayName = 'Documento PDF'
    Left = 207
    Top = 79
  end
end
