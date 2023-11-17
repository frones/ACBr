object frmCTeDAEventoRL: TfrmCTeDAEventoRL
  Left = 328
  Top = 244
  Caption = 'frmCTeDAEventoRL'
  ClientHeight = 346
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object RLCTeEvento: TRLReport
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
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source)  v3.24(B14)  \251 Copyright '#194#169' 1999-2' +
      '008 Fortes Inform'#195#161'tica'
    DisplayName = 'Documento PDF'
    Left = 351
    Top = 39
  end
  object Datasource1: TDataSource
    Left = 256
    Top = 76
  end
end
