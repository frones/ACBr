object frmDACTeRL: TfrmDACTeRL
  Left = 239
  Top = 218
  Width = 915
  Height = 335
  Caption = 'frmDACTeRL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RLCTe: TRLReport
    Left = 2
    Top = 2
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Margins.LeftMargin = 7.000000000000000000
    Margins.TopMargin = 7.000000000000000000
    Margins.RightMargin = 7.000000000000000000
    Margins.BottomMargin = 7.000000000000000000
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport (Open Source)  v3.24(B14)  \251 Copyright '#194#169' 1999-20' +
      '08 Fortes Inform'#195#161'tica'
    DisplayName = 'Documento PDF'
    Left = 136
    Top = 48
  end
  object Datasource1: TDataSource
    Left = 184
    Top = 48
  end
end
