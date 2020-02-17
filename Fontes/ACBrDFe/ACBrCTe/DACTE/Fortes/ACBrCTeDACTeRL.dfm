object frmDACTeRL: TfrmDACTeRL
  Left = 285
  Top = 185
  Caption = 'frmDACTeRL'
  ClientHeight = 296
  ClientWidth = 899
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
    Left = 136
    Top = 48
  end
  object Datasource1: TDataSource
    Left = 184
    Top = 48
  end
end
