object frmCTeDAEventoRL: TfrmCTeDAEventoRL
  Left = 353
  Height = 357
  Top = 246
  Width = 842
  Caption = 'frmCTeDAEventoRL'
  ClientHeight = 357
  ClientWidth = 842
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object RLCTeEvento: TRLReport
    Left = 2
    Height = 1123
    Top = 2
    Width = 794
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Margins.LeftMargin = 7
    Margins.TopMargin = 7
    Margins.RightMargin = 7
    Margins.BottomMargin = 7
    RealBounds.Left = 0
    RealBounds.Top = 0
    RealBounds.Width = 0
    RealBounds.Height = 0
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 'FortesReport (Open Source) v3.24(B14)  \251 Copyright © 1999-2008 Fortes Informática'
    DisplayName = 'Documento PDF'
    left = 351
    top = 39
  end
  object Datasource1: TDatasource
    left = 256
    top = 76
  end
end
