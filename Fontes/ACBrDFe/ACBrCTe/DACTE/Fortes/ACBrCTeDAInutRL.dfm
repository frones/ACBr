object frmCTeDAInutRL: TfrmCTeDAInutRL
  Left = 196
  Top = 397
  Caption = 'frmCTeDAInutRL'
  ClientHeight = 404
  ClientWidth = 880
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
  object RLCTeInut: TRLReport
    Left = 21
    Top = 51
    Width = 794
    Height = 1123
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Author = 
      'FortesReport 3.23 - Copyright '#195#8218#194#169' 1999-2009 Fortes Inform'#195#402#194#161'ti' +
      'ca'
    DocumentInfo.Creator = 'Projeto ACBr (Componente CT-e)'
    DisplayName = 'Documento  PDF'
    Left = 368
    Top = 176
  end
  object DataSource1: TDataSource
    Left = 440
    Top = 176
  end
end
