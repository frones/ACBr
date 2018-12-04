object frmNFeDAInutRL: TfrmNFeDAInutRL
  Left = 196
  Top = 397
  Caption = 'frmNFeDAInutRL'
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
  object RLNFeInut: TRLReport
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
    PreviewOptions.FormStyle = fsStayOnTop
    PreviewOptions.ShowModal = True
    PrintDialog = False
    ShowProgress = False
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Author = 
      'FortesReport 3.23 - Copyright © 1999-2009 Fortes Informática'
    DocumentInfo.Creator = 'Projeto ACBr (Componente NF-e)'
    DisplayName = 'Documento PDF'
    Left = 369
    Top = 174
  end
  object DataSource1: TDataSource
    Left = 428
    Top = 180
  end
end
