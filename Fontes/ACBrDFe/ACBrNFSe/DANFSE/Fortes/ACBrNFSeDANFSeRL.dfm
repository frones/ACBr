object frlDANFSeRL: TfrlDANFSeRL
  Left = 280
  Top = 152
  Caption = 'frlDANFSeRL'
  ClientHeight = 537
  ClientWidth = 850
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RLNFSe: TRLReport
    Left = -4
    Top = -2
    Width = 794
    Height = 1123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    PreviewOptions.ShowModal = True
    PreviewOptions.Caption = 'DANFe'
    ShowProgress = False
    OnNeedData = RLNFSeNeedData
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Author = 'FortesReport 3.23 - Copyright '#194#169' 1999-2009 Fortes Inform'#195#161'tica'
    DocumentInfo.Creator = 'Projeto ACBr (Componente NFS-e)'
    DisplayName = 'Documento PDF'
    Left = 368
    Top = 152
  end
end
