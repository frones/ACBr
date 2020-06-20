object FormMenuTEF: TFormMenuTEF
  Left = 563
  Top = 293
  Width = 624
  Height = 400
  Caption = 'Selecione uma op'#231#227'o'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 24
  object Splitter1: TSplitter
    Left = 283
    Top = 50
    Width = 5
    Height = 261
    Align = alRight
    Visible = False
  end
  object lbOpcoes: TListBox
    Left = 0
    Top = 50
    Width = 283
    Height = 261
    Align = alClient
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 24
    ParentFont = False
    TabOrder = 0
    OnClick = lbOpcoesClick
    OnKeyPress = lbOpcoesKeyPress
  end
  object pTitulo: TPanel
    Left = 0
    Top = 0
    Width = 608
    Height = 50
    Align = alTop
    Caption = 'pTitulo'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 311
    Width = 608
    Height = 50
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      608
      50)
    object btOK: TBitBtn
      Left = 512
      Top = 8
      Width = 71
      Height = 35
      Anchors = [akTop]
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
    end
    object btCancel: TBitBtn
      Left = 360
      Top = 8
      Width = 120
      Height = 35
      Anchors = [akTop]
      Caption = 'Cancelar'
      TabOrder = 2
      Kind = bkCancel
    end
    object btVoltar: TBitBtn
      Left = 24
      Top = 8
      Width = 97
      Height = 35
      Anchors = [akTop]
      Caption = 'Voltar'
      TabOrder = 1
      Kind = bkRetry
    end
  end
  object mOpcao: TMemo
    Left = 288
    Top = 50
    Width = 320
    Height = 261
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Visible = False
    WordWrap = False
  end
end
