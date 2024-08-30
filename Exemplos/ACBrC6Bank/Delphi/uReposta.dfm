object frmResposta: TfrmResposta
  Left = 0
  Top = 0
  Caption = 'Resposta'
  ClientHeight = 462
  ClientWidth = 708
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Retorno de Envio'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 112
    ExplicitTop = 16
    ExplicitWidth = 185
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 708
    Height = 421
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    ExplicitLeft = 168
    ExplicitTop = 152
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
