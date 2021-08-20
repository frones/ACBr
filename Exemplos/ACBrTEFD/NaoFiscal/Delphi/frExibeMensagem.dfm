object FormExibeMensagem: TFormExibeMensagem
  Left = 708
  Top = 289
  BorderStyle = bsDialog
  ClientHeight = 163
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClick = FormClick
  OnShow = FormShow
  DesignSize = (
    515
    163)
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TButton
    Left = 208
    Top = 116
    Width = 99
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnKeyDown = btOkKeyDown
  end
  object pMensagem: TPanel
    Left = 0
    Top = 0
    Width = 515
    Height = 92
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lMensagem: TLabel
      Left = 1
      Top = 1
      Width = 513
      Height = 90
      Align = alClient
      Alignment = taCenter
      Caption = 'Mensagem ao Operador'
      Layout = tlCenter
      WordWrap = True
      OnClick = lMensagemClick
    end
  end
  object tEspera: TTimer
    Enabled = False
    OnTimer = tEsperaTimer
    Left = 456
    Top = 120
  end
end
