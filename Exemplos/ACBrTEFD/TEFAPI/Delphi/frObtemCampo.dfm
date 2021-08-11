object FormObtemCampo: TFormObtemCampo
  Left = 644
  Top = 278
  BorderStyle = bsDialog
  Caption = 'Entre com a Informa'#231#227'o'
  ClientHeight = 170
  ClientWidth = 422
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 420
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    422
    170)
  PixelsPerInch = 96
  TextHeight = 24
  object edtResposta: TEdit
    Left = 32
    Top = 80
    Width = 356
    Height = 32
    Anchors = [akLeft, akRight, akBottom]
    AutoSelect = False
    TabOrder = 0
    OnChange = edtRespostaChange
    OnKeyDown = edtRespostaKeyDown
    OnKeyPress = edtRespostaKeyPress
  end
  object btOk: TBitBtn
    Left = 317
    Top = 127
    Width = 71
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    TabOrder = 1
    Kind = bkOK
  end
  object btCancel: TBitBtn
    Left = 187
    Top = 127
    Width = 103
    Height = 35
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object pTitulo: TPanel
    Left = 0
    Top = 0
    Width = 422
    Height = 56
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object lTitulo: TLabel
      Left = 1
      Top = 1
      Width = 420
      Height = 54
      Align = alClient
      Alignment = taCenter
      Caption = 'lTitulo'
      Layout = tlCenter
      WordWrap = True
    end
  end
  object btVoltar: TBitBtn
    Left = 32
    Top = 127
    Width = 97
    Height = 35
    Anchors = [akLeft, akBottom]
    Caption = 'Voltar'
    TabOrder = 2
    Kind = bkRetry
  end
end
