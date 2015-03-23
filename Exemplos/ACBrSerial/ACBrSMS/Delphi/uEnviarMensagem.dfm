object frmEnviarMensagem: TfrmEnviarMensagem
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Enviar Mensagem'
  ClientHeight = 311
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Telefone'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 51
    Height = 13
    Caption = 'Mensagem'
  end
  object lblContador: TLabel
    Left = 431
    Top = 51
    Width = 12
    Height = 13
    Alignment = taRightJustify
    Caption = '...'
  end
  object edtTelefone: TEdit
    Left = 8
    Top = 24
    Width = 136
    Height = 21
    TabOrder = 0
  end
  object btnEnviar: TButton
    Left = 237
    Top = 276
    Width = 100
    Height = 25
    Caption = 'Enviar'
    TabOrder = 4
    OnClick = btnEnviarClick
  end
  object memMensagem: TMemo
    Left = 8
    Top = 67
    Width = 435
    Height = 133
    TabOrder = 1
    OnChange = memMensagemChange
  end
  object btnCancelar: TButton
    Left = 343
    Top = 276
    Width = 100
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 5
    OnClick = btnCancelarClick
  end
  object ckbQuebrarMensagem: TCheckBox
    Left = 8
    Top = 206
    Width = 236
    Height = 15
    Caption = 'Quebrar mensagem grande e enviar em lote.'
    TabOrder = 2
  end
  object rdgBandeja: TRadioGroup
    Left = 8
    Top = 227
    Width = 435
    Height = 43
    Caption = 'Bandeja'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Sincard 1'
      'Sincard 2')
    TabOrder = 3
  end
end
