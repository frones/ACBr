object frmTrocarBandeja: TfrmTrocarBandeja
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Trocar a bandeja'
  ClientHeight = 122
  ClientWidth = 222
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object rdgBandeja: TRadioGroup
    Left = 8
    Top = 8
    Width = 205
    Height = 76
    Caption = 'Bandeja'
    ItemIndex = 0
    Items.Strings = (
      'Sincard 1'
      'Sincard 2')
    TabOrder = 0
  end
  object btnTrocarChip: TButton
    Left = 7
    Top = 90
    Width = 100
    Height = 25
    Caption = 'Trocar chip'
    TabOrder = 1
    OnClick = btnTrocarChipClick
  end
  object btnCancelar: TButton
    Left = 113
    Top = 90
    Width = 100
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 2
    OnClick = btnCancelarClick
  end
end
