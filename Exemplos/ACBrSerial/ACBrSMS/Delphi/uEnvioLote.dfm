object frmEnvioLote: TfrmEnvioLote
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Envio de mensagens em lote'
  ClientHeight = 310
  ClientWidth = 645
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
  object btnTrocarChip: TButton
    Left = 431
    Top = 277
    Width = 100
    Height = 25
    Caption = 'Enviar'
    TabOrder = 0
    OnClick = btnTrocarChipClick
  end
  object btnCancelar: TButton
    Left = 537
    Top = 277
    Width = 100
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 1
    OnClick = btnCancelarClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'arquivos de texto delimitado|*.txt'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Abrir arquivo'
    Left = 380
    Top = 100
  end
end
