object FormIncluirPagamento: TFormIncluirPagamento
  Left = 627
  Top = 239
  BorderStyle = bsDialog
  Caption = 'Incluir Pagamento'
  ClientHeight = 94
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label14: TLabel
    Left = 19
    Top = 8
    Width = 101
    Height = 13
    Caption = 'Forma de Pagamento'
    Color = clBtnFace
    ParentColor = False
  end
  object Label16: TLabel
    Left = 227
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Valor Pago'
    Color = clBtnFace
    ParentColor = False
  end
  object cbFormaPagamento: TComboBox
    Left = 19
    Top = 24
    Width = 182
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = seValorPagoChange
  end
  object btGravar: TBitBtn
    Left = 227
    Top = 56
    Width = 89
    Height = 28
    Caption = 'Gravar'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
  end
  object btCancelar: TBitBtn
    Left = 19
    Top = 56
    Width = 89
    Height = 28
    Cancel = True
    Caption = 'Cancelar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
  object seValorPago: TSpinEdit
    Left = 227
    Top = 22
    Width = 86
    Height = 22
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 0
    Value = 1
    OnChange = seValorPagoChange
  end
end
