object FormIncluirPagamento: TFormIncluirPagamento
  Left = 627
  Top = 239
  BorderStyle = bsDialog
  Caption = 'Incluir Pagamento'
  ClientHeight = 94
  ClientWidth = 269
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
    Left = 11
    Top = 8
    Width = 101
    Height = 13
    Caption = 'Forma de Pagamento'
    Color = clBtnFace
    ParentColor = False
  end
  object Label16: TLabel
    Left = 163
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Valor Pago'
    Color = clBtnFace
    ParentColor = False
  end
  object cbFormaPagamento: TComboBox
    Left = 11
    Top = 24
    Width = 137
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = '01 - Dinheiro'
    OnChange = seValorPagoChange
    Items.Strings = (
      '01 - Dinheiro'
      '02 - Cheque'
      '03 - Cart'#227'o de Cr'#233'dito'
      '04 - Cart'#227'o de D'#233'bito'
      '99 - Outros')
  end
  object btGravar: TBitBtn
    Left = 163
    Top = 56
    Width = 89
    Height = 28
    Caption = 'Gravar'
    Default = True
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
  end
  object btCancelar: TBitBtn
    Left = 11
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
    TabOrder = 1
  end
  object seValorPago: TSpinEdit
    Left = 163
    Top = 22
    Width = 86
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = seValorPagoChange
  end
end
