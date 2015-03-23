object frmGerencialFormatado: TfrmGerencialFormatado
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'frmGerencialFormatado'
  ClientHeight = 470
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object memRelatorio: TMemo
    Left = 8
    Top = 8
    Width = 398
    Height = 413
    Lines.Strings = (
      '<e>TEXTO EXPANDIDO</e>'
      '<n>TEXTO NEGRITO</n>'
      '<s>TEXTO SUBLINHADO</s>'
      '<c>TEXTO CONDENSADO</c>'
      '<i>TEXTO ITALICO</i>'
      '<ad>Alinhado a direita</ad>'
      '<ce>Centralizado</ce>'
      '</linha_dupla>'
      ''
      'CODIGOS DE BARRA'
      '</linha_simples>'
      'EAN 8'
      '<ean8>1234567</ean8>'
      ''
      'EAN 13'
      '<ean13>123456789012</ean13>'
      ''
      'STANDART 2 0F 5'
      '<std>12345678</std>'
      ''
      'INTERLEAVE 2 OF 5'
      '<inter>12345678</inter>'
      ''
      'CODE 11'
      '<code11>123456789</code11>'
      ''
      'CODE 39'
      '<code39>123456789ABCD</code39>'
      ''
      'CODE 93'
      '<code93>123456789ABCD</code93>'
      ''
      'CODE 128'
      '<code128>123456789ABCD</code128>'
      ''
      'UPCA'
      '<upca>12345678901</upca>'
      ''
      'CODABAR'
      '<codabar>A123456789A</codabar>'
      ''
      'MSI'
      '<msi>1234567890</msi>')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnImprimir: TButton
    Left = 95
    Top = 437
    Width = 75
    Height = 25
    Caption = 'Imprimir'
    TabOrder = 1
    OnClick = btnImprimirClick
  end
  object btnCancelar: TButton
    Left = 235
    Top = 437
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    TabOrder = 2
    OnClick = btnCancelarClick
  end
end
