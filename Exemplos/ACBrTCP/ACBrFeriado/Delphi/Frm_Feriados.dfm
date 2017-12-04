object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Demo Feriados'
  ClientHeight = 370
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  DesignSize = (
    566
    370)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 57
    Height = 13
    Caption = 'WebService'
  end
  object Label2: TLabel
    Left = 24
    Top = 70
    Width = 19
    Height = 13
    Caption = 'Ano'
  end
  object Label3: TLabel
    Left = 103
    Top = 70
    Width = 13
    Height = 13
    Caption = 'UF'
  end
  object Label4: TLabel
    Left = 182
    Top = 70
    Width = 33
    Height = 13
    Caption = 'Cidade'
  end
  object Label5: TLabel
    Left = 147
    Top = 11
    Width = 29
    Height = 13
    Alignment = taRightJustify
    Caption = 'Token'
  end
  object Label6: TLabel
    Left = 139
    Top = 38
    Width = 37
    Height = 13
    Alignment = taRightJustify
    Caption = 'Arquivo'
  end
  object btnBuscar: TButton
    Left = 24
    Top = 116
    Width = 75
    Height = 25
    Caption = 'Buscar'
    TabOrder = 0
    OnClick = btnBuscarClick
  end
  object cboWebService: TComboBox
    Left = 24
    Top = 43
    Width = 73
    Height = 21
    ItemIndex = 1
    TabOrder = 1
    Text = 'JSON'
    Items.Strings = (
      'Calendario'
      'JSON')
  end
  object edtAno: TEdit
    Left = 24
    Top = 89
    Width = 73
    Height = 21
    TabOrder = 2
  end
  object edtUF: TEdit
    Left = 103
    Top = 89
    Width = 73
    Height = 21
    TabOrder = 3
  end
  object edtCidade: TEdit
    Left = 182
    Top = 89
    Width = 203
    Height = 21
    TabOrder = 4
  end
  object mmResultado: TMemo
    Left = 24
    Top = 147
    Width = 521
    Height = 206
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
  end
  object edtToken: TEdit
    Left = 182
    Top = 8
    Width = 363
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object edtArquivo: TEdit
    Left = 182
    Top = 35
    Width = 292
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Button1: TButton
    Left = 480
    Top = 33
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Procurar'
    TabOrder = 8
    OnClick = Button1Click
  end
end
