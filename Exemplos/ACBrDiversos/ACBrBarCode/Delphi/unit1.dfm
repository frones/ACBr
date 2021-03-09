object Form1: TForm1
  Left = 486
  Top = 250
  Caption = 'Form1'
  ClientHeight = 306
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ACBrBarCode1: TACBrBarCode
    Left = 240
    Top = 80
    Width = 440
    Height = 64
    Modul = 3
    Ratio = 3.000000000000000000
    Typ = bcCodeEAN13
    ShowTextFont.Charset = DEFAULT_CHARSET
    ShowTextFont.Color = clWindowText
    ShowTextFont.Height = -11
    ShowTextFont.Name = 'Tahoma'
    ShowTextFont.Style = []
    ShowTextPosition = stpTopCenter
  end
  object Image1: TImage
    Left = 240
    Top = 192
    Width = 442
    Height = 90
  end
  object Label1: TLabel
    Left = 16
    Top = 14
    Width = 20
    Height = 13
    Caption = 'Tipo'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 25
    Height = 13
    Caption = 'Ratio'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 112
    Top = 80
    Width = 34
    Height = 13
    Caption = 'Modulo'
    Color = clBtnFace
    ParentColor = False
  end
  object edBarcode: TEdit
    Left = 240
    Top = 16
    Width = 440
    Height = 21
    TabOrder = 0
    Text = '34199786700000600001094444554946410024258000'
  end
  object btParaBarCode: TButton
    Left = 400
    Top = 48
    Width = 124
    Height = 25
    Caption = 'Para ACBrBarCode'
    TabOrder = 1
    OnClick = btParaBarCodeClick
  end
  object btParaImagem: TButton
    Left = 416
    Top = 152
    Width = 96
    Height = 25
    Caption = 'Para Imagem'
    TabOrder = 2
    OnClick = btParaImagemClick
  end
  object cbBarCodeType: TComboBox
    Left = 16
    Top = 32
    Width = 200
    Height = 21
    TabOrder = 3
    Text = 'cbBarCodeType'
    OnChange = cbBarCodeTypeChange
  end
  object seRatio: TSpinEdit
    Left = 16
    Top = 96
    Width = 64
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = seRatioChange
  end
  object seModule: TSpinEdit
    Left = 112
    Top = 96
    Width = 64
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = seModuleChange
  end
end
