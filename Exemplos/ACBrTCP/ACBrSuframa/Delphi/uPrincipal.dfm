object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Demo ACBrSuframa'
  ClientHeight = 407
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Caption = 'N'#250'mero'
  end
  object Label2: TLabel
    Left = 135
    Top = 8
    Width = 25
    Height = 13
    Caption = 'CNPJ'
  end
  object edtSuframa: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'edtSuframa'
  end
  object btnConsultar: TButton
    Left = 304
    Top = 22
    Width = 92
    Height = 25
    Caption = 'Consultar'
    TabOrder = 2
    OnClick = btnConsultarClick
  end
  object memResposta: TMemo
    Left = 8
    Top = 53
    Width = 603
    Height = 346
    TabStop = False
    Lines.Strings = (
      'memResposta')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object edtCnpj: TEdit
    Left = 135
    Top = 24
    Width = 163
    Height = 21
    TabOrder = 1
    Text = 'edtCnpj'
  end
  object ACBrSuframa1: TACBrSuframa
    ProxyPort = '8080'
    Left = 148
    Top = 69
  end
end
