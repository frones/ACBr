object PrincipalView: TPrincipalView
  Left = 419
  Top = 55
  BorderStyle = bsSingle
  Caption = 'PrincipalView'
  ClientHeight = 465
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lb_idReference: TLabel
    Left = 8
    Top = 8
    Width = 82
    Height = 13
    Caption = 'id de Refer'#234'ncia:'
  end
  object lbProductName: TLabel
    Left = 8
    Top = 54
    Width = 87
    Height = 13
    Caption = 'Nome do Produto:'
  end
  object lbValue: TLabel
    Left = 8
    Top = 100
    Width = 44
    Height = 13
    Caption = 'Valor R$:'
  end
  object lbFirstName: TLabel
    Left = 8
    Top = 146
    Width = 31
    Height = 13
    Caption = 'Nome:'
  end
  object lb_LastName: TLabel
    Left = 8
    Top = 192
    Width = 58
    Height = 13
    Caption = 'Sobrenome:'
  end
  object lb_email: TLabel
    Left = 8
    Top = 277
    Width = 32
    Height = 13
    Caption = 'E-mail:'
  end
  object lb_Phone: TLabel
    Left = 8
    Top = 323
    Width = 46
    Height = 13
    Caption = 'Telefone:'
  end
  object Image1: TImage
    Left = 183
    Top = 27
    Width = 223
    Height = 213
    Proportional = True
    Stretch = True
  end
  object lbStatus: TLabel
    Left = 183
    Top = 416
    Width = 223
    Height = 41
    Alignment = taCenter
    AutoSize = False
    Caption = 'Status Pagamento'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object lb_Document: TLabel
    Left = 8
    Top = 235
    Width = 48
    Height = 13
    Caption = 'Document'
  end
  object txt_idReference: TEdit
    Left = 8
    Top = 27
    Width = 169
    Height = 21
    TabOrder = 0
    Text = '998800'
  end
  object txt_ProdutName: TEdit
    Left = 8
    Top = 73
    Width = 169
    Height = 21
    MaxLength = 20
    TabOrder = 1
    Text = 'Teste 123'
  end
  object txt_Value: TEdit
    Left = 8
    Top = 119
    Width = 169
    Height = 21
    TabOrder = 2
    Text = '1'
  end
  object txt_FirstName: TEdit
    Left = 8
    Top = 165
    Width = 169
    Height = 21
    TabOrder = 3
    Text = 'Jo'#227'o'
  end
  object txt_LastName: TEdit
    Left = 8
    Top = 211
    Width = 169
    Height = 21
    MaxLength = 20
    TabOrder = 4
    Text = 'Baldin'
  end
  object txt_email: TEdit
    Left = 8
    Top = 296
    Width = 169
    Height = 21
    MaxLength = 120
    TabOrder = 5
    Text = 'baldin@picpay.com'
  end
  object txt_Phone: TEdit
    Left = 8
    Top = 342
    Width = 169
    Height = 21
    TabOrder = 6
    Text = '+55 92 99300-9989'
  end
  object btnSolicitar: TButton
    Left = 8
    Top = 369
    Width = 169
    Height = 25
    Caption = 'Solicitar Pagamento'
    TabOrder = 7
    OnClick = btnSolicitarClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 400
    Width = 169
    Height = 57
    Lines.Strings = (
      'Memo1')
    TabOrder = 8
  end
  object txt_Document: TEdit
    Left = 8
    Top = 254
    Width = 169
    Height = 21
    MaxLength = 120
    TabOrder = 9
    Text = '000.000.001-91'
  end
  object Button1: TButton
    Left = 331
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 192
    Top = 254
    Width = 121
    Height = 21
    TabOrder = 11
  end
  object ACBrPicpay1: TACBrPicPay
    ProxyPort = '8080'
    Comprador.TipoInscricao = pFisica
    TempoRetorno = 0
    TipoRetorno = trThread
    OnStatusPayment = ACBrPicpay1StatusPayment
    OnWaitingPayment = ACBrPicpay1WaitingPayment
    OnWaitingTimeout = ACBrPicpay1WaitingTimeout
    Left = 232
    Top = 280
  end
end
