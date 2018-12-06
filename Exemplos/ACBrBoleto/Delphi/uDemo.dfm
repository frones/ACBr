object frmDemo: TfrmDemo
  Left = 288
  Top = 133
  Width = 827
  Height = 577
  Caption = 'Demo ACBrBoleto '
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 785
    Height = 63
    Caption = 'Informa'#231#245'es Sobre a Cobran'#231'a'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 177
      Height = 13
      Caption = 'Mensagem para Local de Pagamento'
    end
    object Label2: TLabel
      Left = 383
      Top = 16
      Width = 70
      Height = 13
      Caption = 'Esp'#233'cie Docto'
    end
    object Label3: TLabel
      Left = 463
      Top = 16
      Width = 74
      Height = 13
      Caption = 'Esp'#233'cie Moeda'
    end
    object Label4: TLabel
      Left = 544
      Top = 16
      Width = 30
      Height = 13
      Caption = 'Aceite'
    end
    object Label5: TLabel
      Left = 624
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Carteira'
    end
    object Label6: TLabel
      Left = 672
      Top = 16
      Width = 70
      Height = 13
      Caption = 'Nosso N'#250'mero'
    end
    object edtLocalPag: TEdit
      Left = 8
      Top = 32
      Width = 369
      Height = 21
      TabOrder = 0
      Text = 'Pagar prefer'#234'ncialmente nas ag'#234'ncias do Bradesco'
    end
    object edtEspecieDoc: TEdit
      Left = 383
      Top = 32
      Width = 73
      Height = 21
      TabOrder = 1
      Text = 'DM'
    end
    object edtEspecieMod: TEdit
      Left = 463
      Top = 32
      Width = 76
      Height = 21
      TabOrder = 2
      Text = '$'
    end
    object cbxAceite: TComboBox
      Left = 544
      Top = 32
      Width = 73
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      Text = 'Sim'
      Items.Strings = (
        'Sim'
        'N'#227'o')
    end
    object edtCarteira: TEdit
      Left = 624
      Top = 32
      Width = 41
      Height = 21
      TabOrder = 4
      Text = '09'
    end
    object edtNossoNro: TEdit
      Left = 672
      Top = 32
      Width = 103
      Height = 21
      TabOrder = 5
      Text = '1'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 88
    Width = 609
    Height = 105
    Caption = 'Acr'#233'scimos\Descontos'
    TabOrder = 1
    object Label7: TLabel
      Left = 8
      Top = 16
      Width = 78
      Height = 13
      Caption = 'ValorMora\Juros'
    end
    object Label8: TLabel
      Left = 162
      Top = 16
      Width = 73
      Height = 13
      Caption = 'Valor Desconto'
    end
    object Label9: TLabel
      Left = 316
      Top = 16
      Width = 80
      Height = 13
      Caption = 'Valor Abatimento'
    end
    object Label10: TLabel
      Left = 466
      Top = 16
      Width = 37
      Height = 13
      Caption = '% Multa'
    end
    object Label11: TLabel
      Left = 8
      Top = 56
      Width = 83
      Height = 13
      Caption = 'Data Multa_Juros'
    end
    object Label12: TLabel
      Left = 162
      Top = 56
      Width = 72
      Height = 13
      Caption = 'Data Desconto'
    end
    object Label13: TLabel
      Left = 316
      Top = 56
      Width = 79
      Height = 13
      Caption = 'Data Abatimento'
    end
    object Label14: TLabel
      Left = 466
      Top = 56
      Width = 65
      Height = 13
      Caption = 'Data Protesto'
    end
    object edtMoraJuros: TEdit
      Left = 8
      Top = 32
      Width = 135
      Height = 21
      TabOrder = 0
      Text = '5'
    end
    object edtValorDesconto: TEdit
      Left = 162
      Top = 32
      Width = 135
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edtValorAbatimento: TEdit
      Left = 316
      Top = 32
      Width = 135
      Height = 21
      TabOrder = 2
      Text = '0'
    end
    object edtMulta: TEdit
      Left = 466
      Top = 32
      Width = 135
      Height = 21
      TabOrder = 3
      Text = '5'
    end
    object edtDataMora: TMaskEdit
      Left = 8
      Top = 72
      Width = 135
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 4
      Text = '  /  /  '
    end
    object edtDataDesconto: TMaskEdit
      Left = 162
      Top = 72
      Width = 135
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 5
      Text = '  /  /  '
    end
    object edtDataAbatimento: TMaskEdit
      Left = 316
      Top = 72
      Width = 135
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 6
      Text = '  /  /  '
    end
    object edtDataProtesto: TMaskEdit
      Left = 466
      Top = 72
      Width = 135
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 7
      Text = '  /  /  '
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 199
    Width = 609
    Height = 114
    Caption = 'Mensagens \ Instru'#231#245'es'
    TabOrder = 2
    object Label15: TLabel
      Left = 320
      Top = 16
      Width = 53
      Height = 13
      Caption = 'Instru'#231#227'o 1'
    end
    object Label16: TLabel
      Left = 464
      Top = 16
      Width = 53
      Height = 13
      Caption = 'Instru'#231#227'o 2'
    end
    object memMensagem: TMemo
      Left = 8
      Top = 16
      Width = 305
      Height = 89
      Lines.Strings = (
        '')
      TabOrder = 0
    end
    object edtInstrucoes1: TEdit
      Left = 320
      Top = 32
      Width = 137
      Height = 21
      TabOrder = 1
    end
    object edtInstrucoes2: TEdit
      Left = 464
      Top = 32
      Width = 137
      Height = 21
      TabOrder = 2
    end
    object Panel2: TPanel
      Left = 320
      Top = 64
      Width = 281
      Height = 41
      Caption = '* Informar o C'#243'digo do Instru'#231#227'o'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object GroupBox4: TGroupBox
    Left = 624
    Top = 88
    Width = 169
    Height = 225
    Caption = 'Informa'#231#245'es Sobre a Duplicata '
    TabOrder = 3
    object Label17: TLabel
      Left = 8
      Top = 16
      Width = 40
      Height = 13
      Caption = 'N'#250'mero '
    end
    object Label18: TLabel
      Left = 8
      Top = 64
      Width = 24
      Height = 13
      Caption = 'Valor'
    end
    object Label19: TLabel
      Left = 8
      Top = 120
      Width = 65
      Height = 13
      Caption = 'Data Emiss'#227'o'
    end
    object Label20: TLabel
      Left = 8
      Top = 176
      Width = 56
      Height = 13
      Caption = 'Vencimento'
    end
    object edtNumeroDoc: TEdit
      Left = 8
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 0
      Text = '0000000001'
    end
    object edtValorDoc: TEdit
      Left = 8
      Top = 80
      Width = 153
      Height = 21
      TabOrder = 1
      Text = '100'
    end
    object edtDataDoc: TMaskEdit
      Left = 8
      Top = 136
      Width = 153
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 2
      Text = '  /  /  '
    end
    object edtVencimento: TMaskEdit
      Left = 8
      Top = 192
      Width = 153
      Height = 21
      EditMask = '!99/99/00;1;_'
      MaxLength = 8
      TabOrder = 3
      Text = '  /  /  '
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 320
    Width = 786
    Height = 143
    Caption = 'Informa'#231#245'es do Sacado'
    TabOrder = 4
    object Label21: TLabel
      Left = 8
      Top = 16
      Width = 28
      Height = 13
      Caption = 'Nome'
    end
    object Label22: TLabel
      Left = 352
      Top = 16
      Width = 58
      Height = 13
      Caption = 'CPF / CNPJ'
    end
    object Label23: TLabel
      Left = 512
      Top = 16
      Width = 29
      Height = 13
      Caption = 'E-Mail'
    end
    object Label24: TLabel
      Left = 8
      Top = 56
      Width = 46
      Height = 13
      Caption = 'Endere'#231'o'
    end
    object Label25: TLabel
      Left = 328
      Top = 56
      Width = 37
      Height = 13
      Caption = 'N'#250'mero'
    end
    object Label26: TLabel
      Left = 400
      Top = 56
      Width = 64
      Height = 13
      Caption = 'Complemento'
    end
    object Label27: TLabel
      Left = 592
      Top = 56
      Width = 27
      Height = 13
      Caption = 'Bairro'
    end
    object Label28: TLabel
      Left = 8
      Top = 96
      Width = 33
      Height = 13
      Caption = 'Cidade'
    end
    object Label29: TLabel
      Left = 304
      Top = 96
      Width = 21
      Height = 13
      Caption = 'CEP'
    end
    object Label30: TLabel
      Left = 384
      Top = 96
      Width = 14
      Height = 13
      Caption = 'UF'
    end
    object Label31: TLabel
      Left = 424
      Top = 98
      Width = 34
      Height = 13
      Caption = 'LayOut'
      Color = clBtnFace
      ParentColor = False
    end
    object edtNome: TEdit
      Left = 8
      Top = 32
      Width = 337
      Height = 21
      TabOrder = 0
      Text = 'Joao Roberto Pirea'
    end
    object edtCPFCNPJ: TEdit
      Left = 352
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 1
      Text = '87.854.233-78'
    end
    object edtEmail: TEdit
      Left = 512
      Top = 32
      Width = 265
      Height = 21
      TabOrder = 2
      Text = 'joao@gmail.com'
    end
    object edtEndereco: TEdit
      Left = 8
      Top = 72
      Width = 313
      Height = 21
      TabOrder = 3
      Text = 'Rua XI de Agosto'
    end
    object edtNumero: TEdit
      Left = 328
      Top = 72
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '1000'
    end
    object edtComplemento: TEdit
      Left = 400
      Top = 72
      Width = 185
      Height = 21
      TabOrder = 5
    end
    object edtBairro: TEdit
      Left = 592
      Top = 72
      Width = 185
      Height = 21
      TabOrder = 6
      Text = 'Centro'
    end
    object edtCidade: TEdit
      Left = 8
      Top = 112
      Width = 289
      Height = 21
      TabOrder = 7
      Text = 'Tatui'
    end
    object edtCEP: TEdit
      Left = 304
      Top = 112
      Width = 73
      Height = 21
      TabOrder = 8
      Text = '18270-000'
    end
    object edtUF: TEdit
      Left = 384
      Top = 112
      Width = 33
      Height = 21
      TabOrder = 9
      Text = 'SP'
    end
    object cbxLayOut: TComboBox
      Left = 424
      Top = 114
      Width = 138
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
      OnChange = cbxLayOutChange
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 488
    Width = 811
    Height = 50
    Align = alBottom
    TabOrder = 5
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Gerar HTML'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 104
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Gerar PDF'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 192
      Top = 16
      Width = 131
      Height = 25
      Caption = 'Zerar Lista de Boletos'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 336
      Top = 16
      Width = 97
      Height = 25
      Caption = 'Incluir Boleto'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 448
      Top = 16
      Width = 129
      Height = 25
      Caption = 'Incluir V'#225'rios Boletos'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 592
      Top = 16
      Width = 97
      Height = 25
      Caption = 'Gerar Remessa'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 704
      Top = 16
      Width = 89
      Height = 25
      Caption = 'Imprimir'
      TabOrder = 6
      OnClick = Button7Click
    end
  end
end
