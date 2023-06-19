object frmDemoBoleto: TfrmDemoBoleto
  Left = 288
  Top = 133
  Caption = 'Demo ACBrBoleto '
  ClientHeight = 523
  ClientWidth = 986
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 986
    Height = 523
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Ficha de Pagamento'
      object GroupBox2: TGroupBox
        Left = -2
        Top = 0
        Width = 609
        Height = 105
        Caption = 'Acr'#233'scimos\Descontos'
        TabOrder = 0
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
      object GroupBox4: TGroupBox
        Left = 613
        Top = 3
        Width = 169
        Height = 225
        Caption = 'Informa'#231#245'es Sobre a Duplicata '
        TabOrder = 1
        object Label17: TLabel
          Left = 8
          Top = 58
          Width = 95
          Height = 13
          Caption = 'N'#250'mero Documento'
        end
        object Label18: TLabel
          Left = 8
          Top = 97
          Width = 24
          Height = 13
          Caption = 'Valor'
        end
        object Label19: TLabel
          Left = 8
          Top = 140
          Width = 65
          Height = 13
          Caption = 'Data Emiss'#227'o'
        end
        object Label20: TLabel
          Left = 8
          Top = 178
          Width = 56
          Height = 13
          Caption = 'Vencimento'
        end
        object Label6: TLabel
          Left = 8
          Top = 17
          Width = 70
          Height = 13
          Caption = 'Nosso N'#250'mero'
        end
        object edtNumeroDoc: TEdit
          Left = 8
          Top = 74
          Width = 153
          Height = 21
          TabOrder = 0
          Text = '0000000001'
        end
        object edtValorDoc: TEdit
          Left = 8
          Top = 113
          Width = 153
          Height = 21
          TabOrder = 1
          Text = '100'
        end
        object edtDataDoc: TMaskEdit
          Left = 8
          Top = 156
          Width = 153
          Height = 21
          EditMask = '!99/99/00;1;_'
          MaxLength = 8
          TabOrder = 2
          Text = '  /  /  '
        end
        object edtVencimento: TMaskEdit
          Left = 8
          Top = 194
          Width = 153
          Height = 21
          EditMask = '!99/99/00;1;_'
          MaxLength = 8
          TabOrder = 3
          Text = '  /  /  '
        end
        object edtNossoNro: TEdit
          Left = 8
          Top = 33
          Width = 153
          Height = 21
          TabOrder = 4
          Text = '12345'
        end
      end
      object GroupBox3: TGroupBox
        Left = -2
        Top = 111
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
      object PageControl2: TPageControl
        Left = 3
        Top = 231
        Width = 779
        Height = 157
        ActivePage = TabSheet5
        TabOrder = 3
        object TabSheet5: TTabSheet
          Caption = 'Pagador'
          ImageIndex = 1
          object Label28: TLabel
            Left = 3
            Top = 87
            Width = 33
            Height = 13
            Caption = 'Cidade'
          end
          object Label24: TLabel
            Left = 3
            Top = 45
            Width = 46
            Height = 13
            Caption = 'Endere'#231'o'
          end
          object Label21: TLabel
            Left = 3
            Top = 3
            Width = 28
            Height = 13
            Caption = 'Nome'
          end
          object Label22: TLabel
            Left = 346
            Top = 3
            Width = 58
            Height = 13
            Caption = 'CPF / CNPJ'
          end
          object Label25: TLabel
            Left = 328
            Top = 45
            Width = 37
            Height = 13
            Caption = 'N'#250'mero'
          end
          object Label29: TLabel
            Left = 304
            Top = 87
            Width = 21
            Height = 13
            Caption = 'CEP'
          end
          object Label30: TLabel
            Left = 384
            Top = 87
            Width = 14
            Height = 13
            Caption = 'UF'
          end
          object Label26: TLabel
            Left = 400
            Top = 45
            Width = 64
            Height = 13
            Caption = 'Complemento'
          end
          object Label23: TLabel
            Left = 505
            Top = 3
            Width = 29
            Height = 13
            Caption = 'E-Mail'
          end
          object Label27: TLabel
            Left = 585
            Top = 45
            Width = 27
            Height = 13
            Caption = 'Bairro'
          end
          object edtCidade: TEdit
            Left = 3
            Top = 104
            Width = 289
            Height = 21
            TabOrder = 7
            Text = 'Tatui'
          end
          object edtEndereco: TEdit
            Left = 3
            Top = 62
            Width = 313
            Height = 21
            TabOrder = 3
            Text = 'Rua dos clientes'
          end
          object edtNome: TEdit
            Left = 3
            Top = 20
            Width = 337
            Height = 21
            TabOrder = 0
            Text = 'CLIENTE DE TESTES'
          end
          object edtCPFCNPJ: TEdit
            Left = 346
            Top = 20
            Width = 153
            Height = 21
            TabOrder = 1
            Text = '676.387.808-76'
          end
          object edtNumero: TEdit
            Left = 328
            Top = 62
            Width = 65
            Height = 21
            TabOrder = 4
            Text = '100'
          end
          object edtCEP: TEdit
            Left = 304
            Top = 104
            Width = 73
            Height = 21
            TabOrder = 8
            Text = '18270-000'
          end
          object edtUF: TEdit
            Left = 384
            Top = 104
            Width = 33
            Height = 21
            TabOrder = 9
            Text = 'SP'
          end
          object edtComplemento: TEdit
            Left = 400
            Top = 62
            Width = 179
            Height = 21
            TabOrder = 5
          end
          object edtEmail: TEdit
            Left = 505
            Top = 20
            Width = 263
            Height = 21
            TabOrder = 2
            Text = 'testes@testes.com'
          end
          object edtBairro: TEdit
            Left = 585
            Top = 62
            Width = 183
            Height = 21
            TabOrder = 6
            Text = 'Centro'
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Benefici'#225'rio'
          object Label53: TLabel
            Left = 3
            Top = 3
            Width = 28
            Height = 13
            Caption = 'Nome'
          end
          object Label54: TLabel
            Left = 346
            Top = 3
            Width = 58
            Height = 13
            Caption = 'CPF / CNPJ'
          end
          object Label55: TLabel
            Left = 3
            Top = 45
            Width = 46
            Height = 13
            Caption = 'Endere'#231'o'
          end
          object Label56: TLabel
            Left = 328
            Top = 45
            Width = 37
            Height = 13
            Caption = 'N'#250'mero'
          end
          object Label57: TLabel
            Left = 400
            Top = 45
            Width = 64
            Height = 13
            Caption = 'Complemento'
          end
          object Label58: TLabel
            Left = 585
            Top = 45
            Width = 27
            Height = 13
            Caption = 'Bairro'
          end
          object Label59: TLabel
            Left = 3
            Top = 87
            Width = 33
            Height = 13
            Caption = 'Cidade'
          end
          object Label60: TLabel
            Left = 304
            Top = 87
            Width = 21
            Height = 13
            Caption = 'CEP'
          end
          object Label61: TLabel
            Left = 384
            Top = 87
            Width = 14
            Height = 13
            Caption = 'UF'
          end
          object Label62: TLabel
            Left = 500
            Top = 3
            Width = 40
            Height = 13
            Caption = 'Fantasia'
          end
          object Label63: TLabel
            Left = 423
            Top = 87
            Width = 42
            Height = 13
            Caption = 'Telefone'
          end
          object edtBenifRazao: TEdit
            Left = 3
            Top = 20
            Width = 337
            Height = 21
            TabOrder = 0
            Text = 'EMPRESA DE TESTES'
          end
          object edtBenifCNPJ: TEdit
            Left = 346
            Top = 20
            Width = 153
            Height = 21
            TabOrder = 1
            Text = '99.999.999/9999-62'
          end
          object edtBenifEndereco: TEdit
            Left = 3
            Top = 62
            Width = 313
            Height = 21
            TabOrder = 3
            Text = 'Rua das Empresas'
          end
          object edtBenifNum: TEdit
            Left = 328
            Top = 62
            Width = 65
            Height = 21
            TabOrder = 4
            Text = '100'
          end
          object edtBenifComplemento: TEdit
            Left = 400
            Top = 62
            Width = 179
            Height = 21
            TabOrder = 5
            Text = 'Compl'
          end
          object edtBenifBairro: TEdit
            Left = 580
            Top = 62
            Width = 183
            Height = 21
            TabOrder = 6
            Text = 'Centro'
          end
          object edtBenifCidade: TEdit
            Left = 3
            Top = 100
            Width = 289
            Height = 21
            TabOrder = 7
            Text = 'Tatui'
          end
          object edtBenifCEP: TEdit
            Left = 304
            Top = 100
            Width = 73
            Height = 21
            TabOrder = 8
            Text = '18270-000'
          end
          object edtBenifUF: TEdit
            Left = 384
            Top = 100
            Width = 33
            Height = 21
            TabOrder = 9
            Text = 'SP'
          end
          object edtBenifFantasia: TEdit
            Left = 500
            Top = 20
            Width = 263
            Height = 21
            TabOrder = 2
            Text = 'A Empresa'
          end
          object edtBenifTelefone: TEdit
            Left = 423
            Top = 100
            Width = 179
            Height = 21
            TabOrder = 10
            Text = '(11) 99999-9999'
          end
        end
      end
      object GroupBox6: TGroupBox
        Left = 575
        Top = 405
        Width = 382
        Height = 87
        Caption = 'Impress'#227'o'
        TabOrder = 4
        object btnImpressaoHTML: TButton
          Left = 52
          Top = 22
          Width = 75
          Height = 25
          Caption = 'Gerar HTML'
          TabOrder = 0
          OnClick = btnImpressaoHTMLClick
        end
        object btnImpressaoPDF: TButton
          Left = 133
          Top = 22
          Width = 75
          Height = 25
          Caption = 'Gerar PDF'
          TabOrder = 1
          OnClick = btnImpressaoPDFClick
        end
        object btnImpressaoSpooler: TButton
          Left = 151
          Top = 53
          Width = 98
          Height = 25
          Caption = 'Imprimir'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = btnImpressaoSpoolerClick
        end
        object btnImpressaoStream: TButton
          Left = 214
          Top = 22
          Width = 98
          Height = 25
          Caption = 'Imprimir Stream'
          TabOrder = 3
          OnClick = btnImpressaoStreamClick
        end
        object btnImpressaoPDFIndividual: TButton
          Left = 23
          Top = 53
          Width = 122
          Height = 25
          Caption = 'Gerar PDF Individual'
          TabOrder = 4
          OnClick = btnImpressaoPDFIndividualClick
        end
        object btnImprimirTeste: TButton
          Left = 255
          Top = 53
          Width = 98
          Height = 25
          Caption = 'Imprimir Teste'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
          OnClick = btnImprimirTesteClick
        end
      end
      object PageControl3: TPageControl
        Left = 5
        Top = 394
        Width = 564
        Height = 98
        ActivePage = TabSheet6
        TabOrder = 5
        object TabSheet6: TTabSheet
          Caption = 'Motores'
          object Label80: TLabel
            Left = 2
            Top = 3
            Width = 97
            Height = 13
            Caption = 'Motor Relat'#243'rio *'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object Label31: TLabel
            Left = 224
            Top = 3
            Width = 137
            Height = 13
            Caption = 'Layout Fortes Report / FPDF'
            Color = clBtnFace
            ParentColor = False
          end
          object Label83: TLabel
            Left = 3
            Top = 49
            Width = 196
            Height = 13
            Caption = '*descomente no projeto o motor desejado'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object Label84: TLabel
            Left = 411
            Top = 3
            Width = 55
            Height = 13
            Caption = 'Senha PDF'
          end
          object cbxMotorRelatorio: TComboBox
            Left = 2
            Top = 22
            Width = 198
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbxMotorRelatorioChange
          end
          object cbxLayOut: TComboBox
            Left = 224
            Top = 22
            Width = 161
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbxLayOutChange
          end
          object cbxImprimirVersoFatura: TCheckBox
            Left = 224
            Top = 49
            Width = 161
            Height = 17
            Caption = 'Imprimir Verso da Fatura'
            Enabled = False
            TabOrder = 2
          end
          object edtSenhaPDF: TEdit
            Left = 411
            Top = 22
            Width = 135
            Height = 21
            TabOrder = 3
            Text = 'ABC123'
          end
        end
        object TabSheet7: TTabSheet
          Caption = 'Path'#39's'
          ImageIndex = 1
          object Label81: TLabel
            Left = 3
            Top = 3
            Width = 20
            Height = 13
            Caption = 'FR3'
          end
          object Label82: TLabel
            Left = 255
            Top = 3
            Width = 53
            Height = 13
            Caption = 'Logomarca'
          end
          object edtPathFR3: TEdit
            Left = 2
            Top = 19
            Width = 247
            Height = 21
            TabOrder = 0
          end
          object edtPathLogoMarca: TEdit
            Left = 255
            Top = 19
            Width = 247
            Height = 21
            TabOrder = 1
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Conf. Carteira'
      ImageIndex = 1
      object Label65: TLabel
        Left = 255
        Top = 415
        Width = 123
        Height = 13
        Caption = 'Path Arquivo de Remessa'
      end
      object Label66: TLabel
        Left = 255
        Top = 453
        Width = 126
        Height = 13
        Caption = 'Path de Retorno + Arquivo'
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 3
        Width = 785
        Height = 117
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
          Left = 613
          Top = 15
          Width = 36
          Height = 13
          Caption = 'Carteira'
        end
        object Label32: TLabel
          Left = 8
          Top = 58
          Width = 31
          Height = 13
          Caption = 'Banco'
        end
        object Label49: TLabel
          Left = 255
          Top = 58
          Width = 79
          Height = 13
          Caption = 'Tipo Distribui'#231#227'o'
        end
        object Label51: TLabel
          Left = 511
          Top = 58
          Width = 104
          Height = 13
          Caption = 'Respons'#225'vel Emiss'#227'o'
        end
        object Label52: TLabel
          Left = 639
          Top = 58
          Width = 75
          Height = 13
          Caption = 'Tipo da Carteira'
        end
        object Label64: TLabel
          Left = 660
          Top = 15
          Width = 94
          Height = 13
          Caption = 'Tipo da Documento'
        end
        object Label50: TLabel
          Left = 383
          Top = 58
          Width = 60
          Height = 13
          Caption = 'Carac. Titulo'
        end
        object edtLocalPag: TEdit
          Left = 8
          Top = 32
          Width = 369
          Height = 21
          TabOrder = 2
          Text = 'Pagar prefer'#234'ncialmente nas ag'#234'ncias do Bradesco'
        end
        object edtEspecieDoc: TEdit
          Left = 383
          Top = 32
          Width = 73
          Height = 21
          TabOrder = 3
          Text = 'DM'
        end
        object edtEspecieMod: TEdit
          Left = 463
          Top = 32
          Width = 76
          Height = 21
          TabOrder = 4
          Text = '$'
        end
        object cbxAceite: TComboBox
          Left = 544
          Top = 32
          Width = 63
          Height = 21
          TabOrder = 5
          Text = 'Sim'
          Items.Strings = (
            'Sim'
            'N'#227'o')
        end
        object edtCarteira: TEdit
          Left = 613
          Top = 31
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '09'
        end
        object cbxBanco: TComboBox
          Left = 8
          Top = 77
          Width = 241
          Height = 21
          Style = csDropDownList
          TabOrder = 6
        end
        object cbxTipoDistribuicao: TComboBox
          Left = 255
          Top = 77
          Width = 122
          Height = 21
          Style = csDropDownList
          TabOrder = 7
        end
        object cbxResponsavelEmissao: TComboBox
          Left = 511
          Top = 77
          Width = 122
          Height = 21
          Style = csDropDownList
          TabOrder = 9
        end
        object cbxTipoCarteira: TComboBox
          Left = 639
          Top = 77
          Width = 122
          Height = 21
          Style = csDropDownList
          TabOrder = 10
        end
        object cbxTipoDocumento: TComboBox
          Left = 660
          Top = 31
          Width = 101
          Height = 21
          Style = csDropDownList
          TabOrder = 1
        end
        object cbxCaracteristicaTitulo: TComboBox
          Left = 383
          Top = 77
          Width = 122
          Height = 21
          Style = csDropDownList
          TabOrder = 8
        end
      end
      object GroupBox10: TGroupBox
        Left = 0
        Top = 130
        Width = 249
        Height = 162
        Caption = 'CNAB'
        TabOrder = 1
        object Label33: TLabel
          Left = 3
          Top = 16
          Width = 65
          Height = 13
          Caption = 'Vers'#227'o CNAB'
        end
        object Label34: TLabel
          Left = 93
          Top = 16
          Width = 43
          Height = 13
          Caption = 'L.V. Lote'
        end
        object Label35: TLabel
          Left = 165
          Top = 16
          Width = 58
          Height = 13
          Caption = 'L.V. Arquivo'
        end
        object Label45: TLabel
          Left = 3
          Top = 62
          Width = 110
          Height = 13
          Caption = 'Codigo de Transmiss'#227'o'
        end
        object Label46: TLabel
          Left = 119
          Top = 62
          Width = 101
          Height = 13
          Caption = 'Densidade Grava'#231#227'o'
        end
        object Label37: TLabel
          Left = 3
          Top = 110
          Width = 73
          Height = 13
          Caption = 'Prefix Remessa'
        end
        object cbxCNAB: TComboBox
          Left = 3
          Top = 35
          Width = 84
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'CNAB400'
            'CNAB240')
        end
        object edtCNABLVLote: TEdit
          Left = 93
          Top = 35
          Width = 66
          Height = 21
          TabOrder = 1
        end
        object edtCNABLVArquivo: TEdit
          Left = 165
          Top = 35
          Width = 59
          Height = 21
          TabOrder = 2
        end
        object edtCodigoTransmissao: TEdit
          Left = 3
          Top = 81
          Width = 110
          Height = 21
          TabOrder = 3
        end
        object edtDensidadeGravacao: TEdit
          Left = 119
          Top = 81
          Width = 101
          Height = 21
          TabOrder = 4
        end
        object edtPrefixRemessa: TEdit
          Left = 3
          Top = 129
          Width = 101
          Height = 21
          TabOrder = 5
        end
      end
      object GroupBox11: TGroupBox
        Left = 0
        Top = 298
        Width = 249
        Height = 70
        Caption = 'Outras Configura'#231#245'es'
        TabOrder = 2
        object Label36: TLabel
          Left = 3
          Top = 16
          Width = 17
          Height = 13
          Caption = 'CIP'
        end
        object edtCIP: TEdit
          Left = 3
          Top = 35
          Width = 66
          Height = 21
          TabOrder = 0
        end
      end
      object GroupBox12: TGroupBox
        Left = 255
        Top = 130
        Width = 530
        Height = 175
        Caption = 'Beneficiario'
        TabOrder = 3
        object Label38: TLabel
          Left = 11
          Top = 16
          Width = 39
          Height = 13
          Caption = 'Agencia'
        end
        object Label39: TLabel
          Left = 118
          Top = 16
          Width = 57
          Height = 13
          Caption = 'Agencia DV'
        end
        object Label40: TLabel
          Left = 225
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Conta'
        end
        object Label41: TLabel
          Left = 332
          Top = 16
          Width = 46
          Height = 13
          Caption = 'Conta DV'
        end
        object Label42: TLabel
          Left = 11
          Top = 67
          Width = 82
          Height = 13
          Caption = 'AgenciaContaDV'
        end
        object Label43: TLabel
          Left = 118
          Top = 67
          Width = 45
          Height = 13
          Caption = 'Convenio'
        end
        object Label44: TLabel
          Left = 225
          Top = 67
          Width = 55
          Height = 13
          Caption = 'Modalidade'
        end
        object Label47: TLabel
          Left = 332
          Top = 67
          Width = 47
          Height = 13
          Caption = 'Opera'#231#227'o'
        end
        object Label48: TLabel
          Left = 11
          Top = 122
          Width = 65
          Height = 13
          Caption = 'C'#243'd. Cedente'
        end
        object edtAgencia: TEdit
          Left = 11
          Top = 35
          Width = 101
          Height = 21
          TabOrder = 0
        end
        object edtAgenciaDV: TEdit
          Left = 118
          Top = 35
          Width = 101
          Height = 21
          TabOrder = 1
        end
        object edtConta: TEdit
          Left = 225
          Top = 35
          Width = 101
          Height = 21
          TabOrder = 2
        end
        object edtContaDV: TEdit
          Left = 332
          Top = 35
          Width = 101
          Height = 21
          TabOrder = 3
        end
        object edtAgenciaContaDV: TEdit
          Left = 11
          Top = 86
          Width = 101
          Height = 21
          TabOrder = 4
        end
        object edtConvenio: TEdit
          Left = 118
          Top = 86
          Width = 101
          Height = 21
          TabOrder = 5
        end
        object edtModalidade: TEdit
          Left = 225
          Top = 86
          Width = 101
          Height = 21
          TabOrder = 6
        end
        object edtOperacao: TEdit
          Left = 332
          Top = 86
          Width = 101
          Height = 21
          TabOrder = 7
        end
        object edtCodigoCedente: TEdit
          Left = 11
          Top = 141
          Width = 101
          Height = 21
          TabOrder = 8
        end
      end
      object ckbEmHomologacao: TCheckBox
        Left = 0
        Top = 382
        Width = 151
        Height = 17
        Caption = 'Boleto em Homologa'#231#227'o'
        TabOrder = 4
      end
      object ckbImprimirMensagemPadrao: TCheckBox
        Left = 0
        Top = 405
        Width = 224
        Height = 17
        Caption = 'Imprimir Msg. Padr'#227'o Componente/Banco'
        TabOrder = 5
      end
      object ckbLerCedenteArquivoRetorno: TCheckBox
        Left = 0
        Top = 428
        Width = 224
        Height = 17
        Caption = 'Ler Cedente do Arq. Retorno'
        TabOrder = 6
      end
      object ckbLerNossoNumeroCompleto: TCheckBox
        Left = 0
        Top = 449
        Width = 224
        Height = 17
        Caption = 'Ler Nosso N'#250'mero Completo'
        TabOrder = 7
      end
      object ckbRemoverAcentuacaoRemessa: TCheckBox
        Left = 0
        Top = 472
        Width = 224
        Height = 17
        Caption = 'Remover Acentua'#231#227'o Remessa'
        TabOrder = 8
      end
      object GroupBox14: TGroupBox
        Left = 255
        Top = 309
        Width = 530
        Height = 105
        Caption = 'WebServices '
        TabOrder = 9
        object Label67: TLabel
          Left = 11
          Top = 16
          Width = 37
          Height = 13
          Caption = 'ClientID'
        end
        object Label68: TLabel
          Left = 181
          Top = 16
          Width = 57
          Height = 13
          Caption = 'ClientSecret'
        end
        object Label69: TLabel
          Left = 351
          Top = 16
          Width = 40
          Height = 13
          Caption = 'KeyUser'
        end
        object Label70: TLabel
          Left = 351
          Top = 57
          Width = 37
          Height = 13
          Caption = 'SSL Lib'
        end
        object Label71: TLabel
          Left = 11
          Top = 57
          Width = 31
          Height = 13
          Caption = 'Scope'
        end
        object edtClientID: TEdit
          Left = 11
          Top = 33
          Width = 164
          Height = 21
          TabOrder = 0
        end
        object edtClientSecret: TEdit
          Left = 181
          Top = 33
          Width = 164
          Height = 21
          TabOrder = 1
        end
        object edtKeyUser: TEdit
          Left = 351
          Top = 33
          Width = 164
          Height = 21
          TabOrder = 2
        end
        object chkIndicadorPix: TCheckBox
          Left = 181
          Top = 76
          Width = 151
          Height = 17
          Caption = 'Indicador de Pix'
          TabOrder = 5
        end
        object cbxSSLLib: TComboBox
          Left = 351
          Top = 73
          Width = 164
          Height = 21
          Style = csDropDownList
          TabOrder = 4
        end
        object edtScope: TEdit
          Left = 11
          Top = 73
          Width = 164
          Height = 21
          TabOrder = 3
        end
      end
      object edtPathRemessa: TEdit
        Left = 255
        Top = 431
        Width = 369
        Height = 21
        TabOrder = 10
      end
      object edtPathRetorno: TEdit
        Left = 255
        Top = 466
        Width = 369
        Height = 21
        TabOrder = 11
      end
      object btnRetorno: TButton
        Left = 630
        Top = 464
        Width = 32
        Height = 25
        Caption = '...'
        TabOrder = 12
        OnClick = btnRetornoClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'E-Mail'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ImageIndex = 2
      ParentFont = False
      object Label72: TLabel
        Left = 17
        Top = 13
        Width = 74
        Height = 16
        Caption = 'From e-Mail:'
      end
      object Label73: TLabel
        Left = 336
        Top = 13
        Width = 74
        Height = 16
        Caption = 'From Name:'
      end
      object Label74: TLabel
        Left = 17
        Top = 69
        Width = 31
        Height = 16
        Caption = 'Host:'
      end
      object Label75: TLabel
        Left = 336
        Top = 69
        Width = 27
        Height = 16
        Caption = 'Port:'
      end
      object Label76: TLabel
        Left = 488
        Top = 69
        Width = 112
        Height = 16
        Caption = 'Tipo Autentica'#231#227'o:'
      end
      object Label77: TLabel
        Left = 17
        Top = 126
        Width = 72
        Height = 16
        Caption = 'User Name:'
      end
      object Label78: TLabel
        Left = 336
        Top = 126
        Width = 63
        Height = 16
        Caption = 'Password:'
      end
      object Label79: TLabel
        Left = 17
        Top = 462
        Width = 542
        Height = 16
        Cursor = crHandPoint
        Caption = 
          'Configura'#231#245'es do ACBrMail para os principais servi'#231'os de emails ' +
          'do mercado'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        OnClick = Label79Click
      end
      object edtFrom: TEdit
        Left = 17
        Top = 32
        Width = 304
        Height = 24
        TabOrder = 0
      end
      object edtFromName: TEdit
        Left = 336
        Top = 32
        Width = 417
        Height = 24
        TabOrder = 1
      end
      object edtHost: TEdit
        Left = 17
        Top = 88
        Width = 304
        Height = 24
        TabOrder = 2
      end
      object edtPort: TEdit
        Left = 336
        Top = 88
        Width = 121
        Height = 24
        TabOrder = 3
      end
      object chkTLS: TCheckBox
        Left = 488
        Top = 91
        Width = 57
        Height = 17
        Caption = 'TLS'
        TabOrder = 4
      end
      object chkSSL: TCheckBox
        Left = 560
        Top = 91
        Width = 65
        Height = 17
        Caption = 'SSL'
        TabOrder = 5
      end
      object edtUserName: TEdit
        Left = 17
        Top = 147
        Width = 304
        Height = 24
        TabOrder = 6
      end
      object chkMostrarSenha: TCheckBox
        Left = 405
        Top = 127
        Width = 84
        Height = 17
        Caption = 'Mostrar?'
        TabOrder = 7
        OnClick = chkMostrarSenhaClick
      end
      object edtPassword: TEdit
        Left = 336
        Top = 147
        Width = 289
        Height = 24
        TabOrder = 8
      end
      object btnEnviarEmail: TButton
        Left = 637
        Top = 147
        Width = 116
        Height = 25
        Caption = 'Enviar Email'
        TabOrder = 9
        OnClick = btnEnviarEmailClick
      end
    end
  end
  object grpFichaBancaria: TGroupBox
    Left = 795
    Top = 35
    Width = 166
    Height = 113
    Caption = 'Ficha Banc'#225'ria'
    TabOrder = 1
    object Button3: TButton
      Left = 11
      Top = 20
      Width = 145
      Height = 25
      Caption = 'Zerar Lista de Boletos'
      TabOrder = 0
      OnClick = Button3Click
    end
    object btnBoletoIndividual: TButton
      Left = 11
      Top = 50
      Width = 145
      Height = 25
      Caption = 'Incluir Boleto'
      TabOrder = 1
      OnClick = btnBoletoIndividualClick
    end
    object Button5: TButton
      Left = 11
      Top = 80
      Width = 145
      Height = 25
      Caption = 'Incluir V'#225'rios Boletos'
      TabOrder = 2
      OnClick = Button5Click
    end
  end
  object grpCNAB: TGroupBox
    Left = 795
    Top = 153
    Width = 166
    Height = 86
    Caption = 'CNAB '
    TabOrder = 2
    object btnGerarRemessa: TButton
      Left = 11
      Top = 20
      Width = 145
      Height = 25
      Caption = 'Gerar Remessa'
      TabOrder = 0
      OnClick = btnGerarRemessaClick
    end
    object btnLerRetorno: TButton
      Left = 11
      Top = 50
      Width = 145
      Height = 25
      Caption = 'Ler Retorno'
      TabOrder = 1
      OnClick = btnLerRetornoClick
    end
  end
  object grpWebServicesApi: TGroupBox
    Left = 795
    Top = 244
    Width = 166
    Height = 86
    Caption = 'WebServices / API '
    TabOrder = 3
    object btnWSConsulta: TButton
      Left = 11
      Top = 20
      Width = 145
      Height = 25
      Caption = 'Consultar Boleto'
      TabOrder = 0
      OnClick = btnWSConsultaClick
    end
    object btnWSRegistrar: TButton
      Left = 11
      Top = 50
      Width = 145
      Height = 25
      Caption = 'Registrar Boleto On Line'
      TabOrder = 1
      OnClick = btnWSRegistrarClick
    end
  end
  object GroupBox13: TGroupBox
    Left = 795
    Top = 335
    Width = 166
    Height = 88
    Caption = 'Ini '
    TabOrder = 4
    object btnConfigLer: TButton
      Left = 11
      Top = 20
      Width = 145
      Height = 25
      Caption = 'Ler Ini Config'
      TabOrder = 0
      OnClick = btnConfigLerClick
    end
    object btnConfigGravar: TButton
      Left = 11
      Top = 50
      Width = 145
      Height = 25
      Caption = 'Salvar Ini Config'
      TabOrder = 1
      OnClick = btnConfigGravarClick
    end
  end
  object flpndlgRetorno: TOpenDialog
    Filter = '*.txt|*.txt|*.ret|*.ret|*.*|*.*'
    Options = []
    Left = 711
    Top = 371
  end
end
