object frmPagamentos: TfrmPagamentos
  Left = 200
  Top = 173
  Width = 916
  Height = 476
  Caption = 'Pagamentos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgPagamentos: TPageControl
    Left = 0
    Top = 0
    Width = 900
    Height = 437
    ActivePage = tsLotePagamento
    Align = alClient
    Images = frPagamentosAPITeste.ImageList1
    TabHeight = 25
    TabOrder = 0
    TabWidth = 200
    object tsLotePagamento: TTabSheet
      Caption = 'Lote de Pagamento'
      ImageIndex = 33
      object pnLote: TPanel
        Left = 0
        Top = 0
        Width = 892
        Height = 402
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnLoteDados: TPanel
          Left = 0
          Top = 0
          Width = 892
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object lbNumRequisicao: TLabel
            Left = 30
            Top = 58
            Width = 93
            Height = 13
            Hint = 
              'N mero controlado pelo cliente, de 1 a 9999999, para identificar' +
              ' cada acionamento da API e os lan amentos de cada chamada. N o p' +
              'recisa ser sequencial. De uso  nico (por contrato de pagamento)'
            Caption = 'Numero Requisicao'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbCodigoContrato: TLabel
            Left = 224
            Top = 56
            Width = 80
            Height = 13
            Hint = 
              'Contrato (ou conv nio) de pagamento entre o terceiro e o Banco d' +
              'o Brasil'
            Caption = 'Codigo Contrato*'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbNumeroAgenciaDebito: TLabel
            Left = 419
            Top = 56
            Width = 138
            Height = 13
            Hint = 
              'N mero da ag ncia da Conta Corrente onde dever  ser efetuado o d' +
              ' bito do valor total de cada requisi  o. (4 algarismos sem o d g' +
              'ito verificador)'
            Caption = 'Agencia da Conta de Debito*'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbNumeroContaCorrenteDebito: TLabel
            Left = 614
            Top = 56
            Width = 127
            Height = 13
            Hint = 
              'N mero da Conta Corrente onde dever  ser efetuado o d bito do va' +
              'lor total de cada requisi  o'
            Caption = 'Conta Corrente de  Debito*'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbDigitoCCDebito: TLabel
            Left = 798
            Top = 56
            Width = 48
            Height = 13
            Hint = 
              'Digito verificador da Conta corrente onde dever  ser efetuado o ' +
              'd bito do valor total de cada requisi  o'
            Caption = 'Digito CC*'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
          end
          object lbLancamentos: TLabel
            Left = 720
            Top = 298
            Width = 105
            Height = 20
            Caption = 'Lancamentos: '
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object lbLancamentosQtd: TLabel
            Left = 819
            Top = 298
            Width = 31
            Height = 21
            Alignment = taRightJustify
            AutoSize = False
            Caption = '00'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object lbLoteTipo: TLabel
            Left = 30
            Top = 10
            Width = 21
            Height = 13
            Caption = 'Tipo'
            Color = clBtnFace
            ParentColor = False
          end
          object edNumRequisicao: TEdit
            Left = 30
            Top = 71
            Width = 180
            Height = 21
            Hint = 
              'N mero controlado pelo cliente, de 1 a 9999999, para identificar' +
              ' cada acionamento da API e os lan amentos de cada chamada. N o p' +
              'recisa ser sequencial. De uso  nico (por contrato de pagamento)'
            MaxLength = 7
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object edCodigoContrato: TEdit
            Left = 224
            Top = 71
            Width = 180
            Height = 21
            Hint = 
              'Contrato (ou conv nio) de pagamento entre o terceiro e o Banco d' +
              'o Brasil'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object edNumeroAgenciaDebito: TEdit
            Left = 419
            Top = 71
            Width = 180
            Height = 21
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
          end
          object edNumeroContaCorrenteDebito: TEdit
            Left = 614
            Top = 71
            Width = 180
            Height = 21
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
          end
          object edDigitoCCDebito: TEdit
            Left = 798
            Top = 71
            Width = 52
            Height = 21
            MaxLength = 1
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
          end
          object btPreencher: TBitBtn
            Left = 30
            Top = 298
            Width = 180
            Height = 28
            Caption = 'Preencher Dados Ficticios'
            TabOrder = 6
            OnClick = btPreencherClick
          end
          object btLimparLancamentos: TBitBtn
            Left = 419
            Top = 298
            Width = 180
            Height = 28
            Caption = 'Limpar Lancamentos'
            TabOrder = 8
            OnClick = btLimparLancamentosClick
          end
          object btIncluirLancamento: TBitBtn
            Left = 224
            Top = 298
            Width = 180
            Height = 28
            Caption = 'Incluir Lancamento'
            TabOrder = 7
            OnClick = btIncluirLancamentoClick
          end
          object cbLoteTipo: TComboBox
            Left = 30
            Top = 25
            Width = 180
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 0
            Text = 'Boletos'
            OnSelect = cbLoteTipoSelect
            Items.Strings = (
              'Boletos'
              'Guias com C digo de Barras'
              'GRU'
              'DARF (Normal/Preto)'
              'GPS')
          end
          object pnLancamentos: TPanel
            Left = 30
            Top = 103
            Width = 820
            Height = 184
            BevelOuter = bvNone
            TabOrder = 9
            object pgLancamentos: TPageControl
              Left = 0
              Top = 0
              Width = 820
              Height = 184
              ActivePage = tsBoletoLancamento
              Align = alClient
              TabOrder = 0
              object tsBoletoLancamento: TTabSheet
                Caption = 'Boleto'
                object gbBoletosLacamento: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 820
                  Height = 184
                  Caption = 'Lancamento'
                  TabOrder = 0
                  object pnBoletosLancamento: TPanel
                    Left = 2
                    Top = 15
                    Width = 816
                    Height = 167
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbBoletosDocumentoDebito: TLabel
                      Left = 15
                      Top = 8
                      Width = 117
                      Height = 13
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      Caption = 'Num. Documento Debito'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosCodigoBarras: TLabel
                      Left = 176
                      Top = 8
                      Width = 113
                      Height = 13
                      Hint = 
                        'C digo de barras do boleto a ser pago, 44 d gitos. N o   aceito ' +
                        'o n mero da linha digit vel. Informar como string'
                      Caption = 'Num. Codigo de Barras*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosValorPagamento: TLabel
                      Left = 583
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 'Valor do pagamento total do boleto em reais'
                      Caption = 'Valor Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosDataPagamento: TLabel
                      Left = 467
                      Top = 8
                      Width = 84
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver. Exemplo: 9012022'
                      Caption = 'Data Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosSeuDocumento: TLabel
                      Left = 15
                      Top = 56
                      Width = 56
                      Height = 13
                      Hint = 
                        'Seu n mero na solicita  o de pagamento, equivalente ao Seu N mer' +
                        'o do boleto. N o validado pelo BB. Informar como string'
                      Caption = 'SeuNumero'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosDescricaoPagamento: TLabel
                      Left = 176
                      Top = 56
                      Width = 105
                      Height = 13
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      Caption = 'Descricao Pagamento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosNossoNumero: TLabel
                      Left = 470
                      Top = 56
                      Width = 67
                      Height = 13
                      Hint = 
                        'Numero do boleto, equivalente ao Nosso N mero. N o validado pelo' +
                        ' BB. Informar como string'
                      Caption = 'NossoNumero'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosValorNominal: TLabel
                      Left = 700
                      Top = 8
                      Width = 69
                      Height = 13
                      Hint = 'Valor original registrado pelo benefici rio do boleto'
                      Caption = 'Valor Nominal*'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbBoletosValorDesconto: TLabel
                      Left = 583
                      Top = 56
                      Width = 73
                      Height = 13
                      Hint = 
                        'Valor do desconto e/ou abatimento registrados pelo benefici rio ' +
                        'do boleto'
                      Caption = 'Valor Desconto'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosDocumentoPagador: TLabel
                      Left = 100
                      Top = 104
                      Width = 98
                      Height = 13
                      Hint = 'CPF ou CNPJ do pagador do boleto'
                      Caption = 'Documento Pagador'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosDocumentoBeneficiario: TLabel
                      Left = 367
                      Top = 104
                      Width = 117
                      Height = 13
                      Hint = 'CPF ou CNPJ do benefici rio do boleto'
                      Caption = 'Documento Baneficiario*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosDocumentoAvalista: TLabel
                      Left = 634
                      Top = 104
                      Width = 95
                      Height = 13
                      Hint = 'CPF ou CNPJ do avalista do boleto'
                      Caption = 'Documento Avalista'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosTipoPagador: TLabel
                      Left = 15
                      Top = 104
                      Width = 64
                      Height = 13
                      Hint = 'Indica se o documento do pagador   um CPF ou um CNPJ'
                      Caption = 'Tipo Pagador'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosTipoBeneficiario: TLabel
                      Left = 282
                      Top = 104
                      Width = 67
                      Height = 13
                      Hint = 'Indica se o documento do benefici rio   um CPF ou um CNPJ'
                      Caption = 'Tipo Benefic.*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosTipoAvalista: TLabel
                      Left = 549
                      Top = 104
                      Width = 61
                      Height = 13
                      Hint = 'Indica se o documento do avalista   um CPF ou um CNPJ'
                      Caption = 'Tipo Avalista'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbBoletosValorMoraMulta: TLabel
                      Left = 700
                      Top = 56
                      Width = 83
                      Height = 13
                      Hint = 
                        'Valor dos juros de mora e/ou multa registrados pelo benefici rio' +
                        ' do boleto'
                      Caption = 'Valor Juros/Multa'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edBoletosDocumentoDebito: TEdit
                      Left = 15
                      Top = 23
                      Width = 145
                      Height = 21
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 0
                    end
                    object edBoletosCodigoBarras: TEdit
                      Left = 176
                      Top = 23
                      Width = 274
                      Height = 21
                      Hint = 
                        'C digo de barras do boleto a ser pago, 44 d gitos. N o   aceito ' +
                        'o n mero da linha digit vel. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 1
                    end
                    object edBoletosValorPagamento: TEdit
                      Left = 583
                      Top = 23
                      Width = 101
                      Height = 21
                      Hint = 'Valor do pagamento total do boleto em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 3
                    end
                    object dtBoletosDataPagamento: TDateTimePicker
                      Left = 466
                      Top = 23
                      Width = 101
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver. Exemplo: 9012022'
                      Date = 45664.565578831020000000
                      Time = 45664.565578831020000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 2
                    end
                    object edBoletosSeuDocumento: TEdit
                      Left = 15
                      Top = 71
                      Width = 145
                      Height = 21
                      Hint = 
                        'Seu n mero na solicita  o de pagamento, equivalente ao Seu N mer' +
                        'o do boleto. N o validado pelo BB. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 5
                    end
                    object edBoletosDescricaoPagamento: TEdit
                      Left = 176
                      Top = 71
                      Width = 274
                      Height = 21
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 6
                    end
                    object edBoletosNossoNumero: TEdit
                      Left = 467
                      Top = 71
                      Width = 100
                      Height = 21
                      Hint = 
                        'Numero do boleto, equivalente ao Nosso N mero. N o validado pelo' +
                        ' BB. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 7
                    end
                    object edBoletosValorNominal: TEdit
                      Left = 700
                      Top = 23
                      Width = 101
                      Height = 21
                      Hint = 'Valor original registrado pelo benefici rio do boleto'
                      TabOrder = 4
                    end
                    object edBoletosValorDesconto: TEdit
                      Left = 583
                      Top = 71
                      Width = 101
                      Height = 21
                      Hint = 
                        'Valor do desconto e/ou abatimento registrados pelo benefici rio ' +
                        'do boleto'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 8
                    end
                    object cbBoletosTipoPagador: TComboBox
                      Left = 15
                      Top = 119
                      Width = 70
                      Height = 21
                      Hint = 'Indica se o documento do pagador   um CPF ou um CNPJ'
                      Style = csDropDownList
                      ItemHeight = 13
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 10
                      Items.Strings = (
                        'CPF'
                        'CNPJ')
                    end
                    object edBoletosDocumentoPagador: TEdit
                      Left = 100
                      Top = 119
                      Width = 167
                      Height = 21
                      Hint = 'CPF ou CNPJ do pagador do boleto'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 11
                    end
                    object edBoletosDocumentoBeneficiario: TEdit
                      Left = 367
                      Top = 119
                      Width = 167
                      Height = 21
                      Hint = 'CPF ou CNPJ do benefici rio do boleto'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 13
                    end
                    object edBoletosDocumentoAvalista: TEdit
                      Left = 634
                      Top = 119
                      Width = 167
                      Height = 21
                      Hint = 'CPF ou CNPJ do avalista do boleto'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 15
                    end
                    object cbBoletosTipoBeneficiario: TComboBox
                      Left = 282
                      Top = 119
                      Width = 70
                      Height = 21
                      Hint = 'Indica se o documento do benefici rio   um CPF ou um CNPJ'
                      Style = csDropDownList
                      ItemHeight = 13
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 12
                      Items.Strings = (
                        'CPF'
                        'CNPJ')
                    end
                    object cbBoletosTipoAvalista: TComboBox
                      Left = 549
                      Top = 119
                      Width = 70
                      Height = 21
                      Hint = 'Indica se o documento do avalista   um CPF ou um CNPJ'
                      Style = csDropDownList
                      ItemHeight = 13
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 14
                      Items.Strings = (
                        'CPF'
                        'CNPJ')
                    end
                    object edBoletosValorMoraMulta: TEdit
                      Left = 700
                      Top = 71
                      Width = 101
                      Height = 21
                      Hint = 
                        'Valor dos juros de mora e/ou multa registrados pelo benefici rio' +
                        ' do boleto'
                      TabOrder = 9
                    end
                  end
                end
              end
              object tsGuiaCodBarras: TTabSheet
                Caption = 'Guia Cod.Barras'
                object gbGuiaLacamento: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 820
                  Height = 184
                  Caption = 'Lancamento'
                  TabOrder = 0
                  object pnGuiaLancamento: TPanel
                    Left = 2
                    Top = 15
                    Width = 816
                    Height = 167
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbGuiaDocumentoDebito: TLabel
                      Left = 462
                      Top = 56
                      Width = 117
                      Height = 13
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      Caption = 'Num. Documento Debito'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGuiaCodigoBarras: TLabel
                      Left = 15
                      Top = 8
                      Width = 113
                      Height = 13
                      Hint = 
                        'C digo de barras ou linha digit vel da guia a ser paga, 44 d git' +
                        'os (excluir os d gitos verificadores)'
                      Caption = 'Num. Codigo de Barras*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGuiaValorPagamento: TLabel
                      Left = 638
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 'Valor do pagamento em reais'
                      Caption = 'Valor Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGuiaDataPagamento: TLabel
                      Left = 463
                      Top = 8
                      Width = 84
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGuiaSeuDocumento: TLabel
                      Left = 638
                      Top = 56
                      Width = 56
                      Height = 13
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      Caption = 'SeuNumero'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGuiaDescricaoPagamento: TLabel
                      Left = 15
                      Top = 56
                      Width = 105
                      Height = 13
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      Caption = 'Descricao Pagamento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object edGuiaDocumentoDebito: TEdit
                      Left = 462
                      Top = 71
                      Width = 160
                      Height = 21
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 4
                    end
                    object edGuiaCodigoBarras: TEdit
                      Left = 15
                      Top = 23
                      Width = 431
                      Height = 21
                      Hint = 
                        'C digo de barras ou linha digit vel da guia a ser paga, 44 d git' +
                        'os (excluir os d gitos verificadores)'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 0
                    end
                    object edGuiaValorPagamento: TEdit
                      Left = 638
                      Top = 23
                      Width = 160
                      Height = 21
                      Hint = 'Valor do pagamento em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 2
                    end
                    object dtGuiaDataPagamento: TDateTimePicker
                      Left = 462
                      Top = 23
                      Width = 160
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45664.565578831020000000
                      Time = 45664.565578831020000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 1
                    end
                    object edGuiaSeuDocumento: TEdit
                      Left = 638
                      Top = 71
                      Width = 160
                      Height = 21
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 5
                    end
                    object edGuiaDescricaoPagamento: TEdit
                      Left = 15
                      Top = 71
                      Width = 431
                      Height = 21
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 3
                    end
                  end
                end
              end
              object tsGRU: TTabSheet
                Caption = 'GRU'
                object gbGRURequisicao: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 812
                  Height = 156
                  Align = alClient
                  Caption = 'Requisicao'
                  TabOrder = 0
                  object pnGRULancamento: TPanel
                    Left = 2
                    Top = 15
                    Width = 808
                    Height = 139
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbGRUCodigoBarras: TLabel
                      Left = 15
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 
                        'C digo de barras ou linha digit vel da guia a ser paga, 44 d git' +
                        'os (excluir os d gitos verificadores)'
                      Caption = 'Codigo de Barras*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorPagamento: TLabel
                      Left = 688
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 'Valor do pagamento em reais'
                      Caption = 'Valor Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUDataPagamento: TLabel
                      Left = 433
                      Top = 8
                      Width = 84
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorAcrescimo: TLabel
                      Left = 688
                      Top = 56
                      Width = 105
                      Height = 13
                      Hint = 'Valor de outros acr scimos'
                      Caption = 'Valor Outro Acrescimo'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorPrincipal: TLabel
                      Left = 15
                      Top = 56
                      Width = 71
                      Height = 13
                      Hint = 'Valor do principal'
                      Caption = 'Valor Principal*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorDesconto: TLabel
                      Left = 160
                      Top = 56
                      Width = 73
                      Height = 13
                      Hint = 'Valor do desconto ou abatimento'
                      Caption = 'Valor Desconto'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorOutraDeducao: TLabel
                      Left = 305
                      Top = 56
                      Width = 100
                      Height = 13
                      Hint = 'Valor de outras dedu  es'
                      Caption = 'Valor Outra Deducao'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorMulta: TLabel
                      Left = 432
                      Top = 56
                      Width = 53
                      Height = 13
                      Hint = 'Valor da mora/multa'
                      Caption = 'Valor Multa'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUValorJuros: TLabel
                      Left = 560
                      Top = 56
                      Width = 97
                      Height = 13
                      Hint = 'Valor de juros/encargos'
                      Caption = 'Valor Juros/Encargo'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUDescricaoPagamento: TLabel
                      Left = 15
                      Top = 104
                      Width = 99
                      Height = 13
                      Hint = 
                        'String. Campo de uso livre pelo cliente conveniado - sem tratame' +
                        'nto pelo Banco'
                      Caption = 'Descri  o Pagamento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUDataVencimento: TLabel
                      Left = 561
                      Top = 8
                      Width = 82
                      Height = 13
                      Hint = 
                        'Data de vencimento da GRU, em formato ddmmaaaa. Omitir o zero   ' +
                        'esquerda do dia, se houver'
                      Caption = 'Data Vencimento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUDocumentoDebito: TLabel
                      Left = 258
                      Top = 104
                      Width = 117
                      Height = 13
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      Caption = 'Num. Documento Debito'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUNumeroReferencia: TLabel
                      Left = 433
                      Top = 104
                      Width = 92
                      Height = 13
                      Hint = 'N mero de refer ncia da GRU. Informar como string'
                      Caption = 'Numero Referencia'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUMesAnoCompetencia: TLabel
                      Left = 560
                      Top = 104
                      Width = 104
                      Height = 13
                      Hint = 
                        'M s e ano da compet ncia da GRU, formato mmaaaa. Omitir o zero  ' +
                        ' esquerda do m s, se houver'
                      Caption = 'MesAno Competencia'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGRUidContribuinte: TLabel
                      Left = 688
                      Top = 104
                      Width = 71
                      Height = 13
                      Hint = 'CPF ou CNPJ do contribuinte respons vel pela GRU'
                      Caption = 'id Contribuinte*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object edGRUCodigoBarras: TEdit
                      Left = 15
                      Top = 23
                      Width = 403
                      Height = 21
                      Hint = 
                        'C digo de barras ou linha digit vel da guia a ser paga, 44 d git' +
                        'os (excluir os d gitos verificadores)'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 0
                    end
                    object edGRUValorPagamento: TEdit
                      Left = 688
                      Top = 23
                      Width = 113
                      Height = 21
                      Hint = 'Valor do pagamento em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 3
                    end
                    object dtGRUDataPagamento: TDateTimePicker
                      Left = 432
                      Top = 23
                      Width = 113
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45664.565578831020000000
                      Time = 45664.565578831020000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 1
                    end
                    object edGRUValorAcrescimo: TEdit
                      Left = 688
                      Top = 71
                      Width = 113
                      Height = 21
                      Hint = 'Valor de outros acr scimos'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 9
                    end
                    object edGRUValorPrincipal: TEdit
                      Left = 15
                      Top = 71
                      Width = 130
                      Height = 21
                      Hint = 'Valor do principal'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 4
                    end
                    object edGRUValorDesconto: TEdit
                      Left = 160
                      Top = 71
                      Width = 130
                      Height = 21
                      Hint = 'Valor do desconto ou abatimento'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 5
                    end
                    object edGRUValorOutraDeducao: TEdit
                      Left = 305
                      Top = 71
                      Width = 113
                      Height = 21
                      Hint = 'Valor de outras dedu  es'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 6
                    end
                    object edGRUValorMulta: TEdit
                      Left = 432
                      Top = 71
                      Width = 113
                      Height = 21
                      Hint = 'Valor da mora/multa'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 7
                    end
                    object edGRUValorJuros: TEdit
                      Left = 560
                      Top = 71
                      Width = 113
                      Height = 21
                      Hint = 'Valor de juros/encargos'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 8
                    end
                    object edGRUDescricaoPagamento: TEdit
                      Left = 15
                      Top = 119
                      Width = 227
                      Height = 21
                      Hint = 
                        'String. Campo de uso livre pelo cliente conveniado - sem tratame' +
                        'nto pelo Banco'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 10
                    end
                    object dtGRUDataVencimento: TDateTimePicker
                      Left = 560
                      Top = 23
                      Width = 113
                      Height = 23
                      Hint = 
                        'Data de vencimento da GRU, em formato ddmmaaaa. Omitir o zero   ' +
                        'esquerda do dia, se houver'
                      Date = 45677.000000000000000000
                      Time = 45677.000000000000000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 2
                    end
                    object edGRUDocumentoDebito: TEdit
                      Left = 258
                      Top = 119
                      Width = 160
                      Height = 21
                      Hint = 
                        'N mero que ser  mostrado no extrato da conta do pagador. Por ser' +
                        ' um campo opcional, se n o inserido, a conta de d bito ter  um  ' +
                        'nico lan amento no valor total de todos os lan amentos de cr dit' +
                        'os validados. De igual modo ser  se o mesmo n mero for informado' +
                        ' para todos os lan amentos de cr dito. Quando n meros diferentes' +
                        ' forem informados para cada lan amento, os lan amentos de d bito' +
                        's ser o individualizados'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 11
                    end
                    object edGRUNumeroReferencia: TEdit
                      Left = 433
                      Top = 119
                      Width = 112
                      Height = 21
                      Hint = 'N mero de refer ncia da GRU. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 12
                    end
                    object edGRUMesAnoCompetencia: TEdit
                      Left = 560
                      Top = 119
                      Width = 113
                      Height = 21
                      Hint = 
                        'M s e ano da compet ncia da GRU, formato mmaaaa. Omitir o zero  ' +
                        ' esquerda do m s, se houver'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 13
                    end
                    object edGRUidContribuinte: TEdit
                      Left = 688
                      Top = 119
                      Width = 113
                      Height = 21
                      Hint = 'CPF ou CNPJ do contribuinte respons vel pela GRU'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 14
                    end
                  end
                end
              end
              object tsGPS: TTabSheet
                Caption = 'GPS'
                object gbGPSLancamento: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 812
                  Height = 156
                  Align = alClient
                  Caption = 'Lancamento'
                  TabOrder = 0
                  object pnGPSLancamento: TPanel
                    Left = 2
                    Top = 15
                    Width = 808
                    Height = 139
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbGPSValorPagamento: TLabel
                      Left = 177
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 'Valor do pagamento em reais'
                      Caption = 'Valor Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSDataPagamento: TLabel
                      Left = 15
                      Top = 8
                      Width = 84
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSIDContribuinte: TLabel
                      Left = 614
                      Top = 56
                      Width = 143
                      Height = 13
                      Hint = 'N mero de identifica  o do contribuinte'
                      Caption = 'Identifica  o Contribuinte GPS*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSCodReceita: TLabel
                      Left = 15
                      Top = 56
                      Width = 138
                      Height = 13
                      Hint = 'C digo da Receita do Tributo Guia Da Previd ncia Social - GPS'
                      Caption = 'Codigo Receita Tributo GPS*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSMesAnoCompetencia: TLabel
                      Left = 215
                      Top = 56
                      Width = 129
                      Height = 13
                      Hint = 
                        'M s e ano da compet ncia da GPS, formato mmaaaa. Omitir o zero  ' +
                        ' esquerda do m s, se houver'
                      Caption = 'MesAno Competencia GPS'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSTipoContribuinte: TLabel
                      Left = 414
                      Top = 56
                      Width = 127
                      Height = 13
                      Hint = 'C digo do tipo de contribuinte na Guia da Previd ncia Social'
                      Caption = 'Cod.Tipo Contribuinte GPS'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSDescricaoPagamento: TLabel
                      Left = 655
                      Top = 8
                      Width = 105
                      Height = 13
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      Caption = 'Descricao Pagamento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSDocumentoDebito: TLabel
                      Left = 337
                      Top = 8
                      Width = 114
                      Height = 13
                      Hint = 
                        #9'N mero que ser  mostrado no extrato da conta do pagador. Por se' +
                        'r um campo opcional, se n o inserido, a conta de d bito ter  um ' +
                        ' nico lan amento no valor total de todos os lan amentos de cr di' +
                        'tos validados. De igual modo ser  se o mesmo n mero for informad' +
                        'o para todos os lan amentos de cr dito. Quando n meros diferente' +
                        's forem informados para cada lan amento, os lan amentos de d bit' +
                        'os ser o individualizados'
                      Caption = 'Num. Documento D bito'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSIdTributo: TLabel
                      Left = 16
                      Top = 104
                      Width = 159
                      Height = 13
                      Hint = 'N mero de identifica  o na Previd ncia. Informar como string'
                      Caption = 'Codigo Identificador Tributo GPS*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSValorDevido: TLabel
                      Left = 215
                      Top = 104
                      Width = 65
                      Height = 13
                      Hint = 'Valor devido ao INSS pelo contribuinte'
                      Caption = 'Valor Devido*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSOutrasEntradas: TLabel
                      Left = 414
                      Top = 104
                      Width = 107
                      Height = 13
                      Hint = 'Valor outras entradas'
                      Caption = 'Valor Outras Entradas*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSSeuDocumento: TLabel
                      Left = 496
                      Top = 8
                      Width = 77
                      Height = 13
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      Caption = 'Seu Documento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbGPSValorAtualizacao: TLabel
                      Left = 614
                      Top = 104
                      Width = 126
                      Height = 13
                      Hint = 'Valor da atualiza  o monet ria'
                      Caption = 'Valor Atualiza  o Monetaria'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object edGPSValorPagamento: TEdit
                      Left = 177
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 'Valor do pagamento em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 1
                    end
                    object dtGPSDataPagamento: TDateTimePicker
                      Left = 15
                      Top = 23
                      Width = 146
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45664.565578831020000000
                      Time = 45664.565578831020000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 0
                    end
                    object edGPSIDContribuinte: TEdit
                      Left = 614
                      Top = 71
                      Width = 187
                      Height = 21
                      Hint = 'N mero de identifica  o do contribuinte'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 8
                    end
                    object edGPSCodReceita: TEdit
                      Left = 15
                      Top = 71
                      Width = 187
                      Height = 21
                      Hint = 'C digo da Receita do Tributo Guia Da Previd ncia Social - GPS'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 5
                    end
                    object edGPSMesAnoCompetencia: TEdit
                      Left = 215
                      Top = 71
                      Width = 187
                      Height = 21
                      Hint = 
                        'M s e ano da compet ncia da GPS, formato mmaaaa. Omitir o zero  ' +
                        ' esquerda do m s, se houver'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 6
                    end
                    object edGPSDescricaoPagamento: TEdit
                      Left = 655
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 4
                    end
                    object edGPSDocumentoDebito: TEdit
                      Left = 337
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        #9'N mero que ser  mostrado no extrato da conta do pagador. Por se' +
                        'r um campo opcional, se n o inserido, a conta de d bito ter  um ' +
                        ' nico lan amento no valor total de todos os lan amentos de cr di' +
                        'tos validados. De igual modo ser  se o mesmo n mero for informad' +
                        'o para todos os lan amentos de cr dito. Quando n meros diferente' +
                        's forem informados para cada lan amento, os lan amentos de d bit' +
                        'os ser o individualizados'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 2
                    end
                    object edGPSIdTributo: TEdit
                      Left = 15
                      Top = 119
                      Width = 187
                      Height = 21
                      Hint = 'N mero de identifica  o na Previd ncia. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 9
                    end
                    object edGPSValorDevido: TEdit
                      Left = 215
                      Top = 119
                      Width = 187
                      Height = 21
                      Hint = 'Valor devido ao INSS pelo contribuinte'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 10
                    end
                    object edGPSOutrasEntradas: TEdit
                      Left = 414
                      Top = 119
                      Width = 187
                      Height = 21
                      Hint = 'Valor outras entradas'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 11
                    end
                    object edGPSSeuDocumento: TEdit
                      Left = 496
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 3
                    end
                    object cbGPSTipoContribuinte: TComboBox
                      Left = 414
                      Top = 71
                      Width = 187
                      Height = 21
                      Hint = 'C digo do tipo de contribuinte na Guia da Previd ncia Social'
                      Style = csDropDownList
                      ItemHeight = 13
                      TabOrder = 7
                      Items.Strings = (
                        'Nenhum'
                        'CNPJ'
                        'CPF'
                        'NIT / PIS / PASEP'
                        'CEI'
                        'NB'
                        'Num T tulo'
                        'DEBCAD'
                        'Refer ncia')
                    end
                    object edGPSValorAtualizacao: TEdit
                      Left = 614
                      Top = 119
                      Width = 187
                      Height = 21
                      Hint = 'Valor da atualiza  o monet ria'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 12
                    end
                  end
                end
              end
              object tsDARF: TTabSheet
                Caption = 'DARF'
                object gbDARFLancamento: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 812
                  Height = 156
                  Align = alClient
                  Caption = 'Lancamento'
                  TabOrder = 0
                  object pnDARFLancamento: TPanel
                    Left = 2
                    Top = 15
                    Width = 808
                    Height = 139
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbDARFValorPagamento: TLabel
                      Left = 177
                      Top = 8
                      Width = 85
                      Height = 13
                      Hint = 'Valor do pagamento em reais'
                      Caption = 'Valor Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFDataPagamento: TLabel
                      Left = 15
                      Top = 8
                      Width = 84
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Pagamento*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFIDContribuinte: TLabel
                      Left = 655
                      Top = 56
                      Width = 118
                      Height = 13
                      Hint = 'N mero de identifica  o do contribuinte'
                      Caption = 'Identifica  o Contribuinte*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFCodReceita: TLabel
                      Left = 337
                      Top = 56
                      Width = 113
                      Height = 13
                      Hint = 'C digo da Receita do Tributo Guia Da Previd ncia Social - GPS'
                      Caption = 'Codigo Receita Tributo*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFTipoContribuinte: TLabel
                      Left = 496
                      Top = 56
                      Width = 106
                      Height = 13
                      Hint = 'C digo do tipo de contribuinte na Guia da Previd ncia Social'
                      Caption = 'Cod.Tipo Contribuinte*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFDescricaoPagamento: TLabel
                      Left = 655
                      Top = 8
                      Width = 105
                      Height = 13
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      Caption = 'Descricao Pagamento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFDocumentoDebito: TLabel
                      Left = 337
                      Top = 8
                      Width = 117
                      Height = 13
                      Hint = 
                        #9'N mero que ser  mostrado no extrato da conta do pagador. Por se' +
                        'r um campo opcional, se n o inserido, a conta de d bito ter  um ' +
                        ' nico lan amento no valor total de todos os lan amentos de cr di' +
                        'tos validados. De igual modo ser  se o mesmo n mero for informad' +
                        'o para todos os lan amentos de cr dito. Quando n meros diferente' +
                        's forem informados para cada lan amento, os lan amentos de d bit' +
                        'os ser o individualizados'
                      Caption = 'Num. Documento Debito'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFIdTributo: TLabel
                      Left = 16
                      Top = 104
                      Width = 134
                      Height = 13
                      Hint = 'N mero de identifica  o na Previd ncia. Informar como string'
                      Caption = 'Codigo Identificador Tributo*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFValorPrincipal: TLabel
                      Left = 177
                      Top = 104
                      Width = 71
                      Height = 13
                      Hint = 'Valor devido ao INSS pelo contribuinte'
                      Caption = 'Valor Principal*'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFValorMulta: TLabel
                      Left = 337
                      Top = 104
                      Width = 53
                      Height = 13
                      Hint = 'Valor outras entradas'
                      Caption = 'Valor Multa'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFSeuDocumento: TLabel
                      Left = 496
                      Top = 8
                      Width = 77
                      Height = 13
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      Caption = 'Seu Documento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFValorJuros: TLabel
                      Left = 496
                      Top = 104
                      Width = 97
                      Height = 13
                      Hint = 'Valor da atualiza  o monet ria'
                      Caption = 'Valor Juros/Encargo'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFDataApuracao: TLabel
                      Left = 16
                      Top = 56
                      Width = 72
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Apuracao'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFDataVencimento: TLabel
                      Left = 177
                      Top = 56
                      Width = 82
                      Height = 13
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Caption = 'Data Vencimento'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object lbDARFNumReferencia: TLabel
                      Left = 655
                      Top = 104
                      Width = 92
                      Height = 13
                      Hint = 'Valor do pagamento em reais'
                      Caption = 'Numero Referencia'
                      Color = clBtnFace
                      ParentColor = False
                      ParentShowHint = False
                      ShowHint = True
                    end
                    object edDARFValorPagamento: TEdit
                      Left = 177
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 'Valor do pagamento em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 1
                    end
                    object dtDARFDataPagamento: TDateTimePicker
                      Left = 15
                      Top = 23
                      Width = 146
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45664.565578831020000000
                      Time = 45664.565578831020000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 0
                    end
                    object edDARFIDContribuinte: TEdit
                      Left = 655
                      Top = 71
                      Width = 146
                      Height = 21
                      Hint = 'N mero de identifica  o do contribuinte'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 7
                    end
                    object edDARFCodReceita: TEdit
                      Left = 337
                      Top = 71
                      Width = 146
                      Height = 21
                      Hint = 'C digo da Receita do Tributo Guia Da Previd ncia Social - GPS'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 5
                    end
                    object edDARFDescricaoPagamento: TEdit
                      Left = 655
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        'Campo de uso livre pelo cliente conveniado - sem tratamento pelo' +
                        ' Banco'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 4
                    end
                    object edDARFDocumentoDebito: TEdit
                      Left = 337
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        #9'N mero que ser  mostrado no extrato da conta do pagador. Por se' +
                        'r um campo opcional, se n o inserido, a conta de d bito ter  um ' +
                        ' nico lan amento no valor total de todos os lan amentos de cr di' +
                        'tos validados. De igual modo ser  se o mesmo n mero for informad' +
                        'o para todos os lan amentos de cr dito. Quando n meros diferente' +
                        's forem informados para cada lan amento, os lan amentos de d bit' +
                        'os ser o individualizados'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 2
                    end
                    object edDARFIdTributo: TEdit
                      Left = 15
                      Top = 119
                      Width = 147
                      Height = 21
                      Hint = 'N mero de identifica  o na Previd ncia. Informar como string'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 8
                    end
                    object edDARFValorPrincipal: TEdit
                      Left = 177
                      Top = 119
                      Width = 146
                      Height = 21
                      Hint = 'Valor devido ao INSS pelo contribuinte'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 9
                    end
                    object edDARFValorMulta: TEdit
                      Left = 337
                      Top = 119
                      Width = 146
                      Height = 21
                      Hint = 'Valor outras entradas'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 10
                    end
                    object edDARFSeuDocumento: TEdit
                      Left = 496
                      Top = 23
                      Width = 146
                      Height = 21
                      Hint = 
                        'N mero de uso livre pelo cliente conveniado, n o validado pelo B' +
                        'B. Informar como string (at  20 caracteres)'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 3
                    end
                    object cbDARFTipoContribuinte: TComboBox
                      Left = 496
                      Top = 71
                      Width = 146
                      Height = 21
                      Hint = 'C digo do tipo de contribuinte na Guia da Previd ncia Social'
                      Style = csDropDownList
                      ItemHeight = 13
                      TabOrder = 6
                      Items.Strings = (
                        'Nenhum'
                        'CNPJ'
                        'CPF'
                        'NIT / PIS / PASEP'
                        'CEI'
                        'NB'
                        'Num T tulo'
                        'DEBCAD'
                        'Refer ncia')
                    end
                    object edDARFValorJuros: TEdit
                      Left = 496
                      Top = 119
                      Width = 146
                      Height = 21
                      Hint = 'Valor da atualiza  o monet ria'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 11
                    end
                    object dtDARFDataApuracao: TDateTimePicker
                      Left = 16
                      Top = 71
                      Width = 146
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45677.000000000000000000
                      Time = 45677.000000000000000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 12
                    end
                    object dtDARFDataVencimento: TDateTimePicker
                      Left = 177
                      Top = 71
                      Width = 146
                      Height = 23
                      Hint = 
                        'Data do pagamento, em formato ddmmaaaa. Omitir o zero   esquerda' +
                        ' do dia, se houver'
                      Date = 45677.000000000000000000
                      Time = 45677.000000000000000000
                      MaxDate = 2958465.000000000000000000
                      MinDate = -53780.000000000000000000
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 13
                    end
                    object edDARFNumReferencia: TEdit
                      Left = 655
                      Top = 119
                      Width = 146
                      Height = 21
                      Hint = 'Valor do pagamento em reais'
                      ParentShowHint = False
                      ShowHint = True
                      TabOrder = 14
                    end
                  end
                end
              end
            end
          end
        end
        object pnLoteRodape: TPanel
          Left = 0
          Top = 351
          Width = 892
          Height = 51
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btLoteEnviar: TBitBtn
            Left = 19
            Top = 9
            Width = 139
            Height = 28
            Caption = 'Enviar Lote'
            TabOrder = 0
            OnClick = btLoteEnviarClick
          end
        end
      end
    end
    object tsGuias: TTabSheet
      Caption = 'Consultas'
      ImageIndex = 8
      object PageControl1: TPageControl
        Left = 0
        Top = 0
        Width = 892
        Height = 402
        ActivePage = tsLoteConsultar
        Align = alClient
        TabHeight = 25
        TabOrder = 0
        TabWidth = 200
        object tsLoteConsultar: TTabSheet
          Caption = 'Consultar Lote'
          object pnLoteConsultar: TPanel
            Left = 0
            Top = 0
            Width = 884
            Height = 367
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              884
              367)
            object lbConsultaLoteTipo: TLabel
              Left = 30
              Top = 10
              Width = 21
              Height = 13
              Caption = 'Tipo'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConsultaIdLote: TLabel
              Left = 224
              Top = 12
              Width = 33
              Height = 13
              Caption = 'Id Lote'
              Color = clBtnFace
              ParentColor = False
              ParentShowHint = False
              ShowHint = True
            end
            object cbConsultaLoteTipo: TComboBox
              Left = 30
              Top = 25
              Width = 180
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = 'Boletos'
              OnSelect = cbLoteTipoSelect
              Items.Strings = (
                'Boletos'
                'Guias com C digo de Barras'
                'GRU'
                'DARF (Normal/Preto)'
                'GPS')
            end
            object edConsultarIdLote: TEdit
              Left = 224
              Top = 25
              Width = 180
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
            end
            object btConsultarLote: TBitBtn
              Left = 416
              Top = 23
              Width = 120
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Consultar'
              TabOrder = 2
              OnClick = btConsultarLoteClick
            end
            object mmConsultaLog: TMemo
              Left = 30
              Top = 72
              Width = 826
              Height = 280
              TabOrder = 3
            end
          end
        end
        object tsPagamentoConsultar: TTabSheet
          Caption = 'Pagamento Especifico'
          object pnPagamentoConsultar: TPanel
            Left = 0
            Top = 0
            Width = 884
            Height = 367
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              884
              367)
            object lbConsultarIdPagamentoTipo: TLabel
              Left = 30
              Top = 10
              Width = 21
              Height = 13
              Caption = 'Tipo'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConsultarIdPagamento: TLabel
              Left = 224
              Top = 12
              Width = 66
              Height = 13
              Caption = 'Id Pagamento'
              Color = clBtnFace
              ParentColor = False
              ParentShowHint = False
              ShowHint = True
            end
            object cbConsultarIdPagamentoTipo: TComboBox
              Left = 30
              Top = 25
              Width = 180
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = 'Boletos'
              OnSelect = cbLoteTipoSelect
              Items.Strings = (
                'Boletos'
                'Guias com C digo de Barras'
                'GRU'
                'DARF (Normal/Preto)'
                'GPS')
            end
            object edConsultarIdPagamento: TEdit
              Left = 224
              Top = 25
              Width = 180
              Height = 21
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
            end
            object btConsultarIdPagamento: TBitBtn
              Left = 416
              Top = 23
              Width = 120
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Consultar'
              TabOrder = 2
              OnClick = btConsultarIdPagamentoClick
            end
            object mmConsultarPagamentoLog: TMemo
              Left = 30
              Top = 72
              Width = 826
              Height = 280
              TabOrder = 3
            end
          end
        end
      end
    end
  end
end
