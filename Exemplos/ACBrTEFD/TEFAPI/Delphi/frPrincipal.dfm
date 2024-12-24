object FormPrincipal: TFormPrincipal
  Left = 440
  Top = 146
  Caption = 'ACBrTEFAPI - Demo'
  ClientHeight = 600
  ClientWidth = 1110
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 1110
  ParentFont = True
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 775
    Top = 0
    Width = 5
    Height = 600
    Align = alRight
  end
  object pPrincipal: TPanel
    Left = 0
    Top = 0
    Width = 775
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 0
      Top = 406
      Width = 775
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object pgPrincipal: TPageControl
      Left = 0
      Top = 0
      Width = 775
      Height = 406
      ActivePage = tsConfiguracao
      Align = alTop
      Constraints.MinHeight = 375
      Images = ImageList1
      TabOrder = 0
      object tsConfiguracao: TTabSheet
        Caption = 'Configura'#231#227'o'
        object PageControl1: TPageControl
          Left = 0
          Top = 0
          Width = 767
          Height = 376
          ActivePage = tsConfigTEF
          Align = alClient
          TabOrder = 0
          object tsConfigTEF: TTabSheet
            Caption = 'Configura'#231#227'o TEF'
            object pConfiguracao: TPanel
              Left = 0
              Top = 0
              Width = 759
              Height = 346
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object gbConfigTEF: TGroupBox
                Left = 0
                Top = 0
                Width = 316
                Height = 346
                Align = alLeft
                Caption = 'TEF'
                TabOrder = 0
                DesignSize = (
                  316
                  346)
                object Label11: TLabel
                  Left = 9
                  Top = 74
                  Width = 45
                  Height = 15
                  Alignment = taRightJustify
                  Caption = 'Arq.Log:'
                  Color = clBtnFace
                  ParentColor = False
                end
                object SbArqLog: TSpeedButton
                  Left = 126
                  Top = 91
                  Width = 24
                  Height = 22
                  Caption = '...'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  OnClick = SbArqLogClick
                end
                object Label9: TLabel
                  Left = 168
                  Top = 28
                  Width = 44
                  Height = 15
                  Caption = 'QRCode'
                  Color = clBtnFace
                  ParentColor = False
                end
                object Label1: TLabel
                  Left = 9
                  Top = 28
                  Width = 85
                  Height = 15
                  Caption = 'Gerenciador TEF'
                  Color = clBtnFace
                  ParentColor = False
                end
                object Label10: TLabel
                  Left = 168
                  Top = 74
                  Width = 105
                  Height = 15
                  Caption = 'Imprimir Via Cliente'
                  Color = clBtnFace
                  ParentColor = False
                end
                object Label12: TLabel
                  Left = 9
                  Top = 120
                  Width = 105
                  Height = 15
                  Caption = 'Transa'#231#227'o Pendente'
                  Color = clBtnFace
                  ParentColor = False
                end
                object Label18: TLabel
                  Left = 168
                  Top = 120
                  Width = 138
                  Height = 15
                  Caption = 'Pendencia na Inicializa'#231#227'o'
                  Color = clBtnFace
                  ParentColor = False
                end
                object cbSuportaDesconto: TCheckBox
                  Left = 195
                  Top = 172
                  Width = 114
                  Height = 19
                  Caption = 'Suporta Desconto'
                  TabOrder = 9
                end
                object cbSuportaSaque: TCheckBox
                  Left = 195
                  Top = 197
                  Width = 96
                  Height = 19
                  Caption = 'Suporta Saque'
                  TabOrder = 10
                end
                object cbImprimirViaReduzida: TCheckBox
                  Left = 9
                  Top = 195
                  Width = 136
                  Height = 19
                  Caption = 'Imprimir Via Reduzida'
                  TabOrder = 7
                end
                object edLog: TEdit
                  Left = 9
                  Top = 90
                  Width = 117
                  Height = 23
                  Cursor = crIBeam
                  TabOrder = 2
                end
                object btTestarTEF: TBitBtn
                  Left = 97
                  Top = 272
                  Width = 117
                  Height = 36
                  Anchors = [akLeft, akBottom]
                  Caption = 'Testar TEF'
                  TabOrder = 11
                  OnClick = btTestarTEFClick
                end
                object cbxQRCode: TComboBox
                  Left = 168
                  Top = 44
                  Width = 135
                  Height = 23
                  Style = csDropDownList
                  ItemIndex = 1
                  TabOrder = 1
                  Text = 'Auto'
                  Items.Strings = (
                    'N'#227'o Suportado'
                    'Auto'
                    'Exibir no PinPad'
                    'Exibir na Tela'
                    'Imprimir')
                end
                object cbConfirmarAutomaticamente: TCheckBox
                  Left = 9
                  Top = 220
                  Width = 229
                  Height = 19
                  Caption = 'Confirmar Transa'#231#227'o Automaticamente'
                  TabOrder = 8
                end
                object cbxGP: TComboBox
                  Left = 9
                  Top = 44
                  Width = 141
                  Height = 23
                  Style = csDropDownList
                  TabOrder = 0
                end
                object cbAutoAtendimento: TCheckBox
                  Left = 10
                  Top = 170
                  Width = 182
                  Height = 19
                  Caption = 'Terminal de Auto atendimento'
                  TabOrder = 6
                end
                object btSalvarParametros: TBitBtn
                  Left = 14
                  Top = 314
                  Width = 136
                  Height = 28
                  Anchors = [akLeft, akBottom]
                  Caption = 'Salvar Par'#226'metros'
                  TabOrder = 12
                  OnClick = btSalvarParametrosClick
                end
                object btLerParametros: TBitBtn
                  Left = 167
                  Top = 314
                  Width = 136
                  Height = 28
                  Anchors = [akLeft, akBottom]
                  Caption = 'Ler Par'#226'metros'
                  TabOrder = 13
                  OnClick = btLerParametrosClick
                end
                object cbxImpressaoViaCliente: TComboBox
                  Left = 168
                  Top = 90
                  Width = 135
                  Height = 23
                  Style = csDropDownList
                  ItemIndex = 0
                  TabOrder = 3
                  Text = 'Imprimir'
                  Items.Strings = (
                    'Imprimir'
                    'Perguntar'
                    'N'#227'o Imprimir')
                end
                object cbxTransacaoPendente: TComboBox
                  Left = 9
                  Top = 136
                  Width = 141
                  Height = 23
                  Style = csDropDownList
                  ItemIndex = 0
                  TabOrder = 4
                  Text = 'Confirmar'
                  Items.Strings = (
                    'Confirmar'
                    'Estornar'
                    'Perguntar')
                end
                object cbxTransacaoPendenteInicializacao: TComboBox
                  Left = 168
                  Top = 136
                  Width = 135
                  Height = 23
                  Style = csDropDownList
                  ItemIndex = 1
                  TabOrder = 5
                  Text = 'Processar Pendentes'
                  Items.Strings = (
                    'N'#227'o Fazer nada'
                    'Processar Pendentes'
                    'Cancelar/Estornar')
                end
              end
              object pConfigImpSwHouseEstab: TPanel
                Left = 316
                Top = 0
                Width = 443
                Height = 346
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object GroupBox1: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 443
                  Height = 113
                  Align = alTop
                  Caption = 'Software House e Aplica'#231#227'o'
                  TabOrder = 0
                  DesignSize = (
                    443
                    113)
                  object Label14: TLabel
                    Left = 24
                    Top = 15
                    Width = 33
                    Height = 15
                    Caption = 'Nome'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label16: TLabel
                    Left = 24
                    Top = 65
                    Width = 88
                    Height = 15
                    Caption = 'Nome Aplica'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label19: TLabel
                    Left = 294
                    Top = 65
                    Width = 34
                    Height = 15
                    Anchors = [akTop, akRight]
                    Caption = 'Vers'#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label15: TLabel
                    Left = 294
                    Top = 17
                    Width = 27
                    Height = 15
                    Anchors = [akTop, akRight]
                    Caption = 'CNPJ'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRazaoSocialSwHouse: TEdit
                    Left = 24
                    Top = 33
                    Width = 246
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 0
                    Text = 'PROJETO ACBR'
                  end
                  object edNomeAplicacao: TEdit
                    Left = 24
                    Top = 81
                    Width = 246
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 2
                    Text = 'TEFAPIDemo'
                  end
                  object edVersaoAplicacao: TEdit
                    Left = 294
                    Top = 81
                    Width = 141
                    Height = 23
                    Anchors = [akTop, akRight]
                    TabOrder = 3
                    Text = '1.0'
                  end
                  object edCNPJSwHouse: TEdit
                    Left = 294
                    Top = 33
                    Width = 141
                    Height = 23
                    Anchors = [akTop, akRight]
                    TabOrder = 1
                  end
                end
                object GroupBox2: TGroupBox
                  Left = 0
                  Top = 113
                  Width = 443
                  Height = 65
                  Align = alTop
                  Caption = 'Estabelecimento Comercial'
                  TabOrder = 1
                  DesignSize = (
                    443
                    65)
                  object Label17: TLabel
                    Left = 24
                    Top = 15
                    Width = 65
                    Height = 15
                    Caption = 'Raz'#227'o Social'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label21: TLabel
                    Left = 294
                    Top = 15
                    Width = 27
                    Height = 15
                    Anchors = [akTop, akRight]
                    Caption = 'CNPJ'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRazaoSocialEstabelecimento: TEdit
                    Left = 24
                    Top = 31
                    Width = 246
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 0
                    Text = 'PROJETO ACBR'
                  end
                  object edCNPJEstabelecimento: TEdit
                    Left = 294
                    Top = 30
                    Width = 141
                    Height = 23
                    Anchors = [akTop, akRight]
                    TabOrder = 1
                  end
                end
                object gbDadosTerminal: TGroupBox
                  Left = 0
                  Top = 178
                  Width = 443
                  Height = 139
                  Align = alTop
                  Caption = 'Dados Terminal'
                  TabOrder = 2
                  DesignSize = (
                    443
                    139)
                  object Label8: TLabel
                    Left = 24
                    Top = 18
                    Width = 70
                    Height = 15
                    Caption = 'Cod.Terminal'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label20: TLabel
                    Left = 294
                    Top = 58
                    Width = 68
                    Height = 15
                    Caption = 'Porta PinPad'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label23: TLabel
                    Left = 24
                    Top = 58
                    Width = 95
                    Height = 15
                    Caption = 'Endere'#231'o Servidor'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label24: TLabel
                    Left = 294
                    Top = 21
                    Width = 70
                    Height = 15
                    Caption = 'Cod.Empresa'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label30: TLabel
                    Left = 24
                    Top = 97
                    Width = 24
                    Height = 15
                    Caption = 'Filial'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edCodTerminal: TEdit
                    Left = 24
                    Top = 36
                    Width = 246
                    Height = 23
                    TabOrder = 0
                  end
                  object edPortaPinPad: TEdit
                    Left = 294
                    Top = 72
                    Width = 75
                    Height = 23
                    TabOrder = 3
                  end
                  object edEnderecoServidor: TEdit
                    Left = 24
                    Top = 73
                    Width = 246
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 2
                  end
                  object edCodEmpresa: TEdit
                    Left = 294
                    Top = 36
                    Width = 141
                    Height = 23
                    TabOrder = 1
                  end
                  object edCodFilial: TEdit
                    Left = 24
                    Top = 113
                    Width = 174
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 4
                    Text = '0001'
                  end
                end
              end
            end
          end
          object tsConfigImpressora: TTabSheet
            Caption = 'Impressora'
            ImageIndex = 1
            object gbConfigImpressora: TGroupBox
              Left = 0
              Top = 0
              Width = 759
              Height = 124
              Align = alTop
              Caption = 'Impressora'
              TabOrder = 0
              DesignSize = (
                759
                124)
              object Label25: TLabel
                Left = 171
                Top = 70
                Width = 64
                Height = 15
                Caption = 'Linhas Pular'
                Color = clBtnFace
                ParentColor = False
              end
              object Label26: TLabel
                Left = 93
                Top = 70
                Width = 42
                Height = 15
                Caption = 'Espa'#231'os'
                Color = clBtnFace
                ParentColor = False
              end
              object Label27: TLabel
                Left = 12
                Top = 70
                Width = 43
                Height = 15
                Caption = 'Colunas'
                Color = clBtnFace
                ParentColor = False
              end
              object Label28: TLabel
                Left = 12
                Top = 19
                Width = 41
                Height = 15
                Caption = 'Modelo'
                Color = clBtnFace
                ParentColor = False
                Transparent = False
              end
              object Label7: TLabel
                Left = 179
                Top = 19
                Width = 28
                Height = 15
                Caption = 'Porta'
                Color = clBtnFace
                ParentColor = False
              end
              object Label29: TLabel
                Left = 251
                Top = 70
                Width = 79
                Height = 15
                Caption = 'P'#225'g. de c'#243'digo'
                Color = clBtnFace
                ParentColor = False
                Transparent = False
              end
              object btSerial: TSpeedButton
                Left = 321
                Top = 32
                Width = 25
                Height = 26
                Anchors = [akTop, akRight]
                OnClick = btSerialClick
              end
              object btProcuraImpressoras: TSpeedButton
                Left = 345
                Top = 32
                Width = 25
                Height = 26
                Anchors = [akTop, akRight]
                OnClick = btProcuraImpressorasClick
              end
              object seLinhasPular: TSpinEdit
                Left = 171
                Top = 87
                Width = 60
                Height = 24
                MaxValue = 255
                MinValue = 0
                TabOrder = 4
                Value = 0
              end
              object seEspLinhas: TSpinEdit
                Left = 93
                Top = 87
                Width = 60
                Height = 24
                MaxValue = 255
                MinValue = 0
                TabOrder = 3
                Value = 0
              end
              object seColunas: TSpinEdit
                Left = 12
                Top = 87
                Width = 60
                Height = 24
                MaxValue = 999
                MinValue = 1
                TabOrder = 2
                Value = 48
              end
              object cbxModeloPosPrinter: TComboBox
                Left = 12
                Top = 36
                Width = 145
                Height = 23
                Style = csDropDownList
                TabOrder = 0
              end
              object cbxPorta: TComboBox
                Left = 171
                Top = 36
                Width = 169
                Height = 23
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 1
              end
              object cbxPagCodigo: TComboBox
                Left = 251
                Top = 87
                Width = 89
                Height = 23
                Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
                Style = csDropDownList
                TabOrder = 5
              end
              object btTestarPosPrinter: TBitBtn
                Left = 367
                Top = 29
                Width = 57
                Height = 74
                Anchors = [akTop, akRight]
                Caption = 'Testar'
                Layout = blGlyphTop
                TabOrder = 6
                OnClick = btTestarPosPrinterClick
              end
            end
          end
        end
      end
      object tsOperacao: TTabSheet
        Caption = 'Opera'#231#227'o'
        ImageIndex = 1
        object Splitter1: TSplitter
          Left = 762
          Top = 0
          Width = 5
          Height = 376
          Align = alRight
        end
        object pOperacao: TPanel
          Left = 0
          Top = 0
          Width = 762
          Height = 376
          Align = alClient
          BevelOuter = bvNone
          Constraints.MinWidth = 460
          TabOrder = 0
          object gbTotaisVenda: TGroupBox
            Left = 0
            Top = 50
            Width = 762
            Height = 136
            Align = alTop
            Caption = 'Valores da Opera'#231#227'o'
            TabOrder = 0
            DesignSize = (
              762
              136)
            object Label2: TLabel
              Left = 25
              Top = 24
              Width = 60
              Height = 15
              Caption = 'Valor Inicial'
              Color = clBtnFace
              ParentColor = False
            end
            object Label3: TLabel
              Left = 136
              Top = 24
              Width = 50
              Height = 15
              Caption = 'Desconto'
              Color = clBtnFace
              ParentColor = False
            end
            object Label4: TLabel
              Left = 248
              Top = 24
              Width = 56
              Height = 15
              Caption = 'Acr'#233'scimo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label5: TLabel
              Left = 25
              Top = 80
              Width = 89
              Height = 13
              Caption = 'Total Opera'#231#227'o'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object Label6: TLabel
              Left = 136
              Top = 80
              Width = 63
              Height = 13
              Caption = 'Total Pago'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object Label13: TLabel
              Left = 248
              Top = 80
              Width = 34
              Height = 13
              Caption = 'Troco'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object edTotalVenda: TEdit
              Left = 25
              Top = 96
              Width = 89
              Height = 24
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 6
              Text = '0.00'
            end
            object edTotalPago: TEdit
              Left = 136
              Top = 96
              Width = 89
              Height = 24
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 5
              Text = '0.00'
            end
            object edTroco: TEdit
              Left = 248
              Top = 96
              Width = 89
              Height = 24
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 4
              Text = '0.00'
            end
            object btEfetuarPagamentos: TBitBtn
              Left = 630
              Top = 83
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Pagamentos'
              Default = True
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 3
              OnClick = btEfetuarPagamentosClick
            end
            object btAdministrativo: TBitBtn
              Left = 630
              Top = 16
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Administrativo'
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 10
              OnClick = btAdministrativoClick
            end
            object cbSimularErroNoDoctoFiscal: TCheckBox
              Left = 354
              Top = 102
              Width = 151
              Height = 19
              Caption = 'Simular Erro Docto Fiscal'
              TabOrder = 8
            end
            object btObterCPF: TButton
              Left = 511
              Top = 16
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Solicita CPF'
              TabOrder = 7
              OnClick = btObterCPFClick
            end
            object btMsgPinPad: TButton
              Left = 511
              Top = 51
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Msg PinPad'
              TabOrder = 9
              OnClick = btMsgPinPadClick
            end
            object seValorInicialVenda: TEdit
              Left = 24
              Top = 40
              Width = 89
              Height = 23
              TabOrder = 0
              OnChange = seValorInicialVendaChange
              OnKeyPress = seValorInicialVendaKeyPress
            end
            object seTotalDesconto: TEdit
              Left = 136
              Top = 40
              Width = 89
              Height = 23
              TabOrder = 1
              OnChange = seTotalDescontoChange
              OnKeyPress = seValorInicialVendaKeyPress
            end
            object seTotalAcrescimo: TEdit
              Left = 248
              Top = 40
              Width = 89
              Height = 23
              TabOrder = 2
              OnChange = seTotalAcrescimoChange
              OnKeyPress = seValorInicialVendaKeyPress
            end
            object btExibirImagemPinPad: TButton
              Left = 392
              Top = 16
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Imagem PinPad'
              TabOrder = 11
              OnClick = btExibirImagemPinPadClick
            end
            object btMenuPinPad: TButton
              Left = 511
              Top = 83
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Menu PinPad'
              TabOrder = 12
              OnClick = btMenuPinPadClick
            end
            object btCancelarUltima: TBitBtn
              Left = 630
              Top = 51
              Width = 113
              Height = 28
              Anchors = [akTop, akRight]
              Caption = 'Cancelar Ultima'
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ImageIndex = 11
              Images = ImageList1
              ParentFont = False
              TabOrder = 13
              OnClick = btCancelarUltimaClick
            end
          end
          object gbPagamentos: TGroupBox
            Left = 0
            Top = 186
            Width = 762
            Height = 190
            Align = alClient
            Caption = 'Pagamentos'
            TabOrder = 1
            object sgPagamentos: TStringGrid
              Left = 2
              Top = 17
              Width = 652
              Height = 171
              Align = alClient
              ColCount = 7
              DefaultColWidth = 30
              FixedCols = 0
              RowCount = 1
              FixedRows = 0
              TabOrder = 0
            end
            object pBotoesPagamentos: TPanel
              Left = 654
              Top = 17
              Width = 106
              Height = 171
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 1
              DesignSize = (
                106
                171)
              object btIncluirPagamentos: TBitBtn
                Left = 8
                Top = 16
                Width = 88
                Height = 28
                Anchors = [akTop, akRight]
                Caption = 'Incluir'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                OnClick = btIncluirPagamentosClick
              end
              object btExcluirPagamento: TBitBtn
                Left = 8
                Top = 48
                Width = 88
                Height = 28
                Anchors = [akTop, akRight]
                Caption = 'Excluir'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = btExcluirPagamentoClick
              end
            end
          end
          object pStatus: TPanel
            Left = 0
            Top = 0
            Width = 762
            Height = 50
            Align = alTop
            BevelInner = bvLowered
            BevelWidth = 2
            Caption = 'CAIXA LIVRE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -24
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            object lNumOperacao: TLabel
              Left = 673
              Top = 4
              Width = 85
              Height = 42
              Align = alRight
              Caption = '000000'
              Color = clBtnFace
              ParentColor = False
              Layout = tlCenter
              Visible = False
            end
            object btOperacao: TBitBtn
              Left = 8
              Top = 11
              Width = 120
              Height = 28
              Cancel = True
              Caption = 'Cancelar Opera'#231#227'o'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              Visible = False
              OnClick = btOperacaoClick
            end
          end
        end
      end
    end
    object pLogs: TPanel
      Left = 0
      Top = 411
      Width = 775
      Height = 189
      Align = alClient
      TabOrder = 1
      object sbLimparLog: TSpeedButton
        Left = 751
        Top = 1
        Width = 23
        Height = 118
        OnClick = sbLimparLogClick
      end
      object mLog: TMemo
        Left = 1
        Top = 1
        Width = 773
        Height = 187
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object pImpressao: TPanel
    Left = 780
    Top = 0
    Width = 330
    Height = 600
    Align = alRight
    BevelOuter = bvNone
    Constraints.MinWidth = 300
    TabOrder = 0
    object lSaidaImpressao: TLabel
      Left = 0
      Top = 213
      Width = 330
      Height = 20
      Align = alTop
      Alignment = taCenter
      Caption = 'Sa'#237'da de Impress'#227'o'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lURLTEF: TLabel
      Left = 0
      Top = 0
      Width = 330
      Height = 20
      Cursor = crHandPoint
      Align = alTop
      Alignment = taCenter
      Caption = 'projetoacbr.com.br/tef'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      OnClick = lURLTEFClick
    end
    object mImpressao: TMemo
      Left = 0
      Top = 233
      Width = 330
      Height = 179
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Lucida Console'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object pSimulador: TPanel
      Left = 0
      Top = 532
      Width = 330
      Height = 68
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        330
        68)
      object btMudaPagina: TBitBtn
        Left = 113
        Top = 8
        Width = 112
        Height = 52
        Anchors = [akTop]
        Caption = 'Opera'#231'oes'
        Layout = blGlyphTop
        TabOrder = 0
        OnClick = btMudaPaginaClick
      end
    end
    object pMensagem: TPanel
      Left = 0
      Top = 20
      Width = 330
      Height = 193
      Align = alTop
      Anchors = []
      BevelInner = bvLowered
      BevelWidth = 2
      BorderStyle = bsSingle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Visible = False
      object pMensagemOperador: TPanel
        Left = 4
        Top = 4
        Width = 318
        Height = 70
        Align = alTop
        TabOrder = 0
        Visible = False
        object lTituloMsgOperador: TLabel
          Left = 1
          Top = 1
          Width = 316
          Height = 13
          Align = alTop
          Caption = 'Mensagem Operador'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object lMensagemOperador: TLabel
          Left = 1
          Top = 14
          Width = 316
          Height = 55
          Align = alClient
          Alignment = taCenter
          Caption = 'lMensagemOperador'
          Color = clBtnFace
          ParentColor = False
          Layout = tlCenter
          WordWrap = True
        end
      end
      object pMensagemCliente: TPanel
        Left = 4
        Top = 74
        Width = 318
        Height = 111
        Align = alClient
        TabOrder = 1
        Visible = False
        object lTituloMensagemCliente: TLabel
          Left = 1
          Top = 1
          Width = 316
          Height = 13
          Align = alTop
          Caption = 'Mensagem Cliente'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object lMensagemCliente: TLabel
          Left = 1
          Top = 14
          Width = 316
          Height = 96
          Align = alClient
          Alignment = taCenter
          Caption = 'lMensagemCliente'
          Color = clBtnFace
          ParentColor = False
          Layout = tlCenter
          WordWrap = True
        end
      end
    end
    object pImpressoraBotes: TPanel
      Left = 0
      Top = 498
      Width = 330
      Height = 34
      Align = alBottom
      TabOrder = 3
      DesignSize = (
        330
        34)
      object btImprimir: TBitBtn
        Left = 161
        Top = 2
        Width = 80
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Imprimir'
        TabOrder = 0
        OnClick = btImprimirClick
      end
      object btLimparImpressora: TBitBtn
        Left = 244
        Top = 2
        Width = 80
        Height = 28
        Anchors = [akTop, akRight]
        Caption = 'Limpar'
        TabOrder = 1
        OnClick = btLimparImpressoraClick
      end
      object cbEnviarImpressora: TCheckBox
        Left = 8
        Top = 8
        Width = 113
        Height = 19
        Caption = 'Enviar Impressora'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
    object pQRCode: TPanel
      Left = 0
      Top = 412
      Width = 330
      Height = 86
      Align = alBottom
      TabOrder = 4
      Visible = False
      object imgQRCode: TImage
        Left = 1
        Top = 1
        Width = 328
        Height = 84
        Align = alClient
        Center = True
        Proportional = True
        Stretch = True
      end
    end
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 0
    Left = 960
    Top = 248
  end
  object ACBrTEFAPI1: TACBrTEFAPI
    DadosAutomacao.AutoAtendimento = False
    QuandoGravarLog = ACBrTEFAPI1QuandoGravarLog
    QuandoFinalizarOperacao = ACBrTEFAPI1QuandoFinalizarOperacao
    QuandoFinalizarTransacao = ACBrTEFAPI1QuandoFinalizarTransacao
    QuandoDetectarTransacaoPendente = ACBrTEFAPI1QuandoDetectarTransacaoPendente
    QuandoEsperarOperacao = ACBrTEFAPI1QuandoEsperarOperacao
    QuandoExibirMensagem = ACBrTEFAPI1QuandoExibirMensagem
    QuandoPerguntarMenu = ACBrTEFAPI1QuandoPerguntarMenu
    QuandoPerguntarCampo = ACBrTEFAPI1QuandoPerguntarCampo
    QuandoExibirQRCode = ACBrTEFAPI1QuandoExibirQRCode
    Left = 962
    Top = 301
  end
  object ImageList1: TImageList
    Left = 968
    Top = 208
    Bitmap = {
      494C01010D000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F3F3
      F30000000000A0A0A00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003C3C3C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B4B4B400202020000000000040404000000000003C3C3C00000000007070
      7000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A0A0
      A0000000000000000000000000000000000000000000000000006C6C6C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7E7E7000000
      0000E3E3E3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001C1C
      1C00000000000000000000000000000000000000000000000000585858000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007C7C
      7C000000000000000000000000000000000000000000000000001C1C1C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004848
      4800000000000000000000000000000000000000000000000000444444000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000444444000000
      000000000000000000000000000000000000A0A0A00000000000DFDFDF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004848
      48000000000014141400606060000C0C0C0000000000CFCFCF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000054545400000000000000000000000000FBFBFB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3F3F30060606000181818001C1C1C0068686800F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3F3F30060606000181818001C1C1C0068686800F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3F3F3005C5C5C00181818001C1C1C0064646400F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000101010000000000000000000000000000000000000000000000000001818
      1800000000000000000000000000000000000000000000000000000000000000
      0000101010000000000000000000000000000000000000000000000000001818
      18000000000000000000000000000000000000000000B4B4B400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DBDBDB00000000000000000000000000000000000000
      0000101010000000000000000000000000000000000000000000000000001818
      1800000000000000000000000000000000000000000000000000000000003C3C
      3C00000000000000000000000000000000000000000000000000000000000000
      0000545454000000000000000000000000000000000000000000000000003C3C
      3C00000000000000000000000000000000000000000000000000000000000000
      0000545454000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010101000000000000000000000000000000000003C3C
      3C00000000000000000000000000000000000000000000000000000000000000
      0000545454000000000000000000000000000000000000000000101010000000
      0000000000000000000000000000404040004040400000000000000000000000
      0000000000002424240000000000000000000000000000000000101010000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002424240000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000101010000000
      00000000000050505000000000000000000000000000000000004C4C4C000000
      00000000000024242400000000000000000000000000F3F3F300000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      00000000000000000000000000000000000000000000F3F3F300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F3F3F300000000000000
      00005050500000000000909090000000000000000000ACACAC00000000003030
      3000000000000000000000000000000000000000000060606000000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      0000000000000000000080808000000000000000000060606000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C5C5C00000000000000
      000000000000909090000000000090909000ACACAC0000000000707070000000
      000000000000000000007C7C7C00000000000000000018181800000000000000
      0000404040008080800080808000BFBFBF00BFBFBF0080808000808080004040
      4000000000000000000038383800000000000000000018181800000000000000
      0000404040008080800080808000808080008080800080808000808080004040
      4000000000000000000038383800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018181800000000000000
      0000000000000000000090909000000000000000000070707000000000000000
      000000000000000000003838380000000000000000001C1C1C00000000000000
      0000404040008080800080808000BFBFBF00BFBFBF0080808000808080004040
      400000000000000000003C3C3C0000000000000000001C1C1C00000000000000
      0000404040008080800080808000808080008080800080808000808080004040
      400000000000000000003C3C3C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C1C1C00000000000000
      00000000000000000000ACACAC00000000000000000090909000000000000000
      000000000000000000003C3C3C00000000000000000068686800000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      0000000000000000000088888800000000000000000068686800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000088888800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000064646400000000000000
      000000000000ACACAC0000000000707070009090900000000000909090000000
      00000000000000000000848484000000000000000000F7F7F700000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      00000000000000000000000000000000000000000000F7F7F700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F7F700000000000000
      00004C4C4C000000000070707000000000000000000090909000000000003030
      3000000000000000000000000000000000000000000000000000181818000000
      0000000000000000000000000000404040004040400000000000000000000000
      0000000000003030300000000000000000000000000000000000181818000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003030300000000000000000000000000098989800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8C00000000000000000000000000181818000000
      0000000000003030300000000000000000000000000000000000303030000000
      0000000000003030300000000000000000000000000000000000000000005454
      5400000000000000000000000000000000000000000000000000000000000000
      0000707070000000000000000000000000000000000000000000000000005454
      5400000000000000000000000000000000000000000000000000000000000000
      0000707070000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005454
      5400000000000000000000000000000000000000000000000000000000000000
      0000707070000000000000000000000000000000000000000000000000000000
      0000242424000000000000000000000000000000000000000000000000003030
      3000000000000000000000000000000000000000000000000000000000000000
      0000242424000000000000000000000000000000000000000000000000003030
      3000000000000000000000000000000000000000000000000000000000000000
      0000000000002C2C2C0000000000000000000000000000000000303030000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000242424000000000000000000000000000000000000000000000000003030
      3000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000383838003C3C3C0088888800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000383838003C3C3C0088888800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FBFBFB002C2C2C0000000000000000004C4C4C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007C7C7C00383838003C3C3C0084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3F3F30060606000181818001C1C1C0068686800F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000101010000000000000000000000000000000000000000000000000001818
      1800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080808000808080008080800080808000808080000000
      0000000000000000000000000000000000000000000000000000000000003C3C
      3C00000000000000000000000000000000000000000000000000000000000000
      0000545454000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000080808000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800080808000000000000000000000000000101010000000
      0000000000000000000028282800000000000000000000000000000000000000
      0000000000002424240000000000000000000000000000000000000000000000
      0000707070000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F3F3F300000000000000
      0000000000002828280000000000E7E7E7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000060606000000000000000
      00002828280000000000E7E7E700000000002828280000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018181800000000000000
      000000000000E7E7E700000000003C3C3C000000000028282800000000000000
      0000000000000000000038383800000000000000000000000000000000008888
      8800000000000000000000000000000000000000000000000000000000000000
      0000909090000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001010100000000000000000001C1C1C00000000000000
      0000282828000000000000000000000000003C3C3C0000000000E7E7E7000000
      000000000000000000003C3C3C00000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A0A0A000000000000000000068686800000000000000
      000000000000000000000000000000000000000000003C3C3C0000000000E7E7
      E700000000000000000088888800000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000909090001010
      1000000000000000000000000000000000000000000000000000000000000000
      000010101000A0A0A000000000000000000000000000F7F7F700000000000000
      00000000000000000000000000000000000000000000000000003C3C3C000000
      0000282828000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000181818000000
      0000000000000000000000000000000000000000000000000000000000003C3C
      3C0000000000303030000000000000000000000000000000000000000000BFBF
      BF00000000008080800080808000404040004040400080808000808080000000
      0000BFBFBF000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005454
      5400000000000000000000000000000000000000000000000000000000000000
      0000707070000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BFBFBF00BFBFBF0000000000000000000000
      0000000000000000000000000000000000000000000080808000808080006868
      6800000000000000000000000000686868008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000242424000000000000000000000000000000000000000000000000003030
      3000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000383838003C3C3C0088888800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000000000000000000000000000000000000000000000000000B4B4
      B400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CFCFCF00E7E7E70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D7D7D700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000404
      0400000000000000000000000000F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006C6C6C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040404000000
      0000000000000000000000000000F7F7F7000000000058585800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A8A8A800000000000000000000000000000000000000
      0000000000000000000000000000444444003838380000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ACACAC0000000000000000000000
      0000000000002828280000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000282828000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0B0B000141414000000
      000000000000000000000000000000000000D7D7D70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EBEBEB00FBFBFB0000000000040404000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000ACACAC0000000000000000008C8C8C00000000000000
      000000000000000000000000000000000000000000000000000000000000E7E7
      E7000000000000000000000000000000000000000000D7D7D700000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF000000
      000000000000000000000000000000000000000000000000000000000000E7E7
      E700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000242424001C1C1C0000000000000000000000
      00000000000000000000000000000000000000000000DBDBDB00000000000000
      0000000000000000000000000000000000000000000000000000D7D7D7000000
      000000000000000000000000000000000000000000000C0C0C00000000000000
      0000000000000000000000000000000000000000000028282800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FBFBFB000000
      00000000000034343400D7D7D70000000000EFEFEF005C5C5C00000000000000
      000000000000000000000000000000000000EBEBEB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007878
      7800E7E7E7000000000000000000000000007C7C7C0000000000000000003C3C
      3C00282828000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CBCBCB00505050002020200040404000B0B0B000000000000000
      000000000000E7E7E70000000000000000006C6C6C00000000003C3C3C000000
      0000000000002828280000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E7E7E7000000000000000000FBFBFB00000000000000
      0000000000001818180000000000000000000404040000000000000000000000
      0000000000000000000000000000000000000000000098989800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8C00000000000000000000000000000000004040
      4000808080008080800080808000808080008080800080808000000000000000
      0000909090000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018181800000000000000000000000000F3F3F30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000686868000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D7D7
      D700000000000000000000000000505050000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BFBFBF00D3D3D30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFF7000000000000FFE3000000000000FFCF000000000000F08F000000000000
      E01F000000000000C73F000000000000CF9F000000000000CF9F000000000000
      CF9F000000000000CF1F000000000000E03F000000000000F07F000000000000
      FFFF000000000000FFFF000000000000FFFFFFFFFFFFFFFFF81FF81FFFFFF81F
      F00FF00F8001F00FE007E0078001E007C003C0038001C0038003800380018423
      8001800180018241800180018001818180018001800181818001800180018241
      8003800380018423C003C0038001C003E007E007F99FE007F00FF00FF81FF00F
      FC3FFC3FF83FFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF81FF81FFFFF
      F00FF00FF81FFFFFF00FE007F81FC0FF87E1C003F00FC04787E18203F00FC07F
      87E18501F00FC07F80018881E007C04380098041E007C07F80018021E007C041
      C0038013E007C041F00FC003E007807FF00FE007F66F807FF00FF00FF00FFFFF
      FFFFFC3FF00FFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFF1FFFFFFFFFFFF
      FFE0FFFFC003FFFFFFC08001C003FFFFFF039FF9C3C3FFFFFF079FF9C3C39F01
      F20F9FF9C1838F81C00F9FF9C0038FC1803F8001C003C101007F8001C003F001
      007F8001CFC3F839187F9FF9CFC3FFFDB87F8001C007FFFFF07FFFFFC00FFFFF
      E0FFFFFFFFFFFFFFF3FFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object ACBrAbecsPinPad1: TACBrAbecsPinPad
    OnWriteLog = ACBrAbecsPinPad1WriteLog
    Left = 840
    Top = 296
  end
end
