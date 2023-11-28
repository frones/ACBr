object frPixCDMatera: TfrPixCDMatera
  Left = 431
  Top = 143
  Caption = 'ACBrPIXCD Matera'
  ClientHeight = 738
  ClientWidth = 1030
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  TextHeight = 15
  object lbUrlPIX: TLabel
    Left = 0
    Top = 719
    Width = 1030
    Height = 19
    Cursor = crHandPoint
    Align = alBottom
    Alignment = taCenter
    Caption = 'https://projetoacbr.com.br/pix'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = lbUrlPIXClick
    ExplicitTop = 717
    ExplicitWidth = 224
  end
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 1030
    Height = 719
    ActivePage = tsConfig
    Align = alClient
    Images = ImageList1
    TabHeight = 30
    TabOrder = 0
    TabWidth = 250
    OnChange = pgPrincipalChange
    ExplicitWidth = 1026
    ExplicitHeight = 718
    object tsFluxoPagto: TTabSheet
      Caption = 'Fluxo de Pagamento'
      object pnFluxoBackground: TPanel
        Left = 0
        Top = 0
        Width = 1022
        Height = 679
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 1018
        ExplicitHeight = 678
        object pnFluxoPagto: TPanel
          Left = 44
          Top = 17
          Width = 938
          Height = 623
          BevelOuter = bvNone
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object gbFluxoStatus: TGroupBox
            Left = 0
            Top = 0
            Width = 938
            Height = 90
            Align = alTop
            Caption = 'Status'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            object pnFluxoStatus: TPanel
              Left = 2
              Top = 17
              Width = 934
              Height = 71
              Align = alClient
              BevelOuter = bvNone
              Caption = 'VENDENDO'
              Color = clMenuHighlight
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWhite
              Font.Height = -24
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentBackground = False
              ParentFont = False
              TabOrder = 0
              StyleElements = [seBorder]
            end
          end
          object pnFluxoRodape: TPanel
            Left = 0
            Top = 227
            Width = 938
            Height = 349
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 1
            object pnFluxoQRCode: TPanel
              Left = 672
              Top = 0
              Width = 266
              Height = 235
              Align = alClient
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 1
              object imFluxoQRCode: TImage
                Left = 0
                Top = 0
                Width = 266
                Height = 235
                Align = alClient
                Center = True
                Constraints.MinHeight = 140
                Proportional = True
                Stretch = True
              end
            end
            object pnFluxoBotoes: TPanel
              Left = 312
              Top = 0
              Width = 360
              Height = 235
              Align = alLeft
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvNone
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentColor = True
              ParentFont = False
              TabOrder = 2
              object pnFluxoBotoesErroConsultar: TPanel
                Left = 24
                Top = 0
                Width = 312
                Height = 235
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 2
                Visible = False
                object btFluxoTentarNovamente: TBitBtn
                  Left = 0
                  Top = 0
                  Width = 312
                  Height = 32
                  Caption = 'Tentar Novamente'
                  TabOrder = 0
                end
                object btFluxoCancelarConsulta: TBitBtn
                  Left = 0
                  Top = 32
                  Width = 312
                  Height = 32
                  Caption = 'Cancelar'
                  TabOrder = 1
                end
                object btFluxoFecharVenda: TBitBtn
                  Left = 0
                  Top = 64
                  Width = 312
                  Height = 32
                  Caption = 'Fechar Venda'
                  TabOrder = 2
                end
              end
              object pnFluxoBotoesRight: TPanel
                Left = 336
                Top = 0
                Width = 24
                Height = 235
                Align = alRight
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 1
              end
              object pnFluxoBotoesPrincipais: TPanel
                Left = 24
                Top = 0
                Width = 312
                Height = 235
                Align = alClient
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 0
                object btFluxoPagar: TBitBtn
                  Left = 0
                  Top = 0
                  Width = 312
                  Height = 30
                  Caption = 'PAGAR'
                  TabOrder = 0
                  OnClick = btFluxoPagarClick
                end
                object btFluxoEstornarPagto: TBitBtn
                  Left = 0
                  Top = 30
                  Width = 312
                  Height = 30
                  Caption = 'Estornar Pagamento'
                  TabOrder = 1
                  Visible = False
                  OnClick = btFluxoEstornarPagtoClick
                end
                object btFluxoNovaVenda: TBitBtn
                  Left = 0
                  Top = 60
                  Width = 312
                  Height = 30
                  Caption = 'Nova Venda'
                  TabOrder = 2
                  Visible = False
                  OnClick = btFluxoNovaVendaClick
                end
                object btFluxoCancelarVenda: TBitBtn
                  Left = 0
                  Top = 90
                  Width = 312
                  Height = 30
                  Caption = 'Cancelar'
                  TabOrder = 3
                  Visible = False
                  OnClick = btFluxoCancelarVendaClick
                end
              end
              object pnFluxoBotoesRight1: TPanel
                Left = 0
                Top = 0
                Width = 24
                Height = 235
                Align = alLeft
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 3
              end
            end
            object pnFluxoTotal: TPanel
              Left = 0
              Top = 0
              Width = 312
              Height = 235
              Align = alLeft
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 0
              object gbFluxoTotal: TGroupBox
                Left = 0
                Top = 0
                Width = 312
                Height = 104
                Align = alTop
                Caption = 'TOTAL'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 0
                object pnFluxoValor: TPanel
                  Left = 2
                  Top = 16
                  Width = 308
                  Height = 86
                  Align = alClient
                  BevelOuter = bvNone
                  ParentColor = True
                  TabOrder = 0
                  object edFluxoValor: TEdit
                    Left = 35
                    Top = 21
                    Width = 233
                    Height = 41
                    AutoSize = False
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -33
                    Font.Name = 'Tahoma'
                    Font.Style = []
                    ParentFont = False
                    TabOrder = 0
                    Text = '5,00'
                  end
                end
              end
            end
            object pnFluxoDiv7: TPanel
              Left = 0
              Top = 235
              Width = 938
              Height = 10
              Align = alBottom
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 3
            end
            object pnFluxoCopiaECola: TPanel
              Left = 0
              Top = 297
              Width = 938
              Height = 52
              Align = alBottom
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 4
              Visible = False
              object lbFluxoCopiaECola: TLabel
                Left = 26
                Top = 0
                Width = 92
                Height = 15
                Caption = 'PIX Copia e Cola'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object btFluxoCopiaECola: TSpeedButton
                Left = 888
                Top = 16
                Width = 26
                Height = 27
                OnClick = btFluxoCopiaEColaClick
              end
              object edFluxoCopiaECola: TEdit
                Left = 26
                Top = 16
                Width = 856
                Height = 27
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -17
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
              end
            end
            object pnFluxotransactionId: TPanel
              Left = 0
              Top = 245
              Width = 938
              Height = 52
              Align = alBottom
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 5
              Visible = False
              object lbFluxoCopiaECola1: TLabel
                Left = 26
                Top = 0
                Width = 76
                Height = 15
                Caption = 'transactionID'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object btFluxotransactionID: TSpeedButton
                Left = 888
                Top = 16
                Width = 26
                Height = 27
                OnClick = btFluxotransactionIDClick
              end
              object edFluxotransactionID: TEdit
                Left = 26
                Top = 16
                Width = 856
                Height = 27
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -17
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
              end
            end
          end
          object pnFluxoDiv2: TPanel
            Left = 0
            Top = 202
            Width = 938
            Height = 10
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 2
          end
          object pnFluxoDiv3: TPanel
            Left = 0
            Top = 212
            Width = 938
            Height = 15
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 3
          end
          object gbFluxoCliente: TGroupBox
            Left = 0
            Top = 105
            Width = 938
            Height = 97
            Align = alTop
            Caption = 'INFORMA'#199#213'ES ADICIONAIS'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 4
            object pnFluxoCliente: TPanel
              Left = 2
              Top = 17
              Width = 934
              Height = 78
              Align = alClient
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 0
              object lbFluxoClienteNome: TLabel
                Left = 312
                Top = 13
                Width = 29
                Height = 15
                Caption = 'Valor'
              end
              object lbFluxoClienteDoc: TLabel
                Left = 24
                Top = 13
                Width = 21
                Height = 15
                Caption = 'Info'
              end
              object edFluxoClienteNome: TEdit
                Left = 312
                Top = 28
                Width = 600
                Height = 32
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -20
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 0
                Text = 'Nome Consumidor'
              end
              object edFluxoClienteDoc: TEdit
                Left = 24
                Top = 28
                Width = 264
                Height = 32
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -20
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 1
                Text = 'Cliente'
              end
            end
          end
          object pnFluxoDiv4: TPanel
            Left = 0
            Top = 90
            Width = 938
            Height = 15
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 5
          end
          object pnSiteEfetuarPagto: TPanel
            Left = 0
            Top = 576
            Width = 938
            Height = 52
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 6
            Visible = False
            object lbSiteEfetuarPagto: TLabel
              Left = 0
              Top = 0
              Width = 938
              Height = 52
              Align = alClient
              Alignment = taCenter
              Caption = 'CLIQUE AQUI PARA EFETUAR O PAGAMENTO'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHighlight
              Font.Height = -20
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Layout = tlCenter
              OnClick = lbSiteEfetuarPagtoClick
              OnMouseEnter = lbSiteEfetuarPagtoMouseEnter
              OnMouseLeave = lbSiteEfetuarPagtoMouseLeave
              ExplicitWidth = 437
              ExplicitHeight = 24
            end
          end
        end
      end
    end
    object tsTestes: TTabSheet
      Caption = 'Opera'#231#245'es'
      ImageIndex = 14
      object Splitter2: TSplitter
        Left = 662
        Top = 0
        Width = 5
        Height = 679
        Align = alRight
        ExplicitLeft = 658
        ExplicitHeight = 677
      end
      object pcTestes: TPageControl
        Left = 0
        Top = 0
        Width = 662
        Height = 679
        ActivePage = tsGerarQRCodes
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 0
        TabWidth = 200
        object tsGerarQRCodes: TTabSheet
          Caption = 'Gerar QRCodes'
          ImageIndex = 1
          OnShow = tsGerarQRCodesShow
          object pnGerarQRCodeInfo: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 112
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lbQRCodeExternalID: TLabel
              Left = 15
              Top = 15
              Width = 59
              Height = 15
              Caption = 'External ID'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeValor: TLabel
              Left = 343
              Top = 15
              Width = 27
              Height = 15
              Caption = 'Valor'
              Color = clBtnFace
              ParentColor = False
            end
            object btQRCodeGerarExternalID: TSpeedButton
              Left = 295
              Top = 30
              Width = 24
              Height = 23
              Flat = True
              OnClick = btQRCodeGerarExternalIDClick
            end
            object lbQRCodeTipoCobranca: TLabel
              Left = 487
              Top = 15
              Width = 98
              Height = 15
              Caption = 'Tipo de Cobran'#231'a'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeMediatorFee: TLabel
              Left = 15
              Top = 59
              Width = 90
              Height = 15
              Caption = 'Mediator Fee R$'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeTipoMediatorFee: TLabel
              Left = 155
              Top = 59
              Width = 118
              Height = 15
              Caption = 'Tipo de Mediator  Fee'
              Color = clBtnFace
              ParentColor = False
            end
            object edQRCodeExternalID: TEdit
              Left = 15
              Top = 30
              Width = 280
              Height = 23
              TabOrder = 0
            end
            object edQRCodeValor: TEdit
              Left = 343
              Top = 30
              Width = 125
              Height = 23
              TabOrder = 1
            end
            object cbQRCodeTipoCobranca: TComboBox
              Left = 487
              Top = 30
              Width = 143
              Height = 23
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 2
              Text = 'Normal'
              OnChange = cbQRCodeTipoCobrancaChange
              Items.Strings = (
                'Normal'
                'Com Vencimento')
            end
            object edQRCodeMediatorFee: TEdit
              Left = 15
              Top = 75
              Width = 125
              Height = 23
              TabOrder = 3
            end
            object cbQRCodeTipoMediatorFee: TComboBox
              Left = 155
              Top = 75
              Width = 143
              Height = 23
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 4
              Text = 'Reais'
              OnChange = cbQRCodeTipoMediatorFeeChange
              Items.Strings = (
                'Reais'
                'Porcentagem')
            end
          end
          object pnQRCodeResult: TPanel
            Left = 0
            Top = 459
            Width = 654
            Height = 146
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            Visible = False
            object btCobCopiaECola: TSpeedButton
              Left = 605
              Top = 23
              Width = 26
              Height = 26
              OnClick = btCobCopiaEColaClick
            end
            object lbCobCopiaECola: TLabel
              Left = 240
              Top = 10
              Width = 72
              Height = 15
              Caption = 'Copia e Cola'
              Color = clBtnFace
              ParentColor = False
            end
            object lbDisclaimerCallBack: TLabel
              Left = 240
              Top = 64
              Width = 356
              Height = 96
              Alignment = taCenter
              Caption = 
                'A Flagship recomenda o uso do webhook para que a agencia tenha'#13#10 +
                'os retornos das opera'#231#245'es em produ'#231#227'o, principalmente do '#13#10'detal' +
                'hamento dos processos de abertura de contas. '#13#10'Esta configura'#231#227'o' +
                ' pode ser feita tamb'#233'm em sandbox, e '#13#10'para isso, acionar a Flag' +
                'ship para apoio na configura'#231#227'o.'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 3683321
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              WordWrap = True
            end
            object edCobCopiaECola: TEdit
              Left = 240
              Top = 26
              Width = 363
              Height = 23
              TabOrder = 0
            end
            object pnGerarQRCodeImg: TPanel
              Left = 0
              Top = 0
              Width = 232
              Height = 146
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
              object imGerarQRCodeImg: TImage
                Left = 16
                Top = 4
                Width = 200
                Height = 175
                Proportional = True
                Stretch = True
              end
            end
          end
          object gbGerarQRCodeInfoAdicional: TGroupBox
            Left = 0
            Top = 112
            Width = 654
            Height = 122
            Align = alTop
            Caption = 'Informa'#231#245'es Adicionais'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            object pnQRCodeInfoAdicionais: TPanel
              Left = 2
              Top = 17
              Width = 650
              Height = 103
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbQRCodeADname: TLabel
                Left = 14
                Top = 3
                Width = 34
                Height = 15
                Caption = 'Nome'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodeADcontent: TLabel
                Left = 256
                Top = 3
                Width = 54
                Height = 15
                Caption = 'Conte'#250'do'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodeCallBack: TLabel
                Left = 14
                Top = 50
                Width = 95
                Height = 15
                Hint = 
                  'A Flagship recomenda o uso do webhook para que a agencia tenha'#13#10 +
                  'os retornos das opera'#231#245'es em produ'#231#227'o, principalmente do '#13#10'detal' +
                  'hamento dos processos de abertura de contas. '#13#10'Esta configura'#231#227'o' +
                  ' pode ser feita tamb'#233'm em sandbox, e '#13#10'para isso, acionar a Flag' +
                  'ship para apoio na configura'#231#227'o.'
                Caption = 'CallBackAddress'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCoderecipientComment: TLabel
                Left = 256
                Top = 50
                Width = 110
                Height = 15
                Caption = 'Recipient Comment'
                Color = clBtnFace
                ParentColor = False
              end
              object edQRCodeADname: TEdit
                Left = 14
                Top = 18
                Width = 218
                Height = 23
                TabOrder = 0
              end
              object edQRCodeADcontent: TEdit
                Left = 256
                Top = 18
                Width = 244
                Height = 23
                TabOrder = 1
              end
              object chkQRCodeADshowToPayer: TCheckBox
                Left = 512
                Top = 16
                Width = 112
                Height = 19
                Caption = 'Exibir ao Pagador'
                TabOrder = 2
              end
              object edQRCodeCallBack: TEdit
                Left = 14
                Top = 65
                Width = 218
                Height = 23
                Hint = 
                  'A Flagship recomenda o uso do webhook para que a agencia tenha'#13#10 +
                  'os retornos das opera'#231#245'es em produ'#231#227'o, principalmente do '#13#10'detal' +
                  'hamento dos processos de abertura de contas. '#13#10'Esta configura'#231#227'o' +
                  ' pode ser feita tamb'#233'm em sandbox, e '#13#10'para isso, acionar a Flag' +
                  'ship para apoio na configura'#231#227'o.'
                TabOrder = 3
              end
              object edQRCoderecipientComment: TEdit
                Left = 256
                Top = 65
                Width = 368
                Height = 23
                TabOrder = 4
              end
            end
          end
          object gbQRCodeDetalhesCobranca: TGroupBox
            Left = 0
            Top = 234
            Width = 654
            Height = 225
            Align = alTop
            Caption = 'Detalhes da Cobran'#231'a'
            TabOrder = 3
            Visible = False
            object pnQRCodeDetalhesCobranca: TPanel
              Left = 2
              Top = 17
              Width = 650
              Height = 206
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbQRCodedueDate: TLabel
                Left = 185
                Top = 103
                Width = 47
                Height = 15
                Caption = 'dueDate'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodePayercpfcnpj: TLabel
                Left = 14
                Top = 3
                Width = 65
                Height = 15
                Caption = 'CPF / CNPJ'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodePayerName: TLabel
                Left = 185
                Top = 3
                Width = 34
                Height = 15
                Caption = 'Name'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayerstreet: TLabel
                Left = 14
                Top = 53
                Width = 64
                Height = 15
                Caption = 'Logradouro'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayercity: TLabel
                Left = 185
                Top = 53
                Width = 40
                Height = 15
                Caption = 'Cidade'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayeruf: TLabel
                Left = 330
                Top = 53
                Width = 16
                Height = 15
                Caption = 'UF'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayerCEP: TLabel
                Left = 14
                Top = 103
                Width = 25
                Height = 15
                Caption = 'CEP'
                Color = clBtnFace
                ParentColor = False
              end
              object edQRCodedueDate: TDateTimePicker
                Left = 185
                Top = 118
                Width = 184
                Height = 23
                Date = 45114.000000000000000000
                Time = 45114.000000000000000000
                MaxDate = 2958465.999988426000000000
                MinDate = -53780.000000000000000000
                TabOrder = 0
              end
              object gbQRCodeDiscount: TGroupBox
                Left = 371
                Top = 0
                Width = 120
                Height = 206
                Align = alRight
                Caption = 'discounts*'
                TabOrder = 1
                object pnQRCodeDiscount: TPanel
                  Left = 2
                  Top = 17
                  Width = 116
                  Height = 187
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbQRCodediscountsvaluePerc: TLabel
                    Left = 10
                    Top = 55
                    Width = 54
                    Height = 15
                    Caption = 'valuePerc'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsmodality: TLabel
                    Left = 10
                    Top = 3
                    Width = 46
                    Height = 15
                    Caption = 'modality'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsdate: TLabel
                    Left = 10
                    Top = 108
                    Width = 24
                    Height = 15
                    Caption = 'date'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsAviso: TLabel
                    Left = 11
                    Top = 149
                    Width = 57
                    Height = 45
                    Caption = '*somente em'#13#10'produ'#231#227'o'
                    Color = clBtnFace
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clRed
                    Font.Height = -12
                    Font.Name = 'MS Sans Serif'
                    Font.Style = []
                    ParentColor = False
                    ParentFont = False
                    WordWrap = True
                  end
                  object edQRCodediscountsvaluePerc: TEdit
                    Left = 10
                    Top = 70
                    Width = 83
                    Height = 23
                    TabOrder = 0
                    Text = '5,00'
                  end
                  object edQRCodediscountsmodality: TEdit
                    Left = 10
                    Top = 18
                    Width = 83
                    Height = 23
                    TabOrder = 1
                    Text = '1'
                  end
                  object edQRCodediscountsdate: TDateTimePicker
                    Left = 10
                    Top = 123
                    Width = 83
                    Height = 23
                    Date = 45114.000000000000000000
                    Time = 45114.000000000000000000
                    MaxDate = 2958465.999988426000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 2
                  end
                end
              end
              object edQRCodePayercpfcnpj: TEdit
                Left = 14
                Top = 18
                Width = 154
                Height = 23
                TabOrder = 2
                OnKeyPress = edOnlyNumbersKeyPress
              end
              object edQRCodePayerName: TEdit
                Left = 185
                Top = 18
                Width = 184
                Height = 23
                TabOrder = 3
              end
              object edQRCodepayerstreet: TEdit
                Left = 14
                Top = 68
                Width = 154
                Height = 23
                TabOrder = 4
              end
              object edQRCodepayercity: TEdit
                Left = 185
                Top = 68
                Width = 135
                Height = 23
                TabOrder = 5
              end
              object edQRCodepayeruf: TEdit
                Left = 330
                Top = 68
                Width = 39
                Height = 23
                TabOrder = 6
              end
              object edQRCodepayerCEP: TEdit
                Left = 14
                Top = 118
                Width = 154
                Height = 23
                TabOrder = 7
                OnChange = edContaCriarCEPChange
                OnKeyPress = edOnlyNumbersKeyPress
              end
              object pnQRCodeValues: TPanel
                Left = 491
                Top = 0
                Width = 159
                Height = 206
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 8
                object gbQRCodeValues: TGroupBox
                  Left = 0
                  Top = 68
                  Width = 159
                  Height = 69
                  Align = alTop
                  Caption = 'fines'
                  TabOrder = 0
                  object pnQRCodeValuesFines: TPanel
                    Left = 2
                    Top = 17
                    Width = 155
                    Height = 50
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodefinesvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 54
                      Height = 15
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodefinesmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 46
                      Height = 15
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodefinesvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 23
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodefinesmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 23
                      TabOrder = 1
                      Text = '1'
                    end
                  end
                end
                object gbQRCodeValuesReduction: TGroupBox
                  Left = 0
                  Top = 137
                  Width = 159
                  Height = 68
                  Align = alTop
                  Caption = 'reduction'
                  TabOrder = 1
                  object pnQRCodeValuesReduction: TPanel
                    Left = 2
                    Top = 17
                    Width = 155
                    Height = 49
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodereductionvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 54
                      Height = 15
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodereductionmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 46
                      Height = 15
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodereductionvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 23
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodereductionmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 23
                      TabOrder = 1
                      Text = '1'
                    end
                  end
                end
                object gbQRCodeValuesInterests: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 159
                  Height = 68
                  Align = alTop
                  Caption = 'interests'
                  TabOrder = 2
                  object pnQRCodeValuesInterests: TPanel
                    Left = 2
                    Top = 17
                    Width = 155
                    Height = 49
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodeinterestsvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 54
                      Height = 15
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodeinterestsmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 46
                      Height = 15
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodeinterestsvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 23
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodeinterestsmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 23
                      TabOrder = 1
                      Text = '1'
                    end
                  end
                end
              end
            end
          end
          object pnQRCodeDetalhesCobrancaRodape: TPanel
            Left = 0
            Top = 605
            Width = 654
            Height = 34
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 4
            object btCriarCobranca: TBitBtn
              Left = 505
              Top = 5
              Width = 130
              Height = 26
              Caption = 'Criar'
              TabOrder = 0
              OnClick = btCriarCobrancaClick
            end
            object btQRCodeCriarLimparDados: TBitBtn
              Left = 382
              Top = 5
              Width = 119
              Height = 26
              Caption = 'Limpar Dados'
              TabOrder = 1
              OnClick = btQRCodeCriarLimparDadosClick
            end
            object btQRCodeCriarPreencherDados: TBitBtn
              Left = 196
              Top = 5
              Width = 186
              Height = 26
              Caption = 'Preencher(Dados Fict'#237'cios)'
              TabOrder = 2
              OnClick = btQRCodeCriarPreencherDadosClick
            end
          end
        end
        object tsConsultas: TTabSheet
          Caption = 'Consultas'
          ImageIndex = 8
          object pnConsultas: TPanel
            Left = 15
            Top = 25
            Width = 633
            Height = 639
            BevelOuter = bvNone
            TabOrder = 0
          end
          object pgConsultas: TPageControl
            Left = 0
            Top = 0
            Width = 654
            Height = 639
            ActivePage = tsConsultaMediator
            Align = alClient
            TabOrder = 1
            object tsConsultaTxID: TTabSheet
              Caption = 'Consulta Transa'#231#227'o'
              object gbConsultaTxID: TGroupBox
                Left = 0
                Top = 0
                Width = 646
                Height = 95
                Align = alTop
                Caption = 'Consulta de Transa'#231#245'es por transactionId'
                TabOrder = 0
                object pnConsultaTxID: TPanel
                  Left = 2
                  Top = 17
                  Width = 642
                  Height = 76
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    642
                    76)
                  object lbConsultarCobTransactionID: TLabel
                    Left = 25
                    Top = 15
                    Width = 80
                    Height = 15
                    Caption = 'Transaction ID'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edConsultarCobTransactionID: TEdit
                    Left = 25
                    Top = 30
                    Width = 439
                    Height = 23
                    TabOrder = 0
                  end
                  object btConsultarCob: TBitBtn
                    Left = 491
                    Top = 27
                    Width = 130
                    Height = 26
                    Anchors = [akTop, akRight]
                    Caption = 'Consultar'
                    TabOrder = 1
                    OnClick = btConsultarCobClick
                  end
                end
              end
            end
            object tsConsultaSaldoEC: TTabSheet
              Caption = 'Consultas Account'
              object pnConsultasExtratoEC: TPanel
                Left = 0
                Top = 0
                Width = 646
                Height = 216
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 0
                object gbConsultaExtratoEC: TGroupBox
                  Left = 0
                  Top = 112
                  Width = 646
                  Height = 104
                  Align = alClient
                  Caption = 'Consulta de extrato - EC'
                  TabOrder = 0
                  object pnConsultaExtratoEC: TPanel
                    Left = 2
                    Top = 17
                    Width = 642
                    Height = 85
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    DesignSize = (
                      642
                      85)
                    object lbConsultaStart1: TLabel
                      Left = 21
                      Top = 15
                      Width = 25
                      Height = 15
                      Caption = 'Start'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbconsultaEnding1: TLabel
                      Left = 165
                      Top = 15
                      Width = 39
                      Height = 15
                      Caption = 'Ending'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edConsultaStart1: TDateTimePicker
                      Left = 21
                      Top = 32
                      Width = 130
                      Height = 23
                      Date = 45209.000000000000000000
                      Time = 45209.000000000000000000
                      MaxDate = 2958465.999988426000000000
                      MinDate = -53780.000000000000000000
                      TabOrder = 0
                    end
                    object edconsultaEnding1: TDateTimePicker
                      Left = 165
                      Top = 32
                      Width = 130
                      Height = 23
                      Date = 45209.000000000000000000
                      Time = 45209.000000000000000000
                      MaxDate = 2958465.999988426000000000
                      MinDate = -53780.000000000000000000
                      TabOrder = 1
                    end
                    object btConsultarExtratoEC: TBitBtn
                      Left = 507
                      Top = 29
                      Width = 130
                      Height = 26
                      Anchors = [akTop, akRight]
                      Caption = 'Consultar'
                      TabOrder = 2
                      OnClick = btConsultarExtratoECClick
                    end
                  end
                end
                object gbconsultaSaldoEC: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 646
                  Height = 112
                  Align = alTop
                  Caption = 'Consulta de saldo - EC'
                  TabOrder = 1
                  object pnConsultaSaldoEC: TPanel
                    Left = 2
                    Top = 17
                    Width = 642
                    Height = 93
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    DesignSize = (
                      642
                      93)
                    object lbAccavaliable: TLabel
                      Left = 493
                      Top = 23
                      Width = 114
                      Height = 38
                      Alignment = taRightJustify
                      Caption = 'R$ 0,00'
                      Color = clBtnFace
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -33
                      Font.Name = 'MS Sans Serif'
                      Font.Style = [fsBold]
                      ParentColor = False
                      ParentFont = False
                      Visible = False
                    end
                    object lbConsultaSaldoECAvaliable: TLabel
                      Left = 344
                      Top = 37
                      Width = 84
                      Height = 23
                      Caption = 'Avaliable:'
                      Color = clBtnFace
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -20
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentColor = False
                      ParentFont = False
                      Visible = False
                    end
                    object btConsultarSaldoEC: TBitBtn
                      Left = 9
                      Top = 32
                      Width = 130
                      Height = 26
                      Anchors = [akTop, akRight]
                      Caption = 'Consultar'
                      TabOrder = 0
                      OnClick = btConsultarSaldoECClick
                    end
                  end
                end
              end
              object pnAccExtrato: TPanel
                Left = 0
                Top = 216
                Width = 646
                Height = 393
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object sgAccExtrato: TStringGrid
                  Left = 0
                  Top = 0
                  Width = 646
                  Height = 393
                  Align = alClient
                  ColCount = 6
                  FixedCols = 0
                  TabOrder = 0
                  ColWidths = (
                    64
                    245
                    72
                    201
                    201
                    35)
                end
              end
            end
            object tsConsultaMediator: TTabSheet
              Caption = 'Consultas Mediator'
              object gbConsultaExtratoIntegrador: TGroupBox
                Left = 0
                Top = 144
                Width = 646
                Height = 140
                Align = alTop
                Caption = 'Consulta de extrato - Integrador (Mediator)'
                TabOrder = 0
                object pnConsultaExtratoIntegrador: TPanel
                  Left = 2
                  Top = 17
                  Width = 642
                  Height = 121
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    642
                    121)
                  object lbConsultarEXIntegradorAccountID: TLabel
                    Left = 25
                    Top = 15
                    Width = 105
                    Height = 15
                    Caption = 'Mediator Account Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbConsultaStart: TLabel
                    Left = 25
                    Top = 64
                    Width = 25
                    Height = 15
                    Caption = 'Start'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbconsultaEnding: TLabel
                    Left = 169
                    Top = 64
                    Width = 39
                    Height = 15
                    Caption = 'Ending'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edConsultarEXIntegradorAccountID: TEdit
                    Left = 25
                    Top = 30
                    Width = 455
                    Height = 23
                    TabOrder = 0
                  end
                  object edConsultaStart: TDateTimePicker
                    Left = 25
                    Top = 79
                    Width = 130
                    Height = 23
                    Date = 45209.000000000000000000
                    Time = 45209.000000000000000000
                    MaxDate = 2958465.999988426000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 1
                  end
                  object edconsultaEnding: TDateTimePicker
                    Left = 169
                    Top = 79
                    Width = 130
                    Height = 23
                    Date = 45209.000000000000000000
                    Time = 45209.000000000000000000
                    MaxDate = 2958465.999988426000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 2
                  end
                  object btConsultarExtratoMediator: TBitBtn
                    Left = 507
                    Top = 27
                    Width = 130
                    Height = 26
                    Anchors = [akTop, akRight]
                    Caption = 'Consultar'
                    TabOrder = 3
                    OnClick = btConsultarExtratoMediatorClick
                  end
                end
              end
              object gbconsultaSaldoIntegrador: TGroupBox
                Left = 0
                Top = 0
                Width = 646
                Height = 144
                Align = alTop
                Caption = 'Consulta de saldo - Integrador (Mediator)'
                TabOrder = 1
                object pnConsultaSaldoIntegrador: TPanel
                  Left = 2
                  Top = 17
                  Width = 642
                  Height = 125
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    642
                    125)
                  object lbConsultarSaldoIntegradorAccountID: TLabel
                    Left = 25
                    Top = 15
                    Width = 105
                    Height = 15
                    Caption = 'Mediator Account Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbMediatoravaliableStr: TLabel
                    Left = 25
                    Top = 80
                    Width = 82
                    Height = 23
                    Caption = 'avaliable:'
                    Color = clBtnFace
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -20
                    Font.Name = 'MS Sans Serif'
                    Font.Style = []
                    ParentColor = False
                    ParentFont = False
                    Visible = False
                  end
                  object lbMediatoravaliable: TLabel
                    Left = 185
                    Top = 66
                    Width = 114
                    Height = 38
                    Alignment = taRightJustify
                    Caption = 'R$ 0,00'
                    Color = clBtnFace
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -33
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentColor = False
                    ParentFont = False
                    Visible = False
                  end
                  object edConsultarSaldoIntegradorAccountID: TEdit
                    Left = 25
                    Top = 30
                    Width = 455
                    Height = 23
                    TabOrder = 0
                  end
                  object btConsultarSaldoMediator: TBitBtn
                    Left = 491
                    Top = 27
                    Width = 130
                    Height = 26
                    Anchors = [akTop, akRight]
                    Caption = 'Consultar'
                    TabOrder = 1
                    OnClick = btConsultarSaldoMediatorClick
                  end
                end
              end
              object pnMediatorExtrato: TPanel
                Left = 0
                Top = 284
                Width = 646
                Height = 325
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 2
                object sgMediatorExtrato: TStringGrid
                  Left = 0
                  Top = 0
                  Width = 646
                  Height = 325
                  Align = alClient
                  ColCount = 6
                  FixedCols = 0
                  TabOrder = 0
                  ColWidths = (
                    64
                    245
                    72
                    201
                    201
                    35)
                end
              end
            end
          end
        end
        object tsDevolucoes: TTabSheet
          Caption = 'Devolu'#231#245'es'
          ImageIndex = 12
          OnShow = tsDevolucoesShow
          object gbDevolucao: TGroupBox
            Left = 0
            Top = 64
            Width = 654
            Height = 575
            Align = alClient
            Caption = 'Efetuar Devolu'#231#227'o'
            TabOrder = 0
            object pnDevolucao: TPanel
              Left = 2
              Top = 17
              Width = 650
              Height = 556
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbDevolucaoCobTransactionID: TLabel
                Left = 15
                Top = 25
                Width = 80
                Height = 15
                Caption = 'Transaction ID'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoValor: TLabel
                Left = 176
                Top = 73
                Width = 27
                Height = 15
                Caption = 'Valor'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoReasonCode: TLabel
                Left = 15
                Top = 73
                Width = 116
                Height = 15
                Caption = 'Codigo da devolu'#231#227'o'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoExternalID: TLabel
                Left = 337
                Top = 25
                Width = 59
                Height = 15
                Caption = 'External ID'
                Color = clBtnFace
                ParentColor = False
              end
              object btQRCodeGerarExternalID1: TSpeedButton
                Left = 621
                Top = 40
                Width = 24
                Height = 23
                Flat = True
                OnClick = btQRCodeGerarExternalID1Click
              end
              object lbMediatorFeeDevolucao: TLabel
                Left = 337
                Top = 73
                Width = 90
                Height = 15
                Caption = 'Mediator Fee R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFeeDevolucao: TLabel
                Left = 498
                Top = 73
                Width = 118
                Height = 15
                Caption = 'Tipo de Mediator  Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object edDevolucaoCobTransactionID: TEdit
                Left = 15
                Top = 40
                Width = 308
                Height = 23
                TabOrder = 0
              end
              object edDevolucaoValor: TEdit
                Left = 176
                Top = 89
                Width = 147
                Height = 23
                TabOrder = 1
              end
              object edDevolucaoExternalID: TEdit
                Left = 337
                Top = 39
                Width = 281
                Height = 23
                TabOrder = 2
              end
              object pnDevolucaoBotoes: TPanel
                Left = 0
                Top = 508
                Width = 650
                Height = 48
                Align = alBottom
                BevelOuter = bvNone
                TabOrder = 3
                object btDevolucaoPreencherDados: TBitBtn
                  Left = 192
                  Top = 11
                  Width = 186
                  Height = 26
                  Caption = 'Preencher(Dados Fict'#237'cios)'
                  TabOrder = 0
                  OnClick = btDevolucaoPreencherDadosClick
                end
                object btDevolucaoLimparDados: TBitBtn
                  Left = 389
                  Top = 11
                  Width = 119
                  Height = 26
                  Caption = 'Limpar Dados'
                  TabOrder = 1
                  OnClick = btDevolucaoLimparDadosClick
                end
                object btDevolucao: TBitBtn
                  Left = 519
                  Top = 13
                  Width = 130
                  Height = 23
                  Caption = 'Devolu'#231#227'o'
                  TabOrder = 2
                  OnClick = btDevolucaoClick
                end
              end
              object edMediatorFeeDevolucao: TEdit
                Left = 337
                Top = 89
                Width = 147
                Height = 23
                TabOrder = 4
              end
              object cbTipoMediatorFeeDevolucao: TComboBox
                Left = 498
                Top = 89
                Width = 147
                Height = 23
                Style = csDropDownList
                ItemIndex = 0
                TabOrder = 5
                Text = 'Reais'
                OnChange = cbTipoMediatorFeeDevolucaoChange
                Items.Strings = (
                  'Reais'
                  'Porcentagem')
              end
              object cbDevolucaoReasonCode: TComboBox
                Left = 15
                Top = 89
                Width = 147
                Height = 23
                TabOrder = 6
                OnDropDown = cbDevolucaoReasonCodeDropDown
              end
            end
          end
          object pnMotivosDevolucao: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 64
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object btConsultarMotivosDevolucoes: TBitBtn
              Left = 15
              Top = 22
              Width = 130
              Height = 26
              Caption = 'Consultar Motivos'
              TabOrder = 0
              OnClick = btConsultarMotivosDevolucoesClick
            end
          end
        end
        object tsRetirada: TTabSheet
          Caption = 'Retirada'
          ImageIndex = 33
          object pnRetirada: TPanel
            Left = 15
            Top = 25
            Width = 634
            Height = 663
            BevelOuter = bvNone
            TabOrder = 0
            object gbConsultaAliasRetirada: TGroupBox
              Left = 0
              Top = 0
              Width = 634
              Height = 133
              Align = alTop
              Caption = 'Consultar alias do destinat'#225'rio'
              TabOrder = 0
              object pnConsultaAliasRetirada: TPanel
                Left = 2
                Top = 17
                Width = 630
                Height = 114
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object lbConsultarCobAccountID2: TLabel
                  Left = 15
                  Top = 10
                  Width = 56
                  Height = 15
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbConsultarCobTransactionID3: TLabel
                  Left = 323
                  Top = 11
                  Width = 97
                  Height = 15
                  Caption = 'Alias Destinat'#225'rio'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edRetiradaConsultaAliasAccountID: TEdit
                  Left = 15
                  Top = 25
                  Width = 290
                  Height = 23
                  TabOrder = 0
                end
                object edRetiradaConsultaaliasAliasDestinatario: TEdit
                  Left = 323
                  Top = 25
                  Width = 290
                  Height = 23
                  TabOrder = 1
                end
                object btConsultarAliasRetirada: TBitBtn
                  Left = 483
                  Top = 66
                  Width = 130
                  Height = 26
                  Caption = 'Consultar alias'
                  TabOrder = 2
                  OnClick = btConsultarAliasRetiradaClick
                end
                object btRetiradaConsultaPreencherDados: TBitBtn
                  Left = 171
                  Top = 69
                  Width = 186
                  Height = 26
                  Caption = 'Preencher(Dados Fict'#237'cios)'
                  TabOrder = 3
                  OnClick = btRetiradaConsultaPreencherDadosClick
                end
                object btretiradaConsultaLimparDados: TBitBtn
                  Left = 357
                  Top = 69
                  Width = 119
                  Height = 26
                  Caption = 'Limpar Dados'
                  TabOrder = 4
                  OnClick = btretiradaConsultaLimparDadosClick
                end
              end
            end
            object gbRetirada: TGroupBox
              Left = 0
              Top = 133
              Width = 634
              Height = 499
              Align = alTop
              Caption = 'Retirada'
              TabOrder = 1
              object gbPIXRetirada: TGroupBox
                Left = 2
                Top = 131
                Width = 630
                Height = 145
                Align = alTop
                Caption = 'PIX'
                TabOrder = 0
                object pnPIXRetirada: TPanel
                  Left = 2
                  Top = 17
                  Width = 626
                  Height = 126
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbRetiradaAliasDestinatario: TLabel
                    Left = 15
                    Top = 10
                    Width = 97
                    Height = 15
                    Caption = 'Alias Destinat'#225'rio'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaendToEndId: TLabel
                    Left = 323
                    Top = 10
                    Width = 78
                    Height = 15
                    Caption = 'End To End ID'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaPSPId: TLabel
                    Left = 15
                    Top = 65
                    Width = 37
                    Height = 15
                    Caption = 'PSP Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountdestinationBranch: TLabel
                    Left = 153
                    Top = 65
                    Width = 151
                    Height = 15
                    Caption = 'Account Destination Branch'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountDestinationAccount: TLabel
                    Left = 464
                    Top = 65
                    Width = 154
                    Height = 15
                    Caption = 'Account Destination Account'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTaxID: TLabel
                    Left = 323
                    Top = 65
                    Width = 28
                    Height = 15
                    Caption = 'TaxId'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRetiradaAliasDestinatario: TEdit
                    Left = 15
                    Top = 25
                    Width = 290
                    Height = 23
                    TabOrder = 0
                  end
                  object edRetiradaendToEndId: TEdit
                    Left = 323
                    Top = 25
                    Width = 282
                    Height = 23
                    TabOrder = 1
                  end
                  object edRetiradaPSPId: TEdit
                    Left = 15
                    Top = 80
                    Width = 125
                    Height = 23
                    TabOrder = 2
                  end
                  object edRetiradaAccountdestinationBranch: TEdit
                    Left = 153
                    Top = 80
                    Width = 152
                    Height = 23
                    TabOrder = 3
                  end
                  object edRetiradaAccountDestinationAccount: TEdit
                    Left = 464
                    Top = 80
                    Width = 141
                    Height = 23
                    TabOrder = 4
                  end
                  object edRetiradaTaxID: TEdit
                    Left = 323
                    Top = 80
                    Width = 128
                    Height = 23
                    TabOrder = 5
                  end
                end
              end
              object pnlRetirada: TPanel
                Left = 2
                Top = 17
                Width = 630
                Height = 114
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object lbRetiradaExternalID: TLabel
                  Left = 17
                  Top = 10
                  Width = 59
                  Height = 15
                  Caption = 'External ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btRetiradaGerarExternalID: TSpeedButton
                  Left = 283
                  Top = 24
                  Width = 24
                  Height = 23
                  Flat = True
                  OnClick = btRetiradaGerarExternalIDClick
                end
                object lbRetiradaTipoRetirada: TLabel
                  Left = 325
                  Top = 10
                  Width = 91
                  Height = 15
                  Caption = 'Tipo de Retirada'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbRetiradaMediatorFee: TLabel
                  Left = 155
                  Top = 65
                  Width = 71
                  Height = 15
                  Caption = 'Mediator Fee'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbRetiradaValor: TLabel
                  Left = 17
                  Top = 65
                  Width = 27
                  Height = 15
                  Caption = 'Valor'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edRetiradaExternalID: TEdit
                  Left = 17
                  Top = 25
                  Width = 266
                  Height = 23
                  TabOrder = 0
                end
                object cbRetiradaTipoRetirada: TComboBox
                  Left = 325
                  Top = 25
                  Width = 282
                  Height = 23
                  Style = csDropDownList
                  ItemIndex = 0
                  TabOrder = 1
                  Text = 'PIX'
                  OnChange = cbRetiradaTipoRetiradaChange
                  Items.Strings = (
                    'PIX'
                    'TED')
                end
                object edRetiradaMediatorFee: TEdit
                  Left = 155
                  Top = 79
                  Width = 128
                  Height = 23
                  TabOrder = 2
                end
                object edRetiradaValor: TEdit
                  Left = 17
                  Top = 79
                  Width = 127
                  Height = 23
                  TabOrder = 3
                end
              end
              object pnRetiradaBotoes: TPanel
                Left = 2
                Top = 421
                Width = 630
                Height = 61
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
                object btRetiradaPreencherDados: TBitBtn
                  Left = 167
                  Top = 16
                  Width = 186
                  Height = 26
                  Caption = 'Preencher(Dados Fict'#237'cios)'
                  TabOrder = 0
                  OnClick = btRetiradaPreencherDadosClick
                end
                object btretiradaLimparDados: TBitBtn
                  Left = 355
                  Top = 17
                  Width = 119
                  Height = 26
                  Caption = 'Limpar Dados'
                  TabOrder = 1
                  OnClick = btretiradaLimparDadosClick
                end
                object btRetirada: TBitBtn
                  Left = 477
                  Top = 16
                  Width = 130
                  Height = 26
                  Caption = 'Retirada'
                  TabOrder = 2
                  OnClick = btRetiradaClick
                end
              end
              object gbTEDRetirada: TGroupBox
                Left = 2
                Top = 276
                Width = 630
                Height = 145
                Align = alTop
                Caption = 'TED'
                TabOrder = 3
                Visible = False
                object pnTEDRetirada: TPanel
                  Left = 2
                  Top = 17
                  Width = 626
                  Height = 126
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbRetiradaTEDBankDestination: TLabel
                    Left = 15
                    Top = 10
                    Width = 94
                    Height = 15
                    Caption = 'Bank Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDName: TLabel
                    Left = 323
                    Top = 10
                    Width = 34
                    Height = 15
                    Caption = 'Name'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaPersonType: TLabel
                    Left = 15
                    Top = 65
                    Width = 68
                    Height = 15
                    Caption = 'Person Type'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDTaxID: TLabel
                    Left = 301
                    Top = 65
                    Width = 28
                    Height = 15
                    Caption = 'TaxId'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDBranchDestination: TLabel
                    Left = 383
                    Top = 65
                    Width = 105
                    Height = 15
                    Caption = 'Branch Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDAccountDestination: TLabel
                    Left = 497
                    Top = 65
                    Width = 109
                    Height = 15
                    Caption = 'Account Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountTypeDestination: TLabel
                    Left = 153
                    Top = 65
                    Width = 137
                    Height = 15
                    Caption = 'Account Type Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRetiradaTEDBankDestination: TEdit
                    Left = 15
                    Top = 25
                    Width = 290
                    Height = 23
                    TabOrder = 0
                  end
                  object edRetiradaTEDName: TEdit
                    Left = 323
                    Top = 25
                    Width = 282
                    Height = 23
                    TabOrder = 1
                  end
                  object edRetiradaTEDTaxID: TEdit
                    Left = 301
                    Top = 80
                    Width = 70
                    Height = 23
                    TabOrder = 2
                  end
                  object edRetiradaTEDBranchDestination: TEdit
                    Left = 383
                    Top = 80
                    Width = 100
                    Height = 23
                    TabOrder = 3
                  end
                  object edRetiradaTEDAccountDestination: TEdit
                    Left = 497
                    Top = 80
                    Width = 108
                    Height = 23
                    TabOrder = 4
                  end
                  object cbRetiradaPersonType: TComboBox
                    Left = 15
                    Top = 80
                    Width = 125
                    Height = 23
                    ItemIndex = 0
                    TabOrder = 5
                    Text = 'PERSON'
                    Items.Strings = (
                      'PERSON'
                      'CORPORATE')
                  end
                  object cbRetiradaAccountTypeDestination: TComboBox
                    Left = 153
                    Top = 80
                    Width = 135
                    Height = 23
                    ItemIndex = 0
                    TabOrder = 6
                    Text = 'CC'
                    Items.Strings = (
                      'CC'
                      'POUPANCA'
                      'IP')
                  end
                end
              end
            end
          end
        end
      end
      object pnLogs1: TPanel
        Left = 667
        Top = 0
        Width = 355
        Height = 679
        Align = alRight
        TabOrder = 1
        object lbLog1: TLabel
          Left = 1
          Top = 1
          Width = 353
          Height = 15
          Align = alTop
          Caption = 'Log das Requisi'#231#245'es:'
          Color = clBtnFace
          ParentColor = False
          ExplicitWidth = 121
        end
        object mmLogOperacoes: TMemo
          Left = 1
          Top = 16
          Width = 353
          Height = 630
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape1: TPanel
          Left = 1
          Top = 646
          Width = 353
          Height = 32
          Align = alBottom
          TabOrder = 1
          DesignSize = (
            353
            32)
          object btLogOperacoesLimpar: TBitBtn
            Left = 252
            Top = 1
            Width = 83
            Height = 26
            Anchors = [akTop]
            Caption = 'Limpar'
            TabOrder = 0
            OnClick = btLogOperacoesLimparClick
          end
        end
      end
    end
    object tsContaEChaves: TTabSheet
      Caption = 'Contas/Chaves PIX'
      ImageIndex = 3
      object Splitter1: TSplitter
        Left = 1017
        Top = 0
        Width = 5
        Height = 679
        Align = alRight
        ExplicitLeft = 1013
        ExplicitHeight = 677
      end
      object pgContasEChaves: TPageControl
        Left = 0
        Top = 0
        Width = 662
        Height = 679
        ActivePage = tsContaCriar
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 0
        TabWidth = 200
        object tsContaCriar: TTabSheet
          Caption = 'Criar Conta'
          ImageIndex = 31
          object pnContaCriar: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 594
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbContaCriarExternalID: TLabel
              Left = 20
              Top = 25
              Width = 94
              Height = 15
              Caption = 'External Identifier'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaCriarExternalID: TSpeedButton
              Left = 261
              Top = 40
              Width = 24
              Height = 23
              Flat = True
              OnClick = btContaCriarExternalIDClick
            end
            object lbContaCriarTipoCliente: TLabel
              Left = 301
              Top = 25
              Width = 66
              Height = 15
              Caption = 'Tipo Cliente'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriarTipoConta: TLabel
              Left = 467
              Top = 25
              Width = 77
              Height = 15
              Caption = 'Tipo da Conta'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriarNomeCliente: TLabel
              Left = 20
              Top = 75
              Width = 76
              Height = 15
              Caption = 'Nome Cliente'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriarCelular: TLabel
              Left = 272
              Top = 75
              Width = 40
              Height = 15
              Caption = 'Celular'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriar: TLabel
              Left = 381
              Top = 75
              Width = 36
              Height = 15
              Caption = 'E-mail'
              Color = clBtnFace
              ParentColor = False
            end
            object lbCNPJ: TLabel
              Left = 537
              Top = 75
              Width = 32
              Height = 15
              Caption = 'CNPJ'
              Color = clBtnFace
              ParentColor = False
            end
            object lbAvisoAberturaConta: TLabel
              Left = 64
              Top = 536
              Width = 543
              Height = 48
              Alignment = taCenter
              Caption = 
                'Atente-se de que antes da abertura de qualquer conta em produ'#231#227'o' +
                ', o representante '#13#10'da empresa precisa obrigatoriamente aceitar ' +
                'os Termos de Uso da Plataforma '#13#10'(documento desenvolvido e distr' +
                'ibuido pela Flagship)'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 3683321
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object gbDadosAdicionais: TGroupBox
              Left = 20
              Top = 270
              Width = 622
              Height = 258
              Caption = 'Dados Adicionais da Conta'
              TabOrder = 0
              Visible = False
              object pnImagem: TPanel
                Left = 2
                Top = 17
                Width = 618
                Height = 239
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object Imagem: TImage
                  Left = 0
                  Top = 37
                  Width = 618
                  Height = 202
                  Align = alClient
                  AutoSize = True
                  Center = True
                  Proportional = True
                  Stretch = True
                  ExplicitHeight = 204
                end
                object pnRodapeImagem: TPanel
                  Left = 0
                  Top = 0
                  Width = 618
                  Height = 37
                  Align = alTop
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    618
                    37)
                  object btVoltar: TBitBtn
                    Left = 582
                    Top = 5
                    Width = 21
                    Height = 26
                    Anchors = [akTop]
                    Caption = 'X'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -12
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                    TabOrder = 0
                    OnClick = btVoltarClick
                  end
                end
              end
              object pcContaCriarDadosAdicionais: TPageControl
                Left = 2
                Top = 17
                Width = 618
                Height = 239
                ActivePage = tsContaCriarCorporate
                Align = alClient
                TabOrder = 0
                Visible = False
                object tsContaCriarCorporate: TTabSheet
                  Caption = 'Corporate'
                  object pnContaCriarCorporate: TPanel
                    Left = 0
                    Top = 0
                    Width = 610
                    Height = 209
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbContaCriarFundacao: TLabel
                      Left = 10
                      Top = 5
                      Width = 84
                      Height = 15
                      Caption = 'Data Funda'#231#227'o'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarNomeEmpresa: TLabel
                      Left = 103
                      Top = 5
                      Width = 88
                      Height = 15
                      Caption = 'Nome Empresa'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteCelular: TLabel
                      Left = 10
                      Top = 55
                      Width = 40
                      Height = 15
                      Caption = 'Celular'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentante: TLabel
                      Left = 103
                      Top = 55
                      Width = 136
                      Height = 15
                      Caption = 'Nome do Representante'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteMae: TLabel
                      Left = 242
                      Top = 55
                      Width = 77
                      Height = 15
                      Caption = 'Nome da M'#227'e'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteCPF: TLabel
                      Left = 365
                      Top = 5
                      Width = 109
                      Height = 15
                      Caption = 'CPF Representante'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteEmail: TLabel
                      Left = 365
                      Top = 55
                      Width = 32
                      Height = 15
                      Caption = 'Email'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarNascimento: TLabel
                      Left = 486
                      Top = 5
                      Width = 96
                      Height = 15
                      Caption = 'Nasc. Represent.'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteCEP: TLabel
                      Left = 10
                      Top = 105
                      Width = 25
                      Height = 15
                      Caption = 'CEP'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteLogradouro: TLabel
                      Left = 103
                      Top = 105
                      Width = 64
                      Height = 15
                      Caption = 'Logradouro'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteNumero: TLabel
                      Left = 242
                      Top = 105
                      Width = 45
                      Height = 15
                      Caption = 'N'#250'mero'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteBairro: TLabel
                      Left = 314
                      Top = 105
                      Width = 33
                      Height = 15
                      Caption = 'Bairro'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteCidade: TLabel
                      Left = 427
                      Top = 105
                      Width = 40
                      Height = 15
                      Caption = 'Cidade'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteUF: TLabel
                      Left = 544
                      Top = 105
                      Width = 16
                      Height = 15
                      Caption = 'UF'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbContaCriarRepresentanteFoto: TLabel
                      Left = 10
                      Top = 155
                      Width = 24
                      Height = 15
                      Caption = 'Foto'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarRepresentanteFoto: TSpeedButton
                      Left = 148
                      Top = 170
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteFotoClick
                    end
                    object lbContaCriarRepresentanteRGFrente: TLabel
                      Left = 208
                      Top = 155
                      Width = 91
                      Height = 15
                      Caption = 'Foto RG (Frente)'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarRepresentanteRGFotoFrente: TSpeedButton
                      Left = 344
                      Top = 171
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteRGFotoFrenteClick
                    end
                    object lbContaCriarRepresentanteRGVerso: TLabel
                      Left = 402
                      Top = 155
                      Width = 87
                      Height = 15
                      Caption = 'Foto RG (Verso)'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarRepresentanteRGFotoVerso: TSpeedButton
                      Left = 542
                      Top = 171
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteRGFotoVersoClick
                    end
                    object btContaCriarRepresentanteMostrarRGFotoVerso: TSpeedButton
                      Left = 573
                      Top = 170
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteMostrarRGFotoVersoClick
                    end
                    object btContaCriarRepresentanteMostrarRGFotoFrente: TSpeedButton
                      Left = 373
                      Top = 170
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteMostrarRGFotoFrenteClick
                    end
                    object btContaCriarRepresentanteMostrarRGFotoFrente1: TSpeedButton
                      Left = 176
                      Top = 171
                      Width = 24
                      Height = 23
                      Flat = True
                      Font.Charset = DEFAULT_CHARSET
                      Font.Color = clWindowText
                      Font.Height = -11
                      Font.Name = 'MS Sans Serif'
                      Font.Style = []
                      ParentFont = False
                      ParentShowHint = False
                      ShowHint = True
                      OnClick = btContaCriarRepresentanteMostrarRGFotoFrente1Click
                    end
                    object edContaCriarNomeEmpresa: TEdit
                      Left = 103
                      Top = 20
                      Width = 252
                      Height = 23
                      TabOrder = 1
                    end
                    object edContaCriarRepresentanteCelular: TEdit
                      Left = 10
                      Top = 70
                      Width = 83
                      Height = 23
                      TabOrder = 4
                    end
                    object edContaCriarRepresentanteNome: TEdit
                      Left = 103
                      Top = 70
                      Width = 130
                      Height = 23
                      TabOrder = 5
                    end
                    object edContaCriarRepresentanteMae: TEdit
                      Left = 242
                      Top = 70
                      Width = 113
                      Height = 23
                      TabOrder = 6
                    end
                    object edContaCriarRepresentanteCPF: TEdit
                      Left = 365
                      Top = 20
                      Width = 102
                      Height = 23
                      TabOrder = 2
                    end
                    object edContaCriarRepresentanteEmail: TEdit
                      Left = 365
                      Top = 70
                      Width = 232
                      Height = 23
                      TabOrder = 7
                    end
                    object edContaCriarFundacao: TDateTimePicker
                      Left = 10
                      Top = 20
                      Width = 83
                      Height = 23
                      Date = 45114.000000000000000000
                      Time = 45114.000000000000000000
                      MaxDate = 2958465.999988426000000000
                      MinDate = -53780.000000000000000000
                      TabOrder = 0
                    end
                    object edContaCriarNascimento: TDateTimePicker
                      Left = 486
                      Top = 20
                      Width = 111
                      Height = 23
                      Date = 45114.000000000000000000
                      Time = 45114.000000000000000000
                      MaxDate = 2958465.999988426000000000
                      MinDate = -53780.000000000000000000
                      TabOrder = 3
                    end
                    object edContaCriarRepresentanteCEP: TEdit
                      Left = 10
                      Top = 120
                      Width = 83
                      Height = 23
                      TabOrder = 8
                      OnChange = edContaCriarCEPChange
                      OnKeyPress = edOnlyNumbersKeyPress
                    end
                    object edContaCriarRepresentanteLogradouro: TEdit
                      Left = 103
                      Top = 120
                      Width = 130
                      Height = 23
                      TabOrder = 9
                    end
                    object edContaCriarRepresentanteNumero: TEdit
                      Left = 242
                      Top = 120
                      Width = 60
                      Height = 23
                      TabOrder = 10
                    end
                    object edContaCriarRepresentanteBairro: TEdit
                      Left = 314
                      Top = 120
                      Width = 103
                      Height = 23
                      TabOrder = 11
                    end
                    object edContaCriarRepresentanteCidade: TEdit
                      Left = 427
                      Top = 120
                      Width = 107
                      Height = 23
                      TabOrder = 12
                    end
                    object edContaCriarRepresentanteUF: TEdit
                      Left = 544
                      Top = 120
                      Width = 39
                      Height = 23
                      TabOrder = 13
                    end
                    object edContaCriarRepresentanteFoto: TEdit
                      Left = 10
                      Top = 170
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 14
                    end
                    object edContaCriarRepresentanteRGFotoFrente: TEdit
                      Left = 208
                      Top = 170
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 15
                    end
                    object edContaCriarRepresentanteRGFotoVerso: TEdit
                      Left = 404
                      Top = 170
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 16
                    end
                  end
                end
              end
            end
            object gbContaCriarEndereco: TGroupBox
              Left = 20
              Top = 136
              Width = 622
              Height = 120
              Caption = 'Endere'#231'o'
              TabOrder = 1
              object pnContaCriarEndereco: TPanel
                Left = 2
                Top = 17
                Width = 618
                Height = 101
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object lbContaCriarCEP: TLabel
                  Left = 16
                  Top = 0
                  Width = 25
                  Height = 15
                  Caption = 'CEP'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarLogradouro: TLabel
                  Left = 198
                  Top = 0
                  Width = 64
                  Height = 15
                  Caption = 'Logradouro'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarNumero: TLabel
                  Left = 518
                  Top = 0
                  Width = 45
                  Height = 15
                  Caption = 'N'#250'mero'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarBairro: TLabel
                  Left = 198
                  Top = 50
                  Width = 33
                  Height = 15
                  Caption = 'Bairro'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarComplemento: TLabel
                  Left = 16
                  Top = 50
                  Width = 79
                  Height = 15
                  Caption = 'Complemento'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarCidade: TLabel
                  Left = 358
                  Top = 50
                  Width = 40
                  Height = 15
                  Caption = 'Cidade'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarUF: TLabel
                  Left = 518
                  Top = 50
                  Width = 16
                  Height = 15
                  Caption = 'UF'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edContaCriarCEP: TEdit
                  Left = 15
                  Top = 15
                  Width = 168
                  Height = 23
                  TabOrder = 0
                  OnChange = edContaCriarCEPChange
                  OnKeyPress = edOnlyNumbersKeyPress
                end
                object edContaCriarLogradouro: TEdit
                  Left = 198
                  Top = 15
                  Width = 304
                  Height = 23
                  TabOrder = 1
                end
                object edContaCriarNumero: TEdit
                  Left = 519
                  Top = 15
                  Width = 85
                  Height = 23
                  TabOrder = 2
                end
                object edContaCriarBairro: TEdit
                  Left = 198
                  Top = 65
                  Width = 145
                  Height = 23
                  TabOrder = 3
                end
                object edContaCriarComplemento: TEdit
                  Left = 16
                  Top = 65
                  Width = 167
                  Height = 23
                  TabOrder = 4
                end
                object edContaCriarCidade: TEdit
                  Left = 358
                  Top = 65
                  Width = 144
                  Height = 23
                  TabOrder = 5
                end
                object edContaCriarUF: TEdit
                  Left = 518
                  Top = 65
                  Width = 86
                  Height = 23
                  TabOrder = 6
                end
              end
            end
            object edContaCriarExternalID: TEdit
              Left = 20
              Top = 40
              Width = 241
              Height = 23
              TabOrder = 2
            end
            object cbContaCriarTipoCliente: TComboBox
              Left = 300
              Top = 40
              Width = 152
              Height = 23
              Style = csDropDownList
              TabOrder = 3
              OnChange = cbContaCriarTipoClienteChange
            end
            object cbCriarContaTipoConta: TComboBox
              Left = 467
              Top = 40
              Width = 175
              Height = 23
              Style = csDropDownList
              TabOrder = 4
            end
            object edContaCriarNomeCliente: TEdit
              Left = 20
              Top = 90
              Width = 241
              Height = 23
              TabOrder = 5
            end
            object edContaCriarCelular: TEdit
              Left = 272
              Top = 90
              Width = 103
              Height = 23
              TabOrder = 6
              OnKeyPress = edOnlyNumbersKeyPress
            end
            object edContaCriarEmail: TEdit
              Left = 381
              Top = 90
              Width = 147
              Height = 23
              TabOrder = 7
            end
            object edCNPJ: TEdit
              Left = 537
              Top = 90
              Width = 105
              Height = 23
              TabOrder = 8
            end
          end
          object pnContaCriarRodape: TPanel
            Left = 0
            Top = 594
            Width = 654
            Height = 45
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              654
              45)
            object btContaCriar: TBitBtn
              Left = 525
              Top = 13
              Width = 112
              Height = 26
              Anchors = [akTop]
              Caption = 'Criar'
              TabOrder = 2
              OnClick = btContaCriarClick
            end
            object btContaCriarPreencherDados: TBitBtn
              Left = 201
              Top = 13
              Width = 186
              Height = 26
              Anchors = [akTop]
              Caption = 'Preencher(Dados Fict'#237'cios)'
              TabOrder = 0
              OnClick = btContaCriarPreencherDadosClick
            end
            object btContaCriarLimparDados: TBitBtn
              Left = 402
              Top = 13
              Width = 119
              Height = 26
              Anchors = [akTop]
              Caption = 'Limpar Dados'
              TabOrder = 1
              OnClick = btContaCriarLimparDadosClick
            end
          end
        end
        object tsContaConsultar: TTabSheet
          Caption = 'Consultar Conta'
          ImageIndex = 29
          object pnContaConsultar: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 639
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbContaConsultarAccountId: TLabel
              Left = 15
              Top = 20
              Width = 56
              Height = 15
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaConsultar: TBitBtn
              Left = 520
              Top = 32
              Width = 112
              Height = 26
              Caption = 'Consultar'
              TabOrder = 0
              OnClick = btContaConsultarClick
            end
            object edContaConsultarAccountId: TEdit
              Left = 15
              Top = 35
              Width = 489
              Height = 23
              TabOrder = 1
            end
            object mmContaConsultarResposta: TMemo
              Left = 15
              Top = 80
              Width = 617
              Height = 496
              TabOrder = 2
            end
          end
        end
        object tsContaInativar: TTabSheet
          Caption = 'Inativar Conta'
          ImageIndex = 17
          object pnContaInativar: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 639
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              654
              639)
            object lbContaInativarAccountId: TLabel
              Left = 15
              Top = 25
              Width = 56
              Height = 15
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaInativar: TBitBtn
              Left = 529
              Top = 37
              Width = 112
              Height = 26
              Anchors = [akTop]
              Caption = 'Inativar'
              TabOrder = 0
              OnClick = btContaInativarClick
            end
            object edContaInativarAccountId: TEdit
              Left = 15
              Top = 40
              Width = 489
              Height = 23
              TabOrder = 1
            end
          end
        end
        object tsChavePIX: TTabSheet
          Caption = 'Chave PIX'
          ImageIndex = 28
          object pnChavePIX: TPanel
            Left = 0
            Top = 0
            Width = 654
            Height = 639
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object gbChavePIXIncluir: TGroupBox
              Left = 15
              Top = 25
              Width = 592
              Height = 95
              Caption = 'Incluir Chave PIX'
              TabOrder = 0
              object pnChavePIXIncluir: TPanel
                Left = 2
                Top = 17
                Width = 588
                Height = 76
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  76)
                object lbChavePIXIncluirExternalId: TLabel
                  Left = 228
                  Top = 15
                  Width = 59
                  Height = 15
                  Caption = 'External ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXIncluirAccountId: TLabel
                  Left = 15
                  Top = 15
                  Width = 56
                  Height = 15
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btChavePIXCriarExternalId: TSpeedButton
                  Left = 416
                  Top = 30
                  Width = 24
                  Height = 23
                  Flat = True
                  OnClick = btChavePIXCriarExternalIdClick
                end
                object edChavePIXIncluirExternalId: TEdit
                  Left = 228
                  Top = 30
                  Width = 188
                  Height = 23
                  TabOrder = 0
                end
                object btChavePIXIncluir: TBitBtn
                  Left = 467
                  Top = 27
                  Width = 112
                  Height = 26
                  Anchors = [akTop]
                  Caption = 'Incluir'
                  TabOrder = 1
                  OnClick = btChavePIXIncluirClick
                end
                object edChavePIXIncluirAccountId: TEdit
                  Left = 15
                  Top = 30
                  Width = 200
                  Height = 23
                  TabOrder = 2
                end
              end
            end
            object gbChavePIXConsultar: TGroupBox
              Left = 15
              Top = 142
              Width = 592
              Height = 95
              Caption = 'Consultar Chaves PIX'
              TabOrder = 1
              object pnChavesPIXConsultar: TPanel
                Left = 2
                Top = 17
                Width = 588
                Height = 76
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  76)
                object lbChavePIXConsultar: TLabel
                  Left = 15
                  Top = 15
                  Width = 56
                  Height = 15
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edChavePIXConsultar: TEdit
                  Left = 15
                  Top = 30
                  Width = 425
                  Height = 23
                  TabOrder = 0
                end
                object btChavePIXConsultar: TBitBtn
                  Left = 467
                  Top = 27
                  Width = 112
                  Height = 26
                  Anchors = [akTop]
                  Caption = 'Consultar'
                  TabOrder = 1
                  OnClick = btChavePIXConsultarClick
                end
              end
            end
            object gbChavePIXExcluir: TGroupBox
              Left = 15
              Top = 264
              Width = 592
              Height = 95
              Caption = 'Excluir Chave PIX'
              TabOrder = 2
              object pnChavePIXExcluir: TPanel
                Left = 2
                Top = 17
                Width = 588
                Height = 76
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  76)
                object lbChavePIXExcluirAccountId: TLabel
                  Left = 15
                  Top = 15
                  Width = 56
                  Height = 15
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXExcluir: TLabel
                  Left = 228
                  Top = 15
                  Width = 56
                  Height = 15
                  Caption = 'Chave PIX'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btChavePIXExcluir: TBitBtn
                  Left = 467
                  Top = 27
                  Width = 112
                  Height = 26
                  Anchors = [akTop]
                  Caption = 'Excluir'
                  TabOrder = 0
                  OnClick = btChavePIXExcluirClick
                end
                object edChavePIXExcluirAccountId: TEdit
                  Left = 15
                  Top = 30
                  Width = 200
                  Height = 23
                  TabOrder = 1
                end
                object edChavePIXExcluir: TEdit
                  Left = 228
                  Top = 30
                  Width = 212
                  Height = 23
                  TabOrder = 2
                end
              end
            end
          end
        end
      end
      object pnLogs: TPanel
        Left = 662
        Top = 0
        Width = 355
        Height = 679
        Align = alRight
        TabOrder = 1
        object lbLog: TLabel
          Left = 1
          Top = 1
          Width = 353
          Height = 15
          Align = alTop
          Caption = 'Log das Requisi'#231#245'es:'
          Color = clBtnFace
          ParentColor = False
          ExplicitWidth = 121
        end
        object mmLogGerencial: TMemo
          Left = 1
          Top = 16
          Width = 353
          Height = 630
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape: TPanel
          Left = 1
          Top = 646
          Width = 353
          Height = 32
          Align = alBottom
          TabOrder = 1
          DesignSize = (
            353
            32)
          object btLogGerencialLimpar: TBitBtn
            Left = 252
            Top = 1
            Width = 83
            Height = 26
            Anchors = [akTop]
            Caption = 'Limpar'
            TabOrder = 0
            OnClick = btLogGerencialLimparClick
          end
        end
      end
    end
    object tsConfig: TTabSheet
      Caption = 'Configura'#231#227'o'
      ImageIndex = 2
      object pBotoesConfiguracao: TPanel
        Left = 0
        Top = 642
        Width = 1022
        Height = 37
        Align = alBottom
        TabOrder = 0
        object btSalvarParametros: TBitBtn
          Left = 32
          Top = 4
          Width = 136
          Height = 28
          Caption = 'Salvar Par'#226'metros'
          TabOrder = 0
          OnClick = btSalvarParametrosClick
        end
        object btLerParametros: TBitBtn
          Left = 176
          Top = 4
          Width = 136
          Height = 28
          Caption = 'Ler Par'#226'metros'
          TabOrder = 1
          OnClick = btLerParametrosClick
        end
      end
      object pgConfig: TPageControl
        Left = 0
        Top = 0
        Width = 1022
        Height = 642
        ActivePage = tsMatera
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 1
        TabWidth = 172
        object tsPIX: TTabSheet
          Caption = 'PIX'
          object pConfPIX: TPanel
            Left = 0
            Top = 0
            Width = 1014
            Height = 602
            Align = alClient
            Anchors = []
            BevelOuter = bvSpace
            TabOrder = 0
            object pnConfig2: TPanel
              Left = 1
              Top = 81
              Width = 1012
              Height = 133
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object gbProxy: TGroupBox
                Left = 0
                Top = 0
                Width = 304
                Height = 133
                Align = alLeft
                Caption = 'Proxy'
                TabOrder = 0
                object pnProxy: TPanel
                  Left = 2
                  Top = 17
                  Width = 300
                  Height = 114
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    300
                    114)
                  object lbProxyHost: TLabel
                    Left = 10
                    Top = 5
                    Width = 26
                    Height = 15
                    Caption = 'Host'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxyPorta: TLabel
                    Left = 192
                    Top = 5
                    Width = 29
                    Height = 15
                    Anchors = [akTop, akRight]
                    Caption = 'Porta'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxyUsuario: TLabel
                    Left = 10
                    Top = 50
                    Width = 44
                    Height = 15
                    Caption = 'Usu'#225'rio'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxySenha: TLabel
                    Left = 192
                    Top = 50
                    Width = 36
                    Height = 15
                    Anchors = [akTop, akRight]
                    Caption = 'Senha'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btProxyVerSenha: TSpeedButton
                    Left = 268
                    Top = 65
                    Width = 23
                    Height = 23
                    AllowAllUp = True
                    Anchors = [akTop, akRight]
                    GroupIndex = 1
                    Flat = True
                    OnClick = btProxyVerSenhaClick
                  end
                  object edProxyHost: TEdit
                    Left = 10
                    Top = 20
                    Width = 168
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 0
                  end
                  object edProxyUsuario: TEdit
                    Left = 10
                    Top = 65
                    Width = 168
                    Height = 23
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 1
                  end
                  object edProxySenha: TEdit
                    Left = 192
                    Top = 65
                    Width = 74
                    Height = 23
                    Anchors = [akTop, akRight]
                    PasswordChar = '*'
                    TabOrder = 2
                  end
                  object edProxyPorta: TSpinEdit
                    Left = 192
                    Top = 20
                    Width = 99
                    Height = 24
                    Anchors = [akTop, akRight]
                    MaxValue = 999999
                    MinValue = 0
                    TabOrder = 3
                    Value = 0
                  end
                end
              end
              object gbLog: TGroupBox
                Left = 304
                Top = 0
                Width = 304
                Height = 133
                Align = alLeft
                Caption = 'Log'
                TabOrder = 1
                object pnLog: TPanel
                  Left = 2
                  Top = 17
                  Width = 300
                  Height = 114
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbLogArquivo: TLabel
                    Left = 20
                    Top = 5
                    Width = 40
                    Height = 15
                    Caption = 'Arquivo'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbLogNivel: TLabel
                    Left = 20
                    Top = 50
                    Width = 27
                    Height = 15
                    Caption = 'N'#237'vel'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btLogArquivo: TSpeedButton
                    Left = 228
                    Top = 20
                    Width = 24
                    Height = 23
                    Hint = 'Abrir Arquivo de Log'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -12
                    Font.Name = 'MS Sans Serif'
                    Font.Style = []
                    ParentFont = False
                    ParentShowHint = False
                    ShowHint = True
                  end
                  object edLogArquivo: TEdit
                    Left = 20
                    Top = 20
                    Width = 208
                    Height = 23
                    TabOrder = 0
                  end
                  object cbLogNivel: TComboBox
                    Left = 20
                    Top = 65
                    Width = 232
                    Height = 23
                    Style = csDropDownList
                    ItemIndex = 2
                    TabOrder = 1
                    Text = 'Normal'
                    Items.Strings = (
                      'Nenhum'
                      'Baixo'
                      'Normal'
                      'Alto'
                      'Muito Alto')
                  end
                end
              end
            end
            object pnConfig1: TPanel
              Left = 1
              Top = 1
              Width = 1012
              Height = 80
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object gbPSP: TGroupBox
                Left = 0
                Top = 0
                Width = 304
                Height = 80
                Align = alLeft
                Caption = 'PSP Matera'
                TabOrder = 0
                object pnPSP: TPanel
                  Left = 2
                  Top = 17
                  Width = 300
                  Height = 61
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbAmbiente: TLabel
                    Left = 14
                    Top = 5
                    Width = 52
                    Height = 15
                    Caption = 'Ambiente'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbTimeout: TLabel
                    Left = 192
                    Top = 5
                    Width = 45
                    Height = 15
                    Caption = 'Timeout'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object cbAmbiente: TComboBox
                    Left = 14
                    Top = 20
                    Width = 164
                    Height = 23
                    Style = csDropDownList
                    TabOrder = 0
                    OnChange = cbAmbienteChange
                  end
                  object edTimeout: TSpinEdit
                    Left = 192
                    Top = 20
                    Width = 99
                    Height = 24
                    Increment = 10
                    MaxValue = 999999
                    MinValue = 0
                    TabOrder = 1
                    Value = 0
                  end
                end
              end
              object gbCobranca: TGroupBox
                Left = 304
                Top = 0
                Width = 304
                Height = 80
                Align = alLeft
                Caption = 'Cobran'#231'a'
                TabOrder = 1
                object pnCobranca: TPanel
                  Left = 2
                  Top = 17
                  Width = 300
                  Height = 61
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbExpiracao: TLabel
                    Left = 24
                    Top = 5
                    Width = 54
                    Height = 15
                    Caption = 'Expira'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object seCobrancaExpiracao: TSpinEdit
                    Left = 24
                    Top = 20
                    Width = 232
                    Height = 24
                    Increment = 10
                    MaxValue = 999999
                    MinValue = 0
                    TabOrder = 0
                    Value = 3600
                  end
                end
              end
            end
          end
        end
        object tsMatera: TTabSheet
          Caption = 'PSP Matera'
          ImageIndex = 34
          object pnPSPMatera: TPanel
            Left = 0
            Top = 0
            Width = 1014
            Height = 602
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object gbMediator: TGroupBox
              Left = 0
              Top = 0
              Width = 1014
              Height = 304
              Align = alTop
              Caption = 'Configura'#231#245'es Mediator'
              TabOrder = 0
              object lbPSPClientID: TLabel
                Left = 21
                Top = 27
                Width = 47
                Height = 15
                Caption = 'Client ID'
                Color = clBtnFace
                ParentColor = False
              end
              object lbPSPClientSecret: TLabel
                Left = 21
                Top = 80
                Width = 70
                Height = 15
                Caption = 'Client Secret'
                Color = clBtnFace
                ParentColor = False
              end
              object lbErroCertificado: TLabel
                Left = 21
                Top = 251
                Width = 92
                Height = 15
                Caption = 'lbErroCertificado'
                Color = clBtnFace
                ParentColor = False
              end
              object lbArqCertificado: TLabel
                Left = 21
                Top = 210
                Width = 102
                Height = 15
                Caption = 'Arquivo Certificado'
                Color = clBtnFace
                ParentColor = False
              end
              object imErroCertificado: TImage
                Left = 149
                Top = 209
                Width = 16
                Height = 16
                Visible = False
              end
              object btAcharArqCertificado: TSpeedButton
                Left = 598
                Top = 225
                Width = 24
                Height = 23
                Flat = True
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                ParentShowHint = False
                ShowHint = True
                OnClick = btAcharArqCertificadoClick
              end
              object btAcharChavePrivada: TSpeedButton
                Left = 598
                Top = 150
                Width = 24
                Height = 23
                Flat = True
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                ParentShowHint = False
                ShowHint = True
                OnClick = btAcharChavePrivadaClick
              end
              object lbArqChavePrivada: TLabel
                Left = 21
                Top = 135
                Width = 122
                Height = 15
                Caption = 'Arquivo Chave Privada'
                Color = clBtnFace
                ParentColor = False
              end
              object lbErroChavePrivada: TLabel
                Left = 21
                Top = 175
                Width = 109
                Height = 15
                Caption = 'lbErroChavePrivada'
                Color = clBtnFace
                ParentColor = False
              end
              object imErroChavePrivada: TImage
                Left = 149
                Top = 134
                Width = 16
                Height = 16
                Visible = False
              end
              object lbPSPSecretKey: TLabel
                Left = 320
                Top = 80
                Width = 58
                Height = 15
                Caption = 'Secret Key'
                Color = clBtnFace
                ParentColor = False
              end
              object edPSPClientID: TEdit
                Left = 21
                Top = 41
                Width = 287
                Height = 23
                PasswordChar = '*'
                TabOrder = 0
              end
              object edPSPClientSecret: TEdit
                Left = 21
                Top = 93
                Width = 287
                Height = 23
                PasswordChar = '*'
                TabOrder = 1
              end
              object edArqCertificado: TEdit
                Left = 21
                Top = 225
                Width = 576
                Height = 23
                AutoSize = False
                TabOrder = 2
                OnExit = edArqCertificadoExit
              end
              object edArqChavePrivada: TEdit
                Left = 21
                Top = 150
                Width = 576
                Height = 23
                AutoSize = False
                TabOrder = 3
                OnExit = edArqChavePrivadaExit
              end
              object edPSPSecretKey: TEdit
                Left = 320
                Top = 95
                Width = 302
                Height = 23
                PasswordChar = '*'
                TabOrder = 4
              end
            end
            object gbAccount: TGroupBox
              Left = 0
              Top = 304
              Width = 1014
              Height = 298
              Align = alClient
              Caption = 'Configura'#231#245'es Contas'
              TabOrder = 1
              ExplicitTop = 302
              object lbAccountId: TLabel
                Left = 21
                Top = 25
                Width = 56
                Height = 15
                Caption = 'Account Id'
                Color = clBtnFace
                ParentColor = False
              end
              object lbChavePIX: TLabel
                Left = 21
                Top = 72
                Width = 56
                Height = 15
                Caption = 'Chave PIX'
                Color = clBtnFace
                ParentColor = False
              end
              object lbMediatorFee: TLabel
                Left = 21
                Top = 125
                Width = 141
                Height = 15
                Caption = 'Mediator Fee QRCode R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFee: TLabel
                Left = 185
                Top = 125
                Width = 115
                Height = 15
                Caption = 'Tipo de Mediator Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object lbMediatorFeeEstorno: TLabel
                Left = 21
                Top = 178
                Width = 136
                Height = 15
                Caption = 'Mediator Fee Estorno R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFeeEstorno: TLabel
                Left = 185
                Top = 178
                Width = 118
                Height = 15
                Caption = 'Tipo de Mediator  Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object cbAccountId: TComboBox
                Left = 21
                Top = 41
                Width = 601
                Height = 23
                TabOrder = 0
                OnSelect = cbAccountIdSelect
              end
              object cbChavePIX: TComboBox
                Left = 21
                Top = 88
                Width = 601
                Height = 23
                TabOrder = 1
              end
              object edMediatorFee: TEdit
                Left = 21
                Top = 141
                Width = 145
                Height = 23
                TabOrder = 2
              end
              object cbTipoMediatorFee: TComboBox
                Left = 185
                Top = 141
                Width = 145
                Height = 23
                Style = csDropDownList
                ItemIndex = 0
                TabOrder = 3
                Text = 'Reais'
                OnChange = cbTipoMediatorFeeChange
                Items.Strings = (
                  'Reais'
                  'Porcentagem')
              end
              object edMediatorFeeEstorno: TEdit
                Left = 21
                Top = 194
                Width = 145
                Height = 23
                TabOrder = 4
              end
              object cbTipoMediatorFeeEstorno: TComboBox
                Left = 185
                Top = 194
                Width = 145
                Height = 23
                Style = csDropDownList
                ItemIndex = 0
                TabOrder = 5
                Text = 'Reais'
                OnChange = cbTipoMediatorFeeEstornoChange
                Items.Strings = (
                  'Reais'
                  'Porcentagem')
              end
            end
          end
        end
      end
    end
  end
  object ImageList1: TImageList
    Left = 736
    Top = 24
  end
  object ACBrPixCD1: TACBrPixCD
    Recebedor.CodCategoriaComerciante = 0
    Left = 736
    Top = 88
  end
  object ACBrPSPMatera1: TACBrPSPMatera
    Scopes = [scCobWrite, scCobRead, scPixWrite, scPixRead]
    Left = 832
    Top = 88
  end
  object ACBrCEP1: TACBrCEP
    ProxyPort = '8080'
    WebService = wsCorreios
    PesquisarIBGE = True
    Left = 768
    Top = 24
  end
  object OpenDialog1: TOpenDialog
    Left = 800
    Top = 24
  end
  object ACBrOpenSSLUtils1: TACBrOpenSSLUtils
    Left = 832
    Top = 24
  end
  object tmConsultarPagto: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmConsultarPagtoTimer
    Left = 933
    Top = 32
  end
  object tmConsultarEstorno: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmConsultarEstornoTimer
    Left = 933
    Top = 88
  end
end
