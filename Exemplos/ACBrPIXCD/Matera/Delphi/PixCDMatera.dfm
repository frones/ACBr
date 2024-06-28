object frPixCDMatera: TfrPixCDMatera
  Left = 543
  Top = 128
  Width = 1058
  Height = 815
  Caption = 'ACBrPIXCD Matera'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbUrlPIX: TLabel
    Left = 0
    Top = 756
    Width = 1042
    Height = 20
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
  end
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 1042
    Height = 756
    ActivePage = tsContaEChaves
    Align = alClient
    Images = ImageList1
    TabHeight = 30
    TabOrder = 0
    TabWidth = 250
    OnChange = pgPrincipalChange
    object tsFluxoPagto: TTabSheet
      Caption = 'Fluxo de Pagamento'
      object pnFluxoBackground: TPanel
        Left = 0
        Top = 0
        Width = 1034
        Height = 716
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object pnFluxoPagto: TPanel
          Left = 44
          Top = 17
          Width = 938
          Height = 623
          BevelOuter = bvNone
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
              ParentFont = False
              TabOrder = 0
            end
          end
          object pnFluxoRodape: TPanel
            Left = 0
            Top = 227
            Width = 938
            Height = 349
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object pnFluxoQRCode: TPanel
              Left = 672
              Top = 0
              Width = 266
              Height = 235
              Align = alClient
              BevelOuter = bvNone
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
                TabOrder = 1
              end
              object pnFluxoBotoesPrincipais: TPanel
                Left = 24
                Top = 0
                Width = 312
                Height = 235
                Align = alClient
                BevelOuter = bvNone
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
                  TabOrder = 0
                  object edFluxoValor: TEdit
                    Left = 34
                    Top = 15
                    Width = 234
                    Height = 48
                    AutoSize = False
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -33
                    Font.Name = 'Tahoma'
                    Font.Style = [fsBold]
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
              TabOrder = 3
            end
            object pnFluxoCopiaECola: TPanel
              Left = 0
              Top = 297
              Width = 938
              Height = 52
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 4
              Visible = False
              object lbFluxoCopiaECola: TLabel
                Left = 26
                Top = 0
                Width = 92
                Height = 15
                Caption = 'PIX Copia e Cola'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentColor = False
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
              TabOrder = 5
              Visible = False
              object lbFluxoCopiaECola1: TLabel
                Left = 26
                Top = 0
                Width = 76
                Height = 15
                Caption = 'transactionID'
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentColor = False
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
            TabOrder = 2
          end
          object pnFluxoDiv3: TPanel
            Left = 0
            Top = 212
            Width = 938
            Height = 15
            Align = alTop
            BevelOuter = bvNone
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
              TabOrder = 0
              object lbFluxoClienteNome: TLabel
                Left = 312
                Top = 13
                Width = 29
                Height = 15
                Caption = 'Valor'
                Color = clBtnFace
                ParentColor = False
              end
              object lbFluxoClienteDoc: TLabel
                Left = 24
                Top = 13
                Width = 21
                Height = 15
                Caption = 'Info'
                Color = clBtnFace
                ParentColor = False
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
            TabOrder = 5
          end
          object pnSiteEfetuarPagto: TPanel
            Left = 0
            Top = 576
            Width = 938
            Height = 52
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 6
            Visible = False
            object lbSiteEfetuarPagto: TLabel
              Left = 0
              Top = 0
              Width = 437
              Height = 24
              Align = alClient
              Alignment = taCenter
              Caption = 'CLIQUE AQUI PARA EFETUAR O PAGAMENTO'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHighlight
              Font.Height = -20
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
              Layout = tlCenter
              OnClick = lbSiteEfetuarPagtoClick
              OnMouseEnter = lbSiteEfetuarPagtoMouseEnter
              OnMouseLeave = lbSiteEfetuarPagtoMouseLeave
            end
          end
        end
      end
    end
    object tsTestes: TTabSheet
      Caption = 'Opera'#231#245'es'
      ImageIndex = 14
      object Splitter2: TSplitter
        Left = 674
        Top = 0
        Width = 5
        Height = 716
        Align = alRight
      end
      object pcTestes: TPageControl
        Left = 0
        Top = 0
        Width = 674
        Height = 716
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
            Width = 666
            Height = 112
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lbQRCodeExternalID: TLabel
              Left = 184
              Top = 20
              Width = 52
              Height = 13
              Caption = 'External ID'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeValor: TLabel
              Left = 512
              Top = 20
              Width = 24
              Height = 13
              Caption = 'Valor'
              Color = clBtnFace
              ParentColor = False
            end
            object btQRCodeGerarExternalID: TSpeedButton
              Left = 472
              Top = 35
              Width = 24
              Height = 23
              Flat = True
              OnClick = btQRCodeGerarExternalIDClick
            end
            object lbQRCodeTipoCobranca: TLabel
              Left = 16
              Top = 20
              Width = 85
              Height = 13
              Caption = 'Tipo de Cobran'#231'a'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeMediatorFee: TLabel
              Left = 184
              Top = 65
              Width = 79
              Height = 13
              Caption = 'Mediator Fee R$'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeTipoMediatorFee: TLabel
              Left = 16
              Top = 65
              Width = 104
              Height = 13
              Caption = 'Tipo de Mediator  Fee'
              Color = clBtnFace
              ParentColor = False
            end
            object edQRCodeExternalID: TEdit
              Left = 184
              Top = 35
              Width = 288
              Height = 21
              TabOrder = 0
            end
            object edQRCodeValor: TEdit
              Left = 512
              Top = 35
              Width = 125
              Height = 21
              TabOrder = 1
            end
            object cbQRCodeTipoCobranca: TComboBox
              Left = 16
              Top = 35
              Width = 151
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 2
              Text = 'Normal'
              OnChange = cbQRCodeTipoCobrancaChange
              Items.Strings = (
                'Normal'
                'Com Vencimento')
            end
            object edQRCodeMediatorFee: TEdit
              Left = 184
              Top = 81
              Width = 288
              Height = 21
              TabOrder = 3
            end
            object cbQRCodeTipoMediatorFee: TComboBox
              Left = 16
              Top = 81
              Width = 151
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
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
            Width = 666
            Height = 183
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            Visible = False
            object btQRCodeCriarCopiaECola: TSpeedButton
              Left = 605
              Top = 23
              Width = 26
              Height = 26
              OnClick = btQRCodeCriarCopiaEColaClick
            end
            object lbCobCopiaECola: TLabel
              Left = 240
              Top = 10
              Width = 60
              Height = 13
              Caption = 'Copia e Cola'
              Color = clBtnFace
              ParentColor = False
            end
            object lbDisclaimerCallBack: TLabel
              Left = 240
              Top = 64
              Width = 372
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
              Height = 21
              TabOrder = 0
            end
            object pnGerarQRCodeImg: TPanel
              Left = 0
              Top = 0
              Width = 232
              Height = 183
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
            Width = 666
            Height = 122
            Align = alTop
            Caption = 'Informa'#231#245'es Adicionais'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            object pnQRCodeInfoAdicionais: TPanel
              Left = 2
              Top = 15
              Width = 662
              Height = 105
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbQRCodeADname: TLabel
                Left = 14
                Top = 3
                Width = 28
                Height = 13
                Caption = 'Nome'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodeADcontent: TLabel
                Left = 256
                Top = 3
                Width = 46
                Height = 13
                Caption = 'Conte'#250'do'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodeCallBack: TLabel
                Left = 14
                Top = 50
                Width = 80
                Height = 13
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
                Width = 92
                Height = 13
                Caption = 'Recipient Comment'
                Color = clBtnFace
                ParentColor = False
              end
              object edQRCodeADname: TEdit
                Left = 14
                Top = 18
                Width = 218
                Height = 21
                TabOrder = 0
              end
              object edQRCodeADcontent: TEdit
                Left = 256
                Top = 18
                Width = 244
                Height = 21
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
                Height = 21
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
                Height = 21
                TabOrder = 4
              end
            end
          end
          object gbQRCodeDetalhesCobranca: TGroupBox
            Left = 0
            Top = 234
            Width = 666
            Height = 225
            Align = alTop
            Caption = 'Detalhes da Cobran'#231'a'
            TabOrder = 3
            Visible = False
            object pnQRCodeDetalhesCobranca: TPanel
              Left = 2
              Top = 15
              Width = 662
              Height = 208
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbQRCodedueDate: TLabel
                Left = 185
                Top = 103
                Width = 41
                Height = 13
                Caption = 'dueDate'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodePayercpfcnpj: TLabel
                Left = 14
                Top = 3
                Width = 58
                Height = 13
                Caption = 'CPF / CNPJ'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodePayerName: TLabel
                Left = 185
                Top = 3
                Width = 28
                Height = 13
                Caption = 'Name'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayerstreet: TLabel
                Left = 14
                Top = 53
                Width = 54
                Height = 13
                Caption = 'Logradouro'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayercity: TLabel
                Left = 185
                Top = 53
                Width = 33
                Height = 13
                Caption = 'Cidade'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayeruf: TLabel
                Left = 330
                Top = 53
                Width = 14
                Height = 13
                Caption = 'UF'
                Color = clBtnFace
                ParentColor = False
              end
              object lbQRCodepayerCEP: TLabel
                Left = 14
                Top = 103
                Width = 21
                Height = 13
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
                Left = 383
                Top = 0
                Width = 120
                Height = 208
                Align = alRight
                Caption = 'discounts*'
                TabOrder = 1
                object pnQRCodeDiscount: TPanel
                  Left = 2
                  Top = 15
                  Width = 116
                  Height = 191
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbQRCodediscountsvaluePerc: TLabel
                    Left = 10
                    Top = 55
                    Width = 48
                    Height = 13
                    Caption = 'valuePerc'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsmodality: TLabel
                    Left = 10
                    Top = 3
                    Width = 38
                    Height = 13
                    Caption = 'modality'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsdate: TLabel
                    Left = 10
                    Top = 108
                    Width = 21
                    Height = 13
                    Caption = 'date'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbQRCodediscountsAviso: TLabel
                    Left = 11
                    Top = 149
                    Width = 61
                    Height = 26
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
                    Height = 21
                    TabOrder = 0
                    Text = '5,00'
                  end
                  object edQRCodediscountsmodality: TEdit
                    Left = 10
                    Top = 18
                    Width = 83
                    Height = 21
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
                Height = 21
                TabOrder = 2
                OnKeyPress = edOnlyNumbersKeyPress
              end
              object edQRCodePayerName: TEdit
                Left = 185
                Top = 18
                Width = 184
                Height = 21
                TabOrder = 3
              end
              object edQRCodepayerstreet: TEdit
                Left = 14
                Top = 68
                Width = 154
                Height = 21
                TabOrder = 4
              end
              object edQRCodepayercity: TEdit
                Left = 185
                Top = 68
                Width = 135
                Height = 21
                TabOrder = 5
              end
              object edQRCodepayeruf: TEdit
                Left = 330
                Top = 68
                Width = 39
                Height = 21
                TabOrder = 6
              end
              object edQRCodepayerCEP: TEdit
                Left = 14
                Top = 118
                Width = 154
                Height = 21
                TabOrder = 7
                OnChange = edContaCriarCEPChange
                OnKeyPress = edOnlyNumbersKeyPress
              end
              object pnQRCodeValues: TPanel
                Left = 503
                Top = 0
                Width = 159
                Height = 208
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
                    Top = 15
                    Width = 155
                    Height = 52
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodefinesvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodefinesmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 38
                      Height = 13
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodefinesvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 21
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodefinesmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 21
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
                    Top = 15
                    Width = 155
                    Height = 51
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodereductionvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodereductionmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 38
                      Height = 13
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodereductionvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 21
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodereductionmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 21
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
                    Top = 15
                    Width = 155
                    Height = 51
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbQRCodeinterestsvaluePerc: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbQRCodeinterestsmodality: TLabel
                      Left = 85
                      Top = 3
                      Width = 38
                      Height = 13
                      Caption = 'modality'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object edQRCodeinterestsvaluePerc: TEdit
                      Left = 10
                      Top = 18
                      Width = 64
                      Height = 21
                      TabOrder = 0
                      Text = '5,00'
                    end
                    object edQRCodeinterestsmodality: TEdit
                      Left = 85
                      Top = 18
                      Width = 61
                      Height = 21
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
            Top = 642
            Width = 666
            Height = 34
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 4
            object btQRCodeCriar: TBitBtn
              Left = 507
              Top = 5
              Width = 130
              Height = 26
              Caption = 'Criar'
              TabOrder = 0
              OnClick = btQRCodeCriarClick
            end
            object btQRCodeCriarLimparDados: TBitBtn
              Left = 383
              Top = 5
              Width = 119
              Height = 26
              Caption = 'Limpar Dados'
              TabOrder = 1
              OnClick = btQRCodeCriarLimparDadosClick
            end
            object btQRCodeCriarPreencherDados: TBitBtn
              Left = 191
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
            Width = 666
            Height = 676
            ActivePage = tsConsultaTxID
            Align = alClient
            TabOrder = 1
            object tsConsultaTxID: TTabSheet
              Caption = 'Consulta Transa'#231#227'o'
              object gbConsultaTxID: TGroupBox
                Left = 0
                Top = 0
                Width = 658
                Height = 95
                Align = alTop
                Caption = 'Consulta de Transa'#231#245'es por transactionId'
                TabOrder = 0
                object pnConsultaTxID: TPanel
                  Left = 2
                  Top = 15
                  Width = 654
                  Height = 78
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    654
                    78)
                  object lbConsultarCobTransactionID: TLabel
                    Left = 25
                    Top = 15
                    Width = 70
                    Height = 13
                    Caption = 'Transaction ID'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edConsultarCobTransactionID: TEdit
                    Left = 25
                    Top = 30
                    Width = 439
                    Height = 21
                    TabOrder = 0
                  end
                  object btConsultarCob: TBitBtn
                    Left = 503
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
                Width = 658
                Height = 216
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 0
                object gbConsultaExtratoEC: TGroupBox
                  Left = 0
                  Top = 112
                  Width = 658
                  Height = 104
                  Align = alClient
                  Caption = 'Consulta de extrato - EC'
                  TabOrder = 0
                  object pnConsultaExtratoEC: TPanel
                    Left = 2
                    Top = 15
                    Width = 654
                    Height = 87
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    DesignSize = (
                      654
                      87)
                    object lbConsultaStart1: TLabel
                      Left = 21
                      Top = 15
                      Width = 22
                      Height = 13
                      Caption = 'Start'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbconsultaEnding1: TLabel
                      Left = 165
                      Top = 15
                      Width = 33
                      Height = 13
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
                      Left = 503
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
                  Width = 658
                  Height = 112
                  Align = alTop
                  Caption = 'Consulta de saldo - EC'
                  TabOrder = 1
                  object pnConsultaSaldoEC: TPanel
                    Left = 2
                    Top = 15
                    Width = 654
                    Height = 95
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    DesignSize = (
                      654
                      95)
                    object lbAccavaliable: TLabel
                      Left = 487
                      Top = 23
                      Width = 120
                      Height = 37
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
                      Width = 86
                      Height = 25
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
                      Left = 21
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
                Width = 658
                Height = 432
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object sgAccExtrato: TStringGrid
                  Left = 0
                  Top = 0
                  Width = 658
                  Height = 432
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
                Width = 658
                Height = 140
                Align = alTop
                Caption = 'Consulta de extrato - Integrador (Mediator)'
                TabOrder = 0
                object pnConsultaExtratoIntegrador: TPanel
                  Left = 2
                  Top = 15
                  Width = 654
                  Height = 123
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    654
                    123)
                  object lbConsultarEXIntegradorAccountID: TLabel
                    Left = 25
                    Top = 15
                    Width = 96
                    Height = 13
                    Caption = 'Mediator Account Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbConsultaStart: TLabel
                    Left = 25
                    Top = 64
                    Width = 22
                    Height = 13
                    Caption = 'Start'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbconsultaEnding: TLabel
                    Left = 169
                    Top = 64
                    Width = 33
                    Height = 13
                    Caption = 'Ending'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edConsultarEXIntegradorAccountID: TEdit
                    Left = 25
                    Top = 30
                    Width = 455
                    Height = 21
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
                    Left = 503
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
                Width = 658
                Height = 144
                Align = alTop
                Caption = 'Consulta de saldo - Integrador (Mediator)'
                TabOrder = 1
                object pnConsultaSaldoIntegrador: TPanel
                  Left = 2
                  Top = 15
                  Width = 654
                  Height = 127
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    654
                    127)
                  object lbConsultarSaldoIntegradorAccountID: TLabel
                    Left = 25
                    Top = 15
                    Width = 96
                    Height = 13
                    Caption = 'Mediator Account Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbMediatoravaliableStr: TLabel
                    Left = 25
                    Top = 80
                    Width = 83
                    Height = 25
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
                    Left = 179
                    Top = 66
                    Width = 120
                    Height = 37
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
                    Height = 21
                    TabOrder = 0
                  end
                  object btConsultarSaldoMediator: TBitBtn
                    Left = 503
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
                Width = 658
                Height = 364
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 2
                object sgMediatorExtrato: TStringGrid
                  Left = 0
                  Top = 0
                  Width = 658
                  Height = 364
                  Align = alClient
                  ColCount = 7
                  FixedCols = 0
                  TabOrder = 0
                  ColWidths = (
                    64
                    131
                    269
                    95
                    201
                    89
                    64)
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
            Width = 666
            Height = 612
            Align = alClient
            Caption = 'Efetuar Devolu'#231#227'o'
            TabOrder = 0
            object pnDevolucao: TPanel
              Left = 2
              Top = 15
              Width = 662
              Height = 595
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbDevolucaoCobTransactionID: TLabel
                Left = 15
                Top = 25
                Width = 70
                Height = 13
                Caption = 'Transaction ID'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoValor: TLabel
                Left = 176
                Top = 73
                Width = 24
                Height = 13
                Caption = 'Valor'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoReasonCode: TLabel
                Left = 15
                Top = 73
                Width = 101
                Height = 13
                Caption = 'Codigo da devolu'#231#227'o'
                Color = clBtnFace
                ParentColor = False
              end
              object lbDevolucaoExternalID: TLabel
                Left = 337
                Top = 25
                Width = 52
                Height = 13
                Caption = 'External ID'
                Color = clBtnFace
                ParentColor = False
              end
              object btDevolucaoExternalID: TSpeedButton
                Left = 621
                Top = 40
                Width = 24
                Height = 23
                Flat = True
                OnClick = btDevolucaoExternalIDClick
              end
              object lbMediatorFeeDevolucao: TLabel
                Left = 337
                Top = 73
                Width = 79
                Height = 13
                Caption = 'Mediator Fee R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFeeDevolucao: TLabel
                Left = 498
                Top = 73
                Width = 104
                Height = 13
                Caption = 'Tipo de Mediator  Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object edDevolucaoCobTransactionID: TEdit
                Left = 15
                Top = 40
                Width = 308
                Height = 21
                TabOrder = 0
              end
              object edDevolucaoValor: TEdit
                Left = 176
                Top = 89
                Width = 147
                Height = 21
                TabOrder = 1
              end
              object edDevolucaoExternalID: TEdit
                Left = 337
                Top = 39
                Width = 281
                Height = 21
                TabOrder = 2
              end
              object pnDevolucaoBotoes: TPanel
                Left = 0
                Top = 547
                Width = 662
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
                Height = 21
                TabOrder = 4
              end
              object cbTipoMediatorFeeDevolucao: TComboBox
                Left = 498
                Top = 89
                Width = 147
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
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
                Height = 21
                ItemHeight = 13
                TabOrder = 6
                OnDropDown = cbDevolucaoReasonCodeDropDown
              end
            end
          end
          object pnMotivosDevolucao: TPanel
            Left = 0
            Top = 0
            Width = 666
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
                Top = 15
                Width = 630
                Height = 116
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object lbConsultarCobAccountID2: TLabel
                  Left = 15
                  Top = 10
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbConsultarCobTransactionID3: TLabel
                  Left = 323
                  Top = 11
                  Width = 81
                  Height = 13
                  Caption = 'Alias Destinat'#225'rio'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edRetiradaConsultaAliasAccountID: TEdit
                  Left = 15
                  Top = 25
                  Width = 290
                  Height = 21
                  TabOrder = 0
                end
                object edRetiradaConsultaaliasAliasDestinatario: TEdit
                  Left = 323
                  Top = 25
                  Width = 290
                  Height = 21
                  TabOrder = 1
                end
                object btConsultarAliasRetirada: TBitBtn
                  Left = 483
                  Top = 69
                  Width = 130
                  Height = 26
                  Caption = 'Consultar alias'
                  TabOrder = 2
                  OnClick = btConsultarAliasRetiradaClick
                end
                object btRetiradaConsultaPreencherDados: TBitBtn
                  Left = 162
                  Top = 69
                  Width = 186
                  Height = 26
                  Caption = 'Preencher(Dados Fict'#237'cios)'
                  TabOrder = 3
                  OnClick = btRetiradaConsultaPreencherDadosClick
                end
                object btRetiradaConsultaLimparDados: TBitBtn
                  Left = 357
                  Top = 69
                  Width = 119
                  Height = 26
                  Caption = 'Limpar Dados'
                  TabOrder = 4
                  OnClick = btRetiradaConsultaLimparDadosClick
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
                Top = 129
                Width = 630
                Height = 145
                Align = alTop
                Caption = 'PIX'
                TabOrder = 0
                object pnPIXRetirada: TPanel
                  Left = 2
                  Top = 15
                  Width = 626
                  Height = 128
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbRetiradaAliasDestinatario: TLabel
                    Left = 15
                    Top = 10
                    Width = 81
                    Height = 13
                    Caption = 'Alias Destinat'#225'rio'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaendToEndId: TLabel
                    Left = 323
                    Top = 10
                    Width = 71
                    Height = 13
                    Caption = 'End To End ID'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaPSPId: TLabel
                    Left = 15
                    Top = 65
                    Width = 33
                    Height = 13
                    Caption = 'PSP Id'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountdestinationBranch: TLabel
                    Left = 153
                    Top = 65
                    Width = 133
                    Height = 13
                    Caption = 'Account Destination Branch'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountDestinationAccount: TLabel
                    Left = 464
                    Top = 65
                    Width = 139
                    Height = 13
                    Caption = 'Account Destination Account'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTaxID: TLabel
                    Left = 323
                    Top = 65
                    Width = 27
                    Height = 13
                    Caption = 'TaxId'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRetiradaAliasDestinatario: TEdit
                    Left = 15
                    Top = 25
                    Width = 290
                    Height = 21
                    TabOrder = 0
                  end
                  object edRetiradaendToEndId: TEdit
                    Left = 323
                    Top = 25
                    Width = 282
                    Height = 21
                    TabOrder = 1
                  end
                  object edRetiradaPSPId: TEdit
                    Left = 15
                    Top = 80
                    Width = 125
                    Height = 21
                    TabOrder = 2
                  end
                  object edRetiradaAccountdestinationBranch: TEdit
                    Left = 153
                    Top = 80
                    Width = 152
                    Height = 21
                    TabOrder = 3
                  end
                  object edRetiradaAccountDestinationAccount: TEdit
                    Left = 464
                    Top = 80
                    Width = 141
                    Height = 21
                    TabOrder = 4
                  end
                  object edRetiradaTaxID: TEdit
                    Left = 323
                    Top = 80
                    Width = 128
                    Height = 21
                    TabOrder = 5
                  end
                end
              end
              object pnlRetirada: TPanel
                Left = 2
                Top = 15
                Width = 630
                Height = 114
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 1
                object lbRetiradaExternalID: TLabel
                  Left = 17
                  Top = 10
                  Width = 52
                  Height = 13
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
                  Width = 79
                  Height = 13
                  Caption = 'Tipo de Retirada'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbRetiradaMediatorFee: TLabel
                  Left = 155
                  Top = 65
                  Width = 62
                  Height = 13
                  Caption = 'Mediator Fee'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbRetiradaValor: TLabel
                  Left = 17
                  Top = 65
                  Width = 24
                  Height = 13
                  Caption = 'Valor'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edRetiradaExternalID: TEdit
                  Left = 17
                  Top = 25
                  Width = 266
                  Height = 21
                  TabOrder = 0
                end
                object cbRetiradaTipoRetirada: TComboBox
                  Left = 325
                  Top = 25
                  Width = 282
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
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
                  Height = 21
                  TabOrder = 2
                end
                object edRetiradaValor: TEdit
                  Left = 17
                  Top = 79
                  Width = 127
                  Height = 21
                  TabOrder = 3
                end
              end
              object pnRetiradaBotoes: TPanel
                Left = 2
                Top = 419
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
                object btRetiradaLimparDados: TBitBtn
                  Left = 355
                  Top = 17
                  Width = 119
                  Height = 26
                  Caption = 'Limpar Dados'
                  TabOrder = 1
                  OnClick = btRetiradaLimparDadosClick
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
                Top = 274
                Width = 630
                Height = 145
                Align = alTop
                Caption = 'TED'
                TabOrder = 3
                Visible = False
                object pnTEDRetirada: TPanel
                  Left = 2
                  Top = 15
                  Width = 626
                  Height = 128
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbRetiradaTEDBankDestination: TLabel
                    Left = 15
                    Top = 10
                    Width = 81
                    Height = 13
                    Caption = 'Bank Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDName: TLabel
                    Left = 323
                    Top = 10
                    Width = 28
                    Height = 13
                    Caption = 'Name'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaPersonType: TLabel
                    Left = 15
                    Top = 65
                    Width = 60
                    Height = 13
                    Caption = 'Person Type'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDTaxID: TLabel
                    Left = 301
                    Top = 65
                    Width = 27
                    Height = 13
                    Caption = 'TaxId'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDBranchDestination: TLabel
                    Left = 383
                    Top = 65
                    Width = 90
                    Height = 13
                    Caption = 'Branch Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaTEDAccountDestination: TLabel
                    Left = 497
                    Top = 65
                    Width = 96
                    Height = 13
                    Caption = 'Account Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbRetiradaAccountTypeDestination: TLabel
                    Left = 153
                    Top = 65
                    Width = 123
                    Height = 13
                    Caption = 'Account Type Destination'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edRetiradaTEDBankDestination: TEdit
                    Left = 15
                    Top = 25
                    Width = 290
                    Height = 21
                    TabOrder = 0
                  end
                  object edRetiradaTEDName: TEdit
                    Left = 323
                    Top = 25
                    Width = 282
                    Height = 21
                    TabOrder = 1
                  end
                  object edRetiradaTEDTaxID: TEdit
                    Left = 301
                    Top = 80
                    Width = 70
                    Height = 21
                    TabOrder = 2
                  end
                  object edRetiradaTEDBranchDestination: TEdit
                    Left = 383
                    Top = 80
                    Width = 100
                    Height = 21
                    TabOrder = 3
                  end
                  object edRetiradaTEDAccountDestination: TEdit
                    Left = 497
                    Top = 80
                    Width = 108
                    Height = 21
                    TabOrder = 4
                  end
                  object cbRetiradaPersonType: TComboBox
                    Left = 15
                    Top = 80
                    Width = 125
                    Height = 21
                    ItemHeight = 13
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
                    Height = 21
                    ItemHeight = 13
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
        Left = 679
        Top = 0
        Width = 355
        Height = 716
        Align = alRight
        TabOrder = 1
        object lbLog1: TLabel
          Left = 1
          Top = 1
          Width = 353
          Height = 13
          Align = alTop
          Caption = 'Log das Requisi'#231#245'es:'
          Color = clBtnFace
          ParentColor = False
        end
        object mmLogOperacoes: TMemo
          Left = 1
          Top = 14
          Width = 353
          Height = 669
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape1: TPanel
          Left = 1
          Top = 683
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
        Left = 1029
        Top = 0
        Width = 5
        Height = 716
        Align = alRight
      end
      object pgContasEChaves: TPageControl
        Left = 0
        Top = 0
        Width = 674
        Height = 716
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
            Width = 666
            Height = 631
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbContaCriarExternalID: TLabel
              Left = 20
              Top = 25
              Width = 81
              Height = 13
              Caption = 'External Identifier'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaCriarExternalID: TSpeedButton
              Left = 343
              Top = 40
              Width = 24
              Height = 23
              Flat = True
              OnClick = btContaCriarExternalIDClick
            end
            object lbContaCriarNomeCliente: TLabel
              Left = 20
              Top = 75
              Width = 63
              Height = 13
              Caption = 'Nome Cliente'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriarCelular: TLabel
              Left = 271
              Top = 75
              Width = 32
              Height = 13
              Caption = 'Celular'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaCriar: TLabel
              Left = 381
              Top = 75
              Width = 28
              Height = 13
              Caption = 'E-mail'
              Color = clBtnFace
              ParentColor = False
            end
            object lbCNPJ: TLabel
              Left = 381
              Top = 25
              Width = 27
              Height = 13
              Caption = 'CNPJ'
              Color = clBtnFace
              ParentColor = False
            end
            object lbAvisoAberturaConta: TLabel
              Left = 18
              Top = 536
              Width = 592
              Height = 80
              Alignment = taCenter
              Caption = 
                'Atente-se de que antes da abertura de qualquer conta em produ'#231#227'o' +
                ', o representante '#13#10'da empresa precisa obrigatoriamente aceitar ' +
                'os Termos de Uso da Plataforma '#13#10'(documento desenvolvido e distr' +
                'ibuido pela Flagship)'#13#10'Caso o procedimento seja feito virtualmen' +
                'te '#233' necess'#225'rio capturar a geolocaliza'#231#227'o. '#13#10'Abaixo segue um exe' +
                'mplo de como armazenar as informa'#231#245'es'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = 3683321
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object gbContaCriarEndereco: TGroupBox
              Left = 20
              Top = 136
              Width = 622
              Height = 120
              Caption = 'Endere'#231'o'
              TabOrder = 0
              object pnContaCriarEndereco: TPanel
                Left = 2
                Top = 15
                Width = 618
                Height = 103
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object lbContaCriarCEP: TLabel
                  Left = 15
                  Top = 0
                  Width = 21
                  Height = 13
                  Caption = 'CEP'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarLogradouro: TLabel
                  Left = 198
                  Top = 0
                  Width = 54
                  Height = 13
                  Caption = 'Logradouro'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarNumero: TLabel
                  Left = 518
                  Top = 0
                  Width = 37
                  Height = 13
                  Caption = 'N'#250'mero'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarBairro: TLabel
                  Left = 198
                  Top = 50
                  Width = 27
                  Height = 13
                  Caption = 'Bairro'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarComplemento: TLabel
                  Left = 15
                  Top = 50
                  Width = 64
                  Height = 13
                  Caption = 'Complemento'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarCidade: TLabel
                  Left = 358
                  Top = 50
                  Width = 33
                  Height = 13
                  Caption = 'Cidade'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbContaCriarUF: TLabel
                  Left = 518
                  Top = 50
                  Width = 14
                  Height = 13
                  Caption = 'UF'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edContaCriarCEP: TEdit
                  Left = 15
                  Top = 15
                  Width = 168
                  Height = 21
                  TabOrder = 0
                  OnChange = edContaCriarCEPChange
                  OnKeyPress = edOnlyNumbersKeyPress
                end
                object edContaCriarLogradouro: TEdit
                  Left = 198
                  Top = 15
                  Width = 304
                  Height = 21
                  TabOrder = 1
                end
                object edContaCriarNumero: TEdit
                  Left = 519
                  Top = 15
                  Width = 85
                  Height = 21
                  TabOrder = 2
                end
                object edContaCriarBairro: TEdit
                  Left = 198
                  Top = 65
                  Width = 145
                  Height = 21
                  TabOrder = 3
                end
                object edContaCriarComplemento: TEdit
                  Left = 15
                  Top = 65
                  Width = 167
                  Height = 21
                  TabOrder = 4
                end
                object edContaCriarCidade: TEdit
                  Left = 358
                  Top = 65
                  Width = 144
                  Height = 21
                  TabOrder = 5
                end
                object edContaCriarUF: TEdit
                  Left = 518
                  Top = 65
                  Width = 86
                  Height = 21
                  TabOrder = 6
                end
              end
            end
            object edContaCriarExternalID: TEdit
              Left = 20
              Top = 40
              Width = 324
              Height = 21
              TabOrder = 1
            end
            object edContaCriarNomeCliente: TEdit
              Left = 20
              Top = 90
              Width = 231
              Height = 21
              TabOrder = 2
            end
            object edContaCriarCelular: TEdit
              Left = 264
              Top = 90
              Width = 103
              Height = 21
              TabOrder = 3
              OnKeyPress = edOnlyNumbersKeyPress
            end
            object edContaCriarEmail: TEdit
              Left = 381
              Top = 90
              Width = 261
              Height = 21
              TabOrder = 4
            end
            object edCNPJ: TEdit
              Left = 381
              Top = 40
              Width = 261
              Height = 21
              TabOrder = 5
            end
            object pcContaCriarDados: TPageControl
              Left = 20
              Top = 259
              Width = 622
              Height = 197
              ActivePage = tsContaCriarDocumentos
              TabOrder = 6
              object tsContaCriarDadosAdicionais: TTabSheet
                Caption = 'Dados Adicionais'
                object pnContaCriarCorporate: TPanel
                  Left = 0
                  Top = 0
                  Width = 614
                  Height = 169
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbContaCriarFundacao: TLabel
                    Left = 15
                    Top = 14
                    Width = 74
                    Height = 13
                    Caption = 'Data Funda'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarNomeEmpresa: TLabel
                    Left = 108
                    Top = 14
                    Width = 72
                    Height = 13
                    Caption = 'Nome Empresa'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCelular: TLabel
                    Left = 15
                    Top = 64
                    Width = 32
                    Height = 13
                    Caption = 'Celular'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentante: TLabel
                    Left = 108
                    Top = 64
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteMae: TLabel
                    Left = 247
                    Top = 64
                    Width = 67
                    Height = 13
                    Caption = 'Nome da M'#227'e'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCPF: TLabel
                    Left = 370
                    Top = 14
                    Width = 93
                    Height = 13
                    Caption = 'CPF Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteEmail: TLabel
                    Left = 370
                    Top = 64
                    Width = 25
                    Height = 13
                    Caption = 'Email'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarNascimento: TLabel
                    Left = 491
                    Top = 14
                    Width = 83
                    Height = 13
                    Caption = 'Nasc. Represent.'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCEP: TLabel
                    Left = 15
                    Top = 112
                    Width = 21
                    Height = 13
                    Caption = 'CEP'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteLogradouro: TLabel
                    Left = 108
                    Top = 114
                    Width = 54
                    Height = 13
                    Caption = 'Logradouro'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteNumero: TLabel
                    Left = 247
                    Top = 114
                    Width = 37
                    Height = 13
                    Caption = 'N'#250'mero'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteBairro: TLabel
                    Left = 319
                    Top = 114
                    Width = 27
                    Height = 13
                    Caption = 'Bairro'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCidade: TLabel
                    Left = 432
                    Top = 114
                    Width = 33
                    Height = 13
                    Caption = 'Cidade'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteUF: TLabel
                    Left = 563
                    Top = 112
                    Width = 14
                    Height = 13
                    Caption = 'UF'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edContaCriarNomeEmpresa: TEdit
                    Left = 108
                    Top = 29
                    Width = 252
                    Height = 21
                    TabOrder = 1
                  end
                  object edContaCriarRepresentanteCelular: TEdit
                    Left = 15
                    Top = 79
                    Width = 83
                    Height = 21
                    TabOrder = 4
                  end
                  object edContaCriarRepresentanteNome: TEdit
                    Left = 108
                    Top = 79
                    Width = 130
                    Height = 21
                    TabOrder = 5
                  end
                  object edContaCriarRepresentanteMae: TEdit
                    Left = 247
                    Top = 79
                    Width = 113
                    Height = 21
                    TabOrder = 6
                  end
                  object edContaCriarRepresentanteCPF: TEdit
                    Left = 370
                    Top = 29
                    Width = 102
                    Height = 21
                    TabOrder = 2
                  end
                  object edContaCriarRepresentanteEmail: TEdit
                    Left = 370
                    Top = 79
                    Width = 232
                    Height = 21
                    TabOrder = 7
                  end
                  object edContaCriarFundacao: TDateTimePicker
                    Left = 15
                    Top = 29
                    Width = 83
                    Height = 23
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 0
                  end
                  object edContaCriarNascimento: TDateTimePicker
                    Left = 491
                    Top = 29
                    Width = 111
                    Height = 23
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 3
                  end
                  object edContaCriarRepresentanteCEP: TEdit
                    Left = 15
                    Top = 129
                    Width = 83
                    Height = 21
                    TabOrder = 8
                    OnChange = edContaCriarCEPChange
                    OnKeyPress = edOnlyNumbersKeyPress
                  end
                  object edContaCriarRepresentanteLogradouro: TEdit
                    Left = 108
                    Top = 129
                    Width = 130
                    Height = 21
                    TabOrder = 9
                  end
                  object edContaCriarRepresentanteNumero: TEdit
                    Left = 247
                    Top = 129
                    Width = 60
                    Height = 21
                    TabOrder = 10
                  end
                  object edContaCriarRepresentanteBairro: TEdit
                    Left = 319
                    Top = 129
                    Width = 103
                    Height = 21
                    TabOrder = 11
                  end
                  object edContaCriarRepresentanteCidade: TEdit
                    Left = 432
                    Top = 129
                    Width = 120
                    Height = 21
                    TabOrder = 12
                  end
                  object edContaCriarRepresentanteUF: TEdit
                    Left = 563
                    Top = 129
                    Width = 39
                    Height = 21
                    TabOrder = 13
                  end
                end
              end
              object tsContaCriarDocumentos: TTabSheet
                Caption = 'Documentos'
                object pnImagem: TPanel
                  Left = 0
                  Top = 0
                  Width = 614
                  Height = 169
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object Imagem: TImage
                    Left = 0
                    Top = 37
                    Width = 614
                    Height = 132
                    Align = alClient
                    AutoSize = True
                    Center = True
                    Proportional = True
                    Stretch = True
                  end
                  object pnRodapeImagem: TPanel
                    Left = 0
                    Top = 0
                    Width = 614
                    Height = 37
                    Align = alTop
                    BevelOuter = bvNone
                    TabOrder = 0
                    DesignSize = (
                      614
                      37)
                    object btVoltar: TBitBtn
                      Left = 578
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
                    end
                  end
                end
                object pnContaCriarCorporate1: TPanel
                  Left = 0
                  Top = 0
                  Width = 614
                  Height = 169
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 1
                  object lbContaCriarRepresentanteFoto: TLabel
                    Left = 15
                    Top = 80
                    Width = 21
                    Height = 13
                    Caption = 'Foto'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btContaCriarRepresentanteFoto: TSpeedButton
                    Left = 153
                    Top = 95
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
                  object btContaCriarRepresentanteMostrarFoto: TSpeedButton
                    Left = 180
                    Top = 95
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
                    OnClick = btContaCriarRepresentanteMostrarFotoClick
                  end
                  object btContaCriarContratoSocial: TSpeedButton
                    Left = 350
                    Top = 96
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
                    OnClick = btContaCriarContratoSocialClick
                  end
                  object lbContaCriarContratoSocial: TLabel
                    Left = 213
                    Top = 80
                    Width = 72
                    Height = 13
                    Caption = 'Contrato Social'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarProcuracao: TLabel
                    Left = 409
                    Top = 80
                    Width = 103
                    Height = 13
                    Caption = 'Procura'#231#227'o(Opcional)'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btContaCriarProcuracao: TSpeedButton
                    Left = 546
                    Top = 96
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
                    OnClick = btContaCriarProcuracaoClick
                  end
                  object edContaCriarRepresentanteFoto: TEdit
                    Left = 15
                    Top = 95
                    Width = 135
                    Height = 23
                    AutoSize = False
                    TabOrder = 0
                  end
                  object edContaCriarContratoSocial: TEdit
                    Left = 213
                    Top = 95
                    Width = 133
                    Height = 23
                    AutoSize = False
                    TabOrder = 1
                  end
                  object rgContaCriarTipoDocumento: TRadioGroup
                    Left = 15
                    Top = 19
                    Width = 185
                    Height = 39
                    Columns = 2
                    ItemIndex = 1
                    Items.Strings = (
                      'CNH'
                      'RG')
                    TabOrder = 2
                  end
                  object pnCriarContaTipoCNH: TPanel
                    Left = 213
                    Top = 8
                    Width = 386
                    Height = 50
                    BevelOuter = bvNone
                    TabOrder = 4
                    Visible = False
                    object lbContaCriarDocumentoCNH: TLabel
                      Left = 1
                      Top = 10
                      Width = 23
                      Height = 13
                      Caption = 'CNH'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarAbrirDocumentoCNH: TSpeedButton
                      Left = 137
                      Top = 26
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
                      OnClick = btContaCriarAbrirDocumentoCNHClick
                    end
                    object btContaCriarVerDocumentoCNH: TSpeedButton
                      Left = 166
                      Top = 26
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
                      OnClick = btContaCriarVerDocumentoCNHClick
                    end
                    object edContaCriarDocumentoCNH: TEdit
                      Left = 1
                      Top = 25
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 0
                    end
                  end
                  object pnCriarContaTipoRG: TPanel
                    Left = 213
                    Top = 8
                    Width = 386
                    Height = 50
                    BevelOuter = bvNone
                    TabOrder = 3
                    Visible = False
                    object lbContaCriarRepresentanteRGFrente: TLabel
                      Left = 1
                      Top = 10
                      Width = 79
                      Height = 13
                      Caption = 'Foto RG (Frente)'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarRepresentanteRGFotoFrente: TSpeedButton
                      Left = 137
                      Top = 26
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
                      Left = 195
                      Top = 10
                      Width = 76
                      Height = 13
                      Caption = 'Foto RG (Verso)'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object btContaCriarRepresentanteRGFotoVerso: TSpeedButton
                      Left = 335
                      Top = 26
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
                      Left = 360
                      Top = 26
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
                      Left = 164
                      Top = 26
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
                    object edContaCriarRepresentanteRGFotoFrente: TEdit
                      Left = 1
                      Top = 25
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 0
                    end
                    object edContaCriarRepresentanteRGFotoVerso: TEdit
                      Left = 197
                      Top = 25
                      Width = 133
                      Height = 23
                      AutoSize = False
                      TabOrder = 1
                    end
                  end
                  object edContaCriarProcuracao: TEdit
                    Left = 409
                    Top = 95
                    Width = 133
                    Height = 23
                    AutoSize = False
                    TabOrder = 5
                  end
                end
              end
            end
            object gbGeolocalizacao: TGroupBox
              Left = 20
              Top = 459
              Width = 622
              Height = 68
              Caption = 'Encontrar Geolocaliza'#231#227'o (IP2Location)'
              TabOrder = 7
              DesignSize = (
                622
                68)
              object lbGeolocalizacaoIP: TLabel
                Left = 15
                Top = 19
                Width = 49
                Height = 13
                Caption = 'IP Externo'
                Color = clBtnFace
                ParentColor = False
              end
              object edGeolocalizacaoIP: TEdit
                Left = 15
                Top = 33
                Width = 168
                Height = 21
                TabOrder = 0
                OnChange = edContaCriarCEPChange
                OnKeyPress = edOnlyNumbersKeyPress
              end
              object btGeolocalizacaoPesquisar: TBitBtn
                Left = 184
                Top = 31
                Width = 112
                Height = 24
                Anchors = [akTop]
                Caption = 'Pesquisar'
                TabOrder = 1
                OnClick = btGeolocalizacaoPesquisarClick
              end
              object pnGeolocalizacaoResult: TPanel
                Left = 314
                Top = 15
                Width = 306
                Height = 51
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 2
                Visible = False
                object lbGeolocalizacaoLongitude: TLabel
                  Left = 162
                  Top = 0
                  Width = 47
                  Height = 13
                  Caption = 'Longitude'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbGeolocalizacaoLatitude: TLabel
                  Left = 24
                  Top = 0
                  Width = 38
                  Height = 13
                  Caption = 'Latitude'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edGeolocalizacaoLongitude: TEdit
                  Left = 162
                  Top = 15
                  Width = 130
                  Height = 21
                  TabOrder = 0
                end
                object edGeolocalizacaoLatitude: TEdit
                  Left = 24
                  Top = 15
                  Width = 128
                  Height = 21
                  TabOrder = 1
                end
              end
            end
          end
          object pnContaCriarRodape: TPanel
            Left = 0
            Top = 631
            Width = 666
            Height = 45
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              666
              45)
            object btContaCriar: TBitBtn
              Left = 521
              Top = 13
              Width = 112
              Height = 26
              Anchors = [akTop]
              Caption = 'Criar'
              TabOrder = 2
              OnClick = btContaCriarClick
            end
            object btContaCriarPreencherDados: TBitBtn
              Left = 199
              Top = 13
              Width = 186
              Height = 26
              Anchors = [akTop]
              Caption = 'Preencher(Dados Fict'#237'cios)'
              TabOrder = 0
              OnClick = btContaCriarPreencherDadosClick
            end
            object btContaCriarLimparDados: TBitBtn
              Left = 394
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
            Width = 666
            Height = 676
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbContaConsultarAccountId: TLabel
              Left = 15
              Top = 20
              Width = 52
              Height = 13
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
              Height = 21
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
            Width = 666
            Height = 676
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              666
              676)
            object lbContaInativarAccountId: TLabel
              Left = 15
              Top = 25
              Width = 52
              Height = 13
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaInativar: TBitBtn
              Left = 539
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
              Height = 21
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
            Width = 666
            Height = 676
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
                Top = 15
                Width = 588
                Height = 78
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  78)
                object lbChavePIXIncluirExternalId: TLabel
                  Left = 228
                  Top = 15
                  Width = 52
                  Height = 13
                  Caption = 'External ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXIncluirAccountId: TLabel
                  Left = 15
                  Top = 15
                  Width = 52
                  Height = 13
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
                  Height = 21
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
                  Height = 21
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
                Top = 15
                Width = 588
                Height = 78
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  78)
                object lbChavePIXConsultar: TLabel
                  Left = 15
                  Top = 15
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edChavePIXConsultar: TEdit
                  Left = 15
                  Top = 30
                  Width = 425
                  Height = 21
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
                Top = 15
                Width = 588
                Height = 78
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  78)
                object lbChavePIXExcluirAccountId: TLabel
                  Left = 15
                  Top = 15
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXExcluir: TLabel
                  Left = 228
                  Top = 15
                  Width = 51
                  Height = 13
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
                  Height = 21
                  TabOrder = 1
                end
                object edChavePIXExcluir: TEdit
                  Left = 228
                  Top = 30
                  Width = 212
                  Height = 21
                  TabOrder = 2
                end
              end
            end
          end
        end
      end
      object pnLogs: TPanel
        Left = 674
        Top = 0
        Width = 355
        Height = 716
        Align = alRight
        TabOrder = 1
        object lbLog: TLabel
          Left = 1
          Top = 1
          Width = 353
          Height = 13
          Align = alTop
          Caption = 'Log das Requisi'#231#245'es:'
          Color = clBtnFace
          ParentColor = False
        end
        object mmLogGerencial: TMemo
          Left = 1
          Top = 14
          Width = 353
          Height = 669
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape: TPanel
          Left = 1
          Top = 683
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
        Top = 679
        Width = 1034
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
        Width = 1034
        Height = 679
        ActivePage = tsPIX
        Align = alClient
        TabHeight = 30
        TabOrder = 1
        TabWidth = 172
        object tsPIX: TTabSheet
          Caption = 'PIX'
          object pConfPIX: TPanel
            Left = 0
            Top = 0
            Width = 1026
            Height = 639
            Align = alClient
            Anchors = []
            BevelOuter = bvSpace
            TabOrder = 0
            object pnConfig2: TPanel
              Left = 1
              Top = 81
              Width = 1024
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
                  Top = 15
                  Width = 300
                  Height = 116
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    300
                    116)
                  object lbProxyHost: TLabel
                    Left = 10
                    Top = 5
                    Width = 22
                    Height = 13
                    Caption = 'Host'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxyPorta: TLabel
                    Left = 192
                    Top = 5
                    Width = 25
                    Height = 13
                    Anchors = [akTop, akRight]
                    Caption = 'Porta'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxyUsuario: TLabel
                    Left = 10
                    Top = 50
                    Width = 36
                    Height = 13
                    Caption = 'Usu'#225'rio'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbProxySenha: TLabel
                    Left = 192
                    Top = 50
                    Width = 31
                    Height = 13
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
                    Height = 21
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 0
                  end
                  object edProxyUsuario: TEdit
                    Left = 10
                    Top = 65
                    Width = 168
                    Height = 21
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 1
                  end
                  object edProxySenha: TEdit
                    Left = 192
                    Top = 65
                    Width = 74
                    Height = 21
                    Anchors = [akTop, akRight]
                    PasswordChar = '*'
                    TabOrder = 2
                  end
                  object edProxyPorta: TSpinEdit
                    Left = 192
                    Top = 20
                    Width = 99
                    Height = 22
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
                  Top = 15
                  Width = 300
                  Height = 116
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbLogArquivo: TLabel
                    Left = 20
                    Top = 5
                    Width = 36
                    Height = 13
                    Caption = 'Arquivo'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbLogNivel: TLabel
                    Left = 20
                    Top = 50
                    Width = 26
                    Height = 13
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
                    Height = 21
                    TabOrder = 0
                  end
                  object cbLogNivel: TComboBox
                    Left = 20
                    Top = 65
                    Width = 232
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
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
              Width = 1024
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
                  Top = 15
                  Width = 300
                  Height = 63
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbAmbiente: TLabel
                    Left = 14
                    Top = 5
                    Width = 44
                    Height = 13
                    Caption = 'Ambiente'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbTimeout: TLabel
                    Left = 192
                    Top = 5
                    Width = 38
                    Height = 13
                    Caption = 'Timeout'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object cbAmbiente: TComboBox
                    Left = 14
                    Top = 20
                    Width = 164
                    Height = 21
                    Style = csDropDownList
                    ItemHeight = 13
                    TabOrder = 0
                    OnChange = cbAmbienteChange
                  end
                  object edTimeout: TSpinEdit
                    Left = 192
                    Top = 20
                    Width = 99
                    Height = 22
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
                  Top = 15
                  Width = 300
                  Height = 63
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbExpiracao: TLabel
                    Left = 24
                    Top = 5
                    Width = 47
                    Height = 13
                    Caption = 'Expira'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object seCobrancaExpiracao: TSpinEdit
                    Left = 24
                    Top = 20
                    Width = 232
                    Height = 22
                    Increment = 10
                    MaxValue = 999999
                    MinValue = 0
                    TabOrder = 0
                    Value = 3600
                  end
                end
              end
            end
            object pnIP2Location: TPanel
              Left = -6
              Top = 214
              Width = 1032
              Height = 82
              BevelOuter = bvNone
              TabOrder = 2
              object gbIP2Location: TGroupBox
                Left = 0
                Top = 0
                Width = 615
                Height = 82
                Align = alLeft
                Caption = 'IP2Location (Geolocaliza'#231#227'o)'
                TabOrder = 0
                DesignSize = (
                  615
                  82)
                object lbIP2LocationAPIKey: TLabel
                  Left = 10
                  Top = 21
                  Width = 38
                  Height = 13
                  Caption = 'API Key'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edIP2LocationAPIKey: TEdit
                  Left = 10
                  Top = 36
                  Width = 554
                  Height = 21
                  Anchors = [akLeft, akTop, akRight]
                  TabOrder = 0
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
            Width = 1026
            Height = 639
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object gbMediator: TGroupBox
              Left = 0
              Top = 0
              Width = 1026
              Height = 304
              Align = alTop
              Caption = 'Configura'#231#245'es Mediator'
              TabOrder = 0
              object lbPSPClientID: TLabel
                Left = 21
                Top = 27
                Width = 40
                Height = 13
                Caption = 'Client ID'
                Color = clBtnFace
                ParentColor = False
              end
              object lbPSPClientSecret: TLabel
                Left = 21
                Top = 80
                Width = 60
                Height = 13
                Caption = 'Client Secret'
                Color = clBtnFace
                ParentColor = False
              end
              object lbErroCertificado: TLabel
                Left = 21
                Top = 251
                Width = 77
                Height = 13
                Caption = 'lbErroCertificado'
                Color = clBtnFace
                ParentColor = False
              end
              object lbArqCertificado: TLabel
                Left = 21
                Top = 210
                Width = 89
                Height = 13
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
                Width = 109
                Height = 13
                Caption = 'Arquivo Chave Privada'
                Color = clBtnFace
                ParentColor = False
              end
              object lbErroChavePrivada: TLabel
                Left = 21
                Top = 175
                Width = 94
                Height = 13
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
                Left = 328
                Top = 80
                Width = 52
                Height = 13
                Caption = 'Secret Key'
                Color = clBtnFace
                ParentColor = False
              end
              object btVerClientID: TSpeedButton
                Left = 288
                Top = 40
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
                OnClick = btVerClientIDClick
              end
              object btVerClientSecret: TSpeedButton
                Left = 288
                Top = 93
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
                OnClick = btVerClientSecretClick
              end
              object btVerSecretKey: TSpeedButton
                Left = 598
                Top = 93
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
                OnClick = btVerSecretKeyClick
              end
              object edPSPClientID: TEdit
                Left = 21
                Top = 41
                Width = 267
                Height = 21
                PasswordChar = '*'
                TabOrder = 0
              end
              object edPSPClientSecret: TEdit
                Left = 21
                Top = 93
                Width = 267
                Height = 21
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
                Left = 328
                Top = 93
                Width = 269
                Height = 21
                PasswordChar = '*'
                TabOrder = 4
              end
            end
            object gbAccount: TGroupBox
              Left = 0
              Top = 304
              Width = 1026
              Height = 335
              Align = alClient
              Caption = 'Configura'#231#245'es Contas'
              TabOrder = 1
              object lbAccountId: TLabel
                Left = 21
                Top = 33
                Width = 52
                Height = 13
                Caption = 'Account Id'
                Color = clBtnFace
                ParentColor = False
              end
              object lbChavePIX: TLabel
                Left = 21
                Top = 80
                Width = 51
                Height = 13
                Caption = 'Chave PIX'
                Color = clBtnFace
                ParentColor = False
              end
              object lbMediatorFee: TLabel
                Left = 21
                Top = 133
                Width = 123
                Height = 13
                Caption = 'Mediator Fee QRCode R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFee: TLabel
                Left = 185
                Top = 133
                Width = 104
                Height = 13
                Caption = 'Tipo de Mediator  Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object lbMediatorFeeEstorno: TLabel
                Left = 21
                Top = 186
                Width = 118
                Height = 13
                Caption = 'Mediator Fee Estorno R$'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTipoMediatorFeeEstorno: TLabel
                Left = 185
                Top = 186
                Width = 104
                Height = 13
                Caption = 'Tipo de Mediator  Fee'
                Color = clBtnFace
                ParentColor = False
              end
              object cbAccountId: TComboBox
                Left = 21
                Top = 49
                Width = 601
                Height = 21
                ItemHeight = 13
                TabOrder = 0
                OnSelect = cbAccountIdSelect
              end
              object cbChavePIX: TComboBox
                Left = 21
                Top = 96
                Width = 601
                Height = 21
                ItemHeight = 13
                TabOrder = 1
              end
              object edMediatorFee: TEdit
                Left = 21
                Top = 149
                Width = 145
                Height = 21
                TabOrder = 2
              end
              object cbTipoMediatorFee: TComboBox
                Left = 185
                Top = 149
                Width = 145
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
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
                Top = 202
                Width = 145
                Height = 21
                TabOrder = 4
              end
              object cbTipoMediatorFeeEstorno: TComboBox
                Left = 185
                Top = 202
                Width = 145
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
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
    ContentsEncodingCompress = []
    NivelLog = 0
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
  object ImageList1: TImageList
    Left = 736
    Top = 24
    Bitmap = {
      494C010122002700040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000F2C4F200B2B2B200737373003D3D3D003D3D3D0073737300B2B2B200F5B2
      F500000000000000000000000000000000000000000000000000000000000000
      0000FB83FB00BBBBBB0094949400585858005858580094949400BBBBBB00FB83
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AFAF
      AF0055555500BDBDBD00E8E8E8000000000000000000E8E8E800BDBDBD005555
      5500B0B0B000000000000000000000000000000000000000000000000000C5C5
      C500353535006A6A6A00A3A3A300C5C5C500C5C5C500A3A3A3006A6A6A003535
      3500C5C5C5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8C8C009A9A
      9A00000000000000000000000000000000000000000000000000000000000000
      0000999999008D8D8D0000000000000000000000000000000000939393002E2E
      2E00C3C3C3000000000000000000E4E4E400E4E4E4000000000000000000C3C3
      C3002E2E2E009393930000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AEAEAE009A9A9A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000098989800B0B0B0000000000000000000C5C5C5002E2E2E00E8E8
      E8000000000000000000000000009E9E9E009E9E9E0000000000000000000000
      0000E8E8E8002E2E2E00C5C5C500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2C4F20056565600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000055555500F5B2F500FB83FB0035353500C3C3C3000000
      00000000000000000000D6D6D6004040400045454500E2E2E200000000000000
      000000000000C3C3C30035353500FB83FB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B1B1B100BDBDBD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00B2B2B200BBBBBB006A6A6A00000000000000
      000000000000000000001C1C1C00A0A0A000A9A9A90038383800000000000000
      000000000000000000006A6A6A00BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000071717100EBE2EB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E8E8E8007373730094949400A3A3A300000000000000
      000000000000D6D6D60000000000000000000000000000000000FB83FB000000
      00000000000000000000A3A3A300949494000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004040400000000000000000000000
      0000C5C5C500B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600C5C5
      C5000000000000000000000000004242420058585800C5C5C500000000000000
      0000000000000000000000000000CECECE00717171007E7E7E00000000000000
      00000000000000000000C5C5C500585858000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D3D3D0000000000000000000000
      0000CACACA00C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100C1C1C100CACA
      CA000000000000000000000000004040400058585800C5C5C500000000000000
      00000000000000000000A9A9A9002E2E2E00A3A3A30000000000000000000000
      00000000000000000000C5C5C500585858000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000071717100EBE2EB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E8E8E8007373730094949400A3A3A300000000000000
      000000000000FB83FB0000000000E4E4E400000000009E9E9E00E4E4E4000000
      00000000000000000000A3A3A300949494000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0B0B000BEBEBE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BDBDBD00B2B2B200BBBBBB006A6A6A00000000000000
      0000000000000000000000000000D0D0D000CACACA0000000000EED4EE000000
      000000000000000000006A6A6A00BCBCBC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2C4F20058585800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000056565600F2C4F200FB83FB0035353500C3C3C3000000
      00000000000000000000CBCBCB001C1C1C0000000000ADADAD00000000000000
      000000000000C3C3C30035353500FB83FB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ADADAD009B9B9B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009A9A9A00AFAFAF000000000000000000C5C5C5002E2E2E00E8E8
      E8000000000000000000000000009B9B9B009797970000000000000000000000
      0000E8E8E8002E2E2E00C5C5C500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008B8B8B009B9B
      9B00000000000000000000000000000000000000000000000000000000000000
      00009A9A9A008C8C8C0000000000000000000000000000000000939393002E2E
      2E00C3C3C3000000000000000000E4E4E400E4E4E4000000000000000000C3C3
      C3002E2E2E009393930000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ADAD
      AD0056565600BEBEBE00EBE2EB000000000000000000E8E8E800BEBEBE005656
      5600AEAEAE00000000000000000000000000000000000000000000000000C5C5
      C500353535006A6A6A00A3A3A300C5C5C500C5C5C500A3A3A3006A6A6A003535
      3500C5C5C5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F2C4F200B0B0B000727272003B3B3B003B3B3B0072727200B0B0B000F2C4
      F200000000000000000000000000000000000000000000000000000000000000
      0000FB83FB00BBBBBB0094949400585858005858580094949400BBBBBB00FB83
      FB00000000000000000000000000000000000000000000000000000000000000
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
      0000F2C4F200B2B2B200737373003D3D3D003D3D3D0073737300B2B2B200F5B2
      F500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AFAF
      AF0055555500BDBDBD00E8E8E8000000000000000000E8E8E800BDBDBD005555
      5500B0B0B0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D7D7D7002A2A2A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000016161600D7D7D700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6D6D600D5D5
      D500D5D5D50000000000000000000000000000000000000000008C8C8C009A9A
      9A00000000000000000000000000000000000000000000000000000000000000
      0000999999008D8D8D0000000000000000000000000000000000FB83FB00BDBD
      BD00A2A2A200AEAEAE00DDDDDD00000000000000000000000000E5E5E5009C9C
      9C009C9C9C009C9C9C00C5C5C50000000000000000009F9F9F0081818100D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500818181009F9F9F00000000000000000000000000000000004B4B
      4B00C3C3C3000000000000000000000000000000000000000000BDBDBD000000
      000000000000D5D5D500000000000000000000000000AEAEAE009A9A9A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000098989800B0B0B0000000000000000000CCCCCC00404040006262
      62009696960081818100222222009B9B9B000000000000000000D5D5D5002A2A
      2A00979797005D5D5D009C9C9C0000000000000000009C9C9C009C9C9C00E5E5
      E500D5D5D500D5D5D500E5E5E500000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C0000000000000000000000000000000000C3C3
      C3002A2A2A00C2C2C20000000000000000000000000000000000C6C6C6000D0D
      0D0000000000D5D5D5000000000000000000F2C4F20056565600000000000000
      0000000000000000000000000000C7C7C700C7C7C70000000000000000000000
      0000000000000000000055555500F5B2F500DFDFDF002A2A2A00BABABA000000
      00000000000000000000E1E1E1004F4F4F00ABABAB0000000000D0D0D0004D4D
      4D00000000009C9C9C009898980000000000000000009C9C9C009C9C9C00A3A3
      A3004242420042424200A3A3A300000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C00000000000000000000000000000000000000
      0000C5C5C5001C1C1C00C2C2C2000000000000000000C4C4C40022222200C4C4
      C400BDBDBD00D6D6D6000000000000000000B1B1B100BDBDBD00000000000000
      0000000000000000000000000000BBBBBB00BBBBBB0000000000000000000000
      00000000000000000000BCBCBC00B2B2B2008F8F8F009393930000000000AFAF
      AF00515151008282820000000000CECECE001616160042424200353535004D4D
      4D00000000009C9C9C002222220042424200000000009C9C9C009C9C9C000000
      00000000000000000000000000000000000000000000ADADAD00CBCBCB000000
      0000000000009C9C9C009C9C9C00000000000000000000000000000000000000
      000000000000C6C6C6002E2E2E00C2C2C20000000000A7A7A700C2C2C2000000
      00000000000000000000000000000000000071717100EBE2EB00000000000000
      0000000000000000000000000000BBBBBB00BBBBBB0000000000000000000000
      00000000000000000000E8E8E8007373730040404000CCCCCC00E4E4E4002626
      2600C0C0C00065656500B0B0B00000000000D5D5D500D5D5D500D5D5D500D9D9
      D90000000000E5E5E500B2B2B20000000000000000009C9C9C009C9C9C00C5C5
      C5009C9C9C009C9C9C00C5C5C50000000000ABABAB001C1C1C0022222200CDCD
      CD00000000009C9C9C009C9C9C00000000000000000000000000000000000000
      00000000000000000000C2C2C20026262600C5C5C50000000000000000000000
      0000000000000000000000000000000000004040400000000000000000000000
      0000C5C5C500B6B6B600B6B6B6008484840084848400B6B6B600B6B6B600C5C5
      C5000000000000000000000000004242420040404000CCCCCC00E4E4E4002626
      2600C0C0C00065656500B0B0B00000000000D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500D5D5D500B2B2B20000000000000000009C9C9C009C9C9C00C5C5
      C5009C9C9C009C9C9C00C5C5C50000000000CECECE00D9D9D900BDBDBD002A2A
      2A00CBCBCB009B9B9B009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000C5C5C50026262600C2C2C200000000000000
      0000000000000000000000000000000000003D3D3D0000000000000000000000
      0000CACACA00C1C1C100C1C1C1008D8D8D008D8D8D00C1C1C100C1C1C100CACA
      CA00000000000000000000000000404040008F8F8F009393930000000000AFAF
      AF00515151008282820000000000CECECE001616160042424200424242004242
      420042424200424242004242420042424200000000009C9C9C009C9C9C000000
      000000000000000000000000000000000000000000000000000000000000BBBB
      BB00BFBFBF009B9B9B009C9C9C00000000000000000000000000000000000000
      000000000000C6C6C600A7A7A70000000000C2C2C2002A2A2A00C2C2C2000000
      00000000000000000000000000000000000071717100EBE2EB00000000000000
      0000000000000000000000000000BBBBBB00BBBBBB0000000000000000000000
      00000000000000000000E8E8E80073737300E0E0E0002A2A2A00B7B7B7000000
      00000000000000000000DFDFDF0045454500AFAFAF0000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C009C9C
      9C0000000000000000009C9C9C00000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C00000000000000000000000000000000000000
      0000C2C2C20016161600C5C5C5000000000000000000C5C5C5001C1C1C00C1C1
      C100B9B9B900D6D6D6000000000000000000B0B0B000BEBEBE00000000000000
      0000000000000000000000000000BBBBBB00BBBBBB0000000000000000000000
      00000000000000000000BDBDBD00B2B2B20000000000CCCCCC00404040006262
      62009696960081818100222222009C9C9C000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C00E5E5
      E500D5D5D500D5D5D500E5E5E500000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C0000000000000000000000000000000000C3C3
      C3002A2A2A00C2C2C20000000000000000000000000000000000C4C4C4000000
      000000000000D5D5D5000000000000000000F2C4F20058585800000000000000
      0000000000000000000000000000C7C7C700C7C7C70000000000000000000000
      0000000000000000000056565600F2C4F2000000000000000000FB83FB00BDBD
      BD00A2A2A200AEAEAE00DDDDDD00000000000000000000000000000000000000
      000000000000000000000000000000000000000000009F9F9F0081818100D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500818181009F9F9F00000000000000000000000000000000004B4B
      4B00C3C3C3000000000000000000000000000000000000000000BDBDBD000000
      000000000000D5D5D500000000000000000000000000ADADAD009B9B9B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009A9A9A00AFAFAF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D8D8D800515151004242
      4200424242004242420042424200424242004242420042424200424242004242
      4200424242004B4B4B00D8D8D800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6D6D600D5D5
      D500D5D5D50000000000000000000000000000000000000000008B8B8B009B9B
      9B00000000000000000000000000000000000000000000000000000000000000
      00009A9A9A008C8C8C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ADAD
      AD0056565600BEBEBE00EBE2EB000000000000000000E8E8E800BEBEBE005656
      5600AEAEAE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F2C4F200B0B0B000727272003B3B3B003B3B3B0072727200B0B0B000F2C4
      F200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E8E8E800EBE2EB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BFBF
      BF009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C00BBBBBB000000000000000000000000000000000000000000000000000000
      000000000000E0E0E000898989001C1C1C00353535008F8F8F00E4E4E4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E4E4E400BCBCBC00A1A1A100A3A3A300BEBEBE00E6E6E6000000
      0000000000000000000000000000000000000000000000000000ADADAD006262
      62005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D
      5D005F5F5F00B4B4B40000000000000000000000000000000000DADADA001616
      1600000000000000000000000000000000000000000000000000000000000000
      000032323200E3E3E30000000000000000000000000000000000000000000000
      0000CFCFCF00474747000000000000000000DEDEDE008D8D8D0051515100D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      00009B9B9B003D3D3D007575750097979700969696007171710040404000A1A1
      A1000000000000000000000000000000000000000000000000005F5F5F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006565650000000000000000000000000000000000D5D5D5000000
      000082828200CACACA004D4D4D00B2B2B200B2B2B2004D4D4D00CACACA008282
      82001C1C1C00DBDBDB000000000000000000000000000000000000000000E0E0
      E000323232000000000000000000000000000000000000000000ACACAC004040
      4000E6E6E6000000000000000000000000000000000000000000000000007878
      780068686800DBDBDB0000000000000000000000000000000000D7D7D7005F5F
      5F008383830000000000000000000000000000000000000000005D5D5D000000
      0000000000009C9C9C00D5D5D500D5D5D5009C9C9C00000000005D5D5D000000
      0000000000005D5D5D0000000000000000000000000000000000D5D5D5000000
      000082828200CACACA004D4D4D00D5D5D500D5D5D5004D4D4D00CACACA008282
      82001C1C1C00DBDBDB0000000000000000000000000000000000000000007070
      7000000000000000000000000000000000000000000000000000000000009292
      92007979790000000000000000000000000000000000000000009B9B9B006868
      6800000000000000000000000000E5E5E500E5E5E50000000000000000000000
      00005D5D5D00A4A4A400000000000000000000000000000000005D5D5D000000
      00009C9C9C005D5D5D0081818100818181005D5D5D0094949400353535000000
      0000000000005F5F5F0000000000000000000000000000000000D5D5D5000000
      000035353500585858001C1C1C00CACACA00CACACA001C1C1C00585858003535
      35001C1C1C00DBDBDB0000000000000000000000000000000000CDCDCD001616
      160000000000000000000000000000000000000000000000000000000000F5B2
      F50035353500D4D4D400000000000000000000000000E4E4E4003D3D3D00DBDB
      DB00000000000000000000000000ABABAB00ABABAB0000000000000000000000
      0000D5D5D50042424200EBE2EB00000000000000000000000000888888000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008D8D8D0000000000000000000000000000000000D5D5D5000000
      000094949400E6E6E600585858004D4D4D004D4D4D0058585800E6E6E6009494
      94001C1C1C00DBDBDB0000000000000000000000000000000000949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000909090009F9F9F00000000000000000000000000BCBCBC00757575000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000066666600C5C5C50000000000000000000000000000000000D9D9
      D900D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500CACACA004D4D4D00B2B2
      B200DBDBDB000000000000000000000000000000000000000000D5D5D5000000
      00009C9C9C00000000005D5D5D00B2B2B200B2B2B2005D5D5D00000000009C9C
      9C001C1C1C00DBDBDB00000000000000000000000000000000006D6D6D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2C2C20076767600000000000000000000000000A2A2A200979797000000
      0000000000000000000000000000C5C5C500C5C5C50000000000000000000000
      0000000000008E8E8E00ACACAC00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000D5D5D5000000
      000082828200CACACA004D4D4D00B2B2B200B2B2B2004D4D4D00CACACA008282
      82001C1C1C00DBDBDB00000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      00000000000063636300000000000000000000000000A3A3A300969696000000
      00000000000000000000000000009C9C9C009C9C9C0000000000000000000000
      0000000000008D8D8D00ADADAD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007E7E7E00DBDB
      DB00000000000000000000000000000000000000000000000000D5D5D5000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C1C1C00DBDBDB00000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      0000000000005D5D5D00000000000000000000000000BEBEBE00717171000000
      00000000000000000000000000009C9C9C009C9C9C0000000000000000000000
      00000000000063636300C7C7C700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEDEDE002626
      2600000000000000000000000000000000000000000000000000000000000000
      00001C1C1C00DBDBDB00000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      0000000000005D5D5D00000000000000000000000000E6E6E60040404000D7D7
      D7000000000000000000000000009C9C9C009C9C9C0000000000000000000000
      0000D1D1D10047474700F2C4F200000000000000000000000000000000000000
      000000000000000000000000000000000000DEDEDE009A9A9A00CACACA00B2B2
      B200B6B6B600000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      00001C1C1C00DBDBDB00000000000000000000000000000000005D5D5D00CECE
      CE00000000000000000000000000000000000000000000000000000000000000
      0000000000005D5D5D0000000000000000000000000000000000A1A1A1005F5F
      5F00000000000000000000000000E5E5E500E5E5E5000000000000000000FB83
      FB0055555500AAAAAA0000000000000000000000000000000000000000000000
      0000000000000000000000000000CDCDCD00C0C0C000DBDBDB00C4C4C400D2D2
      D200D5D5D500B1B1B10000000000000000000000000000000000000000000000
      0000C0C0C0002626260000000000000000000000000000000000000000000000
      00001C1C1C00DBDBDB0000000000000000000000000000000000868686002A2A
      2A0097979700DFDFDF0000000000000000000000000000000000000000000000
      0000262626008888880000000000000000000000000000000000000000008181
      81005D5D5D00D5D5D50000000000000000000000000000000000D1D1D1005555
      55008C8C8C000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C7C7C700949494009C9C9C009595
      9500B4B4B4000000000000000000000000000000000000000000000000000000
      000000000000BFBFBF002E2E2E00000000000000000000000000000000000000
      00002E2E2E00E1E1E1000000000000000000000000000000000000000000D6D6
      D6008B8B8B004040400083838300CFCFCF000000000000000000383838008F8F
      8F00D9D9D9000000000000000000000000000000000000000000000000000000
      0000A4A4A40042424200666666008E8E8E008D8D8D006363630047474700AAAA
      AA00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C7C7C7009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C00CBCBCB000000000000000000000000000000000000000000000000000000
      000000000000E2E2E200A3A3A3004242420047474700A7A7A700E4E4E4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EBE2EB00C5C5C500ACACAC00ADADAD00C7C7C700F2C4F2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FD5FFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E5E5E500D5D5D500D5D5D500D5D5D500D5D5D500E5E5E5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000ABABAB005D5D5D005D5D5D005D5D5D005D5D5D00ABABAB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DDDDDD00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500DBDBDB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00080808000636363006363630083838300C5C5C5000000
      0000000000000000000000000000000000000000000000000000ADADAD006262
      62005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D
      5D005F5F5F00B4B4B400000000000000000000000000B8B8B800222222000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C1C1C00C7C7C700000000000000000000000000000000000000
      0000D8D8D800A0A0A0009C9C9C009C9C9C009C9C9C009C9C9C009E9E9E00D9D9
      D900000000000000000000000000000000000000000000000000000000000000
      0000767676004B4B4B00AEAEAE00D1D1D100D0D0D000AAAAAA00454545007E7E
      7E000000000000000000000000000000000000000000000000005F5F5F00B2B2
      B200D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500B2B2B200656565000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C00000000000000000000000000000000000000
      00002E2E2E000000000000000000000000000000000000000000000000005A5A
      5A00000000000000000000000000000000000000000000000000000000007676
      7600838383000000000000000000000000000000000000000000FD5FFD007A7A
      7A008080800000000000000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      0000D5D5D5005D5D5D000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C00000000000000000000000000000000000000
      0000000000004D4D4D005D5D5D005D5D5D005D5D5D005D5D5D004D4D4D000000
      0000000000000000000000000000000000000000000000000000C0C0C0004B4B
      4B0000000000000000000000000000000000000000000000000000000000FB83
      FB0040404000C8C8C800000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      0000D5D5D5005D5D5D000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C00000000000000000000000000000000000000
      000000000000CACACA0000000000000000000000000000000000CACACA000000
      000000000000000000000000000000000000000000000000000080808000AEAE
      AE00000000000000000000000000000000000000000000000000000000000000
      0000A3A3A3008B8B8B00000000000000000000000000000000005D5D5D00D5D5
      D500000000000000000000000000000000000000000000000000000000000000
      0000D5D5D5005D5D5D000000000000000000000000009C9C9C00000000000000
      0000000000000000000022222200A8A8A800A1A1A10016161600000000000000
      000000000000000000009C9C9C00000000000000000000000000000000000000
      000000000000D5D5D50000000000000000000000000000000000D5D5D5000000
      000000000000000000000000000000000000000000000000000063636300D1D1
      D100000000000000000000000000C5C5C500C5C5C50000000000000000000000
      0000C9C9C9006C6C6C00000000000000000000000000000000005D5D5D008686
      8600D1D1D100CDCDCD00A3A3A300D0D0D000CBCBCB00A3A3A300CFCFCF00CDCD
      CD00858585005D5D5D000000000000000000000000009C9C9C00000000000000
      0000161616007E7E7E00E1E1E100FB83FB0000000000DDDDDD00767676000D0D
      0D0000000000000000009C9C9C00000000000000000000000000000000000000
      000000000000D5D5D50000000000000000000000000000000000D5D5D5000000
      000000000000000000000000000000000000000000000000000060606000D2D2
      D2000000000000000000000000009C9C9C009C9C9C0000000000000000000000
      0000C7C7C7006A6A6A00000000000000000000000000C8C8C800323232008787
      87001C1C1C0035353500838383002A2A2A0032323200838383002A2A2A002626
      26008787870032323200D4D4D40000000000000000009C9C9C00000000004D4D
      4D00C0C0C00000000000C4C4C400515151005A5A5A00CACACA0000000000BBBB
      BB0047474700000000009C9C9C00000000000000000000000000000000000000
      000000000000D5D5D50000000000000000000000000000000000D5D5D5000000
      000000000000000000000000000000000000000000000000000080808000ADAD
      AD000000000000000000000000009C9C9C009C9C9C0000000000000000000000
      00009E9E9E008A8A8A000000000000000000000000009F9F9F008F8F8F000000
      00008585850093939300000000008B8B8B009393930000000000858585009393
      93000000000085858500AFAFAF0000000000000000009C9C9C0072727200EBE2
      EB00E1E1E100858585001616160000000000000000001C1C1C008D8D8D00E5E5
      E500E6E6E600707070009C9C9C00000000000000000000000000000000000000
      000000000000D5D5D50000000000000000000000000000000000D5D5D5000000
      0000000000000000000000000000000000000000000000000000C3C3C3004949
      4900FB83FB0000000000000000009C9C9C009C9C9C000000000000000000EBE2
      EB003B3B3B00CBCBCB00000000000000000000000000B9B9B9007C7C7C000000
      0000A9A9A9008C8C8C00000000009C9C9C009C9C9C000000000080808000B3B3
      B300000000006E6E6E00C3C3C30000000000000000009C9C9C008F8F8F00B2B2
      B200383838000000000000000000000000000000000000000000000000004040
      4000B9B9B900919191009C9C9C00000000000000000000000000000000000000
      000000000000D5D5D50000000000000000000000000000000000D5D5D5000000
      0000000000000000000000000000000000000000000000000000000000007C7C
      7C008A8A8A0000000000000000009C9C9C009C9C9C0000000000000000008080
      80008686860000000000000000000000000000000000DCDCDC003B3B3B000000
      0000BEBEBE0076767600000000009C9C9C009C9C9C00000000006D6D6D00C7C7
      C700F89DF8002E2E2E00E2E2E2000000000000000000C4C4C400262626000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000032323200C4C4C400000000000000000000000000000000000000
      000000000000B2B2B200D5D5D500D5D5D500D5D5D500D5D5D500B2B2B2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EBE2EB0000000000000000009C9C9C009C9C9C000000000000000000EBE2
      EB0000000000000000000000000000000000000000000000000058585800BABA
      BA00ADADAD0053535300CDCDCD008282820082828200C9C9C90049494900B3B3
      B300B0B0B0006363630000000000000000000000000000000000DEDEDE00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500E3E3E30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000D0D
      0D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ABABAB00ABABAB0000000000000000000000
      0000000000000000000000000000000000000000000000000000BABABA006C6C
      6C00686868006868680068686800686868006868680068686800686868006868
      68006C6C6C00C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007F7F7F000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEDEDE00DDDDDD00DDDDDD00DDDDDD00DDDDDD00E1E1E1000000
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
      000000000000E4E4E400BCBCBC00A2A2A200A3A3A300BEBEBE00E6E6E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E4E4E400BCBCBC00A2A2A200A3A3A300BEBEBE00E6E6E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009B9B9B0035353500000000000000000000000000000000003B3B3B00A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      00009B9B9B0035353500000000000000000000000000000000003B3B3B00A0A0
      A00000000000000000000000000000000000000000000000000000000000D7D7
      D700D5D5D500D5D5D500D5D5D500DADADA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007979
      7900000000000000000000000000000000000000000000000000000000000000
      0000828282000000000000000000000000000000000000000000000000007878
      7800000000000000000000000000000000000000000000000000000000000000
      0000828282000000000000000000000000000000000000000000808080000D0D
      0D0000000000000000000000000016161600C7C7C70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9B000000
      00000000000000000000666666002A2A2A000000000000000000000000000000
      000000000000A4A4A400000000000000000000000000000000009B9B9B000000
      00000D0D0D006A6A6A001C1C1C00000000000000000022222200696969000D0D
      0D0000000000A4A4A400000000000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C00000000005D5D5D000000
      00009C9C9C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E4E4E400353535000000
      0000000000009191910000000000C0C0C0001C1C1C0000000000000000000000
      00000000000040404000EBE2EB000000000000000000E4E4E400353535000000
      00006A6A6A0000000000B2B2B2001C1C1C0022222200B9B9B900000000005F5F
      5F000000000040404000EBE2EB000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C0000000000DBDBDB00D5D5
      D500E5E5E5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BCBCBC00000000000000
      00008F8F8F0000000000E2E2E20000000000BEBEBE0022222200000000000000
      00000000000000000000C5C5C5000000000000000000BCBCBC00000000000000
      00001C1C1C00B2B2B20000000000B2B2B200BABABA0000000000AAAAAA001616
      16000000000000000000C5C5C5000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C0000000000DBDBDB00D5D5
      D500D5D5D500DBDBDB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A2A2A200000000008686
      860000000000CECECE0040404000A6A6A60000000000BEBEBE001C1C1C000000
      00000000000000000000ACACAC000000000000000000A2A2A200000000000000
      0000000000001C1C1C00B2B2B2000000000000000000A9A9A900161616000000
      00000000000000000000ACACAC000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C00000000005D5D5D000000
      0000000000005D5D5D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A3A3A300000000007D7D
      7D00C6C6C600262626000000000000000000A6A6A60000000000C0C0C0002A2A
      2A000000000000000000ADADAD000000000000000000A3A3A300000000000000
      00000000000022222200BABABA000000000000000000B2B2B2001C1C1C000000
      00000000000000000000ADADAD000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BEBEBE00000000000000
      00000000000000000000000000000000000016161600A8A8A80000000000C0C0
      C0001C1C1C0000000000C7C7C7000000000000000000BEBEBE00000000000000
      000022222200B9B9B90000000000A9A9A900B2B2B20000000000B2B2B2001C1C
      1C000000000000000000C7C7C7000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000009C9C9C0000000000ABABAB009C9C
      9C009C9C9C009C9C9C00C5C5C500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E6E6E6003B3B3B000000
      0000000000000000000000000000000000000000000016161600A8A8A8000000
      0000A3A3A30045454500F2C4F2000000000000000000E6E6E6003B3B3B000000
      00006969690000000000AAAAAA00161616001C1C1C00B2B2B200FB83FB005D5D
      5D000000000045454500F2C4F200000000000000000000000000ABABAB009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C00C5C5C50000000000ABABAB009C9C
      9C009C9C9C009C9C9C00C5C5C500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A1A1A1000000
      0000000000000000000000000000000000000000000000000000161616009595
      950047474700AAAAAA0000000000000000000000000000000000A0A0A0000000
      00000D0D0D005F5F5F001616160000000000000000001C1C1C005D5D5D000D0D
      0D0000000000AAAAAA00000000000000000000000000C5C5C5009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C00E5E5E500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008282
      8200000000000000000000000000000000000000000000000000000000000000
      00008B8B8B000000000000000000000000000000000000000000000000008181
      8100000000000000000000000000000000000000000000000000000000000000
      00008A8A8A0000000000000000000000000000000000C5C5C5009C9C9C007979
      7900000000000000000026262600929292009C9C9C00E5E5E500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A4A4A400404040000000000000000000000000000000000045454500AAAA
      AA00000000000000000000000000000000000000000000000000000000000000
      0000A4A4A400404040000000000000000000000000000000000045454500AAAA
      AA00000000000000000000000000000000000000000000000000000000000000
      0000D5D5D500D5D5D500DEDEDE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EBE2EB00C5C5C500ADADAD00ADADAD00C7C7C700F2C4F2000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EBE2EB00C5C5C500ADADAD00ADADAD00C7C7C700F2C4F2000000
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
      000000000000D4D4D400D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2
      D200D4D4D400E5E5E50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000727272005555550068686800686868006868680068686800686868006868
      6800555555000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DDDDDD00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500DBDBDB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      000000000000DBDBDB0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B6B6B600222222000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C1C1C00C7C7C700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E5E5E500E2E2E2000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C0063636300D7D7D7000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C000000000000000000C5C5C5009C9C9C009C9C
      9C009C9C9C009C9C9C00B1B1B100000000000000000000000000000000000000
      000000000000C3C3C300DBDBDB000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C000000000016161600A3A3A300FB83FB00000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C0000000000000000009C9C9C00000000000000
      0000000000005C5C5C00E1E1E100000000000000000000000000000000000000
      0000DFDFDF00323232009F9F9F000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C0000000000000000000000000060606000CACACA000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C0000000000000000009C9C9C00000000000000
      000035353500DCDCDC000000000000000000000000000000000000000000F5B2
      F500626262004B4B4B00E8E8E8000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C0000000000000000000000000000000000222222008C8C
      8C00FB83FB00000000000000000000000000000000009C9C9C009C9C9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C0000000000000000009C9C9C00000000003D3D
      3D000D0D0D0073737300CFCFCF000000000000000000E8E8E800B7B7B7004040
      400045454500D5D5D500000000000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C0000000000000000000000000000000000262626009797
      970000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C0000000000000000009C9C9C0063636300E3E3
      E300BBBBBB005656560000000000161616003535350000000000222222007E7E
      7E00E0E0E00000000000000000000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C000000000000000000000000006A6A6A00D2D2D2000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000B5B5B500E5E5E5000000
      00000000000000000000CECECE00AEAEAE00A8A8A800BBBBBB00DDDDDD000000
      00000000000000000000000000000000000000000000A7A7A700989898000000
      00005D5D5D00D5D5D50000000000000000000000000000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000009C9C9C000000000022222200ADADAD0000000000000000000000
      000000000000000000000000000000000000000000009C9C9C005D5D5D009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C005D5D5D009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700989898000000
      0000717171004B4B4B005C5C5C005C5C5C005C5C5C005C5C5C005C5C5C005C5C
      5C00494949001616160000000000000000000000000000000000000000000000
      0000000000009C9C9C0071717100DDDDDD000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A700989898000000
      0000DEDEDE00A3A3A3009F9F9F009F9F9F009F9F9F009F9F9F009F9F9F009F9F
      9F00A2A2A200C1C1C10000000000000000000000000000000000000000000000
      000000000000E0E0E00000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C300262626000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000032323200C3C3C300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A8A8A8008E8E8E00FD5F
      FD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEDEDE00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500E3E3E30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D4D4D4003B3B3B002626
      2600262626002626260026262600262626002626260026262600262626000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D9D9D900D2D2
      D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D200D2D2D2000000
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
      0000CCCCCC000000000000000000000000000000000000000000DFDFDF00D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500E0E0E00000000000000000000000000000000000ADADAD006262
      62005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D
      5D005F5F5F00B4B4B40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B9B9
      B90035353500D6D6D600000000000000000000000000E8E8E800353535000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003D3D3D00EED4EE000000000000000000000000005F5F5F000000
      0000000000000000000065656500C8C8C800C5C5C5005C5C5C00000000000000
      0000000000006565650000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B6B6B6003232
      3200C8C8C80000000000000000000000000000000000E5E5E500000000008888
      8800C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5C500C5C5
      C5008888880000000000E5E5E5000000000000000000000000005D5D5D000000
      00000000000035353500E3E3E3000000000000000000DEDEDE00262626000000
      0000000000005D5D5D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DBDBDB00ACACAC009E9E9E00BBBBBB00FB83FB00C2C2C2003D3D3D00C5C5
      C5000000000000000000000000000000000000000000E5E5E50000000000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000B2B2B20000000000E5E5E5000000000000000000000000005D5D5D000000
      00000000000053535300FD5FFD000000000000000000F5B2F500454545000000
      0000000000005D5D5D00000000000000000000000000D7D7D700C6C6C6000000
      00000000000000000000000000000000000000000000ADADAD009C9C9C009C9C
      9C009C9C9C009C9C9C00C5C5C50000000000000000000000000000000000AAAA
      AA003B3B3B008888880096969600666666005353530078787800CBCBCB000000
      00000000000000000000000000000000000000000000E5E5E50000000000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000B2B2B20000000000E5E5E5000000000000000000000000005D5D5D000000
      00000000000016161600C2C2C2000000000000000000BBBBBB000D0D0D000000
      0000000000005D5D5D000000000000000000000000009696960040404000E5E5
      E5000000000000000000000000000000000000000000DCDCDC00515151000000
      000000000000000000009C9C9C00000000000000000000000000C7C7C7002A2A
      2A00DADADA00000000000000000000000000A3A3A3005A5A5A00FD5FFD000000
      00000000000000000000000000000000000000000000E5E5E50000000000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000B2B2B20000000000E5E5E5000000000000000000000000005D5D5D000000
      000000000000000000002A2A2A007C7C7C007878780026262600000000000000
      0000000000005D5D5D00000000000000000000000000E4E4E400424242006C6C
      6C00000000000000000000000000000000000000000000000000D6D6D6002E2E
      2E0000000000000000009C9C9C0000000000000000000000000078787800ADAD
      AD00000000000000000000000000000000000000000065656500C1C1C1000000
      00000000000000000000000000000000000000000000E5E5E50000000000B2B2
      B200000000000000000000000000000000000000000000000000000000000000
      0000B2B2B20000000000E5E5E5000000000000000000000000005D5D5D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005D5D5D0000000000000000000000000000000000D0D0D0004040
      400049494900BCBCBC00EED4EE000000000000000000CDCDCD006C6C6C001616
      16003B3B3B00000000009C9C9C000000000000000000000000005F5F5F00D1D1
      D100000000000000000000000000000000000000000090909000A9A9A9000000
      00000000000000000000000000000000000000000000E5E5E500000000009F9F
      9F00E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E5009F9F9F0000000000E5E5E5000000000000000000000000005D5D5D004D4D
      4D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D00585858001C1C1C000000
      0000000000005D5D5D000000000000000000000000000000000000000000DCDC
      DC00777777001C1C1C000D0D0D003535350022222200000000005C5C5C00C0C0
      C000DFDFDF005C5C5C009C9C9C000000000000000000000000006D6D6D00C2C2
      C200000000000000000000000000000000000000000072727200B8B8B8000000
      00000000000000000000000000000000000000000000E5E5E500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000026262600E7E7E7000000000000000000000000005D5D5D00CACA
      CA000000000000000000000000000000000000000000E6E6E600585858000000
      0000000000005D5D5D0000000000000000000000000000000000000000000000
      000000000000DBDBDB00B9B9B900A8A8A800AFAFAF00D0D0D000000000000000
      000000000000E1E1E100B1B1B100000000000000000000000000ADADAD005F5F
      5F0000000000000000000000000000000000CBCBCB003D3D3D00E3E3E3000000
      00000000000000000000000000000000000000000000E5E5E500000000000000
      00000000000000000000000000000000000070707000B2B2B200B2B2B200B2B2
      B200B2B2B200CACACA00000000000000000000000000000000005D5D5D00D5D5
      D5000000000000000000000000000000000000000000000000005D5D5D000000
      0000000000009292920000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007D7D
      7D0051515100B5B5B500CACACA00A3A3A30032323200B9B9B900000000000000
      00000000000000000000000000000000000000000000EBE2EB003D3D3D000000
      0000000000000000000000000000787878000000000000000000000000000000
      000000000000000000000000000000000000000000000000000062626200B2B2
      B200D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500CACACA004D4D4D000000
      0000909090000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B2B2B200767676006A6A6A0085858500D2D2D20000000000000000000000
      0000000000000000000000000000000000000000000000000000E2E2E200D5D5
      D500D5D5D500D5D5D500D5D5D500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B2B2B2006262
      62005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D005D5D5D009292
      9200000000000000000000000000000000000000000000000000000000000000
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
      000000000000E8E8E800B4B4B400929292008A8A8A00B4B4B400E1E1E1000000
      00000000000000000000000000000000000000000000E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E5E5E500E5E5E500E5E5E500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D9D9
      D900D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D6D6D600E2E2E2000000000000000000000000000000000000000000FD5F
      FD00737373001C1C1C0070707000AAAAAA00B2B2B20070707000262626004545
      4500FD5FFD00000000000000000000000000E8E8E80040404000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004B4B4B00F5B2F5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2C2C200515151001616
      1600000000000000000000000000000000000000000000000000000000000000
      0000000000002E2E2E008F8F8F00000000000000000000000000000000007C7C
      7C004D4D4D00D0D0D00000000000000000000000000000000000E0E0E0007373
      73007C7C7C00FD5FFD00000000000000000000000000B2B2B200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BABABA00000000000000000000000000000000000000
      0000E6E6E600A8A8A8006A6A6A0053535300535353006E6E6E00ACACAC00EBE2
      EB0000000000000000000000000000000000CECECE000D0D0D00000000000000
      0000000000000000000026262600C0C0C000C0C0C00026262600000000000000
      0000000000000000000000000000A8A8A80000000000000000009E9E9E007373
      7300000000000000000000000000E8E8E800DFDFDF0000000000000000000000
      0000737373004545450000000000000000000000000000000000686868000000
      0000000000000000000000000000A9A9A900A9A9A90000000000000000000000
      000000000000737373000000000000000000000000000000000000000000ABAB
      AB00000000000000000055555500949494009292920051515100000000001616
      1600B2B2B2000000000000000000000000007272720000000000000000000000
      0000000000002E2E2E00BFBFBF000000000000000000BFBFBF002E2E2E000000
      000000000000000000000000000056565600000000000000000022222200E1E1
      E100000000000000000000000000B2B2B2008C8C8C0000000000000000000000
      0000E1E1E10026262600E1E1E100000000000000000000000000CCCCCC002222
      2200000000000000000000000000000000000000000000000000000000000000
      000026262600D3D3D30000000000000000000000000000000000939393000000
      00000000000095959500000000000000000000000000FD5FFD008D8D8D000000
      00000D0D0D009B9B9B0000000000000000001616160000000000000000000000
      000026262600BFBFBF0000000000000000000000000000000000BFBFBF002626
      26000000000000000000000000005858580000000000D5D5D5004F4F4F000000
      0000000000000000000000000000B2B2B2008C8C8C0000000000000000000000
      0000000000006E6E6E00B4B4B400000000000000000000000000000000008585
      85000000000000000000000000009F9F9F009F9F9F0000000000000000000000
      00008F8F8F0000000000000000000000000000000000B2B2B2000D0D0D000000
      00005555550000000000D2D2D2007171710073737300D8D8D800FB83FB004B4B
      4B000000000016161600BBBBBB00000000005555550000000000000000000000
      00004747470099999900C5C5C5000000000000000000C5C5C500999999004747
      4700000000000000000000000000AEAEAE0000000000B4B4B4008D8D8D000000
      0000000000000000000000000000B2B2B2008C8C8C0000000000000000000000
      000000000000B2B2B2008A8A8A0000000000000000000000000000000000E4E4
      E4000D0D0D000000000000000000B2B2B200B2B2B20000000000000000001C1C
      1C00EED4EE00000000000000000000000000F2C4F2003B3B3B00000000000000
      0000949494000000000071717100000000000000000079797900000000008787
      8700000000000000000047474700FD5FFD00B6B6B60000000000000000000000
      000000000000000000009C9C9C0000000000000000009C9C9C00000000000000
      0000000000003D3D3D009B9B9B000000000000000000B9B9B900868686000000
      0000000000000000000000000000D9D9D900CACACA0000000000000000000000
      000000000000ABABAB0091919100000000000000000000000000000000000000
      0000A7A7A7000000000000000000B2B2B200B2B2B2000000000000000000B1B1
      B10000000000000000000000000000000000F89DF80040404000000000000000
      000092929200000000007373730000000000000000007D7D7D00000000008484
      840000000000000000004D4D4D000000000000000000959595001C1C1C000000
      0000000000000000000094949400000000000000000094949400000000000000
      000073737300E8E8E800000000000000000000000000D5D5D5004F4F4F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006E6E6E00B4B4B400000000000000000000000000000000000000
      00000000000056565600000000006A6A6A006A6A6A0000000000626262000000
      00000000000000000000000000000000000000000000B9B9B9000D0D0D000000
      000051515100FD5FFD00D8D8D800797979007D7D7D00DDDDDD00F2C4F2004747
      4700000000001C1C1C00C0C0C000000000000000000000000000D5D5D5009494
      94000000000000000000353535005D5D5D005D5D5D0035353500000000000000
      0000C6C6C60000000000000000000000000000000000000000004B4B4B00D0D0
      D000000000000000000000000000B2B2B2008C8C8C0000000000000000000000
      0000D0D0D0001C1C1C00E8E8E800000000000000000000000000000000000000
      000000000000C2C2C2000D0D0D00000000000000000016161600CACACA000000
      00000000000000000000000000000000000000000000000000009B9B9B000D0D
      0D00000000008D8D8D00FB83FB000000000000000000F2C4F200858585000000
      00000D0D0D00A3A3A30000000000000000000000000000000000000000000000
      00007A7A7A000000000000000000000000000000000000000000000000008888
      8800000000000000000000000000000000000000000000000000B6B6B6004D4D
      4D00E0E0E0000000000000000000000000000000000000000000000000000000
      00004D4D4D007373730000000000000000000000000000000000000000000000
      0000000000000000000079797900000000000000000083838300000000000000
      000000000000000000000000000000000000000000000000000000000000B5B5
      B5001C1C1C00000000004B4B4B00868686008484840047474700000000002626
      2600BBBBBB000000000000000000000000000000000000000000000000000000
      000000000000A5A5A5004B4B4B0022222200262626004F4F4F00ADADAD000000
      0000000000000000000000000000000000000000000000000000000000007C7C
      7C004D4D4D00D0D0D00000000000000000000000000000000000E0E0E0007373
      73007C7C7C00FD5FFD0000000000000000000000000000000000000000000000
      00000000000000000000DADADA002E2E2E0038383800E0E0E000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F5B2F500B2B2B20078787800585858005A5A5A007C7C7C00B6B6B600FB83
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DDDDDD00DEDEDE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B6B6B6004949490051515100858585008D8D8D0051515100222222009E9E
      9E00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009A9A9A00A3A3A30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D5D5D500B9B9B900B3B3B300D5D5D500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FB83FB000000000000000000000000000000
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
      0000FD5FFD00F2C4F20000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F89DF800A9A9A9009A9A9A00F89DF800000000000000
      000000000000000000000000000000000000000000000000000000000000FD5F
      FD00FD5FFD00FD5FFD0000000000000000000000000000000000000000000000
      0000FD5FFD00FD5FFD0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E8E8
      E8005A5A5A0051515100DCDCDC00000000000000000000000000FD5FFD00FD5F
      FD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5FFD00FD5F
      FD00FD5FFD00FD5FFD0000000000000000000000000000000000000000000000
      000000000000F5B2F5002E2E2E00000000000000000032323200DDDDDD000000
      0000000000000000000000000000000000000000000000000000686868002E2E
      2E002E2E2E000D0D0D000000000000000000DEDEDE004D4D4D0000000000A0A0
      A0000D0D0D001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7E7E7006565
      6500000000000000000053535300FD5FFD0000000000A9A9A9001C1C1C001C1C
      1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C
      1C001C1C1C001C1C1C00FD5FFD00000000000000000000000000000000000000
      000000000000737373000000000000000000000000000000000032323200E7E7
      E70000000000000000000000000000000000000000000000000068686800D2D2
      D200CBCBCB006666660000000000D1D1D100AFAFAF00D6D6D600000000009898
      980083838300D2D2D20000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E8E8E800656565000000
      00000000000000000000787878000000000000000000E6E6E600D4D4D400D4D4
      D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4D400D4D4
      D400D4D4D400D4D4D40000000000000000000000000000000000000000000000
      0000D0D0D0009F9F9F00222222000000000000000000000000006D6D6D00C3C3
      C30000000000000000000000000000000000000000000000000068686800CDCD
      CD00C7C7C7006363630000000000B2B2B2008C8C8C0000000000474747000000
      00009F9F9F000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E7E7E7005A5A5A00000000000000
      0000000000007F7F7F000000000000000000000000000000000000000000D4D4
      D400FB83FB000000000000000000D7D7D700000000000000000000000000DADA
      DA00000000000000000000000000000000000000000000000000F5B2F5003232
      320000000000A5A5A500C0C0C0002E2E2E0000000000A9A9A900E4E4E4002A2A
      2A002A2A2A00D8D8D80000000000000000000000000000000000797979004747
      4700474747004747470000000000B2B2B2008C8C8C0000000000000000000000
      0000D4D4D4001C1C1C00FD5FFD00000000000000000000000000000000000000
      0000000000000000000000000000E8E8E8005A5A5A0000000000000000000000
      00007F7F7F000000000000000000000000000000000000000000000000003232
      3200CACACA00000000000000000055555500D4D4D40000000000000000006A6A
      6A00D1D1D1000000000000000000000000000000000000000000787878000000
      00000000000000000000BEBEBE00BEBEBE008D8D8D00CDCDCD004D4D4D000000
      0000000000002A2A2A00F5B2F50000000000000000000000000000000000DADA
      DA00CDCDCD0000000000D4D4D400B9B9B900B0B0B000D1D1D100D1D1D1000000
      0000D4D4D4001C1C1C00FD5FFD0000000000000000000000000000000000F89D
      F800D3D3D300D5D5D500DADADA00656565000000000000000000000000007878
      7800000000000000000000000000000000000000000000000000000000003232
      3200CACACA00000000000000000055555500D4D4D40000000000000000006A6A
      6A00D1D1D10000000000000000000000000000000000BFBFBF00000000000000
      00000000000000000000000000009F9F9F00CCCCCC002E2E2E00000000000000
      000000000000000000009797970000000000000000000000000000000000A8A8
      A80082828200FB83FB0042424200B7B7B700D5D5D5006C6C6C006C6C6C00CCCC
      CC00CDCDCD006E6E6E0000000000000000000000000000000000B2B2B2004949
      4900000000000D0D0D002A2A2A00000000000000000000000000787878000000
      0000000000000000000000000000000000000000000000000000000000003232
      3200CACACA00000000000000000055555500D4D4D40000000000000000006A6A
      6A00D1D1D10000000000000000000000000000000000CBCBCB00000000000000
      0000000000000000000032323200C1C1C100EBE2EB004F4F4F00000000000000
      00000000000000000000A8A8A80000000000000000000000000070707000D5D5
      D500000000002E2E2E002E2E2E002E2E2E00929292000000000000000000A3A3
      A300A3A3A30000000000000000000000000000000000B0B0B000000000000000
      000000000000000000000000000000000000000000007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000004545
      4500CBCBCB0000000000000000005F5F5F00D6D6D60000000000000000007272
      7200D3D3D3000000000000000000000000000000000000000000767676000000
      00000000000000000000BEBEBE00BEBEBE008D8D8D00CBCBCB004B4B4B000000
      0000000000002E2E2E00F5B2F500000000000000000000000000D1D1D100EBE2
      EB0000000000C8C8C800CDCDCD00C8C8C800D6D6D6000000000000000000DADA
      DA00DADADA00000000000000000000000000FB83FB003B3B3B00000000002626
      26001C1C1C0000000000000000000000000035353500E3E3E300000000000000
      000000000000000000000000000000000000000000000000000000000000D3D3
      D300FB83FB000000000000000000D6D6D600000000000000000000000000D9D9
      D900000000000000000000000000000000000000000000000000000000008686
      860075757500C3C3C300A3A3A300161616000000000088888800E0E0E0008787
      870086868600F2C4F20000000000000000000000000000000000686868006262
      62006363630022222200C8C8C8002E2E2E0000000000C8C8C800474747005C5C
      5C00626262000D0D0D00FD5FFD0000000000D5D5D500000000002A2A2A00C4C4
      C400BEBEBE0022222200000000000000000016161600DADADA00000000000000
      00000000000000000000000000000000000000000000E7E7E700D5D5D500D5D5
      D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D500D5D5D500D5D5D50000000000000000000000000000000000000000000000
      0000DADADA006A6A6A000000000000000000000000000000000000000000BCBC
      BC0000000000000000000000000000000000000000000000000068686800D0D0
      D000C5C5C5006A6A6A00C8C8C8002E2E2E00E2E2E2000000000047474700C5C5
      C500D0D0D00032323200FD5FFD0000000000D4D4D40026262600C5C5C5000000
      000000000000BEBEBE001C1C1C00000000001C1C1C00DCDCDC00000000000000
      00000000000000000000000000000000000000000000B1B1B1000D0D0D000000
      00002E2E2E005C5C5C005D5D5D005D5D5D005D5D5D005D5D5D00565656000D0D
      0D00000000002A2A2A0000000000000000000000000000000000000000000000
      00000000000077777700000000000000000000000000000000002E2E2E00E5E5
      E50000000000000000000000000000000000000000000000000068686800DBDB
      DB00D5D5D5006A6A6A00DADADA00A3A3A300CDCDCD00EBE2EB0047474700D0D0
      D000DBDBDB0032323200FD5FFD000000000000000000CECECE00000000000000
      000000000000B6B6B60016161600000000005A5A5A0000000000000000000000
      0000000000000000000000000000000000000000000000000000ADADAD002A2A
      2A0051515100C0C0C000000000000000000000000000DCDCDC009B9B9B002222
      220083838300CECECE0000000000000000000000000000000000000000000000
      0000000000000000000079797900000000000000000076767600EED4EE000000
      00000000000000000000000000000000000000000000000000008B8B8B006868
      6800686868006868680000000000000000008D8D8D00D1D1D100797979006868
      6800686868006868680000000000000000000000000000000000000000000000
      0000B6B6B60016161600000000001C1C1C00C8C8C80000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F89D
      F800AFAFAF00474747007D7D7D00D0D0D000ACACAC002E2E2E0076767600D4D4
      D400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CDCDCD00BEBEBE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C7C7
      C700161616000000000066666600C8C8C8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A0A0A0004F4F4F0071717100E3E3E300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E7E7E700EED4EE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E2E2E2000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF00F00FF00F00000000E187E00700000000
      CFF3C663000000009FF98E71000000003FFC1C38000000003FFC3C3C00000000
      3FFC399C00000000700E3E3C00000000700E3C7C000000003FFC389C00000000
      3FFC3C1C000000003FFC1C38000000009FF98E7100000000CFF3C66300000000
      E187E00700000000F00FF00F00000000FFFFFFFFFFFFF00FFFFFFFFFFFFFE187
      FFFF8001FFC7CFF3C1C18001E7C39FF980C181F9E3C33E7C1C4981F9F1833E7C
      22089F99F89F3E7C01088109FC7F700E01008101FE3F700E22009FE1F91F3E7C
      1C7F81F9F1833E7C80FF81F9E3C33E7CC1FF8001E7C39FF9FFFF8001FFC7CFF3
      FFFFFFFFFFFFE187FFFFFFFFFFFFF00FFFFFFFFFFE7FFFFFFFFFE007F81FF81F
      C003C003F00FF00FC003C003E0C7E3C7C843C003E0E7CE73C003C003C0E38E71
      C003C003C0F39FF9E007C423C0F39E79FFCFC003CF039E79FFCFC003CF039E79
      FFFFC003CF038E71FF07E003CF03CE63FE03F003C303E3C7FF07F803E007F00F
      FFFFFC07F81FF81FFFFFFFFFFEFFFFFFFFFFF81FFFFFFFFFFFFFF81FFFFFFFFF
      C003FFFFF81FC0038001F00FF00FC0038001F00FE7C7CFF38001F00FCFE3CFF3
      8001F3CFCFF3CFF38001F3CFCE73C0038081F3CFCE7380018421F3CFCE739249
      8001F3CFC66392498001F3CFE66792418001F00FF66FC003C003F00FFE7FC003
      FFFFF00FFFFFFFFFFFFFF81FFFFFFFFFFFFFFFFFFFFFFFFFF81FF81FFFFFFFFF
      F00FF00FE0FFFFFFE007E007C07FFFFFC003C003C047F00F82018421C047F00F
      85018241C043F00F88818181C043F00F80418181C07FF00F80218241C041F00F
      80118401C041F00FC003C003803FF00FE007E007803FFFFFF00FF00FF1FFFFFF
      F81FF81FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF803FFFFFFFFFFFFF003FFFFFFFF
      FFFFF3F1FFFFC003FFFFF3F1FBFF8001FFFF93F1F8FF9FF981F993F1F83F9FF9
      81F193F1F81F9FF983E193F1F8079FF9818393F1F80F8001800793F1F81F8001
      9C1F93F1F87F8001FFFF9003F8FF9FF9FFFF9003FBFF8001FFFF801FFFFFC003
      FFFF801FFFFFFFFFFFFFC01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFF7C003C003FFFFFFE38001C003FFFFFFC78001C183FFFFF00F8FF1C1839F81
      E01F8FF1C1838F81C71F8FF1C0038FC1CF9F8FF1C003C181CF9F8001C003E001
      CF9F8001CF83F839CF1F8003CFC3FFFFE03F80FFC007FFFFF07FC1FFC00FFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF81F8001FFFF
      E003E0070000FFFF8001E3C38001F00F0000CE73C003E0070180CE71C003C383
      03C09E79E007840101809E79E007042001819E79F00F042181839FF9F81F8001
      C007CE71F81FC183F00FC7F3FC3FE007F81FE3C3FC3FF00FFE7FF00FFE7FFFFF
      FFFFFC3FFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3FFFFFC3FE3F3FFE1C003
      F81FC321FFC08001F80FC223FF818003F00FC247FF03E6EFC003C271FE07E667
      C001E411E00FE6678001E003C01FE6678001C867803FE667C001C867003FE6EF
      E003C001003F8003F00FC041183F8003F80FC001B87FC383FC1FC303F07FE00F
      FE7FFFFFE0FFFC3FFFFFFFFFF3FFFEFF00000000000000000000000000000000
      000000000000}
  end
end
