object frPixCDMatera: TfrPixCDMatera
  Left = 269
  Top = 77
  Width = 1053
  Height = 818
  Caption = 'ACBrPIXCD Matera'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbUrlTEF: TLabel
    Left = 0
    Top = 759
    Width = 1037
    Height = 20
    Cursor = crHandPoint
    Align = alBottom
    Alignment = taCenter
    Caption = 'https://projetoacbr.com.br/tef'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = lbUrlTEFClick
  end
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 1037
    Height = 759
    ActivePage = tsFluxoPagto
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
        Width = 1029
        Height = 719
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
        object pnFluxoPagto: TPanel
          Left = 44
          Top = 17
          Width = 938
          Height = 547
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = True
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
            Height = 202
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 1
            object pnFluxoQRCode: TPanel
              Left = 672
              Top = 0
              Width = 266
              Height = 140
              Align = alClient
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 1
              object imFluxoQRCode: TImage
                Left = 0
                Top = 0
                Width = 266
                Height = 140
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
              Height = 140
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
                Height = 140
                Align = alClient
                BevelOuter = bvNone
                ParentColor = True
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
                Height = 140
                Align = alRight
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 1
              end
              object pnFluxoBotoesPrincipais: TPanel
                Left = 24
                Top = 0
                Width = 312
                Height = 140
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
                  Top = 62
                  Width = 312
                  Height = 30
                  Caption = 'Estornar Pagamento'
                  TabOrder = 1
                end
                object btFluxoNovaVenda: TBitBtn
                  Left = 0
                  Top = 92
                  Width = 312
                  Height = 30
                  Caption = 'Nova Venda'
                  TabOrder = 2
                end
                object btFluxoCancelarCobranca: TBitBtn
                  Left = 0
                  Top = 30
                  Width = 312
                  Height = 32
                  Caption = 'Cancelar Cobran'#231'a'
                  TabOrder = 3
                end
              end
              object pnFluxoBotoesRight1: TPanel
                Left = 0
                Top = 0
                Width = 24
                Height = 140
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
              Height = 140
              Align = alLeft
              BevelOuter = bvNone
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = []
              ParentColor = True
              ParentFont = False
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
                object edValor: TEdit
                  Left = 27
                  Top = 31
                  Width = 254
                  Height = 42
                  AutoSize = False
                  BiDiMode = bdLeftToRight
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -27
                  Font.Name = 'Tahoma'
                  Font.Style = []
                  ParentBiDiMode = False
                  ParentFont = False
                  TabOrder = 0
                  Text = '5,00'
                end
              end
            end
            object pnFluxoDiv7: TPanel
              Left = 0
              Top = 140
              Width = 938
              Height = 10
              Align = alBottom
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 3
            end
            object pnFluxoCopiaECola: TPanel
              Left = 0
              Top = 150
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
                Left = 887
                Top = 15
                Width = 26
                Height = 27
              end
              object edFluxoCopiaECola: TEdit
                Left = 26
                Top = 16
                Width = 862
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
          object gbFluxoCliente1: TGroupBox
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
            object pnFluxoCliente1: TPanel
              Left = 2
              Top = 17
              Width = 934
              Height = 78
              Align = alClient
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 0
              object lbFluxoClienteNome1: TLabel
                Left = 312
                Top = 13
                Width = 29
                Height = 15
                Caption = 'Valor'
                Color = clBtnFace
                ParentColor = False
              end
              object lbFluxoClienteDoc1: TLabel
                Left = 24
                Top = 13
                Width = 21
                Height = 15
                Caption = 'Info'
                Color = clBtnFace
                ParentColor = False
              end
              object edFluxoClienteNome1: TEdit
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
              object edFluxoClienteDoc1: TEdit
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
                Text = 'Cliente:'
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
        end
      end
    end
    object tsTestes: TTabSheet
      Caption = 'Opera'#231#245'es'
      ImageIndex = 14
      object Splitter2: TSplitter
        Left = 669
        Top = 0
        Width = 5
        Height = 719
        Align = alRight
      end
      object pcTestes: TPageControl
        Left = 0
        Top = 0
        Width = 669
        Height = 719
        ActivePage = tsGerarQRCodes
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 0
        TabWidth = 172
        object tsGerarQRCodes: TTabSheet
          Caption = 'Gerar QRCodes'
          ImageIndex = 1
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 661
            Height = 118
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lbChavePIXIncluirAccountId1: TLabel
              Left = 16
              Top = 16
              Width = 52
              Height = 13
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object lbQRCodeExternalID: TLabel
              Left = 16
              Top = 64
              Width = 52
              Height = 13
              Caption = 'External ID'
              Color = clBtnFace
              ParentColor = False
            end
            object lbItemPreco: TLabel
              Left = 344
              Top = 64
              Width = 24
              Height = 13
              Caption = 'Valor'
              Color = clBtnFace
              ParentColor = False
            end
            object btQRCodeGerarExternalID: TSpeedButton
              Left = 296
              Top = 79
              Width = 24
              Height = 23
              Flat = True
            end
            object lbChavePIXIncluir1: TLabel
              Left = 344
              Top = 16
              Width = 51
              Height = 13
              Caption = 'Chave PIX'
              Color = clBtnFace
              ParentColor = False
            end
            object lbItemPreco9: TLabel
              Left = 488
              Top = 64
              Width = 85
              Height = 13
              Caption = 'Tipo de Cobran'#231'a'
              Color = clBtnFace
              ParentColor = False
            end
            object edQRCodeAccountId: TEdit
              Left = 16
              Top = 31
              Width = 304
              Height = 21
              TabOrder = 0
            end
            object edQRCodeExternalID: TEdit
              Left = 16
              Top = 79
              Width = 280
              Height = 21
              TabOrder = 1
            end
            object edQRCodeValor: TEdit
              Left = 344
              Top = 79
              Width = 125
              Height = 21
              TabOrder = 2
              Text = '5,00'
            end
            object edQRCodeChavePIX: TEdit
              Left = 344
              Top = 31
              Width = 287
              Height = 21
              TabOrder = 3
            end
            object cbQRCodeTipoCobranca: TComboBox
              Left = 488
              Top = 79
              Width = 143
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 4
              Text = 'Normal'
              OnChange = cbQRCodeTipoCobrancaChange
              Items.Strings = (
                'Normal'
                'Com Vencimento')
            end
          end
          object pnQRCodeResult: TPanel
            Left = 0
            Top = 465
            Width = 661
            Height = 180
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
              Width = 60
              Height = 13
              Caption = 'Copia e Cola'
              Color = clBtnFace
              ParentColor = False
            end
            object edCobCopiaECola: TEdit
              Left = 240
              Top = 26
              Width = 363
              Height = 21
              TabOrder = 0
            end
            object Panel12: TPanel
              Left = 0
              Top = 0
              Width = 232
              Height = 180
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 1
              object imCobQRCode: TImage
                Left = 16
                Top = 4
                Width = 200
                Height = 175
                Proportional = True
                Stretch = True
              end
            end
          end
          object GroupBox6: TGroupBox
            Left = 0
            Top = 118
            Width = 661
            Height = 122
            Align = alTop
            Caption = 'Informa'#231#245'es Adicionais'
            TabOrder = 2
            object pnQRCodeInfoAdicionais: TPanel
              Left = 2
              Top = 15
              Width = 657
              Height = 105
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbChavePIXConsultar1: TLabel
                Left = 14
                Top = 3
                Width = 28
                Height = 13
                Caption = 'Nome'
                Color = clBtnFace
                ParentColor = False
              end
              object lbChavePIXConsultar2: TLabel
                Left = 256
                Top = 3
                Width = 46
                Height = 13
                Caption = 'Conte'#250'do'
                Color = clBtnFace
                ParentColor = False
              end
              object lbChavePIXConsultar3: TLabel
                Left = 14
                Top = 50
                Width = 80
                Height = 13
                Caption = 'CallBackAddress'
                Color = clBtnFace
                ParentColor = False
              end
              object lbChavePIXConsultar4: TLabel
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
            Top = 240
            Width = 661
            Height = 225
            Align = alTop
            Caption = 'Detalhes da Cobran'#231'a'
            TabOrder = 3
            Visible = False
            object Panel5: TPanel
              Left = 2
              Top = 15
              Width = 657
              Height = 208
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object lbContaCriarFundacao1: TLabel
                Left = 185
                Top = 103
                Width = 41
                Height = 13
                Caption = 'dueDate'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarCPF_CNPJ1: TLabel
                Left = 14
                Top = 3
                Width = 58
                Height = 13
                Caption = 'CPF / CNPJ'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarNomeCliente1: TLabel
                Left = 185
                Top = 3
                Width = 28
                Height = 13
                Caption = 'Name'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarRepresentanteLogradouro1: TLabel
                Left = 14
                Top = 53
                Width = 54
                Height = 13
                Caption = 'Logradouro'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarRepresentanteCidade1: TLabel
                Left = 185
                Top = 53
                Width = 33
                Height = 13
                Caption = 'Cidade'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarRepresentanteUF1: TLabel
                Left = 330
                Top = 53
                Width = 14
                Height = 13
                Caption = 'UF'
                Color = clBtnFace
                ParentColor = False
              end
              object lbContaCriarRepresentanteCEP1: TLabel
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
                Date = 45114.706492662040000000
                Time = 45114.706492662040000000
                MaxDate = 2958465.000000000000000000
                MinDate = -53780.000000000000000000
                TabOrder = 0
              end
              object GroupBox7: TGroupBox
                Left = 390
                Top = 0
                Width = 107
                Height = 208
                Align = alRight
                Caption = 'discounts'
                TabOrder = 1
                object Panel11: TPanel
                  Left = 2
                  Top = 15
                  Width = 103
                  Height = 191
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbItemPreco7: TLabel
                    Left = 10
                    Top = 55
                    Width = 48
                    Height = 13
                    Caption = 'valuePerc'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbItemPreco8: TLabel
                    Left = 10
                    Top = 3
                    Width = 38
                    Height = 13
                    Caption = 'modality'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarFundacao2: TLabel
                    Left = 10
                    Top = 108
                    Width = 21
                    Height = 13
                    Caption = 'date'
                    Color = clBtnFace
                    ParentColor = False
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
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
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
              object Panel6: TPanel
                Left = 497
                Top = 0
                Width = 160
                Height = 208
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 8
                object GroupBox4: TGroupBox
                  Left = 0
                  Top = 68
                  Width = 160
                  Height = 69
                  Align = alTop
                  Caption = 'fines'
                  TabOrder = 0
                  object Panel8: TPanel
                    Left = 2
                    Top = 15
                    Width = 156
                    Height = 52
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbItemPreco3: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbItemPreco4: TLabel
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
                object GroupBox5: TGroupBox
                  Left = 0
                  Top = 137
                  Width = 160
                  Height = 68
                  Align = alTop
                  Caption = 'reduction'
                  TabOrder = 1
                  object Panel9: TPanel
                    Left = 2
                    Top = 15
                    Width = 156
                    Height = 51
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbItemPreco5: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbItemPreco6: TLabel
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
                object GroupBox3: TGroupBox
                  Left = 0
                  Top = 0
                  Width = 160
                  Height = 68
                  Align = alTop
                  Caption = 'interests'
                  TabOrder = 2
                  object Panel7: TPanel
                    Left = 2
                    Top = 15
                    Width = 156
                    Height = 51
                    Align = alClient
                    BevelOuter = bvNone
                    TabOrder = 0
                    object lbItemPreco1: TLabel
                      Left = 10
                      Top = 3
                      Width = 48
                      Height = 13
                      Caption = 'valuePerc'
                      Color = clBtnFace
                      ParentColor = False
                    end
                    object lbItemPreco2: TLabel
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
          object Panel10: TPanel
            Left = 0
            Top = 645
            Width = 661
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
        object tsConsultarCobranca: TTabSheet
          Caption = 'Consultar Cobran'#231'a'
          ImageIndex = 8
          object lbConsultarCobTransactionID: TLabel
            Left = 328
            Top = 32
            Width = 70
            Height = 13
            Caption = 'Transaction ID'
            Color = clBtnFace
            ParentColor = False
          end
          object lbConsultarCobAccountID: TLabel
            Left = 16
            Top = 32
            Width = 52
            Height = 13
            Caption = 'Account Id'
            Color = clBtnFace
            ParentColor = False
          end
          object edConsultarCobTransactionID: TEdit
            Left = 328
            Top = 47
            Width = 290
            Height = 21
            TabOrder = 0
          end
          object edConsultarCobAccountID: TEdit
            Left = 16
            Top = 47
            Width = 290
            Height = 21
            TabOrder = 1
          end
          object btConsultarCob: TBitBtn
            Left = 16
            Top = 80
            Width = 130
            Height = 26
            Caption = 'Consultar'
            TabOrder = 2
            OnClick = btConsultarCobClick
          end
        end
      end
      object pnLogs1: TPanel
        Left = 674
        Top = 0
        Width = 355
        Height = 719
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
          Height = 672
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape1: TPanel
          Left = 1
          Top = 686
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
        Left = 1024
        Top = 0
        Width = 5
        Height = 719
        Align = alRight
      end
      object pcTestes1: TPageControl
        Left = 0
        Top = 0
        Width = 669
        Height = 719
        ActivePage = tsContaCriar
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 0
        TabWidth = 200
        object tsContaCriar: TTabSheet
          Caption = 'Criar Conta'
          ImageIndex = 31
          object lbContaCriarExternalID: TLabel
            Left = 15
            Top = 20
            Width = 81
            Height = 13
            Caption = 'External Identifier'
            Color = clBtnFace
            ParentColor = False
          end
          object btContaCriarExternalID: TSpeedButton
            Left = 256
            Top = 35
            Width = 24
            Height = 23
            Flat = True
            OnClick = btContaCriarExternalIDClick
          end
          object lbContaCriarTipoCliente: TLabel
            Left = 296
            Top = 20
            Width = 56
            Height = 13
            Caption = 'Tipo Cliente'
            Color = clBtnFace
            ParentColor = False
          end
          object lbContaCriarTipoConta: TLabel
            Left = 462
            Top = 20
            Width = 67
            Height = 13
            Caption = 'Tipo da Conta'
            Color = clBtnFace
            ParentColor = False
          end
          object lbContaCriarNomeCliente: TLabel
            Left = 15
            Top = 70
            Width = 63
            Height = 13
            Caption = 'Nome Cliente'
            Color = clBtnFace
            ParentColor = False
          end
          object lbContaCriarCPF_CNPJ: TLabel
            Left = 496
            Top = 64
            Width = 58
            Height = 13
            Caption = 'CPF / CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object lbContaCriarCelular: TLabel
            Left = 213
            Top = 70
            Width = 32
            Height = 13
            Caption = 'Celular'
            Color = clBtnFace
            ParentColor = False
          end
          object lbContaCriar: TLabel
            Left = 325
            Top = 70
            Width = 28
            Height = 13
            Caption = 'E-mail'
            Color = clBtnFace
            ParentColor = False
          end
          object edContaCriarExternalID: TEdit
            Left = 15
            Top = 35
            Width = 241
            Height = 21
            TabOrder = 0
          end
          object cbContaCriarTipoCliente: TComboBox
            Left = 295
            Top = 35
            Width = 152
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = cbContaCriarTipoClienteChange
          end
          object cbCriarContaTipoConta: TComboBox
            Left = 462
            Top = 35
            Width = 176
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
          end
          object edContaCriarNomeCliente: TEdit
            Left = 15
            Top = 85
            Width = 185
            Height = 21
            TabOrder = 3
          end
          object edContaCriarCPF_CNPJ: TEdit
            Left = 496
            Top = 85
            Width = 142
            Height = 21
            TabOrder = 4
            OnKeyPress = edOnlyNumbersKeyPress
          end
          object edContaCriarCelular: TEdit
            Left = 213
            Top = 85
            Width = 97
            Height = 21
            TabOrder = 5
            OnKeyPress = edOnlyNumbersKeyPress
          end
          object edContaCriarEmail: TEdit
            Left = 325
            Top = 85
            Width = 152
            Height = 21
            TabOrder = 6
          end
          object gbContaCriarEndereco: TGroupBox
            Left = 15
            Top = 122
            Width = 623
            Height = 120
            Caption = 'Endere'#231'o'
            TabOrder = 7
            object lbContaCriarCEP: TLabel
              Left = 48
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
              Left = 16
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
              TabOrder = 4
            end
            object edContaCriarComplemento: TEdit
              Left = 16
              Top = 65
              Width = 167
              Height = 21
              TabOrder = 3
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
          object gbDadosAdicionais: TGroupBox
            Left = 15
            Top = 255
            Width = 623
            Height = 258
            Caption = 'Dados Adicionais da Conta'
            TabOrder = 8
            object pcContaCriarDadosAdicionais: TPageControl
              Left = 2
              Top = 15
              Width = 619
              Height = 241
              ActivePage = tsContaCriarCorporate
              Align = alClient
              TabOrder = 0
              Visible = False
              object tsContaCriarCorporate: TTabSheet
                Caption = 'Corporate'
                object pnContaCriarCorporate: TPanel
                  Left = 0
                  Top = 0
                  Width = 611
                  Height = 213
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbContaCriarFundacao: TLabel
                    Left = 10
                    Top = 5
                    Width = 74
                    Height = 13
                    Caption = 'Data Funda'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarNomeEmpresa: TLabel
                    Left = 103
                    Top = 5
                    Width = 72
                    Height = 13
                    Caption = 'Nome Empresa'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCelular: TLabel
                    Left = 10
                    Top = 55
                    Width = 32
                    Height = 13
                    Caption = 'Celular'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentante: TLabel
                    Left = 103
                    Top = 55
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteMae: TLabel
                    Left = 242
                    Top = 55
                    Width = 67
                    Height = 13
                    Caption = 'Nome da M'#227'e'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCPF: TLabel
                    Left = 365
                    Top = 5
                    Width = 93
                    Height = 13
                    Caption = 'CPF Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteEmail: TLabel
                    Left = 365
                    Top = 55
                    Width = 25
                    Height = 13
                    Caption = 'Email'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarNascimento: TLabel
                    Left = 477
                    Top = 5
                    Width = 83
                    Height = 13
                    Caption = 'Nasc. Represent.'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCEP: TLabel
                    Left = 10
                    Top = 105
                    Width = 21
                    Height = 13
                    Caption = 'CEP'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteLogradouro: TLabel
                    Left = 103
                    Top = 105
                    Width = 54
                    Height = 13
                    Caption = 'Logradouro'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteNumero: TLabel
                    Left = 242
                    Top = 105
                    Width = 37
                    Height = 13
                    Caption = 'N'#250'mero'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteBairro: TLabel
                    Left = 314
                    Top = 105
                    Width = 27
                    Height = 13
                    Caption = 'Bairro'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteCidade: TLabel
                    Left = 427
                    Top = 105
                    Width = 33
                    Height = 13
                    Caption = 'Cidade'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteUF: TLabel
                    Left = 544
                    Top = 105
                    Width = 14
                    Height = 13
                    Caption = 'UF'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbContaCriarRepresentanteFoto: TLabel
                    Left = 10
                    Top = 155
                    Width = 21
                    Height = 13
                    Caption = 'Foto'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btContaCriarRepresentanteFoto: TSpeedButton
                    Left = 166
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
                    OnClick = btAcharArqCertificadoClick
                  end
                  object lbContaCriarRepresentanteRGFrente: TLabel
                    Left = 200
                    Top = 155
                    Width = 79
                    Height = 13
                    Caption = 'Foto RG (Frente)'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btContaCriarRepresentanteRGFotoFrente: TSpeedButton
                    Left = 360
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
                    OnClick = btAcharArqCertificadoClick
                  end
                  object lbContaCriarRepresentanteRGVerso: TLabel
                    Left = 394
                    Top = 155
                    Width = 76
                    Height = 13
                    Caption = 'Foto RG (Verso)'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btContaCriarRepresentanteRGFotoVerso: TSpeedButton
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
                    OnClick = btAcharArqCertificadoClick
                  end
                  object edContaCriarNomeEmpresa: TEdit
                    Left = 103
                    Top = 20
                    Width = 252
                    Height = 21
                    TabOrder = 1
                  end
                  object edContaCriarRepresentanteCelular: TEdit
                    Left = 10
                    Top = 70
                    Width = 83
                    Height = 21
                    TabOrder = 4
                  end
                  object edContaCriarRepresentanteNome: TEdit
                    Left = 103
                    Top = 70
                    Width = 130
                    Height = 21
                    TabOrder = 5
                  end
                  object edContaCriarRepresentanteMae: TEdit
                    Left = 242
                    Top = 70
                    Width = 113
                    Height = 21
                    TabOrder = 6
                  end
                  object edContaCriarRepresentanteCPF: TEdit
                    Left = 365
                    Top = 20
                    Width = 102
                    Height = 21
                    TabOrder = 2
                  end
                  object edContaCriarRepresentanteEmail: TEdit
                    Left = 365
                    Top = 70
                    Width = 232
                    Height = 21
                    TabOrder = 7
                  end
                  object edContaCriarFundacao: TDateTimePicker
                    Left = 10
                    Top = 20
                    Width = 83
                    Height = 23
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 0
                  end
                  object edContaCriarNascimento: TDateTimePicker
                    Left = 486
                    Top = 20
                    Width = 111
                    Height = 23
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 3
                  end
                  object edContaCriarRepresentanteCEP: TEdit
                    Left = 10
                    Top = 120
                    Width = 83
                    Height = 21
                    TabOrder = 8
                    OnChange = edContaCriarCEPChange
                    OnKeyPress = edOnlyNumbersKeyPress
                  end
                  object edContaCriarRepresentanteLogradouro: TEdit
                    Left = 103
                    Top = 120
                    Width = 130
                    Height = 21
                    TabOrder = 9
                  end
                  object edContaCriarRepresentanteNumero: TEdit
                    Left = 242
                    Top = 120
                    Width = 60
                    Height = 21
                    TabOrder = 10
                  end
                  object edContaCriarRepresentanteBairro: TEdit
                    Left = 314
                    Top = 120
                    Width = 103
                    Height = 21
                    TabOrder = 11
                  end
                  object edContaCriarRepresentanteCidade: TEdit
                    Left = 427
                    Top = 120
                    Width = 107
                    Height = 21
                    TabOrder = 12
                  end
                  object edContaCriarRepresentanteUF: TEdit
                    Left = 544
                    Top = 120
                    Width = 39
                    Height = 21
                    TabOrder = 13
                  end
                  object edContaCriarRepresentanteFoto: TEdit
                    Left = 10
                    Top = 170
                    Width = 155
                    Height = 23
                    AutoSize = False
                    TabOrder = 14
                    OnExit = edArqCertificadoExit
                  end
                  object edContaCriarRepresentanteRGFotoFrente: TEdit
                    Left = 200
                    Top = 170
                    Width = 158
                    Height = 23
                    AutoSize = False
                    TabOrder = 15
                    OnExit = edArqCertificadoExit
                  end
                  object edContaCriarRepresentanteRGFotoVerso: TEdit
                    Left = 396
                    Top = 170
                    Width = 172
                    Height = 23
                    AutoSize = False
                    TabOrder = 16
                    OnExit = edArqCertificadoExit
                  end
                end
              end
              object tsContaCriarPerson: TTabSheet
                Caption = 'Person'
                object pnContaCriarPerson: TPanel
                  Left = 0
                  Top = 0
                  Width = 611
                  Height = 213
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  object lbContaCriarRepresentante1: TLabel
                    Left = 16
                    Top = 16
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante2: TLabel
                    Left = 168
                    Top = 16
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante3: TLabel
                    Left = 312
                    Top = 16
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante4: TLabel
                    Left = 448
                    Top = 16
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante5: TLabel
                    Left = 16
                    Top = 72
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante6: TLabel
                    Left = 168
                    Top = 72
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante7: TLabel
                    Left = 312
                    Top = 72
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaFundacao1: TLabel
                    Left = 448
                    Top = 72
                    Width = 74
                    Height = 13
                    Caption = 'Data Funda'#231#227'o'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object lbCriarContaRepresentante8: TLabel
                    Left = 16
                    Top = 128
                    Width = 116
                    Height = 13
                    Caption = 'Nome do Representante'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object edContaCriarRepresentanteNome1: TEdit
                    Left = 16
                    Top = 31
                    Width = 130
                    Height = 23
                    TabOrder = 0
                  end
                  object edCriarContaRepresentanteNome2: TEdit
                    Left = 168
                    Top = 31
                    Width = 130
                    Height = 23
                    TabOrder = 1
                  end
                  object edCriarContaRepresentanteNome3: TEdit
                    Left = 312
                    Top = 31
                    Width = 130
                    Height = 23
                    TabOrder = 2
                  end
                  object edCriarContaRepresentanteNome4: TEdit
                    Left = 448
                    Top = 31
                    Width = 130
                    Height = 23
                    TabOrder = 3
                  end
                  object edCriarContaRepresentanteNome5: TEdit
                    Left = 16
                    Top = 87
                    Width = 130
                    Height = 23
                    TabOrder = 4
                  end
                  object edCriarContaRepresentanteNome6: TEdit
                    Left = 168
                    Top = 87
                    Width = 130
                    Height = 23
                    TabOrder = 5
                  end
                  object edCriarContaRepresentanteNome7: TEdit
                    Left = 312
                    Top = 87
                    Width = 130
                    Height = 23
                    TabOrder = 6
                  end
                  object edCriarContaFundacao1: TDateTimePicker
                    Left = 448
                    Top = 87
                    Width = 83
                    Height = 23
                    Date = 45114.706492662040000000
                    Time = 45114.706492662040000000
                    MaxDate = 2958465.000000000000000000
                    MinDate = -53780.000000000000000000
                    TabOrder = 7
                  end
                  object edCriarContaRepresentanteNome8: TEdit
                    Left = 16
                    Top = 143
                    Width = 130
                    Height = 23
                    TabOrder = 8
                  end
                end
              end
            end
          end
          object pnContaCriarRodape: TPanel
            Left = 0
            Top = 646
            Width = 661
            Height = 33
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 9
            DesignSize = (
              661
              33)
            object btContaCriar: TBitBtn
              Left = 542
              Top = 3
              Width = 112
              Height = 26
              Anchors = [akTop]
              Caption = 'Criar'
              TabOrder = 2
              OnClick = btContaCriarClick
            end
            object btContaCriarPreencherDados: TBitBtn
              Left = 185
              Top = 3
              Width = 186
              Height = 26
              Anchors = [akTop]
              Caption = 'Preencher(Dados Fict'#237'cios)'
              TabOrder = 0
              OnClick = btContaCriarPreencherDadosClick
            end
            object btContaCriarLimparDados: TBitBtn
              Left = 387
              Top = 3
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
            Width = 661
            Height = 679
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
            Width = 661
            Height = 679
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              661
              679)
            object lbContaInativarAccountId: TLabel
              Left = 15
              Top = 20
              Width = 52
              Height = 13
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object btContaInativar: TBitBtn
              Left = 530
              Top = 32
              Width = 112
              Height = 26
              Anchors = [akTop]
              Caption = 'Inativar'
              TabOrder = 0
              OnClick = btContaInativarClick
            end
            object edContaInativarAccountId: TEdit
              Left = 15
              Top = 35
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
            Width = 661
            Height = 679
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object gbChavePIXIncluir: TGroupBox
              Left = 16
              Top = 20
              Width = 592
              Height = 116
              Caption = 'Incluir Chave PIX'
              TabOrder = 0
              object pnChavePIXIncluir: TPanel
                Left = 2
                Top = 15
                Width = 588
                Height = 99
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  99)
                object lbChavePIXIncluir: TLabel
                  Left = 228
                  Top = 16
                  Width = 51
                  Height = 13
                  Caption = 'Chave PIX'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXIncluirAccountId: TLabel
                  Left = 15
                  Top = 16
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btChavePIXCriar: TSpeedButton
                  Left = 416
                  Top = 31
                  Width = 24
                  Height = 23
                  Flat = True
                  OnClick = btChavePIXCriarClick
                end
                object edChavePIXIncluir: TEdit
                  Left = 228
                  Top = 31
                  Width = 188
                  Height = 23
                  TabOrder = 0
                end
                object btChavePIXIncluir: TBitBtn
                  Left = 474
                  Top = 32
                  Width = 112
                  Height = 26
                  Anchors = [akTop]
                  Caption = 'Incluir'
                  TabOrder = 1
                  OnClick = btChavePIXIncluirClick
                end
                object edChavePIXIncluirAccountId: TEdit
                  Left = 15
                  Top = 31
                  Width = 200
                  Height = 23
                  TabOrder = 2
                end
              end
            end
            object gbChavePIXConsultar: TGroupBox
              Left = 16
              Top = 160
              Width = 592
              Height = 116
              Caption = 'Consultar Chaves PIX'
              TabOrder = 1
              object pnChavesPIXConsultar: TPanel
                Left = 2
                Top = 15
                Width = 588
                Height = 99
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  99)
                object lbChavePIXConsultar: TLabel
                  Left = 15
                  Top = 20
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edChavePIXConsultar: TEdit
                  Left = 15
                  Top = 35
                  Width = 425
                  Height = 23
                  TabOrder = 0
                end
                object btChavePIXConsultar: TBitBtn
                  Left = 474
                  Top = 32
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
              Left = 16
              Top = 296
              Width = 592
              Height = 116
              Caption = 'Excluir Chave PIX'
              TabOrder = 2
              object pnChavePIXExcluir: TPanel
                Left = 2
                Top = 15
                Width = 588
                Height = 99
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  588
                  99)
                object lbChavePIXExcluirAccountId: TLabel
                  Left = 15
                  Top = 16
                  Width = 52
                  Height = 13
                  Caption = 'Account Id'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbChavePIXExcluir: TLabel
                  Left = 228
                  Top = 16
                  Width = 51
                  Height = 13
                  Caption = 'Chave PIX'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btChavePIXExcluir: TBitBtn
                  Left = 474
                  Top = 32
                  Width = 112
                  Height = 26
                  Anchors = [akTop]
                  Caption = 'Excluir'
                  TabOrder = 0
                  OnClick = btChavePIXExcluirClick
                end
                object edChavePIXExcluirAccountId: TEdit
                  Left = 15
                  Top = 31
                  Width = 200
                  Height = 23
                  TabOrder = 1
                end
                object edChavePIXExcluir: TEdit
                  Left = 228
                  Top = 31
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
        Left = 669
        Top = 0
        Width = 355
        Height = 719
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
          Height = 672
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object pnLogsRodape: TPanel
          Left = 1
          Top = 686
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
        Top = 682
        Width = 1029
        Height = 37
        Align = alBottom
        TabOrder = 0
        object btSalvarParametros: TBitBtn
          Left = 16
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
        end
      end
      object PageControl1: TPageControl
        Left = 0
        Top = 0
        Width = 1029
        Height = 682
        ActivePage = tsMatera
        Align = alClient
        Images = ImageList1
        TabHeight = 30
        TabOrder = 1
        TabWidth = 172
        object tsPIX: TTabSheet
          Caption = 'PIX'
          DesignSize = (
            1021
            642)
          object pConfPIX: TPanel
            Left = 266
            Top = 199
            Width = 592
            Height = 248
            Anchors = []
            BevelOuter = bvSpace
            TabOrder = 0
            object Panel2: TPanel
              Left = 1
              Top = 81
              Width = 590
              Height = 133
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object gbProxy: TGroupBox
                Left = 0
                Top = 0
                Width = 300
                Height = 133
                Align = alLeft
                Caption = 'Proxy'
                TabOrder = 0
                object pnProxy: TPanel
                  Left = 2
                  Top = 15
                  Width = 296
                  Height = 116
                  Align = alClient
                  BevelOuter = bvNone
                  TabOrder = 0
                  DesignSize = (
                    296
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
                    Left = 188
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
                    Left = 188
                    Top = 50
                    Width = 31
                    Height = 13
                    Anchors = [akTop, akRight]
                    Caption = 'Senha'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object btProxyVerSenha: TSpeedButton
                    Left = 264
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
                    Width = 164
                    Height = 21
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 0
                  end
                  object edProxyUsuario: TEdit
                    Left = 10
                    Top = 65
                    Width = 164
                    Height = 21
                    Anchors = [akLeft, akTop, akRight]
                    TabOrder = 1
                  end
                  object edProxySenha: TEdit
                    Left = 188
                    Top = 65
                    Width = 74
                    Height = 21
                    Anchors = [akTop, akRight]
                    PasswordChar = '*'
                    TabOrder = 2
                  end
                  object edProxyPorta: TSpinEdit
                    Left = 188
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
                Left = 300
                Top = 0
                Width = 290
                Height = 133
                Align = alClient
                Caption = 'Log'
                TabOrder = 1
                object pnLog: TPanel
                  Left = 2
                  Top = 15
                  Width = 286
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
            object Panel1: TPanel
              Left = 1
              Top = 1
              Width = 590
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
                Width = 286
                Height = 80
                Align = alClient
                Caption = 'Cobran'#231'a'
                TabOrder = 1
                object pnCobranca: TPanel
                  Left = 2
                  Top = 15
                  Width = 282
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
          end
        end
        object tsMatera: TTabSheet
          Caption = 'PSP Matera'
          ImageIndex = 34
          object pnPSPMatera: TPanel
            Left = 0
            Top = 0
            Width = 1021
            Height = 642
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbClientID: TLabel
              Left = 224
              Top = 88
              Width = 40
              Height = 13
              Caption = 'Client ID'
              Color = clBtnFace
              ParentColor = False
            end
            object lbClientSecret: TLabel
              Left = 456
              Top = 88
              Width = 60
              Height = 13
              Caption = 'Client Secret'
              Color = clBtnFace
              ParentColor = False
            end
            object lbErroCertificado: TLabel
              Left = 224
              Top = 312
              Width = 77
              Height = 13
              Caption = 'lbErroCertificado'
              Color = clBtnFace
              ParentColor = False
            end
            object lbArqCertificado: TLabel
              Left = 224
              Top = 272
              Width = 89
              Height = 13
              Caption = 'Arquivo Certificado'
              Color = clBtnFace
              ParentColor = False
            end
            object imErroCertificado: TImage
              Left = 352
              Top = 271
              Width = 16
              Height = 16
              Visible = False
            end
            object btAcharArqCertificado: TSpeedButton
              Left = 801
              Top = 288
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
              Left = 801
              Top = 216
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
              Left = 224
              Top = 200
              Width = 109
              Height = 13
              Caption = 'Arquivo Chave Privada'
              Color = clBtnFace
              ParentColor = False
            end
            object lbErroChavePrivada: TLabel
              Left = 224
              Top = 240
              Width = 94
              Height = 13
              Caption = 'lbErroChavePrivada'
              Color = clBtnFace
              ParentColor = False
            end
            object imErroChavePrivada: TImage
              Left = 352
              Top = 199
              Width = 16
              Height = 16
              Visible = False
            end
            object lbSecretKey: TLabel
              Left = 224
              Top = 144
              Width = 52
              Height = 13
              Caption = 'Secret Key'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaConsultarAccountId1: TLabel
              Left = 224
              Top = 344
              Width = 52
              Height = 13
              Caption = 'Account Id'
              Color = clBtnFace
              ParentColor = False
            end
            object lbContaConsultarAccountId2: TLabel
              Left = 224
              Top = 397
              Width = 51
              Height = 13
              Caption = 'Chave PIX'
              Color = clBtnFace
              ParentColor = False
            end
            object edPSPClientID: TEdit
              Left = 224
              Top = 103
              Width = 216
              Height = 21
              PasswordChar = '*'
              TabOrder = 0
            end
            object edPSPClientSecret: TEdit
              Left = 456
              Top = 103
              Width = 369
              Height = 21
              PasswordChar = '*'
              TabOrder = 1
            end
            object edArqCertificado: TEdit
              Left = 224
              Top = 288
              Width = 576
              Height = 23
              AutoSize = False
              TabOrder = 2
              OnExit = edArqCertificadoExit
            end
            object edArqChavePrivada: TEdit
              Left = 224
              Top = 216
              Width = 576
              Height = 23
              AutoSize = False
              TabOrder = 3
              OnExit = edArqChavePrivadaExit
            end
            object edPSPSecretKey: TEdit
              Left = 224
              Top = 159
              Width = 601
              Height = 21
              PasswordChar = '*'
              TabOrder = 4
            end
            object edAccountId: TEdit
              Left = 224
              Top = 359
              Width = 601
              Height = 21
              TabOrder = 5
            end
            object edChavePIX: TEdit
              Left = 224
              Top = 417
              Width = 601
              Height = 21
              TabOrder = 6
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
end
