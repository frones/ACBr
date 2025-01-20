object frPagamentosAPITeste: TfrPagamentosAPITeste
  Left = 200
  Top = 166
  Width = 920
  Height = 586
  Caption = 'ACBrBaaS - Pagamento em Lote'
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
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 904
    Height = 547
    ActivePage = tsConfig
    Align = alClient
    Images = ImageList1
    TabHeight = 25
    TabOrder = 0
    TabWidth = 250
    object tsConfig: TTabSheet
      Caption = 'Configura'#231#227'o'
      ImageIndex = 2
      object pgConfig: TPageControl
        Left = 0
        Top = 0
        Width = 896
        Height = 465
        ActivePage = tsPagamentosAPI
        Align = alClient
        Images = ImageList1
        TabHeight = 25
        TabOrder = 0
        TabWidth = 246
        object tsPagamentosAPI: TTabSheet
          Caption = 'PagamentosAPI'
          ImageIndex = 2
          object gbProxy: TGroupBox
            Left = 128
            Top = 178
            Width = 280
            Height = 117
            Caption = 'Proxy'
            TabOrder = 0
            object pnProxy: TPanel
              Left = 2
              Top = 15
              Width = 276
              Height = 100
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                276
                100)
              object lbProxyHost: TLabel
                Left = 16
                Top = 0
                Width = 22
                Height = 13
                Caption = 'Host'
                Color = clBtnFace
                ParentColor = False
              end
              object lbProxyPorta: TLabel
                Left = 168
                Top = 0
                Width = 25
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Porta'
                Color = clBtnFace
                ParentColor = False
              end
              object lbProxyUsuario: TLabel
                Left = 16
                Top = 44
                Width = 36
                Height = 13
                Caption = 'Usu'#225'rio'
                Color = clBtnFace
                ParentColor = False
              end
              object lbProxySenha: TLabel
                Left = 168
                Top = 44
                Width = 31
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Senha'
                Color = clBtnFace
                ParentColor = False
              end
              object sbVerSenhaProxy: TSpeedButton
                Left = 244
                Top = 60
                Width = 23
                Height = 23
                AllowAllUp = True
                Anchors = [akTop, akRight]
                GroupIndex = 1
                Flat = True
              end
              object edProxyHost: TEdit
                Left = 16
                Top = 16
                Width = 138
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
              object edProxyUsuario: TEdit
                Left = 16
                Top = 60
                Width = 138
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 1
              end
              object edProxySenha: TEdit
                Left = 168
                Top = 60
                Width = 74
                Height = 21
                Anchors = [akTop, akRight]
                PasswordChar = '*'
                TabOrder = 2
              end
              object seProxyPorta: TSpinEdit
                Left = 168
                Top = 16
                Width = 74
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
            Left = 423
            Top = 178
            Width = 265
            Height = 117
            Caption = 'Log'
            TabOrder = 1
            object pnLog: TPanel
              Left = 2
              Top = 15
              Width = 261
              Height = 100
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                261
                100)
              object lbLogArquivo: TLabel
                Left = 16
                Top = 0
                Width = 36
                Height = 13
                Caption = 'Arquivo'
                Color = clBtnFace
                ParentColor = False
              end
              object lbLogNivel: TLabel
                Left = 16
                Top = 44
                Width = 26
                Height = 13
                Caption = 'N'#237'vel'
                Color = clBtnFace
                ParentColor = False
              end
              object sbArqLog: TSpeedButton
                Left = 225
                Top = 16
                Width = 24
                Height = 23
                Hint = 'Abrir Arquivo de Log'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                ParentShowHint = False
                ShowHint = True
              end
              object edArqLog: TEdit
                Left = 16
                Top = 16
                Width = 208
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
              object cbNivelLog: TComboBox
                Left = 16
                Top = 60
                Width = 208
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
          object gbBanco: TGroupBox
            Left = 128
            Top = 96
            Width = 560
            Height = 74
            Caption = 'Banco'
            TabOrder = 2
            object pnBanco: TPanel
              Left = 2
              Top = 15
              Width = 556
              Height = 57
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                556
                57)
              object Label13: TLabel
                Left = 16
                Top = 3
                Width = 58
                Height = 13
                Caption = 'Banco Atual'
                Color = clBtnFace
                ParentColor = False
              end
              object lbAmbiente: TLabel
                Left = 224
                Top = 3
                Width = 44
                Height = 13
                Caption = 'Ambiente'
                Color = clBtnFace
                ParentColor = False
              end
              object lbTimeout: TLabel
                Left = 416
                Top = 3
                Width = 38
                Height = 13
                Caption = 'Timeout'
                Color = clBtnFace
                ParentColor = False
              end
              object imErroBanco: TImage
                Left = 190
                Top = 23
                Width = 16
                Height = 16
                Anchors = [akTop, akRight]
                Visible = False
              end
              object cbBancoAtual: TComboBox
                Left = 16
                Top = 19
                Width = 174
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 0
                OnChange = cbBancoAtualChange
              end
              object cbAmbiente: TComboBox
                Left = 216
                Top = 19
                Width = 178
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 1
              end
              object seTimeout: TSpinEdit
                Left = 416
                Top = 19
                Width = 113
                Height = 22
                Increment = 10
                MaxValue = 999999
                MinValue = 0
                TabOrder = 2
                Value = 0
              end
            end
          end
        end
        object tsBancos: TTabSheet
          Caption = 'Bancos'
          ImageIndex = 3
          object pgBancos: TPageControl
            Left = 0
            Top = 0
            Width = 888
            Height = 430
            ActivePage = tsBB
            Align = alClient
            TabHeight = 25
            TabOrder = 0
            TabWidth = 120
            object tsBB: TTabSheet
              Caption = 'Banco do Brasil'
              object pConfPSPBB: TPanel
                Left = 24
                Top = 21
                Width = 792
                Height = 353
                BevelOuter = bvNone
                TabOrder = 0
                object lbBBClientID: TLabel
                  Left = 28
                  Top = 14
                  Width = 40
                  Height = 13
                  Caption = 'Client ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBClientSecret: TLabel
                  Left = 28
                  Top = 69
                  Width = 60
                  Height = 13
                  Caption = 'Client Secret'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBDevAppKey: TLabel
                  Left = 28
                  Top = 125
                  Width = 125
                  Height = 13
                  Caption = 'Developer Application Key'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edBBClientID: TEdit
                  Left = 28
                  Top = 31
                  Width = 748
                  Height = 21
                  TabOrder = 0
                end
                object edBBClientSecret: TEdit
                  Left = 28
                  Top = 87
                  Width = 748
                  Height = 21
                  TabOrder = 1
                end
                object edBBDevAppKey: TEdit
                  Left = 28
                  Top = 143
                  Width = 516
                  Height = 21
                  TabOrder = 2
                end
                object rgBBTipoCertificado: TRadioGroup
                  Left = 564
                  Top = 124
                  Width = 212
                  Height = 42
                  Caption = 'Tipo Certificado'
                  Columns = 2
                  ItemIndex = 0
                  Items.Strings = (
                    'PFX'
                    'Chave/Certificado')
                  TabOrder = 3
                end
                object pnBBCertificados: TPanel
                  Left = 0
                  Top = 183
                  Width = 792
                  Height = 170
                  Align = alBottom
                  BevelOuter = bvNone
                  TabOrder = 4
                  object pgBBCertificados: TPageControl
                    Left = 0
                    Top = 0
                    Width = 792
                    Height = 170
                    ActivePage = tsBBChaveECertificado
                    Align = alClient
                    TabOrder = 0
                    Visible = False
                    object tsBBPFX: TTabSheet
                      Caption = 'PFX'
                      object pnBBPFX: TPanel
                        Left = 0
                        Top = 0
                        Width = 784
                        Height = 142
                        Align = alClient
                        BevelOuter = bvNone
                        TabOrder = 0
                        DesignSize = (
                          784
                          142)
                        object imBBErroPFX: TImage
                          Left = 11
                          Top = 35
                          Width = 16
                          Height = 16
                          Visible = False
                        end
                        object lbBBArqPFX: TLabel
                          Left = 27
                          Top = 15
                          Width = 59
                          Height = 13
                          Caption = 'Arquivo PFX'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object lbBBErroPFX: TLabel
                          Left = 27
                          Top = 56
                          Width = 108
                          Height = 13
                          Caption = 'lbBBErroChavePrivada'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object sbBBAcharPFX: TSpeedButton
                          Left = 473
                          Top = 32
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
                          OnClick = sbBBAcharPFXClick
                        end
                        object lbBBSenhaPFX: TLabel
                          Left = 520
                          Top = 15
                          Width = 84
                          Height = 13
                          Caption = 'Senha Certificado'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object btBBVerSenhaPFX: TSpeedButton
                          Left = 744
                          Top = 32
                          Width = 23
                          Height = 23
                          AllowAllUp = True
                          Anchors = [akTop, akRight]
                          GroupIndex = 1
                          Flat = True
                        end
                        object edBBArqPFX: TEdit
                          Left = 27
                          Top = 32
                          Width = 481
                          Height = 23
                          Anchors = [akLeft, akTop, akRight]
                          AutoSize = False
                          TabOrder = 0
                          OnExit = edBBArqPFXExit
                        end
                        object edBBSenhaPFX: TEdit
                          Left = 520
                          Top = 32
                          Width = 184
                          Height = 23
                          PasswordChar = '*'
                          TabOrder = 1
                        end
                      end
                    end
                    object tsBBChaveECertificado: TTabSheet
                      Caption = 'Chave Privada/Certificado'
                      object pnBBChaveECert: TPanel
                        Left = 0
                        Top = 0
                        Width = 784
                        Height = 142
                        Align = alClient
                        BevelOuter = bvNone
                        TabOrder = 0
                        Visible = False
                        DesignSize = (
                          784
                          142)
                        object imBBErroChavePrivada: TImage
                          Left = 8
                          Top = 35
                          Width = 16
                          Height = 16
                          Visible = False
                        end
                        object lbBBArqChavePrivada: TLabel
                          Left = 24
                          Top = 15
                          Width = 109
                          Height = 13
                          Caption = 'Arquivo Chave Privada'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object lbBBErroChavePrivada: TLabel
                          Left = 24
                          Top = 56
                          Width = 108
                          Height = 13
                          Caption = 'lbBBErroChavePrivada'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object sbBBAcharChavePrivada: TSpeedButton
                          Left = 753
                          Top = 32
                          Width = 24
                          Height = 23
                          Anchors = [akTop, akRight]
                          Flat = True
                          Font.Charset = DEFAULT_CHARSET
                          Font.Color = clWindowText
                          Font.Height = -11
                          Font.Name = 'MS Sans Serif'
                          Font.Style = []
                          ParentFont = False
                          ParentShowHint = False
                          ShowHint = True
                          OnClick = sbBBAcharChavePrivadaClick
                        end
                        object sbBBAcharArqCertificado: TSpeedButton
                          Left = 753
                          Top = 96
                          Width = 24
                          Height = 23
                          Anchors = [akTop, akRight]
                          Flat = True
                          Font.Charset = DEFAULT_CHARSET
                          Font.Color = clWindowText
                          Font.Height = -11
                          Font.Name = 'MS Sans Serif'
                          Font.Style = []
                          ParentFont = False
                          ParentShowHint = False
                          ShowHint = True
                          OnClick = sbBBAcharArqCertificadoClick
                        end
                        object lbBBArqCertificado: TLabel
                          Left = 24
                          Top = 80
                          Width = 89
                          Height = 13
                          Caption = 'Arquivo Certificado'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object imBBErroCertificado: TImage
                          Left = 8
                          Top = 98
                          Width = 16
                          Height = 16
                          Visible = False
                        end
                        object lbBBErroCertificado: TLabel
                          Left = 24
                          Top = 118
                          Width = 91
                          Height = 13
                          Caption = 'lbBBErroCertificado'
                          Color = clBtnFace
                          ParentColor = False
                        end
                        object edBBArqChavePrivada: TEdit
                          Left = 24
                          Top = 32
                          Width = 724
                          Height = 23
                          Anchors = [akLeft, akTop, akRight]
                          AutoSize = False
                          TabOrder = 0
                          OnExit = edBBArqChavePrivadaExit
                        end
                        object edBBArqCertificado: TEdit
                          Left = 24
                          Top = 96
                          Width = 724
                          Height = 23
                          Anchors = [akLeft, akTop, akRight]
                          AutoSize = False
                          TabOrder = 1
                          OnExit = edBBArqCertificadoExit
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
      object pnConfigRodape: TPanel
        Left = 0
        Top = 465
        Width = 896
        Height = 47
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btSalvarParametros: TBitBtn
          Left = 19
          Top = 9
          Width = 139
          Height = 28
          Caption = 'Salvar Par'#226'metros'
          TabOrder = 0
          OnClick = btSalvarParametrosClick
        end
        object btLerParametros: TBitBtn
          Left = 171
          Top = 9
          Width = 139
          Height = 28
          Caption = 'Ler Par'#226'metros'
          TabOrder = 1
        end
      end
    end
    object tsTestes: TTabSheet
      Caption = 'Testes'
      ImageIndex = 14
      object pgTestes: TPageControl
        Left = 0
        Top = 0
        Width = 896
        Height = 512
        ActivePage = tsPagamentos
        Align = alClient
        Images = ImageList1
        TabHeight = 25
        TabOrder = 0
        TabWidth = 170
        object tsPagamentos: TTabSheet
          Caption = 'Pagamentos'
          ImageIndex = 29
        end
      end
    end
  end
  object ACBrPagamentosAPI1: TACBrPagamentosAPI
    Ambiente = eamHomologacao
    TimeOut = 0
    ArqLOG = '_log.txt'
    NivelLog = 0
    Left = 496
  end
  object ACBrPagamentosAPIBB1: TACBrPagamentosAPIBB
    ProxyPort = '8080'
    ContentsEncodingCompress = []
    NivelLog = 0
    Scopes = [pscLotesRequisicao, pscTransferenciasInfo, pscTransferenciasRequisicao, pscCancelarRequisicao, pscDevolvidosInfo, pscLotesInfo, pscGuiasSemCodigoBarrasInfo, pscPagamentosInfo, pscGuiasSemCodigoBarrasRequisicao, pscCodigoBarrasInfo, pscBoletosRequisicao, pscGuiasCodigoBarrasInfo, pscGuiasCodigoBarrasRequisicao, pscTransferenciasPixInfo, pscTransferenciasPixRequisicao, pscPixInfo, pscBoletosInfo, pscLancamentosInfo]
    Left = 552
  end
  object ImageList1: TImageList
    Left = 624
  end
  object ACBrOpenSSLUtils1: TACBrOpenSSLUtils
    Left = 664
  end
  object OpenDialog1: TOpenDialog
    Left = 704
  end
end
