object frPrincipal: TfrPrincipal
  Left = 1042
  Top = 224
  Width = 721
  Height = 627
  Caption = 'Aplica'#231#227'o de Demonstra'#231#227'o ACBrExtratoAPI'
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
    Width = 705
    Height = 588
    ActivePage = tsConsulta
    Align = alClient
    TabOrder = 0
    object tsConsulta: TTabSheet
      Caption = 'Extrato'
      object Label5: TLabel
        Left = 20
        Top = 20
        Width = 39
        Height = 13
        Caption = 'Ag'#234'ncia'
        Color = clBtnFace
        ParentColor = False
      end
      object Label6: TLabel
        Left = 201
        Top = 20
        Width = 28
        Height = 13
        Caption = 'Conta'
        Color = clBtnFace
        ParentColor = False
      end
      object lbInicio: TLabel
        Left = 376
        Top = 20
        Width = 27
        Height = 13
        Caption = 'In'#237'cio'
        Color = clBtnFace
        ParentColor = False
      end
      object lbFim: TLabel
        Left = 494
        Top = 20
        Width = 16
        Height = 13
        Caption = 'Fim'
        Color = clBtnFace
        ParentColor = False
      end
      object btConsultarExtrato: TButton
        Left = 20
        Top = 72
        Width = 168
        Height = 33
        Caption = 'Consultar Extrato'
        TabOrder = 0
        OnClick = btConsultarExtratoClick
      end
      object edAgencia: TEdit
        Left = 20
        Top = 35
        Width = 168
        Height = 21
        TabOrder = 1
      end
      object edConta: TEdit
        Left = 201
        Top = 35
        Width = 160
        Height = 21
        TabOrder = 2
      end
      object gdLancamentos: TStringGrid
        Left = 20
        Top = 120
        Width = 655
        Height = 419
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        TabOrder = 3
        ColWidths = (
          64
          107
          101
          249
          93)
      end
      object edInicio: TDateTimePicker
        Left = 376
        Top = 35
        Width = 103
        Height = 23
        Date = 45544.501780219910000000
        Time = 45544.501780219910000000
        TabOrder = 4
      end
      object edFim: TDateTimePicker
        Left = 494
        Top = 35
        Width = 103
        Height = 23
        Date = 45544.501780219910000000
        Time = 45544.501780219910000000
        TabOrder = 5
      end
    end
    object tsConfig: TTabSheet
      Caption = 'Configura'#231#227'o'
      object pnConfig: TPanel
        Left = 0
        Top = 0
        Width = 697
        Height = 178
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 20
        TabOrder = 0
        object gbConfigGeral: TGroupBox
          Left = 20
          Top = 20
          Width = 208
          Height = 138
          Align = alLeft
          Caption = 'Geral'
          TabOrder = 0
          object pnConfigGeral: TPanel
            Left = 2
            Top = 15
            Width = 204
            Height = 121
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbConfigGeralBanco: TLabel
              Left = 15
              Top = 10
              Width = 31
              Height = 13
              Caption = 'Banco'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConfigGeralAmbiente: TLabel
              Left = 15
              Top = 60
              Width = 44
              Height = 13
              Caption = 'Ambiente'
              Color = clBtnFace
              ParentColor = False
            end
            object cbConfigGeralBanco: TComboBox
              Left = 15
              Top = 25
              Width = 170
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
              OnChange = cbConfigGeralBancoChange
            end
            object cbConfigGeralAmbiente: TComboBox
              Left = 15
              Top = 75
              Width = 170
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 1
              OnChange = cbConfigGeralAmbienteChange
            end
          end
        end
        object gbConfigProxy: TGroupBox
          Left = 228
          Top = 20
          Width = 245
          Height = 138
          Align = alLeft
          Caption = 'Proxy'
          TabOrder = 1
          object pnConfigProxy: TPanel
            Left = 2
            Top = 15
            Width = 241
            Height = 121
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              241
              121)
            object lbConfigProxyHost: TLabel
              Left = 15
              Top = 10
              Width = 22
              Height = 13
              Caption = 'Host'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConfigProxyPorta: TLabel
              Left = 140
              Top = 10
              Width = 25
              Height = 13
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConfigProxyUsuario: TLabel
              Left = 15
              Top = 60
              Width = 36
              Height = 13
              Caption = 'Usu'#225'rio'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConfigProxySenha: TLabel
              Left = 140
              Top = 60
              Width = 31
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object btConfigProxyVerSenha: TSpeedButton
              Left = 202
              Top = 75
              Width = 23
              Height = 23
              AllowAllUp = True
              Anchors = [akTop, akRight]
              GroupIndex = 1
              OnClick = btConfigProxyVerSenhaClick
            end
            object edConfigProxyHost: TEdit
              Left = 15
              Top = 25
              Width = 111
              Height = 21
              TabOrder = 0
            end
            object edConfigProxyUsuario: TEdit
              Left = 15
              Top = 75
              Width = 111
              Height = 21
              TabOrder = 1
            end
            object edConfigProxySenha: TEdit
              Left = 140
              Top = 75
              Width = 63
              Height = 21
              PasswordChar = '*'
              TabOrder = 2
            end
            object edConfigProxyPorta: TSpinEdit
              Left = 140
              Top = 25
              Width = 85
              Height = 22
              MaxValue = 999999
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
          end
        end
        object gbConfigLog: TGroupBox
          Left = 473
          Top = 20
          Width = 204
          Height = 138
          Align = alClient
          Caption = 'Log'
          TabOrder = 2
          object pnConfigLog: TPanel
            Left = 2
            Top = 15
            Width = 200
            Height = 121
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbConfigLogArquivo: TLabel
              Left = 15
              Top = 10
              Width = 36
              Height = 13
              Caption = 'Arquivo'
              Color = clBtnFace
              ParentColor = False
            end
            object lbConfigLogNivel: TLabel
              Left = 15
              Top = 60
              Width = 26
              Height = 13
              Caption = 'N'#237'vel'
              Color = clBtnFace
              ParentColor = False
            end
            object btConfigLogArquivo: TSpeedButton
              Left = 161
              Top = 25
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
              OnClick = btConfigLogArquivoClick
            end
            object edConfigLogArquivo: TEdit
              Left = 15
              Top = 25
              Width = 147
              Height = 21
              TabOrder = 0
            end
            object cbConfigLogNivel: TComboBox
              Left = 15
              Top = 75
              Width = 170
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
      object pnConfigBancos: TPanel
        Left = 0
        Top = 178
        Width = 697
        Height = 320
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object pgConfigBancos: TPageControl
          Left = 0
          Top = 0
          Width = 697
          Height = 320
          ActivePage = tsConfigBB
          Align = alClient
          TabOrder = 0
          Visible = False
          object tsConfigBB: TTabSheet
            Caption = 'tsConfigBB'
            object gbConfigBB: TGroupBox
              Left = 0
              Top = 0
              Width = 689
              Height = 292
              Align = alClient
              Caption = 'Banco do Brasil'
              TabOrder = 0
              Visible = False
              object pnConfigBB: TPanel
                Left = 2
                Top = 15
                Width = 685
                Height = 275
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  685
                  275)
                object lbBBMCITeste: TLabel
                  Left = 330
                  Top = 10
                  Width = 183
                  Height = 13
                  Caption = 'x-br-com-bb-ipa-miteste (Homologa'#231#227'o)'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBDevAppKey: TLabel
                  Left = 15
                  Top = 10
                  Width = 125
                  Height = 13
                  Caption = 'Developer Application Key'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBClientID: TLabel
                  Left = 15
                  Top = 60
                  Width = 40
                  Height = 13
                  Caption = 'Client ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBClientSecret: TLabel
                  Left = 15
                  Top = 110
                  Width = 60
                  Height = 13
                  Caption = 'Client Secret'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBChavePrivada: TLabel
                  Left = 15
                  Top = 160
                  Width = 109
                  Height = 13
                  Caption = 'Arquivo Chave Privada'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBErroChavePrivada: TLabel
                  Left = 144
                  Top = 160
                  Width = 75
                  Height = 13
                  Caption = '(N'#227'o informado)'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btBBAcharChavePrivada: TSpeedButton
                  Left = 646
                  Top = 175
                  Width = 24
                  Height = 23
                  Anchors = [akTop, akRight]
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btBBAcharChavePrivadaClick
                end
                object btBBAcharCertificado: TSpeedButton
                  Left = 646
                  Top = 225
                  Width = 24
                  Height = 23
                  Anchors = [akTop, akRight]
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btBBAcharCertificadoClick
                end
                object lbBBCertificado: TLabel
                  Left = 15
                  Top = 210
                  Width = 115
                  Height = 13
                  Caption = 'Arquivo Certificado PEM'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbBBErroCertificado: TLabel
                  Left = 155
                  Top = 210
                  Width = 75
                  Height = 13
                  Caption = '(N'#227'o informado)'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edBBMCITeste: TEdit
                  Left = 330
                  Top = 25
                  Width = 300
                  Height = 21
                  TabOrder = 0
                end
                object eBBDevAppKey: TEdit
                  Left = 15
                  Top = 25
                  Width = 300
                  Height = 21
                  TabOrder = 1
                end
                object edBBClientID: TEdit
                  Left = 15
                  Top = 75
                  Width = 615
                  Height = 21
                  TabOrder = 2
                end
                object edBBClientSecret: TEdit
                  Left = 15
                  Top = 125
                  Width = 615
                  Height = 21
                  TabOrder = 3
                end
                object edBBChavePrivada: TEdit
                  Left = 15
                  Top = 175
                  Width = 632
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  AutoSize = False
                  TabOrder = 4
                  Text = 'BBChavePrivada.key'
                  OnChange = edBBArqsChange
                  OnExit = edBBChavePrivadaExit
                end
                object edBBCertificado: TEdit
                  Left = 15
                  Top = 225
                  Width = 632
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  AutoSize = False
                  TabOrder = 5
                  Text = 'BBCertificado.pem'
                  OnChange = edBBArqsChange
                  OnExit = edBBCertificadoExit
                end
              end
            end
          end
          object tsConfigInter: TTabSheet
            Caption = 'tsConfigInter'
            object gbConfigInter: TGroupBox
              Left = 0
              Top = 0
              Width = 689
              Height = 292
              Align = alClient
              Caption = 'Banco Inter'
              TabOrder = 0
              Visible = False
              object pnConfigInter: TPanel
                Left = 2
                Top = 15
                Width = 685
                Height = 275
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                DesignSize = (
                  685
                  275)
                object lbInterClientID: TLabel
                  Left = 15
                  Top = 10
                  Width = 40
                  Height = 13
                  Caption = 'Client ID'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbInterClientSecret: TLabel
                  Left = 15
                  Top = 60
                  Width = 60
                  Height = 13
                  Caption = 'Client Secret'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbInterChavePrivada: TLabel
                  Left = 15
                  Top = 110
                  Width = 109
                  Height = 13
                  Caption = 'Arquivo Chave Privada'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbInterErroChavePrivada: TLabel
                  Left = 144
                  Top = 110
                  Width = 75
                  Height = 13
                  Caption = '(N'#227'o informado)'
                  Color = clBtnFace
                  ParentColor = False
                end
                object btInterAcharChavePrivada: TSpeedButton
                  Left = 646
                  Top = 125
                  Width = 24
                  Height = 23
                  Anchors = [akTop, akRight]
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btInterAcharChavePrivadaClick
                end
                object btInterAcharCertificado: TSpeedButton
                  Left = 646
                  Top = 175
                  Width = 24
                  Height = 23
                  Anchors = [akTop, akRight]
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btInterAcharCertificadoClick
                end
                object lbInterCertificado: TLabel
                  Left = 15
                  Top = 160
                  Width = 115
                  Height = 13
                  Caption = 'Arquivo Certificado PEM'
                  Color = clBtnFace
                  ParentColor = False
                end
                object lbInterErroCertificado: TLabel
                  Left = 155
                  Top = 160
                  Width = 75
                  Height = 13
                  Caption = '(N'#227'o informado)'
                  Color = clBtnFace
                  ParentColor = False
                end
                object edInterClientID: TEdit
                  Left = 15
                  Top = 25
                  Width = 615
                  Height = 23
                  TabOrder = 0
                end
                object edInterClientSecret: TEdit
                  Left = 15
                  Top = 75
                  Width = 615
                  Height = 23
                  TabOrder = 1
                end
                object edInterChavePrivada: TEdit
                  Left = 15
                  Top = 125
                  Width = 632
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  AutoSize = False
                  TabOrder = 2
                  Text = 'InterChavePrivada.key'
                  OnChange = edInterArqsChange
                  OnExit = edInterChavePrivadaExit
                end
                object edInterCertificado: TEdit
                  Left = 15
                  Top = 175
                  Width = 632
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  AutoSize = False
                  TabOrder = 3
                  Text = 'InterCertificado.pem'
                  OnChange = edInterArqsChange
                  OnExit = edInterCertificadoExit
                end
              end
            end
          end
        end
      end
      object pnConfigRodape: TPanel
        Left = 0
        Top = 511
        Width = 697
        Height = 49
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object btGravarConfig: TButton
          Left = 200
          Top = 8
          Width = 168
          Height = 33
          Caption = 'Gravar Configura'#231#227'o'
          TabOrder = 0
          OnClick = btGravarConfigClick
        end
        object btLerConfig: TButton
          Left = 20
          Top = 8
          Width = 168
          Height = 33
          Caption = 'Ler Configura'#231#227'o'
          TabOrder = 1
          OnClick = btLerConfigClick
        end
      end
    end
  end
  object ACBrExtratoAPI1: TACBrExtratoAPI
    Ambiente = eamHomologacao
    LogNivel = 0
    Left = 176
    Top = 8
  end
  object ACBrOpenSSLUtils1: TACBrOpenSSLUtils
    Left = 264
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 360
    Top = 8
  end
end
