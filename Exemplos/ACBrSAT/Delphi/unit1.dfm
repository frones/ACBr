object Form1: TForm1
  Left = 241
  Top = 173
  Width = 930
  Height = 519
  ActiveControl = PageControl1
  Caption = 'SAT Teste - Projeto ACBr'
  Color = clBtnFace
  Constraints.MinHeight = 460
  Constraints.MinWidth = 850
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 242
    Width = 914
    Height = 6
    Cursor = crVSplit
    Align = alTop
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 248
    Width = 914
    Height = 189
    ActivePage = tsLog
    Align = alClient
    TabOrder = 1
    object tsLog: TTabSheet
      Caption = 'Log de Comandos'
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 906
        Height = 161
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object tsGerado: TTabSheet
      Caption = 'XML Gerado'
      inline mVendaEnviar: TMemo
        Left = 0
        Top = 0
        Width = 889
        Height = 161
        Cursor = crIBeam
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object tsRecebido: TTabSheet
      Caption = 'XML Recebido'
      inline mRecebido: TWebBrowser
        Left = 0
        Top = 0
        Width = 889
        Height = 161
        Cursor = crIBeam
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000E15B0000A41000000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
    object tsCancelamento: TTabSheet
      Caption = 'XML Cancelamento'
      inline mCancelamentoEnviar: TMemo
        Left = 0
        Top = 0
        Width = 889
        Height = 136
        Cursor = crIBeam
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 136
        Width = 889
        Height = 25
        Align = alBottom
        TabOrder = 1
        object Label18: TLabel
          Left = 1
          Top = 1
          Width = 105
          Height = 13
          Align = alLeft
          Caption = 'Chave Cancelamento:'
          Color = clBtnFace
          ParentColor = False
          Layout = tlCenter
        end
        object edChaveCancelamento: TEdit
          Left = 127
          Top = 1
          Width = 704
          Height = 21
          TabOrder = 0
        end
      end
    end
    object tsRedeXML: TTabSheet
      Caption = 'XML Rede'
      inline mRede: TWebBrowser
        Left = 0
        Top = 0
        Width = 889
        Height = 161
        Cursor = crIBeam
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000E15B0000A41000000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 914
    Height = 242
    Align = alTop
    TabOrder = 0
    object gpOperacao: TGroupBox
      Left = 1
      Top = 1
      Width = 169
      Height = 240
      Align = alLeft
      Caption = 'Inicializa'#231#227'o'
      TabOrder = 0
      object bInicializar: TButton
        Left = 30
        Top = 71
        Width = 105
        Height = 33
        Caption = 'Inicializar'
        TabOrder = 1
        OnClick = bInicializarClick
      end
      object cbxModelo: TComboBox
        Left = 16
        Top = 40
        Width = 133
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbxModeloChange
        Items.Strings = (
          '')
      end
      object GroupBox2: TGroupBox
        Left = 2
        Top = 149
        Width = 165
        Height = 89
        Align = alBottom
        Caption = 'Par'#226'metros'
        TabOrder = 2
        object btLerParams: TButton
          Left = 31
          Top = 23
          Width = 105
          Height = 25
          Caption = 'Ler'
          TabOrder = 0
          OnClick = btLerParamsClick
        end
        object btSalvarParams: TButton
          Left = 31
          Top = 55
          Width = 105
          Height = 25
          Caption = 'Salvar'
          TabOrder = 1
          OnClick = btSalvarParamsClick
        end
      end
    end
    object GroupBox1: TGroupBox
      Left = 170
      Top = 1
      Width = 743
      Height = 240
      Align = alClient
      Caption = 'Configura'#231#227'o'
      TabOrder = 1
      object PageControl2: TPageControl
        Left = 2
        Top = 15
        Width = 739
        Height = 223
        ActivePage = Impressao
        Align = alClient
        TabOrder = 0
        object tsDadosSAT: TTabSheet
          Caption = 'Dados do SAT CFe'
          DesignSize = (
            731
            195)
          object Label9: TLabel
            Left = 21
            Top = 7
            Width = 40
            Height = 13
            Alignment = taRightJustify
            Caption = 'Arq.Log:'
            Color = clBtnFace
            ParentColor = False
          end
          object SbArqLog: TSpeedButton
            Left = 185
            Top = 24
            Width = 24
            Height = 22
            Caption = '...'
            OnClick = SbArqLogClick
          end
          object Label10: TLabel
            Left = 222
            Top = 7
            Width = 54
            Height = 13
            Alignment = taRightJustify
            Caption = 'Nome DLL:'
            Color = clBtnFace
            ParentColor = False
          end
          object Label1: TLabel
            Left = 16
            Top = 56
            Width = 93
            Height = 13
            Caption = 'C'#243'digo de Ativa'#231#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object Label4: TLabel
            Left = 217
            Top = 56
            Width = 36
            Height = 13
            Caption = 'C'#243'd.UF'
            Color = clBtnFace
            ParentColor = False
          end
          object Label3: TLabel
            Left = 312
            Top = 57
            Width = 51
            Height = 13
            Caption = 'Num.Caixa'
            Color = clBtnFace
            ParentColor = False
          end
          object Label13: TLabel
            Left = 280
            Top = 106
            Width = 33
            Height = 13
            Caption = 'Vers'#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object Label6: TLabel
            Left = 504
            Top = 6
            Width = 44
            Height = 13
            Caption = 'Ambiente'
            Color = clBtnFace
            ParentColor = False
          end
          object sbNomeDLL: TSpeedButton
            Left = 500
            Top = 25
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbNomeDLLClick
          end
          object Label8: TLabel
            Left = 196
            Top = 164
            Width = 72
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Itens a Vender:'
            Color = clBtnFace
            ParentColor = False
          end
          object edLog: TEdit
            Left = 17
            Top = 24
            Width = 163
            Height = 21
            Cursor = crIBeam
            TabOrder = 0
          end
          object edNomeDLL: TEdit
            Left = 217
            Top = 24
            Width = 279
            Height = 21
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object edtCodigoAtivacao: TEdit
            Left = 16
            Top = 72
            Width = 164
            Height = 21
            TabOrder = 3
          end
          object edtCodUF: TEdit
            Left = 218
            Top = 72
            Width = 61
            Height = 21
            TabOrder = 4
          end
          object seNumeroCaixa: TSpinEdit
            Left = 312
            Top = 73
            Width = 58
            Height = 22
            MaxValue = 999
            MinValue = 1
            TabOrder = 5
            Value = 1
          end
          object cbxFormatXML: TCheckBox
            Left = 152
            Top = 104
            Width = 95
            Height = 19
            Caption = 'Formatar XML'
            Checked = True
            State = cbChecked
            TabOrder = 7
          end
          object cbxAmbiente: TComboBox
            Left = 545
            Top = 23
            Width = 152
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
            ItemHeight = 13
            TabOrder = 2
          end
          object cbxRemoverAcentos: TCheckBox
            Left = 152
            Top = 127
            Width = 113
            Height = 19
            Caption = 'Remover Acentos'
            Checked = True
            State = cbChecked
            TabOrder = 8
          end
          object GroupBox5: TGroupBox
            Left = 433
            Top = 56
            Width = 265
            Height = 124
            Anchors = [akTop, akRight]
            Caption = 'Salvar XMLs'
            TabOrder = 10
            object cbxSalvarEnvio: TCheckBox
              Left = 9
              Top = 22
              Width = 111
              Height = 19
              Caption = 'Salvar Envio'
              Checked = True
              State = cbChecked
              TabOrder = 2
            end
            object cbxSalvarCFe: TCheckBox
              Left = 9
              Top = 47
              Width = 111
              Height = 19
              Caption = 'Salvar CFe'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object cbxSalvarCFeCanc: TCheckBox
              Left = 9
              Top = 72
              Width = 111
              Height = 19
              Caption = 'Salvar CFeCanc'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
            object cbxSepararPorCNPJ: TCheckBox
              Left = 9
              Top = 97
              Width = 111
              Height = 19
              Caption = 'Separar por CNPJ'
              Checked = True
              State = cbChecked
              TabOrder = 3
            end
            object cbxSepararPorModelo: TCheckBox
              Left = 129
              Top = 22
              Width = 124
              Height = 19
              Caption = 'Separar por Modelo'
              Checked = True
              State = cbChecked
              TabOrder = 7
            end
            object cbxSepararPorDia: TCheckBox
              Left = 129
              Top = 47
              Width = 124
              Height = 19
              Caption = 'Separar por Dia'
              Checked = True
              State = cbChecked
              TabOrder = 6
            end
            object cbxSepararPorMes: TCheckBox
              Left = 129
              Top = 72
              Width = 124
              Height = 19
              Caption = 'Separar por M'#234's'
              Checked = True
              State = cbChecked
              TabOrder = 4
            end
            object cbxSepararPorAno: TCheckBox
              Left = 129
              Top = 97
              Width = 124
              Height = 19
              Caption = 'Separar por Ano'
              Checked = True
              State = cbChecked
              TabOrder = 5
            end
          end
          object GroupBox6: TGroupBox
            Left = 8
            Top = 98
            Width = 137
            Height = 48
            Caption = 'P'#225'gina de C'#243'digo'
            TabOrder = 6
            object sePagCod: TSpinEdit
              Left = 8
              Top = 16
              Width = 64
              Height = 22
              MaxValue = 65001
              MinValue = 0
              TabOrder = 0
              Value = 0
              OnChange = sePagCodChange
            end
            object cbxUTF8: TCheckBox
              Left = 79
              Top = 20
              Width = 47
              Height = 19
              Caption = 'UTF8'
              TabOrder = 1
            end
          end
          object seItensVenda: TSpinEdit
            Left = 277
            Top = 160
            Width = 90
            Height = 22
            Anchors = [akTop, akRight]
            Increment = 3
            MaxValue = 999
            MinValue = 3
            TabOrder = 9
            Value = 3
          end
          object sfeVersaoEnt: TEdit
            Left = 280
            Top = 120
            Width = 85
            Height = 21
            TabOrder = 11
            Text = '0.07'
            OnChange = sfeVersaoEntChange
          end
        end
        object tsDadosEmit: TTabSheet
          Caption = 'Dados Emitente'
          object Label11: TLabel
            Left = 12
            Top = 23
            Width = 27
            Height = 13
            Caption = 'CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object Label12: TLabel
            Left = 192
            Top = 23
            Width = 64
            Height = 13
            Caption = 'Insc.Estadual'
            Color = clBtnFace
            ParentColor = False
          end
          object Label14: TLabel
            Left = 336
            Top = 23
            Width = 68
            Height = 13
            Caption = 'Insc.Municipal'
            Color = clBtnFace
            ParentColor = False
          end
          object Label15: TLabel
            Left = 192
            Top = 71
            Width = 96
            Height = 13
            Caption = 'Regime Trib. ISSQN'
            Color = clBtnFace
            ParentColor = False
          end
          object Label16: TLabel
            Left = 336
            Top = 71
            Width = 71
            Height = 13
            Caption = 'Ind.Rat.ISSQN'
            Color = clBtnFace
            ParentColor = False
          end
          object Label17: TLabel
            Left = 12
            Top = 73
            Width = 83
            Height = 13
            Caption = 'Regime Tributario'
            Color = clBtnFace
            ParentColor = False
          end
          object edtEmitCNPJ: TEdit
            Left = 12
            Top = 38
            Width = 166
            Height = 21
            Cursor = crIBeam
            TabOrder = 0
          end
          object edtEmitIE: TEdit
            Left = 192
            Top = 38
            Width = 134
            Height = 21
            Cursor = crIBeam
            TabOrder = 1
          end
          object edtEmitIM: TEdit
            Left = 336
            Top = 38
            Width = 134
            Height = 21
            Cursor = crIBeam
            TabOrder = 2
          end
          object cbxRegTribISSQN: TComboBox
            Left = 192
            Top = 87
            Width = 130
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 4
          end
          object cbxIndRatISSQN: TComboBox
            Left = 336
            Top = 87
            Width = 134
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 5
          end
          object cbxRegTributario: TComboBox
            Left = 12
            Top = 87
            Width = 166
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 3
          end
        end
        object tsDadosSwHouse: TTabSheet
          Caption = 'Dados Sw.House'
          DesignSize = (
            731
            195)
          object Label2: TLabel
            Left = 10
            Top = 15
            Width = 27
            Height = 13
            Caption = 'CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object Label5: TLabel
            Left = 10
            Top = 71
            Width = 181
            Height = 13
            Caption = 'Assinatura Sw.House (344 caracteres)'
            Color = clBtnFace
            ParentColor = False
          end
          object edtSwHCNPJ: TEdit
            Left = 10
            Top = 31
            Width = 355
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edtSwHAssinatura: TEdit
            Left = 10
            Top = 89
            Width = 620
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
        end
        object tsRede: TTabSheet
          Caption = 'Rede'
          object gbPPPoE: TGroupBox
            Left = 160
            Top = 48
            Width = 289
            Height = 105
            Caption = 'PPPoE'
            TabOrder = 5
            Visible = False
            object lSSID7: TLabel
              Left = 17
              Top = 4
              Width = 36
              Height = 13
              Caption = 'Usuario'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID8: TLabel
              Left = 17
              Top = 34
              Width = 31
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object edRedeUsuario: TEdit
              Left = 64
              Top = -4
              Width = 88
              Height = 21
              TabOrder = 0
            end
            object edRedeSenha: TEdit
              Left = 64
              Top = 26
              Width = 88
              Height = 21
              TabOrder = 1
            end
          end
          object gbIPFix: TGroupBox
            Left = 160
            Top = 48
            Width = 289
            Height = 109
            Caption = 'IPFIX'
            TabOrder = 4
            Visible = False
            object lSSID2: TLabel
              Left = 36
              Top = 28
              Width = 10
              Height = 13
              Caption = 'IP'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID3: TLabel
              Left = 21
              Top = 58
              Width = 26
              Height = 13
              Caption = 'Mask'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID4: TLabel
              Left = 4
              Top = 88
              Width = 42
              Height = 13
              Caption = 'Gateway'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID5: TLabel
              Left = 156
              Top = 28
              Width = 29
              Height = 13
              Caption = 'DNS1'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID6: TLabel
              Left = 156
              Top = 58
              Width = 29
              Height = 13
              Caption = 'DNS2'
              Color = clBtnFace
              ParentColor = False
            end
            object edRedeIP: TEdit
              Left = 60
              Top = 20
              Width = 88
              Height = 21
              TabOrder = 0
            end
            object edRedeMask: TEdit
              Left = 60
              Top = 50
              Width = 88
              Height = 21
              TabOrder = 1
            end
            object edRedeGW: TEdit
              Left = 60
              Top = 80
              Width = 88
              Height = 21
              TabOrder = 2
            end
            object edRedeDNS1: TEdit
              Left = 196
              Top = 20
              Width = 87
              Height = 21
              TabOrder = 3
            end
            object edRedeDNS2: TEdit
              Left = 196
              Top = 50
              Width = 87
              Height = 21
              TabOrder = 4
            end
          end
          object rgRedeTipoInter: TRadioGroup
            Left = 8
            Top = 0
            Width = 144
            Height = 41
            Caption = 'Tipo Rede'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'ETHE'
              'WIFI')
            TabOrder = 0
            OnClick = rgRedeTipoInterClick
          end
          object gbWiFi: TGroupBox
            Left = 8
            Top = 48
            Width = 144
            Height = 109
            Caption = 'WiFi'
            TabOrder = 3
            Visible = False
            object lSSID: TLabel
              Left = 8
              Top = 28
              Width = 28
              Height = 13
              Caption = 'SSID:'
              Color = clBtnFace
              ParentColor = False
            end
            object Label24: TLabel
              Left = 8
              Top = 58
              Width = 22
              Height = 13
              Caption = 'Seg:'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID1: TLabel
              Left = 8
              Top = 88
              Width = 31
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object edRedeSSID: TEdit
              Left = 47
              Top = 20
              Width = 82
              Height = 21
              TabOrder = 0
            end
            object cbxRedeSeg: TComboBox
              Left = 47
              Top = 50
              Width = 82
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 1
              TabOrder = 1
              Text = 'WEP'
              Items.Strings = (
                'NONE'
                'WEP'
                'WPA-PERSONAL'
                'WPA-ENTERPRISE')
            end
            object edRedeCodigo: TEdit
              Left = 47
              Top = 80
              Width = 82
              Height = 21
              TabOrder = 2
            end
          end
          object rgRedeTipoLan: TRadioGroup
            Left = 160
            Top = 0
            Width = 288
            Height = 41
            Caption = 'Tipo Rede'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'DHCP'
              'PPPoE'
              'IPFIX')
            TabOrder = 1
            OnClick = rgRedeTipoLanClick
          end
          object gbProxy: TGroupBox
            Left = 455
            Top = 0
            Width = 150
            Height = 157
            Caption = 'Proxy'
            TabOrder = 2
            object lSSID9: TLabel
              Left = 24
              Top = 54
              Width = 10
              Height = 13
              Caption = 'IP'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID10: TLabel
              Left = 11
              Top = 84
              Width = 25
              Height = 13
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID11: TLabel
              Left = 3
              Top = 110
              Width = 36
              Height = 13
              Caption = 'Usuario'
              Color = clBtnFace
              ParentColor = False
            end
            object lSSID12: TLabel
              Left = 8
              Top = 136
              Width = 31
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object cbxRedeProxy: TComboBox
              Left = 8
              Top = 22
              Width = 129
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = '0= N'#227'o usa proxy'
              OnChange = cbxRedeProxyChange
              Items.Strings = (
                '0= N'#227'o usa proxy'
                '1= Proxy com configura'#231#227'o'
                '2= Proxy transparente')
            end
            object edRedeProxyIP: TEdit
              Left = 49
              Top = 50
              Width = 88
              Height = 21
              Enabled = False
              TabOrder = 1
            end
            object edRedeProxyUser: TEdit
              Left = 49
              Top = 102
              Width = 88
              Height = 21
              Enabled = False
              TabOrder = 3
            end
            object edRedeProxySenha: TEdit
              Left = 49
              Top = 128
              Width = 88
              Height = 21
              Enabled = False
              TabOrder = 4
            end
            object edRedeProxyPorta: TSpinEdit
              Left = 49
              Top = 76
              Width = 88
              Height = 22
              Enabled = False
              MaxValue = 999999
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
          end
        end
        object Impressao: TTabSheet
          Caption = 'Impressao'
          object GroupBox3: TGroupBox
            Left = 8
            Top = 27
            Width = 335
            Height = 142
            TabOrder = 2
            object Label19: TLabel
              Left = 16
              Top = 16
              Width = 36
              Height = 13
              Caption = 'Largura'
              Color = clBtnFace
              ParentColor = False
            end
            object Label20: TLabel
              Left = 104
              Top = 16
              Width = 25
              Height = 13
              Caption = 'Topo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label21: TLabel
              Left = 16
              Top = 64
              Width = 30
              Height = 13
              Caption = 'Fundo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label22: TLabel
              Left = 104
              Top = 64
              Width = 45
              Height = 13
              Caption = 'Esquerda'
              Color = clBtnFace
              ParentColor = False
            end
            object Label23: TLabel
              Left = 192
              Top = 64
              Width = 30
              Height = 13
              Caption = 'Direita'
              Color = clBtnFace
              ParentColor = False
            end
            object lImpressora: TLabel
              Left = 144
              Top = 118
              Width = 93
              Height = 13
              Caption = 'Impresssora Default'
              Color = clBtnFace
              ParentColor = False
            end
            object seLargura: TSpinEdit
              Left = 16
              Top = 32
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
            object seMargemTopo: TSpinEdit
              Left = 104
              Top = 32
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object seMargemFundo: TSpinEdit
              Left = 16
              Top = 82
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object seMargemEsquerda: TSpinEdit
              Left = 104
              Top = 82
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object seMargemDireita: TSpinEdit
              Left = 192
              Top = 82
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 5
              Value = 0
            end
            object bImpressora: TButton
              Left = 16
              Top = 110
              Width = 122
              Height = 25
              Caption = 'Definir Impressora'
              TabOrder = 6
              OnClick = bImpressoraClick
            end
            object cbPreview: TCheckBox
              Left = 200
              Top = 42
              Width = 61
              Height = 19
              Caption = 'Preview'
              TabOrder = 2
            end
          end
          object GroupBox4: TGroupBox
            Left = 353
            Top = 27
            Width = 322
            Height = 142
            TabOrder = 3
            DesignSize = (
              322
              142)
            object Label25: TLabel
              Left = 155
              Top = 66
              Width = 31
              Height = 26
              Caption = 'Linhas'#13#10'Pular'
              Color = clBtnFace
              ParentColor = False
            end
            object Label26: TLabel
              Left = 82
              Top = 66
              Width = 41
              Height = 26
              Caption = 'Espa'#231'os'#13#10'Linhas'
              Color = clBtnFace
              ParentColor = False
            end
            object Label27: TLabel
              Left = 10
              Top = 81
              Width = 38
              Height = 13
              Caption = 'Colunas'
              Color = clBtnFace
              ParentColor = False
            end
            object Label28: TLabel
              Left = 8
              Top = 12
              Width = 35
              Height = 13
              Caption = 'Modelo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label7: TLabel
              Left = 158
              Top = 12
              Width = 25
              Height = 13
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object Label29: TLabel
              Left = 218
              Top = 81
              Width = 54
              Height = 13
              Caption = 'Pag.codigo'
              Color = clBtnFace
              ParentColor = False
            end
            object btSerial: TSpeedButton
              Left = 286
              Top = 32
              Width = 25
              Height = 25
              Anchors = [akLeft, akTop, akBottom]
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF323232
                3232323E3E3E565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFF565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFF503200FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                565656565656FFFFFFFFFFFF3232322626262626262626262626265032005032
                000000504873FFFFFFFFFFFFFFFFFFFF6E6E6EFFFFFFFFFFFFFFFFFFFFFFFF6E
                6E6E32323232323232323232323250320000005025AAFFFFFFFFFFFFFF565656
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5656563232323232326E6E6E5032005032
                008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFF50320050320056
                56564A4A4A5050003232325032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
                FFFFFF5656563E3E3E2626265032006262625656565050003232325032005032
                008FFF6B8ED4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5050005050006E
                6E6E5656565050003250005032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8686865656565656563250005032005032
                008FFF6B48B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3232323E
                3E3EA4A0A08686866E6E6E565656503200C0C0C02557FFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFF5050004A4A4A3232323232323232323232325032
                00FFFFFF6B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              OnClick = btSerialClick
            end
            object seLinhasPular: TSpinEdit
              Left = 155
              Top = 102
              Width = 41
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object seEspLinhas: TSpinEdit
              Left = 82
              Top = 102
              Width = 41
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object seColunas: TSpinEdit
              Left = 10
              Top = 102
              Width = 41
              Height = 22
              MaxValue = 999
              MinValue = 1
              TabOrder = 2
              Value = 48
            end
            object cbxModeloPosPrinter: TComboBox
              Left = 6
              Top = 32
              Width = 145
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
              OnChange = cbxModeloChange
            end
            object cbxPorta: TComboBox
              Left = 158
              Top = 32
              Width = 128
              Height = 21
              ItemHeight = 13
              TabOrder = 1
            end
            object cbxPagCodigo: TComboBox
              Left = 218
              Top = 102
              Width = 93
              Height = 21
              Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 5
            end
          end
          object cbUsarFortes: TRadioButton
            Left = 118
            Top = 2
            Width = 139
            Height = 25
            Caption = 'Usar Fortes'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = cbUsarFortesClick
          end
          object cbUsarEscPos: TRadioButton
            Left = 450
            Top = 2
            Width = 143
            Height = 25
            Caption = 'Usar EscPOS'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = cbUsarEscPosClick
          end
          object cbLogoLateral: TCheckBox
            Left = 8
            Top = 173
            Width = 85
            Height = 19
            Caption = 'Logo Lateral'
            TabOrder = 4
          end
          object cbQRCodeLateral: TCheckBox
            Left = 112
            Top = 173
            Width = 102
            Height = 19
            Caption = 'QRCode Lateral'
            TabOrder = 5
          end
          object cbImprimir1Linha: TCheckBox
            Left = 232
            Top = 173
            Width = 102
            Height = 19
            Caption = 'Item em 1 linha'
            TabOrder = 6
          end
          object cbImprimirDescAcres: TCheckBox
            Left = 368
            Top = 173
            Width = 127
            Height = 19
            Caption = 'Imprime Desc/Acres'
            TabOrder = 7
          end
          object cbImprimirChaveUmaLinha: TCheckBox
            Left = 536
            Top = 173
            Width = 111
            Height = 19
            Caption = 'Chave em 1 linha'
            TabOrder = 8
          end
        end
        object tsMFe: TTabSheet
          Caption = 'MFE'
          object Label30: TLabel
            Left = 8
            Top = 8
            Width = 67
            Height = 13
            Caption = 'Pasta Entrada'
            Color = clBtnFace
            ParentColor = False
          end
          object Label31: TLabel
            Left = 8
            Top = 48
            Width = 75
            Height = 13
            Caption = 'Pasta Resposta'
            Color = clBtnFace
            ParentColor = False
          end
          object Label32: TLabel
            Left = 8
            Top = 88
            Width = 38
            Height = 13
            Caption = 'Timeout'
            Color = clBtnFace
            ParentColor = False
          end
          object edMFEInput: TEdit
            Left = 8
            Top = 24
            Width = 264
            Height = 21
            TabOrder = 0
            Text = 'c:\Integrador\Input\'
          end
          object edMFEOutput: TEdit
            Left = 8
            Top = 64
            Width = 264
            Height = 21
            TabOrder = 1
            Text = 'c:\Integrador\Output\'
          end
          object seMFETimeout: TSpinEdit
            Left = 8
            Top = 104
            Width = 82
            Height = 22
            MaxValue = 0
            MinValue = 10
            TabOrder = 2
            Value = 10
          end
          object btMFEEnviarPagamento: TButton
            Left = 320
            Top = 23
            Width = 147
            Height = 25
            Caption = 'MFE Enviar Pagamento'
            TabOrder = 3
            OnClick = btMFEEnviarPagamentoClick
          end
          object btMFEVerificarStatus: TButton
            Left = 320
            Top = 51
            Width = 147
            Height = 25
            Caption = 'Verificar Status Validador'
            TabOrder = 4
            OnClick = btMFEVerificarStatusClick
          end
          object btMFEEnviarStatusPagamento: TButton
            Left = 472
            Top = 23
            Width = 147
            Height = 25
            Caption = 'Enviar Status Pagamento'
            TabOrder = 5
            OnClick = btMFEEnviarStatusPagamentoClick
          end
          object btMFERespostaFiscal: TButton
            Left = 472
            Top = 51
            Width = 147
            Height = 25
            Caption = 'Resposta Fiscal'
            TabOrder = 6
            OnClick = btMFERespostaFiscalClick
          end
        end
        object TabSheet1: TTabSheet
          Caption = 'Valida'#231#227'o'
          DesignSize = (
            731
            195)
          object Label33: TLabel
            Left = 25
            Top = 16
            Width = 123
            Height = 13
            Alignment = taRightJustify
            Caption = 'Schema Venda Aplica'#231#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object sbSchemaVendaAPL: TSpeedButton
            Left = 656
            Top = 33
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbSchemaVendaAPLClick
          end
          object sbSchemaVendaSAT: TSpeedButton
            Left = 656
            Top = 81
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbSchemaVendaSATClick
          end
          object Label34: TLabel
            Left = 19
            Top = 64
            Width = 97
            Height = 13
            Alignment = taRightJustify
            Caption = 'Schema Venda SAT'
            Color = clBtnFace
            ParentColor = False
          end
          object Label35: TLabel
            Left = 16
            Top = 119
            Width = 137
            Height = 13
            Caption = 'Biblioteca de analise de XML'
            Color = clBtnFace
            ParentColor = False
          end
          object edSchemaVendaAPL: TEdit
            Left = 16
            Top = 33
            Width = 637
            Height = 21
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edSchemaVendaSAT: TEdit
            Left = 16
            Top = 81
            Width = 637
            Height = 21
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object cbxXmlSignLib: TComboBox
            Left = 16
            Top = 136
            Width = 192
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 437
    Width = 914
    Height = 23
    Panels = <
      item
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 224
    Top = 224
    object MenuItem1: TMenuItem
      Caption = 'Ativa'#231#227'o'
      object mAtivarSAT: TMenuItem
        Caption = 'Ativar SAT'
        OnClick = mAtivarSATClick
      end
      object mComunicarCertificado: TMenuItem
        Caption = 'Comunicar Certificado'
        OnClick = mComunicarCertificadoClick
      end
      object mAssociarAssinatura: TMenuItem
        Caption = 'Associar Assinatura'
        OnClick = mAssociarAssinaturaClick
      end
      object MenuItem3: TMenuItem
        Caption = '-'
      end
      object mBloquearSAT: TMenuItem
        Caption = 'Bloquear SAT'
        OnClick = mBloquearSATClick
      end
      object mDesbloquearSAT: TMenuItem
        Caption = 'Desbloquear SAT'
        OnClick = mDesbloquearSATClick
      end
      object MenuItem4: TMenuItem
        Caption = '-'
      end
      object MenuItem5: TMenuItem
        Caption = 'Trocar  C'#243'digo de Ativa'#231#227'o'
        OnClick = MenuItem5Click
      end
    end
    object MenuItem2: TMenuItem
      Caption = 'Venda'
      object mGerarVenda: TMenuItem
        Caption = 'Gerar Venda'
        OnClick = mGerarVendaClick
      end
      object mEnviarVenda: TMenuItem
        Caption = 'Enviar Venda'
        OnClick = mEnviarVendaClick
      end
      object MenuItem21: TMenuItem
        Caption = '-'
      end
      object mImprimirExtratoVenda: TMenuItem
        Caption = 'Imprimir Extrato Venda'
        OnClick = mImprimirExtratoVendaClick
      end
      object mImprimirExtratoVendaResumido: TMenuItem
        Caption = 'Imprimir Extrato Venda Resumido'
        OnClick = mImprimirExtratoVendaResumidoClick
      end
      object MenuItem18: TMenuItem
        Caption = 'Gerar Texto Extrato Venda (MFE)'
        OnClick = MenuItem18Click
      end
      object MenuItem14: TMenuItem
        Caption = '-'
      end
      object MenuItem19: TMenuItem
        Caption = 'Validar XML Gerado Aplica'#231#227'o'
        OnClick = MenuItem19Click
      end
      object MenuItem22: TMenuItem
        Caption = 'Validar XML Recebido SAT'
        OnClick = MenuItem22Click
      end
      object MenuItem20: TMenuItem
        Caption = '-'
      end
      object MenuItem15: TMenuItem
        Caption = 'Carregar XML'
        OnClick = MenuItem15Click
      end
    end
    object MenuItem12: TMenuItem
      Caption = 'Cancelamento'
      object miGerarXMLCancelamento: TMenuItem
        Caption = 'Gerar XML Cancelamento'
        OnClick = miGerarXMLCancelamentoClick
      end
      object miEnviarCancelamento: TMenuItem
        Caption = 'Enviar Cancelamento'
        OnClick = miEnviarCancelamentoClick
      end
      object miImprimirExtratoCancelamento: TMenuItem
        Caption = 'Imprimir Extrato Cancelamento'
        OnClick = miImprimirExtratoCancelamentoClick
      end
      object MenuItem16: TMenuItem
        Caption = '-'
      end
      object MenuItem17: TMenuItem
        Caption = 'Carregar XML de Cancelamento'
        OnClick = MenuItem17Click
      end
    end
    object MenuItem6: TMenuItem
      Caption = 'Consultas'
      object mConsultarStatusOperacional: TMenuItem
        Caption = 'Consultar Status Operacional'
        OnClick = mConsultarStatusOperacionalClick
      end
      object mConsultarSAT: TMenuItem
        Caption = 'Consultar SAT'
        OnClick = mConsultarSATClick
      end
      object mConsultarNumeroSessao: TMenuItem
        Caption = 'Consultar Numero Sess'#227'o'
        OnClick = mConsultarNumeroSessaoClick
      end
    end
    object MenuItem7: TMenuItem
      Caption = 'Configura'#231#227'o'
      object mAtaulizarSoftwareSAT: TMenuItem
        Caption = 'Atualizar Software SAT'
        OnClick = mAtaulizarSoftwareSATClick
      end
      object MenuItem9: TMenuItem
        Caption = '-'
      end
      object mConfigurarInterfaceRede: TMenuItem
        Caption = 'Ler XML Interface Rede'
        OnClick = mConfigurarInterfaceRedeClick
      end
      object MenuItem13: TMenuItem
        Caption = 'Gerar XML Interface Rede'
        OnClick = MenuItem13Click
      end
      object MenuItem11: TMenuItem
        Caption = '-'
      end
      object MenuItem10: TMenuItem
        Caption = 'Configurar Interface Rede'
        OnClick = MenuItem10Click
      end
    end
    object MenuItem8: TMenuItem
      Caption = 'Diversos'
      object mTesteFimAFim: TMenuItem
        Caption = 'Teste Fim a Fim'
        OnClick = mTesteFimAFimClick
      end
      object mExtrairLogs: TMenuItem
        Caption = 'Extrair Logs'
        OnClick = mExtrairLogsClick
      end
    end
    object mLimpar: TMenuItem
      Caption = 'Limpar'
      OnClick = mLimparClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 224
    Top = 280
  end
  object ACBrSAT1: TACBrSAT
    Extrato = ACBrSATExtratoFortes1
    NomeDLL = 'c:\sat\SAT.DLL'
    ValidarNumeroSessaoResposta = True
    NumeroTentativasValidarSessao = 2
    OnGravarLog = ACBrSAT1GravarLog
    Config.infCFe_versaoDadosEnt = 0.050000000000000000
    Config.ide_numeroCaixa = 0
    Config.ide_tpAmb = taHomologacao
    Config.emit_cRegTrib = RTSimplesNacional
    Config.emit_cRegTribISSQN = RTISSMicroempresaMunicipal
    Config.emit_indRatISSQN = irSim
    Config.EhUTF8 = True
    Config.PaginaDeCodigo = 65001
    Config.XmlSignLib = xsMsXml
    ConfigArquivos.SepararPorCNPJ = True
    ConfigArquivos.SepararPorModelo = True
    ConfigArquivos.SepararPorAno = True
    ConfigArquivos.SepararPorMes = True
    ConfigArquivos.SepararPorDia = True
    ConfigArquivos.PrefixoArqCFe = 'AD'
    ConfigArquivos.PrefixoArqCFeCanc = 'ADC'
    Rede.tipoInter = infETHE
    Rede.seg = segNONE
    Rede.tipoLan = lanIPFIX
    Rede.lanIP = '192.168.137.2'
    Rede.lanMask = '255.255.255.0'
    Rede.lanGW = '192.168.137.1'
    Rede.lanDNS1 = '192.168.137.1'
    Rede.lanDNS2 = '192.168.137.1'
    Rede.proxy = 0
    Rede.proxy_porta = 0
    OnMensagemSEFAZ = ACBrSAT1MensagemSEFAZ
    OnCalcPath = ACBrSAT1CalcPath
    Left = 64
    Top = 224
  end
  object ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS
    Site = 'http://www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    MsgAppQRCode = 
      'Consulte o QR Code pelo aplicativo  "De olho na nota", dispon'#237've' +
      'l na AppStore (Apple) e PlayStore (Android)'
    ImprimeDescAcrescItem = False
    PosPrinter = ACBrPosPrinter1
    Left = 64
    Top = 280
  end
  object ACBrSATExtratoFortes1: TACBrSATExtratoFortes
    Site = 'http://www.projetoacbr.com.br'
    MargemInferior = 4.000000000000000000
    MargemSuperior = 2.000000000000000000
    MargemEsquerda = 2.000000000000000000
    MargemDireita = 2.000000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    ACBrSAT = ACBrSAT1
    PictureLogo.Data = {
      07544269746D6170DE0D0000424DDE0D0000000000003E00000028000000FA00
      00006D0000000100010000000000A00D0000232E0000232E0000020000000000
      000000000000FFFFFF00EFDBEDADD6DB6ED6D6A59A6AB364A49352A4A4C94909
      148A451212088892210098AC9556ADB56259335AD795991B5754B6DA9B2D2CD4
      D2D4A965515692949900E6D6EEB5B29B5B2ECCB2A8D5AADAA94A88495252532B
      4A2548AAAA9144A944805DEDB5FEF7D5BDF7BDBF777CD767DB77776FAFD76DD5
      B5BBBEA4AD6CBB574900A6BFFFAF6DFFEFFDF7DFFFBFFFFFFFFFFFFDFFBFFEFF
      FFFEFFFFFFFFFFFFF500BD410910A608880928400841207FFFFF088880204112
      42050210248440442480D4AAD6DB55DA6B554B7DB5ED2D7FFFFFE5A6BED65AA5
      ACD2B5B7955B9DABB900B76D6A5B6CA756EAD54ADB9AD3FFFFFFDD75532DDB79
      377ADB69DAECAD5CC940F0D295A49359AD36DAB54BC92CFFFFFFE64A99C8A526
      DA4D849A6B12E692B6409D9B536F5D2DFB9F6777B7FFEF7FFFFFF5EFECFDDFB5
      7EB5FBEF44D659D65480CEA574C96EDFFBFFF777FFEFFFFFFFFFFEEFEDFFFFFA
      FFFBFFFBF54A9A29AC80D0B5AEB4AFFD9F33EF7F7E6EF9FFFFFFFE7EDDC7B9BB
      65BBBA77EEBBAD9D5480B75A225757FBBBF5F7B7F66FFDFFFFFFFEFFEED9BFB4
      FFB5FFEECB5256E4AA40ACCB55CADBB3BFBFFFF6F7EDDFFFFFFFFFEF6FFFBFFD
      FFBFBBF7F4A7B5376A40D253FA2D29FB5F9E7EF3EBD79FFFFFFFFFD7DF7D9F37
      7FBDF3F6DB3549925400D6DA05B367ECC345844C0A6FC9FFFFFFFF28A803E3FF
      FF421C2EE5D8EECDA980A94AEED659C5BB6CAB5B7525B3FFFFFFFFDB37FFFFFF
      FD776593DAA69AB5AC40D6B592AB5A3694B6B565D9B95DFFFFFFFFCFFFFFFFFF
      F994DAF4A69B35325100EB4AB5494EDA6B4B55944EA649FFFFFFFFFFFFFFFFFF
      F4AB262B297566CDCE8051649AF5B3275570AAB76156E7FFFFFFFFFFFFFFFFFF
      E66AF3CB552B54AD7480BEBB6A16CCE8D6AF6D6A3DB19FFFFFFFFFFFFFFFFFFF
      ED551935B6996B529C80B54D4B667256529246578A4E53FFFFFFFFFFFFFFFFFF
      DAC99AA4A9E6A6DAC900CCCAB5DAAF4B39B5BAA8D5ADB7FFFFFFFFFFFFFFFFFF
      CABB66DD547555265940BB5DB22D32BBAEB6DB5B7AD2ABFFFFFFFFFFFFFFFFFF
      B696556BAB12C9E6A480D5D69F65D554D2C945668B6D5524924A5148A5492492
      48918A8052545430D200EEB5B5DCBEDA5DDF7DADF695AD96DDB5AEB75AEEFEEE
      DF7D76BD198B128A0A80B35B56B74BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFC8420A8C54540ADADAD2ADBFFFFFFFFFFFFFFFFD7FFFFFFFFFFFFFFFF
      FFFFFFF4DB6C442A6900DAD6F2EB6DFFFFFEEF7BFFFFE0002FFFFFF000000001
      FFFFC000224297448480DB695B8001FFFFFC0005FFFF000007FFFFF000000000
      1FFFC000153348A92200CD76990000FFFFFC0007FFF8000000FFFFF3FFFFFFF8
      03FFCFFF1298225264C0B49B668000FFFFF80003FFF40000007FFFF3FFFFFFFF
      E1FFCFFF248552291500EF5BBB4000FFFFFC0007FFE80000003FFFF3FFFFFFFF
      F8FFCFFF26A92912924099B4B50000FFFFF8000FFFC00000000FFFF3FFFFFFFF
      FC7FCFFF19548D4A4A80D6B7A7C0007FFFF00007FF8000000007FFF3FFFFFFFF
      FE3FCFFF048962A4A440EDAA5960007FFFF0001FFF0000000007FFF3FFFFFFFF
      FF3FCFFF344924492500ADD7B680007FFFF8000FFE0000000003FFF3FFFFFFFF
      FF1FCFFF0B6534B15980966CCEE0003FFFE0001FFE0000000001FFF3FFFFFFFF
      FF9FCFFF11288B0C8440F5AEF36000000000001FFC0000000001FFF3FFFFFFFF
      FF8FCFFF190940A32A00BAD5355000000000003FFC0000000000FFF3FFF553FF
      FFCFCFFF096A3950930097734DA000000000003FF80000780000FFF3FFF0003F
      FFCFCFFF24918C152480E92DEED000000000003FF80003FE80007FF3FFF0001F
      FFCFCFFF262443489900D7B6527800000000007FF00007FF80007FF3FFF3FF8F
      FFE7CFFF1295695249406CED6D5000000000007FE0000FFFA0003FF3FFF3FFC7
      FFC7CFFF292292294240DAB6B6B400000000007FF0001FFFC0003FF3FFF3FFE7
      FFC7CFFF159424869A00B755B3D80000000000FFE0001FFFE0003FF3FFE3FFE7
      FFE7CFFF10A95B521300B9B54AB40000000000FFE0003FFFF0005FF3FFF3FFE7
      FFC7CFFF1656A465A2C0A6AAB5AC0000000000FFC0003FFFF001BFF3FFF3FFE7
      FFCFCFFF12844A104800DB5DB66E0000000001FFC0003FFFF007FFF3FFF3FFE7
      FFCFCFFF08A8A54C2A00ADA6CBB40006ED8001FFC0007FFFF87FFFF3FFF3FFC7
      FFCFCFFF154A528B9280D4F56D5A0007FF0003FFC0003FFFFDFFFFF3FFE3FF0F
      FF8FCFFF924908B05240BB2B566B0007FE8001FFE0007FFFFBFFFFF3FFF0001F
      FF9FCFFF892AB50525009DDAED9D0007FF0003FFC0007FFFFFFFFFF3FFF0007F
      FF1FCFFF8D1085524440D6DD356A8001FF0007FF80003FFFFFFFFFF3FFFFFFFF
      FE3FCFFF81AB48AAB340EB4BB6B70003FE0007FFC0007FFFFFFFFFF3FFFFFFFF
      FE3FCFFFC85032450880ADB95B598003FE0007FFC0007FFFFFFFFFF3FFFFFFFF
      F87FCFFFC64B4D326440D9B7B2AEC002FC0007FF80007FFFFFFFFFF3FFFFFFFF
      E1FFCFFFE104A4489500AECADDD34001FE000FFFC0007FFFFFFFFFF3FFFFFFFF
      03FFCFFFE00225961240D57B4ADD2001FC000FFFC0003FFFFFFFFFF3FFFFFFFE
      0FFFCFFFF8019242C080ADB36D22C000FC000FFFC0007FFFFFFFFFF3FFFFFFFF
      87FFCFFFFFF052689C40D69ED7BFA000F8001FFFC0007FFFFFFFFFF3FFFFFFFF
      E1FFCFFFFFF224852280DB656AD0C000FC001FFFC0003FFFFFFFFFF3FFF005FF
      F9FFCFFFFFF195354900B976DD3B3000F8002FFFC0003FFFFFFFFFF3FFF0007F
      FCFFCFFE3FF898828A40A73B26D5B00070003FFFC0007FFFFFFFFFF3FFF3D83F
      FC7FCFFE3FF842946100DBCCF6D4C00078003FFFE0003FFFF7FFFFF3FFF3FF3F
      FE7FCFFE1FF8AA521480B46D995AB80070003FFFC0003FFFF2DFFFF3FFF3FF9F
      FE3FCFFE1FFC95499500AFB6D6CFC80050007FFFE0001FFFF057FFF3FFF3FF9F
      FF3F8FFE0FFC42254880D2ADD5D1680020007FFFE0001FFFE0005FF3FFF3FF9F
      FF3F8FFE47FC28A22B00B9DA2A7EB4003000FFFFE00007FFD0007FF3FFF3FF9F
      FF3F400021E05532C040DEB5B74B54002000FFFFF0000FFFA0003FF3FFF3FF9F
      FF1E000040012A883640A6CEE969A8000000FFFFF00005FF80007FF3FFE3FF1F
      FF3CEDEDA80484534880DB525EB5B6000000FFFFF800017E0000FFF3FFF3FE3F
      FF3D2010551A92248200B56FA9D6B4000001FFFFF00000000000FFF3FFF0007F
      FF3A36AA8AC266C8B340CED9556956000001FFFFFC0000000000FFF3FFF001FF
      FF31CAD468A491150880F4DAB7BDD5000003FFFFFC0000000001FFF3FFFFFFFF
      FF2C112A95192A2A4440ABADDA4E6C000003FFFFFE0000000003FFF3FFFFFFFF
      FE259D23292522D0A680956B5B6B5B000003FFFFFF0000000001FFF3FFFFFFFF
      FE10C2548AD248869100F5D6B6D2A6800007FFFFFF0000000007FFF3FFFFFFFF
      FC5A6954D4192C514C80BB35ACDDB6800007FFFFFF800000000FFFF3FFFFFFFF
      F8252A8A54C325A92440D6AA5732D5400007FFFFFFE00000001FFFF3FFFFFFFF
      F88A94612A5492442900B5EDF35A6B40000BFFFFFFF00000003FFFF3FFFFFFFF
      E1D14936C4A8A42AC940B6569CEDB680000DFFFFFFF8000000FFFFF3FFFFFFFF
      C22C9484550AA2A24480DBB9A7AA6AC0000CFFFFFFFE000001FFFFF1FFFFFFFC
      0455656492D35514A480AD4DB95B556104377FFFFFFFC00017FFFFF000000000
      1BA2921A523409892A00D56ACEB5EF3EFFC93FFFFFFFFC48BFFFFFF000000000
      649A4AC52488C8654280B3B6755433ADAB5A9FFFFFFFFFFFFFFFFFFFFFFFFFFF
      88496932934695249240BDBD95ABB5D2B12FA7FFFFFFFFFFFFFFFFFFFFFFFFFC
      57A68A48A4A922989080CA45DB5ECC6B2ED16BFFFFFFFFFFFFFFFFFFFFFFFFF2
      923154954A154A452A00B6FA6DB27775A6DDA8FFFFFFFFFFFFFFFFFFFFFFFFE5
      694A654A95A125354A80DB5F4CBBAB16752B2F3FFFFFFFFFFFFFFFFFFFFFFFDA
      4AAA1255625522229140CAC5F39B55EB6DD8D29FFFFFFFFFFFFFFFFFFFFFFF12
      9495AC8A190AD2C82A40DDB96D6DB559924ED9B7FFFFFFFFFFFFFFFFFFFFFE64
      A692636495B00A36C840B6B695B2CAAADDB5AD65FFFFFFFFFFFFFFFFFFFFF1B6
      496A08AAC22B69412680A9DB5B2DBAF6CDCD532AFFFFFFFFFFFFFFFFFFFFE813
      5A4AB492594495949100DE95EDDAAB4D346669593FFFFFFFFFFFFFFFFFFF9D49
      25954B152A944822AA40B76C5565B4D6D6D5BAD6A7FFFFFFFFFFFFFFFFFD26CA
      4A4924EA912325520500A96FAB3B6EA9AB6CCD2AB1FFFFFFFFFFFFFFFFF4D255
      A9549249269894955080DB92DDCD556A72B365D95CBFFFFFFFFFFFFFFFAAA5A8
      A66A649268AC92A09A40ACF9525B6B779B5A5AA6A64FFFFFFFFFFFFFFE09291A
      52932A4D8C912A4D2300DB5EADA6592CCDAE966955B0FFFFFFFFFFFFE1F552D3
      2C94A561124A92A49100EDA6F6DD9BC9DAB56B56B6AD1FFFFFFFFFFE542A9AA4
      A569149AE292448948C0A5755B66CA6E2D4B73B6A956C4FFFFFFFFE29B456554
      CA16CA491A69AA564A00FDBAA4F2B555E6EC8A495AA56A937FFFEC1A49665296
      51C852A44912492149009757BB2EDBB6D4B3756CB673354C808013B2A531A959
      2C52A956D4A5308C6480EAECDD66B6D92EDD6B57274ACD6CDA5DD24D6AA894A5
      532D4A48129A4D6206409DB366D9CCA76692A4A9D95932C66DE4B954B5566B4A
      448924A765249213B240EB36B55B5ADAB2EB5F6A93A5B52B02164D2A4925496A
      BAD4B25012A149645100A5CF776CCF554D29A896EC55AD72EDD2A2CB5ADA4A29
      035656A5694A6A582480DA688A453226EAB6A5594AD24A8C566D54D492AAA98D
      58A2452D963492459A80}
    MsgAppQRCode = 
      'Consulte o QR Code pelo aplicativo  "De olho na nota", dispon'#237've' +
      'l na AppStore (Apple) e PlayStore (Android)'
    EspacoFinal = 50
    LogoWidth = 100
    LogoHeigth = 70
    Left = 64
    Top = 344
  end
  object PrintDialog1: TPrintDialog
    Left = 608
    Top = 256
  end
  object SaveDialog1: TSaveDialog
    Left = 261
    Top = 280
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    Modelo = ppEscPosEpson
    PaginaDeCodigo = pcNone
    EspacoEntreLinhas = 40
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 7
    ArqLOG = 'c:\temp\posprinter.log'
    Left = 103
    Top = 279
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport Community Edition v4.0 \251 Copyright ?1999-2016 Fo' +
      'rtes Inform?ca'
    DisplayName = 'Documento PDF'
    Left = 188
    Top = 315
  end
  object ACBrIntegrador1: TACBrIntegrador
    OnGravarLog = ACBrSAT1GravarLog
    PastaInput = 'C:\Integrador\Input\'
    PastaOutput = 'C:\Integrador\Output\'
    Left = 100
    Top = 344
  end
end
