object fmPrincipal: TfmPrincipal
  Left = 241
  Top = 173
  ActiveControl = PageControl1
  Caption = 'SAT Teste - Projeto ACBr'
  ClientHeight = 529
  ClientWidth = 954
  Color = clBtnFace
  Constraints.MinHeight = 460
  Constraints.MinWidth = 850
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 289
    Width = 954
    Height = 6
    Cursor = crVSplit
    Align = alTop
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 295
    Width = 954
    Height = 211
    ActivePage = tsLog
    Align = alClient
    TabOrder = 1
    object tsLog: TTabSheet
      Caption = 'Log de Comandos'
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 946
        Height = 183
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsGerado: TTabSheet
      Caption = 'XML Gerado'
      inline mVendaEnviar: TMemo
        Left = 0
        Top = 0
        Width = 946
        Height = 183
        Cursor = crIBeam
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsRecebido: TTabSheet
      Caption = 'XML Recebido'
      inline mRecebido: TWebBrowser
        Left = 0
        Top = 0
        Width = 946
        Height = 183
        Cursor = crIBeam
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000C6610000EA1200000000000000000000000000000000000000000000
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
        Width = 946
        Height = 158
        Cursor = crIBeam
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 158
        Width = 946
        Height = 25
        Align = alBottom
        TabOrder = 1
        object Label18: TLabel
          Left = 1
          Top = 1
          Width = 105
          Height = 23
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
        Width = 946
        Height = 183
        Cursor = crIBeam
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000C6610000EA1200000000000000000000000000000000000000000000
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
    Width = 954
    Height = 289
    Align = alTop
    TabOrder = 0
    object gpOperacao: TGroupBox
      Left = 1
      Top = 1
      Width = 169
      Height = 287
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
        TabOrder = 0
        OnChange = cbxModeloChange
        Items.Strings = (
          '')
      end
      object GroupBox2: TGroupBox
        Left = 2
        Top = 196
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
      Width = 783
      Height = 287
      Align = alClient
      Caption = 'Configura'#231#227'o'
      TabOrder = 1
      object PageControl2: TPageControl
        Left = 2
        Top = 15
        Width = 779
        Height = 270
        ActivePage = Impressao
        Align = alClient
        TabOrder = 0
        object tsDadosSAT: TTabSheet
          Caption = 'Dados do SAT CFe'
          DesignSize = (
            771
            242)
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
            Left = 542
            Top = 6
            Width = 44
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Ambiente'
            Color = clBtnFace
            ParentColor = False
          end
          object sbNomeDLL: TSpeedButton
            Left = 498
            Top = 25
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbNomeDLLClick
          end
          object Label8: TLabel
            Left = 204
            Top = 164
            Width = 72
            Height = 13
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
            Width = 277
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
            Left = 543
            Top = 23
            Width = 152
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
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
            Left = 431
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
            Left = 280
            Top = 160
            Width = 85
            Height = 22
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
            TabOrder = 4
          end
          object cbxIndRatISSQN: TComboBox
            Left = 336
            Top = 87
            Width = 134
            Height = 21
            Style = csDropDownList
            TabOrder = 5
          end
          object cbxRegTributario: TComboBox
            Left = 12
            Top = 87
            Width = 166
            Height = 21
            Style = csDropDownList
            TabOrder = 3
          end
        end
        object tsDadosSwHouse: TTabSheet
          Caption = 'Dados Sw.House'
          DesignSize = (
            771
            242)
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
            Width = 353
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edtSwHAssinatura: TEdit
            Left = 10
            Top = 89
            Width = 618
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
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          object Label80: TLabel
            Left = 2
            Top = 2
            Width = 137
            Height = 20
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = 'Motor Relat'#243'rio *'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
          object Label83: TLabel
            Left = 452
            Top = 5
            Width = 252
            Height = 16
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Caption = '*descomente no projeto o motor desejado'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object grpFortesFast: TGroupBox
            Left = 8
            Top = 27
            Width = 335
            Height = 187
            Caption = 'Fortes / Fast '
            TabOrder = 0
            DesignSize = (
              335
              187)
            object Label19: TLabel
              Left = 16
              Top = 16
              Width = 46
              Height = 16
              Caption = 'Largura'
              Color = clBtnFace
              ParentColor = False
            end
            object Label20: TLabel
              Left = 104
              Top = 16
              Width = 33
              Height = 16
              Caption = 'Topo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label21: TLabel
              Left = 16
              Top = 64
              Width = 38
              Height = 16
              Caption = 'Fundo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label22: TLabel
              Left = 104
              Top = 64
              Width = 59
              Height = 16
              Caption = 'Esquerda'
              Color = clBtnFace
              ParentColor = False
            end
            object Label23: TLabel
              Left = 192
              Top = 64
              Width = 39
              Height = 16
              Caption = 'Direita'
              Color = clBtnFace
              ParentColor = False
            end
            object lImpressora: TLabel
              Left = 146
              Top = 161
              Width = 120
              Height = 16
              Caption = 'Impresssora Default'
              Color = clBtnFace
              ParentColor = False
            end
            object sbFR3: TSpeedButton
              Left = 301
              Top = 131
              Width = 24
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              OnClick = sbFR3Click
            end
            object Label36: TLabel
              Left = 16
              Top = 115
              Width = 81
              Height = 16
              Caption = 'Caminho FR3'
              Color = clBtnFace
              ParentColor = False
            end
            object seLargura: TSpinEdit
              Left = 16
              Top = 32
              Width = 64
              Height = 26
              MaxValue = 9999
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
            object seMargemTopo: TSpinEdit
              Left = 104
              Top = 32
              Width = 64
              Height = 26
              MaxValue = 9999
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object seMargemFundo: TSpinEdit
              Left = 16
              Top = 82
              Width = 64
              Height = 26
              MaxValue = 9999
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object seMargemEsquerda: TSpinEdit
              Left = 104
              Top = 82
              Width = 64
              Height = 26
              MaxValue = 9999
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object seMargemDireita: TSpinEdit
              Left = 192
              Top = 82
              Width = 64
              Height = 26
              MaxValue = 9999
              MinValue = 0
              TabOrder = 5
              Value = 0
            end
            object bImpressora: TButton
              Left = 16
              Top = 158
              Width = 122
              Height = 25
              Caption = 'Definir Impressora'
              TabOrder = 6
              OnClick = bImpressoraClick
            end
            object cbPreview: TCheckBox
              Left = 200
              Top = 42
              Width = 81
              Height = 19
              Caption = 'Preview'
              TabOrder = 2
            end
            object edtPathFR3: TEdit
              Left = 16
              Top = 131
              Width = 279
              Height = 24
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 7
            end
          end
          object gbESCPOS: TGroupBox
            Left = 353
            Top = 27
            Width = 322
            Height = 142
            Caption = ' ESCPOS '
            Enabled = False
            TabOrder = 1
            DesignSize = (
              322
              142)
            object Label25: TLabel
              Left = 155
              Top = 66
              Width = 39
              Height = 32
              Caption = 'Linhas'#13#10'Pular'
              Color = clBtnFace
              ParentColor = False
            end
            object Label26: TLabel
              Left = 82
              Top = 66
              Width = 54
              Height = 32
              Caption = 'Espa'#231'os'#13#10'Linhas'
              Color = clBtnFace
              ParentColor = False
            end
            object Label27: TLabel
              Left = 10
              Top = 81
              Width = 49
              Height = 16
              Caption = 'Colunas'
              Color = clBtnFace
              ParentColor = False
            end
            object Label28: TLabel
              Left = 8
              Top = 12
              Width = 46
              Height = 16
              Caption = 'Modelo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label7: TLabel
              Left = 158
              Top = 12
              Width = 32
              Height = 16
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object Label29: TLabel
              Left = 218
              Top = 81
              Width = 70
              Height = 16
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
              Height = 26
              MaxValue = 255
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object seEspLinhas: TSpinEdit
              Left = 82
              Top = 102
              Width = 41
              Height = 26
              MaxValue = 255
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object seColunas: TSpinEdit
              Left = 10
              Top = 102
              Width = 41
              Height = 26
              MaxValue = 999
              MinValue = 1
              TabOrder = 2
              Value = 48
            end
            object cbxModeloPosPrinter: TComboBox
              Left = 6
              Top = 32
              Width = 145
              Height = 24
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbxModeloChange
            end
            object cbxPorta: TComboBox
              Left = 158
              Top = 32
              Width = 128
              Height = 24
              TabOrder = 1
            end
            object cbxPagCodigo: TComboBox
              Left = 218
              Top = 102
              Width = 93
              Height = 24
              Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
              Style = csDropDownList
              TabOrder = 5
            end
          end
          object cbLogoLateral: TCheckBox
            Left = 8
            Top = 220
            Width = 98
            Height = 19
            Caption = 'Logo Lateral'
            TabOrder = 2
          end
          object cbQRCodeLateral: TCheckBox
            Left = 112
            Top = 220
            Width = 122
            Height = 19
            Caption = 'QRCode Lateral'
            TabOrder = 3
          end
          object cbImprimir1Linha: TCheckBox
            Left = 240
            Top = 220
            Width = 130
            Height = 19
            Caption = 'Item em 1 linha'
            TabOrder = 4
          end
          object cbImprimirDescAcres: TCheckBox
            Left = 368
            Top = 220
            Width = 162
            Height = 19
            Caption = 'Imprime Desc/Acres'
            TabOrder = 5
          end
          object cbImprimirChaveUmaLinha: TCheckBox
            Left = 536
            Top = 220
            Width = 137
            Height = 19
            Caption = 'Chave em 1 linha'
            TabOrder = 6
          end
          object cbxMotorRelatorio: TComboBox
            Left = 145
            Top = 2
            Width = 297
            Height = 24
            Margins.Left = 5
            Margins.Top = 5
            Margins.Right = 5
            Margins.Bottom = 5
            Style = csDropDownList
            TabOrder = 7
            OnChange = cbxMotorRelatorioChange
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
            Value = 0
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
            771
            242)
          object Label33: TLabel
            Left = 16
            Top = 14
            Width = 123
            Height = 13
            Alignment = taRightJustify
            Caption = 'Schema Venda Aplica'#231#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object sbSchemaVendaAPL: TSpeedButton
            Left = 654
            Top = 33
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbSchemaVendaAPLClick
          end
          object sbSchemaVendaSAT: TSpeedButton
            Left = 654
            Top = 81
            Width = 24
            Height = 22
            Anchors = [akTop, akRight]
            Caption = '...'
            OnClick = sbSchemaVendaSATClick
          end
          object Label34: TLabel
            Left = 16
            Top = 62
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
            Width = 635
            Height = 21
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edSchemaVendaSAT: TEdit
            Left = 16
            Top = 81
            Width = 635
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
            TabOrder = 2
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 506
    Width = 954
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
    Left = 872
    Top = 424
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
      object ConsultarltimaSessoFiscal1: TMenuItem
        Caption = 'Consultar '#218'ltima Sess'#227'o Fiscal'
        OnClick = ConsultarltimaSessoFiscal1Click
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
      object N1: TMenuItem
        Caption = '-'
      end
      object CarregarLote1: TMenuItem
        Caption = 'Carregar Lote'
        OnClick = CarregarLote1Click
      end
    end
    object mLimpar: TMenuItem
      Caption = 'Limpar'
      OnClick = mLimparClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 752
    Top = 432
  end
  object ACBrSAT1: TACBrSAT
    Integrador = ACBrIntegrador1
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
    Config.XmlSignLib = xsLibXml2
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
    Left = 32
    Top = 328
  end
  object ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    Site = 'http://www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    ExpandeLogoMarcaConfig.Altura = 0
    ExpandeLogoMarcaConfig.Esquerda = 0
    ExpandeLogoMarcaConfig.Topo = 0
    ExpandeLogoMarcaConfig.Largura = 0
    ExpandeLogoMarcaConfig.Dimensionar = False
    ExpandeLogoMarcaConfig.Esticar = True
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    CasasDecimais.Aliquota = 2
    CasasDecimais.MaskAliquota = ',0.00'
    MsgAppQRCode = 
      'Consulte o QR Code pelo aplicativo  "De olho na nota", dispon'#237've' +
      'l na AppStore (Apple) e PlayStore (Android)'
    ImprimeDescAcrescItem = False
    FormularioContinuo = True
    PosPrinter = ACBrPosPrinter1
    Left = 176
    Top = 432
  end
  object PrintDialog1: TPrintDialog
    Left = 808
    Top = 400
  end
  object SaveDialog1: TSaveDialog
    Left = 701
    Top = 408
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
    Left = 247
    Top = 407
  end
  object RLPDFFilter1: TRLPDFFilter
    DocumentInfo.Creator = 
      'FortesReport Community Edition v4.0 \251 Copyright ?1999-2016 Fo' +
      'rtes Inform?ca'
    DisplayName = 'Documento PDF'
    Left = 436
    Top = 363
  end
  object ACBrIntegrador1: TACBrIntegrador
    PastaInput = 'C:\Integrador\Input\'
    PastaOutput = 'C:\Integrador\Output\'
    Left = 44
    Top = 392
  end
end
