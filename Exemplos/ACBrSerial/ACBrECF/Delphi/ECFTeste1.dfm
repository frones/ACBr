object Form1: TForm1
  Left = 382
  Top = 218
  Width = 806
  Height = 447
  VertScrollBar.Range = 59
  ActiveControl = PageControl1
  Caption = 'Teste de Impressora Fiscal'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 440
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 369
    Width = 790
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 300
      end
      item
        Width = 50
      end>
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 790
    Height = 329
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 0
    object TabSheet4: TTabSheet
      Caption = 'ECF'
      ImageIndex = 3
      DesignSize = (
        782
        301)
      object SbArqLog: TSpeedButton
        Left = 759
        Top = 216
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = SbArqLogClick
      end
      object Label1: TLabel
        Left = 20
        Top = 8
        Width = 35
        Height = 13
        Caption = 'Modelo'
      end
      object Label4: TLabel
        Left = 148
        Top = 8
        Width = 25
        Height = 13
        Caption = 'Porta'
      end
      object Label5: TLabel
        Left = 267
        Top = 8
        Width = 40
        Height = 13
        Caption = 'TimeOut'
      end
      object Label7: TLabel
        Left = 331
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Intervalo'
      end
      object Label6: TLabel
        Left = 11
        Top = 141
        Width = 104
        Height = 13
        Caption = 'Mensagem Aguarde...'
      end
      object Label9: TLabel
        Left = 10
        Top = 224
        Width = 40
        Height = 13
        Alignment = taRightJustify
        Caption = 'Arq.Log:'
      end
      object Label20: TLabel
        Left = 3
        Top = 256
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Operador:'
      end
      object Label21: TLabel
        Left = 592
        Top = 141
        Width = 124
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Mensagem Trabalhando...'
      end
      object Label22: TLabel
        Left = 403
        Top = 8
        Width = 62
        Height = 13
        Caption = 'Linhas Buffer'
      end
      object Label29: TLabel
        Left = 484
        Top = 8
        Width = 14
        Height = 13
        Caption = 'UF'
      end
      object Label46: TLabel
        Left = 545
        Top = 5
        Width = 87
        Height = 13
        Caption = 'P'#225'gina de C'#243'digo:'
        Color = clBtnFace
        ParentColor = False
      end
      object cbxModelo: TComboBox
        Left = 20
        Top = 25
        Width = 105
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = cbxModeloChange
        Items.Strings = (
          'Procurar'
          'ecfNaoFiscal'
          'ecfBematech'
          'ecfSweda'
          'ecfDaruma'
          'ecfSchalter'
          'ecfMecaf'
          'ecfYanco'
          'ecfDataRegis'
          'ecfUrano'
          'ecfICash'
          'ecfQuattro')
      end
      object cbxPorta: TComboBox
        Left = 148
        Top = 25
        Width = 105
        Height = 21
        TabOrder = 5
        Text = 'Procurar'
        OnChange = cbxPortaChange
      end
      object chTentar: TCheckBox
        Left = 11
        Top = 68
        Width = 121
        Height = 17
        Caption = 'Tentar Novamente'
        TabOrder = 6
        OnClick = chTentarClick
      end
      object chBloqueia: TCheckBox
        Left = 11
        Top = 90
        Width = 145
        Height = 17
        Caption = 'Bloqueia Mouse Teclado'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = chBloqueiaClick
      end
      object chExibeMsg: TCheckBox
        Left = 11
        Top = 113
        Width = 131
        Height = 17
        Caption = 'Exibe Msg Aguarde...'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = chExibeMsgClick
      end
      object chArredondaPorQtd: TCheckBox
        Left = 195
        Top = 68
        Width = 187
        Height = 17
        Caption = 'Arredondamento por Quantidade'
        TabOrder = 7
        OnClick = chArredondaPorQtdClick
      end
      object chGavetaSinalInvertido: TCheckBox
        Left = 195
        Top = 90
        Width = 187
        Height = 17
        Caption = 'Gaveta Sinal Invertido'
        TabOrder = 9
        OnClick = chGavetaSinalInvertidoClick
      end
      object mMsg: TMemo
        Left = 11
        Top = 160
        Width = 564
        Height = 41
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 13
        OnChange = mMsgChange
      end
      object edLog: TEdit
        Left = 56
        Top = 216
        Width = 703
        Height = 21
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 15
        Text = 'acbrlog.txt'
        OnChange = edLogChange
      end
      object seTimeOut: TSpinEdit
        Left = 267
        Top = 24
        Width = 46
        Height = 22
        AutoSize = False
        MaxValue = 100
        MinValue = 1
        TabOrder = 0
        Value = 10
        OnChange = seTimeOutChange
      end
      object seIntervaloAposComando: TSpinEdit
        Left = 331
        Top = 24
        Width = 54
        Height = 22
        AutoSize = False
        Increment = 10
        MaxValue = 1000
        MinValue = 0
        TabOrder = 1
        Value = 100
        OnChange = seIntervaloAposComandoChange
      end
      object btSerial: TBitBtn
        Left = 687
        Top = 248
        Width = 88
        Height = 28
        Anchors = [akTop, akRight]
        Cancel = True
        Caption = 'Serial'
        Default = True
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
        ModalResult = 1
        TabOrder = 19
        OnClick = btSerialClick
      end
      object chDescricaoGrande: TCheckBox
        Left = 195
        Top = 113
        Width = 187
        Height = 17
        Caption = 'Descri'#231#227'o Grande'
        Checked = True
        State = cbChecked
        TabOrder = 12
        OnClick = chDescricaoGrandeClick
      end
      object edOperador: TEdit
        Left = 56
        Top = 248
        Width = 101
        Height = 21
        Cursor = crIBeam
        TabOrder = 18
        OnChange = edOperadorChange
      end
      object edMsgTrabalhando: TEdit
        Left = 590
        Top = 160
        Width = 175
        Height = 21
        Cursor = crIBeam
        Anchors = [akTop, akRight]
        TabOrder = 14
        Text = 'Impressora est'#225' trabalhando'
        OnClick = edMsgTrabalhandoChange
      end
      object Button1: TButton
        Left = 169
        Top = 246
        Width = 101
        Height = 25
        Caption = 'Fontes do ECF'
        TabOrder = 16
        OnClick = Button1Click
      end
      object speLinBuf: TSpinEdit
        Left = 403
        Top = 24
        Width = 65
        Height = 22
        AutoSize = False
        MaxValue = 1000
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = speLinBufChange
      end
      object btnIdentificaPafECF: TButton
        Left = 276
        Top = 246
        Width = 101
        Height = 25
        Caption = 'Identifica PAF-ECF'
        TabOrder = 17
        OnClick = btnIdentificaPafECFClick
      end
      object chArredondamentoItemMFD: TCheckBox
        Left = 380
        Top = 111
        Width = 147
        Height = 19
        Caption = 'Arredondamento Item MFD'
        TabOrder = 10
        OnClick = chArredondamentoItemMFDClick
      end
      object cbxUF: TComboBox
        Left = 484
        Top = 24
        Width = 47
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnChange = cbxModeloChange
        Items.Strings = (
          'AC'
          'AL'
          'AM'
          'AP'
          'BA'
          'CE'
          'DF'
          'ES'
          'GO'
          'MA'
          'MG'
          'MS'
          'MT'
          'PA'
          'PB'
          'PE'
          'PI'
          'PR'
          'RJ'
          'RN'
          'RO'
          'RR'
          'RS'
          'SC'
          'SE'
          'SP'
          'TO')
      end
      object chbCupomMania: TCheckBox
        Left = 171
        Top = 281
        Width = 187
        Height = 17
        Caption = 'Cupom Mania'
        TabOrder = 20
        OnClick = chbCupomManiaClick
      end
      object sePaginaCodigo: TSpinEdit
        Left = 545
        Top = 24
        Width = 87
        Height = 22
        MaxValue = 9999999
        MinValue = 0
        TabOrder = 21
        Value = 0
        OnChange = sePaginaCodigoChange
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Cmd/Resp'
      object Label2: TLabel
        Left = 0
        Top = 54
        Width = 782
        Height = 17
        Align = alTop
        AutoSize = False
        Caption = 'Resposta'
        Layout = tlBottom
      end
      object Label17: TLabel
        Left = 0
        Top = 0
        Width = 782
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = 'Comando Enviado'
        Layout = tlBottom
      end
      object mResp: TMemo
        Left = 0
        Top = 71
        Width = 782
        Height = 230
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 13
        Font.Name = 'Courier'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WantReturns = False
        WordWrap = False
      end
      object mEnviado: TMemo
        Left = 0
        Top = 15
        Width = 782
        Height = 39
        Align = alTop
        TabOrder = 0
        WantReturns = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Cupom'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 41
        Width = 782
        Height = 260
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object mBobina: TMemo
          Left = 0
          Top = 0
          Width = 782
          Height = 260
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Fixedsys'
          Font.Pitch = fpVariable
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          Visible = False
          WordWrap = False
        end
        object wbBobina: TWebBrowser
          Left = 0
          Top = 0
          Width = 782
          Height = 260
          Align = alClient
          TabOrder = 1
          ControlData = {
            4C000000D2500000DF1A00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 782
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object cbMemoHTML: TCheckBox
          Left = 272
          Top = 9
          Width = 73
          Height = 21
          Caption = 'HTML'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = cbMemoHTMLClick
        end
        object bBobinaParams: TButton
          Left = 88
          Top = 8
          Width = 75
          Height = 24
          Caption = 'Parametros'
          TabOrder = 0
          OnClick = bBobinaParamsClick
        end
        object bBobinaLimpar: TButton
          Left = 176
          Top = 8
          Width = 75
          Height = 24
          Caption = 'Limpar'
          TabOrder = 1
          OnClick = bBobinaLimparClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'RFD'
      ImageIndex = 2
      object PageControl2: TPageControl
        Left = 0
        Top = 55
        Width = 782
        Height = 246
        ActivePage = TabSheet6
        Align = alClient
        TabOrder = 1
        object TabSheet6: TTabSheet
          Caption = 'Sw.House'
          ImageIndex = 1
          object Label8: TLabel
            Left = 12
            Top = 10
            Width = 63
            Height = 13
            Caption = 'Razao Social'
          end
          object Label13: TLabel
            Left = 276
            Top = 10
            Width = 23
            Height = 13
            Caption = 'COO'
          end
          object Label10: TLabel
            Left = 12
            Top = 51
            Width = 27
            Height = 13
            Caption = 'CNPJ'
          end
          object Label11: TLabel
            Left = 143
            Top = 51
            Width = 64
            Height = 13
            Caption = 'Insc.Estadual'
          end
          object Label12: TLabel
            Left = 276
            Top = 51
            Width = 68
            Height = 13
            Caption = 'Insc.Municipal'
          end
          object Label14: TLabel
            Left = 12
            Top = 91
            Width = 46
            Height = 13
            Caption = 'Aplicativo'
          end
          object Label15: TLabel
            Left = 192
            Top = 91
            Width = 37
            Height = 13
            Caption = 'Numero'
          end
          object Label16: TLabel
            Left = 276
            Top = 91
            Width = 33
            Height = 13
            Caption = 'Vers'#227'o'
          end
          object Label18: TLabel
            Left = 12
            Top = 132
            Width = 35
            Height = 13
            Caption = 'Linha 1'
          end
          object Label19: TLabel
            Left = 204
            Top = 132
            Width = 35
            Height = 13
            Caption = 'Linha 2'
          end
          object edSH_RazaoSocial: TEdit
            Left = 12
            Top = 26
            Width = 245
            Height = 21
            Cursor = crIBeam
            TabOrder = 0
            OnChange = edSH_RazaoSocialChange
          end
          object edSH_COO: TEdit
            Left = 276
            Top = 26
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 1
            OnChange = edSH_COOChange
          end
          object edSH_CNPJ: TEdit
            Left = 12
            Top = 66
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 5
            OnChange = edSH_CNPJChange
          end
          object edSH_IE: TEdit
            Left = 144
            Top = 66
            Width = 114
            Height = 21
            Cursor = crIBeam
            TabOrder = 7
            OnChange = edSH_IEChange
          end
          object edSH_IM: TEdit
            Left = 276
            Top = 66
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 9
            OnChange = edSH_IMChange
          end
          object edSH_Aplicativo: TEdit
            Left = 12
            Top = 106
            Width = 169
            Height = 21
            Cursor = crIBeam
            TabOrder = 2
            OnChange = edSH_AplicativoChange
          end
          object edSH_NumeroAP: TEdit
            Left = 192
            Top = 106
            Width = 65
            Height = 21
            Cursor = crIBeam
            TabOrder = 3
            OnChange = edSH_NumeroAPChange
          end
          object edSH_VersaoAP: TEdit
            Left = 276
            Top = 106
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 4
            OnChange = edSH_VersaoAPChange
          end
          object edSH_Linha1: TEdit
            Left = 12
            Top = 146
            Width = 181
            Height = 21
            Cursor = crIBeam
            TabOrder = 6
            OnChange = edSH_Linha1Change
          end
          object edSH_Linha2: TEdit
            Left = 208
            Top = 146
            Width = 189
            Height = 21
            Cursor = crIBeam
            TabOrder = 8
            OnChange = edSH_Linha2Change
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'ACBrRFD.INI'
          object Panel4: TPanel
            Left = 0
            Top = 184
            Width = 774
            Height = 34
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            object bRFDLer: TButton
              Left = 131
              Top = 6
              Width = 75
              Height = 25
              Caption = 'Ler'
              TabOrder = 0
              OnClick = bRFDLerClick
            end
            object bRFDSalvar: TButton
              Left = 219
              Top = 6
              Width = 75
              Height = 25
              Caption = 'Salvar'
              TabOrder = 1
              OnClick = bRFDSalvarClick
            end
          end
          object mRFDParam: TMemo
            Left = 0
            Top = 0
            Width = 774
            Height = 184
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 12
            Font.Name = 'Fixedsys'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 782
        Height = 55
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          782
          55)
        object sbDirRFD: TSpeedButton
          Left = 757
          Top = 23
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          Caption = '...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = 11
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = sbDirRFDClick
        end
        object Label3: TLabel
          Left = 134
          Top = 7
          Width = 107
          Height = 13
          Caption = 'Diret'#243'rio arquivos RFD'
        end
        object chRFD: TCheckBox
          Left = 24
          Top = 13
          Width = 82
          Height = 31
          Caption = 'Gerar RFD'
          TabOrder = 0
          OnClick = chRFDClick
        end
        object edDirRFD: TEdit
          Left = 134
          Top = 23
          Width = 623
          Height = 18
          Cursor = crIBeam
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = 11
          Font.Name = 'Arial'
          Font.Pitch = fpVariable
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = edDirRFDChange
        end
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Dados RZ'
      ImageIndex = 6
      object Label37: TLabel
        Left = 0
        Top = 31
        Width = 782
        Height = 21
        AutoSize = False
        Caption = 'Resposta'
        Layout = tlBottom
      end
      object btnDadosRZ: TButton
        Left = 11
        Top = 3
        Width = 108
        Height = 25
        Caption = 'Ler Dados RZ'
        TabOrder = 0
        OnClick = btnDadosRZClick
      end
      object mRZ: TMemo
        Left = 0
        Top = 54
        Width = 782
        Height = 247
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 13
        Font.Name = 'Courier'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WantReturns = False
        WordWrap = False
      end
      object btnDadosUltimaRZ: TButton
        Left = 152
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Ler Dados '#218'ltima RZ'
        TabOrder = 2
        OnClick = btnDadosUltimaRZClick
      end
    end
    object tbsMenuFiscal: TTabSheet
      Caption = 'Menu Fiscal'
      ImageIndex = 6
      object grpMenuFiscalOpcoes: TGroupBox
        Left = 0
        Top = 0
        Width = 782
        Height = 233
        Align = alClient
        Caption = 'Op'#231#245'es do Menu fiscal'
        TabOrder = 0
        object Label31: TLabel
          Left = 10
          Top = 165
          Width = 269
          Height = 13
          Caption = 'Para os menus que geram arquivos, verificar o DemoPAF'
          Font.Charset = ANSI_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = []
          ParentFont = False
        end
        object btnMenuFiscalLX: TButton
          Left = 10
          Top = 20
          Width = 171
          Height = 25
          Caption = 'LX'
          TabOrder = 0
          OnClick = btnMenuFiscalLXClick
        end
        object btnMenuFiscalLMFC: TButton
          Left = 10
          Top = 51
          Width = 171
          Height = 25
          Caption = 'LMFC'
          TabOrder = 1
          OnClick = btnMenuFiscalLMFCClick
        end
        object btnMenuFiscalLMFS: TButton
          Left = 10
          Top = 82
          Width = 171
          Height = 25
          Caption = 'LMFS'
          TabOrder = 2
          OnClick = btnMenuFiscalLMFSClick
        end
        object btnMenuFiscalMFDEspelho: TButton
          Left = 187
          Top = 20
          Width = 171
          Height = 25
          Caption = 'Espelho MFD'
          TabOrder = 3
          OnClick = btnMenuFiscalMFDEspelhoClick
        end
        object btnMenuFiscalMFDArq: TButton
          Left = 187
          Top = 51
          Width = 171
          Height = 25
          Caption = 'Arq. MFD'
          TabOrder = 4
          OnClick = btnMenuFiscalMFDArqClick
        end
        object btnMenuFiscalRelMeiosPagto: TButton
          Left = 364
          Top = 20
          Width = 171
          Height = 25
          Caption = 'Meios Pagto.'
          TabOrder = 5
          OnClick = btnMenuFiscalRelMeiosPagtoClick
        end
        object btnMenuFiscalRelDAVEmitidos: TButton
          Left = 364
          Top = 51
          Width = 171
          Height = 25
          Caption = 'DAV Emitidos'
          TabOrder = 6
          OnClick = btnMenuFiscalRelDAVEmitidosClick
        end
        object btnMenuFiscalRelIdentPAFECF: TButton
          Left = 364
          Top = 82
          Width = 171
          Height = 25
          Caption = 'Identifica'#231#227'o PAF-ECF'
          TabOrder = 7
          OnClick = btnMenuFiscalRelIdentPAFECFClick
        end
        object btnMenuFiscalConfigPAFECF: TButton
          Left = 364
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Configura'#231#245'es do PAF-ECF'
          TabOrder = 8
          OnClick = btnMenuFiscalConfigPAFECFClick
        end
        object btnMenuFiscalNotaPaulista: TButton
          Left = 187
          Top = 82
          Width = 171
          Height = 25
          Caption = 'CAT52'
          TabOrder = 9
          OnClick = btnMenuFiscalNotaPaulistaClick
        end
        object btnArqMFNovo: TButton
          Left = 10
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Arq.MF (novo)'
          TabOrder = 10
          OnClick = btnArqMFNovoClick
        end
        object btnArqMFDNovo: TButton
          Left = 187
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Arq.MFD (novo)'
          TabOrder = 11
          OnClick = btnArqMFDNovoClick
        end
      end
      object pgcMenuFiscalTipo: TPageControl
        Left = 0
        Top = 233
        Width = 782
        Height = 68
        ActivePage = tbsMenuFiscalTipoData
        Align = alBottom
        TabOrder = 3
        object tbsMenuFiscalTipoData: TTabSheet
          Caption = 'Emiss'#227'o por intervalo de data'
          object Label24: TLabel
            Left = 16
            Top = 15
            Width = 18
            Height = 13
            Alignment = taRightJustify
            Caption = 'de :'
          end
          object Label25: TLabel
            Left = 142
            Top = 15
            Width = 15
            Height = 13
            Caption = 'at'#233
          end
          object edtDtInicial: TDateTimePicker
            Left = 40
            Top = 10
            Width = 96
            Height = 21
            Date = 40620.448168067130000000
            Time = 40620.448168067130000000
            TabOrder = 0
          end
          object edtDtFinal: TDateTimePicker
            Left = 163
            Top = 10
            Width = 96
            Height = 21
            Date = 40620.448168067130000000
            Time = 40620.448168067130000000
            TabOrder = 1
          end
        end
        object tbsMenuFiscalTipoCOO: TTabSheet
          Caption = 'Emiss'#227'o por intervalo de COO'
          ImageIndex = 1
          object Label26: TLabel
            Left = 16
            Top = 15
            Width = 18
            Height = 13
            Alignment = taRightJustify
            Caption = 'de :'
          end
          object Label30: TLabel
            Left = 142
            Top = 15
            Width = 15
            Height = 13
            Caption = 'at'#233
          end
          object edtCOOInicial: TSpinEdit
            Left = 40
            Top = 10
            Width = 96
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 1
          end
          object edtCOOFinal: TSpinEdit
            Left = 163
            Top = 10
            Width = 96
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 1
          end
        end
      end
      object chkMenuFiscalCotepe1704: TCheckBox
        Left = 30
        Top = 211
        Width = 456
        Height = 17
        Caption = 
          'Gerar o arquivo no formato do ato Cotepe 17/04 (Somente para os ' +
          'menus: LMFC, Arq.MFD)'
        TabOrder = 2
      end
      object chkMenuFiscalGerarArquivo: TCheckBox
        Left = 10
        Top = 188
        Width = 468
        Height = 17
        Caption = 'Efetuar a gera'#231#227'o de arquivo'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsTagsImpressao: TTabSheet
      Caption = 'Tags de Impress'#227'o'
      ImageIndex = 6
      DesignSize = (
        782
        301)
      object Label28: TLabel
        Left = 84
        Top = 24
        Width = 34
        Height = 13
        Caption = 'Altura'
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label27: TLabel
        Left = 12
        Top = 24
        Width = 44
        Height = 13
        Caption = 'Largura'
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label23: TLabel
        Left = 0
        Top = 0
        Width = 782
        Height = 16
        Align = alTop
        Alignment = taCenter
        Caption = 'Tags de Formata'#231#227'o de Impress'#227'o e C'#243'digo de Barras'
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ExplicitWidth = 384
      end
      object speBarrasLargura: TSpinEdit
        Left = 12
        Top = 40
        Width = 46
        Height = 22
        AutoSize = False
        MaxValue = 6
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = speBarrasLarguraChange
      end
      object BitBtn6: TBitBtn
        Left = 576
        Top = 22
        Width = 133
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Enviar Linha Gerencial'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = BitBtn6Click
      end
      object MemoTesteTags: TMemo
        Left = 0
        Top = 97
        Width = 782
        Height = 204
        Align = alBottom
        Lines.Strings = (
          'TEXTO LIVRE'
          'At'#233' 600 caracteres'
          #193#201#205#211#218#225#233#237#243#250#231#199#227#245#195#213#202#234#192#224
          ''
          '</linha_dupla>'
          ''
          '<CE>*** TAGS DE FORMATA'#199#195'O ***</CE>'
          '<e>EXPANDIDO</e>'
          '<N>Negrito</n>'
          '<S>Sublinhado</s>'
          '<C>CONDENSADO</C>'
          '<i>ITALICO</I>'
          '</linha_simples>'
          ''
          '<CE>*** TAGS DE C'#211'DIGO DE BARRAS ***</CE>'
          'EAN 8:'
          '<ean8>1234567</ean8>'
          'EAN13:'
          '<ean13>123456789012</ean13>'
          'INT25:'
          '<inter>1234567890</inter>'
          'CODE39: '
          '<code39>ABCDE12345</code39>'
          'CODE93:'
          '<code93>ABC123abc</code93>'
          'CODE128:'
          '<code128>$-=+ABC123abc</code128>'
          'UPCA:'
          '<upca>12345678901</upca>'
          'CODABAR :'
          '<codabar>$12345</codabar>'
          '</linha_simples>'
          ''
          '<CE>*** TAGS DE FORMATA'#199#195'O ***</CE>'
          '<ce>NO CENTRO</CE>'
          '<AD>A Direira</ad>'
          '</linha_simples>'
          ''
          '<CE>*** TESTE DE TAGS INV'#193'LIDAS ***</CE>'
          '<ce> <>tags inv'#225'lidas no texto">">><<</CE>'
          '<AD><da><ec></</A Direira</ad>'
          ''
          '</linha_dupla>')
        ScrollBars = ssVertical
        TabOrder = 5
      end
      object chBarrasImprimeTexto: TCheckBox
        Left = 164
        Top = 40
        Width = 151
        Height = 19
        Caption = 'Imprime Texto na Barra'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
        State = cbChecked
        TabOrder = 3
        OnClick = chBarrasImprimeTextoClick
      end
      object speBarrasAltura: TSpinEdit
        Left = 84
        Top = 40
        Width = 46
        Height = 22
        AutoSize = False
        MaxValue = 200
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = speBarrasAlturaChange
      end
      object chIgnorarTagsFormatacao: TCheckBox
        Left = 164
        Top = 66
        Width = 183
        Height = 19
        Caption = 'Ignorar TAGs de Formata'#231#227'o'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = chIgnorarTagsFormatacaoClick
      end
    end
  end
  object pBotoes: TPanel
    Left = 0
    Top = 329
    Width = 790
    Height = 40
    Cursor = crHelp
    Hint = 'Sobre o ACBrMonitor ?'
    Align = alBottom
    TabOrder = 1
    TabStop = True
    DesignSize = (
      790
      40)
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 788
      Height = 38
      Align = alClient
      Picture.Data = {
        0A544A504547496D616765C9180000FFD8FFE000104A46494600010101004400
        440000FFFE0018437265617465642077697468205468652047494D5000FFDB00
        430006040506050406060506070706080A100A0A09090A140E0F0C1017141818
        171416161A1D251F1A1B231C1616202C20232627292A29191F2D302D28302528
        2928FFDB0043010707070A080A130A0A13281A161A2828282828282828282828
        2828282828282828282828282828282828282828282828282828282828282828
        28282828282828FFC00011080028040003012200021101031101FFC4001F0000
        010501010101010100000000000000000102030405060708090A0BFFC400B510
        0002010303020403050504040000017D01020300041105122131410613516107
        227114328191A1082342B1C11552D1F02433627282090A161718191A25262728
        292A3435363738393A434445464748494A535455565758595A63646566676869
        6A737475767778797A838485868788898A92939495969798999AA2A3A4A5A6A7
        A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE1E2
        E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301010101010101
        01010000000000000102030405060708090A0BFFC400B5110002010204040304
        0705040400010277000102031104052131061241510761711322328108144291
        A1B1C109233352F0156272D10A162434E125F11718191A262728292A35363738
        393A434445464748494A535455565758595A636465666768696A737475767778
        797A82838485868788898A92939495969798999AA2A3A4A5A6A7A8A9AAB2B3B4
        B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3E4E5E6E7E8E9
        EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00D9BFFF008FC9FF00
        EBA37F3AAAD56AFF00FE3F27FF00AE8DFCEB2759BD4D374BBABC93EEC11B3E3D
        703815F6B0F851F00D394ECBB961AAE697A45E6AB2EDB488951F7A46E157EA6B
        E7ED3FC51AFDADC586A77B7B732D83DC10D1B39DAE14A965C7D1ABED2B2B8B28
        7438EEEDB6476220F394AF4098CE7F2AF0788B37AB955387B28734A6DA4FA2FF
        0083D8FA2C0E46AACDFB59FBABB185A3F806D5E541A85CCB213C958B0A3F339F
        E95B93F8134548F315A1703AE646CFF3AF97F51F17F8D3E2978CDF4BF0C5CCF6
        F6C59BC982190C6AB18FE376156E1BBF899F0B3C5D656F792DEEA11CD87302BB
        5C473A67040EE0D713CB331AD4B93118AB557AF2A76B7DD6FC8F5234F0949FB9
        495BCF5FCFFCCF7FBAF04E9132911A4D01F54933FCF35CBEB1E04BFB656934F6
        FB646392AAB871F877AECBC5896DAB2786E596DD845752348F1480A9FF0052C7
        0C3D41AC1B8B0B38A4BFFB368227B7B04124F22CAAA4020B700919E057C852CF
        F32CBF13EC253752CAED3B3D2FDDB4FF0013BAAE5383C4D3E75151F35A7FC03C
        E6456462AE0AB038208C106A26AECFC4BE19B779B4E9F450C8F78E50C458957F
        DDB38201E87E5C7E3CF4AE2AE0B410B4D3417290AB98CCAD6F204DD9DB8DD8C6
        73C75EB5F7B95710E1332A0AB29723BD9A9349DFF5F23E5F199557C254E4B732
        EE9086A36A92559219BCAB8827B79768709344D192A7B8C81E951B57B34AAC2B
        454E9C934FAAD51C5284A9BE59AB3F318D519A7B530D58D119A61A79A8CD05A1
        869869E6A3348B430D30D3CD2DBF946E22FB497106F1E614196DB9E71EF8A19A
        223B886581944D13C65943A875232A790467B1F5A85D1D55599582B67692383F
        4AEBB50D7F4DD523985CDBCF6EEF1BC2ACA04BE5A798B22019DBD0875FA37B62
        9F79E24D3E79BE4371140925CF92BE4237941D9991C0DD8C8CE31DBA83C563ED
        25FCA6EA11EE712D51B576371E24B20B225ADB045659B936F1FCCE55423FB7CC
        A5B03A678A77F6FE87F64BD592C5E479DB794688005BE43904300BCAB7F09EBC
        632451ED25FCA528AEE7216D677177E67D961925F2D77BEC5CED5F53551ABBE8
        F56D274FD5A696FDCDF45308D955208CAA4477168C84700100AFA818231D2B03
        5AD52C6EF488ADEDA2612A98480615510858CAB80C0E5B7B10C72074A1546DEC
        5F2A4B731A7B1BB86D63B99AD678EDE4FB92BC642B7D0F43549ABAB7D5B4C7D6
        9B5377BADF306DD098159606319552A4B7CDB1882A081C0ED53CBE27B04F96DE
        DFE5227F319ADA3FDE39B68D237239C7EF519C8ED9CF26973CBB16A2BB9C5353
        0D76AFE24D29B4EBB8FEC4BF6A9A31B9DADC30918C0887A30DB8915DC1C1E5B3
        8C8C553D635FB4BF4D5045BED4CB712BC3B2DA33BE13F7236E46CDBC9E3392C4
        F614D4E4FA16A2BB9CC8B5B86BD166B139BA327942203E62F9C6DC7AE78A5B3D
        3AFAFD656B1B3B9B958B1BCC313384CE719C0E3A1FC8D7731F8AF408AE4CC967
        3822FC5E20F217298BA127CA77E07EEBE5C63AF7C562783F5EB3D1E19C5E6FDC
        6F2DAE5716C93644624C8F988DA7E71861EF53ED26D3762D455F73942688E379
        9CAC4A5982B3103D00249FC0026BB3D27C4FA641796AF776F234105B2C613C85
        70CFBF2E580652D95E324FD41150CDAFE8C6CD235825CC69711C68B02AA2878A
        5552DF31DCC19D3918E01E09C51CF2EC528A3929E39AD2EA48A65786E2172ACA
        7864607047B106BE85F85BAB5C6ADE10B796FA4324F13B42646392C07427DF07
        1F85781F88EFA3D4BC41A9DF401C43737324C81C6182B31233EFCD7B57C1564F
        F842FE6233F6993F90AE2CC95E8A6F7B9AD2F88F40DC3D451B87A8A66E8FD451
        BA3F515E09D23F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45003F70F5146E1EA299BA3F
        5146E8FD45003F70F5146E1EA299BA3F5146E8FD45007097DFF1F93FFD746FE7
        5E6BF19753FB36850582361EEE4CB0FF006179FE78AF4ABF205E4FCE3F78DFCE
        BCD7C59E0FD47C49E2DB6B8966B54D310C719DD21DC1339638C75E4D7D9C2AD3
        8A5CCD687C5E0D43EB1CD37648C1F10EA5E199BE10689A4D95E6ED72CE633CA9
        E530C97CEE1BB18E3E5FCABD93E14EAD2F897E036AD6C926EBDD3AD26B52BD49
        1B094FD38FC28D53E1AFC399B49BA4B08608AF1A2610C86793E57C70704FAD63
        FC08D1F53F026B1A80D5EEB4E6D32F61DAE127DF861D0E31E848AF98AF89C263
        70729252BC25CE94959B77BE9DD6FA1F56AA469CADCEB556D1AEBA7FC139EFD9
        166863F1F6A314ACA2692C888C1EA48604E3F0AFA2FC67F103C37E0FD56C6D7C
        4173F679AE919A37D9B828071CE3903FC2BE71F187C35B9B2F14BEABE00D5E05
        1248648E2598C72C24F50A7B8A6681F0AFC43E26F11437DE3BD50A5BA9065792
        46965751FC238E2BAF1186C266153EB92A9EEDB6DA5F76FF002B0A389504E09A
        BDFB9F47F89AF2DF5097C3377672ACB6D34B23C722F46530BE08AE7AFECD6FF5
        3BDF2B434BEF2563599DA709BF2090369E1B8F5AD5F13DCDB451E84BA440F710
        D8C8CA628B0A553CA651F7CA8EE3BD615DC96D773BCD3E85A89790057C5C46A1
        C0E99025C1FC6BF31CCB035E58CF690A5371E5B2B7ABDF63DCC3E2692A567357
        B93C536351D33C40B2BCF63702385609540FB379A42874C7724A86CE78E84720
        D4BCBA5B2F065FDD18629A48AF27689655DCA24FB5304623D9883F855817B0DC
        DC5A5BDE0B7D2EC2D5D2531492A991CA105142AE42A8201CE7B6315CD6BBAB29
        F0EDDE9896D2BC925EBBACA9246C854DC1901E1F77DDF6AE2C365B88A92A54AA
        5376E7575DA37FEAE5D5C6D1846528CD5ECFAF5398BDBABABEBB7BABFB86B8B8
        6509BCA85C28CE000001D493F8D576A94A37F74FE551BA95EA08FAD7EE387A74
        284151A0928AD923F3BA9567564E73776C89A98D4F6A61ADC1119A8CD486A334
        8B430D466A43519A0B430D466A43519A0B430D46D5D5E9FE1A86FBFB20A5D84F
        B4AC6D3A104B22B5C18B72F18FEEF19CF3554787E09A0B46B7D4632D2C73CAE5
        E36015620C49E99E8BD3D4D65ED626EA0CE69A98D5B8BA32A6AD15A4F728C876
        CAED18391098C49BC647F70F4EB91535AF8625BD4416F3471CA20591D663B417
        60CCA8A7D4A0538F7A6EA456E528B3996A8DABA6834084497515E5F46934369F
        6860A18888E5301BE5E7863D3BF7AA7AB680FA6C5134F776E5A690A46A3772A3
        1F3924600C303CF3ED4BDA45BB16A2CC26A8DABA97F08DC0B8110BCB7FDE3A43
        03E1F6CEEDBB010EDC11F29F9BA73F5C5783C2F7133CA1AE6DE348911D9DB760
        06B779FB0CF0A847D7147B48F72D459CDB546D5D245E19927BC8AD62BDB6333C
        3F6860438D9114DE189C7752381EA3DF16AEBC29041A6A16BE88DD0925696456
        2D12448A872005C9277AF7E338238384EAC56868A2CE39AA36AEA66F094B0EDF
        3EFED1322471C39FDDA2862FF77A1046075E7A0A2E7C1B750DA4F726F6CCC489
        BD18B15F37F72931032060ED91473D4F147B58F72D459C9B546D5D058F8766BE
        D312EA1B983CD93CD31DB9DDBDC47B4B738DA386EE7B1ABD7DE0EFECFB5D4A4B
        ED4ADB7DAC25D562CB65C4888548C640F9C60F7FC0D0EA453B5CA499C69AF7AF
        82AD10F05FCFD7ED327F215E0E457BB7C18300F06E253F37DA64F5F415C599FF
        0007E66D4BE23BFDD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCEBE7CE824D
        D07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6A3
        DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE8
        024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8F
        D6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEB
        FCE8024DD07A8FD68DD07A8FD6A2DD6BEBFCE8DF6BEA7F5A0097741EA3F5A374
        1EA3F5A8B7DAFA9FD68DF6BEA7F5A0097741EA3F5A3741EA3F5A8B7DAFA9FD68
        DF6BEA7F5A0097741EA3F5A3741EA3F5A8B7DAFA9FD68DF6BEA7F5A0097741EA
        3F5A3741EA3F5A8B7DAFA9FD68DF6BEA7F5A0097741EA3F5A3741EA3F5A8B7DA
        FA9FD68DF6BEA7F5A0097741EA3F5A3741EA3F5A8B7DAFA9FD68DF6BEA7F5A00
        97741EA3F5A3741EA3F5A8B7DAFA9FD68DF6BEA7F5A0097741EA3F5A3741EA3F
        5A8B7DAFA9FD68DF6BEA7F5A0097741EA3F5A3741EA3F5A8F75AFAFF003A375A
        FAFF003A0093741EA3F5A3741EA3F5A8F75AFAFF003A375AFAFF003A0093741E
        A3F5A3741EA3F5A8F75AFAFF003A375AFAFF003A0093741EA3F5A3741EA3F5A8
        F75AFAFF003A375AFAFF003A0093741EA3F5A3741EA3F5A8F75AFAFF003A375A
        FAFF003A0093741EA3F5A3741EA3F5A8F75AFAFF003A375AFAFF003A0093741E
        A3F5A3741EA3F5A8F75AFAFF003A375AFAFF003A0093741EA3F5A3741EA3F5A8
        B75AFAFF003A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9
        FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD
        07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A
        37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A
        8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DD6
        BEA7F5A5DD6BEBFCE8024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE802
        4DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6
        A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFC
        E8024DD07A8FD68DD07A8FD6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A
        8FD6A3DD6BEBFCE8DD6BEBFCE8024DD07A8FD68DD07A8FD6A3DD6BEBFCE9375A
        FA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD6
        8DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7
        F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD
        07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2
        DF6BEA7F5A37DAFA9FD68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD
        68025DD07A8FD68DD07A8FD6A2DF6BEA7F5A37DAFA9FD680385D4A276D42E0A8
        C8F31B1CFBD456F085994CF1968C1C950C013ED4514F1195D275652BBD5DFA7F
        91F13CCEE5BF2EC307F7773924E3E65F97D075E7B5204D3F7731DD05C7F7D726
        8A2B3FECCA3DDFE1FE4573F9166CA4D3ED2EA19956E0BC72AB86DCBD0763CD7A
        32EB7A3ED1BAFA0CF7C483FC68A2BA70F97D249D9BFC3FC8EBC3E2250BD92393
        D5FC52935DB7D8E4B986240426C74F98E7A9CD41178B264CEF9A561D8111FF00
        3A28AC5E0637BF33FEBE443C4546EF728EAFAC5AEACAAF77062742C15E3603E5
        E700F3F4FD6B3DDB4C0AFB1662DB485DD22E338EBC1F5A28A8797526EEDBFC3F
        C88737277667F1FDF4FF00BEC553D418155018120F639A28AECCB32EA54F1319
        A6EEAFF97A116B141AA334515F565A2334C345148B44669868A282D119A61A28
        A0B44B1DFDDC5B3CAB995362845DAC46007DE00FF8173F5AB16BAFEA36B2B48B
        70CC76C80063F74BA152C31D0E0D1454B8A7BA355268A726A97AC662F7323994
        10E58E49C800F5E9C003E9C509AD6A514B2C90DE4D134B8DFE59DA0E060703D0
        71F4E28A28E55D8B4D8C5D6B528FCBD97B38D89E5AFCFD138F97E9C018F6AAF3
        6A77D216325DCCE58924B392492C18FEAA0FE14514B95762D3639B5DD50973F6
        FB91BD42361C8E0678FF00C79BF33EA6A27D67526B7581AFAE0C4ABB150B9C01
        B0A63FEF962BF438A28A3963D8B4D9126AFA846B02C7793A883223C39F94608C
        0F6C1231EE699FDAFA82CCB32DE4C2452CC086E85800DF98001F61451472AEC5
        A6C5B7D6AF62BF8AEE59E59E48D9DBE791864B8C372083CF7E79A5D53C45A95F
        DC5D48F70F1A5C001E2463B7010201C927EEA819EA7BD1454F246F7B169B2849
        A95EBB3335D4C4B19093BBAEFF00BFF9F7A7CDAD6A72B3B4B7D3B9923313966C
        EE524120FAF201E7D051453E55D8B4CCC22BDEFE15D9258F84205BF578E59646
        942E08214E31FCB3F8D14579B9A49AA697766D4773AFCD8FF79FF5A3363FDE7F
        D68A2BC23A43363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003363FDE7FD68CD8FF0079FF005A28A003363FDE7FD68CD8FF0079
        FF005A28A003758FABFEB46EB1F57FD68A2800CD8FABFEB466C7FBCFFAD14500
        19B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3
        FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7
        FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1
        450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1
        FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB4
        66C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCF
        FAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD14500
        19B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3
        FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7
        FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1
        450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1
        FEF3FEB466C7FBCFFAD1450019B1FEF3FEB466C7FBCFFAD1450019B1FEF3FEB4
        66C7FBCFFAD145001BAC7D5FF5A3758FABFEB451401FFFD9}
      OnClick = Image1Click
    end
    object bAtivar: TBitBtn
      Left = 689
      Top = 4
      Width = 96
      Height = 32
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Ativar'
      Default = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF0808391818A51818A51818A51818A51818A518
        18A51818A51818A51818A51818A51818A51818A5101042FFFFFFFFFFFF1818AD
        0000C60000C60000CE0000CE0000DE1818E71818EF0808EF0008EF1821EF2131
        F73142FF2929BDFFFFFFFFFFFF1010AD0000B50000BD0000C66B6BE7DEDEFFFF
        FFFFFFFFFFE7E7FF848CF71821EF1829EF2939EF2129C6FFFFFFFFFFFF1010AD
        0000B50000BD8C8CE7FFFFFFDEDEF79494EF8C8CEFD6D6F7FFFFFFADB5FF2129
        EF2939EF2129C6FFFFFFFFFFFF1010AD0000B56B6BD6FFFFFFADADEF0808CE00
        00D60000DE0000E79494EFFFFFFF949CF71829EF2129C6FFFFFFFFFFFF1010A5
        1010BDD6D6F7F7F7FF3131CE1818D61818DE1010DE0000E70008E7E7E7FFEFEF
        FF2931EF2129C6FFFFFFFFFFFF1818A53131BDEFEFFFDEDEF72929CE3131D631
        31DE3131DE3939E71010E7B5B5F7FFFFFF3139EF1821BDFFFFFFFFFFFF1818A5
        3939BDE7E7F7EFEFFF4242CE3939D69C9CEF9C9CEF4A4AE74A4AEFDEDEFFEFF7
        FF1821EF1818BDFFFFFFFFFFFF2121A54242BDADADDEFFFFFF9C9CE74A4AD6EF
        EFFFF7F7FF5252E79494EFFFFFFFC6C6F70810E71010BDFFFFFFFFFFFF2929A5
        5A5AC66363C6DEDEEFFFFFFF9494E7E7E7F7E7E7FFA5A5EFFFFFFFEFEFF78484
        E76363EF1010BDFFFFFFFFFFFF2929A56B6BC66363C67373C6ADADD68484D6E7
        E7FFEFEFFFA5A5E7D6D6EF8484DE7B7BE78C8CEF3131BDFFFFFFFFFFFF3131A5
        7B7BC67373C67373CE7373C67B7BCEF7F7FFF7F7FF8484DE8484D68C8CE78C8C
        E79494E74242BDFFFFFFFFFFFF313194ADADDEADADDEADADDEADADE7B5B5E7B5
        B5E7B5B5E7BDBDEFBDBDEFC6C6EFC6C6F7D6D6F78C8CB5FFFFFFFFFFFF181894
        21218C3131943131943131943131943131943131943131943131943131943131
        94292994181884FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ModalResult = 1
      TabOrder = 0
      OnClick = bAtivarClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 317
    object Principal1: TMenuItem
      Caption = 'Principal'
      object Ativcar1: TMenuItem
        Caption = 'Ativar'
      end
      object Desativar1: TMenuItem
        Caption = 'Desativar'
        OnClick = Desativar1Click
      end
      object Testar1: TMenuItem
        Caption = 'Testar'
        OnClick = Testar1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Sobre1: TMenuItem
        Caption = 'Sobre ACBr'
        OnClick = Sobre1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Sair1: TMenuItem
        Caption = 'Sair'
        OnClick = Sair1Click
      end
    end
    object Variaveis1: TMenuItem
      Caption = 'Variaveis'
      object Equipamento1: TMenuItem
        Caption = 'Equipamento'
        object Estado1: TMenuItem
          Caption = 'Estado'
          OnClick = Estado1Click
        end
        object DataHora1: TMenuItem
          Caption = 'Data Hora'
          OnClick = DataHora1Click
        end
        object N42: TMenuItem
          Caption = '-'
        end
        object mModeloStr: TMenuItem
          Caption = 'ModeloStr'
          OnClick = mModeloStrClick
        end
        object NumECF1: TMenuItem
          Caption = 'Num ECF'
          OnClick = NumECF1Click
        end
        object NumLoja1: TMenuItem
          Caption = 'Num Loja'
          OnClick = NumLoja1Click
        end
        object UsuarioAual1: TMenuItem
          Caption = 'Usuario Aual'
          OnClick = UsuarioAual1Click
        end
        object NumSerieMFD: TMenuItem
          Caption = 'Num S'#233'rie MFD'
          OnClick = NumSerieMFDClick
        end
        object NSrie1: TMenuItem
          Caption = 'Num S'#233'rie'
          OnClick = NSrie1Click
        end
        object NVerso1: TMenuItem
          Caption = 'Num Vers'#227'o'
          OnClick = NVerso1Click
        end
        object CNPJIE1: TMenuItem
          Caption = 'CNPJ'
          OnClick = CNPJIE1Click
        end
        object IE1: TMenuItem
          Caption = 'IE'
          OnClick = IE1Click
        end
        object Modelo1: TMenuItem
          Caption = 'Modelo'
          OnClick = Modelo1Click
        end
        object SubModelo1: TMenuItem
          Caption = 'SubModelo'
          OnClick = SubModelo1Click
        end
        object NumerodeReduesZrestantes1: TMenuItem
          Caption = 'Numero de Redu'#231#245'es Z restantes'
          OnClick = NumerodeReduesZrestantes1Click
        end
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object Flags1: TMenuItem
        Caption = 'Flags'
        object PoucoPapel1: TMenuItem
          Caption = 'Pouco Papel ?'
          OnClick = PoucoPapel1Click
        end
        object HorarioVerao2: TMenuItem
          Caption = 'Horario Verao ?'
          OnClick = HorarioVerao2Click
        end
        object Arredonda1: TMenuItem
          Caption = 'Arredonda ?'
          OnClick = Arredonda1Click
        end
        object MFD1: TMenuItem
          Caption = #201' MFD ?'
          OnClick = MFD1Click
        end
        object Termica1: TMenuItem
          Caption = #201' Termica ?'
          OnClick = Termica1Click
        end
        object ParametroDescontoISSQN1: TMenuItem
          Caption = 'ParametroDescontoISSQN ?'
          OnClick = ParametroDescontoISSQN1Click
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MapaResumo1: TMenuItem
        Caption = 'Mapa Resumo'
        object DataMovimento1: TMenuItem
          Caption = 'Data Movimento'
          OnClick = DataMovimento1Click
        end
        object DataHoraltimaReduoZ1: TMenuItem
          Caption = 'Data/Hora '#218'ltima Redu'#231#227'o Z'
          OnClick = DataHoraltimaReduoZ1Click
        end
        object DadosReducaoZ1: TMenuItem
          Caption = 'Dados Reducao Z (60M)'
          OnClick = DadosReducaoZ1Click
        end
        object DadosUltimaReduoZ1: TMenuItem
          Caption = 'Dados '#218'ltima Redu'#231#227'o Z'
          OnClick = DadosUltimaReduoZ1Click
        end
        object N20: TMenuItem
          Caption = '-'
        end
        object NumCOO1: TMenuItem
          Caption = 'NumCOO'
          OnClick = NumCOO1Click
        end
        object NumCRZ1: TMenuItem
          Caption = 'Num CRZ'
          OnClick = NumCRZ1Click
        end
        object NumCRO1: TMenuItem
          Caption = 'Num CRO'
          OnClick = NumCRO1Click
        end
        object NumCCF1: TMenuItem
          Caption = 'Num CCF'
          OnClick = NumCCF1Click
        end
        object NumCOOInicial1: TMenuItem
          Caption = 'Num COO Inicial'
          OnClick = NumCOOInicial1Click
        end
        object NumGNF1: TMenuItem
          Caption = 'Num GNF'
          OnClick = NumGNF1Click
        end
        object N26: TMenuItem
          Caption = '-'
        end
        object VendaBruta1: TMenuItem
          Caption = 'Venda Bruta'
          OnClick = VendaBruta1Click
        end
        object GrandeTotal1: TMenuItem
          Caption = 'Grande Total'
          OnClick = GrandeTotal1Click
        end
        object N22: TMenuItem
          Caption = '-'
        end
        object TotalCancelamentos1: TMenuItem
          Caption = 'Total Cancelamentos'
          OnClick = TotalCancelamentos1Click
        end
        object TotalDescontos1: TMenuItem
          Caption = 'Total Descontos'
          OnClick = TotalDescontos1Click
        end
        object TotalAcrescimos1: TMenuItem
          Caption = 'Total Acrescimos'
          OnClick = TotalAcrescimos1Click
        end
        object TotalNoFiscal1: TMenuItem
          Caption = 'Total N'#227'o Fiscal'
          OnClick = TotalNoFiscal1Click
        end
        object N21: TMenuItem
          Caption = '-'
        end
        object TotalSubstituicaoTributaria1: TMenuItem
          Caption = 'Total Substituicao Tributaria'
          OnClick = TotalSubstituicaoTributaria1Click
        end
        object TotalNaoTributado1: TMenuItem
          Caption = 'Total Nao Tributado'
          OnClick = TotalNaoTributado1Click
        end
        object TotalIsencao1: TMenuItem
          Caption = 'Total Isencao'
          OnClick = TotalIsencao1Click
        end
        object N40: TMenuItem
          Caption = '-'
        end
        object otalSubstituicaoTributariaISSQN1: TMenuItem
          Caption = 'Total Substituicao Tributaria ISSQN'
          OnClick = otalSubstituicaoTributariaISSQN1Click
        end
        object otalNaoTributadoISSQN1: TMenuItem
          Caption = 'Total Nao Tributado ISSQN'
          OnClick = otalNaoTributadoISSQN1Click
        end
        object otalIsencao1: TMenuItem
          Caption = 'Total Isencao ISSQN'
          OnClick = otalIsencao1Click
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Aliquotas1: TMenuItem
        Caption = 'Aliquotas'
        object AliquotasICMS1: TMenuItem
          Caption = 'Carrega Aliquotas (ICMS)'
          OnClick = AliquotasICMS1Click
        end
        object LerTotaisAliquotas1: TMenuItem
          Caption = 'Ler Totais Aliquotas'
          OnClick = LerTotaisAliquotas1Click
        end
        object N34: TMenuItem
          Caption = '-'
        end
        object AchaAliquotaporIndice1: TMenuItem
          Caption = 'Acha Aliquota por Indice'
          OnClick = AchaAliquotaporIndice1Click
        end
        object AchaAliquotaporValor1: TMenuItem
          Caption = 'Acha Aliquota por Valor'
          OnClick = AchaAliquotaporValor1Click
        end
      end
      object FormasdePagamento2: TMenuItem
        Caption = 'Formas de Pagamento'
        object FormasdePagamento1: TMenuItem
          Caption = 'Carrega Formas Pagamento'
          OnClick = FormasdePagamento1Click
        end
        object LerTotaisFormadePagamento1: TMenuItem
          Caption = 'Ler Totais Forma de Pagamento'
          OnClick = LerTotaisFormadePagamento1Click
        end
        object N35: TMenuItem
          Caption = '-'
        end
        object AcharMeioPagamentoporIndice1: TMenuItem
          Caption = 'Achar Formas Pagamento por Indice'
          OnClick = AcharMeioPagamentoporIndice1Click
        end
        object AcharMeiodePagametoporDescrio1: TMenuItem
          Caption = 'Achar Formas Pagameto por Descri'#231#227'o'
          OnClick = AcharMeiodePagametoporDescrio1Click
        end
        object N41: TMenuItem
          Caption = '-'
        end
        object LerTroco1: TMenuItem
          Caption = 'Ler Troco'
          OnClick = LerTroco1Click
        end
      end
      object ComprovantesNaoFiscais1: TMenuItem
        Caption = 'Comprovantes Nao Fiscais'
        object CarregaComprovantesNAOFiscais1: TMenuItem
          Caption = 'Carrega Comprovantes Nao Fiscais'
          OnClick = CarregaComprovantesNAOFiscais1Click
        end
        object LerTotaisComprovanetNaoFiscal1: TMenuItem
          Caption = 'Ler Totais Comprovante Nao Fiscal'
          OnClick = LerTotaisComprovanetNaoFiscal1Click
        end
        object N36: TMenuItem
          Caption = '-'
        end
        object AchaCNFporIndice1: TMenuItem
          Caption = 'Acha CNF por Indice'
          OnClick = AchaCNFporIndice1Click
        end
        object AchaCNFporDescrio1: TMenuItem
          Caption = 'Acha CNF por Descri'#231#227'o'
          OnClick = AchaCNFporDescrio1Click
        end
      end
      object RelatriosGerenciais1: TMenuItem
        Caption = 'Relat'#243'rios Gerenciais'
        object CarregaRelatriosGerenciais1: TMenuItem
          Caption = 'Carrega Relat'#243'rios Gerenciais'
          OnClick = CarregaRelatriosGerenciais1Click
        end
        object N37: TMenuItem
          Caption = '-'
        end
        object AchaRGporIndice1: TMenuItem
          Caption = 'Acha RG por Indice '
          OnClick = AchaRGporIndice1Click
        end
        object AchaRGporDescrio1: TMenuItem
          Caption = 'Acha RG por Descri'#231#227'o'
          OnClick = AchaRGporDescrio1Click
        end
      end
      object CarregaUnidadesdeMedida1: TMenuItem
        Caption = 'Carrega Unidades de Medida'
        OnClick = CarregaUnidadesdeMedida1Click
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object LerTodasasVariveis1: TMenuItem
        Caption = 'Ler Todas as Variveis'
        OnClick = LerTodasasVariveis1Click
      end
    end
    object Relatrios1: TMenuItem
      Caption = 'Relat'#243'rios'
      object LeituraX1: TMenuItem
        Caption = 'Leitura X'
        OnClick = LeituraX1Click
      end
      object ReduoZ1: TMenuItem
        Caption = 'Redu'#231#227'o Z'
        OnClick = ReduoZ1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object CupomVinculado1: TMenuItem
        Caption = 'Cupom Vinculado'
        object CupomVinculadoCompleto2: TMenuItem
          Caption = 'Cupom Vinculado Completo'
          OnClick = ListaCupomVinculado1Click
        end
        object N27: TMenuItem
          Caption = '-'
        end
        object AbreCupomVinculado1: TMenuItem
          Caption = 'Abre Cupom Vinculado'
          OnClick = AbreCupomVinculado1Click
        end
        object ImprimeLinhaCupomVinculado2: TMenuItem
          Caption = 'Imprime Linha Cupom Vinculado'
          OnClick = ImprimeLinhaVinculado1Click
        end
        object N28: TMenuItem
          Caption = '-'
        end
        object estedeVinculado1: TMenuItem
          Caption = 'Teste de Vinculado'
          OnClick = TestedeVinculado1Click
        end
      end
      object RelatorioGerencial1: TMenuItem
        Caption = 'Relatorio Gerencial'
        object ListaRelatorioGerencial1: TMenuItem
          Caption = 'Relatorio Gerencial Completo'
          OnClick = ListaRelatorioGerencial1Click
        end
        object RelatorioGerencialcomformatacao1: TMenuItem
          Caption = 'Relatorio Gerencial com formatacao'
          OnClick = RelatorioGerencialcomformatacao1Click
        end
        object N17: TMenuItem
          Caption = '-'
        end
        object AbreRelatorioGerencial1: TMenuItem
          Caption = 'Abre Relatorio Gerencial'
          OnClick = AbreRelatorioGerencial1Click
        end
        object ImprimeLinhaRelatorio1: TMenuItem
          Caption = 'Imprime Linha Relatorio'
          OnClick = ImprimeLinhaRelatorio1Click
        end
      end
      object PularLinhas1: TMenuItem
        Caption = 'Pular Linhas'
        OnClick = PularLinhas1Click
      end
      object CortaPapel1: TMenuItem
        Caption = 'Corta Papel'
        OnClick = CortaPapel1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object FechaRelatrio1: TMenuItem
        Caption = 'Fecha Relat'#243'rio / Vinculado'
        OnClick = FechaRelatrio1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object LeituradeMemoriaFiscal1: TMenuItem
        Caption = 'Leitura de Memoria Fiscal'
        object CapturaporNReduaoZ1: TMenuItem
          Caption = 'Captura por N.Redu'#231'aoZ'
          OnClick = CapturaporNReduaoZ1Click
        end
        object CapturaporPeriodo1: TMenuItem
          Caption = 'Captura por Periodo'
          OnClick = CapturaporPeriodo1Click
        end
        object ImprimeporNReduaoZ1: TMenuItem
          Caption = 'Imprime por N.Redu'#231'aoZ'
          OnClick = ImprimeporNReduaoZ1Click
        end
        object ImprimeporPeriodo1: TMenuItem
          Caption = 'Imprime por Periodo'
          OnClick = ImprimeporPeriodo1Click
        end
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object LeituraMFD1: TMenuItem
        Caption = 'Leitura MFD Serial'
        object PorCOO1: TMenuItem
          Caption = 'Por Intervalo de COO'
          OnClick = PorCOO1Click
        end
        object PorPeriodo1: TMenuItem
          Caption = 'Por Intervalo de Periodo'
          OnClick = PorPeriodo1Click
        end
        object N38: TMenuItem
          Caption = '-'
        end
        object PorCOO2: TMenuItem
          Caption = 'Por COO'
          OnClick = PorCOO2Click
        end
        object PorDatadeMovimento1: TMenuItem
          Caption = 'Por Data de Movimento'
          OnClick = PorDatadeMovimento1Click
        end
      end
      object N39: TMenuItem
        Caption = '-'
      end
      object LeituraSerialMFD1: TMenuItem
        Caption = 'Espelho MFD DLL'
        object PorCOO3: TMenuItem
          Caption = 'Por COO'
          OnClick = PorCOO3Click
        end
        object PorPeriodo2: TMenuItem
          Caption = 'Por Periodo'
          OnClick = PorPeriodo2Click
        end
      end
      object ArquivoMFDDLL1: TMenuItem
        Caption = 'Arquivo MFD DLL'
        object PorCOO4: TMenuItem
          Caption = 'Por COO'
          OnClick = PorCOO4Click
        end
        object PorPeriodo3: TMenuItem
          Caption = 'Por Periodo'
          OnClick = PorPeriodo3Click
        end
      end
      object N43: TMenuItem
        Caption = '-'
      end
      object DAV1: TMenuItem
        Caption = 'DAV'
        OnClick = DAV1Click
      end
      object DAVOS1: TMenuItem
        Caption = 'DAV-OS'
        OnClick = DAVOS1Click
      end
    end
    object Cupom1: TMenuItem
      Caption = 'Cupom Fiscal'
      object TestaPodeAbrirCupom1: TMenuItem
        Caption = 'Testa Pode Abrir Cupom'
        OnClick = TestaPodeAbrirCupom1Click
      end
      object IdentificaConsumidor1: TMenuItem
        Caption = 'Identifica Consumidor'
        OnClick = IdentificaConsumidor1Click
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object AbrirCupom1: TMenuItem
        Caption = 'Abre Cupom'
        OnClick = AbrirCupom1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object VenderItem1: TMenuItem
        Caption = 'Vende Item'
        OnClick = VenderItem1Click
      end
      object LegendaInmetroproximoItem1: TMenuItem
        Caption = 'Legenda Inmetro proximo item '
        OnClick = LegendaInmetroproximoItem1Click
      end
      object N30: TMenuItem
        Caption = '-'
      end
      object CancelarItemVendido1: TMenuItem
        Caption = 'Cancela Item Vendido'
        OnClick = CancelarItemVendido1Click
      end
      object CancelaItemVendidoParcial1: TMenuItem
        Caption = 'Cancela Item Vendido Parcial'
        OnClick = CancelaItemVendidoParcial1Click
      end
      object CancelaDescontoAcrescimoItem1: TMenuItem
        Caption = 'Cancela DescontoAcrescimo Item'
        OnClick = CancelaDescontoAcrescimoItem1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Sub1: TMenuItem
        Caption = 'SubTotaliza Cupom'
        OnClick = Sub1Click
      end
      object CancelaDescontoAcrescimoSubTotal1: TMenuItem
        Caption = 'Cancela DescontoAcrescimo SubTotal'
        OnClick = CancelaDescontoAcrescimoSubTotal1Click
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object EfetuarPagamento1: TMenuItem
        Caption = 'Efetua Pagamento'
        OnClick = EfetuarPagamento1Click
      end
      object EstornaMeiodePagamento1: TMenuItem
        Caption = 'Estorna Meio de Pagamento'
        OnClick = EstornaMeiodePagamento1Click
      end
      object N33: TMenuItem
        Caption = '-'
      end
      object FecharCupom1: TMenuItem
        Caption = 'Fecha Cupom'
        OnClick = FecharCupom1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object CancelaCupom1: TMenuItem
        Caption = 'Cancela Cupom'
        OnClick = CancelaCupom1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Variveis1: TMenuItem
        Caption = 'Vari'#225'veis'
        object NUltimoCupom1: TMenuItem
          Caption = 'Num Cupom'
          OnClick = NUltimoCupom1Click
        end
        object SubTotal1: TMenuItem
          Caption = 'SubTotal'
          OnClick = SubTotal1Click
        end
        object TotalPago1: TMenuItem
          Caption = 'Total Pago'
          OnClick = TotalPago1Click
        end
        object UltimoItemVendido1: TMenuItem
          Caption = 'Num.Ultimo Item'
          OnClick = UltimoItemVendido1Click
        end
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object TestedeVelocidade1: TMenuItem
        Caption = 'Teste de Velocidade'
        OnClick = TestedeVelocidade1Click
      end
      object VendaFrentica1: TMenuItem
        Caption = 'Venda Fren'#233'tica'
        OnClick = VendaFrentica1Click
      end
    end
    object NoFiscal1: TMenuItem
      Caption = 'N'#227'o Fiscal'
      object NoFiscalCompleto1: TMenuItem
        Caption = 'N'#227'o Fiscal Completo'
        OnClick = NoFiscalCompleto1Click
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object Sangria1: TMenuItem
        Caption = 'Sangria'
        OnClick = Sangria1Click
      end
      object Suprimento1: TMenuItem
        Caption = 'Suprimento'
        OnClick = Suprimento1Click
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object AbreNoFiscal1: TMenuItem
        Caption = 'Abre N'#227'o Fiscal'
        OnClick = AbreNoFiscal1Click
      end
      object RegistraItemNaoFiscal1: TMenuItem
        Caption = 'Registra Item N'#227'o Fiscal'
        OnClick = RegistraItemNaoFiscal1Click
      end
      object SubTotalizaNaoFiscal1: TMenuItem
        Caption = 'SubTotaliza N'#227'o Fiscal'
        OnClick = SubTotalizaNaoFiscal1Click
      end
      object EfetuaPagamentoNaoFiscal1: TMenuItem
        Caption = 'Efetua Pagamento N'#227'o Fiscal'
        OnClick = EfetuaPagamentoNaoFiscal1Click
      end
      object FechaNoFiscal1: TMenuItem
        Caption = 'Fecha N'#227'o Fiscal'
        OnClick = FechaNoFiscal1Click
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object CancelaNoFiscal1: TMenuItem
        Caption = 'Cancela N'#227'o Fiscal'
        OnClick = CancelaNoFiscal1Click
      end
    end
    object Dispositivos1: TMenuItem
      Caption = 'Dispositivos'
      object Gaveta1: TMenuItem
        Caption = '&Gaveta'
        object AbreGaveta1: TMenuItem
          Caption = 'Abre Gaveta'
          OnClick = AbreGaveta1Click
        end
        object GavetaAberta1: TMenuItem
          Caption = 'Gaveta Aberta ?'
          OnClick = GavetaAberta1Click
        end
      end
      object Cheque1: TMenuItem
        Caption = '&Cheque'
        object ChequePronto1: TMenuItem
          Caption = 'Cheque Pronto ?'
          OnClick = ChequePronto1Click
        end
        object ImprimeCheque1: TMenuItem
          Caption = 'Imprime Cheque'
          OnClick = ImprimeCheque1Click
        end
        object CancelaImpressoCheque1: TMenuItem
          Caption = 'Cancela Impress'#227'o Cheque'
          OnClick = CancelaImpressoCheque1Click
        end
      end
    end
    object Utilitrios1: TMenuItem
      Caption = 'Utilit'#225'rios'
      object ProgramaAliquota1: TMenuItem
        Caption = 'Programa Aliquota'
        OnClick = ProgramaAliquota1Click
      end
      object ProgramaFormadePagamento1: TMenuItem
        Caption = 'Programa Forma de Pagamento'
        OnClick = ProgramaFormadePagamento1Click
      end
      object ProgramaComprovanteNAOFiscal1: TMenuItem
        Caption = 'Programa Comprovante NAO Fiscal'
        OnClick = ProgramaComprovanteNAOFiscal1Click
      end
      object ProgramaRelatrioGerencial1: TMenuItem
        Caption = 'Programa Relat'#243'rio Gerencial'
        OnClick = ProgramaRelatrioGerencial1Click
      end
      object ProgramaUnidadeMedida1: TMenuItem
        Caption = 'Programa Unidade Medida'
        OnClick = ProgramaUnidadeMedida1Click
      end
      object ProgramaIdentificaoPafECF1: TMenuItem
        Caption = 'Programa Identifica'#231#227'o Paf-ECF'
        OnClick = ProgramaIdentificaoPafECF1Click
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object ConsultaRegistradorECF1: TMenuItem
        Caption = 'Consulta Registrador ECF'
        OnClick = ConsultaRegistradorECF1Click
      end
      object DeCodificaTexto1: TMenuItem
        Caption = 'De/Codifica Texto'
        OnClick = DeCodificaTexto1Click
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object HorarioVerao1: TMenuItem
        Caption = 'Muda Hor'#225'rio Ver'#227'o'
        OnClick = HorarioVerao1Click
      end
      object MudaArredondamento1: TMenuItem
        Caption = 'Muda Arredondamento'
        OnClick = MudaArredondamento1Click
      end
      object ImpactoAgulhas1: TMenuItem
        Caption = 'Impacto Agulhas'
        OnClick = ImpactoAgulhas1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object CorrigeEstadodeErro1: TMenuItem
        Caption = 'Corrige Estado de Erro'
        OnClick = CorrigeEstadodeErro1Click
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object EnviaComando1: TMenuItem
        Caption = 'Envia Comando'
        OnClick = EnviaComando1Click
      end
    end
  end
  object ACBrECF1: TACBrECF
    QuebraLinhaRodape = False
    Porta = 'procurar'
    ReTentar = False
    DescricaoGrande = True
    MsgAguarde = 'Aguardando a resposta da Impressora: %d segundos'
    MsgTrabalhando = 'Impressora est'#225' trabalhando'
    MsgRelatorio = 'Imprimindo %s  %d'#170' Via '
    MsgPausaRelatorio = 'Destaque a %d'#170' via, <ENTER> proxima, %d seg.'
    PaginaDeCodigo = 850
    OnMsgAguarde = ACBrECF1MsgAguarde
    OnAguardandoRespostaChange = ACBrECF1AguardandoRespostaChange
    OnMsgPoucoPapel = ACBrECF1MsgPoucoPapel
    OnChangeEstado = ACBrECF1ChangeEstado
    DecimaisPreco = 2
    MemoBobina = mBobina
    MemoParams.Strings = (
      '[Cabecalho]'
      'LIN000=<center><b>Nome da Empresa</b></center>'
      'LIN001=<center>Nome da Rua , 1234  -  Bairro</center>'
      'LIN002=<center>Cidade  -  UF  -  99999-999</center>'
      
        'LIN003=<center>CNPJ: 01.234.567/0001-22    IE: 012.345.678.90</c' +
        'enter>'
      
        'LIN004=<table width=100%><tr><td align=left><code>Data</code> <c' +
        'ode>Hora</code></td><td align=right>COO: <b><code>NumCupom</code' +
        '></b></td></tr></table>'
      'LIN005=<hr>'
      ''
      '[Cabecalho_Item]'
      'LIN000=ITEM   CODIGO     DESCRICAO'
      'LIN001=QTD         x UNITARIO       Aliq     VALOR (R$)'
      'LIN002=<hr>'
      
        'MascaraItem=III CCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDQQQQ' +
        'QQQQ UU x VVVVVVVVVVVVV AAAAAA TTTTTTTTTTTTT'
      ''
      '[Rodape]'
      'LIN000=<hr>'
      
        'LIN001=<table width=100%><tr><td align=left><code>Data</code> <c' +
        'ode>Hora</code></td><td align=right>Projeto ACBr: <b><code>ACBR<' +
        '/code></b></td></tr></table>'
      'LIN002=<center>Obrigado Volte Sempre</center>'
      'LIN003=<hr>'
      ''
      '[Formato]'
      'Colunas=48'
      'HTML=1'
      'HTML_Title_Size=4'
      'HTML_Font=<font size="5" face="Lucida Console">')
    OnBobinaAdicionaLinhas = ACBrECF1BobinaAdicionaLinhas
    ECFVirtual = ACBrECFVirtualNaoFiscal1
    ArqLOG = 'acbrlog.txt'
    ConfigBarras.MostrarCodigo = True
    ConfigBarras.LarguraLinha = 3
    ConfigBarras.Altura = 10
    ConfigBarras.Margem = 0
    InfoRodapeCupom.Imposto.ModoCompacto = False
    Left = 128
    Top = 317
  end
  object ACBrRFD1: TACBrRFD
    Left = 180
    Top = 317
  end
  object dlgDialogoSalvar: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Arquivos texto|*.txt'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Salvar arquivo texto'
    Left = 255
    Top = 315
  end
  object ACBrECFVirtualNaoFiscal1: TACBrECFVirtualNaoFiscal
    ECF = ACBrECF1
    QuandoGravarArqINI = ACBrECFVirtualNaoFiscal1GravaArqINI
    QuandoLerArqINI = ACBrECFVirtualNaoFiscal1LeArqINI
    ExibeAvisoLegal = True
    Colunas = 48
    NumECF = 1
    NumCRO = 1
    CNPJ = '01.234.567/0001-22'
    IE = '012.345.678.90'
    IM = '1234-0'
    Cabecalho.Strings = (
      'Nome da Empresa'
      'Nome da Rua , 1234  -  Bairro'
      'Cidade  -  UF  -  99999-999')
    CabecalhoItem.Strings = (
      'ITEM   CODIGO             DESCRICAO'
      '.             QTDxUNITARIO   Aliq    VALOR (R$)'
      '------------------------------------------------')
    MascaraItem = 
      'III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD QQQQQQQQ U' +
      'UxVVVVVVVVV AAAAAAA TTTTTTTTTTT'
    Left = 412
    Top = 88
  end
end
