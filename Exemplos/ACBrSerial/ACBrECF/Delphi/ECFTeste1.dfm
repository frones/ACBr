object Form1: TForm1
  Left = 401
  Top = 220
  Width = 810
  Height = 493
  VertScrollBar.Range = 59
  ActiveControl = pgPrincipal
  AutoScroll = False
  Caption = 'Teste de Impressora Fiscal'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 440
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
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
    Top = 415
    Width = 794
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
  object pgPrincipal: TPageControl
    Left = 0
    Top = 0
    Width = 794
    Height = 375
    ActivePage = tsECF
    Align = alClient
    TabOrder = 0
    object tsECF: TTabSheet
      Caption = 'ECF'
      ImageIndex = 3
      DesignSize = (
        786
        347)
      object SbArqLog: TSpeedButton
        Left = 170
        Top = 170
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = SbArqLogClick
      end
      object Label1: TLabel
        Left = 20
        Top = 8
        Width = 34
        Height = 13
        Caption = 'Modelo'
      end
      object Label4: TLabel
        Left = 148
        Top = 8
        Width = 26
        Height = 13
        Caption = 'Porta'
      end
      object Label5: TLabel
        Left = 266
        Top = 8
        Width = 40
        Height = 13
        Caption = 'TimeOut'
      end
      object Label7: TLabel
        Left = 331
        Top = 8
        Width = 44
        Height = 13
        Caption = 'Intervalo'
      end
      object Label6: TLabel
        Left = 11
        Top = 284
        Width = 107
        Height = 13
        Caption = 'Mensagem Aguarde...'
      end
      object Label9: TLabel
        Left = 11
        Top = 179
        Width = 42
        Height = 13
        Alignment = taRightJustify
        Caption = 'Arq.Log:'
      end
      object Label20: TLabel
        Left = 195
        Top = 179
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Operador:'
      end
      object Label21: TLabel
        Left = 406
        Top = 257
        Width = 126
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Mensagem Trabalhando...'
      end
      object Label22: TLabel
        Left = 31
        Top = 205
        Width = 67
        Height = 13
        Caption = 'Linhas Buffer:'
      end
      object Label46: TLabel
        Left = 11
        Top = 230
        Width = 87
        Height = 13
        Caption = 'P'#225'gina de C'#243'digo:'
        Color = clBtnFace
        ParentColor = False
      end
      object Label69: TLabel
        Left = 400
        Top = 128
        Width = 52
        Height = 13
        Caption = 'ECF Virtual'
        Color = clBtnFace
        ParentColor = False
      end
      object Label32: TLabel
        Left = 474
        Top = 8
        Width = 52
        Height = 13
        Caption = 'BandWidth'
        Color = clBtnFace
        ParentColor = False
      end
      object cbxModelo: TComboBox
        Left = 20
        Top = 25
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
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
        ItemHeight = 13
        TabOrder = 1
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
        Top = 107
        Width = 145
        Height = 17
        Caption = 'Bloqueia Mouse Teclado'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = chBloqueiaClick
      end
      object chExibeMsg: TCheckBox
        Left = 11
        Top = 131
        Width = 131
        Height = 17
        Caption = 'Exibe Msg Aguarde...'
        Checked = True
        State = cbChecked
        TabOrder = 12
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
        Top = 88
        Width = 187
        Height = 17
        Caption = 'Gaveta Sinal Invertido'
        TabOrder = 9
        OnClick = chGavetaSinalInvertidoClick
      end
      object mMsg: TMemo
        Left = 11
        Top = 303
        Width = 568
        Height = 41
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 26
        OnChange = mMsgChange
      end
      object edLog: TEdit
        Left = 59
        Top = 171
        Width = 105
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
        TabOrder = 2
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
        TabOrder = 3
        Value = 100
        OnChange = seIntervaloAposComandoChange
      end
      object btSerial: TBitBtn
        Left = 391
        Top = 21
        Width = 70
        Height = 28
        Cancel = True
        Caption = 'Serial'
        Default = True
        ModalResult = 1
        TabOrder = 4
        OnClick = btSerialClick
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
      end
      object chDescricaoGrande: TCheckBox
        Left = 195
        Top = 131
        Width = 187
        Height = 17
        Caption = 'Descri'#231#227'o Grande'
        Checked = True
        State = cbChecked
        TabOrder = 13
        OnClick = chDescricaoGrandeClick
      end
      object edOperador: TEdit
        Left = 251
        Top = 171
        Width = 101
        Height = 21
        Cursor = crIBeam
        TabOrder = 16
        OnChange = edOperadorChange
      end
      object edMsgTrabalhando: TEdit
        Left = 404
        Top = 276
        Width = 175
        Height = 21
        Cursor = crIBeam
        Anchors = [akTop, akRight]
        TabOrder = 25
        Text = 'Impressora est'#225' trabalhando'
        OnClick = edMsgTrabalhandoChange
      end
      object speLinBuf: TSpinEdit
        Left = 104
        Top = 202
        Width = 87
        Height = 22
        AutoSize = False
        MaxValue = 1000
        MinValue = 0
        TabOrder = 19
        Value = 0
        OnChange = speLinBufChange
      end
      object chArredondamentoItemMFD: TCheckBox
        Left = 195
        Top = 109
        Width = 147
        Height = 19
        Caption = 'Arredondamento Item MFD'
        TabOrder = 11
        OnClick = chArredondamentoItemMFDClick
      end
      object chbCupomMania: TCheckBox
        Left = 234
        Top = 229
        Width = 187
        Height = 17
        Caption = 'Cupom Mania'
        TabOrder = 22
        OnClick = chbCupomManiaClick
      end
      object sePaginaCodigo: TSpinEdit
        Left = 104
        Top = 227
        Width = 87
        Height = 22
        MaxValue = 9999999
        MinValue = 0
        TabOrder = 24
        Value = 0
        OnChange = sePaginaCodigoChange
      end
      object chControlePorta: TCheckBox
        Left = 11
        Top = 88
        Width = 121
        Height = 17
        Caption = 'Controle Porta'
        TabOrder = 8
        OnClick = chControlePortaClick
      end
      object cbxECFVirtual: TComboBox
        Left = 400
        Top = 144
        Width = 105
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 14
        OnChange = cbxECFVirtualChange
        Items.Strings = (
          'N'#227'o Fiscal'
          'SAT'
          'NFCe')
      end
      object seBandWidth: TSpinEdit
        Left = 474
        Top = 24
        Width = 54
        Height = 22
        Increment = 10
        MaxValue = 1000
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnChange = seBandWidthChange
      end
      object Button3: TButton
        Left = 545
        Top = 170
        Width = 40
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Info'
        TabOrder = 18
        OnClick = Button3Click
      end
      object edInfo: TEdit
        Left = 463
        Top = 171
        Width = 80
        Height = 21
        Cursor = crIBeam
        Anchors = [akTop, akRight]
        TabOrder = 17
      end
      object chAACUsar: TCheckBox
        Left = 234
        Top = 206
        Width = 187
        Height = 17
        Caption = 'Usar Arq.Auxiliar Criptografado'
        TabOrder = 20
        OnClick = chAACUsarClick
      end
      object chAACFlush: TCheckBox
        Left = 427
        Top = 206
        Width = 187
        Height = 17
        Caption = 'Flush'
        TabOrder = 21
        OnClick = chAACFlushClick
      end
      object chProcessMessages: TCheckBox
        Left = 427
        Top = 229
        Width = 187
        Height = 17
        Caption = 'ProcessMessages'
        TabOrder = 23
        OnClick = chProcessMessagesClick
      end
    end
    object tsCMD: TTabSheet
      Caption = 'Cmd/Resp'
      object Label2: TLabel
        Left = 0
        Top = 54
        Width = 786
        Height = 17
        Align = alTop
        AutoSize = False
        Caption = 'Resposta'
        Layout = tlBottom
      end
      object Label17: TLabel
        Left = 0
        Top = 0
        Width = 786
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = 'Comando Enviado'
        Layout = tlBottom
      end
      object mResp: TMemo
        Left = 0
        Top = 71
        Width = 786
        Height = 276
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 13
        Font.Name = 'Courier New'
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
        Width = 786
        Height = 39
        Align = alTop
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WantReturns = False
      end
    end
    object tsCupom: TTabSheet
      Caption = 'Cupom'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 41
        Width = 786
        Height = 306
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object mBobina: TMemo
          Left = 0
          Top = 0
          Width = 786
          Height = 306
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Fixedsys'
          Font.Pitch = fpVariable
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
          Visible = False
          WordWrap = False
        end
        object wbBobina: TWebBrowser
          Left = 0
          Top = 0
          Width = 786
          Height = 306
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C0000003C510000A01F00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object cbMemoHTML: TCheckBox
          Left = 272
          Top = 9
          Width = 73
          Height = 21
          Caption = 'HTML'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbMemoHTMLClick
        end
        object bBobinaParams: TButton
          Left = 88
          Top = 8
          Width = 75
          Height = 24
          Caption = 'Parametros'
          TabOrder = 1
          OnClick = bBobinaParamsClick
        end
        object bBobinaLimpar: TButton
          Left = 176
          Top = 8
          Width = 75
          Height = 24
          Caption = 'Limpar'
          TabOrder = 2
          OnClick = bBobinaLimparClick
        end
      end
    end
    object tsRFD: TTabSheet
      Caption = 'RFD'
      ImageIndex = 2
      object pgRFD: TPageControl
        Left = 0
        Top = 55
        Width = 786
        Height = 292
        ActivePage = TabSheet6
        Align = alClient
        TabOrder = 1
        object TabSheet6: TTabSheet
          Caption = 'Sw.House'
          ImageIndex = 1
          object Label8: TLabel
            Left = 12
            Top = 10
            Width = 60
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
            Width = 25
            Height = 13
            Caption = 'CNPJ'
          end
          object Label11: TLabel
            Left = 143
            Top = 51
            Width = 65
            Height = 13
            Caption = 'Insc.Estadual'
          end
          object Label12: TLabel
            Left = 276
            Top = 51
            Width = 67
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
            Width = 34
            Height = 13
            Caption = 'Linha 1'
          end
          object Label19: TLabel
            Left = 204
            Top = 132
            Width = 34
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
            TabOrder = 2
            OnChange = edSH_CNPJChange
          end
          object edSH_IE: TEdit
            Left = 144
            Top = 66
            Width = 114
            Height = 21
            Cursor = crIBeam
            TabOrder = 3
            OnChange = edSH_IEChange
          end
          object edSH_IM: TEdit
            Left = 276
            Top = 66
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 4
            OnChange = edSH_IMChange
          end
          object edSH_Aplicativo: TEdit
            Left = 12
            Top = 106
            Width = 169
            Height = 21
            Cursor = crIBeam
            TabOrder = 5
            OnChange = edSH_AplicativoChange
          end
          object edSH_NumeroAP: TEdit
            Left = 192
            Top = 106
            Width = 65
            Height = 21
            Cursor = crIBeam
            TabOrder = 6
            OnChange = edSH_NumeroAPChange
          end
          object edSH_VersaoAP: TEdit
            Left = 276
            Top = 106
            Width = 121
            Height = 21
            Cursor = crIBeam
            TabOrder = 7
            OnChange = edSH_VersaoAPChange
          end
          object edSH_Linha1: TEdit
            Left = 12
            Top = 146
            Width = 181
            Height = 21
            Cursor = crIBeam
            TabOrder = 8
            OnChange = edSH_Linha1Change
          end
          object edSH_Linha2: TEdit
            Left = 208
            Top = 146
            Width = 189
            Height = 21
            Cursor = crIBeam
            TabOrder = 9
            OnChange = edSH_Linha2Change
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'ACBrRFD.INI'
          object Panel4: TPanel
            Left = 0
            Top = 230
            Width = 778
            Height = 34
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
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
            Width = 778
            Height = 230
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 12
            Font.Name = 'Fixedsys'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 55
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          786
          55)
        object sbDirRFD: TSpeedButton
          Left = 761
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
          Width = 108
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
          Width = 627
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
    object tsTagsImpressao: TTabSheet
      Caption = 'Tags de Impress'#227'o'
      ImageIndex = 6
      DesignSize = (
        786
        347)
      object Label28: TLabel
        Left = 84
        Top = 24
        Width = 29
        Height = 13
        Caption = 'Altura'
        Color = clBtnFace
        ParentColor = False
      end
      object Label27: TLabel
        Left = 12
        Top = 24
        Width = 37
        Height = 13
        Caption = 'Largura'
        Color = clBtnFace
        ParentColor = False
      end
      object Label23: TLabel
        Left = 0
        Top = 0
        Width = 786
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Tags de Formata'#231#227'o de Impress'#227'o e C'#243'digo de Barras'
        Color = clBtnFace
        ParentColor = False
      end
      object speBarrasLargura: TSpinEdit
        Left = 12
        Top = 40
        Width = 46
        Height = 22
        AutoSize = False
        MaxValue = 6
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = speBarrasLarguraChange
      end
      object BitBtn6: TBitBtn
        Left = 610
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
        Top = 143
        Width = 786
        Height = 204
        Align = alBottom
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Pitch = fpVariable
        Font.Style = []
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
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 6
      end
      object chBarrasImprimeTexto: TCheckBox
        Left = 164
        Top = 40
        Width = 151
        Height = 19
        Caption = 'Imprime Texto na Barra'
        Checked = True
        State = cbChecked
        TabOrder = 1
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
        TabOrder = 3
        Value = 0
        OnChange = speBarrasAlturaChange
      end
      object chIgnorarTagsFormatacao: TCheckBox
        Left = 164
        Top = 66
        Width = 183
        Height = 19
        Caption = 'Ignorar TAGs de Formata'#231#227'o'
        TabOrder = 5
        OnClick = chIgnorarTagsFormatacaoClick
      end
      object Button2: TButton
        Left = 668
        Top = 51
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '1...255'
        TabOrder = 4
        OnClick = Button2Click
      end
    end
    object tsDadosRedZ: TTabSheet
      Caption = 'Dados Redu'#231#227'o Z'
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
        Top = 60
        Width = 786
        Height = 287
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 13
        Font.Name = 'Courier New'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 2
        WantReturns = False
        WordWrap = False
      end
      object btnDadosUltimaRZ: TButton
        Left = 152
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Ler Dados '#218'ltima RZ'
        TabOrder = 1
        OnClick = btnDadosUltimaRZClick
      end
    end
    object tbsMenuFiscal: TTabSheet
      Caption = 'Menu Fiscal'
      ImageIndex = 6
      object grpMenuFiscalOpcoes: TGroupBox
        Left = 0
        Top = 0
        Width = 786
        Height = 279
        Align = alClient
        Caption = 'Op'#231#245'es do Menu fiscal'
        TabOrder = 0
        object Label31: TLabel
          Left = 10
          Top = 165
          Width = 272
          Height = 13
          Caption = 'Para os menus que geram arquivos, verificar o DemoPAF'
          Font.Charset = ANSI_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
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
          TabOrder = 3
          OnClick = btnMenuFiscalLMFCClick
        end
        object btnMenuFiscalLMFS: TButton
          Left = 10
          Top = 82
          Width = 171
          Height = 25
          Caption = 'LMFS'
          TabOrder = 6
          OnClick = btnMenuFiscalLMFSClick
        end
        object btnMenuFiscalMFDEspelho: TButton
          Left = 187
          Top = 20
          Width = 171
          Height = 25
          Caption = 'Espelho MFD'
          TabOrder = 1
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
          TabOrder = 2
          OnClick = btnMenuFiscalRelMeiosPagtoClick
        end
        object btnMenuFiscalRelDAVEmitidos: TButton
          Left = 364
          Top = 51
          Width = 171
          Height = 25
          Caption = 'DAV Emitidos'
          TabOrder = 5
          OnClick = btnMenuFiscalRelDAVEmitidosClick
        end
        object btnMenuFiscalRelIdentPAFECF: TButton
          Left = 364
          Top = 82
          Width = 171
          Height = 25
          Caption = 'Identifica'#231#227'o PAF-ECF'
          TabOrder = 8
          OnClick = btnMenuFiscalRelIdentPAFECFClick
        end
        object btnMenuFiscalConfigPAFECF: TButton
          Left = 364
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Configura'#231#245'es do PAF-ECF'
          TabOrder = 11
          OnClick = btnMenuFiscalConfigPAFECFClick
        end
        object btnMenuFiscalNotaPaulista: TButton
          Left = 187
          Top = 82
          Width = 171
          Height = 25
          Caption = 'CAT52'
          TabOrder = 7
          OnClick = btnMenuFiscalNotaPaulistaClick
        end
        object btnArqMFNovo: TButton
          Left = 10
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Arq.MF (novo)'
          TabOrder = 9
          OnClick = btnArqMFNovoClick
        end
        object btnArqMFDNovo: TButton
          Left = 187
          Top = 113
          Width = 171
          Height = 25
          Caption = 'Arq.MFD (novo)'
          TabOrder = 10
          OnClick = btnArqMFDNovoClick
        end
      end
      object pgcMenuFiscalTipo: TPageControl
        Left = 0
        Top = 279
        Width = 786
        Height = 68
        ActivePage = tbsMenuFiscalTipoData
        Align = alBottom
        TabOrder = 3
        object tbsMenuFiscalTipoData: TTabSheet
          Caption = 'Emiss'#227'o por intervalo de data'
          object Label24: TLabel
            Left = 15
            Top = 15
            Width = 19
            Height = 13
            Alignment = taRightJustify
            Caption = 'de :'
          end
          object Label25: TLabel
            Left = 142
            Top = 15
            Width = 16
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
            Left = 15
            Top = 15
            Width = 19
            Height = 13
            Alignment = taRightJustify
            Caption = 'de :'
          end
          object Label30: TLabel
            Left = 142
            Top = 15
            Width = 16
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
        TabOrder = 1
      end
    end
    object tsArqAuxCript: TTabSheet
      Caption = 'Arq.Aux.Cript.'
      ImageIndex = 7
      DesignSize = (
        786
        347)
      object Label33: TLabel
        Left = 9
        Top = 234
        Width = 71
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Nome Arquivo:'
        Color = clBtnFace
        ParentColor = False
      end
      object SbAACNomeArq: TSpeedButton
        Left = 421
        Top = 228
        Width = 24
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = '...'
        OnClick = SbAACNomeArqClick
      end
      object Label41: TLabel
        Left = 19
        Top = 263
        Width = 61
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Arquivo Log:'
        Color = clBtnFace
        ParentColor = False
      end
      object SbAACArqLog: TSpeedButton
        Left = 421
        Top = 257
        Width = 24
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = '...'
        OnClick = SbAACArqLogClick
      end
      object bAACGravarArquivo: TButton
        Left = 477
        Top = 257
        Width = 102
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Gravar Arquivo'
        TabOrder = 4
        OnClick = bAACGravarArquivoClick
      end
      object edAACNomeArq: TEdit
        Left = 84
        Top = 228
        Width = 329
        Height = 21
        Cursor = crIBeam
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        Text = 'arquivo_auxiliar_criptografado.txt'
      end
      object edAACLog: TEdit
        Left = 84
        Top = 257
        Width = 329
        Height = 21
        Cursor = crIBeam
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 3
        Text = 'acbr_aac_log.txt'
      end
      object pgAAC: TPageControl
        Left = 0
        Top = 0
        Width = 786
        Height = 213
        ActivePage = tsAACECFs
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object tsAACDados: TTabSheet
          Caption = 'Dados Cadastrais'
          object gbAAC_SH: TGroupBox
            Left = 0
            Top = 0
            Width = 778
            Height = 99
            Align = alTop
            Caption = 'Dados da Software House'
            TabOrder = 0
            object Label34: TLabel
              Left = 50
              Top = 38
              Width = 25
              Height = 13
              Caption = 'CNPJ'
              Color = clBtnFace
              ParentColor = False
            end
            object Label35: TLabel
              Left = 186
              Top = 37
              Width = 65
              Height = 13
              Caption = 'Insc.Estadual'
              Color = clBtnFace
              ParentColor = False
            end
            object Label36: TLabel
              Left = 314
              Top = 38
              Width = 67
              Height = 13
              Caption = 'Insc.Municipal'
              Color = clBtnFace
              ParentColor = False
            end
            object edAAC_SH_RazaoSocial: TEdit
              Left = 50
              Top = 15
              Width = 385
              Height = 21
              Cursor = crIBeam
              MaxLength = 40
              TabOrder = 0
            end
            object edAAC_SH_CNPJ: TEdit
              Left = 50
              Top = 53
              Width = 121
              Height = 21
              Cursor = crIBeam
              MaxLength = 14
              TabOrder = 1
            end
            object edAAC_SH_IE: TEdit
              Left = 186
              Top = 53
              Width = 114
              Height = 21
              Cursor = crIBeam
              MaxLength = 14
              TabOrder = 2
            end
            object edAAC_SH_IM: TEdit
              Left = 314
              Top = 53
              Width = 121
              Height = 21
              Cursor = crIBeam
              MaxLength = 14
              TabOrder = 3
            end
          end
          object gbAAC_PAF: TGroupBox
            Left = 0
            Top = 99
            Width = 778
            Height = 105
            Align = alTop
            Caption = 'Dados do PAF-ECF'
            TabOrder = 1
            DesignSize = (
              778
              105)
            object Label38: TLabel
              Left = 50
              Top = -3
              Width = 46
              Height = 13
              Caption = 'Aplicativo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label40: TLabel
              Left = 314
              Top = -3
              Width = 40
              Height = 13
              Caption = 'Vers'#195#163'o'
              Color = clBtnFace
              ParentColor = False
            end
            object Label39: TLabel
              Left = 50
              Top = 35
              Width = 21
              Height = 13
              Caption = 'MD5'
              Color = clBtnFace
              ParentColor = False
            end
            object SbAACMD5Atualizar: TSpeedButton
              Left = 438
              Top = 51
              Width = 24
              Height = 22
              Anchors = [akLeft, akBottom]
              Caption = '...'
              OnClick = SbAACMD5AtualizarClick
            end
            object edAAC_PAF_Aplicativo: TEdit
              Left = 50
              Top = 13
              Width = 242
              Height = 21
              Cursor = crIBeam
              MaxLength = 40
              TabOrder = 0
            end
            object edAAC_PAF_Versao: TEdit
              Left = 314
              Top = 13
              Width = 121
              Height = 21
              Cursor = crIBeam
              MaxLength = 40
              TabOrder = 1
            end
            object edAAC_PAF_MD5: TEdit
              Left = 50
              Top = 51
              Width = 385
              Height = 21
              Cursor = crIBeam
              MaxLength = 32
              TabOrder = 2
            end
          end
        end
        object tsAACECFs: TTabSheet
          Caption = 'ECFs'
          DesignSize = (
            778
            185)
          object Label42: TLabel
            Left = 8
            Top = 12
            Width = 52
            Height = 13
            Caption = 'N'#250'm. S'#233'rie'
            Color = clBtnFace
            ParentColor = False
          end
          object Label43: TLabel
            Left = 184
            Top = 12
            Width = 22
            Height = 13
            Caption = 'CRO'
            Color = clBtnFace
            ParentColor = False
          end
          object Label44: TLabel
            Left = 280
            Top = 12
            Width = 89
            Height = 13
            Caption = 'Valor Grande Total'
            Color = clBtnFace
            ParentColor = False
          end
          object DBGrid1: TDBGrid
            Left = 0
            Top = 65
            Width = 778
            Height = 120
            Align = alBottom
            TabOrder = 5
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clBlack
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Pitch = fpVariable
            TitleFont.Style = []
          end
          object edAAC_ECF_NumSerie: TDBEdit
            Left = 8
            Top = 28
            Width = 160
            Height = 21
            Cursor = crIBeam
            DataField = 'NumSerie'
            TabOrder = 1
          end
          object edAAC_ECF_CRO: TDBEdit
            Left = 184
            Top = 28
            Width = 74
            Height = 21
            Cursor = crIBeam
            DataField = 'CRO'
            TabOrder = 2
          end
          object edAAC_ECF_GT: TDBEdit
            Left = 280
            Top = 28
            Width = 140
            Height = 21
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            DataField = 'ValorGT'
            TabOrder = 3
          end
          object bACCVerificarGT: TButton
            Left = 440
            Top = 6
            Width = 83
            Height = 25
            Caption = 'GT Valido ?'
            TabOrder = 0
            OnClick = bACCVerificarGTClick
          end
          object bAACAtualizarGT: TButton
            Left = 440
            Top = 34
            Width = 83
            Height = 25
            Caption = 'Atualizar GT'
            TabOrder = 4
            OnClick = bAACAtualizarGTClick
          end
        end
        object tsAACParams: TTabSheet
          Caption = 'Params'
          object mAACParams: TMemo
            Left = 0
            Top = 0
            Width = 778
            Height = 185
            Align = alClient
            Lines.Strings = (
              'ExemploParametro1 = VALOR'
              'ExemploParametro2 = 1'
              'ExemploParametro3 = 20110504'
              '')
            TabOrder = 0
          end
        end
      end
      object bAACAbrirArquivo: TButton
        Left = 477
        Top = 226
        Width = 102
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Abrir Arquivo'
        TabOrder = 2
        OnClick = bAACAbrirArquivoClick
      end
    end
    object tsSAT: TTabSheet
      Caption = 'SAT Virtual'
      ImageIndex = 8
      object pgSAT: TPageControl
        Left = 0
        Top = 0
        Width = 786
        Height = 347
        ActivePage = tsSATImpressao
        Align = alClient
        TabOrder = 0
        object tsDadosSAT: TTabSheet
          Caption = 'Dados do SAT CFe'
          DesignSize = (
            778
            319)
          object Label47: TLabel
            Left = 14
            Top = 7
            Width = 42
            Height = 13
            Alignment = taRightJustify
            Caption = 'Arq.Log:'
            Color = clBtnFace
            ParentColor = False
          end
          object SbArqLog1: TSpeedButton
            Left = 185
            Top = 24
            Width = 24
            Height = 22
            Caption = '...'
            OnClick = SbArqLogClick
          end
          object Label48: TLabel
            Left = 220
            Top = 7
            Width = 51
            Height = 13
            Alignment = taRightJustify
            Caption = 'Nome DLL:'
            Color = clBtnFace
            ParentColor = False
          end
          object Label49: TLabel
            Left = 16
            Top = 56
            Width = 93
            Height = 13
            Caption = 'C'#243'digo de Ativa'#231#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object Label50: TLabel
            Left = 217
            Top = 56
            Width = 36
            Height = 13
            Caption = 'C'#243'd.UF'
            Color = clBtnFace
            ParentColor = False
          end
          object Label51: TLabel
            Left = 312
            Top = 57
            Width = 52
            Height = 13
            Caption = 'Num.Caixa'
            Color = clBtnFace
            ParentColor = False
          end
          object Label52: TLabel
            Left = 152
            Top = 106
            Width = 59
            Height = 13
            Caption = 'P'#225'g.C'#243'digo:'
            Color = clBtnFace
            ParentColor = False
          end
          object Label53: TLabel
            Left = 257
            Top = 106
            Width = 33
            Height = 13
            Caption = 'Vers'#227'o'
            Color = clBtnFace
            ParentColor = False
          end
          object Label54: TLabel
            Left = 385
            Top = 55
            Width = 45
            Height = 13
            Caption = 'Ambiente'
            Color = clBtnFace
            ParentColor = False
          end
          object Label70: TLabel
            Left = 20
            Top = 176
            Width = 56
            Height = 13
            Alignment = taRightJustify
            Caption = 'Modelo SAT'
            Color = clBtnFace
            ParentColor = False
          end
          object edLogSAT: TEdit
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
            Width = 390
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
            TabOrder = 2
          end
          object edtCodUF: TEdit
            Left = 218
            Top = 72
            Width = 61
            Height = 21
            TabOrder = 3
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
          object cbxUTF8: TCheckBox
            Left = 16
            Top = 103
            Width = 47
            Height = 19
            Caption = 'UTF8'
            TabOrder = 6
          end
          object sePagCod: TSpinEdit
            Left = 152
            Top = 123
            Width = 83
            Height = 22
            MaxValue = 65001
            MinValue = 0
            TabOrder = 9
            Value = 0
          end
          object cbxFormatXML: TCheckBox
            Left = 17
            Top = 127
            Width = 86
            Height = 19
            Caption = 'Formatar XML'
            Checked = True
            State = cbChecked
            TabOrder = 10
          end
          object cbxAmbiente: TComboBox
            Left = 385
            Top = 72
            Width = 222
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 4
          end
          object cbxSalvarCFe: TCheckBox
            Left = 424
            Top = 120
            Width = 77
            Height = 19
            Caption = 'Salvar CFes'
            Checked = True
            State = cbChecked
            TabOrder = 7
          end
          object cbxModeloSAT: TComboBox
            Left = 17
            Top = 192
            Width = 133
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 11
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
          object sfeVersaoEnt: TEdit
            Left = 257
            Top = 123
            Width = 121
            Height = 21
            TabOrder = 8
            Text = '0,00'
          end
        end
        object tsDadosEmit: TTabSheet
          Caption = 'Dados Emitente'
          object Label55: TLabel
            Left = 12
            Top = 23
            Width = 25
            Height = 13
            Caption = 'CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object Label56: TLabel
            Left = 192
            Top = 23
            Width = 65
            Height = 13
            Caption = 'Insc.Estadual'
            Color = clBtnFace
            ParentColor = False
          end
          object Label57: TLabel
            Left = 336
            Top = 23
            Width = 67
            Height = 13
            Caption = 'Insc.Municipal'
            Color = clBtnFace
            ParentColor = False
          end
          object Label58: TLabel
            Left = 192
            Top = 71
            Width = 94
            Height = 13
            Caption = 'Regime Trib. ISSQN'
            Color = clBtnFace
            ParentColor = False
          end
          object Label59: TLabel
            Left = 336
            Top = 71
            Width = 72
            Height = 13
            Caption = 'Ind.Rat.ISSQN'
            Color = clBtnFace
            ParentColor = False
          end
          object Label60: TLabel
            Left = 12
            Top = 73
            Width = 84
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
            ItemHeight = 0
            TabOrder = 4
          end
          object cbxIndRatISSQN: TComboBox
            Left = 336
            Top = 87
            Width = 134
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 5
          end
          object cbxRegTributario: TComboBox
            Left = 12
            Top = 87
            Width = 166
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 3
          end
        end
        object tsDadosSwHouse: TTabSheet
          Caption = 'Dados Sw.House'
          DesignSize = (
            778
            319)
          object Label61: TLabel
            Left = 10
            Top = 15
            Width = 25
            Height = 13
            Caption = 'CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object Label62: TLabel
            Left = 10
            Top = 71
            Width = 185
            Height = 13
            Caption = 'Assinatura Sw.House (344 caracteres)'
            Color = clBtnFace
            ParentColor = False
          end
          object edtSwHCNPJ: TEdit
            Left = 10
            Top = 31
            Width = 331
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edtSwHAssinatura: TEdit
            Left = 10
            Top = 89
            Width = 596
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
        end
        object tsSATImpressao: TTabSheet
          Caption = 'Impress'#227'o'
          object GroupBox3: TGroupBox
            Left = 0
            Top = 0
            Width = 522
            Height = 319
            Align = alClient
            Caption = 'Fortes'
            TabOrder = 0
            object Label63: TLabel
              Left = 8
              Top = 0
              Width = 37
              Height = 13
              Caption = 'Largura'
              Color = clBtnFace
              ParentColor = False
            end
            object Label64: TLabel
              Left = 96
              Top = 0
              Width = 24
              Height = 13
              Caption = 'Topo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label65: TLabel
              Left = 8
              Top = 48
              Width = 30
              Height = 13
              Caption = 'Fundo'
              Color = clBtnFace
              ParentColor = False
            end
            object Label66: TLabel
              Left = 96
              Top = 48
              Width = 45
              Height = 13
              Caption = 'Esquerda'
              Color = clBtnFace
              ParentColor = False
            end
            object Label67: TLabel
              Left = 184
              Top = 48
              Width = 31
              Height = 13
              Caption = 'Direita'
              Color = clBtnFace
              ParentColor = False
            end
            object lImpressora: TLabel
              Left = 136
              Top = 104
              Width = 97
              Height = 13
              Caption = 'Impresssora Default'
            end
            object seLargura: TSpinEdit
              Left = 8
              Top = 14
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object seMargemTopo: TSpinEdit
              Left = 96
              Top = 14
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
            object seMargemFundo: TSpinEdit
              Left = 8
              Top = 62
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object seMargemEsquerda: TSpinEdit
              Left = 96
              Top = 62
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 5
              Value = 0
            end
            object seMargemDireita: TSpinEdit
              Left = 184
              Top = 62
              Width = 64
              Height = 22
              MaxValue = 9999
              MinValue = 0
              TabOrder = 6
              Value = 0
            end
            object bImpressora: TButton
              Left = 8
              Top = 96
              Width = 110
              Height = 23
              Caption = 'Definir Impressora'
              TabOrder = 7
            end
            object cbUsarFortes: TRadioButton
              Left = 192
              Top = 0
              Width = 74
              Height = 19
              Caption = 'Usar Fortes'
              TabOrder = 0
            end
            object cbPreview: TCheckBox
              Left = 192
              Top = 24
              Width = 58
              Height = 19
              Caption = 'Preview'
              TabOrder = 3
            end
          end
          object GroupBox4: TGroupBox
            Left = 522
            Top = 0
            Width = 256
            Height = 319
            Align = alRight
            Caption = 'EscPOS'
            TabOrder = 1
            DesignSize = (
              256
              319)
            object Label68: TLabel
              Left = 26
              Top = 56
              Width = 83
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Porta Impressora'
              Color = clBtnFace
              ParentColor = False
            end
            object edtPorta: TEdit
              Left = 16
              Top = 73
              Width = 215
              Height = 21
              Anchors = [akTop, akRight]
              TabOrder = 2
              Text = '\\127.0.0.1\EPSON'
            end
            object btSerial1: TBitBtn
              Left = 179
              Top = 8
              Width = 52
              Height = 43
              Anchors = [akTop, akRight]
              Caption = 'Serial'
              ModalResult = 1
              TabOrder = 1
              OnClick = btSerial1Click
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
              Layout = blGlyphTop
            end
            object cbUsarEscPos: TRadioButton
              Left = 8
              Top = 0
              Width = 85
              Height = 19
              Caption = 'Usar EscPOS'
              TabOrder = 0
            end
          end
        end
      end
    end
    object tsNFCe: TTabSheet
      Caption = 'NFCe'
      ImageIndex = 9
      object pgNFCe: TPageControl
        Left = 0
        Top = 0
        Width = 786
        Height = 347
        ActivePage = tsNFCeCertificado
        Align = alClient
        Anchors = []
        TabOrder = 0
        object tsNFCeCertificado: TTabSheet
          Caption = 'Certificado'
          object GroupBox2: TGroupBox
            Left = 6
            Top = 10
            Width = 270
            Height = 169
            Caption = 'Certificado'
            TabOrder = 0
            object Label71: TLabel
              Left = 8
              Top = 16
              Width = 41
              Height = 13
              Caption = 'Caminho'
              Color = clBtnFace
              ParentColor = False
            end
            object Label72: TLabel
              Left = 8
              Top = 56
              Width = 30
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object sbtnCaminhoCert: TSpeedButton
              Left = 234
              Top = 32
              Width = 24
              Height = 24
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000064000000640000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000000000000033000000330000000A469AD300469A
                D300469AD300469AD300469AD300459AD400429DDC0071777C0075706D007472
                700075726F007B736B00000000333A6EA0FF3B6E9EFF213F5A5D469AD3000000
                000B0000000A0000000A0000000A0000000A0000000A00000011000000320000
                00330000003300000033316EA7FF29B2FFFF44C8FFFF3A83CCFF0000002F2959
                7A5F2655755C2655745C2655755C2555755B225578594D5359776B696AF96767
                6BFF67676AFF6D6A69FF81746BFF3BC6FFFF57DBFFFF3980C9FF4598D0F14398
                D2FF4094D0FF3E92CFFF3E92CFFF3D95D4FF4D8BB8FF79706BFFE9C890FFFFEE
                A7FFFFF5B1FFE8D5A3FF77716CFFA08F85FF327ECDFF3881CB004499D2FF3F94
                D0FFABFBFFFF9BF3FFFF92F2FFFF92F8FFFF797C7DFFEAC186FFFFE7A6FFFFE7
                A6FFFFEFB3FFFFF9BBFFE9D6A2FF817772FF2F75A467469CD5004397D1FF56AC
                DDFF8EDAF5FFA2EDFFFF82E6FFFF82EDFFFF766D6DFFFFE19DFFFFEFCAFFFFE7
                B3FFFFE9ABFFFFEFB2FFFFF3AEFF7C7370FF3990CCC5449AD5004296D1FF71C4
                EAFF6CBCE6FFBBF2FFFF75DFFFFF75E6FFFF7B7171FFFFDD97FFFFF7E4FFFFED
                C8FFFFE7B2FFFFE6A5FFFFEBA3FF807674FF4AA3DFFF000000124095D0FF90DD
                F8FF44A0D8FFDDFCFFFFDAFBFFFFDAFFFFFF8D8A8AFFEDBD7CFFFFF5DBFFFFF7
                E4FFFFEECAFFFFE5A4FFEBC890FF89898AFF7BC6EFFF3277A6713E93CFFFB2F6
                FFFF51ACDEFF358ACAFF358BCBFF348DCFFF4F8CB9FF93857DFFEEBE7DFFFFDD
                96FFFFE09BFFECC589FF8C817DFF8AC8DCFFA9E4FCFF3A8AC2C83D92CFFFB8F3
                FFFF77DFFEFF7BE0FEFF7BE1FFFF7BE1FFFF79E4FFFF69ACCEFF88878DFF9389
                85FF928986FF9A9795FFBED8E2FFD4FCFFFFDBFDFFFF3E94D0FF3C92CFFFC0F3
                FFFF70D9FBFF73DAFBFF74DAFBFF74DAFBFF73DBFDFF73DFFFFF4CAEE4FF3692
                D5FF3692D5FF3591D3FF3390D1FF3791D0FF3D94D0FF4398D2AE3B92CFFFCAF6
                FFFF69D5F9FF6CD5F9FF6AD4F9FF69D4F9FF69D5F9FF6AD6FBFF6BD9FCFF6CDB
                FEFF6CDBFEFF6ADBFEFFDBFDFFFF3C93D0FF367BAA2F469AD3003B92CFFFD5F7
                FFFF60D1F9FF61D0F8FFB4EBFDFFD9F6FFFFDAF8FFFFDAF8FFFFDAF9FFFFDBF9
                FFFFDAF9FFFFDAFAFFFFDFFEFFFF3D94D0FF4599D335469AD3003D94D0FFDCFC
                FFFFD8F7FFFFD8F7FFFFDBFAFFFF358ECDFF3991CEFF3A92CFFF3A92CFFF3A92
                CFFF3A92CFFF3A92CFFF3D94D0FF4298D2EA469AD300469AD3004398D2EF3D94
                D0FF3A92CFFF3A92CFFF3D94D0FF4197D1E44398D22B4498D2324498D2334498
                D2334498D2334499D2334499D337459AD300469AD300469AD300}
              OnClick = sbtnCaminhoCertClick
            end
            object Label73: TLabel
              Left = 8
              Top = 96
              Width = 79
              Height = 13
              Caption = 'N'#250'mero de S'#233'rie'
              Color = clBtnFace
              ParentColor = False
            end
            object sbtnGetCert: TSpeedButton
              Left = 234
              Top = 111
              Width = 24
              Height = 24
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000064000000640000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000001E0000003300000033000000330000001E000000005CA3D9005CA3
                D9005CA3D9005CA3D9005CA3D9005CA3D9005EA3DB0065A3E50000A156000000
                001E008C55AC009E5EFF009D5DFF009E5EFF008C55AC0000001E5CA3D9005CA3
                D9005CA3D9005CA3D9005CA3D9005CA3D9005DA3DB0064A3E20000A05100008C
                51AB00A669FF00BA84FF77DFC4FF00BA84FF00A66AFF008C55AC000000300000
                003300000033000000330000003300000033000000330000003300000033009D
                54FF00C089FF00BB82FFFFFFFFFF00BB82FF00C08BFF009E5EFF579FD5F2539E
                D7FF509CD7FF4F9CD7FF509CD7FF509CD7FF509CD7FF529DD9FF5D9EE3FF009A
                4FFF75E5CAFFFFFFFFFFFFFFFFFFFFFFFFFF77E5CCFF009C5BFF539ED7FFF0F7
                F7FFEAF4F5FFEBF4F6FFECF5F6FFECF5F7FFECF5F7FFEEF6F9FFFBF9FFFF0098
                4DFF00CB94FF00C88EFFFFFFFFFF00C88EFF00CC96FF009D58FF509CD7FFEAF4
                F6FFE0EEF4FF90BEDFFF90BFDFFF8FBFE1FF8FC1E3FF91C2E6FF9AC6EDFF38AD
                98FF00AD6BFF00D39CFF73EDD3FF00D29AFF00AD68FF289F97FF509CD7FFECF5
                F6FF8ABCDCFF8ABCDCFFD6ECF3FFD3DBDEFFD0BCB2FFD2D2D1FFD3B9AFFFDFBA
                B5FF52B28DFF009B55FF009952FF00984EFF65C49EFF60A0E4FF509CD7FFEDF5
                F7FF84B9DBFFCFE8EFFFCAE6EFFFC9E9F6FFC47D5CFFC6947DFFC66940FFC98B
                72FFD18A72FFD78972FFDCA294FF94BFEAFFF8F9FFFF559DDAFF509CD7FFEEF6
                F7FF7EB4D9FFC5E3ECFFC2E2ECFFC2E4F0FFC2D8DFFFC3A89AFFC2B1A8FFC2EE
                FEFFC3ECFBFFC5EBFAFFC8E9F6FF81B7DDFFEFF7F8FF519CD7FF509CD7FFEEF6
                F7FF79B2D8FFBEE0ECFFBDE0EBFFBFE2EDFFC1E5EFFFBFE6F2FFBCE5F4FFACB0
                B2FFA9ACACFFACAFB0FFBEE4F0FF79B3D9FFEEF6F7FF509CD7FF509CD7FFEFF6
                F7FF74B1D6FFB9E0EAFFBAE1EAFF2876C5FF4A98D7FF4796D7FFB7E2EEFFACAC
                AAFFB4E5F5FFAEADABFFB8E3EEFF74B2D6FFEFF6F7FF509CD7FF509CD7FFEEF6
                F7FF6EAED3FF71B0D4FFB3DEE8FFB5DFE9FFB6E0E9FFB4DFE9FFB0DFEBFFAFB0
                AFFFAFAFABFFB1B1B0FF71B2D7FF6EAFD4FFEEF6F7FF509CD7FF509CD7FFEDF5
                F6FFA3D7E4FF68ABD1FF69ABD1FF69ABD1FF69ABD1FF69ABD1FF68ACD2FF68AD
                D4FF68AED5FF69AED4FF68ACD2FFA3D7E4FFEDF5F6FF509CD7FF539ED7FFF2F8
                F7FFEDF5F7FFEFF6F7FFF0F7F7FFF0F7F7FFF0F7F7FFF0F7F7FFF0F7F8FFF0F7
                F8FFF0F7F8FFF0F7F8FFEFF6F7FFEDF5F7FFF2F8F7FF539ED7FF58A1D8EF539E
                D7FF509CD7FF509CD7FF509DD7FF519DD7FF519DD7FF519DD7FF519DD7FF519D
                D7FF519DD7FF509DD7FF509CD7FF509CD7FF539ED7FF58A1D8EF}
              OnClick = sbtnGetCertClick
            end
            object edtCaminho: TEdit
              Left = 8
              Top = 32
              Width = 224
              Height = 21
              TabOrder = 0
            end
            object edtSenha: TEdit
              Left = 8
              Top = 72
              Width = 249
              Height = 21
              PasswordChar = '*'
              TabOrder = 1
            end
            object edtNumSerie: TEdit
              Left = 8
              Top = 112
              Width = 224
              Height = 21
              TabOrder = 2
            end
          end
        end
        object tsNFCeGeral: TTabSheet
          Caption = 'Geral'
          ImageIndex = 1
          object Label74: TLabel
            Left = 6
            Top = 176
            Width = 55
            Height = 13
            Caption = 'Logo Marca'
            Color = clBtnFace
            ParentColor = False
          end
          object sbtnLogoMarca: TSpeedButton
            Left = 231
            Top = 191
            Width = 24
            Height = 24
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000064000000640000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000033000000330000000A469AD300469A
              D300469AD300469AD300469AD300459AD400429DDC0071777C0075706D007472
              700075726F007B736B00000000333A6EA0FF3B6E9EFF213F5A5D469AD3000000
              000B0000000A0000000A0000000A0000000A0000000A00000011000000320000
              00330000003300000033316EA7FF29B2FFFF44C8FFFF3A83CCFF0000002F2959
              7A5F2655755C2655745C2655755C2555755B225578594D5359776B696AF96767
              6BFF67676AFF6D6A69FF81746BFF3BC6FFFF57DBFFFF3980C9FF4598D0F14398
              D2FF4094D0FF3E92CFFF3E92CFFF3D95D4FF4D8BB8FF79706BFFE9C890FFFFEE
              A7FFFFF5B1FFE8D5A3FF77716CFFA08F85FF327ECDFF3881CB004499D2FF3F94
              D0FFABFBFFFF9BF3FFFF92F2FFFF92F8FFFF797C7DFFEAC186FFFFE7A6FFFFE7
              A6FFFFEFB3FFFFF9BBFFE9D6A2FF817772FF2F75A467469CD5004397D1FF56AC
              DDFF8EDAF5FFA2EDFFFF82E6FFFF82EDFFFF766D6DFFFFE19DFFFFEFCAFFFFE7
              B3FFFFE9ABFFFFEFB2FFFFF3AEFF7C7370FF3990CCC5449AD5004296D1FF71C4
              EAFF6CBCE6FFBBF2FFFF75DFFFFF75E6FFFF7B7171FFFFDD97FFFFF7E4FFFFED
              C8FFFFE7B2FFFFE6A5FFFFEBA3FF807674FF4AA3DFFF000000124095D0FF90DD
              F8FF44A0D8FFDDFCFFFFDAFBFFFFDAFFFFFF8D8A8AFFEDBD7CFFFFF5DBFFFFF7
              E4FFFFEECAFFFFE5A4FFEBC890FF89898AFF7BC6EFFF3277A6713E93CFFFB2F6
              FFFF51ACDEFF358ACAFF358BCBFF348DCFFF4F8CB9FF93857DFFEEBE7DFFFFDD
              96FFFFE09BFFECC589FF8C817DFF8AC8DCFFA9E4FCFF3A8AC2C83D92CFFFB8F3
              FFFF77DFFEFF7BE0FEFF7BE1FFFF7BE1FFFF79E4FFFF69ACCEFF88878DFF9389
              85FF928986FF9A9795FFBED8E2FFD4FCFFFFDBFDFFFF3E94D0FF3C92CFFFC0F3
              FFFF70D9FBFF73DAFBFF74DAFBFF74DAFBFF73DBFDFF73DFFFFF4CAEE4FF3692
              D5FF3692D5FF3591D3FF3390D1FF3791D0FF3D94D0FF4398D2AE3B92CFFFCAF6
              FFFF69D5F9FF6CD5F9FF6AD4F9FF69D4F9FF69D5F9FF6AD6FBFF6BD9FCFF6CDB
              FEFF6CDBFEFF6ADBFEFFDBFDFFFF3C93D0FF367BAA2F469AD3003B92CFFFD5F7
              FFFF60D1F9FF61D0F8FFB4EBFDFFD9F6FFFFDAF8FFFFDAF8FFFFDAF9FFFFDBF9
              FFFFDAF9FFFFDAFAFFFFDFFEFFFF3D94D0FF4599D335469AD3003D94D0FFDCFC
              FFFFD8F7FFFFD8F7FFFFDBFAFFFF358ECDFF3991CEFF3A92CFFF3A92CFFF3A92
              CFFF3A92CFFF3A92CFFF3D94D0FF4298D2EA469AD300469AD3004398D2EF3D94
              D0FF3A92CFFF3A92CFFF3D94D0FF4197D1E44398D22B4498D2324498D2334498
              D2334498D2334499D2334499D337459AD300469AD300469AD300}
            OnClick = sbtnLogoMarcaClick
          end
          object sbtnPathSalvar: TSpeedButton
            Left = 231
            Top = 247
            Width = 24
            Height = 24
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000064000000640000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000033000000330000000A469AD300469A
              D300469AD300469AD300469AD300459AD400429DDC0071777C0075706D007472
              700075726F007B736B00000000333A6EA0FF3B6E9EFF213F5A5D469AD3000000
              000B0000000A0000000A0000000A0000000A0000000A00000011000000320000
              00330000003300000033316EA7FF29B2FFFF44C8FFFF3A83CCFF0000002F2959
              7A5F2655755C2655745C2655755C2555755B225578594D5359776B696AF96767
              6BFF67676AFF6D6A69FF81746BFF3BC6FFFF57DBFFFF3980C9FF4598D0F14398
              D2FF4094D0FF3E92CFFF3E92CFFF3D95D4FF4D8BB8FF79706BFFE9C890FFFFEE
              A7FFFFF5B1FFE8D5A3FF77716CFFA08F85FF327ECDFF3881CB004499D2FF3F94
              D0FFABFBFFFF9BF3FFFF92F2FFFF92F8FFFF797C7DFFEAC186FFFFE7A6FFFFE7
              A6FFFFEFB3FFFFF9BBFFE9D6A2FF817772FF2F75A467469CD5004397D1FF56AC
              DDFF8EDAF5FFA2EDFFFF82E6FFFF82EDFFFF766D6DFFFFE19DFFFFEFCAFFFFE7
              B3FFFFE9ABFFFFEFB2FFFFF3AEFF7C7370FF3990CCC5449AD5004296D1FF71C4
              EAFF6CBCE6FFBBF2FFFF75DFFFFF75E6FFFF7B7171FFFFDD97FFFFF7E4FFFFED
              C8FFFFE7B2FFFFE6A5FFFFEBA3FF807674FF4AA3DFFF000000124095D0FF90DD
              F8FF44A0D8FFDDFCFFFFDAFBFFFFDAFFFFFF8D8A8AFFEDBD7CFFFFF5DBFFFFF7
              E4FFFFEECAFFFFE5A4FFEBC890FF89898AFF7BC6EFFF3277A6713E93CFFFB2F6
              FFFF51ACDEFF358ACAFF358BCBFF348DCFFF4F8CB9FF93857DFFEEBE7DFFFFDD
              96FFFFE09BFFECC589FF8C817DFF8AC8DCFFA9E4FCFF3A8AC2C83D92CFFFB8F3
              FFFF77DFFEFF7BE0FEFF7BE1FFFF7BE1FFFF79E4FFFF69ACCEFF88878DFF9389
              85FF928986FF9A9795FFBED8E2FFD4FCFFFFDBFDFFFF3E94D0FF3C92CFFFC0F3
              FFFF70D9FBFF73DAFBFF74DAFBFF74DAFBFF73DBFDFF73DFFFFF4CAEE4FF3692
              D5FF3692D5FF3591D3FF3390D1FF3791D0FF3D94D0FF4398D2AE3B92CFFFCAF6
              FFFF69D5F9FF6CD5F9FF6AD4F9FF69D4F9FF69D5F9FF6AD6FBFF6BD9FCFF6CDB
              FEFF6CDBFEFF6ADBFEFFDBFDFFFF3C93D0FF367BAA2F469AD3003B92CFFFD5F7
              FFFF60D1F9FF61D0F8FFB4EBFDFFD9F6FFFFDAF8FFFFDAF8FFFFDAF9FFFFDBF9
              FFFFDAF9FFFFDAFAFFFFDFFEFFFF3D94D0FF4599D335469AD3003D94D0FFDCFC
              FFFFD8F7FFFFD8F7FFFFDBFAFFFF358ECDFF3991CEFF3A92CFFF3A92CFFF3A92
              CFFF3A92CFFF3A92CFFF3D94D0FF4298D2EA469AD300469AD3004398D2EF3D94
              D0FF3A92CFFF3A92CFFF3D94D0FF4197D1E44398D22B4498D2324498D2334498
              D2334498D2334499D2334499D337459AD300469AD300469AD300}
            OnClick = sbtnPathSalvarClick
          end
          object rgTipoDanfe: TRadioGroup
            Left = 6
            Top = 16
            Width = 249
            Height = 49
            Caption = 'DANFE'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Retrato'
              'Paisagem')
            TabOrder = 0
          end
          object rgFormaEmissao: TRadioGroup
            Left = 6
            Top = 72
            Width = 249
            Height = 97
            Caption = 'Forma de Emiss'#227'o'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Normal'
              'Conting'#234'ncia'
              'SCAN'
              'DPEC'
              'FSDA')
            TabOrder = 1
          end
          object edtLogoMarca: TEdit
            Left = 6
            Top = 192
            Width = 223
            Height = 21
            TabOrder = 2
          end
          object ckSalvar: TCheckBox
            Left = 8
            Top = 226
            Width = 196
            Height = 19
            Caption = 'Salvar Arquivos de Envio e Resposta'
            TabOrder = 3
          end
          object edtPathLogs: TEdit
            Left = 6
            Top = 248
            Width = 223
            Height = 21
            TabOrder = 4
          end
        end
        object tsNFCeWebservice: TTabSheet
          Caption = 'WebService'
          ImageIndex = 2
          object GroupBox6: TGroupBox
            Left = 8
            Top = 9
            Width = 265
            Height = 161
            Caption = 'WebService'
            TabOrder = 1
            object Label75: TLabel
              Left = 8
              Top = 16
              Width = 119
              Height = 13
              Caption = 'Selecione UF de Destino:'
              Color = clBtnFace
              ParentColor = False
            end
            object ckVisualizar: TCheckBox
              Left = 8
              Top = 118
              Width = 119
              Height = 19
              Caption = 'Visualizar Mensagem'
              TabOrder = 2
            end
            object cbUF: TComboBox
              Left = 8
              Top = 32
              Width = 249
              Height = 24
              Style = csDropDownList
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Pitch = fpVariable
              Font.Style = []
              ItemHeight = 16
              ItemIndex = 24
              ParentFont = False
              TabOrder = 0
              Text = 'SP'
              Items.Strings = (
                'AC'
                'AL'
                'AP'
                'AM'
                'BA'
                'CE'
                'DF'
                'ES'
                'GO'
                'MA'
                'MT'
                'MS'
                'MG'
                'PA'
                'PB'
                'PR'
                'PE'
                'PI'
                'RJ'
                'RN'
                'RS'
                'RO'
                'RR'
                'SC'
                'SP'
                'SE'
                'TO')
            end
            object rgTipoAmb: TRadioGroup
              Left = 8
              Top = 61
              Width = 249
              Height = 52
              Caption = 'Selecione o Ambiente de Destino'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                'Produ'#231#227'o'
                'Homologa'#231#227'o')
              TabOrder = 1
            end
          end
          object gbProxy: TGroupBox
            Left = 280
            Top = 9
            Width = 265
            Height = 123
            Caption = 'Proxy'
            TabOrder = 0
            object Label76: TLabel
              Left = 8
              Top = 16
              Width = 22
              Height = 13
              Caption = 'Host'
              Color = clBtnFace
              ParentColor = False
            end
            object Label77: TLabel
              Left = 208
              Top = 16
              Width = 26
              Height = 13
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object Label78: TLabel
              Left = 8
              Top = 56
              Width = 36
              Height = 13
              Caption = 'Usu'#225'rio'
              Color = clBtnFace
              ParentColor = False
            end
            object Label79: TLabel
              Left = 138
              Top = 56
              Width = 30
              Height = 13
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object edtProxyHost: TEdit
              Left = 8
              Top = 32
              Width = 193
              Height = 21
              TabOrder = 0
            end
            object edtProxyPorta: TEdit
              Left = 208
              Top = 32
              Width = 50
              Height = 21
              TabOrder = 1
            end
            object edtProxyUser: TEdit
              Left = 8
              Top = 72
              Width = 123
              Height = 21
              TabOrder = 2
            end
            object edtProxySenha: TEdit
              Left = 135
              Top = 72
              Width = 123
              Height = 21
              PasswordChar = '*'
              TabOrder = 3
            end
          end
        end
        object tsNFCeEmitente: TTabSheet
          Caption = 'Emitente'
          ImageIndex = 3
          object Label80: TLabel
            Left = 15
            Top = 18
            Width = 25
            Height = 13
            Caption = 'CNPJ'
            Color = clBtnFace
            ParentColor = False
          end
          object Label81: TLabel
            Left = 143
            Top = 18
            Width = 43
            Height = 13
            Caption = 'Insc.Est.'
            Color = clBtnFace
            ParentColor = False
          end
          object Label82: TLabel
            Left = 280
            Top = 18
            Width = 67
            Height = 13
            Caption = 'Raz'#195#163'o Social'
            Color = clBtnFace
            ParentColor = False
          end
          object Label83: TLabel
            Left = 15
            Top = 60
            Width = 41
            Height = 13
            Caption = 'Fantasia'
            Color = clBtnFace
            ParentColor = False
          end
          object Label84: TLabel
            Left = 15
            Top = 140
            Width = 55
            Height = 13
            Caption = 'Logradouro'
            Color = clBtnFace
            ParentColor = False
          end
          object Label85: TLabel
            Left = 215
            Top = 140
            Width = 43
            Height = 13
            Caption = 'N'#195#186'mero'
            Color = clBtnFace
            ParentColor = False
          end
          object Label86: TLabel
            Left = 15
            Top = 188
            Width = 65
            Height = 13
            Caption = 'Complemento'
            Color = clBtnFace
            ParentColor = False
          end
          object Label87: TLabel
            Left = 143
            Top = 188
            Width = 28
            Height = 13
            Caption = 'Bairro'
            Color = clBtnFace
            ParentColor = False
          end
          object Label88: TLabel
            Left = 16
            Top = 228
            Width = 68
            Height = 13
            Caption = 'C'#195#179'd. Cidade '
            Color = clBtnFace
            ParentColor = False
          end
          object Label89: TLabel
            Left = 84
            Top = 228
            Width = 33
            Height = 13
            Caption = 'Cidade'
            Color = clBtnFace
            ParentColor = False
          end
          object Label90: TLabel
            Left = 233
            Top = 228
            Width = 13
            Height = 13
            Caption = 'UF'
            Color = clBtnFace
            ParentColor = False
          end
          object Label91: TLabel
            Left = 143
            Top = 100
            Width = 19
            Height = 13
            Caption = 'CEP'
            Color = clBtnFace
            ParentColor = False
          end
          object Label92: TLabel
            Left = 15
            Top = 100
            Width = 24
            Height = 13
            Caption = 'Fone'
            Color = clBtnFace
            ParentColor = False
          end
          object edtEmitCNPJNFe: TEdit
            Left = 15
            Top = 34
            Width = 123
            Height = 21
            TabOrder = 0
          end
          object edtEmitIENFe: TEdit
            Left = 144
            Top = 34
            Width = 123
            Height = 21
            TabOrder = 1
          end
          object edtEmitRazao: TEdit
            Left = 280
            Top = 34
            Width = 312
            Height = 21
            TabOrder = 2
          end
          object edtEmitFantasia: TEdit
            Left = 15
            Top = 76
            Width = 577
            Height = 21
            TabOrder = 3
          end
          object edtEmitFone: TEdit
            Left = 15
            Top = 116
            Width = 125
            Height = 21
            TabOrder = 4
          end
          object edtEmitCEP: TEdit
            Left = 144
            Top = 116
            Width = 123
            Height = 21
            TabOrder = 5
          end
          object edtEmitLogradouro: TEdit
            Left = 15
            Top = 156
            Width = 196
            Height = 21
            TabOrder = 6
          end
          object edtEmitNumero: TEdit
            Left = 217
            Top = 156
            Width = 50
            Height = 21
            TabOrder = 7
          end
          object edtEmitComp: TEdit
            Left = 15
            Top = 204
            Width = 123
            Height = 21
            TabOrder = 8
          end
          object edtEmitBairro: TEdit
            Left = 144
            Top = 204
            Width = 123
            Height = 21
            TabOrder = 9
          end
          object edtEmitCodCidade: TEdit
            Left = 16
            Top = 244
            Width = 61
            Height = 21
            TabOrder = 10
          end
          object edtEmitCidade: TEdit
            Left = 84
            Top = 244
            Width = 142
            Height = 21
            TabOrder = 11
          end
          object edtEmitUF: TEdit
            Left = 233
            Top = 244
            Width = 35
            Height = 21
            TabOrder = 12
          end
        end
      end
    end
  end
  object pBotoes: TPanel
    Left = 0
    Top = 375
    Width = 794
    Height = 40
    Cursor = crHelp
    Hint = 'Sobre o ACBrMonitor ?'
    Align = alBottom
    TabOrder = 1
    TabStop = True
    DesignSize = (
      794
      40)
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 792
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
      Left = 693
      Top = 4
      Width = 96
      Height = 32
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Ativar'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = bAtivarClick
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
      object SalvarParmetros1: TMenuItem
        Caption = 'Salvar Par'#226'metros'
        OnClick = SalvarParmetros1Click
      end
      object lerParmetros1: TMenuItem
        Caption = 'Ler Par'#226'metros'
        OnClick = lerParmetros1Click
      end
      object N44: TMenuItem
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
        object SubModeloECF1: TMenuItem
          Caption = 'SubModeloECF'
          OnClick = SubModeloECF1Click
        end
        object N51: TMenuItem
          Caption = '-'
        end
        object NumECF1: TMenuItem
          Caption = 'Num ECF'
          OnClick = NumECF1Click
        end
        object NumLoja1: TMenuItem
          Caption = 'Num Loja'
          OnClick = NumLoja1Click
        end
        object NSrie1: TMenuItem
          Caption = 'Num S'#233'rie'
          OnClick = NSrie1Click
        end
        object NumSerieMFD: TMenuItem
          Caption = 'Num S'#233'rie MFD'
          OnClick = NumSerieMFDClick
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
        object IM1: TMenuItem
          Caption = 'IM'
          OnClick = IM1Click
        end
        object PAF1: TMenuItem
          Caption = 'PAF'
          OnClick = PAF1Click
        end
        object UsuarioAual1: TMenuItem
          Caption = 'Usuario Atual'
          OnClick = UsuarioAual1Click
        end
        object Cliche1: TMenuItem
          Caption = 'Cliche'
          OnClick = Cliche1Click
        end
        object DataHoraSwBasico1: TMenuItem
          Caption = 'Data Hora Sw.Basico'
          OnClick = DataHoraSwBasico1Click
        end
        object N52: TMenuItem
          Caption = '-'
        end
        object Decimais1: TMenuItem
          Caption = 'Decimais'
          OnClick = Decimais1Click
        end
        object Colunas1: TMenuItem
          Caption = 'Colunas'
          OnClick = Colunas1Click
        end
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object Flags1: TMenuItem
        Caption = 'Flags'
        object ipoltimoDocumento1: TMenuItem
          Caption = 'Tipo '#218'ltimo Documento'
          OnClick = ipoltimoDocumento1Click
        end
        object N50: TMenuItem
          Caption = '-'
        end
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
        object IdentificaConsumidorRodap1: TMenuItem
          Caption = 'Identifica Consumidor Rodap'#233' ?'
          OnClick = IdentificaConsumidorRodap1Click
        end
        object ParametroDescontoISSQN1: TMenuItem
          Caption = 'ParametroDescontoISSQN ?'
          OnClick = ParametroDescontoISSQN1Click
        end
        object MFAdicional1: TMenuItem
          Caption = 'MF Adicional ?'
          OnClick = MFAdicional1Click
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
        object NmeroReduesZrestantes1: TMenuItem
          Caption = 'N'#250'mero Redu'#231#245'es Z restantes'
          OnClick = NmeroReduesZrestantes1Click
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
        object NumGNFC1: TMenuItem
          Caption = 'Num GNFC'
          OnClick = NumGNFC1Click
        end
        object NumGRG1: TMenuItem
          Caption = 'Num GRG'
          OnClick = NumGRG1Click
        end
        object NumCDC1: TMenuItem
          Caption = 'Num CDC'
          OnClick = NumCDC1Click
        end
        object NumCCDC1: TMenuItem
          Caption = 'Num CCDC'
          OnClick = NumCCDC1Click
        end
        object NumCFD1: TMenuItem
          Caption = 'Num CFD'
          OnClick = NumCFD1Click
        end
        object NumNCN1: TMenuItem
          Caption = 'Num NCN'
          OnClick = NumNCN1Click
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
        object otalTroco1: TMenuItem
          Caption = 'Total Troco'
          OnClick = otalTroco1Click
        end
        object N21: TMenuItem
          Caption = '-'
        end
        object otalICMS1: TMenuItem
          Caption = 'Total ICMS'
          object otalCancelamentos1: TMenuItem
            Caption = 'Total Cancelamentos'
            OnClick = otalCancelamentos1Click
          end
          object otalDescontos1: TMenuItem
            Caption = 'Total Descontos'
            OnClick = otalDescontos1Click
          end
          object otalAcrescimos1: TMenuItem
            Caption = 'Total Acrescimos'
            OnClick = otalAcrescimos1Click
          end
          object N48: TMenuItem
            Caption = '-'
          end
          object otalSubstituicaoTributaria1: TMenuItem
            Caption = 'Total Substituicao Tributaria'
            OnClick = otalSubstituicaoTributaria1Click
          end
          object otalNaoTributado1: TMenuItem
            Caption = 'Total Nao Tributado'
            OnClick = otalNaoTributado1Click
          end
          object otalIseno1: TMenuItem
            Caption = 'Total Isen'#231#227'o'
            OnClick = otalIseno1Click
          end
        end
        object otaisISSQN1: TMenuItem
          Caption = 'Totais ISSQN'
          object otalCancelamentos2: TMenuItem
            Caption = 'Total Cancelamentos'
            OnClick = otalCancelamentos2Click
          end
          object otalDescontos2: TMenuItem
            Caption = 'Total Descontos'
            OnClick = otalDescontos2Click
          end
          object otalAcrescimos2: TMenuItem
            Caption = 'Total Acrescimos'
            OnClick = otalAcrescimos2Click
          end
          object N49: TMenuItem
            Caption = '-'
          end
          object otalSubstituicaoTributaria2: TMenuItem
            Caption = 'Total Substituicao Tributaria'
            OnClick = otalSubstituicaoTributaria2Click
          end
          object otalNaoTributado2: TMenuItem
            Caption = 'Total Nao Tributado'
            OnClick = otalNaoTributado2Click
          end
          object otalIseno2: TMenuItem
            Caption = 'Total Isen'#231#227'o'
            OnClick = otalIseno2Click
          end
        end
        object N40: TMenuItem
          Caption = '-'
        end
        object TotalNoFiscal1: TMenuItem
          Caption = 'Total N'#227'o Fiscal'
          object ValorTotal1: TMenuItem
            Caption = 'Valor Total'
            OnClick = ValorTotal1Click
          end
          object otalCancelamentos3: TMenuItem
            Caption = 'Total Cancelamentos'
            OnClick = otalCancelamentos3Click
          end
          object otalDescontos3: TMenuItem
            Caption = 'Total Descontos'
            OnClick = otalDescontos3Click
          end
          object otalAcrscimos1: TMenuItem
            Caption = 'Total Acr'#233'scimos'
            OnClick = otalAcrscimos1Click
          end
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
      object otalizadoresnofiscais1: TMenuItem
        Caption = 'Totalizadores N'#227'o Fiscais'
        object CarregaTotalizadoresNaoTributados1: TMenuItem
          Caption = 'Carrega Totalizadores Nao Tributados'
          OnClick = CarregaTotalizadoresNaoTributados1Click
        end
        object LerTotaisTotalizadoresNaoTributados1: TMenuItem
          Caption = 'Ler Totais Totalizadores Nao Tributados'
          OnClick = LerTotaisTotalizadoresNaoTributados1Click
        end
        object N47: TMenuItem
          Caption = '-'
        end
        object AchaTotalizadorNaoTributadoIndice1: TMenuItem
          Caption = 'Acha Totalizador Nao Tributado Indice'
          OnClick = AchaTotalizadorNaoTributadoIndice1Click
        end
        object estarLeituradeTotais1: TMenuItem
          Caption = 'Testar Leitura de Totais'
          OnClick = estarLeituradeTotais1Click
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
        object N53: TMenuItem
          Caption = '-'
        end
        object EstornaCCD1: TMenuItem
          Caption = 'EstornaCCD'
          OnClick = EstornaCCD1Click
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
      object LeituraXSerial1: TMenuItem
        Caption = 'Leitura X Serial'
        OnClick = LeituraXSerial1Click
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
      object AbreBilhetePassagem1: TMenuItem
        Caption = 'Abre Bilhete Passagem'
        OnClick = AbreBilhetePassagem1Click
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
      object estedeArredondamento1: TMenuItem
        Caption = 'Teste de Arredondamento'
        OnClick = estedeArredondamento1Click
      end
      object estedeRateio1: TMenuItem
        Caption = 'Teste de Rateio'
        OnClick = estedeRateio1Click
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
        object LeituraCMC71: TMenuItem
          Caption = 'Leitura CMC7'
          OnClick = LeituraCMC71Click
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
      object ProgramaUnidadeMedida1: TMenuItem
        Caption = 'Programa Unidade Medida'
        OnClick = ProgramaUnidadeMedida1Click
      end
      object ProgramaRelatrioGerencial1: TMenuItem
        Caption = 'Programa Relat'#243'rio Gerencial'
        OnClick = ProgramaRelatrioGerencial1Click
      end
      object N54: TMenuItem
        Caption = '-'
      end
      object IdentificaOperador1: TMenuItem
        Caption = 'Identifica Operador'
        OnClick = IdentificaOperador1Click
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
    object SAT1: TMenuItem
      Caption = 'SAT'
      object mConsultarStatusSAT: TMenuItem
        Caption = 'Consultar Status Operacional'
        OnClick = mConsultarStatusSATClick
      end
      object mConsultarSAT: TMenuItem
        Caption = 'Consultar SAT'
        OnClick = mConsultarSATClick
      end
      object mConsultarSessaoSAT: TMenuItem
        Caption = 'Consultar Sess'#227'o SAT'
        OnClick = mConsultarSessaoSATClick
      end
      object N45: TMenuItem
        Caption = '-'
      end
      object CarregarCFe1: TMenuItem
        Caption = 'Carregar CFe'
        OnClick = CarregarCFe1Click
      end
      object N46: TMenuItem
        Caption = '-'
      end
      object ImprimirExtratoVenda1: TMenuItem
        Caption = 'Imprimir Extrato Venda'
        OnClick = ImprimirExtratoVenda1Click
      end
      object ImprimirExtratoResumido1: TMenuItem
        Caption = 'Imprimir Extrato Resumido'
        OnClick = ImprimirExtratoResumido1Click
      end
      object ImprimirExtratoCancelamento1: TMenuItem
        Caption = 'Imprimir Extrato Cancelamento'
        OnClick = ImprimirExtratoCancelamento1Click
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
    ECFVirtual = ACBrECFVirtualNFCe1
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
    QuandoGravarArqINI = ACBrECFVirtualNaoFiscal1GravaArqINI
    QuandoLerArqINI = ACBrECFVirtualNaoFiscal1LeArqINI
    PosPrinter = ACBrPosPrinter1
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
    ExibeAvisoLegal = True
    Left = 592
    Top = 340
  end
  object ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    TipoDANFE = tiSemGeracao
    FonteLinhaItem.Charset = DEFAULT_CHARSET
    FonteLinhaItem.Color = clWindowText
    FonteLinhaItem.Height = -9
    FonteLinhaItem.Name = 'Lucida Console'
    FonteLinhaItem.Style = []
    Left = 718
    Top = 336
  end
  object ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 0.800000000000000000
    MargemSuperior = 0.800000000000000000
    MargemEsquerda = 0.600000000000000000
    MargemDireita = 0.510000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    ACBrNFe = ACBrNFe1
    TipoDANFE = tiSemGeracao
    Left = 757
    Top = 333
  end
  object ACBrECFVirtualNFCe1: TACBrECFVirtualNFCe
    ECF = ACBrECF1
    QuandoGravarArqINI = ACBrECFVirtualNFCe1QuandoGravarArqINI
    QuandoLerArqINI = ACBrECFVirtualNFCe1QuandoLerArqINI
    PosPrinter = ACBrPosPrinter1
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
      '</linha_simples>')
    MascaraItem = 
      'III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD QQQQQQQQ U' +
      'UxVVVVVVVVV AAAAAAA TTTTTTTTTTT'
    ACBrNFCe = ACBrNFe1
    QuandoVenderItem = ACBrECFVirtualNFCe1QuandoVenderItem
    QuandoEfetuarPagamento = ACBrECFVirtualNFCe1QuandoEfetuarPagamento
    QuandoFecharDocumento = ACBrECFVirtualNFCe1QuandoFecharDocumento
    Left = 676
    Top = 333
  end
  object ACBrNFe1: TACBrNFe
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.VersaoQRCode = veqr000
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Configuracoes.RespTec.IdCSRT = 0
    DANFE = ACBrNFeDANFeESCPOS1
    Left = 634
    Top = 336
  end
  object ACBrSAT1: TACBrSAT
    Extrato = ACBrSATExtratoFortes1
    Config.infCFe_versaoDadosEnt = 0.070000000000000010
    Config.ide_numeroCaixa = 0
    Config.ide_tpAmb = taHomologacao
    Config.emit_cRegTrib = RTSimplesNacional
    Config.emit_cRegTribISSQN = RTISSMicroempresaMunicipal
    Config.emit_indRatISSQN = irSim
    Config.EhUTF8 = False
    Config.PaginaDeCodigo = 0
    Config.XmlSignLib = xsNone
    ConfigArquivos.PrefixoArqCFe = 'AD'
    ConfigArquivos.PrefixoArqCFeCanc = 'ADC'
    Rede.tipoInter = infETHE
    Rede.seg = segNONE
    Rede.tipoLan = lanDHCP
    Rede.proxy = 0
    Rede.proxy_porta = 0
    Left = 596
    Top = 135
  end
  object ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
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
    Left = 644
    Top = 138
  end
  object ACBrSATExtratoFortes1: TACBrSATExtratoFortes
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
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
    MsgAppQRCode = 
      'Consulte o QR Code pelo aplicativo  "De olho na nota", dispon'#237've' +
      'l na AppStore (Apple) e PlayStore (Android)'
    ImprimeCodigoEan = True
    Left = 701
    Top = 138
  end
  object ACBrECFVirtualSAT1: TACBrECFVirtualSAT
    QuandoGravarArqINI = ACBrECFVirtualSAT1QuandoGravarArqINI
    QuandoLerArqINI = ACBrECFVirtualSAT1QuandoLerArqINI
    QuandoCancelarCupom = ACBrECFVirtualSAT1QuandoCancelarCupom
    PosPrinter = ACBrPosPrinter1
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
      '</linha_simples>')
    MascaraItem = 
      'III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD QQQQQQQQ U' +
      'UxVVVVVVVVV AAAAAAA TTTTTTTTTTT'
    ACBrSAT = ACBrSAT1
    QuandoAbrirDocumento = ACBrECFVirtualSAT1QuandoAbrirDocumento
    QuandoVenderItem = ACBrECFVirtualSAT1QuandoVenderItem
    QuandoEfetuarPagamento = ACBrECFVirtualSAT1QuandoEfetuarPagamento
    Left = 554
    Top = 138
  end
  object ACBrAAC1: TACBrAAC
    IdentPAF.Paf.TipoFuncionamento = tpfStandAlone
    IdentPAF.Paf.TipoDesenvolvimento = tpdComercializavel
    IdentPAF.Paf.IntegracaoPAFECF = tpiRetaguarda
    IdentPAF.Paf.RealizaPreVenda = False
    IdentPAF.Paf.RealizaDAVECF = False
    IdentPAF.Paf.RealizaDAVNaoFiscal = False
    IdentPAF.Paf.RealizaDAVOS = False
    IdentPAF.Paf.DAVConfAnexoII = False
    IdentPAF.Paf.RealizaLancamentoMesa = False
    IdentPAF.Paf.IndiceTecnicoProd = False
    IdentPAF.Paf.BarSimilarECFRestaurante = False
    IdentPAF.Paf.BarSimilarECFComum = False
    IdentPAF.Paf.BarSimilarBalanca = False
    IdentPAF.Paf.UsaImpressoraNaoFiscal = False
    IdentPAF.Paf.DAVDiscrFormula = False
    IdentPAF.Paf.ImpedeVendaVlrZero = False
    IdentPAF.Paf.AcumulaVolumeDiario = False
    IdentPAF.Paf.ArmazenaEncerranteIniFinal = False
    IdentPAF.Paf.EmiteContrEncerrAposREDZLEIX = False
    IdentPAF.Paf.IntegradoComBombas = False
    IdentPAF.Paf.CriaAbastDivergEncerrante = False
    IdentPAF.Paf.CadastroPlacaBomba = False
    IdentPAF.Paf.TransportePassageiro = False
    IdentPAF.Paf.TotalizaValoresLista = False
    IdentPAF.Paf.TransfPreVenda = False
    IdentPAF.Paf.TransfDAV = False
    IdentPAF.Paf.RecompoeGT = False
    IdentPAF.Paf.RecompoeNumSerie = False
    IdentPAF.Paf.EmitePED = False
    IdentPAF.Paf.CupomMania = False
    IdentPAF.Paf.MinasLegal = False
    IdentPAF.Paf.NotaLegalDF = False
    IdentPAF.Paf.ParaibaLegal = False
    IdentPAF.Paf.TrocoEmCartao = False
    OnAntesAbrirArquivo = ACBrAAC1AntesAbrirArquivo
    OnDepoisAbrirArquivo = ACBrAAC1DepoisAbrirArquivo
    OnAntesGravarArquivo = ACBrAAC1AntesGravarArquivo
    Left = 221
    Top = 183
  end
  object OpenDialog1: TOpenDialog
    Left = 223
    Top = 363
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    Modelo = ppEscBematech
    EspacoEntreLinhas = 20
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 7
    ControlePorta = True
    Left = 367
    Top = 327
  end
end
