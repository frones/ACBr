object FrPosPrinterTeste: TFrPosPrinterTeste
  Left = 630
  Top = 311
  Width = 885
  Height = 569
  ActiveControl = bTagFormtacaoCaracter
  Caption = 'ACBrPosPrinter - Teste'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 256
    Top = 0
    Width = 613
    Height = 530
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 356
      Width = 611
      Height = 173
      Align = alBottom
      TabOrder = 0
      object bTagFormtacaoCaracter: TButton
        Left = 24
        Top = 48
        Width = 183
        Height = 25
        Caption = 'Tags de Formata'#231#227'o de Caracter'
        TabOrder = 0
        OnClick = bTagFormtacaoCaracterClick
      end
      object bTagsAlinhamento: TButton
        Left = 24
        Top = 72
        Width = 183
        Height = 25
        Caption = 'Tags de Alinhamento'
        TabOrder = 1
        OnClick = bTagsAlinhamentoClick
      end
      object bTagQRCode: TButton
        Left = 216
        Top = 72
        Width = 183
        Height = 25
        Caption = 'Tags de QRCode'
        TabOrder = 5
        OnClick = bTagQRCodeClick
      end
      object bTagsCodBarras: TButton
        Left = 216
        Top = 48
        Width = 183
        Height = 25
        Caption = 'Tags de Codigo de Barras'
        TabOrder = 4
        OnClick = bTagsCodBarrasClick
      end
      object bTagsTesteInvalidas: TButton
        Left = 24
        Top = 96
        Width = 183
        Height = 25
        Caption = 'Teste de Tags Invalidas'
        TabOrder = 2
        OnClick = bTagsTesteInvalidasClick
      end
      object bTagsTestePagCodigo: TButton
        Left = 24
        Top = 120
        Width = 183
        Height = 25
        Caption = 'Teste de P'#225'gina de C'#243'digo'
        TabOrder = 3
        OnClick = bTagsTestePagCodigoClick
      end
      object bImpLinhaALinha: TButton
        Left = 216
        Top = 120
        Width = 183
        Height = 25
        Caption = 'Teste Impress'#227'o Linha a Linha'
        TabOrder = 7
        OnClick = bImpLinhaALinhaClick
      end
      object bImpTagsValidas: TButton
        Left = 408
        Top = 48
        Width = 183
        Height = 25
        Caption = 'Ajuda - Tags Validas'
        TabOrder = 8
        OnClick = bImpTagsValidasClick
      end
      object bLerStatus: TButton
        Left = 408
        Top = 120
        Width = 183
        Height = 25
        Caption = 'Leitura de Status'
        TabOrder = 11
        OnClick = bLerStatusClick
      end
      object bLerInfo: TButton
        Left = 408
        Top = 96
        Width = 183
        Height = 25
        Caption = 'Leitura de Informa'#231#245'es'
        TabOrder = 10
        OnClick = bLerInfoClick
      end
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 609
        Height = 39
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 12
        DesignSize = (
          609
          39)
        object bLimpar: TBitBtn
          Left = 420
          Top = 7
          Width = 83
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'Limpar'
          TabOrder = 0
          OnClick = bLimparClick
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00101073FF000029FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00000431FF10106BFFFF00FF00FF00FF00FF00FF000000
            8CFF3134F7FF0808CEFF000031FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000039FF080CCEFF3130EFFF00007BFFFF00FF00000063FF0808
            E7FF4A49F7FF4A4DF7FF0000C6FF000039FFFF00FF00FF00FF00FF00FF00FF00
            FF0000004AFF0004CEFF5A59EFFF5A59F7FF0808DEFF00004AFF000484FF2120
            FFFF3134FFFF5A59FFFF4A49EFFF0004C6FF000042FFFF00FF00FF00FF000000
            52FF0808CEFF5A59EFFF7371FFFF5255FFFF3134FFFF00046BFF000473FF181C
            FFFF2928FFFF3938FFFF5255FFFF4245EFFF0004C6FF00004AFF00005AFF0808
            CEFF5255EFFF6B69FFFF5251FFFF4241FFFF3130FFFF080C52FFFF00FF000808
            7BFF181CFFFF2928FFFF3134FFFF4A49FFFF3134EFFF0000BDFF0004C6FF4245
            EFFF5A59FFFF4245FFFF393CFFFF292CFFFF080C63FFFF00FF00FF00FF00FF00
            FF0008086BFF1818FFFF292CFFFF393CFFFF4A4DFFFF3130EFFF3134EFFF4A49
            FFFF3938FFFF3130FFFF2124F7FF08084AFFFF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000452FF393CEFFF6361FFFF6361FFFF6361FFFF6361FFFF6361
            FFFF5A59FFFF2928DEFF000439FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0000048CFF393CF7FF6361FFFF6361FFFF6365FFFF6365
            FFFF3134EFFF000463FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000005AFF0000A5FF3130F7FF6B69FFFF6B69FFFF6B69FFFF6B69
            FFFF2120E7FF00009CFF000042FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000052FF0000A5FF3134F7FF7B79FFFF7B79FFFF7B79FFFF7B79FFFF7B79
            FFFF7375FFFF2124E7FF000094FF000039FFFF00FF00FF00FF00FF00FF000000
            52FF00009CFF4241F7FF8C8AFFFF8C8AFFFF8C8AFFFF3130C6FF4A49DEFF8C8E
            FFFF8C8AFFFF8486FFFF2928E7FF00008CFF000039FFFF00FF00000018FF0000
            9CFF4A49F7FF9C9AFFFF9C9AFFFF9C9AFFFF3134BDFFFF00FF00000021FF5255
            E7FF9C9EFFFF9C9AFFFF9496FFFF292CE7FF00007BFFFF00FF00000029FF5255
            FFFFADAEFFFFADAAFFFFADAAFFFF393CBDFFFF00FF00FF00FF00FF00FF000000
            29FF5A5DE7FFADAEFFFFADAAFFFFADAAFFFF3130DEFFFF00FF00FF00FF004245
            BDFFBDBAFFFFBDBAFFFF393CBDFFFF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00000029FF6B69E7FFCECBFFFFA5A6FFFF21248CFFFF00FF00FF00FF00FF00
            FF00393CBDFF4241C6FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000029FF5A59E7FF181C7BFFFF00FF00FF00FF00}
        end
        object bImprimir: TBitBtn
          Left = 511
          Top = 7
          Width = 92
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'Imprimir'
          TabOrder = 1
          OnClick = bImprimirClick
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000064000000640000000000000000000000000000000000
            00000000000000000000000000007F7F7FFF4F4F4FFF5F5F5FFFBFBFBFFF0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000007F7F7FFF5F5F5FFFBFBFBFFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F
            5FFFBFBFBFFF0000000000000000000000000000000000000000000000007F7F
            7FFF5F5F5FFFBFBFBFFF9F9F9FFF8F8F8FFF9F9F9FFF5F5F5FFF3F3F3FFF5F5F
            5FFF5F5F5FFF5F5F5FFFBFBFBFFF0000000000000000000000009F9F9FFFBFBF
            BFFF9F9F9FFF9F9F9FFFBFBFBFFF9F9F9FFF9F9F9FFF7F7F7FFF7F7F7FFF5F5F
            5FFF3F3F3FFF5F5F5FFF5F5F5FFF5F5F5FFF00000000000000009F9F9FFF8F8F
            8FFFBFBFBFFFBFBFBFFFBFBFBFFF9F9F9FFF9F9F9FFF7F7F7FFF7F7F7FFF7F7F
            7FFF7F7F7FFF5F5F5FFF3F3F3FFF1F1F1FFF0000000000000000BFBFBFFF9F9F
            9FFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFF8F8F8FFF7F7F7FFF7F7F
            7FFF7F7F7FFF7F7F7FFF7F7F7FFF5F5F5FFF3F3F3FFF00000000BFBFBFFF9F9F
            9FFFBFBFBFFFDFDFDFFFDFDFDFFFBFBFBFFFAFAFAFFF9F9F9FFF9F9F9FFF8F8F
            8FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF4F4F4FFF5F5F5FFFBFBFBFFFAFAF
            AFFFCFCFCFFF8FCF8FFF5F9F9FFF7F7FBFFF9F9F9FFFAFAFAFFFAFAFAFFF6F6F
            6FFF9F9F9FFF8F8F8FFF7F7F7FFF7F7F7FFF4F4F4FFFBFBFBFFFBFBFBFFFAFAF
            AFFFBFBFBFFF9F9F9FFF8F8F8FFF9F9F9FFFBFBFBFFF5F5F5FFF7F7F7FFFEFEF
            EFFF4F4F4FFF9F9F9FFF9F9F9FFF8F8F8FFF7F7F7FFF00000000000000000000
            0000BFBFBFFFAFAFAFFFBFBFBFFF8F8F8FFF5F5F5FFFFFBFBFFFFF7F7FFFFFBF
            BFFF6F6F6FFF8F8F8FFFAFAFAFFFDFDFDFFF0000000000000000000000000000
            00000000000000000000BFBFBFFFAFAFAFFFAFAFAFFFFFFFFFFFFF7F7FFFFF7F
            7FFFEFEFEFFF6F6F6FFF00000000000000000000000000000000000000000000
            000000000000000000000000000000000000BFBFBFFFBFBFBFFFFFBFBFFFFF7F
            7FFFFFBFBFFFEFEFEFFF6F6F6FFF000000000000000000000000000000000000
            00000000000000000000000000000000000000000000DFDFDFFFDFDFDFFFFFBF
            BFFFFF7F7FFFFFBFBFFFEFEFEFFF3F3F3FFF0000000000000000000000000000
            0000000000000000000000000000000000000000000000000000DFDFDFFFDFDF
            DFFFFFFFFFFFBFBFBFFFBFBFBFFF000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000DFDF
            DFFFBFBFBFFF0000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000}
        end
        object Button1: TButton
          Left = 64
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Senha'
          TabOrder = 2
          OnClick = Button1Click
        end
      end
      object bTagLogo: TButton
        Left = 216
        Top = 96
        Width = 183
        Height = 25
        Caption = 'Tags de Logotipo'
        TabOrder = 6
        OnClick = bTagLogoClick
      end
      object bTagGaveta: TButton
        Left = 408
        Top = 72
        Width = 183
        Height = 25
        Caption = 'Tag de Gaveta'
        TabOrder = 9
        OnClick = bTagGavetaClick
      end
      object bTagsTestePageMode: TButton
        Left = 24
        Top = 144
        Width = 183
        Height = 25
        Caption = 'Teste de Page Mode'
        TabOrder = 13
        OnClick = bTagsTestePageModeClick
      end
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 611
      Height = 355
      ActivePage = tsImprimir
      Align = alClient
      TabOrder = 1
      object tsImprimir: TTabSheet
        Caption = 'Texto a Imprimir'
        object mImp: TMemo
          Left = 0
          Top = 0
          Width = 603
          Height = 327
          Align = alClient
          TabOrder = 0
          WordWrap = False
        end
      end
      object tsLog: TTabSheet
        Caption = 'Eventos Log'
        object mLog: TMemo
          Left = 0
          Top = 0
          Width = 603
          Height = 327
          Align = alClient
          TabOrder = 0
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 256
    Height = 530
    Align = alLeft
    TabOrder = 1
    object gbConfiguracao: TGroupBox
      Left = 1
      Top = 1
      Width = 254
      Height = 247
      Align = alTop
      Caption = 'Configura'#231#227'o'
      TabOrder = 0
      DesignSize = (
        254
        247)
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 35
        Height = 13
        Caption = 'Modelo'
        Color = clBtnFace
        ParentColor = False
      end
      object Label4: TLabel
        Left = 16
        Top = 56
        Width = 25
        Height = 13
        Caption = 'Porta'
        Color = clBtnFace
        ParentColor = False
      end
      object Label2: TLabel
        Left = 15
        Top = 104
        Width = 38
        Height = 13
        Caption = 'Colunas'
        Color = clBtnFace
        ParentColor = False
      end
      object Label3: TLabel
        Left = 72
        Top = 104
        Width = 41
        Height = 13
        Caption = 'Espa'#231'os'
        Color = clBtnFace
        ParentColor = False
      end
      object Label5: TLabel
        Left = 127
        Top = 200
        Width = 54
        Height = 13
        Caption = 'Pag.codigo'
        Color = clBtnFace
        ParentColor = False
      end
      object btSerial: TSpeedButton
        Left = 220
        Top = 72
        Width = 23
        Height = 22
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
      object Label6: TLabel
        Left = 131
        Top = 104
        Width = 28
        Height = 13
        Caption = 'Buffer'
        Color = clBtnFace
        ParentColor = False
      end
      object Label9: TLabel
        Left = 132
        Top = 152
        Width = 40
        Height = 13
        Alignment = taRightJustify
        Caption = 'Arq.Log:'
        Color = clBtnFace
        ParentColor = False
      end
      object SbArqLog: TSpeedButton
        Left = 220
        Top = 168
        Width = 24
        Height = 22
        Hint = 'Abre Arquivo de Log'
        Caption = '...'
        OnClick = SbArqLogClick
      end
      object Label7: TLabel
        Left = 192
        Top = 104
        Width = 58
        Height = 13
        Caption = 'Linhas Pular'
        Color = clBtnFace
        ParentColor = False
      end
      object cbTraduzirTags: TCheckBox
        Left = 15
        Top = 197
        Width = 88
        Height = 19
        Hint = 'Traduz as Tags de Formata'#231#227'o e Cod.Barras'
        Caption = 'TraduzirTags'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbTraduzirTagsClick
      end
      object cbIgnorarTags: TCheckBox
        Left = 15
        Top = 216
        Width = 83
        Height = 19
        Hint = 'N'#227'o processa as Tags, imprime-as como texto normal'
        Caption = 'IgnorarTags'
        TabOrder = 1
        OnClick = cbIgnorarTagsClick
      end
      object cbxModelo: TComboBox
        Left = 16
        Top = 32
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = cbxModeloChange
      end
      object cbxPorta: TComboBox
        Left = 16
        Top = 72
        Width = 192
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        OnChange = cbxPortaChange
      end
      object seColunas: TSpinEdit
        Left = 15
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 999
        MinValue = 1
        TabOrder = 4
        Value = 48
        OnChange = seColunasChange
      end
      object cbControlePorta: TCheckBox
        Left = 15
        Top = 158
        Width = 97
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Controle Porta'
        TabOrder = 5
        OnClick = cbControlePortaClick
      end
      object seEspLinhas: TSpinEdit
        Left = 72
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnChange = seEspLinhasChange
      end
      object cbxPagCodigo: TComboBox
        Left = 127
        Top = 216
        Width = 117
        Height = 21
        Hint = 'Pagina de c'#243'digo usada pela Impressora POS'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
        OnChange = cbxPagCodigoChange
      end
      object seLinhasBuffer: TSpinEdit
        Left = 131
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 8
        Value = 0
        OnChange = seLinhasBufferChange
      end
      object edLog: TEdit
        Left = 129
        Top = 168
        Width = 84
        Height = 21
        Cursor = crIBeam
        Hint = 'Arquivo de Log para gravar os comandos enviados e recebidos'
        TabOrder = 9
        Text = 'PosPrinter.log'
      end
      object bAtivar: TBitBtn
        Left = 175
        Top = 16
        Width = 67
        Height = 43
        Anchors = [akTop, akRight]
        Caption = 'Ativar'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ModalResult = 1
        ParentFont = False
        TabOrder = 10
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
        Layout = blGlyphTop
      end
      object seLinhasPular: TSpinEdit
        Left = 192
        Top = 120
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 11
        Value = 0
        OnChange = seLinhasPularChange
      end
      object cbCortarPapel: TCheckBox
        Left = 15
        Top = 178
        Width = 85
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Cortar Papel'
        Checked = True
        State = cbChecked
        TabOrder = 12
        OnClick = cbCortarPapelClick
      end
    end
    object gbCodBarrasConfig: TGroupBox
      Left = 1
      Top = 248
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'Cod.Barras'
      TabOrder = 1
      object Label8: TLabel
        Left = 16
        Top = 16
        Width = 36
        Height = 13
        Caption = 'Largura'
        Color = clBtnFace
        ParentColor = False
      end
      object Label10: TLabel
        Left = 72
        Top = 16
        Width = 27
        Height = 13
        Caption = 'Altura'
        Color = clBtnFace
        ParentColor = False
      end
      object seBarrasLargura: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 5
        MinValue = 0
        TabOrder = 0
        Value = 1
        OnChange = seBarrasLarguraChange
      end
      object seBarrasAltura: TSpinEdit
        Left = 72
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = seBarrasAlturaChange
      end
      object cbHRI: TCheckBox
        Left = 136
        Top = 32
        Width = 94
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Exibe Numero'
        TabOrder = 2
        OnClick = cbHRIClick
      end
    end
    object gbCodBarrasConfig1: TGroupBox
      Left = 1
      Top = 312
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'QRCode'
      TabOrder = 2
      object Label11: TLabel
        Left = 16
        Top = 16
        Width = 21
        Height = 13
        Caption = 'Tipo'
        Color = clBtnFace
        ParentColor = False
      end
      object Label12: TLabel
        Left = 72
        Top = 16
        Width = 63
        Height = 13
        Caption = 'Largura Mod.'
        Color = clBtnFace
        ParentColor = False
      end
      object Label13: TLabel
        Left = 167
        Top = 16
        Width = 48
        Height = 13
        Caption = 'ErrorLevel'
        Color = clBtnFace
        ParentColor = False
      end
      object seQRCodeTipo: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 2
        MinValue = 1
        TabOrder = 0
        Value = 2
        OnChange = seQRCodeTipoChange
      end
      object seQRCodeLarguraModulo: TSpinEdit
        Left = 88
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 1
        Value = 4
        OnChange = seQRCodeLarguraModuloChange
      end
      object seQRCodeErrorLevel: TSpinEdit
        Left = 168
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 3
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = seQRCodeErrorLevelChange
      end
    end
    object gbCodBarrasConfig2: TGroupBox
      Left = 1
      Top = 376
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'LogoTipo'
      TabOrder = 3
      object Label14: TLabel
        Left = 16
        Top = 16
        Width = 20
        Height = 13
        Caption = 'KC1'
        Color = clBtnFace
        ParentColor = False
      end
      object Label15: TLabel
        Left = 72
        Top = 16
        Width = 20
        Height = 13
        Caption = 'KC2'
        Color = clBtnFace
        ParentColor = False
      end
      object Label16: TLabel
        Left = 128
        Top = 16
        Width = 31
        Height = 13
        Caption = 'FatorX'
        Color = clBtnFace
        ParentColor = False
      end
      object Label17: TLabel
        Left = 185
        Top = 16
        Width = 31
        Height = 13
        Caption = 'FatorY'
        Color = clBtnFace
        ParentColor = False
      end
      object seLogoKC1: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 126
        MinValue = 0
        TabOrder = 0
        Value = 32
        OnChange = seLogoKC1Change
      end
      object seLogoKC2: TSpinEdit
        Left = 72
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 126
        MinValue = 0
        TabOrder = 1
        Value = 32
        OnChange = seLogoKC2Change
      end
      object seLogoFatorX: TSpinEdit
        Left = 129
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 4
        MinValue = 1
        TabOrder = 2
        Value = 4
        OnChange = seLogoFatorXChange
      end
      object seLogoFatorY: TSpinEdit
        Left = 184
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 4
        MinValue = 1
        TabOrder = 3
        Value = 4
        OnChange = seLogoFatorYChange
      end
    end
    object gbGavetaConfig: TGroupBox
      Left = 1
      Top = 440
      Width = 254
      Height = 64
      Align = alTop
      Caption = 'Gaveta'
      TabOrder = 4
      object Label18: TLabel
        Left = 63
        Top = 16
        Width = 16
        Height = 13
        Caption = 'ON'
        Color = clBtnFace
        ParentColor = False
      end
      object Label19: TLabel
        Left = 112
        Top = 16
        Width = 20
        Height = 13
        Caption = 'OFF'
        Color = clBtnFace
        ParentColor = False
      end
      object Label20: TLabel
        Left = 16
        Top = 16
        Width = 35
        Height = 13
        Caption = 'Gaveta'
        Color = clBtnFace
        ParentColor = False
      end
      object seGavetaTempoON: TSpinEdit
        Left = 63
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 250
        MinValue = 50
        TabOrder = 1
        Value = 50
        OnChange = seGavetaTempoONChange
      end
      object cbGavetaSinalInvertido: TCheckBox
        Left = 167
        Top = 32
        Width = 67
        Height = 19
        Hint = 
          'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
          'ta Serial ap'#243's o envio'
        Caption = 'Invertido'
        TabOrder = 3
        OnClick = cbGavetaSinalInvertidoClick
      end
      object seGavetaTempoOFF: TSpinEdit
        Left = 112
        Top = 31
        Width = 41
        Height = 22
        MaxValue = 250
        MinValue = 50
        TabOrder = 2
        Value = 200
        OnChange = seGavetaTempoOFFChange
      end
      object seGavetaNum: TSpinEdit
        Left = 16
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 2
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
    end
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    Modelo = ppEscPosEpson
    Device.NomeDocumento = 'ACBrPorPrinterDemo'
    Device.HookAtivar = ACBrDeviceHookAtivar
    Device.HookDesativar = ACBrDeviceHookDesativar
    Device.HookEnviaString = ACBrPosPrinter1ACBrDeviceHookEnviaString
    Device.HookLeString = ACBrPosPrinter1ACBrDeviceHookLeString
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 0
    CortaPapel = False
    VerificarImpressora = True
    OnGravarLog = ACBrPosPrinter1GravarLog
    Left = 280
    Top = 256
  end
end
