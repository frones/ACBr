object frmDemo_ACBrCTe: TfrmDemo_ACBrCTe
  Left = 192
  Top = 125
  Caption = 'Demo ACBrCTe'
  ClientHeight = 529
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 529
    Align = alLeft
    TabOrder = 0
    object lblColaborador: TLabel
      Left = 18
      Top = 446
      Width = 261
      Height = 13
      Cursor = crHandPoint
      Caption = 'Veja a lista de Colaboradores do Projeto ACBr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = lblColaboradorClick
    end
    object lblPatrocinador: TLabel
      Left = 16
      Top = 470
      Width = 265
      Height = 13
      Cursor = crHandPoint
      Caption = 'Veja a lista de Patrocinadores do Projeto ACBr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = lblPatrocinadorClick
    end
    object lblDoar1: TLabel
      Left = 21
      Top = 494
      Width = 255
      Height = 13
      Cursor = crHandPoint
      Caption = 'Para se tornar Patrocinador do Projeto ACBr,'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = lblDoar1Click
    end
    object lblDoar2: TLabel
      Left = 117
      Top = 510
      Width = 63
      Height = 13
      Cursor = crHandPoint
      Caption = 'clique aqui'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 281
      Height = 377
      Caption = 'Configura'#231#245'es'
      TabOrder = 0
      object PageControl1: TPageControl
        Left = 2
        Top = 15
        Width = 277
        Height = 360
        ActivePage = TabSheet1
        Align = alClient
        MultiLine = True
        TabOrder = 0
        object TabSheet1: TTabSheet
          Caption = 'Certificado'
          object lSSLLib: TLabel
            Left = 35
            Top = 16
            Width = 34
            Height = 13
            Alignment = taRightJustify
            Caption = 'SSLLib'
            Color = clBtnFace
            ParentColor = False
          end
          object lCryptLib: TLabel
            Left = 31
            Top = 43
            Width = 38
            Height = 13
            Alignment = taRightJustify
            Caption = 'CryptLib'
            Color = clBtnFace
            ParentColor = False
          end
          object lHttpLib: TLabel
            Left = 35
            Top = 70
            Width = 34
            Height = 13
            Alignment = taRightJustify
            Caption = 'HttpLib'
            Color = clBtnFace
            ParentColor = False
          end
          object lXmlSign: TLabel
            Left = 12
            Top = 97
            Width = 57
            Height = 13
            Alignment = taRightJustify
            Caption = 'XMLSignLib'
            Color = clBtnFace
            ParentColor = False
          end
          object GroupBox2: TGroupBox
            Left = 0
            Top = 173
            Width = 265
            Height = 144
            Caption = 'Certificado'
            TabOrder = 0
            object Label1: TLabel
              Left = 8
              Top = 16
              Width = 41
              Height = 13
              Caption = 'Caminho'
            end
            object Label2: TLabel
              Left = 8
              Top = 56
              Width = 31
              Height = 13
              Caption = 'Senha'
            end
            object sbtnCaminhoCert: TSpeedButton
              Left = 235
              Top = 32
              Width = 23
              Height = 24
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000130B0000130B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
                333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
                0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
                07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
                07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
                0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
                33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
                B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
                3BB33773333773333773B333333B3333333B7333333733333337}
              NumGlyphs = 2
              OnClick = sbtnCaminhoCertClick
            end
            object Label25: TLabel
              Left = 8
              Top = 98
              Width = 79
              Height = 13
              Caption = 'N'#250'mero de S'#233'rie'
            end
            object sbtnGetCert: TSpeedButton
              Left = 235
              Top = 110
              Width = 23
              Height = 24
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000130B0000130B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
                333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
                0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
                07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
                07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
                0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
                33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
                B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
                3BB33773333773333773B333333B3333333B7333333733333337}
              NumGlyphs = 2
              OnClick = sbtnGetCertClick
            end
            object edtCaminho: TEdit
              Left = 8
              Top = 32
              Width = 225
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
              Width = 225
              Height = 21
              TabOrder = 2
            end
          end
          object cbSSLLib: TComboBox
            Left = 80
            Top = 8
            Width = 160
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbSSLLibChange
          end
          object cbCryptLib: TComboBox
            Left = 80
            Top = 35
            Width = 160
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnChange = cbCryptLibChange
          end
          object cbHttpLib: TComboBox
            Left = 80
            Top = 62
            Width = 160
            Height = 21
            Style = csDropDownList
            TabOrder = 3
            OnChange = cbHttpLibChange
          end
          object cbXmlSignLib: TComboBox
            Left = 80
            Top = 89
            Width = 160
            Height = 21
            Style = csDropDownList
            TabOrder = 4
            OnChange = cbXmlSignLibChange
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Geral'
          ImageIndex = 1
          object GroupBox3: TGroupBox
            Left = 0
            Top = 4
            Width = 265
            Height = 269
            Caption = 'Geral'
            TabOrder = 0
            object Label7: TLabel
              Left = 8
              Top = 176
              Width = 57
              Height = 13
              Caption = 'Logo Marca'
            end
            object sbtnLogoMarca: TSpeedButton
              Left = 235
              Top = 188
              Width = 23
              Height = 24
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000130B0000130B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
                333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
                0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
                07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
                07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
                0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
                33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
                B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
                3BB33773333773333773B333333B3333333B7333333733333337}
              NumGlyphs = 2
              OnClick = sbtnLogoMarcaClick
            end
            object sbtnPathSalvar: TSpeedButton
              Left = 235
              Top = 236
              Width = 23
              Height = 24
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000130B0000130B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
                333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
                0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
                07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
                07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
                0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
                33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
                B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
                3BB33773333773333773B333333B3333333B7333333733333337}
              NumGlyphs = 2
              OnClick = sbtnPathSalvarClick
            end
            object edtLogoMarca: TEdit
              Left = 8
              Top = 192
              Width = 228
              Height = 21
              TabOrder = 0
            end
            object edtPathLogs: TEdit
              Left = 8
              Top = 240
              Width = 228
              Height = 21
              TabOrder = 1
            end
            object ckSalvar: TCheckBox
              Left = 8
              Top = 224
              Width = 209
              Height = 15
              Caption = 'Salvar Arquivos de Envio e Resposta'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
            end
            object rgTipoDACTe: TRadioGroup
              Left = 8
              Top = 16
              Width = 249
              Height = 49
              Caption = 'DACTe'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                'Retrato'
                'Paisagem')
              TabOrder = 3
            end
            object rgFormaEmissao: TRadioGroup
              Left = 8
              Top = 72
              Width = 249
              Height = 97
              Caption = 'Forma de Emiss'#227'o'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                '1 = Normal'
                '4 = EPEC'
                '5 = Conting. FSDA'
                '7 = SVC-RS'
                '8 = SVC-SP')
              TabOrder = 4
            end
          end
          object rgModeloDF: TRadioGroup
            Left = 0
            Top = 279
            Width = 129
            Height = 49
            Caption = 'Modelo DF'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'CT-e'
              'CT-e OS')
            TabOrder = 1
          end
          object rgVersaoDF: TRadioGroup
            Left = 133
            Top = 279
            Width = 129
            Height = 50
            Caption = 'Vers'#227'o DF'
            Columns = 2
            ItemIndex = 1
            Items.Strings = (
              '2.00'
              '3.00')
            TabOrder = 2
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'WebService'
          ImageIndex = 2
          object GroupBox4: TGroupBox
            Left = 0
            Top = 4
            Width = 265
            Height = 186
            Caption = 'WebService'
            TabOrder = 0
            object Label6: TLabel
              Left = 8
              Top = 16
              Width = 231
              Height = 13
              Caption = 'Selecione UF da SEFAZ-Autorizadora (Emitente):'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object lSSLLib1: TLabel
              Left = 8
              Top = 160
              Width = 44
              Height = 13
              Alignment = taRightJustify
              Caption = 'SSLType'
              Color = clBtnFace
              ParentColor = False
            end
            object ckVisualizar: TCheckBox
              Left = 8
              Top = 118
              Width = 153
              Height = 17
              Caption = 'Visualizar Mensagem'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
            end
            object cbUF: TComboBox
              Left = 8
              Top = 32
              Width = 249
              Height = 24
              Style = csDropDownList
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ItemIndex = 24
              ParentFont = False
              TabOrder = 1
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
              TabOrder = 2
            end
            object cbSSLType: TComboBox
              Left = 64
              Top = 152
              Width = 160
              Height = 21
              Hint = 'Depende de configura'#231#227'o de  SSL.HttpLib'
              Style = csDropDownList
              TabOrder = 3
              OnChange = cbSSLTypeChange
            end
          end
          object gbProxy: TGroupBox
            Left = 0
            Top = 202
            Width = 265
            Height = 105
            Caption = 'Proxy'
            TabOrder = 1
            object Label8: TLabel
              Left = 8
              Top = 16
              Width = 22
              Height = 13
              Caption = 'Host'
            end
            object Label9: TLabel
              Left = 208
              Top = 16
              Width = 25
              Height = 13
              Caption = 'Porta'
            end
            object Label10: TLabel
              Left = 8
              Top = 56
              Width = 36
              Height = 13
              Caption = 'Usu'#225'rio'
            end
            object Label11: TLabel
              Left = 138
              Top = 56
              Width = 31
              Height = 13
              Caption = 'Senha'
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
        object TabSheet4: TTabSheet
          Caption = 'Emitente'
          ImageIndex = 3
          object Label12: TLabel
            Left = 8
            Top = 4
            Width = 27
            Height = 13
            Caption = 'CNPJ'
          end
          object Label13: TLabel
            Left = 136
            Top = 4
            Width = 41
            Height = 13
            Caption = 'Insc.Est.'
          end
          object Label14: TLabel
            Left = 8
            Top = 44
            Width = 63
            Height = 13
            Caption = 'Raz'#227'o Social'
          end
          object Label15: TLabel
            Left = 8
            Top = 84
            Width = 40
            Height = 13
            Caption = 'Fantasia'
          end
          object Label16: TLabel
            Left = 8
            Top = 164
            Width = 54
            Height = 13
            Caption = 'Logradouro'
          end
          object Label17: TLabel
            Left = 208
            Top = 164
            Width = 37
            Height = 13
            Caption = 'N'#250'mero'
          end
          object Label18: TLabel
            Left = 8
            Top = 204
            Width = 64
            Height = 13
            Caption = 'Complemento'
          end
          object Label19: TLabel
            Left = 136
            Top = 204
            Width = 27
            Height = 13
            Caption = 'Bairro'
          end
          object Label20: TLabel
            Left = 8
            Top = 244
            Width = 61
            Height = 13
            Caption = 'C'#243'd. Cidade '
          end
          object Label21: TLabel
            Left = 76
            Top = 244
            Width = 33
            Height = 13
            Caption = 'Cidade'
          end
          object Label22: TLabel
            Left = 225
            Top = 244
            Width = 14
            Height = 13
            Caption = 'UF'
          end
          object Label23: TLabel
            Left = 136
            Top = 124
            Width = 21
            Height = 13
            Caption = 'CEP'
          end
          object Label24: TLabel
            Left = 8
            Top = 124
            Width = 24
            Height = 13
            Caption = 'Fone'
          end
          object edtEmitCNPJ: TEdit
            Left = 8
            Top = 20
            Width = 123
            Height = 21
            TabOrder = 0
          end
          object edtEmitIE: TEdit
            Left = 137
            Top = 20
            Width = 123
            Height = 21
            TabOrder = 1
          end
          object edtEmitRazao: TEdit
            Left = 8
            Top = 60
            Width = 252
            Height = 21
            TabOrder = 2
          end
          object edtEmitFantasia: TEdit
            Left = 8
            Top = 100
            Width = 252
            Height = 21
            TabOrder = 3
          end
          object edtEmitFone: TEdit
            Left = 8
            Top = 140
            Width = 125
            Height = 21
            TabOrder = 4
          end
          object edtEmitCEP: TEdit
            Left = 137
            Top = 140
            Width = 123
            Height = 21
            TabOrder = 5
          end
          object edtEmitLogradouro: TEdit
            Left = 8
            Top = 180
            Width = 196
            Height = 21
            TabOrder = 6
          end
          object edtEmitNumero: TEdit
            Left = 210
            Top = 180
            Width = 50
            Height = 21
            TabOrder = 7
          end
          object edtEmitComp: TEdit
            Left = 8
            Top = 220
            Width = 123
            Height = 21
            TabOrder = 8
          end
          object edtEmitBairro: TEdit
            Left = 137
            Top = 220
            Width = 123
            Height = 21
            TabOrder = 9
          end
          object edtEmitCodCidade: TEdit
            Left = 8
            Top = 260
            Width = 61
            Height = 21
            TabOrder = 10
          end
          object edtEmitCidade: TEdit
            Left = 76
            Top = 260
            Width = 142
            Height = 21
            TabOrder = 11
          end
          object edtEmitUF: TEdit
            Left = 225
            Top = 260
            Width = 35
            Height = 21
            TabOrder = 12
          end
        end
        object TabSheet7: TTabSheet
          Caption = 'Email'
          ImageIndex = 4
          object GroupBox5: TGroupBox
            Left = 1
            Top = 4
            Width = 264
            Height = 324
            Caption = 'Email'
            TabOrder = 0
            object Label3: TLabel
              Left = 8
              Top = 16
              Width = 72
              Height = 13
              Caption = 'Servidor SMTP'
            end
            object Label4: TLabel
              Left = 206
              Top = 16
              Width = 25
              Height = 13
              Caption = 'Porta'
            end
            object Label5: TLabel
              Left = 8
              Top = 56
              Width = 36
              Height = 13
              Caption = 'Usu'#225'rio'
            end
            object Label26: TLabel
              Left = 137
              Top = 56
              Width = 31
              Height = 13
              Caption = 'Senha'
            end
            object Label27: TLabel
              Left = 8
              Top = 96
              Width = 121
              Height = 13
              Caption = 'Assunto do email enviado'
            end
            object Label28: TLabel
              Left = 8
              Top = 168
              Width = 95
              Height = 13
              Caption = 'Mensagem do Email'
            end
            object edtSmtpHost: TEdit
              Left = 8
              Top = 32
              Width = 193
              Height = 21
              TabOrder = 0
            end
            object edtSmtpPort: TEdit
              Left = 206
              Top = 32
              Width = 51
              Height = 21
              TabOrder = 1
            end
            object edtSmtpUser: TEdit
              Left = 8
              Top = 72
              Width = 120
              Height = 21
              TabOrder = 2
            end
            object edtSmtpPass: TEdit
              Left = 137
              Top = 72
              Width = 120
              Height = 21
              TabOrder = 3
            end
            object edtEmailAssunto: TEdit
              Left = 8
              Top = 112
              Width = 249
              Height = 21
              TabOrder = 4
            end
            object cbEmailSSL: TCheckBox
              Left = 10
              Top = 144
              Width = 167
              Height = 17
              Caption = 'SMTP exige conex'#227'o segura'
              TabOrder = 5
            end
            object mmEmailMsg: TMemo
              Left = 8
              Top = 184
              Width = 249
              Height = 130
              TabOrder = 6
            end
          end
        end
      end
    end
    object btnSalvarConfig: TBitBtn
      Left = 70
      Top = 392
      Width = 153
      Height = 25
      Caption = 'Salvar Configura'#231#245'es'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnSalvarConfigClick
    end
  end
  object Panel2: TPanel
    Left = 297
    Top = 0
    Width = 558
    Height = 529
    Align = alClient
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 556
      Height = 224
      Align = alTop
      TabOrder = 0
      object btnImprimir: TButton
        Left = 9
        Top = 95
        Width = 177
        Height = 25
        Caption = 'Imprimir DACTe'
        TabOrder = 3
        OnClick = btnImprimirClick
      end
      object btnConsultar: TButton
        Left = 192
        Top = 34
        Width = 177
        Height = 25
        Caption = 'Consultar carregando XML'
        TabOrder = 4
        OnClick = btnConsultarClick
      end
      object btnValidarXML: TButton
        Left = 9
        Top = 64
        Width = 177
        Height = 25
        Caption = 'Validar XML'
        TabOrder = 1
        OnClick = btnValidarXMLClick
      end
      object btnStatusServ: TButton
        Left = 9
        Top = 6
        Width = 177
        Height = 25
        Caption = 'Status Servi'#231'o'
        TabOrder = 0
        OnClick = btnStatusServClick
      end
      object btnCancCTe: TButton
        Left = 9
        Top = 126
        Width = 177
        Height = 25
        Caption = 'Cancelamento por Evento'
        TabOrder = 6
        OnClick = btnCancCTeClick
      end
      object btnCriarEnviar: TButton
        Left = 375
        Top = 6
        Width = 177
        Height = 25
        Caption = 'Criar e Enviar'
        TabOrder = 2
        OnClick = btnCriarEnviarClick
      end
      object btnInutilizar: TButton
        Left = 9
        Top = 192
        Width = 177
        Height = 25
        Caption = 'Inutilizar Numera'#231#227'o'
        TabOrder = 5
        OnClick = btnInutilizarClick
      end
      object btnGerarCTe: TButton
        Left = 192
        Top = 6
        Width = 177
        Height = 25
        Caption = 'Gerar CTe'
        TabOrder = 7
        OnClick = btnGerarCTeClick
      end
      object btnConsCad: TButton
        Left = 192
        Top = 64
        Width = 177
        Height = 25
        Caption = 'Consulta Cadastro'
        TabOrder = 8
        OnClick = btnConsCadClick
      end
      object btnGerarPDF: TButton
        Left = 192
        Top = 95
        Width = 177
        Height = 25
        Caption = 'Gerar PDF'
        TabOrder = 9
        OnClick = btnGerarPDFClick
      end
      object btnEnviarEmail: TButton
        Left = 375
        Top = 95
        Width = 177
        Height = 25
        Caption = 'Enviar CTe Email'
        TabOrder = 10
        OnClick = btnEnviarEmailClick
      end
      object btnConsultarRecibo: TButton
        Left = 9
        Top = 34
        Width = 177
        Height = 25
        Caption = 'Consultar Recibo Lote'
        TabOrder = 11
        OnClick = btnConsultarReciboClick
      end
      object btnEnvEPEC: TButton
        Left = 375
        Top = 64
        Width = 177
        Height = 25
        Caption = 'Enviar EPEC'
        TabOrder = 12
        OnClick = btnEnvEPECClick
      end
      object btnImprimirEvento: TButton
        Left = 192
        Top = 126
        Width = 177
        Height = 25
        Caption = 'Imprimir Evento'
        TabOrder = 13
        OnClick = btnImprimirEventoClick
      end
      object btnConsultarChave: TButton
        Left = 375
        Top = 34
        Width = 177
        Height = 25
        Caption = 'Consultar pela Chave'
        TabOrder = 14
        OnClick = btnConsultarChaveClick
      end
      object btnEnviarEventoEmail: TButton
        Left = 192
        Top = 160
        Width = 177
        Height = 25
        Caption = 'Enviar Evento Email'
        TabOrder = 15
        OnClick = btnEnviarEventoEmailClick
      end
      object btnGerarPDFEvento: TButton
        Left = 375
        Top = 126
        Width = 177
        Height = 25
        Caption = 'Gerar PDF Evento'
        TabOrder = 16
        OnClick = btnGerarPDFEventoClick
      end
      object btnImprimirInut: TButton
        Left = 192
        Top = 192
        Width = 177
        Height = 25
        Caption = 'Imprimir Inutiliza'#231#227'o'
        TabOrder = 17
        OnClick = btnImprimirInutClick
      end
      object btnGerarPDFInut: TButton
        Left = 375
        Top = 192
        Width = 177
        Height = 25
        Caption = 'Gerar PDF Inutiliza'#231#227'o'
        TabOrder = 18
        OnClick = btnGerarPDFInutClick
      end
    end
    object PageControl2: TPageControl
      Left = 1
      Top = 225
      Width = 556
      Height = 303
      ActivePage = TabSheet5
      Align = alClient
      TabOrder = 1
      object TabSheet5: TTabSheet
        Caption = 'Respostas'
        object MemoResp: TMemo
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          TabOrder = 0
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'XML Resposta'
        ImageIndex = 1
        object WBResposta: TWebBrowser
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C000000A33800006C1C00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
        object memoLog: TMemo
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'CTe'
        ImageIndex = 3
        object trvwCTe: TTreeView
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'Retorno Completo WS'
        ImageIndex = 4
        object memoRespWS: TMemo
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          TabOrder = 0
        end
      end
      object Dados: TTabSheet
        Caption = 'Dados'
        ImageIndex = 5
        object MemoDados: TMemo
          Left = 0
          Top = 0
          Width = 548
          Height = 275
          Align = alClient
          TabOrder = 0
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*-nfe.XML'
    Filter = 
      'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|To' +
      'dos os Arquivos (*.*)|*.*'
    Title = 'Selecione a NFe'
    Left = 578
    Top = 304
  end
  object ACBrCTe1: TACBrCTe
    MAIL = ACBrMail1
    OnStatusChange = ACBrCTe1StatusChange
    Configuracoes.Geral.SSLLib = libOpenSSL
    Configuracoes.Geral.SSLCryptLib = cryOpenSSL
    Configuracoes.Geral.SSLHttpLib = httpOpenSSL
    Configuracoes.Geral.SSLXmlSignLib = xsLibXml2
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.ModeloDF = moCTeOS
    Configuracoes.Arquivos.PathSchemas = 'D:\Componentes\DelphiXE3\ACBR\Exemplos\ACBrDFe\Schemas\CTe\'
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.Salvar = True
    Configuracoes.WebServices.QuebradeLinha = '|'
    Configuracoes.RespTec.IdCSRT = 0
    DACTE = ACBrCTeDACTeRL1
    Left = 330
    Top = 302
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 504
    Top = 300
  end
  object ACBrCTeDACTeRL1: TACBrCTeDACTeRL
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
    ACBrCTE = ACBrCTe1
    ImprimirHoraSaida = False
    TipoDACTE = tiSemGeracao
    TamanhoPapel = tpA4
    Cancelada = False
    ExibeResumoCanhoto = False
    EPECEnviado = False
    ImprimeDescPorc = False
    PrintDialog = True
    Left = 410
    Top = 302
  end
end
