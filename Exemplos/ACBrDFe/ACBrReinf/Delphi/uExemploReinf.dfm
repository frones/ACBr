object Form2: TForm2
  Left = 193
  Top = 126
  Caption = 'Exemplo Reinf'
  ClientHeight = 608
  ClientWidth = 888
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 608
    Align = alLeft
    TabOrder = 0
    object lblColaborador: TLabel
      Left = 18
      Top = 540
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
      OnMouseEnter = lblColaboradorMouseEnter
      OnMouseLeave = lblColaboradorMouseLeave
    end
    object lblPatrocinador: TLabel
      Left = 16
      Top = 555
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
      OnMouseEnter = lblColaboradorMouseEnter
      OnMouseLeave = lblColaboradorMouseLeave
    end
    object lblDoar1: TLabel
      Left = 21
      Top = 570
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
      OnMouseEnter = lblColaboradorMouseEnter
      OnMouseLeave = lblColaboradorMouseLeave
    end
    object lblDoar2: TLabel
      Left = 117
      Top = 583
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
      OnClick = lblDoar1Click
      OnMouseEnter = lblColaboradorMouseEnter
      OnMouseLeave = lblColaboradorMouseLeave
    end
    object btnSalvarConfig: TBitBtn
      Left = 78
      Top = 515
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
      TabOrder = 0
      OnClick = btnSalvarConfigClick
    end
    object PageControl2: TPageControl
      Left = 8
      Top = 9
      Width = 283
      Height = 504
      ActivePage = TabSheet5
      TabOrder = 1
      object TabSheet5: TTabSheet
        Caption = 'Configura'#231#245'es'
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 275
          Height = 476
          ActivePage = TabSheet7
          Align = alClient
          MultiLine = True
          TabOrder = 0
          object TabSheet6: TTabSheet
            Caption = 'Certificado'
            object lSSLLib: TLabel
              Left = 39
              Top = 16
              Width = 30
              Height = 13
              Alignment = taRightJustify
              Caption = 'SSLLib'
              Color = clBtnFace
              ParentColor = False
            end
            object lCryptLib: TLabel
              Left = 29
              Top = 43
              Width = 40
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
              Left = 17
              Top = 97
              Width = 52
              Height = 13
              Alignment = taRightJustify
              Caption = 'XMLSignLib'
              Color = clBtnFace
              ParentColor = False
            end
            object gbCertificado: TGroupBox
              Left = 2
              Top = 118
              Width = 263
              Height = 144
              Caption = 'Certificado'
              TabOrder = 0
              object Label4: TLabel
                Left = 8
                Top = 16
                Width = 41
                Height = 13
                Caption = 'Caminho'
              end
              object Label6: TLabel
                Left = 8
                Top = 56
                Width = 30
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
                Top = 96
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
              object SpeedButton1: TSpeedButton
                Left = 206
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
                OnClick = SpeedButton1Click
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
                Width = 193
                Height = 21
                TabOrder = 2
              end
            end
            object Button2: TButton
              Left = 8
              Top = 266
              Width = 99
              Height = 25
              Caption = 'Data de Validade'
              TabOrder = 1
              OnClick = Button2Click
            end
            object Button3: TButton
              Left = 112
              Top = 266
              Width = 73
              Height = 25
              Caption = 'Num.S'#233'rie'
              TabOrder = 2
              OnClick = Button3Click
            end
            object Button4: TButton
              Left = 8
              Top = 298
              Width = 99
              Height = 25
              Caption = 'Subject Name'
              TabOrder = 3
              OnClick = Button4Click
            end
            object Button5: TButton
              Left = 112
              Top = 298
              Width = 73
              Height = 25
              Caption = 'CNPJ'
              TabOrder = 4
              OnClick = Button5Click
            end
            object Button10: TButton
              Left = 188
              Top = 298
              Width = 76
              Height = 25
              Caption = 'Issuer Name'
              TabOrder = 5
              OnClick = Button10Click
            end
            object GroupBox2: TGroupBox
              Left = 2
              Top = 328
              Width = 263
              Height = 69
              Caption = 'Calculo de Hash e assinatura'
              TabOrder = 6
              object edHash: TEdit
                Left = 3
                Top = 14
                Width = 249
                Height = 21
                TabOrder = 0
              end
              object Button6: TButton
                Left = 8
                Top = 41
                Width = 99
                Height = 25
                Caption = 'SHA256+RSA'
                TabOrder = 1
                OnClick = Button6Click
              end
              object cbAssinar: TCheckBox
                Left = 144
                Top = 41
                Width = 54
                Height = 19
                Caption = 'Assinar'
                Checked = True
                State = cbChecked
                TabOrder = 2
              end
            end
            object Button7: TButton
              Left = 8
              Top = 403
              Width = 128
              Height = 25
              Caption = 'HTTPS sem Certificado'
              TabOrder = 7
              OnClick = Button7Click
            end
            object Button9: TButton
              Left = 144
              Top = 403
              Width = 115
              Height = 25
              Caption = 'Leitura de X509'
              TabOrder = 8
              OnClick = Button9Click
            end
            object cbSSLLib: TComboBox
              Left = 80
              Top = 8
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 9
              OnChange = cbSSLLibChange
            end
            object cbCryptLib: TComboBox
              Left = 80
              Top = 35
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 10
              OnChange = cbCryptLibChange
            end
            object cbHttpLib: TComboBox
              Left = 80
              Top = 62
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 11
              OnChange = cbHttpLibChange
            end
            object cbXmlSignLib: TComboBox
              Left = 80
              Top = 89
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 12
              OnChange = cbXmlSignLibChange
            end
          end
          object TabSheet7: TTabSheet
            Caption = 'Geral'
            ImageIndex = 1
            object GroupBox3: TGroupBox
              Left = 0
              Top = 4
              Width = 265
              Height = 381
              Caption = 'Geral'
              TabOrder = 0
              object sbtnPathSalvar: TSpeedButton
                Left = 235
                Top = 311
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
              object Label29: TLabel
                Left = 8
                Top = 88
                Width = 86
                Height = 13
                Caption = 'Forma de Emiss'#227'o'
              end
              object Label31: TLabel
                Left = 8
                Top = 50
                Width = 72
                Height = 13
                Caption = 'Formato Alerta'
              end
              object Label32: TLabel
                Left = 8
                Top = 131
                Width = 119
                Height = 13
                Caption = 'Vers'#227'o Documento Fiscal'
              end
              object Label42: TLabel
                Left = 8
                Top = 336
                Width = 198
                Height = 13
                Caption = 'Diret'#243'rios com os arquivos XSD(Schemas)'
              end
              object spPathSchemas: TSpeedButton
                Left = 235
                Top = 351
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
                OnClick = spPathSchemasClick
              end
              object edtPathLogs: TEdit
                Left = 8
                Top = 315
                Width = 228
                Height = 21
                TabOrder = 0
              end
              object ckSalvar: TCheckBox
                Left = 8
                Top = 299
                Width = 209
                Height = 15
                Caption = 'Salvar Arquivos de Envio e Resposta'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
              end
              object cbFormaEmissao: TComboBox
                Left = 8
                Top = 104
                Width = 248
                Height = 21
                TabOrder = 2
              end
              object cbxAtualizarXML: TCheckBox
                Left = 8
                Top = 16
                Width = 97
                Height = 17
                Caption = 'Atualizar XML'
                TabOrder = 3
              end
              object cbxExibirErroSchema: TCheckBox
                Left = 8
                Top = 32
                Width = 129
                Height = 17
                Caption = 'Exibir Erro Schema'
                TabOrder = 4
              end
              object edtFormatoAlerta: TEdit
                Left = 8
                Top = 66
                Width = 248
                Height = 21
                TabOrder = 5
              end
              object cbxRetirarAcentos: TCheckBox
                Left = 8
                Top = 281
                Width = 193
                Height = 17
                Caption = 'Retirar Acentos dos XMLs enviados'
                TabOrder = 6
              end
              object cbVersaoDF: TComboBox
                Left = 8
                Top = 147
                Width = 248
                Height = 21
                TabOrder = 7
              end
              object edtPathSchemas: TEdit
                Left = 8
                Top = 352
                Width = 228
                Height = 21
                TabOrder = 8
              end
            end
          end
          object TabSheet8: TTabSheet
            Caption = 'WebService'
            ImageIndex = 2
            object GroupBox5: TGroupBox
              Left = 0
              Top = 4
              Width = 265
              Height = 190
              Caption = 'WebService'
              TabOrder = 0
              object Label7: TLabel
                Left = 8
                Top = 16
                Width = 121
                Height = 13
                Caption = 'Selecione UF de Destino:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
              end
              object lTimeOut: TLabel
                Left = 167
                Top = 116
                Width = 40
                Height = 13
                Caption = 'TimeOut'
                Color = clBtnFace
                ParentColor = False
              end
              object lSSLLib1: TLabel
                Left = 19
                Top = 164
                Width = 41
                Height = 13
                Alignment = taRightJustify
                Caption = 'SSLType'
                Color = clBtnFace
                ParentColor = False
              end
              object cbxVisualizar: TCheckBox
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
                ItemIndex = 2
                Items.Strings = (
                  'Produ'#231#227'o'
                  'Restrita Reais'
                  'Restrita Fict'#237'cios')
                TabOrder = 2
              end
              object cbxSalvarSOAP: TCheckBox
                Left = 8
                Top = 136
                Width = 153
                Height = 17
                Caption = 'Salvar envelope SOAP'
                TabOrder = 3
              end
              object seTimeOut: TSpinEdit
                Left = 167
                Top = 132
                Width = 66
                Height = 22
                Increment = 10
                MaxValue = 999999
                MinValue = 1000
                TabOrder = 4
                Value = 5000
              end
              object cbSSLType: TComboBox
                Left = 72
                Top = 160
                Width = 160
                Height = 21
                Hint = 'Depende de configura'#231#227'o de  SSL.HttpLib'
                Style = csDropDownList
                TabOrder = 5
                OnChange = cbSSLTypeChange
              end
            end
            object gbProxy: TGroupBox
              Left = 0
              Top = 283
              Width = 265
              Height = 104
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
                Width = 26
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
                Width = 30
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
            object gbxRetornoEnvio: TGroupBox
              Left = 0
              Top = 200
              Width = 265
              Height = 77
              Caption = 'Retorno de Envio Reinf'
              TabOrder = 2
              object Label36: TLabel
                Left = 93
                Top = 27
                Width = 51
                Height = 13
                Caption = 'Tentativas'
              end
              object Label37: TLabel
                Left = 176
                Top = 27
                Width = 44
                Height = 13
                Caption = 'Intervalo'
              end
              object Label38: TLabel
                Left = 8
                Top = 27
                Width = 45
                Height = 13
                Hint = 
                  'Aguardar quantos segundos para primeira consulta de retorno de e' +
                  'nvio'
                Caption = 'Aguardar'
              end
              object cbxAjustarAut: TCheckBox
                Left = 8
                Top = 12
                Width = 234
                Height = 17
                Caption = 'Ajustar Automaticamente prop. "Aguardar"'
                TabOrder = 0
              end
              object edtTentativas: TEdit
                Left = 93
                Top = 43
                Width = 57
                Height = 21
                TabOrder = 2
              end
              object edtIntervalo: TEdit
                Left = 176
                Top = 43
                Width = 57
                Height = 21
                TabOrder = 3
              end
              object edtAguardar: TEdit
                Left = 8
                Top = 43
                Width = 57
                Height = 21
                Hint = 
                  'Aguardar quantos segundos para primeira consulta de retorno de e' +
                  'nvio'
                TabOrder = 1
              end
            end
          end
          object TabSheet12: TTabSheet
            Caption = 'Emitente'
            ImageIndex = 3
            object Label12: TLabel
              Left = 8
              Top = 4
              Width = 25
              Height = 13
              Caption = 'CNPJ'
            end
            object Label13: TLabel
              Left = 136
              Top = 4
              Width = 43
              Height = 13
              Caption = 'Insc.Est.'
            end
            object Label14: TLabel
              Left = 8
              Top = 44
              Width = 60
              Height = 13
              Caption = 'Raz'#227'o Social'
            end
            object Label15: TLabel
              Left = 8
              Top = 84
              Width = 41
              Height = 13
              Caption = 'Fantasia'
            end
            object Label16: TLabel
              Left = 8
              Top = 164
              Width = 55
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
              Width = 65
              Height = 13
              Caption = 'Complemento'
            end
            object Label19: TLabel
              Left = 136
              Top = 204
              Width = 28
              Height = 13
              Caption = 'Bairro'
            end
            object Label20: TLabel
              Left = 8
              Top = 244
              Width = 62
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
              Width = 13
              Height = 13
              Caption = 'UF'
            end
            object Label23: TLabel
              Left = 136
              Top = 124
              Width = 19
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
            object PageControl3: TPageControl
              Left = 0
              Top = 288
              Width = 267
              Height = 142
              ActivePage = TabSheet9
              Align = alBottom
              TabOrder = 13
              object TabSheet9: TTabSheet
                Caption = 'Contato'
                object Label5: TLabel
                  Left = 3
                  Top = 0
                  Width = 27
                  Height = 13
                  Caption = 'Nome'
                end
                object Label26: TLabel
                  Left = 2
                  Top = 35
                  Width = 19
                  Height = 13
                  Caption = 'CPF'
                end
                object Label27: TLabel
                  Left = 4
                  Top = 74
                  Width = 24
                  Height = 13
                  Caption = 'Fone'
                end
                object Label28: TLabel
                  Left = 129
                  Top = 74
                  Width = 15
                  Height = 13
                  Caption = 'Cel'
                end
                object Label39: TLabel
                  Left = 129
                  Top = 35
                  Width = 24
                  Height = 13
                  Caption = 'Email'
                end
                object edContNome: TEdit
                  Left = 2
                  Top = 14
                  Width = 252
                  Height = 21
                  TabOrder = 0
                end
                object edContCPF: TEdit
                  Left = 2
                  Top = 51
                  Width = 123
                  Height = 21
                  TabOrder = 1
                end
                object edContFone: TEdit
                  Left = 1
                  Top = 89
                  Width = 125
                  Height = 21
                  TabOrder = 3
                end
                object edContCel: TEdit
                  Left = 129
                  Top = 89
                  Width = 123
                  Height = 21
                  TabOrder = 4
                end
                object edContEmail: TEdit
                  Left = 129
                  Top = 51
                  Width = 123
                  Height = 21
                  TabOrder = 2
                end
              end
              object TabSheet10: TTabSheet
                Caption = 'Software House'
                ImageIndex = 1
                object Label40: TLabel
                  Left = 3
                  Top = 0
                  Width = 60
                  Height = 13
                  Caption = 'Raz'#227'o Social'
                end
                object Label41: TLabel
                  Left = 2
                  Top = 35
                  Width = 25
                  Height = 13
                  Caption = 'CNPJ'
                end
                object Label43: TLabel
                  Left = 129
                  Top = 35
                  Width = 24
                  Height = 13
                  Caption = 'Email'
                end
                object Label44: TLabel
                  Left = 4
                  Top = 74
                  Width = 24
                  Height = 13
                  Caption = 'Fone'
                end
                object Label45: TLabel
                  Left = 129
                  Top = 74
                  Width = 39
                  Height = 13
                  Caption = 'Contato'
                end
                object edSoftRazao: TEdit
                  Left = 2
                  Top = 14
                  Width = 252
                  Height = 21
                  TabOrder = 0
                end
                object edSoftCNPJ: TEdit
                  Left = 2
                  Top = 51
                  Width = 123
                  Height = 21
                  TabOrder = 1
                end
                object edSoftEmail: TEdit
                  Left = 129
                  Top = 51
                  Width = 123
                  Height = 21
                  TabOrder = 2
                end
                object edSoftFone: TEdit
                  Left = 1
                  Top = 89
                  Width = 125
                  Height = 21
                  TabOrder = 3
                end
                object edSoftContato: TEdit
                  Left = 129
                  Top = 89
                  Width = 123
                  Height = 21
                  TabOrder = 4
                end
              end
            end
          end
          object TabSheet13: TTabSheet
            Caption = 'Arquivos'
            ImageIndex = 4
            object sbPathReinf: TSpeedButton
              Left = 240
              Top = 101
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
              OnClick = sbPathReinfClick
            end
            object Label35: TLabel
              Left = 6
              Top = 87
              Width = 100
              Height = 13
              Caption = 'Pasta Arquivos Reinf'
            end
            object Label47: TLabel
              Left = 6
              Top = 128
              Width = 109
              Height = 13
              Caption = 'Pasta Arquivos Evento'
            end
            object sbPathEvento: TSpeedButton
              Left = 240
              Top = 142
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
              OnClick = sbPathEventoClick
            end
            object cbxSalvarArqs: TCheckBox
              Left = 6
              Top = 0
              Width = 210
              Height = 17
              Caption = 'Salvar Arquivos em Pastas Separadas'
              TabOrder = 0
            end
            object cbxPastaMensal: TCheckBox
              Left = 6
              Top = 16
              Width = 210
              Height = 17
              Caption = 'Criar Pastas Mensalmente'
              TabOrder = 1
            end
            object cbxAdicionaLiteral: TCheckBox
              Left = 6
              Top = 32
              Width = 210
              Height = 17
              Caption = 'Adicionar Literal no nome das pastas'
              TabOrder = 2
            end
            object cbxSalvaPathEvento: TCheckBox
              Left = 6
              Top = 49
              Width = 233
              Height = 17
              Caption = 'Salvar Arqs de Eventos'
              TabOrder = 3
            end
            object cbxSepararPorCNPJ: TCheckBox
              Left = 6
              Top = 65
              Width = 233
              Height = 17
              Caption = 'Separar Arqs pelo CNPJ do Certificado'
              TabOrder = 4
            end
            object edtPathReinf: TEdit
              Left = 6
              Top = 103
              Width = 235
              Height = 21
              TabOrder = 5
            end
            object edtPathEvento: TEdit
              Left = 6
              Top = 144
              Width = 235
              Height = 21
              TabOrder = 6
            end
          end
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 297
    Top = 0
    Width = 591
    Height = 608
    Align = alClient
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 1
      Top = 78
      Width = 589
      Height = 529
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = '  Eventos   '
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 581
          Height = 365
          Align = alClient
          Caption = ' Eventos de Tabela '
          TabOrder = 0
          object chk1000: TCheckBox
            Left = 8
            Top = 23
            Width = 268
            Height = 17
            Caption = 'R-1000 - Informa'#231#245'es do Contribuinte'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 0
            OnClick = chk1000Click
          end
          object chk2010: TCheckBox
            Left = 8
            Top = 64
            Width = 553
            Height = 17
            Caption = 'R-2010 - Reten'#231#227'o Contribui'#231#227'o Previdenci'#225'ria  Servi'#231'os Tomados '
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 1
            OnClick = chk1000Click
          end
          object chk2020: TCheckBox
            Left = 8
            Top = 86
            Width = 553
            Height = 17
            Caption = 
              'R-2020 - Reten'#231#227'o Contribui'#231#227'o Previdenci'#225'ria  Servi'#231'os Prestado' +
              's'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 2
            OnClick = chk1000Click
          end
          object chk2098: TCheckBox
            Left = 8
            Top = 214
            Width = 289
            Height = 17
            Caption = 'R-2098 - Reabertura dos Eventos Peri'#243'dicos'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 3
            OnClick = chk1000Click
          end
          object chk1070: TCheckBox
            Left = 8
            Top = 43
            Width = 268
            Height = 17
            Caption = 'R-1070 - Tabela de Processos'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 4
            OnClick = chk1000Click
          end
          object chk2099: TCheckBox
            Left = 8
            Top = 235
            Width = 289
            Height = 17
            Caption = 'R-2099 - Fechamento dos Eventos Peri'#243'dicos'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 6
            OnClick = chk1000Click
          end
          object chk9000: TCheckBox
            Left = 8
            Top = 277
            Width = 271
            Height = 17
            Caption = 'R-9000 - Exclus'#227'o de Eventos'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 7
            OnClick = chk1000Click
          end
          object chk2060: TCheckBox
            Left = 8
            Top = 172
            Width = 413
            Height = 17
            Caption = 
              'R-2060 - Contribui'#231#227'o Previdenci'#225'ria sobre a Receita Bruta - CPR' +
              'B'
            Ctl3D = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 8
            OnClick = chk1000Click
          end
          object chk2070: TCheckBox
            Left = 8
            Top = 193
            Width = 377
            Height = 17
            Caption = 'R-2070 - Reten'#231#245'es na Fonte - IR, CSLL, Cofins, PIS/PASEP'
            Ctl3D = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 9
            OnClick = chk1000Click
          end
          object chk3010: TCheckBox
            Left = 8
            Top = 256
            Width = 469
            Height = 17
            Caption = 'R-3010 - Receita de Espet'#225'culo Desportivo'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 5
            OnClick = chk1000Click
          end
          object chk2030: TCheckBox
            Left = 8
            Top = 108
            Width = 413
            Height = 17
            Caption = 'R-2030 - Recursos Recebidos por Associa'#231#227'o Desportiva'
            Ctl3D = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 10
            OnClick = chk1000Click
          end
          object chk2040: TCheckBox
            Left = 8
            Top = 129
            Width = 409
            Height = 17
            Caption = 'R-2040 - Recursos Repassados para Associa'#231#227'o Desportiva'
            Ctl3D = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 11
            OnClick = chk1000Click
          end
          object chk2050: TCheckBox
            Left = 8
            Top = 150
            Width = 457
            Height = 17
            Caption = 
              'R-2050 - Comercializa'#231#227'o da Produ'#231#227'o por Produtor Rural PJ/Agroi' +
              'nd'#250'stria'
            Ctl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 12
            OnClick = chk1000Click
          end
        end
        object GroupBox4: TGroupBox
          Left = 0
          Top = 365
          Width = 581
          Height = 136
          Align = alBottom
          Caption = ' Dados Adicionais '
          TabOrder = 1
          object Label1: TLabel
            Left = 4
            Top = 15
            Width = 52
            Height = 13
            Caption = 'Protocolo :'
          end
          object lblRecibo: TLabel
            Left = 4
            Top = 53
            Width = 50
            Height = 13
            Caption = 'Nr. Recibo'
            Visible = False
          end
          object lblEvento: TLabel
            Left = 258
            Top = 53
            Width = 60
            Height = 13
            Caption = 'Cod. Evento'
            Visible = False
          end
          object edProtocolo: TEdit
            Left = 4
            Top = 29
            Width = 541
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object edRecibo: TEdit
            Left = 4
            Top = 66
            Width = 247
            Height = 21
            TabOrder = 1
            Visible = False
          end
          object cbEvento: TComboBox
            Left = 258
            Top = 66
            Width = 87
            Height = 21
            TabOrder = 2
            Visible = False
            Items.Strings = (
              'R-2010'
              'R-2020'
              'R-2030'
              'R-2040'
              'R-2050'
              'R-2060'
              'R-2070'
              'R-3010')
          end
          object ChkRetificadora: TCheckBox
            Left = 356
            Top = 65
            Width = 97
            Height = 17
            Caption = 'Retificadora'
            TabOrder = 3
            Visible = False
          end
          object rdgOperacao: TRadioGroup
            Left = 2
            Top = 91
            Width = 577
            Height = 43
            Align = alBottom
            Caption = '  Opera'#231#227'o  '
            Columns = 3
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ItemIndex = 0
            Items.Strings = (
              'Inclus'#227'o'
              'Altera'#231#227'o'
              'Exclus'#227'o')
            ParentFont = False
            TabOrder = 4
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = '  Dados de Retorno Eventos'
        ImageIndex = 1
        object mmoDados: TMemo
          Left = 0
          Top = 0
          Width = 581
          Height = 501
          Align = alClient
          Lines.Strings = (
            'mmoRet')
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = '  XML de Envio   '
        ImageIndex = 2
        object mmoXMLEnv: TMemo
          Left = 0
          Top = 0
          Width = 581
          Height = 501
          Align = alClient
          Lines.Strings = (
            'Memo1')
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet4: TTabSheet
        Caption = '  XML Retorno  '
        ImageIndex = 3
        object mmoXMLRet: TMemo
          Left = 0
          Top = 0
          Width = 581
          Height = 501
          Align = alClient
          Lines.Strings = (
            'Memo1')
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsLog: TTabSheet
        Caption = 'Log'
        ImageIndex = 4
        object memoLog: TMemo
          Left = 0
          Top = 0
          Width = 581
          Height = 501
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 730
          ExplicitHeight = 237
        end
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 589
      Height = 77
      Align = alTop
      TabOrder = 1
      object btnGerar: TButton
        Left = 8
        Top = 10
        Width = 137
        Height = 24
        Caption = 'Gerar Arquivos'
        TabOrder = 0
        OnClick = btnGerarClick
      end
      object btnValidarSchema: TButton
        Left = 151
        Top = 42
        Width = 137
        Height = 24
        Caption = 'Validar Schema'
        TabOrder = 1
        OnClick = btnValidarSchemaClick
      end
      object btnValidarAssinatura: TButton
        Left = 8
        Top = 42
        Width = 137
        Height = 24
        Caption = 'Validar Assinatura'
        TabOrder = 2
        OnClick = btnValidarAssinaturaClick
      end
      object btnConsultar: TButton
        Left = 294
        Top = 42
        Width = 137
        Height = 24
        Caption = 'Consultar'
        TabOrder = 3
        OnClick = btnConsultarClick
      end
      object btnLerArqINI: TButton
        Left = 151
        Top = 10
        Width = 137
        Height = 24
        Caption = 'Ler Arquivo INI'
        TabOrder = 4
        OnClick = btnLerArqINIClick
      end
      object btnEnviar: TButton
        Left = 294
        Top = 9
        Width = 137
        Height = 24
        Caption = 'Enviar'
        TabOrder = 5
        OnClick = btnEnviarClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.XML'
    Filter = 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*'
    Title = 'Selecione o Arquivo'
    Left = 394
    Top = 399
  end
  object ACBrReinf1: TACBrReinf
    OnStatusChange = ACBrReinf1StatusChange
    OnGerarLog = ACBrReinf1GerarLog
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    OnTransmissaoEventos = ACBrReinf1TransmissaoEventos
    Left = 326
    Top = 400
  end
end
