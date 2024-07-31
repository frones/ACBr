object frmACBrNFe: TfrmACBrNFe
  Left = 252
  Top = 127
  Caption = 'ACBrNFe - Programa Exemplo'
  ClientHeight = 612
  ClientWidth = 866
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object pnlMenus: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 612
    Align = alLeft
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 295
      Height = 544
      ActivePage = TabSheet1
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Configura'#231#245'es'
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 287
          Height = 516
          ActivePage = TabSheet12
          Align = alClient
          MultiLine = True
          TabOrder = 0
          object TabSheet3: TTabSheet
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
              Height = 184
              Caption = 'Certificado'
              TabOrder = 0
              object Label1: TLabel
                Left = 7
                Top = 60
                Width = 41
                Height = 13
                Caption = 'Caminho'
              end
              object Label2: TLabel
                Left = 7
                Top = 100
                Width = 30
                Height = 13
                Caption = 'Senha'
              end
              object sbtnCaminhoCert: TSpeedButton
                Left = 234
                Top = 76
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
                Left = 7
                Top = 140
                Width = 79
                Height = 13
                Caption = 'N'#250'mero de S'#233'rie'
              end
              object sbtnGetCert: TSpeedButton
                Left = 234
                Top = 154
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
              object sbtnNumSerie: TSpeedButton
                Left = 205
                Top = 154
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
                OnClick = sbtnNumSerieClick
              end
              object Label51: TLabel
                Left = 7
                Top = 17
                Width = 65
                Height = 13
                Caption = 'URL para PFX'
              end
              object edtCaminho: TEdit
                Left = 7
                Top = 76
                Width = 225
                Height = 21
                TabOrder = 1
              end
              object edtSenha: TEdit
                Left = 7
                Top = 116
                Width = 249
                Height = 21
                PasswordChar = '*'
                TabOrder = 2
              end
              object edtNumSerie: TEdit
                Left = 7
                Top = 156
                Width = 193
                Height = 21
                TabOrder = 3
              end
              object edtURLPFX: TEdit
                Left = 7
                Top = 33
                Width = 249
                Height = 21
                TabOrder = 0
              end
            end
            object btnDataValidade: TButton
              Left = 7
              Top = 305
              Width = 99
              Height = 25
              Caption = 'Data de Validade'
              TabOrder = 1
              OnClick = btnDataValidadeClick
            end
            object btnNumSerie: TButton
              Left = 111
              Top = 305
              Width = 73
              Height = 25
              Caption = 'Num.S'#233'rie'
              TabOrder = 2
              OnClick = btnNumSerieClick
            end
            object btnSubName: TButton
              Left = 7
              Top = 337
              Width = 99
              Height = 25
              Caption = 'Subject Name'
              TabOrder = 4
              OnClick = btnSubNameClick
            end
            object btnCNPJ: TButton
              Left = 112
              Top = 337
              Width = 73
              Height = 25
              Caption = 'CNPJ'
              TabOrder = 5
              OnClick = btnCNPJClick
            end
            object btnIssuerName: TButton
              Left = 190
              Top = 306
              Width = 76
              Height = 25
              Caption = 'Issuer Name'
              TabOrder = 3
              OnClick = btnIssuerNameClick
            end
            object GroupBox1: TGroupBox
              Left = 3
              Top = 367
              Width = 263
              Height = 69
              Caption = 'Calculo de Hash e assinatura'
              TabOrder = 7
              object Edit1: TEdit
                Left = 3
                Top = 14
                Width = 249
                Height = 21
                TabOrder = 0
                Text = '0548133600013704583493000190'
              end
              object btnSha256: TButton
                Left = 8
                Top = 41
                Width = 99
                Height = 25
                Caption = 'SHA256+RSA'
                TabOrder = 1
                OnClick = btnSha256Click
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
            object btnHTTPS: TButton
              Left = 9
              Top = 442
              Width = 128
              Height = 25
              Caption = 'HTTPS sem Certificado'
              TabOrder = 8
              OnClick = btnHTTPSClick
            end
            object btnLeituraX509: TButton
              Left = 145
              Top = 442
              Width = 115
              Height = 25
              Caption = 'Leitura de X509'
              TabOrder = 9
              OnClick = btnLeituraX509Click
            end
            object cbSSLLib: TComboBox
              Left = 80
              Top = 8
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 10
              OnChange = cbSSLLibChange
            end
            object cbCryptLib: TComboBox
              Left = 80
              Top = 35
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 11
              OnChange = cbCryptLibChange
            end
            object cbHttpLib: TComboBox
              Left = 80
              Top = 62
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 12
              OnChange = cbHttpLibChange
            end
            object cbXmlSignLib: TComboBox
              Left = 80
              Top = 89
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 13
              OnChange = cbXmlSignLibChange
            end
            object btVersao: TButton
              Left = 190
              Top = 336
              Width = 76
              Height = 25
              Caption = 'Ver.SSL Lib'
              TabOrder = 6
              OnClick = btVersaoClick
            end
          end
          object TabSheet4: TTabSheet
            Caption = 'Geral'
            ImageIndex = 1
            object GroupBox3: TGroupBox
              Left = 3
              Top = 12
              Width = 265
              Height = 389
              Caption = 'Geral'
              TabOrder = 0
              object sbtnPathSalvar: TSpeedButton
                Left = 238
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
              object Label30: TLabel
                Left = 8
                Top = 126
                Width = 120
                Height = 13
                Caption = 'Modelo Documento Fiscal'
              end
              object Label32: TLabel
                Left = 8
                Top = 165
                Width = 119
                Height = 13
                Caption = 'Vers'#227'o Documento Fiscal'
              end
              object Label33: TLabel
                Left = 8
                Top = 299
                Width = 184
                Height = 13
                Caption = 'IdToken/IdCSC (Somente para NFC-e)'
              end
              object Label34: TLabel
                Left = 7
                Top = 339
                Width = 164
                Height = 13
                Caption = 'Token/CSC (Somente para NFC-e)'
              end
              object Label42: TLabel
                Left = 8
                Top = 260
                Width = 198
                Height = 13
                Caption = 'Diret'#243'rios com os arquivos XSD(Schemas)'
              end
              object spPathSchemas: TSpeedButton
                Left = 238
                Top = 276
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
                Top = 239
                Width = 228
                Height = 21
                TabOrder = 0
              end
              object ckSalvar: TCheckBox
                Left = 8
                Top = 223
                Width = 209
                Height = 15
                Caption = 'Salvar Arquivos de Envio e Resposta'
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
              object cbModeloDF: TComboBox
                Left = 8
                Top = 142
                Width = 248
                Height = 21
                TabOrder = 6
              end
              object cbxRetirarAcentos: TCheckBox
                Left = 8
                Top = 205
                Width = 193
                Height = 17
                Caption = 'Retirar Acentos dos XMLs enviados'
                TabOrder = 7
              end
              object cbVersaoDF: TComboBox
                Left = 8
                Top = 181
                Width = 248
                Height = 21
                TabOrder = 8
              end
              object edtIdToken: TEdit
                Left = 8
                Top = 315
                Width = 248
                Height = 21
                TabOrder = 9
              end
              object edtToken: TEdit
                Left = 7
                Top = 355
                Width = 248
                Height = 21
                TabOrder = 10
              end
              object edtPathSchemas: TEdit
                Left = 8
                Top = 276
                Width = 228
                Height = 21
                TabOrder = 11
              end
            end
          end
          object TabSheet7: TTabSheet
            Caption = 'WebService'
            ImageIndex = 2
            object GroupBox4: TGroupBox
              Left = 5
              Top = 4
              Width = 265
              Height = 190
              Caption = 'WebService'
              TabOrder = 0
              object Label6: TLabel
                Left = 8
                Top = 16
                Width = 126
                Height = 13
                Caption = 'Selecione UF do Emitente:'
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
                Top = 168
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
              Left = 5
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
              Left = 5
              Top = 200
              Width = 265
              Height = 77
              Caption = 'Retorno de Envio'
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
            object Label52: TLabel
              Left = 8
              Top = 289
              Width = 83
              Height = 13
              Caption = 'Tipo de Empresa:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
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
            object cbTipoEmpresa: TComboBox
              Left = 8
              Top = 305
              Width = 249
              Height = 24
              Style = csDropDownList
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 13
              Items.Strings = (
                'Simples Nacional'
                'Simples Nacional, excesso sublimite de receita bruta'
                'Regime Normal')
            end
          end
          object TabSheet13: TTabSheet
            Caption = 'Arquivos'
            ImageIndex = 4
            object sbPathNFe: TSpeedButton
              Left = 247
              Top = 131
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
              OnClick = sbPathNFeClick
            end
            object Label35: TLabel
              Left = 6
              Top = 116
              Width = 94
              Height = 13
              Caption = 'Pasta Arquivos NFe'
            end
            object Label40: TLabel
              Left = 6
              Top = 202
              Width = 129
              Height = 13
              Caption = 'Pasta Arquivos Inutiliza'#231#227'o'
            end
            object sbPathInu: TSpeedButton
              Left = 247
              Top = 217
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
              OnClick = sbPathInuClick
            end
            object Label47: TLabel
              Left = 6
              Top = 159
              Width = 109
              Height = 13
              Caption = 'Pasta Arquivos Evento'
            end
            object sbPathEvento: TSpeedButton
              Left = 247
              Top = 174
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
            object Label39: TLabel
              Left = 6
              Top = 245
              Width = 94
              Height = 13
              Caption = 'Pasta Arquivos PDF'
            end
            object sbPathPDF: TSpeedButton
              Left = 247
              Top = 260
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
              OnClick = sbPathPDFClick
            end
            object cbxSalvarArqs: TCheckBox
              Left = 6
              Top = 0
              Width = 210
              Height = 17
              Caption = 'Salvar Arquivos'
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
            object cbxEmissaoPathNFe: TCheckBox
              Left = 6
              Top = 48
              Width = 251
              Height = 17
              Caption = 'Salvar Documento pelo campo Data de Emiss'#227'o'
              TabOrder = 3
            end
            object cbxSalvaPathEvento: TCheckBox
              Left = 6
              Top = 64
              Width = 233
              Height = 17
              Caption = 'Salvar Arquivos de Eventos'
              TabOrder = 4
            end
            object cbxSepararPorCNPJ: TCheckBox
              Left = 6
              Top = 80
              Width = 233
              Height = 17
              Caption = 'Separar Arqs pelo CNPJ do Certificado'
              TabOrder = 5
            end
            object edtPathNFe: TEdit
              Left = 6
              Top = 132
              Width = 235
              Height = 21
              TabOrder = 7
            end
            object edtPathInu: TEdit
              Left = 6
              Top = 218
              Width = 235
              Height = 21
              TabOrder = 9
            end
            object edtPathEvento: TEdit
              Left = 6
              Top = 175
              Width = 235
              Height = 21
              TabOrder = 8
            end
            object cbxSepararPorModelo: TCheckBox
              Left = 6
              Top = 96
              Width = 251
              Height = 17
              Caption = 'Separar Arqs pelo Modelo do Documento'
              TabOrder = 6
            end
            object edtPathPDF: TEdit
              Left = 6
              Top = 261
              Width = 235
              Height = 21
              TabOrder = 10
            end
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Documento Auxiliar'
        ImageIndex = 1
        object Label7: TLabel
          Left = 8
          Top = 8
          Width = 55
          Height = 13
          Caption = 'Logo Marca'
        end
        object sbtnLogoMarca: TSpeedButton
          Left = 238
          Top = 22
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
        object edtLogoMarca: TEdit
          Left = 8
          Top = 24
          Width = 228
          Height = 21
          TabOrder = 0
        end
        object rgTipoDanfe: TRadioGroup
          Left = 8
          Top = 56
          Width = 257
          Height = 58
          Caption = 'DANFE'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Retrato'
            'Paisagem'
            'Simplificado')
          TabOrder = 1
        end
        object gbEscPos: TGroupBox
          Left = 8
          Top = 120
          Width = 257
          Height = 233
          Caption = 'EscPos'
          TabOrder = 2
          object Label43: TLabel
            Left = 8
            Top = 24
            Width = 34
            Height = 13
            Caption = 'Modelo'
            Color = clBtnFace
            ParentColor = False
          end
          object Label44: TLabel
            Left = 8
            Top = 72
            Width = 26
            Height = 13
            Caption = 'Porta'
            Color = clBtnFace
            ParentColor = False
          end
          object Label45: TLabel
            Left = 8
            Top = 184
            Width = 55
            Height = 13
            Caption = 'Pag.Codigo'
            Color = clBtnFace
            ParentColor = False
          end
          object Label48: TLabel
            Left = 8
            Top = 136
            Width = 38
            Height = 13
            Caption = 'Colunas'
            Color = clBtnFace
            ParentColor = False
          end
          object Label49: TLabel
            Left = 80
            Top = 120
            Width = 39
            Height = 26
            Caption = 'Espa'#231'os'#13#10'Linhas'
            Color = clBtnFace
            ParentColor = False
          end
          object Label50: TLabel
            Left = 144
            Top = 120
            Width = 30
            Height = 26
            Caption = 'Linhas'#13#10'Pular'
            Color = clBtnFace
            ParentColor = False
          end
          object btSerial: TBitBtn
            Left = 216
            Top = 88
            Width = 22
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
            Layout = blGlyphTop
            ModalResult = 1
            TabOrder = 0
            OnClick = btSerialClick
          end
          object cbxModeloPosPrinter: TComboBox
            Left = 8
            Top = 40
            Width = 233
            Height = 21
            Style = csDropDownList
            TabOrder = 1
          end
          object cbxPorta: TComboBox
            Left = 8
            Top = 88
            Width = 201
            Height = 21
            TabOrder = 2
          end
          object cbxPagCodigo: TComboBox
            Left = 8
            Top = 200
            Width = 101
            Height = 21
            Style = csDropDownList
            TabOrder = 3
          end
          object seColunas: TSpinEdit
            Left = 8
            Top = 152
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 4
            Value = 0
          end
          object seEspLinhas: TSpinEdit
            Left = 80
            Top = 152
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 5
            Value = 0
          end
          object seLinhasPular: TSpinEdit
            Left = 144
            Top = 152
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 6
            Value = 0
          end
          object cbCortarPapel: TCheckBox
            Left = 143
            Top = 196
            Width = 85
            Height = 19
            Hint = 
              'Conecta a Porta Serial a cada comando enviado'#13#10'Desconecta da Por' +
              'ta Serial ap'#243's o envio'
            Caption = 'Cortar Papel'
            Checked = True
            State = cbChecked
            TabOrder = 7
          end
        end
        object rgDANFCE: TRadioGroup
          Left = 8
          Top = 361
          Width = 257
          Height = 49
          Caption = 'DANFCE'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'Fortes'
            'EscPos'
            'A4')
          TabOrder = 3
        end
      end
      object TabSheet14: TTabSheet
        Caption = 'Email'
        ImageIndex = 2
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 69
          Height = 13
          Caption = 'Servidor SMTP'
        end
        object Label4: TLabel
          Left = 206
          Top = 8
          Width = 26
          Height = 13
          Caption = 'Porta'
        end
        object Label5: TLabel
          Left = 8
          Top = 48
          Width = 36
          Height = 13
          Caption = 'Usu'#225'rio'
        end
        object Label26: TLabel
          Left = 137
          Top = 48
          Width = 30
          Height = 13
          Caption = 'Senha'
        end
        object Label27: TLabel
          Left = 8
          Top = 88
          Width = 122
          Height = 13
          Caption = 'Assunto do email enviado'
        end
        object Label28: TLabel
          Left = 8
          Top = 160
          Width = 93
          Height = 13
          Caption = 'Mensagem do Email'
        end
        object edtSmtpHost: TEdit
          Left = 8
          Top = 24
          Width = 193
          Height = 21
          TabOrder = 0
        end
        object edtSmtpPort: TEdit
          Left = 206
          Top = 24
          Width = 51
          Height = 21
          TabOrder = 1
        end
        object edtSmtpUser: TEdit
          Left = 8
          Top = 64
          Width = 120
          Height = 21
          TabOrder = 2
        end
        object edtSmtpPass: TEdit
          Left = 137
          Top = 64
          Width = 120
          Height = 21
          TabOrder = 3
        end
        object edtEmailAssunto: TEdit
          Left = 8
          Top = 104
          Width = 249
          Height = 21
          TabOrder = 4
        end
        object cbEmailSSL: TCheckBox
          Left = 10
          Top = 136
          Width = 167
          Height = 17
          Caption = 'SMTP exige conex'#227'o segura'
          TabOrder = 5
        end
        object mmEmailMsg: TMemo
          Left = 8
          Top = 176
          Width = 249
          Height = 130
          TabOrder = 6
        end
      end
    end
    object btnSalvarConfig: TBitBtn
      Left = 74
      Top = 560
      Width = 153
      Height = 36
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
  object pnlCentral: TPanel
    Left = 297
    Top = 0
    Width = 569
    Height = 612
    Align = alClient
    TabOrder = 1
    object pgcBotoes: TPageControl
      Left = 1
      Top = 1
      Width = 567
      Height = 197
      ActivePage = tsEnvios
      Align = alTop
      TabOrder = 0
      object tsEnvios: TTabSheet
        Caption = 'Envios'
        ImageIndex = 2
        object btnCriarEnviar: TButton
          Left = 376
          Top = 8
          Width = 177
          Height = 25
          Caption = 'Criar e Enviar'
          TabOrder = 2
          OnClick = btnCriarEnviarClick
        end
        object btnValidarRegrasNegocio: TButton
          Left = 9
          Top = 71
          Width = 177
          Height = 25
          Caption = 'Validar Regras de Neg'#243'cio'
          TabOrder = 5
          OnClick = btnValidarRegrasNegocioClick
        end
        object btnGerarTXT: TButton
          Left = 190
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Gerar TXT'
          TabOrder = 1
          OnClick = btnGerarTXTClick
        end
        object btnGerarXML: TButton
          Left = 7
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Gerar NFe'
          TabOrder = 0
          OnClick = btnGerarXMLClick
        end
        object btnImportarXML: TButton
          Left = 191
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Importar TXT/XML'
          TabOrder = 4
          OnClick = btnImportarXMLClick
        end
        object btnGerarPDF: TButton
          Left = 375
          Top = 102
          Width = 177
          Height = 25
          Caption = 'Gerar PDF'
          TabOrder = 10
          OnClick = btnGerarPDFClick
        end
        object btnValidarXML: TButton
          Left = 192
          Top = 71
          Width = 177
          Height = 25
          Caption = 'Validar XML'
          TabOrder = 6
          OnClick = btnValidarXMLClick
        end
        object btnImprimir: TButton
          Left = 192
          Top = 102
          Width = 177
          Height = 25
          Caption = 'Imprimir DANFE'
          TabOrder = 9
          OnClick = btnImprimirClick
        end
        object btnEnviarEmail: TButton
          Left = 9
          Top = 133
          Width = 177
          Height = 25
          Caption = 'Enviar NFe Email'
          TabOrder = 11
          OnClick = btnEnviarEmailClick
        end
        object btnAdicionarProtocolo: TButton
          Left = 9
          Top = 102
          Width = 177
          Height = 25
          Caption = 'Adicionar Protocolo ao XML'
          TabOrder = 8
          OnClick = btnAdicionarProtocoloClick
        end
        object btnCarregarXMLEnviar: TButton
          Left = 8
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Carregar XML e Enviar'
          TabOrder = 3
          OnClick = btnCarregarXMLEnviarClick
        end
        object btnValidarAssinatura: TButton
          Left = 375
          Top = 71
          Width = 177
          Height = 25
          Caption = 'Validar Assinatura'
          TabOrder = 7
          OnClick = btnValidarAssinaturaClick
        end
        object btnImprimirDANFCE: TButton
          Left = 192
          Top = 133
          Width = 177
          Height = 25
          Caption = 'Imprimir DANFCE'
          TabOrder = 12
          OnClick = btnImprimirDANFCEClick
        end
        object btnImprimirDANFCEOffline: TButton
          Left = 375
          Top = 133
          Width = 177
          Height = 25
          Caption = 'Imprimir DANFCE Offline'
          TabOrder = 13
          OnClick = btnImprimirDANFCEOfflineClick
        end
      end
      object tsConsultas: TTabSheet
        Caption = 'Consultas'
        ImageIndex = 3
        object btnConsultar: TButton
          Left = 191
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Consultar carregando XML'
          TabOrder = 1
          OnClick = btnConsultarClick
        end
        object btnConsultarChave: TButton
          Left = 8
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Consultar pela Chave'
          TabOrder = 3
          OnClick = btnConsultarChaveClick
        end
        object btnConsCad: TButton
          Left = 192
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Consulta Cadastro'
          TabOrder = 4
          OnClick = btnConsCadClick
        end
        object btnConsultarRecibo: TButton
          Left = 376
          Top = 8
          Width = 177
          Height = 25
          Caption = 'Consultar Recibo Lote'
          TabOrder = 2
          OnClick = btnConsultarReciboClick
        end
        object btnStatusServ: TButton
          Left = 8
          Top = 9
          Width = 177
          Height = 25
          Caption = ' Status de Servi'#231'o'
          TabOrder = 0
          OnClick = btnStatusServClick
        end
        object btnAdministrarCSC: TButton
          Left = 8
          Top = 100
          Width = 177
          Height = 25
          Caption = 'Administrar CSC'
          TabOrder = 5
          OnClick = btnAdministrarCSCClick
        end
      end
      object tsEventos: TTabSheet
        Caption = 'Eventos'
        ImageIndex = 4
        object btnCancelarXML: TButton
          Left = 8
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Cancelamento com XML'
          TabOrder = 0
          OnClick = btnCancelarXMLClick
        end
        object btnCancelarChave: TButton
          Left = 191
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Cancelamento pela Chave'
          TabOrder = 1
          OnClick = btnCancelarChaveClick
        end
        object btnCartadeCorrecao: TButton
          Left = 374
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Carta de Corre'#231#227'o'
          TabOrder = 2
          OnClick = btnCartadeCorrecaoClick
        end
        object btnImprimirEvento: TButton
          Left = 8
          Top = 141
          Width = 177
          Height = 25
          Caption = 'Imprimir Evento'
          TabOrder = 4
          OnClick = btnImprimirEventoClick
        end
        object btnEnviarEventoEmail: TButton
          Left = 191
          Top = 141
          Width = 177
          Height = 25
          Caption = 'Enviar Evento Email'
          TabOrder = 5
          OnClick = btnEnviarEventoEmailClick
        end
        object btnAtorInterNFeTransp: TButton
          Left = 8
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Ator Interessado na NF-e Transp.'
          TabOrder = 3
          OnClick = btnAtorInterNFeTranspClick
        end
        object btnEventoEPEC: TButton
          Left = 191
          Top = 40
          Width = 177
          Height = 25
          Caption = 'EPEC'
          TabOrder = 6
          OnClick = btnEventoEPECClick
        end
        object btnInsucessoEntrega: TButton
          Left = 8
          Top = 69
          Width = 177
          Height = 25
          Caption = 'Insucesso na Entrega'
          TabOrder = 7
          OnClick = btnInsucessoEntregaClick
        end
        object btnCancInsucessoEntrega: TButton
          Left = 191
          Top = 69
          Width = 177
          Height = 25
          Caption = 'Canc. Insucesso na Entrega'
          TabOrder = 8
          OnClick = btnCancInsucessoEntregaClick
        end
        object btnEventoECONF: TButton
          Left = 8
          Top = 100
          Width = 177
          Height = 25
          Caption = 'ECONF'
          TabOrder = 9
          OnClick = btnEventoECONFClick
        end
        object btnEventoCancECONF: TButton
          Left = 191
          Top = 100
          Width = 177
          Height = 25
          Caption = 'Cancelar ECONF'
          TabOrder = 10
          OnClick = btnEventoCancECONFClick
        end
      end
      object tsInutilizacao: TTabSheet
        Caption = 'Inutiliza'#231#227'o'
        ImageIndex = 5
        object btnInutilizar: TButton
          Left = 8
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Inutilizar Numera'#231#227'o'
          TabOrder = 0
          OnClick = btnInutilizarClick
        end
        object btnInutilizarImprimir: TButton
          Left = 191
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Inutilizar Imprimir'
          TabOrder = 1
          OnClick = btnInutilizarImprimirClick
        end
      end
      object tsDistribuicao: TTabSheet
        Caption = 'Distribui'#231#227'o DFe'
        ImageIndex = 5
        object btnManifDestConfirmacao: TButton
          Left = 8
          Top = 40
          Width = 178
          Height = 25
          Caption = 'Manif. Dest. - Conf. Opera'#231#227'o'
          TabOrder = 3
          OnClick = btnManifDestConfirmacaoClick
        end
        object btnDistrDFePorUltNSU: TButton
          Left = 8
          Top = 9
          Width = 178
          Height = 25
          Caption = 'Distribui'#231#227'o DF-e por '#250'ltimo NSU'
          TabOrder = 0
          OnClick = btnDistrDFePorUltNSUClick
        end
        object btnDistrDFePorNSU: TButton
          Left = 192
          Top = 9
          Width = 178
          Height = 25
          Caption = 'Distribui'#231#227'o DF-e por NSU'
          TabOrder = 1
          OnClick = btnDistrDFePorNSUClick
        end
        object btnDistrDFePorChave: TButton
          Left = 376
          Top = 9
          Width = 178
          Height = 25
          Caption = 'Distribui'#231#227'o DF-e por Chave'
          TabOrder = 2
          OnClick = btnDistrDFePorChaveClick
        end
        object btnManifDestDesconnhecimento: TButton
          Left = 192
          Top = 40
          Width = 178
          Height = 25
          Caption = 'Manif. Dest. - Desconhecimento'
          TabOrder = 4
          OnClick = btnManifDestConfirmacaoClick
        end
        object btnManifDestCiencia: TButton
          Left = 377
          Top = 40
          Width = 178
          Height = 25
          Caption = 'Manif. Dest. - Ci'#234'ncia'
          TabOrder = 5
          OnClick = btnManifDestConfirmacaoClick
        end
        object btnManifDestOperNaoRealizada: TButton
          Left = 8
          Top = 71
          Width = 178
          Height = 25
          Caption = 'Manif. Dest. - Oper. N'#227'o Realizada'
          TabOrder = 6
          OnClick = btnManifDestConfirmacaoClick
        end
      end
    end
    object pgRespostas: TPageControl
      Left = 1
      Top = 198
      Width = 567
      Height = 413
      ActivePage = Dados
      Align = alClient
      TabOrder = 1
      object TabSheet5: TTabSheet
        Caption = 'Respostas'
        object MemoResp: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 385
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'XML Resposta'
        ImageIndex = 1
        object WBResposta: TWebBrowser
          Left = 0
          Top = 0
          Width = 559
          Height = 385
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C000000C6390000CA2700000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126200000000000000004C0000000114020000000000C000000000000046
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
          Width = 559
          Height = 385
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'Documento'
        ImageIndex = 3
        object trvwDocumento: TTreeView
          Left = 0
          Top = 0
          Width = 559
          Height = 385
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
          Width = 559
          Height = 385
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object Dados: TTabSheet
        Caption = 'Dados'
        ImageIndex = 5
        object MemoDados: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 385
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Lucida Console'
          Font.Style = []
          Lines.Strings = (
            '')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object ACBrNFe1: TACBrNFe
    MAIL = ACBrMail1
    OnStatusChange = ACBrNFe1StatusChange
    OnGerarLog = ACBrNFe1GerarLog
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormaEmissao = teContingencia
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.VersaoDF = ve200
    Configuracoes.Geral.AtualizarXMLCancelado = True
    Configuracoes.Geral.VersaoQRCode = veqr000
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 15000
    Configuracoes.WebServices.AjustaAguardaConsultaRet = True
    Configuracoes.WebServices.TimeOut = 20000
    Configuracoes.WebServices.QuebradeLinha = '|'
    Configuracoes.RespTec.IdCSRT = 0
    DANFE = ACBrNFeDANFeRL1
    Left = 330
    Top = 239
  end
  object ACBrNFeDANFeRL1: TACBrNFeDANFeRL
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    Usuario = 'ACBr'
    MargemInferior = 0.700000000000000000
    MargemSuperior = 0.700000000000000000
    MargemEsquerda = 0.700000000000000000
    MargemDireita = 0.700000000000000000
    ExpandeLogoMarcaConfig.Altura = 0
    ExpandeLogoMarcaConfig.Esquerda = 0
    ExpandeLogoMarcaConfig.Topo = 0
    ExpandeLogoMarcaConfig.Largura = 0
    ExpandeLogoMarcaConfig.Dimensionar = False
    ExpandeLogoMarcaConfig.Esticar = True
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 4
    CasasDecimais.vUnCom = 4
    CasasDecimais.MaskqCom = '###,###,###,##0.00'
    CasasDecimais.MaskvUnCom = '###,###,###,##0.00'
    CasasDecimais.Aliquota = 2
    CasasDecimais.MaskAliquota = ',0.00'
    ACBrNFe = ACBrNFe1
    ExibeResumoCanhoto = False
    ExibeCampoFatura = False
    Left = 427
    Top = 239
  end
  object ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
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
    CasasDecimais.MaskqCom = '###,###,###,##0.00'
    CasasDecimais.MaskvUnCom = '###,###,###,##0.00'
    CasasDecimais.Aliquota = 2
    CasasDecimais.MaskAliquota = ',0.00'
    TipoDANFE = tiSemGeracao
    ImprimeNomeFantasia = True
    FormularioContinuo = True
    FonteLinhaItem.Charset = DEFAULT_CHARSET
    FonteLinhaItem.Color = clWindowText
    FonteLinhaItem.Height = -9
    FonteLinhaItem.Name = 'Lucida Console'
    FonteLinhaItem.Style = []
    Left = 562
    Top = 239
  end
  object ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
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
    CasasDecimais.qCom = 4
    CasasDecimais.vUnCom = 4
    CasasDecimais.MaskqCom = '###,###,###,##0.00'
    CasasDecimais.MaskvUnCom = '###,###,###,##0.00'
    CasasDecimais.Aliquota = 2
    CasasDecimais.MaskAliquota = ',0.00'
    TipoDANFE = tiSemGeracao
    FormularioContinuo = True
    PosPrinter = ACBrPosPrinter1
    Left = 561
    Top = 295
  end
  object ACBrPosPrinter1: TACBrPosPrinter
    Modelo = ppEscPosEpson
    Porta = 'COM9'
    EspacoEntreLinhas = 30
    ConfigBarras.MostrarCodigo = False
    ConfigBarras.LarguraLinha = 0
    ConfigBarras.Altura = 0
    ConfigBarras.Margem = 0
    ConfigQRCode.Tipo = 2
    ConfigQRCode.LarguraModulo = 4
    ConfigQRCode.ErrorLevel = 0
    LinhasEntreCupons = 5
    Left = 425
    Top = 295
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 330
    Top = 295
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*-nfe.XML'
    Filter = 
      'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|To' +
      'dos os Arquivos (*.*)|*.*'
    Title = 'Selecione a NFe'
    Left = 427
    Top = 351
  end
  object ACBrIntegrador1: TACBrIntegrador
    PastaInput = 'C:\Integrador\Input\'
    PastaOutput = 'C:\Integrador\Output\'
    Left = 334
    Top = 350
  end
  object ACBrNFeDANFCeFortesA41: TACBrNFeDANFCeFortesA4
    Sistema = 'Projeto ACBr - www.projetoacbr.com.br'
    MargemInferior = 8.000000000000000000
    MargemSuperior = 8.000000000000000000
    MargemEsquerda = 6.000000000000000000
    MargemDireita = 5.099999999999999000
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
    FormularioContinuo = True
    Left = 560
    Top = 352
  end
end
