object frmACBrNF3e: TfrmACBrNF3e
  Left = 448
  Top = 80
  Caption = 'ACBrNF3e - Programa Exemplo'
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
      OnClick = lblDoar2Click
    end
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 295
      Height = 504
      ActivePage = TabSheet1
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Configura'#231#245'es'
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 287
          Height = 476
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
              object sbtnNumSerie: TSpeedButton
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
                OnClick = sbtnNumSerieClick
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
            object btnDataValidade: TButton
              Left = 8
              Top = 266
              Width = 99
              Height = 25
              Caption = 'Data de Validade'
              TabOrder = 1
              OnClick = btnDataValidadeClick
            end
            object btnNumSerie: TButton
              Left = 112
              Top = 266
              Width = 73
              Height = 25
              Caption = 'Num.S'#233'rie'
              TabOrder = 2
              OnClick = btnNumSerieClick
            end
            object btnSubName: TButton
              Left = 8
              Top = 298
              Width = 99
              Height = 25
              Caption = 'Subject Name'
              TabOrder = 3
              OnClick = btnSubNameClick
            end
            object btnCNPJ: TButton
              Left = 112
              Top = 298
              Width = 73
              Height = 25
              Caption = 'CNPJ'
              TabOrder = 4
              OnClick = btnCNPJClick
            end
            object btnIssuerName: TButton
              Left = 188
              Top = 298
              Width = 76
              Height = 25
              Caption = 'Issuer Name'
              TabOrder = 5
              OnClick = btnIssuerNameClick
            end
            object GroupBox1: TGroupBox
              Left = 2
              Top = 328
              Width = 263
              Height = 69
              Caption = 'Calculo de Hash e assinatura'
              TabOrder = 6
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
              Left = 8
              Top = 403
              Width = 128
              Height = 25
              Caption = 'HTTPS sem Certificado'
              TabOrder = 7
              OnClick = btnHTTPSClick
            end
            object btnLeituraX509: TButton
              Left = 144
              Top = 403
              Width = 115
              Height = 25
              Caption = 'Leitura de X509'
              TabOrder = 8
              OnClick = btnLeituraX509Click
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
              object Label32: TLabel
                Left = 8
                Top = 165
                Width = 119
                Height = 13
                Caption = 'Vers'#227'o Documento Fiscal'
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
                Top = 205
                Width = 193
                Height = 17
                Caption = 'Retirar Acentos dos XMLs enviados'
                TabOrder = 6
              end
              object cbVersaoDF: TComboBox
                Left = 8
                Top = 181
                Width = 248
                Height = 21
                TabOrder = 7
              end
              object edtPathSchemas: TEdit
                Left = 8
                Top = 276
                Width = 228
                Height = 21
                TabOrder = 8
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
          object TabSheet13: TTabSheet
            Caption = 'Arquivos'
            ImageIndex = 4
            object sbPathNF3e: TSpeedButton
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
              OnClick = sbPathNF3eClick
            end
            object Label35: TLabel
              Left = 6
              Top = 116
              Width = 100
              Height = 13
              Caption = 'Pasta Arquivos NF3e'
            end
            object Label47: TLabel
              Left = 6
              Top = 162
              Width = 109
              Height = 13
              Caption = 'Pasta Arquivos Evento'
            end
            object sbPathEvento: TSpeedButton
              Left = 247
              Top = 177
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
            object cbxEmissaoPathNF3e: TCheckBox
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
            object edtPathNF3e: TEdit
              Left = 6
              Top = 132
              Width = 235
              Height = 21
              TabOrder = 6
            end
            object edtPathEvento: TEdit
              Left = 6
              Top = 178
              Width = 235
              Height = 21
              TabOrder = 7
            end
            object cbxSepararPorModelo: TCheckBox
              Left = 6
              Top = 96
              Width = 251
              Height = 17
              Caption = 'Separar Arqs pelo Modelo do Documento'
              TabOrder = 8
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
        object rgTipoDANF3e: TRadioGroup
          Left = 8
          Top = 56
          Width = 257
          Height = 49
          Caption = 'DANF3E'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Retrato'
            'Paisagem')
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
        object rgDANF3E: TRadioGroup
          Left = 8
          Top = 361
          Width = 257
          Height = 49
          Caption = 'DANF3E'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Fortes Report'
            'Esc Pos')
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
      Height = 163
      ActivePage = tsEnvios
      Align = alTop
      TabOrder = 0
      object tsEnvios: TTabSheet
        Caption = 'Envios'
        ImageIndex = 2
        object btnCriarEnviar: TButton
          Left = 191
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Criar e Enviar'
          TabOrder = 0
          OnClick = btnCriarEnviarClick
        end
        object btnValidarRegrasNegocio: TButton
          Left = 7
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Validar Regras de Neg'#243'cio'
          TabOrder = 1
          OnClick = btnValidarRegrasNegocioClick
        end
        object btnGerarXML: TButton
          Left = 7
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Gerar NF3e'
          TabOrder = 2
          OnClick = btnGerarXMLClick
        end
        object btnValidarXML: TButton
          Left = 190
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Validar XML'
          TabOrder = 3
          OnClick = btnValidarXMLClick
        end
        object btnEnviarEmail: TButton
          Left = 7
          Top = 102
          Width = 177
          Height = 25
          Caption = 'Enviar NF3e Email'
          TabOrder = 4
          OnClick = btnEnviarEmailClick
        end
        object btnAdicionarProtocolo: TButton
          Left = 7
          Top = 71
          Width = 177
          Height = 25
          Caption = 'Adicionar Protocolo ao XML'
          TabOrder = 5
          OnClick = btnAdicionarProtocoloClick
        end
        object btnCarregarXMLEnviar: TButton
          Left = 374
          Top = 9
          Width = 177
          Height = 25
          Caption = 'Carregar XML e Enviar'
          TabOrder = 6
          OnClick = btnCarregarXMLEnviarClick
        end
        object btnValidarAssinatura: TButton
          Left = 373
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Validar Assinatura'
          TabOrder = 7
          OnClick = btnValidarAssinaturaClick
        end
        object btnImprimirDANF3E: TButton
          Left = 190
          Top = 70
          Width = 177
          Height = 25
          Caption = 'Imprimir DANF3E'
          TabOrder = 8
          OnClick = btnImprimirDANF3EClick
        end
        object btnImprimirDANF3EOffline: TButton
          Left = 373
          Top = 70
          Width = 177
          Height = 25
          Caption = 'Imprimir DANF3E Offline'
          TabOrder = 9
          OnClick = btnImprimirDANF3EOfflineClick
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
          TabOrder = 0
          OnClick = btnConsultarClick
        end
        object btnConsultarChave: TButton
          Left = 8
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Consultar pela Chave'
          TabOrder = 1
          OnClick = btnConsultarChaveClick
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
          TabOrder = 3
          OnClick = btnStatusServClick
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
        object btnImprimirEvento: TButton
          Left = 8
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Imprimir Evento'
          TabOrder = 2
          OnClick = btnImprimirEventoClick
        end
        object btnEnviarEventoEmail: TButton
          Left = 191
          Top = 40
          Width = 177
          Height = 25
          Caption = 'Enviar Evento Email'
          TabOrder = 3
          OnClick = btnEnviarEventoEmailClick
        end
      end
      object tsDistribuicao: TTabSheet
        Caption = 'Distribui'#231#227'o DFe'
        ImageIndex = 5
        object btnDistribuicaoDFe: TButton
          Left = 8
          Top = 9
          Width = 178
          Height = 25
          Caption = 'Distribui'#231#227'o Documentos Fiscais'
          TabOrder = 0
          OnClick = btnDistribuicaoDFeClick
        end
      end
    end
    object pgRespostas: TPageControl
      Left = 1
      Top = 164
      Width = 567
      Height = 447
      ActivePage = TabSheet5
      Align = alClient
      TabOrder = 1
      object TabSheet5: TTabSheet
        Caption = 'Respostas'
        object MemoResp: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 419
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
          Height = 419
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C000000C63900004E2B00000000000000000000000000000000000000000000
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
          Height = 419
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
          Height = 419
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
          Height = 419
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
          Height = 419
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
        end
      end
    end
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
    Left = 433
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
    Left = 334
    Top = 295
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*-nfe.XML'
    Filter = 
      'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|To' +
      'dos os Arquivos (*.*)|*.*'
    Title = 'Selecione a NFe'
    Left = 531
    Top = 295
  end
  object ACBrNF3e1: TACBrNF3e
    MAIL = ACBrMail1
    OnStatusChange = ACBrNF3e1StatusChange
    OnGerarLog = ACBrNF3e1GerarLog
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
    DANF3e = ACBrNF3eDANF3eESCPOS1
    Left = 334
    Top = 238
  end
  object ACBrNF3eDANF3eESCPOS1: TACBrNF3eDANF3eESCPOS
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
    ACBrNF3e = ACBrNF3e1
    TipoDANF3e = tiSemGeracao
    Cancelada = False
    ViaConsumidor = True
    ImprimeNomeFantasia = False
    PosPrinter = ACBrPosPrinter1
    Left = 454
    Top = 238
  end
end
