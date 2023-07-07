object frmACBrNFSe: TfrmACBrNFSe
  Left = 282
  Top = 118
  Caption = 'ACBrNFSeX - Programa Exemplo - Vers'#227'o Delphi'
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
              TabOrder = 4
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
              TabOrder = 5
              OnClick = btnDataValidadeClick
            end
            object btnNumSerie: TButton
              Left = 112
              Top = 266
              Width = 73
              Height = 25
              Caption = 'Num.S'#233'rie'
              TabOrder = 6
              OnClick = btnNumSerieClick
            end
            object btnSubName: TButton
              Left = 8
              Top = 298
              Width = 99
              Height = 25
              Caption = 'Subject Name'
              TabOrder = 7
              OnClick = btnSubNameClick
            end
            object btnCNPJ: TButton
              Left = 112
              Top = 298
              Width = 73
              Height = 25
              Caption = 'CNPJ'
              TabOrder = 8
              OnClick = btnCNPJClick
            end
            object btnIssuerName: TButton
              Left = 188
              Top = 298
              Width = 76
              Height = 25
              Caption = 'Issuer Name'
              TabOrder = 9
              OnClick = btnIssuerNameClick
            end
            object grpCalculo: TGroupBox
              Left = 2
              Top = 328
              Width = 263
              Height = 69
              Caption = 'Calculo de Hash e assinatura'
              TabOrder = 10
              object edtTexto: TEdit
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
              TabOrder = 11
              OnClick = btnHTTPSClick
            end
            object btnLeituraX509: TButton
              Left = 144
              Top = 403
              Width = 115
              Height = 25
              Caption = 'Leitura de X509'
              TabOrder = 12
              OnClick = btnLeituraX509Click
            end
            object cbSSLLib: TComboBox
              Left = 80
              Top = 8
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbSSLLibChange
            end
            object cbCryptLib: TComboBox
              Left = 80
              Top = 35
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 1
              OnChange = cbCryptLibChange
            end
            object cbHttpLib: TComboBox
              Left = 80
              Top = 62
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 2
              OnChange = cbHttpLibChange
            end
            object cbXmlSignLib: TComboBox
              Left = 80
              Top = 89
              Width = 160
              Height = 21
              Style = csDropDownList
              TabOrder = 3
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
              Height = 365
              Caption = 'Geral'
              TabOrder = 0
              object sbtnPathSalvar: TSpeedButton
                Left = 238
                Top = 159
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
              object spPathSchemas: TSpeedButton
                Left = 238
                Top = 235
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
              object Label6: TLabel
                Left = 9
                Top = 220
                Width = 108
                Height = 13
                Caption = 'Schemas do Provedor:'
              end
              object Label49: TLabel
                Left = 8
                Top = 307
                Width = 80
                Height = 13
                Alignment = taRightJustify
                Caption = 'Layout da NFS-e'
                Color = clBtnFace
                ParentColor = False
              end
              object edtPathLogs: TEdit
                Left = 8
                Top = 162
                Width = 228
                Height = 21
                TabOrder = 6
              end
              object chkSalvarGer: TCheckBox
                Left = 8
                Top = 146
                Width = 209
                Height = 15
                Caption = 'Salvar Arquivos de Envio e Resposta'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 5
              end
              object cbFormaEmissao: TComboBox
                Left = 8
                Top = 104
                Width = 248
                Height = 21
                TabOrder = 3
              end
              object cbxAtualizarXML: TCheckBox
                Left = 8
                Top = 16
                Width = 97
                Height = 17
                Caption = 'Atualizar XML'
                TabOrder = 0
              end
              object cbxExibirErroSchema: TCheckBox
                Left = 8
                Top = 32
                Width = 129
                Height = 17
                Caption = 'Exibir Erro Schema'
                TabOrder = 1
              end
              object edtFormatoAlerta: TEdit
                Left = 8
                Top = 66
                Width = 248
                Height = 21
                TabOrder = 2
              end
              object cbxRetirarAcentos: TCheckBox
                Left = 8
                Top = 128
                Width = 193
                Height = 17
                Caption = 'Retirar Acentos dos XMLs enviados'
                TabOrder = 4
              end
              object edtPathSchemas: TEdit
                Left = 8
                Top = 235
                Width = 228
                Height = 21
                TabOrder = 7
              end
              object chkConsultaLoteAposEnvio: TCheckBox
                Left = 9
                Top = 262
                Width = 152
                Height = 17
                Caption = 'Consultar Lote Ap'#243's Envio'
                TabOrder = 8
              end
              object chkConsultaAposCancelar: TCheckBox
                Left = 9
                Top = 282
                Width = 152
                Height = 17
                Caption = 'Consultar Ap'#243's Cancelar'
                TabOrder = 9
              end
              object chkMontarPathSchemas: TCheckBox
                Left = 9
                Top = 199
                Width = 240
                Height = 17
                Caption = 'Montar automaticamente o Path dos Schemas'
                TabOrder = 10
              end
              object cbLayoutNFSe: TComboBox
                Left = 9
                Top = 323
                Width = 160
                Height = 21
                Style = csDropDownList
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
              Height = 253
              Caption = 'WebService'
              TabOrder = 0
              object lTimeOut: TLabel
                Left = 169
                Top = 72
                Width = 40
                Height = 13
                Caption = 'TimeOut'
                Color = clBtnFace
                ParentColor = False
              end
              object lSSLLib1: TLabel
                Left = 21
                Top = 124
                Width = 41
                Height = 13
                Alignment = taRightJustify
                Caption = 'SSLType'
                Color = clBtnFace
                ParentColor = False
              end
              object Label30: TLabel
                Left = 5
                Top = 143
                Width = 30
                Height = 13
                Caption = 'Senha'
              end
              object Label33: TLabel
                Left = 135
                Top = 143
                Width = 36
                Height = 13
                Caption = 'Usu'#225'rio'
              end
              object Label34: TLabel
                Left = 5
                Top = 176
                Width = 67
                Height = 13
                Caption = 'Frase Secreta'
              end
              object Label41: TLabel
                Left = 5
                Top = 210
                Width = 83
                Height = 13
                Caption = 'Chave de Acesso'
              end
              object Label44: TLabel
                Left = 135
                Top = 210
                Width = 106
                Height = 13
                Caption = 'Chave de Autoriza'#231#227'o'
              end
              object cbxVisualizar: TCheckBox
                Left = 10
                Top = 74
                Width = 153
                Height = 17
                Caption = 'Visualizar Mensagem'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
              end
              object rgTipoAmb: TRadioGroup
                Left = 10
                Top = 17
                Width = 249
                Height = 52
                Caption = 'Selecione o Ambiente de Destino'
                Columns = 2
                ItemIndex = 0
                Items.Strings = (
                  'Produ'#231#227'o'
                  'Homologa'#231#227'o')
                TabOrder = 0
              end
              object chkSalvarSOAP: TCheckBox
                Left = 10
                Top = 92
                Width = 153
                Height = 17
                Caption = 'Salvar envelope SOAP'
                TabOrder = 2
              end
              object seTimeOut: TSpinEdit
                Left = 169
                Top = 88
                Width = 66
                Height = 22
                Increment = 10
                MaxValue = 999999
                MinValue = 1000
                TabOrder = 3
                Value = 5000
              end
              object cbSSLType: TComboBox
                Left = 74
                Top = 116
                Width = 160
                Height = 21
                Hint = 'Depende de configura'#231#227'o de  SSL.HttpLib'
                Style = csDropDownList
                TabOrder = 4
                OnChange = cbSSLTypeChange
              end
              object edtSenhaWeb: TEdit
                Left = 5
                Top = 156
                Width = 126
                Height = 21
                PasswordChar = '*'
                TabOrder = 5
              end
              object edtUserWeb: TEdit
                Left = 135
                Top = 156
                Width = 124
                Height = 21
                TabOrder = 6
              end
              object edtFraseSecWeb: TEdit
                Left = 5
                Top = 190
                Width = 254
                Height = 21
                TabOrder = 7
              end
              object edtChaveAcessoWeb: TEdit
                Left = 5
                Top = 224
                Width = 126
                Height = 21
                TabOrder = 8
              end
              object edtChaveAutorizWeb: TEdit
                Left = 135
                Top = 224
                Width = 124
                Height = 21
                TabOrder = 9
              end
            end
            object gbProxy: TGroupBox
              Left = 5
              Top = 334
              Width = 265
              Height = 93
              Caption = 'Proxy'
              TabOrder = 1
              object Label8: TLabel
                Left = 8
                Top = 12
                Width = 22
                Height = 13
                Caption = 'Host'
              end
              object Label9: TLabel
                Left = 208
                Top = 12
                Width = 26
                Height = 13
                Caption = 'Porta'
              end
              object Label10: TLabel
                Left = 8
                Top = 50
                Width = 36
                Height = 13
                Caption = 'Usu'#225'rio'
              end
              object Label11: TLabel
                Left = 138
                Top = 50
                Width = 30
                Height = 13
                Caption = 'Senha'
              end
              object edtProxyHost: TEdit
                Left = 8
                Top = 28
                Width = 193
                Height = 21
                TabOrder = 0
              end
              object edtProxyPorta: TEdit
                Left = 208
                Top = 28
                Width = 50
                Height = 21
                TabOrder = 1
              end
              object edtProxyUser: TEdit
                Left = 8
                Top = 66
                Width = 123
                Height = 21
                TabOrder = 2
              end
              object edtProxySenha: TEdit
                Left = 135
                Top = 66
                Width = 123
                Height = 21
                PasswordChar = '*'
                TabOrder = 3
              end
            end
            object gbxRetornoEnvio: TGroupBox
              Left = 5
              Top = 262
              Width = 265
              Height = 71
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
              Width = 70
              Height = 13
              Caption = 'Insc. Municipal'
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
            object Label21: TLabel
              Left = 8
              Top = 248
              Width = 33
              Height = 13
              Caption = 'Cidade'
            end
            object Label22: TLabel
              Left = 8
              Top = 288
              Width = 13
              Height = 13
              Caption = 'UF'
            end
            object Label20: TLabel
              Left = 63
              Top = 288
              Width = 59
              Height = 13
              Caption = 'C'#243'd. Cidade'
            end
            object Label32: TLabel
              Left = 167
              Top = 288
              Width = 80
              Height = 13
              Caption = 'Total de Cidades'
            end
            object Label43: TLabel
              Left = 8
              Top = 328
              Width = 76
              Height = 13
              Caption = 'CNPJ Prefeitura'
            end
            object Label47: TLabel
              Left = 8
              Top = 372
              Width = 48
              Height = 13
              Caption = 'Provedor:'
            end
            object lblSchemas: TLabel
              Left = 61
              Top = 372
              Width = 65
              Height = 13
              Caption = 'lblSchemas'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label45: TLabel
              Left = 150
              Top = 389
              Width = 37
              Height = 13
              Caption = 'Vers'#227'o:'
            end
            object lblVersaoSchemas: TLabel
              Left = 203
              Top = 390
              Width = 53
              Height = 13
              Caption = 'lblVersao'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label48: TLabel
              Left = 8
              Top = 390
              Width = 37
              Height = 13
              Caption = 'Layout:'
            end
            object lblLayout: TLabel
              Left = 61
              Top = 390
              Width = 52
              Height = 13
              Caption = 'lblLayout'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object edtEmitCNPJ: TEdit
              Left = 8
              Top = 20
              Width = 123
              Height = 21
              TabOrder = 0
            end
            object edtEmitIM: TEdit
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
            object edtEmitUF: TEdit
              Left = 8
              Top = 304
              Width = 35
              Height = 21
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 11
            end
            object edtCodCidade: TEdit
              Left = 63
              Top = 304
              Width = 89
              Height = 21
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 12
            end
            object edtTotalCidades: TEdit
              Left = 167
              Top = 304
              Width = 89
              Height = 21
              TabStop = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 13
            end
            object edtEmitCidade: TEdit
              Left = 27
              Top = 264
              Width = 142
              Height = 21
              TabStop = False
              ReadOnly = True
              TabOrder = 14
              Visible = False
            end
            object cbCidades: TComboBox
              Left = 3
              Top = 264
              Width = 257
              Height = 21
              TabOrder = 10
              Text = 'Selecione uma Cidade'
              OnChange = cbCidadesChange
              Items.Strings = (
                'Alfenas/3101607/MG'
                'Americana/3501608/SP'
                'Ananindeua/1500800/PA'
                'Anapolis/5201108/GO'
                'Aparecida de Goiania/5201405/GO'
                'Apucarana/4101408/PR'
                'Aquiraz/2301000/CE'
                'Aragua'#237'na/1702109/TO'
                'Araraquara/3503208/SP'
                'Araras/3503307/SP'
                'Araucaria/4101804/PR'
                'Araxa/3104007/MG'
                'Arcos/3104205/MG'
                'Assis Chateaubriand/4102000/PR'
                'Bage/4301602/RS'
                'Bambui/3105103/MG'
                'Barbacena/3105608/MG'
                'Bariri/3505203/SP'
                'Barra do Garcas/5101803/MT'
                'Barroso/3105905/MG'
                'Barreiras/2903201/BA'
                'Belford Roxo/3300456/RJ'
                'Belo Horizonte/3106200/MG'
                'Bento Gon'#231'alves/4302105/RS'
                'Bertioga/3506359/SP'
                'Betim/3106705/MG'
                'Boa Vista/1400100/RR'
                'Brusque/4202909/SC'
                'Cachoeiro do Itapemirim/3201209/ES'
                'Campinas/3509502/SP'
                'Campo Grande/5002704/MS'
                'Campos dos Goytacazes/3301009/RJ'
                'Canoas/4304606/RS'
                'Capao Bonito/3510203/SP'
                'Carazinho/4304705/RS'
                'Caruaru/2604106/PE'
                'Cataguases/3115300/MG'
                'Catanduva/3511102/SP'
                'Caxias/2103000/MA'
                'Cedral/3511300/SP'
                'Chapeco/4204202/SC'
                'Colina/3512001/SP'
                'Contagem/3118601/MG'
                'Cotia/3513009/SP'
                'Cornelio Procopio/4106407/PR'
                'Corumba/5003207/MS'
                'Criciuma/4204608/SC'
                'Cruzeiro/3513405/SP'
                'Cuiaba/5103403/MT'
                'Curitiba/4106902/PR'
                'Diadema/3513801/SP'
                'Duque de Caxias/3301702/RJ'
                'Erechim/4307005/RS'
                'Estancia Velha/4307609/RS'
                'Eunapolis/2910727/BA'
                'Fazenda Rio Grande/4107652/PR'
                'Feira de Santana/2910800/BA'
                'Feliz/4308102/RS'
                'Formiga/3126109/MG'
                'Fortaleza/2304400/CE'
                'Foz Do Iguacu/4108304/PR'
                'Franca/3516200/SP'
                'Francisco Beltrao/4108403/PR'
                'Frederico Westphalen/4308508/RS'
                'Frutal/3127107/MG'
                'Gaspar/4205902/SC'
                'Goiania/5208707/GO'
                'Guaiba/4309308/RS'
                'Guanambi/2911709/BA'
                'Guapore/4309407/RS'
                'Guarapari/3202405/ES'
                'Guaratingueta/3518404/SP'
                'Guarulhos/3518800/SP'
                'Hortolandia/3519071/SP'
                'Ibate/3519303/SP'
                'Ijui/4310207/RS'
                'Indaial/4207502/SC'
                'Ipatinga/3131307/MG'
                'Itajai/4208203/SC'
                'Itanhaem/3522109/SP'
                'Itapema/4208302/SC'
                'Itatinga/3523503/SP'
                'Itu/3523909/SP'
                'Ituporanga/4208500/SC'
                'Ivoti/4310801/RS'
                'Jaguariuna/3524709/SP'
                'Jau/3525300/SP'
                'Joacaba/4209003/SC'
                'Joao Pessoa/2507507/PB'
                'Juina/5105150/MT'
                'Juiz de Fora/3136702/MG'
                'Jundiai/3525904/SP'
                'Lages/4209300/SC'
                'Lagoa Santa/3137601/MG'
                'Lavras/3138203/MG'
                'Maceio/2704302/AL'
                'Manaus/1302603/AM'
                'Marechal Deodoro/3704708/AL'
                'Marica/3302700/RJ'
                'Maringa/4115200/PR'
                'Maua/3529401/SP'
                'Mirassol/3530300/SP'
                'Mococa/3530508/SP'
                'Mogi das Cruzes/3530607/SP'
                'Montes Claros/3143302/MG'
                'Muriae/3143906/MG'
                'Natal/2408102/RN'
                'Nova Friburgo/3303401/RJ'
                'Nova Mutum/5106224/MT'
                'Novo Hamburgo/4313409/RS'
                'Niteroi/3303302/RJ'
                'Olimpia/3533908/SP'
                'Palhoca/4211900/SC'
                'Palmas/1721000/TO'
                'Para de Minas/3147105/MG'
                'Paracatu/3147006/MG'
                'Paranagua/4118204/PR'
                'Paranavai/4118402/PR'
                'Parauapebas/15055306/PA'
                'Patrocinio/3148103/MG'
                'Paulinia/3536505/SP'
                'Pelotas/4314407/RS'
                'Pindamonhangaba/3538006/SP'
                'Pinhalzinho/4212908/SC'
                'Ponta Grossa/4119905/PR'
                'Ponte Serrada/4213401/SC'
                'Porto Alegre/4314902/RS'
                'Porto Seguro/2925303/BA'
                'Porto Velho/1100205/RO'
                'Presidente Prudente/3541406/SP'
                'Presidente Venceslau/3541505/SP'
                'Recife/2611606/PE'
                'Registro/3542602/SP'
                'Ribeirao das Neves/3154606/MG'
                'Ribeirao Preto/3543402/SP'
                'Rio Claro/3543907/SP'
                'Rio de Janeiro/3304557/RJ'
                'Rio do Sul/4214805/SC'
                'Rondonopolis/5107602/MT'
                'Salto/3545209/SP'
                'Salvador/2927408/BA'
                'Santa Clara do Sul/4316758/RS'
                'Santa Luzia/3157807/MG'
                'Santo Andre/3547809/SP'
                'Santos/3548500/SP'
                'Sao Bento do Sul/4215802/SC'
                'Sao Bernardo do Campos/3548708/SP'
                'Sao Borja/4318002/RS'
                'Sao Caetano do Sul/3548807/SP'
                'Sao Carlos/3548906/SP'
                'Sao Joao da Boa Vista/3549102/SP'
                'Sao Jose/4216602/SC'
                'Sao Jose do Rio Pardo/3549706/SP'
                'Sao Jose do Rio Preto/3549805/SP'
                'Sao Jose dos Campos/3549904/SP'
                'Sao Jose dos Pinhais/4125506/PR'
                'Sao Leopoldo/4318705/RS'
                'Sao Lourenco do Oeste/4216909/SC'
                'Sao Miguel do Oeste/4217204/SC'
                'Sao Paulo/3550308/SP'
                'Saquarema/3305505/RJ'
                'Schroeder/4217402/SC'
                'Sinop/5107909/MT'
                'Soledade/4320800/RS'
                'Tangara da Serra/5107958/MT'
                'Tatui/3554003/SP'
                'Telemaco Borba/4127106/PR'
                'Tijucas/4218004/SC'
                'Uba/3169901/MG'
                'Uberaba/3170107/MG'
                'Umuarama/4128104/PR'
                'Uniao da Vitoria/4128203/PR'
                'Urussanga/ 4219002/SC'
                'Vargem Grande do Sul/3556404/SP'
                'Varginha/3170701/MG'
                'Varzea Grande/5108402/MT'
                'Vilhena/1100304/RO'
                'Vitoria/3205309/ES'
                'Votuporanga/3557105/SP'
                'Nova Serrana/3145208/MG')
            end
            object edtCNPJPrefeitura: TEdit
              Left = 8
              Top = 344
              Width = 123
              Height = 21
              TabOrder = 15
            end
          end
          object TabSheet13: TTabSheet
            Caption = 'Arquivos'
            ImageIndex = 4
            object sbPathNFSe: TSpeedButton
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
              OnClick = sbPathNFSeClick
            end
            object Label35: TLabel
              Left = 6
              Top = 116
              Width = 100
              Height = 13
              Caption = 'Pasta Arquivos NFSe'
            end
            object chkSalvarArq: TCheckBox
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
            object cbxEmissaoPathNFSe: TCheckBox
              Left = 6
              Top = 48
              Width = 251
              Height = 17
              Caption = 'Salvar Documento pelo campo Data de Emiss'#227'o'
              TabOrder = 3
            end
            object cbxSepararPorCNPJ: TCheckBox
              Left = 6
              Top = 80
              Width = 233
              Height = 17
              Caption = 'Separar Arqs pelo CNPJ do Certificado'
              TabOrder = 4
            end
            object edtPathNFSe: TEdit
              Left = 6
              Top = 132
              Width = 235
              Height = 21
              TabOrder = 5
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
          Width = 121
          Height = 13
          Caption = 'Logo Marca da Prefeitura'
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
        object Label39: TLabel
          Left = 8
          Top = 112
          Width = 178
          Height = 13
          Caption = 'Logo Marca do Prestador de Servi'#231'os'
        end
        object sbtnPrestLogo: TSpeedButton
          Left = 238
          Top = 127
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
          OnClick = sbtnPrestLogoClick
        end
        object Label40: TLabel
          Left = 8
          Top = 151
          Width = 93
          Height = 13
          Caption = 'Nome da Prefeitura'
        end
        object Label46: TLabel
          Left = 8
          Top = 198
          Width = 49
          Height = 13
          Caption = 'Pasta PDF'
        end
        object sbtnPathPDF: TSpeedButton
          Left = 249
          Top = 213
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
          OnClick = sbtnPathPDFClick
        end
        object edtLogoMarca: TEdit
          Left = 8
          Top = 24
          Width = 228
          Height = 21
          TabOrder = 0
        end
        object rgTipoDANFSE: TRadioGroup
          Left = 8
          Top = 56
          Width = 257
          Height = 49
          Caption = 'DANFSE'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Retrato'
            'Paisagem')
          TabOrder = 1
        end
        object edtPrestLogo: TEdit
          Left = 8
          Top = 128
          Width = 228
          Height = 21
          TabOrder = 2
        end
        object edtPrefeitura: TEdit
          Left = 8
          Top = 167
          Width = 249
          Height = 21
          TabOrder = 3
        end
        object edtPathPDF: TEdit
          Left = 8
          Top = 214
          Width = 235
          Height = 21
          TabOrder = 4
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
          Top = 175
          Width = 93
          Height = 13
          Caption = 'Mensagem do Email'
        end
        object Label42: TLabel
          Left = 9
          Top = 307
          Width = 99
          Height = 13
          Caption = 'e-mail do Remetente'
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
          Top = 191
          Width = 249
          Height = 115
          TabOrder = 7
        end
        object edtEmailRemetente: TEdit
          Left = 8
          Top = 323
          Width = 249
          Height = 21
          TabOrder = 8
        end
        object cbEmailTLS: TCheckBox
          Left = 10
          Top = 152
          Width = 199
          Height = 17
          Caption = 'SMTP exige conex'#227'o - TLS'
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
    object pgRespostas: TPageControl
      Left = 1
      Top = 164
      Width = 567
      Height = 447
      ActivePage = TabSheet8
      Align = alClient
      TabOrder = 0
      object TabSheet8: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
        object memoLog: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 419
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Lucida Console'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'XML de Envio'
        ImageIndex = 3
        object WBXmlEnvio: TWebBrowser
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
      object TabSheet6: TTabSheet
        Caption = 'XML de Retorno'
        ImageIndex = 1
        object WBXmlRetorno: TWebBrowser
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
      object TabSheet5: TTabSheet
        Caption = 'XML das Notas'
        object WBXmlNotas: TWebBrowser
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
    end
    object pgcProvedores: TPageControl
      Left = 1
      Top = 1
      Width = 567
      Height = 163
      ActivePage = tsDemais
      Align = alTop
      TabOrder = 1
      object tsDemais: TTabSheet
        Caption = 'Demais Provedores'
        object pgcBotoes: TPageControl
          Left = 0
          Top = 0
          Width = 559
          Height = 135
          ActivePage = tsEnvios
          Align = alClient
          TabOrder = 0
          object tsEnvios: TTabSheet
            Caption = 'Envios'
            ImageIndex = 2
            object btnGerarEnviarLote: TButton
              Left = 1
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Enviar Lote RPS (Ass'#237'ncrono)'
              TabOrder = 3
              OnClick = btnGerarEnviarLoteClick
            end
            object btnGerarEnviarNFSe: TButton
              Left = 184
              Top = 40
              Width = 177
              Height = 25
              Caption = 'Enviar um RPS (Unit'#225'rio)'
              TabOrder = 4
              OnClick = btnGerarEnviarNFSeClick
            end
            object btnGerarEnviarSincrono: TButton
              Left = 367
              Top = 40
              Width = 177
              Height = 25
              Caption = 'Enviar Lote RPS (S'#237'ncrono)'
              TabOrder = 5
              OnClick = btnGerarEnviarSincronoClick
            end
            object btnImprimir: TButton
              Left = 184
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Imprimir DANFSe'
              TabOrder = 1
              OnClick = btnImprimirClick
            end
            object btnEnviaremail: TButton
              Left = 367
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Enviar e-mail'
              TabOrder = 2
              OnClick = btnEnviarEmailClick
            end
            object btnLinkNFSe: TButton
              Left = 184
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Link NFSe'
              TabOrder = 7
              OnClick = btnLinkNFSeClick
            end
            object btnGerarLoteRPS: TButton
              Left = 367
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Gerar Lote RPS'
              TabOrder = 8
              OnClick = btnGerarLoteRPSClick
            end
            object btnSubsNFSe: TButton
              Left = 1
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Substituir NFSe'
              TabOrder = 6
              OnClick = btnSubsNFSeClick
            end
            object btnEmitir: TButton
              Left = 1
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Emitir Nota (Novo)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              OnClick = btnEmitirClick
            end
          end
          object tsConsultas: TTabSheet
            Caption = 'Consultas'
            ImageIndex = 3
            object btnConsultarSitLote: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Consultar Situa'#231#227'o do Lote'
              TabOrder = 0
              OnClick = btnConsultarSitLoteClick
            end
            object btnConsultarLote: TButton
              Left = 186
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Consultar Lote'
              TabOrder = 1
              OnClick = btnConsultarLoteClick
            end
            object btnConsultarNFSeRPS: TButton
              Left = 369
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe por RPS'
              TabOrder = 2
              OnClick = btnConsultarNFSeRPSClick
            end
            object btnConsultarNFSePeriodo: TButton
              Left = 186
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe por Per'#237'odo'
              TabOrder = 4
              OnClick = btnConsultarNFSePeriodoClick
            end
            object btnConsultarNFSePeloNumero: TButton
              Left = 3
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe por numero '
              TabOrder = 3
              OnClick = btnConsultarNFSePeloNumeroClick
            end
            object btnConsultarNFSeFaixa: TButton
              Left = 369
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe por Faixa'
              TabOrder = 5
              OnClick = btnConsultarNFSeFaixaClick
            end
            object btnConsultarNFSeGenerico: TButton
              Left = 3
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe Gen'#233'rico'
              TabOrder = 6
              OnClick = btnConsultarNFSeGenericoClick
            end
            object btnConsultarLinkNFSe: TButton
              Left = 186
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Consultar Link NFSe'
              TabOrder = 7
              OnClick = btnConsultarLinkNFSeClick
            end
          end
          object tsConsServPrest: TTabSheet
            Caption = 'Cons. Serv. Prestados'
            ImageIndex = 4
            object btnConsultarNFSeServicoPrestadoPorNumero: TButton
              Left = 3
              Top = 10
              Width = 238
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Prestado Por N'#250'mero'
              TabOrder = 0
              OnClick = btnConsultarNFSeServicoPrestadoPorNumeroClick
            end
            object btnConsultarNFSeServicoPrestadoPorPeriodo: TButton
              Left = 259
              Top = 10
              Width = 262
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Prestado Por Periodo'
              TabOrder = 1
              OnClick = btnConsultarNFSeServicoPrestadoPorPeriodoClick
            end
            object btnConsultarNFSeServicoPrestadoPorTomador: TButton
              Left = 3
              Top = 41
              Width = 238
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Prestado Por Tomador'
              TabOrder = 2
              OnClick = btnConsultarNFSeServicoPrestadoPorTomadorClick
            end
            object btnConsultarNFSeServicoPrestadoPorIntermediario: TButton
              Left = 259
              Top = 41
              Width = 262
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Prestado Por Intermedi'#225'rio'
              TabOrder = 3
              OnClick = btnConsultarNFSeServicoPrestadoPorIntermediarioClick
            end
          end
          object tsConsServTom: TTabSheet
            Caption = 'Cons. Serv. Tomados'
            ImageIndex = 5
            object btnConsultarNFSeServicoTomadoPorNumero: TButton
              Left = 3
              Top = 10
              Width = 238
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Tomado Por N'#250'mero'
              TabOrder = 0
              OnClick = btnConsultarNFSeServicoTomadoPorNumeroClick
            end
            object btnConsultarNFSeServicoTomadoPorPeriodo: TButton
              Left = 259
              Top = 10
              Width = 262
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Tomado Por Periodo'
              TabOrder = 1
              OnClick = btnConsultarNFSeServicoTomadoPorPeriodoClick
            end
            object btnConsultarNFSeServicoTomadoPorPrestador: TButton
              Left = 3
              Top = 41
              Width = 238
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Tomado Por Prestador'
              TabOrder = 2
              OnClick = btnConsultarNFSeServicoTomadoPorPrestadorClick
            end
            object btnConsultarNFSeServicoTomadoPorIntermediario: TButton
              Left = 259
              Top = 41
              Width = 262
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Tomado Por Intermedi'#225'rio'
              TabOrder = 3
              OnClick = btnConsultarNFSeServicoTomadoPorIntermediarioClick
            end
            object btnConsultarNFSeServicoTomadoPorTomador: TButton
              Left = 3
              Top = 72
              Width = 238
              Height = 25
              Caption = 'Consultar NFSe Servi'#231'o Tomado Por Tomador'
              TabOrder = 4
              OnClick = btnConsultarNFSeServicoTomadoPorTomadorClick
            end
          end
          object tsCancelamento: TTabSheet
            Caption = 'Cancelamento'
            ImageIndex = 4
            object btnCancNFSe: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Cancelar NFSe'
              TabOrder = 0
              OnClick = btnCancNFSeClick
            end
          end
          object tsTeste: TTabSheet
            Caption = 'Teste'
            ImageIndex = 3
            object btnGerarEnviarTeste_SP: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Enviar Lote RPS (Teste)'
              TabOrder = 0
              OnClick = btnGerarEnviarTeste_SPClick
            end
            object mmoObs: TMemo
              Left = 3
              Top = 56
              Width = 540
              Height = 50
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              Lines.Strings = (
                'Somente para os provedores: ISSDSF, Siat e ISSSaoPaulo.')
              ParentFont = False
              ReadOnly = True
              TabOrder = 1
            end
          end
          object tsOutros: TTabSheet
            Caption = 'Outros'
            ImageIndex = 6
            object btnGerarToken: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Gerar Token'
              TabOrder = 0
              OnClick = btnGerarTokenClick
            end
            object btnLerINI: TButton
              Left = 3
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Ler Arq. INI e Gerar Rps'
              TabOrder = 1
              OnClick = btnLerINIClick
            end
          end
        end
      end
      object tsPadraoNacional: TTabSheet
        Caption = 'Provedor Padr'#227'o Nacional'
        ImageIndex = 1
        object pgcBotoes1: TPageControl
          Left = 0
          Top = 0
          Width = 559
          Height = 136
          ActivePage = tsEnvios1
          Align = alTop
          TabOrder = 0
          object tsEnvios1: TTabSheet
            Caption = 'Envios'
            ImageIndex = 2
            object btnImprimirPN: TButton
              Left = 186
              Top = 9
              Width = 177
              Height = 25
              Caption = 'Imprimir DANFSe'
              TabOrder = 1
              OnClick = btnImprimirPNClick
            end
            object btnEnviaremailPN: TButton
              Left = 369
              Top = 9
              Width = 177
              Height = 25
              Caption = 'Enviar e-mail'
              TabOrder = 2
              OnClick = btnEnviaremailPNClick
            end
            object btnEmitirPN: TButton
              Left = 3
              Top = 9
              Width = 177
              Height = 25
              Caption = 'Emitir Nota'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnClick = btnEmitirPNClick
            end
          end
          object tsEventos1: TTabSheet
            Caption = 'Eventos'
            ImageIndex = 7
            object btnEventoCancNFSePN: TButton
              Left = 4
              Top = 13
              Width = 177
              Height = 25
              Caption = 'Cancelar NFSe'
              TabOrder = 0
              OnClick = btnEventoCancNFSePNClick
            end
            object btnEventoCancPorSubPN: TButton
              Left = 187
              Top = 13
              Width = 177
              Height = 25
              Caption = 'Cancelar por Substitui'#231#227'o'
              TabOrder = 1
              OnClick = btnEventoCancPorSubPNClick
            end
            object btnEventoAnaliseCancPN: TButton
              Left = 368
              Top = 13
              Width = 177
              Height = 25
              Caption = 'Analise para Cancelamento'
              TabOrder = 2
              OnClick = btnEventoAnaliseCancPNClick
            end
            object btnEventoConfPrestPN: TButton
              Left = 4
              Top = 44
              Width = 177
              Height = 25
              Caption = 'Confirma'#231#227'o Prestador'
              TabOrder = 3
              OnClick = btnEventoConfPrestPNClick
            end
            object btnEventoConfTomPN: TButton
              Left = 187
              Top = 44
              Width = 177
              Height = 25
              Caption = 'Confirma'#231#227'o Tomador'
              TabOrder = 4
              OnClick = btnEventoConfTomPNClick
            end
            object btnEventoConfInterPN: TButton
              Left = 368
              Top = 43
              Width = 177
              Height = 25
              Caption = 'Confirma'#231#227'o Intermedi'#225'rio'
              TabOrder = 5
              OnClick = btnEventoConfInterPNClick
            end
            object btnEventoRejPrestPN: TButton
              Left = 4
              Top = 75
              Width = 177
              Height = 25
              Caption = 'Rejei'#231#227'o Prestador'
              TabOrder = 6
              OnClick = btnEventoRejPrestPNClick
            end
            object btnEventoRejTomPN: TButton
              Left = 187
              Top = 75
              Width = 177
              Height = 25
              Caption = 'Rejei'#231#227'o Tomador'
              TabOrder = 7
              OnClick = btnEventoRejTomPNClick
            end
            object btnEventoRejInterPN: TButton
              Left = 368
              Top = 75
              Width = 177
              Height = 25
              Caption = 'Rejei'#231#227'o Intermedi'#225'rio'
              TabOrder = 8
              OnClick = btnEventoRejInterPNClick
            end
          end
          object tsConsultas1: TTabSheet
            Caption = 'Consultas'
            ImageIndex = 3
            object btnConsultarDPSPorChavePN: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Consultar DPS por Chave'
              TabOrder = 0
              OnClick = btnConsultarDPSPorChavePNClick
            end
            object btnConsultarNFSePelaChavePN: TButton
              Left = 186
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Consultar NFSe por Chave'
              TabOrder = 1
              OnClick = btnConsultarNFSePelaChavePNClick
            end
            object btnObterPDFdoDANFSEPN: TButton
              Left = 3
              Top = 72
              Width = 177
              Height = 25
              Caption = 'Obter PDF do DANFSE'
              TabOrder = 2
              OnClick = btnObterPDFdoDANFSEPNClick
            end
            object btnConsultarEventoChavPN: TButton
              Left = 3
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Consultar Evento por Chave'
              TabOrder = 3
              OnClick = btnConsultarEventoChavPNClick
            end
            object btnConsultarDFe: TButton
              Left = 186
              Top = 41
              Width = 177
              Height = 25
              Caption = 'Consultar DFe'
              TabOrder = 4
              OnClick = btnConsultarDFeClick
            end
          end
          object tsConsultarParametros: TTabSheet
            Caption = 'Consultar Par'#226'metros Municipais'
            ImageIndex = 3
            object btnConsultarParamMunicConvenio: TButton
              Left = 368
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Conv'#234'nio'
              TabOrder = 0
              OnClick = btnConsultarParamMunicConvenioClick
            end
            object btnConsultarParamMunicAliquota: TButton
              Left = 3
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Aliquota'
              TabOrder = 1
              OnClick = btnConsultarParamMunicAliquotaClick
            end
            object btnConsultarParamMunicHistAliquota: TButton
              Left = 186
              Top = 10
              Width = 177
              Height = 25
              Caption = 'Historico Aliquotas'
              TabOrder = 2
              OnClick = btnConsultarParamMunicHistAliquotaClick
            end
            object btnConsultarParamMunicRegimesEspeciais: TButton
              Left = 3
              Top = 44
              Width = 177
              Height = 25
              Caption = 'Regimes Especiais'
              TabOrder = 3
              OnClick = btnConsultarParamMunicRegimesEspeciaisClick
            end
            object btnConsultarParamMunicRetencoes: TButton
              Left = 186
              Top = 44
              Width = 177
              Height = 25
              Caption = 'Reten'#231#245'es'
              TabOrder = 4
              OnClick = btnConsultarParamMunicRetencoesClick
            end
            object btnConsultarParamMunicBeneficio: TButton
              Left = 369
              Top = 44
              Width = 177
              Height = 25
              Caption = 'Beneficio'
              TabOrder = 5
              OnClick = btnConsultarParamMunicBeneficioClick
            end
          end
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
    Left = 336
    Top = 208
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 336
    Top = 320
  end
  object ACBrNFSeX1: TACBrNFSeX
    MAIL = ACBrMail1
    OnStatusChange = ACBrNFSeX1StatusChange
    OnGerarLog = ACBrNFSeX1GerarLog
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.CodigoMunicipio = 0
    Configuracoes.Geral.Provedor = proNenhum
    Configuracoes.Geral.Versao = ve100
    Configuracoes.Geral.Emitente.DadosEmitente.Email = 'contato@provedor.com.br'
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    DANFSE = ACBrNFSeXDANFSeRL1
    Left = 334
    Top = 265
  end
  object ACBrNFSeXDANFSeRL1: TACBrNFSeXDANFSeRL
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
    ACBrNFSe = ACBrNFSeX1
    Cancelada = False
    Provedor = proNenhum
    TamanhoFonte = 6
    FormatarNumeroDocumentoNFSe = True
    Producao = snSim
    Left = 470
    Top = 265
  end
end
