object Form1: TForm1
  Left = 235
  Top = 113
  Caption = 'ACBrNFe - Demonstra'#231#227'o'
  ClientHeight = 608
  ClientWidth = 1107
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 595
    Align = alLeft
    TabOrder = 0
    object lblColaborador: TLabel
      Left = 18
      Top = 511
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
      OnMouseEnter = lblMouseEnter
      OnMouseLeave = lblMouseLeave
    end
    object lblPatrocinador: TLabel
      Left = 16
      Top = 535
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
      OnMouseEnter = lblMouseEnter
      OnMouseLeave = lblMouseLeave
    end
    object lblDoar1: TLabel
      Left = 21
      Top = 559
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
      OnMouseEnter = lblMouseEnter
      OnMouseLeave = lblMouseLeave
    end
    object lblDoar2: TLabel
      Left = 117
      Top = 575
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
      OnMouseEnter = lblMouseEnter
      OnMouseLeave = lblMouseLeave
    end
    object btnSalvarConfig: TBitBtn
      Left = 70
      Top = 480
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
    object PageControl1: TPageControl
      Left = 8
      Top = 9
      Width = 283
      Height = 465
      ActivePage = TabSheet1
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Configura'#231#245'es'
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 275
          Height = 437
          ActivePage = TabSheet3
          Align = alClient
          MultiLine = True
          TabOrder = 0
          object TabSheet3: TTabSheet
            Caption = 'Certificado'
            object Label43: TLabel
              Left = 9
              Top = 8
              Width = 78
              Height = 13
              Caption = 'Tipo de Emiss'#227'o'
            end
            object Label44: TLabel
              Left = 9
              Top = 51
              Width = 216
              Height = 65
              Caption = 
                'Informe o n'#250'mero de s'#233'rie do certificado'#13#10'Dispon'#237'vel no Internet' +
                ' Explorer no menu'#13#10'Ferramentas - Op'#231#245'es da Internet - Conte'#250'do '#13 +
                #10'Certificados - Exibir - Detalhes - '#13#10'N'#250'mero do certificado'
              Visible = False
            end
            object gbCertificado: TGroupBox
              Left = 2
              Top = 122
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
            object cbTipoEmissao: TComboBox
              Left = 9
              Top = 27
              Width = 238
              Height = 21
              Style = csDropDownList
              TabOrder = 1
              OnChange = cbTipoEmissaoChange
            end
            object Button1: TButton
              Left = 16
              Top = 272
              Width = 99
              Height = 25
              Caption = 'Data de Validade'
              TabOrder = 2
              OnClick = Button1Click
            end
            object Button2: TButton
              Left = 144
              Top = 272
              Width = 99
              Height = 25
              Caption = 'Num.S'#233'rie'
              TabOrder = 3
              OnClick = Button2Click
            end
            object Button3: TButton
              Left = 16
              Top = 304
              Width = 99
              Height = 25
              Caption = 'Subject Name'
              TabOrder = 4
              OnClick = Button3Click
            end
            object Button4: TButton
              Left = 144
              Top = 304
              Width = 99
              Height = 25
              Caption = 'CNPJ'
              TabOrder = 5
              OnClick = Button4Click
            end
            object Edit1: TEdit
              Left = 9
              Top = 344
              Width = 249
              Height = 21
              TabOrder = 6
              Text = '0548133600013704583493000190'
            end
            object Button5: TButton
              Left = 64
              Top = 366
              Width = 99
              Height = 25
              Caption = 'SHA256+RSA'
              TabOrder = 7
              OnClick = Button5Click
            end
            object cbAssinar: TCheckBox
              Left = 176
              Top = 372
              Width = 54
              Height = 19
              Caption = 'Assinar'
              Checked = True
              State = cbChecked
              TabOrder = 8
            end
          end
          object TabSheet4: TTabSheet
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
                Width = 68
                Height = 13
                Caption = 'Formato Alerta'
              end
              object Label30: TLabel
                Left = 8
                Top = 126
                Width = 123
                Height = 13
                Caption = 'Modelo Documento Fiscal'
              end
              object Label32: TLabel
                Left = 8
                Top = 165
                Width = 121
                Height = 13
                Caption = 'Vers'#227'o Documento Fiscal'
              end
              object Label33: TLabel
                Left = 8
                Top = 203
                Width = 75
                Height = 13
                Caption = 'IdToken/IdCSC'
              end
              object Label34: TLabel
                Left = 8
                Top = 241
                Width = 57
                Height = 13
                Caption = 'Token/CSC'
              end
              object Label42: TLabel
                Left = 8
                Top = 336
                Width = 199
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
              object cbModeloDF: TComboBox
                Left = 8
                Top = 142
                Width = 248
                Height = 21
                TabOrder = 6
              end
              object cbxRetirarAcentos: TCheckBox
                Left = 8
                Top = 281
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
                Top = 219
                Width = 248
                Height = 21
                TabOrder = 9
              end
              object edtToken: TEdit
                Left = 8
                Top = 257
                Width = 248
                Height = 21
                TabOrder = 10
              end
              object edtPathSchemas: TEdit
                Left = 8
                Top = 352
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
              Left = 0
              Top = 4
              Width = 265
              Height = 157
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
                Width = 241
                Height = 17
                Caption = 'Salvar envelope SOAP'
                TabOrder = 3
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
            object gbxRetornoEnvio: TGroupBox
              Left = 1
              Top = 167
              Width = 291
              Height = 114
              Caption = 'Retorno de Envio de NFe'
              TabOrder = 2
              object Label36: TLabel
                Left = 102
                Top = 27
                Width = 50
                Height = 13
                Caption = 'Tentativas'
              end
              object Label37: TLabel
                Left = 7
                Top = 67
                Width = 41
                Height = 13
                Caption = 'Intervalo'
              end
              object Label38: TLabel
                Left = 8
                Top = 27
                Width = 43
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
                Left = 102
                Top = 43
                Width = 85
                Height = 21
                TabOrder = 2
              end
              object edtIntervalo: TEdit
                Left = 7
                Top = 83
                Width = 85
                Height = 21
                TabOrder = 3
              end
              object edtAguardar: TEdit
                Left = 8
                Top = 43
                Width = 85
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
          object TabSheet13: TTabSheet
            Caption = 'Arquivos'
            ImageIndex = 4
            object sbPathNFe: TSpeedButton
              Left = 240
              Top = 130
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
            object Label39: TLabel
              Left = 6
              Top = 154
              Width = 142
              Height = 13
              Caption = 'Pasta Arquivos Cancelamento'
            end
            object sbPathCan: TSpeedButton
              Left = 240
              Top = 168
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
              OnClick = sbPathCanClick
            end
            object Label46: TLabel
              Left = 6
              Top = 192
              Width = 192
              Height = 13
              Caption = 'Pasta Arquivos CC-e - Carta de Corre'#231#227'o'
            end
            object sbPathCCe: TSpeedButton
              Left = 240
              Top = 206
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
              OnClick = sbPathCCeClick
            end
            object Label40: TLabel
              Left = 6
              Top = 230
              Width = 127
              Height = 13
              Caption = 'Pasta Arquivos Inutiliza'#231#227'o'
            end
            object sbPathInu: TSpeedButton
              Left = 240
              Top = 244
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
            object Label41: TLabel
              Left = 6
              Top = 268
              Width = 103
              Height = 13
              Caption = 'Pasta Arquivos DPEC'
            end
            object sbPathDPEC: TSpeedButton
              Left = 240
              Top = 282
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
              OnClick = sbPathDPECClick
            end
            object Label47: TLabel
              Left = 6
              Top = 306
              Width = 108
              Height = 13
              Caption = 'Pasta Arquivos Evento'
            end
            object sbPathEvento: TSpeedButton
              Left = 240
              Top = 320
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
            object cbxEmissaoPathNFe: TCheckBox
              Left = 6
              Top = 48
              Width = 233
              Height = 17
              Caption = 'Salvar NFe pelo campo Data de Emiss'#227'o'
              TabOrder = 3
            end
            object cbxSalvaPathEvento: TCheckBox
              Left = 6
              Top = 64
              Width = 233
              Height = 17
              Caption = 'Salvar Arqs de Eventos'
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
            object edtPathCCe: TEdit
              Left = 6
              Top = 208
              Width = 235
              Height = 21
              TabOrder = 6
            end
            object edtPathNFe: TEdit
              Left = 6
              Top = 132
              Width = 235
              Height = 21
              TabOrder = 7
            end
            object edtPathCan: TEdit
              Left = 6
              Top = 170
              Width = 235
              Height = 21
              TabOrder = 8
            end
            object edtPathInu: TEdit
              Left = 6
              Top = 246
              Width = 235
              Height = 21
              TabOrder = 9
            end
            object edtPathDPEC: TEdit
              Left = 6
              Top = 284
              Width = 235
              Height = 21
              TabOrder = 10
            end
            object edtPathEvento: TEdit
              Left = 6
              Top = 322
              Width = 235
              Height = 21
              TabOrder = 11
            end
            object cbxSepararPorModelo: TCheckBox
              Left = 6
              Top = 96
              Width = 251
              Height = 17
              Caption = 'Separar Arqs pelo Modelo do Documento'
              TabOrder = 12
            end
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'DANFe'
        ImageIndex = 1
        object Label7: TLabel
          Left = 8
          Top = 8
          Width = 57
          Height = 13
          Caption = 'Logo Marca'
        end
        object sbtnLogoMarca: TSpeedButton
          Left = 235
          Top = 20
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
          Width = 249
          Height = 49
          Caption = 'DANFE'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Retrato'
            'Paisagem')
          TabOrder = 1
        end
      end
      object TabSheet14: TTabSheet
        Caption = 'Email'
        ImageIndex = 2
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 72
          Height = 13
          Caption = 'Servidor SMTP'
        end
        object Label4: TLabel
          Left = 206
          Top = 8
          Width = 25
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
          Width = 31
          Height = 13
          Caption = 'Senha'
        end
        object Label27: TLabel
          Left = 8
          Top = 88
          Width = 121
          Height = 13
          Caption = 'Assunto do email enviado'
        end
        object Label28: TLabel
          Left = 8
          Top = 160
          Width = 95
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
  end
  object Panel2: TPanel
    Left = 297
    Top = 0
    Width = 810
    Height = 595
    Align = alClient
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 808
      Height = 300
      Align = alTop
      TabOrder = 0
      object PageControl3: TPageControl
        Left = 1
        Top = 1
        Width = 806
        Height = 298
        ActivePage = tsNFe
        Align = alClient
        TabOrder = 0
        object tsNFe: TTabSheet
          Caption = 'NF-e'
          object btnImprimir: TButton
            Left = 192
            Top = 156
            Width = 177
            Height = 25
            Caption = 'Imprimir DANFE'
            TabOrder = 0
            OnClick = btnImprimirClick
          end
          object btnConsultar: TButton
            Left = 8
            Top = 66
            Width = 177
            Height = 25
            Caption = 'Consultar carregando XML'
            TabOrder = 1
            OnClick = btnConsultarClick
          end
          object btnValidarXML: TButton
            Left = 376
            Top = 126
            Width = 177
            Height = 25
            Caption = 'Validar XML'
            TabOrder = 2
            OnClick = btnValidarXMLClick
          end
          object btnStatusServ: TButton
            Left = 9
            Top = 6
            Width = 177
            Height = 25
            Caption = ' Status de Servi'#231'o'
            TabOrder = 3
            OnClick = btnStatusServClick
          end
          object btnCancNF: TButton
            Left = 9
            Top = 125
            Width = 177
            Height = 25
            Caption = 'Cancelamento NFe com XML'
            TabOrder = 4
            OnClick = btnCancNFClick
          end
          object btnCriarEnviar: TButton
            Left = 8
            Top = 36
            Width = 177
            Height = 25
            Caption = 'Criar e Enviar'
            TabOrder = 5
            OnClick = btnCriarEnviarClick
          end
          object btnInutilizar: TButton
            Left = 192
            Top = 6
            Width = 177
            Height = 25
            Caption = 'Inutilizar Numera'#231#227'o'
            TabOrder = 6
            OnClick = btnInutilizarClick
          end
          object btnGerarNFE: TButton
            Left = 192
            Top = 96
            Width = 177
            Height = 25
            Caption = 'Gerar NFe'
            TabOrder = 7
            OnClick = btnGerarNFEClick
          end
          object btnConsCad: TButton
            Left = 193
            Top = 66
            Width = 177
            Height = 25
            Caption = 'Consulta Cadastro'
            TabOrder = 8
            OnClick = btnConsCadClick
          end
          object btnGerarPDF: TButton
            Left = 192
            Top = 126
            Width = 177
            Height = 25
            Caption = 'Gerar PDF'
            TabOrder = 9
            OnClick = btnGerarPDFClick
          end
          object btnEnviarEmail: TButton
            Left = 376
            Top = 156
            Width = 177
            Height = 25
            Caption = 'Enviar NFe Email'
            TabOrder = 10
            OnClick = btnEnviarEmailClick
          end
          object btnConsultarRecibo: TButton
            Left = 192
            Top = 36
            Width = 177
            Height = 25
            Caption = 'Consultar Recibo Lote'
            TabOrder = 11
            OnClick = btnConsultarReciboClick
          end
          object btnImportarXML: TButton
            Left = 375
            Top = 95
            Width = 177
            Height = 25
            Caption = 'Importar TXT/XML'
            TabOrder = 12
            OnClick = btnImportarXMLClick
          end
          object btnConsultarChave: TButton
            Left = 8
            Top = 96
            Width = 177
            Height = 25
            Caption = 'Consultar pela Chave'
            TabOrder = 13
            OnClick = btnConsultarChaveClick
          end
          object btnCancelarChave: TButton
            Left = 9
            Top = 156
            Width = 177
            Height = 25
            Caption = 'Cancelamento NFe pela Chave'
            TabOrder = 14
            OnClick = btnCancelarChaveClick
          end
          object btnGerarTXT: TButton
            Left = 376
            Top = 66
            Width = 177
            Height = 25
            Caption = 'Gerar TXT'
            TabOrder = 15
            OnClick = btnGerarTXTClick
          end
          object btnAdicionarProtNFe: TButton
            Left = 9
            Top = 184
            Width = 177
            Height = 25
            Caption = 'Adicionar nfeProc ao XML'
            TabOrder = 16
            OnClick = btnAdicionarProtNFeClick
          end
          object btnCarregarXMLEnviar: TButton
            Left = 192
            Top = 184
            Width = 177
            Height = 25
            Caption = 'Carregar XML e Enviar'
            TabOrder = 17
            OnClick = btnCarregarXMLEnviarClick
          end
          object btnCartadeCorrecao: TButton
            Left = 376
            Top = 184
            Width = 177
            Height = 25
            Caption = 'Carta de Corre'#231#227'o'
            TabOrder = 18
            OnClick = btnCartadeCorrecaoClick
          end
          object btnValidarAssinatura: TButton
            Left = 9
            Top = 212
            Width = 177
            Height = 25
            Caption = 'Validar Assinatura'
            TabOrder = 19
            OnClick = btnValidarAssinaturaClick
          end
          object btnManifDestConfirmacao: TButton
            Left = 192
            Top = 212
            Width = 178
            Height = 25
            Caption = 'Manif. Dest. - Conf. Opera'#231#227'o'
            TabOrder = 20
            OnClick = btnManifDestConfirmacaoClick
          end
          object btnNfeDestinadas: TButton
            Left = 375
            Top = 211
            Width = 178
            Height = 25
            Caption = 'Consulta NFe Destinadas'
            TabOrder = 21
            OnClick = btnNfeDestinadasClick
          end
          object btnImprimirCCe: TButton
            Left = 8
            Top = 240
            Width = 177
            Height = 25
            Caption = 'Imprimir Evento'
            TabOrder = 22
            OnClick = btnImprimirCCeClick
          end
          object btnEnviarEvento: TButton
            Left = 193
            Top = 240
            Width = 177
            Height = 25
            Caption = 'Enviar Evento Email'
            TabOrder = 23
            OnClick = btnEnviarEventoClick
          end
          object btnDistribuicaoDFe: TButton
            Left = 375
            Top = 240
            Width = 178
            Height = 25
            Caption = 'Distribui'#231#227'o Documentos Fiscais'
            TabOrder = 24
            OnClick = btnDistribuicaoDFeClick
          end
          object btnInutilizarImprimir: TButton
            Left = 375
            Top = 6
            Width = 177
            Height = 25
            Caption = 'Inutilizar Imprimir'
            TabOrder = 25
            OnClick = btnInutilizarImprimirClick
          end
        end
        object tsNFCe: TTabSheet
          Caption = 'NFC-e'
          ImageIndex = 1
          object btnCriarEnviarNFCe: TButton
            Left = 9
            Top = 6
            Width = 177
            Height = 25
            Caption = 'Criar e Enviar'
            TabOrder = 0
            OnClick = btnCriarEnviarNFCeClick
          end
        end
        object tsNFCETef: TTabSheet
          Caption = 'NFC-e TEFD'
          ImageIndex = 2
          object Panel4: TPanel
            Left = 0
            Top = 243
            Width = 798
            Height = 27
            Align = alBottom
            BevelInner = bvLowered
            TabOrder = 0
            object sECF: TShape
              Left = 8
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object lECFName: TLabel
              Left = 31
              Top = 8
              Width = 50
              Height = 13
              Caption = 'lECFName'
              Color = clBtnFace
              ParentColor = False
            end
            object sTEFDial: TShape
              Left = 97
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object sTEFDisc: TShape
              Left = 193
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object sHiperTEF: TShape
              Left = 289
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object sCliSiTef: TShape
              Left = 393
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object sVSPague: TShape
              Left = 481
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object sAuttar: TShape
              Left = 579
              Top = 7
              Width = 17
              Height = 16
              Brush.Color = clRed
              Shape = stCircle
            end
            object ckTEFDIAL: TCheckBox
              Left = 121
              Top = 5
              Width = 69
              Height = 19
              Caption = 'TEF_DIAL'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object ckTEFDISC: TCheckBox
              Left = 217
              Top = 6
              Width = 69
              Height = 19
              Caption = 'TEF_DISC'
              TabOrder = 1
            end
            object ckHIPERTEF: TCheckBox
              Left = 313
              Top = 7
              Width = 76
              Height = 19
              Caption = 'HIPER_TEF'
              TabOrder = 2
            end
            object ckCliSiTef: TCheckBox
              Left = 417
              Top = 6
              Width = 60
              Height = 19
              Caption = 'CliSiTef'
              TabOrder = 3
            end
            object bCancelarResp: TButton
              Left = 650
              Top = 3
              Width = 88
              Height = 23
              Caption = 'CancelarResp'
              TabOrder = 4
              Visible = False
            end
            object ckVSPague: TCheckBox
              Left = 505
              Top = 5
              Width = 72
              Height = 19
              Caption = 'VeSPague'
              TabOrder = 5
            end
            object ckAuttar: TCheckBox
              Left = 603
              Top = 6
              Width = 53
              Height = 19
              Caption = 'Auttar'
              TabOrder = 6
            end
          end
          object PageControl2: TPageControl
            Left = 0
            Top = 0
            Width = 798
            Height = 244
            ActivePage = tsConfig
            Align = alTop
            TabOrder = 1
            object tsConfig: TTabSheet
              Caption = 'Configura'#231#227'o'
              object Panel5: TPanel
                Left = 0
                Top = 0
                Width = 790
                Height = 216
                Align = alClient
                TabOrder = 0
                object gbConfigTEF: TGroupBox
                  Left = 1
                  Top = 1
                  Width = 788
                  Height = 214
                  Align = alClient
                  Caption = 'TEF'
                  TabOrder = 0
                  object Label52: TLabel
                    Left = 42
                    Top = 24
                    Width = 83
                    Height = 13
                    Caption = 'Selecionar o G.P.'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label53: TLabel
                    Left = 317
                    Top = 23
                    Width = 60
                    Height = 13
                    Caption = 'EsperaSleep'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object Label54: TLabel
                    Left = 317
                    Top = 79
                    Width = 54
                    Height = 13
                    Caption = 'EsperaSTS'
                    Color = clBtnFace
                    ParentColor = False
                  end
                  object bInicializar: TButton
                    Left = 21
                    Top = 79
                    Width = 133
                    Height = 25
                    Caption = 'Inicializar'
                    TabOrder = 1
                    OnClick = bInicializarClick
                  end
                  object ckAutoAtivar: TCheckBox
                    Left = 169
                    Top = 34
                    Width = 104
                    Height = 19
                    Caption = 'Auto Ativar G.P.'
                    Checked = True
                    State = cbChecked
                    TabOrder = 3
                    OnClick = ckAutoAtivarClick
                  end
                  object cbxGP: TComboBox
                    Left = 21
                    Top = 44
                    Width = 133
                    Height = 21
                    Style = csDropDownList
                    TabOrder = 0
                    OnChange = cbxGPChange
                  end
                  object bAtivarGP: TButton
                    Left = 21
                    Top = 110
                    Width = 133
                    Height = 25
                    Caption = 'Ativar GP'
                    TabOrder = 2
                    OnClick = bAtivarGPClick
                  end
                  object ckMultiplosCartoes: TCheckBox
                    Left = 169
                    Top = 62
                    Width = 113
                    Height = 19
                    Caption = 'Multiplos Cart'#245'es'
                    Checked = True
                    State = cbChecked
                    TabOrder = 4
                    OnClick = ckMultiplosCartoesClick
                  end
                  object ckAutoFinalizarCupom: TCheckBox
                    Left = 169
                    Top = 113
                    Width = 129
                    Height = 19
                    Caption = 'AutoFinalizarCupom'
                    TabOrder = 6
                    OnClick = ckAutoFinalizarCupomClick
                  end
                  object ckAutoEfetuarPagamento: TCheckBox
                    Left = 169
                    Top = 88
                    Width = 144
                    Height = 19
                    Caption = 'AutoEfetuarPagamento'
                    TabOrder = 5
                    OnClick = ckAutoEfetuarPagamentoClick
                  end
                  object edEsperaSleep: TEdit
                    Left = 317
                    Top = 39
                    Width = 56
                    Height = 21
                    TabOrder = 7
                    Text = '250'
                    OnChange = edEsperaSleepChange
                  end
                  object edEsperaSTS: TEdit
                    Left = 317
                    Top = 95
                    Width = 56
                    Height = 21
                    TabOrder = 8
                    Text = '7'
                    OnChange = edEsperaSTSChange
                  end
                end
              end
            end
            object tsOperacao: TTabSheet
              Caption = 'Opera'#231#227'o'
              object gbCupomECF: TGroupBox
                Left = 0
                Top = 0
                Width = 449
                Height = 216
                Align = alLeft
                Caption = 'ECF'
                TabOrder = 0
                object GroupBoxDadosDaVenda: TGroupBox
                  Left = 3
                  Top = 13
                  Width = 214
                  Height = 103
                  Caption = 'Dados da Venda'
                  TabOrder = 0
                  object Label50: TLabel
                    Left = 91
                    Top = 18
                    Width = 117
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Valor total da Venda'
                    Color = clBtnFace
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentColor = False
                    ParentFont = False
                  end
                  object Label45: TLabel
                    Left = 9
                    Top = 18
                    Width = 65
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'N'#250'm. NFCe'
                    Color = clBtnFace
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentColor = False
                    ParentFont = False
                  end
                  object edValorVenda: TEdit
                    Left = 87
                    Top = 37
                    Width = 121
                    Height = 25
                    AutoSize = False
                    TabOrder = 0
                  end
                  object EditNumNFCe: TEdit
                    Left = 9
                    Top = 37
                    Width = 75
                    Height = 25
                    AutoSize = False
                    Enabled = False
                    TabOrder = 1
                    Text = '0'
                  end
                  object ButtonIniciaVenda: TButton
                    Left = 8
                    Top = 65
                    Width = 113
                    Height = 31
                    Caption = 'Iniciar Venda'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsBold]
                    ParentFont = False
                    TabOrder = 2
                    OnClick = ButtonIniciaVendaClick
                  end
                  object ButtonCancelarVenda: TButton
                    Left = 133
                    Top = 65
                    Width = 75
                    Height = 30
                    Caption = 'Cancelar'
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -11
                    Font.Name = 'MS Sans Serif'
                    Font.Style = [fsStrikeOut]
                    ParentFont = False
                    TabOrder = 3
                    OnClick = ButtonCancelarVendaClick
                  end
                end
                object GroupBoxFechamento: TGroupBox
                  Left = 3
                  Top = 122
                  Width = 214
                  Height = 79
                  Caption = 'Demostrativo Pagamento'
                  TabOrder = 1
                  object labelDescricaoTotalRecebido: TLabel
                    Left = 14
                    Top = 17
                    Width = 99
                    Height = 16
                    Caption = 'Total Recebido:'
                    Font.Charset = ANSI_CHARSET
                    Font.Color = clGreen
                    Font.Height = -13
                    Font.Name = 'Arial Narrow'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object Label51: TLabel
                    Left = 13
                    Top = 51
                    Width = 96
                    Height = 16
                    Caption = 'SaldoRestante:'
                    Font.Charset = ANSI_CHARSET
                    Font.Color = clGreen
                    Font.Height = -13
                    Font.Name = 'Arial Narrow'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object EditTotalPago: TEdit
                    Left = 110
                    Top = 16
                    Width = 91
                    Height = 25
                    AutoSize = False
                    Enabled = False
                    TabOrder = 0
                    Text = '0'
                  end
                  object EditSaldoRestante: TEdit
                    Left = 110
                    Top = 47
                    Width = 91
                    Height = 25
                    AutoSize = False
                    Enabled = False
                    TabOrder = 1
                    Text = '0'
                  end
                end
                object GroupBox1: TGroupBox
                  Left = 223
                  Top = 13
                  Width = 214
                  Height = 188
                  Caption = 'Compo pagar?'
                  TabOrder = 2
                  object Label55: TLabel
                    Left = 7
                    Top = 15
                    Width = 165
                    Height = 22
                    Caption = 'Valor Pagamento:'
                    Font.Charset = ANSI_CHARSET
                    Font.Color = clBlue
                    Font.Height = -19
                    Font.Name = 'Arial Narrow'
                    Font.Style = [fsBold]
                    ParentFont = False
                  end
                  object edValorPagamento: TEdit
                    Left = 7
                    Top = 40
                    Width = 138
                    Height = 43
                    AutoSize = False
                    Font.Charset = ANSI_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -24
                    Font.Name = 'Arial Narrow'
                    Font.Style = [fsBold]
                    ParentFont = False
                    TabOrder = 0
                  end
                  object ButtonPagarEmCartao: TButton
                    Left = 106
                    Top = 116
                    Width = 97
                    Height = 25
                    Caption = 'Pagar em Cartao'
                    TabOrder = 1
                    OnClick = ButtonPagarEmCartaoClick
                  end
                  object ButtonPagarEmDinheiro: TButton
                    Left = 3
                    Top = 116
                    Width = 97
                    Height = 25
                    Caption = 'Pagar em Dinheiro'
                    TabOrder = 2
                    OnClick = ButtonPagarEmDinheiroClick
                  end
                end
              end
              object gbComandosTEF: TGroupBox
                Left = 449
                Top = 0
                Width = 341
                Height = 216
                Align = alClient
                Caption = 'TEF'
                TabOrder = 1
                object bADM: TButton
                  Left = 179
                  Top = 13
                  Width = 159
                  Height = 25
                  Caption = 'Administrativo TEF (ADM)'
                  TabOrder = 1
                  OnClick = bADMClick
                end
                object cbxGP1: TComboBox
                  Left = 14
                  Top = 15
                  Width = 142
                  Height = 21
                  Style = csDropDownList
                  TabOrder = 0
                end
              end
            end
          end
        end
      end
    end
    object pgRespostas: TPageControl
      Left = 1
      Top = 301
      Width = 808
      Height = 293
      ActivePage = TabSheetCupomTef
      Align = alClient
      TabOrder = 1
      object TabSheet5: TTabSheet
        Caption = 'Respostas'
        object MemoResp: TMemo
          Left = 0
          Top = 0
          Width = 800
          Height = 265
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
          Width = 800
          Height = 265
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C000000AF520000631B00000000000000000000000000000000000000000000
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
          Width = 800
          Height = 265
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'NFe'
        ImageIndex = 3
        object trvwNFe: TTreeView
          Left = 0
          Top = 0
          Width = 800
          Height = 265
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
          Width = 800
          Height = 265
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
          Width = 800
          Height = 265
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object TabSheet11: TTabSheet
        Caption = 'RetornoConsulta NFe 2.01'
        ImageIndex = 6
        object TreeViewRetornoConsulta: TTreeView
          Left = 0
          Top = 0
          Width = 800
          Height = 265
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object TabSheetCupomTef: TTabSheet
        Caption = 'Cupom TEF'
        ImageIndex = 7
        object MemoCupomTEF: TMemo
          Left = 0
          Top = 0
          Width = 428
          Height = 265
          Align = alLeft
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object pMensagem: TPanel
          Left = 428
          Top = 0
          Width = 372
          Height = 265
          Align = alRight
          BevelInner = bvLowered
          BevelWidth = 2
          BorderStyle = bsSingle
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          Visible = False
          object pMensagemOperador: TPanel
            Left = 4
            Top = 4
            Width = 360
            Height = 119
            Align = alClient
            TabOrder = 0
            Visible = False
            object lMensagemOperador: TLabel
              Left = 1
              Top = 1
              Width = 358
              Height = 117
              Align = alClient
              Alignment = taCenter
              Caption = 'lMensagemOperador'
              Color = clBtnFace
              ParentColor = False
              Layout = tlCenter
              WordWrap = True
            end
            object Label48: TLabel
              Left = 0
              Top = 0
              Width = 118
              Height = 13
              Caption = 'Mensagem Operador'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
          end
          object pMensagemCliente: TPanel
            Left = 4
            Top = 123
            Width = 360
            Height = 134
            Align = alBottom
            TabOrder = 1
            Visible = False
            object Label49: TLabel
              Left = 0
              Top = 0
              Width = 104
              Height = 13
              Caption = 'Mensagem Cliente'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
            end
            object lMensagemCliente: TLabel
              Left = 1
              Top = 1
              Width = 358
              Height = 132
              Align = alClient
              Alignment = taCenter
              Caption = 'lMensagemCliente'
              Color = clBtnFace
              ParentColor = False
              Layout = tlCenter
              WordWrap = True
            end
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 595
    Width = 1107
    Height = 13
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*-nfe.XML'
    Filter = 
      'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|To' +
      'dos os Arquivos (*.*)|*.*'
    Title = 'Selecione a NFe'
    Left = 688
    Top = 376
  end
  object ACBrNFe1: TACBrNFe
    MAIL = ACBrMail1
    OnStatusChange = ACBrNFe1StatusChange
    Configuracoes.Geral.SSLLib = libCapicom
    Configuracoes.Geral.SSLCryptLib = cryCapicom
    Configuracoes.Geral.SSLHttpLib = httpWinINet
    Configuracoes.Geral.SSLXmlSignLib = xsMsXmlCapicom
    Configuracoes.Geral.FormaEmissao = teContingencia
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.VersaoDF = ve200
    Configuracoes.Geral.VersaoQRCode = veqr000
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'GO'
    Configuracoes.WebServices.AguardarConsultaRet = 15000
    Configuracoes.WebServices.AjustaAguardaConsultaRet = True
    Configuracoes.WebServices.QuebradeLinha = '|'
    DANFE = ACBrNFeDANFeRL1
    Left = 445
    Top = 369
  end
  object ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS
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
    Left = 556
    Top = 458
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    Left = 555
    Top = 380
  end
  object ACBrTEFD1: TACBrTEFD
    Identificacao.NomeAplicacao = 'TEFDDemo'
    Identificacao.VersaoAplicacao = '3.0'
    Identificacao.SoftwareHouse = 'ACBr'
    Identificacao.RazaoSocial = 'Projeto ACBr'
    MultiplosCartoes = True
    NumeroMaximoCartoes = 3
    AutoFinalizarCupom = False
    EsperaSTS = 7
    CHQEmGerencial = True
    TEFDial.ArqLOG = 'TEF_DIAL.log'
    TEFDial.Habilitado = True
    TEFDial.ArqTemp = 'C:\TEF_DIAL\req\intpos.tmp'
    TEFDial.ArqReq = 'C:\TEF_DIAL\req\intpos.001'
    TEFDial.ArqSTS = 'C:\TEF_DIAL\resp\intpos.sts'
    TEFDial.ArqResp = 'C:\TEF_DIAL\resp\intpos.001'
    TEFDial.GPExeName = 'C:\TEF_DIAL\tef_dial.exe'
    TEFDisc.ArqTemp = 'C:\TEF_Disc\req\intpos.tmp'
    TEFDisc.ArqReq = 'C:\TEF_Disc\req\intpos.001'
    TEFDisc.ArqSTS = 'C:\TEF_Disc\resp\intpos.sts'
    TEFDisc.ArqResp = 'C:\TEF_Disc\resp\intpos.001'
    TEFDisc.GPExeName = 'C:\TEF_Disc\tef_Disc.exe'
    TEFHiper.ArqTemp = 'c:\HiperTEF\req\IntPos.tmp'
    TEFHiper.ArqReq = 'C:\HiperTEF\req\IntPos.001'
    TEFHiper.ArqSTS = 'C:\HiperTEF\resp\IntPos.sts'
    TEFHiper.ArqResp = 'C:\HiperTEF\resp\IntPos.001'
    TEFHiper.GPExeName = 'C:\HiperTEF\HiperTEF.exe'
    TEFCliSiTef.ArqLOG = 'CliSiTef.log'
    TEFCliSiTef.EnderecoIP = '127.0.0.1'
    TEFCliSiTef.CodigoLoja = '00000000'
    TEFCliSiTef.NumeroTerminal = 'SE000001'
    TEFCliSiTef.OnExibeMenu = ACBrTEFD1CliSiTefExibeMenu
    TEFCliSiTef.OnObtemCampo = ACBrTEFD1CliSiTefObtemCampo
    TEFVeSPague.ArqLOG = 'VeSPague.log'
    TEFVeSPague.Aplicacao = 'ACBr_TEFDDemo'
    TEFVeSPague.AplicacaoVersao = '1.0'
    TEFVeSPague.GPExeName = 'C:\VeSPague\Client\VeSPagueClient.bat'
    TEFVeSPague.GPExeParams = '189.115.24.32 65432'
    TEFVeSPague.EnderecoIP = 'localhost'
    TEFVeSPague.Porta = '60906'
    TEFVeSPague.TimeOut = 500
    TEFVeSPague.TemPendencias = False
    TEFVeSPague.TransacaoCRT = 'Cartao Vender'
    TEFVeSPague.TransacaoCHQ = 'Cheque Consultar'
    TEFVeSPague.TransacaoCNC = 'Administracao Cancelar'
    TEFVeSPague.TransacaoReImpressao = 'Administracao Reimprimir'
    TEFVeSPague.TransacaoPendente = 'Administracao Pendente'
    TEFGPU.ArqTemp = 'C:\TEF_GPU\req\intpos.tmp'
    TEFGPU.ArqReq = 'C:\TEF_GPU\req\intpos.001'
    TEFGPU.ArqSTS = 'C:\TEF_GPU\resp\intpos.sts'
    TEFGPU.ArqResp = 'C:\TEF_GPU\resp\intpos.001'
    TEFGPU.GPExeName = 'C:\TEF_GPU\GPU.exe'
    TEFBanese.ArqTemp = 'C:\bcard\req\pergunta.tmp'
    TEFBanese.ArqReq = 'C:\bcard\req\pergunta.txt'
    TEFBanese.ArqSTS = 'C:\bcard\resp\status.txt'
    TEFBanese.ArqResp = 'C:\bcard\resp\resposta.txt'
    TEFBanese.ArqRespBkp = 'C:\bcard\resposta.txt'
    TEFBanese.ArqRespMovBkp = 'C:\bcard\copiamovimento.txt'
    TEFAuttar.ArqTemp = 'C:\Auttar_TefIP\req\intpos.tmp'
    TEFAuttar.ArqReq = 'C:\Auttar_TefIP\req\intpos.001'
    TEFAuttar.ArqSTS = 'C:\Auttar_TefIP\resp\intpos.sts'
    TEFAuttar.ArqResp = 'C:\Auttar_TefIP\resp\intpos.001'
    TEFAuttar.GPExeName = 'C:\Program Files (x86)\Auttar\IntegradorTEF-IP.exe'
    TEFGood.ArqTemp = 'C:\good\gettemp.dat'
    TEFGood.ArqReq = 'C:\good\getreq.dat'
    TEFGood.ArqSTS = 'C:\good\getstat.dat'
    TEFGood.ArqResp = 'C:\good\getresp.dat'
    TEFGood.GPExeName = 'C:\good\GETGoodMed.exe'
    TEFFoxWin.ArqTemp = 'C:\FwTEF\req\intpos.tmp'
    TEFFoxWin.ArqReq = 'C:\FwTEF\req\intpos.001'
    TEFFoxWin.ArqSTS = 'C:\FwTEF\rsp\intpos.sts'
    TEFFoxWin.ArqResp = 'C:\FwTEF\rsp\intpos.001'
    TEFFoxWin.GPExeName = 'C:\FwTEF\bin\FwTEF.exe'
    TEFCliDTEF.ArqResp = ''
    TEFPetrocard.ArqTemp = 'C:\CardTech\req\intpos.tmp'
    TEFPetrocard.ArqReq = 'C:\CardTech\req\intpos.001'
    TEFPetrocard.ArqSTS = 'C:\CardTech\resp\intpos.sts'
    TEFPetrocard.ArqResp = 'C:\CardTech\resp\intpos.001'
    TEFPetrocard.GPExeName = 'C:\CardTech\sac.exe'
    TEFCrediShop.ArqTemp = 'C:\tef_cshp\req\intpos.tmp'
    TEFCrediShop.ArqReq = 'C:\tef_cshp\req\intpos.001'
    TEFCrediShop.ArqSTS = 'C:\tef_cshp\resp\intpos.sts'
    TEFCrediShop.ArqResp = 'C:\tef_cshp\resp\intpos.001'
    TEFCrediShop.GPExeName = 'C:\tef_cshp\vpos_tef.exe'
    TEFTicketCar.ArqTemp = 'C:\TCS\TX\INTTCS.tmp'
    TEFTicketCar.ArqReq = 'C:\TCS\TX\INTTCS.001'
    TEFTicketCar.ArqSTS = 'C:\TCS\RX\INTTCS.RET'
    TEFTicketCar.ArqResp = 'C:\TCS\RX\INTTCS.001'
    TEFTicketCar.GPExeName = 'C:\TCS\tcs.exe'
    TEFTicketCar.NumLoja = 0
    TEFTicketCar.NumCaixa = 0
    TEFTicketCar.AtualizaPrecos = False
    TEFConvCard.ArqTemp = 'C:\ger_convenio\tx\crtsol.tmp'
    TEFConvCard.ArqReq = 'C:\ger_convenio\tx\crtsol.001'
    TEFConvCard.ArqSTS = 'C:\ger_convenio\rx\crtsol.ok'
    TEFConvCard.ArqResp = 'C:\ger_convenio\rx\crtsol.001'
    TEFConvCard.GPExeName = 'C:\ger_convcard\convcard.exe'
    OnExibeMsg = ACBrTEFD1ExibeMsg
    OnComandaECF = ACBrTEFD1ComandaECF
    OnComandaECFSubtotaliza = ACBrTEFD1ComandaECFSubtotaliza
    OnComandaECFAbreVinculado = ACBrTEFD1ComandaECFAbreVinculado
    OnComandaECFImprimeVia = ACBrTEFD1ComandaECFImprimeVia
    OnInfoECF = ACBrTEFD1InfoECF
    Left = 328
    Top = 360
  end
  object ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes
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
    Left = 686
    Top = 453
  end
  object ACBrNFeDANFeRL1: TACBrNFeDANFeRL
    MargemInferior = 0.700000000000000000
    MargemSuperior = 0.700000000000000000
    MargemEsquerda = 0.700000000000000000
    MargemDireita = 0.700000000000000000
    CasasDecimais.Formato = tdetInteger
    CasasDecimais.qCom = 2
    CasasDecimais.vUnCom = 2
    CasasDecimais.MaskqCom = ',0.00'
    CasasDecimais.MaskvUnCom = ',0.00'
    ACBrNFe = ACBrNFe1
    ExibeCampoFatura = False
    Left = 420
    Top = 468
  end
end
