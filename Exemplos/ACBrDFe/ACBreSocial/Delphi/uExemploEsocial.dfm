object FExemploEsocial: TFExemploEsocial
  Left = 201
  Top = 130
  Caption = 'Exemplo eSocial'
  ClientHeight = 603
  ClientWidth = 1021
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 281
    Top = 0
    Width = 740
    Height = 603
    Align = alClient
    TabOrder = 0
    object pgcEventos: TPageControl
      Left = 1
      Top = 1
      Width = 738
      Height = 332
      ActivePage = tbsEventosTabela
      Align = alTop
      TabOrder = 0
      object tbsEventosTabela: TTabSheet
        Caption = 'Eventos de Tabela'
        object cbS1000: TCheckBox
          Left = 12
          Top = 12
          Width = 196
          Height = 17
          Caption = 'S-1000 Informa'#231#245'es do Empregador'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
        end
        object cbS1005: TCheckBox
          Left = 12
          Top = 33
          Width = 329
          Height = 17
          Caption = 'S-1005 Tabela de Estabelecimentos e Obras de Constru'#231#227'o Civil'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
        end
        object cbS1010: TCheckBox
          Left = 12
          Top = 54
          Width = 156
          Height = 17
          Caption = 'S-1010 Tabela de Rubricas'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 2
        end
        object cbS1020: TCheckBox
          Left = 12
          Top = 74
          Width = 236
          Height = 17
          Caption = 'S-1020 Tabela de Lota'#231#245'es/Departamentos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 3
        end
        object cbS1030: TCheckBox
          Left = 12
          Top = 97
          Width = 144
          Height = 17
          Caption = 'S-1030 Tabela de Cargos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 4
        end
        object cbS1035: TCheckBox
          Left = 12
          Top = 118
          Width = 249
          Height = 17
          Caption = 'S-1035 Tabela de Carreiras P'#250'blicas'
          TabOrder = 5
        end
        object Checb_ZeraBase: TCheckBox
          Left = 278
          Top = 10
          Width = 138
          Height = 17
          Caption = 'Zera Base Homologa'#231#227'o'
          TabOrder = 6
        end
        object cbS1040: TCheckBox
          Left = 374
          Top = 33
          Width = 156
          Height = 17
          Caption = 'S-1040 Tabela de Fun'#231#245'es'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 7
        end
        object cbS1050: TCheckBox
          Left = 374
          Top = 54
          Width = 248
          Height = 17
          Caption = 'S-1050 Tabela de Hor'#225'rios/Turnos de Trabalho'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 8
        end
        object cbS1060: TCheckBox
          Left = 374
          Top = 75
          Width = 228
          Height = 17
          Caption = 'S-1060 Tabela de Ambientes de Trabalho'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 9
        end
        object cbS1070: TCheckBox
          Left = 374
          Top = 96
          Width = 168
          Height = 17
          Caption = 'S-1070 - Tabela de Processos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 10
        end
        object cbS1080: TCheckBox
          Left = 374
          Top = 118
          Width = 224
          Height = 17
          Caption = 'S-1080 - Tabela de Operadores Portu'#225'rios'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 11
        end
      end
      object tbsEventosPeriodicos: TTabSheet
        Caption = 'Eventos Peri'#243'dicos'
        ImageIndex = 1
        object cbS1200: TCheckBox
          Left = 12
          Top = 12
          Width = 209
          Height = 17
          Caption = 'S-1200 Remunera'#231#227'o do Trabalhador'
          TabOrder = 0
        end
        object cbS1202: TCheckBox
          Left = 12
          Top = 35
          Width = 209
          Height = 17
          Caption = 'S-1202 - Remunera'#231#227'o de trabalhadores RPPS'
          TabOrder = 1
        end
        object cbS1207: TCheckBox
          Left = 12
          Top = 58
          Width = 265
          Height = 17
          Caption = 'S-1207 - Benef'#237'cios  Previdenci'#225'rios - RPPS'
          TabOrder = 2
        end
        object cbS1210: TCheckBox
          Left = 12
          Top = 81
          Width = 273
          Height = 17
          Caption = 'S-1210 - Pagamentos de Rendimentos do Trabalho'
          TabOrder = 3
        end
        object cbS1250: TCheckBox
          Left = 12
          Top = 106
          Width = 332
          Height = 16
          Caption = 'S-1250 - Aquisi'#231#227'o de Produ'#231#227'o Rural'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 4
        end
        object cbS1260: TCheckBox
          Left = 12
          Top = 127
          Width = 332
          Height = 17
          Caption = 'S-1260 - Comercializa'#231#227'o da Produ'#231#227'o Rural Pessoa F'#237'sica'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 5
        end
        object cbS1300: TCheckBox
          Left = 350
          Top = 128
          Width = 289
          Height = 15
          Caption = 'S-1300 - Contribui'#231#227'o Sindical Patronal'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 6
        end
        object cbS1299: TCheckBox
          Left = 350
          Top = 105
          Width = 289
          Height = 17
          Caption = 'S-1299 - Fechamento dos Eventos Peri'#243'dicos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 7
        end
        object cbS1298: TCheckBox
          Left = 350
          Top = 82
          Width = 289
          Height = 17
          Caption = 'S-1298 - Reabertura dos Eventos Peri'#243'dicos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 8
        end
        object cbS1295: TCheckBox
          Left = 350
          Top = 59
          Width = 381
          Height = 17
          Caption = 
            'S-1295 - Solicita'#231#227'o de Totaliza'#231#227'o para Pagamento em Conting'#234'nc' +
            'ia'
          TabOrder = 9
        end
        object cbS1280: TCheckBox
          Left = 350
          Top = 35
          Width = 321
          Height = 18
          Caption = 'S-1280 - Informa'#231#245'es Complementares aos Eventos Peri'#243'dicos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 10
        end
        object cbS1270: TCheckBox
          Left = 350
          Top = 12
          Width = 332
          Height = 17
          Caption = 'S-1270 - Contrata'#231#227'o de Trabalhadores Avulsos N'#227'o Portu'#225'rios'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 11
        end
      end
      object tbsEventosNaoPeriodicos: TTabSheet
        Caption = 'Eventos N'#227'o Peri'#243'dicos'
        ImageIndex = 2
        object cbS2190: TCheckBox
          Left = 12
          Top = 12
          Width = 321
          Height = 17
          Caption = 'S-2190 - Admiss'#227'o de Trabalhador - Registro Preliminar'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
        end
        object cbS2200: TCheckBox
          Left = 12
          Top = 35
          Width = 153
          Height = 17
          Caption = 'S-2200 -  Evento Admiss'#227'o'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
        end
        object cbS2205: TCheckBox
          Left = 12
          Top = 58
          Width = 289
          Height = 17
          Caption = 'S-2205 - Altera'#231#227'o de Dados Cadastrais do Trabalhador'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 2
        end
        object cbS2206: TCheckBox
          Left = 12
          Top = 81
          Width = 236
          Height = 17
          Caption = 'S-2206 - Altera'#231#227'o do Contrato de Trabalho'
          TabOrder = 3
        end
        object cbS2210: TCheckBox
          Left = 12
          Top = 104
          Width = 265
          Height = 17
          Caption = 'S-2210 - Comunica'#231#227'o de Acidente de Trabalho'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 4
        end
        object cbS2220: TCheckBox
          Left = 12
          Top = 127
          Width = 265
          Height = 17
          Caption = 'S-2220 -  Monitoramento da Sa'#250'de do Trabalhador'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 5
        end
        object cbS2230: TCheckBox
          Left = 12
          Top = 173
          Width = 177
          Height = 17
          Caption = 'S-2230 - Afastamento Tempor'#225'rio'
          TabOrder = 6
        end
        object cbS2240: TCheckBox
          Left = 12
          Top = 150
          Width = 332
          Height = 17
          Caption = 'S-2240 -  Condi'#231#245'es Ambientais do Trabalho - Fatores de Risco'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 7
        end
        object cbS2241: TCheckBox
          Left = 12
          Top = 196
          Width = 332
          Height = 17
          Caption = 'S-2241 - Insalubridade, Periculosidade e Aposentadoria Especial'
          TabOrder = 8
        end
        object cbAviso: TComboBox
          Left = 476
          Top = 12
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 9
          Text = 'Aviso'
          Items.Strings = (
            'Aviso'
            'Cancelamento')
        end
        object cbS2250: TCheckBox
          Left = 350
          Top = 12
          Width = 120
          Height = 17
          Caption = 'S-2250 - Aviso Pr'#233'vio'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 10
        end
        object cbS3000: TCheckBox
          Left = 350
          Top = 196
          Width = 175
          Height = 17
          Caption = 'S-3000 - Exclus'#227'o de Eventos'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 11
        end
        object cbS2400: TCheckBox
          Left = 350
          Top = 173
          Width = 321
          Height = 17
          Caption = 'S-2400 - Cadastro de Benef'#237'cios Previd'#234'nci'#225'rios - RPPS'
          TabOrder = 12
        end
        object cbS2399: TCheckBox
          Left = 350
          Top = 150
          Width = 248
          Height = 17
          Caption = 'S-2399 - Trabalhador Sem V'#237'nculo - T'#233'rmino'
          TabOrder = 13
        end
        object cbS2306: TCheckBox
          Left = 350
          Top = 127
          Width = 311
          Height = 17
          Caption = 'S-2306 - Trabalhador Sem V'#237'nculo - Altera'#231#227'o Contratual'
          TabOrder = 14
        end
        object cbS2300: TCheckBox
          Left = 350
          Top = 104
          Width = 233
          Height = 17
          Caption = 'S-2300 - Trabalhador Sem V'#237'nculo - In'#237'cio'
          TabOrder = 15
        end
        object cbS2299: TCheckBox
          Left = 350
          Top = 81
          Width = 135
          Height = 17
          Caption = 'S-2299 - Desligamento'
          TabOrder = 16
        end
        object cbS2298: TCheckBox
          Left = 350
          Top = 58
          Width = 153
          Height = 17
          Caption = 'S-2298 - Reintegra'#231#227'o'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 17
        end
        object cbs2260: TCheckBox
          Left = 350
          Top = 39
          Width = 332
          Height = 17
          Caption = 'S-2260 Convoca'#231#227'o para Trabalho Intermitente'
          TabOrder = 18
        end
      end
    end
    object pgWebservice: TPageControl
      Left = 1
      Top = 259
      Width = 738
      Height = 343
      ActivePage = tsComandos
      Align = alBottom
      TabOrder = 1
      object tsComandos: TTabSheet
        Caption = 'Comandos'
        ImageIndex = 5
        object btnGerar: TButton
          Left = 10
          Top = 12
          Width = 109
          Height = 25
          Caption = 'Gerar Arquivos'
          TabOrder = 0
          OnClick = btnGerarClick
        end
        object btnConsultar: TButton
          Left = 250
          Top = 43
          Width = 109
          Height = 25
          Caption = 'Consultar'
          TabOrder = 1
          OnClick = btnConsultarClick
        end
        object btnEnviar: TButton
          Left = 10
          Top = 43
          Width = 109
          Height = 25
          Caption = 'Enviar'
          TabOrder = 2
          OnClick = btnEnviarClick
        end
        object btnCarregarXML: TButton
          Left = 130
          Top = 12
          Width = 109
          Height = 25
          Caption = 'Carregar XML'
          TabOrder = 3
          OnClick = btnCarregarXMLClick
        end
        object btnCarregarINI: TButton
          Left = 250
          Top = 12
          Width = 109
          Height = 25
          Caption = 'Carregar INI'
          TabOrder = 4
          OnClick = btnCarregarINIClick
        end
        object btnGerarEnviar: TButton
          Left = 130
          Top = 43
          Width = 109
          Height = 25
          Caption = 'Gerar e Enviar'
          TabOrder = 5
          OnClick = btnGerarEnviarClick
        end
        object btnConsIdeEveEmp: TButton
          Left = 12
          Top = 74
          Width = 227
          Height = 25
          Caption = 'Consultar Identificadores Eventos Empregador'
          TabOrder = 6
          OnClick = btnConsIdeEveEmpClick
        end
        object btnDownloadEventos: TButton
          Left = 250
          Top = 74
          Width = 109
          Height = 25
          Caption = 'Download Eventos'
          TabOrder = 7
          OnClick = btnDownloadEventosClick
        end
      end
      object tsFormaEnvio: TTabSheet
        Caption = 'Forma de Envio'
        ImageIndex = 5
        object rdgGrupo: TRadioGroup
          Left = 0
          Top = 0
          Width = 730
          Height = 43
          Align = alTop
          Caption = '  Grupo  '
          Columns = 3
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemIndex = 0
          Items.Strings = (
            'Iniciais ou Tabelas '
            'N'#227'o peri'#243'dicos'
            'Peri'#243'dicos.')
          ParentFont = False
          TabOrder = 0
        end
        object rdgOperacao: TRadioGroup
          Left = 0
          Top = 43
          Width = 730
          Height = 43
          Align = alTop
          Caption = '  Opera'#231#227'o  '
          Columns = 3
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemIndex = 0
          Items.Strings = (
            'Inclus'#227'o'
            'Altera'#231#227'o'
            'Exclus'#227'o')
          ParentFont = False
          TabOrder = 1
        end
        object chkClear: TCheckBox
          Left = 12
          Top = 92
          Width = 121
          Height = 17
          Caption = 'Limpar Pasta Docs'
          TabOrder = 2
        end
      end
      object tsResposta: TTabSheet
        Caption = 'Resposta'
        ImageIndex = 1
        object MemoResp: TMemo
          Left = 0
          Top = 0
          Width = 730
          Height = 315
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsLog: TTabSheet
        Caption = 'Log'
        ImageIndex = 6
        object memoLog: TMemo
          Left = 0
          Top = 0
          Width = 730
          Height = 315
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsXmlEnvio: TTabSheet
        Caption = 'XML de Envio'
        ImageIndex = 2
        object MemoXmlEnvio: TMemo
          Left = 0
          Top = 0
          Width = 730
          Height = 315
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsXmlRetorno: TTabSheet
        Caption = 'XML Retorno'
        ImageIndex = 3
        object MemoXmlRetorno: TMemo
          Left = 0
          Top = 0
          Width = 730
          Height = 315
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsDados: TTabSheet
        Caption = 'Dados'
        ImageIndex = 6
        object MemoDados: TMemo
          Left = 0
          Top = 0
          Width = 730
          Height = 315
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 603
    Align = alLeft
    TabOrder = 1
    object lblColaborador: TLabel
      Left = 10
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
      Left = 8
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
      Left = 13
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
      Left = 109
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
    object btnSalvarConfig: TBitBtn
      Left = 66
      Top = 513
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
      Left = 1
      Top = 1
      Width = 279
      Height = 508
      ActivePage = TabSheet1
      Align = alTop
      MultiLine = True
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Geral'
        ImageIndex = 1
        object GroupBox1: TGroupBox
          Left = 3
          Top = 2
          Width = 263
          Height = 226
          Caption = 'Geral'
          TabOrder = 0
          object sbtnPathSalvar: TSpeedButton
            Left = 233
            Top = 156
            Width = 23
            Height = 23
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
          object Label3: TLabel
            Left = 8
            Top = 34
            Width = 68
            Height = 13
            Caption = 'Formato Alerta'
          end
          object Label4: TLabel
            Left = 8
            Top = 77
            Width = 121
            Height = 13
            Caption = 'Vers'#227'o Documento Fiscal'
          end
          object spPathSchemas: TSpeedButton
            Left = 233
            Top = 193
            Width = 23
            Height = 23
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
          object Label1: TLabel
            Left = 8
            Top = 179
            Width = 199
            Height = 13
            Caption = 'Diret'#243'rios com os arquivos XSD(Schemas)'
          end
          object edtPathLogs: TEdit
            Left = 8
            Top = 157
            Width = 226
            Height = 21
            TabOrder = 0
          end
          object ckSalvar: TCheckBox
            Left = 8
            Top = 138
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
          object cbxExibirErroSchema: TCheckBox
            Left = 8
            Top = 16
            Width = 129
            Height = 17
            Caption = 'Exibir Erro Schema'
            TabOrder = 2
          end
          object edtFormatoAlerta: TEdit
            Left = 8
            Top = 50
            Width = 248
            Height = 21
            TabOrder = 3
          end
          object cbxRetirarAcentos: TCheckBox
            Left = 8
            Top = 120
            Width = 193
            Height = 17
            Caption = 'Retirar Acentos dos XMLs enviados'
            TabOrder = 4
          end
          object cbVersaoDF: TComboBox
            Left = 8
            Top = 93
            Width = 248
            Height = 21
            TabOrder = 5
          end
          object edtPathSchemas: TEdit
            Left = 8
            Top = 194
            Width = 226
            Height = 21
            TabOrder = 6
          end
        end
        object gbDadosEmpresa: TGroupBox
          Left = 3
          Top = 234
          Width = 263
          Height = 167
          Caption = 'Dados Empresa'
          TabOrder = 1
          object Label2: TLabel
            Left = 8
            Top = 18
            Width = 102
            Height = 13
            Caption = 'CNPJ do Empregador'
          end
          object Label5: TLabel
            Left = 8
            Top = 58
            Width = 101
            Height = 13
            Caption = 'CNPJ do Transmissor'
          end
          object Label7: TLabel
            Left = 8
            Top = 101
            Width = 96
            Height = 13
            Caption = 'Tipo de Empregador'
          end
          object edtIdEmpregador: TEdit
            Left = 7
            Top = 34
            Width = 248
            Height = 21
            TabOrder = 0
          end
          object edtIdTransmissor: TEdit
            Left = 7
            Top = 74
            Width = 248
            Height = 21
            TabOrder = 1
          end
          object cbTEmpregador: TComboBox
            Left = 8
            Top = 117
            Width = 248
            Height = 21
            TabOrder = 2
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Certificado'
        object Label6: TLabel
          Left = 35
          Top = 16
          Width = 34
          Height = 13
          Alignment = taRightJustify
          Caption = 'SSLLib'
          Color = clBtnFace
          ParentColor = False
        end
        object Label12: TLabel
          Left = 31
          Top = 43
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'CryptLib'
          Color = clBtnFace
          ParentColor = False
        end
        object Label13: TLabel
          Left = 35
          Top = 70
          Width = 34
          Height = 13
          Alignment = taRightJustify
          Caption = 'HttpLib'
          Color = clBtnFace
          ParentColor = False
        end
        object Label14: TLabel
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
          Left = 2
          Top = 118
          Width = 263
          Height = 144
          Caption = 'Certificado'
          TabOrder = 0
          object Label15: TLabel
            Left = 8
            Top = 16
            Width = 41
            Height = 13
            Caption = 'Caminho'
          end
          object Label16: TLabel
            Left = 8
            Top = 56
            Width = 31
            Height = 13
            Caption = 'Senha'
          end
          object sbtnCaminhoCert: TSpeedButton
            Left = 232
            Top = 31
            Width = 23
            Height = 23
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
          object Label17: TLabel
            Left = 8
            Top = 96
            Width = 79
            Height = 13
            Caption = 'N'#250'mero de S'#233'rie'
          end
          object sbtnGetCert: TSpeedButton
            Left = 235
            Top = 111
            Width = 23
            Height = 23
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
          object sbtnGetCert1: TSpeedButton
            Left = 214
            Top = 111
            Width = 23
            Height = 23
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
            OnClick = sbtnListaCertClick
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
            Width = 208
            Height = 21
            TabOrder = 2
          end
        end
        object btnValidadeData: TButton
          Left = 8
          Top = 266
          Width = 99
          Height = 25
          Caption = 'Data de Validade'
          TabOrder = 1
          OnClick = btnValidadeDataClick
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
        object btnSubjectName: TButton
          Left = 8
          Top = 298
          Width = 99
          Height = 25
          Caption = 'Subject Name'
          TabOrder = 3
          OnClick = btnSubjectNameClick
        end
        object btnCNPJ: TButton
          Left = 109
          Top = 297
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
        object GroupBox3: TGroupBox
          Left = 2
          Top = 328
          Width = 263
          Height = 69
          Caption = 'Calculo de Hash e assinatura'
          TabOrder = 6
          object edtCalcHash: TEdit
            Left = 7
            Top = 14
            Width = 249
            Height = 21
            TabOrder = 0
            Text = '0548133600013704583493000190'
          end
          object btnSHA_RSA: TButton
            Left = 8
            Top = 41
            Width = 99
            Height = 25
            Caption = 'SHA256+RSA'
            TabOrder = 1
            OnClick = btnSHA_RSAClick
          end
          object cbAssinar: TCheckBox
            Left = 144
            Top = 44
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
        object btnX509: TButton
          Left = 144
          Top = 403
          Width = 115
          Height = 25
          Caption = 'Leitura de X509'
          TabOrder = 8
          OnClick = btnX509Click
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
      object TabSheet5: TTabSheet
        Caption = 'WebService'
        ImageIndex = 2
        object GroupBox4: TGroupBox
          Left = 0
          Top = 4
          Width = 265
          Height = 149
          Caption = 'WebService'
          TabOrder = 0
          object Label18: TLabel
            Left = 191
            Top = 72
            Width = 40
            Height = 13
            Caption = 'TimeOut'
            Color = clBtnFace
            ParentColor = False
          end
          object Label19: TLabel
            Left = 8
            Top = 119
            Width = 44
            Height = 13
            Alignment = taRightJustify
            Caption = 'SSLType'
            Color = clBtnFace
            ParentColor = False
          end
          object cbxVisualizar: TCheckBox
            Left = 8
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
            TabOrder = 0
          end
          object rgTipoAmb: TRadioGroup
            Left = 8
            Top = 16
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
          object cbxSalvarSOAP: TCheckBox
            Left = 8
            Top = 92
            Width = 153
            Height = 17
            Caption = 'Salvar envelope SOAP'
            TabOrder = 2
          end
          object seTimeOut: TSpinEdit
            Left = 191
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
            Left = 97
            Top = 116
            Width = 160
            Height = 21
            Hint = 'Depende de configura'#231#227'o de  SSL.HttpLib'
            Style = csDropDownList
            TabOrder = 4
            OnChange = cbSSLTypeChange
          end
        end
        object GroupBox8: TGroupBox
          Left = 0
          Top = 242
          Width = 265
          Height = 104
          Caption = 'Proxy'
          TabOrder = 1
          object Label20: TLabel
            Left = 8
            Top = 16
            Width = 22
            Height = 13
            Caption = 'Host'
          end
          object Label21: TLabel
            Left = 208
            Top = 16
            Width = 25
            Height = 13
            Caption = 'Porta'
          end
          object Label22: TLabel
            Left = 8
            Top = 56
            Width = 36
            Height = 13
            Caption = 'Usu'#225'rio'
          end
          object Label23: TLabel
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
        object GroupBox9: TGroupBox
          Left = 0
          Top = 159
          Width = 265
          Height = 77
          Caption = 'Retorno de Envio do eSocial '
          TabOrder = 2
          object Label24: TLabel
            Left = 93
            Top = 31
            Width = 50
            Height = 13
            Caption = 'Tentativas'
          end
          object Label26: TLabel
            Left = 176
            Top = 31
            Width = 41
            Height = 13
            Caption = 'Intervalo'
          end
          object Label27: TLabel
            Left = 8
            Top = 31
            Width = 43
            Height = 13
            Hint = 
              'Aguardar quantos segundos para primeira consulta de retorno de e' +
              'nvio'
            Caption = 'Aguardar'
          end
          object cbxAjustarAut: TCheckBox
            Left = 8
            Top = 14
            Width = 234
            Height = 17
            Caption = 'Ajustar Automaticamente prop. "Aguardar"'
            TabOrder = 0
          end
          object edtTentativas: TEdit
            Left = 93
            Top = 47
            Width = 57
            Height = 21
            TabOrder = 2
          end
          object edtIntervalo: TEdit
            Left = 176
            Top = 47
            Width = 57
            Height = 21
            TabOrder = 3
          end
          object edtAguardar: TEdit
            Left = 8
            Top = 47
            Width = 57
            Height = 21
            Hint = 
              'Aguardar quantos segundos para primeira consulta de retorno de e' +
              'nvio'
            TabOrder = 1
          end
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Arquivos'
        ImageIndex = 4
        object sbPatheSocial: TSpeedButton
          Left = 240
          Top = 142
          Width = 23
          Height = 23
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
          OnClick = sbPatheSocialClick
        end
        object Label28: TLabel
          Left = 6
          Top = 127
          Width = 109
          Height = 13
          Caption = 'Pasta Arquivos eSocial'
        end
        object Label36: TLabel
          Left = 6
          Top = 173
          Width = 108
          Height = 13
          Caption = 'Pasta Arquivos Evento'
        end
        object sbPathEvento: TSpeedButton
          Left = 240
          Top = 188
          Width = 23
          Height = 23
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
          Top = 12
          Width = 210
          Height = 17
          Caption = 'Salvar Arquivos em Pastas Separadas'
          TabOrder = 0
        end
        object cbxPastaMensal: TCheckBox
          Left = 6
          Top = 28
          Width = 210
          Height = 17
          Caption = 'Criar Pastas Mensalmente'
          TabOrder = 1
        end
        object cbxAdicionaLiteral: TCheckBox
          Left = 6
          Top = 44
          Width = 210
          Height = 17
          Caption = 'Adicionar Literal no nome das pastas'
          TabOrder = 2
        end
        object cbxEmissaoPatheSocial: TCheckBox
          Left = 6
          Top = 60
          Width = 233
          Height = 17
          Caption = 'Salvar eSocial pelo campo Data de Emiss'#227'o'
          TabOrder = 3
        end
        object cbxSalvaPathEvento: TCheckBox
          Left = 6
          Top = 76
          Width = 233
          Height = 17
          Caption = 'Salvar Arqs de Eventos'
          TabOrder = 4
        end
        object cbxSepararPorCNPJ: TCheckBox
          Left = 6
          Top = 92
          Width = 233
          Height = 17
          Caption = 'Separar Arqs pelo CNPJ do Certificado'
          TabOrder = 5
        end
        object edtPatheSocial: TEdit
          Left = 6
          Top = 143
          Width = 235
          Height = 21
          TabOrder = 6
        end
        object edtPathEvento: TEdit
          Left = 6
          Top = 189
          Width = 235
          Height = 21
          TabOrder = 7
        end
      end
    end
  end
  object ACBreSocial1: TACBreSocial
    OnStatusChange = ACBreSocial1StatusChange
    OnGerarLog = ACBreSocial1GerarLog
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Arquivos.PathSalvar = 'C:\teste_esocial\'
    Configuracoes.Arquivos.PathSchemas = '..\..\schemas\eSocial\'
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    OnTransmissaoEventos = ACBreSocial1TransmissaoEventos
    Left = 664
    Top = 304
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*-nfe.XML'
    Filter = 
      'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|To' +
      'dos os Arquivos (*.*)|*.*'
    Title = 'Selecione a NFe'
    Left = 752
    Top = 304
  end
end
