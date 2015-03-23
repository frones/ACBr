object frmTelaTeste: TfrmTelaTeste
  Left = 307
  Top = 142
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SEF 2 / eDOC'
  ClientHeight = 492
  ClientWidth = 651
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
  object stat1: TStatusBar
    Left = 0
    Top = 473
    Width = 651
    Height = 19
    Panels = <>
  end
  object pgc1: TPageControl
    Left = 8
    Top = 16
    Width = 633
    Height = 447
    ActivePage = ts1
    TabOrder = 1
    object ts1: TTabSheet
      Caption = 'Gera'#231#227'o do arquivo'
      object bvl1: TBevel
        Left = 8
        Top = 124
        Width = 221
        Height = 290
      end
      object lbl1: TLabel
        Left = 32
        Top = 118
        Width = 47
        Height = 13
        Caption = 'SEF 2012'
      end
      object bvl2: TBevel
        Left = 394
        Top = 127
        Width = 218
        Height = 279
      end
      object lbl2: TLabel
        Left = 408
        Top = 122
        Width = 29
        Height = 13
        Caption = 'eDOC'
      end
      object btnB_0: TButton
        Left = 24
        Top = 153
        Width = 193
        Height = 25
        Caption = 'Criar Bloco 0'
        TabOrder = 0
        OnClick = btnB_0Click
      end
      object btnB_E: TButton
        Left = 24
        Top = 185
        Width = 193
        Height = 25
        Caption = 'Criar Bloco E'
        TabOrder = 1
        OnClick = btnB_EClick
      end
      object btnSalva_SEF2: TButton
        Left = 24
        Top = 376
        Width = 193
        Height = 25
        Caption = 'Salvar arquivo SEF2'
        TabOrder = 2
        OnClick = btnSalva_SEF2Click
      end
      object btnB_C: TButton
        Left = 408
        Top = 152
        Width = 193
        Height = 25
        Caption = 'Criar Bloco C = (eDoc)'
        Enabled = False
        TabOrder = 3
        OnClick = btnB_CClick
      end
      object btnSalva_eDoc: TButton
        Left = 416
        Top = 374
        Width = 193
        Height = 25
        Caption = 'Salvar arquivo eDoc'
        Enabled = False
        TabOrder = 4
        OnClick = btnSalva_eDocClick
      end
      object btnB_G: TButton
        Left = 24
        Top = 249
        Width = 193
        Height = 25
        Caption = 'Criar Bloco G'
        Enabled = False
        TabOrder = 5
      end
      object btnB_F: TButton
        Left = 24
        Top = 217
        Width = 193
        Height = 25
        Caption = 'Criar Bloco F'
        Enabled = False
        TabOrder = 6
      end
      object btnB_H: TButton
        Left = 24
        Top = 277
        Width = 193
        Height = 25
        Caption = 'Criar Bloco H'
        Enabled = False
        TabOrder = 7
      end
      object rgConteudoArquivo: TRadioGroup
        Left = 7
        Top = 39
        Width = 610
        Height = 65
        Caption = 'Tabela Conte'#250'do do Arquivo-texto'
        ItemIndex = 0
        Items.Strings = (
          '20 -(SEF2)  Lan'#231'amentos de opera'#231#245'es e resultados fiscais'
          '91 -(eDOC)  Extrato de documentos fiscais')
        TabOrder = 8
        OnClick = rgConteudoArquivoClick
      end
      object btnDefinirPath: TButton
        Left = 16
        Top = 7
        Width = 177
        Height = 25
        Caption = 'Definir Path Arquivo'
        TabOrder = 9
        OnClick = btnDefinirPathClick
      end
    end
    object ts2: TTabSheet
      Caption = '0030 Perfil'
      ImageIndex = 1
      object lbl20: TLabel
        Left = 8
        Top = 8
        Width = 148
        Height = 13
        Caption = 'Indicador de entrada de dados:'
      end
      object lbl21: TLabel
        Left = 8
        Top = 32
        Width = 209
        Height = 13
        Caption = 'Indicador do documento contido no arquivo:'
      end
      object lbl22: TLabel
        Left = 8
        Top = 56
        Width = 233
        Height = 13
        Caption = 'Indicador de exigibilidade da escritura'#231#227'o do ISS:'
      end
      object lbl23: TLabel
        Left = 8
        Top = 80
        Width = 242
        Height = 13
        Caption = 'Indicador de exigibilidade da escritura'#231#227'o do ICMS:'
      end
      object lbl24: TLabel
        Left = 8
        Top = 104
        Width = 358
        Height = 13
        Caption = 
          'Indicador de exigibilidade do Registro de Impress'#227'o de Documento' +
          's Fiscais:'
      end
      object lbl25: TLabel
        Left = 8
        Top = 128
        Width = 356
        Height = 13
        Caption = 
          'Indicador de exigibilidade do Registro de Utiliza'#231#227'o de Document' +
          'os Fiscais:'
      end
      object lbl26: TLabel
        Left = 8
        Top = 152
        Width = 333
        Height = 13
        Caption = 
          'Indicador de exigibilidade do Livro de Movimenta'#231#227'o de Combust'#237'v' +
          'eis:'
      end
      object lbl27: TLabel
        Left = 8
        Top = 176
        Width = 239
        Height = 13
        Caption = 'Indicador de exigibilidade do Registro de Ve'#237'culos:'
      end
      object lbl28: TLabel
        Left = 8
        Top = 200
        Width = 273
        Height = 13
        Caption = 'Indicador de exigibilidade anual do Registro de Invent'#225'rio:'
      end
      object lbl29: TLabel
        Left = 8
        Top = 224
        Width = 246
        Height = 13
        Caption = 'Indicador de apresenta'#231#227'o da escritura'#231#227'o cont'#225'bil:'
      end
      object lbl30: TLabel
        Left = 8
        Top = 248
        Width = 188
        Height = 13
        Caption = 'Indicador de opera'#231#245'es sujeitas ao ISS:'
      end
      object lbl31: TLabel
        Left = 8
        Top = 272
        Width = 478
        Height = 13
        Caption = 
          'Indicador de opera'#231#245'es sujeitas '#224' reten'#231#227'o tribut'#225'ria do ISS, na' +
          ' condi'#231#227'o de contribuinte-substitu'#237'do:'
      end
      object lbl32: TLabel
        Left = 8
        Top = 320
        Width = 494
        Height = 13
        Caption = 
          'Indicador de opera'#231#245'es sujeitas '#224' substitui'#231#227'o tribut'#225'ria do ICM' +
          'S, na condi'#231#227'o de contribuinte-substituto:'
      end
      object lbl33: TLabel
        Left = 8
        Top = 344
        Width = 378
        Height = 13
        Caption = 
          'Indicador de opera'#231#245'es sujeitas '#224' antecipa'#231#227'o tribut'#225'ria do ICMS' +
          ', nas entradas:'
      end
      object lbl34: TLabel
        Left = 8
        Top = 368
        Width = 184
        Height = 13
        Caption = 'Indicador de opera'#231#245'es sujeitas ao IPI:'
      end
      object lbl35: TLabel
        Left = 8
        Top = 392
        Width = 286
        Height = 13
        Caption = 'Indicador de apresenta'#231#227'o avulsa do Registro de Invent'#225'rio:'
      end
      object lbl36: TLabel
        Left = 8
        Top = 296
        Width = 197
        Height = 13
        Caption = 'Indicador de opera'#231#245'es sujeitas ao ICMS:'
      end
      object cbbIND_ED: TComboBox
        Tag = 5
        Left = 165
        Top = 3
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = '0- Digita'#231#227'o de dados'
        Items.Strings = (
          '0- Digita'#231#227'o de dados'
          '1- Importa'#231#227'o de arquivo texto'
          '2- Valida'#231#227'o de arquivo texto'
          '')
      end
      object cbbIND_ARQ: TComboBox
        Tag = 5
        Left = 221
        Top = 27
        Width = 356
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Text = '5- Livros de resultados e obriga'#231#245'es'
        Items.Strings = (
          '0- Documento original emitido em arquivo'
          '1- Transcri'#231#227'o de documentos de emiss'#227'o pr'#243'pria'
          '2- Transcri'#231#227'o de documentos emitidos por terceiros'
          '3- Transcri'#231#227'o de documentos capturados por digitaliza'#231#227'o'
          
            '4- Transcri'#231#227'o de documentos emitidos em equipamento especializa' +
            'do'
          '5- Livros de resultados e obriga'#231#245'es'
          '6- Livros e mapas de controle'
          '7- Guias de informa'#231#245'es econ'#244'mico-fiscais'
          '8- Livros da contabilidade'
          '9- Extratos de documentos'
          '')
      end
      object cbbPRF_ISS: TComboBox
        Tag = 5
        Left = 245
        Top = 51
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = '9- N'#227'o obrigado a escriturar'
        Items.Strings = (
          '0- Sim, com regime simplificado de escritura'#231#227'o do imposto'
          
            '2- Sim, com regime integral de escritura'#231#227'o e apura'#231#227'o do impost' +
            'o'
          '9- N'#227'o obrigado a escriturar'
          '')
      end
      object cbbPRF_ICMS: TComboBox
        Tag = 5
        Left = 253
        Top = 75
        Width = 212
        Height = 21
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 3
        Text = 
          '2- Sim, com regime integral de escritura'#231#227'o e apura'#231#227'o normal do' +
          ' imposto'
        Items.Strings = (
          '0- Sim, com regime simplificado de escritura'#231#227'o do imposto'
          
            '1- Sim, com regime intermedi'#225'rio de escritura'#231#227'o e apura'#231#227'o norm' +
            'al do imposto'
          
            '2- Sim, com regime integral de escritura'#231#227'o e apura'#231#227'o normal do' +
            ' imposto'
          '9- N'#227'o obrigado a escriturar')
      end
      object cbbPRF_RIDF: TComboBox
        Tag = 5
        Left = 373
        Top = 99
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 4
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbPRF_RUDF: TComboBox
        Tag = 5
        Left = 365
        Top = 123
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 5
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbPRF_LMC: TComboBox
        Tag = 5
        Left = 349
        Top = 147
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 6
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbPRF_RV: TComboBox
        Tag = 5
        Left = 253
        Top = 171
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 7
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbPRF_RI: TComboBox
        Tag = 5
        Left = 285
        Top = 195
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 8
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_EC: TComboBox
        Tag = 5
        Left = 261
        Top = 219
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 9
        Text = '9- N'#227'o obrigado a escriturar'
        Items.Strings = (
          '0- Completa registrada em arquivo digital'
          
            '1- Completa registrada em papel, microfilme, fichas avulsas, ou ' +
            'fichas/folhas cont'#237'nuas'
          '2- Simplificada registrada em arquivo digital'
          
            '3- Simplificada registrada papel, microfilme, fichas avulsas, ou' +
            ' fichas/folhas cont'#237'nuas'
          '4- Livro Caixa registrado em arquivo digital'
          
            '5- Livro Caixa registrado papel, microfilme, fichas avulsas, ou ' +
            'fichas/folhas cont'#237'nuas'
          '9- N'#227'o obrigado a escriturar'
          '')
      end
      object cbbIND_ISS: TComboBox
        Tag = 5
        Left = 205
        Top = 243
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 10
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_RT: TComboBox
        Tag = 5
        Left = 493
        Top = 267
        Width = 129
        Height = 21
        ItemHeight = 13
        TabOrder = 11
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_ST: TComboBox
        Tag = 5
        Left = 512
        Top = 315
        Width = 109
        Height = 21
        ItemHeight = 13
        TabOrder = 12
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_AT: TComboBox
        Tag = 5
        Left = 389
        Top = 339
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 13
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_IPI: TComboBox
        Tag = 5
        Left = 197
        Top = 363
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 14
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_RI: TComboBox
        Tag = 5
        Left = 301
        Top = 387
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 15
        Text = '1- N'#227'o'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
      object cbbIND_ICMS: TComboBox
        Tag = 5
        Left = 213
        Top = 291
        Width = 212
        Height = 21
        ItemHeight = 13
        TabOrder = 16
        Text = '0- Sim'
        Items.Strings = (
          '0- Sim'
          '1- N'#227'o'
          '')
      end
    end
  end
  object dlgSave1: TSaveDialog
    Left = 296
    Top = 40
  end
  object ACBrSEF2: TACBrSEF2
    Path = 'C:\Program Files (x86)\Borland\Delphi7\Bin\'
    Delimitador = '|'
    TrimString = True
    CurMascara = '#0.00'
    Left = 324
    Top = 192
  end
end
