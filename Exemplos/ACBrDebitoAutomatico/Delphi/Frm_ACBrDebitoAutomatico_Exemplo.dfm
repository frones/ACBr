object frmACBrDebitoAutomatico_Exemplo: TfrmACBrDebitoAutomatico_Exemplo
  Left = 192
  Top = 114
  Caption = 
    'Programa exemplo do Componente ACBrDebitoAutomatico (Padr'#227'o FEBR' +
    'ABAN 150 Posi'#231#245'es)'
  ClientHeight = 518
  ClientWidth = 811
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object Label1: TLabel
    Left = 303
    Top = 134
    Width = 94
    Height = 13
    Caption = 'Log de Mensagens:'
  end
  object btnGerar: TButton
    Left = 304
    Top = 72
    Width = 169
    Height = 25
    Caption = 'Gerar Arquivo Remessa'
    TabOrder = 0
    OnClick = btnGerarClick
  end
  object Memo1: TMemo
    Left = 304
    Top = 8
    Width = 497
    Height = 57
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'Arquivo padr'#227'o FEBRABAN de 150 Posi'#231#245'es, para o Interc'#226'mbio de '
      'Informa'#231#245'es entre Bancos e Empresas (D'#233'bito Automatico).')
    ParentFont = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 518
    Align = alLeft
    TabOrder = 2
    object lblColaborador: TLabel
      Left = 15
      Top = 452
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
      Left = 15
      Top = 467
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
      Top = 483
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
      Left = 116
      Top = 499
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
    object gbConfiguracoes: TGroupBox
      Left = 8
      Top = 7
      Width = 281
      Height = 408
      Caption = 'Configura'#231#245'es'
      TabOrder = 0
      object PageControl1: TPageControl
        Left = 2
        Top = 15
        Width = 277
        Height = 390
        ActivePage = TabSheet2
        MultiLine = True
        TabOrder = 0
        object TabSheet4: TTabSheet
          Caption = 'Dados da Empresa'
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
        object TabSheet2: TTabSheet
          Caption = 'Dados do Banco'
          ImageIndex = 1
          object Label30: TLabel
            Left = 8
            Top = 8
            Width = 31
            Height = 13
            Caption = 'Banco'
          end
          object sbtnPathSalvar: TSpeedButton
            Left = 244
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
          object Label2: TLabel
            Left = 9
            Top = 97
            Width = 45
            Height = 13
            Caption = 'Conv'#234'nio'
          end
          object Label10: TLabel
            Left = 9
            Top = 51
            Width = 83
            Height = 13
            Caption = 'Vers'#227'o do Layout'
          end
          object cbBanco: TComboBox
            Left = 8
            Top = 24
            Width = 249
            Height = 24
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object ckSalvar: TCheckBox
            Left = 10
            Top = 290
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
          object edtPathSalvar: TEdit
            Left = 10
            Top = 314
            Width = 228
            Height = 21
            TabOrder = 2
          end
          object edtConvenio: TEdit
            Left = 9
            Top = 113
            Width = 123
            Height = 21
            TabOrder = 3
          end
          object grpConta: TGroupBox
            Left = 9
            Top = 141
            Width = 250
            Height = 145
            Caption = 'Conta'
            TabOrder = 4
            object Label3: TLabel
              Left = 8
              Top = 15
              Width = 39
              Height = 13
              Caption = 'Ag'#234'ncia'
            end
            object Label4: TLabel
              Left = 140
              Top = 15
              Width = 15
              Height = 13
              Caption = 'DV'
            end
            object Label5: TLabel
              Left = 7
              Top = 57
              Width = 28
              Height = 13
              Caption = 'Conta'
            end
            object Label6: TLabel
              Left = 139
              Top = 57
              Width = 15
              Height = 13
              Caption = 'DV'
            end
            object Label7: TLabel
              Left = 8
              Top = 101
              Width = 15
              Height = 13
              Caption = 'DV'
            end
            object Label8: TLabel
              Left = 42
              Top = 101
              Width = 67
              Height = 13
              Caption = 'Tipo de Conta'
            end
            object edtAgencia: TEdit
              Left = 8
              Top = 31
              Width = 123
              Height = 21
              TabOrder = 0
            end
            object edtAgenciaDV: TEdit
              Left = 140
              Top = 31
              Width = 25
              Height = 21
              TabOrder = 1
            end
            object edtContaNumero: TEdit
              Left = 7
              Top = 73
              Width = 123
              Height = 21
              TabOrder = 2
            end
            object edtContaDV: TEdit
              Left = 139
              Top = 73
              Width = 25
              Height = 21
              TabOrder = 3
            end
            object edtDV: TEdit
              Left = 8
              Top = 117
              Width = 25
              Height = 21
              TabOrder = 4
            end
            object edtTipoConta: TEdit
              Left = 42
              Top = 117
              Width = 123
              Height = 21
              TabOrder = 5
            end
          end
          object cbLayoutVersao: TComboBox
            Left = 9
            Top = 67
            Width = 249
            Height = 24
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 5
          end
        end
      end
    end
    object btnSalvarConfig: TBitBtn
      Left = 64
      Top = 421
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
  object LogMsg: TMemo
    Left = 303
    Top = 150
    Width = 500
    Height = 368
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnLer: TButton
    Left = 304
    Top = 103
    Width = 169
    Height = 25
    Caption = 'Ler Arquivo Retorno'
    TabOrder = 4
    OnClick = btnLerClick
  end
  object OpenDialog1: TOpenDialog
    Left = 368
    Top = 216
  end
  object ACBrDebitoAutomatico1: TACBrDebitoAutomatico
    Configuracoes.Arquivos.PathSalvar = 'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\'
    Configuracoes.Arquivos.Salvar = True
    Left = 368
    Top = 160
  end
end
