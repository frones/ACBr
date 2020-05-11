object frmACBrEDI: TfrmACBrEDI
  Left = 249
  Top = 82
  Caption = 'ACBrEDI - Programa Exemplo'
  ClientHeight = 453
  ClientWidth = 866
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMenus: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 453
    Align = alLeft
    TabOrder = 0
    object lblColaborador: TLabel
      Left = 14
      Top = 384
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
      Left = 12
      Top = 399
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
      Left = 17
      Top = 414
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
      Left = 113
      Top = 427
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
      Height = 352
      ActivePage = TabSheet1
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Configura'#231#245'es'
        object PageControl4: TPageControl
          Left = 0
          Top = 0
          Width = 287
          Height = 324
          ActivePage = TabSheet12
          Align = alClient
          MultiLine = True
          TabOrder = 0
          object TabSheet12: TTabSheet
            Caption = 'Transportadora'
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
              CharCase = ecUpperCase
              TabOrder = 0
            end
            object edtEmitIE: TEdit
              Left = 137
              Top = 20
              Width = 123
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 1
            end
            object edtEmitRazao: TEdit
              Left = 8
              Top = 60
              Width = 252
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 2
            end
            object edtEmitFantasia: TEdit
              Left = 8
              Top = 100
              Width = 252
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 3
            end
            object edtEmitFone: TEdit
              Left = 8
              Top = 140
              Width = 125
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 4
            end
            object edtEmitCEP: TEdit
              Left = 137
              Top = 140
              Width = 123
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 5
            end
            object edtEmitLogradouro: TEdit
              Left = 8
              Top = 180
              Width = 196
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 6
            end
            object edtEmitNumero: TEdit
              Left = 210
              Top = 180
              Width = 50
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 7
            end
            object edtEmitComp: TEdit
              Left = 8
              Top = 220
              Width = 123
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 8
            end
            object edtEmitBairro: TEdit
              Left = 137
              Top = 220
              Width = 123
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 9
            end
            object edtEmitCodCidade: TEdit
              Left = 8
              Top = 260
              Width = 61
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 10
            end
            object edtEmitCidade: TEdit
              Left = 76
              Top = 260
              Width = 142
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 11
            end
            object edtEmitUF: TEdit
              Left = 225
              Top = 260
              Width = 35
              Height = 21
              CharCase = ecUpperCase
              TabOrder = 12
            end
          end
          object TabSheet13: TTabSheet
            Caption = 'Arquivos E.D.I.'
            ImageIndex = 4
            object sbPathOCOR: TSpeedButton
              Left = 244
              Top = 27
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
              OnClick = sbPathOCORClick
            end
            object Label35: TLabel
              Left = 3
              Top = 12
              Width = 219
              Height = 13
              Caption = 'Pasta com os Arquivos de Ocorr'#234'ncia (OCOR)'
            end
            object Label1: TLabel
              Left = 3
              Top = 60
              Width = 232
              Height = 13
              Caption = 'Pasta com os Arquivos Conhecimento (CONEMB)'
            end
            object sbPathCONEMB: TSpeedButton
              Left = 244
              Top = 75
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
              OnClick = sbPathCONEMBClick
            end
            object Label2: TLabel
              Left = 3
              Top = 108
              Width = 226
              Height = 13
              Caption = 'Pasta com os Arquivos de Cobran'#231'a (DOCCOB)'
            end
            object SpeedButton2: TSpeedButton
              Left = 244
              Top = 123
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
              OnClick = SpeedButton2Click
            end
            object Label3: TLabel
              Left = 3
              Top = 155
              Width = 228
              Height = 13
              Caption = 'Pasta com os Arquivos Notas Fiscais (NOTAFIS)'
            end
            object SpeedButton3: TSpeedButton
              Left = 244
              Top = 170
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
              OnClick = SpeedButton3Click
            end
            object Label4: TLabel
              Left = 3
              Top = 201
              Width = 223
              Height = 13
              Caption = 'Pasta com os Arquivos PRE-FATURA (PREFAT)'
            end
            object SpeedButton4: TSpeedButton
              Left = 244
              Top = 216
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
              OnClick = SpeedButton4Click
            end
            object edtPathOCOR: TEdit
              Left = 3
              Top = 28
              Width = 235
              Height = 21
              TabOrder = 0
              Text = 'C:\ACBrEDI\EDI\OCORRE\'
            end
            object edtPathCONEMB: TEdit
              Left = 3
              Top = 76
              Width = 235
              Height = 21
              TabOrder = 1
              Text = 'C:\ACBrEDI\EDI\CONEMB\'
            end
            object edtPathDOCCOB: TEdit
              Left = 3
              Top = 124
              Width = 235
              Height = 21
              TabOrder = 2
              Text = 'C:\ACBrEDI\EDI\DOCCOB\'
            end
            object edtPathNOTAFIS: TEdit
              Left = 3
              Top = 171
              Width = 235
              Height = 21
              TabOrder = 3
              Text = 'C:\ACBrEDI\EDI\NOTAFIS\'
            end
            object edtPathPREFAT: TEdit
              Left = 3
              Top = 217
              Width = 235
              Height = 21
              TabOrder = 4
              Text = 'C:\ACBrEDI\EDI\PREFAT\'
            end
            object rgVersao: TRadioGroup
              Left = 3
              Top = 244
              Width = 235
              Height = 49
              Caption = '< Vers'#227'o do E.D.I. >'
              Columns = 4
              ItemIndex = 3
              Items.Strings = (
                '3.0'
                '3.0a'
                '3.10'
                '5.0')
              TabOrder = 5
            end
          end
        end
      end
    end
    object btnSalvarConfig: TBitBtn
      Left = 85
      Top = 359
      Width = 142
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
    Height = 453
    Align = alClient
    TabOrder = 1
    object pgcBotoes: TPageControl
      Left = 1
      Top = 1
      Width = 567
      Height = 187
      ActivePage = tsOperacao
      Align = alTop
      TabOrder = 0
      object tsOperacao: TTabSheet
        Caption = 'Opera'#231#227'o'
        ImageIndex = 1
        object Button1: TButton
          Left = 25
          Top = 14
          Width = 169
          Height = 25
          Caption = 'Gerar Arquivo (OCOR)'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 25
          Top = 45
          Width = 169
          Height = 25
          Caption = 'Ler Arquivo (OCOR)'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 200
          Top = 45
          Width = 169
          Height = 25
          Caption = 'Ler Arquivo (CONEMB)'
          TabOrder = 2
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 200
          Top = 14
          Width = 169
          Height = 25
          Caption = 'Gerar Arquivo (CONEMB)'
          TabOrder = 3
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 375
          Top = 45
          Width = 169
          Height = 25
          Caption = 'Ler Arquivo (DOCCOB)'
          TabOrder = 4
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 375
          Top = 14
          Width = 169
          Height = 25
          Caption = 'Gerar Arquivo (DOCCOB)'
          TabOrder = 5
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 25
          Top = 113
          Width = 169
          Height = 25
          Caption = 'Ler Arquivo (NOTAFIS)'
          TabOrder = 6
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 25
          Top = 79
          Width = 169
          Height = 25
          Caption = 'Gerar Arquivo (NOTAFIS)'
          TabOrder = 7
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 200
          Top = 113
          Width = 169
          Height = 25
          Caption = 'Ler Arquivo (PREFAT)'
          TabOrder = 8
          OnClick = Button9Click
        end
        object Button10: TButton
          Left = 200
          Top = 79
          Width = 169
          Height = 25
          Caption = 'Gerar Arquivo (PREFAT) '
          TabOrder = 9
          OnClick = Button10Click
        end
      end
    end
    object pgRespostas: TPageControl
      Left = 1
      Top = 188
      Width = 567
      Height = 264
      ActivePage = Dados
      Align = alClient
      TabOrder = 1
      object Dados: TTabSheet
        Caption = 'Dados'
        ImageIndex = 5
        object MemoDados: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 236
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object CONEMB: TACBrEDIConhectos
    Left = 352
    Top = 224
  end
  object DOCCOB: TACBrEDICobranca
    Left = 408
    Top = 224
  end
  object NOTAFIS: TACBrEDINotaFiscais
    Left = 464
    Top = 224
  end
  object OCOR: TACBrEDIOcorrencia
    Left = 520
    Top = 224
  end
  object PREFAT: TACBrEDIPreFatura
    Left = 576
    Top = 224
  end
end
