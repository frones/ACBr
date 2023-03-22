object Form1: TForm1
  Left = 192
  Top = 125
  Caption = 'Consulta de lotes enviados de CF-e-SAT'
  ClientHeight = 637
  ClientWidth = 973
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PanelClient: TPanel
    Left = 289
    Top = 0
    Width = 684
    Height = 637
    Align = alClient
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 678
    ExplicitHeight = 628
    object Label1: TLabel
      Left = 12
      Top = 16
      Width = 122
      Height = 13
      Caption = 'N'#250'mero de s'#233'rie do  SAT:'
    end
    object Label2: TLabel
      Left = 48
      Top = 48
      Width = 86
      Height = 13
      Caption = 'Data e hora incial:'
    end
    object Label3: TLabel
      Left = 48
      Top = 80
      Width = 86
      Height = 13
      Caption = 'Data e Hora Final:'
    end
    object Label4: TLabel
      Left = 32
      Top = 112
      Width = 102
      Height = 13
      Caption = 'Chave de seguran'#231'a:'
    end
    object Button1: TButton
      Left = 440
      Top = 100
      Width = 75
      Height = 25
      Caption = 'Consultar'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edchaveSeguranca: TEdit
      Left = 144
      Top = 104
      Width = 289
      Height = 21
      TabOrder = 1
    end
    object eddhFinal: TEdit
      Left = 144
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object eddhInicial: TEdit
      Left = 144
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object ednserieSAT: TEdit
      Left = 144
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 4
    end
    object trvwNFe: TTreeView
      Left = 16
      Top = 136
      Width = 513
      Height = 473
      Indent = 19
      TabOrder = 5
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 637
    ActivePage = TabConfiguracoes
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 628
    object TabConfiguracoes: TTabSheet
      Caption = 'Configura'#231#245'es'
      object Panel1: TPanel
        Left = 0
        Top = 538
        Width = 281
        Height = 71
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 529
        object Shape1: TShape
          Left = 0
          Top = 0
          Width = 281
          Height = 1
          Align = alTop
          Pen.Color = clSilver
          ExplicitWidth = 274
        end
        object Label18: TLabel
          Left = 72
          Top = 48
          Width = 149
          Height = 13
          Cursor = crHandPoint
          Caption = 'Mais sobre o Projeto ACBr'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          OnClick = Label18Click
        end
        object btnSalvarConfig: TBitBtn
          Left = 44
          Top = 9
          Width = 190
          Height = 30
          Cursor = crHandPoint
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
      end
      object PageControl2: TPageControl
        Left = 0
        Top = 0
        Width = 281
        Height = 538
        ActivePage = TabGeral
        Align = alClient
        TabOrder = 1
        ExplicitHeight = 529
        object TabGeral: TTabSheet
          Caption = 'Geral'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          object gbGeral: TGroupBox
            Left = 8
            Top = 11
            Width = 257
            Height = 288
            Caption = 'Geral'
            TabOrder = 0
            object Label11: TLabel
              Left = 8
              Top = 54
              Width = 88
              Height = 16
              Caption = 'Formato Alerta'
              Color = clBtnFace
              ParentColor = False
            end
            object Label12: TLabel
              Left = 8
              Top = 110
              Width = 114
              Height = 16
              Caption = 'Forma de Emiss'#227'o'
              Color = clBtnFace
              ParentColor = False
            end
            object Label13: TLabel
              Left = 8
              Top = 166
              Width = 88
              Height = 16
              Caption = 'Vers'#227'o Dados'
              Color = clBtnFace
              ParentColor = False
            end
            object cbxExibirErrosSchema: TCheckBox
              Left = 8
              Top = 22
              Width = 134
              Height = 23
              Caption = 'Exibir Erro Schema'
              TabOrder = 0
            end
            object edtFormatoAlerta: TEdit
              Left = 8
              Top = 72
              Width = 221
              Height = 24
              TabOrder = 1
            end
            object cbFormaEmissao: TComboBox
              Left = 8
              Top = 129
              Width = 221
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 2
            end
            object cbxRetirarAcentos: TCheckBox
              Left = 8
              Top = 221
              Width = 237
              Height = 23
              Caption = 'Retirar Acentos dos XMLs enviados'
              TabOrder = 3
            end
            object cbxRetirarEspacos: TCheckBox
              Left = 8
              Top = 248
              Width = 239
              Height = 23
              Caption = 'Retirar Espacos dos XMLs enviados'
              TabOrder = 4
            end
            object cbVersaoDados: TComboBox
              Left = 8
              Top = 184
              Width = 125
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              ItemIndex = 0
              TabOrder = 5
              Text = '0.07'
              OnChange = cbVersaoDadosChange
              Items.Strings = (
                '0.07'
                '0.08'
                '0.09')
            end
          end
          object gbLib: TGroupBox
            Left = 8
            Top = 309
            Width = 257
            Height = 163
            Caption = 'Lib'
            TabOrder = 1
            object Label14: TLabel
              Left = 8
              Top = 25
              Width = 43
              Height = 16
              Caption = 'SSLLib'
              Color = clBtnFace
              ParentColor = False
            end
            object Label15: TLabel
              Left = 8
              Top = 60
              Width = 49
              Height = 16
              Caption = 'CryptLib'
              Color = clBtnFace
              ParentColor = False
            end
            object Label16: TLabel
              Left = 8
              Top = 95
              Width = 42
              Height = 16
              Caption = 'HttpLib'
              Color = clBtnFace
              ParentColor = False
            end
            object Label17: TLabel
              Left = 8
              Top = 129
              Width = 71
              Height = 16
              Caption = 'XMLSignLib'
              Color = clBtnFace
              ParentColor = False
            end
            object cbSSLLib: TComboBox
              Left = 81
              Top = 19
              Width = 137
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 0
              OnChange = cbSSLLibChange
            end
            object cbCryptLib: TComboBox
              Left = 81
              Top = 54
              Width = 137
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 1
              OnChange = cbCryptLibChange
            end
            object cbHttpLib: TComboBox
              Left = 81
              Top = 89
              Width = 137
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 2
              OnChange = cbHttpLibChange
            end
            object cbXMLSignLib: TComboBox
              Left = 81
              Top = 123
              Width = 137
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 3
              OnChange = cbXMLSignLibChange
            end
          end
        end
        object TabWebService: TTabSheet
          Caption = 'WebService'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          object gbProxy: TGroupBox
            Left = 8
            Top = 255
            Width = 248
            Height = 131
            Caption = 'Proxy'
            TabOrder = 0
            object Label7: TLabel
              Left = 8
              Top = 23
              Width = 28
              Height = 16
              Caption = 'Host'
              Color = clBtnFace
              ParentColor = False
            end
            object Label8: TLabel
              Left = 179
              Top = 23
              Width = 32
              Height = 16
              Caption = 'Porta'
              Color = clBtnFace
              ParentColor = False
            end
            object Label9: TLabel
              Left = 8
              Top = 71
              Width = 47
              Height = 16
              Caption = 'Usu'#225'rio'
              Color = clBtnFace
              ParentColor = False
            end
            object Label10: TLabel
              Left = 133
              Top = 71
              Width = 39
              Height = 16
              Caption = 'Senha'
              Color = clBtnFace
              ParentColor = False
            end
            object edtProxyHost: TEdit
              Left = 8
              Top = 41
              Width = 160
              Height = 24
              TabOrder = 0
            end
            object edtProxyPorta: TEdit
              Left = 179
              Top = 41
              Width = 58
              Height = 24
              TabOrder = 1
            end
            object edtProxyUser: TEdit
              Left = 8
              Top = 89
              Width = 111
              Height = 24
              TabOrder = 2
            end
            object edtProxySenha: TEdit
              Left = 133
              Top = 89
              Width = 104
              Height = 24
              PasswordChar = '*'
              TabOrder = 3
            end
          end
          object gbWebService: TGroupBox
            Left = 8
            Top = 8
            Width = 248
            Height = 228
            Caption = 'WebService'
            TabOrder = 1
            object Label5: TLabel
              Left = 172
              Top = 111
              Width = 51
              Height = 16
              Caption = 'TimeOut'
              Color = clBtnFace
              ParentColor = False
            end
            object Label6: TLabel
              Left = 8
              Top = 177
              Width = 57
              Height = 16
              Caption = 'SSLType'
              Color = clBtnFace
              ParentColor = False
            end
            object rgTipoAmbiente: TRadioGroup
              Left = 8
              Top = 20
              Width = 229
              Height = 80
              Caption = 'Selecione o ambiente de Destino'
              Columns = 2
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Pitch = fpVariable
              Font.Style = []
              Font.Quality = fqDraft
              ItemIndex = 0
              Items.Strings = (
                'Produ'#231#227'o'
                'Homologa'#231#227'o')
              ParentFont = False
              TabOrder = 0
            end
            object cbxSalvarSOAP: TCheckBox
              Left = 8
              Top = 133
              Width = 159
              Height = 23
              Caption = 'Salvar envelope SOAP'
              TabOrder = 1
            end
            object seTimeOut: TSpinEdit
              Left = 172
              Top = 131
              Width = 65
              Height = 26
              Increment = 10
              MaxValue = 999999
              MinValue = 1000
              TabOrder = 2
              Value = 5000
            end
            object cbxVisualizar: TCheckBox
              Left = 8
              Top = 111
              Width = 148
              Height = 23
              Caption = 'Visualizar Mensagem'
              TabOrder = 3
            end
            object cbSSLType: TComboBox
              Left = 85
              Top = 175
              Width = 152
              Height = 24
              Style = csOwnerDrawFixed
              ItemHeight = 18
              TabOrder = 4
              OnChange = cbSSLTypeChange
            end
          end
        end
      end
    end
  end
  object ACBrSATWS1: TACBrSATWS
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Left = 480
    Top = 8
  end
end
