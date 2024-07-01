object Form1: TForm1
  Left = 451
  Top = 163
  Caption = 'CEP Teste'
  ClientHeight = 450
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 136
    Width = 600
    Height = 314
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 600
    Height = 136
    ActivePage = tsIBGE
    Align = alTop
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Configura'#231#227'o'
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 180
        Height = 108
        Align = alLeft
        Caption = 'WebService'
        TabOrder = 0
        object Label9: TLabel
          Left = 7
          Top = 57
          Width = 34
          Height = 13
          Caption = 'Chave:'
          Color = clBtnFace
          ParentColor = False
        end
        object cbxWS: TComboBox
          Left = 7
          Top = 24
          Width = 156
          Height = 21
          Style = csDropDownList
          TabOrder = 0
        end
        object edChaveBuscarCEP: TEdit
          Left = 7
          Top = 72
          Width = 156
          Height = 21
          TabOrder = 1
        end
      end
      object GroupBox1: TGroupBox
        Left = 180
        Top = 0
        Width = 412
        Height = 108
        Align = alClient
        Caption = 'Proxy'
        TabOrder = 1
        object Label2: TLabel
          Left = 22
          Top = 16
          Width = 22
          Height = 13
          Caption = 'Host'
          Color = clBtnFace
          ParentColor = False
        end
        object Label3: TLabel
          Left = 191
          Top = 16
          Width = 25
          Height = 13
          Caption = 'Porta'
          Color = clBtnFace
          ParentColor = False
        end
        object Label4: TLabel
          Left = 22
          Top = 57
          Width = 36
          Height = 13
          Caption = 'Usu'#225'rio'
          Color = clBtnFace
          ParentColor = False
        end
        object Label5: TLabel
          Left = 142
          Top = 57
          Width = 31
          Height = 13
          Caption = 'Senha'
          Color = clBtnFace
          ParentColor = False
        end
        object edProxyHost: TEdit
          Left = 22
          Top = 30
          Width = 154
          Height = 21
          TabOrder = 0
        end
        object edProxyPort: TEdit
          Left = 191
          Top = 32
          Width = 56
          Height = 21
          TabOrder = 1
        end
        object edProxyUser: TEdit
          Left = 22
          Top = 72
          Width = 105
          Height = 21
          TabOrder = 2
        end
        object edProxyPass: TEdit
          Left = 142
          Top = 72
          Width = 105
          Height = 21
          TabOrder = 3
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Buscar CEP'
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 164
        Height = 108
        Align = alLeft
        Caption = 'Por CEP'
        TabOrder = 0
        object edCEP: TEdit
          Left = 26
          Top = 26
          Width = 112
          Height = 21
          TabOrder = 0
          Text = '18270-170'
        end
        object bBuscarCEP: TButton
          Left = 26
          Top = 59
          Width = 112
          Height = 33
          Caption = 'Buscar'
          TabOrder = 1
          OnClick = bBuscarCEPClick
        end
      end
      object GroupBox4: TGroupBox
        Left = 164
        Top = 0
        Width = 428
        Height = 108
        Align = alClient
        Caption = 'Por Endere'#231'o'
        TabOrder = 1
        object Label1: TLabel
          Left = 70
          Top = 17
          Width = 57
          Height = 13
          Caption = 'Logradouro:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label6: TLabel
          Left = 14
          Top = 60
          Width = 36
          Height = 13
          Caption = 'Cidade:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label7: TLabel
          Left = 159
          Top = 60
          Width = 17
          Height = 13
          Caption = 'UF:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label8: TLabel
          Left = 198
          Top = 60
          Width = 76
          Height = 13
          Caption = 'Bairro (opcional)'
          Color = clBtnFace
          ParentColor = False
        end
        object Label10: TLabel
          Left = 14
          Top = 17
          Width = 24
          Height = 13
          Caption = 'Tipo:'
          Color = clBtnFace
          ParentColor = False
        end
        object edLogradouro: TEdit
          Left = 70
          Top = 33
          Width = 264
          Height = 21
          TabOrder = 1
          Text = 'Coronel Aureliano'
        end
        object bBuscarLogradouro: TButton
          Left = 347
          Top = 24
          Width = 56
          Height = 72
          Caption = 'Buscar'
          TabOrder = 5
          OnClick = bBuscarLogradouroClick
        end
        object edCidade: TEdit
          Left = 14
          Top = 76
          Width = 136
          Height = 21
          TabOrder = 2
          Text = 'Tatu'#237
        end
        object edUF: TEdit
          Left = 159
          Top = 76
          Width = 25
          Height = 21
          TabOrder = 3
          Text = 'SP'
        end
        object edBairro: TEdit
          Left = 198
          Top = 77
          Width = 136
          Height = 21
          TabOrder = 4
        end
        object edTipo_Logradouro: TEdit
          Left = 14
          Top = 33
          Width = 48
          Height = 21
          TabOrder = 0
          Text = 'Rua'
        end
      end
    end
    object tsIBGE: TTabSheet
      Caption = 'Buscar IBGE'
      object GroupBox5: TGroupBox
        Left = 0
        Top = 0
        Width = 164
        Height = 108
        Align = alLeft
        Caption = 'Por C'#243'digo'
        TabOrder = 0
        object edIBGECod: TEdit
          Left = 26
          Top = 26
          Width = 112
          Height = 21
          TabOrder = 0
          Text = '3554003'
        end
        object bBuscarCEP1: TButton
          Left = 26
          Top = 59
          Width = 112
          Height = 33
          Caption = 'Buscar'
          TabOrder = 1
          OnClick = bBuscarCEP1Click
        end
      end
      object GroupBox6: TGroupBox
        Left = 164
        Top = 0
        Width = 428
        Height = 108
        Align = alClient
        Caption = 'Por Nome'
        TabOrder = 1
        DesignSize = (
          428
          108)
        object bBuscarLogradouro1: TButton
          Left = 38
          Top = 59
          Width = 112
          Height = 33
          Caption = 'Buscar'
          TabOrder = 1
          OnClick = bBuscarLogradouro1Click
        end
        object edIBGENome: TEdit
          Left = 38
          Top = 26
          Width = 348
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'Bragan'#231'a Paulista'
        end
        object cbIgnorar: TCheckBox
          Left = 184
          Top = 64
          Width = 153
          Height = 17
          Caption = 'Ignorar Caixa e Acentos'
          TabOrder = 2
        end
      end
    end
  end
  object ACBrCEP1: TACBrCEP
    ProxyPort = '8080'
    OnAntesAbrirHTTP = ACBrCEP1AntesAbrirHTTP
    WebService = wsBuscarCep
    PesquisarIBGE = True
    OnBuscaEfetuada = ACBrCEP1BuscaEfetuada
    Left = 248
    Top = 160
  end
  object ACBrIBGE1: TACBrIBGE
    ProxyPort = '8080'
    OnAntesAbrirHTTP = ACBrIBGE1AntesAbrirHTTP
    CacheArquivo = 'ACBrIBGE.txt'
    OnBuscaEfetuada = ACBrIBGE1BuscaEfetuada
    Left = 312
    Top = 160
  end
end
