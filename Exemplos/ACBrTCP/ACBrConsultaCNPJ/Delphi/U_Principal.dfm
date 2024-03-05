object F_Principal: TF_Principal
  Left = 336
  Top = 105
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Consulta de CNPJ'
  ClientHeight = 658
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Panel2: TPanel
    Left = 8
    Top = 201
    Width = 633
    Height = 456
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 11
      Width = 98
      Height = 16
      Caption = 'Tipo de Empresa'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 112
      Top = 11
      Width = 77
      Height = 16
      Caption = 'Raz'#227'o Social'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 536
      Top = 11
      Width = 49
      Height = 16
      Caption = 'Abertura'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 192
      Top = 59
      Width = 59
      Height = 16
      Caption = 'Endere'#231'o:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 486
      Top = 59
      Width = 45
      Height = 16
      Caption = 'N'#250'mero'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 105
      Width = 80
      Height = 16
      Caption = 'Complemento'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 344
      Top = 105
      Width = 34
      Height = 16
      Caption = 'Bairro'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 8
      Top = 153
      Width = 40
      Height = 16
      Caption = 'Cidade'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 305
      Top = 153
      Width = 17
      Height = 16
      Caption = 'UF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 344
      Top = 153
      Width = 27
      Height = 16
      Caption = 'CEP'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 464
      Top = 153
      Width = 51
      Height = 16
      Caption = 'Situa'#231#227'o'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 8
      Top = 59
      Width = 50
      Height = 16
      Caption = 'Fantasia'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 8
      Top = 249
      Width = 90
      Height = 16
      Caption = 'CNAE Principal'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 8
      Top = 299
      Width = 120
      Height = 16
      Caption = 'CNAE Secund'#225'rio(s)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label17: TLabel
      Left = 8
      Top = 201
      Width = 117
      Height = 16
      Caption = 'Endere'#231'o Eletr'#244'nico'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 344
      Top = 201
      Width = 47
      Height = 16
      Caption = 'Telefone'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label19: TLabel
      Left = 473
      Top = 11
      Width = 31
      Height = 16
      Caption = 'Porte'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object EditTipo: TEdit
      Left = 8
      Top = 29
      Width = 98
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object EditRazaoSocial: TEdit
      Left = 112
      Top = 29
      Width = 355
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object EditAbertura: TEdit
      Left = 536
      Top = 29
      Width = 88
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object EditEndereco: TEdit
      Left = 192
      Top = 77
      Width = 288
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object EditNumero: TEdit
      Left = 486
      Top = 77
      Width = 138
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object EditComplemento: TEdit
      Left = 8
      Top = 123
      Width = 330
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object EditBairro: TEdit
      Left = 344
      Top = 123
      Width = 280
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object EditCidade: TEdit
      Left = 8
      Top = 171
      Width = 289
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object EditUF: TEdit
      Left = 305
      Top = 171
      Width = 33
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object EditCEP: TEdit
      Left = 344
      Top = 171
      Width = 114
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
    object EditSituacao: TEdit
      Left = 464
      Top = 171
      Width = 160
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
    end
    object EditFantasia: TEdit
      Left = 8
      Top = 77
      Width = 178
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object ListCNAE2: TListBox
      Left = 8
      Top = 318
      Width = 616
      Height = 125
      ItemHeight = 13
      TabOrder = 16
    end
    object EditCNAE1: TEdit
      Left = 8
      Top = 269
      Width = 616
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
    end
    object EditEmail: TEdit
      Left = 8
      Top = 219
      Width = 330
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
    end
    object EditTelefone: TEdit
      Left = 344
      Top = 219
      Width = 280
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
    end
    object EditPorte: TEdit
      Left = 473
      Top = 29
      Width = 57
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 634
    Height = 187
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 2
      Width = 630
      Height = 183
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Consulta'
        object Label1: TLabel
          Left = 28
          Top = 84
          Width = 85
          Height = 16
          Caption = 'Digite o CNPJ:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label14: TLabel
          Left = 28
          Top = 10
          Width = 50
          Height = 16
          Caption = 'Provedor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object EditCNPJ: TMaskEdit
          Left = 28
          Top = 98
          Width = 288
          Height = 41
          EditMask = '00.000.000/0000-00;1;_'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -27
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 18
          ParentFont = False
          TabOrder = 0
          Text = '  .   .   /    -  '
        end
        object cbbProvedor: TComboBox
          Left = 28
          Top = 32
          Width = 288
          Height = 21
          TabOrder = 1
          Text = 'cbbProvedor'
        end
        object ButBuscar: TBitBtn
          Left = 331
          Top = 53
          Width = 187
          Height = 86
          Caption = 'Consultar'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          Glyph.Data = {
            E6040000424DE604000000000000360000002800000014000000140000000100
            180000000000B0040000120B0000120B00000000000000000000FFFFFFFDFDFD
            C1C1C1A7A7A7F9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6D4D6C8C6C89F
            9F9FAEAEAEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEDEBECD4CBCEC9C7C89595
            95CCCCCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAE7E8D4CCCFBAB9B98D8D8D
            EDEDEDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDCDDD3CBCEAEAEAE949494FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6D0D2CDC7C99A9A9AB4B4B4FFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2CCCEC6C0C2868686E3E3E3FFFFFF
            FBFBFBEDEDEDECECECF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFED7CFD1B6B3B3808081A2A2A2949496A3
            A3A49E9EA089898AABABABF3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB9B8B9B7B7B9E4E3E4FCFCFDFFFFFFFFFF
            FFFAFAFAD2D2D2838384E4E4E4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF1F1F1ADADAFF3F3F3F9F9F9F8F8F8FAFAFAFAFAFAFAFAFA
            FBFBFBECECEC878788F6F6F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFAFAFB1ECEBEBF3F3F3F4F4F4F6F6F6F6F6F6F7F7F7F6F6F6F6F6F6F8
            F8F8D8D8D9AAAAAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8
            BAB9BBF0F0F0EFEFEFF4F4F4F4F4F4F4F4F4F5F5F5F4F4F4F5F5F5F4F4F4F6F6
            F6929293DFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAEAEBC8C7C8ED
            EDEDEFEFEFF3F3F3F2F2F2F2F2F2F3F3F3F3F3F3F3F3F3F4F4F4F7F7F7B1B1B2
            C0C0C1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E9BFBEBEEAEAEAEDED
            EDF0F0F0F1F1F1F1F1F1F2F2F2F2F2F2F2F2F2F2F2F2F4F4F4B5B5B7C2C2C3FF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2F3ADACACE4E5E4E7E7E7E8E8E8
            E8E8E8E8E8E8EAEAEAEAEAEAECECECEEEEEEF2F2F2AFAFB0DBDBDCFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB1B0B0CACACAF1F1F1EEEEEEEDEDEDEB
            EBEBECECECEEEEEEEFEFEFF2F2F2EBEAEBB1B1B2FFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFE7E7E7969695DFDFDFF2F3F3EEEEEEEDEDEDEFEF
            EFEFEFEFF2F2F2F0F0F0B2B2B4F3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFD6D6D69C9B9BCFCFCFE8E8E8EEEEEEF0F0F0ECECEC
            DDDDDDB3B3B4D6D6D6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFE9E9E9B0B0AFA4A3A3ACACABB0B0AFB0AFAFAEAEAFE5
            E5E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFAFAFAE6E6E6D9D8D8DADADAE4E3E4FCFCFCFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFF}
          ParentFont = False
          TabOrder = 2
          OnClick = ButBuscarClick
        end
      end
      object Proxy: TTabSheet
        Caption = 'Proxy'
        ImageIndex = 1
        object Label20: TLabel
          Left = 72
          Top = 24
          Width = 22
          Height = 13
          Caption = 'Host'
        end
        object Label21: TLabel
          Left = 72
          Top = 51
          Width = 20
          Height = 13
          Caption = 'Port'
        end
        object Label24: TLabel
          Left = 72
          Top = 105
          Width = 30
          Height = 13
          Caption = 'Senha'
        end
        object Label25: TLabel
          Left = 72
          Top = 78
          Width = 36
          Height = 13
          Caption = 'Usuario'
        end
        object edtHost: TEdit
          Left = 136
          Top = 21
          Width = 121
          Height = 21
          ImeName = 'edtHost'
          TabOrder = 0
        end
        object edtPort: TEdit
          Left = 136
          Top = 48
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtUsuario: TEdit
          Left = 136
          Top = 75
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object EdtSenha: TEdit
          Left = 136
          Top = 100
          Width = 121
          Height = 21
          ImeName = '1EdtSenha'
          TabOrder = 3
        end
      end
    end
  end
  object ACBrConsultaCNPJ1: TACBrConsultaCNPJ
    ProxyHost = ''
    ProxyPort = '8080'
    ProxyUser = ''
    ProxyPass = ''
    PesquisarIBGE = False
    IniServicos = 
      'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\ACBrConsultaC' +
      'NPJServicos.ini'
    Left = 568
    Top = 138
  end
end
