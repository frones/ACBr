object FNFSe: TFNFSe
  Left = 0
  Top = 0
  Caption = 'NFS-e'
  ClientHeight = 397
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  TextHeight = 15
  object G: TGroupBox
    Left = 8
    Top = 8
    Width = 537
    Height = 385
    Caption = 'NFS-e'
    TabOrder = 0
    object ckVariosItens: TCheckBox
      Left = 151
      Top = 311
      Width = 129
      Height = 17
      Caption = 'For'#231'ar V'#225'rios Itens'
      TabOrder = 4
    end
    object rgTipoImpressao: TRadioGroup
      Left = 151
      Top = 56
      Width = 129
      Height = 132
      Caption = 'Formato da Impress'#227'o'
      ItemIndex = 0
      Items.Strings = (
        'Autom'#225'tico'
        'Retrato'
        'Paisagem'
        'Simplificado'
        'Etiqueta')
      TabOrder = 1
    end
    object Button1: TButton
      Left = 11
      Top = 25
      Width = 75
      Height = 25
      Caption = 'DANFSE'
      TabOrder = 0
      OnClick = Button1Click
    end
    object rgStatus: TRadioGroup
      Left = 286
      Top = 56
      Width = 158
      Height = 132
      Caption = 'Situa'#231#227'o'
      ItemIndex = 0
      Items.Strings = (
        'Autom'#225'tico'
        'Sem protocolo'
        'Cancelada'
        'Denegada')
      TabOrder = 2
    end
    object gbLogomarca: TGroupBox
      Left = 151
      Top = 194
      Width = 174
      Height = 103
      Caption = 'Op'#231#245'es'
      TabOrder = 3
      object ckLogomarcaPrefeitura: TCheckBox
        Left = 16
        Top = 24
        Width = 145
        Height = 17
        Caption = 'Logomarca Prefeitura'
        TabOrder = 0
      end
      object ckLogomarcaPrestador: TCheckBox
        Left = 16
        Top = 47
        Width = 137
        Height = 17
        Caption = 'Logomarca Prestador'
        TabOrder = 1
      end
      object ckQRCode: TCheckBox
        Left = 16
        Top = 70
        Width = 81
        Height = 17
        Caption = 'QR Code'
        TabOrder = 2
      end
    end
    object ckHomologacao: TCheckBox
      Left = 151
      Top = 334
      Width = 145
      Height = 17
      Caption = 'For'#231'ar Homologa'#231#227'o'
      TabOrder = 5
    end
    object rbProvedor: TRadioGroup
      Left = 11
      Top = 56
      Width = 134
      Height = 313
      Caption = 'Provedor'
      ItemIndex = 0
      Items.Strings = (
        'ISS Curitiba'
        'ISSDSF'
        'ISS S'#227'o Paulo'
        'Betha'
        'Ginfes'
        'Tiplan')
      TabOrder = 6
    end
    object ckOutrasInformacoes: TCheckBox
      Left = 151
      Top = 357
      Width = 161
      Height = 17
      Caption = 'For'#231'ar Outras Informa'#231#245'es'
      TabOrder = 7
    end
  end
  object ACBrNFSeX1: TACBrNFSeX
    Configuracoes.Geral.SSLLib = libNone
    Configuracoes.Geral.SSLCryptLib = cryNone
    Configuracoes.Geral.SSLHttpLib = httpNone
    Configuracoes.Geral.SSLXmlSignLib = xsNone
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.Geral.CodigoMunicipio = 0
    Configuracoes.Geral.Provedor = proNenhum
    Configuracoes.Geral.Versao = ve100
    Configuracoes.Arquivos.OrdenacaoPath = <>
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Left = 240
    Top = 24
  end
  object ACBrIBGE1: TACBrIBGE
    ProxyPort = '8080'
    CacheArquivo = 'ACBrIBGE.txt'
    Left = 352
    Top = 32
  end
end
