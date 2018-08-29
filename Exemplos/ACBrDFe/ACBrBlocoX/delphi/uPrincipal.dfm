object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  Caption = 'ACBr Bloco X - Demonstra'#231#227'o'
  ClientHeight = 350
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 30
    Top = 34
    Width = 52
    Height = 13
    Caption = 'Certificado'
  end
  object Label2: TLabel
    Left = 30
    Top = 77
    Width = 30
    Height = 13
    Caption = 'Senha'
  end
  object SpeedButton1: TSpeedButton
    Left = 277
    Top = 50
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Button1: TButton
    Left = 465
    Top = 43
    Width = 226
    Height = 71
    Caption = 'Gerar XML Estoque'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 30
    Top = 50
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 30
    Top = 93
    Width = 241
    Height = 21
    TabOrder = 2
  end
  object Button2: TButton
    Left = 465
    Top = 120
    Width = 226
    Height = 71
    Caption = 'Gerar XML Redu'#231#227'o Z'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 465
    Top = 252
    Width = 226
    Height = 71
    Caption = 'Validar e enviar XML'
    TabOrder = 4
    OnClick = Button3Click
  end
  object ACBrBlocoX1: TACBrBlocoX
    Configuracoes.VersaoER = erv0205
    Configuracoes.Geral.SSLLib = libWinCrypt
    Configuracoes.Geral.SSLCryptLib = cryWinCrypt
    Configuracoes.Geral.SSLHttpLib = httpWinHttp
    Configuracoes.Geral.SSLXmlSignLib = xsLibXml2
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Left = 310
    Top = 125
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.XML'
    Filter = 'Arquivos XML|*.XML'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Salvar arquivo'
    Left = 230
    Top = 135
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'arquivos xml|*.xml'
    Title = 'Validar e enviar'
    Left = 150
    Top = 140
  end
end
