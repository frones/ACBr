object frmPrincipal: TfrmPrincipal
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ACBr Bloco X - Demonstra'#231#227'o'
  ClientHeight = 574
  ClientWidth = 885
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
    Left = 6
    Top = 3
    Width = 52
    Height = 13
    Caption = 'Certificado'
  end
  object Label2: TLabel
    Left = 6
    Top = 82
    Width = 30
    Height = 13
    Caption = 'Senha'
  end
  object SpeedButton1: TSpeedButton
    Left = 330
    Top = 19
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label3: TLabel
    Left = 6
    Top = 125
    Width = 216
    Height = 13
    Caption = 'Recibo (Consultar/Reprocessar/Download...)'
  end
  object Label4: TLabel
    Left = 6
    Top = 211
    Width = 80
    Height = 13
    Caption = 'XML Retorno WS'
  end
  object Label5: TLabel
    Left = 6
    Top = 167
    Width = 218
    Height = 13
    Caption = 'Motivo para Cancelamento/Reprocessamento'
  end
  object Label6: TLabel
    Left = 6
    Top = 42
    Width = 79
    Height = 13
    Caption = 'N'#250'mero de S'#233'rie'
  end
  object btnGerarXMLEstoque: TButton
    Left = 421
    Top = 8
    Width = 226
    Height = 35
    Caption = 'Gerar XML Estoque'
    Enabled = False
    TabOrder = 0
    OnClick = btnGerarXMLEstoqueClick
  end
  object Edit1: TEdit
    Left = 6
    Top = 19
    Width = 321
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 6
    Top = 98
    Width = 321
    Height = 21
    PasswordChar = '*'
    TabOrder = 8
  end
  object btnGerarXMLRZ: TButton
    Left = 421
    Top = 48
    Width = 226
    Height = 35
    Caption = 'Gerar XML Redu'#231#227'o Z'
    Enabled = False
    TabOrder = 3
    OnClick = btnGerarXMLRZClick
  end
  object btnValidarEnviarXML: TButton
    Left = 421
    Top = 89
    Width = 226
    Height = 35
    Caption = 'Validar e enviar XML'
    TabOrder = 6
    OnClick = btnValidarEnviarXMLClick
  end
  object btnConsultarProcArquivo: TButton
    Left = 421
    Top = 129
    Width = 226
    Height = 35
    Caption = 'Consultar Processamento Arquivo'
    TabOrder = 9
    OnClick = btnConsultarProcArquivoClick
  end
  object edtRecibo: TEdit
    Left = 6
    Top = 140
    Width = 321
    Height = 21
    TabOrder = 10
  end
  object mmoRetWS: TMemo
    Left = 6
    Top = 230
    Width = 871
    Height = 339
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 12
  end
  object btnReprocArquivo: TButton
    Left = 651
    Top = 8
    Width = 226
    Height = 35
    Caption = 'Reprocessar Arquivo'
    TabOrder = 1
    OnClick = btnReprocArquivoClick
  end
  object btnDownloadArquivo: TButton
    Left = 651
    Top = 48
    Width = 226
    Height = 35
    Caption = 'Download Arquivo'
    TabOrder = 4
    OnClick = btnDownloadArquivoClick
  end
  object edtMotivo: TEdit
    Left = 6
    Top = 182
    Width = 321
    Height = 21
    TabOrder = 11
  end
  object btnCancArquivo: TButton
    Left = 651
    Top = 89
    Width = 226
    Height = 35
    Caption = 'Cancelar Arquivo'
    TabOrder = 7
    OnClick = btnCancArquivoClick
  end
  object edtNumSerie: TEdit
    Left = 6
    Top = 57
    Width = 321
    Height = 21
    TabOrder = 5
  end
  object ACBrBlocoX1: TACBrBlocoX
    OnAntesDeAssinar = ACBrBlocoX1AntesDeAssinar
    Configuracoes.VersaoER = erv0205
    Configuracoes.Geral.SSLLib = libWinCrypt
    Configuracoes.Geral.SSLCryptLib = cryWinCrypt
    Configuracoes.Geral.SSLHttpLib = httpWinHttp
    Configuracoes.Geral.SSLXmlSignLib = xsLibXml2
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.Ambiente = taProducao
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Left = 246
    Top = 285
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.XML'
    Filter = 'Arquivos XML|*.XML'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Salvar arquivo'
    Left = 246
    Top = 231
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'arquivos xml|*.xml'
    Title = 'Validar e enviar'
    Left = 246
    Top = 340
  end
end
