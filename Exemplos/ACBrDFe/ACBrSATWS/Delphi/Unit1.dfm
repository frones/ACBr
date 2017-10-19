object Form1: TForm1
  Left = 192
  Top = 125
  Width = 567
  Height = 675
  Caption = 'Consulta de lotes enviados de CF-e-SAT'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
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
  object ednserieSAT: TEdit
    Left = 144
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object eddhInicial: TEdit
    Left = 144
    Top = 40
    Width = 121
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
  object edchaveSeguranca: TEdit
    Left = 144
    Top = 104
    Width = 289
    Height = 21
    TabOrder = 3
  end
  object Button1: TButton
    Left = 440
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Consultar'
    TabOrder = 4
    OnClick = Button1Click
  end
  object trvwNFe: TTreeView
    Left = 16
    Top = 136
    Width = 513
    Height = 473
    Indent = 19
    TabOrder = 5
  end
  object ACBrSATWS1: TACBrSATWS
    Configuracoes.Geral.SSLLib = libCapicomDelphiSoap
    Configuracoes.Geral.FormatoAlerta = 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'
    Configuracoes.WebServices.UF = 'SP'
    Configuracoes.WebServices.AguardarConsultaRet = 0
    Configuracoes.WebServices.QuebradeLinha = '|'
    Left = 480
    Top = 8
  end
end
