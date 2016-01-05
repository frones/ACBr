object frmPrincipal: TfrmPrincipal
  Left = 422
  Top = 332
  BorderStyle = bsDialog
  Caption = 'Demo ACBRCargaBal'
  ClientHeight = 209
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 14
    Width = 34
    Height = 13
    Caption = 'Modelo'
  end
  object Label2: TLabel
    Left = 15
    Top = 57
    Width = 198
    Height = 13
    Caption = 'Diret'#243'rio onde ser'#227'o gerados os arquivos'
  end
  object lblStatus: TLabel
    Left = 15
    Top = 110
    Width = 41
    Height = 13
    Caption = 'lblStatus'
  end
  object cbxModelo: TComboBox
    Left = 15
    Top = 30
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'Filizola'
      'Toledo'
      'Urano'
      'ToledoMGV5')
  end
  object edtDiretorio: TEdit
    Left = 15
    Top = 73
    Width = 391
    Height = 21
    TabOrder = 1
    Text = 'edtDiretorio'
  end
  object btnEscolherDiretorio: TButton
    Left = 412
    Top = 71
    Width = 29
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = btnEscolherDiretorioClick
  end
  object btnGerarArquivo: TButton
    Left = 233
    Top = 170
    Width = 101
    Height = 25
    Caption = 'Gerar arquivo'
    TabOrder = 3
    OnClick = btnGerarArquivoClick
  end
  object btnFechar: TButton
    Left = 340
    Top = 170
    Width = 101
    Height = 25
    Cancel = True
    Caption = 'Fechar'
    TabOrder = 4
    OnClick = btnFecharClick
  end
  object ProgressBar1: TProgressBar
    Left = 15
    Top = 125
    Width = 426
    Height = 17
    TabOrder = 5
  end
  object ACBrCargaBal1: TACBrCargaBal
    Modelo = modFilizola
    OnProgresso = ACBrCargaBal1Progresso
    Left = 35
    Top = 160
  end
end
