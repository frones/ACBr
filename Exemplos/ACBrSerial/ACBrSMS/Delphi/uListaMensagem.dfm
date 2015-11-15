object frmListaMensagem: TfrmListaMensagem
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Lista de mensagens'
  ClientHeight = 450
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox4: TGroupBox
    Left = 0
    Top = 0
    Width = 790
    Height = 106
    Align = alTop
    TabOrder = 0
    object btnListarMensagens: TButton
      Left = 445
      Top = 71
      Width = 145
      Height = 25
      Caption = 'Listar mensagens'
      TabOrder = 1
      OnClick = btnListarMensagensClick
    end
    object rgdFiltroMsg: TRadioGroup
      Left = 10
      Top = 5
      Width = 429
      Height = 42
      Caption = 'Filtro'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Todas'
        'Somente lidas'
        'Somente n'#227'o lidas')
      TabOrder = 0
    end
    object rdgBandeja: TRadioGroup
      Left = 10
      Top = 53
      Width = 429
      Height = 43
      Caption = 'Bandeja'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Sincard 1'
        'Sincard 2')
      TabOrder = 2
    end
  end
  object dbgMensagens: TDBGrid
    Left = 0
    Top = 106
    Width = 790
    Height = 344
    Align = alClient
    DataSource = dsMensagens
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object dsMensagens: TDataSource
    AutoEdit = False
    Left = 40
    Top = 152
  end
end
