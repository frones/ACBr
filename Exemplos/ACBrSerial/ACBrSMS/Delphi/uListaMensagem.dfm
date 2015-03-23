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
      Left = 470
      Top = 42
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
  object lstMensagens: TListView
    Left = 0
    Top = 106
    Width = 790
    Height = 344
    Align = alClient
    Columns = <
      item
        Caption = 'Data/Hora'
        Width = 150
      end
      item
        Caption = 'N'#250'mero'
        Width = 150
      end
      item
        Caption = 'Mensagem'
        Width = 350
      end>
    Items.ItemData = {
      034E0000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000000000000FF
      FFFFFFFFFFFFFF00000000FFFFFFFF0000000000}
    TabOrder = 1
    ViewStyle = vsReport
  end
end
