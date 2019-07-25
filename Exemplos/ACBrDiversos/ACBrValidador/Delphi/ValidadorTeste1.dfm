object frValidador: TfrValidador
  Left = 282
  Top = 156
  Width = 352
  Height = 286
  HorzScrollBar.Range = 329
  VertScrollBar.Range = 243
  ActiveControl = edDocto
  Caption = 'Validador de Documentos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'Documento'
  end
  object Label2: TLabel
    Left = 232
    Top = 8
    Width = 21
    Height = 13
    Caption = 'Tipo'
  end
  object Label3: TLabel
    Left = 8
    Top = 144
    Width = 89
    Height = 13
    Caption = 'Mensagem de Erro'
  end
  object Label4: TLabel
    Left = 144
    Top = 8
    Width = 64
    Height = 13
    Caption = 'Complemento'
  end
  object Label5: TLabel
    Left = 8
    Top = 56
    Width = 58
    Height = 13
    Caption = 'Ignorar Char'
  end
  object edDocto: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    Cursor = crIBeam
    TabOrder = 0
    OnChange = edDoctoChange
  end
  object cbTipoDocto: TComboBox
    Left = 232
    Top = 24
    Width = 97
    Height = 21
    Style = csDropDownList
    DropDownCount = 20
    TabOrder = 2
    OnChange = cbTipoDoctoChange
    Items.Strings = (
      'docCPF'
      'docCNPJ'
      'docUF'
      'docInscEst'
      'docNumCheque'
      'docPIS'
      'docCEP'
      'docCartaoCredito'
      'docSuframa'
      'docGTIN'
      'docRenavam'
      'docEmail'
      'docCNH'
      'docPrefixoGTIN'
      'docCAEPF'
      'docPlacaMercosul')
  end
  object mMsgErro: TMemo
    Left = 8
    Top = 160
    Width = 321
    Height = 49
    ReadOnly = True
    TabOrder = 8
  end
  object bValidar: TButton
    Left = 173
    Top = 215
    Width = 75
    Height = 25
    Caption = '&Validar'
    Default = True
    TabOrder = 9
    OnClick = bValidarClick
  end
  object edComple: TEdit
    Left = 144
    Top = 24
    Width = 73
    Height = 21
    Cursor = crIBeam
    TabOrder = 1
  end
  object edIgnorar: TEdit
    Left = 8
    Top = 72
    Width = 57
    Height = 21
    Cursor = crIBeam
    TabOrder = 4
  end
  object cbPermiteVazio: TCheckBox
    Left = 144
    Top = 56
    Width = 153
    Height = 17
    Caption = 'Permite Vazio'
    TabOrder = 3
    OnClick = cbPermiteVazioClick
  end
  object cbAjustarTam: TCheckBox
    Left = 144
    Top = 80
    Width = 153
    Height = 17
    Caption = 'Ajustar Tamanho'
    TabOrder = 5
    OnClick = cbAjustarTamClick
  end
  object bFormatar: TButton
    Left = 254
    Top = 215
    Width = 75
    Height = 25
    Caption = 'Formatar'
    TabOrder = 10
    OnClick = bFormatarClick
  end
  object cbException: TCheckBox
    Left = 144
    Top = 128
    Width = 153
    Height = 17
    Caption = 'Gerar Exce'#231#227'o'
    TabOrder = 7
    OnClick = cbExceptionClick
  end
  object cbExibeDigCorreto: TCheckBox
    Left = 144
    Top = 104
    Width = 153
    Height = 17
    Caption = 'Exibe Digito Calculado'
    TabOrder = 6
    OnClick = cbExibeDigCorretoClick
  end
  object ACBrValidador1: TACBrValidador
    IgnorarChar = './-'
    OnMsgErro = ACBrValidador1MsgErro
    Left = 16
    Top = 216
  end
end
