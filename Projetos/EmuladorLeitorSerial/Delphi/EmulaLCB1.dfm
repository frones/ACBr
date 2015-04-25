object frEmulador: TfrEmulador
  Left = 277
  Top = 256
  Width = 372
  Height = 216
  HorzScrollBar.Range = 339
  VertScrollBar.Range = 180
  ActiveControl = edBarra
  BorderStyle = bsToolWindow
  Caption = 'Emulando Leitor Serial'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 77
    Width = 75
    Height = 13
    Caption = 'Código a Enviar'
  end
  object Label4: TLabel
    Left = 272
    Top = 77
    Width = 25
    Height = 13
    Caption = 'Porta'
  end
  object Label2: TLabel
    Left = 11
    Top = 8
    Width = 323
    Height = 57
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Use uma porta Serial diferente da especificada no ACBrLCB. Essa ' +
      'janela envia Strings pela Serial simulando um Leitor de Cod.Barr' +
      'as. Use o mesmo cabo utilizado pelos Emuladores de ECF para liga' +
      'r as duas Portas Seriais'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 192
    Top = 77
    Width = 29
    Height = 13
    Caption = 'Sufixo'
  end
  object Label3: TLabel
    Left = 16
    Top = 160
    Width = 81
    Height = 13
    Caption = 'Código Enviado: '
  end
  object lEnviado: TLabel
    Left = 107
    Top = 160
    Width = 230
    Height = 20
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edBarra: TEdit
    Left = 10
    Top = 93
    Width = 169
    Height = 21
    Cursor = crIBeam
    TabOrder = 0
    Text = '7893000440166'
  end
  object cbxPorta: TComboBox
    Left = 272
    Top = 93
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'COM2'
    OnChange = cbxPortaChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'LPT1'
      'LPT2'
      'LPT3')
  end
  object edSufixo: TEdit
    Left = 192
    Top = 93
    Width = 65
    Height = 21
    Cursor = crIBeam
    TabOrder = 1
    Text = '#13'
  end
  object Button1: TButton
    Left = 264
    Top = 129
    Width = 75
    Height = 25
    Caption = 'Enviar'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object cbMudaCodigo: TCheckBox
    Left = 8
    Top = 120
    Width = 185
    Height = 31
    Caption = 'Mudar Código Automáticamente'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
