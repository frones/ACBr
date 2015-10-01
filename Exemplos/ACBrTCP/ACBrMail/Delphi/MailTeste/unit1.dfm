object Form1: TForm1
  Left = 303
  Top = 194
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Exemplo ACBrMail'
  ClientHeight = 349
  ClientWidth = 751
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 232
    Width = 194
    Height = 30
    Alignment = taCenter
    AutoSize = False
    Caption = 'Altere os par'#226'metros no c'#243'digo'#13#10'antes de fazer os envios.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 213
    Top = 6
    Width = 36
    Height = 13
    Caption = 'Subject'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 213
    Top = 56
    Width = 69
    Height = 13
    Caption = 'Alt Body (TXT)'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 216
    Top = 160
    Width = 61
    Height = 13
    Caption = 'Body (HTML)'
    Color = clBtnFace
    ParentColor = False
  end
  object Label5: TLabel
    Left = 520
    Top = 6
    Width = 20
    Height = 13
    Caption = 'LOG'
    Color = clBtnFace
    ParentColor = False
  end
  object bEnviar: TButton
    Left = 8
    Top = 280
    Width = 194
    Height = 25
    Caption = 'Enviar Email'
    TabOrder = 6
    OnClick = bEnviarClick
  end
  object mLog: TMemo
    Left = 520
    Top = 24
    Width = 219
    Height = 280
    TabOrder = 11
  end
  object ProgressBar1: TProgressBar
    Left = 213
    Top = 317
    Width = 526
    Height = 20
    Max = 11
    Step = 1
    TabOrder = 12
  end
  object mAltBody: TMemo
    Left = 213
    Top = 72
    Width = 283
    Height = 80
    Lines.Strings = (
      'Linha 1'
      'Linha 2'
      '   Teste de e-mail'
      #193#201#205#211#218#231#199#227#194)
    TabOrder = 9
    WordWrap = False
  end
  object edSubject: TEdit
    Left = 213
    Top = 24
    Width = 283
    Height = 21
    TabOrder = 8
    Text = 'Teste de Envio '#193#201#205#211#218#199#231#225#233#237#250#243' - 0'
  end
  object cbUsarThread: TCheckBox
    Left = 16
    Top = 160
    Width = 80
    Height = 19
    Caption = 'Usar thread'
    TabOrder = 5
  end
  object bEnviarLote: TButton
    Left = 8
    Top = 312
    Width = 194
    Height = 25
    Caption = 'Enviar 5 emails por Thread'
    TabOrder = 7
    OnClick = bEnviarLoteClick
  end
  object cbUsarTXT: TCheckBox
    Left = 16
    Top = 16
    Width = 158
    Height = 19
    Caption = 'Enviar Mensagem em TXT'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cbUsarHTML: TCheckBox
    Left = 16
    Top = 40
    Width = 170
    Height = 19
    Caption = 'Enviar Mensagem em HTML'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object cbAddImgHTML: TCheckBox
    Left = 16
    Top = 64
    Width = 156
    Height = 19
    Caption = 'Incluir Imagem em HTML'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbAddPDF: TCheckBox
    Left = 16
    Top = 112
    Width = 129
    Height = 19
    Caption = 'Incluir Anexo de PDF'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object mBody: TMemo
    Left = 213
    Top = 176
    Width = 283
    Height = 128
    Lines.Strings = (
      
        '<html><head><meta http-equiv="content-type" content="text/html; ' +
        'charset=UTF-8"></head>'
      '<body text="#000000" bgcolor="#FFFFFF">'
      '<h1>Texto em HTML.</h1><br>'
      '<p>Teste de Envio '#193#201#205#211#218#199#231#225#233#237#250#243' '#221#205#195#227#245#213'</p><br>'
      '<img src='#39'cid:LogoACBr'#39'>'
      '</body></html>'
      ''
      '')
    TabOrder = 10
    WordWrap = False
  end
  object cbAddXML: TCheckBox
    Left = 16
    Top = 136
    Width = 141
    Height = 19
    Caption = 'Incluir XML por Stream'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object cbAddImgAtt: TCheckBox
    Left = 16
    Top = 88
    Width = 156
    Height = 19
    Caption = 'Incluir Imagem em Anexo'
    Checked = True
    State = cbChecked
    TabOrder = 13
  end
  object ACBrMail1: TACBrMail
    Host = '127.0.0.1'
    Port = '25'
    SetSSL = False
    SetTLS = False
    Attempts = 3
    DefaultCharset = UTF_8
    IDECharset = CP1252
    OnBeforeMailProcess = ACBrMail1BeforeMailProcess
    OnMailProcess = ACBrMail1MailProcess
    OnAfterMailProcess = ACBrMail1AfterMailProcess
    OnMailException = ACBrMail1MailException
    Left = 88
    Top = 192
  end
end
