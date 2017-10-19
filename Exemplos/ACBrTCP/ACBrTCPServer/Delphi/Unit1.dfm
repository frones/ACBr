object Form1: TForm1
  Left = 247
  Top = 165
  Width = 600
  Height = 390
  VertScrollBar.Range = 113
  AutoScroll = False
  Caption = 'ACBrTCPServer'
  Color = clBackground
  Constraints.MinHeight = 240
  Constraints.MinWidth = 430
  ParentFont = True
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 113
    Width = 592
    Height = 246
    Align = alClient
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 391
      Top = 1
      Width = 5
      Height = 244
      Align = alRight
      Beveled = True
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 390
      Height = 244
      Align = alClient
      TabOrder = 0
      object mOutput: TMemo
        Left = 1
        Top = 1
        Width = 388
        Height = 242
        Align = alClient
        Lines.Strings = (
          'Conecte nesse servidor usando:'
          ''
          'telnet IP_dessa_maquina nPorta'
          ''
          'Exemplo:'
          ''
          'telnet localhost 3434')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object Panel3: TPanel
      Left = 396
      Top = 1
      Width = 195
      Height = 244
      Align = alRight
      TabOrder = 1
      DesignSize = (
        195
        244)
      object CheckListBox1: TCheckListBox
        Left = 1
        Top = 1
        Width = 193
        Height = 188
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
      object Button1: TButton
        Left = 12
        Top = 202
        Width = 75
        Height = 27
        Anchors = [akRight, akBottom]
        Caption = 'Matar'
        TabOrder = 1
        OnClick = Button1Click
      end
      object bEnviar: TButton
        Left = 104
        Top = 202
        Width = 75
        Height = 27
        Anchors = [akRight, akBottom]
        Caption = 'Enviar Resp'
        TabOrder = 2
        OnClick = bEnviarClick
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 113
    Align = alTop
    TabOrder = 0
    DesignSize = (
      592
      113)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Porta:'
    end
    object Label4: TLabel
      Left = 104
      Top = 8
      Width = 53
      Height = 13
      Caption = 'Terminador'
    end
    object Label5: TLabel
      Left = 200
      Top = 8
      Width = 40
      Height = 13
      Caption = 'TimeOut'
    end
    object Label3: TLabel
      Left = 482
      Top = 89
      Width = 50
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Conex'#245'es:'
      OnClick = Label3Click
    end
    object lNConexoes: TLabel
      Left = 541
      Top = 89
      Width = 22
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label3Click
    end
    object Label2: TLabel
      Left = 9
      Top = 56
      Width = 48
      Height = 13
      Caption = 'Resposta:'
    end
    object edPorta: TEdit
      Left = 8
      Top = 24
      Width = 73
      Height = 21
      TabOrder = 0
      Text = '3434'
    end
    object edTerm: TEdit
      Left = 104
      Top = 24
      Width = 73
      Height = 21
      TabOrder = 2
      Text = '#13,#10'
    end
    object edTimeOut: TEdit
      Left = 200
      Top = 24
      Width = 73
      Height = 21
      TabOrder = 5
      Text = '10000'
    end
    object bDesativar: TButton
      Left = 392
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Desativar'
      TabOrder = 1
      OnClick = bDesativarClick
    end
    object bAtivar: TButton
      Left = 296
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Ativar'
      TabOrder = 3
      OnClick = bAtivarClick
    end
    object edEnviar: TEdit
      Left = 8
      Top = 72
      Width = 460
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      Text = 'OK'
    end
  end
  object ACBrTCPServer1: TACBrTCPServer
    IP = '0.0.0.0'
    Port = '0'
    TimeOut = 10000
    Terminador = '#13,#10'
    OnConecta = ACBrTCPServer1Conecta
    OnDesConecta = ACBrTCPServer1DesConecta
    OnRecebeDados = ACBrTCPServer1RecebeDados
    Left = 288
    Top = 32
  end
end
