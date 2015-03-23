object Form1: TForm1
  Left = 320
  Top = 122
  Width = 379
  Height = 329
  HorzScrollBar.Range = 363
  VertScrollBar.Range = 281
  ActiveControl = edCod
  AutoScroll = False
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lInstr: TLabel
    Left = 16
    Top = 224
    Width = 337
    Height = 57
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Esse programa demonstra o uso dos componentes ACBrLCB e ACBrECF.' +
      ' '#201' necess'#225'rio configurar corretamente os componentes ACBr para o' +
      ' modelo de equipamento que vc possui,  antes de compilar o progr' +
      'ama'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 280
    Top = 14
    Width = 73
    Height = 13
    Caption = 'Atraso Abertura'
  end
  object lAtraso: TLabel
    Left = 92
    Top = 240
    Width = 189
    Height = 24
    Caption = 'Simulando ATRASO'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Microsoft Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Label2: TLabel
    Left = 280
    Top = 51
    Width = 64
    Height = 13
    Caption = 'Atraso Venda'
  end
  object bLiberaECF: TButton
    Left = 280
    Top = 160
    Width = 83
    Height = 41
    Caption = 'Cancela Venda'
    TabOrder = 0
    OnClick = bLiberaECFClick
  end
  object pVenda: TPanel
    Left = 8
    Top = 8
    Width = 257
    Height = 193
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 88
      Height = 13
      Caption = 'C'#243'digo do Produto'
    end
    object Label6: TLabel
      Left = 152
      Top = 24
      Width = 17
      Height = 13
      Caption = 'Aliq'
    end
    object Label7: TLabel
      Left = 200
      Top = 24
      Width = 16
      Height = 13
      Caption = 'UN'
    end
    object Label3: TLabel
      Left = 16
      Top = 68
      Width = 48
      Height = 13
      Caption = 'Descri'#231#227'o'
    end
    object Label4: TLabel
      Left = 16
      Top = 112
      Width = 55
      Height = 13
      Caption = 'Quantidade'
    end
    object Label5: TLabel
      Left = 104
      Top = 112
      Width = 67
      Height = 13
      Caption = 'Pre'#231'o Unit'#225'rio'
    end
    object edCod: TEdit
      Left = 16
      Top = 40
      Width = 101
      Height = 21
      TabOrder = 0
      Text = '7123456789012'
    end
    object edAliq: TEdit
      Left = 152
      Top = 40
      Width = 33
      Height = 21
      TabOrder = 2
      Text = 'FF'
    end
    object edUN: TEdit
      Left = 200
      Top = 40
      Width = 33
      Height = 21
      TabOrder = 5
      Text = 'UN'
    end
    object edDescricao: TEdit
      Left = 16
      Top = 84
      Width = 217
      Height = 21
      TabOrder = 1
      Text = 'DESCRICAO DO PRODUTO'
    end
    object edQtd: TEdit
      Left = 16
      Top = 128
      Width = 57
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object edUnit: TEdit
      Left = 104
      Top = 128
      Width = 129
      Height = 21
      TabOrder = 4
      Text = '0,01'
    end
    object bVendeItem: TButton
      Left = 88
      Top = 160
      Width = 75
      Height = 25
      Caption = 'Vende Item'
      Default = True
      TabOrder = 6
      OnClick = bVendeItemClick
    end
  end
  object Button1: TButton
    Left = 282
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Abre Cupom'
    TabOrder = 3
    OnClick = Button1Click
  end
  object edAtrasoAbre: TEdit
    Left = 280
    Top = 30
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '6000'
  end
  object edAtrasoVende: TEdit
    Left = 280
    Top = 67
    Width = 57
    Height = 21
    TabOrder = 4
    Text = '1000'
  end
  object ACBrECF1: TACBrECF
    Modelo = ecfNaoFiscal
    Porta = 'C:\TEMP\SAIDA.TXT'
    DescricaoGrande = True
    MsgAguarde = 'Aguardando a resposta da Impressora: %d segundos'
    MsgRelatorio = 'Imprimindo %s  %d'#170' Via '
    MsgPausaRelatorio = 'Destaque a %d'#170' via, <ENTER> proxima, %d seg.'
    FormMsgFonte.Charset = DEFAULT_CHARSET
    FormMsgFonte.Color = clWhite
    FormMsgFonte.Height = 11
    FormMsgFonte.Name = 'MS Shell Dlg'
    FormMsgFonte.Pitch = fpVariable
    FormMsgFonte.Style = []
    FormMsgColor = clHighlight
    OnAguardandoRespostaChange = ACBrECF1AguardandoRespostaChange
    Device.HandShake = hsRTS_CTS
    Device.HardFlow = True
    Left = 197
    Top = 168
  end
  object ACBrLCB1: TACBrLCB
    Porta = 'COM2'
    Sufixo = '#13'
    UsarFila = True
    FilaMaxItens = 100
    OnLeCodigo = ACBrLCB1LeCodigo
    Left = 227
    Top = 168
  end
end
