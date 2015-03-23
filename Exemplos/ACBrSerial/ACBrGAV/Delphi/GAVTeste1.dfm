object Form1: TForm1
  Left = 254
  Top = 135
  Width = 277
  Height = 197
  HorzScrollBar.Range = 257
  VertScrollBar.Range = 161
  ActiveControl = cbxModelo
  AutoScroll = False
  Caption = 'Teste de Gavetas'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 224
    Top = 16
    Width = 31
    Height = 30
    Hint = 'Testar impressora ligada a ACBrECF'
    Glyph.Data = {
      96010000424D9601000000000000760000002800000018000000180000000100
      04000000000020010000120B0000120B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFF00F
      FFFFFFFFFFFFFFFFFF00078000FFFFFFFFFFFFFF007778088800FFFFFFFFFFF0
      7778878008880FFFFFFFF0077887878880088000FFFFF7788777878888800888
      0FFFF88777778788888880080FFF8787777F8788888888800FFF878777FF8878
      8888888880FF87877FF9978778888888800F8787FF77777887888888808F8F88
      FAA788777008788880FF8F77877877770F70877880FFF88778887700FFF07887
      8FFFFFF887F788FCFFC8088FFFFFFFFFF88F77FFFCFF80FFFFFFFFFFFFFFFF8F
      CFFCF80FFFFF0FF0FF000FFCFFCFFC80FFFF0F00F0FFF0F8FCFFCFF80FFFF0F0
      F0FFFFFF8FFCFFF88FFFF00FF0FFFFFFF8CFF88FFFFFFF0FFF000FFFFF888FFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 12
    Top = 4
    Width = 87
    Height = 13
    Caption = '&Modelo Gaveta'
    FocusControl = cbxModelo
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 144
    Top = 4
    Width = 31
    Height = 13
    Caption = '&Porta'
    FocusControl = cbxPorta
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
  end
  object gbEstado: TGroupBox
    Left = 8
    Top = 48
    Width = 249
    Height = 81
    Caption = 'Estado da Gaveta'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lEstado: TLabel
      Left = 8
      Top = 21
      Width = 233
      Height = 36
      Alignment = taCenter
      AutoSize = False
      Caption = 'Desconhecido'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 25
      Font.Name = 'helvetica'
      Font.Pitch = fpVariable
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbxMonitorar: TCheckBox
      Left = 8
      Top = 56
      Width = 233
      Height = 17
      Caption = 'Monitorar Estado a cada 3 segundos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 11
      Font.Name = 'MS Sans Serif'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbxMonitorarClick
    end
  end
  object btAbrir: TBitBtn
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Abrir'
    TabOrder = 3
    OnClick = btAbrirClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00500000000000
      000557777777777777750BBBBBBBBBBBBBB07F5555FFFFFFF5570BBBB0000000
      BBB07F5557777777FF570BBB077BBB770BB07F557755555775570BBBBBBBBBBB
      BBB07F5555FFFFFFF5570BBBB0000000BBB07F5557777777F5570BBBB0FFFFF0
      BBB07F5557FFFFF7F5570BBBB0000000BBB07F555777777755570BBBBBBBBBBB
      BBB07FFFFFFFFFFFFFF700000000000000007777777777777777500FFFFFFFFF
      F005577FF555FFFFF7755500FFF00000005555775FF7777777F5550F777FFFFF
      F055557F777FFF5557F5550000000FFF00555577777775FF77F5550777777000
      7055557FFFFFF777F7F555000000000000555577777777777755}
    NumGlyphs = 2
  end
  object btEstado: TBitBtn
    Left = 95
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Estado'
    TabOrder = 4
    OnClick = btEstadoClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333F797F3333333333F737373FF333333BFB999BFB
      33333337737773773F3333BFBF797FBFB33333733337333373F33BFBFBFBFBFB
      FB3337F33333F33337F33FBFBFB9BFBFBF3337333337F333373FFBFBFBF97BFB
      FBF37F333337FF33337FBFBFBFB99FBFBFB37F3333377FF3337FFBFBFBFB99FB
      FBF37F33333377FF337FBFBF77BF799FBFB37F333FF3377F337FFBFB99FB799B
      FBF373F377F3377F33733FBF997F799FBF3337F377FFF77337F33BFBF99999FB
      FB33373F37777733373333BFBF999FBFB3333373FF77733F7333333BFBFBFBFB
      3333333773FFFF77333333333FBFBF3333333333377777333333}
    NumGlyphs = 2
  end
  object BitBtn1: TBitBtn
    Left = 182
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 5
    OnClick = BitBtn1Click
    Kind = bkCancel
  end
  object cbxModelo: TComboBox
    Left = 12
    Top = 20
    Width = 125
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbxModeloChange
    Items.Strings = (
      'gavNenhuma'
      'gavSerialMenno'
      'gavSerialGerbo'
      'gavImpressoraECF'
      'gavImpressoraComum')
  end
  object cbxPorta: TComboBox
    Left = 144
    Top = 20
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'COM1'
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
  object Timer1: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = btEstadoClick
    Left = 224
    Top = 96
  end
  object ACBrGAV1: TACBrGAV
    Porta = 'COM1'
    StrComando = '#027,v,#140 | Bematech'
    Left = 24
    Top = 72
  end
  object ACBrECF1: TACBrECF
    Modelo = ecfBematech
    Porta = 'COM2'
    MsgAguarde = 'Aguardando a resposta da Impressora: %d segundos'
    MsgRelatorio = 'Imprimindo %s  %dª Via '
    MsgPausaRelatorio = 'Destaque a %dª via, <ENTER> proxima, %d seg.'
    FormMsgFonte.Charset = DEFAULT_CHARSET
    FormMsgFonte.Color = clWhite
    FormMsgFonte.Height = 11
    FormMsgFonte.Name = 'MS Sans Serif'
    FormMsgFonte.Pitch = fpVariable
    FormMsgFonte.Style = []
    FormMsgColor = clHighlight
    Left = 224
    Top = 48
  end
end
