object FrVendaFrenetica: TFrVendaFrenetica
  Left = 355
  Top = 312
  Caption = 'Venda Fren'#233'tica'
  ClientHeight = 216
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    559
    216)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 120
    Width = 110
    Height = 13
    Caption = 'C'#243'digo do Produto:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 36
    Top = 104
    Width = 489
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 
      '<Digite o c'#243'digo do produto abaixo e aperte Enter para vender. N' +
      #227'o '#233' necess'#225'rio abrir o cupom antes.>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 506
    Height = 76
    Caption = 
      'ATEN'#199#195'O:'#13#10'Esse '#233' apenas um exemplo de venda de itens em fila. '#13#10 +
      'Esta implementa'#231#227'o faz pouco ou nada na quest'#227'o de tratar erros ' +
      'que podem surgir.'#13#10'Ao implementar no seu software, voc'#234' '#233' respon' +
      's'#225'vel por fazer isso.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object edCodProduto: TEdit
    Left = 8
    Top = 136
    Width = 409
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = edCodProdutoKeyPress
  end
  object btnCancelarCupom: TButton
    Left = 242
    Top = 175
    Width = 95
    Height = 25
    Caption = 'Cancelar Cupom'
    TabOrder = 1
    TabStop = False
    OnClick = btnCancelarCupomClick
  end
  object chkProcessMessages: TCheckBox
    Left = 8
    Top = 168
    Width = 113
    Height = 17
    TabStop = False
    Caption = 'ProcessMessages'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkProcessMessagesClick
  end
  object btnSair: TButton
    Left = 450
    Top = 175
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Sair'
    ModalResult = 2
    TabOrder = 3
    TabStop = False
    OnClick = btnSairClick
  end
  object chkNaoExibirMsgs: TCheckBox
    Left = 8
    Top = 184
    Width = 209
    Height = 17
    TabStop = False
    Caption = 'N'#227'o Exibir Mensagem ao Abrir o Cupom'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object btnFechaCupom: TButton
    Left = 346
    Top = 175
    Width = 95
    Height = 25
    Cancel = True
    Caption = 'Fecha Cupom'
    ModalResult = 2
    TabOrder = 5
    TabStop = False
    OnClick = btnFechaCupomClick
  end
  object TimerVendeItem: TTimer
    Enabled = False
    Interval = 25
    OnTimer = TimerVendeItemTimer
    Left = 440
    Top = 128
  end
  object ACBrLCB1: TACBrLCB
    Porta = 'TECLADO'
    Sufixo = '#13'
    UsarFila = True
    FilaMaxItens = 100
    Intervalo = 300
    OnLeCodigo = ACBrLCB1LeCodigo
    Left = 512
    Top = 128
  end
end
