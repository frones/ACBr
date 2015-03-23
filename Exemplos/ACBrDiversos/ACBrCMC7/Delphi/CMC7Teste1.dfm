object Form1: TForm1
  Left = 449
  Top = 161
  Width = 296
  Height = 368
  VertScrollBar.Range = 67
  ActiveControl = edtcmc7
  AutoScroll = False
  Caption = 'ACBrCMC7'
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 249
    Width = 30
    Height = 13
    Caption = 'Comp.'
  end
  object Label2: TLabel
    Left = 63
    Top = 249
    Width = 31
    Height = 13
    Caption = 'Banco'
  end
  object Label3: TLabel
    Left = 128
    Top = 249
    Width = 39
    Height = 13
    Caption = 'Ag'#234'ncia'
  end
  object Label4: TLabel
    Left = 198
    Top = 249
    Width = 28
    Height = 13
    Caption = 'Conta'
  end
  object Label5: TLabel
    Left = 8
    Top = 289
    Width = 52
    Height = 13
    Caption = 'N'#186' Cheque'
  end
  object Label6: TLabel
    Left = 138
    Top = 289
    Width = 52
    Height = 13
    Caption = 'Tipifica'#231#227'o'
  end
  object edtcmc7: TEdit
    Left = 8
    Top = 16
    Width = 273
    Height = 21
    TabOrder = 0
    OnKeyPress = edtcmc7KeyPress
  end
  object btnAtivar: TButton
    Left = 163
    Top = 42
    Width = 116
    Height = 25
    Caption = 'Ativar Leitor Serial'
    TabOrder = 1
    OnClick = btnAtivarClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 73
    Width = 271
    Height = 160
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object edtcomp: TEdit
    Left = 8
    Top = 265
    Width = 49
    Height = 21
    TabOrder = 3
  end
  object edtbanco: TEdit
    Left = 64
    Top = 265
    Width = 59
    Height = 21
    TabOrder = 4
  end
  object edtagencia: TEdit
    Left = 131
    Top = 265
    Width = 59
    Height = 21
    TabOrder = 5
  end
  object edtconta: TEdit
    Left = 197
    Top = 265
    Width = 84
    Height = 21
    TabOrder = 6
  end
  object edtnumcheque: TEdit
    Left = 8
    Top = 305
    Width = 123
    Height = 21
    TabOrder = 7
  end
  object edttipificacao: TEdit
    Left = 138
    Top = 305
    Width = 66
    Height = 21
    TabOrder = 8
  end
  object Button1: TButton
    Left = 213
    Top = 301
    Width = 68
    Height = 25
    Caption = 'Gerar CMC7'
    TabOrder = 9
    OnClick = Button1Click
  end
  object ACBrLCB1: TACBrLCB
    Porta = 'COM1'
    Sufixo = '#13'
    OnLeCodigo = ACBrLCB1LeCodigo
    Left = 8
    Top = 40
  end
  object ACBrCMC71: TACBrCMC7
    Left = 40
    Top = 40
  end
end
