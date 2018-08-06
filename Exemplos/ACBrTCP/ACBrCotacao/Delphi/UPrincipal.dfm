object frmPrincipal: TfrmPrincipal
  Left = 445
  Top = 195
  Width = 712
  Height = 541
  Caption = 'ACBrCotacao'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 696
    Height = 50
    Align = alTop
    TabOrder = 0
    object Label3: TLabel
      Left = 420
      Top = 18
      Width = 94
      Height = 13
      Alignment = taRightJustify
      Caption = 'Data de refer'#234'ncia:'
    end
    object btnAtualizarMostrar: TButton
      Left = 13
      Top = 13
      Width = 128
      Height = 25
      Caption = 'Atualizar e mostrar'
      TabOrder = 0
      OnClick = btnAtualizarMostrarClick
    end
    object btnProcurarSimbolo: TButton
      Left = 147
      Top = 13
      Width = 128
      Height = 25
      Caption = 'Procurar por Simbolo'
      TabOrder = 1
      OnClick = btnProcurarSimboloClick
    end
    object DateTimePicker1: TDateTimePicker
      Left = 520
      Top = 17
      Width = 101
      Height = 21
      Date = 41781.732459942130000000
      Time = 41781.732459942130000000
      TabOrder = 2
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 50
    Width = 696
    Height = 325
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 375
    Width = 696
    Height = 127
    Align = alBottom
    Caption = 'Como utilizar'
    TabOrder = 2
    object Label1: TLabel
      Left = 2
      Top = 36
      Width = 692
      Height = 89
      Align = alClient
      Caption = 
        '   Moedas do Tipo "A":'#13#10'        - Para calcular o valor equivale' +
        'nte em US$ (d'#243'lar americano), divida o montante na moeda consult' +
        'ada pela respectiva paridade.'#13#10'        - Para obter o valor em R' +
        '$ (reais), multiplique o montante na moeda consultada pela respe' +
        'ctiva taxa.'#13#10'   Moedas do tipo "B":'#13#10'        - Para calcular o v' +
        'alor equivalente em US$ (d'#243'lar americano), multiplique o montant' +
        'e na moeda consultada pela respectiva paridade.'#13#10'        - Para ' +
        'obter o valor em R$ (reais), multiplique o montante na moeda con' +
        'sultada pela respectiva taxa.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 2
      Top = 15
      Width = 692
      Height = 21
      Cursor = crHandPoint
      Align = alTop
      AutoSize = False
      Caption = 'https://www4.bcb.gov.br/pec/taxas/batch/cotacaomoedas.asp'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label2Click
    end
  end
  object ACBrCotacao1: TACBrCotacao
    ProxyPort = '8080'
    Left = 215
    Top = 105
  end
end
