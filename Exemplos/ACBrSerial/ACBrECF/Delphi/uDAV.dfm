object frmDAV: TfrmDAV
  Left = 323
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Emiss'#227'o de DAV'
  ClientHeight = 410
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 620
    Height = 375
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = '1. Abertura'
      object Label1: TLabel
        Left = 15
        Top = 24
        Width = 91
        Height = 13
        Caption = 'T'#237'po de documento'
      end
      object Label2: TLabel
        Left = 195
        Top = 24
        Width = 37
        Height = 13
        Caption = 'N'#250'mero'
      end
      object Label3: TLabel
        Left = 195
        Top = 67
        Width = 38
        Height = 13
        Caption = 'Emissao'
      end
      object Label4: TLabel
        Left = 15
        Top = 67
        Width = 41
        Height = 13
        Caption = 'Situa'#231#227'o'
      end
      object Label5: TLabel
        Left = 312
        Top = 67
        Width = 46
        Height = 13
        Caption = 'Vendedor'
      end
      object Label6: TLabel
        Left = 15
        Top = 187
        Width = 76
        Height = 13
        Caption = 'Nome do cliente'
      end
      object Label7: TLabel
        Left = 15
        Top = 230
        Width = 45
        Height = 13
        Caption = 'Endere'#231'o'
      end
      object Label11: TLabel
        Left = 15
        Top = 144
        Width = 54
        Height = 13
        Caption = 'CNPJ / CPF'
      end
      object btnAbrirDAV: TButton
        Left = 460
        Top = 300
        Width = 121
        Height = 25
        Caption = 'Abrir DAV'
        TabOrder = 8
        OnClick = btnAbrirDAVClick
      end
      object edtTipoDocumento: TComboBox
        Left = 15
        Top = 40
        Width = 174
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        MaxLength = 30
        TabOrder = 0
        Text = 'PEDIDO'
        Items.Strings = (
          'PEDIDO'
          'OR'#199'AMENTO')
      end
      object edtNumero: TEdit
        Left = 195
        Top = 40
        Width = 111
        Height = 21
        MaxLength = 13
        TabOrder = 1
        Text = '1'
      end
      object edtData: TDateTimePicker
        Left = 195
        Top = 83
        Width = 111
        Height = 21
        Date = 40897.923269560180000000
        Time = 40897.923269560180000000
        TabOrder = 3
      end
      object edtSituacao: TEdit
        Left = 15
        Top = 83
        Width = 174
        Height = 21
        MaxLength = 30
        TabOrder = 2
        Text = 'Aberto'
      end
      object edtVendedor: TEdit
        Left = 312
        Top = 83
        Width = 269
        Height = 21
        MaxLength = 30
        TabOrder = 4
        Text = 'Nome do Vendedor'
      end
      object edtNomeCliente: TEdit
        Left = 15
        Top = 203
        Width = 566
        Height = 21
        MaxLength = 50
        TabOrder = 6
        Text = 'Nome completo do cliente'
      end
      object edtEndereco: TEdit
        Left = 15
        Top = 246
        Width = 566
        Height = 21
        TabOrder = 7
        Text = 
          'Rua Teodoro Ter'#234'ncio Pereira, 111 - Sala 999 - Salgados - S'#227'o Pa' +
          'ulo - SP'
      end
      object edtCNPJCPF: TEdit
        Left = 15
        Top = 160
        Width = 156
        Height = 21
        MaxLength = 14
        TabOrder = 5
        Text = '11122233399'
      end
    end
    object TabSheet2: TTabSheet
      Caption = '2. Registro de item'
      ImageIndex = 1
      object Label14: TLabel
        Left = 15
        Top = 24
        Width = 33
        Height = 13
        Caption = 'C'#243'digo'
      end
      object Label15: TLabel
        Left = 15
        Top = 110
        Width = 56
        Height = 13
        Caption = 'Quantidade'
      end
      object Label18: TLabel
        Left = 148
        Top = 110
        Width = 25
        Height = 13
        Caption = 'Unid.'
      end
      object Label19: TLabel
        Left = 15
        Top = 67
        Width = 46
        Height = 13
        Caption = 'Descri'#231#227'o'
      end
      object Label22: TLabel
        Left = 188
        Top = 110
        Width = 63
        Height = 13
        Caption = 'Valor unit'#225'rio'
      end
      object Label23: TLabel
        Left = 321
        Top = 110
        Width = 71
        Height = 13
        Caption = 'Valor desconto'
      end
      object Label24: TLabel
        Left = 454
        Top = 110
        Width = 74
        Height = 13
        Caption = 'Valor acr'#233'scimo'
      end
      object btnRegistrarItem: TButton
        Left = 460
        Top = 300
        Width = 121
        Height = 25
        Caption = 'Registrar Item'
        TabOrder = 8
        OnClick = btnRegistrarItemClick
      end
      object edtProdCodigo: TEdit
        Left = 15
        Top = 40
        Width = 156
        Height = 21
        MaxLength = 13
        TabOrder = 0
        Text = '7891113339871'
      end
      object edtProdQuantidade: TEdit
        Left = 15
        Top = 126
        Width = 127
        Height = 21
        TabOrder = 2
        Text = '1'
        OnKeyPress = edtProdQuantidadeKeyPress
      end
      object edtProdUnidade: TEdit
        Left = 148
        Top = 126
        Width = 34
        Height = 21
        MaxLength = 3
        TabOrder = 3
        Text = 'UND'
      end
      object edtProdDescricao: TEdit
        Left = 15
        Top = 83
        Width = 566
        Height = 21
        TabOrder = 1
        Text = 'Descri'#231#227'o completa da mercadoria '
      end
      object edtProdVlUnitario: TEdit
        Left = 188
        Top = 126
        Width = 127
        Height = 21
        TabOrder = 4
        Text = '1,23'
        OnKeyPress = edtProdQuantidadeKeyPress
      end
      object edtProdVlDesconto: TEdit
        Left = 321
        Top = 126
        Width = 127
        Height = 21
        TabOrder = 5
        Text = '0,00'
        OnKeyPress = edtProdQuantidadeKeyPress
      end
      object edtProdVlAcrescimo: TEdit
        Left = 454
        Top = 126
        Width = 127
        Height = 21
        TabOrder = 6
        Text = '0,00'
        OnKeyPress = edtProdQuantidadeKeyPress
      end
      object ckbProdCancelado: TCheckBox
        Left = 25
        Top = 168
        Width = 81
        Height = 17
        Caption = 'Cancelado'
        TabOrder = 7
      end
    end
    object TabSheet3: TTabSheet
      Caption = '3. Fechamento'
      ImageIndex = 2
      object btnFecharRelatorio: TButton
        Left = 460
        Top = 300
        Width = 121
        Height = 25
        Caption = 'Fechar relat'#243'rio'
        TabOrder = 1
        OnClick = btnFecharRelatorioClick
      end
      object memObervacao: TMemo
        Left = 15
        Top = 30
        Width = 566
        Height = 89
        TabOrder = 0
      end
    end
  end
  object pnlRodape: TPanel
    Left = 0
    Top = 375
    Width = 620
    Height = 35
    Align = alBottom
    TabOrder = 1
    object btnCancelar: TButton
      Left = 519
      Top = 1
      Width = 100
      Height = 33
      Caption = 'Fechar'
      TabOrder = 0
      OnClick = btnCancelarClick
    end
  end
end
