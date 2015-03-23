object FrmPrincipal: TFrmPrincipal
  Left = 307
  Top = 140
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Microterminal Wilbor'
  ClientHeight = 516
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 349
    Height = 501
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Fluxo'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 341
        Height = 20
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'Fluxo de teclas'
        TabOrder = 0
      end
      object ListBoxFluxoTeclas: TListBox
        Left = 0
        Top = 20
        Width = 341
        Height = 453
        Align = alClient
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Configura'#231#245'es'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 4
        Top = 12
        Width = 333
        Height = 453
        Caption = 'Configura'#231#245'es do Microterminal'
        TabOrder = 0
        object Label4: TLabel
          Left = 12
          Top = 36
          Width = 35
          Height = 13
          Caption = 'Modelo'
        end
        object Label1: TLabel
          Left = 88
          Top = 36
          Width = 25
          Height = 13
          Caption = 'Porta'
        end
        object lblPassoRotacao: TLabel
          Left = 162
          Top = 76
          Width = 79
          Height = 13
          Caption = 'Passo (Rota'#231#227'o)'
        end
        object Label2: TLabel
          Left = 152
          Top = 36
          Width = 48
          Height = 13
          Caption = 'BaudRate'
        end
        object Label3: TLabel
          Left = 216
          Top = 36
          Width = 41
          Height = 13
          Caption = 'Intervalo'
        end
        object Label5: TLabel
          Left = 264
          Top = 36
          Width = 60
          Height = 13
          Caption = 'N'#186' Terminais'
        end
        object cbxModelo: TComboBox
          Left = 12
          Top = 52
          Width = 73
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          Text = 'Wilbor'
          Items.Strings = (
            'Wilbor')
        end
        object cbxPorta: TComboBox
          Left = 88
          Top = 52
          Width = 61
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Text = 'COM1'
          Items.Strings = (
            'COM1'
            'COM2'
            'COM3')
        end
        object ckbComutadora: TCheckBox
          Left = 12
          Top = 96
          Width = 81
          Height = 17
          Caption = 'Comutadora'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object ckbRotacao: TCheckBox
          Left = 100
          Top = 96
          Width = 65
          Height = 17
          Caption = 'Rota'#231#227'o'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object edtPassoRotacao: TEdit
          Left = 164
          Top = 92
          Width = 77
          Height = 21
          TabOrder = 4
          Text = '200'
        end
        object cbxBaudRate: TComboBox
          Left = 152
          Top = 52
          Width = 61
          Height = 21
          ItemHeight = 13
          TabOrder = 5
          Text = '38400'
          Items.Strings = (
            '9600'
            '38400'
            '115200')
        end
        object edtIntervalo: TEdit
          Left = 216
          Top = 52
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '200'
        end
        object cbxTerminais: TSpinEdit
          Left = 264
          Top = 52
          Width = 61
          Height = 22
          MaxLength = 2
          MaxValue = 50
          MinValue = 0
          TabOrder = 7
          Value = 1
        end
        object btnSalvar: TButton
          Left = 248
          Top = 88
          Width = 77
          Height = 25
          Caption = 'Salvar'
          TabOrder = 8
          OnClick = btnSalvarClick
        end
      end
    end
  end
  object ACBrTER1: TACBrTER
    Modelo = terWilbor
    Porta = 'COM1'
    Intervalo = 5
    Device.Baud = 38400
    OnRecebeChar = ACBrTER1RecebeChar
    Left = 244
    Top = 220
  end
  object BancoDados: TADOConnection
    CommandTimeout = 99999999
    ConnectionTimeout = 99999999
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 244
    Top = 276
  end
  object QryCadProdutos: TADOQuery
    Connection = BancoDados
    CommandTimeout = 9999999
    Parameters = <>
    Left = 244
    Top = 388
  end
  object QryComandas: TADOQuery
    Connection = BancoDados
    CommandTimeout = 9999999
    Parameters = <>
    Left = 244
    Top = 332
  end
  object SPItensComanda: TADOStoredProc
    Connection = BancoDados
    CommandTimeout = 9999999
    ProcedureName = 'CADKINGCHEFFBALCAO;1'
    Parameters = <
      item
        Name = 'RETURN_VALUE'
        DataType = ftInteger
        Direction = pdReturnValue
        Precision = 10
        Value = Null
      end
      item
        Name = '@CODIGOFICHA'
        Attributes = [paNullable]
        DataType = ftString
        Size = 30
        Value = Null
      end
      item
        Name = '@NUMEROCONTROLEITEM'
        Attributes = [paNullable]
        DataType = ftInteger
        Precision = 10
        Value = Null
      end
      item
        Name = '@MESA'
        Attributes = [paNullable]
        DataType = ftString
        Size = 20
        Value = Null
      end
      item
        Name = '@DATAHORACONTROLEPEDIDO'
        Attributes = [paNullable]
        DataType = ftDateTime
        Value = Null
      end
      item
        Name = '@CODIGOBARRAS'
        Attributes = [paNullable]
        DataType = ftString
        Size = 15
        Value = Null
      end
      item
        Name = '@DESCRICAOPRODUTO'
        Attributes = [paNullable]
        DataType = ftString
        Size = 50
        Value = Null
      end
      item
        Name = '@QUANTIDADE'
        Attributes = [paNullable]
        DataType = ftFloat
        Value = Null
      end
      item
        Name = '@VALORUNITARIO'
        Attributes = [paNullable]
        DataType = ftFloat
        Value = Null
      end
      item
        Name = '@VALORTOTAL'
        Attributes = [paNullable]
        DataType = ftFloat
        Value = Null
      end
      item
        Name = '@ALIQUOTAICMS'
        Attributes = [paNullable]
        DataType = ftString
        Size = 30
        Value = Null
      end
      item
        Name = '@NOMEATENDENTE'
        Attributes = [paNullable]
        DataType = ftString
        Size = 50
        Value = Null
      end
      item
        Name = '@IMPRESSORA'
        Attributes = [paNullable]
        DataType = ftString
        Size = 50
        Value = Null
      end>
    Left = 244
    Top = 440
  end
end
