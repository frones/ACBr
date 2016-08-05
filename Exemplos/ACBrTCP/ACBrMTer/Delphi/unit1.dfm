object Form1: TForm1
  Left = 448
  Top = 177
  Width = 774
  Height = 420
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnConectados: TPanel
    Left = 0
    Top = 128
    Width = 328
    Height = 261
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 184
      Top = 1
      Width = 5
      Height = 259
      Align = alRight
    end
    object clbConectados: TCheckListBox
      Left = 189
      Top = 1
      Width = 138
      Height = 259
      Align = alRight
      ItemHeight = 13
      TabOrder = 0
    end
    object mOutput: TMemo
      Left = 1
      Top = 1
      Width = 183
      Height = 259
      Align = alClient
      Lines.Strings = (
        'Output:'
        '')
      ReadOnly = True
      TabOrder = 1
    end
  end
  object pnComandos: TPanel
    Left = 328
    Top = 0
    Width = 438
    Height = 389
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object PageControl2: TPageControl
      Left = 1
      Top = 1
      Width = 436
      Height = 387
      ActivePage = tsComandos
      Align = alClient
      TabOrder = 0
      OnChange = PageControl2Change
      object tsComandos: TTabSheet
        Caption = 'Comandos'
        DesignSize = (
          428
          359)
        object lbPosLinha: TLabel
          Left = 166
          Top = 175
          Width = 29
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbPosColuna: TLabel
          Left = 254
          Top = 175
          Width = 37
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Coluna:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbDesLinha: TLabel
          Left = 166
          Top = 218
          Width = 29
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbQtdPosicoes: TLabel
          Left = 165
          Top = 261
          Width = 73
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Qtd. Posic'#195#181'es'
          Color = clBtnFace
          ParentColor = False
        end
        object lbLimparLinha: TLabel
          Left = 166
          Top = 304
          Width = 29
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbSerial: TLabel
          Left = 359
          Top = 130
          Width = 30
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Serial:'
          Color = clBtnFace
          ParentColor = False
        end
        object btBackSpace: TButton
          Left = 166
          Top = 16
          Width = 80
          Height = 25
          Caption = 'BackSpace'
          TabOrder = 0
          OnClick = btBackSpaceClick
        end
        object btLimparDisplay: TButton
          Left = 41
          Top = 16
          Width = 109
          Height = 25
          Caption = 'Limpa Display'
          TabOrder = 1
          OnClick = btLimparDisplayClick
        end
        object btBeep: TButton
          Left = 264
          Top = 16
          Width = 80
          Height = 25
          Caption = 'Beep'
          TabOrder = 2
          OnClick = btBeepClick
        end
        object btEnviarTexto: TButton
          Left = 41
          Top = 59
          Width = 109
          Height = 25
          Caption = 'Enviar Texto'
          TabOrder = 3
          OnClick = btEnviarTextoClick
        end
        object edEnviarTexto: TEdit
          Left = 166
          Top = 59
          Width = 240
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          Text = 'Texto para Enviar'
        end
        object btPosicionarCursor: TButton
          Left = 41
          Top = 188
          Width = 109
          Height = 25
          Caption = 'Posicionar Cursor'
          TabOrder = 5
          OnClick = btPosicionarCursorClick
        end
        object btDeslocarCursor: TButton
          Left = 40
          Top = 274
          Width = 109
          Height = 25
          Caption = 'Deslocar Cursor'
          TabOrder = 6
          OnClick = btDeslocarCursorClick
        end
        object btDeslocarLinha: TButton
          Left = 41
          Top = 231
          Width = 109
          Height = 25
          Caption = 'Deslocar Linha'
          TabOrder = 7
          OnClick = btDeslocarLinhaClick
        end
        object btLimparLinha: TButton
          Left = 41
          Top = 317
          Width = 109
          Height = 25
          Caption = 'Limpar Linha'
          TabOrder = 8
          OnClick = btLimparLinhaClick
        end
        object edPosLinha: TSpinEdit
          Left = 166
          Top = 190
          Width = 71
          Height = 22
          MaxValue = 2
          MinValue = 0
          TabOrder = 9
          Value = 1
        end
        object edPosColuna: TSpinEdit
          Left = 254
          Top = 190
          Width = 71
          Height = 22
          MaxValue = 40
          MinValue = 0
          TabOrder = 10
          Value = 1
        end
        object edDesLinha: TSpinEdit
          Left = 166
          Top = 233
          Width = 71
          Height = 22
          MaxValue = 2
          MinValue = -1
          TabOrder = 11
          Value = 1
        end
        object edQtdPosicao: TSpinEdit
          Left = 165
          Top = 276
          Width = 71
          Height = 22
          MaxValue = 0
          MinValue = -20
          TabOrder = 12
          Value = 0
        end
        object edLimparLinha: TSpinEdit
          Left = 166
          Top = 319
          Width = 71
          Height = 22
          MaxValue = 2
          MinValue = 1
          TabOrder = 13
          Value = 1
        end
        object btEnviarSerial: TButton
          Left = 41
          Top = 145
          Width = 109
          Height = 25
          Caption = 'Enviar p/ Serial'
          TabOrder = 14
          OnClick = btEnviarSerialClick
        end
        object edEnviarSerial: TEdit
          Left = 166
          Top = 145
          Width = 182
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 15
          Text = 'Texto para Serial'
        end
        object edSerial: TSpinEdit
          Left = 359
          Top = 145
          Width = 47
          Height = 22
          Anchors = [akTop, akRight]
          MaxValue = 4
          MinValue = 0
          TabOrder = 16
          Value = 0
        end
        object btEnviarParalela: TButton
          Left = 41
          Top = 102
          Width = 109
          Height = 25
          Caption = 'Enviar p/ Paralela'
          TabOrder = 17
          OnClick = btEnviarParalelaClick
        end
        object edEnviarParalela: TEdit
          Left = 166
          Top = 102
          Width = 240
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 18
          Text = 'Texto para Paralela'
        end
        object btLimparLinha1: TButton
          Left = 264
          Top = 317
          Width = 109
          Height = 25
          Caption = 'OnLine'
          TabOrder = 19
          OnClick = btLimparLinha1Click
        end
      end
      object tsFluxoVendas: TTabSheet
        Caption = 'Fluxo de Vendas'
        object pnComandas: TPanel
          Left = 220
          Top = 0
          Width = 208
          Height = 359
          Align = alClient
          TabOrder = 0
          object gbComandas: TGroupBox
            Left = 1
            Top = 1
            Width = 206
            Height = 357
            Align = alClient
            Caption = 'Comandas:'
            TabOrder = 0
            object dbgComandas: TDBGrid
              Left = 2
              Top = 15
              Width = 202
              Height = 340
              Align = alClient
              DataSource = dsComandas
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -11
              TitleFont.Name = 'Tahoma'
              TitleFont.Style = []
              Columns = <
                item
                  Expanded = False
                  FieldName = 'CODCOMANDA'
                  Title.Caption = 'Cod.'
                  Width = 108
                  Visible = True
                end
                item
                  Alignment = taCenter
                  Expanded = False
                  FieldName = 'QTD_ITENS'
                  Title.Alignment = taCenter
                  Title.Caption = 'Itens'
                  Width = 62
                  Visible = True
                end>
            end
          end
        end
        object pnTerminais: TPanel
          Left = 0
          Top = 0
          Width = 220
          Height = 359
          Align = alLeft
          TabOrder = 1
          object pnLegenda: TPanel
            Left = 1
            Top = 308
            Width = 218
            Height = 50
            Align = alBottom
            TabOrder = 0
          end
          object dbgTerminais: TDBGrid
            Left = 1
            Top = 64
            Width = 218
            Height = 244
            Align = alClient
            DataSource = dsTerminais
            TabOrder = 1
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            Columns = <
              item
                Expanded = False
                FieldName = 'IP_TERMINAL'
                Title.Caption = 'IP Terminal'
                Width = 134
                Visible = True
              end
              item
                Alignment = taCenter
                Expanded = False
                FieldName = 'STATUS'
                Title.Alignment = taCenter
                Title.Caption = 'Status'
                Width = 80
                Visible = True
              end>
          end
          object pnAtivarFluxo: TPanel
            Left = 1
            Top = 1
            Width = 218
            Height = 63
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            object btFluxoVendas: TButton
              Left = 37
              Top = 19
              Width = 144
              Height = 25
              Caption = 'Iniciar Fluxo de Vendas'
              TabOrder = 0
              OnClick = btFluxoVendasClick
            end
          end
        end
      end
    end
  end
  object pnConfig: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 128
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    DesignSize = (
      328
      128)
    object lbPorta: TLabel
      Left = 136
      Top = 11
      Width = 30
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Porta:'
      Color = clBtnFace
      ParentColor = False
    end
    object lbModelo: TLabel
      Left = 16
      Top = 11
      Width = 38
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Modelo:'
      Color = clBtnFace
      ParentColor = False
    end
    object lbTerminador: TLabel
      Left = 16
      Top = 57
      Width = 58
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Terminador:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label1: TLabel
      Left = 136
      Top = 57
      Width = 42
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Timeout:'
      Color = clBtnFace
      ParentColor = False
    end
    object btAtivar: TButton
      Left = 240
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Ativar'
      TabOrder = 0
      OnClick = btAtivarClick
    end
    object btDesativar: TButton
      Left = 240
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Desativar'
      TabOrder = 1
      OnClick = btDesativarClick
    end
    object edPorta: TEdit
      Left = 136
      Top = 26
      Width = 88
      Height = 21
      TabOrder = 2
      Text = '6550'
    end
    object cbModelo: TComboBox
      Left = 16
      Top = 26
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 3
      Text = 'mtrVT100'
      Items.Strings = (
        'mtrNenhum'
        'mtrVT100'
        'mtrSTX/ETX'
        'mtrPMTG')
    end
    object edTerminador: TEdit
      Left = 16
      Top = 72
      Width = 104
      Height = 21
      TabOrder = 4
    end
    object edTimeout: TEdit
      Left = 136
      Top = 72
      Width = 88
      Height = 21
      TabOrder = 5
      Text = '5000'
    end
    object btAtualizar: TButton
      Left = 240
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Atualizar'
      TabOrder = 6
      OnClick = btAtualizarClick
    end
    object cbEcoAutomatico: TCheckBox
      Left = 16
      Top = 96
      Width = 140
      Height = 19
      Caption = 'Enviar Eco Automatico'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object ACBrMTer1: TACBrMTer
    ArqLog = '_ACBrMTer.log'
    EcoAuto = True
    IP = '0.0.0.0'
    Port = '6550'
    TimeOut = 5000
    OnConecta = ACBrMTer1Conecta
    OnDesconecta = ACBrMTer1Desconecta
    OnRecebeDados = ACBrMTer1RecebeDados
    Left = 24
    Top = 224
  end
  object dsTerminais: TDataSource
    DataSet = memTerminais
    Left = 224
    Top = 164
  end
  object dsComandas: TDataSource
    DataSet = memComandas
    Left = 208
    Top = 244
  end
  object memComandas: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'CODCOMANDA'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'QTD_ITENS'
        DataType = ftSmallint
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 116
    Top = 260
  end
  object memTerminais: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'IP_TERMINAL'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'COMANDA'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'RESPOSTA'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'STATUS'
        DataType = ftString
        Size = 20
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 108
    Top = 204
  end
end
