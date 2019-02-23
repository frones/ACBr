object Form1: TForm1
  Left = 637
  Top = 218
  Width = 985
  Height = 445
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
  object pnComandos: TPanel
    Left = 480
    Top = 0
    Width = 489
    Height = 406
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 1
      Top = 1
      Height = 404
    end
    object PageControl2: TPageControl
      Left = 4
      Top = 1
      Width = 484
      Height = 404
      ActivePage = tsComandos
      Align = alClient
      TabOrder = 0
      OnChange = PageControl2Change
      object tsComandos: TTabSheet
        Caption = 'Comandos'
        DesignSize = (
          476
          376)
        object lbPosLinha: TLabel
          Left = 166
          Top = 176
          Width = 29
          Height = 13
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbPosColuna: TLabel
          Left = 254
          Top = 176
          Width = 37
          Height = 13
          Caption = 'Coluna:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbDesLinha: TLabel
          Left = 166
          Top = 219
          Width = 29
          Height = 13
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbQtdPosicoes: TLabel
          Left = 165
          Top = 262
          Width = 73
          Height = 13
          Caption = 'Qtd. Posic'#195#181'es'
          Color = clBtnFace
          ParentColor = False
        end
        object lbLimparLinha: TLabel
          Left = 166
          Top = 305
          Width = 29
          Height = 13
          Caption = 'Linha:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbSerial: TLabel
          Left = 410
          Top = 131
          Width = 30
          Height = 13
          Anchors = [akTop, akRight]
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
          Width = 288
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
          MaxValue = 10
          MinValue = 0
          TabOrder = 9
          Value = 1
        end
        object edPosColuna: TSpinEdit
          Left = 254
          Top = 190
          Width = 71
          Height = 22
          MaxValue = 300
          MinValue = 0
          TabOrder = 10
          Value = 1
        end
        object edDesLinha: TSpinEdit
          Left = 166
          Top = 233
          Width = 71
          Height = 22
          MaxValue = 10
          MinValue = -1
          TabOrder = 11
          Value = 1
        end
        object edQtdPosicao: TSpinEdit
          Left = 165
          Top = 276
          Width = 71
          Height = 22
          MaxValue = 300
          MinValue = -20
          TabOrder = 12
          Value = 0
        end
        object edLimparLinha: TSpinEdit
          Left = 166
          Top = 319
          Width = 71
          Height = 22
          MaxValue = 10
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
          Width = 230
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 15
          Text = 'Texto para Serial'
        end
        object edSerial: TSpinEdit
          Left = 407
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
          Width = 288
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
          Width = 256
          Height = 376
          Align = alClient
          TabOrder = 0
          object gbComandas: TGroupBox
            Left = 1
            Top = 1
            Width = 254
            Height = 374
            Align = alClient
            Caption = 'Comandas:'
            TabOrder = 0
            object dbgComandas: TDBGrid
              Left = 2
              Top = 15
              Width = 250
              Height = 357
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
          Height = 376
          Align = alLeft
          TabOrder = 1
          object pnLegenda: TPanel
            Left = 1
            Top = 325
            Width = 218
            Height = 50
            Align = alBottom
            TabOrder = 0
          end
          object dbgTerminais: TDBGrid
            Left = 1
            Top = 64
            Width = 218
            Height = 261
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
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 406
    Align = alClient
    BevelOuter = bvSpace
    TabOrder = 1
    object pnConectados: TPanel
      Left = 1
      Top = 169
      Width = 478
      Height = 236
      Align = alClient
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 285
        Top = 1
        Width = 5
        Height = 234
        Align = alRight
      end
      object clbConectados: TCheckListBox
        Left = 290
        Top = 1
        Width = 187
        Height = 234
        Align = alRight
        ItemHeight = 13
        TabOrder = 0
      end
      object mOutput: TMemo
        Left = 1
        Top = 1
        Width = 284
        Height = 234
        Align = alClient
        Lines.Strings = (
          'Output:'
          '')
        ReadOnly = True
        TabOrder = 1
      end
    end
    object pgConfigs: TPageControl
      Left = 1
      Top = 1
      Width = 478
      Height = 168
      ActivePage = tsConfig
      Align = alTop
      TabOrder = 1
      object tsConfig: TTabSheet
        Caption = 'Configura'#231#227'o'
        object lbPorta: TLabel
          Left = 185
          Top = 4
          Width = 30
          Height = 13
          Caption = 'Porta:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbModelo: TLabel
          Left = 16
          Top = 4
          Width = 38
          Height = 13
          Caption = 'Modelo:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label1: TLabel
          Left = 185
          Top = 48
          Width = 42
          Height = 13
          Caption = 'Timeout:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbEchoMode: TLabel
          Left = 16
          Top = 48
          Width = 53
          Height = 13
          Caption = 'EchoMode:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbTerminador: TLabel
          Left = 16
          Top = 91
          Width = 58
          Height = 13
          Caption = 'Terminador:'
          Color = clBtnFace
          ParentColor = False
        end
        object lbTerminador1: TLabel
          Left = 184
          Top = 91
          Width = 98
          Height = 13
          Caption = 'Terminador Balan'#231'a:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label3: TLabel
          Left = 264
          Top = 49
          Width = 22
          Height = 13
          Caption = 'Wait'
          Color = clBtnFace
          ParentColor = False
        end
        object btAtivarDesativar: TButton
          Left = 384
          Top = 24
          Width = 75
          Height = 59
          Caption = 'Ativar'
          TabOrder = 0
          OnClick = btAtivarDesativarClick
        end
        object edPorta: TEdit
          Left = 185
          Top = 20
          Width = 147
          Height = 21
          TabOrder = 1
          Text = '6550'
        end
        object cbModelo: TComboBox
          Left = 16
          Top = 20
          Width = 144
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 1
          TabOrder = 2
          Text = 'mtrVT100'
          Items.Strings = (
            'mtrNenhum'
            'mtrVT100'
            'mtrSTX/ETX'
            'mtrPMTG')
        end
        object btAtualizar: TButton
          Left = 384
          Top = 106
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 3
          OnClick = btAtualizarClick
        end
        object cbEchoMode: TComboBox
          Left = 16
          Top = 64
          Width = 144
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 4
          Text = 'mdeNormal'
          OnChange = cbEchoModeChange
          Items.Strings = (
            'mdeNormal'
            'mdeNone'
            'mdePassword')
        end
        object edTerminador: TComboBox
          Left = 16
          Top = 106
          Width = 144
          Height = 21
          ItemHeight = 13
          TabOrder = 5
          OnChange = edTerminadorChange
        end
        object edTerminadorBalanca: TComboBox
          Left = 185
          Top = 106
          Width = 147
          Height = 21
          ItemHeight = 13
          TabOrder = 6
          OnChange = edTerminadorBalancaChange
        end
        object seTimeout: TSpinEdit
          Left = 184
          Top = 64
          Width = 64
          Height = 23
          Increment = 10
          MaxValue = 10000
          MinValue = 1
          TabOrder = 7
          Value = 1000
        end
        object seWait: TSpinEdit
          Left = 264
          Top = 64
          Width = 57
          Height = 23
          Increment = 10
          MaxValue = 5000
          MinValue = 0
          TabOrder = 8
          Value = 200
          OnChange = seWaitChange
        end
      end
      object tsBalanca: TTabSheet
        Caption = 'Balan'#231'a'
        DesignSize = (
          470
          140)
        object Label2: TLabel
          Left = 16
          Top = 16
          Width = 34
          Height = 13
          Caption = 'Modelo'
          Color = clBtnFace
          ParentColor = False
        end
        object Label4: TLabel
          Left = 360
          Top = 18
          Width = 55
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Porta Serial'
          Color = clBtnFace
          ParentColor = False
        end
        object cbBalanca: TComboBox
          Left = 16
          Top = 32
          Width = 313
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
        end
        object edSerialPeso: TSpinEdit
          Left = 361
          Top = 43
          Width = 59
          Height = 22
          Anchors = []
          MaxValue = 4
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object btSolicitarPeso: TButton
          Left = 189
          Top = 84
          Width = 96
          Height = 32
          Anchors = []
          Caption = 'Solicitar Peso'
          TabOrder = 2
          OnClick = btSolicitarPesoClick
        end
      end
    end
  end
  object ACBrMTer1: TACBrMTer
    ArqLog = '_ACBrMTer.log'
    Balanca = ACBrBAL1
    EchoMode = mdePassword
    IP = '0.0.0.0'
    PasswordChar = '*'
    Port = '6550'
    TerminadorBalanca = '#3'
    OnConecta = ACBrMTer1Conecta
    OnDesconecta = ACBrMTer1Desconecta
    OnRecebeDados = ACBrMTer1RecebeDados
    OnRecebePeso = ACBrMTer1RecebePeso
    OnRecebeOnLine = ACBrMTer1RecebeOnLine
    Left = 40
    Top = 224
  end
  object dsTerminais: TDataSource
    DataSet = memTerminais
    Left = 208
    Top = 212
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
  object ACBrBAL1: TACBrBAL
    Porta = 'USB'
    ArqLOG = '_LogBal.txt'
    PosIni = 1
    PosFim = 5
    Left = 276
    Top = 224
  end
end
