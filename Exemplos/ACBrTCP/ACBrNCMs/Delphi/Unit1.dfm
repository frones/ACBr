object Form1: TForm1
  Left = 230
  Top = 197
  Width = 1040
  Height = 524
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    1024
    485)
  PixelsPerInch = 96
  TextHeight = 13
  object pnMenu: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 485
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object lbNumRegistros: TLabel
      Left = 16
      Top = 424
      Width = 111
      Height = 13
      Caption = 'Numero de Registros: 0'
    end
    object lbUltAtualizacao: TLabel
      Left = 16
      Top = 443
      Width = 105
      Height = 13
      Caption = 'Ultima Atualizacao:  ---'
    end
    object btListarNCMs: TButton
      Left = 16
      Top = 16
      Width = 161
      Height = 25
      Caption = 'Obter NCMs'
      TabOrder = 0
      OnClick = btListarNCMsClick
    end
    object btSalvarEmArquivo: TButton
      Left = 16
      Top = 56
      Width = 161
      Height = 25
      Caption = 'Salvar em Arquivo'
      TabOrder = 1
      OnClick = btSalvarEmArquivoClick
    end
    object gbFiltrarPorCodigo: TGroupBox
      Left = 16
      Top = 95
      Width = 161
      Height = 98
      Caption = 'Filtrar por C'#243'digo'
      TabOrder = 2
      object edFiltroCodigo: TEdit
        Left = 16
        Top = 24
        Width = 129
        Height = 21
        TabOrder = 0
      end
      object btFiltrarPorCodigo: TButton
        Left = 16
        Top = 56
        Width = 129
        Height = 25
        Caption = 'Filtrar'
        TabOrder = 1
        OnClick = btFiltrarPorCodigoClick
      end
    end
    object gbFiltrarPorDescricao: TGroupBox
      Left = 16
      Top = 200
      Width = 161
      Height = 110
      Caption = 'Filtrar por Descri'#231#227'o'
      TabOrder = 3
      object edFiltroDescricao: TEdit
        Left = 16
        Top = 48
        Width = 129
        Height = 21
        TabOrder = 0
      end
      object cbTipoFiltro: TComboBox
        Left = 16
        Top = 21
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'Inicia Por'
        Items.Strings = (
          'Inicia Por'
          'Cont'#233'm'
          'Finaliza Por')
      end
      object btFiltrarPorDescricao: TButton
        Left = 16
        Top = 72
        Width = 129
        Height = 25
        Caption = 'Filtrar'
        TabOrder = 2
        OnClick = btFiltrarPorDescricaoClick
      end
    end
    object gbValidarNCM: TGroupBox
      Left = 16
      Top = 320
      Width = 161
      Height = 98
      Caption = 'Validar NCM'
      TabOrder = 4
      object edValidarNCM: TEdit
        Left = 16
        Top = 24
        Width = 129
        Height = 21
        TabOrder = 0
      end
      object btValidarNCM: TButton
        Left = 16
        Top = 56
        Width = 129
        Height = 25
        Caption = 'Validar'
        TabOrder = 1
        OnClick = btValidarNCMClick
      end
    end
  end
  object DBGrid1: TDBGrid
    Left = 185
    Top = 0
    Width = 839
    Height = 485
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CODNCM'
        Title.Caption = 'NCM'
        Width = 102
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DESCRICAO'
        Title.Caption = 'Descri'#231#227'o'
        Width = 400
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'DATAINICIO'
        Title.Alignment = taCenter
        Title.Caption = 'Data In'#237'cio'
        Width = 80
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'DATAFIM'
        Title.Alignment = taCenter
        Title.Caption = 'Data Fim'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TIPOATO'
        Title.Alignment = taCenter
        Title.Caption = 'Tipo Ato'
        Width = 100
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'NUMEROATO'
        Title.Alignment = taCenter
        Title.Caption = 'N'#250'mero Ato'
        Width = 80
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'ANOATO'
        Title.Alignment = taCenter
        Title.Caption = 'Ano Ato'
        Width = 80
        Visible = True
      end>
  end
  object pCarregando: TPanel
    Left = 485
    Top = 216
    Width = 282
    Height = 62
    Anchors = []
    Caption = 'Aguarde Carregando na Tabela'
    TabOrder = 2
    Visible = False
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'CODNCM'
        DataType = ftString
        Size = 8
      end
      item
        Name = 'DESCRICAO'
        DataType = ftString
        Size = 200
      end
      item
        Name = 'DATAINICIO'
        DataType = ftDate
      end
      item
        Name = 'DATAFIM'
        DataType = ftDate
      end
      item
        Name = 'TIPOATO'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'NUMEROATO'
        DataType = ftString
        Size = 6
      end
      item
        Name = 'ANOATO'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 259
    Top = 124
    object ClientDataSet1CODNCM: TStringField
      FieldName = 'CODNCM'
      Size = 8
    end
    object ClientDataSet1DESCRICAO: TStringField
      FieldName = 'DESCRICAO'
      Size = 200
    end
    object ClientDataSet1DATAINICIO: TDateField
      FieldName = 'DATAINICIO'
    end
    object ClientDataSet1DATAFIM: TDateField
      FieldName = 'DATAFIM'
    end
    object ClientDataSet1TIPOATO: TStringField
      FieldName = 'TIPOATO'
    end
    object ClientDataSet1NUMEROATO: TStringField
      FieldName = 'NUMEROATO'
      Size = 6
    end
    object ClientDataSet1ANOATO: TIntegerField
      FieldName = 'ANOATO'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 258
    Top = 188
  end
  object ACBrNCMs1: TACBrNCMs
    ProxyPort = '8080'
    UrlConsulta = 
      'https://portalunico.siscomex.gov.br/classif/api/publico/nomencla' +
      'tura/download/json'
    CacheArquivo = 'ACBrNCM.json'
    Left = 256
    Top = 256
  end
end
