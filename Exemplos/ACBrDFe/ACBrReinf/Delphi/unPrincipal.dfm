object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Exemplo Reinf'
  ClientHeight = 583
  ClientWidth = 703
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 474
    Top = 456
    Width = 19
    Height = 13
    Caption = 'CPF'
  end
  object GroupBox4: TGroupBox
    Left = 0
    Top = 506
    Width = 703
    Height = 77
    Align = alBottom
    Caption = ' Dados Adicionais '
    TabOrder = 0
    ExplicitLeft = -47
    ExplicitTop = 392
    ExplicitWidth = 750
    object Label1: TLabel
      Left = 13
      Top = 35
      Width = 52
      Height = 13
      Caption = 'Protocolo :'
    end
    object btnGerar: TButton
      Left = 380
      Top = 24
      Width = 351
      Height = 40
      Caption = 'Gerar Arquivos e Enviar'
      TabOrder = 0
      OnClick = btnGerarClick
    end
    object edProtocolo: TEdit
      Left = 70
      Top = 32
      Width = 280
      Height = 21
      TabOrder = 1
      Text = '1.2.201707.0000000000000065403'
    end
    object chkClear: TCheckBox
      Left = 8
      Top = 57
      Width = 121
      Height = 17
      Caption = 'Limpar Pasta Docs'
      TabOrder = 2
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 703
    Height = 506
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    ExplicitLeft = -47
    ExplicitTop = -226
    ExplicitWidth = 750
    ExplicitHeight = 695
    object TabSheet1: TTabSheet
      Caption = '  Eventos   '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 0
        Top = 86
        Width = 695
        Height = 307
        Align = alTop
        Caption = ' Eventos de Tabela '
        TabOrder = 0
        ExplicitTop = 87
        object chk1000: TCheckBox
          Left = 8
          Top = 23
          Width = 233
          Height = 17
          Caption = 'R-1000 - Informa'#231#245'es do Contribuinte'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
        end
        object cbS1010: TCheckBox
          Left = 8
          Top = 64
          Width = 553
          Height = 17
          Caption = 'R-2010 - Reten'#231#227'o Contribui'#231#227'o Previdenci'#225'ria  Servi'#231'os Tomados '
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 1
        end
        object cbS1020: TCheckBox
          Left = 8
          Top = 86
          Width = 665
          Height = 17
          Caption = 
            'R-2020 - Reten'#231#227'o Contribui'#231#227'o Previdenci'#225'ria  Servi'#231'os Prestado' +
            's'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 2
        end
        object cbS1030: TCheckBox
          Left = 8
          Top = 108
          Width = 271
          Height = 17
          Caption = 'R-2098 - Reabertura dos Eventos Peri'#243'dicos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 3
        end
        object cbS1070: TCheckBox
          Left = 8
          Top = 43
          Width = 268
          Height = 17
          Caption = 'R-1070 - Tabela de Processos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 4
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 130
          Width = 271
          Height = 17
          Caption = 'R-2099 - Reabertura dos Eventos Peri'#243'dicos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 5
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 152
          Width = 271
          Height = 17
          Caption = 'R-9000 - Exclus'#227'o de Eventos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 6
        end
      end
      object rdgOperacao: TRadioGroup
        Left = 0
        Top = 43
        Width = 695
        Height = 43
        Align = alTop
        Caption = '  Opera'#231#227'o  '
        Columns = 3
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ItemIndex = 0
        Items.Strings = (
          'Inclus'#227'o'
          'Altera'#231#227'o'
          'Exclus'#227'o')
        ParentFont = False
        TabOrder = 1
        ExplicitWidth = 742
      end
      object rdgGrupo: TRadioGroup
        Left = 0
        Top = 0
        Width = 695
        Height = 43
        Align = alTop
        Caption = '  Grupo  '
        Columns = 3
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ItemIndex = 0
        Items.Strings = (
          'Iniciais ou Tabelas '
          'N'#227'o peri'#243'dicos'
          'Peri'#243'dicos.')
        ParentFont = False
        TabOrder = 2
        ExplicitWidth = 742
      end
      object Panel1: TPanel
        Left = 0
        Top = 393
        Width = 695
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitTop = 618
        ExplicitWidth = 742
        object Label2: TLabel
          Left = 4
          Top = 6
          Width = 50
          Height = 13
          Caption = 'Nr. Recibo'
        end
        object Label3: TLabel
          Left = 258
          Top = 6
          Width = 60
          Height = 13
          Caption = 'Cod. Evento'
        end
        object Label4: TLabel
          Left = 350
          Top = 7
          Width = 19
          Height = 13
          Caption = 'CPF'
        end
        object Label6: TLabel
          Left = 471
          Top = 6
          Width = 17
          Height = 13
          Caption = 'NIS'
        end
        object edRetificador: TEdit
          Left = 5
          Top = 19
          Width = 247
          Height = 21
          TabOrder = 0
          Text = '1.2.0000000000000072985'
        end
        object chkS1000Excluir: TCheckBox
          Left = 648
          Top = 20
          Width = 89
          Height = 17
          Caption = 'Excluir S1000'
          TabOrder = 1
        end
        object cbEvento: TComboBox
          Left = 258
          Top = 19
          Width = 87
          Height = 21
          TabOrder = 2
          Items.Strings = (
            'S1000'
            'S1005'
            'S1010'
            'S1020'
            'S1030'
            'S1035'
            'S1040'
            'S1050'
            'S1060'
            'S1070'
            'S1080'
            'S2100'
            'S1200'
            'S1202'
            'S1207'
            'S1210'
            'S1220'
            'S1250'
            'S1260'
            'S1270'
            'S1280'
            'S1295'
            'S1298'
            'S1299'
            'S1300'
            'S2190'
            'S2200'
            'S2205'
            'S2206'
            'S2210'
            'S2220'
            'S2230'
            'S2240'
            'S2241'
            'S2250'
            'S2298'
            'S2299'
            'S2300'
            'S2305'
            'S2306'
            'S2399'
            'S2400'
            'S3000'
            'S4000'
            'S4999')
        end
        object edCPF: TEdit
          Left = 350
          Top = 19
          Width = 114
          Height = 21
          TabOrder = 3
          Text = '04855800392'
        end
        object edNIS: TEdit
          Left = 470
          Top = 18
          Width = 114
          Height = 21
          TabOrder = 4
          Text = '16179749354'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  Dados de Retorno Eventos'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoRet: TMemo
        Left = 0
        Top = 0
        Width = 695
        Height = 478
        Align = alClient
        Lines.Strings = (
          'mmoRet')
        TabOrder = 0
        ExplicitWidth = 742
        ExplicitHeight = 667
      end
    end
    object TabSheet3: TTabSheet
      Caption = '  XML de Envio   '
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 695
        Height = 478
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitHeight = 643
      end
    end
    object TabSheet4: TTabSheet
      Caption = '  XML Retorno  '
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 695
        Height = 478
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitHeight = 643
      end
    end
    object TabSheet5: TTabSheet
      Caption = '  Consulta Status   '
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoStatus: TMemo
        Left = 0
        Top = 0
        Width = 695
        Height = 478
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 742
        ExplicitHeight = 667
      end
    end
  end
  object FormStorage1: TFormStorage
    StoredProps.Strings = (
      'chkClear.Checked'
      'chk1000.Checked'
      'cbS1010.Checked'
      'cbS1020.Checked'
      'cbS1030.Checked'
      'cbS1070.Checked'
      'rdgOperacao.ItemIndex'
      'rdgGrupo.ItemIndex'
      'edRetificador.Text'
      'cbEvento.ItemIndex')
    StoredValues = <>
    Left = 620
    Top = 369
  end
end
