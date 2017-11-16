object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Exemplo Reinf'
  ClientHeight = 583
  ClientWidth = 570
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
    Width = 570
    Height = 77
    Align = alBottom
    Caption = ' Dados Adicionais '
    TabOrder = 0
    object Label1: TLabel
      Left = 13
      Top = 35
      Width = 52
      Height = 13
      Caption = 'Protocolo :'
    end
    object btnGerar: TButton
      Left = 367
      Top = 24
      Width = 196
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
    Width = 570
    Height = 506
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = '  Eventos   '
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 0
        Top = 86
        Width = 562
        Height = 307
        Align = alTop
        Caption = ' Eventos de Tabela '
        TabOrder = 0
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
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
        end
        object chk2010: TCheckBox
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
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 1
        end
        object chk2020: TCheckBox
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
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 2
        end
        object chk2098: TCheckBox
          Left = 8
          Top = 149
          Width = 271
          Height = 17
          Caption = 'R-2098 - Reabertura dos Eventos Peri'#243'dicos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 3
        end
        object chk1070: TCheckBox
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
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 4
        end
        object chk2099: TCheckBox
          Left = 8
          Top = 170
          Width = 271
          Height = 17
          Caption = 'R-2099 - Fechamento dos Eventos Peri'#243'dicos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 5
        end
        object chk9000: TCheckBox
          Left = 8
          Top = 192
          Width = 271
          Height = 17
          Caption = 'R-9000 - Exclus'#227'o de Eventos'
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 6
        end
        object CheckBox3: TCheckBox
          Left = 8
          Top = 107
          Width = 337
          Height = 17
          Caption = 
            'R-2060 - Contribui'#231#227'o Previdenci'#225'ria sobre a Receita Bruta - CPR' +
            'B'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsStrikeOut]
          ParentFont = False
          TabOrder = 7
        end
        object CheckBox4: TCheckBox
          Left = 8
          Top = 128
          Width = 337
          Height = 17
          Caption = 'R-2070 - Reten'#231#245'es na Fonte - IR, CSLL, Cofins, PIS/PASEP'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsStrikeOut]
          ParentFont = False
          TabOrder = 8
        end
        object Button1: TButton
          Left = 291
          Top = 166
          Width = 116
          Height = 25
          Caption = 'Consulta Fechamento'
          Enabled = False
          TabOrder = 9
        end
      end
      object rdgOperacao: TRadioGroup
        Left = 0
        Top = 43
        Width = 562
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
      end
      object rdgGrupo: TRadioGroup
        Left = 0
        Top = 0
        Width = 562
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
      end
      object Panel1: TPanel
        Left = 0
        Top = 393
        Width = 562
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
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
        object edRecibo: TEdit
          Left = 5
          Top = 19
          Width = 247
          Height = 21
          TabOrder = 0
        end
        object cbEvento: TComboBox
          Left = 258
          Top = 19
          Width = 87
          Height = 21
          TabOrder = 1
          Items.Strings = (
            'R-2010'
            'R-2020')
        end
      end
      object ChkRetificadora: TCheckBox
        Left = 376
        Top = 413
        Width = 97
        Height = 17
        Caption = 'Retificadora'
        TabOrder = 4
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
        Width = 562
        Height = 478
        Align = alClient
        Lines.Strings = (
          'mmoRet')
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = '  XML de Envio   '
      ImageIndex = 2
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 562
        Height = 478
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
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
        Width = 562
        Height = 478
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object FormStorage1: TFormStorage
    StoredProps.Strings = (
      'chkClear.Checked'
      'chk1000.Checked'
      'chk2010.Checked'
      'chk2020.Checked'
      'chk2098.Checked'
      'chk1070.Checked'
      'rdgOperacao.ItemIndex'
      'rdgGrupo.ItemIndex'
      'edRecibo.Text'
      'cbEvento.ItemIndex'
      'chk2099.Checked')
    StoredValues = <>
    Left = 516
    Top = 345
  end
end
