object FormImport: TFormImport
  Left = 0
  Top = 0
  Caption = 'Exemplo de importa'#231#227'o do C100 e C170'
  ClientHeight = 545
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 41
    Height = 13
    Caption = 'Arquivo:'
  end
  object Label2: TLabel
    Left = 464
    Top = 11
    Width = 146
    Height = 13
    Caption = 'Colabora'#231#227'o: Claudinei Simons'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LblEmp: TLabel
    Left = 8
    Top = 36
    Width = 60
    Height = 16
    Caption = 'Empresa:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 376
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Abrir e gerar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EdtFile: TEdit
    Left = 55
    Top = 8
    Width = 315
    Height = 21
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 631
    Height = 197
    Align = alBottom
    Caption = 'Bloco C - Registro C100'
    TabOrder = 2
    object Grid_C100: TDBGrid
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 621
      Height = 174
      Align = alClient
      DataSource = DS_C100
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 264
    Width = 631
    Height = 278
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Bloco C - Registro C170'
    TabOrder = 3
    object Grid_C170: TDBGrid
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 621
      Height = 255
      Align = alClient
      DataSource = DS_C170
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 'Arquivo txt|*.txt'
    Left = 40
    Top = 136
  end
  object CDS_C100: TClientDataSet
    Aggregates = <>
    Params = <>
    AfterScroll = CDS_C100AfterScroll
    Left = 16
    Top = 176
    object CDS_C100IND_OPER: TIntegerField
      FieldName = 'IND_OPER'
    end
    object CDS_C100IND_EMIT: TIntegerField
      FieldName = 'IND_EMIT'
    end
    object CDS_C100COD_PART: TStringField
      FieldName = 'COD_PART'
      Size = 50
    end
    object CDS_C100COD_MOD: TStringField
      FieldName = 'COD_MOD'
      Size = 4
    end
    object CDS_C100COD_SIT: TIntegerField
      FieldName = 'COD_SIT'
    end
    object CDS_C100SER: TStringField
      FieldName = 'SER'
      Size = 2
    end
    object CDS_C100NUM_DOC: TStringField
      FieldName = 'NUM_DOC'
      Size = 15
    end
    object CDS_C100CHV_NFE: TStringField
      FieldName = 'CHV_NFE'
      Size = 50
    end
    object CDS_C100DT_DOC: TDateField
      FieldName = 'DT_DOC'
    end
    object CDS_C100DT_E_S: TDateField
      FieldName = 'DT_E_S'
    end
    object CDS_C100VL_DOC: TFloatField
      FieldName = 'VL_DOC'
    end
    object CDS_C100IND_PGTO: TIntegerField
      FieldName = 'IND_PGTO'
    end
    object CDS_C100VL_DESC: TFloatField
      FieldName = 'VL_DESC'
    end
    object CDS_C100VL_ABAT_NT: TFloatField
      FieldName = 'VL_ABAT_NT'
    end
    object CDS_C100VL_MERC: TFloatField
      FieldName = 'VL_MERC'
    end
    object CDS_C100IND_FRT: TIntegerField
      FieldName = 'IND_FRT'
    end
    object CDS_C100VL_FRT: TFloatField
      FieldName = 'VL_FRT'
    end
    object CDS_C100VL_SEG: TFloatField
      FieldName = 'VL_SEG'
    end
    object CDS_C100VL_OUT_DA: TFloatField
      FieldName = 'VL_OUT_DA'
    end
    object CDS_C100VL_BC_ICMS: TFloatField
      FieldName = 'VL_BC_ICMS'
    end
    object CDS_C100VL_ICMS: TFloatField
      FieldName = 'VL_ICMS'
    end
    object CDS_C100VL_BC_ICMS_ST: TFloatField
      FieldName = 'VL_BC_ICMS_ST'
    end
    object CDS_C100VL_ICMS_ST: TFloatField
      FieldName = 'VL_ICMS_ST'
    end
    object CDS_C100VL_IPI: TFloatField
      FieldName = 'VL_IPI'
    end
    object CDS_C100VL_PIS: TFloatField
      FieldName = 'VL_PIS'
    end
    object CDS_C100VL_COFINS: TFloatField
      FieldName = 'VL_COFINS'
    end
    object CDS_C100VL_PIS_ST: TFloatField
      FieldName = 'VL_PIS_ST'
    end
    object CDS_C100VL_COFINS_ST: TFloatField
      FieldName = 'VL_COFINS_ST'
    end
    object CDS_C100ID: TIntegerField
      FieldName = 'ID'
      Visible = False
    end
  end
  object DS_C100: TDataSource
    DataSet = CDS_C100
    Left = 48
    Top = 176
  end
  object CDS_C170: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 16
    Top = 312
    object CDS_C170NUM_ITEM: TStringField
      FieldName = 'NUM_ITEM'
      Size = 10
    end
    object CDS_C170COD_ITEM: TStringField
      FieldName = 'COD_ITEM'
      Size = 15
    end
    object CDS_C170DESCR_COMPL: TStringField
      FieldName = 'DESCR_COMPL'
      Size = 100
    end
    object CDS_C170QTD: TFloatField
      FieldName = 'QTD'
    end
    object CDS_C170UNID: TStringField
      FieldName = 'UNID'
      Size = 4
    end
    object CDS_C170VL_ITEM: TFloatField
      FieldName = 'VL_ITEM'
    end
    object CDS_C170VL_DESC: TFloatField
      FieldName = 'VL_DESC'
    end
    object CDS_C170IND_MOV: TIntegerField
      FieldName = 'IND_MOV'
    end
    object CDS_C170CST_ICMS: TStringField
      FieldName = 'CST_ICMS'
      Size = 3
    end
    object CDS_C170CFOP: TStringField
      FieldName = 'CFOP'
      Size = 4
    end
    object CDS_C170COD_NAT: TStringField
      FieldName = 'COD_NAT'
      Size = 10
    end
    object CDS_C170VL_BC_ICMS: TFloatField
      FieldName = 'VL_BC_ICMS'
    end
    object CDS_C170ALIQ_ICMS: TFloatField
      FieldName = 'ALIQ_ICMS'
    end
    object CDS_C170VL_ICMS: TFloatField
      FieldName = 'VL_ICMS'
    end
    object CDS_C170VL_BC_ICMS_ST: TFloatField
      FieldName = 'VL_BC_ICMS_ST'
    end
    object CDS_C170ALIQ_ST: TFloatField
      FieldName = 'ALIQ_ST'
    end
    object CDS_C170VL_ICMS_ST: TFloatField
      FieldName = 'VL_ICMS_ST'
    end
    object CDS_C170IND_APUR: TIntegerField
      FieldName = 'IND_APUR'
    end
    object CDS_C170CST_IPI: TStringField
      FieldName = 'CST_IPI'
      Size = 3
    end
    object CDS_C170COD_ENQ: TStringField
      FieldName = 'COD_ENQ'
      Size = 10
    end
    object CDS_C170VL_BC_IPI: TFloatField
      FieldName = 'VL_BC_IPI'
    end
    object CDS_C170ALIQ_IPI: TFloatField
      FieldName = 'ALIQ_IPI'
    end
    object CDS_C170VL_IPI: TFloatField
      FieldName = 'VL_IPI'
    end
    object CDS_C170CST_PIS: TStringField
      FieldName = 'CST_PIS'
      Size = 3
    end
    object CDS_C170VL_BC_PIS: TFloatField
      FieldName = 'VL_BC_PIS'
    end
    object CDS_C170ALIQ_PIS_PERC: TFloatField
      FieldName = 'ALIQ_PIS_PERC'
    end
    object CDS_C170QUANT_BC_PIS: TFloatField
      FieldName = 'QUANT_BC_PIS'
    end
    object CDS_C170ALIQ_PIS_R: TFloatField
      FieldName = 'ALIQ_PIS_R'
    end
    object CDS_C170VL_PIS: TFloatField
      FieldName = 'VL_PIS'
    end
    object CDS_C170CST_COFINS: TStringField
      FieldName = 'CST_COFINS'
      Size = 3
    end
    object CDS_C170VL_BC_COFINS: TFloatField
      FieldName = 'VL_BC_COFINS'
    end
    object CDS_C170ALIQ_COFINS_PERC: TFloatField
      FieldName = 'ALIQ_COFINS_PERC'
    end
    object CDS_C170QUANT_BC_COFINS: TFloatField
      FieldName = 'QUANT_BC_COFINS'
    end
    object CDS_C170ALIQ_COFINS_R: TFloatField
      FieldName = 'ALIQ_COFINS_R'
    end
    object CDS_C170VL_COFINS: TFloatField
      FieldName = 'VL_COFINS'
    end
    object CDS_C170COD_CTA: TStringField
      FieldName = 'COD_CTA'
    end
    object CDS_C170PARENT: TIntegerField
      FieldName = 'PARENT'
      Visible = False
    end
  end
  object DS_C170: TDataSource
    DataSet = CDS_C170
    Left = 48
    Top = 312
  end
  object SpedPCImp: TACBrSpedFiscalImportar
    ACBrSpedFiscal = SPEDFiscal
    Left = 59
    Top = 101
  end
  object SPEDFiscal: TACBrSPEDFiscal
    Path = 'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\'
    Delimitador = '|'
    TrimString = True
    CurMascara = '#0.00'
    Left = 131
    Top = 101
  end
end
