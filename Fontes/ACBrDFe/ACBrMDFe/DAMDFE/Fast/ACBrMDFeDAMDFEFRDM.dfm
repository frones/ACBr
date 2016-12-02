object DMACBrMDFeDAMDFEFR: TDMACBrMDFeDAMDFEFR
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 442
  Width = 722
  object cdsIdentificacao: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 39
    Top = 12
  end
  object frxIdentificacao: TfrxDBDataset
    UserName = 'Identificacao'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsIdentificacao
    BCDToCurrency = False
    Left = 116
    Top = 12
  end
  object cdsEmitente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 44
    Top = 71
  end
  object frxEmitente: TfrxDBDataset
    UserName = 'Emitente'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEmitente
    BCDToCurrency = False
    Left = 116
    Top = 71
  end
  object cdsModalRodo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 204
    Top = 11
  end
  object frxModalRodo: TfrxDBDataset
    UserName = 'ModalRodo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsModalRodo
    BCDToCurrency = False
    Left = 281
    Top = 11
  end
  object frxModalAereo: TfrxDBDataset
    UserName = 'ModalAereo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalAereo
    BCDToCurrency = False
    Left = 121
    Top = 181
  end
  object CDSModalAereo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 44
    Top = 181
  end
  object CDSModalAqua: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 44
    Top = 236
  end
  object frxModalAqua: TfrxDBDataset
    UserName = 'ModalAqua'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalAqua
    BCDToCurrency = False
    Left = 126
    Top = 236
  end
  object frxModalFerrov: TfrxDBDataset
    UserName = 'ModalFerrov'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalFerrov
    BCDToCurrency = False
    Left = 300
    Top = 182
  end
  object CDSModalFerrov: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 214
    Top = 178
  end
  object cdsParametros: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 201
    Top = 68
  end
  object frxParametros: TfrxDBDataset
    UserName = 'Parametros'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsParametros
    BCDToCurrency = False
    Left = 273
    Top = 68
  end
  object CDSModalFerrovVagoes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 215
    Top = 234
  end
  object frxModalFerrovVagoes: TfrxDBDataset
    UserName = 'ModalFerrovVagoes'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSModalFerrovVagoes
    BCDToCurrency = False
    Left = 309
    Top = 230
  end
  object CDSDocumentos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 44
    Top = 291
  end
  object frxDocumentos: TfrxDBDataset
    UserName = 'Documentos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = CDSDocumentos
    BCDToCurrency = False
    Left = 126
    Top = 291
  end
  object cdsEventos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 210
    Top = 289
  end
  object frxEventos: TfrxDBDataset
    UserName = 'Eventos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEventos
    BCDToCurrency = False
    Left = 305
    Top = 293
  end
  object cdsMunCarrega: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 43
    Top = 128
  end
  object frxMunCarrega: TfrxDBDataset
    UserName = 'MunCarrega'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsMunCarrega
    BCDToCurrency = False
    Left = 120
    Top = 128
  end
  object cdsPercurso: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 203
    Top = 128
  end
  object frxPercurso: TfrxDBDataset
    UserName = 'Percurso'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsPercurso
    BCDToCurrency = False
    Left = 280
    Top = 128
  end
  object frxTermCarrega: TfrxDBDataset
    UserName = 'TermCarrega'
    CloseDataSource = False
    DataSet = cdsTermCarrega
    BCDToCurrency = False
    Left = 415
    Top = 16
  end
  object frxTermDescarrega: TfrxDBDataset
    UserName = 'TermDescarrega'
    CloseDataSource = False
    DataSet = cdsTermDescarrega
    BCDToCurrency = False
    Left = 415
    Top = 67
  end
  object frxEmbarcaComboio: TfrxDBDataset
    UserName = 'EmbarcaComboio'
    CloseDataSource = False
    DataSet = cdsEmbarcaComboio
    BCDToCurrency = False
    Left = 415
    Top = 119
  end
  object frxInfUnidCargaVazia: TfrxDBDataset
    UserName = 'InfUnidCargaVazia'
    CloseDataSource = False
    DataSet = cdsInfUnidCargaVazia
    BCDToCurrency = False
    Left = 415
    Top = 171
  end
  object cdsTermCarrega: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 527
    Top = 14
  end
  object cdsTermDescarrega: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 527
    Top = 66
  end
  object cdsEmbarcaComboio: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 527
    Top = 118
  end
  object cdsInfUnidCargaVazia: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 527
    Top = 171
  end
end
