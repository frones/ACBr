object dmACBrNFeFR: TdmACBrNFeFR
  OldCreateOrder = False
  Height = 526
  Width = 720
  object frxPDFExport: TfrxPDFExport
    UseFileCache = True
    ShowProgress = True
    OverwritePrompt = False
    DataOnly = False
    PrintOptimized = True
    Outline = False
    Background = True
    HTMLTags = True
    Quality = 95
    Author = 'FastReport'
    Subject = 'Exportando DANFE para PDF'
    ProtectionFlags = [ePrint, eModify, eCopy, eAnnot]
    HideToolbar = False
    HideMenubar = False
    HideWindowUI = False
    FitWindow = False
    CenterWindow = False
    PrintScaling = False
    Left = 48
    Top = 92
  end
  object cdsIdentificacao: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 36
    object cdsIdentificacaoId: TStringField
      FieldName = 'Id'
      Size = 44
    end
    object cdsIdentificacaoChave: TStringField
      FieldName = 'Chave'
      Size = 60
    end
    object cdsIdentificacaocUF: TStringField
      FieldName = 'cUF'
      Size = 2
    end
    object cdsIdentificacaocNF: TStringField
      FieldName = 'cNF'
      Size = 9
    end
    object cdsIdentificacaoNatOp: TStringField
      FieldName = 'NatOp'
      Size = 60
    end
    object cdsIdentificacaoIndPag: TStringField
      FieldName = 'IndPag'
      Size = 1
    end
    object cdsIdentificacaoMod_: TStringField
      FieldName = 'Mod_'
      Size = 2
    end
    object cdsIdentificacaoSerie: TStringField
      FieldName = 'Serie'
      Size = 3
    end
    object cdsIdentificacaoNNF: TStringField
      FieldName = 'NNF'
      Size = 11
    end
    object cdsIdentificacaoDEmi: TStringField
      FieldName = 'DEmi'
      Size = 10
    end
    object cdsIdentificacaoDSaiEnt: TStringField
      FieldName = 'DSaiEnt'
      Size = 10
    end
    object cdsIdentificacaoTpNF: TStringField
      FieldName = 'TpNF'
      Size = 1
    end
    object cdsIdentificacaoCMunFG: TStringField
      FieldName = 'CMunFG'
      Size = 7
    end
    object cdsIdentificacaoTpImp: TStringField
      FieldName = 'TpImp'
      Size = 1
    end
    object cdsIdentificacaoTpEmis: TStringField
      FieldName = 'TpEmis'
      Size = 1
    end
    object cdsIdentificacaoCDV: TStringField
      FieldName = 'CDV'
      Size = 1
    end
    object cdsIdentificacaoTpAmb: TStringField
      FieldName = 'TpAmb'
      Size = 1
    end
    object cdsIdentificacaoFinNFe: TStringField
      FieldName = 'FinNFe'
      Size = 1
    end
    object cdsIdentificacaoProcEmi: TStringField
      FieldName = 'ProcEmi'
      Size = 1
    end
    object cdsIdentificacaoVerProc: TStringField
      FieldName = 'VerProc'
    end
    object cdsIdentificacaoHoraSaida: TStringField
      FieldName = 'HoraSaida'
      Size = 10
    end
    object cdsIdentificacaoMensagemFiscal: TStringField
      FieldName = 'MensagemFiscal'
      Size = 200
    end
    object cdsIdentificacaoURL: TStringField
      FieldName = 'URL'
      Size = 1000
    end
  end
  object cdsEmitente: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 92
    object cdsEmitenteCNPJ: TStringField
      FieldName = 'CNPJ'
      Size = 18
    end
    object cdsEmitenteXNome: TStringField
      FieldName = 'XNome'
      Size = 60
    end
    object cdsEmitenteXFant: TStringField
      FieldName = 'XFant'
      Size = 60
    end
    object cdsEmitenteXLgr: TStringField
      FieldName = 'XLgr'
      Size = 60
    end
    object cdsEmitenteNro: TStringField
      FieldName = 'Nro'
      Size = 60
    end
    object cdsEmitenteXCpl: TStringField
      FieldName = 'XCpl'
      Size = 60
    end
    object cdsEmitenteXBairro: TStringField
      FieldName = 'XBairro'
      Size = 60
    end
    object cdsEmitenteCMun: TStringField
      FieldName = 'CMun'
      Size = 7
    end
    object cdsEmitenteXMun: TStringField
      FieldName = 'XMun'
      Size = 60
    end
    object cdsEmitenteUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
    object cdsEmitenteCEP: TStringField
      FieldName = 'CEP'
      Size = 9
    end
    object cdsEmitenteCPais: TStringField
      FieldName = 'CPais'
      Size = 4
    end
    object cdsEmitenteXPais: TStringField
      FieldName = 'XPais'
      Size = 60
    end
    object cdsEmitenteFone: TStringField
      FieldName = 'Fone'
      Size = 15
    end
    object cdsEmitenteIE: TStringField
      FieldName = 'IE'
      Size = 14
    end
    object cdsEmitenteIM: TStringField
      FieldName = 'IM'
      Size = 15
    end
    object cdsEmitenteIEST: TStringField
      FieldName = 'IEST'
      Size = 15
    end
    object cdsEmitenteCRT: TStringField
      FieldName = 'CRT'
      Size = 1
    end
    object cdsEmitenteDESCR_CST: TStringField
      FieldName = 'DESCR_CST'
      Size = 30
    end
    object cdsEmitenteDADOS_ENDERECO: TStringField
      FieldName = 'DADOS_ENDERECO'
      Size = 1000
    end
  end
  object cdsDestinatario: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 148
    object cdsDestinatarioCNPJCPF: TStringField
      FieldName = 'CNPJCPF'
      Size = 18
    end
    object cdsDestinatarioXNome: TStringField
      FieldName = 'XNome'
      Size = 60
    end
    object cdsDestinatarioXLgr: TStringField
      FieldName = 'XLgr'
      Size = 60
    end
    object cdsDestinatarioNro: TStringField
      FieldName = 'Nro'
      Size = 60
    end
    object cdsDestinatarioXCpl: TStringField
      FieldName = 'XCpl'
      Size = 60
    end
    object cdsDestinatarioXBairro: TStringField
      FieldName = 'XBairro'
      Size = 60
    end
    object cdsDestinatarioCMun: TStringField
      FieldName = 'CMun'
      Size = 7
    end
    object cdsDestinatarioXMun: TStringField
      FieldName = 'XMun'
      Size = 60
    end
    object cdsDestinatarioUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
    object cdsDestinatarioCEP: TStringField
      FieldName = 'CEP'
      Size = 9
    end
    object cdsDestinatarioCPais: TStringField
      FieldName = 'CPais'
      Size = 4
    end
    object cdsDestinatarioXPais: TStringField
      FieldName = 'XPais'
      Size = 60
    end
    object cdsDestinatarioFone: TStringField
      FieldName = 'Fone'
      Size = 15
    end
    object cdsDestinatarioIE: TStringField
      DisplayWidth = 18
      FieldName = 'IE'
      Size = 18
    end
    object cdsDestinatarioConsumidor: TStringField
      DisplayWidth = 150
      FieldName = 'Consumidor'
      Size = 150
    end
  end
  object cdsDadosProdutos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 204
    object cdsDadosProdutosCProd: TStringField
      FieldName = 'CProd'
      Size = 60
    end
    object cdsDadosProdutoscEAN: TStringField
      FieldName = 'cEAN'
      Size = 60
    end
    object cdsDadosProdutosXProd: TStringField
      FieldName = 'XProd'
      Size = 120
    end
    object cdsDadosProdutosinfAdProd: TStringField
      FieldName = 'infAdProd'
      Size = 500
    end
    object cdsDadosProdutosNCM: TStringField
      FieldName = 'NCM'
      Size = 8
    end
    object cdsDadosProdutosEXTIPI: TStringField
      FieldName = 'EXTIPI'
      Size = 8
    end
    object cdsDadosProdutosgenero: TStringField
      FieldName = 'genero'
      Size = 8
    end
    object cdsDadosProdutosCFOP: TStringField
      FieldName = 'CFOP'
      Size = 4
    end
    object cdsDadosProdutosUCom: TStringField
      FieldName = 'UCom'
      Size = 6
    end
    object cdsDadosProdutosQCom: TFloatField
      FieldName = 'QCom'
    end
    object cdsDadosProdutosVUnCom: TFloatField
      FieldName = 'VUnCom'
    end
    object cdsDadosProdutosVProd: TFloatField
      FieldName = 'VProd'
    end
    object cdsDadosProdutoscEANTrib: TStringField
      FieldName = 'cEANTrib'
      Size = 60
    end
    object cdsDadosProdutosUTrib2: TStringField
      FieldName = 'UTrib'
      Size = 6
    end
    object cdsDadosProdutosQTrib2: TFloatField
      FieldName = 'QTrib'
    end
    object cdsDadosProdutosVUnTrib2: TFloatField
      FieldName = 'VUnTrib'
    end
    object cdsDadosProdutosvFrete: TFloatField
      FieldName = 'vFrete'
    end
    object cdsDadosProdutosVOutro: TFloatField
      FieldName = 'VOutro'
    end
    object cdsDadosProdutosvSeg: TFloatField
      FieldName = 'vSeg'
    end
    object cdsDadosProdutosvDesc: TStringField
      FieldName = 'vDesc'
      Size = 16
    end
    object cdsDadosProdutosORIGEM: TStringField
      FieldName = 'ORIGEM'
      Size = 1
    end
    object cdsDadosProdutosCST: TStringField
      FieldName = 'CST'
      Size = 3
    end
    object cdsDadosProdutosVBC: TFloatField
      FieldName = 'VBC'
    end
    object cdsDadosProdutosPICMS: TFloatField
      FieldName = 'PICMS'
    end
    object cdsDadosProdutosVICMS: TFloatField
      FieldName = 'VICMS'
    end
    object cdsDadosProdutosVIPI: TFloatField
      FieldName = 'VIPI'
    end
    object cdsDadosProdutosPIPI: TFloatField
      FieldName = 'PIPI'
    end
    object cdsDadosProdutosDescricaoProduto: TStringField
      FieldName = 'DescricaoProduto'
      Size = 1000
    end
    object cdsDadosProdutosVTotTrib: TFloatField
      FieldName = 'VTotTrib'
    end
    object cdsDadosProdutosChaveNFe: TStringField
      FieldName = 'ChaveNFe'
      Size = 50
    end
    object cdsDadosProdutosvISSQN: TFloatField
      FieldName = 'vISSQN'
    end
    object cdsDadosProdutosvBcISSQN: TFloatField
      FieldName = 'vBcISSQN'
    end
    object cdsDadosProdutosUTrib: TStringField
      FieldName = 'Unidade'
      Size = 6
    end
    object cdsDadosProdutosQTrib: TFloatField
      FieldName = 'Quantidade'
    end
    object cdsDadosProdutosVUnTrib: TFloatField
      FieldName = 'ValorUnitario'
    end
    object cdsDadosProdutosvBcST: TFloatField
      FieldName = 'vBcST'
    end
    object cdsDadosProdutosvICMSST: TFloatField
      FieldName = 'vICMSST'
    end
    object cdsDadosProdutosnLote: TStringField
      FieldName = 'nLote'
    end
    object cdsDadosProdutosqLote: TFloatField
      FieldName = 'qLote'
    end
    object cdsDadosProdutosdFab: TDateField
      FieldName = 'dFab'
    end
    object cdsDadosProdutosdVal: TDateField
      FieldName = 'dVal'
    end
  end
  object cdsParametros: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 140
    Top = 260
    object cdsParametrosResumoCanhoto: TStringField
      FieldName = 'ResumoCanhoto'
      Size = 200
    end
    object cdsParametrosMensagem0: TStringField
      FieldName = 'Mensagem0'
      Size = 60
    end
    object cdsParametrosImagem: TStringField
      FieldName = 'Imagem'
      Size = 256
    end
    object cdsParametrosSistema: TStringField
      FieldName = 'Sistema'
      Size = 60
    end
    object cdsParametrosUsuario: TStringField
      FieldName = 'Usuario'
      Size = 60
    end
    object cdsParametrosFax: TStringField
      FieldName = 'Fax'
      Size = 60
    end
    object cdsParametrosSite: TStringField
      FieldName = 'Site'
      Size = 60
    end
    object cdsParametrosEmail: TStringField
      FieldName = 'Email'
      Size = 60
    end
    object cdsParametrosDesconto: TStringField
      FieldName = 'Desconto'
      Size = 60
    end
    object cdsParametrosTotalLiquido: TStringField
      FieldName = 'TotalLiquido'
    end
    object cdsParametrosChaveAcesso_Descricao: TStringField
      FieldName = 'ChaveAcesso_Descricao'
      Size = 90
    end
    object cdsParametrosContingencia_ID: TStringField
      FieldName = 'Contingencia_ID'
      Size = 36
    end
    object cdsParametrosContingencia_Descricao: TStringField
      FieldName = 'Contingencia_Descricao'
      Size = 60
    end
    object cdsParametrosContingencia_Valor: TStringField
      FieldName = 'Contingencia_Valor'
      Size = 60
    end
    object cdsParametrosLinhasPorPagina: TIntegerField
      FieldName = 'LinhasPorPagina'
    end
    object cdsParametrosLogoExpandido: TStringField
      FieldName = 'LogoExpandido'
      Size = 1
    end
    object cdsParametrosDESCR_CST: TStringField
      FieldName = 'DESCR_CST'
      Size = 30
    end
    object cdsParametrosConsultaAutenticidade: TStringField
      FieldName = 'ConsultaAutenticidade'
      Size = 300
    end
    object cdsParametrosCasas_qCom: TIntegerField
      FieldName = 'Casas_qCom'
    end
    object cdsParametrosCasas_vUnCom: TIntegerField
      FieldName = 'Casas_vUnCom'
    end
    object cdsParametrosMask_qCom: TStringField
      FieldName = 'Mask_qCom'
    end
    object cdsParametrosMask_vUnCom: TStringField
      FieldName = 'Mask_vUnCom'
    end
    object cdsParametrosLogoCarregado: TBlobField
      FieldName = 'LogoCarregado'
    end
    object cdsParametrosQrCodeCarregado: TBlobField
      FieldName = 'QrCodeCarregado'
      BlobType = ftGraphic
      Size = 1000
    end
    object cdsParametrosDescricaoViaEstabelec: TStringField
      FieldName = 'DescricaoViaEstabelec'
      Size = 30
    end
    object cdsParametrosQtdeItens: TIntegerField
      FieldName = 'QtdeItens'
    end
    object cdsParametrosExpandirDadosAdicionaisAuto: TStringField
      FieldName = 'ExpandirDadosAdicionaisAuto'
      Size = 1
    end
  end
  object cdsInformacoesAdicionais: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 260
    object cdsInformacoesAdicionaisOBS: TStringField
      FieldName = 'OBS'
      Size = 6900
    end
    object cdsInformacoesAdicionaisLinhasOBS: TIntegerField
      FieldName = 'LinhasOBS'
    end
  end
  object cdsDuplicatas: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 36
    object cdsDuplicatasNDup: TStringField
      FieldName = 'NDup'
      Size = 60
    end
    object cdsDuplicatasDVenc: TStringField
      FieldName = 'DVenc'
      Size = 10
    end
    object cdsDuplicatasVDup: TFloatField
      FieldName = 'VDup'
    end
    object cdsDuplicatasChaveNFe: TStringField
      FieldName = 'ChaveNFe'
      Size = 50
    end
  end
  object cdsCalculoImposto: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 92
    object cdsCalculoImpostoVBC: TFloatField
      FieldName = 'VBC'
    end
    object cdsCalculoImpostoVICMS: TFloatField
      FieldName = 'VICMS'
    end
    object cdsCalculoImpostoVBCST: TFloatField
      FieldName = 'VBCST'
    end
    object cdsCalculoImpostoVST: TFloatField
      FieldName = 'VST'
    end
    object cdsCalculoImpostoVProd: TFloatField
      FieldName = 'VProd'
    end
    object cdsCalculoImpostoVFrete: TFloatField
      FieldName = 'VFrete'
    end
    object cdsCalculoImpostoVSeg: TFloatField
      FieldName = 'VSeg'
    end
    object cdsCalculoImpostoVDesc: TFloatField
      FieldName = 'VDesc'
    end
    object cdsCalculoImpostoVII: TFloatField
      FieldName = 'VII'
    end
    object cdsCalculoImpostoVIPI: TFloatField
      FieldName = 'VIPI'
    end
    object cdsCalculoImpostoVPIS: TFloatField
      FieldName = 'VPIS'
    end
    object cdsCalculoImpostoVCOFINS: TFloatField
      FieldName = 'VCOFINS'
    end
    object cdsCalculoImpostoVOutro: TFloatField
      FieldName = 'VOutro'
    end
    object cdsCalculoImpostoVNF: TFloatField
      FieldName = 'VNF'
    end
    object cdsCalculoImpostoVTotTrib: TFloatField
      FieldName = 'VTotTrib'
    end
    object cdsCalculoImpostoVTribPerc: TFloatField
      FieldName = 'VTribPerc'
    end
    object cdsCalculoImpostoVTribFonte: TStringField
      FieldName = 'VTribFonte'
      Size = 100
    end
    object cdsCalculoImpostovTotPago: TFloatField
      FieldName = 'vTotPago'
    end
    object cdsCalculoImpostovTroco: TFloatField
      FieldName = 'vTroco'
    end
  end
  object cdsTransportador: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 148
    object cdsTransportadorModFrete: TStringField
      FieldName = 'ModFrete'
      Size = 14
    end
    object cdsTransportadorCNPJCPF: TStringField
      FieldName = 'CNPJCPF'
      Size = 18
    end
    object cdsTransportadorXNome: TStringField
      FieldName = 'XNome'
      Size = 60
    end
    object cdsTransportadorIE: TStringField
      FieldName = 'IE'
      Size = 14
    end
    object cdsTransportadorXEnder: TStringField
      FieldName = 'XEnder'
      Size = 60
    end
    object cdsTransportadorXMun: TStringField
      FieldName = 'XMun'
      Size = 60
    end
    object cdsTransportadorUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
  end
  object cdsVeiculo: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 204
    object cdsVeiculoPLACA: TStringField
      FieldName = 'PLACA'
      Size = 8
    end
    object cdsVeiculoUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
    object cdsVeiculoRNTC: TStringField
      FieldName = 'RNTC'
    end
  end
  object cdsVolumes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 260
    object cdsVolumesQVol: TFloatField
      FieldName = 'QVol'
    end
    object cdsVolumesEsp: TStringField
      FieldName = 'Esp'
      Size = 60
    end
    object cdsVolumesMarca: TStringField
      FieldName = 'Marca'
      Size = 60
    end
    object cdsVolumesNVol: TStringField
      FieldName = 'NVol'
      Size = 60
    end
    object cdsVolumesPesoL: TFloatField
      FieldName = 'PesoL'
    end
    object cdsVolumesPesoB: TFloatField
      FieldName = 'PesoB'
    end
  end
  object cdsISSQN: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 36
    object cdsISSQNvSERV: TFloatField
      FieldName = 'vSERV'
    end
    object cdsISSQNvBC: TFloatField
      FieldName = 'vBC'
    end
    object cdsISSQNvISS: TFloatField
      FieldName = 'vISS'
    end
  end
  object cdsFatura: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 92
    object cdsFaturaPagamento: TStringField
      FieldName = 'Pagamento'
    end
    object cdsFaturanFat: TStringField
      FieldName = 'nFat'
      Size = 60
    end
    object cdsFaturavOrig: TFloatField
      FieldName = 'vOrig'
    end
    object cdsFaturavDesc: TFloatField
      FieldName = 'vDesc'
    end
    object cdsFaturavLiq: TFloatField
      FieldName = 'vLiq'
    end
  end
  object cdsLocalRetirada: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 148
    object cdsLocalRetiradaCNPJ: TStringField
      FieldName = 'CNPJ'
      Size = 18
    end
    object cdsLocalRetiradaXLgr: TStringField
      FieldName = 'XLgr'
      Size = 60
    end
    object cdsLocalRetiradaNro: TStringField
      FieldName = 'Nro'
      Size = 60
    end
    object cdsLocalRetiradaXCpl: TStringField
      FieldName = 'XCpl'
      Size = 60
    end
    object cdsLocalRetiradaXBairro: TStringField
      FieldName = 'XBairro'
      Size = 60
    end
    object cdsLocalRetiradaCMun: TStringField
      FieldName = 'CMun'
      Size = 7
    end
    object cdsLocalRetiradaXMun: TStringField
      FieldName = 'XMun'
      Size = 60
    end
    object cdsLocalRetiradaUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
  end
  object cdsLocalEntrega: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 204
    object cdsLocalEntregaCNPJ: TStringField
      FieldName = 'CNPJ'
      Size = 18
    end
    object cdsLocalEntregaXLgr: TStringField
      FieldName = 'XLgr'
      Size = 60
    end
    object cdsLocalEntregaNro: TStringField
      FieldName = 'Nro'
      Size = 60
    end
    object cdsLocalEntregaXCpl: TStringField
      FieldName = 'XCpl'
      Size = 60
    end
    object cdsLocalEntregaXBairro: TStringField
      FieldName = 'XBairro'
      Size = 60
    end
    object cdsLocalEntregaCMun: TStringField
      FieldName = 'CMun'
      Size = 7
    end
    object cdsLocalEntregaXMun: TStringField
      FieldName = 'XMun'
      Size = 60
    end
    object cdsLocalEntregaUF: TStringField
      FieldName = 'UF'
      Size = 2
    end
  end
  object frxIdentificacao: TfrxDBDataset
    UserName = 'Identificacao'
    CloseDataSource = False
    FieldAliases.Strings = (
      'Id=Id'
      'Chave=Chave'
      'cUF=cUF'
      'cNF=cNF'
      'NatOp=NatOp'
      'IndPag=IndPag'
      'Mod_=Mod_'
      'Serie=Serie'
      'NNF=NNF'
      'DEmi=DEmi'
      'DSaiEnt=DSaiEnt'
      'TpNF=TpNF'
      'CMunFG=CMunFG'
      'TpImp=TpImp'
      'TpEmis=TpEmis'
      'CDV=CDV'
      'TpAmb=TpAmb'
      'FinNFe=FinNFe'
      'ProcEmi=ProcEmi'
      'VerProc=VerProc'
      'HoraSaida=HoraSaida'
      'MensagemFiscal=MensagemFiscal'
      'URL=URL')
    OpenDataSource = False
    DataSet = cdsIdentificacao
    BCDToCurrency = False
    Left = 172
    Top = 36
  end
  object frxEmitente: TfrxDBDataset
    UserName = 'Emitente'
    CloseDataSource = False
    FieldAliases.Strings = (
      'CNPJ=CNPJ'
      'XNome=XNome'
      'XFant=XFant'
      'XLgr=XLgr'
      'Nro=Nro'
      'XCpl=XCpl'
      'XBairro=XBairro'
      'CMun=CMun'
      'XMun=XMun'
      'UF=UF'
      'CEP=CEP'
      'CPais=CPais'
      'XPais=XPais'
      'Fone=Fone'
      'IE=IE'
      'IM=IM'
      'IEST=IEST'
      'CRT=CRT'
      'DESCR_CST=DESCR_CST'
      'DADOS_ENDERECO=DADOS_ENDERECO')
    OpenDataSource = False
    DataSet = cdsEmitente
    BCDToCurrency = False
    Left = 172
    Top = 92
  end
  object frxDestinatario: TfrxDBDataset
    UserName = 'Destinatario'
    CloseDataSource = False
    FieldAliases.Strings = (
      'CNPJCPF=CNPJCPF'
      'XNome=XNome'
      'XLgr=XLgr'
      'Nro=Nro'
      'XCpl=XCpl'
      'XBairro=XBairro'
      'CMun=CMun'
      'XMun=XMun'
      'UF=UF'
      'CEP=CEP'
      'CPais=CPais'
      'XPais=XPais'
      'Fone=Fone'
      'IE=IE'
      'Consumidor=Consumidor')
    OpenDataSource = False
    DataSet = cdsDestinatario
    BCDToCurrency = False
    Left = 172
    Top = 148
  end
  object frxDadosProdutos: TfrxDBDataset
    UserName = 'DadosProdutos'
    CloseDataSource = False
    FieldAliases.Strings = (
      'CProd=CProd'
      'cEAN=cEAN'
      'XProd=XProd'
      'infAdProd=infAdProd'
      'NCM=NCM'
      'EXTIPI=EXTIPI'
      'genero=genero'
      'CFOP=CFOP'
      'UCom=UCom'
      'QCom=QCom'
      'VUnCom=VUnCom'
      'VProd=VProd'
      'cEANTrib=cEANTrib'
      'UTrib=UTrib'
      'QTrib=QTrib'
      'VUnTrib=VUnTrib'
      'vFrete=vFrete'
      'VOutro=VOutro'
      'vSeg=vSeg'
      'vDesc=vDesc'
      'ORIGEM=ORIGEM'
      'CST=CST'
      'VBC=VBC'
      'PICMS=PICMS'
      'VICMS=VICMS'
      'VIPI=VIPI'
      'PIPI=PIPI'
      'DescricaoProduto=DescricaoProduto'
      'VTotTrib=VTotTrib'
      'ChaveNFe=ChaveNFe'
      'vISSQN=vISSQN'
      'vBcISSQN=vBcISSQN'
      'Unidade=Unidade'
      'Quantidade=Quantidade'
      'ValorUnitario=ValorUnitario'
      'vBcST=vBcST'
      'vICMSST=vICMSST'
      'nLote=nLote'
      'qLote=qLote'
      'dFab=dFab'
      'dVal=dVal')
    OpenDataSource = False
    DataSet = cdsDadosProdutos
    BCDToCurrency = False
    Left = 172
    Top = 204
  end
  object frxParametros: TfrxDBDataset
    UserName = 'Parametros'
    CloseDataSource = False
    FieldAliases.Strings = (
      'ResumoCanhoto=ResumoCanhoto'
      'Mensagem0=Mensagem0'
      'Imagem=Imagem'
      'Sistema=Sistema'
      'Usuario=Usuario'
      'Fax=Fax'
      'Site=Site'
      'Email=Email'
      'Desconto=Desconto'
      'TotalLiquido=TotalLiquido'
      'ChaveAcesso_Descricao=ChaveAcesso_Descricao'
      'Contingencia_ID=Contingencia_ID'
      'Contingencia_Descricao=Contingencia_Descricao'
      'Contingencia_Valor=Contingencia_Valor'
      'LinhasPorPagina=LinhasPorPagina'
      'LogoExpandido=LogoExpandido'
      'DESCR_CST=DESCR_CST'
      'ConsultaAutenticidade=ConsultaAutenticidade'
      'Casas_qCom=Casas_qCom'
      'Casas_vUnCom=Casas_vUnCom'
      'Mask_qCom=Mask_qCom'
      'Mask_vUnCom=Mask_vUnCom'
      'LogoCarregado=LogoCarregado'
      'QrCodeCarregado=QrCodeCarregado'
      'DescricaoViaEstabelec=DescricaoViaEstabelec'
      'QtdeItens=QtdeItens'
      'ExpandirDadosAdicionaisAuto=ExpandirDadosAdicionaisAuto')
    OpenDataSource = False
    DataSet = cdsParametros
    BCDToCurrency = False
    Left = 172
    Top = 260
  end
  object frxDuplicatas: TfrxDBDataset
    UserName = 'Duplicatas'
    CloseDataSource = False
    FieldAliases.Strings = (
      'NDup=NDup'
      'DVenc=DVenc'
      'VDup=VDup'
      'ChaveNFe=ChaveNFe')
    OpenDataSource = False
    DataSet = cdsDuplicatas
    BCDToCurrency = False
    Left = 304
    Top = 36
  end
  object frxCalculoImposto: TfrxDBDataset
    UserName = 'CalculoImposto'
    CloseDataSource = False
    FieldAliases.Strings = (
      'VBC=VBC'
      'VICMS=VICMS'
      'VBCST=VBCST'
      'VST=VST'
      'VProd=VProd'
      'VFrete=VFrete'
      'VSeg=VSeg'
      'VDesc=VDesc'
      'VII=VII'
      'VIPI=VIPI'
      'VPIS=VPIS'
      'VCOFINS=VCOFINS'
      'VOutro=VOutro'
      'VNF=VNF'
      'VTotTrib=VTotTrib'
      'VTribPerc=VTribPerc'
      'VTribFonte=VTribFonte'
      'vTotPago=vTotPago'
      'vTroco=vTroco')
    OpenDataSource = False
    DataSet = cdsCalculoImposto
    BCDToCurrency = False
    Left = 304
    Top = 92
  end
  object frxTransportador: TfrxDBDataset
    UserName = 'Transportador'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsTransportador
    BCDToCurrency = False
    Left = 304
    Top = 148
  end
  object frxVeiculo: TfrxDBDataset
    UserName = 'Veiculo'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsVeiculo
    BCDToCurrency = False
    Left = 304
    Top = 204
  end
  object frxVolumes: TfrxDBDataset
    UserName = 'Volumes'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsVolumes
    BCDToCurrency = False
    Left = 304
    Top = 260
  end
  object frxISSQN: TfrxDBDataset
    UserName = 'ISSQN'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsISSQN
    BCDToCurrency = False
    Left = 432
    Top = 36
  end
  object frxFatura: TfrxDBDataset
    UserName = 'Fatura'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsFatura
    BCDToCurrency = False
    Left = 432
    Top = 92
  end
  object frxLocalRetirada: TfrxDBDataset
    UserName = 'LocalRetirada'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsLocalRetirada
    BCDToCurrency = False
    Left = 432
    Top = 148
  end
  object frxLocalEntrega: TfrxDBDataset
    UserName = 'LocalEntrega'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsLocalEntrega
    BCDToCurrency = False
    Left = 432
    Top = 204
  end
  object frxInformacoesAdicionais: TfrxDBDataset
    UserName = 'InformacoesAdicionais'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsInformacoesAdicionais
    BCDToCurrency = False
    Left = 432
    Top = 260
  end
  object frxBarCodeObject: TfrxBarCodeObject
    Left = 48
    Top = 148
  end
  object frxReport: TfrxReport
    Version = '4.15.6'
    DotMatrixReport = False
    EngineOptions.DoublePass = True
    IniFile = '\Software\Fast Reports'
    PreviewOptions.AllowEdit = False
    PreviewOptions.Buttons = [pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 40401.475989294000000000
    ReportOptions.LastChange = 41991.668248865740000000
    ScriptLanguage = 'PascalScript'
    StoreInDFM = False
    OnBeforePrint = frxReportBeforePrint
    OnReportPrint = 'frxReportOnReportPrint'
    Left = 48
    Top = 36
  end
  object cdsEventos: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 272
    Top = 320
  end
  object frxEventos: TfrxDBDataset
    UserName = 'Eventos'
    CloseDataSource = False
    OpenDataSource = False
    DataSet = cdsEventos
    BCDToCurrency = False
    Left = 304
    Top = 320
  end
  object cdsPagamento: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 400
    Top = 316
    object cdsPagamentotPag: TStringField
      FieldName = 'tPag'
      Size = 50
    end
    object cdsPagamentovPag: TFloatField
      FieldName = 'vPag'
    end
    object cdsPagamentoCNPJ: TStringField
      FieldName = 'CNPJ'
      Size = 50
    end
    object cdsPagamentotBand: TStringField
      FieldName = 'tBand'
      Size = 50
    end
    object cdsPagamentocAut: TStringField
      FieldName = 'cAut'
    end
  end
  object frxPagamento: TfrxDBDataset
    UserName = 'Pagamento'
    CloseDataSource = False
    FieldAliases.Strings = (
      'tPag=tPag'
      'vPag=vPag'
      'CNPJ=CNPJ'
      'tBand=tBand'
      'cAut=cAut')
    OpenDataSource = False
    DataSet = cdsPagamento
    BCDToCurrency = False
    Left = 432
    Top = 316
  end
end
