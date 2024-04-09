unit ACBr.DANFSeX.Consts;

interface

const

  {Marca d'água}
  SMsgHomologacaoSemValorFiscal : string = 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
  SMsgNFSeCancelada             : string = 'NFS-e CANCELADA';
  {Cabeçalho}
  STituloNFSe         : string = 'Nota Fiscal de Serviço Eletrônica - NFS-e';
  SNumNota            : string = 'Número da Nota';
  SDataHoraEmissao    : string = 'Data e Hora de Emissão';
  SNumNFSeSubstituida : string = 'Número NFSe substituída:';
  SCompetencia        : string = 'Competência:';
  SNumRPS             : string = 'Num. RPS/Ser.:';
  SMunicipioPrestacao : string = 'Município de Prestação do Serviço:';
  SCodigoVerificacao  : string = 'Código de Verificação';
  SPagina             : string = 'Página';
  {Prestador}
  STituloPrestadorServicos  : string = 'PRESTADOR DE SERVIÇOS';
  SPrestadorNomeRazaoSocial : string = 'Nome/Razão Social: ';
  SPrestadorCPFCNPJ         : string = 'CPF/CNPJ: ';
  SPrestadorInscEst         : string = 'IE: ';
  SPrestadorInscMuni        : string = 'IM: ';
  {Tomador}
  STituloTomadorServicos      : string = 'TOMADOR DE SERVIÇOS';
  STomadorNomeRazaoSocial     : string = 'Nome/Razão Social: ';
  STomadorCPFCNPJ             : string = 'CPF/CNPJ: ';
  STomadorNIF                 : string = 'NIF: ';
  STomadorInscEst             : string = 'Inscrição Estadual: ';
  STomadorInscMuni            : string = 'Inscrição Municipal: ';
  STomadorEndereco            : string = 'Endereço: ';
  STomadorEnderecoComplemento : string = 'Complemento: ';
  STomadorMunicipio           : string = 'Município: ';
  STomadorUF                  : string = 'UF: ';
  STomadorEmail               : string = 'e-mail: ';
  STomadorTelefone            : string = 'Telefone: ';
  {Servicos}
  STituloDiscriminacaoServicos : string = 'DISCRIMINAÇÃO DOS SERVIÇOS';
  SServTitColunaDescricao      : string = 'Descrição';
  SServTitColunaValorUnitario  : string = 'Valor Unitário';
  SServTitColunaQuantidade     : string = 'Qtde';
  SServTitColunaValorServico   : string = 'Valor do Serviço';
  SServTitColunaBaseCalculo    : string = 'Base de Cálc.(%)';
  SServTitColunaISS            : string = 'ISS';
  SServCodigoServico             : string = 'Código do Serviço: ';
  SServAtividade                 : string = 'Atividade: ';
  SServCodigoTributacaoMunicipio : string = 'Código de Tributação do Município: ';
  {Construção Civil}
  STituloConstrucaoCivil       : string = 'DETALHAMENTO ESPECIFICO DA CONSTRUÇÃO CIVIL';
  SConstrCivilCodObra          : string = 'Código da Obra: ';
  SConstrCivilCodART           : string = 'Código ART: ';
  {Tributos e Detalhes}
  STituloTributosFederais : string = 'TRIBUTOS FEDERAIS';
  STribFedPIS             : string = 'PIS (R$)';
  STribFedCOFINS          : string = 'COFINS (R$)';
  STribFedIR              : string = 'IR (R$)';
  STribFedINSS            : string = 'INSS (R$)';
  STribFedCSLL            : string = 'CSLL (R$)';

  SCabecalhoTribDetalhamentoValores  : string = 'Detalhamento de Valores - Prestador dos Serviços ';
  STribDetVal_ValorServicos          : string = 'Valor dos Serviços';
  STribDetVal_DescontoIncondicionado : string = '(-) Desconto Incondicionado';
  STribDetVal_DescontoCondicionado   : string = '(-) Desconto Condicionado';
  STribDetVal_RetencoesFederais      : string = '(-) Retenções Federais';
  STribDetVal_OutrasRetencoes        : string = '(-) Outras Retenções';
  STribDetVal_ISSRetido              : string = '(-) ISS Retido';
  STribDetVal_ValorLiquido           : string = '(=) Valor Líquido';

  SCabecalhoTribOutrasInformacoes   : string = 'Outras Informações ';
  STribOutrInf_NaturezaOperacao     : string = 'Natureza da Operação';
  STribOutrInf_RegimeEspecial       : string = 'Regime Especial de Tributação';
  STribOutrInf_OpcaoSimplesNacional : string = 'Opção Simples Nacional = ';
  STribOutrInf_IncentivadorCultural : string = 'Incentivador Cultural = ';

  SCabecalhoTribCalculoISSQNDevido      : string = 'Cálculo do ISSQN devido no Município';
  STribCalcISSQN_ValorServicos          : string = 'Valor dos Serviços';
  STribCalcISSQN_DeducoesPermitidas     : string = '(-) Deduções permitidas em Lei';
  STribCalcISSQN_DescontoIncondicionado : string = '(-) Desconto Incondicionado';
  STribCalcISSQN_BaseCalculo            : string = '(=) Base de Cálculo';
  STribCalcISSQN_Aliquota               : string = '(x) Alíquota (%) 2,00';
  STribCalcISSQN_ISSAReter              : string = 'ISS a reter:';
  STribCalcISSQN_ValorISS               : string = '(=) Valor ISS';

  STribValorTotalNota: string = 'VALOR TOTAL DA NOTA = R$ ';
  {Outras Informações}
  STituloOutrasInformacoes  : string = 'OUTRAS INFORMAÇÕES';
  sOutrasInfDataHora        : string = 'DATA E HORA DA IMPRESSÃO:';
  sOutrasInfUsuario         : string = 'USUÁRIO:';
  sOutrasInfDesenvolvidoPor : string = 'Desenvolvido por';
  {CanhotoRecibo}
  SCanhotoRecibo_Recebemos                : string = 'Recebi(emos) de';
  SCanhotoRecibo_OsServicosConstantes     : string = 'os serviços constantes da Nota Fiscal Eletronica de Serviço  (NFSe) ao lado.';
  SCanhotoRecibo_Emissao                  : string = 'Emissão:';
  SCanhotoRecibo_Tomador                  : string = '-Tomador:';
  SCanhotoRecibo_Total                    : string = '-Total:';
  SCanhotoRecibo_Data                     : string = 'DATA';
  SCanhotoRecibo_DataPlaceHolder          : string = '_______ / _______ / __________';
  SCanhotoRecibo_IntentificacaoAssinatura : string = 'Identificação e Assinatura do Recebedor';
  SCanhotoRecibo_NumeroNota               : string = 'Número da Nota';

implementation

end.
