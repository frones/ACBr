{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Alessandro Yamasaki           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 07/12/2010: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 11/01/2011:Alessandro Yamasaki
|*  - Ajustes referente aos conteudos da Origem do Processo
|*  - Criado 'Local da execução do serviço',
|*  - Criado 'Base de Cálculo do Crédito'
|*  - Criado 'Origem Credito'
*******************************************************************************}

unit ACBrEPCBlocos;

interface

uses
  SysUtils, Classes, DateUtils, ACBrTXTUtils;

type
  /// Versão do Leiaute do arquivo - TRegistro0000
  TACBrCodVer = (
                 vlVersao100,  // Código 001 - Versão 100 ADE Cofis nº 31/2010 de 01/01/2011
                 vlVersao101,  // Código 002 - Versão 101 ADE Cofis nº 34/2010 de 01/01/2011
                 vlVersao200,  // Código 002 - Versão 200 ADE Cofis nº 20/2012
                 vlVersao201,  // Código 003 - Versão 201 ADE Cofis nº 20/2012 de 14/03/2012
                 vlVersao202,  // Código 004
                 vlVersao310,  // Código 005 - ADE Cofis nº 82/2018 - Apuração em 01/01/2019
                 vlVersao320   // Código 006 - ADE Cofis ??? - Apuração em 01/01/2020
                );
  TACBrVersaoLeiaute = TACBrCodVer;

  /// Indicador de movimento - TOpenBlocos
  TACBrIndMov = (
                 imComDados, // 0- Bloco com dados informados;
                 imSemDados  // 1- Bloco sem dados informados.
                );
  TACBrIndicadorMovimento = TACBrIndMov;

  // Tipo de Escrituração
  TACBrTipoEscrit = (
                     tpEscrOriginal,     // 0 - Original
                     tpEscrRetificadora  // 1 - Retificadora
                   );
  // Tipo de Escrituração
  TACBrTipoEscrituracao = TACBrTipoEscrit;

  // Indicador de situação especial
  TACBrIndSitEsp = (
                    indSitAbertura,      // 0 - Abertura
                    indSitCisao,         // 1 - Cisão
                    indSitFusao,         // 2 - Fusão
                    indSitIncorporacao,  // 3 - Incorporação
                    indSitEncerramento,  // 4 - Encerramento
                    indNenhum            // 5 - Vazio
                  );
  TACBrIndicadorSituacaoEspecial = TACBrIndSitEsp;

  // Indicador da natureza da pessoa juridica
  TACBrIndNatPJ = (
                   indNatPJSocEmpresariaGeral,    // 0 - Sociedade empresária geral
                   indNatPJSocCooperativa,        // 1 - Sociedade Cooperativa
                   indNatPJEntExclusivaFolhaSal,  // 2 - Entidade sujeita ao PIS/Pasep exclusivamente com base  na folha de salários
                   indNatPJSocEmpresariaGeralSCP, // 3 - Geral participante de SCP
                   indNatPJSocCooperativaSCP,     // 4 - Sociedade Cooperativa Participante SCP
                   indNatPJSocContaParticante,    // 5 - Sociedade em Conta de Particpante
                   indNatPJNenhum
                 );
  TACBrIndicadorNaturezaPJ = TACBrIndNatPJ;

  //Indicador de tipo de atividade prepoderante
  TACBrIndAtiv = (
                  indAtivIndustrial,       // 0 - Industrial ou equiparado a industrial
                  indAtivPrestadorServico, // 1 - Prestador de serviços
                  indAtivComercio,         // 2 - Atividade de comércios
                  indAtivoFincanceira,     // 3 - Atividade Financeira
                  indAtivoImobiliaria,     // 4 - Atividade Imobiliária
                  indAtivoOutros           // 9 - Outros
               );
  TACBrIndicadorAtividade = TACBrIndAtiv;

  //Codigo indicador da incidencia tributária no período (0110)
  TACBrCodIncTrib = (
                     codEscrOpIncNaoCumulativo, // 1 - Escrituração de operações com incidencia exclusivamente no regime não cumulativo
                     codEscrOpIncCumulativo,    // 2 - Escrituração de operações com incidencia exclusivamente no regime cumulativo
                     codEscrOpIncAmbos          // 3 - Escrituração de operações com incidencia nos regimes cumulativo e não cumulativo
                   );
  TACBrCodIndIncTributaria = TACBrCodIncTrib;


  //Código indicador de  método  de apropriação de  créditos  comuns, no caso  de incidencia no regime não cumulativo(COD_INC_TRIB = 1 ou 3)(0110)
  TACBrIndAproCred = (
                       indMetodoApropriacaoDireta,   // 0 - Método de apropriação direta
                       indMetodoDeRateioProporcional // 1 - Método de rateio proporcional(Receita Bruta);
                     );

  //Código indicador do Tipo de Contribuição Apurada no Período(0110)
  TACBrCodTipoCont = (
                       codIndTipoConExclAliqBasica, // 1 - Apuração da Contribuição Exclusivamente a Alíquota Básica
                       codIndTipoAliqEspecificas    // 2 - Apuração da Contribuição a Alíquotas Específicas (Diferenciadas e/ou por Unidade de Medida de Produto)
                     );
  TACBrCodIndTipoCon = TACBrCodTipoCont;

  //Código indicador do critério de escrituração e apuração adotado
  TACBrIndRegCum = (
                     codRegimeCaixa,                   // 1 – Regime de Caixa – Escrituração consolidada (Registro F500);
                     codRegimeCompetEscritConsolidada, // 2 – Regime de Competência - Escrituração consolidada (Registro F550);
                     codRegimeCompetEscritDetalhada    // 9 – Regime de Competência - Escrituração detalhada, com base nos registros dos Blocos “A”, “C”, “D” e “F”.
                   );
  TACBrCodIndCritEscrit = TACBrIndRegCum;

  /// Tipo do item – Atividades Industriais, Comerciais e Serviços:
  TACBrTipoItem = (
                    tiMercadoriaRevenda,    // 00 – Mercadoria para Revenda
                    tiMateriaPrima,         // 01 – Matéria-Prima;
                    tiEmbalagem,            // 02 – Embalagem;
                    tiProdutoProcesso,      // 03 – Produto em Processo;
                    tiProdutoAcabado,       // 04 – Produto Acabado;
                    tiSubproduto,           // 05 – Subproduto;
                    tiProdutoIntermediario, // 06 – Produto Intermediário;
                    tiMaterialConsumo,      // 07 – Material de Uso e Consumo;
                    tiAtivoImobilizado,     // 08 – Ativo Imobilizado;
                    tiServicos,             // 09 – Serviços;
                    tiOutrosInsumos,        // 10 – Outros Insumos;
                    tiOutras                // 99 – Outras
                   );

  /// Indicador do tipo de operação:
  TACBrIndOper = (
                   itoContratado,     // 0 - Serviço Contratado pelo Estabelecimento
                   itoPrestado        // 1 - Serviço Prestado pelo Estabelecimento
                 );
  TACBrIndicadorTpOperacao = TACBrIndOper;

  /// Indicador do emitente do documento fiscal:
  TACBrIndEmit = (
                  iedfProprio,       // 0 - Emissão própria
                  iedfTerceiro       // 1 - Emissão de Terceiros
                 );
  TACBrIndicadorEmitenteDF = TACBrIndEmit;

  /// Indicador do tipo de pagamento
  TACBrIndPgto = (
                  tpVista,             // 0 - À Vista
                  tpPrazo,             // 1 - A Prazo
                  tpSemPagamento,      // 9 - Sem pagamento (2 - Outros, para registros após 01/07/2012)
                  tpNenhum             // Preencher vazio
                 );
  TACBrTipoPagamento = TACBrIndPgto;

  //Código da Base de Cálculo do Crédito - {NAT_BC_CRED} - 4.3.7 - Tabela Base de Cálculo do Crédito
  TACBrNatBcCred = (
                    bccVazio,                         // ''   // vazio.
                    bccAqBensRevenda,                 // '01' // Aquisição de bens para revenda
                    bccAqBensUtiComoInsumo,           // '02' // Aquisição de bens utilizados como insumo
                    bccAqServUtiComoInsumo,           // '03' // Aquisição de serviços utilizados como insumo
                    bccEnergiaEletricaTermica,        // '04' // Energia elétrica e térmica, inclusive sob a forma de vapor
                    bccAluguelPredios,                // '05' // Aluguéis de prédios
                    bccAluguelMaqEquipamentos,        // '06' // Aluguéis de máquinas e equipamentos
                    bccArmazenagemMercadoria,         // '07' // Armazenagem de mercadoria e frete na operação de venda
                    bccConArrendamentoMercantil,      // '08' // Contraprestações de arrendamento mercantil
                    bccMaqCredDepreciacao,            // '09' // Máquinas, equipamentos e outros bens incorporados ao ativo imobilizado (crédito sobre encargos de depreciação).
                    bccMaqCredAquisicao,              // '10' // Máquinas, equipamentos e outros bens incorporados ao ativo imobilizado (crédito com base no valor de aquisição).
                    bccAmortizacaoDepreciacaoImoveis, // '11' // Amortização e Depreciação de edificações e benfeitorias em imóveis
                    bccDevolucaoSujeita,              // '12' // Devolução de Vendas Sujeitas à Incidência Não-Cumulativa
                    bccOutrasOpeComDirCredito,        // '13' // Outras Operações com Direito a Crédito
                    bccAtTransporteSubcontratacao,    // '14' // Atividade de Transporte de Cargas – Subcontratação
                    bccAtImobCustoIncorrido,          // '15' // Atividade Imobiliária – Custo Incorrido de Unidade Imobiliária
                    bccAtImobCustoOrcado,             // '16' // Atividade Imobiliária – Custo Orçado de unidade não concluída
                    bccAtPresServ,                    // '17' // Atividade de Prestação de Serviços de Limpeza, Conservação e Manutenção – vale-transporte, vale-refeição ou vale-alimentação, fardamento ou uniforme.
                    bccEstoqueAberturaBens            // '18' // Estoque de abertura de bens
                  );
  TACBrBaseCalculoCredito = TACBrNatBcCred;

  // Indicador da Origem do Crédito
  TACBrIndOrigCred = (
                       opcMercadoInterno,      // 0 – Operação no Mercado Interno
                       opcImportacao ,         // 1 – Operação de Importação
                       opcVazio                // Vazio.
                     );
  TACBrOrigemCredito = TACBrIndOrigCred;

  // Identificação dos bens ou grupo de bens incorporados ao Ativo Imobilizado - {IDENT_BEM_IMOB}
  TACBrIdentBemImob = (
                        iocEdificacoesBenfeitorias,       // '01' // Edificações e Benfeitorias
                        iocInstalacoes,                   // '03' // Instalações
                        iocMaquinas,                      // '04' // Máquinas
                        iocEquipamentos,                  // '05' // Equipamentos
                        iocVeiculos,                      // '06' // Veículos
                        iocOutros                         // '99' // Outros bens incorporados ao Ativo Imobilizado
                      );
  TACBrIdentificacaoBem = TACBrIdentBemImob;

  // Indicador da Utilização dos Bens Incorporados ao Ativo Imobilizado - {IND_UTIL_BEM_IMOB}
  TACBrIndUtilBemImob = (
                          ubiProducaoBensDestinadosVenda,   // '1' // Produção de Bens Destinados a Venda
                          ubiPrestacaoServicos,             // '2' // Prestação de Serviços
                          ubiLocacaoTerceiros,              // '3' // Locação a Terceiros
                          ubiOutros                         // '9' // Outros
                        );
  TACBrUtilBensIncorporados = TACBrIndUtilBemImob;

  // Indicador do Número de Parcelas a serem apropriadas (Crédito sobre Valor de Aquisição) - {IND_NR_PARC}
  TACBrIndNrParc = (
                     inrIntegral,           // '1' // Integral (Mês de Aquisição)
                     inr12Meses,            // '2' // 12 Meses
                     inr24Meses,            // '3' // 24 Meses
                     inr48Meses,            // '4' // 48 Meses
                     inr6Meses,             // '5' // 6 Meses (Embalagens de bebidas frias)
                     inrOutra               // '9' // Outra periodicidade definida em Lei
                   );
  TACBrNumeroParcelas = TACBrIndNrParc;

  /// Indicador de tipo de atividade - TRegistro0000
//  TACBrAtividade   = (
//                       atIndustrial,         // 0 – Industrial ou equiparado a industrial
//                       atPrestadorDeServicos,// 1 - Prestador de serviços
//                       atComercio,           // 2 - Atividade de comércio
//                       atFinanceira,         // 3 - Atividade financeira
//                       atImobiliaria,        // 4 - Atividade imobiliaria
//                       atOutros = 9          // 9 - Outros
//                      );

  /// Código da finalidade do arquivo - TRegistro0000
//  TACBrCodFinalidade = (
//                         raOriginal,     // 0 - Remessa do arquivo original
//                         raSubstituto    // 1 - Remessa do arquivo substituto
//                        );
  /// Indicador do tipo de operação:
  TACBrIndTipoOper = (
                      tpEntradaAquisicao, // 0 - Entrada
                      tpSaidaPrestacao    // 1 - Saída
                     );
  TACBrTipoOperacao = TACBrIndTipoOper;

  /// Indicador do emitente do documento fiscal
  TACBrEmitente = (
                    edEmissaoPropria,         // 0 - Emissão própria
                    edTerceiros               // 1 - Terceiro
                   );
  /// Indicador do tipo do frete
  TACBrIndFrt = (
                 tfPorContaEmitente,             // 0 - Por conta do emitente
                 tfPorContaDestinatario,         // 1 - Por conta do destinatário 
                 tfPorContaTerceiros,            // 2 - Por conta de terceiros
                 tfProprioPorContaEmitente,      // 3 - Próprio Por conta do emitente
                 tfProprioPorContaDestinatario,  // 4 - Próprio Por conta do destinatário
                 tfSemCobrancaFrete,             // 9 - Sem cobrança de frete
                 tfNenhum
                );
  TACBrTipoFrete = TACBrIndFrt;

  /// Indicador do tipo do frete da operação de redespacho
//  TACBrTipoFreteRedespacho = (
//                               frSemRedespacho,         // 0 – Sem redespacho
//                               frPorContaEmitente,      // 1 - Por conta do emitente
//                               frPorContaDestinatario,  // 2 - Por conta do destinatário
//                               frOutros,                // 9 – Outros
//                               frNenhum                 // Preencher vazio
//                              );
  /// Indicador da origem do processo
  TACBrOrigemProcesso = (
                          opJusticaFederal,   // 1 - Justiça Federal'
                          opSecexRFB,         // 3 – Secretaria da Receita Federal do Brasil
                          opOutros,           // 9 - Outros
                          opNenhum           // Preencher vazio
                         );
  ///
//  TACBrDoctoArrecada = (
//                         daEstadualArrecadacao,  // 0 - Documento Estadual de Arrecadação
//                         daGNRE                  // 1 - GNRE
//                        );
  /// Indicador do tipo de transporte
//  TACBrTipoTransporte = (
//                          ttRodoviario,         // 0 – Rodoviário
//                          ttFerroviario,        // 1 – Ferroviário
//                          ttRodoFerroviario,    // 2 – Rodo-Ferroviário
//                          ttAquaviario,         // 3 – Aquaviário
//                          ttDutoviario,         // 4 – Dutoviário
//                          ttAereo,              // 5 – Aéreo
//                          ttOutros              // 9 – Outros
//                         );
  /// Documento de importação
  TACBrDoctoImporta = (
                        diImportacao,           // 0 – Declaração de Importação
                        diSimplificadaImport    // 1 – Declaração Simplificada de Importação
                       );
  /// Indicador do tipo de título de crédito
//  TACBrTipoTitulo = (
//                      tcDuplicata,             // 00- Duplicata
//                      tcCheque,                // 01- Cheque
//                      tcPromissoria,           // 02- Promissória
//                      tcRecibo,                // 03- Recibo
//                      tcOutros                 // 99- Outros (descrever)
//                     );

  /// Movimentação física do ITEM/PRODUTO:
  TACBrIndMovFisica = (
                        mfSim,           // 0 - Sim
                        mfNao            // 1 - Não
                             );
  TACBrMovimentacaoFisica = TACBrIndMovFisica;

  /// Indicador de período de apuração do IPI
  TACBrApuracaoIPI = (
                       iaMensal,               // 0 - Mensal
                       iaDecendial,            // 1 - Decendial
                       iaVazio
                      );
  /// Indicador de tipo de referência da base de cálculo do ICMS (ST) do produto farmacêutico
//  TACBrTipoBaseMedicamento = (
//                               bmCalcTabeladoSugerido,           // 0 - Base de cálculo referente ao preço tabelado ou preço máximo sugerido;
//                               bmCalMargemAgregado,              // 1 - Base cálculo – Margem de valor agregado;
//                               bmCalListNegativa,                // 2 - Base de cálculo referente à Lista Negativa;
//                               bmCalListaPositiva,               // 3 - Base de cálculo referente à Lista Positiva;
//                               bmCalListNeutra                   // 4 - Base de cálculo referente à Lista Neutra
//                              );
  /// Tipo Produto
//  TACBrTipoProduto = (
//                       tpSimilar,   // 0 - Similar
//                       tpGenerico,  // 1 - Genérico
//                       tpMarca      // 2 - Ético ou de Marca
//                      );
  /// Indicador do tipo da arma de fogo
//  TACBrTipoArmaFogo = (
//                        tafPermitido,     // 0 - Permitido
//                        tafRestrito       // 1 - Restrito
//                       );
  /// Indicador do tipo de operação com veículo
//  TACBrTipoOperacaoVeiculo = (
//                               tovVendaPConcess,   // 0 - Venda para concessionária
//                               tovFaturaDireta,    // 1 - Faturamento direto
//                               tovVendaDireta,     // 2 - Venda direta
//                               tovVendaDConcess,   // 3 - Venda da concessionária
//                               tovVendaOutros      // 9 - Outros
//                              );
  /// Indicador do tipo de receita
//  TACBrTipoReceita = (
//                       trPropria,   // 0 - Receita própria
//                       trTerceiro   // 1 - Receita de terceiros
//                      );

  /// Indicador do tipo do veículo transportador
//  TACBrTipoVeiculo = (
//                       tvEmbarcacao,
//                       tvEmpuradorRebocador
//                      );
  /// Indicador do tipo da navegação
//  TACBrTipoNavegacao = (
//                         tnInterior,
//                         tnCabotagem
//                        );
  /// Situação do Documento
  /// Código da situação do documento fiscal:
  /// Código da situação do documento fiscal:
  TACBrCodSit = (
                 sdfRegular,                 // 00 – Documento regular
                 sdfExtRegular,              // 01 - Escrituração extemporânea de documento regular
                 sdfCancelado,               // 02 – Documento cancelado
                 sdfExtCancelado,            // 03 Escrituração extemporânea de documento cancelado
                 sdfDenegado,                // 04 NF-e ou CT-e – denegado
                 sdfInutilizado,             // 05 NF-e ou CT-e - Numeração inutilizada
                 sdfComplementar,            // 06 Documento Fiscal Complementar
                 sdfExtComplementar,         // 07 Escrituração extemporânea de documento complementar
                 sdfEspecial                 // 08 Documento Fiscal emitido com base em Regime Especial ou Norma Específica
                 );
  TACBrSituacaoDF = TACBrCodSit;

  /// Código da situação do documento fiscal (Registro 1900):
  TACBrCodSitF = (
                  csffRegular,            // 00 – Documento regular
                  csfCancelado,           // 02 – Documento cancelado
                  csfOutros               // 99 – Outros
                  );

(*
  TACBrSituacaoDocto = (
                         sdRegular,                 // 00 - Documento regular
                         sdExtempRegular,           // 01 - Escrituração extemporânea de documento regular
                         sdCancelado,               // 02 - Documento cancelado
                         sdCanceladoExtemp,         // 03 - Escrituração extemporânea de documento cancelado
                         sdDoctoDenegado,           // 04 - NF-e ou CT-e - denegado
                         sdDoctoNumInutilizada,     // 05 - NF-e ou CT-e - Numeração inutilizada
                         sdFiscalCompl,             // 06 - Documento Fiscal Complementar
                         sdExtempCompl,             // 07 - Escrituração extemporânea de documento complementar
                         sdRegimeEspecNEsp          // 08 - Documento Fiscal emitido com base em Regime Especial ou Norma Específica
                        );
*)
  /// Indicador do tipo de tarifa aplicada:
//  TACBrTipoTarifa = (
//                      tipExp,     // 0 - Exp
//                      tipEnc,     // 1 - Enc
//                      tipCI,      // 2 - CI
//                      tipOutra    // 9 - Outra
//                     );
  /// Indicador da natureza do frete
//  TACBrNaturezaFrete = (
//                         nfNegociavel,      // 0 - Negociavel
//                         nfNaoNegociavel    // 1 - Não Negociavel
//                        );

  //INDICADOR DE NATUREZA DE FRETE CONTRATADO
  TACBrNaturezaFrtContratado = (
                                 nfcVendaOnusEstVendedor,    //0 - Operações de vendas, com ônus suportado pelo estabelecimento vendedor
                                 nfcVendaOnusAdquirente,     //1 - Operações de vendas, com ônus suportado pelo adquirente
                                 nfcCompraGeraCred,          //2 - Operações de compras (bens para revenda, matériasprima e outros produtos, geradores de crédito)
                                 nfcCompraNaoGeraCred,       //3 - Operações de compras (bens para revenda, matériasprima e outros produtos, não geradores de crédito)
                                 nfcTransfAcabadosPJ,        //4 - Transferência de produtos acabados entre estabelecimentos da pessoa jurídica
                                 nfcTransfNaoAcabadosPJ,     //5 - Transferência de produtos em elaboração entre estabelecimentos da pessoa jurídica
                                 nfcOutras                   //9 - Outras.
                                );

  /// Indicador do tipo de receita
//  TACBrIndTipoReceita = (
//                          recServicoPrestado,          // 0 - Receita própria - serviços prestados;
//                          recCobrancaDebitos,          // 1 - Receita própria - cobrança de débitos;
//                          recVendaMerc,                // 2 - Receita própria - venda de mercadorias;
//                          recServicoPrePago,           // 3 - Receita própria - venda de serviço pré-pago;
//                          recOutrasProprias,           // 4 - Outras receitas próprias;
//                          recTerceiroCoFaturamento,    // 5 - Receitas de terceiros (co-faturamento);
//                          recTerceiroOutras            // 9 - Outras receitas de terceiros
//                         );
  /// Indicador do tipo de serviço prestado
//  TACBrServicoPrestado = (
//                           spTelefonia,                // 0- Telefonia;
//                           spComunicacaoDados,         // 1- Comunicação de dados;
//                           spTVAssinatura,             // 2- TV por assinatura;
//                           spAcessoInternet,           // 3- Provimento de acesso à Internet;
//                           spMultimidia,               // 4- Multimídia;
//                           spOutros                    // 9- Outros
//                          );
  /// Indicador de movimento
//  TACBrMovimentoST = (
//                       mstSemOperacaoST,   // 0 - Sem operações com ST
//                       mstComOperacaoST    // 1 - Com operações de ST
//                      );
  /// Indicador do tipo de ajuste
//  TACBrTipoAjuste = (
//                      ajDebito,            // 0 - Ajuste a débito;
//                      ajCredito            // 1- Ajuste a crédito
//                     );
  /// Indicador da origem do documento vinculado ao ajuste
//  TACBrOrigemDocto = (
//                       odPorcessoJudicial, // 0 - Processo Judicial;
//                       odProcessoAdminist, // 1 - Processo Administrativo;
//                       odPerDcomp,         // 2 - PER/DCOMP;
//                       odOutros            // 9 – Outros.
//                      );
  /// Indicador de propriedade/posse do item
//  TACBrPosseItem = (
//                     piInformante,           // 0- Item de propriedade do informante e em seu poder;
//                     piInformanteNoTerceiro, // 1- Item de propriedade do informante em posse de terceiros;
//                     piTerceiroNoInformante  // 2- Item de propriedade de terceiros em posse do informante
//                    );
  /// Informe o tipo de documento
//  TACBrTipoDocto = (
//                     docDeclaracaoExportacao,           // 0 - Declaração de Exportação;
//                     docDeclaracaoSimplesExportacao     // 1 - Declaração Simplificada de Exportação.
//                    );
  /// Preencher com
//  TACBrExportacao = (
//                      exDireta,             // 0 - Exportação Direta
//                      exIndireta            // 1 - Exportação Indireta
//                     );
  /// Informação do tipo de conhecimento de embarque
//  TACBrConhecEmbarque = (
//                          ceAWB,            //01 – AWB;
//                          ceMAWB,           //02 – MAWB;
//                          ceHAWB,           //03 – HAWB;
//                          ceCOMAT,          //04 – COMAT;
//                          ceRExpressas,     //06 – R. EXPRESSAS;
//                          ceEtiqREspressas, //07 – ETIQ. REXPRESSAS;
//                          ceHrExpressas,    //08 – HR. EXPRESSAS;
//                          ceAV7,            //09 – AV7;
//                          ceBL,             //10 – BL;
//                          ceMBL,            //11 – MBL;
//                          ceHBL,            //12 – HBL;
//                          ceCTR,            //13 – CRT;
//                          ceDSIC,           //14 – DSIC;
//                          ceComatBL,        //16 – COMAT BL;
//                          ceRWB,            //17 – RWB;
//                          ceHRWB,           //18 – HRWB;
//                          ceTifDta,         //19 – TIF/DTA;
//                          ceCP2,            //20 – CP2;
//                          ceNaoIATA,        //91 – NÂO IATA;
//                          ceMNaoIATA,       //92 – MNAO IATA;
//                          ceHNaoIATA,       //93 – HNAO IATA;
//                          ceCOutros         //99 – OUTROS.
//                         );
  /// Identificador de medição
//  TACBrMedicao = (
//                   medAnalogico,            // 0 - analógico;
//                   medDigital               // 1 – digital
//                  );
  /// Tipo de movimentação do bem ou componente
//  TACBrMovimentoBens = (
//                         mbcSI,             // SI = Saldo inicial de bens imobilizados
//                         mbcIM,             // IM = Imobilização de bem individual
//                         mbcIA,             // IA = Imobilização em Andamento - Componente
//                         mbcCI,             // CI = Conclusão de Imobilização em Andamento – Bem Resultante
//                         mbcMC,             // MC = Imobilização oriunda do Ativo Circulante
//                         mbcBA,             // BA = Baixa do Saldo de ICMS - Fim do período de apropriação
//                         mbcAT,             // AT = Alienação ou Transferência
//                         mbcPE,             // PE = Perecimento, Extravio ou Deterioração
//                         mbcOT              // OT = Outras Saídas do Imobilizado
//                        );
  /// Código de grupo de tensão
//  TACBrGrupoTensao = (
//                       gtA1,          // 01 - A1 - Alta Tensão (230kV ou mais)
//                       gtA2,          // 02 - A2 - Alta Tensão (88 a 138kV)
//                       gtA3,          // 03 - A3 - Alta Tensão (69kV)
//                       gtA3a,         // 04 - A3a - Alta Tensão (30kV a 44kV)
//                       gtA4,          // 05 - A4 - Alta Tensão (2,3kV a 25kV)
//                       gtAS,          // 06 - AS - Alta Tensão Subterrâneo 06
//                       gtB107,        // 07 - B1 - Residencial 07
//                       gtB108,        // 08 - B1 - Residencial Baixa Renda 08
//                       gtB209,        // 09 - B2 - Rural 09
//                       gtB2Rural,     // 10 - B2 - Cooperativa de Eletrificação Rural
//                       gtB2Irrigacao, // 11 - B2 - Serviço Público de Irrigação
//                       gtB3,          // 12 - B3 - Demais Classes
//                       gtB4a,         // 13 - B4a - Iluminação Pública - rede de distribuição
//                       gtB4b          // 14 - B4b - Iluminação Pública - bulbo de lâmpada
//                      );
  /// Código de classe de consumo de energia elétrica ou gás
//  TACBrClasseConsumo = (
//                         ccComercial,         // 01 - Comercial
//                         ccConsumoProprio,    // 02 - Consumo Próprio
//                         ccIluminacaoPublica, // 03 - Iluminação Pública
//                         ccIndustrial,        // 04 - Industrial
//                         ccPoderPublico,      // 05 - Poder Público
//                         ccResidencial,       // 06 - Residencial
//                         ccRural,             // 07 - Rural
//                         ccServicoPublico     // 08 -Serviço Público
//                        );
  /// Código de tipo de Ligação
//  TACBrTipoLigacao = (
//                       tlMonofasico,          // 1 - Monofásico
//                       tlBifasico,            // 2 - Bifásico
//                       tlTrifasico            // 3 - Trifásico
//                      );
  /// Código dispositivo autorizado
//  TACBrDispositivo = (
//                       cdaFormSeguranca,  // 00 - Formulário de Segurança
//                       cdaFSDA,           // 01 - FS-DA – Formulário de Segurança para Impressão de DANFE
//                       cdaNFe,            // 02 – Formulário de segurança - NF-e
//                       cdaFormContinuo,   // 03 - Formulário Contínuo
//                       cdaBlocos,         // 04 – Blocos
//                       cdaJogosSoltos     // 05 - Jogos Soltos
//                      );
  /// Código do Tipo de Assinante
//  TACBrTipoAssinante = (
//                         assComercialIndustrial,    // 1 - Comercial/Industrial
//                         assPodrPublico,            // 2 - Poder Público
//                         assResidencial,            // 3 - Residencial/Pessoa física
//                         assPublico,                // 4 - Público
//                         assSemiPublico,            // 5 - Semi-Público
//                         assOutros                  // 6 - Outros
//                        );
  /// Código da natureza da conta/grupo de contas
  TACBrNaturezaConta = (
                         ncgAtivo,        // 01 - Contas de ativo
                         ncgPassivo,      // 02 - Contas de passivo
                         ncgLiquido,      // 03 - Patrimônio líquido
                         ncgResultado,    // 04 - Contas de resultado
                         ncgCompensacao,  // 05 - Contas de compensação
                         ncgOutras        // 09 - Outras
                        );

  /// Código da tabela de modelo de documento fiscais:
//  TACBrCodModeloDoc = (
//                       dfiNFSTransporte,          // 07 - Nota Fiscal de Serviço de Transporte
//                       dfiConhecimentoRodoviario, // 08 - Conhecimento de Transporte Rodoviário de Cargas
//                       dfiConhecimentoAvulso,     // 8B - Conhecimento de Transporte de Cargas Avulso
//                       dfiConhecimentoAquaviario, // 09 - Conhecimento de Transporte Aquaviário de Cargas
//                       dfiConhecimentoAereo,      // 10 - Conhecimento Aério
//                       dfiConhecimentoFerroviario,// 11 - Conhecimento de Transporte Ferroviário de Cargas
//                       dfiConhecimentoMultimodal, // 26 - Conhecimento de Transporte Multimodal de Cargas
//                       dfiNFTranspFerro,          // 27 - Nota Fiscal de Transporte Ferroviário de Cargas
//                       dfiCTE,                    // 57 - Conhecimento de Transporte Eletrônico - CT-e
//                       dfiBilheteRedoviario,      // 13 - Bilhete de passagem Rodoviário
//                       dfiBilheteAquaviario,      // 14 - Bilhete de passagem Aquaviário
//                       dfiBilheteBagagem,         // 15 - Bilhete de passagem e Nota de Bagagem
//                       dfiBilheteFerroviario,     // 16 - Bilhete de passagem Ferrroviário
//                       dfiResumoMovimento,        // 18 - Resumo de Movimento Diário
//                       dfiCFBilhete,              // 2E - Cupom Fiscal Bilhete de Passagem
//                       dfiNFSComunicacao,         // 21 - Nota Fiscal de Serviço de Comunicação
//                       dfiNFSTelecomunicacao      // 22 - Nota Fiscal de Serviço de Telecomunicação
//                      );

  ///Código da Situação Tributária referente ao ICMS.
  TACBrCstIcms = ( sticmsNenhum,
                   sticmsTributadaIntegralmente                              , // '000' //	Tributada integralmente
                   sticmsTributadaComCobracaPorST                            , // '010' //	Tributada e com cobrança do ICMS por substituição tributária
                   sticmsComReducao                                          , // '020' //	Com redução de base de cálculo
                   sticmsIsentaComCobracaPorST                               , // '030' //	Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   sticmsIsenta                                              , // '040' //	Isenta
                   sticmsNaoTributada                                        , // '041' //	Não tributada
                   sticmsSuspensao                                           , // '050' //	Suspensão
                   sticmsDiferimento                                         , // '051' //	Diferimento
                   sticmsCobradoAnteriormentePorST                           , // '060' //	ICMS cobrado anteriormente por substituição tributária
                   sticmsComReducaoPorST                                     , // '070' //	Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   sticmsOutros                                              , // '090' //	Outros
                   sticmsEstrangeiraImportacaoDiretaTributadaIntegralmente   , // '100' // Estrangeira - Importação direta - Tributada integralmente
                   sticmsEstrangeiraImportacaoDiretaTributadaComCobracaPorST , // '110' // Estrangeira - Importação direta - Tributada e com cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraImportacaoDiretaComReducao               , // '120' // Estrangeira - Importação direta - Com redução de base de cálculo
                   sticmsEstrangeiraImportacaoDiretaIsentaComCobracaPorST    , // '130' // Estrangeira - Importação direta - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraImportacaoDiretaIsenta                   , // '140' // Estrangeira - Importação direta - Isenta
                   sticmsEstrangeiraImportacaoDiretaNaoTributada             , // '141' // Estrangeira - Importação direta - Não tributada
                   sticmsEstrangeiraImportacaoDiretaSuspensao                , // '150' // Estrangeira - Importação direta - Suspensão
                   sticmsEstrangeiraImportacaoDiretaDiferimento              , // '151' // Estrangeira - Importação direta - Diferimento
                   sticmsEstrangeiraImportacaoDiretaCobradoAnteriormentePorST, // '160' // Estrangeira - Importação direta - ICMS cobrado anteriormente por substituição tributária
                   sticmsEstrangeiraImportacaoDiretaComReducaoPorST          , // '170' // Estrangeira - Importação direta - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraImportacaoDiretaOutros                   , // '190' // Estrangeira - Importação direta - Outras
                   sticmsEstrangeiraAdqMercIntTributadaIntegralmente         , // '200' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   sticmsEstrangeiraAdqMercIntTributadaComCobracaPorST       , // '210' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraAdqMercIntComReducao                     , // '220' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   sticmsEstrangeiraAdqMercIntIsentaComCobracaPorST          , // '230' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraAdqMercIntIsenta                         , // '240' // Estrangeira - Adquirida no mercado interno - Isenta
                   sticmsEstrangeiraAdqMercIntNaoTributada                   , // '241' // Estrangeira - Adquirida no mercado interno - Não tributada
                   sticmsEstrangeiraAdqMercIntSuspensao                      , // '250' // Estrangeira - Adquirida no mercado interno - Suspensão
                   sticmsEstrangeiraAdqMercIntDiferimento                    , // '251' // Estrangeira - Adquirida no mercado interno - Diferimento
                   sticmsEstrangeiraAdqMercIntCobradoAnteriormentePorST      , // '260' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   sticmsEstrangeiraAdqMercIntComReducaoPorST                , // '270' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   sticmsEstrangeiraAdqMercIntOutros                         , // '290' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms300, // '300' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   csticms310, // '310' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   csticms320, // '320' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   csticms330, // '330' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms340, // '340' // Estrangeira - Adquirida no mercado interno - Isenta
                   csticms341, // '341' // Estrangeira - Adquirida no mercado interno - Não tributada
                   csticms350, // '350' // Estrangeira - Adquirida no mercado interno - Suspensão
                   csticms351, // '351' // Estrangeira - Adquirida no mercado interno - Diferimento
                   csticms360, // '360' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   csticms370, // '370' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms390, // '390' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms400, // '400' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   csticms410, // '410' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   csticms420, // '420' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   csticms430, // '430' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms440, // '440' // Estrangeira - Adquirida no mercado interno - Isenta
                   csticms441, // '441' // Estrangeira - Adquirida no mercado interno - Não tributada
                   csticms450, // '450' // Estrangeira - Adquirida no mercado interno - Suspensão
                   csticms451, // '451' // Estrangeira - Adquirida no mercado interno - Diferimento
                   csticms460, // '460' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   csticms470, // '470' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms490, // '490' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms500, // '500' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   csticms510, // '510' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   csticms520, // '520' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   csticms530, // '530' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms540, // '540' // Estrangeira - Adquirida no mercado interno - Isenta
                   csticms541, // '541' // Estrangeira - Adquirida no mercado interno - Não tributada
                   csticms550, // '550' // Estrangeira - Adquirida no mercado interno - Suspensão
                   csticms551, // '551' // Estrangeira - Adquirida no mercado interno - Diferimento
                   csticms560, // '560' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   csticms570, // '570' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms590, // '590' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms600, // '600' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   csticms610, // '610' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   csticms620, // '620' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   csticms630, // '630' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms640, // '640' // Estrangeira - Adquirida no mercado interno - Isenta
                   csticms641, // '641' // Estrangeira - Adquirida no mercado interno - Não tributada
                   csticms650, // '650' // Estrangeira - Adquirida no mercado interno - Suspensão
                   csticms651, // '651' // Estrangeira - Adquirida no mercado interno - Diferimento
                   csticms660, // '660' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   csticms670, // '670' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms690, // '690' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms700, // '700' // Estrangeira - Adquirida no mercado interno - Tributada integralmente
                   csticms710, // '710' // Estrangeira - Adquirida no mercado interno - Tributada e com cobrança do ICMS por substituição tributária
                   csticms720, // '720' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo
                   csticms730, // '730' // Estrangeira - Adquirida no mercado interno - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms740, // '740' // Estrangeira - Adquirida no mercado interno - Isenta
                   csticms741, // '741' // Estrangeira - Adquirida no mercado interno - Não tributada
                   csticms750, // '750' // Estrangeira - Adquirida no mercado interno - Suspensão
                   csticms751, // '751' // Estrangeira - Adquirida no mercado interno - Diferimento
                   csticms760, // '760' // Estrangeira - Adquirida no mercado interno - ICMS cobrado anteriormente por substituição tributária
                   csticms770, // '770' // Estrangeira - Adquirida no mercado interno - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms790, // '790' // Estrangeira - Adquirida no mercado interno - Outras
                   csticms800, // '800' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Tributada integralmente
                   csticms810, // '810' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Tributada e com cobrança do ICMS por substituição tributária
                   csticms820, // '820' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Com redução de base de cálculo
                   csticms830, // '830' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
                   csticms840, // '840' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Isenta
                   csticms841, // '841' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Não tributada
                   csticms850, // '850' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Suspensão
                   csticms851, // '851' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Diferimento
                   csticms860, // '860' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - ICMS cobrado anteriormente por substituição tributária
                   csticms870, // '870' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
                   csticms890, // '890' // Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70% (setenta por cento) - Outras

                   sticmsSimplesNacionalTributadaComPermissaoCredito         , // '101' // Simples Nacional - Tributada pelo Simples Nacional com permissão de crédito
                   sticmsSimplesNacionalTributadaSemPermissaoCredito         , // '102' // Simples Nacional - Tributada pelo Simples Nacional sem permissão de crédito
                   sticmsSimplesNacionalIsencaoPorFaixaReceitaBruta          , // '103' // Simples Nacional - Isenção do ICMS no Simples Nacional para faixa de receita bruta
                   sticmsSimplesNacionalTributadaComPermissaoCreditoComST    , // '201' // Simples Nacional - Tributada pelo Simples Nacional com permissão de crédito e com cobrança do ICMS por substituição tributária
                   sticmsSimplesNacionalTributadaSemPermissaoCreditoComST    , // '202' // Simples Nacional - Tributada pelo Simples Nacional sem permissão de crédito e com cobrança do ICMS por substituição tributária
                   sticmsSimplesNacionalIsencaoPorFaixaReceitaBrutaComST     , // '203' // Simples Nacional - Isenção do ICMS no Simples Nacional para faixa de receita bruta e com cobrança do ICMS por substituição tributária
                   sticmsSimplesNacionalImune                                , // '300' // Simples Nacional - Imune
                   sticmsSimplesNacionalNaoTributada                         , // '400' // Simples Nacional - Não tributada pelo Simples Nacional
                   sticmsSimplesNacionalCobradoAnteriormentePorST            , // '500' // Simples Nacional - ICMS cobrado anteriormente por substituição tributária (substituído) ou por antecipação
                   sticmsSimplesNacionalOutros                               , // '900' // Simples Nacional - Outros

                   sticmsTributacaoMonofasicaPropriaCombustives              , // '002' // Tributação Monofásica Própria do ICMS nas operações com combustíveis
                   sticmsTributacaoMonofasicaPropriacomRetencaoCombustiveis  , // '015' // Tributação Monofásica Própria e com responsabilidade pela retenção do ICMS nas operações com combustíveis
                   sticmsTributacaoMonofasicaRecolhimentoDiferidoCombustiveis, // '053' // Tributação Monofásica com recolhimento diferido do ICMS nas operações com combustíveis
                   sticmsTributacaoMonofasicaCombustiveisCobradoAnteriormente  // '061' // Tributação Monofásica sobre combustíveis com ICMS cobrado anteriormente
                );
  TACBrSituacaoTribICMS = TACBrCstIcms;

  /// Código da Situação Tributária referente ao IPI.
  TACBrCstIpi = (
                 stipiEntradaRecuperacaoCredito ,// '00' // Entrada com recuperação de crédito
                 stipiEntradaTributradaZero     ,// '01' // Entrada tributada com alíquota zero
                 stipiEntradaIsenta             ,// '02' // Entrada isenta
                 stipiEntradaNaoTributada       ,// '03' // Entrada não-tributada
                 stipiEntradaImune              ,// '04' // Entrada imune
                 stipiEntradaComSuspensao       ,// '05' // Entrada com suspensão
                 stipiOutrasEntradas            ,// '49' // Outras entradas
                 stipiSaidaTributada            ,// '50' // Saída tributada
                 stipiSaidaTributadaZero        ,// '51' // Saída tributada com alíquota zero
                 stipiSaidaIsenta               ,// '52' // Saída isenta
                 stipiSaidaNaoTributada         ,// '53' // Saída não-tributada
                 stipiSaidaImune                ,// '54' // Saída imune
                 stipiSaidaComSuspensao         ,// '55' // Saída com suspensão
                 stipiOutrasSaidas              ,// '99' // Outras saídas
                 stipiVazio
                );
  TACBrSituacaoTribIPI = TACBrCstIpi;

  /// Código da Situação Tributária referente ao PIS.
  TACBrCstPis = (
                  stpisValorAliquotaNormal,                            // '01' // Operação Tributável com Alíquota Básica   // valor da operação alíquota normal (cumulativo/não cumulativo)).
                  stpisValorAliquotaDiferenciada,                      // '02' // Operação Tributável com Alíquota Diferenciada // valor da operação (alíquota diferenciada)).
                  stpisQtdeAliquotaUnidade,                            // '03' // Operação Tributável com Alíquota por Unidade de Medida de Produto // quantidade vendida x alíquota por unidade de produto).
                  stpisMonofaticaAliquotaZero,                         // '04' // Operação Tributável Monofásica - Revenda a Alíquota Zero
                  stpisValorAliquotaPorST,                             // '05' // Operação Tributável por Substituição Tributária
                  stpisAliquotaZero,                                   // '06' // Operação Tributável a Alíquota Zero
                  stpisIsentaContribuicao,                             // '07' // Operação Isenta da Contribuição
                  stpisSemIncidenciaContribuicao,                      // '08' // Operação sem Incidência da Contribuição
                  stpisSuspensaoContribuicao,                          // '09' // Operação com Suspensão da Contribuição
                  stpisOutrasOperacoesSaida,                           // '49' // Outras Operações de Saída
                  stpisOperCredExcRecTribMercInt,                      // '50' // Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno
                  stpisOperCredExcRecNaoTribMercInt,                   // '51' // Operação com Direito a Crédito – Vinculada Exclusivamente a Receita Não Tributada no Mercado Interno
                  stpisOperCredExcRecExportacao ,                      // '52' // Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação
                  stpisOperCredRecTribNaoTribMercInt,                  // '53' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno
                  stpisOperCredRecTribMercIntEExportacao,              // '54' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação
                  stpisOperCredRecNaoTribMercIntEExportacao,           // '55' // Operação com Direito a Crédito - Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação
                  stpisOperCredRecTribENaoTribMercIntEExportacao,      // '56' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação
                  stpisCredPresAquiExcRecTribMercInt,                  // '60' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno
                  stpisCredPresAquiExcRecNaoTribMercInt,               // '61' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno
                  stpisCredPresAquiExcExcRecExportacao,                // '62' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação
                  stpisCredPresAquiRecTribNaoTribMercInt,              // '63' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno
                  stpisCredPresAquiRecTribMercIntEExportacao,          // '64' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação
                  stpisCredPresAquiRecNaoTribMercIntEExportacao,       // '65' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação
                  stpisCredPresAquiRecTribENaoTribMercIntEExportacao,  // '66' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação
                  stpisOutrasOperacoes_CredPresumido,                  // '67' // Crédito Presumido - Outras Operações
                  stpisOperAquiSemDirCredito,                          // '70' // Operação de Aquisição sem Direito a Crédito
                  stpisOperAquiComIsensao,                             // '71' // Operação de Aquisição com Isenção
                  stpisOperAquiComSuspensao,                           // '72' // Operação de Aquisição com Suspensão
                  stpisOperAquiAliquotaZero,                           // '73' // Operação de Aquisição a Alíquota Zero
                  stpisOperAqui_SemIncidenciaContribuicao,             // '74' // Operação de Aquisição sem Incidência da Contribuição
                  stpisOperAquiPorST,                                  // '75' // Operação de Aquisição por Substituição Tributária
                  stpisOutrasOperacoesEntrada,                         // '98' // Outras Operações de Entrada
                  stpisOutrasOperacoes,                                // '99' // Outras Operações
                  stpisNenhum                                          // '00' // Nenhum
                 );
  TACBrSituacaoTribPIS = TACBrCstPis;

  /// Código da Situação Tributária referente ao COFINS.
  TACBrCstCofins = (
                    stcofinsValorAliquotaNormal,                           // '01' // Operação Tributável com Alíquota Básica                           // valor da operação alíquota normal (cumulativo/não cumulativo)).
                    stcofinsValorAliquotaDiferenciada,                     // '02' // Operação Tributável com Alíquota Diferenciada                     // valor da operação (alíquota diferenciada)).
                    stcofinsQtdeAliquotaUnidade,                           // '03' // Operação Tributável com Alíquota por Unidade de Medida de Produto // quantidade vendida x alíquota por unidade de produto).
                    stcofinsMonofaticaAliquotaZero,                        // '04' // Operação Tributável Monofásica - Revenda a Alíquota Zero
                    stcofinsValorAliquotaPorST,                            // '05' // Operação Tributável por Substituição Tributária
                    stcofinsAliquotaZero,                                  // '06' // Operação Tributável a Alíquota Zero
                    stcofinsIsentaContribuicao,                            // '07' // Operação Isenta da Contribuição
                    stcofinsSemIncidenciaContribuicao,                     // '08' // Operação sem Incidência da Contribuição
                    stcofinsSuspensaoContribuicao,                         // '09' // Operação com Suspensão da Contribuição
                    stcofinsOutrasOperacoesSaida,                          // '49' // Outras Operações de Saída
                    stcofinsOperCredExcRecTribMercInt,                     // '50' // Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno
                    stcofinsOperCredExcRecNaoTribMercInt,                  // '51' // Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno
                    stcofinsOperCredExcRecExportacao ,                     // '52' // Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação
                    stcofinsOperCredRecTribNaoTribMercInt,                 // '53' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno
                    stcofinsOperCredRecTribMercIntEExportacao,             // '54' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação
                    stcofinsOperCredRecNaoTribMercIntEExportacao,          // '55' // Operação com Direito a Crédito - Vinculada a Receitas Não Tributadas no Mercado Interno e de Exportação
                    stcofinsOperCredRecTribENaoTribMercIntEExportacao,     // '56' // Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno e de Exportação
                    stcofinsCredPresAquiExcRecTribMercInt,                 // '60' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno
                    stcofinsCredPresAquiExcRecNaoTribMercInt,              // '61' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno
                    stcofinsCredPresAquiExcExcRecExportacao,               // '62' // Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação
                    stcofinsCredPresAquiRecTribNaoTribMercInt,             // '63' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno
                    stcofinsCredPresAquiRecTribMercIntEExportacao,         // '64' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação
                    stcofinsCredPresAquiRecNaoTribMercIntEExportacao,      // '65' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação
                    stcofinsCredPresAquiRecTribENaoTribMercIntEExportacao, // '66' // Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno e de Exportação
                    stcofinsOutrasOperacoes_CredPresumido,                 // '67' // Crédito Presumido - Outras Operações
                    stcofinsOperAquiSemDirCredito,                         // '70' // Operação de Aquisição sem Direito a Crédito
                    stcofinsOperAquiComIsensao,                            // '71' // Operação de Aquisição com Isenção
                    stcofinsOperAquiComSuspensao,                          // '72' // Operação de Aquisição com Suspensão
                    stcofinsOperAquiAliquotaZero,                          // '73' // Operação de Aquisição a Alíquota Zero
                    stcofinsOperAqui_SemIncidenciaContribuicao,            // '74' // Operação de Aquisição sem Incidência da Contribuição
                    stcofinsOperAquiPorST,                                 // '75' // Operação de Aquisição por Substituição Tributária
                    stcofinsOutrasOperacoesEntrada,                        // '98' // Outras Operações de Entrada
                    stcofinsOutrasOperacoes,                               // '99' // Outras Operações
                    stcofinsNenhum                                         // '00' // Nenhum
                  );
  TACBrSituacaoTribCOFINS = TACBrCstCofins;

  TACBrCstPisCofins = (
                    stpiscofinsOperTribuComAliqBasica,                     //01 Operação Tributável com Alíquota Básica
                    stpiscofinsOperTribuAliqZero,                          //06 Operação Tributável a Alíquota Zero
                    stpiscofinsOperIsentaContribuicao,                     //07 Operação Isenta da Contribuição
                    stpiscofinsOperSemIncidenciaContribuicao,              //08 Operação sem Incidência da Contribuição
                    stpiscofinsOperComSuspensaoContribuicao,               //09 Operação com Suspensão da Contribuição
                    stpiscofinsOutrasOperacoesSaida,                       //49 Outras Operações de Saída
                    stpiscofinsOutrasDespesas,                             //99 Outras Operações
                    stpiscofinsNenhum
                  );
  TACBrSituacaoTribPISCOFINS = TACBrCstPisCofins;

  // Local da Execução do Serviço
  TACBrLocalExecServico = (
                            lesExecutPais,     // 0 – Executado no País;
                            lesExecutExterior  // 1 – Executado no Exterior, cujo resultado se verifique no País.
                          );




  //Código indicador da tabela de incidencia, conforme anexo III
  TACBrIndCodIncidencia = (
                            codIndiTabNaoTem,
                            codIndTabI,     // 01 - Tabela I
                            codIndTabII,    // 02 - Tabela II
                            codIndTabIII,   // 03 - Tabela III
                            codIndTabIV,    // 04 - Tabela IV
                            codIndTabV,     // 05 - Tabela V
                            codIndTabVI,    // 06 - Tabela VI
                            codIndTabVII,   // 07 - Tabela VII
                            codIndTabVIII,  // 08 - Tabela VIII
                            codIndTabIX,    // 09 - Tabela IX
                            codIndTabX,     // 10 - Tabela X
                            codIndTabXI,    // 11 - Tabela XI
                            codIndiTabXII   // 12 - Tabela XII
                          );
  //Indicador do tipo de conta (0500)
  TACBrIndCTA = (
                   indCTASintetica,  //S Sintética
                   indCTAnalitica    //A Analitica
                );
  //Indicador da apuração das contribuições e créditos, na escrituração das operações por NF-e e ECF (C010)
  TACBrIndEscrituracao = (
                          IndEscriConsolidado,     //1 – Apuração com base nos registros de consolidação das operações por NF-e (C180 e C190) e por ECF (C490);
                          IndEscriIndividualizado  //2 – Apuração com base no registro individualizado de NF-e (C100 e C170) e de ECF (C400)
                         );

  //Indicador do tipo de ajuste
  TACBrIndAJ = (
                  indAjReducao,  // '0' // Ajuste de redução;
                  indAjAcressimo // '1' // Ajuste de acréscimo.
                );
  //4.3.8 - Tabela Código de Ajustes de Contribuição ou Créditos:
  TACBrCodAj = (
                  codAjAcaoJudicial,        // '01' // Ajuste Oriundo de Ação Judicial
                  codAjProAdministrativo,   // '02' // Ajuste Oriundo de Processo Administrativo
                  codAjLegTributaria,       // '03' // Ajuste Oriundo da Legislação Tributária
                  codAjEspRTI,              // '04' // Ajuste Oriundo Especificamente do RTT
                  codAjOutrasSituacaoes,    // '05' // Ajuste Oriundo de Outras Situações
                  codAjEstorno,             // '06' // Estorno
                  codAjCPRBAdocaoRegCaixa,  // '07' // Ajuste da CPRB: Adoção do Regime de Caixa
                  codAjCPRBDiferValRecPer,  // '08' // Ajuste da CPRB: Diferimento de Valores a Recolher no Período
                  codAjCPRBAdicValDifPerAnt // '09' // Ajuste da CPRB: Adição de Valores Diferidos em Período(s) Anterior(es)
                );
  //Indicador da Natureza da Receita
  TACBrIndNatRec = (
                    inrNaoCumulativa = 0, // 0 // Receita de Natureza Não Cumulativa
                    inrCumulativa    = 1, // 1 // Receita de Natureza Cumulativa
                    inrNenhum        = 9  // 9 // Nenhum/Vazio
                   );
  //Natureza do Crédito Diferido, vinculado à receita tributada no mercado interno, a descontar
  TACBrNatCredDesc = (
                      ncdAliqBasica,       //'01' // Crédito a Alíquota Básica;
                      ncdAliqDiferenciada, //'02' // Crédito a Alíquota Diferenciada;
                      ncdAliqUnidProduto,  //'03' // Crédito a Alíquota por Unidade de Produto;
                      ncdPresAgroindustria //'04' // Crédito Presumido da Agroindústria.
                   );
  //4.3.6 - Tabela Código de Tipo de Crédito
  TACBrCodCred = (
//                  CÓDIGOS VINCULADOS À RECEITA TRIBUTADA NO MERCADO INTERNO - Grupo 100
                    ccRTMIAliqBasica,        // '101' // Crédito vinculado à receita tributada no mercado interno - Alíquota Básica
                    ccRTMIAliqDiferenciada,  // '102' // Crédito vinculado à receita tributada no mercado interno - Alíquotas Diferenciadas
                    ccRTMIAliqUnidProduto,   // '103' // Crédito vinculado à receita tributada no mercado interno - Alíquota por Unidade de Produto
                    ccRTMIEstAbertura,       // '104' // Crédito vinculado à receita tributada no mercado interno - Estoque de Abertura
                    ccRTMIAquiEmbalagem,     // '105' // Crédito vinculado à receita tributada no mercado interno - Aquisição Embalagens para revenda
                    ccRTMIPreAgroindustria,  // '106' // Crédito vinculado à receita tributada no mercado interno - Presumido da Agroindústria
                    ccRTMIImportacao,        // '108' // Crédito vinculado à receita tributada no mercado interno - Importação
                    ccRTMIAtivImobiliaria,   // '109' // Crédito vinculado à receita tributada no mercado interno - Atividade Imobiliária
                    ccRTMIOutros,            // '199' // Crédito vinculado à receita tributada no mercado interno - Outros
                  //CÓDIGOS VINCULADOS À RECEITA NÃO TRIBUTADA NO MERCADO INTERNO - Grupo 200
                    ccRNTMIAliqBasica,       // '201' // Crédito vinculado à receita não tributada no mercado interno - Alíquota Básica
                    ccRNTMIAliqDiferenciada, // '202' // Crédito vinculado à receita não tributada no mercado interno - Alíquotas Diferenciadas
                    ccRNTMIAliqUnidProduto,  // '203' // Crédito vinculado à receita não tributada no mercado interno - Alíquota por Unidade de Produto
                    ccRNTMIEstAbertura,      // '204' // Crédito vinculado à receita não tributada no mercado interno - Estoque de Abertura
                    ccRNTMIAquiEmbalagem,    // '205' // Crédito vinculado à receita não tributada no mercado interno - Aquisição Embalagens para revenda
                    ccRNTMIPreAgroindustria, // '206' // Crédito vinculado à receita não tributada no mercado interno - Presumido da Agroindústria
                    ccRNTMIImportacao,       // '208' // Crédito vinculado à receita não tributada no mercado interno - Importação
                    ccRNTMIOutros,           // '299' // Crédito vinculado à receita não tributada no mercado interno - Outros
                  //CÓDIGOS VINCULADOS À RECEITA DE EXPORTAÇÃO - Grupo 300
                    ccREAliqBasica,          // '301' // Crédito vinculado à receita de exportação - Alíquota Básica
                    ccREAliqDiferenciada,    // '302' // Crédito vinculado à receita de exportação - Alíquotas Diferenciadas
                    ccREAliqUnidProduto,     // '303' // Crédito vinculado à receita de exportação - Alíquota por Unidade de Produto
                    ccREEstAbertura,         // '304' // Crédito vinculado à receita de exportação - Estoque de Abertura
                    ccREAquiEmbalagem,       // '305' // Crédito vinculado à receita de exportação - Aquisição Embalagens para revenda
                    ccREPreAgroindustria,    // '306' // Crédito vinculado à receita de exportação - Presumido da Agroindústria
                    ccREPreAgroindustriaPCR, // '307' // Crédito vinculado à receita de exportação - Presumido da Agroindústria – Passível de Compensação e/ou Ressarcimento
                    ccREImportacao,          // '308' // Crédito vinculado à receita de exportação - Importação
                    ccREOutros               // '399' // Crédito vinculado à receita de exportação - Outros
                 );
  //Indicador do Tipo de Sociedade Cooperativa:
  TACBrIndTipCoop = (
                      itcProdAgropecuaria, // '01' // Cooperativa de Produção Agropecuária;
                      itcConsumo,          // '02' // Cooperativa de Consumo;
                      itcCredito,          // '03' // Cooperativa de Crédito;
                      itcEletRural,        // '04' // Cooperativa de Eletrificação Rural;
                      itcTransCargas,      // '05' // Cooperativa de Transporte Rodoviário de Cargas;
                      itcMedicos,          // '06' // Cooperativa de Médicos;
                      itcOutras            // '99' // Outras.
                     );
  //Indicador de Crédito Oriundo de:
  TACBrIndCredOri = (
                      icoOperProprias   = 0, // 0 // Operações próprias
                      icoEvenFusaoCisao = 1 // 1 // Evento de incorporação, cisão ou fusão
                     );

  //Indicador do tipo de receita:
  TACBrIndRec = (
                  irPropServPrestados         = 0,  // 0 // Receita própria - serviços prestados;
                  irPropCobDebitos            = 1,  // 1 // Receita própria - cobrança de débitos;
                  irPropServPrePagAnterior    = 2,  // 2 // Receita própria - venda de serviço pré-pago – faturamento de períodos anteriores;
                  irPropServPrePagAtual       = 3,  // 3 // Receita própria - venda de serviço pré-pago – faturamento no período;
                  irPropServOutrosComunicacao = 4,  // 4 // Outras receitas próprias de serviços de comunicação e telecomunicação;
                  irCFaturamento              = 5,  // 5 // Receita própria - co-faturamento;
                  irServAFaturar              = 6,  // 6 // Receita própria – serviços a faturar em período futuro;
                  irNaoAcumulativa            = 7,  // 7 // Outras receitas próprias de natureza não-cumulativa;
                  irTerceiros                 = 8,  // 8 // Outras receitas de terceiros
                  irOutras                    = 9   // 9 // Outras receitas
                 );

  //Indicador de opção de utilização do crédito disponível no período:
  TACBrIndDescCred = (
                       idcTotal   = 0, // 0 // Utilização do valor total para desconto da contribuição apurada no período, no Registro M200;
                       idcParcial = 1  // 1 // Utilização de valor parcial para desconto da contribuição apurada no período, no Registro M200
                     );

  //4.3.5 - Tabela Código de Contribuição Social Apurada
  TACBrCodCont = (
                    ccNaoAcumAliqBasica ,                // 01 // Contribuição não-cumulativa apurada a alíquota básica
                    ccNaoAcumAliqDiferenciada ,          // 02 // Contribuição não-cumulativa apurada a alíquotas diferenciadas
                    ccNaoAcumAliqUnidProduto ,           // 03 // Contribuição não-cumulativa apurada a alíquota por unidade de medida de produto
                    ccNaoAcumAliqBasicaAtivImobiliaria , // 04 // Contribuição não-cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaPorST ,                     // 31 // Contribuição apurada por substituição tributária
                    ccApuradaPorSTManaus ,               // 32 // Contribuição apurada por substituição tributária - Vendas à Zona Franca de Manaus
                    ccAcumAliqBasica ,                   // 51 // Contribuição cumulativa apurada a alíquota básica
                    ccAcumAliqDiferenciada ,             // 52 // Contribuição cumulativa apurada a alíquotas diferenciadas
                    ccAcumAliqUnidProduto ,              // 53 // Contribuição cumulativa apurada a alíquota por unidade de medida de produto
                    ccAcumAliqBasicaAtivImobiliaria ,    // 54 // Contribuição cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaAtivImobiliaria ,           // 70 // Contribuição apurada da Atividade Imobiliária - RET
                    ccApuradaSCPNaoCumulativa ,          // 71 // Contribuição apurada de SCP - Incidência Não Cumulativa
                    ccApuradaSCPCumulativa  ,            // 72 // Contribuição apurada de SCP - Incidência Cumulativa
                    ccPISPasepSalarios                   // 99 // Contribuição para o PIS/Pasep - Folha de Salários
                 );

   //Indicador de Natureza da Retenção na Fonte:
   TACBrIndNatRetFonte = (
                           indRetOrgAutarquiasFundFederais, // 01 - Retenção por Órgãos, Autarquias e Fundações Federais
                           indRetEntAdmPublicaFederal,      // 02 - Retenção por outras Entidades da Administração Pública Federal
                           indRetPesJuridicasDireitoPri,    // 03 - Retenção por Pessoas Jurídicas de Direito Privado
                           indRecolhimentoSociedadeCoop,    // 04 - Recolhimento por Sociedade Cooperativa
                           indRetFabricanteMaqVeiculos,     // 05 - Retenção por Fabricante de Máquinas e Veículos
                           indOutrasRetencoes               // 99 - Outras Retenções
                          );

   //Indicador de Origem de Deduções Diversas:
   TACBrIndOrigemDiversas = (

                              indCredPreMed,              // 01 – Créditos Presumidos - Medicamentos
                              indCredAdmRegCumulativoBeb, // 02 – Créditos Admitidos no Regime Cumulativo – Bebidas Frias
                              indContribSTZFM,            // 03 – Contribuição Paga pelo Substituto Tributário - ZFM
                              indSTNaoOCFatoGeradorPres,  // 04 – Substituição Tributária – Não Ocorrência do Fato Gerador Presumido
                              indOutrasDeducoes           // 99 - Outras Deduções

                             );

   //Indicador da Natureza da Dedução:
   TACBrIndNatDeducao = (
                          indNaoAcumulativa,  // 0 – Dedução de Natureza Não Cumulativa
                          indAcumulativa // 1 – Dedução de Natureza Cumulativa
                        );

   //Indicador do Tipo da Operação (RegsitroF100 - IND_OPER):
   TACBrIndTpOperacaoReceita = (
                          indRepCustosDespesasEncargos, //0 – Operação Representativa de Aquisição, Custos, Despesa ou Encargos, Sujeita à Incidência de Crédito de PIS/Pasep ou Cofins (CST 50 a 66).
                          indRepReceitaAuferida,        //1 – Operação Representativa de Receita Auferida Sujeita ao Pagamento da Contribuição para o PIS/Pasep e da Cofins (CST 01, 02, 03 ou 05).
                          indRepReceitaNaoAuferida      //2 - Operação Representativa de Receita Auferida Não Sujeita ao Pagamento da Contribuição para o PIS/Pasep e da Cofins (CST 04, 06, 07, 08, 09, 49 ou 99).
                        );

   //Indicador da composição da receita recebida no período (RegsitroF525 - IND_REC):
   TACBrInd_Rec = (
                   crCliente,          //01- Clientes
                   crAdministradora,   //02- Administradora de cartão de débito/crédito
                   crTituloDeCredito,  //03- Título de crédito - Duplicata, nota promissória, cheque, etc.
                   crDocumentoFiscal,  //04- Documento fiscal
                   crItemVendido,      //05- Item vendido (produtos e serviços)
                   crOutros            //99- Outros (Detalhar no campo 10 – Informação Complementar)
                 );
   TACBrIndicadorDaComposicaoDaReceitaRecebida = TACBrInd_Rec;

   // Tabela Código de Ajustes de Base de Cálculo das Contribuições (*) – Versão 1.01

   TACBrTabCodAjBaseCalcContrib = (tcaVendasCanceladas,      // 01 - Vendas canceladas de receitas tributadas em períodos anteriores
                                   tcaDevolucoesVendas,      // 02 - Devoluções de vendas tributadas em períodos anteriores
                                   tcaICMSaRecolher,         // 21 - ICMS a recolher sobre Operações próprias
                                   tcaOutrVlrsDecJudicial,   // 41 - Outros valores a excluir, vinculados a decisão judicial
                                   tcaOutrVlrsSemDecJudicial // 42 - Outros valores a excluir, não vinculados a decisão judicial
                                   );

   // Indicador de apropriação do ajuste
   TACBrIndicadorApropAjuste = (iaaRefPisCofins,  //   01 – Referente ao PIS/Pasep e a Cofins
                                iaaUnicaPISPasep, //   02 – Referente unicamente ao PIS/Pasep
                                iaaRefUnicaCofins //   03 – Referente unicamente à Cofins
                                );



  TOpenBlocos = class
  private
    FIND_MOV: TACBrIndMov;    /// Indicador de movimento: 0- Bloco com dados informados, 1- Bloco sem dados informados.
  public
    property IND_MOV: TACBrIndMov read FIND_MOV write FIND_MOV;
  end;

const
  ACBrOrigemProcessoStr : array[0..3] of string =
                        ('1',  // 1 - Justiça Federal'
                        '3',   // 3 – Secretaria da Receita Federal do Brasil
                        '9',   // 9 - Outros
                        '' );  // Preencher vazio


cstcofins01 = stcofinsValorAliquotaNormal;
cstcofins02 = stcofinsValorAliquotaDiferenciada;
cstcofins03 = stcofinsQtdeAliquotaUnidade;
cstcofins04 = stcofinsMonofaticaAliquotaZero;
cstcofins05 = stcofinsValorAliquotaPorST;
cstcofins06 = stcofinsAliquotaZero;
cstcofins07 = stcofinsIsentaContribuicao;
cstcofins08 = stcofinsSemIncidenciaContribuicao;
cstcofins09 = stcofinsSuspensaoContribuicao;
cstcofins49 = stcofinsOutrasOperacoesSaida;
cstcofins50 = stcofinsOperCredExcRecTribMercInt;
cstcofins51 = stcofinsOperCredExcRecNaoTribMercInt;
cstcofins52 = stcofinsOperCredExcRecExportacao ;
cstcofins53 = stcofinsOperCredRecTribNaoTribMercInt;
cstcofins54 = stcofinsOperCredRecTribMercIntEExportacao;
cstcofins55 = stcofinsOperCredRecNaoTribMercIntEExportacao;
cstcofins56 = stcofinsOperCredRecTribENaoTribMercIntEExportacao;
cstcofins60 = stcofinsCredPresAquiExcRecTribMercInt;
cstcofins61 = stcofinsCredPresAquiExcRecNaoTribMercInt;
cstcofins62 = stcofinsCredPresAquiExcExcRecExportacao;
cstcofins63 = stcofinsCredPresAquiRecTribNaoTribMercInt;
cstcofins64 = stcofinsCredPresAquiRecTribMercIntEExportacao;
cstcofins65 = stcofinsCredPresAquiRecNaoTribMercIntEExportacao;
cstcofins66 = stcofinsCredPresAquiRecTribENaoTribMercIntEExportacao;
cstcofins67 = stcofinsOutrasOperacoes_CredPresumido;
cstcofins70 = stcofinsOperAquiSemDirCredito;
cstcofins71 = stcofinsOperAquiComIsensao;
cstcofins72 = stcofinsOperAquiComSuspensao;
cstcofins73 = stcofinsOperAquiAliquotaZero;
cstcofins74 = stcofinsOperAqui_SemIncidenciaContribuicao;
cstcofins75 = stcofinsOperAquiPorST;
cstcofins98 = stcofinsOutrasOperacoesEntrada;
cstcofins99 = stcofinsOutrasOperacoes;

cstpis01 = stpisValorAliquotaNormal;
cstpis02 = stpisValorAliquotaDiferenciada;
cstpis03 = stpisQtdeAliquotaUnidade;
cstpis04 = stpisMonofaticaAliquotaZero;
cstpis05 = stpisValorAliquotaPorST;
cstpis06 = stpisAliquotaZero;
cstpis07 = stpisIsentaContribuicao;
cstpis08 = stpisSemIncidenciaContribuicao;
cstpis09 = stpisSuspensaoContribuicao;
cstpis49 = stpisOutrasOperacoesSaida;
cstpis50 = stpisOperCredExcRecTribMercInt;
cstpis51 = stpisOperCredExcRecNaoTribMercInt;
cstpis52 = stpisOperCredExcRecExportacao;
cstpis53 = stpisOperCredRecTribNaoTribMercInt;
cstpis54 = stpisOperCredRecTribMercIntEExportacao;
cstpis55 = stpisOperCredRecNaoTribMercIntEExportacao;
cstpis56 = stpisOperCredRecTribENaoTribMercIntEExportacao;
cstpis60 = stpisCredPresAquiExcRecTribMercInt;
cstpis61 = stpisCredPresAquiExcRecNaoTribMercInt;
cstpis62 = stpisCredPresAquiExcExcRecExportacao;
cstpis63 = stpisCredPresAquiRecTribNaoTribMercInt;
cstpis64 = stpisCredPresAquiRecTribMercIntEExportacao;
cstpis65 = stpisCredPresAquiRecNaoTribMercIntEExportacao;
cstpis66 = stpisCredPresAquiRecTribENaoTribMercIntEExportacao;
cstpis67 = stpisOutrasOperacoes_CredPresumido;
cstpis70 = stpisOperAquiSemDirCredito;
cstpis71 = stpisOperAquiComIsensao;
cstpis72 = stpisOperAquiComSuspensao;
cstpis73 = stpisOperAquiAliquotaZero;
cstpis74 = stpisOperAqui_SemIncidenciaContribuicao;
cstpis75 = stpisOperAquiPorST;
cstpis98 = stpisOutrasOperacoesEntrada;
cstpis99 = stpisOutrasOperacoes;

csticms000 = sticmsTributadaIntegralmente;
csticms010 = sticmsTributadaComCobracaPorST;
csticms020 = sticmsComReducao;
csticms030 = sticmsIsentaComCobracaPorST;
csticms040 = sticmsIsenta;
csticms041 = sticmsNaoTributada;
csticms050 = sticmsSuspensao;
csticms051 = sticmsDiferimento;
csticms060 = sticmsCobradoAnteriormentePorST;
csticms070 = sticmsComReducaoPorST;
csticms090 = sticmsOutros;
csticms100 = sticmsEstrangeiraImportacaoDiretaTributadaIntegralmente;
csticms110 = sticmsEstrangeiraImportacaoDiretaTributadaComCobracaPorST;
csticms120 = sticmsEstrangeiraImportacaoDiretaComReducao;
csticms130 = sticmsEstrangeiraImportacaoDiretaIsentaComCobracaPorST;
csticms140 = sticmsEstrangeiraImportacaoDiretaIsenta;
csticms141 = sticmsEstrangeiraImportacaoDiretaNaoTributada;
csticms150 = sticmsEstrangeiraImportacaoDiretaSuspensao;
csticms151 = sticmsEstrangeiraImportacaoDiretaDiferimento;
csticms160 = sticmsEstrangeiraImportacaoDiretaCobradoAnteriormentePorST;
csticms170 = sticmsEstrangeiraImportacaoDiretaComReducaoPorST;
csticms190 = sticmsEstrangeiraImportacaoDiretaOutros;
csticms200 = sticmsEstrangeiraAdqMercIntTributadaIntegralmente;
csticms210 = sticmsEstrangeiraAdqMercIntTributadaComCobracaPorST;
csticms220 = sticmsEstrangeiraAdqMercIntComReducao;
csticms230 = sticmsEstrangeiraAdqMercIntIsentaComCobracaPorST;
csticms240 = sticmsEstrangeiraAdqMercIntIsenta;
csticms241 = sticmsEstrangeiraAdqMercIntNaoTributada;
csticms250 = sticmsEstrangeiraAdqMercIntSuspensao;
csticms251 = sticmsEstrangeiraAdqMercIntDiferimento;
csticms260 = sticmsEstrangeiraAdqMercIntCobradoAnteriormentePorST;
csticms270 = sticmsEstrangeiraAdqMercIntComReducaoPorST;
csticms290 = sticmsEstrangeiraAdqMercIntOutros;

csosnicms101 = sticmsSimplesNacionalTributadaComPermissaoCredito;
csosnicms102 = sticmsSimplesNacionalTributadaSemPermissaoCredito;
csosnicms103 = sticmsSimplesNacionalIsencaoPorFaixaReceitaBruta;
csosnicms201 = sticmsSimplesNacionalTributadaComPermissaoCreditoComST;
csosnicms202 = sticmsSimplesNacionalTributadaSemPermissaoCreditoComST;
csosnicms203 = sticmsSimplesNacionalIsencaoPorFaixaReceitaBrutaComST;
csosnicms300 = sticmsSimplesNacionalImune;
csosnicms400 = sticmsSimplesNacionalNaoTributada;
csosnicms500 = sticmsSimplesNacionalCobradoAnteriormentePorST;
csosnicms900 = sticmsSimplesNacionalOutros;

cstipi00 = stipiEntradaRecuperacaoCredito;
cstipi01 = stipiEntradaTributradaZero;
cstipi02 = stipiEntradaIsenta;
cstipi03 = stipiEntradaNaoTributada;
cstipi04 = stipiEntradaImune;
cstipi05 = stipiEntradaComSuspensao;
cstipi49 = stipiOutrasEntradas;
cstipi50 = stipiSaidaTributada;
cstipi51 = stipiSaidaTributadaZero;
cstipi52 = stipiSaidaIsenta;
cstipi53 = stipiSaidaNaoTributada;
cstipi54 = stipiSaidaImune;
cstipi55 = stipiSaidaComSuspensao;
cstipi99 = stipiOutrasSaidas;
cstipiVazio = stipiVazio;

sdRegular = sdfRegular;
sdExtempRegular = sdfExtRegular;
sdCancelado = sdfCancelado;
sdCanceladoExtemp = sdfExtCancelado;
sdDoctoDenegado = sdfDenegado;
sdDoctoNumInutilizada = sdfInutilizado;
sdFiscalCompl = sdfComplementar;
sdExtempCompl = sdfExtComplementar;
sdRegimeEspecNEsp = sdfEspecial;

bcc00 = bccVazio;
bcc01 = bccAqBensRevenda;
bcc02 = bccAqBensUtiComoInsumo;
bcc03 = bccAqServUtiComoInsumo;
bcc04 = bccEnergiaEletricaTermica;
bcc05 = bccAluguelPredios;
bcc06 = bccAluguelMaqEquipamentos;
bcc07 = bccArmazenagemMercadoria;
bcc08 = bccConArrendamentoMercantil;
bcc09 = bccMaqCredDepreciacao;
bcc10 = bccMaqCredAquisicao;
bcc11 = bccAmortizacaoDepreciacaoImoveis;
bcc12 = bccDevolucaoSujeita;
bcc13 = bccOutrasOpeComDirCredito;
bcc14 = bccAtTransporteSubcontratacao;
bcc15 = bccAtImobCustoIncorrido;
bcc16 = bccAtImobCustoOrcado;
bcc17 = bccAtPresServ;
bcc18 = bccEstoqueAberturaBens;

function CodVerToStr(AValue: TACBrCodVer): string;
function StrToCodVer(const AValue: string): TACBrCodVer;
function TipoEscritToStr(AValue: TACBrTipoEscrit): string;
function StrToTipoEscrit(const AValue: string): TACBrTipoEscrit;
function IndNatPJToStr(AValue: TACBrIndNatPJ): string;
function StrToIndNatPJ(const AValue: string): TACBrIndNatPJ;
function IndAtivToStr(AValue: TACBrIndAtiv): string;
function StrToIndAtiv(const AValue: string): TACBrIndAtiv;
function CodIncTribToStr(AValue: TACBrCodIncTrib): string;
function StrToCodIncTrib(const AValue: string): TACBrCodIncTrib;
function IndAproCredToStr(AValue: TACBrIndAproCred): string;
function StrToIndAproCred(const AValue: string): TACBrIndAproCred;
function CodTipoContToStr(AValue: TACBrCodTipoCont): string;
function StrToCodTipoCont(const AValue: string): TACBrCodTipoCont;
function IndRegCumToStr(AValue: TACBrIndRegCum): string;
function StrToIndRegCum(const AValue: string): TACBrIndRegCum;
function TipoItemToStr(AValue: TACBrTipoItem): string;
function StrToTipoItem(const AValue: string): TACBrTipoItem;
function IndOperToStr(AVAlue: TACBrIndOper): string;
function StrToIndOper(const AVAlue: string): TACBrIndOper;
function IndEmitToStr(AValue: TACBrIndEmit): string;
function StrToIndEmit(const AValue: string): TACBrIndEmit;
function CodSitToStr(AValue: TACBrCodSit): string;
function StrToCodSit(const AValue: string): TACBrCodSit;
function CodSitFToStr(AValue: TACBrCodSitF): string;
function StrToCodSitF(const AValue: string): TACBrCodSitF;
function IndPgtoToStr(AValue: TACBrIndPgto): string;
function StrToIndPgto(const AValue: string): TACBrIndPgto;
function NatBcCredToStr(AValue: TACBrNatBcCred): string;
function StrToNatBcCred(const AValue: string): TACBrNatBcCred;
function IndOrigCredToStr(AValue: TACBrIndOrigCred): string;
function StrToIndOrigCred(const AValue: String): TACBrIndOrigCred;
function IdentBemImobToStr(AValue: TACBrIdentBemImob): string;
function StrToIdentBemImob(const AValue: String): TACBrIdentBemImob;
function IndUtilBemImobToStr(AValue: TACBrIndUtilBemImob): string;
function StrToIndUtilBemImob(const AValue: String): TACBrIndUtilBemImob;
function IndNrParcToStr(AValue: TACBrIndNrParc): string;
function StrToIndNrParc(const AValue: String): TACBrIndNrParc;
function CstPisToStr(AValue: TACBrCstPis): string;
function StrToCstPis(const AValue: String): TACBrCstPis;
function CstPisCofinsToStr(AValue: TACBrCstPisCofins): string;
function StrToCstPisCofins(const AValue: String): TACBrCstPisCofins;
function CstCofinsToStr(AValue: TACBrCstCofins): string;
function StrToCstCofins(const AValue: String): TACBrCstCofins;
function CstIcmsToStr(AValue: TACBrCstIcms): string;
function StrToCstIcms(const AValue: String): TACBrCstIcms;
function CstIpiToStr(AValue: TACBrCstIpi): string;
function StrToCstIpi(const AValue: String): TACBrCstIpi;
function IndTipoOperToStr(AVAlue: TACBrIndTipoOper): string;
function StrToIndTipoOper(const AVAlue: string): TACBrIndTipoOper;
function IndMovFisicaToStr(AValue: TACBrIndMovFisica): string;
function StrToIndMovFisica(const AValue: string): TACBrIndMovFisica;
function IndFrtToStr(AValue: TACBrIndFrt): string;
function IndCTAToStr(AValue: TACBrIndCTA): string;
function StrToIndFrt(const AValue: string): TACBrIndFrt;
function StrToIndSitEsp(const AValue: string): TACBrIndSitEsp;
function NatFrtContratadoToStr(AValue: TACBrNaturezaFrtContratado): string;
function NaturezaContaToStr(AValue: TACBrNaturezaConta): string;

// 03-07-2015 - Rodrigo Coelho - Novas conversões faltantes
function IndTpOperacaoReceitaToStr(AValue: TACBrIndTpOperacaoReceita): string;
function OrigemProcessoToStr(AValue: TACBrOrigemProcesso): string;
function LocalExecServicoToStr(const AValue: TACBrLocalExecServico): string;
function IndEscrituracaoToStr(const AValue: TACBrIndEscrituracao): string;
function DoctoImportaToStr(const AValue: TACBrDoctoImporta): string;
function ApuracaoIPIToStr(const AValue: TACBrApuracaoIPI): string;
function IndMovToStr(const AValue: TACBrIndMov): string;
function EmitenteToStr(const AValue: TACBrEmitente): string;
function IndCredOriToStr(const AValue: TACBrIndCredOri): string;
function IndDescCredToStr(const AValue: TACBrIndDescCred): string;
function IndAJToStr(const AValue: TACBrIndAJ): string;
function CodContToStr(const AValue: TACBrCodCont): string;
function IndTipCoopToStr(const AValue: TACBrIndTipCoop): string;
function CodAjToStr(const AValue: TACBrCodAj): string;
function NatCredDescToStr(const AValue: TACBrNatCredDesc): string;
function TabCodAjBaseCalcToStr(const AValue: TACBrTabCodAjBaseCalcContrib): string;
function IndicadorApropAjusteToStr(const AValue: TACBrIndicadorApropAjuste): string;




// 20-02-2015 - Data Lider - Novas Conversões para Importação
function StrToIndCodIncidencia(const AValue: string): TACBrIndCodIncidencia;
function StrToIndMov(const AValue: string): TACBrIndMov;
function StrToNaturezaConta(const AValue: string): TACBrNaturezaConta;
function StrToIndCTA(const AValue: string): TACBrIndCTA;
function StrToOrigemProcesso(const AValue: string): TACBrOrigemProcesso;
function StrToLocalExecServico(const AValue: string): TACBrLocalExecServico;
function StrToIndEscrituracao(const AValue: string): TACBrIndEscrituracao;
function StrToEmitente(const AValue: string): TACBrEmitente;
function StrToDoctoImporta(const AValue: string): TACBrDoctoImporta;
function StrToApuracaoIPI(const AValue: string): TACBrApuracaoIPI;
function StrToNaturezaFrtContratado(const AValue: string): TACBrNaturezaFrtContratado;
function StrToIndRec(const AValue: string): TACBrIndRec;
function StrToIndTpOperacaoReceita(const AValue: string): TACBrIndTpOperacaoReceita;
function StrToInd_Rec(const AValue: string):TACBrInd_Rec;
function StrToIndNatRetFonte(const AValue: string):TACBrIndNatRetFonte;
function StrToIndNatRec(const AValue: string):TACBrIndNatRec;
function StrToIndOrigemDiversas(const AValue: string):TACBrIndOrigemDiversas;
function StrToIndNatDeducao(const AValue: string):TACBrIndNatDeducao;
function StrToCodCred(const AValue: string): TACBrCodCred;
function StrToIndAJ(const AValue: string):TACBrIndAJ;
function StrToCodAj(const AValue: string): TACBrCodAj;
function StrToCodCont(const AValue: string): TACBrCodCont;
function StrToIndTipCoop(const AValue: string): TACBrIndTipCoop;
function StrToNatCredDesc(const AValue: string): TACBrNatCredDesc;
function StrToIndCredOri(const AValue: string):TACBrIndCredOri;
function StrToIndDescCred(const AValue: string):TACBrIndDescCred;
function StrToCodAjBaseCalcContrib(const AValue: string):TACBrTabCodAjBaseCalcContrib;
function StrToIndicadorApropAjuste(const AValue: string): TACBrIndicadorApropAjuste;



implementation

function StrToIndDescCred(const AValue: string):TACBrIndDescCred;
begin
  Result := TACBrIndDescCred(StrToIntDef(AValue, 0));
end;

function StrToIndCredOri(const AValue: string):TACBrIndCredOri;
begin
  Result := TACBrIndCredOri(StrToIntDef(AValue, 0));
end;

function StrToNatCredDesc(const AValue: string): TACBrNatCredDesc;
begin
  if AValue = '01' then
    Result := ncdAliqBasica
  else if AValue = '02' then
    Result :=  ncdAliqDiferenciada
  else if AValue = '03' then
    Result := ncdAliqUnidProduto
  else if AValue = '04' then
    Result := ncdPresAgroindustria
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03 e 04)',[AValue]));
end;

function StrToIndTipCoop(const AValue: string): TACBrIndTipCoop;
begin
  if AValue = '01' then
    Result := itcProdAgropecuaria
  else if AValue = '02' then
    Result :=  itcConsumo
  else if AValue = '03' then
    Result := itcCredito
  else if AValue = '04' then
    Result := itcEletRural
  else if AValue = '05' then
    Result := itcTransCargas
  else if AValue = '06' then
    Result := itcMedicos
  else if AValue = '99' then
    Result := itcOutras
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,05,06 ou 99)',[AValue]));
end;

function StrToCodCont(const AValue: string): TACBrCodCont;
begin
  if AValue = '01' then
    Result := ccNaoAcumAliqBasica
  else if AValue = '02' then
    Result :=  ccNaoAcumAliqDiferenciada
  else if AValue = '03' then
    Result := ccNaoAcumAliqUnidProduto
  else if AValue = '04' then
    Result := ccNaoAcumAliqBasicaAtivImobiliaria
  else if AValue = '31' then
    Result := ccApuradaPorST
  else if AValue = '32' then
    Result := ccApuradaPorSTManaus
  else if AValue = '51' then
    Result :=  ccAcumAliqBasica
  else if AValue = '52' then
    Result := ccAcumAliqDiferenciada
  else if AValue = '53' then
    Result := ccAcumAliqUnidProduto
  else if AValue = '54' then
    Result := ccAcumAliqBasicaAtivImobiliaria
  else if AValue = '70' then
    Result :=  ccApuradaAtivImobiliaria
  else if AValue = '71' then
    Result :=  ccApuradaSCPNaoCumulativa
  else if AValue = '72' then
    Result := ccApuradaSCPCumulativa
  else if AValue = '99' then
    Result := ccPISPasepSalarios
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,31,32,51,52,53,54,70,71,72 ou 99)',[AValue]));
end;


function StrToCodAj(const AValue: string): TACBrCodAj;
begin
  if AValue = '01' then
    Result := codAjAcaoJudicial
  else if AValue = '02' then
    Result :=  codAjAcaoJudicial
  else if AValue = '03' then
    Result := codAjLegTributaria
  else if AValue = '04' then
    Result := codAjEspRTI
  else if AValue = '05' then
    Result := codAjOutrasSituacaoes
  else if AValue = '06' then
    Result := codAjEstorno
   else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,05 e 06)',[AValue]));
end;

function StrToIndAJ(const AValue: string):TACBrIndAJ;
begin
  Result := TACBrIndAJ(StrToIntDef(AValue, 0));
end;

function StrToCodCred(const AValue: string): TACBrCodCred;
begin
   //CÓDIGOS VINCULADOS À RECEITA TRIBUTADA NO MERCADO INTERNO - Grupo 100
  if AValue = '101' then
    Result := ccRTMIAliqBasica
  else if AValue = '102' then
    Result :=  ccRTMIAliqDiferenciada
  else if AValue = '103' then
    Result := ccRTMIAliqUnidProduto
  else if AValue = '104' then
    Result := ccRTMIEstAbertura
  else if AValue = '105' then
    Result := ccRTMIAquiEmbalagem
  else if AValue = '106' then
    Result := ccRTMIPreAgroindustria
  else if AValue = '108' then
    Result :=  ccRTMIImportacao
  else if AValue = '109' then
    Result := ccRTMIAtivImobiliaria
  else if AValue = '199' then
    Result := ccRTMIOutros
  //CÓDIGOS VINCULADOS À RECEITA NÃO TRIBUTADA NO MERCADO INTERNO - Grupo 200
  else if AValue = '201' then
    Result := ccRNTMIAliqBasica
  else if AValue = '202' then
    Result :=  ccRNTMIAliqDiferenciada
  else if AValue = '203' then
    Result :=  ccRNTMIAliqUnidProduto
  else if AValue = '204' then
    Result := ccRNTMIEstAbertura
  else if AValue = '205' then
    Result := ccRNTMIAquiEmbalagem
  else if AValue = '206' then
    Result :=  ccRNTMIPreAgroindustria
  else if AValue = '208' then
    Result :=  ccRNTMIImportacao
  else if AValue = '299' then
    Result := ccRNTMIOutros
  //CÓDIGOS VINCULADOS À RECEITA DE EXPORTAÇÃO - Grupo 300
  else if AValue = '301' then
    Result := ccREAliqBasica
  else if AValue = '302' then
    Result := ccREAliqDiferenciada
  else if AValue = '303' then
    Result :=  ccREAliqUnidProduto
  else if AValue = '304' then
    Result :=  ccREEstAbertura
  else if AValue = '305' then
    Result := ccREAquiEmbalagem
  else if AValue = '306' then
    Result :=  ccREPreAgroindustria
  else if AValue = '307' then
    Result := ccREPreAgroindustriaPCR
  else if AValue = '308' then
    Result := ccREImportacao
  else if AValue = '399' then
    Result := ccREOutros
   else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (101 a 109, 199, 201 a 208 e 299, 301 a 308 e 399)',[AValue]));
end;

function StrToIndNatDeducao(const AValue: string):TACBrIndNatDeducao;
begin
  Result := TACBrIndNatDeducao(StrToIntDef(AValue, 0));
end;

function StrToIndOrigemDiversas(const AValue: string):TACBrIndOrigemDiversas;
begin
  if AValue = '01' then
    Result := indCredPreMed
  else if AValue = '02' then
    Result := indCredAdmRegCumulativoBeb
  else if AValue = '03' then
    Result := indContribSTZFM
  else if AValue = '04' then
    Result := indSTNaoOCFatoGeradorPres
  else if AValue = '99' then
    Result := indOutrasDeducoes
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04 e 99)',[AValue]));
end;

function StrToIndNatRec(const AValue: string):TACBrIndNatRec;
begin
  Result := TACBrIndNatRec(StrToIntDef(AValue, 0));
end;

function StrToIndNatRetFonte(const AValue: string):TACBrIndNatRetFonte;
begin
  if AValue = '01' then
    Result := indRetOrgAutarquiasFundFederais
  else if AValue = '02' then
    Result := indRetEntAdmPublicaFederal
  else if AValue = '03' then
    Result := indRetPesJuridicasDireitoPri
  else if AValue = '04' then
    Result := indRecolhimentoSociedadeCoop
  else if AValue = '05' then
    Result := indRetFabricanteMaqVeiculos
  else if AValue = '99' then
    Result := indOutrasRetencoes
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,05 e 99)',[AValue]));
end;

function StrToInd_Rec(const AValue: string):TACBrInd_Rec;
begin
  if AValue = '01' then
    Result := crCliente
  else if AValue = '02' then
    Result :=  crAdministradora
  else if AValue = '03' then
    Result :=   crTituloDeCredito
  else if AValue = '04' then
    Result :=    crDocumentoFiscal
  else if AValue = '05' then
    Result :=   crItemVendido
  else if AValue = '99' then
    Result :=   crOutros
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,05 e 99)',[AValue]));
end;

function StrToIndTpOperacaoReceita(const AValue: string): TACBrIndTpOperacaoReceita;
begin
  Result := TACBrIndTpOperacaoReceita(StrToIntDef(AValue, 0));
end;

function StrToIndRec(const AValue: string): TACBrIndRec;
begin
  Result := TACBrIndRec(StrToIntDef(AValue, 0));
end;

function StrToNaturezaFrtContratado(const AValue: string): TACBrNaturezaFrtContratado;
begin
  if AValue = '0' then
    Result := nfcVendaOnusEstVendedor
  else if AValue = '1' then
    Result := nfcVendaOnusAdquirente
  else if AValue = '2' then
    Result := nfcCompraGeraCred
  else if AValue = '3' then
    Result := nfcCompraNaoGeraCred
  else if AValue = '4' then
    Result := nfcTransfAcabadosPJ
  else if AValue = '5' then
    Result :=  nfcTransfNaoAcabadosPJ
//  else if AValue = '9' then
//    Result := nfcOutras
  else
    Result := nfcOutras;
end;


function StrToApuracaoIPI(const AValue: string): TACBrApuracaoIPI;
begin
  if AValue = EmptyStr then
    Result := iaVazio
  else
    Result := TACBrApuracaoIPI(StrToIntDef(AValue, 0));
end;

function StrToDoctoImporta(const AValue: string): TACBrDoctoImporta;
begin
  Result := TACBrDoctoImporta(StrToIntDef(AValue, 0));
end;

function StrToEmitente(const AValue: string): TACBrEmitente;
begin
  Result := TACBrEmitente(StrToIntDef(AValue, 0));
end;

function StrToIndEscrituracao(const AValue: string): TACBrIndEscrituracao;
begin
  if AValue = '2' then
    Result := IndEscriIndividualizado
  else
    Result := IndEscriConsolidado;
end;

// 20-02-2015 - Data Lider - Início da alteração nos arquivo.
function StrToLocalExecServico(const AValue: string): TACBrLocalExecServico;
begin
  Result := TACBrLocalExecServico(StrToIntDef(AValue, 0));
end;

function StrToOrigemProcesso(const AValue: string): TACBrOrigemProcesso;
begin
  if AValue = '1' then
    Result := opJusticaFederal
  else if AValue = '3' then
    Result := opSecexRFB
  else if AValue = '9' then
    Result := opOutros
  else
    Result := opNenhum;
end;

function StrToIndCTA(const AValue: string): TACBrIndCTA;
begin
  if AValue = 'S' then
    Result := indCTASintetica
  else if AValue = 'A' then
    Result := indCTAnalitica
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (S e A)',[AValue]));
end;

function StrToNaturezaConta(const AValue: string): TACBrNaturezaConta;
begin
  if (AValue = '01') then
    Result := ncgAtivo
  else if (AValue = '02') then
    Result := ncgPassivo
  else if (AValue = '03') then
    Result := ncgLiquido
  else if (AValue = '04') then
    Result := ncgResultado
  else if (AValue = '05') then
    Result := ncgCompensacao
  else if (AValue = '09') then
    Result := ncgOutras
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,03,04,05 e 09)',[AValue]));
end;

function StrToIndMov(const AValue: string): TACBrIndMov;
begin
  Result := TACBrIndMov(StrToIntDef(AValue, 0));
end;

function StrToIndCodIncidencia(const AValue: string): TACBrIndCodIncidencia;
begin
  if (AValue = '01') then
    Result := codIndTabI // 01 - Tabela I
  else if (AValue = '02') then
    Result := codIndTabII // 02 - Tabela II
  else if (AValue = '03') then
    Result := codIndTabIII // 03 - Tabela III
  else if (AValue = '04') then
    Result := codIndTabIV // 04 - Tabela IV
  else if (AValue = '05') then
    Result := codIndTabV // 05 - Tabela V
  else if (AValue = '06') then
    Result := codIndTabVI // 06 - Tabela VI
  else if (AValue = '07') then
    Result := codIndTabVII // 07 - Tabela VII
  else if (AValue = '08') then
    Result := codIndTabVIII // 08 - Tabela VIII
  else if (AValue = '09') then
    Result := codIndTabIX // 09 - Tabela IX
  else if (AValue = '10') then
    Result := codIndTabX // 10 - Tabela X
  else if (AValue = '11') then
    Result := codIndTabXI // 11 - Tabela XI
  else if (AValue = '12') then
    Result := codIndiTabXII // 12 - Tabela XII
  else
    Result := codIndiTabNaoTem;
end;

// 20-02-2015 - Data Lider - Fim da alteração.

{ TOpenBlocos }

//function StrToEnumerado(const s: string; const AString:
//  array of string; const AEnumerados: array of variant): variant;
//var
//  iFor: integer;
//  bOK: boolean;
//begin
//  result := -1;
//  for iFor := Low(AString) to High(AString) do
//  begin
//    if AnsiSameText(s, AString[iFor]) then
//    begin
//       Result := AEnumerados[iFor];
//       Break;
//    end;
//  end;
//  bOK := Result <> -1;
//  if not bOK then
//    Result := AEnumerados[0];
//end;

//function EnumeradoToStr(const t: variant; const AString:
//  array of string; const AEnumerados: array of variant): variant;
//var
//  iFor: integer;
//begin
//  Result := '';
//  for iFor := Low(AEnumerados) to High(AEnumerados) do
//  begin
//    if t = AEnumerados[iFor] then
//    begin
//      Result := AString[iFor];
//      Break;
//    end;
//  end;
//end;

function CodVerToStr(AValue: TACBrCodVer): string;
begin
   if AValue = vlVersao100 then
      Result := '001'
   else
   if AValue = vlVersao101 then
      Result := '002'
   else
   if AValue = vlVersao200 then
      Result := '002'
   else
   if AValue = vlVersao201 then
      Result := '003'
   else
   if AValue = vlVersao202 then
      Result := '004'
   else
   if AValue = vlVersao310 then
      Result := '005'
   else
   if AValue = vlVersao320 then
      Result := '006'
   else
     raise Exception.Create('Valor informado inválido para ser convertido em TACBrCodVer');
end;

function StrToCodVer(const AValue: string): TACBrCodVer;
begin
  if AValue = '001' then
    Result := vlVersao100
  else
  if AValue = '002' then
    Result := vlVersao200
  else
  if AValue = '003' then
    Result := vlVersao201
  else
  if AValue = '004' then
    Result := vlVersao202
  else
  if AValue = '005' then
    Result := vlVersao310
  else
  if AValue = '006' then
    Result := vlVersao320
  else
    raise Exception.Create(format('Valor informado [%s] deve estar entre '+
          '[''001'', ''002'', ''003'', ''004'', ''005'', ''006'']',[AValue]));
end;

function TipoEscritToStr(AValue: TACBrTipoEscrit): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToTipoEscrit(const AValue: string): TACBrTipoEscrit;
begin
   Result := TACBrTipoEscrit( StrToIntDef( AValue, 0) );
end;

function IndSitEspToStr(AValue: TACBrIndSitEsp): String;
begin
   if AValue = indNenhum then
      Result := ''
   else
      Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndSitEsp(const AValue: string): TACBrIndSitEsp;
begin
   Result := TACBrIndSitEsp( StrToIntDef( AValue, 0) );
end;

function IndNatPJToStr(AValue: TACBrIndNatPJ): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndNatPJ(const AValue: string): TACBrIndNatPJ;
begin
   Result := TACBrIndNatPJ( StrToIntDef( AValue, 0) );
end;

function IndAtivToStr(AValue: TACBrIndAtiv): string;
begin
   if AValue = indAtivoOutros then
      Result := '9'
   else
      Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndAtiv(const AValue: string): TACBrIndAtiv;
begin
   if AValue = '9' then
      Result := indAtivoOutros
   else
      Result := TACBrIndAtiv( StrToIntDef( AValue, 9) );
end;

function CodIncTribToStr(AValue: TACBrCodIncTrib): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToCodIncTrib(const AValue: string): TACBrCodIncTrib;
begin
   Result := TACBrCodIncTrib( StrToIntDef( AValue, 1) -1 );
end;

function IndAproCredToStr(AValue: TACBrIndAproCred): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndAproCred(const AValue: string): TACBrIndAproCred;
begin
   Result := TACBrIndAproCred( StrToIntDef( AValue, 1) -1 );
end;

function CodTipoContToStr(AValue: TACBrCodTipoCont): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToCodTipoCont(const AValue: string): TACBrCodTipoCont;
begin
   Result := TACBrCodTipoCont( StrToIntDef( AValue, 0) -1 );
end;

function IndRegCumToStr(AValue: TACBrIndRegCum): string;
begin
   if AValue = codRegimeCompetEscritDetalhada then
      Result := '9'
   else
      Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndRegCum(const AValue: string): TACBrIndRegCum;
begin
   if AValue = '9' then
      Result := codRegimeCompetEscritDetalhada
   else
      Result := TACBrIndRegCum( StrToIntDef( AValue, 1) -1 );
end;

function TipoItemToStr(AValue: TACBrTipoItem): string;
begin
   if AValue = tiOutras then
      Result := '99'
   else
      Result := FormatFloat('00', Integer( AValue ));
end;

function StrToTipoItem(const AValue: string): TACBrTipoItem;
begin
   if AValue = '99' then
      Result := tiOutras
   else
      Result := TACBrTipoItem( StrToIntDef( AValue, 0) );
end;

function IndOperToStr(AVAlue: TACBrIndOper): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndOper(const AVAlue: string): TACBrIndOper;
begin
   Result := TACBrIndOper( StrToIntDef( AValue, 0) );
end;

function IndEmitToStr(AValue: TACBrIndEmit): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndEmit(const AValue: string): TACBrIndEmit;
begin
   Result := TACBrIndEmit( StrToIntDef( AValue, 0) );
end;

function CodSitToStr(AValue: TACBrCodSit): string;
begin
   Result := FormatFloat('00', Integer( AValue ) );
end;

function StrToCodSit(const AValue: string): TACBrCodSit;
begin
   Result := TACBrCodSit( StrToIntDef( AValue, 0) );
end;

function CodSitFToStr(AValue: TACBrCodSitF): string;
begin
  if AValue = csffRegular then
    Result := '00'
  else
  if AValue = csfCancelado then
    Result := '02'
  else
  if AValue = csfOutros then
    Result := '99'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrCodSitF');
end;

function StrToCodSitF(const AValue: string): TACBrCodSitF;
begin
   if AValue = '00' then
      Result := csffRegular
   else
   if AValue = '02' then
      Result := csfCancelado
   else
   if AValue = '99' then
      Result := csfOutros
    else
     raise Exception.Create(format('Valor informado [%s] deve estar entre (00,02 e 99)',[AValue]));
end;

function IndPgtoToStr(AValue: TACBrIndPgto): string;
begin
{ Indicador do tipo de pagamento:
0- À vista;
1- A prazo;
9- Sem pagamento.

Obs.: A partir de 01/07/2012 passará a ser:
Indicador do tipo de pagamento:
0- À vista;
1- A prazo;
2 - Outros
}
   if AValue = tpSemPagamento then
      Result := '9'
   else
   if AValue = tpNenhum then
      Result := ''
   else
      Result := IntToStr( Integer( AValue ) );
end;

function StrToIndPgto(const AValue: string): TACBrIndPgto;
begin
   if AValue = '9' then
      Result := tpSemPagamento
   else
   if AValue = ''  then
      Result := tpNenhum
   else
      Result := TACBrIndPgto( StrToIntDef( AValue, 9) );
end;

function NatBcCredToStr(AValue: TACBrNatBcCred): string;
begin
   if AValue = bccVazio then
      Result := ''
   else
      Result := FormatFloat('00', Integer( AValue ) );
end;

function StrToNatBcCred(const AValue: string): TACBrNatBcCred;
begin
   if AValue = '' then
      Result := bccVazio
   else
      Result := TACBrNatBcCred( StrToIntDef( AValue, 1) );
end;

function NaturezaContaToStr(AValue: TACBrNaturezaConta): string;
begin
  case AValue of
    ncgAtivo: Result := '01';
    ncgPassivo: Result := '02';
    ncgLiquido: Result := '03';
    ncgResultado: Result := '04';
    ncgCompensacao: Result := '05';
    ncgOutras: Result := '09';
  end;
end;

function IndTpOperacaoReceitaToStr(AValue: TACBrIndTpOperacaoReceita): string;
begin
  Result := IntToStr(Integer(AValue));
end;

function OrigemProcessoToStr(AValue: TACBrOrigemProcesso): string;
begin
  if (AValue = opJusticaFederal) then
    Result := '1'
  else if (AValue = opSecexRFB) then
    Result := '3'
  else if (AValue = opOutros) then
    Result := '9'
  else
    Result := '';
end;

function LocalExecServicoToStr(const AValue: TACBrLocalExecServico): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function IndEscrituracaoToStr(const AValue: TACBrIndEscrituracao): string;
begin
  if AValue = IndEscriConsolidado then
    Result := '1'
  else
    Result := '2';
end;

function DoctoImportaToStr(const AValue: TACBrDoctoImporta): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function ApuracaoIPIToStr(const AValue: TACBrApuracaoIPI): string;
begin
  if (AValue = iaVazio) then
    Result := EmptyStr
  else
    Result := IntToStr(Ord(AValue));
end;

function IndMovToStr(const AValue: TACBrIndMov): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function EmitenteToStr(const AValue: TACBrEmitente): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function IndCredOriToStr(const AValue: TACBrIndCredOri): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function IndDescCredToStr(const AValue: TACBrIndDescCred): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function IndAJToStr(const AValue: TACBrIndAJ): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function CodContToStr(const AValue: TACBrCodCont): string;
begin
  if AValue = ccNaoAcumAliqBasica then
    Result := '01'
  else if AValue = ccNaoAcumAliqDiferenciada then
    Result :=  '02'
  else if AValue = ccNaoAcumAliqUnidProduto then
    Result := '03'
  else if AValue = ccNaoAcumAliqBasicaAtivImobiliaria then
    Result := '04'
  else if AValue = ccApuradaPorST then
    Result := '31'
  else if AValue = ccApuradaPorSTManaus then
    Result := '32'
  else if AValue = ccAcumAliqBasica then
    Result := '51'
  else if AValue = ccAcumAliqDiferenciada then
    Result := '52'
  else if AValue = ccAcumAliqUnidProduto then
    Result := '53'
  else if AValue = ccAcumAliqBasicaAtivImobiliaria then
    Result := '54'
  else if AValue = ccApuradaAtivImobiliaria then
    Result := '70'
  else if AValue = ccApuradaSCPNaoCumulativa then
    Result :=  '71'
  else if AValue = ccApuradaSCPCumulativa then
    Result := '72'
  else if AValue = ccPISPasepSalarios then
    Result := '99'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrCodCont');
end;

function IndTipCoopToStr(const AValue: TACBrIndTipCoop): string;
begin
  if AValue = itcProdAgropecuaria then
    Result := '01'
  else if AValue = itcConsumo then
    Result :=  '02'
  else if AValue = itcCredito then
    Result := '03'
  else if AValue = itcEletRural then
    Result := '04'
  else if AValue = itcTransCargas then
    Result := '05'
  else if AValue = itcMedicos then
    Result := '06'
  else if AValue = itcOutras then
    Result := '99'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrIndTipCoop');
end;

function CodAjToStr(const AValue: TACBrCodAj): string;
begin
  if AValue = codAjAcaoJudicial then
    Result := '01'
  else if AValue = codAjProAdministrativo then
    Result :=  '02'
  else if AValue = codAjLegTributaria then
    Result := '03'
  else if AValue = codAjEspRTI then
    Result := '04'
  else if AValue = codAjOutrasSituacaoes then
    Result := '05'
  else if AValue = codAjEstorno then
    Result := '06'
  else if AValue = codAjCPRBAdocaoRegCaixa then
    Result := '07'
  else if AValue = codAjCPRBDiferValRecPer then
    Result := '08'
  else if AValue = codAjCPRBAdicValDifPerAnt then
    Result := '09'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrCodAj');
end;

function NatCredDescToStr(const AValue: TACBrNatCredDesc): string;
begin
  if AValue = ncdAliqBasica then
    Result := '01'
  else if AValue = ncdAliqDiferenciada then
    Result := '02'
  else if AValue = ncdAliqUnidProduto then
    Result := '03'
  else if AValue = ncdPresAgroindustria then
    Result := '04'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrNatCredDesc');
end;

function TabCodAjBaseCalcToStr(const AValue: TACBrTabCodAjBaseCalcContrib): string;
begin
  if AValue = tcaVendasCanceladas then
    Result := '01'
  else if AValue = tcaDevolucoesVendas then
    Result := '02'
  else if AValue = tcaICMSaRecolher then
    Result := '21'
  else if AValue = tcaOutrVlrsDecJudicial then
    Result := '41'
  else if AValue = tcaOutrVlrsSemDecJudicial then
    Result := '42'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrTabCodAjBaseCalcContrib');
end;


function IndicadorApropAjusteToStr(const AValue: TACBrIndicadorApropAjuste): string;
begin
  if AValue = iaaRefPisCofins then
    Result := '01'
  else if AValue = iaaUnicaPISPasep then
    Result := '02'
  else if AValue = iaaRefUnicaCofins then
    Result := '03'
  else
    raise Exception.Create('Valor informado inválido para ser convertido em TACBrIndicadorApropAjuste');
end;


function NatFrtContratadoToStr(AValue: TACBrNaturezaFrtContratado): string;
begin
  if (AValue = nfcOutras) then
    Result := '9'
  else
    Result := IntToStr(Integer(AValue));
end;

function IndOrigCredToStr(AValue: TACBrIndOrigCred): string;
begin
   if AValue = opcVazio then
      Result := ''
   else
      Result := IntToStr( Integer( AValue ) );
end;

function StrToIndOrigCred(const AValue: String): TACBrIndOrigCred;
begin
   if AValue = '' then
      Result := opcVazio
   else
      Result := TACBrIndOrigCred( StrToIntDef( AValue, 0) );
end;

function IdentBemImobToStr(AValue: TACBrIdentBemImob): string;
begin
  case Integer( AValue ) of
    Integer(iocEdificacoesBenfeitorias) : Result := '01';   // '01' // Edificações e Benfeitorias
    Integer(iocInstalacoes)             : Result := '03';   // '03' // Instalações
    Integer(iocMaquinas)                : Result := '04';   // '04' // Máquinas
    Integer(iocEquipamentos)            : Result := '05';   // '05' // Equipamentos
    Integer(iocVeiculos)                : Result := '06';   // '06' // Veículos
    Integer(iocOutros)                  : Result := '99';   // '99' // Outros bens incorporados ao Ativo Imobilizado
  else
    Result := '';
  end;
end;

function StrToIdentBemImob(const AValue: String): TACBrIdentBemImob;
begin
  case StrToIntDef( AValue, 0 ) of
    1: Result := iocEdificacoesBenfeitorias;   // '01' // Edificações e Benfeitorias
    3: Result := iocInstalacoes;               // '03' // Instalações
    4: Result := iocMaquinas;                  // '04' // Máquinas
    5: Result := iocEquipamentos;              // '05' // Equipamentos
    6: Result := iocVeiculos;                  // '06' // Veículos
  else
    Result := iocOutros;                       // '99' // Outros bens incorporados ao Ativo Imobilizado
  end;
end;

function IndUtilBemImobToStr(AValue: TACBrIndUtilBemImob): string;
begin
  case Integer( AValue ) of
    Integer(ubiProducaoBensDestinadosVenda) : Result := '1';   // '1' // Produção de Bens Destinados a Venda
    Integer(ubiPrestacaoServicos)           : Result := '2';   // '2' // Prestação de Serviços
    Integer(ubiLocacaoTerceiros)            : Result := '3';   // '3' // Locação a Terceiros
    Integer(ubiOutros)                      : Result := '9';   // '9' // Outros
  else
    Result := '';
  end;
end;

function StrToIndUtilBemImob(const AValue: String): TACBrIndUtilBemImob;
begin
  case StrToIntDef( AValue, 0) of
    1: Result := ubiProducaoBensDestinadosVenda;   // '1' // Produção de Bens Destinados a Venda
    2: Result := ubiPrestacaoServicos;             // '2' // Prestação de Serviços
    3: Result := ubiLocacaoTerceiros;              // '3' // Locação a Terceiros
  else
    Result := ubiOutros;                           // '9' // Outros
  end;
end;

function IndNrParcToStr(AValue: TACBrIndNrParc): string;
begin
  case Integer( AValue ) of
    Integer(inrIntegral): Result := '1';           // '1' // Integral (Mês de Aquisição)
    Integer(inr12Meses) : Result := '2';           // '2' // 12 Meses
    Integer(inr24Meses) : Result := '3';           // '3' // 24 Meses
    Integer(inr48Meses) : Result := '4';           // '4' // 48 Meses
    Integer(inr6Meses)  : Result := '5';           // '5' // 6 Meses (Embalagens de bebidas frias)
    Integer(inrOutra)   : Result := '9';           // '9' // Outra periodicidade definida em Lei
  else
    Result := '';
  end;
end;

function StrToIndNrParc(const AValue: String): TACBrIndNrParc;
begin
  case StrToIntDef( AValue, 0) of
    1: Result := inrIntegral;      // '1' // Integral (Mês de Aquisição)
    2: Result := inr12Meses;       // '2' // 12 Meses
    3: Result := inr24Meses;       // '3' // 24 Meses
    4: Result := inr48Meses;       // '4' // 48 Meses
    5: Result := inr6Meses;        // '5' // 6 Meses (Embalagens de bebidas frias)
  else
    Result := inrOutra;            // '9' // Outra periodicidade definida em Lei
  end;
end;

function CstPisToStr(AValue: TACBrCstPis): string;
begin
   Result := CstPis[ Integer( AValue ) ];
end;

function StrToCstPis(const AValue: String): TACBrCstPis;
var
   ifor: Integer;
begin
 Result := stpisNenhum;
   for ifor := 0 to High(CstPis) do
   begin
      if AValue = CstPis[ifor] then
      begin
         Result := TACBrCstPis( ifor );
         Break;
      end;
   end;
end;

function CstPisCofinsToStr(AValue: TACBrCstPisCofins): string;
begin
  Result := CstPisCofins[ Integer( AValue ) ];
end;

function StrToCstPisCofins(const AValue: String): TACBrCstPisCofins;
var
   ifor: Integer;
begin
 Result := stpiscofinsNenhum;
   for ifor := 0 to High(CstPisCofins) do
   begin
      if AValue = CstPisCofins[ifor] then
      begin
         Result := TACBrCstPisCofins( ifor );
         Break;
      end;
   end;
end;

function CstCofinsToStr(AValue: TACBrCstCofins): string;
begin
   Result := CstCofins[ Integer( AValue ) ];
end;

function StrToCstCofins(const AValue: String): TACBrCstCofins;
var
ifor: Integer;
begin
 Result := stcofinsNenhum;
   for ifor := 0 to High(CstCofins) do
   begin
      if AValue = CstCofins[ifor] then
      begin
         Result := TACBrCstCofins( ifor );
         Break;
      end;
   end;
end;

function CstIcmsToStr(AValue: TACBrCstIcms): string;
begin
   Result := CstIcms[ Integer( AValue ) ];
end;

function StrToCstIcms(const AValue: String): TACBrCstIcms;
var
ifor: Integer;
begin
   Result := sticmsNenhum;
   for ifor := 0 to High(CstIcms) do
   begin
      if AValue = CstIcms[ifor] then
      begin
         Result := TACBrCstIcms( ifor );
         Break;
      end;
   end;
end;

function CstIpiToStr(AValue: TACBrCstIpi): string;
begin
   Result := CstIpi[ Integer( AValue ) ];
end;

function StrToCstIpi(const AValue: String): TACBrCstIpi;
var
ifor: Integer;
begin
 Result := stipiVazio;
   for ifor := 0 to High(CstIpi) do
   begin
      if AValue = CstIpi[ifor] then
      begin
         Result := TACBrCstIpi( ifor );
         Break;
      end;
   end;
end;

function IndTipoOperToStr(AValue: TACBrIndTipoOper): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndTipoOper(const AValue: string): TACBrIndTipoOper;
begin
   Result := TACBrIndTipoOper( StrToIntDef( AValue, 0) );
end;

function IndCTAToStr(AValue: TACBrIndCTA): string;
begin
  case AValue of
    indCTASintetica: Result := 'S';
    indCTAnalitica: Result := 'A';
  end;
end;

function IndFrtToStr(AValue: TACBrIndFrt): string;
begin
{
  TACBrIndFrt = (
                 tfPorContaEmitente,     // 0 - Por conta de terceiros
                 tfPorContaDestinatario, // 1 - Por conta do emitente
                 tfPorContaTerceiros,    // 2 - Por conta do destinatário
                 tfSemCobrancaFrete,     // 9 - Sem cobrança de frete
                 tfNenhum
Indicador do tipo do frete:
0- Por conta de terceiros;
1- Por conta do emitente;
2- Por conta do destinatário;
9- Sem cobrança de frete.
17  IND_FRT
Obs.: A partir de 01/01/2012 passará a ser:
Indicador do tipo do frete:
0- Por conta do emitente;
1- Por conta do destinatário/remetente;
2- Por conta de terceiros;
9- Sem cobrança de frete.

   if DT_INI >= EncodeDate(2012,01,01) then
   begin
}
      if AValue = tfSemCobrancaFrete then
      begin
         Result := '9';
         Exit;
      end
      else
      if AValue = tfNenhum then
      begin
         Result := '';
         Exit;
      end;
      Result := IntToStr( Integer( AValue ) );
//   end;
end;

function StrToIndFrt(const AValue: string): TACBrIndFrt;
begin
   if AValue = '9' then
   begin
      Result := tfSemCobrancaFrete;
      Exit;
   end
   else
   if AValue = '' then
   begin
      Result := tfNenhum;
      Exit;
   end;
   Result := TACBrIndFrt( StrToIntDef( AValue, 0) );
end;

function IndMovFisicaToStr(AValue: TACBrIndMovFisica): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndMovFisica(const AValue: string): TACBrIndMovFisica;
begin
   Result := TACBrIndMovFisica( StrToIntDef( AValue, 0) );
end;

function StrToCodAjBaseCalcContrib(const AValue: string): TACBrTabCodAjBaseCalcContrib;
begin
  if AValue = '01' then
    Result := tcaVendasCanceladas
  else if AValue = '02' then
    Result :=  tcaDevolucoesVendas
  else if AValue = '21' then
    Result := tcaICMSaRecolher
  else if AValue = '41' then
    Result := tcaOutrVlrsDecJudicial
  else if AValue = '42' then
    Result := tcaOutrVlrsSemDecJudicial
  else
    raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02,21,41 e 42)', [AValue]));
end;


function StrToIndicadorApropAjuste(const AValue: string): TACBrIndicadorApropAjuste;
begin
  if AValue = '01' then
    Result := iaaRefPisCofins
  else if AValue = '02' then
    Result :=  iaaUnicaPISPasep
  else if AValue = '03' then
    Result := iaaRefUnicaCofins
  else
    raise Exception.Create(format('Valor informado [%s] deve estar entre (01,02 e 03)', [AValue]));
end;



end.
