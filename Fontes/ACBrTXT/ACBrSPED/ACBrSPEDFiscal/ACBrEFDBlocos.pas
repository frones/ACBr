{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBlocos;

interface

uses
  SysUtils, Classes, DateUtils, ACBrTXTUtils, ACBrSped, StrUtils;

type

  EACBrSPEDFiscalException = class(EACBrSPEDException);

  /// Indicador de movimento - TOpenBlocos
  TACBrIndMov = (imComDados, // 0- Bloco com dados informados;
                 imSemDados  // 1- Bloco sem dados informados.
                             );
  TACBrIndicadorMovimento = TACBrIndMov;

  /// Perfil de apresentação do arquivo fiscal - TRegistro0000
  TACBrIndPerfil             = (pfPerfilA, // A – Perfil A
                                pfPerfilB, // B – Perfil B
                                pfPerfilC, // C – Perfil C
                                pfNenhum   // Nenhum
                                );
  TACBrPerfil = TACBrIndPerfil;

  /// Indicador de tipo de atividade - TRegistro0000
  TACBrIndAtiv            = (atIndustrial, // 0 – Industrial ou equiparado a industrial
                             atOutros      // 1 – Outros.
                             );
  TACBrAtividade = TACBrIndAtiv;

  /// Versão do Leiaute do arquivo - TRegistro0000
  TACBrCodVer             = (vlVersao100,  // Código 001 - Versão 100 Ato COTEPE 01/01/2008
                             vlVersao101,  // Código 002 - Versão 101 Ato COTEPE 01/01/2009
                             vlVersao102,  // Código 003 - Versão 102 Ato COTEPE 01/01/2010
                             vlVersao103,  // Código 004 - Versão 103 Ato COTEPE 01/01/2011
                             vlVersao104,  // Código 005 - Versão 104 Ato COTEPE 01/07/2012
                             vlVersao105,  // Código 006 - Versão 105 Ato COTEPE 01/07/2012
                             vlVersao106,  // Código 007 - Versão 106 Ato COTEPE 01/07/2013
                             vlVersao107,  // Código 008 - Versão 107 Ato COTEPE 01/07/2014
                             vlVersao108,  // Código 009 - Versão 108 Ato COTEPE 01/07/2015
                             vlVersao109,  // Código 010 - Versão 109 Ato COTEPE 01/07/2016
                             vlVersao110,  // Código 011 - Versão 110 Ato COTEPE 01/01/2017
                             vlVersao111   // Código 012 - Versão 111 Ato COTEPE 01/01/2018
                             );
  TACBrVersaoLeiaute = TACBrCodVer;

  /// Código da finalidade do arquivo - TRegistro0000
  TACBrCodFin           = (raOriginal,     // 0 - Remessa do arquivo original
                           raSubstituto    // 1 - Remessa do arquivo substituto
                             );
  TACBrCodFinalidade = TACBrCodFin;

  /// Tipo do item – Atividades Industriais, Comerciais e Serviços:
  TACBrTipoItem = (tiMercadoriaRevenda,    // 00 – Mercadoria para Revenda
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
  TACBrIndOper      = (tpEntradaAquisicao, // 0 - Entrada
                       tpSaidaPrestacao    // 1 - Saída
                       );
  TACBrTipoOperacao = TACBrIndOper;

  /// Indicador do emitente do documento fiscal
  TACBrIndEmit = (edEmissaoPropria,         // 0 - Emissão própria
                  edTerceiros               // 1 - Terceiro
                  );
  TACBrEmitente = TACBrIndEmit;

  /// Indicador do tipo de pagamento
  TACBrIndPgto = (tpVista,             // 0 - À Vista
                  tpPrazo,             // 1 - A Prazo
                  tpOutros,            // 2 - Outros
                  tpSemPagamento,      // 9 - Sem pagamento
                  tpNenhum             // Preencher vazio
                  );
  TACBrTipoPagamento = TACBrIndPgto;

  /// Indicador do tipo do frete
  TACBrIndFrt = (tfPorContaEmitente,      // 0 - Por conta do emitente
                 tfPorContaDestinatario,  // 1 - Por conta do destinatário
                 tfPorContaTerceiros,     // 2 - Por conta de terceiros
                 tfSemCobrancaFrete,      // 9 - Sem cobrança de frete
                 tfNenhum                 // Preencher vazio
                 );
  TACBrTipoFrete = TACBrIndFrt;

  /// Indicador do tipo do frete da operação de redespacho
  TACBrTipoFreteRedespacho = (frSemRedespacho,         // 0 – Sem redespacho
                              frPorContaEmitente,      // 1 - Por conta do emitente
                              frPorContaDestinatario,  // 2 - Por conta do destinatário
                              frOutros,                // 9 – Outros
                              frNenhum                 // Preencher vazio
                              );
  /// Indicador da origem do processo
  TACBrOrigemProcesso = (opSefaz,            // 0 - Sefaz
                         opJusticaFederal,   // 1 - Justiça Federal
                         opJusticaEstadual,  // 2 - Justiça Estadual
                         opSecexRFB,         // 3 - Secex/RFB
                         opOutros,           // 9 - Outros
                         opNenhum           // Preencher vazio
                         );
  /// Indicador do tipo de operação
  TACBrIndOperST      = (toCombustiveisLubrificantes, // 0 - Combustíveis e Lubrificantes
                         toLeasingVeiculos            // 1 - leasing de veículos ou faturamento direto
                         );
  TACBrTipoOperacaoST = TACBrIndOperST;

  TACBrDoctoArrecada = (daEstadualArrecadacao,  // 0 - Documento Estadual de Arrecadação
                        daGNRE                  // 1 - GNRE
                        );
  /// Indicador do tipo de transporte
  TACBrTipoTransporte = (ttRodoviario,         // 0 – Rodoviário
                         ttFerroviario,        // 1 – Ferroviário
                         ttRodoFerroviario,    // 2 – Rodo-Ferroviário
                         ttAquaviario,         // 3 – Aquaviário
                         ttDutoviario,         // 4 – Dutoviário
                         ttAereo,              // 5 – Aéreo
                         ttOutros              // 9 – Outros
                         );
  /// Documento de importação
  TACBrDoctoImporta = (diImportacao,           // 0 – Declaração de Importação
                       diSimplificadaImport    // 1 – Declaração Simplificada de Importação
                       );
  /// Indicador do tipo de título de crédito
  TACBrTipoTitulo = (tcDuplicata,             // 00- Duplicata
                     tcCheque,                // 01- Cheque
                     tcPromissoria,           // 02- Promissória
                     tcRecibo,                // 03- Recibo
                     tcOutros                 // 99- Outros (descrever)
                     );

  /// Movimentação física do ITEM/PRODUTO:
  TACBrIndMovFisica = (mfSim,           // 0 - Sim
                       mfNao            // 1 - Não
                      );
  TACBrMovimentacaoFisica = TACBrIndMovFisica;

  /// Indicador de período de apuração do IPI
  TACBrApuracaoIPI = (iaMensal,               // 0 - Mensal
                      iaDecendial,             // 1 - Decendial
                      iaNenhum                // Vazio
                      );
  /// Indicador de tipo de referência da base de cálculo do ICMS (ST) do produto farmacêutico
  TACBrTipoBaseMedicamento = (bmCalcTabeladoSugerido,           // 0 - Base de cálculo referente ao preço tabelado ou preço máximo sugerido;
                              bmCalMargemAgregado,              // 1 - Base cálculo – Margem de valor agregado;
                              bmCalListNegativa,                // 2 - Base de cálculo referente à Lista Negativa;
                              bmCalListaPositiva,               // 3 - Base de cálculo referente à Lista Positiva;
                              bmCalListNeutra                   // 4 - Base de cálculo referente à Lista Neutra
                              );
  /// Tipo Produto
  TACBrTipoProduto = (tpSimilar,   // 0 - Similar
                      tpGenerico,  // 1 - Genérico
                      tpMarca      // 2 - Ético ou de Marca
                      );
  /// Indicador do tipo da arma de fogo
  TACBrTipoArmaFogo = (tafPermitido,     // 0 - Permitido
                       tafRestrito       // 1 - Restrito
                       );
  /// Indicador do tipo de operação com veículo
  TACBrIndVeicOper         = (tovVendaPConcess,   // 0 - Venda para concessionária
                              tovFaturaDireta,    // 1 - Faturamento direto
                              tovVendaDireta,     // 2 - Venda direta
                              tovVendaDConcess,   // 3 - Venda da concessionária
                              tovVendaOutros      // 9 - Outros
                              );
  TACBrTipoOperacaoVeiculo = TACBrIndVeicOper;

  /// Indicador do tipo de receita
  TACBrIndRec = (trPropria,   // 0 - Receita própria
                 trTerceiro   // 1 - Receita de terceiros
                );
  TACBrTipoReceita = TACBrIndRec;

  /// Indicador do tipo do veículo transportador
  TACBrTipoVeiculo = (tvEmbarcacao,
                      tvEmpuradorRebocador
                      );
  /// Indicador do tipo da navegação
  TACBrTipoNavegacao = (tnInterior,
                        tnCabotagem
                        );
  /// Situação do Documento
  TACBrCodSit = (sdRegular,                 // 00 - Documento regular
                 sdExtempRegular,           // 01 - Escrituração extemporânea de documento regular
                 sdCancelado,               // 02 - Documento cancelado
                 sdCanceladoExtemp,         // 03 - Escrituração extemporânea de documento cancelado
                 sdDoctoDenegado,           // 04 - NF-e ou CT-e - denegado
                 sdDoctoNumInutilizada,     // 05 - NF-e ou CT-e - Numeração inutilizada
                 sdFiscalCompl,             // 06 - Documento Fiscal Complementar
                 sdExtempCompl,             // 07 - Escrituração extemporânea de documento complementar
                 sdRegimeEspecNEsp          // 08 - Documento Fiscal emitido com base em Regime Especial ou Norma Específica
                 );
  TACBrSituacaoDocto = TACBrCodSit;

  /// Indicador do tipo de tarifa aplicada:
  TACBrTipoTarifa = (tipExp,     // 0 - Exp
                     tipEnc,     // 1 - Enc
                     tipCI,      // 2 - CI
                     tipOutra    // 9 - Outra
                     );
  /// Indicador da natureza do frete
  TACBrNaturezaFrete = (nfNegociavel,      // 0 - Negociavel
                        nfNaoNegociavel    // 1 - Não Negociavel
                        );
  /// Indicador do tipo de receita
  TACBrIndReceita = (recServicoPrestado,          // 0 - Receita própria - serviços prestados;
                     recCobrancaDebitos,          // 1 - Receita própria - cobrança de débitos;
                     recVendaMerc,                // 2 - Receita própria - venda de mercadorias;
                     recServicoPrePago,           // 3 - Receita própria - venda de serviço pré-pago;
                     recOutrasProprias,           // 4 - Outras receitas próprias;
                     recTerceiroCoFaturamento,    // 5 - Receitas de terceiros (co-faturamento);
                     recTerceiroOutras            // 9 - Outras receitas de terceiros
                     );
  TACBrIndTipoReceita = TACBrIndReceita;

  /// Indicador do tipo de serviço prestado
  TACBrServicoPrestado = (spTelefonia,                // 0- Telefonia;
                          spComunicacaoDados,         // 1- Comunicação de dados;
                          spTVAssinatura,             // 2- TV por assinatura;
                          spAcessoInternet,           // 3- Provimento de acesso à Internet;
                          spMultimidia,               // 4- Multimídia;
                          spOutros                    // 9- Outros
                          );
  /// Indicador de movimento
  TACBrMovimentoST = (mstSemOperacaoST,   // 0 - Sem operações com ST
                      mstComOperacaoST    // 1 - Com operações de ST
                      );

  /// Indicador do tipo de ajuste
  TACBrTipoAjuste = (ajDebito,            // 0 - Ajuste a débito;
                     ajCredito            // 1- Ajuste a crédito
                     );
  /// Indicador da origem do documento vinculado ao ajuste
  TACBrOrigemDocto = (odPorcessoJudicial, // 0 - Processo Judicial;
                      odProcessoAdminist, // 1 - Processo Administrativo;
                      odPerDcomp,         // 2 - PER/DCOMP;
                      odOutros            //9 – Outros.
                      );
  /// Indicador de propriedade/posse do item
  TACBrIndProp = (piInformante,           // 0- Item de propriedade do informante e em seu poder;
                  piInformanteNoTerceiro, // 1- Item de propriedade do informante em posse de terceiros;
                  piTerceiroNoInformante  // 2- Item de propriedade de terceiros em posse do informante
                 );
  TACBrPosseItem = TACBrIndProp;
  /// Informe o tipo de documento
  TACBrTipoDocto = (docDeclaracaoExportacao,           // 0 - Declaração de Exportação;
                    docDeclaracaoSimplesExportacao     // 1 - Declaração Simplificada de Exportação.
                    );
  /// Preencher com
  TACBrExportacao = (exDireta,             // 0 - Exportação Direta
                     exIndireta            // 1 - Exportação Indireta
                     );
  /// Indicador Tipo de Estoque K200
  TACBrIndEstoque = ( estPropInformantePoder, // 0 = Estoque de propriedade do informante e em seu poder
                      estPropInformanteTerceiros,// 1 = Estoque de propriedade do informante e em posse de terceiros;
                      estPropTerceirosInformante // 2 = Estoque de propriedade de terceiros e em posse do informante
                    );
  /// Informação do tipo de conhecimento de embarque
  TACBrConhecEmbarque = (ceAWB,            //01 – AWB;
                         ceMAWB,           //02 – MAWB;
                         ceHAWB,           //03 – HAWB;
                         ceCOMAT,          //04 – COMAT;
                         ceRExpressas,     //06 – R. EXPRESSAS;
                         ceEtiqREspressas, //07 – ETIQ. REXPRESSAS;
                         ceHrExpressas,    //08 – HR. EXPRESSAS;
                         ceAV7,            //09 – AV7;
                         ceBL,             //10 – BL;
                         ceMBL,            //11 – MBL;
                         ceHBL,            //12 – HBL;
                         ceCTR,            //13 – CRT;
                         ceDSIC,           //14 – DSIC;
                         ceComatBL,        //16 – COMAT BL;
                         ceRWB,            //17 – RWB;
                         ceHRWB,           //18 – HRWB;
                         ceTifDta,         //19 – TIF/DTA;
                         ceCP2,            //20 – CP2;
                         ceNaoIATA,        //91 – NÂO IATA;
                         ceMNaoIATA,       //92 – MNAO IATA;
                         ceHNaoIATA,       //93 – HNAO IATA;
                         ceCOutros         //99 – OUTROS.
                         );
  /// Identificador de medição
  TACBrMedicao = (medAnalogico,            // 0 - analógico;
                  medDigital               // 1 – digital
                  );
  /// Tipo de movimentação do bem ou componente
  TACBrMovimentoBens = (mbcSI,             // SI = Saldo inicial de bens imobilizados
                        mbcIM,             // IM = Imobilização de bem individual
                        mbcIA,             // IA = Imobilização em Andamento - Componente
                        mbcCI,             // CI = Conclusão de Imobilização em Andamento – Bem Resultante
                        mbcMC,             // MC = Imobilização oriunda do Ativo Circulante
                        mbcBA,             // BA = Baixa do Saldo de ICMS - Fim do período de apropriação
                        mbcAT,             // AT = Alienação ou Transferência
                        mbcPE,             // PE = Perecimento, Extravio ou Deterioração
                        mbcOT              // OT = Outras Saídas do Imobilizado
                        );
  /// Código de grupo de tensão
  TACBrGrupoTensao = (gtNenhum,      // '' - Vazio. Para uso quando o documento for cancelado.
                      gtA1,          // 01 - A1 - Alta Tensão (230kV ou mais)
                      gtA2,          // 02 - A2 - Alta Tensão (88 a 138kV)
                      gtA3,          // 03 - A3 - Alta Tensão (69kV)
                      gtA3a,         // 04 - A3a - Alta Tensão (30kV a 44kV)
                      gtA4,          // 05 - A4 - Alta Tensão (2,3kV a 25kV)
                      gtAS,          // 06 - AS - Alta Tensão Subterrâneo 06
                      gtB107,        // 07 - B1 - Residencial 07
                      gtB108,        // 08 - B1 - Residencial Baixa Renda 08
                      gtB209,        // 09 - B2 - Rural 09
                      gtB2Rural,     // 10 - B2 - Cooperativa de Eletrificação Rural
                      gtB2Irrigacao, // 11 - B2 - Serviço Público de Irrigação
                      gtB3,          // 12 - B3 - Demais Classes
                      gtB4a,         // 13 - B4a - Iluminação Pública - rede de distribuição
                      gtB4b          // 14 - B4b - Iluminação Pública - bulbo de lâmpada
                      );
  /// Código de classe de consumo de energia elétrica ou gás
  TACBrClasseConsumo = (ccComercial,         // 01 - Comercial
                        ccConsumoProprio,    // 02 - Consumo Próprio
                        ccIluminacaoPublica, // 03 - Iluminação Pública
                        ccIndustrial,        // 04 - Industrial
                        ccPoderPublico,      // 05 - Poder Público
                        ccResidencial,       // 06 - Residencial
                        ccRural,             // 07 - Rural
                        ccServicoPublico     // 08 -Serviço Público
                        );
  /// Código de tipo de Ligação
  TACBrTpLigacao = (tlNenhum,              // '' - Para uso quando o documento for cancelado
                    tlMonofasico,          // 1 - Monofásico
                    tlBifasico,            // 2 - Bifásico
                    tlTrifasico            // 3 - Trifásico
                    );
  TACBrTipoLigacao = TACBrTpLigacao;

  /// Código dispositivo autorizado
  TACBrDispositivo = (cdaFormSeguranca,  // 00 - Formulário de Segurança
                      cdaFSDA,           // 01 - FS-DA – Formulário de Segurança para Impressão de DANFE
                      cdaNFe,            // 02 – Formulário de segurança - NF-e
                      cdaFormContinuo,   // 03 - Formulário Contínuo
                      cdaBlocos,         // 04 – Blocos
                      cdaJogosSoltos     // 05 - Jogos Soltos
                      );
  /// Código do Tipo de Assinante
  TACBrTpAssinante = (assComercialIndustrial,    // 1 - Comercial/Industrial
                      assPodrPublico,            // 2 - Poder Público
                      assResidencial,            // 3 - Residencial/Pessoa física
                      assPublico,                // 4 - Público
                      assSemiPublico,            // 5 - Semi-Público
                      assOutros,                 // 6 - Outros
                      assNenhum                  // Preencher vazio
                      );
  TACBrTipoAssinante = TACBrTpAssinante;

  /// Motivo do Inventário
  TACBrMotInv = (miFinalPeriodo,
                 miMudancaTributacao,
                 miBaixaCadastral,
                 miRegimePagamento,
                 miDeterminacaoFiscos
                );
  TACBrMotivoInventario = TACBrMotInv;

  ///Código da Situação Tributária referente ao ICMS.
  TACBrCstIcms = ( sticmsNenhum                                              ,
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
                   sticmsSimplesNacionalOutros                                 // '900' // Simples Nacional - Outros
                );
  TACBrSituacaoTribICMS = TACBrCstIcms;

  /// Indicador de movimento ICMS Diferencial Alíquota
  TACBrMovimentoDIFAL = (mDifalSemOperacaoICMS,   // 0 - Sem operações com ICMS Diferencial de Alíquota da UF
                      mDifalComOperacaoICMS    // 1 - Com operações com ICMS Diferencial de Alíquota da UF
                      );

  TACBrNaturezaConta = (
                         ncgAtivo,        // 01 - Contas de ativo
                         ncgPassivo,      // 02 - Contas de passivo
                         ncgLiquido,      // 03 - Patrimônio líquido
                         ncgResultado,    // 04 - Contas de resultado
                         ncgCompensacao,  // 05 - Contas de compensação
                         ncgOutras        // 09 - Outras
                        );

  //Indicador do tipo de conta (0500)
  TACBrIndCTA = (
                   indCTASintetica,  //S Sintética
                   indCTAnalitica    //A Analitica
                );

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

  TOpenBlocos = class
  private
    FIND_MOV: TACBrIndMov;    /// Indicador de movimento: 0- Bloco com dados informados, 1- Bloco sem dados informados.
  public
    property IND_MOV: TACBrIndMov read FIND_MOV write FIND_MOV;
  end;

  // Fuções do ACBrEFDBlocos.
  function StrToCodVer(const AValue: string): TACBrCodVer;
  function CodVerToStr(AValue: TACBrCodVer): string;

  function IndOperToStr(AVAlue: TACBrIndOper): string;
  function StrToIndOper(const AValue: string): TACBrIndOper;
  function TipoItemToStr(AValue: TACBrTipoItem): string;
  function StrToTipoItem(const AValue: string): TACBrTipoItem;
  function IndEmitToStr(AValue: TACBrIndEmit): string;
  function StrToIndEmit(const AValue: string): TACBrIndEmit;
  function CodSitToStr(AValue: TACBrCodSit): string;
  function StrToCodSit(const AValue: string): TACBrCodSit;
  function IndPgtoToStr(AValue: TACBrIndPgto): string;
  function StrToIndPgto(const AValue: string): TACBrIndPgto;
  function IndFrtToStr(AValue: TACBrIndFrt): string;
  function StrToIndFrt(const AValue: string): TACBrIndFrt;
  function IndMovFisicaToStr(AValue: TACBrIndMovFisica): string;
  function StrToIndMovFisica(const AValue: string): TACBrIndMovFisica;
  function StrToMotInv(const AValue: string): TACBrMotInv;
  function MotInvToStr(AValue: TACBrMotInv): string;
  function IndPropToStr(AValue: TACBrIndProp): string;
  function StrToIndProp(const AValue: string): TACBrIndProp;
  function TpLigacaoToStr(AValue: TACBrTpLigacao): string;
  function StrToTpLigacao(const AValue: string): TACBrTpLigacao;
  function GrupoTensaoToStr(AValue: TACBrGrupoTensao): string;
  function StrToGrupoTensao(const AValue: string): TACBrGrupoTensao;
  function IndRecToStr(AValue: TACBrIndRec): string;
  function StrToIndRec(const AValue: string): TACBrIndRec;
  function TpAssinanteToStr(AValue: TACBrTpAssinante): string;
  function StrToTpAssinante(const AValue: string): TACBrTpAssinante;
  function IndReceitaToStr(AValue: TACBrIndReceita): string;
  function StrToIndReceita(const AValue: string): TACBrIndReceita;

  // Rodrigo Buschmann | Digibyte - 04/07/2017
  function CodFinToStr(AValue: TACBrCodFin): string;
  function StrToCodFin(const AValue: string): TACBrCodFin;
  function IndPerfilToStr(AValue: TACBrIndPerfil): string;
  function StrToIndPerfil(const AValue: string): TACBrIndPerfil;
  function IndAtivToStr(AValue: TACBrIndAtiv): string;
  function StrToIndAtiv(const AValue: string): TACBrIndAtiv;
  function IndMovToStr(const AValue: TACBrIndMov): string;
  function StrToIndMov(const AValue: string): TACBrIndMov;
  function NaturezaContaToStr(AValue: TACBrNaturezaConta): string;
  function StrToNaturezaConta(const AValue: string): TACBrNaturezaConta;
  function IndCTAToStr(AValue: TACBrIndCTA): string;
  function StrToIndCTA(const AValue: string): TACBrIndCTA;
  function IndTipoOperToStr(AVAlue: TACBrIndOper): string;
  function StrToIndTipoOper(const AValue: string): TACBrIndOper;
  function EmitenteToStr(const AValue: TACBrEmitente): string;
  function StrToEmitente(const AValue: string): TACBrEmitente;
  function OrigemProcessoToStr(AValue: TACBrOrigemProcesso): string;
  function StrToOrigemProcesso(const AValue: string): TACBrOrigemProcesso;
  function DoctoImportaToStr(const AValue: TACBrDoctoImporta): string;
  function StrToDoctoImporta(const AValue: string): TACBrDoctoImporta;
  function CstPisToStr(AValue: TACBrCstPis): string;
  function StrToCstPis(const AValue: string): TACBrCstPis;
  function CstPisCofinsToStr(AValue: TACBrCstPisCofins): string;
  function StrToCstPisCofins(const AValue: string): TACBrCstPisCofins;
  function CstCofinsToStr(AValue: TACBrCstCofins): string;
  function StrToCstCofins(const AValue: string): TACBrCstCofins;
  function CstIcmsToStr(AValue: TACBrCstIcms): string;
  function StrToCstIcms(const AValue: string): TACBrCstIcms;
  function CstIpiToStr(AValue: TACBrCstIpi): string;
  function StrToCstIpi(const AValue: string): TACBrCstIpi;
  function ApuracaoIPIToStr(const AValue: TACBrApuracaoIPI): string;
  function StrToApuracaoIPI(const AValue: string): TACBrApuracaoIPI;
  function MovimentoStToStr(AValue: TACBrMovimentoST): string;
  function StrToMovimentoSt(const AValue: string): TACBrMovimentoST;
  function TipoAjusteToStr(AValue: TACBrIndRec): string;
  function StrToTipoAjuste(const AValue: string): TACBrTipoAjuste;
  function OrigemDoctoToStr(AValue: TACBrOrigemDocto): string;
  function StrToOrigemDocto(const AValue: string): TACBrOrigemDocto;

implementation

{ TOpenBlocos }

function StrToCodVer(const AValue: string): TACBrCodVer;
begin
   if AValue = '001' then
      Result := vlVersao100
   else
   if AValue = '002' then
      Result := vlVersao101
   else
   if AValue = '003' then
      Result := vlVersao102
   else
   if AValue = '004' then
      Result := vlVersao103
   else
   if AValue = '005' then
      Result := vlVersao104
   else
   if AValue = '006' then
      Result := vlVersao105
   else
   if AValue = '007' then
      Result := vlVersao106
   else
   if AValue = '008' then
      Result := vlVersao107
   else
   if AValue = '009' then
      Result := vlVersao108
   else
   if AValue = '010' then
      Result := vlVersao109
   else
   if AValue = '011' then
      Result := vlVersao110
   else
   if AValue = '012' then
      Result := vlVersao111
   else
     raise EACBrSPEDFiscalException.CreateFmt('Versão desconhecida. Versao "%s" não é um valor válido.', [AValue]);
end;

function CodVerToStr(AValue: TACBrCodVer): string;
begin
  case AValue of
    vlVersao100:
      Result := '001';
    vlVersao101:
      Result := '002';
    vlVersao102:
      Result := '003';
    vlVersao103:
      Result := '004';
    vlVersao104:
      Result := '005';
    vlVersao105:
      Result := '006';
    vlVersao106:
      Result := '007';
    vlVersao107:
      Result := '008';
    vlVersao108:
      Result := '009';
    vlVersao109:
      Result := '010';
    vlVersao110:
      Result := '011';
    vlVersao111:
      Result := '012';
  else
    Result := EmptyStr;
  end;
end;

function IndOperToStr(AValue: TACBrIndOper): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndOper(const AValue: string): TACBrIndOper;
begin
   Result := TACBrIndOper( StrToIntDef( AValue, 0) );
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

function IndEmitToStr(AValue: TACBrIndEmit): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
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

function IndPgtoToStr(AValue: TACBrIndPgto): string;
begin
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

function IndFrtToStr(AValue: TACBrIndFrt): string;
begin
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

function StrToMotInv(const AValue: string): TACBrMotInv;
begin
   if AValue = '01' then
      Result := miFinalPeriodo
   else
   if AValue = '02' then
      Result := miMudancaTributacao
   else
   if AValue = '03' then
      Result := miBaixaCadastral
   else
   if AValue = '04' then
      Result := miRegimePagamento
   else
   if AValue = '05' then
      Result := miDeterminacaoFiscos
   else
     raise EACBrSPEDFiscalException.CreateFmt('O motivo do inventário "%s" não é um valor válido.', [AValue]);
end;

function MotInvToStr(AValue: TACBrMotInv): string;
begin
   if AValue = miFinalPeriodo then
      Result := '01'
   else
   if AValue = miMudancaTributacao then
      Result := '02'
   else
   if AValue = miBaixaCadastral then
      Result := '03'
   else
   if AValue = miRegimePagamento then
      Result := '04'
   else
   if AValue = miDeterminacaoFiscos then
      Result := '05';
end;

function IndPropToStr(AValue: TACBrIndProp): string;
begin
   Result := FormatFloat('00', Integer( AValue ) );
end;

function StrToIndProp(const AValue: string): TACBrIndProp;
begin
   Result := TACBrIndProp( StrToIntDef( AValue, 0) );
end;

function TpLigacaoToStr(AValue: TACBrTpLigacao): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToTpLigacao(const AValue: string): TACBrTpLigacao;
begin
   Result := TACBrTpLigacao( StrToIntDef( AValue, 0) );
end;

function GrupoTensaoToStr(AValue: TACBrGrupoTensao): string;
begin
   if AValue = gtNenhum then
      Result := ''
   else
      Result := FormatFloat('00', Integer( AValue ) + 1 );
end;

function StrToGrupoTensao(const AValue: string): TACBrGrupoTensao;
begin
   if AValue = '' then
      Result := gtNenhum
   else
      Result := TACBrGrupoTensao( StrToIntDef( AValue, 0) );
end;

function IndRecToStr(AValue: TACBrIndRec): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndRec(const AValue: string): TACBrIndRec;
begin
   Result := TACBrIndRec( StrToIntDef( AValue, 0) );
end;

function TpAssinanteToStr(AValue: TACBrTpAssinante): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToTpAssinante(const AValue: string): TACBrTpAssinante;
begin
   if AValue = '' then
      Result := assNenhum
   else
      Result := TACBrTpAssinante( StrToIntDef( AValue, 6) );
end;

function IndReceitaToStr(AValue: TACBrIndReceita): string;
begin
   if AValue = recTerceiroOutras then
      Result := '9'
   else
      Result := IntToStr( Integer( AValue ) );
end;

function StrToIndReceita(const AValue: string): TACBrIndReceita;
begin
   if AValue = '9' then
      Result := recTerceiroOutras
   else
      Result := TACBrIndReceita( StrToIntDef( AValue, 6) );
end;

function CodFinToStr(AValue: TACBrCodFin): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToCodFin(const AValue: string): TACBrCodFin;
begin
   Result := TACBrCodFin( StrToIntDef( AValue, 0) );
end;

function IndPerfilToStr(AValue: TACBrIndPerfil): string;
begin
  case AValue of
    pfPerfilA : result := 'A';
    pfPerfilB : result := 'B';
    pfPerfilC : result := 'C';
    else
      Result := '';
   end;
end;

function StrToIndPerfil(const AValue: string): TACBrIndPerfil;
var
  cPerfil: Char;
begin
   if Length(AValue) > 0 then
     cPerfil := UpCase(AValue[1])
   else
     cPerfil := ' ';

   case cPerfil of
     'A': Result := pfPerfilA;
     'B': Result := pfPerfilB;
     'C': Result := pfPerfilC;
    else
      Result := pfNenhum;
   end;
end;

function IndAtivToStr(AValue: TACBrIndAtiv): string;
begin
   Result := IntToStr( Integer( AValue ) + 1 );
end;

function StrToIndAtiv(const AValue: string): TACBrIndAtiv;
begin
   Result := TACBrIndAtiv( StrToIntDef( AValue, 0) );
end;

function IndMovToStr(const AValue: TACBrIndMov): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function StrToIndMov(const AValue: string): TACBrIndMov;
begin
  Result := TACBrIndMov(StrToIntDef(AValue, 0));
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

function IndCTAToStr(AValue: TACBrIndCTA): string;
begin
  case AValue of
    indCTASintetica: Result := 'S';
    indCTAnalitica: Result := 'A';
  end;
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

function IndTipoOperToStr(AValue: TACBrIndOper): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToIndTipoOper(const AValue: string): TACBrIndOper;
begin
   Result := TACBrIndOper( StrToIntDef( AValue, 0) );
end;

function EmitenteToStr(const AValue: TACBrEmitente): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function StrToEmitente(const AValue: string): TACBrEmitente;
begin
  Result := TACBrEmitente(StrToIntDef(AValue, 0));
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

function DoctoImportaToStr(const AValue: TACBrDoctoImporta): string;
begin
  Result := IntToStr(Ord(AValue));
end;

function StrToDoctoImporta(const AValue: string): TACBrDoctoImporta;
begin
  Result := TACBrDoctoImporta(StrToIntDef(AValue, 0));
end;

function CstPisToStr(AValue: TACBrCstPis): string;
begin
   Result := CstPis[ Integer( AValue ) ];
end;

function StrToCstPis(const AValue: string): TACBrCstPis;
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

function StrToCstPisCofins(const AValue: string): TACBrCstPisCofins;
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

function StrToCstCofins(const AValue: string): TACBrCstCofins;
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

function StrToCstIcms(const AValue: string): TACBrCstIcms;
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

function StrToCstIpi(const AValue: string): TACBrCstIpi;
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

function ApuracaoIPIToStr(const AValue: TACBrApuracaoIPI): string;
begin
  if (AValue = iaNenhum) then
    Result := EmptyStr
  else
    Result := IntToStr(Ord(AValue));
end;

function StrToApuracaoIPI(const AValue: string): TACBrApuracaoIPI;
begin
  if AValue = EmptyStr then
    Result := iaNenhum
  else
    Result := TACBrApuracaoIPI(StrToIntDef(AValue, 0));
end;

function MovimentoStToStr(AValue: TACBrMovimentoST): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToMovimentoSt(const AValue: string): TACBrMovimentoST;
begin
   Result := TACBrMovimentoSt( StrToIntDef( AValue, 0) );
end;

function TipoAjusteToStr(AValue: TACBrIndRec): string;
begin
   Result := IntToStr( Integer( AValue ) );
end;

function StrToTipoAjuste(const AValue: string): TACBrTipoAjuste;
begin
   Result := TACBrTipoAjuste( StrToIntDef( AValue, 0) );
end;

function OrigemDoctoToStr(AValue: TACBrOrigemDocto): string;
begin
   if AValue = odOutros then
      Result := '9'
   else
      Result := IntToStr( Integer( AValue ) );
end;


function StrToOrigemDocto(const AValue: string): TACBrOrigemDocto;
begin
   if AValue = '9' then
      Result := odOutros
   else
      Result := TACBrOrigemDocto( StrToIntDef( AValue, 0) );
end;

end.
