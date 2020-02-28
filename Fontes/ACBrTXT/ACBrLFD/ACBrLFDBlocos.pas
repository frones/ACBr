{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro                }
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

unit ACBrLFDBlocos;

interface

uses
  SysUtils, Classes, DateUtils, ACBrTXTClass, contnrs;

Const
  /// Código da Situação Tributária referente ao IPI.
  ipiEntradaRecuperacaoCredito = '00' ; // Entrada com recuperação de crédito
  ipiEntradaTributradaZero     = '01' ; // Entrada tributada com alíquota zero
  ipiEntradaIsenta             = '02' ; // Entrada isenta
  ipiEntradaNaoTributada       = '03' ; // Entrada não-tributada
  ipiEntradaImune              = '04' ; // Entrada imune
  ipiEntradaComSuspensao       = '05' ; // Entrada com suspensão
  ipiOutrasEntradas            = '49' ; // Outras entradas
  ipiSaidaTributada            = '50' ; // Saída tributada
  ipiSaidaTributadaZero        = '51' ; // Saída tributada com alíquota zero
  ipiSaidaIsenta               = '52' ; // Saída isenta
  ipiSaidaNaoTributada         = '53' ; // Saída não-tributada
  ipiSaidaImune                = '54' ; // Saída imune
  ipiSaidaComSuspensao         = '55' ; // Saída com suspensão
  ipiOutrasSaidas              = '99' ; // Outras saídas

type
  /// Indicador de movimento - TOpenBlocos
  TACBrLIndicadorMovimento = (imlComDados, // 0- Bloco com dados informados;
                             imlSemDados  // 1- Bloco sem dados informados.
                             );

  /// Código indicador de circulação
  TACBrIndicadorCirculacao = (icReal, // 0- Movimentação (circulação) real do ite
                              icSimbolica // 1- Movimentação (circulação) simbólica do item
                              );

  /// Indicador do tipo de totalização do ECF
  TACBrTipoTotalECF = (itDiario, // 0 - Total do dia
                       itMensal // 1 - Total do mês
                       );

  /// Indicador de observações do Mapa-Resumo ECF
  TACBrObsMapaResumo = (ioSemObservacoes, // 0- MR-ECF sem observação
                        ioComObservacoes // 1- MR-ECF com observação(ões)
                        );

  TACBrDataInventario = (di0, // 0- Levantado no último dia do ano civil, coincidente com a data do balanço
                         di1, // 1- Levantado no último dia do ano civil, divergente da data do balanço
                         di2, // 2- Levantado na data do balanço, divergente do último dia do ano civil
                         di3 // 3- Levantado em data divergente da data do último balanço e do último dia do ano civil
                         );

  TACBrTotalAquisicaoPrestacao = (isSubtotalAquisicaoInterna, // 0- Subtotal das aquisições internas
                                  isSubtotalAquisicaoOutros, // 1- Subtotal das aquisições de outros municípios
                                  isSubtotalAquisicaoExterior, // 2- Subtotal das aquisições do exterior
                                  isTotalAquisicoes, // 3- Total das aquisições do período
                                  isSubtotalPrestacaoInterna, // 4- Subtotal das prestações internas
                                  isSubtotalPrestacaoOutros, // 5- Subtotal das prestações para outros municípios
                                  isSubtotalPrestacaoExterior, // 6- Subtotal das prestações para o exterior
                                  isTotalPrestacoes // 7- Total das prestações do período
                                  );

  TACBrTipoDeducaoISS = (tdCompensacaoAMaior, // 0- Compensação do ISS calculado a maior
                         tdIncentivoCultura, // 1- Benefício fiscal por incentivo à cultura
                         tdDecisaoJudicial, // 2- Decisão administrativa ou judicial
                         tdOutros // 9- Outros
                         );

  /// Indicador do tipo de compensação do ISS
  TACBrTipoCompensacaoISS = (ciCancelamento, // 0- Cancelamento de nota fiscal
                             ciGlosaValor, // 1- Glosa de valor faturado
                             ciErroValor, // 2- Erro de preenchimento de valor faturado
                             ciErroValorDeclarado, // 3- Erro de preenchimento de valor faturado
                             ciErroValorDeducao, // 4- Erro de preenchimento de valor de dedução da base de cálculo
                             ciErroValorISSRetido, // 5- Erro de preenchimento de valor de ISS retido
                             ciOutros // 9- Outros
                             );

  /// Indicador de habilitação profissional
  TACBrHabilitacaoProfissional = (hpHabilitado, // 0- Profissional habilitado
                                  hpNaoHabilitado // 1- Profissional não habilitado
                                  );

  /// Indicador de escolaridade
  TACBrEscolaridade = (neSuperior, // 0- Nïvel superior
                            neMedio // 1- Nível médio
                            );

  /// Indicador de participação societária
  TACBrParticipacaoSocietaria = (psSocio, // 0- Sócio
                                 psNaoSocio // 1- Não sócio
                                 );

  {Juliana Tamizou - começa aqui}
  /// Versão do Leiaute do arquivo - TRegistro0000
  TACBrLVersaoLeiaute      = (vlVersao1001,  // Código 1001 - Versão 1.0.0.1
                             vlVersao1002,  // Código 1002 - Versão 1.0.0.2
                             vlVersao1003,  // Código 1003 - Versão 1.0.0.3
                             vlVersao1004,  // Código 1004 - Versão 1.0.0.4
                             vlVersao1005,  // Código 1005 - Versão 1.0.0.5
                             vlVersao2000   // Código 2000 - Versão 2.0.0.0
                             );
  /// Código da finalidade do arquivo - TRegistro0000
  TACBrLCodFinalidade      = (ralRegular,                       // 00 - Remessa regular do arquivo original
                             ralSubstituto,                    // 01 - Remessa do arquivo substituto
                             ralDadosAdicionais,               // 02 - Remessa de arquivo com dados adicionais a arquivo anteriormente remetido
                             ralIntimacaoEsp,                  // 03 - Remessa de arquivo requerido por intimação específica
                             ralCorrecaoIDP,                   // 04 - Remessa de arquivo requerido para correção do Índice de Participação dos Municípios
                             ralPubDiarioOficial,              // 05 - Remessa de arquivo requerido por ato publicado no Diário Oficial
                             ralSintegraRegular,               // 15 - Sintegra - remessa regular de arquivo das operações interestaduais
                             ralSintegraSubstituto,            // 16 - Sintegra - remessa de arquivo substituto das operações interestaduais
                             ralSintegraDadosAdicionais,       // 17 - Sintegra - remessa de arquivo com dados adicionais das operações interestaduais
                             ralSintegraRegularICMSST,         // 18 - Sintegra - remessa regular de arquivo das operações interestaduais com substituição tributária do ICMS
                             ralSintegraSubstitutoICMSST,      // 19 - Sintegra - remessa de arquivo substituto das operações interestaduais com substituição tributária do ICMS
                             ralSintegraDadosAdicionaisICMSST, // 20 - Sintegra - remessa de arquivo com dados adicionais das operações interestaduais com substituição tributária do ICMS
                             ralRegularSefin,                  // 25 - Remessa para a Sefin/Mun de arquivo de retenções do ISS efetuadas por terceiros
                             ralSubstitutoSefin,               // 26 - Remessa para a Sefin/Mun de arquivo substituto de retenções do ISS efetuadas por terceiros
                             ralDadosAdicionaisSefin,          // 27 - Remessa para a Sefin/Mun de arquivo com dados adicionais de retenções do ISS efetuadas por terceiros
                             ralEmissaoDocumento,              // 30 - Emissão de documento
                             ralEmissaoDocAvulso,              // 31 - Emissão de documento fiscal avulso por repartição fiscal
                             ralSolicAuditorFical,             // 61 - Solicitação de Auditor-Fiscal da Secretaria da Receita Previdenciária através de MPF
                             ralEntregaSecretariaReceita,      // 62 - Entrega na Secretaria da Receita Previdenciária - movimento anual de órgão público, conforme intimação
                             ralInfComplementarSefaz           // 90 - Remessa de informações complementares para a Sefaz da unidade da federação de origem
                             );


  /// Situação do Documento
  TACBrlSituacaoDocto = (sdlRegular,                     // 00 - Documento regular
                        sdlExtempRegular,               // 01 - Escrituração extemporânea de documento regular
                        sdlCancelado,                   // 02 - Documento cancelado
                        sdlCancelamentoDocAnterior,     // 03 - Cancelamento de cupom fiscal anterior
                        sdlCanceladoExtemp,             // 04 - Escrituração extemporânea de documento cancelado
                        sdlDesfazimentoNegocio,         // 05 - Desfazimento de negócio
                        sdlDocumentoReferenciado,       // 06 - Documento referenciado
                        sdlRegularSimples,              // 07 - Documento regular - Simples Nacional
                        sdlExtempRegularSimples,        // 08 - Documento regular extemporâneo - Simples Nacional
                        sdlLancDoctoregular,            // 50 - Lançamento de documento regular
                        sdlLancExtempDoctoRegular,      // 51 - Lançamento de documento regular extemporâneo
                        sdlLancDoctoCancelado,          // 52 - Lançamento de documento cancelado
                        sdlLancCancelamentoDocAnterior, // 53 - Lançamento de cancelamento de cupom fiscal anterior
                        sdlLancCanceladoExtemp,         // 54 - Lançamento de documento cancelado extemporâneo
                        sdlLancDesfazimentoNegocio,     // 55 - Lançamento de desfazimento de negócio
                        sdlLancDocumentoReferenciado,   // 56 - Lançamento de documento referenciado
                        sdlLancDoctoOutrasSituacoes,    // 58 - Lançamento de documento em outras situações de repercussão nula
                        sdlLancDoctoRepercNevativa      // 59 - Lançamento de documento com repercussão negativa
                        );

  /// Tipo do item – Atividades Industriais, Comerciais e Serviços:
  TACBrTipoItem = (tiMercadoria,           // 00 – Mercadoria para Revenda
                   tiMateriaPrima,         // 01 – Matéria-Prima;
                   tiProdutoIntermediario, // 02 - Produto intermediário;
                   tiProdutoemfabricacao,  // 03 - Produto em fabricação;
                   tiProdutoAcabado,       // 04 – Produto Acabado;
                   tiEmbalagem,            // 05 – Embalagem;
                   tiOutras                // 99 – Outras
                   );

  /// Indicador do tipo de operação:
  TACBrLTipoOperacao = (tplEntradaAquisicao, // 0 - Entrada
                       tplSaidaPrestacao    // 1 - Saída
                       );

  /// Indicador do emitente do documento fiscal
  TACBrlEmitente = (edlEmissaoPropria,         // 0 - Emissão própria
                   edlTerceiros               // 1 - Terceiro
                   );

  /// Indicador do tipo de pagamento
  TACBrlTipoPagamento = (tplVista,             // 0 - À Vista
                        tplPrazo              // 1 - A Prazo
                        );

  /// Indicador do tipo do frete
  TACBrTipoFrete = (tflPorContaEmitente,      // 0 - Por conta do emitente
                    tflPorContaDestinatario,  // 1 - Por conta do destinatário
                    tflSemIndicacaoFrete,     // 2 - Sem indicação de frete
                    tflPorContaTerceiros      // 3 - Por conta de terceiros
                    );

  /// Indicador da origem do processo
  TACBrlOrigemProcesso = (oplSefaz,            // 0 - Sefaz
                         oplJusticaFederal,   // 1 - Justiça Federal
                         oplJusticaEstadual,  // 2 - Justiça Estadual
                         oplOutros,           // 9 - Outros
                         oplNenhum           // Preencher vazio
                         );
  /// Indicador do tipo de operação
  TACBrTipoOperacaoST = (toCombustiveisLubrificantes, // 0 - Combustíveis e Lubrificantes
                         toLeasingVeiculos            // 1 - leasing de veículos ou faturamento direto
                         );

  TACBrDoctoArrecada = (daEstadualArrecadacao,  // 0 - Documento Estadual de Arrecadação
                        daGNRE,                 // 1 - GNRE
                        daMunicipalArrecadacao, // 2 - Documento de arrecadação municipal
                        daFederalArrecadacao,   // 3 - Documento de arrecadação federal
                        daOutros                // 9 - Outros: descrever
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

  /// Indicador do tipo de título de crédito
  TACBrTipoTitulo = (tcDuplicata,             // 00- Duplicata
                     tcCheque,                // 01- Cheque
                     tcPromissoria,           // 02- Promissória
                     tcRecibo,                // 03- Recibo
                     tcOutros                 // 99- Outros (descrever)
                     );

  /// Indicador de tipo de referência da base de cálculo do ICMS (ST) do produto farmacêutico
  TACBrTipoBaseMedicamento = (bmCalcTabeladoSugerido,           // 0 - Base de cálculo referente ao preço tabelado ou preço máximo sugerido;
                              bmCalListNegativa,                // 1 - Base de cálculo referente à Lista Negativa;
                              bmCalListaPositiva,               // 2 - Base de cálculo referente à Lista Positiva;
                              bmCalListNeutra                   // 3 - Base de cálculo referente à Lista Neutra
                              );
  /// Tipo Produto
  TACBrTipoProduto = (tpSimilar,   // 0 - Similar
                      tpGenerico,  // 1 - Genérico
                      tpReferencia // 2 - Ético ou de Marca
                      );

  /// Indicador do tipo da arma de fogo
  TACBrTipoArmaFogo = (tafPermitido,     // 0 - Permitido
                       tafRestrito       // 1 - Restrito
                       );

  /// Indicador do tipo de operação com veículo
  TACBrTipoOperacaoVeiculo = (tovVendaPConcess,   // 0 - Venda para concessionária
                              tovFaturaDireta,    // 1 - Faturamento direto
                              tovVendaDireta,     // 2 - Venda direta
                              tovVendaOutros      // 9 - Outros
                              );

  /// Indicador do tipo de receita
  TACBrTipoReceita = (trPropria,   // 0 - Receita própria
                      trTerceiro   // 1 - Receita de terceiros
                      );

  /// Indicador do tipo do veículo transportador
  TACBrTipoVeiculo = (tvEmbarcacao,
                      tvEmpuradorRebocador
                      );

  /// Indicador do tipo da navegação
  TACBrTipoNavegacao = (tnInterior,
                        tnCabotagem
                        );

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
  TACBrIndTipoReceita = (recServicoPrestado,          // 0 - Receita própria - serviços prestados;
                         recCobrancaDebitos,          // 1 - Receita própria - cobrança de débitos;
                         recVendaMerc,                // 2 - Receita própria - venda de mercadorias;
                         recServicoPrePago,           // 3 - Receita própria - venda de serviço pré-pago;
                         recOutrasProprias,           // 4 - Outras receitas próprias;
                         recTerceiroCoFaturamento,    // 5 - Receitas de terceiros (co-faturamento);
                         recTerceiroOutras            // 9 - Outras receitas de terceiros
                         );

  /// Indicador do tipo de serviço prestado
  TACBrServicoPrestado = (spTelefonia,                // 0- Telefonia;
                          spComunicacaoDados,         // 1- Comunicação de dados;
                          spTVAssinatura,             // 2- TV por assinatura;
                          spAcessoInternet,           // 3- Provimento de acesso à Internet;
                          spMultimidia,               // 4- Multimídia;
                          spOutros                    // 9- Outros
                          );

  /// Indicador de movimento
  TACBrlMovimentoST = (mstlSemOperacaoST,   // 0 - Sem operações com ST
                      mstlComOperacaoST    // 1 - Com operações de ST
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
  TACBrlPosseItem = (pilInformante,           // 0- Item de propriedade do informante e em seu poder;
                    pilInformanteNoTerceiro, // 1- Item de propriedade do informante em posse de terceiros;
                    pilTerceiroNoInformante  // 2- Item de propriedade de terceiros em posse do informante
                    );

  /// Identificador de medição
  TACBrMedicao = (medAnalogico,            // 0 - analógico;
                  medDigital               // 1 – digital
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
  TACBrTipoLigacao = (tlNenhum,              // '' - Para uso quando o documento for cancelado
                      tlMonofasico,          // 1 - Monofásico
                      tlBifasico,            // 2 - Bifásico
                      tlTrifasico            // 3 - Trifásico
                      );

  /// Código dispositivo autorizado
  TACBrDispositivo = (cdaFormSeguranca,  // 00 - Formulário de Segurança
                      cdaFSDA,           // 01 - FS-DA – Formulário de Segurança para Impressão de DANFE
                      cdaNFe,            // 02 – Formulário de segurança - NF-e
                      cdaFormContinuo,   // 03 - Formulário Contínuo
                      cdaBlocos,         // 04 – Blocos
                      cdaJogosSoltos     // 05 - Jogos Soltos
                      );

  /// Código do Tipo de Assinante
  TACBrTipoAssinante = (assComercialIndustrial,    // 1 - Comercial/Industrial
                        assPodrPublico,            // 2 - Poder Público
                        assResidencial,            // 3 - Residencial/Pessoa física
                        assPublico,                // 4 - Público
                        assSemiPublico,            // 5 - Semi-Público
                        assOutros                  // 6 - Outros
                        );

  // Indicador de Entrada de Dados
  TACBrTipoEntradaDados = (enDigitacao,      // 0 - Digitação de dados
                           enImportacaoTXT,  // 1 - Importação de arquivo texto
                           enValidacaoTXT    // 2 - Validação de arquivo texto
                           );


  // Indicador de  conteúdo do arquivo
  TACBrConteudoArquivo = (coDocFiscal,        // 0 - Registros de documento fiscal
                          coEscrFiscal,       // 1 - Lançamentos de escrituração fiscal
                          coControlesFiscais, // 2 - Lançamentos de controles fiscais
                          coInfEconoFiscal,   // 3 - Registros de informação econômico-fiscal
                          coEscrContabil,     // 4 - Lançamentos de escrituração contábil
                          coDemoContabeis,    // 5 - Registros de demonstrações da contábeis
                          coExtratos          // 6 - Registros de extratos de documentos fiscais ou contábeis
                          );


  // Indicador de Escrituração Fiscal
  TACBrTipoEscrFiscal = (esSimplificada,     // 0 - Simplificada
                         esIntermediaria,    // 1 - Intermediária
                         esIntegral,         // 2 - Integral
                         esNaoObrigado       // 3 - Não obrigado
                         );

  // Indicador de Escrituração Contábil
  TACBrTipoEscrContabil = (ecCompletaDigital,     // 0 - Completa, registrada em arquivo digital
                           ecCompletaPapel,       // 1 - Completa, registrada em papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                           ecSimplificadaDigital, // 2 - Simplificada, registrada em arquivo digital
                           ecSimplificadaPapel,   // 3 - Simplificada, registrada papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                           ecLivroCaixaDigital,   // 4 - Livro Caixa, registrado em arquivo digital
                           ecLivroCaixaPapel,     // 5- Livro Caixa, registrado papel, microfilme, fichas avulsas, ou fichas/folhas contínuas
                           ecNaoObrigado          // 6- Não obrigado
                           );


  TACBrTipoTributacao = (tAliqInformada,    // 0 - Alíquota informada
                         tInicioAtividade,  // 1 - Início de atividades (percentual mínimo)
                         tAliqNaoInformada, // 2 - Alíquota não informada (percentual máximo)
                         tValorFixo         // 3 - Tributação por valor fixo (sem retenção)
                         );


  TACBrAnexoRT = (aAnexoIII,  // 3 - Anexo III
                  aAnexoIV,   // 4 - Anexo IV
                  aAnexoV     // 5 - Anexo V
                  );

   TACBrAnexoCRD = (aAnexoI,  // 1 - Anexo I
                    aAnexoII  // 2 - Anexo II
                    );


  {Tabela de Benefícios Fiscais do ICMS}
  TACBrCODBFICMS = (bNenhum,
                    bDF001,  // DF001 Regime Especial para Atacadista - TARE
                    bPE001,  // Programa de Desenvolvimento do Estado de Pernambuco - Prodepe
                    bPE002,  // Programa de Desenvolvimento da Indústria Naval e de Mecânica Pesada Associada do Estado de Pernambuco - Prodinpe
                    bPE003,  // Programa de Apoio às Empresas de Base Tecnológica - Probatec
                    bPE004,  // Programa de Desenvolvimento da Indústria de Calçados
                    bPE005,  // Centrais de Distribuição de Supermercados e Lojas de Departamentos
                    pPE006,  // Atacadistas de Alimentos, Bebidas, Produtos de Higiene Pessoal e Limpeza
                    pPE007   // Indústrias do Pólo Têxtil e de Confecções
                    );

  {Tabela de Benefícios Fiscais do ISS}
  TACBrCODBFISS = (bSNenhum,
                   bSDF001   // DF001 Programa de Incentivo à Arte e à Cultura
                    );

  {Indicador de Auteração}
  TACBrIndAlteracao = (alPerda, // 0 - Perda
                       alGanho  // 1 - Ganho
                       );

  {Indicador de Tipo de Complemento de ICMS}
  TACBrIndCompICMS = ( cSemValor,         // 00- Sem valor de crédito do ICMS a complementar;
                       cDifICMSST,        // 01- Complemento relativo à diferença do ICMS da substituição tributária calculado a menor;
                       cDifAliqAtivoFixo, // 02- Complemento do diferencial de alíquotas do ICMS relativo a aquisições para o ativo fixo;
                       cAliqUsoConsumo,   // 03- Complemento do diferencial de alíquotas do ICMS relativo a aquisições para uso e/ou consumo;
                       cAliqOutrasSitu,   // 04- Complemento do diferencial de alíquotas do ICMS relativo a outras situações;
                       cAntecTributaria,  // 05- Complemento relativo à antecipação tributária;
                       cProgBenFical,     // 06- Complemento relativo a programa de benefício fiscal;
                       cOutrasSituacoes   // 99- Outras situações (descrever em observações)
                      );


  { TOpenBlocos }

  TOpenBlocos = class
  private
    FCOD_MUN: Integer;
    FIND_MOV: TACBrLIndicadorMovimento;    /// Indicador de movimento: 0- Bloco com dados informados, 1- Bloco sem dados informados.
  public
    property IND_MOV: TACBrLIndicadorMovimento read FIND_MOV write FIND_MOV;
    property COD_MUN: Integer read FCOD_MUN write FCOD_MUN;
  end;

  TACBrLFDRegistros = class(TObjectList)
  public
    function AchaUltimoPai(ANomePai, ANomeFilho: String): Integer;
  end;

implementation

{ TOpenBlocos }

{ TACBrLFDRegistros }

function TACBrLFDRegistros.AchaUltimoPai(ANomePai, ANomeFilho: String): Integer;
begin
  Result := Count - 1;
  if Result < 0 then
    raise Exception.CreateFmt('O registro %:0s deve ser filho do registro %:1s, e não existe nenhum %:1s pai!', [ANomeFilho, ANomePai]);
end;

end.
