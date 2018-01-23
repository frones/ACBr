{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit pcnConversaoReinf;

{$I ACBr.inc}

interface

uses
  SysUtils, Classes;

type

  tpTpInsc                = (tiCNPJ = 1, tiCPF = 2, tiCNO = 4);

  TpTpInscProp            = (tpCNPJ, tpCPF);

  TLayReinf = (orLayENVIO, orLayConsulta);

  TTypeOperacao         = (toInclusao, toAlteracao, toExclusao);

  TpTpAmb                 = (taNenhum, taProducao, taProducaoRestritaDadosReais, taProducaoRestritaDadosFicticios);

  tpSimNao                = (tpSim, tpNao);

  TpProcEmi               = (peNenhum, peAplicEmpregador, peAplicGoverno);

  TpIndCoop               = (icNaoecooperativa, icCooperativadeTrabalho, icCooperativadeProducao, icOutrasCooperativas );

  tpTpProc                = (tpAdministrativo = 1, tpJudicial = 2);

  tpIndSusp               = (siLiminarMandadoSeguranca = 1,
                             siAntecipacaoTutela = 4,
                             siLiminarMedidaCautelar = 5,
                             siSentencaMandadoSegurancaFavoravelContribuinte = 8,
                             siSentencaAcaoOrdinariaFavContribuinteConfirmadaPeloTRF = 9,
                             siAcordaoTRFFavoravelContribuinte = 10,
                             siAcordaoSTJRecursoEspecialFavoravelContribuinte = 11,
                             siAcordaoSTFRecursoExtraordinarioFavoravelContribuinte = 12,
                             siSentenca1instanciaNaoTransitadaJulgadoEfeitoSusp = 13,
                             siDecisaoDefinitivaAFavorDoContribuinte = 90,
                             siSemSuspensaoDaExigibilidade = 92);


  TindSitPJ               = (spNormal, spExtincao, spFusao, spCisao, spIncorporacao);

  TTypeAutoria            = (taContribuinte = 1, taOutraEntidade = 2);

  TIndRetificacao         = (trOriginal = 1, trRetificacao = 2);

  TpindObra               = (ioNaoeObraDeConstrucaoCivil = 0, ioObradeConstrucaoCivilTotal = 1, ioObradeConstrucaoCivilParcial = 2);

  TpindCPRB               = (icNaoContribuintePrevidenciariaReceitaBruta = 0, icContribuintePrevidenciaReceitaBruta = 1);

  TtpProcRetPrinc         = (tprAdministrativoTomador = 1, tprJudicialTomador = 2, tprJudicialPrestador = 3);


  TReinfSchema            = (
                            rsevtInfoContri,         // R-1000 - Informações do Contribuinte
                            rsevtTabProcesso,        // R-1070 - Tabela de Processos Administrativos/Judiciais
                            rsevtServTom,            // R-2010 - Retenção Contribuição Previdenciária - Serviços Tomados
                            rsevtServPrest,          // R-2020 - Retenção Contribuição Previdenciária - Serviços Prestados
                            rsevtAssocDespRec,       // R-2030 - Recursos Recebidos por Associação Desportiva
                            rsevtAssocDespRep,       // R-2040 - Recursos Repassados para Associação Desportiva
                            rsevtComProd,            // R-2050 - Comercialização da Produção por Produtor Rural PJ/Agroindústria
                            rsevtCPRB,               // R-2060 - Contribuição Previdenciária sobre a Receita Bruta - CPRB
                            rsevtPgtosDivs,          // R-2070 - Retenções na Fonte - IR, CSLL, Cofins, PIS/PASEP
                            rsevtReabreEvPer,        // R-2098 - Reabertura dos Eventos Periódicos
                            rsevtFechaEvPer,         // R-2099 - Fechamento dos Eventos Periódicos
                            rsevtEspDesportivo,      // R-3010 - Receita de Espetáculo Desportivo
                            rsevtTotal,              // R-5001 - Informações das bases e dos tributos consolidados por contribuinte
                            rsevtTotalConsolid,      // R-5011 - Informações de bases e tributos consolidadas por período de apuração
                            rsevtExclusao            // R-9000 - Exclusão de Eventos
                            );

  TtpAjuste               = (taReducao, taAcrescimo);

  TcodAjuste              = (
                            caRegimeCaixa = 1,           // Ajuste da CPRB: Adoção do Regime de Caixa
                            caDifValRecPer = 2,          // Ajuste da CPRB: Diferimento de Valores a recolher no período
                            caAdiValDif = 3,             // Adição de valores Diferidos em Período(s) Anteriores(es)
                            caExpDiretas = 4,            // Exportações diretas
                            caTransInternacional = 5,    // Transporte internacional de cargas
                            caVendasCanceladas = 6,      // Vendas canceladas e os descontos incondicionais concedidos
                            caIPI = 7,                   // IPI, se incluído na receita bruta
                            caICMS = 8,                  // ICMS, quando cobrado pelo vendedor dos bens ou prestador dos serviços na condição de substituto tributário
                            caReceBruta = 9,             // Receita bruta reconhecida pela construção, recuperação, reforma, ampliação ou melhoramento da infraestrutura, cuja contrapartida seja ativo intangível representativo de direito de exploração, no caso de contratos de concessão de serviços públicos
                            caValAporte = 10,            // O valor do aporte de recursos realizado nos termos do art 6 §3 inciso III da Lei 11.079/2004
                            caOutras = 11                // Demais ajustes oriundos da Legislação Tributária, estorno ou outras situações
                            );

  TindExistInfo           = (
                            eiComMovComInfo = 1,    // Há informações de bases e/ou de tributos
                            eiComMovSemInfo = 2,    // Há movimento, porém não há informações de bases ou de tributos
                            eiSemMov = 3            // Não há movimento na competência
                            );

  TindEscrituracao        = ( ieNaoObrig,  // 0 - Não é obrigada
                              ieObrig      // 1 - Empresa obrigada a entregar a ECD
                            );

  TindDesoneracao         = ( idNaoAplic,  // 0 - Não Aplicável
                              idAplic      // 1 - Empresa enquadrada nos art. 7º a 9º da Lei 12.546/2011
                            );

  TindAcordoIsenMulta     = ( aiSemAcordo, // 0 - Sem acordo
                              aiComAcordo  // 1 - Com acordo
                            );

  TindNIF                 = ( nifCom = 1,        // 1 - Beneficiário com NIF;
                              nifDispensado = 2, // 2 - Beneficiário dispensado do NIF
                              nifNaoExige = 3    // 3 - País não exige NIF
                            );

  TindTpDeducao           = ( itdOficial = 1,    // 1 - Previdência Oficial
                              itdPrivada = 2,    // 2 - Previdência Privada
                              itdFapi = 3,       // 3 - Fapi
                              itdFunpresp = 4,   // 4 - Funpresp
                              itdPensao = 5,     // 5 - Pensão Alimentícia
                              itdDependentes = 6 // 6 - Dependentes
                            );

  TtpIsencao              = ( tiIsenta = 1,              // 1 - Parcela Isenta 65 anos
                              tiAjudaCusto = 2,          // 2 - Diária e Ajuda de Custo
                              tiIndenizaRescisao = 3,    // 3 - Indenização e rescisão de contrato, inclusive a título de PDV
                              tiAbono = 4,               // 4 - Abono pecuniário
                              tiOutros = 5,              // 5 - Outros (especificar)
                              tiLucros = 6,              // 6 - Lucros e dividendos pagos a partir de 1996
                              tiSocioMicroempresa = 7,   // 7 - Valores pagos a titular ou sócio de microempresa ou empresa de pequeno porte, exceto pró-labore e alugueis
                              tiPensaoAposentadoria = 8, // 8 - Pensão, aposentadoria ou reforma por moléstia grave ou acidente em serviço
                              tiBeneficiosIndiretos = 9, // 9 - Benefícios indiretos e/ou reembolso de despesas recebidas por voluntário da copa do mundo ou da copa das confederações
                              tiBolsaEstudo = 10,        // 10 - Bolsa de estudo recebida por médico-residente
                              tiComplAposentadoria = 11  // 11 - Complementação de aposentadoria, correspondente às contribuições efetuadas no período de 01/01/1989 a 31/12/1995
                            );

  TindPerReferencia       = ( iprMensal = 1,     // 1 - Folha de Pagamento Mensal
                              iprDecTerceiro = 2 // 2 - Folha do Décimo Terceiro Salário
                            );

  TindOrigemRecursos      = ( iorProprios = 1, // 1 - Recursos do próprio declarante
                              iorTerceiros = 2 // 2 - Recursos de terceiros - Declarante é a Instituição Financeira responsável apenas pelo repasse dos valores
                            );

  TtpRepasse              = ( trPatrocinio = 1,    // 1 - Patrocínio
                              trLicenciamento = 2, // 2 - Licenciamento de marcas e símbolos
                              trPublicidade = 3,   // 3 - Publicidade
                              trPropaganda = 4,    // 4 - Propaganda
                              trTransmissao = 5    // 5 - Transmissão de espetáculos
                            );

  TindCom                 = ( icProdRural = 1,  // 1 - Comercialização da Produção por Prod. Rural PJ/Agroindústria, exceto para entidades executoras do PAA
                              icPAA = 2,        // 8 - Comercialização da Produção para Entidade do Programa de Aquisição de Alimentos - PAA
                              icMercExterno = 3 // 9 - Comercialização direta da Produção no Mercado Externo
                            );

  TtpCompeticao           = ( ttcOficial = 1,   // 1 - Oficial
                              ttcnaoOficial = 2 // 2 - Não Oficial
                            );

  TcategEvento            = ( tceInternacional = 1,  // 1 - Internacional
                              tceInterestadual = 2,  // 2 - Interestadual
                              tceEstadual = 3,       // 3 - Estadual
                              tceLocal = 4           // 4 - Local
                            );

  TtpIngresso             = ( ttiArquibancada = 1, // 1 - Arquibancada
                              ttiGeral = 2,        // 2 - Geral
                              ttiCadeiras = 3,     // 3 - Cadeiras
                              ttiCamarote = 4      // 4 - Camarote
                            );

  TtpReceita              = ( ttrTransmissso = 1, // 1 - Transmissão
                              ttrPropaganda = 2,  // 2 - Propaganda
                              ttrPublicidade = 3, // 3 - Publicidade
                              ttrSorteio = 4,     // 4 - Sorteio
                              ttrOutros = 5       // 5 - Outros
                            );

   TpcnVersaoReinf        = ( v1_02_00, // v1.2
                              v1_03_00  // v1.3
                            );

const
  PrefixVersao = '-v';
  TReinfSchemaStr : array[0..14] of string = ('evtInfoContribuinte',                 // R-1000 - Informações do Contribuinte
                                              'evtTabProcesso',                      // R-1070 - Tabela de Processos Administrativos/Judiciais
                                              'evtTomadorServicos',                  // R-2010 - Retenção Contribuição Previdenciária - Serviços Tomados
                                              'evtPrestadorServicos',                // R-2020 - Retenção Contribuição Previdenciária - Serviços Prestados
                                              'evtRecursoRecebidoAssociacao',        // R-2030 - Recursos Recebidos por Associação Desportiva
                                              'evtRecursoRepassadoAssociacao',       // R-2040 - Recursos Repassados para Associação Desportiva
                                              'evtInfoProdRural',                    // R-2050 - Comercialização da Produção por Produtor Rural PJ/Agroindústria
                                              'evtInfoCPRB',                         // R-2060 - Contribuição Previdenciária sobre a Receita Bruta - CPRB
                                              'evtPagamentosDiversos',               // R-2070 - Retenções na Fonte - IR, CSLL, Cofins, PIS/PASEP
                                              'evtReabreEvPer',                      // R-2098 - Reabertura dos Eventos Periódicos
                                              'evtFechamento',                       // R-2099 - Fechamento dos Eventos Periódicos
                                              'evtEspDesportivo',                    // R-3010 - Receita de Espetáculo Desportivo
                                              'evtTotal',                            // R-5001 - Informações das bases e dos tributos consolidados por contribuinte
                                              'evtTotalConsolid',                    // R-5011 - Informações de bases e tributos consolidadas por período de apuração
                                              'evtExclusao'                          // R-9000 - Exclusão de Eventos
                                              );

  TReinfSchemaRegistro : array[0..14] of string = ('R-1000', // rsevtInfoContri    - Informações do Contribuinte
                                                   'R-1070', // rsevtTabProcesso   - Tabela de Processos Administrativos/Judiciais
                                                   'R-2010', // rsevtServTom       - Retenção Contribuição Previdenciária - Serviços Tomados
                                                   'R-2020', // rsevtServPrest     - Retenção Contribuição Previdenciária - Serviços Prestados
                                                   'R-2030', // rsevtAssocDespRec  - Recursos Recebidos por Associação Desportiva
                                                   'R-2040', // rsevtAssocDespRep  - Recursos Repassados para Associação Desportiva
                                                   'R-2050', // rsevtComProd       - Comercialização da Produção por Produtor Rural PJ/Agroindústria
                                                   'R-2060', // rsevtCPRB          - Contribuição Previdenciária sobre a Receita Bruta - CPRB
                                                   'R-2070', // rsevtPgtosDivs     - Retenções na Fonte - IR, CSLL, Cofins, PIS/PASEP
                                                   'R-2098', // rsevtReabreEvPer   - Reabertura dos Eventos Periódicos
                                                   'R-2099', // rsevtFechaEvPer    - Fechamento dos Eventos Periódicos
                                                   'R-3010', // rsevtEspDesportivo - Receita de Espetáculo Desportivo
                                                   'R-5001', // rsevtTotal         - Informações das bases e dos tributos consolidados por contribuinte
                                                   'R-5011', // rsevtTotalConsolid - Informações de bases e tributos consolidadas por período de apuração
                                                   'R-9000'  // rsevtExclusao      - Exclusão de Eventos
                                                   );

implementation

end.
