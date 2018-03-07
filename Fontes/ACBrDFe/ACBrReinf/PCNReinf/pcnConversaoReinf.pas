{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

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

  TtpInsc                 = (tiCNPJ, tiCPF, tiCNO);

  TtpInscProp             = (tpCNPJ, tpCPF);

  TLayOutReinf            = (LayEnvioLoteEventos, LayConsultaLoteEventos);

  TStatusReinf            = (stIdle, stEnvLoteEventos, stConsultaLote);

  TindOperacao            = (toInclusao, toAlteracao, toExclusao);

  TtpAmb                  = (taNenhum, taProducao, taProducaoRestritaDadosReais, taProducaoRestritaDadosFicticios);

  TtpSimNao               = (tpSim, tpNao);

  TprocEmi                = (peNenhum, peAplicEmpregador, peAplicGoverno);

  TpIndCoop               = (icNaoecooperativa, icCooperativadeTrabalho, icCooperativadeProducao, icOutrasCooperativas );

  TtpProc                 = (tpAdministrativo, tpJudicial);

  TindSusp                = (siLiminarMandadoSeguranca,
                             siAntecipacaoTutela,
                             siLiminarMedidaCautelar,
                             siSentencaMandadoSegurancaFavoravelContribuinte ,
                             siSentencaAcaoOrdinariaFavContribuinteConfirmadaPeloTRF,
                             siAcordaoTRFFavoravelContribuinte,
                             siAcordaoSTJRecursoEspecialFavoravelContribuinte,
                             siAcordaoSTFRecursoExtraordinarioFavoravelContribuinte,
                             siSentenca1instanciaNaoTransitadaJulgadoEfeitoSusp,
                             siDecisaoDefinitivaAFavorDoContribuinte,
                             siSemSuspensaoDaExigibilidade);

  TTipoEvento             = (teR1000, teR1070, teR2010, teR2020, teR2030,
                             teR2040, teR2050, teR2060, teR2070, teR2098,
                             teR2099, teR3010, teR5001, teR5011, teR9000);

  TindSitPJ               = (spNormal, spExtincao, spFusao, spCisao, spIncorporacao);

  TindAutoria             = (taContribuinte, taOutraEntidade);

  TIndRetificacao         = (trOriginal, trRetificacao);

  TpindObra               = (ioNaoeObraDeConstrucaoCivil,
                             ioObradeConstrucaoCivilTotal,
                             ioObradeConstrucaoCivilParcial);

  TpindCPRB               = (icNaoContribuintePrevidenciariaReceitaBruta,
                             icContribuintePrevidenciaReceitaBruta);

  TtpProcRetPrinc         = (tprAdministrativoTomador, tprJudicialTomador,
                             tprJudicialPrestador);

  TReinfSchema            = (
                            schevtInfoContri,         // R-1000 - Informações do Contribuinte
                            schevtTabProcesso,        // R-1070 - Tabela de Processos Administrativos/Judiciais
                            schevtServTom,            // R-2010 - Retenção Contribuição Previdenciária - Serviços Tomados
                            schevtServPrest,          // R-2020 - Retenção Contribuição Previdenciária - Serviços Prestados
                            schevtAssocDespRec,       // R-2030 - Recursos Recebidos por Associação Desportiva
                            schevtAssocDespRep,       // R-2040 - Recursos Repassados para Associação Desportiva
                            schevtComProd,            // R-2050 - Comercialização da Produção por Produtor Rural PJ/Agroindústria
                            schevtCPRB,               // R-2060 - Contribuição Previdenciária sobre a Receita Bruta - CPRB
                            schevtPgtosDivs,          // R-2070 - Retenções na Fonte - IR, CSLL, Cofins, PIS/PASEP
                            schevtReabreEvPer,        // R-2098 - Reabertura dos Eventos Periódicos
                            schevtFechaEvPer,         // R-2099 - Fechamento dos Eventos Periódicos
                            schevtEspDesportivo,      // R-3010 - Receita de Espetáculo Desportivo
                            schevtTotal,              // R-5001 - Informações das bases e dos tributos consolidados por contribuinte
                            schevtTotalConsolid,      // R-5011 - Informações de bases e tributos consolidadas por período de apuração
                            schevtExclusao,           // R-9000 - Exclusão de Eventos
                            schErro, schConsultaLoteEventos, schEnvioLoteEventos
                            );

  TtpAjuste               = (taReducao, taAcrescimo);

  TcodAjuste              = (
                            caRegimeCaixa,          // Ajuste da CPRB: Adoção do Regime de Caixa
                            caDifValRecPer,         // Ajuste da CPRB: Diferimento de Valores a recolher no período
                            caAdiValDif,            // Adição de valores Diferidos em Período(s) Anteriores(es)
                            caExpDiretas,           // Exportações diretas
                            caTransInternacional,   // Transporte internacional de cargas
                            caVendasCanceladas,     // Vendas canceladas e os descontos incondicionais concedidos
                            caIPI,                  // IPI, se incluído na receita bruta
                            caICMS,                 // ICMS, quando cobrado pelo vendedor dos bens ou prestador dos serviços na condição de substituto tributário
                            caReceBruta,            // Receita bruta reconhecida pela construção, recuperação, reforma, ampliação ou melhoramento da infraestrutura, cuja contrapartida seja ativo intangível representativo de direito de exploração, no caso de contratos de concessão de serviços públicos
                            caValAporte,            // O valor do aporte de recursos realizado nos termos do art 6 §3 inciso III da Lei 11.079/2004
                            caOutras                // Demais ajustes oriundos da Legislação Tributária, estorno ou outras situações
                            );

  TindExistInfo           = (
                            eiComMovComInfo,    // Há informações de bases e/ou de tributos
                            eiComMovSemInfo,    // Há movimento, porém não há informações de bases ou de tributos
                            eiSemMov            // Não há movimento na competência
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

  TindNIF                 = ( nifCom,        // 1 - Beneficiário com NIF;
                              nifDispensado, // 2 - Beneficiário dispensado do NIF
                              nifNaoExige    // 3 - País não exige NIF
                            );

  TindTpDeducao           = ( itdOficial,    // 1 - Previdência Oficial
                              itdPrivada,    // 2 - Previdência Privada
                              itdFapi,       // 3 - Fapi
                              itdFunpresp,   // 4 - Funpresp
                              itdPensao,     // 5 - Pensão Alimentícia
                              itdDependentes // 6 - Dependentes
                            );

  TtpIsencao              = ( tiIsenta,              // 1 - Parcela Isenta 65 anos
                              tiAjudaCusto,          // 2 - Diária e Ajuda de Custo
                              tiIndenizaRescisao,    // 3 - Indenização e rescisão de contrato, inclusive a título de PDV
                              tiAbono,               // 4 - Abono pecuniário
                              tiOutros,              // 5 - Outros (especificar)
                              tiLucros,              // 6 - Lucros e dividendos pagos a partir de 1996
                              tiSocioMicroempresa,   // 7 - Valores pagos a titular ou sócio de microempresa ou empresa de pequeno porte, exceto pró-labore e alugueis
                              tiPensaoAposentadoria, // 8 - Pensão, aposentadoria ou reforma por moléstia grave ou acidente em serviço
                              tiBeneficiosIndiretos, // 9 - Benefícios indiretos e/ou reembolso de despesas recebidas por voluntário da copa do mundo ou da copa das confederações
                              tiBolsaEstudo,         // 10 - Bolsa de estudo recebida por médico-residente
                              tiComplAposentadoria   // 11 - Complementação de aposentadoria, correspondente às contribuições efetuadas no período de 01/01/1989 a 31/12/1995
                            );

  TindPerReferencia       = ( iprMensal,     // 1 - Folha de Pagamento Mensal
                              iprDecTerceiro // 2 - Folha do Décimo Terceiro Salário
                            );

  TindOrigemRecursos      = ( iorProprios, // 1 - Recursos do próprio declarante
                              iorTerceiros // 2 - Recursos de terceiros - Declarante é a Instituição Financeira responsável apenas pelo repasse dos valores
                            );

  TtpRepasse              = ( trPatrocinio,    // 1 - Patrocínio
                              trLicenciamento, // 2 - Licenciamento de marcas e símbolos
                              trPublicidade,   // 3 - Publicidade
                              trPropaganda,    // 4 - Propaganda
                              trTransmissao    // 5 - Transmissão de espetáculos
                            );

  TindCom                 = ( icProdRural,  // 1 - Comercialização da Produção por Prod. Rural PJ/Agroindústria, exceto para entidades executoras do PAA
                              icPAA,        // 8 - Comercialização da Produção para Entidade do Programa de Aquisição de Alimentos - PAA
                              icMercExterno // 9 - Comercialização direta da Produção no Mercado Externo
                            );

  TtpCompeticao           = ( ttcOficial,   // 1 - Oficial
                              ttcnaoOficial // 2 - Não Oficial
                            );

  TcategEvento            = ( tceInternacional,  // 1 - Internacional
                              tceInterestadual,  // 2 - Interestadual
                              tceEstadual,       // 3 - Estadual
                              tceLocal           // 4 - Local
                            );

  TtpIngresso             = ( ttiArquibancada, // 1 - Arquibancada
                              ttiGeral,        // 2 - Geral
                              ttiCadeiras,     // 3 - Cadeiras
                              ttiCamarote      // 4 - Camarote
                            );

  TtpReceita              = ( ttrTransmissso, // 1 - Transmissão
                              ttrPropaganda,  // 2 - Propaganda
                              ttrPublicidade, // 3 - Publicidade
                              ttrSorteio,     // 4 - Sorteio
                              ttrOutros       // 5 - Outros
                            );

  TVersaoReinf            = ( v1_02_00, // v1.2
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

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutReinf;

function SchemaReinfToStr(const t: TReinfSchema): String;

function LayOutReinfToSchema(const t: TLayOutReinf): TReinfSchema;
function LayOutReinfToServico(const t: TLayOutReinf): String;

function VersaoReinfToDbl(const t: TVersaoReinf): Real;
function VersaoReinfToStr(const t: TVersaoReinf): String;

function TpInscricaoToStr(const t: TtpInsc ): string;
function StrToTpInscricao(var ok: boolean; const s: string): TtpInsc;

function tpAmbReinfToStr(const t: TtpAmb ): string;
function StrTotpAmbReinf(var ok: boolean; const s: string): TtpAmb;

function procEmiReinfToStr(const t: TprocEmi ): string;
function StrToprocEmiReinf(var ok: boolean; const s: string): TprocEmi;

function indEscrituracaoToStr(const t: TindEscrituracao ): string;
function StrToindEscrituracao(var ok: boolean; const s: string): TindEscrituracao;

function indDesoneracaoToStr(const t: TindDesoneracao ): string;
function StrToindDesoneracao(var ok: boolean; const s: string): TindDesoneracao;

function indAcordoIsenMultaToStr(const t: TindAcordoIsenMulta ): string;
function StrToindAcordoIsenMulta(var ok: boolean; const s: string): TindAcordoIsenMulta;

function indSitPJToStr(const t: TindSitPJ ): string;
function StrToindSitPJ(var ok: boolean; const s: string): TindSitPJ;

function SimNaoToStr(const t: TtpSimNao): string;
function StrToSimNao(var ok: boolean; const s: string): TtpSimNao;

function TpProcToStr(const t: TtpProc ): string;
function StrToTpProc(var ok: boolean; const s: string): TtpProc;

function indAutoriaToStr(const t: TindAutoria ): string;
function StrToindAutoria(var ok: boolean; const s: string): TindAutoria;

function IndSuspToStr(const t: TindSusp ): string;
function StrToIndSusp(var ok: boolean; const s: string): TindSusp;

function IndRetificacaoToStr(const t: TIndRetificacao ): string;
function StrToIndRetificacao(var ok: boolean; const s: string): TIndRetificacao;

function indObraToStr(const t: TpindObra ): string;
function StrToindObra(var ok: boolean; const s: string): TpindObra;

function indCPRBToStr(const t: TpindCPRB ): string;
function StrToindCPRB(var ok: boolean; const s: string): TpindCPRB;

function tpProcRetPrincToStr(const t: TtpProcRetPrinc ): string;
function StrTotpProcRetPrinc(var ok: boolean; const s: string): TtpProcRetPrinc;

function tpRepasseToStr(const t: TtpRepasse ): string;
function StrTotpRepasse(var ok: boolean; const s: string): TtpRepasse;

function indComToStr(const t: TindCom ): string;
function StrToindCom(var ok: boolean; const s: string): TindCom;

function tpAjusteToStr(const t: TtpAjuste ): string;
function StrTotpAjuste(var ok: boolean; const s: string): TtpAjuste;

function codAjusteToStr(const t: TcodAjuste ): string;
function StrTocodAjuste(var ok: boolean; const s: string): TcodAjuste;

function indNIFToStr(const t: TindNIF ): string;
function StrToindNIF(var ok: boolean; const s: string): TindNIF;

function indTpDeducaoToStr(const t: TindTpDeducao ): string;
function StrToindTpDeducao(var ok: boolean; const s: string): TindTpDeducao;

function tpIsencaoToStr(const t: TtpIsencao ): string;
function StrTotpIsencao(var ok: boolean; const s: string): TtpIsencao;

function indPerReferenciaToStr(const t: TindPerReferencia ): string;
function StrToindPerReferencia(var ok: boolean; const s: string): TindPerReferencia;

function indOrigemRecursosToStr(const t: TindOrigemRecursos ): string;
function StrToindOrigemRecursos(var ok: boolean; const s: string): TindOrigemRecursos;

function tpCompeticaoToStr(const t: TtpCompeticao ): string;
function StrTotpCompeticao(var ok: boolean; const s: string): TtpCompeticao;

function categEventoToStr(const t: TcategEvento ): string;
function StrTocategEvento(var ok: boolean; const s: string): TcategEvento;

function tpIngressoToStr(const t: TtpIngresso ): string;
function StrTotpIngresso(var ok: boolean; const s: string): TtpIngresso;

function tpReceitaToStr(const t: TtpReceita ): string;
function StrTotpReceita(var ok: boolean; const s: string): TtpReceita;

function indExistInfoToStr(const t: TindExistInfo ): string;
function StrToindExistInfo(var ok: boolean; const s: string): TindExistInfo;

implementation

uses
  pcnConversao, typinfo;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutReinf;
begin
   Result := StrToEnumerado(ok, s,
    ['EnviarLoteEventos', 'ConsultarLoteEventos'],
    [ LayEnvioLoteEventos, LayConsultaLoteEventos ] );
end;

function SchemaReinfToStr(const t: TReinfSchema): String;
begin
  Result := GetEnumName(TypeInfo(TReinfSchema), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function LayOutReinfToSchema(const t: TLayOutReinf): TReinfSchema;
begin
   case t of
    LayEnvioLoteEventos:    Result := schEnvioLoteEventos;
    LayConsultaLoteEventos: Result := schConsultaLoteEventos;
  else
    Result := schErro;
  end;
end;

function LayOutReinfToServico(const t: TLayOutReinf): String;
begin
   Result := EnumeradoToStr(t,
    ['EnviarLoteEventos', 'ConsultarLoteEventos'],
    [ LayEnvioLoteEventos, LayConsultaLoteEventos ] );
end;

function VersaoReinfToDbl(const t: TVersaoReinf): Real;
begin
  case t of
    v1_02_00: result := 1.02;
    v1_03_00: result := 1.03;
  else
    result := 0;
  end;
end;

function VersaoReinfToStr(const t: TVersaoReinf): String;
begin
  result := EnumeradoToStr(t, ['1_02_00', '1_03_00'], [v1_02_00, v1_03_00]);
end;

function TpInscricaoToStr(const t:TtpInsc ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '4'] );
end;

function StrToTpInscricao(var ok: boolean; const s: string): TtpInsc;
begin
  result := TtpInsc( StrToEnumerado2(ok , s, ['1', '2', '4'] ) );
end;

function tpAmbReinfToStr(const t: TtpAmb ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1', '2', '3']);
end;

function StrTotpAmbReinf(var ok: boolean; const s: string): TtpAmb;
begin
  result := TtpAmb( StrToEnumerado2(ok , s, ['0', '1', '2', '3']) );
end;

function ProcEmiReinfToStr(const t: TprocEmi ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1', '2']);
end;

function StrToProcEmiReinf(var ok: boolean; const s: string): TprocEmi;
begin
  result := TprocEmi( StrToEnumerado2(ok , s, ['0', '1', '2']) );
end;

function indEscrituracaoToStr(const t: TindEscrituracao ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1']);
end;

function StrToindEscrituracao(var ok: boolean; const s: string): TindEscrituracao;
begin
  result := TindEscrituracao( StrToEnumerado2(ok , s, ['0', '1']) );
end;

function indDesoneracaoToStr(const t: TindDesoneracao ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1']);
end;

function StrToindDesoneracao(var ok: boolean; const s: string): TindDesoneracao;
begin
  result := TindDesoneracao( StrToEnumerado2(ok , s, ['0', '1']) );
end;

function indAcordoIsenMultaToStr(const t: TindAcordoIsenMulta ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1']);
end;

function StrToindAcordoIsenMulta(var ok: boolean; const s: string): TindAcordoIsenMulta;
begin
  result := TindAcordoIsenMulta( StrToEnumerado2(ok , s, ['0', '1']) );
end;

function indSitPJToStr(const t: TindSitPJ ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1', '2', '3', '4']);
end;

function StrToindSitPJ(var ok: boolean; const s: string): TindSitPJ;
begin
  result := TindSitPJ( StrToEnumerado2(ok , s, ['0', '1', '2', '3', '4']) );
end;

function SimNaoToStr(const t: TtpSimNao): string;
begin
  result := EnumeradoToStr2(t, ['S', 'N']);
end;

function StrToSimNao(var ok: boolean; const s: string): TtpSimNao;
begin
  result := TtpSimNao( StrToEnumerado2(ok , s, ['S', 'N']) );
end;

function TpProcToStr(const t: TtpProc ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrToTpProc(var ok: boolean; const s: string): TtpProc;
begin
  result := TtpProc( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function indAutoriaToStr(const t: TindAutoria ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrToindAutoria(var ok: boolean; const s: string): TindAutoria;
begin
  result := TindAutoria( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function IndSuspToStr(const t: TindSusp ): string;
begin
  result := EnumeradoToStr2(t, ['01', '04', '05', '08', '09', '10', '11', '12',
                                '13', '90', '92']);
end;

function StrToIndSusp(var ok: boolean; const s: string): TindSusp;
begin
  result := TindSusp( StrToEnumerado2(ok , s, ['01', '04', '05', '08', '09',
                                                '10', '11', '12', '13', '90',
                                                '92']) );
end;

function IndRetificacaoToStr(const t: TIndRetificacao ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrToIndRetificacao(var ok: boolean; const s: string): TIndRetificacao;
begin
  result := TIndRetificacao( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function indObraToStr(const t: TpindObra ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1', '2']);
end;

function StrToindObra(var ok: boolean; const s: string): TpindObra;
begin
  result := TpindObra( StrToEnumerado2(ok , s, ['0', '1', '2']) );
end;

function indCPRBToStr(const t: TpindCPRB ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1']);
end;

function StrToindCPRB(var ok: boolean; const s: string): TpindCPRB;
begin
  result := TpindCPRB( StrToEnumerado2(ok , s, ['0', '1']) );
end;

function tpProcRetPrincToStr(const t: TtpProcRetPrinc ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3']);
end;

function StrTotpProcRetPrinc(var ok: boolean; const s: string): TtpProcRetPrinc;
begin
  result := TtpProcRetPrinc( StrToEnumerado2(ok , s, ['1', '2', '3']) );
end;

function tpRepasseToStr(const t: TtpRepasse ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '5']);
end;

function StrTotpRepasse(var ok: boolean; const s: string): TtpRepasse;
begin
  result := TtpRepasse( StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5']) );
end;

function indComToStr(const t: TindCom ): string;
begin
  result := EnumeradoToStr2(t, ['1', '8', '9']);
end;

function StrToindCom(var ok: boolean; const s: string): TindCom;
begin
  result := TindCom( StrToEnumerado2(ok , s, ['1', '8', '9']) );
end;

function tpAjusteToStr(const t: TtpAjuste ): string;
begin
  result := EnumeradoToStr2(t, ['0', '1']);
end;

function StrTotpAjuste(var ok: boolean; const s: string): TtpAjuste;
begin
  result := TtpAjuste( StrToEnumerado2(ok , s, ['0', '1']) );
end;

function codAjusteToStr(const t: TcodAjuste ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                                '10', '11']);
end;

function StrTocodAjuste(var ok: boolean; const s: string): TcodAjuste;
begin
  result := TcodAjuste( StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5', '6',
                                                 '7', '8', '9', '10', '11']) );
end;

function indNIFToStr(const t: TindNIF ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3']);
end;

function StrToindNIF(var ok: boolean; const s: string): TindNIF;
begin
  result := TindNIF( StrToEnumerado2(ok , s, ['1', '2', '3']) );
end;

function indTpDeducaoToStr(const t: TindTpDeducao ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '5', '6']);
end;

function StrToindTpDeducao(var ok: boolean; const s: string): TindTpDeducao;
begin
  result := TindTpDeducao( StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5', '6']) );
end;

function tpIsencaoToStr(const t: TtpIsencao ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                                '10', '11']);
end;

function StrTotpIsencao(var ok: boolean; const s: string): TtpIsencao;
begin
  result := TtpIsencao( StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5', '6',
                                                 '7', '8', '9', '10', '11']) );
end;

function indPerReferenciaToStr(const t: TindPerReferencia ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrToindPerReferencia(var ok: boolean; const s: string): TindPerReferencia;
begin
  result := TindPerReferencia( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function indOrigemRecursosToStr(const t: TindOrigemRecursos ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrToindOrigemRecursos(var ok: boolean; const s: string): TindOrigemRecursos;
begin
  result := TindOrigemRecursos( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function tpCompeticaoToStr(const t: TtpCompeticao ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2']);
end;

function StrTotpCompeticao(var ok: boolean; const s: string): TtpCompeticao;
begin
  result := TtpCompeticao( StrToEnumerado2(ok , s, ['1', '2']) );
end;

function categEventoToStr(const t: TcategEvento ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4']);
end;

function StrTocategEvento(var ok: boolean; const s: string): TcategEvento;
begin
  result := TcategEvento( StrToEnumerado2(ok , s, ['1', '2', '3', '4']) );
end;

function tpIngressoToStr(const t: TtpIngresso ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4']);
end;

function StrTotpIngresso(var ok: boolean; const s: string): TtpIngresso;
begin
  result := TtpIngresso( StrToEnumerado2(ok , s, ['1', '2', '3', '4']) );
end;

function tpReceitaToStr(const t: TtpReceita ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '5']);
end;

function StrTotpReceita(var ok: boolean; const s: string): TtpReceita;
begin
  result := TtpReceita( StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5']) );
end;

function indExistInfoToStr(const t: TindExistInfo ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3']);
end;

function StrToindExistInfo(var ok: boolean; const s: string): TindExistInfo;
begin
  result := TindExistInfo( StrToEnumerado2(ok , s, ['1', '2', '3']) );
end;

end.
