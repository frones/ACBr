{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrNF3eConversao;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type
  TStatusNF3e = (stIdle, stNF3eStatusServico, stNF3eRecepcao,
                 stNF3eRetRecepcao, stNF3eRecibo, stNF3eConsulta,
                 stNF3eInutilizacao, stNF3eEvento, stDistDFeInt,
                 stEnvioWebService, stNF3eEmail);

type
  TVersaoNF3e = (ve100);

const
  TVersaoNF3eArrayStrings: array[TVersaoNF3e] of string = ('1.00');
  TVersaoNF3eArrayDouble: array[TVersaoNF3e] of Double = (1.00);

type
  TSchemaNF3e = (schErro, schconsStatServNF3e, schNF3e, schconsReciNF3e,
                 schconsSitNF3e, schInutNF3e, schenvEvento,
                 schCancNF3e);

const
  TSchemaNF3eArrayStrings: array[TSchemaNF3e] of string = ('', '', '', '',
    '', '', '', 'evCancNF3e');

type
  TLayOut = (LayNF3eStatusServico, LayNF3eRecepcao, LayNF3eRecepcaoSinc,
             LayNF3eRetRecepcao, LayNF3eConsulta, LayNF3eEvento,
             LayNF3eQRCode, LayNF3eURLConsulta);

const
  TLayOutNF3eArrayStrings: array[TLayOut] of string = ('NF3eStatusServico',
    'NF3eRecepcao', 'NF3eRecepcaoSinc', 'NF3eRetRecepcao', 'NF3eConsulta',
    'NFComRecepcaoEvento', 'URL-QRCode', 'URL-ConsultaNF3e');

type
  TVersaoQrCode = (veqr000, veqr100, veqr200);

const
  TVersaoQrCodeArrayStrings: array[TVersaoQrCode] of string = ('0', '1', '2');
  TVersaoQrCodeArrayDouble: array[TVersaoQrCode] of Double = (0, 1, 2);

type
  TSiteAutorizador = (sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9);

const
  TSiteAutorizadorArrayStrings: array[TSiteAutorizador] of string = ('0','1', '2',
    '3', '4', '5', '6', '7', '8', '9');

type
  TFinalidadeNF3e = (fnNormal, fnSubstituicao, fnAjuste);

const
  TFinalidadeNF3eArrayStrings: array[TFinalidadeNF3e] of string = ('1', '2',
    '3');

type
  TtpAcesso = (taGerador, taCativo, taLivre, taParcialmenteLivre,
               taConsumidorEspecial, taParcialmenteEspecial, taComunhao,
               taSuprimento, taDistribuidora);

const
  TtpAcessoArrayStrings: array[TtpAcesso] of string = ('0', '1', '2', '3', '4',
    '5', '6', '7', '8');

type
  TtpClasse = (tcComercial, tcConsumidorProprio, tcIluminacaoPublica,
               tcIndustrial, tcPoderPublico, tcResidencial, tcRural,
               tcServicoPublico);

const
  TtpClasseArrayStrings: array[TtpClasse] of string = ('01', '02', '03', '04',
    '05', '06', '07', '08');
  TtpClasseDescArrayStrings: array[TtpClasse] of string = ('Comercial',
    'Consumo Proprio', 'Iluminacao Publica', 'Industrial', 'Poder Publico',
    'Residencial', 'Rural', 'Servico Publico');

type
  TtpSubClasse = (tscResidencial, tscResidBaixaRenda,
                  tscResidBaixaRendaIndigena, tscResidBaixaRendaQuilombola,
                  tscResidBaixaRendaAssitSocial, tscResidBaixaRendaMultifamiliar,
                  tscComercial, tscServTransporte, tscServComunicacao,
                  tscAssociacao, tscTemplosReligiosos, tscAdmCondominial,
                  tscIluminacaoRodovias, tscSermafaros, tscOutrosServicos,
                  tscAgropecuariaRural, tscAgropecuariaUrbana,
                  tscResidenciaRural, tscCooperativaEletrifRural,
                  tscAgroindustria, tscServPublIrrigacaoRural, tscEscolaAgrotecnica,
                  tscAquicultura, tscPoderPublicoFederal, tscPoderPublicoEstadual,
                  tscPoderPublicoMunicipal, tscTracaoEletrica, tscAguaEsgoto,
                  tscOutros);

const
  TtpSubClasseArrayStrings: array[TtpSubClasse] of string = ('01', '02', '03',
    '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16',
    '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '99');

type
  TtpFase = (tfMonofasico, tfBifasico, tfTrifasico);

const
  TtpFaseArrayStrings: array[TtpFase] of string = ('1', '2', '3');
  TtpFaseDescArrayStrings: array[TtpFase] of string = ('Monofasico', 'Bifasico',
    'Trifasico');

type
  TtpGrpTensao = (tgtA1, tgtA2, tgtA3, tgtA3a, tgtA4, tgtAS, tgtB1,
                  tgtB1BaixaRenda, tgtB2, tgtB2Cooperativa, tgtB2ServicoPublico,
                  tgtB3, tgtB4a, tgtB4b);

const
  TtpGrpTensaoArrayStrings: array[TtpGrpTensao] of string = ('01', '02', '03',
    '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14');

type
  TtpModTar = (tmtConvencionalMonomia, tmtConvencionalBinomia, tmtHorariaAzul,
               tmtHorariaAzulAPE, tmtHorariaVerde, tmtHorariaVerdeAPE,
               tmtHorariaBranca, tmtPrePagamento, tmtGeracao, tmtDistribuicao);

const
  TtpModTarArrayStrings: array[TtpModTar] of string = ('01', '02', '03', '04',
    '05', '06', '07', '08', '09', '10');

type
  TmotSub = (msErroLeitura, msErroPreco, msDecisaoJudicial,
             msErroCadastral, msErroTributacao);

const
  TmotSubArrayStrings: array[TmotSub] of string = ('01', '02', '03', '04', '05');

type
  TtpGrContrat = (tgDemanda, tgMontante, tgReserva, tgEnergia);

const
  TtpGrContratArrayStrings: array[TtpGrContrat] of string = ('1', '2', '3', '4');

type
  TtpPosTar = (tpUnico, tpPonta, tpForaPonto, tpIntermediario);

const
  TtpPosTarArrayStrings: array[TtpPosTar] of string = ('0', '1', '2', '3');

type
  TtpPartComp = (tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada,
                 tpMista);

const
  TtpPartCompArrayStrings: array[TtpPartComp] of string = ('1', '2', '3', '4',
    '5');

type
  TtpAjuste = (taNenhum, taItemaserSubstituido, taItemSubstituicao, taItemEliminado,
               taItemIncluido);

const
  TtpAjusteArrayStrings: array[TtpAjuste] of string = ('0', '1', '2', '3', '4');

type
  TmotAjuste = (maErroLeitura, maErroPreco, maDecisaoJudicial,
                maErroCadastral, maErroTributacao);

const
  TmotAjusteArrayStrings: array[TmotAjuste] of string = ('1', '2', '3', '4', '5');

type
  TtpAto = (taREH, taDespacho, taREN);

const
  TtpAtoArrayStrings: array[TtpAto] of string = ('1', '2', '3');

type
  TtpTarif = (ttTE, ttTUSD);

const
  TtpTarifArrayStrings: array[TtpTarif] of string = ('1', '2');

type
  TcPosTarif = (tptUnico, tptPonta, tptForaPonta, tptIntermediario,
                tptPontaReservado, tptForaPontoReservado,
                tptItermediarioReservado, tptReservado);

const
  TcPosTarifArrayStrings: array[TcPosTarif] of string = ('0', '1', '2', '3', '4',
    '5', '6', '7');

type
  TuMed = (umkW, umkWh);

const
  TuMedArrayStrings: array[TuMed] of string = ('1', '2');

type
  TuMedFat = (umfkW, umfkWh, umfkVAr, umfkVArh);

const
  TuMedFatArrayStrings: array[TuMedFat] of string = ('1', '2', '3', '4');
  TuMedFatDescArrayStrings: array[TuMedFat] of string = ('kW', 'kWh', 'kVAr',
    'kVArh');

type
  TmotDifTarif = (mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto,
                  mdtAlteracao);

const
  TmotDifTarifArrayStrings: array[TmotDifTarif] of string = ('01', '02', '03',
    '04');

type
  TtpBand = (tbVerde, tbAmarela, tbVermelha1, tbVermelha2, tbEscassez);

const
  TtpBandArrayStrings: array[TtpBand] of string = ('1', '2', '3', '4', '5');

type
  TmotDifBand = (mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto,
                 mdbAlteracao);

const
  TmotDifBandArrayStrings: array[TmotDifBand] of string = ('01', '02', '03', '04');

type
  TindOrigemQtd = (ioMedia, ioMedido, ioContatada, ioCalculada, ioCusto,
                   ioSemQuantidade);

const
  TindOrigemQtdArrayStrings: array[TindOrigemQtd] of string = ('1', '2', '3', '4',
    '5', '6');

type
  TtpGrMed = (tgmDemanda, tgmDemandaReativa, tgmEnergiaAtiva,
              tgmEnergiaAtivaInjetada, tgmEnergiaReativa);

const
  TtpGrMedArrayStrings: array[TtpGrMed] of string = ('01', '02', '03', '04', '05');

type
  TtpMotNaoLeitura = (tmNenhum, tmConsumidor, tmDistribuidora, tmIndependente);

const
  TtpMotNaoLeituraArrayStrings: array[TtpMotNaoLeitura] of string = ('0', '1',
    '2', '3');

type
  TtpProc = (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual);

const
  TtpProcArrayStrings: array[TtpProc] of string = ('0', '1', '2');

type
  TtpLanc = (tlDebito, tlCredito);

const
  TtpLancArrayStrings: array[TtpLanc] of string = ('D', 'C');

type
  TtpFonteEnergia = (feHidraulica, feSolar, feEolica, feBiomassa, feBiogas,
                     feHibrida);

const
  TtpFonteEnergiaArrayStrings: array[TtpFonteEnergia] of string = ('1', '2', '3',
    '4', '5', '6');

type
  TIndicador = (tiSim, tiNao);

const
  TIndicadorArrayStrings: array[TIndicador] of string = ('1', '0');

type
  TCSTCofins = (cof01, cof02, cof03, cof04, cof05, cof06, cof07, cof08, cof09,
                cof49, cof50, cof51, cof52, cof53, cof54, cof55, cof56, cof60,
                cof61, cof62, cof63, cof64, cof65, cof66, cof67, cof70, cof71,
                cof72, cof73, cof74, cof75, cof98, cof99);

const
  TCSTCofinsArrayStrings: array[TCSTCofins] of string = ('01', '02', '03', '04',
    '05', '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56',
    '60', '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74',
    '75', '98', '99');
  TCSTCofinsDescArrayStrings: array[TCSTCofins] of string = (
    '01 - Operação Tributável com Alíquota Básica',
    '02 - Operação Tributável com Alíquota Diferenciada',
    '03 - Operação Tributável com Alíquota por Unidade de Medida de Produto',
    '04 - Operação Tributável Monofásica - Revenda a Alíquota Zero',
    '05 - Operação Tributável por Substituição Tributária',
    '06 - Operação Tributável a Alíquota Zero',
    '07 - Operação Isenta da Contribuição',
    '08 - Operação sem Incidência da Contribuição',
    '09 - Operação com Suspensão da Contribuição',
    '49 - Outras Operações de Saída',
    '50 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
    '51 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Não Tributada no Mercado Interno',
    '52 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação',
    '53 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
    '54 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
    '55 - Operação com Direito a Crédito - Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
    '56 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
    '60 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
    '61 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno',
    '62 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação',
    '63 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
    '64 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
    '65 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
    '66 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
    '67 - Crédito Presumido - Outras Operações',
    '70 - Operação de Aquisição sem Direito a Crédito',
    '71 - Operação de Aquisição com Isenção',
    '72 - Operação de Aquisição com Suspensão',
    '73 - Operação de Aquisição a Alíquota Zero',
    '74 - Operação de Aquisição sem Incidência da Contribuição',
    '75 - Operação de Aquisição por Substituição Tributária',
    '98 - Outras Operações de Entrada',
    '99 - Outras Operações');

type
  TCSTPis = (pis01, pis02, pis03, pis04, pis05, pis06, pis07, pis08, pis09,
             pis49, pis50, pis51, pis52, pis53, pis54, pis55, pis56, pis60,
             pis61, pis62, pis63, pis64, pis65, pis66, pis67, pis70, pis71,
             pis72, pis73, pis74, pis75, pis98, pis99);

const
  TCSTPisArrayStrings: array[TCSTPis] of string = ('01', '02', '03', '04', '05',
    '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56', '60',
    '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74', '75',
    '98', '99');
  TCSTPisDescArrayStrings: array[TCSTPis] of string = (
    '01 - Operação Tributável com Alíquota Básica',
    '02 - Operação Tributável com Alíquota Diferenciada',
    '03 - Operação Tributável com Alíquota por Unidade de Medida de Produto',
    '04 - Operação Tributável Monofásica - Revenda a Alíquota Zero',
    '05 - Operação Tributável por Substituição Tributária',
    '06 - Operação Tributável a Alíquota Zero',
    '07 - Operação Isenta da Contribuição',
    '08 - Operação sem Incidência da Contribuição',
    '09 - Operação com Suspensão da Contribuição',
    '49 - Outras Operações de Saída',
    '50 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
    '51 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Não Tributada no Mercado Interno',
    '52 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação',
    '53 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
    '54 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
    '55 - Operação com Direito a Crédito - Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
    '56 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
    '60 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
    '61 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno',
    '62 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação',
    '63 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
    '64 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
    '65 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
    '66 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
    '67 - Crédito Presumido - Outras Operações',
    '70 - Operação de Aquisição sem Direito a Crédito',
    '71 - Operação de Aquisição com Isenção',
    '72 - Operação de Aquisição com Suspensão',
    '73 - Operação de Aquisição a Alíquota Zero',
    '74 - Operação de Aquisição sem Incidência da Contribuição',
    '75 - Operação de Aquisição por Substituição Tributária',
    '98 - Outras Operações de Entrada',
    '99 - Outras Operações');

type
  TCSTIcms = (cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
              cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
              cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60);

const
  TCSTIcmsArrayStrings: array[TCSTIcms] of string = ('', '00', '10', '20', '30',
    '40', '41', '45', '50', '51', '60', '70', '80', '81', '90', '90', 'SN', '10',
    '90', '41', '60');
  TCSTIcmsDescArrayStrings: array[TCSTIcms] of string = (
    'VAZIO',
    '00 - TRIBUTAÇÃO NORMAL DO ICMS',
    '10 - TRIBUTAÇÃO COM COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '20 - TRIBUTAÇÃO COM REDUÇÃO DE BC DO ICMS',
    '30 - TRIBUTAÇÃO ISENTA E COM COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '40 - ICMS ISENÇÃO',
    '41 - ICMS NÃO TRIBUTADO',
    '45 - ICMS ISENTO, NÃO TRIBUTADO OU DIFERIDO',
    '50 - ICMS SUSPENSÃO',
    '51 - ICMS DIFERIDO',
    '60 - ICMS COBRADO POR SUBSTITUIÇÃO TRIBUTÁRIA',
    '70 - TRIBUTAÇÃO COM REDUÇÃO DE BC E COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '80 - RESPONSABILIDADE DO RECOLHIMENTO DO ICMS ATRIBUÍDO AO TOMADOR OU 3° POR ST',
    '81 - ICMS DEVIDO À OUTRA UF',
    '90 - ICMS OUTROS',
    '90 - ICMS DEVIDO A UF DE ORIGEM DA PRESTACAO, QUANDO DIFERENTE DA UF DO EMITENTE',
    '90 - SIMPLES NACIONAL',
    '10 - TRIBUTADA E COM COBRANÇA DO ICMS POR SUBSTITUIÇÃO TRIBUTÁRIA - PARTILHA',
    '90 - OUTROS - PARTILHA',
    '41 - NÃO TRIBUTADO - REPASSE',
    '60 - COBRADO ANTERIORMENTE POR SUBSTITUIÇÃO TRIBUTÁRIA - REPASSE');

type
  TindIEDest = (inContribuinte, inIsento, inNaoContribuinte);

const
  TindIEDestArrayStrings: array[TindIEDest] of string = ('1', '2', '9');

{
  Declaração das funções de conversão
}
function LayOutToServico(const t: TLayOut): string;
function ServicoToLayOut(out ok: Boolean; const s: string): TLayOut;

function LayOutToSchema(const t: TLayOut): TSchemaNF3e;

function SchemaNF3eToStr(const t: TSchemaNF3e): string;
function StrToSchemaNF3e(const s: string): TSchemaNF3e;
function SchemaEventoToStr(const t: TSchemaNF3e): string;

function StrToVersaoNF3e(out ok: Boolean; const s: string): TVersaoNF3e;
function VersaoNF3eToStr(const t: TVersaoNF3e): string;

function DblToVersaoNF3e(out ok: Boolean; const d: Real): TVersaoNF3e;
function VersaoNF3eToDbl(const t: TVersaoNF3e): Real;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
function StrToVersaoQrCode(out ok: Boolean; const s: string): TVersaoQrCode;
function VersaoQrCodeToDbl(const t: TVersaoQrCode): Real;

function finNF3eToStr(const t: TFinalidadeNF3e): string;
function StrTofinNF3e(out ok: Boolean; const s: string): TFinalidadeNF3e;

function tpAcessoToStr(const t: TtpAcesso): string;
function StrTotpAcesso(out ok: Boolean; const s: string): TtpAcesso;

function tpClasseToStr(const t: TtpClasse): string;
function StrTotpClasse(out ok: Boolean; const s: string): TtpClasse;
function tpClasseToDesc(const t: TtpClasse): string;

function tpSubClasseToStr(const t: TtpSubClasse): string;
function StrTotpSubClasse(out ok: Boolean; const s: string): TtpSubClasse;

function tpFaseToStr(const t: TtpFase): string;
function StrTotpFase(out ok: Boolean; const s: string): TtpFase;
function tpFaseToDesc(const t: TtpFase): string;

function tpGrpTensaoToStr(const t: TtpGrpTensao): string;
function StrTotpGrpTensao(out ok: Boolean; const s: string): TtpGrpTensao;

function tpModTarToStr(const t: TtpModTar): string;
function StrTotpModTar(out ok: Boolean; const s: string): TtpModTar;

function MotSubToStr(const t: TmotSub): string;
function StrToMotSub(out ok: Boolean; const s: string): TmotSub;

function tpGrContratToStr(const t: TtpGrContrat): string;
function StrTotpGrContrat(out ok: Boolean; const s: string): TtpGrContrat;

function tpPosTarToStr(const t: TtpPosTar): string;
function StrTotpPosTar(out ok: Boolean; const s: string): TtpPosTar;

function tpPartCompToStr(const t: TtpPartComp): string;
function StrTotpPartComp(out ok: Boolean; const s: string): TtpPartComp;

function tpAjusteToStr(const t: TtpAjuste): string;
function StrTotpAjuste(out ok: Boolean; const s: string): TtpAjuste;

function MotAjusteToStr(const t: TmotAjuste): string;
function StrToMotAjuste(out ok: Boolean; const s: string): TmotAjuste;

function tpAtoToStr(const t: TtpAto): string;
function StrTotpAto(out ok: Boolean; const s: string): TtpAto;

function tpTarifToStr(const t: TtpTarif): string;
function StrTotpTarif(out ok: Boolean; const s: string): TtpTarif;

function cPosTarifToStr(const t: TcPosTarif): string;
function StrTocPosTarif(out ok: Boolean; const s: string): TcPosTarif;

function uMedToStr(const t: TuMed): string;
function StrTouMed(out ok: Boolean; const s: string): TuMed;

function uMedFatToStr(const t: TuMedFat): string;
function StrTouMedFat(out ok: Boolean; const s: string): TuMedFat;
function uMedFatToDesc(const t: TuMedFat): string;

function motDifTarifToStr(const t: TmotDifTarif): string;
function StrTomotDifTarif(out ok: Boolean; const s: string): TmotDifTarif;

function tpBandToStr(const t: TtpBand): string;
function StrTotpBand(out ok: Boolean; const s: string): TtpBand;

function motDifBandToStr(const t: TmotDifBand): string;
function StrTomotDifBand(out ok: Boolean; const s: string): TmotDifBand;

function indOrigemQtdToStr(const t: TindOrigemQtd): string;
function StrToindOrigemQtd(out ok: Boolean; const s: string): TindOrigemQtd;

function tpGrMedToStr(const t: TtpGrMed): string;
function StrTotpGrMed(out ok: Boolean; const s: string): TtpGrMed;

function tpMotNaoLeituraToStr(const t: TtpMotNaoLeitura): string;
function StrTotpMotNaoLeitura(out ok: Boolean; const s: string): TtpMotNaoLeitura;

function tpProcToStr(const t: TtpProc): string;
function StrTotpProc(out ok: Boolean; const s: string): TtpProc;

function tpLancToStr(const t: TtpLanc): string;
function StrTotpLanc(out ok: Boolean; const s: string): TtpLanc;

function tpFonteEnergiaToStr(const t: TtpFonteEnergia): string;
function StrTotpFonteEnergia(out ok: Boolean; const s: string): TtpFonteEnergia;

function SiteAutorizadorToStr(const t: TSiteAutorizador): string;
function StrToSiteAutorizator(out ok: Boolean; const s: string): TSiteAutorizador;

function TIndicadorToStr(const t: TIndicador): string;
function StrToTIndicador(out ok: boolean; const s: string): TIndicador;

function CSTCOFINSToStrTagPosText(const t: TCSTCofins): string;
function CSTCOFINSToStr(const t: TCSTCofins): string;
function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;

function CSTPISToStrTagPosText(const t: TCSTPis): string;
function CSTPISToStr(const t: TCSTPis): string;
function StrToCSTPIS(out ok: boolean; const s: string): TCSTPis;

function CSTICMSToStr(const t: TCSTIcms): string;
function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;
function CSTICMSToStrTagPos(const t: TCSTIcms): string;
function CSTICMSToStrTagPosText(const t: TCSTIcms): string;

function indIEDestToStr(const t: TindIEDest ): string;
function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;

function StrToTpEventoNF3e(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo,
  ACBrBase;

function LayOutToServico(const t: TLayOut): string;
begin
  result := TLayOutNF3eArrayStrings[t];
end;

function ServicoToLayOut(out ok: Boolean; const s: string): TLayOut;
begin
  Result := StrToEnumerado(ok, s,
    ['NF3eStatusServico', 'NF3eRecepcao', 'NF3eRecepcaoSinc', 'NF3eRetRecepcao',
     'NF3eConsulta', 'NF3eInutilizacao', 'NF3eRecepcaoEvento', 'NF3eDistribuicaoDFe'],
    [LayNF3eStatusServico, LayNF3eRecepcao, LayNF3eRecepcaoSinc,
     LayNF3eRetRecepcao, LayNF3eConsulta, LayNF3eEvento] );
end;

function LayOutToSchema(const t: TLayOut): TSchemaNF3e;
begin
  case t of
    LayNF3eStatusServico: Result := schconsStatServNF3e;
    LayNF3eRecepcao,
    LayNF3eRecepcaoSinc:  Result := schNF3e;
    LayNF3eRetRecepcao:   Result := schconsReciNF3e;
    LayNF3eConsulta:      Result := schconsSitNF3e;
    LayNF3eEvento:        Result := schenvEvento;
  else
    Result := schErro;
  end;
end;

function SchemaNF3eToStr(const t: TSchemaNF3e): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaNF3e), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNF3e(const s: string): TSchemaNF3e;
var
  P: Integer;
  SchemaStr: string;
  CodSchema: Integer;
begin
  P := pos('_',s);
  if p > 0 then
    SchemaStr := copy(s,1,P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr,3) <> 'sch' then
    SchemaStr := 'sch'+SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaNF3e), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNF3e válido.',[SchemaStr]));
  end;

  Result := TSchemaNF3e( CodSchema );
end;

function StrToVersaoNF3e(out ok: Boolean; const s: string): TVersaoNF3e;
var
  idx: TVersaoNF3e;
begin
  for idx := Low(TVersaoNF3eArrayStrings) to High(TVersaoNF3eArrayStrings) do
  begin
    if (TVersaoNF3eArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoNF3e: %s', [s]);
end;

function VersaoNF3eToStr(const t: TVersaoNF3e): string;
begin
  result := TVersaoNF3eArrayStrings[t];
end;

 function DblToVersaoNF3e(out ok: Boolean; const d: Real): TVersaoNF3e;
 begin
   ok := True;

   if (d = 1.0) then
     Result := ve100
   else
   begin
     Result := ve100;
     ok := False;
   end;
 end;

 function VersaoNF3eToDbl(const t: TVersaoNF3e): Real;
 begin
   case t of
     ve100: Result := 1.00;
   else
     Result := 0;
   end;
 end;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
begin
  result := TVersaoQrCodeArrayStrings[t];
end;

function StrToVersaoQrCode(out ok: Boolean; const s: string): TVersaoQrCode;
var
  idx: TVersaoQrCode;
begin
  for idx := Low(TVersaoQrCodeArrayStrings) to High(TVersaoQrCodeArrayStrings) do
  begin
    if (TVersaoQrCodeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoQrCode: %s', [s]);
end;

function VersaoQrCodeToDbl(const t: TVersaoQrCode): Real;
begin
  case t of
    veqr000: Result := 0;
    veqr100: Result := 1;
    veqr200: Result := 2;
  else
    Result := 0;
  end;
end;

function SchemaEventoToStr(const t: TSchemaNF3e): string;
begin
  result := TSchemaNF3eArrayStrings[t];
end;

function finNF3eToStr(const t: TFinalidadeNF3e): string;
begin
  result := TFinalidadeNF3eArrayStrings[t];
end;

function StrTofinNF3e(out ok: Boolean; const s: string): TFinalidadeNF3e;
var
  idx: TFinalidadeNF3e;
begin
  for idx := Low(TFinalidadeNF3eArrayStrings) to High(TFinalidadeNF3eArrayStrings) do
  begin
    if (TFinalidadeNF3eArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TFinalidadeNF3e: %s', [s]);
end;

function tpAcessoToStr(const t: TtpAcesso): string;
begin
  result := TtpAcessoArrayStrings[t];
end;

function StrTotpAcesso(out ok: Boolean; const s: string): TtpAcesso;
var
  idx: TtpAcesso;
begin
  for idx := Low(TtpAcessoArrayStrings) to High(TtpAcessoArrayStrings) do
  begin
    if (TtpAcessoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAcesso: %s', [s]);
end;

function tpClasseToStr(const t: TtpClasse): string;
begin
  result := TtpClasseArrayStrings[t];
end;

function StrTotpClasse(out ok: Boolean; const s: string): TtpClasse;
var
  idx: TtpClasse;
begin
  for idx := Low(TtpClasseArrayStrings) to High(TtpClasseArrayStrings) do
  begin
    if (TtpClasseArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpClasse: %s', [s]);
end;

function tpClasseToDesc(const t: TtpClasse): string;
begin
  result := TtpClasseDescArrayStrings[t];
end;

function tpSubClasseToStr(const t: TtpSubClasse): string;
begin
  result := TtpSubClasseArrayStrings[t];
end;

function StrTotpSubClasse(out ok: Boolean; const s: string): TtpSubClasse;
var
  idx: TtpSubClasse;
begin
  for idx := Low(TtpSubClasseArrayStrings) to High(TtpSubClasseArrayStrings) do
  begin
    if (TtpSubClasseArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpSubClasse: %s', [s]);
end;

function tpFaseToStr(const t: TtpFase): string;
begin
  result := TtpFaseArrayStrings[t];
end;

function StrTotpFase(out ok: Boolean; const s: string): TtpFase;
var
  idx: TtpFase;
begin
  for idx := Low(TtpFaseArrayStrings) to High(TtpFaseArrayStrings) do
  begin
    if (TtpFaseArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpFase: %s', [s]);
end;

function tpFaseToDesc(const t: TtpFase): string;
begin
  result := TtpFaseDescArrayStrings[t];
end;

function tpGrpTensaoToStr(const t: TtpGrpTensao): string;
begin
  result := TtpGrpTensaoArrayStrings[t];
end;

function StrTotpGrpTensao(out ok: Boolean; const s: string): TtpGrpTensao;
var
  idx: TtpGrpTensao;
begin
  for idx := Low(TtpGrpTensaoArrayStrings) to High(TtpGrpTensaoArrayStrings) do
  begin
    if (TtpGrpTensaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpGrpTensao: %s', [s]);
end;

function tpModTarToStr(const t: TtpModTar): string;
begin
  result := TtpModTarArrayStrings[t];
end;

function StrTotpModTar(out ok: Boolean; const s: string): TtpModTar;
var
  idx: TtpModTar;
begin
  for idx := Low(TtpModTarArrayStrings) to High(TtpModTarArrayStrings) do
  begin
    if (TtpModTarArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpModTar: %s', [s]);
end;

function MotSubToStr(const t: TmotSub): string;
begin
  result := TmotSubArrayStrings[t];
end;

function StrToMotSub(out ok: Boolean; const s: string): TmotSub;
var
  idx: TmotSub;
begin
  for idx := Low(TmotSubArrayStrings) to High(TmotSubArrayStrings) do
  begin
    if (TmotSubArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotSub: %s', [s]);
end;

function tpGrContratToStr(const t: TtpGrContrat): string;
begin
  result := TtpGrContratArrayStrings[t];
end;

function StrTotpGrContrat(out ok: Boolean; const s: string): TtpGrContrat;
var
  idx: TtpGrContrat;
begin
  for idx := Low(TtpGrContratArrayStrings) to High(TtpGrContratArrayStrings) do
  begin
    if (TtpGrContratArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpGrContrat: %s', [s]);
end;

function tpPosTarToStr(const t: TtpPosTar): string;
begin
  result := TtpPosTarArrayStrings[t];
end;

function StrTotpPosTar(out ok: Boolean; const s: string): TtpPosTar;
var
  idx: TtpPosTar;
begin
  for idx := Low(TtpPosTarArrayStrings) to High(TtpPosTarArrayStrings) do
  begin
    if (TtpPosTarArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpPosTar: %s', [s]);
end;

function tpPartCompToStr(const t: TtpPartComp): string;
begin
  result := TtpPartCompArrayStrings[t];
end;

function StrTotpPartComp(out ok: Boolean; const s: string): TtpPartComp;
var
  idx: TtpPartComp;
begin
  for idx := Low(TtpPartCompArrayStrings) to High(TtpPartCompArrayStrings) do
  begin
    if (TtpPartCompArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpPartComp: %s', [s]);
end;

function tpAjusteToStr(const t: TtpAjuste): string;
begin
  result := TtpAjusteArrayStrings[t];
end;

function StrTotpAjuste(out ok: Boolean; const s: string): TtpAjuste;
var
  idx: TtpAjuste;
begin
  for idx := Low(TtpAjusteArrayStrings) to High(TtpAjusteArrayStrings) do
  begin
    if (TtpAjusteArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAjuste: %s', [s]);
end;

function MotAjusteToStr(const t: TmotAjuste): string;
begin
  result := TmotAjusteArrayStrings[t];
end;

function StrToMotAjuste(out ok: Boolean; const s: string): TmotAjuste;
var
  idx: TmotAjuste;
begin
  for idx := Low(TmotAjusteArrayStrings) to High(TmotAjusteArrayStrings) do
  begin
    if (TmotAjusteArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotAjuste: %s', [s]);
end;

function tpAtoToStr(const t: TtpAto): string;
begin
  result := TtpAtoArrayStrings[t];
end;

function StrTotpAto(out ok: Boolean; const s: string): TtpAto;
var
  idx: TtpAto;
begin
  for idx := Low(TtpAtoArrayStrings) to High(TtpAtoArrayStrings) do
  begin
    if (TtpAtoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAto: %s', [s]);
end;

function tpTarifToStr(const t: TtpTarif): string;
begin
  result := TtpTarifArrayStrings[t];
end;

function StrTotpTarif(out ok: Boolean; const s: string): TtpTarif;
var
  idx: TtpTarif;
begin
  for idx := Low(TtpTarifArrayStrings) to High(TtpTarifArrayStrings) do
  begin
    if (TtpTarifArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpTarif: %s', [s]);
end;

function cPosTarifToStr(const t: TcPosTarif): string;
begin
  result := TcPosTarifArrayStrings[t];
end;

function StrTocPosTarif(out ok: Boolean; const s: string): TcPosTarif;
var
  idx: TcPosTarif;
begin
  for idx := Low(TcPosTarifArrayStrings) to High(TcPosTarifArrayStrings) do
  begin
    if (TcPosTarifArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TcPosTarif: %s', [s]);
end;

function uMedToStr(const t: TuMed): string;
begin
  result := TuMedArrayStrings[t];
end;

function StrTouMed(out ok: Boolean; const s: string): TuMed;
var
  idx: TuMed;
begin
  for idx := Low(TuMedArrayStrings) to High(TuMedArrayStrings) do
  begin
    if (TuMedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TuMed: %s', [s]);
end;

function uMedFatToStr(const t: TuMedFat): string;
begin
  result := TuMedFatArrayStrings[t];
end;

function StrTouMedFat(out ok: Boolean; const s: string): TuMedFat;
var
  idx: TuMedFat;
begin
  for idx := Low(TuMedFatArrayStrings) to High(TuMedFatArrayStrings) do
  begin
    if (TuMedFatArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TuMedFat: %s', [s]);
end;

function uMedFatToDesc(const t: TuMedFat): string;
begin
  result := TuMedFatDescArrayStrings[t];
end;

function motDifTarifToStr(const t: TmotDifTarif): string;
begin
  result := TmotDifTarifArrayStrings[t];
end;

function StrTomotDifTarif(out ok: Boolean; const s: string): TmotDifTarif;
var
  idx: TmotDifTarif;
begin
  for idx := Low(TmotDifTarifArrayStrings) to High(TmotDifTarifArrayStrings) do
  begin
    if (TmotDifTarifArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotDifTarif: %s', [s]);
end;

function tpBandToStr(const t: TtpBand): string;
begin
  result := TtpBandArrayStrings[t];
end;

function StrTotpBand(out ok: Boolean; const s: string): TtpBand;
var
  idx: TtpBand;
begin
  for idx := Low(TtpBandArrayStrings) to High(TtpBandArrayStrings) do
  begin
    if (TtpBandArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpBand: %s', [s]);
end;

function motDifBandToStr(const t: TmotDifBand): string;
begin
  result := TmotDifBandArrayStrings[t];
end;

function StrTomotDifBand(out ok: Boolean; const s: string): TmotDifBand;
var
  idx: TmotDifBand;
begin
  for idx := Low(TmotDifBandArrayStrings) to High(TmotDifBandArrayStrings) do
  begin
    if (TmotDifBandArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotDifBand: %s', [s]);
end;

function indOrigemQtdToStr(const t: TindOrigemQtd): string;
begin
  result := TindOrigemQtdArrayStrings[t];
end;

function StrToindOrigemQtd(out ok: Boolean; const s: string): TindOrigemQtd;
var
  idx: TindOrigemQtd;
begin
  for idx := Low(TindOrigemQtdArrayStrings) to High(TindOrigemQtdArrayStrings) do
  begin
    if (TindOrigemQtdArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TindOrigemQtd: %s', [s]);
end;

function tpGrMedToStr(const t: TtpGrMed): string;
begin
  result := TtpGrMedArrayStrings[t];
end;

function StrTotpGrMed(out ok: Boolean; const s: string): TtpGrMed;
var
  idx: TtpGrMed;
begin
  for idx := Low(TtpGrMedArrayStrings) to High(TtpGrMedArrayStrings) do
  begin
    if (TtpGrMedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpGrMed: %s', [s]);
end;

function tpMotNaoLeituraToStr(const t: TtpMotNaoLeitura): string;
begin
  result := TtpMotNaoLeituraArrayStrings[t];
end;

function StrTotpMotNaoLeitura(out ok: Boolean; const s: string): TtpMotNaoLeitura;
var
  idx: TtpMotNaoLeitura;
begin
  for idx := Low(TtpMotNaoLeituraArrayStrings) to High(TtpMotNaoLeituraArrayStrings) do
  begin
    if (TtpMotNaoLeituraArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpMotNaoLeitura: %s', [s]);
end;

function tpProcToStr(const t: TtpProc): string;
begin
  result := TtpProcArrayStrings[t];
end;

function StrTotpProc(out ok: Boolean; const s: string): TtpProc;
var
  idx: TtpProc;
begin
  for idx := Low(TtpProcArrayStrings) to High(TtpProcArrayStrings) do
  begin
    if (TtpProcArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpProc: %s', [s]);
end;

function tpLancToStr(const t: TtpLanc): string;
begin
  result := TtpLancArrayStrings[t];
end;

function StrTotpLanc(out ok: Boolean; const s: string): TtpLanc;
var
  idx: TtpLanc;
begin
  for idx := Low(TtpLancArrayStrings) to High(TtpLancArrayStrings) do
  begin
    if (TtpLancArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpLanc: %s', [s]);
end;

function tpFonteEnergiaToStr(const t: TtpFonteEnergia): string;
begin
  result := TtpFonteEnergiaArrayStrings[t];
end;

function StrTotpFonteEnergia(out ok: Boolean; const s: string): TtpFonteEnergia;
var
  idx: TtpFonteEnergia;
begin
  for idx := Low(TtpFonteEnergiaArrayStrings) to High(TtpFonteEnergiaArrayStrings) do
  begin
    if (TtpFonteEnergiaArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpFonteEnergia: %s', [s]);
end;

function SiteAutorizadorToStr(const t: TSiteAutorizador): string;
begin
  result := TSiteAutorizadorArrayStrings[t];
end;

function StrToSiteAutorizator(out ok: Boolean; const s: string): TSiteAutorizador;
var
  idx: TSiteAutorizador;
begin
  for idx := Low(TSiteAutorizadorArrayStrings) to High(TSiteAutorizadorArrayStrings) do
  begin
    if (TSiteAutorizadorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TSiteAutorizador: %s', [s]);
end;

function TIndicadorToStr(const t: TIndicador): string;
begin
  result := TIndicadorArrayStrings[t];
end;

function StrToTIndicador(out ok: boolean; const s: string): TIndicador;
var
  idx: TIndicador;
begin
  for idx := Low(TIndicadorArrayStrings) to High(TIndicadorArrayStrings) do
  begin
    if (TIndicadorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TIndicador: %s', [s]);
end;

function CSTCOFINSToStrTagPosText(const t: TCSTCofins): string;
begin
  result := TCSTCofinsDescArrayStrings[t];
end;

function CSTCOFINSToStr(const t: TCSTCofins): string;
begin
  result := TCSTCofinsArrayStrings[t];
end;

function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;
var
  idx: TCSTCofins;
begin
  for idx := Low(TCSTCofinsArrayStrings) to High(TCSTCofinsArrayStrings) do
  begin
    if (TCSTCofinsArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTCofins: %s', [s]);
end;

function CSTPISToStrTagPosText(const t: TCSTPis): string;
begin
  result := TCSTPisDescArrayStrings[t];
end;

function CSTPISToStr(const t: TCSTPIS): string;
begin
  result := TCSTPisArrayStrings[t];
end;

function StrToCSTPIS(out ok: boolean; const s: string): TCSTPIS;
var
  idx: TCSTPIS;
begin
  for idx := Low(TCSTPISArrayStrings) to High(TCSTPISArrayStrings) do
  begin
    if (TCSTPISArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTPIS: %s', [s]);
end;

function CSTICMSToStr(const t: TCSTIcms): string;
begin
  result := TCSTIcmsArrayStrings[t];
end;

function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;
var
  idx: TCSTIcms;
begin
  for idx := Low(TCSTIcmsArrayStrings) to High(TCSTIcmsArrayStrings) do
  begin
    if (TCSTIcmsArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTIcms: %s', [s]);
end;

function CSTICMSToStrTagPos(const t: TCSTIcms): string;
begin
  result := TCSTIcmsArrayStrings[t];
end;

function CSTICMSToStrTagPosText(const t: TCSTIcms): string;
begin
  result := TCSTIcmsDescArrayStrings[t];
end;

function indIEDestToStr(const t: TindIEDest ): string;
begin
  result := TindIEDestArrayStrings[t];
end;

function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;
var
  idx: TindIEDest;
begin
  for idx := Low(TindIEDestArrayStrings) to High(TindIEDestArrayStrings) do
  begin
    if (TindIEDestArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TindIEDest: %s', [s]);
end;

function StrToTpEventoNF3e(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '240140', '240150', '240170'],
            [teNaoMapeado, teCancelamento, teAutorizadoSubstituicao,
             teAutorizadoAjuste, teLiberacaoPrazoCancelado]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNF3e, 'NF3e');

end.

