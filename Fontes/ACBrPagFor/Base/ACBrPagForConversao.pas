{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrPagForConversao;

interface

uses
  SysUtils, StrUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Classes;

type
  TBanco = (pagNenhum,
     pagBancodoBrasil, pagBancoDigito, pagNuBank,
     pagPagSeguro, pagMercadoPago, pagBradesco, pagSofisaDireto, pagInter, pagItau,
     pagCaixaEconomica, pagSantander, pagOriginal, pagBanCooB, pagVotorantim,
     pagBanrisul, pagSafra, pagBRB, pagUnicredCooperativa, pagBancoRibeiraoPreto,
     pagCetelem, pagSemear, pagPlannerCorretora, pagB3, pagRabobank, pagSicredi,
     pagBNPParibasBrasil, pagUnicredCentralRS, pagKirtonBank, pagPortoCred,
     pagKebHanaBrasil, pagXPInvestimentos, pagBancoXP, pagSuperPagamentos,
     pagGerencianetPagamentos, pagUniprimeNortedoParana, pagCapitalMarkets,
     pagMorganStanley, pagUBSBrasilCCTVM, pagTrevisoCC, pagHipercardBancoMultiplo,
     pagJSafra, pagUniprimeCentral, pagAlfa, pagABNAmro, pagCargill, pagServiCoop,
     pagBradescard, pagNovaFutura, pagGoldmanSachsBrasil, pagCCCNoroesteBrasileiro,
     pagCCMDespTransSCeRS, pagInbursa, pagBancodaAmazonia, pagConfidenceCC,
     pagBancodoEstadodoPara, pagCasaCredito, pagAlbatrossCCV, pagBancoCECRED,
     pagCooperativaCreditoEspiritoSanto, pagBancoBBI, pagBradescoFinanciamentos,
     pagBancoDoNordeste, pagCCBBrasil, pagHSFinanceira, pagLeccaCFI,
     pagKDBBrasil, pagTopazio, pagCCROuro, pagPolocred, pagCCRSaoMigueldoOeste,
     pagICAPBrasil, pagSocred, pagNatixisBrasil, pagCaruana,
     pagCodepeCVC, pagOriginalAgronegocio, pagBancoBrasileiroNegocios,
     pagStandardChartered, pagCresol, pagAgibank, pagBancodaChinaBrasil,
     pagGetMoneyCC, pagBANDEPE, pagConfidenceCambio, pagFinaxis, pagSenff,
     pagMultiMoneyCC, pagBRK, pagBancodoEstadodeSergipe, pagBEXSBancodeCambio,
     pagBRPartners, pagBPP, pagBRLTrustDTVM, pagWesternUniondoBrasil,
     pagParanaBanco, pagBariguiCH, pagBOCOMBBM, pagCapital, pagWooriBank, pagFacta,
     pagStone, pagBrokerBrasilCC, pagMercantil, pagItauBBA, pagTriangulo,
     pagSenso, pagICBCBrasil, pagVipsCC, pagUBSBrasil, pagMSBank, pagMarmetal,
     pagVortx, pagCommerzbank, pagAvista, pagGuittaCC, pagCCRPrimaveraDoLeste,
     pagDacasaFinanceira, pagGenial, pagIBCCTVM, pagBANESTES, pagABCBrasil,
     pagScotiabankBrasil, pagBTGPactual, pagModal, pagClassico, pagGuanabara,
     pagIndustrialdoBrasil, pagCreditSuisse, pagFairCC, pagLaNacionArgentina,
     pagCitibankNA, pagCedula, pagBradescoBERJ, pagJPMorgan, pagCaixaGeralBrasil,
     pagCitibank, pagRodobens, pagFator, pagBNDES, pagAtivaInvestimentos,
     pagBGCLiquidez, pagAlvorada, pagItauConsignado, pagMaxima,
     pagHaitongBi, pagOliveiraTrust, pagBNYMellonBanco, pagPernambucabasFinanc,
     pagLaProvinciaBuenosAires, pagBrasilPlural, pagJPMorganChaseBank, pagAndbank,
     pagINGBankNV, pagBCV, pagLevycamCCV, pagRepOrientalUruguay, pagBEXSCC,
     pagHSBC, pagArbi, pagIntesaSanPaolo, pagTricury, pagInterCap, pagFibra,
     pagLusoBrasileiro, pagPAN, pagBradescoCartoes, pagItauBank, pagMUFGBrasil,
     pagSumitomoMitsui, pagOmniBanco, pagItauUnibancoHolding, pagIndusval,
     pagCrefisa, pagMizuhodoBrasil, pagInvestcredUni, pagBMG, pagFicsa,
     pagSagiturCC, pagSocieteGeneraleBrasil, pagMagliano, pagTullettPrebon,
     pagCreditSuisseHedgingGriffo, pagPaulista, pagBankofAmericaMerrillLynch,
     pagCCRRegMogiana, pagPine, pagEasynvest, pagDaycoval, pagCarol,
     pagRenascenca, pagDeutscheBank, pagCifra, pagGuide, pagRendimento, pagBS2,
     pagBS2DistribuidoraTitulos, pagOleBonsucessoConsignado, pagLastroRDV,
     pagFrenteCC, pagBTCC, pagNovoBancoContinental, pagCreditAgricoleBrasil,
     pagBancoSistema, pagCredialianca, pagVR, pagBancoOurinvest, pagCredicoamo,
     pagRBCapitalInvestimentos, pagJohnDeere, pagAdvanced, pagC6, pagDigimais);
     {
  pagABCBrasil, pagAgibank, pagAlfa, pagAndbank, pagB3, pagBancodaAmazonia, pagBancodaChinaBrasil,
    pagBancodoBrasil, pagBancodoEstadodeSergipe, pagBancodoEstadodoPara, pagBanrisul, pagBancoDoNordeste, pagBANDEPE,
    pagBANESTES, pagBankofAmericaMerrillLynch, pagBCV, pagBEXSBancodeCambio, pagBMG, pagBNPParibasBrasil, pagBNYMellonBanco,
    pagBOCOMBBM, pagBradescard, pagBradesco, pagBRB, pagBS2, pagBTGPactual, pagC6Consignado, pagCaixaEconomica,
    pagCaixaGeralBrasil, pagCargill, pagCetelem, pagChinaConstructionBank, pagCifra, pagCitibankNA, pagCitibank,
    pagCreditAgricoleBrasil, pagCreditSuisse, pagDaycoval, pagDeutscheBank, pagDigimais, pagFibra, pagFinaxis, pagGenial,
    pagGuanabara, pagHipercardBancoMultiplo, pagHSBC, pagInbursa, pagIndustrialdoBrasil, pagINGBankNV, pagInter,
    pagInvestcredUni, pagItau, pagJPMorgan, pagJSafra, pagJohnDeere, pagJPMorganChaseBank, pagNationalAssociation,
    pagKirtonBank, pagLetsbank, pagLusoBrasileiro, pagMaster, pagMercantil, pagMizuhodoBrasil, pagModal, pagMSBank,
    pagMUFGBrasil, pagOleBonsucessoConsignado, pagOriginal, pagPAN, pagParanaBanco, pagPaulista, pagPine, pagRabobank,
    pagRendimento, pagRodobens, pagSafra, pagSantander, pagScotiabankBrasil, pagSemear, pagSenff, pagSicoob, pagSicredi,
    pagSocieteGeneraleBrasil, pagSorocred, pagStateStreetBrasil, pagSumitomoMitsui, pagTopazio, pagTravelex, pagTriangulo,
    pagUBSBrasil, pagVoiter, pagVotorantim, pagVR, pagWesternUniondoBrasil, pagXP, pagBancoCECRED);
    }
  TTipoInscricao = (tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros);

  TTipoArquivo = (taRemessa, taRetorno);

  TTipoServico = (tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                  tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                  tsConsultaMargem, tsAverbacaoConsignacao,
                  tsPagamentoDividendos, tsManutencaoConsignacao,
                  tsConsignacaoParcelas, tsGlosaConsignacao,
                  tsConsultaTributosaPagar, tsDebentures, tsPagamentoFornecedor,
                  tsPagamentoContas, tsInteroperabilidade, tsCompror, tsComprorRotativo,
                  tsAlegacaoSacado, tsPagamentoSalarios,
                  tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                  tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                  tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                  tsPagamentoAutorizado, tsPagamentoCredenciados,
                  tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                  tsPagamentoBeneficios, tsPagamentosDiversos, tsNenhum);

  TFormaLancamento = (flCreditoContaCorrente, flChequePagamento, flDocTed,
                      flCartaoSalario, flCreditoContaPoupanca,
                      flCreditoContaCorrenteMesmaTitularidade,
                      flDocMesmaTitularidade, flOPDisposicao,
                      flPagamentoContas, flPagamentoConcessionarias,
                      flTributoDARFNormal, flTributoGPS,
                      flTributoDARFSimples, flTributoIPTU,
                      flPagamentoAutenticacao, flTributoDARJ,
                      flTributoGARESPICMS, flTributoGARESPDR,
                      flTributoGARESPITCMD, flTributoIPVA,
                      flTributoLicenciamento, flTributoDPVAT,
                      flLiquidacaoTitulosProprioBanco,
                      flLiquidacaoTitulosOutrosBancos,
                      flLiberacaoTitulosNotaFiscalEletronica,
                      flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                      flExtratoContaCorrente, flTEDOutraTitularidade,
                      flTEDMesmaTitularidade, flTEDTransferencia,
                      flDebitoContaCorrente, flExtratoGestaoCaixa,
                      flDepositoJudicialContaCorrente, flCartaoSalarioItau,
                      flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                      flTributoGNRe, flPIXTransferencia, flPIXQRCode, flNenhum);

  TTipoMovimento = (tmInclusao, tmConsulta, tmSuspensao, tmEstorno, tmReativacao,
                    tmAlteracao, tmLiquidacao, tmExclusao);

  TInstrucaoMovimento = (imInclusaoRegistroDetalheLiberado,
                         imInclusaoRegistroDetalheBloqueado,
                         imAltecacaoPagamentoLiberadoparaBloqueio,
                         imAlteracaoPagamentoBloqueadoparaLiberado,
                         imAlteracaoValorTitulo,
                         imAlteracaoDataPagamento,
                         imPagamentoDiretoFornecedor,
                         imManutencaoemCarteira,
                         imRetiradadeCarteira,
                         imEstornoDevolucaoCamaraCentralizadora,
                         imAlegacaoSacado,
                         imExclusaoRegistro);

  TTipoMoeda = (tmBonusTesouroNacional, tmReal, tmDolarAmericano,
                tmEscudoPortugues, tmFrancoFrances, tmFrancoSuico,
                tmIenJapones, tmIndiceGeralPrecos, tmIndiceGeralPrecosMercado,
                tmLibraEsterlina, tmMarcoAlemao, tmTaxaReferencialDiaria,
                tmUnidadePadraoCapital, tmUnidadePadraoFinanciamento,
                tmUnidadeFiscalReferencia, tmUnidadeMonetariaEuropeia);

  TTipoTributo = (ttGPS, ttDARFNormal, ttDARFSimples, ttDARJ, ttGareICMS,
                  ttIPVA, ttDPVAT, ttLicenciamento, ttFGTS);

  TIndTributo = (itNenhum, itDARFNormal, itDARFSimples, itGPS, itDARJ,
                 itIPVA, itLicenciamento, itDPVAT);

  TTipoOperacao = (toCredito, toDebito, toExtrato, toGestao,
                   toInformacao, toRemessa, toRetorno);

  TTipoMovimentoPagto = (tmpCredito,
                         tmpDebito,
                         tmpAcumulado,
                         tmpRendimentoTributavelDeducaoIRRF,
                         tmpRendimentoIsentoNaoTributavel,
                         tmpRendimentoSujeitoTributacaoExclusiva,
                         tmpRendimentoRecebidoAcumuladamente,
                         tmpInformacaoComplementar);

  TCodigoPagamentoGps = (cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                         cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                         cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                         cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                         cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                         cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                         cpgEmpresasOptantesSimplesCNPJ,
                         cpgEmpresasGeralCNPJ,
                         cpgEmpresasGeralCEI,
                         cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                         cpgReclamatoriaTrabalhistaCNPJ);

  TTipoChavePix = (tcpNenhum, tcpTelefone, tcpEmail, tcpCPFCNPJ, tcpAleatoria, tcpDadosBancarios);

  TTipoCampo = (tcStr, tcStrZero, tcInt, tcInt64, tcDe2, tcDe5, tcDe8, tcDat,
                tcHor, tcDatISO);

  TOcorrencia = (toEsp,
                 to00, to01, to02, to03,
                 toAA, toAB, toAC, toAD, toAE, toAF, toAG, toAH, toAI, toAJ,
                 toAK, toAL, toAM, toAN, toAO, toAP, toAQ, toAR, toAS, toAT,
                 toAU, toAV, toAW, toAX, toAY, toAZ,
                 toBA, toBB, toBC, toBD, toBE, toBF, toBG, toBH, toBI, toBJ,
                 toBK, toBL, toBM, toBN, toBO, toBP, toBQ, toBR, toBS,
                 toCA, toCB, toCC, toCD, toCE, toCF, toCG, toCH, toCI, toCJ,
                 toCK, toCL, toCM, toCN, toCO, toCP, toCQ, toCR, toCS,
                 toDA, toDB, toDC, toDD, toDE, toDF, toDG, toDH, toDI, toDJ,
                 toDK, toDL, toDM, toDV,
                 toD0, toD1, toD2, toD3, toD4, toD5, toD6, toD7, toD8, toD9,
                 toEM, toEX, toE0, toE1, toE2, toE3, toE4,
                 toFC, toFD,
                 toHA, toHB, toHC, toHD, toHE, toHF, toHG, toHH, toHI, toHJ,
                 toHK, toHL, toHM, toHN, toHO, toHP, toHQ, toHR, toHS, toHT,
                 toHU, toHV, toHW, toHX, toHY, toHZ, toH1, toH2, toH3, toH4,
                 toH5, toH6, toH7, toH8, toH9,
                 toIA, toIB, toIC, toID, toIE, toIF, toIG, toIH, toII, toIJ,
                 toIK, toIL, toIM, toIN, toIO, toIP, toIQ, toIR,
                 toIS, toIT, toIU, toIV, toIX,
                 toLA, toLC,
                 toNA, toNB, toNC, toND, toNE, toNF, toNG, toNH, toNI, toNR,
                 toPA, toPB, toPC, toPD, toPE, toPF, toPG, toPH, toPI, toPJ,
                 toPK, toPL, toPM, toPN,
                 toRJ, toRS, toSS,
                 toTA, toTI,
                 toX1, toX2, toX3, toX4,
                 toYA, toYB, toYC, toYD, toYE, toYF,
                 toZA, toZB, toZC, toZD, toZE, toZF, toZG, toZH, toZI, toZJ,
                 toZK);

  TNaturezaLanc = (nlDPV, nlSCR, nlSSR, nlCDS);

  TIndFormaPag = (ifpDebitoContaCorrente, ifpDebitoEmprestimo,
                  ifpDebitoCartaoCredito);

function StrToEnumerado(var ok: boolean; const s: string; const AString: array of string; const AEnumerados: array of variant): variant;
function EnumeradoToStr(const t: variant; const AString: array of string; const AEnumerados: array of variant): variant;

function BancoToStr(const t: TBanco): String;
function BancoToDesc(const t: TBanco): String;
function StrToBanco(var ok: boolean; const s: String): TBanco;
function BancoToIspb(const t: TBanco): String;

function TpInscricaoToStr(const t: TTipoInscricao): String;
function StrToTpInscricao(var ok: boolean; const s: string): TTipoInscricao;
function InscricaoToStr_SegN(const t: TTipoInscricao): String;

function TpArquivoToStr(const t: TTipoArquivo): String;
function StrToTpArquivo(var ok: boolean; const s: String): TTipoArquivo;

function TpServicoToStr(const t: TTipoServico): String;
function StrToTpServico(var ok: boolean; const s: String): TTipoServico;

function FmLancamentoToStr(const t: TFormaLancamento): String;
function StrToFmLancamento(var ok: boolean; const s: String): TFormaLancamento;

function TpMovimentoToStr(const t: TTipoMovimento): String;
function StrToTpMovimento(var ok:boolean; const s: string): TTipoMovimento;

function InMovimentoToStr(const t: TInstrucaoMovimento): String;
function StrToInMovimento(var ok: boolean; const s: string): TInstrucaoMovimento;

function TpMoedaToStr(const t: TTipoMoeda): String;
function StrToTpMoeda(var ok: boolean; const s: string): TTipoMoeda;

function TpIndTributoToStr(const t: TIndTributo): String;
function StrToIndTributo(var ok: boolean; const s: string): TIndTributo;

function TpOperacaoToStr(const t: TTipoOperacao): String;
function StrToTpOperacao(var ok: boolean; const s: string): TTipoOperacao;

function TpMovimentoPagtoToStr(const t: TTipoMovimentoPagto): String;
function StrToTpMovimentoPagto(var ok:boolean; const s: string): TTipoMovimentoPagto;

function CodigoPagamentoGpsToStr(const t: TCodigoPagamentoGps): String;
function StrToCodigoPagamentoGps(var ok: boolean; const s: string): TCodigoPagamentoGps;

function TipoChavePixToStr(const t: TTipoChavePIX): String;
function StrToTipoChavePIX(var ok:boolean; const s: string): TTipoChavePIX;

function TpTributoToStr(const t: TTipoTributo): String;

function LinhaDigitavelParaBarras(const linha: string):string;

function StrToTOcorrencia(var ok:boolean; const s: string): TOcorrencia;

function StrToNaturezaLanc(var ok: boolean; const s: String): TNaturezaLanc;

function IndFormaPagToStr(const t: TIndFormaPag): String;

implementation

function StrToEnumerado(var ok: boolean; const s: string; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: Integer;
begin
  result := -1;

  for i := Low(AString) to High(AString) do
    if AnsiSameText(s, AString[i]) then
      result := AEnumerados[i];

  ok := result <> -1;

  if not ok then
    result := AEnumerados[0];
end;

function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: Integer;
begin
  result := '';

  for i := Low(AEnumerados) to High(AEnumerados) do
    if t = AEnumerados[i] then
      result := AString[i];
end;

function BancoToStr(const t: TBanco): String;
begin
  result := EnumeradoToStr(t,
    ['000',
     '001', '335', '260', '290', '323', '237', '637', '077', '341', '104',
     '033', '212', '756', '655', '041', '422', '070', '136', '741', '739',
     '743', '100', '096', '747', '748', '752', '091', '399', '108', '757',
     '102', '348', '340', '364', '084', '180', '066', '015', '143', '062',
     '074', '099', '025', '075', '040', '190', '063', '191', '064', '097',
     '016', '012', '003', '060', '037', '159', '172', '085', '114', '036',
     '394', '004', '320', '189', '105', '076', '082', '286', '093', '273',
     '157', '183', '014', '130', '127', '079', '081', '118', '133', '121',
     '083', '138', '024', '095', '094', '276', '137', '092', '047', '144',
     '126', '301', '173', '119', '254', '268', '107', '412', '124', '149',
     '197', '142', '389', '184', '634', '545', '132', '298', '129', '128',
     '194', '310', '163', '280', '146', '279', '182', '278', '271', '021',
     '246', '751', '208', '746', '241', '612', '604', '505', '196', '300',
     '477', '266', '122', '376', '473', '745', '120', '265', '007', '188',
     '134', '641', '029', '243', '078', '111', '017', '174', '495', '125',
     '488', '065', '492', '250', '145', '494', '253', '269', '213', '139',
     '018', '630', '224', '600', '623', '204', '479', '456', '464', '613',
     '652', '653', '069', '370', '249', '318', '626', '270', '366', '113',
     '131', '011', '611', '755', '089', '643', '140', '707', '288', '101',
     '487', '233', '117', '633', '218', '292', '169', '293', '285', '080',
     '753', '222', '754', '098', '610', '712', '010', '283', '217', '117',
     '336', '654'],
    [pagNenhum,
     pagBancodoBrasil, pagBancoDigito, pagNuBank,
     pagPagSeguro, pagMercadoPago, pagBradesco, pagSofisaDireto, pagInter, pagItau,
     pagCaixaEconomica, pagSantander, pagOriginal, pagBanCooB, pagVotorantim,
     pagBanrisul, pagSafra, pagBRB, pagUnicredCooperativa, pagBancoRibeiraoPreto,
     pagCetelem, pagSemear, pagPlannerCorretora, pagB3, pagRabobank, pagSicredi,
     pagBNPParibasBrasil, pagUnicredCentralRS, pagKirtonBank, pagPortoCred,
     pagKebHanaBrasil, pagXPInvestimentos, pagBancoXP, pagSuperPagamentos,
     pagGerencianetPagamentos, pagUniprimeNortedoParana, pagCapitalMarkets,
     pagMorganStanley, pagUBSBrasilCCTVM, pagTrevisoCC, pagHipercardBancoMultiplo,
     pagJSafra, pagUniprimeCentral, pagAlfa, pagABNAmro, pagCargill, pagServiCoop,
     pagBradescard, pagNovaFutura, pagGoldmanSachsBrasil, pagCCCNoroesteBrasileiro,
     pagCCMDespTransSCeRS, pagInbursa, pagBancodaAmazonia, pagConfidenceCC,
     pagBancodoEstadodoPara, pagCasaCredito, pagAlbatrossCCV, pagBancoCECRED,
     pagCooperativaCreditoEspiritoSanto, pagBancoBBI, pagBradescoFinanciamentos,
     pagBancoDoNordeste, pagCCBBrasil, pagHSFinanceira, pagLeccaCFI,
     pagKDBBrasil, pagTopazio, pagCCROuro, pagPolocred, pagCCRSaoMigueldoOeste,
     pagICAPBrasil, pagSocred, pagNatixisBrasil, pagCaruana,
     pagCodepeCVC, pagOriginalAgronegocio, pagBancoBrasileiroNegocios,
     pagStandardChartered, pagCresol, pagAgibank, pagBancodaChinaBrasil,
     pagGetMoneyCC, pagBANDEPE, pagConfidenceCambio, pagFinaxis, pagSenff,
     pagMultiMoneyCC, pagBRK, pagBancodoEstadodeSergipe, pagBEXSBancodeCambio,
     pagBRPartners, pagBPP, pagBRLTrustDTVM, pagWesternUniondoBrasil,
     pagParanaBanco, pagBariguiCH, pagBOCOMBBM, pagCapital, pagWooriBank, pagFacta,
     pagStone, pagBrokerBrasilCC, pagMercantil, pagItauBBA, pagTriangulo,
     pagSenso, pagICBCBrasil, pagVipsCC, pagUBSBrasil, pagMSBank, pagMarmetal,
     pagVortx, pagCommerzbank, pagAvista, pagGuittaCC, pagCCRPrimaveraDoLeste,
     pagDacasaFinanceira, pagGenial, pagIBCCTVM, pagBANESTES, pagABCBrasil,
     pagScotiabankBrasil, pagBTGPactual, pagModal, pagClassico, pagGuanabara,
     pagIndustrialdoBrasil, pagCreditSuisse, pagFairCC, pagLaNacionArgentina,
     pagCitibankNA, pagCedula, pagBradescoBERJ, pagJPMorgan, pagCaixaGeralBrasil,
     pagCitibank, pagRodobens, pagFator, pagBNDES, pagAtivaInvestimentos,
     pagBGCLiquidez, pagAlvorada, pagItauConsignado, pagMaxima,
     pagHaitongBi, pagOliveiraTrust, pagBNYMellonBanco, pagPernambucabasFinanc,
     pagLaProvinciaBuenosAires, pagBrasilPlural, pagJPMorganChaseBank, pagAndbank,
     pagINGBankNV, pagBCV, pagLevycamCCV, pagRepOrientalUruguay, pagBEXSCC,
     pagHSBC, pagArbi, pagIntesaSanPaolo, pagTricury, pagInterCap, pagFibra,
     pagLusoBrasileiro, pagPAN, pagBradescoCartoes, pagItauBank, pagMUFGBrasil,
     pagSumitomoMitsui, pagOmniBanco, pagItauUnibancoHolding, pagIndusval,
     pagCrefisa, pagMizuhodoBrasil, pagInvestcredUni, pagBMG, pagFicsa,
     pagSagiturCC, pagSocieteGeneraleBrasil, pagMagliano, pagTullettPrebon,
     pagCreditSuisseHedgingGriffo, pagPaulista, pagBankofAmericaMerrillLynch,
     pagCCRRegMogiana, pagPine, pagEasynvest, pagDaycoval, pagCarol,
     pagRenascenca, pagDeutscheBank, pagCifra, pagGuide, pagRendimento, pagBS2,
     pagBS2DistribuidoraTitulos, pagOleBonsucessoConsignado, pagLastroRDV,
     pagFrenteCC, pagBTCC, pagNovoBancoContinental, pagCreditAgricoleBrasil,
     pagBancoSistema, pagCredialianca, pagVR, pagBancoOurinvest, pagCredicoamo,
     pagRBCapitalInvestimentos, pagJohnDeere, pagAdvanced, pagC6, pagDigimais]);
end;

function BancoToDesc(const t: TBanco): String;
begin
  result := EnumeradoToStr(t,
    ['Nenhum',
     'BANCO DO BRASIL S.A', 'Banco Digito S.A', 'NU PAGAMENTOS S.A',
     'Pagseguro Internet S.A', 'Mercado Pago', 'BANCO SOFISA S.A', 'BANCO INTER S.A',
     'ITAU UNIBANCO S.A', 'CAIXA ECONOMICA FEDERAL', 'BANCO SANTANDER BRASIL S.A',
     'BANCO ORIGINAL S.A', 'BANCOOB', 'BANCO VOTORANTIM S.A', 'BANRISUL',
     'BANCO BRADESCO S.A', 'BANCO SAFRA S.A', 'BANCO DE BRASILIA',
     'UNICRED COOPERATIVA', 'BANCO RIBEIRAO PRETO', 'BANCO CETELEM S.A',
     'BANCO SEMEAR S.A', 'PLANNER CORRETORA DE VALORES S.A', 'BANCO B3 S.A',
     'RABOBANK INTERNACIONAL DO BRASIL S.A', 'SICREDI S.A', 'BNP PARIBAS BRASIL S.A',
     'UNICRED CENTRAL RS', 'KIRTON BANK', 'PORTOCRED S.A',
     'BANCO KEB HANA DO BRASIL S.A', 'XP INVESTIMENTOS S.A', 'BANCO XP S/A',
     'SUPER PAGAMENTOS S/A', 'GERENCIANET PAGAMENTOS DO BRASIL',
     'UNIPRIME NORTE DO PARANA', 'CM CAPITAL MARKETS CCTVM LTDA',
     'BANCO MORGAN STANLEY S.A', 'UBS BRASIL CCTVM S.A', 'TREVISO CC S.A',
     'HIPERCARD BM S.A', 'BCO. J.SAFRA S.A', 'UNIPRIME CENTRAL CCC LTDA',
     'BANCO ALFA S.A.', 'BCO ABN AMRO S.A', 'BANCO CARGILL S.A', 'SERVICOOP',
     'BANCO BRADESCARD', 'NOVA FUTURA CTVM LTDA', 'GOLDMAN SACHS DO BRASIL BM S.A',
     'CCC NOROESTE BRASILEIRO LTDA', 'CCM DESP TRANS SC E RS', 'BANCO INBURSA',
     'BANCO DA AMAZONIA S.A', 'CONFIDENCE CC S.A', 'BANCO DO ESTADO DO PARA S.A',
     'CASA CREDITO S.A', 'ALBATROSS CCV S.A', 'COOP CENTRAL AILOS',
     'CENTRAL COOPERATIVA DE CREDITO NO ESTADO DO ESPIRITO SANTO',
     'BANCO BBI S.A', 'BANCO BRADESCO FINANCIAMENTOS S.A',
     'BANCO DO NORDESTE DO BRASIL S.A.', 'BANCO CCB BRASIL S.A', 'HS FINANCEIRA',
     'LECCA CFI S.A', 'BANCO KDB BRASIL S.A.', 'BANCO TOPAZIO S.A', 'CCR DE OURO',
     'POLOCRED SCMEPP LTDA', 'CCR DE SAO MIGUEL DO OESTE',
     'ICAP DO BRASIL CTVM LTDA', 'SOCRED S.A', 'NATIXIS BRASIL S.A', 'CARUANA SCFI',
     'CODEPE CVC S.A', 'BANCO ORIGINAL DO AGRONEGOCIO S.A',
     'BBN BANCO BRASILEIRO DE NEGOCIOS S.A', 'STANDARD CHARTERED BI S.A',
     'CRESOL CONFEDERACAO', 'BANCO AGIBANK S.A', 'BANCO DA CHINA BRASIL S.A',
     'GET MONEY CC LTDA', 'BCO BANDEPE S.A', 'BANCO CONFIDENCE DE CAMBIO S.A',
     'BANCO FINAXIS', 'SENFF S.A', 'MULTIMONEY CC LTDA', 'BRK S.A',
     'BANCO BCO DO ESTADO DE SERGIPE S.A', 'BEXS BANCO DE CAMBIO S.A.',
     'BR PARTNERS BI', 'BPP INSTITUICAO DE PAGAMENTOS S.A', 'BRL TRUST DTVM SA',
     'BANCO WESTERN UNION', 'PARANA BANCO S.A', 'BARIGUI CH', 'BANCO BOCOM BBM S.A',
     'BANCO CAPITAL S.A', 'BANCO WOORI BANK DO BRASIL S.A', 'FACTA S.A. CFI',
     'STONE PAGAMENTOS S.A', 'BROKER BRASIL CC LTDA', 'BANCO MERCANTIL DO BRASIL S.A.',
     'BANCO ITAU BBA S.A', 'BANCO TRIANGULO S.A', 'SENSO CCVM S.A',
     'ICBC DO BRASIL BM S.A', 'VIPS CC LTDA', 'UBS BRASIL BI S.A',
     'MS BANK S.A BANCO DE CAMBIO', 'PARMETAL DTVM LTDA', 'VORTX DTVM LTDA',
     'COMMERZBANK BRASIL S.A', 'AVISTA S.A', 'GUITTA CC LTDA',
     'CCR DE PRIMAVERA DO LESTE', 'DACASA FINANCEIRA S/A',
     'GENIAL INVESTIMENTOS CVM S.A', 'IB CCTVM LTDA', 'BANCO BANESTES S.A',
     'BANCO ABC BRASIL S.A', 'SCOTIABANK BRASIL', 'BANCO BTG PACTUAL S.A',
     'BANCO MODAL S.A', 'BANCO CLASSICO S.A', 'BANCO GUANABARA S.A',
     'BANCO INDUSTRIAL DO BRASIL S.A', 'BANCO CREDIT SUISSE (BRL) S.A',
     'BANCO FAIR CC S.A', 'BANCO LA NACION ARGENTINA', 'CITIBANK N.A',
     'BANCO CEDULA S.A', 'BANCO BRADESCO BERJ S.A', 'BANCO J.P. MORGAN S.A',
     'BANCO CAIXA GERAL BRASIL S.A', 'BANCO CITIBANK S.A', 'BANCO RODOBENS S.A',
     'BANCO FATOR S.A', 'BNDES', 'ATIVA S.A INVESTIMENTOS', 'BGC LIQUIDEZ DTVM LTDA',
     'BANCO ALVORADA S.A', 'BANCO ITAU CONSIGNADO S.A', 'BANCO MAXIMA S.A',
     'HAITONG BI DO BRASIL S.A', 'BANCO OLIVEIRA TRUST DTVM S.A',
     'BNY MELLON BANCO S.A', 'PERNAMBUCANAS FINANC S.A',
     'LA PROVINCIA BUENOS AIRES BANCO', 'BRASIL PLURAL S.A BANCO',
     'JPMORGAN CHASE BANK', 'BANCO ANDBANK S.A', 'ING BANK N.V', 'BANCO BCV',
     'LEVYCAM CCV LTDA', 'BANCO REP ORIENTAL URUGUAY', 'BEXS CC S.A',
     'HSBC BANCO DE INVESTIMENTO', 'BCO ARBI S.A', 'INTESA SANPAOLO BRASIL S.A',
     'BANCO TRICURY S.A', 'BANCO INTERCAP S.A', 'BANCO FIBRA S.A',
     'BANCO LUSO BRASILEIRO S.A', 'BANCO PAN', 'BANCO BRADESCO CARTOES S.A',
     'BANCO ITAUBANK S.A', 'BANCO MUFG BRASIL S.A',
     'BANCO SUMITOMO MITSUI BRASIL S.A', 'OMNI BANCO S.A',
     'ITAU UNIBANCO HOLDING BM S.A', 'BANCO INDUSVAL S.A', 'BANCO CREFISA S.A',
     'BANCO MIZUHO S.A', 'BANCO INVESTCRED UNIBANCO S.A', 'BANCO BMG S.A',
     'BANCO FICSA S.A', 'SAGITUR CC LTDA', 'BANCO SOCIETE GENERALE BRASIL',
     'MAGLIANO S.A', 'TULLETT PREBON BRASIL CVC LTDA',
     'C.SUISSE HEDGING-GRIFFO CV S.A', 'BANCO PAULISTA',
     'BOFA MERRILL LYNCH BM S.A', 'CCR REG MOGIANA', 'BANCO PINE S.A',
     'EASYNVEST – TITULO CV S.A', 'BANCO DAYCOVAL S.A', 'CAROL DTVM LTDA',
     'RENASCENCA DTVM LTDA', 'DEUTSCHE BANK S.A', 'BANCO CIFRA', 'GUIDE',
     'BANCO RENDIMENTO S.A', 'BANCO BS2 S.A',
     'BS2 DISTRIBUIDORA DE TITULOS E INVESTIMENTOS',
     'BANCO OLE BONSUCESSO CONSIGNADO S.A', 'LASTRO RDV DTVM LTDA',
     'FRENTE CC LTDA', 'B&T CC LTDA', 'NOVO BANCO CONTINENTAL S.A',
     'BANCO CREDIT AGRICOLE BR S.A', 'BANCO SISTEMA', 'CREDIALIANCA CCR',
     'BANCO VR S.A', 'BANCO OURINVEST S.A', 'CREDICOAMO',
     'RB CAPITAL INVESTIMENTOS DTVM LTDA', 'BANCO JOHN DEERE S.A',
     'ADVANCED CC LTDA', 'BANCO C6 S.A', 'BANCO DIGIMAIS S.A'],
    [pagNenhum,
     pagBancodoBrasil, pagBancoDigito, pagNuBank, pagPagSeguro, pagMercadoPago,
     pagSofisaDireto, pagInter, pagItau, pagCaixaEconomica, pagSantander,
     pagOriginal, pagBanCooB, pagVotorantim, pagBanrisul, pagBradesco, pagSafra,
     pagBRB, pagUnicredCooperativa, pagBancoRibeiraoPreto, pagCetelem, pagSemear,
     pagPlannerCorretora, pagB3, pagRabobank, pagSicredi, pagBNPParibasBrasil,
     pagUnicredCentralRS, pagKirtonBank, pagPortoCred, pagKebHanaBrasil,
     pagXPInvestimentos, pagBancoXP, pagSuperPagamentos,
     pagGerencianetPagamentos, pagUniprimeNortedoParana, pagCapitalMarkets,
     pagMorganStanley, pagUBSBrasilCCTVM, pagTrevisoCC, pagHipercardBancoMultiplo,
     pagJSafra, pagUniprimeCentral, pagAlfa, pagABNAmro, pagCargill, pagServiCoop,
     pagBradescard, pagNovaFutura, pagGoldmanSachsBrasil, pagCCCNoroesteBrasileiro,
     pagCCMDespTransSCeRS, pagInbursa, pagBancodaAmazonia, pagConfidenceCC,
     pagBancodoEstadodoPara, pagCasaCredito, pagAlbatrossCCV, pagBancoCECRED,
     pagCooperativaCreditoEspiritoSanto, pagBancoBBI, pagBradescoFinanciamentos,
     pagBancoDoNordeste, pagCCBBrasil, pagHSFinanceira, pagLeccaCFI,
     pagKDBBrasil, pagTopazio, pagCCROuro, pagPolocred, pagCCRSaoMigueldoOeste,
     pagICAPBrasil, pagSocred, pagNatixisBrasil, pagCaruana,
     pagCodepeCVC, pagOriginalAgronegocio, pagBancoBrasileiroNegocios,
     pagStandardChartered, pagCresol, pagAgibank, pagBancodaChinaBrasil,
     pagGetMoneyCC, pagBANDEPE, pagConfidenceCambio, pagFinaxis, pagSenff,
     pagMultiMoneyCC, pagBRK, pagBancodoEstadodeSergipe, pagBEXSBancodeCambio,
     pagBRPartners, pagBPP, pagBRLTrustDTVM, pagWesternUniondoBrasil,
     pagParanaBanco, pagBariguiCH, pagBOCOMBBM, pagCapital, pagWooriBank, pagFacta,
     pagStone, pagBrokerBrasilCC, pagMercantil, pagItauBBA, pagTriangulo,
     pagSenso, pagICBCBrasil, pagVipsCC, pagUBSBrasil, pagMSBank, pagMarmetal,
     pagVortx, pagCommerzbank, pagAvista, pagGuittaCC, pagCCRPrimaveraDoLeste,
     pagDacasaFinanceira, pagGenial, pagIBCCTVM, pagBANESTES, pagABCBrasil,
     pagScotiabankBrasil, pagBTGPactual, pagModal, pagClassico, pagGuanabara,
     pagIndustrialdoBrasil, pagCreditSuisse, pagFairCC, pagLaNacionArgentina,
     pagCitibankNA, pagCedula, pagBradescoBERJ, pagJPMorgan, pagCaixaGeralBrasil,
     pagCitibank, pagRodobens, pagFator, pagBNDES, pagAtivaInvestimentos,
     pagBGCLiquidez, pagAlvorada, pagItauConsignado, pagMaxima,
     pagHaitongBi, pagOliveiraTrust, pagBNYMellonBanco, pagPernambucabasFinanc,
     pagLaProvinciaBuenosAires, pagBrasilPlural, pagJPMorganChaseBank, pagAndbank,
     pagINGBankNV, pagBCV, pagLevycamCCV, pagRepOrientalUruguay, pagBEXSCC,
     pagHSBC, pagArbi, pagIntesaSanPaolo, pagTricury, pagInterCap, pagFibra,
     pagLusoBrasileiro, pagPAN, pagBradescoCartoes, pagItauBank, pagMUFGBrasil,
     pagSumitomoMitsui, pagOmniBanco, pagItauUnibancoHolding, pagIndusval,
     pagCrefisa, pagMizuhodoBrasil, pagInvestcredUni, pagBMG, pagFicsa,
     pagSagiturCC, pagSocieteGeneraleBrasil, pagMagliano, pagTullettPrebon,
     pagCreditSuisseHedgingGriffo, pagPaulista, pagBankofAmericaMerrillLynch,
     pagCCRRegMogiana, pagPine, pagEasynvest, pagDaycoval, pagCarol,
     pagRenascenca, pagDeutscheBank, pagCifra, pagGuide, pagRendimento, pagBS2,
     pagBS2DistribuidoraTitulos, pagOleBonsucessoConsignado, pagLastroRDV,
     pagFrenteCC, pagBTCC, pagNovoBancoContinental, pagCreditAgricoleBrasil,
     pagBancoSistema, pagCredialianca, pagVR, pagBancoOurinvest, pagCredicoamo,
     pagRBCapitalInvestimentos, pagJohnDeere, pagAdvanced, pagC6, pagDigimais]);
end;

function StrToBanco(var ok: boolean; const s: String): TBanco;
begin
  Result := StrToEnumerado(ok, s,
    ['000',
     '001', '335', '260', '290', '323', '237', '637', '077', '341', '104',
     '033', '212', '756', '655', '041', '422', '070', '136', '741', '739',
     '743', '100', '096', '747', '748', '752', '091', '399', '108', '757',
     '102', '348', '340', '364', '084', '180', '066', '015', '143', '062',
     '074', '099', '025', '075', '040', '190', '063', '191', '064', '097',
     '016', '012', '003', '060', '037', '159', '172', '085', '114', '036',
     '394', '004', '320', '189', '105', '076', '082', '286', '093', '273',
     '157', '183', '014', '130', '127', '079', '081', '118', '133', '121',
     '083', '138', '024', '095', '094', '276', '137', '092', '047', '144',
     '126', '301', '173', '119', '254', '268', '107', '412', '124', '149',
     '197', '142', '389', '184', '634', '545', '132', '298', '129', '128',
     '194', '310', '163', '280', '146', '279', '182', '278', '271', '021',
     '246', '751', '208', '746', '241', '612', '604', '505', '196', '300',
     '477', '266', '122', '376', '473', '745', '120', '265', '007', '188',
     '134', '641', '029', '243', '078', '111', '017', '174', '495', '125',
     '488', '065', '492', '250', '145', '494', '253', '269', '213', '139',
     '018', '630', '224', '600', '623', '204', '479', '456', '464', '613',
     '652', '653', '069', '370', '249', '318', '626', '270', '366', '113',
     '131', '011', '611', '755', '089', '643', '140', '707', '288', '101',
     '487', '233', '117', '633', '218', '292', '169', '293', '285', '080',
     '753', '222', '754', '098', '610', '712', '010', '283', '217', '117',
     '336', '654'],
    [pagNenhum,
     pagBancodoBrasil, pagBancoDigito, pagNuBank,
     pagPagSeguro, pagMercadoPago, pagBradesco, pagSofisaDireto, pagInter, pagItau,
     pagCaixaEconomica, pagSantander, pagOriginal, pagBanCooB, pagVotorantim,
     pagBanrisul, pagSafra, pagBRB, pagUnicredCooperativa, pagBancoRibeiraoPreto,
     pagCetelem, pagSemear, pagPlannerCorretora, pagB3, pagRabobank, pagSicredi,
     pagBNPParibasBrasil, pagUnicredCentralRS, pagKirtonBank, pagPortoCred,
     pagKebHanaBrasil, pagXPInvestimentos, pagBancoXP, pagSuperPagamentos,
     pagGerencianetPagamentos, pagUniprimeNortedoParana, pagCapitalMarkets,
     pagMorganStanley, pagUBSBrasilCCTVM, pagTrevisoCC, pagHipercardBancoMultiplo,
     pagJSafra, pagUniprimeCentral, pagAlfa, pagABNAmro, pagCargill, pagServiCoop,
     pagBradescard, pagNovaFutura, pagGoldmanSachsBrasil, pagCCCNoroesteBrasileiro,
     pagCCMDespTransSCeRS, pagInbursa, pagBancodaAmazonia, pagConfidenceCC,
     pagBancodoEstadodoPara, pagCasaCredito, pagAlbatrossCCV, pagBancoCECRED,
     pagCooperativaCreditoEspiritoSanto, pagBancoBBI, pagBradescoFinanciamentos,
     pagBancoDoNordeste, pagCCBBrasil, pagHSFinanceira, pagLeccaCFI,
     pagKDBBrasil, pagTopazio, pagCCROuro, pagPolocred, pagCCRSaoMigueldoOeste,
     pagICAPBrasil, pagSocred, pagNatixisBrasil, pagCaruana,
     pagCodepeCVC, pagOriginalAgronegocio, pagBancoBrasileiroNegocios,
     pagStandardChartered, pagCresol, pagAgibank, pagBancodaChinaBrasil,
     pagGetMoneyCC, pagBANDEPE, pagConfidenceCambio, pagFinaxis, pagSenff,
     pagMultiMoneyCC, pagBRK, pagBancodoEstadodeSergipe, pagBEXSBancodeCambio,
     pagBRPartners, pagBPP, pagBRLTrustDTVM, pagWesternUniondoBrasil,
     pagParanaBanco, pagBariguiCH, pagBOCOMBBM, pagCapital, pagWooriBank, pagFacta,
     pagStone, pagBrokerBrasilCC, pagMercantil, pagItauBBA, pagTriangulo,
     pagSenso, pagICBCBrasil, pagVipsCC, pagUBSBrasil, pagMSBank, pagMarmetal,
     pagVortx, pagCommerzbank, pagAvista, pagGuittaCC, pagCCRPrimaveraDoLeste,
     pagDacasaFinanceira, pagGenial, pagIBCCTVM, pagBANESTES, pagABCBrasil,
     pagScotiabankBrasil, pagBTGPactual, pagModal, pagClassico, pagGuanabara,
     pagIndustrialdoBrasil, pagCreditSuisse, pagFairCC, pagLaNacionArgentina,
     pagCitibankNA, pagCedula, pagBradescoBERJ, pagJPMorgan, pagCaixaGeralBrasil,
     pagCitibank, pagRodobens, pagFator, pagBNDES, pagAtivaInvestimentos,
     pagBGCLiquidez, pagAlvorada, pagItauConsignado, pagMaxima,
     pagHaitongBi, pagOliveiraTrust, pagBNYMellonBanco, pagPernambucabasFinanc,
     pagLaProvinciaBuenosAires, pagBrasilPlural, pagJPMorganChaseBank, pagAndbank,
     pagINGBankNV, pagBCV, pagLevycamCCV, pagRepOrientalUruguay, pagBEXSCC,
     pagHSBC, pagArbi, pagIntesaSanPaolo, pagTricury, pagInterCap, pagFibra,
     pagLusoBrasileiro, pagPAN, pagBradescoCartoes, pagItauBank, pagMUFGBrasil,
     pagSumitomoMitsui, pagOmniBanco, pagItauUnibancoHolding, pagIndusval,
     pagCrefisa, pagMizuhodoBrasil, pagInvestcredUni, pagBMG, pagFicsa,
     pagSagiturCC, pagSocieteGeneraleBrasil, pagMagliano, pagTullettPrebon,
     pagCreditSuisseHedgingGriffo, pagPaulista, pagBankofAmericaMerrillLynch,
     pagCCRRegMogiana, pagPine, pagEasynvest, pagDaycoval, pagCarol,
     pagRenascenca, pagDeutscheBank, pagCifra, pagGuide, pagRendimento, pagBS2,
     pagBS2DistribuidoraTitulos, pagOleBonsucessoConsignado, pagLastroRDV,
     pagFrenteCC, pagBTCC, pagNovoBancoContinental, pagCreditAgricoleBrasil,
     pagBancoSistema, pagCredialianca, pagVR, pagBancoOurinvest, pagCredicoamo,
     pagRBCapitalInvestimentos, pagJohnDeere, pagAdvanced, pagC6, pagDigimais]);
end;

function BancoToIspb(const t: TBanco): String;
begin
  Result := EnumeradoToStr(t,
      ['',
       '00000000', '27098060', '18236120', '08561701', '10573521', '60746948',
       '60889128', '00416968', '60701190', '00360305', '90400888', '92894922',
       '02038232', '59588111', '92702067', '58160789', '00000208', '00315557',
       '00517645', '00558456', '00795423', '00806535', '00997185', '01023570',
       '01181521', '01522368', '        ', '01701201', '01800019', '02318507',
       '02332886', '33264668', '09554480', '09089356', '02398976', '02685483',
       '02801938', '02819125', '02992317', '03012230', '03017677', '03046391',
       '03323840', '03532415', '03609817', '03973814', '04184779', '04257795',
       '04332281', '04632856', '04715685', '04866275', '04902979', '04913129',
       '04913711', '05442029', '        ', '05463212', '05790149', '06271464',
       '07207996', '07237373', '07450604', '07512441', '07652226', '07656500',
       '07679404', '07853842', '07945233', '08253539', '09105360', '09210106',
       '09274232', '09313766', '09512542', '09516419', '10264663', '        ',
       '10398952', '10664513', '10690848', '10853017', '10866788', '11703662',
       '11758741', '11970623', '        ', '12865507', '13009717', '13059145',
       '13220493', '13370835', '13486793', '13720915', '14388334', '14511781',
       '15114366', '15173776', '15357060', '15581638', '16501555', '16944141',
       '17184037', '17298092', '17351180', '17352220', '17453575', '17772370',
       '18520834', '19307785', '20155248', '22610500', '23522214', '23862762',
       '24074692', '26563270', '        ', '27652684', '27842177', '28127603',
       '28195667', '29030467', '30306294', '30723886', '31597552', '31880826',
       '31895683', '32062580', '32648370', '33042151', '33042953', '33132044',
       '33147315', '33172537', '33466988', '33479023', '33603457', '33644196',
       '33657248', '33775974', '33862244', '        ', '33885724', '33923798',
       '34111187', '36113876', '42272526', '43180355', '44189447', '45246410',
       '46518205', '48795256', '49336860', '50585090', '50579044', '        ',
       '52937216', '53518684', '54403563', '55230916', '57839805', '58497702',
       '58616418', '59118133', '59285411', '        ', '60394079', '60498557',
       '60518222', '60850229', '60872504', '61024352', '61033106', '61088183',
       '61182408', '61186680', '61348538', '61444949', '61533584', '61723847',
       '61747085', '61809182', '61820817', '62073200', '62109566', '62144175',
       '62169875', '62232889', '62237649', '62287735', '62331228', '62421979',
       '92856905', '68900810', '71027866', '28650236', '        ', '71590442',
       '71677850', '73622748', '74828799', '75647891', '76543115', '78157146',
       '78626983', '78632767', '81723108', '89960090', '91884981', '92856905',
       '31872495', '92874270'],
    [pagNenhum,
     pagBancodoBrasil, pagBancoDigito, pagNuBank,
     pagPagSeguro, pagMercadoPago, pagBradesco, pagSofisaDireto, pagInter, pagItau,
     pagCaixaEconomica, pagSantander, pagOriginal, pagBanCooB, pagVotorantim,
     pagBanrisul, pagSafra, pagBRB, pagUnicredCooperativa, pagBancoRibeiraoPreto,
     pagCetelem, pagSemear, pagPlannerCorretora, pagB3, pagRabobank, pagSicredi,
     pagBNPParibasBrasil, pagUnicredCentralRS, pagKirtonBank, pagPortoCred,
     pagKebHanaBrasil, pagXPInvestimentos, pagBancoXP, pagSuperPagamentos,
     pagGerencianetPagamentos, pagUniprimeNortedoParana, pagCapitalMarkets,
     pagMorganStanley, pagUBSBrasilCCTVM, pagTrevisoCC, pagHipercardBancoMultiplo,
     pagJSafra, pagUniprimeCentral, pagAlfa, pagABNAmro, pagCargill, pagServiCoop,
     pagBradescard, pagNovaFutura, pagGoldmanSachsBrasil, pagCCCNoroesteBrasileiro,
     pagCCMDespTransSCeRS, pagInbursa, pagBancodaAmazonia, pagConfidenceCC,
     pagBancodoEstadodoPara, pagCasaCredito, pagAlbatrossCCV, pagBancoCECRED,
     pagCooperativaCreditoEspiritoSanto, pagBancoBBI, pagBradescoFinanciamentos,
     pagBancoDoNordeste, pagCCBBrasil, pagHSFinanceira, pagLeccaCFI,
     pagKDBBrasil, pagTopazio, pagCCROuro, pagPolocred, pagCCRSaoMigueldoOeste,
     pagICAPBrasil, pagSocred, pagNatixisBrasil, pagCaruana,
     pagCodepeCVC, pagOriginalAgronegocio, pagBancoBrasileiroNegocios,
     pagStandardChartered, pagCresol, pagAgibank, pagBancodaChinaBrasil,
     pagGetMoneyCC, pagBANDEPE, pagConfidenceCambio, pagFinaxis, pagSenff,
     pagMultiMoneyCC, pagBRK, pagBancodoEstadodeSergipe, pagBEXSBancodeCambio,
     pagBRPartners, pagBPP, pagBRLTrustDTVM, pagWesternUniondoBrasil,
     pagParanaBanco, pagBariguiCH, pagBOCOMBBM, pagCapital, pagWooriBank, pagFacta,
     pagStone, pagBrokerBrasilCC, pagMercantil, pagItauBBA, pagTriangulo,
     pagSenso, pagICBCBrasil, pagVipsCC, pagUBSBrasil, pagMSBank, pagMarmetal,
     pagVortx, pagCommerzbank, pagAvista, pagGuittaCC, pagCCRPrimaveraDoLeste,
     pagDacasaFinanceira, pagGenial, pagIBCCTVM, pagBANESTES, pagABCBrasil,
     pagScotiabankBrasil, pagBTGPactual, pagModal, pagClassico, pagGuanabara,
     pagIndustrialdoBrasil, pagCreditSuisse, pagFairCC, pagLaNacionArgentina,
     pagCitibankNA, pagCedula, pagBradescoBERJ, pagJPMorgan, pagCaixaGeralBrasil,
     pagCitibank, pagRodobens, pagFator, pagBNDES, pagAtivaInvestimentos,
     pagBGCLiquidez, pagAlvorada, pagItauConsignado, pagMaxima,
     pagHaitongBi, pagOliveiraTrust, pagBNYMellonBanco, pagPernambucabasFinanc,
     pagLaProvinciaBuenosAires, pagBrasilPlural, pagJPMorganChaseBank, pagAndbank,
     pagINGBankNV, pagBCV, pagLevycamCCV, pagRepOrientalUruguay, pagBEXSCC,
     pagHSBC, pagArbi, pagIntesaSanPaolo, pagTricury, pagInterCap, pagFibra,
     pagLusoBrasileiro, pagPAN, pagBradescoCartoes, pagItauBank, pagMUFGBrasil,
     pagSumitomoMitsui, pagOmniBanco, pagItauUnibancoHolding, pagIndusval,
     pagCrefisa, pagMizuhodoBrasil, pagInvestcredUni, pagBMG, pagFicsa,
     pagSagiturCC, pagSocieteGeneraleBrasil, pagMagliano, pagTullettPrebon,
     pagCreditSuisseHedgingGriffo, pagPaulista, pagBankofAmericaMerrillLynch,
     pagCCRRegMogiana, pagPine, pagEasynvest, pagDaycoval, pagCarol,
     pagRenascenca, pagDeutscheBank, pagCifra, pagGuide, pagRendimento, pagBS2,
     pagBS2DistribuidoraTitulos, pagOleBonsucessoConsignado, pagLastroRDV,
     pagFrenteCC, pagBTCC, pagNovoBancoContinental, pagCreditAgricoleBrasil,
     pagBancoSistema, pagCredialianca, pagVR, pagBancoOurinvest, pagCredicoamo,
     pagRBCapitalInvestimentos, pagJohnDeere, pagAdvanced, pagC6, pagDigimais]);
end;

function TpInscricaoToStr(const t: TTipoInscricao): String;
begin
 result := EnumeradoToStr(t, ['0', '1', '2', '3', '9'],
                             [tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros]);
end;

function StrToTpInscricao(var ok: boolean; const s: string): TTipoInscricao;
begin
 result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '9'],
                                 [tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros]);
end;

function InscricaoToStr_SegN(const t: TTipoInscricao): String;
begin
 result := EnumeradoToStr(t, ['1', '2', '3', '9'],
                             [tiCNPJ, tiCPF, tiPISPASEP, tiOutros]);
end;

function TpArquivoToStr(const t: TTipoArquivo): String;
begin
 result := EnumeradoToStr(t, ['1', '2'], [taRemessa, taRetorno]);
end;

function StrToTpArquivo(var ok: boolean; const s: String): TTipoArquivo;
begin
 result := StrToEnumerado(ok, s, ['1', '2'], [taRemessa, taRetorno]);
end;

function TpServicoToStr(const t: TTipoServico): String;
begin
 result := EnumeradoToStr(t, ['01', '03', '04', '05', '06', '07', '08', '09', '10',
                              '11', '12', '13', '14', '15', '20', '22', '23', '25',
                              '26', '29', '30', '32', '33', '34', '40', '41', '50',
                              '60', '70', '75', '77', '80', '90', '98', '  '],
                       [tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                        tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                        tsConsultaMargem, tsAverbacaoConsignacao,
                        tsPagamentoDividendos, tsManutencaoConsignacao,
                        tsConsignacaoParcelas, tsGlosaConsignacao,
                        tsConsultaTributosaPagar, tsDebentures,
                        tsPagamentoFornecedor,
                        tsPagamentoContas, tsInteroperabilidade, tsCompror, tsComprorRotativo,
                        tsAlegacaoSacado, tsPagamentoSalarios,
                        tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                        tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                        tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                        tsPagamentoAutorizado, tsPagamentoCredenciados,
                        tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                        tsPagamentoBeneficios, tsPagamentosDiversos,
                        tsNenhum]);
end;

function StrToTpServico(var ok: boolean; const s: String): TTipoServico;
begin
  result := StrToEnumerado(ok, s,
                           ['01', '03', '04', '05', '06', '07', '08', '09', '10',
                            '11', '12', '13', '14', '15', '20', '22', '23', '25',
                            '26', '29', '30', '32', '33', '34', '40', '41', '50',
                            '60', '70', '75', '77', '80', '90', '98', '  '],
                       [tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                        tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                        tsConsultaMargem, tsAverbacaoConsignacao,
                        tsPagamentoDividendos, tsManutencaoConsignacao,
                        tsConsignacaoParcelas, tsGlosaConsignacao,
                        tsConsultaTributosaPagar, tsDebentures,
                        tsPagamentoFornecedor,
                        tsPagamentoContas, tsInteroperabilidade, tsCompror, tsComprorRotativo,
                        tsAlegacaoSacado, tsPagamentoSalarios,
                        tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                        tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                        tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                        tsPagamentoAutorizado, tsPagamentoCredenciados,
                        tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                        tsPagamentoBeneficios, tsPagamentosDiversos,
                        tsNenhum]);
end;

function StrToFmLancamento(var ok: boolean; const s: String): TFormaLancamento;
begin
 result := StrToEnumerado(ok, s,
                          ['01', '02', '03', '04', '05', '06', '07', '10', '11', '13',
                           '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
                           '26', '27', '30', '31', '32', '33', '35', '40', '41', '43',
                           '44', '50', '60', '70', '71', '72', '73', '91', '45', '47', '  '],
                         [flCreditoContaCorrente, flChequePagamento, flDocTed,
                          flCartaoSalario, flCreditoContaPoupanca,
                          flCreditoContaCorrenteMesmaTitularidade,
                          flDocMesmaTitularidade, flOPDisposicao,
                          flPagamentoContas, flPagamentoConcessionarias,
                          flTributoDARFNormal, flTributoGPS,
                          flTributoDARFSimples, flTributoIPTU,
                          flPagamentoAutenticacao, flTributoDARJ,
                          flTributoGARESPICMS, flTributoGARESPDR,
                          flTributoGARESPITCMD, flTributoIPVA,
                          flTributoLicenciamento, flTributoDPVAT,
                          flLiquidacaoTitulosProprioBanco,
                          flLiquidacaoTitulosOutrosBancos,
                          flLiberacaoTitulosNotaFiscalEletronica,
                          flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                          flExtratoContaCorrente, flTEDOutraTitularidade,
                          flTEDMesmaTitularidade, flTEDTransferencia,
                          flDebitoContaCorrente, flCartaoSalarioItau,
                          flExtratoGestaoCaixa, flDepositoJudicialContaCorrente,
                          flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                          flTributoGNRe, flPIXTransferencia, flPIXQRCode, flNenhum]);
end;

function FmLancamentoToStr(const t: TFormaLancamento): String;
begin
 result := EnumeradoToStr(t,
                          ['01', '02', '03', '04', '05', '06', '07', '10', '11', '13',
                           '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
                           '26', '27', '30', '31', '32', '33', '35', '40', '41', '43',
                           '44', '50', '60', '70', '71', '72', '73', '91', '45', '47', '  '],
                         [flCreditoContaCorrente, flChequePagamento, flDocTed,
                          flCartaoSalario, flCreditoContaPoupanca,
                          flCreditoContaCorrenteMesmaTitularidade,
                          flDocMesmaTitularidade, flOPDisposicao,
                          flPagamentoContas, flPagamentoConcessionarias,
                          flTributoDARFNormal, flTributoGPS,
                          flTributoDARFSimples, flTributoIPTU,
                          flPagamentoAutenticacao, flTributoDARJ,
                          flTributoGARESPICMS, flTributoGARESPDR,
                          flTributoGARESPITCMD, flTributoIPVA,
                          flTributoLicenciamento, flTributoDPVAT,
                          flLiquidacaoTitulosProprioBanco,
                          flLiquidacaoTitulosOutrosBancos,
                          flLiberacaoTitulosNotaFiscalEletronica,
                          flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                          flExtratoContaCorrente, flTEDOutraTitularidade,
                          flTEDMesmaTitularidade, flTEDTransferencia,
                          flDebitoContaCorrente, flCartaoSalarioItau,
                          flExtratoGestaoCaixa, flDepositoJudicialContaCorrente,
                          flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                          flTributoGNRe, flPIXTransferencia, flPIXQRCode, flNenhum]);
end;

function TpMovimentoToStr(const t: TTipoMovimento): String;
begin
 result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '7', '9'],
                             [tmInclusao, tmConsulta, tmSuspensao, tmEstorno,
                              tmReativacao, tmAlteracao, tmLiquidacao, tmExclusao]);
end;

function StrToTpMovimento(var ok:boolean; const s: string): TTipoMovimento;
begin
 result := StrToEnumerado(ok, s,
                              ['0', '1', '2', '3', '4', '5', '7', '9'],
                             [tmInclusao, tmConsulta, tmSuspensao, tmEstorno,
                              tmReativacao, tmAlteracao, tmLiquidacao, tmExclusao]);
end;

function InMovimentoToStr(const t: TInstrucaoMovimento): String;
begin
 result := EnumeradoToStr(t, ['00', '09', '10', '11', '17', '19', '23',
                                       '25', '27', '33', '40', '99'],
                              [imInclusaoRegistroDetalheLiberado,
                               imInclusaoRegistroDetalheBloqueado,
                               imAltecacaoPagamentoLiberadoparaBloqueio,
                               imAlteracaoPagamentoBloqueadoparaLiberado,
                               imAlteracaoValorTitulo,
                               imAlteracaoDataPagamento,
                               imPagamentoDiretoFornecedor,
                               imManutencaoemCarteira,
                               imRetiradadeCarteira,
                               imEstornoDevolucaoCamaraCentralizadora,
                               imAlegacaoSacado,
                               imExclusaoRegistro]);
end;

function StrToInMovimento(var ok: boolean; const s: string): TInstrucaoMovimento;
begin
 result := StrToEnumerado(ok, s, ['00', '09', '10', '11', '17', '19',
                                  '23', '25', '27', '33', '40', '99'],
                                 [imInclusaoRegistroDetalheLiberado,
                                  imInclusaoRegistroDetalheBloqueado,
                                  imAltecacaoPagamentoLiberadoparaBloqueio,
                                  imAlteracaoPagamentoBloqueadoparaLiberado,
                                  imAlteracaoValorTitulo,
                                  imAlteracaoDataPagamento,
                                  imPagamentoDiretoFornecedor,
                                  imManutencaoemCarteira,
                                  imRetiradadeCarteira,
                                  imEstornoDevolucaoCamaraCentralizadora,
                                  imAlegacaoSacado,
                                  imExclusaoRegistro]);
end;

function TpMoedaToStr(const t: TTipoMoeda): String;
begin
  result := EnumeradoToStr(t, ['BTN', 'BRL', 'USD', 'PTE', 'FRF', 'CHF', 'JPY',
                               'IGP', 'IGM', 'GBP', 'ITL', 'DEM', 'TRD', 'UPC',
                               'UPF', 'UFR', 'XEU'],
                   [tmBonusTesouroNacional, tmReal, tmDolarAmericano,
                    tmEscudoPortugues, tmFrancoFrances, tmFrancoSuico,
                    tmIenJapones, tmIndiceGeralPrecos, tmIndiceGeralPrecosMercado,
                    tmLibraEsterlina, tmMarcoAlemao, tmTaxaReferencialDiaria,
                    tmUnidadePadraoCapital, tmUnidadePadraoFinanciamento,
                    tmUnidadeFiscalReferencia, tmUnidadeMonetariaEuropeia]);
end;

function StrToTpMoeda(var ok: boolean; const s: string): TTipoMoeda;
begin
  result := StrToEnumerado(ok, s,
                   ['BTN', 'BRL', 'USD', 'PTE', 'FRF', 'CHF', 'JPY',
                    'IGP', 'IGM', 'GBP', 'ITL', 'DEM', 'TRD', 'UPC',
                    'UPF', 'UFR', 'XEU'],
                   [tmBonusTesouroNacional, tmReal, tmDolarAmericano,
                    tmEscudoPortugues, tmFrancoFrances, tmFrancoSuico,
                    tmIenJapones, tmIndiceGeralPrecos, tmIndiceGeralPrecosMercado,
                    tmLibraEsterlina, tmMarcoAlemao, tmTaxaReferencialDiaria,
                    tmUnidadePadraoCapital, tmUnidadePadraoFinanciamento,
                    tmUnidadeFiscalReferencia, tmUnidadeMonetariaEuropeia]);
end;

function TpIndTributoToStr(const t: TIndTributo): String;
begin
  result := EnumeradoToStr(t, ['00', '16', '18', '17', '21', '25', '26', '27'],
                          [itNenhum, itDARFNormal, itDARFSimples, itGPS, itDARJ,
                           itIPVA, itLicenciamento, itDPVAT]);
end;

function StrToIndTributo(var ok: boolean; const s:string): TIndTributo;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '16', '18', '17', '21', '25', '26', '27'],
                           [itNenhum, itDARFNormal, itDARFSimples, itGPS, itDARJ,
                           itIPVA, itLicenciamento, itDPVAT]);
end;

function TpOperacaoToStr(const t: TTipoOperacao): String;
begin
  result := EnumeradoToStr(t, ['C', 'D', 'E', 'G', 'I', 'R', 'T'],
                            [toCredito, toDebito, toExtrato, toGestao,
                             toInformacao, toRemessa, toRetorno]);
end;

function StrToTpOperacao(var ok: boolean; const s:string): TTipoOperacao;
begin
  result := StrToEnumerado(ok, s,
                           ['C', 'D', 'E', 'G', 'I', 'R', 'T'],
                           [toCredito, toDebito, toExtrato, toGestao,
                            toInformacao, toRemessa, toRetorno]);
end;

function TpMovimentoPagtoToStr(const t: TTipoMovimentoPagto): String;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7', '9'],
                            [tmpCredito,
                             tmpDebito,
                             tmpAcumulado,
                             tmpRendimentoTributavelDeducaoIRRF,
                             tmpRendimentoIsentoNaoTributavel,
                             tmpRendimentoSujeitoTributacaoExclusiva,
                             tmpInformacaoComplementar,
                             tmpRendimentoRecebidoAcumuladamente]);
end;

function StrToTpMovimentoPagto(var ok:boolean; const s:string): TTipoMovimentoPagto;
begin
  result := StrToEnumerado(ok, s,
                               ['1', '2', '3', '4', '5', '6', '7', '9'],
                               [tmpCredito,
                                tmpDebito,
                                tmpAcumulado,
                                tmpRendimentoTributavelDeducaoIRRF,
                                tmpRendimentoIsentoNaoTributavel,
                                tmpRendimentoSujeitoTributacaoExclusiva,
                                tmpInformacaoComplementar,
                                tmpRendimentoRecebidoAcumuladamente]);
end;

function CodigoPagamentoGpsToStr(const t: TCodigoPagamentoGps): String;
begin
  result := EnumeradoToStr(t, ['1007', '1104', '1406', '1457', '1503', '1554',
                               '2003', '2100', '2208', '2631', '2909'],
                            [cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                             cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                             cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                             cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                             cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                             cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                             cpgEmpresasOptantesSimplesCNPJ,
                             cpgEmpresasGeralCNPJ,
                             cpgEmpresasGeralCEI,
                             cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                             cpgReclamatoriaTrabalhistaCNPJ]);
end;

function StrToCodigoPagamentoGps(var ok: boolean; const s: string): TCodigoPagamentoGps;
begin
  result := StrToEnumerado(ok, s, ['1007', '1104', '1406', '1457', '1503', '1554',
                                   '2003', '2100', '2208', '2631', '2909'],
                                  [cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                                   cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                                   cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                                   cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                                   cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                                   cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                                   cpgEmpresasOptantesSimplesCNPJ,
                                   cpgEmpresasGeralCNPJ,
                                   cpgEmpresasGeralCEI,
                                   cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                                   cpgReclamatoriaTrabalhistaCNPJ]);
end;

function TipoChavePixToStr(const t: TTipoChavePIX): String;
begin
  result := EnumeradoToStr(t, ['  ', '01', '02', '03', '04', '05'],
                  [tcpnenhum, tcpTelefone, tcpEmail, tcpCPFCNPJ, tcpAleatoria, tcpDadosBancarios]);
end;

function StrToTipoChavePIX(var ok:boolean; const s: string): TTipoChavePIX;
begin
  result := StrToEnumerado(ok, s, ['', '  ', '01', '02', '03', '04', '05'],
       [tcpNenhum, tcpNenhum, tcpTelefone, tcpEmail, tcpCPFCNPJ, tcpAleatoria, tcpDadosBancarios]);
end;

function TpTributoToStr(const t: TTipoTributo): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '07', '08', '', '11'],
                            [ttGPS, ttDARFNormal, ttDARFSimples, ttDARJ, ttGareICMS,
                             ttIPVA, ttDPVAT, ttLicenciamento, ttFGTS])
end;

function LinhaDigitavelParaBarras(const linha:string):string;
begin
  if length(linha) <> 47 then
    raise Exception.Create('O tamanho da string não corresponde a uma linha digitável!')
  else
    Result := copy(linha, 01, 01) + copy(linha, 02, 03) + copy(linha, 33, 01) +
              copy(linha, 34, 04) + copy(linha, 38, 10) + copy(linha, 05, 05) +
              copy(linha, 11, 10) + copy(linha, 22, 10);
end;

function StrToTOcorrencia(var ok:boolean; const s: string): TOcorrencia;
begin
  result := StrToEnumerado(ok, s,
                ['  ',
                 '00', '01', '02', '03',
                 'AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AI', 'AJ',
                 'AK', 'AL', 'AM', 'AN', 'AO', 'AP', 'AQ', 'AR', 'AS', 'AT',
                 'AU', 'AV', 'AW', 'AX', 'AY', 'AZ',
                 'BA', 'BB', 'BC', 'BD', 'BE', 'BF', 'BG', 'BH', 'BI', 'BJ',
                 'BK', 'BL', 'BM', 'BN', 'BO', 'BP', 'BQ', 'BR', 'BS',
                 'CA', 'CB', 'CC', 'CD', 'CE', 'CF', 'CG', 'CH', 'CI', 'CJ',
                 'CK', 'CL', 'CM', 'CN', 'CO', 'CP', 'CQ', 'CR', 'CS',
                 'DA', 'DB', 'DC', 'DD', 'DE', 'DF', 'DG', 'DH', 'DI', 'DJ',
                 'DK', 'DL', 'DM', 'DV',
                 'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9',
                 'EM', 'EX', 'E0', 'E1', 'E2', 'E3', 'E4',
                 'FC', 'FD',
                 'HA', 'HB', 'HC', 'HD', 'HE', 'HF', 'HG', 'HH', 'HI', 'HJ',
                 'HK', 'HL', 'HM', 'HN', 'HO', 'HP', 'HQ', 'HR', 'HS', 'HT',
                 'HU', 'HV', 'HW', 'HX', 'HY', 'HZ', 'H1', 'H2', 'H3', 'H4',
                 'H5', 'H6', 'H7', 'H8', 'H9',
                 'IA', 'IB', 'IC', 'ID', 'IE', 'IF', 'IG', 'IH', 'II', 'IJ',
                 'IK', 'IL', 'IM', 'IN', 'IO', 'IP', 'IQ', 'IR',
                 'IS', 'IT', 'IU', 'IV', 'IX',
                 'LA', 'LC',
                 'NA', 'NB', 'NC', 'ND', 'NE', 'NF', 'NG', 'NH', 'NI', 'NR',
                 'PA', 'PB', 'PC', 'PD', 'PE', 'PF', 'PG', 'PH', 'PI', 'PJ',
                 'PK', 'PL', 'PM', 'PN',
                 'RJ', 'RS', 'SS',
                 'TA', 'TI',
                 'X1', 'X2', 'X3', 'X4',
                 'YA', 'YB', 'YC', 'YD', 'YE', 'YF',
                 'ZA', 'ZB', 'ZC', 'ZD', 'ZE', 'ZF', 'ZG', 'ZH', 'ZI', 'ZJ',
                 'ZK'],
                [toEsp,
                 to00, to01, to02, to03,
                 toAA, toAB, toAC, toAD, toAE, toAF, toAG, toAH, toAI, toAJ,
                 toAK, toAL, toAM, toAN, toAO, toAP, toAQ, toAR, toAS, toAT,
                 toAU, toAV, toAW, toAX, toAY, toAZ,
                 toBA, toBB, toBC, toBD, toBE, toBF, toBG, toBH, toBI, toBJ,
                 toBK, toBL, toBM, toBN, toBO, toBP, toBQ, toBR, toBS,
                 toCA, toCB, toCC, toCD, toCE, toCF, toCG, toCH, toCI, toCJ,
                 toCK, toCL, toCM, toCN, toCO, toCP, toCQ, toCR, toCS,
                 toDA, toDB, toDC, toDD, toDE, toDF, toDG, toDH, toDI, toDJ,
                 toDK, toDL, toDM, toDV,
                 toD0, toD1, toD2, toD3, toD4, toD5, toD6, toD7, toD8, toD9,
                 toEM, toEX, toE0, toE1, toE2, toE3, toE4,
                 toFC, toFD,
                 toHA, toHB, toHC, toHD, toHE, toHF, toHG, toHH, toHI, toHJ,
                 toHK, toHL, toHM, toHN, toHO, toHP, toHQ, toHR, toHS, toHT,
                 toHU, toHV, toHW, toHX, toHY, toHZ, toH1, toH2, toH3, toH4,
                 toH5, toH6, toH7, toH8, toH9,
                 toIA, toIB, toIC, toID, toIE, toIF, toIG, toIH, toII, toIJ,
                 toIK, toIL, toIM, toIN, toIO, toIP, toIQ, toIR,
                 toIS, toIT, toIU, toIV, toIX,
                 toLA, toLC,
                 toNA, toNB, toNC, toND, toNE, toNF, toNG, toNH, toNI, toNR,
                 toPA, toPB, toPC, toPD, toPE, toPF, toPG, toPH, toPI, toPJ,
                 toPK, toPL, toPM, toPN,
                 toRJ, toRS, toSS,
                 toTA, toTI,
                 toX1, toX2, toX3, toX4,
                 toYA, toYB, toYC, toYD, toYE, toYF,
                 toZA, toZB, toZC, toZD, toZE, toZF, toZG, toZH, toZI, toZJ,
                 toZK]);
end;

function StrToNaturezaLanc(var ok: boolean; const s: String): TNaturezaLanc;
begin
  result := StrToEnumerado(ok, s, ['DPV', 'SCR', 'SSR', 'CDS'],
                                  [nlDPV, nlSCR, nlSSR, nlCDS]);
end;

function IndFormaPagToStr(const t: TIndFormaPag): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03'],
                            [ifpDebitoContaCorrente, ifpDebitoEmprestimo,
                             ifpDebitoCartaoCredito])
end;

end.

