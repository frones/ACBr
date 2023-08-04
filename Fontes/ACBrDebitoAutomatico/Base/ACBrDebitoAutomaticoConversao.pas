{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit ACBrDebitoAutomaticoConversao;

interface

uses
  SysUtils, StrUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Classes;

type
  TTipoCampo = (tcStr, tcStrZero, tcInt, tcInt64, tcDe2, tcDe5, tcDe8, tcDat,
                tcHor, tcDatISO);

  TBanco = (debNenhum,
     debBancodoBrasil, debBancoDigito, debNuBank,
     debPagSeguro, debMercadoPago, debBradesco, debSofisaDireto, debInter, debItau,
     debCaixaEconomica, debSantander, debOriginal, debBanCooB, debVotorantim,
     debBanrisul, debSafra, debBRB, debUnicredCooperativa, debBancoRibeiraoPreto,
     debCetelem, debSemear, debPlannerCorretora, debB3, debRabobank, debSicredi,
     debBNPParibasBrasil, debUnicredCentralRS, debKirtonBank, debPortoCred,
     debKebHanaBrasil, debXPInvestimentos, debBancoXP, debSuperPagamentos,
     debGerencianetPagamentos, debUniprimeNortedoParana, debCapitalMarkets,
     debMorganStanley, debUBSBrasilCCTVM, debTrevisoCC, debHipercardBancoMultiplo,
     debJSafra, debUniprimeCentral, debAlfa, debABNAmro, debCargill, debServiCoop,
     debBradescard, debNovaFutura, debGoldmanSachsBrasil, debCCCNoroesteBrasileiro,
     debCCMDespTransSCeRS, debInbursa, debBancodaAmazonia, debConfidenceCC,
     debBancodoEstadodoPara, debCasaCredito, debAlbatrossCCV, debBancoCECRED,
     debCooperativaCreditoEspiritoSanto, debBancoBBI, debBradescoFinanciamentos,
     debBancoDoNordeste, debCCBBrasil, debHSFinanceira, debLeccaCFI,
     debKDBBrasil, debTopazio, debCCROuro, debPolocred, debCCRSaoMigueldoOeste,
     debICAPBrasil, debSocred, debNatixisBrasil, debCaruana,
     debCodepeCVC, debOriginalAgronegocio, debBancoBrasileiroNegocios,
     debStandardChartered, debCresol, debAgibank, debBancodaChinaBrasil,
     debGetMoneyCC, debBANDEPE, debConfidenceCambio, debFinaxis, debSenff,
     debMultiMoneyCC, debBRK, debBancodoEstadodeSergipe, debBEXSBancodeCambio,
     debBRPartners, debBPP, debBRLTrustDTVM, debWesternUniondoBrasil,
     debParanaBanco, debBariguiCH, debBOCOMBBM, debCapital, debWooriBank, debFacta,
     debStone, debBrokerBrasilCC, debMercantil, debItauBBA, debTriangulo,
     debSenso, debICBCBrasil, debVipsCC, debUBSBrasil, debMSBank, debMarmetal,
     debVortx, debCommerzbank, debAvista, debGuittaCC, debCCRPrimaveraDoLeste,
     debDacasaFinanceira, debGenial, debIBCCTVM, debBANESTES, debABCBrasil,
     debScotiabankBrasil, debBTGPactual, debModal, debClassico, debGuanabara,
     debIndustrialdoBrasil, debCreditSuisse, debFairCC, debLaNacionArgentina,
     debCitibankNA, debCedula, debBradescoBERJ, debJPMorgan, debCaixaGeralBrasil,
     debCitibank, debRodobens, debFator, debBNDES, debAtivaInvestimentos,
     debBGCLiquidez, debAlvorada, debItauConsignado, debMaxima,
     debHaitongBi, debOliveiraTrust, debBNYMellonBanco, debPernambucabasFinanc,
     debLaProvinciaBuenosAires, debBrasilPlural, debJPMorganChaseBank, debAndbank,
     debINGBankNV, debBCV, debLevycamCCV, debRepOrientalUruguay, debBEXSCC,
     debHSBC, debArbi, debIntesaSanPaolo, debTricury, debInterCap, debFibra,
     debLusoBrasileiro, debPAN, debBradescoCartoes, debItauBank, debMUFGBrasil,
     debSumitomoMitsui, debOmniBanco, debItauUnibancoHolding, debIndusval,
     debCrefisa, debMizuhodoBrasil, debInvestcredUni, debBMG, debFicsa,
     debSagiturCC, debSocieteGeneraleBrasil, debMagliano, debTullettPrebon,
     debCreditSuisseHedgingGriffo, debPaulista, debBankofAmericaMerrillLynch,
     debCCRRegMogiana, debPine, debEasynvest, debDaycoval, debCarol,
     debRenascenca, debDeutscheBank, debCifra, debGuide, debRendimento, debBS2,
     debBS2DistribuidoraTitulos, debOleBonsucessoConsignado, debLastroRDV,
     debFrenteCC, debBTCC, debNovoBancoContinental, debCreditAgricoleBrasil,
     debBancoSistema, debCredialianca, debVR, debBancoOurinvest, debCredicoamo,
     debRBCapitalInvestimentos, debJohnDeere, debAdvanced, debC6, debDigimais);


  TDebitoLayoutVersao = (lv4, lv5, lv6, lv8);

  TDebitoTipoArquivo = (taRemessa, taRetorno);

  TDebitoMovimentoCadastro = (mcExclusao, mcInclusao);

  TDebitoMovimentoAlteracao = (maAlteracao, maExclusao);

  TDebitoMoeda = (mUFIR, mREAL);

  TDebitoCPFCNPJ = (cCNPJ, cCPF);

  TDebitoUsoEmpresaXY = (ueX, ueY, ueVazio);

  TDebitoSituacaoAgencia = (saAtiva, saRegimeEncerramento);

  TDebitoAlteracaoOpcao = (aoSemAlteracao, aoSim, aoNaoAlterar);

  TDebitoTipoOperacao = (toArredondamentoMercantil, toOperacaoCredito, toOutro);

  TDebitoSimNao = (snSim, snNao);

  TDebitoMovimentoDebito = (mdDebitoNormal, mdCancelamento, mdInclusaoOptanteDebAut);

  TDebitoRetorno = (trEsp,
                    tr00, tr01, tr02, tr04, tr05, tr10,
                    tr12, tr13, tr14, tr15, tr18, tr19, tr20,
                    tr30, tr31,
                    tr96, tr97, tr98, tr99,
                    trDP, trFP, trCF, trNC, trCH, trPV, trDT, trOP, trCE,
                    trCD, trPB);

function StrToEnumerado(var ok: boolean; const s: string; const AString: array of string; const AEnumerados: array of variant): variant;
function EnumeradoToStr(const t: variant; const AString: array of string; const AEnumerados: array of variant): variant;

function BancoToStr(const t: TBanco): String;
function BancoToDesc(const t: TBanco): String;
function StrToBanco(var ok: boolean; const s: String): TBanco;
function BancoToIspb(const t: TBanco): String;

function TipoArquivoToStr(const t: TDebitoTipoArquivo): String;
function StrToTipoArquivo(var ok: boolean; const s: String): TDebitoTipoArquivo;

function LayoutVersaoToStr(const t: TDebitoLayoutVersao): String;
function StrToLayoutVersao(var ok: boolean; const s: String): TDebitoLayoutVersao;

function MovimentoCadastroToStr(const t: TDebitoMovimentoCadastro): String;
function StrToMovimentoCadastro(var ok: boolean; const s: String): TDebitoMovimentoCadastro;

function AlteracaoOpcaoToStr(const t: TDebitoAlteracaoOpcao): String;
function StrToAlteracaoOpcao(var ok: boolean; const s: String): TDebitoAlteracaoOpcao;

function MovimentoAlteracaoToStr(const t: TDebitoMovimentoAlteracao): String;
function StrToMovimentoAlteracao(var ok: boolean; const s: String): TDebitoMovimentoAlteracao;

function MoedaToStr(const t: TDebitoMoeda): String;
function StrToMoeda(var ok: boolean; const s: String): TDebitoMoeda;

function UsoEmpresaXYToStr(const t: TDebitoUsoEmpresaXY): String;
function StrToUsoEmpresaXY(var ok: boolean; const s: String): TDebitoUsoEmpresaXY;

function CPFCNPJToStr(const t: TDebitoCPFCNPJ): String;
function StrToCPFCNPJ(var ok: boolean; const s: String): TDebitoCPFCNPJ;

function TipoOperacaoToStr(const t: TDebitoTipoOperacao): String;
function StrToTipoOperacao(var ok: boolean; const s: String): TDebitoTipoOperacao;

function SimNaoToStr(const t: TDebitoSimNao): String;
function StrToSimNao(var ok: boolean; const s: String): TDebitoSimNao;

function MovimentoDebitoToStr(const t: TDebitoMovimentoDebito): String;
function StrToMovimentoDebito(var ok: boolean; const s: String): TDebitoMovimentoDebito;

function SituacaoAgenciaToStr(const t: TDebitoSituacaoAgencia): String;
function StrToSituacaoAgencia(var ok: boolean; const s: String): TDebitoSituacaoAgencia;

function RetornoToStr(const t: TDebitoRetorno): String;
function StrToRetorno(var ok: boolean; const s: String): TDebitoRetorno;

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
    [debNenhum,
     debBancodoBrasil, debBancoDigito, debNuBank,
     debPagSeguro, debMercadoPago, debBradesco, debSofisaDireto, debInter, debItau,
     debCaixaEconomica, debSantander, debOriginal, debBanCooB, debVotorantim,
     debBanrisul, debSafra, debBRB, debUnicredCooperativa, debBancoRibeiraoPreto,
     debCetelem, debSemear, debPlannerCorretora, debB3, debRabobank, debSicredi,
     debBNPParibasBrasil, debUnicredCentralRS, debKirtonBank, debPortoCred,
     debKebHanaBrasil, debXPInvestimentos, debBancoXP, debSuperPagamentos,
     debGerencianetPagamentos, debUniprimeNortedoParana, debCapitalMarkets,
     debMorganStanley, debUBSBrasilCCTVM, debTrevisoCC, debHipercardBancoMultiplo,
     debJSafra, debUniprimeCentral, debAlfa, debABNAmro, debCargill, debServiCoop,
     debBradescard, debNovaFutura, debGoldmanSachsBrasil, debCCCNoroesteBrasileiro,
     debCCMDespTransSCeRS, debInbursa, debBancodaAmazonia, debConfidenceCC,
     debBancodoEstadodoPara, debCasaCredito, debAlbatrossCCV, debBancoCECRED,
     debCooperativaCreditoEspiritoSanto, debBancoBBI, debBradescoFinanciamentos,
     debBancoDoNordeste, debCCBBrasil, debHSFinanceira, debLeccaCFI,
     debKDBBrasil, debTopazio, debCCROuro, debPolocred, debCCRSaoMigueldoOeste,
     debICAPBrasil, debSocred, debNatixisBrasil, debCaruana,
     debCodepeCVC, debOriginalAgronegocio, debBancoBrasileiroNegocios,
     debStandardChartered, debCresol, debAgibank, debBancodaChinaBrasil,
     debGetMoneyCC, debBANDEPE, debConfidenceCambio, debFinaxis, debSenff,
     debMultiMoneyCC, debBRK, debBancodoEstadodeSergipe, debBEXSBancodeCambio,
     debBRPartners, debBPP, debBRLTrustDTVM, debWesternUniondoBrasil,
     debParanaBanco, debBariguiCH, debBOCOMBBM, debCapital, debWooriBank, debFacta,
     debStone, debBrokerBrasilCC, debMercantil, debItauBBA, debTriangulo,
     debSenso, debICBCBrasil, debVipsCC, debUBSBrasil, debMSBank, debMarmetal,
     debVortx, debCommerzbank, debAvista, debGuittaCC, debCCRPrimaveraDoLeste,
     debDacasaFinanceira, debGenial, debIBCCTVM, debBANESTES, debABCBrasil,
     debScotiabankBrasil, debBTGPactual, debModal, debClassico, debGuanabara,
     debIndustrialdoBrasil, debCreditSuisse, debFairCC, debLaNacionArgentina,
     debCitibankNA, debCedula, debBradescoBERJ, debJPMorgan, debCaixaGeralBrasil,
     debCitibank, debRodobens, debFator, debBNDES, debAtivaInvestimentos,
     debBGCLiquidez, debAlvorada, debItauConsignado, debMaxima,
     debHaitongBi, debOliveiraTrust, debBNYMellonBanco, debPernambucabasFinanc,
     debLaProvinciaBuenosAires, debBrasilPlural, debJPMorganChaseBank, debAndbank,
     debINGBankNV, debBCV, debLevycamCCV, debRepOrientalUruguay, debBEXSCC,
     debHSBC, debArbi, debIntesaSanPaolo, debTricury, debInterCap, debFibra,
     debLusoBrasileiro, debPAN, debBradescoCartoes, debItauBank, debMUFGBrasil,
     debSumitomoMitsui, debOmniBanco, debItauUnibancoHolding, debIndusval,
     debCrefisa, debMizuhodoBrasil, debInvestcredUni, debBMG, debFicsa,
     debSagiturCC, debSocieteGeneraleBrasil, debMagliano, debTullettPrebon,
     debCreditSuisseHedgingGriffo, debPaulista, debBankofAmericaMerrillLynch,
     debCCRRegMogiana, debPine, debEasynvest, debDaycoval, debCarol,
     debRenascenca, debDeutscheBank, debCifra, debGuide, debRendimento, debBS2,
     debBS2DistribuidoraTitulos, debOleBonsucessoConsignado, debLastroRDV,
     debFrenteCC, debBTCC, debNovoBancoContinental, debCreditAgricoleBrasil,
     debBancoSistema, debCredialianca, debVR, debBancoOurinvest, debCredicoamo,
     debRBCapitalInvestimentos, debJohnDeere, debAdvanced, debC6, debDigimais]);
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
    [debNenhum,
     debBancodoBrasil, debBancoDigito, debNuBank, debPagSeguro, debMercadoPago,
     debSofisaDireto, debInter, debItau, debCaixaEconomica, debSantander,
     debOriginal, debBanCooB, debVotorantim, debBanrisul, debBradesco, debSafra,
     debBRB, debUnicredCooperativa, debBancoRibeiraoPreto, debCetelem, debSemear,
     debPlannerCorretora, debB3, debRabobank, debSicredi, debBNPParibasBrasil,
     debUnicredCentralRS, debKirtonBank, debPortoCred, debKebHanaBrasil,
     debXPInvestimentos, debBancoXP, debSuperPagamentos,
     debGerencianetPagamentos, debUniprimeNortedoParana, debCapitalMarkets,
     debMorganStanley, debUBSBrasilCCTVM, debTrevisoCC, debHipercardBancoMultiplo,
     debJSafra, debUniprimeCentral, debAlfa, debABNAmro, debCargill, debServiCoop,
     debBradescard, debNovaFutura, debGoldmanSachsBrasil, debCCCNoroesteBrasileiro,
     debCCMDespTransSCeRS, debInbursa, debBancodaAmazonia, debConfidenceCC,
     debBancodoEstadodoPara, debCasaCredito, debAlbatrossCCV, debBancoCECRED,
     debCooperativaCreditoEspiritoSanto, debBancoBBI, debBradescoFinanciamentos,
     debBancoDoNordeste, debCCBBrasil, debHSFinanceira, debLeccaCFI,
     debKDBBrasil, debTopazio, debCCROuro, debPolocred, debCCRSaoMigueldoOeste,
     debICAPBrasil, debSocred, debNatixisBrasil, debCaruana,
     debCodepeCVC, debOriginalAgronegocio, debBancoBrasileiroNegocios,
     debStandardChartered, debCresol, debAgibank, debBancodaChinaBrasil,
     debGetMoneyCC, debBANDEPE, debConfidenceCambio, debFinaxis, debSenff,
     debMultiMoneyCC, debBRK, debBancodoEstadodeSergipe, debBEXSBancodeCambio,
     debBRPartners, debBPP, debBRLTrustDTVM, debWesternUniondoBrasil,
     debParanaBanco, debBariguiCH, debBOCOMBBM, debCapital, debWooriBank, debFacta,
     debStone, debBrokerBrasilCC, debMercantil, debItauBBA, debTriangulo,
     debSenso, debICBCBrasil, debVipsCC, debUBSBrasil, debMSBank, debMarmetal,
     debVortx, debCommerzbank, debAvista, debGuittaCC, debCCRPrimaveraDoLeste,
     debDacasaFinanceira, debGenial, debIBCCTVM, debBANESTES, debABCBrasil,
     debScotiabankBrasil, debBTGPactual, debModal, debClassico, debGuanabara,
     debIndustrialdoBrasil, debCreditSuisse, debFairCC, debLaNacionArgentina,
     debCitibankNA, debCedula, debBradescoBERJ, debJPMorgan, debCaixaGeralBrasil,
     debCitibank, debRodobens, debFator, debBNDES, debAtivaInvestimentos,
     debBGCLiquidez, debAlvorada, debItauConsignado, debMaxima,
     debHaitongBi, debOliveiraTrust, debBNYMellonBanco, debPernambucabasFinanc,
     debLaProvinciaBuenosAires, debBrasilPlural, debJPMorganChaseBank, debAndbank,
     debINGBankNV, debBCV, debLevycamCCV, debRepOrientalUruguay, debBEXSCC,
     debHSBC, debArbi, debIntesaSanPaolo, debTricury, debInterCap, debFibra,
     debLusoBrasileiro, debPAN, debBradescoCartoes, debItauBank, debMUFGBrasil,
     debSumitomoMitsui, debOmniBanco, debItauUnibancoHolding, debIndusval,
     debCrefisa, debMizuhodoBrasil, debInvestcredUni, debBMG, debFicsa,
     debSagiturCC, debSocieteGeneraleBrasil, debMagliano, debTullettPrebon,
     debCreditSuisseHedgingGriffo, debPaulista, debBankofAmericaMerrillLynch,
     debCCRRegMogiana, debPine, debEasynvest, debDaycoval, debCarol,
     debRenascenca, debDeutscheBank, debCifra, debGuide, debRendimento, debBS2,
     debBS2DistribuidoraTitulos, debOleBonsucessoConsignado, debLastroRDV,
     debFrenteCC, debBTCC, debNovoBancoContinental, debCreditAgricoleBrasil,
     debBancoSistema, debCredialianca, debVR, debBancoOurinvest, debCredicoamo,
     debRBCapitalInvestimentos, debJohnDeere, debAdvanced, debC6, debDigimais]);
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
    [debNenhum,
     debBancodoBrasil, debBancoDigito, debNuBank,
     debPagSeguro, debMercadoPago, debBradesco, debSofisaDireto, debInter, debItau,
     debCaixaEconomica, debSantander, debOriginal, debBanCooB, debVotorantim,
     debBanrisul, debSafra, debBRB, debUnicredCooperativa, debBancoRibeiraoPreto,
     debCetelem, debSemear, debPlannerCorretora, debB3, debRabobank, debSicredi,
     debBNPParibasBrasil, debUnicredCentralRS, debKirtonBank, debPortoCred,
     debKebHanaBrasil, debXPInvestimentos, debBancoXP, debSuperPagamentos,
     debGerencianetPagamentos, debUniprimeNortedoParana, debCapitalMarkets,
     debMorganStanley, debUBSBrasilCCTVM, debTrevisoCC, debHipercardBancoMultiplo,
     debJSafra, debUniprimeCentral, debAlfa, debABNAmro, debCargill, debServiCoop,
     debBradescard, debNovaFutura, debGoldmanSachsBrasil, debCCCNoroesteBrasileiro,
     debCCMDespTransSCeRS, debInbursa, debBancodaAmazonia, debConfidenceCC,
     debBancodoEstadodoPara, debCasaCredito, debAlbatrossCCV, debBancoCECRED,
     debCooperativaCreditoEspiritoSanto, debBancoBBI, debBradescoFinanciamentos,
     debBancoDoNordeste, debCCBBrasil, debHSFinanceira, debLeccaCFI,
     debKDBBrasil, debTopazio, debCCROuro, debPolocred, debCCRSaoMigueldoOeste,
     debICAPBrasil, debSocred, debNatixisBrasil, debCaruana,
     debCodepeCVC, debOriginalAgronegocio, debBancoBrasileiroNegocios,
     debStandardChartered, debCresol, debAgibank, debBancodaChinaBrasil,
     debGetMoneyCC, debBANDEPE, debConfidenceCambio, debFinaxis, debSenff,
     debMultiMoneyCC, debBRK, debBancodoEstadodeSergipe, debBEXSBancodeCambio,
     debBRPartners, debBPP, debBRLTrustDTVM, debWesternUniondoBrasil,
     debParanaBanco, debBariguiCH, debBOCOMBBM, debCapital, debWooriBank, debFacta,
     debStone, debBrokerBrasilCC, debMercantil, debItauBBA, debTriangulo,
     debSenso, debICBCBrasil, debVipsCC, debUBSBrasil, debMSBank, debMarmetal,
     debVortx, debCommerzbank, debAvista, debGuittaCC, debCCRPrimaveraDoLeste,
     debDacasaFinanceira, debGenial, debIBCCTVM, debBANESTES, debABCBrasil,
     debScotiabankBrasil, debBTGPactual, debModal, debClassico, debGuanabara,
     debIndustrialdoBrasil, debCreditSuisse, debFairCC, debLaNacionArgentina,
     debCitibankNA, debCedula, debBradescoBERJ, debJPMorgan, debCaixaGeralBrasil,
     debCitibank, debRodobens, debFator, debBNDES, debAtivaInvestimentos,
     debBGCLiquidez, debAlvorada, debItauConsignado, debMaxima,
     debHaitongBi, debOliveiraTrust, debBNYMellonBanco, debPernambucabasFinanc,
     debLaProvinciaBuenosAires, debBrasilPlural, debJPMorganChaseBank, debAndbank,
     debINGBankNV, debBCV, debLevycamCCV, debRepOrientalUruguay, debBEXSCC,
     debHSBC, debArbi, debIntesaSanPaolo, debTricury, debInterCap, debFibra,
     debLusoBrasileiro, debPAN, debBradescoCartoes, debItauBank, debMUFGBrasil,
     debSumitomoMitsui, debOmniBanco, debItauUnibancoHolding, debIndusval,
     debCrefisa, debMizuhodoBrasil, debInvestcredUni, debBMG, debFicsa,
     debSagiturCC, debSocieteGeneraleBrasil, debMagliano, debTullettPrebon,
     debCreditSuisseHedgingGriffo, debPaulista, debBankofAmericaMerrillLynch,
     debCCRRegMogiana, debPine, debEasynvest, debDaycoval, debCarol,
     debRenascenca, debDeutscheBank, debCifra, debGuide, debRendimento, debBS2,
     debBS2DistribuidoraTitulos, debOleBonsucessoConsignado, debLastroRDV,
     debFrenteCC, debBTCC, debNovoBancoContinental, debCreditAgricoleBrasil,
     debBancoSistema, debCredialianca, debVR, debBancoOurinvest, debCredicoamo,
     debRBCapitalInvestimentos, debJohnDeere, debAdvanced, debC6, debDigimais]);
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
    [debNenhum,
     debBancodoBrasil, debBancoDigito, debNuBank,
     debPagSeguro, debMercadoPago, debBradesco, debSofisaDireto, debInter, debItau,
     debCaixaEconomica, debSantander, debOriginal, debBanCooB, debVotorantim,
     debBanrisul, debSafra, debBRB, debUnicredCooperativa, debBancoRibeiraoPreto,
     debCetelem, debSemear, debPlannerCorretora, debB3, debRabobank, debSicredi,
     debBNPParibasBrasil, debUnicredCentralRS, debKirtonBank, debPortoCred,
     debKebHanaBrasil, debXPInvestimentos, debBancoXP, debSuperPagamentos,
     debGerencianetPagamentos, debUniprimeNortedoParana, debCapitalMarkets,
     debMorganStanley, debUBSBrasilCCTVM, debTrevisoCC, debHipercardBancoMultiplo,
     debJSafra, debUniprimeCentral, debAlfa, debABNAmro, debCargill, debServiCoop,
     debBradescard, debNovaFutura, debGoldmanSachsBrasil, debCCCNoroesteBrasileiro,
     debCCMDespTransSCeRS, debInbursa, debBancodaAmazonia, debConfidenceCC,
     debBancodoEstadodoPara, debCasaCredito, debAlbatrossCCV, debBancoCECRED,
     debCooperativaCreditoEspiritoSanto, debBancoBBI, debBradescoFinanciamentos,
     debBancoDoNordeste, debCCBBrasil, debHSFinanceira, debLeccaCFI,
     debKDBBrasil, debTopazio, debCCROuro, debPolocred, debCCRSaoMigueldoOeste,
     debICAPBrasil, debSocred, debNatixisBrasil, debCaruana,
     debCodepeCVC, debOriginalAgronegocio, debBancoBrasileiroNegocios,
     debStandardChartered, debCresol, debAgibank, debBancodaChinaBrasil,
     debGetMoneyCC, debBANDEPE, debConfidenceCambio, debFinaxis, debSenff,
     debMultiMoneyCC, debBRK, debBancodoEstadodeSergipe, debBEXSBancodeCambio,
     debBRPartners, debBPP, debBRLTrustDTVM, debWesternUniondoBrasil,
     debParanaBanco, debBariguiCH, debBOCOMBBM, debCapital, debWooriBank, debFacta,
     debStone, debBrokerBrasilCC, debMercantil, debItauBBA, debTriangulo,
     debSenso, debICBCBrasil, debVipsCC, debUBSBrasil, debMSBank, debMarmetal,
     debVortx, debCommerzbank, debAvista, debGuittaCC, debCCRPrimaveraDoLeste,
     debDacasaFinanceira, debGenial, debIBCCTVM, debBANESTES, debABCBrasil,
     debScotiabankBrasil, debBTGPactual, debModal, debClassico, debGuanabara,
     debIndustrialdoBrasil, debCreditSuisse, debFairCC, debLaNacionArgentina,
     debCitibankNA, debCedula, debBradescoBERJ, debJPMorgan, debCaixaGeralBrasil,
     debCitibank, debRodobens, debFator, debBNDES, debAtivaInvestimentos,
     debBGCLiquidez, debAlvorada, debItauConsignado, debMaxima,
     debHaitongBi, debOliveiraTrust, debBNYMellonBanco, debPernambucabasFinanc,
     debLaProvinciaBuenosAires, debBrasilPlural, debJPMorganChaseBank, debAndbank,
     debINGBankNV, debBCV, debLevycamCCV, debRepOrientalUruguay, debBEXSCC,
     debHSBC, debArbi, debIntesaSanPaolo, debTricury, debInterCap, debFibra,
     debLusoBrasileiro, debPAN, debBradescoCartoes, debItauBank, debMUFGBrasil,
     debSumitomoMitsui, debOmniBanco, debItauUnibancoHolding, debIndusval,
     debCrefisa, debMizuhodoBrasil, debInvestcredUni, debBMG, debFicsa,
     debSagiturCC, debSocieteGeneraleBrasil, debMagliano, debTullettPrebon,
     debCreditSuisseHedgingGriffo, debPaulista, debBankofAmericaMerrillLynch,
     debCCRRegMogiana, debPine, debEasynvest, debDaycoval, debCarol,
     debRenascenca, debDeutscheBank, debCifra, debGuide, debRendimento, debBS2,
     debBS2DistribuidoraTitulos, debOleBonsucessoConsignado, debLastroRDV,
     debFrenteCC, debBTCC, debNovoBancoContinental, debCreditAgricoleBrasil,
     debBancoSistema, debCredialianca, debVR, debBancoOurinvest, debCredicoamo,
     debRBCapitalInvestimentos, debJohnDeere, debAdvanced, debC6, debDigimais]);
end;

function TipoArquivoToStr(const t: TDebitoTipoArquivo): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [taRemessa, taRetorno]);
end;

function StrToTipoArquivo(var ok: boolean; const s: String): TDebitoTipoArquivo;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [taRemessa, taRetorno]);
end;

function LayoutVersaoToStr(const t: TDebitoLayoutVersao): String;
begin
  result := EnumeradoToStr(t, ['04', '05', '08'],
                              [lv4, lv5, lv8]);
end;

function StrToLayoutVersao(var ok: boolean; const s: String): TDebitoLayoutVersao;
begin
  result := StrToEnumerado(ok, s, ['04', '05', '08'],
                                  [lv4, lv5, lv8]);
end;

function MovimentoCadastroToStr(const t: TDebitoMovimentoCadastro): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [mcExclusao, mcInclusao]);
end;

function StrToMovimentoCadastro(var ok: boolean; const s: String): TDebitoMovimentoCadastro;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [mcExclusao, mcInclusao]);
end;

function AlteracaoOpcaoToStr(const t: TDebitoAlteracaoOpcao): String;
begin
  result := EnumeradoToStr(t, ['0', '1', '2'],
                              [aoSemAlteracao, aoSim, aoNaoAlterar]);
end;

function StrToAlteracaoOpcao(var ok: boolean; const s: String): TDebitoAlteracaoOpcao;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [aoSemAlteracao, aoSim, aoNaoAlterar]);
end;

function MovimentoAlteracaoToStr(const t: TDebitoMovimentoAlteracao): String;
begin
  result := EnumeradoToStr(t, ['0', '1'],
                              [maAlteracao, maExclusao]);
end;

function StrToMovimentoAlteracao(var ok: boolean; const s: String): TDebitoMovimentoAlteracao;
begin
  result := StrToEnumerado(ok, s, ['0', '1'],
                                  [maAlteracao, maExclusao]);
end;

function MoedaToStr(const t: TDebitoMoeda): String;
begin
  result := EnumeradoToStr(t, ['01', '03'],
                              [mUFIR, mREAL]);
end;

function StrToMoeda(var ok: boolean; const s: String): TDebitoMoeda;
begin
  result := StrToEnumerado(ok, s, ['01', '03'],
                                  [mUFIR, mREAL]);
end;

function UsoEmpresaXYToStr(const t: TDebitoUsoEmpresaXY): String;
begin
  result := EnumeradoToStr(t, ['X', 'Y', ' '],
                              [ueX, ueY, ueVazio]);
end;

function StrToUsoEmpresaXY(var ok: boolean; const s: String): TDebitoUsoEmpresaXY;
begin
  result := StrToEnumerado(ok, s, ['X', 'Y', ' '],
                                  [ueX, ueY, ueVazio]);
end;

function CPFCNPJToStr(const t: TDebitoCPFCNPJ): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [cCNPJ, cCPF]);
end;

function StrToCPFCNPJ(var ok: boolean; const s: String): TDebitoCPFCNPJ;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [cCNPJ, cCPF]);
end;

function TipoOperacaoToStr(const t: TDebitoTipoOperacao): String;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'],
                       [toArredondamentoMercantil, toOperacaoCredito, toOutro]);
end;

function StrToTipoOperacao(var ok: boolean; const s: String): TDebitoTipoOperacao;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'],
                       [toArredondamentoMercantil, toOperacaoCredito, toOutro]);
end;

function SimNaoToStr(const t: TDebitoSimNao): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [snSim, snNao]);
end;

function StrToSimNao(var ok: boolean; const s: String): TDebitoSimNao;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [snSim, snNao]);
end;

function MovimentoDebitoToStr(const t: TDebitoMovimentoDebito): String;
begin
  result := EnumeradoToStr(t, ['0', '1', '5'],
                     [mdDebitoNormal, mdCancelamento, mdInclusaoOptanteDebAut]);
end;

function StrToMovimentoDebito(var ok: boolean; const s: String): TDebitoMovimentoDebito;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '5'],
                     [mdDebitoNormal, mdCancelamento, mdInclusaoOptanteDebAut]);
end;

function SituacaoAgenciaToStr(const t: TDebitoSituacaoAgencia): String;
begin
  result := EnumeradoToStr(t, ['A', 'B'],
                              [saAtiva, saRegimeEncerramento]);
end;

function StrToSituacaoAgencia(var ok: boolean; const s: String): TDebitoSituacaoAgencia;
begin
  result := StrToEnumerado(ok, s, ['A', 'B'],
                                  [saAtiva, saRegimeEncerramento]);
end;

function RetornoToStr(const t: TDebitoRetorno): String;
begin
  result := EnumeradoToStr(t, ['  ',
                               '00', '01', '02', '04', '05', '10', '12', '13',
                               '14', '15', '18', '19', '20', '30', '31', '96',
                               '97', '98', '99', 'DP', 'FP', 'CF', 'NC', 'CH',
                               'PV', 'DT', 'OP', 'CE', 'CD', 'PB'],
                          [trEsp,
                           tr00, tr01, tr02, tr04, tr05, tr10,
                           tr12, tr13, tr14, tr15, tr18, tr19, tr20,
                           tr30, tr31,
                           tr96, tr97, tr98, tr99,
                           trDP, trFP, trCF, trNC, trCH, trPV, trDT, trOP, trCE,
                           trCD, trPB]);
end;

function StrToRetorno(var ok: boolean; const s: String): TDebitoRetorno;
begin
  result := StrToEnumerado(ok, s, ['  ',
                               '00', '01', '02', '04', '05', '10', '12', '13',
                               '14', '15', '18', '19', '20', '30', '31', '96',
                               '97', '98', '99', 'DP', 'FP', 'CF', 'NC', 'CH',
                               'PV', 'DT', 'OP', 'CE', 'CD', 'PB'],
                          [trEsp,
                           tr00, tr01, tr02, tr04, tr05, tr10,
                           tr12, tr13, tr14, tr15, tr18, tr19, tr20,
                           tr30, tr31,
                           tr96, tr97, tr98, tr99,
                           trDP, trFP, trCF, trNC, trCH, trPV, trDT, trOP, trCE,
                           trCD, trPB]);
end;

end.

