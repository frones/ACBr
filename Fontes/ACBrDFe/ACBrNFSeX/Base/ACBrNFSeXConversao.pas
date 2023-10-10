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

unit ACBrNFSeXConversao;

interface

uses
  SysUtils,
  {$IFNDEF VER130}
    Variants,
  {$ENDIF}
  Classes, typinfo, StrUtils,
  ACBrBase;

type
  TStatusACBrNFSe = (stNFSeIdle, stNFSeRecepcao, stNFSeConsultaSituacao,
                     stNFSeConsulta, stNFSeCancelamento, stNFSeSubstituicao,
                     stNFSeImprimir, stNFSeEmail, stNFSeAbrirSessao,
                     stNFSeFecharSessao, stNFSeAguardaProcesso,
                     stNFSeEnvioWebService, stNFSeGerarToken,
                     stNFSeConsultarEvento, stNFSeConsultarDFe,
                     stNFSeConsultarParam, stNFSeConsultarSeqRps,
                     stNFSeConsultarLinkNFSe);

  TVersaoNFSe = (ve100, ve101, ve102, ve103,
                 ve200, ve201, ve202, ve203, ve204);

  TLayout =(loABRASF, loProprio);

  TLayoutNFSe = (lnfsProvedor, lnfsPadraoNacionalv1);

  TStatusRPS = (srNormal, srCancelado);

  TStatusNFSe = (snNormal, snCancelado, snSubstituido);

  TnfseNaturezaOperacao = (no0, no1, no2, no3, no4, no5, no6, no7, no8, no9,
                           no10, no11, no12, no13, no14, no15, no17, no18,
                           no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
                           no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
                           no70, no71, no72, no78, no79,
                           no101, no102, no103, no104, no105, no106, no107, no108, no109,
                           no110, no111, no112, no113, no114, no115, no116, no117, no118,
                           no121,
                           no200, no201,
                           no300, no301,
                           no400,
                           no501, no511, no512, no515, no521, no522,
                           no539, no541, no549, no551,
                           no601, no611, no612, no613, no615, no616, no621, no622,
                           no701, no711, no712,
                           no901, no902, no911, no912, no921, no931, no951, no952, no971,
                           no981, no991);

  TnfseExigibilidadeISS = (exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                           exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo, exiISSFixo);

  TnfseRegimeEspecialTributacao = (retNenhum, retMicroempresaMunicipal, retEstimativa,
                                   retSociedadeProfissionais, retCooperativa,
                                   retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                                   retLucroReal, retLucroPresumido, retSimplesNacional,
                                   retImune, retEmpresaIndividualRELI, retEmpresaPP,
                                   retMicroEmpresario, retOutros, retMovimentoMensal,
                                   retISSQNAutonomos, retISSQNSociedade,
                                   retNotarioRegistrador,
                                   retTribFaturamentoVariavel, retFixo,
                                   retIsencao,retExigibSuspensaJudicial,
                                   retExigibSuspensaAdm);

  TnfseSimNao = (snSim, snNao);

  TnfseCondicaoPagamento = (cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoCredito,
                            cpCartaoDebito, cpDeposito, cpCheque, cpPIX);

  TTipoRPS = (trRPS, trNFConjugada, trCupom, trNone);

  TIndicacaoCpfCnpj = (iccCPF, iccCNPJ, iccNaoInformado);

  TSituacaoLoteRps = (sLoteNaoRecibo, sLoteNaoProcessado, sLoteProcessadoErro,
                      sLoteProcessadoSucesso, sLoteProcessadoAviso,
                      sLoteEmProcessamento, sLoteValidado, sLoteImportado);

  TDeducaoPor = (dpNenhum, dpPercentual, dpValor);

  TTipoDeducao = (tdNenhum, tdMateriais, tdSubEmpreitada, tdValor, tdVeiculacao,
                  tdPercentual, tdPercMateriais, tdIntermediacao, tdEquipamento);

  TnfseProvedor = (proNenhum,
                   proPadraoNacional,
                   proAbaco, proABase, proActcon, proAdm, proADPM, proAEG,
                   proAgili, proAssessorPublico, proAsten, proBauhaus, proBetha,
                   proBHISS, proCenti, proCIGA, proCitta, proConam, proContass,
                   proCoplan, proCTA, proCTAConsult, proDataSmart, proDBSeller,
                   proDeISS, proDesenvolve, proDigifred, proDSF, proeGoverneISS,
                   proeISS, proEL, proEloTech, proEquiplano, proeReceita, proEtherium,
                   proFacundo, proFGMaiss, profintelISS, proFiorilli, proFisco,
                   proFISSLex, proFuturize, proGeisWeb, progeNFe, proGestaoISS,
                   proGiap, proGinfes, proGiss, proGovBR, proGovDigital, proGoverna,
                   proHorus, proiiBrasil, proInfisc, proIntertec, proIPM,
                   proISSBarueri, proISSCamacari, proISSCambe, proISSCuritiba,
                   proISSDigital, proISSDSF, proISSe, proISSFortaleza,
                   proISSGoiania, proISSIntel, proISSJoinville, proISSLencois,
                   proISSNatal, proISSNet, proISSPortoVelho, proISSRecife,
                   proISSRio, proISSSalvador, proISSSaoPaulo, proISSSJP,
                   proISSVitoria, proLexsom, proLibre, proLink3, proMegaSoft,
                   proMetropolisWeb, proMitra, proModernizacaoPublica,
                   proNEAInformatica, proNFSeBrasil, proNotaInteligente,
                   proPrescon, proPriMax, proProdata, proPronim, proPublica,
                   proPublicSoft, proRLZ, proSaatri, proSafeWeb, proSH3, proSiam,
                   proSiapNet, proSiappa, proSiapSistemas, proSiat, proSigCorp,
                   proSigep, proSigISS, proSigISSWeb, proSilTecnologia, proSimple,
                   proSimplISS, proSintese, proSisPMJP, proSistemas4R, proSmarAPD,
                   proSoftPlan, proSpeedGov, proSSInformatica, proSudoeste,
                   proSysISS, proSystemPro, proTcheInfo, proTecnos, proThema,
                   proTinus, proTiplan, proTributus, proVersaTecnologia,
                   proVirtual, proWebFisco, proWebISS);

  TnfseSituacaoTributaria = (stRetencao, stNormal, stSubstituicao, stNenhum);

  TnfseResponsavelRetencao = (rtTomador, rtPrestador, rtIntermediario, rtNenhum);

  TTipoEmissao = (teNormalNFSe, teContigenciaNFSe);

  TEmpreitadaGlobal = (EgConstrucaoCivil, EgOutros);

  TTipoDANFSE = (tpPadrao, tpISSDSF, tpFiorilli);

  TnfseFrete = (tfPrestador, tfTomador);

  TTipoTributacaoRPS = (ttTribnoMun, ttTribforaMun, ttTribnoMunIsento,
                        ttTribforaMunIsento, ttTribnoMunImune, ttTribforaMunImune,
                        ttTribnoMunSuspensa, ttTribforaMunSuspensa, ttExpServicos,
                        ttSimplesNacional, ttRetidonoMun);

  TLogradouroLocalPrestacaoServico = (llpTomador, llpPrestador);

  TnfseCanhoto = (tcNenhum, tcCabecalho, tcRodape);

  TRegRec = (regNenhum, regMovimento, regCancelado, regIsento, regImune,
             regNaoIncidencia, regEstimativa, regSocLiberal,
             regSimplesNacional, regMEI);

  TFrmRec = (frmNenhum, frmNormal, frmRetidoNaFonte, frmSimplesNacional,
             frmFixoAnual, frmSemRecolhimento, frmDevidoOutroMunicipio,
             frmFixoMensal);

  TOperacao = (toSemDeducao, toComDeducaoMateriais, toImuneIsenta,
               toDevolucaoSimplesRemessa, toIntermediacao);

  TTributacao = (ttIsentaISS, ttNaoIncidencianoMunic, ttImune, ttExigibilidadeSusp,
                 ttNaoTributavel, ttTributavel, ttTributavelFixo, ttTributavelSN,
                 ttMEI);

  TUnidade = (tuHora, tuQtde);

  TMetodo = (tmRecepcionar, tmConsultarSituacao, tmConsultarLote,
             tmConsultarNFSePorRps, tmConsultarNFSe,
             tmConsultarNFSePorFaixa, tmConsultarNFSeServicoPrestado,
             tmConsultarNFSeServicoTomado, tmCancelarNFSe,
             tmGerar, tmGerarLote, tmRecepcionarSincrono, tmSubstituirNFSe,
             tmAbrirSessao, tmFecharSessao, tmTeste, tmTodos,
             tmGerarToken, tmEnviarEvento, tmConsultarEvento, tmConsultarDFe,
             tmConsultarParam, tmConsultarSeqRps, tmConsultarLinkNFSe);

  TFormatoItemListaServico = (filsComFormatacao, filsSemFormatacao,
                              filsComFormatacaoSemZeroEsquerda,
                              filsSemFormatacaoSemZeroEsquerda,
                              filsNaoSeAplica);

  TSituacaoTrib = (tsTributadaNoPrestador, tsTibutadaNoTomador, tsIsenta, tsImune,
                   tsNaoTributada, tsFixo, tsOutroMunicipio);

  TTipoPessoa = (tpPFNaoIdentificada, tpPF, tpPJdoMunicipio, tpPJforaMunicipio,
                 tpPJforaPais);

  TtpConsulta = (tcPorNumero, tcPorFaixa, tcPorPeriodo, tcServicoPrestado,
                 tcServicoTomado, tcPorCodigoVerificacao);

  TtpPeriodo = (tpEmissao, tpCompetencia);

  TmodoEnvio = (meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario,
                meTeste);

  TtpXML = (txmlRPS, txmlNFSe, txmlEspelho);

  TTipoLancamento = (tlDevidoNoMunicPrestador, tlDevidoNoMunicTomador,
                     tlSimplesNacional, tlIsentoImune, tlCancelado);

  TtpDocumento = (tdNFSe, tdRPS);

  TtpRetorno = (trXml, trPDF);

  TFormatoArq = (tfaXml, tfaJson, tfaTxt);

  // Usado pelo PadraoNacional
  TtpEmit = (tePrestador, teTomador, teIntermediario);

  TOptanteSN = (osnNaoOptante, osnOptanteMEI, osnOptanteMEEPP);

  TRegimeApuracaoSN = (raFederaisMunicipalpeloSN, raFederaisSN,
                       raFederaisMunicipalforaSN);

  TcMotivo = (cmDesenquadramento, cmEnquadramento, cmInclusao, cmExclusao,
              cmRejeicao, cmOutros);

  TmdPrestacao = (mpDesconhecido, mpTransfronteirico, mpConsumoBrasil,
                  mpPresencaComercialExterior, mpMovimentoTempPessoasFisicas);

  TvincPrest = (vpSemVinculo, vpControlada, vpControladora, vpColigada, vpMatriz,
                vpFilial, vpOutro);

  TmecAFComexP = (mapsDesconhecido, mapsNenhum, mapsACC, mapsACE, mapsBNDESPos,
                  mapsBNDESPre, mapsFGE, mapsPROEXEqual, mapsPROEXFinanc);

  TmecAFComexT = (matsDesconhecido, matsNenhum, matsAdmPublica, matsAlugueis,
                  matsArredondamento, matsComissao, matsDespesas,
                  matsEventosFIFASubsidiaria, matsEventosFIFA, matsFretes,
                  matsMaterialAeronautico, matsPromocaoBens,
                  matsPromocaoTuristicos, matsPromocaoBrasilExt,
                  matsPromocaoServicoExt, matsRECINE, matsRECOPA, matsPatentes,
                  matsREICOMP, matsREIDI, matsREPENEC, matsREPES, matsRETAERO,
                  matsRETID, matsRoyalties, matsServicosAvaliacao, matsZPE);

  TMovTempBens = (mtDesconhecido, mtNao, mtVincDeclImport, mtVincDeclExport);

  Tcateg = (cLocacao, cSubLocacao, cArrendamento, cDireitoPassagem, cPermissao);

  Tobjeto = (oFerrovia, oRodovia, oPostes, oCabos, oDutos, oCondutos);

  TcategVeic = (cvDesconhecido, cvAutomovel, cvCaminhao, cvAutomovelComSemiReboque,
                cvCaminhaoComSemiReboque, cvAutomovelComReboque,
                cvCaminhaoComReboque, cvCaminhaoTratorComSemiReboque,
                cvMotocicleta, cvVeiculoEspecial, cvVeiculoIsento);

  Trodagem = (trSimples, trDupla);

  TtpDedRed = (drAlimentacao, drMateriais, drProducaoExt, drReembolso,
               drRepasseConsorciado, drRepassePlanoSaude, drServicos,
               drSubEmpreitada, drOutrasDeducoes);

  TtribISSQN = (tiOperacaoTributavel, tiExportacao, tiNaoIncidencia, tiImunidade);

  TtpImunidade = (timNenhum, timImunidade, timPatrimonio, timTemplos,
                  timPatrimonioPartidos, timLivros, timFonogramas);

  TtpRetISSQN = (trNaoRetido, trRetidoPeloTomador, trRetidoPeloIntermediario);

  TtpBM = (tbAliquota, tbReducaoBC, tbIsencao);

  TtpSusp = (tsNenhum, tsDecisaoJudicial, tsProcessoAdm);

  TCST = (cst00, cst01, cst02, cst03, cst04, cst05, cst06, cst07, cst08, cst09);

  TtpRetPisCofins = (trpcRetido, trpcNaoRetido);

  TindTotTrib = (indNao, indSim);

  TambGer = (agPrefeitura, agSistemaNacional);

  TtpEmis = (tePadraoNacional, teProprio);

  TprocEmi = (peWebService, peWebFisco, peAppFisco);

  TtpEvento = (teCancelamento, teCancelamentoSubstituicao,
               teAnaliseParaCancelamento, teCancelamentoDeferido,
               teCancelamentoIndeferido, teConfirmacaoPrestador,
               teConfirmacaoTomador, teConfirmacaoIntermediario,
               teConfirmacaoTacita, teRejeicaoPrestador, teRejeicaoTomador,
               teRejeicaoIntermediario, teAnulacaoRejeicao,
               teCancelamentoPorOficio, teBloqueioPorOficio,
               teDesbloqueioPorOficio, teNenhum);

  TParamMunic = (pmAliquota, pmHistoricoAliquota, pmConvenio,
                 pmRegimesEspeciais, pmRetencoes, pmBeneficios);

  TAssinaturas = (taConfigProvedor, taAssinar, taNaoAssinar);

function StatusRPSToStr(const t: TStatusRPS): string;
function StrToStatusRPS(out ok: boolean; const s: string): TStatusRPS;

function StatusNFSeToStr(const t: TStatusNFSe): string;
function StrToStatusNFSe(out ok: boolean; const s: string): TStatusNFSe;

function NaturezaOperacaoToStr(const t: TnfseNaturezaOperacao): string;
function StrToNaturezaOperacao(out ok: boolean; const s: string): TnfseNaturezaOperacao;

function IndicacaoCpfCnpjToStr(const t: TIndicacaoCpfCnpj): string;
function StrToIndicacaoCpfCnpj(out ok: boolean; const s: string): TIndicacaoCpfCnpj;

function StrToProvedor(const s: string): TnfseProvedor;

function CodIBGEToCodTOM(const ACodigo: Integer): string;
function CodTOMToCodIBGE(const ACodigo: string): string;

function TipoEmissaoToStr(const t: TTipoEmissao): string;
function StrToTipoEmissao(out ok: boolean; const s: string): TTipoEmissao;
function TipoEmissaoDescricao(const t: TTipoEmissao): string;

function EmpreitadaGlobalToStr(const t: TEmpreitadaGlobal): string;
function StrToEmpreitadaGlobal(out ok: boolean; const s: string): TEmpreitadaGlobal;

function ObterDescricaoServico(const cCodigo: string): string;

function ChaveAcesso(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ: string;
                     ASerie:Integer; ANumero, ACodigo: Integer;
                     AModelo: Integer=56): string;

function VersaoXML(const AXML: string): string;

function GerarNomeNFSe(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ: string;
                               ANumero: Int64; AModelo: Integer = 56): string;

function StrToVersaoNFSe(out ok: Boolean; const s: string): TVersaoNFSe;
function VersaoNFSeToStr(const t: TVersaoNFSe): string;

function TipoFreteToStr(const t: TnfseFrete): string;
function StrToTipoFrete(out ok: boolean; const s: string): TnfseFrete;

function CanhotoToStr(const t: TnfseCanhoto): string;
function StrToCanhoto(out ok: boolean; const s: string): TnfseCanhoto;

function RegRecToStr(const t: TRegRec): string; //Governa
function StrToRegRec(out ok: boolean; const s: string): TRegRec; //Governa

function FrmRecToStr(const t: TFrmRec): string; //Governa
function StrToFrmRec(out ok: boolean; const s: string): TFrmRec; //Governa

function OperacaoToStr(const t: TOperacao): string;
function StrToOperacao(out ok: boolean; const s: string): TOperacao;
function OperacaoDescricao(const t: TOperacao): string;

function UnidadeToStr(const t: TUnidade): string;
function StrToUnidade(out ok: boolean; const s: string): TUnidade;

function SepararDados(const AString: string; const Chave: string;
  const MantemChave : Boolean = False;
  const PermitePrefixo: Boolean = True): string;

function tpConsultaToStr(const t: TtpConsulta): string;
function StrTotpConsulta(out ok: boolean; const s: string): TtpConsulta;

function tpPeriodoToStr(const t: TtpPeriodo): string;
function StrTotpPeriodo(out ok: boolean; const s: string): TtpPeriodo;

function tpRetornoToStr(const t: TtpRetorno): string;
function StrTotpRetorno(out ok: boolean; const s: string): TtpRetorno;

function MetodoToStr(const t: TMetodo): string;

function ModoEnvioToStr(const t: TmodoEnvio): string;

function TipoLancamentoToStr(const t: TTipoLancamento): string;
function StrToTipoLancamento(out ok: boolean; const s: string): TTipoLancamento;

function tpDocumentoToStr(const t: TtpDocumento): string;
function StrTotpDocumento(out ok: boolean; const s: string): TtpDocumento;

function tpEmitToStr(const t: TtpEmit): string;
function StrTotpEmit(out ok: Boolean; const s: string): TtpEmit;

function OptanteSNToStr(const t: TOptanteSN): string;
function StrToOptanteSN(out ok: Boolean; const s: string): TOptanteSN;

function RegimeApuracaoSNToStr(const t: TRegimeApuracaoSN): string;
function StrToRegimeApuracaoSN(out ok: Boolean; const s: string): TRegimeApuracaoSN;

function cMotivoToStr(const t: TcMotivo): string;
function StrTocMotivo(out ok: Boolean; const s: string): TcMotivo;

function mdPrestacaoToStr(const t: TmdPrestacao): string;
function StrTomdPrestacao(out ok: Boolean; const s: string): TmdPrestacao;

function vincPrestToStr(const t: TvincPrest): string;
function StrTovincPrest(out ok: Boolean; const s: string): TvincPrest;

function mecAFComexPToStr(const t: TmecAFComexP): string;
function StrTomecAFComexP(out ok: Boolean; const s: string): TmecAFComexP;

function mecAFComexTToStr(const t: TmecAFComexT): string;
function StrTomecAFComexT(out ok: Boolean; const s: string): TmecAFComexT;

function MovTempBensToStr(const t: TMovTempBens): string;
function StrToMovTempBens(out ok: Boolean; const s: string): TMovTempBens;

function categToStr(const t: Tcateg): string;
function StrTocateg(out ok: Boolean; const s: string): Tcateg;

function objetoToStr(const t: Tobjeto): string;
function StrToobjeto(out ok: Boolean; const s: string): Tobjeto;

function categVeicToStr(const t: TcategVeic): string;
function StrTocategVeic(out ok: Boolean; const s: string): TcategVeic;

function rodagemToStr(const t: Trodagem): string;
function StrTorodagem(out ok: Boolean; const s: string): Trodagem;

function tpDedRedToStr(const t: TtpDedRed): string;
function StrTotpDedRed(out ok: Boolean; const s: string): TtpDedRed;

function tribISSQNToStr(const t: TtribISSQN): string;
function StrTotribISSQN(out ok: Boolean; const s: string): TtribISSQN;

function tpImunidadeToStr(const t: TtpImunidade): string;
function StrTotpImunidade(out ok: Boolean; const s: string): TtpImunidade;

function tpRetISSQNToStr(const t: TtpRetISSQN): string;
function StrTotpRetISSQN(out ok: Boolean; const s: string): TtpRetISSQN;

function tpBMToStr(const t: TtpBM): string;
function StrTotpBM(out ok: Boolean; const s: string): TtpBM;

function tpSuspToStr(const t: TtpSusp): string;
function StrTotpSusp(out ok: Boolean; const s: string): TtpSusp;

function CSTToStr(const t: TCST): string;
function StrToCST(out ok: Boolean; const s: string): TCST;

function tpRetPisCofinsToStr(const t: TtpRetPisCofins): string;
function StrTotpRetPisCofins(out ok: Boolean; const s: string): TtpRetPisCofins;

function indTotTribToStr(const t: TindTotTrib): string;
function StrToindTotTrib(out ok: Boolean; const s: string): TindTotTrib;

function ambGerToStr(const t: TambGer): string;
function StrToambGer(out ok: Boolean; const s: string): TambGer;

function tpEmisToStr(const t: TtpEmis): string;
function StrTotpEmis(out ok: Boolean; const s: string): TtpEmis;

function procEmiToStr(const t: TprocEmi): string;
function StrToprocEmi(out ok: Boolean; const s: string): TprocEmi;

function tpEventoToStr(const t: TtpEvento): string;
function StrTotpEvento(out ok: Boolean; const s: string): TtpEvento;
function tpEventoToDesc(const t: TtpEvento): string;

function ParamMunicToStr(const t: TParamMunic): string;
function StrToParamMunic(out ok: Boolean; const s: string): TParamMunic;

function CodIBGEPaisToSiglaISO2(t: Integer): string;
function SiglaISO2ToCodIBGEPais(const t: string): Integer;

const

  SiglaISO2Pais: array[0..247] of string = ('AF', 'AL', 'CW', 'DE', 'BF', 'AD',
        'AO', 'AI', 'AQ', 'AG', 'SA', 'DZ', 'AR', 'AM', 'AW', 'AU', 'AT', 'AZ',
        'BS', 'BH', 'BD', 'BB', 'BY', 'BE', 'BZ', 'BM', 'MM', 'BO', 'BQ', 'BA',
        'BW', 'BV', 'BR', 'BN', 'BG', 'BI', 'BT', 'CV', 'KY', 'KH', 'CM', 'CA',
        'KZ', 'QA', 'CL', 'CN', 'TW', 'CY', 'CC', 'CO', 'KM', 'CG', 'CK', 'KP',
        'KR', 'CI', 'HR', 'CR', 'KW', 'CU', 'BJ', 'DK', 'DM', 'EC', 'EG', 'ER',
        'AE', 'ES', 'SI', 'SK', 'US', 'EE', 'ET', 'FK', 'FO', 'PH', 'FI', 'FR',
        'GA', 'GM', 'GH', 'GE', 'GS', 'GI', 'GD', 'GR', 'GL', 'GP', 'GU', 'GT',
        'GG', 'GF', 'GN', 'GQ', 'GW', 'GY', 'HT', 'HM', 'HN', 'HK', 'HU', 'YE',
        'IM', 'IN', 'ID', 'IQ', 'IR', 'IE', 'IS', 'IL', 'IT', 'JM', 'JE', 'JP',
        'JO', 'KI', 'LA', 'LS', 'LV', 'LB', 'LR', 'LY', 'LI', 'LT', 'LU', 'MO',
        'MK', 'MG', 'MY', 'MW', 'MV', 'ML', 'MT', 'MP', 'MA', 'MH', 'MQ', 'MU',
        'MR', 'YT', 'MX', 'MD', 'MC', 'MN', 'ME', 'FM', 'MS', 'MZ', 'NA', 'NR',
        'CX', 'NP', 'NI', 'NE', 'NG', 'NU', 'NF', 'NO', 'NC', 'PG', 'NZ', 'VU',
        'OM', 'UM', 'NL', 'PW', 'PK', 'PS', 'PA', 'PY', 'PE', 'PN', 'PF', 'PL',
        'PT', 'PR', 'KE', 'KG', 'GB', 'CF', 'DO', 'RE', 'ZW', 'RO', 'RW', 'RU',
        'SB', 'EH', 'SV', 'WS', 'AS', 'BL', 'KN', 'SX', 'SM', 'MF', 'PM', 'VC',
        'SH', 'LC', 'ST', 'SN', 'SC', 'SL', 'RS', 'SG', 'SY', 'SO', 'LK', 'SZ',
        'SJ', 'ZA', 'SD', 'SS', 'SE', 'CH', 'SR', 'TJ', 'TH', 'TZ', 'TF', 'IO',
        'DJ', 'TD', 'CZ', 'TL', 'TG', 'TK', 'TO', 'TT', 'TN', 'TC', 'TM', 'TR',
        'TV', 'UA', 'UG', 'UY', 'UZ', 'VA', 'VE', 'VN', 'VG', 'VI', 'WF', 'FJ',
        'CD', 'ZM');

  CodigoIBGEPais: array[0..247] of Integer = (0132, 0175, 0200, 0230, 0310,
    0370, 0400, 0418, 0420, 0434, 0531, 0590, 0639, 0647, 0655, 0698, 0728,
    0736, 0779, 0809, 0817, 0833, 0850, 0876, 0884, 0906, 0930, 0973, 0990,
    0981, 1015, 1023, 1058, 1082, 1112, 1155, 1198, 1279, 1376, 1414, 1457,
    1490, 1538, 1546, 1589, 1600, 1619, 1635, 1651, 1694, 1732, 1775, 1830,
    1872, 1902, 1937, 1953, 1961, 1988, 1996, 2291, 2321, 2356, 2399, 2402,
    2437, 2445, 2453, 2461, 2470, 2496, 2518, 2534, 2550, 2593, 2674, 2712,
    2755, 2810, 2852, 2895, 2917, 2925, 2933, 2976, 3018, 3050, 3093, 3131,
    3174, 3212, 3255, 3298, 3310, 3344, 3379, 3417, 3433, 3450, 3514, 3557,
    3573, 3595, 3611, 3654, 3697, 3727, 3751, 3794, 3832, 3867, 3913, 3930,
    3999, 4030, 4111, 4200, 4260, 4278, 4316, 4340, 4383, 4405, 4421, 4456,
    4472, 4499, 4502, 4553, 4588, 4618, 4642, 4677, 4723, 4740, 4766, 4774,
    4855, 4880, 4898, 4936, 4944, 4952, 4979, 4985, 4995, 5010, 5053, 5070,
    5088, 5118, 5177, 5215, 5258, 5282, 5312, 5355, 5380, 5428, 5452, 5487,
    5517, 5568, 5665, 5738, 5754, 5762, 5780, 5800, 5860, 5894, 5932, 5991,
    6033, 6076, 6114, 6238, 6254, 6289, 6408, 6475, 6602, 6653, 6700, 6750,
    6769, 6777, 6858, 6874, 6904, 6912, 6939, 6955, 6980, 6971, 6998, 7005,
    7056, 7102, 7153, 7200, 7285, 7315, 7358, 7370, 7412, 7447, 7480, 7501,
    7544, 7552, 7560, 7595, 7600, 7641, 7676, 7706, 7722, 7765, 7803, 7811,
    7820, 7838, 7889, 7919, 7951, 8001, 8052, 8109, 8150, 8206, 8230, 8249,
    8273, 8281, 8311, 8338, 8451, 8478, 8486, 8508, 8583, 8630, 8664, 8753,
    8702, 8885, 8907);

implementation

uses
  ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrXmlBase;


function StatusRPSToStr(const t: TStatusRPS): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [srNormal, srCancelado]);
end;

function StrToStatusRPS(out ok: boolean; const s: string): TStatusRPS;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [srNormal, srCancelado]);
end;

function StatusNFSeToStr(const t: TStatusNFSe): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [snNormal, snCancelado, snSubstituido]);
end;

function StrToStatusNFSe(out ok: boolean; const s: string): TStatusNFSe;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [snNormal, snCancelado, snSubstituido]);
end;

function NaturezaOperacaoToStr(const t: TnfseNaturezaOperacao): string;
begin
  Result := EnumeradoToStr(t,
                           ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                            '10', '11', '12', '13', '14', '15', '17', '18',
                            '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
                            '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
                            '70', '71', '72', '78', '79',
                            '101', '102', '103', '104', '105', '106', '107', '108',
                            '109', '110', '111', '112', '113', '114', '115', '116',
                            '117', '118', '121', '200', '201', '300', '301', '400',
                            '501', '511', '512', '515', '521', '522', '539', '541',
                            '549', '551', '601', '611', '612', '613', '615', '616',
                            '621', '622', '701', '711', '712', '901', '902', '911',
                            '912', '921', '931', '951', '952', '971', '981', '991'
                           ],
                           [no0, no1, no2, no3, no4, no5, no6, no7, no8, no9,
                            no10, no11, no12, no13, no14, no15, no17, no18,
                            no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
                            no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
                            no70, no71, no72, no78, no79,
                            no101, no102, no103, no104, no105, no106, no107, no108,
                            no109, no110, no111, no112, no113, no114, no115, no116,
                            no117, no118, no121, no200, no201, no300, no301, no400,
                            no501, no511, no512, no515, no521, no522, no539, no541,
                            no549, no551, no601, no611, no612, no613, no615, no616,
                            no621, no622, no701, no711, no712, no901, no902, no911,
                            no912, no921, no931, no951, no952, no971, no981, no991]);

end;

function StrToNaturezaOperacao(out ok: boolean; const s: string): TnfseNaturezaOperacao;
begin
  Result := StrToEnumerado(ok, s,
                           ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                            '10', '11', '12', '13', '14', '15', '17', '18',
                            '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
                            '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
                            '70', '71', '72', '78', '79',
                            '101', '102', '103', '104', '105', '106', '107', '108',
                            '109', '110', '111', '112', '113', '114', '115', '116',
                            '117', '118', '121', '200', '201', '300', '301', '400',
                            '501', '511', '512', '515', '521', '522', '539', '541',
                            '549', '551', '601', '611', '612', '613', '615', '616',
                            '621', '622', '701', '711', '712', '901', '902', '911',
                            '912', '921', '931', '951', '952', '971', '981', '991'
                           ],
                           [no0, no1, no2, no3, no4, no5, no6, no7, no8, no9,
                            no10, no11, no12, no13, no14, no15, no17, no18,
                            no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
                            no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
                            no70, no71, no72, no78, no79,
                            no101, no102, no103, no104, no105, no106, no107, no108,
                            no109, no110, no111, no112, no113, no114, no115, no116,
                            no117, no118, no121, no200, no201, no300, no301, no400,
                            no501, no511, no512, no515, no521, no522, no539, no541,
                            no549, no551, no601, no611, no612, no613, no615, no616,
                            no621, no622, no701, no711, no712, no901, no902, no911,
                            no912, no921, no931, no951, no952, no971, no981, no991]);
end;

function IndicacaoCpfCnpjToStr(const t: TIndicacaoCpfCnpj): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [iccCPF, iccCNPJ, iccNaoInformado]);
end;

function StrToIndicacaoCpfCnpj(out ok: boolean; const s: string): TIndicacaoCpfCnpj;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [iccCPF, iccCNPJ, iccNaoInformado]);
end;

function StrToProvedor(const s: string): TnfseProvedor;
var
  ProvedorStr: string;
  CodProvedor: Integer;
begin
  ProvedorStr := 'pro' + s;

  CodProvedor := GetEnumValue(TypeInfo(TnfseProvedor), ProvedorStr);

  if CodProvedor = -1 then
    Result := proNenhum
  else
    Result := TnfseProvedor(CodProvedor);
end;

function CodIBGEToCodTOM(const ACodigo: Integer): string;
var
 CodTOM: string;

 procedure P00;
 begin
   case ACodigo of
        25300: CodTOM := ''; // Goiânia/GO
        33800: CodTOM := ''; // Aparecida de Goiânia/GO
       530020: CodTOM := ''; // Brazlandia/DF
        28800: CodTOM := ''; // Trindade/GO
   end;
 end;

 procedure P11;
 begin
   case ACodigo of
      1100015: CodTOM := '0033'; // Alta Floresta D Oeste/RO
      1100023: CodTOM := '0007'; // Ariquemes/RO
      1100031: CodTOM := '0037'; // Cabixi/RO
      1100049: CodTOM := '0009'; // Cacoal/RO
      1100056: CodTOM := '0027'; // Cerejeiras/RO
      1100064: CodTOM := '0023'; // Colorado Do Oeste/RO
      1100072: CodTOM := '0981'; // Corumbiara/RO
      1100080: CodTOM := '0021'; // Costa Marques/RO
      1100098: CodTOM := '0025'; // Espigao D Oeste/RO
      1100106: CodTOM := '0001'; // Guajara-Mirim/RO
      1100114: CodTOM := '0015'; // Jaru/RO
      1100122: CodTOM := '0005'; // Ji-Parana/RO
      1100130: CodTOM := '0039'; // Machadinho Doeste/RO
      1100148: CodTOM := '0041'; // Nova Brasilandia D Oeste/RO
      1100155: CodTOM := '0017'; // Ouro Preto Do Oeste/RO
      1100189: CodTOM := '0011'; // Pimenta Bueno/RO
      1100205: CodTOM := '0003'; // Porto Velho/RO
      1100254: CodTOM := '0019'; // Presidente Medici/RO
      1100262: CodTOM := '0687'; // Rio Crespo/RO
      1100288: CodTOM := '0029'; // Rolim De Moura/RO
      1100296: CodTOM := '0043'; // Santa Luzia D Oeste/RO
      1100304: CodTOM := '0013'; // Vilhena/RO
      1100320: CodTOM := '0045'; // Sao Miguel Do Guapore/RO
      1100338: CodTOM := '0047'; // Nova Mamore/RO
      1100346: CodTOM := '0035'; // Alvorada D Oeste/RO
      1100379: CodTOM := '0002'; // Alto Alegre Dos Parecis/RO
      1100403: CodTOM := '0675'; // Alto Paraiso/RO
      1100452: CodTOM := '0004'; // Buritis/RO
      1100502: CodTOM := '0689'; // Novo Horizonte Do Oeste/RO
      1100601: CodTOM := '0677'; // Cacaulandia/RO
      1100700: CodTOM := '0679'; // Campo Novo De Rondonia/RO
      1100809: CodTOM := '0681'; // Candeias Do Jamari/RO
      1100908: CodTOM := '0691'; // Castanheiras/RO
      1100924: CodTOM := '0006'; // Chupinguaia/RO
      1100940: CodTOM := '0008'; // Cujubim/RO
      1101005: CodTOM := '0693'; // Governador Jorge Teixeira/RO
      1101104: CodTOM := '0683'; // Itapua Do Oeste/RO
      1101203: CodTOM := '0695'; // Ministro Andreazza/RO
      1101302: CodTOM := '0697'; // Mirante Da Serra/RO
      1101401: CodTOM := '0685'; // Monte Negro/RO
      1101435: CodTOM := '0010'; // Nova Uniao/RO
      1101450: CodTOM := '0012'; // Parecis/RO
      1101468: CodTOM := '0014'; // Pimenteiras Do Oeste/RO
      1101476: CodTOM := '0016'; // Primavera De Rondonia/RO
      1101484: CodTOM := '0018'; // Sao Felipe D Oeste/RO
      1101492: CodTOM := '0020'; // Sao Francisco Do Guapore/RO
      1101500: CodTOM := '0699'; // Seringueiras/RO
      1101559: CodTOM := '0022'; // Teixeiropolis/RO
      1101609: CodTOM := '0975'; // Theobroma/RO
      1101708: CodTOM := '0977'; // Urupa/RO
      1101757: CodTOM := '0024'; // Vale Do Anari/RO
      1101807: CodTOM := '0979'; // Vale Do Paraiso/RO
   end;
 end;

 procedure P12;
 begin
   case ACodigo of
      1200013: CodTOM := '0643'; // Acrelandia/AC
      1200054: CodTOM := '0157'; // Assis Brasil/AC
      1200104: CodTOM := '0105'; // Brasileia/AC
      1200138: CodTOM := '0645'; // Bujari/AC
      1200179: CodTOM := '0647'; // Capixaba/AC
      1200203: CodTOM := '0107'; // Cruzeiro Do Sul/AC
      1200252: CodTOM := '0651'; // Epitaciolandia/AC
      1200302: CodTOM := '0113'; // Feijo/AC
      1200328: CodTOM := '0653'; // Jordao/AC
      1200336: CodTOM := '0109'; // Mancio Lima/AC
      1200344: CodTOM := '0155'; // Manoel Urbano/AC
      1200351: CodTOM := '0655'; // Marechal Thaumaturgo/AC
      1200385: CodTOM := '0151'; // Placido De Castro/AC
      1200393: CodTOM := '0657'; // Porto Walter/AC
      1200401: CodTOM := '0139'; // Rio Branco/AC
      1200427: CodTOM := '0659'; // Rodrigues Alves/AC
      1200435: CodTOM := '0661'; // Santa Rosa Do Purus/AC
      1200450: CodTOM := '0153'; // Senador Guiomard/AC
      1200500: CodTOM := '0145'; // Sena Madureira/AC
      1200609: CodTOM := '0147'; // Tarauaca/AC
      1200708: CodTOM := '0149'; // Xapuri/AC
      1200807: CodTOM := '0649'; // Porto Acre/AC
   end;
 end;

 procedure P13;
 begin
   case ACodigo of
      1300029: CodTOM := '0289'; // Alvaraes/AM
      1300060: CodTOM := '0291'; // Amatura/AM
      1300086: CodTOM := '0293'; // Anama/AM
      1300102: CodTOM := '0203'; // Anori/AM
      1300144: CodTOM := '0969'; // Apui/AM
      1300201: CodTOM := '0205'; // Atalaia Do Norte/AM
      1300300: CodTOM := '0207'; // Autazes/AM
      1300409: CodTOM := '0209'; // Barcelos/AM
      1300508: CodTOM := '0211'; // Barreirinha/AM
      1300607: CodTOM := '0213'; // Benjamin Constant/AM
      1300631: CodTOM := '0295'; // Beruri/AM
      1300680: CodTOM := '0297'; // Boa Vista Do Ramos/AM
      1300706: CodTOM := '0215'; // Boca Do Acre/AM
      1300805: CodTOM := '0217'; // Borba/AM
      1300839: CodTOM := '0299'; // Caapiranga/AM
      1300904: CodTOM := '0219'; // Canutama/AM
      1301001: CodTOM := '0221'; // Carauari/AM
      1301100: CodTOM := '0223'; // Careiro/AM
      1301159: CodTOM := '0965'; // Careiro Da Varzea/AM
      1301209: CodTOM := '0225'; // Coari/AM
      1301308: CodTOM := '0227'; // Codajas/AM
      1301407: CodTOM := '0229'; // Eirunepe/AM
      1301506: CodTOM := '0231'; // Envira/AM
      1301605: CodTOM := '0233'; // Fonte Boa/AM
      1301654: CodTOM := '0967'; // Guajara/AM
      1301704: CodTOM := '0235'; // Humaita/AM
      1301803: CodTOM := '0239'; // Ipixuna/AM
      1301852: CodTOM := '9835'; // Iranduba/AM
      1301902: CodTOM := '0241'; // Itacoatiara/AM
      1301951: CodTOM := '9837'; // Itamarati/AM
      1302009: CodTOM := '0243'; // Itapiranga/AM
      1302108: CodTOM := '0245'; // Japura/AM
      1302207: CodTOM := '0247'; // Jurua/AM
      1302306: CodTOM := '0249'; // Jutai/AM
      1302405: CodTOM := '0251'; // Labrea/AM
      1302504: CodTOM := '0253'; // Manacapuru/AM
      1302553: CodTOM := '9839'; // Manaquiri/AM
      1302603: CodTOM := '0255'; // Manaus/AM
      1302702: CodTOM := '0257'; // Manicore/AM
      1302801: CodTOM := '0259'; // Maraa/AM
      1302900: CodTOM := '0261'; // Maues/AM
      1303007: CodTOM := '0263'; // Nhamunda/AM
      1303106: CodTOM := '0265'; // Nova Olinda Do Norte/AM
      1303205: CodTOM := '0201'; // Novo Airao/AM
      1303304: CodTOM := '0267'; // Novo Aripuana/AM
      1303403: CodTOM := '0269'; // Parintins/AM
      1303502: CodTOM := '0271'; // Pauini/AM
      1303536: CodTOM := '9841'; // Presidente Figueiredo/AM
      1303569: CodTOM := '9843'; // Rio Preto Da Eva/AM
      1303601: CodTOM := '0237'; // Santa Isabel Do Rio Negro/AM
      1303700: CodTOM := '0273'; // Santo Antonio Do Ica/AM
      1303809: CodTOM := '0283'; // Sao Gabriel Da Cachoeira/AM
      1303908: CodTOM := '0275'; // Sao Paulo De Olivenca/AM
      1303957: CodTOM := '9845'; // Sao Sebastiao Do Uatuma/AM
      1304005: CodTOM := '0277'; // Silves/AM
      1304062: CodTOM := '9847'; // Tabatinga/AM
      1304104: CodTOM := '0279'; // Tapaua/AM
      1304203: CodTOM := '0281'; // Tefe/AM
      1304237: CodTOM := '9851'; // Tonantins/AM
      1304260: CodTOM := '9849'; // Uarini/AM
      1304302: CodTOM := '0285'; // Urucara/AM
      1304401: CodTOM := '0287'; // Urucurituba/AM
   end;
 end;

 procedure P14;
 begin
   case ACodigo of
      1400027: CodTOM := '0026'; // Amajari/RR
      1400050: CodTOM := '0305'; // Alto Alegre/RR
      1400100: CodTOM := '0301'; // Boa Vista/RR
      1400159: CodTOM := '0307'; // Bonfim/RR
      1400175: CodTOM := '0028'; // Canta/RR
      1400209: CodTOM := '0303'; // Caracarai/RR
      1400233: CodTOM := '0030'; // Caroebe/RR
      1400282: CodTOM := '0032'; // Iracema/RR
      1400308: CodTOM := '0309'; // Mucajai/RR
      1400407: CodTOM := '0311'; // Normandia/RR
      1400456: CodTOM := '0034'; // Pacaraima/RR
      1400472: CodTOM := '0036'; // Rorainopolis/RR
      1400506: CodTOM := '0313'; // Sao Joao Da Baliza/RR
      1400605: CodTOM := '0315'; // Sao Luiz/RR
      1400704: CodTOM := '0038'; // Uiramuta/RR
   end;
 end;

 procedure P15;
 begin
   case ACodigo of
      1500107: CodTOM := '0401'; // Abaetetuba/PA
      1500131: CodTOM := '0375'; // Abel Figueiredo/PA
      1500206: CodTOM := '0403'; // Acara/PA
      1500305: CodTOM := '0405'; // Afua/PA
      1500347: CodTOM := '0383'; // Agua Azul Do Norte/PA
      1500404: CodTOM := '0407'; // Alenquer/PA
      1500503: CodTOM := '0409'; // Almeirim/PA
      1500602: CodTOM := '0411'; // Altamira/PA
      1500701: CodTOM := '0413'; // Anajas/PA
      1500800: CodTOM := '0415'; // Ananindeua/PA
      1500859: CodTOM := '0040'; // Anapu/PA
      1500909: CodTOM := '0417'; // Augusto Correa/PA
      1500958: CodTOM := '0389'; // Aurora Do Para/PA
      1501006: CodTOM := '0419'; // Aveiro/PA
      1501105: CodTOM := '0421'; // Bagre/PA
      1501204: CodTOM := '0423'; // Baiao/PA
      1501253: CodTOM := '0042'; // Bannach/PA
      1501303: CodTOM := '0425'; // Barcarena/PA
      1501402: CodTOM := '0427'; // Belem/PA
      1501451: CodTOM := '0044'; // Belterra/PA
      1501501: CodTOM := '0429'; // Benevides/PA
      1501576: CodTOM := '0575'; // Bom Jesus Do Tocantins/PA
      1501600: CodTOM := '0431'; // Bonito/PA
      1501709: CodTOM := '0433'; // Braganca/PA
      1501725: CodTOM := '0639'; // Brasil Novo/PA
      1501758: CodTOM := '0577'; // Brejo Grande Do Araguaia/PA
      1501782: CodTOM := '0625'; // Breu Branco/PA
      1501808: CodTOM := '0435'; // Breves/PA
      1501907: CodTOM := '0437'; // Bujaru/PA
      1501956: CodTOM := '0046'; // Cachoeira Do Piria/PA
      1502004: CodTOM := '0439'; // Cachoeira Do Arari/PA
      1502103: CodTOM := '0441'; // Cameta/PA
      1502152: CodTOM := '0048'; // Canaa Dos Carajas/PA
      1502202: CodTOM := '0443'; // Capanema/PA
      1502301: CodTOM := '0445'; // Capitao Poco/PA
      1502400: CodTOM := '0447'; // Castanhal/PA
      1502509: CodTOM := '0449'; // Chaves/PA
      1502608: CodTOM := '0451'; // Colares/PA
      1502707: CodTOM := '0453'; // Conceicao Do Araguaia/PA
      1502756: CodTOM := '0579'; // Concordia Do Para/PA
      1502764: CodTOM := '0385'; // Cumaru Do Norte/PA
      1502772: CodTOM := '0581'; // Curionopolis/PA
      1502806: CodTOM := '0455'; // Curralinho/PA
      1502855: CodTOM := '0050'; // Curua/PA
      1502905: CodTOM := '0457'; // Curuca/PA
      1502939: CodTOM := '0583'; // Dom Eliseu/PA
      1502954: CodTOM := '0377'; // Eldorado Dos Carajas/PA
      1503002: CodTOM := '0459'; // Faro/PA
      1503044: CodTOM := '0052'; // Floresta Do Araguaia/PA
      1503077: CodTOM := '0585'; // Garrafao Do Norte/PA
      1503093: CodTOM := '0627'; // Goianesia Do Para/PA
      1503101: CodTOM := '0461'; // Gurupa/PA
      1503200: CodTOM := '0463'; // Igarape-Acu/PA
      1503309: CodTOM := '0465'; // Igarape-Miri/PA
      1503408: CodTOM := '0467'; // Inhangapi/PA
      1503457: CodTOM := '0621'; // Ipixuna Do Para/PA
      1503507: CodTOM := '0469'; // Irituia/PA
      1503606: CodTOM := '0471'; // Itaituba/PA
      1503705: CodTOM := '0473'; // Itupiranga/PA
      1503754: CodTOM := '0631'; // Jacareacanga/PA
      1503804: CodTOM := '0475'; // Jacunda/PA
      1503903: CodTOM := '0477'; // Juruti/PA
      1504000: CodTOM := '0479'; // Limoeiro Do Ajuru/PA
      1504059: CodTOM := '0587'; // Mae Do Rio/PA
      1504109: CodTOM := '0481'; // Magalhaes Barata/PA
      1504208: CodTOM := '0483'; // Maraba/PA
      1504307: CodTOM := '0485'; // Maracana/PA
      1504406: CodTOM := '0487'; // Marapanim/PA
      1504422: CodTOM := '0054'; // Marituba/PA
      1504455: CodTOM := '0589'; // Medicilandia/PA
      1504505: CodTOM := '0489'; // Melgaco/PA
      1504604: CodTOM := '0491'; // Mocajuba/PA
      1504703: CodTOM := '0493'; // Moju/PA
      1504752: CodTOM := '1190'; // Mojuí dos Campos/PA
      1504802: CodTOM := '0495'; // Monte Alegre/PA
      1504901: CodTOM := '0497'; // Muana/PA
      1504950: CodTOM := '0391'; // Nova Esperanca Do Piria/PA
      1504976: CodTOM := '0056'; // Nova Ipixuna/PA
      1505007: CodTOM := '0499'; // Nova Timboteua/PA
      1505031: CodTOM := '0633'; // Novo Progresso/PA
      1505064: CodTOM := '0629'; // Novo Repartimento/PA
      1505106: CodTOM := '0501'; // Obidos/PA
      1505205: CodTOM := '0503'; // Oeiras Do Para/PA
      1505304: CodTOM := '0505'; // Oriximina/PA
      1505403: CodTOM := '0507'; // Ourem/PA
      1505437: CodTOM := '0591'; // Ourilandia Do Norte/PA
      1505486: CodTOM := '0593'; // Pacaja/PA
      1505494: CodTOM := '0379'; // Palestina Do Para/PA
      1505502: CodTOM := '0509'; // Paragominas/PA
      1505536: CodTOM := '0595'; // Parauapebas/PA
      1505551: CodTOM := '0387'; // Pau D Arco/PA
      1505601: CodTOM := '0511'; // Peixe-Boi/PA
      1505635: CodTOM := '0058'; // Picarra/PA
      1505650: CodTOM := '0060'; // Placas/PA
      1505700: CodTOM := '0513'; // Ponta De Pedras/PA
      1505809: CodTOM := '0515'; // Portel/PA
      1505908: CodTOM := '0517'; // Porto De Moz/PA
      1506005: CodTOM := '0519'; // Prainha/PA
      1506104: CodTOM := '0521'; // Primavera/PA
      1506112: CodTOM := '0062'; // Quatipuru/PA
      1506138: CodTOM := '0567'; // Redencao/PA
      1506161: CodTOM := '0569'; // Rio Maria/PA
      1506187: CodTOM := '0573'; // Rondon Do Para/PA
      1506195: CodTOM := '0597'; // Ruropolis/PA
      1506203: CodTOM := '0523'; // Salinopolis/PA
      1506302: CodTOM := '0525'; // Salvaterra/PA
      1506351: CodTOM := '0369'; // Santa Barbara Do Para/PA
      1506401: CodTOM := '0527'; // Santa Cruz Do Arari/PA
      1506500: CodTOM := '0529'; // Santa Isabel Do Para/PA
      1506559: CodTOM := '0371'; // Santa Luzia Do Para/PA
      1506583: CodTOM := '0599'; // Santa Maria Das Barreiras/PA
      1506609: CodTOM := '0531'; // Santa Maria Do Para/PA
      1506708: CodTOM := '0533'; // Santana Do Araguaia/PA
      1506807: CodTOM := '0535'; // Santarem/PA
      1506906: CodTOM := '0537'; // Santarem Novo/PA
      1507003: CodTOM := '0539'; // Santo Antonio Do Taua/PA
      1507102: CodTOM := '0541'; // Sao Caetano De Odivelas/PA
      1507151: CodTOM := '0381'; // Sao Domingos Do Araguaia/PA
      1507201: CodTOM := '0543'; // Sao Domingos Do Capim/PA
      1507300: CodTOM := '0545'; // Sao Felix Do Xingu/PA
      1507409: CodTOM := '0547'; // Sao Francisco Do Para/PA
      1507458: CodTOM := '0619'; // Sao Geraldo Do Araguaia/PA
      1507466: CodTOM := '0064'; // Sao Joao Da Ponta/PA
      1507474: CodTOM := '0393'; // Sao Joao De Pirabas/PA
      1507508: CodTOM := '0549'; // Sao Joao Do Araguaia/PA
      1507607: CodTOM := '0551'; // Sao Miguel Do Guama/PA
      1507706: CodTOM := '0553'; // Sao Sebastiao Da Boa Vista/PA
      1507755: CodTOM := '0066'; // Sapucaia/PA
      1507805: CodTOM := '0555'; // Senador Jose Porfirio/PA
      1507904: CodTOM := '0557'; // Soure/PA
      1507953: CodTOM := '0395'; // Tailandia/PA
      1507961: CodTOM := '0373'; // Terra Alta/PA
      1507979: CodTOM := '0637'; // Terra Santa/PA
      1508001: CodTOM := '0559'; // Tome-Acu/PA
      1508035: CodTOM := '0068'; // Tracuateua/PA
      1508050: CodTOM := '0635'; // Trairao/PA
      1508084: CodTOM := '0397'; // Tucuma/PA
      1508100: CodTOM := '0561'; // Tucurui/PA
      1508126: CodTOM := '0623'; // Ulianopolis/PA
      1508159: CodTOM := '0399'; // Uruara/PA
      1508209: CodTOM := '0563'; // Vigia/PA
      1508308: CodTOM := '0565'; // Viseu/PA
      1508357: CodTOM := '0641'; // Vitoria Do Xingu/PA
      1508407: CodTOM := '0571'; // Xinguara/PA
   end;
 end;

 procedure P16;
 begin
   case ACodigo of
      1600055: CodTOM := '0665'; // Serra Do Navio/AP
      1600105: CodTOM := '0601'; // Amapa/AP
      1600154: CodTOM := '0663'; // Pedra Branca Do Amapari/AP
      1600204: CodTOM := '0603'; // Calcoene/AP
      1600212: CodTOM := '0667'; // Cutias/AP
      1600238: CodTOM := '0611'; // Ferreira Gomes/AP
      1600253: CodTOM := '0669'; // Itaubal/AP
      1600279: CodTOM := '0613'; // Laranjal Do Jari/AP
      1600303: CodTOM := '0605'; // Macapa/AP
      1600402: CodTOM := '0607'; // Mazagao/AP
      1600501: CodTOM := '0609'; // Oiapoque/AP
      1600535: CodTOM := '0671'; // Porto Grande/AP
      1600550: CodTOM := '0673'; // Pracuuba/AP
      1600600: CodTOM := '0615'; // Santana/AP
      1600709: CodTOM := '0617'; // Tartarugalzinho/AP
      1600808: CodTOM := '0070'; // Vitoria Do Jari/AP
   end;
 end;

 procedure P17;
 begin
   case ACodigo of
      1700251: CodTOM := '0337'; // Abreulandia/TO
      1700301: CodTOM := '0072'; // Aguiarnopolis/TO
      1700350: CodTOM := '9441'; // Alianca Do Tocantins/TO
      1700400: CodTOM := '9207'; // Almas/TO
      1700707: CodTOM := '9213'; // Alvorada/TO
      1701002: CodTOM := '9219'; // Ananas/TO
      1701051: CodTOM := '0165'; // Angico/TO
      1701101: CodTOM := '9713'; // Aparecida Do Rio Negro/TO
      1701309: CodTOM := '0167'; // Aragominas/TO
      1701903: CodTOM := '9237'; // Araguacema/TO
      1702000: CodTOM := '9239'; // Araguacu/TO
      1702109: CodTOM := '9241'; // Araguaina/TO
      1702158: CodTOM := '0169'; // Araguana/TO
      1702208: CodTOM := '9243'; // Araguatins/TO
      1702307: CodTOM := '9245'; // Arapoema/TO
      1702406: CodTOM := '9247'; // Arraias/TO
      1702554: CodTOM := '9685'; // Augustinopolis/TO
      1702703: CodTOM := '9253'; // Aurora Do Tocantins/TO
      1702901: CodTOM := '9257'; // Axixa Do Tocantins/TO
      1703008: CodTOM := '9259'; // Babaculandia/TO
      1703057: CodTOM := '0074'; // Bandeirantes Do Tocantins/TO
      1703073: CodTOM := '0076'; // Barra Do Ouro/TO
      1703107: CodTOM := '9693'; // Barrolandia/TO
      1703206: CodTOM := '9695'; // Bernardo Sayao/TO
      1703305: CodTOM := '0341'; // Bom Jesus Do Tocantins/TO
      1703602: CodTOM := '0339'; // Brasilandia Do Tocantins/TO
      1703701: CodTOM := '9273'; // Brejinho De Nazare/TO
      1703800: CodTOM := '9715'; // Buriti Do Tocantins/TO
      1703826: CodTOM := '0171'; // Cachoeirinha/TO
      1703842: CodTOM := '0173'; // Campos Lindos/TO
      1703867: CodTOM := '0327'; // Cariri Do Tocantins/TO
      1703883: CodTOM := '0175'; // Carmolandia/TO
      1703891: CodTOM := '0177'; // Carrasco Bonito/TO
      1703909: CodTOM := '9717'; // Caseara/TO
      1704105: CodTOM := '0343'; // Centenario/TO
      1704600: CodTOM := '0078'; // Chapada De Areia/TO
      1705102: CodTOM := '0080'; // Chapada Da Natividade/TO
      1705508: CodTOM := '9311'; // Colinas Do Tocantins/TO
      1705557: CodTOM := '9697'; // Combinado/TO
      1705607: CodTOM := '9313'; // Conceicao Do Tocantins/TO
      1706001: CodTOM := '9321'; // Couto Magalhaes/TO
      1706100: CodTOM := '9323'; // Cristalandia/TO
      1706258: CodTOM := '0082'; // Crixas Do Tocantins/TO
      1706506: CodTOM := '0179'; // Darcinopolis/TO
      1707009: CodTOM := '9341'; // Dianopolis/TO
      1707108: CodTOM := '9719'; // Divinopolis Do Tocantins/TO
      1707207: CodTOM := '9345'; // Dois Irmaos Do Tocantins/TO
      1707306: CodTOM := '9347'; // Duere/TO
      1707405: CodTOM := '0181'; // Esperantina/TO
      1707553: CodTOM := '9683'; // Fatima/TO
      1707652: CodTOM := '9667'; // Figueiropolis/TO
      1707702: CodTOM := '9355'; // Filadelfia/TO
      1708205: CodTOM := '9365'; // Formoso Do Araguaia/TO
      1708254: CodTOM := '0345'; // Fortaleza Do Tabocao/TO
      1708304: CodTOM := '9699'; // Goianorte/TO
      1709005: CodTOM := '9533'; // Goiatins/TO
      1709302: CodTOM := '9627'; // Guarai/TO
      1709500: CodTOM := '9385'; // Gurupi/TO
      1709807: CodTOM := '0084'; // Ipueiras/TO
      1710508: CodTOM := '9405'; // Itacaja/TO
      1710706: CodTOM := '9409'; // Itaguatins/TO
      1710904: CodTOM := '0347'; // Itapiratins/TO
      1711100: CodTOM := '9417'; // Itapora Do Tocantins/TO
      1711506: CodTOM := '0329'; // Jau Do Tocantins/TO
      1711803: CodTOM := '0349'; // Juarina/TO
      1711902: CodTOM := '0367'; // Lagoa Da Confusao/TO
      1711951: CodTOM := '0353'; // Lagoa Do Tocantins/TO
      1712009: CodTOM := '0351'; // Lajeado/TO
      1712157: CodTOM := '0086'; // Lavandeira/TO
      1712405: CodTOM := '9569'; // Lizarda/TO
      1712454: CodTOM := '0088'; // Luzinopolis/TO
      1712504: CodTOM := '9711'; // Marianopolis Do Tocantins/TO
      1712702: CodTOM := '0317'; // Mateiros/TO
      1712801: CodTOM := '0183'; // Maurilandia Do Tocantins/TO
      1713205: CodTOM := '9461'; // Miracema Do Tocantins/TO
      1713304: CodTOM := '9463'; // Miranorte/TO
      1713601: CodTOM := '9469'; // Monte Do Carmo/TO
      1713700: CodTOM := '0090'; // Monte Santo Do Tocantins/TO
      1713809: CodTOM := '0185'; // Palmeiras Do Tocantins/TO
      1713957: CodTOM := '0187'; // Muricilandia/TO
      1714203: CodTOM := '9481'; // Natividade/TO
      1714302: CodTOM := '9483'; // Nazare/TO
      1714880: CodTOM := '9663'; // Nova Olinda/TO
      1715002: CodTOM := '9721'; // Nova Rosalandia/TO
      1715101: CodTOM := '9499'; // Novo Acordo/TO
      1715150: CodTOM := '9703'; // Novo Alegre/TO
      1715259: CodTOM := '0321'; // Novo Jardim/TO
      1715507: CodTOM := '0092'; // Oliveira De Fatima/TO
      1715705: CodTOM := '0189'; // Palmeirante/TO
      1715754: CodTOM := '9649'; // Palmeiropolis/TO
      1716109: CodTOM := '9519'; // Paraiso Do Tocantins/TO
      1716208: CodTOM := '9521'; // Parana/TO
      1716307: CodTOM := '0191'; // Pau D Arco/TO
      1716505: CodTOM := '9525'; // Pedro Afonso/TO
      1716604: CodTOM := '9527'; // Peixe/TO
      1716653: CodTOM := '9705'; // Pequizeiro/TO
      1716703: CodTOM := '9529'; // Colmeia/TO
      1717008: CodTOM := '9537'; // Pindorama Do Tocantins/TO
      1717206: CodTOM := '0355'; // Piraque/TO
      1717503: CodTOM := '9547'; // Pium/TO
      1717800: CodTOM := '9551'; // Ponte Alta Do Bom Jesus/TO
      1717909: CodTOM := '9553'; // Ponte Alta Do Tocantins/TO
      1718006: CodTOM := '9723'; // Porto Alegre Do Tocantins/TO
      1718204: CodTOM := '9559'; // Porto Nacional/TO
      1718303: CodTOM := '9725'; // Praia Norte/TO
      1718402: CodTOM := '9629'; // Presidente Kennedy/TO
      1718451: CodTOM := '0094'; // Pugmil/TO
      1718501: CodTOM := '0357'; // Recursolandia/TO
      1718550: CodTOM := '0193'; // Riachinho/TO
      1718659: CodTOM := '0323'; // Rio Da Conceicao/TO
      1718709: CodTOM := '0359'; // Rio Dos Bois/TO
      1718758: CodTOM := '9679'; // Rio Sono/TO
      1718808: CodTOM := '9727'; // Sampaio/TO
      1718840: CodTOM := '0331'; // Sandolandia/TO
      1718865: CodTOM := '0195'; // Santa Fe Do Araguaia/TO
      1718881: CodTOM := '0361'; // Santa Maria Do Tocantins/TO
      1718899: CodTOM := '0096'; // Santa Rita Do Tocantins/TO
      1718907: CodTOM := '9729'; // Santa Rosa Do Tocantins/TO
      1719004: CodTOM := '9731'; // Santa Tereza Do Tocantins/TO
      1720002: CodTOM := '0098'; // Santa Terezinha Do Tocantins/TO
      1720101: CodTOM := '0197'; // Sao Bento Do Tocantins/TO
      1720150: CodTOM := '0363'; // Sao Felix Do Tocantins/TO
      1720200: CodTOM := '0199'; // Sao Miguel Do Tocantins/TO
      1720259: CodTOM := '0333'; // Sao Salvador Do Tocantins/TO
      1720309: CodTOM := '9603'; // Sao Sebastiao Do Tocantins/TO
      1720499: CodTOM := '9691'; // Sao Valerio Da Natividade/TO
      1720655: CodTOM := '9659'; // Silvanopolis/TO
      1720804: CodTOM := '9613'; // Sitio Novo Do Tocantins/TO
      1720853: CodTOM := '0335'; // Sucupira/TO
      1720903: CodTOM := '9615'; // Taguatinga/TO
      1720937: CodTOM := '0325'; // Taipas Do Tocantins/TO
      1720978: CodTOM := '0100'; // Talisma/TO
      1721000: CodTOM := '9733'; // Palmas/TO
      1721109: CodTOM := '9619'; // Tocantinia/TO
      1721208: CodTOM := '9621'; // Tocantinopolis/TO
      1721257: CodTOM := '0102'; // Tupirama/TO
      1721307: CodTOM := '0365'; // Tupiratins/TO
      1722081: CodTOM := '9665'; // Wanderlandia/TO
      1722107: CodTOM := '9643'; // Xambioa/TO
   end;
 end;

 procedure P21;
 begin
   case ACodigo of
      2100055: CodTOM := '0961'; // Acailandia/MA
      2100105: CodTOM := '0701'; // Afonso Cunha/MA
      2100154: CodTOM := '0104'; // Agua Doce Do Maranhao/MA
      2100204: CodTOM := '0703'; // Alcantara/MA
      2100303: CodTOM := '0705'; // Aldeias Altas/MA
      2100402: CodTOM := '0707'; // Altamira Do Maranhao/MA
      2100436: CodTOM := '0106'; // Alto Alegre Do Maranhao/MA
      2100477: CodTOM := '0108'; // Alto Alegre Do Pindare/MA
      2100501: CodTOM := '0709'; // Alto Parnaiba/MA
      2100550: CodTOM := '0110'; // Amapa Do Maranhao/MA
      2100600: CodTOM := '0711'; // Amarante Do Maranhao/MA
      2100709: CodTOM := '0713'; // Anajatuba/MA
      2100808: CodTOM := '0715'; // Anapurus/MA
      2100832: CodTOM := '0112'; // Apicum-Acu/MA
      2100873: CodTOM := '0114'; // Araguana/MA
      2100907: CodTOM := '0717'; // Araioses/MA
      2100956: CodTOM := '1281'; // Arame/MA
      2101004: CodTOM := '0719'; // Arari/MA
      2101103: CodTOM := '0721'; // Axixa/MA
      2101202: CodTOM := '0723'; // Bacabal/MA
      2101251: CodTOM := '0116'; // Bacabeira/MA
      2101301: CodTOM := '0725'; // Bacuri/MA
      2101350: CodTOM := '0118'; // Bacurituba/MA
      2101400: CodTOM := '0727'; // Balsas/MA
      2101509: CodTOM := '0729'; // Barao De Grajau/MA
      2101608: CodTOM := '0731'; // Barra Do Corda/MA
      2101707: CodTOM := '0733'; // Barreirinhas/MA
      2101731: CodTOM := '0120'; // Belagua/MA
      2101772: CodTOM := '0122'; // Bela Vista Do Maranhao/MA
      2101806: CodTOM := '0735'; // Benedito Leite/MA
      2101905: CodTOM := '0737'; // Bequimao/MA
      2101939: CodTOM := '0124'; // Bernardo Do Mearim/MA
      2101970: CodTOM := '0126'; // Boa Vista Do Gurupi/MA
      2102002: CodTOM := '0955'; // Bom Jardim/MA
      2102036: CodTOM := '0128'; // Bom Jesus Das Selvas/MA
      2102077: CodTOM := '0130'; // Bom Lugar/MA
      2102101: CodTOM := '0739'; // Brejo/MA
      2102150: CodTOM := '0132'; // Brejo De Areia/MA
      2102200: CodTOM := '0741'; // Buriti/MA
      2102309: CodTOM := '0743'; // Buriti Bravo/MA
      2102325: CodTOM := '0134'; // Buriticupu/MA
      2102358: CodTOM := '0136'; // Buritirana/MA
      2102374: CodTOM := '0138'; // Cachoeira Grande/MA
      2102408: CodTOM := '0745'; // Cajapio/MA
      2102507: CodTOM := '0747'; // Cajari/MA
      2102556: CodTOM := '0140'; // Campestre Do Maranhao/MA
      2102606: CodTOM := '0749'; // Candido Mendes/MA
      2102705: CodTOM := '0751'; // Cantanhede/MA
      2102754: CodTOM := '0142'; // Capinzal Do Norte/MA
      2102804: CodTOM := '0753'; // Carolina/MA
      2102903: CodTOM := '0755'; // Carutapera/MA
      2103000: CodTOM := '0757'; // Caxias/MA
      2103109: CodTOM := '0759'; // Cedral/MA
      2103125: CodTOM := '0144'; // Central Do Maranhao/MA
      2103158: CodTOM := '0146'; // Centro Do Guilherme/MA
      2103174: CodTOM := '0148'; // Centro Novo Do Maranhao/MA
      2103208: CodTOM := '0761'; // Chapadinha/MA
      2103257: CodTOM := '0150'; // Cidelandia/MA
      2103307: CodTOM := '0763'; // Codo/MA
      2103406: CodTOM := '0765'; // Coelho Neto/MA
      2103505: CodTOM := '0767'; // Colinas/MA
      2103554: CodTOM := '0152'; // Conceicao Do Lago-Acu/MA
      2103604: CodTOM := '0769'; // Coroata/MA
      2103703: CodTOM := '0771'; // Cururupu/MA
      2103752: CodTOM := '0154'; // Davinopolis/MA
      2103802: CodTOM := '0773'; // Dom Pedro/MA
      2103901: CodTOM := '0775'; // Duque Bacelar/MA
      2104008: CodTOM := '0777'; // Esperantinopolis/MA
      2104057: CodTOM := '0963'; // Estreito/MA
      2104073: CodTOM := '0156'; // Feira Nova Do Maranhao/MA
      2104081: CodTOM := '0158'; // Fernando Falcao/MA
      2104099: CodTOM := '0160'; // Formosa Da Serra Negra/MA
      2104107: CodTOM := '0779'; // Fortaleza Dos Nogueiras/MA
      2104206: CodTOM := '0781'; // Fortuna/MA
      2104305: CodTOM := '0783'; // Godofredo Viana/MA
      2104404: CodTOM := '0785'; // Goncalves Dias/MA
      2104503: CodTOM := '0787'; // Governador Archer/MA
      2104552: CodTOM := '0162'; // Governador Edison Lobao/MA
      2104602: CodTOM := '0789'; // Governador Eugenio Barros/MA
      2104628: CodTOM := '0164'; // Governador Luiz Rocha/MA
      2104651: CodTOM := '0166'; // Governador Newton Bello/MA
      2104677: CodTOM := '0168'; // Governador Nunes Freire/MA
      2104701: CodTOM := '0791'; // Graca Aranha/MA
      2104800: CodTOM := '0793'; // Grajau/MA
      2104909: CodTOM := '0795'; // Guimaraes/MA
      2105005: CodTOM := '0797'; // Humberto De Campos/MA
      2105104: CodTOM := '0799'; // Icatu/MA
      2105153: CodTOM := '0170'; // Igarape Do Meio/MA
      2105203: CodTOM := '0801'; // Igarape Grande/MA
      2105302: CodTOM := '0803'; // Imperatriz/MA
      2105351: CodTOM := '0172'; // Itaipava Do Grajau/MA
      2105401: CodTOM := '0807'; // Itapecuru Mirim/MA
      2105427: CodTOM := '0174'; // Itinga Do Maranhao/MA
      2105450: CodTOM := '0176'; // Jatoba/MA
      2105476: CodTOM := '0178'; // Jenipapo Dos Vieiras/MA
      2105500: CodTOM := '0809'; // Joao Lisboa/MA
      2105609: CodTOM := '0811'; // Joselandia/MA
      2105658: CodTOM := '0180'; // Junco Do Maranhao/MA
      2105708: CodTOM := '0813'; // Lago Da Pedra/MA
      2105807: CodTOM := '0815'; // Lago Do Junco/MA
      2105906: CodTOM := '0817'; // Lago Verde/MA
      2105922: CodTOM := '0182'; // Lagoa Do Mato/MA
      2105948: CodTOM := '0184'; // Lago Dos Rodrigues/MA
      2105963: CodTOM := '0186'; // Lagoa Grande Do Maranhao/MA
      2105989: CodTOM := '0188'; // Lajeado Novo/MA
      2106003: CodTOM := '0819'; // Lima Campos/MA
      2106102: CodTOM := '0821'; // Loreto/MA
      2106201: CodTOM := '0823'; // Luis Domingues/MA
      2106300: CodTOM := '0825'; // Magalhaes De Almeida/MA
      2106326: CodTOM := '0190'; // Maracacume/MA
      2106359: CodTOM := '0192'; // Maraja Do Sena/MA
      2106375: CodTOM := '0194'; // Maranhaozinho/MA
      2106409: CodTOM := '0827'; // Mata Roma/MA
      2106508: CodTOM := '0829'; // Matinha/MA
      2106607: CodTOM := '0831'; // Matoes/MA
      2106631: CodTOM := '0196'; // Matoes Do Norte/MA
      2106672: CodTOM := '0198'; // Milagres Do Maranhao/MA
      2106706: CodTOM := '0833'; // Mirador/MA
      2106755: CodTOM := '1283'; // Miranda Do Norte/MA
      2106805: CodTOM := '0835'; // Mirinzal/MA
      2106904: CodTOM := '0837'; // Moncao/MA
      2107001: CodTOM := '0839'; // Montes Altos/MA
      2107100: CodTOM := '0841'; // Morros/MA
      2107209: CodTOM := '0843'; // Nina Rodrigues/MA
      2107258: CodTOM := '0200'; // Nova Colinas/MA
      2107308: CodTOM := '0845'; // Nova Iorque/MA
      2107357: CodTOM := '0202'; // Nova Olinda Do Maranhao/MA
      2107407: CodTOM := '0847'; // Olho D Agua Das Cunhas/MA
      2107456: CodTOM := '0204'; // Olinda Nova Do Maranhao/MA
      2107506: CodTOM := '0849'; // Paco Do Lumiar/MA
      2107605: CodTOM := '0851'; // Palmeirandia/MA
      2107704: CodTOM := '0853'; // Paraibano/MA
      2107803: CodTOM := '0855'; // Parnarama/MA
      2107902: CodTOM := '0857'; // Passagem Franca/MA
      2108009: CodTOM := '0859'; // Pastos Bons/MA
      2108058: CodTOM := '0206'; // Paulino Neves/MA
      2108108: CodTOM := '0959'; // Paulo Ramos/MA
      2108207: CodTOM := '0861'; // Pedreiras/MA
      2108256: CodTOM := '0208'; // Pedro Do Rosario/MA
      2108306: CodTOM := '0863'; // Penalva/MA
      2108405: CodTOM := '0865'; // Peri Mirim/MA
      2108454: CodTOM := '0210'; // Peritoro/MA
      2108504: CodTOM := '0867'; // Pindare-Mirim/MA
      2108603: CodTOM := '0869'; // Pinheiro/MA
      2108702: CodTOM := '0871'; // Pio Xii/MA
      2108801: CodTOM := '0873'; // Pirapemas/MA
      2108900: CodTOM := '0875'; // Pocao De Pedras/MA
      2109007: CodTOM := '0877'; // Porto Franco/MA
      2109056: CodTOM := '0212'; // Porto Rico Do Maranhao/MA
      2109106: CodTOM := '0879'; // Presidente Dutra/MA
      2109205: CodTOM := '0881'; // Presidente Juscelino/MA
      2109239: CodTOM := '0214'; // Presidente Medici/MA
      2109270: CodTOM := '0216'; // Presidente Sarney/MA
      2109304: CodTOM := '0883'; // Presidente Vargas/MA
      2109403: CodTOM := '0885'; // Primeira Cruz/MA
      2109452: CodTOM := '0218'; // Raposa/MA
      2109502: CodTOM := '0887'; // Riachao/MA
      2109551: CodTOM := '0220'; // Ribamar Fiquene/MA
      2109601: CodTOM := '0891'; // Rosario/MA
      2109700: CodTOM := '0893'; // Sambaiba/MA
      2109759: CodTOM := '0222'; // Santa Filomena Do Maranhao/MA
      2109809: CodTOM := '0895'; // Santa Helena/MA
      2109908: CodTOM := '0957'; // Santa Ines/MA
      2110005: CodTOM := '0897'; // Santa Luzia/MA
      2110039: CodTOM := '1285'; // Santa Luzia Do Parua/MA
      2110104: CodTOM := '0899'; // Santa Quiteria Do Maranhao/MA
      2110203: CodTOM := '0901'; // Santa Rita/MA
      2110237: CodTOM := '0224'; // Santana Do Maranhao/MA
      2110278: CodTOM := '0226'; // Santo Amaro Do Maranhao/MA
      2110302: CodTOM := '0903'; // Santo Antonio Dos Lopes/MA
      2110401: CodTOM := '0905'; // Sao Benedito Do Rio Preto/MA
      2110500: CodTOM := '0907'; // Sao Bento/MA
      2110609: CodTOM := '0909'; // Sao Bernardo/MA
      2110658: CodTOM := '0228'; // Sao Domingos Do Azeitao/MA
      2110708: CodTOM := '0911'; // Sao Domingos Do Maranhao/MA
      2110807: CodTOM := '0913'; // Sao Felix De Balsas/MA
      2110856: CodTOM := '0230'; // Sao Francisco Do Brejao/MA
      2110906: CodTOM := '0915'; // Sao Francisco Do Maranhao/MA
      2111003: CodTOM := '0917'; // Sao Joao Batista/MA
      2111029: CodTOM := '0232'; // Sao Joao Do Caru/MA
      2111052: CodTOM := '0234'; // Sao Joao Do Paraiso/MA
      2111078: CodTOM := '0236'; // Sao Joao Do Soter/MA
      2111102: CodTOM := '0919'; // Sao Joao Dos Patos/MA
      2111201: CodTOM := '0889'; // Sao Jose De Ribamar/MA
      2111250: CodTOM := '0238'; // Sao Jose Dos Basilios/MA
      2111300: CodTOM := '0921'; // Sao Luis/MA
      2111409: CodTOM := '0805'; // Sao Luis Gonzaga Do Maranhao/MA
      2111508: CodTOM := '0923'; // Sao Mateus Do Maranhao/MA
      2111532: CodTOM := '0240'; // Sao Pedro Da Agua Branca/MA
      2111573: CodTOM := '0242'; // Sao Pedro Dos Crentes/MA
      2111607: CodTOM := '0925'; // Sao Raimundo Das Mangabeiras/MA
      2111631: CodTOM := '0244'; // Sao Raimundo Do Doca Bezerra/MA
      2111672: CodTOM := '0246'; // Sao Roberto/MA
      2111706: CodTOM := '0927'; // Sao Vicente Ferrer/MA
      2111722: CodTOM := '0248'; // Satubinha/MA
      2111748: CodTOM := '0250'; // Senador Alexandre Costa/MA
      2111763: CodTOM := '0252'; // Senador La Rocque/MA
      2111789: CodTOM := '0254'; // Serrano Do Maranhao/MA
      2111805: CodTOM := '0929'; // Sitio Novo/MA
      2111904: CodTOM := '0931'; // Sucupira Do Norte/MA
      2111953: CodTOM := '0256'; // Sucupira Do Riachao/MA
      2112001: CodTOM := '0933'; // Tasso Fragoso/MA
      2112100: CodTOM := '0935'; // Timbiras/MA
      2112209: CodTOM := '0937'; // Timon/MA
      2112233: CodTOM := '0258'; // Trizidela Do Vale/MA
      2112274: CodTOM := '0260'; // Tufilandia/MA
      2112308: CodTOM := '0939'; // Tuntum/MA
      2112407: CodTOM := '0941'; // Turiacu/MA
      2112456: CodTOM := '0262'; // Turilandia/MA
      2112506: CodTOM := '0943'; // Tutoia/MA
      2112605: CodTOM := '0945'; // Urbano Santos/MA
      2112704: CodTOM := '0947'; // Vargem Grande/MA
      2112803: CodTOM := '0949'; // Viana/MA
      2112852: CodTOM := '0264'; // Vila Nova Dos Martirios/MA
      2112902: CodTOM := '0951'; // Vitoria Do Mearim/MA
      2113009: CodTOM := '0953'; // Vitorino Freire/MA
      2114007: CodTOM := '1287'; // Ze Doca/MA
   end;
 end;

 procedure P22;
 begin
   case ACodigo of
      2200053: CodTOM := '0266'; // Acaua/PI
      2200103: CodTOM := '1001'; // Agricolandia/PI
      2200202: CodTOM := '1003'; // Agua Branca/PI
      2200251: CodTOM := '9767'; // Alagoinha Do Piaui/PI
      2200277: CodTOM := '2269'; // Alegrete Do Piaui/PI
      2200301: CodTOM := '1005'; // Alto Longa/PI
      2200400: CodTOM := '1007'; // Altos/PI
      2200459: CodTOM := '0268'; // Alvorada Do Gurgueia/PI
      2200509: CodTOM := '1009'; // Amarante/PI
      2200608: CodTOM := '1011'; // Angical Do Piaui/PI
      2200707: CodTOM := '1013'; // Anisio De Abreu/PI
      2200806: CodTOM := '1015'; // Antonio Almeida/PI
      2200905: CodTOM := '1017'; // Aroazes/PI
      2200954: CodTOM := '1188'; // Aroeiras Do Itaim/PI
      2201002: CodTOM := '1019'; // Arraial/PI
      2201051: CodTOM := '0270'; // Assuncao Do Piaui/PI
      2201101: CodTOM := '1021'; // Avelino Lopes/PI
      2201150: CodTOM := '2245'; // Baixa Grande Do Ribeiro/PI
      2201176: CodTOM := '0272'; // Barra D Alcantara/PI
      2201200: CodTOM := '1023'; // Barras/PI
      2201309: CodTOM := '1025'; // Barreiras Do Piaui/PI
      2201408: CodTOM := '1027'; // Barro Duro/PI
      2201507: CodTOM := '1029'; // Batalha/PI
      2201556: CodTOM := '0274'; // Bela Vista Do Piaui/PI
      2201572: CodTOM := '0276'; // Belem Do Piaui/PI
      2201606: CodTOM := '1031'; // Beneditinos/PI
      2201705: CodTOM := '1033'; // Bertolinia/PI
      2201739: CodTOM := '0278'; // Betania Do Piaui/PI
      2201770: CodTOM := '0280'; // Boa Hora/PI
      2201804: CodTOM := '1035'; // Bocaina/PI
      2201903: CodTOM := '1037'; // Bom Jesus/PI
      2201919: CodTOM := '2287'; // Bom Principio Do Piaui/PI
      2201929: CodTOM := '2251'; // Bonfim Do Piaui/PI
      2201945: CodTOM := '0282'; // Boqueirao Do Piaui/PI
      2201960: CodTOM := '2283'; // Brasileira/PI
      2201988: CodTOM := '0284'; // Brejo Do Piaui/PI
      2202000: CodTOM := '1039'; // Buriti Dos Lopes/PI
      2202026: CodTOM := '1297'; // Buriti Dos Montes/PI
      2202059: CodTOM := '1299'; // Cabeceiras Do Piaui/PI
      2202075: CodTOM := '0286'; // Cajazeiras Do Piaui/PI
      2202083: CodTOM := '0288'; // Cajueiro Da Praia/PI
      2202091: CodTOM := '2271'; // Caldeirao Grande Do Piaui/PI
      2202109: CodTOM := '1041'; // Campinas Do Piaui/PI
      2202117: CodTOM := '0290'; // Campo Alegre Do Fidalgo/PI
      2202133: CodTOM := '0292'; // Campo Grande Do Piaui/PI
      2202174: CodTOM := '0294'; // Campo Largo Do Piaui/PI
      2202208: CodTOM := '1043'; // Campo Maior/PI
      2202251: CodTOM := '2247'; // Canavieira/PI
      2202307: CodTOM := '1045'; // Canto Do Buriti/PI
      2202406: CodTOM := '1047'; // Capitao De Campos/PI
      2202455: CodTOM := '0296'; // Capitao Gervasio Oliveira/PI
      2202505: CodTOM := '1049'; // Caracol/PI
      2202539: CodTOM := '0298'; // Caraubas Do Piaui/PI
      2202554: CodTOM := '0300'; // Caridade Do Piaui/PI
      2202604: CodTOM := '1051'; // Castelo Do Piaui/PI
      2202653: CodTOM := '0302'; // Caxingo/PI
      2202703: CodTOM := '1053'; // Cocal/PI
      2202711: CodTOM := '0304'; // Cocal De Telha/PI
      2202729: CodTOM := '0306'; // Cocal Dos Alves/PI
      2202737: CodTOM := '0995'; // Coivaras/PI
      2202752: CodTOM := '2249'; // Colonia Do Gurgueia/PI
      2202778: CodTOM := '2253'; // Colonia Do Piaui/PI
      2202802: CodTOM := '1055'; // Conceicao Do Caninde/PI
      2202851: CodTOM := '2255'; // Coronel Jose Dias/PI
      2202901: CodTOM := '1057'; // Corrente/PI
      2203008: CodTOM := '1059'; // Cristalandia Do Piaui/PI
      2203107: CodTOM := '1061'; // Cristino Castro/PI
      2203206: CodTOM := '1063'; // Curimata/PI
      2203230: CodTOM := '0308'; // Currais/PI
      2203255: CodTOM := '0310'; // Curralinhos/PI
      2203271: CodTOM := '0312'; // Curral Novo Do Piaui/PI
      2203305: CodTOM := '1065'; // Demerval Lobao/PI
      2203354: CodTOM := '1229'; // Dirceu Arcoverde/PI
      2203404: CodTOM := '1067'; // Dom Expedito Lopes/PI
      2203420: CodTOM := '1141'; // Domingos Mourao/PI
      2203453: CodTOM := '1289'; // Dom Inocencio/PI
      2203503: CodTOM := '1069'; // Elesbao Veloso/PI
      2203602: CodTOM := '1071'; // Eliseu Martins/PI
      2203701: CodTOM := '1073'; // Esperantina/PI
      2203750: CodTOM := '2257'; // Fartura Do Piaui/PI
      2203800: CodTOM := '1075'; // Flores Do Piaui/PI
      2203859: CodTOM := '0314'; // Floresta Do Piaui/PI
      2203909: CodTOM := '1077'; // Floriano/PI
      2204006: CodTOM := '1079'; // Francinopolis/PI
      2204105: CodTOM := '1081'; // Francisco Ayres/PI
      2204154: CodTOM := '0316'; // Francisco Macedo/PI
      2204204: CodTOM := '1083'; // Francisco Santos/PI
      2204303: CodTOM := '1085'; // Fronteiras/PI
      2204352: CodTOM := '0318'; // Geminiano/PI
      2204402: CodTOM := '1087'; // Gilbues/PI
      2204501: CodTOM := '1089'; // Guadalupe/PI
      2204550: CodTOM := '0320'; // Guaribas/PI
      2204600: CodTOM := '1091'; // Hugo Napoleao/PI
      2204659: CodTOM := '0322'; // Ilha Grande/PI
      2204709: CodTOM := '1093'; // Inhuma/PI
      2204808: CodTOM := '1095'; // Ipiranga Do Piaui/PI
      2204907: CodTOM := '1097'; // Isaias Coelho/PI
      2205003: CodTOM := '1099'; // Itainopolis/PI
      2205102: CodTOM := '1101'; // Itaueira/PI
      2205151: CodTOM := '2273'; // Jacobina Do Piaui/PI
      2205201: CodTOM := '1103'; // Jaicos/PI
      2205250: CodTOM := '0997'; // Jardim Do Mulato/PI
      2205276: CodTOM := '0324'; // Jatoba Do Piaui/PI
      2205300: CodTOM := '1105'; // Jerumenha/PI
      2205359: CodTOM := '0326'; // Joao Costa/PI
      2205409: CodTOM := '1107'; // Joaquim Pires/PI
      2205458: CodTOM := '0328'; // Joca Marques/PI
      2205508: CodTOM := '1109'; // Jose De Freitas/PI
      2205516: CodTOM := '0330'; // Juazeiro Do Piaui/PI
      2205524: CodTOM := '0332'; // Julio Borges/PI
      2205532: CodTOM := '0334'; // Jurema/PI
      2205540: CodTOM := '0336'; // Lagoinha Do Piaui/PI
      2205557: CodTOM := '0999'; // Lagoa Alegre/PI
      2205565: CodTOM := '2259'; // Lagoa Do Barro Do Piaui/PI
      2205573: CodTOM := '0338'; // Lagoa De Sao Francisco/PI
      2205581: CodTOM := '0340'; // Lagoa Do Piaui/PI
      2205599: CodTOM := '0342'; // Lagoa Do Sitio/PI
      2205607: CodTOM := '1111'; // Landri Sales/PI
      2205706: CodTOM := '1113'; // Luis Correia/PI
      2205805: CodTOM := '1115'; // Luzilandia/PI
      2205854: CodTOM := '0344'; // Madeiro/PI
      2205904: CodTOM := '1117'; // Manoel Emidio/PI
      2205953: CodTOM := '2275'; // Marcolandia/PI
      2206001: CodTOM := '1119'; // Marcos Parente/PI
      2206050: CodTOM := '0346'; // Massape Do Piaui/PI
      2206100: CodTOM := '1121'; // Matias Olimpio/PI
      2206209: CodTOM := '1123'; // Miguel Alves/PI
      2206308: CodTOM := '1125'; // Miguel Leao/PI
      2206357: CodTOM := '0348'; // Milton Brandao/PI
      2206407: CodTOM := '1127'; // Monsenhor Gil/PI
      2206506: CodTOM := '1129'; // Monsenhor Hipolito/PI
      2206605: CodTOM := '1131'; // Monte Alegre Do Piaui/PI
      2206654: CodTOM := '0350'; // Morro Cabeca No Tempo/PI
      2206670: CodTOM := '0352'; // Morro Do Chapeu Do Piaui/PI
      2206696: CodTOM := '0354'; // Murici Dos Portelas/PI
      2206704: CodTOM := '1133'; // Nazare Do Piaui/PI
      2206720: CodTOM := '1180'; // Nazária/PI
      2206753: CodTOM := '0356'; // Nossa Senhora De Nazare/PI
      2206803: CodTOM := '1135'; // Nossa Senhora Dos Remedios/PI
      2206902: CodTOM := '1137'; // Novo Oriente Do Piaui/PI
      2206951: CodTOM := '0358'; // Novo Santo Antonio/PI
      2207009: CodTOM := '1139'; // Oeiras/PI
      2207108: CodTOM := '0360'; // Olho D Agua Do Piaui/PI
      2207207: CodTOM := '1143'; // Padre Marcos/PI
      2207306: CodTOM := '1145'; // Paes Landim/PI
      2207355: CodTOM := '0362'; // Pajeu Do Piaui/PI
      2207405: CodTOM := '1147'; // Palmeira Do Piaui/PI
      2207504: CodTOM := '1149'; // Palmeirais/PI
      2207553: CodTOM := '0364'; // Paqueta/PI
      2207603: CodTOM := '1151'; // Parnagua/PI
      2207702: CodTOM := '1153'; // Parnaiba/PI
      2207751: CodTOM := '1293'; // Passagem Franca Do Piaui/PI
      2207777: CodTOM := '2277'; // Patos Do Piaui/PI
      2207793: CodTOM := '1104'; // Pau D Arco Do Piaui/PI
      2207801: CodTOM := '1155'; // Paulistana/PI
      2207850: CodTOM := '0366'; // Pavussu/PI
      2207900: CodTOM := '1157'; // Pedro Ii/PI
      2207934: CodTOM := '0368'; // Pedro Laurentino/PI
      2207959: CodTOM := '0370'; // Nova Santa Rita/PI
      2208007: CodTOM := '1159'; // Picos/PI
      2208106: CodTOM := '1161'; // Pimenteiras/PI
      2208205: CodTOM := '1163'; // Pio Ix/PI
      2208304: CodTOM := '1165'; // Piracuruca/PI
      2208403: CodTOM := '1167'; // Piripiri/PI
      2208502: CodTOM := '1169'; // Porto/PI
      2208551: CodTOM := '0372'; // Porto Alegre Do Piaui/PI
      2208601: CodTOM := '1171'; // Prata Do Piaui/PI
      2208650: CodTOM := '2279'; // Queimada Nova/PI
      2208700: CodTOM := '1173'; // Redencao Do Gurgueia/PI
      2208809: CodTOM := '1175'; // Regeneracao/PI
      2208858: CodTOM := '0374'; // Riacho Frio/PI
      2208874: CodTOM := '0376'; // Ribeira Do Piaui/PI
      2208908: CodTOM := '1177'; // Ribeiro Goncalves/PI
      2209005: CodTOM := '1179'; // Rio Grande Do Piaui/PI
      2209104: CodTOM := '1181'; // Santa Cruz Do Piaui/PI
      2209153: CodTOM := '1295'; // Santa Cruz Dos Milagres/PI
      2209203: CodTOM := '1183'; // Santa Filomena/PI
      2209302: CodTOM := '1185'; // Santa Luz/PI
      2209351: CodTOM := '2281'; // Santana Do Piaui/PI
      2209377: CodTOM := '2261'; // Santa Rosa Do Piaui/PI
      2209401: CodTOM := '1187'; // Santo Antonio De Lisboa/PI
      2209450: CodTOM := '0378'; // Santo Antonio Dos Milagres/PI
      2209500: CodTOM := '1189'; // Santo Inacio Do Piaui/PI
      2209559: CodTOM := '2263'; // Sao Braz Do Piaui/PI
      2209609: CodTOM := '1191'; // Sao Felix Do Piaui/PI
      2209658: CodTOM := '0380'; // Sao Francisco De Assis Do Piaui/PI
      2209708: CodTOM := '1193'; // Sao Francisco Do Piaui/PI
      2209757: CodTOM := '0382'; // Sao Goncalo Do Gurgueia/PI
      2209807: CodTOM := '1195'; // Sao Goncalo Do Piaui/PI
      2209856: CodTOM := '1291'; // Sao Joao Da Canabrava/PI
      2209872: CodTOM := '0384'; // Sao Joao Da Fronteira/PI
      2209906: CodTOM := '1197'; // Sao Joao Da Serra/PI
      2209955: CodTOM := '0386'; // Sao Joao Da Varjota/PI
      2209971: CodTOM := '0388'; // Sao Joao Do Arraial/PI
      2210003: CodTOM := '1199'; // Sao Joao Do Piaui/PI
      2210052: CodTOM := '2285'; // Sao Jose Do Divino/PI
      2210102: CodTOM := '1201'; // Sao Jose Do Peixe/PI
      2210201: CodTOM := '1203'; // Sao Jose Do Piaui/PI
      2210300: CodTOM := '1205'; // Sao Juliao/PI
      2210359: CodTOM := '2265'; // Sao Lourenco Do Piaui/PI
      2210375: CodTOM := '0390'; // Sao Luis Do Piaui/PI
      2210383: CodTOM := '0392'; // Sao Miguel Da Baixa Grande/PI
      2210391: CodTOM := '0394'; // Sao Miguel Do Fidalgo/PI
      2210409: CodTOM := '1207'; // Sao Miguel Do Tapuio/PI
      2210508: CodTOM := '1209'; // Sao Pedro Do Piaui/PI
      2210607: CodTOM := '1211'; // Sao Raimundo Nonato/PI
      2210623: CodTOM := '0396'; // Sebastiao Barros/PI
      2210631: CodTOM := '0398'; // Sebastiao Leal/PI
      2210656: CodTOM := '1379'; // Sigefredo Pacheco/PI
      2210706: CodTOM := '1213'; // Simoes/PI
      2210805: CodTOM := '1215'; // Simplicio Mendes/PI
      2210904: CodTOM := '1217'; // Socorro Do Piaui/PI
      2210938: CodTOM := '0400'; // Sussuapara/PI
      2210953: CodTOM := '0402'; // Tamboril Do Piaui/PI
      2210979: CodTOM := '0404'; // Tanque Do Piaui/PI
      2211001: CodTOM := '1219'; // Teresina/PI
      2211100: CodTOM := '1221'; // Uniao/PI
      2211209: CodTOM := '1223'; // Urucui/PI
      2211308: CodTOM := '1225'; // Valenca Do Piaui/PI
      2211357: CodTOM := '2267'; // Varzea Branca/PI
      2211407: CodTOM := '1227'; // Varzea Grande/PI
      2211506: CodTOM := '0406'; // Vera Mendes/PI
      2211605: CodTOM := '0408'; // Vila Nova Do Piaui/PI
      2211704: CodTOM := '0410'; // Wall Ferraz/PI
   end;
 end;

 procedure P23;
 begin
   case ACodigo of
      2300101: CodTOM := '1301'; // Abaiara/CE';
      2300150: CodTOM := '1231'; // Acarape/CE';
      2300200: CodTOM := '1303'; // Acarau/CE';
      2300309: CodTOM := '1305'; // Acopiara/CE';
      2300408: CodTOM := '1307'; // Aiuaba/CE';
      2300507: CodTOM := '1309'; // Alcantaras/CE';
      2300606: CodTOM := '1311'; // Altaneira/CE';
      2300705: CodTOM := '1313'; // Alto Santo/CE';
      2300754: CodTOM := '1587'; // Amontada/CE';
      2300804: CodTOM := '1315'; // Antonina Do Norte/CE';
      2300903: CodTOM := '1317'; // Apuiares/CE';
      2301000: CodTOM := '1319'; // Aquiraz/CE';
      2301109: CodTOM := '1321'; // Aracati/CE';
      2301208: CodTOM := '1323'; // Aracoiaba/CE';
      2301257: CodTOM := '0989'; // Ararenda/CE';
      2301307: CodTOM := '1325'; // Araripe/CE';
      2301406: CodTOM := '1327'; // Aratuba/CE';
      2301505: CodTOM := '1329'; // Arneiroz/CE';
      2301604: CodTOM := '1331'; // Assare/CE';
      2301703: CodTOM := '1333'; // Aurora/CE';
      2301802: CodTOM := '1335'; // Baixio/CE';
      2301851: CodTOM := '1233'; // Banabuiu/CE';
      2301901: CodTOM := '1337'; // Barbalha/CE';
      2301950: CodTOM := '1235'; // Barreira/CE';
      2302008: CodTOM := '1339'; // Barro/CE';
      2302057: CodTOM := '1237'; // Barroquinha/CE';
      2302107: CodTOM := '1341'; // Baturite/CE';
      2302206: CodTOM := '1343'; // Beberibe/CE';
      2302305: CodTOM := '1345'; // Bela Cruz/CE';
      2302404: CodTOM := '1347'; // Boa Viagem/CE';
      2302503: CodTOM := '1349'; // Brejo Santo/CE';
      2302602: CodTOM := '1351'; // Camocim/CE';
      2302701: CodTOM := '1353'; // Campos Sales/CE';
      2302800: CodTOM := '1355'; // Caninde/CE';
      2302909: CodTOM := '1357'; // Capistrano/CE';
      2303006: CodTOM := '1359'; // Caridade/CE';
      2303105: CodTOM := '1361'; // Carire/CE';
      2303204: CodTOM := '1363'; // Caririacu/CE';
      2303303: CodTOM := '1365'; // Carius/CE';
      2303402: CodTOM := '1367'; // Carnaubal/CE';
      2303501: CodTOM := '1369'; // Cascavel/CE';
      2303600: CodTOM := '1371'; // Catarina/CE';
      2303659: CodTOM := '0983'; // Catunda/CE';
      2303709: CodTOM := '1373'; // Caucaia/CE';
      2303808: CodTOM := '1375'; // Cedro/CE';
      2303907: CodTOM := '1377'; // Chaval/CE';
      2303931: CodTOM := '0993'; // Choro/CE';
      2303956: CodTOM := '1239'; // Chorozinho/CE';
      2304004: CodTOM := '1381'; // Coreau/CE';
      2304103: CodTOM := '1383'; // Crateus/CE';
      2304202: CodTOM := '1385'; // Crato/CE';
      2304236: CodTOM := '1241'; // Croata/CE';
      2304251: CodTOM := '1589'; // Cruz/CE';
      2304269: CodTOM := '1243'; // Deputado Irapuan Pinheiro/CE';
      2304277: CodTOM := '1245'; // Erere/CE';
      2304285: CodTOM := '1247'; // Eusebio/CE';
      2304301: CodTOM := '1387'; // Farias Brito/CE';
      2304350: CodTOM := '1591'; // Forquilha/CE';
      2304400: CodTOM := '1389'; // Fortaleza/CE';
      2304459: CodTOM := '0987'; // Fortim/CE';
      2304509: CodTOM := '1391'; // Frecheirinha/CE';
      2304608: CodTOM := '1393'; // General Sampaio/CE';
      2304657: CodTOM := '1249'; // Graca/CE';
      2304707: CodTOM := '1395'; // Granja/CE';
      2304806: CodTOM := '1397'; // Granjeiro/CE';
      2304905: CodTOM := '1399'; // Groairas/CE';
      2304954: CodTOM := '1251'; // Guaiuba/CE';
      2305001: CodTOM := '1401'; // Guaraciaba Do Norte/CE';
      2305100: CodTOM := '1403'; // Guaramiranga/CE';
      2305209: CodTOM := '1405'; // Hidrolandia/CE';
      2305233: CodTOM := '1253'; // Horizonte/CE';
      2305266: CodTOM := '1255'; // Ibaretama/CE';
      2305308: CodTOM := '1407'; // Ibiapina/CE';
      2305332: CodTOM := '1257'; // Ibicuitinga/CE';
      2305357: CodTOM := '1593'; // Icapui/CE';
      2305407: CodTOM := '1409'; // Ico/CE';
      2305506: CodTOM := '1411'; // Iguatu/CE';
      2305605: CodTOM := '1413'; // Independencia/CE';
      2305654: CodTOM := '1259'; // Ipaporanga/CE';
      2305704: CodTOM := '1415'; // Ipaumirim/CE';
      2305803: CodTOM := '1417'; // Ipu/CE';
      2305902: CodTOM := '1419'; // Ipueiras/CE';
      2306009: CodTOM := '1421'; // Iracema/CE';
      2306108: CodTOM := '1423'; // Iraucuba/CE';
      2306207: CodTOM := '1425'; // Itaicaba/CE';
      2306256: CodTOM := '0991'; // Itaitinga/CE';
      2306306: CodTOM := '1427'; // Itapage/CE';
      2306405: CodTOM := '1429'; // Itapipoca/CE';
      2306504: CodTOM := '1431'; // Itapiuna/CE';
      2306553: CodTOM := '1595'; // Itarema/CE';
      2306603: CodTOM := '1433'; // Itatira/CE';
      2306702: CodTOM := '1435'; // Jaguaretama/CE';
      2306801: CodTOM := '1437'; // Jaguaribara/CE';
      2306900: CodTOM := '1439'; // Jaguaribe/CE';
      2307007: CodTOM := '1441'; // Jaguaruana/CE';
      2307106: CodTOM := '1443'; // Jardim/CE';
      2307205: CodTOM := '1445'; // Jati/CE';
      2307254: CodTOM := '0985'; // Jijoca De Jericoacoara/CE';
      2307304: CodTOM := '1447'; // Juazeiro Do Norte/CE';
      2307403: CodTOM := '1449'; // Jucas/CE';
      2307502: CodTOM := '1451'; // Lavras Da Mangabeira/CE';
      2307601: CodTOM := '1453'; // Limoeiro Do Norte/CE';
      2307635: CodTOM := '1261'; // Madalena/CE';
      2307650: CodTOM := '1585'; // Maracanau/CE';
      2307700: CodTOM := '1455'; // Maranguape/CE';
      2307809: CodTOM := '1457'; // Marco/CE';
      2307908: CodTOM := '1459'; // Martinopole/CE';
      2308005: CodTOM := '1461'; // Massape/CE';
      2308104: CodTOM := '1463'; // Mauriti/CE';
      2308203: CodTOM := '1465'; // Meruoca/CE';
      2308302: CodTOM := '1467'; // Milagres/CE';
      2308351: CodTOM := '1597'; // Milha/CE';
      2308377: CodTOM := '1263'; // Miraima/CE';
      2308401: CodTOM := '1469'; // Missao Velha/CE';
      2308500: CodTOM := '1471'; // Mombaca/CE';
      2308609: CodTOM := '1473'; // Monsenhor Tabosa/CE';
      2308708: CodTOM := '1475'; // Morada Nova/CE';
      2308807: CodTOM := '1477'; // Moraujo/CE';
      2308906: CodTOM := '1479'; // Morrinhos/CE';
      2309003: CodTOM := '1481'; // Mucambo/CE';
      2309102: CodTOM := '1483'; // Mulungu/CE';
      2309201: CodTOM := '1485'; // Nova Olinda/CE';
      2309300: CodTOM := '1487'; // Nova Russas/CE';
      2309409: CodTOM := '1489'; // Novo Oriente/CE';
      2309458: CodTOM := '1265'; // Ocara/CE';
      2309508: CodTOM := '1491'; // Oros/CE';
      2309607: CodTOM := '1493'; // Pacajus/CE';
      2309706: CodTOM := '1495'; // Pacatuba/CE';
      2309805: CodTOM := '1497'; // Pacoti/CE';
      2309904: CodTOM := '1499'; // Pacuja/CE';
      2310001: CodTOM := '1501'; // Palhano/CE';
      2310100: CodTOM := '1503'; // Palmacia/CE';
      2310209: CodTOM := '1505'; // Paracuru/CE';
      2310258: CodTOM := '1599'; // Paraipaba/CE';
      2310308: CodTOM := '1507'; // Parambu/CE';
      2310407: CodTOM := '1509'; // Paramoti/CE';
      2310506: CodTOM := '1511'; // Pedra Branca/CE';
      2310605: CodTOM := '1513'; // Penaforte/CE';
      2310704: CodTOM := '1515'; // Pentecoste/CE';
      2310803: CodTOM := '1517'; // Pereiro/CE';
      2310852: CodTOM := '1267'; // Pindoretama/CE';
      2310902: CodTOM := '1519'; // Piquet Carneiro/CE';
      2310951: CodTOM := '1269'; // Pires Ferreira/CE';
      2311009: CodTOM := '1521'; // Poranga/CE';
      2311108: CodTOM := '1523'; // Porteiras/CE';
      2311207: CodTOM := '1525'; // Potengi/CE';
      2311231: CodTOM := '1271'; // Potiretama/CE';
      2311264: CodTOM := '9917'; // Quiterianopolis/CE';
      2311306: CodTOM := '1527'; // Quixada/CE';
      2311355: CodTOM := '9853'; // Quixelo/CE';
      2311405: CodTOM := '1529'; // Quixeramobim/CE';
      2311504: CodTOM := '1531'; // Quixere/CE';
      2311603: CodTOM := '1533'; // Redencao/CE';
      2311702: CodTOM := '1535'; // Reriutaba/CE';
      2311801: CodTOM := '1537'; // Russas/CE';
      2311900: CodTOM := '1539'; // Saboeiro/CE';
      2311959: CodTOM := '1273'; // Salitre/CE';
      2312007: CodTOM := '1541'; // Santana Do Acarau/CE';
      2312106: CodTOM := '1543'; // Santana Do Cariri/CE';
      2312205: CodTOM := '1545'; // Santa Quiteria/CE';
      2312304: CodTOM := '1547'; // Sao Benedito/CE';
      2312403: CodTOM := '1549'; // Sao Goncalo Do Amarante/CE';
      2312502: CodTOM := '1551'; // Sao Joao Do Jaguaribe/CE';
      2312601: CodTOM := '1553'; // Sao Luis Do Curu/CE';
      2312700: CodTOM := '1555'; // Senador Pompeu/CE';
      2312809: CodTOM := '1557'; // Senador Sa/CE';
      2312908: CodTOM := '1559'; // Sobral/CE';
      2313005: CodTOM := '1561'; // Solonopole/CE';
      2313104: CodTOM := '1563'; // Tabuleiro Do Norte/CE';
      2313203: CodTOM := '1565'; // Tamboril/CE';
      2313252: CodTOM := '1275'; // Tarrafas/CE';
      2313302: CodTOM := '1567'; // Taua/CE';
      2313351: CodTOM := '1277'; // Tejucuoca/CE';
      2313401: CodTOM := '1569'; // Tiangua/CE';
      2313500: CodTOM := '1571'; // Trairi/CE';
      2313559: CodTOM := '1279'; // Tururu/CE';
      2313609: CodTOM := '1573'; // Ubajara/CE';
      2313708: CodTOM := '1575'; // Umari/CE';
      2313757: CodTOM := '9855'; // Umirim/CE';
      2313807: CodTOM := '1577'; // Uruburetama/CE';
      2313906: CodTOM := '1579'; // Uruoca/CE';
      2313955: CodTOM := '9857'; // Varjota/CE';
      2314003: CodTOM := '1581'; // Varzea Alegre/CE';
      2314102: CodTOM := '1583'; // Vicosa Do Ceara/CE';
   end;
 end;

 procedure P24;
 begin
   case ACodigo of
      2400109: CodTOM := '1601'; // Acari/RN';
      2400208: CodTOM := '1603'; // Acu/RN';
      2400307: CodTOM := '1605'; // Afonso Bezerra/RN';
      2400406: CodTOM := '1607'; // Agua Nova/RN';
      2400505: CodTOM := '1609'; // Alexandria/RN';
      2400604: CodTOM := '1611'; // Almino Afonso/RN';
      2400703: CodTOM := '1613'; // Alto Do Rodrigues/RN';
      2400802: CodTOM := '1615'; // Angicos/RN';
      2400901: CodTOM := '1617'; // Antonio Martins/RN';
      2401008: CodTOM := '1619'; // Apodi/RN';
      2401107: CodTOM := '1621'; // Areia Branca/RN';
      2401206: CodTOM := '1623'; // Ares/RN';
      2401305: CodTOM := '1625'; // Augusto Severo/RN';
      2401404: CodTOM := '1627'; // Baia Formosa/RN';
      2401453: CodTOM := '3003'; // Barauna/RN';
      2401503: CodTOM := '1629'; // Barcelona/RN';
      2401602: CodTOM := '1631'; // Bento Fernandes/RN';
      2401651: CodTOM := '0412'; // Bodo/RN';
      2401701: CodTOM := '1633'; // Bom Jesus/RN';
      2401800: CodTOM := '1635'; // Brejinho/RN';
      2401859: CodTOM := '0414'; // Caicara Do Norte/RN';
      2401909: CodTOM := '1637'; // Caicara Do Rio Do Vento/RN';
      2402006: CodTOM := '1639'; // Caico/RN';
      2402105: CodTOM := '1641'; // Campo Redondo/RN';
      2402204: CodTOM := '1643'; // Canguaretama/RN';
      2402303: CodTOM := '1645'; // Caraubas/RN';
      2402402: CodTOM := '1647'; // Carnauba Dos Dantas/RN';
      2402501: CodTOM := '1649'; // Carnaubais/RN';
      2402600: CodTOM := '1651'; // Ceara-Mirim/RN';
      2402709: CodTOM := '1653'; // Cerro Cora/RN';
      2402808: CodTOM := '1655'; // Coronel Ezequiel/RN';
      2402907: CodTOM := '1657'; // Coronel Joao Pessoa/RN';
      2403004: CodTOM := '1659'; // Cruzeta/RN';
      2403103: CodTOM := '1661'; // Currais Novos/RN';
      2403202: CodTOM := '1663'; // Doutor Severiano/RN';
      2403251: CodTOM := '1779'; // Parnamirim/RN';
      2403301: CodTOM := '1665'; // Encanto/RN';
      2403400: CodTOM := '1667'; // Equador/RN';
      2403509: CodTOM := '1669'; // Espirito Santo/RN';
      2403608: CodTOM := '1671'; // Extremoz/RN';
      2403707: CodTOM := '1673'; // Felipe Guerra/RN';
      2403756: CodTOM := '0416'; // Fernando Pedroza/RN';
      2403806: CodTOM := '1675'; // Florania/RN';
      2403905: CodTOM := '1677'; // Francisco Dantas/RN';
      2404002: CodTOM := '1751'; // Frutuoso Gomes/RN';
      2404101: CodTOM := '1679'; // Galinhos/RN';
      2404200: CodTOM := '1681'; // Goianinha/RN';
      2404309: CodTOM := '1683'; // Governador Dix-Sept Rosado/RN';
      2404408: CodTOM := '1685'; // Grossos/RN';
      2404507: CodTOM := '1687'; // Guamare/RN';
      2404606: CodTOM := '1689'; // Ielmo Marinho/RN';
      2404705: CodTOM := '1691'; // Ipanguacu/RN';
      2404804: CodTOM := '1693'; // Ipueira/RN';
      2404853: CodTOM := '0418'; // Itaja/RN';
      2404903: CodTOM := '1695'; // Itau/RN';
      2405009: CodTOM := '1697'; // Jacana/RN';
      2405108: CodTOM := '1699'; // Jandaira/RN';
      2405207: CodTOM := '1701'; // Janduis/RN';
      2405306: CodTOM := '1703'; // Januario Cicco/RN';
      2405405: CodTOM := '1705'; // Japi/RN';
      2405504: CodTOM := '1707'; // Jardim De Angicos/RN';
      2405603: CodTOM := '1709'; // Jardim De Piranhas/RN';
      2405702: CodTOM := '1711'; // Jardim Do Serido/RN';
      2405801: CodTOM := '1713'; // Joao Camara/RN';
      2405900: CodTOM := '1715'; // Joao Dias/RN';
      2406007: CodTOM := '1717'; // Jose Da Penha/RN';
      2406106: CodTOM := '1719'; // Jucurutu/RN';
      2406155: CodTOM := '1108'; // Jundia/RN';
      2406205: CodTOM := '1723'; // Lagoa D Anta/RN';
      2406304: CodTOM := '1725'; // Lagoa De Pedras/RN';
      2406403: CodTOM := '1727'; // Lagoa De Velhos/RN';
      2406502: CodTOM := '1729'; // Lagoa Nova/RN';
      2406601: CodTOM := '1731'; // Lagoa Salgada/RN';
      2406700: CodTOM := '1733'; // Lajes/RN';
      2406809: CodTOM := '1735'; // Lajes Pintadas/RN';
      2406908: CodTOM := '1737'; // Lucrecia/RN';
      2407005: CodTOM := '1739'; // Luis Gomes/RN';
      2407104: CodTOM := '1741'; // Macaiba/RN';
      2407203: CodTOM := '1743'; // Macau/RN';
      2407252: CodTOM := '0420'; // Major Sales/RN';
      2407302: CodTOM := '1745'; // Marcelino Vieira/RN';
      2407401: CodTOM := '1747'; // Martins/RN';
      2407500: CodTOM := '1749'; // Maxaranguape/RN';
      2407609: CodTOM := '1721'; // Messias Targino/RN';
      2407708: CodTOM := '1753'; // Montanhas/RN';
      2407807: CodTOM := '1755'; // Monte Alegre/RN';
      2407906: CodTOM := '1757'; // Monte Das Gameleiras/RN';
      2408003: CodTOM := '1759'; // Mossoro/RN';
      2408102: CodTOM := '1761'; // Natal/RN';
      2408201: CodTOM := '1763'; // Nisia Floresta/RN';
      2408300: CodTOM := '1765'; // Nova Cruz/RN';
      2408409: CodTOM := '1767'; // Olho-D Agua Do Borges/RN';
      2408508: CodTOM := '1769'; // Ouro Branco/RN';
      2408607: CodTOM := '1771'; // Parana/RN';
      2408706: CodTOM := '1773'; // Parau/RN';
      2408805: CodTOM := '1775'; // Parazinho/RN';
      2408904: CodTOM := '1777'; // Parelhas/RN';
      2408953: CodTOM := '0422'; // Rio Do Fogo/RN';
      2409100: CodTOM := '1781'; // Passa E Fica/RN';
      2409209: CodTOM := '1783'; // Passagem/RN';
      2409308: CodTOM := '1785'; // Patu/RN';
      2409332: CodTOM := '0424'; // Santa Maria/RN';
      2409407: CodTOM := '1787'; // Pau Dos Ferros/RN';
      2409506: CodTOM := '1789'; // Pedra Grande/RN';
      2409605: CodTOM := '1791'; // Pedra Preta/RN';
      2409704: CodTOM := '1793'; // Pedro Avelino/RN';
      2409803: CodTOM := '1795'; // Pedro Velho/RN';
      2409902: CodTOM := '1797'; // Pendencias/RN';
      2410009: CodTOM := '1799'; // Piloes/RN';
      2410108: CodTOM := '1801'; // Poco Branco/RN';
      2410207: CodTOM := '1803'; // Portalegre/RN';
      2410256: CodTOM := '0426'; // Porto Do Mangue/RN';
      2410306: CodTOM := '1805'; // Presidente Juscelino/RN';
      2410405: CodTOM := '1807'; // Pureza/RN';
      2410504: CodTOM := '1809'; // Rafael Fernandes/RN';
      2410603: CodTOM := '1893'; // Rafael Godeiro/RN';
      2410702: CodTOM := '1811'; // Riacho Da Cruz/RN';
      2410801: CodTOM := '1813'; // Riacho De Santana/RN';
      2410900: CodTOM := '1815'; // Riachuelo/RN';
      2411007: CodTOM := '1817'; // Rodolfo Fernandes/RN';
      2411056: CodTOM := '0428'; // Tibau/RN';
      2411106: CodTOM := '1819'; // Ruy Barbosa/RN';
      2411205: CodTOM := '1823'; // Santa Cruz/RN';
      2411403: CodTOM := '1827'; // Santana Do Matos/RN';
      2411429: CodTOM := '1825'; // Santana Do Serido/RN';
      2411502: CodTOM := '1829'; // Santo Antonio/RN';
      2411601: CodTOM := '1831'; // Sao Bento Do Norte/RN';
      2411700: CodTOM := '1833'; // Sao Bento Do Trairi/RN';
      2411809: CodTOM := '1835'; // Sao Fernando/RN';
      2411908: CodTOM := '1821'; // Sao Francisco Do Oeste/RN';
      2412005: CodTOM := '1837'; // Sao Goncalo Do Amarante/RN';
      2412104: CodTOM := '1839'; // Sao Joao Do Sabugi/RN';
      2412203: CodTOM := '1841'; // Sao Jose De Mipibu/RN';
      2412302: CodTOM := '1843'; // Sao Jose Do Campestre/RN';
      2412401: CodTOM := '1845'; // Sao Jose Do Serido/RN';
      2412500: CodTOM := '1847'; // Sao Miguel/RN';
      2412559: CodTOM := '0430'; // Sao Miguel Do Gostoso/RN';
      2412609: CodTOM := '1849'; // Sao Paulo Do Potengi/RN';
      2412708: CodTOM := '1851'; // Sao Pedro/RN';
      2412807: CodTOM := '1853'; // Sao Rafael/RN';
      2412906: CodTOM := '1855'; // Sao Tome/RN';
      2413003: CodTOM := '1857'; // Sao Vicente/RN';
      2413102: CodTOM := '1859'; // Senador Eloi De Souza/RN';
      2413201: CodTOM := '1861'; // Senador Georgino Avelino/RN';
      2413300: CodTOM := '1863'; // Serra De Sao Bento/RN';
      2413359: CodTOM := '1927'; // Serra Do Mel/RN';
      2413409: CodTOM := '1865'; // Serra Negra Do Norte/RN';
      2413508: CodTOM := '1867'; // Serrinha/RN';
      2413557: CodTOM := '0432'; // Serrinha Dos Pintos/RN';
      2413607: CodTOM := '1869'; // Severiano Melo/RN';
      2413706: CodTOM := '1871'; // Sitio Novo/RN';
      2413805: CodTOM := '1873'; // Taboleiro Grande/RN';
      2413904: CodTOM := '1875'; // Taipu/RN';
      2414001: CodTOM := '1877'; // Tangara/RN';
      2414100: CodTOM := '1879'; // Tenente Ananias/RN';
      2414159: CodTOM := '0434'; // Tenente Laurentino Cruz/RN';
      2414209: CodTOM := '1881'; // Tibau Do Sul/RN';
      2414308: CodTOM := '1883'; // Timbauba Dos Batistas/RN';
      2414407: CodTOM := '1885'; // Touros/RN';
      2414456: CodTOM := '0436'; // Triunfo Potiguar/RN';
      2414506: CodTOM := '1887'; // Umarizal/RN';
      2414605: CodTOM := '1889'; // Upanema/RN';
      2414704: CodTOM := '1891'; // Varzea/RN';
      2414753: CodTOM := '0438'; // Venha-Ver/RN';
      2414803: CodTOM := '1895'; // Vera Cruz/RN';
      2414902: CodTOM := '1897'; // Vicosa/RN';
      2415008: CodTOM := '1899'; // Vila Flor/RN';
   end;
 end;

 procedure P25;
 begin
   case ACodigo of
      2500106: CodTOM := '1901'; // Agua Branca/PB';
      2500205: CodTOM := '1903'; // Aguiar/PB';
      2500304: CodTOM := '1905'; // Alagoa Grande/PB';
      2500403: CodTOM := '1907'; // Alagoa Nova/PB';
      2500502: CodTOM := '1909'; // Alagoinha/PB';
      2500536: CodTOM := '0440'; // Alcantil/PB';
      2500577: CodTOM := '0442'; // Algodao De Jandaira/PB';
      2500601: CodTOM := '1911'; // Alhandra/PB';
      2500700: CodTOM := '1913'; // Sao Joao Do Rio Do Peixe/PB';
      2500734: CodTOM := '0444'; // Amparo/PB';
      2500775: CodTOM := '0446'; // Aparecida/PB';
      2500809: CodTOM := '1915'; // Aracagi/PB';
      2500908: CodTOM := '1917'; // Arara/PB';
      2501005: CodTOM := '1919'; // Araruna/PB';
      2501104: CodTOM := '1921'; // Areia/PB';
      2501153: CodTOM := '0448'; // Areia De Baraunas/PB';
      2501203: CodTOM := '1923'; // Areial/PB';
      2501302: CodTOM := '1925'; // Aroeiras/PB';
      2501351: CodTOM := '0450'; // Assuncao/PB';
      2501401: CodTOM := '1929'; // Baia Da Traicao/PB';
      2501500: CodTOM := '1931'; // Bananeiras/PB';
      2501534: CodTOM := '0452'; // Barauna/PB';
      2501575: CodTOM := '0454'; // Barra De Santana/PB';
      2501609: CodTOM := '1933'; // Barra De Santa Rosa/PB';
      2501708: CodTOM := '1935'; // Barra De Sao Miguel/PB';
      2501807: CodTOM := '1937'; // Bayeux/PB';
      2501906: CodTOM := '1939'; // Belem/PB';
      2502003: CodTOM := '1941'; // Belem Do Brejo Do Cruz/PB';
      2502052: CodTOM := '0456'; // Bernardino Batista/PB';
      2502102: CodTOM := '1943'; // Boa Ventura/PB';
      2502151: CodTOM := '0458'; // Boa Vista/PB';
      2502201: CodTOM := '1945'; // Bom Jesus/PB';
      2502300: CodTOM := '1947'; // Bom Sucesso/PB';
      2502409: CodTOM := '1949'; // Bonito De Santa Fe/PB';
      2502508: CodTOM := '1951'; // Boqueirao/PB';
      2502607: CodTOM := '1953'; // Igaracy/PB';
      2502706: CodTOM := '1955'; // Borborema/PB';
      2502805: CodTOM := '1957'; // Brejo Do Cruz/PB';
      2502904: CodTOM := '1959'; // Brejo Dos Santos/PB';
      2503001: CodTOM := '1961'; // Caapora/PB';
      2503100: CodTOM := '1963'; // Cabaceiras/PB';
      2503209: CodTOM := '1965'; // Cabedelo/PB';
      2503308: CodTOM := '1967'; // Cachoeira Dos Indios/PB';
      2503407: CodTOM := '1969'; // Cacimba De Areia/PB';
      2503506: CodTOM := '1971'; // Cacimba De Dentro/PB';
      2503555: CodTOM := '0460'; // Cacimbas/PB';
      2503605: CodTOM := '1973'; // Caicara/PB';
      2503704: CodTOM := '1975'; // Cajazeiras/PB';
      2503753: CodTOM := '0462'; // Cajazeirinhas/PB';
      2503803: CodTOM := '1977'; // Caldas Brandao/PB';
      2503902: CodTOM := '1979'; // Camalau/PB';
      2504009: CodTOM := '1981'; // Campina Grande/PB';
      2504033: CodTOM := '0464'; // Capim/PB';
      2504074: CodTOM := '0466'; // Caraubas/PB';
      2504108: CodTOM := '1983'; // Carrapateira/PB';
      2504157: CodTOM := '0468'; // Casserengue/PB';
      2504207: CodTOM := '1985'; // Catingueira/PB';
      2504306: CodTOM := '1987'; // Catole Do Rocha/PB';
      2504355: CodTOM := '0470'; // Caturite/PB';
      2504405: CodTOM := '1989'; // Conceicao/PB';
      2504504: CodTOM := '1991'; // Condado/PB';
      2504603: CodTOM := '1993'; // Conde/PB';
      2504702: CodTOM := '1995'; // Congo/PB';
      2504801: CodTOM := '1997'; // Coremas/PB';
      2504850: CodTOM := '0472'; // Coxixola/PB';
      2504900: CodTOM := '1999'; // Cruz Do Espirito Santo/PB';
      2505006: CodTOM := '2001'; // Cubati/PB';
      2505105: CodTOM := '2003'; // Cuite/PB';
      2505204: CodTOM := '2005'; // Cuitegi/PB';
      2505238: CodTOM := '0474'; // Cuite De Mamanguape/PB';
      2505279: CodTOM := '0476'; // Curral De Cima/PB';
      2505303: CodTOM := '2007'; // Curral Velho/PB';
      2505352: CodTOM := '0478'; // Damiao/PB';
      2505402: CodTOM := '2009'; // Desterro/PB';
      2505501: CodTOM := '2011'; // Vista Serrana/PB';
      2505600: CodTOM := '2013'; // Diamante/PB';
      2505709: CodTOM := '2015'; // Dona Ines/PB';
      2505808: CodTOM := '2017'; // Duas Estradas/PB';
      2505907: CodTOM := '2019'; // Emas/PB';
      2506004: CodTOM := '2021'; // Esperanca/PB';
      2506103: CodTOM := '2023'; // Fagundes/PB';
      2506202: CodTOM := '2025'; // Frei Martinho/PB';
      2506251: CodTOM := '0480'; // Gado Bravo/PB';
      2506301: CodTOM := '2027'; // Guarabira/PB';
      2506400: CodTOM := '2029'; // Gurinhem/PB';
      2506509: CodTOM := '2031'; // Gurjao/PB';
      2506608: CodTOM := '2033'; // Ibiara/PB';
      2506707: CodTOM := '2035'; // Imaculada/PB';
      2506806: CodTOM := '2037'; // Inga/PB';
      2506905: CodTOM := '2039'; // Itabaiana/PB';
      2507002: CodTOM := '2041'; // Itaporanga/PB';
      2507101: CodTOM := '2043'; // Itapororoca/PB';
      2507200: CodTOM := '2045'; // Itatuba/PB';
      2507309: CodTOM := '2047'; // Jacarau/PB';
      2507408: CodTOM := '2049'; // Jerico/PB';
      2507507: CodTOM := '2051'; // Joao Pessoa/PB';
      2507606: CodTOM := '2053'; // Juarez Tavora/PB';
      2507705: CodTOM := '2055'; // Juazeirinho/PB';
      2507804: CodTOM := '2057'; // Junco Do Serido/PB';
      2507903: CodTOM := '2059'; // Juripiranga/PB';
      2508000: CodTOM := '2061'; // Juru/PB';
      2508109: CodTOM := '2063'; // Lagoa/PB';
      2508208: CodTOM := '2065'; // Lagoa De Dentro/PB';
      2508307: CodTOM := '2067'; // Lagoa Seca/PB';
      2508406: CodTOM := '2069'; // Lastro/PB';
      2508505: CodTOM := '2071'; // Livramento/PB';
      2508554: CodTOM := '0482'; // Logradouro/PB';
      2508604: CodTOM := '2073'; // Lucena/PB';
      2508703: CodTOM := '2075'; // Mae D Agua/PB';
      2508802: CodTOM := '2077'; // Malta/PB';
      2508901: CodTOM := '2079'; // Mamanguape/PB';
      2509008: CodTOM := '2081'; // Manaira/PB';
      2509057: CodTOM := '0484'; // Marcacao/PB';
      2509107: CodTOM := '2083'; // Mari/PB';
      2509156: CodTOM := '0486'; // Marizopolis/PB';
      2509206: CodTOM := '2085'; // Massaranduba/PB';
      2509305: CodTOM := '2087'; // Mataraca/PB';
      2509339: CodTOM := '0488'; // Matinhas/PB';
      2509370: CodTOM := '0490'; // Mato Grosso/PB';
      2509396: CodTOM := '0492'; // Matureia/PB';
      2509404: CodTOM := '2089'; // Mogeiro/PB';
      2509503: CodTOM := '2091'; // Montadas/PB';
      2509602: CodTOM := '2093'; // Monte Horebe/PB';
      2509701: CodTOM := '2095'; // Monteiro/PB';
      2509800: CodTOM := '2097'; // Mulungu/PB';
      2509909: CodTOM := '2099'; // Natuba/PB';
      2510006: CodTOM := '2101'; // Nazarezinho/PB';
      2510105: CodTOM := '2103'; // Nova Floresta/PB';
      2510204: CodTOM := '2105'; // Nova Olinda/PB';
      2510303: CodTOM := '2107'; // Nova Palmeira/PB';
      2510402: CodTOM := '2109'; // Olho D Agua/PB';
      2510501: CodTOM := '2111'; // Olivedos/PB';
      2510600: CodTOM := '2113'; // Ouro Velho/PB';
      2510659: CodTOM := '0494'; // Parari/PB';
      2510709: CodTOM := '2115'; // Passagem/PB';
      2510808: CodTOM := '2117'; // Patos/PB';
      2510907: CodTOM := '2119'; // Paulista/PB';
      2511004: CodTOM := '2121'; // Pedra Branca/PB';
      2511103: CodTOM := '2123'; // Pedra Lavrada/PB';
      2511202: CodTOM := '2125'; // Pedras De Fogo/PB';
      2511301: CodTOM := '2127'; // Pianco/PB';
      2511400: CodTOM := '2129'; // Picui/PB';
      2511509: CodTOM := '2131'; // Pilar/PB';
      2511608: CodTOM := '2133'; // Piloes/PB';
      2511707: CodTOM := '2135'; // Piloezinhos/PB';
      2511806: CodTOM := '2137'; // Pirpirituba/PB';
      2511905: CodTOM := '2139'; // Pitimbu/PB';
      2512002: CodTOM := '2141'; // Pocinhos/PB';
      2512036: CodTOM := '0496'; // Poco Dantas/PB';
      2512077: CodTOM := '0498'; // Poco De Jose De Moura/PB';
      2512101: CodTOM := '2143'; // Pombal/PB';
      2512200: CodTOM := '2145'; // Prata/PB';
      2512309: CodTOM := '2147'; // Princesa Isabel/PB';
      2512408: CodTOM := '2149'; // Puxinana/PB';
      2512507: CodTOM := '2151'; // Queimadas/PB';
      2512606: CodTOM := '2153'; // Quixaba/PB';
      2512705: CodTOM := '2155'; // Remigio/PB';
      2512721: CodTOM := '0500'; // Pedro Regis/PB';
      2512747: CodTOM := '0502'; // Riachao/PB';
      2512754: CodTOM := '0504'; // Riachao Do Bacamarte/PB';
      2512762: CodTOM := '0506'; // Riachao Do Poco/PB';
      2512788: CodTOM := '0508'; // Riacho De Santo Antonio/PB';
      2512804: CodTOM := '2157'; // Riacho Dos Cavalos/PB';
      2512903: CodTOM := '2159'; // Rio Tinto/PB';
      2513000: CodTOM := '2161'; // Salgadinho/PB';
      2513109: CodTOM := '2163'; // Salgado De Sao Felix/PB';
      2513158: CodTOM := '0510'; // Santa Cecilia/PB';
      2513208: CodTOM := '2165'; // Santa Cruz/PB';
      2513307: CodTOM := '2167'; // Santa Helena/PB';
      2513356: CodTOM := '0512'; // Santa Ines/PB';
      2513406: CodTOM := '2169'; // Santa Luzia/PB';
      2513505: CodTOM := '2171'; // Santana De Mangueira/PB';
      2513604: CodTOM := '2173'; // Santana Dos Garrotes/PB';
      2513653: CodTOM := '0514'; // Santarem/PB';
      2513703: CodTOM := '2175'; // Santa Rita/PB';
      2513802: CodTOM := '2177'; // Santa Teresinha/PB';
      2513851: CodTOM := '0516'; // Santo Andre/PB';
      2513901: CodTOM := '2179'; // Sao Bento/PB';
      2513927: CodTOM := '0518'; // Sao Bentinho/PB';
      2513943: CodTOM := '0520'; // Sao Domingos Do Cariri/PB';
      2513968: CodTOM := '0522'; // Sao Domingos/PB';
      2513984: CodTOM := '0524'; // Sao Francisco/PB';
      2514008: CodTOM := '2181'; // Sao Joao Do Cariri/PB';
      2514107: CodTOM := '2183'; // Sao Joao Do Tigre/PB';
      2514206: CodTOM := '2185'; // Sao Jose Da Lagoa Tapada/PB';
      2514305: CodTOM := '2187'; // Sao Jose De Caiana/PB';
      2514404: CodTOM := '2189'; // Sao Jose De Espinharas/PB';
      2514453: CodTOM := '0526'; // Sao Jose Dos Ramos/PB';
      2514503: CodTOM := '2191'; // Sao Jose De Piranhas/PB';
      2514552: CodTOM := '0528'; // Sao Jose De Princesa/PB';
      2514602: CodTOM := '2193'; // Sao Jose Do Bonfim/PB';
      2514651: CodTOM := '0530'; // Sao Jose Do Brejo Do Cruz/PB';
      2514701: CodTOM := '2195'; // Sao Jose Do Sabugi/PB';
      2514800: CodTOM := '2197'; // Sao Jose Dos Cordeiros/PB';
      2514909: CodTOM := '2199'; // Sao Mamede/PB';
      2515005: CodTOM := '2201'; // Sao Miguel De Taipu/PB';
      2515104: CodTOM := '2203'; // Sao Sebastiao De Lagoa De Roca/PB';
      2515203: CodTOM := '2205'; // Sao Sebastiao Do Umbuzeiro/PB';
      2515302: CodTOM := '2207'; // Sape/PB';
      2515401: CodTOM := '2209'; // Serido/PB';
      2515500: CodTOM := '2211'; // Serra Branca/PB';
      2515609: CodTOM := '2213'; // Serra Da Raiz/PB';
      2515708: CodTOM := '2215'; // Serra Grande/PB';
      2515807: CodTOM := '2217'; // Serra Redonda/PB';
      2515906: CodTOM := '2219'; // Serraria/PB';
      2515930: CodTOM := '0532'; // Sertaozinho/PB';
      2515971: CodTOM := '0534'; // Sobrado/PB';
      2516003: CodTOM := '2221'; // Solanea/PB';
      2516102: CodTOM := '2223'; // Soledade/PB';
      2516151: CodTOM := '0536'; // Sossego/PB';
      2516201: CodTOM := '2225'; // Sousa/PB';
      2516300: CodTOM := '2227'; // Sume/PB';
      2516409: CodTOM := '2229'; // Campo De Santana/PB';
      2516508: CodTOM := '2231'; // Taperoa/PB';
      2516607: CodTOM := '2233'; // Tavares/PB';
      2516706: CodTOM := '2235'; // Teixeira/PB';
      2516755: CodTOM := '0538'; // Tenorio/PB';
      2516805: CodTOM := '2237'; // Triunfo/PB';
      2516904: CodTOM := '2239'; // Uirauna/PB';
      2517001: CodTOM := '2241'; // Umbuzeiro/PB';
      2517100: CodTOM := '2243'; // Varzea/PB';
      2517209: CodTOM := '0540'; // Vieiropolis/PB';
      2517407: CodTOM := '0542'; // Zabele/PB';
   end;
 end;

 procedure P26;
 begin
   case ACodigo of
      2600054: CodTOM := '2631'; // Abreu E Lima/PE';
      2600104: CodTOM := '2301'; // Afogados Da Ingazeira/PE';
      2600203: CodTOM := '2303'; // Afranio/PE';
      2600302: CodTOM := '2305'; // Agrestina/PE';
      2600401: CodTOM := '2307'; // Agua Preta/PE';
      2600500: CodTOM := '2309'; // Aguas Belas/PE';
      2600609: CodTOM := '2311'; // Alagoinha/PE';
      2600708: CodTOM := '2313'; // Alianca/PE';
      2600807: CodTOM := '2315'; // Altinho/PE';
      2600906: CodTOM := '2317'; // Amaraji/PE';
      2601003: CodTOM := '2319'; // Angelim/PE';
      2601052: CodTOM := '0544'; // Aracoiaba/PE';
      2601102: CodTOM := '2321'; // Araripina/PE';
      2601201: CodTOM := '2323'; // Arcoverde/PE';
      2601300: CodTOM := '2325'; // Barra De Guabiraba/PE';
      2601409: CodTOM := '2327'; // Barreiros/PE';
      2601508: CodTOM := '2329'; // Belem De Maria/PE';
      2601607: CodTOM := '2331'; // Belem De Sao Francisco/PE';
      2601706: CodTOM := '2333'; // Belo Jardim/PE';
      2601805: CodTOM := '2335'; // Betania/PE';
      2601904: CodTOM := '2337'; // Bezerros/PE';
      2602001: CodTOM := '2339'; // Bodoco/PE';
      2602100: CodTOM := '2341'; // Bom Conselho/PE';
      2602209: CodTOM := '2343'; // Bom Jardim/PE';
      2602308: CodTOM := '2345'; // Bonito/PE';
      2602407: CodTOM := '2347'; // Brejao/PE';
      2602506: CodTOM := '2349'; // Brejinho/PE';
      2602605: CodTOM := '2351'; // Brejo Da Madre De Deus/PE';
      2602704: CodTOM := '2353'; // Buenos Aires/PE';
      2602803: CodTOM := '2355'; // Buique/PE';
      2602902: CodTOM := '2357'; // Cabo De Santo Agostinho/PE';
      2603009: CodTOM := '2359'; // Cabrobo/PE';
      2603108: CodTOM := '2361'; // Cachoeirinha/PE';
      2603207: CodTOM := '2363'; // Caetes/PE';
      2603306: CodTOM := '2365'; // Calcado/PE';
      2603405: CodTOM := '2367'; // Calumbi/PE';
      2603454: CodTOM := '2629'; // Camaragibe/PE';
      2603504: CodTOM := '2369'; // Camocim De Sao Felix/PE';
      2603603: CodTOM := '2371'; // Camutanga/PE';
      2603702: CodTOM := '2373'; // Canhotinho/PE';
      2603801: CodTOM := '2375'; // Capoeiras/PE';
      2603900: CodTOM := '2377'; // Carnaiba/PE';
      2603926: CodTOM := '2635'; // Carnaubeira Da Penha/PE';
      2604007: CodTOM := '2379'; // Carpina/PE';
      2604106: CodTOM := '2381'; // Caruaru/PE';
      2604155: CodTOM := '0546'; // Casinhas/PE';
      2604205: CodTOM := '2383'; // Catende/PE';
      2604304: CodTOM := '2385'; // Cedro/PE';
      2604403: CodTOM := '2387'; // Cha De Alegria/PE';
      2604502: CodTOM := '2389'; // Cha Grande/PE';
      2604601: CodTOM := '2391'; // Condado/PE';
      2604700: CodTOM := '2393'; // Correntes/PE';
      2604809: CodTOM := '2395'; // Cortes/PE';
      2604908: CodTOM := '2397'; // Cumaru/PE';
      2605004: CodTOM := '2399'; // Cupira/PE';
      2605103: CodTOM := '2401'; // Custodia/PE';
      2605152: CodTOM := '2299'; // Dormentes/PE';
      2605202: CodTOM := '2403'; // Escada/PE';
      2605301: CodTOM := '2405'; // Exu/PE';
      2605400: CodTOM := '2407'; // Feira Nova/PE';
      2605459: CodTOM := '3001'; // Fernando De Noronha/PE';
      2605509: CodTOM := '2409'; // Ferreiros/PE';
      2605608: CodTOM := '2411'; // Flores/PE';
      2605707: CodTOM := '2413'; // Floresta/PE';
      2605806: CodTOM := '2415'; // Frei Miguelinho/PE';
      2605905: CodTOM := '2417'; // Gameleira/PE';
      2606002: CodTOM := '2419'; // Garanhuns/PE';
      2606101: CodTOM := '2421'; // Gloria Do Goita/PE';
      2606200: CodTOM := '2423'; // Goiana/PE';
      2606309: CodTOM := '2425'; // Granito/PE';
      2606408: CodTOM := '2427'; // Gravata/PE';
      2606507: CodTOM := '2429'; // Iati/PE';
      2606606: CodTOM := '2431'; // Ibimirim/PE';
      2606705: CodTOM := '2433'; // Ibirajuba/PE';
      2606804: CodTOM := '2435'; // Igarassu/PE';
      2606903: CodTOM := '2437'; // Iguaraci/PE';
      2607000: CodTOM := '2439'; // Inaja/PE';
      2607109: CodTOM := '2441'; // Ingazeira/PE';
      2607208: CodTOM := '2443'; // Ipojuca/PE';
      2607307: CodTOM := '2445'; // Ipubi/PE';
      2607406: CodTOM := '2447'; // Itacuruba/PE';
      2607505: CodTOM := '2449'; // Itaiba/PE';
      2607604: CodTOM := '2451'; // Ilha De Itamaraca/PE';
      2607653: CodTOM := '2597'; // Itambe/PE';
      2607703: CodTOM := '2453'; // Itapetim/PE';
      2607752: CodTOM := '2633'; // Itapissuma/PE';
      2607802: CodTOM := '2455'; // Itaquitinga/PE';
      2607901: CodTOM := '2457'; // Jaboatao Dos Guararapes/PE';
      2607950: CodTOM := '0548'; // Jaqueira/PE';
      2608008: CodTOM := '2459'; // Jatauba/PE';
      2608057: CodTOM := '0550'; // Jatoba/PE';
      2608107: CodTOM := '2461'; // Joao Alfredo/PE';
      2608206: CodTOM := '2463'; // Joaquim Nabuco/PE';
      2608255: CodTOM := '2295'; // Jucati/PE';
      2608305: CodTOM := '2465'; // Jupi/PE';
      2608404: CodTOM := '2467'; // Jurema/PE';
      2608453: CodTOM := '2289'; // Lagoa Do Carro/PE';
      2608503: CodTOM := '2469'; // Lagoa Do Itaenga/PE';
      2608602: CodTOM := '2471'; // Lagoa Do Ouro/PE';
      2608701: CodTOM := '2473'; // Lagoa Dos Gatos/PE';
      2608750: CodTOM := '0552'; // Lagoa Grande/PE';
      2608800: CodTOM := '2475'; // Lajedo/PE';
      2608909: CodTOM := '2477'; // Limoeiro/PE';
      2609006: CodTOM := '2479'; // Macaparana/PE';
      2609105: CodTOM := '2481'; // Machados/PE';
      2609154: CodTOM := '0554'; // Manari/PE';
      2609204: CodTOM := '2483'; // Maraial/PE';
      2609303: CodTOM := '2485'; // Mirandiba/PE';
      2609402: CodTOM := '2487'; // Moreno/PE';
      2609501: CodTOM := '2489'; // Nazare Da Mata/PE';
      2609600: CodTOM := '2491'; // Olinda/PE';
      2609709: CodTOM := '2493'; // Orobo/PE';
      2609808: CodTOM := '2495'; // Oroco/PE';
      2609907: CodTOM := '2497'; // Ouricuri/PE';
      2610004: CodTOM := '2499'; // Palmares/PE';
      2610103: CodTOM := '2501'; // Palmeirina/PE';
      2610202: CodTOM := '2503'; // Panelas/PE';
      2610301: CodTOM := '2505'; // Paranatama/PE';
      2610400: CodTOM := '2507'; // Parnamirim/PE';
      2610509: CodTOM := '2509'; // Passira/PE';
      2610608: CodTOM := '2511'; // Paudalho/PE';
      2610707: CodTOM := '2513'; // Paulista/PE';
      2610806: CodTOM := '2515'; // Pedra/PE';
      2610905: CodTOM := '2517'; // Pesqueira/PE';
      2611002: CodTOM := '2519'; // Petrolandia/PE';
      2611101: CodTOM := '2521'; // Petrolina/PE';
      2611200: CodTOM := '2523'; // Pocao/PE';
      2611309: CodTOM := '2525'; // Pombos/PE';
      2611408: CodTOM := '2527'; // Primavera/PE';
      2611507: CodTOM := '2529'; // Quipapa/PE';
      2611533: CodTOM := '2637'; // Quixaba/PE';
      2611606: CodTOM := '2531'; // Recife/PE';
      2611705: CodTOM := '2533'; // Riacho Das Almas/PE';
      2611804: CodTOM := '2535'; // Ribeirao/PE';
      2611903: CodTOM := '2537'; // Rio Formoso/PE';
      2612000: CodTOM := '2539'; // Saire/PE';
      2612109: CodTOM := '2541'; // Salgadinho/PE';
      2612208: CodTOM := '2543'; // Salgueiro/PE';
      2612307: CodTOM := '2545'; // Saloa/PE';
      2612406: CodTOM := '2547'; // Sanharo/PE';
      2612455: CodTOM := '2297'; // Santa Cruz/PE';
      2612471: CodTOM := '2639'; // Santa Cruz Da Baixa Verde/PE';
      2612505: CodTOM := '2549'; // Santa Cruz Do Capibaribe/PE';
      2612554: CodTOM := '0556'; // Santa Filomena/PE';
      2612604: CodTOM := '2551'; // Santa Maria Da Boa Vista/PE';
      2612703: CodTOM := '2553'; // Santa Maria Do Cambuca/PE';
      2612802: CodTOM := '2555'; // Santa Terezinha/PE';
      2612901: CodTOM := '2557'; // Sao Benedito Do Sul/PE';
      2613008: CodTOM := '2559'; // Sao Bento Do Una/PE';
      2613107: CodTOM := '2561'; // Sao Caitano/PE';
      2613206: CodTOM := '2563'; // Sao Joao/PE';
      2613305: CodTOM := '2565'; // Sao Joaquim Do Monte/PE';
      2613404: CodTOM := '2567'; // Sao Jose Da Coroa Grande/PE';
      2613503: CodTOM := '2569'; // Sao Jose Do Belmonte/PE';
      2613602: CodTOM := '2571'; // Sao Jose Do Egito/PE';
      2613701: CodTOM := '2573'; // Sao Lourenco Da Mata/PE';
      2613800: CodTOM := '2575'; // Sao Vicente Ferrer/PE';
      2613909: CodTOM := '2577'; // Serra Talhada/PE';
      2614006: CodTOM := '2579'; // Serrita/PE';
      2614105: CodTOM := '2581'; // Sertania/PE';
      2614204: CodTOM := '2583'; // Sirinhaem/PE';
      2614303: CodTOM := '2585'; // Moreilandia/PE';
      2614402: CodTOM := '2587'; // Solidao/PE';
      2614501: CodTOM := '2589'; // Surubim/PE';
      2614600: CodTOM := '2591'; // Tabira/PE';
      2614709: CodTOM := '2593'; // Tacaimbo/PE';
      2614808: CodTOM := '2595'; // Tacaratu/PE';
      2614857: CodTOM := '0558'; // Tamandare/PE';
      2615003: CodTOM := '2599'; // Taquaritinga Do Norte/PE';
      2615102: CodTOM := '2601'; // Terezinha/PE';
      2615201: CodTOM := '2603'; // Terra Nova/PE';
      2615300: CodTOM := '2605'; // Timbauba/PE';
      2615409: CodTOM := '2607'; // Toritama/PE';
      2615508: CodTOM := '2609'; // Tracunhaem/PE';
      2615607: CodTOM := '2611'; // Trindade/PE';
      2615706: CodTOM := '2613'; // Triunfo/PE';
      2615805: CodTOM := '2615'; // Tupanatinga/PE';
      2615904: CodTOM := '2617'; // Tuparetama/PE';
      2616001: CodTOM := '2619'; // Venturosa/PE';
      2616100: CodTOM := '2621'; // Verdejante/PE';
      2616183: CodTOM := '2291'; // Vertente Do Lerio/PE';
      2616209: CodTOM := '2623'; // Vertentes/PE';
      2616308: CodTOM := '2625'; // Vicencia/PE';
      2616407: CodTOM := '2627'; // Vitoria De Santo Antao/PE';
      2616506: CodTOM := '2293'; // Xexeu/PE';
   end;
 end;

 procedure P27;
 begin
   case ACodigo of
      2700102: CodTOM := '2701'; // Agua Branca/AL';
      2700201: CodTOM := '2703'; // Anadia/AL';
      2700300: CodTOM := '2705'; // Arapiraca/AL';
      2700409: CodTOM := '2707'; // Atalaia/AL';
      2700508: CodTOM := '2709'; // Barra De Santo Antonio/AL';
      2700607: CodTOM := '2711'; // Barra De Sao Miguel/AL';
      2700706: CodTOM := '2713'; // Batalha/AL';
      2700805: CodTOM := '2715'; // Belem/AL';
      2700904: CodTOM := '2717'; // Belo Monte/AL';
      2701001: CodTOM := '2719'; // Boca Da Mata/AL';
      2701100: CodTOM := '2721'; // Branquinha/AL';
      2701209: CodTOM := '2723'; // Cacimbinhas/AL';
      2701308: CodTOM := '2725'; // Cajueiro/AL';
      2701357: CodTOM := '0560'; // Campestre/AL';
      2701407: CodTOM := '2727'; // Campo Alegre/AL';
      2701506: CodTOM := '2729'; // Campo Grande/AL';
      2701605: CodTOM := '2731'; // Canapi/AL';
      2701704: CodTOM := '2733'; // Capela/AL';
      2701803: CodTOM := '2735'; // Carneiros/AL';
      2701902: CodTOM := '2737'; // Cha Preta/AL';
      2702009: CodTOM := '2739'; // Coite Do Noia/AL';
      2702108: CodTOM := '2741'; // Colonia Leopoldina/AL';
      2702207: CodTOM := '2743'; // Coqueiro Seco/AL';
      2702306: CodTOM := '2745'; // Coruripe/AL';
      2702355: CodTOM := '2889'; // Craibas/AL';
      2702405: CodTOM := '2747'; // Delmiro Gouveia/AL';
      2702504: CodTOM := '2749'; // Dois Riachos/AL';
      2702553: CodTOM := '2643'; // Estrela De Alagoas/AL';
      2702603: CodTOM := '2751'; // Feira Grande/AL';
      2702702: CodTOM := '2753'; // Feliz Deserto/AL';
      2702801: CodTOM := '2755'; // Flexeiras/AL';
      2702900: CodTOM := '2757'; // Girau Do Ponciano/AL';
      2703007: CodTOM := '2759'; // Ibateguara/AL';
      2703106: CodTOM := '2761'; // Igaci/AL';
      2703205: CodTOM := '2763'; // Igreja Nova/AL';
      2703304: CodTOM := '2765'; // Inhapi/AL';
      2703403: CodTOM := '2767'; // Jacare Dos Homens/AL';
      2703502: CodTOM := '2769'; // Jacuipe/AL';
      2703601: CodTOM := '2771'; // Japaratinga/AL';
      2703700: CodTOM := '2773'; // Jaramataia/AL';
      2703759: CodTOM := '0562'; // Jequia Da Praia/AL';
      2703809: CodTOM := '2775'; // Joaquim Gomes/AL';
      2703908: CodTOM := '2777'; // Jundia/AL';
      2704005: CodTOM := '2779'; // Junqueiro/AL';
      2704104: CodTOM := '2781'; // Lagoa Da Canoa/AL';
      2704203: CodTOM := '2783'; // Limoeiro De Anadia/AL';
      2704302: CodTOM := '2785'; // Maceio/AL';
      2704401: CodTOM := '2787'; // Major Isidoro/AL';
      2704500: CodTOM := '2789'; // Maragogi/AL';
      2704609: CodTOM := '2791'; // Maravilha/AL';
      2704708: CodTOM := '2793'; // Marechal Deodoro/AL';
      2704807: CodTOM := '2795'; // Maribondo/AL';
      2704906: CodTOM := '2797'; // Mar Vermelho/AL';
      2705002: CodTOM := '2799'; // Mata Grande/AL';
      2705101: CodTOM := '2801'; // Matriz De Camaragibe/AL';
      2705200: CodTOM := '2803'; // Messias/AL';
      2705309: CodTOM := '2805'; // Minador Do Negrao/AL';
      2705408: CodTOM := '2807'; // Monteiropolis/AL';
      2705507: CodTOM := '2809'; // Murici/AL';
      2705606: CodTOM := '2811'; // Novo Lino/AL';
      2705705: CodTOM := '2813'; // Olho D Agua Das Flores/AL';
      2705804: CodTOM := '2815'; // Olho D Agua Do Casado/AL';
      2705903: CodTOM := '2817'; // Olho D Agua Grande/AL';
      2706000: CodTOM := '2819'; // Olivenca/AL';
      2706109: CodTOM := '2821'; // Ouro Branco/AL';
      2706208: CodTOM := '2823'; // Palestina/AL';
      2706307: CodTOM := '2825'; // Palmeira Dos Indios/AL';
      2706406: CodTOM := '2827'; // Pao De Acucar/AL';
      2706422: CodTOM := '2645'; // Pariconha/AL';
      2706448: CodTOM := '2641'; // Paripueira/AL';
      2706505: CodTOM := '2829'; // Passo De Camaragibe/AL';
      2706604: CodTOM := '2831'; // Paulo Jacinto/AL';
      2706703: CodTOM := '2833'; // Penedo/AL';
      2706802: CodTOM := '2835'; // Piacabucu/AL';
      2706901: CodTOM := '2837'; // Pilar/AL';
      2707008: CodTOM := '2839'; // Pindoba/AL';
      2707107: CodTOM := '2841'; // Piranhas/AL';
      2707206: CodTOM := '2843'; // Poco Das Trincheiras/AL';
      2707305: CodTOM := '2845'; // Porto Calvo/AL';
      2707404: CodTOM := '2847'; // Porto De Pedras/AL';
      2707503: CodTOM := '2849'; // Porto Real Do Colegio/AL';
      2707602: CodTOM := '2851'; // Quebrangulo/AL';
      2707701: CodTOM := '2853'; // Rio Largo/AL';
      2707800: CodTOM := '2855'; // Roteiro/AL';
      2707909: CodTOM := '2857'; // Santa Luzia Do Norte/AL';
      2708006: CodTOM := '2859'; // Santana Do Ipanema/AL';
      2708105: CodTOM := '2861'; // Santana Do Mundau/AL';
      2708204: CodTOM := '2863'; // Sao Bras/AL';
      2708303: CodTOM := '2865'; // Sao Jose Da Laje/AL';
      2708402: CodTOM := '2867'; // Sao Jose Da Tapera/AL';
      2708501: CodTOM := '2869'; // Sao Luis Do Quitunde/AL';
      2708600: CodTOM := '2871'; // Sao Miguel Dos Campos/AL';
      2708709: CodTOM := '2873'; // Sao Miguel Dos Milagres/AL';
      2708808: CodTOM := '2875'; // Sao Sebastiao/AL';
      2708907: CodTOM := '2877'; // Satuba/AL';
      2708956: CodTOM := '2891'; // Senador Rui Palmeira/AL';
      2709004: CodTOM := '2879'; // Tanque D Arca/AL';
      2709103: CodTOM := '2881'; // Taquarana/AL';
      2709152: CodTOM := '0971'; // Teotonio Vilela/AL';
      2709202: CodTOM := '2883'; // Traipu/AL';
      2709301: CodTOM := '2885'; // Uniao Dos Palmares/AL';
      2709400: CodTOM := '2887'; // Vicosa/AL';
   end;
 end;

 procedure P28;
 begin
   case ACodigo of
      2800100: CodTOM := '3101'; // Amparo De Sao Francisco/SE';
      2800209: CodTOM := '3103'; // Aquidaba/SE';
      2800308: CodTOM := '3105'; // Aracaju/SE';
      2800407: CodTOM := '3107'; // Araua/SE';
      2800506: CodTOM := '3109'; // Areia Branca/SE';
      2800605: CodTOM := '3111'; // Barra Dos Coqueiros/SE';
      2800670: CodTOM := '3115'; // Boquim/SE';
      2800704: CodTOM := '3113'; // Brejo Grande/SE';
      2801009: CodTOM := '3119'; // Campo Do Brito/SE';
      2801108: CodTOM := '3121'; // Canhoba/SE';
      2801207: CodTOM := '3123'; // Caninde De Sao Francisco/SE';
      2801306: CodTOM := '3125'; // Capela/SE';
      2801405: CodTOM := '3127'; // Carira/SE';
      2801504: CodTOM := '3129'; // Carmopolis/SE';
      2801603: CodTOM := '3131'; // Cedro De Sao Joao/SE';
      2801702: CodTOM := '3133'; // Cristinapolis/SE';
      2801900: CodTOM := '3137'; // Cumbe/SE';
      2802007: CodTOM := '3139'; // Divina Pastora/SE';
      2802106: CodTOM := '3141'; // Estancia/SE';
      2802205: CodTOM := '3143'; // Feira Nova/SE';
      2802304: CodTOM := '3145'; // Frei Paulo/SE';
      2802403: CodTOM := '3149'; // Gararu/SE';
      2802502: CodTOM := '3147'; // General Maynard/SE';
      2802601: CodTOM := '3151'; // Gracho Cardoso/SE';
      2802700: CodTOM := '3153'; // Ilha Das Flores/SE';
      2802809: CodTOM := '3155'; // Indiaroba/SE';
      2802908: CodTOM := '3157'; // Itabaiana/SE';
      2803005: CodTOM := '3159'; // Itabaianinha/SE';
      2803104: CodTOM := '3161'; // Itabi/SE';
      2803203: CodTOM := '3163'; // Itaporanga D Ajuda/SE';
      2803302: CodTOM := '3165'; // Japaratuba/SE';
      2803401: CodTOM := '3167'; // Japoata/SE';
      2803500: CodTOM := '3169'; // Lagarto/SE';
      2803609: CodTOM := '3171'; // Laranjeiras/SE';
      2803708: CodTOM := '3173'; // Macambira/SE';
      2803807: CodTOM := '3175'; // Malhada Dos Bois/SE';
      2803906: CodTOM := '3177'; // Malhador/SE';
      2804003: CodTOM := '3179'; // Maruim/SE';
      2804102: CodTOM := '3181'; // Moita Bonita/SE';
      2804201: CodTOM := '3183'; // Monte Alegre De Sergipe/SE';
      2804300: CodTOM := '3185'; // Muribeca/SE';
      2804409: CodTOM := '3187'; // Neopolis/SE';
      2804458: CodTOM := '3135'; // Nossa Senhora Aparecida/SE';
      2804508: CodTOM := '3189'; // Nossa Senhora Da Gloria/SE';
      2804607: CodTOM := '3191'; // Nossa Senhora Das Dores/SE';
      2804706: CodTOM := '3193'; // Nossa Senhora De Lourdes/SE';
      2804805: CodTOM := '3195'; // Nossa Senhora Do Socorro/SE';
      2804904: CodTOM := '3197'; // Pacatuba/SE';
      2805000: CodTOM := '3199'; // Pedra Mole/SE';
      2805109: CodTOM := '3201'; // Pedrinhas/SE';
      2805208: CodTOM := '3203'; // Pinhao/SE';
      2805307: CodTOM := '3205'; // Pirambu/SE';
      2805406: CodTOM := '3207'; // Poco Redondo/SE';
      2805505: CodTOM := '3209'; // Poco Verde/SE';
      2805604: CodTOM := '3211'; // Porto Da Folha/SE';
      2805703: CodTOM := '3213'; // Propria/SE';
      2805802: CodTOM := '3215'; // Riachao Do Dantas/SE';
      2805901: CodTOM := '3217'; // Riachuelo/SE';
      2806008: CodTOM := '3219'; // Ribeiropolis/SE';
      2806107: CodTOM := '3221'; // Rosario Do Catete/SE';
      2806206: CodTOM := '3223'; // Salgado/SE';
      2806305: CodTOM := '3225'; // Santa Luzia Do Itanhy/SE';
      2806404: CodTOM := '2647'; // Santana Do Sao Francisco/SE';
      2806503: CodTOM := '3229'; // Santa Rosa De Lima/SE';
      2806602: CodTOM := '3231'; // Santo Amaro Das Brotas/SE';
      2806701: CodTOM := '3233'; // Sao Cristovao/SE';
      2806800: CodTOM := '3235'; // Sao Domingos/SE';
      2806909: CodTOM := '3237'; // Sao Francisco/SE';
      2807006: CodTOM := '3239'; // Sao Miguel Do Aleixo/SE';
      2807105: CodTOM := '3241'; // Simao Dias/SE';
      2807204: CodTOM := '3243'; // Siriri/SE';
      2807303: CodTOM := '3245'; // Telha/SE';
      2807402: CodTOM := '3247'; // Tobias Barreto/SE';
      2807501: CodTOM := '3249'; // Tomar Do Geru/SE';
      2807600: CodTOM := '3251'; // Umbauba/SE';
   end;
 end;

 procedure P29;
 begin
   case ACodigo of
      2900108: CodTOM := '3301'; // Abaira/BA';
      2900207: CodTOM := '3303'; // Abare/BA';
      2900306: CodTOM := '3305'; // Acajutiba/BA';
      2900355: CodTOM := '3253'; // Adustina/BA';
      2900405: CodTOM := '3307'; // Agua Fria/BA';
      2900504: CodTOM := '3309'; // Erico Cardoso/BA';
      2900603: CodTOM := '3311'; // Aiquara/BA';
      2900702: CodTOM := '3313'; // Alagoinhas/BA';
      2900801: CodTOM := '3315'; // Alcobaca/BA';
      2900900: CodTOM := '3317'; // Almadina/BA';
      2901007: CodTOM := '3319'; // Amargosa/BA';
      2901106: CodTOM := '3321'; // Amelia Rodrigues/BA';
      2901155: CodTOM := '3071'; // America Dourada/BA';
      2901205: CodTOM := '3323'; // Anage/BA';
      2901304: CodTOM := '3325'; // Andarai/BA';
      2901353: CodTOM := '3255'; // Andorinha/BA';
      2901403: CodTOM := '3327'; // Angical/BA';
      2901502: CodTOM := '3329'; // Anguera/BA';
      2901601: CodTOM := '3331'; // Antas/BA';
      2901700: CodTOM := '3333'; // Antonio Cardoso/BA';
      2901809: CodTOM := '3335'; // Antonio Goncalves/BA';
      2901908: CodTOM := '3337'; // Apora/BA';
      2901957: CodTOM := '3257'; // Apuarema/BA';
      2902005: CodTOM := '3339'; // Aracatu/BA';
      2902054: CodTOM := '3259'; // Aracas/BA';
      2902104: CodTOM := '3341'; // Araci/BA';
      2902203: CodTOM := '3343'; // Aramari/BA';
      2902252: CodTOM := '3073'; // Arataca/BA';
      2902302: CodTOM := '3345'; // Aratuipe/BA';
      2902401: CodTOM := '3347'; // Aurelino Leal/BA';
      2902500: CodTOM := '3349'; // Baianopolis/BA';
      2902609: CodTOM := '3351'; // Baixa Grande/BA';
      2902658: CodTOM := '3261'; // Banzae/BA';
      2902708: CodTOM := '3353'; // Barra/BA';
      2902807: CodTOM := '3355'; // Barra Da Estiva/BA';
      2902906: CodTOM := '3357'; // Barra Do Choca/BA';
      2903003: CodTOM := '3359'; // Barra Do Mendes/BA';
      2903102: CodTOM := '3361'; // Barra Do Rocha/BA';
      2903201: CodTOM := '3363'; // Barreiras/BA';
      2903235: CodTOM := '3075'; // Barro Alto/BA';
      2903276: CodTOM := '3077'; // Barrocas/BA';
      2903300: CodTOM := '3365'; // Barro Preto/BA';
      2903409: CodTOM := '3367'; // Belmonte/BA';
      2903508: CodTOM := '3369'; // Belo Campo/BA';
      2903607: CodTOM := '3371'; // Biritinga/BA';
      2903706: CodTOM := '3373'; // Boa Nova/BA';
      2903805: CodTOM := '3375'; // Boa Vista Do Tupim/BA';
      2903904: CodTOM := '3377'; // Bom Jesus Da Lapa/BA';
      2903953: CodTOM := '3263'; // Bom Jesus Da Serra/BA';
      2904001: CodTOM := '3379'; // Boninal/BA';
      2904050: CodTOM := '3265'; // Bonito/BA';
      2904100: CodTOM := '3381'; // Boquira/BA';
      2904209: CodTOM := '3383'; // Botupora/BA';
      2904308: CodTOM := '3385'; // Brejoes/BA';
      2904407: CodTOM := '3387'; // Brejolandia/BA';
      2904506: CodTOM := '3389'; // Brotas De Macaubas/BA';
      2904605: CodTOM := '3391'; // Brumado/BA';
      2904704: CodTOM := '3393'; // Buerarema/BA';
      2904753: CodTOM := '3079'; // Buritirama/BA';
      2904803: CodTOM := '3395'; // Caatiba/BA';
      2904852: CodTOM := '3267'; // Cabaceiras Do Paraguacu/BA';
      2904902: CodTOM := '3397'; // Cachoeira/BA';
      2905008: CodTOM := '3399'; // Cacule/BA';
      2905107: CodTOM := '3401'; // Caem/BA';
      2905156: CodTOM := '3269'; // Caetanos/BA';
      2905206: CodTOM := '3403'; // Caetite/BA';
      2905305: CodTOM := '3405'; // Cafarnaum/BA';
      2905404: CodTOM := '3407'; // Cairu/BA';
      2905503: CodTOM := '3409'; // Caldeirao Grande/BA';
      2905602: CodTOM := '3411'; // Camacan/BA';
      2905701: CodTOM := '3413'; // Camacari/BA';
      2905800: CodTOM := '3415'; // Camamu/BA';
      2905909: CodTOM := '3417'; // Campo Alegre De Lourdes/BA';
      2906006: CodTOM := '3419'; // Campo Formoso/BA';
      2906105: CodTOM := '3421'; // Canapolis/BA';
      2906204: CodTOM := '3423'; // Canarana/BA';
      2906303: CodTOM := '3425'; // Canavieiras/BA';
      2906402: CodTOM := '3427'; // Candeal/BA';
      2906501: CodTOM := '3429'; // Candeias/BA';
      2906600: CodTOM := '3431'; // Candiba/BA';
      2906709: CodTOM := '3433'; // Candido Sales/BA';
      2906808: CodTOM := '3435'; // Cansancao/BA';
      2906824: CodTOM := '3085'; // Canudos/BA';
      2906857: CodTOM := '3081'; // Capela Do Alto Alegre/BA';
      2906873: CodTOM := '3083'; // Capim Grosso/BA';
      2906899: CodTOM := '3271'; // Caraibas/BA';
      2906907: CodTOM := '3437'; // Caravelas/BA';
      2907004: CodTOM := '3439'; // Cardeal Da Silva/BA';
      2907103: CodTOM := '3441'; // Carinhanha/BA';
      2907202: CodTOM := '3443'; // Casa Nova/BA';
      2907301: CodTOM := '3445'; // Castro Alves/BA';
      2907400: CodTOM := '3447'; // Catolandia/BA';
      2907509: CodTOM := '3449'; // Catu/BA';
      2907558: CodTOM := '3273'; // Caturama/BA';
      2907608: CodTOM := '3451'; // Central/BA';
      2907707: CodTOM := '3453'; // Chorrocho/BA';
      2907806: CodTOM := '3455'; // Cicero Dantas/BA';
      2907905: CodTOM := '3457'; // Cipo/BA';
      2908002: CodTOM := '3459'; // Coaraci/BA';
      2908101: CodTOM := '3461'; // Cocos/BA';
      2908200: CodTOM := '3463'; // Conceicao Da Feira/BA';
      2908309: CodTOM := '3465'; // Conceicao Do Almeida/BA';
      2908408: CodTOM := '3467'; // Conceicao Do Coite/BA';
      2908507: CodTOM := '3469'; // Conceicao Do Jacuipe/BA';
      2908606: CodTOM := '3471'; // Conde/BA';
      2908705: CodTOM := '3473'; // Condeuba/BA';
      2908804: CodTOM := '3475'; // Contendas Do Sincora/BA';
      2908903: CodTOM := '3477'; // Coracao De Maria/BA';
      2909000: CodTOM := '3479'; // Cordeiros/BA';
      2909109: CodTOM := '3481'; // Coribe/BA';
      2909208: CodTOM := '3483'; // Coronel Joao Sa/BA';
      2909307: CodTOM := '3485'; // Correntina/BA';
      2909406: CodTOM := '3487'; // Cotegipe/BA';
      2909505: CodTOM := '3489'; // Cravolandia/BA';
      2909604: CodTOM := '3491'; // Crisopolis/BA';
      2909703: CodTOM := '3493'; // Cristopolis/BA';
      2909802: CodTOM := '3495'; // Cruz Das Almas/BA';
      2909901: CodTOM := '3497'; // Curaca/BA';
      2910008: CodTOM := '3499'; // Dario Meira/BA';
      2910057: CodTOM := '3087'; // Dias D Avila/BA';
      2910107: CodTOM := '3501'; // Dom Basilio/BA';
      2910206: CodTOM := '3503'; // Dom Macedo Costa/BA';
      2910305: CodTOM := '3505'; // Elisio Medrado/BA';
      2910404: CodTOM := '3507'; // Encruzilhada/BA';
      2910503: CodTOM := '3509'; // Entre Rios/BA';
      2910602: CodTOM := '3511'; // Esplanada/BA';
      2910701: CodTOM := '3513'; // Euclides Da Cunha/BA';
      2910727: CodTOM := '3117'; // Eunapolis/BA';
      2910750: CodTOM := '3089'; // Fatima/BA';
      2910776: CodTOM := '3275'; // Feira Da Mata/BA';
      2910800: CodTOM := '3515'; // Feira De Santana/BA';
      2910859: CodTOM := '3091'; // Filadelfia/BA';
      2910909: CodTOM := '3517'; // Firmino Alves/BA';
      2911006: CodTOM := '3519'; // Floresta Azul/BA';
      2911105: CodTOM := '3521'; // Formosa Do Rio Preto/BA';
      2911204: CodTOM := '3523'; // Gandu/BA';
      2911253: CodTOM := '3093'; // Gaviao/BA';
      2911303: CodTOM := '3525'; // Gentio Do Ouro/BA';
      2911402: CodTOM := '3527'; // Gloria/BA';
      2911501: CodTOM := '3529'; // Gongogi/BA';
      2911600: CodTOM := '3531'; // Governador Mangabeira/BA';
      2911659: CodTOM := '3095'; // Guajeru/BA';
      2911709: CodTOM := '3533'; // Guanambi/BA';
      2911808: CodTOM := '3535'; // Guaratinga/BA';
      2911857: CodTOM := '3097'; // Heliopolis/BA';
      2911907: CodTOM := '3537'; // Iacu/BA';
      2912004: CodTOM := '3539'; // Ibiassuce/BA';
      2912103: CodTOM := '3541'; // Ibicarai/BA';
      2912202: CodTOM := '3543'; // Ibicoara/BA';
      2912301: CodTOM := '3545'; // Ibicui/BA';
      2912400: CodTOM := '3547'; // Ibipeba/BA';
      2912509: CodTOM := '3551'; // Ibipitanga/BA';
      2912608: CodTOM := '3553'; // Ibiquera/BA';
      2912707: CodTOM := '3555'; // Ibirapitanga/BA';
      2912806: CodTOM := '3557'; // Ibirapua/BA';
      2912905: CodTOM := '3559'; // Ibirataia/BA';
      2913002: CodTOM := '3561'; // Ibitiara/BA';
      2913101: CodTOM := '3563'; // Ibitita/BA';
      2913200: CodTOM := '3565'; // Ibotirama/BA';
      2913309: CodTOM := '3567'; // Ichu/BA';
      2913408: CodTOM := '3569'; // Igapora/BA';
      2913457: CodTOM := '3277'; // Igrapiuna/BA';
      2913507: CodTOM := '3571'; // Iguai/BA';
      2913606: CodTOM := '3573'; // Ilheus/BA';
      2913705: CodTOM := '3575'; // Inhambupe/BA';
      2913804: CodTOM := '3577'; // Ipecaeta/BA';
      2913903: CodTOM := '3579'; // Ipiau/BA';
      2914000: CodTOM := '3581'; // Ipira/BA';
      2914109: CodTOM := '3583'; // Ipupiara/BA';
      2914208: CodTOM := '3585'; // Irajuba/BA';
      2914307: CodTOM := '3587'; // Iramaia/BA';
      2914406: CodTOM := '3589'; // Iraquara/BA';
      2914505: CodTOM := '3591'; // Irara/BA';
      2914604: CodTOM := '3593'; // Irece/BA';
      2914653: CodTOM := '3279'; // Itabela/BA';
      2914703: CodTOM := '3595'; // Itaberaba/BA';
      2914802: CodTOM := '3597'; // Itabuna/BA';
      2914901: CodTOM := '3599'; // Itacare/BA';
      2915007: CodTOM := '3601'; // Itaete/BA';
      2915106: CodTOM := '3603'; // Itagi/BA';
      2915205: CodTOM := '3605'; // Itagiba/BA';
      2915304: CodTOM := '3607'; // Itagimirim/BA';
      2915353: CodTOM := '3281'; // Itaguacu Da Bahia/BA';
      2915403: CodTOM := '3609'; // Itaju Do Colonia/BA';
      2915502: CodTOM := '3611'; // Itajuipe/BA';
      2915601: CodTOM := '3613'; // Itamaraju/BA';
      2915700: CodTOM := '3615'; // Itamari/BA';
      2915809: CodTOM := '3617'; // Itambe/BA';
      2915908: CodTOM := '3619'; // Itanagra/BA';
      2916005: CodTOM := '3621'; // Itanhem/BA';
      2916104: CodTOM := '3623'; // Itaparica/BA';
      2916203: CodTOM := '3625'; // Itape/BA';
      2916302: CodTOM := '3627'; // Itapebi/BA';
      2916401: CodTOM := '3629'; // Itapetinga/BA';
      2916500: CodTOM := '3631'; // Itapicuru/BA';
      2916609: CodTOM := '3633'; // Itapitanga/BA';
      2916708: CodTOM := '3635'; // Itaquara/BA';
      2916807: CodTOM := '3637'; // Itarantim/BA';
      2916856: CodTOM := '3283'; // Itatim/BA';
      2916906: CodTOM := '3639'; // Itirucu/BA';
      2917003: CodTOM := '3641'; // Itiuba/BA';
      2917102: CodTOM := '3643'; // Itororo/BA';
      2917201: CodTOM := '3645'; // Ituacu/BA';
      2917300: CodTOM := '3647'; // Itubera/BA';
      2917334: CodTOM := '3285'; // Iuiu/BA';
      2917359: CodTOM := '9859'; // Jaborandi/BA';
      2917409: CodTOM := '3649'; // Jacaraci/BA';
      2917508: CodTOM := '3651'; // Jacobina/BA';
      2917607: CodTOM := '3653'; // Jaguaquara/BA';
      2917706: CodTOM := '3655'; // Jaguarari/BA';
      2917805: CodTOM := '3657'; // Jaguaripe/BA';
      2917904: CodTOM := '3659'; // Jandaira/BA';
      2918001: CodTOM := '3661'; // Jequie/BA';
      2918100: CodTOM := '3663'; // Jeremoabo/BA';
      2918209: CodTOM := '3665'; // Jiquirica/BA';
      2918308: CodTOM := '3667'; // Jitauna/BA';
      2918357: CodTOM := '3099'; // Joao Dourado/BA';
      2918407: CodTOM := '3669'; // Juazeiro/BA';
      2918456: CodTOM := '3287'; // Jucurucu/BA';
      2918506: CodTOM := '3671'; // Jussara/BA';
      2918555: CodTOM := '3069'; // Jussari/BA';
      2918605: CodTOM := '3673'; // Jussiape/BA';
      2918704: CodTOM := '3675'; // Lafaiete Coutinho/BA';
      2918753: CodTOM := '3289'; // Lagoa Real/BA';
      2918803: CodTOM := '3677'; // Laje/BA';
      2918902: CodTOM := '3679'; // Lajedao/BA';
      2919009: CodTOM := '3681'; // Lajedinho/BA';
      2919058: CodTOM := '3291'; // Lajedo Do Tabocal/BA';
      2919108: CodTOM := '3683'; // Lamarao/BA';
      2919157: CodTOM := '3973'; // Lapao/BA';
      2919207: CodTOM := '3685'; // Lauro De Freitas/BA';
      2919306: CodTOM := '3687'; // Lencois/BA';
      2919405: CodTOM := '3689'; // Licinio De Almeida/BA';
      2919504: CodTOM := '3691'; // Livramento De Nossa Senhora/BA';
      2919553: CodTOM := '1112'; // Luis Eduardo Magalhaes/BA';
      2919603: CodTOM := '3693'; // Macajuba/BA';
      2919702: CodTOM := '3695'; // Macarani/BA';
      2919801: CodTOM := '3697'; // Macaubas/BA';
      2919900: CodTOM := '3699'; // Macurure/BA';
      2919926: CodTOM := '3293'; // Madre De Deus/BA';
      2919959: CodTOM := '3975'; // Maetinga/BA';
      2920007: CodTOM := '3701'; // Maiquinique/BA';
      2920106: CodTOM := '3703'; // Mairi/BA';
      2920205: CodTOM := '3705'; // Malhada/BA';
      2920304: CodTOM := '3707'; // Malhada De Pedras/BA';
      2920403: CodTOM := '3709'; // Manoel Vitorino/BA';
      2920452: CodTOM := '3977'; // Mansidao/BA';
      2920502: CodTOM := '3711'; // Maracas/BA';
      2920601: CodTOM := '3713'; // Maragogipe/BA';
      2920700: CodTOM := '3715'; // Marau/BA';
      2920809: CodTOM := '3717'; // Marcionilio Souza/BA';
      2920908: CodTOM := '3719'; // Mascote/BA';
      2921005: CodTOM := '3721'; // Mata De Sao Joao/BA';
      2921054: CodTOM := '3295'; // Matina/BA';
      2921104: CodTOM := '3723'; // Medeiros Neto/BA';
      2921203: CodTOM := '3725'; // Miguel Calmon/BA';
      2921302: CodTOM := '3727'; // Milagres/BA';
      2921401: CodTOM := '3729'; // Mirangaba/BA';
      2921450: CodTOM := '3297'; // Mirante/BA';
      2921500: CodTOM := '3731'; // Monte Santo/BA';
      2921609: CodTOM := '3733'; // Morpara/BA';
      2921708: CodTOM := '3735'; // Morro Do Chapeu/BA';
      2921807: CodTOM := '3737'; // Mortugaba/BA';
      2921906: CodTOM := '3739'; // Mucuge/BA';
      2922003: CodTOM := '3741'; // Mucuri/BA';
      2922052: CodTOM := '3299'; // Mulungu Do Morro/BA';
      2922102: CodTOM := '3743'; // Mundo Novo/BA';
      2922201: CodTOM := '3745'; // Muniz Ferreira/BA';
      2922250: CodTOM := '3005'; // Muquem De Sao Francisco/BA';
      2922300: CodTOM := '3747'; // Muritiba/BA';
      2922409: CodTOM := '3749'; // Mutuipe/BA';
      2922508: CodTOM := '3751'; // Nazare/BA';
      2922607: CodTOM := '3753'; // Nilo Pecanha/BA';
      2922656: CodTOM := '3979'; // Nordestina/BA';
      2922706: CodTOM := '3755'; // Nova Canaa/BA';
      2922730: CodTOM := '3007'; // Nova Fatima/BA';
      2922755: CodTOM := '3009'; // Nova Ibia/BA';
      2922805: CodTOM := '3757'; // Nova Itarana/BA';
      2922854: CodTOM := '3011'; // Nova Redencao/BA';
      2922904: CodTOM := '3759'; // Nova Soure/BA';
      2923001: CodTOM := '3761'; // Nova Vicosa/BA';
      2923035: CodTOM := '3013'; // Novo Horizonte/BA';
      2923050: CodTOM := '3015'; // Novo Triunfo/BA';
      2923100: CodTOM := '3763'; // Olindina/BA';
      2923209: CodTOM := '3765'; // Oliveira Dos Brejinhos/BA';
      2923308: CodTOM := '3767'; // Ouricangas/BA';
      2923357: CodTOM := '3017'; // Ourolandia/BA';
      2923407: CodTOM := '3769'; // Palmas De Monte Alto/BA';
      2923506: CodTOM := '3771'; // Palmeiras/BA';
      2923605: CodTOM := '3773'; // Paramirim/BA';
      2923704: CodTOM := '3775'; // Paratinga/BA';
      2923803: CodTOM := '3777'; // Paripiranga/BA';
      2923902: CodTOM := '3779'; // Pau Brasil/BA';
      2924009: CodTOM := '3781'; // Paulo Afonso/BA';
      2924058: CodTOM := '3981'; // Pe De Serra/BA';
      2924108: CodTOM := '3783'; // Pedrao/BA';
      2924207: CodTOM := '3785'; // Pedro Alexandre/BA';
      2924306: CodTOM := '3787'; // Piata/BA';
      2924405: CodTOM := '3789'; // Pilao Arcado/BA';
      2924504: CodTOM := '3791'; // Pindai/BA';
      2924603: CodTOM := '3793'; // Pindobacu/BA';
      2924652: CodTOM := '3983'; // Pintadas/BA';
      2924678: CodTOM := '3019'; // Pirai Do Norte/BA';
      2924702: CodTOM := '3795'; // Piripa/BA';
      2924801: CodTOM := '3797'; // Piritiba/BA';
      2924900: CodTOM := '3799'; // Planaltino/BA';
      2925006: CodTOM := '3801'; // Planalto/BA';
      2925105: CodTOM := '3803'; // Pocoes/BA';
      2925204: CodTOM := '3805'; // Pojuca/BA';
      2925253: CodTOM := '3021'; // Ponto Novo/BA';
      2925303: CodTOM := '3807'; // Porto Seguro/BA';
      2925402: CodTOM := '3809'; // Potiragua/BA';
      2925501: CodTOM := '3811'; // Prado/BA';
      2925600: CodTOM := '3813'; // Presidente Dutra/BA';
      2925709: CodTOM := '3815'; // Presidente Janio Quadros/BA';
      2925758: CodTOM := '3023'; // Presidente Tancredo Neves/BA';
      2925808: CodTOM := '3817'; // Queimadas/BA';
      2925907: CodTOM := '3819'; // Quijingue/BA';
      2925931: CodTOM := '3025'; // Quixabeira/BA';
      2925956: CodTOM := '3985'; // Rafael Jambeiro/BA';
      2926004: CodTOM := '3821'; // Remanso/BA';
      2926103: CodTOM := '3823'; // Retirolandia/BA';
      2926202: CodTOM := '3825'; // Riachao Das Neves/BA';
      2926301: CodTOM := '3827'; // Riachao Do Jacuipe/BA';
      2926400: CodTOM := '3829'; // Riacho De Santana/BA';
      2926509: CodTOM := '3831'; // Ribeira Do Amparo/BA';
      2926608: CodTOM := '3833'; // Ribeira Do Pombal/BA';
      2926657: CodTOM := '3027'; // Ribeirao Do Largo/BA';
      2926707: CodTOM := '3835'; // Rio De Contas/BA';
      2926806: CodTOM := '3837'; // Rio Do Antonio/BA';
      2926905: CodTOM := '3839'; // Rio Do Pires/BA';
      2927002: CodTOM := '3841'; // Rio Real/BA';
      2927101: CodTOM := '3843'; // Rodelas/BA';
      2927200: CodTOM := '3845'; // Ruy Barbosa/BA';
      2927309: CodTOM := '3847'; // Salinas Da Margarida/BA';
      2927408: CodTOM := '3849'; // Salvador/BA';
      2927507: CodTOM := '3851'; // Santa Barbara/BA';
      2927606: CodTOM := '3853'; // Santa Brigida/BA';
      2927705: CodTOM := '3855'; // Santa Cruz Cabralia/BA';
      2927804: CodTOM := '3857'; // Santa Cruz Da Vitoria/BA';
      2927903: CodTOM := '3859'; // Santa Ines/BA';
      2928000: CodTOM := '3861'; // Santaluz/BA';
      2928059: CodTOM := '3987'; // Santa Luzia/BA';
      2928109: CodTOM := '3863'; // Santa Maria Da Vitoria/BA';
      2928208: CodTOM := '3865'; // Santana/BA';
      2928307: CodTOM := '3867'; // Santanopolis/BA';
      2928406: CodTOM := '3549'; // Santa Rita De Cassia/BA';
      2928505: CodTOM := '3869'; // Santa Teresinha/BA';
      2928604: CodTOM := '3871'; // Santo Amaro/BA';
      2928703: CodTOM := '3873'; // Santo Antonio De Jesus/BA';
      2928802: CodTOM := '3875'; // Santo Estevao/BA';
      2928901: CodTOM := '3877'; // Sao Desiderio/BA';
      2928950: CodTOM := '3029'; // Sao Domingos/BA';
      2929008: CodTOM := '3879'; // Sao Felix/BA';
      2929057: CodTOM := '3031'; // Sao Felix Do Coribe/BA';
      2929107: CodTOM := '3881'; // Sao Felipe/BA';
      2929206: CodTOM := '3883'; // Sao Francisco Do Conde/BA';
      2929255: CodTOM := '3989'; // Sao Gabriel/BA';
      2929305: CodTOM := '3885'; // Sao Goncalo Dos Campos/BA';
      2929354: CodTOM := '3035'; // Sao Jose Da Vitoria/BA';
      2929370: CodTOM := '3033'; // Sao Jose Do Jacuipe/BA';
      2929404: CodTOM := '3887'; // Sao Miguel Das Matas/BA';
      2929503: CodTOM := '3889'; // Sao Sebastiao Do Passe/BA';
      2929602: CodTOM := '3891'; // Sapeacu/BA';
      2929701: CodTOM := '3893'; // Satiro Dias/BA';
      2929750: CodTOM := '3037'; // Saubara/BA';
      2929800: CodTOM := '3895'; // Saude/BA';
      2929909: CodTOM := '3897'; // Seabra/BA';
      2930006: CodTOM := '3899'; // Sebastiao Laranjeiras/BA';
      2930105: CodTOM := '3901'; // Senhor Do Bonfim/BA';
      2930154: CodTOM := '3039'; // Serra Do Ramalho/BA';
      2930204: CodTOM := '3903'; // Sento Se/BA';
      2930303: CodTOM := '3905'; // Serra Dourada/BA';
      2930402: CodTOM := '3907'; // Serra Preta/BA';
      2930501: CodTOM := '3909'; // Serrinha/BA';
      2930600: CodTOM := '3911'; // Serrolandia/BA';
      2930709: CodTOM := '3913'; // Simoes Filho/BA';
      2930758: CodTOM := '3041'; // Sitio Do Mato/BA';
      2930766: CodTOM := '3043'; // Sitio Do Quinto/BA';
      2930774: CodTOM := '3045'; // Sobradinho/BA';
      2930808: CodTOM := '3915'; // Souto Soares/BA';
      2930907: CodTOM := '3917'; // Tabocas Do Brejo Velho/BA';
      2931004: CodTOM := '3919'; // Tanhacu/BA';
      2931053: CodTOM := '3991'; // Tanque Novo/BA';
      2931103: CodTOM := '3921'; // Tanquinho/BA';
      2931202: CodTOM := '3923'; // Taperoa/BA';
      2931301: CodTOM := '3925'; // Tapiramuta/BA';
      2931350: CodTOM := '3993'; // Teixeira De Freitas/BA';
      2931400: CodTOM := '3927'; // Teodoro Sampaio/BA';
      2931509: CodTOM := '3929'; // Teofilandia/BA';
      2931608: CodTOM := '3931'; // Teolandia/BA';
      2931707: CodTOM := '3933'; // Terra Nova/BA';
      2931806: CodTOM := '3935'; // Tremedal/BA';
      2931905: CodTOM := '3937'; // Tucano/BA';
      2932002: CodTOM := '3939'; // Uaua/BA';
      2932101: CodTOM := '3941'; // Ubaira/BA';
      2932200: CodTOM := '3943'; // Ubaitaba/BA';
      2932309: CodTOM := '3945'; // Ubata/BA';
      2932408: CodTOM := '3947'; // Uibai/BA';
      2932457: CodTOM := '3047'; // Umburanas/BA';
      2932507: CodTOM := '3949'; // Una/BA';
      2932606: CodTOM := '3951'; // Urandi/BA';
      2932705: CodTOM := '3953'; // Urucuca/BA';
      2932804: CodTOM := '3955'; // Utinga/BA';
      2932903: CodTOM := '3957'; // Valenca/BA';
      2933000: CodTOM := '3959'; // Valente/BA';
      2933059: CodTOM := '3997'; // Varzea Da Roca/BA';
      2933109: CodTOM := '3961'; // Varzea Do Poco/BA';
      2933158: CodTOM := '3995'; // Varzea Nova/BA';
      2933174: CodTOM := '3049'; // Varzedo/BA';
      2933208: CodTOM := '3963'; // Vera Cruz/BA';
      2933257: CodTOM := '3051'; // Vereda/BA';
      2933307: CodTOM := '3965'; // Vitoria Da Conquista/BA';
      2933406: CodTOM := '3967'; // Wagner/BA';
      2933455: CodTOM := '3999'; // Wanderley/BA';
      2933505: CodTOM := '3969'; // Wenceslau Guimaraes/BA';
      2933604: CodTOM := '3971'; // Xique-Xique/BA';
   end;
 end;

 procedure P31;
 begin
   case ACodigo of
      3100104: CodTOM := '4001'; // Abadia Dos Dourados/MG';
      3100203: CodTOM := '4003'; // Abaete/MG';
      3100302: CodTOM := '4005'; // Abre Campo/MG';
      3100401: CodTOM := '4007'; // Acaiaca/MG';
      3100500: CodTOM := '4009'; // Acucena/MG';
      3100609: CodTOM := '4011'; // Agua Boa/MG';
      3100708: CodTOM := '4013'; // Agua Comprida/MG';
      3100807: CodTOM := '4015'; // Aguanil/MG';
      3100906: CodTOM := '4017'; // Aguas Formosas/MG';
      3101003: CodTOM := '4019'; // Aguas Vermelhas/MG';
      3101102: CodTOM := '4021'; // Aimores/MG';
      3101201: CodTOM := '4023'; // Aiuruoca/MG';
      3101300: CodTOM := '4025'; // Alagoa/MG';
      3101409: CodTOM := '4027'; // Albertina/MG';
      3101508: CodTOM := '4029'; // Alem Paraiba/MG';
      3101607: CodTOM := '4031'; // Alfenas/MG';
      3101631: CodTOM := '2681'; // Alfredo Vasconcelos/MG';
      3101706: CodTOM := '4033'; // Almenara/MG';
      3101805: CodTOM := '4035'; // Alpercata/MG';
      3101904: CodTOM := '4037'; // Alpinopolis/MG';
      3102001: CodTOM := '4039'; // Alterosa/MG';
      3102050: CodTOM := '0564'; // Alto Caparao/MG';
      3102100: CodTOM := '4041'; // Alto Rio Doce/MG';
      3102209: CodTOM := '4043'; // Alvarenga/MG';
      3102308: CodTOM := '4045'; // Alvinopolis/MG';
      3102407: CodTOM := '4047'; // Alvorada De Minas/MG';
      3102506: CodTOM := '4049'; // Amparo Do Serra/MG';
      3102605: CodTOM := '4051'; // Andradas/MG';
      3102704: CodTOM := '4196'; // Cachoeira De Pajeu/MG';
      3102803: CodTOM := '4055'; // Andrelandia/MG';
      3102852: CodTOM := '0566'; // Angelandia/MG';
      3102902: CodTOM := '4057'; // Antonio Carlos/MG';
      3103009: CodTOM := '4059'; // Antonio Dias/MG';
      3103108: CodTOM := '4061'; // Antonio Prado De Minas/MG';
      3103207: CodTOM := '4063'; // Aracai/MG';
      3103306: CodTOM := '4065'; // Aracitaba/MG';
      3103405: CodTOM := '4067'; // Aracuai/MG';
      3103504: CodTOM := '4069'; // Araguari/MG';
      3103603: CodTOM := '4071'; // Arantina/MG';
      3103702: CodTOM := '4073'; // Araponga/MG';
      3103751: CodTOM := '2903'; // Arapora/MG';
      3103801: CodTOM := '4075'; // Arapua/MG';
      3103900: CodTOM := '4077'; // Araujos/MG';
      3104007: CodTOM := '4079'; // Araxa/MG';
      3104106: CodTOM := '4081'; // Arceburgo/MG';
      3104205: CodTOM := '4083'; // Arcos/MG';
      3104304: CodTOM := '4085'; // Areado/MG';
      3104403: CodTOM := '4087'; // Argirita/MG';
      3104452: CodTOM := '0568'; // Aricanduva/MG';
      3104502: CodTOM := '4089'; // Arinos/MG';
      3104601: CodTOM := '4091'; // Astolfo Dutra/MG';
      3104700: CodTOM := '4093'; // Ataleia/MG';
      3104809: CodTOM := '4095'; // Augusto De Lima/MG';
      3104908: CodTOM := '4097'; // Baependi/MG';
      3105004: CodTOM := '4099'; // Baldim/MG';
      3105103: CodTOM := '4101'; // Bambui/MG';
      3105202: CodTOM := '4103'; // Bandeira/MG';
      3105301: CodTOM := '4105'; // Bandeira Do Sul/MG';
      3105400: CodTOM := '4107'; // Barao De Cocais/MG';
      3105509: CodTOM := '4109'; // Barao De Monte Alto/MG';
      3105608: CodTOM := '4111'; // Barbacena/MG';
      3105707: CodTOM := '4113'; // Barra Longa/MG';
      3105905: CodTOM := '4117'; // Barroso/MG';
      3106002: CodTOM := '4119'; // Bela Vista De Minas/MG';
      3106101: CodTOM := '4121'; // Belmiro Braga/MG';
      3106200: CodTOM := '4123'; // Belo Horizonte/MG';
      3106309: CodTOM := '4125'; // Belo Oriente/MG';
      3106408: CodTOM := '4127'; // Belo Vale/MG';
      3106507: CodTOM := '4129'; // Berilo/MG';
      3106606: CodTOM := '4131'; // Bertopolis/MG';
      3106655: CodTOM := '0570'; // Berizal/MG';
      3106705: CodTOM := '4133'; // Betim/MG';
      3106804: CodTOM := '4135'; // Bias Fortes/MG';
      3106903: CodTOM := '4137'; // Bicas/MG';
      3107000: CodTOM := '4139'; // Biquinhas/MG';
      3107109: CodTOM := '4141'; // Boa Esperanca/MG';
      3107208: CodTOM := '4143'; // Bocaina De Minas/MG';
      3107307: CodTOM := '4145'; // Bocaiuva/MG';
      3107406: CodTOM := '4147'; // Bom Despacho/MG';
      3107505: CodTOM := '4149'; // Bom Jardim De Minas/MG';
      3107604: CodTOM := '4151'; // Bom Jesus Da Penha/MG';
      3107703: CodTOM := '4153'; // Bom Jesus Do Amparo/MG';
      3107802: CodTOM := '4155'; // Bom Jesus Do Galho/MG';
      3107901: CodTOM := '4157'; // Bom Repouso/MG';
      3108008: CodTOM := '4159'; // Bom Sucesso/MG';
      3108107: CodTOM := '4161'; // Bonfim/MG';
      3108206: CodTOM := '4163'; // Bonfinopolis De Minas/MG';
      3108255: CodTOM := '0572'; // Bonito De Minas/MG';
      3108305: CodTOM := '4165'; // Borda Da Mata/MG';
      3108404: CodTOM := '4167'; // Botelhos/MG';
      3108503: CodTOM := '4169'; // Botumirim/MG';
      3108552: CodTOM := '0574'; // Brasilandia De Minas/MG';
      3108602: CodTOM := '4171'; // Brasilia De Minas/MG';
      3108701: CodTOM := '4173'; // Bras Pires/MG';
      3108800: CodTOM := '4175'; // Braunas/MG';
      3108909: CodTOM := '4177'; // Brasopolis/MG';
      3109006: CodTOM := '4179'; // Brumadinho/MG';
      3109105: CodTOM := '4181'; // Bueno Brandao/MG';
      3109204: CodTOM := '4183'; // Buenopolis/MG';
      3109253: CodTOM := '0576'; // Bugre/MG';
      3109303: CodTOM := '4185'; // Buritis/MG';
      3109402: CodTOM := '4187'; // Buritizeiro/MG';
      3109451: CodTOM := '0578'; // Cabeceira Grande/MG';
      3109501: CodTOM := '4189'; // Cabo Verde/MG';
      3109600: CodTOM := '4191'; // Cachoeira Da Prata/MG';
      3109709: CodTOM := '4193'; // Cachoeira De Minas/MG';
      3109808: CodTOM := '4195'; // Cachoeira Dourada/MG';
      3109907: CodTOM := '4197'; // Caetanopolis/MG';
      3110004: CodTOM := '4199'; // Caete/MG';
      3110103: CodTOM := '4201'; // Caiana/MG';
      3110202: CodTOM := '4203'; // Cajuri/MG';
      3110301: CodTOM := '4205'; // Caldas/MG';
      3110400: CodTOM := '4207'; // Camacho/MG';
      3110509: CodTOM := '4209'; // Camanducaia/MG';
      3110608: CodTOM := '4211'; // Cambui/MG';
      3110707: CodTOM := '4213'; // Cambuquira/MG';
      3110806: CodTOM := '4215'; // Campanario/MG';
      3110905: CodTOM := '4217'; // Campanha/MG';
      3111002: CodTOM := '4219'; // Campestre/MG';
      3111101: CodTOM := '4221'; // Campina Verde/MG';
      3111150: CodTOM := '0580'; // Campo Azul/MG';
      3111200: CodTOM := '4223'; // Campo Belo/MG';
      3111309: CodTOM := '4225'; // Campo Do Meio/MG';
      3111408: CodTOM := '4227'; // Campo Florido/MG';
      3111507: CodTOM := '4229'; // Campos Altos/MG';
      3111606: CodTOM := '4231'; // Campos Gerais/MG';
      3111705: CodTOM := '4233'; // Canaa/MG';
      3111804: CodTOM := '4235'; // Canapolis/MG';
      3111903: CodTOM := '4237'; // Cana Verde/MG';
      3112000: CodTOM := '4239'; // Candeias/MG';
      3112059: CodTOM := '0582'; // Cantagalo/MG';
      3112109: CodTOM := '4241'; // Caparao/MG';
      3112208: CodTOM := '4243'; // Capela Nova/MG';
      3112307: CodTOM := '4245'; // Capelinha/MG';
      3112406: CodTOM := '4247'; // Capetinga/MG';
      3112505: CodTOM := '4249'; // Capim Branco/MG';
      3112604: CodTOM := '4251'; // Capinopolis/MG';
      3112653: CodTOM := '2651'; // Capitao Andrade/MG';
      3112703: CodTOM := '4253'; // Capitao Eneas/MG';
      3112802: CodTOM := '4255'; // Capitolio/MG';
      3112901: CodTOM := '4257'; // Caputira/MG';
      3113008: CodTOM := '4259'; // Carai/MG';
      3113107: CodTOM := '4261'; // Caranaiba/MG';
      3113206: CodTOM := '4263'; // Carandai/MG';
      3113305: CodTOM := '4265'; // Carangola/MG';
      3113404: CodTOM := '4267'; // Caratinga/MG';
      3113503: CodTOM := '4269'; // Carbonita/MG';
      3113602: CodTOM := '4271'; // Careacu/MG';
      3113701: CodTOM := '4273'; // Carlos Chagas/MG';
      3113800: CodTOM := '4275'; // Carmesia/MG';
      3113909: CodTOM := '4277'; // Carmo Da Cachoeira/MG';
      3114006: CodTOM := '4279'; // Carmo Da Mata/MG';
      3114105: CodTOM := '4281'; // Carmo De Minas/MG';
      3114204: CodTOM := '4283'; // Carmo Do Cajuru/MG';
      3114303: CodTOM := '4285'; // Carmo Do Paranaiba/MG';
      3114402: CodTOM := '4287'; // Carmo Do Rio Claro/MG';
      3114501: CodTOM := '4289'; // Carmopolis De Minas/MG';
      3114550: CodTOM := '2685'; // Carneirinho/MG';
      3114600: CodTOM := '4291'; // Carrancas/MG';
      3114709: CodTOM := '4293'; // Carvalhopolis/MG';
      3114808: CodTOM := '4295'; // Carvalhos/MG';
      3114907: CodTOM := '4297'; // Casa Grande/MG';
      3115003: CodTOM := '4299'; // Cascalho Rico/MG';
      3115102: CodTOM := '4301'; // Cassia/MG';
      3115201: CodTOM := '4303'; // Conceicao Da Barra De Minas/MG';
      3115300: CodTOM := '4305'; // Cataguases/MG';
      3115359: CodTOM := '0584'; // Catas Altas/MG';
      3115409: CodTOM := '4307'; // Catas Altas Da Noruega/MG';
      3115458: CodTOM := '2653'; // Catuji/MG';
      3115474: CodTOM := '0586'; // Catuti/MG';
      3115508: CodTOM := '4309'; // Caxambu/MG';
      3115607: CodTOM := '4311'; // Cedro Do Abaete/MG';
      3115706: CodTOM := '4313'; // Central De Minas/MG';
      3115805: CodTOM := '4315'; // Centralina/MG';
      3115904: CodTOM := '4317'; // Chacara/MG';
      3116001: CodTOM := '4319'; // Chale/MG';
      3116100: CodTOM := '4321'; // Chapada Do Norte/MG';
      3116159: CodTOM := '0588'; // Chapada Gaucha/MG';
      3116209: CodTOM := '4323'; // Chiador/MG';
      3116308: CodTOM := '4325'; // Cipotanea/MG';
      3116407: CodTOM := '4327'; // Claraval/MG';
      3116506: CodTOM := '4329'; // Claro Dos Pocoes/MG';
      3116605: CodTOM := '4331'; // Claudio/MG';
      3116704: CodTOM := '4333'; // Coimbra/MG';
      3116803: CodTOM := '4335'; // Coluna/MG';
      3116902: CodTOM := '4337'; // Comendador Gomes/MG';
      3117009: CodTOM := '4339'; // Comercinho/MG';
      3117108: CodTOM := '4341'; // Conceicao Da Aparecida/MG';
      3117207: CodTOM := '4343'; // Conceicao Das Pedras/MG';
      3117306: CodTOM := '4345'; // Conceicao Das Alagoas/MG';
      3117405: CodTOM := '4347'; // Conceicao De Ipanema/MG';
      3117504: CodTOM := '4349'; // Conceicao Do Mato Dentro/MG';
      3117603: CodTOM := '4351'; // Conceicao Do Para/MG';
      3117702: CodTOM := '4353'; // Conceicao Do Rio Verde/MG';
      3117801: CodTOM := '4355'; // Conceicao Dos Ouros/MG';
      3117836: CodTOM := '0590'; // Conego Marinho/MG';
      3117876: CodTOM := '0592'; // Confins/MG';
      3117900: CodTOM := '4357'; // Congonhal/MG';
      3118007: CodTOM := '4359'; // Congonhas/MG';
      3118106: CodTOM := '4361'; // Congonhas Do Norte/MG';
      3118205: CodTOM := '4363'; // Conquista/MG';
      3118304: CodTOM := '4365'; // Conselheiro Lafaiete/MG';
      3118403: CodTOM := '4367'; // Conselheiro Pena/MG';
      3118502: CodTOM := '4369'; // Consolacao/MG';
      3118601: CodTOM := '4371'; // Contagem/MG';
      3118700: CodTOM := '4373'; // Coqueiral/MG';
      3118809: CodTOM := '4375'; // Coracao De Jesus/MG';
      3118908: CodTOM := '4377'; // Cordisburgo/MG';
      3119005: CodTOM := '4379'; // Cordislandia/MG';
      3119104: CodTOM := '4381'; // Corinto/MG';
      3119203: CodTOM := '4383'; // Coroaci/MG';
      3119302: CodTOM := '4385'; // Coromandel/MG';
      3119401: CodTOM := '4387'; // Coronel Fabriciano/MG';
      3119500: CodTOM := '4389'; // Coronel Murta/MG';
      3119609: CodTOM := '4391'; // Coronel Pacheco/MG';
      3119708: CodTOM := '4393'; // Coronel Xavier Chaves/MG';
      3119807: CodTOM := '4395'; // Corrego Danta/MG';
      3119906: CodTOM := '4397'; // Corrego Do Bom Jesus/MG';
      3119955: CodTOM := '0594'; // Corrego Fundo/MG';
      3120003: CodTOM := '4399'; // Corrego Novo/MG';
      3120102: CodTOM := '4401'; // Couto De Magalhaes De Minas/MG';
      3120151: CodTOM := '0596'; // Crisolita/MG';
      3120201: CodTOM := '4403'; // Cristais/MG';
      3120300: CodTOM := '4405'; // Cristalia/MG';
      3120409: CodTOM := '4407'; // Cristiano Otoni/MG';
      3120508: CodTOM := '4409'; // Cristina/MG';
      3120607: CodTOM := '4411'; // Crucilandia/MG';
      3120706: CodTOM := '4413'; // Cruzeiro Da Fortaleza/MG';
      3120805: CodTOM := '4415'; // Cruzilia/MG';
      3120839: CodTOM := '0598'; // Cuparaque/MG';
      3120870: CodTOM := '0600'; // Curral De Dentro/MG';
      3120904: CodTOM := '4417'; // Curvelo/MG';
      3121001: CodTOM := '4419'; // Datas/MG';
      3121100: CodTOM := '4421'; // Delfim Moreira/MG';
      3121209: CodTOM := '4423'; // Delfinopolis/MG';
      3121258: CodTOM := '0602'; // Delta/MG';
      3121308: CodTOM := '4425'; // Descoberto/MG';
      3121407: CodTOM := '4427'; // Desterro De Entre Rios/MG';
      3121506: CodTOM := '4429'; // Desterro Do Melo/MG';
      3121605: CodTOM := '4431'; // Diamantina/MG';
      3121704: CodTOM := '4433'; // Diogo De Vasconcelos/MG';
      3121803: CodTOM := '4435'; // Dionisio/MG';
      3121902: CodTOM := '4437'; // Divinesia/MG';
      3122009: CodTOM := '4439'; // Divino/MG';
      3122108: CodTOM := '4441'; // Divino Das Laranjeiras/MG';
      3122207: CodTOM := '4443'; // Divinolandia De Minas/MG';
      3122306: CodTOM := '4445'; // Divinopolis/MG';
      3122355: CodTOM := '0604'; // Divisa Alegre/MG';
      3122405: CodTOM := '4447'; // Divisa Nova/MG';
      3122454: CodTOM := '2657'; // Divisopolis/MG';
      3122470: CodTOM := '0606'; // Dom Bosco/MG';
      3122504: CodTOM := '4449'; // Dom Cavati/MG';
      3122603: CodTOM := '4451'; // Dom Joaquim/MG';
      3122702: CodTOM := '4453'; // Dom Silverio/MG';
      3122801: CodTOM := '4455'; // Dom Vicoso/MG';
      3122900: CodTOM := '4457'; // Dona Eusebia/MG';
      3123007: CodTOM := '4459'; // Dores De Campos/MG';
      3123106: CodTOM := '4461'; // Dores De Guanhaes/MG';
      3123205: CodTOM := '4463'; // Dores Do Indaia/MG';
      3123304: CodTOM := '4465'; // Dores Do Turvo/MG';
      3123403: CodTOM := '4467'; // Doresopolis/MG';
      3123502: CodTOM := '4469'; // Douradoquara/MG';
      3123528: CodTOM := '2675'; // Durande/MG';
      3123601: CodTOM := '4471'; // Eloi Mendes/MG';
      3123700: CodTOM := '4473'; // Engenheiro Caldas/MG';
      3123809: CodTOM := '4475'; // Engenheiro Navarro/MG';
      3123858: CodTOM := '2663'; // Entre Folhas/MG';
      3123908: CodTOM := '4477'; // Entre Rios De Minas/MG';
      3124005: CodTOM := '4479'; // Ervalia/MG';
      3124104: CodTOM := '4481'; // Esmeraldas/MG';
      3124203: CodTOM := '4483'; // Espera Feliz/MG';
      3124302: CodTOM := '4485'; // Espinosa/MG';
      3124401: CodTOM := '4487'; // Espirito Santo Do Dourado/MG';
      3124500: CodTOM := '4489'; // Estiva/MG';
      3124609: CodTOM := '4491'; // Estrela Dalva/MG';
      3124708: CodTOM := '4493'; // Estrela Do Indaia/MG';
      3124807: CodTOM := '4495'; // Estrela Do Sul/MG';
      3124906: CodTOM := '4497'; // Eugenopolis/MG';
      3125002: CodTOM := '4499'; // Ewbank Da Camara/MG';
      3125101: CodTOM := '4501'; // Extrema/MG';
      3125200: CodTOM := '4503'; // Fama/MG';
      3125309: CodTOM := '4505'; // Faria Lemos/MG';
      3125408: CodTOM := '4507'; // Felicio Dos Santos/MG';
      3125507: CodTOM := '5238'; // Sao Goncalo Do Rio Preto/MG';
      3125606: CodTOM := '4511'; // Felisburgo/MG';
      3125705: CodTOM := '4513'; // Felixlandia/MG';
      3125804: CodTOM := '4515'; // Fernandes Tourinho/MG';
      3125903: CodTOM := '4517'; // Ferros/MG';
      3125952: CodTOM := '2683'; // Fervedouro/MG';
      3126000: CodTOM := '4519'; // Florestal/MG';
      3126109: CodTOM := '4521'; // Formiga/MG';
      3126208: CodTOM := '4523'; // Formoso/MG';
      3126307: CodTOM := '4525'; // Fortaleza De Minas/MG';
      3126406: CodTOM := '4527'; // Fortuna De Minas/MG';
      3126505: CodTOM := '4529'; // Francisco Badaro/MG';
      3126604: CodTOM := '4531'; // Francisco Dumont/MG';
      3126703: CodTOM := '4533'; // Francisco Sa/MG';
      3126752: CodTOM := '0608'; // Franciscopolis/MG';
      3126802: CodTOM := '4535'; // Frei Gaspar/MG';
      3126901: CodTOM := '4537'; // Frei Inocencio/MG';
      3126950: CodTOM := '0610'; // Frei Lagonegro/MG';
      3127008: CodTOM := '4539'; // Fronteira/MG';
      3127057: CodTOM := '4935'; // Fronteira Dos Vales/MG';
      3127073: CodTOM := '0612'; // Fruta De Leite/MG';
      3127107: CodTOM := '4541'; // Frutal/MG';
      3127206: CodTOM := '4543'; // Funilandia/MG';
      3127305: CodTOM := '4545'; // Galileia/MG';
      3127339: CodTOM := '0614'; // Gameleiras/MG';
      3127354: CodTOM := '0616'; // Glaucilandia/MG';
      3127370: CodTOM := '0618'; // Goiabeira/MG';
      3127388: CodTOM := '0620'; // Goiana/MG';
      3127404: CodTOM := '4547'; // Goncalves/MG';
      3127503: CodTOM := '4549'; // Gonzaga/MG';
      3127602: CodTOM := '4551'; // Gouveia/MG';
      3127701: CodTOM := '4553'; // Governador Valadares/MG';
      3127800: CodTOM := '4555'; // Grao Mogol/MG';
      3127909: CodTOM := '4557'; // Grupiara/MG';
      3128006: CodTOM := '4559'; // Guanhaes/MG';
      3128105: CodTOM := '4561'; // Guape/MG';
      3128204: CodTOM := '4563'; // Guaraciaba/MG';
      3128253: CodTOM := '0622'; // Guaraciama/MG';
      3128303: CodTOM := '4565'; // Guaranesia/MG';
      3128402: CodTOM := '4567'; // Guarani/MG';
      3128501: CodTOM := '4569'; // Guarara/MG';
      3128600: CodTOM := '4571'; // Guarda-Mor/MG';
      3128709: CodTOM := '4573'; // Guaxupe/MG';
      3128808: CodTOM := '4575'; // Guidoval/MG';
      3128907: CodTOM := '4577'; // Guimarania/MG';
      3129004: CodTOM := '4579'; // Guiricema/MG';
      3129103: CodTOM := '4581'; // Gurinhata/MG';
      3129202: CodTOM := '4583'; // Heliodora/MG';
      3129301: CodTOM := '4585'; // Iapu/MG';
      3129400: CodTOM := '4587'; // Ibertioga/MG';
      3129509: CodTOM := '4589'; // Ibia/MG';
      3129608: CodTOM := '4591'; // Ibiai/MG';
      3129657: CodTOM := '0624'; // Ibiracatu/MG';
      3129707: CodTOM := '4593'; // Ibiraci/MG';
      3129806: CodTOM := '4595'; // Ibirite/MG';
      3129905: CodTOM := '4597'; // Ibitiura De Minas/MG';
      3130002: CodTOM := '4599'; // Ibituruna/MG';
      3130051: CodTOM := '2693'; // Icarai De Minas/MG';
      3130101: CodTOM := '4601'; // Igarape/MG';
      3130200: CodTOM := '4603'; // Igaratinga/MG';
      3130309: CodTOM := '4605'; // Iguatama/MG';
      3130408: CodTOM := '4607'; // Ijaci/MG';
      3130507: CodTOM := '4609'; // Ilicinea/MG';
      3130556: CodTOM := '0626'; // Imbe De Minas/MG';
      3130606: CodTOM := '4611'; // Inconfidentes/MG';
      3130655: CodTOM := '0628'; // Indaiabira/MG';
      3130705: CodTOM := '4613'; // Indianopolis/MG';
      3130804: CodTOM := '4615'; // Ingai/MG';
      3130903: CodTOM := '4617'; // Inhapim/MG';
      3131000: CodTOM := '4619'; // Inhauma/MG';
      3131109: CodTOM := '4621'; // Inimutaba/MG';
      3131158: CodTOM := '2665'; // Ipaba/MG';
      3131208: CodTOM := '4623'; // Ipanema/MG';
      3131307: CodTOM := '4625'; // Ipatinga/MG';
      3131406: CodTOM := '4627'; // Ipiacu/MG';
      3131505: CodTOM := '4629'; // Ipuiuna/MG';
      3131604: CodTOM := '4631'; // Irai De Minas/MG';
      3131703: CodTOM := '4633'; // Itabira/MG';
      3131802: CodTOM := '4635'; // Itabirinha/MG';
      3131901: CodTOM := '4637'; // Itabirito/MG';
      3132008: CodTOM := '4639'; // Itacambira/MG';
      3132107: CodTOM := '4641'; // Itacarambi/MG';
      3132206: CodTOM := '4643'; // Itaguara/MG';
      3132305: CodTOM := '4645'; // Itaipe/MG';
      3132404: CodTOM := '4647'; // Itajuba/MG';
      3132503: CodTOM := '4649'; // Itamarandiba/MG';
      3132602: CodTOM := '4651'; // Itamarati De Minas/MG';
      3132701: CodTOM := '4653'; // Itambacuri/MG';
      3132800: CodTOM := '4655'; // Itambe Do Mato Dentro/MG';
      3132909: CodTOM := '4657'; // Itamogi/MG';
      3133006: CodTOM := '4659'; // Itamonte/MG';
      3133105: CodTOM := '4661'; // Itanhandu/MG';
      3133204: CodTOM := '4663'; // Itanhomi/MG';
      3133303: CodTOM := '4665'; // Itaobim/MG';
      3133402: CodTOM := '4667'; // Itapagipe/MG';
      3133501: CodTOM := '4669'; // Itapecerica/MG';
      3133600: CodTOM := '4671'; // Itapeva/MG';
      3133709: CodTOM := '4673'; // Itatiaiucu/MG';
      3133758: CodTOM := '5731'; // Itau De Minas/MG';
      3133808: CodTOM := '4675'; // Itauna/MG';
      3133907: CodTOM := '4677'; // Itaverava/MG';
      3134004: CodTOM := '4679'; // Itinga/MG';
      3134103: CodTOM := '4681'; // Itueta/MG';
      3134202: CodTOM := '4683'; // Ituiutaba/MG';
      3134301: CodTOM := '4685'; // Itumirim/MG';
      3134400: CodTOM := '4687'; // Iturama/MG';
      3134509: CodTOM := '4689'; // Itutinga/MG';
      3134608: CodTOM := '4691'; // Jaboticatubas/MG';
      3134707: CodTOM := '4693'; // Jacinto/MG';
      3134806: CodTOM := '4695'; // Jacui/MG';
      3134905: CodTOM := '4697'; // Jacutinga/MG';
      3135001: CodTOM := '4699'; // Jaguaracu/MG';
      3135050: CodTOM := '2893'; // Jaiba/MG';
      3135076: CodTOM := '2655'; // Jampruca/MG';
      3135100: CodTOM := '4701'; // Janauba/MG';
      3135209: CodTOM := '4703'; // Januaria/MG';
      3135308: CodTOM := '4705'; // Japaraiba/MG';
      3135357: CodTOM := '0630'; // Japonvar/MG';
      3135407: CodTOM := '4707'; // Jeceaba/MG';
      3135456: CodTOM := '0632'; // Jenipapo De Minas/MG';
      3135506: CodTOM := '4709'; // Jequeri/MG';
      3135605: CodTOM := '4711'; // Jequitai/MG';
      3135704: CodTOM := '4713'; // Jequitiba/MG';
      3135803: CodTOM := '4715'; // Jequitinhonha/MG';
      3135902: CodTOM := '4717'; // Jesuania/MG';
      3136009: CodTOM := '4719'; // Joaima/MG';
      3136108: CodTOM := '4721'; // Joanesia/MG';
      3136207: CodTOM := '4723'; // Joao Monlevade/MG';
      3136306: CodTOM := '4725'; // Joao Pinheiro/MG';
      3136405: CodTOM := '4727'; // Joaquim Felicio/MG';
      3136504: CodTOM := '4729'; // Jordania/MG';
      3136520: CodTOM := '0634'; // Jose Goncalves De Minas/MG';
      3136553: CodTOM := '0636'; // Jose Raydan/MG';
      3136579: CodTOM := '0638'; // Josenopolis/MG';
      3136603: CodTOM := '4731'; // Nova Uniao/MG';
      3136652: CodTOM := '2691'; // Juatuba/MG';
      3136702: CodTOM := '4733'; // Juiz De Fora/MG';
      3136801: CodTOM := '4735'; // Juramento/MG';
      3136900: CodTOM := '4737'; // Juruaia/MG';
      3136959: CodTOM := '0640'; // Juvenilia/MG';
      3137007: CodTOM := '4739'; // Ladainha/MG';
      3137106: CodTOM := '4741'; // Lagamar/MG';
      3137205: CodTOM := '4743'; // Lagoa Da Prata/MG';
      3137304: CodTOM := '4745'; // Lagoa Dos Patos/MG';
      3137403: CodTOM := '4747'; // Lagoa Dourada/MG';
      3137502: CodTOM := '4749'; // Lagoa Formosa/MG';
      3137536: CodTOM := '2905'; // Lagoa Grande/MG';
      3137601: CodTOM := '4751'; // Lagoa Santa/MG';
      3137700: CodTOM := '4753'; // Lajinha/MG';
      3137809: CodTOM := '4755'; // Lambari/MG';
      3137908: CodTOM := '4757'; // Lamim/MG';
      3138005: CodTOM := '4759'; // Laranjal/MG';
      3138104: CodTOM := '4761'; // Lassance/MG';
      3138203: CodTOM := '4763'; // Lavras/MG';
      3138302: CodTOM := '4765'; // Leandro Ferreira/MG';
      3138351: CodTOM := '0642'; // Leme Do Prado/MG';
      3138401: CodTOM := '4767'; // Leopoldina/MG';
      3138500: CodTOM := '4769'; // Liberdade/MG';
      3138609: CodTOM := '4771'; // Lima Duarte/MG';
      3138625: CodTOM := '2687'; // Limeira Do Oeste/MG';
      3138658: CodTOM := '2695'; // Lontra/MG';
      3138674: CodTOM := '0644'; // Luisburgo/MG';
      3138682: CodTOM := '0646'; // Luislandia/MG';
      3138708: CodTOM := '4773'; // Luminarias/MG';
      3138807: CodTOM := '4775'; // Luz/MG';
      3138906: CodTOM := '4777'; // Machacalis/MG';
      3139003: CodTOM := '4779'; // Machado/MG';
      3139102: CodTOM := '4781'; // Madre De Deus De Minas/MG';
      3139201: CodTOM := '4783'; // Malacacheta/MG';
      3139250: CodTOM := '2895'; // Mamonas/MG';
      3139300: CodTOM := '4785'; // Manga/MG';
      3139409: CodTOM := '4787'; // Manhuacu/MG';
      3139508: CodTOM := '4789'; // Manhumirim/MG';
      3139607: CodTOM := '4791'; // Mantena/MG';
      3139706: CodTOM := '4793'; // Maravilhas/MG';
      3139805: CodTOM := '4795'; // Mar De Espanha/MG';
      3139904: CodTOM := '4797'; // Maria Da Fe/MG';
      3140001: CodTOM := '4799'; // Mariana/MG';
      3140100: CodTOM := '4801'; // Marilac/MG';
      3140159: CodTOM := '0648'; // Mario Campos/MG';
      3140209: CodTOM := '4803'; // Maripa De Minas/MG';
      3140308: CodTOM := '4805'; // Marlieria/MG';
      3140407: CodTOM := '4807'; // Marmelopolis/MG';
      3140506: CodTOM := '4809'; // Martinho Campos/MG';
      3140530: CodTOM := '0650'; // Martins Soares/MG';
      3140555: CodTOM := '2659'; // Mata Verde/MG';
      3140605: CodTOM := '4811'; // Materlandia/MG';
      3140704: CodTOM := '4813'; // Mateus Leme/MG';
      3140803: CodTOM := '4815'; // Matias Barbosa/MG';
      3140852: CodTOM := '2897'; // Matias Cardoso/MG';
      3140902: CodTOM := '4817'; // Matipo/MG';
      3141009: CodTOM := '4819'; // Mato Verde/MG';
      3141108: CodTOM := '4821'; // Matozinhos/MG';
      3141207: CodTOM := '4823'; // Matutina/MG';
      3141306: CodTOM := '4825'; // Medeiros/MG';
      3141405: CodTOM := '4827'; // Medina/MG';
      3141504: CodTOM := '4829'; // Mendes Pimentel/MG';
      3141603: CodTOM := '4831'; // Merces/MG';
      3141702: CodTOM := '4833'; // Mesquita/MG';
      3141801: CodTOM := '4835'; // Minas Novas/MG';
      3141900: CodTOM := '4837'; // Minduri/MG';
      3142007: CodTOM := '4839'; // Mirabela/MG';
      3142106: CodTOM := '4841'; // Miradouro/MG';
      3142205: CodTOM := '4843'; // Mirai/MG';
      3142254: CodTOM := '0652'; // Miravania/MG';
      3142304: CodTOM := '4845'; // Moeda/MG';
      3142403: CodTOM := '4847'; // Moema/MG';
      3142502: CodTOM := '4849'; // Monjolos/MG';
      3142601: CodTOM := '4851'; // Monsenhor Paulo/MG';
      3142700: CodTOM := '4853'; // Montalvania/MG';
      3142809: CodTOM := '4855'; // Monte Alegre De Minas/MG';
      3142908: CodTOM := '4857'; // Monte Azul/MG';
      3143005: CodTOM := '4859'; // Monte Belo/MG';
      3143104: CodTOM := '4861'; // Monte Carmelo/MG';
      3143153: CodTOM := '0654'; // Monte Formoso/MG';
      3143203: CodTOM := '4863'; // Monte Santo De Minas/MG';
      3143302: CodTOM := '4865'; // Montes Claros/MG';
      3143401: CodTOM := '4867'; // Monte Siao/MG';
      3143450: CodTOM := '2697'; // Montezuma/MG';
      3143500: CodTOM := '4869'; // Morada Nova De Minas/MG';
      3143609: CodTOM := '4871'; // Morro Da Garca/MG';
      3143708: CodTOM := '4873'; // Morro Do Pilar/MG';
      3143807: CodTOM := '4875'; // Munhoz/MG';
      3143906: CodTOM := '4877'; // Muriae/MG';
      3144003: CodTOM := '4879'; // Mutum/MG';
      3144102: CodTOM := '4881'; // Muzambinho/MG';
      3144201: CodTOM := '4883'; // Nacip Raydan/MG';
      3144300: CodTOM := '4885'; // Nanuque/MG';
      3144359: CodTOM := '0656'; // Naque/MG';
      3144375: CodTOM := '0658'; // Natalandia/MG';
      3144409: CodTOM := '4887'; // Natercia/MG';
      3144508: CodTOM := '4889'; // Nazareno/MG';
      3144607: CodTOM := '4891'; // Nepomuceno/MG';
      3144656: CodTOM := '0660'; // Ninheira/MG';
      3144672: CodTOM := '0662'; // Nova Belem/MG';
      3144706: CodTOM := '4893'; // Nova Era/MG';
      3144805: CodTOM := '4895'; // Nova Lima/MG';
      3144904: CodTOM := '4897'; // Nova Modica/MG';
      3145000: CodTOM := '4899'; // Nova Ponte/MG';
      3145059: CodTOM := '0664'; // Nova Porteirinha/MG';
      3145109: CodTOM := '4901'; // Nova Resende/MG';
      3145208: CodTOM := '4903'; // Nova Serrana/MG';
      3145307: CodTOM := '4905'; // Novo Cruzeiro/MG';
      3145356: CodTOM := '0666'; // Novo Oriente De Minas/MG';
      3145372: CodTOM := '0668'; // Novorizonte/MG';
      3145406: CodTOM := '4907'; // Olaria/MG';
      3145455: CodTOM := '0670'; // Olhos-D Agua/MG';
      3145505: CodTOM := '4909'; // Olimpio Noronha/MG';
      3145604: CodTOM := '4911'; // Oliveira/MG';
      3145703: CodTOM := '4913'; // Oliveira Fortes/MG';
      3145802: CodTOM := '4915'; // Onca De Pitangui/MG';
      3145851: CodTOM := '0672'; // Oratorios/MG';
      3145877: CodTOM := '0674'; // Orizania/MG';
      3145901: CodTOM := '4917'; // Ouro Branco/MG';
      3146008: CodTOM := '4919'; // Ouro Fino/MG';
      3146107: CodTOM := '4921'; // Ouro Preto/MG';
      3146206: CodTOM := '4923'; // Ouro Verde De Minas/MG';
      3146255: CodTOM := '0676'; // Padre Carvalho/MG';
      3146305: CodTOM := '4925'; // Padre Paraiso/MG';
      3146404: CodTOM := '4927'; // Paineiras/MG';
      3146503: CodTOM := '4929'; // Pains/MG';
      3146552: CodTOM := '0678'; // Pai Pedro/MG';
      3146602: CodTOM := '4931'; // Paiva/MG';
      3146701: CodTOM := '4933'; // Palma/MG';
      3146750: CodTOM := '2661'; // Palmopolis/MG';
      3146909: CodTOM := '4937'; // Papagaios/MG';
      3147006: CodTOM := '4939'; // Paracatu/MG';
      3147105: CodTOM := '4941'; // Para De Minas/MG';
      3147204: CodTOM := '4943'; // Paraguacu/MG';
      3147303: CodTOM := '4945'; // Paraisopolis/MG';
      3147402: CodTOM := '4947'; // Paraopeba/MG';
      3147501: CodTOM := '4949'; // Passabem/MG';
      3147600: CodTOM := '4951'; // Passa Quatro/MG';
      3147709: CodTOM := '4953'; // Passa Tempo/MG';
      3147808: CodTOM := '4955'; // Passa-Vinte/MG';
      3147907: CodTOM := '4957'; // Passos/MG';
      3147956: CodTOM := '0680'; // Patis/MG';
      3148004: CodTOM := '4959'; // Patos De Minas/MG';
      3148103: CodTOM := '4961'; // Patrocinio/MG';
      3148202: CodTOM := '4963'; // Patrocinio Do Muriae/MG';
      3148301: CodTOM := '4965'; // Paula Candido/MG';
      3148400: CodTOM := '4967'; // Paulistas/MG';
      3148509: CodTOM := '4969'; // Pavao/MG';
      3148608: CodTOM := '4971'; // Pecanha/MG';
      3148707: CodTOM := '4973'; // Pedra Azul/MG';
      3148756: CodTOM := '0682'; // Pedra Bonita/MG';
      3148806: CodTOM := '4975'; // Pedra Do Anta/MG';
      3148905: CodTOM := '4977'; // Pedra Do Indaia/MG';
      3149002: CodTOM := '4979'; // Pedra Dourada/MG';
      3149101: CodTOM := '4981'; // Pedralva/MG';
      3149150: CodTOM := '2899'; // Pedras De Maria Da Cruz/MG';
      3149200: CodTOM := '4983'; // Pedrinopolis/MG';
      3149309: CodTOM := '4985'; // Pedro Leopoldo/MG';
      3149408: CodTOM := '4987'; // Pedro Teixeira/MG';
      3149507: CodTOM := '4989'; // Pequeri/MG';
      3149606: CodTOM := '4991'; // Pequi/MG';
      3149705: CodTOM := '4993'; // Perdigao/MG';
      3149804: CodTOM := '4995'; // Perdizes/MG';
      3149903: CodTOM := '4997'; // Perdoes/MG';
      3149952: CodTOM := '0684'; // Periquito/MG';
      3150000: CodTOM := '4999'; // Pescador/MG';
      3150109: CodTOM := '5001'; // Piau/MG';
      3150158: CodTOM := '0686'; // Piedade De Caratinga/MG';
      3150208: CodTOM := '5003'; // Piedade De Ponte Nova/MG';
      3150307: CodTOM := '5005'; // Piedade Do Rio Grande/MG';
      3150406: CodTOM := '5007'; // Piedade Dos Gerais/MG';
      3150505: CodTOM := '5009'; // Pimenta/MG';
      3150539: CodTOM := '0688'; // Pingo-D Agua/MG';
      3150570: CodTOM := '0690'; // Pintopolis/MG';
      3150604: CodTOM := '5011'; // Piracema/MG';
      3150703: CodTOM := '5013'; // Pirajuba/MG';
      3150802: CodTOM := '5015'; // Piranga/MG';
      3150901: CodTOM := '5017'; // Pirangucu/MG';
      3151008: CodTOM := '5019'; // Piranguinho/MG';
      3151107: CodTOM := '5021'; // Pirapetinga/MG';
      3151206: CodTOM := '5023'; // Pirapora/MG';
      3151305: CodTOM := '5025'; // Pirauba/MG';
      3151404: CodTOM := '5027'; // Pitangui/MG';
      3151503: CodTOM := '5029'; // Piumhi/MG';
      3151602: CodTOM := '5031'; // Planura/MG';
      3151701: CodTOM := '5033'; // Poco Fundo/MG';
      3151800: CodTOM := '5035'; // Pocos De Caldas/MG';
      3151909: CodTOM := '5037'; // Pocrane/MG';
      3152006: CodTOM := '5039'; // Pompeu/MG';
      3152105: CodTOM := '5041'; // Ponte Nova/MG';
      3152131: CodTOM := '0692'; // Ponto Chique/MG';
      3152170: CodTOM := '0694'; // Ponto Dos Volantes/MG';
      3152204: CodTOM := '5043'; // Porteirinha/MG';
      3152303: CodTOM := '5045'; // Porto Firme/MG';
      3152402: CodTOM := '5047'; // Pote/MG';
      3152501: CodTOM := '5049'; // Pouso Alegre/MG';
      3152600: CodTOM := '5051'; // Pouso Alto/MG';
      3152709: CodTOM := '5053'; // Prados/MG';
      3152808: CodTOM := '5055'; // Prata/MG';
      3152907: CodTOM := '5057'; // Pratapolis/MG';
      3153004: CodTOM := '5059'; // Pratinha/MG';
      3153103: CodTOM := '5061'; // Presidente Bernardes/MG';
      3153202: CodTOM := '5063'; // Presidente Juscelino/MG';
      3153301: CodTOM := '5065'; // Presidente Kubitschek/MG';
      3153400: CodTOM := '5067'; // Presidente Olegario/MG';
      3153509: CodTOM := '5069'; // Alto Jequitiba/MG';
      3153608: CodTOM := '5071'; // Prudente De Morais/MG';
      3153707: CodTOM := '5073'; // Quartel Geral/MG';
      3153806: CodTOM := '5075'; // Queluzito/MG';
      3153905: CodTOM := '5077'; // Raposos/MG';
      3154002: CodTOM := '5079'; // Raul Soares/MG';
      3154101: CodTOM := '5081'; // Recreio/MG';
      3154150: CodTOM := '0696'; // Reduto/MG';
      3154200: CodTOM := '5083'; // Resende Costa/MG';
      3154309: CodTOM := '5085'; // Resplendor/MG';
      3154408: CodTOM := '5087'; // Ressaquinha/MG';
      3154457: CodTOM := '2901'; // Riachinho/MG';
      3154507: CodTOM := '5089'; // Riacho Dos Machados/MG';
      3154606: CodTOM := '5091'; // Ribeirao Das Neves/MG';
      3154705: CodTOM := '5093'; // Ribeirao Vermelho/MG';
      3154804: CodTOM := '5095'; // Rio Acima/MG';
      3154903: CodTOM := '5097'; // Rio Casca/MG';
      3155009: CodTOM := '5099'; // Rio Doce/MG';
      3155108: CodTOM := '5101'; // Rio Do Prado/MG';
      3155207: CodTOM := '5103'; // Rio Espera/MG';
      3155306: CodTOM := '5105'; // Rio Manso/MG';
      3155405: CodTOM := '5107'; // Rio Novo/MG';
      3155504: CodTOM := '5109'; // Rio Paranaiba/MG';
      3155603: CodTOM := '5111'; // Rio Pardo De Minas/MG';
      3155702: CodTOM := '5113'; // Rio Piracicaba/MG';
      3155801: CodTOM := '5115'; // Rio Pomba/MG';
      3155900: CodTOM := '5117'; // Rio Preto/MG';
      3156007: CodTOM := '5119'; // Rio Vermelho/MG';
      3156106: CodTOM := '5121'; // Ritapolis/MG';
      3156205: CodTOM := '5123'; // Rochedo De Minas/MG';
      3156304: CodTOM := '5125'; // Rodeiro/MG';
      3156403: CodTOM := '5127'; // Romaria/MG';
      3156452: CodTOM := '0698'; // Rosario Da Limeira/MG';
      3156502: CodTOM := '5129'; // Rubelita/MG';
      3156601: CodTOM := '5131'; // Rubim/MG';
      3156700: CodTOM := '5133'; // Sabara/MG';
      3156809: CodTOM := '5135'; // Sabinopolis/MG';
      3156908: CodTOM := '5137'; // Sacramento/MG';
      3157005: CodTOM := '5139'; // Salinas/MG';
      3157104: CodTOM := '5141'; // Salto Da Divisa/MG';
      3157203: CodTOM := '5143'; // Santa Barbara/MG';
      3157252: CodTOM := '2667'; // Santa Barbara Do Leste/MG';
      3157278: CodTOM := '0700'; // Santa Barbara Do Monte Verde/MG';
      3157302: CodTOM := '5145'; // Santa Barbara Do Tugurio/MG';
      3157336: CodTOM := '0702'; // Santa Cruz De Minas/MG';
      3157377: CodTOM := '0704'; // Santa Cruz De Salinas/MG';
      3157401: CodTOM := '5147'; // Santa Cruz Do Escalvado/MG';
      3157500: CodTOM := '5149'; // Santa Efigenia De Minas/MG';
      3157609: CodTOM := '5151'; // Santa Fe De Minas/MG';
      3157658: CodTOM := '0706'; // Santa Helena De Minas/MG';
      3157708: CodTOM := '5153'; // Santa Juliana/MG';
      3157807: CodTOM := '5155'; // Santa Luzia/MG';
      3157906: CodTOM := '5157'; // Santa Margarida/MG';
      3158003: CodTOM := '5159'; // Santa Maria De Itabira/MG';
      3158102: CodTOM := '5161'; // Santa Maria Do Salto/MG';
      3158201: CodTOM := '5163'; // Santa Maria Do Suacui/MG';
      3158300: CodTOM := '5165'; // Santana Da Vargem/MG';
      3158409: CodTOM := '5167'; // Santana De Cataguases/MG';
      3158508: CodTOM := '5169'; // Santana De Pirapama/MG';
      3158607: CodTOM := '5171'; // Santana Do Deserto/MG';
      3158706: CodTOM := '5173'; // Santana Do Garambeu/MG';
      3158805: CodTOM := '5175'; // Santana Do Jacare/MG';
      3158904: CodTOM := '5177'; // Santana Do Manhuacu/MG';
      3158953: CodTOM := '2673'; // Santana Do Paraiso/MG';
      3159001: CodTOM := '5179'; // Santana Do Riacho/MG';
      3159100: CodTOM := '5181'; // Santana Dos Montes/MG';
      3159209: CodTOM := '5183'; // Santa Rita De Caldas/MG';
      3159308: CodTOM := '5185'; // Santa Rita De Jacutinga/MG';
      3159357: CodTOM := '2669'; // Santa Rita De Minas/MG';
      3159407: CodTOM := '5187'; // Santa Rita De Ibitipoca/MG';
      3159506: CodTOM := '5189'; // Santa Rita Do Itueto/MG';
      3159605: CodTOM := '5191'; // Santa Rita Do Sapucai/MG';
      3159704: CodTOM := '5193'; // Santa Rosa Da Serra/MG';
      3159803: CodTOM := '5195'; // Santa Vitoria/MG';
      3159902: CodTOM := '5197'; // Santo Antonio Do Amparo/MG';
      3160009: CodTOM := '5199'; // Santo Antonio Do Aventureiro/MG';
      3160108: CodTOM := '5201'; // Santo Antonio Do Grama/MG';
      3160207: CodTOM := '5203'; // Santo Antonio Do Itambe/MG';
      3160306: CodTOM := '5205'; // Santo Antonio Do Jacinto/MG';
      3160405: CodTOM := '5207'; // Santo Antonio Do Monte/MG';
      3160454: CodTOM := '0708'; // Santo Antonio Do Retiro/MG';
      3160504: CodTOM := '5209'; // Santo Antonio Do Rio Abaixo/MG';
      3160603: CodTOM := '5211'; // Santo Hipolito/MG';
      3160702: CodTOM := '5213'; // Santos Dumont/MG';
      3160801: CodTOM := '5215'; // Sao Bento Abade/MG';
      3160900: CodTOM := '5217'; // Sao Bras Do Suacui/MG';
      3160959: CodTOM := '0710'; // Sao Domingos Das Dores/MG';
      3161007: CodTOM := '5219'; // Sao Domingos Do Prata/MG';
      3161056: CodTOM := '0712'; // Sao Felix De Minas/MG';
      3161106: CodTOM := '5221'; // Sao Francisco/MG';
      3161205: CodTOM := '5223'; // Sao Francisco De Paula/MG';
      3161304: CodTOM := '5225'; // Sao Francisco De Sales/MG';
      3161403: CodTOM := '5227'; // Sao Francisco Do Gloria/MG';
      3161502: CodTOM := '5229'; // Sao Geraldo/MG';
      3161601: CodTOM := '5231'; // Sao Geraldo Da Piedade/MG';
      3161650: CodTOM := '0714'; // Sao Geraldo Do Baixio/MG';
      3161700: CodTOM := '5233'; // Sao Goncalo Do Abaete/MG';
      3161809: CodTOM := '5235'; // Sao Goncalo Do Para/MG';
      3161908: CodTOM := '5237'; // Sao Goncalo Do Rio Abaixo/MG';
      3162005: CodTOM := '5239'; // Sao Goncalo Do Sapucai/MG';
      3162104: CodTOM := '5241'; // Sao Gotardo/MG';
      3162203: CodTOM := '5243'; // Sao Joao Batista Do Gloria/MG';
      3162252: CodTOM := '0716'; // Sao Joao Da Lagoa/MG';
      3162302: CodTOM := '5245'; // Sao Joao Da Mata/MG';
      3162401: CodTOM := '5247'; // Sao Joao Da Ponte/MG';
      3162450: CodTOM := '0718'; // Sao Joao Das Missoes/MG';
      3162500: CodTOM := '5249'; // Sao Joao Del Rei/MG';
      3162559: CodTOM := '2677'; // Sao Joao Do Manhuacu/MG';
      3162575: CodTOM := '2679'; // Sao Joao Do Manteninha/MG';
      3162609: CodTOM := '5251'; // Sao Joao Do Oriente/MG';
      3162658: CodTOM := '0720'; // Sao Joao Do Pacui/MG';
      3162708: CodTOM := '5253'; // Sao Joao Do Paraiso/MG';
      3162807: CodTOM := '5255'; // Sao Joao Evangelista/MG';
      3162906: CodTOM := '5257'; // Sao Joao Nepomuceno/MG';
      3162922: CodTOM := '0722'; // Sao Joaquim De Bicas/MG';
      3162948: CodTOM := '0724'; // Sao Jose Da Barra/MG';
      3162955: CodTOM := '2649'; // Sao Jose Da Lapa/MG';
      3163003: CodTOM := '5259'; // Sao Jose Da Safira/MG';
      3163102: CodTOM := '5261'; // Sao Jose Da Varginha/MG';
      3163201: CodTOM := '5263'; // Sao Jose Do Alegre/MG';
      3163300: CodTOM := '5265'; // Sao Jose Do Divino/MG';
      3163409: CodTOM := '5267'; // Sao Jose Do Goiabal/MG';
      3163508: CodTOM := '5269'; // Sao Jose Do Jacuri/MG';
      3163607: CodTOM := '5271'; // Sao Jose Do Mantimento/MG';
      3163706: CodTOM := '5273'; // Sao Lourenco/MG';
      3163805: CodTOM := '5275'; // Sao Miguel Do Anta/MG';
      3163904: CodTOM := '5277'; // Sao Pedro Da Uniao/MG';
      3164001: CodTOM := '5279'; // Sao Pedro Dos Ferros/MG';
      3164100: CodTOM := '5281'; // Sao Pedro Do Suacui/MG';
      3164209: CodTOM := '5283'; // Sao Romao/MG';
      3164308: CodTOM := '5285'; // Sao Roque De Minas/MG';
      3164407: CodTOM := '5287'; // Sao Sebastiao Da Bela Vista/MG';
      3164431: CodTOM := '0726'; // Sao Sebastiao Da Vargem Alegre/MG';
      3164472: CodTOM := '0728'; // Sao Sebastiao Do Anta/MG';
      3164506: CodTOM := '5289'; // Sao Sebastiao Do Maranhao/MG';
      3164605: CodTOM := '5291'; // Sao Sebastiao Do Oeste/MG';
      3164704: CodTOM := '5293'; // Sao Sebastiao Do Paraiso/MG';
      3164803: CodTOM := '5295'; // Sao Sebastiao Do Rio Preto/MG';
      3164902: CodTOM := '5297'; // Sao Sebastiao Do Rio Verde/MG';
      3165008: CodTOM := '5299'; // Sao Tiago/MG';
      3165107: CodTOM := '5301'; // Sao Tomas De Aquino/MG';
      3165206: CodTOM := '5303'; // Sao Thome Das Letras/MG';
      3165305: CodTOM := '5305'; // Sao Vicente De Minas/MG';
      3165404: CodTOM := '5307'; // Sapucai-Mirim/MG';
      3165503: CodTOM := '5309'; // Sardoa/MG';
      3165537: CodTOM := '0730'; // Sarzedo/MG';
      3165552: CodTOM := '0732'; // Setubinha/MG';
      3165560: CodTOM := '0734'; // Sem-Peixe/MG';
      3165578: CodTOM := '2689'; // Senador Amaral/MG';
      3165602: CodTOM := '5311'; // Senador Cortes/MG';
      3165701: CodTOM := '5313'; // Senador Firmino/MG';
      3165800: CodTOM := '5315'; // Senador Jose Bento/MG';
      3165909: CodTOM := '5317'; // Senador Modestino Goncalves/MG';
      3166006: CodTOM := '5319'; // Senhora De Oliveira/MG';
      3166105: CodTOM := '5321'; // Senhora Do Porto/MG';
      3166204: CodTOM := '5323'; // Senhora Dos Remedios/MG';
      3166303: CodTOM := '5325'; // Sericita/MG';
      3166402: CodTOM := '5327'; // Seritinga/MG';
      3166501: CodTOM := '5329'; // Serra Azul De Minas/MG';
      3166600: CodTOM := '5331'; // Serra Da Saudade/MG';
      3166709: CodTOM := '5333'; // Serra Dos Aimores/MG';
      3166808: CodTOM := '5335'; // Serra Do Salitre/MG';
      3166907: CodTOM := '5337'; // Serrania/MG';
      3166956: CodTOM := '0736'; // Serranopolis De Minas/MG';
      3167004: CodTOM := '5339'; // Serranos/MG';
      3167103: CodTOM := '5341'; // Serro/MG';
      3167202: CodTOM := '5343'; // Sete Lagoas/MG';
      3167301: CodTOM := '5345'; // Silveirania/MG';
      3167400: CodTOM := '5347'; // Silvianopolis/MG';
      3167509: CodTOM := '5349'; // Simao Pereira/MG';
      3167608: CodTOM := '5351'; // Simonesia/MG';
      3167707: CodTOM := '5353'; // Sobralia/MG';
      3167806: CodTOM := '5355'; // Soledade De Minas/MG';
      3167905: CodTOM := '5357'; // Tabuleiro/MG';
      3168002: CodTOM := '5359'; // Taiobeiras/MG';
      3168051: CodTOM := '0738'; // Taparuba/MG';
      3168101: CodTOM := '5361'; // Tapira/MG';
      3168200: CodTOM := '5363'; // Tapirai/MG';
      3168309: CodTOM := '5365'; // Taquaracu De Minas/MG';
      3168408: CodTOM := '5367'; // Tarumirim/MG';
      3168507: CodTOM := '5369'; // Teixeiras/MG';
      3168606: CodTOM := '5371'; // Teofilo Otoni/MG';
      3168705: CodTOM := '5373'; // Timoteo/MG';
      3168804: CodTOM := '5375'; // Tiradentes/MG';
      3168903: CodTOM := '5377'; // Tiros/MG';
      3169000: CodTOM := '5379'; // Tocantins/MG';
      3169059: CodTOM := '0740'; // Tocos Do Moji/MG';
      3169109: CodTOM := '5381'; // Toledo/MG';
      3169208: CodTOM := '5383'; // Tombos/MG';
      3169307: CodTOM := '5385'; // Tres Coracoes/MG';
      3169356: CodTOM := '4115'; // Tres Marias/MG';
      3169406: CodTOM := '5387'; // Tres Pontas/MG';
      3169505: CodTOM := '5389'; // Tumiritinga/MG';
      3169604: CodTOM := '5391'; // Tupaciguara/MG';
      3169703: CodTOM := '5393'; // Turmalina/MG';
      3169802: CodTOM := '5395'; // Turvolandia/MG';
      3169901: CodTOM := '5397'; // Uba/MG';
      3170008: CodTOM := '5399'; // Ubai/MG';
      3170057: CodTOM := '2671'; // Ubaporanga/MG';
      3170107: CodTOM := '5401'; // Uberaba/MG';
      3170206: CodTOM := '5403'; // Uberlandia/MG';
      3170305: CodTOM := '5405'; // Umburatiba/MG';
      3170404: CodTOM := '5407'; // Unai/MG';
      3170438: CodTOM := '0742'; // Uniao De Minas/MG';
      3170479: CodTOM := '0744'; // Uruana De Minas/MG';
      3170503: CodTOM := '5409'; // Urucania/MG';
      3170529: CodTOM := '2699'; // Urucuia/MG';
      3170578: CodTOM := '0746'; // Vargem Alegre/MG';
      3170602: CodTOM := '5411'; // Vargem Bonita/MG';
      3170651: CodTOM := '0748'; // Vargem Grande Do Rio Pardo/MG';
      3170701: CodTOM := '5413'; // Varginha/MG';
      3170750: CodTOM := '0750'; // Varjao De Minas/MG';
      3170800: CodTOM := '5415'; // Varzea Da Palma/MG';
      3170909: CodTOM := '5417'; // Varzelandia/MG';
      3171006: CodTOM := '5419'; // Vazante/MG';
      3171030: CodTOM := '0752'; // Verdelandia/MG';
      3171071: CodTOM := '0754'; // Veredinha/MG';
      3171105: CodTOM := '5423'; // Verissimo/MG';
      3171154: CodTOM := '0756'; // Vermelho Novo/MG';
      3171204: CodTOM := '5425'; // Vespasiano/MG';
      3171303: CodTOM := '5427'; // Vicosa/MG';
      3171402: CodTOM := '5429'; // Vieiras/MG';
      3171501: CodTOM := '5431'; // Mathias Lobato/MG';
      3171600: CodTOM := '5433'; // Virgem Da Lapa/MG';
      3171709: CodTOM := '5435'; // Virginia/MG';
      3171808: CodTOM := '5437'; // Virginopolis/MG';
      3171907: CodTOM := '5439'; // Virgolandia/MG';
      3172004: CodTOM := '5441'; // Visconde Do Rio Branco/MG';
      3172103: CodTOM := '5443'; // Volta Grande/MG';
      3172202: CodTOM := '5421'; // Wenceslau Braz/MG';
   end;
 end;

 procedure P32;
 begin
   case ACodigo of
      3200102: CodTOM := '5601'; // Afonso Claudio/ES';
      3200136: CodTOM := '5733'; // Aguia Branca/ES';
      3200169: CodTOM := '5717'; // Agua Doce Do Norte/ES';
      3200201: CodTOM := '5603'; // Alegre/ES';
      3200300: CodTOM := '5605'; // Alfredo Chaves/ES';
      3200359: CodTOM := '5719'; // Alto Rio Novo/ES';
      3200409: CodTOM := '5607'; // Anchieta/ES';
      3200508: CodTOM := '5609'; // Apiaca/ES';
      3200607: CodTOM := '5611'; // Aracruz/ES';
      3200706: CodTOM := '5613'; // Atilio Vivacqua/ES';
      3200805: CodTOM := '5615'; // Baixo Guandu/ES';
      3200904: CodTOM := '5617'; // Barra De Sao Francisco/ES';
      3201001: CodTOM := '5619'; // Boa Esperanca/ES';
      3201100: CodTOM := '5621'; // Bom Jesus Do Norte/ES';
      3201159: CodTOM := '0758'; // Brejetuba/ES';
      3201209: CodTOM := '5623'; // Cachoeiro De Itapemirim/ES';
      3201308: CodTOM := '5625'; // Cariacica/ES';
      3201407: CodTOM := '5627'; // Castelo/ES';
      3201506: CodTOM := '5629'; // Colatina/ES';
      3201605: CodTOM := '5631'; // Conceicao Da Barra/ES';
      3201704: CodTOM := '5633'; // Conceicao Do Castelo/ES';
      3201803: CodTOM := '5635'; // Divino De Sao Lourenco/ES';
      3201902: CodTOM := '5637'; // Domingos Martins/ES';
      3202009: CodTOM := '5639'; // Dores Do Rio Preto/ES';
      3202108: CodTOM := '5641'; // Ecoporanga/ES';
      3202207: CodTOM := '5643'; // Fundao/ES';
      3202256: CodTOM := '1114'; // Governador Lindenberg/ES';
      3202306: CodTOM := '5645'; // Guacui/ES';
      3202405: CodTOM := '5647'; // Guarapari/ES';
      3202454: CodTOM := '5709'; // Ibatiba/ES';
      3202504: CodTOM := '5649'; // Ibiracu/ES';
      3202553: CodTOM := '6011'; // Ibitirama/ES';
      3202603: CodTOM := '5651'; // Iconha/ES';
      3202652: CodTOM := '2931'; // Irupi/ES';
      3202702: CodTOM := '5653'; // Itaguacu/ES';
      3202801: CodTOM := '5655'; // Itapemirim/ES';
      3202900: CodTOM := '5657'; // Itarana/ES';
      3203007: CodTOM := '5659'; // Iuna/ES';
      3203056: CodTOM := '5713'; // Jaguare/ES';
      3203106: CodTOM := '5661'; // Jeronimo Monteiro/ES';
      3203130: CodTOM := '5721'; // Joao Neiva/ES';
      3203163: CodTOM := '5723'; // Laranja Da Terra/ES';
      3203205: CodTOM := '5663'; // Linhares/ES';
      3203304: CodTOM := '5665'; // Mantenopolis/ES';
      3203320: CodTOM := '0760'; // Marataizes/ES';
      3203346: CodTOM := '2929'; // Marechal Floriano/ES';
      3203353: CodTOM := '5707'; // Marilandia/ES';
      3203403: CodTOM := '5667'; // Mimoso Do Sul/ES';
      3203502: CodTOM := '5669'; // Montanha/ES';
      3203601: CodTOM := '5671'; // Mucurici/ES';
      3203700: CodTOM := '5673'; // Muniz Freire/ES';
      3203809: CodTOM := '5675'; // Muqui/ES';
      3203908: CodTOM := '5677'; // Nova Venecia/ES';
      3204005: CodTOM := '5679'; // Pancas/ES';
      3204054: CodTOM := '5715'; // Pedro Canario/ES';
      3204104: CodTOM := '5681'; // Pinheiros/ES';
      3204203: CodTOM := '5683'; // Piuma/ES';
      3204252: CodTOM := '0762'; // Ponto Belo/ES';
      3204302: CodTOM := '5685'; // Presidente Kennedy/ES';
      3204351: CodTOM := '5711'; // Rio Bananal/ES';
      3204401: CodTOM := '5687'; // Rio Novo Do Sul/ES';
      3204500: CodTOM := '5689'; // Santa Leopoldina/ES';
      3204559: CodTOM := '5725'; // Santa Maria De Jetiba/ES';
      3204609: CodTOM := '5691'; // Santa Teresa/ES';
      3204658: CodTOM := '2933'; // Sao Domingos Do Norte/ES';
      3204708: CodTOM := '5693'; // Sao Gabriel Da Palha/ES';
      3204807: CodTOM := '5695'; // Sao Jose Do Calcado/ES';
      3204906: CodTOM := '5697'; // Sao Mateus/ES';
      3204955: CodTOM := '0764'; // Sao Roque Do Canaa/ES';
      3205002: CodTOM := '5699'; // Serra/ES';
      3205010: CodTOM := '0766'; // Sooretama/ES';
      3205036: CodTOM := '5727'; // Vargem Alta/ES';
      3205069: CodTOM := '5729'; // Venda Nova Do Imigrante/ES';
      3205101: CodTOM := '5701'; // Viana/ES';
      3205150: CodTOM := '2935'; // Vila Pavao/ES';
      3205176: CodTOM := '0768'; // Vila Valerio/ES';
      3205200: CodTOM := '5703'; // Vila Velha/ES';
      3205309: CodTOM := '5705'; // Vitoria/ES';
   end;
 end;

 procedure P33;
 begin
   case ACodigo of
      3300100: CodTOM := '5801'; // Angra Dos Reis/RJ';
      3300159: CodTOM := '2919'; // Aperibe/RJ';
      3300209: CodTOM := '5803'; // Araruama/RJ';
      3300225: CodTOM := '2925'; // Areal/RJ';
      3300233: CodTOM := '0770'; // Armacao Dos Buzios/RJ';
      3300258: CodTOM := '5927'; // Arraial Do Cabo/RJ';
      3300308: CodTOM := '5805'; // Barra Do Pirai/RJ';
      3300407: CodTOM := '5807'; // Barra Mansa/RJ';
      3300456: CodTOM := '2909'; // Belford Roxo/RJ';
      3300506: CodTOM := '5809'; // Bom Jardim/RJ';
      3300605: CodTOM := '5811'; // Bom Jesus Do Itabapoana/RJ';
      3300704: CodTOM := '5813'; // Cabo Frio/RJ';
      3300803: CodTOM := '5815'; // Cachoeiras De Macacu/RJ';
      3300902: CodTOM := '5817'; // Cambuci/RJ';
      3300936: CodTOM := '0772'; // Carapebus/RJ';
      3300951: CodTOM := '2927'; // Comendador Levy Gasparian/RJ';
      3301009: CodTOM := '5819'; // Campos Dos Goytacazes/RJ';
      3301108: CodTOM := '5821'; // Cantagalo/RJ';
      3301157: CodTOM := '2915'; // Cardoso Moreira/RJ';
      3301207: CodTOM := '5823'; // Carmo/RJ';
      3301306: CodTOM := '5825'; // Casimiro De Abreu/RJ';
      3301405: CodTOM := '5827'; // Conceicao De Macabu/RJ';
      3301504: CodTOM := '5829'; // Cordeiro/RJ';
      3301603: CodTOM := '5831'; // Duas Barras/RJ';
      3301702: CodTOM := '5833'; // Duque De Caxias/RJ';
      3301801: CodTOM := '5835'; // Engenheiro Paulo De Frontin/RJ';
      3301850: CodTOM := '2907'; // Guapimirim/RJ';
      3301876: CodTOM := '0774'; // Iguaba Grande/RJ';
      3301900: CodTOM := '5837'; // Itaborai/RJ';
      3302007: CodTOM := '5839'; // Itaguai/RJ';
      3302056: CodTOM := '5929'; // Italva/RJ';
      3302106: CodTOM := '5841'; // Itaocara/RJ';
      3302205: CodTOM := '5843'; // Itaperuna/RJ';
      3302254: CodTOM := '6003'; // Itatiaia/RJ';
      3302270: CodTOM := '2913'; // Japeri/RJ';
      3302304: CodTOM := '5845'; // Laje Do Muriae/RJ';
      3302403: CodTOM := '5847'; // Macae/RJ';
      3302452: CodTOM := '0776'; // Macuco/RJ';
      3302502: CodTOM := '5849'; // Mage/RJ';
      3302601: CodTOM := '5851'; // Mangaratiba/RJ';
      3302700: CodTOM := '5853'; // Marica/RJ';
      3302809: CodTOM := '5855'; // Mendes/RJ';
      3302858: CodTOM := '1116'; // Mesquita/RJ';
      3302908: CodTOM := '5857'; // Miguel Pereira/RJ';
      3303005: CodTOM := '5859'; // Miracema/RJ';
      3303104: CodTOM := '5861'; // Natividade/RJ';
      3303203: CodTOM := '5863'; // Nilopolis/RJ';
      3303302: CodTOM := '5865'; // Niteroi/RJ';
      3303401: CodTOM := '5867'; // Nova Friburgo/RJ';
      3303500: CodTOM := '5869'; // Nova Iguacu/RJ';
      3303609: CodTOM := '5871'; // Paracambi/RJ';
      3303708: CodTOM := '5873'; // Paraiba Do Sul/RJ';
      3303807: CodTOM := '5875'; // Parati/RJ';
      3303856: CodTOM := '6005'; // Paty Do Alferes/RJ';
      3303906: CodTOM := '5877'; // Petropolis/RJ';
      3303955: CodTOM := '0778'; // Pinheiral/RJ';
      3304003: CodTOM := '5879'; // Pirai/RJ';
      3304102: CodTOM := '5881'; // Porciuncula/RJ';
      3304110: CodTOM := '0780'; // Porto Real/RJ';
      3304128: CodTOM := '2923'; // Quatis/RJ';
      3304144: CodTOM := '2911'; // Queimados/RJ';
      3304151: CodTOM := '6007'; // Quissama/RJ';
      3304201: CodTOM := '5883'; // Resende/RJ';
      3304300: CodTOM := '5885'; // Rio Bonito/RJ';
      3304409: CodTOM := '5887'; // Rio Claro/RJ';
      3304508: CodTOM := '5889'; // Rio Das Flores/RJ';
      3304524: CodTOM := '2921'; // Rio Das Ostras/RJ';
      3304557: CodTOM := '6001'; // Rio De Janeiro/RJ';
      3304607: CodTOM := '5891'; // Santa Maria Madalena/RJ';
      3304706: CodTOM := '5893'; // Santo Antonio De Padua/RJ';
      3304755: CodTOM := '0782'; // Sao Francisco De Itabapoana/RJ';
      3304805: CodTOM := '5895'; // Sao Fidelis/RJ';
      3304904: CodTOM := '5897'; // Sao Goncalo/RJ';
      3305000: CodTOM := '5899'; // Sao Joao Da Barra/RJ';
      3305109: CodTOM := '5901'; // Sao Joao De Meriti/RJ';
      3305133: CodTOM := '0784'; // Sao Jose De Uba/RJ';
      3305158: CodTOM := '6009'; // Sao Jose Do Vale Do Rio Preto/RJ';
      3305208: CodTOM := '5903'; // Sao Pedro Da Aldeia/RJ';
      3305307: CodTOM := '5905'; // Sao Sebastiao Do Alto/RJ';
      3305406: CodTOM := '5907'; // Sapucaia/RJ';
      3305505: CodTOM := '5909'; // Saquarema/RJ';
      3305554: CodTOM := '0786'; // Seropedica/RJ';
      3305604: CodTOM := '5911'; // Silva Jardim/RJ';
      3305703: CodTOM := '5913'; // Sumidouro/RJ';
      3305752: CodTOM := '0788'; // Tangua/RJ';
      3305802: CodTOM := '5915'; // Teresopolis/RJ';
      3305901: CodTOM := '5917'; // Trajano De Morais/RJ';
      3306008: CodTOM := '5919'; // Tres Rios/RJ';
      3306107: CodTOM := '5921'; // Valenca/RJ';
      3306156: CodTOM := '2917'; // Varre-Sai/RJ';
      3306206: CodTOM := '5923'; // Vassouras/RJ';
      3306305: CodTOM := '5925'; // Volta Redonda/RJ';
   end;
 end;

 procedure P35;
 begin
   case ACodigo of
      3500105: CodTOM := '6101'; // Adamantina/SP';
      3500204: CodTOM := '6103'; // Adolfo/SP';
      3500303: CodTOM := '6105'; // Aguai/SP';
      3500402: CodTOM := '6107'; // Aguas Da Prata/SP';
      3500501: CodTOM := '6109'; // Aguas De Lindoia/SP';
      3500550: CodTOM := '7019'; // Aguas De Santa Barbara/SP';
      3500600: CodTOM := '6111'; // Aguas De Sao Pedro/SP';
      3500709: CodTOM := '6113'; // Agudos/SP';
      3500758: CodTOM := '2995'; // Alambari/SP';
      3500808: CodTOM := '6115'; // Alfredo Marcondes/SP';
      3500907: CodTOM := '6117'; // Altair/SP';
      3501004: CodTOM := '6119'; // Altinopolis/SP';
      3501103: CodTOM := '6121'; // Alto Alegre/SP';
      3501152: CodTOM := '3065'; // Aluminio/SP';
      3501202: CodTOM := '6123'; // Alvares Florence/SP';
      3501301: CodTOM := '6125'; // Alvares Machado/SP';
      3501400: CodTOM := '6127'; // Alvaro De Carvalho/SP';
      3501509: CodTOM := '6129'; // Alvinlandia/SP';
      3501608: CodTOM := '6131'; // Americana/SP';
      3501707: CodTOM := '6133'; // Americo Brasiliense/SP';
      3501806: CodTOM := '6135'; // Americo De Campos/SP';
      3501905: CodTOM := '6137'; // Amparo/SP';
      3502002: CodTOM := '6139'; // Analandia/SP';
      3502101: CodTOM := '6141'; // Andradina/SP';
      3502200: CodTOM := '6143'; // Angatuba/SP';
      3502309: CodTOM := '6145'; // Anhembi/SP';
      3502408: CodTOM := '6147'; // Anhumas/SP';
      3502507: CodTOM := '6149'; // Aparecida/SP';
      3502606: CodTOM := '6151'; // Aparecida D Oeste/SP';
      3502705: CodTOM := '6153'; // Apiai/SP';
      3502754: CodTOM := '3067'; // Aracariguama/SP';
      3502804: CodTOM := '6155'; // Aracatuba/SP';
      3502903: CodTOM := '6157'; // Aracoiaba Da Serra/SP';
      3503000: CodTOM := '6159'; // Aramina/SP';
      3503109: CodTOM := '6161'; // Arandu/SP';
      3503158: CodTOM := '2991'; // Arapei/SP';
      3503208: CodTOM := '6163'; // Araraquara/SP';
      3503307: CodTOM := '6165'; // Araras/SP';
      3503356: CodTOM := '0790'; // Arco-Iris/SP';
      3503406: CodTOM := '6167'; // Arealva/SP';
      3503505: CodTOM := '6169'; // Areias/SP';
      3503604: CodTOM := '6171'; // Areiopolis/SP';
      3503703: CodTOM := '6173'; // Ariranha/SP';
      3503802: CodTOM := '6175'; // Artur Nogueira/SP';
      3503901: CodTOM := '6177'; // Aruja/SP';
      3503950: CodTOM := '2981'; // Aspasia/SP';
      3504008: CodTOM := '6179'; // Assis/SP';
      3504107: CodTOM := '6181'; // Atibaia/SP';
      3504206: CodTOM := '6183'; // Auriflama/SP';
      3504305: CodTOM := '6185'; // Avai/SP';
      3504404: CodTOM := '6187'; // Avanhandava/SP';
      3504503: CodTOM := '6189'; // Avare/SP';
      3504602: CodTOM := '6191'; // Bady Bassitt/SP';
      3504701: CodTOM := '6193'; // Balbinos/SP';
      3504800: CodTOM := '6195'; // Balsamo/SP';
      3504909: CodTOM := '6197'; // Bananal/SP';
      3505005: CodTOM := '6201'; // Barao De Antonina/SP';
      3505104: CodTOM := '6199'; // Barbosa/SP';
      3505203: CodTOM := '6203'; // Bariri/SP';
      3505302: CodTOM := '6205'; // Barra Bonita/SP';
      3505351: CodTOM := '2997'; // Barra Do Chapeu/SP';
      3505401: CodTOM := '6207'; // Barra Do Turvo/SP';
      3505500: CodTOM := '6209'; // Barretos/SP';
      3505609: CodTOM := '6211'; // Barrinha/SP';
      3505708: CodTOM := '6213'; // Barueri/SP';
      3505807: CodTOM := '6215'; // Bastos/SP';
      3505906: CodTOM := '6217'; // Batatais/SP';
      3506003: CodTOM := '6219'; // Bauru/SP';
      3506102: CodTOM := '6221'; // Bebedouro/SP';
      3506201: CodTOM := '6223'; // Bento De Abreu/SP';
      3506300: CodTOM := '6225'; // Bernardino De Campos/SP';
      3506359: CodTOM := '2965'; // Bertioga/SP';
      3506409: CodTOM := '6227'; // Bilac/SP';
      3506508: CodTOM := '6229'; // Birigui/SP';
      3506607: CodTOM := '6231'; // Biritiba-Mirim/SP';
      3506706: CodTOM := '6233'; // Boa Esperanca Do Sul/SP';
      3506805: CodTOM := '6235'; // Bocaina/SP';
      3506904: CodTOM := '6237'; // Bofete/SP';
      3507001: CodTOM := '6239'; // Boituva/SP';
      3507100: CodTOM := '6241'; // Bom Jesus Dos Perdoes/SP';
      3507159: CodTOM := '3059'; // Bom Sucesso De Itarare/SP';
      3507209: CodTOM := '6243'; // Bora/SP';
      3507308: CodTOM := '6245'; // Boraceia/SP';
      3507407: CodTOM := '6247'; // Borborema/SP';
      3507456: CodTOM := '7247'; // Borebi/SP';
      3507506: CodTOM := '6249'; // Botucatu/SP';
      3507605: CodTOM := '6251'; // Braganca Paulista/SP';
      3507704: CodTOM := '6255'; // Brauna/SP';
      3507753: CodTOM := '0792'; // Brejo Alegre/SP';
      3507803: CodTOM := '6257'; // Brodowski/SP';
      3507902: CodTOM := '6259'; // Brotas/SP';
      3508009: CodTOM := '6261'; // Buri/SP';
      3508108: CodTOM := '6263'; // Buritama/SP';
      3508207: CodTOM := '6265'; // Buritizal/SP';
      3508306: CodTOM := '6267'; // Cabralia Paulista/SP';
      3508405: CodTOM := '6269'; // Cabreuva/SP';
      3508504: CodTOM := '6271'; // Cacapava/SP';
      3508603: CodTOM := '6273'; // Cachoeira Paulista/SP';
      3508702: CodTOM := '6275'; // Caconde/SP';
      3508801: CodTOM := '6277'; // Cafelandia/SP';
      3508900: CodTOM := '6279'; // Caiabu/SP';
      3509007: CodTOM := '6281'; // Caieiras/SP';
      3509106: CodTOM := '6283'; // Caiua/SP';
      3509205: CodTOM := '6285'; // Cajamar/SP';
      3509254: CodTOM := '2967'; // Cajati/SP';
      3509304: CodTOM := '6287'; // Cajobi/SP';
      3509403: CodTOM := '6289'; // Cajuru/SP';
      3509452: CodTOM := '2999'; // Campina Do Monte Alegre/SP';
      3509502: CodTOM := '6291'; // Campinas/SP';
      3509601: CodTOM := '6293'; // Campo Limpo Paulista/SP';
      3509700: CodTOM := '6295'; // Campos Do Jordao/SP';
      3509809: CodTOM := '6297'; // Campos Novos Paulista/SP';
      3509908: CodTOM := '6299'; // Cananeia/SP';
      3509957: CodTOM := '0794'; // Canas/SP';
      3510005: CodTOM := '6301'; // Candido Mota/SP';
      3510104: CodTOM := '6303'; // Candido Rodrigues/SP';
      3510153: CodTOM := '2947'; // Canitar/SP';
      3510203: CodTOM := '6305'; // Capao Bonito/SP';
      3510302: CodTOM := '6307'; // Capela Do Alto/SP';
      3510401: CodTOM := '6309'; // Capivari/SP';
      3510500: CodTOM := '6311'; // Caraguatatuba/SP';
      3510609: CodTOM := '6313'; // Carapicuiba/SP';
      3510708: CodTOM := '6315'; // Cardoso/SP';
      3510807: CodTOM := '6317'; // Casa Branca/SP';
      3510906: CodTOM := '6319'; // Cassia Dos Coqueiros/SP';
      3511003: CodTOM := '6321'; // Castilho/SP';
      3511102: CodTOM := '6323'; // Catanduva/SP';
      3511201: CodTOM := '6325'; // Catigua/SP';
      3511300: CodTOM := '6327'; // Cedral/SP';
      3511409: CodTOM := '6329'; // Cerqueira Cesar/SP';
      3511508: CodTOM := '6331'; // Cerquilho/SP';
      3511607: CodTOM := '6333'; // Cesario Lange/SP';
      3511706: CodTOM := '6335'; // Charqueada/SP';
      3511904: CodTOM := '6339'; // Clementina/SP';
      3512001: CodTOM := '6341'; // Colina/SP';
      3512100: CodTOM := '6343'; // Colombia/SP';
      3512209: CodTOM := '6345'; // Conchal/SP';
      3512308: CodTOM := '6347'; // Conchas/SP';
      3512407: CodTOM := '6349'; // Cordeiropolis/SP';
      3512506: CodTOM := '6351'; // Coroados/SP';
      3512605: CodTOM := '6353'; // Coronel Macedo/SP';
      3512704: CodTOM := '6355'; // Corumbatai/SP';
      3512803: CodTOM := '6357'; // Cosmopolis/SP';
      3512902: CodTOM := '6359'; // Cosmorama/SP';
      3513009: CodTOM := '6361'; // Cotia/SP';
      3513108: CodTOM := '6363'; // Cravinhos/SP';
      3513207: CodTOM := '6365'; // Cristais Paulista/SP';
      3513306: CodTOM := '6367'; // Cruzalia/SP';
      3513405: CodTOM := '6369'; // Cruzeiro/SP';
      3513504: CodTOM := '6371'; // Cubatao/SP';
      3513603: CodTOM := '6373'; // Cunha/SP';
      3513702: CodTOM := '6375'; // Descalvado/SP';
      3513801: CodTOM := '6377'; // Diadema/SP';
      3513850: CodTOM := '7249'; // Dirce Reis/SP';
      3513900: CodTOM := '6379'; // Divinolandia/SP';
      3514007: CodTOM := '6381'; // Dobrada/SP';
      3514106: CodTOM := '6383'; // Dois Corregos/SP';
      3514205: CodTOM := '6385'; // Dolcinopolis/SP';
      3514304: CodTOM := '6387'; // Dourado/SP';
      3514403: CodTOM := '6389'; // Dracena/SP';
      3514502: CodTOM := '6391'; // Duartina/SP';
      3514601: CodTOM := '6393'; // Dumont/SP';
      3514700: CodTOM := '6395'; // Echapora/SP';
      3514809: CodTOM := '6397'; // Eldorado/SP';
      3514908: CodTOM := '6399'; // Elias Fausto/SP';
      3514924: CodTOM := '2975'; // Elisiario/SP';
      3514957: CodTOM := '7251'; // Embauba/SP';
      3515004: CodTOM := '6401'; // Embu/SP';
      3515103: CodTOM := '6403'; // Embu-Guacu/SP';
      3515129: CodTOM := '2961'; // Emilianopolis/SP';
      3515152: CodTOM := '2949'; // Engenheiro Coelho/SP';
      3515186: CodTOM := '6865'; // Espirito Santo Do Pinhal/SP';
      3515194: CodTOM := '7253'; // Espirito Santo Do Turvo/SP';
      3515202: CodTOM := '6405'; // Estrela D Oeste/SP';
      3515301: CodTOM := '6407'; // Estrela Do Norte/SP';
      3515350: CodTOM := '7255'; // Euclides Da Cunha Paulista/SP';
      3515400: CodTOM := '6409'; // Fartura/SP';
      3515509: CodTOM := '6411'; // Fernandopolis/SP';
      3515608: CodTOM := '6413'; // Fernando Prestes/SP';
      3515657: CodTOM := '0796'; // Fernao/SP';
      3515707: CodTOM := '6415'; // Ferraz De Vasconcelos/SP';
      3515806: CodTOM := '6417'; // Flora Rica/SP';
      3515905: CodTOM := '6419'; // Floreal/SP';
      3516002: CodTOM := '6421'; // Florida Paulista/SP';
      3516101: CodTOM := '6423'; // Florinia/SP';
      3516200: CodTOM := '6425'; // Franca/SP';
      3516309: CodTOM := '6427'; // Francisco Morato/SP';
      3516408: CodTOM := '6429'; // Franco Da Rocha/SP';
      3516507: CodTOM := '6431'; // Gabriel Monteiro/SP';
      3516606: CodTOM := '6433'; // Galia/SP';
      3516705: CodTOM := '6435'; // Garca/SP';
      3516804: CodTOM := '6437'; // Gastao Vidigal/SP';
      3516853: CodTOM := '0798'; // Gaviao Peixoto/SP';
      3516903: CodTOM := '6439'; // General Salgado/SP';
      3517000: CodTOM := '6441'; // Getulina/SP';
      3517109: CodTOM := '6443'; // Glicerio/SP';
      3517208: CodTOM := '6445'; // Guaicara/SP';
      3517307: CodTOM := '6447'; // Guaimbe/SP';
      3517406: CodTOM := '6449'; // Guaira/SP';
      3517505: CodTOM := '6451'; // Guapiacu/SP';
      3517604: CodTOM := '6453'; // Guapiara/SP';
      3517703: CodTOM := '6455'; // Guara/SP';
      3517802: CodTOM := '6457'; // Guaracai/SP';
      3517901: CodTOM := '6459'; // Guaraci/SP';
      3518008: CodTOM := '6461'; // Guarani D Oeste/SP';
      3518107: CodTOM := '6463'; // Guaranta/SP';
      3518206: CodTOM := '6465'; // Guararapes/SP';
      3518305: CodTOM := '6467'; // Guararema/SP';
      3518404: CodTOM := '6469'; // Guaratingueta/SP';
      3518503: CodTOM := '6471'; // Guarei/SP';
      3518602: CodTOM := '6473'; // Guariba/SP';
      3518701: CodTOM := '6475'; // Guaruja/SP';
      3518800: CodTOM := '6477'; // Guarulhos/SP';
      3518859: CodTOM := '7257'; // Guatapara/SP';
      3518909: CodTOM := '6479'; // Guzolandia/SP';
      3519006: CodTOM := '6481'; // Herculandia/SP';
      3519055: CodTOM := '2953'; // Holambra/SP';
      3519071: CodTOM := '2951'; // Hortolandia/SP';
      3519105: CodTOM := '6483'; // Iacanga/SP';
      3519204: CodTOM := '6485'; // Iacri/SP';
      3519253: CodTOM := '7259'; // Iaras/SP';
      3519303: CodTOM := '6487'; // Ibate/SP';
      3519402: CodTOM := '6489'; // Ibira/SP';
      3519501: CodTOM := '6491'; // Ibirarema/SP';
      3519600: CodTOM := '6493'; // Ibitinga/SP';
      3519709: CodTOM := '6495'; // Ibiuna/SP';
      3519808: CodTOM := '6497'; // Icem/SP';
      3519907: CodTOM := '6499'; // Iepe/SP';
      3520004: CodTOM := '6501'; // Igaracu Do Tiete/SP';
      3520103: CodTOM := '6503'; // Igarapava/SP';
      3520202: CodTOM := '6505'; // Igarata/SP';
      3520301: CodTOM := '6507'; // Iguape/SP';
      3520400: CodTOM := '6509'; // Ilhabela/SP';
      3520426: CodTOM := '2969'; // Ilha Comprida/SP';
      3520442: CodTOM := '2943'; // Ilha Solteira/SP';
      3520509: CodTOM := '6511'; // Indaiatuba/SP';
      3520608: CodTOM := '6513'; // Indiana/SP';
      3520707: CodTOM := '6515'; // Indiapora/SP';
      3520806: CodTOM := '6517'; // Inubia Paulista/SP';
      3520905: CodTOM := '6519'; // Ipaussu/SP';
      3521002: CodTOM := '6521'; // Ipero/SP';
      3521101: CodTOM := '6523'; // Ipeuna/SP';
      3521150: CodTOM := '0800'; // Ipigua/SP';
      3521200: CodTOM := '6525'; // Iporanga/SP';
      3521309: CodTOM := '6527'; // Ipua/SP';
      3521408: CodTOM := '6529'; // Iracemapolis/SP';
      3521507: CodTOM := '6531'; // Irapua/SP';
      3521606: CodTOM := '6533'; // Irapuru/SP';
      3521705: CodTOM := '6535'; // Itabera/SP';
      3521804: CodTOM := '6537'; // Itai/SP';
      3521903: CodTOM := '6539'; // Itajobi/SP';
      3522000: CodTOM := '6541'; // Itaju/SP';
      3522109: CodTOM := '6543'; // Itanhaem/SP';
      3522158: CodTOM := '3053'; // Itaoca/SP';
      3522208: CodTOM := '6545'; // Itapecerica Da Serra/SP';
      3522307: CodTOM := '6547'; // Itapetininga/SP';
      3522406: CodTOM := '6549'; // Itapeva/SP';
      3522505: CodTOM := '6551'; // Itapevi/SP';
      3522604: CodTOM := '6553'; // Itapira/SP';
      3522653: CodTOM := '3055'; // Itapirapua Paulista/SP';
      3522703: CodTOM := '6555'; // Itapolis/SP';
      3522802: CodTOM := '6557'; // Itaporanga/SP';
      3522901: CodTOM := '6559'; // Itapui/SP';
      3523008: CodTOM := '6561'; // Itapura/SP';
      3523107: CodTOM := '6563'; // Itaquaquecetuba/SP';
      3523206: CodTOM := '6565'; // Itarare/SP';
      3523305: CodTOM := '6567'; // Itariri/SP';
      3523404: CodTOM := '6569'; // Itatiba/SP';
      3523503: CodTOM := '6571'; // Itatinga/SP';
      3523602: CodTOM := '6573'; // Itirapina/SP';
      3523701: CodTOM := '6575'; // Itirapua/SP';
      3523800: CodTOM := '6577'; // Itobi/SP';
      3523909: CodTOM := '6579'; // Itu/SP';
      3524006: CodTOM := '6581'; // Itupeva/SP';
      3524105: CodTOM := '6583'; // Ituverava/SP';
      3524204: CodTOM := '6585'; // Jaborandi/SP';
      3524303: CodTOM := '6587'; // Jaboticabal/SP';
      3524402: CodTOM := '6589'; // Jacarei/SP';
      3524501: CodTOM := '6591'; // Jaci/SP';
      3524600: CodTOM := '6593'; // Jacupiranga/SP';
      3524709: CodTOM := '6595'; // Jaguariuna/SP';
      3524808: CodTOM := '6597'; // Jales/SP';
      3524907: CodTOM := '6599'; // Jambeiro/SP';
      3525003: CodTOM := '6601'; // Jandira/SP';
      3525102: CodTOM := '6603'; // Jardinopolis/SP';
      3525201: CodTOM := '6605'; // Jarinu/SP';
      3525300: CodTOM := '6607'; // Jau/SP';
      3525409: CodTOM := '6609'; // Jeriquara/SP';
      3525508: CodTOM := '6611'; // Joanopolis/SP';
      3525607: CodTOM := '6613'; // Joao Ramalho/SP';
      3525706: CodTOM := '6615'; // Jose Bonifacio/SP';
      3525805: CodTOM := '6617'; // Julio Mesquita/SP';
      3525854: CodTOM := '0802'; // Jumirim/SP';
      3525904: CodTOM := '6619'; // Jundiai/SP';
      3526001: CodTOM := '6621'; // Junqueiropolis/SP';
      3526100: CodTOM := '6623'; // Juquia/SP';
      3526209: CodTOM := '6625'; // Juquitiba/SP';
      3526308: CodTOM := '6627'; // Lagoinha/SP';
      3526407: CodTOM := '6629'; // Laranjal Paulista/SP';
      3526506: CodTOM := '6631'; // Lavinia/SP';
      3526605: CodTOM := '6633'; // Lavrinhas/SP';
      3526704: CodTOM := '6635'; // Leme/SP';
      3526803: CodTOM := '6637'; // Lencois Paulista/SP';
      3526902: CodTOM := '6639'; // Limeira/SP';
      3527009: CodTOM := '6641'; // Lindoia/SP';
      3527108: CodTOM := '6643'; // Lins/SP';
      3527207: CodTOM := '6645'; // Lorena/SP';
      3527256: CodTOM := '2937'; // Lourdes/SP';
      3527306: CodTOM := '6647'; // Louveira/SP';
      3527405: CodTOM := '6649'; // Lucelia/SP';
      3527504: CodTOM := '6651'; // Lucianopolis/SP';
      3527603: CodTOM := '6653'; // Luis Antonio/SP';
      3527702: CodTOM := '6655'; // Luiziania/SP';
      3527801: CodTOM := '6657'; // Lupercio/SP';
      3527900: CodTOM := '6659'; // Lutecia/SP';
      3528007: CodTOM := '6661'; // Macatuba/SP';
      3528106: CodTOM := '6663'; // Macaubal/SP';
      3528205: CodTOM := '6665'; // Macedonia/SP';
      3528304: CodTOM := '6667'; // Magda/SP';
      3528403: CodTOM := '6669'; // Mairinque/SP';
      3528502: CodTOM := '6671'; // Mairipora/SP';
      3528601: CodTOM := '6673'; // Manduri/SP';
      3528700: CodTOM := '6675'; // Maraba Paulista/SP';
      3528809: CodTOM := '6677'; // Maracai/SP';
      3528858: CodTOM := '2977'; // Marapoama/SP';
      3528908: CodTOM := '6679'; // Mariapolis/SP';
      3529005: CodTOM := '6681'; // Marilia/SP';
      3529104: CodTOM := '6683'; // Marinopolis/SP';
      3529203: CodTOM := '6685'; // Martinopolis/SP';
      3529302: CodTOM := '6687'; // Matao/SP';
      3529401: CodTOM := '6689'; // Maua/SP';
      3529500: CodTOM := '6691'; // Mendonca/SP';
      3529609: CodTOM := '6693'; // Meridiano/SP';
      3529658: CodTOM := '2983'; // Mesopolis/SP';
      3529708: CodTOM := '6695'; // Miguelopolis/SP';
      3529807: CodTOM := '6697'; // Mineiros Do Tiete/SP';
      3529906: CodTOM := '6699'; // Miracatu/SP';
      3530003: CodTOM := '6701'; // Mira Estrela/SP';
      3530102: CodTOM := '6703'; // Mirandopolis/SP';
      3530201: CodTOM := '6705'; // Mirante Do Paranapanema/SP';
      3530300: CodTOM := '6707'; // Mirassol/SP';
      3530409: CodTOM := '6709'; // Mirassolandia/SP';
      3530508: CodTOM := '6711'; // Mococa/SP';
      3530607: CodTOM := '6713'; // Mogi Das Cruzes/SP';
      3530706: CodTOM := '6715'; // Mogi Guacu/SP';
      3530805: CodTOM := '6717'; // Mogi Mirim/SP';
      3530904: CodTOM := '6719'; // Mombuca/SP';
      3531001: CodTOM := '6721'; // Moncoes/SP';
      3531100: CodTOM := '6723'; // Mongagua/SP';
      3531209: CodTOM := '6725'; // Monte Alegre Do Sul/SP';
      3531308: CodTOM := '6727'; // Monte Alto/SP';
      3531407: CodTOM := '6729'; // Monte Aprazivel/SP';
      3531506: CodTOM := '6731'; // Monte Azul Paulista/SP';
      3531605: CodTOM := '6733'; // Monte Castelo/SP';
      3531704: CodTOM := '6735'; // Monteiro Lobato/SP';
      3531803: CodTOM := '6737'; // Monte Mor/SP';
      3531902: CodTOM := '6739'; // Morro Agudo/SP';
      3532009: CodTOM := '6741'; // Morungaba/SP';
      3532058: CodTOM := '7263'; // Motuca/SP';
      3532108: CodTOM := '6743'; // Murutinga Do Sul/SP';
      3532157: CodTOM := '0804'; // Nantes/SP';
      3532207: CodTOM := '6745'; // Narandiba/SP';
      3532306: CodTOM := '6747'; // Natividade Da Serra/SP';
      3532405: CodTOM := '6749'; // Nazare Paulista/SP';
      3532504: CodTOM := '6751'; // Neves Paulista/SP';
      3532603: CodTOM := '6753'; // Nhandeara/SP';
      3532702: CodTOM := '6755'; // Nipoa/SP';
      3532801: CodTOM := '6757'; // Nova Alianca/SP';
      3532827: CodTOM := '3061'; // Nova Campina/SP';
      3532843: CodTOM := '2985'; // Nova Canaa Paulista/SP';
      3532868: CodTOM := '0806'; // Nova Castilho/SP';
      3532900: CodTOM := '6759'; // Nova Europa/SP';
      3533007: CodTOM := '6761'; // Nova Granada/SP';
      3533106: CodTOM := '6763'; // Nova Guataporanga/SP';
      3533205: CodTOM := '6765'; // Nova Independencia/SP';
      3533254: CodTOM := '2979'; // Novais/SP';
      3533304: CodTOM := '6767'; // Nova Luzitania/SP';
      3533403: CodTOM := '6769'; // Nova Odessa/SP';
      3533502: CodTOM := '6771'; // Novo Horizonte/SP';
      3533601: CodTOM := '6773'; // Nuporanga/SP';
      3533700: CodTOM := '6775'; // Ocaucu/SP';
      3533809: CodTOM := '6777'; // Oleo/SP';
      3533908: CodTOM := '6779'; // Olimpia/SP';
      3534005: CodTOM := '6781'; // Onda Verde/SP';
      3534104: CodTOM := '6783'; // Oriente/SP';
      3534203: CodTOM := '6785'; // Orindiuva/SP';
      3534302: CodTOM := '6787'; // Orlandia/SP';
      3534401: CodTOM := '6789'; // Osasco/SP';
      3534500: CodTOM := '6791'; // Oscar Bressane/SP';
      3534609: CodTOM := '6793'; // Osvaldo Cruz/SP';
      3534708: CodTOM := '6795'; // Ourinhos/SP';
      3534757: CodTOM := '0808'; // Ouroeste/SP';
      3534807: CodTOM := '6797'; // Ouro Verde/SP';
      3534906: CodTOM := '6799'; // Pacaembu/SP';
      3535002: CodTOM := '6801'; // Palestina/SP';
      3535101: CodTOM := '6803'; // Palmares Paulista/SP';
      3535200: CodTOM := '6805'; // Palmeira D Oeste/SP';
      3535309: CodTOM := '6807'; // Palmital/SP';
      3535408: CodTOM := '6809'; // Panorama/SP';
      3535507: CodTOM := '6811'; // Paraguacu Paulista/SP';
      3535606: CodTOM := '6813'; // Paraibuna/SP';
      3535705: CodTOM := '6815'; // Paraiso/SP';
      3535804: CodTOM := '6817'; // Paranapanema/SP';
      3535903: CodTOM := '6819'; // Paranapua/SP';
      3536000: CodTOM := '6821'; // Parapua/SP';
      3536109: CodTOM := '6823'; // Pardinho/SP';
      3536208: CodTOM := '6825'; // Pariquera-Acu/SP';
      3536257: CodTOM := '2989'; // Parisi/SP';
      3536307: CodTOM := '6827'; // Patrocinio Paulista/SP';
      3536406: CodTOM := '6829'; // Pauliceia/SP';
      3536505: CodTOM := '6831'; // Paulinia/SP';
      3536570: CodTOM := '0810'; // Paulistania/SP';
      3536604: CodTOM := '6833'; // Paulo De Faria/SP';
      3536703: CodTOM := '6835'; // Pederneiras/SP';
      3536802: CodTOM := '6837'; // Pedra Bela/SP';
      3536901: CodTOM := '6839'; // Pedranopolis/SP';
      3537008: CodTOM := '6841'; // Pedregulho/SP';
      3537107: CodTOM := '6843'; // Pedreira/SP';
      3537156: CodTOM := '2963'; // Pedrinhas Paulista/SP';
      3537206: CodTOM := '6845'; // Pedro De Toledo/SP';
      3537305: CodTOM := '6847'; // Penapolis/SP';
      3537404: CodTOM := '6849'; // Pereira Barreto/SP';
      3537503: CodTOM := '6851'; // Pereiras/SP';
      3537602: CodTOM := '6853'; // Peruibe/SP';
      3537701: CodTOM := '6855'; // Piacatu/SP';
      3537800: CodTOM := '6857'; // Piedade/SP';
      3537909: CodTOM := '6859'; // Pilar Do Sul/SP';
      3538006: CodTOM := '6861'; // Pindamonhangaba/SP';
      3538105: CodTOM := '6863'; // Pindorama/SP';
      3538204: CodTOM := '6867'; // Pinhalzinho/SP';
      3538303: CodTOM := '6869'; // Piquerobi/SP';
      3538501: CodTOM := '6871'; // Piquete/SP';
      3538600: CodTOM := '6873'; // Piracaia/SP';
      3538709: CodTOM := '6875'; // Piracicaba/SP';
      3538808: CodTOM := '6877'; // Piraju/SP';
      3538907: CodTOM := '6879'; // Pirajui/SP';
      3539004: CodTOM := '6881'; // Pirangi/SP';
      3539103: CodTOM := '6883'; // Pirapora Do Bom Jesus/SP';
      3539202: CodTOM := '6885'; // Pirapozinho/SP';
      3539301: CodTOM := '6887'; // Pirassununga/SP';
      3539400: CodTOM := '6889'; // Piratininga/SP';
      3539509: CodTOM := '6891'; // Pitangueiras/SP';
      3539608: CodTOM := '6893'; // Planalto/SP';
      3539707: CodTOM := '6895'; // Platina/SP';
      3539806: CodTOM := '6897'; // Poa/SP';
      3539905: CodTOM := '6899'; // Poloni/SP';
      3540002: CodTOM := '6901'; // Pompeia/SP';
      3540101: CodTOM := '6903'; // Pongai/SP';
      3540200: CodTOM := '6905'; // Pontal/SP';
      3540259: CodTOM := '2987'; // Pontalinda/SP';
      3540309: CodTOM := '6907'; // Pontes Gestal/SP';
      3540408: CodTOM := '6909'; // Populina/SP';
      3540507: CodTOM := '6911'; // Porangaba/SP';
      3540606: CodTOM := '6913'; // Porto Feliz/SP';
      3540705: CodTOM := '6915'; // Porto Ferreira/SP';
      3540754: CodTOM := '2993'; // Potim/SP';
      3540804: CodTOM := '6917'; // Potirendaba/SP';
      3540853: CodTOM := '0812'; // Pracinha/SP';
      3540903: CodTOM := '6919'; // Pradopolis/SP';
      3541000: CodTOM := '6921'; // Praia Grande/SP';
      3541059: CodTOM := '0814'; // Pratania/SP';
      3541109: CodTOM := '6923'; // Presidente Alves/SP';
      3541208: CodTOM := '6925'; // Presidente Bernardes/SP';
      3541307: CodTOM := '6927'; // Presidente Epitacio/SP';
      3541406: CodTOM := '6929'; // Presidente Prudente/SP';
      3541505: CodTOM := '6931'; // Presidente Venceslau/SP';
      3541604: CodTOM := '6933'; // Promissao/SP';
      3541653: CodTOM := '0816'; // Quadra/SP';
      3541703: CodTOM := '6935'; // Quata/SP';
      3541802: CodTOM := '6937'; // Queiroz/SP';
      3541901: CodTOM := '6939'; // Queluz/SP';
      3542008: CodTOM := '6941'; // Quintana/SP';
      3542107: CodTOM := '6943'; // Rafard/SP';
      3542206: CodTOM := '6945'; // Rancharia/SP';
      3542305: CodTOM := '6947'; // Redencao Da Serra/SP';
      3542404: CodTOM := '6949'; // Regente Feijo/SP';
      3542503: CodTOM := '6951'; // Reginopolis/SP';
      3542602: CodTOM := '6953'; // Registro/SP';
      3542701: CodTOM := '6955'; // Restinga/SP';
      3542800: CodTOM := '6957'; // Ribeira/SP';
      3542909: CodTOM := '6959'; // Ribeirao Bonito/SP';
      3543006: CodTOM := '6961'; // Ribeirao Branco/SP';
      3543105: CodTOM := '6963'; // Ribeirao Corrente/SP';
      3543204: CodTOM := '6965'; // Ribeirao Do Sul/SP';
      3543238: CodTOM := '0818'; // Ribeirao Dos Indios/SP';
      3543253: CodTOM := '3057'; // Ribeirao Grande/SP';
      3543303: CodTOM := '6967'; // Ribeirao Pires/SP';
      3543402: CodTOM := '6969'; // Ribeirao Preto/SP';
      3543501: CodTOM := '6971'; // Riversul/SP';
      3543600: CodTOM := '6973'; // Rifaina/SP';
      3543709: CodTOM := '6975'; // Rincao/SP';
      3543808: CodTOM := '6977'; // Rinopolis/SP';
      3543907: CodTOM := '6979'; // Rio Claro/SP';
      3544004: CodTOM := '6981'; // Rio Das Pedras/SP';
      3544103: CodTOM := '6983'; // Rio Grande Da Serra/SP';
      3544202: CodTOM := '6985'; // Riolandia/SP';
      3544251: CodTOM := '7265'; // Rosana/SP';
      3544301: CodTOM := '6987'; // Roseira/SP';
      3544400: CodTOM := '6989'; // Rubiacea/SP';
      3544509: CodTOM := '6991'; // Rubineia/SP';
      3544608: CodTOM := '6993'; // Sabino/SP';
      3544707: CodTOM := '6995'; // Sagres/SP';
      3544806: CodTOM := '6997'; // Sales/SP';
      3544905: CodTOM := '6999'; // Sales Oliveira/SP';
      3545001: CodTOM := '7001'; // Salesopolis/SP';
      3545100: CodTOM := '7003'; // Salmourao/SP';
      3545159: CodTOM := '5445'; // Saltinho/SP';
      3545209: CodTOM := '7005'; // Salto/SP';
      3545308: CodTOM := '7007'; // Salto De Pirapora/SP';
      3545407: CodTOM := '7009'; // Salto Grande/SP';
      3545506: CodTOM := '7011'; // Sandovalina/SP';
      3545605: CodTOM := '7013'; // Santa Adelia/SP';
      3545704: CodTOM := '7015'; // Santa Albertina/SP';
      3545803: CodTOM := '7017'; // Santa Barbara D Oeste/SP';
      3546009: CodTOM := '7021'; // Santa Branca/SP';
      3546108: CodTOM := '7023'; // Santa Clara D Oeste/SP';
      3546207: CodTOM := '7025'; // Santa Cruz Da Conceicao/SP';
      3546256: CodTOM := '0820'; // Santa Cruz Da Esperanca/SP';
      3546306: CodTOM := '7027'; // Santa Cruz Das Palmeiras/SP';
      3546405: CodTOM := '7029'; // Santa Cruz Do Rio Pardo/SP';
      3546504: CodTOM := '7031'; // Santa Ernestina/SP';
      3546603: CodTOM := '7033'; // Santa Fe Do Sul/SP';
      3546702: CodTOM := '7035'; // Santa Gertrudes/SP';
      3546801: CodTOM := '7037'; // Santa Isabel/SP';
      3546900: CodTOM := '7039'; // Santa Lucia/SP';
      3547007: CodTOM := '7041'; // Santa Maria Da Serra/SP';
      3547106: CodTOM := '7043'; // Santa Mercedes/SP';
      3547205: CodTOM := '7045'; // Santana Da Ponte Pensa/SP';
      3547304: CodTOM := '7047'; // Santana De Parnaiba/SP';
      3547403: CodTOM := '7049'; // Santa Rita D Oeste/SP';
      3547502: CodTOM := '7051'; // Santa Rita Do Passa Quatro/SP';
      3547601: CodTOM := '7053'; // Santa Rosa De Viterbo/SP';
      3547650: CodTOM := '0822'; // Santa Salete/SP';
      3547700: CodTOM := '7055'; // Santo Anastacio/SP';
      3547809: CodTOM := '7057'; // Santo Andre/SP';
      3547908: CodTOM := '7059'; // Santo Antonio Da Alegria/SP';
      3548005: CodTOM := '7061'; // Santo Antonio De Posse/SP';
      3548054: CodTOM := '2939'; // Santo Antonio Do Aracangua/SP';
      3548104: CodTOM := '7063'; // Santo Antonio Do Jardim/SP';
      3548203: CodTOM := '7065'; // Santo Antonio Do Pinhal/SP';
      3548302: CodTOM := '7067'; // Santo Expedito/SP';
      3548401: CodTOM := '7069'; // Santopolis Do Aguapei/SP';
      3548500: CodTOM := '7071'; // Santos/SP';
      3548609: CodTOM := '7073'; // Sao Bento Do Sapucai/SP';
      3548708: CodTOM := '7075'; // Sao Bernardo Do Campo/SP';
      3548807: CodTOM := '7077'; // Sao Caetano Do Sul/SP';
      3548906: CodTOM := '7079'; // Sao Carlos/SP';
      3549003: CodTOM := '7081'; // Sao Francisco/SP';
      3549102: CodTOM := '7083'; // Sao Joao Da Boa Vista/SP';
      3549201: CodTOM := '7085'; // Sao Joao Das Duas Pontes/SP';
      3549250: CodTOM := '2941'; // Sao Joao De Iracema/SP';
      3549300: CodTOM := '7087'; // Sao Joao Do Pau D Alho/SP';
      3549409: CodTOM := '7089'; // Sao Joaquim Da Barra/SP';
      3549508: CodTOM := '7091'; // Sao Jose Da Bela Vista/SP';
      3549607: CodTOM := '7093'; // Sao Jose Do Barreiro/SP';
      3549706: CodTOM := '7095'; // Sao Jose Do Rio Pardo/SP';
      3549805: CodTOM := '7097'; // Sao Jose Do Rio Preto/SP';
      3549904: CodTOM := '7099'; // Sao Jose Dos Campos/SP';
      3549953: CodTOM := '5447'; // Sao Lourenco Da Serra/SP';
      3550001: CodTOM := '7101'; // Sao Luis Do Paraitinga/SP';
      3550100: CodTOM := '7103'; // Sao Manuel/SP';
      3550209: CodTOM := '7105'; // Sao Miguel Arcanjo/SP';
      3550308: CodTOM := '7107'; // Sao Paulo/SP';
      3550407: CodTOM := '7109'; // Sao Pedro/SP';
      3550506: CodTOM := '7111'; // Sao Pedro Do Turvo/SP';
      3550605: CodTOM := '7113'; // Sao Roque/SP';
      3550704: CodTOM := '7115'; // Sao Sebastiao/SP';
      3550803: CodTOM := '7117'; // Sao Sebastiao Da Grama/SP';
      3550902: CodTOM := '7119'; // Sao Simao/SP';
      3551009: CodTOM := '7121'; // Sao Vicente/SP';
      3551108: CodTOM := '7123'; // Sarapui/SP';
      3551207: CodTOM := '7125'; // Sarutaia/SP';
      3551306: CodTOM := '7127'; // Sebastianopolis Do Sul/SP';
      3551405: CodTOM := '7129'; // Serra Azul/SP';
      3551504: CodTOM := '7131'; // Serrana/SP';
      3551603: CodTOM := '7133'; // Serra Negra/SP';
      3551702: CodTOM := '7135'; // Sertaozinho/SP';
      3551801: CodTOM := '7137'; // Sete Barras/SP';
      3551900: CodTOM := '7139'; // Severinia/SP';
      3552007: CodTOM := '7141'; // Silveiras/SP';
      3552106: CodTOM := '7143'; // Socorro/SP';
      3552205: CodTOM := '7145'; // Sorocaba/SP';
      3552304: CodTOM := '7147'; // Sud Mennucci/SP';
      3552403: CodTOM := '7149'; // Sumare/SP';
      3552502: CodTOM := '7151'; // Suzano/SP';
      3552551: CodTOM := '2945'; // Suzanapolis/SP';
      3552601: CodTOM := '7153'; // Tabapua/SP';
      3552700: CodTOM := '7155'; // Tabatinga/SP';
      3552809: CodTOM := '7157'; // Taboao Da Serra/SP';
      3552908: CodTOM := '7159'; // Taciba/SP';
      3553005: CodTOM := '7161'; // Taguai/SP';
      3553104: CodTOM := '7163'; // Taiacu/SP';
      3553203: CodTOM := '7165'; // Taiuva/SP';
      3553302: CodTOM := '7167'; // Tambau/SP';
      3553401: CodTOM := '7169'; // Tanabi/SP';
      3553500: CodTOM := '7171'; // Tapirai/SP';
      3553609: CodTOM := '7173'; // Tapiratiba/SP';
      3553658: CodTOM := '0824'; // Taquaral/SP';
      3553708: CodTOM := '7175'; // Taquaritinga/SP';
      3553807: CodTOM := '7177'; // Taquarituba/SP';
      3553856: CodTOM := '3063'; // Taquarivai/SP';
      3553906: CodTOM := '7179'; // Tarabai/SP';
      3553955: CodTOM := '7267'; // Taruma/SP';
      3554003: CodTOM := '7181'; // Tatui/SP';
      3554102: CodTOM := '7183'; // Taubate/SP';
      3554201: CodTOM := '7185'; // Tejupa/SP';
      3554300: CodTOM := '7187'; // Teodoro Sampaio/SP';
      3554409: CodTOM := '7189'; // Terra Roxa/SP';
      3554508: CodTOM := '7191'; // Tiete/SP';
      3554607: CodTOM := '7193'; // Timburi/SP';
      3554656: CodTOM := '3227'; // Torre De Pedra/SP';
      3554706: CodTOM := '7195'; // Torrinha/SP';
      3554755: CodTOM := '0826'; // Trabiju/SP';
      3554805: CodTOM := '7197'; // Tremembe/SP';
      3554904: CodTOM := '7199'; // Tres Fronteiras/SP';
      3554953: CodTOM := '2955'; // Tuiuti/SP';
      3555000: CodTOM := '7201'; // Tupa/SP';
      3555109: CodTOM := '7203'; // Tupi Paulista/SP';
      3555208: CodTOM := '7205'; // Turiuba/SP';
      3555307: CodTOM := '7207'; // Turmalina/SP';
      3555356: CodTOM := '2971'; // Ubarana/SP';
      3555406: CodTOM := '7209'; // Ubatuba/SP';
      3555505: CodTOM := '7211'; // Ubirajara/SP';
      3555604: CodTOM := '7213'; // Uchoa/SP';
      3555703: CodTOM := '7215'; // Uniao Paulista/SP';
      3555802: CodTOM := '7217'; // Urania/SP';
      3555901: CodTOM := '7219'; // Uru/SP';
      3556008: CodTOM := '7221'; // Urupes/SP';
      3556107: CodTOM := '7223'; // Valentim Gentil/SP';
      3556206: CodTOM := '7225'; // Valinhos/SP';
      3556305: CodTOM := '7227'; // Valparaiso/SP';
      3556354: CodTOM := '2957'; // Vargem/SP';
      3556404: CodTOM := '7231'; // Vargem Grande Do Sul/SP';
      3556453: CodTOM := '7273'; // Vargem Grande Paulista/SP';
      3556503: CodTOM := '7233'; // Varzea Paulista/SP';
      3556602: CodTOM := '7235'; // Vera Cruz/SP';
      3556701: CodTOM := '7237'; // Vinhedo/SP';
      3556800: CodTOM := '7239'; // Viradouro/SP';
      3556909: CodTOM := '7241'; // Vista Alegre Do Alto/SP';
      3556958: CodTOM := '0828'; // Vitoria Brasil/SP';
      3557006: CodTOM := '7243'; // Votorantim/SP';
      3557105: CodTOM := '7245'; // Votuporanga/SP';
      3557154: CodTOM := '2973'; // Zacarias/SP';
      3557204: CodTOM := '6337'; // Chavantes/SP';
      3557303: CodTOM := '2959'; // Estiva Gerbi/SP';
   end;
 end;

 procedure P41;
 begin
   case ACodigo of
      4100103: CodTOM := '7401'; // Abatia/PR';
      4100202: CodTOM := '7403'; // Adrianopolis/PR';
      4100301: CodTOM := '7405'; // Agudos Do Sul/PR';
      4100400: CodTOM := '7407'; // Almirante Tamandare/PR';
      4100459: CodTOM := '8455'; // Altamira Do Parana/PR';
      4100509: CodTOM := '7951'; // Altonia/PR';
      4100608: CodTOM := '7409'; // Alto Parana/PR';
      4100707: CodTOM := '7411'; // Alto Piquiri/PR';
      4100806: CodTOM := '7413'; // Alvorada Do Sul/PR';
      4100905: CodTOM := '7415'; // Amapora/PR';
      4101002: CodTOM := '7417'; // Ampere/PR';
      4101051: CodTOM := '5463'; // Anahy/PR';
      4101101: CodTOM := '7419'; // Andira/PR';
      4101150: CodTOM := '5509'; // Angulo/PR';
      4101200: CodTOM := '7421'; // Antonina/PR';
      4101309: CodTOM := '7423'; // Antonio Olinto/PR';
      4101408: CodTOM := '7425'; // Apucarana/PR';
      4101507: CodTOM := '7427'; // Arapongas/PR';
      4101606: CodTOM := '7429'; // Arapoti/PR';
      4101655: CodTOM := '0830'; // Arapua/PR';
      4101705: CodTOM := '7431'; // Araruna/PR';
      4101804: CodTOM := '7435'; // Araucaria/PR';
      4101853: CodTOM := '0832'; // Ariranha Do Ivai/PR';
      4101903: CodTOM := '7437'; // Assai/PR';
      4102000: CodTOM := '7953'; // Assis Chateaubriand/PR';
      4102109: CodTOM := '7439'; // Astorga/PR';
      4102208: CodTOM := '7441'; // Atalaia/PR';
      4102307: CodTOM := '7443'; // Balsa Nova/PR';
      4102406: CodTOM := '7445'; // Bandeirantes/PR';
      4102505: CodTOM := '7447'; // Barbosa Ferraz/PR';
      4102604: CodTOM := '7449'; // Barracao/PR';
      4102703: CodTOM := '7451'; // Barra Do Jacare/PR';
      4102752: CodTOM := '0834'; // Bela Vista Da Caroba/PR';
      4102802: CodTOM := '7453'; // Bela Vista Do Paraiso/PR';
      4102901: CodTOM := '7455'; // Bituruna/PR';
      4103008: CodTOM := '7457'; // Boa Esperanca/PR';
      4103024: CodTOM := '5471'; // Boa Esperanca Do Iguacu/PR';
      4103040: CodTOM := '0836'; // Boa Ventura De Sao Roque/PR';
      4103057: CodTOM := '7981'; // Boa Vista Da Aparecida/PR';
      4103107: CodTOM := '7459'; // Bocaiuva Do Sul/PR';
      4103156: CodTOM := '0838'; // Bom Jesus Do Sul/PR';
      4103206: CodTOM := '7461'; // Bom Sucesso/PR';
      4103222: CodTOM := '9979'; // Bom Sucesso Do Sul/PR';
      4103305: CodTOM := '7463'; // Borrazopolis/PR';
      4103354: CodTOM := '7983'; // Braganey/PR';
      4103370: CodTOM := '5521'; // Brasilandia Do Sul/PR';
      4103404: CodTOM := '7465'; // Cafeara/PR';
      4103453: CodTOM := '7985'; // Cafelandia/PR';
      4103479: CodTOM := '5491'; // Cafezal Do Sul/PR';
      4103503: CodTOM := '7467'; // California/PR';
      4103602: CodTOM := '7469'; // Cambara/PR';
      4103701: CodTOM := '7471'; // Cambe/PR';
      4103800: CodTOM := '7473'; // Cambira/PR';
      4103909: CodTOM := '7475'; // Campina Da Lagoa/PR';
      4103958: CodTOM := '0840'; // Campina Do Simao/PR';
      4104006: CodTOM := '7477'; // Campina Grande Do Sul/PR';
      4104055: CodTOM := '8475'; // Campo Bonito/PR';
      4104105: CodTOM := '7479'; // Campo Do Tenente/PR';
      4104204: CodTOM := '7481'; // Campo Largo/PR';
      4104253: CodTOM := '0842'; // Campo Magro/PR';
      4104303: CodTOM := '7483'; // Campo Mourao/PR';
      4104402: CodTOM := '7485'; // Candido De Abreu/PR';
      4104428: CodTOM := '5499'; // Candoi/PR';
      4104451: CodTOM := '8451'; // Cantagalo/PR';
      4104501: CodTOM := '7487'; // Capanema/PR';
      4104600: CodTOM := '7489'; // Capitao Leonidas Marques/PR';
      4104659: CodTOM := '0844'; // Carambei/PR';
      4104709: CodTOM := '7491'; // Carlopolis/PR';
      4104808: CodTOM := '7493'; // Cascavel/PR';
      4104907: CodTOM := '7495'; // Castro/PR';
      4105003: CodTOM := '7497'; // Catanduvas/PR';
      4105102: CodTOM := '7499'; // Centenario Do Sul/PR';
      4105201: CodTOM := '7501'; // Cerro Azul/PR';
      4105300: CodTOM := '7957'; // Ceu Azul/PR';
      4105409: CodTOM := '7503'; // Chopinzinho/PR';
      4105508: CodTOM := '7505'; // Cianorte/PR';
      4105607: CodTOM := '7507'; // Cidade Gaucha/PR';
      4105706: CodTOM := '7509'; // Clevelandia/PR';
      4105805: CodTOM := '7513'; // Colombo/PR';
      4105904: CodTOM := '7515'; // Colorado/PR';
      4106001: CodTOM := '7517'; // Congonhinhas/PR';
      4106100: CodTOM := '7519'; // Conselheiro Mairinck/PR';
      4106209: CodTOM := '7521'; // Contenda/PR';
      4106308: CodTOM := '7523'; // Corbelia/PR';
      4106407: CodTOM := '7525'; // Cornelio Procopio/PR';
      4106456: CodTOM := '0846'; // Coronel Domingos Soares/PR';
      4106506: CodTOM := '7527'; // Coronel Vivida/PR';
      4106555: CodTOM := '8479'; // Corumbatai Do Sul/PR';
      4106571: CodTOM := '5473'; // Cruzeiro Do Iguacu/PR';
      4106605: CodTOM := '7529'; // Cruzeiro Do Oeste/PR';
      4106704: CodTOM := '7531'; // Cruzeiro Do Sul/PR';
      4106803: CodTOM := '7533'; // Cruz Machado/PR';
      4106852: CodTOM := '0848'; // Cruzmaltina/PR';
      4106902: CodTOM := '7535'; // Curitiba/PR';
      4107009: CodTOM := '7537'; // Curiuva/PR';
      4107108: CodTOM := '7539'; // Diamante Do Norte/PR';
      4107124: CodTOM := '5465'; // Diamante Do Sul/PR';
      4107157: CodTOM := '9915'; // Diamante D Oeste/PR';
      4107207: CodTOM := '7541'; // Dois Vizinhos/PR';
      4107256: CodTOM := '8465'; // Douradina/PR';
      4107306: CodTOM := '7543'; // Doutor Camargo/PR';
      4107405: CodTOM := '7545'; // Eneas Marques/PR';
      4107504: CodTOM := '7547'; // Engenheiro Beltrao/PR';
      4107520: CodTOM := '0850'; // Esperanca Nova/PR';
      4107538: CodTOM := '5529'; // Entre Rios Do Oeste/PR';
      4107546: CodTOM := '0852'; // Espigao Alto Do Iguacu/PR';
      4107553: CodTOM := '5511'; // Farol/PR';
      4107603: CodTOM := '7549'; // Faxinal/PR';
      4107652: CodTOM := '9983'; // Fazenda Rio Grande/PR';
      4107702: CodTOM := '7551'; // Fenix/PR';
      4107736: CodTOM := '0854'; // Fernandes Pinheiro/PR';
      4107751: CodTOM := '8457'; // Figueira/PR';
      4107801: CodTOM := '7553'; // Florai/PR';
      4107850: CodTOM := '5475'; // Flor Da Serra Do Sul/PR';
      4107900: CodTOM := '7555'; // Floresta/PR';
      4108007: CodTOM := '7557'; // Florestopolis/PR';
      4108106: CodTOM := '7559'; // Florida/PR';
      4108205: CodTOM := '7561'; // Formosa Do Oeste/PR';
      4108304: CodTOM := '7563'; // Foz Do Iguacu/PR';
      4108320: CodTOM := '7977'; // Francisco Alves/PR';
      4108403: CodTOM := '7565'; // Francisco Beltrao/PR';
      4108452: CodTOM := '0856'; // Foz Do Jordao/PR';
      4108502: CodTOM := '7567'; // General Carneiro/PR';
      4108551: CodTOM := '9947'; // Godoy Moreira/PR';
      4108601: CodTOM := '7569'; // Goioere/PR';
      4108650: CodTOM := '0858'; // Goioxim/PR';
      4108700: CodTOM := '7959'; // Grandes Rios/PR';
      4108809: CodTOM := '7571'; // Guaira/PR';
      4108908: CodTOM := '7573'; // Guairaca/PR';
      4108957: CodTOM := '0860'; // Guamiranga/PR';
      4109005: CodTOM := '7575'; // Guapirama/PR';
      4109104: CodTOM := '7577'; // Guaporema/PR';
      4109203: CodTOM := '7579'; // Guaraci/PR';
      4109302: CodTOM := '7581'; // Guaraniacu/PR';
      4109401: CodTOM := '7583'; // Guarapuava/PR';
      4109500: CodTOM := '7585'; // Guaraquecaba/PR';
      4109609: CodTOM := '7587'; // Guaratuba/PR';
      4109658: CodTOM := '9981'; // Honorio Serpa/PR';
      4109708: CodTOM := '7589'; // Ibaiti/PR';
      4109757: CodTOM := '9949'; // Ibema/PR';
      4109807: CodTOM := '7591'; // Ibipora/PR';
      4109906: CodTOM := '7593'; // Icaraima/PR';
      4110003: CodTOM := '7595'; // Iguaracu/PR';
      4110052: CodTOM := '5467'; // Iguatu/PR';
      4110078: CodTOM := '0862'; // Imbau/PR';
      4110102: CodTOM := '7597'; // Imbituva/PR';
      4110201: CodTOM := '7599'; // Inacio Martins/PR';
      4110300: CodTOM := '7601'; // Inaja/PR';
      4110409: CodTOM := '7961'; // Indianopolis/PR';
      4110508: CodTOM := '7603'; // Ipiranga/PR';
      4110607: CodTOM := '7605'; // Ipora/PR';
      4110656: CodTOM := '5485'; // Iracema Do Oeste/PR';
      4110706: CodTOM := '7607'; // Irati/PR';
      4110805: CodTOM := '7609'; // Iretama/PR';
      4110904: CodTOM := '7611'; // Itaguaje/PR';
      4110953: CodTOM := '5525'; // Itaipulandia/PR';
      4111001: CodTOM := '7613'; // Itambaraca/PR';
      4111100: CodTOM := '7615'; // Itambe/PR';
      4111209: CodTOM := '7617'; // Itapejara D Oeste/PR';
      4111258: CodTOM := '5451'; // Itaperucu/PR';
      4111308: CodTOM := '7619'; // Itauna Do Sul/PR';
      4111407: CodTOM := '7621'; // Ivai/PR';
      4111506: CodTOM := '7623'; // Ivaipora/PR';
      4111555: CodTOM := '9955'; // Ivate/PR';
      4111605: CodTOM := '7625'; // Ivatuba/PR';
      4111704: CodTOM := '7627'; // Jaboti/PR';
      4111803: CodTOM := '7629'; // Jacarezinho/PR';
      4111902: CodTOM := '7631'; // Jaguapita/PR';
      4112009: CodTOM := '7633'; // Jaguariaiva/PR';
      4112108: CodTOM := '7635'; // Jandaia Do Sul/PR';
      4112207: CodTOM := '7637'; // Janiopolis/PR';
      4112306: CodTOM := '7639'; // Japira/PR';
      4112405: CodTOM := '7641'; // Japura/PR';
      4112504: CodTOM := '7643'; // Jardim Alegre/PR';
      4112603: CodTOM := '7645'; // Jardim Olinda/PR';
      4112702: CodTOM := '7647'; // Jataizinho/PR';
      4112751: CodTOM := '7997'; // Jesuitas/PR';
      4112801: CodTOM := '7649'; // Joaquim Tavora/PR';
      4112900: CodTOM := '7651'; // Jundiai Do Sul/PR';
      4112959: CodTOM := '8463'; // Juranda/PR';
      4113007: CodTOM := '7653'; // Jussara/PR';
      4113106: CodTOM := '7655'; // Kalore/PR';
      4113205: CodTOM := '7657'; // Lapa/PR';
      4113254: CodTOM := '5501'; // Laranjal/PR';
      4113304: CodTOM := '7659'; // Laranjeiras Do Sul/PR';
      4113403: CodTOM := '7661'; // Leopolis/PR';
      4113429: CodTOM := '5507'; // Lidianopolis/PR';
      4113452: CodTOM := '9959'; // Lindoeste/PR';
      4113502: CodTOM := '7663'; // Loanda/PR';
      4113601: CodTOM := '7665'; // Lobato/PR';
      4113700: CodTOM := '7667'; // Londrina/PR';
      4113734: CodTOM := '8481'; // Luiziana/PR';
      4113759: CodTOM := '8459'; // Lunardelli/PR';
      4113809: CodTOM := '7669'; // Lupionopolis/PR';
      4113908: CodTOM := '7671'; // Mallet/PR';
      4114005: CodTOM := '7673'; // Mambore/PR';
      4114104: CodTOM := '7675'; // Mandaguacu/PR';
      4114203: CodTOM := '7677'; // Mandaguari/PR';
      4114302: CodTOM := '7679'; // Mandirituba/PR';
      4114351: CodTOM := '0864'; // Manfrinopolis/PR';
      4114401: CodTOM := '7511'; // Mangueirinha/PR';
      4114500: CodTOM := '7681'; // Manoel Ribas/PR';
      4114609: CodTOM := '7683'; // Marechal Candido Rondon/PR';
      4114708: CodTOM := '7685'; // Maria Helena/PR';
      4114807: CodTOM := '7687'; // Marialva/PR';
      4114906: CodTOM := '7433'; // Marilandia Do Sul/PR';
      4115002: CodTOM := '7975'; // Marilena/PR';
      4115101: CodTOM := '7689'; // Mariluz/PR';
      4115200: CodTOM := '7691'; // Maringa/PR';
      4115309: CodTOM := '7693'; // Mariopolis/PR';
      4115358: CodTOM := '5487'; // Maripa/PR';
      4115408: CodTOM := '7695'; // Marmeleiro/PR';
      4115457: CodTOM := '0866'; // Marquinho/PR';
      4115507: CodTOM := '7697'; // Marumbi/PR';
      4115606: CodTOM := '7699'; // Matelandia/PR';
      4115705: CodTOM := '7963'; // Matinhos/PR';
      4115739: CodTOM := '5503'; // Mato Rico/PR';
      4115754: CodTOM := '5459'; // Maua Da Serra/PR';
      4115804: CodTOM := '7701'; // Medianeira/PR';
      4115853: CodTOM := '5531'; // Mercedes/PR';
      4115903: CodTOM := '7703'; // Mirador/PR';
      4116000: CodTOM := '7705'; // Miraselva/PR';
      4116059: CodTOM := '8469'; // Missal/PR';
      4116109: CodTOM := '7707'; // Moreira Sales/PR';
      4116208: CodTOM := '7709'; // Morretes/PR';
      4116307: CodTOM := '7711'; // Munhoz De Melo/PR';
      4116406: CodTOM := '7713'; // Nossa Senhora Das Gracas/PR';
      4116505: CodTOM := '7715'; // Nova Alianca Do Ivai/PR';
      4116604: CodTOM := '7717'; // Nova America Da Colina/PR';
      4116703: CodTOM := '7965'; // Nova Aurora/PR';
      4116802: CodTOM := '7719'; // Nova Cantu/PR';
      4116901: CodTOM := '7721'; // Nova Esperanca/PR';
      4116950: CodTOM := '5477'; // Nova Esperanca Do Sudoeste/PR';
      4117008: CodTOM := '7723'; // Nova Fatima/PR';
      4117057: CodTOM := '5479'; // Nova Laranjeiras/PR';
      4117107: CodTOM := '7725'; // Nova Londrina/PR';
      4117206: CodTOM := '7967'; // Nova Olimpia/PR';
      4117214: CodTOM := '5457'; // Nova Santa Barbara/PR';
      4117222: CodTOM := '7979'; // Nova Santa Rosa/PR';
      4117255: CodTOM := '7995'; // Nova Prata Do Iguacu/PR';
      4117271: CodTOM := '9913'; // Nova Tebas/PR';
      4117297: CodTOM := '5517'; // Novo Itacolomi/PR';
      4117305: CodTOM := '7727'; // Ortigueira/PR';
      4117404: CodTOM := '7729'; // Ourizona/PR';
      4117453: CodTOM := '9965'; // Ouro Verde Do Oeste/PR';
      4117503: CodTOM := '7731'; // Paicandu/PR';
      4117602: CodTOM := '7733'; // Palmas/PR';
      4117701: CodTOM := '7735'; // Palmeira/PR';
      4117800: CodTOM := '7737'; // Palmital/PR';
      4117909: CodTOM := '7739'; // Palotina/PR';
      4118006: CodTOM := '7741'; // Paraiso Do Norte/PR';
      4118105: CodTOM := '7743'; // Paranacity/PR';
      4118204: CodTOM := '7745'; // Paranagua/PR';
      4118303: CodTOM := '7747'; // Paranapoema/PR';
      4118402: CodTOM := '7749'; // Paranavai/PR';
      4118451: CodTOM := '5533'; // Pato Bragado/PR';
      4118501: CodTOM := '7751'; // Pato Branco/PR';
      4118600: CodTOM := '7753'; // Paula Freitas/PR';
      4118709: CodTOM := '7755'; // Paulo Frontin/PR';
      4118808: CodTOM := '7757'; // Peabiru/PR';
      4118857: CodTOM := '0868'; // Perobal/PR';
      4118907: CodTOM := '7969'; // Perola/PR';
      4119004: CodTOM := '7759'; // Perola D Oeste/PR';
      4119103: CodTOM := '7761'; // Pien/PR';
      4119152: CodTOM := '5453'; // Pinhais/PR';
      4119202: CodTOM := '7763'; // Pinhalao/PR';
      4119251: CodTOM := '5495'; // Pinhal De Sao Bento/PR';
      4119301: CodTOM := '7765'; // Pinhao/PR';
      4119400: CodTOM := '7767'; // Pirai Do Sul/PR';
      4119509: CodTOM := '7769'; // Piraquara/PR';
      4119608: CodTOM := '7771'; // Pitanga/PR';
      4119657: CodTOM := '5461'; // Pitangueiras/PR';
      4119707: CodTOM := '7773'; // Planaltina Do Parana/PR';
      4119806: CodTOM := '7775'; // Planalto/PR';
      4119905: CodTOM := '7777'; // Ponta Grossa/PR';
      4119954: CodTOM := '0870'; // Pontal Do Parana/PR';
      4120002: CodTOM := '7779'; // Porecatu/PR';
      4120101: CodTOM := '7781'; // Porto Amazonas/PR';
      4120150: CodTOM := '0872'; // Porto Barreiro/PR';
      4120200: CodTOM := '7783'; // Porto Rico/PR';
      4120309: CodTOM := '7785'; // Porto Vitoria/PR';
      4120333: CodTOM := '0874'; // Prado Ferreira/PR';
      4120358: CodTOM := '7991'; // Pranchita/PR';
      4120408: CodTOM := '7787'; // Presidente Castelo Branco/PR';
      4120507: CodTOM := '7789'; // Primeiro De Maio/PR';
      4120606: CodTOM := '7791'; // Prudentopolis/PR';
      4120655: CodTOM := '0876'; // Quarto Centenario/PR';
      4120705: CodTOM := '7793'; // Quatigua/PR';
      4120804: CodTOM := '7795'; // Quatro Barras/PR';
      4120853: CodTOM := '5535'; // Quatro Pontes/PR';
      4120903: CodTOM := '7955'; // Quedas Do Iguacu/PR';
      4121000: CodTOM := '7797'; // Querencia Do Norte/PR';
      4121109: CodTOM := '7799'; // Quinta Do Sol/PR';
      4121208: CodTOM := '7801'; // Quitandinha/PR';
      4121257: CodTOM := '5527'; // Ramilandia/PR';
      4121307: CodTOM := '7803'; // Rancho Alegre/PR';
      4121356: CodTOM := '5513'; // Rancho Alegre D Oeste/PR';
      4121406: CodTOM := '7805'; // Realeza/PR';
      4121505: CodTOM := '7807'; // Reboucas/PR';
      4121604: CodTOM := '7809'; // Renascenca/PR';
      4121703: CodTOM := '7811'; // Reserva/PR';
      4121752: CodTOM := '0878'; // Reserva Do Iguacu/PR';
      4121802: CodTOM := '7813'; // Ribeirao Claro/PR';
      4121901: CodTOM := '7815'; // Ribeirao Do Pinhal/PR';
      4122008: CodTOM := '7817'; // Rio Azul/PR';
      4122107: CodTOM := '7819'; // Rio Bom/PR';
      4122156: CodTOM := '5481'; // Rio Bonito Do Iguacu/PR';
      4122172: CodTOM := '0880'; // Rio Branco Do Ivai/PR';
      4122206: CodTOM := '7821'; // Rio Branco Do Sul/PR';
      4122305: CodTOM := '7823'; // Rio Negro/PR';
      4122404: CodTOM := '7825'; // Rolandia/PR';
      4122503: CodTOM := '7827'; // Roncador/PR';
      4122602: CodTOM := '7829'; // Rondon/PR';
      4122651: CodTOM := '8473'; // Rosario Do Ivai/PR';
      4122701: CodTOM := '7831'; // Sabaudia/PR';
      4122800: CodTOM := '7833'; // Salgado Filho/PR';
      4122909: CodTOM := '7835'; // Salto Do Itarare/PR';
      4123006: CodTOM := '7837'; // Salto Do Lontra/PR';
      4123105: CodTOM := '7839'; // Santa Amelia/PR';
      4123204: CodTOM := '7841'; // Santa Cecilia Do Pavao/PR';
      4123303: CodTOM := '7843'; // Santa Cruz De Monte Castelo/PR';
      4123402: CodTOM := '7845'; // Santa Fe/PR';
      4123501: CodTOM := '7971'; // Santa Helena/PR';
      4123600: CodTOM := '7847'; // Santa Ines/PR';
      4123709: CodTOM := '7849'; // Santa Isabel Do Ivai/PR';
      4123808: CodTOM := '7851'; // Santa Izabel Do Oeste/PR';
      4123824: CodTOM := '5469'; // Santa Lucia/PR';
      4123857: CodTOM := '5505'; // Santa Maria Do Oeste/PR';
      4123907: CodTOM := '7853'; // Santa Mariana/PR';
      4123956: CodTOM := '5519'; // Santa Monica/PR';
      4124004: CodTOM := '7855'; // Santana Do Itarare/PR';
      4124020: CodTOM := '9969'; // Santa Tereza Do Oeste/PR';
      4124053: CodTOM := '8467'; // Santa Terezinha De Itaipu/PR';
      4124103: CodTOM := '7859'; // Santo Antonio Da Platina/PR';
      4124202: CodTOM := '7861'; // Santo Antonio Do Caiua/PR';
      4124301: CodTOM := '7863'; // Santo Antonio Do Paraiso/PR';
      4124400: CodTOM := '7857'; // Santo Antonio Do Sudoeste/PR';
      4124509: CodTOM := '7865'; // Santo Inacio/PR';
      4124608: CodTOM := '7867'; // Sao Carlos Do Ivai/PR';
      4124707: CodTOM := '7869'; // Sao Jeronimo Da Serra/PR';
      4124806: CodTOM := '7871'; // Sao Joao/PR';
      4124905: CodTOM := '7873'; // Sao Joao Do Caiua/PR';
      4125001: CodTOM := '7875'; // Sao Joao Do Ivai/PR';
      4125100: CodTOM := '7877'; // Sao Joao Do Triunfo/PR';
      4125209: CodTOM := '7881'; // Sao Jorge D Oeste/PR';
      4125308: CodTOM := '7879'; // Sao Jorge Do Ivai/PR';
      4125357: CodTOM := '7999'; // Sao Jorge Do Patrocinio/PR';
      4125407: CodTOM := '7883'; // Sao Jose Da Boa Vista/PR';
      4125456: CodTOM := '8471'; // Sao Jose Das Palmeiras/PR';
      4125506: CodTOM := '7885'; // Sao Jose Dos Pinhais/PR';
      4125555: CodTOM := '5515'; // Sao Manoel Do Parana/PR';
      4125605: CodTOM := '7887'; // Sao Mateus Do Sul/PR';
      4125704: CodTOM := '7889'; // Sao Miguel Do Iguacu/PR';
      4125753: CodTOM := '5489'; // Sao Pedro Do Iguacu/PR';
      4125803: CodTOM := '7891'; // Sao Pedro Do Ivai/PR';
      4125902: CodTOM := '7893'; // Sao Pedro Do Parana/PR';
      4126009: CodTOM := '7895'; // Sao Sebastiao Da Amoreira/PR';
      4126108: CodTOM := '7897'; // Sao Tome/PR';
      4126207: CodTOM := '7899'; // Sapopema/PR';
      4126256: CodTOM := '8461'; // Sarandi/PR';
      4126272: CodTOM := '5493'; // Saudade Do Iguacu/PR';
      4126306: CodTOM := '7901'; // Senges/PR';
      4126355: CodTOM := '0882'; // Serranopolis Do Iguacu/PR';
      4126405: CodTOM := '7903'; // Sertaneja/PR';
      4126504: CodTOM := '7905'; // Sertanopolis/PR';
      4126603: CodTOM := '7907'; // Siqueira Campos/PR';
      4126652: CodTOM := '8477'; // Sulina/PR';
      4126678: CodTOM := '0884'; // Tamarana/PR';
      4126702: CodTOM := '7909'; // Tamboara/PR';
      4126801: CodTOM := '7911'; // Tapejara/PR';
      4126900: CodTOM := '7973'; // Tapira/PR';
      4127007: CodTOM := '7913'; // Teixeira Soares/PR';
      4127106: CodTOM := '7915'; // Telemaco Borba/PR';
      4127205: CodTOM := '7917'; // Terra Boa/PR';
      4127304: CodTOM := '7919'; // Terra Rica/PR';
      4127403: CodTOM := '7921'; // Terra Roxa/PR';
      4127502: CodTOM := '7923'; // Tibagi/PR';
      4127601: CodTOM := '7925'; // Tijucas Do Sul/PR';
      4127700: CodTOM := '7927'; // Toledo/PR';
      4127809: CodTOM := '7929'; // Tomazina/PR';
      4127858: CodTOM := '7987'; // Tres Barras Do Parana/PR';
      4127882: CodTOM := '5455'; // Tunas Do Parana/PR';
      4127908: CodTOM := '7931'; // Tuneiras Do Oeste/PR';
      4127957: CodTOM := '7993'; // Tupassi/PR';
      4127965: CodTOM := '8453'; // Turvo/PR';
      4128005: CodTOM := '7933'; // Ubirata/PR';
      4128104: CodTOM := '7935'; // Umuarama/PR';
      4128203: CodTOM := '7937'; // Uniao Da Vitoria/PR';
      4128302: CodTOM := '7939'; // Uniflor/PR';
      4128401: CodTOM := '7941'; // Urai/PR';
      4128500: CodTOM := '7943'; // Wenceslau Braz/PR';
      4128534: CodTOM := '5497'; // Ventania/PR';
      4128559: CodTOM := '7989'; // Vera Cruz Do Oeste/PR';
      4128609: CodTOM := '7945'; // Vere/PR';
      4128625: CodTOM := '5523'; // Alto Paraiso/PR';
      4128633: CodTOM := '5449'; // Doutor Ulysses/PR';
      4128658: CodTOM := '5483'; // Virmond/PR';
      4128708: CodTOM := '7947'; // Vitorino/PR';
      4128807: CodTOM := '7949'; // Xambre/PR';
   end;
 end;

 procedure P42;
 begin
   case ACodigo of
      4200051: CodTOM := '9939'; // Abdon Batista/SC';
      4200101: CodTOM := '8001'; // Abelardo Luz/SC';
      4200200: CodTOM := '8003'; // Agrolandia/SC';
      4200309: CodTOM := '8005'; // Agronomica/SC';
      4200408: CodTOM := '8007'; // Agua Doce/SC';
      4200507: CodTOM := '8009'; // Aguas De Chapeco/SC';
      4200556: CodTOM := '5577'; // Aguas Frias/SC';
      4200606: CodTOM := '8011'; // Aguas Mornas/SC';
      4200705: CodTOM := '8013'; // Alfredo Wagner/SC';
      4200754: CodTOM := '0886'; // Alto Bela Vista/SC';
      4200804: CodTOM := '8015'; // Anchieta/SC';
      4200903: CodTOM := '8017'; // Angelina/SC';
      4201000: CodTOM := '8019'; // Anita Garibaldi/SC';
      4201109: CodTOM := '8021'; // Anitapolis/SC';
      4201208: CodTOM := '8023'; // Antonio Carlos/SC';
      4201257: CodTOM := '9941'; // Apiuna/SC';
      4201273: CodTOM := '5597'; // Arabuta/SC';
      4201307: CodTOM := '8025'; // Araquari/SC';
      4201406: CodTOM := '8027'; // Ararangua/SC';
      4201505: CodTOM := '8029'; // Armazem/SC';
      4201604: CodTOM := '8031'; // Arroio Trinta/SC';
      4201653: CodTOM := '5599'; // Arvoredo/SC';
      4201703: CodTOM := '8033'; // Ascurra/SC';
      4201802: CodTOM := '8035'; // Atalanta/SC';
      4201901: CodTOM := '8037'; // Aurora/SC';
      4201950: CodTOM := '0888'; // Balneario Arroio Do Silva/SC';
      4202008: CodTOM := '8039'; // Balneario Camboriu/SC';
      4202057: CodTOM := '5549'; // Balneario Barra Do Sul/SC';
      4202073: CodTOM := '0890'; // Balneario Gaivota/SC';
      4220000: CodTOM := '1192'; // Balneario Rincão/SC';
      4202081: CodTOM := '0892'; // Bandeirante/SC';
      4202099: CodTOM := '0894'; // Barra Bonita/SC';
      4202107: CodTOM := '8041'; // Barra Velha/SC';
      4202131: CodTOM := '0896'; // Bela Vista Do Toldo/SC';
      4202156: CodTOM := '5745'; // Belmonte/SC';
      4202206: CodTOM := '8043'; // Benedito Novo/SC';
      4202305: CodTOM := '8045'; // Biguacu/SC';
      4202404: CodTOM := '8047'; // Blumenau/SC';
      4202438: CodTOM := '0898'; // Bocaina Do Sul/SC';
      4202453: CodTOM := '5537'; // Bombinhas/SC';
      4202503: CodTOM := '8389'; // Bom Jardim Da Serra/SC';
      4202537: CodTOM := '0900'; // Bom Jesus/SC';
      4202578: CodTOM := '0902'; // Bom Jesus Do Oeste/SC';
      4202602: CodTOM := '8049'; // Bom Retiro/SC';
      4202701: CodTOM := '8051'; // Botuvera/SC';
      4202800: CodTOM := '8053'; // Braco Do Norte/SC';
      4202859: CodTOM := '5557'; // Braco Do Trombudo/SC';
      4202875: CodTOM := '0904'; // Brunopolis/SC';
      4202909: CodTOM := '8055'; // Brusque/SC';
      4203006: CodTOM := '8057'; // Cacador/SC';
      4203105: CodTOM := '8059'; // Caibi/SC';
      4203154: CodTOM := '5553'; // Calmon/SC';
      4203204: CodTOM := '8061'; // Camboriu/SC';
      4203253: CodTOM := '0906'; // Capao Alto/SC';
      4203303: CodTOM := '8063'; // Campo Alegre/SC';
      4203402: CodTOM := '8065'; // Campo Belo Do Sul/SC';
      4203501: CodTOM := '8067'; // Campo Ere/SC';
      4203600: CodTOM := '8069'; // Campos Novos/SC';
      4203709: CodTOM := '8071'; // Canelinha/SC';
      4203808: CodTOM := '8073'; // Canoinhas/SC';
      4203907: CodTOM := '8075'; // Capinzal/SC';
      4203956: CodTOM := '5545'; // Capivari De Baixo/SC';
      4204004: CodTOM := '8077'; // Catanduvas/SC';
      4204103: CodTOM := '8079'; // Caxambu Do Sul/SC';
      4204152: CodTOM := '9943'; // Celso Ramos/SC';
      4204178: CodTOM := '5567'; // Cerro Negro/SC';
      4204194: CodTOM := '0908'; // Chapadao Do Lageado/SC';
      4204202: CodTOM := '8081'; // Chapeco/SC';
      4204251: CodTOM := '5543'; // Cocal Do Sul/SC';
      4204301: CodTOM := '8083'; // Concordia/SC';
      4204350: CodTOM := '5579'; // Cordilheira Alta/SC';
      4204400: CodTOM := '8085'; // Coronel Freitas/SC';
      4204459: CodTOM := '5735'; // Coronel Martins/SC';
      4204509: CodTOM := '8087'; // Corupa/SC';
      4204558: CodTOM := '8395'; // Correia Pinto/SC';
      4204608: CodTOM := '8089'; // Criciuma/SC';
      4204707: CodTOM := '8091'; // Cunha Pora/SC';
      4204756: CodTOM := '0910'; // Cunhatai/SC';
      4204806: CodTOM := '8093'; // Curitibanos/SC';
      4204905: CodTOM := '8095'; // Descanso/SC';
      4205001: CodTOM := '8097'; // Dionisio Cerqueira/SC';
      4205100: CodTOM := '8099'; // Dona Emma/SC';
      4205159: CodTOM := '9945'; // Doutor Pedrinho/SC';
      4205175: CodTOM := '0912'; // Entre Rios/SC';
      4205191: CodTOM := '0914'; // Ermo/SC';
      4205209: CodTOM := '8101'; // Erval Velho/SC';
      4205308: CodTOM := '8103'; // Faxinal Dos Guedes/SC';
      4205357: CodTOM := '0916'; // Flor Do Sertao/SC';
      4205407: CodTOM := '8105'; // Florianopolis/SC';
      4205431: CodTOM := '5581'; // Formosa Do Sul/SC';
      4205456: CodTOM := '0973'; // Forquilhinha/SC';
      4205506: CodTOM := '8107'; // Fraiburgo/SC';
      4205555: CodTOM := '0918'; // Frei Rogerio/SC';
      4205605: CodTOM := '8109'; // Galvao/SC';
      4205704: CodTOM := '8113'; // Garopaba/SC';
      4205803: CodTOM := '8115'; // Garuva/SC';
      4205902: CodTOM := '8117'; // Gaspar/SC';
      4206009: CodTOM := '8111'; // Governador Celso Ramos/SC';
      4206108: CodTOM := '8119'; // Grao Para/SC';
      4206207: CodTOM := '8121'; // Gravatal/SC';
      4206306: CodTOM := '8123'; // Guabiruba/SC';
      4206405: CodTOM := '8125'; // Guaraciaba/SC';
      4206504: CodTOM := '8127'; // Guaramirim/SC';
      4206603: CodTOM := '8129'; // Guaruja Do Sul/SC';
      4206652: CodTOM := '5583'; // Guatambu/SC';
      4206702: CodTOM := '8131'; // Herval D Oeste/SC';
      4206751: CodTOM := '0920'; // Ibiam/SC';
      4206801: CodTOM := '8133'; // Ibicare/SC';
      4206900: CodTOM := '8135'; // Ibirama/SC';
      4207007: CodTOM := '8137'; // Icara/SC';
      4207106: CodTOM := '8139'; // Ilhota/SC';
      4207205: CodTOM := '8141'; // Imarui/SC';
      4207304: CodTOM := '8143'; // Imbituba/SC';
      4207403: CodTOM := '8145'; // Imbuia/SC';
      4207502: CodTOM := '8147'; // Indaial/SC';
      4207577: CodTOM := '0922'; // Iomere/SC';
      4207601: CodTOM := '8149'; // Ipira/SC';
      4207650: CodTOM := '9951'; // Ipora Do Oeste/SC';
      4207684: CodTOM := '5737'; // Ipuacu/SC';
      4207700: CodTOM := '8151'; // Ipumirim/SC';
      4207759: CodTOM := '9953'; // Iraceminha/SC';
      4207809: CodTOM := '8153'; // Irani/SC';
      4207858: CodTOM := '5585'; // Irati/SC';
      4207908: CodTOM := '8155'; // Irineopolis/SC';
      4208005: CodTOM := '8157'; // Ita/SC';
      4208104: CodTOM := '8159'; // Itaiopolis/SC';
      4208203: CodTOM := '8161'; // Itajai/SC';
      4208302: CodTOM := '8163'; // Itapema/SC';
      4208401: CodTOM := '8165'; // Itapiranga/SC';
      4208450: CodTOM := '9985'; // Itapoa/SC';
      4208500: CodTOM := '8167'; // Ituporanga/SC';
      4208609: CodTOM := '8169'; // Jabora/SC';
      4208708: CodTOM := '8171'; // Jacinto Machado/SC';
      4208807: CodTOM := '8173'; // Jaguaruna/SC';
      4208906: CodTOM := '8175'; // Jaragua Do Sul/SC';
      4208955: CodTOM := '5587'; // Jardinopolis/SC';
      4209003: CodTOM := '8177'; // Joacaba/SC';
      4209102: CodTOM := '8179'; // Joinville/SC';
      4209151: CodTOM := '9957'; // Jose Boiteux/SC';
      4209177: CodTOM := '0924'; // Jupia/SC';
      4209201: CodTOM := '8181'; // Lacerdopolis/SC';
      4209300: CodTOM := '8183'; // Lages/SC';
      4209409: CodTOM := '8185'; // Laguna/SC';
      4209458: CodTOM := '5739'; // Lajeado Grande/SC';
      4209508: CodTOM := '8187'; // Laurentino/SC';
      4209607: CodTOM := '8189'; // Lauro Muller/SC';
      4209706: CodTOM := '8191'; // Lebon Regis/SC';
      4209805: CodTOM := '8193'; // Leoberto Leal/SC';
      4209854: CodTOM := '9961'; // Lindoia Do Sul/SC';
      4209904: CodTOM := '8195'; // Lontras/SC';
      4210001: CodTOM := '8197'; // Luiz Alves/SC';
      4210035: CodTOM := '0926'; // Luzerna/SC';
      4210050: CodTOM := '5575'; // Macieira/SC';
      4210100: CodTOM := '8199'; // Mafra/SC';
      4210209: CodTOM := '8201'; // Major Gercino/SC';
      4210308: CodTOM := '8203'; // Major Vieira/SC';
      4210407: CodTOM := '8391'; // Maracaja/SC';
      4210506: CodTOM := '8205'; // Maravilha/SC';
      4210555: CodTOM := '9963'; // Marema/SC';
      4210605: CodTOM := '8207'; // Massaranduba/SC';
      4210704: CodTOM := '8209'; // Matos Costa/SC';
      4210803: CodTOM := '8211'; // Meleiro/SC';
      4210852: CodTOM := '5559'; // Mirim Doce/SC';
      4210902: CodTOM := '8213'; // Modelo/SC';
      4211009: CodTOM := '8215'; // Mondai/SC';
      4211058: CodTOM := '5561'; // Monte Carlo/SC';
      4211108: CodTOM := '8217'; // Monte Castelo/SC';
      4211207: CodTOM := '8219'; // Morro Da Fumaca/SC';
      4211256: CodTOM := '5539'; // Morro Grande/SC';
      4211306: CodTOM := '8221'; // Navegantes/SC';
      4211405: CodTOM := '8223'; // Nova Erechim/SC';
      4211454: CodTOM := '5589'; // Nova Itaberaba/SC';
      4211504: CodTOM := '8225'; // Nova Trento/SC';
      4211603: CodTOM := '8227'; // Nova Veneza/SC';
      4211652: CodTOM := '5591'; // Novo Horizonte/SC';
      4211702: CodTOM := '8229'; // Orleans/SC';
      4211751: CodTOM := '8397'; // Otacilio Costa/SC';
      4211801: CodTOM := '8231'; // Ouro/SC';
      4211850: CodTOM := '5741'; // Ouro Verde/SC';
      4211876: CodTOM := '0928'; // Paial/SC';
      4211892: CodTOM := '0930'; // Painel/SC';
      4211900: CodTOM := '8233'; // Palhoca/SC';
      4212007: CodTOM := '8235'; // Palma Sola/SC';
      4212056: CodTOM := '0932'; // Palmeira/SC';
      4212106: CodTOM := '8237'; // Palmitos/SC';
      4212205: CodTOM := '8239'; // Papanduva/SC';
      4212239: CodTOM := '5747'; // Paraiso/SC';
      4212254: CodTOM := '5541'; // Passo De Torres/SC';
      4212270: CodTOM := '5743'; // Passos Maia/SC';
      4212304: CodTOM := '8241'; // Paulo Lopes/SC';
      4212403: CodTOM := '8243'; // Pedras Grandes/SC';
      4212502: CodTOM := '8245'; // Penha/SC';
      4212601: CodTOM := '8247'; // Peritiba/SC';
      4212650: CodTOM := '1194'; // Pescaria Brava/SC;
      4212700: CodTOM := '8249'; // Petrolandia/SC';
      4212809: CodTOM := '8251'; // Balneario Picarras/SC';
      4212908: CodTOM := '8253'; // Pinhalzinho/SC';
      4213005: CodTOM := '8255'; // Pinheiro Preto/SC';
      4213104: CodTOM := '8257'; // Piratuba/SC';
      4213153: CodTOM := '5593'; // Planalto Alegre/SC';
      4213203: CodTOM := '8259'; // Pomerode/SC';
      4213302: CodTOM := '8261'; // Ponte Alta/SC';
      4213351: CodTOM := '5569'; // Ponte Alta Do Norte/SC';
      4213401: CodTOM := '8263'; // Ponte Serrada/SC';
      4213500: CodTOM := '8265'; // Porto Belo/SC';
      4213609: CodTOM := '8267'; // Porto Uniao/SC';
      4213708: CodTOM := '8269'; // Pouso Redondo/SC';
      4213807: CodTOM := '8271'; // Praia Grande/SC';
      4213906: CodTOM := '8273'; // Presidente Castello Branco/SC';
      4214003: CodTOM := '8275'; // Presidente Getulio/SC';
      4214102: CodTOM := '8277'; // Presidente Nereu/SC';
      4214151: CodTOM := '0934'; // Princesa/SC';
      4214201: CodTOM := '8279'; // Quilombo/SC';
      4214300: CodTOM := '8281'; // Rancho Queimado/SC';
      4214409: CodTOM := '8283'; // Rio Das Antas/SC';
      4214508: CodTOM := '8285'; // Rio Do Campo/SC';
      4214607: CodTOM := '8287'; // Rio Do Oeste/SC';
      4214706: CodTOM := '8289'; // Rio Dos Cedros/SC';
      4214805: CodTOM := '8291'; // Rio Do Sul/SC';
      4214904: CodTOM := '8293'; // Rio Fortuna/SC';
      4215000: CodTOM := '8295'; // Rio Negrinho/SC';
      4215059: CodTOM := '5571'; // Rio Rufino/SC';
      4215075: CodTOM := '5749'; // Riqueza/SC';
      4215109: CodTOM := '8297'; // Rodeio/SC';
      4215208: CodTOM := '8299'; // Romelandia/SC';
      4215307: CodTOM := '8301'; // Salete/SC';
      4215356: CodTOM := '0936'; // Saltinho/SC';
      4215406: CodTOM := '8303'; // Salto Veloso/SC';
      4215455: CodTOM := '5547'; // Sangao/SC';
      4215505: CodTOM := '8305'; // Santa Cecilia/SC';
      4215554: CodTOM := '5751'; // Santa Helena/SC';
      4215604: CodTOM := '8307'; // Santa Rosa De Lima/SC';
      4215653: CodTOM := '9967'; // Santa Rosa Do Sul/SC';
      4215679: CodTOM := '5555'; // Santa Terezinha/SC';
      4215687: CodTOM := '0938'; // Santa Terezinha Do Progresso/SC';
      4215695: CodTOM := '0940'; // Santiago Do Sul/SC';
      4215703: CodTOM := '8309'; // Santo Amaro Da Imperatriz/SC';
      4215752: CodTOM := '0942'; // Sao Bernardino/SC';
      4215802: CodTOM := '8311'; // Sao Bento Do Sul/SC';
      4215901: CodTOM := '8313'; // Sao Bonifacio/SC';
      4216008: CodTOM := '8315'; // Sao Carlos/SC';
      4216057: CodTOM := '5573'; // Sao Cristovao Do Sul/SC';
      4216107: CodTOM := '8317'; // Sao Domingos/SC';
      4216206: CodTOM := '8319'; // Sao Francisco Do Sul/SC';
      4216255: CodTOM := '5753'; // Sao Joao Do Oeste/SC';
      4216305: CodTOM := '8321'; // Sao Joao Batista/SC';
      4216354: CodTOM := '5551'; // Sao Joao Do Itaperiu/SC';
      4216404: CodTOM := '8323'; // Sao Joao Do Sul/SC';
      4216503: CodTOM := '8325'; // Sao Joaquim/SC';
      4216602: CodTOM := '8327'; // Sao Jose/SC';
      4216701: CodTOM := '8329'; // Sao Jose Do Cedro/SC';
      4216800: CodTOM := '8331'; // Sao Jose Do Cerrito/SC';
      4216909: CodTOM := '8333'; // Sao Lourenco Do Oeste/SC';
      4217006: CodTOM := '8335'; // Sao Ludgero/SC';
      4217105: CodTOM := '8337'; // Sao Martinho/SC';
      4217154: CodTOM := '5755'; // Sao Miguel Da Boa Vista/SC';
      4217204: CodTOM := '8339'; // Sao Miguel Do Oeste/SC';
      4217253: CodTOM := '0944'; // Sao Pedro De Alcantara/SC';
      4217303: CodTOM := '8341'; // Saudades/SC';
      4217402: CodTOM := '8343'; // Schroeder/SC';
      4217501: CodTOM := '8345'; // Seara/SC';
      4217550: CodTOM := '9989'; // Serra Alta/SC';
      4217600: CodTOM := '8347'; // Sideropolis/SC';
      4217709: CodTOM := '8349'; // Sombrio/SC';
      4217758: CodTOM := '5595'; // Sul Brasil/SC';
      4217808: CodTOM := '8351'; // Taio/SC';
      4217907: CodTOM := '8353'; // Tangara/SC';
      4217956: CodTOM := '0946'; // Tigrinhos/SC';
      4218004: CodTOM := '8355'; // Tijucas/SC';
      4218103: CodTOM := '8393'; // Timbe Do Sul/SC';
      4218202: CodTOM := '8357'; // Timbo/SC';
      4218251: CodTOM := '9971'; // Timbo Grande/SC';
      4218301: CodTOM := '8359'; // Tres Barras/SC';
      4218350: CodTOM := '0948'; // Treviso/SC';
      4218400: CodTOM := '8361'; // Treze De Maio/SC';
      4218509: CodTOM := '8363'; // Treze Tilias/SC';
      4218608: CodTOM := '8365'; // Trombudo Central/SC';
      4218707: CodTOM := '8367'; // Tubarao/SC';
      4218756: CodTOM := '9991'; // Tunapolis/SC';
      4218806: CodTOM := '8369'; // Turvo/SC';
      4218855: CodTOM := '9973'; // Uniao Do Oeste/SC';
      4218905: CodTOM := '8371'; // Urubici/SC';
      4218954: CodTOM := '9975'; // Urupema/SC';
      4219002: CodTOM := '8373'; // Urussanga/SC';
      4219101: CodTOM := '8375'; // Vargeao/SC';
      4219150: CodTOM := '5563'; // Vargem/SC';
      4219176: CodTOM := '5565'; // Vargem Bonita/SC';
      4219200: CodTOM := '8377'; // Vidal Ramos/SC';
      4219309: CodTOM := '8379'; // Videira/SC';
      4219358: CodTOM := '9977'; // Vitor Meireles/SC';
      4219408: CodTOM := '8381'; // Witmarsum/SC';
      4219507: CodTOM := '8383'; // Xanxere/SC';
      4219606: CodTOM := '8385'; // Xavantina/SC';
      4219705: CodTOM := '8387'; // Xaxim/SC';
      4219853: CodTOM := '0950'; // Zortea/SC';
   end;
 end;

 procedure P43;
 begin
   case ACodigo of
      4300034: CodTOM := '1118'; // Acegua/RS';
      4300059: CodTOM := '8499'; // Agua Santa/RS';
      4300109: CodTOM := '8501'; // Agudo/RS';
      4300208: CodTOM := '8503'; // Ajuricaba/RS';
      4300307: CodTOM := '8505'; // Alecrim/RS';
      4300406: CodTOM := '8507'; // Alegrete/RS';
      4300455: CodTOM := '8497'; // Alegria/RS';
      4300471: CodTOM := '1120'; // Almirante Tamandare Do Sul/RS';
      4300505: CodTOM := '8509'; // Alpestre/RS';
      4300554: CodTOM := '8495'; // Alto Alegre/RS';
      4300570: CodTOM := '6045'; // Alto Feliz/RS';
      4300604: CodTOM := '8511'; // Alvorada/RS';
      4300638: CodTOM := '8493'; // Amaral Ferrador/RS';
      4300646: CodTOM := '5969'; // Ametista Do Sul/RS';
      4300661: CodTOM := '8491'; // Andre Da Rocha/RS';
      4300703: CodTOM := '8513'; // Anta Gorda/RS';
      4300802: CodTOM := '8515'; // Antonio Prado/RS';
      4300851: CodTOM := '5779'; // Arambare/RS';
      4300877: CodTOM := '0952'; // Ararica/RS';
      4300901: CodTOM := '8517'; // Aratiba/RS';
      4301008: CodTOM := '8519'; // Arroio Do Meio/RS';
      4301057: CodTOM := '8489'; // Arroio Do Sal/RS';
      4301073: CodTOM := '1122'; // Arroio Do Padre/RS';
      4301107: CodTOM := '8521'; // Arroio Dos Ratos/RS';
      4301206: CodTOM := '8523'; // Arroio Do Tigre/RS';
      4301305: CodTOM := '8525'; // Arroio Grande/RS';
      4301404: CodTOM := '8527'; // Arvorezinha/RS';
      4301503: CodTOM := '8529'; // Augusto Pestana/RS';
      4301552: CodTOM := '8487'; // Aurea/RS';
      4301602: CodTOM := '8531'; // Bage/RS';
      4301636: CodTOM := '0954'; // Balneario Pinhal/RS';
      4301651: CodTOM := '8485'; // Barao/RS';
      4301701: CodTOM := '8533'; // Barao De Cotegipe/RS';
      4301750: CodTOM := '5771'; // Barao Do Triunfo/RS';
      4301800: CodTOM := '8535'; // Barracao/RS';
      4301859: CodTOM := '6069'; // Barra Do Guarita/RS';
      4301875: CodTOM := '0956'; // Barra Do Quarai/RS';
      4301909: CodTOM := '8537'; // Barra Do Ribeiro/RS';
      4301925: CodTOM := '5959'; // Barra Do Rio Azul/RS';
      4301958: CodTOM := '5943'; // Barra Funda/RS';
      4302006: CodTOM := '8539'; // Barros Cassal/RS';
      4302055: CodTOM := '0958'; // Benjamin Constant Do Sul/RS';
      4302105: CodTOM := '8541'; // Bento Goncalves/RS';
      4302154: CodTOM := '5981'; // Boa Vista Das Missoes/RS';
      4302204: CodTOM := '8543'; // Boa Vista Do Burica/RS';
      4302220: CodTOM := '1124'; // Boa Vista Do Cadeado/RS';
      4302238: CodTOM := '1126'; // Boa Vista Do Incra/RS';
      4302253: CodTOM := '0960'; // Boa Vista Do Sul/RS';
      4302303: CodTOM := '8545'; // Bom Jesus/RS';
      4302352: CodTOM := '9823'; // Bom Principio/RS';
      4302378: CodTOM := '6071'; // Bom Progresso/RS';
      4302402: CodTOM := '8547'; // Bom Retiro Do Sul/RS';
      4302451: CodTOM := '8483'; // Boqueirao Do Leao/RS';
      4302501: CodTOM := '8549'; // Bossoroca/RS';
      4302584: CodTOM := '1128'; // Bozano/RS';
      4302600: CodTOM := '8551'; // Braga/RS';
      4302659: CodTOM := '8449'; // Brochier/RS';
      4302709: CodTOM := '8553'; // Butia/RS';
      4302808: CodTOM := '8555'; // Cacapava Do Sul/RS';
      4302907: CodTOM := '8557'; // Cacequi/RS';
      4303004: CodTOM := '8559'; // Cachoeira Do Sul/RS';
      4303103: CodTOM := '8561'; // Cachoeirinha/RS';
      4303202: CodTOM := '8563'; // Cacique Doble/RS';
      4303301: CodTOM := '8565'; // Caibate/RS';
      4303400: CodTOM := '8567'; // Caicara/RS';
      4303509: CodTOM := '8569'; // Camaqua/RS';
      4303558: CodTOM := '8447'; // Camargo/RS';
      4303608: CodTOM := '8571'; // Cambara Do Sul/RS';
      4303673: CodTOM := '6013'; // Campestre Da Serra/RS';
      4303707: CodTOM := '8573'; // Campina Das Missoes/RS';
      4303806: CodTOM := '8575'; // Campinas Do Sul/RS';
      4303905: CodTOM := '8577'; // Campo Bom/RS';
      4304002: CodTOM := '8579'; // Campo Novo/RS';
      4304101: CodTOM := '8445'; // Campos Borges/RS';
      4304200: CodTOM := '8581'; // Candelaria/RS';
      4304309: CodTOM := '8583'; // Candido Godoi/RS';
      4304358: CodTOM := '6083'; // Candiota/RS';
      4304408: CodTOM := '8585'; // Canela/RS';
      4304507: CodTOM := '8587'; // Cangucu/RS';
      4304606: CodTOM := '8589'; // Canoas/RS';
      4304614: CodTOM := '1130'; // Canudos Do Vale/RS';
      4304622: CodTOM := '1132'; // Capao Bonito Do Sul/RS';
      4304630: CodTOM := '8915'; // Capao Da Canoa/RS';
      4304655: CodTOM := '1134'; // Capao Do Cipo/RS';
      4304663: CodTOM := '8973'; // Capao Do Leao/RS';
      4304671: CodTOM := '0962'; // Capivari Do Sul/RS';
      4304689: CodTOM := '8443'; // Capela De Santana/RS';
      4304697: CodTOM := '6025'; // Capitao/RS';
      4304705: CodTOM := '8591'; // Carazinho/RS';
      4304713: CodTOM := '0964'; // Caraa/RS';
      4304804: CodTOM := '8593'; // Carlos Barbosa/RS';
      4304853: CodTOM := '5961'; // Carlos Gomes/RS';
      4304903: CodTOM := '8595'; // Casca/RS';
      4304952: CodTOM := '8441'; // Caseiros/RS';
      4305009: CodTOM := '8597'; // Catuipe/RS';
      4305108: CodTOM := '8599'; // Caxias Do Sul/RS';
      4305116: CodTOM := '5963'; // Centenario/RS';
      4305124: CodTOM := '0966'; // Cerrito/RS';
      4305132: CodTOM := '8439'; // Cerro Branco/RS';
      4305157: CodTOM := '8437'; // Cerro Grande/RS';
      4305173: CodTOM := '8435'; // Cerro Grande Do Sul/RS';
      4305207: CodTOM := '8601'; // Cerro Largo/RS';
      4305306: CodTOM := '8603'; // Chapada/RS';
      4305355: CodTOM := '8693'; // Charqueadas/RS';
      4305371: CodTOM := '5965'; // Charrua/RS';
      4305405: CodTOM := '8605'; // Chiapetta/RS';
      4305439: CodTOM := '0968'; // Chui/RS';
      4305447: CodTOM := '0970'; // Chuvisca/RS';
      4305454: CodTOM := '8433'; // Cidreira/RS';
      4305504: CodTOM := '8607'; // Ciriaco/RS';
      4305587: CodTOM := '6029'; // Colinas/RS';
      4305603: CodTOM := '8609'; // Colorado/RS';
      4305702: CodTOM := '8611'; // Condor/RS';
      4305801: CodTOM := '8613'; // Constantina/RS';
      4305835: CodTOM := '1136'; // Coqueiro Baixo/RS';
      4305850: CodTOM := '5945'; // Coqueiros Do Sul/RS';
      4305871: CodTOM := '6055'; // Coronel Barros/RS';
      4305900: CodTOM := '8615'; // Coronel Bicaco/RS';
      4305934: CodTOM := '1138'; // Coronel Pilar/RS';
      4305959: CodTOM := '8977'; // Cotipora/RS';
      4305975: CodTOM := '5797'; // Coxilha/RS';
      4306007: CodTOM := '8617'; // Crissiumal/RS';
      4306056: CodTOM := '8431'; // Cristal/RS';
      4306072: CodTOM := '0972'; // Cristal Do Sul/RS';
      4306106: CodTOM := '8619'; // Cruz Alta/RS';
      4306130: CodTOM := '1140'; // Cruzaltense/RS';
      4306205: CodTOM := '8621'; // Cruzeiro Do Sul/RS';
      4306304: CodTOM := '8623'; // David Canabarro/RS';
      4306320: CodTOM := '6073'; // Derrubadas/RS';
      4306353: CodTOM := '8429'; // Dezesseis De Novembro/RS';
      4306379: CodTOM := '0974'; // Dilermando De Aguiar/RS';
      4306403: CodTOM := '8625'; // Dois Irmaos/RS';
      4306429: CodTOM := '5971'; // Dois Irmaos Das Missoes/RS';
      4306452: CodTOM := '8427'; // Dois Lajeados/RS';
      4306502: CodTOM := '8627'; // Dom Feliciano/RS';
      4306551: CodTOM := '0976'; // Dom Pedro De Alcantara/RS';
      4306601: CodTOM := '8629'; // Dom Pedrito/RS';
      4306700: CodTOM := '8631'; // Dona Francisca/RS';
      4306734: CodTOM := '8425'; // Doutor Mauricio Cardoso/RS';
      4306759: CodTOM := '0978'; // Doutor Ricardo/RS';
      4306767: CodTOM := '8423'; // Eldorado Do Sul/RS';
      4306809: CodTOM := '8633'; // Encantado/RS';
      4306908: CodTOM := '8635'; // Encruzilhada Do Sul/RS';
      4306924: CodTOM := '5947'; // Engenho Velho/RS';
      4306932: CodTOM := '8419'; // Entre-Ijuis/RS';
      4306957: CodTOM := '8421'; // Entre Rios Do Sul/RS';
      4306973: CodTOM := '8417'; // Erebango/RS';
      4307005: CodTOM := '8637'; // Erechim/RS';
      4307054: CodTOM := '8415'; // Ernestina/RS';
      4307104: CodTOM := '8639'; // Herval/RS';
      4307203: CodTOM := '8641'; // Erval Grande/RS';
      4307302: CodTOM := '8643'; // Erval Seco/RS';
      4307401: CodTOM := '8645'; // Esmeralda/RS';
      4307450: CodTOM := '0980'; // Esperanca Do Sul/RS';
      4307500: CodTOM := '8647'; // Espumoso/RS';
      4307559: CodTOM := '7301'; // Estacao/RS';
      4307609: CodTOM := '8649'; // Estancia Velha/RS';
      4307708: CodTOM := '8651'; // Esteio/RS';
      4307807: CodTOM := '8653'; // Estrela/RS';
      4307815: CodTOM := '0982'; // Estrela Velha/RS';
      4307831: CodTOM := '8413'; // Eugenio De Castro/RS';
      4307864: CodTOM := '8411'; // Fagundes Varela/RS';
      4307906: CodTOM := '8655'; // Farroupilha/RS';
      4308003: CodTOM := '8657'; // Faxinal Do Soturno/RS';
      4308052: CodTOM := '8409'; // Faxinalzinho/RS';
      4308078: CodTOM := '0984'; // Fazenda Vilanova/RS';
      4308102: CodTOM := '8659'; // Feliz/RS';
      4308201: CodTOM := '8661'; // Flores Da Cunha/RS';
      4308250: CodTOM := '0986'; // Floriano Peixoto/RS';
      4308300: CodTOM := '8663'; // Fontoura Xavier/RS';
      4308409: CodTOM := '8665'; // Formigueiro/RS';
      4308433: CodTOM := '1142'; // Forquetinha/RS';
      4308458: CodTOM := '9827'; // Fortaleza Dos Valos/RS';
      4308508: CodTOM := '8667'; // Frederico Westphalen/RS';
      4308607: CodTOM := '8669'; // Garibaldi/RS';
      4308656: CodTOM := '6081'; // Garruchos/RS';
      4308706: CodTOM := '8671'; // Gaurama/RS';
      4308805: CodTOM := '8673'; // General Camara/RS';
      4308854: CodTOM := '5799'; // Gentil/RS';
      4308904: CodTOM := '8677'; // Getulio Vargas/RS';
      4309001: CodTOM := '8679'; // Girua/RS';
      4309050: CodTOM := '8407'; // Glorinha/RS';
      4309100: CodTOM := '8681'; // Gramado/RS';
      4309126: CodTOM := '5949'; // Gramado Dos Loureiros/RS';
      4309159: CodTOM := '5763'; // Gramado Xavier/RS';
      4309209: CodTOM := '8683'; // Gravatai/RS';
      4309258: CodTOM := '8405'; // Guabiju/RS';
      4309308: CodTOM := '8685'; // Guaiba/RS';
      4309407: CodTOM := '8687'; // Guapore/RS';
      4309506: CodTOM := '8689'; // Guarani Das Missoes/RS';
      4309555: CodTOM := '8403'; // Harmonia/RS';
      4309571: CodTOM := '0988'; // Herveiras/RS';
      4309605: CodTOM := '8691'; // Horizontina/RS';
      4309654: CodTOM := '6085'; // Hulha Negra/RS';
      4309704: CodTOM := '8695'; // Humaita/RS';
      4309753: CodTOM := '8401'; // Ibarama/RS';
      4309803: CodTOM := '8697'; // Ibiaca/RS';
      4309902: CodTOM := '8699'; // Ibiraiaras/RS';
      4309951: CodTOM := '7299'; // Ibirapuita/RS';
      4310009: CodTOM := '8701'; // Ibiruba/RS';
      4310108: CodTOM := '8703'; // Igrejinha/RS';
      4310207: CodTOM := '8705'; // Ijui/RS';
      4310306: CodTOM := '8707'; // Ilopolis/RS';
      4310330: CodTOM := '7297'; // Imbe/RS';
      4310363: CodTOM := '7295'; // Imigrante/RS';
      4310405: CodTOM := '8709'; // Independencia/RS';
      4310413: CodTOM := '6051'; // Inhacora/RS';
      4310439: CodTOM := '8399'; // Ipe/RS';
      4310462: CodTOM := '7399'; // Ipiranga Do Sul/RS';
      4310504: CodTOM := '8711'; // Irai/RS';
      4310538: CodTOM := '0990'; // Itaara/RS';
      4310553: CodTOM := '7397'; // Itacurubi/RS';
      4310579: CodTOM := '6027'; // Itapuca/RS';
      4310603: CodTOM := '8713'; // Itaqui/RS';
      4310652: CodTOM := '1144'; // Itati/RS';
      4310702: CodTOM := '8715'; // Itatiba Do Sul/RS';
      4310751: CodTOM := '7395'; // Ivora/RS';
      4310801: CodTOM := '8717'; // Ivoti/RS';
      4310850: CodTOM := '7393'; // Jaboticaba/RS';
      4310876: CodTOM := '1146'; // Jacuizinho/RS';
      4310900: CodTOM := '8719'; // Jacutinga/RS';
      4311007: CodTOM := '8721'; // Jaguarao/RS';
      4311106: CodTOM := '8723'; // Jaguari/RS';
      4311122: CodTOM := '7391'; // Jaquirana/RS';
      4311130: CodTOM := '0992'; // Jari/RS';
      4311155: CodTOM := '9829'; // Joia/RS';
      4311205: CodTOM := '8725'; // Julio De Castilhos/RS';
      4311239: CodTOM := '1148'; // Lagoa Bonita Do Sul/RS';
      4311254: CodTOM := '7389'; // Lagoao/RS';
      4311270: CodTOM := '5951'; // Lagoa Dos Tres Cantos/RS';
      4311304: CodTOM := '8727'; // Lagoa Vermelha/RS';
      4311403: CodTOM := '8729'; // Lajeado/RS';
      4311429: CodTOM := '5983'; // Lajeado Do Bugre/RS';
      4311502: CodTOM := '8731'; // Lavras Do Sul/RS';
      4311601: CodTOM := '8733'; // Liberato Salzano/RS';
      4311627: CodTOM := '6017'; // Lindolfo Collor/RS';
      4311643: CodTOM := '6047'; // Linha Nova/RS';
      4311700: CodTOM := '8735'; // Machadinho/RS';
      4311718: CodTOM := '0994'; // Macambara/RS';
      4311734: CodTOM := '0996'; // Mampituba/RS';
      4311759: CodTOM := '6079'; // Manoel Viana/RS';
      4311775: CodTOM := '5783'; // Maquine/RS';
      4311791: CodTOM := '6039'; // Marata/RS';
      4311809: CodTOM := '8737'; // Marau/RS';
      4311908: CodTOM := '8739'; // Marcelino Ramos/RS';
      4311981: CodTOM := '5759'; // Mariana Pimentel/RS';
      4312005: CodTOM := '8741'; // Mariano Moro/RS';
      4312054: CodTOM := '0998'; // Marques De Souza/RS';
      4312104: CodTOM := '8743'; // Mata/RS';
      4312138: CodTOM := '5931'; // Mato Castelhano/RS';
      4312153: CodTOM := '6031'; // Mato Leitao/RS';
      4312179: CodTOM := '1150'; // Mato Queimado/RS';
      4312203: CodTOM := '8745'; // Maximiliano De Almeida/RS';
      4312252: CodTOM := '5773'; // Minas Do Leao/RS';
      4312302: CodTOM := '8747'; // Miraguai/RS';
      4312351: CodTOM := '7387'; // Montauri/RS';
      4312377: CodTOM := '1000'; // Monte Alegre Dos Campos/RS';
      4312385: CodTOM := '5993'; // Monte Belo Do Sul/RS';
      4312401: CodTOM := '8749'; // Montenegro/RS';
      4312427: CodTOM := '5933'; // Mormaco/RS';
      4312443: CodTOM := '5775'; // Morrinhos Do Sul/RS';
      4312450: CodTOM := '7385'; // Morro Redondo/RS';
      4312476: CodTOM := '6019'; // Morro Reuter/RS';
      4312500: CodTOM := '8751'; // Mostardas/RS';
      4312609: CodTOM := '8753'; // Mucum/RS';
      4312617: CodTOM := '1002'; // Muitos Capoes/RS';
      4312625: CodTOM := '5935'; // Muliterno/RS';
      4312658: CodTOM := '8755'; // Nao-Me-Toque/RS';
      4312674: CodTOM := '5937'; // Nicolau Vergueiro/RS';
      4312708: CodTOM := '8757'; // Nonoai/RS';
      4312757: CodTOM := '7383'; // Nova Alvorada/RS';
      4312807: CodTOM := '8759'; // Nova Araca/RS';
      4312906: CodTOM := '8761'; // Nova Bassano/RS';
      4312955: CodTOM := '5953'; // Nova Boa Vista/RS';
      4313003: CodTOM := '8763'; // Nova Brescia/RS';
      4313011: CodTOM := '1004'; // Nova Candelaria/RS';
      4313037: CodTOM := '7381'; // Nova Esperanca Do Sul/RS';
      4313060: CodTOM := '7379'; // Nova Hartz/RS';
      4313086: CodTOM := '5991'; // Nova Padua/RS';
      4313102: CodTOM := '8765'; // Nova Palma/RS';
      4313201: CodTOM := '8767'; // Nova Petropolis/RS';
      4313300: CodTOM := '8769'; // Nova Prata/RS';
      4313334: CodTOM := '1006'; // Nova Ramada/RS';
      4313359: CodTOM := '7377'; // Nova Roma Do Sul/RS';
      4313375: CodTOM := '5757'; // Nova Santa Rita/RS';
      4313391: CodTOM := '1008'; // Novo Cabrais/RS';
      4313409: CodTOM := '8771'; // Novo Hamburgo/RS';
      4313425: CodTOM := '6057'; // Novo Machado/RS';
      4313441: CodTOM := '5973'; // Novo Tiradentes/RS';
      4313466: CodTOM := '1152'; // Novo Xingu/RS';
      4313490: CodTOM := '5985'; // Novo Barreiro/RS';
      4313508: CodTOM := '8773'; // Osorio/RS';
      4313607: CodTOM := '8775'; // Paim Filho/RS';
      4313656: CodTOM := '8967'; // Palmares Do Sul/RS';
      4313706: CodTOM := '8777'; // Palmeira Das Missoes/RS';
      4313805: CodTOM := '8779'; // Palmitinho/RS';
      4313904: CodTOM := '8781'; // Panambi/RS';
      4313953: CodTOM := '7375'; // Pantano Grande/RS';
      4314001: CodTOM := '8783'; // Parai/RS';
      4314027: CodTOM := '7373'; // Paraiso Do Sul/RS';
      4314035: CodTOM := '6041'; // Pareci Novo/RS';
      4314050: CodTOM := '9825'; // Parobe/RS';
      4314068: CodTOM := '1010'; // Passa Sete/RS';
      4314076: CodTOM := '5765'; // Passo Do Sobrado/RS';
      4314100: CodTOM := '8785'; // Passo Fundo/RS';
      4314134: CodTOM := '1154'; // Paulo Bento/RS';
      4314159: CodTOM := '7371'; // Paverama/RS';
      4314175: CodTOM := '1156'; // Pedras Altas/RS';
      4314209: CodTOM := '8787'; // Pedro Osorio/RS';
      4314308: CodTOM := '8789'; // Pejucara/RS';
      4314407: CodTOM := '8791'; // Pelotas/RS';
      4314423: CodTOM := '6021'; // Picada Cafe/RS';
      4314456: CodTOM := '7369'; // Pinhal/RS';
      4314464: CodTOM := '1158'; // Pinhal Da Serra/RS';
      4314472: CodTOM := '5787'; // Pinhal Grande/RS';
      4314498: CodTOM := '5975'; // Pinheirinho Do Vale/RS';
      4314506: CodTOM := '8793'; // Pinheiro Machado/RS';
      4314548: CodTOM := '1160'; // Pinto Bandeira/RS';
      4314555: CodTOM := '7367'; // Pirapo/RS';
      4314605: CodTOM := '8795'; // Piratini/RS';
      4314704: CodTOM := '8797'; // Planalto/RS';
      4314753: CodTOM := '7365'; // Poco Das Antas/RS';
      4314779: CodTOM := '5939'; // Pontao/RS';
      4314787: CodTOM := '5967'; // Ponte Preta/RS';
      4314803: CodTOM := '8799'; // Portao/RS';
      4314902: CodTOM := '8801'; // Porto Alegre/RS';
      4315008: CodTOM := '8803'; // Porto Lucena/RS';
      4315057: CodTOM := '6065'; // Porto Maua/RS';
      4315073: CodTOM := '6067'; // Porto Vera Cruz/RS';
      4315107: CodTOM := '8805'; // Porto Xavier/RS';
      4315131: CodTOM := '7363'; // Pouso Novo/RS';
      4315149: CodTOM := '6023'; // Presidente Lucena/RS';
      4315156: CodTOM := '7361'; // Progresso/RS';
      4315172: CodTOM := '7359'; // Protasio Alves/RS';
      4315206: CodTOM := '8807'; // Putinga/RS';
      4315305: CodTOM := '8809'; // Quarai/RS';
      4315313: CodTOM := '1162'; // Quatro Irmaos/RS';
      4315321: CodTOM := '5789'; // Quevedos/RS';
      4315354: CodTOM := '7357'; // Quinze De Novembro/RS';
      4315404: CodTOM := '8811'; // Redentora/RS';
      4315453: CodTOM := '7355'; // Relvado/RS';
      4315503: CodTOM := '8813'; // Restinga Seca/RS';
      4315552: CodTOM := '5955'; // Rio Dos Indios/RS';
      4315602: CodTOM := '8815'; // Rio Grande/RS';
      4315701: CodTOM := '8817'; // Rio Pardo/RS';
      4315750: CodTOM := '7353'; // Riozinho/RS';
      4315800: CodTOM := '8819'; // Roca Sales/RS';
      4315909: CodTOM := '8821'; // Rodeio Bonito/RS';
      4315958: CodTOM := '1164'; // Rolador/RS';
      4316006: CodTOM := '8823'; // Rolante/RS';
      4316105: CodTOM := '8825'; // Ronda Alta/RS';
      4316204: CodTOM := '8827'; // Rondinha/RS';
      4316303: CodTOM := '8829'; // Roque Gonzales/RS';
      4316402: CodTOM := '8831'; // Rosario Do Sul/RS';
      4316428: CodTOM := '5987'; // Sagrada Familia/RS';
      4316436: CodTOM := '7339'; // Saldanha Marinho/RS';
      4316451: CodTOM := '8975'; // Salto Do Jacui/RS';
      4316477: CodTOM := '6061'; // Salvador Das Missoes/RS';
      4316501: CodTOM := '8833'; // Salvador Do Sul/RS';
      4316600: CodTOM := '8835'; // Sananduva/RS';
      4316709: CodTOM := '8837'; // Santa Barbara Do Sul/RS';
      4316733: CodTOM := '1166'; // Santa Cecilia Do Sul/RS';
      4316758: CodTOM := '6033'; // Santa Clara Do Sul/RS';
      4316808: CodTOM := '8839'; // Santa Cruz Do Sul/RS';
      4316907: CodTOM := '8841'; // Santa Maria/RS';
      4316956: CodTOM := '7337'; // Santa Maria Do Herval/RS';
      4316972: CodTOM := '1168'; // Santa Margarida Do Sul/RS';
      4317004: CodTOM := '8843'; // Santana Da Boa Vista/RS';
      4317103: CodTOM := '8845'; // Sant  Ana Do Livramento/RS';
      4317202: CodTOM := '8847'; // Santa Rosa/RS';
      4317251: CodTOM := '5995'; // Santa Tereza/RS';
      4317301: CodTOM := '8849'; // Santa Vitoria Do Palmar/RS';
      4317400: CodTOM := '8851'; // Santiago/RS';
      4317509: CodTOM := '8853'; // Santo Angelo/RS';
      4317558: CodTOM := '5941'; // Santo Antonio Do Palma/RS';
      4317608: CodTOM := '8855'; // Santo Antonio Da Patrulha/RS';
      4317707: CodTOM := '8857'; // Santo Antonio Das Missoes/RS';
      4317756: CodTOM := '5957'; // Santo Antonio Do Planalto/RS';
      4317806: CodTOM := '8859'; // Santo Augusto/RS';
      4317905: CodTOM := '8861'; // Santo Cristo/RS';
      4317954: CodTOM := '5977'; // Santo Expedito Do Sul/RS';
      4318002: CodTOM := '8863'; // Sao Borja/RS';
      4318051: CodTOM := '7351'; // Sao Domingos Do Sul/RS';
      4318101: CodTOM := '8865'; // Sao Francisco De Assis/RS';
      4318200: CodTOM := '8867'; // Sao Francisco De Paula/RS';
      4318309: CodTOM := '8869'; // Sao Gabriel/RS';
      4318408: CodTOM := '8871'; // Sao Jeronimo/RS';
      4318424: CodTOM := '7349'; // Sao Joao Da Urtiga/RS';
      4318432: CodTOM := '5791'; // Sao Joao Do Polesine/RS';
      4318440: CodTOM := '7347'; // Sao Jorge/RS';
      4318457: CodTOM := '5989'; // Sao Jose Das Missoes/RS';
      4318465: CodTOM := '7345'; // Sao Jose Do Herval/RS';
      4318481: CodTOM := '7343'; // Sao Jose Do Hortencio/RS';
      4318499: CodTOM := '6059'; // Sao Jose Do Inhacora/RS';
      4318507: CodTOM := '8873'; // Sao Jose Do Norte/RS';
      4318606: CodTOM := '8875'; // Sao Jose Do Ouro/RS';
      4318614: CodTOM := '1170'; // Sao Jose Do Sul/RS';
      4318622: CodTOM := '6015'; // Sao Jose Dos Ausentes/RS';
      4318705: CodTOM := '8877'; // Sao Leopoldo/RS';
      4318804: CodTOM := '8879'; // Sao Lourenco Do Sul/RS';
      4318903: CodTOM := '8881'; // Sao Luiz Gonzaga/RS';
      4319000: CodTOM := '8883'; // Sao Marcos/RS';
      4319109: CodTOM := '8885'; // Sao Martinho/RS';
      4319125: CodTOM := '5793'; // Sao Martinho Da Serra/RS';
      4319158: CodTOM := '7341'; // Sao Miguel Das Missoes/RS';
      4319208: CodTOM := '8887'; // Sao Nicolau/RS';
      4319307: CodTOM := '8889'; // Sao Paulo Das Missoes/RS';
      4319356: CodTOM := '6043'; // Sao Pedro Da Serra/RS';
      4319364: CodTOM := '1172'; // Sao Pedro Das Missoes/RS';
      4319372: CodTOM := '6063'; // Sao Pedro Do Butia/RS';
      4319406: CodTOM := '8891'; // Sao Pedro Do Sul/RS';
      4319505: CodTOM := '8893'; // Sao Sebastiao Do Cai/RS';
      4319604: CodTOM := '8895'; // Sao Sepe/RS';
      4319703: CodTOM := '8897'; // Sao Valentim/RS';
      4319711: CodTOM := '5997'; // Sao Valentim Do Sul/RS';
      4319737: CodTOM := '6075'; // Sao Valerio Do Sul/RS';
      4319752: CodTOM := '7293'; // Sao Vendelino/RS';
      4319802: CodTOM := '8675'; // Sao Vicente Do Sul/RS';
      4319901: CodTOM := '8899'; // Sapiranga/RS';
      4320008: CodTOM := '8901'; // Sapucaia Do Sul/RS';
      4320107: CodTOM := '8903'; // Sarandi/RS';
      4320206: CodTOM := '8905'; // Seberi/RS';
      4320230: CodTOM := '7335'; // Sede Nova/RS';
      4320263: CodTOM := '7317'; // Segredo/RS';
      4320305: CodTOM := '8907'; // Selbach/RS';
      4320321: CodTOM := '1012'; // Senador Salgado Filho/RS';
      4320354: CodTOM := '5781'; // Sentinela Do Sul/RS';
      4320404: CodTOM := '8909'; // Serafina Correa/RS';
      4320453: CodTOM := '6035'; // Serio/RS';
      4320503: CodTOM := '8911'; // Sertao/RS';
      4320552: CodTOM := '5761'; // Sertao Santana/RS';
      4320578: CodTOM := '1014'; // Sete De Setembro/RS';
      4320602: CodTOM := '8913'; // Severiano De Almeida/RS';
      4320651: CodTOM := '7315'; // Silveira Martins/RS';
      4320677: CodTOM := '5767'; // Sinimbu/RS';
      4320701: CodTOM := '8917'; // Sobradinho/RS';
      4320800: CodTOM := '8919'; // Soledade/RS';
      4320859: CodTOM := '1016'; // Tabai/RS';
      4320909: CodTOM := '8921'; // Tapejara/RS';
      4321006: CodTOM := '8923'; // Tapera/RS';
      4321105: CodTOM := '8925'; // Tapes/RS';
      4321204: CodTOM := '8927'; // Taquara/RS';
      4321303: CodTOM := '8929'; // Taquari/RS';
      4321329: CodTOM := '7313'; // Taquarucu Do Sul/RS';
      4321352: CodTOM := '8971'; // Tavares/RS';
      4321402: CodTOM := '8931'; // Tenente Portela/RS';
      4321436: CodTOM := '7333'; // Terra De Areia/RS';
      4321451: CodTOM := '9821'; // Teutonia/RS';
      4321469: CodTOM := '1174'; // Tio Hugo/RS';
      4321477: CodTOM := '6077'; // Tiradentes Do Sul/RS';
      4321493: CodTOM := '1018'; // Toropi/RS';
      4321501: CodTOM := '8933'; // Torres/RS';
      4321600: CodTOM := '8935'; // Tramandai/RS';
      4321626: CodTOM := '6037'; // Travesseiro/RS';
      4321634: CodTOM := '7331'; // Tres Arroios/RS';
      4321667: CodTOM := '7329'; // Tres Cachoeiras/RS';
      4321709: CodTOM := '8937'; // Tres Coroas/RS';
      4321808: CodTOM := '8939'; // Tres De Maio/RS';
      4321832: CodTOM := '5777'; // Tres Forquilhas/RS';
      4321857: CodTOM := '7327'; // Tres Palmeiras/RS';
      4321907: CodTOM := '8941'; // Tres Passos/RS';
      4321956: CodTOM := '7325'; // Trindade Do Sul/RS';
      4322004: CodTOM := '8943'; // Triunfo/RS';
      4322103: CodTOM := '8945'; // Tucunduva/RS';
      4322152: CodTOM := '7323'; // Tunas/RS';
      4322186: CodTOM := '5979'; // Tupanci Do Sul/RS';
      4322202: CodTOM := '8947'; // Tupancireta/RS';
      4322251: CodTOM := '7321'; // Tupandi/RS';
      4322301: CodTOM := '8949'; // Tuparendi/RS';
      4322327: CodTOM := '1020'; // Turucu/RS';
      4322343: CodTOM := '1022'; // Ubiretama/RS';
      4322350: CodTOM := '5999'; // Uniao Da Serra/RS';
      4322376: CodTOM := '1024'; // Unistalda/RS';
      4322400: CodTOM := '8951'; // Uruguaiana/RS';
      4322509: CodTOM := '8953'; // Vacaria/RS';
      4322525: CodTOM := '1026'; // Vale Verde/RS';
      4322533: CodTOM := '5769'; // Vale Do Sol/RS';
      4322541: CodTOM := '6049'; // Vale Real/RS';
      4322558: CodTOM := '7319'; // Vanini/RS';
      4322608: CodTOM := '8955'; // Venancio Aires/RS';
      4322707: CodTOM := '8957'; // Vera Cruz/RS';
      4322806: CodTOM := '8959'; // Veranopolis/RS';
      4322855: CodTOM := '1028'; // Vespasiano Correa/RS';
      4322905: CodTOM := '8961'; // Viadutos/RS';
      4323002: CodTOM := '8963'; // Viamao/RS';
      4323101: CodTOM := '8965'; // Vicente Dutra/RS';
      4323200: CodTOM := '8969'; // Victor Graeff/RS';
      4323309: CodTOM := '7311'; // Vila Flores/RS';
      4323358: CodTOM := '1030'; // Vila Langaro/RS';
      4323408: CodTOM := '7309'; // Vila Maria/RS';
      4323457: CodTOM := '5795'; // Vila Nova Do Sul/RS';
      4323507: CodTOM := '7307'; // Vista Alegre/RS';
      4323606: CodTOM := '7305'; // Vista Alegre Do Prata/RS';
      4323705: CodTOM := '7303'; // Vista Gaucha/RS';
      4323754: CodTOM := '6053'; // Vitoria Das Missoes/RS';
      4323770: CodTOM := '1176'; // Westfalia/RS';
      4323804: CodTOM := '5785'; // Xangri-La/RS';
   end;
 end;

 procedure P50;
 begin
   case ACodigo of
      5000203: CodTOM := '9003'; // Agua Clara/MS';
      5000252: CodTOM := '0141'; // Alcinopolis/MS';
      5000609: CodTOM := '9011'; // Amambai/MS';
      5000708: CodTOM := '9013'; // Anastacio/MS';
      5000807: CodTOM := '9015'; // Anaurilandia/MS';
      5000856: CodTOM := '9169'; // Angelica/MS';
      5000906: CodTOM := '9017'; // Antonio Joao/MS';
      5001003: CodTOM := '9019'; // Aparecida Do Taboado/MS';
      5001102: CodTOM := '9021'; // Aquidauana/MS';
      5001243: CodTOM := '9171'; // Aral Moreira/MS';
      5001508: CodTOM := '9029'; // Bandeirantes/MS';
      5001904: CodTOM := '9037'; // Bataguassu/MS';
      5002001: CodTOM := '9039'; // Bataypora/MS';
      5002100: CodTOM := '9041'; // Bela Vista/MS';
      5002159: CodTOM := '9801'; // Bodoquena/MS';
      5002209: CodTOM := '9043'; // Bonito/MS';
      5002308: CodTOM := '9045'; // Brasilandia/MS';
      5002407: CodTOM := '9055'; // Caarapo/MS';
      5002605: CodTOM := '9049'; // Camapua/MS';
      5002704: CodTOM := '9051'; // Campo Grande/MS';
      5002803: CodTOM := '9053'; // Caracol/MS';
      5002902: CodTOM := '9057'; // Cassilandia/MS';
      5002951: CodTOM := '9787'; // Chapadao Do Sul/MS';
      5003108: CodTOM := '9061'; // Corguinho/MS';
      5003157: CodTOM := '9997'; // Coronel Sapucaia/MS';
      5003207: CodTOM := '9063'; // Corumba/MS';
      5003256: CodTOM := '9803'; // Costa Rica/MS';
      5003306: CodTOM := '9065'; // Coxim/MS';
      5003454: CodTOM := '9175'; // Deodapolis/MS';
      5003488: CodTOM := '9793'; // Dois Irmaos Do Buriti/MS';
      5003504: CodTOM := '9805'; // Douradina/MS';
      5003702: CodTOM := '9073'; // Dourados/MS';
      5003751: CodTOM := '9173'; // Eldorado/MS';
      5003801: CodTOM := '9075'; // Fatima Do Sul/MS';
      5003900: CodTOM := '1178'; // Figueirao/MS';
      5004007: CodTOM := '9079'; // Gloria De Dourados/MS';
      5004106: CodTOM := '9081'; // Guia Lopes Da Laguna/MS';
      5004304: CodTOM := '9085'; // Iguatemi/MS';
      5004403: CodTOM := '9087'; // Inocencia/MS';
      5004502: CodTOM := '9089'; // Itapora/MS';
      5004601: CodTOM := '9807'; // Itaquirai/MS';
      5004700: CodTOM := '9093'; // Ivinhema/MS';
      5004809: CodTOM := '0161'; // Japora/MS';
      5004908: CodTOM := '9097'; // Jaraguari/MS';
      5005004: CodTOM := '9099'; // Jardim/MS';
      5005103: CodTOM := '9101'; // Jatei/MS';
      5005152: CodTOM := '9923'; // Juti/MS';
      5005202: CodTOM := '9103'; // Ladario/MS';
      5005251: CodTOM := '0163'; // Laguna Carapa/MS';
      5005400: CodTOM := '9107'; // Maracaju/MS';
      5005608: CodTOM := '9111'; // Miranda/MS';
      5005681: CodTOM := '9179'; // Mundo Novo/MS';
      5005707: CodTOM := '9113'; // Navirai/MS';
      5005806: CodTOM := '9115'; // Nioaque/MS';
      5006002: CodTOM := '0143'; // Nova Alvorada Do Sul/MS';
      5006200: CodTOM := '9123'; // Nova Andradina/MS';
      5006259: CodTOM := '0159'; // Novo Horizonte Do Sul/MS';
      5006275: CodTOM := '1196'; // Paraiso Das Aguas/MS';
      5006309: CodTOM := '9125'; // Paranaiba/MS';
      5006358: CodTOM := '9739'; // Paranhos/MS';
      5006408: CodTOM := '9127'; // Pedro Gomes/MS';
      5006606: CodTOM := '9131'; // Ponta Pora/MS';
      5006903: CodTOM := '9137'; // Porto Murtinho/MS';
      5007109: CodTOM := '9141'; // Ribas Do Rio Pardo/MS';
      5007208: CodTOM := '9143'; // Rio Brilhante/MS';
      5007307: CodTOM := '9145'; // Rio Negro/MS';
      5007406: CodTOM := '9147'; // Rio Verde De Mato Grosso/MS';
      5007505: CodTOM := '9149'; // Rochedo/MS';
      5007554: CodTOM := '9745'; // Santa Rita Do Pardo/MS';
      5007695: CodTOM := '9809'; // Sao Gabriel Do Oeste/MS';
      5007703: CodTOM := '9813'; // Sete Quedas/MS';
      5007802: CodTOM := '9811'; // Selviria/MS';
      5007901: CodTOM := '9157'; // Sidrolandia/MS';
      5007935: CodTOM := '9757'; // Sonora/MS';
      5007950: CodTOM := '9815'; // Tacuru/MS';
      5007976: CodTOM := '9817'; // Taquarussu/MS';
      5008008: CodTOM := '9159'; // Terenos/MS';
      5008305: CodTOM := '9165'; // Tres Lagoas/MS';
      5008404: CodTOM := '9187'; // Vicentina/MS';
   end;
 end;

 procedure P51;
 begin
   case ACodigo of
      5100102: CodTOM := '9001'; // Acorizal/MT';
      5100201: CodTOM := '9191'; // Agua Boa/MT';
      5100250: CodTOM := '8987'; // Alta Floresta/MT';
      5100300: CodTOM := '9005'; // Alto Araguaia/MT';
      5100359: CodTOM := '0127'; // Alto Boa Vista/MT';
      5100409: CodTOM := '9007'; // Alto Garcas/MT';
      5100508: CodTOM := '9009'; // Alto Paraguai/MT';
      5100607: CodTOM := '9911'; // Alto Taquari/MT';
      5100805: CodTOM := '9773'; // Apiacas/MT';
      5101001: CodTOM := '9869'; // Araguaiana/MT';
      5101209: CodTOM := '9023'; // Araguainha/MT';
      5101258: CodTOM := '8989'; // Araputanga/MT';
      5101308: CodTOM := '9025'; // Arenapolis/MT';
      5101407: CodTOM := '9027'; // Aripuana/MT';
      5101605: CodTOM := '9031'; // Barao De Melgaco/MT';
      5101704: CodTOM := '9033'; // Barra Do Bugres/MT';
      5101803: CodTOM := '9035'; // Barra Do Garcas/MT';
      5101852: CodTOM := '1078'; // Bom Jesus Do Araguaia/MT';
      5101902: CodTOM := '9873'; // Brasnorte/MT';
      5102504: CodTOM := '9047'; // Caceres/MT';
      5102603: CodTOM := '9863'; // Campinapolis/MT';
      5102637: CodTOM := '9777'; // Campo Novo Do Parecis/MT';
      5102678: CodTOM := '9779'; // Campo Verde/MT';
      5102686: CodTOM := '1032'; // Campos De Julio/MT';
      5102694: CodTOM := '0129'; // Canabrava Do Norte/MT';
      5102702: CodTOM := '9193'; // Canarana/MT';
      5102793: CodTOM := '1034'; // Carlinda/MT';
      5102850: CodTOM := '9783'; // Castanheira/MT';
      5103007: CodTOM := '9059'; // Chapada Dos Guimaraes/MT';
      5103056: CodTOM := '9789'; // Claudia/MT';
      5103106: CodTOM := '9865'; // Cocalinho/MT';
      5103205: CodTOM := '8979'; // Colider/MT';
      5103254: CodTOM := '1080'; // Colniza/MT';
      5103304: CodTOM := '9883'; // Comodoro/MT';
      5103353: CodTOM := '0131'; // Confresa/MT';
      5103361: CodTOM := '1082'; // Conquista D Oeste/MT';
      5103379: CodTOM := '0089'; // Cotriguacu/MT';
      5103403: CodTOM := '9067'; // Cuiaba/MT';
      5103437: CodTOM := '1084'; // Curvelandia/MT';
      5103452: CodTOM := '9833'; // Denise/MT';
      5103502: CodTOM := '9069'; // Diamantino/MT';
      5103601: CodTOM := '9071'; // Dom Aquino/MT';
      5103700: CodTOM := '1036'; // Feliz Natal/MT';
      5103809: CodTOM := '9881'; // Figueiropolis D Oeste/MT';
      5103858: CodTOM := '1038'; // Gaucha Do Norte/MT';
      5103908: CodTOM := '9077'; // General Carneiro/MT';
      5103957: CodTOM := '0135'; // Gloria D Oeste/MT';
      5104104: CodTOM := '9887'; // Guaranta Do Norte/MT';
      5104203: CodTOM := '9083'; // Guiratinga/MT';
      5104500: CodTOM := '9877'; // Indiavai/MT';
      5104526: CodTOM := '1184'; // Ipiranga Do Norte/MT';
      5104542: CodTOM := '1186'; // Itanhanga/MT';
      5104559: CodTOM := '9901'; // Itauba/MT';
      5104609: CodTOM := '9091'; // Itiquira/MT';
      5104807: CodTOM := '9095'; // Jaciara/MT';
      5104906: CodTOM := '9861'; // Jangada/MT';
      5105002: CodTOM := '8991'; // Jauru/MT';
      5105101: CodTOM := '9819'; // Juara/MT';
      5105150: CodTOM := '9831'; // Juina/MT';
      5105176: CodTOM := '9921'; // Juruena/MT';
      5105200: CodTOM := '9189'; // Juscimeira/MT';
      5105234: CodTOM := '0137'; // Lambari D Oeste/MT';
      5105259: CodTOM := '9925'; // Lucas Do Rio Verde/MT';
      5105309: CodTOM := '9105'; // Luciara/MT';
      5105507: CodTOM := '9109'; // Vila Bela Da Santissima Trindade/MT';
      5105580: CodTOM := '9899'; // Marcelandia/MT';
      5105606: CodTOM := '9929'; // Matupa/MT';
      5105622: CodTOM := '9177'; // Mirassol D Oeste/MT';
      5105903: CodTOM := '9117'; // Nobres/MT';
      5106000: CodTOM := '9119'; // Nortelandia/MT';
      5106109: CodTOM := '9121'; // Nossa Senhora Do Livramento/MT';
      5106158: CodTOM := '0117'; // Nova Bandeirantes/MT';
      5106174: CodTOM := '1086'; // Nova Nazare/MT';
      5106182: CodTOM := '1040'; // Nova Lacerda/MT';
      5106190: CodTOM := '1088'; // Nova Santa Helena/MT';
      5106208: CodTOM := '8981'; // Nova Brasilandia/MT';
      5106216: CodTOM := '9889'; // Nova Canaa Do Norte/MT';
      5106224: CodTOM := '9937'; // Nova Mutum/MT';
      5106232: CodTOM := '9893'; // Nova Olimpia/MT';
      5106240: CodTOM := '1042'; // Nova Ubirata/MT';
      5106257: CodTOM := '9195'; // Nova Xavantina/MT';
      5106265: CodTOM := '1044'; // Novo Mundo/MT';
      5106273: CodTOM := '9903'; // Novo Horizonte Do Norte/MT';
      5106281: CodTOM := '9867'; // Novo Sao Joaquim/MT';
      5106299: CodTOM := '9885'; // Paranaita/MT';
      5106307: CodTOM := '8983'; // Paranatinga/MT';
      5106315: CodTOM := '1090'; // Novo Santo Antonio/MT';
      5106372: CodTOM := '9181'; // Pedra Preta/MT';
      5106422: CodTOM := '9891'; // Peixoto De Azevedo/MT';
      5106455: CodTOM := '0091'; // Planalto Da Serra/MT';
      5106505: CodTOM := '9129'; // Pocone/MT';
      5106653: CodTOM := '0095'; // Pontal Do Araguaia/MT';
      5106703: CodTOM := '9133'; // Ponte Branca/MT';
      5106752: CodTOM := '8999'; // Pontes E Lacerda/MT';
      5106778: CodTOM := '9895'; // Porto Alegre Do Norte/MT';
      5106802: CodTOM := '9135'; // Porto Dos Gauchos/MT';
      5106828: CodTOM := '9875'; // Porto Esperidiao/MT';
      5106851: CodTOM := '0101'; // Porto Estrela/MT';
      5107008: CodTOM := '9139'; // Poxoreo/MT';
      5107040: CodTOM := '9871'; // Primavera Do Leste/MT';
      5107065: CodTOM := '0097'; // Querencia/MT';
      5107107: CodTOM := '8993'; // Sao Jose Dos Quatro Marcos/MT';
      5107156: CodTOM := '9879'; // Reserva Do Cabacal/MT';
      5107180: CodTOM := '9741'; // Ribeirao Cascalheira/MT';
      5107198: CodTOM := '0099'; // Ribeiraozinho/MT';
      5107206: CodTOM := '8995'; // Rio Branco/MT';
      5107248: CodTOM := '0123'; // Santa Carmem/MT';
      5107263: CodTOM := '0115'; // Santo Afonso/MT';
      5107297: CodTOM := '6087'; // Sao Jose Do Povo/MT';
      5107305: CodTOM := '9199'; // Sao Jose Do Rio Claro/MT';
      5107354: CodTOM := '0133'; // Sao Jose Do Xingu/MT';
      5107404: CodTOM := '0093'; // Sao Pedro Da Cipa/MT';
      5107578: CodTOM := '1092'; // Rondolandia/MT';
      5107602: CodTOM := '9151'; // Rondonopolis/MT';
      5107701: CodTOM := '9153'; // Rosario Oeste/MT';
      5107743: CodTOM := '1094'; // Santa Cruz Do Xingu/MT';
      5107750: CodTOM := '8997'; // Salto Do Ceu/MT';
      5107768: CodTOM := '1096'; // Santa Rita Do Trivelato/MT';
      5107776: CodTOM := '9197'; // Santa Terezinha/MT';
      5107792: CodTOM := '1098'; // Santo Antonio Do Leste/MT';
      5107800: CodTOM := '9155'; // Santo Antonio Do Leverger/MT';
      5107859: CodTOM := '9183'; // Sao Felix Do Araguaia/MT';
      5107875: CodTOM := '1046'; // Sapezal/MT';
      5107883: CodTOM := '1100'; // Serra Nova Dourada/MT';
      5107909: CodTOM := '8985'; // Sinop/MT';
      5107925: CodTOM := '9907'; // Sorriso/MT';
      5107941: CodTOM := '0125'; // Tabapora/MT';
      5107958: CodTOM := '9185'; // Tangara Da Serra/MT';
      5108006: CodTOM := '9763'; // Tapurah/MT';
      5108055: CodTOM := '9909'; // Terra Nova Do Norte/MT';
      5108105: CodTOM := '9161'; // Tesouro/MT';
      5108204: CodTOM := '9163'; // Torixoreu/MT';
      5108303: CodTOM := '1048'; // Uniao Do Sul/MT';
      5108352: CodTOM := '1102'; // Vale De Sao Domingos/MT';
      5108402: CodTOM := '9167'; // Varzea Grande/MT';
      5108501: CodTOM := '9905'; // Vera/MT';
      5108600: CodTOM := '9897'; // Vila Rica/MT';
      5108808: CodTOM := '0121'; // Nova Guarita/MT';
      5108857: CodTOM := '0103'; // Nova Marilandia/MT';
      5108907: CodTOM := '0111'; // Nova Maringa/MT';
      5108956: CodTOM := '0119'; // Nova Monte Verde/MT';
   end;
 end;

 procedure P52;
 begin
   case ACodigo of
      5200050: CodTOM := '1050'; // Abadia De Goias/GO';
      5200100: CodTOM := '9201'; // Abadiania/GO';
      5200134: CodTOM := '9645'; // Acreuna/GO';
      5200159: CodTOM := '9769'; // Adelandia/GO';
      5200175: CodTOM := '9771'; // Agua Fria De Goias/GO';
      5200209: CodTOM := '9203'; // Agua Limpa/GO';
      5200258: CodTOM := '1052'; // Aguas Lindas De Goias/GO';
      5200308: CodTOM := '9205'; // Alexania/GO';
      5200506: CodTOM := '9209'; // Aloandia/GO';
      5200555: CodTOM := '0085'; // Alto Horizonte/GO';
      5200605: CodTOM := '9211'; // Alto Paraiso De Goias/GO';
      5200803: CodTOM := '9215'; // Alvorada Do Norte/GO';
      5200829: CodTOM := '1054'; // Amaralina/GO';
      5200852: CodTOM := '9661'; // Americano Do Brasil/GO';
      5200902: CodTOM := '9217'; // Amorinopolis/GO';
      5201108: CodTOM := '9221'; // Anapolis/GO';
      5201207: CodTOM := '9223'; // Anhanguera/GO';
      5201306: CodTOM := '9225'; // Anicuns/GO';
      5201405: CodTOM := '9227'; // Aparecida De Goiania/GO';
      5201454: CodTOM := '0071'; // Aparecida Do Rio Doce/GO';
      5201504: CodTOM := '9229'; // Apore/GO';
      5201603: CodTOM := '9231'; // Aracu/GO';
      5201702: CodTOM := '9233'; // Aragarcas/GO';
      5201801: CodTOM := '9235'; // Aragoiania/GO';
      5202155: CodTOM := '9669'; // Araguapaz/GO';
      5202353: CodTOM := '9671'; // Arenopolis/GO';
      5202502: CodTOM := '9249'; // Aruana/GO';
      5202601: CodTOM := '9251'; // Aurilandia/GO';
      5202809: CodTOM := '9255'; // Avelinopolis/GO';
      5203104: CodTOM := '9261'; // Baliza/GO';
      5203203: CodTOM := '9263'; // Barro Alto/GO';
      5203302: CodTOM := '9265'; // Bela Vista De Goias/GO';
      5203401: CodTOM := '9267'; // Bom Jardim De Goias/GO';
      5203500: CodTOM := '9269'; // Bom Jesus De Goias/GO';
      5203559: CodTOM := '9775'; // Bonfinopolis/GO';
      5203575: CodTOM := '1056'; // Bonopolis/GO';
      5203609: CodTOM := '9271'; // Brazabrantes/GO';
      5203807: CodTOM := '9275'; // Britania/GO';
      5203906: CodTOM := '9277'; // Buriti Alegre/GO';
      5203939: CodTOM := '0063'; // Buriti De Goias/GO';
      5203962: CodTOM := '0061'; // Buritinopolis/GO';
      5204003: CodTOM := '9279'; // Cabeceiras/GO';
      5204102: CodTOM := '9281'; // Cachoeira Alta/GO';
      5204201: CodTOM := '9283'; // Cachoeira De Goias/GO';
      5204250: CodTOM := '9673'; // Cachoeira Dourada/GO';
      5204300: CodTOM := '9285'; // Cacu/GO';
      5204409: CodTOM := '9287'; // Caiaponia/GO';
      5204508: CodTOM := '9289'; // Caldas Novas/GO';
      5204557: CodTOM := '0031'; // Caldazinha/GO';
      5204607: CodTOM := '9291'; // Campestre De Goias/GO';
      5204656: CodTOM := '9687'; // Campinacu/GO';
      5204706: CodTOM := '9293'; // Campinorte/GO';
      5204805: CodTOM := '9295'; // Campo Alegre De Goias/GO';
      5204854: CodTOM := '1070'; // Campo Limpo De Goias/GO';
      5204904: CodTOM := '9297'; // Campos Belos/GO';
      5204953: CodTOM := '9781'; // Campos Verdes/GO';
      5205000: CodTOM := '9299'; // Carmo Do Rio Verde/GO';
      5205059: CodTOM := '0081'; // Castelandia/GO';
      5205109: CodTOM := '9301'; // Catalao/GO';
      5205208: CodTOM := '9303'; // Caturai/GO';
      5205307: CodTOM := '9305'; // Cavalcante/GO';
      5205406: CodTOM := '9307'; // Ceres/GO';
      5205455: CodTOM := '9785'; // Cezarina/GO';
      5205471: CodTOM := '0073'; // Chapadao Do Ceu/GO';
      5205497: CodTOM := '0077'; // Cidade Ocidental/GO';
      5205513: CodTOM := '0055'; // Cocalzinho De Goias/GO';
      5205521: CodTOM := '9791'; // Colinas Do Sul/GO';
      5205703: CodTOM := '9315'; // Corrego Do Ouro/GO';
      5205802: CodTOM := '9317'; // Corumba De Goias/GO';
      5205901: CodTOM := '9319'; // Corumbaiba/GO';
      5206206: CodTOM := '9325'; // Cristalina/GO';
      5206305: CodTOM := '9327'; // Cristianopolis/GO';
      5206404: CodTOM := '9329'; // Crixas/GO';
      5206503: CodTOM := '9331'; // Crominia/GO';
      5206602: CodTOM := '9333'; // Cumari/GO';
      5206701: CodTOM := '9335'; // Damianopolis/GO';
      5206800: CodTOM := '9337'; // Damolandia/GO';
      5206909: CodTOM := '9339'; // Davinopolis/GO';
      5207105: CodTOM := '9343'; // Diorama/GO';
      5207253: CodTOM := '9675'; // Doverlandia/GO';
      5207352: CodTOM := '9795'; // Edealina/GO';
      5207402: CodTOM := '9349'; // Edeia/GO';
      5207501: CodTOM := '9351'; // Estrela Do Norte/GO';
      5207535: CodTOM := '9797'; // Faina/GO';
      5207600: CodTOM := '9353'; // Fazenda Nova/GO';
      5207808: CodTOM := '9357'; // Firminopolis/GO';
      5207907: CodTOM := '9359'; // Flores De Goias/GO';
      5208004: CodTOM := '9361'; // Formosa/GO';
      5208103: CodTOM := '9363'; // Formoso/GO';
      5208152: CodTOM := '1072'; // Gameleira De Goias/GO';
      5208301: CodTOM := '9309'; // Divinopolis De Goias/GO';
      5208400: CodTOM := '9367'; // Goianapolis/GO';
      5208509: CodTOM := '9369'; // Goiandira/GO';
      5208608: CodTOM := '9371'; // Goianesia/GO';
      5208707: CodTOM := '9373'; // Goiania/GO';
      5208806: CodTOM := '9375'; // Goianira/GO';
      5208905: CodTOM := '9377'; // Goias/GO';
      5209101: CodTOM := '9379'; // Goiatuba/GO';
      5209150: CodTOM := '9799'; // Gouvelandia/GO';
      5209200: CodTOM := '9381'; // Guapo/GO';
      5209291: CodTOM := '0065'; // Guaraita/GO';
      5209408: CodTOM := '9383'; // Guarani De Goias/GO';
      5209457: CodTOM := '9993'; // Guarinos/GO';
      5209606: CodTOM := '9387'; // Heitorai/GO';
      5209705: CodTOM := '9389'; // Hidrolandia/GO';
      5209804: CodTOM := '9391'; // Hidrolina/GO';
      5209903: CodTOM := '9393'; // Iaciara/GO';
      5209937: CodTOM := '0069'; // Inaciolandia/GO';
      5209952: CodTOM := '9681'; // Indiara/GO';
      5210000: CodTOM := '9395'; // Inhumas/GO';
      5210109: CodTOM := '9397'; // Ipameri/GO';
      5210158: CodTOM := '1074'; // Ipiranga De Goias/GO';
      5210208: CodTOM := '9399'; // Ipora/GO';
      5210307: CodTOM := '9401'; // Israelandia/GO';
      5210406: CodTOM := '9403'; // Itaberai/GO';
      5210562: CodTOM := '9919'; // Itaguari/GO';
      5210604: CodTOM := '9407'; // Itaguaru/GO';
      5210802: CodTOM := '9411'; // Itaja/GO';
      5210901: CodTOM := '9413'; // Itapaci/GO';
      5211008: CodTOM := '9415'; // Itapirapua/GO';
      5211206: CodTOM := '9419'; // Itapuranga/GO';
      5211305: CodTOM := '9421'; // Itaruma/GO';
      5211404: CodTOM := '9423'; // Itaucu/GO';
      5211503: CodTOM := '9425'; // Itumbiara/GO';
      5211602: CodTOM := '9427'; // Ivolandia/GO';
      5211701: CodTOM := '9429'; // Jandaia/GO';
      5211800: CodTOM := '9431'; // Jaragua/GO';
      5211909: CodTOM := '9433'; // Jatai/GO';
      5212006: CodTOM := '9435'; // Jaupaci/GO';
      5212055: CodTOM := '0049'; // Jesupolis/GO';
      5212105: CodTOM := '9437'; // Joviania/GO';
      5212204: CodTOM := '9439'; // Jussara/GO';
      5212253: CodTOM := '1076'; // Lagoa Santa/GO';
      5212303: CodTOM := '9443'; // Leopoldo De Bulhoes/GO';
      5212501: CodTOM := '9445'; // Luziania/GO';
      5212600: CodTOM := '9447'; // Mairipotaba/GO';
      5212709: CodTOM := '9449'; // Mambai/GO';
      5212808: CodTOM := '9451'; // Mara Rosa/GO';
      5212907: CodTOM := '9453'; // Marzagao/GO';
      5212956: CodTOM := '9927'; // Matrincha/GO';
      5213004: CodTOM := '9457'; // Maurilandia/GO';
      5213053: CodTOM := '9931'; // Mimoso De Goias/GO';
      5213087: CodTOM := '9647'; // Minacu/GO';
      5213103: CodTOM := '9459'; // Mineiros/GO';
      5213400: CodTOM := '9465'; // Moipora/GO';
      5213509: CodTOM := '9467'; // Monte Alegre De Goias/GO';
      5213707: CodTOM := '9471'; // Montes Claros De Goias/GO';
      5213756: CodTOM := '9933'; // Montividiu/GO';
      5213772: CodTOM := '0079'; // Montividiu Do Norte/GO';
      5213806: CodTOM := '9473'; // Morrinhos/GO';
      5213855: CodTOM := '9935'; // Morro Agudo De Goias/GO';
      5213905: CodTOM := '9475'; // Mossamedes/GO';
      5214002: CodTOM := '9477'; // Mozarlandia/GO';
      5214051: CodTOM := '9651'; // Mundo Novo/GO';
      5214101: CodTOM := '9479'; // Mutunopolis/GO';
      5214408: CodTOM := '9485'; // Nazario/GO';
      5214507: CodTOM := '9487'; // Neropolis/GO';
      5214606: CodTOM := '9489'; // Niquelandia/GO';
      5214705: CodTOM := '9491'; // Nova America/GO';
      5214804: CodTOM := '9493'; // Nova Aurora/GO';
      5214838: CodTOM := '9653'; // Nova Crixas/GO';
      5214861: CodTOM := '9655'; // Nova Gloria/GO';
      5214879: CodTOM := '0087'; // Nova Iguacu De Goias/GO';
      5214903: CodTOM := '9495'; // Nova Roma/GO';
      5215009: CodTOM := '9497'; // Nova Veneza/GO';
      5215207: CodTOM := '9501'; // Novo Brasil/GO';
      5215231: CodTOM := '1058'; // Novo Gama/GO';
      5215256: CodTOM := '9735'; // Novo Planalto/GO';
      5215306: CodTOM := '9503'; // Orizona/GO';
      5215405: CodTOM := '9505'; // Ouro Verde De Goias/GO';
      5215504: CodTOM := '9507'; // Ouvidor/GO';
      5215603: CodTOM := '9509'; // Padre Bernardo/GO';
      5215652: CodTOM := '9737'; // Palestina De Goias/GO';
      5215702: CodTOM := '9511'; // Palmeiras De Goias/GO';
      5215801: CodTOM := '9513'; // Palmelo/GO';
      5215900: CodTOM := '9515'; // Palminopolis/GO';
      5216007: CodTOM := '9517'; // Panama/GO';
      5216304: CodTOM := '9455'; // Paranaiguara/GO';
      5216403: CodTOM := '9523'; // Parauna/GO';
      5216452: CodTOM := '0075'; // Perolandia/GO';
      5216809: CodTOM := '9531'; // Petrolina De Goias/GO';
      5216908: CodTOM := '9535'; // Pilar De Goias/GO';
      5217104: CodTOM := '9539'; // Piracanjuba/GO';
      5217203: CodTOM := '9541'; // Piranhas/GO';
      5217302: CodTOM := '9543'; // Pirenopolis/GO';
      5217401: CodTOM := '9545'; // Pires Do Rio/GO';
      5217609: CodTOM := '9595'; // Planaltina/GO';
      5217708: CodTOM := '9549'; // Pontalina/GO';
      5218003: CodTOM := '9555'; // Porangatu/GO';
      5218052: CodTOM := '1060'; // Porteirao/GO';
      5218102: CodTOM := '9557'; // Portelandia/GO';
      5218300: CodTOM := '9561'; // Posse/GO';
      5218391: CodTOM := '0051'; // Professor Jamil/GO';
      5218508: CodTOM := '9563'; // Quirinopolis/GO';
      5218607: CodTOM := '9565'; // Rialma/GO';
      5218706: CodTOM := '9567'; // Rianapolis/GO';
      5218789: CodTOM := '9995'; // Rio Quente/GO';
      5218805: CodTOM := '9571'; // Rio Verde/GO';
      5218904: CodTOM := '9573'; // Rubiataba/GO';
      5219001: CodTOM := '9575'; // Sanclerlandia/GO';
      5219100: CodTOM := '9577'; // Santa Barbara De Goias/GO';
      5219209: CodTOM := '9579'; // Santa Cruz De Goias/GO';
      5219258: CodTOM := '9743'; // Santa Fe De Goias/GO';
      5219308: CodTOM := '9581'; // Santa Helena De Goias/GO';
      5219357: CodTOM := '9689'; // Santa Isabel/GO';
      5219407: CodTOM := '9583'; // Santa Rita Do Araguaia/GO';
      5219456: CodTOM := '1062'; // Santa Rita Do Novo Destino/GO';
      5219506: CodTOM := '9585'; // Santa Rosa De Goias/GO';
      5219605: CodTOM := '9587'; // Santa Tereza De Goias/GO';
      5219704: CodTOM := '9589'; // Santa Terezinha De Goias/GO';
      5219712: CodTOM := '0083'; // Santo Antonio Da Barra/GO';
      5219738: CodTOM := '0053'; // Santo Antonio De Goias/GO';
      5219753: CodTOM := '9677'; // Santo Antonio Do Descoberto/GO';
      5219803: CodTOM := '9591'; // Sao Domingos/GO';
      5219902: CodTOM := '9593'; // Sao Francisco De Goias/GO';
      5220009: CodTOM := '9597'; // Sao Joao D Alianca/GO';
      5220058: CodTOM := '9747'; // Sao Joao Da Parauna/GO';
      5220108: CodTOM := '9599'; // Sao Luis De Montes Belos/GO';
      5220157: CodTOM := '9749'; // Sao Luiz Do Norte/GO';
      5220207: CodTOM := '9601'; // Sao Miguel Do Araguaia/GO';
      5220264: CodTOM := '9751'; // Sao Miguel Do Passa Quatro/GO';
      5220280: CodTOM := '1064'; // Sao Patricio/GO';
      5220405: CodTOM := '9605'; // Sao Simao/GO';
      5220454: CodTOM := '9753'; // Senador Canedo/GO';
      5220504: CodTOM := '9607'; // Serranopolis/GO';
      5220603: CodTOM := '9609'; // Silvania/GO';
      5220686: CodTOM := '9755'; // Simolandia/GO';
      5220702: CodTOM := '9611'; // Sitio D Abadia/GO';
      5221007: CodTOM := '9617'; // Taquaral De Goias/GO';
      5221080: CodTOM := '9759'; // Teresina De Goias/GO';
      5221197: CodTOM := '0057'; // Terezopolis De Goias/GO';
      5221304: CodTOM := '9623'; // Tres Ranchos/GO';
      5221403: CodTOM := '9625'; // Trindade/GO';
      5221452: CodTOM := '9761'; // Trombas/GO';
      5221502: CodTOM := '9631'; // Turvania/GO';
      5221551: CodTOM := '9765'; // Turvelandia/GO';
      5221577: CodTOM := '0059'; // Uirapuru/GO';
      5221601: CodTOM := '9633'; // Uruacu/GO';
      5221700: CodTOM := '9635'; // Uruana/GO';
      5221809: CodTOM := '9637'; // Urutai/GO';
      5221858: CodTOM := '1066'; // Valparaiso De Goias/GO';
      5221908: CodTOM := '9639'; // Varjao/GO';
      5222005: CodTOM := '9641'; // Vianopolis/GO';
      5222054: CodTOM := '9657'; // Vicentinopolis/GO';
      5222203: CodTOM := '0067'; // Vila Boa/GO';
      5222302: CodTOM := '1068'; // Vila Propicio/GO';
   end;
 end;

 procedure P53;
 begin
   case ACodigo of
      5300108: CodTOM := '9701'; // Brasilia/DF';
   end;
 end;

begin
  CodTOM := '';

  if (ACodigo >= 0) and (ACodigo < 1100015) then P00
  else if (ACodigo >= 1100015) and (ACodigo <= 1101807) then P11
  else if (ACodigo >= 1200013) and (ACodigo <= 1200807) then P12
  else if (ACodigo >= 1300029) and (ACodigo <= 1304401) then P13
  else if (ACodigo >= 1400027) and (ACodigo <= 1400704) then P14
  else if (ACodigo >= 1500107) and (ACodigo <= 1508407) then P15
  else if (ACodigo >= 1600055) and (ACodigo <= 1600808) then P16
  else if (ACodigo >= 1700251) and (ACodigo <= 1722107) then P17
  else if (ACodigo >= 2100055) and (ACodigo <= 2114007) then P21
  else if (ACodigo >= 2200053) and (ACodigo <= 2211704) then P22
  else if (ACodigo >= 2300101) and (ACodigo <= 2314102) then P23
  else if (ACodigo >= 2400109) and (ACodigo <= 2415008) then P24
  else if (ACodigo >= 2500106) and (ACodigo <= 2517407) then P25
  else if (ACodigo >= 2600054) and (ACodigo <= 2616506) then P26
  else if (ACodigo >= 2700102) and (ACodigo <= 2709400) then P27
  else if (ACodigo >= 2800100) and (ACodigo <= 2807600) then P28
  else if (ACodigo >= 2900108) and (ACodigo <= 2933604) then P29
  else if (ACodigo >= 3100104) and (ACodigo <= 3172202) then P31
  else if (ACodigo >= 3200102) and (ACodigo <= 3205309) then P32
  else if (ACodigo >= 3300100) and (ACodigo <= 3306305) then P33
  else if (ACodigo >= 3500105) and (ACodigo <= 3557303) then P35
  else if (ACodigo >= 4100103) and (ACodigo <= 4128807) then P41
  else if (ACodigo >= 4200051) and (ACodigo <= 4219853) then P42
  else if (ACodigo >= 4300034) and (ACodigo <= 4323804) then P43
  else if (ACodigo >= 5000203) and (ACodigo <= 5008404) then P50
  else if (ACodigo >= 5100102) and (ACodigo <= 5108956) then P51
  else if (ACodigo >= 5200050) and (ACodigo <= 5222302) then P52
  else P53;

 Result := CodTOM;

 if (Trim(Result) = '') and (ACodigo > 0) and (ACodigo <= 9999) then
   Result:= IntToStr(ACodigo);
end;

function CodTOMToCodIBGE(const ACodigo: string): string;
var
  CodTOM, CodIBGE: integer;
begin
  CodTOM := strtoint64(ACodigo);

  case CodTOM of
    0033: CodIBGE := 1100015; // Alta Floresta D Oeste/RO
    0007: CodIBGE := 1100023; // Ariquemes/RO
    0037: CodIBGE := 1100031; // Cabixi/RO
    0009: CodIBGE := 1100049; // Cacoal/RO
    0027: CodIBGE := 1100056; // Cerejeiras/RO
    0023: CodIBGE := 1100064; // Colorado Do Oeste/RO
    0981: CodIBGE := 1100072; // Corumbiara/RO
    0021: CodIBGE := 1100080; // Costa Marques/RO
    0025: CodIBGE := 1100098; // Espigao D Oeste/RO
    0001: CodIBGE := 1100106; // Guajara-Mirim/RO
    0015: CodIBGE := 1100114; // Jaru/RO
    0005: CodIBGE := 1100122; // Ji-Parana/RO
    0039: CodIBGE := 1100130; // Machadinho Doeste/RO
    0041: CodIBGE := 1100148; // Nova Brasilandia D Oeste/RO
    0017: CodIBGE := 1100155; // Ouro Preto Do Oeste/RO
    0011: CodIBGE := 1100189; // Pimenta Bueno/RO
    0003: CodIBGE := 1100205; // Porto Velho/RO
    0019: CodIBGE := 1100254; // Presidente Medici/RO
    0687: CodIBGE := 1100262; // Rio Crespo/RO
    0029: CodIBGE := 1100288; // Rolim De Moura/RO
    0043: CodIBGE := 1100296; // Santa Luzia D Oeste/RO
    0013: CodIBGE := 1100304; // Vilhena/RO
    0045: CodIBGE := 1100320; // Sao Miguel Do Guapore/RO
    0047: CodIBGE := 1100338; // Nova Mamore/RO
    0035: CodIBGE := 1100346; // Alvorada D Oeste/RO
    0002: CodIBGE := 1100379; // Alto Alegre Dos Parecis/RO
    0675: CodIBGE := 1100403; // Alto Paraiso/RO
    0004: CodIBGE := 1100452; // Buritis/RO
    0689: CodIBGE := 1100502; // Novo Horizonte Do Oeste/RO
    0677: CodIBGE := 1100601; // Cacaulandia/RO
    0679: CodIBGE := 1100700; // Campo Novo De Rondonia/RO
    0681: CodIBGE := 1100809; // Candeias Do Jamari/RO
    0691: CodIBGE := 1100908; // Castanheiras/RO
    0006: CodIBGE := 1100924; // Chupinguaia/RO
    0008: CodIBGE := 1100940; // Cujubim/RO
    0693: CodIBGE := 1101005; // Governador Jorge Teixeira/RO
    0683: CodIBGE := 1101104; // Itapua Do Oeste/RO
    0695: CodIBGE := 1101203; // Ministro Andreazza/RO
    0697: CodIBGE := 1101302; // Mirante Da Serra/RO
    0685: CodIBGE := 1101401; // Monte Negro/RO
    0010: CodIBGE := 1101435; // Nova Uniao/RO
    0012: CodIBGE := 1101450; // Parecis/RO
    0014: CodIBGE := 1101468; // Pimenteiras Do Oeste/RO
    0016: CodIBGE := 1101476; // Primavera De Rondonia/RO
    0018: CodIBGE := 1101484; // Sao Felipe D Oeste/RO
    0020: CodIBGE := 1101492; // Sao Francisco Do Guapore/RO
    0699: CodIBGE := 1101500; // Seringueiras/RO
    0022: CodIBGE := 1101559; // Teixeiropolis/RO
    0975: CodIBGE := 1101609; // Theobroma/RO
    0977: CodIBGE := 1101708; // Urupa/RO
    0024: CodIBGE := 1101757; // Vale Do Anari/RO
    0979: CodIBGE := 1101807; // Vale Do Paraiso/RO
    0643: CodIBGE := 1200013; // Acrelandia/AC
    0157: CodIBGE := 1200054; // Assis Brasil/AC
    0105: CodIBGE := 1200104; // Brasileia/AC
    0645: CodIBGE := 1200138; // Bujari/AC
    0647: CodIBGE := 1200179; // Capixaba/AC
    0107: CodIBGE := 1200203; // Cruzeiro Do Sul/AC
    0651: CodIBGE := 1200252; // Epitaciolandia/AC
    0113: CodIBGE := 1200302; // Feijo/AC
    0653: CodIBGE := 1200328; // Jordao/AC
    0109: CodIBGE := 1200336; // Mancio Lima/AC
    0155: CodIBGE := 1200344; // Manoel Urbano/AC
    0655: CodIBGE := 1200351; // Marechal Thaumaturgo/AC
    0151: CodIBGE := 1200385; // Placido De Castro/AC
    0657: CodIBGE := 1200393; // Porto Walter/AC
    0139: CodIBGE := 1200401; // Rio Branco/AC
    0659: CodIBGE := 1200427; // Rodrigues Alves/AC
    0661: CodIBGE := 1200435; // Santa Rosa Do Purus/AC
    0153: CodIBGE := 1200450; // Senador Guiomard/AC
    0145: CodIBGE := 1200500; // Sena Madureira/AC
    0147: CodIBGE := 1200609; // Tarauaca/AC
    0149: CodIBGE := 1200708; // Xapuri/AC
    0649: CodIBGE := 1200807; // Porto Acre/AC
    0289: CodIBGE := 1300029; // Alvaraes/AM
    0291: CodIBGE := 1300060; // Amatura/AM
    0293: CodIBGE := 1300086; // Anama/AM
    0203: CodIBGE := 1300102; // Anori/AM
    0969: CodIBGE := 1300144; // Apui/AM
    0205: CodIBGE := 1300201; // Atalaia Do Norte/AM
    0207: CodIBGE := 1300300; // Autazes/AM
    0209: CodIBGE := 1300409; // Barcelos/AM
    0211: CodIBGE := 1300508; // Barreirinha/AM
    0213: CodIBGE := 1300607; // Benjamin Constant/AM
    0295: CodIBGE := 1300631; // Beruri/AM
    0297: CodIBGE := 1300680; // Boa Vista Do Ramos/AM
    0215: CodIBGE := 1300706; // Boca Do Acre/AM
    0217: CodIBGE := 1300805; // Borba/AM
    0299: CodIBGE := 1300839; // Caapiranga/AM
    0219: CodIBGE := 1300904; // Canutama/AM
    0221: CodIBGE := 1301001; // Carauari/AM
    0223: CodIBGE := 1301100; // Careiro/AM
    0965: CodIBGE := 1301159; // Careiro Da Varzea/AM
    0225: CodIBGE := 1301209; // Coari/AM
    0227: CodIBGE := 1301308; // Codajas/AM
    0229: CodIBGE := 1301407; // Eirunepe/AM
    0231: CodIBGE := 1301506; // Envira/AM
    0233: CodIBGE := 1301605; // Fonte Boa/AM
    0967: CodIBGE := 1301654; // Guajara/AM
    0235: CodIBGE := 1301704; // Humaita/AM
    0239: CodIBGE := 1301803; // Ipixuna/AM
    9835: CodIBGE := 1301852; // Iranduba/AM
    0241: CodIBGE := 1301902; // Itacoatiara/AM
    9837: CodIBGE := 1301951; // Itamarati/AM
    0243: CodIBGE := 1302009; // Itapiranga/AM
    0245: CodIBGE := 1302108; // Japura/AM
    0247: CodIBGE := 1302207; // Jurua/AM
    0249: CodIBGE := 1302306; // Jutai/AM
    0251: CodIBGE := 1302405; // Labrea/AM
    0253: CodIBGE := 1302504; // Manacapuru/AM
    9839: CodIBGE := 1302553; // Manaquiri/AM
    0255: CodIBGE := 1302603; // Manaus/AM
    0257: CodIBGE := 1302702; // Manicore/AM
    0259: CodIBGE := 1302801; // Maraa/AM
    0261: CodIBGE := 1302900; // Maues/AM
    0263: CodIBGE := 1303007; // Nhamunda/AM
    0265: CodIBGE := 1303106; // Nova Olinda Do Norte/AM
    0201: CodIBGE := 1303205; // Novo Airao/AM
    0267: CodIBGE := 1303304; // Novo Aripuana/AM
    0269: CodIBGE := 1303403; // Parintins/AM
    0271: CodIBGE := 1303502; // Pauini/AM
    9841: CodIBGE := 1303536; // Presidente Figueiredo/AM
    9843: CodIBGE := 1303569; // Rio Preto Da Eva/AM
    0237: CodIBGE := 1303601; // Santa Isabel Do Rio Negro/AM
    0273: CodIBGE := 1303700; // Santo Antonio Do Ica/AM
    0283: CodIBGE := 1303809; // Sao Gabriel Da Cachoeira/AM
    0275: CodIBGE := 1303908; // Sao Paulo De Olivenca/AM
    9845: CodIBGE := 1303957; // Sao Sebastiao Do Uatuma/AM
    0277: CodIBGE := 1304005; // Silves/AM
    9847: CodIBGE := 1304062; // Tabatinga/AM
    0279: CodIBGE := 1304104; // Tapaua/AM
    0281: CodIBGE := 1304203; // Tefe/AM
    9851: CodIBGE := 1304237; // Tonantins/AM
    9849: CodIBGE := 1304260; // Uarini/AM
    0285: CodIBGE := 1304302; // Urucara/AM
    0287: CodIBGE := 1304401; // Urucurituba/AM
    0026: CodIBGE := 1400027; // Amajari/RR
    0305: CodIBGE := 1400050; // Alto Alegre/RR
    0301: CodIBGE := 1400100; // Boa Vista/RR
    0307: CodIBGE := 1400159; // Bonfim/RR
    0028: CodIBGE := 1400175; // Canta/RR
    0303: CodIBGE := 1400209; // Caracarai/RR
    0030: CodIBGE := 1400233; // Caroebe/RR
    0032: CodIBGE := 1400282; // Iracema/RR
    0309: CodIBGE := 1400308; // Mucajai/RR
    0311: CodIBGE := 1400407; // Normandia/RR
    0034: CodIBGE := 1400456; // Pacaraima/RR
    0036: CodIBGE := 1400472; // Rorainopolis/RR
    0313: CodIBGE := 1400506; // Sao Joao Da Baliza/RR
    0315: CodIBGE := 1400605; // Sao Luiz/RR
    0038: CodIBGE := 1400704; // Uiramuta/RR
    0401: CodIBGE := 1500107; // Abaetetuba/PA
    0375: CodIBGE := 1500131; // Abel Figueiredo/PA
    0403: CodIBGE := 1500206; // Acara/PA
    0405: CodIBGE := 1500305; // Afua/PA
    0383: CodIBGE := 1500347; // Agua Azul Do Norte/PA
    0407: CodIBGE := 1500404; // Alenquer/PA
    0409: CodIBGE := 1500503; // Almeirim/PA
    0411: CodIBGE := 1500602; // Altamira/PA
    0413: CodIBGE := 1500701; // Anajas/PA
    0415: CodIBGE := 1500800; // Ananindeua/PA
    0040: CodIBGE := 1500859; // Anapu/PA
    0417: CodIBGE := 1500909; // Augusto Correa/PA
    0389: CodIBGE := 1500958; // Aurora Do Para/PA
    0419: CodIBGE := 1501006; // Aveiro/PA
    0421: CodIBGE := 1501105; // Bagre/PA
    0423: CodIBGE := 1501204; // Baiao/PA
    0042: CodIBGE := 1501253; // Bannach/PA
    0425: CodIBGE := 1501303; // Barcarena/PA
    0427: CodIBGE := 1501402; // Belem/PA
    0044: CodIBGE := 1501451; // Belterra/PA
    0429: CodIBGE := 1501501; // Benevides/PA
    0575: CodIBGE := 1501576; // Bom Jesus Do Tocantins/PA
    0431: CodIBGE := 1501600; // Bonito/PA
    0433: CodIBGE := 1501709; // Braganca/PA
    0639: CodIBGE := 1501725; // Brasil Novo/PA
    0577: CodIBGE := 1501758; // Brejo Grande Do Araguaia/PA
    0625: CodIBGE := 1501782; // Breu Branco/PA
    0435: CodIBGE := 1501808; // Breves/PA
    0437: CodIBGE := 1501907; // Bujaru/PA
    0046: CodIBGE := 1501956; // Cachoeira Do Piria/PA
    0439: CodIBGE := 1502004; // Cachoeira Do Arari/PA
    0441: CodIBGE := 1502103; // Cameta/PA
    0048: CodIBGE := 1502152; // Canaa Dos Carajas/PA
    0443: CodIBGE := 1502202; // Capanema/PA
    0445: CodIBGE := 1502301; // Capitao Poco/PA
    0447: CodIBGE := 1502400; // Castanhal/PA
    0449: CodIBGE := 1502509; // Chaves/PA
    0451: CodIBGE := 1502608; // Colares/PA
    0453: CodIBGE := 1502707; // Conceicao Do Araguaia/PA
    0579: CodIBGE := 1502756; // Concordia Do Para/PA
    0385: CodIBGE := 1502764; // Cumaru Do Norte/PA
    0581: CodIBGE := 1502772; // Curionopolis/PA
    0455: CodIBGE := 1502806; // Curralinho/PA
    0050: CodIBGE := 1502855; // Curua/PA
    0457: CodIBGE := 1502905; // Curuca/PA
    0583: CodIBGE := 1502939; // Dom Eliseu/PA
    0377: CodIBGE := 1502954; // Eldorado Dos Carajas/PA
    0459: CodIBGE := 1503002; // Faro/PA
    0052: CodIBGE := 1503044; // Floresta Do Araguaia/PA
    0585: CodIBGE := 1503077; // Garrafao Do Norte/PA
    0627: CodIBGE := 1503093; // Goianesia Do Para/PA
    0461: CodIBGE := 1503101; // Gurupa/PA
    0463: CodIBGE := 1503200; // Igarape-Acu/PA
    0465: CodIBGE := 1503309; // Igarape-Miri/PA
    0467: CodIBGE := 1503408; // Inhangapi/PA
    0621: CodIBGE := 1503457; // Ipixuna Do Para/PA
    0469: CodIBGE := 1503507; // Irituia/PA
    0471: CodIBGE := 1503606; // Itaituba/PA
    0473: CodIBGE := 1503705; // Itupiranga/PA
    0631: CodIBGE := 1503754; // Jacareacanga/PA
    0475: CodIBGE := 1503804; // Jacunda/PA
    0477: CodIBGE := 1503903; // Juruti/PA
    0479: CodIBGE := 1504000; // Limoeiro Do Ajuru/PA
    0587: CodIBGE := 1504059; // Mae Do Rio/PA
    0481: CodIBGE := 1504109; // Magalhaes Barata/PA
    0483: CodIBGE := 1504208; // Maraba/PA
    0485: CodIBGE := 1504307; // Maracana/PA
    0487: CodIBGE := 1504406; // Marapanim/PA
    0054: CodIBGE := 1504422; // Marituba/PA
    0589: CodIBGE := 1504455; // Medicilandia/PA
    0489: CodIBGE := 1504505; // Melgaco/PA
    0491: CodIBGE := 1504604; // Mocajuba/PA
    0493: CodIBGE := 1504703; // Moju/PA
    1190: CodIBGE := 1504752; // Mojuí dos Campos/PA
    0495: CodIBGE := 1504802; // Monte Alegre/PA
    0497: CodIBGE := 1504901; // Muana/PA
    0391: CodIBGE := 1504950; // Nova Esperanca Do Piria/PA
    0056: CodIBGE := 1504976; // Nova Ipixuna/PA
    0499: CodIBGE := 1505007; // Nova Timboteua/PA
    0633: CodIBGE := 1505031; // Novo Progresso/PA
    0629: CodIBGE := 1505064; // Novo Repartimento/PA
    0501: CodIBGE := 1505106; // Obidos/PA
    0503: CodIBGE := 1505205; // Oeiras Do Para/PA
    0505: CodIBGE := 1505304; // Oriximina/PA
    0507: CodIBGE := 1505403; // Ourem/PA
    0591: CodIBGE := 1505437; // Ourilandia Do Norte/PA
    0593: CodIBGE := 1505486; // Pacaja/PA
    0379: CodIBGE := 1505494; // Palestina Do Para/PA
    0509: CodIBGE := 1505502; // Paragominas/PA
    0595: CodIBGE := 1505536; // Parauapebas/PA
    0387: CodIBGE := 1505551; // Pau D Arco/PA
    0511: CodIBGE := 1505601; // Peixe-Boi/PA
    0058: CodIBGE := 1505635; // Picarra/PA
    0060: CodIBGE := 1505650; // Placas/PA
    0513: CodIBGE := 1505700; // Ponta De Pedras/PA
    0515: CodIBGE := 1505809; // Portel/PA
    0517: CodIBGE := 1505908; // Porto De Moz/PA
    0519: CodIBGE := 1506005; // Prainha/PA
    0521: CodIBGE := 1506104; // Primavera/PA
    0062: CodIBGE := 1506112; // Quatipuru/PA
    0567: CodIBGE := 1506138; // Redencao/PA
    0569: CodIBGE := 1506161; // Rio Maria/PA
    0573: CodIBGE := 1506187; // Rondon Do Para/PA
    0597: CodIBGE := 1506195; // Ruropolis/PA
    0523: CodIBGE := 1506203; // Salinopolis/PA
    0525: CodIBGE := 1506302; // Salvaterra/PA
    0369: CodIBGE := 1506351; // Santa Barbara Do Para/PA
    0527: CodIBGE := 1506401; // Santa Cruz Do Arari/PA
    0529: CodIBGE := 1506500; // Santa Isabel Do Para/PA
    0371: CodIBGE := 1506559; // Santa Luzia Do Para/PA
    0599: CodIBGE := 1506583; // Santa Maria Das Barreiras/PA
    0531: CodIBGE := 1506609; // Santa Maria Do Para/PA
    0533: CodIBGE := 1506708; // Santana Do Araguaia/PA
    0535: CodIBGE := 1506807; // Santarem/PA
    0537: CodIBGE := 1506906; // Santarem Novo/PA
    0539: CodIBGE := 1507003; // Santo Antonio Do Taua/PA
    0541: CodIBGE := 1507102; // Sao Caetano De Odivelas/PA
    0381: CodIBGE := 1507151; // Sao Domingos Do Araguaia/PA
    0543: CodIBGE := 1507201; // Sao Domingos Do Capim/PA
    0545: CodIBGE := 1507300; // Sao Felix Do Xingu/PA
    0547: CodIBGE := 1507409; // Sao Francisco Do Para/PA
    0619: CodIBGE := 1507458; // Sao Geraldo Do Araguaia/PA
    0064: CodIBGE := 1507466; // Sao Joao Da Ponta/PA
    0393: CodIBGE := 1507474; // Sao Joao De Pirabas/PA
    0549: CodIBGE := 1507508; // Sao Joao Do Araguaia/PA
    0551: CodIBGE := 1507607; // Sao Miguel Do Guama/PA
    0553: CodIBGE := 1507706; // Sao Sebastiao Da Boa Vista/PA
    0066: CodIBGE := 1507755; // Sapucaia/PA
    0555: CodIBGE := 1507805; // Senador Jose Porfirio/PA
    0557: CodIBGE := 1507904; // Soure/PA
    0395: CodIBGE := 1507953; // Tailandia/PA
    0373: CodIBGE := 1507961; // Terra Alta/PA
    0637: CodIBGE := 1507979; // Terra Santa/PA
    0559: CodIBGE := 1508001; // Tome-Acu/PA
    0068: CodIBGE := 1508035; // Tracuateua/PA
    0635: CodIBGE := 1508050; // Trairao/PA
    0397: CodIBGE := 1508084; // Tucuma/PA
    0561: CodIBGE := 1508100; // Tucurui/PA
    0623: CodIBGE := 1508126; // Ulianopolis/PA
    0399: CodIBGE := 1508159; // Uruara/PA
    0563: CodIBGE := 1508209; // Vigia/PA
    0565: CodIBGE := 1508308; // Viseu/PA
    0641: CodIBGE := 1508357; // Vitoria Do Xingu/PA
    0571: CodIBGE := 1508407; // Xinguara/PA
    0665: CodIBGE := 1600055; // Serra Do Navio/AP
    0601: CodIBGE := 1600105; // Amapa/AP
    0663: CodIBGE := 1600154; // Pedra Branca Do Amapari/AP
    0603: CodIBGE := 1600204; // Calcoene/AP
    0667: CodIBGE := 1600212; // Cutias/AP
    0611: CodIBGE := 1600238; // Ferreira Gomes/AP
    0669: CodIBGE := 1600253; // Itaubal/AP
    0613: CodIBGE := 1600279; // Laranjal Do Jari/AP
    0605: CodIBGE := 1600303; // Macapa/AP
    0607: CodIBGE := 1600402; // Mazagao/AP
    0609: CodIBGE := 1600501; // Oiapoque/AP
    0671: CodIBGE := 1600535; // Porto Grande/AP
    0673: CodIBGE := 1600550; // Pracuuba/AP
    0615: CodIBGE := 1600600; // Santana/AP
    0617: CodIBGE := 1600709; // Tartarugalzinho/AP
    0070: CodIBGE := 1600808; // Vitoria Do Jari/AP
    0337: CodIBGE := 1700251; // Abreulandia/TO
    0072: CodIBGE := 1700301; // Aguiarnopolis/TO
    9441: CodIBGE := 1700350; // Alianca Do Tocantins/TO
    9207: CodIBGE := 1700400; // Almas/TO
    9213: CodIBGE := 1700707; // Alvorada/TO
    9219: CodIBGE := 1701002; // Ananas/TO
    0165: CodIBGE := 1701051; // Angico/TO
    9713: CodIBGE := 1701101; // Aparecida Do Rio Negro/TO
    0167: CodIBGE := 1701309; // Aragominas/TO
    9237: CodIBGE := 1701903; // Araguacema/TO
    9239: CodIBGE := 1702000; // Araguacu/TO
    9241: CodIBGE := 1702109; // Araguaina/TO
    0169: CodIBGE := 1702158; // Araguana/TO
    9243: CodIBGE := 1702208; // Araguatins/TO
    9245: CodIBGE := 1702307; // Arapoema/TO
    9247: CodIBGE := 1702406; // Arraias/TO
    9685: CodIBGE := 1702554; // Augustinopolis/TO
    9253: CodIBGE := 1702703; // Aurora Do Tocantins/TO
    9257: CodIBGE := 1702901; // Axixa Do Tocantins/TO
    9259: CodIBGE := 1703008; // Babaculandia/TO
    0074: CodIBGE := 1703057; // Bandeirantes Do Tocantins/TO
    0076: CodIBGE := 1703073; // Barra Do Ouro/TO
    9693: CodIBGE := 1703107; // Barrolandia/TO
    9695: CodIBGE := 1703206; // Bernardo Sayao/TO
    0341: CodIBGE := 1703305; // Bom Jesus Do Tocantins/TO
    0339: CodIBGE := 1703602; // Brasilandia Do Tocantins/TO
    9273: CodIBGE := 1703701; // Brejinho De Nazare/TO
    9715: CodIBGE := 1703800; // Buriti Do Tocantins/TO
    0171: CodIBGE := 1703826; // Cachoeirinha/TO
    0173: CodIBGE := 1703842; // Campos Lindos/TO
    0327: CodIBGE := 1703867; // Cariri Do Tocantins/TO
    0175: CodIBGE := 1703883; // Carmolandia/TO
    0177: CodIBGE := 1703891; // Carrasco Bonito/TO
    9717: CodIBGE := 1703909; // Caseara/TO
    0343: CodIBGE := 1704105; // Centenario/TO
    0078: CodIBGE := 1704600; // Chapada De Areia/TO
    0080: CodIBGE := 1705102; // Chapada Da Natividade/TO
    9311: CodIBGE := 1705508; // Colinas Do Tocantins/TO
    9697: CodIBGE := 1705557; // Combinado/TO
    9313: CodIBGE := 1705607; // Conceicao Do Tocantins/TO
    9321: CodIBGE := 1706001; // Couto Magalhaes/TO
    9323: CodIBGE := 1706100; // Cristalandia/TO
    0082: CodIBGE := 1706258; // Crixas Do Tocantins/TO
    0179: CodIBGE := 1706506; // Darcinopolis/TO
    9341: CodIBGE := 1707009; // Dianopolis/TO
    9719: CodIBGE := 1707108; // Divinopolis Do Tocantins/TO
    9345: CodIBGE := 1707207; // Dois Irmaos Do Tocantins/TO
    9347: CodIBGE := 1707306; // Duere/TO
    0181: CodIBGE := 1707405; // Esperantina/TO
    9683: CodIBGE := 1707553; // Fatima/TO
    9667: CodIBGE := 1707652; // Figueiropolis/TO
    9355: CodIBGE := 1707702; // Filadelfia/TO
    9365: CodIBGE := 1708205; // Formoso Do Araguaia/TO
    0345: CodIBGE := 1708254; // Fortaleza Do Tabocao/TO
    9699: CodIBGE := 1708304; // Goianorte/TO
    9533: CodIBGE := 1709005; // Goiatins/TO
    9627: CodIBGE := 1709302; // Guarai/TO
    9385: CodIBGE := 1709500; // Gurupi/TO
    0084: CodIBGE := 1709807; // Ipueiras/TO
    9405: CodIBGE := 1710508; // Itacaja/TO
    9409: CodIBGE := 1710706; // Itaguatins/TO
    0347: CodIBGE := 1710904; // Itapiratins/TO
    9417: CodIBGE := 1711100; // Itapora Do Tocantins/TO
    0329: CodIBGE := 1711506; // Jau Do Tocantins/TO
    0349: CodIBGE := 1711803; // Juarina/TO
    0367: CodIBGE := 1711902; // Lagoa Da Confusao/TO
    0353: CodIBGE := 1711951; // Lagoa Do Tocantins/TO
    0351: CodIBGE := 1712009; // Lajeado/TO
    0086: CodIBGE := 1712157; // Lavandeira/TO
    9569: CodIBGE := 1712405; // Lizarda/TO
    0088: CodIBGE := 1712454; // Luzinopolis/TO
    9711: CodIBGE := 1712504; // Marianopolis Do Tocantins/TO
    0317: CodIBGE := 1712702; // Mateiros/TO
    0183: CodIBGE := 1712801; // Maurilandia Do Tocantins/TO
    9461: CodIBGE := 1713205; // Miracema Do Tocantins/TO
    9463: CodIBGE := 1713304; // Miranorte/TO
    9469: CodIBGE := 1713601; // Monte Do Carmo/TO
    0090: CodIBGE := 1713700; // Monte Santo Do Tocantins/TO
    0185: CodIBGE := 1713809; // Palmeiras Do Tocantins/TO
    0187: CodIBGE := 1713957; // Muricilandia/TO
    9481: CodIBGE := 1714203; // Natividade/TO
    9483: CodIBGE := 1714302; // Nazare/TO
    9663: CodIBGE := 1714880; // Nova Olinda/TO
    9721: CodIBGE := 1715002; // Nova Rosalandia/TO
    9499: CodIBGE := 1715101; // Novo Acordo/TO
    9703: CodIBGE := 1715150; // Novo Alegre/TO
    0321: CodIBGE := 1715259; // Novo Jardim/TO
    0092: CodIBGE := 1715507; // Oliveira De Fatima/TO
    0189: CodIBGE := 1715705; // Palmeirante/TO
    9649: CodIBGE := 1715754; // Palmeiropolis/TO
    9519: CodIBGE := 1716109; // Paraiso Do Tocantins/TO
    9521: CodIBGE := 1716208; // Parana/TO
    0191: CodIBGE := 1716307; // Pau D Arco/TO
    9525: CodIBGE := 1716505; // Pedro Afonso/TO
    9527: CodIBGE := 1716604; // Peixe/TO
    9705: CodIBGE := 1716653; // Pequizeiro/TO
    9529: CodIBGE := 1716703; // Colmeia/TO
    9537: CodIBGE := 1717008; // Pindorama Do Tocantins/TO
    0355: CodIBGE := 1717206; // Piraque/TO
    9547: CodIBGE := 1717503; // Pium/TO
    9551: CodIBGE := 1717800; // Ponte Alta Do Bom Jesus/TO
    9553: CodIBGE := 1717909; // Ponte Alta Do Tocantins/TO
    9723: CodIBGE := 1718006; // Porto Alegre Do Tocantins/TO
    9559: CodIBGE := 1718204; // Porto Nacional/TO
    9725: CodIBGE := 1718303; // Praia Norte/TO
    9629: CodIBGE := 1718402; // Presidente Kennedy/TO
    0094: CodIBGE := 1718451; // Pugmil/TO
    0357: CodIBGE := 1718501; // Recursolandia/TO
    0193: CodIBGE := 1718550; // Riachinho/TO
    0323: CodIBGE := 1718659; // Rio Da Conceicao/TO
    0359: CodIBGE := 1718709; // Rio Dos Bois/TO
    9679: CodIBGE := 1718758; // Rio Sono/TO
    9727: CodIBGE := 1718808; // Sampaio/TO
    0331: CodIBGE := 1718840; // Sandolandia/TO
    0195: CodIBGE := 1718865; // Santa Fe Do Araguaia/TO
    0361: CodIBGE := 1718881; // Santa Maria Do Tocantins/TO
    0096: CodIBGE := 1718899; // Santa Rita Do Tocantins/TO
    9729: CodIBGE := 1718907; // Santa Rosa Do Tocantins/TO
    9731: CodIBGE := 1719004; // Santa Tereza Do Tocantins/TO
    0098: CodIBGE := 1720002; // Santa Terezinha Do Tocantins/TO
    0197: CodIBGE := 1720101; // Sao Bento Do Tocantins/TO
    0363: CodIBGE := 1720150; // Sao Felix Do Tocantins/TO
    0199: CodIBGE := 1720200; // Sao Miguel Do Tocantins/TO
    0333: CodIBGE := 1720259; // Sao Salvador Do Tocantins/TO
    9603: CodIBGE := 1720309; // Sao Sebastiao Do Tocantins/TO
    9691: CodIBGE := 1720499; // Sao Valerio Da Natividade/TO
    9659: CodIBGE := 1720655; // Silvanopolis/TO
    9613: CodIBGE := 1720804; // Sitio Novo Do Tocantins/TO
    0335: CodIBGE := 1720853; // Sucupira/TO
    9615: CodIBGE := 1720903; // Taguatinga/TO
    0325: CodIBGE := 1720937; // Taipas Do Tocantins/TO
    0100: CodIBGE := 1720978; // Talisma/TO
    9733: CodIBGE := 1721000; // Palmas/TO
    9619: CodIBGE := 1721109; // Tocantinia/TO
    9621: CodIBGE := 1721208; // Tocantinopolis/TO
    0102: CodIBGE := 1721257; // Tupirama/TO
    0365: CodIBGE := 1721307; // Tupiratins/TO
    9665: CodIBGE := 1722081; // Wanderlandia/TO
    9643: CodIBGE := 1722107; // Xambioa/TO
    0961: CodIBGE := 2100055; // Acailandia/MA
    0701: CodIBGE := 2100105; // Afonso Cunha/MA
    0104: CodIBGE := 2100154; // Agua Doce Do Maranhao/MA
    0703: CodIBGE := 2100204; // Alcantara/MA
    0705: CodIBGE := 2100303; // Aldeias Altas/MA
    0707: CodIBGE := 2100402; // Altamira Do Maranhao/MA
    0106: CodIBGE := 2100436; // Alto Alegre Do Maranhao/MA
    0108: CodIBGE := 2100477; // Alto Alegre Do Pindare/MA
    0709: CodIBGE := 2100501; // Alto Parnaiba/MA
    0110: CodIBGE := 2100550; // Amapa Do Maranhao/MA
    0711: CodIBGE := 2100600; // Amarante Do Maranhao/MA
    0713: CodIBGE := 2100709; // Anajatuba/MA
    0715: CodIBGE := 2100808; // Anapurus/MA
    0112: CodIBGE := 2100832; // Apicum-Acu/MA
    0114: CodIBGE := 2100873; // Araguana/MA
    0717: CodIBGE := 2100907; // Araioses/MA
    1281: CodIBGE := 2100956; // Arame/MA
    0719: CodIBGE := 2101004; // Arari/MA
    0721: CodIBGE := 2101103; // Axixa/MA
    0723: CodIBGE := 2101202; // Bacabal/MA
    0116: CodIBGE := 2101251; // Bacabeira/MA
    0725: CodIBGE := 2101301; // Bacuri/MA
    0118: CodIBGE := 2101350; // Bacurituba/MA
    0727: CodIBGE := 2101400; // Balsas/MA
    0729: CodIBGE := 2101509; // Barao De Grajau/MA
    0731: CodIBGE := 2101608; // Barra Do Corda/MA
    0733: CodIBGE := 2101707; // Barreirinhas/MA
    0120: CodIBGE := 2101731; // Belagua/MA
    0122: CodIBGE := 2101772; // Bela Vista Do Maranhao/MA
    0735: CodIBGE := 2101806; // Benedito Leite/MA
    0737: CodIBGE := 2101905; // Bequimao/MA
    0124: CodIBGE := 2101939; // Bernardo Do Mearim/MA
    0126: CodIBGE := 2101970; // Boa Vista Do Gurupi/MA
    0955: CodIBGE := 2102002; // Bom Jardim/MA
    0128: CodIBGE := 2102036; // Bom Jesus Das Selvas/MA
    0130: CodIBGE := 2102077; // Bom Lugar/MA
    0739: CodIBGE := 2102101; // Brejo/MA
    0132: CodIBGE := 2102150; // Brejo De Areia/MA
    0741: CodIBGE := 2102200; // Buriti/MA
    0743: CodIBGE := 2102309; // Buriti Bravo/MA
    0134: CodIBGE := 2102325; // Buriticupu/MA
    0136: CodIBGE := 2102358; // Buritirana/MA
    0138: CodIBGE := 2102374; // Cachoeira Grande/MA
    0745: CodIBGE := 2102408; // Cajapio/MA
    0747: CodIBGE := 2102507; // Cajari/MA
    0140: CodIBGE := 2102556; // Campestre Do Maranhao/MA
    0749: CodIBGE := 2102606; // Candido Mendes/MA
    0751: CodIBGE := 2102705; // Cantanhede/MA
    0142: CodIBGE := 2102754; // Capinzal Do Norte/MA
    0753: CodIBGE := 2102804; // Carolina/MA
    0755: CodIBGE := 2102903; // Carutapera/MA
    0757: CodIBGE := 2103000; // Caxias/MA
    0759: CodIBGE := 2103109; // Cedral/MA
    0144: CodIBGE := 2103125; // Central Do Maranhao/MA
    0146: CodIBGE := 2103158; // Centro Do Guilherme/MA
    0148: CodIBGE := 2103174; // Centro Novo Do Maranhao/MA
    0761: CodIBGE := 2103208; // Chapadinha/MA
    0150: CodIBGE := 2103257; // Cidelandia/MA
    0763: CodIBGE := 2103307; // Codo/MA
    0765: CodIBGE := 2103406; // Coelho Neto/MA
    0767: CodIBGE := 2103505; // Colinas/MA
    0152: CodIBGE := 2103554; // Conceicao Do Lago-Acu/MA
    0769: CodIBGE := 2103604; // Coroata/MA
    0771: CodIBGE := 2103703; // Cururupu/MA
    0154: CodIBGE := 2103752; // Davinopolis/MA
    0773: CodIBGE := 2103802; // Dom Pedro/MA
    0775: CodIBGE := 2103901; // Duque Bacelar/MA
    0777: CodIBGE := 2104008; // Esperantinopolis/MA
    0963: CodIBGE := 2104057; // Estreito/MA
    0156: CodIBGE := 2104073; // Feira Nova Do Maranhao/MA
    0158: CodIBGE := 2104081; // Fernando Falcao/MA
    0160: CodIBGE := 2104099; // Formosa Da Serra Negra/MA
    0779: CodIBGE := 2104107; // Fortaleza Dos Nogueiras/MA
    0781: CodIBGE := 2104206; // Fortuna/MA
    0783: CodIBGE := 2104305; // Godofredo Viana/MA
    0785: CodIBGE := 2104404; // Goncalves Dias/MA
    0787: CodIBGE := 2104503; // Governador Archer/MA
    0162: CodIBGE := 2104552; // Governador Edison Lobao/MA
    0789: CodIBGE := 2104602; // Governador Eugenio Barros/MA
    0164: CodIBGE := 2104628; // Governador Luiz Rocha/MA
    0166: CodIBGE := 2104651; // Governador Newton Bello/MA
    0168: CodIBGE := 2104677; // Governador Nunes Freire/MA
    0791: CodIBGE := 2104701; // Graca Aranha/MA
    0793: CodIBGE := 2104800; // Grajau/MA
    0795: CodIBGE := 2104909; // Guimaraes/MA
    0797: CodIBGE := 2105005; // Humberto De Campos/MA
    0799: CodIBGE := 2105104; // Icatu/MA
    0170: CodIBGE := 2105153; // Igarape Do Meio/MA
    0801: CodIBGE := 2105203; // Igarape Grande/MA
    0803: CodIBGE := 2105302; // Imperatriz/MA
    0172: CodIBGE := 2105351; // Itaipava Do Grajau/MA
    0807: CodIBGE := 2105401; // Itapecuru Mirim/MA
    0174: CodIBGE := 2105427; // Itinga Do Maranhao/MA
    0176: CodIBGE := 2105450; // Jatoba/MA
    0178: CodIBGE := 2105476; // Jenipapo Dos Vieiras/MA
    0809: CodIBGE := 2105500; // Joao Lisboa/MA
    0811: CodIBGE := 2105609; // Joselandia/MA
    0180: CodIBGE := 2105658; // Junco Do Maranhao/MA
    0813: CodIBGE := 2105708; // Lago Da Pedra/MA
    0815: CodIBGE := 2105807; // Lago Do Junco/MA
    0817: CodIBGE := 2105906; // Lago Verde/MA
    0182: CodIBGE := 2105922; // Lagoa Do Mato/MA
    0184: CodIBGE := 2105948; // Lago Dos Rodrigues/MA
    0186: CodIBGE := 2105963; // Lagoa Grande Do Maranhao/MA
    0188: CodIBGE := 2105989; // Lajeado Novo/MA
    0819: CodIBGE := 2106003; // Lima Campos/MA
    0821: CodIBGE := 2106102; // Loreto/MA
    0823: CodIBGE := 2106201; // Luis Domingues/MA
    0825: CodIBGE := 2106300; // Magalhaes De Almeida/MA
    0190: CodIBGE := 2106326; // Maracacume/MA
    0192: CodIBGE := 2106359; // Maraja Do Sena/MA
    0194: CodIBGE := 2106375; // Maranhaozinho/MA
    0827: CodIBGE := 2106409; // Mata Roma/MA
    0829: CodIBGE := 2106508; // Matinha/MA
    0831: CodIBGE := 2106607; // Matoes/MA
    0196: CodIBGE := 2106631; // Matoes Do Norte/MA
    0198: CodIBGE := 2106672; // Milagres Do Maranhao/MA
    0833: CodIBGE := 2106706; // Mirador/MA
    1283: CodIBGE := 2106755; // Miranda Do Norte/MA
    0835: CodIBGE := 2106805; // Mirinzal/MA
    0837: CodIBGE := 2106904; // Moncao/MA
    0839: CodIBGE := 2107001; // Montes Altos/MA
    0841: CodIBGE := 2107100; // Morros/MA
    0843: CodIBGE := 2107209; // Nina Rodrigues/MA
    0200: CodIBGE := 2107258; // Nova Colinas/MA
    0845: CodIBGE := 2107308; // Nova Iorque/MA
    0202: CodIBGE := 2107357; // Nova Olinda Do Maranhao/MA
    0847: CodIBGE := 2107407; // Olho D Agua Das Cunhas/MA
    0204: CodIBGE := 2107456; // Olinda Nova Do Maranhao/MA
    0849: CodIBGE := 2107506; // Paco Do Lumiar/MA
    0851: CodIBGE := 2107605; // Palmeirandia/MA
    0853: CodIBGE := 2107704; // Paraibano/MA
    0855: CodIBGE := 2107803; // Parnarama/MA
    0857: CodIBGE := 2107902; // Passagem Franca/MA
    0859: CodIBGE := 2108009; // Pastos Bons/MA
    0206: CodIBGE := 2108058; // Paulino Neves/MA
    0959: CodIBGE := 2108108; // Paulo Ramos/MA
    0861: CodIBGE := 2108207; // Pedreiras/MA
    0208: CodIBGE := 2108256; // Pedro Do Rosario/MA
    0863: CodIBGE := 2108306; // Penalva/MA
    0865: CodIBGE := 2108405; // Peri Mirim/MA
    0210: CodIBGE := 2108454; // Peritoro/MA
    0867: CodIBGE := 2108504; // Pindare-Mirim/MA
    0869: CodIBGE := 2108603; // Pinheiro/MA
    0871: CodIBGE := 2108702; // Pio Xii/MA
    0873: CodIBGE := 2108801; // Pirapemas/MA
    0875: CodIBGE := 2108900; // Pocao De Pedras/MA
    0877: CodIBGE := 2109007; // Porto Franco/MA
    0212: CodIBGE := 2109056; // Porto Rico Do Maranhao/MA
    0879: CodIBGE := 2109106; // Presidente Dutra/MA
    0881: CodIBGE := 2109205; // Presidente Juscelino/MA
    0214: CodIBGE := 2109239; // Presidente Medici/MA
    0216: CodIBGE := 2109270; // Presidente Sarney/MA
    0883: CodIBGE := 2109304; // Presidente Vargas/MA
    0885: CodIBGE := 2109403; // Primeira Cruz/MA
    0218: CodIBGE := 2109452; // Raposa/MA
    0887: CodIBGE := 2109502; // Riachao/MA
    0220: CodIBGE := 2109551; // Ribamar Fiquene/MA
    0891: CodIBGE := 2109601; // Rosario/MA
    0893: CodIBGE := 2109700; // Sambaiba/MA
    0222: CodIBGE := 2109759; // Santa Filomena Do Maranhao/MA
    0895: CodIBGE := 2109809; // Santa Helena/MA
    0957: CodIBGE := 2109908; // Santa Ines/MA
    0897: CodIBGE := 2110005; // Santa Luzia/MA
    1285: CodIBGE := 2110039; // Santa Luzia Do Parua/MA
    0899: CodIBGE := 2110104; // Santa Quiteria Do Maranhao/MA
    0901: CodIBGE := 2110203; // Santa Rita/MA
    0224: CodIBGE := 2110237; // Santana Do Maranhao/MA
    0226: CodIBGE := 2110278; // Santo Amaro Do Maranhao/MA
    0903: CodIBGE := 2110302; // Santo Antonio Dos Lopes/MA
    0905: CodIBGE := 2110401; // Sao Benedito Do Rio Preto/MA
    0907: CodIBGE := 2110500; // Sao Bento/MA
    0909: CodIBGE := 2110609; // Sao Bernardo/MA
    0228: CodIBGE := 2110658; // Sao Domingos Do Azeitao/MA
    0911: CodIBGE := 2110708; // Sao Domingos Do Maranhao/MA
    0913: CodIBGE := 2110807; // Sao Felix De Balsas/MA
    0230: CodIBGE := 2110856; // Sao Francisco Do Brejao/MA
    0915: CodIBGE := 2110906; // Sao Francisco Do Maranhao/MA
    0917: CodIBGE := 2111003; // Sao Joao Batista/MA
    0232: CodIBGE := 2111029; // Sao Joao Do Caru/MA
    0234: CodIBGE := 2111052; // Sao Joao Do Paraiso/MA
    0236: CodIBGE := 2111078; // Sao Joao Do Soter/MA
    0919: CodIBGE := 2111102; // Sao Joao Dos Patos/MA
    0889: CodIBGE := 2111201; // Sao Jose De Ribamar/MA
    0238: CodIBGE := 2111250; // Sao Jose Dos Basilios/MA
    0921: CodIBGE := 2111300; // Sao Luis/MA
    0805: CodIBGE := 2111409; // Sao Luis Gonzaga Do Maranhao/MA
    0923: CodIBGE := 2111508; // Sao Mateus Do Maranhao/MA
    0240: CodIBGE := 2111532; // Sao Pedro Da Agua Branca/MA
    0242: CodIBGE := 2111573; // Sao Pedro Dos Crentes/MA
    0925: CodIBGE := 2111607; // Sao Raimundo Das Mangabeiras/MA
    0244: CodIBGE := 2111631; // Sao Raimundo Do Doca Bezerra/MA
    0246: CodIBGE := 2111672; // Sao Roberto/MA
    0927: CodIBGE := 2111706; // Sao Vicente Ferrer/MA
    0248: CodIBGE := 2111722; // Satubinha/MA
    0250: CodIBGE := 2111748; // Senador Alexandre Costa/MA
    0252: CodIBGE := 2111763; // Senador La Rocque/MA
    0254: CodIBGE := 2111789; // Serrano Do Maranhao/MA
    0929: CodIBGE := 2111805; // Sitio Novo/MA
    0931: CodIBGE := 2111904; // Sucupira Do Norte/MA
    0256: CodIBGE := 2111953; // Sucupira Do Riachao/MA
    0933: CodIBGE := 2112001; // Tasso Fragoso/MA
    0935: CodIBGE := 2112100; // Timbiras/MA
    0937: CodIBGE := 2112209; // Timon/MA
    0258: CodIBGE := 2112233; // Trizidela Do Vale/MA
    0260: CodIBGE := 2112274; // Tufilandia/MA
    0939: CodIBGE := 2112308; // Tuntum/MA
    0941: CodIBGE := 2112407; // Turiacu/MA
    0262: CodIBGE := 2112456; // Turilandia/MA
    0943: CodIBGE := 2112506; // Tutoia/MA
    0945: CodIBGE := 2112605; // Urbano Santos/MA
    0947: CodIBGE := 2112704; // Vargem Grande/MA
    0949: CodIBGE := 2112803; // Viana/MA
    0264: CodIBGE := 2112852; // Vila Nova Dos Martirios/MA
    0951: CodIBGE := 2112902; // Vitoria Do Mearim/MA
    0953: CodIBGE := 2113009; // Vitorino Freire/MA
    1287: CodIBGE := 2114007; // Ze Doca/MA
    0266: CodIBGE := 2200053; // Acaua/PI
    1001: CodIBGE := 2200103; // Agricolandia/PI
    1003: CodIBGE := 2200202; // Agua Branca/PI
    9767: CodIBGE := 2200251; // Alagoinha Do Piaui/PI
    2269: CodIBGE := 2200277; // Alegrete Do Piaui/PI
    1005: CodIBGE := 2200301; // Alto Longa/PI
    1007: CodIBGE := 2200400; // Altos/PI
    0268: CodIBGE := 2200459; // Alvorada Do Gurgueia/PI
    1009: CodIBGE := 2200509; // Amarante/PI
    1011: CodIBGE := 2200608; // Angical Do Piaui/PI
    1013: CodIBGE := 2200707; // Anisio De Abreu/PI
    1015: CodIBGE := 2200806; // Antonio Almeida/PI
    1017: CodIBGE := 2200905; // Aroazes/PI
    1188: CodIBGE := 2200954; // Aroeiras Do Itaim/PI/A
    1019: CodIBGE := 2201002; // Arraial/PI
    0270: CodIBGE := 2201051; // Assuncao Do Piaui/PI
    1021: CodIBGE := 2201101; // Avelino Lopes/PI
    2245: CodIBGE := 2201150; // Baixa Grande Do Ribeiro/PI
    0272: CodIBGE := 2201176; // Barra D Alcantara/PI
    1023: CodIBGE := 2201200; // Barras/PI
    1025: CodIBGE := 2201309; // Barreiras Do Piaui/PI
    1027: CodIBGE := 2201408; // Barro Duro/PI
    1029: CodIBGE := 2201507; // Batalha/PI
    0274: CodIBGE := 2201556; // Bela Vista Do Piaui/PI
    0276: CodIBGE := 2201572; // Belem Do Piaui/PI
    1031: CodIBGE := 2201606; // Beneditinos/PI
    1033: CodIBGE := 2201705; // Bertolinia/PI
    0278: CodIBGE := 2201739; // Betania Do Piaui/PI
    0280: CodIBGE := 2201770; // Boa Hora/PI
    1035: CodIBGE := 2201804; // Bocaina/PI
    1037: CodIBGE := 2201903; // Bom Jesus/PI
    2287: CodIBGE := 2201919; // Bom Principio Do Piaui/PI
    2251: CodIBGE := 2201929; // Bonfim Do Piaui/PI
    0282: CodIBGE := 2201945; // Boqueirao Do Piaui/PI
    2283: CodIBGE := 2201960; // Brasileira/PI
    0284: CodIBGE := 2201988; // Brejo Do Piaui/PI
    1039: CodIBGE := 2202000; // Buriti Dos Lopes/PI
    1297: CodIBGE := 2202026; // Buriti Dos Montes/PI
    1299: CodIBGE := 2202059; // Cabeceiras Do Piaui/PI
    0286: CodIBGE := 2202075; // Cajazeiras Do Piaui/PI
    0288: CodIBGE := 2202083; // Cajueiro Da Praia/PI
    2271: CodIBGE := 2202091; // Caldeirao Grande Do Piaui/PI
    1041: CodIBGE := 2202109; // Campinas Do Piaui/PI
    0290: CodIBGE := 2202117; // Campo Alegre Do Fidalgo/PI
    0292: CodIBGE := 2202133; // Campo Grande Do Piaui/PI
    0294: CodIBGE := 2202174; // Campo Largo Do Piaui/PI
    1043: CodIBGE := 2202208; // Campo Maior/PI
    2247: CodIBGE := 2202251; // Canavieira/PI
    1045: CodIBGE := 2202307; // Canto Do Buriti/PI
    1047: CodIBGE := 2202406; // Capitao De Campos/PI
    0296: CodIBGE := 2202455; // Capitao Gervasio Oliveira/PI
    1049: CodIBGE := 2202505; // Caracol/PI
    0298: CodIBGE := 2202539; // Caraubas Do Piaui/PI
    0300: CodIBGE := 2202554; // Caridade Do Piaui/PI
    1051: CodIBGE := 2202604; // Castelo Do Piaui/PI
    0302: CodIBGE := 2202653; // Caxingo/PI
    1053: CodIBGE := 2202703; // Cocal/PI
    0304: CodIBGE := 2202711; // Cocal De Telha/PI
    0306: CodIBGE := 2202729; // Cocal Dos Alves/PI
    0995: CodIBGE := 2202737; // Coivaras/PI
    2249: CodIBGE := 2202752; // Colonia Do Gurgueia/PI
    2253: CodIBGE := 2202778; // Colonia Do Piaui/PI
    1055: CodIBGE := 2202802; // Conceicao Do Caninde/PI
    2255: CodIBGE := 2202851; // Coronel Jose Dias/PI
    1057: CodIBGE := 2202901; // Corrente/PI
    1059: CodIBGE := 2203008; // Cristalandia Do Piaui/PI
    1061: CodIBGE := 2203107; // Cristino Castro/PI
    1063: CodIBGE := 2203206; // Curimata/PI
    0308: CodIBGE := 2203230; // Currais/PI
    0310: CodIBGE := 2203255; // Curralinhos/PI
    0312: CodIBGE := 2203271; // Curral Novo Do Piaui/PI
    1065: CodIBGE := 2203305; // Demerval Lobao/PI
    1229: CodIBGE := 2203354; // Dirceu Arcoverde/PI
    1067: CodIBGE := 2203404; // Dom Expedito Lopes/PI
    1141: CodIBGE := 2203420; // Domingos Mourao/PI
    1289: CodIBGE := 2203453; // Dom Inocencio/PI
    1069: CodIBGE := 2203503; // Elesbao Veloso/PI
    1071: CodIBGE := 2203602; // Eliseu Martins/PI
    1073: CodIBGE := 2203701; // Esperantina/PI
    2257: CodIBGE := 2203750; // Fartura Do Piaui/PI
    1075: CodIBGE := 2203800; // Flores Do Piaui/PI
    0314: CodIBGE := 2203859; // Floresta Do Piaui/PI
    1077: CodIBGE := 2203909; // Floriano/PI
    1079: CodIBGE := 2204006; // Francinopolis/PI
    1081: CodIBGE := 2204105; // Francisco Ayres/PI
    0316: CodIBGE := 2204154; // Francisco Macedo/PI
    1083: CodIBGE := 2204204; // Francisco Santos/PI
    1085: CodIBGE := 2204303; // Fronteiras/PI
    0318: CodIBGE := 2204352; // Geminiano/PI
    1087: CodIBGE := 2204402; // Gilbues/PI
    1089: CodIBGE := 2204501; // Guadalupe/PI
    0320: CodIBGE := 2204550; // Guaribas/PI
    1091: CodIBGE := 2204600; // Hugo Napoleao/PI
    0322: CodIBGE := 2204659; // Ilha Grande/PI
    1093: CodIBGE := 2204709; // Inhuma/PI
    1095: CodIBGE := 2204808; // Ipiranga Do Piaui/PI
    1097: CodIBGE := 2204907; // Isaias Coelho/PI
    1099: CodIBGE := 2205003; // Itainopolis/PI
    1101: CodIBGE := 2205102; // Itaueira/PI
    2273: CodIBGE := 2205151; // Jacobina Do Piaui/PI
    1103: CodIBGE := 2205201; // Jaicos/PI
    0997: CodIBGE := 2205250; // Jardim Do Mulato/PI
    0324: CodIBGE := 2205276; // Jatoba Do Piaui/PI
    1105: CodIBGE := 2205300; // Jerumenha/PI
    0326: CodIBGE := 2205359; // Joao Costa/PI
    1107: CodIBGE := 2205409; // Joaquim Pires/PI
    0328: CodIBGE := 2205458; // Joca Marques/PI
    1109: CodIBGE := 2205508; // Jose De Freitas/PI
    0330: CodIBGE := 2205516; // Juazeiro Do Piaui/PI
    0332: CodIBGE := 2205524; // Julio Borges/PI
    0334: CodIBGE := 2205532; // Jurema/PI
    0336: CodIBGE := 2205540; // Lagoinha Do Piaui/PI
    0999: CodIBGE := 2205557; // Lagoa Alegre/PI
    2259: CodIBGE := 2205565; // Lagoa Do Barro Do Piaui/PI
    0338: CodIBGE := 2205573; // Lagoa De Sao Francisco/PI
    0340: CodIBGE := 2205581; // Lagoa Do Piaui/PI
    0342: CodIBGE := 2205599; // Lagoa Do Sitio/PI
    1111: CodIBGE := 2205607; // Landri Sales/PI
    1113: CodIBGE := 2205706; // Luis Correia/PI
    1115: CodIBGE := 2205805; // Luzilandia/PI
    0344: CodIBGE := 2205854; // Madeiro/PI
    1117: CodIBGE := 2205904; // Manoel Emidio/PI
    2275: CodIBGE := 2205953; // Marcolandia/PI
    1119: CodIBGE := 2206001; // Marcos Parente/PI
    0346: CodIBGE := 2206050; // Massape Do Piaui/PI
    1121: CodIBGE := 2206100; // Matias Olimpio/PI
    1123: CodIBGE := 2206209; // Miguel Alves/PI
    1125: CodIBGE := 2206308; // Miguel Leao/PI
    0348: CodIBGE := 2206357; // Milton Brandao/PI
    1127: CodIBGE := 2206407; // Monsenhor Gil/PI
    1129: CodIBGE := 2206506; // Monsenhor Hipolito/PI
    1131: CodIBGE := 2206605; // Monte Alegre Do Piaui/PI
    0350: CodIBGE := 2206654; // Morro Cabeca No Tempo/PI
    0352: CodIBGE := 2206670; // Morro Do Chapeu Do Piaui/PI
    0354: CodIBGE := 2206696; // Murici Dos Portelas/PI
    1133: CodIBGE := 2206704; // Nazare Do Piaui/PI
    1180: CodIBGE := 2206720; // Nazária/PI
    0356: CodIBGE := 2206753; // Nossa Senhora De Nazare/PI
    1135: CodIBGE := 2206803; // Nossa Senhora Dos Remedios/PI
    1137: CodIBGE := 2206902; // Novo Oriente Do Piaui/PI
    0358: CodIBGE := 2206951; // Novo Santo Antonio/PI
    1139: CodIBGE := 2207009; // Oeiras/PI
    0360: CodIBGE := 2207108; // Olho D Agua Do Piaui/PI
    1143: CodIBGE := 2207207; // Padre Marcos/PI
    1145: CodIBGE := 2207306; // Paes Landim/PI
    0362: CodIBGE := 2207355; // Pajeu Do Piaui/PI
    1147: CodIBGE := 2207405; // Palmeira Do Piaui/PI
    1149: CodIBGE := 2207504; // Palmeirais/PI
    0364: CodIBGE := 2207553; // Paqueta/PI
    1151: CodIBGE := 2207603; // Parnagua/PI
    1153: CodIBGE := 2207702; // Parnaiba/PI
    1293: CodIBGE := 2207751; // Passagem Franca Do Piaui/PI
    2277: CodIBGE := 2207777; // Patos Do Piaui/PI
    1104: CodIBGE := 2207793; // Pau D Arco Do Piaui/PI
    1155: CodIBGE := 2207801; // Paulistana/PI
    0366: CodIBGE := 2207850; // Pavussu/PI
    1157: CodIBGE := 2207900; // Pedro Ii/PI
    0368: CodIBGE := 2207934; // Pedro Laurentino/PI
    0370: CodIBGE := 2207959; // Nova Santa Rita/PI
    1159: CodIBGE := 2208007; // Picos/PI
    1161: CodIBGE := 2208106; // Pimenteiras/PI
    1163: CodIBGE := 2208205; // Pio Ix/PI
    1165: CodIBGE := 2208304; // Piracuruca/PI
    1167: CodIBGE := 2208403; // Piripiri/PI
    1169: CodIBGE := 2208502; // Porto/PI
    0372: CodIBGE := 2208551; // Porto Alegre Do Piaui/PI
    1171: CodIBGE := 2208601; // Prata Do Piaui/PI
    2279: CodIBGE := 2208650; // Queimada Nova/PI
    1173: CodIBGE := 2208700; // Redencao Do Gurgueia/PI
    1175: CodIBGE := 2208809; // Regeneracao/PI
    0374: CodIBGE := 2208858; // Riacho Frio/PI
    0376: CodIBGE := 2208874; // Ribeira Do Piaui/PI
    1177: CodIBGE := 2208908; // Ribeiro Goncalves/PI
    1179: CodIBGE := 2209005; // Rio Grande Do Piaui/PI
    1181: CodIBGE := 2209104; // Santa Cruz Do Piaui/PI
    1295: CodIBGE := 2209153; // Santa Cruz Dos Milagres/PI
    1183: CodIBGE := 2209203; // Santa Filomena/PI
    1185: CodIBGE := 2209302; // Santa Luz/PI
    2281: CodIBGE := 2209351; // Santana Do Piaui/PI
    2261: CodIBGE := 2209377; // Santa Rosa Do Piaui/PI
    1187: CodIBGE := 2209401; // Santo Antonio De Lisboa/PI
    0378: CodIBGE := 2209450; // Santo Antonio Dos Milagres/PI
    1189: CodIBGE := 2209500; // Santo Inacio Do Piaui/PI
    2263: CodIBGE := 2209559; // Sao Braz Do Piaui/PI
    1191: CodIBGE := 2209609; // Sao Felix Do Piaui/PI
    0380: CodIBGE := 2209658; // Sao Francisco De Assis Do Piaui/PI
    1193: CodIBGE := 2209708; // Sao Francisco Do Piaui/PI
    0382: CodIBGE := 2209757; // Sao Goncalo Do Gurgueia/PI
    1195: CodIBGE := 2209807; // Sao Goncalo Do Piaui/PI
    1291: CodIBGE := 2209856; // Sao Joao Da Canabrava/PI
    0384: CodIBGE := 2209872; // Sao Joao Da Fronteira/PI
    1197: CodIBGE := 2209906; // Sao Joao Da Serra/PI
    0386: CodIBGE := 2209955; // Sao Joao Da Varjota/PI
    0388: CodIBGE := 2209971; // Sao Joao Do Arraial/PI
    1199: CodIBGE := 2210003; // Sao Joao Do Piaui/PI
    2285: CodIBGE := 2210052; // Sao Jose Do Divino/PI
    1201: CodIBGE := 2210102; // Sao Jose Do Peixe/PI
    1203: CodIBGE := 2210201; // Sao Jose Do Piaui/PI
    1205: CodIBGE := 2210300; // Sao Juliao/PI
    2265: CodIBGE := 2210359; // Sao Lourenco Do Piaui/PI
    0390: CodIBGE := 2210375; // Sao Luis Do Piaui/PI
    0392: CodIBGE := 2210383; // Sao Miguel Da Baixa Grande/PI
    0394: CodIBGE := 2210391; // Sao Miguel Do Fidalgo/PI
    1207: CodIBGE := 2210409; // Sao Miguel Do Tapuio/PI
    1209: CodIBGE := 2210508; // Sao Pedro Do Piaui/PI
    1211: CodIBGE := 2210607; // Sao Raimundo Nonato/PI
    0396: CodIBGE := 2210623; // Sebastiao Barros/PI
    0398: CodIBGE := 2210631; // Sebastiao Leal/PI
    1379: CodIBGE := 2210656; // Sigefredo Pacheco/PI
    1213: CodIBGE := 2210706; // Simoes/PI
    1215: CodIBGE := 2210805; // Simplicio Mendes/PI
    1217: CodIBGE := 2210904; // Socorro Do Piaui/PI
    0400: CodIBGE := 2210938; // Sussuapara/PI
    0402: CodIBGE := 2210953; // Tamboril Do Piaui/PI
    0404: CodIBGE := 2210979; // Tanque Do Piaui/PI
    1219: CodIBGE := 2211001; // Teresina/PI
    1221: CodIBGE := 2211100; // Uniao/PI
    1223: CodIBGE := 2211209; // Urucui/PI
    1225: CodIBGE := 2211308; // Valenca Do Piaui/PI
    2267: CodIBGE := 2211357; // Varzea Branca/PI
    1227: CodIBGE := 2211407; // Varzea Grande/PI
    0406: CodIBGE := 2211506; // Vera Mendes/PI
    0408: CodIBGE := 2211605; // Vila Nova Do Piaui/PI
    0410: CodIBGE := 2211704; // Wall Ferraz/PI

    1301: CodIBGE := 2300101; // Abaiara/CE
    1231: CodIBGE := 2300150; // Acarape/CE
    1303: CodIBGE := 2300200; // Acarau/CE
    1305: CodIBGE := 2300309; // Acopiara/CE
    1307: CodIBGE := 2300408; // Aiuaba/CE
    1309: CodIBGE := 2300507; // Alcantaras/CE
    1311: CodIBGE := 2300606; // Altaneira/CE
    1313: CodIBGE := 2300705; // Alto Santo/CE
    1587: CodIBGE := 2300754; // Amontada/CE
    1315: CodIBGE := 2300804; // Antonina Do Norte/CE
    1317: CodIBGE := 2300903; // Apuiares/CE
    1319: CodIBGE := 2301000; // Aquiraz/CE
    1321: CodIBGE := 2301109; // Aracati/CE
    1323: CodIBGE := 2301208; // Aracoiaba/CE
    0989: CodIBGE := 2301257; // Ararenda/CE
    1325: CodIBGE := 2301307; // Araripe/CE
    1327: CodIBGE := 2301406; // Aratuba/CE
    1329: CodIBGE := 2301505; // Arneiroz/CE
    1331: CodIBGE := 2301604; // Assare/CE
    1333: CodIBGE := 2301703; // Aurora/CE
    1335: CodIBGE := 2301802; // Baixio/CE
    1233: CodIBGE := 2301851; // Banabuiu/CE
    1337: CodIBGE := 2301901; // Barbalha/CE
    1235: CodIBGE := 2301950; // Barreira/CE
    1339: CodIBGE := 2302008; // Barro/CE
    1237: CodIBGE := 2302057; // Barroquinha/CE
    1341: CodIBGE := 2302107; // Baturite/CE
    1343: CodIBGE := 2302206; // Beberibe/CE
    1345: CodIBGE := 2302305; // Bela Cruz/CE
    1347: CodIBGE := 2302404; // Boa Viagem/CE
    1349: CodIBGE := 2302503; // Brejo Santo/CE
    1351: CodIBGE := 2302602; // Camocim/CE
    1353: CodIBGE := 2302701; // Campos Sales/CE
    1355: CodIBGE := 2302800; // Caninde/CE
    1357: CodIBGE := 2302909; // Capistrano/CE
    1359: CodIBGE := 2303006; // Caridade/CE
    1361: CodIBGE := 2303105; // Carire/CE
    1363: CodIBGE := 2303204; // Caririacu/CE
    1365: CodIBGE := 2303303; // Carius/CE
    1367: CodIBGE := 2303402; // Carnaubal/CE
    1369: CodIBGE := 2303501; // Cascavel/CE
    1371: CodIBGE := 2303600; // Catarina/CE
    0983: CodIBGE := 2303659; // Catunda/CE
    1373: CodIBGE := 2303709; // Caucaia/CE
    1375: CodIBGE := 2303808; // Cedro/CE
    1377: CodIBGE := 2303907; // Chaval/CE
    0993: CodIBGE := 2303931; // Choro/CE
    1239: CodIBGE := 2303956; // Chorozinho/CE
    1381: CodIBGE := 2304004; // Coreau/CE
    1383: CodIBGE := 2304103; // Crateus/CE
    1385: CodIBGE := 2304202; // Crato/CE
    1241: CodIBGE := 2304236; // Croata/CE
    1589: CodIBGE := 2304251; // Cruz/CE
    1243: CodIBGE := 2304269; // Deputado Irapuan Pinheiro/CE
    1245: CodIBGE := 2304277; // Erere/CE
    1247: CodIBGE := 2304285; // Eusebio/CE
    1387: CodIBGE := 2304301; // Farias Brito/CE
    1591: CodIBGE := 2304350; // Forquilha/CE
    1389: CodIBGE := 2304400; // Fortaleza/CE
    0987: CodIBGE := 2304459; // Fortim/CE
    1391: CodIBGE := 2304509; // Frecheirinha/CE
    1393: CodIBGE := 2304608; // General Sampaio/CE
    1249: CodIBGE := 2304657; // Graca/CE
    1395: CodIBGE := 2304707; // Granja/CE
    1397: CodIBGE := 2304806; // Granjeiro/CE
    1399: CodIBGE := 2304905; // Groairas/CE
    1251: CodIBGE := 2304954; // Guaiuba/CE
    1401: CodIBGE := 2305001; // Guaraciaba Do Norte/CE
    1403: CodIBGE := 2305100; // Guaramiranga/CE
    1405: CodIBGE := 2305209; // Hidrolandia/CE
    1253: CodIBGE := 2305233; // Horizonte/CE
    1255: CodIBGE := 2305266; // Ibaretama/CE
    1407: CodIBGE := 2305308; // Ibiapina/CE
    1257: CodIBGE := 2305332; // Ibicuitinga/CE
    1593: CodIBGE := 2305357; // Icapui/CE
    1409: CodIBGE := 2305407; // Ico/CE
    1411: CodIBGE := 2305506; // Iguatu/CE
    1413: CodIBGE := 2305605; // Independencia/CE
    1259: CodIBGE := 2305654; // Ipaporanga/CE
    1415: CodIBGE := 2305704; // Ipaumirim/CE
    1417: CodIBGE := 2305803; // Ipu/CE
    1419: CodIBGE := 2305902; // Ipueiras/CE
    1421: CodIBGE := 2306009; // Iracema/CE
    1423: CodIBGE := 2306108; // Iraucuba/CE
    1425: CodIBGE := 2306207; // Itaicaba/CE
    0991: CodIBGE := 2306256; // Itaitinga/CE
    1427: CodIBGE := 2306306; // Itapage/CE
    1429: CodIBGE := 2306405; // Itapipoca/CE
    1431: CodIBGE := 2306504; // Itapiuna/CE
    1595: CodIBGE := 2306553; // Itarema/CE
    1433: CodIBGE := 2306603; // Itatira/CE
    1435: CodIBGE := 2306702; // Jaguaretama/CE
    1437: CodIBGE := 2306801; // Jaguaribara/CE
    1439: CodIBGE := 2306900; // Jaguaribe/CE
    1441: CodIBGE := 2307007; // Jaguaruana/CE
    1443: CodIBGE := 2307106; // Jardim/CE
    1445: CodIBGE := 2307205; // Jati/CE
    0985: CodIBGE := 2307254; // Jijoca De Jericoacoara/CE
    1447: CodIBGE := 2307304; // Juazeiro Do Norte/CE
    1449: CodIBGE := 2307403; // Jucas/CE
    1451: CodIBGE := 2307502; // Lavras Da Mangabeira/CE
    1453: CodIBGE := 2307601; // Limoeiro Do Norte/CE
    1261: CodIBGE := 2307635; // Madalena/CE
    1585: CodIBGE := 2307650; // Maracanau/CE
    1455: CodIBGE := 2307700; // Maranguape/CE
    1457: CodIBGE := 2307809; // Marco/CE
    1459: CodIBGE := 2307908; // Martinopole/CE
    1461: CodIBGE := 2308005; // Massape/CE
    1463: CodIBGE := 2308104; // Mauriti/CE
    1465: CodIBGE := 2308203; // Meruoca/CE
    1467: CodIBGE := 2308302; // Milagres/CE
    1597: CodIBGE := 2308351; // Milha/CE
    1263: CodIBGE := 2308377; // Miraima/CE
    1469: CodIBGE := 2308401; // Missao Velha/CE
    1471: CodIBGE := 2308500; // Mombaca/CE
    1473: CodIBGE := 2308609; // Monsenhor Tabosa/CE
    1475: CodIBGE := 2308708; // Morada Nova/CE
    1477: CodIBGE := 2308807; // Moraujo/CE
    1479: CodIBGE := 2308906; // Morrinhos/CE
    1481: CodIBGE := 2309003; // Mucambo/CE
    1483: CodIBGE := 2309102; // Mulungu/CE
    1485: CodIBGE := 2309201; // Nova Olinda/CE
    1487: CodIBGE := 2309300; // Nova Russas/CE
    1489: CodIBGE := 2309409; // Novo Oriente/CE
    1265: CodIBGE := 2309458; // Ocara/CE
    1491: CodIBGE := 2309508; // Oros/CE
    1493: CodIBGE := 2309607; // Pacajus/CE
    1495: CodIBGE := 2309706; // Pacatuba/CE
    1497: CodIBGE := 2309805; // Pacoti/CE
    1499: CodIBGE := 2309904; // Pacuja/CE
    1501: CodIBGE := 2310001; // Palhano/CE
    1503: CodIBGE := 2310100; // Palmacia/CE
    1505: CodIBGE := 2310209; // Paracuru/CE
    1599: CodIBGE := 2310258; // Paraipaba/CE
    1507: CodIBGE := 2310308; // Parambu/CE
    1509: CodIBGE := 2310407; // Paramoti/CE
    1511: CodIBGE := 2310506; // Pedra Branca/CE
    1513: CodIBGE := 2310605; // Penaforte/CE
    1515: CodIBGE := 2310704; // Pentecoste/CE
    1517: CodIBGE := 2310803; // Pereiro/CE
    1267: CodIBGE := 2310852; // Pindoretama/CE
    1519: CodIBGE := 2310902; // Piquet Carneiro/CE
    1269: CodIBGE := 2310951; // Pires Ferreira/CE
    1521: CodIBGE := 2311009; // Poranga/CE
    1523: CodIBGE := 2311108; // Porteiras/CE
    1525: CodIBGE := 2311207; // Potengi/CE
    1271: CodIBGE := 2311231; // Potiretama/CE
    9917: CodIBGE := 2311264; // Quiterianopolis/CE
    1527: CodIBGE := 2311306; // Quixada/CE
    9853: CodIBGE := 2311355; // Quixelo/CE
    1529: CodIBGE := 2311405; // Quixeramobim/CE
    1531: CodIBGE := 2311504; // Quixere/CE
    1533: CodIBGE := 2311603; // Redencao/CE
    1535: CodIBGE := 2311702; // Reriutaba/CE
    1537: CodIBGE := 2311801; // Russas/CE
    1539: CodIBGE := 2311900; // Saboeiro/CE
    1273: CodIBGE := 2311959; // Salitre/CE
    1541: CodIBGE := 2312007; // Santana Do Acarau/CE
    1543: CodIBGE := 2312106; // Santana Do Cariri/CE
    1545: CodIBGE := 2312205; // Santa Quiteria/CE
    1547: CodIBGE := 2312304; // Sao Benedito/CE
    1549: CodIBGE := 2312403; // Sao Goncalo Do Amarante/CE
    1551: CodIBGE := 2312502; // Sao Joao Do Jaguaribe/CE
    1553: CodIBGE := 2312601; // Sao Luis Do Curu/CE
    1555: CodIBGE := 2312700; // Senador Pompeu/CE
    1557: CodIBGE := 2312809; // Senador Sa/CE
    1559: CodIBGE := 2312908; // Sobral/CE
    1561: CodIBGE := 2313005; // Solonopole/CE
    1563: CodIBGE := 2313104; // Tabuleiro Do Norte/CE
    1565: CodIBGE := 2313203; // Tamboril/CE
    1275: CodIBGE := 2313252; // Tarrafas/CE
    1567: CodIBGE := 2313302; // Taua/CE
    1277: CodIBGE := 2313351; // Tejucuoca/CE
    1569: CodIBGE := 2313401; // Tiangua/CE
    1571: CodIBGE := 2313500; // Trairi/CE
    1279: CodIBGE := 2313559; // Tururu/CE
    1573: CodIBGE := 2313609; // Ubajara/CE
    1575: CodIBGE := 2313708; // Umari/CE
    9855: CodIBGE := 2313757; // Umirim/CE
    1577: CodIBGE := 2313807; // Uruburetama/CE
    1579: CodIBGE := 2313906; // Uruoca/CE
    9857: CodIBGE := 2313955; // Varjota/CE
    1581: CodIBGE := 2314003; // Varzea Alegre/CE
    1583: CodIBGE := 2314102; // Vicosa Do Ceara/CE
    1601: CodIBGE := 2400109; // Acari/RN
    1603: CodIBGE := 2400208; // Acu/RN
    1605: CodIBGE := 2400307; // Afonso Bezerra/RN
    1607: CodIBGE := 2400406; // Agua Nova/RN
    1609: CodIBGE := 2400505; // Alexandria/RN
    1611: CodIBGE := 2400604; // Almino Afonso/RN
    1613: CodIBGE := 2400703; // Alto Do Rodrigues/RN
    1615: CodIBGE := 2400802; // Angicos/RN
    1617: CodIBGE := 2400901; // Antonio Martins/RN
    1619: CodIBGE := 2401008; // Apodi/RN
    1621: CodIBGE := 2401107; // Areia Branca/RN
    1623: CodIBGE := 2401206; // Ares/RN
    1625: CodIBGE := 2401305; // Augusto Severo/RN
    1627: CodIBGE := 2401404; // Baia Formosa/RN
    3003: CodIBGE := 2401453; // Barauna/RN
    1629: CodIBGE := 2401503; // Barcelona/RN
    1631: CodIBGE := 2401602; // Bento Fernandes/RN
    0412: CodIBGE := 2401651; // Bodo/RN
    1633: CodIBGE := 2401701; // Bom Jesus/RN
    1635: CodIBGE := 2401800; // Brejinho/RN
    0414: CodIBGE := 2401859; // Caicara Do Norte/RN
    1637: CodIBGE := 2401909; // Caicara Do Rio Do Vento/RN
    1639: CodIBGE := 2402006; // Caico/RN
    1641: CodIBGE := 2402105; // Campo Redondo/RN
    1643: CodIBGE := 2402204; // Canguaretama/RN
    1645: CodIBGE := 2402303; // Caraubas/RN
    1647: CodIBGE := 2402402; // Carnauba Dos Dantas/RN
    1649: CodIBGE := 2402501; // Carnaubais/RN
    1651: CodIBGE := 2402600; // Ceara-Mirim/RN
    1653: CodIBGE := 2402709; // Cerro Cora/RN
    1655: CodIBGE := 2402808; // Coronel Ezequiel/RN
    1657: CodIBGE := 2402907; // Coronel Joao Pessoa/RN
    1659: CodIBGE := 2403004; // Cruzeta/RN
    1661: CodIBGE := 2403103; // Currais Novos/RN
    1663: CodIBGE := 2403202; // Doutor Severiano/RN
    1779: CodIBGE := 2403251; // Parnamirim/RN
    1665: CodIBGE := 2403301; // Encanto/RN
    1667: CodIBGE := 2403400; // Equador/RN
    1669: CodIBGE := 2403509; // Espirito Santo/RN
    1671: CodIBGE := 2403608; // Extremoz/RN
    1673: CodIBGE := 2403707; // Felipe Guerra/RN
    0416: CodIBGE := 2403756; // Fernando Pedroza/RN
    1675: CodIBGE := 2403806; // Florania/RN
    1677: CodIBGE := 2403905; // Francisco Dantas/RN
    1751: CodIBGE := 2404002; // Frutuoso Gomes/RN
    1679: CodIBGE := 2404101; // Galinhos/RN
    1681: CodIBGE := 2404200; // Goianinha/RN
    1683: CodIBGE := 2404309; // Governador Dix-Sept Rosado/RN
    1685: CodIBGE := 2404408; // Grossos/RN
    1687: CodIBGE := 2404507; // Guamare/RN
    1689: CodIBGE := 2404606; // Ielmo Marinho/RN
    1691: CodIBGE := 2404705; // Ipanguacu/RN
    1693: CodIBGE := 2404804; // Ipueira/RN
    0418: CodIBGE := 2404853; // Itaja/RN
    1695: CodIBGE := 2404903; // Itau/RN
    1697: CodIBGE := 2405009; // Jacana/RN
    1699: CodIBGE := 2405108; // Jandaira/RN
    1701: CodIBGE := 2405207; // Janduis/RN
    1703: CodIBGE := 2405306; // Januario Cicco/RN
    1705: CodIBGE := 2405405; // Japi/RN
    1707: CodIBGE := 2405504; // Jardim De Angicos/RN
    1709: CodIBGE := 2405603; // Jardim De Piranhas/RN
    1711: CodIBGE := 2405702; // Jardim Do Serido/RN
    1713: CodIBGE := 2405801; // Joao Camara/RN
    1715: CodIBGE := 2405900; // Joao Dias/RN
    1717: CodIBGE := 2406007; // Jose Da Penha/RN
    1719: CodIBGE := 2406106; // Jucurutu/RN
    1108: CodIBGE := 2406155; // Jundia/RN
    1723: CodIBGE := 2406205; // Lagoa D Anta/RN
    1725: CodIBGE := 2406304; // Lagoa De Pedras/RN
    1727: CodIBGE := 2406403; // Lagoa De Velhos/RN
    1729: CodIBGE := 2406502; // Lagoa Nova/RN
    1731: CodIBGE := 2406601; // Lagoa Salgada/RN
    1733: CodIBGE := 2406700; // Lajes/RN
    1735: CodIBGE := 2406809; // Lajes Pintadas/RN
    1737: CodIBGE := 2406908; // Lucrecia/RN
    1739: CodIBGE := 2407005; // Luis Gomes/RN
    1741: CodIBGE := 2407104; // Macaiba/RN
    1743: CodIBGE := 2407203; // Macau/RN
    0420: CodIBGE := 2407252; // Major Sales/RN
    1745: CodIBGE := 2407302; // Marcelino Vieira/RN
    1747: CodIBGE := 2407401; // Martins/RN
    1749: CodIBGE := 2407500; // Maxaranguape/RN
    1721: CodIBGE := 2407609; // Messias Targino/RN
    1753: CodIBGE := 2407708; // Montanhas/RN
    1755: CodIBGE := 2407807; // Monte Alegre/RN
    1757: CodIBGE := 2407906; // Monte Das Gameleiras/RN
    1759: CodIBGE := 2408003; // Mossoro/RN
    1761: CodIBGE := 2408102; // Natal/RN
    1763: CodIBGE := 2408201; // Nisia Floresta/RN
    1765: CodIBGE := 2408300; // Nova Cruz/RN
    1767: CodIBGE := 2408409; // Olho-D Agua Do Borges/RN
    1769: CodIBGE := 2408508; // Ouro Branco/RN
    1771: CodIBGE := 2408607; // Parana/RN
    1773: CodIBGE := 2408706; // Parau/RN
    1775: CodIBGE := 2408805; // Parazinho/RN
    1777: CodIBGE := 2408904; // Parelhas/RN
    0422: CodIBGE := 2408953; // Rio Do Fogo/RN
    1781: CodIBGE := 2409100; // Passa E Fica/RN
    1783: CodIBGE := 2409209; // Passagem/RN
    1785: CodIBGE := 2409308; // Patu/RN
    0424: CodIBGE := 2409332; // Santa Maria/RN
    1787: CodIBGE := 2409407; // Pau Dos Ferros/RN
    1789: CodIBGE := 2409506; // Pedra Grande/RN
    1791: CodIBGE := 2409605; // Pedra Preta/RN
    1793: CodIBGE := 2409704; // Pedro Avelino/RN
    1795: CodIBGE := 2409803; // Pedro Velho/RN
    1797: CodIBGE := 2409902; // Pendencias/RN
    1799: CodIBGE := 2410009; // Piloes/RN
    1801: CodIBGE := 2410108; // Poco Branco/RN
    1803: CodIBGE := 2410207; // Portalegre/RN
    0426: CodIBGE := 2410256; // Porto Do Mangue/RN
    1805: CodIBGE := 2410306; // Presidente Juscelino/RN
    1807: CodIBGE := 2410405; // Pureza/RN
    1809: CodIBGE := 2410504; // Rafael Fernandes/RN
    1893: CodIBGE := 2410603; // Rafael Godeiro/RN
    1811: CodIBGE := 2410702; // Riacho Da Cruz/RN
    1813: CodIBGE := 2410801; // Riacho De Santana/RN
    1815: CodIBGE := 2410900; // Riachuelo/RN
    1817: CodIBGE := 2411007; // Rodolfo Fernandes/RN
    0428: CodIBGE := 2411056; // Tibau/RN
    1819: CodIBGE := 2411106; // Ruy Barbosa/RN
    1823: CodIBGE := 2411205; // Santa Cruz/RN
    1827: CodIBGE := 2411403; // Santana Do Matos/RN
    1825: CodIBGE := 2411429; // Santana Do Serido/RN
    1829: CodIBGE := 2411502; // Santo Antonio/RN
    1831: CodIBGE := 2411601; // Sao Bento Do Norte/RN
    1833: CodIBGE := 2411700; // Sao Bento Do Trairi/RN
    1835: CodIBGE := 2411809; // Sao Fernando/RN
    1821: CodIBGE := 2411908; // Sao Francisco Do Oeste/RN
    1837: CodIBGE := 2412005; // Sao Goncalo Do Amarante/RN
    1839: CodIBGE := 2412104; // Sao Joao Do Sabugi/RN
    1841: CodIBGE := 2412203; // Sao Jose De Mipibu/RN
    1843: CodIBGE := 2412302; // Sao Jose Do Campestre/RN
    1845: CodIBGE := 2412401; // Sao Jose Do Serido/RN
    1847: CodIBGE := 2412500; // Sao Miguel/RN
    0430: CodIBGE := 2412559; // Sao Miguel Do Gostoso/RN
    1849: CodIBGE := 2412609; // Sao Paulo Do Potengi/RN
    1851: CodIBGE := 2412708; // Sao Pedro/RN
    1853: CodIBGE := 2412807; // Sao Rafael/RN
    1855: CodIBGE := 2412906; // Sao Tome/RN
    1857: CodIBGE := 2413003; // Sao Vicente/RN
    1859: CodIBGE := 2413102; // Senador Eloi De Souza/RN
    1861: CodIBGE := 2413201; // Senador Georgino Avelino/RN
    1863: CodIBGE := 2413300; // Serra De Sao Bento/RN
    1927: CodIBGE := 2413359; // Serra Do Mel/RN
    1865: CodIBGE := 2413409; // Serra Negra Do Norte/RN
    1867: CodIBGE := 2413508; // Serrinha/RN
    0432: CodIBGE := 2413557; // Serrinha Dos Pintos/RN
    1869: CodIBGE := 2413607; // Severiano Melo/RN
    1871: CodIBGE := 2413706; // Sitio Novo/RN
    1873: CodIBGE := 2413805; // Taboleiro Grande/RN
    1875: CodIBGE := 2413904; // Taipu/RN
    1877: CodIBGE := 2414001; // Tangara/RN
    1879: CodIBGE := 2414100; // Tenente Ananias/RN
    0434: CodIBGE := 2414159; // Tenente Laurentino Cruz/RN
    1881: CodIBGE := 2414209; // Tibau Do Sul/RN
    1883: CodIBGE := 2414308; // Timbauba Dos Batistas/RN
    1885: CodIBGE := 2414407; // Touros/RN
    0436: CodIBGE := 2414456; // Triunfo Potiguar/RN
    1887: CodIBGE := 2414506; // Umarizal/RN
    1889: CodIBGE := 2414605; // Upanema/RN
    1891: CodIBGE := 2414704; // Varzea/RN
    0438: CodIBGE := 2414753; // Venha-Ver/RN
    1895: CodIBGE := 2414803; // Vera Cruz/RN
    1897: CodIBGE := 2414902; // Vicosa/RN
    1899: CodIBGE := 2415008; // Vila Flor/RN
    1901: CodIBGE := 2500106; // Agua Branca/PB
    1903: CodIBGE := 2500205; // Aguiar/PB
    1905: CodIBGE := 2500304; // Alagoa Grande/PB
    1907: CodIBGE := 2500403; // Alagoa Nova/PB
    1909: CodIBGE := 2500502; // Alagoinha/PB
    0440: CodIBGE := 2500536; // Alcantil/PB
    0442: CodIBGE := 2500577; // Algodao De Jandaira/PB
    1911: CodIBGE := 2500601; // Alhandra/PB
    1913: CodIBGE := 2500700; // Sao Joao Do Rio Do Peixe/PB
    0444: CodIBGE := 2500734; // Amparo/PB
    0446: CodIBGE := 2500775; // Aparecida/PB
    1915: CodIBGE := 2500809; // Aracagi/PB
    1917: CodIBGE := 2500908; // Arara/PB
    1919: CodIBGE := 2501005; // Araruna/PB
    1921: CodIBGE := 2501104; // Areia/PB
    0448: CodIBGE := 2501153; // Areia De Baraunas/PB
    1923: CodIBGE := 2501203; // Areial/PB
    1925: CodIBGE := 2501302; // Aroeiras/PB
    0450: CodIBGE := 2501351; // Assuncao/PB
    1929: CodIBGE := 2501401; // Baia Da Traicao/PB
    1931: CodIBGE := 2501500; // Bananeiras/PB
    0452: CodIBGE := 2501534; // Barauna/PB
    0454: CodIBGE := 2501575; // Barra De Santana/PB
    1933: CodIBGE := 2501609; // Barra De Santa Rosa/PB
    1935: CodIBGE := 2501708; // Barra De Sao Miguel/PB
    1937: CodIBGE := 2501807; // Bayeux/PB
    1939: CodIBGE := 2501906; // Belem/PB
    1941: CodIBGE := 2502003; // Belem Do Brejo Do Cruz/PB
    0456: CodIBGE := 2502052; // Bernardino Batista/PB
    1943: CodIBGE := 2502102; // Boa Ventura/PB
    0458: CodIBGE := 2502151; // Boa Vista/PB
    1945: CodIBGE := 2502201; // Bom Jesus/PB
    1947: CodIBGE := 2502300; // Bom Sucesso/PB
    1949: CodIBGE := 2502409; // Bonito De Santa Fe/PB
    1951: CodIBGE := 2502508; // Boqueirao/PB
    1953: CodIBGE := 2502607; // Igaracy/PB
    1955: CodIBGE := 2502706; // Borborema/PB
    1957: CodIBGE := 2502805; // Brejo Do Cruz/PB
    1959: CodIBGE := 2502904; // Brejo Dos Santos/PB
    1961: CodIBGE := 2503001; // Caapora/PB
    1963: CodIBGE := 2503100; // Cabaceiras/PB
    1965: CodIBGE := 2503209; // Cabedelo/PB
    1967: CodIBGE := 2503308; // Cachoeira Dos Indios/PB
    1969: CodIBGE := 2503407; // Cacimba De Areia/PB
    1971: CodIBGE := 2503506; // Cacimba De Dentro/PB
    0460: CodIBGE := 2503555; // Cacimbas/PB
    1973: CodIBGE := 2503605; // Caicara/PB
    1975: CodIBGE := 2503704; // Cajazeiras/PB
    0462: CodIBGE := 2503753; // Cajazeirinhas/PB
    1977: CodIBGE := 2503803; // Caldas Brandao/PB
    1979: CodIBGE := 2503902; // Camalau/PB
    1981: CodIBGE := 2504009; // Campina Grande/PB
    0464: CodIBGE := 2504033; // Capim/PB
    0466: CodIBGE := 2504074; // Caraubas/PB
    1983: CodIBGE := 2504108; // Carrapateira/PB
    0468: CodIBGE := 2504157; // Casserengue/PB
    1985: CodIBGE := 2504207; // Catingueira/PB
    1987: CodIBGE := 2504306; // Catole Do Rocha/PB
    0470: CodIBGE := 2504355; // Caturite/PB
    1989: CodIBGE := 2504405; // Conceicao/PB
    1991: CodIBGE := 2504504; // Condado/PB
    1993: CodIBGE := 2504603; // Conde/PB
    1995: CodIBGE := 2504702; // Congo/PB
    1997: CodIBGE := 2504801; // Coremas/PB
    0472: CodIBGE := 2504850; // Coxixola/PB
    1999: CodIBGE := 2504900; // Cruz Do Espirito Santo/PB
    2001: CodIBGE := 2505006; // Cubati/PB
    2003: CodIBGE := 2505105; // Cuite/PB
    2005: CodIBGE := 2505204; // Cuitegi/PB
    0474: CodIBGE := 2505238; // Cuite De Mamanguape/PB
    0476: CodIBGE := 2505279; // Curral De Cima/PB
    2007: CodIBGE := 2505303; // Curral Velho/PB
    0478: CodIBGE := 2505352; // Damiao/PB
    2009: CodIBGE := 2505402; // Desterro/PB
    2011: CodIBGE := 2505501; // Vista Serrana/PB
    2013: CodIBGE := 2505600; // Diamante/PB
    2015: CodIBGE := 2505709; // Dona Ines/PB
    2017: CodIBGE := 2505808; // Duas Estradas/PB
    2019: CodIBGE := 2505907; // Emas/PB
    2021: CodIBGE := 2506004; // Esperanca/PB
    2023: CodIBGE := 2506103; // Fagundes/PB
    2025: CodIBGE := 2506202; // Frei Martinho/PB
    0480: CodIBGE := 2506251; // Gado Bravo/PB
    2027: CodIBGE := 2506301; // Guarabira/PB
    2029: CodIBGE := 2506400; // Gurinhem/PB
    2031: CodIBGE := 2506509; // Gurjao/PB
    2033: CodIBGE := 2506608; // Ibiara/PB
    2035: CodIBGE := 2506707; // Imaculada/PB
    2037: CodIBGE := 2506806; // Inga/PB
    2039: CodIBGE := 2506905; // Itabaiana/PB
    2041: CodIBGE := 2507002; // Itaporanga/PB
    2043: CodIBGE := 2507101; // Itapororoca/PB
    2045: CodIBGE := 2507200; // Itatuba/PB
    2047: CodIBGE := 2507309; // Jacarau/PB
    2049: CodIBGE := 2507408; // Jerico/PB
    2051: CodIBGE := 2507507; // Joao Pessoa/PB
    2053: CodIBGE := 2507606; // Juarez Tavora/PB
    2055: CodIBGE := 2507705; // Juazeirinho/PB
    2057: CodIBGE := 2507804; // Junco Do Serido/PB
    2059: CodIBGE := 2507903; // Juripiranga/PB
    2061: CodIBGE := 2508000; // Juru/PB
    2063: CodIBGE := 2508109; // Lagoa/PB
    2065: CodIBGE := 2508208; // Lagoa De Dentro/PB
    2067: CodIBGE := 2508307; // Lagoa Seca/PB
    2069: CodIBGE := 2508406; // Lastro/PB
    2071: CodIBGE := 2508505; // Livramento/PB
    0482: CodIBGE := 2508554; // Logradouro/PB
    2073: CodIBGE := 2508604; // Lucena/PB
    2075: CodIBGE := 2508703; // Mae D Agua/PB
    2077: CodIBGE := 2508802; // Malta/PB
    2079: CodIBGE := 2508901; // Mamanguape/PB
    2081: CodIBGE := 2509008; // Manaira/PB
    0484: CodIBGE := 2509057; // Marcacao/PB
    2083: CodIBGE := 2509107; // Mari/PB
    0486: CodIBGE := 2509156; // Marizopolis/PB
    2085: CodIBGE := 2509206; // Massaranduba/PB
    2087: CodIBGE := 2509305; // Mataraca/PB
    0488: CodIBGE := 2509339; // Matinhas/PB
    0490: CodIBGE := 2509370; // Mato Grosso/PB
    0492: CodIBGE := 2509396; // Matureia/PB
    2089: CodIBGE := 2509404; // Mogeiro/PB
    2091: CodIBGE := 2509503; // Montadas/PB
    2093: CodIBGE := 2509602; // Monte Horebe/PB
    2095: CodIBGE := 2509701; // Monteiro/PB
    2097: CodIBGE := 2509800; // Mulungu/PB
    2099: CodIBGE := 2509909; // Natuba/PB
    2101: CodIBGE := 2510006; // Nazarezinho/PB
    2103: CodIBGE := 2510105; // Nova Floresta/PB
    2105: CodIBGE := 2510204; // Nova Olinda/PB
    2107: CodIBGE := 2510303; // Nova Palmeira/PB
    2109: CodIBGE := 2510402; // Olho D Agua/PB
    2111: CodIBGE := 2510501; // Olivedos/PB
    2113: CodIBGE := 2510600; // Ouro Velho/PB
    0494: CodIBGE := 2510659; // Parari/PB
    2115: CodIBGE := 2510709; // Passagem/PB
    2117: CodIBGE := 2510808; // Patos/PB
    2119: CodIBGE := 2510907; // Paulista/PB
    2121: CodIBGE := 2511004; // Pedra Branca/PB
    2123: CodIBGE := 2511103; // Pedra Lavrada/PB
    2125: CodIBGE := 2511202; // Pedras De Fogo/PB
    2127: CodIBGE := 2511301; // Pianco/PB
    2129: CodIBGE := 2511400; // Picui/PB
    2131: CodIBGE := 2511509; // Pilar/PB
    2133: CodIBGE := 2511608; // Piloes/PB
    2135: CodIBGE := 2511707; // Piloezinhos/PB
    2137: CodIBGE := 2511806; // Pirpirituba/PB
    2139: CodIBGE := 2511905; // Pitimbu/PB
    2141: CodIBGE := 2512002; // Pocinhos/PB
    0496: CodIBGE := 2512036; // Poco Dantas/PB
    0498: CodIBGE := 2512077; // Poco De Jose De Moura/PB
    2143: CodIBGE := 2512101; // Pombal/PB
    2145: CodIBGE := 2512200; // Prata/PB
    2147: CodIBGE := 2512309; // Princesa Isabel/PB
    2149: CodIBGE := 2512408; // Puxinana/PB
    2151: CodIBGE := 2512507; // Queimadas/PB
    2153: CodIBGE := 2512606; // Quixaba/PB
    2155: CodIBGE := 2512705; // Remigio/PB
    0500: CodIBGE := 2512721; // Pedro Regis/PB
    0502: CodIBGE := 2512747; // Riachao/PB
    0504: CodIBGE := 2512754; // Riachao Do Bacamarte/PB
    0506: CodIBGE := 2512762; // Riachao Do Poco/PB
    0508: CodIBGE := 2512788; // Riacho De Santo Antonio/PB
    2157: CodIBGE := 2512804; // Riacho Dos Cavalos/PB
    2159: CodIBGE := 2512903; // Rio Tinto/PB
    2161: CodIBGE := 2513000; // Salgadinho/PB
    2163: CodIBGE := 2513109; // Salgado De Sao Felix/PB
    0510: CodIBGE := 2513158; // Santa Cecilia/PB
    2165: CodIBGE := 2513208; // Santa Cruz/PB
    2167: CodIBGE := 2513307; // Santa Helena/PB
    0512: CodIBGE := 2513356; // Santa Ines/PB
    2169: CodIBGE := 2513406; // Santa Luzia/PB
    2171: CodIBGE := 2513505; // Santana De Mangueira/PB
    2173: CodIBGE := 2513604; // Santana Dos Garrotes/PB
    0514: CodIBGE := 2513653; // Santarem/PB
    2175: CodIBGE := 2513703; // Santa Rita/PB
    2177: CodIBGE := 2513802; // Santa Teresinha/PB
    0516: CodIBGE := 2513851; // Santo Andre/PB
    2179: CodIBGE := 2513901; // Sao Bento/PB
    0518: CodIBGE := 2513927; // Sao Bentinho/PB
    0520: CodIBGE := 2513943; // Sao Domingos Do Cariri/PB
    0522: CodIBGE := 2513968; // Sao Domingos/PB
    0524: CodIBGE := 2513984; // Sao Francisco/PB
    2181: CodIBGE := 2514008; // Sao Joao Do Cariri/PB
    2183: CodIBGE := 2514107; // Sao Joao Do Tigre/PB
    2185: CodIBGE := 2514206; // Sao Jose Da Lagoa Tapada/PB
    2187: CodIBGE := 2514305; // Sao Jose De Caiana/PB
    2189: CodIBGE := 2514404; // Sao Jose De Espinharas/PB
    0526: CodIBGE := 2514453; // Sao Jose Dos Ramos/PB
    2191: CodIBGE := 2514503; // Sao Jose De Piranhas/PB
    0528: CodIBGE := 2514552; // Sao Jose De Princesa/PB
    2193: CodIBGE := 2514602; // Sao Jose Do Bonfim/PB
    0530: CodIBGE := 2514651; // Sao Jose Do Brejo Do Cruz/PB
    2195: CodIBGE := 2514701; // Sao Jose Do Sabugi/PB
    2197: CodIBGE := 2514800; // Sao Jose Dos Cordeiros/PB
    2199: CodIBGE := 2514909; // Sao Mamede/PB
    2201: CodIBGE := 2515005; // Sao Miguel De Taipu/PB
    2203: CodIBGE := 2515104; // Sao Sebastiao De Lagoa De Roca/PB
    2205: CodIBGE := 2515203; // Sao Sebastiao Do Umbuzeiro/PB
    2207: CodIBGE := 2515302; // Sape/PB
    2209: CodIBGE := 2515401; // Serido/PB
    2211: CodIBGE := 2515500; // Serra Branca/PB
    2213: CodIBGE := 2515609; // Serra Da Raiz/PB
    2215: CodIBGE := 2515708; // Serra Grande/PB
    2217: CodIBGE := 2515807; // Serra Redonda/PB
    2219: CodIBGE := 2515906; // Serraria/PB
    0532: CodIBGE := 2515930; // Sertaozinho/PB
    0534: CodIBGE := 2515971; // Sobrado/PB
    2221: CodIBGE := 2516003; // Solanea/PB
    2223: CodIBGE := 2516102; // Soledade/PB
    0536: CodIBGE := 2516151; // Sossego/PB
    2225: CodIBGE := 2516201; // Sousa/PB
    2227: CodIBGE := 2516300; // Sume/PB
    2229: CodIBGE := 2516409; // Campo De Santana/PB
    2231: CodIBGE := 2516508; // Taperoa/PB
    2233: CodIBGE := 2516607; // Tavares/PB
    2235: CodIBGE := 2516706; // Teixeira/PB
    0538: CodIBGE := 2516755; // Tenorio/PB
    2237: CodIBGE := 2516805; // Triunfo/PB
    2239: CodIBGE := 2516904; // Uirauna/PB
    2241: CodIBGE := 2517001; // Umbuzeiro/PB
    2243: CodIBGE := 2517100; // Varzea/PB
    0540: CodIBGE := 2517209; // Vieiropolis/PB
    0542: CodIBGE := 2517407; // Zabele/PB
    2631: CodIBGE := 2600054; // Abreu E Lima/PE
    2301: CodIBGE := 2600104; // Afogados Da Ingazeira/PE
    2303: CodIBGE := 2600203; // Afranio/PE
    2305: CodIBGE := 2600302; // Agrestina/PE
    2307: CodIBGE := 2600401; // Agua Preta/PE
    2309: CodIBGE := 2600500; // Aguas Belas/PE
    2311: CodIBGE := 2600609; // Alagoinha/PE
    2313: CodIBGE := 2600708; // Alianca/PE
    2315: CodIBGE := 2600807; // Altinho/PE
    2317: CodIBGE := 2600906; // Amaraji/PE
    2319: CodIBGE := 2601003; // Angelim/PE
    0544: CodIBGE := 2601052; // Aracoiaba/PE
    2321: CodIBGE := 2601102; // Araripina/PE
    2323: CodIBGE := 2601201; // Arcoverde/PE
    2325: CodIBGE := 2601300; // Barra De Guabiraba/PE
    2327: CodIBGE := 2601409; // Barreiros/PE
    2329: CodIBGE := 2601508; // Belem De Maria/PE
    2331: CodIBGE := 2601607; // Belem De Sao Francisco/PE
    2333: CodIBGE := 2601706; // Belo Jardim/PE
    2335: CodIBGE := 2601805; // Betania/PE
    2337: CodIBGE := 2601904; // Bezerros/PE
    2339: CodIBGE := 2602001; // Bodoco/PE
    2341: CodIBGE := 2602100; // Bom Conselho/PE
    2343: CodIBGE := 2602209; // Bom Jardim/PE
    2345: CodIBGE := 2602308; // Bonito/PE
    2347: CodIBGE := 2602407; // Brejao/PE
    2349: CodIBGE := 2602506; // Brejinho/PE
    2351: CodIBGE := 2602605; // Brejo Da Madre De Deus/PE
    2353: CodIBGE := 2602704; // Buenos Aires/PE
    2355: CodIBGE := 2602803; // Buique/PE
    2357: CodIBGE := 2602902; // Cabo De Santo Agostinho/PE
    2359: CodIBGE := 2603009; // Cabrobo/PE
    2361: CodIBGE := 2603108; // Cachoeirinha/PE
    2363: CodIBGE := 2603207; // Caetes/PE
    2365: CodIBGE := 2603306; // Calcado/PE
    2367: CodIBGE := 2603405; // Calumbi/PE
    2629: CodIBGE := 2603454; // Camaragibe/PE
    2369: CodIBGE := 2603504; // Camocim De Sao Felix/PE
    2371: CodIBGE := 2603603; // Camutanga/PE
    2373: CodIBGE := 2603702; // Canhotinho/PE
    2375: CodIBGE := 2603801; // Capoeiras/PE
    2377: CodIBGE := 2603900; // Carnaiba/PE
    2635: CodIBGE := 2603926; // Carnaubeira Da Penha/PE
    2379: CodIBGE := 2604007; // Carpina/PE
    2381: CodIBGE := 2604106; // Caruaru/PE
    0546: CodIBGE := 2604155; // Casinhas/PE
    2383: CodIBGE := 2604205; // Catende/PE
    2385: CodIBGE := 2604304; // Cedro/PE
    2387: CodIBGE := 2604403; // Cha De Alegria/PE
    2389: CodIBGE := 2604502; // Cha Grande/PE
    2391: CodIBGE := 2604601; // Condado/PE
    2393: CodIBGE := 2604700; // Correntes/PE
    2395: CodIBGE := 2604809; // Cortes/PE
    2397: CodIBGE := 2604908; // Cumaru/PE
    2399: CodIBGE := 2605004; // Cupira/PE
    2401: CodIBGE := 2605103; // Custodia/PE
    2299: CodIBGE := 2605152; // Dormentes/PE
    2403: CodIBGE := 2605202; // Escada/PE
    2405: CodIBGE := 2605301; // Exu/PE
    2407: CodIBGE := 2605400; // Feira Nova/PE
    3001: CodIBGE := 2605459; // Fernando De Noronha/PE
    2409: CodIBGE := 2605509; // Ferreiros/PE
    2411: CodIBGE := 2605608; // Flores/PE
    2413: CodIBGE := 2605707; // Floresta/PE
    2415: CodIBGE := 2605806; // Frei Miguelinho/PE
    2417: CodIBGE := 2605905; // Gameleira/PE
    2419: CodIBGE := 2606002; // Garanhuns/PE
    2421: CodIBGE := 2606101; // Gloria Do Goita/PE
    2423: CodIBGE := 2606200; // Goiana/PE
    2425: CodIBGE := 2606309; // Granito/PE
    2427: CodIBGE := 2606408; // Gravata/PE
    2429: CodIBGE := 2606507; // Iati/PE
    2431: CodIBGE := 2606606; // Ibimirim/PE
    2433: CodIBGE := 2606705; // Ibirajuba/PE
    2435: CodIBGE := 2606804; // Igarassu/PE
    2437: CodIBGE := 2606903; // Iguaraci/PE
    2439: CodIBGE := 2607000; // Inaja/PE
    2441: CodIBGE := 2607109; // Ingazeira/PE
    2443: CodIBGE := 2607208; // Ipojuca/PE
    2445: CodIBGE := 2607307; // Ipubi/PE
    2447: CodIBGE := 2607406; // Itacuruba/PE
    2449: CodIBGE := 2607505; // Itaiba/PE
    2451: CodIBGE := 2607604; // Ilha De Itamaraca/PE
    2597: CodIBGE := 2607653; // Itambe/PE
    2453: CodIBGE := 2607703; // Itapetim/PE
    2633: CodIBGE := 2607752; // Itapissuma/PE
    2455: CodIBGE := 2607802; // Itaquitinga/PE
    2457: CodIBGE := 2607901; // Jaboatao Dos Guararapes/PE
    0548: CodIBGE := 2607950; // Jaqueira/PE
    2459: CodIBGE := 2608008; // Jatauba/PE
    0550: CodIBGE := 2608057; // Jatoba/PE
    2461: CodIBGE := 2608107; // Joao Alfredo/PE
    2463: CodIBGE := 2608206; // Joaquim Nabuco/PE
    2295: CodIBGE := 2608255; // Jucati/PE
    2465: CodIBGE := 2608305; // Jupi/PE
    2467: CodIBGE := 2608404; // Jurema/PE
    2289: CodIBGE := 2608453; // Lagoa Do Carro/PE
    2469: CodIBGE := 2608503; // Lagoa Do Itaenga/PE
    2471: CodIBGE := 2608602; // Lagoa Do Ouro/PE
    2473: CodIBGE := 2608701; // Lagoa Dos Gatos/PE
    0552: CodIBGE := 2608750; // Lagoa Grande/PE
    2475: CodIBGE := 2608800; // Lajedo/PE
    2477: CodIBGE := 2608909; // Limoeiro/PE
    2479: CodIBGE := 2609006; // Macaparana/PE
    2481: CodIBGE := 2609105; // Machados/PE
    0554: CodIBGE := 2609154; // Manari/PE
    2483: CodIBGE := 2609204; // Maraial/PE
    2485: CodIBGE := 2609303; // Mirandiba/PE
    2487: CodIBGE := 2609402; // Moreno/PE
    2489: CodIBGE := 2609501; // Nazare Da Mata/PE
    2491: CodIBGE := 2609600; // Olinda/PE
    2493: CodIBGE := 2609709; // Orobo/PE
    2495: CodIBGE := 2609808; // Oroco/PE
    2497: CodIBGE := 2609907; // Ouricuri/PE
    2499: CodIBGE := 2610004; // Palmares/PE
    2501: CodIBGE := 2610103; // Palmeirina/PE
    2503: CodIBGE := 2610202; // Panelas/PE
    2505: CodIBGE := 2610301; // Paranatama/PE
    2507: CodIBGE := 2610400; // Parnamirim/PE
    2509: CodIBGE := 2610509; // Passira/PE
    2511: CodIBGE := 2610608; // Paudalho/PE
    2513: CodIBGE := 2610707; // Paulista/PE
    2515: CodIBGE := 2610806; // Pedra/PE
    2517: CodIBGE := 2610905; // Pesqueira/PE
    2519: CodIBGE := 2611002; // Petrolandia/PE
    2521: CodIBGE := 2611101; // Petrolina/PE
    2523: CodIBGE := 2611200; // Pocao/PE
    2525: CodIBGE := 2611309; // Pombos/PE
    2527: CodIBGE := 2611408; // Primavera/PE
    2529: CodIBGE := 2611507; // Quipapa/PE
    2637: CodIBGE := 2611533; // Quixaba/PE
    2531: CodIBGE := 2611606; // Recife/PE
    2533: CodIBGE := 2611705; // Riacho Das Almas/PE
    2535: CodIBGE := 2611804; // Ribeirao/PE
    2537: CodIBGE := 2611903; // Rio Formoso/PE
    2539: CodIBGE := 2612000; // Saire/PE
    2541: CodIBGE := 2612109; // Salgadinho/PE
    2543: CodIBGE := 2612208; // Salgueiro/PE
    2545: CodIBGE := 2612307; // Saloa/PE
    2547: CodIBGE := 2612406; // Sanharo/PE
    2297: CodIBGE := 2612455; // Santa Cruz/PE
    2639: CodIBGE := 2612471; // Santa Cruz Da Baixa Verde/PE
    2549: CodIBGE := 2612505; // Santa Cruz Do Capibaribe/PE
    0556: CodIBGE := 2612554; // Santa Filomena/PE
    2551: CodIBGE := 2612604; // Santa Maria Da Boa Vista/PE
    2553: CodIBGE := 2612703; // Santa Maria Do Cambuca/PE
    2555: CodIBGE := 2612802; // Santa Terezinha/PE
    2557: CodIBGE := 2612901; // Sao Benedito Do Sul/PE
    2559: CodIBGE := 2613008; // Sao Bento Do Una/PE
    2561: CodIBGE := 2613107; // Sao Caitano/PE
    2563: CodIBGE := 2613206; // Sao Joao/PE
    2565: CodIBGE := 2613305; // Sao Joaquim Do Monte/PE
    2567: CodIBGE := 2613404; // Sao Jose Da Coroa Grande/PE
    2569: CodIBGE := 2613503; // Sao Jose Do Belmonte/PE
    2571: CodIBGE := 2613602; // Sao Jose Do Egito/PE
    2573: CodIBGE := 2613701; // Sao Lourenco Da Mata/PE
    2575: CodIBGE := 2613800; // Sao Vicente Ferrer/PE
    2577: CodIBGE := 2613909; // Serra Talhada/PE
    2579: CodIBGE := 2614006; // Serrita/PE
    2581: CodIBGE := 2614105; // Sertania/PE
    2583: CodIBGE := 2614204; // Sirinhaem/PE
    2585: CodIBGE := 2614303; // Moreilandia/PE
    2587: CodIBGE := 2614402; // Solidao/PE
    2589: CodIBGE := 2614501; // Surubim/PE
    2591: CodIBGE := 2614600; // Tabira/PE
    2593: CodIBGE := 2614709; // Tacaimbo/PE
    2595: CodIBGE := 2614808; // Tacaratu/PE
    0558: CodIBGE := 2614857; // Tamandare/PE
    2599: CodIBGE := 2615003; // Taquaritinga Do Norte/PE
    2601: CodIBGE := 2615102; // Terezinha/PE
    2603: CodIBGE := 2615201; // Terra Nova/PE
    2605: CodIBGE := 2615300; // Timbauba/PE
    2607: CodIBGE := 2615409; // Toritama/PE
    2609: CodIBGE := 2615508; // Tracunhaem/PE
    2611: CodIBGE := 2615607; // Trindade/PE
    2613: CodIBGE := 2615706; // Triunfo/PE
    2615: CodIBGE := 2615805; // Tupanatinga/PE
    2617: CodIBGE := 2615904; // Tuparetama/PE
    2619: CodIBGE := 2616001; // Venturosa/PE
    2621: CodIBGE := 2616100; // Verdejante/PE
    2291: CodIBGE := 2616183; // Vertente Do Lerio/PE
    2623: CodIBGE := 2616209; // Vertentes/PE
    2625: CodIBGE := 2616308; // Vicencia/PE
    2627: CodIBGE := 2616407; // Vitoria De Santo Antao/PE
    2293: CodIBGE := 2616506; // Xexeu/PE
    2701: CodIBGE := 2700102; // Agua Branca/AL
    2703: CodIBGE := 2700201; // Anadia/AL
    2705: CodIBGE := 2700300; // Arapiraca/AL
    2707: CodIBGE := 2700409; // Atalaia/AL
    2709: CodIBGE := 2700508; // Barra De Santo Antonio/AL
    2711: CodIBGE := 2700607; // Barra De Sao Miguel/AL
    2713: CodIBGE := 2700706; // Batalha/AL
    2715: CodIBGE := 2700805; // Belem/AL
    2717: CodIBGE := 2700904; // Belo Monte/AL
    2719: CodIBGE := 2701001; // Boca Da Mata/AL
    2721: CodIBGE := 2701100; // Branquinha/AL
    2723: CodIBGE := 2701209; // Cacimbinhas/AL
    2725: CodIBGE := 2701308; // Cajueiro/AL
    0560: CodIBGE := 2701357; // Campestre/AL
    2727: CodIBGE := 2701407; // Campo Alegre/AL
    2729: CodIBGE := 2701506; // Campo Grande/AL
    2731: CodIBGE := 2701605; // Canapi/AL
    2733: CodIBGE := 2701704; // Capela/AL
    2735: CodIBGE := 2701803; // Carneiros/AL
    2737: CodIBGE := 2701902; // Cha Preta/AL
    2739: CodIBGE := 2702009; // Coite Do Noia/AL
    2741: CodIBGE := 2702108; // Colonia Leopoldina/AL
    2743: CodIBGE := 2702207; // Coqueiro Seco/AL
    2745: CodIBGE := 2702306; // Coruripe/AL
    2889: CodIBGE := 2702355; // Craibas/AL
    2747: CodIBGE := 2702405; // Delmiro Gouveia/AL
    2749: CodIBGE := 2702504; // Dois Riachos/AL
    2643: CodIBGE := 2702553; // Estrela De Alagoas/AL
    2751: CodIBGE := 2702603; // Feira Grande/AL
    2753: CodIBGE := 2702702; // Feliz Deserto/AL
    2755: CodIBGE := 2702801; // Flexeiras/AL
    2757: CodIBGE := 2702900; // Girau Do Ponciano/AL
    2759: CodIBGE := 2703007; // Ibateguara/AL
    2761: CodIBGE := 2703106; // Igaci/AL
    2763: CodIBGE := 2703205; // Igreja Nova/AL
    2765: CodIBGE := 2703304; // Inhapi/AL
    2767: CodIBGE := 2703403; // Jacare Dos Homens/AL
    2769: CodIBGE := 2703502; // Jacuipe/AL
    2771: CodIBGE := 2703601; // Japaratinga/AL
    2773: CodIBGE := 2703700; // Jaramataia/AL
    0562: CodIBGE := 2703759; // Jequia Da Praia/AL
    2775: CodIBGE := 2703809; // Joaquim Gomes/AL
    2777: CodIBGE := 2703908; // Jundia/AL
    2779: CodIBGE := 2704005; // Junqueiro/AL
    2781: CodIBGE := 2704104; // Lagoa Da Canoa/AL
    2783: CodIBGE := 2704203; // Limoeiro De Anadia/AL
    2785: CodIBGE := 2704302; // Maceio/AL
    2787: CodIBGE := 2704401; // Major Isidoro/AL
    2789: CodIBGE := 2704500; // Maragogi/AL
    2791: CodIBGE := 2704609; // Maravilha/AL
    2793: CodIBGE := 2704708; // Marechal Deodoro/AL
    2795: CodIBGE := 2704807; // Maribondo/AL
    2797: CodIBGE := 2704906; // Mar Vermelho/AL
    2799: CodIBGE := 2705002; // Mata Grande/AL
    2801: CodIBGE := 2705101; // Matriz De Camaragibe/AL
    2803: CodIBGE := 2705200; // Messias/AL
    2805: CodIBGE := 2705309; // Minador Do Negrao/AL
    2807: CodIBGE := 2705408; // Monteiropolis/AL
    2809: CodIBGE := 2705507; // Murici/AL
    2811: CodIBGE := 2705606; // Novo Lino/AL
    2813: CodIBGE := 2705705; // Olho D Agua Das Flores/AL
    2815: CodIBGE := 2705804; // Olho D Agua Do Casado/AL
    2817: CodIBGE := 2705903; // Olho D Agua Grande/AL
    2819: CodIBGE := 2706000; // Olivenca/AL
    2821: CodIBGE := 2706109; // Ouro Branco/AL
    2823: CodIBGE := 2706208; // Palestina/AL
    2825: CodIBGE := 2706307; // Palmeira Dos Indios/AL
    2827: CodIBGE := 2706406; // Pao De Acucar/AL
    2645: CodIBGE := 2706422; // Pariconha/AL
    2641: CodIBGE := 2706448; // Paripueira/AL
    2829: CodIBGE := 2706505; // Passo De Camaragibe/AL
    2831: CodIBGE := 2706604; // Paulo Jacinto/AL
    2833: CodIBGE := 2706703; // Penedo/AL
    2835: CodIBGE := 2706802; // Piacabucu/AL
    2837: CodIBGE := 2706901; // Pilar/AL
    2839: CodIBGE := 2707008; // Pindoba/AL
    2841: CodIBGE := 2707107; // Piranhas/AL
    2843: CodIBGE := 2707206; // Poco Das Trincheiras/AL
    2845: CodIBGE := 2707305; // Porto Calvo/AL
    2847: CodIBGE := 2707404; // Porto De Pedras/AL
    2849: CodIBGE := 2707503; // Porto Real Do Colegio/AL
    2851: CodIBGE := 2707602; // Quebrangulo/AL
    2853: CodIBGE := 2707701; // Rio Largo/AL
    2855: CodIBGE := 2707800; // Roteiro/AL
    2857: CodIBGE := 2707909; // Santa Luzia Do Norte/AL
    2859: CodIBGE := 2708006; // Santana Do Ipanema/AL
    2861: CodIBGE := 2708105; // Santana Do Mundau/AL
    2863: CodIBGE := 2708204; // Sao Bras/AL
    2865: CodIBGE := 2708303; // Sao Jose Da Laje/AL
    2867: CodIBGE := 2708402; // Sao Jose Da Tapera/AL
    2869: CodIBGE := 2708501; // Sao Luis Do Quitunde/AL
    2871: CodIBGE := 2708600; // Sao Miguel Dos Campos/AL
    2873: CodIBGE := 2708709; // Sao Miguel Dos Milagres/AL
    2875: CodIBGE := 2708808; // Sao Sebastiao/AL
    2877: CodIBGE := 2708907; // Satuba/AL
    2891: CodIBGE := 2708956; // Senador Rui Palmeira/AL
    2879: CodIBGE := 2709004; // Tanque D Arca/AL
    2881: CodIBGE := 2709103; // Taquarana/AL
    0971: CodIBGE := 2709152; // Teotonio Vilela/AL
    2883: CodIBGE := 2709202; // Traipu/AL
    2885: CodIBGE := 2709301; // Uniao Dos Palmares/AL
    2887: CodIBGE := 2709400; // Vicosa/AL
    3101: CodIBGE := 2800100; // Amparo De Sao Francisco/SE
    3103: CodIBGE := 2800209; // Aquidaba/SE
    3105: CodIBGE := 2800308; // Aracaju/SE
    3107: CodIBGE := 2800407; // Araua/SE
    3109: CodIBGE := 2800506; // Areia Branca/SE
    3111: CodIBGE := 2800605; // Barra Dos Coqueiros/SE
    3115: CodIBGE := 2800670; // Boquim/SE
    3113: CodIBGE := 2800704; // Brejo Grande/SE
    3119: CodIBGE := 2801009; // Campo Do Brito/SE
    3121: CodIBGE := 2801108; // Canhoba/SE
    3123: CodIBGE := 2801207; // Caninde De Sao Francisco/SE
    3125: CodIBGE := 2801306; // Capela/SE
    3127: CodIBGE := 2801405; // Carira/SE
    3129: CodIBGE := 2801504; // Carmopolis/SE
    3131: CodIBGE := 2801603; // Cedro De Sao Joao/SE
    3133: CodIBGE := 2801702; // Cristinapolis/SE
    3137: CodIBGE := 2801900; // Cumbe/SE
    3139: CodIBGE := 2802007; // Divina Pastora/SE
    3141: CodIBGE := 2802106; // Estancia/SE
    3143: CodIBGE := 2802205; // Feira Nova/SE
    3145: CodIBGE := 2802304; // Frei Paulo/SE
    3149: CodIBGE := 2802403; // Gararu/SE
    3147: CodIBGE := 2802502; // General Maynard/SE
    3151: CodIBGE := 2802601; // Gracho Cardoso/SE
    3153: CodIBGE := 2802700; // Ilha Das Flores/SE
    3155: CodIBGE := 2802809; // Indiaroba/SE
    3157: CodIBGE := 2802908; // Itabaiana/SE
    3159: CodIBGE := 2803005; // Itabaianinha/SE
    3161: CodIBGE := 2803104; // Itabi/SE
    3163: CodIBGE := 2803203; // Itaporanga D Ajuda/SE
    3165: CodIBGE := 2803302; // Japaratuba/SE
    3167: CodIBGE := 2803401; // Japoata/SE
    3169: CodIBGE := 2803500; // Lagarto/SE
    3171: CodIBGE := 2803609; // Laranjeiras/SE
    3173: CodIBGE := 2803708; // Macambira/SE
    3175: CodIBGE := 2803807; // Malhada Dos Bois/SE
    3177: CodIBGE := 2803906; // Malhador/SE
    3179: CodIBGE := 2804003; // Maruim/SE
    3181: CodIBGE := 2804102; // Moita Bonita/SE
    3183: CodIBGE := 2804201; // Monte Alegre De Sergipe/SE
    3185: CodIBGE := 2804300; // Muribeca/SE
    3187: CodIBGE := 2804409; // Neopolis/SE
    3135: CodIBGE := 2804458; // Nossa Senhora Aparecida/SE
    3189: CodIBGE := 2804508; // Nossa Senhora Da Gloria/SE
    3191: CodIBGE := 2804607; // Nossa Senhora Das Dores/SE
    3193: CodIBGE := 2804706; // Nossa Senhora De Lourdes/SE
    3195: CodIBGE := 2804805; // Nossa Senhora Do Socorro/SE
    3197: CodIBGE := 2804904; // Pacatuba/SE
    3199: CodIBGE := 2805000; // Pedra Mole/SE
    3201: CodIBGE := 2805109; // Pedrinhas/SE
    3203: CodIBGE := 2805208; // Pinhao/SE
    3205: CodIBGE := 2805307; // Pirambu/SE
    3207: CodIBGE := 2805406; // Poco Redondo/SE
    3209: CodIBGE := 2805505; // Poco Verde/SE
    3211: CodIBGE := 2805604; // Porto Da Folha/SE
    3213: CodIBGE := 2805703; // Propria/SE
    3215: CodIBGE := 2805802; // Riachao Do Dantas/SE
    3217: CodIBGE := 2805901; // Riachuelo/SE
    3219: CodIBGE := 2806008; // Ribeiropolis/SE
    3221: CodIBGE := 2806107; // Rosario Do Catete/SE
    3223: CodIBGE := 2806206; // Salgado/SE
    3225: CodIBGE := 2806305; // Santa Luzia Do Itanhy/SE
    2647: CodIBGE := 2806404; // Santana Do Sao Francisco/SE
    3229: CodIBGE := 2806503; // Santa Rosa De Lima/SE
    3231: CodIBGE := 2806602; // Santo Amaro Das Brotas/SE
    3233: CodIBGE := 2806701; // Sao Cristovao/SE
    3235: CodIBGE := 2806800; // Sao Domingos/SE
    3237: CodIBGE := 2806909; // Sao Francisco/SE
    3239: CodIBGE := 2807006; // Sao Miguel Do Aleixo/SE
    3241: CodIBGE := 2807105; // Simao Dias/SE
    3243: CodIBGE := 2807204; // Siriri/SE
    3245: CodIBGE := 2807303; // Telha/SE
    3247: CodIBGE := 2807402; // Tobias Barreto/SE
    3249: CodIBGE := 2807501; // Tomar Do Geru/SE
    3251: CodIBGE := 2807600; // Umbauba/SE
    3301: CodIBGE := 2900108; // Abaira/BA
    3303: CodIBGE := 2900207; // Abare/BA
    3305: CodIBGE := 2900306; // Acajutiba/BA
    3253: CodIBGE := 2900355; // Adustina/BA
    3307: CodIBGE := 2900405; // Agua Fria/BA
    3309: CodIBGE := 2900504; // Erico Cardoso/BA
    3311: CodIBGE := 2900603; // Aiquara/BA
    3313: CodIBGE := 2900702; // Alagoinhas/BA
    3315: CodIBGE := 2900801; // Alcobaca/BA
    3317: CodIBGE := 2900900; // Almadina/BA
    3319: CodIBGE := 2901007; // Amargosa/BA
    3321: CodIBGE := 2901106; // Amelia Rodrigues/BA
    3071: CodIBGE := 2901155; // America Dourada/BA
    3323: CodIBGE := 2901205; // Anage/BA
    3325: CodIBGE := 2901304; // Andarai/BA
    3255: CodIBGE := 2901353; // Andorinha/BA
    3327: CodIBGE := 2901403; // Angical/BA
    3329: CodIBGE := 2901502; // Anguera/BA
    3331: CodIBGE := 2901601; // Antas/BA
    3333: CodIBGE := 2901700; // Antonio Cardoso/BA
    3335: CodIBGE := 2901809; // Antonio Goncalves/BA
    3337: CodIBGE := 2901908; // Apora/BA
    3257: CodIBGE := 2901957; // Apuarema/BA
    3339: CodIBGE := 2902005; // Aracatu/BA
    3259: CodIBGE := 2902054; // Aracas/BA
    3341: CodIBGE := 2902104; // Araci/BA
    3343: CodIBGE := 2902203; // Aramari/BA
    3073: CodIBGE := 2902252; // Arataca/BA
    3345: CodIBGE := 2902302; // Aratuipe/BA
    3347: CodIBGE := 2902401; // Aurelino Leal/BA
    3349: CodIBGE := 2902500; // Baianopolis/BA
    3351: CodIBGE := 2902609; // Baixa Grande/BA
    3261: CodIBGE := 2902658; // Banzae/BA
    3353: CodIBGE := 2902708; // Barra/BA
    3355: CodIBGE := 2902807; // Barra Da Estiva/BA
    3357: CodIBGE := 2902906; // Barra Do Choca/BA
    3359: CodIBGE := 2903003; // Barra Do Mendes/BA
    3361: CodIBGE := 2903102; // Barra Do Rocha/BA
    3363: CodIBGE := 2903201; // Barreiras/BA
    3075: CodIBGE := 2903235; // Barro Alto/BA
    3077: CodIBGE := 2903276; // Barrocas/BA
    3365: CodIBGE := 2903300; // Barro Preto/BA
    3367: CodIBGE := 2903409; // Belmonte/BA
    3369: CodIBGE := 2903508; // Belo Campo/BA
    3371: CodIBGE := 2903607; // Biritinga/BA
    3373: CodIBGE := 2903706; // Boa Nova/BA
    3375: CodIBGE := 2903805; // Boa Vista Do Tupim/BA
    3377: CodIBGE := 2903904; // Bom Jesus Da Lapa/BA
    3263: CodIBGE := 2903953; // Bom Jesus Da Serra/BA
    3379: CodIBGE := 2904001; // Boninal/BA
    3265: CodIBGE := 2904050; // Bonito/BA
    3381: CodIBGE := 2904100; // Boquira/BA
    3383: CodIBGE := 2904209; // Botupora/BA
    3385: CodIBGE := 2904308; // Brejoes/BA
    3387: CodIBGE := 2904407; // Brejolandia/BA
    3389: CodIBGE := 2904506; // Brotas De Macaubas/BA
    3391: CodIBGE := 2904605; // Brumado/BA
    3393: CodIBGE := 2904704; // Buerarema/BA
    3079: CodIBGE := 2904753; // Buritirama/BA
    3395: CodIBGE := 2904803; // Caatiba/BA
    3267: CodIBGE := 2904852; // Cabaceiras Do Paraguacu/BA
    3397: CodIBGE := 2904902; // Cachoeira/BA
    3399: CodIBGE := 2905008; // Cacule/BA
    3401: CodIBGE := 2905107; // Caem/BA
    3269: CodIBGE := 2905156; // Caetanos/BA
    3403: CodIBGE := 2905206; // Caetite/BA
    3405: CodIBGE := 2905305; // Cafarnaum/BA
    3407: CodIBGE := 2905404; // Cairu/BA
    3409: CodIBGE := 2905503; // Caldeirao Grande/BA
    3411: CodIBGE := 2905602; // Camacan/BA
    3413: CodIBGE := 2905701; // Camacari/BA
    3415: CodIBGE := 2905800; // Camamu/BA
    3417: CodIBGE := 2905909; // Campo Alegre De Lourdes/BA
    3419: CodIBGE := 2906006; // Campo Formoso/BA
    3421: CodIBGE := 2906105; // Canapolis/BA
    3423: CodIBGE := 2906204; // Canarana/BA
    3425: CodIBGE := 2906303; // Canavieiras/BA
    3427: CodIBGE := 2906402; // Candeal/BA
    3429: CodIBGE := 2906501; // Candeias/BA
    3431: CodIBGE := 2906600; // Candiba/BA
    3433: CodIBGE := 2906709; // Candido Sales/BA
    3435: CodIBGE := 2906808; // Cansancao/BA
    3085: CodIBGE := 2906824; // Canudos/BA
    3081: CodIBGE := 2906857; // Capela Do Alto Alegre/BA
    3083: CodIBGE := 2906873; // Capim Grosso/BA
    3271: CodIBGE := 2906899; // Caraibas/BA
    3437: CodIBGE := 2906907; // Caravelas/BA
    3439: CodIBGE := 2907004; // Cardeal Da Silva/BA
    3441: CodIBGE := 2907103; // Carinhanha/BA
    3443: CodIBGE := 2907202; // Casa Nova/BA
    3445: CodIBGE := 2907301; // Castro Alves/BA
    3447: CodIBGE := 2907400; // Catolandia/BA
    3449: CodIBGE := 2907509; // Catu/BA
    3273: CodIBGE := 2907558; // Caturama/BA
    3451: CodIBGE := 2907608; // Central/BA
    3453: CodIBGE := 2907707; // Chorrocho/BA
    3455: CodIBGE := 2907806; // Cicero Dantas/BA
    3457: CodIBGE := 2907905; // Cipo/BA
    3459: CodIBGE := 2908002; // Coaraci/BA
    3461: CodIBGE := 2908101; // Cocos/BA
    3463: CodIBGE := 2908200; // Conceicao Da Feira/BA
    3465: CodIBGE := 2908309; // Conceicao Do Almeida/BA
    3467: CodIBGE := 2908408; // Conceicao Do Coite/BA
    3469: CodIBGE := 2908507; // Conceicao Do Jacuipe/BA
    3471: CodIBGE := 2908606; // Conde/BA
    3473: CodIBGE := 2908705; // Condeuba/BA
    3475: CodIBGE := 2908804; // Contendas Do Sincora/BA
    3477: CodIBGE := 2908903; // Coracao De Maria/BA
    3479: CodIBGE := 2909000; // Cordeiros/BA
    3481: CodIBGE := 2909109; // Coribe/BA
    3483: CodIBGE := 2909208; // Coronel Joao Sa/BA
    3485: CodIBGE := 2909307; // Correntina/BA
    3487: CodIBGE := 2909406; // Cotegipe/BA
    3489: CodIBGE := 2909505; // Cravolandia/BA
    3491: CodIBGE := 2909604; // Crisopolis/BA
    3493: CodIBGE := 2909703; // Cristopolis/BA
    3495: CodIBGE := 2909802; // Cruz Das Almas/BA
    3497: CodIBGE := 2909901; // Curaca/BA
    3499: CodIBGE := 2910008; // Dario Meira/BA
    3087: CodIBGE := 2910057; // Dias D Avila/BA
    3501: CodIBGE := 2910107; // Dom Basilio/BA
    3503: CodIBGE := 2910206; // Dom Macedo Costa/BA
    3505: CodIBGE := 2910305; // Elisio Medrado/BA
    3507: CodIBGE := 2910404; // Encruzilhada/BA
    3509: CodIBGE := 2910503; // Entre Rios/BA
    3511: CodIBGE := 2910602; // Esplanada/BA
    3513: CodIBGE := 2910701; // Euclides Da Cunha/BA
    3117: CodIBGE := 2910727; // Eunapolis/BA
    3089: CodIBGE := 2910750; // Fatima/BA
    3275: CodIBGE := 2910776; // Feira Da Mata/BA
    3515: CodIBGE := 2910800; // Feira De Santana/BA
    3091: CodIBGE := 2910859; // Filadelfia/BA
    3517: CodIBGE := 2910909; // Firmino Alves/BA
    3519: CodIBGE := 2911006; // Floresta Azul/BA
    3521: CodIBGE := 2911105; // Formosa Do Rio Preto/BA
    3523: CodIBGE := 2911204; // Gandu/BA
    3093: CodIBGE := 2911253; // Gaviao/BA
    3525: CodIBGE := 2911303; // Gentio Do Ouro/BA
    3527: CodIBGE := 2911402; // Gloria/BA
    3529: CodIBGE := 2911501; // Gongogi/BA
    3531: CodIBGE := 2911600; // Governador Mangabeira/BA
    3095: CodIBGE := 2911659; // Guajeru/BA
    3533: CodIBGE := 2911709; // Guanambi/BA
    3535: CodIBGE := 2911808; // Guaratinga/BA
    3097: CodIBGE := 2911857; // Heliopolis/BA
    3537: CodIBGE := 2911907; // Iacu/BA
    3539: CodIBGE := 2912004; // Ibiassuce/BA
    3541: CodIBGE := 2912103; // Ibicarai/BA
    3543: CodIBGE := 2912202; // Ibicoara/BA
    3545: CodIBGE := 2912301; // Ibicui/BA
    3547: CodIBGE := 2912400; // Ibipeba/BA
    3551: CodIBGE := 2912509; // Ibipitanga/BA
    3553: CodIBGE := 2912608; // Ibiquera/BA
    3555: CodIBGE := 2912707; // Ibirapitanga/BA
    3557: CodIBGE := 2912806; // Ibirapua/BA
    3559: CodIBGE := 2912905; // Ibirataia/BA
    3561: CodIBGE := 2913002; // Ibitiara/BA
    3563: CodIBGE := 2913101; // Ibitita/BA
    3565: CodIBGE := 2913200; // Ibotirama/BA
    3567: CodIBGE := 2913309; // Ichu/BA
    3569: CodIBGE := 2913408; // Igapora/BA
    3277: CodIBGE := 2913457; // Igrapiuna/BA
    3571: CodIBGE := 2913507; // Iguai/BA
    3573: CodIBGE := 2913606; // Ilheus/BA
    3575: CodIBGE := 2913705; // Inhambupe/BA
    3577: CodIBGE := 2913804; // Ipecaeta/BA
    3579: CodIBGE := 2913903; // Ipiau/BA
    3581: CodIBGE := 2914000; // Ipira/BA
    3583: CodIBGE := 2914109; // Ipupiara/BA
    3585: CodIBGE := 2914208; // Irajuba/BA
    3587: CodIBGE := 2914307; // Iramaia/BA
    3589: CodIBGE := 2914406; // Iraquara/BA
    3591: CodIBGE := 2914505; // Irara/BA
    3593: CodIBGE := 2914604; // Irece/BA
    3279: CodIBGE := 2914653; // Itabela/BA
    3595: CodIBGE := 2914703; // Itaberaba/BA
    3597: CodIBGE := 2914802; // Itabuna/BA
    3599: CodIBGE := 2914901; // Itacare/BA
    3601: CodIBGE := 2915007; // Itaete/BA
    3603: CodIBGE := 2915106; // Itagi/BA
    3605: CodIBGE := 2915205; // Itagiba/BA
    3607: CodIBGE := 2915304; // Itagimirim/BA
    3281: CodIBGE := 2915353; // Itaguacu Da Bahia/BA
    3609: CodIBGE := 2915403; // Itaju Do Colonia/BA
    3611: CodIBGE := 2915502; // Itajuipe/BA
    3613: CodIBGE := 2915601; // Itamaraju/BA
    3615: CodIBGE := 2915700; // Itamari/BA
    3617: CodIBGE := 2915809; // Itambe/BA
    3619: CodIBGE := 2915908; // Itanagra/BA
    3621: CodIBGE := 2916005; // Itanhem/BA
    3623: CodIBGE := 2916104; // Itaparica/BA
    3625: CodIBGE := 2916203; // Itape/BA
    3627: CodIBGE := 2916302; // Itapebi/BA
    3629: CodIBGE := 2916401; // Itapetinga/BA
    3631: CodIBGE := 2916500; // Itapicuru/BA
    3633: CodIBGE := 2916609; // Itapitanga/BA
    3635: CodIBGE := 2916708; // Itaquara/BA
    3637: CodIBGE := 2916807; // Itarantim/BA
    3283: CodIBGE := 2916856; // Itatim/BA
    3639: CodIBGE := 2916906; // Itirucu/BA
    3641: CodIBGE := 2917003; // Itiuba/BA
    3643: CodIBGE := 2917102; // Itororo/BA
    3645: CodIBGE := 2917201; // Ituacu/BA
    3647: CodIBGE := 2917300; // Itubera/BA
    3285: CodIBGE := 2917334; // Iuiu/BA
    9859: CodIBGE := 2917359; // Jaborandi/BA
    3649: CodIBGE := 2917409; // Jacaraci/BA
    3651: CodIBGE := 2917508; // Jacobina/BA
    3653: CodIBGE := 2917607; // Jaguaquara/BA
    3655: CodIBGE := 2917706; // Jaguarari/BA
    3657: CodIBGE := 2917805; // Jaguaripe/BA
    3659: CodIBGE := 2917904; // Jandaira/BA
    3661: CodIBGE := 2918001; // Jequie/BA
    3663: CodIBGE := 2918100; // Jeremoabo/BA
    3665: CodIBGE := 2918209; // Jiquirica/BA
    3667: CodIBGE := 2918308; // Jitauna/BA
    3099: CodIBGE := 2918357; // Joao Dourado/BA
    3669: CodIBGE := 2918407; // Juazeiro/BA
    3287: CodIBGE := 2918456; // Jucurucu/BA
    3671: CodIBGE := 2918506; // Jussara/BA
    3069: CodIBGE := 2918555; // Jussari/BA
    3673: CodIBGE := 2918605; // Jussiape/BA
    3675: CodIBGE := 2918704; // Lafaiete Coutinho/BA
    3289: CodIBGE := 2918753; // Lagoa Real/BA
    3677: CodIBGE := 2918803; // Laje/BA
    3679: CodIBGE := 2918902; // Lajedao/BA
    3681: CodIBGE := 2919009; // Lajedinho/BA
    3291: CodIBGE := 2919058; // Lajedo Do Tabocal/BA
    3683: CodIBGE := 2919108; // Lamarao/BA
    3973: CodIBGE := 2919157; // Lapao/BA
    3685: CodIBGE := 2919207; // Lauro De Freitas/BA
    3687: CodIBGE := 2919306; // Lencois/BA
    3689: CodIBGE := 2919405; // Licinio De Almeida/BA
    3691: CodIBGE := 2919504; // Livramento De Nossa Senhora/BA
    1112: CodIBGE := 2919553; // Luis Eduardo Magalhaes/BA
    3693: CodIBGE := 2919603; // Macajuba/BA
    3695: CodIBGE := 2919702; // Macarani/BA
    3697: CodIBGE := 2919801; // Macaubas/BA
    3699: CodIBGE := 2919900; // Macurure/BA
    3293: CodIBGE := 2919926; // Madre De Deus/BA
    3975: CodIBGE := 2919959; // Maetinga/BA
    3701: CodIBGE := 2920007; // Maiquinique/BA
    3703: CodIBGE := 2920106; // Mairi/BA
    3705: CodIBGE := 2920205; // Malhada/BA
    3707: CodIBGE := 2920304; // Malhada De Pedras/BA
    3709: CodIBGE := 2920403; // Manoel Vitorino/BA
    3977: CodIBGE := 2920452; // Mansidao/BA
    3711: CodIBGE := 2920502; // Maracas/BA
    3713: CodIBGE := 2920601; // Maragogipe/BA
    3715: CodIBGE := 2920700; // Marau/BA
    3717: CodIBGE := 2920809; // Marcionilio Souza/BA
    3719: CodIBGE := 2920908; // Mascote/BA
    3721: CodIBGE := 2921005; // Mata De Sao Joao/BA
    3295: CodIBGE := 2921054; // Matina/BA
    3723: CodIBGE := 2921104; // Medeiros Neto/BA
    3725: CodIBGE := 2921203; // Miguel Calmon/BA
    3727: CodIBGE := 2921302; // Milagres/BA
    3729: CodIBGE := 2921401; // Mirangaba/BA
    3297: CodIBGE := 2921450; // Mirante/BA
    3731: CodIBGE := 2921500; // Monte Santo/BA
    3733: CodIBGE := 2921609; // Morpara/BA
    3735: CodIBGE := 2921708; // Morro Do Chapeu/BA
    3737: CodIBGE := 2921807; // Mortugaba/BA
    3739: CodIBGE := 2921906; // Mucuge/BA
    3741: CodIBGE := 2922003; // Mucuri/BA
    3299: CodIBGE := 2922052; // Mulungu Do Morro/BA
    3743: CodIBGE := 2922102; // Mundo Novo/BA
    3745: CodIBGE := 2922201; // Muniz Ferreira/BA
    3005: CodIBGE := 2922250; // Muquem De Sao Francisco/BA
    3747: CodIBGE := 2922300; // Muritiba/BA
    3749: CodIBGE := 2922409; // Mutuipe/BA
    3751: CodIBGE := 2922508; // Nazare/BA
    3753: CodIBGE := 2922607; // Nilo Pecanha/BA
    3979: CodIBGE := 2922656; // Nordestina/BA
    3755: CodIBGE := 2922706; // Nova Canaa/BA
    3007: CodIBGE := 2922730; // Nova Fatima/BA
    3009: CodIBGE := 2922755; // Nova Ibia/BA
    3757: CodIBGE := 2922805; // Nova Itarana/BA
    3011: CodIBGE := 2922854; // Nova Redencao/BA
    3759: CodIBGE := 2922904; // Nova Soure/BA
    3761: CodIBGE := 2923001; // Nova Vicosa/BA
    3013: CodIBGE := 2923035; // Novo Horizonte/BA
    3015: CodIBGE := 2923050; // Novo Triunfo/BA
    3763: CodIBGE := 2923100; // Olindina/BA
    3765: CodIBGE := 2923209; // Oliveira Dos Brejinhos/BA
    3767: CodIBGE := 2923308; // Ouricangas/BA
    3017: CodIBGE := 2923357; // Ourolandia/BA
    3769: CodIBGE := 2923407; // Palmas De Monte Alto/BA
    3771: CodIBGE := 2923506; // Palmeiras/BA
    3773: CodIBGE := 2923605; // Paramirim/BA
    3775: CodIBGE := 2923704; // Paratinga/BA
    3777: CodIBGE := 2923803; // Paripiranga/BA
    3779: CodIBGE := 2923902; // Pau Brasil/BA
    3781: CodIBGE := 2924009; // Paulo Afonso/BA
    3981: CodIBGE := 2924058; // Pe De Serra/BA
    3783: CodIBGE := 2924108; // Pedrao/BA
    3785: CodIBGE := 2924207; // Pedro Alexandre/BA
    3787: CodIBGE := 2924306; // Piata/BA
    3789: CodIBGE := 2924405; // Pilao Arcado/BA
    3791: CodIBGE := 2924504; // Pindai/BA
    3793: CodIBGE := 2924603; // Pindobacu/BA
    3983: CodIBGE := 2924652; // Pintadas/BA
    3019: CodIBGE := 2924678; // Pirai Do Norte/BA
    3795: CodIBGE := 2924702; // Piripa/BA
    3797: CodIBGE := 2924801; // Piritiba/BA
    3799: CodIBGE := 2924900; // Planaltino/BA
    3801: CodIBGE := 2925006; // Planalto/BA
    3803: CodIBGE := 2925105; // Pocoes/BA
    3805: CodIBGE := 2925204; // Pojuca/BA
    3021: CodIBGE := 2925253; // Ponto Novo/BA
    3807: CodIBGE := 2925303; // Porto Seguro/BA
    3809: CodIBGE := 2925402; // Potiragua/BA
    3811: CodIBGE := 2925501; // Prado/BA
    3813: CodIBGE := 2925600; // Presidente Dutra/BA
    3815: CodIBGE := 2925709; // Presidente Janio Quadros/BA
    3023: CodIBGE := 2925758; // Presidente Tancredo Neves/BA
    3817: CodIBGE := 2925808; // Queimadas/BA
    3819: CodIBGE := 2925907; // Quijingue/BA
    3025: CodIBGE := 2925931; // Quixabeira/BA
    3985: CodIBGE := 2925956; // Rafael Jambeiro/BA
    3821: CodIBGE := 2926004; // Remanso/BA
    3823: CodIBGE := 2926103; // Retirolandia/BA
    3825: CodIBGE := 2926202; // Riachao Das Neves/BA
    3827: CodIBGE := 2926301; // Riachao Do Jacuipe/BA
    3829: CodIBGE := 2926400; // Riacho De Santana/BA
    3831: CodIBGE := 2926509; // Ribeira Do Amparo/BA
    3833: CodIBGE := 2926608; // Ribeira Do Pombal/BA
    3027: CodIBGE := 2926657; // Ribeirao Do Largo/BA
    3835: CodIBGE := 2926707; // Rio De Contas/BA
    3837: CodIBGE := 2926806; // Rio Do Antonio/BA
    3839: CodIBGE := 2926905; // Rio Do Pires/BA
    3841: CodIBGE := 2927002; // Rio Real/BA
    3843: CodIBGE := 2927101; // Rodelas/BA
    3845: CodIBGE := 2927200; // Ruy Barbosa/BA
    3847: CodIBGE := 2927309; // Salinas Da Margarida/BA
    3849: CodIBGE := 2927408; // Salvador/BA
    3851: CodIBGE := 2927507; // Santa Barbara/BA
    3853: CodIBGE := 2927606; // Santa Brigida/BA
    3855: CodIBGE := 2927705; // Santa Cruz Cabralia/BA
    3857: CodIBGE := 2927804; // Santa Cruz Da Vitoria/BA
    3859: CodIBGE := 2927903; // Santa Ines/BA
    3861: CodIBGE := 2928000; // Santaluz/BA
    3987: CodIBGE := 2928059; // Santa Luzia/BA
    3863: CodIBGE := 2928109; // Santa Maria Da Vitoria/BA
    3865: CodIBGE := 2928208; // Santana/BA
    3867: CodIBGE := 2928307; // Santanopolis/BA
    3549: CodIBGE := 2928406; // Santa Rita De Cassia/BA
    3869: CodIBGE := 2928505; // Santa Teresinha/BA
    3871: CodIBGE := 2928604; // Santo Amaro/BA
    3873: CodIBGE := 2928703; // Santo Antonio De Jesus/BA
    3875: CodIBGE := 2928802; // Santo Estevao/BA
    3877: CodIBGE := 2928901; // Sao Desiderio/BA
    3029: CodIBGE := 2928950; // Sao Domingos/BA
    3879: CodIBGE := 2929008; // Sao Felix/BA
    3031: CodIBGE := 2929057; // Sao Felix Do Coribe/BA
    3881: CodIBGE := 2929107; // Sao Felipe/BA
    3883: CodIBGE := 2929206; // Sao Francisco Do Conde/BA
    3989: CodIBGE := 2929255; // Sao Gabriel/BA
    3885: CodIBGE := 2929305; // Sao Goncalo Dos Campos/BA
    3035: CodIBGE := 2929354; // Sao Jose Da Vitoria/BA
    3033: CodIBGE := 2929370; // Sao Jose Do Jacuipe/BA
    3887: CodIBGE := 2929404; // Sao Miguel Das Matas/BA
    3889: CodIBGE := 2929503; // Sao Sebastiao Do Passe/BA
    3891: CodIBGE := 2929602; // Sapeacu/BA
    3893: CodIBGE := 2929701; // Satiro Dias/BA
    3037: CodIBGE := 2929750; // Saubara/BA
    3895: CodIBGE := 2929800; // Saude/BA
    3897: CodIBGE := 2929909; // Seabra/BA
    3899: CodIBGE := 2930006; // Sebastiao Laranjeiras/BA
    3901: CodIBGE := 2930105; // Senhor Do Bonfim/BA
    3039: CodIBGE := 2930154; // Serra Do Ramalho/BA
    3903: CodIBGE := 2930204; // Sento Se/BA
    3905: CodIBGE := 2930303; // Serra Dourada/BA
    3907: CodIBGE := 2930402; // Serra Preta/BA
    3909: CodIBGE := 2930501; // Serrinha/BA
    3911: CodIBGE := 2930600; // Serrolandia/BA
    3913: CodIBGE := 2930709; // Simoes Filho/BA
    3041: CodIBGE := 2930758; // Sitio Do Mato/BA
    3043: CodIBGE := 2930766; // Sitio Do Quinto/BA
    3045: CodIBGE := 2930774; // Sobradinho/BA
    3915: CodIBGE := 2930808; // Souto Soares/BA
    3917: CodIBGE := 2930907; // Tabocas Do Brejo Velho/BA
    3919: CodIBGE := 2931004; // Tanhacu/BA
    3991: CodIBGE := 2931053; // Tanque Novo/BA
    3921: CodIBGE := 2931103; // Tanquinho/BA
    3923: CodIBGE := 2931202; // Taperoa/BA
    3925: CodIBGE := 2931301; // Tapiramuta/BA
    3993: CodIBGE := 2931350; // Teixeira De Freitas/BA
    3927: CodIBGE := 2931400; // Teodoro Sampaio/BA
    3929: CodIBGE := 2931509; // Teofilandia/BA
    3931: CodIBGE := 2931608; // Teolandia/BA
    3933: CodIBGE := 2931707; // Terra Nova/BA
    3935: CodIBGE := 2931806; // Tremedal/BA
    3937: CodIBGE := 2931905; // Tucano/BA
    3939: CodIBGE := 2932002; // Uaua/BA
    3941: CodIBGE := 2932101; // Ubaira/BA
    3943: CodIBGE := 2932200; // Ubaitaba/BA
    3945: CodIBGE := 2932309; // Ubata/BA
    3947: CodIBGE := 2932408; // Uibai/BA
    3047: CodIBGE := 2932457; // Umburanas/BA
    3949: CodIBGE := 2932507; // Una/BA
    3951: CodIBGE := 2932606; // Urandi/BA
    3953: CodIBGE := 2932705; // Urucuca/BA
    3955: CodIBGE := 2932804; // Utinga/BA
    3957: CodIBGE := 2932903; // Valenca/BA
    3959: CodIBGE := 2933000; // Valente/BA
    3997: CodIBGE := 2933059; // Varzea Da Roca/BA
    3961: CodIBGE := 2933109; // Varzea Do Poco/BA
    3995: CodIBGE := 2933158; // Varzea Nova/BA
    3049: CodIBGE := 2933174; // Varzedo/BA
    3963: CodIBGE := 2933208; // Vera Cruz/BA
    3051: CodIBGE := 2933257; // Vereda/BA
    3965: CodIBGE := 2933307; // Vitoria Da Conquista/BA
    3967: CodIBGE := 2933406; // Wagner/BA
    3999: CodIBGE := 2933455; // Wanderley/BA
    3969: CodIBGE := 2933505; // Wenceslau Guimaraes/BA
    3971: CodIBGE := 2933604; // Xique-Xique/BA
    4001: CodIBGE := 3100104; // Abadia Dos Dourados/MG
    4003: CodIBGE := 3100203; // Abaete/MG
    4005: CodIBGE := 3100302; // Abre Campo/MG
    4007: CodIBGE := 3100401; // Acaiaca/MG
    4009: CodIBGE := 3100500; // Acucena/MG
    4011: CodIBGE := 3100609; // Agua Boa/MG
    4013: CodIBGE := 3100708; // Agua Comprida/MG
    4015: CodIBGE := 3100807; // Aguanil/MG
    4017: CodIBGE := 3100906; // Aguas Formosas/MG
    4019: CodIBGE := 3101003; // Aguas Vermelhas/MG
    4021: CodIBGE := 3101102; // Aimores/MG
    4023: CodIBGE := 3101201; // Aiuruoca/MG
    4025: CodIBGE := 3101300; // Alagoa/MG
    4027: CodIBGE := 3101409; // Albertina/MG
    4029: CodIBGE := 3101508; // Alem Paraiba/MG
    4031: CodIBGE := 3101607; // Alfenas/MG
    2681: CodIBGE := 3101631; // Alfredo Vasconcelos/MG
    4033: CodIBGE := 3101706; // Almenara/MG
    4035: CodIBGE := 3101805; // Alpercata/MG
    4037: CodIBGE := 3101904; // Alpinopolis/MG
    4039: CodIBGE := 3102001; // Alterosa/MG
    0564: CodIBGE := 3102050; // Alto Caparao/MG
    4041: CodIBGE := 3102100; // Alto Rio Doce/MG
    4043: CodIBGE := 3102209; // Alvarenga/MG
    4045: CodIBGE := 3102308; // Alvinopolis/MG
    4047: CodIBGE := 3102407; // Alvorada De Minas/MG
    4049: CodIBGE := 3102506; // Amparo Do Serra/MG
    4051: CodIBGE := 3102605; // Andradas/MG
    4196: CodIBGE := 3102704; // Cachoeira De Pajeu/MG
    4055: CodIBGE := 3102803; // Andrelandia/MG
    0566: CodIBGE := 3102852; // Angelandia/MG
    4057: CodIBGE := 3102902; // Antonio Carlos/MG
    4059: CodIBGE := 3103009; // Antonio Dias/MG
    4061: CodIBGE := 3103108; // Antonio Prado De Minas/MG
    4063: CodIBGE := 3103207; // Aracai/MG
    4065: CodIBGE := 3103306; // Aracitaba/MG
    4067: CodIBGE := 3103405; // Aracuai/MG
    4069: CodIBGE := 3103504; // Araguari/MG
    4071: CodIBGE := 3103603; // Arantina/MG
    4073: CodIBGE := 3103702; // Araponga/MG
    2903: CodIBGE := 3103751; // Arapora/MG
    4075: CodIBGE := 3103801; // Arapua/MG
    4077: CodIBGE := 3103900; // Araujos/MG
    4079: CodIBGE := 3104007; // Araxa/MG
    4081: CodIBGE := 3104106; // Arceburgo/MG
    4083: CodIBGE := 3104205; // Arcos/MG
    4085: CodIBGE := 3104304; // Areado/MG
    4087: CodIBGE := 3104403; // Argirita/MG
    0568: CodIBGE := 3104452; // Aricanduva/MG
    4089: CodIBGE := 3104502; // Arinos/MG
    4091: CodIBGE := 3104601; // Astolfo Dutra/MG
    4093: CodIBGE := 3104700; // Ataleia/MG
    4095: CodIBGE := 3104809; // Augusto De Lima/MG
    4097: CodIBGE := 3104908; // Baependi/MG
    4099: CodIBGE := 3105004; // Baldim/MG
    4101: CodIBGE := 3105103; // Bambui/MG
    4103: CodIBGE := 3105202; // Bandeira/MG
    4105: CodIBGE := 3105301; // Bandeira Do Sul/MG
    4107: CodIBGE := 3105400; // Barao De Cocais/MG
    4109: CodIBGE := 3105509; // Barao De Monte Alto/MG
    4111: CodIBGE := 3105608; // Barbacena/MG
    4113: CodIBGE := 3105707; // Barra Longa/MG
    4117: CodIBGE := 3105905; // Barroso/MG
    4119: CodIBGE := 3106002; // Bela Vista De Minas/MG
    4121: CodIBGE := 3106101; // Belmiro Braga/MG
    4123: CodIBGE := 3106200; // Belo Horizonte/MG
    4125: CodIBGE := 3106309; // Belo Oriente/MG
    4127: CodIBGE := 3106408; // Belo Vale/MG
    4129: CodIBGE := 3106507; // Berilo/MG
    4131: CodIBGE := 3106606; // Bertopolis/MG
    0570: CodIBGE := 3106655; // Berizal/MG
    4133: CodIBGE := 3106705; // Betim/MG
    4135: CodIBGE := 3106804; // Bias Fortes/MG
    4137: CodIBGE := 3106903; // Bicas/MG
    4139: CodIBGE := 3107000; // Biquinhas/MG
    4141: CodIBGE := 3107109; // Boa Esperanca/MG
    4143: CodIBGE := 3107208; // Bocaina De Minas/MG
    4145: CodIBGE := 3107307; // Bocaiuva/MG
    4147: CodIBGE := 3107406; // Bom Despacho/MG
    4149: CodIBGE := 3107505; // Bom Jardim De Minas/MG
    4151: CodIBGE := 3107604; // Bom Jesus Da Penha/MG
    4153: CodIBGE := 3107703; // Bom Jesus Do Amparo/MG
    4155: CodIBGE := 3107802; // Bom Jesus Do Galho/MG
    4157: CodIBGE := 3107901; // Bom Repouso/MG
    4159: CodIBGE := 3108008; // Bom Sucesso/MG
    4161: CodIBGE := 3108107; // Bonfim/MG
    4163: CodIBGE := 3108206; // Bonfinopolis De Minas/MG
    0572: CodIBGE := 3108255; // Bonito De Minas/MG
    4165: CodIBGE := 3108305; // Borda Da Mata/MG
    4167: CodIBGE := 3108404; // Botelhos/MG
    4169: CodIBGE := 3108503; // Botumirim/MG
    0574: CodIBGE := 3108552; // Brasilandia De Minas/MG
    4171: CodIBGE := 3108602; // Brasilia De Minas/MG
    4173: CodIBGE := 3108701; // Bras Pires/MG
    4175: CodIBGE := 3108800; // Braunas/MG
    4177: CodIBGE := 3108909; // Brasopolis/MG
    4179: CodIBGE := 3109006; // Brumadinho/MG
    4181: CodIBGE := 3109105; // Bueno Brandao/MG
    4183: CodIBGE := 3109204; // Buenopolis/MG
    0576: CodIBGE := 3109253; // Bugre/MG
    4185: CodIBGE := 3109303; // Buritis/MG
    4187: CodIBGE := 3109402; // Buritizeiro/MG
    0578: CodIBGE := 3109451; // Cabeceira Grande/MG
    4189: CodIBGE := 3109501; // Cabo Verde/MG
    4191: CodIBGE := 3109600; // Cachoeira Da Prata/MG
    4193: CodIBGE := 3109709; // Cachoeira De Minas/MG
    4195: CodIBGE := 3109808; // Cachoeira Dourada/MG
    4197: CodIBGE := 3109907; // Caetanopolis/MG
    4199: CodIBGE := 3110004; // Caete/MG
    4201: CodIBGE := 3110103; // Caiana/MG
    4203: CodIBGE := 3110202; // Cajuri/MG
    4205: CodIBGE := 3110301; // Caldas/MG
    4207: CodIBGE := 3110400; // Camacho/MG
    4209: CodIBGE := 3110509; // Camanducaia/MG
    4211: CodIBGE := 3110608; // Cambui/MG
    4213: CodIBGE := 3110707; // Cambuquira/MG
    4215: CodIBGE := 3110806; // Campanario/MG
    4217: CodIBGE := 3110905; // Campanha/MG
    4219: CodIBGE := 3111002; // Campestre/MG
    4221: CodIBGE := 3111101; // Campina Verde/MG
    0580: CodIBGE := 3111150; // Campo Azul/MG
    4223: CodIBGE := 3111200; // Campo Belo/MG
    4225: CodIBGE := 3111309; // Campo Do Meio/MG
    4227: CodIBGE := 3111408; // Campo Florido/MG
    4229: CodIBGE := 3111507; // Campos Altos/MG
    4231: CodIBGE := 3111606; // Campos Gerais/MG
    4233: CodIBGE := 3111705; // Canaa/MG
    4235: CodIBGE := 3111804; // Canapolis/MG
    4237: CodIBGE := 3111903; // Cana Verde/MG
    4239: CodIBGE := 3112000; // Candeias/MG
    0582: CodIBGE := 3112059; // Cantagalo/MG
    4241: CodIBGE := 3112109; // Caparao/MG
    4243: CodIBGE := 3112208; // Capela Nova/MG
    4245: CodIBGE := 3112307; // Capelinha/MG
    4247: CodIBGE := 3112406; // Capetinga/MG
    4249: CodIBGE := 3112505; // Capim Branco/MG
    4251: CodIBGE := 3112604; // Capinopolis/MG
    2651: CodIBGE := 3112653; // Capitao Andrade/MG
    4253: CodIBGE := 3112703; // Capitao Eneas/MG
    4255: CodIBGE := 3112802; // Capitolio/MG
    4257: CodIBGE := 3112901; // Caputira/MG
    4259: CodIBGE := 3113008; // Carai/MG
    4261: CodIBGE := 3113107; // Caranaiba/MG
    4263: CodIBGE := 3113206; // Carandai/MG
    4265: CodIBGE := 3113305; // Carangola/MG
    4267: CodIBGE := 3113404; // Caratinga/MG
    4269: CodIBGE := 3113503; // Carbonita/MG
    4271: CodIBGE := 3113602; // Careacu/MG
    4273: CodIBGE := 3113701; // Carlos Chagas/MG
    4275: CodIBGE := 3113800; // Carmesia/MG
    4277: CodIBGE := 3113909; // Carmo Da Cachoeira/MG
    4279: CodIBGE := 3114006; // Carmo Da Mata/MG
    4281: CodIBGE := 3114105; // Carmo De Minas/MG
    4283: CodIBGE := 3114204; // Carmo Do Cajuru/MG
    4285: CodIBGE := 3114303; // Carmo Do Paranaiba/MG
    4287: CodIBGE := 3114402; // Carmo Do Rio Claro/MG
    4289: CodIBGE := 3114501; // Carmopolis De Minas/MG
    2685: CodIBGE := 3114550; // Carneirinho/MG
    4291: CodIBGE := 3114600; // Carrancas/MG
    4293: CodIBGE := 3114709; // Carvalhopolis/MG
    4295: CodIBGE := 3114808; // Carvalhos/MG
    4297: CodIBGE := 3114907; // Casa Grande/MG
    4299: CodIBGE := 3115003; // Cascalho Rico/MG
    4301: CodIBGE := 3115102; // Cassia/MG
    4303: CodIBGE := 3115201; // Conceicao Da Barra De Minas/MG
    4305: CodIBGE := 3115300; // Cataguases/MG
    0584: CodIBGE := 3115359; // Catas Altas/MG
    4307: CodIBGE := 3115409; // Catas Altas Da Noruega/MG
    2653: CodIBGE := 3115458; // Catuji/MG
    0586: CodIBGE := 3115474; // Catuti/MG
    4309: CodIBGE := 3115508; // Caxambu/MG
    4311: CodIBGE := 3115607; // Cedro Do Abaete/MG
    4313: CodIBGE := 3115706; // Central De Minas/MG
    4315: CodIBGE := 3115805; // Centralina/MG
    4317: CodIBGE := 3115904; // Chacara/MG
    4319: CodIBGE := 3116001; // Chale/MG
    4321: CodIBGE := 3116100; // Chapada Do Norte/MG
    0588: CodIBGE := 3116159; // Chapada Gaucha/MG
    4323: CodIBGE := 3116209; // Chiador/MG
    4325: CodIBGE := 3116308; // Cipotanea/MG
    4327: CodIBGE := 3116407; // Claraval/MG
    4329: CodIBGE := 3116506; // Claro Dos Pocoes/MG
    4331: CodIBGE := 3116605; // Claudio/MG
    4333: CodIBGE := 3116704; // Coimbra/MG
    4335: CodIBGE := 3116803; // Coluna/MG
    4337: CodIBGE := 3116902; // Comendador Gomes/MG
    4339: CodIBGE := 3117009; // Comercinho/MG
    4341: CodIBGE := 3117108; // Conceicao Da Aparecida/MG
    4343: CodIBGE := 3117207; // Conceicao Das Pedras/MG
    4345: CodIBGE := 3117306; // Conceicao Das Alagoas/MG
    4347: CodIBGE := 3117405; // Conceicao De Ipanema/MG
    4349: CodIBGE := 3117504; // Conceicao Do Mato Dentro/MG
    4351: CodIBGE := 3117603; // Conceicao Do Para/MG
    4353: CodIBGE := 3117702; // Conceicao Do Rio Verde/MG
    4355: CodIBGE := 3117801; // Conceicao Dos Ouros/MG
    0590: CodIBGE := 3117836; // Conego Marinho/MG
    0592: CodIBGE := 3117876; // Confins/MG
    4357: CodIBGE := 3117900; // Congonhal/MG
    4359: CodIBGE := 3118007; // Congonhas/MG
    4361: CodIBGE := 3118106; // Congonhas Do Norte/MG
    4363: CodIBGE := 3118205; // Conquista/MG
    4365: CodIBGE := 3118304; // Conselheiro Lafaiete/MG
    4367: CodIBGE := 3118403; // Conselheiro Pena/MG
    4369: CodIBGE := 3118502; // Consolacao/MG
    4371: CodIBGE := 3118601; // Contagem/MG
    4373: CodIBGE := 3118700; // Coqueiral/MG
    4375: CodIBGE := 3118809; // Coracao De Jesus/MG
    4377: CodIBGE := 3118908; // Cordisburgo/MG
    4379: CodIBGE := 3119005; // Cordislandia/MG
    4381: CodIBGE := 3119104; // Corinto/MG
    4383: CodIBGE := 3119203; // Coroaci/MG
    4385: CodIBGE := 3119302; // Coromandel/MG
    4387: CodIBGE := 3119401; // Coronel Fabriciano/MG
    4389: CodIBGE := 3119500; // Coronel Murta/MG
    4391: CodIBGE := 3119609; // Coronel Pacheco/MG
    4393: CodIBGE := 3119708; // Coronel Xavier Chaves/MG
    4395: CodIBGE := 3119807; // Corrego Danta/MG
    4397: CodIBGE := 3119906; // Corrego Do Bom Jesus/MG
    0594: CodIBGE := 3119955; // Corrego Fundo/MG
    4399: CodIBGE := 3120003; // Corrego Novo/MG
    4401: CodIBGE := 3120102; // Couto De Magalhaes De Minas/MG
    0596: CodIBGE := 3120151; // Crisolita/MG
    4403: CodIBGE := 3120201; // Cristais/MG
    4405: CodIBGE := 3120300; // Cristalia/MG
    4407: CodIBGE := 3120409; // Cristiano Otoni/MG
    4409: CodIBGE := 3120508; // Cristina/MG
    4411: CodIBGE := 3120607; // Crucilandia/MG
    4413: CodIBGE := 3120706; // Cruzeiro Da Fortaleza/MG
    4415: CodIBGE := 3120805; // Cruzilia/MG
    0598: CodIBGE := 3120839; // Cuparaque/MG
    0600: CodIBGE := 3120870; // Curral De Dentro/MG
    4417: CodIBGE := 3120904; // Curvelo/MG
    4419: CodIBGE := 3121001; // Datas/MG
    4421: CodIBGE := 3121100; // Delfim Moreira/MG
    4423: CodIBGE := 3121209; // Delfinopolis/MG
    0602: CodIBGE := 3121258; // Delta/MG
    4425: CodIBGE := 3121308; // Descoberto/MG
    4427: CodIBGE := 3121407; // Desterro De Entre Rios/MG
    4429: CodIBGE := 3121506; // Desterro Do Melo/MG
    4431: CodIBGE := 3121605; // Diamantina/MG
    4433: CodIBGE := 3121704; // Diogo De Vasconcelos/MG
    4435: CodIBGE := 3121803; // Dionisio/MG
    4437: CodIBGE := 3121902; // Divinesia/MG
    4439: CodIBGE := 3122009; // Divino/MG
    4441: CodIBGE := 3122108; // Divino Das Laranjeiras/MG
    4443: CodIBGE := 3122207; // Divinolandia De Minas/MG
    4445: CodIBGE := 3122306; // Divinopolis/MG
    0604: CodIBGE := 3122355; // Divisa Alegre/MG
    4447: CodIBGE := 3122405; // Divisa Nova/MG
    2657: CodIBGE := 3122454; // Divisopolis/MG
    0606: CodIBGE := 3122470; // Dom Bosco/MG
    4449: CodIBGE := 3122504; // Dom Cavati/MG
    4451: CodIBGE := 3122603; // Dom Joaquim/MG
    4453: CodIBGE := 3122702; // Dom Silverio/MG
    4455: CodIBGE := 3122801; // Dom Vicoso/MG
    4457: CodIBGE := 3122900; // Dona Eusebia/MG
    4459: CodIBGE := 3123007; // Dores De Campos/MG
    4461: CodIBGE := 3123106; // Dores De Guanhaes/MG
    4463: CodIBGE := 3123205; // Dores Do Indaia/MG
    4465: CodIBGE := 3123304; // Dores Do Turvo/MG
    4467: CodIBGE := 3123403; // Doresopolis/MG
    4469: CodIBGE := 3123502; // Douradoquara/MG
    2675: CodIBGE := 3123528; // Durande/MG
    4471: CodIBGE := 3123601; // Eloi Mendes/MG
    4473: CodIBGE := 3123700; // Engenheiro Caldas/MG
    4475: CodIBGE := 3123809; // Engenheiro Navarro/MG
    2663: CodIBGE := 3123858; // Entre Folhas/MG
    4477: CodIBGE := 3123908; // Entre Rios De Minas/MG
    4479: CodIBGE := 3124005; // Ervalia/MG
    4481: CodIBGE := 3124104; // Esmeraldas/MG
    4483: CodIBGE := 3124203; // Espera Feliz/MG
    4485: CodIBGE := 3124302; // Espinosa/MG
    4487: CodIBGE := 3124401; // Espirito Santo Do Dourado/MG
    4489: CodIBGE := 3124500; // Estiva/MG
    4491: CodIBGE := 3124609; // Estrela Dalva/MG
    4493: CodIBGE := 3124708; // Estrela Do Indaia/MG
    4495: CodIBGE := 3124807; // Estrela Do Sul/MG
    4497: CodIBGE := 3124906; // Eugenopolis/MG
    4499: CodIBGE := 3125002; // Ewbank Da Camara/MG
    4501: CodIBGE := 3125101; // Extrema/MG
    4503: CodIBGE := 3125200; // Fama/MG
    4505: CodIBGE := 3125309; // Faria Lemos/MG
    4507: CodIBGE := 3125408; // Felicio Dos Santos/MG
    5238: CodIBGE := 3125507; // Sao Goncalo Do Rio Preto/MG
    4511: CodIBGE := 3125606; // Felisburgo/MG
    4513: CodIBGE := 3125705; // Felixlandia/MG
    4515: CodIBGE := 3125804; // Fernandes Tourinho/MG
    4517: CodIBGE := 3125903; // Ferros/MG
    2683: CodIBGE := 3125952; // Fervedouro/MG
    4519: CodIBGE := 3126000; // Florestal/MG
    4521: CodIBGE := 3126109; // Formiga/MG
    4523: CodIBGE := 3126208; // Formoso/MG
    4525: CodIBGE := 3126307; // Fortaleza De Minas/MG
    4527: CodIBGE := 3126406; // Fortuna De Minas/MG
    4529: CodIBGE := 3126505; // Francisco Badaro/MG
    4531: CodIBGE := 3126604; // Francisco Dumont/MG
    4533: CodIBGE := 3126703; // Francisco Sa/MG
    0608: CodIBGE := 3126752; // Franciscopolis/MG
    4535: CodIBGE := 3126802; // Frei Gaspar/MG
    4537: CodIBGE := 3126901; // Frei Inocencio/MG
    0610: CodIBGE := 3126950; // Frei Lagonegro/MG
    4539: CodIBGE := 3127008; // Fronteira/MG
    4935: CodIBGE := 3127057; // Fronteira Dos Vales/MG
    0612: CodIBGE := 3127073; // Fruta De Leite/MG
    4541: CodIBGE := 3127107; // Frutal/MG
    4543: CodIBGE := 3127206; // Funilandia/MG
    4545: CodIBGE := 3127305; // Galileia/MG
    0614: CodIBGE := 3127339; // Gameleiras/MG
    0616: CodIBGE := 3127354; // Glaucilandia/MG
    0618: CodIBGE := 3127370; // Goiabeira/MG
    0620: CodIBGE := 3127388; // Goiana/MG
    4547: CodIBGE := 3127404; // Goncalves/MG
    4549: CodIBGE := 3127503; // Gonzaga/MG
    4551: CodIBGE := 3127602; // Gouveia/MG
    4553: CodIBGE := 3127701; // Governador Valadares/MG
    4555: CodIBGE := 3127800; // Grao Mogol/MG
    4557: CodIBGE := 3127909; // Grupiara/MG
    4559: CodIBGE := 3128006; // Guanhaes/MG
    4561: CodIBGE := 3128105; // Guape/MG
    4563: CodIBGE := 3128204; // Guaraciaba/MG
    0622: CodIBGE := 3128253; // Guaraciama/MG
    4565: CodIBGE := 3128303; // Guaranesia/MG
    4567: CodIBGE := 3128402; // Guarani/MG
    4569: CodIBGE := 3128501; // Guarara/MG
    4571: CodIBGE := 3128600; // Guarda-Mor/MG
    4573: CodIBGE := 3128709; // Guaxupe/MG
    4575: CodIBGE := 3128808; // Guidoval/MG
    4577: CodIBGE := 3128907; // Guimarania/MG
    4579: CodIBGE := 3129004; // Guiricema/MG
    4581: CodIBGE := 3129103; // Gurinhata/MG
    4583: CodIBGE := 3129202; // Heliodora/MG
    4585: CodIBGE := 3129301; // Iapu/MG
    4587: CodIBGE := 3129400; // Ibertioga/MG
    4589: CodIBGE := 3129509; // Ibia/MG
    4591: CodIBGE := 3129608; // Ibiai/MG
    0624: CodIBGE := 3129657; // Ibiracatu/MG
    4593: CodIBGE := 3129707; // Ibiraci/MG
    4595: CodIBGE := 3129806; // Ibirite/MG
    4597: CodIBGE := 3129905; // Ibitiura De Minas/MG
    4599: CodIBGE := 3130002; // Ibituruna/MG
    2693: CodIBGE := 3130051; // Icarai De Minas/MG
    4601: CodIBGE := 3130101; // Igarape/MG
    4603: CodIBGE := 3130200; // Igaratinga/MG
    4605: CodIBGE := 3130309; // Iguatama/MG
    4607: CodIBGE := 3130408; // Ijaci/MG
    4609: CodIBGE := 3130507; // Ilicinea/MG
    0626: CodIBGE := 3130556; // Imbe De Minas/MG
    4611: CodIBGE := 3130606; // Inconfidentes/MG
    0628: CodIBGE := 3130655; // Indaiabira/MG
    4613: CodIBGE := 3130705; // Indianopolis/MG
    4615: CodIBGE := 3130804; // Ingai/MG
    4617: CodIBGE := 3130903; // Inhapim/MG
    4619: CodIBGE := 3131000; // Inhauma/MG
    4621: CodIBGE := 3131109; // Inimutaba/MG
    2665: CodIBGE := 3131158; // Ipaba/MG
    4623: CodIBGE := 3131208; // Ipanema/MG
    4625: CodIBGE := 3131307; // Ipatinga/MG
    4627: CodIBGE := 3131406; // Ipiacu/MG
    4629: CodIBGE := 3131505; // Ipuiuna/MG
    4631: CodIBGE := 3131604; // Irai De Minas/MG
    4633: CodIBGE := 3131703; // Itabira/MG
    4635: CodIBGE := 3131802; // Itabirinha/MG
    4637: CodIBGE := 3131901; // Itabirito/MG
    4639: CodIBGE := 3132008; // Itacambira/MG
    4641: CodIBGE := 3132107; // Itacarambi/MG
    4643: CodIBGE := 3132206; // Itaguara/MG
    4645: CodIBGE := 3132305; // Itaipe/MG
    4647: CodIBGE := 3132404; // Itajuba/MG
    4649: CodIBGE := 3132503; // Itamarandiba/MG
    4651: CodIBGE := 3132602; // Itamarati De Minas/MG
    4653: CodIBGE := 3132701; // Itambacuri/MG
    4655: CodIBGE := 3132800; // Itambe Do Mato Dentro/MG
    4657: CodIBGE := 3132909; // Itamogi/MG
    4659: CodIBGE := 3133006; // Itamonte/MG
    4661: CodIBGE := 3133105; // Itanhandu/MG
    4663: CodIBGE := 3133204; // Itanhomi/MG
    4665: CodIBGE := 3133303; // Itaobim/MG
    4667: CodIBGE := 3133402; // Itapagipe/MG
    4669: CodIBGE := 3133501; // Itapecerica/MG
    4671: CodIBGE := 3133600; // Itapeva/MG
    4673: CodIBGE := 3133709; // Itatiaiucu/MG
    5731: CodIBGE := 3133758; // Itau De Minas/MG
    4675: CodIBGE := 3133808; // Itauna/MG
    4677: CodIBGE := 3133907; // Itaverava/MG
    4679: CodIBGE := 3134004; // Itinga/MG
    4681: CodIBGE := 3134103; // Itueta/MG
    4683: CodIBGE := 3134202; // Ituiutaba/MG
    4685: CodIBGE := 3134301; // Itumirim/MG
    4687: CodIBGE := 3134400; // Iturama/MG
    4689: CodIBGE := 3134509; // Itutinga/MG
    4691: CodIBGE := 3134608; // Jaboticatubas/MG
    4693: CodIBGE := 3134707; // Jacinto/MG
    4695: CodIBGE := 3134806; // Jacui/MG
    4697: CodIBGE := 3134905; // Jacutinga/MG
    4699: CodIBGE := 3135001; // Jaguaracu/MG
    2893: CodIBGE := 3135050; // Jaiba/MG
    2655: CodIBGE := 3135076; // Jampruca/MG
    4701: CodIBGE := 3135100; // Janauba/MG
    4703: CodIBGE := 3135209; // Januaria/MG
    4705: CodIBGE := 3135308; // Japaraiba/MG
    0630: CodIBGE := 3135357; // Japonvar/MG
    4707: CodIBGE := 3135407; // Jeceaba/MG
    0632: CodIBGE := 3135456; // Jenipapo De Minas/MG
    4709: CodIBGE := 3135506; // Jequeri/MG
    4711: CodIBGE := 3135605; // Jequitai/MG
    4713: CodIBGE := 3135704; // Jequitiba/MG
    4715: CodIBGE := 3135803; // Jequitinhonha/MG
    4717: CodIBGE := 3135902; // Jesuania/MG
    4719: CodIBGE := 3136009; // Joaima/MG
    4721: CodIBGE := 3136108; // Joanesia/MG
    4723: CodIBGE := 3136207; // Joao Monlevade/MG
    4725: CodIBGE := 3136306; // Joao Pinheiro/MG
    4727: CodIBGE := 3136405; // Joaquim Felicio/MG
    4729: CodIBGE := 3136504; // Jordania/MG
    0634: CodIBGE := 3136520; // Jose Goncalves De Minas/MG
    0636: CodIBGE := 3136553; // Jose Raydan/MG
    0638: CodIBGE := 3136579; // Josenopolis/MG
    4731: CodIBGE := 3136603; // Nova Uniao/MG
    2691: CodIBGE := 3136652; // Juatuba/MG
    4733: CodIBGE := 3136702; // Juiz De Fora/MG
    4735: CodIBGE := 3136801; // Juramento/MG
    4737: CodIBGE := 3136900; // Juruaia/MG
    0640: CodIBGE := 3136959; // Juvenilia/MG
    4739: CodIBGE := 3137007; // Ladainha/MG
    4741: CodIBGE := 3137106; // Lagamar/MG
    4743: CodIBGE := 3137205; // Lagoa Da Prata/MG
    4745: CodIBGE := 3137304; // Lagoa Dos Patos/MG
    4747: CodIBGE := 3137403; // Lagoa Dourada/MG
    4749: CodIBGE := 3137502; // Lagoa Formosa/MG
    2905: CodIBGE := 3137536; // Lagoa Grande/MG
    4751: CodIBGE := 3137601; // Lagoa Santa/MG
    4753: CodIBGE := 3137700; // Lajinha/MG
    4755: CodIBGE := 3137809; // Lambari/MG
    4757: CodIBGE := 3137908; // Lamim/MG
    4759: CodIBGE := 3138005; // Laranjal/MG
    4761: CodIBGE := 3138104; // Lassance/MG
    4763: CodIBGE := 3138203; // Lavras/MG
    4765: CodIBGE := 3138302; // Leandro Ferreira/MG
    0642: CodIBGE := 3138351; // Leme Do Prado/MG
    4767: CodIBGE := 3138401; // Leopoldina/MG
    4769: CodIBGE := 3138500; // Liberdade/MG
    4771: CodIBGE := 3138609; // Lima Duarte/MG
    2687: CodIBGE := 3138625; // Limeira Do Oeste/MG
    2695: CodIBGE := 3138658; // Lontra/MG
    0644: CodIBGE := 3138674; // Luisburgo/MG
    0646: CodIBGE := 3138682; // Luislandia/MG
    4773: CodIBGE := 3138708; // Luminarias/MG
    4775: CodIBGE := 3138807; // Luz/MG
    4777: CodIBGE := 3138906; // Machacalis/MG
    4779: CodIBGE := 3139003; // Machado/MG
    4781: CodIBGE := 3139102; // Madre De Deus De Minas/MG
    4783: CodIBGE := 3139201; // Malacacheta/MG
    2895: CodIBGE := 3139250; // Mamonas/MG
    4785: CodIBGE := 3139300; // Manga/MG
    4787: CodIBGE := 3139409; // Manhuacu/MG
    4789: CodIBGE := 3139508; // Manhumirim/MG
    4791: CodIBGE := 3139607; // Mantena/MG
    4793: CodIBGE := 3139706; // Maravilhas/MG
    4795: CodIBGE := 3139805; // Mar De Espanha/MG
    4797: CodIBGE := 3139904; // Maria Da Fe/MG
    4799: CodIBGE := 3140001; // Mariana/MG
    4801: CodIBGE := 3140100; // Marilac/MG
    0648: CodIBGE := 3140159; // Mario Campos/MG
    4803: CodIBGE := 3140209; // Maripa De Minas/MG
    4805: CodIBGE := 3140308; // Marlieria/MG
    4807: CodIBGE := 3140407; // Marmelopolis/MG
    4809: CodIBGE := 3140506; // Martinho Campos/MG
    0650: CodIBGE := 3140530; // Martins Soares/MG
    2659: CodIBGE := 3140555; // Mata Verde/MG
    4811: CodIBGE := 3140605; // Materlandia/MG
    4813: CodIBGE := 3140704; // Mateus Leme/MG
    4815: CodIBGE := 3140803; // Matias Barbosa/MG
    2897: CodIBGE := 3140852; // Matias Cardoso/MG
    4817: CodIBGE := 3140902; // Matipo/MG
    4819: CodIBGE := 3141009; // Mato Verde/MG
    4821: CodIBGE := 3141108; // Matozinhos/MG
    4823: CodIBGE := 3141207; // Matutina/MG
    4825: CodIBGE := 3141306; // Medeiros/MG
    4827: CodIBGE := 3141405; // Medina/MG
    4829: CodIBGE := 3141504; // Mendes Pimentel/MG
    4831: CodIBGE := 3141603; // Merces/MG
    4833: CodIBGE := 3141702; // Mesquita/MG
    4835: CodIBGE := 3141801; // Minas Novas/MG
    4837: CodIBGE := 3141900; // Minduri/MG
    4839: CodIBGE := 3142007; // Mirabela/MG
    4841: CodIBGE := 3142106; // Miradouro/MG
    4843: CodIBGE := 3142205; // Mirai/MG
    0652: CodIBGE := 3142254; // Miravania/MG
    4845: CodIBGE := 3142304; // Moeda/MG
    4847: CodIBGE := 3142403; // Moema/MG
    4849: CodIBGE := 3142502; // Monjolos/MG
    4851: CodIBGE := 3142601; // Monsenhor Paulo/MG
    4853: CodIBGE := 3142700; // Montalvania/MG
    4855: CodIBGE := 3142809; // Monte Alegre De Minas/MG
    4857: CodIBGE := 3142908; // Monte Azul/MG
    4859: CodIBGE := 3143005; // Monte Belo/MG
    4861: CodIBGE := 3143104; // Monte Carmelo/MG
    0654: CodIBGE := 3143153; // Monte Formoso/MG
    4863: CodIBGE := 3143203; // Monte Santo De Minas/MG
    4865: CodIBGE := 3143302; // Montes Claros/MG
    4867: CodIBGE := 3143401; // Monte Siao/MG
    2697: CodIBGE := 3143450; // Montezuma/MG
    4869: CodIBGE := 3143500; // Morada Nova De Minas/MG
    4871: CodIBGE := 3143609; // Morro Da Garca/MG
    4873: CodIBGE := 3143708; // Morro Do Pilar/MG
    4875: CodIBGE := 3143807; // Munhoz/MG
    4877: CodIBGE := 3143906; // Muriae/MG
    4879: CodIBGE := 3144003; // Mutum/MG
    4881: CodIBGE := 3144102; // Muzambinho/MG
    4883: CodIBGE := 3144201; // Nacip Raydan/MG
    4885: CodIBGE := 3144300; // Nanuque/MG
    0656: CodIBGE := 3144359; // Naque/MG
    0658: CodIBGE := 3144375; // Natalandia/MG
    4887: CodIBGE := 3144409; // Natercia/MG
    4889: CodIBGE := 3144508; // Nazareno/MG
    4891: CodIBGE := 3144607; // Nepomuceno/MG
    0660: CodIBGE := 3144656; // Ninheira/MG
    0662: CodIBGE := 3144672; // Nova Belem/MG
    4893: CodIBGE := 3144706; // Nova Era/MG
    4895: CodIBGE := 3144805; // Nova Lima/MG
    4897: CodIBGE := 3144904; // Nova Modica/MG
    4899: CodIBGE := 3145000; // Nova Ponte/MG
    0664: CodIBGE := 3145059; // Nova Porteirinha/MG
    4901: CodIBGE := 3145109; // Nova Resende/MG
    4903: CodIBGE := 3145208; // Nova Serrana/MG
    4905: CodIBGE := 3145307; // Novo Cruzeiro/MG
    0666: CodIBGE := 3145356; // Novo Oriente De Minas/MG
    0668: CodIBGE := 3145372; // Novorizonte/MG
    4907: CodIBGE := 3145406; // Olaria/MG
    0670: CodIBGE := 3145455; // Olhos-D Agua/MG
    4909: CodIBGE := 3145505; // Olimpio Noronha/MG
    4911: CodIBGE := 3145604; // Oliveira/MG
    4913: CodIBGE := 3145703; // Oliveira Fortes/MG
    4915: CodIBGE := 3145802; // Onca De Pitangui/MG
    0672: CodIBGE := 3145851; // Oratorios/MG
    0674: CodIBGE := 3145877; // Orizania/MG
    4917: CodIBGE := 3145901; // Ouro Branco/MG
    4919: CodIBGE := 3146008; // Ouro Fino/MG
    4921: CodIBGE := 3146107; // Ouro Preto/MG
    4923: CodIBGE := 3146206; // Ouro Verde De Minas/MG
    0676: CodIBGE := 3146255; // Padre Carvalho/MG
    4925: CodIBGE := 3146305; // Padre Paraiso/MG
    4927: CodIBGE := 3146404; // Paineiras/MG
    4929: CodIBGE := 3146503; // Pains/MG
    0678: CodIBGE := 3146552; // Pai Pedro/MG
    4931: CodIBGE := 3146602; // Paiva/MG
    4933: CodIBGE := 3146701; // Palma/MG
    2661: CodIBGE := 3146750; // Palmopolis/MG
    4937: CodIBGE := 3146909; // Papagaios/MG
    4939: CodIBGE := 3147006; // Paracatu/MG
    4941: CodIBGE := 3147105; // Para De Minas/MG
    4943: CodIBGE := 3147204; // Paraguacu/MG
    4945: CodIBGE := 3147303; // Paraisopolis/MG
    4947: CodIBGE := 3147402; // Paraopeba/MG
    4949: CodIBGE := 3147501; // Passabem/MG
    4951: CodIBGE := 3147600; // Passa Quatro/MG
    4953: CodIBGE := 3147709; // Passa Tempo/MG
    4955: CodIBGE := 3147808; // Passa-Vinte/MG
    4957: CodIBGE := 3147907; // Passos/MG
    0680: CodIBGE := 3147956; // Patis/MG
    4959: CodIBGE := 3148004; // Patos De Minas/MG
    4961: CodIBGE := 3148103; // Patrocinio/MG
    4963: CodIBGE := 3148202; // Patrocinio Do Muriae/MG
    4965: CodIBGE := 3148301; // Paula Candido/MG
    4967: CodIBGE := 3148400; // Paulistas/MG
    4969: CodIBGE := 3148509; // Pavao/MG
    4971: CodIBGE := 3148608; // Pecanha/MG
    4973: CodIBGE := 3148707; // Pedra Azul/MG
    0682: CodIBGE := 3148756; // Pedra Bonita/MG
    4975: CodIBGE := 3148806; // Pedra Do Anta/MG
    4977: CodIBGE := 3148905; // Pedra Do Indaia/MG
    4979: CodIBGE := 3149002; // Pedra Dourada/MG
    4981: CodIBGE := 3149101; // Pedralva/MG
    2899: CodIBGE := 3149150; // Pedras De Maria Da Cruz/MG
    4983: CodIBGE := 3149200; // Pedrinopolis/MG
    4985: CodIBGE := 3149309; // Pedro Leopoldo/MG
    4987: CodIBGE := 3149408; // Pedro Teixeira/MG
    4989: CodIBGE := 3149507; // Pequeri/MG
    4991: CodIBGE := 3149606; // Pequi/MG
    4993: CodIBGE := 3149705; // Perdigao/MG
    4995: CodIBGE := 3149804; // Perdizes/MG
    4997: CodIBGE := 3149903; // Perdoes/MG
    0684: CodIBGE := 3149952; // Periquito/MG
    4999: CodIBGE := 3150000; // Pescador/MG
    5001: CodIBGE := 3150109; // Piau/MG
    0686: CodIBGE := 3150158; // Piedade De Caratinga/MG
    5003: CodIBGE := 3150208; // Piedade De Ponte Nova/MG
    5005: CodIBGE := 3150307; // Piedade Do Rio Grande/MG
    5007: CodIBGE := 3150406; // Piedade Dos Gerais/MG
    5009: CodIBGE := 3150505; // Pimenta/MG
    0688: CodIBGE := 3150539; // Pingo-D Agua/MG
    0690: CodIBGE := 3150570; // Pintopolis/MG
    5011: CodIBGE := 3150604; // Piracema/MG
    5013: CodIBGE := 3150703; // Pirajuba/MG
    5015: CodIBGE := 3150802; // Piranga/MG
    5017: CodIBGE := 3150901; // Pirangucu/MG
    5019: CodIBGE := 3151008; // Piranguinho/MG
    5021: CodIBGE := 3151107; // Pirapetinga/MG
    5023: CodIBGE := 3151206; // Pirapora/MG
    5025: CodIBGE := 3151305; // Pirauba/MG
    5027: CodIBGE := 3151404; // Pitangui/MG
    5029: CodIBGE := 3151503; // Piumhi/MG
    5031: CodIBGE := 3151602; // Planura/MG
    5033: CodIBGE := 3151701; // Poco Fundo/MG
    5035: CodIBGE := 3151800; // Pocos De Caldas/MG
    5037: CodIBGE := 3151909; // Pocrane/MG
    5039: CodIBGE := 3152006; // Pompeu/MG
    5041: CodIBGE := 3152105; // Ponte Nova/MG
    0692: CodIBGE := 3152131; // Ponto Chique/MG
    0694: CodIBGE := 3152170; // Ponto Dos Volantes/MG
    5043: CodIBGE := 3152204; // Porteirinha/MG
    5045: CodIBGE := 3152303; // Porto Firme/MG
    5047: CodIBGE := 3152402; // Pote/MG
    5049: CodIBGE := 3152501; // Pouso Alegre/MG
    5051: CodIBGE := 3152600; // Pouso Alto/MG
    5053: CodIBGE := 3152709; // Prados/MG
    5055: CodIBGE := 3152808; // Prata/MG
    5057: CodIBGE := 3152907; // Pratapolis/MG
    5059: CodIBGE := 3153004; // Pratinha/MG
    5061: CodIBGE := 3153103; // Presidente Bernardes/MG
    5063: CodIBGE := 3153202; // Presidente Juscelino/MG
    5065: CodIBGE := 3153301; // Presidente Kubitschek/MG
    5067: CodIBGE := 3153400; // Presidente Olegario/MG
    5069: CodIBGE := 3153509; // Alto Jequitiba/MG
    5071: CodIBGE := 3153608; // Prudente De Morais/MG
    5073: CodIBGE := 3153707; // Quartel Geral/MG
    5075: CodIBGE := 3153806; // Queluzito/MG
    5077: CodIBGE := 3153905; // Raposos/MG
    5079: CodIBGE := 3154002; // Raul Soares/MG
    5081: CodIBGE := 3154101; // Recreio/MG
    0696: CodIBGE := 3154150; // Reduto/MG
    5083: CodIBGE := 3154200; // Resende Costa/MG
    5085: CodIBGE := 3154309; // Resplendor/MG
    5087: CodIBGE := 3154408; // Ressaquinha/MG
    2901: CodIBGE := 3154457; // Riachinho/MG
    5089: CodIBGE := 3154507; // Riacho Dos Machados/MG
    5091: CodIBGE := 3154606; // Ribeirao Das Neves/MG
    5093: CodIBGE := 3154705; // Ribeirao Vermelho/MG
    5095: CodIBGE := 3154804; // Rio Acima/MG
    5097: CodIBGE := 3154903; // Rio Casca/MG
    5099: CodIBGE := 3155009; // Rio Doce/MG
    5101: CodIBGE := 3155108; // Rio Do Prado/MG
    5103: CodIBGE := 3155207; // Rio Espera/MG
    5105: CodIBGE := 3155306; // Rio Manso/MG
    5107: CodIBGE := 3155405; // Rio Novo/MG
    5109: CodIBGE := 3155504; // Rio Paranaiba/MG
    5111: CodIBGE := 3155603; // Rio Pardo De Minas/MG
    5113: CodIBGE := 3155702; // Rio Piracicaba/MG
    5115: CodIBGE := 3155801; // Rio Pomba/MG
    5117: CodIBGE := 3155900; // Rio Preto/MG
    5119: CodIBGE := 3156007; // Rio Vermelho/MG
    5121: CodIBGE := 3156106; // Ritapolis/MG
    5123: CodIBGE := 3156205; // Rochedo De Minas/MG
    5125: CodIBGE := 3156304; // Rodeiro/MG
    5127: CodIBGE := 3156403; // Romaria/MG
    0698: CodIBGE := 3156452; // Rosario Da Limeira/MG
    5129: CodIBGE := 3156502; // Rubelita/MG
    5131: CodIBGE := 3156601; // Rubim/MG
    5133: CodIBGE := 3156700; // Sabara/MG
    5135: CodIBGE := 3156809; // Sabinopolis/MG
    5137: CodIBGE := 3156908; // Sacramento/MG
    5139: CodIBGE := 3157005; // Salinas/MG
    5141: CodIBGE := 3157104; // Salto Da Divisa/MG
    5143: CodIBGE := 3157203; // Santa Barbara/MG
    2667: CodIBGE := 3157252; // Santa Barbara Do Leste/MG
    0700: CodIBGE := 3157278; // Santa Barbara Do Monte Verde/MG
    5145: CodIBGE := 3157302; // Santa Barbara Do Tugurio/MG
    0702: CodIBGE := 3157336; // Santa Cruz De Minas/MG
    0704: CodIBGE := 3157377; // Santa Cruz De Salinas/MG
    5147: CodIBGE := 3157401; // Santa Cruz Do Escalvado/MG
    5149: CodIBGE := 3157500; // Santa Efigenia De Minas/MG
    5151: CodIBGE := 3157609; // Santa Fe De Minas/MG
    0706: CodIBGE := 3157658; // Santa Helena De Minas/MG
    5153: CodIBGE := 3157708; // Santa Juliana/MG
    5155: CodIBGE := 3157807; // Santa Luzia/MG
    5157: CodIBGE := 3157906; // Santa Margarida/MG
    5159: CodIBGE := 3158003; // Santa Maria De Itabira/MG
    5161: CodIBGE := 3158102; // Santa Maria Do Salto/MG
    5163: CodIBGE := 3158201; // Santa Maria Do Suacui/MG
    5165: CodIBGE := 3158300; // Santana Da Vargem/MG
    5167: CodIBGE := 3158409; // Santana De Cataguases/MG
    5169: CodIBGE := 3158508; // Santana De Pirapama/MG
    5171: CodIBGE := 3158607; // Santana Do Deserto/MG
    5173: CodIBGE := 3158706; // Santana Do Garambeu/MG
    5175: CodIBGE := 3158805; // Santana Do Jacare/MG
    5177: CodIBGE := 3158904; // Santana Do Manhuacu/MG
    2673: CodIBGE := 3158953; // Santana Do Paraiso/MG
    5179: CodIBGE := 3159001; // Santana Do Riacho/MG
    5181: CodIBGE := 3159100; // Santana Dos Montes/MG
    5183: CodIBGE := 3159209; // Santa Rita De Caldas/MG
    5185: CodIBGE := 3159308; // Santa Rita De Jacutinga/MG
    2669: CodIBGE := 3159357; // Santa Rita De Minas/MG
    5187: CodIBGE := 3159407; // Santa Rita De Ibitipoca/MG
    5189: CodIBGE := 3159506; // Santa Rita Do Itueto/MG
    5191: CodIBGE := 3159605; // Santa Rita Do Sapucai/MG
    5193: CodIBGE := 3159704; // Santa Rosa Da Serra/MG
    5195: CodIBGE := 3159803; // Santa Vitoria/MG
    5197: CodIBGE := 3159902; // Santo Antonio Do Amparo/MG
    5199: CodIBGE := 3160009; // Santo Antonio Do Aventureiro/MG
    5201: CodIBGE := 3160108; // Santo Antonio Do Grama/MG
    5203: CodIBGE := 3160207; // Santo Antonio Do Itambe/MG
    5205: CodIBGE := 3160306; // Santo Antonio Do Jacinto/MG
    5207: CodIBGE := 3160405; // Santo Antonio Do Monte/MG
    0708: CodIBGE := 3160454; // Santo Antonio Do Retiro/MG
    5209: CodIBGE := 3160504; // Santo Antonio Do Rio Abaixo/MG
    5211: CodIBGE := 3160603; // Santo Hipolito/MG
    5213: CodIBGE := 3160702; // Santos Dumont/MG
    5215: CodIBGE := 3160801; // Sao Bento Abade/MG
    5217: CodIBGE := 3160900; // Sao Bras Do Suacui/MG
    0710: CodIBGE := 3160959; // Sao Domingos Das Dores/MG
    5219: CodIBGE := 3161007; // Sao Domingos Do Prata/MG
    0712: CodIBGE := 3161056; // Sao Felix De Minas/MG
    5221: CodIBGE := 3161106; // Sao Francisco/MG
    5223: CodIBGE := 3161205; // Sao Francisco De Paula/MG
    5225: CodIBGE := 3161304; // Sao Francisco De Sales/MG
    5227: CodIBGE := 3161403; // Sao Francisco Do Gloria/MG
    5229: CodIBGE := 3161502; // Sao Geraldo/MG
    5231: CodIBGE := 3161601; // Sao Geraldo Da Piedade/MG
    0714: CodIBGE := 3161650; // Sao Geraldo Do Baixio/MG
    5233: CodIBGE := 3161700; // Sao Goncalo Do Abaete/MG
    5235: CodIBGE := 3161809; // Sao Goncalo Do Para/MG
    5237: CodIBGE := 3161908; // Sao Goncalo Do Rio Abaixo/MG
    5239: CodIBGE := 3162005; // Sao Goncalo Do Sapucai/MG
    5241: CodIBGE := 3162104; // Sao Gotardo/MG
    5243: CodIBGE := 3162203; // Sao Joao Batista Do Gloria/MG
    0716: CodIBGE := 3162252; // Sao Joao Da Lagoa/MG
    5245: CodIBGE := 3162302; // Sao Joao Da Mata/MG
    5247: CodIBGE := 3162401; // Sao Joao Da Ponte/MG
    0718: CodIBGE := 3162450; // Sao Joao Das Missoes/MG
    5249: CodIBGE := 3162500; // Sao Joao Del Rei/MG
    2677: CodIBGE := 3162559; // Sao Joao Do Manhuacu/MG
    2679: CodIBGE := 3162575; // Sao Joao Do Manteninha/MG
    5251: CodIBGE := 3162609; // Sao Joao Do Oriente/MG
    0720: CodIBGE := 3162658; // Sao Joao Do Pacui/MG
    5253: CodIBGE := 3162708; // Sao Joao Do Paraiso/MG
    5255: CodIBGE := 3162807; // Sao Joao Evangelista/MG
    5257: CodIBGE := 3162906; // Sao Joao Nepomuceno/MG
    0722: CodIBGE := 3162922; // Sao Joaquim De Bicas/MG
    0724: CodIBGE := 3162948; // Sao Jose Da Barra/MG
    2649: CodIBGE := 3162955; // Sao Jose Da Lapa/MG
    5259: CodIBGE := 3163003; // Sao Jose Da Safira/MG
    5261: CodIBGE := 3163102; // Sao Jose Da Varginha/MG
    5263: CodIBGE := 3163201; // Sao Jose Do Alegre/MG
    5265: CodIBGE := 3163300; // Sao Jose Do Divino/MG
    5267: CodIBGE := 3163409; // Sao Jose Do Goiabal/MG
    5269: CodIBGE := 3163508; // Sao Jose Do Jacuri/MG
    5271: CodIBGE := 3163607; // Sao Jose Do Mantimento/MG
    5273: CodIBGE := 3163706; // Sao Lourenco/MG
    5275: CodIBGE := 3163805; // Sao Miguel Do Anta/MG
    5277: CodIBGE := 3163904; // Sao Pedro Da Uniao/MG
    5279: CodIBGE := 3164001; // Sao Pedro Dos Ferros/MG
    5281: CodIBGE := 3164100; // Sao Pedro Do Suacui/MG
    5283: CodIBGE := 3164209; // Sao Romao/MG
    5285: CodIBGE := 3164308; // Sao Roque De Minas/MG
    5287: CodIBGE := 3164407; // Sao Sebastiao Da Bela Vista/MG
    0726: CodIBGE := 3164431; // Sao Sebastiao Da Vargem Alegre/MG
    0728: CodIBGE := 3164472; // Sao Sebastiao Do Anta/MG
    5289: CodIBGE := 3164506; // Sao Sebastiao Do Maranhao/MG
    5291: CodIBGE := 3164605; // Sao Sebastiao Do Oeste/MG
    5293: CodIBGE := 3164704; // Sao Sebastiao Do Paraiso/MG
    5295: CodIBGE := 3164803; // Sao Sebastiao Do Rio Preto/MG
    5297: CodIBGE := 3164902; // Sao Sebastiao Do Rio Verde/MG
    5299: CodIBGE := 3165008; // Sao Tiago/MG
    5301: CodIBGE := 3165107; // Sao Tomas De Aquino/MG
    5303: CodIBGE := 3165206; // Sao Thome Das Letras/MG
    5305: CodIBGE := 3165305; // Sao Vicente De Minas/MG
    5307: CodIBGE := 3165404; // Sapucai-Mirim/MG
    5309: CodIBGE := 3165503; // Sardoa/MG
    0730: CodIBGE := 3165537; // Sarzedo/MG
    0732: CodIBGE := 3165552; // Setubinha/MG
    0734: CodIBGE := 3165560; // Sem-Peixe/MG
    2689: CodIBGE := 3165578; // Senador Amaral/MG
    5311: CodIBGE := 3165602; // Senador Cortes/MG
    5313: CodIBGE := 3165701; // Senador Firmino/MG
    5315: CodIBGE := 3165800; // Senador Jose Bento/MG
    5317: CodIBGE := 3165909; // Senador Modestino Goncalves/MG
    5319: CodIBGE := 3166006; // Senhora De Oliveira/MG
    5321: CodIBGE := 3166105; // Senhora Do Porto/MG
    5323: CodIBGE := 3166204; // Senhora Dos Remedios/MG
    5325: CodIBGE := 3166303; // Sericita/MG
    5327: CodIBGE := 3166402; // Seritinga/MG
    5329: CodIBGE := 3166501; // Serra Azul De Minas/MG
    5331: CodIBGE := 3166600; // Serra Da Saudade/MG
    5333: CodIBGE := 3166709; // Serra Dos Aimores/MG
    5335: CodIBGE := 3166808; // Serra Do Salitre/MG
    5337: CodIBGE := 3166907; // Serrania/MG
    0736: CodIBGE := 3166956; // Serranopolis De Minas/MG
    5339: CodIBGE := 3167004; // Serranos/MG
    5341: CodIBGE := 3167103; // Serro/MG
    5343: CodIBGE := 3167202; // Sete Lagoas/MG
    5345: CodIBGE := 3167301; // Silveirania/MG
    5347: CodIBGE := 3167400; // Silvianopolis/MG
    5349: CodIBGE := 3167509; // Simao Pereira/MG
    5351: CodIBGE := 3167608; // Simonesia/MG
    5353: CodIBGE := 3167707; // Sobralia/MG
    5355: CodIBGE := 3167806; // Soledade De Minas/MG
    5357: CodIBGE := 3167905; // Tabuleiro/MG
    5359: CodIBGE := 3168002; // Taiobeiras/MG
    0738: CodIBGE := 3168051; // Taparuba/MG
    5361: CodIBGE := 3168101; // Tapira/MG
    5363: CodIBGE := 3168200; // Tapirai/MG
    5365: CodIBGE := 3168309; // Taquaracu De Minas/MG
    5367: CodIBGE := 3168408; // Tarumirim/MG
    5369: CodIBGE := 3168507; // Teixeiras/MG
    5371: CodIBGE := 3168606; // Teofilo Otoni/MG
    5373: CodIBGE := 3168705; // Timoteo/MG
    5375: CodIBGE := 3168804; // Tiradentes/MG
    5377: CodIBGE := 3168903; // Tiros/MG
    5379: CodIBGE := 3169000; // Tocantins/MG
    0740: CodIBGE := 3169059; // Tocos Do Moji/MG
    5381: CodIBGE := 3169109; // Toledo/MG
    5383: CodIBGE := 3169208; // Tombos/MG
    5385: CodIBGE := 3169307; // Tres Coracoes/MG
    4115: CodIBGE := 3169356; // Tres Marias/MG
    5387: CodIBGE := 3169406; // Tres Pontas/MG
    5389: CodIBGE := 3169505; // Tumiritinga/MG
    5391: CodIBGE := 3169604; // Tupaciguara/MG
    5393: CodIBGE := 3169703; // Turmalina/MG
    5395: CodIBGE := 3169802; // Turvolandia/MG
    5397: CodIBGE := 3169901; // Uba/MG
    5399: CodIBGE := 3170008; // Ubai/MG
    2671: CodIBGE := 3170057; // Ubaporanga/MG
    5401: CodIBGE := 3170107; // Uberaba/MG
    5403: CodIBGE := 3170206; // Uberlandia/MG
    5405: CodIBGE := 3170305; // Umburatiba/MG
    5407: CodIBGE := 3170404; // Unai/MG
    0742: CodIBGE := 3170438; // Uniao De Minas/MG
    0744: CodIBGE := 3170479; // Uruana De Minas/MG
    5409: CodIBGE := 3170503; // Urucania/MG
    2699: CodIBGE := 3170529; // Urucuia/MG
    0746: CodIBGE := 3170578; // Vargem Alegre/MG
    5411: CodIBGE := 3170602; // Vargem Bonita/MG
    0748: CodIBGE := 3170651; // Vargem Grande Do Rio Pardo/MG
    5413: CodIBGE := 3170701; // Varginha/MG
    0750: CodIBGE := 3170750; // Varjao De Minas/MG
    5415: CodIBGE := 3170800; // Varzea Da Palma/MG
    5417: CodIBGE := 3170909; // Varzelandia/MG
    5419: CodIBGE := 3171006; // Vazante/MG
    0752: CodIBGE := 3171030; // Verdelandia/MG
    0754: CodIBGE := 3171071; // Veredinha/MG
    5423: CodIBGE := 3171105; // Verissimo/MG
    0756: CodIBGE := 3171154; // Vermelho Novo/MG
    5425: CodIBGE := 3171204; // Vespasiano/MG
    5427: CodIBGE := 3171303; // Vicosa/MG
    5429: CodIBGE := 3171402; // Vieiras/MG
    5431: CodIBGE := 3171501; // Mathias Lobato/MG
    5433: CodIBGE := 3171600; // Virgem Da Lapa/MG
    5435: CodIBGE := 3171709; // Virginia/MG
    5437: CodIBGE := 3171808; // Virginopolis/MG
    5439: CodIBGE := 3171907; // Virgolandia/MG
    5441: CodIBGE := 3172004; // Visconde Do Rio Branco/MG
    5443: CodIBGE := 3172103; // Volta Grande/MG
    5421: CodIBGE := 3172202; // Wenceslau Braz/MG
    5601: CodIBGE := 3200102; // Afonso Claudio/ES
    5733: CodIBGE := 3200136; // Aguia Branca/ES
    5717: CodIBGE := 3200169; // Agua Doce Do Norte/ES
    5603: CodIBGE := 3200201; // Alegre/ES
    5605: CodIBGE := 3200300; // Alfredo Chaves/ES
    5719: CodIBGE := 3200359; // Alto Rio Novo/ES
    5607: CodIBGE := 3200409; // Anchieta/ES
    5609: CodIBGE := 3200508; // Apiaca/ES
    5611: CodIBGE := 3200607; // Aracruz/ES
    5613: CodIBGE := 3200706; // Atilio Vivacqua/ES
    5615: CodIBGE := 3200805; // Baixo Guandu/ES
    5617: CodIBGE := 3200904; // Barra De Sao Francisco/ES
    5619: CodIBGE := 3201001; // Boa Esperanca/ES
    5621: CodIBGE := 3201100; // Bom Jesus Do Norte/ES
    0758: CodIBGE := 3201159; // Brejetuba/ES
    5623: CodIBGE := 3201209; // Cachoeiro De Itapemirim/ES
    5625: CodIBGE := 3201308; // Cariacica/ES
    5627: CodIBGE := 3201407; // Castelo/ES
    5629: CodIBGE := 3201506; // Colatina/ES
    5631: CodIBGE := 3201605; // Conceicao Da Barra/ES
    5633: CodIBGE := 3201704; // Conceicao Do Castelo/ES
    5635: CodIBGE := 3201803; // Divino De Sao Lourenco/ES
    5637: CodIBGE := 3201902; // Domingos Martins/ES
    5639: CodIBGE := 3202009; // Dores Do Rio Preto/ES
    5641: CodIBGE := 3202108; // Ecoporanga/ES
    5643: CodIBGE := 3202207; // Fundao/ES
    1114: CodIBGE := 3202256; // Governador Lindenberg/ES
    5645: CodIBGE := 3202306; // Guacui/ES
    5647: CodIBGE := 3202405; // Guarapari/ES
    5709: CodIBGE := 3202454; // Ibatiba/ES
    5649: CodIBGE := 3202504; // Ibiracu/ES
    6011: CodIBGE := 3202553; // Ibitirama/ES
    5651: CodIBGE := 3202603; // Iconha/ES
    2931: CodIBGE := 3202652; // Irupi/ES
    5653: CodIBGE := 3202702; // Itaguacu/ES
    5655: CodIBGE := 3202801; // Itapemirim/ES
    5657: CodIBGE := 3202900; // Itarana/ES
    5659: CodIBGE := 3203007; // Iuna/ES
    5713: CodIBGE := 3203056; // Jaguare/ES
    5661: CodIBGE := 3203106; // Jeronimo Monteiro/ES
    5721: CodIBGE := 3203130; // Joao Neiva/ES
    5723: CodIBGE := 3203163; // Laranja Da Terra/ES
    5663: CodIBGE := 3203205; // Linhares/ES
    5665: CodIBGE := 3203304; // Mantenopolis/ES
    0760: CodIBGE := 3203320; // Marataizes/ES
    2929: CodIBGE := 3203346; // Marechal Floriano/ES
    5707: CodIBGE := 3203353; // Marilandia/ES
    5667: CodIBGE := 3203403; // Mimoso Do Sul/ES
    5669: CodIBGE := 3203502; // Montanha/ES
    5671: CodIBGE := 3203601; // Mucurici/ES
    5673: CodIBGE := 3203700; // Muniz Freire/ES
    5675: CodIBGE := 3203809; // Muqui/ES
    5677: CodIBGE := 3203908; // Nova Venecia/ES
    5679: CodIBGE := 3204005; // Pancas/ES
    5715: CodIBGE := 3204054; // Pedro Canario/ES
    5681: CodIBGE := 3204104; // Pinheiros/ES
    5683: CodIBGE := 3204203; // Piuma/ES
    0762: CodIBGE := 3204252; // Ponto Belo/ES
    5685: CodIBGE := 3204302; // Presidente Kennedy/ES
    5711: CodIBGE := 3204351; // Rio Bananal/ES
    5687: CodIBGE := 3204401; // Rio Novo Do Sul/ES
    5689: CodIBGE := 3204500; // Santa Leopoldina/ES
    5725: CodIBGE := 3204559; // Santa Maria De Jetiba/ES
    5691: CodIBGE := 3204609; // Santa Teresa/ES
    2933: CodIBGE := 3204658; // Sao Domingos Do Norte/ES
    5693: CodIBGE := 3204708; // Sao Gabriel Da Palha/ES
    5695: CodIBGE := 3204807; // Sao Jose Do Calcado/ES
    5697: CodIBGE := 3204906; // Sao Mateus/ES
    0764: CodIBGE := 3204955; // Sao Roque Do Canaa/ES
    5699: CodIBGE := 3205002; // Serra/ES
    0766: CodIBGE := 3205010; // Sooretama/ES
    5727: CodIBGE := 3205036; // Vargem Alta/ES
    5729: CodIBGE := 3205069; // Venda Nova Do Imigrante/ES
    5701: CodIBGE := 3205101; // Viana/ES
    2935: CodIBGE := 3205150; // Vila Pavao/ES
    0768: CodIBGE := 3205176; // Vila Valerio/ES
    5703: CodIBGE := 3205200; // Vila Velha/ES
    5705: CodIBGE := 3205309; // Vitoria/ES
    5801: CodIBGE := 3300100; // Angra Dos Reis/RJ
    2919: CodIBGE := 3300159; // Aperibe/RJ
    5803: CodIBGE := 3300209; // Araruama/RJ
    2925: CodIBGE := 3300225; // Areal/RJ
    0770: CodIBGE := 3300233; // Armacao Dos Buzios/RJ
    5927: CodIBGE := 3300258; // Arraial Do Cabo/RJ
    5805: CodIBGE := 3300308; // Barra Do Pirai/RJ
    5807: CodIBGE := 3300407; // Barra Mansa/RJ
    2909: CodIBGE := 3300456; // Belford Roxo/RJ
    5809: CodIBGE := 3300506; // Bom Jardim/RJ
    5811: CodIBGE := 3300605; // Bom Jesus Do Itabapoana/RJ
    5813: CodIBGE := 3300704; // Cabo Frio/RJ
    5815: CodIBGE := 3300803; // Cachoeiras De Macacu/RJ
    5817: CodIBGE := 3300902; // Cambuci/RJ
    0772: CodIBGE := 3300936; // Carapebus/RJ
    2927: CodIBGE := 3300951; // Comendador Levy Gasparian/RJ
    5819: CodIBGE := 3301009; // Campos Dos Goytacazes/RJ
    5821: CodIBGE := 3301108; // Cantagalo/RJ
    2915: CodIBGE := 3301157; // Cardoso Moreira/RJ
    5823: CodIBGE := 3301207; // Carmo/RJ
    5825: CodIBGE := 3301306; // Casimiro De Abreu/RJ
    5827: CodIBGE := 3301405; // Conceicao De Macabu/RJ
    5829: CodIBGE := 3301504; // Cordeiro/RJ
    5831: CodIBGE := 3301603; // Duas Barras/RJ
    5833: CodIBGE := 3301702; // Duque De Caxias/RJ
    5835: CodIBGE := 3301801; // Engenheiro Paulo De Frontin/RJ
    2907: CodIBGE := 3301850; // Guapimirim/RJ
    0774: CodIBGE := 3301876; // Iguaba Grande/RJ
    5837: CodIBGE := 3301900; // Itaborai/RJ
    5839: CodIBGE := 3302007; // Itaguai/RJ
    5929: CodIBGE := 3302056; // Italva/RJ
    5841: CodIBGE := 3302106; // Itaocara/RJ
    5843: CodIBGE := 3302205; // Itaperuna/RJ
    6003: CodIBGE := 3302254; // Itatiaia/RJ
    2913: CodIBGE := 3302270; // Japeri/RJ
    5845: CodIBGE := 3302304; // Laje Do Muriae/RJ
    5847: CodIBGE := 3302403; // Macae/RJ
    0776: CodIBGE := 3302452; // Macuco/RJ
    5849: CodIBGE := 3302502; // Mage/RJ
    5851: CodIBGE := 3302601; // Mangaratiba/RJ
    5853: CodIBGE := 3302700; // Marica/RJ
    5855: CodIBGE := 3302809; // Mendes/RJ
    1116: CodIBGE := 3302858; // Mesquita/RJ
    5857: CodIBGE := 3302908; // Miguel Pereira/RJ
    5859: CodIBGE := 3303005; // Miracema/RJ
    5861: CodIBGE := 3303104; // Natividade/RJ
    5863: CodIBGE := 3303203; // Nilopolis/RJ
    5865: CodIBGE := 3303302; // Niteroi/RJ
    5867: CodIBGE := 3303401; // Nova Friburgo/RJ
    5869: CodIBGE := 3303500; // Nova Iguacu/RJ
    5871: CodIBGE := 3303609; // Paracambi/RJ
    5873: CodIBGE := 3303708; // Paraiba Do Sul/RJ
    5875: CodIBGE := 3303807; // Parati/RJ
    6005: CodIBGE := 3303856; // Paty Do Alferes/RJ
    5877: CodIBGE := 3303906; // Petropolis/RJ
    0778: CodIBGE := 3303955; // Pinheiral/RJ
    5879: CodIBGE := 3304003; // Pirai/RJ
    5881: CodIBGE := 3304102; // Porciuncula/RJ
    0780: CodIBGE := 3304110; // Porto Real/RJ
    2923: CodIBGE := 3304128; // Quatis/RJ
    2911: CodIBGE := 3304144; // Queimados/RJ
    6007: CodIBGE := 3304151; // Quissama/RJ
    5883: CodIBGE := 3304201; // Resende/RJ
    5885: CodIBGE := 3304300; // Rio Bonito/RJ
    5887: CodIBGE := 3304409; // Rio Claro/RJ
    5889: CodIBGE := 3304508; // Rio Das Flores/RJ
    2921: CodIBGE := 3304524; // Rio Das Ostras/RJ
    6001: CodIBGE := 3304557; // Rio De Janeiro/RJ
    5891: CodIBGE := 3304607; // Santa Maria Madalena/RJ
    5893: CodIBGE := 3304706; // Santo Antonio De Padua/RJ
    0782: CodIBGE := 3304755; // Sao Francisco De Itabapoana/RJ
    5895: CodIBGE := 3304805; // Sao Fidelis/RJ
    5897: CodIBGE := 3304904; // Sao Goncalo/RJ
    5899: CodIBGE := 3305000; // Sao Joao Da Barra/RJ
    5901: CodIBGE := 3305109; // Sao Joao De Meriti/RJ
    0784: CodIBGE := 3305133; // Sao Jose De Uba/RJ
    6009: CodIBGE := 3305158; // Sao Jose Do Vale Do Rio Preto/RJ
    5903: CodIBGE := 3305208; // Sao Pedro Da Aldeia/RJ
    5905: CodIBGE := 3305307; // Sao Sebastiao Do Alto/RJ
    5907: CodIBGE := 3305406; // Sapucaia/RJ
    5909: CodIBGE := 3305505; // Saquarema/RJ
    0786: CodIBGE := 3305554; // Seropedica/RJ
    5911: CodIBGE := 3305604; // Silva Jardim/RJ
    5913: CodIBGE := 3305703; // Sumidouro/RJ
    0788: CodIBGE := 3305752; // Tangua/RJ
    5915: CodIBGE := 3305802; // Teresopolis/RJ
    5917: CodIBGE := 3305901; // Trajano De Morais/RJ
    5919: CodIBGE := 3306008; // Tres Rios/RJ
    5921: CodIBGE := 3306107; // Valenca/RJ
    2917: CodIBGE := 3306156; // Varre-Sai/RJ
    5923: CodIBGE := 3306206; // Vassouras/RJ
    5925: CodIBGE := 3306305; // Volta Redonda/RJ
    6101: CodIBGE := 3500105; // Adamantina/SP
    6103: CodIBGE := 3500204; // Adolfo/SP
    6105: CodIBGE := 3500303; // Aguai/SP
    6107: CodIBGE := 3500402; // Aguas Da Prata/SP
    6109: CodIBGE := 3500501; // Aguas De Lindoia/SP
    7019: CodIBGE := 3500550; // Aguas De Santa Barbara/SP
    6111: CodIBGE := 3500600; // Aguas De Sao Pedro/SP
    6113: CodIBGE := 3500709; // Agudos/SP
    2995: CodIBGE := 3500758; // Alambari/SP
    6115: CodIBGE := 3500808; // Alfredo Marcondes/SP
    6117: CodIBGE := 3500907; // Altair/SP
    6119: CodIBGE := 3501004; // Altinopolis/SP
    6121: CodIBGE := 3501103; // Alto Alegre/SP
    3065: CodIBGE := 3501152; // Aluminio/SP
    6123: CodIBGE := 3501202; // Alvares Florence/SP
    6125: CodIBGE := 3501301; // Alvares Machado/SP
    6127: CodIBGE := 3501400; // Alvaro De Carvalho/SP
    6129: CodIBGE := 3501509; // Alvinlandia/SP
    6131: CodIBGE := 3501608; // Americana/SP
    6133: CodIBGE := 3501707; // Americo Brasiliense/SP
    6135: CodIBGE := 3501806; // Americo De Campos/SP
    6137: CodIBGE := 3501905; // Amparo/SP
    6139: CodIBGE := 3502002; // Analandia/SP
    6141: CodIBGE := 3502101; // Andradina/SP
    6143: CodIBGE := 3502200; // Angatuba/SP
    6145: CodIBGE := 3502309; // Anhembi/SP
    6147: CodIBGE := 3502408; // Anhumas/SP
    6149: CodIBGE := 3502507; // Aparecida/SP
    6151: CodIBGE := 3502606; // Aparecida D Oeste/SP
    6153: CodIBGE := 3502705; // Apiai/SP
    3067: CodIBGE := 3502754; // Aracariguama/SP
    6155: CodIBGE := 3502804; // Aracatuba/SP
    6157: CodIBGE := 3502903; // Aracoiaba Da Serra/SP
    6159: CodIBGE := 3503000; // Aramina/SP
    6161: CodIBGE := 3503109; // Arandu/SP
    2991: CodIBGE := 3503158; // Arapei/SP
    6163: CodIBGE := 3503208; // Araraquara/SP
    6165: CodIBGE := 3503307; // Araras/SP
    0790: CodIBGE := 3503356; // Arco-Iris/SP
    6167: CodIBGE := 3503406; // Arealva/SP
    6169: CodIBGE := 3503505; // Areias/SP
    6171: CodIBGE := 3503604; // Areiopolis/SP
    6173: CodIBGE := 3503703; // Ariranha/SP
    6175: CodIBGE := 3503802; // Artur Nogueira/SP
    6177: CodIBGE := 3503901; // Aruja/SP
    2981: CodIBGE := 3503950; // Aspasia/SP
    6179: CodIBGE := 3504008; // Assis/SP
    6181: CodIBGE := 3504107; // Atibaia/SP
    6183: CodIBGE := 3504206; // Auriflama/SP
    6185: CodIBGE := 3504305; // Avai/SP
    6187: CodIBGE := 3504404; // Avanhandava/SP
    6189: CodIBGE := 3504503; // Avare/SP
    6191: CodIBGE := 3504602; // Bady Bassitt/SP
    6193: CodIBGE := 3504701; // Balbinos/SP
    6195: CodIBGE := 3504800; // Balsamo/SP
    6197: CodIBGE := 3504909; // Bananal/SP
    6201: CodIBGE := 3505005; // Barao De Antonina/SP
    6199: CodIBGE := 3505104; // Barbosa/SP
    6203: CodIBGE := 3505203; // Bariri/SP
    6205: CodIBGE := 3505302; // Barra Bonita/SP
    2997: CodIBGE := 3505351; // Barra Do Chapeu/SP
    6207: CodIBGE := 3505401; // Barra Do Turvo/SP
    6209: CodIBGE := 3505500; // Barretos/SP
    6211: CodIBGE := 3505609; // Barrinha/SP
    6213: CodIBGE := 3505708; // Barueri/SP
    6215: CodIBGE := 3505807; // Bastos/SP
    6217: CodIBGE := 3505906; // Batatais/SP
    6219: CodIBGE := 3506003; // Bauru/SP
    6221: CodIBGE := 3506102; // Bebedouro/SP
    6223: CodIBGE := 3506201; // Bento De Abreu/SP
    6225: CodIBGE := 3506300; // Bernardino De Campos/SP
    2965: CodIBGE := 3506359; // Bertioga/SP
    6227: CodIBGE := 3506409; // Bilac/SP
    6229: CodIBGE := 3506508; // Birigui/SP
    6231: CodIBGE := 3506607; // Biritiba-Mirim/SP
    6233: CodIBGE := 3506706; // Boa Esperanca Do Sul/SP
    6235: CodIBGE := 3506805; // Bocaina/SP
    6237: CodIBGE := 3506904; // Bofete/SP
    6239: CodIBGE := 3507001; // Boituva/SP
    6241: CodIBGE := 3507100; // Bom Jesus Dos Perdoes/SP
    3059: CodIBGE := 3507159; // Bom Sucesso De Itarare/SP
    6243: CodIBGE := 3507209; // Bora/SP
    6245: CodIBGE := 3507308; // Boraceia/SP
    6247: CodIBGE := 3507407; // Borborema/SP
    7247: CodIBGE := 3507456; // Borebi/SP
    6249: CodIBGE := 3507506; // Botucatu/SP
    6251: CodIBGE := 3507605; // Braganca Paulista/SP
    6255: CodIBGE := 3507704; // Brauna/SP
    0792: CodIBGE := 3507753; // Brejo Alegre/SP
    6257: CodIBGE := 3507803; // Brodowski/SP
    6259: CodIBGE := 3507902; // Brotas/SP
    6261: CodIBGE := 3508009; // Buri/SP
    6263: CodIBGE := 3508108; // Buritama/SP
    6265: CodIBGE := 3508207; // Buritizal/SP
    6267: CodIBGE := 3508306; // Cabralia Paulista/SP
    6269: CodIBGE := 3508405; // Cabreuva/SP
    6271: CodIBGE := 3508504; // Cacapava/SP
    6273: CodIBGE := 3508603; // Cachoeira Paulista/SP
    6275: CodIBGE := 3508702; // Caconde/SP
    6277: CodIBGE := 3508801; // Cafelandia/SP
    6279: CodIBGE := 3508900; // Caiabu/SP
    6281: CodIBGE := 3509007; // Caieiras/SP
    6283: CodIBGE := 3509106; // Caiua/SP
    6285: CodIBGE := 3509205; // Cajamar/SP
    2967: CodIBGE := 3509254; // Cajati/SP
    6287: CodIBGE := 3509304; // Cajobi/SP
    6289: CodIBGE := 3509403; // Cajuru/SP
    2999: CodIBGE := 3509452; // Campina Do Monte Alegre/SP
    6291: CodIBGE := 3509502; // Campinas/SP
    6293: CodIBGE := 3509601; // Campo Limpo Paulista/SP
    6295: CodIBGE := 3509700; // Campos Do Jordao/SP
    6297: CodIBGE := 3509809; // Campos Novos Paulista/SP
    6299: CodIBGE := 3509908; // Cananeia/SP
    0794: CodIBGE := 3509957; // Canas/SP
    6301: CodIBGE := 3510005; // Candido Mota/SP
    6303: CodIBGE := 3510104; // Candido Rodrigues/SP
    2947: CodIBGE := 3510153; // Canitar/SP
    6305: CodIBGE := 3510203; // Capao Bonito/SP
    6307: CodIBGE := 3510302; // Capela Do Alto/SP
    6309: CodIBGE := 3510401; // Capivari/SP
    6311: CodIBGE := 3510500; // Caraguatatuba/SP
    6313: CodIBGE := 3510609; // Carapicuiba/SP
    6315: CodIBGE := 3510708; // Cardoso/SP
    6317: CodIBGE := 3510807; // Casa Branca/SP
    6319: CodIBGE := 3510906; // Cassia Dos Coqueiros/SP
    6321: CodIBGE := 3511003; // Castilho/SP
    6323: CodIBGE := 3511102; // Catanduva/SP
    6325: CodIBGE := 3511201; // Catigua/SP
    6327: CodIBGE := 3511300; // Cedral/SP
    6329: CodIBGE := 3511409; // Cerqueira Cesar/SP
    6331: CodIBGE := 3511508; // Cerquilho/SP
    6333: CodIBGE := 3511607; // Cesario Lange/SP
    6335: CodIBGE := 3511706; // Charqueada/SP
    6339: CodIBGE := 3511904; // Clementina/SP
    6341: CodIBGE := 3512001; // Colina/SP
    6343: CodIBGE := 3512100; // Colombia/SP
    6345: CodIBGE := 3512209; // Conchal/SP
    6347: CodIBGE := 3512308; // Conchas/SP
    6349: CodIBGE := 3512407; // Cordeiropolis/SP
    6351: CodIBGE := 3512506; // Coroados/SP
    6353: CodIBGE := 3512605; // Coronel Macedo/SP
    6355: CodIBGE := 3512704; // Corumbatai/SP
    6357: CodIBGE := 3512803; // Cosmopolis/SP
    6359: CodIBGE := 3512902; // Cosmorama/SP
    6361: CodIBGE := 3513009; // Cotia/SP
    6363: CodIBGE := 3513108; // Cravinhos/SP
    6365: CodIBGE := 3513207; // Cristais Paulista/SP
    6367: CodIBGE := 3513306; // Cruzalia/SP
    6369: CodIBGE := 3513405; // Cruzeiro/SP
    6371: CodIBGE := 3513504; // Cubatao/SP
    6373: CodIBGE := 3513603; // Cunha/SP
    6375: CodIBGE := 3513702; // Descalvado/SP
    6377: CodIBGE := 3513801; // Diadema/SP
    7249: CodIBGE := 3513850; // Dirce Reis/SP
    6379: CodIBGE := 3513900; // Divinolandia/SP
    6381: CodIBGE := 3514007; // Dobrada/SP
    6383: CodIBGE := 3514106; // Dois Corregos/SP
    6385: CodIBGE := 3514205; // Dolcinopolis/SP
    6387: CodIBGE := 3514304; // Dourado/SP
    6389: CodIBGE := 3514403; // Dracena/SP
    6391: CodIBGE := 3514502; // Duartina/SP
    6393: CodIBGE := 3514601; // Dumont/SP
    6395: CodIBGE := 3514700; // Echapora/SP
    6397: CodIBGE := 3514809; // Eldorado/SP
    6399: CodIBGE := 3514908; // Elias Fausto/SP
    2975: CodIBGE := 3514924; // Elisiario/SP
    7251: CodIBGE := 3514957; // Embauba/SP
    6401: CodIBGE := 3515004; // Embu/SP
    6403: CodIBGE := 3515103; // Embu-Guacu/SP
    2961: CodIBGE := 3515129; // Emilianopolis/SP
    2949: CodIBGE := 3515152; // Engenheiro Coelho/SP
    6865: CodIBGE := 3515186; // Espirito Santo Do Pinhal/SP
    7253: CodIBGE := 3515194; // Espirito Santo Do Turvo/SP
    6405: CodIBGE := 3515202; // Estrela D Oeste/SP
    6407: CodIBGE := 3515301; // Estrela Do Norte/SP
    7255: CodIBGE := 3515350; // Euclides Da Cunha Paulista/SP
    6409: CodIBGE := 3515400; // Fartura/SP
    6411: CodIBGE := 3515509; // Fernandopolis/SP
    6413: CodIBGE := 3515608; // Fernando Prestes/SP
    0796: CodIBGE := 3515657; // Fernao/SP
    6415: CodIBGE := 3515707; // Ferraz De Vasconcelos/SP
    6417: CodIBGE := 3515806; // Flora Rica/SP
    6419: CodIBGE := 3515905; // Floreal/SP
    6421: CodIBGE := 3516002; // Florida Paulista/SP
    6423: CodIBGE := 3516101; // Florinia/SP
    6425: CodIBGE := 3516200; // Franca/SP
    6427: CodIBGE := 3516309; // Francisco Morato/SP
    6429: CodIBGE := 3516408; // Franco Da Rocha/SP
    6431: CodIBGE := 3516507; // Gabriel Monteiro/SP
    6433: CodIBGE := 3516606; // Galia/SP
    6435: CodIBGE := 3516705; // Garca/SP
    6437: CodIBGE := 3516804; // Gastao Vidigal/SP
    0798: CodIBGE := 3516853; // Gaviao Peixoto/SP
    6439: CodIBGE := 3516903; // General Salgado/SP
    6441: CodIBGE := 3517000; // Getulina/SP
    6443: CodIBGE := 3517109; // Glicerio/SP
    6445: CodIBGE := 3517208; // Guaicara/SP
    6447: CodIBGE := 3517307; // Guaimbe/SP
    6449: CodIBGE := 3517406; // Guaira/SP
    6451: CodIBGE := 3517505; // Guapiacu/SP
    6453: CodIBGE := 3517604; // Guapiara/SP
    6455: CodIBGE := 3517703; // Guara/SP
    6457: CodIBGE := 3517802; // Guaracai/SP
    6459: CodIBGE := 3517901; // Guaraci/SP
    6461: CodIBGE := 3518008; // Guarani D Oeste/SP
    6463: CodIBGE := 3518107; // Guaranta/SP
    6465: CodIBGE := 3518206; // Guararapes/SP
    6467: CodIBGE := 3518305; // Guararema/SP
    6469: CodIBGE := 3518404; // Guaratingueta/SP
    6471: CodIBGE := 3518503; // Guarei/SP
    6473: CodIBGE := 3518602; // Guariba/SP
    6475: CodIBGE := 3518701; // Guaruja/SP
    6477: CodIBGE := 3518800; // Guarulhos/SP
    7257: CodIBGE := 3518859; // Guatapara/SP
    6479: CodIBGE := 3518909; // Guzolandia/SP
    6481: CodIBGE := 3519006; // Herculandia/SP
    2953: CodIBGE := 3519055; // Holambra/SP
    2951: CodIBGE := 3519071; // Hortolandia/SP
    6483: CodIBGE := 3519105; // Iacanga/SP
    6485: CodIBGE := 3519204; // Iacri/SP
    7259: CodIBGE := 3519253; // Iaras/SP
    6487: CodIBGE := 3519303; // Ibate/SP
    6489: CodIBGE := 3519402; // Ibira/SP
    6491: CodIBGE := 3519501; // Ibirarema/SP
    6493: CodIBGE := 3519600; // Ibitinga/SP
    6495: CodIBGE := 3519709; // Ibiuna/SP
    6497: CodIBGE := 3519808; // Icem/SP
    6499: CodIBGE := 3519907; // Iepe/SP
    6501: CodIBGE := 3520004; // Igaracu Do Tiete/SP
    6503: CodIBGE := 3520103; // Igarapava/SP
    6505: CodIBGE := 3520202; // Igarata/SP
    6507: CodIBGE := 3520301; // Iguape/SP
    6509: CodIBGE := 3520400; // Ilhabela/SP
    2969: CodIBGE := 3520426; // Ilha Comprida/SP
    2943: CodIBGE := 3520442; // Ilha Solteira/SP
    6511: CodIBGE := 3520509; // Indaiatuba/SP
    6513: CodIBGE := 3520608; // Indiana/SP
    6515: CodIBGE := 3520707; // Indiapora/SP
    6517: CodIBGE := 3520806; // Inubia Paulista/SP
    6519: CodIBGE := 3520905; // Ipaussu/SP
    6521: CodIBGE := 3521002; // Ipero/SP
    6523: CodIBGE := 3521101; // Ipeuna/SP
    0800: CodIBGE := 3521150; // Ipigua/SP
    6525: CodIBGE := 3521200; // Iporanga/SP
    6527: CodIBGE := 3521309; // Ipua/SP
    6529: CodIBGE := 3521408; // Iracemapolis/SP
    6531: CodIBGE := 3521507; // Irapua/SP
    6533: CodIBGE := 3521606; // Irapuru/SP
    6535: CodIBGE := 3521705; // Itabera/SP
    6537: CodIBGE := 3521804; // Itai/SP
    6539: CodIBGE := 3521903; // Itajobi/SP
    6541: CodIBGE := 3522000; // Itaju/SP
    6543: CodIBGE := 3522109; // Itanhaem/SP
    3053: CodIBGE := 3522158; // Itaoca/SP
    6545: CodIBGE := 3522208; // Itapecerica Da Serra/SP
    6547: CodIBGE := 3522307; // Itapetininga/SP
    6549: CodIBGE := 3522406; // Itapeva/SP
    6551: CodIBGE := 3522505; // Itapevi/SP
    6553: CodIBGE := 3522604; // Itapira/SP
    3055: CodIBGE := 3522653; // Itapirapua Paulista/SP
    6555: CodIBGE := 3522703; // Itapolis/SP
    6557: CodIBGE := 3522802; // Itaporanga/SP
    6559: CodIBGE := 3522901; // Itapui/SP
    6561: CodIBGE := 3523008; // Itapura/SP
    6563: CodIBGE := 3523107; // Itaquaquecetuba/SP
    6565: CodIBGE := 3523206; // Itarare/SP
    6567: CodIBGE := 3523305; // Itariri/SP
    6569: CodIBGE := 3523404; // Itatiba/SP
    6571: CodIBGE := 3523503; // Itatinga/SP
    6573: CodIBGE := 3523602; // Itirapina/SP
    6575: CodIBGE := 3523701; // Itirapua/SP
    6577: CodIBGE := 3523800; // Itobi/SP
    6579: CodIBGE := 3523909; // Itu/SP
    6581: CodIBGE := 3524006; // Itupeva/SP
    6583: CodIBGE := 3524105; // Ituverava/SP
    6585: CodIBGE := 3524204; // Jaborandi/SP
    6587: CodIBGE := 3524303; // Jaboticabal/SP
    6589: CodIBGE := 3524402; // Jacarei/SP
    6591: CodIBGE := 3524501; // Jaci/SP
    6593: CodIBGE := 3524600; // Jacupiranga/SP
    6595: CodIBGE := 3524709; // Jaguariuna/SP
    6597: CodIBGE := 3524808; // Jales/SP
    6599: CodIBGE := 3524907; // Jambeiro/SP
    6601: CodIBGE := 3525003; // Jandira/SP
    6603: CodIBGE := 3525102; // Jardinopolis/SP
    6605: CodIBGE := 3525201; // Jarinu/SP
    6607: CodIBGE := 3525300; // Jau/SP
    6609: CodIBGE := 3525409; // Jeriquara/SP
    6611: CodIBGE := 3525508; // Joanopolis/SP
    6613: CodIBGE := 3525607; // Joao Ramalho/SP
    6615: CodIBGE := 3525706; // Jose Bonifacio/SP
    6617: CodIBGE := 3525805; // Julio Mesquita/SP
    0802: CodIBGE := 3525854; // Jumirim/SP
    6619: CodIBGE := 3525904; // Jundiai/SP
    6621: CodIBGE := 3526001; // Junqueiropolis/SP
    6623: CodIBGE := 3526100; // Juquia/SP
    6625: CodIBGE := 3526209; // Juquitiba/SP
    6627: CodIBGE := 3526308; // Lagoinha/SP
    6629: CodIBGE := 3526407; // Laranjal Paulista/SP
    6631: CodIBGE := 3526506; // Lavinia/SP
    6633: CodIBGE := 3526605; // Lavrinhas/SP
    6635: CodIBGE := 3526704; // Leme/SP
    6637: CodIBGE := 3526803; // Lencois Paulista/SP
    6639: CodIBGE := 3526902; // Limeira/SP
    6641: CodIBGE := 3527009; // Lindoia/SP
    6643: CodIBGE := 3527108; // Lins/SP
    6645: CodIBGE := 3527207; // Lorena/SP
    2937: CodIBGE := 3527256; // Lourdes/SP
    6647: CodIBGE := 3527306; // Louveira/SP
    6649: CodIBGE := 3527405; // Lucelia/SP
    6651: CodIBGE := 3527504; // Lucianopolis/SP
    6653: CodIBGE := 3527603; // Luis Antonio/SP
    6655: CodIBGE := 3527702; // Luiziania/SP
    6657: CodIBGE := 3527801; // Lupercio/SP
    6659: CodIBGE := 3527900; // Lutecia/SP
    6661: CodIBGE := 3528007; // Macatuba/SP
    6663: CodIBGE := 3528106; // Macaubal/SP
    6665: CodIBGE := 3528205; // Macedonia/SP
    6667: CodIBGE := 3528304; // Magda/SP
    6669: CodIBGE := 3528403; // Mairinque/SP
    6671: CodIBGE := 3528502; // Mairipora/SP
    6673: CodIBGE := 3528601; // Manduri/SP
    6675: CodIBGE := 3528700; // Maraba Paulista/SP
    6677: CodIBGE := 3528809; // Maracai/SP
    2977: CodIBGE := 3528858; // Marapoama/SP
    6679: CodIBGE := 3528908; // Mariapolis/SP
    6681: CodIBGE := 3529005; // Marilia/SP
    6683: CodIBGE := 3529104; // Marinopolis/SP
    6685: CodIBGE := 3529203; // Martinopolis/SP
    6687: CodIBGE := 3529302; // Matao/SP
    6689: CodIBGE := 3529401; // Maua/SP
    6691: CodIBGE := 3529500; // Mendonca/SP
    6693: CodIBGE := 3529609; // Meridiano/SP
    2983: CodIBGE := 3529658; // Mesopolis/SP
    6695: CodIBGE := 3529708; // Miguelopolis/SP
    6697: CodIBGE := 3529807; // Mineiros Do Tiete/SP
    6699: CodIBGE := 3529906; // Miracatu/SP
    6701: CodIBGE := 3530003; // Mira Estrela/SP
    6703: CodIBGE := 3530102; // Mirandopolis/SP
    6705: CodIBGE := 3530201; // Mirante Do Paranapanema/SP
    6707: CodIBGE := 3530300; // Mirassol/SP
    6709: CodIBGE := 3530409; // Mirassolandia/SP
    6711: CodIBGE := 3530508; // Mococa/SP
    6713: CodIBGE := 3530607; // Mogi Das Cruzes/SP
    6715: CodIBGE := 3530706; // Mogi Guacu/SP
    6717: CodIBGE := 3530805; // Mogi Mirim/SP
    6719: CodIBGE := 3530904; // Mombuca/SP
    6721: CodIBGE := 3531001; // Moncoes/SP
    6723: CodIBGE := 3531100; // Mongagua/SP
    6725: CodIBGE := 3531209; // Monte Alegre Do Sul/SP
    6727: CodIBGE := 3531308; // Monte Alto/SP
    6729: CodIBGE := 3531407; // Monte Aprazivel/SP
    6731: CodIBGE := 3531506; // Monte Azul Paulista/SP
    6733: CodIBGE := 3531605; // Monte Castelo/SP
    6735: CodIBGE := 3531704; // Monteiro Lobato/SP
    6737: CodIBGE := 3531803; // Monte Mor/SP
    6739: CodIBGE := 3531902; // Morro Agudo/SP
    6741: CodIBGE := 3532009; // Morungaba/SP
    7263: CodIBGE := 3532058; // Motuca/SP
    6743: CodIBGE := 3532108; // Murutinga Do Sul/SP
    0804: CodIBGE := 3532157; // Nantes/SP
    6745: CodIBGE := 3532207; // Narandiba/SP
    6747: CodIBGE := 3532306; // Natividade Da Serra/SP
    6749: CodIBGE := 3532405; // Nazare Paulista/SP
    6751: CodIBGE := 3532504; // Neves Paulista/SP
    6753: CodIBGE := 3532603; // Nhandeara/SP
    6755: CodIBGE := 3532702; // Nipoa/SP
    6757: CodIBGE := 3532801; // Nova Alianca/SP
    3061: CodIBGE := 3532827; // Nova Campina/SP
    2985: CodIBGE := 3532843; // Nova Canaa Paulista/SP
    0806: CodIBGE := 3532868; // Nova Castilho/SP
    6759: CodIBGE := 3532900; // Nova Europa/SP
    6761: CodIBGE := 3533007; // Nova Granada/SP
    6763: CodIBGE := 3533106; // Nova Guataporanga/SP
    6765: CodIBGE := 3533205; // Nova Independencia/SP
    2979: CodIBGE := 3533254; // Novais/SP
    6767: CodIBGE := 3533304; // Nova Luzitania/SP
    6769: CodIBGE := 3533403; // Nova Odessa/SP
    6771: CodIBGE := 3533502; // Novo Horizonte/SP
    6773: CodIBGE := 3533601; // Nuporanga/SP
    6775: CodIBGE := 3533700; // Ocaucu/SP
    6777: CodIBGE := 3533809; // Oleo/SP
    6779: CodIBGE := 3533908; // Olimpia/SP
    6781: CodIBGE := 3534005; // Onda Verde/SP
    6783: CodIBGE := 3534104; // Oriente/SP
    6785: CodIBGE := 3534203; // Orindiuva/SP
    6787: CodIBGE := 3534302; // Orlandia/SP
    6789: CodIBGE := 3534401; // Osasco/SP
    6791: CodIBGE := 3534500; // Oscar Bressane/SP
    6793: CodIBGE := 3534609; // Osvaldo Cruz/SP
    6795: CodIBGE := 3534708; // Ourinhos/SP
    0808: CodIBGE := 3534757; // Ouroeste/SP
    6797: CodIBGE := 3534807; // Ouro Verde/SP
    6799: CodIBGE := 3534906; // Pacaembu/SP
    6801: CodIBGE := 3535002; // Palestina/SP
    6803: CodIBGE := 3535101; // Palmares Paulista/SP
    6805: CodIBGE := 3535200; // Palmeira D Oeste/SP
    6807: CodIBGE := 3535309; // Palmital/SP
    6809: CodIBGE := 3535408; // Panorama/SP
    6811: CodIBGE := 3535507; // Paraguacu Paulista/SP
    6813: CodIBGE := 3535606; // Paraibuna/SP
    6815: CodIBGE := 3535705; // Paraiso/SP
    6817: CodIBGE := 3535804; // Paranapanema/SP
    6819: CodIBGE := 3535903; // Paranapua/SP
    6821: CodIBGE := 3536000; // Parapua/SP
    6823: CodIBGE := 3536109; // Pardinho/SP
    6825: CodIBGE := 3536208; // Pariquera-Acu/SP
    2989: CodIBGE := 3536257; // Parisi/SP
    6827: CodIBGE := 3536307; // Patrocinio Paulista/SP
    6829: CodIBGE := 3536406; // Pauliceia/SP
    6831: CodIBGE := 3536505; // Paulinia/SP
    0810: CodIBGE := 3536570; // Paulistania/SP
    6833: CodIBGE := 3536604; // Paulo De Faria/SP
    6835: CodIBGE := 3536703; // Pederneiras/SP
    6837: CodIBGE := 3536802; // Pedra Bela/SP
    6839: CodIBGE := 3536901; // Pedranopolis/SP
    6841: CodIBGE := 3537008; // Pedregulho/SP
    6843: CodIBGE := 3537107; // Pedreira/SP
    2963: CodIBGE := 3537156; // Pedrinhas Paulista/SP
    6845: CodIBGE := 3537206; // Pedro De Toledo/SP
    6847: CodIBGE := 3537305; // Penapolis/SP
    6849: CodIBGE := 3537404; // Pereira Barreto/SP
    6851: CodIBGE := 3537503; // Pereiras/SP
    6853: CodIBGE := 3537602; // Peruibe/SP
    6855: CodIBGE := 3537701; // Piacatu/SP
    6857: CodIBGE := 3537800; // Piedade/SP
    6859: CodIBGE := 3537909; // Pilar Do Sul/SP
    6861: CodIBGE := 3538006; // Pindamonhangaba/SP
    6863: CodIBGE := 3538105; // Pindorama/SP
    6867: CodIBGE := 3538204; // Pinhalzinho/SP
    6869: CodIBGE := 3538303; // Piquerobi/SP
    6871: CodIBGE := 3538501; // Piquete/SP
    6873: CodIBGE := 3538600; // Piracaia/SP
    6875: CodIBGE := 3538709; // Piracicaba/SP
    6877: CodIBGE := 3538808; // Piraju/SP
    6879: CodIBGE := 3538907; // Pirajui/SP
    6881: CodIBGE := 3539004; // Pirangi/SP
    6883: CodIBGE := 3539103; // Pirapora Do Bom Jesus/SP
    6885: CodIBGE := 3539202; // Pirapozinho/SP
    6887: CodIBGE := 3539301; // Pirassununga/SP
    6889: CodIBGE := 3539400; // Piratininga/SP
    6891: CodIBGE := 3539509; // Pitangueiras/SP
    6893: CodIBGE := 3539608; // Planalto/SP
    6895: CodIBGE := 3539707; // Platina/SP
    6897: CodIBGE := 3539806; // Poa/SP
    6899: CodIBGE := 3539905; // Poloni/SP
    6901: CodIBGE := 3540002; // Pompeia/SP
    6903: CodIBGE := 3540101; // Pongai/SP
    6905: CodIBGE := 3540200; // Pontal/SP
    2987: CodIBGE := 3540259; // Pontalinda/SP
    6907: CodIBGE := 3540309; // Pontes Gestal/SP
    6909: CodIBGE := 3540408; // Populina/SP
    6911: CodIBGE := 3540507; // Porangaba/SP
    6913: CodIBGE := 3540606; // Porto Feliz/SP
    6915: CodIBGE := 3540705; // Porto Ferreira/SP
    2993: CodIBGE := 3540754; // Potim/SP
    6917: CodIBGE := 3540804; // Potirendaba/SP
    0812: CodIBGE := 3540853; // Pracinha/SP
    6919: CodIBGE := 3540903; // Pradopolis/SP
    6921: CodIBGE := 3541000; // Praia Grande/SP
    0814: CodIBGE := 3541059; // Pratania/SP
    6923: CodIBGE := 3541109; // Presidente Alves/SP
    6925: CodIBGE := 3541208; // Presidente Bernardes/SP
    6927: CodIBGE := 3541307; // Presidente Epitacio/SP
    6929: CodIBGE := 3541406; // Presidente Prudente/SP
    6931: CodIBGE := 3541505; // Presidente Venceslau/SP
    6933: CodIBGE := 3541604; // Promissao/SP
    0816: CodIBGE := 3541653; // Quadra/SP
    6935: CodIBGE := 3541703; // Quata/SP
    6937: CodIBGE := 3541802; // Queiroz/SP
    6939: CodIBGE := 3541901; // Queluz/SP
    6941: CodIBGE := 3542008; // Quintana/SP
    6943: CodIBGE := 3542107; // Rafard/SP
    6945: CodIBGE := 3542206; // Rancharia/SP
    6947: CodIBGE := 3542305; // Redencao Da Serra/SP
    6949: CodIBGE := 3542404; // Regente Feijo/SP
    6951: CodIBGE := 3542503; // Reginopolis/SP
    6953: CodIBGE := 3542602; // Registro/SP
    6955: CodIBGE := 3542701; // Restinga/SP
    6957: CodIBGE := 3542800; // Ribeira/SP
    6959: CodIBGE := 3542909; // Ribeirao Bonito/SP
    6961: CodIBGE := 3543006; // Ribeirao Branco/SP
    6963: CodIBGE := 3543105; // Ribeirao Corrente/SP
    6965: CodIBGE := 3543204; // Ribeirao Do Sul/SP
    0818: CodIBGE := 3543238; // Ribeirao Dos Indios/SP
    3057: CodIBGE := 3543253; // Ribeirao Grande/SP
    6967: CodIBGE := 3543303; // Ribeirao Pires/SP
    6969: CodIBGE := 3543402; // Ribeirao Preto/SP
    6971: CodIBGE := 3543501; // Riversul/SP
    6973: CodIBGE := 3543600; // Rifaina/SP
    6975: CodIBGE := 3543709; // Rincao/SP
    6977: CodIBGE := 3543808; // Rinopolis/SP
    6979: CodIBGE := 3543907; // Rio Claro/SP
    6981: CodIBGE := 3544004; // Rio Das Pedras/SP
    6983: CodIBGE := 3544103; // Rio Grande Da Serra/SP
    6985: CodIBGE := 3544202; // Riolandia/SP
    7265: CodIBGE := 3544251; // Rosana/SP
    6987: CodIBGE := 3544301; // Roseira/SP
    6989: CodIBGE := 3544400; // Rubiacea/SP
    6991: CodIBGE := 3544509; // Rubineia/SP
    6993: CodIBGE := 3544608; // Sabino/SP
    6995: CodIBGE := 3544707; // Sagres/SP
    6997: CodIBGE := 3544806; // Sales/SP
    6999: CodIBGE := 3544905; // Sales Oliveira/SP
    7001: CodIBGE := 3545001; // Salesopolis/SP
    7003: CodIBGE := 3545100; // Salmourao/SP
    5445: CodIBGE := 3545159; // Saltinho/SP
    7005: CodIBGE := 3545209; // Salto/SP
    7007: CodIBGE := 3545308; // Salto De Pirapora/SP
    7009: CodIBGE := 3545407; // Salto Grande/SP
    7011: CodIBGE := 3545506; // Sandovalina/SP
    7013: CodIBGE := 3545605; // Santa Adelia/SP
    7015: CodIBGE := 3545704; // Santa Albertina/SP
    7017: CodIBGE := 3545803; // Santa Barbara D Oeste/SP
    7021: CodIBGE := 3546009; // Santa Branca/SP
    7023: CodIBGE := 3546108; // Santa Clara D Oeste/SP
    7025: CodIBGE := 3546207; // Santa Cruz Da Conceicao/SP
    0820: CodIBGE := 3546256; // Santa Cruz Da Esperanca/SP
    7027: CodIBGE := 3546306; // Santa Cruz Das Palmeiras/SP
    7029: CodIBGE := 3546405; // Santa Cruz Do Rio Pardo/SP
    7031: CodIBGE := 3546504; // Santa Ernestina/SP
    7033: CodIBGE := 3546603; // Santa Fe Do Sul/SP
    7035: CodIBGE := 3546702; // Santa Gertrudes/SP
    7037: CodIBGE := 3546801; // Santa Isabel/SP
    7039: CodIBGE := 3546900; // Santa Lucia/SP
    7041: CodIBGE := 3547007; // Santa Maria Da Serra/SP
    7043: CodIBGE := 3547106; // Santa Mercedes/SP
    7045: CodIBGE := 3547205; // Santana Da Ponte Pensa/SP
    7047: CodIBGE := 3547304; // Santana De Parnaiba/SP
    7049: CodIBGE := 3547403; // Santa Rita D Oeste/SP
    7051: CodIBGE := 3547502; // Santa Rita Do Passa Quatro/SP
    7053: CodIBGE := 3547601; // Santa Rosa De Viterbo/SP
    0822: CodIBGE := 3547650; // Santa Salete/SP
    7055: CodIBGE := 3547700; // Santo Anastacio/SP
    7057: CodIBGE := 3547809; // Santo Andre/SP
    7059: CodIBGE := 3547908; // Santo Antonio Da Alegria/SP
    7061: CodIBGE := 3548005; // Santo Antonio De Posse/SP
    2939: CodIBGE := 3548054; // Santo Antonio Do Aracangua/SP
    7063: CodIBGE := 3548104; // Santo Antonio Do Jardim/SP
    7065: CodIBGE := 3548203; // Santo Antonio Do Pinhal/SP
    7067: CodIBGE := 3548302; // Santo Expedito/SP
    7069: CodIBGE := 3548401; // Santopolis Do Aguapei/SP
    7071: CodIBGE := 3548500; // Santos/SP
    7073: CodIBGE := 3548609; // Sao Bento Do Sapucai/SP
    7075: CodIBGE := 3548708; // Sao Bernardo Do Campo/SP
    7077: CodIBGE := 3548807; // Sao Caetano Do Sul/SP
    7079: CodIBGE := 3548906; // Sao Carlos/SP
    7081: CodIBGE := 3549003; // Sao Francisco/SP
    7083: CodIBGE := 3549102; // Sao Joao Da Boa Vista/SP
    7085: CodIBGE := 3549201; // Sao Joao Das Duas Pontes/SP
    2941: CodIBGE := 3549250; // Sao Joao De Iracema/SP
    7087: CodIBGE := 3549300; // Sao Joao Do Pau D Alho/SP
    7089: CodIBGE := 3549409; // Sao Joaquim Da Barra/SP
    7091: CodIBGE := 3549508; // Sao Jose Da Bela Vista/SP
    7093: CodIBGE := 3549607; // Sao Jose Do Barreiro/SP
    7095: CodIBGE := 3549706; // Sao Jose Do Rio Pardo/SP
    7097: CodIBGE := 3549805; // Sao Jose Do Rio Preto/SP
    7099: CodIBGE := 3549904; // Sao Jose Dos Campos/SP
    5447: CodIBGE := 3549953; // Sao Lourenco Da Serra/SP
    7101: CodIBGE := 3550001; // Sao Luis Do Paraitinga/SP
    7103: CodIBGE := 3550100; // Sao Manuel/SP
    7105: CodIBGE := 3550209; // Sao Miguel Arcanjo/SP
    7107: CodIBGE := 3550308; // Sao Paulo/SP
    7109: CodIBGE := 3550407; // Sao Pedro/SP
    7111: CodIBGE := 3550506; // Sao Pedro Do Turvo/SP
    7113: CodIBGE := 3550605; // Sao Roque/SP
    7115: CodIBGE := 3550704; // Sao Sebastiao/SP
    7117: CodIBGE := 3550803; // Sao Sebastiao Da Grama/SP
    7119: CodIBGE := 3550902; // Sao Simao/SP
    7121: CodIBGE := 3551009; // Sao Vicente/SP
    7123: CodIBGE := 3551108; // Sarapui/SP
    7125: CodIBGE := 3551207; // Sarutaia/SP
    7127: CodIBGE := 3551306; // Sebastianopolis Do Sul/SP
    7129: CodIBGE := 3551405; // Serra Azul/SP
    7131: CodIBGE := 3551504; // Serrana/SP
    7133: CodIBGE := 3551603; // Serra Negra/SP
    7135: CodIBGE := 3551702; // Sertaozinho/SP
    7137: CodIBGE := 3551801; // Sete Barras/SP
    7139: CodIBGE := 3551900; // Severinia/SP
    7141: CodIBGE := 3552007; // Silveiras/SP
    7143: CodIBGE := 3552106; // Socorro/SP
    7145: CodIBGE := 3552205; // Sorocaba/SP
    7147: CodIBGE := 3552304; // Sud Mennucci/SP
    7149: CodIBGE := 3552403; // Sumare/SP
    7151: CodIBGE := 3552502; // Suzano/SP
    2945: CodIBGE := 3552551; // Suzanapolis/SP
    7153: CodIBGE := 3552601; // Tabapua/SP
    7155: CodIBGE := 3552700; // Tabatinga/SP
    7157: CodIBGE := 3552809; // Taboao Da Serra/SP
    7159: CodIBGE := 3552908; // Taciba/SP
    7161: CodIBGE := 3553005; // Taguai/SP
    7163: CodIBGE := 3553104; // Taiacu/SP
    7165: CodIBGE := 3553203; // Taiuva/SP
    7167: CodIBGE := 3553302; // Tambau/SP
    7169: CodIBGE := 3553401; // Tanabi/SP
    7171: CodIBGE := 3553500; // Tapirai/SP
    7173: CodIBGE := 3553609; // Tapiratiba/SP
    0824: CodIBGE := 3553658; // Taquaral/SP
    7175: CodIBGE := 3553708; // Taquaritinga/SP
    7177: CodIBGE := 3553807; // Taquarituba/SP
    3063: CodIBGE := 3553856; // Taquarivai/SP
    7179: CodIBGE := 3553906; // Tarabai/SP
    7267: CodIBGE := 3553955; // Taruma/SP
    7181: CodIBGE := 3554003; // Tatui/SP
    7183: CodIBGE := 3554102; // Taubate/SP
    7185: CodIBGE := 3554201; // Tejupa/SP
    7187: CodIBGE := 3554300; // Teodoro Sampaio/SP
    7189: CodIBGE := 3554409; // Terra Roxa/SP
    7191: CodIBGE := 3554508; // Tiete/SP
    7193: CodIBGE := 3554607; // Timburi/SP
    3227: CodIBGE := 3554656; // Torre De Pedra/SP
    7195: CodIBGE := 3554706; // Torrinha/SP
    0826: CodIBGE := 3554755; // Trabiju/SP
    7197: CodIBGE := 3554805; // Tremembe/SP
    7199: CodIBGE := 3554904; // Tres Fronteiras/SP
    2955: CodIBGE := 3554953; // Tuiuti/SP
    7201: CodIBGE := 3555000; // Tupa/SP
    7203: CodIBGE := 3555109; // Tupi Paulista/SP
    7205: CodIBGE := 3555208; // Turiuba/SP
    7207: CodIBGE := 3555307; // Turmalina/SP
    2971: CodIBGE := 3555356; // Ubarana/SP
    7209: CodIBGE := 3555406; // Ubatuba/SP
    7211: CodIBGE := 3555505; // Ubirajara/SP
    7213: CodIBGE := 3555604; // Uchoa/SP
    7215: CodIBGE := 3555703; // Uniao Paulista/SP
    7217: CodIBGE := 3555802; // Urania/SP
    7219: CodIBGE := 3555901; // Uru/SP
    7221: CodIBGE := 3556008; // Urupes/SP
    7223: CodIBGE := 3556107; // Valentim Gentil/SP
    7225: CodIBGE := 3556206; // Valinhos/SP
    7227: CodIBGE := 3556305; // Valparaiso/SP
    2957: CodIBGE := 3556354; // Vargem/SP
    7231: CodIBGE := 3556404; // Vargem Grande Do Sul/SP
    7273: CodIBGE := 3556453; // Vargem Grande Paulista/SP
    7233: CodIBGE := 3556503; // Varzea Paulista/SP
    7235: CodIBGE := 3556602; // Vera Cruz/SP
    7237: CodIBGE := 3556701; // Vinhedo/SP
    7239: CodIBGE := 3556800; // Viradouro/SP
    7241: CodIBGE := 3556909; // Vista Alegre Do Alto/SP
    0828: CodIBGE := 3556958; // Vitoria Brasil/SP
    7243: CodIBGE := 3557006; // Votorantim/SP
    7245: CodIBGE := 3557105; // Votuporanga/SP
    2973: CodIBGE := 3557154; // Zacarias/SP
    6337: CodIBGE := 3557204; // Chavantes/SP
    2959: CodIBGE := 3557303; // Estiva Gerbi/SP
    7401: CodIBGE := 4100103; // Abatia/PR
    7403: CodIBGE := 4100202; // Adrianopolis/PR
    7405: CodIBGE := 4100301; // Agudos Do Sul/PR
    7407: CodIBGE := 4100400; // Almirante Tamandare/PR
    8455: CodIBGE := 4100459; // Altamira Do Parana/PR
    7951: CodIBGE := 4100509; // Altonia/PR
    7409: CodIBGE := 4100608; // Alto Parana/PR
    7411: CodIBGE := 4100707; // Alto Piquiri/PR
    7413: CodIBGE := 4100806; // Alvorada Do Sul/PR
    7415: CodIBGE := 4100905; // Amapora/PR
    7417: CodIBGE := 4101002; // Ampere/PR
    5463: CodIBGE := 4101051; // Anahy/PR
    7419: CodIBGE := 4101101; // Andira/PR
    5509: CodIBGE := 4101150; // Angulo/PR
    7421: CodIBGE := 4101200; // Antonina/PR
    7423: CodIBGE := 4101309; // Antonio Olinto/PR
    7425: CodIBGE := 4101408; // Apucarana/PR
    7427: CodIBGE := 4101507; // Arapongas/PR
    7429: CodIBGE := 4101606; // Arapoti/PR
    0830: CodIBGE := 4101655; // Arapua/PR
    7431: CodIBGE := 4101705; // Araruna/PR
    7435: CodIBGE := 4101804; // Araucaria/PR
    0832: CodIBGE := 4101853; // Ariranha Do Ivai/PR
    7437: CodIBGE := 4101903; // Assai/PR
    7953: CodIBGE := 4102000; // Assis Chateaubriand/PR
    7439: CodIBGE := 4102109; // Astorga/PR
    7441: CodIBGE := 4102208; // Atalaia/PR
    7443: CodIBGE := 4102307; // Balsa Nova/PR
    7445: CodIBGE := 4102406; // Bandeirantes/PR
    7447: CodIBGE := 4102505; // Barbosa Ferraz/PR
    7449: CodIBGE := 4102604; // Barracao/PR
    7451: CodIBGE := 4102703; // Barra Do Jacare/PR
    0834: CodIBGE := 4102752; // Bela Vista Da Caroba/PR
    7453: CodIBGE := 4102802; // Bela Vista Do Paraiso/PR
    7455: CodIBGE := 4102901; // Bituruna/PR
    7457: CodIBGE := 4103008; // Boa Esperanca/PR
    5471: CodIBGE := 4103024; // Boa Esperanca Do Iguacu/PR
    0836: CodIBGE := 4103040; // Boa Ventura De Sao Roque/PR
    7981: CodIBGE := 4103057; // Boa Vista Da Aparecida/PR
    7459: CodIBGE := 4103107; // Bocaiuva Do Sul/PR
    0838: CodIBGE := 4103156; // Bom Jesus Do Sul/PR
    7461: CodIBGE := 4103206; // Bom Sucesso/PR
    9979: CodIBGE := 4103222; // Bom Sucesso Do Sul/PR
    7463: CodIBGE := 4103305; // Borrazopolis/PR
    7983: CodIBGE := 4103354; // Braganey/PR
    5521: CodIBGE := 4103370; // Brasilandia Do Sul/PR
    7465: CodIBGE := 4103404; // Cafeara/PR
    7985: CodIBGE := 4103453; // Cafelandia/PR
    5491: CodIBGE := 4103479; // Cafezal Do Sul/PR
    7467: CodIBGE := 4103503; // California/PR
    7469: CodIBGE := 4103602; // Cambara/PR
    7471: CodIBGE := 4103701; // Cambe/PR
    7473: CodIBGE := 4103800; // Cambira/PR
    7475: CodIBGE := 4103909; // Campina Da Lagoa/PR
    0840: CodIBGE := 4103958; // Campina Do Simao/PR
    7477: CodIBGE := 4104006; // Campina Grande Do Sul/PR
    8475: CodIBGE := 4104055; // Campo Bonito/PR
    7479: CodIBGE := 4104105; // Campo Do Tenente/PR
    7481: CodIBGE := 4104204; // Campo Largo/PR
    0842: CodIBGE := 4104253; // Campo Magro/PR
    7483: CodIBGE := 4104303; // Campo Mourao/PR
    7485: CodIBGE := 4104402; // Candido De Abreu/PR
    5499: CodIBGE := 4104428; // Candoi/PR
    8451: CodIBGE := 4104451; // Cantagalo/PR
    7487: CodIBGE := 4104501; // Capanema/PR
    7489: CodIBGE := 4104600; // Capitao Leonidas Marques/PR
    0844: CodIBGE := 4104659; // Carambei/PR
    7491: CodIBGE := 4104709; // Carlopolis/PR
    7493: CodIBGE := 4104808; // Cascavel/PR
    7495: CodIBGE := 4104907; // Castro/PR
    7497: CodIBGE := 4105003; // Catanduvas/PR
    7499: CodIBGE := 4105102; // Centenario Do Sul/PR
    7501: CodIBGE := 4105201; // Cerro Azul/PR
    7957: CodIBGE := 4105300; // Ceu Azul/PR
    7503: CodIBGE := 4105409; // Chopinzinho/PR
    7505: CodIBGE := 4105508; // Cianorte/PR
    7507: CodIBGE := 4105607; // Cidade Gaucha/PR
    7509: CodIBGE := 4105706; // Clevelandia/PR
    7513: CodIBGE := 4105805; // Colombo/PR
    7515: CodIBGE := 4105904; // Colorado/PR
    7517: CodIBGE := 4106001; // Congonhinhas/PR
    7519: CodIBGE := 4106100; // Conselheiro Mairinck/PR
    7521: CodIBGE := 4106209; // Contenda/PR
    7523: CodIBGE := 4106308; // Corbelia/PR
    7525: CodIBGE := 4106407; // Cornelio Procopio/PR
    0846: CodIBGE := 4106456; // Coronel Domingos Soares/PR
    7527: CodIBGE := 4106506; // Coronel Vivida/PR
    8479: CodIBGE := 4106555; // Corumbatai Do Sul/PR
    5473: CodIBGE := 4106571; // Cruzeiro Do Iguacu/PR
    7529: CodIBGE := 4106605; // Cruzeiro Do Oeste/PR
    7531: CodIBGE := 4106704; // Cruzeiro Do Sul/PR
    7533: CodIBGE := 4106803; // Cruz Machado/PR
    0848: CodIBGE := 4106852; // Cruzmaltina/PR
    7535: CodIBGE := 4106902; // Curitiba/PR
    7537: CodIBGE := 4107009; // Curiuva/PR
    7539: CodIBGE := 4107108; // Diamante Do Norte/PR
    5465: CodIBGE := 4107124; // Diamante Do Sul/PR
    9915: CodIBGE := 4107157; // Diamante D Oeste/PR
    7541: CodIBGE := 4107207; // Dois Vizinhos/PR
    8465: CodIBGE := 4107256; // Douradina/PR
    7543: CodIBGE := 4107306; // Doutor Camargo/PR
    7545: CodIBGE := 4107405; // Eneas Marques/PR
    7547: CodIBGE := 4107504; // Engenheiro Beltrao/PR
    0850: CodIBGE := 4107520; // Esperanca Nova/PR
    5529: CodIBGE := 4107538; // Entre Rios Do Oeste/PR
    0852: CodIBGE := 4107546; // Espigao Alto Do Iguacu/PR
    5511: CodIBGE := 4107553; // Farol/PR
    7549: CodIBGE := 4107603; // Faxinal/PR
    9983: CodIBGE := 4107652; // Fazenda Rio Grande/PR
    7551: CodIBGE := 4107702; // Fenix/PR
    0854: CodIBGE := 4107736; // Fernandes Pinheiro/PR
    8457: CodIBGE := 4107751; // Figueira/PR
    7553: CodIBGE := 4107801; // Florai/PR
    5475: CodIBGE := 4107850; // Flor Da Serra Do Sul/PR
    7555: CodIBGE := 4107900; // Floresta/PR
    7557: CodIBGE := 4108007; // Florestopolis/PR
    7559: CodIBGE := 4108106; // Florida/PR
    7561: CodIBGE := 4108205; // Formosa Do Oeste/PR
    7563: CodIBGE := 4108304; // Foz Do Iguacu/PR
    7977: CodIBGE := 4108320; // Francisco Alves/PR
    7565: CodIBGE := 4108403; // Francisco Beltrao/PR
    0856: CodIBGE := 4108452; // Foz Do Jordao/PR
    7567: CodIBGE := 4108502; // General Carneiro/PR
    9947: CodIBGE := 4108551; // Godoy Moreira/PR
    7569: CodIBGE := 4108601; // Goioere/PR
    0858: CodIBGE := 4108650; // Goioxim/PR
    7959: CodIBGE := 4108700; // Grandes Rios/PR
    7571: CodIBGE := 4108809; // Guaira/PR
    7573: CodIBGE := 4108908; // Guairaca/PR
    0860: CodIBGE := 4108957; // Guamiranga/PR
    7575: CodIBGE := 4109005; // Guapirama/PR
    7577: CodIBGE := 4109104; // Guaporema/PR
    7579: CodIBGE := 4109203; // Guaraci/PR
    7581: CodIBGE := 4109302; // Guaraniacu/PR
    7583: CodIBGE := 4109401; // Guarapuava/PR
    7585: CodIBGE := 4109500; // Guaraquecaba/PR
    7587: CodIBGE := 4109609; // Guaratuba/PR
    9981: CodIBGE := 4109658; // Honorio Serpa/PR
    7589: CodIBGE := 4109708; // Ibaiti/PR
    9949: CodIBGE := 4109757; // Ibema/PR
    7591: CodIBGE := 4109807; // Ibipora/PR
    7593: CodIBGE := 4109906; // Icaraima/PR
    7595: CodIBGE := 4110003; // Iguaracu/PR
    5467: CodIBGE := 4110052; // Iguatu/PR
    0862: CodIBGE := 4110078; // Imbau/PR
    7597: CodIBGE := 4110102; // Imbituva/PR
    7599: CodIBGE := 4110201; // Inacio Martins/PR
    7601: CodIBGE := 4110300; // Inaja/PR
    7961: CodIBGE := 4110409; // Indianopolis/PR
    7603: CodIBGE := 4110508; // Ipiranga/PR
    7605: CodIBGE := 4110607; // Ipora/PR
    5485: CodIBGE := 4110656; // Iracema Do Oeste/PR
    7607: CodIBGE := 4110706; // Irati/PR
    7609: CodIBGE := 4110805; // Iretama/PR
    7611: CodIBGE := 4110904; // Itaguaje/PR
    5525: CodIBGE := 4110953; // Itaipulandia/PR
    7613: CodIBGE := 4111001; // Itambaraca/PR
    7615: CodIBGE := 4111100; // Itambe/PR
    7617: CodIBGE := 4111209; // Itapejara D Oeste/PR
    5451: CodIBGE := 4111258; // Itaperucu/PR
    7619: CodIBGE := 4111308; // Itauna Do Sul/PR
    7621: CodIBGE := 4111407; // Ivai/PR
    7623: CodIBGE := 4111506; // Ivaipora/PR
    9955: CodIBGE := 4111555; // Ivate/PR
    7625: CodIBGE := 4111605; // Ivatuba/PR
    7627: CodIBGE := 4111704; // Jaboti/PR
    7629: CodIBGE := 4111803; // Jacarezinho/PR
    7631: CodIBGE := 4111902; // Jaguapita/PR
    7633: CodIBGE := 4112009; // Jaguariaiva/PR
    7635: CodIBGE := 4112108; // Jandaia Do Sul/PR
    7637: CodIBGE := 4112207; // Janiopolis/PR
    7639: CodIBGE := 4112306; // Japira/PR
    7641: CodIBGE := 4112405; // Japura/PR
    7643: CodIBGE := 4112504; // Jardim Alegre/PR
    7645: CodIBGE := 4112603; // Jardim Olinda/PR
    7647: CodIBGE := 4112702; // Jataizinho/PR
    7997: CodIBGE := 4112751; // Jesuitas/PR
    7649: CodIBGE := 4112801; // Joaquim Tavora/PR
    7651: CodIBGE := 4112900; // Jundiai Do Sul/PR
    8463: CodIBGE := 4112959; // Juranda/PR
    7653: CodIBGE := 4113007; // Jussara/PR
    7655: CodIBGE := 4113106; // Kalore/PR
    7657: CodIBGE := 4113205; // Lapa/PR
    5501: CodIBGE := 4113254; // Laranjal/PR
    7659: CodIBGE := 4113304; // Laranjeiras Do Sul/PR
    7661: CodIBGE := 4113403; // Leopolis/PR
    5507: CodIBGE := 4113429; // Lidianopolis/PR
    9959: CodIBGE := 4113452; // Lindoeste/PR
    7663: CodIBGE := 4113502; // Loanda/PR
    7665: CodIBGE := 4113601; // Lobato/PR
    7667: CodIBGE := 4113700; // Londrina/PR
    8481: CodIBGE := 4113734; // Luiziana/PR
    8459: CodIBGE := 4113759; // Lunardelli/PR
    7669: CodIBGE := 4113809; // Lupionopolis/PR
    7671: CodIBGE := 4113908; // Mallet/PR
    7673: CodIBGE := 4114005; // Mambore/PR
    7675: CodIBGE := 4114104; // Mandaguacu/PR
    7677: CodIBGE := 4114203; // Mandaguari/PR
    7679: CodIBGE := 4114302; // Mandirituba/PR
    0864: CodIBGE := 4114351; // Manfrinopolis/PR
    7511: CodIBGE := 4114401; // Mangueirinha/PR
    7681: CodIBGE := 4114500; // Manoel Ribas/PR
    7683: CodIBGE := 4114609; // Marechal Candido Rondon/PR
    7685: CodIBGE := 4114708; // Maria Helena/PR
    7687: CodIBGE := 4114807; // Marialva/PR
    7433: CodIBGE := 4114906; // Marilandia Do Sul/PR
    7975: CodIBGE := 4115002; // Marilena/PR
    7689: CodIBGE := 4115101; // Mariluz/PR
    7691: CodIBGE := 4115200; // Maringa/PR
    7693: CodIBGE := 4115309; // Mariopolis/PR
    5487: CodIBGE := 4115358; // Maripa/PR
    7695: CodIBGE := 4115408; // Marmeleiro/PR
    0866: CodIBGE := 4115457; // Marquinho/PR
    7697: CodIBGE := 4115507; // Marumbi/PR
    7699: CodIBGE := 4115606; // Matelandia/PR
    7963: CodIBGE := 4115705; // Matinhos/PR
    5503: CodIBGE := 4115739; // Mato Rico/PR
    5459: CodIBGE := 4115754; // Maua Da Serra/PR
    7701: CodIBGE := 4115804; // Medianeira/PR
    5531: CodIBGE := 4115853; // Mercedes/PR
    7703: CodIBGE := 4115903; // Mirador/PR
    7705: CodIBGE := 4116000; // Miraselva/PR
    8469: CodIBGE := 4116059; // Missal/PR
    7707: CodIBGE := 4116109; // Moreira Sales/PR
    7709: CodIBGE := 4116208; // Morretes/PR
    7711: CodIBGE := 4116307; // Munhoz De Melo/PR
    7713: CodIBGE := 4116406; // Nossa Senhora Das Gracas/PR
    7715: CodIBGE := 4116505; // Nova Alianca Do Ivai/PR
    7717: CodIBGE := 4116604; // Nova America Da Colina/PR
    7965: CodIBGE := 4116703; // Nova Aurora/PR
    7719: CodIBGE := 4116802; // Nova Cantu/PR
    7721: CodIBGE := 4116901; // Nova Esperanca/PR
    5477: CodIBGE := 4116950; // Nova Esperanca Do Sudoeste/PR
    7723: CodIBGE := 4117008; // Nova Fatima/PR
    5479: CodIBGE := 4117057; // Nova Laranjeiras/PR
    7725: CodIBGE := 4117107; // Nova Londrina/PR
    7967: CodIBGE := 4117206; // Nova Olimpia/PR
    5457: CodIBGE := 4117214; // Nova Santa Barbara/PR
    7979: CodIBGE := 4117222; // Nova Santa Rosa/PR
    7995: CodIBGE := 4117255; // Nova Prata Do Iguacu/PR
    9913: CodIBGE := 4117271; // Nova Tebas/PR
    5517: CodIBGE := 4117297; // Novo Itacolomi/PR
    7727: CodIBGE := 4117305; // Ortigueira/PR
    7729: CodIBGE := 4117404; // Ourizona/PR
    9965: CodIBGE := 4117453; // Ouro Verde Do Oeste/PR
    7731: CodIBGE := 4117503; // Paicandu/PR
    7733: CodIBGE := 4117602; // Palmas/PR
    7735: CodIBGE := 4117701; // Palmeira/PR
    7737: CodIBGE := 4117800; // Palmital/PR
    7739: CodIBGE := 4117909; // Palotina/PR
    7741: CodIBGE := 4118006; // Paraiso Do Norte/PR
    7743: CodIBGE := 4118105; // Paranacity/PR
    7745: CodIBGE := 4118204; // Paranagua/PR
    7747: CodIBGE := 4118303; // Paranapoema/PR
    7749: CodIBGE := 4118402; // Paranavai/PR
    5533: CodIBGE := 4118451; // Pato Bragado/PR
    7751: CodIBGE := 4118501; // Pato Branco/PR
    7753: CodIBGE := 4118600; // Paula Freitas/PR
    7755: CodIBGE := 4118709; // Paulo Frontin/PR
    7757: CodIBGE := 4118808; // Peabiru/PR
    0868: CodIBGE := 4118857; // Perobal/PR
    7969: CodIBGE := 4118907; // Perola/PR
    7759: CodIBGE := 4119004; // Perola D Oeste/PR
    7761: CodIBGE := 4119103; // Pien/PR
    5453: CodIBGE := 4119152; // Pinhais/PR
    7763: CodIBGE := 4119202; // Pinhalao/PR
    5495: CodIBGE := 4119251; // Pinhal De Sao Bento/PR
    7765: CodIBGE := 4119301; // Pinhao/PR
    7767: CodIBGE := 4119400; // Pirai Do Sul/PR
    7769: CodIBGE := 4119509; // Piraquara/PR
    7771: CodIBGE := 4119608; // Pitanga/PR
    5461: CodIBGE := 4119657; // Pitangueiras/PR
    7773: CodIBGE := 4119707; // Planaltina Do Parana/PR
    7775: CodIBGE := 4119806; // Planalto/PR
    7777: CodIBGE := 4119905; // Ponta Grossa/PR
    0870: CodIBGE := 4119954; // Pontal Do Parana/PR
    7779: CodIBGE := 4120002; // Porecatu/PR
    7781: CodIBGE := 4120101; // Porto Amazonas/PR
    0872: CodIBGE := 4120150; // Porto Barreiro/PR
    7783: CodIBGE := 4120200; // Porto Rico/PR
    7785: CodIBGE := 4120309; // Porto Vitoria/PR
    0874: CodIBGE := 4120333; // Prado Ferreira/PR
    7991: CodIBGE := 4120358; // Pranchita/PR
    7787: CodIBGE := 4120408; // Presidente Castelo Branco/PR
    7789: CodIBGE := 4120507; // Primeiro De Maio/PR
    7791: CodIBGE := 4120606; // Prudentopolis/PR
    0876: CodIBGE := 4120655; // Quarto Centenario/PR
    7793: CodIBGE := 4120705; // Quatigua/PR
    7795: CodIBGE := 4120804; // Quatro Barras/PR
    5535: CodIBGE := 4120853; // Quatro Pontes/PR
    7955: CodIBGE := 4120903; // Quedas Do Iguacu/PR
    7797: CodIBGE := 4121000; // Querencia Do Norte/PR
    7799: CodIBGE := 4121109; // Quinta Do Sol/PR
    7801: CodIBGE := 4121208; // Quitandinha/PR
    5527: CodIBGE := 4121257; // Ramilandia/PR
    7803: CodIBGE := 4121307; // Rancho Alegre/PR
    5513: CodIBGE := 4121356; // Rancho Alegre D Oeste/PR
    7805: CodIBGE := 4121406; // Realeza/PR
    7807: CodIBGE := 4121505; // Reboucas/PR
    7809: CodIBGE := 4121604; // Renascenca/PR
    7811: CodIBGE := 4121703; // Reserva/PR
    0878: CodIBGE := 4121752; // Reserva Do Iguacu/PR
    7813: CodIBGE := 4121802; // Ribeirao Claro/PR
    7815: CodIBGE := 4121901; // Ribeirao Do Pinhal/PR
    7817: CodIBGE := 4122008; // Rio Azul/PR
    7819: CodIBGE := 4122107; // Rio Bom/PR
    5481: CodIBGE := 4122156; // Rio Bonito Do Iguacu/PR
    0880: CodIBGE := 4122172; // Rio Branco Do Ivai/PR
    7821: CodIBGE := 4122206; // Rio Branco Do Sul/PR
    7823: CodIBGE := 4122305; // Rio Negro/PR
    7825: CodIBGE := 4122404; // Rolandia/PR
    7827: CodIBGE := 4122503; // Roncador/PR
    7829: CodIBGE := 4122602; // Rondon/PR
    8473: CodIBGE := 4122651; // Rosario Do Ivai/PR
    7831: CodIBGE := 4122701; // Sabaudia/PR
    7833: CodIBGE := 4122800; // Salgado Filho/PR
    7835: CodIBGE := 4122909; // Salto Do Itarare/PR
    7837: CodIBGE := 4123006; // Salto Do Lontra/PR
    7839: CodIBGE := 4123105; // Santa Amelia/PR
    7841: CodIBGE := 4123204; // Santa Cecilia Do Pavao/PR
    7843: CodIBGE := 4123303; // Santa Cruz De Monte Castelo/PR
    7845: CodIBGE := 4123402; // Santa Fe/PR
    7971: CodIBGE := 4123501; // Santa Helena/PR
    7847: CodIBGE := 4123600; // Santa Ines/PR
    7849: CodIBGE := 4123709; // Santa Isabel Do Ivai/PR
    7851: CodIBGE := 4123808; // Santa Izabel Do Oeste/PR
    5469: CodIBGE := 4123824; // Santa Lucia/PR
    5505: CodIBGE := 4123857; // Santa Maria Do Oeste/PR
    7853: CodIBGE := 4123907; // Santa Mariana/PR
    5519: CodIBGE := 4123956; // Santa Monica/PR
    7855: CodIBGE := 4124004; // Santana Do Itarare/PR
    9969: CodIBGE := 4124020; // Santa Tereza Do Oeste/PR
    8467: CodIBGE := 4124053; // Santa Terezinha De Itaipu/PR
    7859: CodIBGE := 4124103; // Santo Antonio Da Platina/PR
    7861: CodIBGE := 4124202; // Santo Antonio Do Caiua/PR
    7863: CodIBGE := 4124301; // Santo Antonio Do Paraiso/PR
    7857: CodIBGE := 4124400; // Santo Antonio Do Sudoeste/PR
    7865: CodIBGE := 4124509; // Santo Inacio/PR
    7867: CodIBGE := 4124608; // Sao Carlos Do Ivai/PR
    7869: CodIBGE := 4124707; // Sao Jeronimo Da Serra/PR
    7871: CodIBGE := 4124806; // Sao Joao/PR
    7873: CodIBGE := 4124905; // Sao Joao Do Caiua/PR
    7875: CodIBGE := 4125001; // Sao Joao Do Ivai/PR
    7877: CodIBGE := 4125100; // Sao Joao Do Triunfo/PR
    7881: CodIBGE := 4125209; // Sao Jorge D Oeste/PR
    7879: CodIBGE := 4125308; // Sao Jorge Do Ivai/PR
    7999: CodIBGE := 4125357; // Sao Jorge Do Patrocinio/PR
    7883: CodIBGE := 4125407; // Sao Jose Da Boa Vista/PR
    8471: CodIBGE := 4125456; // Sao Jose Das Palmeiras/PR
    7885: CodIBGE := 4125506; // Sao Jose Dos Pinhais/PR
    5515: CodIBGE := 4125555; // Sao Manoel Do Parana/PR
    7887: CodIBGE := 4125605; // Sao Mateus Do Sul/PR
    7889: CodIBGE := 4125704; // Sao Miguel Do Iguacu/PR
    5489: CodIBGE := 4125753; // Sao Pedro Do Iguacu/PR
    7891: CodIBGE := 4125803; // Sao Pedro Do Ivai/PR
    7893: CodIBGE := 4125902; // Sao Pedro Do Parana/PR
    7895: CodIBGE := 4126009; // Sao Sebastiao Da Amoreira/PR
    7897: CodIBGE := 4126108; // Sao Tome/PR
    7899: CodIBGE := 4126207; // Sapopema/PR
    8461: CodIBGE := 4126256; // Sarandi/PR
    5493: CodIBGE := 4126272; // Saudade Do Iguacu/PR
    7901: CodIBGE := 4126306; // Senges/PR
    0882: CodIBGE := 4126355; // Serranopolis Do Iguacu/PR
    7903: CodIBGE := 4126405; // Sertaneja/PR
    7905: CodIBGE := 4126504; // Sertanopolis/PR
    7907: CodIBGE := 4126603; // Siqueira Campos/PR
    8477: CodIBGE := 4126652; // Sulina/PR
    0884: CodIBGE := 4126678; // Tamarana/PR
    7909: CodIBGE := 4126702; // Tamboara/PR
    7911: CodIBGE := 4126801; // Tapejara/PR
    7973: CodIBGE := 4126900; // Tapira/PR
    7913: CodIBGE := 4127007; // Teixeira Soares/PR
    7915: CodIBGE := 4127106; // Telemaco Borba/PR
    7917: CodIBGE := 4127205; // Terra Boa/PR
    7919: CodIBGE := 4127304; // Terra Rica/PR
    7921: CodIBGE := 4127403; // Terra Roxa/PR
    7923: CodIBGE := 4127502; // Tibagi/PR
    7925: CodIBGE := 4127601; // Tijucas Do Sul/PR
    7927: CodIBGE := 4127700; // Toledo/PR
    7929: CodIBGE := 4127809; // Tomazina/PR
    7987: CodIBGE := 4127858; // Tres Barras Do Parana/PR
    5455: CodIBGE := 4127882; // Tunas Do Parana/PR
    7931: CodIBGE := 4127908; // Tuneiras Do Oeste/PR
    7993: CodIBGE := 4127957; // Tupassi/PR
    8453: CodIBGE := 4127965; // Turvo/PR
    7933: CodIBGE := 4128005; // Ubirata/PR
    7935: CodIBGE := 4128104; // Umuarama/PR
    7937: CodIBGE := 4128203; // Uniao Da Vitoria/PR
    7939: CodIBGE := 4128302; // Uniflor/PR
    7941: CodIBGE := 4128401; // Urai/PR
    7943: CodIBGE := 4128500; // Wenceslau Braz/PR
    5497: CodIBGE := 4128534; // Ventania/PR
    7989: CodIBGE := 4128559; // Vera Cruz Do Oeste/PR
    7945: CodIBGE := 4128609; // Vere/PR
    5523: CodIBGE := 4128625; // Alto Paraiso/PR
    5449: CodIBGE := 4128633; // Doutor Ulysses/PR
    5483: CodIBGE := 4128658; // Virmond/PR
    7947: CodIBGE := 4128708; // Vitorino/PR
    7949: CodIBGE := 4128807; // Xambre/PR
    9939: CodIBGE := 4200051; // Abdon Batista/SC
    8001: CodIBGE := 4200101; // Abelardo Luz/SC
    8003: CodIBGE := 4200200; // Agrolandia/SC
    8005: CodIBGE := 4200309; // Agronomica/SC
    8007: CodIBGE := 4200408; // Agua Doce/SC
    8009: CodIBGE := 4200507; // Aguas De Chapeco/SC
    5577: CodIBGE := 4200556; // Aguas Frias/SC
    8011: CodIBGE := 4200606; // Aguas Mornas/SC
    8013: CodIBGE := 4200705; // Alfredo Wagner/SC
    0886: CodIBGE := 4200754; // Alto Bela Vista/SC
    8015: CodIBGE := 4200804; // Anchieta/SC
    8017: CodIBGE := 4200903; // Angelina/SC
    8019: CodIBGE := 4201000; // Anita Garibaldi/SC
    8021: CodIBGE := 4201109; // Anitapolis/SC
    8023: CodIBGE := 4201208; // Antonio Carlos/SC
    9941: CodIBGE := 4201257; // Apiuna/SC
    5597: CodIBGE := 4201273; // Arabuta/SC
    8025: CodIBGE := 4201307; // Araquari/SC
    8027: CodIBGE := 4201406; // Ararangua/SC
    8029: CodIBGE := 4201505; // Armazem/SC
    8031: CodIBGE := 4201604; // Arroio Trinta/SC
    5599: CodIBGE := 4201653; // Arvoredo/SC
    8033: CodIBGE := 4201703; // Ascurra/SC
    8035: CodIBGE := 4201802; // Atalanta/SC
    8037: CodIBGE := 4201901; // Aurora/SC
    0888: CodIBGE := 4201950; // Balneario Arroio Do Silva/SC
    8039: CodIBGE := 4202008; // Balneario Camboriu/SC
    5549: CodIBGE := 4202057; // Balneario Barra Do Sul/SC
    0890: CodIBGE := 4202073; // Balneario Gaivota/SC
    0892: CodIBGE := 4202081; // Bandeirante/SC
    0894: CodIBGE := 4202099; // Barra Bonita/SC
    8041: CodIBGE := 4202107; // Barra Velha/SC
    0896: CodIBGE := 4202131; // Bela Vista Do Toldo/SC
    5745: CodIBGE := 4202156; // Belmonte/SC
    8043: CodIBGE := 4202206; // Benedito Novo/SC
    8045: CodIBGE := 4202305; // Biguacu/SC
    8047: CodIBGE := 4202404; // Blumenau/SC
    0898: CodIBGE := 4202438; // Bocaina Do Sul/SC
    5537: CodIBGE := 4202453; // Bombinhas/SC
    8389: CodIBGE := 4202503; // Bom Jardim Da Serra/SC
    0900: CodIBGE := 4202537; // Bom Jesus/SC
    0902: CodIBGE := 4202578; // Bom Jesus Do Oeste/SC
    8049: CodIBGE := 4202602; // Bom Retiro/SC
    8051: CodIBGE := 4202701; // Botuvera/SC
    8053: CodIBGE := 4202800; // Braco Do Norte/SC
    5557: CodIBGE := 4202859; // Braco Do Trombudo/SC
    0904: CodIBGE := 4202875; // Brunopolis/SC
    8055: CodIBGE := 4202909; // Brusque/SC
    8057: CodIBGE := 4203006; // Cacador/SC
    8059: CodIBGE := 4203105; // Caibi/SC
    5553: CodIBGE := 4203154; // Calmon/SC
    8061: CodIBGE := 4203204; // Camboriu/SC
    0906: CodIBGE := 4203253; // Capao Alto/SC
    8063: CodIBGE := 4203303; // Campo Alegre/SC
    8065: CodIBGE := 4203402; // Campo Belo Do Sul/SC
    8067: CodIBGE := 4203501; // Campo Ere/SC
    8069: CodIBGE := 4203600; // Campos Novos/SC
    8071: CodIBGE := 4203709; // Canelinha/SC
    8073: CodIBGE := 4203808; // Canoinhas/SC
    8075: CodIBGE := 4203907; // Capinzal/SC
    5545: CodIBGE := 4203956; // Capivari De Baixo/SC
    8077: CodIBGE := 4204004; // Catanduvas/SC
    8079: CodIBGE := 4204103; // Caxambu Do Sul/SC
    9943: CodIBGE := 4204152; // Celso Ramos/SC
    5567: CodIBGE := 4204178; // Cerro Negro/SC
    0908: CodIBGE := 4204194; // Chapadao Do Lageado/SC
    8081: CodIBGE := 4204202; // Chapeco/SC
    5543: CodIBGE := 4204251; // Cocal Do Sul/SC
    8083: CodIBGE := 4204301; // Concordia/SC
    5579: CodIBGE := 4204350; // Cordilheira Alta/SC
    8085: CodIBGE := 4204400; // Coronel Freitas/SC
    5735: CodIBGE := 4204459; // Coronel Martins/SC
    8087: CodIBGE := 4204509; // Corupa/SC
    8395: CodIBGE := 4204558; // Correia Pinto/SC
    8089: CodIBGE := 4204608; // Criciuma/SC
    8091: CodIBGE := 4204707; // Cunha Pora/SC
    0910: CodIBGE := 4204756; // Cunhatai/SC
    8093: CodIBGE := 4204806; // Curitibanos/SC
    8095: CodIBGE := 4204905; // Descanso/SC
    8097: CodIBGE := 4205001; // Dionisio Cerqueira/SC
    8099: CodIBGE := 4205100; // Dona Emma/SC
    9945: CodIBGE := 4205159; // Doutor Pedrinho/SC
    0912: CodIBGE := 4205175; // Entre Rios/SC
    0914: CodIBGE := 4205191; // Ermo/SC
    8101: CodIBGE := 4205209; // Erval Velho/SC
    8103: CodIBGE := 4205308; // Faxinal Dos Guedes/SC
    0916: CodIBGE := 4205357; // Flor Do Sertao/SC
    8105: CodIBGE := 4205407; // Florianopolis/SC
    5581: CodIBGE := 4205431; // Formosa Do Sul/SC
    0973: CodIBGE := 4205456; // Forquilhinha/SC
    8107: CodIBGE := 4205506; // Fraiburgo/SC
    0918: CodIBGE := 4205555; // Frei Rogerio/SC
    8109: CodIBGE := 4205605; // Galvao/SC
    8113: CodIBGE := 4205704; // Garopaba/SC
    8115: CodIBGE := 4205803; // Garuva/SC
    8117: CodIBGE := 4205902; // Gaspar/SC
    8111: CodIBGE := 4206009; // Governador Celso Ramos/SC
    8119: CodIBGE := 4206108; // Grao Para/SC
    8121: CodIBGE := 4206207; // Gravatal/SC
    8123: CodIBGE := 4206306; // Guabiruba/SC
    8125: CodIBGE := 4206405; // Guaraciaba/SC
    8127: CodIBGE := 4206504; // Guaramirim/SC
    8129: CodIBGE := 4206603; // Guaruja Do Sul/SC
    5583: CodIBGE := 4206652; // Guatambu/SC
    8131: CodIBGE := 4206702; // Herval D Oeste/SC
    0920: CodIBGE := 4206751; // Ibiam/SC
    8133: CodIBGE := 4206801; // Ibicare/SC
    8135: CodIBGE := 4206900; // Ibirama/SC
    8137: CodIBGE := 4207007; // Icara/SC
    8139: CodIBGE := 4207106; // Ilhota/SC
    8141: CodIBGE := 4207205; // Imarui/SC
    8143: CodIBGE := 4207304; // Imbituba/SC
    8145: CodIBGE := 4207403; // Imbuia/SC
    8147: CodIBGE := 4207502; // Indaial/SC
    0922: CodIBGE := 4207577; // Iomere/SC
    8149: CodIBGE := 4207601; // Ipira/SC
    9951: CodIBGE := 4207650; // Ipora Do Oeste/SC
    5737: CodIBGE := 4207684; // Ipuacu/SC
    8151: CodIBGE := 4207700; // Ipumirim/SC
    9953: CodIBGE := 4207759; // Iraceminha/SC
    8153: CodIBGE := 4207809; // Irani/SC
    5585: CodIBGE := 4207858; // Irati/SC
    8155: CodIBGE := 4207908; // Irineopolis/SC
    8157: CodIBGE := 4208005; // Ita/SC
    8159: CodIBGE := 4208104; // Itaiopolis/SC
    8161: CodIBGE := 4208203; // Itajai/SC
    8163: CodIBGE := 4208302; // Itapema/SC
    8165: CodIBGE := 4208401; // Itapiranga/SC
    9985: CodIBGE := 4208450; // Itapoa/SC
    8167: CodIBGE := 4208500; // Ituporanga/SC
    8169: CodIBGE := 4208609; // Jabora/SC
    8171: CodIBGE := 4208708; // Jacinto Machado/SC
    8173: CodIBGE := 4208807; // Jaguaruna/SC
    8175: CodIBGE := 4208906; // Jaragua Do Sul/SC
    5587: CodIBGE := 4208955; // Jardinopolis/SC
    8177: CodIBGE := 4209003; // Joacaba/SC
    8179: CodIBGE := 4209102; // Joinville/SC
    9957: CodIBGE := 4209151; // Jose Boiteux/SC
    0924: CodIBGE := 4209177; // Jupia/SC
    8181: CodIBGE := 4209201; // Lacerdopolis/SC
    8183: CodIBGE := 4209300; // Lages/SC
    8185: CodIBGE := 4209409; // Laguna/SC
    5739: CodIBGE := 4209458; // Lajeado Grande/SC
    8187: CodIBGE := 4209508; // Laurentino/SC
    8189: CodIBGE := 4209607; // Lauro Muller/SC
    8191: CodIBGE := 4209706; // Lebon Regis/SC
    8193: CodIBGE := 4209805; // Leoberto Leal/SC
    9961: CodIBGE := 4209854; // Lindoia Do Sul/SC
    8195: CodIBGE := 4209904; // Lontras/SC
    8197: CodIBGE := 4210001; // Luiz Alves/SC
    0926: CodIBGE := 4210035; // Luzerna/SC
    5575: CodIBGE := 4210050; // Macieira/SC
    8199: CodIBGE := 4210100; // Mafra/SC
    8201: CodIBGE := 4210209; // Major Gercino/SC
    8203: CodIBGE := 4210308; // Major Vieira/SC
    8391: CodIBGE := 4210407; // Maracaja/SC
    8205: CodIBGE := 4210506; // Maravilha/SC
    9963: CodIBGE := 4210555; // Marema/SC
    8207: CodIBGE := 4210605; // Massaranduba/SC
    8209: CodIBGE := 4210704; // Matos Costa/SC
    8211: CodIBGE := 4210803; // Meleiro/SC
    5559: CodIBGE := 4210852; // Mirim Doce/SC
    8213: CodIBGE := 4210902; // Modelo/SC
    8215: CodIBGE := 4211009; // Mondai/SC
    5561: CodIBGE := 4211058; // Monte Carlo/SC
    8217: CodIBGE := 4211108; // Monte Castelo/SC
    8219: CodIBGE := 4211207; // Morro Da Fumaca/SC
    5539: CodIBGE := 4211256; // Morro Grande/SC
    8221: CodIBGE := 4211306; // Navegantes/SC
    8223: CodIBGE := 4211405; // Nova Erechim/SC
    5589: CodIBGE := 4211454; // Nova Itaberaba/SC
    8225: CodIBGE := 4211504; // Nova Trento/SC
    8227: CodIBGE := 4211603; // Nova Veneza/SC
    5591: CodIBGE := 4211652; // Novo Horizonte/SC
    8229: CodIBGE := 4211702; // Orleans/SC
    8397: CodIBGE := 4211751; // Otacilio Costa/SC
    8231: CodIBGE := 4211801; // Ouro/SC
    5741: CodIBGE := 4211850; // Ouro Verde/SC
    0928: CodIBGE := 4211876; // Paial/SC
    0930: CodIBGE := 4211892; // Painel/SC
    8233: CodIBGE := 4211900; // Palhoca/SC
    8235: CodIBGE := 4212007; // Palma Sola/SC
    0932: CodIBGE := 4212056; // Palmeira/SC
    8237: CodIBGE := 4212106; // Palmitos/SC
    8239: CodIBGE := 4212205; // Papanduva/SC
    5747: CodIBGE := 4212239; // Paraiso/SC
    5541: CodIBGE := 4212254; // Passo De Torres/SC
    5743: CodIBGE := 4212270; // Passos Maia/SC
    8241: CodIBGE := 4212304; // Paulo Lopes/SC
    8243: CodIBGE := 4212403; // Pedras Grandes/SC
    8245: CodIBGE := 4212502; // Penha/SC
    8247: CodIBGE := 4212601; // Peritiba/SC
    1194: CodIBGE := 4212650; // Pescaria Brava/SC
    8249: CodIBGE := 4212700; // Petrolandia/SC
    8251: CodIBGE := 4212809; // Balneario Picarras/SC
    8253: CodIBGE := 4212908; // Pinhalzinho/SC
    8255: CodIBGE := 4213005; // Pinheiro Preto/SC
    8257: CodIBGE := 4213104; // Piratuba/SC
    5593: CodIBGE := 4213153; // Planalto Alegre/SC
    8259: CodIBGE := 4213203; // Pomerode/SC
    8261: CodIBGE := 4213302; // Ponte Alta/SC
    5569: CodIBGE := 4213351; // Ponte Alta Do Norte/SC
    8263: CodIBGE := 4213401; // Ponte Serrada/SC
    8265: CodIBGE := 4213500; // Porto Belo/SC
    8267: CodIBGE := 4213609; // Porto Uniao/SC
    8269: CodIBGE := 4213708; // Pouso Redondo/SC
    8271: CodIBGE := 4213807; // Praia Grande/SC
    8273: CodIBGE := 4213906; // Presidente Castello Branco/SC
    8275: CodIBGE := 4214003; // Presidente Getulio/SC
    8277: CodIBGE := 4214102; // Presidente Nereu/SC
    0934: CodIBGE := 4214151; // Princesa/SC
    8279: CodIBGE := 4214201; // Quilombo/SC
    8281: CodIBGE := 4214300; // Rancho Queimado/SC
    8283: CodIBGE := 4214409; // Rio Das Antas/SC
    8285: CodIBGE := 4214508; // Rio Do Campo/SC
    8287: CodIBGE := 4214607; // Rio Do Oeste/SC
    8289: CodIBGE := 4214706; // Rio Dos Cedros/SC
    8291: CodIBGE := 4214805; // Rio Do Sul/SC
    8293: CodIBGE := 4214904; // Rio Fortuna/SC
    8295: CodIBGE := 4215000; // Rio Negrinho/SC
    5571: CodIBGE := 4215059; // Rio Rufino/SC
    5749: CodIBGE := 4215075; // Riqueza/SC
    8297: CodIBGE := 4215109; // Rodeio/SC
    8299: CodIBGE := 4215208; // Romelandia/SC
    8301: CodIBGE := 4215307; // Salete/SC
    0936: CodIBGE := 4215356; // Saltinho/SC
    8303: CodIBGE := 4215406; // Salto Veloso/SC
    5547: CodIBGE := 4215455; // Sangao/SC
    8305: CodIBGE := 4215505; // Santa Cecilia/SC
    5751: CodIBGE := 4215554; // Santa Helena/SC
    8307: CodIBGE := 4215604; // Santa Rosa De Lima/SC
    9967: CodIBGE := 4215653; // Santa Rosa Do Sul/SC
    5555: CodIBGE := 4215679; // Santa Terezinha/SC
    0938: CodIBGE := 4215687; // Santa Terezinha Do Progresso/SC
    0940: CodIBGE := 4215695; // Santiago Do Sul/SC
    8309: CodIBGE := 4215703; // Santo Amaro Da Imperatriz/SC
    0942: CodIBGE := 4215752; // Sao Bernardino/SC
    8311: CodIBGE := 4215802; // Sao Bento Do Sul/SC
    8313: CodIBGE := 4215901; // Sao Bonifacio/SC
    8315: CodIBGE := 4216008; // Sao Carlos/SC
    5573: CodIBGE := 4216057; // Sao Cristovao Do Sul/SC
    8317: CodIBGE := 4216107; // Sao Domingos/SC
    8319: CodIBGE := 4216206; // Sao Francisco Do Sul/SC
    5753: CodIBGE := 4216255; // Sao Joao Do Oeste/SC
    8321: CodIBGE := 4216305; // Sao Joao Batista/SC
    5551: CodIBGE := 4216354; // Sao Joao Do Itaperiu/SC
    8323: CodIBGE := 4216404; // Sao Joao Do Sul/SC
    8325: CodIBGE := 4216503; // Sao Joaquim/SC
    8327: CodIBGE := 4216602; // Sao Jose/SC
    8329: CodIBGE := 4216701; // Sao Jose Do Cedro/SC
    8331: CodIBGE := 4216800; // Sao Jose Do Cerrito/SC
    8333: CodIBGE := 4216909; // Sao Lourenco Do Oeste/SC
    8335: CodIBGE := 4217006; // Sao Ludgero/SC
    8337: CodIBGE := 4217105; // Sao Martinho/SC
    5755: CodIBGE := 4217154; // Sao Miguel Da Boa Vista/SC
    8339: CodIBGE := 4217204; // Sao Miguel Do Oeste/SC
    0944: CodIBGE := 4217253; // Sao Pedro De Alcantara/SC
    8341: CodIBGE := 4217303; // Saudades/SC
    8343: CodIBGE := 4217402; // Schroeder/SC
    8345: CodIBGE := 4217501; // Seara/SC
    9989: CodIBGE := 4217550; // Serra Alta/SC
    8347: CodIBGE := 4217600; // Sideropolis/SC
    8349: CodIBGE := 4217709; // Sombrio/SC
    5595: CodIBGE := 4217758; // Sul Brasil/SC
    8351: CodIBGE := 4217808; // Taio/SC
    8353: CodIBGE := 4217907; // Tangara/SC
    0946: CodIBGE := 4217956; // Tigrinhos/SC
    8355: CodIBGE := 4218004; // Tijucas/SC
    8393: CodIBGE := 4218103; // Timbe Do Sul/SC
    8357: CodIBGE := 4218202; // Timbo/SC
    9971: CodIBGE := 4218251; // Timbo Grande/SC
    8359: CodIBGE := 4218301; // Tres Barras/SC
    0948: CodIBGE := 4218350; // Treviso/SC
    8361: CodIBGE := 4218400; // Treze De Maio/SC
    8363: CodIBGE := 4218509; // Treze Tilias/SC
    8365: CodIBGE := 4218608; // Trombudo Central/SC
    8367: CodIBGE := 4218707; // Tubarao/SC
    9991: CodIBGE := 4218756; // Tunapolis/SC
    8369: CodIBGE := 4218806; // Turvo/SC
    9973: CodIBGE := 4218855; // Uniao Do Oeste/SC
    8371: CodIBGE := 4218905; // Urubici/SC
    9975: CodIBGE := 4218954; // Urupema/SC
    8373: CodIBGE := 4219002; // Urussanga/SC
    8375: CodIBGE := 4219101; // Vargeao/SC
    5563: CodIBGE := 4219150; // Vargem/SC
    5565: CodIBGE := 4219176; // Vargem Bonita/SC
    8377: CodIBGE := 4219200; // Vidal Ramos/SC
    8379: CodIBGE := 4219309; // Videira/SC
    9977: CodIBGE := 4219358; // Vitor Meireles/SC
    8381: CodIBGE := 4219408; // Witmarsum/SC
    8383: CodIBGE := 4219507; // Xanxere/SC
    8385: CodIBGE := 4219606; // Xavantina/SC
    8387: CodIBGE := 4219705; // Xaxim/SC
    0950: CodIBGE := 4219853; // Zortea/SC
    1192: CodIBGE := 4220000; // Balneário Rincão/SC
    1118: CodIBGE := 4300034; // Acegua/RS
    8499: CodIBGE := 4300059; // Agua Santa/RS
    8501: CodIBGE := 4300109; // Agudo/RS
    8503: CodIBGE := 4300208; // Ajuricaba/RS
    8505: CodIBGE := 4300307; // Alecrim/RS
    8507: CodIBGE := 4300406; // Alegrete/RS
    8497: CodIBGE := 4300455; // Alegria/RS
    1120: CodIBGE := 4300471; // Almirante Tamandare Do Sul/RS
    8509: CodIBGE := 4300505; // Alpestre/RS
    8495: CodIBGE := 4300554; // Alto Alegre/RS
    6045: CodIBGE := 4300570; // Alto Feliz/RS
    8511: CodIBGE := 4300604; // Alvorada/RS
    8493: CodIBGE := 4300638; // Amaral Ferrador/RS
    5969: CodIBGE := 4300646; // Ametista Do Sul/RS
    8491: CodIBGE := 4300661; // Andre Da Rocha/RS
    8513: CodIBGE := 4300703; // Anta Gorda/RS
    8515: CodIBGE := 4300802; // Antonio Prado/RS
    5779: CodIBGE := 4300851; // Arambare/RS
    0952: CodIBGE := 4300877; // Ararica/RS
    8517: CodIBGE := 4300901; // Aratiba/RS
    8519: CodIBGE := 4301008; // Arroio Do Meio/RS
    8489: CodIBGE := 4301057; // Arroio Do Sal/RS
    1122: CodIBGE := 4301073; // Arroio Do Padre/RS
    8521: CodIBGE := 4301107; // Arroio Dos Ratos/RS
    8523: CodIBGE := 4301206; // Arroio Do Tigre/RS
    8525: CodIBGE := 4301305; // Arroio Grande/RS
    8527: CodIBGE := 4301404; // Arvorezinha/RS
    8529: CodIBGE := 4301503; // Augusto Pestana/RS
    8487: CodIBGE := 4301552; // Aurea/RS
    8531: CodIBGE := 4301602; // Bage/RS
    0954: CodIBGE := 4301636; // Balneario Pinhal/RS
    8485: CodIBGE := 4301651; // Barao/RS
    8533: CodIBGE := 4301701; // Barao De Cotegipe/RS
    5771: CodIBGE := 4301750; // Barao Do Triunfo/RS
    8535: CodIBGE := 4301800; // Barracao/RS
    6069: CodIBGE := 4301859; // Barra Do Guarita/RS
    0956: CodIBGE := 4301875; // Barra Do Quarai/RS
    8537: CodIBGE := 4301909; // Barra Do Ribeiro/RS
    5959: CodIBGE := 4301925; // Barra Do Rio Azul/RS
    5943: CodIBGE := 4301958; // Barra Funda/RS
    8539: CodIBGE := 4302006; // Barros Cassal/RS
    0958: CodIBGE := 4302055; // Benjamin Constant Do Sul/RS
    8541: CodIBGE := 4302105; // Bento Goncalves/RS
    5981: CodIBGE := 4302154; // Boa Vista Das Missoes/RS
    8543: CodIBGE := 4302204; // Boa Vista Do Burica/RS
    1124: CodIBGE := 4302220; // Boa Vista Do Cadeado/RS
    1126: CodIBGE := 4302238; // Boa Vista Do Incra/RS
    0960: CodIBGE := 4302253; // Boa Vista Do Sul/RS
    8545: CodIBGE := 4302303; // Bom Jesus/RS
    9823: CodIBGE := 4302352; // Bom Principio/RS
    6071: CodIBGE := 4302378; // Bom Progresso/RS
    8547: CodIBGE := 4302402; // Bom Retiro Do Sul/RS
    8483: CodIBGE := 4302451; // Boqueirao Do Leao/RS
    8549: CodIBGE := 4302501; // Bossoroca/RS
    1128: CodIBGE := 4302584; // Bozano/RS
    8551: CodIBGE := 4302600; // Braga/RS
    8449: CodIBGE := 4302659; // Brochier/RS
    8553: CodIBGE := 4302709; // Butia/RS
    8555: CodIBGE := 4302808; // Cacapava Do Sul/RS
    8557: CodIBGE := 4302907; // Cacequi/RS
    8559: CodIBGE := 4303004; // Cachoeira Do Sul/RS
    8561: CodIBGE := 4303103; // Cachoeirinha/RS
    8563: CodIBGE := 4303202; // Cacique Doble/RS
    8565: CodIBGE := 4303301; // Caibate/RS
    8567: CodIBGE := 4303400; // Caicara/RS
    8569: CodIBGE := 4303509; // Camaqua/RS
    8447: CodIBGE := 4303558; // Camargo/RS
    8571: CodIBGE := 4303608; // Cambara Do Sul/RS
    6013: CodIBGE := 4303673; // Campestre Da Serra/RS
    8573: CodIBGE := 4303707; // Campina Das Missoes/RS
    8575: CodIBGE := 4303806; // Campinas Do Sul/RS
    8577: CodIBGE := 4303905; // Campo Bom/RS
    8579: CodIBGE := 4304002; // Campo Novo/RS
    8445: CodIBGE := 4304101; // Campos Borges/RS
    8581: CodIBGE := 4304200; // Candelaria/RS
    8583: CodIBGE := 4304309; // Candido Godoi/RS
    6083: CodIBGE := 4304358; // Candiota/RS
    8585: CodIBGE := 4304408; // Canela/RS
    8587: CodIBGE := 4304507; // Cangucu/RS
    8589: CodIBGE := 4304606; // Canoas/RS
    1130: CodIBGE := 4304614; // Canudos Do Vale/RS
    1132: CodIBGE := 4304622; // Capao Bonito Do Sul/RS
    8915: CodIBGE := 4304630; // Capao Da Canoa/RS
    1134: CodIBGE := 4304655; // Capao Do Cipo/RS
    8973: CodIBGE := 4304663; // Capao Do Leao/RS
    0962: CodIBGE := 4304671; // Capivari Do Sul/RS
    8443: CodIBGE := 4304689; // Capela De Santana/RS
    6025: CodIBGE := 4304697; // Capitao/RS
    8591: CodIBGE := 4304705; // Carazinho/RS
    0964: CodIBGE := 4304713; // Caraa/RS
    8593: CodIBGE := 4304804; // Carlos Barbosa/RS
    5961: CodIBGE := 4304853; // Carlos Gomes/RS
    8595: CodIBGE := 4304903; // Casca/RS
    8441: CodIBGE := 4304952; // Caseiros/RS
    8597: CodIBGE := 4305009; // Catuipe/RS
    8599: CodIBGE := 4305108; // Caxias Do Sul/RS
    5963: CodIBGE := 4305116; // Centenario/RS
    0966: CodIBGE := 4305124; // Cerrito/RS
    8439: CodIBGE := 4305132; // Cerro Branco/RS
    8437: CodIBGE := 4305157; // Cerro Grande/RS
    8435: CodIBGE := 4305173; // Cerro Grande Do Sul/RS
    8601: CodIBGE := 4305207; // Cerro Largo/RS
    8603: CodIBGE := 4305306; // Chapada/RS
    8693: CodIBGE := 4305355; // Charqueadas/RS
    5965: CodIBGE := 4305371; // Charrua/RS
    8605: CodIBGE := 4305405; // Chiapetta/RS
    0968: CodIBGE := 4305439; // Chui/RS
    0970: CodIBGE := 4305447; // Chuvisca/RS
    8433: CodIBGE := 4305454; // Cidreira/RS
    8607: CodIBGE := 4305504; // Ciriaco/RS
    6029: CodIBGE := 4305587; // Colinas/RS
    8609: CodIBGE := 4305603; // Colorado/RS
    8611: CodIBGE := 4305702; // Condor/RS
    8613: CodIBGE := 4305801; // Constantina/RS
    1136: CodIBGE := 4305835; // Coqueiro Baixo/RS
    5945: CodIBGE := 4305850; // Coqueiros Do Sul/RS
    6055: CodIBGE := 4305871; // Coronel Barros/RS
    8615: CodIBGE := 4305900; // Coronel Bicaco/RS
    1138: CodIBGE := 4305934; // Coronel Pilar/RS
    8977: CodIBGE := 4305959; // Cotipora/RS
    5797: CodIBGE := 4305975; // Coxilha/RS
    8617: CodIBGE := 4306007; // Crissiumal/RS
    8431: CodIBGE := 4306056; // Cristal/RS
    0972: CodIBGE := 4306072; // Cristal Do Sul/RS
    8619: CodIBGE := 4306106; // Cruz Alta/RS
    1140: CodIBGE := 4306130; // Cruzaltense/RS
    8621: CodIBGE := 4306205; // Cruzeiro Do Sul/RS
    8623: CodIBGE := 4306304; // David Canabarro/RS
    6073: CodIBGE := 4306320; // Derrubadas/RS
    8429: CodIBGE := 4306353; // Dezesseis De Novembro/RS
    0974: CodIBGE := 4306379; // Dilermando De Aguiar/RS
    8625: CodIBGE := 4306403; // Dois Irmaos/RS
    5971: CodIBGE := 4306429; // Dois Irmaos Das Missoes/RS
    8427: CodIBGE := 4306452; // Dois Lajeados/RS
    8627: CodIBGE := 4306502; // Dom Feliciano/RS
    0976: CodIBGE := 4306551; // Dom Pedro De Alcantara/RS
    8629: CodIBGE := 4306601; // Dom Pedrito/RS
    8631: CodIBGE := 4306700; // Dona Francisca/RS
    8425: CodIBGE := 4306734; // Doutor Mauricio Cardoso/RS
    0978: CodIBGE := 4306759; // Doutor Ricardo/RS
    8423: CodIBGE := 4306767; // Eldorado Do Sul/RS
    8633: CodIBGE := 4306809; // Encantado/RS
    8635: CodIBGE := 4306908; // Encruzilhada Do Sul/RS
    5947: CodIBGE := 4306924; // Engenho Velho/RS
    8419: CodIBGE := 4306932; // Entre-Ijuis/RS
    8421: CodIBGE := 4306957; // Entre Rios Do Sul/RS
    8417: CodIBGE := 4306973; // Erebango/RS
    8637: CodIBGE := 4307005; // Erechim/RS
    8415: CodIBGE := 4307054; // Ernestina/RS
    8639: CodIBGE := 4307104; // Herval/RS
    8641: CodIBGE := 4307203; // Erval Grande/RS
    8643: CodIBGE := 4307302; // Erval Seco/RS
    8645: CodIBGE := 4307401; // Esmeralda/RS
    0980: CodIBGE := 4307450; // Esperanca Do Sul/RS
    8647: CodIBGE := 4307500; // Espumoso/RS
    7301: CodIBGE := 4307559; // Estacao/RS
    8649: CodIBGE := 4307609; // Estancia Velha/RS
    8651: CodIBGE := 4307708; // Esteio/RS
    8653: CodIBGE := 4307807; // Estrela/RS
    0982: CodIBGE := 4307815; // Estrela Velha/RS
    8413: CodIBGE := 4307831; // Eugenio De Castro/RS
    8411: CodIBGE := 4307864; // Fagundes Varela/RS
    8655: CodIBGE := 4307906; // Farroupilha/RS
    8657: CodIBGE := 4308003; // Faxinal Do Soturno/RS
    8409: CodIBGE := 4308052; // Faxinalzinho/RS
    0984: CodIBGE := 4308078; // Fazenda Vilanova/RS
    8659: CodIBGE := 4308102; // Feliz/RS
    8661: CodIBGE := 4308201; // Flores Da Cunha/RS
    0986: CodIBGE := 4308250; // Floriano Peixoto/RS
    8663: CodIBGE := 4308300; // Fontoura Xavier/RS
    8665: CodIBGE := 4308409; // Formigueiro/RS
    1142: CodIBGE := 4308433; // Forquetinha/RS
    9827: CodIBGE := 4308458; // Fortaleza Dos Valos/RS
    8667: CodIBGE := 4308508; // Frederico Westphalen/RS
    8669: CodIBGE := 4308607; // Garibaldi/RS
    6081: CodIBGE := 4308656; // Garruchos/RS
    8671: CodIBGE := 4308706; // Gaurama/RS
    8673: CodIBGE := 4308805; // General Camara/RS
    5799: CodIBGE := 4308854; // Gentil/RS
    8677: CodIBGE := 4308904; // Getulio Vargas/RS
    8679: CodIBGE := 4309001; // Girua/RS
    8407: CodIBGE := 4309050; // Glorinha/RS
    8681: CodIBGE := 4309100; // Gramado/RS
    5949: CodIBGE := 4309126; // Gramado Dos Loureiros/RS
    5763: CodIBGE := 4309159; // Gramado Xavier/RS
    8683: CodIBGE := 4309209; // Gravatai/RS
    8405: CodIBGE := 4309258; // Guabiju/RS
    8685: CodIBGE := 4309308; // Guaiba/RS
    8687: CodIBGE := 4309407; // Guapore/RS
    8689: CodIBGE := 4309506; // Guarani Das Missoes/RS
    8403: CodIBGE := 4309555; // Harmonia/RS
    0988: CodIBGE := 4309571; // Herveiras/RS
    8691: CodIBGE := 4309605; // Horizontina/RS
    6085: CodIBGE := 4309654; // Hulha Negra/RS
    8695: CodIBGE := 4309704; // Humaita/RS
    8401: CodIBGE := 4309753; // Ibarama/RS
    8697: CodIBGE := 4309803; // Ibiaca/RS
    8699: CodIBGE := 4309902; // Ibiraiaras/RS
    7299: CodIBGE := 4309951; // Ibirapuita/RS
    8701: CodIBGE := 4310009; // Ibiruba/RS
    8703: CodIBGE := 4310108; // Igrejinha/RS
    8705: CodIBGE := 4310207; // Ijui/RS
    8707: CodIBGE := 4310306; // Ilopolis/RS
    7297: CodIBGE := 4310330; // Imbe/RS
    7295: CodIBGE := 4310363; // Imigrante/RS
    8709: CodIBGE := 4310405; // Independencia/RS
    6051: CodIBGE := 4310413; // Inhacora/RS
    8399: CodIBGE := 4310439; // Ipe/RS
    7399: CodIBGE := 4310462; // Ipiranga Do Sul/RS
    8711: CodIBGE := 4310504; // Irai/RS
    0990: CodIBGE := 4310538; // Itaara/RS
    7397: CodIBGE := 4310553; // Itacurubi/RS
    6027: CodIBGE := 4310579; // Itapuca/RS
    8713: CodIBGE := 4310603; // Itaqui/RS
    1144: CodIBGE := 4310652; // Itati/RS
    8715: CodIBGE := 4310702; // Itatiba Do Sul/RS
    7395: CodIBGE := 4310751; // Ivora/RS
    8717: CodIBGE := 4310801; // Ivoti/RS
    7393: CodIBGE := 4310850; // Jaboticaba/RS
    1146: CodIBGE := 4310876; // Jacuizinho/RS
    8719: CodIBGE := 4310900; // Jacutinga/RS
    8721: CodIBGE := 4311007; // Jaguarao/RS
    8723: CodIBGE := 4311106; // Jaguari/RS
    7391: CodIBGE := 4311122; // Jaquirana/RS
    0992: CodIBGE := 4311130; // Jari/RS
    9829: CodIBGE := 4311155; // Joia/RS
    8725: CodIBGE := 4311205; // Julio De Castilhos/RS
    1148: CodIBGE := 4311239; // Lagoa Bonita Do Sul/RS
    7389: CodIBGE := 4311254; // Lagoao/RS
    5951: CodIBGE := 4311270; // Lagoa Dos Tres Cantos/RS
    8727: CodIBGE := 4311304; // Lagoa Vermelha/RS
    8729: CodIBGE := 4311403; // Lajeado/RS
    5983: CodIBGE := 4311429; // Lajeado Do Bugre/RS
    8731: CodIBGE := 4311502; // Lavras Do Sul/RS
    8733: CodIBGE := 4311601; // Liberato Salzano/RS
    6017: CodIBGE := 4311627; // Lindolfo Collor/RS
    6047: CodIBGE := 4311643; // Linha Nova/RS
    8735: CodIBGE := 4311700; // Machadinho/RS
    0994: CodIBGE := 4311718; // Macambara/RS
    0996: CodIBGE := 4311734; // Mampituba/RS
    6079: CodIBGE := 4311759; // Manoel Viana/RS
    5783: CodIBGE := 4311775; // Maquine/RS
    6039: CodIBGE := 4311791; // Marata/RS
    8737: CodIBGE := 4311809; // Marau/RS
    8739: CodIBGE := 4311908; // Marcelino Ramos/RS
    5759: CodIBGE := 4311981; // Mariana Pimentel/RS
    8741: CodIBGE := 4312005; // Mariano Moro/RS
    0998: CodIBGE := 4312054; // Marques De Souza/RS
    8743: CodIBGE := 4312104; // Mata/RS
    5931: CodIBGE := 4312138; // Mato Castelhano/RS
    6031: CodIBGE := 4312153; // Mato Leitao/RS
    1150: CodIBGE := 4312179; // Mato Queimado/RS
    8745: CodIBGE := 4312203; // Maximiliano De Almeida/RS
    5773: CodIBGE := 4312252; // Minas Do Leao/RS
    8747: CodIBGE := 4312302; // Miraguai/RS
    7387: CodIBGE := 4312351; // Montauri/RS
    1000: CodIBGE := 4312377; // Monte Alegre Dos Campos/RS
    5993: CodIBGE := 4312385; // Monte Belo Do Sul/RS
    8749: CodIBGE := 4312401; // Montenegro/RS
    5933: CodIBGE := 4312427; // Mormaco/RS
    5775: CodIBGE := 4312443; // Morrinhos Do Sul/RS
    7385: CodIBGE := 4312450; // Morro Redondo/RS
    6019: CodIBGE := 4312476; // Morro Reuter/RS
    8751: CodIBGE := 4312500; // Mostardas/RS
    8753: CodIBGE := 4312609; // Mucum/RS
    1002: CodIBGE := 4312617; // Muitos Capoes/RS
    5935: CodIBGE := 4312625; // Muliterno/RS
    8755: CodIBGE := 4312658; // Nao-Me-Toque/RS
    5937: CodIBGE := 4312674; // Nicolau Vergueiro/RS
    8757: CodIBGE := 4312708; // Nonoai/RS
    7383: CodIBGE := 4312757; // Nova Alvorada/RS
    8759: CodIBGE := 4312807; // Nova Araca/RS
    8761: CodIBGE := 4312906; // Nova Bassano/RS
    5953: CodIBGE := 4312955; // Nova Boa Vista/RS
    8763: CodIBGE := 4313003; // Nova Brescia/RS
    1004: CodIBGE := 4313011; // Nova Candelaria/RS
    7381: CodIBGE := 4313037; // Nova Esperanca Do Sul/RS
    7379: CodIBGE := 4313060; // Nova Hartz/RS
    5991: CodIBGE := 4313086; // Nova Padua/RS
    8765: CodIBGE := 4313102; // Nova Palma/RS
    8767: CodIBGE := 4313201; // Nova Petropolis/RS
    8769: CodIBGE := 4313300; // Nova Prata/RS
    1006: CodIBGE := 4313334; // Nova Ramada/RS
    7377: CodIBGE := 4313359; // Nova Roma Do Sul/RS
    5757: CodIBGE := 4313375; // Nova Santa Rita/RS
    1008: CodIBGE := 4313391; // Novo Cabrais/RS
    8771: CodIBGE := 4313409; // Novo Hamburgo/RS
    6057: CodIBGE := 4313425; // Novo Machado/RS
    5973: CodIBGE := 4313441; // Novo Tiradentes/RS
    1152: CodIBGE := 4313466; // Novo Xingu/RS
    5985: CodIBGE := 4313490; // Novo Barreiro/RS
    8773: CodIBGE := 4313508; // Osorio/RS
    8775: CodIBGE := 4313607; // Paim Filho/RS
    8967: CodIBGE := 4313656; // Palmares Do Sul/RS
    8777: CodIBGE := 4313706; // Palmeira Das Missoes/RS
    8779: CodIBGE := 4313805; // Palmitinho/RS
    8781: CodIBGE := 4313904; // Panambi/RS
    7375: CodIBGE := 4313953; // Pantano Grande/RS
    8783: CodIBGE := 4314001; // Parai/RS
    7373: CodIBGE := 4314027; // Paraiso Do Sul/RS
    6041: CodIBGE := 4314035; // Pareci Novo/RS
    9825: CodIBGE := 4314050; // Parobe/RS
    1010: CodIBGE := 4314068; // Passa Sete/RS
    5765: CodIBGE := 4314076; // Passo Do Sobrado/RS
    8785: CodIBGE := 4314100; // Passo Fundo/RS
    1154: CodIBGE := 4314134; // Paulo Bento/RS
    7371: CodIBGE := 4314159; // Paverama/RS
    1156: CodIBGE := 4314175; // Pedras Altas/RS
    8787: CodIBGE := 4314209; // Pedro Osorio/RS
    8789: CodIBGE := 4314308; // Pejucara/RS
    8791: CodIBGE := 4314407; // Pelotas/RS
    6021: CodIBGE := 4314423; // Picada Cafe/RS
    7369: CodIBGE := 4314456; // Pinhal/RS
    1158: CodIBGE := 4314464; // Pinhal Da Serra/RS
    5787: CodIBGE := 4314472; // Pinhal Grande/RS
    5975: CodIBGE := 4314498; // Pinheirinho Do Vale/RS
    8793: CodIBGE := 4314506; // Pinheiro Machado/RS
    1160: CodIBGE := 4314548; // Pinto Bandeira/RS
    7367: CodIBGE := 4314555; // Pirapo/RS
    8795: CodIBGE := 4314605; // Piratini/RS
    8797: CodIBGE := 4314704; // Planalto/RS
    7365: CodIBGE := 4314753; // Poco Das Antas/RS
    5939: CodIBGE := 4314779; // Pontao/RS
    5967: CodIBGE := 4314787; // Ponte Preta/RS
    8799: CodIBGE := 4314803; // Portao/RS
    8801: CodIBGE := 4314902; // Porto Alegre/RS
    8803: CodIBGE := 4315008; // Porto Lucena/RS
    6065: CodIBGE := 4315057; // Porto Maua/RS
    6067: CodIBGE := 4315073; // Porto Vera Cruz/RS
    8805: CodIBGE := 4315107; // Porto Xavier/RS
    7363: CodIBGE := 4315131; // Pouso Novo/RS
    6023: CodIBGE := 4315149; // Presidente Lucena/RS
    7361: CodIBGE := 4315156; // Progresso/RS
    7359: CodIBGE := 4315172; // Protasio Alves/RS
    8807: CodIBGE := 4315206; // Putinga/RS
    8809: CodIBGE := 4315305; // Quarai/RS
    1162: CodIBGE := 4315313; // Quatro Irmaos/RS
    5789: CodIBGE := 4315321; // Quevedos/RS
    7357: CodIBGE := 4315354; // Quinze De Novembro/RS
    8811: CodIBGE := 4315404; // Redentora/RS
    7355: CodIBGE := 4315453; // Relvado/RS
    8813: CodIBGE := 4315503; // Restinga Seca/RS
    5955: CodIBGE := 4315552; // Rio Dos Indios/RS
    8815: CodIBGE := 4315602; // Rio Grande/RS
    8817: CodIBGE := 4315701; // Rio Pardo/RS
    7353: CodIBGE := 4315750; // Riozinho/RS
    8819: CodIBGE := 4315800; // Roca Sales/RS
    8821: CodIBGE := 4315909; // Rodeio Bonito/RS
    1164: CodIBGE := 4315958; // Rolador/RS
    8823: CodIBGE := 4316006; // Rolante/RS
    8825: CodIBGE := 4316105; // Ronda Alta/RS
    8827: CodIBGE := 4316204; // Rondinha/RS
    8829: CodIBGE := 4316303; // Roque Gonzales/RS
    8831: CodIBGE := 4316402; // Rosario Do Sul/RS
    5987: CodIBGE := 4316428; // Sagrada Familia/RS
    7339: CodIBGE := 4316436; // Saldanha Marinho/RS
    8975: CodIBGE := 4316451; // Salto Do Jacui/RS
    6061: CodIBGE := 4316477; // Salvador Das Missoes/RS
    8833: CodIBGE := 4316501; // Salvador Do Sul/RS
    8835: CodIBGE := 4316600; // Sananduva/RS
    8837: CodIBGE := 4316709; // Santa Barbara Do Sul/RS
    1166: CodIBGE := 4316733; // Santa Cecilia Do Sul/RS
    6033: CodIBGE := 4316758; // Santa Clara Do Sul/RS
    8839: CodIBGE := 4316808; // Santa Cruz Do Sul/RS
    8841: CodIBGE := 4316907; // Santa Maria/RS
    7337: CodIBGE := 4316956; // Santa Maria Do Herval/RS
    1168: CodIBGE := 4316972; // Santa Margarida Do Sul/RS
    8843: CodIBGE := 4317004; // Santana Da Boa Vista/RS
    8845: CodIBGE := 4317103; // Sant  Ana Do Livramento/RS
    8847: CodIBGE := 4317202; // Santa Rosa/RS
    5995: CodIBGE := 4317251; // Santa Tereza/RS
    8849: CodIBGE := 4317301; // Santa Vitoria Do Palmar/RS
    8851: CodIBGE := 4317400; // Santiago/RS
    8853: CodIBGE := 4317509; // Santo Angelo/RS
    5941: CodIBGE := 4317558; // Santo Antonio Do Palma/RS
    8855: CodIBGE := 4317608; // Santo Antonio Da Patrulha/RS
    8857: CodIBGE := 4317707; // Santo Antonio Das Missoes/RS
    5957: CodIBGE := 4317756; // Santo Antonio Do Planalto/RS
    8859: CodIBGE := 4317806; // Santo Augusto/RS
    8861: CodIBGE := 4317905; // Santo Cristo/RS
    5977: CodIBGE := 4317954; // Santo Expedito Do Sul/RS
    8863: CodIBGE := 4318002; // Sao Borja/RS
    7351: CodIBGE := 4318051; // Sao Domingos Do Sul/RS
    8865: CodIBGE := 4318101; // Sao Francisco De Assis/RS
    8867: CodIBGE := 4318200; // Sao Francisco De Paula/RS
    8869: CodIBGE := 4318309; // Sao Gabriel/RS
    8871: CodIBGE := 4318408; // Sao Jeronimo/RS
    7349: CodIBGE := 4318424; // Sao Joao Da Urtiga/RS
    5791: CodIBGE := 4318432; // Sao Joao Do Polesine/RS
    7347: CodIBGE := 4318440; // Sao Jorge/RS
    5989: CodIBGE := 4318457; // Sao Jose Das Missoes/RS
    7345: CodIBGE := 4318465; // Sao Jose Do Herval/RS
    7343: CodIBGE := 4318481; // Sao Jose Do Hortencio/RS
    6059: CodIBGE := 4318499; // Sao Jose Do Inhacora/RS
    8873: CodIBGE := 4318507; // Sao Jose Do Norte/RS
    8875: CodIBGE := 4318606; // Sao Jose Do Ouro/RS
    1170: CodIBGE := 4318614; // Sao Jose Do Sul/RS
    6015: CodIBGE := 4318622; // Sao Jose Dos Ausentes/RS
    8877: CodIBGE := 4318705; // Sao Leopoldo/RS
    8879: CodIBGE := 4318804; // Sao Lourenco Do Sul/RS
    8881: CodIBGE := 4318903; // Sao Luiz Gonzaga/RS
    8883: CodIBGE := 4319000; // Sao Marcos/RS
    8885: CodIBGE := 4319109; // Sao Martinho/RS
    5793: CodIBGE := 4319125; // Sao Martinho Da Serra/RS
    7341: CodIBGE := 4319158; // Sao Miguel Das Missoes/RS
    8887: CodIBGE := 4319208; // Sao Nicolau/RS
    8889: CodIBGE := 4319307; // Sao Paulo Das Missoes/RS
    6043: CodIBGE := 4319356; // Sao Pedro Da Serra/RS
    1172: CodIBGE := 4319364; // Sao Pedro Das Missoes/RS
    6063: CodIBGE := 4319372; // Sao Pedro Do Butia/RS
    8891: CodIBGE := 4319406; // Sao Pedro Do Sul/RS
    8893: CodIBGE := 4319505; // Sao Sebastiao Do Cai/RS
    8895: CodIBGE := 4319604; // Sao Sepe/RS
    8897: CodIBGE := 4319703; // Sao Valentim/RS
    5997: CodIBGE := 4319711; // Sao Valentim Do Sul/RS
    6075: CodIBGE := 4319737; // Sao Valerio Do Sul/RS
    7293: CodIBGE := 4319752; // Sao Vendelino/RS
    8675: CodIBGE := 4319802; // Sao Vicente Do Sul/RS
    8899: CodIBGE := 4319901; // Sapiranga/RS
    8901: CodIBGE := 4320008; // Sapucaia Do Sul/RS
    8903: CodIBGE := 4320107; // Sarandi/RS
    8905: CodIBGE := 4320206; // Seberi/RS
    7335: CodIBGE := 4320230; // Sede Nova/RS
    7317: CodIBGE := 4320263; // Segredo/RS
    8907: CodIBGE := 4320305; // Selbach/RS
    1012: CodIBGE := 4320321; // Senador Salgado Filho/RS
    5781: CodIBGE := 4320354; // Sentinela Do Sul/RS
    8909: CodIBGE := 4320404; // Serafina Correa/RS
    6035: CodIBGE := 4320453; // Serio/RS
    8911: CodIBGE := 4320503; // Sertao/RS
    5761: CodIBGE := 4320552; // Sertao Santana/RS
    1014: CodIBGE := 4320578; // Sete De Setembro/RS
    8913: CodIBGE := 4320602; // Severiano De Almeida/RS
    7315: CodIBGE := 4320651; // Silveira Martins/RS
    5767: CodIBGE := 4320677; // Sinimbu/RS
    8917: CodIBGE := 4320701; // Sobradinho/RS
    8919: CodIBGE := 4320800; // Soledade/RS
    1016: CodIBGE := 4320859; // Tabai/RS
    8921: CodIBGE := 4320909; // Tapejara/RS
    8923: CodIBGE := 4321006; // Tapera/RS
    8925: CodIBGE := 4321105; // Tapes/RS
    8927: CodIBGE := 4321204; // Taquara/RS
    8929: CodIBGE := 4321303; // Taquari/RS
    7313: CodIBGE := 4321329; // Taquarucu Do Sul/RS
    8971: CodIBGE := 4321352; // Tavares/RS
    8931: CodIBGE := 4321402; // Tenente Portela/RS
    7333: CodIBGE := 4321436; // Terra De Areia/RS
    9821: CodIBGE := 4321451; // Teutonia/RS
    1174: CodIBGE := 4321469; // Tio Hugo/RS
    6077: CodIBGE := 4321477; // Tiradentes Do Sul/RS
    1018: CodIBGE := 4321493; // Toropi/RS
    8933: CodIBGE := 4321501; // Torres/RS
    8935: CodIBGE := 4321600; // Tramandai/RS
    6037: CodIBGE := 4321626; // Travesseiro/RS
    7331: CodIBGE := 4321634; // Tres Arroios/RS
    7329: CodIBGE := 4321667; // Tres Cachoeiras/RS
    8937: CodIBGE := 4321709; // Tres Coroas/RS
    8939: CodIBGE := 4321808; // Tres De Maio/RS
    5777: CodIBGE := 4321832; // Tres Forquilhas/RS
    7327: CodIBGE := 4321857; // Tres Palmeiras/RS
    8941: CodIBGE := 4321907; // Tres Passos/RS
    7325: CodIBGE := 4321956; // Trindade Do Sul/RS
    8943: CodIBGE := 4322004; // Triunfo/RS
    8945: CodIBGE := 4322103; // Tucunduva/RS
    7323: CodIBGE := 4322152; // Tunas/RS
    5979: CodIBGE := 4322186; // Tupanci Do Sul/RS
    8947: CodIBGE := 4322202; // Tupancireta/RS
    7321: CodIBGE := 4322251; // Tupandi/RS
    8949: CodIBGE := 4322301; // Tuparendi/RS
    1020: CodIBGE := 4322327; // Turucu/RS
    1022: CodIBGE := 4322343; // Ubiretama/RS
    5999: CodIBGE := 4322350; // Uniao Da Serra/RS
    1024: CodIBGE := 4322376; // Unistalda/RS
    8951: CodIBGE := 4322400; // Uruguaiana/RS
    8953: CodIBGE := 4322509; // Vacaria/RS
    1026: CodIBGE := 4322525; // Vale Verde/RS
    5769: CodIBGE := 4322533; // Vale Do Sol/RS
    6049: CodIBGE := 4322541; // Vale Real/RS
    7319: CodIBGE := 4322558; // Vanini/RS
    8955: CodIBGE := 4322608; // Venancio Aires/RS
    8957: CodIBGE := 4322707; // Vera Cruz/RS
    8959: CodIBGE := 4322806; // Veranopolis/RS
    1028: CodIBGE := 4322855; // Vespasiano Correa/RS
    8961: CodIBGE := 4322905; // Viadutos/RS
    8963: CodIBGE := 4323002; // Viamao/RS
    8965: CodIBGE := 4323101; // Vicente Dutra/RS
    8969: CodIBGE := 4323200; // Victor Graeff/RS
    7311: CodIBGE := 4323309; // Vila Flores/RS
    1030: CodIBGE := 4323358; // Vila Langaro/RS
    7309: CodIBGE := 4323408; // Vila Maria/RS
    5795: CodIBGE := 4323457; // Vila Nova Do Sul/RS
    7307: CodIBGE := 4323507; // Vista Alegre/RS
    7305: CodIBGE := 4323606; // Vista Alegre Do Prata/RS
    7303: CodIBGE := 4323705; // Vista Gaucha/RS
    6053: CodIBGE := 4323754; // Vitoria Das Missoes/RS
    1176: CodIBGE := 4323770; // Westfalia/RS
    5785: CodIBGE := 4323804; // Xangri-La/RS
    9003: CodIBGE := 5000203; // Agua Clara/MS
    0141: CodIBGE := 5000252; // Alcinopolis/MS
    9011: CodIBGE := 5000609; // Amambai/MS
    9013: CodIBGE := 5000708; // Anastacio/MS
    9015: CodIBGE := 5000807; // Anaurilandia/MS
    9169: CodIBGE := 5000856; // Angelica/MS
    9017: CodIBGE := 5000906; // Antonio Joao/MS
    9019: CodIBGE := 5001003; // Aparecida Do Taboado/MS
    9021: CodIBGE := 5001102; // Aquidauana/MS
    9171: CodIBGE := 5001243; // Aral Moreira/MS
    9029: CodIBGE := 5001508; // Bandeirantes/MS
    9037: CodIBGE := 5001904; // Bataguassu/MS
    9039: CodIBGE := 5002001; // Bataypora/MS
    9041: CodIBGE := 5002100; // Bela Vista/MS
    9801: CodIBGE := 5002159; // Bodoquena/MS
    9043: CodIBGE := 5002209; // Bonito/MS
    9045: CodIBGE := 5002308; // Brasilandia/MS
    9055: CodIBGE := 5002407; // Caarapo/MS
    9049: CodIBGE := 5002605; // Camapua/MS
    9051: CodIBGE := 5002704; // Campo Grande/MS
    9053: CodIBGE := 5002803; // Caracol/MS
    9057: CodIBGE := 5002902; // Cassilandia/MS
    9787: CodIBGE := 5002951; // Chapadao Do Sul/MS
    9061: CodIBGE := 5003108; // Corguinho/MS
    9997: CodIBGE := 5003157; // Coronel Sapucaia/MS
    9063: CodIBGE := 5003207; // Corumba/MS
    9803: CodIBGE := 5003256; // Costa Rica/MS
    9065: CodIBGE := 5003306; // Coxim/MS
    9175: CodIBGE := 5003454; // Deodapolis/MS
    9793: CodIBGE := 5003488; // Dois Irmaos Do Buriti/MS
    9805: CodIBGE := 5003504; // Douradina/MS
    9073: CodIBGE := 5003702; // Dourados/MS
    9173: CodIBGE := 5003751; // Eldorado/MS
    9075: CodIBGE := 5003801; // Fatima Do Sul/MS
    1178: CodIBGE := 5003900; // Figueirao/MS
    9079: CodIBGE := 5004007; // Gloria De Dourados/MS
    9081: CodIBGE := 5004106; // Guia Lopes Da Laguna/MS
    9085: CodIBGE := 5004304; // Iguatemi/MS
    9087: CodIBGE := 5004403; // Inocencia/MS
    9089: CodIBGE := 5004502; // Itapora/MS
    9807: CodIBGE := 5004601; // Itaquirai/MS
    9093: CodIBGE := 5004700; // Ivinhema/MS
    0161: CodIBGE := 5004809; // Japora/MS
    9097: CodIBGE := 5004908; // Jaraguari/MS
    9099: CodIBGE := 5005004; // Jardim/MS
    9101: CodIBGE := 5005103; // Jatei/MS
    9923: CodIBGE := 5005152; // Juti/MS
    9103: CodIBGE := 5005202; // Ladario/MS
    0163: CodIBGE := 5005251; // Laguna Carapa/MS
    9107: CodIBGE := 5005400; // Maracaju/MS
    9111: CodIBGE := 5005608; // Miranda/MS
    9179: CodIBGE := 5005681; // Mundo Novo/MS
    9113: CodIBGE := 5005707; // Navirai/MS
    9115: CodIBGE := 5005806; // Nioaque/MS
    0143: CodIBGE := 5006002; // Nova Alvorada Do Sul/MS
    9123: CodIBGE := 5006200; // Nova Andradina/MS
    0159: CodIBGE := 5006259; // Novo Horizonte Do Sul/MS
    1196: CodIBGE := 5006275; // Paraiso Das Aguas/MS
    9125: CodIBGE := 5006309; // Paranaiba/MS
    9739: CodIBGE := 5006358; // Paranhos/MS
    9127: CodIBGE := 5006408; // Pedro Gomes/MS
    9131: CodIBGE := 5006606; // Ponta Pora/MS
    9137: CodIBGE := 5006903; // Porto Murtinho/MS
    9141: CodIBGE := 5007109; // Ribas Do Rio Pardo/MS
    9143: CodIBGE := 5007208; // Rio Brilhante/MS
    9145: CodIBGE := 5007307; // Rio Negro/MS
    9147: CodIBGE := 5007406; // Rio Verde De Mato Grosso/MS
    9149: CodIBGE := 5007505; // Rochedo/MS
    9745: CodIBGE := 5007554; // Santa Rita Do Pardo/MS
    9809: CodIBGE := 5007695; // Sao Gabriel Do Oeste/MS
    9813: CodIBGE := 5007703; // Sete Quedas/MS
    9811: CodIBGE := 5007802; // Selviria/MS
    9157: CodIBGE := 5007901; // Sidrolandia/MS
    9757: CodIBGE := 5007935; // Sonora/MS
    9815: CodIBGE := 5007950; // Tacuru/MS
    9817: CodIBGE := 5007976; // Taquarussu/MS
    9159: CodIBGE := 5008008; // Terenos/MS
    9165: CodIBGE := 5008305; // Tres Lagoas/MS
    9187: CodIBGE := 5008404; // Vicentina/MS
    9001: CodIBGE := 5100102; // Acorizal/MT
    9191: CodIBGE := 5100201; // Agua Boa/MT
    8987: CodIBGE := 5100250; // Alta Floresta/MT
    9005: CodIBGE := 5100300; // Alto Araguaia/MT
    0127: CodIBGE := 5100359; // Alto Boa Vista/MT
    9007: CodIBGE := 5100409; // Alto Garcas/MT
    9009: CodIBGE := 5100508; // Alto Paraguai/MT
    9911: CodIBGE := 5100607; // Alto Taquari/MT
    9773: CodIBGE := 5100805; // Apiacas/MT
    9869: CodIBGE := 5101001; // Araguaiana/MT
    9023: CodIBGE := 5101209; // Araguainha/MT
    8989: CodIBGE := 5101258; // Araputanga/MT
    9025: CodIBGE := 5101308; // Arenapolis/MT
    9027: CodIBGE := 5101407; // Aripuana/MT
    9031: CodIBGE := 5101605; // Barao De Melgaco/MT
    9033: CodIBGE := 5101704; // Barra Do Bugres/MT
    9035: CodIBGE := 5101803; // Barra Do Garcas/MT
    1078: CodIBGE := 5101852; // Bom Jesus Do Araguaia/MT
    9873: CodIBGE := 5101902; // Brasnorte/MT
    9047: CodIBGE := 5102504; // Caceres/MT
    9863: CodIBGE := 5102603; // Campinapolis/MT
    9777: CodIBGE := 5102637; // Campo Novo Do Parecis/MT
    9779: CodIBGE := 5102678; // Campo Verde/MT
    1032: CodIBGE := 5102686; // Campos De Julio/MT
    0129: CodIBGE := 5102694; // Canabrava Do Norte/MT
    9193: CodIBGE := 5102702; // Canarana/MT
    1034: CodIBGE := 5102793; // Carlinda/MT
    9783: CodIBGE := 5102850; // Castanheira/MT
    9059: CodIBGE := 5103007; // Chapada Dos Guimaraes/MT
    9789: CodIBGE := 5103056; // Claudia/MT
    9865: CodIBGE := 5103106; // Cocalinho/MT
    8979: CodIBGE := 5103205; // Colider/MT
    1080: CodIBGE := 5103254; // Colniza/MT
    9883: CodIBGE := 5103304; // Comodoro/MT
    0131: CodIBGE := 5103353; // Confresa/MT
    1082: CodIBGE := 5103361; // Conquista D Oeste/MT
    0089: CodIBGE := 5103379; // Cotriguacu/MT
    9067: CodIBGE := 5103403; // Cuiaba/MT
    1084: CodIBGE := 5103437; // Curvelandia/MT
    9833: CodIBGE := 5103452; // Denise/MT
    9069: CodIBGE := 5103502; // Diamantino/MT
    9071: CodIBGE := 5103601; // Dom Aquino/MT
    1036: CodIBGE := 5103700; // Feliz Natal/MT
    9881: CodIBGE := 5103809; // Figueiropolis D Oeste/MT
    1038: CodIBGE := 5103858; // Gaucha Do Norte/MT
    9077: CodIBGE := 5103908; // General Carneiro/MT
    0135: CodIBGE := 5103957; // Gloria D Oeste/MT
    9887: CodIBGE := 5104104; // Guaranta Do Norte/MT
    9083: CodIBGE := 5104203; // Guiratinga/MT
    9877: CodIBGE := 5104500; // Indiavai/MT
    1184: CodIBGE := 5104526; // Ipiranga Do Norte/MT
    1186: CodIBGE := 5104542; // Itanhanga/MT
    9901: CodIBGE := 5104559; // Itauba/MT
    9091: CodIBGE := 5104609; // Itiquira/MT
    9095: CodIBGE := 5104807; // Jaciara/MT
    9861: CodIBGE := 5104906; // Jangada/MT
    8991: CodIBGE := 5105002; // Jauru/MT
    9819: CodIBGE := 5105101; // Juara/MT
    9831: CodIBGE := 5105150; // Juina/MT
    9921: CodIBGE := 5105176; // Juruena/MT
    9189: CodIBGE := 5105200; // Juscimeira/MT
    0137: CodIBGE := 5105234; // Lambari D Oeste/MT
    9925: CodIBGE := 5105259; // Lucas Do Rio Verde/MT
    9105: CodIBGE := 5105309; // Luciara/MT
    9109: CodIBGE := 5105507; // Vila Bela Da Santissima Trindade/MT
    9899: CodIBGE := 5105580; // Marcelandia/MT
    9929: CodIBGE := 5105606; // Matupa/MT
    9177: CodIBGE := 5105622; // Mirassol D Oeste/MT
    9117: CodIBGE := 5105903; // Nobres/MT
    9119: CodIBGE := 5106000; // Nortelandia/MT
    9121: CodIBGE := 5106109; // Nossa Senhora Do Livramento/MT
    0117: CodIBGE := 5106158; // Nova Bandeirantes/MT
    1086: CodIBGE := 5106174; // Nova Nazare/MT
    1040: CodIBGE := 5106182; // Nova Lacerda/MT
    1088: CodIBGE := 5106190; // Nova Santa Helena/MT
    8981: CodIBGE := 5106208; // Nova Brasilandia/MT
    9889: CodIBGE := 5106216; // Nova Canaa Do Norte/MT
    9937: CodIBGE := 5106224; // Nova Mutum/MT
    9893: CodIBGE := 5106232; // Nova Olimpia/MT
    1042: CodIBGE := 5106240; // Nova Ubirata/MT
    9195: CodIBGE := 5106257; // Nova Xavantina/MT
    1044: CodIBGE := 5106265; // Novo Mundo/MT
    9903: CodIBGE := 5106273; // Novo Horizonte Do Norte/MT
    9867: CodIBGE := 5106281; // Novo Sao Joaquim/MT
    9885: CodIBGE := 5106299; // Paranaita/MT
    8983: CodIBGE := 5106307; // Paranatinga/MT
    1090: CodIBGE := 5106315; // Novo Santo Antonio/MT
    9181: CodIBGE := 5106372; // Pedra Preta/MT
    9891: CodIBGE := 5106422; // Peixoto De Azevedo/MT
    0091: CodIBGE := 5106455; // Planalto Da Serra/MT
    9129: CodIBGE := 5106505; // Pocone/MT
    0095: CodIBGE := 5106653; // Pontal Do Araguaia/MT
    9133: CodIBGE := 5106703; // Ponte Branca/MT
    8999: CodIBGE := 5106752; // Pontes E Lacerda/MT
    9895: CodIBGE := 5106778; // Porto Alegre Do Norte/MT
    9135: CodIBGE := 5106802; // Porto Dos Gauchos/MT
    9875: CodIBGE := 5106828; // Porto Esperidiao/MT
    0101: CodIBGE := 5106851; // Porto Estrela/MT
    9139: CodIBGE := 5107008; // Poxoreo/MT
    9871: CodIBGE := 5107040; // Primavera Do Leste/MT
    0097: CodIBGE := 5107065; // Querencia/MT
    8993: CodIBGE := 5107107; // Sao Jose Dos Quatro Marcos/MT
    9879: CodIBGE := 5107156; // Reserva Do Cabacal/MT
    9741: CodIBGE := 5107180; // Ribeirao Cascalheira/MT
    0099: CodIBGE := 5107198; // Ribeiraozinho/MT
    8995: CodIBGE := 5107206; // Rio Branco/MT
    0123: CodIBGE := 5107248; // Santa Carmem/MT
    0115: CodIBGE := 5107263; // Santo Afonso/MT
    6087: CodIBGE := 5107297; // Sao Jose Do Povo/MT
    9199: CodIBGE := 5107305; // Sao Jose Do Rio Claro/MT
    0133: CodIBGE := 5107354; // Sao Jose Do Xingu/MT
    0093: CodIBGE := 5107404; // Sao Pedro Da Cipa/MT
    1092: CodIBGE := 5107578; // Rondolandia/MT
    9151: CodIBGE := 5107602; // Rondonopolis/MT
    9153: CodIBGE := 5107701; // Rosario Oeste/MT
    1094: CodIBGE := 5107743; // Santa Cruz Do Xingu/MT
    8997: CodIBGE := 5107750; // Salto Do Ceu/MT
    1096: CodIBGE := 5107768; // Santa Rita Do Trivelato/MT
    9197: CodIBGE := 5107776; // Santa Terezinha/MT
    1098: CodIBGE := 5107792; // Santo Antonio Do Leste/MT
    9155: CodIBGE := 5107800; // Santo Antonio Do Leverger/MT
    9183: CodIBGE := 5107859; // Sao Felix Do Araguaia/MT
    1046: CodIBGE := 5107875; // Sapezal/MT
    1100: CodIBGE := 5107883; // Serra Nova Dourada/MT
    8985: CodIBGE := 5107909; // Sinop/MT
    9907: CodIBGE := 5107925; // Sorriso/MT
    0125: CodIBGE := 5107941; // Tabapora/MT
    9185: CodIBGE := 5107958; // Tangara Da Serra/MT
    9763: CodIBGE := 5108006; // Tapurah/MT
    9909: CodIBGE := 5108055; // Terra Nova Do Norte/MT
    9161: CodIBGE := 5108105; // Tesouro/MT
    9163: CodIBGE := 5108204; // Torixoreu/MT
    1048: CodIBGE := 5108303; // Uniao Do Sul/MT
    1102: CodIBGE := 5108352; // Vale De Sao Domingos/MT
    9167: CodIBGE := 5108402; // Varzea Grande/MT
    9905: CodIBGE := 5108501; // Vera/MT
    9897: CodIBGE := 5108600; // Vila Rica/MT
    0121: CodIBGE := 5108808; // Nova Guarita/MT
    0103: CodIBGE := 5108857; // Nova Marilandia/MT
    0111: CodIBGE := 5108907; // Nova Maringa/MT
    0119: CodIBGE := 5108956; // Nova Monte Verde/MT
    1050: CodIBGE := 5200050; // Abadia De Goias/GO
    9201: CodIBGE := 5200100; // Abadiania/GO
    9645: CodIBGE := 5200134; // Acreuna/GO
    9769: CodIBGE := 5200159; // Adelandia/GO
    9771: CodIBGE := 5200175; // Agua Fria De Goias/GO
    9203: CodIBGE := 5200209; // Agua Limpa/GO
    1052: CodIBGE := 5200258; // Aguas Lindas De Goias/GO
    9205: CodIBGE := 5200308; // Alexania/GO
    9209: CodIBGE := 5200506; // Aloandia/GO
    0085: CodIBGE := 5200555; // Alto Horizonte/GO
    9211: CodIBGE := 5200605; // Alto Paraiso De Goias/GO
    9215: CodIBGE := 5200803; // Alvorada Do Norte/GO
    1054: CodIBGE := 5200829; // Amaralina/GO
    9661: CodIBGE := 5200852; // Americano Do Brasil/GO
    9217: CodIBGE := 5200902; // Amorinopolis/GO
    9221: CodIBGE := 5201108; // Anapolis/GO
    9223: CodIBGE := 5201207; // Anhanguera/GO
    9225: CodIBGE := 5201306; // Anicuns/GO
    9227: CodIBGE := 5201405; // Aparecida De Goiania/GO
    0071: CodIBGE := 5201454; // Aparecida Do Rio Doce/GO
    9229: CodIBGE := 5201504; // Apore/GO
    9231: CodIBGE := 5201603; // Aracu/GO
    9233: CodIBGE := 5201702; // Aragarcas/GO
    9235: CodIBGE := 5201801; // Aragoiania/GO
    9669: CodIBGE := 5202155; // Araguapaz/GO
    9671: CodIBGE := 5202353; // Arenopolis/GO
    9249: CodIBGE := 5202502; // Aruana/GO
    9251: CodIBGE := 5202601; // Aurilandia/GO
    9255: CodIBGE := 5202809; // Avelinopolis/GO
    9261: CodIBGE := 5203104; // Baliza/GO
    9263: CodIBGE := 5203203; // Barro Alto/GO
    9265: CodIBGE := 5203302; // Bela Vista De Goias/GO
    9267: CodIBGE := 5203401; // Bom Jardim De Goias/GO
    9269: CodIBGE := 5203500; // Bom Jesus De Goias/GO
    9775: CodIBGE := 5203559; // Bonfinopolis/GO
    1056: CodIBGE := 5203575; // Bonopolis/GO
    9271: CodIBGE := 5203609; // Brazabrantes/GO
    9275: CodIBGE := 5203807; // Britania/GO
    9277: CodIBGE := 5203906; // Buriti Alegre/GO
    0063: CodIBGE := 5203939; // Buriti De Goias/GO
    0061: CodIBGE := 5203962; // Buritinopolis/GO
    9279: CodIBGE := 5204003; // Cabeceiras/GO
    9281: CodIBGE := 5204102; // Cachoeira Alta/GO
    9283: CodIBGE := 5204201; // Cachoeira De Goias/GO
    9673: CodIBGE := 5204250; // Cachoeira Dourada/GO
    9285: CodIBGE := 5204300; // Cacu/GO
    9287: CodIBGE := 5204409; // Caiaponia/GO
    9289: CodIBGE := 5204508; // Caldas Novas/GO
    0031: CodIBGE := 5204557; // Caldazinha/GO
    9291: CodIBGE := 5204607; // Campestre De Goias/GO
    9687: CodIBGE := 5204656; // Campinacu/GO
    9293: CodIBGE := 5204706; // Campinorte/GO
    9295: CodIBGE := 5204805; // Campo Alegre De Goias/GO
    1070: CodIBGE := 5204854; // Campo Limpo De Goias/GO
    9297: CodIBGE := 5204904; // Campos Belos/GO
    9781: CodIBGE := 5204953; // Campos Verdes/GO
    9299: CodIBGE := 5205000; // Carmo Do Rio Verde/GO
    0081: CodIBGE := 5205059; // Castelandia/GO
    9301: CodIBGE := 5205109; // Catalao/GO
    9303: CodIBGE := 5205208; // Caturai/GO
    9305: CodIBGE := 5205307; // Cavalcante/GO
    9307: CodIBGE := 5205406; // Ceres/GO
    9785: CodIBGE := 5205455; // Cezarina/GO
    0073: CodIBGE := 5205471; // Chapadao Do Ceu/GO
    0077: CodIBGE := 5205497; // Cidade Ocidental/GO
    0055: CodIBGE := 5205513; // Cocalzinho De Goias/GO
    9791: CodIBGE := 5205521; // Colinas Do Sul/GO
    9315: CodIBGE := 5205703; // Corrego Do Ouro/GO
    9317: CodIBGE := 5205802; // Corumba De Goias/GO
    9319: CodIBGE := 5205901; // Corumbaiba/GO
    9325: CodIBGE := 5206206; // Cristalina/GO
    9327: CodIBGE := 5206305; // Cristianopolis/GO
    9329: CodIBGE := 5206404; // Crixas/GO
    9331: CodIBGE := 5206503; // Crominia/GO
    9333: CodIBGE := 5206602; // Cumari/GO
    9335: CodIBGE := 5206701; // Damianopolis/GO
    9337: CodIBGE := 5206800; // Damolandia/GO
    9339: CodIBGE := 5206909; // Davinopolis/GO
    9343: CodIBGE := 5207105; // Diorama/GO
    9675: CodIBGE := 5207253; // Doverlandia/GO
    9795: CodIBGE := 5207352; // Edealina/GO
    9349: CodIBGE := 5207402; // Edeia/GO
    9351: CodIBGE := 5207501; // Estrela Do Norte/GO
    9797: CodIBGE := 5207535; // Faina/GO
    9353: CodIBGE := 5207600; // Fazenda Nova/GO
    9357: CodIBGE := 5207808; // Firminopolis/GO
    9359: CodIBGE := 5207907; // Flores De Goias/GO
    9361: CodIBGE := 5208004; // Formosa/GO
    9363: CodIBGE := 5208103; // Formoso/GO
    1072: CodIBGE := 5208152; // Gameleira De Goias/GO
    9309: CodIBGE := 5208301; // Divinopolis De Goias/GO
    9367: CodIBGE := 5208400; // Goianapolis/GO
    9369: CodIBGE := 5208509; // Goiandira/GO
    9371: CodIBGE := 5208608; // Goianesia/GO
    9373: CodIBGE := 5208707; // Goiania/GO
    9375: CodIBGE := 5208806; // Goianira/GO
    9377: CodIBGE := 5208905; // Goias/GO
    9379: CodIBGE := 5209101; // Goiatuba/GO
    9799: CodIBGE := 5209150; // Gouvelandia/GO
    9381: CodIBGE := 5209200; // Guapo/GO
    0065: CodIBGE := 5209291; // Guaraita/GO
    9383: CodIBGE := 5209408; // Guarani De Goias/GO
    9993: CodIBGE := 5209457; // Guarinos/GO
    9387: CodIBGE := 5209606; // Heitorai/GO
    9389: CodIBGE := 5209705; // Hidrolandia/GO
    9391: CodIBGE := 5209804; // Hidrolina/GO
    9393: CodIBGE := 5209903; // Iaciara/GO
    0069: CodIBGE := 5209937; // Inaciolandia/GO
    9681: CodIBGE := 5209952; // Indiara/GO
    9395: CodIBGE := 5210000; // Inhumas/GO
    9397: CodIBGE := 5210109; // Ipameri/GO
    1074: CodIBGE := 5210158; // Ipiranga De Goias/GO
    9399: CodIBGE := 5210208; // Ipora/GO
    9401: CodIBGE := 5210307; // Israelandia/GO
    9403: CodIBGE := 5210406; // Itaberai/GO
    9919: CodIBGE := 5210562; // Itaguari/GO
    9407: CodIBGE := 5210604; // Itaguaru/GO
    9411: CodIBGE := 5210802; // Itaja/GO
    9413: CodIBGE := 5210901; // Itapaci/GO
    9415: CodIBGE := 5211008; // Itapirapua/GO
    9419: CodIBGE := 5211206; // Itapuranga/GO
    9421: CodIBGE := 5211305; // Itaruma/GO
    9423: CodIBGE := 5211404; // Itaucu/GO
    9425: CodIBGE := 5211503; // Itumbiara/GO
    9427: CodIBGE := 5211602; // Ivolandia/GO
    9429: CodIBGE := 5211701; // Jandaia/GO
    9431: CodIBGE := 5211800; // Jaragua/GO
    9433: CodIBGE := 5211909; // Jatai/GO
    9435: CodIBGE := 5212006; // Jaupaci/GO
    0049: CodIBGE := 5212055; // Jesupolis/GO
    9437: CodIBGE := 5212105; // Joviania/GO
    9439: CodIBGE := 5212204; // Jussara/GO
    1076: CodIBGE := 5212253; // Lagoa Santa/GO
    9443: CodIBGE := 5212303; // Leopoldo De Bulhoes/GO
    9445: CodIBGE := 5212501; // Luziania/GO
    9447: CodIBGE := 5212600; // Mairipotaba/GO
    9449: CodIBGE := 5212709; // Mambai/GO
    9451: CodIBGE := 5212808; // Mara Rosa/GO
    9453: CodIBGE := 5212907; // Marzagao/GO
    9927: CodIBGE := 5212956; // Matrincha/GO
    9457: CodIBGE := 5213004; // Maurilandia/GO
    9931: CodIBGE := 5213053; // Mimoso De Goias/GO
    9647: CodIBGE := 5213087; // Minacu/GO
    9459: CodIBGE := 5213103; // Mineiros/GO
    9465: CodIBGE := 5213400; // Moipora/GO
    9467: CodIBGE := 5213509; // Monte Alegre De Goias/GO
    9471: CodIBGE := 5213707; // Montes Claros De Goias/GO
    9933: CodIBGE := 5213756; // Montividiu/GO
    0079: CodIBGE := 5213772; // Montividiu Do Norte/GO
    9473: CodIBGE := 5213806; // Morrinhos/GO
    9935: CodIBGE := 5213855; // Morro Agudo De Goias/GO
    9475: CodIBGE := 5213905; // Mossamedes/GO
    9477: CodIBGE := 5214002; // Mozarlandia/GO
    9651: CodIBGE := 5214051; // Mundo Novo/GO
    9479: CodIBGE := 5214101; // Mutunopolis/GO
    9485: CodIBGE := 5214408; // Nazario/GO
    9487: CodIBGE := 5214507; // Neropolis/GO
    9489: CodIBGE := 5214606; // Niquelandia/GO
    9491: CodIBGE := 5214705; // Nova America/GO
    9493: CodIBGE := 5214804; // Nova Aurora/GO
    9653: CodIBGE := 5214838; // Nova Crixas/GO
    9655: CodIBGE := 5214861; // Nova Gloria/GO
    0087: CodIBGE := 5214879; // Nova Iguacu De Goias/GO
    9495: CodIBGE := 5214903; // Nova Roma/GO
    9497: CodIBGE := 5215009; // Nova Veneza/GO
    9501: CodIBGE := 5215207; // Novo Brasil/GO
    1058: CodIBGE := 5215231; // Novo Gama/GO
    9735: CodIBGE := 5215256; // Novo Planalto/GO
    9503: CodIBGE := 5215306; // Orizona/GO
    9505: CodIBGE := 5215405; // Ouro Verde De Goias/GO
    9507: CodIBGE := 5215504; // Ouvidor/GO
    9509: CodIBGE := 5215603; // Padre Bernardo/GO
    9737: CodIBGE := 5215652; // Palestina De Goias/GO
    9511: CodIBGE := 5215702; // Palmeiras De Goias/GO
    9513: CodIBGE := 5215801; // Palmelo/GO
    9515: CodIBGE := 5215900; // Palminopolis/GO
    9517: CodIBGE := 5216007; // Panama/GO
    9455: CodIBGE := 5216304; // Paranaiguara/GO
    9523: CodIBGE := 5216403; // Parauna/GO
    0075: CodIBGE := 5216452; // Perolandia/GO
    9531: CodIBGE := 5216809; // Petrolina De Goias/GO
    9535: CodIBGE := 5216908; // Pilar De Goias/GO
    9539: CodIBGE := 5217104; // Piracanjuba/GO
    9541: CodIBGE := 5217203; // Piranhas/GO
    9543: CodIBGE := 5217302; // Pirenopolis/GO
    9545: CodIBGE := 5217401; // Pires Do Rio/GO
    9595: CodIBGE := 5217609; // Planaltina/GO
    9549: CodIBGE := 5217708; // Pontalina/GO
    9555: CodIBGE := 5218003; // Porangatu/GO
    1060: CodIBGE := 5218052; // Porteirao/GO
    9557: CodIBGE := 5218102; // Portelandia/GO
    9561: CodIBGE := 5218300; // Posse/GO
    0051: CodIBGE := 5218391; // Professor Jamil/GO
    9563: CodIBGE := 5218508; // Quirinopolis/GO
    9565: CodIBGE := 5218607; // Rialma/GO
    9567: CodIBGE := 5218706; // Rianapolis/GO
    9995: CodIBGE := 5218789; // Rio Quente/GO
    9571: CodIBGE := 5218805; // Rio Verde/GO
    9573: CodIBGE := 5218904; // Rubiataba/GO
    9575: CodIBGE := 5219001; // Sanclerlandia/GO
    9577: CodIBGE := 5219100; // Santa Barbara De Goias/GO
    9579: CodIBGE := 5219209; // Santa Cruz De Goias/GO
    9743: CodIBGE := 5219258; // Santa Fe De Goias/GO
    9581: CodIBGE := 5219308; // Santa Helena De Goias/GO
    9689: CodIBGE := 5219357; // Santa Isabel/GO
    9583: CodIBGE := 5219407; // Santa Rita Do Araguaia/GO
    1062: CodIBGE := 5219456; // Santa Rita Do Novo Destino/GO
    9585: CodIBGE := 5219506; // Santa Rosa De Goias/GO
    9587: CodIBGE := 5219605; // Santa Tereza De Goias/GO
    9589: CodIBGE := 5219704; // Santa Terezinha De Goias/GO
    0083: CodIBGE := 5219712; // Santo Antonio Da Barra/GO
    0053: CodIBGE := 5219738; // Santo Antonio De Goias/GO
    9677: CodIBGE := 5219753; // Santo Antonio Do Descoberto/GO
    9591: CodIBGE := 5219803; // Sao Domingos/GO
    9593: CodIBGE := 5219902; // Sao Francisco De Goias/GO
    9597: CodIBGE := 5220009; // Sao Joao D Alianca/GO
    9747: CodIBGE := 5220058; // Sao Joao Da Parauna/GO
    9599: CodIBGE := 5220108; // Sao Luis De Montes Belos/GO
    9749: CodIBGE := 5220157; // Sao Luiz Do Norte/GO
    9601: CodIBGE := 5220207; // Sao Miguel Do Araguaia/GO
    9751: CodIBGE := 5220264; // Sao Miguel Do Passa Quatro/GO
    1064: CodIBGE := 5220280; // Sao Patricio/GO
    9605: CodIBGE := 5220405; // Sao Simao/GO
    9753: CodIBGE := 5220454; // Senador Canedo/GO
    9607: CodIBGE := 5220504; // Serranopolis/GO
    9609: CodIBGE := 5220603; // Silvania/GO
    9755: CodIBGE := 5220686; // Simolandia/GO
    9611: CodIBGE := 5220702; // Sitio D Abadia/GO
    9617: CodIBGE := 5221007; // Taquaral De Goias/GO
    9759: CodIBGE := 5221080; // Teresina De Goias/GO
    0057: CodIBGE := 5221197; // Terezopolis De Goias/GO
    9623: CodIBGE := 5221304; // Tres Ranchos/GO
    9625: CodIBGE := 5221403; // Trindade/GO
    9761: CodIBGE := 5221452; // Trombas/GO
    9631: CodIBGE := 5221502; // Turvania/GO
    9765: CodIBGE := 5221551; // Turvelandia/GO
    0059: CodIBGE := 5221577; // Uirapuru/GO
    9633: CodIBGE := 5221601; // Uruacu/GO
    9635: CodIBGE := 5221700; // Uruana/GO
    9637: CodIBGE := 5221809; // Urutai/GO
    1066: CodIBGE := 5221858; // Valparaiso De Goias/GO
    9639: CodIBGE := 5221908; // Varjao/GO
    9641: CodIBGE := 5222005; // Vianopolis/GO
    9657: CodIBGE := 5222054; // Vicentinopolis/GO
    0067: CodIBGE := 5222203; // Vila Boa/GO
    1068: CodIBGE := 5222302; // Vila Propicio/GO
    9701: CodIBGE := 5300108; // Brasilia/DF
  else
    CodIBGE := -1;
  end;

  Result := inttostr(CodIBGE);
end;

function TipoEmissaoToStr(const t: TTipoEmissao): string;
begin
  Result := EnumeradoToStr(t,
                           ['N', 'C'],
                           [teNormalNFSe, teContigenciaNFSe]);
end;

function StrToTipoEmissao(out ok: boolean; const s: string): TTipoEmissao;
begin
  Result := StrToEnumerado(ok, s,
                           ['N', 'C'],
                           [teNormalNFSe, teContigenciaNFSe]);
end;

function EmpreitadaGlobalToStr(const t: TEmpreitadaGlobal): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [EgConstrucaoCivil, EgOutros]);
end;

function StrToEmpreitadaGlobal(out ok: boolean; const s: string): TEmpreitadaGlobal;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [EgConstrucaoCivil, EgOutros]);
end;

function ObterDescricaoServico(const cCodigo: string): string;
var
  i: Integer;
  PathArquivo: string;
  List: TstringList;
begin
  Result := '';
  PathArquivo := PathWithDelim(ExtractFilePath(ParamStr(0))) + 'TabServicos.txt';

  if (FileExists(PathArquivo)) and (cCodigo <> '') then
  begin
    List := TstringList.Create;

    try
      List.LoadFromFile(PathArquivo);
      i := 0;

      while (i < list.count) and (Result = '') do
      begin
        if pos(cCodigo, List[i]) > 0 then
          Result := Trim(stringReplace(list[i], ccodigo, '', []));
        inc(i);
      end;
    finally
      List.free;
    end;
  end;
end;

function ChaveAcesso(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ: string;
  ASerie: Integer; ANumero, ACodigo: Integer; AModelo: Integer): string;
var
  vUF, vDataEmissao, vSerie, vNumero,
  vCodigo, vModelo: string;
begin
  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vModelo      := Poem_Zeros(AModelo, 2);
  vSerie       := Poem_Zeros(ASerie, 3);
  vNumero      := Poem_Zeros(ANumero, 9);
  vCodigo      := Poem_Zeros(ACodigo, 9);

  Result := vUF + vDataEmissao + ACNPJ + vModelo + vSerie + vNumero + vCodigo;
end;

function VersaoXML(const AXML: string): string;
var
  i: Integer;
begin
  i := Pos('<Cidade>', AXML);

  if i > 0 then
    Result := '1'
  else
    Result := '2';
end;

function GerarNomeNFSe(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ: string;
                       ANumero: Int64; AModelo: Integer): string;
var
  vUF, vDataEmissao, vNumero, vModelo: string;
begin
  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vModelo      := Poem_Zeros(AModelo, 2);
  vNumero      := Poem_Zeros(ANumero, 15);

  Result := vUF + vDataEmissao + ACNPJ + vModelo + vNumero;
end;

function StrToVersaoNFSe(out ok: Boolean; const s: string): TVersaoNFSe;
begin
  Result := StrToEnumerado(ok, s, ['1.00', '1.01', '1.02', '1.03',
                                   '2.00', '2.01', '2.02', '2.03', '2.04'],
                                  [ve100, ve101, ve102, ve103,
                                   ve200, ve201, ve202, ve203, ve204]);
end;

function VersaoNFSeToStr(const t: TVersaoNFSe): string;
begin
  Result := EnumeradoToStr(t, ['1.00', '1.01', '1.02', '1.03',
                               '2.00', '2.01', '2.02', '2.03', '2.04'],
                              [ve100, ve101, ve102, ve103,
                               ve200, ve201, ve202, ve203, ve204]);
end;

function OperacaoDescricao(const t: TOperacao): string;
begin
  case t of
    toSemDeducao             : Result := 'A - Sem Dedução';
    toComDeducaoMateriais    : Result := 'B - Com Dedução/Materiais';
    toImuneIsenta            : Result := 'C - Imune/Isenta de ISSQN';
    toDevolucaoSimplesRemessa: Result := 'D - Devolução/Simples Remessa';
    toIntermediacao          : Result := 'J - Intermediario';
  else
    Result := '';
  end;
end;

function TipoEmissaoDescricao(const t: TTipoEmissao): string;
begin
  case t of
    teNormalNFSe     : Result := 'N - Normal';
    teContigenciaNFSe: Result := 'C - Contingência';
  end;
end;

function TipoFreteToStr(const t: TnfseFrete): string;
begin
  Result := EnumeradoToStr(t,
                           ['0', '1'],
                           [tfPrestador, tfTomador]);
end;

function StrToTipoFrete(out ok: boolean; const s: string): TnfseFrete;
begin
  Result := StrToEnumerado(ok, s,
                          ['0', '1'],
                          [tfPrestador, tfTomador]);
end;

function CanhotoToStr(const t: TnfseCanhoto): string;
begin
  Result := EnumeradoToStr(t,
                           ['0', '1', '2'],
                           [tcNenhum, tcCabecalho, tcRodape]);
end;

function StrToCanhoto(out ok: boolean; const s: string): TnfseCanhoto;
begin
  Result := StrToEnumerado(ok, s,
                           ['0', '1', '2'],
                           [tcNenhum, tcCabecalho, tcRodape]);
end;

function RegRecToStr(const t: TRegRec): string;
begin
  Result := EnumeradoToStr(t,
                           ['', '00', '02', '03', '04', '07', '08', '09', '11', '12'],
                           [regNenhum, regMovimento, regCancelado, regIsento,
                            regImune, regNaoIncidencia, regEstimativa,
                            regSocLiberal, regSimplesNacional, regMEI]);
end;

function StrToRegRec(out ok: boolean; const s: string): TRegRec;
begin
  Result := StrToEnumerado(ok, s,
                           ['', '00', '02', '03', '04', '07', '08', '09', '11', '12'],
                           [regNenhum, regMovimento, regCancelado, regIsento,
                            regImune, regNaoIncidencia, regEstimativa,
                            regSocLiberal, regSimplesNacional, regMEI]);
end;

function FrmRecToStr(const t: TFrmRec): string;
begin
  Result := EnumeradoToStr(t,
                           ['','00', '01', '03', '04', '05', '06', '07'],
                           [frmNenhum, frmNormal, frmRetidoNaFonte,
                            frmSimplesNacional, frmFixoAnual, frmSemRecolhimento,
                            frmDevidoOutroMunicipio, frmFixoMensal]);
end;

function StrToFrmRec(out ok: boolean; const s: string): TFrmRec;
begin
  Result := StrToEnumerado(ok, s,
                           ['', '00', '01', '03', '04', '05', '06', '07'],
                           [frmNenhum, frmNormal, frmRetidoNaFonte,
                            frmSimplesNacional, frmFixoAnual, frmSemRecolhimento,
                            frmDevidoOutroMunicipio, frmFixoMensal]);
end;

function OperacaoToStr(const t: TOperacao): string;
begin
  Result := EnumeradoToStr(t,
                           ['A', 'B', 'C', 'D', 'J'],
                           [toSemDeducao, toComDeducaoMateriais, toImuneIsenta,
                            toDevolucaoSimplesRemessa, toIntermediacao]);
end;

function StrToOperacao(out ok: boolean; const s: string): TOperacao;
begin
  Result := StrToEnumerado(ok, s,
                           ['A', 'B', 'C', 'D', 'J'],
                           [toSemDeducao, toComDeducaoMateriais, toImuneIsenta,
                            toDevolucaoSimplesRemessa, toIntermediacao]);
end;

function UnidadeToStr(const t: TUnidade): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [tuHora, tuQtde]);
end;

function StrToUnidade(out ok: boolean; const s: string): TUnidade;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [tuHora, tuQtde]);
end;

function SepararDados(const AString: string; const Chave: string; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True): string;
var
  PosIni, PosFim: Integer;
  UTexto, UChave: string;
  Prefixo: string;
begin
  Result := '';
  UTexto := AString;
  UChave := Chave;
  PosFim := 0;
  Prefixo := '';

  if MantemChave then
  begin
    PosIni := Pos('<' + UChave, UTexto);
    if PosIni > 0 then
      PosFim := Pos('/' + UChave, UTexto) + length(UChave) + 3;
  end
  else
  begin
    PosIni := Pos('<' + UChave, UTexto);
    if PosIni > 0 then
    begin
      PosIni := PosIni + Pos('>', copy(UTexto, PosIni, length(UTexto)));
      PosFim := Pos('/' + UChave + '>', UTexto);
    end;
  end;

  if (PosFim = 0) and PermitePrefixo then
  begin
    PosIni := Pos(':' + Chave, Astring);
    if PosIni > 1 then
    begin
      while (PosIni > 1) and (Astring[PosIni - 1] <> '<') do
      begin
        Prefixo := Astring[PosIni - 1] + Prefixo;
        PosIni := PosIni - 1;
      end;
      Result := SeparaDados(Astring, Prefixo + ':' + Chave, MantemChave, False);
    end
  end
  else
    Result := copy(Astring, PosIni, PosFim - (PosIni + 1));
end;

function tpConsultaToStr(const t: TtpConsulta): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6'],
                           [tcPorNumero, tcPorFaixa, tcPorPeriodo,
                            tcServicoPrestado, tcServicoTomado,
                            tcPorCodigoVerificacao]);
end;

function StrTotpConsulta(out ok: boolean; const s: string): TtpConsulta;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6'],
                           [tcPorNumero, tcPorFaixa, tcPorPeriodo,
                            tcServicoPrestado, tcServicoTomado,
                            tcPorCodigoVerificacao]);
end;

function tpPeriodoToStr(const t: TtpPeriodo): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2'],
                           [tpEmissao, tpCompetencia]);
end;

function StrTotpPeriodo(out ok: boolean; const s: string): TtpPeriodo;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [tpEmissao, tpCompetencia]);
end;

function tpRetornoToStr(const t: TtpRetorno): string;
begin
  Result := EnumeradoToStr(t,
                           ['XML', 'PDF'],
                           [trXml, trPDF]);
end;

function StrTotpRetorno(out ok: boolean; const s: string): TtpRetorno;
begin
  Result := StrToEnumerado(ok, s,
                           ['XML', 'PDF'],
                           [trXml, trPDF]);
end;

function MetodoToStr(const t: TMetodo): string;
begin
  Result := EnumeradoToStr(t,
                       ['Recepcionar', 'ConsultarSituacao', 'ConsultarLote',
                        'ConsultarNFSePorRps', 'ConsultarNFSe',
                        'ConsultarNFSePorFaixa', 'ConsultarNFSeServicoPrestado',
                        'ConsultarNFSeServicoTomado', 'CancelarNFSe',
                        'Gerar', 'GerarLote', 'RecepcionarSincrono', 'SubstituirNFSe',
                        'AbrirSessao', 'FecharSessao', 'Teste', 'Todos',
                        'GerarToken', 'EnviarEvento', 'ConsultarEvento',
                        'ConsultarDFe', 'ConsultarParam', 'ConsultarSeqRps',
                        'ConsultarLinkNFSe'],
                       [tmRecepcionar, tmConsultarSituacao, tmConsultarLote,
                        tmConsultarNFSePorRps, tmConsultarNFSe,
                        tmConsultarNFSePorFaixa, tmConsultarNFSeServicoPrestado,
                        tmConsultarNFSeServicoTomado, tmCancelarNFSe,
                        tmGerar, tmGerarLote, tmRecepcionarSincrono, tmSubstituirNFSe,
                        tmAbrirSessao, tmFecharSessao, tmTeste, tmTodos,
                        tmGerarToken, tmEnviarEvento, tmConsultarEvento,
                        tmConsultarDFe, tmConsultarParam, tmConsultarSeqRps,
                        tmConsultarLinkNFSe]);
end;

function ModoEnvioToStr(const t: TmodoEnvio): string;
begin
  Result := EnumeradoToStr(t,
                       ['Automatico', 'Enviar Lote', 'Enviar Lote Síncrono',
                        'Gerar NFSe', 'Teste de Envio de Lote'],
                       [meAutomatico, meLoteAssincrono, meLoteSincrono,
                        meUnitario, meTeste]);
end;

function TipoLancamentoToStr(const t: TTipoLancamento): string;
begin
  Result := EnumeradoToStr(t, ['N', 'T', 'P', 'R', 'C'],
                         [tlDevidoNoMunicPrestador, tlDevidoNoMunicTomador,
                          tlSimplesNacional, tlIsentoImune, tlCancelado]);
end;

function StrToTipoLancamento(out ok: boolean; const s: string): TTipoLancamento;
begin
  Result := StrToEnumerado(ok, s, ['N', 'T', 'P', 'R', 'C'],
                         [tlDevidoNoMunicPrestador, tlDevidoNoMunicTomador,
                          tlSimplesNacional, tlIsentoImune, tlCancelado]);
end;

function tpDocumentoToStr(const t: TtpDocumento): string;
begin
  Result := EnumeradoToStr(t, ['1', '2'], [tdNFSe, tdRPS]);
end;

function StrTotpDocumento(out ok: boolean; const s: string): TtpDocumento;
begin
  Result := StrToEnumerado(ok, s, ['1', '2'], [tdNFSe, tdRPS]);
end;

function tpEmitToStr(const t: TtpEmit): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
                           [tePrestador, teTomador, teIntermediario]);
end;

function StrTotpEmit(out ok: Boolean; const s: string): TtpEmit;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
                           [tePrestador, teTomador, teIntermediario]);
end;

function OptanteSNToStr(const t: TOptanteSN): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
                           [osnNaoOptante, osnOptanteMEI, osnOptanteMEEPP]);
end;

function StrToOptanteSN(out ok: Boolean; const s: string): TOptanteSN;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
                           [osnNaoOptante, osnOptanteMEI, osnOptanteMEEPP]);
end;

function RegimeApuracaoSNToStr(const t: TRegimeApuracaoSN): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
                           [raFederaisMunicipalpeloSN, raFederaisSN,
                            raFederaisMunicipalforaSN]);
end;

function StrToRegimeApuracaoSN(out ok: Boolean; const s: string): TRegimeApuracaoSN;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
                           [raFederaisMunicipalpeloSN, raFederaisSN,
                            raFederaisMunicipalforaSN]);
end;

function cMotivoToStr(const t: TcMotivo): string;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '99'],
                           [cmDesenquadramento, cmEnquadramento, cmInclusao,
                            cmExclusao, cmRejeicao, cmOutros]);
end;

function StrTocMotivo(out ok: Boolean; const s: string): TcMotivo;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '99'],
                           [cmDesenquadramento, cmEnquadramento, cmInclusao,
                            cmExclusao, cmRejeicao, cmOutros]);
end;

function mdPrestacaoToStr(const t: TmdPrestacao): string;
begin
  result := EnumeradoToStr(t,
                           ['0', '1', '2', '3', '4'],
                           [mpDesconhecido, mpTransfronteirico, mpConsumoBrasil,
                   mpPresencaComercialExterior, mpMovimentoTempPessoasFisicas]);
end;

function StrTomdPrestacao(out ok: Boolean; const s: string): TmdPrestacao;
begin
  result := StrToEnumerado(ok, s,
                           ['0', '1', '2', '3', '4'],
                           [mpDesconhecido, mpTransfronteirico, mpConsumoBrasil,
                   mpPresencaComercialExterior, mpMovimentoTempPessoasFisicas]);
end;

function vincPrestToStr(const t: TvincPrest): string;
begin
  result := EnumeradoToStr(t,
                           ['0', '1', '2', '3', '4', '5', '6'],
                           [vpSemVinculo, vpControlada, vpControladora,
                            vpColigada, vpMatriz, vpFilial, vpOutro]);
end;

function StrTovincPrest(out ok: Boolean; const s: string): TvincPrest;
begin
  result := StrToEnumerado(ok, s,
                           ['0', '1', '2', '3', '4', '5', '6'],
                           [vpSemVinculo, vpControlada, vpControladora,
                            vpColigada, vpMatriz, vpFilial, vpOutro]);
end;

function mecAFComexPToStr(const t: TmecAFComexP): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08'],
                           [mapsDesconhecido, mapsNenhum, mapsACC, mapsACE,
                            mapsBNDESPos, mapsBNDESPre, mapsFGE, mapsPROEXEqual,
                            mapsPROEXFinanc]);
end;

function StrTomecAFComexP(out ok: Boolean; const s: string): TmecAFComexP;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08'],
                           [mapsDesconhecido, mapsNenhum, mapsACC, mapsACE,
                            mapsBNDESPos, mapsBNDESPre, mapsFGE, mapsPROEXEqual,
                            mapsPROEXFinanc]);
end;

function mecAFComexTToStr(const t: TmecAFComexT): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '11', '12', '13', '14', '15', '16', '17',
                            '18', '19', '20', '21', '22', '23', '24', '25', '26'],
    [matsDesconhecido, matsNenhum, matsAdmPublica, matsAlugueis,
     matsArredondamento, matsComissao, matsDespesas, matsEventosFIFASubsidiaria,
     matsEventosFIFA, matsFretes, matsMaterialAeronautico, matsPromocaoBens,
     matsPromocaoTuristicos, matsPromocaoBrasilExt, matsPromocaoServicoExt,
     matsRECINE, matsRECOPA, matsPatentes, matsREICOMP, matsREIDI, matsREPENEC,
     matsREPES, matsRETAERO, matsRETID, matsRoyalties, matsServicosAvaliacao,
     matsZPE]);
end;

function StrTomecAFComexT(out ok: Boolean; const s: string): TmecAFComexT;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10', '11', '12', '13', '14', '15', '16', '17',
                            '18', '19', '20', '21', '22', '23', '24', '25', '26'],
    [matsDesconhecido, matsNenhum, matsAdmPublica, matsAlugueis,
     matsArredondamento, matsComissao, matsDespesas, matsEventosFIFASubsidiaria,
     matsEventosFIFA, matsFretes, matsMaterialAeronautico, matsPromocaoBens,
     matsPromocaoTuristicos, matsPromocaoBrasilExt, matsPromocaoServicoExt,
     matsRECINE, matsRECOPA, matsPatentes, matsREICOMP, matsREIDI, matsREPENEC,
     matsREPES, matsRETAERO, matsRETID, matsRoyalties, matsServicosAvaliacao,
     matsZPE]);
end;

function MovTempBensToStr(const t: TMovTempBens): string;
begin
  result := EnumeradoToStr(t,
                           ['0', '1', '2', '3'],
                           [mtDesconhecido, mtNao, mtVincDeclImport,
                            mtVincDeclExport]);
end;

function StrToMovTempBens(out ok: Boolean; const s: string): TMovTempBens;
begin
  result := StrToEnumerado(ok, s,
                           ['0', '1', '2', '3'],
                           [mtDesconhecido, mtNao, mtVincDeclImport,
                            mtVincDeclExport]);
end;

function categToStr(const t: Tcateg): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [cLocacao, cSubLocacao, cArrendamento,
                            cDireitoPassagem, cPermissao]);
end;

function StrTocateg(out ok: Boolean; const s: string): Tcateg;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [cLocacao, cSubLocacao, cArrendamento,
                            cDireitoPassagem, cPermissao]);
end;

function objetoToStr(const t: Tobjeto): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6'],
                           [oFerrovia, oRodovia, oPostes, oCabos, oDutos,
                            oCondutos]);
end;

function StrToobjeto(out ok: Boolean; const s: string): Tobjeto;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6'],
                           [oFerrovia, oRodovia, oPostes, oCabos, oDutos,
                            oCondutos]);
end;

function categVeicToStr(const t: TcategVeic): string;
begin
  result := EnumeradoToStr(t,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10'],
    [cvDesconhecido, cvAutomovel, cvCaminhao, cvAutomovelComSemiReboque,
     cvCaminhaoComSemiReboque, cvAutomovelComReboque, cvCaminhaoComReboque,
     cvCaminhaoTratorComSemiReboque, cvMotocicleta, cvVeiculoEspecial,
     cvVeiculoIsento]);
end;

function StrTocategVeic(out ok: Boolean; const s: string): TcategVeic;
begin
  result := StrToEnumerado(ok, s,
                           ['00', '01', '02', '03', '04', '05', '06', '07', '08',
                            '09', '10'],
    [cvDesconhecido, cvAutomovel, cvCaminhao, cvAutomovelComSemiReboque,
     cvCaminhaoComSemiReboque, cvAutomovelComReboque, cvCaminhaoComReboque,
     cvCaminhaoTratorComSemiReboque, cvMotocicleta, cvVeiculoEspecial,
     cvVeiculoIsento]);
end;

function rodagemToStr(const t: Trodagem): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2'],
                           [trSimples, trDupla]);
end;

function StrTorodagem(out ok: Boolean; const s: string): Trodagem;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [trSimples, trDupla]);
end;

function tpDedRedToStr(const t: TtpDedRed): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '99'],
    [drAlimentacao, drMateriais, drProducaoExt, drReembolso, drRepasseConsorciado,
     drRepassePlanoSaude, drServicos, drSubEmpreitada, drOutrasDeducoes]);
end;

function StrTotpDedRed(out ok: Boolean; const s: string): TtpDedRed;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5', '6', '7', '8', '99'],
    [drAlimentacao, drMateriais, drProducaoExt, drReembolso, drRepasseConsorciado,
     drRepassePlanoSaude, drServicos, drSubEmpreitada, drOutrasDeducoes]);
end;

function tribISSQNToStr(const t: TtribISSQN): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4'],
            [tiOperacaoTributavel, tiExportacao, tiNaoIncidencia, tiImunidade]);
end;

function StrTotribISSQN(out ok: Boolean; const s: string): TtribISSQN;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4'],
            [tiOperacaoTributavel, tiExportacao, tiNaoIncidencia, tiImunidade]);
end;

function tpImunidadeToStr(const t: TtpImunidade): string;
begin
  result := EnumeradoToStr(t,
                           ['', '0', '1', '2', '3', '4', '5'],
                [timNenhum, timImunidade, timPatrimonio, timTemplos,
                 timPatrimonioPartidos, timLivros, timFonogramas]);
end;

function StrTotpImunidade(out ok: Boolean; const s: string): TtpImunidade;
begin
  result := StrToEnumerado(ok, s,
                           ['', '0', '1', '2', '3', '4', '5'],
                [timNenhum, timImunidade, timPatrimonio, timTemplos,
                 timPatrimonioPartidos, timLivros, timFonogramas]);
end;

function tpRetISSQNToStr(const t: TtpRetISSQN): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                 [trNaoRetido, trRetidoPeloTomador, trRetidoPeloIntermediario]);
end;

function StrTotpRetISSQN(out ok: Boolean; const s: string): TtpRetISSQN;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                 [trNaoRetido, trRetidoPeloTomador, trRetidoPeloIntermediario]);
end;

function tpBMToStr(const t: TtpBM): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [tbAliquota, tbReducaoBC, tbIsencao]);
end;

function StrTotpBM(out ok: Boolean; const s: string): TtpBM;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [tbAliquota, tbReducaoBC, tbIsencao]);
end;

function tpSuspToStr(const t: TtpSusp): string;
begin
  result := EnumeradoToStr(t,
                           ['', '1', '2'],
                           [tsNenhum, tsDecisaoJudicial, tsProcessoAdm]);
end;

function StrTotpSusp(out ok: Boolean; const s: string): TtpSusp;
begin
  result := StrToEnumerado(ok, s,
                           ['', '1', '2'],
                           [tsNenhum, tsDecisaoJudicial, tsProcessoAdm]);
end;

function CSTToStr(const t: TCST): string;
begin
  result := EnumeradoToStr(t,
        ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09'],
        [cst00, cst01, cst02, cst03, cst04, cst05, cst06, cst07, cst08, cst09]);
end;

function StrToCST(out ok: Boolean; const s: string): TCST;
begin
  result := StrToEnumerado(ok, s,
        ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09'],
        [cst00, cst01, cst02, cst03, cst04, cst05, cst06, cst07, cst08, cst09]);
end;

function tpRetPisCofinsToStr(const t: TtpRetPisCofins): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2'],
                           [trpcRetido, trpcNaoRetido]);
end;

function StrTotpRetPisCofins(out ok: Boolean; const s: string): TtpRetPisCofins;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [trpcRetido, trpcNaoRetido]);
end;

function indTotTribToStr(const t: TindTotTrib): string;
begin
  result := EnumeradoToStr(t,
                           ['0'],
                           [indNao]);
end;

function StrToindTotTrib(out ok: Boolean; const s: string): TindTotTrib;
begin
  result := StrToEnumerado(ok, s,
                           ['0'],
                           [indNao]);
end;

function ambGerToStr(const t: TambGer): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2'],
                           [agPrefeitura, agSistemaNacional]);
end;

function StrToambGer(out ok: Boolean; const s: string): TambGer;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [agPrefeitura, agSistemaNacional]);
end;

function tpEmisToStr(const t: TtpEmis): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2'],
                           [tePadraoNacional, teProprio]);
end;

function StrTotpEmis(out ok: Boolean; const s: string): TtpEmis;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [tePadraoNacional, teProprio]);
end;

function procEmiToStr(const t: TprocEmi): string;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [peWebService, peWebFisco, peAppFisco]);
end;

function StrToprocEmi(out ok: Boolean; const s: string): TprocEmi;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [peWebService, peWebFisco, peAppFisco]);
end;

function tpEventoToStr(const t: TtpEvento): string;
begin
  result := EnumeradoToStr(t,
                         ['e101101', 'e105102', 'e101103', 'e105104', 'e105105',
                          'e202201', 'e203202', 'e204203', 'e205204', 'e202205',
                          'e203206', 'e204207', 'e205208', 'e305101', 'e305102',
                          'e305103'],
                   [teCancelamento, teCancelamentoSubstituicao,
                    teAnaliseParaCancelamento, teCancelamentoDeferido,
                    teCancelamentoIndeferido, teConfirmacaoPrestador,
                    teConfirmacaoTomador, teConfirmacaoIntermediario,
                    teConfirmacaoTacita, teRejeicaoPrestador, teRejeicaoTomador,
                    teRejeicaoIntermediario, teAnulacaoRejeicao,
                    teCancelamentoPorOficio, teBloqueioPorOficio,
                    teDesbloqueioPorOficio]);
end;

function StrTotpEvento(out ok: Boolean; const s: string): TtpEvento;
begin
  result := StrToEnumerado(ok, s,
                         ['e101101', 'e105102', 'e101103', 'e105104', 'e105105',
                          'e202201', 'e203202', 'e204203', 'e205204', 'e202205',
                          'e203206', 'e204207', 'e205208', 'e305101', 'e305102',
                          'e305103'],
                   [teCancelamento, teCancelamentoSubstituicao,
                    teAnaliseParaCancelamento, teCancelamentoDeferido,
                    teCancelamentoIndeferido, teConfirmacaoPrestador,
                    teConfirmacaoTomador, teConfirmacaoIntermediario,
                    teConfirmacaoTacita, teRejeicaoPrestador, teRejeicaoTomador,
                    teRejeicaoIntermediario, teAnulacaoRejeicao,
                    teCancelamentoPorOficio, teBloqueioPorOficio,
                    teDesbloqueioPorOficio]);
end;

function tpEventoToDesc(const t: TtpEvento): string;
begin
  result := EnumeradoToStr(t,
                         ['Cancelamento de NFS-e',
                          'Cancelamento de NFS-e por Substituicao',
                          'Solicitacao de Analise Fiscal para Cancelamento de NFS-e',
                          'Cancelamento de NFS-e Deferido por Análise Fiscal',
                          'Cancelamento de NFS-e Indeferido por Análise Fiscal',
                          'Confirmação do Prestador',
                          'Confirmação do Tomador',
                          'Confirmação do Intermediário',
                          'Confirmação Tácita',
                          'Rejeição do Prestador',
                          'Rejeição do Tomador',
                          'Rejeição do Intermediário',
                          'Anulação da Rejeição',
                          'Cancelamento de NFS-e por Ofício',
                          'Bloqueio de NFS-e por Ofício',
                          'Desbloqueio de NFS-e por Ofício'],
                   [teCancelamento, teCancelamentoSubstituicao,
                    teAnaliseParaCancelamento, teCancelamentoDeferido,
                    teCancelamentoIndeferido, teConfirmacaoPrestador,
                    teConfirmacaoTomador, teConfirmacaoIntermediario,
                    teConfirmacaoTacita, teRejeicaoPrestador, teRejeicaoTomador,
                    teRejeicaoIntermediario, teAnulacaoRejeicao,
                    teCancelamentoPorOficio, teBloqueioPorOficio,
                    teDesbloqueioPorOficio]);
end;

function ParamMunicToStr(const t: TParamMunic): string;
begin
  result := EnumeradoToStr(t,
                           ['Aliquota', 'HistoricoAliquota', 'Convenio',
                            'RegimesEspeciais', 'Retencoes', 'Beneficios'],
                           [pmAliquota, pmHistoricoAliquota, pmConvenio,
                            pmRegimesEspeciais, pmRetencoes, pmBeneficios]);
end;

function StrToParamMunic(out ok: Boolean; const s: string): TParamMunic;
begin
  result := StrToEnumerado(ok, s,
                           ['Aliquota', 'HistoricoAliquota', 'Convenio',
                            'RegimesEspeciais', 'Retencoes', 'Beneficios'],
                           [pmAliquota, pmHistoricoAliquota, pmConvenio,
                            pmRegimesEspeciais, pmRetencoes, pmBeneficios]);
end;


function CodIBGEPaisToSiglaISO2(t: Integer): string;
var
  i: Integer;
begin
  Result := 'ZZ';

  for i := Low(CodigoIBGEPais) to High(CodigoIBGEPais) do
  begin
    if CodigoIBGEPais[i] = t then
    begin
      Result := SiglaISO2Pais[i];
      exit;
    end;
  end;
end;

function SiglaISO2ToCodIBGEPais(const t: string): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := Low(SiglaISO2Pais) to High(SiglaISO2Pais) do
  begin
    if SiglaISO2Pais[i] = t then
    begin
      Result := CodigoIBGEPais[i];
      exit;
    end;
  end;
end;

end.
