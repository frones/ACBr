////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnConversaoNFe;

interface

uses
  SysUtils, StrUtils, Classes,
  ACBrBase,
  pcnConversao;

type

  TpcnIndicadorPagamento = (ipVista, ipPrazo, ipOutras, ipNenhum);
const
  TpcnIndicadorPagamentoArrayStrings: array[TpcnIndicadorPagamento] of string = ('0', '1', '2', '');

function IndpagToStr(const t: TpcnIndicadorPagamento): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função IndpagToStrEX'{$ENDIF};
function StrToIndpag(out ok: boolean; const s: string): TpcnIndicadorPagamento; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função StrToIndpagEX'{$ENDIF};
function IndpagToStrEX(const t: TpcnIndicadorPagamento): string;
function StrToIndpagEX(const s: string): TpcnIndicadorPagamento;

type
  TpcnPercentualTributos = (ptValorProdutos, ptValorNF, ptPersonalizado);
function PercTribToStr(const t: TpcnPercentualTributos): string;
function StrToPercTrib(out ok: boolean; const s: string): TpcnPercentualTributos;

type
  TpcnDeterminacaoBaseIcms = (dbiMargemValorAgregado, dbiPauta, dbiPrecoTabelado, dbiValorOperacao, dbiNenhum);
  TpcnDeterminacaoBaseIcmsST = (dbisPrecoTabelado, dbisListaNegativa,
                                dbisListaPositiva, dbisListaNeutra,
                                dbisMargemValorAgregado, dbisPauta,
                                dbisValordaOperacao);
  TpcnMotivoDesoneracaoICMS = (mdiTaxi, mdiDeficienteFisico, mdiProdutorAgropecuario, mdiFrotistaLocadora, mdiDiplomaticoConsular,
                               mdiAmazoniaLivreComercio, mdiSuframa, mdiVendaOrgaosPublicos, mdiOutros, mdiDeficienteCondutor,
                               mdiDeficienteNaoCondutor, mdiOrgaoFomento, mdiOlimpiadaRio2016, mdiSolicitadoFisco );

function modBCToStr(const t: TpcnDeterminacaoBaseIcms): string;
function modBCToStrTagPosText(const t: TpcnDeterminacaoBaseIcms): string;
function StrTomodBC(out ok: boolean; const s: string): TpcnDeterminacaoBaseIcms;
function modBCSTToStr(const t: TpcnDeterminacaoBaseIcmsST): string;
function modBCSTToStrTagPosText(const t: TpcnDeterminacaoBaseIcmsST): string;
function StrTomodBCST(out ok: boolean; const s: string): TpcnDeterminacaoBaseIcmsST;
function motDesICMSToStr(const t: TpcnMotivoDesoneracaoICMS): string;
function motDesICMSToStrTagPosText(const t: TpcnMotivoDesoneracaoICMS): string;
function StrTomotDesICMS(out ok: boolean; const s: string): TpcnMotivoDesoneracaoICMS;

type
  TpcnCstIpi = (ipi00, ipi49, ipi50, ipi99, ipi01, ipi02, ipi03, ipi04, ipi05, ipi51, ipi52, ipi53, ipi54, ipi55);

function CSTIPIToStrTagPosText(const t: TpcnCstIpi): string;
function CSTIPIToStr(const t: TpcnCstIpi): string;
function StrToCSTIPI(out ok: boolean; const s: string): TpcnCstIpi;

type
  TpcnIndicadorProcesso = (ipSEFAZ, ipJusticaFederal, ipJusticaEstadual,
                           ipSecexRFB, ipCONFAZ, ipOutros);

function indProcToStr(const t: TpcnIndicadorProcesso): string;
function indProcToDescrStr(const t: TpcnIndicadorProcesso): string;
function StrToindProc(out ok: boolean; const s: string): TpcnIndicadorProcesso;

type
  TpcnCRT = (crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal, crtMEI);

function CRTToStr(const t: TpcnCRT): string;
function StrToCRT(out ok: boolean; const s: string): TpcnCRT;
function CRTTocRegTrib(const t: TpcnCRT): TpcnRegTrib;

type
  TpcnIndicadorTotal = (itSomaTotalNFe, itNaoSomaTotalNFe );

function indTotToStr(const t: TpcnIndicadorTotal): string;
function StrToindTot(out ok: boolean; const s: string): TpcnIndicadorTotal;

type
  TpcnECFModRef = (ECFModRefVazio, ECFModRef2B,ECFModRef2C,ECFModRef2D);

function ECFModRefToStr(const t:  TpcnECFModRef ): string;
function StrToECFModRef(out ok: boolean; const s: string): TpcnECFModRef;

type
  TpcnISSQNcSitTrib  = ( ISSQNcSitTribVazio , ISSQNcSitTribNORMAL, ISSQNcSitTribRETIDA, ISSQNcSitTribSUBSTITUTA,ISSQNcSitTribISENTA);

function ISSQNcSitTribToStr(const t: TpcnISSQNcSitTrib ): string;
function ISSQNcSitTribToStrTagPosText(const t: TpcnISSQNcSitTrib ): string;
function StrToISSQNcSitTrib(out ok: boolean; const s: string) : TpcnISSQNcSitTrib;

type
  TpcnImprimeDescAcrescItem = (idaiSempre, idaiNunca, idaiComValor);
  TImprimirUnidQtdeValor = (iuComercial, iuTributavel, iuComercialETributavel);

  TpcnDestinoOperacao = (doInterna, doInterestadual, doExterior);
  TpcnConsumidorFinal = (cfNao, cfConsumidorFinal);
  TpcnPresencaComprador = (pcNao, pcPresencial, pcInternet, pcTeleatendimento, pcEntregaDomicilio, pcPresencialForaEstabelecimento, pcOutros);

function DestinoOperacaoToStr(const t: TpcnDestinoOperacao): string;
function StrToDestinoOperacao(out ok: boolean; const s: string): TpcnDestinoOperacao;
function ConsumidorFinalToStr(const t: TpcnConsumidorFinal): string;
function StrToConsumidorFinal(out ok: boolean; const s: string): TpcnConsumidorFinal;
function PresencaCompradorToStr(const t: TpcnPresencaComprador): string;
function StrToPresencaComprador(out ok: boolean; const s: string): TpcnPresencaComprador;

type
  TpcnFormaPagamento = (fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito, fpCreditoLoja,
                        fpValeAlimentacao, fpValeRefeicao, fpValePresente, fpValeCombustivel,
                        fpDuplicataMercantil, fpBoletoBancario, fpDepositoBancario,
                        fpPagamentoInstantaneo, fpTransfBancario, fpProgramaFidelidade,
                        fpSemPagamento, fpRegimeEspecial, fpOutro, fpPagamentoInstantaneoEstatico,
                        fpCreditoEmLojaPorDevolucao, fpFalhaHardware);
  TpcnBandeiraCartao = (bcVisa, bcMasterCard, bcAmericanExpress, bcSorocred, bcDinersClub,
                        bcElo, bcHipercard, bcAura, bcCabal, bcAlelo, bcBanesCard,
                        bcCalCard, bcCredz, bcDiscover, bcGoodCard, bcGreenCard,
                        bcHiper, bcJcB, bcMais, bcMaxVan, bcPolicard, bcRedeCompras,
                        bcSodexo, bcValeCard, bcVerocheque, bcVR, bcTicket,
                        bcOutros);

function FormaPagamentoToStr(const t: TpcnFormaPagamento): string;
function FormaPagamentoToDescricao(const t: TpcnFormaPagamento): string; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use FormaPagamentoToDescricao(const t: TpcnFormaPagamento; const xPag: String)'{$EndIf};
function FormaPagamentoToDescricao(const t: TpcnFormaPagamento; const xPag: String): string; overload;
function StrToFormaPagamento(out ok: boolean; const s: string): TpcnFormaPagamento;
function BandeiraCartaoToStr(const t: TpcnBandeiraCartao): string;
function BandeiraCartaoToDescStr(const t: TpcnBandeiraCartao): string;
function StrToBandeiraCartao(out ok: boolean; const s: string): TpcnBandeiraCartao;

type
  TpcnTipoViaTransp = (tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal,
                       tvFerroviaria, tvRodoviaria, tvConduto, tvMeiosProprios,
                       tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque);

function TipoViaTranspToStr(const t: TpcnTipoViaTransp ): string;
function StrToTipoViaTransp(out ok: boolean; const s: string): TpcnTipoViaTransp;
function TipoViaTranspToDescricao(const t: TpcnTipoViaTransp): string;

type
  TpcnTipoIntermedio = (tiContaPropria, tiContaOrdem, tiEncomenda);
  TpcnindISSRet = (iirSim, iirNao);
  TpcnindISS = (iiExigivel, iiNaoIncidencia, iiIsencao, iiExportacao, iiImunidade, iiExigSuspDecisaoJudicial, iiExigSuspProcessoAdm);

function TipoIntermedioToStr(const t: TpcnTipoIntermedio ): string;
function StrToTipoIntermedio(out ok: boolean; const s: string): TpcnTipoIntermedio;
function TipoIntermedioToDescricao(const t: TpcnTipoIntermedio ): string;
function indISSRetToStr(const t: TpcnindISSRet ): string;
function StrToindISSRet(out ok: boolean; const s: string): TpcnindISSRet;
function indISSToStr(const t: TpcnindISS ): string;
function StrToindISS(out ok: boolean; const s: string): TpcnindISS;
function indISSToStrTagPosText(const t: TpcnindISS ): string;

type
  TpcnTipoAutor = (taEmpresaEmitente, taEmpresaDestinataria, taEmpresa, taFisco, taRFB, taOutros);

function TipoAutorToStr(const t: TpcnTipoAutor ): string;
function StrToTipoAutor(out ok: boolean; const s: string): TpcnTipoAutor;

type
  TpcnIndOperacao = (ioConsultaCSC, ioNovoCSC, ioRevogaCSC);

function IndOperacaoToStr(const t: TpcnIndOperacao ): string;
function StrToIndOperacao(out ok: boolean; const s: string): TpcnIndOperacao;

type
  TForcarGeracaoTag = (fgtNunca, fgtSomenteProducao, fgtSomenteHomologacao, fgtSempre);

type
  TIndSomaPISST = (ispNenhum, ispPISSTNaoCompoe, ispPISSTCompoe);
  TIndSomaCOFINSST = (iscNenhum, iscCOFINSSTNaoCompoe, iscCOFINSSTCompoe);

function indSomaPISSTToStr(const t: TIndSomaPISST): String;
function StrToindSomaPISST(out ok: Boolean; const s: String): TIndSomaPISST;

function indSomaCOFINSSTToStr(const t: TIndSomaCOFINSST): String;
function StrToindSomaCOFINSST(out ok: Boolean; const s: String): TIndSomaCOFINSST;

type
  TStatusACBrNFe = (stIdle, stNFeStatusServico, stNFeRecepcao, stNFeRetRecepcao,
    stNFeConsulta, stNFeCancelamento, stNFeInutilizacao, stNFeRecibo,
    stNFeCadastro, stNFeEmail, stNFeCCe, stNFeEvento, stConsNFeDest,
    stDownloadNFe, stAdmCSCNFCe, stDistDFeInt, stEnvioWebService);

  TpcnInformacoesDePagamento = (eipNunca, eipAdicionais, eipQuadro);

  TpcnVersaoDF = (ve200, ve300, ve310, ve400);

const
  TVersaoDFArrayStrings: array[TpcnVersaoDF] of string = ('2.00', '3.00', '3.10',
    '4.00');
  TVersaoDFArrayDouble: array[TpcnVersaoDF] of Double = (2.00, 3.00, 3.10, 4.00);

type
  TSchemaNFe = (schErro, schNfe, schcancNFe, schInutNFe, schEnvCCe,
                schEnvEventoCancNFe, schEnvConfRecebto, schEnvEPEC,
                schconsReciNFe, schconsSitNFe, schconsStatServ, schconsCad,
                schenvEvento, schconsNFeDest, schdownloadNFe, schretEnviNFe,
                schadmCscNFCe, schdistDFeInt, scheventoEPEC, schCancSubst,
                schPedProrrog1, schPedProrrog2, schCanPedProrrog1,
                schCanPedProrrog2, schManifDestConfirmacao,
                schManifDestCiencia, schManifDestDesconhecimento,
                schManifDestOperNaoRealizada, schCompEntrega, schCancCompEntrega,
                schAtorInteressadoNFe, schInsucessoEntregaNFe,
                schCancInsucessoEntregaNFe, schConcFinanceira,
                schCancConcFinanceira);

const
  TSchemaNFeArrayStrings: array[TSchemaNFe] of string = ('Erro', 'Nfe',
    'cancNFe', 'InutNFe', 'EnvCCe', 'EnvEventoCancNFe', 'EnvConfRecebto',
    'EnvEPEC', 'consReciNFe', 'consSitNFe', 'consStatServ', 'consCad',
    'envEvento', 'consNFeDest', 'downloadNFe', 'retEnviNFe', 'admCscNFCe',
    'distDFeInt', 'eventoEPEC', 'CancSubst', 'PedProrrog1', 'PedProrrog2',
    'CanPedProrrog1', 'CanPedProrrog2', 'ManifDestConfirmacao',
    'ManifDestCiencia', 'ManifDestDesconhecimento', 'ManifDestOperNaoRealizada',
    'CompEntrega', 'CancCompEntrega', 'AtorInteressadoNFe',
    'InsucessoEntrega', 'CancInsucessoEntrega', 'ConcFinanceira',
    'CancConcFinanceira');

  TEventoArrayStrings: array[TSchemaNFe] of string = ('', '', 'e110111', '',
    'e110110', '', '', 'e110140', '', '', '', '', '', '', '', '', '', '', '',
    'e110112', 'e111500', 'e111501', 'e111502', 'e111503', 'e210200', 'e210210',
    'e210220', 'e210240', 'e110130', 'e110131', 'e110150', 'e110192', 'e110193',
    'e110750', 'e110751');

type
  TLayOut = (LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento,
    LayNfeInutilizacao, LayNfeConsulta, LayNfeStatusServico,
    LayNfeCadastro, LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
    LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
    LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC, LayNFeURLQRCode,
    LayURLConsultaNFe, LayURLConsultaNFCe);

const
  TLayOutArrayStrings: array[TLayOut] of string = ('NfeRecepcao',
    'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
    'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
    'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
    'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
    'NFeDistribuicaoDFe', 'EventoEPEC', 'URL-QRCode', 'URL-ConsultaNFe',
    'URL-ConsultaNFCe');

type
  TpcnFinalidadeNFe = (fnNormal, fnComplementar, fnAjuste, fnDevolucao);

const
  TFinalidadeNFeArrayStrings: array[TpcnFinalidadeNFe] of string = ('1', '2', '3',
    '4');

type
  TpcnModeloDF = (moNFe, moNFCe);

const
  TModeloDFArrayStrings: array[TpcnModeloDF] of string = ('55', '65');

type
  TpcnIndicadorNFe = (inTodas, inSemManifestacaoComCiencia,
                      inSemManifestacaoSemCiencia);

const
  TIndicadorNFeArrayStrings: array[TpcnIndicadorNFe] of string = ('0', '1', '2');

type
  TpcnVersaoQrCode = (veqr000, veqr100, veqr200);

const
  TVersaoQrCodeArrayStrings: array[TpcnVersaoQrCode] of string = ('0', '1', '2');
  TVersaoQrCodeArrayDouble: array[TpcnVersaoQrCode] of Double = (0, 1.00, 2.00);

type
  TpcnTipoOperacao = (toVendaConcessionaria, toFaturamentoDireto, toVendaDireta,
                      toOutros);

const
  TTipoOperacaoArrayStrings: array[TpcnTipoOperacao] of string = ('1', '2', '3',
    '0');
  TTipoOperacaoDescArrayStrings: array[TpcnTipoOperacao]
    of string = ('1-VENDA CONCESSIONÁRIA', '2-FAT. DIRETO CONS. FINAL',
      '3-VENDA DIRETA', '0-OUTROS');

type
  TpcnCondicaoVeiculo = (cvAcabado, cvInacabado, cvSemiAcabado);

const
  TCondicaoVeiculoArrayStrings: array[TpcnCondicaoVeiculo] of string = ('1', '2',
    '3');
  TCondicaoVeiculoDescArrayStrings: array[TpcnCondicaoVeiculo]
     of string = ('1-ACABADO', '2-INACABADO', '3-SEMI-ACABADO');

type
  TpcnTipoArma = (taUsoPermitido, taUsoRestrito);

const
  TTipoArmaArrayStrings: array[TpcnTipoArma] of string = ('0', '1');
  TTipoArmaDescArrayStrings: array[TpcnTipoArma] of string = ('0-USO PERMITIDO',
    '1-USO RESTRITO');

type
  TpcnIndEscala = (ieRelevante, ieNaoRelevante, ieNenhum);

const
  TIndEscalaArrayStrings: array[TpcnIndEscala] of string = ('S', 'N', '');

type
  TpcnModalidadeFrete = (mfContaEmitente, mfContaDestinatario, mfContaTerceiros,
                         mfProprioRemetente, mfProprioDestinatario, mfSemFrete);

const
  TModalidadeFreteArrayStrings: array[TpcnModalidadeFrete] of string = ('0', '1',
    '2', '3', '4', '9');

type
  TAutorizacao = (taNaoPermite, taPermite, taNaoInformar);

const
  TAutorizacaoArrayStrings: array[TAutorizacao] of string = ('0', '1', '');

type
  TindIntermed = (iiSemOperacao, iiOperacaoSemIntermediador,
                  iiOperacaoComIntermediador);

const
  TindIntermedArrayStrings: array[TindIntermed] of string = ('', '0', '1');

type
  TtpAto = (taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS);

const
  TtpAtoArrayStrings: array[TtpAto] of string = ('', '08', '10', '12', '14',
    '15');

type
  TindImport = (iiNacional, iiImportado);

const
  TindImportArrayStrings: array[TindImport] of string = ('0', '1');

type
  TmotRedAdRem = (motTranspColetivo, motOutros);

const
  TmotRedAdRemArrayStrings: array[TmotRedAdRem] of string = ('1', '9');

type
  TtpMotivo = (tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro);

const
  TtpMotivoArrayStrings: array[TtpMotivo] of string = ('1', '2', '3', '4');

{
  Declaração das funções de conversão
}
function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;

function LayOutToServico(const t: TLayOut): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;

function LayOutToSchema(const t: TLayOut): TSchemaNFe;

function SchemaNFeToStr(const t: TSchemaNFe): String;
function StrToSchemaNFe(const s: String): TSchemaNFe;
function SchemaEventoToStr(const t: TSchemaNFe): String;

function FinNFeToStr(const t: TpcnFinalidadeNFe): String;
function StrToFinNFe(out ok: Boolean; const s: String): TpcnFinalidadeNFe;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): String;
function StrToIndicadorNFe(out ok: Boolean; const s: String): TpcnIndicadorNFe;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): String;
function StrToVersaoQrCode(out ok: Boolean; const s: String): TpcnVersaoQrCode;
function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Real;

function ModeloDFToStr(const t: TpcnModeloDF): String;
function StrToModeloDF(out ok: Boolean; const s: String): TpcnModeloDF;
function ModeloDFToPrefixo(const t: TpcnModeloDF): String;

function StrToVersaoDF(out ok: Boolean; const s: String): TpcnVersaoDF;
function VersaoDFToStr(const t: TpcnVersaoDF): String;

function DblToVersaoDF(out ok: Boolean; const d: Real): TpcnVersaoDF;
function VersaoDFToDbl(const t: TpcnVersaoDF): Real;

function tpOPToStr(const t: TpcnTipoOperacao): string;
function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;

function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;

function tpArmaToStr(const t: TpcnTipoArma): string;
function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;

function VeiculosRestricaoStr( const iRestricao :Integer ): String;
function VeiculosCorDENATRANStr( const sCorDENATRAN : String ): String;
function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): String;
function VeiculosVinStr( const sVin: String ): String;
function VeiculosEspecieStr( const iEspecie : Integer ): String;
function VeiculosTipoStr( const iTipoVeic : Integer ): String;
function VeiculosCombustivelStr( const sTpComb : String ): String;
function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): String;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): String;

function IndEscalaToStr(const t: TpcnIndEscala): String;
function StrToIndEscala(out ok: Boolean; const s: String): TpcnIndEscala;
function modFreteToStr(const t: TpcnModalidadeFrete): string;
function StrTomodFrete(out ok: boolean; const s: string): TpcnModalidadeFrete;
function modFreteToDesStr(const t: TpcnModalidadeFrete; versao: TpcnVersaoDF): string;

function AutorizacaoToStr(const t: TAutorizacao): string;
function StrToAutorizacao(out ok: boolean; const s: string): TAutorizacao;

function IndIntermedToStr(const t: TindIntermed): string;
function StrToIndIntermed(out ok: boolean; const s: string): TindIntermed;

function tpAtoToStr(const t: TtpAto): string;
function StrTotpAto(out ok: boolean; const s: string): TtpAto;

function indImportToStr(const t: TindImport): string;
function StrToindImport(out ok: boolean; const s: string): TindImport;

function motRedAdRemToStr(const t: TmotRedAdRem): string;
function StrTomotRedAdRem(out ok: boolean; const s: string): TmotRedAdRem;

function tpMotivoToStr(const t: TtpMotivo): string;
function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;

implementation

uses
  typinfo;

// Indicador do Tipo de pagamento **********************************************
function IndpagToStr(const t: TpcnIndicadorPagamento): string;
begin
  Result := EnumeradoToStr(t, TpcnIndicadorPagamentoArrayStrings, [ipVista, ipPrazo, ipOutras, ipNenhum]);
end;

function StrToIndpag(out ok: boolean; const s: string): TpcnIndicadorPagamento;
begin
  Result := StrToEnumerado(ok, s, TpcnIndicadorPagamentoArrayStrings, [ipVista, ipPrazo, ipOutras, ipNenhum]);
end;

function IndpagToStrEX(const t: TpcnIndicadorPagamento): string;
begin
  Result := TpcnIndicadorPagamentoArrayStrings[t];
end;

function StrToIndpagEX(const s: string): TpcnIndicadorPagamento;
var
  idx: TpcnIndicadorPagamento;
begin
  for idx:= Low(TpcnIndicadorPagamentoArrayStrings) to High(TpcnIndicadorPagamentoArrayStrings)do
  begin
    if(TpcnIndicadorPagamentoArrayStrings[idx] = s)then
    begin
      Result := idx;
      Exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TpcnIndicadorPagamento: %s', [s]);
end;

function PercTribToStr(const t: TpcnPercentualTributos): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2'],
                              [ptValorProdutos, ptValorNF, ptPersonalizado]);
end;

function StrToPercTrib(out ok: boolean; const s: string): TpcnPercentualTributos;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [ptValorProdutos, ptValorNF, ptPersonalizado]);
end;

// N13 - Modalidade de determinação da BC do ICMS ******************************
function modBCToStrTagPosText(const t: TpcnDeterminacaoBaseIcms): string;
begin
  result := EnumeradoToStr(t, ['0 - Margem Valor Agregado (%)', '1 - Pauta (Valor)', '2 - Preço Tabelado Máx. (valor)', '3 - Valor da operação', ''],
    [dbiMargemValorAgregado, dbiPauta, dbiPrecoTabelado, dbiValorOperacao, dbiNenhum]);
end;

function modBCToStr(const t: TpcnDeterminacaoBaseIcms): string;
begin
  // 0 - Margem Valor Agregado (%);
  // 1 - Pauta (Valor);
  // 2 - Preço Tabelado Máx. (valor);
  // 3 - valor da operação.
  result := EnumeradoToStr(t, ['0', '1', '2', '3', ''],
    [dbiMargemValorAgregado, dbiPauta, dbiPrecoTabelado, dbiValorOperacao, dbiNenhum]);
end;

function StrTomodBC(out ok: boolean; const s: string): TpcnDeterminacaoBaseIcms;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', ''],
    [dbiMargemValorAgregado, dbiPauta, dbiPrecoTabelado, dbiValorOperacao, dbiNenhum]);
end;

// N18 - Modalidade de determinação da BC do ICMS ST ***************************
function modBCSTToStrTagPosText(const t: TpcnDeterminacaoBaseIcmsST): string;
begin
  result := EnumeradoToStr(t, ['0 - Preço tabelado ou máximo sugerido', '1 - Lista Negativa (valor)',
   '2 - Lista Positiva (valor)', '3 - Lista Neutra (valor)',
   '4 - Margem Valor Agregado (%)', '5 - Pauta (valor)', '6 - Valor da Operação', ''],
    [dbisPrecoTabelado, dbisListaNegativa, dbisListaPositiva, dbisListaNeutra,
     dbisMargemValorAgregado, dbisPauta, dbisValordaOperacao,
     TpcnDeterminacaoBaseIcmsST(-1)]);
end;

function modBCSTToStr(const t: TpcnDeterminacaoBaseIcmsST): string;
begin
  // 0 – Preço tabelado ou máximo sugerido;
  // 1 - Lista Negativa (valor);
  // 2 - Lista Positiva (valor);
  // 3 - Lista Neutra (valor);
  // 4 - Margem Valor Agregado (%);
  // 5 - Pauta (valor);
  // 6 - Valor da Operação
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '6', ''],
    [dbisPrecoTabelado, dbisListaNegativa, dbisListaPositiva, dbisListaNeutra,
     dbisMargemValorAgregado, dbisPauta, dbisValordaOperacao,
     TpcnDeterminacaoBaseIcmsST(-1)]);
end;

function StrTomodBCST(out ok: boolean; const s: string): TpcnDeterminacaoBaseIcmsST;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '6', ''],
    [dbisPrecoTabelado, dbisListaNegativa, dbisListaPositiva, dbisListaNeutra,
     dbisMargemValorAgregado, dbisPauta, dbisValordaOperacao,
     TpcnDeterminacaoBaseIcmsST(-1)]);
end;

// N28 - Motivo da desoneração do ICMS ***************************
function motDesICMSToStr(const t: TpcnMotivoDesoneracaoICMS): string;
begin
    // 1 – Táxi;
    // 2 – Deficiente Físico;
    // 3 – Produtor Agropecuário;
    // 4 – Frotista/Locadora;
    // 5 – Diplomático/Consular;
    // 6 – Utilitários e Motocicletas da
    // Amazônia Ocidental e Áreas de
    // Livre Comércio (Resolução
    // 714/88 e 790/94 – CONTRAN e
    // suas alterações);
    // 7 – SUFRAMA;
    // 8 – Venda a Orgãos Publicos;
    // 9 – outros. (v2.0)
    // 10 – Deficiente Condutor (Convênio ICMS 38/12). (v3.1)
    // 11 – Deficiente não Condutor (Convênio ICMS 38/12). (v3.1)
    // 12 - Orgão Fomento
    // 16 - Olimpiadas Rio 2016
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
                               '11', '12', '16', '90', ''],
    [mdiTaxi, mdiDeficienteFisico, mdiProdutorAgropecuario, mdiFrotistaLocadora,
     mdiDiplomaticoConsular, mdiAmazoniaLivreComercio, mdiSuframa, mdiVendaOrgaosPublicos,
     mdiOutros, mdiDeficienteCondutor, mdiDeficienteNaoCondutor, mdiOrgaoFomento,
     mdiOlimpiadaRio2016, mdiSolicitadoFisco,
     TpcnMotivoDesoneracaoICMS(-1)]);
end;

function StrTomotDesICMS(out ok: boolean; const s: string): TpcnMotivoDesoneracaoICMS;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                                   '10', '11', '12', '16', '90', ''],
    [mdiTaxi, mdiDeficienteFisico, mdiProdutorAgropecuario, mdiFrotistaLocadora,
     mdiDiplomaticoConsular, mdiAmazoniaLivreComercio, mdiSuframa, mdiVendaOrgaosPublicos,
     mdiOutros, mdiDeficienteCondutor, mdiDeficienteNaoCondutor, mdiOrgaoFomento,
     mdiOlimpiadaRio2016, mdiSolicitadoFisco,
     TpcnMotivoDesoneracaoICMS(-1)]);
end;

function motDesICMSToStrTagPosText(const t: TpcnMotivoDesoneracaoICMS): string;
begin
  // 1 – Táxi;
  // 2 – Deficiente Físico;
  // 3 – Produtor Agropecuário;
  // 4 – Frotista/Locadora;
  // 5 – Diplomático/Consular;
  // 6 – Utilitários e Motocicletas da
  // Amazônia Ocidental e Áreas de
  // Livre Comércio (Resolução
  // 714/88 e 790/94 – CONTRAN e
  // suas alterações);
  // 7 – SUFRAMA;
  // 8 – Venda a Orgãos Publicos;
  // 9 – outros. (v2.0)
  // 10 – Deficiente Condutor (Convênio ICMS 38/12). (v3.1)
  // 11 – Deficiente não Condutor (Convênio ICMS 38/12). (v3.1)
  // 12 - Orgão Fomento
  // 16 - Olimpiadas Rio 2016
  // 90 - Solicitado pelo Fisco
result := EnumeradoToStr(t, ['1 – Táxi', '2 – Deficiente Físico', '3 – Produtor Agropecuário',
  '4 – Frotista/Locadora', '5 – Diplomático/Consular', '6 - Utilit./Motos da Am./Áreas Livre Com.',
  '7 – SUFRAMA', '8 – Venda a Orgãos Publicos', '9 – Outros', '10 – Deficiente Condutor',
  '11 – Deficiente não Condutor', '12 - Orgão Fomento', '16 - Olimpiadas Rio 2016',
  '90 - Solicitado pelo Fisco', ''],
  [mdiTaxi, mdiDeficienteFisico, mdiProdutorAgropecuario, mdiFrotistaLocadora,
   mdiDiplomaticoConsular, mdiAmazoniaLivreComercio, mdiSuframa, mdiVendaOrgaosPublicos,
   mdiOutros, mdiDeficienteCondutor, mdiDeficienteNaoCondutor, mdiOrgaoFomento,
   mdiOlimpiadaRio2016, mdiSolicitadoFisco,
   TpcnMotivoDesoneracaoICMS(-1)]);
end;

// CST IPI *********************************************************************
function CSTIPIToStr(const t: TpcnCstIpi): string;
begin
  result := EnumeradoToStr(t, ['00', '49', '50', '99', '01', '02', '03', '04', '05', '51', '52', '53', '54', '55'],
    [ipi00, ipi49, ipi50, ipi99, ipi01, ipi02, ipi03, ipi04, ipi05, ipi51, ipi52, ipi53, ipi54, ipi55]);
end;

function StrToCSTIPI(out ok: boolean; const s: string): TpcnCstIpi;
begin
  result := StrToEnumerado(ok, s, ['00', '49', '50', '99', '01', '02', '03', '04', '05', '51', '52', '53', '54', '55'],
    [ipi00, ipi49, ipi50, ipi99, ipi01, ipi02, ipi03, ipi04, ipi05, ipi51, ipi52, ipi53, ipi54, ipi55]);
end;

function CSTIPIToStrTagPosText(const t: TpcnCstIpi): string;
begin
  case t of
    ipi00 : Result := '00 - Entrada com Recuperação de Crédito';
    ipi01 : Result := '01 - Entrada Tributável com Alíquota Zero';
    ipi02 : Result := '02 - Entrada Isenta';
    ipi03 : Result := '03 - Entrada Não-Tributada';
    ipi04 : Result := '04 - Entrada Imune';
    ipi05 : Result := '05 - Entrada com Suspensão';
    ipi49 : Result := '49 - Outras Entradas';
    ipi50 : Result := '50 - Saída Tributada';
    ipi51 : Result := '51 - Saída Tributável com Alíquota Zero';
    ipi52 : Result := '52 - Saída Isenta';
    ipi53 : Result := '53 - Saída Não-Tributada';
    ipi54 : Result := '54 - Saída Imune';
    ipi55 : Result := '55 - Saída com Suspensão';
    ipi99 : Result := '99 - Outras Saídas';
  end;
end;

// 401i - Indicador da origem do processo **************************************
function indProcToStr(const t: TpcnIndicadorProcesso): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '9'],
  [ipSEFAZ, ipJusticaFederal, ipJusticaEstadual, ipSecexRFB, ipCONFAZ, ipOutros]);
end;

function indProcToDescrStr(const t: TpcnIndicadorProcesso): string;
begin
  case t of
    ipSEFAZ           : result  := 'SEFAZ';
    ipJusticaFederal  : result  := 'JUSTIÇA FEDERAL';
    ipJusticaEstadual : result  := 'JUSTIÇA ESTADUAL';
    ipSecexRFB        : result  := 'SECEX / RFB';
    ipCONFAZ          : Result  := 'CONFAZ';
    ipOutros          : result  := 'OUTROS';
  end;
end;

function StrToindProc(out ok: boolean; const s: string): TpcnIndicadorProcesso;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '9'],
  [ipSEFAZ, ipJusticaFederal, ipJusticaEstadual, ipSecexRFB, ipCONFAZ, ipOutros]);
end;

// 49a - Código do Regime Tributário **************************************
function CRTToStr(const t: TpcnCRT): string;
begin
  result := EnumeradoToStr(t, ['','1', '2', '3', '4'],
    [crtRegimeNormal, crtSimplesNacional, crtSimplesExcessoReceita,
     crtRegimeNormal, crtMEI]);
end;

function StrToCRT(out ok: boolean; const s: string): TpcnCRT;
begin
  result := StrToEnumerado(ok, s, ['','1', '2', '3', '4'],
    [crtRegimeNormal, crtSimplesNacional, crtSimplesExcessoReceita,
     crtRegimeNormal, crtMEI]);
end;

function CRTTocRegTrib(const t: TpcnCRT): TpcnRegTrib;
begin
  if T = crtSimplesNacional then
    Result := RTSimplesNacional
  else
    Result := RTRegimeNormal;
end;

// 117b - Indicador de soma no total da NFe **************************************
function indTotToStr(const t: TpcnIndicadorTotal): string;
begin
  result := EnumeradoToStr(t, ['0', '1'], [itNaoSomaTotalNFe, itSomaTotalNFe]);
end;

function StrToindTot(out ok: boolean; const s: string): TpcnIndicadorTotal;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [itNaoSomaTotalNFe, itSomaTotalNFe]);
end;

// B20k - Modelo Refenciado  **************************************
function ECFModRefToStr(const t: TpcnECFModRef): string;
begin
    result := EnumeradoToStr(t, ['', '2B', '2C','2D'],[ECFModRefVazio ,ECFModRef2B,ECFModRef2C,ECFModRef2D]);
end;

function StrToECFModRef(out ok: boolean; const s: string): TpcnECFModRef;
begin
  result := StrToEnumerado(ok, s, ['', '2B', '2C','2D'],[ECFModRefVazio, ECFModRef2B,ECFModRef2C,ECFModRef2D]);
end;

function ISSQNcSitTribToStr(const t: TpcnISSQNcSitTrib ): string;
begin
    result := EnumeradoToStr(t, ['','N','R','S','I'],[ISSQNcSitTribVazio , ISSQNcSitTribNORMAL, ISSQNcSitTribRETIDA, ISSQNcSitTribSUBSTITUTA,ISSQNcSitTribISENTA]);
end;

function StrToISSQNcSitTrib(out ok: boolean; const s: string) : TpcnISSQNcSitTrib;
begin
  result := StrToEnumerado(ok, s, ['','N','R','S','I'],[ISSQNcSitTribVazio , ISSQNcSitTribNORMAL, ISSQNcSitTribRETIDA, ISSQNcSitTribSUBSTITUTA,ISSQNcSitTribISENTA]);
end;

function ISSQNcSitTribToStrTagPosText(const t: TpcnISSQNcSitTrib): string;
begin
  result := EnumeradoToStr(t, ['','N - Normal','R - Retida','S - Substituta','I - Isenta'],
  [ISSQNcSitTribVazio , ISSQNcSitTribNORMAL, ISSQNcSitTribRETIDA, ISSQNcSitTribSUBSTITUTA,ISSQNcSitTribISENTA]);
end;

function DestinoOperacaoToStr(const t: TpcnDestinoOperacao): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'],
                              [doInterna, doInterestadual, doExterior]);
end;

function StrToDestinoOperacao(out ok: boolean; const s: string): TpcnDestinoOperacao;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'],
                                  [doInterna, doInterestadual, doExterior]);
end;

function ConsumidorFinalToStr(const t: TpcnConsumidorFinal): string;
begin
  result := EnumeradoToStr(t, ['0', '1'],
                              [cfNao, cfConsumidorFinal]);
end;

function StrToConsumidorFinal(out ok: boolean; const s: string): TpcnConsumidorFinal;
begin
  result := StrToEnumerado(ok, s, ['0', '1'],
                                  [cfNao, cfConsumidorFinal]);
end;

function PresencaCompradorToStr(const t: TpcnPresencaComprador): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '9'],
                              [pcNao, pcPresencial, pcInternet, pcTeleatendimento, pcEntregaDomicilio,
                              pcPresencialForaEstabelecimento, pcOutros]);
end;

function StrToPresencaComprador(out ok: boolean; const s: string): TpcnPresencaComprador;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '9'],
                                  [pcNao, pcPresencial, pcInternet, pcTeleatendimento, pcEntregaDomicilio,
                                  pcPresencialForaEstabelecimento, pcOutros]);
end;

function FormaPagamentoToStr(const t: TpcnFormaPagamento): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '10', '11', '12',
                               '13', '14', '15', '16', '17', '18', '19', '90',
                               '98', '99', '20', '21', '22'],
                              [fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito,
                               fpCreditoLoja, fpValeAlimentacao, fpValeRefeicao,
                               fpValePresente, fpValeCombustivel, fpDuplicataMercantil,
                               fpBoletoBancario, fpDepositoBancario,
                               fpPagamentoInstantaneo, fpTransfBancario,
                               fpProgramaFidelidade, fpSemPagamento, fpRegimeEspecial,
                               fpOutro, fpPagamentoInstantaneoEstatico,
                               fpCreditoEmLojaPorDevolucao, fpFalhaHardware]);
end;

function FormaPagamentoToDescricao(const t: TpcnFormaPagamento): string; overload;
begin
  Result := FormaPagamentoToDescricao(t, '');
end;

function FormaPagamentoToDescricao(const t: TpcnFormaPagamento; const xPag: String): string; overload;
begin
  if (t = fpOutro) and (xPag <> '') then
    result := xPag
  else
    result := EnumeradoToStr(t,  ['Dinheiro', 'Cheque', 'Cartão de Crédito',
                                'Cartão de Débito', 'Cartão da Loja (Private Label)',
                                'Vale Alimentação', 'Vale Refeição', 'Vale Presente',
                                'Vale Combustível', 'Duplicata Mercantil',
                                'Boleto Bancário', 'Deposito Bancário',
                                'PIX - Dinâmico', 'Transferência Bancária',
                                'Programa Fidelidade', 'Sem Pagamento',
                                'Regime Especial NFF', 'Outro', 'PIX - Estático',
                                'Crédito em Loja', 'Falha de hardware do sistema emissor'],
                              [fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito,
                               fpCreditoLoja, fpValeAlimentacao, fpValeRefeicao,
                               fpValePresente, fpValeCombustivel, fpDuplicataMercantil,
                               fpBoletoBancario, fpDepositoBancario,
                               fpPagamentoInstantaneo, fpTransfBancario,
                               fpProgramaFidelidade, fpSemPagamento, fpRegimeEspecial,
                               fpOutro, fpPagamentoInstantaneoEstatico,
                               fpCreditoEmLojaPorDevolucao, fpFalhaHardware]);
end;

function StrToFormaPagamento(out ok: boolean; const s: string): TpcnFormaPagamento;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '10', '11', '12',
                                   '13', '14', '15', '16', '17', '18', '19', '90',
                                   '98', '99', '20', '21', '22'],
                              [fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito,
                               fpCreditoLoja, fpValeAlimentacao, fpValeRefeicao,
                               fpValePresente, fpValeCombustivel, fpDuplicataMercantil,
                               fpBoletoBancario, fpDepositoBancario,
                               fpPagamentoInstantaneo, fpTransfBancario,
                               fpProgramaFidelidade, fpSemPagamento, fpRegimeEspecial,
                               fpOutro,fpPagamentoInstantaneoEstatico,
                               fpCreditoEmLojaPorDevolucao, fpFalhaHardware]);
end;

function BandeiraCartaoToDescStr(const t: TpcnBandeiraCartao): string;
begin
  case t of
    bcVisa:            Result := 'Visa';
    bcMasterCard:      Result := 'MasterCard';
    bcAmericanExpress: Result := 'AmericanExpress';
    bcSorocred:        Result := 'Sorocred';
    bcDinersClub:      Result := 'Diners Club';
    bcElo:             Result := 'Elo';
    bcHipercard:       Result := 'Hipercard';
    bcAura:            Result := 'Aura';
    bcCabal:           Result := 'Cabal';
    bcAlelo:           Result := 'Alelo';
    bcBanesCard:       Result := 'BanesCard';
    bcCalCard:         Result := 'CalCard';
    bcCredz:           Result := 'Credz';
    bcDiscover:        Result := 'Discover';
    bcGoodCard:        Result := 'GoodCard';
    bcGreenCard:       Result := 'GreenCard';
    bcHiper:           Result := 'Hiper';
    bcJcB:             Result := 'JcB';
    bcMais:            Result := 'Mais';
    bcMaxVan:          Result := 'MaxVan';
    bcPolicard:        Result := 'Policard';
    bcRedeCompras:     Result := 'RedeCompras';
    bcSodexo:          Result := 'Sodexo';
    bcValeCard:        Result := 'ValeCard';
    bcVerocheque:      Result := 'Verocheque';
    bcVR:              Result := 'VR';
    bcTicket:          Result := 'Ticket';
    bcOutros:          Result := 'Outros'
  end;
end;

function BandeiraCartaoToStr(const t: TpcnBandeiraCartao): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08',
                               '09', '10', '11', '12', '13', '14', '15', '16',
                               '17', '18', '19', '20', '21', '22', '23', '24',
                               '25', '26', '27', '99', ''],
                              [bcVisa, bcMasterCard, bcAmericanExpress, bcSorocred,
                               bcDinersClub, bcElo, bcHipercard, bcAura, bcCabal,
                               bcAlelo, bcBanesCard, bcCalCard, bcCredz, bcDiscover,
                               bcGoodCard, bcGreenCard, bcHiper, bcJcB, bcMais,
                               bcMaxVan, bcPolicard, bcRedeCompras, bcSodexo,
                               bcValeCard, bcVerocheque, bcVR, bcTicket, bcOutros,
                               TpcnBandeiraCartao(-1)]);
end;

function StrToBandeiraCartao(out ok: boolean; const s: string): TpcnBandeiraCartao;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08',
                                   '09', '10', '11', '12', '13', '14', '15', '16',
                                   '17', '18', '19', '20', '21', '22', '23', '24',
                                   '25', '26', '27', '99', ''],
                                  [bcVisa, bcMasterCard, bcAmericanExpress, bcSorocred,
                                   bcDinersClub, bcElo, bcHipercard, bcAura, bcCabal,
                                   bcAlelo, bcBanesCard, bcCalCard, bcCredz, bcDiscover,
                                   bcGoodCard, bcGreenCard, bcHiper, bcJcB, bcMais,
                                   bcMaxVan, bcPolicard, bcRedeCompras, bcSodexo,
                                   bcValeCard, bcVerocheque, bcVR, bcTicket, bcOutros,
                                   TpcnBandeiraCartao(-1)]);
end;

function TipoViaTranspToStr(const t: TpcnTipoViaTransp ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                               '10', '11', '12', '13'],
                              [tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal,
                               tvFerroviaria, tvRodoviaria, tvConduto, tvMeiosProprios,
                               tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque]);
end;

function StrToTipoViaTransp(out ok: boolean; const s: string): TpcnTipoViaTransp;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                                   '10', '11', '12', '13'],
                                  [tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal,
                                   tvFerroviaria, tvRodoviaria, tvConduto, tvMeiosProprios,
                                   tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque]);
end;

function TipoViaTranspToDescricao(const t: TpcnTipoViaTransp): string;
begin
  result := EnumeradoToStr(t, ['01 - Marítima',
                               '02 - Fluvial',
                               '03 - Lacustre',
                               '04 - Aérea',
                               '05 - Postal',
                               '06 - Ferroviária',
                               '07 - Rodoviária',
                               '08 - Conduto / Rede Transmissão',
                               '09 - Meios Próprios',
                               '10 - Entrada / Saída ficta',
                               '11 - Courier',
                               '12 - Em mãos',
                               '13 - Por reboque'],
                              [tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal,
                               tvFerroviaria, tvRodoviaria, tvConduto, tvMeiosProprios,
                               tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque]);
end;

function TipoIntermedioToStr(const t: TpcnTipoIntermedio ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'],
                              [tiContaPropria, tiContaOrdem, tiEncomenda]);
end;

function StrToTipoIntermedio(out ok: boolean; const s: string): TpcnTipoIntermedio;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'],
                                  [tiContaPropria, tiContaOrdem, tiEncomenda]);
end;

function TipoIntermedioToDescricao(const t: TpcnTipoIntermedio): string;
begin
  result := EnumeradoToStr(t, ['1 - Importação por conta própria',
                               '2 - Importação por conta e ordem',
                               '3 - Importação por encomenda'],
                              [tiContaPropria, tiContaOrdem, tiEncomenda]);
end;

function indISSRetToStr(const t: TpcnindISSRet ): string;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [iirSim, iirNao]);
end;

function StrToindISSRet(out ok: boolean; const s: string): TpcnindISSRet;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [iirSim, iirNao]);
end;

function indISSToStr(const t: TpcnindISS ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7'],
                              [iiExigivel, iiNaoIncidencia, iiIsencao, iiExportacao,
                               iiImunidade, iiExigSuspDecisaoJudicial, iiExigSuspProcessoAdm]);
end;

function StrToindISS(out ok: boolean; const s: string): TpcnindISS;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6', '7'],
                                  [iiExigivel, iiNaoIncidencia, iiIsencao, iiExportacao,
                                   iiImunidade, iiExigSuspDecisaoJudicial, iiExigSuspProcessoAdm]);
end;

function indISSToStrTagPosText(const t: TpcnindISS): string;
begin
  result := EnumeradoToStr(t, ['1 - Exigível', '2 - Não incidência', '3 - Isenção', '4 - Exportação',
                               '5 - Imunidade', '6 - Exig. Susp. Dec. Jud.', '7 - Exig. Susp. Proc. Adm.'],
                              [iiExigivel, iiNaoIncidencia, iiIsencao, iiExportacao,
                               iiImunidade, iiExigSuspDecisaoJudicial, iiExigSuspProcessoAdm]);
end;

function TipoAutorToStr(const t: TpcnTipoAutor ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '5', '6', '9'],
                              [taEmpresaEmitente, taEmpresaDestinataria,
                               taEmpresa, taFisco, taRFB, taOutros]);
end;

function StrToTipoAutor(out ok: boolean; const s: string): TpcnTipoAutor;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '5', '6', '9'],
                                  [taEmpresaEmitente, taEmpresaDestinataria,
                                   taEmpresa, taFisco, taRFB, taOutros]);
end;

function IndOperacaoToStr(const t: TpcnIndOperacao ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'],
                              [ioConsultaCSC, ioNovoCSC, ioRevogaCSC]);
end;

function StrToIndOperacao(out ok: boolean; const s: string): TpcnIndOperacao;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'],
                                  [ioConsultaCSC, ioNovoCSC, ioRevogaCSC]);
end;

function indSomaPISSTToStr(const t: TIndSomaPISST): String;
begin
  Result := EnumeradoToStr(t, ['', '0', '1'],
                                [ispNenhum, ispPISSTNaoCompoe, ispPISSTCompoe]);
end;

function StrToindSomaPISST(out ok: Boolean; const s: String): TIndSomaPISST;
begin
  Result := StrToEnumerado(ok, s, ['', '0', '1'],
                                [ispNenhum, ispPISSTNaoCompoe, ispPISSTCompoe]);
end;

function indSomaCOFINSSTToStr(const t: TIndSomaCOFINSST): String;
begin
  Result := EnumeradoToStr(t, ['', '0', '1'],
                          [iscNenhum, iscCOFINSSTNaoCompoe, iscCOFINSSTCompoe]);
end;

function StrToindSomaCOFINSST(out ok: Boolean; const s: String): TIndSomaCOFINSST;
begin
  Result := StrToEnumerado(ok, s, ['', '0', '1'],
                          [iscNenhum, iscCOFINSSTNaoCompoe, iscCOFINSSTCompoe]);
end;

function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110110', '110111', '110112', '110140', '111500',
             '111501', '111502', '111503', '210200', '210210', '210220',
             '210240', '610600', '610614', '790700', '990900', '990910',
             '110180', '610554', '610510', '610615', '610610', '110130',
             '110131', '110150', '610130', '610131', '610601', '110192',
             '110193', '610514', '610500', '110750', '110751'],
            [teNaoMapeado, teCCe, teCancelamento, teCancSubst, teEPECNFe,
             tePedProrrog1, tePedProrrog2, teCanPedProrrog1, teCanPedProrrog2,
             teManifDestConfirmacao, teManifDestCiencia,
             teManifDestDesconhecimento, teManifDestOperNaoRealizada,
             teRegistroCTe, teMDFeAutorizadoComCTe, teAverbacaoExportacao,
             teVistoriaSuframa, teConfInternalizacao, teComprEntrega,
             teRegPasAutMDFeComCte, teRegPasNfeProMDFe,
             teCancelamentoMDFeAutComCTe, teMDFeAutorizado,
             teComprEntregaNFe, teCancComprEntregaNFe, teAtorInteressadoNFe,
             teComprEntregaCTe, teCancComprEntregaCTe, teCTeCancelado,
             teInsucessoEntregaNFe, teCancInsucessoEntregaNFe,
             teRegPasNfeProMDFeCte, teRegistroPassagemNFe, teConcFinanceira,
             teCancConcFinanceira]);
end;

function LayOutToServico(const t: TLayOut): String;
begin
  Result := EnumeradoToStr(t,
    ['NfeRecepcao', 'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
     'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
     'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
     'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
     'NFeDistribuicaoDFe', 'EventoEPEC'],
    [ LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento, LayNfeInutilizacao,
      LayNfeConsulta, LayNfeStatusServico, LayNfeCadastro,
      LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
      LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
      LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC ] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;
begin
  Result := StrToEnumerado(ok, s,
  ['NfeRecepcao', 'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
   'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
   'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
   'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
   'NFeDistribuicaoDFe', 'EventoEPEC'],
  [ LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento, LayNfeInutilizacao,
    LayNfeConsulta, LayNfeStatusServico, LayNfeCadastro,
    LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
    LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
    LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC ] );
end;

function LayOutToSchema(const t: TLayOut): TSchemaNFe;
begin
  case t of
    LayNfeRecepcao:       Result := schNfe;
    LayNfeRetRecepcao:    Result := schconsReciNFe;
    LayNfeCancelamento:   Result := schcancNFe;
    LayNfeInutilizacao:   Result := schInutNFe;
    LayNfeConsulta:       Result := schconsSitNFe;
    LayNfeStatusServico:  Result := schconsStatServ;
    LayNfeCadastro:       Result := schconsCad;
    LayNFeCCe,
    LayNFeEvento,
    LayNFeEventoAN:       Result := schenvEvento;
    LayNFeConsNFeDest:    Result := schconsNFeDest;
    LayNFeDownloadNFe:    Result := schdownloadNFe;
    LayNfeAutorizacao:    Result := schNfe;
    LayNfeRetAutorizacao: Result := schretEnviNFe;
    LayAdministrarCSCNFCe: Result := schadmCscNFCe;
    LayDistDFeInt:        Result := schdistDFeInt;
    LayNFCeEPEC:          Result := scheventoEPEC;
  else
    Result := schErro;
  end;
end;

function SchemaNFeToStr(const t: TSchemaNFe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaNFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNFe(const s: String): TSchemaNFe;
var
  P: Integer;
  SchemaStr: String;
  CodSchema: Integer;
begin
  P := pos('_',s);
  if p > 0 then
    SchemaStr := copy(s,1,P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr,3) <> 'sch' then
    SchemaStr := 'sch'+SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaNFe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNFe válido.',[SchemaStr]));
  end;

  Result := TSchemaNFe( CodSchema );
end;

// B25 - Finalidade de emissão da NF-e *****************************************
function FinNFeToStr(const t: TpcnFinalidadeNFe): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [fnNormal, fnComplementar, fnAjuste, fnDevolucao]);
end;

function StrToFinNFe(out ok: Boolean; const s: String): TpcnFinalidadeNFe;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [fnNormal, fnComplementar, fnAjuste, fnDevolucao]);
end;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
    [inTodas, inSemManifestacaoComCiencia, inSemManifestacaoSemCiencia]);
end;

function StrToIndicadorNFe(out ok: Boolean; const s: String): TpcnIndicadorNFe;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
    [inTodas, inSemManifestacaoComCiencia, inSemManifestacaoSemCiencia]);
end;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
    [veqr000, veqr100, veqr200]);
end;

function StrToVersaoQrCode(out ok: Boolean; const s: String): TpcnVersaoQrCode;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
    [veqr000, veqr100, veqr200]);
end;

function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Real;
begin
  case t of
    veqr000: Result := 0;
    veqr100: Result := 1;
    veqr200: Result := 2;
  else
    Result := 0;
  end;
end;

function ModeloDFToStr(const t: TpcnModeloDF): String;
begin
  Result := EnumeradoToStr(t, ['55', '65'], [moNFe, moNFCe]);
end;

function StrToModeloDF(out ok: Boolean; const s: String): TpcnModeloDF;
begin
  Result := StrToEnumerado(ok, s, ['55', '65'], [moNFe, moNFCe]);
end;

function ModeloDFToPrefixo(const t: TpcnModeloDF): String;
begin
  Case t of
    moNFCe: Result := 'NFCe';
  else
    Result := 'NFe';
  end;
end;

function StrToVersaoDF(out ok: Boolean; const s: String): TpcnVersaoDF;
begin
  Result := StrToEnumerado(ok, s, ['2.00', '3.00', '3.10', '4.00'], [ve200, ve300, ve310, ve400]);
end;

function VersaoDFToStr(const t: TpcnVersaoDF): String;
begin
  Result := EnumeradoToStr(t, ['2.00', '3.00', '3.10', '4.00'], [ve200, ve300, ve310, ve400]);
end;

 function DblToVersaoDF(out ok: Boolean; const d: Real): TpcnVersaoDF;
 begin
   ok := True;

   if (d = 2.0) or (d < 3.0)  then
     Result := ve200
   else if (d >= 3.0) and (d < 3.1) then
     Result := ve300
   else if (d >= 3.10) and (d < 4) then
     Result := ve310
   else if (d >= 4) then
     Result := ve400
   else
   begin
     Result := ve310;
     ok := False;
   end;
 end;

 function VersaoDFToDbl(const t: TpcnVersaoDF): Real;
 begin
   case t of
     ve200: Result := 2.00;
     ve300: Result := 3.00;
     ve310: Result := 3.10;
     ve400: Result := 4.00;
   else
     Result := 0;
   end;
 end;

// J02 - Tipo da operação ******************************************************
 function tpOPToStr(const t: TpcnTipoOperacao): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '0'], [toVendaConcessionaria, toFaturamentoDireto, toVendaDireta, toOutros]);
end;

function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '0'], [toVendaConcessionaria, toFaturamentoDireto, toVendaDireta, toOutros]);
end;

// J22 - Condição do Veículo ***************************************************
function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'], [cvAcabado, cvInacabado, cvSemiAcabado]);
end;

function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'], [cvAcabado, cvInacabado, cvSemiAcabado]);
end;

// L02 - Indicador do tipo de arma de fogo *************************************
function tpArmaToStr(const t: TpcnTipoArma): string;
begin
  result := EnumeradoToStr(t, ['0', '1'], [taUsoPermitido, taUsoRestrito]);
end;

function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [taUsoPermitido, taUsoRestrito]);
end;

function VeiculosRestricaoStr( const iRestricao : Integer ): String;
begin
  case iRestricao of
    0: result := '0-NÃO HÁ';
    1: result := '1-ALIENAÇÃO FIDUCIÁRIA';
    2: result := '2-RESERVA DE DOMICÍLIO';
    3: result := '3-RESERVA DE DOMÍNIO';
    4: result := '4-PENHOR DE VEÍCULOS';
    9: result := '9-OUTRAS'
    else
      result := IntToStr(iRestricao)+ 'NÃO DEFINIDO' ;
  end;
end;

function VeiculosCorDENATRANStr( const sCorDENATRAN : String ): String;
begin
  case StrToIntDef( sCorDENATRAN, 0 ) of
     1: result := '01-AMARELO';
     2: result := '02-AZUL';
     3: result := '03-BEGE';
     4: result := '04-BRANCA';
     5: result := '05-CINZA';
     6: result := '06-DOURADA';
     7: result := '07-GRENÁ';
     8: result := '08-LARANJA';
     9: result := '09-MARROM';
    10: result := '10-PRATA';
    11: result := '11-PRETA';
    12: result := '12-ROSA';
    13: result := '13-ROXA';
    14: result := '14-VERDE';
    15: result := '15-VERMELHA';
    16: result := '16-FANTASIA'
    else
      result := sCorDENATRAN + 'NÃO DEFINIDA' ;
  end;
end;

function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): String;
begin
  case condVeic of
    cvAcabado     : result := '1-ACABADO';
    cvInacabado   : result := '2-INACABADO';
    cvSemiAcabado : result := '3-SEMI-ACABADO';
  end;
end;

function VeiculosVinStr( const sVin: String ): String;
begin
  // Enumerar Vim no futuro ?
  if sVIN = 'R' then
      result := 'R-REMARCADO'
  else
    if sVIN = 'N' then
      result:= 'N-NORMAL'
    else
      result := 'NÃO DEFINIDA' ;
end;

function VeiculosEspecieStr( const iEspecie : Integer ): String;
begin
  case iEspecie of
    1: result := '01-PASSAGEIRO';
    2: result := '02-CARGA';
    3: result := '03-MISTO';
    4: result := '04-CORRIDA';
    5: result := '05-TRAÇÃO';
    6: result := '06-ESPECIAL';
    7: result := '07-COLEÇÃO'
    else
      result := IntToStr(iEspecie ) + 'NÃO DEFINIDA' ;
    end;
end;

function VeiculosTipoStr( const iTipoVeic : Integer ): String;
begin
  case iTipoVeic of
     1: result := '01-BICICLETA';
     2: result := '02-CICLOMOTOR';
     3: result := '03-MOTONETA';
     4: result := '04-MOTOCICLETA';
     5: result := '05-TRICICLO';
     6: result := '06-AUTOMÓVEL';
     7: result := '07-MICROONIBUS';
     8: result := '08-ONIBUS';
     9: result := '09-BONDE';
    10: result := '10-REBOQUE';
    11: result := '11-SEMI-REBOQUE';
    12: result := '12-CHARRETE';
    13: result := '13-CAMIONETA';
    14: result := '14-CAMINHÃO';
    15: result := '15-CARROÇA';
    16: result := '16-CARRO DE MÃO';
    17: result := '17-CAMINHÃO TRATOR';
    18: result := '18-TRATOR DE RODAS';
    19: result := '19-TRATOR DE ESTEIRAS';
    20: result := '20-TRATOR MISTO';
    21: result := '21-QUADRICICLO';
    22: result := '22-CHASSI/PLATAFORMA';
    23: result := '23-CAMINHONETE';
    24: result := '24-SIDE-CAR';
    25: result := '25-UTILITÁRIO';
    26: result := '26-MOTOR-CASA'
    else
      result := IntToStr(iTipoVeic)+'NÃO DEFINIDO' ;
    end;
end;

function VeiculosCombustivelStr( const sTpComb : String ): String;
begin
  case StrToIntDef( stpComb, 0) of
     1: result := '01-ÁLCOOL';
     2: result := '02-GASOLINA';
     3: result := '03-DIESEL';
     4: result := '04-GASOGÊNIO';
     5: result := '05-GÁS METANO';
     6: result := '06-ELETRICO/F INTERNA';
     7: result := '07-ELETRICO/F EXTERNA';
     8: result := '08-GASOLINA/GNC';
     9: result := '09-ÁLCOOL/GNC';
    10: result := '10-DIESEL / GNC';
    11: result := '11-VIDE CAMPO OBSERVAÇÃO';
    12: result := '12-ÁLCOOL/GNV';
    13: result := '13-GASOLINA/GNV';
    14: result := '14-DIESEL/GNV';
    15: result := '15-GÁS NATURAL VEICULAR';
    16: result := '16-ÁLCOOL/GASOLINA';
    17: result := '17-GASOLINA/ÁLCOOL/GNV';
    18: result := '18-GASOLINA/ELÉTRICO'
    else
      result := stpComb +'NÃO DEFINIDO' ;
    end;
end;

function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): String;
begin
  case TtpOP of
    toVendaConcessionaria : result := '1-VENDA CONCESSIONÁRIA';
    toFaturamentoDireto   : result := '2-FAT. DIRETO CONS. FINAL';
    toVendaDireta         : result := '3-VENDA DIRETA';
    toOutros              : result := '0-OUTROS';
  end;

end;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): String;
begin
  case TtpArma of
    taUsoPermitido: result := '0-USO PERMITIDO';
    taUsoRestrito : result := '1-USO RESTRITO';
  end;
end;

function IndEscalaToStr(const t: TpcnIndEscala): String;
begin
  result := EnumeradoToStr(t, ['S', 'N', ''],
                              [ieRelevante, ieNaoRelevante, ieNenhum]);
end;

function StrToIndEscala(out ok: Boolean; const s: String): TpcnIndEscala;
begin
  result := StrToEnumerado(ok, s, ['S', 'N', ''],
                                  [ieRelevante, ieNaoRelevante, ieNenhum]);
end;

// ??? - Modalidade do frete ***************************************************
function modFreteToStr(const t: TpcnModalidadeFrete): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '9'],
    [mfContaEmitente, mfContaDestinatario, mfContaTerceiros, mfProprioRemetente, mfProprioDestinatario, mfSemFrete]);
end;

function StrTomodFrete(out ok: boolean; const s: string): TpcnModalidadeFrete;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2',  '3', '4', '9'],
    [mfContaEmitente, mfContaDestinatario, mfContaTerceiros, mfProprioRemetente, mfProprioDestinatario, mfSemFrete]);
end;

function modFreteToDesStr(const t: TpcnModalidadeFrete; versao: TpcnVersaoDF): string;
begin
  case versao of
    ve200,
    ve300,
    ve310:
      case t  of
        mfContaEmitente       : result := '0 - EMITENTE';
        mfContaDestinatario   : result := '1 - DEST/REM';
        mfContaTerceiros      : result := '2 - TERCEIROS';
        mfProprioRemetente    : result := '3 - PROP/REMT';
        mfProprioDestinatario : result := '4 - PROP/DEST';
        mfSemFrete            : result := '9 - SEM FRETE';
      end;
    ve400:
      case t  of
        mfContaEmitente       : result := '0 - REMETENTE';
        mfContaDestinatario   : result := '1 - DESTINATARIO';
        mfContaTerceiros      : result := '2 - TERCEIROS';
        mfProprioRemetente    : result := '3 - PROP/REMT';
        mfProprioDestinatario : result := '4 - PROP/DEST';
        mfSemFrete            : result := '9 - SEM FRETE';
      end;
  end;
end;

function SchemaEventoToStr(const t: TSchemaNFe): String;
begin
  result := EnumeradoToStr(t, ['e110110', 'e110111', 'e110112', 'e110140',
                               'e111500', 'e111501', 'e111502', 'e111503',
                               'e210200', 'e210210', 'e210220', 'e210240',
                               'e110130', 'e110131', 'e110150', 'e110192',
                               'e110193', 'e110750', 'e110751'],
    [schEnvCCe, schcancNFe, schCancSubst, schEnvEPEC,
     schPedProrrog1, schPedProrrog2, schCanPedProrrog1, schCanPedProrrog2,
     schManifDestConfirmacao, schManifDestCiencia, schManifDestDesconhecimento,
     schManifDestOperNaoRealizada, schCompEntrega, schCancCompEntrega,
     schAtorInteressadoNFe, schInsucessoEntregaNFe, schCancInsucessoEntregaNFe,
     schConcFinanceira, schCancConcFinanceira]);
end;

function AutorizacaoToStr(const t: TAutorizacao): string;
begin
  result := EnumeradoToStr(t, ['0', '1', ''],
                              [taNaoPermite, taPermite, taNaoInformar]);
end;

function StrToAutorizacao(out ok: boolean; const s: string): TAutorizacao;
begin
  result := StrToEnumerado(ok, s, ['0', '1', ''],
                                  [taNaoPermite, taPermite, taNaoInformar]);
end;

function IndIntermedToStr(const t: TindIntermed): string;
begin
  Result := EnumeradoToStr(t, ['', '0', '1'],
       [iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador]);
end;

function StrToIndIntermed(out ok: boolean; const s: string): TindIntermed;
begin
  Result := StrToEnumerado(ok, s, ['', '0', '1'],
       [iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador]);
end;

function tpAtoToStr(const t: TtpAto): string;
begin
  Result := EnumeradoToStr(t, ['', '08', '10', '12', '14', '15'],
       [taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS]);
end;

function StrTotpAto(out ok: boolean; const s: string): TtpAto;
begin
  Result := StrToEnumerado(ok, s, ['', '08', '10', '12', '14', '15'],
       [taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS]);
end;

function indImportToStr(const t: TindImport): string;
begin
  Result := EnumeradoToStr(t, ['0', '1'],
       [iiNacional, iiImportado]);
end;

function StrToindImport(out ok: boolean; const s: string): TindImport;
begin
  Result := StrToEnumerado(ok, s, ['0', '1'],
       [iiNacional, iiImportado]);
end;

function motRedAdRemToStr(const t: TmotRedAdRem): string;
begin
  Result := EnumeradoToStr(t, ['1', '9'],
       [motTranspColetivo, motOutros]);
end;

function StrTomotRedAdRem(out ok: boolean; const s: string): TmotRedAdRem;
begin
  Result := StrToEnumerado(ok, s, ['1', '9'],
       [motTranspColetivo, motOutros]);
end;

function tpMotivoToStr(const t: TtpMotivo): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro]);
end;

function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNFe, 'NFe');

end.

