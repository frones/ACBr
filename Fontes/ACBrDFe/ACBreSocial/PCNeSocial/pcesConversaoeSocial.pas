{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2015: Guilherme Costa
|*  - Adicionado algumas conversões
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}

unit pcesConversaoeSocial;

interface

uses
  SysUtils, StrUtils, Classes;

type
  // Tipificacao = tp + Nome da Variavel ou do campo
    //  Itens da Tipicacao = 2 ou 3 letras da Variavel ou campo + Nome da opcao
    // Unkon = Nao definido

  // Nome das funcoes tipicacao do ESocial ( es )
    //  De tipicacao para String = es+ Nome da Variavel ou do campo + ToStr
    //  De String para tipicacao = esStrTo+ Nome da Variavel ou do campo

  TEmpregador = (tePessoaJuridica, teOrgaoPublico, tePessoaFisica,
                 teOrgaoPublicoExecutivoFederal, teOrgaoPublicoLegislativoFederal,
                 teOrgaoPublicoJudiciarioFederal, teOrgaoPublicoAutonomoFederal);

  TeSocialGrupo = (egIniciais = 1, egNaoPeriodicos = 2, egPeriodicos = 3);

  TeSocialEventos =(eseEnvioLote, eseRetornoLote, eseEnvioConsulta,
                    eseRetornoConsulta, eseEnvioConsultaIdentEvt,
                    eseRetornoConsultaIdentEvt, eseEnvioDownloadEvt,
                    eseRetornoDownloadEvt);

  TModoLancamento         = (mlInclusao, mlAlteracao, mlExclusao);

  TeSocialSchema          = (schErro, schConsultaLoteEventos, schEnvioLoteEventos,
                             schevtAdesao, schevtAdmissao, schevtAdmPrelim,
                             schevtAfastTemp, schevtAltCadastral, schevtAltContratual,
                             schevtAqProd, schevtAvPrevio, schevtBasesTrab, schevtBenPrRP,
                             schevtCadInicial, schevtCAT, schevtCdBenPrRP,
                             schevtComProd, schevtContratAvNP, schevtContrSindPatr,
                             schevtConvInterm, schevtCS, schevtDeslig, schevtExclusao,
                             schevtExpRisco, schevtFechaEvPer, schevtInfoComplPer,
                             schevtInfoEmpregador, schevtInsApo, schevtIrrf,
                             schevtIrrfBenef, schevtMonit, schevtPgtos,
                             schevtReabreEvper, schevtReintegr, schevtRemun, schevtRmnRPPS,
                             schevtTabAmbiente, schevtTabCargo, schevtTabCarreira, schevtTabEstab,
                             schevtTabFuncao, schevtTabHorTur, schevtTabLotacao, schevtTabOperPort,
                             schevtTabProcesso, schevtTabRubrica, schevtTotConting, schevtTSVAltContr,
                             schevtTSVInicio, schevtTSVTermino, schRetornoEnvioLoteEventos,
                             schRetornoEvento, schRetornoProcessamentoLote,
                             schConsultaIdentEventos, schDownloadEventos, schEvtToxic, schEvtTreiCap);

  TLayOut                 = (LayEnvioLoteEventos, LayConsultaLoteEventos,
                             LayConsultaIdentEventos, LayDownloadEventos);

  TStatusACBreSocial      = (stIdle, stEnvLoteEventos, stConsultaLote,
                             stConsultaIdentEvt, stDownloadEvt);

  TTipoEvento             = (teS1000, teS1005, teS1010, teS1020, teS1030, teS1035, teS1040, teS1050,
                             teS1060, teS1070, teS1080, teS2100, teS1200, teS1202, teS1207, teS1210,
                             teS1220, teS1250, teS1260, teS1270, teS1280, teS1295, teS1298, teS1299,
                             teS1300, teS2190, teS2200, teS2205, teS2206, teS2210, teS2220, teS2230,
                             teS2240, teS2245, teS2250, teS2260, teS2298, teS2299, teS2300, teS2305,
                             teS2306, teS2399, teS2400, teS3000, teS4000, teS4999, teS5001, teS5002,
                             teS5003, teS5011, teS5012, teS5013, teS2221);

  tpSimNao                = (tpSim, tpNao);

  TpProcEmi               = (peAplicEmpregador, peAplicGovernamental);

  tpIndSegmento           = (isNormal );

  tpTpInsc                = (tiCNPJ, tiCPF, tiCAEPF, tiCNO, tiCGC);

  TpTpInscProp            = (tpCNPJ, tpCPF );

  TpIndCoop               = (icNaoecooperativa, icCooperativadeTrabalho, icCooperativadeProducao, icOutrasCooperativas );

  TpIndConstr             = (iconNaoeConstrutora, iconEmpresaConstrutora);

  TpIndDesFolha           = (idfNaoAplicavel, idfEmpresaenquadradanosArt7_9);

  TpIndOptRegEletron      = (iorNaooptou, iorOptoupeloregistro);

  TpIndOpcCP              = (icpComercializacao, icpFolhaPagamento, icpNenhum);

  tpAliqRat               = (arat1, arat2, arat3 );

  tpTpProc                = (tpAdministrativo, tpJudicial, tpINSS, tpFAP);

  tpSiglaMin              = (smCNAS, smMEC, smMS, smMDS, smLEI);

  tpIndAcordoIsencaoMulta = (iaiSemacordo, iaiComacordo);

  TptpInscContratante     = (icCNPJ, icCPF);

  tpuf                    = (ufAC, ufAL, ufAP, ufAM, ufBA, ufCE, ufDF, ufES, ufGO, ufMA,
                             ufMT, ufMS, ufMG, ufPA, ufPB, ufPR, ufPE, ufPI, ufRJ, ufRN,
                             ufRS, ufRO, ufRR, ufSC, ufSP, ufSE, ufTO, ufEX);

  tpIndSituacaoEspecial   = (iseSituacaoNormal, iseExtincao, iseFusao, iseCisao, iseIncorporacao);

  tpIndProvDesc           = (ipdProvento, ipdDesconto, ipdInformativa );

  tpCodIncCP              = (cicNaoeBasedeCalculo,
                             cicNaoeBasedeCalculoAcordoInternacionalPrevidenciaSocial,
                             cicBasedeCalculodoSalariodeContribuicaoMensal,
                             cicBasedeCalculodaContribPrevsobre13oSalario,
                             cicBasedeCalculodaContribExclusivaEmpregadorMensal,
                             cicBasedeCalculodaContribExclusivaEmpregador13oSalario,
                             cicBasedeCalculodaContribExclusivaSeguradoMensal,
                             cicBasedeCalculodaContribExclusivaSegurado13oSalario,
                             cicBasedeCalculodaContribPrevSalMaternidade,
                             cicBasedeCalculodaContribPrevSalMaternidade13oSalario,
                             ciBasedeCalcContribPrevAuxilioDoencaRegPropriodePrevSocial,
                             ciBasedeCalcContribPrevAuxDoenca13oSalDoencaRegPropPrevSocial,
                             cicBasedeCalculodaContribMaternidadeINSS,
                             cicBasedeCalculodaContribMaternidade13INSS,
                             cicContribuicaoDescontadadoSegurado,
                             cicContribuicaoDescontadadoSegurado13oSalario,
                             cicContribuicaoDescontadadoSeguradoSEST,
                             cicContribuicaoDescontadadoSeguradoSENAT,
                             cicSalarioFamilia,
                             ciComplementodeSalMinRegimePropriodePrevSocial,
                             cicIncidsuspensajudicialBCSCMensal,
                             cicIncidsuspensajudicialBC13oSalario,
                             cicIncidsuspensajudicialBCSalMaternidade,
                             cicIncidsuspensajudicialBCSalMaternidade13oSalario,
                             cicIncidExclusivaEmpregadorMensal,
                             cicIncidExclusivaEmpregador13osalario,
                             cicIncidExclusivaEmpregadorSalMaternidade,
                             cicIncidExclusivaEmpregadorSalMaternidade13oSalario);

  tpCodIncIRRF            = (ciiNaoeBasedeCalculo, ciiNaoeBasedeCalculoAcordoInternacional,
                             ciiOutrasVerbasNaoConsideradas,
                             ciiBasedeCalculoIRRFRemMensal, ciiBasedeCalculoIRRF13oSalario,
                             ciiBasedeCalculoIRRFFerias, ciiBasedeCalculoIRRFPLR, ciiBasedeCalculoIRRFRRA,
                             ciiValorIRRFRemMensal, ciiValorIRRF13oSalario, ciiValorIRRFFerias, ciiValorIRRFPLR,
                             ciiValorIRRFPLRRRA, ciPrevSocialOficialRemMensal, ciPrevSocialOficial13Salario,
                             ciPrevSocialOficialFerias, ciPrevSocialOficialRRA, ciPrevPrivadaSalarioMensal,
                             ciPrevPrivada13Salario, ciPensaoAlimenticiaRemMensal, ciPensaoAlimenticia13Salario,
                             ciPensaoAlimenticiaFerias, ciPensaoAlimenticiaPLR, ciPensaoAlimenticiaRRA,
                             ciFundoAposentadoriaProgIndFAPIRemMensal,
                             ciFundoAposentadoriaProgIndFAPI13Salario, ciFundacaoPrevCompServidorPubFederalFunprespRemMensal,
                             ciFundacaoPrevCompServidorPubFederalFunpresp13Salario, ciParcelaIsenta65AnosRemMensal,
                             ciParcelaIsenta65Anos13Salario, ciDiaria, ciAjudaDeCusto, ciIndenizacaoRescisaoContratoInclusiveAtituloPDV,
                             ciAbonoPecuniario, ciPensaoAposOureformaPorMolestiaGraveOuAcidServicoRemMensal,
                             ciPensaoAposOureformaPorMolestiaGraveOuAcidServico13Salario,
                             ciValPagosATitOuSocioMicroEmpOuEmpPeqPorteExcProLaboreEAlugueis,
                             ciOutros, ciDepositoJudicial, ciCompJudicialAnoCalendario, ciCompJudicialAnosAnteriores,
                             ciiIncidenciasuspensajudicialBCIRRF, ciiIncidenciasuspensajudicialBCIRRF13Salario,
                             ciiIncidenciasuspensajudicialBCIRRFFerias, ciiIncidenciasuspensajudicialBCIRRFPLR,
                             ciiIncidenciasuspensajudicialBCIRRFRRA);

  tpCodIncFGTS            = (cdfNaoeBasedeCalculo, cdfBasedeCalculoFGTS, cdfBasedeCalculoFGTS13Sal, cdfBasedeCalculoFGTSRescisorio, cdfIncidenciadecisaojudicial);

  tpCodIncSIND            = (cisNaoebasedecalculo, cisBasedecalculo, cisValorlaboraldescontada, cisIncidenciasuspensajudicial);

  tpExtDecisao            = (edContribPatronais, edContribPatronaisSegurados );

  tpTpInscEstab           = (tpI1, tpI3, tpI4, tpVazio ); // Vazio para seguir regra do manual
                                                          // Validação: O campo não deve ser preenchido se {tpLotacao} for
                                                          // igual a [07,10,90]. Nos demais casos, observar conteúdo
                                                          // exigido para o campo {nrInscEstab}, conforme tabela de tipos
                                                          // de lotação.

  tpTpIntervalo           = (tinHorarioFixo, tinHorarioVariavel);

  tpIndSubstPatronalObra  = (ispPatronalSubstituida, ispPatronalNaoSubstituida);

  tpIndDecisao            = (idLiminarMandado, idDepositoJudicial, idDepositoAdministrativo, idAntecipacao,
                             idLiminarMedidaCautelar, idDecisaoNaoTransitada, idContestacaoFAP, idDefinitiva);

  tpindAutoria            = (iaProprioContribuinte, iaOutraEntidade);

  tpIndMatProc            = (impTributaria, impAutorizacaoTrabalhadorMenor,
                             impDispensaPCD, impDispensaAprendiz,
                             impSegurancaoeSaudeTrabalhador,
                             impConversaoLicencaSaudeAcidenteTrabalho,
                             impFGTS, impContribuicaoSindical, impOutros);

  tpIndRetificacao        = (ireOriginal, ireRetificacao);

  tpIndApuracao           = (iapuMensal, ipaAnual);

  tpInfoApurGrauRisco     = (iagCNAEEmpresa, iagCNAEEstabelecimento);

  tpIndApurAliqFap        = (iaaFAPEmpresa, iaaFAPEstabelecimento);

  tpIndMV                 = (imvDescontadaempregador, imvDescontadaoutras, imvSobrelimite);

  tpIndSimples            = (idsNenhum, idsIntegralmente, idsNaosubstituida, idsConcomitante);

  tpNatAtividade          = (navNaoInformar, navUrbano, navRural );

  tpTpTributo             = (tptIRRF, tptPrevidenciaria, tptFGTS, tptContribSind);

  tpGrauExp               = (ge1, ge2, ge3, ge4);

  tpTpAcordo              = (tpaAcordo, tpaComissao, tpaConvenca, tpaDissidio );

  tpIndNIF                = (infBeneficiaNIF, infDispensadoNIF, infPaisnaoNIF );

  tpIndTpDeducao          = (itpOficial, itpPrivada, itpPensao, itpDependentes );

  tpTpIsencao             = (tpiParcelaIsenta, tpiDiariaCusto, tpiIndenizacao, tpiAbono, tpiOutros, tpiLucros,
                             tpiValorespagos, tpiPensao, tpiBeneficios, tpiBolsa, tpiComplementacao);

  tpIndPerReferencia      = (iprFolhaMensal, iprFolhaDecimo);

  tpTpProcRRA             = (tppAdministrativo, tppJudicial);

  tpTpInscAdvogado        = (tadJuridica, tadFisica );

  tpIndOrigemRecursos     = (iorproprio, iorterceiros );

  tpRelDependencia        = (rdConjuge, rdCompanheiro, rdPaiMae, rdFilho, rdEnteado, rdAgregado);

  tpIndObra               = (tioNaoeobra, tioEmpreitadaTotal, tioEmpreitadaParcial, tioSubempreitada );

  tpIndIncidencia         = (indNormal, indAtivConcomitante, indSubstituida );

  tpIndAbrangencia        = (iarTodos, iarEstabelecimento );

  tpIndAquisicao          = (idaGeral, idaEntidade, idaEntidadePAA);

  tpIndComercializacao    = (idcProdRuralAgroindustria, idcProdRuralSegEspecial, idcProdRuralSegEspecialVendas,
                             idcEntidadeinscrita, idcMercadoExterno );

  tpTpRepasse             = (tprPatrocinio, tprLicenciamento, tprPublicidade, tprPropaganda, tprTransmissao);

  tpIdeSubteto            = (isExecutivo, isLegislativo, isJudiciario, isTodosPoderes);

  tpTpPublAlvo            = (tpaServAtiv, tpaAposent, tpaAposentPorInvalidez, tpaPensionista, tpaMilitarAtivo, tpaSeguradoDiferen);

  tpIndSitPF              = (isPFSitNormal, isPFEncerrEspolio, isPFSaidaPaisCaraterPermanente);

  tpIndSitPJ              = (isPJSitNormal, isPJExtincao, isPJFusao, isPJCisao, isPJIncorporacao);

  tpTpRubr                = (tpVencimento, tpDesconto, tpInformativa, tpInformativaDedutora);

  tpSitCargo              = (scCriacao, scExtincao, scReestruturacao);

  tpSitCarr               = tpSitCargo;

  tpAcumCargo             = (acNaoAcumulavel, acProfSaude, acProfessor, acTecCientifico);

  tpContagemEsp           = (ceNao, ceProfInfantilFundamentalMedio, ceProfEnsSuperiorMagistradoMembroMinisPublicoTribContas, ceAtividadedeRisco);

  tpUtilizEPC             = (uEPCNaoAplica, uEPCNaoImplementa, uEPCImplementa);

  tpUtilizEPI             = (uEPINaoAplica, uEPINaoUtilizado, uEPIUtilizado);

  tpLocalAmb              = (laEstabProprioEmpregador, laEstabTerceiro, laPrestInstalacaoTerceiro);

  tpTpAcConv              = (tacAcordoColTrab, tacLegislacaoFederalEstadualMunicipalDistrital, tacConvencaoColTrab,
                             tacSetencNormativa, tacConversaoLicenSaudeAcidTrabalho, tacOutrasVerbas);

  tpIndSusp               = (siLiminarMandadoSeguranca, siDepositoJudicial, siDepositoAdministrativo, siAntecipacaoTutela, siLiminarMedidaCautelar,
                             siSentencaMandadoSegurancaFavoravelContribuinte, siSentencaAcaoOrdinariaFavContribuinteConfirmadaPeloTRF,
                             siAcordaoTRFFavoravelContribuinte, siAcordaoSTJRecursoEspecialFavoravelContribuinte,
                             siAcordaoSTFRecursoExtraordinarioFavoravelContribuinte, siSentenca1instanciaNaoTransitadaJulgadoEfeitoSusp,
                             siContestacaoAdministrativaFAP, siDecisaoDefinitivaAFavorDoContribuinte,
                             siSemSuspensaoDaExigibilidade);

  tpTpInscAmbTab          = (atCNPJ, atCAEPF);

  tpCnh                   = (cnA, cnB, cnC, cnD, cnE, cnAB, cnAC, cnAD, cnAE );

  tpClassTrabEstrang      = (ctVistoPermanente, ctVistoTemporario, ctAsilado, ctRefugiado, ctSolicitanteRefugio,
                             ctResidentePaisFrontBrasil, ctDefFisicoMais51Anos, ctComResidenciaProvAnistiadoSituacaoIrregular,
                             ctPermanenciaBrasilRazaoFilhosOuConjugeBras, ctBeneficiadoAcordoPaisesMercosul,
                             ctDependenteAgenteDiplomaticoOuConsular, ctBeneficiadoTratadoAmizade);

  tpTpDep                 = (tdConjuge, tdCompanheiroComFilhoOuVivaMais5Anos, tdFilhoOuEnteado, tdFilhoOuEnteadoUniverOuEscolaTec,
                             tdIrmaoNetoBisnetoGuardaJudicial, tdIrmaoNetoBisnetoUniverOuEscolaTecGuardaJudicial,
                             tdPaisAvosBisavos, tdMenorPobreGuardaJudicial, tdPessoaIncapazTutorOuCurador, tdExConjuge,
                             tdAgregadoOutros);

  tpTpRegTrab             = (trCLT, trEstatutario);

  tpTpRegPrev             = (rpRGPS, rpRPPS, rpRPPE);

  tpTpAdmissao            = (taAdmissao,
                             taTransfEmpresaMesmoGrupoEconomico,
                             taTransfEmpresaConsorciadaOuDeConsorcio,
                             taTransfPorMotivoSucessaoIncorporacaoCisaoOuFuso,
                             taTransfEmpregadoDomesticoParaOutroRepresentanteMesmaFamiliar,
                             taMudancaDeCPF);

  tpTpIndAdmissao         = (iaNormal, iaDecorrenteAcaoFiscal, iaDecorrenteDecisaoJudicial);

  tpTpRegJor              = (rjSubmetidosHorarioTrabalho, rjAtividadeExtEspecificadaIncisoIArt62CLT,
                             rjFuncoesEspecificadasIncisoIIArt62CLT, rjTeletrabalhoPrevistoIncisoIIIArt62CLT);

  tpOpcFGTS               = (ofOptante, ofNaoOptante);

  tpMtvContrat            = (mcNecessidadeTransitoriaSubstituicaoPessoalRegular, mcAcrescimoExtraordinarioServicos);

  tpIndProvim             = (ipNormal, ipDecorrenteDecisaoJudicial);

  tpTpProv                = (tpNomeacaoCargoEfetivo, tpNomeacaoCargoComissao, tpIncorporacao, tpMatricula, tpReinclusao, tpDiplomacao, tpOutros);

  tpUndSalFixo            = (sfPorHora, sfPorDia, sfPorSemana, sfPorQuinzena, sfPorMes, sfPorTarefa, sfNaoaplicavel);

  tpTpContr               = (PrazoIndeterminado, PrazoDeterminado, PrazoDeterminadoVincOcDeUmFato);

  tpTpJornada             = (tjJornadaComHorarioDiarioFolgaFixos, tjJornada12x36, tjJornadaComHorarioDiarioFixoFolgaVariavel, tjDemaisTiposJornada);

  tpTpDia                 = (diSegundaFeira, diTercaFeira, diQuartaFeira, diQuintaFeira, diSextaFeira, diSabado, diDomingo, diDiaVariavel);

  tpTpExameOcup           = (taAdmissional, taPeriodico, taRetornoAoTrabalho, taMudancaDeFuncao, taMonitoracaoPontual, taDemissional);

  tpResAso                = (raApto, raInapto);

  TpMatBiolog             = (mbUrina, mbSangue);

  tpInterprExm            = (ieEE, ieSC, ieSCMais);

  tpOrdExame              = (orReferencial, oeSequencial);

  tpIndResult             = (irNormal, irAlterado, irEstavel, irAgravamento);

  tpTpAcid                = (taTipico, taDoenca, taTrajetoParaLocalTrabalhoOuEntreLocalTrabalhoEResidEmp);

  tpTpCat                 = (tcInicial, tcReabertura, tcComunicacaoObito);

  tpTpRegistrador         = (trEmpregador, trCooperativa, trSindicatoTrabAvulsosNaoPortuarios, trOrgaoGestorMaoObra,
                             trEmpregado, trDependenteEmpregado, trEntidadeSindicalCompetente, trMedicoAssistente, trAutoridadePublica);

  tpIniciatCAT            = (icIniciativaEmpregador, icOrdemJudicial, icDeterminacaoOrgaoFiscalizador);

  tpTpLocal               = (tlEstabEmpregadorBrasil, tlEstabEmpregadorExterior, tlEmpresaOndeEmpregadorPrestaServico, tlViaPublica, tlAreaRural, tlEmbarcacao, tlOutros);

  tpLateralidade          = (laNaoAplicavel, laEsquerda, laDireita, laAmbas);

  tpIdeOC                 = (idCRM, idCRO, idRMS, idCREA, idOutros);

  tpTpAvPrevio            = (tpAvPrevTrabDadoPeloEmpregadorAoEmpregadoRed2horasDiarias, tpAvPrevTrabDadoPeloEmpregadorAoEmpregadoRedDiasCorridos,
                             tpAvPrevPeloEmpregadoNaoDispensadoDeSeuCumprimento, tpAvPrevTrabDadoPeloEmpregadorRuralComRed1DiaporSemana,
                             tpAvPrevTrabDecorrenteDeAcordoEmpregadoEmpregador);

  tpMtvCancAvPrevio       = (mcReconsideracaoPrevistaArt489CLT, mcDeterminacaoJudicial, mcCumprimentoNormaLegal, mcOutros);

  tpTpReint               = (trReintegracaoDecisaoJudicial, trReintegracaoAnistiaLegal, trReversaoServidorPublico, trReconducaoServidorPublico,
                             trReinclusoMilitar, trOutros);

  tpNrLeiAnistia          = (nrLEI6683_1979, nrLEI8632_1993, nrLEI8878_1994, nrLEI10559_2002, nrLEI10790_2003, nrLEI11282_2006);

  tpTpContribSind         = (csContribSindical, csContribAssociativa, csContribAssistencial, csContribConfederativa);

  tpIndSubstPatrOpPort    = (spIntegralmenteSubstituida, spParcialmenteSubstituida);

  tpIdAquis               = (iaAquiProducaoProdutorRuralPessoaFisSegEspGeral, iaAquiProducaoProdutorRuralPessoaFisSegEspGeralEntPAA,
                             iaAquiProducaoProdutorRuralPessoaJurEntPAA, iaAquiProducaoProdutorRuralPessoaFisSegEspGeralProdIsenta,
                             iaAquiProducaoProdutorRuralPessoaFisSegEspGeralEntPAAProdIsenta, iaAquiProducaoProdutorRuralPessoaJurEntPAAProdIsenta);

  tpIndComerc             = (icComProdPorProdRuralPFInclusiveSegEspEfetuadaDirVarejoConsFinal, icComProdPorProdRuralPFSegEspVendasPJOuIntermPF,
                             icComProdPorProdIsenta, icComProdPorPFSegEspEntProgAquiAliPAA, icComProdMercadoExterno);

  tpTpPgto                = (tpPgtoRemun1200, tpPgtoResc2299, tpPgtoResc2399, tpPgtoRemun1202, tpPgtoBenefPrev1207,
                             tpPgtoFerias, tpPgtoCompAnt);

  tpMotivosAfastamento    = ( mtvAcidenteDoencaTrabalho, mtvAcidenteDoencaNaoTrabalho, mtvAfastLicencaRegimeProprioSemRemuneracao,
                              mtvAposentadoriaInvalidez, mtvAcompanhamentoFamiliaEnfermo, mtvAfastamentoEmpregadoConCuradorFGTS,
                              mtvAfastLicencaRegimeProprioComRemuneracao, mtvCarcere, mtvCargoEletivoCeletistasGeral,
                              mtvCargoEletivoServidorPublico, mtvCessaoRequisicao, mtvGozoFeriasRecesso, mtvLicencaRemunerada,
                              mtvLicencaMaternidade120Dias, mtvLicencaMaternidadeEmpresaCidada, mtvLicencaMaternidadeAbortoNaoCriminoso,
                              mtvLicencaMaternidadeAdocaoGuardaJudicial, mtvLicencaNaoRemunerada, mtvMandatoEleitoralSemRemuneracao,
                              mtvMandatoEleitoralComRemuneracao, mtvMandatoSindical, mtvMulherVitimaViolencia, mtvParticipacaoCNPS,
                              mtvQualificacao, mtvRepresentanteSindical, mtvServicoMilitar, mtvSuspensaoDisciplinar, mtvServidorPublicoDisponibilidade,
                              mtvLicencaMaternidade180Dias, mtvInatividadetrabalhadorAvulso90Dias, mtvLicencaMaternidadeAntecipacaoProrrogacao);

  tpTpAcidTransito        = (tpatAtropelamento, tpatColisao, tpatOutros, tpatNao);

  tpInfOnus               = (ocCedente, ocCessionario, ocCedenteCessionario);

  tpOnusRemun             = (orEmpregador, orSindicato, orEmpregadorSindicato);

  tpNatEstagio            = (neObrigatiorio, neNaoObrigatorio);

  tpNivelEstagio          = (nvFundamental, nvMedio, nvEnsinoProfis, nvSuperior, nvEspecial, nvMaeSocial);

  tpCaepf                 = (tcContrIndividual, tcProdRural, tcSegEspecial);//layout 2.1

  tpOpcConsult            = (ocContribPrevCPF, ocIRporCPF, ocTotContribSociais, ocTotIRRF);

  tpRegPt                 = (rpNaoUtiliza, rpManual, rpMecanico, rpEletronico, rpNaoEletronicoAlternativo, rpEletronicoAlternativo, rpEletronicoOutros);

  tpContApr               = (caDispensado, caDispensadoParcialmente, caObrigado);

  tpContPCD               = (cpcdDispensado, cpcdDispensadoParcialmente, cpcdExigibilidadeSuspensa, cpcdObrigado);

  tpInclContr             = (icLocaisSemFiliais, icEstudoDeMercado, icContratacaoSuperior3Meses);

  tpPlanRP                = (prpPlanoPrevidenciarioOuUnico, prpPlanoFinanceiro);

  tpMtvAlt                = (maPromocao, maReadaptacao, maAproveitamento, maOutros);

  tpOrigemAltAfast        = (oaaPorIniciativaDoEmpregador, oaaRevisaoAdministrativa, oaaDeterminacaoJudicial);

  tpPensaoAlim            = (paNaoExistePensaoAlimenticia, paPercentualDePensaoAlimenticia, paValorDePensaoAlimenticia,
                             paPercentualeValordePensaoAlimenticia);

  tpCumprParcialAviso     = (cpaCumprimentoTotal, cpaCumprimentoParcialNovoEmprego, cpaCumprimentoParcialEmpregador,
                             cpaOutrasCumprimentoParcial, cpaAvisoprevioIndenizadoNaoExigivel );

  tpTpAval               = (tpaQuantitativo, tpaQualitativo);

  tpModTreiCap           = (mtcPresencial, mtcEaD, mtcSemipresencial);

  tpTpTreiCap            = (ttcInicial, ttcPeriodico, ttcReciclagem, ttcEventual, ttcOutros);

  tpTpProf               = (ttpProfissionalEmpregado, ttpProfissionalSemVinculo);

  tpNacProf              = (ttpProfissionalBrasileiro, ttpProfissionalEstrangeiro);

  TVersaoeSocial = (ve02_04_01, ve02_04_02, ve02_05_00);

  tpTmpParc = (tpNaoeTempoParcial, tpLimitado25HorasSemanais, tpLimitado30HorasSemanais, tpLimitado26HorasSemanais);

  // ct00 não consta no manual mas consta no manual do desenvolvedor pg 85, é usado para zerar a base de teste.
  TpClassTrib = (ct00, ct01, ct02, ct03, ct04, ct06, ct07, ct08, ct09, ct10, ct11,
                 ct13, ct14, ct21, ct22, ct60, ct70, ct80, ct85, ct99);

  tpConsulta = (tcEmpregador, tcTabela, tcTrabalhador);

Const
  PrefixoVersao = '-v';

  TEventoString: array[0..43] of String =('evtInfoEmpregador', 'evtTabEstab',
                                          'evtTabRubrica', 'evtTabLotacao',
                                          'evtTabCargo', 'evtTabCarreira',
                                          'evtTabFuncao', 'evtTabHorTur',
                                          'evtTabAmbiente', 'evtTabProcesso',
                                          'evtTabOperPort', 'S-2100', 'evtRemun',
                                          'evtRmnRPPS', 'evtBenPrRP', 'evtPgtos',
                                          'S-1220', 'evtAqProd', 'evtComProd',
                                          'evtContratAvNP', 'evtInfoComplPer',
                                          'evtTotConting', 'evtReabreEvPer',
                                          'evtFechaEvPer', 'evtContrSindPatr',
                                          'evtAdmPrelim', 'evtAdmissao',
                                          'evtAltCadastral', 'evtAltContratual',
                                          'evtCAT', 'evtMonit', 'evtAfastTemp',
                                          'evtExpRisco', 'evtInsApo', 'evtAvPrevio',
                                          'evtConvInterm', 'evtReintegr',
                                          'evtDeslig', 'evtTSVInicio', 'S-2305',
                                          'evtTSVAltContr', 'evtTSVTermino',
                                          'evtCdBenPrRP', 'evtExclusao');

function TipoEventoToStr(const t: TTipoEvento ): string;
function StrToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function StrEventoToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function StringINIToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function StringXMLToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function TipoEventoToStrEvento(const t: TTipoEvento ): string;

function eSprocEmiToStr(const t: TpprocEmi ): string;
function eSStrToprocEmi(var ok: boolean; const s: string): TpprocEmi;

function eSIndSegmentoToStr(const t: TpIndSegmento ): string;
function eSStrToIndSegmento(var ok: boolean; const s: string): TpIndSegmento;

function eSTpInscricaoToStr(const t: tpTpInsc ): string;
function eSStrToTpInscricao(var ok: boolean; const s: string): tpTpInsc;

function eStpTpInscAmbTabToStr(const t: tpTpInscAmbTab ): string;
function eSStrTotpTpInscAmbTab(var ok: boolean; const s: string): tpTpInscAmbTab;

function eSIndCooperativaToStr(const t: TpIndCoop ): string;
function eSStrToIndCooperativa(var ok: boolean; const s: string): TpIndCoop;

function eSIndConstrutoraToStr(const t: TpIndConstr ): string;
function eSStrToIndConstrutora(var ok: boolean; const s: string): TpIndConstr;

function eSIndDesFolhaToStr(const t: TpIndDesFolha ): string;
function eSStrToIndDesFolha(var ok: boolean; const s: string): TpIndDesFolha;

function eSIndOptRegEletronicoToStr(const t: TpIndOptRegEletron ): string;
function eSStrToIndOptRegEletronico(var ok: boolean; const s: string): TpIndOptRegEletron;

function eSIndOpcCPToStr(const t: TpIndOpcCP ): string;
function eSStrToIndOpcCP(var ok: boolean; const s: string): TpIndOpcCP;

function eSAliqRatToStr(const t: TpAliqRat ): string;
function eSStrToAliqRat(var ok: boolean; const s: string): TpAliqRat;

function eSTpProcessoToStr(const t: tpTpProc ): string;
function eSStrToTpProcesso(var ok: boolean; const s: string): tpTpProc;

function eSSiglaMinToStr(const t: TpSiglaMin ): string;
function eSStrToSiglaMin(var ok: boolean; const s: string): TpSiglaMin;

function eSIndAcordoIsencaoMultaToStr(const t: TpIndAcordoIsencaoMulta ): string;
function eSStrToIndAcordoIsencaoMulta(var ok: boolean; const s: string): TpIndAcordoIsencaoMulta;

function eSufToStr(const t: Tpuf ): string;
function eSStrTouf(var ok: boolean; const s: string): Tpuf;

function eSIndSituacaoEspecialToStr(const t: TpIndSituacaoEspecial ): string;
function eSStrToIndSituacaoEspecial(var ok: boolean; const s: string): TpIndSituacaoEspecial;

function eSIndProvDescToStr(const t: TpIndProvDesc ): string;
function eSStrToIndProvDesc(var ok: boolean; const s: string): TpIndProvDesc;

function eSCodIncCPToStr(const t: tpCodIncCP ): string;
function eSStrToCodIncCP(var ok: boolean; const s: string): tpCodIncCP;

function eSCodIncIRRFToStr(const t: tpCodIncIRRF ): string;
function eSStrToCodIncIRRF(var ok: boolean; const s: string): tpCodIncIRRF;

function eSCodIncFGTSToStr(const t: TpCodIncFGTS ): string;
function eSStrToCodIncFGTS(var ok: boolean; const s: string): TpCodIncFGTS;

function eSCodIncSINDToStr(const t: TpCodIncSIND ): string;
function eSStrToCodIncSIND(var ok: boolean; const s: string): TpCodIncSIND;

function eSIndSuspToStr(const t: tpIndSusp): string;
function eSStrToIndSusp(var ok: Boolean; const s: string): tpIndSusp;

function eSExtDecisaoToStr(const t: TpExtDecisao ): string;
function eSStrToExtDecisao(var ok: boolean; const s: string): TpExtDecisao;

function eSTpInscEstabToStr(const t: TpTpInscEstab ): string;
function eSStrToTpInscEstab(var ok: boolean; const s: string): TpTpInscEstab;

function eStpInscContratanteToStr(const t: TptpInscContratante ): string;
function eSStrTotpInscContratante(var ok: boolean; const s: string): TptpInscContratante;

function eSTpInscPropToStr(const t: TpTpInscProp ): string;
function eSStrToTpInscProp(var ok: boolean; const s: string): TpTpInscProp;

function eSTpIntervaloToStr(const t: TpTpIntervalo ): string;
function eSStrToTpIntervalo(var ok: boolean; const s: string): TpTpIntervalo;

function eSIndSubstPatronalObraToStr(const t: TpIndSubstPatronalObra ): string;
function eSStrToIndSubstPatronalObra(var ok: boolean; const s: string): TpIndSubstPatronalObra;

function eSIndDecisaoToStr(const t: TpIndDecisao ): string;
function eSStrToIndDecisao(var ok: boolean; const s: string): TpIndDecisao;

function eSindAutoriaToStr(const t: TpindAutoria ): string;
function eSStrToindAutoria(var ok: boolean; const s: string): TpindAutoria;

function eSTpIndMatProcToStr(const t: tpIndMatProc): string;
function eSStrToTpIndMatProc(var ok: boolean; const s: string): tpIndMatProc;

function eSIndRetificacaoToStr(const t: TpIndRetificacao ): string;
function eSStrToIndRetificacao(out ok: boolean; const s: string): TpIndRetificacao;

function eSIndApuracaoToStr(const t: TpIndApuracao ): string;
function eSStrToIndApuracao(var ok: boolean; const s: string): TpIndApuracao;

function eSInfoApurGrauRiscoToStr(const t: TpInfoApurGrauRisco ): string;
function eSStrToInfoApurGrauRisco(var ok: boolean; const s: string): TpInfoApurGrauRisco;

function eSIndApurAliqFapToStr(const t: TpIndApurAliqFap ): string;
function eSStrToIndApurAliqFap(var ok: boolean; const s: string): TpIndApurAliqFap;

function eSIndMVToStr(const t: TpIndMV ): string;
function eSStrToIndMV(var ok: boolean; const s: string): TpIndMV;

function eSIndSimplesToStr(const t: TpIndSimples ): string;
function eSStrToIndSimples(var ok: boolean; const s: string): TpIndSimples;

function eSNatAtividadeToStr(const t: TpNatAtividade ): string;
function eSStrToNatAtividade(var ok: boolean; const s: string): TpNatAtividade;

function eSTpTributoToStr(const t: TpTpTributo ): string;
function eSStrToTpTributo(var ok: boolean; const s: string): TpTpTributo;

function eSGrauExpToStr(const t: TpGrauExp ): string;
function eSStrToGrauExp(var ok: boolean; const s: string): TpGrauExp;

function eSTpAcordoToStr(const t: TpTpAcordo ): string;
function eSStrToTpAcordo(var ok: boolean; const s: string): TpTpAcordo;

function eSIndNIFToStr(const t: TpIndNIF ): string;
function eSStrToIndNIF(var ok: boolean; const s: string): TpIndNIF;

function eSIndTpDeducaoToStr(const t: TpIndTpDeducao ): string;
function eSStrToIndTpDeducao(var ok: boolean; const s: string): TpIndTpDeducao;

function eSTpIsencaoToStr(const t: TpTpIsencao ): string;
function eSStrToTpIsencao(var ok: boolean; const s: string): TpTpIsencao;

function eSIndPerReferenciaToStr(const t: TpIndPerReferencia ): string;
function eSStrToIndPerReferencia(var ok: boolean; const s: string): TpIndPerReferencia;

function eSTpProcRRAToStr(const t: TpTpProcRRA ): string;
function eSStrToTpProcRRA(var ok: boolean; const s: string): TpTpProcRRA;

function eSTpInscAdvogadoToStr(const t: TpTpInscAdvogado ): string;
function eSStrToTpInscAdvogado(var ok: boolean; const s: string): TpTpInscAdvogado;

function eSIndOrigemRecursosToStr(const t: TpIndOrigemRecursos ): string;
function eSStrToIndOrigemRecursos(var ok: boolean; const s: string): TpIndOrigemRecursos;

function eSRelDependenciaToStr(const t: TpRelDependencia ): string;
function eSStrToRelDependencia(var ok: boolean; const s: string): TpRelDependencia;

function eSIndObraToStr(const t: TpIndObra ): string;
function eSStrToIndObra(var ok: boolean; const s: string): TpIndObra;

function eSIndIncidenciaToStr(const t: TpIndIncidencia ): string;
function eSStrToIndIncidencia(var ok: boolean; const s: string): TpIndIncidencia;

function eSIndAbrangenciaToStr(const t: TpIndAbrangencia ): string;
function eSStrToIndAbrangencia(var ok: boolean; const s: string): TpIndAbrangencia;

function eSIndAquisicaoToStr(const t: TpIndAquisicao ): string;
function eSStrToIndAquisicao(var ok: boolean; const s: string): TpIndAquisicao;

function eSIndComercializacaoToStr(const t: TpIndComercializacao ): string;
function eSStrToIndComercializacao(var ok: boolean; const s: string): TpIndComercializacao;

function eSTpRepasseToStr(const t: TpTpRepasse ): string;
function eSStrToTpRepasse(var ok: boolean; const s: string): TpTpRepasse;

function eSIdeSubtetoToStr(const t: tpIdeSubteto): string;
function eSStrToIdeSubteto(var ok: Boolean; const s: string): tpIdeSubteto;

function eSTpPublAlvoToStr(const t: tpTpPublAlvo): string;
function eSStrToTpPublAlvo(var ok: Boolean; const s: string): tpTpPublAlvo;

function eSIndSitPFToStr(const t: tpIndSitPF): string;
function eSStrToTpIndSitPF(var ok: Boolean; const s: string): tpIndSitPF;

function eSIndSitPJToStr(const t: tpIndSitPJ): string;
function eSStrToIndSitPJ(var ok: Boolean; const s: string): tpIndSitPJ;

function eSTpRubrToStr(const t: tpTpRubr): string;
function eSStrToTpRubr(var ok: Boolean; const s: string): tpTpRubr;

function eStpSitCargoToStr(const t: tpSitCargo): string;
function eSStrToSitCargo(var ok: Boolean; const s: string): tpSitCargo;

function eStpSitCarrToStr(const t: tpSitCarr): string;
function eSStrToSitCarr(var ok: Boolean; const s: string): tpSitCarr;

function eSAcumCargoToStr(const t: tpAcumCargo): string;
function eSStrToAcumCargo(var ok: Boolean; const s: string): tpAcumCargo;

function eSContagemEspToStr(const t: tpContagemEsp): string;
function eSStrToContagemEsp(var ok: Boolean; const s: string): tpContagemEsp;

function eStpUtilizEPCToStr(const t: tpUtilizEPC): string;
function eSStrTotpUtilizEPC(var ok: Boolean; const s: string): tpUtilizEPC;

function eStpUtilizEPIToStr(const t: tpUtilizEPI): string;
function eSStrTotpUtilizEPI(var ok: Boolean; const s: string): tpUtilizEPI;

function eSLocalAmbToStr(const t: tpLocalAmb): string;
function eSStrToLocalAmb(var ok: Boolean; const s: string): tpLocalAmb;

function eSTpAcConvToStr(const t: tpTpAcConv): string;
function eSStrToTpAcConv(var ok: Boolean; const s: string): tpTpAcConv;

function eSCnhToStr(const t: tpCnh): string;
function eSStrToCnh(var ok: Boolean; const s: string): tpCnh;

function eSClassTrabEstrangToStr(const t: tpClassTrabEstrang): string;
function eSStrToClassTrabEstrang(var ok: Boolean; const s: string): tpClassTrabEstrang;

function eStpDepToStr(const t: tpTpDep): string;
function eSStrToTpDep(var ok: Boolean; const s: string): tpTpDep;

function eSTpRegTrabToStr(const t: tpTpRegTrab ): string;
function eSStrToTpRegTrab(var ok: boolean; const s: string): tpTpRegTrab;

function eSTpRegPrevToStr(const t: tpTpRegPrev ): string;
function eSStrTotpRegPrev(var ok: boolean; const s: string): tpTpRegPrev;

function eSTpAdmissaoToStr(const t: tpTpAdmissao ): string;
function eSStrToTpAdmissao(var ok: boolean; const s: string): tpTpAdmissao;

function eSTpIndAdmissaoToStr(const t: tpTpIndAdmissao ): string;
function eSStrToTpIndAdmissao(var ok: boolean; const s: string): tpTpIndAdmissao;

function eSTpRegJorToStr(const t: tpTpRegJor ): string;
function eSStrToTpRegJor(var ok: boolean; const s: string): tpTpRegJor;

function eSOpcFGTSToStr(const t: tpOpcFGTS ): string;
function eSStrToOpcFGTS(var ok: boolean; const s: string): tpOpcFGTS;

function eSMtvContratToStr(const t: tpMtvContrat ): string;
function eSStrToMtvContrat(var ok: boolean; const s: string): tpMtvContrat;

function eSIndProvimToStr(const t: tpIndProvim ): string;
function eSStrToIndProvim(var ok: boolean; const s: string): tpIndProvim;

function eSTpProvToStr(const t: tpTpProv ): string;
function eSStrToTpProv(var ok: boolean; const s: string): tpTpProv;

function eSUndSalFixoToStr(const t: tpUndSalFixo ): string;
function eSStrToUndSalFixo(var ok: boolean; const s: string): tpUndSalFixo;

function eSTpContrToStr(const t: tpTpContr ): string;
function eSStrToTpContr(var ok: boolean; const s: string): tpTpContr;

function eSTpJornadaToStr(const t: tpTpJornada ): string;
function eSStrToTpJornada(var ok: boolean; const s: string): tpTpJornada;

function eSTpDiaToStr(const t: tpTpDia ): string;
function eSStrToTpDia(var ok: boolean; const s: string): tpTpDia;

function eSTpExameOcupToStr(const t: tpTpExameOcup ): string;
function eSStrToTpExameOcup(var ok: boolean; const s: string): tpTpExameOcup;

function eSResAsoToStr(const t: tpResAso ): string;
function eSStrToResAso(var ok: boolean; const s: string): tpResAso;

function eSMatBiologToStr(const t: TpMatBiolog ): string;
function eSStrToMatBiolog(var ok: boolean; const s: string): TpMatBiolog;

function eSInterprExmToStr(const t: tpInterprExm ): string;
function eSStrToInterprExm(var ok: boolean; const s: string): tpInterprExm;

function eSOrdExameToStr(const t: tpOrdExame ): string;
function eSStrToOrdExame(var ok: boolean; const s: string): tpOrdExame;

function eSIndResultToStr(const t: tpIndResult ): string;
function eSStrToIndResult(var ok: boolean; const s: string): tpIndResult;

function eSTpAcidToStr(const t: tpTpAcid ): string;
function eSStrToTpAcid(var ok: boolean; const s: string): tpTpAcid;

function eSTpCatToStr(const t: tpTpCat ): string;
function eSStrToTpCat(var ok: boolean; const s: string): tpTpCat;

function eSSimNaoToStr(const t: tpSimNao ): string;
function eSStrToSimNao(var ok: boolean; const s: string): tpSimNao;

function eSTpRegistradorToStr(const t: tpTpRegistrador ): string;
function eSStrToTpRegistrador(var ok: boolean; const s: string): tpTpRegistrador;

function eSIniciatCATToStr(const t: tpIniciatCAT ): string;
function eSStrToIniciatCAT(var ok: boolean; const s: string): tpIniciatCAT;

function eSTpLocalToStr(const t: tpTpLocal ): string;
function eSStrToTpLocal(var ok: boolean; const s: string): tpTpLocal;

function eSLateralidadeToStr(const t: tpLateralidade ): string;
function eSStrToLateralidade(var ok: boolean; const s: string): tpLateralidade;

function eSIdeOCToStr(const t: tpIdeOC ): string;
function eSStrToIdeOC(var ok: boolean; const s: string): tpIdeOC;

function eSTpAvPrevioToStr(const t: tpTpAvPrevio ): string;
function eSStrToTpAvPrevio(var ok: boolean; const s: string): tpTpAvPrevio;

function eSMtvCancAvPrevioToStr(const t: tpMtvCancAvPrevio ): string;
function eSStrToMtvCancAvPrevio(var ok: boolean; const s: string): tpMtvCancAvPrevio;

function eSTpReintToStr(const t: tpTpReint ): string;
function eSStrToTpReint(var ok: boolean; const s: string): tpTpReint;

function eSNrLeiAnistiaToStr(const t: tpNrLeiAnistia ): string;
function eSStrToNrLeiAnistia(var ok: boolean; const s: string): tpNrLeiAnistia;

function eSTpContribSindToStr(const t: tpTpContribSind ): string;
function eSStrToTpContribSind(var ok: boolean; const s: string): tpTpContribSind;

function eSIndSubstPatrOpPortStr(const t: tpIndSubstPatrOpPort ): string;
function eSStrToIndSubstPatrOpPort(var ok: boolean; const s: string): tpIndSubstPatrOpPort;

function eSIdAquisStr(const t: tpIdAquis ): string;
function eSStrToIdAquis(var ok: boolean; const s: string): tpIdAquis;

function eSIndComercStr(const t: tpIndComerc ): string;
function eSStrToIndComerc(var ok: boolean; const s: string): tpIndComerc;

function eSTpNatEstagioToStr(const t: tpNatEstagio): string;
function eSStrToTpNatEstagio(var ok: boolean; const s: string): tpNatEstagio;

function eSTpCaepfToStr(const t: tpCaepf): string;
function eSStrTotpCaepf(var ok: Boolean; const s: string): tpCaepf;

function eSTpTpPgtoToStr(const t: tpTpPgto): string;
function eSStrTotpTpPgto(var ok: Boolean; const s: string): tpTpPgto;

function eSTpOpcConsultToStr(const t: tpOpcConsult): string;
function eSStrTotpOpcConsult(var ok: Boolean; const s: string): tpOpcConsult;
  
function eSTpNivelEstagioToStr(const t: tpNivelEstagio): string;
function eSStrTotpNivelEstagio(var ok: Boolean; const s: string): tpNivelEstagio;

function eSTpRegPtToStr(const t: tpRegPt): string;
function eSStrTotpRegPt(var ok: boolean; const s: string): tpRegPt;

function eSTpContAprToStr(const t: tpContApr): string;
function eSStrTotpContApr(var ok: boolean; const s: string): tpContApr;

function eSTpContPCDToStr(const t: tpContPCD): string;
function eSStrToTpContPCD(var ok: boolean; const s: string): tpContPCD;

function eSTpInclContrToStr(const t: tpInclContr): string;
function eSStrToTpInclContr(var ok: boolean; const s: string): tpInclContr;

function eSTpPlanRPToStr(const t: tpPlanRP): string;
function eSStrToTpPlanRP(var ok: boolean; const s: string): tpPlanRP;

function eSTpMtvAltToStr(const t: tpMtvAlt): string;
function eSStrToTpMtvAlt(var ok: boolean; const s: string): tpMtvAlt;

function eSTpOrigemAltAfastToStr(const t: tpOrigemAltAfast): string;
function eSStrToTpOrigemAltAfast(var ok: boolean; const s: string): tpOrigemAltAfast;

function eSTpPensaoAlimToStr(const t: tpPensaoAlim): string;
function eSStrToTpPensaoAlim(var ok: boolean; const s: string): tpPensaoAlim;

function eSTpCumprParcialAvisoToStr(const t: tpCumprParcialAviso): string;
function eSStrToTpCumprParcialAviso(var ok: boolean; const s: string): tpCumprParcialAviso;

function eSModoLancamentoToStr(const t: TModoLancamento): string;
function eSStrToModoLancamento(var ok: boolean; const s: string): TModoLancamento;

function LayOuteSocialToServico(const t: TLayOut): String;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;

function LayOutToSchema(const t: TLayOut): TeSocialSchema;

function SchemaESocialToStr(const t: TeSocialSchema): String;
function StrToSchemaESocial(const s: String): TeSocialSchema;
function TipoEventiToSchemaReinf(const t: TTipoEvento): TeSocialSchema;

function StrToVersaoeSocial(out ok: Boolean; const s: String): TVersaoeSocial;
function VersaoeSocialToStr(const t: TVersaoeSocial): String;

function DblToVersaoeSocial(out ok: Boolean; const d: Real): TVersaoeSocial;
function VersaoeSocialToDbl(const t: TVersaoeSocial): Real;

function tpTmpParcToStr(const t: tpTmpParc ): string;
function StrTotpTmpParc(var ok: boolean; const s: string): tpTmpParc;

function tpClassTribToStr(const t: TpClassTrib ): string;
function StrTotpClassTrib(var ok: boolean; const s: string): TpClassTrib;

function eStpTpAcidTransitoToStr(const t: tpTpAcidTransito ): string;
function eSStrTotpTpAcidTransito(var ok: boolean; const s: string): tpTpAcidTransito;

function eStpMotivosAfastamentoToStr(const t: tpMotivosAfastamento ): string;
function eSStrTotpMotivosAfastamento(var ok: boolean; const s: string): tpMotivosAfastamento;

function tpInfOnusToStr(const t: tpInfOnus ): string;
function StrTotpInfOnus(var ok: boolean; const s: string): tpInfOnus;

function tpOnusRemunToStr(const t: tpOnusRemun ): string;
function StrTotpOnusRemun(var ok: boolean; const s: string): tpOnusRemun;

function tpAvalToStr(const t: tpTpAval ): string;
function StrTotpAval(var ok: boolean; const s: string): tpTpAval;

function tpModTreiCapToStr(const t: tpModTreiCap ): string;
function StrTotpModTreiCap(var ok: boolean; const s: string): tpModTreiCap;

function tpTpTreiCapToStr(const t: tpTpTreiCap ): string;
function StrTotpTpTreiCap(var ok: boolean; const s: string): tpTpTreiCap;

function tpTpProfToStr(const t: tpTpProf ): string;
function StrTotpTpProf(var ok: boolean; const s: string): tpTpProf;

function tpNacProfToStr(const t: tpNacProf ): string;
function StrTotpNacProf(var ok: boolean; const s: string): tpNacProf;

implementation

uses
  pcnConversao, typinfo;

const
  TTipoEventoString   : array[0..52] of String =('S-1000', 'S-1005', 'S-1010', 'S-1020', 'S-1030',
                                                 'S-1035', 'S-1040', 'S-1050', 'S-1060', 'S-1070',
                                                 'S-1080', 'S-2100', 'S-1200', 'S-1202', 'S-1207',
                                                 'S-1210', 'S-1220', 'S-1250', 'S-1260', 'S-1270',
                                                 'S-1280', 'S-1295', 'S-1298', 'S-1299', 'S-1300',
                                                 'S-2190', 'S-2200', 'S-2205', 'S-2206', 'S-2210',
                                                 'S-2220', 'S-2230', 'S-2240', 'S-2245', 'S-2250',
                                                 'S-2260', 'S-2298', 'S-2299', 'S-2300', 'S-2305',
                                                 'S-2306', 'S-2399', 'S-2400', 'S-3000', 'S-4000',
                                                 'S-4999', 'S-5001', 'S-5002', 'S-5003', 'S-5011',
                                                 'S-5012', 'S-5013', 'S-2221');

  TUFString           : array[0..27] of String = ('AC','AL','AP','AM','BA','CE','DF','ES','GO',
                                                  'MA','MT','MS','MG','PA','PB','PR','PE','PI',
                                                  'RJ','RN','RS','RO','RR','SC','SP','SE','TO', '');

  TModoLancamentoString : array[0..2] of String = ('inclusao', 'alteracao', 'exclusao');

  TSiglasMinString    : array[0..4] of string = ('CNAS','MEC','MS','MDS','LEI');

  TSimNaoString       : array[0..1] of string = ('S','N' );

  TIndicativoContratacaoPCD : array[0..3] of string = ('0', '1', '2', '9' );

  TMotivoAlteracaoCargoFuncao: array[0..3] of string = ('1', '2', '3', '9');

  TMotivoAfastamento: array[0..30] of string = ('01', '03', '05', '06', '07',
                                                '08', '10', '11', '12', '13',
                                                '14', '15', '16', '17', '18',
                                                '19', '20', '21', '22', '23',
                                                '24', '25', '26', '27', '28',
                                                '29', '30', '31', '33', '34',
                                                '35');

  TGenericosString0_1 : array[0..1] of string = ('0','1' );
  TGenericosString0_2 : array[0..2] of string = ('0','1','2' );
  TGenericosString0_3 : array[0..3] of string = ('0','1','2','3' );
  TGenericosString0_4 : array[0..4] of string = ('0','1','2','3','4' );
  TGenericosString0_5 : array[0..5] of string = ('0','1','2','3','4','5' );
  TGenericosString0_6 : array[0..6] of string = ('0','1','2','3','4','5','6' );

  TGenericosString1   : array[0..0] of string = ('1');
  TGenericosString1_2 : array[0..1] of string = ('1','2' );
  TGenericosString1_3 : array[0..2] of string = ('1','2','3' );
  TGenericosString1_4 : array[0..3] of string = ('1','2','3','4' );
  TGenericosString1_5 : array[0..4] of string = ('1','2','3','4','5' );
  TGenericosString1_6 : array[0..5] of string = ('1','2','3','4','5','6' );
  TGenericosString1_7 : array[0..6] of string = ('1','2','3','4','5','6','7' );
  TGenericosString1_8 : array[0..7] of string = ('1','2','3','4','5','6','7','8' );
  TGenericosString1_9 : array[0..8] of string = ('1','2','3','4','5','6','7','8','9' );

  TGenericosStringA_E : array[0..4] of string = ('A','B','C','D','E');

  TGenericosStringA_F : array[0..5] of string = ('A','B','C','D','E','F');

  TGenericosStringO_N : array[0..1] of string = ('O', 'N');

  TGenericosString01_10 : array[0..9] of string = ('01','02','03','04','05',
                                                   '06','07','08','09','10' );

  TGenericosString01_11 : array[0..10] of string = ('01','02','03','04','05',
                                                    '06','07','08','09','10','11' );

  TGenericosString01_12 : array[0..12] of string = ('01', '02', '03', '04', '05',
                                                    '06', '07', '08', '09', '10',
                                                    '11', '12', '99');

  TNrLeiAnistia : array[0..5] of string = ('LEI66831979',  'LEI86321993',
                                           'LEI88781994',  'LEI105592002',
                                           'LEI107902003', 'LEI112822006');

  TPoderSubteto : array[0..3] of string = ('1', '2', '3', '9');

function LayOuteSocialToServico(const t: TLayOut): String;
begin
   Result := EnumeradoToStr(t,
    ['EnviarLoteEventos', 'ConsultarLoteEventos', 'ConsultarIdentificadoresEventos',
      'DownloadEventos'],
    [LayEnvioLoteEventos, LayConsultaLoteEventos, LayConsultaIdentEventos,
     LayDownloadEventos] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;
begin
   Result := StrToEnumerado(ok, s,
    ['EnviarLoteEventos', 'ConsultarLoteEventos', 'ConsultarIdentificadoresEventos',
      'DownloadEventos'],
    [LayEnvioLoteEventos, LayConsultaLoteEventos, LayConsultaIdentEventos,
    LayDownloadEventos] );
end;

function LayOutToSchema(const t: TLayOut): TeSocialSchema;
begin
  case t of
    LayEnvioLoteEventos:     Result := schEnvioLoteEventos;
    LayConsultaLoteEventos:  Result := schConsultaLoteEventos;
    LayConsultaIdentEventos: Result := schConsultaIdentEventos;
    LayDownloadEventos:      Result := schDownloadEventos;
  else
    Result := schErro;
  end;
end;

function SchemaESocialToStr(const t: TeSocialSchema): String;
begin
  Result := GetEnumName(TypeInfo(TeSocialSchema), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaESocial(const s: String): TeSocialSchema;
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

  CodSchema := GetEnumValue(TypeInfo(TeSocialSchema), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TeSocialSchema válido.',[SchemaStr]));
  end;

  Result := TeSocialSchema( CodSchema );


end;

function TipoEventiToSchemaReinf(const t: TTipoEvento): TeSocialSchema;
begin
   case t of
     teS1000: Result := schevtInfoEmpregador;
     teS1005: Result := schevtTabEstab;
     teS1010: Result := schevtTabRubrica;
     teS1020: Result := schevtTabLotacao;
     teS1030: Result := schevtTabCargo;
     teS1035: Result := schevtTabCarreira;
     teS1040: Result := schevtTabFuncao;
     teS1050: Result := schevtTabHorTur;
     teS1060: Result := schevtTabAmbiente;
     teS1070: Result := schevtTabProcesso;
     teS1080: Result := schevtTabOperPort;
     teS1200: Result := schevtRemun;
     teS1202: Result := schevtRmnRPPS;
     teS1207: Result := schevtBenPrRP;
     teS1210: Result := schevtPgtos;
     teS1250: Result := schevtAqProd;
     teS1260: Result := schevtComProd;
     teS1270: Result := schevtContratAvNP;
     teS1280: Result := schevtInfoComplPer;
     teS1295: Result := schevtTotConting;
     teS1298: Result := schevtReabreEvPer;
     teS1299: Result := schevtFechaEvPer;
     teS1300: Result := schevtContrSindPatr;
     teS2190: Result := schevtAdmPrelim;
     teS2200: Result := schevtAdmissao;
     teS2205: Result := schevtAltCadastral;
     teS2206: Result := schevtAltContratual;
     teS2210: Result := schevtCAT;
     teS2220: Result := schevtMonit;
     teS2221: Result := schevtToxic;
     teS2230: Result := schevtAfastTemp;
     teS2240: Result := schevtExpRisco;
     teS2245: Result := schevtTreiCap;
     teS2250: Result := schevtAvPrevio;
     teS2260: Result := schevtConvInterm;
     teS2298: Result := schevtReintegr;
     teS2299: Result := schevtDeslig;
     teS2300: Result := schevtTSVInicio;
     teS2306: Result := schevtTSVAltContr;
     teS2399: Result := schevtTSVTermino;
     teS2400: Result := schevtCdBenPrRP;
     teS3000: Result := schevtExclusao;
  else
    Result := schErro;
  end;
end;

function TipoEventoToStr(const t: TTipoEvento ): string;
begin
  result := EnumeradoToStr2(t, TTipoEventoString );
end;

function StrToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
begin
  result  := TTipoEvento( StrToEnumerado2(ok , s, TTipoEventoString ) );
end;

function TipoEventoToStrEvento(const t: TTipoEvento ): string;
begin
  result := EnumeradoToStr2(t, TEventoString);
end;

function eSProcEmiToStr(const t: TpProcEmi ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToProcEmi(var ok: boolean; const s: string): TpProcEmi;
begin
  result := TpProcEmi( StrToEnumerado2(ok , s, TGenericosString1_2) );
end;

function eSIndSegmentoToStr(const t: TpIndSegmento ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1 );
end;

function eSStrToIndSegmento(var ok: boolean; const s: string): TpIndSegmento;
begin
  result := TpIndSegmento( StrToEnumerado2(ok , s, TGenericosString1 ) );
end;

function eSTpInscricaoToStr(const t:tpTpInsc ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_5 );
end;

function eSStrToTpInscricao(var ok: boolean; const s: string): tpTpInsc;
begin
  result := tpTpInsc( StrToEnumerado2(ok , s, TGenericosString1_5 ) );
end;

function eStpOpcConsultToStr(const t: tpOpcConsult): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrTotpOpcConsult(var ok: Boolean; const s: string): tpOpcConsult;
begin
  result := tpOpcConsult( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eSTpRegPtToStr(const t: tpRegPt): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_6 );
end;

function eSStrToTpRegPt(var ok: Boolean; const s: string): tpRegPt;
begin
  result := tpRegPt( StrToEnumerado2(ok, s, TGenericosString0_6) );
end;

function eSTpContAprToStr(const t: tpContApr): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2 );
end;

function eSStrToTpContApr(var ok: Boolean; const s: string): tpContApr;
begin
  result := tpContApr( StrToEnumerado2(ok, s, TGenericosString0_2) );
end;

function eSTpContPCDToStr(const t: tpContPCD): string;
begin
  result := EnumeradoToStr2(t, TIndicativoContratacaoPCD );
end;

function eSStrToTpContPCD(var ok: Boolean; const s: string): tpContPCD;
begin
  result := tpContPCD( StrToEnumerado2(ok, s, TIndicativoContratacaoPCD) );
end;

function eSTpInclContrToStr(const t: tpInclContr): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3 );
end;

function eSStrToTpInclContr(var ok: Boolean; const s: string): tpInclContr;
begin
  result := tpInclContr( StrToEnumerado2(ok, s, TGenericosString1_3) );
end;

function eSTpPlanRPToStr(const t: tpPlanRP): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2 );
end;

function eSStrToTpPlanRP(var ok: Boolean; const s: string): tpPlanRP;
begin
  result := tpPlanRP( StrToEnumerado2(ok, s, TGenericosString0_2) );
end;

function eSTpRegTrabToStr(const t: tpTpRegTrab ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToTpRegTrab(var ok: boolean; const s: string): tpTpRegTrab;
begin
  result := tpTpRegTrab( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSTpRegPrevToStr(const t: tpTpRegPrev ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrTotpRegPrev(var ok: boolean; const s: string): tpTpRegPrev;
begin
  result := tpTpRegPrev( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpIndAdmissaoToStr(const t: tpTpIndAdmissao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToTpIndAdmissao(var ok: boolean; const s: string): tpTpIndAdmissao;
begin
  result := tpTpIndAdmissao( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpRegJorToStr(const t: tpTpRegJor ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrToTpRegJor(var ok: boolean; const s: string): tpTpRegJor;
begin
  result := tpTpRegJor( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eSOpcFGTSToStr(const t: tpOpcFGTS ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToOpcFGTS(var ok: boolean; const s: string): tpOpcFGTS;
begin
  result := tpOpcFGTS( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSMtvContratToStr(const t: tpMtvContrat ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToMtvContrat(var ok: boolean; const s: string): tpMtvContrat;
begin
  result := tpMtvContrat( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSIndProvimToStr(const t: tpIndProvim ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToIndProvim(var ok: boolean; const s: string): tpIndProvim;
begin
  result := tpIndProvim( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSTpProvToStr(const t: tpTpProv ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '6', '99' ] );
end;

function eSStrToTpProv(var ok: boolean; const s: string): tpTpProv;
begin
  result := tpTpProv( StrToEnumerado2(ok , s, [ '1', '2', '3', '4', '5', '6', '99' ] ) );
end;

function eSTpIndMatProcToStr(const t: tpIndMatProc): string;
begin
  result := EnumeradoToStr2(t, [ '1', '2', '3', '4', '5', '6', '7', '8', '99']);
end;

function eSStrToTpIndMatProc(var ok: boolean; const s: string): tpIndMatProc;
begin
  result := tpIndMatProc( StrToEnumerado2(ok, s, [ '1', '2', '3', '4', '5', '6', '7', '8', '99']) );
end;

function eSUndSalFixoToStr(const t: tpUndSalFixo ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_7 );
end;

function eSStrToUndSalFixo(var ok: boolean; const s: string): tpUndSalFixo;
begin
  result := tpUndSalFixo( StrToEnumerado2(ok , s, TGenericosString1_7 ) );
end;

function eSTpContrToStr(const t: tpTpContr ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToTpContr(var ok: boolean; const s: string): tpTpContr;
begin
  result := tpTpContr( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpJornadaToStr(const t: tpTpJornada ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '9' ] );
end;

function eSStrToTpJornada(var ok: boolean; const s: string): tpTpJornada;
begin
  result := tpTpJornada( StrToEnumerado2(ok , s, [ '1', '2', '3', '9' ] ) );
end;

function eSTpDiaToStr(const t: tpTpDia ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_8 );
end;

function eSStrToTpDia(var ok: boolean; const s: string): tpTpDia;
begin
  result := tpTpDia( StrToEnumerado2(ok , s, TGenericosString1_8 ) );
end;

function eSTpExameOcupToStr(const t: tpTpExameOcup ): string;
begin
  result := EnumeradoToStr2(t,[ '0', '1', '2', '3', '4', '9' ] );
end;

function eSStrToTpExameOcup(var ok: boolean; const s: string): tpTpExameOcup;
begin
  result := tpTpExameOcup( StrToEnumerado2(ok , s, [ '0', '1', '2', '3', '4', '9' ] ) );
end;

function eSResAsoToStr(const t: tpResAso ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToResAso(var ok: boolean; const s: string): tpResAso;
begin
  result := tpResAso( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSMatBiologToStr(const t: TpMatBiolog ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToMatBiolog(var ok: boolean; const s: string): TpMatBiolog;
begin
  result := TpMatBiolog( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSInterprExmToStr(const t: tpInterprExm ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToInterprExm(var ok: boolean; const s: string): tpInterprExm;
begin
  result := tpInterprExm( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSOrdExameToStr(const t: tpOrdExame ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToOrdExame(var ok: boolean; const s: string): tpOrdExame;
begin
  result := tpOrdExame( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSIndresultToStr(const t: tpIndresult ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrToIndresult(var ok: boolean; const s: string): tpIndresult;
begin
  result := tpIndresult( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eSTpAcidToStr(const t: tpTpAcid ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToTpAcid(var ok: boolean; const s: string): tpTpAcid;
begin
  result := tpTpAcid( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpCatToStr(const t: tpTpCat ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToTpCat(var ok: boolean; const s: string): tpTpCat;
begin
  result := tpTpCat( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpRegistradorToStr(const t: tpTpRegistrador ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_9 );
end;

function eSStrToTpRegistrador(var ok: boolean; const s: string): tpTpRegistrador;
begin
  result := tpTpRegistrador( StrToEnumerado2(ok , s, TGenericosString1_9 ) );
end;

function eSIniciatCATToStr(const t: tpIniciatCAT ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToIniciatCAT(var ok: boolean; const s: string): tpIniciatCAT;
begin
  result := tpIniciatCAT( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSLateralidadeToStr(const t: tpLateralidade ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_3 );
end;

function eSStrToLateralidade(var ok: boolean; const s: string): tpLateralidade;
begin
  result := tpLateralidade( StrToEnumerado2(ok , s, TGenericosString0_3 ) );
end;

function eSIdeOCToStr(const t: tpIdeOC ): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '9']);
end;

function eSStrToIdeOC(var ok: boolean; const s: string): tpIdeOC;
begin
  result := tpIdeOC( StrToEnumerado2(ok, s, ['1', '2', '3', '4', '9'] ) );
end;

function eSTpAvPrevioToStr(const t: tpTpAvPrevio ): string;
begin
  result := EnumeradoToStr2(t, [ '1', '2', '4', '5', '6' ] );
end;

function eSStrToTpAvPrevio(var ok: boolean; const s: string): tpTpAvPrevio;
begin
  result := tpTpAvPrevio( StrToEnumerado2(ok , s, [ '1', '2', '4', '5', '6' ] ) );
end;

function eSNrLeiAnistiaToStr(const t: tpNrLeiAnistia ): string;
begin
  result := EnumeradoToStr2(t, TNrLeiAnistia );
end;

function eSStrToNrLeiAnistia(var ok: boolean; const s: string): tpNrLeiAnistia;
begin
  result := tpNrLeiAnistia( StrToEnumerado2(ok , s, TNrLeiAnistia ) );
end;

function eSTpContribSindToStr(const t: tpTpContribSind ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrToTpContribSind(var ok: boolean; const s: string): tpTpContribSind;
begin
  result := tpTpContribSind( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eSIndSubstPatrOpPortStr(const t: tpIndSubstPatrOpPort ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToIndSubstPatrOpPort(var ok: boolean; const s: string): tpIndSubstPatrOpPort;
begin
  result := tpIndSubstPatrOpPort( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSIdAquisStr(const t: tpIdAquis ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_6 );
end;

function eSStrToIdAquis(var ok: boolean; const s: string): tpIdAquis;
begin
  result := tpIdAquis( StrToEnumerado2(ok , s, TGenericosString1_6 ) );
end;

function eSSimNaoToStr(const t: tpSimNao ): string;
begin
  result := EnumeradoToStr2(t,TSimNaoString );
end;

function eSStrToSimNao(var ok: boolean; const s: string): tpSimNao;
begin
  result := tpSimNao( StrToEnumerado2(ok , s, TSimNaoString ) );
end;

function eStpTpInscAmbTabToStr(const t: tpTpInscAmbTab ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '3' ] );
end;

function eSStrTotpTpInscAmbTab(var ok: boolean; const s: string): tpTpInscAmbTab;
begin
  result := tpTpInscAmbTab( StrToEnumerado2(ok , s,[ '1', '3' ] ));
end;

function eSIndComercStr(const t: tpIndComerc ): string;
begin
  result := EnumeradoToStr2(t,[ '2', '3', '7', '8', '9' ] );
end;

function eSStrToIndComerc(var ok: boolean; const s: string): tpIndComerc;
begin
  result := tpIndComerc( StrToEnumerado2(ok , s,[ '2', '3', '7', '8', '9' ] ));
end;

function eSIndCooperativaToStr(const t:TpIndCoop ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_3 );
end;

function eSStrToIndCooperativa(var ok: boolean; const s: string): TpIndCoop;
begin
  result := TpIndCoop( StrToEnumerado2(ok , s, TGenericosString0_3 ) );
end;

function eSIndConstrutoraToStr(const t:TpIndConstr ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_1 );
end;

function eSStrToIndConstrutora(var ok: boolean; const s: string): TpIndConstr;
begin
  result := TpIndConstr( StrToEnumerado2(ok , s, TGenericosString0_1 ) );
end;

function eSIndDesFolhaToStr(const t:tpIndDesFolha ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_1 );
end;

function eSStrToIndDesFolha(var ok: boolean; const s: string): TpIndDesFolha;
begin
  result := TpIndDesFolha( StrToEnumerado2(ok , s, TGenericosString0_1 ) );
end;

function eSIndOptRegEletronicoToStr(const t:TpIndOptRegEletron ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_1 );
end;

function eSStrToIndOptRegEletronico(var ok: boolean; const s: string): TpIndOptRegEletron;
begin
  result := TpIndOptRegEletron( StrToEnumerado2(ok , s, TGenericosString0_1 ) );
end;

function eSIndOpcCPToStr(const t:TpIndOpcCP ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToIndOpcCP(var ok: boolean; const s: string): TpIndOpcCP;
begin
  result := TpIndOpcCP( StrToEnumerado2(ok , s, TGenericosString1_2 ) );
end;

function eSAliqRatToStr(const t:tpAliqRat ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToAliqRat(var ok: boolean; const s: string): TpAliqRat;
begin
  result := TpAliqRat( StrToEnumerado2(ok , s, TGenericosString1_3 ) );
end;

function eSTpProcessoToStr(const t:tpTpProc ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrToTpProcesso(var ok: boolean; const s: string): tpTpProc;
begin
  result := tpTpProc( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eSSiglaMinToStr(const t:tpSiglaMin ): string;
begin
  result := EnumeradoToStr2(t,TSiglasMinString );
end;

function eSStrToSiglaMin(var ok: boolean; const s: string): TpSiglaMin;
begin
  result := TpSiglaMin( StrToEnumerado2(ok , s,TSiglasMinString ) );
end;

function eSIndAcordoIsencaoMultaToStr(const t:tpIndAcordoIsencaoMulta ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_1 );
end;

function eSStrToIndAcordoIsencaoMulta(var ok: boolean; const s: string): TpIndAcordoIsencaoMulta;
begin
  result := TpIndAcordoIsencaoMulta( StrToEnumerado2(ok , s,TGenericosString0_1 ) );
end;

function eSufToStr(const t:tpuf ): string;
begin
  result := EnumeradoToStr2(t,TUFString );
end;

function eSStrTouf(var ok: boolean; const s: string): Tpuf;
begin
  result := Tpuf( StrToEnumerado2(ok , s,TUFString ) );
end;

function eSIndSituacaoEspecialToStr(const t:tpIndSituacaoEspecial ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_4 );
end;

function eSStrToIndSituacaoEspecial(var ok: boolean; const s: string): TpIndSituacaoEspecial;
begin
  result := TpIndSituacaoEspecial( StrToEnumerado2(ok , s,TGenericosString0_4 ) );
end;

function eSIndProvDescToStr(const t:tpIndProvDesc ): string;
begin
  result := EnumeradoToStr2(t,['P','D','I'] );
end;

function eSStrToIndProvDesc(var ok: boolean; const s: string): TpIndProvDesc;
begin
  result := TpIndProvDesc( StrToEnumerado2(ok , s,['P','D','I'] ));
end;

function eSCodIncCPToStr(const t:tpCodIncCP ): string;
begin
  result := EnumeradoToStr2(t,[ '00', '01', '11', '12', '13', '14', '15', '16', '21', '22', '23', '24', '25', '26', '31',
                                '32', '34', '35', '51', '61', '91', '92', '93', '94', '95', '96', '97', '98'] );
end;

function eSStrToCodIncCP(var ok: boolean; const s: string): tpCodIncCP;
begin
  result := tpCodIncCP( StrToEnumerado2(ok , s,[ '00', '01', '11', '12', '13', '14', '15', '16', '21', '22', '23', '24', '25', '26', '31',
                                                 '32', '34', '35', '51', '61', '91', '92', '93', '94', '95', '96', '97', '98' ] ));
end;

function eSTpLocalToStr(const t: tpTpLocal ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '6', '9' ] );
end;

function eSStrToTpLocal(var ok: boolean; const s: string): tpTpLocal;
begin
  result := tpTpLocal( StrToEnumerado2(ok , s,[ '1', '2', '3', '4', '5', '6', '9' ] ));
end;

function eSMtvCancAvPrevioToStr(const t: tpMtvCancAvPrevio ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '9' ] );
end;

function eSStrToMtvCancAvPrevio(var ok: boolean; const s: string): tpMtvCancAvPrevio;
begin
  result := tpMtvCancAvPrevio( StrToEnumerado2(ok , s,[ '1', '2', '3', '9' ] ));
end;

function eSTpReintToStr(const t: tpTpReint ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '9' ] );
end;

function eSStrToTpReint(var ok: boolean; const s: string): tpTpReint;
begin
  result := tpTpReint( StrToEnumerado2(ok , s,[ '1', '2', '3', '4', '5', '9' ] ));
end;

function eSCodIncIRRFToStr(const t:tpCodIncIRRF ): string;
begin
  result := EnumeradoToStr2(t,[  '00', '01', '09','11', '12', '13', '14', '15', '31', '32', '33', '34',
                                 '35', '41', '42', '43', '44', '46', '47', '51', '52', '53',
                                 '54', '55', '61', '62', '63', '64', '70', '71',
                                 '72', '73', '74', '75', '76', '77', '78', '79', '81', '82',
                                 '83','91', '92', '93', '94', '95' ] );
end;

function eSStrToCodIncIRRF(var ok: boolean; const s: string): tpCodIncIRRF;
begin
  result := tpCodIncIRRF( StrToEnumerado2(ok , s,[  '00', '01', '09','11', '12', '13', '14', '15', '31', '32', '33', '34',
                                                    '35', '41', '42', '43', '44', '46', '47', '51', '52', '53',
                                                    '54', '55', '61', '62', '63', '64', '70', '71',
                                                    '72', '73', '74', '75', '76', '77', '78', '79', '81', '82',
                                                    '83','91', '92', '93', '94', '95' ] ));
end;

function eSCodIncFGTSToStr(const t:tpCodIncFGTS ): string;
begin
  result := EnumeradoToStr2(t,[ '00', '11', '12', '21', '91' ] );
end;

function eSStrToCodIncFGTS(var ok: boolean; const s: string): tpCodIncFGTS;
begin
  result := tpCodIncFGTS( StrToEnumerado2(ok , s,[ '00', '11', '12', '21', '91' ] ));
end;

function eSCodIncSINDToStr(const t:tpCodIncSIND ): string;
begin
  result := EnumeradoToStr2(t,[ '00', '11', '31','91' ] );
end;

function eSStrToCodIncSIND(var ok: boolean; const s: string): tpCodIncSIND;
begin
  result := tpCodIncSIND( StrToEnumerado2(ok , s,[  '00', '11', '31','91'] ));
end;

function eSIndSuspToStr(const t: tpIndSusp): string;
begin
  result := EnumeradoToStr2(t,[ '01', '02', '03','04', '05', '08', '09', '10',
                                '11', '12', '13', '14', '90', '92' ] );
end;

function eSStrToIndSusp(var ok: Boolean; const s: string): tpIndSusp;
begin
  result := tpIndSusp( StrToEnumerado2(ok , s,[ '01', '02', '03', '04', '05', '08', '09', '10',
                                                '11', '12', '13', '14', '90', '92'] ));
end;

function eSExtDecisaoToStr(const t: TpExtDecisao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToExtDecisao(var ok: boolean; const s: string): TpExtDecisao;
begin
  result :=  TpExtDecisao( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSTpInscEstabToStr(const t:tpTpInscEstab ): string;
begin
  result := EnumeradoToStr2(t,['1','3','4','']  );
end;

function eSStrToTpInscEstab(var ok: boolean; const s: string): TpTpInscEstab;
begin
  result := TpTpInscEstab( StrToEnumerado2(ok , s,['1','3','4','']  ));
end;

function eStpInscContratanteToStr(const t:tptpInscContratante ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrTotpInscContratante(var ok: boolean; const s: string): TptpInscContratante;
begin
  result := TptpInscContratante( StrToEnumerado2(ok , s,TGenericosString1_2  ));
end;

function eSTpInscPropToStr(const t:TpTpInscProp ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToTpInscProp(var ok: boolean; const s: string): TpTpInscProp;
begin
  result := TpTpInscProp( StrToEnumerado2(ok , s,TGenericosString1_2  ));
end;

function eSTpIntervaloToStr(const t:tpTpIntervalo ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToTpIntervalo(var ok: boolean; const s: string): TpTpIntervalo;
begin
  result := TpTpIntervalo( StrToEnumerado2(ok , s,TGenericosString1_2  ));
end;

function eSIndSubstPatronalObraToStr(const t:tpIndSubstPatronalObra ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToIndSubstPatronalObra(var ok: boolean; const s: string): TpIndSubstPatronalObra;
begin
  result := TpIndSubstPatronalObra( StrToEnumerado2(ok , s,TGenericosString1_2  ));
end;

function eSIndDecisaoToStr(const t:tpIndDecisao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString01_10  );
end;

function eSStrToIndDecisao(var ok: boolean; const s: string): TpIndDecisao;
begin
  result := TpIndDecisao( StrToEnumerado2(ok , s,TGenericosString01_10  ));
end;

function eSindAutoriaToStr(const t:tpindAutoria ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToindAutoria(var ok: boolean; const s: string): TpindAutoria;
begin
  result := TpindAutoria( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndRetificacaoToStr(const t:tpIndRetificacao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToIndRetificacao(out ok: boolean; const s: string): TpIndRetificacao;
begin
  result := TpIndRetificacao( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndApuracaoToStr(const t:tpIndApuracao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToIndApuracao(var ok: boolean; const s: string): TpIndApuracao;
begin
  result := TpIndApuracao( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSInfoApurGrauRiscoToStr(const t:tpInfoApurGrauRisco ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToInfoApurGrauRisco(var ok: boolean; const s: string): TpInfoApurGrauRisco;
begin
  result := TpInfoApurGrauRisco( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndApurAliqFapToStr(const t:tpIndApurAliqFap ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToIndApurAliqFap(var ok: boolean; const s: string): TpIndApurAliqFap;
begin
  result := TpIndApurAliqFap( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndMVToStr(const t:tpIndMV ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3  );
end;

function eSStrToIndMV(var ok: boolean; const s: string): TpIndMV;
begin
  result := TpIndMV( StrToEnumerado2(ok , s,TGenericosString1_3 ));
end;

function eSIndSimplesToStr(const t:tpIndSimples ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_3  );
end;

function eSStrToIndSimples(var ok: boolean; const s: string): TpIndSimples;
begin
  result := TpIndSimples( StrToEnumerado2(ok , s,TGenericosString0_3 ));
end;

function eSNatAtividadeToStr(const t:tpNatAtividade ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_2  );
end;

function eSStrToNatAtividade(var ok: boolean; const s: string): TpNatAtividade;
begin
  result := TpNatAtividade( StrToEnumerado2(ok , s,TGenericosString0_2 ));
end;

function eSTpTributoToStr(const t:tpTpTributo ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4  );
end;

function eSStrToTpTributo(var ok: boolean; const s: string): TpTpTributo;
begin
  result := TpTpTributo( StrToEnumerado2(ok , s,TGenericosString1_4 ));
end;

function eSGrauExpToStr(const t:tpGrauExp ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4  );
end;

function eSStrToGrauExp(var ok: boolean; const s: string): TpGrauExp;
begin
  result := TpGrauExp( StrToEnumerado2(ok , s,TGenericosString1_4 ));
end;

function eSTpAdmissaoToStr(const t: tpTpAdmissao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_6  );
end;

function eSStrToTpAdmissao(var ok: boolean; const s: string): tpTpAdmissao;
begin
  result := tpTpAdmissao( StrToEnumerado2(ok , s,TGenericosString1_6 ));
end;

function eSTpAcordoToStr(const t:tpTpAcordo ): string;
begin
  result := EnumeradoToStr2(t,[ 'A','B','C','D' ] );
end;

function eSStrToTpAcordo(var ok: boolean; const s: string): TpTpAcordo;
begin
  result := TpTpAcordo( StrToEnumerado2(ok , s,[  'A','B','C','D'] ));
end;

function eSIndNIFToStr(const t:tpIndNIF ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToIndNIF(var ok: boolean; const s: string): TpIndNIF;
begin
  result := TpIndNIF( StrToEnumerado2(ok , s,TGenericosString1_3 ));
end;

function eSIndTpDeducaoToStr(const t:tpIndTpDeducao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrToIndTpDeducao(var ok: boolean; const s: string): TpIndTpDeducao;
begin
  result := TpIndTpDeducao( StrToEnumerado2(ok , s,TGenericosString1_4 ));
end;

function eSTpIsencaoToStr(const t:tpTpIsencao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString01_11 );
end;

function eSStrToTpIsencao(var ok: boolean; const s: string): TpTpIsencao;
begin
  result := TpTpIsencao( StrToEnumerado2(ok , s,TGenericosString01_11 ));
end;

function eSIndPerReferenciaToStr(const t:tpIndPerReferencia ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToIndPerReferencia(var ok: boolean; const s: string): TpIndPerReferencia;
begin
  result := TpIndPerReferencia( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSTpProcRRAToStr(const t:tpTpProcRRA ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToTpProcRRA(var ok: boolean; const s: string): TpTpProcRRA;
begin
  result := TpTpProcRRA( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSTpInscAdvogadoToStr(const t:tpTpInscAdvogado ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToTpInscAdvogado(var ok: boolean; const s: string): TpTpInscAdvogado;
begin
  result := TpTpInscAdvogado( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndOrigemRecursosToStr(const t:tpIndOrigemRecursos ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToIndOrigemRecursos(var ok: boolean; const s: string): TpIndOrigemRecursos;
begin
  result := TpIndOrigemRecursos( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSRelDependenciaToStr(const t:tpRelDependencia ): string;
begin
  result := EnumeradoToStr2(t,['01','02','14','17','18', '29'] );
end;

function eSStrToRelDependencia(var ok: boolean; const s: string): TpRelDependencia;
begin
  result := TpRelDependencia( StrToEnumerado2(ok , s,['01','02','14','17','18', '29'] ));
end;

function eSIndObraToStr(const t:tpIndObra ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_3 );
end;

function eSStrToIndObra(var ok: boolean; const s: string): TpIndObra;
begin
  result := TpIndObra( StrToEnumerado2(ok , s,TGenericosString0_3 ));
end;

function eSIndIncidenciaToStr(const t:tpIndIncidencia ): string;
begin
  result := EnumeradoToStr2(t,[ '1','2','9']  );
end;

function eSStrToIndIncidencia(var ok: boolean; const s: string): TpIndIncidencia;
begin
  result := TpIndIncidencia( StrToEnumerado2(ok , s,[ '1','2','9'] ));
end;

function eSIndAbrangenciaToStr(const t:tpIndAbrangencia ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2  );
end;

function eSStrToIndAbrangencia(var ok: boolean; const s: string): TpIndAbrangencia;
begin
  result := TpIndAbrangencia( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eSIndAquisicaoToStr(const t:tpIndAquisicao ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3  );
end;

function eSStrToIndAquisicao(var ok: boolean; const s: string): TpIndAquisicao;
begin
  result := TpIndAquisicao( StrToEnumerado2(ok , s,TGenericosString1_3 ));
end;

function eSIndComercializacaoToStr(const t:tpIndComercializacao ): string;
begin
  result := EnumeradoToStr2(t,['1','2','3','8','9']  );
end;

function eSStrToIndComercializacao(var ok: boolean; const s: string): TpIndComercializacao;
begin
  result := TpIndComercializacao( StrToEnumerado2(ok , s,['1','2','3','8','9'] ));
end;

function eSCnhToStr(const t: tpCnh): string;
begin
  result := EnumeradoToStr2(t,['A', 'B', 'C', 'D', 'E', 'AB', 'AC', 'AD', 'AE']  );
end;

function eSStrToCnh(var ok: Boolean; const s: string): tpCnh;
begin
  result := tpCnh( StrToEnumerado2(ok , s,['A', 'B', 'C', 'D', 'E', 'AB', 'AC', 'AD', 'AE'] ));
end;

function eSClassTrabEstrangToStr(const t: tpClassTrabEstrang): string;
begin
  result := EnumeradoToStr2(t,TGenericosString01_12  );
end;

function eSStrToClassTrabEstrang(var ok: Boolean; const s: string): tpClassTrabEstrang;
begin
  result := tpClassTrabEstrang( StrToEnumerado2(ok , s,TGenericosString01_12 ));
end;

function eStpDepToStr(const t: tpTpDep): string;
begin
  result := EnumeradoToStr2(t,['01', '02', '03', '04', '06', '07', '09', '10', '11', '12', '99'] );
end;

function eSStrToTpDep(var ok: Boolean; const s: string): tpTpDep;
begin
  result := tpTpDep( StrToEnumerado2(ok , s, ['01', '02', '03', '04', '06', '07', '09', '10', '11', '12', '99'] ));
end;

function eSTpRepasseToStr(const t:tpTpRepasse ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_5 );
end;

function eSStrToTpRepasse(var ok: boolean; const s: string): TpTpRepasse;
begin
  result := TpTpRepasse( StrToEnumerado2(ok , s,TGenericosString1_5 ));
end;

function eSIdeSubtetoToStr(const t: tpIdeSubteto): string;
begin
  result := EnumeradoToStr2(t, TPoderSubteto);
end;

function eSStrToIdeSubteto(var ok: Boolean; const s: string): tpIdeSubteto;
begin
  result := tpIdeSubteto( StrToEnumerado2(ok, s, TPoderSubteto) );
end;

function eSTpPublAlvoToStr(const t: tpTpPublAlvo): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_6);
end;

function eSStrToTpPublAlvo(var ok: Boolean; const s: string): tpTpPublAlvo;
begin
  result := tpTpPublAlvo( StrToEnumerado2(ok , s, TGenericosString1_6 ) );
end;

function eSIndSitPFToStr(const t: tpIndSitPF): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2);
end;

function eSStrToTpIndSitPF(var ok: Boolean; const s: string): tpIndSitPF;
begin
  result := tpIndSitPF( StrToEnumerado2(ok , s, TGenericosString0_2 ));
end;

function eSIndSitPJToStr(const t: tpIndSitPJ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_4);
end;

function eSStrToIndSitPJ(var ok: Boolean; const s: string): tpIndSitPJ;
begin
  result := tpIndSitPJ( StrToEnumerado2(ok , s, TGenericosString0_4 ));
end;

function eSTpRubrToStr(const t: tpTpRubr): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_4);
end;

function eSStrToTpRubr(var ok: Boolean; const s: string): tpTpRubr;
begin
  result := tpTpRubr( StrToEnumerado2(ok , s, TGenericosString1_4 ));
end;

function eStpSitCargoToStr(const t: tpSitCargo): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToSitCargo(var ok: Boolean; const s: string): tpSitCargo;
begin
  result := tpSitCargo( StrToEnumerado2(ok , s, TGenericosString1_3 ));
end;

function eStpSitCarrToStr(const t: tpSitCarr): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToSitCarr(var ok: Boolean; const s: string): tpSitCarr;
begin
  result := tpSitCarr( StrToEnumerado2(ok , s, TGenericosString1_3 ));
end;

function eSAcumCargoToStr(const t: tpAcumCargo): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_4);
end;

function eSStrToAcumCargo(var ok: Boolean; const s: string): tpAcumCargo;
begin
  result := tpAcumCargo( StrToEnumerado2(ok , s, TGenericosString1_4 ));
end;

function eSContagemEspToStr(const t: tpContagemEsp): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_4);
end;

function eSStrToContagemEsp(var ok: Boolean; const s: string): tpContagemEsp;
begin
  result := tpContagemEsp( StrToEnumerado2(ok , s, TGenericosString1_4 ));
end;

function eStpUtilizEPCToStr(const t: tpUtilizEPC): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2);
end;

function eSStrTotpUtilizEPC(var ok: Boolean; const s: string): tpUtilizEPC;
begin
  result := tpUtilizEPC( StrToEnumerado2(ok , s, TGenericosString0_2 ));
end;

function eStpUtilizEPIToStr(const t: tpUtilizEPI): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2);
end;

function eSStrTotpUtilizEPI(var ok: Boolean; const s: string): tpUtilizEPI;
begin
  result := tpUtilizEPI( StrToEnumerado2(ok , s, TGenericosString0_2 ));
end;

function eSLocalAmbToStr(const t: tpLocalAmb): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToLocalAmb(var ok: Boolean; const s: string): tpLocalAmb;
begin
  result := tpLocalAmb( StrToEnumerado2(ok , s, TGenericosString1_3 ));
end;

function eSTpAcConvToStr(const t: tpTpAcConv): string;
begin
  result := EnumeradoToStr2(t, TGenericosStringA_F);
end;

function eSStrToTpAcConv(var ok: Boolean; const s: string): tpTpAcConv;
begin
  result := tpTpAcConv( StrToEnumerado2(ok , s, TGenericosStringA_F ));
end;

function eSTpNatEstagioToStr(const t: tpNatEstagio): string;
begin
  result := EnumeradoToStr2(t, TGenericosStringO_N);
end;

function eSStrToTpNatEstagio(var ok: boolean; const s: string): tpNatEstagio;
begin
  result  := tpNatEstagio( StrToEnumerado2(ok , s, TGenericosStringO_N ) );
end;

function eStpCaepfToStr(const t: tpCaepf): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrTotpCaepf(var ok: boolean; const s: string): tpCaepf;
begin
  result := tpCaepf(StrToEnumerado2(ok, s, TGenericosString1_3));
end;

function eStpTpPgtoToStr(const t: tpTpPgto): string;
begin
  result := EnumeradoToStr2(t,['1', '2', '3', '5', '6', '7', '9'] );
end;

function eSStrTotpTpPgto(var ok:Boolean; const s: string): tpTpPgto;
begin
  result := tpTpPgto(StrToEnumerado2(ok, s, ['1', '2', '3', '5', '6', '7', '9']));
end;

function eStpNivelEstagioToStr(const t: tpNivelEstagio): string;
begin
  result := EnumeradoToStr2(t, ['1', '2', '3', '4', '8', '9']);
end;

function eSStrTotpNivelEstagio(var ok: Boolean; const s: string): tpNivelEstagio;
begin
  result := tpNivelEstagio(StrToEnumerado2(ok, s, ['1', '2', '3', '4', '8', '9']));
end;

function eSTpMtvAltToStr(const t: tpMtvAlt): string;
begin
  result := EnumeradoToStr2(t, TMotivoAlteracaoCargoFuncao);
end;

function eSStrToTpMtvAlt(var ok: boolean; const s: string): tpMtvAlt;
begin
  result := tpMtvAlt(StrToEnumerado2(ok, s, TMotivoAlteracaoCargoFuncao));
end;

function eSTpOrigemAltAfastToStr(const t: tpOrigemAltAfast): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToTpOrigemAltAfast(var ok: boolean; const s: string): tpOrigemAltAfast;
begin
  result := tpOrigemAltAfast(StrToEnumerado2(ok, s, TGenericosString1_3));
end;

function eSTpPensaoAlimToStr(const t: tpPensaoAlim): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_3);
end;

function eSStrToTpPensaoAlim(var ok: boolean; const s: string): tpPensaoAlim;
begin
  result := tpPensaoAlim(StrToEnumerado2(ok, s, TGenericosString0_3));
end;

function eSTpCumprParcialAvisoToStr(const t: tpCumprParcialAviso): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_4);
end;

function eSStrToTpCumprParcialAviso(var ok: boolean; const s: string): tpCumprParcialAviso;
begin
  result := tpCumprParcialAviso(StrToEnumerado2(ok, s, TGenericosString0_4));
end;

function eSModoLancamentoToStr(const t: TModoLancamento): string;
begin
  result := EnumeradoToStr2(t, TModoLancamentoString);
end;

function eSStrToModoLancamento(var ok: boolean; const s: string): TModoLancamento;
begin
  result := TModoLancamento(StrToEnumerado2(ok, s, TModoLancamentoString));
end;

function StrToVersaoeSocial(out ok: Boolean; const s: String): TVersaoeSocial;
begin
  result := StrToEnumerado(ok, s, ['02_04_01', '02_04_02', '02_05_00'],
                          [ve02_04_01, ve02_04_02, ve02_05_00]);
end;

function VersaoeSocialToStr(const t: TVersaoeSocial): String;
begin
  result := EnumeradoToStr(t, ['02_04_01', '02_04_02', '02_05_00'],
                          [ve02_04_01, ve02_04_02, ve02_05_00]);
end;

function DblToVersaoeSocial(out ok: Boolean; const d: Real): TVersaoeSocial;
begin
  ok := True;

  if (d = 2.0401)  then
    result := ve02_04_01
  else
  if (d = 2.0402)  then
    result := ve02_04_02
  else
  if (d = 2.0500)  then
    result := ve02_05_00
  else
  begin
    result := ve02_04_01;
    ok := False;
  end;
end;

function VersaoeSocialToDbl(const t: TVersaoeSocial): Real;
begin
  case t of
    ve02_04_01: result := 2.0401;
    ve02_04_02: result := 2.0402;
    ve02_05_00: result := 2.0500;
  else
    result := 0;
  end;
end;

function tpTmpParcToStr(const t: tpTmpParc ): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3'],
                           [tpNaoeTempoParcial, tpLimitado25HorasSemanais,
                            tpLimitado30HorasSemanais, tpLimitado26HorasSemanais]);
end;

function StrTotpTmpParc(var ok: boolean; const s: string): tpTmpParc;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3'],
                           [tpNaoeTempoParcial, tpLimitado25HorasSemanais,
                            tpLimitado30HorasSemanais, tpLimitado26HorasSemanais]);
end;

function tpClassTribToStr(const t: TpClassTrib ): string;
begin
  result := EnumeradoToStr(t, ['00', '01', '02', '03', '04', '06', '07', '08', '09',
                               '10', '11', '13', '14', '21', '22', '60', '70',
                               '80', '85', '99'],
                              [ct00, ct01, ct02, ct03, ct04, ct06, ct07, ct08, ct09,
                               ct10, ct11, ct13, ct14, ct21, ct22, ct60, ct70,
                               ct80, ct85, ct99]);
end;

function StrTotpClassTrib(var ok: boolean; const s: string): TpClassTrib;
begin
  result := StrToEnumerado(ok, s, ['00', '01', '02', '03', '04', '06', '07', '08', '09',
                                   '10', '11', '13', '14', '21', '22', '60', '70',
                                   '80', '85', '99'],
                              [ct00, ct01, ct02, ct03, ct04, ct06, ct07, ct08, ct09,
                               ct10, ct11, ct13, ct14, ct21, ct22, ct60, ct70,
                               ct80, ct85, ct99]);
end;

function eStpTpAcidTransitoToStr(const t: tpTpAcidTransito ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_4 );
end;

function eSStrTotpTpAcidTransito(var ok: boolean; const s: string): tpTpAcidTransito;
begin
  result := tpTpAcidTransito( StrToEnumerado2(ok , s, TGenericosString1_4 ) );
end;

function eStpMotivosAfastamentoToStr(const t: tpMotivosAfastamento ): string;
begin
  result := EnumeradoToStr2(t, TMotivoAfastamento);
end;

function eSStrTotpMotivosAfastamento(var ok: boolean; const s: string): tpMotivosAfastamento;
begin
   result := tpMotivosAfastamento(StrToEnumerado2(ok , s, TMotivoAfastamento));
end;

function StrEventoToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
const
  EventoString: array[0..52] of String =('evtInfoEmpregador', 'evtTabEstab',
       'evtTabRubrica', 'evtTabLotacao', 'evtTabCargo', 'evtTabCarreira',
       'evtTabFuncao', 'evtTabHorContratual', 'evtTabAmbiente', 'evtTabProcesso',
       'evtTabOperPortuario', 'S-2100', 'evtRemun', 'evtRmnRPPS', 'evtBenPrRP',
       'evtPgtos', 'S-1220', 'evtAqProd', 'evtComProd', 'evtContratAvNP',
       'evtInfoComplPer', 'evtTotConting', 'evtReabreEvPer', 'evtFechaEvPer',
       'evtContrSindPatr', 'evtAdmPrelim', 'evtAdmissao', 'evtAltCadastral',
       'evtAltContratual', 'evtCAT', 'evtASO', 'evtAfastTemp', 'evtExpRisco',
       'evtInsApo', 'evtAvPrevio', 'evtConvInterm', 'evtReintegr', 'evtDeslig',
       'evtTSVInicio', 'S-2305', 'evtTSVAltContr', 'evtTSVTermino', 'evtCdBenPrRP',
       'evtExclusao', 'S-4000', 'S-4999', 'S-5001', 'S-5002', 'S-5003', 'S-5011', 'S-5012', 'S-5013',
       'evtToxic');
begin
  result := TTipoEvento( StrToEnumerado2(ok , s, EventoString ) );
end;

function StringINIToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
var
  i: integer;
begin
  ok := False;
  result := TTipoEvento( 0 );

  try
    for i := 0 to 43 do
      if Pos('[' + TEventoString[i] + ']', s) > 0 then
      begin
        ok := True;
        result := TTipoEvento( i );
        exit;
      end;
  except
    ok := False;
  end;
end;
function StringXMLToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
var
  i: integer;
begin
  ok := False;
  result := TTipoEvento( 0 );

  try
    for i := 0 to 43 do
      if Pos('<' + TEventoString[i]+ ' Id="', s) > 0 then
      begin
        ok := True;
        result := TTipoEvento( i );
        exit;
      end;
  except
    ok := False;
  end;
end;

function tpInfOnusToStr(const t: tpInfOnus ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function StrTotpInfOnus(var ok: boolean; const s: string): tpInfOnus;
begin
  result := tpInfOnus(StrToEnumerado2(ok, s, TGenericosString1_3));
end;

function tpOnusRemunToStr(const t: tpOnusRemun ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function StrTotpOnusRemun(var ok: boolean; const s: string): tpOnusRemun;
begin
  result := tpOnusRemun(StrToEnumerado2(ok, s, TGenericosString1_3));
end;

function tpAvalToStr(const t: tpTpAval): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2 );
end;

function StrTotpAval(var ok: Boolean; const s: string): tpTpAval;
begin
  result := tpTpAval( StrToEnumerado2(ok, s, TGenericosString1_2) );
end;

function tpModTreiCapToStr(const t: tpModTreiCap ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3 );
end;

function StrTotpModTreiCap(var ok: boolean; const s: string): tpModTreiCap;
begin
  result := tpModTreiCap( StrToEnumerado2(ok, s, TGenericosString1_3) );
end;

function tpTpTreiCapToStr(const t: tpTpTreiCap ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_5 );
end;

function StrTotpTpTreiCap(var ok: boolean; const s: string): tpTpTreiCap;
begin
  result := tpTpTreiCap( StrToEnumerado2(ok, s, TGenericosString1_5) );
end;

function tpTpProfToStr(const t: tpTpProf ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2 );
end;

function StrTotpTpProf(var ok: boolean; const s: string): tpTpProf;
begin
  result := tpTpProf( StrToEnumerado2(ok, s, TGenericosString1_2) );
end;

function tpNacProfToStr(const t: tpNacProf ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2 );
end;

function StrTotpNacProf(var ok: boolean; const s: string): tpNacProf;
begin
  result := tpNacProf( StrToEnumerado2(ok, s, TGenericosString1_2) );
end;

end.
