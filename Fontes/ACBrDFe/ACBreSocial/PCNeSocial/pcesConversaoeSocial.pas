{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
{                              Leivio Fontenele                                }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
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
  const
  TModoLancamentoString : array[0..2] of String = ('inclusao', 'alteracao', 'exclusao');
  type

  TeSocialSchema          = (schErro, schConsultaLoteEventos, schEnvioLoteEventos,
                             schevtAdesao, schevtAdmissao, schevtAdmPrelim,
                             schevtAfastTemp, schevtAltCadastral, schevtAltContratual,
                             schevtAqProd, schevtAvPrevio, schevtBasesTrab, schevtCdBenefIn,
                             schevtCadInicial, schevtCAT, schevtBenPrRP,
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
                             schRetornoEvento, schRetornoProcessamentoLote, schConsultaIdentEventos,
                             schDownloadEventos, schEvtToxic, schEvtTreiCap, schevtCdBenefAlt,
                             schevtCdBenIn, schevtCessao, schevtCdBenAlt, schevtReativBen,
                             schevtCdBenTerm, schNaoIdentificado, schevtProcTrab, schevtContProc,
                             schevtInfoIR, schevtExcProcTrab,
                             schevtCdBenPrRP // Especifico para versões do eSocial anterior a vS1.0.0.00
                             );

  TLayOut                 = (LayEnvioLoteEventos, LayConsultaLoteEventos,
                             LayConsultaIdentEventos, LayDownloadEventos);

  TStatusACBreSocial      = (stIdle, stEnvLoteEventos, stConsultaLote,
                             stConsultaIdentEvt, stDownloadEvt);

  TTipoEvento             = (teS1000, teS1005, teS1010, teS1020, teS1030, teS1035, teS1040, teS1050,
                             teS1060, teS1070, teS1080, teS2100, teS1200, teS1202, teS1207, teS1210,
                             teS1220, teS1250, teS1260, teS1270, teS1280, teS1295, teS1298, teS1299,
                             teS1300, teS2190, teS2200, teS2205, teS2206, teS2210, teS2220, teS2230,
                             teS2240, teS2245, teS2250, teS2260, teS2298, teS2299, teS2300, teS2305,
                             teS2306, teS2399, teS2400, teS3000, teS4000, teS4999, teS2231, teS5002,
                             teS5003, teS5011, teS5012, teS5013, teS2221, teS2405, teS2410, teS5001,
                             teS2416, teS2418, teS2420, teS2500, teS2501, teS3500, teS5501, teS5503,
                             teS8299, teConsultaIdentEventos, teConsultaLoteEventos, teDownloadEventos, teEnvioLoteEventos,
                             teErro, teRetornoEnvioLoteEventos, teRetornoEvento, teRetornoProcessamentoLote,
                             teInsApo, teAdesao, teCadInicial, teASO, teNaoIdentificado
                             );
  const
    TTipoEventoString   : array[0..77] of String =('S-1000', 'S-1005', 'S-1010', 'S-1020', 'S-1030',
                                                   'S-1035', 'S-1040', 'S-1050', 'S-1060', 'S-1070',
                                                   'S-1080', 'S-2100', 'S-1200', 'S-1202', 'S-1207',
                                                   'S-1210', 'S-1220', 'S-1250', 'S-1260', 'S-1270',
                                                   'S-1280', 'S-1295', 'S-1298', 'S-1299', 'S-1300',
                                                   'S-2190', 'S-2200', 'S-2205', 'S-2206', 'S-2210',
                                                   'S-2220', 'S-2230', 'S-2240', 'S-2245', 'S-2250',
                                                   'S-2260', 'S-2298', 'S-2299', 'S-2300', 'S-2305',
                                                   'S-2306', 'S-2399', 'S-2400', 'S-3000', 'S-4000',
                                                   'S-4999', 'S-5001', 'S-5002', 'S-5003', 'S-5011',
                                                   'S-5012', 'S-5013', 'S-2221', 'S-2405', 'S-2410',
                                                   'S-2231', 'S-2416', 'S-2418', 'S-2420', 'S-2500',
                                                   'S-2501', 'S-3500', 'S-5501', 'S-5503', 'S-8299',
                                                   'S-CONSULTAIDENTEVENTOS', 'S-CONSULTALOTEEVENTOS',
                                                   'S-DOWNLOADEVENTOS', 'S-ENVIOLOTEEVENTOS',
                                                   'S-ERRO','S-RETORNOENVIOLOTEEVENTOS',
                                                   'S-RETORNOEVENTO', 'S-RETORNOPROCESSAMENTOLOTE',
                                                   'S-EVTINSAPO', 'S-EVTADESAO', 'S-EVTCADINICIAL',
                                                   'S-EVTASO', '');
  type

  tpSimNao                = (tpSim, tpNao);
  const
  TSimNaoString       : array[0..1] of string = ('S','N' );

  type

  TpProcEmi               = (peAplicEmpregador, peAplicGovernamental, peAplicGovWeb, peAplicGovSimplPJ,
                             peAplicGovEvtJud, peAplicGovIntegJuntaCom, peAplicGovDispMoveis);

  tpTpInsc                = (tiCNPJ, tiCPF, tiCAEPF, tiCNO, tiCGC, tiCEI);

  TpTpInscProp            = (tpCNPJ, tpCPF );

  TpIndCoop               = (icNaoecooperativa, icCooperativadeTrabalho, icCooperativadeProducao, icOutrasCooperativas );

  TpIndConstr             = (iconNaoeConstrutora, iconEmpresaConstrutora);

  TpIndDesFolha           = (idfNaoAplicavel, idfEmpresaenquadradanosArt7_9);

  TpIndOptRegEletron      = (iorNaooptou, iorOptoupeloregistro);

  TpIndOpcCP              = (icpComercializacao, icpFolhaPagamento, icpNenhum);

  tpAliqRat               = (arat1, arat2, arat3 );

  tpTpProc                = (tpAdministrativo, tpJudicial, tpINSS, tpFAP);

  tpIndAcordoIsencaoMulta = (iaiSemacordo, iaiComacordo);

  TptpInscContratante     = (icCNPJ, icCPF);

//  tpuf                    = (ufNenhum, ufAC, ufAL, ufAP, ufAM, ufBA, ufCE, ufDF, ufES, ufGO, ufMA,
//                             ufMT, ufMS, ufMG, ufPA, ufPB, ufPR, ufPE, ufPI, ufRJ, ufRN,
//                             ufRS, ufRO, ufRR, ufSC, ufSP, ufSE, ufTO, ufEX);

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
  const
  tpCodIncCPArrayStrings: array[tpCodIncCP] of string = ( '00', '01', '11', '12', '13', '14', '15', '16', '21', '22', '23', '24', '25', '26', '31',
                                                          '32', '34', '35', '51', '61', '91', '92', '93', '94', '95', '96', '97', '98');
  type

  tpCodIncIRRF            = (ciiNaoeBasedeCalculo,                                               {0}    { Item válido até a versão 2.5 }
                             ciiNaoeBasedeCalculoAcordoInternacional,                            {1}    { Item válido até a versão 2.5 }
                             ciiOutrasVerbasNaoConsideradas,                                     {9}
                             ciiBasedeCalculoIRRFRemMensal,                                      {11}
                             ciiBasedeCalculoIRRF13oSalario,                                     {12}
                             ciiBasedeCalculoIRRFFerias,                                         {13}
                             ciiBasedeCalculoIRRFPLR,                                            {14}
                             ciiBasedeCalculoIRRFRRA,                                            {15}    { Item válido até a versão 2.5 }
                             ciiValorIRRFRemMensal,                                              {31}
                             ciiValorIRRF13oSalario,                                             {32}
                             ciiValorIRRFFerias,                                                 {33}
                             ciiValorIRRFPLR,                                                    {34}
                             ciiValorIRRFPLRRRA,                                                 {35}    { Item válido até a versão 2.5 }
                             ciPrevSocialOficialRemMensal,                                       {41}
                             ciPrevSocialOficial13Salario,                                       {42}
                             ciPrevSocialOficialFerias,                                          {43}
                             ciPrevSocialOficialRRA,                                             {44}    { Item válido até a versão 2.5 }
                             ciPrevPrivadaSalarioMensal,                                         {46}
                             ciPrevPrivada13Salario,                                             {47}
                             ciPrevPrivadaFerias,                                                {48}    { Item válido a partir da versão simplificada }
                             ciPensaoAlimenticiaRemMensal,                                       {51}
                             ciPensaoAlimenticia13Salario,                                       {52}
                             ciPensaoAlimenticiaFerias,                                          {53}
                             ciPensaoAlimenticiaPLR,                                             {54}
                             ciPensaoAlimenticiaRRA,                                             {55}    { Item válido até a versão 2.5 }
                             ciFundoAposentadoriaProgIndFAPIRemMensal,                           {61}
                             ciFundoAposentadoriaProgIndFAPI13Salario,                           {62}
                             ciFundacaoPrevCompServidorPubFederalFunprespRemMensal,              {63}
                             ciFundacaoPrevCompServidorPubFederalFunpresp13Salario,              {64}
                             ciFundacaoPrevCompServidorPublicoFerias,                            {65}    { Item válido a partir da versão simplificada }
                             ciFundoDeAposentadoriaProgramadaIndividualFAPIFerias,               {66}    { Item válido a partir da versão simplificada }
                             ciPlanoPrivadoColetivoAssistenciaSaude,                             {67}    { Item válido a partir da versão simplificada }
                             ciParcelaIsenta65AnosRemMensal,                                     {70}
                             ciParcelaIsenta65Anos13Salario,                                     {71}
                             ciDiaria,                                                           {72}
                             ciAjudaDeCusto,                                                     {73}
                             ciIndenizacaoRescisaoContratoInclusiveAtituloPDV,                   {74}
                             ciAbonoPecuniario,                                                  {75}
                             ciPensaoAposOureformaPorMolestiaGraveOuAcidServicoRemMensal,        {76}
                             ciPensaoAposOureformaPorMolestiaGraveOuAcidServico13Salario,        {77}
                             ciValPagosATitOuSocioMicroEmpOuEmpPeqPorteExcProLaboreEAlugueis,    {78}    { Item válido até a versão 2.5 }
                             ciOutros,                                                           {79}
                             ciDepositoJudicial,                                                 {81}    { Item válido até a versão 2.5 }
                             ciCompJudicialAnoCalendario,                                        {82}    { Item válido até a versão 2.5 }
                             ciCompJudicialAnosAnteriores,                                       {83}    { Item válido até a versão 2.5 }
                             ciiIncidenciasuspensajudicialBCIRRF,                                {91}    { Item válido até a versão 2.5 }
                             ciiIncidenciasuspensajudicialBCIRRF13Salario,                       {92}    { Item válido até a versão 2.5 }
                             ciiIncidenciasuspensajudicialBCIRRFFerias,                          {93}    { Item válido até a versão 2.5 }
                             ciiIncidenciasuspensajudicialBCIRRFPLR,                             {94}    { Item válido até a versão 2.5 }
                             ciiIncidenciasuspensajudicialBCIRRFRRA,                             {95}    { Item válido até a versão 2.5 }
                             ciiRendimentoIsentoAuxilioMoradia,                                  {700}   { Item válido a partir da versão simplificada }
                             ciiRendimentoParteNaoTributavelDoValorDeServicoDeTransporte,        {701}   { Item válido a partir da versão simplificada }
                             ciiRendimentoTributavelExigibilidadeSuspensaRemuneracaoMensal,      {9011}  { Item válido a partir da versão simplificada }
                             ciiRendimentoTributavelExigibilidadeSuspensaDecimoTerceiro,         {9012}  { Item válido a partir da versão simplificada }
                             ciiRendimentoTributavelExigibilidadeSuspensaFerias,                 {9013}  { Item válido a partir da versão simplificada }
                             ciiRendimentoTributavelExigibilidadeSuspensaPLR,                    {9014}  { Item válido a partir da versão simplificada }
                             ciiExigSuspensaRetencaoIRRFRemuneracaoMensal,                       {9031}  { Item válido a partir da versão simplificada }
                             ciiExigSuspensaRetencaoIRRFDecimoTerceiro,                          {9032}  { Item válido a partir da versão simplificada }
                             ciiExigSuspensaRetencaoIRRFFerias,                                  {9033}  { Item válido a partir da versão simplificada }
                             ciiExigSuspensaRetencaoIRRFPLR,                                     {9034}  { Item válido a partir da versão simplificada }
                             ciiExigSuspRetencaoIRRFDepositoJudicialMensal,                      {9831}  { Item válido a partir da versão simplificada }
                             ciiExigSuspRetencaoIRRFDepositoJudicialDecimoTerceiro,              {9832}  { Item válido a partir da versão simplificada }
                             ciiExigSuspRetencaoIRRFDepositoJudicialFerias,                      {9833}  { Item válido a partir da versão simplificada }
                             ciiExigSuspRetencaoIRRFDepositoJudicialPLR,                         {9834}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPSORemuneracaoMensal,                     {9041}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPSODecimoTerceiro,                        {9042}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPSOFerias,                                {9043}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPrevPrivadaSalarioMensal,                 {9046}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPrevPrivadaDecimoTerceiro,                {9047}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPrevPrivadaFerias,                        {9048}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPensaoRemuneracaoMensal,                  {9051}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPensaoDecimoTerceiro,                     {9052}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPensaoFerias,                             {9053}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPensaoPLR,                                {9054}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoAposProgIndFAPIRemuneracaoMensal,    {9061}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoAposProgIndFAPIDecimoTerceiro,       {9062}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoPrevComplServPublRemuneracaoMensal,  {9063}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoPrevComplServPublDecimoTerceiro,     {9064}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoPrevComplServPublFerias,             {9065}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFFundoAposProgIndFAPIFerias,               {9066}  { Item válido a partir da versão simplificada }
                             ciiExigSuspDeducaoBaseIRRFPlanoPrivadoColetivoDeAssistenciaASaude,  {9067}  { Item válido a partir da versão simplificada }
                             ciiCompensacaoJudicialAnoCalendario,                                {9082}  { Item válido a partir da versão simplificada }
                             ciiCompensacaoJudicialAnosAnteriores);                              {9083}  { Item válido a partir da versão simplificada }
  const
  tpCodIncIRRFArrayStrings: array[tpCodIncIRRF] of string = ( '00',   '01',   '09',   '11',   '12',   '13',   '14',   '15',   '31',   '32',   '33',
                                                              '34',   '35',   '41',   '42',   '43',   '44',   '46',   '47',   '48',   '51',   '52',
                                                              '53',   '54',   '55',   '61',   '62',   '63',   '64',   '65',   '66',   '67',   '70',
                                                              '71',   '72',   '73',   '74',   '75',   '76',   '77',   '78',   '79',   '81',   '82',
                                                              '83',   '91',   '92',   '93',   '94',   '95',  '700',  '701', '9011', '9012', '9013',
                                                              '9014', '9031', '9032', '9033', '9034', '9831', '9832', '9833', '9834', '9041', '9042',
                                                              '9043', '9046', '9047', '9048', '9051', '9052', '9053', '9054', '9061', '9062', '9063',
                                                               '9064', '9065', '9066', '9067', '9082', '9083'  );
  type

  tpCodIncCPRP            = (cicpNaoeBasedeCalculodeContribuicoesDevidasaoRPPSRegimeMilitar,
                             cicpBasedeCalculodeContribuicoesDevidasaoRPPSRegimeMilitar,
                             cicpBasedeCalculodeContribuicoesDevidasaoRPPSRegimeMilitarDecimo,
                             cicpContribuicaoDescontadadoSeguradoeBeneficiario,
                             cicpContribuicaoDescontadadoSeguradoeBeneficiarioDecimo,
                             cicpSuspensaodeIncidenciaemDecorrenciadeDecisaoJudicial,
                             cicpSuspensaodeIncidenciaemDecorrenciadeDecisaoJudicialDecimo,
                             cicpNenhum);
  const
  tpCodIncCPRPArrayStrings: array[tpCodIncCPRP] of string = ('00', '11', '12', '31', '32', '91', '92', '99');

  type

  tpCodIncFGTS            = (cdfNaoeBasedeCalculo, cdfBasedeCalculoFGTS, cdfBasedeCalculoFGTS13Sal, cdfBasedeCalculoFGTSRescisorio, cdfIncidenciadecisaoJudicialFGTSMensal, cdfIncidenciadecisaoJudicialFGTS13Sal, cdfIncidenciadecisaoJudicial);
  const
  tpCodIncFGTSArrayStrings: array[tpCodIncFGTS] of string = ( '00', '11', '12', '21', '91', '92', '93' );

  type


  tpExtDecisao            = (edContribPatronais, edContribPatronaisSegurados );

  tpIndSubstPatronalObra  = (ispVazio, ispPatronalSubstituida, ispPatronalNaoSubstituida);

  tpindAutoria            = (iaProprioContribuinte, iaOutraEntidade);

  tpIndMatProc            = (impTributaria, impAutorizacaoTrabalhadorMenor,
                             impDispensaPCD, impDispensaAprendiz,
                             impSegurancaoeSaudeTrabalhador,
                             impConversaoLicencaSaudeAcidenteTrabalho,
                             impFGTS, impContribuicaoSindical, impOutros);

  tpIndRetificacao        = (ireOriginal, ireRetificacao);

  tpIndApuracao           = (iapuMensal, ipaAnual);

  tpIndMV                 = (imvDescontadaempregador, imvDescontadaoutras, imvSobrelimite);

  tpIndSimples            = (idsNenhum, idsIntegralmente, idsNaosubstituida, idsConcomitante);

  tpNatAtividade          = (navNaoInformar, navUrbano, navRural);

  tpTpTributo             = (tptIRRF, tptPrevidenciaria, tptFGTS, tptContribSind);

  tpGrauExp               = (ge1, ge2, ge3, ge4);

  tpIndNIF                = (infBeneficiaNIF, infDispensadoNIF, infPaisnaoNIF);

  tpTpProcRRA             = (tppAdministrativo, tppJudicial);

  tpTpProcRet             = (tpprAdministrativo, tpprJudicial);

  tpTpInscAdvogado        = (tadJuridica, tadFisica );

  tpIndIncidencia         = (indNormal, indAtivConcomitante, indSubstituida );

  tpIndAbrangencia        = (iarTodos, iarEstabelecimento );

  tpIndComercializacao    = (idcProdRuralAgroindustria, idcProdRuralSegEspecial, idcProdRuralSegEspecialVendas,
                             idcEntidadeinscrita, idcMercadoExterno );

  tpTpRubr                = (tpVencimento, tpDesconto, tpInformativa, tpInformativaDedutora);

  tpAcumCargo             = (acNaoAcumulavel, acProfSaude, acProfessor, acTecCientifico);

  tpContagemEsp           = (ceNao, ceProfInfantilFundamentalMedio, ceProfEnsSuperiorMagistradoMembroMinisPublicoTribContas, ceAtividadedeRisco);

  tpUtilizEPC             = (uEPCNaoAplica, uEPCNaoImplementa, uEPCImplementa);

  tpUtilizEPI             = (uEPINaoAplica, uEPINaoUtilizado, uEPIUtilizado);

  tpLocalAmb              = (laEstabProprioEmpregador, laEstabTerceiro, laPrestInstalacaoTerceiro);

  tpTpAcConv              = (tacAcordoColTrab, tacLegislacaoFederalEstadualMunicipalDistrital, tacConvencaoColTrab,
                             tacSetencNormativa, tacConversaoLicenSaudeAcidTrabalho, tacOutrasVerbas, tacAntecipacaoDif);

  tpIndSusp               = (siLiminarMandadoSeguranca, siDepositoJudicial, siDepositoAdministrativo, siAntecipacaoTutela, siLiminarMedidaCautelar,
                             siSentencaMandadoSegurancaFavoravelContribuinte, siSentencaAcaoOrdinariaFavContribuinteConfirmadaPeloTRF,
                             siAcordaoTRFFavoravelContribuinte, siAcordaoSTJRecursoEspecialFavoravelContribuinte,
                             siAcordaoSTFRecursoExtraordinarioFavoravelContribuinte, siSentenca1instanciaNaoTransitadaJulgadoEfeitoSusp,
                             siContestacaoAdministrativaFAP, siDecisaoDefinitivaAFavorDoContribuinte,
                             siSemSuspensaoDaExigibilidade);

  tpTpInscAmbTab          = (atCNPJ, atCAEPF);
  const
  tpTpInscAmbTabArrayStrings:array[tpTpInscAmbTab] of String = ( '1', '3' );
  type

  tpCnh                   = (cnA, cnB, cnC, cnD, cnE, cnAB, cnAC, cnAD, cnAE );

  tpClassTrabEstrang      = (ctVistoPermanente, ctVistoTemporario, ctAsilado, ctRefugiado, ctSolicitanteRefugio,
                             ctResidentePaisFrontBrasil, ctDefFisicoMais51Anos, ctComResidenciaProvAnistiadoSituacaoIrregular,
                             ctPermanenciaBrasilRazaoFilhosOuConjugeBras, ctBeneficiadoAcordoPaisesMercosul,
                             ctDependenteAgenteDiplomaticoOuConsular, ctBeneficiadoTratadoAmizade,
                             ctVazio);

  tpTpDep                 = (tdConjuge,
                             tdCompanheiroComFilhoOuVivaMais5Anos,
                             tdFilhoOuEnteado,
                             tdFilhoOuEnteadoUniverOuEscolaTec,
                             tdIrmaoNetoBisnetoGuardaJudicial,
                             tdIrmaoNetoBisnetoUniverOuEscolaTecGuardaJudicial,
                             tdPaisAvosBisavos,
                             tdMenorPobreGuardaJudicial,
                             tdPessoaIncapazTutorOuCurador,
                             tdExConjuge,
                             tdAgregadoOutros,
                             tdNenhum);

  tpRelDep                = (relNetoBisneto,
                             relIrmao,
                             relConjugeCompanheiro,
                             relFilho,
                             relPaisAvosEBisavos,
                             relEnteado,
                             relSogro,
                             relAgregadoOutros);

  tpTpRegTrab             = (trCLT, trEstatutario);

  tpTpRegPrev             = (rpRGPS, rpRPPS, rpRPPE);

  tpTpRegPrevFacultativo  = (rpfNenhum, rpfRGPS, rpfRPPS, rpfRPPE2);

  tpTpAdmissao            = (taAdmissao,
                             taTransfEmpresaMesmoGrupoEconomico,
                             taTransfEmpresaConsorciadaOuDeConsorcio,
                             taTransfPorMotivoSucessaoIncorporacaoCisaoOuFuso,
                             taTransfEmpregadoDomesticoParaOutroRepresentanteMesmaFamiliar,
                             taMudancaDeCPF,
                             taTransfEmpresaConsideradaInapta);

  tpTpIndAdmissao         = (iaNormal, iaDecorrenteAcaoFiscal, iaDecorrenteDecisaoJudicial);

  tpTpRegJor              = (rjSubmetidosHorarioTrabalho, rjAtividadeExtEspecificadaIncisoIArt62CLT,
                             rjFuncoesEspecificadasIncisoIIArt62CLT, rjTeletrabalhoPrevistoIncisoIIIArt62CLT);

  tpOpcFGTS               = (ofOptante, ofNaoOptante);

  tpMtvContrat            = (mcNecessidadeTransitoriaSubstituicaoPessoalRegular, mcAcrescimoExtraordinarioServicos);

  tpIndProvim             = (ipNormal, ipDecorrenteDecisaoJudicial);

  tpTpProv                = (tpNomeacaoCargoEfetivo,
                             tpNomeacaoExcluvivamenteCargoComissao,
                             tpIncorporacaoMatriculaOuNomeacao,
                             tpMatricula,                                              { Item válido até a versão 2.5 }
                             tpRedistribuicao,
                             tpDiplomacao,
                             tpContratacaoPorTempoDeterminado,                         { Item válido a partir da versão simplificada }
                             tpRemocao,                                                { Item válido a partir da versão simplificada }
                             tpDesignacao,                                             { Item válido a partir da versão simplificada }
                             tpMudancaDeCpf,                                           { Item válido a partir da versão simplificada }
                             tpEstabilizados,                                          { Item válido a partir da versão simplificada }
                             tpOutros);

  tpUndSalFixo            = (sfPorHora, sfPorDia, sfPorSemana, sfPorQuinzena, sfPorMes, sfPorTarefa, sfNaoaplicavel);

  tpTpContr               = (PrazoIndeterminado, PrazoDeterminado, PrazoDeterminadoVincOcDeUmFato, PrazoNaoAplicavel);

  tpTpContrS2500          = (TrabalhadorComVinculoFormalizadoSemAlteracaoNasDatasDeAdmissaoEDeDesligamento,
                             TrabalhadorComVinculoFormalizadoComAlteracaoNaDataDeAdmissao,
                             TrabalhadorComVinculoFormalizadoComInclusaoOuAlteracaoDeDataDeDesligamento,
                             TrabalhadorComVinculoFormalizadoComAlteracaoNasDatasDeAdmissaoEDeDesligamento,
                             EmpregadoComReconhecimentoDeVinculo,
                             TrabalhadorSemVinculoDeEmpregoOuEstatutarioSemReconhecimentoDeVinculoEmpregaticio,
                             TrabalhadorComVinculoDeEmpregoFormalizadoEmPeriodoAnteriorAoeSocial, { Item válido a partir da versão 1.2 }
                             ResponsabilidadeIndireta,                                            { Item válido a partir da versão 1.2 }
                             TrabalhadorCujosContratosForamUnificados);                           { Item válido a partir da versão 1.2 }

  tpTpJornada             = (tjJornadaComHorarioDiarioFolgaFixos,                      { Item válido até a versão 2.5 }
                             tjJornada12x36,
                             tjJornadaComHorarioDiarioFixoFolgaVariavel,
                             tjJornadaComHorarioDiarioFixoEFolgaFixaDomingo,           { Item válido a partir da versão simplificada }
                             tjJornadaComHorarioDiarioFixoEFolgaFixaExcetoDomingo,     { Item válido a partir da versão simplificada }
                             tjJornadaComHorarioDiarioFixoEFolhaFixaOutroDiaDaSemana,  { Item válido a partir da versão simplificada }
                             tjTurnoIninterruptoDeRevezamento,                         { Item válido a partir da versão simplificada }
                             tjDemaisTiposJornada);

  tpTpDia                 = (diSegundaFeira, diTercaFeira, diQuartaFeira, diQuintaFeira, diSextaFeira, diSabado, diDomingo, diDiaVariavel);

  tpTpExameOcup           = (taAdmissional, taPeriodico, taRetornoAoTrabalho, taMudancaDeFuncao, taMonitoracaoPontual, taDemissional);

  tpResAso                = (raApto, raInapto, raNaoInformado);

  tpOrdExame              = (orReferencial, oeSequencial, orNaoInformado);

  tpIndResult             = (irNormal, irAlterado, irEstavel, irAgravamento, irNaoInformado);

  tpTpAcid                = (taTipico, taDoenca, taTrajetoParaLocalTrabalhoOuEntreLocalTrabalhoEResidEmp);

  tpTpCat                 = (tcInicial, tcReabertura, tcComunicacaoObito);

  tpIniciatCAT            = (icIniciativaEmpregador, icOrdemJudicial, icDeterminacaoOrgaoFiscalizador);

  tpTpLocal               = (tlEstabEmpregadorBrasil, tlEstabEmpregadorExterior, tlEmpresaOndeEmpregadorPrestaServico, tlViaPublica, tlAreaRural, tlEmbarcacao, tlOutros);

  tpLateralidade          = (laNaoAplicavel, laEsquerda, laDireita, laAmbas);

  tpIdeOC                 = (idNenhum, idCRM, idCRO, idRMS, idCREA, idOutros);
const
  TtpIdeOCArrayStrings: array[tpIdeOC] of string = ('0', '1', '2', '3', '4', '9');

type

  tpTpReint               = (trReintegracaoDecisaoJudicial, trReintegracaoAnistiaLegal, trReversaoServidorPublico, trReconducaoServidorPublico,
                             trReinclusoMilitar, trOutros);

  tpIndSubstPatr          = (spVazio, spIntegralmenteSubstituida, spParcialmenteSubstituida);

  tpIdAquis               = (iaAquiProducaoProdutorRuralPessoaFisSegEspGeral, iaAquiProducaoProdutorRuralPessoaFisSegEspGeralEntPAA,
                             iaAquiProducaoProdutorRuralPessoaJurEntPAA, iaAquiProducaoProdutorRuralPessoaFisSegEspGeralProdIsenta,
                             iaAquiProducaoProdutorRuralPessoaFisSegEspGeralEntPAAProdIsenta, iaAquiProducaoProdutorRuralPessoaJurEntPAAProdIsenta);

  tpIndComerc             = (icComProdPorProdRuralPFInclusiveSegEspEfetuadaDirVarejoConsFinal, icComProdPorProdRuralPFSegEspVendasPJOuIntermPF,
                             icComProdPorProdIsenta, icComProdPorPFSegEspEntProgAquiAliPAA, icComProdMercadoExterno);

  tpTpPgto                = (tpPgtoRemun1200, tpPgtoResc2299, tpPgtoResc2399, tpPgtoRemun1202, tpPgtoBenefPrev1207);

  tpMotivosAfastamento    = (mtvAcidenteDoencaTrabalho, mtvAcidenteDoencaNaoTrabalho, mtvAfastLicencaRegimeProprioSemRemuneracao,
                             mtvAposentadoriaInvalidez, mtvAcompanhamentoFamiliaEnfermo, mtvAfastamentoEmpregadoConCuradorFGTS,
                             mtvAfastLicencaRegimeProprioComRemuneracao, mtvCarcere, mtvCargoEletivoCeletistasGeral,
                             mtvCargoEletivoServidorPublico, mtvCessaoRequisicao, mtvGozoFeriasRecesso, mtvLicencaRemunerada,
                             mtvLicencaMaternidade120Dias, mtvLicencaMaternidadeEmpresaCidada, mtvLicencaMaternidadeAbortoNaoCriminoso,
                             mtvLicencaMaternidadeAdocaoGuardaJudicial, mtvLicencaNaoRemunerada, mtvMandatoEleitoralSemRemuneracao,
                             mtvMandatoEleitoralComRemuneracao, mtvMandatoSindical, mtvMulherVitimaViolencia, mtvParticipacaoCNPS,
                             mtvQualificacao, mtvRepresentanteSindical, mtvServicoMilitar, mtvSuspensaoDisciplinar, mtvServidorPublicoDisponibilidade,
                             mtvLicencaMaternidade180Dias, mtvInatividadetrabalhadorAvulso90Dias, mtvLicencaMaternidadeAntecipacaoProrrogacao,
                             mtvSuspensaoTemporariaContratoTrabalhoNosTermosMP936_2020, mtvImpedimentoConcorrenciaEscalaTrabalhoAvulso);

  tpTpAcidTransito        = (tpatAtropelamento, tpatColisao, tpatOutros, tpatNao);

  tpInfOnus               = (ocCedente, ocCessionario, ocCedenteCessionario);

  tpOnusRemun             = (orEmpregador, orSindicato, orEmpregadorSindicato);

  tpNatEstagio            = (neObrigatiorio, neNaoObrigatorio);

  tpNivelEstagio          = (nvFundamental, nvMedio, nvEnsinoProfis, nvSuperior, nvEspecial, nvMaeSocial);

  tpCaepf                 = (tcVazio, tcContrIndividual, tcProdRural, tcSegEspecial);//layout 2.1

  tpPlanRP                = (prpNenhum = -1,
                             prpSemSegregacaoDaMassa,
                             prpFundoEmCapitalizacao,
                             prpFundoEmReparticao,
                             prpMantidoPeloTesouro);

  tpMtvAlt                = (maPromocao, maReadaptacao, maAproveitamento, maOutros);

  tpOrigemAltAfast        = (oaaPorIniciativaDoEmpregador, oaaRevisaoAdministrativa, oaaDeterminacaoJudicial);

  tpPensaoAlim            = (paNaoExistePensaoAlimenticia,
                             paPercentualDePensaoAlimenticia,
                             paValorDePensaoAlimenticia,
                             paPercentualeValordePensaoAlimenticia,
                             paNenhum);

  const
  tpPensaoAlimArrayStrings: array[tpPensaoAlim] of string = ('0', '1', '2', '3', '');
  type

  tpCumprParcialAviso     = (cpaCumprimentoTotal,
                             cpaCumprimentoParcialNovoEmprego,
                             cpaCumprimentoParcialEmpregador,
                             cpaOutrasCumprimentoParcial,
                             cpaAvisoprevioIndenizadoNaoExigivel);

  tpTpAval                = (tpaQuantitativo, tpaQualitativo);

  tpModTreiCap            = (mtcPresencial, mtcEaD, mtcSemipresencial);

  tpTpTreiCap             = (ttcInicial, ttcPeriodico, ttcReciclagem, ttcEventual, ttcOutros);

  tpTpProf                = (ttpProfissionalEmpregado, ttpProfissionalSemVinculo);

  tpNacProf               = (ttpProfissionalBrasileiro, ttpProfissionalEstrangeiro);

  TVersaoeSocial          = (ve02_04_01, ve02_04_02, ve02_05_00, veS01_00_00, veS01_01_00, veS01_02_00);
const
  TVersaoeSocialArrayStrings : array[TVersaoeSocial] of string = ('02_04_01', '02_04_02', '02_05_00', 'S01_00_00', 'S01_01_00', 'S01_02_00');
  TVersaoeSocialSchemasArrayStrings : array[TVersaoeSocial] of string = ('v02_04_01', 'v02_04_02', 'v02_05_00', 'v_S_01_00_00', 'v_S_01_01_00', 'v_S_01_02_00');
  TVersaoeSocialArrayReals : array[TVersaoeSocial] of Real = (2.0401, 2.0402, 2.0500, 10.0000, 10.1000, 10.2000);

type

  tpTmpParc               = (tpNaoeTempoParcial,
                             tpLimitado25HorasSemanais,
                             tpLimitado30HorasSemanais,
                             tpLimitado26HorasSemanais);

  // ct00 não consta no manual mas consta no manual do desenvolvedor pg 85, é usado para zerar a base de teste.
  tpClassTrib             = (ct00, ct01, ct02, ct03, ct04, ct06, ct07, ct08, ct09, ct10, ct11,
                             ct13, ct14, ct21, ct22, ct60, ct70, ct80, ct85, ct99);

  tpConsulta              = (tcEmpregador, tcTabela, tcTrabalhador);

  tpSimNaoFacultativo     = (snfNada, snfSim, snfNao);
  tpSimFacultativo        = (sfNada, sfSim);

  const
  TSimNaoFacultativoString  : array[0..2] of string = ('', 'S','N');
  TSimFacultativoString     : array[0..1] of string = ('', 'S');

  type

  tpTmpResid              = (ttrNenhum, ttrPrazoIndeterminado, ttrPrazoDeterminado);

  tpCondIng               = (tciNenhum, tciRefugiado, tciSolicitanteDeRefugio, tciPermanenciaNoBrasilReuniaoFamiliar,
                             tciBeneficiadopeloAcordoEntrePaisesDoMercosul, tciDependenteDeAgenteDiplomatico,
                             tciBeneficiadoPeloTratadoDeAmizade, tciOutraCondicao);

  tpIndApurIR             = (tiaiNenhum, tiaiNormal, tiaiSituacaoEspecialIRRF);

  tpIndSitBenef           = (tpisbNenhum,
                             tpisbBeneficioConcedidoPeloProprioOrgaoDeclarante,
                             tpisbBeneficioTransferidoDeOutroOrgao,
                             tpisbMudandaDeCPFDoBeneficiario);

  tpTpPenMorte            = (pmNada, pmVitalicia, pmTemporaria);
  const
  tpTpPenMorteArrayStrings:array[tpTpPenMorte] of string = ('', '1', '2');
  type

  tpMotCessBenef          = (tmcbNenhum,
                             tmcbObito,
                             tmcbReversao,
                             tmcbPorDecisaoJudicial,
                             tmcbCassacao,
                             tmcbTerminoDoPrazoDoBeneficio,
                             tmcbExtincaoDeQuota,
                             tmcbNaoHomologadoPeloTribunalDeContas,
                             tmcbRenunciaExpressa,
                             tmcbTransferenciaDeOrgaoAdministrador,
                             tmcbMudancaDeCPFDoBeneficiario,
                             tmcbNaoRecadastramento);
  const
  tpMotCessBenefArrayStrings:array[tpMotCessBenef] of string = (''  ,'01','02','03','04','05',
                                                                '06','07','08','09','10','11');
  type

  tpMtvTermino            = tpMotCessBenef;

  tpMtvSuspensao          = (mtvNada,
                             mtvSuspensaoPorNaoRecadastramento,
                             mtvOutrosMotivosDeSuspensao);

  tpPercTransf            = (PTr20, PTr40, PTr60, PTr80, PTr100);

  tpIndRemun              = (ireNaoInformado, ireQuarentena, ireDesligamentoReconhecidoJudicialmente, ireAposentadoriaServidor);

  tpTpCCP                 = (CCPAmbitoEmpresa, CCPAmbitoSindicato, Ninter);

  tpMtvDesligTSV          = (mdtNaoInformado,
                             mdtExoneracaoDoDiretorNaoEmpregadoSemJustaCausa,
                             mdtTerminoDeMandatoDoDiretorNaoEmpregado,
                             mdtExoneracaoAPedidoDeDiretorNaoEmpregado,
                             mdtExoneracaoDoDiretorNaoEmpregadoPorCulpaReciprocaOuForcaMaior,
                             mdtMorteDoDiretorNaoEmpregado,
                             mdtExoneracaoDoDiretorNaoEmpregadoPorFalenciaEncerramento,
                             mdtOutros);

  tpRepercProc            = (repDecisaoComPagamentoDeVerbasDeNaturezaRemuneratoria, repDecisaoSemPagamentoDeVerbasDeNaturezaRemuneratoria);

  tpIndReperc             = (repDecisaoComRepercussaoTributariaEOuFGTS, repDecisaoSemRepercussaoTributariaOuFGTS, repDecisaoComRepercussaoExclusivaParaDeclaracaoDeRendimentos);

  tpOrigemProc            = (oprProcessoJudicial, oprDemandaSubmetidaACCPOuAoNINTER);

  tpIndTpDedu             = (itdPrevidenciaOficial, itdPrevidenciaComplementar, itdFAPI, itdFunpresp, itdPensaoAlimenticia, itdDependentes);

  tpTpPrev                = (tprPrivada, tprFAPI, tprFunpresp);

  tpIndAprend             = (tiapContrDireta, tiapContrIndireta);

  tpIndTpDeducao          = (tpdPrevidenciaOficial,
                             tpdPrevidenciaPrivada,
                             tpdFundoDeAposentadoriaFAPI,
                             tpdFundoDePrevidenciaComplementarFunPresp,
                             tpdPensaoAlimenticia,
                             tpdDependentes);

  tpIndTpDeducaoT         = (tpdtPrevidenciaOficial,
                             tpdtPensaoAlimenticia,
                             tpdtDependentes);

  TMatrixEventoInfo       = (meiTipoEvento, meiTipoEventoString, meiVersao, meiEventoString, meiSchema, meiStrEventoToTipoEvento, meiObservacao);

  TRecMatrixEventoInfo = record
    TipoEvento           : TTipoEvento;
    TipoEventoString     : string;
    VersaoeSocial        : TVersaoeSocial;
    Versao               : string;
    EventoString         : string;
    Schema               : TeSocialSchema;
    StrEventoToTipoEvento: string;
    Observacao           : string;
  end;

const
  TEventoString: array[0..52] of String =('evtInfoEmpregador', 'evtTabEstab',
                                          'evtTabRubrica', 'evtTabLotacao',
                                          'evtTabCargo', 'evtTabCarreira',
                                          'evtTabFuncao', 'evtTabHorTur',
                                          'evtTabAmbiente', 'evtTabProcesso',
                                          'evtTabOperPort', 'S-2100',
                                          'evtRemun', 'evtRmnRPPS',
                                          'evtCdBenefIn', 'evtPgtos',
                                          'evtInfoIR', 'evtAqProd',
                                          'evtComProd', 'evtContratAvNP',
                                          'evtInfoComplPer', 'evtTotConting',
                                          'evtReabreEvPer', 'evtFechaEvPer',
                                          'evtContrSindPatr', 'evtAdmPrelim',
                                          'evtAdmissao', 'evtAltCadastral',
                                          'evtAltContratual', 'evtCAT',
                                          'evtMonit', 'evtAfastTemp',
                                          'evtExpRisco', 'evtInsApo',
                                          'evtAvPrevio', 'evtConvInterm',
                                          'evtReintegr', 'evtDeslig',
                                          'evtTSVInicio', 'S-2305',
                                          'evtTSVAltContr', 'evtTSVTermino',
                                          'evtCdBenPrRP', 'evtExclusao',
                                          'evtCdBenefAlt', 'evtCdBenIn',
                                          'evtCessao', 'evtCdBenAlt',
                                          'evtReativBen','evtCdBenTerm',
                                          'evtProcTrab', 'evtContProc',
                                          'evtExcProcTrab');

  EventoStringNova: array[0..58] of String =
      ('evtInfoEmpregador', 'evtTabEstab', 'evtTabRubrica', 'evtTabLotacao', 'evtTabCargo', 'evtTabCarreira',
       'evtTabFuncao', 'evtTabHorContratual', 'evtTabAmbiente', 'evtTabProcesso','evtTabOperPortuario',
       'S-2100', 'evtRemun', 'evtRmnRPPS', 'evtCdBenefIn', 'evtPgtos', 'S-1220', 'evtAqProd', 'evtComProd',
       'evtContratAvNP', 'evtInfoComplPer', 'evtTotConting', 'evtReabreEvPer', 'evtFechaEvPer',
       'evtContrSindPatr', 'evtAdmPrelim', 'evtAdmissao', 'evtAltCadastral','evtAltContratual', 'evtCAT',
       'evtASO', 'evtAfastTemp', 'evtExpRisco', 'evtInsApo', 'evtAvPrevio', 'evtConvInterm',
       'evtReintegr', 'evtDeslig', 'evtTSVInicio', 'S-2305', 'evtTSVAltContr', 'evtTSVTermino',
       'evtCdBenPrRP', 'evtExclusao', 'S-4000', 'S-4999', 'S-5001', 'S-5002', 'S-5003', 'S-5011',
       'S-5012', 'S-5013', 'evtToxic', 'evtCdBenefAlt', 'evtCdBenIn', 'evtCessao', 'evtCdBenAlt',
       'evtReativBen', 'evtCdBenTerm');



  TTrabalhadorSemVinculo: Array[0..27] of integer = (
                                          201, 202, 304, 305, 308, 311, 313, 401, 410, 501,
                                          701, 711, 712, 721, 722, 723, 731, 734, 738, 741,
                                          751, 761, 771, 781, 901, 902, 903, 904);

const
  __ARRAY_MATRIX_EVENTO_INFO: array [1 .. 161] of TRecMatrixEventoInfo =
    (
    // Leiaute S-1.2.0
    (TipoEvento: teS1000                   ; TipoEventoString: 'S-1000'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtInfoEmpregador'          ; Schema: schevtInfoEmpregador       ; StrEventoToTipoEvento: 'evtInfoEmpregador'          ; Observacao: ''),
    (TipoEvento: teS1005                   ; TipoEventoString: 'S-1005'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTabEstab'                ; Schema: schevtTabEstab             ; StrEventoToTipoEvento: 'evtTabEstab'                ; Observacao: ''),
    (TipoEvento: teS1010                   ; TipoEventoString: 'S-1010'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTabRubrica'              ; Schema: schevtTabRubrica           ; StrEventoToTipoEvento: 'evtTabRubrica'              ; Observacao: ''),
    (TipoEvento: teS1020                   ; TipoEventoString: 'S-1020'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTabLotacao'              ; Schema: schevtTabLotacao           ; StrEventoToTipoEvento: 'evtTabLotacao'              ; Observacao: ''),
    (TipoEvento: teS1070                   ; TipoEventoString: 'S-1070'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTabProcesso'             ; Schema: schevtTabProcesso          ; StrEventoToTipoEvento: 'evtTabProcesso'             ; Observacao: ''),
    (TipoEvento: teS1200                   ; TipoEventoString: 'S-1200'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtRemun'                   ; Schema: schevtRemun                ; StrEventoToTipoEvento: 'evtRemun'                   ; Observacao: ''),
    (TipoEvento: teS1202                   ; TipoEventoString: 'S-1202'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtRmnRPPS'                 ; Schema: schevtRmnRPPS              ; StrEventoToTipoEvento: 'evtRmnRPPS'                 ; Observacao: ''),
    (TipoEvento: teS1207                   ; TipoEventoString: 'S-1207'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtBenPrRP'                 ; Schema: schevtBenPrRP              ; StrEventoToTipoEvento: 'evtBenPrRP'                 ; Observacao: 'Até a v2.5 a tag era evtCdBenPrRP'),
    (TipoEvento: teS1210                   ; TipoEventoString: 'S-1210'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtPgtos'                   ; Schema: schevtPgtos                ; StrEventoToTipoEvento: 'evtPgtos'                   ; Observacao: ''),
    (TipoEvento: teS1260                   ; TipoEventoString: 'S-1260'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtComProd'                 ; Schema: schevtComProd              ; StrEventoToTipoEvento: 'evtComProd'                 ; Observacao: ''),
    (TipoEvento: teS1270                   ; TipoEventoString: 'S-1270'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtContratAvNP'             ; Schema: schevtContratAvNP          ; StrEventoToTipoEvento: 'evtContratAvNP'             ; Observacao: ''),
    (TipoEvento: teS1280                   ; TipoEventoString: 'S-1280'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtInfoComplPer'            ; Schema: schevtInfoComplPer         ; StrEventoToTipoEvento: 'evtInfoComplPer'            ; Observacao: ''),
    (TipoEvento: teS1298                   ; TipoEventoString: 'S-1298'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtReabreEvPer'             ; Schema: schevtReabreEvper          ; StrEventoToTipoEvento: 'evtReabreEvPer'             ; Observacao: ''),
    (TipoEvento: teS1299                   ; TipoEventoString: 'S-1299'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtFechaEvPer'              ; Schema: schevtFechaEvPer           ; StrEventoToTipoEvento: 'evtFechaEvPer'              ; Observacao: ''),
    (TipoEvento: teS2190                   ; TipoEventoString: 'S-2190'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtAdmPrelim'               ; Schema: schevtAdmPrelim            ; StrEventoToTipoEvento: 'evtAdmPrelim'               ; Observacao: ''),
    (TipoEvento: teS2200                   ; TipoEventoString: 'S-2200'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtAdmissao'                ; Schema: schevtAdmissao             ; StrEventoToTipoEvento: 'evtAdmissao'                ; Observacao: ''),
    (TipoEvento: teS2205                   ; TipoEventoString: 'S-2205'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtAltCadastral'            ; Schema: schevtAltCadastral         ; StrEventoToTipoEvento: 'evtAltCadastral'            ; Observacao: ''),
    (TipoEvento: teS2206                   ; TipoEventoString: 'S-2206'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtAltContratual'           ; Schema: schevtAltContratual        ; StrEventoToTipoEvento: 'evtAltContratual'           ; Observacao: ''),
    (TipoEvento: teS2210                   ; TipoEventoString: 'S-2210'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCAT'                     ; Schema: schevtCAT                  ; StrEventoToTipoEvento: 'evtCAT'                     ; Observacao: ''),
    (TipoEvento: teS2220                   ; TipoEventoString: 'S-2220'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtMonit'                   ; Schema: schevtMonit                ; StrEventoToTipoEvento: 'evtMonit'                   ; Observacao: ''),
    (TipoEvento: teS2230                   ; TipoEventoString: 'S-2230'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtAfastTemp'               ; Schema: schevtAfastTemp            ; StrEventoToTipoEvento: 'evtAfastTemp'               ; Observacao: ''),
    (TipoEvento: teS2231                   ; TipoEventoString: 'S-2231'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCessao'                  ; Schema: schevtCessao               ; StrEventoToTipoEvento: 'evtCessao'                  ; Observacao: ''),
    (TipoEvento: teS2240                   ; TipoEventoString: 'S-2240'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtExpRisco'                ; Schema: schevtExpRisco             ; StrEventoToTipoEvento: 'evtExpRisco'                ; Observacao: ''),
    (TipoEvento: teS2298                   ; TipoEventoString: 'S-2298'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtReintegr'                ; Schema: schevtReintegr             ; StrEventoToTipoEvento: 'evtReintegr'                ; Observacao: ''),
    (TipoEvento: teS2299                   ; TipoEventoString: 'S-2299'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtDeslig'                  ; Schema: schevtDeslig               ; StrEventoToTipoEvento: 'evtDeslig'                  ; Observacao: ''),
    (TipoEvento: teS2300                   ; TipoEventoString: 'S-2300'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTSVInicio'               ; Schema: schevtTSVInicio            ; StrEventoToTipoEvento: 'evtTSVInicio'               ; Observacao: ''),
    (TipoEvento: teS2306                   ; TipoEventoString: 'S-2306'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTSVAltContr'             ; Schema: schevtTSVAltContr          ; StrEventoToTipoEvento: 'evtTSVAltContr'             ; Observacao: ''),
    (TipoEvento: teS2399                   ; TipoEventoString: 'S-2399'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtTSVTermino'              ; Schema: schevtTSVTermino           ; StrEventoToTipoEvento: 'evtTSVTermino'              ; Observacao: ''),
    (TipoEvento: teS2400                   ; TipoEventoString: 'S-2400'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCdBenefIn'               ; Schema: schevtCdBenefIn            ; StrEventoToTipoEvento: 'evtCdBenefIn'               ; Observacao: ''),
    (TipoEvento: teS2405                   ; TipoEventoString: 'S-2405'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCdBenefAlt'              ; Schema: schevtCdBenefAlt           ; StrEventoToTipoEvento: 'evtCdBenefAlt'              ; Observacao: ''),
    (TipoEvento: teS2410                   ; TipoEventoString: 'S-2410'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCdBenIn'                 ; Schema: schevtCdBenIn              ; StrEventoToTipoEvento: 'evtCdBenIn'                 ; Observacao: ''),
    (TipoEvento: teS2416                   ; TipoEventoString: 'S-2416'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCdBenAlt'                ; Schema: schevtCdBenAlt             ; StrEventoToTipoEvento: 'evtCdBenAlt'                ; Observacao: ''),
    (TipoEvento: teS2418                   ; TipoEventoString: 'S-2418'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtReativBen'               ; Schema: schevtReativBen            ; StrEventoToTipoEvento: 'evtReativBen'               ; Observacao: ''),
    (TipoEvento: teS2420                   ; TipoEventoString: 'S-2420'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCdBenTerm'               ; Schema: schevtCdBenTerm            ; StrEventoToTipoEvento: 'evtCdBenTerm'               ; Observacao: ''),
    (TipoEvento: teS2500                   ; TipoEventoString: 'S-2500'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtProcTrab'                ; Schema: schevtProcTrab             ; StrEventoToTipoEvento: 'evtProcTrab'                ; Observacao: ''),
    (TipoEvento: teS2501                   ; TipoEventoString: 'S-2501'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtContProc'                ; Schema: schevtContProc             ; StrEventoToTipoEvento: 'evtContProc'                ; Observacao: ''),
    (TipoEvento: teS3000                   ; TipoEventoString: 'S-3000'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtExclusao'                ; Schema: schevtExclusao             ; StrEventoToTipoEvento: 'evtExclusao'                ; Observacao: ''),
    (TipoEvento: teS3500                   ; TipoEventoString: 'S-3500'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtExcProcTrab'             ; Schema: schevtExcProcTrab          ; StrEventoToTipoEvento: 'evtExcProcTrab'             ; Observacao: ''),
    (TipoEvento: teS5001                   ; TipoEventoString: 'S-5001'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtBasesTrab'               ; Schema: schevtBasesTrab            ; StrEventoToTipoEvento: 'S-5001'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5002                   ; TipoEventoString: 'S-5002'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtIrrfBenef'               ; Schema: schevtIrrfBenef            ; StrEventoToTipoEvento: 'S-5002'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5003                   ; TipoEventoString: 'S-5003'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtBasesFGTS'               ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5003'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5011                   ; TipoEventoString: 'S-5011'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtCS'                      ; Schema: schevtCS                   ; StrEventoToTipoEvento: 'S-5011'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5013                   ; TipoEventoString: 'S-5013'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtFGTS'                    ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5013'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS8299                   ; TipoEventoString: 'S-8299'                    ; VersaoeSocial: veS01_02_00; Versao: 'vS1_02'; EventoString: 'evtBaixa'                   ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'evtBaixa'                   ; Observacao: ''),
    // Leiaute S-1.1.0
    (TipoEvento: teS1000                   ; TipoEventoString: 'S-1000'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtInfoEmpregador'          ; Schema: schevtInfoEmpregador       ; StrEventoToTipoEvento: 'evtInfoEmpregador'          ; Observacao: ''),
    (TipoEvento: teS1005                   ; TipoEventoString: 'S-1005'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTabEstab'                ; Schema: schevtTabEstab             ; StrEventoToTipoEvento: 'evtTabEstab'                ; Observacao: ''),
    (TipoEvento: teS1010                   ; TipoEventoString: 'S-1010'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTabRubrica'              ; Schema: schevtTabRubrica           ; StrEventoToTipoEvento: 'evtTabRubrica'              ; Observacao: ''),
    (TipoEvento: teS1020                   ; TipoEventoString: 'S-1020'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTabLotacao'              ; Schema: schevtTabLotacao           ; StrEventoToTipoEvento: 'evtTabLotacao'              ; Observacao: ''),
    (TipoEvento: teS1070                   ; TipoEventoString: 'S-1070'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTabProcesso'             ; Schema: schevtTabProcesso          ; StrEventoToTipoEvento: 'evtTabProcesso'             ; Observacao: ''),
    (TipoEvento: teS1200                   ; TipoEventoString: 'S-1200'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtRemun'                   ; Schema: schevtRemun                ; StrEventoToTipoEvento: 'evtRemun'                   ; Observacao: ''),
    (TipoEvento: teS1202                   ; TipoEventoString: 'S-1202'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtRmnRPPS'                 ; Schema: schevtRmnRPPS              ; StrEventoToTipoEvento: 'evtRmnRPPS'                 ; Observacao: ''),
    (TipoEvento: teS1207                   ; TipoEventoString: 'S-1207'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtBenPrRP'                 ; Schema: schevtBenPrRP              ; StrEventoToTipoEvento: 'evtBenPrRP'                 ; Observacao: 'Até a v2.5 a tag era evtCdBenPrRP'),
    (TipoEvento: teS1210                   ; TipoEventoString: 'S-1210'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtPgtos'                   ; Schema: schevtPgtos                ; StrEventoToTipoEvento: 'evtPgtos'                   ; Observacao: ''),
    (TipoEvento: teS1260                   ; TipoEventoString: 'S-1260'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtComProd'                 ; Schema: schevtComProd              ; StrEventoToTipoEvento: 'evtComProd'                 ; Observacao: ''),
    (TipoEvento: teS1270                   ; TipoEventoString: 'S-1270'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtContratAvNP'             ; Schema: schevtContratAvNP          ; StrEventoToTipoEvento: 'evtContratAvNP'             ; Observacao: ''),
    (TipoEvento: teS1280                   ; TipoEventoString: 'S-1280'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtInfoComplPer'            ; Schema: schevtInfoComplPer         ; StrEventoToTipoEvento: 'evtInfoComplPer'            ; Observacao: ''),
    (TipoEvento: teS1298                   ; TipoEventoString: 'S-1298'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtReabreEvPer'             ; Schema: schevtReabreEvper          ; StrEventoToTipoEvento: 'evtReabreEvPer'             ; Observacao: ''),
    (TipoEvento: teS1299                   ; TipoEventoString: 'S-1299'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtFechaEvPer'              ; Schema: schevtFechaEvPer           ; StrEventoToTipoEvento: 'evtFechaEvPer'              ; Observacao: ''),
    (TipoEvento: teS2190                   ; TipoEventoString: 'S-2190'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtAdmPrelim'               ; Schema: schevtAdmPrelim            ; StrEventoToTipoEvento: 'evtAdmPrelim'               ; Observacao: ''),
    (TipoEvento: teS2200                   ; TipoEventoString: 'S-2200'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtAdmissao'                ; Schema: schevtAdmissao             ; StrEventoToTipoEvento: 'evtAdmissao'                ; Observacao: ''),
    (TipoEvento: teS2205                   ; TipoEventoString: 'S-2205'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtAltCadastral'            ; Schema: schevtAltCadastral         ; StrEventoToTipoEvento: 'evtAltCadastral'            ; Observacao: ''),
    (TipoEvento: teS2206                   ; TipoEventoString: 'S-2206'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtAltContratual'           ; Schema: schevtAltContratual        ; StrEventoToTipoEvento: 'evtAltContratual'           ; Observacao: ''),
    (TipoEvento: teS2210                   ; TipoEventoString: 'S-2210'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCAT'                     ; Schema: schevtCAT                  ; StrEventoToTipoEvento: 'evtCAT'                     ; Observacao: ''),
    (TipoEvento: teS2220                   ; TipoEventoString: 'S-2220'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtMonit'                   ; Schema: schevtMonit                ; StrEventoToTipoEvento: 'evtMonit'                   ; Observacao: ''),
    (TipoEvento: teS2230                   ; TipoEventoString: 'S-2230'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtAfastTemp'               ; Schema: schevtAfastTemp            ; StrEventoToTipoEvento: 'evtAfastTemp'               ; Observacao: ''),
    (TipoEvento: teS2231                   ; TipoEventoString: 'S-2231'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCessao'                  ; Schema: schevtCessao               ; StrEventoToTipoEvento: 'evtCessao'                  ; Observacao: ''),
    (TipoEvento: teS2240                   ; TipoEventoString: 'S-2240'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtExpRisco'                ; Schema: schevtExpRisco             ; StrEventoToTipoEvento: 'evtExpRisco'                ; Observacao: ''),
    (TipoEvento: teS2298                   ; TipoEventoString: 'S-2298'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtReintegr'                ; Schema: schevtReintegr             ; StrEventoToTipoEvento: 'evtReintegr'                ; Observacao: ''),
    (TipoEvento: teS2299                   ; TipoEventoString: 'S-2299'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtDeslig'                  ; Schema: schevtDeslig               ; StrEventoToTipoEvento: 'evtDeslig'                  ; Observacao: ''),
    (TipoEvento: teS2300                   ; TipoEventoString: 'S-2300'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTSVInicio'               ; Schema: schevtTSVInicio            ; StrEventoToTipoEvento: 'evtTSVInicio'               ; Observacao: ''),
    (TipoEvento: teS2306                   ; TipoEventoString: 'S-2306'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTSVAltContr'             ; Schema: schevtTSVAltContr          ; StrEventoToTipoEvento: 'evtTSVAltContr'             ; Observacao: ''),
    (TipoEvento: teS2399                   ; TipoEventoString: 'S-2399'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtTSVTermino'              ; Schema: schevtTSVTermino           ; StrEventoToTipoEvento: 'evtTSVTermino'              ; Observacao: ''),
    (TipoEvento: teS2400                   ; TipoEventoString: 'S-2400'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCdBenefIn'               ; Schema: schevtCdBenefIn            ; StrEventoToTipoEvento: 'evtCdBenefIn'               ; Observacao: ''),
    (TipoEvento: teS2405                   ; TipoEventoString: 'S-2405'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCdBenefAlt'              ; Schema: schevtCdBenefAlt           ; StrEventoToTipoEvento: 'evtCdBenefAlt'              ; Observacao: ''),
    (TipoEvento: teS2410                   ; TipoEventoString: 'S-2410'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCdBenIn'                 ; Schema: schevtCdBenIn              ; StrEventoToTipoEvento: 'evtCdBenIn'                 ; Observacao: ''),
    (TipoEvento: teS2416                   ; TipoEventoString: 'S-2416'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCdBenAlt'                ; Schema: schevtCdBenAlt             ; StrEventoToTipoEvento: 'evtCdBenAlt'                ; Observacao: ''),
    (TipoEvento: teS2418                   ; TipoEventoString: 'S-2418'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtReativBen'               ; Schema: schevtReativBen            ; StrEventoToTipoEvento: 'evtReativBen'               ; Observacao: ''),
    (TipoEvento: teS2420                   ; TipoEventoString: 'S-2420'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCdBenTerm'               ; Schema: schevtCdBenTerm            ; StrEventoToTipoEvento: 'evtCdBenTerm'               ; Observacao: ''),
    (TipoEvento: teS2500                   ; TipoEventoString: 'S-2500'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtProcTrab'                ; Schema: schevtProcTrab             ; StrEventoToTipoEvento: 'evtProcTrab'                ; Observacao: ''),
    (TipoEvento: teS2501                   ; TipoEventoString: 'S-2501'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtContProc'                ; Schema: schevtContProc             ; StrEventoToTipoEvento: 'evtContProc'                ; Observacao: ''),
    (TipoEvento: teS3000                   ; TipoEventoString: 'S-3000'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtExclusao'                ; Schema: schevtExclusao             ; StrEventoToTipoEvento: 'evtExclusao'                ; Observacao: ''),
    (TipoEvento: teS3500                   ; TipoEventoString: 'S-3500'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtExcProcTrab'             ; Schema: schevtExcProcTrab          ; StrEventoToTipoEvento: 'evtExcProcTrab'             ; Observacao: ''),
    (TipoEvento: teS5001                   ; TipoEventoString: 'S-5001'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtBasesTrab'               ; Schema: schevtBasesTrab            ; StrEventoToTipoEvento: 'S-5001'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5002                   ; TipoEventoString: 'S-5002'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtIrrfBenef'               ; Schema: schevtIrrfBenef            ; StrEventoToTipoEvento: 'S-5002'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5003                   ; TipoEventoString: 'S-5003'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtBasesFGTS'               ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5003'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5011                   ; TipoEventoString: 'S-5011'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtCS'                      ; Schema: schevtCS                   ; StrEventoToTipoEvento: 'S-5011'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5013                   ; TipoEventoString: 'S-5013'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtFGTS'                    ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5013'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS8299                   ; TipoEventoString: 'S-8299'                    ; VersaoeSocial: veS01_01_00; Versao: 'vS1_01'; EventoString: 'evtBaixa'                   ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'evtBaixa'                   ; Observacao: ''),
    // Leiaute S-1.0.0
    (TipoEvento: teS1000                   ; TipoEventoString: 'S-1000'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtInfoEmpregador'          ; Schema: schevtInfoEmpregador       ; StrEventoToTipoEvento: 'evtInfoEmpregador'          ; Observacao: ''),
    (TipoEvento: teS1005                   ; TipoEventoString: 'S-1005'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTabEstab'                ; Schema: schevtTabEstab             ; StrEventoToTipoEvento: 'evtTabEstab'                ; Observacao: ''),
    (TipoEvento: teS1010                   ; TipoEventoString: 'S-1010'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTabRubrica'              ; Schema: schevtTabRubrica           ; StrEventoToTipoEvento: 'evtTabRubrica'              ; Observacao: ''),
    (TipoEvento: teS1020                   ; TipoEventoString: 'S-1020'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTabLotacao'              ; Schema: schevtTabLotacao           ; StrEventoToTipoEvento: 'evtTabLotacao'              ; Observacao: ''),
    (TipoEvento: teS1070                   ; TipoEventoString: 'S-1070'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTabProcesso'             ; Schema: schevtTabProcesso          ; StrEventoToTipoEvento: 'evtTabProcesso'             ; Observacao: ''),
    (TipoEvento: teS1200                   ; TipoEventoString: 'S-1200'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtRemun'                   ; Schema: schevtRemun                ; StrEventoToTipoEvento: 'evtRemun'                   ; Observacao: ''),
    (TipoEvento: teS1202                   ; TipoEventoString: 'S-1202'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtRmnRPPS'                 ; Schema: schevtRmnRPPS              ; StrEventoToTipoEvento: 'evtRmnRPPS'                 ; Observacao: ''),
    (TipoEvento: teS1207                   ; TipoEventoString: 'S-1207'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtBenPrRP'                 ; Schema: schevtBenPrRP              ; StrEventoToTipoEvento: 'evtBenPrRP'                 ; Observacao: 'Até a v2.5 a tag era evtCdBenPrRP'),
    (TipoEvento: teS1210                   ; TipoEventoString: 'S-1210'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtPgtos'                   ; Schema: schevtPgtos                ; StrEventoToTipoEvento: 'evtPgtos'                   ; Observacao: ''),
    (TipoEvento: teS1220                   ; TipoEventoString: 'S-1220'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'S-1220'                     ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-1220'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS1260                   ; TipoEventoString: 'S-1260'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtComProd'                 ; Schema: schevtComProd              ; StrEventoToTipoEvento: 'evtComProd'                 ; Observacao: ''),
    (TipoEvento: teS1270                   ; TipoEventoString: 'S-1270'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtContratAvNP'             ; Schema: schevtContratAvNP          ; StrEventoToTipoEvento: 'evtContratAvNP'             ; Observacao: ''),
    (TipoEvento: teS1280                   ; TipoEventoString: 'S-1280'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtInfoComplPer'            ; Schema: schevtInfoComplPer         ; StrEventoToTipoEvento: 'evtInfoComplPer'            ; Observacao: ''),
    (TipoEvento: teS1298                   ; TipoEventoString: 'S-1298'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtReabreEvPer'             ; Schema: schevtReabreEvper          ; StrEventoToTipoEvento: 'evtReabreEvPer'             ; Observacao: ''),
    (TipoEvento: teS1299                   ; TipoEventoString: 'S-1299'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtFechaEvPer'              ; Schema: schevtFechaEvPer           ; StrEventoToTipoEvento: 'evtFechaEvPer'              ; Observacao: ''),
    (TipoEvento: teS2100                   ; TipoEventoString: 'S-2100'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'S-2100'                     ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-2100'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS2190                   ; TipoEventoString: 'S-2190'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtAdmPrelim'               ; Schema: schevtAdmPrelim            ; StrEventoToTipoEvento: 'evtAdmPrelim'               ; Observacao: ''),
    (TipoEvento: teS2200                   ; TipoEventoString: 'S-2200'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtAdmissao'                ; Schema: schevtAdmissao             ; StrEventoToTipoEvento: 'evtAdmissao'                ; Observacao: ''),
    (TipoEvento: teS2205                   ; TipoEventoString: 'S-2205'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtAltCadastral'            ; Schema: schevtAltCadastral         ; StrEventoToTipoEvento: 'evtAltCadastral'            ; Observacao: ''),
    (TipoEvento: teS2206                   ; TipoEventoString: 'S-2206'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtAltContratual'           ; Schema: schevtAltContratual        ; StrEventoToTipoEvento: 'evtAltContratual'           ; Observacao: ''),
    (TipoEvento: teS2210                   ; TipoEventoString: 'S-2210'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCAT'                     ; Schema: schevtCAT                  ; StrEventoToTipoEvento: 'evtCAT'                     ; Observacao: ''),
    (TipoEvento: teS2220                   ; TipoEventoString: 'S-2220'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtMonit'                   ; Schema: schevtMonit                ; StrEventoToTipoEvento: 'evtMonit'                   ; Observacao: ''),
    (TipoEvento: teS2221                   ; TipoEventoString: 'S-2221'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtToxic'                   ; Schema: schEvtToxic                ; StrEventoToTipoEvento: 'evtToxic'                   ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS2230                   ; TipoEventoString: 'S-2230'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtAfastTemp'               ; Schema: schevtAfastTemp            ; StrEventoToTipoEvento: 'evtAfastTemp'               ; Observacao: ''),
    (TipoEvento: teS2231                   ; TipoEventoString: 'S-2231'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCessao'                  ; Schema: schevtCessao               ; StrEventoToTipoEvento: 'evtCessao'                  ; Observacao: ''),
    (TipoEvento: teS2240                   ; TipoEventoString: 'S-2240'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtExpRisco'                ; Schema: schevtExpRisco             ; StrEventoToTipoEvento: 'evtExpRisco'                ; Observacao: ''),
    (TipoEvento: teS2298                   ; TipoEventoString: 'S-2298'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtReintegr'                ; Schema: schevtReintegr             ; StrEventoToTipoEvento: 'evtReintegr'                ; Observacao: ''),
    (TipoEvento: teS2299                   ; TipoEventoString: 'S-2299'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtDeslig'                  ; Schema: schevtDeslig               ; StrEventoToTipoEvento: 'evtDeslig'                  ; Observacao: ''),
    (TipoEvento: teS2300                   ; TipoEventoString: 'S-2300'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTSVInicio'               ; Schema: schevtTSVInicio            ; StrEventoToTipoEvento: 'evtTSVInicio'               ; Observacao: ''),
    (TipoEvento: teS2305                   ; TipoEventoString: 'S-2305'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'S-2305'                     ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-2305'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS2306                   ; TipoEventoString: 'S-2306'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTSVAltContr'             ; Schema: schevtTSVAltContr          ; StrEventoToTipoEvento: 'evtTSVAltContr'             ; Observacao: ''),
    (TipoEvento: teS2399                   ; TipoEventoString: 'S-2399'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtTSVTermino'              ; Schema: schevtTSVTermino           ; StrEventoToTipoEvento: 'evtTSVTermino'              ; Observacao: ''),
    (TipoEvento: teS2400                   ; TipoEventoString: 'S-2400'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCdBenefIn'               ; Schema: schevtCdBenefIn            ; StrEventoToTipoEvento: 'evtCdBenefIn'               ; Observacao: ''),
    (TipoEvento: teS2405                   ; TipoEventoString: 'S-2405'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCdBenefAlt'              ; Schema: schevtCdBenefAlt           ; StrEventoToTipoEvento: 'evtCdBenefAlt'              ; Observacao: ''),
    (TipoEvento: teS2410                   ; TipoEventoString: 'S-2410'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCdBenIn'                 ; Schema: schevtCdBenIn              ; StrEventoToTipoEvento: 'evtCdBenIn'                 ; Observacao: ''),
    (TipoEvento: teS2416                   ; TipoEventoString: 'S-2416'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCdBenAlt'                ; Schema: schevtCdBenAlt             ; StrEventoToTipoEvento: 'evtCdBenAlt'                ; Observacao: ''),
    (TipoEvento: teS2418                   ; TipoEventoString: 'S-2418'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtReativBen'               ; Schema: schevtReativBen            ; StrEventoToTipoEvento: 'evtReativBen'               ; Observacao: ''),
    (TipoEvento: teS2420                   ; TipoEventoString: 'S-2420'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCdBenTerm'               ; Schema: schevtCdBenTerm            ; StrEventoToTipoEvento: 'evtCdBenTerm'               ; Observacao: ''),
    (TipoEvento: teS3000                   ; TipoEventoString: 'S-3000'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtExclusao'                ; Schema: schevtExclusao             ; StrEventoToTipoEvento: 'evtExclusao'                ; Observacao: ''),
    (TipoEvento: teS4000                   ; TipoEventoString: 'S-4000'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'NI'                         ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-4000'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS4999                   ; TipoEventoString: 'S-4999'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'NI'                         ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-4999'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teS5001                   ; TipoEventoString: 'S-5001'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtBasesTrab'               ; Schema: schevtBasesTrab            ; StrEventoToTipoEvento: 'S-5001'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5002                   ; TipoEventoString: 'S-5002'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtIrrfBenef'               ; Schema: schevtIrrfBenef            ; StrEventoToTipoEvento: 'S-5002'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5003                   ; TipoEventoString: 'S-5003'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtBasesFGTS'               ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5003'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5011                   ; TipoEventoString: 'S-5011'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtCS'                      ; Schema: schevtCS                   ; StrEventoToTipoEvento: 'S-5011'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS5013                   ; TipoEventoString: 'S-5013'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtFGTS'                    ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'S-5013'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS8299                   ; TipoEventoString: 'S-8299'                    ; VersaoeSocial: veS01_00_00; Versao: 'vS1_00'; EventoString: 'evtBaixa'                   ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'evtBaixa'                   ; Observacao: ''),
    (TipoEvento: teASO                     ; TipoEventoString: 'S-EVTASO'                  ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtASO'                     ; Schema: schNaoIdentificado         ; StrEventoToTipoEvento: 'evtASO'                     ; Observacao: 'Evento não identificado'),
    (TipoEvento: teAdesao                  ; TipoEventoString: 'S-EVTADESAO'               ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtAdesao'                  ; Schema: schevtAdesao               ; StrEventoToTipoEvento: 'evtAdesao'                  ; Observacao: 'Evento não identificado'),
    (TipoEvento: teCadInicial              ; TipoEventoString: 'S-EVTCADINICIAL'           ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtCadInicial'              ; Schema: schevtCadInicial           ; StrEventoToTipoEvento: 'evtCadInicial'              ; Observacao: 'Evento não identificado'),
    (TipoEvento: teConsultaIdentEventos    ; TipoEventoString: 'S-CONSULTAIDENTEVENTOS'    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtConsultaIdentEventos'    ; Schema: schConsultaIdentEventos    ; StrEventoToTipoEvento: 'evtConsultaIdentEventos'    ; Observacao: 'Evento operacional'),
    (TipoEvento: teConsultaLoteEventos     ; TipoEventoString: 'S-CONSULTALOTEEVENTOS'     ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtConsultaLoteEventos'     ; Schema: schConsultaLoteEventos     ; StrEventoToTipoEvento: 'evtConsultaLoteEventos'     ; Observacao: 'Evento operacional'),
    (TipoEvento: teDownloadEventos         ; TipoEventoString: 'S-DOWNLOADEVENTOS'         ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtDownloadEventos'         ; Schema: schDownloadEventos         ; StrEventoToTipoEvento: 'evtDownloadEventos'         ; Observacao: 'Evento operacional'),
    (TipoEvento: teEnvioLoteEventos        ; TipoEventoString: 'S-ENVIOLOTEEVENTOS'        ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtEnvioLoteEventos'        ; Schema: schEnvioLoteEventos        ; StrEventoToTipoEvento: 'evtEnvioLoteEventos'        ; Observacao: 'Evento operacional'),
    (TipoEvento: teErro                    ; TipoEventoString: 'S-ERRO'                    ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtErro'                    ; Schema: schErro                    ; StrEventoToTipoEvento: 'evtErro'                    ; Observacao: 'Evento operacional'),
    (TipoEvento: teInsApo                  ; TipoEventoString: 'S-EVTINSAPO'               ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtInsApo'                  ; Schema: schevtInsApo               ; StrEventoToTipoEvento: 'evtInsApo'                  ; Observacao: 'Evento não identificado'),
    (TipoEvento: teRetornoEnvioLoteEventos ; TipoEventoString: 'S-RETORNOENVIOLOTEEVENTOS' ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtRetornoEnvioLoteEventos' ; Schema: schRetornoEnvioLoteEventos ; StrEventoToTipoEvento: 'evtRetornoEnvioLoteEventos' ; Observacao: 'Evento operacional'),
    (TipoEvento: teRetornoEvento           ; TipoEventoString: 'S-RETORNOEVENTO'           ; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtRetornoEvento'           ; Schema: schRetornoEvento           ; StrEventoToTipoEvento: 'evtRetornoEvento'           ; Observacao: 'Evento operacional'),
    (TipoEvento: teRetornoProcessamentoLote; TipoEventoString: 'S-RETORNOPROCESSAMENTOLOTE'; VersaoeSocial: veS01_00_00; Versao: 'NI'    ; EventoString: 'evtRetornoProcessamentoLote'; Schema: schRetornoProcessamentoLote; StrEventoToTipoEvento: 'evtRetornoProcessamentoLote'; Observacao: 'Evento operacional'),
    // Leiaute 2.5.0
    (TipoEvento: teS1030                   ; TipoEventoString: 'S-1030'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabCargo'                ; Schema: schevtTabCargo             ; StrEventoToTipoEvento: 'evtTabCargo'                ; Observacao: ''),
    (TipoEvento: teS1035                   ; TipoEventoString: 'S-1035'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabCarreira'             ; Schema: schevtTabCarreira          ; StrEventoToTipoEvento: 'evtTabCarreira'             ; Observacao: ''),
    (TipoEvento: teS1040                   ; TipoEventoString: 'S-1040'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabFuncao'               ; Schema: schevtTabFuncao            ; StrEventoToTipoEvento: 'evtTabFuncao'               ; Observacao: ''),
    (TipoEvento: teS1050                   ; TipoEventoString: 'S-1050'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabHorTur'               ; Schema: schevtTabHorTur            ; StrEventoToTipoEvento: 'evtTabHorContratual'        ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS1060                   ; TipoEventoString: 'S-1060'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabAmbiente'             ; Schema: schevtTabAmbiente          ; StrEventoToTipoEvento: 'evtTabAmbiente'             ; Observacao: ''),
    (TipoEvento: teS1080                   ; TipoEventoString: 'S-1080'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTabOperPort'             ; Schema: schevtTabOperPort          ; StrEventoToTipoEvento: 'evtTabOperPortuario'        ; Observacao: 'TeventoString <> StrEventoToTipoEvento'),
    (TipoEvento: teS1207                   ; TipoEventoString: 'S-1207'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtCdBenPrRP'               ; Schema: schevtCdBenPrRP            ; StrEventoToTipoEvento: 'evtCdBenPrRP'               ; Observacao: ''),
    (TipoEvento: teS1250                   ; TipoEventoString: 'S-1250'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtAqProd'                  ; Schema: schevtAqProd               ; StrEventoToTipoEvento: 'evtAqProd'                  ; Observacao: ''),
    (TipoEvento: teS1295                   ; TipoEventoString: 'S-1295'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTotConting'              ; Schema: schevtTotConting           ; StrEventoToTipoEvento: 'evtTotConting'              ; Observacao: ''),
    (TipoEvento: teS1300                   ; TipoEventoString: 'S-1300'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtContrSindPatr'           ; Schema: schevtContrSindPatr        ; StrEventoToTipoEvento: 'evtContrSindPatr'           ; Observacao: ''),
    (TipoEvento: teS2245                   ; TipoEventoString: 'S-2245'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtTreiCap'                 ; Schema: schEvtTreiCap              ; StrEventoToTipoEvento: 'evtTreiCap'                 ; Observacao: ''),
    (TipoEvento: teS2250                   ; TipoEventoString: 'S-2250'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtAvPrevio'                ; Schema: schevtAvPrevio             ; StrEventoToTipoEvento: 'evtAvPrevio'                ; Observacao: ''),
    (TipoEvento: teS2260                   ; TipoEventoString: 'S-2260'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtConvInterm'              ; Schema: schevtConvInterm           ; StrEventoToTipoEvento: 'evtConvInterm'              ; Observacao: ''),
    (TipoEvento: teS5012                   ; TipoEventoString: 'S-5012'                    ; VersaoeSocial: ve02_05_00 ; Versao: 'v2_5'  ; EventoString: 'evtIRRF'                    ; Schema: schevtIrrf                 ; StrEventoToTipoEvento: 'S-5012'                     ; Observacao: 'TeventoString <> StrEventoToTipoEvento')
    );

function GetMatrixEventoInfo(AInfoEventoMatrix: TMatrixEventoInfo; ASearchValue: string; AVersaoeSocial: TVersaoeSocial): TRecMatrixEventoInfo;

function TipoEventoToStr(const t: TTipoEvento ): string;
function StrToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function StrEventoToTipoEvento(out ok: boolean; const s: string): TTipoEvento;
function StringINIToTipoEvento(out ok: boolean; const s: string; AVersaoeSocial: TVersaoeSocial): TTipoEvento;
function StringXMLToTipoEvento(out ok: boolean; const s: string; AVersaoeSocial: TVersaoeSocial): TTipoEvento;
function TipoEventoToStrEvento(const t: TTipoEvento; AVersaoeSocial: TVersaoeSocial): string;

function eSprocEmiToStr(const t: TpprocEmi ): string;
function eSStrToprocEmi(var ok: boolean; const s: string): TpprocEmi;

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

function eSIndAcordoIsencaoMultaToStr(const t: TpIndAcordoIsencaoMulta ): string;
function eSStrToIndAcordoIsencaoMulta(var ok: boolean; const s: string): TpIndAcordoIsencaoMulta;

//function eSufToStr(const t: Tpuf ): string;
//function eSStrTouf(var ok: boolean; const s: string): Tpuf;

function eSCodIncCPToStr(const t: tpCodIncCP ): string;
function eSStrToCodIncCP(var ok: boolean; const s: string): tpCodIncCP;

function eSCodIncIRRFToStr(const t: tpCodIncIRRF ): string;
function eSStrToCodIncIRRF(var ok: boolean; const s: string): tpCodIncIRRF;

function eSCodIncFGTSToStr(const t: TpCodIncFGTS ): string;
function eSStrToCodIncFGTS(var ok: boolean; const s: string): TpCodIncFGTS;

function eSCodIncCPRPToStr(const t: TpCodIncCPRP ): string;
function eSStrToCodIncCPRP(var ok: boolean; const s: string): TpCodIncCPRP;

function eSIndSuspToStr(const t: tpIndSusp): string;
function eSStrToIndSusp(var ok: Boolean; const s: string): tpIndSusp;

function eSExtDecisaoToStr(const t: TpExtDecisao ): string;
function eSStrToExtDecisao(var ok: boolean; const s: string): TpExtDecisao;

function eStpInscContratanteToStr(const t: TptpInscContratante ): string;
function eSStrTotpInscContratante(var ok: boolean; const s: string): TptpInscContratante;

function eSTpInscPropToStr(const t: TpTpInscProp ): string;
function eSStrToTpInscProp(var ok: boolean; const s: string): TpTpInscProp;

function eSIndSubstPatronalObraToStr(const t: TpIndSubstPatronalObra ): string;
function eSStrToIndSubstPatronalObra(var ok: boolean; const s: string): TpIndSubstPatronalObra;

function eSindAutoriaToStr(const t: TpindAutoria ): string;
function eSStrToindAutoria(var ok: boolean; const s: string): TpindAutoria;

function eSTpIndMatProcToStr(const t: tpIndMatProc): string;
function eSStrToTpIndMatProc(var ok: boolean; const s: string): tpIndMatProc;

function eSIndRetificacaoToStr(const t: TpIndRetificacao ): string;
function eSStrToIndRetificacao(out ok: boolean; const s: string): TpIndRetificacao;

function eSIndApuracaoToStr(const t: TpIndApuracao ): string;
function eSStrToIndApuracao(var ok: boolean; const s: string): TpIndApuracao;

function eSIndMVToStr(const t: TpIndMV ): string;
function eSStrToIndMV(var ok: boolean; const s: string): TpIndMV;

function eSIndSimplesToStr(const t: TpIndSimples ): string;
function eSStrToIndSimples(var ok: boolean; const s: string): TpIndSimples;

function eSNatAtividadeToStr(const t: TpNatAtividade ): string;
function eSStrToNatAtividade(var ok: boolean; const s: string): TpNatAtividade;

function eSTpTributoToStr(const t: TpTpTributo ): string;
function eSStrToTpTributo(var ok: boolean; const s: string): TpTpTributo;

function eSTpIndApurIRToStr(const t: tpIndApurIR ): string;
function eSStrToTpindApurIR(var ok: boolean; const s: string): tpIndApurIR;

function eSGrauExpToStr(const t: TpGrauExp ): string;
function eSStrToGrauExp(var ok: boolean; const s: string): TpGrauExp;

function eSIndNIFToStr(const t: TpIndNIF ): string;
function eSStrToIndNIF(var ok: boolean; const s: string): TpIndNIF;

function eSTpProcRRAToStr(const t: TpTpProcRRA ): string;
function eSStrToTpProcRRA(var ok: boolean; const s: string): TpTpProcRRA;

function eStpTpProcRetToStr(const t: tpTpProcRet ): string;
function eSStrTotpTpProcRet(var ok: boolean; const s: string): tpTpProcRet;

function eSTpInscAdvogadoToStr(const t: TpTpInscAdvogado ): string;
function eSStrToTpInscAdvogado(var ok: boolean; const s: string): TpTpInscAdvogado;

function eSIndIncidenciaToStr(const t: TpIndIncidencia ): string;
function eSStrToIndIncidencia(var ok: boolean; const s: string): TpIndIncidencia;

function eSIndAbrangenciaToStr(const t: TpIndAbrangencia ): string;
function eSStrToIndAbrangencia(var ok: boolean; const s: string): TpIndAbrangencia;

function eSIndComercializacaoToStr(const t: TpIndComercializacao ): string;
function eSStrToIndComercializacao(var ok: boolean; const s: string): TpIndComercializacao;

function eSTpRubrToStr(const t: tpTpRubr): string;
function eSStrToTpRubr(var ok: Boolean; const s: string): tpTpRubr;

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

function eSTpRegPrevFacultativoToStr(const t: tpTpRegPrevFacultativo ): string;
function eSStrTotpRegPrevFacultativo(var ok: boolean; const s: string): tpTpRegPrevFacultativo;

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

function eSSimNaoFacultativoToStr(const t: tpSimNaoFacultativo ): string;
function eSStrToSimNaoFacultativo(var ok: boolean; const s: string): tpSimNaoFacultativo;

function eSSimFacultativoToStr(const t: tpSimFacultativo ): string;
function eSStrToSimFacultativo(var ok: boolean; const s: string): tpSimFacultativo;

function eSIniciatCATToStr(const t: tpIniciatCAT ): string;
function eSStrToIniciatCAT(var ok: boolean; const s: string): tpIniciatCAT;

function eSTpLocalToStr(const t: tpTpLocal ): string;
function eSStrToTpLocal(var ok: boolean; const s: string): tpTpLocal;

function eSLateralidadeToStr(const t: tpLateralidade ): string;
function eSStrToLateralidade(var ok: boolean; const s: string): tpLateralidade;

function eSIdeOCToStr(const t: tpIdeOC ): string;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eSIdeOCToStrEX'{$ENDIF};
function eSStrToIdeOC(var ok: boolean; const s: string): tpIdeOC;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função esStrToIdeOCEX'{$ENDIF};
function eSIdeOCToStrEX(const t: tpIdeOC): string;
function esStrToIdeOCEX(const s: string): tpIdeOC;

function eSTpReintToStr(const t: tpTpReint ): string;
function eSStrToTpReint(var ok: boolean; const s: string): tpTpReint;

function eSIndSubstPatrStr(const t: tpIndSubstPatr ): string;
function eSStrToIndSubstPatr(var ok: boolean; const s: string): tpIndSubstPatr;

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

function eSTpNivelEstagioToStr(const t: tpNivelEstagio): string;
function eSStrTotpNivelEstagio(var ok: Boolean; const s: string): tpNivelEstagio;

function eSTpPlanRPToStr(const t: tpPlanRP): string;
function eSStrToTpPlanRP(var ok: boolean; const s: string): tpPlanRP;

function eSTpMtvAltToStr(const t: tpMtvAlt): string;
function eSStrToTpMtvAlt(var ok: boolean; const s: string): tpMtvAlt;

function eSTpOrigemAltAfastToStr(const t: tpOrigemAltAfast): string;
function eSStrToTpOrigemAltAfast(var ok: boolean; const s: string): tpOrigemAltAfast;

function eSTpPensaoAlimToStr(const t: tpPensaoAlim): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a função eSTpPensaoAlimToStrEx.' {$EndIf};
function eSStrToTpPensaoAlim(var ok: boolean; const s: string): tpPensaoAlim; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eSStrToTpPensaoAlimEx.' {$EndIf};
function eSTpPensaoAlimToStrEx(const t: tpPensaoAlim): String;
function eSStrToTpPensaoAlimEx(const s: string): tpPensaoAlim;

function eSTpCumprParcialAvisoToStr(const t: tpCumprParcialAviso): string;
function eSStrToTpCumprParcialAviso(var ok: boolean; const s: string): tpCumprParcialAviso;

function eSModoLancamentoToStr(const t: TModoLancamento): string;
function eSStrToModoLancamento(var ok: boolean; const s: string): TModoLancamento;

function LayOuteSocialToServico(const t: TLayOut): String;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;

function LayOutToSchema(const t: TLayOut): TeSocialSchema;

function SchemaESocialToStr(const t: TeSocialSchema): String;
function StrToSchemaESocial(const s: String): TeSocialSchema;
function TipoEventoToSchemaeSocial(const t: TTipoEvento; AVersaoeSocial: TVersaoeSocial): TeSocialSchema;

{-------------------------------------------------------------------------------
Funções StrToVersaoeSocial e VersaoeSocialToStr foram tornadas obsoletas. Elas não são intercambiáveis.
O valor retornado por uma não é compatível com a outra no caso das versões de leiaute simplificados.
Para converter para string e vice versa, use StrToVersaoeSocialEX e VersaoeSocialToStrEX.
Mas caso precise do valor string para usar nos schemas use as funções StrToVersaoeSocialSchemas e VersaoeSocialToStrSchemas.
-------------------------------------------------------------------------------}
function StrToVersaoeSocial(out ok: Boolean; const s: String): TVersaoeSocial; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a função StrToVersaoeSocialEX ou StrToVersaoeSocialSchemas conforme sua necessidade.' {$ENDIF};
function VersaoeSocialToStr(const t: TVersaoeSocial): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a função VersaoeSocialToStrEX ou VersaoeSocialToStrSchemas conforme sua necessidade.' {$ENDIF};
function VersaoeSocialToDbl(const t: TVersaoeSocial): Real; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a função VersaoeSocialToDblEX.' {$ENDIF};
function DblToVersaoeSocial(out ok: Boolean; const d: Real): TVersaoeSocial;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a função DblToVersaoeSocialEX.' {$ENDIF};
function VersaoeSocialToStrEX(const t: TVersaoeSocial): String;
function StrToVersaoeSocialEX(const s: String): TVersaoeSocial;
function VersaoeSocialToStrSchemas(const t: TVersaoeSocial): String;
function StrToVersaoeSocialSchemas(const s: String): TVersaoeSocial;
function VersaoeSocialToDblEX(const t: TVersaoeSocial): Real;
function DblToVersaoeSocialEX(const d: Real): TVersaoeSocial;

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

function tpTmpResidToStr(const t: tpTmpResid ): string;
function StrTotpTmpResid(var ok: boolean; const s: string): tpTmpResid;

function tpCondIngToStr(const t: tpCondIng ): string;
function StrTotpCondIng(var ok: boolean; const s: string): tpCondIng;

function eSTpIndSitBenefToStr(const t: tpIndSitBenef): string;
function eSStrToTpIndSitBenef(var ok: boolean; const s: string): tpIndSitBenef;

function eStpTpPenMorteToStr(const t: tpTpPenMorte): string;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eStpTpPenMorteToStrEX conforme a sua necessidade' {$ENDIF};
function eSStrTotpTpPenMorte(var ok: boolean; const s: string): tpTpPenMorte;deprecated{$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eSStrTotpTpPenMorteEX conforme a sua necessidade' {$ENDIF};
function eStpTpPenMorteToStrEX(const t: tpTpPenMorte):string;
function eSStrTotpTpPenMorteEX(const s: string): tpTpPenMorte;

function eStpTpMotCessBenefToStr(const t: tpMotCessBenef): string;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eStpTpMotCessBenefToStrEX conforme a sua necessidade' {$ENDIF};
function eSStrToTpMotCessBenef(var ok: boolean; const s: string): tpMotCessBenef;deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS}'Use a função eSStrToTpMotCessBenefEX conforme a sua necessidade' {$ENDIF};
function eStpTpMotCessBenefToStrEX(const t: tpMotCessBenef): string;
function eSStrToTpMotCessBenefEX(const s: string): tpMotCessBenef;

function eStpTpMtvSuspensaoToStr(const t: tpMtvSuspensao): string;
function eSStrToTpMtvSuspensao(var ok: boolean; const s: string): tpMtvSuspensao;

function eSTpPercTransfToStr(const t: tpPercTransf): string;
function eSStrToTpPercTransf(var ok: boolean; const s: string): tpPercTransf;

function TpIndRemunToStr(const t: tpIndRemun): string;
function StrToTpIndRemun(var ok: boolean; const s: string): tpIndRemun;

function eSTpTpCCPToStr(const t: tpTpCCP): string;
function eSStrToTpTpCCP(var ok: boolean; const s: string): tpTpCCP;

function IntToTpProf(codCateg : Integer): tpTpProf;

function eSTpContrS2500ToStr(const t: tpTpContrS2500): string;
function eSStrToTpContrS2500(var ok: boolean; const s: string): tpTpContrS2500;

function eSTpMtvDesligTSVToStr(const t: tpMtvDesligTSV ): string;
function eSStrToTpMtvDesligTSV(var ok: boolean; const s: string): tpMtvDesligTSV;

function eSTpTpRepercProcToStr(const t: tpRepercProc): string;
function eSStrToTpRepercProc(var ok: boolean; const s: string): tpRepercProc;

function eSTpTpIndRepercToStr(const t: tpIndReperc): string;
function eSStrToTpIndReperc(var ok: boolean; const s: string): tpIndReperc;

function eSTpTpOrigemProcToStr(const t: tpOrigemProc): string;
function eSStrToTpOrigemProc(var ok: boolean; const s: string): tpOrigemProc;

function eSTpRelDepToStr(const t: tpRelDep): string;
function eSStrToTpRelDep(var ok: boolean; const s: string): tpRelDep;

function eStpIndTpDeduToStr(const t: tpIndTpDedu): string;
function eSStrTotpIndTpDedu(var ok: boolean; const s: string): tpIndTpDedu;

function eStpTpPrevToStr(const t: tpTpPrev): string;
function eSStrTotpTpPrev(var ok: boolean; const s: string): tpTpPrev;

function eStpIndAprendToStr(const t: tpIndAprend): string;
function eSStrTotpIndAprend(var ok: boolean; const s: string): tpIndAprend;

function eStpTpIndTpDeducaoToStr(const t: tpIndTpDeducao): string;
function eSStrTotpIndTpDeducao(var ok: boolean; const s: string): tpIndTpDeducao;

function eStpTpIndTpDeducaoTToStr(const t: tpIndTpDeducaoT): string;
function eSStrTotpIndTpDeducaoT(var ok: boolean; const s: string): tpIndTpDeducaoT;

implementation

uses
  pcnConversao, typinfo, ACBrBase,
  ACBrUtil.Strings;

const

  TUFString           : array[0..27] of String = ('AC','AL','AP','AM','BA','CE','DF','ES','GO',
                                                  'MA','MT','MS','MG','PA','PB','PR','PE','PI',
                                                  'RJ','RN','RS','RO','RR','SC','SP','SE','TO', '');

  TSiglasMinString    : array[0..4] of string = ('CNAS','MEC','MS','MDS','LEI');

  TIndicativoContratacaoPCD : array[0..3] of string = ('0', '1', '2', '9' );

  TMotivoAlteracaoCargoFuncao: array[0..3] of string = ('1', '2', '3', '9');

  TMotivoAfastamento: array[0..32] of string = ('01', '03', '05', '06', '07',
                                                '08', '10', '11', '12', '13',
                                                '14', '15', '16', '17', '18',
                                                '19', '20', '21', '22', '23',
                                                '24', '25', '26', '27', '28',
                                                '29', '30', '31', '33', '34',
                                                '35', '37', '38');

  TGenericosString0_1 : array[0..1] of string = ('0','1' );
  TGenericosString0_2 : array[0..2] of string = ('0','1','2' );
  TGenericosString0_3 : array[0..3] of string = ('0','1','2','3' );
  TGenericosString0_4 : array[0..4] of string = ('0','1','2','3','4' );
  TGenericosString0_5 : array[0..5] of string = ('0','1','2','3','4','5' );
  TGenericosString0_6 : array[0..6] of string = ('0','1','2','3','4','5','6' );
  TGenericosString0_7 : array[0..7] of string = ('0','1','2','3','4','5','6', '7' );

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

  TGenericosStringA_G : array[0..6] of string = ('A','B','C','D','E','F','G');

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

function TipoEventoToSchemaeSocial(const t: TTipoEvento; AVersaoeSocial: TVersaoeSocial): TeSocialSchema;
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
     teS1210: Result := schevtPgtos;
     teS1220: Result := schevtInfoIR;
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
     teS2231: Result := schevtCessao;
     teS2240: Result := schevtExpRisco;
     teS2245: Result := schevtTreiCap;
     teS2250: Result := schevtAvPrevio;
     teS2260: Result := schevtConvInterm;
     teS2298: Result := schevtReintegr;
     teS2299: Result := schevtDeslig;
     teS2300: Result := schevtTSVInicio;
     teS2306: Result := schevtTSVAltContr;
     teS2399: Result := schevtTSVTermino;
     teS2400: Result := schevtCdBenefIn;
     teS2405: Result := schevtCdBenefAlt;
     teS2410: Result := schevtCdBenIn;
     teS2416: Result := schevtCdBenAlt;
     teS2418: Result := schevtReativBen;
     teS2420: Result := schevtCdBenTerm;
     teS2500: Result := schevtProcTrab;
     teS2501: Result := schevtContProc;
     teS3000: Result := schevtExclusao;
     teS3500: Result := schevtExcProcTrab;

      // Schemas alterados conforme a versão do eSocial
     teS1207:
     begin
      Result := schevtBenPrRP; // veS01_00_00

       if (AVersaoeSocial < veS01_00_00) then
        Result := schevtCdBenPrRP;
     end;
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

function TipoEventoToStrEvento(const t: TTipoEvento; AVersaoeSocial: TVersaoeSocial): string;
begin
 Result := GetMatrixEventoInfo(meiTipoEvento, GetEnumName(TypeInfo(TTipoEvento), Integer(t)), AVersaoeSocial).EventoString;
end;

function eSProcEmiToStr(const t: TpProcEmi ): string;
begin
  result := EnumeradoToStr2(t, [ '1', '2', '3', '4', '8', '9', '22' ] );
end;

function eSStrToProcEmi(var ok: boolean; const s: string): TpProcEmi;
begin
  result := TpProcEmi( StrToEnumerado2(ok, s, [ '1', '2', '3', '4', '8', '9', '22' ]) );
end;

function eSTpInscricaoToStr(const t:tpTpInsc ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_6 );
end;

function eSStrToTpInscricao(var ok: boolean; const s: string): tpTpInsc;
begin
  result := tpTpInsc( StrToEnumerado2(ok , s, TGenericosString1_6 ) );
end;

function eStpPlanRPToStr(const t: tpPlanRP): string;
begin
  result := EnumeradoToStr2(t, [ '0', '1', '2', '3' ] );
end;

function eSStrTotpPlanRP(var ok: Boolean; const s: string): tpPlanRP;
begin
  result := tpPlanRP( StrToEnumerado2(ok, s, [ '0', '1', '2', '3' ]) );
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

function eSTpRegPrevFacultativoToStr(const t: tpTpRegPrevFacultativo ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_3);
end;

function eSStrTotpRegPrevFacultativo(var ok: boolean; const s: string): tpTpRegPrevFacultativo;
begin
  result := tpTpRegPrevFacultativo( StrToEnumerado2(ok , s, TGenericosString0_3 ) );
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
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '99' ] );
end;

function eSStrToTpProv(var ok: boolean; const s: string): tpTpProv;
begin
  result := tpTpProv( StrToEnumerado2(ok , s, [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '99' ] ) );
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
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '6', '7', '9' ] );
end;

function eSStrToTpJornada(var ok: boolean; const s: string): tpTpJornada;
begin
  result := tpTpJornada( StrToEnumerado2(ok , s, [ '1', '2', '3', '4', '5', '6', '7', '9' ] ) );
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
  result := EnumeradoToStr2(t, ['0','1', '2', '3', '4', '9']);
end;

function eSIdeOCToStrEX(const t: tpIdeOC): string;
begin
  Result := TtpIdeOCArrayStrings[t];
end;

function eSStrToIdeOC(var ok: boolean; const s: string): tpIdeOC;
begin
  result := tpIdeOC( StrToEnumerado2(ok, s, ['0', '1', '2', '3', '4', '9'] ) );
end;

function esStrToIdeOCEX(const s: string): tpIdeOC;
var
  idx: tpIdeOC;
begin
  for idx:= Low(TtpIdeOCArrayStrings) to High(TtpIdeOCArrayStrings)do
  begin
    if(TtpIdeOCArrayStrings[idx] = s)then
    begin
      Result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para tpIdeOC: %s', [s]);
end;

function eSIndSubstPatrStr(const t: tpIndSubstPatr ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_2 );
end;

function eSStrToIndSubstPatr(var ok: boolean; const s: string): tpIndSubstPatr;
begin
  result := tpIndSubstPatr( StrToEnumerado2(ok , s, TGenericosString0_2 ) );
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
  if Trim(s) = '' then
    result := tpNao
  else
    result := tpSimNao(StrToEnumerado2(ok , s, TSimNaoString));
end;

function eSSimNaoFacultativoToStr(const t: tpSimNaoFacultativo ): string;
begin
  result := EnumeradoToStr2(t,TSimNaoFacultativoString );
end;

function eSStrToSimNaoFacultativo(var ok: boolean; const s: string): tpSimNaoFacultativo;
begin
  result := tpSimNaoFacultativo( StrToEnumerado2(ok , s, TSimNaoFacultativoString ) );
end;

function eSSimFacultativoToStr(const t: tpSimFacultativo ): string;
begin
  result := EnumeradoToStr2(t,TSimFacultativoString );
end;

function eSStrToSimFacultativo(var ok: boolean; const s: string): tpSimFacultativo;
begin
  result := tpSimFacultativo( StrToEnumerado2(ok , s, TSimFacultativoString ) );
end;

function eStpTpInscAmbTabToStr(const t: tpTpInscAmbTab ): string;
begin
  result := EnumeradoToStr2(t, tpTpInscAmbTabArrayStrings );
end;

function eSStrTotpTpInscAmbTab(var ok: boolean; const s: string): tpTpInscAmbTab;
begin
  result := tpTpInscAmbTab( StrToEnumerado2(ok , s,tpTpInscAmbTabArrayStrings ));
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
  if t = icpNenhum then
    result := ''
  else
    result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eSStrToIndOpcCP(var ok: boolean; const s: string): TpIndOpcCP;
begin
  if Trim(s) = '' then
    result := icpNenhum
  else
    result := TpIndOpcCP(StrToEnumerado2(ok , s, TGenericosString1_2));
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

function eSIndAcordoIsencaoMultaToStr(const t:tpIndAcordoIsencaoMulta ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString0_1 );
end;

function eSStrToIndAcordoIsencaoMulta(var ok: boolean; const s: string): TpIndAcordoIsencaoMulta;
begin
  result := TpIndAcordoIsencaoMulta( StrToEnumerado2(ok , s,TGenericosString0_1 ) );
end;
{
function eSufToStr(const t:tpuf ): string;
begin
  result := EnumeradoToStr2(t,TUFString );
end;

function eSStrTouf(var ok: boolean; const s: string): Tpuf;
begin
  result := Tpuf( StrToEnumerado2(ok , s,TUFString ) );
end;
}

function eSCodIncCPToStr(const t:tpCodIncCP ): string;
begin
  result := EnumeradoToStr2(t, tpCodIncCPArrayStrings);
end;

function eSStrToCodIncCP(var ok: boolean; const s: string): tpCodIncCP;
begin
  result := tpCodIncCP( StrToEnumerado2(ok , s, tpCodIncCPArrayStrings));
end;

function eSTpLocalToStr(const t: tpTpLocal ): string;
begin
  result := EnumeradoToStr2(t,[ '1', '2', '3', '4', '5', '6', '9' ] );
end;

function eSStrToTpLocal(var ok: boolean; const s: string): tpTpLocal;
begin
  result := tpTpLocal( StrToEnumerado2(ok , s,[ '1', '2', '3', '4', '5', '6', '9' ] ));
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
  result := EnumeradoToStr2(t, tpCodIncIRRFArrayStrings);
end;

function eSStrToCodIncIRRF(var ok: boolean; const s: string): tpCodIncIRRF;
begin
  result := tpCodIncIRRF( StrToEnumerado2(ok , s, tpCodIncIRRFArrayStrings));
end;

function eSCodIncFGTSToStr(const t:tpCodIncFGTS ): string;
begin
  result := EnumeradoToStr2(t, tpCodIncFGTSArrayStrings);
end;

function eSStrToCodIncFGTS(var ok: boolean; const s: string): tpCodIncFGTS;
begin
  result := tpCodIncFGTS( StrToEnumerado2(ok , s, tpCodIncFGTSArrayStrings));
end;

function eSCodIncCPRPToStr(const t:tpCodIncCPRP ): string;
begin
  result := EnumeradoToStr2(t,tpCodIncCPRPArrayStrings );
end;

function eSStrToCodIncCPRP(var ok: boolean; const s: string): tpCodIncCPRP;
begin
  result := tpCodIncCPRP( StrToEnumerado2(ok , s, tpCodIncCPRPArrayStrings ));
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

function eSIndSubstPatronalObraToStr(const t:tpIndSubstPatronalObra ): string;
begin
  if t = ispVazio then
    result := ''
  else
    result := EnumeradoToStr2(t, TGenericosString0_2);
end;

function eSStrToIndSubstPatronalObra(var ok: boolean; const s: string): TpIndSubstPatronalObra;
begin
  if Trim(s) = '' then
  begin
    result := ispVazio;
    ok := true;
  end else
  begin
    result := TpIndSubstPatronalObra(StrToEnumerado2(ok , s, TGenericosString0_2));
  end;
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

function eSTpIndApurIRToStr(const t: tpIndApurIR ): string;
begin
  result := EnumeradoToStr2(t, ['', '0', '1']);
end;

function eSStrToTpIndApurIR(var ok: boolean; const s: string): tpIndApurIR;
begin
  result := tpIndApurIR( StrToEnumerado2(ok , s, ['', '0', '1']));
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
  result := EnumeradoToStr2(t,TGenericosString1_7  );
end;

function eSStrToTpAdmissao(var ok: boolean; const s: string): tpTpAdmissao;
begin
  result := tpTpAdmissao( StrToEnumerado2(ok , s,TGenericosString1_7 ));
end;

function eSIndNIFToStr(const t:tpIndNIF ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_3 );
end;

function eSStrToIndNIF(var ok: boolean; const s: string): TpIndNIF;
begin
  result := TpIndNIF( StrToEnumerado2(ok , s,TGenericosString1_3 ));
end;

function eSTpProcRRAToStr(const t:tpTpProcRRA ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToTpProcRRA(var ok: boolean; const s: string): TpTpProcRRA;
begin
  result := TpTpProcRRA( StrToEnumerado2(ok , s,TGenericosString1_2 ));
end;

function eStpTpProcRetToStr(const t:tpTpProcRet ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eSStrTotpTpProcRet(var ok: boolean; const s: string): tpTpProcRet;
begin
  result := tpTpProcRet( StrToEnumerado2(ok, s, TGenericosString1_2));
end;

function eSTpInscAdvogadoToStr(const t:tpTpInscAdvogado ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_2 );
end;

function eSStrToTpInscAdvogado(var ok: boolean; const s: string): TpTpInscAdvogado;
begin
  result := TpTpInscAdvogado( StrToEnumerado2(ok , s,TGenericosString1_2 ));
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
  result := EnumeradoToStr2(t,['01', '02', '03', '04', '06', '07', '09', '10', '11', '12', '99', ''] );
end;

function eSStrToTpDep(var ok: Boolean; const s: string): tpTpDep;
begin
  result := tpTpDep( StrToEnumerado2(ok , s, ['01', '02', '03', '04', '06', '07', '09', '10', '11', '12', '99', ''] ));
end;

function eSTpRubrToStr(const t: tpTpRubr): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_4);
end;

function eSStrToTpRubr(var ok: Boolean; const s: string): tpTpRubr;
begin
  result := tpTpRubr( StrToEnumerado2(ok , s, TGenericosString1_4 ));
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
  result := EnumeradoToStr2(t, TGenericosStringA_G);
end;

function eSStrToTpAcConv(var ok: Boolean; const s: string): tpTpAcConv;
begin
  result := tpTpAcConv( StrToEnumerado2(ok , s, TGenericosStringA_G ));
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
  if t <> tcVazio then
    result := EnumeradoToStr2(t, TGenericosString0_3)
  else
    result := '';
end;

function eSStrTotpCaepf(var ok: boolean; const s: string): tpCaepf;
begin
  if Trim(s) <> '' then
    result := tpCaepf(StrToEnumerado2(ok, s, TGenericosString0_3))
  else
    result := tcVazio;
end;

function eStpTpPgtoToStr(const t: tpTpPgto): string;
begin
  result := EnumeradoToStr2(t,['1', '2', '3', '4', '5'] );
end;

function eSStrTotpTpPgto(var ok:Boolean; const s: string): tpTpPgto;
begin
  result := tpTpPgto(StrToEnumerado2(ok, s, ['1', '2', '3', '4', '5']));
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

function eSTpPensaoAlimToStrEx(const t: tpPensaoAlim): String;
begin
  Result := tpPensaoAlimArrayStrings[t];
end;

function eSStrToTpPensaoAlimEx(const s: string): tpPensaoAlim;
var
  idx: tpPensaoAlim;
begin
  for idx := Low(tpPensaoAlimArrayStrings) to High(tpPensaoAlimArrayStrings)do
  begin
    if tpPensaoAlimArrayStrings[idx] = s then
    begin
      Result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para tpPensaoAlim: %s', [s]);
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
  try
    Result := StrToVersaoeSocialEX(s);
  except
    On E: EACBrException do
    begin
      Result := low(TVersaoeSocial);
      ok :=  False;
    end;
  end;
end;

function VersaoeSocialToStr(const t: TVersaoeSocial): String;
begin
  result := VersaoeSocialToStrSchemas(t);
end;

function VersaoeSocialToStrEx(const t: TVersaoeSocial): String;
begin
  result := TVersaoeSocialArrayStrings[t];
end;

function StrToVersaoeSocialEX(const s: String): TVersaoeSocial;
var
  idx: TVersaoeSocial;
begin
  for idx := Low(TVersaoeSocialArrayStrings) to High(TVersaoeSocialArrayStrings) do
  begin
    if TVersaoeSocialArrayStrings[idx] = s then
    begin
      Result := idx;
      Exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TVersaoeSocial: %s', [s]);
end;

function VersaoeSocialToStrSchemas(const t: TVersaoeSocial): String;
begin
  result := TVersaoeSocialSchemasArrayStrings[t];
end;

function StrToVersaoeSocialSchemas(const s: String): TVersaoeSocial;
var
  idx: TVersaoeSocial;
begin
  for idx := Low(TVersaoeSocialSchemasArrayStrings) to High(TVersaoeSocialSchemasArrayStrings) do
  begin
    if TVersaoeSocialSchemasArrayStrings[idx] = s then
    begin
      Result := idx;
      Exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TVersaoeSocial: %s', [s]);
end;

function VersaoeSocialToDbl(const t: TVersaoeSocial): Real;
begin
  case t of
    ve02_04_01:  result := 2.0401;
    ve02_04_02:  result := 2.0402;
    ve02_05_00:  result := 2.0500;
    veS01_00_00: result := 10.0000;
    veS01_01_00: result := 10.1000;
    veS01_02_00: result := 10.2000;
  else
    result := 0;
  end;
end;

function DblToVersaoeSocial(out ok: Boolean; const d: Real): TVersaoeSocial;
begin
  ok := True;

  if (d = 2.0401)  then
    result := ve02_04_01
  else if (d = 2.0402)  then
    result := ve02_04_02
  else if (d = 2.0500)  then
    result := ve02_05_00
  else if (d = 10.0000)  then
    result := veS01_00_00
  else if (d = 10.1000)  then
    result := veS01_01_00
  else if (d = 10.2000)  then
    result := veS01_02_00
  else
  begin
    result := veS01_00_00;
    ok := False;
  end;
end;

function VersaoeSocialToDblEX(const t: TVersaoeSocial): Real;
begin
  result := TVersaoeSocialArrayReals[t];
end;

function DblToVersaoeSocialEX(const d: Real): TVersaoeSocial;
var
  idx: TVersaoeSocial;
begin
  for idx := Low(TVersaoeSocialArrayReals) to High(TVersaoeSocialArrayReals) do
  begin
    if TVersaoeSocialArrayReals[idx] = d then
    begin
      Result := idx;
      Exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor "Real" inválido para TVersaoeSocial: %f', [d]);
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
begin
  result := TTipoEvento( StrToEnumerado2(ok , s, EventoStringNova ) );
end;

function StringINIToTipoEvento(out ok: boolean; const s: string; AVersaoeSocial: TVersaoeSocial): TTipoEvento;
var
  intSecondPos     : Integer;
  strSearchValue   : string;
  intFirstPos      : integer;
begin
  ok               := False;
  Result           := teNaoIdentificado;
  strSearchValue   := EmptyStr;
  try
    intSecondPos   := PosAt(']', s, 1);
    if(intSecondPos > 0)then
    begin
      intFirstPos  := PosAt('[', s, 1);

      if(intFirstPos > 0)then
        strSearchValue := Trim(Copy(s, intFirstPos + 1, intSecondPos - intFirstPos - 1));
    end;

    if(strSearchValue <> '')then
    begin
      ok := True;
      Result := GetMatrixEventoInfo(meiEventoString, strSearchValue, AVersaoESocial).TipoEvento;
    end;
  except
    on E:Exception do
      ok := False;
  end;
end;

function StringXMLToTipoEvento(out ok: boolean; const s: string; AVersaoeSocial: TVersaoeSocial): TTipoEvento;
var
  intLastPos     : Integer;
  strSearchValue : string;
  intChar        : integer;
begin
  ok             := False;
  Result         := teNaoIdentificado;
  strSearchValue := EmptyStr;

  try
    intLastPos := PosLast('Id="',s);

    if intLastPos > 0 then
    begin
      intChar := PosLast('<', Copy(s, 0, intLastPos -1));

      if intChar > 0 then
        strSearchValue := Trim(Copy(s, intChar + 1, intLastPos - intChar - 1));
    end;

    if strSearchValue <> EmptyStr then
    begin
      ok := True;
      Result := GetMatrixEventoInfo(meiEventoString, strSearchValue, AVersaoeSocial).TipoEvento;
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

function tpTmpResidToStr(const t: tpTmpResid ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_2 );
end;

function StrTotpTmpResid(var ok: boolean; const s: string): tpTmpResid;
begin
  result := tpTmpResid( StrToEnumerado2(ok, s, TGenericosString0_2) );
end;

function tpCondIngToStr(const t: tpCondIng ): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_7 );
end;

function StrTotpCondIng(var ok: boolean; const s: string): tpCondIng;
begin
  result := tpCondIng( StrToEnumerado2(ok, s, TGenericosString0_7) );
end;

function eSTpIndSitBenefToStr(const t: tpIndSitBenef): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_3);
end;

function eSStrToTpIndSitBenef(var ok: boolean; const s: string): tpIndSitBenef;
begin
  result := tpIndSitBenef(StrToEnumerado2(ok, s, TGenericosString0_3) );
end;

function eStpTpPenMorteToStr(const t: tpTpPenMorte): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eStpTpPenMorteToStrEX(const t: tpTpPenMorte):string;
begin
  result := tpTpPenMorteArrayStrings[t];
end;

function eSStrTotpTpPenMorte(var ok: boolean; const s: string): tpTpPenMorte;
begin
  result := tpTpPenMorte(StrToEnumerado2(ok, s, TGenericosString1_2) );
end;

function eSStrTotpTpPenMorteEX(const s: string): tpTpPenMorte;
var
  idx: tpTpPenMorte;
begin
  for idx := Low(tpTpPenMorteArrayStrings) to High(tpTpPenMorteArrayStrings)do
  begin
    if(tpTpPenMorteArrayStrings[idx] = s)then
    begin
      result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para tpTpPenMorte: %s', [s]);
end;

function eStptpMotCessBenefToStr(const t: tpMotCessBenef): string;
begin
  result := EnumeradoToStr2(t, TGenericosString01_11);
end;

function eSStrTotpMotCessBenef(var ok: boolean; const s: string): tpMotCessBenef;
begin
  result := tpMotCessBenef(StrToEnumerado2(ok, s, TGenericosString01_11) );
end;

function eStpTpMotCessBenefToStrEX(const t: tpMotCessBenef): string;
begin
  Result := tpMotCessBenefArrayStrings[t];
end;

function eSStrToTpMotCessBenefEX(const s: string): tpMotCessBenef;
var
  idx: tpMotCessBenef;
begin
  for idx := Low(tpMotCessBenefArrayStrings) to High(tpMotCessBenefArrayStrings)do
  begin
    if(tpMotCessBenefArrayStrings[idx] = s)then
    begin
      Result := idx;
      Exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para tpMotCessBenef: %s', [s]);
end;

function eStpTpMtvSuspensaoToStr(const t: tpMtvSuspensao): string;
begin
  result := EnumeradoToStr2(t,['00', '01', '99'] );
end;

function eSStrToTpMtvSuspensao(var ok: boolean; const s: string): tpMtvSuspensao;
begin
  result := StrToEnumerado(ok, s, ['00', '01', '99'],
                           [mtvNada,
                            mtvSuspensaoPorNaoRecadastramento,
                            mtvOutrosMotivosDeSuspensao]);
end;

function TpIndRemunToStr(const t: tpIndRemun): string;
begin
  result := EnumeradoToStr2(t, TGenericosString0_3);
end;

function StrToTpIndRemun(var ok: boolean; const s: string): tpIndRemun;
begin
  result := tpIndRemun(StrToEnumerado2(ok, s, TGenericosString0_3));
end;

function eSTpTpCCPToStr(const t: tpTpCCP): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToTpTpCCP(var ok: boolean; const s: string): tpTpCCP;
begin
  result := tpTpCCP(StrToEnumerado2(ok, s, TGenericosString1_3));
end;

function eSTpPercTransfToStr(const t: tpPercTransf): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_5);
end;

function eSStrToTpPercTransf(var ok: boolean; const s: string): tpPercTransf;
begin
  result := tpPercTransf(StrToEnumerado2(ok, s, TGenericosString1_5));
end;

function eSTpContrS2500ToStr(const t: tpTpContrS2500 ): string;
begin
  result := EnumeradoToStr2(t,TGenericosString1_9);
end;

function eSStrToTpContrS2500(var ok: boolean; const s: string): tpTpContrS2500;
begin
  result := tpTpContrS2500(StrToEnumerado2(ok , s, TGenericosString1_9));
end;

function eStpMtvDesligTSVToStr(const t: tpMtvDesligTSV ): string;
begin
  result := EnumeradoToStr2(t,['00', '01', '02', '03', '04', '05', '06', '99']);
end;

function eSStrToTpMtvDesligTSV(var ok: boolean; const s: string): tpMtvDesligTSV;
begin
  result := tpMtvDesligTSV(StrToEnumerado2(ok, s, ['00', '01', '02', '03', '04', '05', '06', '99']));
end;

function eSTpTpRepercProcToStr(const t: tpRepercProc): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eSStrToTpRepercProc(var ok: boolean; const s: string): tpRepercProc;
begin
  result := tpRepercProc(StrToEnumerado2(ok , s, TGenericosString1_2));
end;

function eSTpTpIndRepercToStr(const t: tpIndReperc): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_3);
end;

function eSStrToTpIndReperc(var ok: boolean; const s: string): tpIndReperc;
begin
  result := tpIndReperc(StrToEnumerado2(ok , s, TGenericosString1_3));
end;

function eSTpTpOrigemProcToStr(const t: tpOrigemProc): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eSStrToTpOrigemProc(var ok: boolean; const s: string): tpOrigemProc;
begin
  result := tpOrigemProc(StrToEnumerado2(ok , s, TGenericosString1_2));
end;

function eSTpRelDepToStr(const t: tpRelDep): string;
begin
  result := EnumeradoToStr2(t,['01', '02', '03', '04', '05', '06', '07', '99']);
end;

function eSStrToTpRelDep(var ok: boolean; const s: string): tpRelDep;
begin
  result := tpRelDep(StrToEnumerado2(ok , s, ['01', '02', '03', '04', '05', '06', '07', '99']));
end;

function eStpIndTpDeduToStr(const t: tpIndTpDedu): string;
begin
  result := EnumeradoToStr2(t,['1', '2', '3', '4', '5', '7']);
end;

function eSStrTotpIndTpDedu(var ok: boolean; const s: string): tpIndTpDedu;
begin
  result := tpIndTpDedu(StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5', '7']));
end;

function eStpTpPrevToStr(const t: tpTpPrev): string;
begin
  result := EnumeradoToStr2(t,['1', '2', '3']);
end;

function eSStrTotpTpPrev(var ok: boolean; const s: string): tpTpPrev;
begin
  result := tpTpPrev(StrToEnumerado2(ok , s, ['1', '2', '3']));
end;

function eStpIndAprendToStr(const t: tpIndAprend): string;
begin
  result := EnumeradoToStr2(t, TGenericosString1_2);
end;

function eSStrTotpIndAprend(var ok: boolean; const s: string): tpIndAprend;
begin
  result := tpIndAprend(StrToEnumerado2(ok , s, TGenericosString1_2));
end;

function eStpTpIndTpDeducaoToStr(const t: tpIndTpDeducao): string;
begin
  result := EnumeradoToStr2(t,['1', '2', '3', '4', '5', '7']);
end;

function eSStrTotpIndTpDeducao(var ok: boolean; const s: string): tpIndTpDeducao;
begin
  result := tpIndTpDeducao(StrToEnumerado2(ok , s, ['1', '2', '3', '4', '5', '7']));
end;

function eStpTpIndTpDeducaoTToStr(const t: tpIndTpDeducaoT): string;
begin
  result := EnumeradoToStr2(t,['1', '5', '7']);
end;

function eSStrTotpIndTpDeducaoT(var ok: boolean; const s: string): tpIndTpDeducaoT;
begin
  result := tpIndTpDeducaoT(StrToEnumerado2(ok , s, ['1', '5', '7']));
end;

function IntToTpProf(codCateg : Integer): tpTpProf;
var
        ix : integer;
Begin
  result := ttpProfissionalEmpregado;
  for ix := low(TTrabalhadorSemVinculo) to High(TTrabalhadorSemVinculo) do
    if TTrabalhadorSemVinculo[ix] = codCateg then
    Begin
      result := ttpProfissionalSemVinculo;
      break;
    End;
End;

function GetMatrixEventoInfo(AInfoEventoMatrix: TMatrixEventoInfo; ASearchValue: string; AVersaoeSocial: TVersaoeSocial): TRecMatrixEventoInfo;
var
  recMatrixEventoInfo: TRecMatrixEventoInfo;
  bolMatch           : Boolean;
  i: Integer;
begin
  bolMatch := False;

  for i := 1 to Length(__ARRAY_MATRIX_EVENTO_INFO) do
  begin
    recMatrixEventoInfo := __ARRAY_MATRIX_EVENTO_INFO[i];
    case AInfoEventoMatrix of
      meiTipoEvento:            bolMatch := recMatrixEventoInfo.TipoEvento = TTipoEvento(GetEnumValue(TypeInfo(TTipoEvento), ASearchValue));
      meiTipoEventoString:      bolMatch := recMatrixEventoInfo.TipoEventoString = ASearchValue;
      meiVersao:                bolMatch := recMatrixEventoInfo.Versao = ASearchValue;
      meiEventoString:          bolMatch := recMatrixEventoInfo.EventoString = ASearchValue;
      meiSchema:                bolMatch := recMatrixEventoInfo.Schema = TeSocialSchema(GetEnumValue(TypeInfo(TeSocialSchema), ASearchValue));
      meiStrEventoToTipoEvento: bolMatch := recMatrixEventoInfo.StrEventoToTipoEvento = ASearchValue;
      meiObservacao:            bolMatch := recMatrixEventoInfo.StrEventoToTipoEvento = ASearchValue;
    end;

    if bolMatch then
      if AVersaoeSocial = recMatrixEventoInfo.VersaoeSocial then
        Break;
  end;

  if not bolMatch then
  begin
    recMatrixEventoInfo.TipoEvento            := teErro;
    recMatrixEventoInfo.TipoEventoString      := EmptyStr;
    recMatrixEventoInfo.VersaoeSocial         := veS01_00_00;
    recMatrixEventoInfo.Versao                := EmptyStr;
    recMatrixEventoInfo.EventoString          := EmptyStr;
    recMatrixEventoInfo.Schema                := schErro;
    recMatrixEventoInfo.StrEventoToTipoEvento := EmptyStr;
    recMatrixEventoInfo.Observacao            := EmptyStr;
  end;

  Result := recMatrixEventoInfo;
end;

end.
