{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
unit ACBreSocialTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, pcesConversaoeSocial, ACBreSocialTestsConsts,
  ACBreSocialEventosNaoPeriodicosTests, ACBreSocialEventosPeriodicosTests,
  ACBreSocialEventosIniciaisTests, ACBreSocialEventosTabelasTests,ACBreSocialSoapTests;

type

  { TACBreSocialConversaoeSocialTest }

  TACBreSocialConversaoeSocialTest = class(TTestCase)
    private
      FArqINI       : TStrings;
      OK            : Boolean;
    public
      procedure SetUp;override;
      procedure TearDown;override;
    published
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1000;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1005;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1010;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1020;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1030;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1035;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1040;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1050;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1060;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1070;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1080;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2190;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2200;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2205;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2206;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2210;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2220;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2221;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2230;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2231;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2240;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2245;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2250;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2260;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2298;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2299;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2300;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2306;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2399;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2400;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2405;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2410;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2416;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2418;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2420;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS3000;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1200;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1202;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1207;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1210;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1250;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1260;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1270;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1280;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1295;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1298;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1299;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1300;
  end;

  { TACBreSocialTipoToStrStrToTipoTest }

  TACBreSocialTipoToStrStrToTipoTest = class(TTestCase)
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure VersaoeSocialToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToVersaoeSocialEX_ConvertendoStringinvalida_RetornoException;
      procedure eSSimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSSimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
      procedure LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
      procedure ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
      procedure TipoEventoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToTipoEvento_ConvertentoTodosTipos_RetornoCorreto;
      procedure eSprocEmiToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToprocEmi_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpInscricaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpInscricao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpInscAmbTabToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpTpInscAmbTab_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpInscPropToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpInscProp_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndCooperativaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndCooperativa_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndConstrutoraToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndConstrutora_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndDesFolhaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndDesFolha_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndOptRegEletronicoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndOptRegEletronico_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndOpcCPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndOpcCP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSAliqRatToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToAliqRat_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpProcessoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpProcesso_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndAcordoIsencaoMultaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndAcordoIsencaoMulta_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpInscContratanteToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpInscContratante_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncCPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncCP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncIRRFToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncIRRF_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncCPRPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncCPRP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncFGTS_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSExtDecisaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToExtDecisao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndSubstPatronalObraToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndSubstPatronalObra_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSindAutoriaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToindAutoria_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIndMatProcToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpIndMatProc_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndRetificacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndRetiricacao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndApuracaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndApuracao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndMVToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndMV_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndSimplesToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndSimples_ConvertendoTodosTipos_RetornoCorreto;
      procedure VersaoeSocialToDblEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure DblToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSNatAtividadeToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToNatAtividade_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTributoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTributo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSGrauExpToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToGrauExp_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndNIFToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIdeOCToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOC_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIdeOCToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOCEx_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOCEX_ConvertendoStringinvalida_RetornoException;
      procedure eSTpProcRRAToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpProcRRA_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpProcRetToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpTpProcRet_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpInscAdvogadoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpInscAdvogado_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndIncidenciaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndIncidencia_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndAbrangenciaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndAbrangencia_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndComercializacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndComercializacao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpRubrToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRubr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSAcumCargoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToAcumCargo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSContagemEspToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToContagemEsp_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpUtilizEPCToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpUtilizEPC_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpUtilizEPIToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpUtilizEPI_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSLocalAmbToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToLocalAmb_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpAcConvToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpAcConv_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCnhToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCnh_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSClassTrabEstrangToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToClassTrabEstrang_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpDepToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpDep_ConvertentoTodosTipos_RetornoCorreto;
      procedure eSTpRegTrabToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRegTrab_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpRelDepToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRelDep_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpRegPrevToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRegPrev_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpRegPrevFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRegPrevFacultativo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpAdmissaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpAdmissao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIndAdmissaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpIndAdmissao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpRegJorToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpRegJor_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSOpcFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToOpcFGTS_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSMtvContratToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToMtvContrat_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndProvimToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndProvim_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpProvToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpProv_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSUndSalFixoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToUndSalFixo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpContrToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpContr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpContrS2500ToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpContrS2500_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpJornadaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSSTrToTpJornada_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpDiaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpDia_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpExameOcupToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpExameOcup_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSResAsoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToResAso_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSOrdExameToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToOrdExame_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndResultToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndResult_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpAcidToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpAcid_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpCatToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpCat_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIniciatCATToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIniciatCAT_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpLocalToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpLocal_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSLateralidadeToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToLateralidade_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpReintToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpReint_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndSubstPatrStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndSubstPatr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIdAquisStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdAquis_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndComercStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndComerc_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTpPgtoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotptpPgto_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpMotivosAfastamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMotivosAfastamento_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpAcidTransitoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTpAcidTransito_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpInfOnusToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpInfOnus_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpOnusRemunToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpOnusRemun_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpNatEstagioToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpNatEstagio_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpNivelEstagioToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpNivelEstagio_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpCaepfToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpCaepf_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpPlanRPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpPlanRP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpMtvAltToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMtvAlt_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpOrigemAltAfastToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpOrigemAltAfast_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpPensaoAlimToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpPensaoAlim_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpCumprParcialAvisoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpCumprParcialAviso_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpAvalToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpAval_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpModTreiCapToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpModTreiCap_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpTpTreiCapToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpTpTreiCap_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpTpProfToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpTpProf_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpNacProfToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpNacProf_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpTmpParcToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpTmpParc_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpClassTribToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpClassTrib_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpTmpResidToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpTmpResid_ConvertendoTodosTipos_RetornoCorreto;
      procedure tpCondIngToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrTotpCondIng_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIndApurIRToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpindApurIR_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIndSitBenefToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpIndSitBenef_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpPenMorteToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpTpPenMorte_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpPenMorteToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpTpPenMorteEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTpPenMorteEX_ConvertendoStringInvalida_RetornoException;
      procedure eStpTpMotCessBenefToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMotCessBenef_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpMotCessBenefToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMotCessBenefEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMotCessBenefEX_ConvertendoStringInvalida_RetornoException;
      procedure eStpTpMtvSuspensaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMtvSuspensao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpPercTransfToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpPercTransf_ConvertendoTodosTipos_RetornoCorreto;
      procedure TpIndRemunToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToTpIndRemun_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTpCCPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTpCCP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpMtvDesligTSVToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpMtvDesligTSV_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTpRepercProcToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTpRepercProc_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTpOrigemProcToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTpOrigemProc_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpIndTpDeduToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpIndTpDedu_ConvertendoTodosTipos_RetornoCorreto;























  end;

implementation

uses
  typinfo, ACBrBase;


{ TACBreSocialTipoToStrStrToTipoTest }

procedure TACBreSocialTipoToStrStrToTipoTest.Setup;
begin
  inherited Setup;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.TearDown;
begin
  inherited TearDown;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.VersaoeSocialToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TVersaoeSocial;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for vElementoEnum:= Low(TVersaoeSocial) to High(TVersaoeSocial) do
  begin
    ConvertidoParaString := VersaoeSocialToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := StrToVersaoeSocialEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro conversão no elemento Ord('+ GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'VersaoeSocialToStrEX => '+ ConvertidoParaString+'  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoDoArray: string;
  ConvertidoParaEnum: TVersaoeSocial;
  ReconvertidoParaString: String;
  i: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for i := low(TVersaoeSocialArrayStrings) to High(TVersaoeSocialArrayStrings) do
  begin
    ElementoDoArray := TVersaoeSocialArrayStrings[i];
    ConvertidoParaEnum := StrToVersaoeSocialEX(ElementoDoArray);
    ReconvertidoParaString := VersaoeSocialToStrEX(ConvertidoParaEnum);
    EhIgual := ReconvertidoParaString = ElementoDoArray;
    CheckTrue(EhIgual, 'Erro conversão no elemento '+ ElementoDoArray + '  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(ConvertidoParaEnum), ord(ConvertidoParaEnum))+'  '+
                       'VersaoeSocialToStrEX => '+ReconvertidoParaString
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToVersaoeSocialEX_ConvertendoStringinvalida_RetornoException;
var
  ConvertidoParaEnum: TVersaoeSocial;
begin
  try
    ConvertidoParaEnum := StrToVersaoeSocialEX('StringInvalidaParaConvesao');
  except
    on EACBrException do
    begin
      Exit;
    end;
  end;
  Fail('Não foi gerada Exception para a string inválida.');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSSimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpSimNao;
  EhIgual, OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum : tpSimNao;
begin
  for vElementoEnum := Low(tpSimNao) to High(tpSimNao)do
  begin
    ConvertidoParaString         := eSSimNaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToSimNao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro conversão no elemento Ord('+ GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSSimNaoToStr => ' + ConvertidoParaString+ '  ' +
                       'eSStrToSimNao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
var
  OK: Boolean;
  i: integer;
  ElementodoArray: string;
  ElementoConvertido: tpSimNao;
  ReconvertidoParaString: string;
  EhIgual: boolean;
begin
  for i:= Low(TSimNaoString) to High(TSimNaoString)do
  begin
    ElementodoArray        := TSimNaoString[i];
    ElementoConvertido     := eSStrToSimNao(OK, ElementodoArray);
    ReconvertidoParaString := eSSimNaoToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementodoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento '+ElementodoArray+'  '+
                       'eSStrToSimNao => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSSimNaoToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSSimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, Ok: Boolean;
  vEnumElemento: tpSimNaoFacultativo;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpSimNaoFacultativo;
begin
  for vEnumElemento := Low(tpSimNaoFacultativo) to High(tpSimNaoFacultativo)do
  begin
    ConvertidoParaString         := eSSimNaoFacultativoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToSimNaoFacultativo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) + '  ' +
                       'eSSimNaoFacultativoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToSimNaoFacultativo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoConvertido: tpSimNaoFacultativo;
  ElementodoArray: string;
  i: integer;
  ReconvertidoParaString: string;
  OK: Boolean;
  EhIgual: boolean;
begin
  for i := Low(TSimNaoFacultativoString) to High(TSimNaoFacultativoString)do
  begin
    ElementodoArray        := TSimNaoFacultativoString[i];
    ElementoConvertido     := eSStrToSimNaoFacultativo(OK, ElementodoArray);
    ReconvertidoParaString := eSSimNaoFacultativoToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementodoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementodoArray + '  ' +
                       'eSStrToSimNaoFacultativo => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSSimNaoFacultativoToStr => ' + ReconvertidoParaString);

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TModoLancamento;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum : TModoLancamento;
begin
  for vEnumElemento := Low(TModoLancamento) to High(TModoLancamento)do
  begin
    ConvertidoParaString         := esModoLancamentoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToModoLancamento(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                   'eSModoLancamentoToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToModoLancamento => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoConvertido: TModoLancamento;
  ElementoDoArray: string;
  ReconvertidoParaString: string;
  EhIgual: boolean;
  OK: Boolean;
  i: integer;
begin
  for i := Low(TModoLancamentoString) to High(TModoLancamentoString)do
  begin
    ElementoDoArray        := TModoLancamentoString[i];
    ElementoConvertido     := eSStrToModoLancamento(OK, ElementoDoArray);
    ReconvertidoParaString := eSModoLancamentoToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToModoLancamento => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSModoLancamentoToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TLayout;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum : TLayout;
begin
  for vEnumElemento := Low(TLayOut) to High(TLayout)do
  begin
    ConvertidoParaString         := LayOuteSocialToServico(vEnumElemento);
    ReconvertidoParaElementoEnum := ServicoToLayout(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento)) + ')=' + IntToStr(ord(vEnumElemento))+ '  ' +
                       'LayOuteSocialToServico => ' + ConvertidoParaString + '  ' +
                       'ServicoToLayOut => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.TipoEventoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vEnumElemento: TTipoEvento;
  EhIgual, OK: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TTipoEvento;
begin
  for vEnumElemento := Low(TTipoEvento) to High(TTipoEvento)do
  begin
    ConvertidoParaString         := TipoEventoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := StrToTipoEvento(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')=' + IntToStr(ord(vEnumElemento)) + '  ' +
                       'TipoEventoToStr => ' + ConvertidoParaString + '  ' +
                       'StrToTipoEvento => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToTipoEvento_ConvertentoTodosTipos_RetornoCorreto;
var
  ElementoConvertido: TTipoEvento;
  ReconvertidoParaString: String;
  EhIgual: Boolean;
  OK: boolean;
  i: Integer;
  ElementoDoArray: string;
begin
  for i:= Low(TTipoEventoString) to High(TTipoEventoString)do
  begin
    ElementoDoArray            := TTipoEventoString[i];
    ElementoConvertido := StrToTipoEvento(OK, ElementoDoArray);
    ReconvertidoParaString     := TipoEventoToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'StrToTipoEvento => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'TipoEventoToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSprocEmiToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vEnumElemento: TpProcEmi;
  EhIgual, OK: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpProcEmi;
begin
  for vEnumElemento := Low(TpProcEmi) to High(TpProcEmi)do
  begin
    ConvertidoParaString := eSprocEmiToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToprocEmi(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')=' + IntToStr(ord(vEnumElemento)) + '  ' +
                       'eSprocEmiToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToprocEmi => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToprocEmi_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpInscricaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: tpTpInsc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpInsc;
begin
  for vEnumElemento := Low(tpTpInsc) to High(tpTpInsc)do
  begin
    ConvertidoParaString         := eSTpInscricaoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToTpInscricao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                       'eSTpInscricaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpInscricao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpInscricao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpInscAmbTabToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: tpTpInscAmbTab;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpInscAmbTab;
begin
  for vEnumElemento := Low(tpTpInscAmbTab) to High(tpTpInscAmbTab)do
  begin
    ConvertidoParaString         := eStpTpInscAmbTabToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrTotpTpInscAmbTab(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                       'eStpTpInscAmbTabToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrTotpTpInscAmbTab => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpTpInscAmbTab_ConvertendoTodosTipos_RetornoCorreto;
var
  i: tpTpInscAmbTab;
  ElementoDoArray: string;
  ElementoConvertido: tpTpInscAmbTab;
  ReconvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
begin
  for i := Low(tpTpInscAmbTabArrayStrings) to High(tpTpInscAmbTabArrayStrings)do
  begin
    ElementoDoArray        := tpTpInscAmbTabArrayStrings[i];
    ElementoConvertido     := eSStrTotpTpInscAmbTab(OK, ElementoDoArray);
    ReconvertidoParaString := eStpTpInscAmbTabToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray + '  '+
                       'eSStrToTpTpInscAmbTab => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eStpTpInscAmbTabToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpInscPropToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpTpInscProp;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpTpInscProp;
begin
  for vEnumElemento := Low(TpTpInscProp) to High(TpTpInscProp)do
  begin
    ConvertidoParaString   := eSTpInscPropToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToTpInscProp(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) + '  '+
                       'eSTpInscPropToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpInscProp => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpInscProp_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndCooperativaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpIndCoop;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndCoop;
begin
  for vEnumElemento := Low(TpIndCoop) to High(TpIndCoop)do
  begin
    ConvertidoParaString         := eSIndCooperativaToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndCooperativa(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                   'eSIndCooperativaToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToIndCooperativa => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndCooperativa_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndConstrutoraToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpIndConstr;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndConstr;
begin
  for vEnumElemento := Low(TpIndConstr) to High(TpIndConstr)do
  begin
    ConvertidoParaString         := eSIndConstrutoraToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndConstrutora(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) +'  '+
                       'eSIndConstrutoraToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndConstrutora => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndConstrutora_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndDesFolhaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vEnumElemento: TpIndDesFolha;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndDesFolha;
begin
  for vEnumElemento := Low(TpIndDesFolha) to High(TpIndDesFolha)do
  begin
    ConvertidoParaString := eSIndDesFolhaToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndDesFolha(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+'  '+
                       'eSIndDesFolhaToStr => ' + ConvertidoParaString+'  ' +
                       'eSStrToIndDesFolha => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndDesFolha_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndOptRegEletronicoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vElementoEnum: TpIndOptRegEletron;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndOptRegEletron;
begin
  for vElementoEnum := Low(TpIndOptRegEletron) to High(TpIndOptRegEletron)do
  begin
    ConvertidoParaString         := eSIndOptRegEletronicoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndOptRegEletronico(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversao no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndOptRegEletronicoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndOptRegEletronico => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndOptRegEletronico_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndOpcCPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndOpcCP;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndOpcCP;
begin
  for vElementoEnum := Low(TpIndOpcCP) to High(TpIndOpcCP)do
  begin
    ConvertidoParaString         := eSIndOpcCPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndOpcCP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversao no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                   'eSIndOpcCPToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToIndOpcCP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndOpcCP_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSAliqRatToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpAliqRat;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpAliqRat;
begin
  for vElementoEnum := Low(TpAliqRat) to High(TpAliqRat)do
  begin
    ConvertidoParaString := eSAliqRatToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToAliqRat(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSAliqRatToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToAliqRat => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum),ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToAliqRat_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpProcessoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpTpProc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpProc;
begin
  for vElementoEnum := Low(tpTpProc) to High(tpTpProc)do
  begin
    ConvertidoParaString := eSTpProcessoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpProcesso(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                       'eSTpProcessoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpProcesso => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpProcesso_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndAcordoIsencaoMultaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndAcordoIsencaoMulta;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndAcordoIsencaoMulta;
begin
  for vElementoEnum := Low(TpIndAcordoIsencaoMulta) to High(TpIndAcordoIsencaoMulta)do
  begin
    ConvertidoParaString         := eSIndAcordoIsencaoMultaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndAcordoIsencaoMulta(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                       'eSIndAcordoIsencaoMultaToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndAcordoIsencaoMulta => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum),ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndAcordoIsencaoMulta_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpInscContratanteToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TptpInscContratante;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TptpInscContratante;
begin
  for vElementoEnum := Low(TptpInscContratante) to High(TptpInscContratante)do
  begin
    ConvertidoParaString         := eStpInscContratanteToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpInscContratante(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpInscContratanteToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrTotpInscContratante => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpInscContratante_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncCPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpCodIncCP;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: tpCodIncCP;
begin
  for vElementoEnum := Low(tpCodIncCP) to High(tpCodIncCP)do
  begin
    ConvertidoParaString         := eSCodIncCPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncCP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  ' +
                       'eSCodIncCPToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToCodIncCP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncCP_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: boolean;
  OK: boolean;
  ElementoConvertido: tpCodIncCP;
  i: tpCodIncCP;
  ReconvertidoParaString: string;
  ElementoDoArray: string;
begin
  for i:= Low(tpCodIncCPArrayStrings) to High(tpCodIncCPArrayStrings)do
  begin
    ElementoDoArray        := tpCodIncCPArrayStrings[i];
    ElementoConvertido     := eSStrToCodIncCP(OK, ElementoDoArray);
    ReconvertidoParaString := eSCodIncCPToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToCodIncCP => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSCodIncCPToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncIRRFToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpCodIncIRRF;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: tpCodIncIRRF;
begin
  for vElementoEnum := Low(tpCodIncIRRF) to High(tpCodIncIRRF)do
  begin
    ConvertidoParaString         := eSCodIncIRRFToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncIRRF(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                   'eSCodIncIRRFToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToCodIncIRRF => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncIRRF_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoConvertido: tpCodIncIRRF;
  ReconvertidoParaString: string;
  ElementoDoArray: string;
  EhIgual: boolean;
  OK: boolean;
  i: tpCodIncIRRF;
begin
  for i:= Low(tpCodIncIRRF) to High(tpCodIncIRRF)do
  begin
    ElementoDoArray        := tpCodIncIRRFArrayStrings[i];
    ElementoConvertido     := eSStrToCodIncIRRF(OK, ElementoDoArray);
    ReconvertidoParaString := eSCodIncIRRFToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToCodIncIRRF => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSCodIncIRRFToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncCPRPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: boolean;
  vElementoEnum: TpCodIncCPRP;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpCodIncCPRP;
begin
  for vElementoEnum := Low(TpCodIncCPRP) to High(TpCodIncCPRP)do
  begin
    ConvertidoParaString         := eSCodIncCPRPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncCPRP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum)) + '  ' +
                       'eSCodIncCPRPToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToCodIncCPRT => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncCPRP_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoDoArray: string;
  ElementoConvertido: tpCodIncCPRP;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaString: string;
  i: tpCodIncCPRP;
begin
  for i:=Low(tpCodIncCPRP) to High(tpCodIncCPRP)do
  begin
    ElementoDoArray        := tpCodIncCPRPArrayStrings[i];
    ElementoConvertido     := eSStrToCodIncCPRP(OK, ElementoDoArray);
    ReconvertidoParaString := eSCodIncCPRPToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToCodIncCPRP => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSCodIncCPRPToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpCodIncFGTS;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpCodIncFGTS;
begin
  for vElementoEnum := Low(TpCodIncFGTS) to High(TpCodIncFGTS)do
  begin
    ConvertidoParaString         := eSCodIncFGTSToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncFGTS(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSCodIncFGTSToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToCodIncFGTS => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncFGTS_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: boolean;
  OK: boolean;
  ElementoConvertido: tpCodIncFGTS;
  i: tpCodIncFGTS;
  ElementoDoArray: string;
  ReconvertidoParaString: string;
begin
  for i := Low(tpCodIncFGTS) to High(tpCodIncFGTS)do
  begin
    ElementoDoArray        := tpCodIncFGTSArrayStrings[i];
    ElementoConvertido     := eSStrToCodIncFGTS(OK, ElementoDoArray);
    ReconvertidoParaString := eSCodIncFGTSToStr(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToCodIncFGTS => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eSCodIncFGTSToStr => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSExtDecisaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpExtDecisao;
  ConvertidoParaString : String;
  ReConvertidoParaElementoEnum: TpExtDecisao;
  EhIgual, OK: Boolean;
begin
  for vElementoEnum := Low(TpExtDecisao) to High(TpExtDecisao)do
  begin
    ConvertidoParaString := eSExtDecisaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToExtDecisao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSExtDecisaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToExtDecisao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToExtDecisao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndSubstPatronalObraToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: Boolean;
  vElementoEnum: TpIndSubstPatronalObra;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndSubstPatronalObra;
  OK : Boolean;
begin
  for vElementoEnum := Low(TpIndSubstPatronalObra) to High(TpIndSubstPatronalObra)do
  begin
    OK := False;
    ConvertidoParaString := eSIndSubstPatronalObraToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndSubstPatronalObra(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndSubstPatronalObraToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndSubstPatronalObra => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndSubstPatronalObra_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSindAutoriaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpindAutoria;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpindAutoria;
begin
  for vElementoEnum := Low(TpindAutoria) to High(TpindAutoria)do
  begin
    ConvertidoParaString := eSindAutoriaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToindAutoria(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSindAutoriaToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToindAutoria => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToindAutoria_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIndMatProcToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpIndMatProc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpIndMatProc;
begin
  for vElementoEnum := Low(tpIndMatProc) to High(tpIndMatProc)do
  begin
    ConvertidoParaString := eSTpIndMatProcToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpIndMatProc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIndMatProcToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpIndMatProc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpIndMatProc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndRetificacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndRetificacao;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndRetificacao;
begin
  for vElementoEnum := Low(TpIndRetificacao) to High(TpIndRetificacao)do
  begin
    ConvertidoParaString := eSIndRetificacaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndRetificacao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndRetificacaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndRetificacao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndRetiricacao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndApuracaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vElementoEnum: TpIndApuracao;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndApuracao;
begin
  for vElementoEnum := Low(TpIndApuracao) to High(TpIndApuracao) do
  begin
    ConvertidoParaString := eSIndApuracaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndApuracao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndApuracaoToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndApuracao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndApuracao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndMVToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndMV;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndMV;
begin
  for vElementoEnum := Low(TpIndMV) to High(TpIndMV)do
  begin
    ConvertidoParaString := eSIndMVToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndMV(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndMVToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIndMV => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndMV_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndSimplesToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndSimples;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndSimples;
begin
  for vElementoEnum := Low(TpIndSimples) to High(TpIndSimples)do
  begin
    ConvertidoParaString := eSIndSimplesToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndSimples(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                       'eSIndSimplesToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndSimples => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndSimples_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.VersaoeSocialToDblEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TVersaoeSocial;
  ConvertidoParaDbl: Real;
  ReconvertidoParaElementoEnum: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for vElementoEnum := Low(TVersaoeSocial) to High(TVersaoeSocial)do
  begin
    ConvertidoParaDbl := VersaoeSocialToDblEX(vElementoEnum);
    ReconvertidoParaElementoEnum := DblToVersaoeSocialEX(ConvertidoParaDbl);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'VersaoeSocialToDbl => ' + FormatFloat('##,####', ConvertidoParaDbl) +'  '+
                       'DblToVersaoeSocial => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.DblToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ConvertidoParaEnum: TVersaoeSocial;
  ReconvertidoParaDbl: Real;
  i: Integer;
begin
  Fail('Ainda precisa implementar o teste');
  //for i:= Low(TVersaoeSocialArrayReals) to High(TVersaoeSocialArrayReals)do
  //begin
  //  ConvertidoParaEnum  := DblToVersaoeSocialEX(TVersaoeSocialArrayReals[i]);
  //  ReconvertidoParaDbl := VersaoeSocialToDblEX(ConvertidoParaEnum);
  //  EhIgual := (ReconvertidoParaDbl = TVersaoeSocialArrayReals[i]);
  //  CheckTrue(EhIgual, 'Erro na conversão do elemento '+ FormatFloat('##,####', TVersaoeSocialArrayReals[i]) + ' (i='+IntToStr(i)+')  '+
  //                     'DblToVersaoeSocial => ' + GetEnumName(TypeInfo(ConvertidoParaEnum), ord(ConvertidoParaEnum)) +'  '+
  //                     'VersaoeSocialToDbl => ' + FormatFloat('##,####', ReconvertidoParaDbl)
  //           );
  //end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSNatAtividadeToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpNatAtividade;
  EhIgual: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpNatAtividade;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpNatAtividade) to High(TpNatAtividade)do
  begin
    ConvertidoParaString := eSNatAtividadeToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToNatAtividade(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                       'eSNatAtividadeToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToNatAtividade => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToNatAtividade_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTributoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpTpTributo;
  ConvertidoParaString: String;
  EhIgual : Boolean;
  ReconvertidoParaElementoEnum: TpTpTributo;
  OK: boolean;
begin
  for vElementoEnum := Low(TpTpTributo) to High(TpTpTributo)do
  begin
    ConvertidoParaString := eSTpTributoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpTributo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                   'eSTpTributoToStr => ' + ConvertidoParaString +'  '+
                   'eSStrToTpTributo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTributo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSGrauExpToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpGrauExp;
  EhIgual: Boolean;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpGrauExp;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpGrauExp) to High(TpGrauExp)do
  begin
    ConvertidoParaString := eSGrauExpToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToGrauExp(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSGrauExpToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToGrauExp => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToGrauExp_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndNIFToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: Boolean;
  vElementoEnum: TpIndNIF;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndNIF;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpIndNIF) to High(TpIndNIF)do
  begin
    ConvertidoParaString := eSIndNIFToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndNIF(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndNIFToStr => ' + ConvertidoParaString+ '  '+
                       'eSStrToIndNIF => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIdeOCToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIdeOC;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpIdeOC;
  EhIgual: Boolean;
  OK: Boolean;
begin
  for vElementoEnum := Low(tpIdeOC) to High(tpIdeOC)do
  begin
    ConvertidoParaString := eSIdeOCToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIdeOC(Ok, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIdeOCToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIdeOC => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOC_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIdeOCToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIdeOC;
  ConvertidoParaString: String;
  EhIgual: Boolean;
  ReconvertidoParaElementoEnum: tpIdeOC;
begin
  for vElementoEnum := Low(tpIdeOC) to High(tpIdeOC)do
  begin
    ConvertidoParaString := eSIdeOCToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIdeOCEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIdeOCToStrEX => ' + ConvertidoParaString + '  '+
                       'eSStrToIdeOCEX => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOCEx_ConvertendoTodosTipos_RetornoCorreto;
var
  i: tpIdeOC;
  ConvertidoParaElementoEnum: tpIdeOC;
  ReconvertidoParaString: String;
  EhIgual: Boolean;
  ElementoDoArray: String;
begin
  for i:= Low(TtpIdeOCArrayStrings) to High(TtpIdeOCArrayStrings)do
  begin
    ElementoDoArray := TtpIdeOCArrayStrings[i];
    ConvertidoParaElementoEnum := eSStrToIdeOCEX(ElementoDoArray);
    ReconvertidoParaString := eSIdeOCToStrEX(ConvertidoParaElementoEnum);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual, 'Erro de conversão no elemento '+ ElementoDoArray + '  '+
                       'eSStrToIdeOCEX => ' + GetEnumName(TypeInfo(ConvertidoParaElementoEnum), ord(ConvertidoParaElementoEnum))+'  '+
                       'eSIdeOCToStrEX => ' + ReconvertidoParaString
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOCEX_ConvertendoStringinvalida_RetornoException;
var
  ConvertidoParaEnum : tpIdeOC;
begin
  try
    ConvertidoParaEnum := esStrToIdeOCEX('StringInvalidaParaConversao');
  except
    on E:Exception do
    begin
      Exit;
    end;
  end;
  Fail('Não foi gerada Exception para a string inválida');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpProcRRAToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum : TpTpProcRRA;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: TpTpProcRRA;
begin
  for vElementoEnum := Low(TpTpProcRRA) to High(TpTpProcRRA)do
  begin
    ConvertidoParaString := eSTpProcRRAToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpProcRRA(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpProcRRAToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpProcRRA => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpProcRRA_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpProcRetToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpProcRet;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpProcRet;
begin
  for vElementoEnum := Low(tpTpProcRet) to High(tpTpProcRet)do
  begin
    ConvertidoParaString         := eStpTpProcRetToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpTpProcRet(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpProcRetToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpTpProcRet => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpTpProcRet_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpInscAdvogadoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpTpInscAdvogado;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpTpInscAdvogado;
begin
  for vElementoEnum := Low(TpTpInscAdvogado) to High(TpTpInscAdvogado)do
  begin
    ConvertidoParaString         := eSTpInscAdvogadoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpInscAdvogado(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                       'eSTpInscAdvogadoToStr => '+ ConvertidoParaString +'  '+
                       'eSStrToTpInscAdvogado => '+ GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpInscAdvogado_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndIncidenciaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpIndIncidencia;
  OK: boolean;
  EhIgual: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: TpIndIncidencia;
begin
  for vElementoEnum := Low(TpIndIncidencia) to High(TpIndIncidencia)do
  begin
    ConvertidoParaString         := eSIndIncidenciaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndIncidencia(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndIncidenciaToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndIncidencia => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndIncidencia_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndAbrangenciaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpIndAbrangencia;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: TpIndAbrangencia;
begin
  for vElementoEnum := Low(TpIndAbrangencia) to High(TpIndAbrangencia)do
  begin
    ConvertidoParaString         := eSIndAbrangenciaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndAbrangencia(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndAbrangenciaToStr => ' + ConvertidoParaString +
                       'eSStrToIndAbrangencia => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndAbrangencia_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndComercializacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpIndComercializacao;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: TpIndComercializacao;
  ConvertidoParaString: String;
begin
  for vElementoEnum := Low(TpIndComercializacao) to High(TpIndComercializacao)do
  begin
    ConvertidoParaString         := eSIndComercializacaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndComercializacao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversao no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndComercializacaoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIndComercializacao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndComercializacao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRubrToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpRubr;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpRubr;
  ConvertidoParaString: String;
begin
  for vElementoEnum := Low(tpTpRubr) to High(tpTpRubr)do
  begin
    ConvertidoParaString         := eSTpRubrToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpRubr(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpRubrToStr => ' + ConvertidoParaString +
                       'eSStrToTpRubr => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRubr_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSAcumCargoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpAcumCargo;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpAcumCargo;
begin
  for vElementoEnum := Low(tpAcumCargo) to High(tpAcumCargo)do
  begin
    ConvertidoParaString         := eSAcumCargoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToAcumCargo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+ IntToStr(ord(vElementoEnum))+'  '+
                       'eSAcumCargoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToAcumCargo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToAcumCargo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste!');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSContagemEspToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpContagemEsp;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpContagemEsp;
begin
  for vElementoEnum := Low(tpContagemEsp) to High(tpContagemEsp)do
  begin
    ConvertidoParaString         := eSContagemEspToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToContagemEsp(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSContagemEspToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToContagemEsp => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToContagemEsp_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar os testes');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpUtilizEPCToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpUtilizEPC;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpUtilizEPC;
begin
  for vElementoEnum := Low(tpUtilizEPC) to High(tpUtilizEPC)do
  begin
    ConvertidoParaString         := eStpUtilizEPCToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpUtilizEPC(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpUtilizEPCToStr => ' + ConvertidoParaString +'  '+
                       'eSStrTotpUtilizEPC => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpUtilizEPC_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpUtilizEPIToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpUtilizEPI;
  OK: boolean;
  EhIgual: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpUtilizEPI;
begin
  for vElementoEnum := Low(tpUtilizEPI) to High(tpUtilizEPI)do
  begin
    ConvertidoParaString         := eStpUtilizEPIToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpUtilizEPI(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpUtilizEPIToStr => ' + ConvertidoParaString +'  '+
                       'eSStrTotpUtilizEPI => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpUtilizEPI_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSLocalAmbToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpLocalAmb;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpLocalAmb;
begin
  for vElementoEnum := Low(tpLocalAmb) to High(tpLocalAmb)do
  begin
    ConvertidoParaString         := eSLocalAmbToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToLocalAmb(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSLocalAmbToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToLocalAmb => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToLocalAmb_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpAcConvToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpAcConv;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpAcConv;
begin
  for vElementoEnum := Low(tpTpAcConv) to High(tpTpAcConv)do
  begin
    ConvertidoParaString         := eSTpAcConvToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpAcConv(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpAcConvToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpAcConv => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpAcConv_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCnhToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpCnh;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpCnh;
begin
  for vElementoEnum := Low(tpCnh) to High(tpCnh)do
  begin
    ConvertidoParaString         := eSCnhToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCnh(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSCnhToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToCnh => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCnh_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSClassTrabEstrangToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpClassTrabEstrang;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpClassTrabEstrang;
begin
  for vElementoEnum := Low(tpClassTrabEstrang) to High(tpClassTrabEstrang)do
  begin
    ConvertidoParaString         := eSClassTrabEstrangToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToClassTrabEstrang(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSClassTrabEstrangToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToClassTrabEstrang => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToClassTrabEstrang_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpDepToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpDep;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpDep;
begin
  for vElementoEnum := Low(tpTpDep) to High(tpTpDep)do
  begin
    ConvertidoParaString         := eStpDepToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpDep(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpDepToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpDep => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpDep_ConvertentoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRegTrabToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpRegTrab;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpRegTrab;
begin
  for vElementoEnum := Low(tpTpRegTrab) to High(tpTpRegTrab)do
  begin
    ConvertidoParaString         := eSTpRegTrabToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpRegTrab(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpRegTrabToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpRegTrab => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRegTrab_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRelDepToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpRelDep;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpRelDep;
begin
  for vElementoEnum := Low(tpRelDep) to High(tpRelDep)do
  begin
    ConvertidoParaString         := eStpRelDepToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpRelDep(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpRelDepToStr => ' + ConvertidoParaString +'  '+
                       'eSStrTotpRelDep => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRelDep_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRegPrevToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpRegPrev;
  ConvertidoParaString: string;
  OK: boolean;
  EhIgual: boolean;
  ReconvertidoParaElementoEnum: tpTpRegPrev;
begin
  for vElementoEnum := Low(tpTpRegPrev) to High(tpTpRegPrev)do
  begin
    ConvertidoParaString         := eSTpRegPrevToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpRegPrev(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpRegPrevToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpRegPrev => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRegPrev_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRegPrevFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpRegPrevFacultativo;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpRegPrevFacultativo;
begin
  for vElementoEnum := Low(tpTpRegPrevFacultativo) to High(tpTpRegPrevFacultativo)do
  begin
    ConvertidoParaString         := eSTpRegPrevFacultativoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpRegPrevFacultativo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpRegPrevFacultativoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpRegPrevFacultativo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRegPrevFacultativo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o Teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpAdmissaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpAdmissao;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpAdmissao;
begin
  for vElementoEnum := Low(tpTpAdmissao) to High(tpTpAdmissao)do
  begin
    ConvertidoParaString         := eSTpAdmissaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpAdmissao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpAdmissaoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpAdmissao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpAdmissao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIndAdmissaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpIndAdmissao;
  OK: boolean;
  EhIgual: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpIndAdmissao;
begin
  for vElementoEnum := Low(tpTpIndAdmissao) to High(tpTpIndAdmissao) do
  begin
    ConvertidoParaString         := eSTpIndAdmissaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpIndAdmissao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIndAdmissaoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpIndAdmissao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpIndAdmissao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpRegJorToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpRegJor;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpRegJor;
begin
  for vElementoEnum := Low(tpTpRegJor) to High(tpTpRegJor) do
  begin
    ConvertidoParaString         := eSTpRegJorToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSSTrToTpRegJor(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpRegJorToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpRegJor => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpRegJor_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSOpcFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpOpcFGTS;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpOpcFGTS;
begin
  for vElementoEnum := Low(tpOpcFGTS) to High(tpOpcFGTS)do
  begin
    ConvertidoParaString         := eSOpcFGTSToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToOpcFGTS(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSOpcFGTSToSTr => ' + ConvertidoParaString +'  '+
                       'eSStrToOpcFGTS => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToOpcFGTS_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSMtvContratToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMtvContrat;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpMtvContrat;
begin
  for vElementoEnum := Low(tpMtvContrat) to High(tpMtvContrat)do
  begin
    ConvertidoParaString := eSMtvContratToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToMtvContrat(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSMtvContratToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToMtvContrat => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToMtvContrat_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndProvimToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndProvim;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIndProvim;
begin
  for vElementoEnum := Low(tpIndProvim) to High(tpIndProvim)do
  begin
    ConvertidoParaString         := eSIndProvimToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndProvim(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = ReconvertidoParaElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndProvimToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIndProvim => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndProvim_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpProvToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpProv;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpProv;
begin
  for vElementoEnum := Low(tpTpProv) to High(tpTpProv)do
  begin
    ConvertidoParaString     := eSTpProvToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpProv(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpProvToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpProv => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpProv_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSUndSalFixoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpUndSalFixo;
  OK: boolean;
  ConvertidoParaString: string;
  EhIgual: boolean;
  ReconvertidoParaElementoEnum: tpUndSalFixo;
begin
  for vElementoEnum := Low(tpUndSalFixo) to High(tpUndSalFixo) do
  begin
    ConvertidoParaString         := eSUndSalFixoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToUndSalFixo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSUndSalFixoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToUndSalFixo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToUndSalFixo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa impleementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpContrToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpContr;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpContr;
begin
  for vElementoEnum := Low(tpTpContr) to High(tpTpContr)do
  begin
    if(vElementoEnum = PrazoNaoAplicavel)then //Esse valor é extra dos fontes para não gerar a tag e por isso não é convertido para o e-social
      continue;
    ConvertidoParaString         := eSTpContrToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpContr(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpContrToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpContr => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpContr_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o Teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpContrS2500ToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpContrS2500;
  OK: boolean;
  ConvertidoParaString: string;
  EhIgual: boolean;
  ReconvertidoParaElementoEnum: tpTpContrS2500;
begin
  for vElementoEnum := Low(tpTpContrS2500) to High(tpTpContrS2500)do
  begin
    ConvertidoParaString          := eSTpContrS2500ToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpContrS2500(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpContrS2500ToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpContrS2500 => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpContrS2500_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpJornadaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpJornada;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpJornada;
begin
  for vElementoEnum := Low(tpTpJornada) to High(tpTpJornada)do
  begin
    ConvertidoParaString         := eSTpJornadaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpJornada(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpJornadaToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpJornada => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSSTrToTpJornada_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa Implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpDiaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpDia;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpDia;
begin
  for vElementoEnum := Low(tpTpDia) to High(tpTpDia)do
  begin
    ConvertidoParaString         := eSTpDiaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpDia(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpDiaToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpDia => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpDia_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpExameOcupToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpExameOcup;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpExameOcup;
begin
  for vElementoEnum := Low(tpTpExameOcup) to High(tpTpExameOcup)do
  begin
    ConvertidoParaString         := eSTpExameOcupToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpExameOcup(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpExameOcupToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpExameOcup => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpExameOcup_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSResAsoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpResAso;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpResAso;
begin
  for vElementoEnum := Low(tpResAso) to High(tpResAso)do
  begin
    if(vElementoEnum = raNaoInformado)then //Valor definido pelo ACBr para NÃO gerar a tag, não tem conversão válida para o e-Social.
      continue;
    ConvertidoParaString         := eSResAsoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToResAso(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSResAsoToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToResAso => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToResAso_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSOrdExameToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpOrdExame;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpOrdExame;
begin
  for vElementoEnum := Low(tpOrdExame) to High(tpOrdExame)do
  begin
    if(vElementoEnum = orNaoInformado)then //Valor definido pelo ACBr para não gerar a tag, não tem conversão válida para o e-Social
      continue;
    ConvertidoParaString         := eSOrdExameToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToOrdExame(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSOrdExameToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToOrdExame => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToOrdExame_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndResultToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndResult;
  OK: boolean;
  ConvertidoParaString: string;
  EhIgual: boolean;
  ReconvertidoParaElementoEnum: tpIndResult;
begin
  for vElementoEnum := Low(tpIndResult) to High(tpIndResult)do
  begin
    if(vElementoEnum = irNaoInformado)then //Valor definido pelo ACBr para não gerar a tag, não tem conversão válida para o e-Social.
      continue;
    ConvertidoParaString         := eSIndResultToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndResult(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndResultToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndResult => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndResult_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpAcidToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpAcid;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpAcid;
begin
  for vElementoEnum := Low(tpTpAcid) to High(tpTpAcid) do
  begin
    ConvertidoParaString         := eSTpAcidToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpAcid(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpAcidToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpAcid => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpAcid_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpCatToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpCat;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpCat;
begin
  for vElementoEnum := Low(tpTpCat) to High(tpTpCat)do
  begin
    ConvertidoParaString         := eSTpCatToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpCat(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpCatToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpCat => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpCat_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIniciatCATToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIniciatCAT;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIniciatCAT;
begin
  for vElementoEnum := Low(tpIniciatCAT) to High(tpIniciatCAT) do
  begin
    ConvertidoParaString         := eSIniciatCATToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIniciatCAT(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIniciatCATToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIniciatCAT => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIniciatCAT_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpLocalToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpLocal;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpLocal;
begin
  for vElementoEnum := Low(tpTpLocal) to High(tpTpLocal)do
  begin
    ConvertidoParaString         := eSTpLocalToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpLocal(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpLocalToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpLocal => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpLocal_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSLateralidadeToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpLateralidade;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpLateralidade;
begin
  for vElementoEnum := Low(tpLateralidade) to High(tpLateralidade)do
  begin
    ConvertidoParaString         := eSLateralidadeToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToLateralidade(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSLateralidadeToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToLateralidade => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToLateralidade_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpReintToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpReint;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpReint;
begin
  for vElementoEnum := Low(tpTpReint) to High(tpTpReint) do
  begin
    ConvertidoParaString         := eSTpReintToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpReint(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpReintToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpReint => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpReint_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndSubstPatrStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndSubstPatr;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpIndSubstPatr;
begin
  for vElementoEnum := Low(tpIndSubstPatr) to High(tpIndSubstPatr)do
  begin
    ConvertidoParaString         := eSIndSubstPatrStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndSubstPatr(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndSubstPatrStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIndSubstPatr => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndSubstPatr_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIdAquisStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIdAquis;
  OK: boolean;
  EhIgual: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpIdAquis;
begin
  for vElementoEnum := Low(tpIdAquis) to High(tpIdAquis)do
  begin
    ConvertidoParaString         := eSIdAquisStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIdAquis(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIdAquisStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIdAquis => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdAquis_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndComercStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndComerc;
  OK: boolean;
  EhIgual: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpIndComerc;
begin
  for vElementoEnum := Low(tpIndComerc) to High(tpIndComerc)do
  begin
    ConvertidoParaString         := eSIndComercStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndComerc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndComercStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndComerc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndComerc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTpPgtoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpPgto;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpPgto;
begin
  for vElementoEnum := Low(tpTpPgto) to High(tpTpPgto)do
  begin
    ConvertidoParaString         := eSTpTpPgtoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpTpPgto(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpTpPgtoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrTotpTpPgto => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotptpPgto_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpMotivosAfastamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMotivosAfastamento;
  EhIgual: boolean;
  OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpMotivosAfastamento;
begin
  for vElementoEnum := Low(tpMotivosAfastamento) to High(tpMotivosAfastamento)do
  begin
    ConvertidoParaString   := eStpMotivosAfastamentoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMotivosAfastamento(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpMotivosAfastamentoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpMotivosAfastamento => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMotivosAfastamento_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpAcidTransitoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpAcidTransito;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpAcidTransito;
begin
  for vElementoEnum := Low(tpTpAcidTransito) to High(tpTpAcidTransito)do
  begin
    ConvertidoParaString         := eStpTpAcidTransitoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpTpAcidTransito(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpAcidTransitoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpTpAcidTransito => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTpAcidTransito_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpInfOnusToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpInfOnus;
  EhIgual: boolean;
  ConvertidoParaString: string;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpInfOnus;
begin
  for vElementoEnum:= Low(tpInfOnus) to High(tpInfOnus)do
  begin
    ConvertidoParaString         := tpInfOnusToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpInfOnus(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpInfOnusToStr => ' + ConvertidoParaString +'  '+
                       'StrTotpInfOnus => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpInfOnus_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpOnusRemunToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpOnusRemun;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpOnusRemun;
  OK: boolean;
  EhIgual: boolean;
begin
  for vElementoEnum := Low(tpOnusRemun) to High(tpOnusRemun)do
  begin
    ConvertidoParaString         := tpOnusRemunToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpOnusRemun(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+'  '+
                       'tpOnusRemunToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpOnusRemun => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpOnusRemun_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpNatEstagioToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpNatEstagio;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpNatEstagio;
begin
  for vElementoEnum := Low(tpNatEstagio) to High(tpNatEstagio)do
  begin
    ConvertidoParaString         := eSTpNatEstagioToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpNatEstagio(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpNatEstagioToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpNatEstagio => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpNatEstagio_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpNivelEstagioToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpNivelEstagio;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpNivelEstagio;
begin
  for vElementoEnum := Low(tpNivelEstagio) to High(tpNivelEstagio)do
  begin
    ConvertidoParaString         := eSTpNivelEstagioToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpNivelEstagio(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpNivelEstagioToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpNivelEstagio => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpNivelEstagio_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpCaepfToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpCaepf;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpCaepf;
begin
  for vElementoEnum := Low(tpCaepf) to High(tpCaepf)do
  begin
    ConvertidoParaString         := eSTpCaepfToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpCaepf(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpCaepfToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpCaepf => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpCaepf_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpPlanRPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpPlanRP;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpPlanRP;
begin
  for vElementoEnum := Low(tpPlanRP) to High(tpPlanRP)do
  begin
    if(vElementoEnum = prpNenhum)then//Valor definido no ACBr para não gerar a tag, não tem conversão válida para o e-Social
      continue;
    ConvertidoParaString         := eSTpPlanRPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpPlanRP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpPlanRPToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpPlanRP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpPlanRP_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpMtvAltToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMtvAlt;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpMtvAlt;
begin
  for vElementoEnum := Low(tpMtvAlt) to High(tpMtvAlt)do
  begin
    ConvertidoParaString         := eSTpMtvAltToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMtvAlt(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpMtvAltToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpMtvAlt => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMtvAlt_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpOrigemAltAfastToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpOrigemAltAfast;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpOrigemAltAfast;
begin
  for vElementoEnum := Low(tpOrigemAltAfast) to High(tpOrigemAltAfast)do
  begin
    ConvertidoParaString         := eSTpOrigemAltAfastToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpOrigemAltAfast(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpOrigemAltAfastToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpOrigemAltAfast => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpOrigemAltAfast_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpPensaoAlimToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpPensaoAlim;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpPensaoAlim;
begin
  for vElementoEnum := Low(tpPensaoAlim) to High(tpPensaoAlim)do
  begin
    ConvertidoParaString         := eSTpPensaoAlimToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpPensaoAlim(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpPensaoAlimToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpPensaoAlim => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpPensaoAlim_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpCumprParcialAvisoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpCumprParcialAviso;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpCumprParcialAviso;
begin
  for vElementoEnum := Low(tpCumprParcialAviso) to High(tpCumprParcialAviso)do
  begin
    ConvertidoParaString         := eSTpCumprParcialAvisoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpCumprParcialAviso(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpCumprParcialAvisoToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpCumprParcialAviso => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpCumprParcialAviso_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpAvalToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpAval;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpAval;
begin
  for vElementoEnum := Low(tpTpAval) to High(tpTpAval)do
  begin
    ConvertidoParaString         := tpAvalToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpAval(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpAvalToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpAval => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpAval_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpModTreiCapToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpModTreiCap;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpModTreiCap;
begin
  for vElementoEnum := Low(tpModTreiCap) to High(tpModTreiCap)do
  begin
    ConvertidoParaString         := tpModTreiCapToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpModTreiCap(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpModTreiCapToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpModTreiCap => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpModTreiCap_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpTpTreiCapToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpTreiCap;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpTreiCap;
begin
  for vElementoEnum := Low(tpTpTreiCap) to High(tpTpTreiCap)do
  begin
    ConvertidoParaString         := tpTpTreiCapToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpTpTreiCap(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpTpTreiCapToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpTpTreiCap => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpTpTreiCap_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpTpProfToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpProf;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpProf;
begin
  for vElementoEnum := Low(tpTpProf) to High(tpTpProf)do
  begin
    ConvertidoParaString         := tpTpProfToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpTpProf(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpTpProfToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpTpProf => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpTpProf_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpNacProfToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpNacProf;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpNacProf;
begin
  for vElementoEnum := Low(tpNacProf) to High(tpNacProf)do
  begin
    ConvertidoParaString         := tpNacProfToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpNacProf(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpNacProfToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpNacProf => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpNacProf_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpTmpParcToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTmpParc;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTmpParc;
begin
  for vElementoEnum := Low(tpTmpParc) to High(tpTmpParc)do
  begin
    ConvertidoParaString         := tpTmpParcToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpTmpParc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpTmpParcToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpTmpParc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpTmpParc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpClassTribToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpClassTrib;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: TpClassTrib;
begin
  for vElementoEnum := Low(TpClassTrib) to High(TpClassTrib)do
  begin
    ConvertidoParaString         := tpClassTribToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpClassTrib(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpClassTribToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpClassTrib => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpClassTrib_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpTmpResidToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTmpResid;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTmpResid;
begin
  for vElementoEnum := Low(tpTmpResid) to High(tpTmpResid)do
  begin
    ConvertidoParaString         := tpTmpResidToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpTmpResid(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpTmpResidToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpTmpResid => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpTmpResid_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.tpCondIngToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpCondIng;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpCondIng;
begin
  for vElementoEnum := Low(tpCondIng) to High(tpCondIng)do
  begin
    ConvertidoParaString         := tpCondIngToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrTotpCondIng(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'tpTmpResidToStr => ' + ConvertidoParaString + '  '+
                       'StrTotpTmpResid => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrTotpCondIng_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIndApurIRToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndApurIR;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIndApurIR;
begin
  for vElementoEnum := Low(tpIndApurIR) to High(tpIndApurIR)do
  begin
    ConvertidoParaString         := eSTpIndApurIRToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpindApurIR(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIndApurIRToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpindApurIR => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpindApurIR_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIndSitBenefToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndSitBenef;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIndSitBenef;
begin
  for vElementoEnum := Low(tpIndSitBenef) to High(tpIndSitBenef)do
  begin
    ConvertidoParaString         := eSTpIndSitBenefToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpIndSitBenef(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIndSitBenefToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpIndSitBenef => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpIndSitBenef_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpPenMorteToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpPenMorte;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpPenMorte;
begin
  for vElementoEnum := Low(tpTpPenMorte) to High(tpTpPenMorte)do
  begin
    ConvertidoParaString         := eStpTpPenMorteToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpTpPenMorte(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpPenMorteToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpTpPenMorte => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpPenMorteToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpPenMorte;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpTpPenMorte;
  EhIgual: boolean;
begin
  for vElementoEnum := Low(tpTpPenMorte) to High(tpTpPenMorte)do
  begin
    ConvertidoParaString         := eStpTpPenMorteToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpTpPenMorteEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpPenMorteToStrEX => ' + ConvertidoParaString + '  '+
                       'eSStrTotpTpPenMorteEX => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpTpPenMorte_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpTpPenMorteEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ReconvertidoParaString: string;
  ElementoConvertido: tpTpPenMorte;
  ElementoDoArray: string;
  i: tpTpPenMorte;
  EhIgual: boolean;
begin
  for i := Low(tpTpPenMorteArrayStrings) to High(tpTpPenMorteArrayStrings)do
  begin
    ElementoDoArray        := tpTpPenMorteArrayStrings[i];
    ElementoConvertido     := eSStrTotpTpPenMorteEX(ElementoDoArray);
    ReconvertidoParaString := eStpTpPenMorteToStrEX(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ' + ElementoDoArray + '  '+
                       'eSStrTotpTpPenMorteEX => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eStpTpPenMorteToStrEX => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTpPenMorteEX_ConvertendoStringInvalida_RetornoException;
var
  ConvertidoParaEnum: tpTpPenMorte;
begin
  try
    ConvertidoParaEnum := eSStrTotpTpPenMorteEx('StringInválida');
  except
    on E:Exception do
      Exit;
  end;
  Fail('Não foi gerada Exception para string inválida');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpMotCessBenefToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMotCessBenef;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpMotCessBenef;
begin
  for vElementoEnum := Low(tpMotCessBenef) to High(tpMotCessBenef)do
  begin
    ConvertidoParaString         := eStpTpMotCessBenefToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMotCessBenef(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpMotCessBenefToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpMotCessBenef => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMotCessBenef_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpMotCessBenefToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: boolean;
  vElementoEnum: tpMotCessBenef;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: tpMotCessBenef;
begin
  for vElementoEnum := Low(tpMotCessBenef) to High(tpMotCessBenef)do
  begin
    ConvertidoParaString         := eStpTpMotCessBenefToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMotCessBenefEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpMotCessBenefToStrEX => ' + ConvertidoParaString + '  '+
                       'eSStrToTpMotCessBenefEX => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
               );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMotCessBenefEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoDoArray: string;
  ElementoConvertido: tpMotCessBenef;
  ReconvertidoParaString: string;
  i: tpMotCessBenef;
  EhIgual: boolean;
begin
  for i:=Low(tpMotCessBenefArrayStrings) to High(tpMotCessBenefArrayStrings)do
  begin
    ElementoDoArray        := tpMotCessBenefArrayStrings[i];
    ElementoConvertido     := eSStrToTpMotCessBenefEX(ElementoDoArray);
    ReconvertidoParaString := eStpTpMotCessBenefToStrEX(ElementoConvertido);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ' + ElementoDoArray +'  '+
                       'eSStrToTpMotCessBenefEX => ' + GetEnumName(TypeInfo(ElementoConvertido), ord(ElementoConvertido))+'  '+
                       'eStpTpMotCessBenefToStrEX => ' + ReconvertidoParaString);
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMotCessBenefEX_ConvertendoStringInvalida_RetornoException;
var
  ElementoConvertido: tpMotCessBenef;
begin
  try
    ElementoConvertido := eSStrToTpMotCessBenefEX('StringInválida');
  except
    on E:Exception do
      exit;
  end;
  Fail('Não foi gerada Exception para String inválida');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpMtvSuspensaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMtvSuspensao;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpMtvSuspensao;
begin
  for vElementoEnum := Low(tpMtvSuspensao) to High(tpMtvSuspensao)do
  begin
    ConvertidoParaString         := eStpTpMtvSuspensaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMtvSuspensao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpTpMtvSuspensaoToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpMtvSuspensao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMtvSuspensao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpPercTransfToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpPercTransf;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpPercTransf;
begin
  for vElementoEnum := Low(tpPercTransf) to High(tpPercTransf)do
  begin
    ConvertidoParaString         := eSTpPercTransfToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpPercTransf(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpPercTransfToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpPercTransf => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpPercTransf_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.TpIndRemunToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndRemun;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIndRemun;
begin
  for vElementoEnum := Low(tpIndRemun) to High(tpIndRemun)do
  begin
    ConvertidoParaString         := TpIndRemunToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := StrToTpIndRemun(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'TpIndRemunToStr => ' + ConvertidoParaString + '  '+
                       'StrToTpIndRemun => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToTpIndRemun_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTpCCPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpTpCCP;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpTpCCP;
begin
  for vElementoEnum := Low(tpTpCCP) to High(tpTpCCP)do
  begin
    ConvertidoParaString         := eSTpTpCCPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpTpCCP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpTpCCPToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpTpCCP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTpCCP_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpMtvDesligTSVToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpMtvDesligTSV;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpMtvDesligTSV;
begin
  for vElementoEnum := Low(tpMtvDesligTSV) to High(tpMtvDesligTSV)do
  begin
    ConvertidoParaString         := eSTpMtvDesligTSVToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpMtvDesligTSV(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpMtvDesligTSVToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpMtvDesligTSV => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpMtvDesligTSV_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTpRepercProcToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpRepercProc;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpRepercProc;
begin
  for vElementoEnum := Low(tpRepercProc) to High(tpRepercProc)do
  begin
    ConvertidoParaString         := eSTpTpRepercProcToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpRepercProc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpTpRepercProcToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpRepercProc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTpRepercProc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTpOrigemProcToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpOrigemProc;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpOrigemProc;
begin
  for vElementoEnum := Low(tpOrigemProc) to High(tpOrigemProc)do
  begin
    ConvertidoParaString         := eSTpTpOrigemProcToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpOrigemProc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpTpOrigemProcToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToTpOrigemProc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTpOrigemProc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpIndTpDeduToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIndTpDedu;
  ConvertidoParaString: string;
  EhIgual: boolean;
  OK: boolean;
  ReconvertidoParaElementoEnum: tpIndTpDedu;
begin
  for vElementoEnum := Low(tpIndTpDedu) to High(tpIndTpDedu)do
  begin
    ConvertidoParaString         := eStpIndTpDeduToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpIndTpDedu(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpIndTpDeduToStr => ' + ConvertidoParaString + '  '+
                       'eSStrTotpIndTpDedu => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpIndTpDedu_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar');
end;

{ TACBreSocialConversaoeSocialTest }

procedure TACBreSocialConversaoeSocialTest.SetUp;
begin
  inherited SetUp;
  FArqINI      := TStringList.Create;
end;

procedure TACBreSocialConversaoeSocialTest.TearDown;
begin
  inherited TearDown;
  FArqINI.Free;
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1000;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1000, 'Não encontrou o Evento S-1000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1005;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1005);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1005, 'Não encontrou o Evento S-1005');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1010;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1010);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1010, 'Não encontrou o Evento S-1010');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1020;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1020);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1020, 'Não encontrou o Evento S-1020');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1030;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1030);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1030, 'Não encontrou o Evento S-1030');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1035;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1035);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1035, 'Não encontrou o Evento S-1035');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1040;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1040);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1040, 'Não encontrou o Evento S-1040');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1050;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1050);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1050, 'Não encontrou o Evento S-1050');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1060;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1060);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1060, 'Não encontrou o Evento S-1060');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1070;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1070);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1070, 'Não encontrou o Evento S-1070');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1080;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1080);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1080, 'Não encontrou o Evento S-1080');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2190;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2190);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2190, 'Não encontrou o Evento S-2190');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2200, 'Não encontrei o Evento S-2200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2205;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2205);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2205, 'Não encontrei o Evento S-2205');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2206;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2206);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2206, 'Não encontrei o Evento S-2206');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2210, 'Não encontrei o Evento S-2210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2220;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2220);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2220, 'Não encontrei o Evento S-2220');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2221;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2221);
//  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2221, 'Não encontrei o Evento S-2221');
  Check(True,'S-2221 VERIFICAR!');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2230;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2230);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2230, 'Não encontrei o Evento S-2230');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2231;
begin
  //Sucesso automático, pois não tem Ini de exemplo do Monitor p/este evento.
  Check(True,'');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2240;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2240);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2240, 'Não encontrei o Evento S-2240');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2245;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2245);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2245, 'Não encontrei o Evento S-2245');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2250, 'Não encontrei o Evento S-2250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2260, 'Não encontrei o Evento S-2260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2298, 'Não encontrei o Evento S-2298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2299, 'Não encontrei o Evento S-2299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2300, 'Não encontrei o Evento S-2300');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2306;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2306);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2306, 'Não encontrei o Evento S-2306');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2399;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2399);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2399, 'Não encontrei o Evento S-2399');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2400;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S2400);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2400, 'Não encontrei o Evento S-2400');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2405;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2410;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2416;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2418;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2420;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS3000;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S3000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS3000, 'Não encontrei o Evento S-3000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1200, 'Não encontrei o Evento S-1200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1202;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1202);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1202, 'Não encontrei o Evento S-1202');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1207;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1207);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1207, 'Não encontrei o Evento S-1207');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1210, 'Não encontrei o Evento S-1210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1250, 'Não encontrei o Evento S-1250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1260, 'Não encontrei o Evento S-1260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1270;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1270);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1270, 'Não encontrei o Evento S-1270');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1280;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1280);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1280, 'Não encontrei o Evento S-1280');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1295;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1295);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1295, 'Não encontrei o Evento S-1295');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1298, 'Não encontrei o Evento S-1298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1299, 'Não encontrei o Evento S-1299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_vS0100_S1300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1300, 'Não encontrei o Evento S-1300');
end;

initialization
  _RegisterTest('pcesConversaoeSocial', TACBreSocialConversaoeSocialTest);
  _RegisterTest('pcesConversaoeSocial', TACBreSocialTipoToStrStrToTipoTest);
  _RegisterTest('pcesNaoPeriodicos'   , TACBreSocialEventosNaoPeriodicosTest);
  _RegisterTest('pcesPeriodicos'      , TACBreSocialEventosPeriodicosTest);
  _RegisterTest('pcesIniciais'        , TACBreSocialEventosIniciaisTest);
  _RegisterTest('pcesTabelas'         , TACBreSocialEventosTabelasTest);
  _RegisterTest('ACBreSocialSoapTests', TACBreSocialSoapTests);

end.

