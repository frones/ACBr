{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_X;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro X001 - Abertura do Bloco X – Informações Econômicas
  TRegistroX001 = class(TOpenBlocos)
  private
  public
  end;

  /// Registro X280 - Atividades Incentivadas - PJ em Geral

  { TRegistroX280 }

  TRegistroX280 = class
  private
    fATO_CONC: string;
    fIND_ATIV: string;
    fIND_PROJ: string;
    fVIG_FIM:  TDateTime;
    fVIG_INI:  TDateTime;
  public
    property IND_ATIV: string read fIND_ATIV write fIND_ATIV;
    property IND_PROJ: string read fIND_PROJ write fIND_PROJ;
    property ATO_CONC: string read fATO_CONC write fATO_CONC;
    property VIG_INI: TDateTime read fVIG_INI write fVIG_INI;
    property VIG_FIM: TDateTime read fVIG_FIM write fVIG_FIM;
  end;

  /// Registro X291 - Operações com o Exterior - Pessoa
  /// Vinculada/Interposta/País com Tributação Favorecida.

  { TRegistroX291 }

  TRegistroX291 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X292 - Operações com o Exterior - Pessoa Não Vinculada/
  /// Não Interposta/País sem Tributação Favorecida

  { TRegistroX292 }

  TRegistroX292 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X300 - Operações com o Exterior - Exportações (Entradas de Divisas)

  { TRegistroX300 }

  TRegistroX300 = class
  private
    fCOD_CNC:   integer;
    fCOD_NCM:   integer;
    fDESC_EXP:  string;
    fIND_OPER:  string;
    fNUM_ORDEM: string;
    fQTDE:      variant;
    fTIP_EXP:   string;
    fTIP_MET:   string;
    fTIP_MOEDA: string;
    fTOT_OPER:  variant;
    fUNI_MED:   string;
    fVL_AJ:     variant;
    fVL_JUR:    variant;
    fVL_JUR_MAX: variant;
    fVL_JUR_MIN: variant;
    fVL_PAR:    variant;
    fVL_PRAT:   variant;
  public
    property NUM_ORDEM: string read fNUM_ORDEM write fNUM_ORDEM;
    property TIP_EXP: string read fTIP_EXP write fTIP_EXP;
    property DESC_EXP: string read fDESC_EXP write fDESC_EXP;
    property TOT_OPER: variant read fTOT_OPER write fTOT_OPER;
    property COD_NCM: integer read fCOD_NCM write fCOD_NCM;
    property QTDE: variant read fQTDE write fQTDE;
    property UNI_MED: string read fUNI_MED write fUNI_MED;
    property IND_OPER: string read fIND_OPER write fIND_OPER;
    property TIP_MET: string read fTIP_MET write fTIP_MET;
    property VL_PAR: variant read fVL_PAR write fVL_PAR;
    property VL_PRAT: variant read fVL_PRAT write fVL_PRAT;
    property VL_AJ: variant read fVL_AJ write fVL_AJ;
    property VL_JUR: variant read fVL_JUR write fVL_JUR;
    property VL_JUR_MIN: variant read fVL_JUR_MIN write fVL_JUR_MIN;
    property VL_JUR_MAX: variant read fVL_JUR_MAX write fVL_JUR_MAX;
    property COD_CNC: integer read fCOD_CNC write fCOD_CNC;
    property TIP_MOEDA: string read fTIP_MOEDA write fTIP_MOEDA;
  end;

  /// Registro X310 - Operações com o Exterior - Contratantes das  Exportações

  { TRegistroX310 }

  TRegistroX310 = class
  private
    fCOND_PES: integer;
    fNOME:     string;
    fPAIS:     integer;
    fVL_OPER:  variant;
  public
    property NOME: string read fNOME write fNOME;
    property PAIS: integer read fPAIS write fPAIS;
    property VL_OPER: variant read fVL_OPER write fVL_OPER;
    property COND_PES: integer read fCOND_PES write fCOND_PES;
  end;

  /// Registro X320 - Operações com o Exterior - Importações (Saídas de Divisas)

  { TRegistroX320 }

  TRegistroX320 = class
  private
    fCOD_CNC:   string;
    fCOD_NCM:   integer;
    fDESC_IMP:  string;
    fNUM_ORD:   string;
    fQTDE:      variant;
    fTIP_IMP:   integer;
    fTIP_MET:   string;
    fTIP_MOEDA: string;
    fTOT_OPER:  variant;
    fUNI_MED:   string;
    fVL_AJ:     variant;
    fVL_JUR:    variant;
    fVL_JUR_MAX: variant;
    fVL_JUR_MIN: variant;
    fVL_PAR:    variant;
    fVL_PRAT:   variant;
  public
    property NUM_ORD: string read fNUM_ORD write fNUM_ORD;
    property TIP_IMP: integer read fTIP_IMP write fTIP_IMP;
    property DESC_IMP: string read fDESC_IMP write fDESC_IMP;
    property TOT_OPER: variant read fTOT_OPER write fTOT_OPER;
    property COD_NCM: integer read fCOD_NCM write fCOD_NCM;
    property QTDE: variant read fQTDE write fQTDE;
    property UNI_MED: string read fUNI_MED write fUNI_MED;
    property TIP_MET: string read fTIP_MET write fTIP_MET;
    property VL_PAR: variant read fVL_PAR write fVL_PAR;
    property VL_PRAT: variant read fVL_PRAT write fVL_PRAT;
    property VL_AJ: variant read fVL_AJ write fVL_AJ;
    property VL_JUR: variant read fVL_JUR write fVL_JUR;
    property VL_JUR_MIN: variant read fVL_JUR_MIN write fVL_JUR_MIN;
    property VL_JUR_MAX: variant read fVL_JUR_MAX write fVL_JUR_MAX;
    property COD_CNC: string read fCOD_CNC write fCOD_CNC;
    property TIP_MOEDA: string read fTIP_MOEDA write fTIP_MOEDA;
  end;

  /// Registro X330 - Operações com o Exterior - Contratantes das Importações

  { TRegistroX330 }

  TRegistroX330 = class
  private
    fCOND_PES: integer;
    fNOME:     string;
    fPAIS:     integer;
    fVL_OPER:  variant;
  public
    property NOME: string read fNOME write fNOME;
    property PAIS: integer read fPAIS write fPAIS;
    property VL_OPER: variant read fVL_OPER write fVL_OPER;
    property COND_PES: integer read fCOND_PES write fCOND_PES;
  end;

  /// Registro X340 - Identificação da Participação no Exterior

  { TRegistroX340 }

  TRegistroX340 = class
  private
    fIND_CONSOL: string;
    fIND_CONTROLE: integer;
    fIND_REPETRO: string;
    fMOT_NAO_CONSOL: integer;
    fNIF:  string;
    fPAIS: integer;
    fRAZ_SOCIAL: string;
  public
    property RAZ_SOCIAL: string read fRAZ_SOCIAL write fRAZ_SOCIAL;
    property NIF: string read fNIF write fNIF;
    property IND_CONTROLE: integer read fIND_CONTROLE write fIND_CONTROLE;
    property PAIS: integer read fPAIS write fPAIS;
    property IND_REPETRO: string read fIND_REPETRO write fIND_REPETRO;
    property IND_CONSOL: string read fIND_CONSOL write fIND_CONSOL;
    property MOT_NAO_CONSOL: integer read fMOT_NAO_CONSOL write fMOT_NAO_CONSOL;
  end;

  /// Registro X350 - Participações no Exterior - Resultado do Período de  Apuração

  { TRegistroX350 }

  TRegistroX350 = class
  private
    fCUSTOS:      variant;
    fDESP_BRASIL: variant;
    fDESP_OPER:   variant;
    fDESP_OUTRAS: variant;
    fIMP_DEV:     variant;
    fIMP_DEV_ARB: variant;
    fLUC_ARB_ANT_IMP: variant;
    fLUC_ARB_PER_APUR: variant;
    fLUC_BRUTO:   variant;
    fLUC_LIQ:     variant;
    fLUC_LIQ_ANT_IR: variant;
    fLUC_OPER:    variant;
    fREC_AUFERIDAS: variant;
    fREC_LIQ:     variant;
    fREC_OUTRAS:  variant;
    fREC_OUTRAS_OPER: variant;
    fREC_PARTIC:  variant;
  public
    property REC_LIQ: variant read fREC_LIQ write fREC_LIQ;
    property CUSTOS: variant read fCUSTOS write fCUSTOS;
    property LUC_BRUTO: variant read fLUC_BRUTO write fLUC_BRUTO;
    property REC_AUFERIDAS: variant read fREC_AUFERIDAS write fREC_AUFERIDAS;
    property REC_OUTRAS_OPER: variant read fREC_OUTRAS_OPER write fREC_OUTRAS_OPER;
    property DESP_BRASIL: variant read fDESP_BRASIL write fDESP_BRASIL;
    property DESP_OPER: variant read fDESP_OPER write fDESP_OPER;
    property LUC_OPER: variant read fLUC_OPER write fLUC_OPER;
    property REC_PARTIC: variant read fREC_PARTIC write fREC_PARTIC;
    property REC_OUTRAS: variant read fREC_OUTRAS write fREC_OUTRAS;
    property DESP_OUTRAS: variant read fDESP_OUTRAS write fDESP_OUTRAS;
    property LUC_LIQ_ANT_IR: variant read fLUC_LIQ_ANT_IR write fLUC_LIQ_ANT_IR;
    property IMP_DEV: variant read fIMP_DEV write fIMP_DEV;
    property LUC_LIQ: variant read fLUC_LIQ write fLUC_LIQ;
    property LUC_ARB_ANT_IMP: variant read fLUC_ARB_ANT_IMP write fLUC_ARB_ANT_IMP;
    property IMP_DEV_ARB: variant read fIMP_DEV_ARB write fIMP_DEV_ARB;
    property LUC_ARB_PER_APUR: variant read fLUC_ARB_PER_APUR write fLUC_ARB_PER_APUR;
  end;

  /// Registro X351 - Demonstrativo de Resultados e de Imposto a Pagar no Exterior

  { TRegistroX351 }

  TRegistroX351 = class
  private
    fIMP_LUCR:      variant;
    fIMP_LUCR_REAL: variant;
    fIMP_PAG_REND:  variant;
    fIMP_PAG_REND_REAL: variant;
    fIMP_RET_BR:    variant;
    fIMP_RET_EXT:   variant;
    fIMP_RET_EXT_REAL: variant;
    fRES_INV_PER:   variant;
    fRES_INV_PER_REAL: variant;
    fRES_ISEN_PETR_PER: variant;
    fRES_ISEN_PETR_PER_REAL: variant;
    fRES_NEG_ACUM:  variant;
    fRES_POS_TRIB:  variant;
    fRES_POS_TRIB_REAL: variant;
  public
    property RES_INV_PER: variant read fRES_INV_PER write fRES_INV_PER;
    property RES_INV_PER_REAL: variant read fRES_INV_PER_REAL write fRES_INV_PER_REAL;
    property RES_ISEN_PETR_PER: variant read fRES_ISEN_PETR_PER write fRES_ISEN_PETR_PER;
    property RES_ISEN_PETR_PER_REAL: variant read fRES_ISEN_PETR_PER_REAL
      write fRES_ISEN_PETR_PER_REAL;
    property RES_NEG_ACUM: variant read fRES_NEG_ACUM write fRES_NEG_ACUM;
    property RES_POS_TRIB: variant read fRES_POS_TRIB write fRES_POS_TRIB;
    property RES_POS_TRIB_REAL: variant read fRES_POS_TRIB_REAL write fRES_POS_TRIB_REAL;
    property IMP_LUCR: variant read fIMP_LUCR write fIMP_LUCR;
    property IMP_LUCR_REAL: variant read fIMP_LUCR_REAL write fIMP_LUCR_REAL;
    property IMP_PAG_REND: variant read fIMP_PAG_REND write fIMP_PAG_REND;
    property IMP_PAG_REND_REAL: variant read fIMP_PAG_REND_REAL write fIMP_PAG_REND_REAL;
    property IMP_RET_EXT: variant read fIMP_RET_EXT write fIMP_RET_EXT;
    property IMP_RET_EXT_REAL: variant read fIMP_RET_EXT_REAL write fIMP_RET_EXT_REAL;
    property IMP_RET_BR: variant read fIMP_RET_BR write fIMP_RET_BR;
  end;

  /// Registro X352 - Demonstrativo de Resultados no Exterior de Coligadas em Regime de Caixa

  { TRegistroX352 }

  TRegistroX352 = class
  private
    fLUC_DISP:     variant;
    fLUC_DISP_REAL: variant;
    fRES_PER:      variant;
    fRES_PER_REAL: variant;
  public
    property RES_PER: variant read fRES_PER write fRES_PER;
    property RES_PER_REAL: variant read fRES_PER_REAL write fRES_PER_REAL;
    property LUC_DISP: variant read fLUC_DISP write fLUC_DISP;
    property LUC_DISP_REAL: variant read fLUC_DISP_REAL write fLUC_DISP_REAL;

  end;

  /// Registro X353 - Demonstrativo de Consolidação

  { TRegistroX353 }

  TRegistroX353 = class
  private
    fRES_NEG_UTIL:      variant;
    fRES_NEG_UTIL_REAL: variant;
    fSALDO_RES_NEG_NAO_UTIL: variant;
    fSALDO_RES_NEG_NAO_UTIL_REAL: variant;
  public
    property RES_NEG_UTIL: variant read fRES_NEG_UTIL write fRES_NEG_UTIL;
    property RES_NEG_UTIL_REAL: variant read fRES_NEG_UTIL_REAL write fRES_NEG_UTIL_REAL;
    property SALDO_RES_NEG_NAO_UTIL: variant read fSALDO_RES_NEG_NAO_UTIL
      write fSALDO_RES_NEG_NAO_UTIL;
    property SALDO_RES_NEG_NAO_UTIL_REAL: variant
      read fSALDO_RES_NEG_NAO_UTIL_REAL write fSALDO_RES_NEG_NAO_UTIL_REAL;
  end;

  /// Registro X354 - Demonstrativo de Prejuízos Acumulados

  { TRegistroX354 }

  TRegistroX354 = class
  private
    fRES_NEG:      variant;
    fRES_NEG_REAL: variant;
    fSALDO_RES_NEG: variant;
  public
    property RES_NEG: variant read fRES_NEG write fRES_NEG;
    property RES_NEG_REAL: variant read fRES_NEG_REAL write fRES_NEG_REAL;
    property SALDO_RES_NEG: variant read fSALDO_RES_NEG write fSALDO_RES_NEG;
  end;

  /// Registro X355 - Demonstrativo de Rendas Ativas e Passivas

  { TRegistroX355 }

  TRegistroX355 = class
  private
    fPERCENTUAL:      variant;
    fREND_ATIV_PROP:  variant;
    fREND_ATIV_PROP_REAL: variant;
    fREND_PASS_PROP:  variant;
    fREND_PASS_PROP_REAL: variant;
    fREND_TOTAL:      variant;
    fREND_TOTAL_REAL: variant;
  public
    property REND_PASS_PROP: variant read fREND_PASS_PROP write fREND_PASS_PROP;
    property REND_PASS_PROP_REAL: variant read fREND_PASS_PROP_REAL write fREND_PASS_PROP_REAL;
    property REND_TOTAL: variant read fREND_TOTAL write fREND_TOTAL;
    property REND_TOTAL_REAL: variant read fREND_TOTAL_REAL write fREND_TOTAL_REAL;
    property REND_ATIV_PROP: variant read fREND_ATIV_PROP write fREND_ATIV_PROP;
    property REND_ATIV_PROP_REAL: variant read fREND_ATIV_PROP_REAL write fREND_ATIV_PROP_REAL;
    property PERCENTUAL: variant read fPERCENTUAL write fPERCENTUAL;
  end;

  /// Registro X356 - Demonstrativo de Estrutura Societária

  { TRegistroX356 }

  TRegistroX356 = class
  private
    fATIVO_TOTAL: variant;
    fPAT_LIQUIDO: variant;
    fPERC_PART:   variant;
  public
    property PERC_PART: variant read fPERC_PART write fPERC_PART;
    property ATIVO_TOTAL: variant read fATIVO_TOTAL write fATIVO_TOTAL;
    property PAT_LIQUIDO: variant read fPAT_LIQUIDO write fPAT_LIQUIDO;
  end;

  /// Registro X390 - Origem e Aplicação de Recursos - Imunes ou Isentas

  { TRegistroX390 }

  TRegistroX390 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X400 - Comércio Eletrônico e Tecnologia da Informação

  { TRegistroX400 }

  TRegistroX400 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X410 - Comércio Eletrônico

  { TRegistroX410 }

  TRegistroX410 = class
  private
    fIND_HOME_DISP: string;
    fIND_SERV_DISP: string;
    fPAIS: integer;
  public
    property PAIS: integer read fPAIS write fPAIS;
    property IND_HOME_DISP: string read fIND_HOME_DISP write fIND_HOME_DISP;
    property IND_SERV_DISP: string read fIND_SERV_DISP write fIND_SERV_DISP;
  end;

  /// Registro X420 - Royalties Recebidos ou Pagos a Beneficiários do
  /// Brasil e do Exterior

  { TRegistroX420 }

  TRegistroX420 = class
  private
    fPAIS:    integer;
    fTIP_ROY: string;
    fVL_EXPL_DIR_AUT: variant;
    fVL_EXPL_DIR_SW: variant;
    fVL_EXPL_FRANQ: variant;
    fVL_EXPL_INT: variant;
    fVL_EXPL_KNOW: variant;
    fVL_EXPL_MARCA: variant;
    fVL_EXPL_PAT: variant;
  public
    property TIP_ROY: string read fTIP_ROY write fTIP_ROY;
    property PAIS: integer read fPAIS write fPAIS;
    property VL_EXPL_DIR_SW: variant read fVL_EXPL_DIR_SW write fVL_EXPL_DIR_SW;
    property VL_EXPL_DIR_AUT: variant read fVL_EXPL_DIR_AUT write fVL_EXPL_DIR_AUT;
    property VL_EXPL_MARCA: variant read fVL_EXPL_MARCA write fVL_EXPL_MARCA;
    property VL_EXPL_PAT: variant read fVL_EXPL_PAT write fVL_EXPL_PAT;
    property VL_EXPL_KNOW: variant read fVL_EXPL_KNOW write fVL_EXPL_KNOW;
    property VL_EXPL_FRANQ: variant read fVL_EXPL_FRANQ write fVL_EXPL_FRANQ;
    property VL_EXPL_INT: variant read fVL_EXPL_INT write fVL_EXPL_INT;
  end;

  /// Registro X430 - Rendimentos Relativos a Serviços, Juros e
  /// Dividendos Recebidos do Brasil e do Exterior

  { TRegistroX430 }

  TRegistroX430 = class
  private
    fPAIS:     integer;
    fVL_DEMAIS_JUROS: variant;
    fVL_DIVID: variant;
    fVL_JURO:  variant;
    fVL_SERV_ASSIST: variant;
    fVL_SERV_SEM_ASSIST: variant;
    fVL_SERV_SEM_ASSIST_EXT: variant;
  public
    property PAIS: integer read fPAIS write fPAIS;
    property VL_SERV_ASSIST: variant read fVL_SERV_ASSIST write fVL_SERV_ASSIST;
    property VL_SERV_SEM_ASSIST: variant read fVL_SERV_SEM_ASSIST write fVL_SERV_SEM_ASSIST;
    property VL_SERV_SEM_ASSIST_EXT: variant read fVL_SERV_SEM_ASSIST_EXT
      write fVL_SERV_SEM_ASSIST_EXT;
    property VL_JURO: variant read fVL_JURO write fVL_JURO;
    property VL_DEMAIS_JUROS: variant read fVL_DEMAIS_JUROS write fVL_DEMAIS_JUROS;
    property VL_DIVID: variant read fVL_DIVID write fVL_DIVID;
  end;

  /// Registro X450 - Pagamentos/Remessas Relativos a Serviços, Juros e
  /// Dividendos Recebidos do Brasil e do Exterior

  { TRegistroX450 }

  TRegistroX450 = class
  private
    fPAIS: integer;
    fVL_DEMAIS_JUROS: variant;
    fVL_DIVID_PF: variant;
    fVL_DIVID_PJ: variant;
    fVL_JURO_PF: variant;
    fVL_JURO_PJ: variant;
    fVL_SERV_ASSIST: variant;
    fVL_SERV_SEM_ASSIST: variant;
    fVL_SERV_SEM_ASSIST_EXT: variant;
  public
    property PAIS: integer read fPAIS write fPAIS;
    property VL_SERV_ASSIST: variant read fVL_SERV_ASSIST write fVL_SERV_ASSIST;
    property VL_SERV_SEM_ASSIST: variant read fVL_SERV_SEM_ASSIST write fVL_SERV_SEM_ASSIST;
    property VL_SERV_SEM_ASSIST_EXT: variant read fVL_SERV_SEM_ASSIST_EXT
      write fVL_SERV_SEM_ASSIST_EXT;
    property VL_JURO_PF: variant read fVL_JURO_PF write fVL_JURO_PF;
    property VL_JURO_PJ: variant read fVL_JURO_PJ write fVL_JURO_PJ;
    property VL_DEMAIS_JUROS: variant read fVL_DEMAIS_JUROS write fVL_DEMAIS_JUROS;
    property VL_DIVID_PF: variant read fVL_DIVID_PF write fVL_DIVID_PF;
    property VL_DIVID_PJ: variant read fVL_DIVID_PJ write fVL_DIVID_PJ;
  end;

  /// Registro X460 - Inovação Tecnológica e Desenvolvimento Tecnológico

  { TRegistroX460 }

  TRegistroX460 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X470 - Capacitação de Informática e Inclusão Digital

  { TRegistroX470 }

  TRegistroX470 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X480 - Repes, Recap, Padis, PATVD, Reidi, Repenec,
  /// Reicomp, Retaero, Recine, Resíduos Sólidos,
  /// Recopa, Copa do Mundo, Retid, REPNBL-Redes,
  /// Reif e Olimpíadas

  { TRegistroX480 }

  TRegistroX480 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X490 - Pólo Industrial de Manaus e Amazônia Ocidental

  { TRegistroX490 }

  TRegistroX490 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X500 - Zonas de Processamento de Exportação (ZPE)

  { TRegistroX500 }

  TRegistroX500 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X510 - Áreas de Livre Comércio (ALC) F OC

  { TRegistroX510 }

  TRegistroX510 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro X990 - ENCERRAMENTO DO BLOCO X
  TRegistroX990 = class(TCloseBlocos)
  end;


implementation

end.
