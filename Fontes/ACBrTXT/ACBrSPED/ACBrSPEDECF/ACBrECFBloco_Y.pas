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

unit ACBrECFBloco_Y;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro Y001 - Abertura do Bloco Y – Informações Gerais
  TRegistroY001 = class(TOpenBlocos)
  private
  public
  end;

  /// Registro Y520 - Pagamentos/Recebimentos do Exterior ou de Não Residentes

  { TRegistroY520 }

  TRegistroY520 = class(TBlocos)
  private
    fFORMA:    integer;
    fNAT_OPER: integer;
    fPAIS:     integer;
    fTIP_EXT:  string;
    fVL_PERIODO: variant;
  public
    property TIP_EXT: string read fTIP_EXT write fTIP_EXT;
    property PAIS: integer read fPAIS write fPAIS;
    property FORMA: integer read fFORMA write fFORMA;
    property NAT_OPER: integer read fNAT_OPER write fNAT_OPER;
    property VL_PERIODO: variant read fVL_PERIODO write fVL_PERIODO;
  end;

  /// Registro Y540 - Discriminação da Receita de Vendas dos
  /// Estabelecimentos por Atividade Econômica

  { TRegistroY540 }

  TRegistroY540 = class(TBlocos)
  private
    fCNAE: integer;
    fCNPJ_ESTAB: string;
    fVL_REC_ESTAB: variant;
  public
    property CNPJ_ESTAB: string read fCNPJ_ESTAB write fCNPJ_ESTAB;
    property VL_REC_ESTAB: variant read fVL_REC_ESTAB write fVL_REC_ESTAB;
    property CNAE: integer read fCNAE write fCNAE;
  end;

  /// Registro Y550 - Vendas a Comercial Exportadora com Fim
  /// Específico de Exportação

  { TRegistroY550 }

  TRegistroY550 = class(TBlocos)
  private
    fCNPJ_EXP: string;
    fCOD_NCM:  integer;
    fVL_VENDA: variant;
  public
    property CNPJ_EXP: string read fCNPJ_EXP write fCNPJ_EXP;
    property COD_NCM: integer read fCOD_NCM write fCOD_NCM;
    property VL_VENDA: variant read fVL_VENDA write fVL_VENDA;
  end;

  /// Registro Y560 - Detalhamento das Exportações da Comercial Exportadora

  { TRegistroY560 }

  TRegistroY560 = class(TBlocos)
  private
    fCNPJ:      string;
    fCOD_NCM:   integer;
    fVL_COMPRA: variant;
    fVL_EXP:    variant;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property COD_NCM: integer read fCOD_NCM write fCOD_NCM;
    property VL_COMPRA: variant read fVL_COMPRA write fVL_COMPRA;
    property VL_EXP: variant read fVL_EXP write fVL_EXP;
  end;

  /// Registro Y570 - Demonstrativo do Imposto de Renda e CSLL Retidos na Fonte

  { TRegistroY570 }

  TRegistroY570 = class(TBlocos)
  private
    fCNPJ_FON:    string;
    fCOD_REC:     integer;
    fCSLL_RET:    variant;
    fIND_ORG_PUB: string;
    fIR_RET:      variant;
    fNOM_EMP:     string;
    fVL_REND:     variant;
  public
    property CNPJ_FON: string read fCNPJ_FON write fCNPJ_FON;
    property NOM_EMP: string read fNOM_EMP write fNOM_EMP;
    property IND_ORG_PUB: string read fIND_ORG_PUB write fIND_ORG_PUB;
    property COD_REC: integer read fCOD_REC write fCOD_REC;
    property VL_REND: variant read fVL_REND write fVL_REND;
    property IR_RET: variant read fIR_RET write fIR_RET;
    property CSLL_RET: variant read fCSLL_RET write fCSLL_RET;
  end;

  /// Registro Y580 - Doações a Campanhas Eleitorais

  { TRegistroY580 }

  TRegistroY580 = class(TBlocos)
  private
    fCNPJ:      string;
    fFORM_DOA:  integer;
    fTIP_BENEF: integer;
    fVL_DOA:    variant;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property TIP_BENEF: integer read fTIP_BENEF write fTIP_BENEF;
    property FORM_DOA: integer read fFORM_DOA write fFORM_DOA;
    property VL_DOA: variant read fVL_DOA write fVL_DOA;
  end;

  /// Registro Y590 - Ativos no Exterior

  { TRegistroY590 }

  TRegistroY590 = class(TBlocos)
  private
    fDISCRIMINACAO: string;
    fPAIS:      integer;
    fTIP_ATIVO: string;
    fVL_ANT:    variant;
    fVL_ATUAL:  variant;
  public
    property TIP_ATIVO: string read fTIP_ATIVO write fTIP_ATIVO;
    property PAIS: integer read fPAIS write fPAIS;
    property DISCRIMINACAO: string read fDISCRIMINACAO write fDISCRIMINACAO;
    property VL_ANT: variant read fVL_ANT write fVL_ANT;
    property VL_ATUAL: variant read fVL_ATUAL write fVL_ATUAL;
  end;

  /// Registro Y600 - Identificação de Sócios ou Titular

  { TRegistroY600 }

  TRegistroY600 = class(TBlocos)
  private
    fCPF_CNPJ: string;
    fCPF_REP_LEG: string;
    fDT_ALT_SOC: TDateTime;
    fDT_FIM_SOC: TDateTime;
    fIND_QUALIF_SOCIO: string;
    fNOM_EMP: string;
    fPAIS:   integer;
    fPERC_CAP_TOT: variant;
    fPERC_CAP_VOT: variant;
    fQUALIF: string;
    fQUALIF_REP_LEG: integer;
  public
    property DT_ALT_SOC: TDateTime read fDT_ALT_SOC write fDT_ALT_SOC;
    property DT_FIM_SOC: TDateTime read fDT_FIM_SOC write fDT_FIM_SOC;
    property PAIS: integer read fPAIS write fPAIS;
    property IND_QUALIF_SOCIO: string read fIND_QUALIF_SOCIO write fIND_QUALIF_SOCIO;
    property CPF_CNPJ: string read fCPF_CNPJ write fCPF_CNPJ;
    property NOM_EMP: string read fNOM_EMP write fNOM_EMP;
    property QUALIF: string read fQUALIF write fQUALIF;
    property PERC_CAP_TOT: variant read fPERC_CAP_TOT write fPERC_CAP_TOT;
    property PERC_CAP_VOT: variant read fPERC_CAP_VOT write fPERC_CAP_VOT;
    property CPF_REP_LEG: string read fCPF_REP_LEG write fCPF_REP_LEG;
    property QUALIF_REP_LEG: integer read fQUALIF_REP_LEG write fQUALIF_REP_LEG;
  end;

  /// Registro Y611 - Rendimentos de Dirigentes, Conselheiros, Sócios ou Titular

  { TRegistroY611 }

  TRegistroY611 = class(TBlocos)
  private
    fCPF_CNPJ:  string;
    fIND_PF_PJ: string;
    fNOM_EMP:   string;
    fPAIS:      integer;
    fQUALIF:    string;
    fVL_DEM_REND: variant;
    fVL_IR_RET: variant;
    fVL_JUR_CAP: variant;
    fVL_LUC_DIV: variant;
    fVL_REM_TRAB: variant;
  public
    property PAIS: integer read fPAIS write fPAIS;
    property IND_PF_PJ: string read fIND_PF_PJ write fIND_PF_PJ;
    property CPF_CNPJ: string read fCPF_CNPJ write fCPF_CNPJ;
    property NOM_EMP: string read fNOM_EMP write fNOM_EMP;
    property QUALIF: string read fQUALIF write fQUALIF;
    property VL_REM_TRAB: variant read fVL_REM_TRAB write fVL_REM_TRAB;
    property VL_LUC_DIV: variant read fVL_LUC_DIV write fVL_LUC_DIV;
    property VL_JUR_CAP: variant read fVL_JUR_CAP write fVL_JUR_CAP;
    property VL_DEM_REND: variant read fVL_DEM_REND write fVL_DEM_REND;
    property VL_IR_RET: variant read fVL_IR_RET write fVL_IR_RET;
  end;

  /// Registro Y612 - Rendimentos de Dirigentes e Conselheiros - Imunes ou Isentas

  { TRegistroY612 }

  TRegistroY612 = class(TBlocos)
  private
    fCPF:    string;
    fNOME:   string;
    fQUALIF: integer;
    fVL_DEM_REND: variant;
    fVL_IR_RET: variant;
    fVL_REM_TRAB: variant;
  public
    property CPF: string read fCPF write fCPF;
    property NOME: string read fNOME write fNOME;
    property QUALIF: integer read fQUALIF write fQUALIF;
    property VL_REM_TRAB: variant read fVL_REM_TRAB write fVL_REM_TRAB;
    property VL_DEM_REND: variant read fVL_DEM_REND write fVL_DEM_REND;
    property VL_IR_RET: variant read fVL_IR_RET write fVL_IR_RET;
  end;

  /// Registro Y620 - Participação Permanente em Coligadas ou Controladas

  { TRegistroY620 }

  TRegistroY620 = class(TBlocos)
  private
    fCNPJ:      string;
    fDATA_AQUIS: TDateTime;
    fDT_EVENTO: TDateTime;
    fIND_PROC_CART: string;
    fIND_PROC_RFB: string;
    fIND_RELAC: integer;
    fNOME_CART: string;
    fNOM_EMP:   string;
    fNUM_PROC_CART: string;
    fNUM_PROC_RFB: string;
    fPAIS:      integer;
    fPERC_CAP_TOT: variant;
    fPERC_CAP_VOT: variant;
    fRES_EQ_PAT: variant;
    fVALOR_ESTR: variant;
    fVALOR_REAIS: variant;
  public
    property DT_EVENTO: TDateTime read fDT_EVENTO write fDT_EVENTO;
    property IND_RELAC: integer read fIND_RELAC write fIND_RELAC;
    property PAIS: integer read fPAIS write fPAIS;
    property CNPJ: string read fCNPJ write fCNPJ;
    property NOM_EMP: string read fNOM_EMP write fNOM_EMP;
    property VALOR_REAIS: variant read fVALOR_REAIS write fVALOR_REAIS;
    property VALOR_ESTR: variant read fVALOR_ESTR write fVALOR_ESTR;
    property PERC_CAP_TOT: variant read fPERC_CAP_TOT write fPERC_CAP_TOT;
    property PERC_CAP_VOT: variant read fPERC_CAP_VOT write fPERC_CAP_VOT;
    property RES_EQ_PAT: variant read fRES_EQ_PAT write fRES_EQ_PAT;
    property DATA_AQUIS: TDateTime read fDATA_AQUIS write fDATA_AQUIS;
    property IND_PROC_CART: string read fIND_PROC_CART write fIND_PROC_CART;
    property NUM_PROC_CART: string read fNUM_PROC_CART write fNUM_PROC_CART;
    property NOME_CART: string read fNOME_CART write fNOME_CART;
    property IND_PROC_RFB: string read fIND_PROC_RFB write fIND_PROC_RFB;
    property NUM_PROC_RFB: string read fNUM_PROC_RFB write fNUM_PROC_RFB;
  end;

  /// Registro Y630 - Fundos/Clubes de Investimento

  { TRegistroY630 }

  TRegistroY630 = class(TBlocos)
  private
    fCNPJ:      string;
    fDAT_ABERT: TDateTime;
    fDAT_ENCER: TDateTime;
    fPATR_FIN_PER: variant;
    fQTE_QUOT:  integer;
    fQTE_QUOTA: integer;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property QTE_QUOT: integer read fQTE_QUOT write fQTE_QUOT;
    property QTE_QUOTA: integer read fQTE_QUOTA write fQTE_QUOTA;
    property PATR_FIN_PER: variant read fPATR_FIN_PER write fPATR_FIN_PER;
    property DAT_ABERT: TDateTime read fDAT_ABERT write fDAT_ABERT;
    property DAT_ENCER: TDateTime read fDAT_ENCER write fDAT_ENCER;
  end;

  /// Registro Y640 - Participações em Consórcios de Empresas

  { TRegistroY640 }

  TRegistroY640 = class(TBlocos)
  private
    fCNPJ:      string;
    fCNPJ_LID:  string;
    fCOND_DECL: integer;
    fVL_CONS:   variant;
    fVL_DECL:   variant;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property COND_DECL: integer read fCOND_DECL write fCOND_DECL;
    property VL_CONS: variant read fVL_CONS write fVL_CONS;
    property CNPJ_LID: string read fCNPJ_LID write fCNPJ_LID;
    property VL_DECL: variant read fVL_DECL write fVL_DECL;
  end;

  /// Registro Y650 - Participantes do Consórcio

  { TRegistroY650 }

  TRegistroY650 = class(TBlocos)
  private
    fCNPJ:    string;
    fVL_PART: variant;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property VL_PART: variant read fVL_PART write fVL_PART;
  end;

  /// Registro Y660 - Dados de Sucessoras

  { TRegistroY660 }

  TRegistroY660 = class(TBlocos)
  private
    fCNPJ:    string;
    fNOM_EMP: string;
    fPERC_PAT_LIQ: variant;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property NOM_EMP: string read fNOM_EMP write fNOM_EMP;
    property PERC_PAT_LIQ: variant read fPERC_PAT_LIQ write fPERC_PAT_LIQ;
  end;

  /// Registro Y665 - Demonstrativo das Diferenças na Adoção Inicial

  { TRegistroY665 }

  TRegistroY665 = class(TBlocos)
  private
    fCOD_CCUS:     string;
    fCOD_CCUS_SUB: string;
    fCOD_CTA:      string;
    fCOD_SUBCONT:  string;
    fDESC_CTA:     string;
    fDESC_SUB:     string;
    fDIF_SALDOS:   variant;
    fIND_DIF_SALDOS: string;
    fIND_VL_SALDO_FIS: string;
    fIND_VL_SALDO_SOC: string;
    fMET_CONTR:    string;
    fVL_SALDO_FIS: variant;
    fVL_SALDO_SOC: variant;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property DESC_CTA: string read fDESC_CTA write fDESC_CTA;
    property VL_SALDO_SOC: variant read fVL_SALDO_SOC write fVL_SALDO_SOC;
    property IND_VL_SALDO_SOC: string read fIND_VL_SALDO_SOC write fIND_VL_SALDO_SOC;
    property VL_SALDO_FIS: variant read fVL_SALDO_FIS write fVL_SALDO_FIS;
    property IND_VL_SALDO_FIS: string read fIND_VL_SALDO_FIS write fIND_VL_SALDO_FIS;
    property DIF_SALDOS: variant read fDIF_SALDOS write fDIF_SALDOS;
    property IND_DIF_SALDOS: string read fIND_DIF_SALDOS write fIND_DIF_SALDOS;
    property MET_CONTR: string read fMET_CONTR write fMET_CONTR;
    property COD_SUBCONT: string read fCOD_SUBCONT write fCOD_SUBCONT;
    property COD_CCUS_SUB: string read fCOD_CCUS_SUB write fCOD_CCUS_SUB;
    property DESC_SUB: string read fDESC_SUB write fDESC_SUB;
  end;

  /// Registro Y671 - Outras Informações

  { TRegistroY671 }

  TRegistroY671 = class(TBlocos)
  private
    fIND_ALTER_CAPITAL: integer;
    fIND_BCN_CSLL:   integer;
    fVL_ALIQ_RED:    variant;
    fVL_AQ_IMOBILIZADO: variant;
    fVL_AQ_MAQ:      variant;
    fVL_BX_IMOBILIZADO: variant;
    fVL_CSLL_DEPREC_INI: variant;
    fVL_DOA_CRIANCA: variant;
    fVL_DOA_IDOSO:   variant;
    fVL_FOLHA_ALIQ_RED: variant;
    fVL_INC_FIN:     variant;
    fVL_INC_INI:     variant;
    fVL_OC_SEM_IOF:  variant;
  public
    property VL_AQ_MAQ: variant read fVL_AQ_MAQ write fVL_AQ_MAQ;
    property VL_DOA_CRIANCA: variant read fVL_DOA_CRIANCA write fVL_DOA_CRIANCA;
    property VL_DOA_IDOSO: variant read fVL_DOA_IDOSO write fVL_DOA_IDOSO;
    property VL_AQ_IMOBILIZADO: variant read fVL_AQ_IMOBILIZADO write fVL_AQ_IMOBILIZADO;
    property VL_BX_IMOBILIZADO: variant read fVL_BX_IMOBILIZADO write fVL_BX_IMOBILIZADO;
    property VL_INC_INI: variant read fVL_INC_INI write fVL_INC_INI;
    property VL_INC_FIN: variant read fVL_INC_FIN write fVL_INC_FIN;
    property VL_CSLL_DEPREC_INI: variant read fVL_CSLL_DEPREC_INI write fVL_CSLL_DEPREC_INI;
    property VL_OC_SEM_IOF: variant read fVL_OC_SEM_IOF write fVL_OC_SEM_IOF;
    property VL_FOLHA_ALIQ_RED: variant read fVL_FOLHA_ALIQ_RED write fVL_FOLHA_ALIQ_RED;
    property VL_ALIQ_RED: variant read fVL_ALIQ_RED write fVL_ALIQ_RED;
    property IND_ALTER_CAPITAL: integer read fIND_ALTER_CAPITAL write fIND_ALTER_CAPITAL;
    property IND_BCN_CSLL: integer read fIND_BCN_CSLL write fIND_BCN_CSLL;
  end;

  /// Registro Y672 - Outras Informações (Lucro Presumido ou Lucro Arbitrado)

  { TRegistroY672 }

  TRegistroY672 = class(TBlocos)
  private
    fIND_AVAL_ESTOQ: string;
    fIND_REG_APUR: integer;
    fTOT_ATIVO:    variant;
    fVL_ALIQ_RED:  variant;
    fVL_APLIC_FIN: variant;
    fVL_APLIC_FIN_ANT: variant;
    fVL_CAIXA:     variant;
    fVL_CAIXA_ANT: variant;
    fVL_CAPITAL:   variant;
    fVL_CAPITAL_ANT: variant;
    fVL_COMPRA_ATIVO: variant;
    fVL_COMPRA_MERC: variant;
    fVL_CTA_PAG:   variant;
    fVL_CTA_PAG_ANT: variant;
    fVL_CTA_REC:   variant;
    fVL_CTA_REC_ANT: variant;
    fVL_ESTOQUES:  variant;
    fVL_ESTOQUE_ANT: variant;
    fVL_FOLHA:     variant;
    fVL_RECEITAS:  variant;
  public
    property VL_CAPITAL_ANT: variant read fVL_CAPITAL_ANT write fVL_CAPITAL_ANT;
    property VL_CAPITAL: variant read fVL_CAPITAL write fVL_CAPITAL;
    property VL_ESTOQUE_ANT: variant read fVL_ESTOQUE_ANT write fVL_ESTOQUE_ANT;
    property VL_ESTOQUES: variant read fVL_ESTOQUES write fVL_ESTOQUES;
    property VL_CAIXA_ANT: variant read fVL_CAIXA_ANT write fVL_CAIXA_ANT;
    property VL_CAIXA: variant read fVL_CAIXA write fVL_CAIXA;
    property VL_APLIC_FIN_ANT: variant read fVL_APLIC_FIN_ANT write fVL_APLIC_FIN_ANT;
    property VL_APLIC_FIN: variant read fVL_APLIC_FIN write fVL_APLIC_FIN;
    property VL_CTA_REC_ANT: variant read fVL_CTA_REC_ANT write fVL_CTA_REC_ANT;
    property VL_CTA_REC: variant read fVL_CTA_REC write fVL_CTA_REC;
    property VL_CTA_PAG_ANT: variant read fVL_CTA_PAG_ANT write fVL_CTA_PAG_ANT;
    property VL_CTA_PAG: variant read fVL_CTA_PAG write fVL_CTA_PAG;
    property VL_COMPRA_MERC: variant read fVL_COMPRA_MERC write fVL_COMPRA_MERC;
    property VL_COMPRA_ATIVO: variant read fVL_COMPRA_ATIVO write fVL_COMPRA_ATIVO;
    property VL_RECEITAS: variant read fVL_RECEITAS write fVL_RECEITAS;
    property TOT_ATIVO: variant read fTOT_ATIVO write fTOT_ATIVO;
    property VL_FOLHA: variant read fVL_FOLHA write fVL_FOLHA;
    property VL_ALIQ_RED: variant read fVL_ALIQ_RED write fVL_ALIQ_RED;
    property IND_REG_APUR: integer read fIND_REG_APUR write fIND_REG_APUR;
    property IND_AVAL_ESTOQ: string read fIND_AVAL_ESTOQ write fIND_AVAL_ESTOQ;
  end;

  /// Registro Y680 - Mês das Informações de Optantes pelo Refis (Lucro
  /// Real, Presumido e Arbitrado)

  { TRegistroY680 }

  TRegistroY680 = class(TBlocos)
  private
    fMES: string;
  public
    property MES: string read fMES write fMES;
  end;

  /// Registro Y681 - Informações de Optantes pelo Refis (Lucro Real,
  /// Presumido e Arbitrado)

  { TRegistroY681 }

  TRegistroY681 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro Y682 - Informações de Optantes pelo Refis - Imunes ou Isentas

  { TRegistroY682 }

  TRegistroY682 = class(TBlocos)
  private
    fACRES_PATR: variant;
    fMES: integer;
  public
    property MES: integer read fMES write fMES;
    property ACRES_PATR: variant read fACRES_PATR write fACRES_PATR;
  end;

  /// Registro Y690 - Informações de Optantes pelo Paes

  { TRegistroY690 }

  TRegistroY690 = class(TBlocos)
  private
    fMES: integer;
    fVL_REC_BRU: variant;
  public
    property MES: integer read fMES write fMES;
    property VL_REC_BRU: variant read fVL_REC_BRU write fVL_REC_BRU;
  end;

  /// Registro Y800 - Outras Informações

  TRegistroY800 = class(TBlocos)
  private
    fARQ_RTF: string;
    function GetIND_FIM_RTF: string;
  public
    property ARQ_RTF: string read fARQ_RTF write fARQ_RTF;
    property IND_FIM_RTF: string read GetIND_FIM_RTF;
  end;

  /// Registro Y800 - Lista

  TRegistroY800List = class(TObjectList)
  private
    function GetItem(Index: integer): TRegistroY800; /// GetItem
    procedure SetItem(Index: integer; const Value: TRegistroY800); /// SetItem
  public
    function New: TRegistroY800;
    property Items[Index: integer]: TRegistroY800 read GetItem write SetItem;
  end;

  /// Registro Y990 - ENCERRAMENTO DO BLOCO Y
  TRegistroY990 = class(TCloseBlocos)
  end;

implementation

{ TRegistroY800List }

function TRegistroY800List.GetItem(Index: integer): TRegistroY800;
begin
  Result := TRegistroY800(inherited Items[Index]);
end;

procedure TRegistroY800List.SetItem(Index: integer; const Value: TRegistroY800);
begin
  Put(Index, Value);
end;

function TRegistroY800List.New: TRegistroY800;
begin
  Result := TRegistroY800.Create;
  Add(Result);
end;

{ TRegistroY800 }

function TRegistroY800.GetIND_FIM_RTF: string;
begin
  Result := 'Y800FIM';
end;

end.
