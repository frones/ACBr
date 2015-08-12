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

unit ACBrECFBloco_C;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Abertura do Bloco C – Informações Recuperadas da ECD
  TRegistroC001 = class(TOpenBlocos)
  end;

  /// Identificador da ECD

  { TRegistroC040 }

  TRegistroC040 = class(TBlocos)
  private
    fDT_INI:   TDate;
    fHASH_ECD: string;
    fCNPJ:     string;
    fDT_FIN:   TDate;
    fIND_ESC:  string;
    fIND_SIT_ESP: integer;
    fNIRE:     integer;
    fNAT_LIVR: string;
    fNUM_ORD:  integer;
    fCOD_VER_LC: string;
  public
    property HASH_ECD: string read fHASH_ECD write fHASH_ECD;
    property DT_INI: TDate read fDT_INI write fDT_INI;
    property DT_FIN: TDate read fDT_FIN write fDT_FIN;
    property IND_SIT_ESP: integer read fIND_SIT_ESP write fIND_SIT_ESP;
    property CNPJ: string read fCNPJ write fCNPJ;
    property NUM_ORD: integer read fNUM_ORD write fNUM_ORD;
    property NIRE: integer read fNIRE write fNIRE;
    property NAT_LIVR: string read fNAT_LIVR write fNAT_LIVR;
    property COD_VER_LC: string read fCOD_VER_LC write fCOD_VER_LC;
    property IND_ESC: string read fIND_ESC write fIND_ESC;
  end;

  /// Plano de Contas da ECD
  TRegistroC050 = class(TBlocos)
  private
    fCOD_CONTA: string;
    fDT_ALT:  TDate;
    fCTA:     string;
    fCOD_CTA_SUP: string;
    fIND_CTA: string;
    fNIVEL:   integer;
    fCOD_NAT: string;
  public
    property DT_ALT: TDate read fDT_ALT write fDT_ALT;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property IND_CTA: string read fIND_CTA write fIND_CTA;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_CONTA: string read fCOD_CONTA write fCOD_CONTA;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property CTA: string read fCTA write fCTA;
  end;

  /// Plano de Contas Referencial
  TRegistroC051 = class(TBlocos)
  private
    fCOD_CCUS:    string;
    fCOD_CTA_REF: string;
    fCOD_ENT_REF: string;
  public
    property COD_ENT_REF: string read fCOD_ENT_REF write fCOD_ENT_REF;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
  end;

  /// Subcontas Correlatas
  TRegistroC053 = class(TBlocos)
  private
    fCOD_CNT_CORR: string;
    fNAT_SUB_CNT:  string;
    fCOD_IDT:      string;
  public
    property COD_IDT: string read fCOD_IDT write fCOD_IDT;
    property COD_CNT_CORR: string read fCOD_CNT_CORR write fCOD_CNT_CORR;
    property NAT_SUB_CNT: string read fNAT_SUB_CNT write fNAT_SUB_CNT;
  end;

  /// Centro de Custos
  TRegistroC100 = class(TBlocos)
  private
    fDT_ALT:   TDate;
    fCOD_CCUS: string;
    fCCUS:     string;
  public
    property DT_ALT: TDate read fDT_ALT write fDT_ALT;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property CCUS: string read fCCUS write fCCUS;

  end;

  /// Identificação do Período dos Saldos Periódicos das Contas Patrimoniais
  TRegistroC150 = class(TBlocos)
  private
    fDT_FIN: TDate;
    fDT_INI: TDate;
  public
    property DT_INI: TDate read fDT_INI write fDT_INI;
    property DT_FIN: TDate read fDT_FIN write fDT_FIN;
  end;

  /// Detalhes dos Saldos Contábeis das Contas Patrimoniais
  TRegistroC155 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_INI: currency;
    fCOD_CCUS:   string;
    fVL_SLD_FIN: currency;
    fLINHA_ECD:  integer;
    fVL_CRED:    currency;
    fVL_DEB:     currency;
    fIND_VL_SLD_FIN: string;
    fIND_VL_SLD_INI: string;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: currency read fVL_SLD_INI write fVL_SLD_INI;
    property IND_VL_SLD_INI: string read fIND_VL_SLD_INI write fIND_VL_SLD_INI;
    property VL_DEB: currency read fVL_DEB write fVL_DEB;
    property VL_CRED: currency read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: currency read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;
  end;

  /// Transferência de Saldos do Plano de Contas Anterior
  TRegistroC157 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_FIN: variant;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
    fLINHA_ECD:  integer;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;
  end;

  /// Identificação da Data dos Saldos das Contas de Resultado Antes do Encerramento
  TRegistroC350 = class(TBlocos)
  private
    fDT_RES: TDate;
  public
    property DT_RES: TDate read fDT_RES write fDT_RES;
  end;

  /// Detalhes dos Saldos das Contas de Resultado Antes do Encerramento
  TRegistroC355 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_CTA:     Variant;
    fCOD_CCUS:   string;
    fIND_VL_CTA: string;
    fLINHA_ECD:  integer;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: Variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;
  end;

  /// Registro C990 - ENCERRAMENTO DO BLOCO C
  TRegistroC990 = class(TCloseBlocos)
  end;

implementation

end.
