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

unit ACBrECFBloco_M;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro M001 - Abertura do Bloco M – Livro Eletrônico de
  /// Apuração do Lucro Real (e-Lalur) e Licro Eletrônico
  /// de Apuração da Base de Cálculo da CSLL (e-Lacs)
  TRegistroM001 = class(TOpenBlocos)
  end;

  /// Registro M010 - Identificação da Conta na Parte B e-Lalur e do e-Lacs

  { TRegistroM010 }

  TRegistroM010 = class(TBlocos)
  private
    fCNPJ_SIT_ESP: string;
    fIND_Vl_SALDO_INI: string;
    fVl_SALDO_INI: variant;
    fDESC_LAN_ORIG: string;
    fDESC_CTA_LAL: string;
    fDT_LIM_LAL:   TDateTime;
    fDT_AP_LAL:    TDateTime;
    fCOD_LAN_ORIG: integer;
    fCOD_TRIBUTO:  integer;
    fCOD_CTA_B:    string;
  public
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property DESC_CTA_LAL: string read fDESC_CTA_LAL write fDESC_CTA_LAL;
    property DT_AP_LAL: TDateTime read fDT_AP_LAL write fDT_AP_LAL;
    property COD_LAN_ORIG: integer read fCOD_LAN_ORIG write fCOD_LAN_ORIG;
    property DESC_LAN_ORIG: string read fDESC_LAN_ORIG write fDESC_LAN_ORIG;
    property DT_LIM_LAL: TDateTime read fDT_LIM_LAL write fDT_LIM_LAL;
    property COD_TRIBUTO: integer read fCOD_TRIBUTO write fCOD_TRIBUTO;
    property Vl_SALDO_INI: variant read fVl_SALDO_INI write fVl_SALDO_INI;
    property IND_Vl_SALDO_INI: string read fIND_Vl_SALDO_INI write fIND_Vl_SALDO_INI;
    property CNPJ_SIT_ESP: string read fCNPJ_SIT_ESP write fCNPJ_SIT_ESP;
  end;

  /// Registro M030 - Identificação do Período e Forma de Apuração do
  /// IRPJ e da CSLL das Empresas Tributadas pelo Lucro Real
  TRegistroM030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fPER_APUR: string;
    fDT_INI:   TDateTime;
  public
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;
  end;

  /// Registro M300 - Lançamentos da Parte A do e-Lalur

  { TRegistroM300 }

  TRegistroM300 = class(TBlocos)
  private
    fHIST_LAN_LAL: string;
    fIND_RELACAO: integer;
    fCODIGO:    string;
    fTIPO_LANCAMENTO: string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO_LANCAMENTO: string read fTIPO_LANCAMENTO write fTIPO_LANCAMENTO;
    property IND_RELACAO: integer read fIND_RELACAO write fIND_RELACAO;
    property VALOR: variant read fVALOR write fVALOR;
    property HIST_LAN_LAL: string read fHIST_LAN_LAL write fHIST_LAN_LAL;
  end;

  /// Registro M305 - Conta da Parte B do e-Lalur
  TRegistroM305 = class(TBlocos)
  private
    fVL_CTA:     variant;
    fIND_VL_CTA: string;
    fCOD_CTA_B:  string;
  public
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
  end;

  /// Registro M310 - Contas Contábeis Relacionadas ao Lançamento da Parte A do e-Lalur.
  TRegistroM310 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fCOD_CCUS:   string;
    fVL_CTA:     variant;
    fIND_VL_CTA: string;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
  end;

  /// Registro M312 - Números dos Lançamentos Relacionados à Conta Contábil
  TRegistroM312 = class(TBlocos)
  private
    fNUM_LCTO: string;
  public
    property NUM_LCTO: string read fNUM_LCTO write fNUM_LCTO;
  end;

  /// Registro M315 - Identificação de Processos Judiciais e
  /// Administrativos Referentes ao Lançamento
  TRegistroM315 = class(TBlocos)
  private
    fIND_PROC: string;
    fNUM_PROC: string;
  public
    property IND_PROC: string read fIND_PROC write fIND_PROC;
    property NUM_PROC: string read fNUM_PROC write fNUM_PROC;
  end;

  /// Registro M350 - Lançamentos da Parte A do e-Lacs

  { TRegistroM350 }

  TRegistroM350 = class(TBlocos)
  private
    fHIST_LAN_LAL: string;
    fVALOR:     variant;
    fCODIGO:    string;
    fDESCRICAO: string;
    fTIPO_LANCAMENTO: string;
    fIND_RELACAO: integer;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO_LANCAMENTO: string read fTIPO_LANCAMENTO write fTIPO_LANCAMENTO;
    property IND_RELACAO: integer read fIND_RELACAO write fIND_RELACAO;
    property VALOR: variant read fVALOR write fVALOR;
    property HIST_LAN_LAL: string read fHIST_LAN_LAL write fHIST_LAN_LAL;
  end;

  /// Registro M355 - Conta da Parte B do e-Lacs

  { TRegistroM355 }

  TRegistroM355 = class(TBlocos)
  private
    fVL_CTA:     variant;
    fIND_VL_CTA: string;
    fCOD_CTA_B:  string;
  public
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
  end;

  /// Registro M360 - Contas Contábeis Relacionadas ao Lançamento da
  /// Parte A do e-Lacs.

  { TRegistroM360 }

  TRegistroM360 = class(TBlocos)
  private
    fCOD_CCUS:   string;
    fCOD_CONTA:  string;
    fCOD_CENTRO_CUSTOS: string;
    fCOD_CTA:    string;
    fIND_VL_CTA: string;
    fVL_CTA:     variant;
    fIND_VALOR_CONTA: string;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
  end;

  /// Registro M362 - Números dos Lançamentos Relacionados à Conta
  /// Contábil

  { TRegistroM362 }

  TRegistroM362 = class(TBlocos)
  private
    fNUM_LCTO: string;
  public
    property NUM_LCTO: string read fNUM_LCTO write fNUM_LCTO;
  end;

  /// Registro M365 - Identificação de Processos Judiciais e
  /// Administrativos Referentes ao Lançamento

  { TRegistroM365 }

  TRegistroM365 = class(TBlocos)
  private
    fIND_PROC: string;
    fNUM_PROC: string;
  public
    property IND_PROC: string read fIND_PROC write fIND_PROC;
    property NUM_PROC: string read fNUM_PROC write fNUM_PROC;
  end;

  /// Registro M410 - Lançamentos na Conta da Parte B do e-Lalur e do e-
  /// Lacs Sem Reflexo na Parte A

  { TRegistroM410 }

  TRegistroM410 = class(TBlocos)
  private
    fCOD_CTA_B:     string;
    fCOD_CTA_B_CTP: string;
    fCOD_TRIBUTO:   string;
    fHIST_LAN_LALB: string;
    fIND_LAN_ANT:   string;
    fIND_VAL_LAN_LALB_PB: string;
    fVAL_LAN_LALB_PB: variant;
  public
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property COD_TRIBUTO: string read fCOD_TRIBUTO write fCOD_TRIBUTO;
    property VAL_LAN_LALB_PB: variant read fVAL_LAN_LALB_PB write fVAL_LAN_LALB_PB;
    property IND_VAL_LAN_LALB_PB: string read fIND_VAL_LAN_LALB_PB write fIND_VAL_LAN_LALB_PB;
    property COD_CTA_B_CTP: string read fCOD_CTA_B_CTP write fCOD_CTA_B_CTP;
    property HIST_LAN_LALB: string read fHIST_LAN_LALB write fHIST_LAN_LALB;
    property IND_LAN_ANT: string read fIND_LAN_ANT write fIND_LAN_ANT;
  end;

  /// Registro M415 - Identificação de Processos Judiciais e
  /// Administrativos Referentes ao Lançamento

  { TRegistroM415 }

  TRegistroM415 = class(TBlocos)
  private
    fIND_PROC: string;
    fNUM_PROC: string;
  public
    property IND_PROC: string read fIND_PROC write fIND_PROC;
    property NUM_PROC: string read fNUM_PROC write fNUM_PROC;
  end;

  /// Registro M500 - Controle de Saldos das Contas da Parte B do e-Lalur
  /// e do e-Lacs

  { TRegistroM500 }

  TRegistroM500 = class(TBlocos)
  private
    fCOD_CTA_B:      string;
    fCOD_TRIBUTO:    string;
    fIND_SD_FIM_LAL: string;
    fIND_SD_INI_LAL: string;
    fIND_VL_LCTO_PARTE_A: variant;
    fIND_VL_LCTO_PARTE_B: string;
    fSD_FIM_LAL:     variant;
    fSD_INI_LAL:     variant;
    fVL_LCTO_PARTE_A: variant;
    fVL_LCTO_PARTE_B: variant;
  public
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property COD_TRIBUTO: string read fCOD_TRIBUTO write fCOD_TRIBUTO;
    property SD_INI_LAL: variant read fSD_INI_LAL write fSD_INI_LAL;
    property IND_SD_INI_LAL: string read fIND_SD_INI_LAL write fIND_SD_INI_LAL;
    property VL_LCTO_PARTE_A: variant read fVL_LCTO_PARTE_A write fVL_LCTO_PARTE_A;
    property IND_VL_LCTO_PARTE_A: variant read fIND_VL_LCTO_PARTE_A write fIND_VL_LCTO_PARTE_A;
    property VL_LCTO_PARTE_B: variant read fVL_LCTO_PARTE_B write fVL_LCTO_PARTE_B;
    property IND_VL_LCTO_PARTE_B: string read fIND_VL_LCTO_PARTE_B write fIND_VL_LCTO_PARTE_B;
    property SD_FIM_LAL: variant read fSD_FIM_LAL write fSD_FIM_LAL;
    property IND_SD_FIM_LAL: string read fIND_SD_FIM_LAL write fIND_SD_FIM_LAL;
  end;

  /// Registro M990 - ENCERRAMENTO DO BLOCO M
  TRegistroM990 = class(TCloseBlocos)
  end;


implementation

end.
