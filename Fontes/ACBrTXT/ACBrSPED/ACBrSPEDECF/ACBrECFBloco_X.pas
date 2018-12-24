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
|* 27/08/2015 - Ariel Guareschi - Alterado a geração do arquivo bloco X        
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_X;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroX280List = class;
  TRegistroX291List = class;
  TRegistroX292List = class;
  TRegistroX300List = class;
  TRegistroX310List = class;
  TRegistroX320List = class;
  TRegistroX330List = class;
  TRegistroX340List = class;
  TRegistroX350List = class;
  TRegistroX351List = class;
  TRegistroX352List = class;
  TRegistroX353List = class;
  TRegistroX354List = class;
  TRegistroX355List = class;
  TRegistroX356List = class;
  TRegistroX390List = class;
  TRegistroX400List = class;
  TRegistroX410List = class;
  TRegistroX420List = class;
  TRegistroX430List = class;
  TRegistroX450List = class;
  TRegistroX460List = class;
  TRegistroX470List = class;
  TRegistroX480List = class;
  TRegistroX490List = class;
  TRegistroX500List = class;
  TRegistroX510List = class;

  /// Registro X001 - Abertura do Bloco X – Informações Econômicas
  TRegistroX001 = class(TOpenBlocos)
  private
    FRegistroX280 : TRegistroX280List;
    FRegistroX291 : TRegistroX291List;
    FRegistroX292 : TRegistroX292List;
    FRegistroX300 : TRegistroX300List;
    FRegistroX320 : TRegistroX320List;
    FRegistroX340 : TRegistroX340List;
    FRegistroX390 : TRegistroX390List;
    FRegistroX400 : TRegistroX400List;
    FRegistroX410 : TRegistroX410List;
    FRegistroX420 : TRegistroX420List;
    FRegistroX430 : TRegistroX430List;
    FRegistroX450 : TRegistroX450List;
    FRegistroX460 : TRegistroX460List;
    FRegistroX470 : TRegistroX470List;
    FRegistroX480 : TRegistroX480List;
    FRegistroX490 : TRegistroX490List;
    FRegistroX500 : TRegistroX500List;
    FRegistroX510 : TRegistroX510List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy
      
    property RegistroX280 : TRegistroX280List read FRegistroX280 write FRegistroX280;
    property RegistroX291 : TRegistroX291List read FRegistroX291 write FRegistroX291;
    property RegistroX292 : TRegistroX292List read FRegistroX292 write FRegistroX292;
    property RegistroX300 : TRegistroX300List read FRegistroX300 write FRegistroX300;
    property RegistroX320 : TRegistroX320List read FRegistroX320 write FRegistroX320;
    property RegistroX340 : TRegistroX340List read FRegistroX340 write FRegistroX340;
    property RegistroX390 : TRegistroX390List read FRegistroX390 write FRegistroX390;
    property RegistroX400 : TRegistroX400List read FRegistroX400 write FRegistroX400;
    property RegistroX410 : TRegistroX410List read FRegistroX410 write FRegistroX410;
    property RegistroX420 : TRegistroX420List read FRegistroX420 write FRegistroX420;
    property RegistroX430 : TRegistroX430List read FRegistroX430 write FRegistroX430;
    property RegistroX450 : TRegistroX450List read FRegistroX450 write FRegistroX450;
    property RegistroX460 : TRegistroX460List read FRegistroX460 write FRegistroX460;
    property RegistroX470 : TRegistroX470List read FRegistroX470 write FRegistroX470;
    property RegistroX480 : TRegistroX480List read FRegistroX480 write FRegistroX480;
    property RegistroX490 : TRegistroX490List read FRegistroX490 write FRegistroX490;
    property RegistroX500 : TRegistroX500List read FRegistroX500 write FRegistroX500;
    property RegistroX510 : TRegistroX510List read FRegistroX510 write FRegistroX510;
  end;

  TRegistroX001List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX001;
    procedure SetItem(Index: Integer; const Value: TRegistroX001);
  public
    function New: TRegistroX001;
    property Items[Index: Integer]: TRegistroX001 read GetItem write SetItem;
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

  TRegistroX280List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX280;
    procedure SetItem(Index: Integer; const Value: TRegistroX280);
  public
    function New: TRegistroX280;
    property Items[Index: Integer]: TRegistroX280 read GetItem write SetItem;
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

  TRegistroX291List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX291;
    procedure SetItem(Index: Integer; const Value: TRegistroX291);
  public
    function New: TRegistroX291;
    property Items[Index: Integer]: TRegistroX291 read GetItem write SetItem;
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

  TRegistroX292List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX292;
    procedure SetItem(Index: Integer; const Value: TRegistroX292);
  public
    function New: TRegistroX292;
    property Items[Index: Integer]: TRegistroX292 read GetItem write SetItem;
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

    FRegistroX310: TRegistroX310List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

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

    property RegistroX310: TRegistroX310List read FRegistroX310 write FRegistroX310;
  end;

  TRegistroX300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX300;
    procedure SetItem(Index: Integer; const Value: TRegistroX300);
  public
    function New: TRegistroX300;
    property Items[Index: Integer]: TRegistroX300 read GetItem write SetItem;
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

  TRegistroX310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX310;
    procedure SetItem(Index: Integer; const Value: TRegistroX310);
  public
    function New: TRegistroX310;
    property Items[Index: Integer]: TRegistroX310 read GetItem write SetItem;
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

    FRegistroX330: TRegistroX330List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

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

    property RegistroX330: TRegistroX330List read FRegistroX330 write FRegistroX330;
  end;

  TRegistroX320List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX320;
    procedure SetItem(Index: Integer; const Value: TRegistroX320);
  public
    function New: TRegistroX320;
    property Items[Index: Integer]: TRegistroX320 read GetItem write SetItem;
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

  TRegistroX330List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX330;
    procedure SetItem(Index: Integer; const Value: TRegistroX330);
  public
    function New: TRegistroX330;
    property Items[Index: Integer]: TRegistroX330 read GetItem write SetItem;
  end;

  /// Registro X340 - Identificação da Participação no Exterior

  { TRegistroX340 }

  TRegistroX340 = class
  private
    fIND_CONSOL: string;
    fIND_CONTROLE: integer;
    fIND_ISEN_PETR: string;
    fMOT_NAO_CONSOL: integer;
    fNIF:  string;
    fPAIS: integer;
    fRAZ_SOCIAL: string;

    FRegistroX350: TRegistroX350List;
    FRegistroX351: TRegistroX351List;
    FRegistroX352: TRegistroX352List;
    FRegistroX353: TRegistroX353List;
    FRegistroX354: TRegistroX354List;
    FRegistroX355: TRegistroX355List;
    FRegistroX356: TRegistroX356List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RAZ_SOCIAL: string read fRAZ_SOCIAL write fRAZ_SOCIAL;
    property NIF: string read fNIF write fNIF;
    property IND_CONTROLE: integer read fIND_CONTROLE write fIND_CONTROLE;
    property PAIS: integer read fPAIS write fPAIS;
    property IND_ISEN_PETR: string read fIND_ISEN_PETR write fIND_ISEN_PETR;
    property IND_CONSOL: string read fIND_CONSOL write fIND_CONSOL;
    property MOT_NAO_CONSOL: integer read fMOT_NAO_CONSOL write fMOT_NAO_CONSOL;

    property RegistroX350: TRegistroX350List read FRegistroX350 write FRegistroX350;
    property RegistroX351: TRegistroX351List read FRegistroX351 write FRegistroX351;
    property RegistroX352: TRegistroX352List read FRegistroX352 write FRegistroX352;
    property RegistroX353: TRegistroX353List read FRegistroX353 write FRegistroX353;
    property RegistroX354: TRegistroX354List read FRegistroX354 write FRegistroX354;
    property RegistroX355: TRegistroX355List read FRegistroX355 write FRegistroX355;
    property RegistroX356: TRegistroX356List read FRegistroX356 write FRegistroX356;
  end;

  TRegistroX340List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX340;
    procedure SetItem(Index: Integer; const Value: TRegistroX340);
  public
    function New: TRegistroX340;
    property Items[Index: Integer]: TRegistroX340 read GetItem write SetItem;
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

  TRegistroX350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX350;
    procedure SetItem(Index: Integer; const Value: TRegistroX350);
  public
    function New: TRegistroX350;
    property Items[Index: Integer]: TRegistroX350 read GetItem write SetItem;
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

  TRegistroX351List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX351;
    procedure SetItem(Index: Integer; const Value: TRegistroX351);
  public
    function New: TRegistroX351;
    property Items[Index: Integer]: TRegistroX351 read GetItem write SetItem;
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

  TRegistroX352List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX352;
    procedure SetItem(Index: Integer; const Value: TRegistroX352);
  public
    function New: TRegistroX352;
    property Items[Index: Integer]: TRegistroX352 read GetItem write SetItem;
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

  TRegistroX353List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX353;
    procedure SetItem(Index: Integer; const Value: TRegistroX353);
  public
    function New: TRegistroX353;
    property Items[Index: Integer]: TRegistroX353 read GetItem write SetItem;
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

  TRegistroX354List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX354;
    procedure SetItem(Index: Integer; const Value: TRegistroX354);
  public
    function New: TRegistroX354;
    property Items[Index: Integer]: TRegistroX354 read GetItem write SetItem;
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

  TRegistroX355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX355;
    procedure SetItem(Index: Integer; const Value: TRegistroX355);
  public
    function New: TRegistroX355;
    property Items[Index: Integer]: TRegistroX355 read GetItem write SetItem;
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

  TRegistroX356List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX356;
    procedure SetItem(Index: Integer; const Value: TRegistroX356);
  public
    function New: TRegistroX356;
    property Items[Index: Integer]: TRegistroX356 read GetItem write SetItem;
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

  TRegistroX390List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX390;
    procedure SetItem(Index: Integer; const Value: TRegistroX390);
  public
    function New: TRegistroX390;
    property Items[Index: Integer]: TRegistroX390 read GetItem write SetItem;
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

  TRegistroX400List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX400;
    procedure SetItem(Index: Integer; const Value: TRegistroX400);
  public
    function New: TRegistroX400;
    property Items[Index: Integer]: TRegistroX400 read GetItem write SetItem;
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

  TRegistroX410List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX410;
    procedure SetItem(Index: Integer; const Value: TRegistroX410);
  public
    function New: TRegistroX410;
    property Items[Index: Integer]: TRegistroX410 read GetItem write SetItem;
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

  TRegistroX420List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX420;
    procedure SetItem(Index: Integer; const Value: TRegistroX420);
  public
    function New: TRegistroX420;
    property Items[Index: Integer]: TRegistroX420 read GetItem write SetItem;
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

  TRegistroX430List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX430;
    procedure SetItem(Index: Integer; const Value: TRegistroX430);
  public
    function New: TRegistroX430;
    property Items[Index: Integer]: TRegistroX430 read GetItem write SetItem;
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

  TRegistroX450List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX450;
    procedure SetItem(Index: Integer; const Value: TRegistroX450);
  public
    function New: TRegistroX450;
    property Items[Index: Integer]: TRegistroX450 read GetItem write SetItem;
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

  TRegistroX460List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX460;
    procedure SetItem(Index: Integer; const Value: TRegistroX460);
  public
    function New: TRegistroX460;
    property Items[Index: Integer]: TRegistroX460 read GetItem write SetItem;
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

  TRegistroX470List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX470;
    procedure SetItem(Index: Integer; const Value: TRegistroX470);
  public
    function New: TRegistroX470;
    property Items[Index: Integer]: TRegistroX470 read GetItem write SetItem;
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

  TRegistroX480List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX480;
    procedure SetItem(Index: Integer; const Value: TRegistroX480);
  public
    function New: TRegistroX480;
    property Items[Index: Integer]: TRegistroX480 read GetItem write SetItem;
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

  TRegistroX490List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX490;
    procedure SetItem(Index: Integer; const Value: TRegistroX490);
  public
    function New: TRegistroX490;
    property Items[Index: Integer]: TRegistroX490 read GetItem write SetItem;
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

  TRegistroX500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX500;
    procedure SetItem(Index: Integer; const Value: TRegistroX500);
  public
    function New: TRegistroX500;
    property Items[Index: Integer]: TRegistroX500 read GetItem write SetItem;
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

  TRegistroX510List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX510;
    procedure SetItem(Index: Integer; const Value: TRegistroX510);
  public
    function New: TRegistroX510;
    property Items[Index: Integer]: TRegistroX510 read GetItem write SetItem;
  end;

  /// Registro X990 - ENCERRAMENTO DO BLOCO X
  TRegistroX990 = class(TCloseBlocos)
  private
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;


implementation

{ TRegistroX001List }

function TRegistroX001List.GetItem(Index: Integer): TRegistroX001;
begin
  Result := TRegistroX001(Inherited Items[Index]);
end;

function TRegistroX001List.New: TRegistroX001;
begin
  Result := TRegistroX001.Create;
  Add(Result);
end;

procedure TRegistroX001List.SetItem(Index: Integer; const Value: TRegistroX001);
begin
  Put(Index, Value);
end;

{ TRegistroX280List }

function TRegistroX280List.GetItem(Index: Integer): TRegistroX280;
begin
  Result := TRegistroX280(Inherited Items[Index]);
end;

function TRegistroX280List.New: TRegistroX280;
begin
  Result := TRegistroX280.Create;
  Add(Result);
end;

procedure TRegistroX280List.SetItem(Index: Integer; const Value: TRegistroX280);
begin
  Put(Index, Value);
end;

{ TRegistroX291List }

function TRegistroX291List.GetItem(Index: Integer): TRegistroX291;
begin
  Result := TRegistroX291(Inherited Items[Index]);
end;

function TRegistroX291List.New: TRegistroX291;
begin
  Result := TRegistroX291.Create;
  Add(Result);
end;

procedure TRegistroX291List.SetItem(Index: Integer; const Value: TRegistroX291);
begin
  Put(Index, Value);
end;

{ TRegistroX292List }

function TRegistroX292List.GetItem(Index: Integer): TRegistroX292;
begin
  Result := TRegistroX292(Inherited Items[Index]);
end;

function TRegistroX292List.New: TRegistroX292;
begin
  Result := TRegistroX292.Create;
  Add(Result);
end;

procedure TRegistroX292List.SetItem(Index: Integer; const Value: TRegistroX292);
begin
  Put(Index, Value);
end;

{ TRegistroX300List }

function TRegistroX300List.GetItem(Index: Integer): TRegistroX300;
begin
  Result := TRegistroX300(Inherited Items[Index]);
end;

function TRegistroX300List.New: TRegistroX300;
begin
  Result := TRegistroX300.Create;
  Add(Result);
end;

procedure TRegistroX300List.SetItem(Index: Integer; const Value: TRegistroX300);
begin
  Put(Index, Value);
end;

{ TRegistroX310List }

function TRegistroX310List.GetItem(Index: Integer): TRegistroX310;
begin
  Result := TRegistroX310(Inherited Items[Index]);
end;

function TRegistroX310List.New: TRegistroX310;
begin
  Result := TRegistroX310.Create;
  Add(Result);
end;

procedure TRegistroX310List.SetItem(Index: Integer; const Value: TRegistroX310);
begin
  Put(Index, Value);
end;

{ TRegistroX320List }

function TRegistroX320List.GetItem(Index: Integer): TRegistroX320;
begin
  Result := TRegistroX320(Inherited Items[Index]);
end;

function TRegistroX320List.New: TRegistroX320;
begin
  Result := TRegistroX320.Create;
  Add(Result);
end;

procedure TRegistroX320List.SetItem(Index: Integer; const Value: TRegistroX320);
begin
  Put(Index, Value);
end;

{ TRegistroX330List }

function TRegistroX330List.GetItem(Index: Integer): TRegistroX330;
begin
  Result := TRegistroX330(Inherited Items[Index]);
end;

function TRegistroX330List.New: TRegistroX330;
begin
  Result := TRegistroX330.Create;
  Add(Result);
end;

procedure TRegistroX330List.SetItem(Index: Integer; const Value: TRegistroX330);
begin
  Put(Index, Value);
end;

{ TRegistroX340List }

function TRegistroX340List.GetItem(Index: Integer): TRegistroX340;
begin
  Result := TRegistroX340(Inherited Items[Index]);
end;

function TRegistroX340List.New: TRegistroX340;
begin
  Result := TRegistroX340.Create;
  Add(Result);
end;

procedure TRegistroX340List.SetItem(Index: Integer; const Value: TRegistroX340);
begin
  Put(Index, Value);
end;

{ TRegistroX350List }

function TRegistroX350List.GetItem(Index: Integer): TRegistroX350;
begin
  Result := TRegistroX350(Inherited Items[Index]);
end;

function TRegistroX350List.New: TRegistroX350;
begin
  Result := TRegistroX350.Create;
  Add(Result);
end;

procedure TRegistroX350List.SetItem(Index: Integer; const Value: TRegistroX350);
begin
  Put(Index, Value);
end;

{ TRegistroX351List }

function TRegistroX351List.GetItem(Index: Integer): TRegistroX351;
begin
  Result := TRegistroX351(Inherited Items[Index]);
end;

function TRegistroX351List.New: TRegistroX351;
begin
  Result := TRegistroX351.Create;
  Add(Result);
end;

procedure TRegistroX351List.SetItem(Index: Integer; const Value: TRegistroX351);
begin
  Put(Index, Value);
end;

{ TRegistroX352List }

function TRegistroX352List.GetItem(Index: Integer): TRegistroX352;
begin
  Result := TRegistroX352(Inherited Items[Index]);
end;

function TRegistroX352List.New: TRegistroX352;
begin
  Result := TRegistroX352.Create;
  Add(Result);
end;

procedure TRegistroX352List.SetItem(Index: Integer; const Value: TRegistroX352);
begin
  Put(Index, Value);
end;

{ TRegistroX353List }

function TRegistroX353List.GetItem(Index: Integer): TRegistroX353;
begin
  Result := TRegistroX353(Inherited Items[Index]);
end;

function TRegistroX353List.New: TRegistroX353;
begin
  Result := TRegistroX353.Create;
  Add(Result);
end;

procedure TRegistroX353List.SetItem(Index: Integer; const Value: TRegistroX353);
begin
  Put(Index, Value);
end;

{ TRegistroX354List }

function TRegistroX354List.GetItem(Index: Integer): TRegistroX354;
begin
  Result := TRegistroX354(Inherited Items[Index]);
end;

function TRegistroX354List.New: TRegistroX354;
begin
  Result := TRegistroX354.Create;
  Add(Result);
end;

procedure TRegistroX354List.SetItem(Index: Integer; const Value: TRegistroX354);
begin
  Put(Index, Value);
end;

{ TRegistroX355List }

function TRegistroX355List.GetItem(Index: Integer): TRegistroX355;
begin
  Result := TRegistroX355(Inherited Items[Index]);
end;

function TRegistroX355List.New: TRegistroX355;
begin
  Result := TRegistroX355.Create;
  Add(Result);
end;

procedure TRegistroX355List.SetItem(Index: Integer; const Value: TRegistroX355);
begin
  Put(Index, Value);
end;

{ TRegistroX356List }

function TRegistroX356List.GetItem(Index: Integer): TRegistroX356;
begin
  Result := TRegistroX356(Inherited Items[Index]);
end;

function TRegistroX356List.New: TRegistroX356;
begin
  Result := TRegistroX356.Create;
  Add(Result);
end;

procedure TRegistroX356List.SetItem(Index: Integer; const Value: TRegistroX356);
begin
  Put(Index, Value);
end;

{ TRegistroX390List }

function TRegistroX390List.GetItem(Index: Integer): TRegistroX390;
begin
  Result := TRegistroX390(Inherited Items[Index]);
end;

function TRegistroX390List.New: TRegistroX390;
begin
  Result := TRegistroX390.Create;
  Add(Result);
end;

procedure TRegistroX390List.SetItem(Index: Integer; const Value: TRegistroX390);
begin
  Put(Index, Value);
end;

{ TRegistroX400List }

function TRegistroX400List.GetItem(Index: Integer): TRegistroX400;
begin
  Result := TRegistroX400(Inherited Items[Index]);
end;

function TRegistroX400List.New: TRegistroX400;
begin
  Result := TRegistroX400.Create;
  Add(Result);
end;

procedure TRegistroX400List.SetItem(Index: Integer; const Value: TRegistroX400);
begin
  Put(Index, Value);
end;

{ TRegistroX410List }

function TRegistroX410List.GetItem(Index: Integer): TRegistroX410;
begin
  Result := TRegistroX410(Inherited Items[Index]);
end;

function TRegistroX410List.New: TRegistroX410;
begin
  Result := TRegistroX410.Create;
  Add(Result);
end;

procedure TRegistroX410List.SetItem(Index: Integer; const Value: TRegistroX410);
begin
  Put(Index, Value);
end;

{ TRegistroX420List }

function TRegistroX420List.GetItem(Index: Integer): TRegistroX420;
begin
  Result := TRegistroX420(Inherited Items[Index]);
end;

function TRegistroX420List.New: TRegistroX420;
begin
  Result := TRegistroX420.Create;
  Add(Result);
end;

procedure TRegistroX420List.SetItem(Index: Integer; const Value: TRegistroX420);
begin
  Put(Index, Value);
end;

{ TRegistroX430List }

function TRegistroX430List.GetItem(Index: Integer): TRegistroX430;
begin
  Result := TRegistroX430(Inherited Items[Index]);
end;

function TRegistroX430List.New: TRegistroX430;
begin
  Result := TRegistroX430.Create;
  Add(Result);
end;

procedure TRegistroX430List.SetItem(Index: Integer; const Value: TRegistroX430);
begin
  Put(Index, Value);
end;

{ TRegistroX450List }

function TRegistroX450List.GetItem(Index: Integer): TRegistroX450;
begin
  Result := TRegistroX450(Inherited Items[Index]);
end;

function TRegistroX450List.New: TRegistroX450;
begin
  Result := TRegistroX450.Create;
  Add(Result);
end;

procedure TRegistroX450List.SetItem(Index: Integer; const Value: TRegistroX450);
begin
  Put(Index, Value);
end;

{ TRegistroX460List }

function TRegistroX460List.GetItem(Index: Integer): TRegistroX460;
begin
  Result := TRegistroX460(Inherited Items[Index]);
end;

function TRegistroX460List.New: TRegistroX460;
begin
  Result := TRegistroX460.Create;
  Add(Result);
end;

procedure TRegistroX460List.SetItem(Index: Integer; const Value: TRegistroX460);
begin
  Put(Index, Value);
end;

{ TRegistroX470List }

function TRegistroX470List.GetItem(Index: Integer): TRegistroX470;
begin
  Result := TRegistroX470(Inherited Items[Index]);
end;

function TRegistroX470List.New: TRegistroX470;
begin
  Result := TRegistroX470.Create;
  Add(Result);
end;

procedure TRegistroX470List.SetItem(Index: Integer; const Value: TRegistroX470);
begin
  Put(Index, Value);
end;

{ TRegistroX480List }

function TRegistroX480List.GetItem(Index: Integer): TRegistroX480;
begin
  Result := TRegistroX480(Inherited Items[Index]);
end;

function TRegistroX480List.New: TRegistroX480;
begin
  Result := TRegistroX480.Create;
  Add(Result);
end;

procedure TRegistroX480List.SetItem(Index: Integer; const Value: TRegistroX480);
begin
  Put(Index, Value);
end;

{ TRegistroX490List }

function TRegistroX490List.GetItem(Index: Integer): TRegistroX490;
begin
  Result := TRegistroX490(Inherited Items[Index]);
end;

function TRegistroX490List.New: TRegistroX490;
begin
  Result := TRegistroX490.Create;
  Add(Result);
end;

procedure TRegistroX490List.SetItem(Index: Integer; const Value: TRegistroX490);
begin
  Put(Index, Value);
end;

{ TRegistroX500List }

function TRegistroX500List.GetItem(Index: Integer): TRegistroX500;
begin
  Result := TRegistroX500(Inherited Items[Index]);
end;

function TRegistroX500List.New: TRegistroX500;
begin
  Result := TRegistroX500.Create;
  Add(Result);
end;

procedure TRegistroX500List.SetItem(Index: Integer; const Value: TRegistroX500);
begin
  Put(Index, Value);
end;

{ TRegistroX510List }

function TRegistroX510List.GetItem(Index: Integer): TRegistroX510;
begin
  Result := TRegistroX510(Inherited Items[Index]);
end;

function TRegistroX510List.New: TRegistroX510;
begin
  Result := TRegistroX510.Create;
  Add(Result);
end;

procedure TRegistroX510List.SetItem(Index: Integer; const Value: TRegistroX510);
begin
  Put(Index, Value);
end;

{ TRegistroX300 }

constructor TRegistroX300.Create;
begin
  FRegistroX310 := TRegistroX310List.Create;
end;

destructor TRegistroX300.Destroy;
begin
  FRegistroX310.Free;
  inherited;
end;

{ TRegistroX320 }

constructor TRegistroX320.Create;
begin
  FRegistroX330 := TRegistroX330List.Create;
end;

destructor TRegistroX320.Destroy;
begin
  FRegistroX330.Free;
  inherited;
end;

{ TRegistroX340 }

constructor TRegistroX340.Create;
begin
  FRegistroX350 := TRegistroX350List.Create;
  FRegistroX351 := TRegistroX351List.Create;
  FRegistroX352 := TRegistroX352List.Create;
  FRegistroX353 := TRegistroX353List.Create;
  FRegistroX354 := TRegistroX354List.Create;
  FRegistroX355 := TRegistroX355List.Create;
  FRegistroX356 := TRegistroX356List.Create;
end;

destructor TRegistroX340.Destroy;
begin
  FRegistroX350.Free;
  FRegistroX351.Free;
  FRegistroX352.Free;
  FRegistroX353.Free;
  FRegistroX354.Free;
  FRegistroX355.Free;
  FRegistroX356.Free;
  inherited;
end;

{ TRegistroX001 }

constructor TRegistroX001.Create;
begin
  inherited Create;
  FRegistroX280 := TRegistroX280List.Create;
  FRegistroX291 := TRegistroX291List.Create;
  FRegistroX292 := TRegistroX292List.Create;
  FRegistroX300 := TRegistroX300List.Create;
  FRegistroX320 := TRegistroX320List.Create;
  FRegistroX340 := TRegistroX340List.Create;
  FRegistroX390 := TRegistroX390List.Create;
  FRegistroX400 := TRegistroX400List.Create;
  FRegistroX410 := TRegistroX410List.Create;
  FRegistroX420 := TRegistroX420List.Create;
  FRegistroX430 := TRegistroX430List.Create;
  FRegistroX450 := TRegistroX450List.Create;
  FRegistroX460 := TRegistroX460List.Create;
  FRegistroX470 := TRegistroX470List.Create;
  FRegistroX480 := TRegistroX480List.Create;
  FRegistroX490 := TRegistroX490List.Create;
  FRegistroX500 := TRegistroX500List.Create;
  FRegistroX510 := TRegistroX510List.Create;
  IND_DAD := idComDados;
end;

destructor TRegistroX001.Destroy;
begin
  FRegistroX280.Free;
  FRegistroX291.Free;
  FRegistroX292.Free;
  FRegistroX300.Free;
  FRegistroX320.Free;
  FRegistroX340.Free;
  FRegistroX390.Free;
  FRegistroX400.Free;
  FRegistroX410.Free;
  FRegistroX420.Free;
  FRegistroX430.Free;
  FRegistroX450.Free;
  FRegistroX460.Free;
  FRegistroX470.Free;
  FRegistroX480.Free;
  FRegistroX490.Free;
  FRegistroX500.Free;
  FRegistroX510.Free;
  inherited;
end;

end.
