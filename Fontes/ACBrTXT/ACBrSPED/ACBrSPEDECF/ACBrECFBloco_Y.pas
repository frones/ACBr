{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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
|* 27/08/2015 - Ariel Guareschi - Alterado a geração do arquivo bloco Y
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_Y;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroY520List = class;
  TRegistroY540List = class;
  TRegistroY550List = class;
  TRegistroY560List = class;
  TRegistroY570List = class;
  TRegistroY580List = class;
  TRegistroY590List = class;
  TRegistroY600List = class;
  TRegistroY611List = class;
  TRegistroY612List = class;
  TRegistroY620List = class;
  TRegistroY630List = class;
  TRegistroY640List = class;
  TRegistroY650List = class;
  TRegistroY660List = class;
  TRegistroY665List = class;
  TRegistroY671List = class;
  TRegistroY672List = class;
  TRegistroY680List = class;
  TRegistroY681List = class;
  TRegistroY682List = class;
  TRegistroY690List = class;
  TRegistroY800 = class;


  /// Registro Y001 - Abertura do Bloco Y – Informações Gerais
  TRegistroY001 = class(TOpenBlocos)
  private
    FRegistroY520 : TRegistroY520List;
    FRegistroY540 : TRegistroY540List;
    FRegistroY550 : TRegistroY550List;
    FRegistroY560 : TRegistroY560List;
    FRegistroY570 : TRegistroY570List;
    FRegistroY580 : TRegistroY580List;
    FRegistroY590 : TRegistroY590List;
    FRegistroY600 : TRegistroY600List;
    FRegistroY611 : TRegistroY611List;
    FRegistroY612 : TRegistroY612List;
    FRegistroY620 : TRegistroY620List;
    FRegistroY630 : TRegistroY630List;
    FRegistroY640 : TRegistroY640List;
    FRegistroY660 : TRegistroY660List;
    FRegistroY665 : TRegistroY665List;
    FRegistroY671 : TRegistroY671List;
    FRegistroY672 : TRegistroY672List;
    FRegistroY680 : TRegistroY680List;
    FRegistroY682 : TRegistroY682List;
    FRegistroY690 : TRegistroY690List;
    FRegistroY800 : TRegistroY800;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy
      
    property RegistroY520 : TRegistroY520List read FRegistroY520 write FRegistroY520;
    property RegistroY540 : TRegistroY540List read FRegistroY540 write FRegistroY540;
    property RegistroY550 : TRegistroY550List read FRegistroY550 write FRegistroY550;
    property RegistroY560 : TRegistroY560List read FRegistroY560 write FRegistroY560;
    property RegistroY570 : TRegistroY570List read FRegistroY570 write FRegistroY570;
    property RegistroY580 : TRegistroY580List read FRegistroY580 write FRegistroY580;
    property RegistroY590 : TRegistroY590List read FRegistroY590 write FRegistroY590;
    property RegistroY600 : TRegistroY600List read FRegistroY600 write FRegistroY600;
    property RegistroY611 : TRegistroY611List read FRegistroY611 write FRegistroY611;
    property RegistroY612 : TRegistroY612List read FRegistroY612 write FRegistroY612;
    property RegistroY620 : TRegistroY620List read FRegistroY620 write FRegistroY620;
    property RegistroY630 : TRegistroY630List read FRegistroY630 write FRegistroY630;
    property RegistroY640 : TRegistroY640List read FRegistroY640 write FRegistroY640;
    property RegistroY660 : TRegistroY660List read FRegistroY660 write FRegistroY660;
    property RegistroY665 : TRegistroY665List read FRegistroY665 write FRegistroY665;
    property RegistroY671 : TRegistroY671List read FRegistroY671 write FRegistroY671;
    property RegistroY672 : TRegistroY672List read FRegistroY672 write FRegistroY672;
    property RegistroY680 : TRegistroY680List read FRegistroY680 write FRegistroY680;
    property RegistroY682 : TRegistroY682List read FRegistroY682 write FRegistroY682;
    property RegistroY690 : TRegistroY690List read FRegistroY690 write FRegistroY690;
    property RegistroY800 : TRegistroY800     read FRegistroY800 write FRegistroY800;  
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

  /// Registro Y250 - Lista

  TRegistroY520List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY520;
    procedure SetItem(Index: Integer; const Value: TRegistroY520);
  public
    function New: TRegistroY520;
    property Items[Index: Integer]: TRegistroY520 read GetItem write SetItem;
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

  /// Registro Y540 - Lista
  TRegistroY540List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY540;
    procedure SetItem(Index: Integer; const Value: TRegistroY540);
  public
    function New: TRegistroY540;
    property Items[Index: Integer]: TRegistroY540 read GetItem write SetItem;
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

  /// Registro Y550 - Lista

  TRegistroY550List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY550;
    procedure SetItem(Index: Integer; const Value: TRegistroY550);
  public
    function New: TRegistroY550;
    property Items[Index: Integer]: TRegistroY550 read GetItem write SetItem;
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


  /// Registro Y560 - Lista

  TRegistroY560List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY560;
    procedure SetItem(Index: Integer; const Value: TRegistroY560);
  public
    function New: TRegistroY560;
    property Items[Index: Integer]: TRegistroY560 read GetItem write SetItem;
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

  /// Registro Y570 - Lista

  TRegistroY570List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY570;
    procedure SetItem(Index: Integer; const Value: TRegistroY570);
  public
    function New: TRegistroY570;
    property Items[Index: Integer]: TRegistroY570 read GetItem write SetItem;
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


  /// Registro Y580 - Lista

  TRegistroY580List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY580;
    procedure SetItem(Index: Integer; const Value: TRegistroY580);
  public
    function New: TRegistroY580;
    property Items[Index: Integer]: TRegistroY580 read GetItem write SetItem;
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

  /// Registro Y550 - Lista

  TRegistroY590List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY590;
    procedure SetItem(Index: Integer; const Value: TRegistroY590);
  public
    function New: TRegistroY590;
    property Items[Index: Integer]: TRegistroY590 read GetItem write SetItem;
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
    fQUALIF_REP_LEG: TACBrQualificacaoRepLegal;
    fVL_IR_RET: variant;
    fVL_JUR_CAP: variant;
    fVL_REM_TRAB: variant;
    fVL_LUC_DIV: variant;
    fVL_DEM_REND: variant;
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
    property QUALIF_REP_LEG: TACBrQualificacaoRepLegal read fQUALIF_REP_LEG write fQUALIF_REP_LEG;
    property VL_REM_TRAB: variant read fVL_REM_TRAB write fVL_REM_TRAB;
    property VL_LUC_DIV: variant read fVL_LUC_DIV write fVL_LUC_DIV;
    property VL_JUR_CAP: variant read fVL_JUR_CAP write fVL_JUR_CAP;
    property VL_DEM_REND: variant read fVL_DEM_REND write fVL_DEM_REND;
    property VL_IR_RET: variant read fVL_IR_RET write fVL_IR_RET;
  end;

  /// Registro Y600 - Lista

  TRegistroY600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY600;
    procedure SetItem(Index: Integer; const Value: TRegistroY600);
  public
    function New: TRegistroY600;
    property Items[Index: Integer]: TRegistroY600 read GetItem write SetItem;
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


  /// Registro Y611 - Lista

  TRegistroY611List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY611;
    procedure SetItem(Index: Integer; const Value: TRegistroY611);
  public
    function New: TRegistroY611;
    property Items[Index: Integer]: TRegistroY611 read GetItem write SetItem;
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


  /// Registro Y612 - Lista

  TRegistroY612List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY612;
    procedure SetItem(Index: Integer; const Value: TRegistroY612);
  public
    function New: TRegistroY612;
    property Items[Index: Integer]: TRegistroY612 read GetItem write SetItem;
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


  /// Registro Y620 - Lista

  TRegistroY620List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY620;
    procedure SetItem(Index: Integer; const Value: TRegistroY620);
  public
    function New: TRegistroY620;
    property Items[Index: Integer]: TRegistroY620 read GetItem write SetItem;
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

  /// Registro Y630 - Lista

  TRegistroY630List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY630;
    procedure SetItem(Index: Integer; const Value: TRegistroY630);
  public
    function New: TRegistroY630;
    property Items[Index: Integer]: TRegistroY630 read GetItem write SetItem;
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

    FRegistroY650: TRegistroY650List;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy

    property CNPJ: string read fCNPJ write fCNPJ;
    property COND_DECL: integer read fCOND_DECL write fCOND_DECL;
    property VL_CONS: variant read fVL_CONS write fVL_CONS;
    property CNPJ_LID: string read fCNPJ_LID write fCNPJ_LID;
    property VL_DECL: variant read fVL_DECL write fVL_DECL;

    // registros filhos
    property RegistroY650: TRegistroY650List read FRegistroY650 write FRegistroY650;
  end;


  /// Registro Y640 - Lista

  TRegistroY640List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY640;
    procedure SetItem(Index: Integer; const Value: TRegistroY640);
  public
    function New: TRegistroY640;
    property Items[Index: Integer]: TRegistroY640 read GetItem write SetItem;
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


  /// Registro Y650 - Lista

  TRegistroY650List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY650;
    procedure SetItem(Index: Integer; const Value: TRegistroY650);
  public
    function New: TRegistroY650;
    property Items[Index: Integer]: TRegistroY650 read GetItem write SetItem;
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


  /// Registro Y660 - Lista

  TRegistroY660List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY660;
    procedure SetItem(Index: Integer; const Value: TRegistroY660);
  public
    function New: TRegistroY660;
    property Items[Index: Integer]: TRegistroY660 read GetItem write SetItem;
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

  /// Registro Y665 - Lista

  TRegistroY665List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY665;
    procedure SetItem(Index: Integer; const Value: TRegistroY665);
  public
    function New: TRegistroY665;
    property Items[Index: Integer]: TRegistroY665 read GetItem write SetItem;
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

  /// Registro Y671 - Lista

  TRegistroY671List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY671;
    procedure SetItem(Index: Integer; const Value: TRegistroY671);
  public
    function New: TRegistroY671;
    property Items[Index: Integer]: TRegistroY671 read GetItem write SetItem;
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

  /// Registro Y672 - Lista

  TRegistroY672List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY672;
    procedure SetItem(Index: Integer; const Value: TRegistroY672);
  public
    function New: TRegistroY672;
    property Items[Index: Integer]: TRegistroY672 read GetItem write SetItem;
  end;

  /// Registro Y680 - Mês das Informações de Optantes pelo Refis (Lucro
  /// Real, Presumido e Arbitrado)


  { TRegistroY680 }

  TRegistroY680 = class(TBlocos)
  private
    fMES: string;
    FRegistroY681: TRegistroY681List;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy

    property MES: string read fMES write fMES;

    // registros filhos
    property RegistroY681: TRegistroY681List read FRegistroY681 write FRegistroY681;
  end;

  /// Registro Y680 - Lista

  TRegistroY680List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY680;
    procedure SetItem(Index: Integer; const Value: TRegistroY680);
  public
    function New: TRegistroY680;
    property Items[Index: Integer]: TRegistroY680 read GetItem write SetItem;
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

  /// Registro Y681 - Lista

  TRegistroY681List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY681;
    procedure SetItem(Index: Integer; const Value: TRegistroY681);
  public
    function New: TRegistroY681;
    property Items[Index: Integer]: TRegistroY681 read GetItem write SetItem;
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


  /// Registro Y682 - Lista

  TRegistroY682List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY682;
    procedure SetItem(Index: Integer; const Value: TRegistroY682);
  public
    function New: TRegistroY682;
    property Items[Index: Integer]: TRegistroY682 read GetItem write SetItem;
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

  /// Registro Y690 - Lista

  TRegistroY690List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroY690;
    procedure SetItem(Index: Integer; const Value: TRegistroY690);
  public
    function New: TRegistroY690;
    property Items[Index: Integer]: TRegistroY690 read GetItem write SetItem;
  end;

  /// Registro Y800 - Outras Informações

  TRegistroY800 = class
  private
    fTIPO_DOC: String; // Tipo de documento
    fDESC_RTF: String; // Descrição do arquivo
    fHASH_RTF: String; // Hash do arquivo
    fARQ_RTF: String;  // Seqüência de bytes que representem um único arquivo no formato RTF (Rich Text Format).
    function GetIND_FIM_RTF: string;

  public
    property TIPO_DOC: String read fTIPO_DOC write fTIPO_DOC;
    property DESC_RTF: String read fDESC_RTF write fDESC_RTF;
    property HASH_RTF: String read fHASH_RTF write fHASH_RTF;
    property ARQ_RTF: String read fARQ_RTF write fARQ_RTF;
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
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
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

{ TRegistroY520List }

function TRegistroY520List.GetItem(Index: Integer): TRegistroY520;
begin
   Result := TRegistroY520(Inherited Items[Index]);
end;

function TRegistroY520List.New: TRegistroY520;
begin
  Result := TRegistroY520.Create;
  Add(Result);
end;

procedure TRegistroY520List.SetItem(Index: Integer; const Value: TRegistroY520);
begin
  Put(Index, Value);
end;

{ TRegistroY540List }

function TRegistroY540List.GetItem(Index: Integer): TRegistroY540;
begin
   Result := TRegistroY540(Inherited Items[Index]);
end;

function TRegistroY540List.New: TRegistroY540;
begin
  Result := TRegistroY540.Create;
  Add(Result);
end;

procedure TRegistroY540List.SetItem(Index: Integer; const Value: TRegistroY540);
begin
  Put(Index, Value);
end;

{ TRegistroY550List }

function TRegistroY550List.GetItem(Index: Integer): TRegistroY550;
begin
   Result := TRegistroY550(Inherited Items[Index]);
end;

function TRegistroY550List.New: TRegistroY550;
begin
  Result := TRegistroY550.Create;
  Add(Result);
end;

procedure TRegistroY550List.SetItem(Index: Integer; const Value: TRegistroY550);
begin
  Put(Index, Value);
end;


{ TRegistroY560List }

function TRegistroY560List.GetItem(Index: Integer): TRegistroY560;
begin
   Result := TRegistroY560(Inherited Items[Index]);
end;

function TRegistroY560List.New: TRegistroY560;
begin
  Result := TRegistroY560.Create;
  Add(Result);
end;

procedure TRegistroY560List.SetItem(Index: Integer; const Value: TRegistroY560);
begin
  Put(Index, Value);
end;

{ TRegistroY570List }

function TRegistroY570List.GetItem(Index: Integer): TRegistroY570;
begin
   Result := TRegistroY570(Inherited Items[Index]);
end;

function TRegistroY570List.New: TRegistroY570;
begin
  Result := TRegistroY570.Create;
  Add(Result);
end;

procedure TRegistroY570List.SetItem(Index: Integer; const Value: TRegistroY570);
begin
  Put(Index, Value);
end;


{ TRegistroY580List }

function TRegistroY580List.GetItem(Index: Integer): TRegistroY580;
begin
   Result := TRegistroY580(Inherited Items[Index]);
end;

function TRegistroY580List.New: TRegistroY580;
begin
  Result := TRegistroY580.Create;
  Add(Result);
end;

procedure TRegistroY580List.SetItem(Index: Integer; const Value: TRegistroY580);
begin
  Put(Index, Value);
end;


{ TRegistroY590List }

function TRegistroY590List.GetItem(Index: Integer): TRegistroY590;
begin
   Result := TRegistroY590(Inherited Items[Index]);
end;

function TRegistroY590List.New: TRegistroY590;
begin
  Result := TRegistroY590.Create;
  Add(Result);
end;

procedure TRegistroY590List.SetItem(Index: Integer; const Value: TRegistroY590);
begin
  Put(Index, Value);
end;


{ TRegistroY600List }

function TRegistroY600List.GetItem(Index: Integer): TRegistroY600;
begin
   Result := TRegistroY600(Inherited Items[Index]);
end;

function TRegistroY600List.New: TRegistroY600;
begin
  Result := TRegistroY600.Create;
  Add(Result);
end;

procedure TRegistroY600List.SetItem(Index: Integer; const Value: TRegistroY600);
begin
  Put(Index, Value);
end;


{ TRegistroY611List }

function TRegistroY611List.GetItem(Index: Integer): TRegistroY611;
begin
   Result := TRegistroY611(Inherited Items[Index]);
end;

function TRegistroY611List.New: TRegistroY611;
begin
  Result := TRegistroY611.Create;
  Add(Result);
end;

procedure TRegistroY611List.SetItem(Index: Integer; const Value: TRegistroY611);
begin
  Put(Index, Value);
end;

{ TRegistroY612List }

function TRegistroY612List.GetItem(Index: Integer): TRegistroY612;
begin
   Result := TRegistroY612(Inherited Items[Index]);
end;

function TRegistroY612List.New: TRegistroY612;
begin
  Result := TRegistroY612.Create;
  Add(Result);
end;

procedure TRegistroY612List.SetItem(Index: Integer; const Value: TRegistroY612);
begin
  Put(Index, Value);
end;

{ TRegistroY620List }

function TRegistroY620List.GetItem(Index: Integer): TRegistroY620;
begin
   Result := TRegistroY620(Inherited Items[Index]);
end;

function TRegistroY620List.New: TRegistroY620;
begin
  Result := TRegistroY620.Create;
  Add(Result);
end;

procedure TRegistroY620List.SetItem(Index: Integer; const Value: TRegistroY620);
begin
  Put(Index, Value);
end;


{ TRegistroY630List }

function TRegistroY630List.GetItem(Index: Integer): TRegistroY630;
begin
   Result := TRegistroY630(Inherited Items[Index]);
end;

function TRegistroY630List.New: TRegistroY630;
begin
  Result := TRegistroY630.Create;
  Add(Result);
end;

procedure TRegistroY630List.SetItem(Index: Integer; const Value: TRegistroY630);
begin
  Put(Index, Value);
end;


{ TRegistroY640List }

function TRegistroY640List.GetItem(Index: Integer): TRegistroY640;
begin
   Result := TRegistroY640(Inherited Items[Index]);
end;

function TRegistroY640List.New: TRegistroY640;
begin
  Result := TRegistroY640.Create;
  Add(Result);
end;

procedure TRegistroY640List.SetItem(Index: Integer; const Value: TRegistroY640);
begin
  Put(Index, Value);
end;


{ TRegistroY650List }

function TRegistroY650List.GetItem(Index: Integer): TRegistroY650;
begin
   Result := TRegistroY650(Inherited Items[Index]);
end;

function TRegistroY650List.New: TRegistroY650;
begin
  Result := TRegistroY650.Create;
  Add(Result);
end;

procedure TRegistroY650List.SetItem(Index: Integer; const Value: TRegistroY650);
begin
  Put(Index, Value);
end;


{ TRegistroY660List }

function TRegistroY660List.GetItem(Index: Integer): TRegistroY660;
begin
   Result := TRegistroY660(Inherited Items[Index]);
end;

function TRegistroY660List.New: TRegistroY660;
begin
  Result := TRegistroY660.Create;
  Add(Result);
end;

procedure TRegistroY660List.SetItem(Index: Integer; const Value: TRegistroY660);
begin
  Put(Index, Value);
end;


{ TRegistroY665List }

function TRegistroY665List.GetItem(Index: Integer): TRegistroY665;
begin
   Result := TRegistroY665(Inherited Items[Index]);
end;

function TRegistroY665List.New: TRegistroY665;
begin
  Result := TRegistroY665.Create;
  Add(Result);
end;

procedure TRegistroY665List.SetItem(Index: Integer; const Value: TRegistroY665);
begin
  Put(Index, Value);
end;


{ TRegistroY671List }

function TRegistroY671List.GetItem(Index: Integer): TRegistroY671;
begin
   Result := TRegistroY671(Inherited Items[Index]);
end;

function TRegistroY671List.New: TRegistroY671;
begin
  Result := TRegistroY671.Create;
  Add(Result);
end;

procedure TRegistroY671List.SetItem(Index: Integer; const Value: TRegistroY671);
begin
  Put(Index, Value);
end;


{ TRegistroY672List }

function TRegistroY672List.GetItem(Index: Integer): TRegistroY672;
begin
   Result := TRegistroY672(Inherited Items[Index]);
end;

function TRegistroY672List.New: TRegistroY672;
begin
  Result := TRegistroY672.Create;
  Add(Result);
end;

procedure TRegistroY672List.SetItem(Index: Integer; const Value: TRegistroY672);
begin
  Put(Index, Value);
end;


{ TRegistroY680List }

function TRegistroY680List.GetItem(Index: Integer): TRegistroY680;
begin
   Result := TRegistroY680(Inherited Items[Index]);
end;

function TRegistroY680List.New: TRegistroY680;
begin
  Result := TRegistroY680.Create;
  Add(Result);
end;

procedure TRegistroY680List.SetItem(Index: Integer; const Value: TRegistroY680);
begin
  Put(Index, Value);
end;


{ TRegistroY681List }

function TRegistroY681List.GetItem(Index: Integer): TRegistroY681;
begin
   Result := TRegistroY681(Inherited Items[Index]);
end;

function TRegistroY681List.New: TRegistroY681;
begin
  Result := TRegistroY681.Create;
  Add(Result);
end;

procedure TRegistroY681List.SetItem(Index: Integer; const Value: TRegistroY681);
begin
  Put(Index, Value);
end;

{ TRegistroY682List }

function TRegistroY682List.GetItem(Index: Integer): TRegistroY682;
begin
   Result := TRegistroY682(Inherited Items[Index]);
end;

function TRegistroY682List.New: TRegistroY682;
begin
  Result := TRegistroY682.Create;
  Add(Result);
end;

procedure TRegistroY682List.SetItem(Index: Integer; const Value: TRegistroY682);
begin
  Put(Index, Value);
end;

{ TRegistroY690List }

function TRegistroY690List.GetItem(Index: Integer): TRegistroY690;
begin
   Result := TRegistroY690(Inherited Items[Index]);
end;

function TRegistroY690List.New: TRegistroY690;
begin
  Result := TRegistroY690.Create;
  Add(Result);
end;

procedure TRegistroY690List.SetItem(Index: Integer; const Value: TRegistroY690);
begin
  Put(Index, Value);
end;

{ TRegistroY640 }

constructor TRegistroY640.Create;
begin
   inherited;

   FRegistroY650 := TRegistroY650List.Create;
end;

destructor TRegistroY640.Destroy;
begin
   FRegistroY650.Free;

   inherited;
end;

{ TRegistroY680 }

constructor TRegistroY680.Create;
begin
   inherited;
   FRegistroY681 := TRegistroY681List.Create;
end;

destructor TRegistroY680.Destroy;
begin
   FRegistroY681.Free;

   inherited;
end;

{ TRegistroY001 }

constructor TRegistroY001.Create;
begin
  inherited Create;
  FRegistroY520 := TRegistroY520List.Create;
  FRegistroY540 := TRegistroY540List.Create;
  FRegistroY550 := TRegistroY550List.Create;
  FRegistroY560 := TRegistroY560List.Create;
  FRegistroY570 := TRegistroY570List.Create;
  FRegistroY580 := TRegistroY580List.Create;
  FRegistroY590 := TRegistroY590List.Create;
  FRegistroY600 := TRegistroY600List.Create;
  FRegistroY611 := TRegistroY611List.Create;
  FRegistroY612 := TRegistroY612List.Create;
  FRegistroY620 := TRegistroY620List.Create;
  FRegistroY630 := TRegistroY630List.Create;
  FRegistroY640 := TRegistroY640List.Create;
  FRegistroY660 := TRegistroY660List.Create;
  FRegistroY665 := TRegistroY665List.Create;
  FRegistroY671 := TRegistroY671List.Create;
  FRegistroY672 := TRegistroY672List.Create;
  FRegistroY680 := TRegistroY680List.Create;
  FRegistroY682 := TRegistroY682List.Create;
  FRegistroY690 := TRegistroY690List.Create;
  FRegistroY800 := TRegistroY800.Create;
  IND_DAD := idComDados;
end;

destructor TRegistroY001.Destroy;
begin
  FRegistroY520.Free;
  FRegistroY540.Free;
  FRegistroY550.Free;
  FRegistroY560.Free;
  FRegistroY570.Free;
  FRegistroY580.Free;
  FRegistroY590.Free;
  FRegistroY600.Free;
  FRegistroY611.Free;
  FRegistroY612.Free;
  FRegistroY620.Free;
  FRegistroY630.Free;
  FRegistroY640.Free;
  FRegistroY660.Free;
  FRegistroY665.Free;
  FRegistroY671.Free;
  FRegistroY672.Free;
  FRegistroY680.Free;
  FRegistroY682.Free;
  FRegistroY690.Free;
  FRegistroY800.Free;
  inherited;
end;

end.
