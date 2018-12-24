{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 14/02/2014: Juliomar Marchetti
|*  - Criação Bloco K - alterado
*******************************************************************************}

unit ACBrEFDBloco_K;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistroK100List = class;
  TRegistroK200List = class;
  TRegistroK210List = class;  
  TRegistroK215List = class;  
  TRegistroK220List = class;
  TRegistroK230List = class;
  TRegistroK235List = class;
  TRegistroK250List = class;
  TRegistroK255List = class;
  TRegistroK260List = class;  
  TRegistroK265List = class;  
  TRegistroK270List = class;  
  TRegistroK275List = class;  
  TRegistroK280List = class;
  TRegistroK290List = class;
  TRegistroK291List = class;
  TRegistroK292List = class;
  TRegistroK300List = class;
  TRegistroK301List = class;
  TRegistroK302List = class;

  /// Registro K001 - ABERTURA DO BLOCO K

  { TRegistroK001 }

  TRegistroK001 = class(TOpenBlocos)
  private
    FRegistroK100: TRegistroK100List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroK100: TRegistroK100List read FRegistroK100 write FRegistroK100;
  end;

  /// Registro K100 - PERIODO DE APURACAO DO ICMS/IPI

  TRegistroK100 = class
  private
    fDT_FIN: TDateTime; //Data inicial a que a apuração se refere
    fDT_INI: TDateTime; //Data final a que a apuração se refere
    FRegistroK200: TRegistroK200List;
    FRegistroK210: TRegistroK210List;
    FRegistroK220: TRegistroK220List;
    FRegistroK230: TRegistroK230List;
    FRegistroK250: TRegistroK250List;
    FRegistroK260: TRegistroK260List;
    FRegistroK270: TRegistroK270List;
    FRegistroK280: TRegistroK280List;
    FRegistroK290: TRegistroK290List;
    FRegistroK300: TRegistroK300List;
  public
    constructor Create(AOwner: TRegistroK001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI : TDateTime read fDT_INI write fDT_INI;
    property DT_FIN : TDateTime read fDT_FIN write fDT_FIN;

    property RegistroK200: TRegistroK200List read FRegistroK200 write FRegistroK200;
    property RegistroK210: TRegistroK210List read FRegistroK210 write FRegistroK210;
    property RegistroK220: TRegistroK220List read FRegistroK220 write FRegistroK220;
    property RegistroK230: TRegistroK230List read FRegistroK230 write FRegistroK230;
    property RegistroK250: TRegistroK250List read FRegistroK250 write FRegistroK250;
    property RegistroK260: TRegistroK260List read FRegistroK260 write FRegistroK260;
    property RegistroK270: TRegistroK270List read FRegistroK270 write FRegistroK270;
    property RegistroK280: TRegistroK280List read FRegistroK280 write FRegistroK280;
    property RegistroK290: TRegistroK290List read FRegistroK290 write FRegistroK290;
    property RegistroK300: TRegistroK300List read FRegistroK300 write FRegistroK300;
  end;

  /// Registro K100 - Lista

  TRegistroK100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK100; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK100); /// SetItem
  public
    function New(AOwner: TRegistroK001): TRegistroK100;
    property Items[Index: Integer]: TRegistroK100 read GetItem write SetItem;
  end;

  /// Registro K200 - ESTOQUE ESCRITURADO

  TRegistroK200 = class
  private
    fCOD_ITEM: string;
    fCOD_PART: string;
    fDT_EST: TDateTime;
    fIND_EST: TACBrIndEstoque;
    fQTD: Double;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_EST : TDateTime read fDT_EST write fDT_EST;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD : Double read fQTD write fQTD;
    property IND_EST : TACBrIndEstoque read fIND_EST write fIND_EST;
    property COD_PART : string read fCOD_PART write fCOD_PART;
  end;

  /// Registro K200 - Lista

  TRegistroK200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK200; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK200); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK200;
    property Items[Index: Integer]: TRegistroK200 read GetItem write SetItem;
  end;

  /// Registro K210 - DESMONTAGEM DE MERCADORIAS – ITEM DE ORIGEM

  TRegistroK210 = class
  private
    fDT_INI_OS:    TDateTime;
    fDT_FIN_OS:    TDateTime;
    fCOD_DOC_OS:   string;
    fCOD_ITEM_ORI: string;
    fQTD_ORI:      Double;
    FRegistroK215: TRegistroK215List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI_OS    : TDateTime read fDT_INI_OS    write fDT_INI_OS;
    property DT_FIN_OS    : TDateTime read fDT_FIN_OS    write fDT_FIN_OS;
    property COD_DOC_OS   : string    read fCOD_DOC_OS   write fCOD_DOC_OS;
    property COD_ITEM_ORI : string    read fCOD_ITEM_ORI write fCOD_ITEM_ORI;
    property QTD_ORI      : Double    read fQTD_ORI      write fQTD_ORI;

    property RegistroK215: TRegistroK215List read FRegistroK215 write FRegistroK215;
  end;

  /// Registro K210 - Lista

  TRegistroK210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK210; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK210); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK210;
    property Items[Index: Integer]: TRegistroK210 read GetItem write SetItem;
  end;

  /// Registro K215 - DESMONTAGEM DE MERCADORIAS – ITENS DE DESTINO

  TRegistroK215 = class
  private
    fCOD_ITEM_DES: string;
    fQTD_DES     : Double;
  public
    constructor Create(AOwner: TRegistroK210); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM_DES : string read fCOD_ITEM_DES write fCOD_ITEM_DES;
    property QTD_DES: Double read fQTD_DES write fQTD_DES;
  end;

  /// Registro K215 - Lista

  TRegistroK215List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK215; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK215); /// SetItem
  public
    function New(AOwner: TRegistroK210): TRegistroK215;
    property Items[Index: Integer]: TRegistroK215 read GetItem write SetItem;
  end;

  /// Registro K220 - OUTRAS MOVIMENTAÇÕES INTERNA ENTRE MERCADORIAS

  TRegistroK220 = class
  private
    fCOD_ITEM_DEST: string;
    fCOD_ITEM_ORI: string;
    fDT_MOV: TDateTime;
    fQTD: Double;
	  fQTD_DEST: Double;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_MOV : TDateTime read fDT_MOV write fDT_MOV;
    property COD_ITEM_ORI : string read fCOD_ITEM_ORI write fCOD_ITEM_ORI;
    property COD_ITEM_DEST : string read fCOD_ITEM_DEST write fCOD_ITEM_DEST;
    property QTD : Double read fQTD write fQTD;
  	property QTD_DEST: Double read fQTD_DEST write fQTD_DEST;
  end;

  /// Registro K220 - Lista

  TRegistroK220List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK220; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK220); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK220;
    property Items[Index: Integer]: TRegistroK220 read GetItem write SetItem;
  end;

  /// Registro K230 - ITENS PRODUZIDOS

  TRegistroK230 = class
  private
    fCOD_DOC_OP: string;
    fCOD_ITEM: string;
    fDT_FIN_OP: TDateTime;
    fDT_INI_OP: TDateTime;
    fQTD_ENC: Double;
    FRegistroK235: TRegistroK235List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI_OP : TDateTime read fDT_INI_OP write fDT_INI_OP;
    property DT_FIN_OP : TDateTime read fDT_FIN_OP write fDT_FIN_OP;
    property COD_DOC_OP : string read fCOD_DOC_OP write fCOD_DOC_OP;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD_ENC : Double read fQTD_ENC write fQTD_ENC;

    property RegistroK235: TRegistroK235List read FRegistroK235 write FRegistroK235;
  end;

  /// Registro K230 - Lista

  TRegistroK230List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK230; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK230); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK230;
    property Items[Index: Integer]: TRegistroK230 read GetItem write SetItem;
  end;

  /// Registro K235 - INSUMOS CONSUMIDOS

  TRegistroK235 = class
  private
    fCOD_INS_SUBST: string;
    fCOD_ITEM: string;
    fDT_SAIDA: TDateTime;
    fQTD: Double;
  public
    constructor Create(AOwner: TRegistroK230); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_SAIDA : TDateTime  read fDT_SAIDA write fDT_SAIDA;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD: Double read fQTD write fQTD;
    property COD_INS_SUBST : string read fCOD_INS_SUBST write fCOD_INS_SUBST;
  end;

  /// Registro K235 - Lista

  TRegistroK235List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK235; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK235); /// SetItem
  public
    function New(AOwner: TRegistroK230): TRegistroK235;
    property Items[Index: Integer]: TRegistroK235 read GetItem write SetItem;
  end;

  /// REGISTRO K250 – INDUSTRIALIZAÇÃO EFETUADA POR TERCEIROS – ITENS PRODUZIDOS

  TRegistroK250 = class
  private
    fCOD_ITEM: String;
    fDT_PROD: TDateTime;
    fQTD: Double;
    fRegistroK255: TRegistroK255List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_PROD : TDateTime read fDT_PROD write fDT_PROD;
    property COD_ITEM : String  read fCOD_ITEM write fCOD_ITEM;
    property QTD : Double read fQTD write fQTD;

    property RegistroK255 : TRegistroK255List read fRegistroK255 write fRegistroK255;
  end;

  /// Registro K250 - Lista

  TRegistroK250List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK250; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK250); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK250;
    property Items[Index: Integer]: TRegistroK250 read GetItem write SetItem;
  end;

  /// REGISTRO K255 – INDUSTRIALIZAÇÃO EM TERCEIROS – INSUMOS CONSUMIDOS

  TRegistroK255 = class
  private
    fCOD_INS_SUBST: string;
    fCOD_ITEM: string;
    fDT_CONS: TDateTime;
    fQTD: Double;
  public
    constructor Create(AOwner: TRegistroK250); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_CONS : TDateTime read fDT_CONS write fDT_CONS;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD : Double read fQTD write fQTD;
    property COD_INS_SUBST : string read fCOD_INS_SUBST write fCOD_INS_SUBST;

  end;

  /// Registro K255 - Lista

  TRegistroK255List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK255; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK255); /// SetItem
  public
    function New(AOwner: TRegistroK250): TRegistroK255;
    property Items[Index: Integer]: TRegistroK255 read GetItem write SetItem;
  end;

  /// Registro K260 - REPROCESSAMENTO/REPARO DE PRODUTO/INSUMO

  TRegistroK260 = class
  private
    fCOD_OP_OS: String;
    fCOD_ITEM:  String;
    fDT_SAIDA:  TDateTime;
    fQTD_SAIDA: Double;
    fDT_RET:    TDateTime;
    fQTD_RET:   Double;

    fRegistroK265: TRegistroK265List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_OP_OS : String    read fCOD_OP_OS write fCOD_OP_OS;
    property COD_ITEM  : String    read fCOD_ITEM  write fCOD_ITEM;
    property DT_SAIDA  : TDateTime read fDT_SAIDA  write fDT_SAIDA;
    property QTD_SAIDA : Double    read fQTD_SAIDA write fQTD_SAIDA;
    property DT_RET    : TDateTime read fDT_RET    write fDT_RET;
    property QTD_RET   : Double    read fQTD_RET   write fQTD_RET;

    property RegistroK265 : TRegistroK265List read fRegistroK265 write fRegistroK265;
  end;

  /// Registro K260 - Lista

  TRegistroK260List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK260; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK260); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK260;
    property Items[Index: Integer]: TRegistroK260 read GetItem write SetItem;
  end;

  /// REGISTRO K265 – REPROCESSAMENTO/REPARO - MERCADORIAS CONSUMIDAS E/OU RETORNADAS

  TRegistroK265 = class
  private
    fCOD_ITEM: String;
    fQTD_CONS: Double;
    fQTD_RET:  Double;
  public
    constructor Create(AOwner: TRegistroK260); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM : String read fCOD_ITEM write fCOD_ITEM;
    property QTD_CONS : Double read fQTD_CONS write fQTD_CONS;
    property QTD_RET  : Double read fQTD_RET  write fQTD_RET;
  end;

  /// Registro K265 - Lista

  TRegistroK265List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK265; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK265); /// SetItem
  public
    function New(AOwner: TRegistroK260): TRegistroK265;
    property Items[Index: Integer]: TRegistroK265 read GetItem write SetItem;
  end;

  /// REGISTRO K270 – CORREÇÃO DE APONTAMENTO DOS REGISTROS K210, K220, K230, K250 E K260

  TRegistroK270 = class
  private
    fDT_INI_AP:   TDateTime;
    fDT_FIN_AP:   TDateTime;
    fCOD_OP_OS:   String;
    fCOD_ITEM:    String;
    fQTD_COR_POS: Double;
    fQTD_COR_NEG: Double;
    fORIGEM:      String;

    fRegistroK275: TRegistroK275List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI_AP   : TDateTime read fDT_INI_AP   write fDT_INI_AP;
    property DT_FIN_AP   : TDateTime read fDT_FIN_AP   write fDT_FIN_AP;
    property COD_OP_OS   : String    read fCOD_OP_OS   write fCOD_OP_OS;
    property COD_ITEM    : String    read fCOD_ITEM    write fCOD_ITEM;
    property QTD_COR_POS : Double    read fQTD_COR_POS write fQTD_COR_POS;
    property QTD_COR_NEG : Double    read fQTD_COR_NEG write fQTD_COR_NEG;
    property ORIGEM      : String    read fORIGEM      write fORIGEM;

    property RegistroK275 : TRegistroK275List read fRegistroK275 write fRegistroK275;
  end;

  /// Registro K270 - Lista

  TRegistroK270List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK270; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK270); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK270;
    property Items[Index: Integer]: TRegistroK270 read GetItem write SetItem;
  end;

  /// REGISTRO K275 – CORREÇÃO DE APONTAMENTO E RETORNO DE INSUMOS DOS REGISTROS K215, K220, K235, K255 E K265.

  TRegistroK275 = class
  private
    fCOD_ITEM:      String;
    fQTD_COR_POS:   Double;
    fQTD_COR_NEG:   Double;
    fCOD_INS_SUBST: String;
  public
    constructor Create(AOwner: TRegistroK270); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM      : String read fCOD_ITEM      write fCOD_ITEM;
    property QTD_COR_POS   : Double read fQTD_COR_POS   write fQTD_COR_POS;
    property QTD_COR_NEG   : Double read fQTD_COR_NEG   write fQTD_COR_NEG;
    property COD_INS_SUBST : String read fCOD_INS_SUBST write fCOD_INS_SUBST;
  end;

  /// Registro K275 - Lista

  TRegistroK275List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK275; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK275); /// SetItem
  public
    function New(AOwner: TRegistroK270): TRegistroK275;
    property Items[Index: Integer]: TRegistroK275 read GetItem write SetItem;
  end;

  /// REGISTRO K280 – CORREÇÃO DE APONTAMENTO – ESTOQUE ESCRITURADO

  TRegistroK280 = class
  private
    fDT_EST:      TDateTime;
    fCOD_ITEM:    String;
    fQTD_COR_POS: Double;
    fQTD_COR_NEG: Double;
    fIND_EST: TACBrIndEstoque;
    fCOD_PART:    String;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_EST      : TDateTime       read fDT_EST      write fDT_EST;
    property COD_ITEM    : String          read fCOD_ITEM    write fCOD_ITEM;
    property QTD_COR_POS : Double          read fQTD_COR_POS write fQTD_COR_POS;
    property QTD_COR_NEG : Double          read fQTD_COR_NEG write fQTD_COR_NEG;
    property IND_EST     : TACBrIndEstoque read fIND_EST     write fIND_EST;
    property COD_PART    : String          read fCOD_PART    write fCOD_PART;
  end;

  /// Registro K280 - Lista

  TRegistroK280List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK280; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK280); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK280;
    property Items[Index: Integer]: TRegistroK280 read GetItem write SetItem;
  end;

  /// REGISTRO K290: PRODUÇÃO CONJUNTA – ORDEM DE PRODUÇÃO

  TRegistroK290 = class
  private
    fDT_INI_OP:  TDateTime;
    fDT_FIN_OP:  TDateTime;
    fCOD_DOC_OP: String;
    FRegistroK291: TRegistroK291List;
    FRegistroK292: TRegistroK292List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI_OP  : TDateTime  read fDT_INI_OP  write fDT_INI_OP;
    property DT_FIN_OP  : TDateTime  read fDT_FIN_OP  write fDT_FIN_OP;
    property COD_DOC_OP : String     read fCOD_DOC_OP write fCOD_DOC_OP;

    property RegistroK291: TRegistroK291List read FRegistroK291 write FRegistroK291;
    property RegistroK292: TRegistroK292List read FRegistroK292 write FRegistroK292;
  end;

  /// Registro K290 - Lista

  TRegistroK290List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK290; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK290); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK290;
    property Items[Index: Integer]: TRegistroK290 read GetItem write SetItem;
  end;

  /// REGISTRO K291: PRODUÇÃO CONJUNTA - ITENS PRODUZIDOS

  TRegistroK291 = class
  private
    fCOD_ITEM: String;
    fQTD:      Double;
  public
    constructor Create(AOwner: TRegistroK290); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM : String read fCOD_ITEM  write fCOD_ITEM;
    property QTD      : Double read fQTD       write fQTD;
  end;

  /// Registro K291 - Lista

  TRegistroK291List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK291; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK291); /// SetItem
  public
    function New(AOwner: TRegistroK290): TRegistroK291;
    property Items[Index: Integer]: TRegistroK291 read GetItem write SetItem;
  end;

  /// REGISTRO K292: PRODUÇÃO CONJUNTA – INSUMOS CONSUMIDOS

  TRegistroK292 = class
  private
    fCOD_ITEM: String;
    fQTD:      Double;
  public
    constructor Create(AOwner: TRegistroK290); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM : String read fCOD_ITEM  write fCOD_ITEM;
    property QTD      : Double read fQTD       write fQTD;
  end;

  /// Registro K292 - Lista

  TRegistroK292List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK292; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK292); /// SetItem
  public
    function New(AOwner: TRegistroK290): TRegistroK292;
    property Items[Index: Integer]: TRegistroK292 read GetItem write SetItem;
  end;

  /// REGISTRO K300: PRODUÇÃO CONJUNTA – INDUSTRIALIZAÇÃO EFETUADA POR TERCEIROS

  TRegistroK300 = class
  private
    fDT_PROD:  TDateTime;
    FRegistroK301: TRegistroK301List;
    FRegistroK302: TRegistroK302List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_PROD : TDateTime read fDT_PROD  write fDT_PROD;

    property RegistroK301: TRegistroK301List read FRegistroK301 write FRegistroK301;
    property RegistroK302: TRegistroK302List read FRegistroK302 write FRegistroK302;
  end;

  /// Registro K300 - Lista

  TRegistroK300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK300; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK300); /// SetItem
  public
    function New(AOwner: TRegistroK100): TRegistroK300;
    property Items[Index: Integer]: TRegistroK300 read GetItem write SetItem;
  end;

  /// REGISTRO K301: PRODUÇÃO CONJUNTA – INDUSTRIALIZAÇÃO EFETUADA POR TERCEIROS – ITENS PRODUZIDOS

  TRegistroK301 = class
  private
    fCOD_ITEM: String;
    fQTD:      Double;
  public
    constructor Create(AOwner: TRegistroK300); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM : String read fCOD_ITEM  write fCOD_ITEM;
    property QTD      : Double read fQTD       write fQTD;
  end;

  /// Registro K301 - Lista

  TRegistroK301List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK301; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK301); /// SetItem
  public
    function New(AOwner: TRegistroK300): TRegistroK301;
    property Items[Index: Integer]: TRegistroK301 read GetItem write SetItem;
  end;

  /// REGISTRO K302: PRODUÇÃO CONJUNTA – INDUSTRIALIZAÇÃO EFETUADA POR TERCEIROS – INSUMOS CONSUMIDOS

  TRegistroK302 = class
  private
    fCOD_ITEM: String;
    fQTD:      Double;
  public
    constructor Create(AOwner: TRegistroK300); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM : String read fCOD_ITEM  write fCOD_ITEM;
    property QTD      : Double read fQTD       write fQTD;
  end;

  /// Registro K302 - Lista

  TRegistroK302List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK302; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroK302); /// SetItem
  public
    function New(AOwner: TRegistroK300): TRegistroK302;
    property Items[Index: Integer]: TRegistroK302 read GetItem write SetItem;
  end;

  /// Registro K990 - ENCERRAMENTO DO BLOCO K

  TRegistroK990 = class
  private
    fQTD_LIN_K: Integer;    /// Quantidade total de linhas do Bloco K
  public
    property QTD_LIN_K: Integer read fQTD_LIN_K write fQTD_LIN_K;
  end;

implementation

{ TRegistroK001 }

constructor TRegistroK001.Create;
begin
   inherited Create;
   FRegistroK100 := TRegistroK100List.Create;
   //
   IND_MOV := imSemDados;
end;

destructor TRegistroK001.Destroy;
begin
  FRegistroK100.Free;
  inherited Destroy;
end;


{ TRegistroK302List }

function TRegistroK302List.GetItem(Index: Integer): TRegistroK302;
begin
  Result := TRegistroK302(Inherited Items[Index]);
end;

function TRegistroK302List.New(AOwner: TRegistroK300): TRegistroK302;
begin
  Result := TRegistroK302.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK302List.SetItem(Index: Integer; const Value: TRegistroK302);
begin
  Put(Index, Value);
end;

{ TRegistroK302 }

constructor TRegistroK302.Create(AOwner: TRegistroK300);
begin

end;

destructor TRegistroK302.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK301List }

function TRegistroK301List.GetItem(Index: Integer): TRegistroK301;
begin
  Result := TRegistroK301(Inherited Items[Index]);
end;

function TRegistroK301List.New(AOwner: TRegistroK300): TRegistroK301;
begin
  Result := TRegistroK301.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK301List.SetItem(Index: Integer; const Value: TRegistroK301);
begin
  Put(Index, Value);
end;

{ TRegistroK301 }

constructor TRegistroK301.Create(AOwner: TRegistroK300);
begin

end;

destructor TRegistroK301.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK300List }

function TRegistroK300List.GetItem(Index: Integer): TRegistroK300;
begin
  Result := TRegistroK300(Inherited Items[Index]);
end;

function TRegistroK300List.New(AOwner: TRegistroK100): TRegistroK300;
begin
  Result := TRegistroK300.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK300List.SetItem(Index: Integer; const Value: TRegistroK300);
begin
  Put(Index, Value);
end;

{ TRegistroK300 }

constructor TRegistroK300.Create(AOwner: TRegistroK100);
begin
  FRegistroK301 := TRegistroK301List.Create;
  FRegistroK302 := TRegistroK302List.Create;
end;

destructor TRegistroK300.Destroy;
begin
  FRegistroK301.Free;
  FRegistroK302.Free;
  inherited Destroy;
end;
                    
{ TRegistroK292List }

function TRegistroK292List.GetItem(Index: Integer): TRegistroK292;
begin
  Result := TRegistroK292(Inherited Items[Index]);
end;

function TRegistroK292List.New(AOwner: TRegistroK290): TRegistroK292;
begin
  Result := TRegistroK292.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK292List.SetItem(Index: Integer; const Value: TRegistroK292);
begin
  Put(Index, Value);
end;

{ TRegistroK292 }

constructor TRegistroK292.Create(AOwner: TRegistroK290);
begin

end;

destructor TRegistroK292.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK291List }

function TRegistroK291List.GetItem(Index: Integer): TRegistroK291;
begin
  Result := TRegistroK291(Inherited Items[Index]);
end;

function TRegistroK291List.New(AOwner: TRegistroK290): TRegistroK291;
begin
  Result := TRegistroK291.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK291List.SetItem(Index: Integer; const Value: TRegistroK291);
begin
  Put(Index, Value);
end;

{ TRegistroK291 }

constructor TRegistroK291.Create(AOwner: TRegistroK290);
begin

end;

destructor TRegistroK291.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK290List }

function TRegistroK290List.GetItem(Index: Integer): TRegistroK290;
begin
  Result := TRegistroK290(Inherited Items[Index]);
end;

function TRegistroK290List.New(AOwner: TRegistroK100): TRegistroK290;
begin
  Result := TRegistroK290.Create(AOwner);
  Add(Result);
end;

procedure TRegistroK290List.SetItem(Index: Integer; const Value: TRegistroK290);
begin
  Put(Index, Value);
end;

{ TRegistroK290 }

constructor TRegistroK290.Create(AOwner: TRegistroK100);
begin
  FRegistroK291 := TRegistroK291List.Create;
  FRegistroK292 := TRegistroK292List.Create;
end;

destructor TRegistroK290.Destroy;
begin
  FRegistroK291.Free;
  FRegistroK292.Free;
  inherited Destroy;
end;

{ TRegistroK280List }

function TRegistroK280List.GetItem(Index: Integer): TRegistroK280;
begin
  Result := TRegistroK280(Inherited Items[Index]);
end;

procedure TRegistroK280List.SetItem(Index: Integer; const Value: TRegistroK280);
begin
  Put(Index, Value);
end;

function TRegistroK280List.New(AOwner: TRegistroK100): TRegistroK280;
begin
  Result := TRegistroK280.Create(AOwner);
  Add(Result);
end;

{ TRegistroK280 }

constructor TRegistroK280.Create(AOwner: TRegistroK100);
begin

end;

destructor TRegistroK280.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK275 }

constructor TRegistroK275.Create(AOwner: TRegistroK270);
begin

end;

destructor TRegistroK275.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK275List }

function TRegistroK275List.GetItem(Index: Integer): TRegistroK275;
begin
  Result := TRegistroK275(Inherited Items[Index]);
end;

procedure TRegistroK275List.SetItem(Index: Integer; const Value: TRegistroK275);
begin
  Put(Index, Value);
end;

function TRegistroK275List.New(AOwner: TRegistroK270): TRegistroK275;
begin
  Result := TRegistroK275.Create(AOwner);
  Add(Result);
end;

{ TRegistroK270 }

constructor TRegistroK270.Create(AOwner: TRegistroK100);
begin
  fRegistroK275 := TRegistroK275List.Create;
end;

destructor TRegistroK270.Destroy;
begin
  fRegistroK275.Free;
  inherited Destroy;
end;

{ TRegistroK270List }

function TRegistroK270List.GetItem(Index: Integer): TRegistroK270;
begin
  Result := TRegistroK270(Inherited Items[Index]);
end;

procedure TRegistroK270List.SetItem(Index: Integer; const Value: TRegistroK270);
begin
  Put(Index, Value);
end;

function TRegistroK270List.New(AOwner: TRegistroK100): TRegistroK270;
begin
  Result := TRegistroK270.Create(AOwner);
  Add(Result);
end;

{ TRegistroK265 }

constructor TRegistroK265.Create(AOwner: TRegistroK260);
begin

end;

destructor TRegistroK265.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK265List }

function TRegistroK265List.GetItem(Index: Integer): TRegistroK265;
begin
  Result := TRegistroK265(Inherited Items[Index]);
end;

procedure TRegistroK265List.SetItem(Index: Integer; const Value: TRegistroK265);
begin
  Put(Index, Value);
end;

function TRegistroK265List.New(AOwner: TRegistroK260): TRegistroK265;
begin
  Result := TRegistroK265.Create(AOwner);
  Add(Result);
end;

{ TRegistroK260 }

constructor TRegistroK260.Create(AOwner: TRegistroK100);
begin
  fRegistroK265 := TRegistroK265List.Create;
end;

destructor TRegistroK260.Destroy;
begin
  fRegistroK265.Free;
  inherited Destroy;
end;

{ TRegistroK260List }

function TRegistroK260List.GetItem(Index: Integer): TRegistroK260;
begin
  Result := TRegistroK260(Inherited Items[Index]);
end;

procedure TRegistroK260List.SetItem(Index: Integer; const Value: TRegistroK260);
begin
  Put(Index, Value);
end;

function TRegistroK260List.New(AOwner: TRegistroK100): TRegistroK260;
begin
  Result := TRegistroK260.Create(AOwner);
  Add(Result);
end;

{ TRegistroK255 }

constructor TRegistroK255.Create(AOwner: TRegistroK250);
begin

end;

destructor TRegistroK255.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK255List }

function TRegistroK255List.GetItem(Index: Integer): TRegistroK255;
begin
  Result := TRegistroK255(Inherited Items[Index]);
end;

procedure TRegistroK255List.SetItem(Index: Integer; const Value: TRegistroK255);
begin
  Put(Index, Value);
end;

function TRegistroK255List.New(AOwner: TRegistroK250): TRegistroK255;
begin
  Result := TRegistroK255.Create(AOwner);
  Add(Result);
end;

{ TRegistroK250 }

constructor TRegistroK250.Create(AOwner: TRegistroK100);
begin
  fRegistroK255 := TRegistroK255List.Create;
end;

destructor TRegistroK250.Destroy;
begin
  fRegistroK255.Free;
  inherited Destroy;
end;

{ TRegistroK250List }

function TRegistroK250List.GetItem(Index: Integer): TRegistroK250;
begin
  Result := TRegistroK250(Inherited Items[Index]);
end;

procedure TRegistroK250List.SetItem(Index: Integer; const Value: TRegistroK250);
begin
  Put(Index, Value);
end;

function TRegistroK250List.New(AOwner: TRegistroK100): TRegistroK250;
begin
  Result := TRegistroK250.Create(AOwner);
  Add(Result);
end;

{ TRegistroK235 }

constructor TRegistroK235.Create(AOwner: TRegistroK230);
begin

end;

destructor TRegistroK235.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK235List }

function TRegistroK235List.GetItem(Index: Integer): TRegistroK235;
begin
  Result := TRegistroK235(Inherited Items[Index]);
end;

procedure TRegistroK235List.SetItem(Index: Integer; const Value: TRegistroK235);
begin
  Put(Index, Value);
end;

function TRegistroK235List.New(AOwner: TRegistroK230): TRegistroK235;
begin
  Result := TRegistroK235.Create(AOwner);
  Add(Result);
end;

{ TRegistroK230List }

function TRegistroK230List.GetItem(Index: Integer): TRegistroK230;
begin
  Result := TRegistroK230(Inherited Items[Index]);
end;

procedure TRegistroK230List.SetItem(Index: Integer; const Value: TRegistroK230);
begin
  Put(Index, Value);
end;

function TRegistroK230List.New(AOwner: TRegistroK100): TRegistroK230;
begin
  Result := TRegistroK230.Create(AOwner);
  Add(Result);
end;

{ TRegistroK230 }

constructor TRegistroK230.Create(AOwner: TRegistroK100);
begin
  FRegistroK235 := TRegistroK235List.Create;
end;

destructor TRegistroK230.Destroy;
begin
  FRegistroK235.Free;
  inherited Destroy;
end;

{ TRegistroK220List }

function TRegistroK220List.GetItem(Index: Integer): TRegistroK220;
begin
  Result := TRegistroK220(Inherited Items[Index]);
end;

procedure TRegistroK220List.SetItem(Index: Integer; const Value: TRegistroK220);
begin
  Put(Index, Value);
end;

function TRegistroK220List.New(AOwner: TRegistroK100): TRegistroK220;
begin
  Result := TRegistroK220.Create(AOwner);
  Add(Result);
end;

{ TRegistroK220 }

constructor TRegistroK220.Create(AOwner: TRegistroK100);
begin
end;

destructor TRegistroK220.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK215 }

constructor TRegistroK215.Create(AOwner: TRegistroK210);
begin

end;

destructor TRegistroK215.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK215List }

function TRegistroK215List.GetItem(Index: Integer): TRegistroK215;
begin
  Result := TRegistroK215(Inherited Items[Index]);
end;

procedure TRegistroK215List.SetItem(Index: Integer; const Value: TRegistroK215);
begin
  Put(Index, Value);
end;

function TRegistroK215List.New(AOwner: TRegistroK210): TRegistroK215;
begin
  Result := TRegistroK215.Create(AOwner);
  Add(Result);
end;

{ TRegistroK210List }

function TRegistroK210List.GetItem(Index: Integer): TRegistroK210;
begin
  Result := TRegistroK210(Inherited Items[Index]);
end;

procedure TRegistroK210List.SetItem(Index: Integer; const Value: TRegistroK210);
begin
  Put(Index, Value);
end;

function TRegistroK210List.New(AOwner: TRegistroK100): TRegistroK210;
begin
  Result := TRegistroK210.Create(AOwner);
  Add(Result);
end;

{ TRegistroK210 }

constructor TRegistroK210.Create(AOwner: TRegistroK100);
begin
  FRegistroK215 := TRegistroK215List.Create;
end;

destructor TRegistroK210.Destroy;
begin
  FRegistroK215.Free;
  inherited Destroy;
end;

{ TRegistroK200 }

constructor TRegistroK200.Create(AOwner: TRegistroK100);
begin

end;

destructor TRegistroK200.Destroy;
begin
  inherited Destroy;
end;

{ TRegistroK200List }

function TRegistroK200List.GetItem(Index: Integer): TRegistroK200;
begin
  Result := TRegistroK200(Inherited Items[Index]);
end;

procedure TRegistroK200List.SetItem(Index: Integer; const Value: TRegistroK200);
begin
  Put(Index, Value);
end;

function TRegistroK200List.New(AOwner: TRegistroK100): TRegistroK200;
begin
  Result := TRegistroK200.Create(AOwner);
  Add(Result);
end;

{ TRegistroK100List }

function TRegistroK100List.GetItem(Index: Integer): TRegistroK100;
begin
  Result := TRegistroK100(Inherited Items[Index]);
end;

procedure TRegistroK100List.SetItem(Index: Integer; const Value: TRegistroK100);
begin
  Put(Index, Value);
end;

function TRegistroK100List.New(AOwner: TRegistroK001): TRegistroK100;
begin
  Result := TRegistroK100.Create(AOwner);
  Add(Result);
end;

{ TRegistroK100 }

constructor TRegistroK100.Create(AOwner: TRegistroK001);
begin
  FRegistroK200:= TRegistroK200List.Create;  /// BLOCO K - Lista de RegistroK200 (FILHO fo FILHO)
  FRegistroK210:= TRegistroK210List.Create;  /// BLOCO K - Lista de RegistroK210 (FILHO fo FILHO)
  FRegistroK220:= TRegistroK220List.Create;  /// BLOCO K - Lista de RegistroK220 (FILHO fo FILHO)
  FRegistroK230:= TRegistroK230List.Create;  /// BLOCO K - Lista de RegistroK230 (FILHO fo FILHO)
  FRegistroK250:= TRegistroK250List.Create;  /// BLOCO K - Lista de RegistroK250 (FILHO fo FILHO)
  FRegistroK260:= TRegistroK260List.Create;  /// BLOCO K - Lista de RegistroK260 (FILHO fo FILHO)
  FRegistroK270:= TRegistroK270List.Create;  /// BLOCO K - Lista de RegistroK270 (FILHO fo FILHO)
  FRegistroK280:= TRegistroK280List.Create;  /// BLOCO K - Lista de RegistroK280 (FILHO fo FILHO)
  FRegistroK290:= TRegistroK290List.Create;  /// BLOCO K - Lista de RegistroK290 (FILHO fo FILHO)
  FRegistroK300:= TRegistroK300List.Create;  /// BLOCO K - Lista de RegistroK300 (FILHO fo FILHO)  
end;

destructor TRegistroK100.Destroy;
begin
  FRegistroK200.Free;
  FRegistroK210.Free;
  FRegistroK220.Free;
  FRegistroK230.Free;
  FRegistroK250.Free;
  FRegistroK260.Free;
  FRegistroK270.Free;
  FRegistroK280.Free;
  FRegistroK290.Free;
  FRegistroK300.Free;
  inherited;
end;

end.

