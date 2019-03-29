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
|* --/--/2015: Juliomar Marchetti
|*  - Criação.
|* 12/08/2015: Isaque Pinheiro
|*  - Distribuição da primeira versão.
|* 21/08/2015: Lutzem Massao Aihara
|*  - Classe reestruturada.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_M;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroM010List = class;
  TRegistroM030List = class;
  TRegistroM300List = class;
  TRegistroM305List = class;
  TRegistroM310List = class;
  TRegistroM312List = class;
  TRegistroM315List = class;
  TRegistroM350List = class;
  TRegistroM355List = class;
  TRegistroM360List = class;
  TRegistroM362List = class;
  TRegistroM365List = class;
  TRegistroM410List = class;
  TRegistroM415List = class;
  TRegistroM500List = class;

  /// Registro M001 - Abertura do Bloco M – Livro Eletrônico de
  /// Apuração do Lucro Real (e-Lalur) e Licro Eletrônico
  /// de Apuração da Base de Cálculo da CSLL (e-Lacs)
  TRegistroM001 = class(TOpenBlocos)
  private
    FRegistroM010 :TRegistroM010List;
    FRegistroM030 :TRegistroM030List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy

    property RegistroM010: TRegistroM010List read FRegistroM010 write FRegistroM010;
    property RegistroM030: TRegistroM030List read FRegistroM030 write FRegistroM030;
  end;

  /// Registro M010 - Identificação da Conta na Parte B e-Lalur e do e-Lacs
  TRegistroM010 = class(TBlocos)
  private
    fCOD_CTA_B:        String;
    fDESC_CTA_LAL:     String;
    fDT_AP_LAL:        TDateTime;
    fCOD_PB_RFB:       String;
    fCOD_LAN_ORIG:     Integer;
    fDESC_LAN_ORIG:    String;
    fDT_LIM_LAL:       TDateTime;
    fCOD_TRIBUTO:      String;
    fVL_SALDO_INI:     Variant;
    fIND_Vl_SALDO_INI: String;
    fCNPJ_SIT_ESP:     String;
  public
    property COD_CTA_B:        String read fCOD_CTA_B write fCOD_CTA_B;
    property DESC_CTA_LAL:     String read fDESC_CTA_LAL write fDESC_CTA_LAL;
    property DT_AP_LAL:        TDateTime read fDT_AP_LAL write fDT_AP_LAL;
    property COD_PB_RFB:       String read fCOD_PB_RFB write fCOD_PB_RFB;
    property COD_LAN_ORIG:     Integer read fCOD_LAN_ORIG write fCOD_LAN_ORIG;
    property DESC_LAN_ORIG:    String read fDESC_LAN_ORIG write fDESC_LAN_ORIG;
    property DT_LIM_LAL:       TDateTime read fDT_LIM_LAL write fDT_LIM_LAL;
    property COD_TRIBUTO:      String read fCOD_TRIBUTO write fCOD_TRIBUTO;
    property VL_SALDO_INI:     Variant read fVl_SALDO_INI write fVl_SALDO_INI;
    property IND_Vl_SALDO_INI: String read fIND_Vl_SALDO_INI write fIND_Vl_SALDO_INI;
    property CNPJ_SIT_ESP:     String read fCNPJ_SIT_ESP write fCNPJ_SIT_ESP;
  end;

  TRegistroM010List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM010;
    procedure SetItem(Index: Integer; const Value: TRegistroM010);
  public
    function New: TRegistroM010;
    property Items[Index: Integer]: TRegistroM010 read GetItem write SetItem;
  end;

  /// Registro M030 - Identificação do Período e Forma de Apuração do
  /// IRPJ e da CSLL das Empresas Tributadas pelo Lucro Real
  TRegistroM030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    fPER_APUR: String;

    FRegistroM300: TRegistroM300List;
    FRegistroM350: TRegistroM350List;
    FRegistroM410: TRegistroM410List;
    FRegistroM500: TRegistroM500List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    ///
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    property RegistroM300: TRegistroM300List read FRegistroM300 write FRegistroM300;
    property RegistroM350: TRegistroM350List read FRegistroM350 write FRegistroM350;
    property RegistroM410: TRegistroM410List read FRegistroM410 write FRegistroM410;
    property RegistroM500: TRegistroM500List read FRegistroM500 write FRegistroM500;
  end;

  TRegistroM030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM030;
    procedure SetItem(Index: Integer; const Value: TRegistroM030);
  public
    function New: TRegistroM030;
    property Items[Index: Integer]: TRegistroM030 read GetItem write SetItem;
  end;

  /// Registro M300 - Lançamentos da Parte A do e-Lalur
  TRegistroM300 = class(TBlocos)
  private
    fHIST_LAN_LAL: string;
    fIND_RELACAO: integer;
    fCODIGO:    string;
    fTIPO_LANCAMENTO: string;
    fDESCRICAO: string;
    fVALOR:     variant;

    FRegistroM305: TRegistroM305List;
    FRegistroM310: TRegistroM310List;
    FRegistroM315: TRegistroM315List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO_LANCAMENTO: string read fTIPO_LANCAMENTO write fTIPO_LANCAMENTO;
    property IND_RELACAO: integer read fIND_RELACAO write fIND_RELACAO;
    property VALOR: variant read fVALOR write fVALOR;
    property HIST_LAN_LAL: string read fHIST_LAN_LAL write fHIST_LAN_LAL;

    property RegistroM305: TRegistroM305List read FRegistroM305 write FRegistroM305;
    property RegistroM310: TRegistroM310List read FRegistroM310 write FRegistroM310;
    property RegistroM315: TRegistroM315List read FRegistroM315 write FRegistroM315;
  end;

  TRegistroM300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM300;
    procedure SetItem(Index: Integer; const Value: TRegistroM300);
  public
    function New: TRegistroM300;
    property Items[Index: Integer]: TRegistroM300 read GetItem write SetItem;
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

  TRegistroM305List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM305;
    procedure SetItem(Index: Integer; const Value: TRegistroM305);
  public
    function New: TRegistroM305;
    property Items[Index: Integer]: TRegistroM305 read GetItem write SetItem;
  end;

  /// Registro M310 - Contas Contábeis Relacionadas ao Lançamento da Parte A do e-Lalur.
  TRegistroM310 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fCOD_CCUS:   string;
    fVL_CTA:     variant;
    fIND_VL_CTA: string;

    FRegistroM312: TRegistroM312List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;

    property RegistroM312: TRegistroM312List read FRegistroM312 write FRegistroM312;
  end;

  TRegistroM310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM310;
    procedure SetItem(Index: Integer; const Value: TRegistroM310);
  public
    function New: TRegistroM310;
    property Items[Index: Integer]: TRegistroM310 read GetItem write SetItem;
  end;

  /// Registro M312 - Números dos Lançamentos Relacionados à Conta Contábil
  TRegistroM312 = class(TBlocos)
  private
    fNUM_LCTO: string;
  public
    property NUM_LCTO: string read fNUM_LCTO write fNUM_LCTO;
  end;

  TRegistroM312List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM312;
    procedure SetItem(Index: Integer; const Value: TRegistroM312);
  public
    function New: TRegistroM312;
    property Items[Index: Integer]: TRegistroM312 read GetItem write SetItem;
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

  TRegistroM315List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM315;
    procedure SetItem(Index: Integer; const Value: TRegistroM315);
  public
    function New: TRegistroM315;
    property Items[Index: Integer]: TRegistroM315 read GetItem write SetItem;
  end;

  /// Registro M350 - Lançamentos da Parte A do e-Lacs
  TRegistroM350 = class(TBlocos)
  private
    fHIST_LAN_LAL: string;
    fVALOR:     variant;
    fCODIGO:    string;
    fDESCRICAO: string;
    fTIPO_LANCAMENTO: string;
    fIND_RELACAO: integer;

    FRegistroM355: TRegistroM355List;
    FRegistroM360: TRegistroM360List;
    FRegistroM365: TRegistroM365List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO_LANCAMENTO: string read fTIPO_LANCAMENTO write fTIPO_LANCAMENTO;
    property IND_RELACAO: integer read fIND_RELACAO write fIND_RELACAO;
    property VALOR: variant read fVALOR write fVALOR;
    property HIST_LAN_LAL: string read fHIST_LAN_LAL write fHIST_LAN_LAL;

    property RegistroM355: TRegistroM355List read FRegistroM355 write FRegistroM355;
    property RegistroM360: TRegistroM360List read FRegistroM360 write FRegistroM360;
    property RegistroM365: TRegistroM365List read FRegistroM365 write FRegistroM365;
  end;

  TRegistroM350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM350;
    procedure SetItem(Index: Integer; const Value: TRegistroM350);
  public
    function New: TRegistroM350;
    property Items[Index: Integer]: TRegistroM350 read GetItem write SetItem;
  end;

  /// Registro M355 - Conta da Parte B do e-Lacs
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

  TRegistroM355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM355;
    procedure SetItem(Index: Integer; const Value: TRegistroM355);
  public
    function New: TRegistroM355;
    property Items[Index: Integer]: TRegistroM355 read GetItem write SetItem;
  end;

  /// Registro M360 - Contas Contábeis Relacionadas ao Lançamento da
  /// Parte A do e-Lacs.
  TRegistroM360 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fCOD_CCUS:   string;
    fVL_CTA:     variant;
    fIND_VL_CTA: string;

    FRegistroM362: TRegistroM362List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;

    property RegistroM362: TRegistroM362List read FRegistroM362 write FRegistroM362;
  end;

  TRegistroM360List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM360;
    procedure SetItem(Index: Integer; const Value: TRegistroM360);
  public
    function New: TRegistroM360;
    property Items[Index: Integer]: TRegistroM360 read GetItem write SetItem;
  end;

  /// Registro M362 - Números dos Lançamentos Relacionados à Conta Contábil
  TRegistroM362 = class(TBlocos)
  private
    fNUM_LCTO: string;
  public
    property NUM_LCTO: string read fNUM_LCTO write fNUM_LCTO;
  end;

  TRegistroM362List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM362;
    procedure SetItem(Index: Integer; const Value: TRegistroM362);
  public
    function New: TRegistroM362;
    property Items[Index: Integer]: TRegistroM362 read GetItem write SetItem;
  end;

  /// Registro M365 - Identificação de Processos Judiciais e
  /// Administrativos Referentes ao Lançamento
  TRegistroM365 = class(TBlocos)
  private
    fIND_PROC: string;
    fNUM_PROC: string;
  public
    property IND_PROC: string read fIND_PROC write fIND_PROC;
    property NUM_PROC: string read fNUM_PROC write fNUM_PROC;
  end;

  TRegistroM365List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM365;
    procedure SetItem(Index: Integer; const Value: TRegistroM365);
  public
    function New: TRegistroM365;
    property Items[Index: Integer]: TRegistroM365 read GetItem write SetItem;
  end;

  /// Registro M410 - Lançamentos na Conta da Parte B do e-Lalur e do e-
  /// Lacs Sem Reflexo na Parte A
  TRegistroM410 = class(TBlocos)
  private
    fCOD_CTA_B:     string;
    fCOD_CTA_B_CTP: string;
    fCOD_TRIBUTO:   string;
    fHIST_LAN_LALB: string;
    fIND_LAN_ANT:   string;
    fIND_VAL_LAN_LALB_PB: string;
    fVAL_LAN_LALB_PB: variant;

    FRegistroM415: TRegistroM415List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property COD_TRIBUTO: string read fCOD_TRIBUTO write fCOD_TRIBUTO;
    property VAL_LAN_LALB_PB: variant read fVAL_LAN_LALB_PB write fVAL_LAN_LALB_PB;
    property IND_VAL_LAN_LALB_PB: string read fIND_VAL_LAN_LALB_PB write fIND_VAL_LAN_LALB_PB;
    property COD_CTA_B_CTP: string read fCOD_CTA_B_CTP write fCOD_CTA_B_CTP;
    property HIST_LAN_LALB: string read fHIST_LAN_LALB write fHIST_LAN_LALB;
    property IND_LAN_ANT: string read fIND_LAN_ANT write fIND_LAN_ANT;

    property RegistroM415: TRegistroM415List read FRegistroM415 write FRegistroM415;
  end;

  TRegistroM410List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM410;
    procedure SetItem(Index: Integer; const Value: TRegistroM410);
  public
    function New: TRegistroM410;
    property Items[Index: Integer]: TRegistroM410 read GetItem write SetItem;
  end;

  /// Registro M415 - Identificação de Processos Judiciais e
  /// Administrativos Referentes ao Lançamento
  TRegistroM415 = class(TBlocos)
  private
    fIND_PROC: string;
    fNUM_PROC: string;
  public
    property IND_PROC: string read fIND_PROC write fIND_PROC;
    property NUM_PROC: string read fNUM_PROC write fNUM_PROC;
  end;

  TRegistroM415List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM415;
    procedure SetItem(Index: Integer; const Value: TRegistroM415);
  public
    function New: TRegistroM415;
    property Items[Index: Integer]: TRegistroM415 read GetItem write SetItem;
  end;

  /// Registro M500 - Controle de Saldos das Contas da Parte B do e-Lalur
  /// e do e-Lacs
  TRegistroM500 = class(TBlocos)
  private
    fCOD_CTA_B:      string;
    fCOD_TRIBUTO:    string;
    fIND_SD_FIM_LAL: string;
    fIND_SD_INI_LAL: string;
    fIND_VL_LCTO_PARTE_A: string;
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
    property IND_VL_LCTO_PARTE_A: string read fIND_VL_LCTO_PARTE_A write fIND_VL_LCTO_PARTE_A;
    property VL_LCTO_PARTE_B: variant read fVL_LCTO_PARTE_B write fVL_LCTO_PARTE_B;
    property IND_VL_LCTO_PARTE_B: string read fIND_VL_LCTO_PARTE_B write fIND_VL_LCTO_PARTE_B;
    property SD_FIM_LAL: variant read fSD_FIM_LAL write fSD_FIM_LAL;
    property IND_SD_FIM_LAL: string read fIND_SD_FIM_LAL write fIND_SD_FIM_LAL;
  end;

  TRegistroM500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM500;
    procedure SetItem(Index: Integer; const Value: TRegistroM500);
  public
    function New: TRegistroM500;
    property Items[Index: Integer]: TRegistroM500 read GetItem write SetItem;
  end;

  /// Registro M990 - ENCERRAMENTO DO BLOCO M
  TRegistroM990 = class(TCloseBlocos)
  private
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;


implementation

{ TRegistroM010List }

function TRegistroM010List.GetItem(Index: Integer): TRegistroM010;
begin
  Result := TRegistroM010(Inherited Items[Index]);
end;

function TRegistroM010List.New: TRegistroM010;
begin
  Result := TRegistroM010.Create;
  Add(Result);
end;

procedure TRegistroM010List.SetItem(Index: Integer; const Value: TRegistroM010);
begin
  Put(Index, Value);
end;

{ TRegistroM030List }

function TRegistroM030List.GetItem(Index: Integer): TRegistroM030;
begin
  Result := TRegistroM030(Inherited Items[Index]);
end;

function TRegistroM030List.New: TRegistroM030;
begin
  Result := TRegistroM030.Create;
  Add(Result);
end;

procedure TRegistroM030List.SetItem(Index: Integer; const Value: TRegistroM030);
begin
  Put(Index, Value);
end;

{ TRegistroM300List }

function TRegistroM300List.GetItem(Index: Integer): TRegistroM300;
begin
  Result := TRegistroM300(Inherited Items[Index]);
end;

function TRegistroM300List.New: TRegistroM300;
begin
  Result := TRegistroM300.Create;
  Add(Result);
end;

procedure TRegistroM300List.SetItem(Index: Integer; const Value: TRegistroM300);
begin
  Put(Index, Value);
end;

{ TRegistroM305List }

function TRegistroM305List.GetItem(Index: Integer): TRegistroM305;
begin
  Result := TRegistroM305(Inherited Items[Index]);
end;

function TRegistroM305List.New: TRegistroM305;
begin
  Result := TRegistroM305.Create;
  Add(Result);
end;

procedure TRegistroM305List.SetItem(Index: Integer; const Value: TRegistroM305);
begin
  Put(Index, Value);
end;

{ TRegistroM310List }

function TRegistroM310List.GetItem(Index: Integer): TRegistroM310;
begin
  Result := TRegistroM310(Inherited Items[Index]);
end;

function TRegistroM310List.New: TRegistroM310;
begin
  Result := TRegistroM310.Create;
  Add(Result);
end;

procedure TRegistroM310List.SetItem(Index: Integer; const Value: TRegistroM310);
begin
  Put(Index, Value);
end;

{ TRegistroM312List }

function TRegistroM312List.GetItem(Index: Integer): TRegistroM312;
begin
  Result := TRegistroM312(Inherited Items[Index]);
end;

function TRegistroM312List.New: TRegistroM312;
begin
  Result := TRegistroM312.Create;
  Add(Result);
end;

procedure TRegistroM312List.SetItem(Index: Integer; const Value: TRegistroM312);
begin
  Put(Index, Value);
end;

{ TRegistroM315List }

function TRegistroM315List.GetItem(Index: Integer): TRegistroM315;
begin
  Result := TRegistroM315(Inherited Items[Index]);
end;

function TRegistroM315List.New: TRegistroM315;
begin
  Result := TRegistroM315.Create;
  Add(Result);
end;

procedure TRegistroM315List.SetItem(Index: Integer; const Value: TRegistroM315);
begin
  Put(Index, Value);
end;

{ TRegistroM350List }

function TRegistroM350List.GetItem(Index: Integer): TRegistroM350;
begin
  Result := TRegistroM350(Inherited Items[Index]);
end;

function TRegistroM350List.New: TRegistroM350;
begin
  Result := TRegistroM350.Create;
  Add(Result);
end;

procedure TRegistroM350List.SetItem(Index: Integer; const Value: TRegistroM350);
begin
  Put(Index, Value);
end;

{ TRegistroM355List }

function TRegistroM355List.GetItem(Index: Integer): TRegistroM355;
begin
  Result := TRegistroM355(Inherited Items[Index]);
end;

function TRegistroM355List.New: TRegistroM355;
begin
  Result := TRegistroM355.Create;
  Add(Result);
end;

procedure TRegistroM355List.SetItem(Index: Integer; const Value: TRegistroM355);
begin
  Put(Index, Value);
end;

{ TRegistroM360List }

function TRegistroM360List.GetItem(Index: Integer): TRegistroM360;
begin
  Result := TRegistroM360(Inherited Items[Index]);
end;

function TRegistroM360List.New: TRegistroM360;
begin
  Result := TRegistroM360.Create;
  Add(Result);
end;

procedure TRegistroM360List.SetItem(Index: Integer; const Value: TRegistroM360);
begin
  Put(Index, Value);
end;

{ TRegistroM362List }

function TRegistroM362List.GetItem(Index: Integer): TRegistroM362;
begin
  Result := TRegistroM362(Inherited Items[Index]);
end;

function TRegistroM362List.New: TRegistroM362;
begin
  Result := TRegistroM362.Create;
  Add(Result);
end;

procedure TRegistroM362List.SetItem(Index: Integer; const Value: TRegistroM362);
begin
  Put(Index, Value);
end;

{ TRegistroM365List }

function TRegistroM365List.GetItem(Index: Integer): TRegistroM365;
begin
  Result := TRegistroM365(Inherited Items[Index]);
end;

function TRegistroM365List.New: TRegistroM365;
begin
  Result := TRegistroM365.Create;
  Add(Result);
end;

procedure TRegistroM365List.SetItem(Index: Integer; const Value: TRegistroM365);
begin
  Put(Index, Value);
end;

{ TRegistroM410List }

function TRegistroM410List.GetItem(Index: Integer): TRegistroM410;
begin
  Result := TRegistroM410(Inherited Items[Index]);
end;

function TRegistroM410List.New: TRegistroM410;
begin
  Result := TRegistroM410.Create;
  Add(Result);
end;

procedure TRegistroM410List.SetItem(Index: Integer; const Value: TRegistroM410);
begin
  Put(Index, Value);
end;

{ TRegistroM415List }

function TRegistroM415List.GetItem(Index: Integer): TRegistroM415;
begin
  Result := TRegistroM415(Inherited Items[Index]);
end;

function TRegistroM415List.New: TRegistroM415;
begin
  Result := TRegistroM415.Create;
  Add(Result);
end;

procedure TRegistroM415List.SetItem(Index: Integer; const Value: TRegistroM415);
begin
  Put(Index, Value);
end;

{ TRegistroM500List }

function TRegistroM500List.GetItem(Index: Integer): TRegistroM500;
begin
  Result := TRegistroM500(Inherited Items[Index]);
end;

function TRegistroM500List.New: TRegistroM500;
begin
  Result := TRegistroM500.Create;
  Add(Result);
end;

procedure TRegistroM500List.SetItem(Index: Integer; const Value: TRegistroM500);
begin
  Put(Index, Value);
end;

{ TRegistroM030 }

constructor TRegistroM030.Create;
begin
  inherited Create;
  FRegistroM300 := TRegistroM300List.Create;
  FRegistroM350 := TRegistroM350List.Create;
  FRegistroM410 := TRegistroM410List.Create;
  FRegistroM500 := TRegistroM500List.Create;
end;

destructor TRegistroM030.Destroy;
begin
  FRegistroM300.Free;
  FRegistroM350.Free;
  FRegistroM410.Free;
  FRegistroM500.Free;
  inherited;
end;

{ TRegistroM300 }

constructor TRegistroM300.Create;
begin
  inherited Create;
  FRegistroM305 := TRegistroM305List.Create;
  FRegistroM310 := TRegistroM310List.Create;
  FRegistroM315 := TRegistroM315List.Create;
end;

destructor TRegistroM300.Destroy;
begin
  FRegistroM305.Free;
  FRegistroM310.Free;
  FRegistroM315.Free;
  inherited;
end;

{ TRegistroM310 }

constructor TRegistroM310.Create;
begin
  inherited Create;
  FRegistroM312 := TRegistroM312List.Create;
end;

destructor TRegistroM310.Destroy;
begin
  FRegistroM312.Free;
  inherited;
end;

{ TRegistroM350 }

constructor TRegistroM350.Create;
begin
  inherited Create;
  RegistroM355 := TRegistroM355List.Create;
  RegistroM360 := TRegistroM360List.Create;
  RegistroM365 := TRegistroM365List.Create;
end;

destructor TRegistroM350.Destroy;
begin
  RegistroM355.Free;
  RegistroM360.Free;
  RegistroM365.Free;
  inherited;
end;

{ TRegistroM360 }

constructor TRegistroM360.Create;
begin
  inherited Create;
  FRegistroM362 := TRegistroM362List.Create;
end;

destructor TRegistroM360.Destroy;
begin
  FRegistroM362.Free;
  inherited;
end;

{ TRegistroM410 }

constructor TRegistroM410.Create;
begin
  inherited Create;
  FRegistroM415 := TRegistroM415List.Create;
end;

destructor TRegistroM410.Destroy;
begin
  FRegistroM415.Free;
  inherited;
end;

{ TRegistroM001 }

constructor TRegistroM001.Create;
begin
  inherited Create;
  FRegistroM010 := TRegistroM010List.Create;
  FRegistroM030 := TRegistroM030List.Create;
  IND_DAD := idComDados;;
end;

destructor TRegistroM001.Destroy;
begin
  FRegistroM010.Free;
  FRegistroM030.Free;
  inherited;
end;

end.
