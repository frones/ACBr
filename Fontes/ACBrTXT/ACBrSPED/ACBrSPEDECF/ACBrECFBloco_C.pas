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
|* 09/09/2015 - Ariel Guareschi - Estruturado as listas.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_C;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroC040List = Class;
  TRegistroC050List = Class;
  TRegistroC051List = Class;
  TRegistroC053List = Class;
  TRegistroC100List = Class;
  TRegistroC150List = Class;
  TRegistroC155List = Class;
  TRegistroC157List = Class;
  TRegistroC350List = Class;
  TRegistroC355List = Class;

  /// Abertura do Bloco C – Informações Recuperadas da ECD
  TRegistroC001 = class(TOpenBlocos)
    FRegistroC040 :TRegistroC040List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy

    property RegistroC040: TRegistroC040List read FRegistroC040 write FRegistroC040;
  end;

  /// Identificador da ECD
  { TRegistroC040 }

  TRegistroC040 = class(TBlocos)
  private
    fDT_INI:   TDateTime;
    fHASH_ECD: string;
    fCNPJ:     string;
    fDT_FIN:   TDateTime;
    fIND_ESC:  string;
    fIND_SIT_ESP: integer;
    fNIRE:     String;
    fNAT_LIVR: string;
    fNUM_ORD:  integer;
    fCOD_VER_LC: string;

    FRegistroC050: TRegistroC050List;
    FRegistroC100: TRegistroC100List;
    FRegistroC150: TRegistroC150List;
    FRegistroC350: TRegistroC350List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property HASH_ECD: string read fHASH_ECD write fHASH_ECD;
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property IND_SIT_ESP: integer read fIND_SIT_ESP write fIND_SIT_ESP;
    property CNPJ: string read fCNPJ write fCNPJ;
    property NUM_ORD: integer read fNUM_ORD write fNUM_ORD;
    property NIRE: String read fNIRE write fNIRE;
    property NAT_LIVR: string read fNAT_LIVR write fNAT_LIVR;
    property COD_VER_LC: string read fCOD_VER_LC write fCOD_VER_LC;
    property IND_ESC: string read fIND_ESC write fIND_ESC;
    property RegistroC050: TRegistroC050List read FRegistroC050 write FRegistroC050;
    property RegistroC100: TRegistroC100List read FRegistroC100 write FRegistroC100;
    property RegistroC150: TRegistroC150List read FRegistroC150 write FRegistroC150;
    property RegistroC350: TRegistroC350List read FRegistroC350 write FRegistroC350;
  end;

  /// Registro C040 - Lista
  TRegistroC040List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC040;
    procedure SetItem(Index: Integer; const Value: TRegistroC040);
  public
    function New(): TRegistroC040;
    property Items[Index: Integer]: TRegistroC040 read GetItem write SetItem;
  end;

  /// Plano de Contas da ECD
  TRegistroC050 = class(TBlocos)
  private
    fCOD_CONTA: string;
    fDT_ALT:  TDateTime;
    fCTA:     string;
    fCOD_CTA_SUP: string;
    fIND_CTA: string;
    fNIVEL:   integer;
    fCOD_NAT: string;

    FRegistroC051: TRegistroC051List;
    FRegistroC053: TRegistroC053List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_ALT: TDateTime read fDT_ALT write fDT_ALT;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property IND_CTA: string read fIND_CTA write fIND_CTA;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_CONTA: string read fCOD_CONTA write fCOD_CONTA;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property CTA: string read fCTA write fCTA;
    property RegistroC051: TRegistroC051List read FRegistroC051 write FRegistroC051;
    property RegistroC053: TRegistroC053List read FRegistroC053 write FRegistroC053;
  end;

  /// Registro C050 - Lista
  TRegistroC050List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC050;
    procedure SetItem(Index: Integer; const Value: TRegistroC050);
  public
    function New(): TRegistroC050;
    property Items[Index: Integer]: TRegistroC050 read GetItem write SetItem;
  end;

  /// Plano de Contas Referencial
  TRegistroC051 = class(TBlocos)
  private
    fCOD_CCUS:    string;
    fCOD_CTA_REF: string;
    fCOD_ENT_REF: string;
  public
    constructor Create(); virtual; /// Create

    property COD_ENT_REF: string read fCOD_ENT_REF write fCOD_ENT_REF;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
  end;

  /// Registro C051 - Lista
  TRegistroC051List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC051;
    procedure SetItem(Index: Integer; const Value: TRegistroC051);
  public
    function New(): TRegistroC051;
    property Items[Index: Integer]: TRegistroC051 read GetItem write SetItem;
  end;

  /// Subcontas Correlatas
  TRegistroC053 = class(TBlocos)
  private
    fCOD_CNT_CORR: string;
    fNAT_SUB_CNT:  string;
    fCOD_IDT:      string;
  public
    constructor Create(); virtual; /// Create
    
    property COD_IDT: string read fCOD_IDT write fCOD_IDT;
    property COD_CNT_CORR: string read fCOD_CNT_CORR write fCOD_CNT_CORR;
    property NAT_SUB_CNT: string read fNAT_SUB_CNT write fNAT_SUB_CNT;
  end;

  /// Registro C053 - Lista
  TRegistroC053List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC053;
    procedure SetItem(Index: Integer; const Value: TRegistroC053);
  public
    function New(): TRegistroC053;
    property Items[Index: Integer]: TRegistroC053 read GetItem write SetItem;
  end;

  /// Centro de Custos
  TRegistroC100 = class(TBlocos)
  private
    fDT_ALT:   TDateTime;
    fCOD_CCUS: string;
    fCCUS:     string;
  public
    constructor Create(); virtual; /// Create
    property DT_ALT: TDateTime read fDT_ALT write fDT_ALT;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property CCUS: string read fCCUS write fCCUS;
  end;

  /// Registro C100 - Lista
  TRegistroC100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC100;
    procedure SetItem(Index: Integer; const Value: TRegistroC100);
  public
    function New(): TRegistroC100;
    property Items[Index: Integer]: TRegistroC100 read GetItem write SetItem;
  end;


  /// Identificação do Período dos Saldos Periódicos das Contas Patrimoniais
  TRegistroC150 = class(TBlocos)
  private
    fDT_FIN: TDateTime;
    fDT_INI: TDateTime;

    FRegistroC155: TRegistroC155List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property RegistroC155: TRegistroC155List read FRegistroC155 write FRegistroC155;
  end;

  /// Registro C150 - Lista
  TRegistroC150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC150;
    procedure SetItem(Index: Integer; const Value: TRegistroC150);
  public
    function New(): TRegistroC150;
    property Items[Index: Integer]: TRegistroC150 read GetItem write SetItem;
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

    FRegistroC157: TRegistroC157List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: currency read fVL_SLD_INI write fVL_SLD_INI;
    property IND_VL_SLD_INI: string read fIND_VL_SLD_INI write fIND_VL_SLD_INI;
    property VL_DEB: currency read fVL_DEB write fVL_DEB;
    property VL_CRED: currency read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: currency read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;

    property RegistroC157: TRegistroC157List read FRegistroC157 write FRegistroC157;
  end;


  /// Registro C155 - Lista
  TRegistroC155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC155;
    procedure SetItem(Index: Integer; const Value: TRegistroC155);
  public
    function New(): TRegistroC155;
    property Items[Index: Integer]: TRegistroC155 read GetItem write SetItem;
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
    constructor Create(); virtual; /// Create

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;
  end;

  /// Registro C157 - Lista
  TRegistroC157List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC157;
    procedure SetItem(Index: Integer; const Value: TRegistroC157);
  public
    function New(): TRegistroC157;
    property Items[Index: Integer]: TRegistroC157 read GetItem write SetItem;
  end;

  /// Identificação da Data dos Saldos das Contas de Resultado Antes do Encerramento
  TRegistroC350 = class(TBlocos)
  private
    fDT_RES: TDateTime;
    FRegistroC355: TRegistroC355List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_RES: TDateTime read fDT_RES write fDT_RES;
    property RegistroC355: TREgistroc355List read FRegistroC355 write FRegistroC355;
  end;


  /// Registro C350 - Lista
  TRegistroC350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC350;
    procedure SetItem(Index: Integer; const Value: TRegistroC350);
  public
    function New(): TRegistroC350;
    property Items[Index: Integer]: TRegistroC350 read GetItem write SetItem;
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
    constructor Create(); virtual; /// Create
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: Variant read fVL_CTA write fVL_CTA;
    property IND_VL_CTA: string read fIND_VL_CTA write fIND_VL_CTA;
    property LINHA_ECD: integer read fLINHA_ECD write fLINHA_ECD;
  end;

  
  /// Registro C355 - Lista
  TRegistroC355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC355;
    procedure SetItem(Index: Integer; const Value: TRegistroC355);
  public
    function New(): TRegistroC355;
    property Items[Index: Integer]: TRegistroC355 read GetItem write SetItem;
  end;

  /// Registro C990 - ENCERRAMENTO DO BLOCO C
  TRegistroC990 = class(TCloseBlocos)
    fQTD_LIN: Integer; /// Quantidade total de linhas do Bloco C
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;

implementation

{ TRegistroC001 }

constructor TRegistroC001.Create;
begin
   inherited Create;
   FRegistroC040 := TRegistroC040List.Create;
   //
   IND_DAD := idComDados;
end;

destructor TRegistroC001.Destroy;
begin
  FRegistroC040.Free;

  inherited;
end;

{ TRegistroC040List }

function TRegistroC040List.GetItem(Index: Integer): TRegistroC040;
begin
  Result := TRegistroC040(Inherited Items[Index]);
end;

function TRegistroC040List.New(): TRegistroC040;
begin
  Result := TRegistroC040.Create();
  Add(Result);
end;

procedure TRegistroC040List.SetItem(Index: Integer;
  const Value: TRegistroC040);
begin
  Put(Index, Value);
end;

{ TRegistroC050List }

function TRegistroC050List.GetItem(Index: Integer): TRegistroC050;
begin
  Result := TRegistroC050(Inherited Items[Index]);
end;

function TRegistroC050List.New(): TRegistroC050;
begin
  Result := TRegistroC050.Create();
  Add(Result);
end;

procedure TRegistroC050List.SetItem(Index: Integer;
  const Value: TRegistroC050);
begin
  Put(Index, Value);
end;

{ TRegistroC051List }

function TRegistroC051List.GetItem(Index: Integer): TRegistroC051;
begin
  Result := TRegistroC051(Inherited Items[Index]);
end;

function TRegistroC051List.New(): TRegistroC051;
begin
  Result := TRegistroC051.Create();
  Add(Result);
end;

procedure TRegistroC051List.SetItem(Index: Integer;
  const Value: TRegistroC051);
begin
  Put(Index, Value);
end;

{ TRegistroC053List }

function TRegistroC053List.GetItem(Index: Integer): TRegistroC053;
begin
  Result := TRegistroC053(Inherited Items[Index]);
end;

function TRegistroC053List.New(): TRegistroC053;
begin
  Result := TRegistroC053.Create();
  Add(Result);
end;

procedure TRegistroC053List.SetItem(Index: Integer;
  const Value: TRegistroC053);
begin
  Put(Index, Value);
end;

{ TRegistroC040 }

constructor TRegistroC040.Create();
begin
   inherited Create;
   FRegistroC050 := TRegistroC050List.Create;
   FRegistroC100 := TRegistroC100List.Create;
   FRegistroC150 := TRegistroC150List.Create;
   FRegistroC350 := TRegistroC350List.Create;
end;

destructor TRegistroC040.Destroy;
begin
  FRegistroC050.Free;
  FRegistroC100.Free;
  FRegistroC150.Free;
  FRegistroC350.Free;
  inherited;
end;

{ TRegistroC050 }

constructor TRegistroC050.Create();
begin
  inherited Create;
  FRegistroC051 := TRegistroC051List.Create;
  FRegistroC053 := TRegistroC053List.Create;
end;

destructor TRegistroC050.Destroy;
begin
  FRegistroC051.Free;
  FRegistroC053.Free;
  
  inherited;
end;

{ TRegistroC051 }

constructor TRegistroC051.Create();
begin
  inherited Create;
end;

{ TRegistroC053 }

constructor TRegistroC053.Create();
begin
  inherited Create;
end;

{ TRegistroC100 }

constructor TRegistroC100.Create();
begin
  inherited Create;
end;

{ TRegistroC100List }

function TRegistroC100List.GetItem(Index: Integer): TRegistroC100;
begin
  Result := TRegistroC100(Inherited Items[Index]);
end;

function TRegistroC100List.New(): TRegistroC100;
begin
  Result := TRegistroC100.Create();
  Add(Result);
end;

procedure TRegistroC100List.SetItem(Index: Integer;
  const Value: TRegistroC100);
begin
  Put(Index, Value);
end;

{ TRegistroC150 }

constructor TRegistroC150.Create();
begin
  inherited Create;
  FRegistroC155 := TRegistroC155List.Create;
end;

destructor TRegistroC150.Destroy;
begin
  FRegistroC155.Free;
  inherited;
end;

{ TRegistroC150List }

function TRegistroC150List.GetItem(Index: Integer): TRegistroC150;
begin
  Result := TRegistroC150(Inherited Items[Index]);
end;

function TRegistroC150List.New(): TRegistroC150;
begin
  Result := TRegistroC150.Create();
  Add(Result);
end;

procedure TRegistroC150List.SetItem(Index: Integer;
  const Value: TRegistroC150);
begin
  Put(Index, Value);
end;

{ TRegistroC155 }

constructor TRegistroC155.Create();
begin
  inherited Create;
  FRegistroC157 := TRegistroC157List.Create();
end;

destructor TRegistroC155.Destroy;
begin
  FRegistroC157.Free;
  inherited;
end;

{ TRegistroC157List }

function TRegistroC157List.GetItem(Index: Integer): TRegistroC157;
begin
  Result := TRegistroC157(Inherited Items[Index]);
end;

function TRegistroC157List.New(): TRegistroC157;
begin
  Result := TRegistroC157.Create();
  Add(Result);
end;

procedure TRegistroC157List.SetItem(Index: Integer;
  const Value: TRegistroC157);
begin
  Put(Index, Value);
end;

{ TRegistroC155List }

function TRegistroC155List.GetItem(Index: Integer): TRegistroC155;
begin
  Result := TRegistroC155(Inherited Items[Index]);
end;

function TRegistroC155List.New(): TRegistroC155;
begin
  Result := TRegistroC155.Create();
  Add(Result);
end;

procedure TRegistroC155List.SetItem(Index: Integer;
  const Value: TRegistroC155);
begin
  Put(Index, Value);
end;

{ TRegistroC157 }

constructor TRegistroC157.Create();
begin
  inherited Create;

end;

{ TRegistroC350 }

constructor TRegistroC350.Create();
begin
  inherited Create;
  FRegistroC355 := TRegistroC355List.Create();
end;

destructor TRegistroC350.Destroy;
begin
  FRegistroC355.Free;
  inherited;
end;

{ TRegistroC350List }

function TRegistroC350List.GetItem(Index: Integer): TRegistroC350;
begin
  Result := TRegistroC350(Inherited Items[Index]);
end;

function TRegistroC350List.New(): TRegistroC350;
begin
  Result := TRegistroC350.Create();
  Add(Result);
end;

procedure TRegistroC350List.SetItem(Index: Integer;
  const Value: TRegistroC350);
begin
  Put(Index, Value);
end;

{ TRegistroC355 }

constructor TRegistroC355.Create();
begin
  inherited Create;
end;

{ TRegistroC355List }

function TRegistroC355List.GetItem(Index: Integer): TRegistroC355;
begin
  Result := TRegistroC355(Inherited Items[Index]);
end;

function TRegistroC355List.New(): TRegistroC355;
begin
  Result := TRegistroC355.Create();
  Add(Result);
end;

procedure TRegistroC355List.SetItem(Index: Integer;
  const Value: TRegistroC355);
begin
  Put(Index, Value);
end;

end.

