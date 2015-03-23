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
  TRegistroK220List = class;
  TRegistroK230List = class;
  TRegistroK235List = class;
  TRegistroK250List = class;
  TRegistroK255List = class;

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
    FRegistroK220: TRegistroK220List;
    FRegistroK230: TRegistroK230List;
    FRegistroK250: TRegistroK250List;
  public
    constructor Create(AOwner: TRegistroK001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI : TDateTime read fDT_INI write fDT_INI;
    property DT_FIN : TDateTime read fDT_FIN write fDT_FIN;

    property RegistroK200: TRegistroK200List read FRegistroK200 write FRegistroK200;
    property RegistroK220: TRegistroK220List read FRegistroK220 write FRegistroK220;
    property RegistroK230: TRegistroK230List read FRegistroK230 write FRegistroK230;
    property RegistroK250: TRegistroK250List read FRegistroK250 write FRegistroK250;
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
    fQTD: double;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_EST : TDateTime read fDT_EST write fDT_EST;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD : double read fQTD write fQTD;
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

  /// Registro K220 - OUTRAS MOVIMENTAÇÕES INTERNA ENTRE MERCADORIAS

  TRegistroK220 = class
  private
    fCOD_ITEM_DEST: string;
    fCOD_ITEM_ORI: string;
    fDT_MOV: TDateTime;
    fQTD: double;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_MOV : TDateTime read fDT_MOV write fDT_MOV;
    property COD_ITEM_ORI : string read fCOD_ITEM_ORI write fCOD_ITEM_ORI;
    property COD_ITEM_DEST : string read fCOD_ITEM_DEST write fCOD_ITEM_DEST;
    property QTD : double read fQTD write fQTD;
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
    fQTD_ENC: double;
    FRegistroK235: TRegistroK235List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI_OP : TDateTime read fDT_INI_OP write fDT_INI_OP;
    property DT_FIN_OP : TDateTime read fDT_FIN_OP write fDT_FIN_OP;
    property COD_DOC_OP : string read fCOD_DOC_OP write fCOD_DOC_OP;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD_ENC : double read fQTD_ENC write fQTD_ENC;

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
    fQTD: double;
  public
    constructor Create(AOwner: TRegistroK230); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_SAIDA : TDateTime  read fDT_SAIDA write fDT_SAIDA;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD: double read fQTD write fQTD;
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
    fQTD: double;
    fRegistroK255: TRegistroK255List;
  public
    constructor Create(AOwner: TRegistroK100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_PROD : TDateTime read fDT_PROD write fDT_PROD;
    property COD_ITEM : String  read fCOD_ITEM write fCOD_ITEM;
    property QTD : double read fQTD write fQTD;

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
    fQTD: double;
  public
    constructor Create(AOwner: TRegistroK250); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_CONS : TDateTime read fDT_CONS write fDT_CONS;
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property QTD : double read fQTD write fQTD;
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
     FRegistroK100 := TRegistroK100List.Create;
     //
     IND_MOV := imSemDados;
end;

destructor TRegistroK001.Destroy;
begin
  FRegistroK100.Free;
  inherited Destroy;
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
  FRegistroK220:= TRegistroK220List.Create;  /// BLOCO K - Lista de RegistroK220 (FILHO fo FILHO)
  FRegistroK230:= TRegistroK230List.Create;  /// BLOCO K - Lista de RegistroK230 (FILHO fo FILHO)
  FRegistroK250:= TRegistroK250List.Create;  /// BLOCO K - Lista de RegistroK250 (FILHO fo FILHO)
end;

destructor TRegistroK100.Destroy;
begin
  FRegistroK200.Free;
  FRegistroK220.Free;
  FRegistroK230.Free;
  FRegistroK250.Free;
  inherited;
end;

end.

