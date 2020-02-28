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
|* 11/09/2015 - Ariel Guareschi - Estruturar a classe com suas listas
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_E;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroE010List = Class;
  TRegistroE015List = Class;
  TRegistroE020List = Class;
  TRegistroE030List = Class;
  TRegistroE155List = Class;
  TRegistroE355List = Class;

  /// Registro E001 - Abertura do Bloco E – Informações Recuperadas da
  /// ECF Anterior e Cálculo Fiscal dos Dados Recuperados da ECD
  TRegistroE001 = class(TOpenBlocos)
    FRegistroE010 :TRegistroE010List;
    FRegistroE020 :TRegistroE020List;
    FRegistroE030 :TRegistroE030List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy

    property RegistroE010: TRegistroE010List read FRegistroE010 write FRegistroE010;
    property RegistroE020: TRegistroE020List read FRegistroE020 write FRegistroE020;
    property RegistroE030: TRegistroE030List read FRegistroE030 write FRegistroE030;

  end;

  /// Registro E010 - Saldos Finais Recuperados da ECF Anterior
  TRegistroE010 = class(TBlocos)
  private
    fVAL_CTA_REF:  currency;
    fIND_VAL_CTA_REF: string;
    fCOD_NAT:      string;
    fCOD_CTA_REF:  string;
    fDESC_CTA_REF: string;
    FRegistroE015: TRegistroE015List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
    property DESC_CTA_REF: string read fDESC_CTA_REF write fDESC_CTA_REF;
    property VAL_CTA_REF: currency read fVAL_CTA_REF write fVAL_CTA_REF;
    property IND_VAL_CTA_REF: string read fIND_VAL_CTA_REF write fIND_VAL_CTA_REF;
    property RegistroE015: TRegistroE015List read FRegistroE015 write FRegistroE015;
  end;

  /// Registro E010 - Lista
  TRegistroE010List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE010;
    procedure SetItem(Index: Integer; const Value: TRegistroE010);
  public
    function New(): TRegistroE010;
    property Items[Index: Integer]: TRegistroE010 read GetItem write SetItem;
  end;

  /// Registro E015 - Contas Contábeis Mapeadas
  TRegistroE015 = class(TBlocos)
  private
    fCOD_CTA:     string;
    fCOD_CCUS:    string;
    fVAL_CTA:     variant;
    fDESC_CTA:    string;
    fIND_VAL_CTA: string;
  public
    constructor Create(); virtual; /// Create
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property DESC_CTA: string read fDESC_CTA write fDESC_CTA;
    property VAL_CTA: variant read fVAL_CTA write fVAL_CTA;
    property IND_VAL_CTA: string read fIND_VAL_CTA write fIND_VAL_CTA;
  end;

  /// Registro E015 - Lista
  TRegistroE015List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE015;
    procedure SetItem(Index: Integer; const Value: TRegistroE015);
  public
    function New(): TRegistroE015;
    property Items[Index: Integer]: TRegistroE015 read GetItem write SetItem;
  end;

  /// Registro E020 - Saldos Finais das Contas da Parte B do e-Lalur da
  /// ECF Imediatamente Anterior
  TRegistroE020 = class(TBlocos)
  private
    fVL_SALDO_FIN: variant;
    fIND_VL_SALDO_FIN: string;
    fDESC_CTA_LAL: string;
    fDT_LIM_LAL: TDateTime;
    fDT_AP_LAL: TDateTime;
    fDESC_LAN_ORIG: string;
    fCOD_LAN_ORIG: integer;
    fTRIBUTO:   string;
    fCOD_CTA_B: string;
  public
    constructor Create(); virtual; /// Create
    property COD_CTA_B: string read fCOD_CTA_B write fCOD_CTA_B;
    property DESC_CTA_LAL: string read fDESC_CTA_LAL write fDESC_CTA_LAL;
    property DT_AP_LAL: TDateTime read fDT_AP_LAL write fDT_AP_LAL;
    property COD_LAN_ORIG: integer read fCOD_LAN_ORIG write fCOD_LAN_ORIG;
    property DESC_LAN_ORIG: string read fDESC_LAN_ORIG write fDESC_LAN_ORIG;
    property DT_LIM_LAL: TDateTime read fDT_LIM_LAL write fDT_LIM_LAL;
    property TRIBUTO: string read fTRIBUTO write fTRIBUTO;
    property VL_SALDO_FIN: variant read fVL_SALDO_FIN write fVL_SALDO_FIN;
    property IND_VL_SALDO_FIN: string read fIND_VL_SALDO_FIN write fIND_VL_SALDO_FIN;
  end;

  /// Registro E020 - Lista
  TRegistroE020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE020;
    procedure SetItem(Index: Integer; const Value: TRegistroE020);
  public
    function New(): TRegistroE020;
    property Items[Index: Integer]: TRegistroE020 read GetItem write SetItem;
  end;


  /// Registro E030 - IdentificaCão do Período
  TRegistroE030 = class(TBlocos)
  private
    fPER_APUR: string;
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    FRegistroE155: TRegistroE155List;
    FRegistroE355: TRegistroE355List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;
    property RegistroE155: TRegistroE155List read FRegistroE155 write FRegistroE155;
    property RegistroE355: TRegistroE355List read FRegistroE355 write FRegistroE355;
  end;

  /// Registro E030 - Lista
  TRegistroE030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE030;
    procedure SetItem(Index: Integer; const Value: TRegistroE030);
  public
    function New(): TRegistroE030;
    property Items[Index: Integer]: TRegistroE030 read GetItem write SetItem;
  end;


  /// Registro E155 - Detalhes dos Saldos Contábeis Calculados com Base nas ECD
  TRegistroE155 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_FIN: variant;
    fVL_SLD_INI: variant;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
    fIND_VL_SLD_INI: string;
    fVL_CRED:    variant;
    fVL_DEB:     variant;
  public
    constructor Create(); virtual; /// Create
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: variant read fVL_SLD_INI write fVL_SLD_INI;
    property IND_VL_SLD_INI: string read fIND_VL_SLD_INI write fIND_VL_SLD_INI;
    property VL_DEB: variant read fVL_DEB write fVL_DEB;
    property VL_CRED: variant read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  /// Registro E155 - Lista
  TRegistroE155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE155;
    procedure SetItem(Index: Integer; const Value: TRegistroE155);
  public
    function New(): TRegistroE155;
    property Items[Index: Integer]: TRegistroE155 read GetItem write SetItem;
  end;

  /// Registro E355 - Detalhes dos Saldos das Contas de Resultado Antes do Encerramento
  TRegistroE355 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_FIN: variant;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
  public
    constructor Create(); virtual; /// Create
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;


  /// Registro E355 - Lista
  TRegistroE355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE355;
    procedure SetItem(Index: Integer; const Value: TRegistroE355);
  public
    function New(): TRegistroE355;
    property Items[Index: Integer]: TRegistroE355 read GetItem write SetItem;
  end;

  /// Registro E990 - ENCERRAMENTO DO BLOCO E
  TRegistroE990 = class(TCloseBlocos)
  end;

implementation

{ TRegistroE001 }

constructor TRegistroE001.Create;
begin
  inherited Create;
  FRegistroE010 := TRegistroE010List.Create;
  FRegistroE020 := TRegistroE020List.Create;
  FRegistroE030 := TRegistroE030List.Create;
  IND_DAD := idComDados;
end;

destructor TRegistroE001.Destroy;
begin
  FRegistroE010.Free;
  FRegistroE020.Free;
  FRegistroE030.Free;
  inherited;
end;

{ TRegistroE010List }

function TRegistroE010List.GetItem(Index: Integer): TRegistroE010;
begin
  Result := TRegistroE010(Inherited Items[Index]);
end;

function TRegistroE010List.New(): TRegistroE010;
begin
  Result := TRegistroE010.Create();
  Add(Result);
end;

procedure TRegistroE010List.SetItem(Index: Integer;
  const Value: TRegistroE010);
begin
  Put(Index, Value);
end;

{ TRegistroE010 }

constructor TRegistroE010.Create();
begin
  inherited Create;
  FRegistroE015 := TRegistroE015List.Create();
end;

destructor TRegistroE010.Destroy;
begin
  FRegistroE015.Free;
  inherited;
end;

{ TRegistroE015List }

function TRegistroE015List.GetItem(Index: Integer): TRegistroE015;
begin
  Result := TRegistroE015(Inherited Items[Index]);
end;

function TRegistroE015List.New(): TRegistroE015;
begin
  Result := TRegistroE015.Create();
  Add(Result);
end;

procedure TRegistroE015List.SetItem(Index: Integer;
  const Value: TRegistroE015);
begin
  Put(Index, Value);
end;

{ TRegistroE015 }

constructor TRegistroE015.Create();
begin
  inherited Create;
end;

{ TRegistroE020List }

function TRegistroE020List.GetItem(Index: Integer): TRegistroE020;
begin
  Result := TRegistroE020(Inherited Items[Index]);
end;

function TRegistroE020List.New(): TRegistroE020;
begin
  Result := TRegistroE020.Create();
  Add(Result);
end;

procedure TRegistroE020List.SetItem(Index: Integer;
  const Value: TRegistroE020);
begin
  Put(Index, Value);
end;

{ TRegistroE020 }

constructor TRegistroE020.Create();
begin
  inherited Create;
end;

{ TRegistroE030 }

constructor TRegistroE030.Create();
begin
  inherited Create;
  FRegistroE155 := TRegistroE155List.Create;
  FRegistroE355 := TRegistroE355List.Create;
end;

destructor TRegistroE030.Destroy;
begin
  FRegistroE155.Free;
  FRegistroE355.Free;
  inherited;
end;

{ TRegistroE030List }

function TRegistroE030List.GetItem(Index: Integer): TRegistroE030;
begin
  Result := TRegistroE030(Inherited Items[Index]);
end;

function TRegistroE030List.New(): TRegistroE030;
begin
  Result := TRegistroE030.Create();
  Add(Result);
end;

procedure TRegistroE030List.SetItem(Index: Integer;
  const Value: TRegistroE030);
begin
  Put(Index, Value);
end;

{ TRegistroE155 }

constructor TRegistroE155.Create();
begin
  inherited Create;
end;

{ TRegistroE355 }

constructor TRegistroE355.Create();
begin
  inherited Create;
end;

{ TRegistroE155List }

function TRegistroE155List.GetItem(Index: Integer): TRegistroE155;
begin
  Result := TRegistroE155(Inherited Items[Index]);
end;

function TRegistroE155List.New(): TRegistroE155;
begin
  Result := TRegistroE155.Create();
  Add(Result);
end;

procedure TRegistroE155List.SetItem(Index: Integer;
  const Value: TRegistroE155);
begin
  Put(Index, Value);
end;

{ TRegistroE355List }

function TRegistroE355List.GetItem(Index: Integer): TRegistroE355;
begin
  Result := TRegistroE355(Inherited Items[Index])
end;

function TRegistroE355List.New(): TRegistroE355;
begin
  Result := TRegistroE355.Create();
  Add(Result);
end;

procedure TRegistroE355List.SetItem(Index: Integer;
  const Value: TRegistroE355);
begin
  Put(Index, Value);
end;

end.
