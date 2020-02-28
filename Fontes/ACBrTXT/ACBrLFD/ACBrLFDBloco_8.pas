{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio 								   }
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

unit ACBrLFDBloco_8;

interface

uses
  SysUtils, Classes, DateUtils, ACBrLFDBlocos;

type

  TRegistro8020 = class;
  TRegistro8025List = class;
  TRegistro8030List = class;

  /// Registro 8001 - ABERTURA DO BLOCO 8 - DADOS DO DISTRITO FEDERAL

  { TRegistro8001 }

  TRegistro8001 = class(TOpenBlocos)
  private
    FRegistro8020: TRegistro8020;
    FUF: String;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property UF: String read FUF write FUF;
    property Registro8020: TRegistro8020 read FRegistro8020 write FRegistro8020;
  end;

  /// Registro 8020 - MAPA DE MOVIMENTAÇÃO DE COMBUSTÍVEIS

  { TRegistro8020 }

  TRegistro8020 = class
  private
    FIND_DAD: TACBrLIndicadorMovimento; // Indicador de conteúdo
    FDT_INI: TDateTime; /// Data inicial a que o mapa se refere
    FDT_FIN: TDateTime; /// Data final a que o mapa se refere
    FCOMB: Integer; /// Tipo de combustível

    FRegistro8025: TRegistro8025List;
  public
    constructor Create(AOwner: TRegistro8001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_DAD: TACBrLIndicadorMovimento read FIND_DAD write FIND_DAD;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property COMB: Integer read FCOMB write FCOMB;

    property Registro8025: TRegistro8025List read FRegistro8025 write FRegistro8025;
  end;

  /// Registro 8020 - Lista

  { TRegistro8020List }

  TRegistro8020List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistro8020;
    procedure SetItem(Index: Integer; const Value: TRegistro8020);
  public
    function New(AOwner: TRegistro8001): TRegistro8020;
    property Items[Index: Integer]: TRegistro8020 read GetItem write SetItem;
  end;

  /// Registro 8025 - Tanque

  { TRegistro8025 }

  TRegistro8025 = class
  private
    FTANQ: Integer; /// Número identificação do tanque
    FEST_INI: Double; /// Quantidade inicial do estoque no período (em litros)
    FEST_FIN: Double; ///Quantidade final do estoque no período (em litros)
    FENTR: Double; /// Entradas no período (em litros)
    FSAID: Double; /// Saídas no período (em litros)
    FIND_ALT: TACBrIndAlteracao; /// Indicador de alteração no período
    FPER_ALT: Double; /// Percentual de alteração no período

    FRegistro8030: TRegistro8030List;
  public
    constructor Create(AOwner: TRegistro8020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property TANQ: Integer read FTANQ write FTANQ;
    property EST_INI: Double read FEST_INI write FEST_INI;
    property EST_FIN: Double read FEST_FIN write FEST_FIN;
    property ENTR: Double read FENTR write FENTR;
    property SAID: Double read FSAID write FSAID;
    property IND_ALT: TACBrIndAlteracao read FIND_ALT write FIND_ALT;
    property PER_ALT: Double read FPER_ALT write FPER_ALT;

    property Registro8030: TRegistro8030List read FRegistro8030 write FRegistro8030;
  end;

  /// Registro 8025 - Lista

  { TRegistro8025List }

  TRegistro8025List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistro8025;
    procedure SetItem(Index: Integer; const Value: TRegistro8025);
  public
    function New(AOwner: TRegistro8020): TRegistro8025;
    property Items[Index: Integer]: TRegistro8025 read GetItem write SetItem;
  end;

  /// Registro 8030 - Bico

  { TRegistro8030 }

  TRegistro8030 = class
  private
    FBICO: Integer; /// Número identificação do bico
    FNUM_INI: Integer; /// Número inicial do encerrante no período
    FNUM_FIN: Integer; /// Número final do encerrante no período
  public
    constructor Create(AOwner: TRegistro8025); virtual; /// Create

    property BICO: Integer read FBICO write FBICO;
    property NUM_INI: Integer read FNUM_INI write FNUM_INI;
    property NUM_FIN: Integer read FNUM_FIN write FNUM_FIN;
  end;

  /// Registro 8030 - Lista

  { TRegistro8030List }

  TRegistro8030List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistro8030;
    procedure SetItem(Index: Integer; const Value: TRegistro8030);
  public
    function New(AOwner: TRegistro8025): TRegistro8030;
    property Items[Index: Integer]: TRegistro8030 read GetItem write SetItem;
  end;

  /// Registro 8990 - Registros do Distrito Federal - Encerramento

  { TRegistro8990 }

  TRegistro8990 = class
  private
    fQTD_LIN_8: Integer;
  public
    property QTD_LIN_8: Integer read fQTD_LIN_8 write fQTD_LIN_8;
  end;

implementation

{ TRegistro8001 }

constructor TRegistro8001.Create;
begin
  FRegistro8020 := TRegistro8020.Create(Self);

  IND_MOV := imlSemDados;
end;

destructor TRegistro8001.Destroy;
begin
  FRegistro8020.Free;
  inherited;
end;

{ TRegistro8020 }

constructor TRegistro8020.Create(AOwner: TRegistro8001);
begin
  FRegistro8025 := TRegistro8025List.Create;

  IND_DAD := imlSemDados;
end;

destructor TRegistro8020.Destroy;
begin
  FRegistro8025.Free;
  inherited;
end;

{ TRegistro8020List }

function TRegistro8020List.GetItem(Index: Integer): TRegistro8020;
begin
  Result := TRegistro8020(Get(Index));
end;

function TRegistro8020List.New(AOwner: TRegistro8001): TRegistro8020;
begin
  Result := TRegistro8020.Create(AOwner);
  Add(Result);
end;

procedure TRegistro8020List.SetItem(Index: Integer; const Value: TRegistro8020);
begin
  Put(Index, Value);
end;

{ TRegistro8025 }

constructor TRegistro8025.Create(AOwner: TRegistro8020);
begin
  FRegistro8030 := TRegistro8030List.Create;
end;

destructor TRegistro8025.Destroy;
begin
  FRegistro8030.Free;
  inherited;
end;

{ TRegistro8025List }

function TRegistro8025List.GetItem(Index: Integer): TRegistro8025;
begin
  Result := TRegistro8025(Get(Index));
end;

function TRegistro8025List.New(AOwner: TRegistro8020): TRegistro8025;
begin
  Result := TRegistro8025.Create(AOwner);
  Add(Result);
end;

procedure TRegistro8025List.SetItem(Index: Integer; const Value: TRegistro8025);
begin
  Put(Index, Value);
end;

{ TRegistro8030 }

constructor TRegistro8030.Create(AOwner: TRegistro8025);
begin
end;

{ TRegistro8030List }

function TRegistro8030List.GetItem(Index: Integer): TRegistro8030;
begin
  Result := TRegistro8030(Get(Index));
end;

function TRegistro8030List.New(AOwner: TRegistro8025): TRegistro8030;
begin
  Result := TRegistro8030.Create(AOwner);
  Add(Result);
end;

procedure TRegistro8030List.SetItem(Index: Integer; const Value: TRegistro8030);
begin
  Put(Index, Value);
end;

end.
