{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio								   }	  
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

unit ACBrLFDBloco_H;

interface

uses
  SysUtils, Classes, DateUtils, ACBrLFDBlocos;

type

  TRegistroH020 = class;
  TRegistroH030List = class;
  TRegistroH040List = class;
  TRegistroH050List = class;
  TRegistroH060List = class;

  /// Registro H001 - Abertura do Bloco H

  { TRegistroH001 }

  TRegistroH001 = class(TOpenBlocos)
  private
    FRegistroH020: TRegistroH020;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroH020: TRegistroH020 read FRegistroH020 write FRegistroH020;
  end;

  /// Registro H020 - Totais do Inventário

  { TRegistroH020 }

  TRegistroH020 = class
  private
    FDT_INV: TDateTime;
    FVL_ESTQ: Currency;
  public
    constructor Create(AOwner: TRegistroH001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INV: TDatetime read FDT_INV write FDT_INV;
    property VL_ESTQ: Currency read FVL_ESTQ write FVL_ESTQ;
  end;

  /// Registro H030 - Inventário

  { TRegistroH030 }

  TRegistroH030 = class
  private
    FIND_POSSE: TACBrlPosseItem; /// Indicador de propriedade/posse do item
    FCOD_PART: String; /// Código do participante do proprietário ou possuidor que não seja o informante do arquivo
    FIND_ITEM: TACBrTipoItem; /// Indicador do tipo de item inventeriado
    FCOD_NCM: String; /// Código da Nomenclatura Comum do Mercosul
    FCOD_ITEM: String; /// Código do item (campo 02 da Linha 0200)
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_ITEM: Double; /// Valor líquido do item
    FVL_ICMS_REC: Currency; /// Valor do ICMS a recuperar
    FVL_IPI_REC: Currency; /// Valor do IPI a recuperar
    FVL_PIS_REC: Currency; /// Valor do PIS a recuperar
    FVL_COFINS_REC: Currency; /// Valor da COFINS a recuperar
    FVL_TRIB_NC: Currency; /// Valor dos tributos não-cumulativos recuperáveis
    FCOD_INF_OBS: Currency; /// Código de referência à observação
  public
    constructor Create(AOwner: TRegistroH020); virtual; /// Create

    property IND_POSSE: TACBrlPosseItem read FIND_POSSE write FIND_POSSE;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property IND_ITEM: TACBrTipoItem read FIND_ITEM write FIND_ITEM;
    property COD_NCM: String read FCOD_NCM write FCOD_NCM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_ITEM: Double read FVL_ITEM write FVL_ITEM;
    property VL_ICMS_REC: Currency read FVL_ICMS_REC write FVL_ICMS_REC;
    property VL_IPI_REC: Currency read FVL_IPI_REC write FVL_IPI_REC;
    property VL_PIS_REC: Currency read FVL_PIS_REC write FVL_PIS_REC;
    property VL_COFINS_REC: Currency read FVL_COFINS_REC write FVL_COFINS_REC;
    property VL_TRIB_NC: Currency read FVL_TRIB_NC write FVL_TRIB_NC;
    property COD_INF_OBS: Currency read FCOD_INF_OBS write FCOD_INF_OBS;
  end;

  /// Registro H030 - Lista

  { TRegistroH030List }

  TRegistroH030List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroH030;
    procedure SetItem(Index: Integer; const Value: TRegistroH030);
  public
    function New(AOwner: TRegistroH020): TRegistroH030;
    property Items[Index: Integer]: TRegistroH030 read GetItem write SetItem;
  end;

  /// Registro H040 - SUBTOTAIS POR POSSUIDOR/PROPRIETÁRIO

  { TRegistroH040 }

  TRegistroH040 = class
  private
    FIND_POSSE: TACBrlPosseItem; /// Indicador de posse a ser totalizado
    FVL_SUB_POSSE: Currency; /// Valor subtotal por possuidor ou proprietário
  public
    constructor Create(AOwner: TRegistroH020); virtual; /// Create

    property IND_POSSE: TACBrlPosseItem read FIND_POSSE write FIND_POSSE;
    property VL_SUB_POSSE: Currency read FVL_SUB_POSSE write FVL_SUB_POSSE;
  end;

  /// Registro H040 - Lista

  { TRegistroH040List }

  TRegistroH040List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroH040;
    procedure SetItem(Index: Integer; const Value: TRegistroH040);
  public
    function New(AOwner: TRegistroH020): TRegistroH040;
    property Items[Index: Integer]: TRegistroH040 read GetItem write SetItem;
  end;

  /// Registro H050 - SUBTOTAIS POR TIPO DE ITEM

  { TRegistroH050 }

  TRegistroH050 = class
  private
    FIND_ITEM: TACBrTipoProduto; /// Indicador do tipo de item a ser totalizado
    FVL_SUB_ITEM: Currency; /// Valor subtotal por tipo de item
  public
    constructor Create(AOwner: TRegistroH020); virtual; /// Create

    property IND_ITEM: TACBrTipoProduto read FIND_ITEM write FIND_ITEM;
    property VL_SUB_ITEM: Currency read FVL_SUB_ITEM write FVL_SUB_ITEM;
  end;

  /// Registro H050 - Lista

  { TRegistroH050List }

  TRegistroH050List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroH050;
    procedure SetItem(Index: Integer; const Value: TRegistroH050);
  public
    function New(AOwner: TRegistroH020): TRegistroH050;
    property Items[Index: Integer]: TRegistroH050 read GetItem write SetItem;
  end;

  /// Registro H060 - SUBTOTAIS POR NCM

  { TRegistroH060 }

  TRegistroH060 = class
  private
    FCOD_NCM: String; /// Código da Nomenclatura Comum do Mercosul
    FVL_SUB_NCM: Currency; /// Valor subtotal por NCM
  public
    constructor Create(AOwner: TRegistroH020); virtual; /// Create

    property COD_NCM: String read FCOD_NCM write FCOD_NCM;
    property VL_SUB_NCM: Currency read FVL_SUB_NCM write FVL_SUB_NCM;
  end;

  /// Registro H060 - Lista

  { TRegistroH060List }

  TRegistroH060List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroH060;
    procedure SetItem(Index: Integer; const Value: TRegistroH060);
  public
    function New(AOwner: TRegistroH020): TRegistroH060;
    property Items[Index: Integer]: TRegistroH060 read GetItem write SetItem;
  end;

  /// Registro H990 - ENCERRAMENTO DO BLOCO H

  { TRegistroH990 }

  TRegistroH990 = class
  private
    fQTD_LIN_H: Integer;
  public
    property QTD_LIN_H: Integer read fQTD_LIN_H write fQTD_LIN_H;
  end;

implementation

{ TRegistroH001 }

constructor TRegistroH001.Create;
begin
  FRegistroH020 := TRegistroH020.Create(Self);
  IND_MOV := imlSemDados;
end;

destructor TRegistroH001.Destroy;
begin
  FRegistroH020.Free;
  inherited;
end;

{ TRegistroH020 }

constructor TRegistroH020.Create(AOwner: TRegistroH001);
begin
end;

destructor TRegistroH020.Destroy;
begin
  inherited;
end;

{ TRegistroH030 }

constructor TRegistroH030.Create(AOwner: TRegistroH020);
begin
end;

{ TRegistroH030List }

function TRegistroH030List.GetItem(Index: Integer): TRegistroH030;
begin
  Result := TRegistroH030(Get(Index));
end;

function TRegistroH030List.New(AOwner: TRegistroH020): TRegistroH030;
begin
  Result := TRegistroH030.Create(AOwner);
  Add(Result);
end;

procedure TRegistroH030List.SetItem(Index: Integer; const Value: TRegistroH030);
begin
  Put(Index, Value);
end;

{ TRegistroH040 }

constructor TRegistroH040.Create(AOwner: TRegistroH020);
begin
end;

{ TRegistroH040List }

function TRegistroH040List.GetItem(Index: Integer): TRegistroH040;
begin
  Result := TRegistroH040(Get(Index));
end;

function TRegistroH040List.New(AOwner: TRegistroH020): TRegistroH040;
begin
  Result := TRegistroH040.Create(AOwner);
end;

procedure TRegistroH040List.SetItem(Index: Integer; const Value: TRegistroH040);
begin
  Put(Index, Value);
end;

{ TRegistroH050 }

constructor TRegistroH050.Create(AOwner: TRegistroH020);
begin

end;

{ TRegistroH050List }

function TRegistroH050List.GetItem(Index: Integer): TRegistroH050;
begin
  Result := TRegistroH050(Get(Index));
end;

function TRegistroH050List.New(AOwner: TRegistroH020): TRegistroH050;
begin
  Result := TRegistroH050.Create(AOwner);
  Add(Result);
end;

procedure TRegistroH050List.SetItem(Index: Integer; const Value: TRegistroH050);
begin
  Put(Index, Value);
end;

{ TRegistroH060 }

constructor TRegistroH060.Create(AOwner: TRegistroH020);
begin
end;

{ TRegistroH060List }

function TRegistroH060List.GetItem(Index: Integer): TRegistroH060;
begin
  Result := TRegistroH060(Get(Index));
end;

function TRegistroH060List.New(AOwner: TRegistroH020): TRegistroH060;
begin
  Result := TRegistroH060.Create(AOwner);
  Add(Result);
end;

procedure TRegistroH060List.SetItem(Index: Integer; const Value: TRegistroH060);
begin
  Put(Index, Value)
end;

end.
