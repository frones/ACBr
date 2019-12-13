{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                       Isaque Pinheiro                        }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBloco_H;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistroH005List = class;
  TRegistroH010List = class;
  TRegistroH020List = class;
  TRegistroH030List = class;

  /// Registro H001 - ABERTURA DO BLOCO H

  TRegistroH001 = class(TOpenBlocos)
  private
    FRegistroH005: TRegistroH005List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroH005: TRegistroH005List read FRegistroH005 write FRegistroH005;
  end;

  /// Registro H005 - TOTAIS DO INVENTÁRIO

  TRegistroH005 = class
  private
    fDT_INV: TDateTime;    /// Data do inventário:
    fVL_INV: currency;     /// Valor total do estoque:
    fMOT_INV: TACBrMotInv; /// 01 – No final no período;
                           /// 02 – Na mudança de forma de tributação da mercadoria (ICMS);
                           /// 03 – Na solicitação da baixa cadastral, paralisação temporária e outras situações;
                           /// 04 – Na alteração de regime de pagamento – condição do contribuinte;
                           /// 05 – Por determinação dos fiscos.

    FRegistroH010: TRegistroH010List;  /// BLOCO H - Lista de RegistroH010 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INV: TDateTime read FDT_INV write FDT_INV;
    property VL_INV: currency read FVL_INV write FVL_INV;
    property MOT_INV: TACBrMotInv read fMOT_INV write fMOT_INV;

    /// Registros FILHOS
    property RegistroH010: TRegistroH010List read FRegistroH010 write FRegistroH010;
  end;

  /// Registro H005 - Lista

  TRegistroH005List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH005; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroH005); /// SetItem
  public
    function New(): TRegistroH005;
    property Items[Index: Integer]: TRegistroH005 read GetItem write SetItem;
  end;

  /// Registro H010 - INVENTÁRIO

  { TRegistroH010 }

  TRegistroH010 = class
  private
    fCOD_ITEM: String;       /// Código do item (campo 02 do Registro 0200)
    fUNID: String;           /// Unidade do item
    fQTD: Double;            /// Quantidade do item
    fVL_UNIT: Double;        /// Valor unitário do item
    fVL_ITEM: currency;      /// Valor do item
    fIND_PROP: TACBrIndProp; /// Indicador de propriedade/posse do item: 0- Item de propriedade do informante e em seu poder, 1- Item de propriedade do informante em posse de terceiros, 2- Item de propriedade de terceiros em posse do informante
    fCOD_PART: String;       /// Código do participante (campo 02 do Registro 0150): proprietário/possuidor que não seja o informante do arquivo
    fTXT_COMPL: String;      /// Descrição complementar
    fCOD_CTA: String;        /// Código da conta analítica contábil debitada/creditada
    fVL_ITEM_IR: Double;       /// Valor do item para efeitos do Imposto de Renda.

    FRegistroH020: TRegistroH020List;  /// BLOCO H - Lista de RegistroH020 (FILHO)
    FRegistroH030: TRegistroH030List;  /// BLOCO H - Lista de RegistroH030 (FILHO)

  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property QTD: Double read FQTD write FQTD;
    property VL_UNIT: Double read FVL_UNIT write FVL_UNIT;
    property VL_ITEM: currency read FVL_ITEM write FVL_ITEM;
    property IND_PROP: TACBrIndProp read FIND_PROP write FIND_PROP;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property TXT_COMPL: String read FTXT_COMPL write FTXT_COMPL;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
    property VL_ITEM_IR : Double read fVL_ITEM_IR write fVL_ITEM_IR;
    /// Registros FILHOS
    property RegistroH020: TRegistroH020List read FRegistroH020 write FRegistroH020;
    property RegistroH030: TRegistroH030List read FRegistroH030 write FRegistroH030;
  end;

  /// Registro H010 - Lista

  TRegistroH010List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH010; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroH010); /// SetItem
  public
    function LocalizaRegistro(const pCOD_ITEM: String): boolean;
    function New(): TRegistroH010;
    property Items[Index: Integer]: TRegistroH010 read GetItem write SetItem;
  end;

  /// Registro H020 - INFORMAÇÃO COMPLEMENTAR DO INVENTÁRIO

  TRegistroH020 = class
  private
    fCST_ICMS: String;          /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1
    fBC_ICMS: currency;         /// Informe a base de cálculo do ICMS
    fVL_ICMS: currency;         /// Informe o valor do ICMS a ser debitado ou creditado
  public
    constructor Create(AOwner: TRegistroH010); virtual; /// Create

    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property BC_ICMS: currency read FBC_ICMS write FBC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
  end;

  /// Registro H020 - Lista

  TRegistroH020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH020; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroH020); /// SetItem
  public
    function New(AOwner: TRegistroH010): TRegistroH020;
    property Items[Index: Integer]: TRegistroH020 read GetItem write SetItem;
  end;

  ///REGISTRO H030: INFORMAÇÕES COMPLEMENTARES DO INVENTÁRIO DAS MERCADORIAS SUJEITAS AO REGIME DE SUBSTITUIÇÃO TRIBUTÁRIA.

  TRegistroH030 = class
  private
    fVL_ICMS_OP: currency; /// Valor médio unitário do ICMS OP.
    fVL_BC_ICMS_ST: currency; /// Valor médio unitário da base de cálculo do ICMS ST.
    fVL_ICMS_ST: currency; /// Valor médio unitário do ICMS ST.
    fVL_FCP: currency; /// Valor médio unitário do FCP.
  public
    constructor Create(AOwner: TRegistroH010); virtual; /// Create

    property VL_ICMS_OP: currency read fVL_ICMS_OP write fVL_ICMS_OP;
    property VL_BC_ICMS_ST: currency read fVL_BC_ICMS_ST write fVL_BC_ICMS_ST;
    property VL_ICMS_ST: currency read fVL_ICMS_ST write fVL_ICMS_ST;
    property VL_FCP: currency read fVL_FCP write fVL_FCP;
  end;

  /// Registro H030 - Lista

  TRegistroH030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH030; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroH030); /// SetItem
  public
    function New(AOwner: TRegistroH010): TRegistroH030;
    property Items[Index: Integer]: TRegistroH030 read GetItem write SetItem;
  end;

  /// Registro H990 - ENCERRAMENTO DO BLOCO H

  TRegistroH990 = class
  private
    fQTD_LIN_H: Integer;    /// Quantidade total de linhas do Bloco H
  public
    property QTD_LIN_H: Integer read FQTD_LIN_H write FQTD_LIN_H;
  end;

implementation

{ TRegistroH010List }

function TRegistroH010List.GetItem(Index: Integer): TRegistroH010;
begin
  Result := TRegistroH010(Inherited Items[Index]);
end;

function TRegistroH010List.LocalizaRegistro(const pCOD_ITEM: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_ITEM = pCOD_ITEM then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroH010List.New(): TRegistroH010;
begin
  Result := TRegistroH010.Create();
  Add(Result);
end;

procedure TRegistroH010List.SetItem(Index: Integer; const Value: TRegistroH010);
begin
  Put(Index, Value);
end;

{ TRegistroH005List }

function TRegistroH005List.GetItem(Index: Integer): TRegistroH005;
begin
  Result := TRegistroH005(Inherited Items[Index]);
end;

function TRegistroH005List.New(): TRegistroH005;
begin
  Result := TRegistroH005.Create();
  Add(Result);
end;

procedure TRegistroH005List.SetItem(Index: Integer; const Value: TRegistroH005);
begin
  Put(Index, Value);
end;

{ TRegistroH005 }

constructor TRegistroH005.Create();
begin
  inherited Create;
  FRegistroH010 := TRegistroH010List.Create;
end;

destructor TRegistroH005.Destroy;
begin
  FRegistroH010.Free;
  inherited;
end;

{ TRegistroH001 }

constructor TRegistroH001.Create;
begin
   inherited Create;
   FRegistroH005 := TRegistroH005List.Create;
   //
   IND_MOV := imSemDados;
end;

destructor TRegistroH001.Destroy;
begin
  FRegistroH005.Free;
  inherited;
end;

{ TRegistroH020List }

function TRegistroH020List.GetItem(Index: Integer): TRegistroH020;
begin
  Result := TRegistroH020(Inherited Items[Index]);
end;

function TRegistroH020List.New(AOwner: TRegistroH010): TRegistroH020;
begin
  Result := TRegistroH020.Create(AOwner);
  Add(Result);
end;

procedure TRegistroH020List.SetItem(Index: Integer;
  const Value: TRegistroH020);
begin
  Put(Index, Value);
end;

{ TRegistroH010 }

constructor TRegistroH010.Create();
begin
  inherited Create;
  FRegistroH020 := TRegistroH020List.Create;
  FRegistroH030 := TRegistroH030List.Create;
end;

destructor TRegistroH010.Destroy;
begin
  FRegistroH020.Free;
  FRegistroH030.Free;
  inherited;
end;

{ TRegistroH020 }

constructor TRegistroH020.Create(AOwner: TRegistroH010);
begin
end;

{ TRegistroH030 }

constructor TRegistroH030.Create(AOwner: TRegistroH010);
begin
end;

{ TRegistroH030List }

function TRegistroH030List.GetItem(Index: Integer): TRegistroH030;
begin
   Result := TRegistroH030(Inherited Items[Index]);
end;

function TRegistroH030List.New(AOwner: TRegistroH010): TRegistroH030;
begin
  Result := TRegistroH030.Create(AOwner);
  Add(Result);
end;

procedure TRegistroH030List.SetItem(Index: Integer; const Value: TRegistroH030);
begin
  Put(Index, Value);
end;

end.
