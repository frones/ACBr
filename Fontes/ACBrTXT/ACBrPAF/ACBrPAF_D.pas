{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAF_D;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  TRegistroD3List = class;
  TRegistroD4List = class;

  /// REGISTRO TIPO D1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroD1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO D2 - RELAÇÃO DOS DAV EMITIDOS:

  TRegistroD2 = class
  private
    fNUM_FAB: string;      /// Nº de fabricação do ECF
    fMF_ADICIONAL: string; /// Letra indicativa de MF adicional
    fTIPO_ECF: string;     /// Tipo do ECF
    fMARCA_ECF: string;    /// Marca do ECF
    fMODELO_ECF: string;   /// Modelo do ECF
    fCOO: string;          /// Contador de Ordem de Operação do documento onde o DAV foi impresso pelo ECF
    fNUM_DAV: string;      /// Número do DAV emitido
    fDT_DAV: TDateTime;    /// Data de emissão do DAV
    fTIT_DAV: string;      /// Título atribuído ao DAV de acordo com sua função. Ex: Orçamento, Pedido, etc.
    fVLT_DAV: currency;    /// Valor total do DAV emitido, com duas casas decimais
    fCOO_DFV: string;      /// Contador de Ordem de Operação do Documento Fiscal Vinculado
    /// Versão 01.07
    fNUMERO_ECF: string;   /// Número sequencial do ECF emissor do documento fiscal vinculado
    fNOME_CLIENTE: string; /// Nome do Cliente
    fCPF_CNPJ: string;     /// CPF ou CNPJ do adquirente

    fRegistroValido: boolean;

    fRegistroD3: TRegistroD3List; /// Registro FILHO
    FRegistroD4: TRegistroD4List; /// Lista de FRegistroD4
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_FAB: string read FNUM_FAB write FNUM_FAB;
    property MF_ADICIONAL: string read FMF_ADICIONAL write FMF_ADICIONAL;
    property TIPO_ECF: string read FTIPO_ECF write FTIPO_ECF;
    property MARCA_ECF: string read FMARCA_ECF write FMARCA_ECF;
    property MODELO_ECF: string read FMODELO_ECF write FMODELO_ECF;
    property COO: string read FCOO write FCOO;
    property NUM_DAV: string read FNUM_DAV write FNUM_DAV;
    property DT_DAV: TDateTime read FDT_DAV write FDT_DAV;
    property TIT_DAV: string read FTIT_DAV write FTIT_DAV;
    property VLT_DAV: currency read FVLT_DAV write FVLT_DAV;
    property COO_DFV: string read FCOO_DFV write FCOO_DFV;
    property NUMERO_ECF: string read FNUMERO_ECF write FNUMERO_ECF;
    property NOME_CLIENTE: string read FNOME_CLIENTE write FNOME_CLIENTE;
    property CPF_CNPJ: string read FCPF_CNPJ write FCPF_CNPJ;

    property RegistroD3: TRegistroD3List read FRegistroD3 write FRegistroD3;
    property RegistroD4: TRegistroD4List read FRegistroD4 write FRegistroD4;
  end;

  /// REGISTRO D2 - Lista

  TRegistroD2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD2;
    procedure SetItem(Index: Integer; const Value: TRegistroD2);
  public
    function New: TRegistroD2;
    property Items[Index: Integer]: TRegistroD2 read GetItem write SetItem;
  end;

  TRegistroD_Base = class
  private
    fCOD_ITEM: string;
    fVL_TOTAL: Currency;
    fDESC_ITEM: string;
    fIND_CANC: string;
    fVL_ACRES: Currency;
    fUNI_ITEM: string;
    fVL_DESCTO: Currency;
    fVL_UNIT: Currency;
    fDEC_VL_UNIT: Integer;
    fDEC_QTDE_ITEM: Integer;
    fQTDE_ITEM: Currency;
    fALIQ: Currency;
    fSIT_TRIB: string;
  public
    property COD_ITEM: string read fCOD_ITEM write fCOD_ITEM;
    property DESC_ITEM: string read fDESC_ITEM write fDESC_ITEM;
    property QTDE_ITEM: Currency read fQTDE_ITEM write fQTDE_ITEM;
    property UNI_ITEM: string read fUNI_ITEM write fUNI_ITEM;
    property VL_UNIT: Currency read fVL_UNIT write fVL_UNIT;
    property VL_DESCTO: Currency read fVL_DESCTO write fVL_DESCTO;
    property VL_ACRES: Currency read fVL_ACRES write fVL_ACRES;
    property VL_TOTAL: Currency read fVL_TOTAL write fVL_TOTAL;
    property SIT_TRIB : string read fSIT_TRIB write fSIT_TRIB;
    property ALIQ : Currency read fALIQ write fALIQ;
    property IND_CANC: string read fIND_CANC write fIND_CANC;
    property DEC_QTDE_ITEM: Integer read fDEC_QTDE_ITEM write fDEC_QTDE_ITEM;
    property DEC_VL_UNIT: Integer read fDEC_VL_UNIT write fDEC_VL_UNIT;
  end;

  TRegistroD3 = class(TRegistroD_Base)
  private
    fRegistroValido: Boolean;
    fDT_INCLUSAO: TDateTime;
    fNUM_ITEM: Integer;
  public
    constructor Create; virtual;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property DT_INCLUSAO: TDateTime read fDT_INCLUSAO write fDT_INCLUSAO;
    property NUM_ITEM: Integer read fNUM_ITEM write fNUM_ITEM;
  end;

  /// REGISTRO D3 - Lista

  TRegistroD3List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD3;
    procedure SetItem(Index: Integer; const Value: TRegistroD3);
  public
    function New: TRegistroD3;
    property Items[Index: Integer]: TRegistroD3 read GetItem write SetItem;
  end;

  //Registro D4 - Log de alterações do DAV
  TRegistroD4 = class(TRegistroD_Base)
  private
    fRegistroValido: Boolean;
    FNUM_DAV: string;      //Número do DAV
    FDT_ALT: TDateTime;    //Data e Hora da alteração
    FTIP_ALT: String;      //Tipo Alteração ("A" para Alteração, "E" para Exclusão, "I" para Inclusão
  public
    constructor Create; virtual;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_DAV: string   read FNUM_DAV write FNUM_DAV;
    property DT_ALT: TDatetime read FDT_ALT  write FDT_ALT;
    property TIP_ALT: String   read FTIP_ALT write FTIP_ALT;
  end;

  TRegistroD4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD4;
    procedure SetItem(Index: Integer; const Value: TRegistroD4);
  public
    function New: TRegistroD4;
    property Items[Index: Integer]: TRegistroD4 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO D9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroD9 = class(TRegistroX9)
  private
    FTOT_REG_D2: integer;  /// Total de registros
    FTOT_REG_D3: integer;  /// Total de registros
  public
    property TOT_REG_D2: integer read FTOT_REG_D2 write FTOT_REG_D2;
    property TOT_REG_D3: integer read FTOT_REG_D3 write FTOT_REG_D3;
  end;

implementation

(* TRegistroD2List *)

function TRegistroD2List.GetItem(Index: Integer): TRegistroD2;
begin
  Result := TRegistroD2(inherited Items[Index]);
end;

function TRegistroD2List.New: TRegistroD2;
begin
  Result := TRegistroD2.Create;
  Add(Result);
end;

procedure TRegistroD2List.SetItem(Index: Integer; const Value: TRegistroD2);
begin
  Put(Index, Value);
end;

{ TRegistroD2 }

constructor TRegistroD2.Create;
begin
  fRegistroD3 := TRegistroD3List.Create;
  FRegistroD4  := TRegistroD4List.Create;
  fRegistroValido := True;
end;

destructor TRegistroD2.Destroy;
begin
  fRegistroD3.Free;
  FRegistroD4.Free;
  inherited;
end;

{ TRegistroD3 }

constructor TRegistroD3.Create;
begin
   inherited Create;
   fRegistroValido := True;
end;

{ TRegistroD3List }

function TRegistroD3List.GetItem(Index: Integer): TRegistroD3;
begin
  Result := TRegistroD3(inherited Items[Index]);
end;

function TRegistroD3List.New: TRegistroD3;
begin
  Result := TRegistroD3.Create;
  Add(Result);
end;

procedure TRegistroD3List.SetItem(Index: Integer; const Value: TRegistroD3);
begin
  Put(Index, Value);
end;

{ TRegistroD4List }  
function TRegistroD4List.GetItem(Index: Integer): TRegistroD4;
begin
  Result := TRegistroD4(inherited Items[Index]);
end;

function TRegistroD4List.New: TRegistroD4;
begin
  Result := TRegistroD4.Create;
  Add(Result);
end;

procedure TRegistroD4List.SetItem(Index: Integer;
  const Value: TRegistroD4);
begin
  Put(Index, Value);
end;

{ TRegistroD4 }

constructor TRegistroD4.Create;
begin
   inherited Create;
  fRegistroValido := True;
end;

end.
