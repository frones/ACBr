{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Pedro R Costa                              }
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

unit ACBrDeSTDABloco_G;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrDeSTDABlocos, ACBrUtil;

  /// BLOCO G: INFORMAÇÕES ECONÔMICO-FISCAIS

type
  TRegistroG001     = class;
  TRegistroG020     = class;
  TRegistroG600     = class;
  TRegistroG605     = class;
  TRegistroG605List = class;
  TRegistroG610     = class;
//  TRegistroG610List = class;
  TRegistroG615     = class;
  TRegistroG615List = class;
  TRegistroG620     = class;
  TRegistroG620List = class;
  TRegistroG625     = class;
  TRegistroG625List = class;
  TRegistroG990     = class;

  /// REGISTRO G001: ABERTURA DO BLOCO G

  TRegistroG001 = class(TOpenBlocos)
  private
    FRegistroG020: TRegistroG020;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroG020: TRegistroG020 read FRegistroG020 write FRegistroG020;
  end;

  /// REGISTRO G020: GUIA DE INFORMAÇÕES ECONÔMICO-FISCAIS

  TRegistroG020 = class
  private
    FIND_GEF     : TACBrIndContGuias; /// Indicador de conteúdo
    FDT_INI      : TDateTime;         /// Data inicial a que a guia se refere
    FDT_FIN      : TDateTime;         /// Data final a que a guia se refere
    FRegistroG600: TRegistroG600;     ///
    FRegistroG610: TRegistroG610;     ///
    FRegistroG620: TRegistroG620List; ///
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroG600: TRegistroG600     read FRegistroG600 write FRegistroG600;
    property RegistroG610: TRegistroG610     read FRegistroG610 write FRegistroG610;
    property RegistroG620: TRegistroG620List read FRegistroG620 write FRegistroG620;
    property IND_GEF     : TACBrIndContGuias read FIND_GEF      write FIND_GEF;
    property DT_INI      : TDateTime         read FDT_INI       write FDT_INI;
    property DT_FIN      : TDateTime         read FDT_FIN       write FDT_FIN;
  end;

  /// REGISTRO G600: TOTALIZADORES ICMS ANTECIPAÇÃO E DIFERENCIAL DE ALÍQUOTAS ENTRADAS

  TRegistroG600 = class
  private
    FVL_TOT_NF       : Variant; /// Valor total das NFs
    FVL_TOT_AJ       : Variant; /// Valor total dos Ajustes
    FVL_TOT_DA       : Variant; ///Valor total a declarar

    FRegistroG605List: TRegistroG605List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_TOT_NF   : Variant           read FVL_TOT_NF        write FVL_TOT_NF;
    property VL_TOT_AJ   : Variant           read FVL_TOT_AJ        write FVL_TOT_AJ;
    property VL_TOT_DA   : Variant           read FVL_TOT_DA        write FVL_TOT_DA;
    property RegistroG605: TRegistroG605List read FRegistroG605List write FRegistroG605List;
  end;

  /// REGISTRO G605: ANTECIPAÇÃO OU DIFERENCIAL POR UF

  TRegistroG605 = class
  private
    FIND_SIT       : TACBrIndSitDifAnt; /// Indicador de situação de diferencial de alíquota e antecipação em operações interestaduais
    FVL_TOT_ANTC_NF: Variant;           /// Valor total das Antecipações ou Diferencial de Alíquotas destacados nas NFs de entradas interestaduais
    FVL_TOT_AJ_ANTC: Variant;           /// Valor total dos Ajustes das compras sujeitas a Antecipações ou Diferencial de Alíquotas nas entradas interestaduais
    FVL_TOT_DA_ANTC: Variant;           /// Valor total a declarar de ICMS das Antecipações ou Diferencial de Alíquotas nas entradas interestaduais
  public
    constructor Create(); virtual; /// Create

    property IND_SIT       : TACBrIndSitDifAnt read FIND_SIT        write FIND_SIT;
    property VL_TOT_ANTC_NF: Variant           read FVL_TOT_ANTC_NF write FVL_TOT_ANTC_NF;
    property VL_TOT_AJ_ANTC: Variant           read FVL_TOT_AJ_ANTC write FVL_TOT_AJ_ANTC;
    property VL_TOT_DA_ANTC: Variant           read FVL_TOT_DA_ANTC write FVL_TOT_DA_ANTC;
  end;

  TRegistroG605List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG605;
    procedure SetItem(Index: Integer; const Value: TRegistroG605);
  public
    function New(): TRegistroG605;
    property Items[Index: Integer]: TRegistroG605 read GetItem write SetItem;
  end;

  /// REGISTRO G610: TOTALIZADOR DA SUBSTITUIÇÃO TRIBUTÁRIA NAS VENDAS PARA CONSUMO (OU CONSUMIDOR FINAL)

  TRegistroG610 = class
  private
    FVL_TOT_ST_DEC   : Variant;  /// Valor total de Substituição Tributária destacado nas NFs de vendas interestaduais para consumo ou consumidor final
    FVL_TOT_AJ_ST    : Variant;  /// Valor total dos Ajustes nas  vendas interestaduais para consumo ou consumidor final
    FVL_TOT_ST_NF    : Variant;  /// Valor total a declarar nas  vendas interestaduais para consumo ou consumidor final
    FRegistroG615List: TRegistroG615List;

  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_TOT_ST_NF : Variant           read FVL_TOT_ST_NF     write FVL_TOT_ST_NF;
    property VL_TOT_AJ_ST : Variant           read FVL_TOT_AJ_ST     write FVL_TOT_AJ_ST;
    property VL_TOT_ST_DEC: Variant           read FVL_TOT_ST_DEC    write FVL_TOT_ST_DEC;
    property RegistroG615 : TRegistroG615List read FRegistroG615List write FRegistroG615List;
  end;

//  TRegistroG610List = class(TObjectList)
//  private
//    function GetItem(Index: Integer): TRegistroG610;
//    procedure SetItem(Index: Integer; const Value: TRegistroG610);
//  public
//    function New(): TRegistroG610;
//    property Items[Index: Integer]: TRegistroG610 read GetItem write SetItem;
//  end;

  /// REGISTRO G615: ANTECIPAÇÃO POR UF DE ORIGEM (VENDA PARA CONSUMO FINAL)

  TRegistroG615 = class
  private
    FUF              : string;  /// Sigla da unidade da Federação de destino das operações interestaduais
    FVL_TOT_ST_UF_NF : Variant; /// Valor total de Substituição Tributária destacado nas NFs de vendas interestaduais para consumo ou consumidor final por UF
    FVL_TOT_AJ_ST_UF : Variant; /// Valor total dos Ajustes na Substituição Tributária destacado nas NFs de vendas interestaduais para consumo ou consumidor final por UF
    FVL_TOT_ST_UF_DEC: Variant; /// Valor total a declarar de Substituição Tributária nas vendas interestaduais para consumo ou consumidor final por UF
  public
    constructor Create(); /// Create

    property UF              : string  read FUF               write FUF;
    property VL_TOT_ST_UF_NF : Variant read FVL_TOT_ST_UF_NF  write FVL_TOT_ST_UF_NF;
    property VL_TOT_AJ_ST_UF : Variant read FVL_TOT_AJ_ST_UF  write FVL_TOT_AJ_ST_UF;
    property VL_TOT_ST_UF_DEC: Variant read FVL_TOT_ST_UF_DEC write FVL_TOT_ST_UF_DEC;
  end;

  TRegistroG615List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG615;
    procedure SetItem(Index: Integer; const Value: TRegistroG615);
  public
    function New(): TRegistroG615;
    property Items[Index: Integer]: TRegistroG615 read GetItem write SetItem;
  end;

  /// REGISTRO G620: ICMS SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroG620 = class
  private
    FIND_OPER        : TACBrTipoOperacao; /// Indicador de totalização da operações interestaduaiss
    FIND_EMIT        : TACBrEmitente;     /// Indicador do emitente do documento fiscal:
    FVL_TOT_ST_NF    : Currency;          /// Valor total de ST destacado nas NFs
    FVL_TOT_AJ_ST    : Currency;          /// Valor total dos Ajustes de ST destacado nas NFs
    FVL_TOT_ST_DEC   : Currency;          /// Valor total a declarar de ST
    FVL_TOT_ST_COMB  : Currency;          /// Valor total referente a ST de combustível
    FRegistroG625List: TRegistroG625List; ///
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER      : TACBrTipoOperacao read FIND_OPER         write FIND_OPER;
    property IND_EMIT      : TACBrEmitente     read FIND_EMIT         write FIND_EMIT;
    property VL_TOT_ST_NF  : Currency          read FVL_TOT_ST_NF     write FVL_TOT_ST_NF;
    property VL_TOT_AJ_ST  : Currency          read FVL_TOT_AJ_ST     write FVL_TOT_AJ_ST;
    property VL_TOT_ST_DEC : Currency          read FVL_TOT_ST_DEC    write FVL_TOT_ST_DEC;
    property VL_TOT_ST_COMB: Currency          read FVL_TOT_ST_COMB   write FVL_TOT_ST_COMB;
    property RegistroG625  : TRegistroG625List read FRegistroG625List write FRegistroG625List;
  end;

  TRegistroG620List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG620;
    procedure SetItem(Index: Integer; const Value: TRegistroG620);
  public
    function New(): TRegistroG620;
    property Items[Index: Integer]: TRegistroG620 read GetItem write SetItem;
  end;

  /// REGISTRO G625: ST- SUBSTITUIÇÃO POR UF DE DESTINO

  TRegistroG625 = class
  private
    FUF           : string;         /// Sigla da unidade da Federação de origem ou de destino das operações interestaduais
    FIND_TP_ST    : TACBrIndTipoST; /// Indicador do tipo de valor
    FVL_TOT_ST_NF : Currency;       /// Valor ST destacado nas NFs
    FVL_TOT_AJ_ST : Currency;       /// Valor ST de Ajustes destacada nas NFs
    FVL_TOT_DEC_ST: Currency;       /// Valor ST Declarado nas NFs
  public
    constructor Create(); virtual; /// Create

    property UF           : string         read FUF            write FUF;
    property IND_TP_ST    : TACBrIndTipoST read FIND_TP_ST     write FIND_TP_ST;
    property VL_TOT_ST_NF : Currency       read FVL_TOT_ST_NF  write FVL_TOT_ST_NF;
    property VL_TOT_AJ_ST : Currency       read FVL_TOT_AJ_ST  write FVL_TOT_AJ_ST;
    property VL_TOT_DEC_ST: Currency       read FVL_TOT_DEC_ST write FVL_TOT_DEC_ST;
  end;

  TRegistroG625List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG625;
    procedure SetItem(Index: Integer; const Value: TRegistroG625);
  public
    function New(): TRegistroG625;
    property Items[Index: Integer]: TRegistroG625 read GetItem write SetItem;
  end;

  /// REGISTRO G990: ENCERRAMENTO DO BLOCO G

  TRegistroG990 = class
  private
    fQTD_LIN_G: Integer; /// Quantidade total de linhas do Bloco G
  public
    property QTD_LIN_G: Integer read fQTD_LIN_G write fQTD_LIN_G;
  end;

implementation

{ TRegistroG001 }

constructor TRegistroG001.Create;
begin
  inherited Create;
  FRegistroG020 := TRegistroG020.Create();
end;

destructor TRegistroG001.Destroy;
begin
  FRegistroG020.Free;
  inherited;
end;

{ TRegistroG020 }

constructor TRegistroG020.Create();
begin
  inherited Create;
  FRegistroG600 := TRegistroG600.Create();
  FRegistroG610 := TRegistroG610.Create;
  FRegistroG620 := TRegistroG620List.Create;
end;

destructor TRegistroG020.Destroy;
begin
  FRegistroG600.Free;
  inherited;
end;

{ TRegistroG600 }

constructor TRegistroG600.Create();
begin
  inherited Create;
  FRegistroG605List := TRegistroG605List.Create;
end;

destructor TRegistroG600.Destroy;
begin
  FRegistroG605List.Free;
  inherited;
end;

{ TRegistroG605 }

constructor TRegistroG605.Create();
begin

end;

{ TRegistroG605List }

function TRegistroG605List.GetItem(Index: Integer): TRegistroG605;
begin
  Result := TRegistroG605(Inherited Items[Index]);
end;

function TRegistroG605List.New(): TRegistroG605;
begin
  Result := TRegistroG605.Create();
  Add(Result);
end;

procedure TRegistroG605List.SetItem(Index: Integer; const Value: TRegistroG605);
begin
  Put(Index, Value);
end;

{ TRegistroG610 }

constructor TRegistroG610.Create();
begin
  FRegistroG615List := TRegistroG615List.Create;
end;

destructor TRegistroG610.Destroy;
begin
  FRegistroG615List.Free;
  inherited;
end;

(*
{ TRegistroG610List }

function TRegistroG610List.GetItem(Index: Integer): TRegistroG610;
begin
  Result := TRegistroG610(Inherited Items[Index]);
end;

function TRegistroG610List.New(): TRegistroG610;
begin
  Result := TRegistroG610.Create();
  Add(Result);
end;

procedure TRegistroG610List.SetItem(Index: Integer; const Value: TRegistroG610);
begin
  Put(Index, Value);
end;
*)

{ TRegistroG615 }

constructor TRegistroG615.Create();
begin
  inherited Create;

end;

{ TRegistroG615List }

function TRegistroG615List.GetItem(Index: Integer): TRegistroG615;
begin
  Result := TRegistrog615(Inherited Items[Index]);
end;

function TRegistroG615List.New(): TRegistroG615;
begin
  Result := TRegistroG615.Create();
  Add(Result);
end;

procedure TRegistroG615List.SetItem(Index: Integer; const Value: TRegistroG615);
begin
  Put(Index, Value);
end;

{ TRegistroG620 }

constructor TRegistroG620.Create();
begin
  FRegistroG625List := TRegistroG625List.Create;
end;

destructor TRegistroG620.Destroy;
begin
  FRegistroG625List.Free;
  inherited;
end;

{ TRegistroG620List }

function TRegistroG620List.GetItem(Index: Integer): TRegistroG620;
begin
  Result := TRegistroG620(Inherited Items[Index]);
end;

function TRegistroG620List.New(): TRegistroG620;
begin
  Result := TRegistroG620.Create();
  Add(Result);
end;

procedure TRegistroG620List.SetItem(Index: Integer; const Value: TRegistroG620);
begin
  Put(Index, Value);
end;

{ TRegistroG625 }

constructor TRegistroG625.Create();
begin

end;

{ TRegistroG625List }

function TRegistroG625List.GetItem(Index: Integer): TRegistroG625;
begin
  Result := TRegistroG625(Inherited Items[Index]);
end;

function TRegistroG625List.New(): TRegistroG625;
begin
  Result := TRegistroG625.Create();
  Add(Result);
end;

procedure TRegistroG625List.SetItem(Index: Integer; const Value: TRegistroG625);
begin
  Put(Index, Value);
end;

end.
