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

{$I ACBr.inc}

unit ACBrECFBloco_N;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroN030List = class;
  TRegistroN500List = class;
  TRegistroN600List = class;
  TRegistroN610List = class;
  TRegistroN615List = class;
  TRegistroN620List = class;
  TRegistroN630List = class;
  TRegistroN650List = class;
  TRegistroN660List = class;
  TRegistroN670List = class;

  /// Registro N001 - Abertura do bloco N – Cálculo do IRPJ e da CSLL
  TRegistroN001 = class(TOpenBlocos)
  private
    FRegistroN030: TRegistroN030List;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RegistroN030: TRegistroN030List read FRegistroN030 write FRegistroN030;
  end;

  /// Registro N030 - Identificação do Período e Forma de Apuração do
  /// IRPJ e da CSLL das Empresas Tributadas pelo Lucro Real
  TRegistroN030 = class
  private
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    fPER_APUR: string;

    FRegistroN500: TRegistroN500List;
    FRegistroN600: TRegistroN600List;
    FRegistroN610: TRegistroN610List;
    FRegistroN615: TRegistroN615List;
    FRegistroN620: TRegistroN620List;
    FRegistroN630: TRegistroN630List;
    FRegistroN650: TRegistroN650List;
    FRegistroN660: TRegistroN660List;
    FRegistroN670: TRegistroN670List;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    // registros filhos
    property RegistroN500: TRegistroN500List read FRegistroN500 write FRegistroN500;
    property RegistroN600: TRegistroN600List read FRegistroN600 write FRegistroN600;
    property RegistroN610: TRegistroN610List read FRegistroN610 write FRegistroN610;
    property RegistroN615: TRegistroN615List read FRegistroN615 write FRegistroN615;
    property RegistroN620: TRegistroN620List read FRegistroN620 write FRegistroN620;
    property RegistroN630: TRegistroN630List read FRegistroN630 write FRegistroN630;
    property RegistroN650: TRegistroN650List read FRegistroN650 write FRegistroN650;
    property RegistroN660: TRegistroN660List read FRegistroN660 write FRegistroN660;
    property RegistroN670: TRegistroN670List read FRegistroN670 write FRegistroN670;
  end;

  /// Registro N030 - Lista
  TRegistroN030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN030;
    procedure SetItem(Index: Integer; const Value: TRegistroN030);
  public
    function New: TRegistroN030;
    property Items[Index: Integer]: TRegistroN030 read GetItem write SetItem;
  end;

  /// Registro N500 - Base de Cálculo do IRPJ Sobre o Lucro Real Após
  /// as Compensações de Prejuízos
  TRegistroN500 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N500 - Lista
  TRegistroN500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN500;
    procedure SetItem(Index: Integer; const Value: TRegistroN500);
  public
    function New: TRegistroN500;
    property Items[Index: Integer]: TRegistroN500 read GetItem write SetItem;
  end;

  /// Registro N600 - Demonstração do Lucro da Exploração
  TRegistroN600 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N600 - Lista
  TRegistroN600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN600;
    procedure SetItem(Index: Integer; const Value: TRegistroN600);
  public
    function New: TRegistroN600;
    property Items[Index: Integer]: TRegistroN600 read GetItem write SetItem;
  end;

  /// Registro N610 - Cálculo da Isenção e Redução do Imposto sobre Lucro Real
  TRegistroN610 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N610 - Lista
  TRegistroN610List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN610;
    procedure SetItem(Index: Integer; const Value: TRegistroN610);
  public
    function New: TRegistroN610;
    property Items[Index: Integer]: TRegistroN610 read GetItem write SetItem;
  end;

  /// Registro N615 - Informações da Base de Cálculo de Incentivos Fiscais
  TRegistroN615 = class
  private
    fBASE_CALC:    variant;
    fPER_INCEN_FINAM: variant;
    fPER_INCEN_FINOR: variant;
    fPER_INCEN_FUNRES: variant;
    fPER_VL_SUBTOTAL: variant;
    fPER_VL_TOTAL: variant;
    fVL_LIQ_INCEN_FINAM: variant;
    fVL_LIQ_INCEN_FINOR: variant;
    fVL_LIQ_INCEN_FUNRES: variant;
    fVL_SUBTOTAL:  variant;
    fVL_TOTAL:     variant;
  public
    property BASE_CALC: variant read fBASE_CALC write fBASE_CALC;
    property PER_INCEN_FINOR: variant read fPER_INCEN_FINOR write fPER_INCEN_FINOR;
    property VL_LIQ_INCEN_FINOR: variant read fVL_LIQ_INCEN_FINOR write fVL_LIQ_INCEN_FINOR;
    property PER_INCEN_FINAM: variant read fPER_INCEN_FINAM write fPER_INCEN_FINAM;
    property VL_LIQ_INCEN_FINAM: variant read fVL_LIQ_INCEN_FINAM write fVL_LIQ_INCEN_FINAM;
    property VL_SUBTOTAL: variant read fVL_SUBTOTAL write fVL_SUBTOTAL;
    property PER_VL_SUBTOTAL: variant read fPER_VL_SUBTOTAL write fPER_VL_SUBTOTAL;
    property PER_INCEN_FUNRES: variant read fPER_INCEN_FUNRES write fPER_INCEN_FUNRES;
    property VL_LIQ_INCEN_FUNRES: variant read fVL_LIQ_INCEN_FUNRES write fVL_LIQ_INCEN_FUNRES;
    property VL_TOTAL: variant read fVL_TOTAL write fVL_TOTAL;
    property PER_VL_TOTAL: variant read fPER_VL_TOTAL write fPER_VL_TOTAL;
  end;

  /// Registro N615 - Lista
  TRegistroN615List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN615;
    procedure SetItem(Index: Integer; const Value: TRegistroN615);
  public
    function New: TRegistroN615;
    property Items[Index: Integer]: TRegistroN615 read GetItem write SetItem;
  end;


  /// Registro N620 - Cálculo do IRPJ Mensal por Estimativa
  TRegistroN620 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N620 - Lista
  TRegistroN620List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN620;
    procedure SetItem(Index: Integer; const Value: TRegistroN620);
  public
    function New: TRegistroN620;
    property Items[Index: Integer]: TRegistroN620 read GetItem write SetItem;
  end;

  /// Registro N630 - Cálculo do IRPJ Com Base no Lucro Real
  TRegistroN630 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N630 - Lista
  TRegistroN630List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN630;
    procedure SetItem(Index: Integer; const Value: TRegistroN630);
  public
    function New: TRegistroN630;
    property Items[Index: Integer]: TRegistroN630 read GetItem write SetItem;
  end;

  /// Registro N650 - Base de Cálculo da CSLL Após Compensações das
  /// Bases de Cálculo Negativa
  TRegistroN650 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N650 - Lista
  TRegistroN650List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN650;
    procedure SetItem(Index: Integer; const Value: TRegistroN650);
  public
    function New: TRegistroN650;
    property Items[Index: Integer]: TRegistroN650 read GetItem write SetItem;
  end;

  /// Registro N660 - Cálculo da CSLL Mensal por Estimativa
  TRegistroN660 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N660 - Lista
  TRegistroN660List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN660;
    procedure SetItem(Index: Integer; const Value: TRegistroN660);
  public
    function New: TRegistroN660;
    property Items[Index: Integer]: TRegistroN660 read GetItem write SetItem;
  end;

  /// Registro N670 -Cálculo da CSLL Com Base no Lucro Real
  TRegistroN670 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N670 - Lista
  TRegistroN670List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroN670;
    procedure SetItem(Index: Integer; const Value: TRegistroN670);
  public
    function New: TRegistroN670;
    property Items[Index: Integer]: TRegistroN670 read GetItem write SetItem;
  end;

  /// Registro N990 - ENCERRAMENTO DO BLOCO N
  TRegistroN990 = class(TCloseBlocos)
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;


implementation

{ TRegistroN030List }

function TRegistroN030List.GetItem(Index: Integer): TRegistroN030;
begin
   Result := TRegistroN030(Inherited Items[Index]);
end;

function TRegistroN030List.New: TRegistroN030;
begin
  Result := TRegistroN030.Create;
  Add(Result);
end;

procedure TRegistroN030List.SetItem(Index: Integer; const Value: TRegistroN030);
begin
  Put(Index, Value);
end;

{ TRegistroN500List }

function TRegistroN500List.GetItem(Index: Integer): TRegistroN500;
begin
   Result := TRegistroN500(Inherited Items[Index]);
end;

function TRegistroN500List.New: TRegistroN500;
begin
  Result := TRegistroN500.Create;
  Add(Result);
end;

procedure TRegistroN500List.SetItem(Index: Integer; const Value: TRegistroN500);
begin
  Put(Index, Value);
end;

{ TRegistroN500 }

{ TRegistroN600List }

function TRegistroN600List.GetItem(Index: Integer): TRegistroN600;
begin
   Result := TRegistroN600(Inherited Items[Index]);
end;

function TRegistroN600List.New: TRegistroN600;
begin
  Result := TRegistroN600.Create;
  Add(Result);
end;

procedure TRegistroN600List.SetItem(Index: Integer; const Value: TRegistroN600);
begin
  Put(Index, Value);
end;


{ TRegistroN610List }

function TRegistroN610List.GetItem(Index: Integer): TRegistroN610;
begin
   Result := TRegistroN610(Inherited Items[Index]);
end;

function TRegistroN610List.New: TRegistroN610;
begin
  Result := TRegistroN610.Create;
  Add(Result);
end;

procedure TRegistroN610List.SetItem(Index: Integer; const Value: TRegistroN610);
begin
  Put(Index, Value);
end;

{ TRegistroN615List }

function TRegistroN615List.GetItem(Index: Integer): TRegistroN615;
begin
   Result := TRegistroN615(Inherited Items[Index]);
end;

function TRegistroN615List.New: TRegistroN615;
begin
  Result := TRegistroN615.Create;
  Add(Result);
end;

procedure TRegistroN615List.SetItem(Index: Integer; const Value: TRegistroN615);
begin
  Put(Index, Value);
end;

{ TRegistroN620List }

function TRegistroN620List.GetItem(Index: Integer): TRegistroN620;
begin
   Result := TRegistroN620(Inherited Items[Index]);
end;

function TRegistroN620List.New: TRegistroN620;
begin
  Result := TRegistroN620.Create;
  Add(Result);
end;

procedure TRegistroN620List.SetItem(Index: Integer; const Value: TRegistroN620);
begin
  Put(Index, Value);
end;

{ TRegistroN630List }

function TRegistroN630List.GetItem(Index: Integer): TRegistroN630;
begin
   Result := TRegistroN630(Inherited Items[Index]);
end;

function TRegistroN630List.New: TRegistroN630;
begin
  Result := TRegistroN630.Create;
  Add(Result);
end;

procedure TRegistroN630List.SetItem(Index: Integer; const Value: TRegistroN630);
begin
  Put(Index, Value);
end;

{ TRegistroN650List }

function TRegistroN650List.GetItem(Index: Integer): TRegistroN650;
begin
   Result := TRegistroN650(Inherited Items[Index]);
end;

function TRegistroN650List.New: TRegistroN650;
begin
  Result := TRegistroN650.Create;
  Add(Result);
end;

procedure TRegistroN650List.SetItem(Index: Integer; const Value: TRegistroN650);
begin
  Put(Index, Value);
end;


{ TRegistroN660List }

function TRegistroN660List.GetItem(Index: Integer): TRegistroN660;
begin
   Result := TRegistroN660(Inherited Items[Index]);
end;

function TRegistroN660List.New: TRegistroN660;
begin
  Result := TRegistroN660.Create;
  Add(Result);
end;

procedure TRegistroN660List.SetItem(Index: Integer; const Value: TRegistroN660);
begin
  Put(Index, Value);
end;


{ TRegistroN670List }

function TRegistroN670List.GetItem(Index: Integer): TRegistroN670;
begin
   Result := TRegistroN670(Inherited Items[Index]);
end;

function TRegistroN670List.New: TRegistroN670;
begin
  Result := TRegistroN670.Create;
  Add(Result);
end;

procedure TRegistroN670List.SetItem(Index: Integer; const Value: TRegistroN670);
begin
  Put(Index, Value);
end;


{ TRegistroN030 }

constructor TRegistroN030.Create;
begin
   inherited;

   FRegistroN500 := TRegistroN500List.Create;
   FRegistroN600 := TRegistroN600List.Create;
   FRegistroN610 := TRegistroN610List.Create;
   FRegistroN615 := TRegistroN615List.Create;
   FRegistroN620 := TRegistroN620List.Create;
   FRegistroN630 := TRegistroN630List.Create;
   FRegistroN650 := TRegistroN650List.Create;
   FRegistroN660 := TRegistroN660List.Create;
   FRegistroN670 := TRegistroN670List.Create;
end;

destructor TRegistroN030.Destroy;
begin
   FRegistroN670.Free;
   FRegistroN660.Free;
   FRegistroN650.Free;
   FRegistroN630.Free;
   FRegistroN620.Free;
   FRegistroN615.Free;
   FRegistroN610.Free;
   FRegistroN600.Free;
   FRegistroN500.Free;

   inherited;
end;

{ TRegistroN001 }

constructor TRegistroN001.Create;
begin
  inherited Create;
  FRegistroN030 := TRegistroN030List.Create;

  IND_DAD := idSemDados;
end;

destructor TRegistroN001.Destroy;
begin
  FRegistroN030.Free;

  inherited;
end;

end.
