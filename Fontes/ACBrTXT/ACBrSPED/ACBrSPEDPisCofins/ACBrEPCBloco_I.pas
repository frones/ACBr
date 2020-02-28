{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Juliomar Marchetti            }
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

unit ACBrEPCBloco_I;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
  TRegistroI010List = class;
  TRegistroI100List = class;
  TRegistroI199List = class;
  TRegistroI200List = class;
  TRegistroI299List = class;
  TRegistroI300List = class;
  TRegistroI399List = class;

  /// REGISTRO I001: ABERTURA DO BLOCO I

  TRegistroI001 = class(TOpenBlocos)
  private
    FRegistroI010: TRegistroI010List;
  public
    constructor Create;  virtual;   /// Create
    destructor  Destroy; override;  /// Destroy

    property RegistroI010: TRegistroI010List read FRegistroI010 write FRegistroI010;
  end;

  /// REGISTRO I010: IDENTIFICAÇÃO DA PESSOA JURIDICA/ESTABELECIMENTO

  TRegistroI010 = class
  private
    FRegistroI100: TRegistroI100List;

    fCNPJ: string;                           //02 Número de inscrição da pessoa jurídica no CNPJ. N 014*
    fIND_ATIV: integer;                      //03 Indicador de operações realizadas no período N 002*
    fINFO_COMPL: string;                     //04 Informação Complementar C
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property CNPJ : string read fCNPJ write fCNPJ;
    property IND_ATIV : integer read fIND_ATIV write fIND_ATIV;// enumerador
    property INFO_COMPL : string read fINFO_COMPL write fINFO_COMPL;

    property RegistroI100: TRegistroI100List read FRegistroI100 write FRegistroI100;
  end;

  /// Registro I010 - Lista

  TRegistroI010List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI010;
    procedure SetItem(Index: Integer; const Value: TRegistroI010);
  public
    function New: TRegistroI010;
    property Items[Index: Integer]: TRegistroI010 read GetItem write SetItem;
  end;

  /// REGISTRO I100: CONSOLIDAÇÃO DAS OPERAÇÕES DO PERÍODO

  { TRegistroI100 }

  TRegistroI100 = class
  private
    FRegistroI199: TRegistroI199List;
    FRegistroI200 : TRegistroI200List;

    fVL_REC: Double;                        //02 Valor Total do Faturamento/Receita Bruta no Período N - 02
    fCST_PIS_COFINS: TACBrCstPisCofins;     //03 Código de Situação Tributária referente à Receita informada no Campo 02 (Tabelas 4.3.3 e 4.3.4) N 002* -
    fVL_TOT_DED_GER: Double;                //04 Valor Total das Deduções e Exclusões de Caráter Geral N - 02
    fVL_TOT_DED_ESP: Double;                //05 Valor Total das Deduções e Exclusões de Caráter Específico N - 02
    fALIQ_COFINS: Double;                   //06 Valor da base de cálculo do PIS/PASEP N - 02
    fVL_BC_PIS: Double;                     //07 Alíquota do PIS/PASEP (em percentual) N 008 02
    fALIQ_PIS: Double;                      //08 Valor do PIS/PASEP N - 02
    fVL_PIS: Double;                        //09 Valor da base de cálculo da Cofins N - 02
    fVL_BC_COFINS: Double;                  //10 Alíquota da COFINS (em percentual) N 008 02
    fVL_COFINS: double;                     //11 Valor da COFINS N - 02
    fINFO_COMPL: string;                    //12 Informação Complementar dos dados informados no registro C - -

  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property VL_REC : Double read fVL_REC write fVL_REC;
    property CST_PIS_COFINS : TACBrCstPisCofins read fCST_PIS_COFINS write fCST_PIS_COFINS;
    property VL_TOT_DED_GER : Double read fVL_TOT_DED_GER write fVL_TOT_DED_GER;
    property VL_TOT_DED_ESP : Double read fVL_TOT_DED_ESP write fVL_TOT_DED_ESP;
    property VL_BC_PIS : Double read fVL_BC_PIS write fVL_BC_PIS;
    property ALIQ_PIS : Double read fALIQ_PIS write fALIQ_PIS;
    property VL_PIS : Double read fVL_PIS write fVL_PIS;
    property VL_BC_COFINS : Double read fVL_BC_COFINS write fVL_BC_COFINS;
    property ALIQ_COFINS : Double  read fALIQ_COFINS write fALIQ_COFINS;
    property VL_COFINS : double read fVL_COFINS write fVL_COFINS;
    property INFO_COMPL : string read fINFO_COMPL write fINFO_COMPL;

    property RegistroI199: TRegistroI199List read FRegistroI199 write FRegistroI199;
    property RegistroI200: TRegistroI200List read FRegistroI200 write FRegistroI200;
  end;

  /// Registro I100 - Lista

  TRegistroI100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI100;
    procedure SetItem(Index: Integer; const Value: TRegistroI100);
  public
    function New: TRegistroI100;
    property Items[Index: Integer]: TRegistroI100 read GetItem write SetItem;
  end;

  /// REGISTRO I199: PROCESSO REFERENCIADO

  TRegistroI199 = class
  private
    fIND_PROC: TACBrOrigemProcesso;
    fNUM_PROC: string;
  public
    property NUM_PROC : string              read fNUM_PROC write fNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
  end;

  /// Registro I199 - Lista

  { TRegistroI199List }

  TRegistroI199List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI199;
    procedure SetItem(Index: Integer; const Value: TRegistroI199);
  public
    function New: TRegistroI199;
    property Items[Index: Integer]: TRegistroI199 read GetItem write SetItem;
  end;

  /// REGISTRO I200: COMPOSIÇÃO DAS RECEITAS, DEDUÇÕES E/OU EXCLUSÕES DO PERÍODO

  TRegistroI200 = class
  private
    FRegistroI299 : TRegistroI299List;
    FRegistroI300 : TRegistroI300List;

    fNUM_CAMPO: string;                     //02 Informar o número do campo do registro “I100” (Campos 02, 04 ou 05), objeto de informação neste registro.C 002* -
    fCOD_DET: string;                       //03 Código do tipo de detalhamento, conforme Tabelas 7.1.1 e/ou 7.1.2 C 005* -
    fDET_VALOR: double;                     //04 Valor detalhado referente ao campo 03 (COD_DET) deste registro N - 02
    fCOD_CTA: string;                       //05 Código da conta contábil referente ao valor informado no campo 04 (DET_VALOR) C 060 -
    fINFO_COMPL: string;                    //06 Informação Complementar dos dados informados no registro C - -
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property NUM_CAMPO : string read fNUM_CAMPO write fNUM_CAMPO;
    property COD_DET : string read fCOD_DET write fCOD_DET;
    property DET_VALOR : double read fDET_VALOR write fDET_VALOR;
    property COD_CTA : string read fCOD_CTA write fCOD_CTA;
    property INFO_COMPL : string read fINFO_COMPL write fINFO_COMPL;

    property RegistroI299: TRegistroI299List read FRegistroI299 write FRegistroI299;
    property RegistroI300: TRegistroI300List read FRegistroI300 write FRegistroI300;
  end;

  /// Registro I200 - Lista

  TRegistroI200List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI200;
    procedure SetItem(Index: Integer; const Value: TRegistroI200);
  public
    function New: TRegistroI200;
    property Items[Index: Integer]: TRegistroI200 read GetItem write SetItem;
  end;

  /// REGISTRO I299: PROCESSO REFERENCIADO

  TRegistroI299 = class
  private
    fNUM_PROC: string;                    //02 Identificação do processo ou ato concessório C 020 -
    fIND_PROC: TACBrOrigemProcesso;       //03 Indicador da origem do processo: C 001* -
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property NUM_PROC : string read fNUM_PROC write fNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
  end;

  /// Registro I299 - Lista

  TRegistroI299List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI299;
    procedure SetItem(Index: Integer; const Value: TRegistroI299);
  public
    function New: TRegistroI299;
    property Items[Index: Integer]: TRegistroI299 read GetItem write SetItem;
  end;

  /// REGISTRO I300: COMPLEMENTO DAS OPERAÇÕES – DETALHAMENTO DAS RECEITAS, DEDUÇÕES E/OU EXCLUSÕES DO PERÍODO

  TRegistroI300 = class
  private
    FRegistroI399 : TRegistroI399List;

    fCOD_COMP: string;                          //02 Código das Tabelas 7.1.3 (Receitas – Visão Analítica/Referenciada) e/ou 7.1.4 (Deduções e exclusões – Visão Analítica/Referenciada), objeto de complemento neste registro C 060 -
    fDET_VALOR: Double;                         //03 Valor da receita, dedução ou exclusão, objeto de complemento/detalhamento neste registro, conforme código informado no campo 02 (especificados nas tabelas analíticas 7.1.3 e 7.1.4) ou no campo 04 (código da conta contábil) N - 02
    fCOD_CTA: string;                           //04 Código da conta contábil referente ao valor informado no campo 03 C 060 -
    fINFO_COMPL: string;                        //05 Informação Complementar dos dados informados no registro C - -
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property COD_COMP : string read fCOD_COMP write fCOD_COMP;
    property DET_VALOR : Double read fDET_VALOR write fDET_VALOR;
    property COD_CTA : string read fCOD_CTA write fCOD_CTA;
    property INFO_COMPL : string read fINFO_COMPL write fINFO_COMPL;

    property RegistroI399: TRegistroI399List read FRegistroI399 write FRegistroI399;
  end;

  /// Registro I300 - Lista

  TRegistroI300List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI300;
    procedure SetItem(Index: Integer; const Value: TRegistroI300);
  public
    function New: TRegistroI300;
    property Items[Index: Integer]: TRegistroI300 read GetItem write SetItem;
  end;

  /// REGISTRO I399: PROCESSO REFERENCIADO

  TRegistroI399 = class
  private
    fNUM_PROC: string;               //02 Identificação do processo ou ato concessório C 020 -
    fIND_PROC: TACBrOrigemProcesso;  //03 Indicador da origem do processo: 1 - Justiça Federal; 3 – Secretaria da Receita Federal do Brasil 9 – Outros. C 001* -
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property NUM_PROC : string read fNUM_PROC write fNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
  end;

  /// Registro I300 - Lista

  TRegistroI399List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroI399;
    procedure SetItem(Index: Integer; const Value: TRegistroI399);
  public
    function New: TRegistroI399;
    property Items[Index: Integer]: TRegistroI399 read GetItem write SetItem;
  end;

  // REGISTRO I990: ENCERRAMENTO DO BLOCO I

  TRegistroI990 = class
  private
    fQTD_LIN_I : integer;          //02	Quantidade total de linhas do Bloco I	N	-	-
  public
    property QTD_LIN_I: integer read FQTD_LIN_I write FQTD_LIN_i;
  end;

implementation

{ TRegistroI199List }

function TRegistroI199List.GetItem(Index: Integer): TRegistroI199;
begin
  Result := TRegistroI199(Inherited Items[Index]);
end;

procedure TRegistroI199List.SetItem(Index: Integer; const Value: TRegistroI199);
begin
  Put(Index, Value);
end;

function TRegistroI199List.New: TRegistroI199;
begin
  Result := TRegistroI199.Create;
  Add(Result);
end;

{ TRegistroI399List }

function TRegistroI399List.GetItem(Index: Integer): TRegistroI399;
begin
  Result := TRegistroI399(Inherited Items[Index]);
end;

procedure TRegistroI399List.SetItem(Index: Integer; const Value: TRegistroI399);
begin
  Put(Index, Value);
end;

function TRegistroI399List.New: TRegistroI399;
begin
  Result := TRegistroI399.Create;
  Add(Result);
end;

{ TRegistroI300List }

function TRegistroI300List.GetItem(Index: Integer): TRegistroI300;
begin
  Result := TRegistroI300(Inherited Items[Index]);
end;

procedure TRegistroI300List.SetItem(Index: Integer; const Value: TRegistroI300);
begin
  Put(Index, Value);
end;

function TRegistroI300List.New: TRegistroI300;
begin
  Result := TRegistroI300.Create;
  Add(Result);
end;

{ TRegistroI299List }

function TRegistroI299List.GetItem(Index: Integer): TRegistroI299;
begin
  Result := TRegistroI299(Inherited Items[Index]);
end;

procedure TRegistroI299List.SetItem(Index: Integer; const Value: TRegistroI299);
begin
  Put(Index, Value);
end;

function TRegistroI299List.New: TRegistroI299;
begin
  Result := TRegistroI299.Create;
  Add(Result);
end;

{ TRegistroI200List }

function TRegistroI200List.GetItem(Index: Integer): TRegistroI200;
begin
  Result := TRegistroI200(Inherited Items[Index]);
end;

procedure TRegistroI200List.SetItem(Index: Integer; const Value: TRegistroI200);
begin
  Put(Index, Value);
end;

function TRegistroI200List.New: TRegistroI200;
begin
  Result := TRegistroI200.Create;
  Add(Result);
end;

{ TRegistroI100List }

function TRegistroI100List.GetItem(Index: Integer): TRegistroI100;
begin
  Result := TRegistroI100(Inherited Items[Index]);
end;

procedure TRegistroI100List.SetItem(Index: Integer; const Value: TRegistroI100);
begin
  Put(Index, Value);
end;

function TRegistroI100List.New: TRegistroI100;
begin
  Result := TRegistroI100.Create;
  Add(Result);
end;

function TRegistroI010List.GetItem(Index: Integer): TRegistroI010;
begin
  Result := TRegistroI010(Inherited Items[Index]);
end;

procedure TRegistroI010List.SetItem(Index: Integer; const Value: TRegistroI010);
begin
  Put(Index, Value);
end;

function TRegistroI010List.New: TRegistroI010;
begin
  Result := TRegistroI010.Create;
  Add(Result);
end;

constructor TRegistroI399.Create;
begin

end;

destructor TRegistroI399.Destroy;
begin
  inherited Destroy;
end;

constructor TRegistroI300.Create;
begin
  FRegistroI399 := TRegistroI399List.Create;
end;

destructor TRegistroI300.Destroy;
begin
  FRegistroI399.Free;

  inherited Destroy;
end;

constructor TRegistroI299.Create;
begin

end;

destructor TRegistroI299.Destroy;
begin
  inherited Destroy;
end;

constructor TRegistroI200.Create;
begin
  FRegistroI299 := TRegistroI299List.Create;
  FRegistroI300 := TRegistroI300List.Create;
end;

destructor TRegistroI200.Destroy;
begin
  FRegistroI299.Free;
  FRegistroI300.Free;

  inherited Destroy;
end;

constructor TRegistroI100.Create;
begin
  FRegistroI199 := TRegistroI199List.Create;
  FRegistroI200 := TRegistroI200List.Create;

end;

destructor TRegistroI100.Destroy;
begin
  FRegistroI199.Free;
  FRegistroI200.Free;
  inherited Destroy;
end;

constructor TRegistroI010.Create;
begin
  FRegistroI100 := TRegistroI100List.Create;
end;

destructor TRegistroI010.Destroy;
begin
  FRegistroI100.Free;
  inherited Destroy;
end;

constructor TRegistroI001.Create;
begin
  inherited Create;
  FRegistroI010 := TRegistroI010List.Create;
end;

destructor TRegistroI001.Destroy;
begin
  FRegistroI010.Free;
  inherited Destroy;
end;

end.

