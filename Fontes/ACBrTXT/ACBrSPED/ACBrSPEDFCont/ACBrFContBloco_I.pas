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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrFContBloco_I;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrFContBlocos;

type
  /// Registro I001 - ABERTURA DO BLOCO I

  TRegistroI001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroI051List = class;
  TRegistroI155List = class;
  TRegistroI156List = class;
  TRegistroI250List = class;
  TRegistroI256List = class;
  TRegistroI355List = class;
  TRegistroI356List = class;

  /// Registro I050 - PLANO DE CONTAS

  TRegistroI050 = class
  private
    fDT_ALT: TDateTime;       /// Data da inclusão/alteração.
    fCOD_NAT: String;     /// Código da natureza da conta/grupo de contas, conforme tabela publicada pelo Sped.
    fIND_CTA: String;     /// Indicador do tipo de conta: S - Sintética (grupo de contas);A - Analítica (conta).
    fNIVEL: String;       /// Nível da conta analítica/grupo de contas.
    fCOD_CTA: String;     /// Código da conta analítica/grupo de contas.
    fCOD_CTA_SUP: String; /// Código da conta sintética /grupo de contas de nível imediatamente superior.
    fCTA: String;         /// Nome da conta analítica/grupo de contas.

    FRegistroI051: TRegistroI051List;  /// BLOCO I - Lista de RegistroI051 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_ALT: TDateTime read fDT_ALT write fDT_ALT;
    property COD_NAT: String read fCOD_NAT write fCOD_NAT;
    property IND_CTA: String read fIND_CTA write fIND_CTA;
    property NIVEL: String read fNIVEL write fNIVEL;
    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CTA_SUP: String read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property CTA: String read fCTA write fCTA;

    /// Registros FILHOS
    property RegistroI051: TRegistroI051List read FRegistroI051 write FRegistroI051;
  end;

  /// Registro I050 - Lista

  TRegistroI050List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI050;
    procedure SetItem(Index: Integer; const Value: TRegistroI050);
  public
    function New: TRegistroI050;
    property Items[Index: Integer]: TRegistroI050 read GetItem write SetItem;
  end;

  /// Registro I051 - PLANO DE CONTAS REFERENCIAL

  TRegistroI051 = class
  private
    fCOD_ENT_REF: String;    /// Código da instituição responsável pela manutenção do plano de contas referencial.
    fCOD_CCUS: String;       /// Código do centro de custo.
    fCOD_CTA_REF: String;    /// Código da conta de acordo com o plano de contas referencial, conforme tabela publicada pelos órgãos indicados no campo 02- COD_ENT_REF.
  public
    property COD_ENT_REF: String read fCOD_ENT_REF write fCOD_ENT_REF;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
  end;

  /// Registro I051 - Lista

  TRegistroI051List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI051;
    procedure SetItem(Index: Integer; const Value: TRegistroI051);
  public
    function New: TRegistroI051;
    property Items[Index: Integer]: TRegistroI051 read GetItem write SetItem;
  end;

  /// Registro I075 - TABELA DE HISTÓRICO PADRONIZADO

  TRegistroI075 = class
  private
    fCOD_HIST: String;    /// Código do histórico padronizado.
    fDESCR_HIST: String;  /// Descrição do histórico padronizado.
  public
    property COD_HIST: String read fCOD_HIST write fCOD_HIST;
    property DESCR_HIST: String read fDESCR_HIST write fDESCR_HIST;
  end;

  /// Registro I075 - Lista

  TRegistroI075List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI075;
    procedure SetItem(Index: Integer; const Value: TRegistroI075);
  public
    function New: TRegistroI075;
    property Items[Index: Integer]: TRegistroI075 read GetItem write SetItem;
  end;

  /// Registro I100 - CENTRO DE CUSTOS

  TRegistroI100 = class
  private
    fDT_ALT: TdateTime;       /// Data da inclusão/alteração.
    fCOD_CCUS: String;    /// Código do centro de custos.
    fCCUS: String;        /// Nome do centro de custos.
  public
    property DT_ALT: TdateTime read fDT_ALT write fDT_ALT;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property CCUS: String read fCCUS write fCCUS;
  end;

  /// Registro I100 - Lista

  TRegistroI100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI100;
    procedure SetItem(Index: Integer; const Value: TRegistroI100);
  public
    function New: TRegistroI100;
    property Items[Index: Integer]: TRegistroI100 read GetItem write SetItem;
  end;

  /// Registro I150 - SALDOS PERIÓDICOS – IDENTIFICAÇÃO DO PERÍODO

  TRegistroI150 = class
  private
    fDT_INI: TDateTime; /// Data de início do período.
    fDT_FIN: TDateTime; /// Data de fim do período.

    FRegistroI155: TRegistroI155List;  /// BLOCO I - Lista de RegistroI155 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    /// Registros FILHOS
    property RegistroI155: TRegistroI155List read FRegistroI155 write FRegistroI155;
  end;

  /// Registro I150 - Lista

  TRegistroI150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI150;
    procedure SetItem(Index: Integer; const Value: TRegistroI150);
  public
    function New: TRegistroI150;
    property Items[Index: Integer]: TRegistroI150 read GetItem write SetItem;
  end;

  /// Registro I155 - DETALHE DOS SALDOS PERIÓDICOS

  TRegistroI155 = class
  private
    fCOD_CTA: String;     /// Código da conta analítica.
    fCOD_CCUS: String;    /// Código do centro de custos.
    fVL_SLD_INI: Currency;    /// Valor do saldo inicial do período.
    fIND_DC_INI: String;  /// Indicador da situação do saldo inicial:D - Devedor;C - Credor.
    fVL_DEB: Currency;        /// Valor total dos débitos no período.
    fVL_CRED: Currency;       /// Valor total dos créditos no período.
    fVL_SLD_FIN: Currency;    /// Valor do saldo final do período.
    fIND_DC_FIN: String;  /// Indicador da situação do saldo final: D - Devedor; C - Credor.
    
    FRegistroI156: TRegistroI156List;  /// BLOCO I - Lista de RegistroI156 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: Currency read fVL_SLD_INI write fVL_SLD_INI;
    property IND_DC_INI: String read fIND_DC_INI write fIND_DC_INI;
    property VL_DEB: Currency read fVL_DEB write fVL_DEB;
    property VL_CRED: Currency read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: Currency read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_DC_FIN: String read fIND_DC_FIN write fIND_DC_FIN;

    /// Registros FILHOS
    property RegistroI156: TRegistroI156List read FRegistroI156 write FRegistroI156;
  end;

  /// Registro I155 - Lista

  TRegistroI155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI155;
    procedure SetItem(Index: Integer; const Value: TRegistroI155);
  public
    function New: TRegistroI155;
    property Items[Index: Integer]: TRegistroI155 read GetItem write SetItem;
  end;


  /// Registro I156 - Mapeamento Referencial dos Totais de Débitos e Créditos

  TRegistroI156 = class
  private
    fCOD_CTA_REF: String;     /// Código da Conta Referencial.
    fVL_DEB: Currency;    /// Valor Total Débitos.
    fVL_CRED: Currency;    /// Valor Total Créditos.
  public
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_DEB: Currency read fVL_DEB write fVL_DEB;
    property VL_CRED: Currency read fVL_CRED write fVL_CRED;
  end;

  /// Registro I156 - Lista

  TRegistroI156List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI156;
    procedure SetItem(Index: Integer; const Value: TRegistroI156);
  public
    function New: TRegistroI156;
    property Items[Index: Integer]: TRegistroI156 read GetItem write SetItem;
  end;



  // Registro I200 - Lançamentos Contábeis

  TRegistroI200 = class
  private
    fNUM_LCTO: String;        // Número de identificação do lançamento
    fDT_LCTO: TDateTime;         // Data do lançamento
    fVL_LCTO: Currency;           // Valor do Lançamento
    fIND_LCTO: String;        // Indicador do tipo do lançamento

    fRegistroI250: TRegistroI250List; /// BLOCO I - Lista de RegistroI250 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property NUM_LCTO: String read fNUM_LCTO write fNUM_LCTO;
    property DT_LCTO: TDateTime read fDT_LCTO write fDT_LCTO;
    property VL_LCTO: Currency read fVL_LCTO write fVL_LCTO;
    property IND_LCTO: String read fIND_LCTO write fIND_LCTO;
    
    property RegistroI250: TRegistroI250List read fRegistroI250 write fRegistroI250;
  end;

  TRegistroI200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI200;
    procedure SetItem(Index: Integer; Value: TRegistroI200);
  public
    function New: TRegistroI200;
    property Items[Index: Integer]: TRegistroI200 read GetItem write SetItem;
  end;

  // Registro I250 - Partidas do Lançamentos

  TRegistroI250 = class
  private
    fCOD_CTA: String;
    fCOD_CCUS: String;
    fVL_DC: Currency;
    fIND_DC: String;
    fNUM_ARQ: String;
    fCOD_HIST_PAD: String;
    fHIST: String;
    fCOD_PART: String;

    fRegistroI256: TRegistroI256List; /// BLOCO I - Lista de RegistroI256 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property VL_DC: Currency read fVL_DC write fVL_DC;
    property IND_DC: String read fIND_DC write fIND_DC;
    property NUM_ARQ: String  read fNUM_ARQ write fNUM_ARQ;
    property COD_HIST_PAD: String  read fCOD_HIST_PAD write fCOD_HIST_PAD;
    property HIST: String read fHIST write fHIST;
    property COD_PART: String read fCOD_PART write fCOD_PART;

    property RegistroI256: TRegistroI256List read fRegistroI256 write fRegistroI256;
  end;

  // Registro I250 - lista

  TRegistroI250List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI250;
    procedure SetItem(Index: Integer; Value: TRegistroI250);
  public
    function New: TRegistroI250;
    property Items[Index: Integer]: TRegistroI250 read GetItem write SetItem;
  end;


  // Registro I256 - Mapeamento Referencial das Partidas do Lançamento

  TRegistroI256 = class
  private
    fCOD_CTA_REF: String; //Código da Conta Referencial
    fVL_DC: Currency; //Valor da Partida
    fIND_DC: String; //Natureza Partida
  public
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_DC: Currency read fVL_DC write fVL_DC;
    property IND_DC: String read fIND_DC write fIND_DC;
  end;

  // Registro I256 - lista

  TRegistroI256List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI256;
    procedure SetItem(Index: Integer; Value: TRegistroI256);
  public
    function New: TRegistroI256;
    property Items[Index: Integer]: TRegistroI256 read GetItem write SetItem;
  end;



  /// Registro I350 - SALDO DAS CONTAS DE RESULTADO ANTES DO ENCERRAMENTO – IDENTIFICAÇÃO DA DATA

  TRegistroI350 = class
  private
    fDT_RES: TDateTime; /// Data da apuração do resultado.

    FRegistroI355: TRegistroI355List;  /// BLOCO I - Lista de RegistroI355 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_RES: TDateTime read fDT_RES write fDT_RES;
    /// Registros FILHOS
    property RegistroI355: TRegistroI355List read FRegistroI355 write FRegistroI355;
  end;

  /// Registro I350 - Lista

  TRegistroI350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI350;
    procedure SetItem(Index: Integer; const Value: TRegistroI350);
  public
    function New: TRegistroI350;
    property Items[Index: Integer]: TRegistroI350 read GetItem write SetItem;
  end;

  /// Registro I355 - DETALHE DOS SALDOS DAS CONTAS DE RESULTADO ANTES DO ENCERRAMENTO

  TRegistroI355 = class
  private
    fCOD_CTA: String;     /// Código da conta analítica de resultado.
    fCOD_CCUS: String;    /// Código do centro de custos.
    fVL_CTA: Currency;    /// Valor do saldo final antes do lançamento de encerramento.
    fIND_DC: String;  /// Indicador da situação do saldo final: D - Devedor; C - Credor.

    FRegistroI356: TRegistroI356List;  /// BLOCO I - Lista de RegistroI356 (FILHO)

  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property VL_CTA: Currency read fVL_CTA write fVL_CTA;
    property IND_DC: String read fIND_DC write fIND_DC;

    /// Registros FILHOS
    property RegistroI356: TRegistroI356List read FRegistroI356 write FRegistroI356;

  end;

  /// Registro I355 - Lista

  TRegistroI355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI355;
    procedure SetItem(Index: Integer; const Value: TRegistroI355);
  public
    function New: TRegistroI355;
    property Items[Index: Integer]: TRegistroI355 read GetItem write SetItem;
  end;



  /// Registro I356 - Mapeamento Referencial dos Saldos Finais das Contas de Resultado antes

  TRegistroI356 = class
  private
    fCOD_CTA_REF: String;     /// Código da Conta Referencial.
    fVL_CTA: Currency;    /// Valor do saldo final.
    fIND_DC: String;  /// Indicador da situação do saldo final: D - Devedor; C - Credor.

  public
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_CTA: Currency read fVL_CTA write fVL_CTA;
    property IND_DC: String read fIND_DC write fIND_DC;
  end;

  /// Registro I356 - Lista

  TRegistroI356List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI356;
    procedure SetItem(Index: Integer; const Value: TRegistroI356);
  public
    function New: TRegistroI356;
    property Items[Index: Integer]: TRegistroI356 read GetItem write SetItem;
  end;



  /// Registro I990 - ENCERRAMENTO DO BLOCO I

  TRegistroI990 = class
  private
    fQTD_LIN_I: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN_I: Integer read FQTD_LIN_I write FQTD_LIN_I;
  end;

implementation

{ TRegistroI050 }

constructor TRegistroI050.Create;
begin
   FRegistroI051 := TRegistroI051List.Create;
end;

destructor TRegistroI050.Destroy;
begin
  FRegistroI051.Free;
  inherited;
end;

{ TRegistroI050List }

function TRegistroI050List.GetItem(Index: Integer): TRegistroI050;
begin
  Result := TRegistroI050(Inherited Items[Index]);
end;

function TRegistroI050List.New: TRegistroI050;
begin
  Result := TRegistroI050.Create;
  Add(Result);
end;

procedure TRegistroI050List.SetItem(Index: Integer; const Value: TRegistroI050);
begin
  Put(Index, Value);
end;

{ TRegistroI051List }

function TRegistroI051List.GetItem(Index: Integer): TRegistroI051;
begin
  Result := TRegistroI051(Inherited Items[Index]);
end;

function TRegistroI051List.New: TRegistroI051;
begin
  Result := TRegistroI051.Create;
  Add(Result);
end;

procedure TRegistroI051List.SetItem(Index: Integer; const Value: TRegistroI051);
begin
  Put(Index, Value);
end;

{ TRegistroI075List }

function TRegistroI075List.GetItem(Index: Integer): TRegistroI075;
begin
  Result := TRegistroI075(Inherited Items[Index]);
end;

function TRegistroI075List.New: TRegistroI075;
begin
  Result := TRegistroI075.Create;
  Add(Result);
end;

procedure TRegistroI075List.SetItem(Index: Integer; const Value: TRegistroI075);
begin
  Put(Index, Value);
end;

{ TRegistroI100List }

function TRegistroI100List.GetItem(Index: Integer): TRegistroI100;
begin
  Result := TRegistroI100(Inherited Items[Index]);
end;

function TRegistroI100List.New: TRegistroI100;
begin
  Result := TRegistroI100.Create;
  Add(Result);
end;

procedure TRegistroI100List.SetItem(Index: Integer; const Value: TRegistroI100);
begin
  Put(Index, Value);
end;

constructor TRegistroI150.Create;
begin
   FRegistroI155 := TRegistroI155List.Create;
end;

destructor TRegistroI150.Destroy;
begin
  FRegistroI155.Free;
  inherited;
end;

{ TRegistroI150List }

function TRegistroI150List.GetItem(Index: Integer): TRegistroI150;
begin
  Result := TRegistroI150(Inherited Items[Index]);
end;

function TRegistroI150List.New: TRegistroI150;
begin
  Result := TRegistroI150.Create;
  Add(Result);
end;

procedure TRegistroI150List.SetItem(Index: Integer; const Value: TRegistroI150);
begin
  Put(Index, Value);
end;


constructor TRegistroI155.Create;
begin
   FRegistroI156 := TRegistroI156List.Create;
end;

destructor TRegistroI155.Destroy;
begin
  FRegistroI156.Free;
  inherited;
end;


{ TRegistroI155List }

function TRegistroI155List.GetItem(Index: Integer): TRegistroI155;
begin
  Result := TRegistroI155(Inherited Items[Index]);
end;

function TRegistroI155List.New: TRegistroI155;
begin
  Result := TRegistroI155.Create;
  Add(Result);
end;

procedure TRegistroI155List.SetItem(Index: Integer; const Value: TRegistroI155);
begin
  Put(Index, Value);
end;


{ TRegistroI156List }

function TRegistroI156List.GetItem(Index: Integer): TRegistroI156;
begin
  Result := TRegistroI156(Inherited Items[Index]);
end;

function TRegistroI156List.New: TRegistroI156;
begin
  Result := TRegistroI156.Create;
  Add(Result);
end;

procedure TRegistroI156List.SetItem(Index: Integer; const Value: TRegistroI156);
begin
  Put(Index, Value);
end;



// TRegistroI200

constructor TRegistroI200.Create;
begin
  FRegistroI250 := TRegistroI250List.create;
end;

destructor TRegistroI200.Destroy;
begin
  FRegistroI250.Free;
  inherited;
end;

// TRegistroI200List

function TRegistroI200List.GetItem(Index: Integer): TRegistroI200;
begin
  Result := TRegistroI200(Inherited Items[Index]);
end;

function TRegistroI200List.New: TRegistroI200;
begin
  Result := TRegistroI200.Create;
  Add(Result);
end;

procedure TRegistroI200List.SetItem(Index: Integer; Value: TRegistroI200);
begin
  Put(Index, Value);
end;


// TRegistroI250

constructor TRegistroI250.Create;
begin
  FRegistroI256 := TRegistroI256List.create;
end;

destructor TRegistroI250.Destroy;
begin
  FRegistroI256.Free;
  inherited;
end;


// TRegistroI250List

function TRegistroI250List.GetItem(index: Integer): TRegistroI250;
begin
  Result := TRegistroI250( inherited Items[Index]);
end;

function TRegistroI250List.New: TRegistroI250;
begin
   Result := TRegistroI250.Create;
   Add(Result);
end;

procedure TRegistroI250List.SetItem(Index: Integer; Value: TRegistroI250);
begin
   Put(Index, Value);
end;


// TRegistroI256List

function TRegistroI256List.GetItem(index: Integer): TRegistroI256;
begin
  Result := TRegistroI256( inherited Items[Index]);
end;

function TRegistroI256List.New: TRegistroI256;
begin
   Result := TRegistroI256.Create;
   Add(Result);
end;

procedure TRegistroI256List.SetItem(Index: Integer; Value: TRegistroI256);
begin
   Put(Index, Value);
end;


constructor TRegistroI350.Create;
begin
   FRegistroI355 := TRegistroI355List.Create;
end;

destructor TRegistroI350.Destroy;
begin
  FRegistroI355.Free;
  inherited;
end;

{ TRegistroI350List }

function TRegistroI350List.GetItem(Index: Integer): TRegistroI350;
begin
  Result := TRegistroI350(Inherited Items[Index]);
end;

function TRegistroI350List.New: TRegistroI350;
begin
  Result := TRegistroI350.Create;
  Add(Result);
end;

procedure TRegistroI350List.SetItem(Index: Integer; const Value: TRegistroI350);
begin
  Put(Index, Value);
end;


constructor TRegistroI355.Create;
begin
   FRegistroI356 := TRegistroI356List.Create;
end;

destructor TRegistroI355.Destroy;
begin
  FRegistroI356.Free;
  inherited;
end;


{ TRegistroI355List }

function TRegistroI355List.GetItem(Index: Integer): TRegistroI355;
begin
  Result := TRegistroI355(Inherited Items[Index]);
end;

function TRegistroI355List.New: TRegistroI355;
begin
  Result := TRegistroI355.Create;
  Add(Result);
end;

procedure TRegistroI355List.SetItem(Index: Integer; const Value: TRegistroI355);
begin
  Put(Index, Value);
end;

{ TRegistroI356List }

function TRegistroI356List.GetItem(Index: Integer): TRegistroI356;
begin
  Result := TRegistroI356(Inherited Items[Index]);
end;

function TRegistroI356List.New: TRegistroI356;
begin
  Result := TRegistroI356.Create;
  Add(Result);
end;

procedure TRegistroI356List.SetItem(Index: Integer; const Value: TRegistroI356);
begin
  Put(Index, Value);
end;



end.
