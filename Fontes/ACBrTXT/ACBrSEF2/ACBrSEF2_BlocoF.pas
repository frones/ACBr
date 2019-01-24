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

{$I ACBr.inc}

unit ACBrSEF2_BlocoF;

interface

uses
  Classes, Contnrs, SysUtils, ACBrSEF2Conversao;

type
  TRegistroSEFF200List = class;
  TRegistroSEFF205List = class;
  TRegistroSEFF210List = class;
  TRegistroSEFF215List = class;
  TRegistroSEFF220List = class;
  TRegistroSEFF225List = class;
  TRegistroSEFF230List = class;

  //LINHA F001: ABERTURA DO BLOCO F
  TRegistroSEFF001 = class
  private
    fIND_DAD: TSEFIIIndicadorConteudo;
    fRegistroF200: TRegistroSEFF200List;
    fRegistroF205: TRegistroSEFF205List;
    fRegistroF210: TRegistroSEFF210List;
    fRegistroF215: TRegistroSEFF215List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property IND_DAD: TSEFIIIndicadorConteudo read fIND_DAD write fIND_DAD;
    property RegistroF200: TRegistroSEFF200List read fRegistroF200 write fRegistroF200;
    property RegistroF205: TRegistroSEFF205List read fRegistroF205 write fRegistroF205;
    property RegistroF210: TRegistroSEFF210List read fRegistroF210 write fRegistroF210;
    property RegistroF215: TRegistroSEFF215List read fRegistroF215 write fRegistroF215;
  end;

  // LIVRO DE MOVIMENTAÇÃO DE COMBUSTIVEIS
  TRegistroSEFF200 = class
  private
    FIND_LMC: integer;
    FDT_INI: TDateTime;
    FDT_FIN: TDateTime;
  public
    property IND_LMC: integer read FIND_LMC write FIND_LMC; //Indicador de conteúdo:
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
  end;

  TRegistroSEFF200List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF200;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF200);
  public
    function New(): TRegistroSEFF200;
    property notas[Index: integer]: TRegistroSEFF200 read Getnotas write SetNotas;
  end;

  // CADASTRO DAS BOMBAS
  TRegistroSEFF205 = class
  private
    FBOMBA: string;
    FEQP_FAB: string;
    FEQP_CNPJ: string;
    FEQP_MOD: string;
    FIND_BOMB: integer;
  public
    property BOMBA: string read FBOMBA write FBOMBA;
    property EQP_FAB: string read FEQP_FAB write FEQP_FAB;
    property EQP_CNPJ: string read FEQP_CNPJ write FEQP_CNPJ;
    property EQP_MOD: string read FEQP_MOD write FEQP_MOD;
    property IND_BOMB: Integer read FIND_BOMB write FIND_BOMB;
  end;

  TRegistroSEFF205List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF205;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF205);
  public
    function New(): TRegistroSEFF205;
    property notas[Index: integer]: TRegistroSEFF205 read Getnotas write SetNotas;
  end;

  // COMBUSTÍVEL COMERCIALIZADO
  TRegistroSEFF210 = class
  private
    FCOD_ITEM: string ;
    FCOD_INF_OBS: string;
    FRegistroF215: TRegistroSEFF215List;
  public
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property COD_INF_OBS: string read FCOD_INF_OBS write FCOD_INF_OBS;
    property RegistroF215: TRegistroSEFF215List read FRegistroF215 write FRegistroF215;
  end;

  TRegistroSEFF210List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF210;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF210);
  public
    function New(): TRegistroSEFF210;
    property notas[Index: integer]: TRegistroSEFF210 read Getnotas write SetNotas;
  end;

  // MOVIMENTAÇÃO
  TRegistroSEFF215 = class
  private
    FIND_TOT: Integer;
    FDT_MOV: TDateTime;
    FESTQ_INI: double;
    FENTRADAS: double;
    FSAIDAS: double;
    FESTQ_FIN: double;
    FESTQ_FECH: double;
    FPERDAS: double;
    FSOBRAS: double;
    FVL_UNIT: double;
    FVL_SAID: double;
    FRegistroF220List: TRegistroSEFF220List;
    FRegistroF230List: TRegistroSEFF230List;
  public
    property IND_TOT: integer read FIND_TOT write FIND_TOT;
    property DT_MOV: TDateTime read FDT_MOV write FDT_MOV;
    property ESTQ_INI: double read FESTQ_INI write FESTQ_INI;
    property ENTRADAS: double read FENTRADAS write FENTRADAS;
    property SAIDAS: double read FSAIDAS write FSAIDAS;
    property ESTQ_FIN: double read FESTQ_FIN write FESTQ_FIN;
    property ESTQ_FECH: double read FESTQ_FECH write FESTQ_FECH;
    property PERDAS: double read FPERDAS write FPERDAS;
    property SOBRAS: double read FSOBRAS write FSOBRAS;
    property VL_UNIT: double read FVL_UNIT write FVL_UNIT;
    property VL_SAID: double read FVL_SAID write FVL_SAID;

    property RegistroF220: TRegistroSEFF220List read FRegistroF220List write FRegistroF220List;
    property RegistroF230: TRegistroSEFF230List read FRegistroF230List write FRegistroF230List;
  end;

  TRegistroSEFF215List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF215;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF215);
  public
    function New(): TRegistroSEFF215;
    property notas[Index: integer]: TRegistroSEFF215 read Getnotas write SetNotas;
  end;

  // CADASTRO TANQUE
  TRegistroSEFF220 = class
  private
    FTANQUE: string;
    FVOL_INI: Double;
    FVOL_ENTR: Double;
    FVOL_SAID: Double;
    FVOL_FECH: Double;
    FVOL_PRD: Double;
    FVOL_SBR: Double;
    FRegistroF225List: TRegistroSEFF225List;
  public
    property TANQUE: string read FTANQUE write FTANQUE;
    property VOL_INI: Double read FVOL_INI write FVOL_INI;
    property VOL_ENTR: Double read FVOL_ENTR write FVOL_ENTR;
    property VOL_SAID: Double read FVOL_SAID write FVOL_SAID;
    property VOL_FECH: Double read FVOL_FECH write FVOL_FECH;
    property VOL_PRD: Double read FVOL_PRD write FVOL_PRD;
    property VOL_SBR: Double read FVOL_SBR write FVOL_SBR;
    property RegistroF225: TRegistroSEFF225List read FRegistroF225List write FRegistroF225List;
  end;

  TRegistroSEFF220List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF220;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF220);
  public
    function New(): TRegistroSEFF220;
    property notas[Index: integer]: TRegistroSEFF220 read Getnotas write SetNotas;
  end;

  // NOTAS FISCAIS
  TRegistroSEFF225 = class
  private
    FIND_OPER: Integer;
    FIND_EMIT: String;
    FCNPJ: String;
    FUF: string;
    FIE: string;
    FCOD_MOD: TSEFIIDocFiscalReferenciado;
    FSER: string;
    FNUM_DOC: Integer;
    FDT_DOC: TDateTime;
    FVL_DOC: Double;
    FVL_ICMS: Double;
    FVL_ICMS_ST: Double;
    FVOL: Double;
    FCOD_INF_OBS: string;
  public
    property IND_OPER   : Integer read FIND_OPER write FIND_OPER;
    property IND_EMIT   : String read FIND_EMIT write FIND_EMIT;
    property CNPJ       : String read FCNPJ write FCNPJ;
    property UF         : string read FUF write FUF;
    property IE         : string read FIE write FIE;
    property COD_MOD    : TSEFIIDocFiscalReferenciado read FCOD_MOD write FCOD_MOD;
    property SER        : string read FSER write FSER;
    property NUM_DOC    : Integer read FNUM_DOC write FNUM_DOC;
    property DT_DOC     : TDateTime read FDT_DOC write FDT_DOC;
    property VL_DOC     : Double read FVL_DOC write FVL_DOC;
    property VL_ICMS    : Double read FVL_ICMS write FVL_ICMS;
    property VL_ICMS_ST : Double read FVL_ICMS_ST write FVL_ICMS_ST;
    property VOL        : Double read FVOL write FVOL;
    property COD_INF_OBS: string read FCOD_INF_OBS write FCOD_INF_OBS;
  end;

  TRegistroSEFF225List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF225;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF225);
  public
    function New(): TRegistroSEFF225;
    property notas[Index: integer]: TRegistroSEFF225 read Getnotas write SetNotas;
  end;

  // ENCERRANTE
  TRegistroSEFF230 = class
  private
    FBOMBA: string;
    FBICO: string;
    FENC_FIN: Double;
    FENC_INI: Double;
    FVOL_AFER: Double;
  public
    property BOMBA: string read FBOMBA write FBOMBA;
    property BICO: string read FBICO write FBICO;
    property ENC_FIN: Double read FENC_FIN write FENC_FIN;
    property ENC_INI: Double read FENC_INI write FENC_INI;
    property VOL_AFER: Double read FVOL_AFER write FVOL_AFER;
  end;

  TRegistroSEFF230List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFF230;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFF230);
  public
    function New(): TRegistroSEFF230;
    property notas[Index: integer]: TRegistroSEFF230 read Getnotas write SetNotas;
  end;

  /// Registro F990 - Encerramento do Bloco F
  TRegistroSEFF990 = class
  private
    fQTD_LIN_F: integer;
  public
    property QTD_LIN_F: integer read fQTD_LIN_F write fQTD_LIN_F;
  end;

implementation

constructor TRegistroSEFF001.Create;
begin
  fRegistroF200 := TRegistroSEFF200List.Create;
  fRegistroF205 := TRegistroSEFF205List.Create;
  fRegistroF210 := TRegistroSEFF210List.Create;
  fRegistroF215 := TRegistroSEFF215List.Create;
  IND_DAD       := icSemConteudo;
end;

destructor TRegistroSEFF001.Destroy;
begin
  fRegistroF200.Free;
  fRegistroF205.Free;
  fRegistroF210.Free;
  fRegistroF215.Free;
  inherited;
end;

{ TRegistroSEFF200List }

procedure TRegistroSEFF200List.SetNotas(Index: integer; const Value: TRegistroSEFF200);
begin
  Put(Index, Value);
end;

function TRegistroSEFF200List.GetNotas(Index: integer): TRegistroSEFF200;
begin
  Result := TRegistroSEFF200(Get(Index));
end;

function TRegistroSEFF200List.New(): TRegistroSEFF200;
begin
  Result := TRegistroSEFF200.Create;
  Add(Result);
end;

{ TRegistroSEFF205List }

procedure TRegistroSEFF205List.SetNotas(Index: integer; const Value: TRegistroSEFF205);
begin
  Put(Index, Value);
end;

function TRegistroSEFF205List.GetNotas(Index: integer): TRegistroSEFF205;
begin
  Result := TRegistroSEFF205(Get(Index));
end;

function TRegistroSEFF205List.New(): TRegistroSEFF205;
begin
  Result := TRegistroSEFF205.Create;
  Add(Result);
end;

{ TRegistroSEFF210List }

procedure TRegistroSEFF210List.SetNotas(Index: integer; const Value: TRegistroSEFF210);
begin
  Put(Index, Value);
end;

function TRegistroSEFF210List.GetNotas(Index: integer): TRegistroSEFF210;
begin
  Result := TRegistroSEFF210(Get(Index));
end;

function TRegistroSEFF210List.New(): TRegistroSEFF210;
begin
  Result := TRegistroSEFF210.Create;
  Add(Result);
end;

{ TRegistroSEFF215List }

procedure TRegistroSEFF215List.SetNotas(Index: integer; const Value: TRegistroSEFF215);
begin
  Put(Index, Value);
end;

function TRegistroSEFF215List.GetNotas(Index: integer): TRegistroSEFF215;
begin
  Result := TRegistroSEFF215(Get(Index));
end;

function TRegistroSEFF215List.New(): TRegistroSEFF215;
begin
  Result := TRegistroSEFF215.Create;
  Add(Result);
end;

{ TRegistroSEFF220List }

function TRegistroSEFF220List.GetNotas(Index: integer): TRegistroSEFF220;
begin
  Result := TRegistroSEFF220(Get(Index));
end;

function TRegistroSEFF220List.New(): TRegistroSEFF220;
begin
  Result := TRegistroSEFF220.Create;
  Add(Result);
end;

procedure TRegistroSEFF220List.SetNotas(Index: integer; const Value: TRegistroSEFF220);
begin
  Put(Index, Value);
end;

{ TRegistroSEFF230List }

function TRegistroSEFF230List.GetNotas(Index: integer): TRegistroSEFF230;
begin
  Result := TRegistroSEFF230(Get(Index));
end;

function TRegistroSEFF230List.New(): TRegistroSEFF230;
begin
  Result := TRegistroSEFF230.Create;
  Add(Result);
end;

procedure TRegistroSEFF230List.SetNotas(Index: integer; const Value: TRegistroSEFF230);
begin
  Put(Index, Value);
end;

{ TRegistroSEFF225List }

function TRegistroSEFF225List.GetNotas(Index: integer): TRegistroSEFF225;
begin
  Result := TRegistroSEFF225(Get(Index));
end;

function TRegistroSEFF225List.New(): TRegistroSEFF225;
begin
  Result := TRegistroSEFF225.Create;
  Add(Result);
end;

procedure TRegistroSEFF225List.SetNotas(Index: integer; const Value: TRegistroSEFF225);
begin
  Put(Index, Value);
end;

end.
