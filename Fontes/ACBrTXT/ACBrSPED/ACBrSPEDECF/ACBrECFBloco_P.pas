{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
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
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_P;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroP030List = class;

  /// Registro P001 - Abertura do Bloco P – Lucro Presumido
  TRegistroP001 = class(TOpenBlocos)
  private
    FRegistroP030         : TRegistroP030List; // NIVEL 2
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RegistroP030: TRegistroP030List read FRegistroP030 write FRegistroP030;
  end;

  /// Registro P030 - Identificação dos Período e Forma de Apuração do
  /// IRPJ e da CSLL das Empresas Tributadas pelo
  /// Lucro Presumido

  TRegistroP100List = class;
  TRegistroP130List = class;
  TRegistroP150List = class;
  TRegistroP200List = class;
  TRegistroP230List = class;
  TRegistroP300List = class;
  TRegistroP400List = class;
  TRegistroP500List = class;

  { TRegistroP030 }

  TRegistroP030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    fPER_APUR: string;
    
    FRegistroP100: TRegistroP100List;
    FRegistroP130: TRegistroP130List;
    FRegistroP150: TRegistroP150List;
    FRegistroP200: TRegistroP200List;
    FRegistroP230: TRegistroP230List;
    FRegistroP300: TRegistroP300List;
    FRegistroP400: TRegistroP400List;
    FRegistroP500: TRegistroP500List;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    // registros filhos
    property RegistroP100: TRegistroP100List read FRegistroP100 write FRegistroP100;
    property RegistroP130: TRegistroP130List read FRegistroP130 write FRegistroP130;
    property RegistroP150: TRegistroP150List read FRegistroP150 write FRegistroP150;
    property RegistroP200: TRegistroP200List read FRegistroP200 write FRegistroP200;
    property RegistroP230: TRegistroP230List read FRegistroP230 write FRegistroP230;
    property RegistroP300: TRegistroP300List read FRegistroP300 write FRegistroP300;
    property RegistroP400: TRegistroP400List read FRegistroP400 write FRegistroP400;
    property RegistroP500: TRegistroP500List read FRegistroP500 write FRegistroP500;
  end;

  /// Registro P030 - Lista

  TRegistroP030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP030;
    procedure SetItem(Index: Integer; const Value: TRegistroP030);
  public
    function New(AOwner: TRegistroP001): TRegistroP030;
    property Items[Index: Integer]: TRegistroP030 read GetItem write SetItem;
  end;

  /// Registro P100 - Balanço Patrimonial

  { TRegistroP100 }

  TRegistroP100 = class(TBlocos)
  private
    fCODIGO: string;
    fCOD_CTA_SUP: string;
    fCOD_NAT: string;
    fDESCRICAO: string;
    fIND_VAL_CTA_REF_FIN: string;
    fIND_VAL_CTA_REF_INI: string;
    fNIVEL: integer;
    fTIPO:  string;
    fVAL_CTA_REF_FIN: variant;
    fVAL_CTA_REF_INI: variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VAL_CTA_REF_INI: variant read fVAL_CTA_REF_INI write fVAL_CTA_REF_INI;
    property IND_VAL_CTA_REF_INI: string read fIND_VAL_CTA_REF_INI write fIND_VAL_CTA_REF_INI;
    property VAL_CTA_REF_FIN: variant read fVAL_CTA_REF_FIN write fVAL_CTA_REF_FIN;
    property IND_VAL_CTA_REF_FIN: string read fIND_VAL_CTA_REF_FIN write fIND_VAL_CTA_REF_FIN;
  end;

  /// Registro P100 - Lista

  TRegistroP100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP100;
    procedure SetItem(Index: Integer; const Value: TRegistroP100);
  public
    function New: TRegistroP100;
    property Items[Index: Integer]: TRegistroP100 read GetItem write SetItem;
  end;

  /// Registro P130 - Demonstração das Receitas Incentivadas do Lucro Presumido

  { TRegistroP130 }

  TRegistroP130 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P130 - Lista

  TRegistroP130List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP130;
    procedure SetItem(Index: Integer; const Value: TRegistroP130);
  public
    function New: TRegistroP130;
    property Items[Index: Integer]: TRegistroP130 read GetItem write SetItem;
  end;

  /// Registro P150 - Demonstração do Resultado

  { TRegistroP150 }

  TRegistroP150 = class(TBlocos)
  private
    fCODIGO: string;
    fCOD_CTA_SUP: string;
    fCOD_NAT: string;
    fDESCRICAO: string;
    fIND_VALOR: string;
    fNIVEL: integer;
    fTIPO:  string;
    fVALOR: variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VALOR: variant read fVALOR write fVALOR;
    property IND_VALOR: string read fIND_VALOR write fIND_VALOR;

  end;

  /// Registro P150 - Lista

  TRegistroP150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP150;
    procedure SetItem(Index: Integer; const Value: TRegistroP150);
  public
    function New: TRegistroP150;
    property Items[Index: Integer]: TRegistroP150 read GetItem write SetItem;
  end;

  /// Registro P200 - Apuração da Base de Cálculo do Lucro Presumido

  { TRegistroP200 }

  TRegistroP200 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P200 - Lista

  TRegistroP200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP200;
    procedure SetItem(Index: Integer; const Value: TRegistroP200);
  public
    function New: TRegistroP200;
    property Items[Index: Integer]: TRegistroP200 read GetItem write SetItem;
  end;

  /// Registro P230 - Cálculo da Isenção e Redução do Lucro Presumido

  { TRegistroP230 }

  TRegistroP230 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P230 - Lista

  TRegistroP230List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP230;
    procedure SetItem(Index: Integer; const Value: TRegistroP230);
  public
    function New: TRegistroP230;
    property Items[Index: Integer]: TRegistroP230 read GetItem write SetItem;
  end;

  /// Registro P300 - Cálculo do IRPJ com Base no Lucro Presumido

  { TRegistroP300 }

  TRegistroP300 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P300 - Lista

  TRegistroP300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP300;
    procedure SetItem(Index: Integer; const Value: TRegistroP300);
  public
    function New: TRegistroP300;
    property Items[Index: Integer]: TRegistroP300 read GetItem write SetItem;
  end;

  /// Registro P400 - Apuração da Base de Cálculo da CSLL com Base no Lucro Presumido

  { TRegistroP400 }

  TRegistroP400 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P400 - Lista

  TRegistroP400List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP400;
    procedure SetItem(Index: Integer; const Value: TRegistroP400);
  public
    function New: TRegistroP400;
    property Items[Index: Integer]: TRegistroP400 read GetItem write SetItem;
  end;

  /// Registro P500 - Cálculo da CSLL com Base no Lucro Líquido

  { TRegistroP500 }

  TRegistroP500 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro P500 - Lista

  TRegistroP500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP500;
    procedure SetItem(Index: Integer; const Value: TRegistroP500);
  public
    function New: TRegistroP500;
    property Items[Index: Integer]: TRegistroP500 read GetItem write SetItem;
  end;

  /// Registro P990 - ENCERRAMENTO DO BLOCO P
  TRegistroP990 = class(TCloseBlocos)
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;

implementation

{ TRegistroP030List }

function TRegistroP030List.GetItem(Index: Integer): TRegistroP030;
begin
   Result := TRegistroP030(Inherited Items[Index]);
end;

function TRegistroP030List.New(AOwner: TRegistroP001): TRegistroP030;
begin
  Result := TRegistroP030.Create;
  Add(Result);
end;

procedure TRegistroP030List.SetItem(Index: Integer; const Value: TRegistroP030);
begin
  Put(Index, Value);
end;

{ TRegistroP100List }

function TRegistroP100List.GetItem(Index: Integer): TRegistroP100;
begin
   Result := TRegistroP100(Inherited Items[Index]);
end;

function TRegistroP100List.New: TRegistroP100;
begin
  Result := TRegistroP100.Create;
  Add(Result);
end;

procedure TRegistroP100List.SetItem(Index: Integer; const Value: TRegistroP100);
begin
  Put(Index, Value);
end;

{ TRegistroP130List }

function TRegistroP130List.GetItem(Index: Integer): TRegistroP130;
begin
   Result := TRegistroP130(Inherited Items[Index]);
end;

function TRegistroP130List.New: TRegistroP130;
begin
  Result := TRegistroP130.Create;
  Add(Result);
end;

procedure TRegistroP130List.SetItem(Index: Integer; const Value: TRegistroP130);
begin
  Put(Index, Value);
end;


{ TRegistroP150List }

function TRegistroP150List.GetItem(Index: Integer): TRegistroP150;
begin
   Result := TRegistroP150(Inherited Items[Index]);
end;

function TRegistroP150List.New: TRegistroP150;
begin
  Result := TRegistroP150.Create;
  Add(Result);
end;

procedure TRegistroP150List.SetItem(Index: Integer; const Value: TRegistroP150);
begin
  Put(Index, Value);
end;

{ TRegistroP200List }

function TRegistroP200List.GetItem(Index: Integer): TRegistroP200;
begin
   Result := TRegistroP200(Inherited Items[Index]);
end;

function TRegistroP200List.New: TRegistroP200;
begin
  Result := TRegistroP200.Create;
  Add(Result);
end;

procedure TRegistroP200List.SetItem(Index: Integer; const Value: TRegistroP200);
begin
  Put(Index, Value);
end;


{ TRegistroP230List }

function TRegistroP230List.GetItem(Index: Integer): TRegistroP230;
begin
   Result := TRegistroP230(Inherited Items[Index]);
end;

function TRegistroP230List.New: TRegistroP230;
begin
  Result := TRegistroP230.Create;
  Add(Result);
end;

procedure TRegistroP230List.SetItem(Index: Integer; const Value: TRegistroP230);
begin
  Put(Index, Value);
end;

{ TRegistroP300List }

function TRegistroP300List.GetItem(Index: Integer): TRegistroP300;
begin
   Result := TRegistroP300(Inherited Items[Index]);
end;

function TRegistroP300List.New: TRegistroP300;
begin
  Result := TRegistroP300.Create;
  Add(Result);
end;

procedure TRegistroP300List.SetItem(Index: Integer; const Value: TRegistroP300);
begin
  Put(Index, Value);
end;

{ TRegistroP400List }

function TRegistroP400List.GetItem(Index: Integer): TRegistroP400;
begin
   Result := TRegistroP400(Inherited Items[Index]);
end;

function TRegistroP400List.New: TRegistroP400;
begin
  Result := TRegistroP400.Create;
  Add(Result);
end;

procedure TRegistroP400List.SetItem(Index: Integer; const Value: TRegistroP400);
begin
  Put(Index, Value);
end;

{ TRegistroP500List }

function TRegistroP500List.GetItem(Index: Integer): TRegistroP500;
begin
   Result := TRegistroP500(Inherited Items[Index]);
end;

function TRegistroP500List.New: TRegistroP500;
begin
  Result := TRegistroP500.Create;
  Add(Result);
end;

procedure TRegistroP500List.SetItem(Index: Integer; const Value: TRegistroP500);
begin
  Put(Index, Value);
end;

{ TRegistroP030 }

constructor TRegistroP030.Create;
begin
   inherited;

   FRegistroP100 := TRegistroP100List.Create;
   FRegistroP130 := TRegistroP130List.Create;
   FRegistroP150 := TRegistroP150List.Create;
   FRegistroP200 := TRegistroP200List.Create;
   FRegistroP230 := TRegistroP230List.Create;
   FRegistroP300 := TRegistroP300List.Create;
   FRegistroP400 := TRegistroP400List.Create;
   FRegistroP500 := TRegistroP500List.Create;
end;

destructor TRegistroP030.Destroy;
begin
   FRegistroP500.Free;
   FRegistroP400.Free;
   FRegistroP300.Free;
   FRegistroP230.Free;
   FRegistroP200.Free;
   FRegistroP150.Free;
   FRegistroP130.Free;
   FRegistroP100.Free;
   inherited;
end;

{ TRegistroP001 }

constructor TRegistroP001.Create;
begin
  inherited Create;
  fRegistroP030 := TRegistroP030List.Create;
end;

destructor TRegistroP001.Destroy;
begin
  fRegistroP030.Free;
  inherited;
end;

end.
