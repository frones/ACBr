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

unit ACBrECFBloco_L;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro L001 - Abertura do Bloco L – Lucro Real
  TRegistroL001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroL100List = class;
  TRegistroL200List = class;
  TRegistroL210List = class;
  TRegistroL300List = class;

  /// Registro L030 - Identificação dos Períodos e Formas de Apuração do
  /// IRPJ e da CSLL no Ano-Calendário
  TRegistroL030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fPER_APUR: string;
    fDT_INI:   TDateTime;

    FRegistroL200: TRegistroL200List;
    FRegistroL300: TRegistroL300List;
    FRegistroL210: TRegistroL210List;
    FRegistroL100: TRegistroL100List;
  public
    constructor Create; /// Create
    destructor Destroy; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    /// Registros FILHOS
    property RegistroL100: TRegistroL100List read FRegistroL100 write FRegistroL100;
    property RegistroL200: TRegistroL200List read FRegistroL200 write FRegistroL200;
    property RegistroL210: TRegistroL210List read FRegistroL210 write FRegistroL210;
    property RegistroL300: TRegistroL300List read FRegistroL300 write FRegistroL300;
  end;

  TRegistroL030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL030;
    procedure SetItem(Index: Integer; const Value: TRegistroL030);
  public
    function New: TRegistroL030;
    property Items[Index: Integer]: TRegistroL030 read GetItem write SetItem;
  end;

  /// Registro L100 - Balanço Patrimonial

  { TRegistroL100 }

  TRegistroL100 = class(TBlocos)
  private
    fIND_VAL_CTA_REF_FIN: string;
    fIND_VAL_CTA_REF_INI: string;
    fVALOR_SALDO_INICIAL: Currency;
    fDESCRICAO: string;
    fCOD_CTA_SUP: string;
    fCOD_NAT: string;
    fNIVEL:  integer;
    fCODIGO: string;
    fTIPO:   string;
    fVAL_CTA_REF_FIN: Currency;
    fVAL_CTA_REF_INI: Currency;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VAL_CTA_REF_INI: Currency read fVAL_CTA_REF_INI write fVAL_CTA_REF_INI;
    property IND_VAL_CTA_REF_INI: string read fIND_VAL_CTA_REF_INI write fIND_VAL_CTA_REF_INI;
    property VAL_CTA_REF_FIN: Currency read fVAL_CTA_REF_FIN write fVAL_CTA_REF_FIN;
    property IND_VAL_CTA_REF_FIN: string read fIND_VAL_CTA_REF_FIN write fIND_VAL_CTA_REF_FIN;
  end;

  TRegistroL100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL100;
    procedure SetItem(Index: Integer; const Value: TRegistroL100);
  public
    function New: TRegistroL100;
    property Items[Index: Integer]: TRegistroL100 read GetItem write SetItem;
  end;

  /// Registro L200 - Método de Avaliação do Estoque Final
  TRegistroL200 = class(TBlocos)
  private
    fIND_AVAL_ESTOQ: string;
  public
    property IND_AVAL_ESTOQ: string read fIND_AVAL_ESTOQ write fIND_AVAL_ESTOQ;
  end;

  TRegistroL200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL200;
    procedure SetItem(Index: Integer; const Value: TRegistroL200);
  public
    function New: TRegistroL200;
    property Items[Index: Integer]: TRegistroL200 read GetItem write SetItem;
  end;

  /// Registro L210 - Informativo da Composição de Custos
  TRegistroL210 = class(TBlocos)
  private
    fCODIGO:    string;
    fVALOR:     Currency;
    fDESCRICAO: string;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: Currency read fVALOR write fVALOR;
  end;

  TRegistroL210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL210;
    procedure SetItem(Index: Integer; const Value: TRegistroL210);
  public
    function New: TRegistroL210;
    property Items[Index: Integer]: TRegistroL210 read GetItem write SetItem;
  end;

  /// Registro L300 - Demonstração do Resultado do Exercício
  TRegistroL300 = class(TBlocos)
  private
    fIND_VALOR: string;
    fDESCRICAO: string;
    fCOD_CTA_SUP: string;
    fVALOR:   Currency;
    fCOD_NAT: string;
    fNIVEL:   integer;
    fCODIGO:  string;
    fTIPO:    string;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VALOR: Currency read fVALOR write fVALOR;
    property IND_VALOR: string read fIND_VALOR write fIND_VALOR;
  end;

  TRegistroL300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL300;
    procedure SetItem(Index: Integer; const Value: TRegistroL300);
  public
    function New: TRegistroL300;
    property Items[Index: Integer]: TRegistroL300 read GetItem write SetItem;
  end;

  /// Registro L990 - ENCERRAMENTO DO BLOCO L
  TRegistroL990 = class(TCloseBlocos)
  private
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;

implementation

{ TRegistroL030List }

function TRegistroL030List.GetItem(Index: Integer): TRegistroL030;
begin
  Result := TRegistroL030(Inherited Items[Index]);
end;

function TRegistroL030List.New: TRegistroL030;
begin
  Result := TRegistroL030.Create;
  Add(Result);
end;

procedure TRegistroL030List.SetItem(Index: Integer; const Value: TRegistroL030);
begin
  Put(Index, Value);
end;

{ TRegistroL210List }

function TRegistroL210List.GetItem(Index: Integer): TRegistroL210;
begin
  Result := TRegistroL210(Inherited Items[Index]);
end;

function TRegistroL210List.New: TRegistroL210;
begin
  Result := TRegistroL210.Create;
  Add(Result);
end;

procedure TRegistroL210List.SetItem(Index: Integer; const Value: TRegistroL210);
begin
  Put(Index, Value);
end;

{ TRegistroL300List }

function TRegistroL300List.GetItem(Index: Integer): TRegistroL300;
begin
  Result := TRegistroL300(Inherited Items[Index]);
end;

function TRegistroL300List.New: TRegistroL300;
begin
  Result := TRegistroL300.Create;
  Add(Result);
end;

procedure TRegistroL300List.SetItem(Index: Integer; const Value: TRegistroL300);
begin
  Put(Index, Value);
end;

{ TRegistroL200List }

function TRegistroL200List.GetItem(Index: Integer): TRegistroL200;
begin
  Result := TRegistroL200(Inherited Items[Index]);
end;

function TRegistroL200List.New: TRegistroL200;
begin
  Result := TRegistroL200.Create;
  Add(Result);
end;

procedure TRegistroL200List.SetItem(Index: Integer; const Value: TRegistroL200);
begin
  Put(Index, Value);
end;

{ TRegistroL100List }

function TRegistroL100List.GetItem(Index: Integer): TRegistroL100;
begin
  Result := TRegistroL100(Inherited Items[Index]);
end;

function TRegistroL100List.New: TRegistroL100;
begin
  Result := TRegistroL100.Create;
  Add(Result);
end;

procedure TRegistroL100List.SetItem(Index: Integer; const Value: TRegistroL100);
begin
  Put(Index, Value);
end;

{ TRegistroL030 }

constructor TRegistroL030.Create;
begin
   inherited;

   FRegistroL100 := TRegistroL100List.Create;
   FRegistroL200 := TRegistroL200List.Create;
   FRegistroL210 := TRegistroL210List.Create;
   FRegistroL300 := TRegistroL300List.Create;
end;

destructor TRegistroL030.Destroy;
begin
   FRegistroL300.Free;
   FRegistroL210.Free;
   FRegistroL200.Free;
   FRegistroL100.Free;

   inherited;
end;

end.
