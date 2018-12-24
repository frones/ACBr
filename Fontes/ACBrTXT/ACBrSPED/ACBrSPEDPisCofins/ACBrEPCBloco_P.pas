{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
|* 06/02/2012: Jeferson Rodrigo Stefani
|*  - Criação do Bloco P
|* 21/05/2012 : Edilson Alves de Oliveira
|*  - Continuação do bloco P. 
*******************************************************************************}

unit ACBrEPCBloco_P;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
  TRegistroP010List = class;
  TRegistroP100List = class;
  TRegistroP110List = class;
  TRegistroP199List = class;
  TRegistroP200List = class;
  TRegistroP210List = class;

  TRegistroP001 = class( TOpenBlocos )
  private
    fRegistroP010: TRegistroP010List;
    fRegistroP200: TRegistroP200List;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    property RegistroP010 : TRegistroP010List read fRegistroP010 write fRegistroP010;
    property RegistroP200 : TRegistroP200List read fRegistroP200 write fRegistroP200;
  end;

  TRegistroP010 = class
  private
    fREG          : String;
    fCNPJ         : String;
    fRegistroP100 : TRegistroP100List;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    property REG          : String            read fREG          write fREG;
    property CNPJ         : String            read fCNPJ         write fCNPJ;
    property RegistroP100 : TRegistroP100List read fRegistroP100 write fRegistroP100;
  end;

  TRegistroP010List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP010;
    procedure SetItem(Index: Integer; const Value: TRegistroP010);
  public
    function New: TRegistroP010;
    property Items[Index: Integer]: TRegistroP010 read GetItem write SetItem;
  end;

  TRegistroP100 = class
  private
    fREG               : String;
    fDT_INI            : TDateTime;
    fDT_FIM            : TDateTime;
    fVL_REC_TOT_EST    : Currency;
    fCOD_ATIV_ECON     : String;
    fVL_REC_ATIV_ESTAB : Currency;
    fVL_EXC            : Currency;
    fVL_BC_CONT        : Currency;
    fALIQ_CONT         : Currency;
    fVL_CONT_APU       : Currency;
    fCOD_CTA           : String;
    fINFO_COMPL        : String;
    fRegistroP110      : TRegistroP110List;
    fRegistroP199      : TRegistroP199List;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    property REG               : String            read fREG               write fREG;
    property DT_INI            : TDateTime         read fDT_INI            write fDT_INI;
    property DT_FIM            : TDateTime         read fDT_FIM            write fDT_FIM;
    property VL_REC_TOT_EST    : Currency          read fVL_REC_TOT_EST    write fVL_REC_TOT_EST;
    property COD_ATIV_ECON     : String            read fCOD_ATIV_ECON     write fCOD_ATIV_ECON;
    property VL_REC_ATIV_ESTAB : Currency          read fVL_REC_ATIV_ESTAB write fVL_REC_ATIV_ESTAB;
    property VL_EXC            : Currency          read fVL_EXC            write fVL_EXC;
    property VL_BC_CONT        : Currency          read fVL_BC_CONT        write fVL_BC_CONT;
    property ALIQ_CONT         : Currency          read fALIQ_CONT         write fALIQ_CONT;
    property VL_CONT_APU       : Currency          read fVL_CONT_APU       write fVL_CONT_APU;
    property COD_CTA           : String            read fCOD_CTA           write fCOD_CTA;
    property INFO_COMPL        : String            read fINFO_COMPL        write fINFO_COMPL;
    property RegistroP110      : TRegistroP110List read fRegistroP110      write fRegistroP110;
    property RegistroP199      : TRegistroP199List read fRegistroP199      write fRegistroP199;
  end;

  TRegistroP100List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP100;
    procedure SetItem(Index: Integer; const Value: TRegistroP100);
  public
    function New: TRegistroP100;
    property Items[Index: Integer]: TRegistroP100 read GetItem write SetItem;
  end;

  TRegistroP110 = class
  private
    fREG        : String;
    fNUM_CAMPO  : String;
    fCOD_DET    : String;
    fDET_VALOR  : Currency;
    fINF_COMPL  : String;
  public
    property REG        : String   read fREG       write fREG;
    property NUM_CAMPO  : String   read fNUM_CAMPO write fNUM_CAMPO;
    property COD_DET    : String   read fCOD_DET   write fCOD_DET;
    property DET_VALOR  : Currency read fDET_VALOR write fDET_VALOR;
    property INF_COMPL  : String   read fINF_COMPL write fINF_COMPL;
  end;

  TRegistroP110List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP110;
    procedure SetItem(Index: Integer; const Value: TRegistroP110);
  public
    function New: TRegistroP110;
    property Items[Index: Integer]: TRegistroP110 read GetItem write SetItem;
  end;

  TRegistroP199 = class
  private
    fREG      : String;
    fNUM_PROC : String;
    fIND_PROC : String;
  public
    property REG      : String read fREG      write fREG;
    property NUM_PROC : String read fNUM_PROC write fNUM_PROC;
    property IND_PROC : String read fIND_PROC write fIND_PROC;
  end;

  TRegistroP199List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP199;
    procedure SetItem(Index: Integer; const Value: TRegistroP199);
  public
    function New: TRegistroP199;
    property Items[Index: Integer]: TRegistroP199 read GetItem write SetItem;
  end;

  TRegistroP200 = class
  private
    fREG             : String;
    fPER_REF         : String;
    fVL_TOT_CONT_APU : Currency;
    fVL_TOT_AJ_REDUC : Currency;
    fVL_TOT_AJ_ACRES : Currency;
    fVL_TOT_CONT_DEV : Currency;
    fCOD_REC         : String;
    fRegistroP210    : TRegistroP210List;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    property REG             : String            read fREG             write fREG;
    property PER_REF         : String            read fPER_REF         write fPER_REF;
    property VL_TOT_CONT_APU : Currency          read fVL_TOT_CONT_APU write fVL_TOT_CONT_APU;
    property VL_TOT_AJ_REDUC : Currency          read fVL_TOT_AJ_REDUC write fVL_TOT_AJ_REDUC;
    property VL_TOT_AJ_ACRES : Currency          read fVL_TOT_AJ_ACRES write fVL_TOT_AJ_ACRES;
    property VL_TOT_CONT_DEV : Currency          read fVL_TOT_CONT_DEV write fVL_TOT_CONT_DEV;
    property COD_REC         : String            read fCOD_REC         write fCOD_REC;
    property RegistroP210    : TRegistroP210List read fRegistroP210    write fRegistroP210;
  end;

  TRegistroP200List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP200;
    procedure SetItem(Index: Integer; const Value: TRegistroP200);
  public
    function New: TRegistroP200;
    property Items[Index: Integer]: TRegistroP200 read GetItem write SetItem;
  end;

  TRegistroP210 = class
  private
    fREG      : String;
    fIND_AJ   : String;
    fVL_AJ    : Currency;
    fCOD_AJ   : String;
    fNUM_DOC  : String;
    fDESCR_AJ : String;
    fDT_REF   : TDateTime;
  public
    property REG      : String    read fREG      write fREG;
    property IND_AJ   : String    read fIND_AJ   write fIND_AJ;
    property VL_AJ    : Currency  read fVL_AJ    write fVL_AJ;
    property COD_AJ   : String    read fCOD_AJ   write fCOD_AJ;
    property NUM_DOC  : String    read fNUM_DOC  write fNUM_DOC;
    property DESCR_AJ : String    read fDESCR_AJ write fDESCR_AJ;
    property DT_REF   : TDateTime read fDT_REF   write fDT_REF;
  end;

  TRegistroP210List = class( TObjectList )
  private
    function  GetItem(Index: Integer): TRegistroP210;
    procedure SetItem(Index: Integer; const Value: TRegistroP210);
  public
    function New: TRegistroP210;
    property Items[Index: Integer]: TRegistroP210 read GetItem write SetItem;
  end;

  TRegistroP990 = class
  private
    fQTD_LIN_P : Integer;
  public
    property QTD_LIN_P : Integer read fQTD_LIN_P write fQTD_LIN_P;
  end;

implementation


{ TRegistroP001 }

constructor TRegistroP001.Create;
begin
  inherited;
  fRegistroP010 := TRegistroP010List.Create;
  fRegistroP200 := TRegistroP200List.Create;
end;

destructor TRegistroP001.Destroy;
begin
  fRegistroP010.Free;
  fRegistroP200.Free;
  inherited;
end;

{ TRegistroP010 }

constructor TRegistroP010.Create;
begin
  fRegistroP100 := TRegistroP100List.Create;
end;

destructor TRegistroP010.Destroy;
begin
  fRegistroP100.Free;
  inherited;
end;

{ TRegistroP010List }

function TRegistroP010List.GetItem(Index: Integer): TRegistroP010;
begin
  Result := TRegistroP010( inherited Items[Index] );
end;

function TRegistroP010List.New: TRegistroP010;
begin
  Result := TRegistroP010.Create;
  Add( Result );
end;

procedure TRegistroP010List.SetItem(Index: Integer;
  const Value: TRegistroP010);
begin
  Put( Index, Value );
end;

{ TRegistroP100List }

function TRegistroP100List.GetItem(Index: Integer): TRegistroP100;
begin
  Result := TRegistroP100( inherited Items[Index] );
end;

function TRegistroP100List.New: TRegistroP100;
begin
  Result := TRegistroP100.Create;
  Add( Result );
end;

procedure TRegistroP100List.SetItem(Index: Integer;
  const Value: TRegistroP100);
begin
  Put( Index, Value );
end;

{ TRegistroP110List }

function TRegistroP110List.GetItem(Index: Integer): TRegistroP110;
begin
  Result := TRegistroP110( inherited Items[index] );
end;

function TRegistroP110List.New: TRegistroP110;
begin
  Result := TRegistroP110.Create;
  Add( Result );
end;

procedure TRegistroP110List.SetItem(Index: Integer;
  const Value: TRegistroP110);
begin
  Put( Index, Value );
end;

{ TRegistroP199List }

function TRegistroP199List.GetItem(Index: Integer): TRegistroP199;
begin
  Result := TRegistroP199( inherited Items[Index] );
end;

function TRegistroP199List.New: TRegistroP199;
begin
  Result := TRegistroP199.Create;
  Add( Result );
end;

procedure TRegistroP199List.SetItem(Index: Integer;
  const Value: TRegistroP199);
begin
  Put( Index, Value );
end;

{ TRegistroP210List }

function TRegistroP210List.GetItem(Index: Integer): TRegistroP210;
begin
  Result := TRegistroP210( inherited Items[Index] );
end;

function TRegistroP210List.New: TRegistroP210;
begin
  Result := TRegistroP210.Create;
  Add( Result );
end;

procedure TRegistroP210List.SetItem(Index: Integer;
  const Value: TRegistroP210);
begin
  Put( Index, Value );
end;

{ TRegistroP100 }

constructor TRegistroP100.Create;
begin
  fRegistroP110 := TRegistroP110List.Create;
  fRegistroP199 := TRegistroP199List.Create;
end;

destructor TRegistroP100.Destroy;
begin
  fRegistroP110.Free;
  fRegistroP199.Free;
  inherited;
end;

{ TRegistroP200 }

constructor TRegistroP200.Create;
begin
  fRegistroP210 := TRegistroP210List.Create;
end;

destructor TRegistroP200.Destroy;
begin
  fRegistroP210.Free;
  inherited;
end;

{ TRegistroP200List }

function TRegistroP200List.GetItem(Index: Integer): TRegistroP200;
begin
  Result := TRegistroP200( inherited Items[Index] );
end;

function TRegistroP200List.New: TRegistroP200;
begin
  Result := TRegistroP200.Create;
  Add( Result );
end;

procedure TRegistroP200List.SetItem(Index: Integer; const Value: TRegistroP200);
begin
  Put( Index, Value );
end;

end.
