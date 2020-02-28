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

unit ACBrECFBloco_T;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroT030List = class;

  /// Registro T001 - Abertura do Bloco T – Lucro Arbitrado
  TRegistroT001 = class(TOpenBlocos)
  private
    FRegistroT030         : TRegistroT030List; // NIVEL 2
  public
    property RegistroT030: TRegistroT030List read FRegistroT030 write FRegistroT030;
  end;

  /// Registro T030 - Identificação dos Período e Forma de Apuração do
  /// IRPJ e CSLL das Empresas Tributadas pelo Lucro Arbitrado

  TRegistroT120List = class;
  TRegistroT150List = class;
  TRegistroT170List = class;
  TRegistroT181List = class;

  { TRegistroT030 }

  TRegistroT030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    fPER_APUR: string;

    FRegistroT120: TRegistroT120List;
    FRegistroT150: TRegistroT150List;
    FRegistroT170: TRegistroT170List;
    FRegistroT181: TRegistroT181List;

  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    // registros filhos
    property RegistroT120: TRegistroT120List read FRegistroT120 write FRegistroT120;
    property RegistroT150: TRegistroT150List read FRegistroT150 write FRegistroT150;
    property RegistroT170: TRegistroT170List read FRegistroT170 write FRegistroT170;
    property RegistroT181: TRegistroT181List read FRegistroT181 write FRegistroT181;
  end;

  /// Registro T030 - Lista

  TRegistroT030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT030;
    procedure SetItem(Index: Integer; const Value: TRegistroT030);
  public
    function New: TRegistroT030;
    property Items[Index: Integer]: TRegistroT030 read GetItem write SetItem;
  end;

  /// Registro T120 - Apuração da Base de Cálculo do IRPJ com Base no Lucro Arbitrado

  { TRegistroT120 }

  TRegistroT120 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro T120 - Lista

  TRegistroT120List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT120;
    procedure SetItem(Index: Integer; const Value: TRegistroT120);
  public
    function New: TRegistroT120;
    property Items[Index: Integer]: TRegistroT120 read GetItem write SetItem;
  end;

  /// Registro T150 - Cálculo do Imposto de Renda com Base no Lucro Arbitrado

  { TRegistroT150 }

  TRegistroT150 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro T150 - Lista

  TRegistroT150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT150;
    procedure SetItem(Index: Integer; const Value: TRegistroT150);
  public
    function New: TRegistroT150;
    property Items[Index: Integer]: TRegistroT150 read GetItem write SetItem;
  end;

  /// Registro T170 - Apuração da Base de Cálculo da CSLL com Base no Lucro Arbitrado

  { TRegistroT170 }

  TRegistroT170 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro T170 - Lista

  TRegistroT170List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT170;
    procedure SetItem(Index: Integer; const Value: TRegistroT170);
  public
    function New: TRegistroT170;
    property Items[Index: Integer]: TRegistroT170 read GetItem write SetItem;
  end;

  /// Registro T181 - Cálculo da CSLL com Base no Lucro Arbitrado

  { TRegistroT181 }

  TRegistroT181 = class(TBlocos)
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro T181 - Lista

  TRegistroT181List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT181;
    procedure SetItem(Index: Integer; const Value: TRegistroT181);
  public
    function New: TRegistroT181;
    property Items[Index: Integer]: TRegistroT181 read GetItem write SetItem;
  end;

  /// Registro T990 - ENCERRAMENTO DO BLOCO T
  TRegistroT990 = class(TCloseBlocos)
  end;

implementation

{ TRegistroT030List }

function TRegistroT030List.GetItem(Index: Integer): TRegistroT030;
begin
   Result := TRegistroT030(Inherited Items[Index]);
end;

function TRegistroT030List.New: TRegistroT030;
begin
  Result := TRegistroT030.Create;
  Add(Result);
end;

procedure TRegistroT030List.SetItem(Index: Integer; const Value: TRegistroT030);
begin
  Put(Index, Value);
end;

{ TRegistroT120List }

function TRegistroT120List.GetItem(Index: Integer): TRegistroT120;
begin
   Result := TRegistroT120(Inherited Items[Index]);
end;

function TRegistroT120List.New: TRegistroT120;
begin
  Result := TRegistroT120.Create;
  Add(Result);
end;

procedure TRegistroT120List.SetItem(Index: Integer; const Value: TRegistroT120);
begin
  Put(Index, Value);
end;

{ TRegistroT150List }

function TRegistroT150List.GetItem(Index: Integer): TRegistroT150;
begin
   Result := TRegistroT150(Inherited Items[Index]);
end;

function TRegistroT150List.New: TRegistroT150;
begin
  Result := TRegistroT150.Create;
  Add(Result);
end;

procedure TRegistroT150List.SetItem(Index: Integer; const Value: TRegistroT150);
begin
  Put(Index, Value);
end;

{ TRegistroT170List }

function TRegistroT170List.GetItem(Index: Integer): TRegistroT170;
begin
   Result := TRegistroT170(Inherited Items[Index]);
end;

function TRegistroT170List.New: TRegistroT170;
begin
  Result := TRegistroT170.Create;
  Add(Result);
end;

procedure TRegistroT170List.SetItem(Index: Integer; const Value: TRegistroT170);
begin
  Put(Index, Value);
end;

{ TRegistroT181List }

function TRegistroT181List.GetItem(Index: Integer): TRegistroT181;
begin
   Result := TRegistroT181(Inherited Items[Index]);
end;

function TRegistroT181List.New: TRegistroT181;
begin
  Result := TRegistroT181.Create;
  Add(Result);
end;

procedure TRegistroT181List.SetItem(Index: Integer; const Value: TRegistroT181);
begin
  Put(Index, Value);
end;

{ TRegistroP030 }

constructor TRegistroT030.Create;
begin
   inherited;
   FRegistroT120 := TRegistroT120List.Create;
   FRegistroT150 := TRegistroT150List.Create;
   FRegistroT170 := TRegistroT170List.Create;
   FRegistroT181 := TRegistroT181List.Create;
end;

destructor TRegistroT030.Destroy;
begin
   FRegistroT120.Free;
   FRegistroT150.Free;
   FRegistroT170.Free;
   FRegistroT181.Free;
   inherited;
end;

end.

