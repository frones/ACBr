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

{******************************************************************************
|* Historico
|*
|* --/--/2015: Juliomar Marchetti
|*  - Criação.
|* 12/08/2015: Isaque Pinheiro
|*  - Distribuição da primeira versão.
|* 20/08/2015: Lutzem Massao Aihara
|*  - Classe reestruturada.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_L;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistroL030List = class;
  TRegistroL100List = class;
  TRegistroL200List = class;
  TRegistroL210List = class;
  TRegistroL300List = class;

  /// Registro L001 - Abertura do Bloco L - Lucro Liquido - Lucro Real
  TRegistroL001 = class(TOpenBlocos)
  private
    FRegistroL030 :TRegistroL030List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy

    property RegistroL030: TRegistroL030List read FRegistroL030 write FRegistroL030;
  end;

  /// Registro L030 - Identificação dos Períodos e Formas de Apuração do
  /// IRPJ e da CSLL no Ano-Calendário
  TRegistroL030 = class(TBlocos)
  private
    fDT_INI:   TDateTime;
    fDT_FIN:   TDateTime;
    fPER_APUR: String;

    FRegistroL100: TRegistroL100List;
    FRegistroL200: TRegistroL200List;
    FRegistroL210: TRegistroL210List;
    FRegistroL300: TRegistroL300List;
  public
    constructor Create(); /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI:   TDateTime read fDT_INI write fDT_INI;
    property DT_FIN:   TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: String read fPER_APUR write fPER_APUR;

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
    function New(): TRegistroL030;
    property Items[Index: Integer]: TRegistroL030 read GetItem write SetItem;
  end;

  /// Registro L100 - Balanço Patrimonial
  TRegistroL100 = class(TBlocos)
  private
    fCODIGO:              String;
    fDESCRICAO:           String;
    fTIPO:                String;
    fNIVEL:               Integer;
    fCOD_NAT:             String;
    fCOD_CTA_SUP:         String;
    fVAL_CTA_REF_FIN:     Variant;
    fIND_VAL_CTA_REF_INI: String;
    fVAL_CTA_REF_INI:     Variant;
    fVAL_CTA_REF_DEB:     Variant;
    fVAL_CTA_REF_CRED:    Variant;
    fIND_VAL_CTA_REF_FIN: String;
  public
    constructor Create(); virtual; /// Create

    property CODIGO:              String read fCODIGO write fCODIGO;
    property DESCRICAO:           String read fDESCRICAO write fDESCRICAO;
    property TIPO:                String read fTIPO write fTIPO;
    property NIVEL:               Integer read fNIVEL write fNIVEL;
    property COD_NAT:             String read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP:         String read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VAL_CTA_REF_INI:     Variant read fVAL_CTA_REF_INI write fVAL_CTA_REF_INI;
    property IND_VAL_CTA_REF_INI: String read fIND_VAL_CTA_REF_INI write fIND_VAL_CTA_REF_INI;
    property VAL_CTA_REF_FIN:     Variant read fVAL_CTA_REF_FIN write fVAL_CTA_REF_FIN;
    property VAL_CTA_REF_DEB:     Variant read fVAL_CTA_REF_DEB write fVAL_CTA_REF_DEB;
    property VAL_CTA_REF_CRED:    Variant read fVAL_CTA_REF_CRED write fVAL_CTA_REF_CRED;
    property IND_VAL_CTA_REF_FIN: String read fIND_VAL_CTA_REF_FIN write fIND_VAL_CTA_REF_FIN;
  end;

  TRegistroL100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL100;
    procedure SetItem(Index: Integer; const Value: TRegistroL100);
  public
    function New(): TRegistroL100;
    property Items[Index: Integer]: TRegistroL100 read GetItem write SetItem;
  end;

  /// Registro L200 - Método de Avaliação do Estoque Final
  TRegistroL200 = class(TBlocos)
  private
    fIND_AVAL_ESTOQ: String;
  public
    constructor Create(); virtual; /// Create

    property IND_AVAL_ESTOQ: String read fIND_AVAL_ESTOQ write fIND_AVAL_ESTOQ;
  end;

  TRegistroL200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL200;
    procedure SetItem(Index: Integer; const Value: TRegistroL200);
  public
    function New(): TRegistroL200;
    property Items[Index: Integer]: TRegistroL200 read GetItem write SetItem;
  end;

  /// Registro L210 - Informativo da Composição de Custos
  TRegistroL210 = class(TBlocos)
  private
    fCODIGO:    String;
    fDESCRICAO: String;
    fVALOR:     Variant;
  public
    constructor Create(); virtual; /// Create

    property CODIGO:    String read fCODIGO write fCODIGO;
    property DESCRICAO: String read fDESCRICAO write fDESCRICAO;
    property VALOR:     Variant read fVALOR write fVALOR;
  end;

  TRegistroL210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL210;
    procedure SetItem(Index: Integer; const Value: TRegistroL210);
  public
    function New(): TRegistroL210;
    property Items[Index: Integer]: TRegistroL210 read GetItem write SetItem;
  end;

  /// Registro L300 - Demonstração do Resultado do Exercício
  TRegistroL300 = class(TBlocos)
  private
    fCODIGO:      String;
    fDESCRICAO:   String;
    fTIPO:        String;
    fNIVEL:       Integer;
    fCOD_NAT:     String;
    fCOD_CTA_SUP: String;
    fVALOR:       Variant;
    fIND_VALOR:   String;
  public
    constructor Create(); virtual; /// Create

    property CODIGO:      String read fCODIGO write fCODIGO;
    property DESCRICAO:   String read fDESCRICAO write fDESCRICAO;
    property TIPO:        String read fTIPO write fTIPO;
    property NIVEL:       Integer read fNIVEL write fNIVEL;
    property COD_NAT:     String read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: String read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VALOR:       Variant read fVALOR write fVALOR;
    property IND_VALOR:   String read fIND_VALOR write fIND_VALOR;
  end;

  TRegistroL300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL300;
    procedure SetItem(Index: Integer; const Value: TRegistroL300);
  public
    function New(): TRegistroL300;
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

function TRegistroL030List.New(): TRegistroL030;
begin
  Result := TRegistroL030.Create();
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

function TRegistroL210List.New(): TRegistroL210;
begin
  Result := TRegistroL210.Create();
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

function TRegistroL300List.New(): TRegistroL300;
begin
  Result := TRegistroL300.Create();
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

function TRegistroL200List.New(): TRegistroL200;
begin
  Result := TRegistroL200.Create();
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

function TRegistroL100List.New(): TRegistroL100;
begin
  Result := TRegistroL100.Create();
  Add(Result);
end;

procedure TRegistroL100List.SetItem(Index: Integer; const Value: TRegistroL100);
begin
  Put(Index, Value);
end;

{ TRegistroL030 }

constructor TRegistroL030.Create();
begin
   inherited Create;
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

{ TRegistroL001 }

constructor TRegistroL001.Create;
begin
  inherited Create;
  FRegistroL030 := TRegistroL030List.Create;
  //
  IND_DAD := idComDados;;
end;

destructor TRegistroL001.Destroy;
begin
  FRegistroL030.Free;
  inherited;
end;

{ TRegistroL100 }

constructor TRegistroL100.Create();
begin
  inherited Create;
end;

{ TRegistroL300 }

constructor TRegistroL300.Create();
begin
  inherited Create;
end;

{ TRegistroL200 }

constructor TRegistroL200.Create();
begin
  inherited Create;
end;

{ TRegistroL210 }

constructor TRegistroL210.Create();
begin
  inherited Create;
end;

end.
