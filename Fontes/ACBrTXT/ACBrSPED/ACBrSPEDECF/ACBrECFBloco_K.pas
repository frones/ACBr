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

unit ACBrECFBloco_K;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro K001 - Abertura do Bloco K – Saldos das Contas Contábeis e Referenciais
  TRegistroK001 = class(TOpenBlocos)
  end;

  TRegistroK155List = class;
  TRegistroK156List = class;
  TRegistroK355List = class;
  TRegistroK356List = class;

  /// Registro K030 - Identificação dos Períodos e Formas de Apuração do IRPJ e da CSLL no Ano-Calendário
  TRegistroK030 = class(TBlocos)
  private
    fPER_APUR: string;
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    FRegistroK155: TRegistroK155List;
    FRegistroK355: TRegistroK355List;
  public
    constructor Create; /// Create
    destructor Destroy; /// Destroy

    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;

    // registros filhos
    property RegistroK155: TRegistroK155List read FRegistroK155 write FRegistroK155;
    property RegistroK355: TRegistroK355List read FRegistroK355 write FRegistroK355;
  end;


  //  Registro K030 - Lista

  TRegistroK030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK030;
    procedure SetItem(Index: Integer; const Value: TRegistroK030);
  public
    function New: TRegistroK030;
    property Items[Index: Integer]: TRegistroK030 read GetItem write SetItem;
  end;

  /// Registro K155 - Detalhes dos Saldos Contábeis (Depois do
  /// Encerramento do Resultado do Período)

  { TRegistroK155 }

  TRegistroK155 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
    fVL_SLD_FIN: variant;
    fVL_SLD_INI: variant;
    fIND_VL_SLD_INI: string;
    fVL_CRED:    variant;
    fVL_DEB:     variant;
    FRegistroK156: TRegistroK156List;
  public
    constructor Create; /// Create
    destructor Destroy; /// Destroy

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: variant read fVL_SLD_INI write fVL_SLD_INI;
    property IND_VL_SLD_INI: string read fIND_VL_SLD_INI write fIND_VL_SLD_INI;
    property VL_DEB: variant read fVL_DEB write fVL_DEB;
    property VL_CRED: variant read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;

    // registros filhos
    property RegistroK156: TRegistroK156List read FRegistroK156 write FRegistroK156;
  end;

  //  Registro K155 - Lista

  TRegistroK155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK155;
    procedure SetItem(Index: Integer; const Value: TRegistroK155);
  public
    function New: TRegistroK155;
    property Items[Index: Integer]: TRegistroK155 read GetItem write SetItem;
  end;


  /// Registro K156 - Mapeamento Referencial do Saldo Final
  TRegistroK156 = class(TBlocos)
  private
    fVL_SLD_FIN:     variant;
    fIND_VL_SLD_FIN: string;
    fCOD_CTA_REF:    string;
  public
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  //  Registro K156 - Lista

  TRegistroK156List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK156;
    procedure SetItem(Index: Integer; const Value: TRegistroK156);
  public
    function New: TRegistroK156;
    property Items[Index: Integer]: TRegistroK156 read GetItem write SetItem;
  end;

  /// Registro K355 - Saldos Finais das Contas Contábeis de Resultado
  /// Antes do Encerramento
  TRegistroK355 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_FIN: variant;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
    FRegistroK356: TRegistroK356List;
  public
    constructor Create; /// Create
    destructor Destroy; /// Destroy

    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;

    // registros filhos
    property RegistroK356: TRegistroK356List read FRegistroK356 write FRegistroK356;
  end;

  //  Registro K355 - Lista

  TRegistroK355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK355;
    procedure SetItem(Index: Integer; const Value: TRegistroK355);
  public
    function New: TRegistroK355;
    property Items[Index: Integer]: TRegistroK355 read GetItem write SetItem;
  end;

  /// Registro K356 - Mapeamento Referencial dos Saldos Finais das
  /// Contas de Resultado Antes do Encerramento
  TRegistroK356 = class(TBlocos)
  private
    fVL_SLD_FIN:     variant;
    fIND_VL_SLD_FIN: string;
    fCOD_CTA_REF:    string;
  public
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  //  Registro K356 - Lista

  TRegistroK356List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK356;
    procedure SetItem(Index: Integer; const Value: TRegistroK356);
  public
    function New: TRegistroK356;
    property Items[Index: Integer]: TRegistroK356 read GetItem write SetItem;
  end;


  /// Registro K990 - ENCERRAMENTO DO BLOCO K
  TRegistroK990 = class(TCloseBlocos)
    fQTD_LIN: Integer;    /// Quantidade total de linhas do Bloco I
  public
    property QTD_LIN: Integer read FQTD_LIN write FQTD_LIN;
  end;

implementation

{ TRegistroK030List }

function TRegistroK030List.GetItem(Index: Integer): TRegistroK030;
begin
  Result := TRegistroK030(Inherited Items[Index]);
end;

function TRegistroK030List.New: TRegistroK030;
begin
  Result := TRegistroK030.Create;
  Add(Result);
end;

procedure TRegistroK030List.SetItem(Index: Integer; const Value: TRegistroK030);
begin
  Put(Index, Value);
end;

{ TRegistroK155List }

function TRegistroK155List.GetItem(Index: Integer): TRegistroK155;
begin
  Result := TRegistroK155(Inherited Items[Index]);
end;

function TRegistroK155List.New: TRegistroK155;
begin
  Result := TRegistroK155.Create;
  Add(Result);
end;

procedure TRegistroK155List.SetItem(Index: Integer; const Value: TRegistroK155);
begin
  Put(Index, Value);
end;

{ TRegistroK156List }

function TRegistroK156List.GetItem(Index: Integer): TRegistroK156;
begin
  Result := TRegistroK156(Inherited Items[Index]);
end;

function TRegistroK156List.New: TRegistroK156;
begin
  Result := TRegistroK156.Create;
  Add(Result);
end;

procedure TRegistroK156List.SetItem(Index: Integer; const Value: TRegistroK156);
begin
  Put(Index, Value);
end;

{ TRegistroK030 }

constructor TRegistroK030.Create;
begin
   inherited;

   FRegistroK155 := TRegistroK155List.Create;
   FRegistroK355 := TRegistroK355List.Create;
end;

destructor TRegistroK030.Destroy;
begin
  FRegistroK355.Free;
  FRegistroK155.Free;
  
  inherited;
end;

{ TRegistroK355List }

function TRegistroK355List.GetItem(Index: Integer): TRegistroK355;
begin
  Result := TRegistroK355(Inherited Items[Index]);
end;

function TRegistroK355List.New: TRegistroK355;
begin
  Result := TRegistroK355.Create;
  Add(Result);
end;

procedure TRegistroK355List.SetItem(Index: Integer; const Value: TRegistroK355);
begin
  Put(Index, Value);
end;

{ TRegistroK155 }

constructor TRegistroK155.Create;
begin
   inherited;

   FRegistroK156 := TRegistroK156List.Create;
end;

destructor TRegistroK155.Destroy;
begin
   FRegistroK156.Free;
   
   inherited;
end;

{ TRegistroK356List }

function TRegistroK356List.GetItem(Index: Integer): TRegistroK356;
begin
   Result := TRegistroK356(Inherited Items[Index]);
end;

function TRegistroK356List.New: TRegistroK356;
begin
  Result := TRegistroK356.Create;
  Add(Result);
end;

procedure TRegistroK356List.SetItem(Index: Integer; const Value: TRegistroK356);
begin
  Put(Index, Value);
end;

{ TRegistroK355 }

constructor TRegistroK355.Create;
begin
   inherited;

   FRegistroK356 := TRegistroK356List.Create;    
end;

destructor TRegistroK355.Destroy;
begin
   FRegistroK356.Free;

   inherited;
end;

end.
