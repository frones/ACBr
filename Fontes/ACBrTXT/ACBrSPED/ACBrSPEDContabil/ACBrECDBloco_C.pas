{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrECDBloco_C;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECDBlocos;

type
  /// Registro C001 - ABERTURA DO BLOCO C

  TRegistroC001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroC050List = class;
  TRegistroC051List = class;
  TRegistroC052List = class;
  TRegistroC150List = class;
  TRegistroC155List = class;
  TRegistroC600List = class;
  TRegistroC650List = class;

  /// Rregistro C040 – IDENTIFICAÇÃO DA ESCRITURAÇÃO CONTÁBIL RECUPERADA
  TRegistroC040 = class
  private
    FHASH_ECD_REC: string;
    FDT_INI_ECD_REC: TDateTime;
    FDT_FIN_ECD_REC: TDateTime;
    FCNPJ_ECD_REC: string;
    FIND_ESC: char;
    FCOD_VER_LC: string;
    FNUM_ORD: string;
    FNAT_LIVR: string;
    FIND_SIT_ESP_ECD_REC: Integer;
    FIND_NIRE_ECD_REC: Integer;
    FIND_FIN_ESC_ECD_REC: Integer;
    FTIP_ECD_REC: integer;
    FCOD_SCP_ECD_REC: string;
    FIDENT_MF_ECD_REC: string;
    FIND_ESC_CONS_ECD_REC: string;
    FIND_CENTRALIZADA_ECD_REC: integer;
    FIND_MUDANCA_PC_ECD_REC: integer;
    FIND_PLANO_REF_ECD_REC: integer;

    FRegistroC050: TRegistroC050List;
    FRegistroC150: TRegistroC150List;
    FRegistroC600: TRegistroC600List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property HASH_ECD_REC: string              read FHASH_ECD_REC write FHASH_ECD_REC;
    property DT_INI_ECD_REC: TDateTime         read FDT_INI_ECD_REC write FDT_INI_ECD_REC;
    property DT_FIN_ECD_REC: TDateTime         read FDT_FIN_ECD_REC write FDT_FIN_ECD_REC;
    property CNPJ_ECD_REC: string              read FCNPJ_ECD_REC write FCNPJ_ECD_REC;
    property IND_ESC: char                     read FIND_ESC write FIND_ESC;
    property COD_VER_LC: string                read FCOD_VER_LC write FCOD_VER_LC;
    property NUM_ORD: string                   read FNUM_ORD write FNUM_ORD;
    property NAT_LIVR: string                  read FNAT_LIVR write FNAT_LIVR;
    property IND_SIT_ESP_ECD_REC: Integer      read FIND_SIT_ESP_ECD_REC write FIND_SIT_ESP_ECD_REC;
    property IND_NIRE_ECD_REC: Integer         read FIND_NIRE_ECD_REC write FIND_NIRE_ECD_REC;
    property IND_FIN_ESC_ECD_REC: Integer      read FIND_FIN_ESC_ECD_REC write FIND_FIN_ESC_ECD_REC;
    property TIP_ECD_REC: integer              read FTIP_ECD_REC write FTIP_ECD_REC;
    property COD_SCP_ECD_REC: string           read FCOD_SCP_ECD_REC write FCOD_SCP_ECD_REC;
    property IDENT_MF_ECD_REC: string          read FIDENT_MF_ECD_REC write FIDENT_MF_ECD_REC;
    property IND_ESC_CONS_ECD_REC: string      read FIND_ESC_CONS_ECD_REC write FIND_ESC_CONS_ECD_REC;
    property IND_CENTRALIZADA_ECD_REC: integer read FIND_CENTRALIZADA_ECD_REC write FIND_CENTRALIZADA_ECD_REC;
    property IND_MUDANCA_PC_ECD_REC: integer   read FIND_MUDANCA_PC_ECD_REC write FIND_MUDANCA_PC_ECD_REC;
    property IND_PLANO_REF_ECD_REC: integer   read FIND_PLANO_REF_ECD_REC write FIND_PLANO_REF_ECD_REC;

    /// Registros FILHOS
    property RegistroC050: TRegistroC050List read FRegistroC050 write FRegistroC050;
    property RegistroC150: TRegistroC150List read FRegistroC150 write FRegistroC150;
    property RegistroC600: TRegistroC600List read FRegistroC600 write FRegistroC600;
  end;

  /// Registro C050 – PLANO DE CONTAS RECUPERADO

  TRegistroC050 = class
  private
    FDT_ALT: TDateTime;
    FNIVEL: string;
    FIND_CTA: string;
    FCOD_NAT: string;
    FCOD_CTA: string;
    FCOD_CTA_SUP: string;
    FCTA: string;

    FRegistroC051: TRegistroC051List;
    FRegistroC052: TRegistroC052List;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property DT_ALT: TDateTime read FDT_ALT write FDT_ALT;
    property COD_NAT: string read FCOD_NAT write FCOD_NAT;
    property IND_CTA: string read FIND_CTA write FIND_CTA;
    property NIVEL: string read FNIVEL write FNIVEL;
    property COD_CTA: string read FCOD_CTA write FCOD_CTA;
    property COD_CTA_SUP: string read FCOD_CTA_SUP write FCOD_CTA_SUP;
    property CTA: string read FCTA write FCTA;

    /// Registros FILHOS
    property RegistroC051: TRegistroC051List read FRegistroC051 write FRegistroC051;
    property RegistroC052: TRegistroC052List read FRegistroC052 write FRegistroC052;
  end;

  /// Registro K100 - Lista

  TRegistroC050List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC050;
    procedure SetItem(Index: Integer; const Value: TRegistroC050);
  public
    function New: TRegistroC050;
    property Items[Index: Integer]: TRegistroC050 read GetItem write SetItem;
  end;

  /// Registro C051 - PLANO DE CONTAS REFERENCIAL RECUPERADO
  TRegistroC051 = class
  private
    FCOD_CCUS: string;
    FCOD_CTA_REF: string;

  public
    property COD_CCUS: string read FCOD_CCUS write FCOD_CCUS;
    property COD_CTA_REF: string read FCOD_CTA_REF write FCOD_CTA_REF;
  end;

  /// Registro C051 - Lista

  TRegistroC051List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC051;
    procedure SetItem(Index: Integer; const Value: TRegistroC051);
  public
    function New: TRegistroC051;
    property Items[Index: Integer]: TRegistroC051 read GetItem write SetItem;
  end;


  /// Registro C052 - INDICAÇÃO DOS CÓDIGOS DE AGLUTINAÇÃO RECUPERADOS
  TRegistroC052 = class
  private
    FCOD_CCUS: string;
    FCOD_AGL: string;
  public
    property COD_CCUS: string read FCOD_CCUS write FCOD_CCUS;
    property COD_AGL: string read FCOD_AGL write FCOD_AGL;
  end;

  /// Registro C052 - Lista

  TRegistroC052List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC052;
    procedure SetItem(Index: Integer; const Value: TRegistroC052);
  public
    function New: TRegistroC052;
    property Items[Index: Integer]: TRegistroC052 read GetItem write SetItem;
  end;


  /// Registro C150 - SALDOS PERIÓDICOS RECUPERADOS – IDENTIFICAÇÃO DO PERÍODO
  TRegistroC150 = class
  private
    FDT_INI: TDateTime;
    FDT_FIN: TDateTime;

    FRegistroC155: TRegistroC155List;
  public
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;

    /// Registros FILHOS
    property RegistroC155: TRegistroC155List read FRegistroC155 write FRegistroC155;
  end;

  /// Registro C150 - Lista

  TRegistroC150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC150;
    procedure SetItem(Index: Integer; const Value: TRegistroC150);
  public
    function New: TRegistroC150;
    property Items[Index: Integer]: TRegistroC150 read GetItem write SetItem;
  end;

  /// Registro C155 – DETALHE DOS SALDOS PERIÓDICOS RECUPERADOS

  TRegistroC155 = class
  private
    FCOD_CTA_REC: string;
    FCOD_CCUS_REC: string;
    FVL_SLD_INI_REC: Double;
    FIND_DC_INI_REC: string;
    FVL_DEB_REC: Double;
    FVL_CRED_REC: Double;
    FVL_SLD_FIN_REC: Double;
    FIND_DC_FIN_REC: string;
  public
    property COD_CTA_REC: string read FCOD_CTA_REC write FCOD_CTA_REC;
    property COD_CCUS_REC: string read FCOD_CCUS_REC write FCOD_CCUS_REC;
    property VL_SLD_INI_REC: Double read FVL_SLD_INI_REC write FVL_SLD_INI_REC;
    property IND_DC_INI_REC: string read FIND_DC_INI_REC write FIND_DC_INI_REC;
    property VL_DEB_REC: Double read FVL_DEB_REC write FVL_DEB_REC;
    property VL_CRED_REC: Double read FVL_CRED_REC write FVL_CRED_REC;
    property VL_SLD_FIN_REC: Double read FVL_SLD_FIN_REC write FVL_SLD_FIN_REC;
    property IND_DC_FIN_REC: string read FIND_DC_FIN_REC write FIND_DC_FIN_REC;
  end;

  /// Registro C155 - Lista

  TRegistroC155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC155;
    procedure SetItem(Index: Integer; const Value: TRegistroC155);
  public
    function New: TRegistroC155;
    property Items[Index: Integer]: TRegistroC155 read GetItem write SetItem;
  end;

  /// Registro C600 -  DEMONSTRAÇÕES CONTÁBEIS RECUPERADAS - PERÍODO

  TRegistroC600 = class
  private
    FDT_INI: TDateTime;
    FDT_FIN: TDateTime;
    FID_DEM: Integer;
    FCAB_DEM: string;

    FRegistroC650: TRegistroC650List;
  public
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property ID_DEM: Integer read FID_DEM write FID_DEM;
    property CAB_DEM: string read FCAB_DEM write FCAB_DEM;

    /// Registros FILHOS
    property RegistroC650: TRegistroC650List read FRegistroC650 write FRegistroC650;
  end;

  /// Registro C600 - Lista

  TRegistroC600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC600;
    procedure SetItem(Index: Integer; const Value: TRegistroC600);
  public
    function New: TRegistroC600;
    property Items[Index: Integer]: TRegistroC600 read GetItem write SetItem;
  end;

  /// Registro C650 - SALDOS DAS CONTAS CONSOLIDADAS

  TRegistroC650 = class
  private
    FCOD_AGL: string;
    FNIVEL_AGL: string;
    FDESCR_COD_AGL: string;
    FVL_CTA_FIN: Double;
    FIND_DC_CTA_FIN: string;
  public
    property COD_AGL: string read FCOD_AGL write FCOD_AGL;
    property NIVEL_AGL: string read FNIVEL_AGL write FNIVEL_AGL;
    property DESCR_COD_AGL: string read FDESCR_COD_AGL write FDESCR_COD_AGL;
    property VL_CTA_FIN: Double read FVL_CTA_FIN write FVL_CTA_FIN;
    property IND_DC_CTA_FIN: string read FIND_DC_CTA_FIN write FIND_DC_CTA_FIN;
  end;

  /// Registro 650 - Lista

  TRegistroC650List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC650;
    procedure SetItem(Index: Integer; const Value: TRegistroC650);
  public
    function New: TRegistroC650;
    property Items[Index: Integer]: TRegistroC650 read GetItem write SetItem;
  end;

  /// Registro C990 - ENCERRAMENTO DO BLOCO C

  TRegistroC990 = class
  private
    fQTD_LIN_C: Integer;    /// Quantidade total de linhas do Bloco K
  public
    property QTD_LIN_C: Integer read fQTD_LIN_C write fQTD_LIN_C;
  end;

implementation

{ TRegistroC030 }

constructor TRegistroC040.Create;
begin
  FRegistroC050 := TRegistroC050List.Create;
  FRegistroC150 := TRegistroC150List.Create;
  FRegistroC600 := TRegistroC600List.Create;
end;

destructor TRegistroC040.Destroy;
begin
  FRegistroC050.Free;
  FRegistroC150.Free;
  FRegistroC600.Free;
  inherited;
end;

{ TRegistroC050 }

constructor TRegistroC050.Create;
begin
  FRegistroC051 := TRegistroC051List.Create;
end;

destructor TRegistroC050.Destroy;
begin
  FRegistroC051.Free;
  inherited;
end;

{ TRegistroC050List }

function TRegistroC050List.GetItem(Index: Integer): TRegistroC050;
begin
  Result := TRegistroC050(Inherited Items[Index]);
end;

function TRegistroC050List.New: TRegistroC050;
begin
  Result := TRegistroC050.Create;
  Add(Result);
end;

procedure TRegistroC050List.SetItem(Index: Integer; const Value: TRegistroC050);
begin
  Put(Index, Value);
end;

{ TRegistroC051List }

function TRegistroC051List.GetItem(Index: Integer): TRegistroC051;
begin
  Result := TRegistroC051(Inherited Items[Index]);
end;

function TRegistroC051List.New: TRegistroC051;
begin
  Result := TRegistroC051.Create;
  Add(Result);
end;

procedure TRegistroC051List.SetItem(Index: Integer; const Value: TRegistroC051);
begin
  Put(Index, Value);
end;

{ TRegistroC052List }

function TRegistroC052List.GetItem(Index: Integer): TRegistroC052;
begin
  Result := TRegistroC052(Inherited Items[Index]);
end;

function TRegistroC052List.New: TRegistroC052;
begin
  Result := TRegistroC052.Create;
  Add(Result);
end;

procedure TRegistroC052List.SetItem(Index: Integer; const Value: TRegistroC052);
begin
  Put(Index, Value);
end;

{ TRegistroC150List }

function TRegistroC150List.GetItem(Index: Integer): TRegistroC150;
begin
  Result := TRegistroC150(Inherited Items[Index]);
end;

function TRegistroC150List.New: TRegistroC150;
begin
  Result := TRegistroC150.Create;
  Add(Result);
end;

procedure TRegistroC150List.SetItem(Index: Integer; const Value: TRegistroC150);
begin
  Put(Index, Value);
end;

{ TRegistroC155List }

function TRegistroC155List.GetItem(Index: Integer): TRegistroC155;
begin
  Result := TRegistroC155(Inherited Items[Index]);
end;

function TRegistroC155List.New: TRegistroC155;
begin
  Result := TRegistroC155.Create;
  Add(Result);
end;

procedure TRegistroC155List.SetItem(Index: Integer; const Value: TRegistroC155);
begin
  Put(Index, Value);
end;

{ TRegistroC600List }

function TRegistroC600List.GetItem(Index: Integer): TRegistroC600;
begin
  Result := TRegistroC600(Inherited Items[Index]);
end;

function TRegistroC600List.New: TRegistroC600;
begin
  Result := TRegistroC600.Create;
  Add(Result);
end;

procedure TRegistroC600List.SetItem(Index: Integer; const Value: TRegistroC600);
begin
  Put(Index, Value);
end;

{ TRegistroC650List }

function TRegistroC650List.GetItem(Index: Integer): TRegistroC650;
begin
  Result := TRegistroC650(Inherited Items[Index]);
end;

function TRegistroC650List.New: TRegistroC650;
begin
  Result := TRegistroC650.Create;
  Add(Result);
end;

procedure TRegistroC650List.SetItem(Index: Integer; const Value: TRegistroC650);
begin
  Put(Index, Value);
end;

end.
