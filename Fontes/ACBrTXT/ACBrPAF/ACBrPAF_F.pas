{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
unit ACBrPAF_F;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  // F2 - Manifesto fiscal de viagem
  TRegistroF2 = class
  private
    fCNPJ_ORG: string;        // Numero do CNPJ do orgao de delegação do transporte
    fCNPJ_EMP: string;        // Numero do CNPJ da empresa do serviço de transporte
    fCOD_LOCAL: string;       // Código do local de emissão Manifesto Fiscal de viagem...
    fID_LINHA: string;        // Identificação da linha
    fDESC_LINHA: string;      // Descrição da linha
    fDT_PART: TDateTime;      // Data e Hora da partida
    fCOD_VIAGEM: string;      // Código do tipo de viagem

    fRegistroValido: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property CNPJ_ORG: string        read fCNPJ_ORG       write fCNPJ_ORG;
    property CNPJ_EMP: string        read fCNPJ_EMP       write fCNPJ_EMP;
    property COD_LOCAL: string       read fCOD_LOCAL      write fCOD_LOCAL;
    property ID_LINHA: string        read fID_LINHA       write fID_LINHA;
    property DESC_LINHA: string      read fDESC_LINHA     write fDESC_LINHA;
    property DT_PART: TDateTime      read fDT_PART        write fDT_PART;
    property COD_VIAGEM: string      read fCOD_VIAGEM     write fCOD_VIAGEM;
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido;
  end;

  // Registro F2 - Lista
  TRegistroF2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroF2;
    procedure SetItem(Index: Integer; const Value: TRegistroF2);
  public
    function New: TRegistroF2;
    property Items[Index: Integer]: TRegistroF2 read GetItem write SetItem;
  end;

  // F3 - Bilhetes de passagem do manifesto
  TRegistroF3 = class
  private
    fNUM_FAB: string;        // Numero de fabricação do ECF
    fMF_ADICIONAL: string;   // Letrada Indicativa de MF Adicional
    fMODELO_ECF: string;     // Modelo do ECF
    fNUM_USU: integer;       // Nº de ordem do usuario do ECF
    fCCF: integer;           // Número do Contador de Cupom Fiscal
    fCOO: integer;           // Nº do Contador de Ordem de Operação
    fCOD_ORIG: string;       // Codigo do ponto de origem da prestação do serviço
    fCOD_DEST: string;       // Codigo do ponto de destino da prestação do serviço
    fVL_DOC: currency;       // Valor total do documento
    fST: String;             // Código da situação tributaria
    fCOD_TSER: string;       // Codigo do tipo de serviço
    fPOLTRONA: Integer;      // Nº da poltrona

    fRegistroValido: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property NUM_FAB: string         read fNUM_FAB        write fNUM_FAB;
    property MF_ADICIONAL: string    read fMF_ADICIONAL   write fMF_ADICIONAL;
    property MODELO_ECF: string      read fMODELO_ECF     write fMODELO_ECF;
    property NUM_USU: integer        read fNUM_USU        write fNUM_USU;
    property CCF: integer            read fCCF            write fCCF;
    property COO: integer            read fCOO            write fCOO;
    property COD_ORIG: string        read fCOD_ORIG       write fCOD_ORIG;
    property COD_DEST: string        read fCOD_DEST       write fCOD_DEST;
    property VL_DOC: currency        read fVL_DOC         write fVL_DOC;
    property ST: String              read fST             write fST;
    property COD_TSER: string        read fCOD_TSER       write fCOD_TSER;
    property POLTRONA: Integer       read fPOLTRONA       write fPOLTRONA;
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido;
  end;

  // Registro F3 - Lista
  TRegistroF3List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroF3;
    procedure SetItem(Index: Integer; const Value: TRegistroF3);
  public
    function New: TRegistroF3;
    property Items[Index: Integer]: TRegistroF3 read GetItem write SetItem;
  end;

  // F4 - Tipo de serviço
  TRegistroF4 = class
  private
    fCOD_TSER: string;    // Codigo do tipo de serviço
    fQTDE_TOTAL: Integer; // Total de bilhetes vendidos por tipo de serviço

    fRegistroValido: boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property COD_TSER: string        read fCOD_TSER       write fCOD_TSER;
    property QTDE_TOTAL: Integer     read fQTDE_TOTAL     write fQTDE_TOTAL;
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido;
  end;

  // Registro F4 - Lista
  TRegistroF4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroF4;
    procedure SetItem(Index: Integer; const Value: TRegistroF4);
  public
    function New: TRegistroF4;
    property Items[Index: Integer]: TRegistroF4 read GetItem write SetItem;
  end;

implementation

{ TRegistroF2 }
constructor TRegistroF2.Create;
begin
  fRegistroValido := True;
end;

destructor TRegistroF2.Destroy;
begin
  inherited;
end;

{ TRegistroF2List }
function TRegistroF2List.GetItem(Index: Integer): TRegistroF2;
begin
  Result := TRegistroF2(inherited Items[Index]);
end;

function TRegistroF2List.New: TRegistroF2;
begin
  Result := TRegistroF2.Create;
  Add(Result);
end;

procedure TRegistroF2List.SetItem(Index: Integer; const Value: TRegistroF2);
begin
  Put(Index, Value);
end;

{ TRegistroF3 }
constructor TRegistroF3.Create;
begin
  fRegistroValido := True;
end;

destructor TRegistroF3.Destroy;
begin
  inherited;
end;

{ TRegistroF3List }
function TRegistroF3List.GetItem(Index: Integer): TRegistroF3;
begin
  Result := TRegistroF3(inherited Items[Index]);
end;

function TRegistroF3List.New: TRegistroF3;
begin
  Result := TRegistroF3.Create;
  Add(Result);
end;

procedure TRegistroF3List.SetItem(Index: Integer; const Value: TRegistroF3);
begin
  Put(Index, Value);
end;

{ TRegistroF4 }
constructor TRegistroF4.Create;
begin
  fRegistroValido:= True;
  fQTDE_TOTAL:= 0;
end;

destructor TRegistroF4.Destroy;
begin
  inherited;
end;

{ TRegistroF4List }
function TRegistroF4List.GetItem(Index: Integer): TRegistroF4;
begin
  Result := TRegistroF4(inherited Items[Index]);
end;

function TRegistroF4List.New: TRegistroF4;
begin
  Result := TRegistroF4.Create;
  Add(Result);
end;

procedure TRegistroF4List.SetItem(Index: Integer; const Value: TRegistroF4);
begin
  Put(Index, Value);
end;

end.
