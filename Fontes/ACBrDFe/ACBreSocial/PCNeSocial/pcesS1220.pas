{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS1220;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1220Collection = class;
  TS1220CollectionItem = class;
  TEvtInfoIR = class;
  TIdeBenef = class;
  TIdeDepCollection = class;
  TIdeDepCollectionItem = class;
  TInfoIR = class;
  TDedDepenCollection = class;
  TDedDepenCollectionItem = class;
  TInfoComplemDedCollection = class;
  TInfoComplemDedCollectionItem = class;
  TPenAlimCollection = class;
  TPenAlimCollectionItem = class;
  TPrevidComplCollection = class;
  TPrevidComplCollectionItem = class;
  TPlanSaudeCollection = class;
  TPlanSaudeCollectionItem = class;
  TInfoDepCollection = class;
  TInfoDepCollectionItem = class;
  TInfoReembMedCollection = class;
  TInfoReembMedCollectionItem = class;
  TDetReembTitCollection = class;
  TDetReembTitCollectionItem = class;
  TInfoReembDepCollection = class;
  TInfoReembDepCollectionItem = class;
  TDetReembDepCollection = class;
  TDetReembDepCollectionItem = class;
  TInfoProcRetCollection = class;
  TInfoProcRetCollectionItem = class;

  TS1220Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1220CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1220CollectionItem);
  public
    function New: TS1220CollectionItem;
    property Items[Index: Integer]: TS1220CollectionItem read GetItem write SetItem; default;
  end;

  TS1220CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtInfoIR: TEvtInfoIR;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtInfoIR: TEvtInfoIR read FEvtInfoIR write FEvtInfoIR;
  end;

  TEvtInfoIR = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;

    procedure GerarIdeBenef(obj: TIdeBenef);
    procedure GerarIdeDep(obj: TIdeDepCollection);
    procedure GerarInfoIR(obj: TInfoIR);
    procedure GerarDedDepen(obj: TDedDepenCollection; suspensao: boolean = False);
    procedure GerarInfoComplemDed(obj: TInfoComplemDedCollection);
    procedure GerarPenAlim(obj: TPenAlimCollection; suspensao: boolean = False);
    procedure GerarPrevidCompl(obj: TPrevidComplCollection; suspensao: boolean = False);
    procedure GerarPlanSaude(obj: TPlanSaudeCollection);
    procedure GerarInfoDep(obj: TInfoDepCollection);
    procedure GerarInfoReembMed(obj: TInfoReembMedCollection);
    procedure GerarDetReembTit(obj: TDetReembTitCollection);
    procedure GerarInfoReembDep(obj: TInfoReembDepCollection);
    procedure GerarDetReembDep(obj: TDetReembDepCollection);
    procedure GerarInfoProcRet(obj: TInfoProcRetCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
  end;

  TIdeBenef = class(TObject)
  private
    FcpfBenef: string;
    FideDep: TIdeDepCollection;
    FinfoIR: TInfoIR;

    function getIdeDep(): TIdeDepCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instIdeDep(): boolean;

    property cpfBenef: string read FcpfBenef write FcpfBenef;
    property ideDep: TIdeDepCollection read getIdeDep write FideDep;
    property infoIR: TInfoIR read FinfoIR write FinfoIR;
  end;

  TIdeDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeDepCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeDepCollectionItem);
  public
    function New: TIdeDepCollectionItem;
    property Items[Index: Integer]: TIdeDepCollectionItem read GetItem write SetItem;
  end;

  TIdeDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FdtNascto: TDateTime;
    Fnome: string;
    FrelDep: tpRelDep;
    FdescrDep: string;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property nome: string read Fnome write Fnome;
    property relDep: tpRelDep read FrelDep write FrelDep;
    property descrDep: string read FdescrDep write FdescrDep;
  end;

  TInfoIR = class(TObject)
  private
    FvlrDedPC: double;
    FvlrDedPensao: double;
    FvlrDedDepen: double;
    FdedDepen: TdedDepenCollection;
    FinfoComplemDed: TInfoComplemDedCollection;
    FplanSaude: TPlanSaudeCollection;
    FinfoReembMed: TInfoReembMedCollection;
    FinfoProcRet: TInfoProcRetCollection;

    function getDedDepen(): TdedDepenCollection;
    function getInfoComplemDed(): TInfoComplemDedCollection;
    function getPlanSaude(): TPlanSaudeCollection;
    function getInfoReembMed(): TInfoReembMedCollection;
    function getInfoProcRet(): TInfoProcRetCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instDedDepen(): boolean;
    function instInfoComplemDed(): boolean;
    function instPlanSaude(): boolean;
    function instInfoReembMed(): boolean;
    function instInfoProcRet(): boolean;

    property vlrDedPC: double read FvlrDedPC write FvlrDedPC;
    property vlrDedPensao: double read FvlrDedPensao write FvlrDedPensao;
    property vlrDedDepen: double read FvlrDedDepen write FvlrDedDepen;
    property dedDepen: TdedDepenCollection read getDedDepen write FdedDepen;
    property infoComplemDed: TInfoComplemDedCollection read getInfoComplemDed write FinfoComplemDed;
    property planSaude: TPlanSaudeCollection read getPlanSaude write FPlanSaude;
    property infoReembMed: TInfoReembMedCollection read getInfoReembMed write FInfoReembMed;
    property infoProcRet: TInfoProcRetCollection read getInfoProcRet write FinfoProcRet;
  end;

  TDedDepenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDedDepenCollectionItem;
    procedure SetItem(Index: Integer; Value: TDedDepenCollectionItem);
  public
    function New: TDedDepenCollectionItem;
    property Items[Index: Integer]: TDedDepenCollectionItem read GetItem write SetItem;
  end;

  TDedDepenCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrDeducao: double;
    FvlrDedSusp: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDeducao: double read FvlrDeducao write FvlrDeducao;
    property vlrDedSusp: double read FvlrDedSusp write FvlrDedSusp;
  end;

  TInfoComplemDedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoComplemDedCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoComplemDedCollectionItem);
  public
    function New: TInfoComplemDedCollectionItem;
    property Items[Index: Integer]: TInfoComplemDedCollectionItem read GetItem write SetItem;
  end;

  TInfoComplemDedCollectionItem = class(TObject)
  private
    FpenAlim: TPenAlimCollection;
    FprevidCompl: TPrevidComplCollection;

    function getPenAlim(): TPenAlimCollection;
    function getPrevidCompl(): TPrevidComplCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instPenAlim(): boolean;
    function instPrevidCompl(): boolean;

    property penAlim: TPenAlimCollection read getPenAlim write FpenAlim;
    property previdCompl: TPrevidComplCollection read getPrevidCompl write FprevidCompl;
  end;

  TPenAlimCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPenAlimCollectionItem;
    procedure SetItem(Index: Integer; Value: TPenAlimCollectionItem);
  public
    function New: TPenAlimCollectionItem;
    property Items[Index: Integer]: TPenAlimCollectionItem read GetItem write SetItem;
  end;

  TPenAlimCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrPensao: double;
    FvlrPensaoSusp: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrPensao: double read FvlrPensao write FvlrPensao;
    property vlrPensaoSusp: double read FvlrPensaoSusp write FvlrPensaoSusp;
  end;

  TPrevidComplCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPrevidComplCollectionItem;
    procedure SetItem(Index: Integer; Value: TPrevidComplCollectionItem);
  public
    function New: TPrevidComplCollectionItem;
    property Items[Index: Integer]: TPrevidComplCollectionItem read GetItem write SetItem;
  end;

  TPrevidComplCollectionItem = class(TObject)
  private
    FcnpjEntidPC: string;
    FvlrDedPC: double;
    FvlrDedPCSusp: double;
  public
    property cnpjEntidPC: string read FcnpjEntidPC write FcnpjEntidPC;
    property vlrDedPC: double read FvlrDedPC write FvlrDedPC;
    property vlrDedPCSusp: double read FvlrDedPCSusp write FvlrDedPCSusp;
  end;

  TPlanSaudeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPlanSaudeCollectionItem;
    procedure SetItem(Index: Integer; Value: TPlanSaudeCollectionItem);
  public
    function New: TPlanSaudeCollectionItem;
    property Items[Index: Integer]: TPlanSaudeCollectionItem read GetItem write SetItem;
  end;

  TPlanSaudeCollectionItem = class(TObject)
  private
    FcnpjOper: string;
    FregANS: integer;
    FvlrSaude: double;
    FinfoDep: TInfoDepCollection;

    function getInfoDep(): TInfoDepCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoDep(): boolean;

    property cnpjOper: string read FcnpjOper write FcnpjOper;
    property regANS: integer read FregANS write FregANS;
    property vlrSaude: double read FvlrSaude write FvlrSaude;
    property infoDep: TInfoDepCollection read getInfoDep write FinfoDep;
  end;

  TInfoDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoDepCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoDepCollectionItem);
  public
    function New: TInfoDepCollectionItem;
    property Items[Index: Integer]: TInfoDepCollectionItem read GetItem write SetItem;
  end;

  TInfoDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrSaude: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrSaude: double read FvlrSaude write FvlrSaude;
  end;

  TInfoReembMedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoReembMedCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoReembMedCollectionItem);
  public
    function New: TInfoReembMedCollectionItem;
    property Items[Index: Integer]: TInfoReembMedCollectionItem read GetItem write SetItem;
  end;

  TInfoReembMedCollectionItem = class(TObject)
  private
    FindOrgReemb: integer;
    FcnpjOper: string;
    FregANS: integer;
    FdetReembTit: TDetReembTitCollection;
    FinfoReembDep: TinfoReembDepCollection;

    function getDetReembTit(): TDetReembTitCollection;
    function getinfoReembDep(): TinfoReembDepCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instDetReembTit(): boolean;
    function instInfoReembDep(): boolean;

    property indOrgReemb: integer read FindOrgReemb write FindOrgReemb;
    property cnpjOper: string read FcnpjOper write FcnpjOper;
    property regANS: integer read FregANS write FregANS;
    property detReembTit: TDetReembTitCollection read getDetReembTit write FdetReembTit;
    property infoReembDep: TInfoReembDepCollection read getInfoReembDep write FInfoReembDep;
  end;

  TDetReembTitCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetReembTitCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetReembTitCollectionItem);
  public
    function New: TDetReembTitCollectionItem;
    property Items[Index: Integer]: TDetReembTitCollectionItem read GetItem write SetItem;
  end;

  TDetReembTitCollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FvlrReemb: double;
    FvlrReembAnt: double;
  public
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vlrReemb: double read FvlrReemb write FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt write FvlrReembAnt;
  end;

  TInfoReembDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoReembDepCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoReembDepCollectionItem);
  public
    function New: TInfoReembDepCollectionItem;
    property Items[Index: Integer]: TInfoReembDepCollectionItem read GetItem write SetItem;
  end;

  TInfoReembDepCollectionItem = class(TObject)
  private
    FcpfBenef: string;
    FdetReembDep: TDetReembDepCollection;

    function getDetReembDep(): TDetReembDepCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instDetReembDep(): boolean;

    property cpfBenef: string read FcpfBenef write FcpfBenef;
    property detReembDep: TDetReembDepCollection read getDetReembDep write FdetReembDep;
  end;

  TDetReembDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetReembDepCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetReembDepCollectionItem);
  public
    function New: TDetReembDepCollectionItem;
    property Items[Index: Integer]: TDetReembDepCollectionItem read GetItem write SetItem;
  end;

  TDetReembDepCollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FvlrReemb: double;
    FvlrReembAnt: double;
  public
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vlrReemb: double read FvlrReemb write FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt write FvlrReembAnt;
  end;

  TInfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoProcRetCollectionItem);
  public
    function New: TInfoProcRetCollectionItem;
    property Items[Index: Integer]: TInfoProcRetCollectionItem read GetItem write SetItem;
  end;

  TInfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: tpTpProcRet;
    FnrProcRet: string;
    FcodSusp: Int64;
    FpenAlim: TPenAlimCollection;
    FprevidCompl: TPrevidComplCollection;
    FdedDepen: TDedDepenCollection;

    function getPenAlim(): TPenAlimCollection;
    function getPrevidCompl(): TPrevidComplCollection;
    function getDedDepen(): TDedDepenCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instPenAlim(): boolean;
    function instPrevidCompl(): boolean;
    function instDedDepen(): boolean;

    property tpProcRet: tpTpProcRet read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: Int64 read FcodSusp write FcodSusp;
    property penAlim: TPenAlimCollection read getPenAlim write FpenAlim;
    property previdCompl: TPrevidComplCollection read getPrevidCompl write FprevidCompl;
    property dedDepen: TDedDepenCollection read getDedDepen write FdedDepen;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1220Collection }

function TS1220Collection.GetItem(Index: Integer): TS1220CollectionItem;
begin
  Result := TS1220CollectionItem(inherited Items[Index]);
end;

procedure TS1220Collection.SetItem(Index: Integer;
  Value: TS1220CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1220Collection.New: TS1220CollectionItem;
begin
  Result := TS1220CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1220CollectionItem }

constructor TS1220CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teS1220;
  FEvtInfoIR  := TEvtInfoIR.Create(AOwner);
end;

destructor TS1220CollectionItem.Destroy;
begin
  FEvtInfoIR.Free;

  inherited;
end;

{ TIdeBenef }

constructor TIdeBenef.Create;
begin
  inherited Create;

  FideDep := nil;
  FinfoIR := TInfoIR.Create;
end;

destructor TIdeBenef.Destroy;
begin
  if instIdeDep() then
    FreeAndNil(FideDep);

  FreeAndNil(FinfoIR);

  inherited;
end;

function TIdeBenef.instIdeDep(): boolean;
begin
  Result := Assigned(FideDep);
end;

function TIdeBenef.getIdeDep(): TIdeDepCollection;
begin
  if not Assigned(FideDep) then
    FIdeDep := TIdeDepCollection.Create;
  Result := FideDep;
end;

{ TDedDepenCollection }

function TDedDepenCollection.GetItem(Index: Integer): TDedDepenCollectionItem;
begin
  Result := TDedDepenCollectionItem(inherited Items[Index]);
end;

procedure TDedDepenCollection.SetItem(Index: Integer; Value: TDedDepenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDedDepenCollection.New: TDedDepenCollectionItem;
begin
  Result := TDedDepenCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoIR }

constructor TInfoIR.Create;
begin
  inherited Create;

  FdedDepen := nil;
  FinfoComplemDed := nil;
  FplanSaude := nil;
end;

destructor TInfoIR.Destroy;
begin
  if instDedDepen() then
    FreeAndNil(FdedDepen);
  if instInfoComplemDed() then
    FreeAndNil(FinfoComplemDed);
  if instPlanSaude() then
    FreeAndNil(FplanSaude);

  inherited;
end;

function TInfoIR.getInfoComplemDed(): TInfoComplemDedCollection;
begin
  if not Assigned(FinfoComplemDed) then
    FinfoComplemDed := TInfoComplemDedCollection.Create;
  Result := FinfoComplemDed;
end;

function TInfoIR.instInfoComplemDed(): boolean;
begin
  Result := Assigned(FinfoComplemDed);
end;

function TInfoIR.getDedDepen(): TdedDepenCollection;
begin
  if not Assigned(FdedDepen) then
    FdedDepen := TdedDepenCollection.Create;
  Result := FdedDepen;
end;

function TInfoIR.instDedDepen(): boolean;
begin
  Result := Assigned(FdedDepen);
end;

function TInfoIR.getPlanSaude(): TPlanSaudeCollection;
begin
  if not Assigned(FplanSaude) then
    FplanSaude := TPlanSaudeCollection.Create;
  Result := FplanSaude;
end;

function TInfoIR.instPlanSaude(): boolean;
begin
  Result := Assigned(FplanSaude);
end;

function TInfoIR.getInfoReembMed(): TInfoReembMedCollection;
begin
  if not Assigned(FInfoReembMed) then
    FInfoReembMed := TInfoReembMedCollection.Create;
  Result := FInfoReembMed;
end;

function TInfoIR.instInfoReembMed(): boolean;
begin
  Result := Assigned(FInfoReembMed);
end;

function TInfoIR.getInfoProcRet(): TInfoProcRetCollection;
begin
  if not Assigned(FInfoProcRet) then
    FInfoProcRet := TInfoProcRetCollection.Create;
  Result := FInfoProcRet;
end;

function TInfoIR.instInfoProcRet(): boolean;
begin
  Result := Assigned(FInfoProcRet);
end;

{ TIdeDepCollection }

function TIdeDepCollection.GetItem(Index: Integer): TIdeDepCollectionItem;
begin
  Result := TIdeDepCollectionItem(inherited Items[Index]);
end;

procedure TIdeDepCollection.SetItem(Index: Integer; Value: TIdeDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeDepCollection.New: TIdeDepCollectionItem;
begin
  Result := TIdeDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoComplemDedCollection }

function TInfoComplemDedCollection.GetItem(Index: Integer): TInfoComplemDedCollectionItem;
begin
  Result := TInfoComplemDedCollectionItem(inherited Items[Index]);
end;

procedure TInfoComplemDedCollection.SetItem(Index: Integer; Value: TInfoComplemDedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoComplemDedCollection.New: TInfoComplemDedCollectionItem;
begin
  Result := TInfoComplemDedCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoComplemDedCollectionItem }

constructor TInfoComplemDedCollectionItem.Create;
begin
  inherited Create;

  FpenAlim := nil;
  FprevidCompl := nil;
end;

destructor TInfoComplemDedCollectionItem.Destroy;
begin
  if instPenAlim() then
    FreeAndNil(FpenAlim);
  if instPrevidCompl() then
    FreeAndNil(FprevidCompl);
  inherited;
end;

function TInfoComplemDedCollectionItem.getPenAlim(): TPenAlimCollection;
begin
  if not Assigned(FpenAlim) then
    FpenAlim := TPenAlimCollection.Create;
  Result := FpenAlim;
end;

function TInfoComplemDedCollectionItem.instPenAlim(): boolean;
begin
  Result := Assigned(FpenAlim);
end;

function TInfoComplemDedCollectionItem.getPrevidCompl(): TPrevidComplCollection;
begin
  if not Assigned(FprevidCompl) then
    FprevidCompl := TPrevidComplCollection.Create;
  Result := FprevidCompl;
end;

function TInfoComplemDedCollectionItem.instPrevidCompl(): boolean;
begin
  Result := Assigned(FprevidCompl);
end;

{ TPenAlimCollection }

function TPenAlimCollection.GetItem(Index: Integer): TPenAlimCollectionItem;
begin
  Result := TPenAlimCollectionItem(inherited Items[Index]);
end;

procedure TPenAlimCollection.SetItem(Index: Integer; Value: TPenAlimCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TPenAlimCollection.New: TPenAlimCollectionItem;
begin
  Result := TPenAlimCollectionItem.Create;
  Self.Add(Result);
end;

{ TPrevidComplCollection }

function TPrevidComplCollection.GetItem(Index: Integer): TPrevidComplCollectionItem;
begin
  Result := TPrevidComplCollectionItem(inherited Items[Index]);
end;

procedure TPrevidComplCollection.SetItem(Index: Integer; Value: TPrevidComplCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TPrevidComplCollection.New: TPrevidComplCollectionItem;
begin
  Result := TPrevidComplCollectionItem.Create;
  Self.Add(Result);
end;

{ TPlanSaudeCollection }

function TPlanSaudeCollection.GetItem(Index: Integer): TPlanSaudeCollectionItem;
begin
  Result := TPlanSaudeCollectionItem(inherited Items[Index]);
end;

procedure TPlanSaudeCollection.SetItem(Index: Integer; Value: TPlanSaudeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TPlanSaudeCollection.New: TPlanSaudeCollectionItem;
begin
  Result := TPlanSaudeCollectionItem.Create;
  Self.Add(Result);
end;

{ TPlanSaudeCollectionItem }

constructor TPlanSaudeCollectionItem.Create;
begin
  inherited Create;

  FinfoDep := nil;
end;

destructor TPlanSaudeCollectionItem.Destroy;
begin
  if instInfoDep() then
    FreeAndNil(FinfoDep);

  inherited;
end;

function TPlanSaudeCollectionItem.getInfoDep(): TInfoDepCollection;
begin
  if not Assigned(FinfoDep) then
    FinfoDep := TInfoDepCollection.Create;
  Result := FinfoDep;
end;

function TPlanSaudeCollectionItem.instInfoDep(): boolean;
begin
  Result := Assigned(FinfoDep);
end;

{ TInfoDepCollection }

function TInfoDepCollection.GetItem(Index: Integer): TInfoDepCollectionItem;
begin
  Result := TInfoDepCollectionItem(inherited Items[Index]);
end;

procedure TInfoDepCollection.SetItem(Index: Integer; Value: TInfoDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoDepCollection.New: TInfoDepCollectionItem;
begin
  Result := TInfoDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoReembMedCollection }

function TInfoReembMedCollection.GetItem(Index: Integer): TInfoReembMedCollectionItem;
begin
  Result := TInfoReembMedCollectionItem(inherited Items[Index]);
end;

procedure TInfoReembMedCollection.SetItem(Index: Integer; Value: TInfoReembMedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoReembMedCollection.New: TInfoReembMedCollectionItem;
begin
  Result := TInfoReembMedCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoReembMedCollectionItem }

constructor TInfoReembMedCollectionItem.Create;
begin
  inherited Create;

  FdetReembTit := nil;
  FinfoReembDep := nil;
end;

destructor TInfoReembMedCollectionItem.Destroy;
begin
  if instDetReembTit() then
    FreeAndNil(FdetReembTit);
  if instInfoReembDep() then
    FreeAndNil(FinfoReembDep);

  inherited;
end;

function TInfoReembMedCollectionItem.getDetReembTit(): TDetReembTitCollection;
begin
  if not Assigned(FdetReembTit) then
    FdetReembTit := TDetReembTitCollection.Create;
  Result := FdetReembTit;
end;

function TInfoReembMedCollectionItem.instDetReembTit(): boolean;
begin
  Result := Assigned(FdetReembTit);
end;

function TInfoReembMedCollectionItem.getInfoReembDep(): TInfoReembDepCollection;
begin
  if not Assigned(FinfoReembDep) then
    FinfoReembDep := TinfoReembDepCollection.Create;
  Result := FinfoReembDep;
end;

function TInfoReembMedCollectionItem.instInfoReembDep(): boolean;
begin
  Result := Assigned(FInfoReembDep);
end;

{ TDetReembTitCollection }

function TDetReembTitCollection.GetItem(Index: Integer): TDetReembTitCollectionItem;
begin
  Result := TDetReembTitCollectionItem(inherited Items[Index]);
end;

procedure TDetReembTitCollection.SetItem(Index: Integer; Value: TDetReembTitCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetReembTitCollection.New: TDetReembTitCollectionItem;
begin
  Result := TDetReembTitCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoReembDepCollection }

function TInfoReembDepCollection.GetItem(Index: Integer): TInfoReembDepCollectionItem;
begin
  Result := TInfoReembDepCollectionItem(inherited Items[Index]);
end;

procedure TInfoReembDepCollection.SetItem(Index: Integer; Value: TInfoReembDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoReembDepCollection.New: TInfoReembDepCollectionItem;
begin
  Result := TInfoReembDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoReembDepCollectionItem }

constructor TInfoReembDepCollectionItem.Create;
begin
  inherited Create;

  FdetReembDep := nil;
end;

destructor TInfoReembDepCollectionItem.Destroy;
begin
  if instDetReembDep() then
    FreeAndNil(FdetReembDep);

  inherited;
end;

function TInfoReembDepCollectionItem.getDetReembDep(): TDetReembDepCollection;
begin
  if not Assigned(FdetReembDep) then
    FdetReembDep := TDetReembDepCollection.Create;
  Result := FdetReembDep;
end;

function TInfoReembDepCollectionItem.instDetReembDep(): Boolean;
begin
  Result := Assigned(FdetReembDep);
end;

{ TDetReembDepCollection }

function TDetReembDepCollection.GetItem(Index: Integer): TDetReembDepCollectionItem;
begin
  Result := TDetReembDepCollectionItem(inherited Items[Index]);
end;

procedure TDetReembDepCollection.SetItem(Index: Integer; Value: TDetReembDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetReembDepCollection.New: TDetReembDepCollectionItem;
begin
  Result := TDetReembDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoProcRetCollection }

function TInfoProcRetCollection.GetItem(Index: Integer): TInfoProcRetCollectionItem;
begin
  Result := TInfoProcRetCollectionItem(inherited Items[Index]);
end;

procedure TInfoProcRetCollection.SetItem(Index: Integer; Value: TInfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoProcRetCollection.New: TInfoProcRetCollectionItem;
begin
  Result := TInfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoProcRetCollectionItem }

constructor TInfoProcRetCollectionItem.Create;
begin
  inherited Create;

  FpenAlim := nil;
  FprevidCompl := nil;
  FdedDepen := nil;
end;

destructor TInfoProcRetCollectionItem.Destroy;
begin
  if instPenAlim() then
    FreeAndNil(FpenAlim);
  if instPrevidCompl() then
    FreeAndNil(FprevidCompl);
  if instDedDepen() then
    FreeAndNil(FdedDepen);

  inherited;
end;

function TInfoProcRetCollectionItem.getPenAlim(): TPenAlimCollection;
begin
  if not Assigned(FpenAlim) then
    FpenAlim := TPenAlimCollection.Create;
  Result := FpenAlim;
end;

function TInfoProcRetCollectionItem.getPrevidCompl(): TPrevidComplCollection;
begin
  if not Assigned(FprevidCompl) then
    FprevidCompl := TPrevidComplCollection.Create;
  Result := FprevidCompl;
end;

function TInfoProcRetCollectionItem.getDedDepen(): TDedDepenCollection;
begin
  if not Assigned(FdedDepen) then
    FdedDepen := TdedDepenCollection.Create;
  Result := FdedDepen;
end;

function TInfoProcRetCollectionItem.instPenAlim(): boolean;
begin
  Result := Assigned(FpenAlim);
end;

function TInfoProcRetCollectionItem.instPrevidCompl(): boolean;
begin
  Result := Assigned(FprevidCompl);
end;

function TInfoProcRetCollectionItem.instDedDepen(): boolean;
begin
  Result := Assigned(FdedDepen);
end;

{ TEvtInfoIR }

constructor TEvtInfoIR.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
end;

destructor TEvtInfoIR.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;

  inherited;
end;

procedure TEvtInfoIR.GerarIdeBenef(obj: TIdeBenef);
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef',  11, 11, 1, obj.cpfBenef);

  if obj.instIdeDep() then
    GerarIdeDep(obj.ideDep);

  GerarInfoIR(obj.infoIR);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtInfoIR.GerarIdeDep(obj: TIdeDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('ideDep');

    Gerador.wCampo(tcStr, '', 'cpfDep',   11, 11, 1, obj.Items[i].cpfDep);
    Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 0, obj.Items[i].dtNascto);
    Gerador.wCampo(tcStr, '', 'nome',      0, 60, 0, obj.Items[i].nome);
    Gerador.wCampo(tcStr, '', 'relDep',    1,  2, 1, eSTpRelDepToStr(obj.Items[i].relDep));
    Gerador.wCampo(tcStr, '', 'descrDep',  0, 30, 0, obj.Items[i].descrDep);

    Gerador.wGrupo('/ideDep');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'ideDep', 'Identificação dos dependentes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtInfoIR.GerarInfoIR(obj: TInfoIR);
begin
  Gerador.wGrupo('infoIR');

  Gerador.wCampo(tcDe2, '', 'vlrDedPC',     1, 14, 0, obj.vlrDedPC);
  Gerador.wCampo(tcDe2, '', 'vlrDedPensao', 1, 14, 0, obj.vlrDedPensao);
  Gerador.wCampo(tcDe2, '', 'vlrDedDepen',  1, 14, 0, obj.vlrDedDepen);

  if obj.instDedDepen() then
    GerarDedDepen(obj.dedDepen);
  if obj.instInfoComplemDed() then
    GerarInfoComplemDed(obj.infoComplemDed);
  if obj.instPlanSaude() then
    GerarPlanSaude(obj.planSaude);
  if obj.instInfoReembMed() then
    GerarInfoReembMed(obj.infoReembMed);
  if obj.instInfoProcRet() then
    GerarInfoProcRet(obj.infoProcRet);

  Gerador.wGrupo('/infoIR');
end;

procedure TEvtInfoIR.GerarDedDepen(obj: TDedDepenCollection; suspensao: boolean = False);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dedDepen');

    Gerador.wCampo(tcStr, '', 'cpfDep',       11, 11, 1, obj.Items[i].cpfDep);

    if not suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrDeducao',  0, 14, 0, obj.Items[i].vlrDeducao);

    if suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrDedSusp',  0, 14, 0, obj.Items[i].vlrDedSusp);

    Gerador.wGrupo('/dedDepen');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'dedDepen', 'Dedução do rendimento tributável relativa a dependentes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtInfoIR.GerarInfoComplemDed(obj: TInfoComplemDedCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoComplemDed');

    if obj.Items[i].instPenAlim() then
      GerarPenAlim(obj.Items[i].penAlim);
    if obj.Items[i].instPrevidCompl() then
      GerarPrevidCompl(obj.Items[i].previdCompl);

    Gerador.wGrupo('/infoComplemDed');
  end;

  if obj.Count > 3 then
    Gerador.wAlerta('', 'infoComplemDed', 'Informações complementares relativas a deduções do rendimento tributável', ERR_MSG_MAIOR_MAXIMO + '3');
end;

procedure TEvtInfoIR.GerarPenAlim(obj: TPenAlimCollection; suspensao: boolean = False);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('penAlim');

    Gerador.wCampo(tcStr, '', 'cpfDep',          11, 11, 1, obj.Items[i].cpfDep);

    if not suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrPensao',      0, 14, 0, obj.Items[i].vlrPensao);

    if suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrPensaoSusp',  0, 14, 0, obj.Items[i].vlrPensaoSusp);

    Gerador.wGrupo('/penAlim');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'penAlim', 'Informação dos beneficiários da pensão alimentícia', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarPrevidCompl(obj: TPrevidComplCollection; suspensao: boolean = False);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('previdCompl');

    Gerador.wCampo(tcStr, '', 'cnpjEntidPC',     0, 14, 1, obj.Items[i].cnpjEntidPC);
    
    if not suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrDedPC',      0, 14, 1, obj.Items[i].vlrDedPC);

    if suspensao then
      Gerador.wCampo(tcDe2, '', 'vlrDedPCSusp',  0, 14, 1, obj.Items[i].vlrDedPCSusp);
    
    Gerador.wGrupo('/previdCompl');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'previdCompl', 'Informações relativas a planos de previdência complementar', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarPlanSaude(obj: TPlanSaudeCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('planSaude');

    Gerador.wCampo(tcStr, '', 'cnpjOper', 14, 14, 1, obj.Items[i].cnpjOper);
    Gerador.wCampo(tcStr, '', 'regANS',    0,  6, 0, obj.Items[i].regANS);
    Gerador.wCampo(tcDe2, '', 'vlrSaude',  1, 14, 1, obj.Items[i].vlrSaude);

    if obj.Items[i].instInfoDep() then
      GerarInfoDep(obj.Items[i].infoDep);

    Gerador.wGrupo('/planSaude');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'planSaude', 'Plano de saúde coletivo', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarInfoDep(obj: TInfoDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoDep');

    Gerador.wCampo(tcStr, '', 'cpfDep',   11, 11, 1, obj.Items[i].cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrSaude',  1, 14, 1, obj.Items[i].vlrSaude);

    Gerador.wGrupo('/infoDep');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoDep', 'Informações de dependente de plano de saúde coletivo empresarial', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarInfoReembMed(obj: TInfoReembMedCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoReembMed');

    Gerador.wCampo(tcStr, '', 'indOrgReemb',  1,  1, 1, obj.Items[i].indOrgReemb);
    Gerador.wCampo(tcStr, '', 'cnpjOper',    14, 14, 1, obj.Items[i].cnpjOper);
    Gerador.wCampo(tcStr, '', 'regANS',       0,  6, 0, obj.Items[i].regANS);

    if obj.Items[i].instDetReembTit() then
      GerarDetReembTit(obj.Items[i].detReembTit);
    if obj.Items[i].instInfoReembDep() then
      GerarInfoReembDep(obj.Items[i].infoReembDep);

    Gerador.wGrupo('/infoReembMed');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoReembMed', 'Informações relativas a reembolsos efetuados no período de apuração', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarDetReembTit(obj: TDetReembTitCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('detReembTit');

    Gerador.wCampo(tcStr, '', 'tpInsc',       1,  1, 1, eSTpInscricaoToStr(obj.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',       0, 14, 1, obj.Items[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',     0, 14, 0, obj.Items[i].vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt',  0, 14, 0, obj.Items[i].vlrReembAnt);

    Gerador.wGrupo('/detReembTit');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'detReembTit', 'Informação de reembolso do titular do plano de saúde coletivo empresarial', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarInfoReembDep(obj: TInfoReembDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoReembDep');

    Gerador.wCampo(tcStr, '', 'cpfBenef',  11, 11, 1, obj.Items[i].cpfBenef);

    if obj.Items[i].instDetReembDep() then
      GerarDetReembDep(obj.Items[i].detReembDep);

    Gerador.wGrupo('/infoReembDep');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoReembDep', 'Informação de reembolso do dependente do plano de saúde coletivo empresarial.', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarDetReembDep(obj: TDetReembDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('detReembDep');

    Gerador.wCampo(tcStr, '', 'tpInsc',       1,  1, 1, eSTpInscricaoToStr(obj.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',      14, 14, 1, obj.Items[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',     0, 14, 0, obj.Items[i].vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt',  0, 14, 0, obj.Items[i].vlrReembAnt);

    Gerador.wGrupo('/detReembDep');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'detReembDep', 'Detalhamento dos reembolsos efetuados em {perApur} pelo empregador ao trabalhador referente a despesas médicas ou odontológicas', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtInfoIR.GerarInfoProcRet(obj: TInfoProcRetCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoProcRet');

    Gerador.wCampo(tcStr, '', 'tpProcRet',  1,  1, 1, eStpTpProcRetToStr(obj.Items[i].tpProcRet));
    Gerador.wCampo(tcStr, '', 'nrProcRet', 21, 21, 1, obj.Items[i].nrProcRet);
    Gerador.wCampo(tcStr, '', 'codSusp',   14, 14, 1, obj.Items[i].codSusp);
 
    if obj.Items[i].instPenAlim() then
      GerarPenAlim(obj.Items[i].penAlim, True);
    if obj.Items[i].instPrevidCompl() then
      GerarPrevidCompl(obj.Items[i].previdCompl, True);
    if obj.Items[i].instDedDepen() then
      GerarDedDepen(obj.Items[i].dedDepen, True);

    Gerador.wGrupo('/infoProcRet');
  end;

  if obj.Count > 4 then
    Gerador.wAlerta('', 'infoProcRet', 'Informações de processos relacionados a não retenção de tributos ou depósitos judiciais.', ERR_MSG_MAIOR_MAXIMO + '4');
end;

function TEvtInfoIR.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtInfoIR');
    Gerador.wGrupo('evtInfoIR Id="' + Self.Id + '"');

    GerarIdeEvento3(Self.ideEvento, True, False, False);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeBenef(Self.ideBenef);

    Gerador.wGrupo('/evtInfoIR');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtInfoIR');

//    Validar(schevtInfoIR);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtInfoIR.LerArqIni(const AIniString: String): Boolean;
begin
  Result := True;
end;

end.
