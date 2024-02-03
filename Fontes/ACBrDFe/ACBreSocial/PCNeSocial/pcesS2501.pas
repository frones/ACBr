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

unit pcesS2501;

interface

uses
  SysUtils, Classes, StrUtils,
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
  pcesCommon, pcesConversaoeSocial, pcesGerador,
  ACBrUtil.Strings, ACBrUtil.DateTime;

type
  TS2501Collection = class;
  TS2501CollectionItem = class;
  TEvtContProc = class;
  TIdeProc = class;
  TIdeTrabCollection = class;
  TIdeTrabCollectionItem = class;
  TCalcTribCollection = class;
  TCalcTribCollectionItem = class;
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;
  TInfoCRIRRFCollection = class;
  TInfoCRIRRFCollectionItem = class;
  TinfoIRCollection = class;
  TinfoIRCollectionItem = class;
  TdedDepenCollection = class;
  TdedDepenCollectionItem = class;
  TpenAlimCollection = class;
  TpenAlimCollectionItem = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;
  TinfoValoresCollection = class;
  TinfoValoresCollectionItem = class;
  TdedSuspCollection = class;
  TdedSuspCollectionItem = class;
  TbenefPenCollection = class;
  TbenefPenCollectionItem = class;
  TinfoIRComplem = class;
  TinfoDepCollection = class;
  TinfoDepCollectionItem = class;

  TS2501Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2501CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2501CollectionItem);
  public
    function New: TS2501CollectionItem;
    property Items[Index: Integer]: TS2501CollectionItem read GetItem write SetItem; default;
  end;

  TS2501CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtContProc: TEvtContProc;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtContProc: TEvtContProc read FEvtContProc write FEvtContProc;
  end;

  TEvtContProc = class(TESocialEvento)
  private
    FideEvento: TIdeEvento2;
    FideEmpregador: TIdeEmpregador;
    FideProc: TIdeProc;
    FideTrab: TIdeTrabCollection;

    procedure GerarIdeProc(obj: TIdeProc);
    procedure GerarIdeTrab(obj: TIdeTrabCollection);
    procedure GerarCalcTrib(obj: TCalcTribCollection);
    procedure GerarInfoCRIRRF(obj: TInfoCRIRRFCollection);
    procedure GerarInfoCRContrib(obj: TInfoCRContribCollection);
    procedure GerarInfoIR(obj: TinfoIRCollection);
    procedure GerarInfoRRA(obj: TinfoRRA);
    procedure GerarDespProcJud(obj: TDespProcJud);
    procedure GerarIdeAdv(obj: TIdeAdvCollection);
    procedure GerarDedDepen(obj: TdedDepenCollection);
    procedure GerarPenAlim(obj: TpenAlimCollection);
    procedure GerarInfoProcRet(obj: TinfoProcRetCollection);
    procedure GerarInfoValores(obj: TinfoValoresCollection);
    procedure GerarDedSusp(obj: TdedSuspCollection);
    procedure GerarBenefPen(obj: TbenefPenCollection);
    procedure GerarInfoIRComplem(obj: TinfoIRComplem);
    procedure GerarInfoDep(obj: TinfoDepCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideProc: TIdeProc read FideProc write FideProc;
    property ideTrab: TIdeTrabCollection read FideTrab write FideTrab;
  end;

  TIdeProc = class(TObject)
  private
    FnrProcTrab: string;
    FperApurPgto: string;
    Fobs: string;
  public
    property nrProcTrab: string read FnrProcTrab write FnrProcTrab;
    property perApurPgto: string read FperApurPgto write FperApurPgto;
    property obs: string read Fobs write Fobs;
  end;

  TIdeTrabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeTrabCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeTrabCollectionItem);
  public
    function New: TIdeTrabCollectionItem;
    property Items[Index: Integer]: TIdeTrabCollectionItem read GetItem write SetItem; default;
  end;

  TIdeTrabCollectionItem = class(TObject)
  private
    FcpfTrab: string;
    FcalcTrib: TCalcTribCollection;
    FinfoCRIRRF: TInfoCRIRRFCollection;
    FinfoIRComplem: TinfoIRComplem;

    function getCalcTrib: TCalcTribCollection;
    function getInfoCRIRRF: TInfoCRIRRFCollection;
    function getInfoIRComplem: TinfoIRComplem;
  public
    constructor Create;
    destructor Destroy; override;

    function instCalcTrib: boolean;
    function instInfoCRIRRF: boolean;
    function instInfoIRComplem: boolean;
    
    property cpfTrab: string read FcpfTrab write FcpfTrab;
    property calcTrib: TCalcTribCollection read getCalcTrib write FcalcTrib;
    property infoCRIRRF: TInfoCRIRRFCollection read getInfoCRIRRF write FinfoCRIRRF;
    property infoIRComplem: TinfoIRComplem read getInfoIRComplem write FinfoIRComplem;
  end;

  TCalcTribCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TCalcTribCollectionItem;
    procedure SetItem(Index: Integer; Value: TCalcTribCollectionItem);
  public
    function New: TCalcTribCollectionItem;
    property Items[Index: Integer]: TCalcTribCollectionItem read GetItem write SetItem; default;
  end;

  TCalcTribCollectionItem = class(TObject)
  private
    FperRef: string;
    FvrBcCpMensal: double;
    FvrBcCp13: double;
    FvrRendIRRF: double;
    FvrRendIRRF13: double;
    FinfoCRContrib: TInfoCRContribCollection;

    function getInfoCRContrib(): TInfoCRContribCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoCRContrib(): boolean;

    property perRef: string read FperRef write FperRef;
    property vrBcCpMensal: double read FvrBcCpMensal write FvrBcCpMensal;
    property vrBcCp13: double read FvrBcCp13 write FvrBcCp13;
    property vrRendIRRF: double read FvrRendIRRF write FvrRendIRRF;
    property vrRendIRRF13: double read FvrRendIRRF13 write FvrRendIRRF13;
    property infoCRContrib: TInfoCRContribCollection read getInfoCRContrib write FinfoCRContrib;
  end;

  TInfoCRContribCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    function New: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem; default;
  end;

  TInfoCRContribCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrCR: double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrCR: double read FvrCR write FvrCR;
  end;

  TInfoCRIRRFCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRIRRFCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRIRRFCollectionItem);
  public
    function New: TInfoCRIRRFCollectionItem;
    property Items[Index: Integer]: TInfoCRIRRFCollectionItem read GetItem write SetItem; default;
  end;

  TInfoCRIRRFCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrCR: double;
    FinfoIR: TinfoIRCollection;
    FinfoRRA: TinfoRRA;
    FdedDepen: TdedDepenCollection;
    FpenAlim: TpenAlimCollection;
    FinfoProcRet: TinfoProcRetCollection;

    function getInfoIR: TinfoIRCollection;
    function getInfoRRA: TinfoRRA;
    function getDedDepen: TdedDepenCollection;
    function getPenAlim: TpenAlimCollection;
    function getInfoProcRet: TinfoProcRetCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function infoIRInst: boolean;
    function infoRRAInst: boolean;
    function dedDepenInst: boolean;
    function penAlimInst: boolean;
    function infoProcRetInst: boolean;

    property tpCR: string read FtpCR write FtpCR;
    property vrCR: double read FvrCR write FvrCR;
    property infoIR: TinfoIRCollection read getInfoIR write FinfoIR;
    property infoRRA: TinfoRRA read getInfoRRA write FinfoRRA;
    property dedDepen: TdedDepenCollection read getDedDepen write FdedDepen;
    property penAlim: TpenAlimCollection read getPenAlim write FpenAlim;
    property infoProcRet: TinfoProcRetCollection read getInfoProcRet write FinfoProcRet;
  end;

  TpenAlimCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TpenAlimCollectionItem;
    procedure SetItem(Index: Integer; const Value: TpenAlimCollectionItem);
  public
    function New: TpenAlimCollectionItem;
    property Items[Index: Integer]: TpenAlimCollectionItem read GetItem write SetItem; default;
  end;

  TpenAlimCollectionItem = class(TObject)
  private
    FtpRend: integer;
    FcpfDep: string;
    FvlrPensao: double;
  public
    property tpRend: integer read FtpRend write FtpRend;
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrPensao: double read FvlrPensao write FvlrPensao;
  end;

  TinfoIRCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoIRCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoIRCollectionItem);
  public
    function New: TinfoIRCollectionItem;
    property Items[Index: Integer]: TinfoIRCollectionItem read GetItem write SetItem; default;
  end;

  TinfoIRCollectionItem = class(TObject)
  private
    FvrRendTrib: double;
    FvrRendTrib13: double;
    FvrRendMoleGrave: double;
    FvrRendIsen65: double;
    FvrJurosMora: double;
    FvrRendIsenNTrib: double;
    FdescIsenNTrib: string;
    FvrPrevOficial: double;
  public
    property vrRendTrib: double read FvrRendTrib write FvrRendTrib;
    property vrRendTrib13: double read FvrRendTrib13 write FvrRendTrib13;
    property vrRendMoleGrave: double read FvrRendMoleGrave write FvrRendMoleGrave;
    property vrRendIsen65: double read FvrRendIsen65 write FvrRendIsen65;
    property vrJurosMora: double read FvrJurosMora write FvrJurosMora;
    property vrRendIsenNTrib: double read FvrRendIsenNTrib write FvrRendIsenNTrib;
    property descIsenNTrib: string read FdescIsenNTrib write FdescIsenNTrib;
    property vrPrevOficial: double read FvrPrevOficial write FvrPrevOficial;
  end;

  TdedDepenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdedDepenCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdedDepenCollectionItem);
  public
    function New: TdedDepenCollectionItem;
    property Items[Index: Integer]: TdedDepenCollectionItem read GetItem write SetItem; default;
  end;

  TdedDepenCollectionItem = class(TObject)
  private
    FtpRend: integer;
    FcpfDep: string;
    FvlrDeducao: double;
  public
    property tpRend: integer read FtpRend write FtpRend;
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDeducao: double read FvlrDeducao write FvlrDeducao;
  end;

  TinfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoProcRetCollectionItem);
  public
    function New: TinfoProcRetCollectionItem;
    property Items[Index: Integer]: TinfoProcRetCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: tpTpProcRet;
    FnrProcRet: string;
    FcodSusp: string;
    FinfoValores: TinfoValoresCollection;

    function getInfoValores: TinfoValoresCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoValoresInst: boolean;

    property tpProcRet: tpTpProcRet read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: string read FcodSusp write FcodSusp;
    property infoValores: TinfoValoresCollection read getInfoValores write FinfoValores;
  end;

  TinfoValoresCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoValoresCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoValoresCollectionItem);
  public
    function New: TinfoValoresCollectionItem;
    property Items[Index: Integer]: TinfoValoresCollectionItem read GetItem write SetItem; default;
  end;

  TinfoValoresCollectionItem = class(TObject)
  private
    FindApuracao: tpIndApuracao;
    FvlrNRetido: double;
    FvlrDepJud: double;
    FvlrCmpAnoCal: double;
    FvlrCmpAnoAnt: double;
    FvlrRendSusp: double;
    FdedSusp: TdedSuspCollection;

    function getDedSusp: TdedSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dedSuspInst: boolean;

    property indApuracao: tpIndApuracao read FindApuracao write FindApuracao;
    property vlrNRetido: double read FvlrNRetido write FvlrNRetido;
    property vlrDepJud: double read FvlrDepJud write FvlrDepJud;
    property vlrCmpAnoCal: double read FvlrCmpAnoCal write FvlrCmpAnoCal;
    property vlrCmpAnoAnt: double read FvlrCmpAnoAnt write FvlrCmpAnoAnt;
    property vlrRendSusp: double read FvlrRendSusp write FvlrRendSusp;
    property dedSusp: TdedSuspCollection read getDedSusp write FdedSusp;
  end;

  TdedSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdedSuspCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdedSuspCollectionItem);
  public
    function New: TdedSuspCollectionItem;
    property Items[Index: Integer]: TdedSuspCollectionItem read GetItem write SetItem; default;
  end;

  TdedSuspCollectionItem = class(TObject)
  private
    FindTpDeducao: tpIndTpDeducaoT;
    FvlrDedSusp: double;
    FbenefPen: TbenefPenCollection;

   function getBenefPen: TbenefPenCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function benefPenInst: boolean;

    property indTpDeducao: tpIndTpDeducaoT read FindTpDeducao write FindTpDeducao;
    property vlrDedSusp: double read FvlrDedSusp write FvlrDedSusp;
    property benefPen: TbenefPenCollection read getBenefPen write Fbenefpen;
  end;

  TbenefPenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbenefPenCollectionItem;
    procedure SetItem(Index: Integer; const Value: TbenefPenCollectionItem);
  public
    function New: TbenefPenCollectionItem;
    property Items[Index: Integer]: TbenefPenCollectionItem read GetItem write SetItem; default;
  end;

  TbenefPenCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrDepenSusp: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDepenSusp: double read FvlrDepenSusp write FvlrDepenSusp;
  end;

  TinfoIRComplem = class(TObject)
  private
    FdtLaudo: TDateTime;
    FinfoDep: TinfoDepCollection;

    function getInfoDep: TinfoDepCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoDepInst: boolean;

    property dtLaudo: TDateTime read FdtLaudo write FdtLaudo;
    property infoDep: TinfoDepCollection read getInfoDep write FinfoDep;
  end;

  TinfoDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoDepCollectionItem;
    procedure SetItem(Index: Integer; const Value: TInfoDepCollectionItem);
  public
    function New: TInfoDepCollectionItem;
    property Items[Index: Integer]: TInfoDepCollectionItem read GetItem write SetItem; default;
  end;

  TinfoDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FdtNascto: TDateTime;
    Fnome: string;
    FdepIRRF: tpSimNaoFacultativo;
    FtpDep: tpTpDep;
    FdescrDep: string;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property nome: string read Fnome write Fnome;
    property depIRRF: tpSimNaoFacultativo read FdepIRRF write FdepIRRF;
    property tpDep: tpTpDep read FtpDep write FtpDep;
    property descrDep: string read FdescrDep write FdescrDep;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.Base,
  ACBreSocial;

{ TS2501Collection }

function TS2501Collection.GetItem(Index: Integer): TS2501CollectionItem;
begin
  Result := TS2501CollectionItem(inherited Items[Index]);
end;

procedure TS2501Collection.SetItem(Index: Integer;
  Value: TS2501CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2501Collection.New: TS2501CollectionItem;
begin
  Result := TS2501CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2501CollectionItem }

constructor TS2501CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento  := teS2501;
  FEvtContProc := TEvtContProc.Create(AOwner);
end;

destructor TS2501CollectionItem.Destroy;
begin
  FEvtContProc.Free;

  inherited;
end;

{ TIdeTrabCollection }

function TIdeTrabCollection.GetItem(Index: Integer): TIdeTrabCollectionItem;
begin
  Result := TIdeTrabCollectionItem(inherited Items[Index]);
end;

procedure TIdeTrabCollection.SetItem(Index: Integer; Value: TIdeTrabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeTrabCollection.New: TIdeTrabCollectionItem;
begin
  Result := TIdeTrabCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeTrabCollectionItem }

constructor TIdeTrabCollectionItem.Create;
begin
  inherited Create;

  FcalcTrib := nil;
  FinfoCRIRRF := nil;
  FinfoIRComplem := nil;
end;

destructor TIdeTrabCollectionItem.Destroy;
begin
  if instCalcTrib() then
    FreeAndNil(FcalcTrib);

  if instInfoCRIRRF() then
    FreeAndNil(FInfoCRIRRF);

  if instInfoIRComplem() then
    FreeAndNil(FinfoIRComplem);

  inherited;
end;

function TIdeTrabCollectionItem.getCalcTrib(): TCalcTribCollection;
begin
  if not Assigned(FcalcTrib) then
    FcalcTrib := TCalcTribCollection.Create;
  Result := FcalcTrib;
end;

function TIdeTrabCollectionItem.getInfoCRIRRF(): TInfoCRIRRFCollection;
begin
  if not Assigned(FInfoCRIRRF) then
    FInfoCRIRRF := TInfoCRIRRFCollection.Create;
  Result := FInfoCRIRRF;
end;

function TIdeTrabCollectionItem.getInfoIRComplem(): TinfoIRComplem;
begin
  if not Assigned(FInfoIRComplem) then
    FInfoIRComplem := TinfoIRComplem.Create;
  Result := FInfoIRComplem;
end;

function TIdeTrabCollectionItem.instCalcTrib(): boolean;
begin
  Result := Assigned(FcalcTrib);
end;

function TIdeTrabCollectionItem.instInfoCRIRRF(): boolean;
begin
  Result := Assigned(FInfoCRIRRF);
end;

function TIdeTrabCollectionItem.instInfoIRComplem(): boolean;
begin
  Result := Assigned(FInfoIRComplem);
end;
{ TCalcTribCollection }

function TCalcTribCollection.GetItem(Index: Integer): TCalcTribCollectionItem;
begin
  Result := TCalcTribCollectionItem(inherited Items[Index]);
end;

procedure TCalcTribCollection.SetItem(Index: Integer; Value: TCalcTribCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TCalcTribCollection.New: TCalcTribCollectionItem;
begin
  Result := TCalcTribCollectionItem.Create;
  Self.Add(Result);
end;

{ TCalcTribCollectionItem }

constructor TCalcTribCollectionItem.Create;
begin
  inherited Create;

  FinfoCRContrib := nil;
end;

destructor TCalcTribCollectionItem.Destroy;
begin
  if instInfoCRContrib() then
    FreeAndNil(FinfoCRContrib);

  inherited;
end;

function TCalcTribCollectionItem.getInfoCRContrib(): TInfoCRContribCollection;
begin
  if not Assigned(FinfoCRContrib) then
    FinfoCRContrib := TInfoCRContribCollection.Create;
  Result := FinfoCRContrib;
end;

function TCalcTribCollectionItem.instInfoCRContrib(): boolean;
begin
  Result := Assigned(FInfoCRContrib);
end;

{ TInfoCRContribCollection }

function TInfoCRContribCollection.GetItem(Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRContribCollection.New: TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoCRIRRFCollection }

function TInfoCRIRRFCollection.GetItem(Index: Integer): TInfoCRIRRFCollectionItem;
begin
  Result := TInfoCRIRRFCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRIRRFCollection.SetItem(Index: Integer; Value: TInfoCRIRRFCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRIRRFCollection.New: TInfoCRIRRFCollectionItem;
begin
  Result := TInfoCRIRRFCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoCRIRRFCollectionItem }

constructor TInfoCRIRRFCollectionItem.Create;
begin
  inherited Create;

  FinfoIR := nil;
  FinfoRRA := nil;
  FdedDepen := nil;
  FpenAlim := nil;
  FinfoProcRet := nil;
end;

destructor TInfoCRIRRFCollectionItem.Destroy;
begin
  if infoIRInst() then
    FreeAndNil(FinfoIR);

  if infoRRAInst() then
    FreeAndNil(FinfoRRA);

  if dedDepenInst() then
    FreeAndNil(FdedDepen);

  if penAlimInst() then
    FreeAndNil(FpenAlim);

  if infoProcRetInst() then
    FreeAndNil(FinfoProcRet);

  inherited;
end;

function TInfoCRIRRFCollectionItem.getInfoIR(): TinfoIRCollection;
begin
  if not Assigned(FinfoIR) then
    FinfoIR := TInfoIRCollection.Create;
  Result := FinfoIR;
end;

function TInfoCRIRRFCollectionItem.infoIRInst(): boolean;
begin
  Result := Assigned(FInfoIR);
end;

function TInfoCRIRRFCollectionItem.getInfoRRA(): TinfoRRA;
begin
  if not Assigned(FinfoRRA) then
    FinfoRRA := TinfoRRA.Create;
  Result := FinfoRRA;
end;

function TInfoCRIRRFCollectionItem.infoRRAInst(): boolean;
begin
  Result := Assigned(FinfoRRA);
end;

function TInfoCRIRRFCollectionItem.getDedDepen(): TdedDepenCollection;
begin
  if not Assigned(FdedDepen) then
    FdedDepen := TdedDepenCollection.Create;
  Result := FdedDepen;
end;

function TInfoCRIRRFCollectionItem.getPenAlim(): TpenAlimCollection;
begin
  if not Assigned(FpenAlim) then
    FpenAlim := TpenAlimCollection.Create;
  Result := FpenAlim;
end;

function TInfoCRIRRFCollectionItem.dedDepenInst(): boolean;
begin
  Result := Assigned(FdedDepen);
end;

function TInfoCRIRRFCollectionItem.penAlimInst(): boolean;
begin
  Result := Assigned(FpenAlim);
end;

function TInfoCRIRRFCollectionItem.getInfoProcRet(): TinfoProcRetCollection;
begin
  if not Assigned(FinfoProcRet) then
    FinfoProcRet := TinfoProcRetCollection.Create;
  Result := FinfoProcRet;
end;

function TInfoCRIRRFCollectionItem.infoProcRetInst(): boolean;
begin
  Result := Assigned(FinfoProcRet);
end;

{ TInfoIRCollection }

function TInfoIRCollection.GetItem(Index: Integer): TInfoIRCollectionItem;
begin
  Result := TInfoIRCollectionItem(inherited Items[Index]);
end;

procedure TInfoIRCollection.SetItem(Index: Integer; Value: TInfoIRCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoIRCollection.New: TInfoIRCollectionItem;
begin
  Result := TInfoIRCollectionItem.Create;
  Self.Add(Result);
end;

{ TpenAlimCollection }

function TpenAlimCollection.GetItem(Index: Integer): TpenAlimCollectionItem;
begin
  Result := TpenAlimCollectionItem(inherited Items[Index]);
end;

procedure TpenAlimCollection.SetItem(Index: Integer; const Value: TpenAlimCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TpenAlimCollection.New: TpenAlimCollectionItem;
begin
  Result := TpenAlimCollectionItem.Create;
  Self.Add(Result);
end;

{ TdedDepenCollection }

function TdedDepenCollection.GetItem(Index: Integer): TdedDepenCollectionItem;
begin
  Result := TdedDepenCollectionItem(inherited Items[Index]);
end;

procedure TdedDepenCollection.SetItem(Index: Integer; const Value: TdedDepenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdedDepenCollection.New: TdedDepenCollectionItem;
begin
  Result := TdedDepenCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoProcRetCollection }

function TinfoProcRetCollection.GetItem(Index: Integer): TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem(inherited Items[Index]);
end;

procedure TinfoProcRetCollection.SetItem(Index: Integer; const Value: TinfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoProcRetCollection.New: TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoProcRetCollectionItem }

constructor TinfoProcRetCollectionItem.Create;
begin
  inherited Create;

  FinfoValores := nil;
end;

destructor TinfoProcRetCollectionItem.Destroy;
begin
  if infoValoresInst() then
    FreeAndNil(FinfoValores);

  inherited;
end;

function TinfoProcRetCollectionItem.getInfoValores: TinfoValoresCollection;
begin
  if not Assigned(FinfoValores) then
    FinfoValores := TinfoValoresCollection.Create;
  Result := FinfoValores;
end;

function TinfoProcRetCollectionItem.infoValoresInst: boolean;
begin
  Result := Assigned(FinfoValores);
end;

{ TinfoValoresCollection }

function TinfoValoresCollection.GetItem(Index: Integer): TinfoValoresCollectionItem;
begin
  Result := TinfoValoresCollectionItem(inherited Items[Index]);
end;

procedure TinfoValoresCollection.SetItem(Index: Integer; const Value: TinfoValoresCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoValoresCollection.New: TinfoValoresCollectionItem;
begin
  Result := TinfoValoresCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoValoresCollectionItem }

constructor TinfoValoresCollectionItem.Create;
begin
  inherited Create;

  FdedSusp := nil;
end;

destructor TinfoValoresCollectionItem.Destroy;
begin
  if dedSuspInst() then
    FreeAndNil(FdedSusp);

  inherited;
end;

function TinfoValoresCollectionItem.getDedSusp: TdedSuspCollection;
begin
  if not Assigned(FdedSusp) then
    FdedSusp := TdedSuspCollection.Create;
  Result := FdedSusp;
end;

function TinfoValoresCollectionItem.dedSuspInst: boolean;
begin
  Result := Assigned(FdedSusp);
end;

{ TdedSuspCollection }

function TdedSuspCollection.GetItem(Index: Integer): TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem(inherited Items[Index]);
end;

procedure TdedSuspCollection.SetItem(Index: Integer; const Value: TdedSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdedSuspCollection.New: TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem.Create;
  Self.Add(Result);
end;

{ TdedSuspCollectionItem }

constructor TdedSuspCollectionItem.Create;
begin
  inherited Create;

  FbenefPen := nil;
end;

destructor TdedSuspCollectionItem.Destroy;
begin
  if benefPenInst() then
    FreeAndNil(FbenefPen);

  inherited;
end;

function TdedSuspCollectionItem.getBenefPen: TbenefPenCollection;
begin
  if not Assigned(FbenefPen) then
    FbenefPen := TbenefPenCollection.Create;
  Result := FbenefPen;
end;

function TdedSuspCollectionItem.benefPenInst: boolean;
begin
  Result := Assigned(FbenefPen);
end;

{ TbenefPenCollection }

function TbenefPenCollection.GetItem(Index: Integer): TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem(inherited Items[Index]);
end;

procedure TbenefPenCollection.SetItem(Index: Integer; const Value: TbenefPenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbenefPenCollection.New: TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoIRComplem }

constructor TinfoIRComplem.Create;
begin
  inherited Create;

  FinfoDep := nil;
end;

destructor TinfoIRComplem.Destroy;
begin
  if infoDepInst() then
    FreeAndNil(FinfoDep);

  inherited;
end;

function TinfoIRComplem.getInfoDep: TinfoDepCollection;
begin
  if not Assigned(FinfoDep) then
    FinfoDep := TinfoDepCollection.Create;
  Result := FinfoDep;
end;

function TinfoIRComplem.infoDepInst: boolean;
begin
  Result := Assigned(FinfoDep);
end;

{ TinfoDepCollection }

function TinfoDepCollection.GetItem(Index: Integer): TinfoDepCollectionItem;
begin
  Result := TinfoDepCollectionItem(inherited Items[Index]);
end;

procedure TinfoDepCollection.SetItem(Index: Integer; const Value: TinfoDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoDepCollection.New: TinfoDepCollectionItem;
begin
  Result := TinfoDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TEvtContProc }

constructor TEvtContProc.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FideEvento     := TideEvento2.Create;
  FideEmpregador := TideEmpregador.Create;
  FideProc       := TIdeProc.Create;
  FideTrab       := TIdeTrabCollection.Create;
end;

destructor TEvtContProc.Destroy;
begin
  FideEvento.Free;
  FideEmpregador.Free;
  FideProc.Free;
  FideTrab.Free;

  inherited;
end;

procedure TEvtContProc.GerarIdeTrab(obj: TIdeTrabCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('ideTrab cpfTrab="'+obj.Items[i].cpfTrab+'"');

    if obj.Items[i].instCalcTrib() then
      GerarCalcTrib(obj.Items[i].calcTrib);

    if obj.Items[i].instInfoCRIRRF() then
      GerarInfoCRIRRF(obj.items[i].infoCRIRRF);

    if obj.Items[i].instInfoIRComplem() then
      GerarInfoIRComplem(obj.items[i].infoIRComplem);

    Gerador.wGrupo('/ideTrab');
  end;
end;

procedure TEvtContProc.GerarInfoCRContrib(obj: TInfoCRContribCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoCRContrib tpCR="'+obj.Items[i].tpCR+'"'+
                                ' vrCR="'+FloatToString(obj.Items[i].vrCR, '.', FloatMask(2, False)) +'"'
                  );

    Gerador.wGrupo('/infoCRContrib');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoCRContrib', 'Informações das contribuições sociais devidas à Previdência Social e Outras Entidades e Fundos', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtContProc.GerarCalcTrib(obj: TCalcTribCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
//    Gerador.wGrupo('calcTrib');

    Gerador.wGrupo('calcTrib perRef="'+obj.Items[i].perRef+'"' +
                           ' vrBcCpMensal="'+ FloatToString(obj.Items[i].VrBcCpMensal, '.', FloatMask(2, False))+'"' +
                           ' vrBcCp13="'+FloatToString(obj.Items[i].VrBcCp13, '.', FloatMask(2, False))+'"' +
                           ifThen(VersaoDF < veS01_02_00,
                                  ' vrRendIRRF="'+ FloatToString(obj.Items[i].vrRendIRRF, '.', FloatMask(2, False))+'"' +
                                  ' vrRendIRRF13="' + FloatToString(obj.Items[i].vrRendIRRF13, '.', FloatMask(2, False))+'"',
                                  ''));

    if VersaoDF >= veS01_02_00 then
      if obj.Items[i].instInfoCRContrib() then
        GerarInfoCRContrib(obj.Items[i].infoCrContrib);

    Gerador.wGrupo('/calcTrib');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'calcTrib', 'Identificação do período e da base de cálculo dos tributos',
                    ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtContProc.GerarInfoCRIRRF(obj: TInfoCRIRRFCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoCRIRRF tpCR="' + obj.Items[i].tpCR + '"' +
                             ' vrCR="' + FloatToString(obj.Items[i].vrCR, '.', FloatMask(2, False))+'"'
                  );

    if VersaoDF >= veS01_02_00 then
    begin
      if obj[i].infoIRInst() then
        GerarInfoIR(obj[i].infoIR);

      if obj[i].infoRRAInst() then
        GerarInfoRRA(obj[i].infoRRA);

      if obj[i].dedDepenInst() then
        GerarDedDepen(obj[i].dedDepen);

      if obj[i].penAlimInst() then
        GerarPenAlim(obj[i].penAlim);

      if obj[i].infoProcRetInst() then
        GerarInfoProcRet(obj[i].infoProcRet);
    end;

    Gerador.wGrupo('/infoCRIRRF');
  end;
end;

procedure TEvtContProc.GerarInfoIR(obj: TinfoIRCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    if (obj[i].vrRendTrib > 0)      or (obj[i].vrRendTrib13 > 0)  or (obj[i].vrRendMoleGrave > 0) or
       (obj[i].vrRendIsen65 > 0)    or (obj[i].vrJurosMora > 0)   or (obj[i].vrRendIsenNTrib > 0) or
       (obj[i].descIsenNTrib <> '') or (obj[i].vrPrevOficial > 0) then
    begin
      Gerador.wGrupo('infoIR' + IfThen(obj[i].vrRendTrib > 0,      ' vrRendTrib="'      + FloatToString(obj[i].vrRendTrib, '.',   FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].vrRendTrib13 > 0,    ' vrRendTrib13="'    + FloatToString(obj[i].vrRendTrib13, '.', FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].vrRendMoleGrave > 0, ' vrRendMoleGrave="' + FloatToString(obj[i].vrRendMoleGrave, '.', FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].vrRendIsen65 > 0,    ' vrRendIsen65="'    + FloatToString(obj[i].vrRendIsen65, '.', FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].vrJurosMora > 0,     ' vrJurosMora="'     + FloatToString(obj[i].vrJurosMora, '.', FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].vrRendIsenNTrib > 0, ' vrRendIsenNTrib="' + FloatToString(obj[i].vrRendIsenNTrib, '.', FloatMask(2, False)) + '"', '')
                              + IfThen(obj[i].descIsenNTrib <> '', ' descIsenNTrib="'   + TiraAcentos(obj[i].descIsenNTrib)                            + '"', '')
                              + IfThen(obj[i].vrPrevOficial > 0,   ' vrPrevOficial="'   + FloatToString(obj[i].vrPrevOficial, '.', FloatMask(2, False)) + '"', '') 
                    );

      Gerador.wGrupo('/infoIR');
    end;
  end;
end;

procedure TEvtContProc.GerarInfoRRA(obj: TInfoRRA);
begin
  Gerador.wGrupo('infoRRA descRRA="' + TiraAcentos(obj.descRRA) + '"' +
                        ' qtdMesesRRA="' + FloatToString(obj.qtdMesesRRA, '.', FloatMask(1,False)) + '"'
                );

  if obj.instDespProcJud() then
    GerarDespProcJud(obj.despProcJud);

  if obj.instIdeAdv() then
    GerarIdeAdv(obj.ideAdv);

  Gerador.wGrupo('/infoRRA');
end;

procedure TEvtContProc.GerarIdeAdv(obj: TIdeAdvCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('ideAdv tpInsc="' + eSTpInscricaoToStr(obj[i].tpInsc) + '"' +
                         ' nrInsc="' + obj[i].nrInsc + '"' + 
                         IfThen(obj[i].vlrAdv > 0, ' vlrAdv="' + FloatToString(obj[i].vlrAdv, '.', FloatMask(2,False)) + '"', '')
                  );       

    Gerador.wGrupo('/ideAdv');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'ideAdv', 'Identificação dos advogados', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtContProc.GerarDespProcJud(obj: TDespProcJud);
begin
  Gerador.wGrupo('despProcJud vlrDespCustas="' + FloatToString(obj.vlrDespCustas, '.', FloatMask(2,False)) + '"' +
                            ' vlrDespAdvogados="' + FloatToString(obj.vlrDespAdvogados, '.', FloatMask(2,False)) + '"' 
                );

  Gerador.wGrupo('/despProcJud');
end;

procedure TEvtContProc.GerarIdeProc(obj: TIdeProc);
begin
  Gerador.wGrupo('ideProc');

  Gerador.wCampo(tcStr, '', 'nrProcTrab',  15,  20, 1, obj.nrProcTrab);
  Gerador.wCampo(tcStr, '', 'perApurPgto',  7,   7, 1, obj.perApurPgto);

  if obj.obs <> '' then
    Gerador.wCampo(tcStr, '', 'obs',        0, 999, 0, obj.obs);

  Gerador.wGrupo('/ideProc');
end;

function TEvtContProc.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtContProc');
    Gerador.wGrupo('evtContProc Id="' + Self.Id + '"');

    GerarIdeEvento2(self.ideEvento);
    GerarIdeEmpregador(self.ideEmpregador);
    GerarIdeProc(self.ideProc);
    GerarIdeTrab(self.ideTrab);

    Gerador.wGrupo('/evtContProc');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtContProc');

//    Validar(schevtContProc);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtContProc.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtContProc';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideProc';
      ideProc.nrProcTrab  := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      ideProc.perApurPgto := INIRec.ReadString(sSecao, 'perApurPgto', EmptyStr);
      ideProc.obs         := INIRec.ReadString(sSecao, 'obs', EmptyStr);

      I := 1;
      while true do
      begin
        // de 01 até 9999
        sSecao := 'ideTrab' + IntToStrZero(I, 4);
        sFim   := INIRec.ReadString(sSecao, 'cpfTrab', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrab.New do
        begin
          cpfTrab  := sFim;

          J := 1;
          while true do
          begin
            // de 000 até 999
            sSecao := 'calcTrib' + IntToStrZero(I, 4) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with calcTrib.New do
            begin
              perRef       := sFim;
              vrBcCpMensal := StringToFloat(INIRec.ReadString(sSecao, 'vrBcCpMensal', '0'));
              vrBcCp13     := StringToFloat(INIRec.ReadString(sSecao, 'vrBcCp13', '0'));
              vrRendIRRF   := StringToFloat(INIRec.ReadString(sSecao, 'vrRendIRRF', '0'));
              vrRendIRRF13 := StringToFloat(INIRec.ReadString(sSecao, 'vrRendIRRF13', '0'));

              K := 1;
              while true do
              begin
                // de 01 até 999
                sSecao := 'infoCRContrib' + IntToStrZero(I, 4) + IntToStrZero(J, 3) + IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpCR', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoCRContrib.New do
                begin
                  tpCR := sFim;
                  vrCR := StringToFloat(INIRec.ReadString(sSecao, 'vrCR', '0'));
                end;

                Inc(K);
              end;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'infoCRIRRF' + IntToStrZero(I, 4) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpCR', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoCRIRRF.New do
            begin
              tpCR := sFim;
              vrCR := StringToFloat(INIRec.ReadString(sSecao, 'vrCR', '0'));

              K := 1;
              while true do
              begin
                // de 0 até 1
                sSecao := 'infoIR' + IntToStrZero(I, 4) + IntToStrZero(J, 2) + IntToStrZero(K, 1);

                if ( (StringToFloat(INIRec.ReadString(sSecao, 'vrRendTrib', '0')) = 0 ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrRendTrib13', '0')) = 0 ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrRendMoleGrave', '0')) = 0 ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrRendIsen65', '0')) = 0 ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrJurosMora', '0')) = 0 ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrRendIsenNTrib', '0')) = 0 ) and
                     (INIRec.ReadString(sSecao, 'descIsenNTrib', '') = '' ) and
                     (StringToFloat(INIRec.ReadString(sSecao, 'vrPrevOficial', '0')) = 0 ) ) then
                  break;

                with InfoIR.New do
                begin
                  vrRendTrib := StringToFloat(INIRec.ReadString(sSecao, 'vrRendTrib', '0'));
                  vrRendTrib13 := StringToFloat(INIRec.ReadString(sSecao, 'vrRendTrib13', '0'));
                  vrRendMoleGrave := StringToFloat(INIRec.ReadString(sSecao, 'vrRendMoleGrave', '0'));
                  vrRendIsen65 := StringToFloat(INIRec.ReadString(sSecao, 'vrRendIsen65', '0'));
                  vrJurosMora := StringToFloat(INIRec.ReadString(sSecao, 'vrJurosMora', '0'));
                  vrRendIsenNTrib := StringToFloat(INIRec.ReadString(sSecao, 'vrRendIsenNTrib', '0'));
                  descIsenNTrib := INIRec.ReadString(sSecao, 'descIsenNTrib', '');
                  vrPrevOficial := StringToFloat(INIRec.ReadString(sSecao, 'vrPrevOficial', '0'));
                end;

                Inc(K);
              end;

              sSecao := 'infoRRA' + IntToStrZero(I, 4) + IntToStrZero(J, 2);
              with infoRRA do
              begin
                tpProcRRA := eSStrToTpProcRRA(Ok, sFim);
                nrProcRRA := INIRec.ReadString(sSecao, 'nrProcRRA', EmptyStr);
                descRRA := INIRec.ReadString(sSecao, 'descRRA', EmptyStr);
                qtdMesesRRA := INIRec.ReadInteger(sSecao, 'qtdMesesRRA', 0);
                despProcJud.vlrDespCustas := INIRec.ReadFloat(sSecao, 'vlrDespCustas',0);
                despProcJud.vlrDespAdvogados := INIRec.ReadFloat(sSecao, 'vlrDespAdvogados',0);

                K := 1;
                while True do
                begin
                  // de 01 até 99
                  sSecao := 'infoIR' + IntToStrZero(I, 4) + IntToStrZero(J, 2) + IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with ideAdv.New do
                  begin
                    tpInsc := eSStrToTpInscricao(Ok, sFim);
                    nrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
                    vlrAdv := INIRec.ReadFloat(sSecao, 'vlrAdv',0);
                  end;

                  Inc(K);
                end;
              end;

              K := 1;
              while true do
              begin
                // de 001 até 999
                sSecao := 'dedDepen' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                       IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'tpRend', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with DedDepen.New do
                begin
                  tpRend := StrToIntDef(sFim,1);
                  cpfDep := INIRec.ReadString(sSecao, 'cpfDep', '');
                  vlrDeducao := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDeducao', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'penAlim' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                      IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpRend', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with PenAlim.New do
                begin
                  tpRend := StrToIntDef(sFim,1);
                  cpfDep := INIRec.ReadString(sSecao, 'cpfDep', '');
                  vlrPensao := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPensao', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 50
                sSecao := 'infoProcRet' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                          IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpProcRet', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with InfoProcRet.New do
                begin
                  tpProcRet := eSStrTotpTpProcRet(Ok, sFim);
                  nrProcRet := INIRec.ReadString(sSecao, 'nrProcRet', '');
                  codSusp := INIRec.ReadString(sSecao, 'codSusp', '');

                  L := 1;
                  while true do
                  begin
                    // de 0 até 2
                    sSecao := 'infoValores' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                              IntToStrZero(K, 2) + IntToStrZero(L, 1);
                    sFim   := INIRec.ReadString(sSecao, 'indApuracao', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with InfoValores.New do
                    begin
                      indApuracao := eSStrToIndApuracao(Ok, sFim);
                      vlrNRetido := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRetido', ''), 0);
                      vlrDepJud := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepJud', ''), 0);
                      vlrCmpAnoCal := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoCal', ''), 0);
                      vlrCmpAnoAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoAnt', ''), 0);
                      vlrRendSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendSusp', ''), 0);

                      M := 1;
                      while true do
                      begin
                        // de 01 até 25
                        sSecao := 'dedSusp' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                              IntToStrZero(K, 2) + IntToStrZero(L, 1) +
                                              IntToStrZero(M, 2);

                        sFim   := INIRec.ReadString(sSecao, 'indTpDeducao', 'FIM');

                        if (sFim = 'FIM') then
                          break;

                        with DedSusp.New do
                        begin
                          indTpDeducao := eSStrTotpIndTpDeducaoT(Ok, sFim);
                          vlrDedSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedSusp', ''), 0);

                          N := 1;
                          while true do
                          begin
                            // de 01 até 99
                            sSecao := 'benefPen' + IntToStrZero(I, 4) + IntToStrZero(J, 2) +
                                                   IntToStrZero(K, 2) + IntToStrZero(L, 1) +
                                                   IntToStrZero(M, 2) + IntToStrZero(N, 2);

                            sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                            if (sFim = 'FIM') then
                              break;

                            with BenefPen.New do
                            begin
                              cpfDep := sFim;
                              vlrDepenSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepenSusp', ''), 0);
                            end;

                            Inc(N);
                          end;
                        end;

                        Inc(M);
                      end;
                    end;

                    Inc(L);
                  end;
                end;

                Inc(K);
              end;
            end;

            Inc(J);
          end;

          // infoIRComplem.dtLaudo não é obrigatória, verificar se exite o primeiro reg da infoDep.cpfDep
          sSecao := 'infoDep' + IntToStrZero(I, 4) + '001';
          sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

          if (sFim <> 'FIM') and (Length(sFim) > 0) then
          begin
            // de 0 até 1
            sSecao := 'infoIRComplem' + IntToStrZero(I, 4);

            with InfoIRComplem do
            begin
              dtLaudo := StringToDateTime(INIRec.ReadString(sSecao, 'dtLaudo', '0'));

              J := 1;
              while true do
              begin
                // de 001 até 999
                sSecao := 'infoDep' + IntToStrZero(I, 4) + IntToStrZero(J, 3);
                sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with InfoDep.New do
                begin
                  cpfDep := sFim;
                  dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                  nome := INIRec.ReadString(sSecao, 'nome', '');
                  depIRRF := eSStrToSimNaoFacultativo(ok, INIRec.ReadString(sSecao, 'depIRRF', ''));
                  tpDep := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '01'));
                  descrDep := INIRec.ReadString(sSecao, 'descrDep', '');
                end;

                Inc(J);
              end;
            end;
          end;
        end;

        Inc(I);
      end;

    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

procedure TEvtContProc.GerarDedDepen(obj: TdedDepenCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dedDepen tpRend="' + IntToStr(obj.Items[i].tpRend) + '"' +
                           ' cpfDep="' + obj[i].cpfDep + '"' +
                           ' vlrDeducao="' + FloatToString(obj.Items[i].vlrDeducao, '.', FloatMask(2, False)) + '"'
                  );

    Gerador.wGrupo('/dedDepen');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'dedDepen', 'Dedução do rendimento tributável relativa a dependentes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtContProc.GerarPenAlim(obj: TpenAlimCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('penAlim tpRend="' + IntToStr(obj.Items[i].tpRend) + '"' +
                          ' cpfDep="' + obj[i].cpfDep + '"' +
                          ' vlrPensao="' + FloatToString(obj.Items[i].vlrPensao, '.', FloatMask(2, False))+'"'
                  );

    Gerador.wGrupo('/penAlim');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'penAlim', 'Informação dos beneficiários da pensão alimentícia', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtContProc.GerarInfoProcRet(obj: TinfoProcRetCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoProcRet tpProcRet="' + eStpTpProcRetToStr(obj[i].tpProcRet) + '"' +
                              ' nrProcRet="' + obj[i].nrProcRet + '"' +
                              IfThen(obj[i].codSusp <> '',' codSusp="' + obj[i].codSusp + '"', '')
                  );

    if obj[i].infoValoresInst then
      GerarInfoValores(obj[i].infoValores);

    Gerador.wGrupo('/infoProcRet');
  end;

  if obj.Count > 50 then
    Gerador.wAlerta('', 'infoProcRet', 'Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtContProc.GerarInfoValores(obj: TinfoValoresCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    if (obj[i].vlrNRetido > 0) or (obj[i].vlrDepJud > 0) or (obj[i].vlrCmpAnoCal > 0) or
       (obj[i].vlrCmpAnoAnt > 0) or (obj[i].vlrRendSusp > 0) then
    begin
      Gerador.wGrupo('infoValores indApuracao="'  + eSIndApuracaoToStr(obj[i].indApuracao) + '"' 
                         + IfThen(obj[i].vlrNRetido > 0,   ' vlrNRetido="'   + FloatToString(obj[i].vlrNRetido,   '.', FloatMask(2, False)) + '"', '')
                         + IfThen(obj[i].vlrDepJud > 0,    ' vlrDepJud="'    + FloatToString(obj[i].vlrDepJud,    '.', FloatMask(2, False)) + '"', '')
                         + IfThen(obj[i].vlrCmpAnoCal > 0, ' vlrCmpAnoCal="' + FloatToString(obj[i].vlrCmpAnoCal, '.', FloatMask(2, False)) + '"', '')
                         + IfThen(obj[i].vlrCmpAnoAnt > 0, ' vlrCmpAnoAnt="' + FloatToString(obj[i].vlrCmpAnoAnt, '.', FloatMask(2, False)) + '"', '')
                         + IfThen(obj[i].vlrRendSusp > 0,  ' vlrRendSusp="'  + FloatToString(obj[i].vlrRendSusp,  '.', FloatMask(2, False)) + '"', '')
                    );

      if obj[i].dedSuspInst() then
        GerarDedSusp(obj[i].dedSusp);

      Gerador.wGrupo('/infoValores');
    end;
  end;

  if obj.Count > 2 then
    Gerador.wAlerta('', 'infoValores', 'Informações de valores relacionados a não retenção de tributos ou a depósitos judiciais', ERR_MSG_MAIOR_MAXIMO + '2');
end;

procedure TEvtContProc.GerarDedSusp(obj: TdedSuspCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dedSusp indTpDeducao="' + eStpTpIndTpDeducaoTToStr(obj[i].indTpDeducao) + '"' +
                        IfThen(obj[i].vlrDedSusp > 0, ' vlrDedSusp="' + FloatToString(obj[i].vlrDedSusp, '.', FloatMask(2, False)) + '"', '')
                  );

    if obj[i].benefPenInst() then
      GerarBenefPen(obj[i].benefPen);

    Gerador.wGrupo('/dedSusp');
  end;

  if obj.Count > 25 then
    Gerador.wAlerta('', 'dedSusp', 'Detalhamento das deduções com exigibilidade suspensa', ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TEvtContProc.GerarBenefPen(obj: TbenefPenCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('benefPen cpfDep="' + obj[i].cpfDep + '"' +
                           ' vlrDepenSusp="' + FloatToString(obj[i].vlrDepenSusp, '.', FloatMask(2, False)) + '"'
                  );

    Gerador.wGrupo('/benefPen');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'benefPen', 'Informação das deduções suspensas por dependentes e beneficiários da pensão alimentícia', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtContProc.GerarInfoIRComplem(obj: TinfoIRComplem);
begin
  if ((VersaoDF < veS01_02_00) or
      ((not obj.infoDepInst()) and (obj.dtLaudo = 0))) then
    exit;

  Gerador.wGrupo('infoIRComplem' 
                     + IfThen(obj.dtLaudo > 0, ' dtLaudo="' + FormatDateTime('YYYY-MM-DD', obj.dtLaudo)  + '"', '')
                );
  
  if obj.infoDepInst() then
    GerarInfoDep(obj.infoDep);

  Gerador.wGrupo('/infoIRComplem');
end;

procedure TEvtContProc.GerarInfoDep(obj: TinfoDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoDep cpfDep="' + obj[i].cpfDep + '"'
                        + IfThen(obj[i].dtNascto > 0, ' dtNascto="' + FormatDateTime('YYYY-MM-DD', obj[i].dtNascto) + '"', '')
                        + ' nome="' + TiraAcentos(obj[i].nome) + '"'
                        + IfThen(obj[i].depIRRF = snfSim, ' depIRRF="' + eSSimNaoFacultativoToStr(obj[i].depIRRF) + '"', '')
                        + IfThen(obj[i].tpDep <> tdNenhum, ' tpDep="' + eStpDepToStr(obj[i].tpDep) + '"', '')
                        + IfThen(obj[i].descrDep <> '', ' descrDep="' + TiraAcentos(obj[i].descrDep) + '"','')
                  );

    Gerador.wGrupo('/infoDep');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'infoDep', 'Informações de dependentes não cadastrados pelos eventos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

end.
