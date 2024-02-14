{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

unit pcesS2299;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2299CollectionItem = class;
  TEvtDeslig = class;
  TInfoDeslig = class;
  TVerbasRescS2299 = class;
  TDmDevCollectionS2299 = class;
  TDMDevCollectionItemS2299 = class;
  TInfoPerApur = class;
  TInfoPerAnt = class;
  TIdeADCCollection = class;
  TIdeADCItem = class;
  TIdePeriodoCollection = class;
  TIdePeriodoItem = class;
  TConsigFGTSCollection = class;
  TConsigFGTSItem = class;
  TtransfTit = class;
  TInfoTrabIntermCollection = class;
  TInfoTrabIntermItem = class;
  TProcCS = class;
  TinfoIntermCollection = class;
  TinfoIntermCollectionItem = class;
  TRemunAposDeslig = class;

  TS2299Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2299CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2299CollectionItem);
  public
    function Add: TS2299CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2299CollectionItem;
    property Items[Index: Integer]: TS2299CollectionItem read GetItem write SetItem; default;
  end;

  TS2299CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtDeslig : TEvtDeslig;
  public
    constructor Create(AOwner: Tcomponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtDeslig: TEvtDeslig read FEvtDeslig write FEvtDeslig;
  end;

  TEvtDeslig = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoDeslig: TInfoDeslig;

    procedure GerarInfoDeslig(obj: TInfoDeslig);
    procedure GerarSucessaoVinc(obj: TSucessaoVinc2);
    procedure GerarVerbasResc(obj: TVerbasRescS2299);
    procedure GerarProcCS(obj: TProcCS);
    procedure GerarDmDev(pDmDev: TDmDevCollectionS2299);
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
    procedure GerarIdeADC(pIdeADC: TIdeADCCollection);
    procedure GerarIdePeriodo(pIdePeriodo: TIdePeriodoCollection);
    procedure GerarconsigFGTS(obj: TConsigFGTSCollection);
    procedure GerarTransfTit(obj: TtransfTit);
    procedure GerarInfoTrabInterm(obj: TInfoTrabIntermCollection);
    procedure GerarinfoInterm(obj: TinfoIntermCollection);
    procedure GerarRemunAposDeslig(obj: TRemunAposDeslig);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoDeslig: TInfoDeslig read FInfoDeslig write FInfoDeslig;
  end;

  TInfoDeslig = class(TObject)
  private
    FmtvDeslig : String;
    FdtDeslig : TDateTime;
    FindPagtoAPI : tpSimNao;
    FdtProjFimAPI : TDateTime;
    FPensAlim : tpPensaoAlim;
    FpercAliment : Double;
    FVrAlim : Double;
    FnrCertObito : String;
    FnrProcTrab : String;
    FindPDV: tpSimNaoFacultativo;
    FIndCumprParc: tpCumprParcialAviso;
    FObservacao : String; // Descontinuado na versão 2.4.02
    Fobservacoes: TobservacoesCollection;
    FSucessaoVinc : TSucessaoVinc2;
    FVerbasResc : TVerbasRescS2299;
    FQuarentena : TQuarentena;
    FconsigFGTS : TConsigFGTSCollection;
    FInfoASO : TInfoASO;
    FtransfTit : TtransfTit;
    FQtdDiasInterm : Integer;
    FMudancaCPF : TMudancaCPF3;
    FCodCateg : Integer;
    FinfoInterm : TinfoIntermCollection;
    FdtAvPrv : TDateTime;
    FRemunAposDeslig: TRemunAposDeslig;

    function getVerbasResc: TVerbasRescS2299;
    function getInfoInterm: TinfoIntermCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function verbasRescInst(): boolean;
    function infoIntermInst(): boolean;

    property mtvDeslig: String read FmtvDeslig write FmtvDeslig;
    property dtDeslig: TDateTime read FdtDeslig write FdtDeslig;
    property indPagtoAPI : tpSimNao read FindPagtoAPI write FindPagtoAPI;
    property dtProjFimAPI : TDateTime read FdtProjFimAPI write FdtProjFimAPI;
    property percAliment : Double read FpercAliment write FpercAliment;
    property vrAlim: Double read FVrAlim write FVrAlim;
    property pensAlim: tpPensaoAlim read FPensAlim write FPensAlim;
    property nrCertObito : String read FnrCertObito write FnrCertObito;
    property nrProcTrab : String read FnrProcTrab write FnrProcTrab;
    property indPDV: tpSimNaoFacultativo read FindPDV write FindPDV;
    property indCumprParc: tpCumprParcialAviso read FIndCumprParc write FIndCumprParc;
    property Observacao : String read FObservacao write FObservacao;
    property observacoes: TobservacoesCollection read Fobservacoes write Fobservacoes;
    property SucessaoVinc : TSucessaoVinc2 read FSucessaoVinc write FSucessaoVinc;
    property VerbasResc : TVerbasRescS2299 read getVerbasResc write FVerbasResc;
    property Quarentena: TQuarentena read FQuarentena write FQuarentena;
    property consigFGTS: TConsigFGTSCollection read FconsigFGTS write FconsigFGTS;
    property InfoASO : TInfoASO read FInfoASO write FInfoASO;
    property transfTit: TtransfTit read FtransfTit write FtransfTit;
    property mudancaCPF: TMudancaCPF3 read FMudancaCPF write FMudancaCPF;
    property QtdDiasInterm: Integer read FQtdDiasInterm write FQtdDiasInterm;
    property CodCateg: Integer read FCodCateg write FCodCateg;
    property infoInterm: TinfoIntermCollection read getInfoInterm write FinfoInterm;
    property dtAvPrv: TDateTime read FdtAvPrv write FdtAvPrv;
    property remunAposDeslig: TRemunAposDeslig read FRemunAposDeslig write FRemunAposDeslig;
  end;

  TIdePeriodoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdePeriodoItem;
    procedure SetItem(Index: Integer; Value: TIdePeriodoItem);
  public
    function Add: TIdePeriodoItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdePeriodoItem;
    property Items[Index: Integer]: TIdePeriodoItem read GetItem write SetItem; default;
  end;

  TIdePeriodoItem = class(TObject)
  private
    FPerRef: string;
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property perRef: string read FPerRef write FPerRef;
    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TIdeADCCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeADCItem;
    procedure SetItem(Index: Integer; Value: TIdeADCItem);
  public
    function Add: TIdeADCItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeADCItem;
    property Items[Index: Integer]: TIdeADCItem read GetItem write SetItem; default;
  end;

  TIdeADCItem = class(TObject)
  private
    FDtAcConv: TDateTime;
    FTpAcConv: tpTpAcConv;
    FcompAcConv: String;
    FDtEfAcConv: TDateTime;
    FDsc: String;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtAcConv: TDateTime read FDtAcConv write FDtAcConv;
    property tpAcConv: tpTpAcConv read FTpAcConv write FTpAcConv;
    property compAcConv: String read FcompAcConv write FcompAcConv;
    property dtEfAcConv: TDateTime read FdtEfAcConv write FdtEfAcConv;
    property dsc: string read FDsc write FDsc;
    property idePeriodo: TIdePeriodoCollection read FIdePeriodo write FIdePeriodo;
  end;

  TInfoPerAnt = class(TObject)
  private
    FIdeADC: TIdeADCCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property ideADC: TIdeADCCollection read FIdeADC write FIdeADC;
  end;

  TInfoPerApur = class(TObject)
  private
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TinfoIntermCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoIntermCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoIntermCollectionItem);
  public
    function Add: TinfoIntermCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoIntermCollectionItem;
    property Items[Index: Integer]: TinfoIntermCollectionItem read GetItem write SetItem; default;
  end;

  TinfoIntermCollectionItem = class(TObject)
  private
    Fdia: Byte;
  public
    property dia: Byte read Fdia write Fdia;
  end;

  TDmDevCollectionS2299 = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDMDevCollectionItemS2299;
    procedure SetItem(Index: Integer; Value: TDMDevCollectionItemS2299);
  public
    function Add: TDMDevCollectionItemS2299; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItemS2299;
    property Items[Index: Integer]: TDMDevCollectionItemS2299 read GetItem write SetItem; default;
  end;

  TDMDevCollectionItemS2299 = class(TObject)
  private
    FIdeDmDev: string;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;
    FinfoTrabInterm: TinfoTrabIntermCollection;
    FindRRA: tpSimNaoFacultativo;
    FinfoRRA: TinfoRRA;

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
    function getInfoRRA: TInfoRRA;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPerApurInst: boolean;
    function infoPerAntInst: boolean;
    function infoRRAInst: boolean;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property indRRA: tpSimNaoFacultativo read FindRRA write FindRRA;
    property infoRRA: TinfoRRA read getInfoRRA write FinfoRRA;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
  end;

  TVerbasRescS2299 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollectionS2299;
    FProcCS: TProcCS;
  public
    constructor Create;
    destructor Destroy; override;

    property dmDev: TDmDevCollectionS2299 read FDmDev write FDmDev;
    property ProcCS: TProcCS read FProcCS write FProcCS;
  end;

  TConsigFGTSCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TConsigFGTSItem;
    procedure SetItem(Index: Integer; Value: TConsigFGTSItem);
  public
    function Add: TConsigFGTSItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TConsigFGTSItem;
    property Items[Index: Integer]: TConsigFGTSItem read GetItem write SetItem; default;
  end;

  TConsigFGTSItem = class(TObject)
  private
    FidConsig: tpSimNao;
    FinsConsig: string;
    FnrContr: string;
  public
    property idConsig: tpSimNao read FidConsig write FidConsig;
    property insConsig: string read FinsConsig write FinsConsig;
    property nrContr: string read FnrContr write FnrContr;
  end;

  TtransfTit = class(TObject)
  private
    FcpfSubstituto: string;
    FdtNascto: TDateTime;
  public
    property cpfSubstituto: string read FcpfSubstituto write FcpfSubstituto;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
  end;

  TInfoTrabIntermCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoTrabIntermItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabIntermItem);
  public
    function Add: TInfoTrabIntermItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoTrabIntermItem;
    property Items[Index: Integer]: TInfoTrabIntermItem read GetItem write SetItem; default;
  end;

  TInfoTrabIntermItem = class(TObject)
  private
    FcodConv: string;
  public
    property codConv: string read FcodConv write FcodConv;
  end;

  TProcCS = class(TObject)
  private
    FnrProcJud: String;
  public
    property nrProcJud: String read FnrProcJud write FnrProcJud;
  end;

  TRemunAposDeslig = class
  private
   FindRemun: TpIndRemun;
   FdtFimRemun: TDateTime;
  public
   property indRemun: TpIndRemun read FindRemun write FindRemun;
   property dtFimRemun: TDateTime read FdtFimRemun write FdtFimRemun;
  end;
  
implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2299Collection }

function TS2299Collection.Add: TS2299CollectionItem;
begin
  Result := Self.New;
end;

function TS2299Collection.GetItem(Index: Integer): TS2299CollectionItem;
begin
  Result := TS2299CollectionItem(inherited Items[Index]);
end;

procedure TS2299Collection.SetItem(Index: Integer; Value: TS2299CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2299Collection.New: TS2299CollectionItem;
begin
  Result := TS2299CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TInfoDeslig }

constructor TInfoDeslig.Create;
begin
  inherited;

  FSucessaoVinc    := TSucessaoVinc2.Create;
  FVerbasResc      := nil;
  FQuarentena      := TQuarentena.Create;
  FInfoASO         := TInfoASO.Create;
  FtransfTit       := TtransfTit.Create;
  FMudancaCPF      := TMudancaCPF3.Create;
  Fobservacoes     := TobservacoesCollection.Create;
  FconsigFGTS      := TConsigFGTSCollection.Create;
  FQtdDiasInterm   := -1;
  FinfoInterm      := nil;
  FRemunAposDeslig := TRemunAposDeslig.Create;
end;

destructor TInfoDeslig.Destroy;
begin
  FSucessaoVinc.Free;
  FreeAndNil(FVerbasResc);
  FQuarentena.Free;
  FInfoASO.Free;
  FtransfTit.Free;
  FMudancaCPF.Free;
  Fobservacoes.Free;
  FconsigFGTS.Free;
  FreeAndNIl(FinfoInterm);
  FreeAndNil(FRemunAposDeslig);

  inherited;
end;

function TInfoDeslig.getVerbasResc: TVerbasRescS2299;
begin
  if not assigned(FVerbasResc) then
    FVerbasResc := TVerbasRescS2299.Create;
  Result := FVerbasResc;
end;

function TInfoDeslig.verbasRescInst: boolean;
begin
  result := Assigned(FVerbasResc);
end;

function TInfoDeslig.getInfoInterm: TinfoIntermCollection;
begin
  if not(Assigned(FinfoInterm)) then
    FinfoInterm := TinfoIntermCollection.Create;
  Result := FinfoInterm;
end;

function TInfoDeslig.infoIntermInst: boolean;
begin
  Result := Assigned(FinfoInterm);
end;

{ TS2299CollectionItem }

constructor TS2299CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS2299;
  FEvtDeslig  := TEvtDeslig.Create(AOwner);
end;

destructor TS2299CollectionItem.Destroy;
begin
  FEvtDeslig.Free;

  inherited;
end;

{ TDmDevCollection }

function TDmDevCollectionS2299.Add: TDMDevCollectionItemS2299;
begin
  Result := Self.New;
end;

function TDmDevCollectionS2299.GetItem(Index: Integer): TDMDevCollectionItemS2299;
begin
  Result := TDMDevCollectionItemS2299(inherited Items[Index]);
end;

procedure TDmDevCollectionS2299.SetItem(Index: Integer; Value: TDMDevCollectionItemS2299);
begin
  inherited Items[Index] := Value;
end;

function TDmDevCollectionS2299.New: TDMDevCollectionItemS2299;
begin
  Result := TDMDevCollectionItemS2299.Create;
  Self.Add(Result);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItemS2299.Create;
begin
  inherited Create;
  FInfoPerApur    := nil;
  FInfoPerAnt     := nil;
  FInfoTrabInterm := TInfoTrabIntermCollection.Create;
  FInfoRRA        := nil;
end;

destructor TDMDevCollectionItemS2299.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
  FreeAndNil(FInfoTrabInterm);

  if infoRRAInst then
    FreeAndNil(FinfoRRA);

  inherited;
end;

function TDMDevCollectionItemS2299.getInfoPerApur: TInfoPerApur;
begin
  if not Assigned(FInfoPerApur) then
    FInfoPerApur := TInfoPerApur.Create;
  result := FInfoPerApur;
end;

function TDMDevCollectionItemS2299.getInfoPerAnt: TInfoPerAnt;
begin
  if not Assigned(FInfoPerAnt) then
    FInfoPerAnt := TInfoPerAnt.Create;
  result := FInfoPerAnt;
end;

function TDMDevCollectionItemS2299.infoPerApurInst: boolean;
begin
  result := Assigned(FInfoPerApur);
end;

function TDMDevCollectionItemS2299.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

function TDMDevCollectionItemS2299.getInfoRRA: TInfoRRA;
begin
  if not(Assigned(FInfoRRA)) then
    FInfoRRA := TInfoRRA.Create;
  Result := FInfoRRA;
end;

function TDMDevCollectionItemS2299.infoRRAInst: boolean;
begin
  Result := Assigned(FInfoRRA);
end;

{ TinfoIntermCollection }

function TinfoIntermCollection.Add: TinfoIntermCollectionItem;
begin
  Result := Self.New;
end;

function TinfoIntermCollection.GetItem(Index: Integer): TinfoIntermCollectionItem;
begin
  Result := TinfoIntermCollectionItem(inherited Items[Index]);
end;

procedure TinfoIntermCollection.SetItem(Index: Integer; Value: TinfoIntermCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoIntermCollection.New: TinfoIntermCollectionItem;
begin
  Result := TinfoIntermCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoPerApur }

constructor TInfoPerApur.Create;
begin
  inherited Create;
  FIdeEstabLot := TideEstabLotCollection.Create;
end;

destructor TInfoPerApur.Destroy;
begin
  FIdeEstabLot.Free;
  inherited;
end;

{ TInfoPerAnt }

constructor TInfoPerAnt.Create;
begin
  inherited Create;
  FIdeADC := TIdeADCCollection.Create;
end;

destructor TInfoPerAnt.Destroy;
begin
  FIdeADC.Free;
  inherited;
end;

{ TIdeADCCollection }

function TIdeADCCollection.Add: TIdeADCItem;
begin
  Result := Self.New;
end;

function TIdeADCCollection.GetItem(Index: Integer): TIdeADCItem;
begin
  Result := TIdeADCItem(inherited Items[Index]);
end;

procedure TIdeADCCollection.SetItem(Index: Integer; Value: TIdeADCItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeADCCollection.New: TIdeADCItem;
begin
  Result := TIdeADCItem.Create;
  Self.Add(Result);
end;

{ TIdeADCItem }

constructor TIdeADCItem.Create;
begin
  inherited Create;
  FIdePeriodo := TIdePeriodoCollection.Create;
end;

destructor TIdeADCItem.Destroy;
begin
  FIdePeriodo.Free;

  inherited;
end;

{ TIdePeriodoCollection }

function TIdePeriodoCollection.Add: TIdePeriodoItem;
begin
  Result := Self.New;
end;

function TIdePeriodoCollection.GetItem(Index: Integer): TIdePeriodoItem;
begin
  Result := TIdePeriodoItem(inherited Items[Index]);
end;

procedure TIdePeriodoCollection.SetItem(Index: Integer; Value: TIdePeriodoItem);
begin
  inherited Items[Index] := Value;
end;

function TIdePeriodoCollection.New: TIdePeriodoItem;
begin
  Result := TIdePeriodoItem.Create;
  Self.Add(Result);
end;

{ TIdePeriodoItem }

constructor TIdePeriodoItem.Create;
begin
  inherited Create;
  FIdeEstabLot := TideEstabLotCollection.Create;
end;

destructor TIdePeriodoItem.Destroy;
begin
  FIdeEstabLot.Free;

  inherited;
end;

{ TVerbasRescS2299 }

constructor TVerbasRescS2299.Create;
begin
  inherited Create;

  FDmDev := TDmDevCollectionS2299.Create;
  ProcCS := TProcCS.Create;
end;

destructor TVerbasRescS2299.Destroy;
begin
  FDMDev.Free;
  ProcCS.Free;

  inherited;
end;

{ TInfoTrabIntermCollection }

function TInfoTrabIntermCollection.Add: TInfoTrabIntermItem;
begin
  Result := Self.New;
end;

function TInfoTrabIntermCollection.GetItem(
  Index: Integer): TInfoTrabIntermItem;
begin
  Result := TInfoTrabIntermItem(inherited Items[Index]);
end;

procedure TInfoTrabIntermCollection.SetItem(Index: Integer;
  Value: TInfoTrabIntermItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoTrabIntermCollection.New: TInfoTrabIntermItem;
begin
  Result := TInfoTrabIntermItem.Create;
  Self.Add(Result);
end;

{ TConsigFGTSCollection }

function TConsigFGTSCollection.Add: TConsigFGTSItem;
begin
  Result := Self.New;
end;

function TConsigFGTSCollection.GetItem(Index: Integer): TConsigFGTSItem;
begin
  Result := TConsigFGTSItem(inherited Items[Index]);
end;

procedure TConsigFGTSCollection.SetItem(Index: Integer; Value: TConsigFGTSItem);
begin
  inherited Items[Index] := Value;
end;

function TConsigFGTSCollection.New: TConsigFGTSItem;
begin
  Result := TConsigFGTSItem.Create;
  Self.Add(Result);
end;

{ TEvtDeslig }

constructor TEvtDeslig.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVInculo    := TIdeVinculo.Create;
  FInfoDeslig    := TInfoDeslig.Create;
end;

destructor TEvtDeslig.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoDeslig.Free;

  inherited;
end;

procedure TEvtDeslig.GerarInfoDeslig(obj: TInfoDeslig);
begin
  Gerador.wGrupo('infoDeslig');

  Gerador.wCampo(tcStr, '', 'mtvDeslig',    1,  2, 1, obj.mtvDeslig);
  Gerador.wCampo(tcDat, '', 'dtDeslig',    10, 10, 1, obj.dtDeslig);

  if VersaoDF > ve02_05_00 then
    if obj.dtAvPrv > 0 then
      Gerador.wCampo(tcDat, '', 'dtAvPrv', 10, 10, 0, obj.dtAvPrv);

  Gerador.wCampo(tcStr, '', 'indPagtoAPI',  1,  1, 1, eSSimNaoToStr(obj.indPagtoAPI));

  if obj.indPagtoAPI = tpSim then
    Gerador.wCampo(tcDat, '', 'dtProjFimAPI', 10, 10, 0, obj.dtProjFimAPI);

  if obj.pensAlim <> paNenhum then
    Gerador.wCampo(tcStr, '', 'pensAlim',    1,  1, 1, obj.pensAlim);
  Gerador.wCampo(tcDe2, '', 'percAliment', 1,  5, 0, obj.percAliment);
  Gerador.wCampo(tcDe2, '', 'vrAlim',      1, 14, 0, obj.vrAlim);

  if VersaoDF <= ve02_05_00 then
    if ((obj.mtvDeslig='09') or (obj.mtvDeslig='10')) then
      Gerador.wCampo(tcStr, '', 'nrCertObito', 1, 32, 0, obj.nrCertObito);

  if (obj.mtvDeslig='17') then
    Gerador.wCampo(tcStr, '', 'nrProcTrab', 1, 20, 0, obj.nrProcTrab);

  if VersaoDF >= veS01_02_00 then
    if not (StrToIntDef(obj.mtvDeslig,0) in [10, 11, 12, 13, 28, 29, 30, 34, 36, 37, 40, 43, 44]) then
      if obj.indPDV = snfSim then
        Gerador.wCampo(tcStr, '', 'indPDV',  1,  1, 1, eSSimNaoFacultativoToStr(obj.indPDV));

  //O campo é sempre obrigatório para a categoria 111 (Intermitente)
  if (VersaoDF <= ve02_05_00) then
  begin
    Gerador.wCampo(tcStr, '', 'indCumprParc', 1,   1, 1, eSTpCumprParcialAvisoToStr(obj.indCumprParc));
    if ((obj.QtdDiasInterm >= 0) or (obj.CodCateg = 111))  then
      Gerador.wCampo(tcInt, '', 'qtdDiasInterm', 1,   2, 1, obj.QtdDiasInterm);
  end;

  if (VersaoDF >= veS01_00_00) and (obj.infoIntermInst()) then
    GerarinfoInterm(obj.infoInterm);

  if VersaoDF = ve02_04_01 then
    Gerador.wCampo(tcStr, '', 'observacao',   1, 255, 0, obj.Observacao)
  else
    GerarObservacoes(obj.observacoes);

  if (StrToIntDef(obj.mtvDeslig,0) in [11, 12, 13, 25, 28, 29, 30]) then
     GerarSucessaoVinc(obj.SucessaoVinc);

  if (obj.transfTit.cpfSubstituto <> '') And (obj.mtvDeslig='34') then
    GerarTransfTit(obj.transfTit);

  if (VersaoDF >= ve02_05_00) and (obj.mtvDeslig = '36') then
    GerarMudancaCPF3(obj.mudancaCPF);

  if (obj.verbasRescInst) and  (obj.mtvDeslig <> '11') and (obj.mtvDeslig <>'12') and
     (obj.mtvDeslig <>'13') and (obj.mtvDeslig <> '25') and (obj.mtvDeslig <> '28') and
     (obj.mtvDeslig <> '29') and (obj.mtvDeslig <> '30') and (obj.mtvDeslig <> '34') and
     (obj.mtvDeslig <> '36') then
    GerarVerbasResc(obj.VerbasResc);

  GerarQuarentena(obj.Quarentena);
  GerarRemunAposDeslig(obj.remunAposDeslig);
  GerarconsigFGTS(obj.consigFGTS);

  Gerador.wGrupo('/infoDeslig');
end;

procedure TEvtDeslig.GerarSucessaoVinc(obj: TSucessaoVinc2);
begin
  if (obj.cnpjSucessora <> EmptyStr) or (obj.nrInsc <> EmptyStr) then
  begin
    Gerador.wGrupo('sucessaoVinc');

    if VersaoDF <= ve02_05_00 then
    begin
      if VersaoDF >= ve02_05_00 then
        Gerador.wCampo(tcStr, '', 'tpInscSuc',    1,  1, 1, eSTpInscricaoToStr(obj.tpInscSuc));

      Gerador.wCampo(tcStr, '', 'cnpjSucessora', 14, 14, 1, obj.cnpjSucessora);
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'tpInsc',         1,  1, 1, eSTpInscricaoToStr(obj.tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc',        14, 14, 1, obj.nrInsc);
    end;

    Gerador.wGrupo('/sucessaoVinc');
  end;
end;

procedure TEvtDeslig.GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
begin
  Gerador.wGrupo('infoPerApur');

  GerarIdeEstabLot(pInfoPerApur.ideEstabLot);

  Gerador.wGrupo('/infoPerApur');
end;

procedure TEvtDeslig.GerarIdePeriodo(pIdePeriodo: TIdePeriodoCollection);
var
  i: Integer;
begin
  for i := 0 to pIdePeriodo.Count -1 do
  begin
    Gerador.wGrupo('idePeriodo');

    Gerador.wCampo(tcStr, '', 'perRef', 7, 7, 1, pIdePeriodo[i].perRef);

    GerarIdeEstabLot(pIdePeriodo[i].ideEstabLot);

    Gerador.wGrupo('/idePeriodo');
  end;

  if pIdePeriodo.Count > 180 then
    Gerador.wAlerta('', 'idePeriodo', 'Lista de Identificação de Periodos', ERR_MSG_MAIOR_MAXIMO + '180');
end;

procedure TEvtDeslig.GerarIdeADC(pIdeADC: TIdeADCCollection);
var
  i: integer;
begin
  for i := 0 to pIdeADC.Count - 1 do
  begin
    Gerador.wGrupo('ideADC');

    Gerador.wCampo(tcDat, '', 'dtAcConv',   10,  10, 0, pIdeADC[i].dtAcConv);
    Gerador.wCampo(tcStr, '', 'tpAcConv',    1,   1, 1, eSTpAcConvToStr(pIdeADC[i].tpAcConv));
    Gerador.wCampo(tcStr, '', 'compAcConv',  7,   7, 0, pIdeADC[i].compAcConv);
    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcDat, '', 'dtEfAcConv', 10,  10, 1, pIdeADC[i].dtEfAcConv);
    Gerador.wCampo(tcStr, '', 'dsc',         1, 255, 1, pIdeADC[i].dsc);

    GerarIdePeriodo(pIdeADC[i].idePeriodo);

    Gerador.wGrupo('/ideADC');
  end;

  if pIdeADC.Count > 8 then
    Gerador.wAlerta('', 'ideADC', 'Lista de Identificação de ADC', ERR_MSG_MAIOR_MAXIMO + '8');
end;

procedure TEvtDeslig.GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
begin
  Gerador.wGrupo('infoPerAnt');

  GerarIdeADC(pInfoPerAnt.ideADC);

  Gerador.wGrupo('/infoPerAnt');
end;

procedure TEvtDeslig.GerarDmDev(pDmDev: TDmDevCollectionS2299);
var
  i: integer;
begin
  for i := 0 to pDmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcStr, '', 'ideDmDev', 1, 30, 1, pDmDev[i].ideDmDev);

    if VersaoDF >= veS01_01_00 then
    begin
      if (pDmDev[i].indRRA = snfSim) and (pDmDev[i].infoRRAInst()) then
      begin
        Gerador.wCampo(tcStr, '', 'indRRA', 1,  1, 1, eSSimNaoFacultativoToStr(pDmDev[i].indRRA));

        if (pDmDev[i].infoRRAInst()) then
          GerarInfoRRA(pDmDev[i].infoRRA);
      end;
    end;

    if pDmDev[i].infoPerApurInst then
      GerarInfoPerApur(pDmDev[i].infoPerApur);

    if pDmDev[i].infoPerAntInst then
      GerarInfoPerAnt(pDmDev[i].infoPerAnt);

    if VersaoDF <= ve02_05_00 then
      GerarInfoTrabInterm(pDmDev[i].infoTrabInterm);

    Gerador.wGrupo('/dmDev');
  end;

  if pDmDev.Count > 50 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demonstrativos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtDeslig.GerarVerbasResc(obj: TVerbasRescS2299);
begin
  Gerador.wGrupo('verbasResc');

  GerarDmDev(obj.dmDev);
  GerarProcJudTrab(obj.ProcJudTrab);

  if obj.infoMVInst then
    GerarInfoMV(obj.infoMV);

  if (VersaoDF >= ve02_04_02) then
    GerarProcCS(obj.ProcCS);

  Gerador.wGrupo('/verbasResc');
end;

function TEvtDeslig.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtDeslig');
    Gerador.wGrupo('evtDeslig Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento, True, True, True);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo, False);
    GerarInfoDeslig(Self.InfoDeslig);

    Gerador.wGrupo('/evtDeslig');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtDeslig');

//    Validar(schevtDeslig);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

procedure TEvtDeslig.GerarconsigFGTS(obj: TConsigFGTSCollection);
var
  i: Integer;
begin
  if VersaoDF = ve02_04_01 then
  begin
    // Na versão 02.04.01 o grupo <consigFGTS> é obrigatório e a quantidade de
    // ocorrências é 1
    if obj.Count = 1 then
    begin
      Gerador.wGrupo('consigFGTS');

      Gerador.wCampo(tcStr, '', 'idConsig',  1,  1, 1, eSSimNaoToStr(obj[0].idConsig));
      Gerador.wCampo(tcStr, '', 'insConsig', 0,  5, 0, obj[0].insConsig);
      Gerador.wCampo(tcStr, '', 'nrContr',   0, 40, 0, obj[0].nrContr);

      Gerador.wGrupo('/consigFGTS');
    end;
  end
  else
  begin
    // Na versão 02.04.02 o grupo <consigFGTS> é opcional e a quantidade de
    // ocorrências é 9
    for i := 0 to obj.Count -1 do
    begin
       Gerador.wGrupo('consigFGTS');

       Gerador.wCampo(tcStr, '', 'insConsig', 0,  5, 0, obj[i].insConsig);
       Gerador.wCampo(tcStr, '', 'nrContr',   0, 40, 0, obj[i].nrContr);

       Gerador.wGrupo('/consigFGTS');
    end;

    if obj.Count > 99 then
      Gerador.wAlerta('', 'consigFGTS', 'Informações sobre operação de crédito consignado com garantia de FGTS', ERR_MSG_MAIOR_MAXIMO + '99')
  end;
end;

procedure TEvtDeslig.GerarTransfTit(obj: TtransfTit);
begin
  Gerador.wGrupo('transfTit');

  Gerador.wCampo(tcStr, '', 'cpfSubstituto', 11, 11, 1, obj.cpfSubstituto);
  Gerador.wCampo(tcDat, '', 'dtNascto',      10, 10, 1, obj.dtNascto);

  Gerador.wGrupo('/transfTit');
end;

procedure TEvtDeslig.GerarInfoTrabInterm(obj: TInfoTrabIntermCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoTrabInterm');

    Gerador.wCampo(tcStr, '', 'codConv', 1, 30, 1, obj[i].codConv);

    Gerador.wGrupo('/infoTrabInterm');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoTrabInterm', 'Lista de Trabalhos Intermitente', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtDeslig.GerarinfoInterm(obj: TinfoIntermCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoInterm');

    Gerador.wCampo(tcInt, '', 'dia', 1, 2, 1, obj[i].dia);

    Gerador.wGrupo('/infoInterm');
  end;

  if obj.Count > 31 then
    Gerador.wAlerta('', 'infoInterm', 'Informações relativas ao trabalho intermitente', ERR_MSG_MAIOR_MAXIMO + '31');
end;

procedure TEvtDeslig.GerarProcCS(obj: TProcCS);
begin
  if Trim(obj.nrProcJud) <> '' then
  begin
     Gerador.wGrupo('procCS');

     Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, obj.nrProcJud);

     Gerador.wGrupo('/procCS');
  end;
end;

procedure TEvtDeslig.GerarRemunAposDeslig(obj: TRemunAposDeslig);
begin
  if (VersaoDF >= veS01_01_00) and (obj.indRemun <> ireNaoInformado) then
  begin
    Gerador.wGrupo('remunAposDeslig');

    Gerador.wCampo(tcStr, '', 'indRemun',         1,  1, 1, TpIndRemunToStr(Obj.indRemun));
    Gerador.wCampo(tcDat, '', 'dtFimRemun',      10, 10, 1, Obj.dtFimRemun);

    Gerador.wGrupo('/remunAposDeslig');
  end;
end;

function TEvtDeslig.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  teste, sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtDeslig';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      IdeEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.NisTrab   := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'infoDeslig';
      infoDeslig.mtvDeslig    := INIRec.ReadString(sSecao, 'mtvDeslig', EmptyStr);
      InfoDeslig.dtAvPrv      := StringToDateTime(INIRec.ReadString(sSecao, 'dtAvPrv', '0'));
      infoDeslig.dtDeslig     := StringToDateTime(INIRec.ReadString(sSecao, 'dtDeslig', '0'));
      infoDeslig.indPagtoAPI  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPagtoAPI', 'S'));
      infoDeslig.dtProjFimAPI := StringToDateTime(INIRec.ReadString(sSecao, 'dtProjFimAPI', '0'));
      infoDeslig.pensAlim     := eSStrToTpPensaoAlimEx(INIRec.ReadString(sSecao, 'pensAlim', ''));
      infoDeslig.percAliment  := StringToFloatDef(INIRec.ReadString(sSecao, 'percAliment', ''), 0);
      infoDeslig.vrAlim       := StringToFloatDef(INIRec.ReadString(sSecao, 'vrAlim', ''), 0);
      infoDeslig.nrCertObito  := INIRec.ReadString(sSecao, 'nrCertObito', EmptyStr);
      infoDeslig.nrProcTrab   := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      infoDeslig.indPDV       := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indPDV', '1'));
      infoDeslig.indCumprParc := eSStrToTpCumprParcialAviso(Ok, INIRec.ReadString(sSecao, 'indCumprParc', '0'));
      infoDeslig.qtdDiasInterm := INIRec.ReadInteger(sSecao, 'qtdDiasInterm', -1);
      infoDeslig.Observacao   := INIRec.ReadString(sSecao, 'observacao', EmptyStr);
      infoDeslig.dtAvPrv      := StringToDateTime(INIRec.ReadString(sSecao, 'dtAvPrv', '0'));   //20/05/2021

      K := 1;
      while true do
      begin
        // de 01 até 31
        sSecao := 'infoInterm' + IntToStrZero(K, 2);
        sFim   := INIRec.ReadString(sSecao, 'dia', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with InfoDeslig.infoInterm.New do
        begin
          dia := INIRec.ReadInteger(sSecao, 'dia', 0);
        end;

        Inc(K);
      end;

      K := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'observacoes' + IntToStrZero(K, 2);
        sFim   := INIRec.ReadString(sSecao, 'observacao', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with InfoDeslig.observacoes.New do
        begin
          observacao := sFim;
        end;

        Inc(K);
      end;

      sSecao := 'sucessaoVinc';
      if INIRec.ReadString(sSecao, 'tpInscSuc', '') <> '' then
        infoDeslig.sucessaoVinc.tpInscSuc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscSuc', EmptyStr));

      if INIRec.ReadString(sSecao, 'cnpjSucessora', '') <> '' then
        infoDeslig.sucessaoVinc.cnpjSucessora := INIRec.ReadString(sSecao, 'cnpjSucessora', EmptyStr);

      infoDeslig.sucessaoVinc.tpInsc  := eSStrToTpInscricao(ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      infoDeslig.sucessaoVinc.nrInsc  :=  INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'transfTit';
      if INIRec.ReadString(sSecao, 'cpfSubstituto', '') <> '' then
      begin
        infoDeslig.transfTit.cpfSubstituto := INIRec.ReadString(sSecao, 'cpfSubstituto', EmptyStr);
        infoDeslig.transfTit.dtNascto      := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      end;

      sSecao := 'mudancaCPF';
      if INIRec.ReadString(sSecao, 'novoCPF', '') <> '' then
      begin
        infoDeslig.mudancaCPF.novoCPF := INIRec.ReadString(sSecao, 'novoCPF', EmptyStr);
      end;

      I := 1;
      while true do
      begin
        // de 01 até 50
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.VerbasResc.dmDev.New do
        begin
          ideDmDev := sFim;
          indRRA   := eSStrToSimNaoFacultativo(OK, INIRec.ReadString(sSecao, 'indRRA', EmptyStr));

          sSecao := 'infoRRA' + IntToStrZero(I, 2);
          infoRRA.tpProcRRA   := eSStrToTpProcRRA(Ok, INIRec.ReadString(sSecao, 'tpProcRRA', EmptyStr));
          infoRRA.nrProcRRA   := INIRec.ReadString(sSecao, 'nrProcRRA', EmptyStr);
          infoRRA.descRRA     := INIRec.ReadString(sSecao, 'descRRA', EmptyStr);
          infoRRA.qtdMesesRRA := INIRec.ReadFloat(sSecao, 'qtdMesesRRA', 0);

          sSecao := 'despProcJud' + IntToStrZero(I, 2);
          infoRRA.despProcJud.vlrDespCustas    := INIRec.ReadFloat(sSecao, 'vlrDespCustas', 0);
          infoRRA.despProcJud.vlrDespAdvogados := INIRec.ReadFloat(sSecao, 'vlrDespAdvogados', 0);

          J := 1;
          while true do
          begin
            // de 00 até 99
            sSecao := 'ideAdv' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoRRA.ideAdv.New do
            begin
              tpInsc := eSStrToTpInscricao(Ok, sFim);
              nrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
              vlrAdv := INIRec.ReadFloat(sSecao, 'vlrAdv', 0);;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 24
            sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerApur.ideEstabLot.New do
            begin
              tpInsc     := eSStrToTpInscricao(Ok, sFim);
              NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
              codLotacao := INIRec.ReadString(sSecao, 'codLotacao', EmptyStr);

              K := 1;
              while true do
              begin
                // de 001 até 200
                sSecao := 'detVerbas' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                            IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with detVerbas.New do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                  indApurIR  := eSStrToTpindApurIR(ok, INIRec.ReadString(sSecao, 'indApurIR', '0'));  //20/05/2021
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'detOper' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                            IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoSaudeColet.detOper.New do
                begin
                  cnpjOper := sFim;
                  regANS   := INIRec.ReadString(sSecao, 'regANS', EmptyStr);
                  vrPgTit  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrPgTit', ''), 0);

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detPlano' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                IntToStrZero(K, 2) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'tpDep', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with detPlano.New do
                    begin
                      tpDep    := eSStrToTpDep(Ok, sFim);
                      cpfDep   := INIRec.ReadString(sSecao, 'cpfDep', EmptyStr);
                      nmDep    := INIRec.ReadString(sSecao, 'nmDep', EmptyStr);
                      dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                      vlrPgDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgDep', ''), 0);

                    end;

                    Inc(L);
                  end;

                end;

                Inc(K);
              end;

              sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              if INIRec.ReadString(sSecao, 'grauExp', '') <> '' then
                infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));

              sSecao := 'infoSimples' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              if INIRec.ReadString(sSecao, 'indSimples', '') <> '' then
                infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));
            end;

            Inc(J);
          end;

          K := 1;
          while true do
          begin
            // de 1 até 8
            sSecao := 'ideADC' + IntToStrZero(I, 2) + IntToStrZero(K, 1);
            sFim   := INIRec.ReadString(sSecao, 'dtAcConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerAnt.ideADC.New do
            begin
              dtAcConv   := StringToDateTime(sFim);
              tpAcConv   := eSStrToTpAcConv(Ok, INIRec.ReadString(sSecao, 'tpAcConv', 'A'));
              compAcConv := INIRec.ReadString(sSecao, 'compAcConv', EmptyStr);
              if VersaoDF <= ve02_05_00 then
                dtEfAcConv := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfAcConv', '0'));
              dsc        := INIRec.ReadString(sSecao, 'dsc', EmptyStr);

              L := 1;
              while true do
              begin
                // de 001 até 180
                sSecao := 'idePeriodo' + IntToStrZero(I, 2) +
                            IntToStrZero(K, 1) + IntToStrZero(L, 3);
                sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with idePeriodo.New do
                begin
                  perRef := sFim;

                  M := 1;
                  while true do
                  begin
                    // de 01 até 24
                    sSecao := 'ideEstabLot' + IntToStrZero(I, 2) +
                                IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                IntToStrZero(M, 2);
                    sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with ideEstabLot.New do
                    begin
                      tpInsc     := eSStrToTpInscricao(Ok, sFim);
                      NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
                      codLotacao := INIRec.ReadString(sSecao, 'codLotacao', EmptyStr);

                      N := 1;
                      while true do
                      begin
                        // de 001 até 200
                        sSecao := 'detVerbas' + IntToStrZero(I, 2) +
                                IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                IntToStrZero(M, 2) + IntToStrZero(N, 3);
                        sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with detVerbas.New do
                        begin
                          codRubr    := sFim;
                          ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                          qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                          fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                          vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                          vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                        end;

                        Inc(N);
                      end;

                      sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) +
                                IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                IntToStrZero(M, 2);
                      if INIRec.ReadString(sSecao, 'grauExp', '') <> '' then
                        infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));

                      sSecao := 'infoSimples' + IntToStrZero(I, 2) +
                                IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                IntToStrZero(M, 2);
                      if INIRec.ReadString(sSecao, 'indSimples', '') <> '' then
                        infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));
                    end;

                    Inc(M);
                  end;
                end;

                Inc(L);
              end;
            end;

            Inc(K);
          end;

          J := 1;
          while true do
          begin
            // de 00 até 99
            sSecao := 'infoTrabInterm' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoTrabInterm.New do
            begin
              codConv := sFim;
            end;

            Inc(J);
          end;
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpTrib', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.VerbasResc.procJudTrab.New do
        begin
          tpTrib    := eSStrToTpTributo(Ok, sFim);
          nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);
          codSusp   := INIRec.ReadString(sSecao, 'codSusp', '');
        end;

        Inc(I);
      end;

      sSecao := 'infoMV';
      if INIRec.ReadString(sSecao, 'indMV', '') <> '' then
      begin
        infoDeslig.VerbasResc.infoMV.indMV := eSStrToIndMV(Ok, INIRec.ReadString(sSecao, 'indMV', '1'));

        I := 1;
        while true do
        begin
          // de 01 até 999
          sSecao := 'remunOutrEmpr' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoDeslig.VerbasResc.infoMV.remunOutrEmpr.New do
          begin
            TpInsc     := eSStrToTpInscricao(Ok, sFim);
            NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
            codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
            vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
          end;

          Inc(I);
        end;
      end;

      sSecao := 'ProcCS';
      if INIRec.ReadString(sSecao, 'nrProcJud', '') <> '' then
        infoDeslig.VerbasResc.ProcCS.nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');

      sSecao := 'quarentena';
      if INIRec.ReadString(sSecao, 'dtFimQuar', '') <> '' then
        infoDeslig.quarentena.dtFimQuar := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimQuar', '0'));

      sSecao := 'remunAposDeslig';
      teste := INIRec.ReadString(sSecao, 'indRemun', '');
      if teste <> '' then
      begin
        infoDeslig.remunAposDeslig.indRemun := StrToTpIndRemun(Ok, INIRec.ReadString(sSecao, 'indRemun', '0'));
        infoDeslig.remunAposDeslig.dtFimRemun := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimRemun', '0'));
      end;

      I := 1;
      while true do
      begin
        // de 01 até 10
        sSecao := 'consigFGTS' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'insConsig', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.consigFGTS.New do
        begin
          insConsig  := sFim;
          nrContr   := INIRec.ReadString(sSecao, 'nrContr', EmptyStr);
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

end.
