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

unit pcesS1200;

interface

uses
  SysUtils, 
	Classes, 
	Controls,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
		System.Generics.Collections, 
		System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
		System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, 
	pcnGerador, 
	pcnConsts,
  pcesCommon, 
	pcesConversaoeSocial, 
	pcesGerador;

type
  TRemunPer1200Collection = class;
  TRemunPer1200CollectionItem = class;
  TIdeEstabLotCollectionS1200 = class;
  TIdeEstabLotCollectionItemS1200 = class;
  TIdePeriodoCollectionItem = class;
  TIdePeriodoCollection = class;
  TIdeADCCollectionItem = class;
  TIdeADCCollection = class;
  TInfoPerAnt = class;
  TInfoPerApur = class;
  TeS1200IdeTrabalhador = class;
  TInfoComplem = class;
  TEvtRemun = class;
  TS1200CollectionItem = class;
  TS1200Collection = class;
  TDMDevCollectionS1200 = class;
  TDMDevCollectionItemS1200 = class;
  TSucessaoVinc = class;
  TInfoTrabIntermCollectionItem = class;
  TInfoTrabIntermCollection = class;
  TInfoComplCont = class;
  TinfoIntermCollection = class;
  TinfoIntermCollectionItem = class;

  TS1200Collection = class(TeSocialCollection)
  private
    function GetItem(Index: integer): TS1200CollectionItem;
    procedure SetItem(Index: integer; Value: TS1200CollectionItem);
  public
    function Add: TS1200CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1200CollectionItem;
    property Items[Index: integer]: TS1200CollectionItem read GetItem write SetItem; default;
  end;

  TS1200CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtRemun: TEvtRemun;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRemun: TEvtRemun read FEvtRemun write FEvtRemun;
  end;

  TDMDevCollectionS1200 = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TDMDevCollectionItemS1200;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItemS1200);
  public
    function Add: TDMDevCollectionItemS1200; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItemS1200;
    property Items[Index: integer]: TDMDevCollectionItemS1200 read GetItem write SetItem; default;
  end;

  TDMDevCollectionItemS1200 = class(TObject)
  private
    FIdeDmDev: string;
    FCodCateg: integer;
    FindRRA: tpSimNaoFacultativo;
    FinfoRRA: TinfoRRA;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;
    FinfoTrabInterm: TinfoTrabIntermCollection;
    FinfoComplCont: TInfoComplCont;

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
    function getInfoComplCont: TInfoComplCont;
    function getInfoRRA: TInfoRRA;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPerApurInst(): boolean;
    function infoPerAntInst(): boolean;
    function infoComplContInst(): boolean;
    function infoTrabIntermInst(): boolean;
    function infoRRAInst(): boolean;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property codCateg: integer read FCodCateg write FCodCateg;
    property indRRA: tpSimNaoFacultativo read FindRRA write FindRRA;
    property infoRRA: TinfoRRA read getInfoRRA write FinfoRRA;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
    property infoComplCont: TInfoComplCont read getInfoComplCont write FinfoComplCont;
  end;

  TEvtRemun = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TeS1200IdeTrabalhador;
    FDMDev: TDMDevCollectionS1200;

    { Geradores específicos desta classe }
    procedure GerarIdeEstabLot(objIdeEstabLot: TIdeEstabLotCollectionS1200; const nomeRemunPer: string = 'remunPerApur');
    procedure GerarIdeEstabLot2(objIdeEstabLot: TIdeEstabLotCollectionS1200; const nomeRemunPer: string = 'remunPerAnt');
    procedure GerarRemunPer(objRemunPer: TRemunPer1200Collection; const nomeRemunPer: string = 'remunPerApur');
    procedure GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
    procedure GerarIdeADC(objIdeADC: TIdeADCCollection);

    procedure GerarIdeTrabalhador;
    procedure GerarInfoComplem;
    procedure GerarSucessaoVinc;
    procedure GerarDmDev;
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
    procedure GerarInfoTrabInterm(pInfoTrabInterm: TInfoTrabIntermCollection);
    procedure GerarInfoInterm(obj: TinfoIntermCollection);
    procedure GerarInfoComplCont(pInfoComplCont: TInfoComplCont);

  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideTrabalhador: TeS1200IdeTrabalhador read FIdeTrabalhador write FIdeTrabalhador;
    property dmDev: TDMDevCollectionS1200 read FDMDev write FDMDev;
  end;

  TRemunPer1200Collection = class(TACBrObjectList)
  private
    FNomeGrupoXML: string;
    function GetItem(Index: integer): TRemunPer1200CollectionItem;
    procedure SetItem(Index: integer; Value: TRemunPer1200CollectionItem);
  public
    function Add: TRemunPer1200CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRemunPer1200CollectionItem;
    property Items[Index: integer]: TRemunPer1200CollectionItem read GetItem write SetItem;
    property grupoXML: string read FNomeGrupoXML;
  end;

  TRemunPer1200CollectionItem = class(TRemunPerCollectionItem)
  private
    FIndSimples: tpIndSimples;
    FInfoAgNocivo: TInfoAgNocivo;
    FinfoTrabInterm: TinfoTrabIntermCollection;

    function getInfoAgNocivo: TInfoAgNocivo;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function infoAgNocivoInst(): boolean;

    property indSimples: tpIndSimples read FIndSimples write FIndSimples;
    property infoAgNocivo: TInfoAgNocivo read getInfoAgNocivo write FInfoAgNocivo;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
  end;

  TIdeEstabLotCollectionS1200 = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TIdeEstabLotCollectionItemS1200;
    procedure SetItem(Index: integer; Value: TIdeEstabLotCollectionItemS1200);
  public
    function Add: TIdeEstabLotCollectionItemS1200; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabLotCollectionItemS1200;
    property Items[Index: integer]: TIdeEstabLotCollectionItemS1200 read GetItem write SetItem;
  end;

  TIdeEstabLotCollectionItemS1200 = class(TObject)
  private
    FTpInsc: TpTpInsc;
    FNrInsc: string;
    FCodLotacao: string;
    FQtdDiasAv: integer;
    FRemunPerApur: TRemunPer1200Collection;
    FRemunPerAnt: TRemunPer1200Collection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: TpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codLotacao: string read FCodLotacao write FCodLotacao;
    property qtdDiasAv: integer read FQtdDiasAv write FQtdDiasAv;
    property remunPerApur: TRemunPer1200Collection read FRemunPerApur write FRemunPerApur;
    property remunPerAnt: TRemunPer1200Collection read FRemunPerAnt write FRemunPerAnt;
  end;

  TIdePeriodoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TIdePeriodoCollectionItem;
    procedure SetItem(Index: integer; Value: TIdePeriodoCollectionItem);
  public
    function Add: TIdePeriodoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdePeriodoCollectionItem;
    property Items[Index: integer]: TIdePeriodoCollectionItem read GetItem write SetItem;
  end;

  TIdePeriodoCollectionItem = class(TObject)
  private
    FPerRef: string;
    FIdeEstabLot: TIdeEstabLotCollectionS1200;
  public
    constructor Create;
    destructor Destroy; override;
    property perRef: string read FPerRef write FPerRef;
    property ideEstabLot: TIdeEstabLotCollectionS1200 read FIdeEstabLot write FIdeEstabLot;
  end;

  TIdeADCCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TIdeADCCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeADCCollectionItem);
  public
    function Add: TIdeADCCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeADCCollectionItem;
    property Items[Index: integer]: TIdeADCCollectionItem read GetItem write SetItem;
  end;

  TIdeADCCollectionItem = class(TObject)
  private
    FDtAcConv: TDate;
    FTpAcConv: tpTpAcConv;
    FCompAcConv: String;
    FDtEfAcConv: TDate;
    FDSC: string;
    FRemunSuc: tpSimNao;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtAcConv: TDate read FDtAcConv write FDtAcConv;
    property tpAcConv: tpTpAcConv read FTpAcConv write FTpAcConv;
    property compAcConv: String read FCompAcConv write FCompAcConv;
    property dtEfAcConv: TDate read FDtEfAcConv write FDtEfAcConv;
    property dsc: string read FDSC write FDSC;
    property remunSuc: tpSimNao read FRemunSuc write FRemunSuc;
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
    FIdeEstabLot: TIdeEstabLotCollectionS1200;
  public
    constructor Create;
    destructor Destroy; override;
    property ideEstabLot: TIdeEstabLotCollectionS1200 read FIdeEstabLot write FIdeEstabLot;
  end;

  TeS1200IdeTrabalhador = class(TideTrabalhador2) // S-1200
  private
    FInfoMV: TInfoMV;
    FInfoComplem: TInfoComplem;
    FProcJudTrab: TProcJudTrabCollection;
    FinfoInterm: TinfoIntermCollection;

    function getInfoComplem: TInfoComplem;
    function getInfoMV: TInfoMV;
    function getprocJudTrab: TProcJudTrabCollection;
    function getInfoInterm: TinfoIntermCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoMVInst(): boolean;
    function infoComplemInst(): boolean;
    function procJudTrabInst(): boolean;
    function infoIntermInst(): boolean;

    property infoMV: TInfoMV read getInfoMV write FInfoMV;
    property infoComplem: TInfoComplem read getInfoComplem write FInfoComplem;
    property procJudTrab: TProcJudTrabCollection read getprocJudTrab write FProcJudTrab;
    property infoInterm: TinfoIntermCollection read getInfoInterm write FinfoInterm;
  end;

  TSucessaoVinc = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FMatricAnt: string;
    FdtAdm: TDateTime;
    FObservacao: string;
  public
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property tpInscAnt: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property cnpjEmpregAnt: string read FnrInsc write FnrInsc;
    property matricAnt: string read FMatricAnt write FMatricAnt;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property observacao: string read FObservacao write FObservacao;
  end;

  TInfoComplem = class(TObject)
  private
    FNmTrab: string;
    FDtNascto: TDate;
    FCodCBO: string;
    FNatAtividade: tpNatAtividade;
    FQtdDiasTrab: integer;
    FSucessaoVinc: TSucessaoVinc;

    function getSucessaoVinc: TSucessaoVinc;
  public
    constructor Create;
    destructor Destroy; override;
    function sucessaoVincInst: boolean;

    property nmTrab: string read FNmTrab write FNmTrab;
    property dtNascto: TDate read FDtNascto write FDtNascto;
    property codCBO: string read FCodCBO write FCodCBO;
    property natAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property qtdDiasTrab: integer read FQtdDiasTrab write FQtdDiasTrab;
    property sucessaoVinc: TSucessaoVinc read getSucessaoVinc write FSucessaoVinc;
  end;

  TInfoTrabIntermCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TInfoTrabIntermCollectionItem;
    procedure SetItem(Index: integer; Value: TInfoTrabIntermCollectionItem);
  public
    function Add: TInfoTrabIntermCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoTrabIntermCollectionItem;
    property Items[Index: integer]: TInfoTrabIntermCollectionItem read GetItem write SetItem; default;
  end;

  TInfoTrabIntermCollectionItem = class(TObject)
  private
    FcodConv: string;
  public
    property codConv: string read FcodConv write FcodConv;
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
    FqtdDiasInterm : Byte;
    Fdia : Byte;
  public
    property qtdDiasInterm : Byte read FqtdDiasInterm write FqtdDiasInterm;
    property dia : Byte read Fdia write Fdia;
  end;

  TInfoComplCont = class(TObject)
  private
    FCodCBO: string;
    FNatAtividade: tpNatAtividade;
    FQtdDiasTrab: integer;
  public
    property codCBO: string read FCodCBO write FCodCBO;
    property natAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property qtdDiasTrab: integer read FQtdDiasTrab write FQtdDiasTrab;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TRemunPer1200CollectionItem }

constructor TRemunPer1200CollectionItem.Create;
begin
  inherited Create;
  FInfoAgNocivo   := nil;
  FinfoTrabInterm := TinfoTrabIntermCollection.Create;
end;

destructor TRemunPer1200CollectionItem.Destroy;
begin
  FreeAndNil(FInfoAgNocivo);
  FinfoTrabInterm.Free;

  inherited;
end;

function TRemunPer1200CollectionItem.getInfoAgNocivo: TInfoAgNocivo;
begin
  if not(Assigned(FInfoAgNocivo)) then
    FInfoAgNocivo := TInfoAgNocivo.Create;
  Result := FInfoAgNocivo;
end;

function TRemunPer1200CollectionItem.infoAgNocivoInst: boolean;
begin
  Result := Assigned(FInfoAgNocivo);
end;

{ TRemunPer1200Collection }
function TRemunPer1200Collection.Add: TRemunPer1200CollectionItem;
begin
  Result := Self.New;
end;

function TRemunPer1200Collection.GetItem(Index: integer): TRemunPer1200CollectionItem;
begin
  Result := TRemunPer1200CollectionItem(inherited Items[Index]);
end;

procedure TRemunPer1200Collection.SetItem(Index: integer; Value: TRemunPer1200CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRemunPer1200Collection.New: TRemunPer1200CollectionItem;
begin
  Result := TRemunPer1200CollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeEstabLotCollectionItemS1200 }

constructor TIdeEstabLotCollectionItemS1200.Create;
begin
  inherited Create;
  FRemunPerApur := TRemunPer1200Collection.Create;
  FRemunPerAnt  := TRemunPer1200Collection.Create;
end;

destructor TIdeEstabLotCollectionItemS1200.Destroy;
begin
  FRemunPerApur.Free;
  FRemunPerAnt.Free;

  inherited;
end;

{ TIdeEstabLotCollectionS1200 }
function TIdeEstabLotCollectionS1200.Add: TIdeEstabLotCollectionItemS1200;
begin
  Result := Self.New;
end;

function TIdeEstabLotCollectionS1200.GetItem(Index: integer): TIdeEstabLotCollectionItemS1200;
begin
  Result := TIdeEstabLotCollectionItemS1200(inherited Items[Index]);
end;

procedure TIdeEstabLotCollectionS1200.SetItem(Index: integer; Value: TIdeEstabLotCollectionItemS1200);
begin
  inherited Items[Index] := Value;
end;

function TIdeEstabLotCollectionS1200.New: TIdeEstabLotCollectionItemS1200;
begin
  Result := TIdeEstabLotCollectionItemS1200.Create;
  Self.Add(Result);
end;

{ TIdePeriodoCollectionItem }
constructor TIdePeriodoCollectionItem.Create;
begin
  inherited Create;
  FIdeEstabLot := TIdeEstabLotCollectionS1200.Create;
end;

destructor TIdePeriodoCollectionItem.Destroy;
begin
  FIdeEstabLot.Free;

  inherited;
end;

{ TIdePeriodoCollection }
function TIdePeriodoCollection.Add: TIdePeriodoCollectionItem;
begin
  Result := Self.New;
end;

function TIdePeriodoCollection.GetItem(Index: integer): TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem(inherited Items[Index]);
end;

procedure TIdePeriodoCollection.SetItem(Index: integer; Value: TIdePeriodoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdePeriodoCollection.New: TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeADCCollectionItem }
constructor TIdeADCCollectionItem.Create;
begin
  inherited Create;
  FIdePeriodo := TIdePeriodoCollection.Create;
end;

destructor TIdeADCCollectionItem.Destroy;
begin
  FIdePeriodo.Free;
  inherited;
end;

{ TIdeADCCollection }
function TIdeADCCollection.Add: TIdeADCCollectionItem;
begin
  Result := Self.New;
end;

function TIdeADCCollection.GetItem(Index: integer): TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem(inherited Items[Index]);
end;

procedure TIdeADCCollection.SetItem(Index: integer; Value: TIdeADCCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeADCCollection.New: TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoPerAnt }
constructor TInfoPerAnt.Create;
begin
  inherited;

  FIdeADC := TIdeADCCollection.Create;
end;

destructor TInfoPerAnt.Destroy;
begin
  FIdeADC.Free;

  inherited;
end;

{ TInfoPerApur }
constructor TInfoPerApur.Create;
begin
  inherited;

  FIdeEstabLot := TIdeEstabLotCollectionS1200.Create;
end;

destructor TInfoPerApur.Destroy;
begin
  FIdeEstabLot.Free;

  inherited;
end;

{ TInfoComplem }

constructor TInfoComplem.Create;
begin
  inherited;

  FSucessaoVinc := nil;
end;

destructor TInfoComplem.Destroy;
begin
  FreeAndNil(FSucessaoVinc);
  inherited;
end;

function TInfoComplem.getSucessaoVinc: TSucessaoVinc;
begin
  if not Assigned(FSucessaoVinc) then
    FSucessaoVinc := TSucessaoVinc.Create;
  Result := FSucessaoVinc;
end;

function TInfoComplem.sucessaoVincInst: boolean;
begin
  Result := Assigned(FSucessaoVinc);
end;

{ TideTrabalhador }
constructor TeS1200IdeTrabalhador.Create;
begin
  inherited Create;
  FInfoMV      := nil;
  FInfoComplem := nil;
  FinfoInterm  := nil;
  FProcJudTrab := nil;
end;

destructor TeS1200IdeTrabalhador.Destroy;
begin
  FreeAndNil(FInfoMV);
  FreeAndNil(FInfoComplem);
  FreeAndNil(FinfoInterm);
  FreeAndNil(FProcJudTrab);

  inherited;
end;

function TeS1200IdeTrabalhador.getprocJudTrab: TProcJudTrabCollection;
begin
  if not Assigned(FProcJudTrab) then
    FProcJudTrab := TProcJudTrabCollection.Create;
  Result := FProcJudTrab;
end;

function TeS1200IdeTrabalhador.procJudTrabInst: boolean;
begin
  Result := Assigned(FProcJudTrab);
end;

function TeS1200IdeTrabalhador.getInfoComplem: TInfoComplem;
begin
  if not(Assigned(FInfoComplem)) then
    FInfoComplem := TInfoComplem.Create;
  Result := FInfoComplem;
end;

function TeS1200IdeTrabalhador.getInfoInterm: TinfoIntermCollection;
begin
  if not(Assigned(FinfoInterm)) then
    FinfoInterm := TinfoIntermCollection.Create;
  Result := FinfoInterm;
end;

function TeS1200IdeTrabalhador.getInfoMV: TInfoMV;
begin
  if not(Assigned(FInfoMV)) then
    FInfoMV := TInfoMV.Create;
  Result := FInfoMV;
end;

function TeS1200IdeTrabalhador.infoComplemInst: boolean;
begin
  Result := Assigned(FInfoComplem);
end;

function TeS1200IdeTrabalhador.infoIntermInst: boolean;
begin
  Result := Assigned(FinfoInterm);
end;

function TeS1200IdeTrabalhador.infoMVInst: boolean;
begin
  Result := Assigned(FInfoMV);
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

{ TDMDevCollection }

function TDMDevCollectionS1200.Add: TDMDevCollectionItemS1200;
begin
  Result := Self.New;
end;

function TDMDevCollectionS1200.GetItem(Index: integer): TDMDevCollectionItemS1200;
begin
  Result := TDMDevCollectionItemS1200(inherited Items[Index]);
end;

procedure TDMDevCollectionS1200.SetItem(Index: integer; Value: TDMDevCollectionItemS1200);
begin
  inherited Items[Index] := Value;
end;

function TDMDevCollectionS1200.New: TDMDevCollectionItemS1200;
begin
  Result := TDMDevCollectionItemS1200.Create;
  Self.Add(Result);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItemS1200.Create;
begin
  inherited Create;

  FinfoTrabInterm := TinfoTrabIntermCollection.Create;
  FInfoPerApur    := nil;
  FInfoPerAnt     := nil;
  FinfoComplCont  := nil;
  FinfoRRA        := nil;
end;

destructor TDMDevCollectionItemS1200.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
  FreeAndNil(FinfoComplCont);
  FinfoTrabInterm.Free;

  if infoRRAInst() then
    FreeAndNil(FinfoRRA);

  inherited;
end;

function TDMDevCollectionItemS1200.getInfoPerApur: TInfoPerApur;
begin
  if not(Assigned(FInfoPerApur)) then
    FInfoPerApur := TInfoPerApur.Create;
  Result := FInfoPerApur;
end;

function TDMDevCollectionItemS1200.infoPerApurInst: boolean;
begin
  Result := Assigned(FInfoPerApur);
end;

function TDMDevCollectionItemS1200.infoTrabIntermInst: boolean;
begin
  Result := Assigned(FinfoTrabInterm);
end;

function TDMDevCollectionItemS1200.getInfoComplCont: TInfoComplCont;
begin
  if not(Assigned(FInfoComplCont)) then
    FInfoComplCont := TInfoComplCont.Create;
  Result := FInfoComplCont;
end;

function TDMDevCollectionItemS1200.getInfoPerAnt: TInfoPerAnt;
begin
  if not(Assigned(FInfoPerAnt)) then
    FInfoPerAnt := TInfoPerAnt.Create;
  Result := FInfoPerAnt;
end;

function TDMDevCollectionItemS1200.infoComplContInst: boolean;
begin
  Result := Assigned(FinfoComplCont);
end;

function TDMDevCollectionItemS1200.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

function TDMDevCollectionItemS1200.getInfoRRA: TInfoRRA;
begin
  if not(Assigned(FInfoRRA)) then
    FInfoRRA := TInfoRRA.Create;
  Result := FInfoRRA;
end;

function TDMDevCollectionItemS1200.infoRRAInst: boolean;
begin
  Result := Assigned(FInfoRRA);
end;

{ TEvtRemun }
constructor TEvtRemun.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento      := TIdeEvento3.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTrabalhador := TeS1200IdeTrabalhador.Create;
  FDMDev          := TDMDevCollectionS1200.Create;
end;

destructor TEvtRemun.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FDMDev.Free;

  inherited;
end;

procedure TEvtRemun.GerarIdeADC(objIdeADC: TIdeADCCollection);
var
  i: integer;
begin
  for i := 0 to objIdeADC.Count - 1 do
  begin
    Gerador.wGrupo('ideADC');

    Gerador.wCampo(tcDat, '', 'dtAcConv',   10,  10, 0, objIdeADC.Items[i].dtAcConv);
    Gerador.wCampo(tcStr, '', 'tpAcConv',    1,   1, 1, eSTpAcConvToStr(objIdeADC.Items[i].tpAcConv));

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'compAcConv',  7,   7, 0, objIdeADC.Items[i].compAcConv);
      Gerador.wCampo(tcDat, '', 'dtEfAcConv', 10,  10, 0, objIdeADC.Items[i].dtEfAcConv);
    end;

    Gerador.wCampo(tcStr, '', 'dsc',         1, 255, 1, objIdeADC.Items[i].dsc);
    Gerador.wCampo(tcStr, '', 'remunSuc',    1,   1, 1, eSSimNaoToStr(objIdeADC.Items[i].remunSuc));

    GerarIdePeriodo(objIdeADC.Items[i].idePeriodo);

    Gerador.wGrupo('/ideADC');
  end;

  if objIdeADC.Count > 8 then
    Gerador.wAlerta('', 'ideADC', 'Lista de Identificação de Instrumentos', ERR_MSG_MAIOR_MAXIMO + '8');
end;

procedure TEvtRemun.GerarIdeEstabLot(objIdeEstabLot: TIdeEstabLotCollectionS1200; const nomeRemunPer: string = 'remunPerApur');
var
  i: integer;
begin
  for i := 0 to objIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');

    Gerador.wCampo(tcInt, '', 'tpInsc',     1,  1, 1, eSTpInscricaoToStr(objIdeEstabLot.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',     1, 15, 1, objIdeEstabLot.Items[i].nrInsc);
    Gerador.wCampo(tcStr, '', 'codLotacao', 1, 30, 1, objIdeEstabLot.Items[i].codLotacao);
    Gerador.wCampo(tcInt, '', 'qtdDiasAv',  1,  2, 0, objIdeEstabLot.Items[i].qtdDiasAv);

    GerarRemunPer(objIdeEstabLot.Items[i].remunPerApur, nomeRemunPer);

    Gerador.wGrupo('/ideEstabLot');
  end;

  if objIdeEstabLot.Count > 500 then
    Gerador.wAlerta('', 'ideEstabLot', 'Lista de Estabelecimentos/Locação', ERR_MSG_MAIOR_MAXIMO + '500');
end;

procedure TEvtRemun.GerarIdeEstabLot2(objIdeEstabLot: TIdeEstabLotCollectionS1200; const nomeRemunPer: string = 'remunPerAnt');
var
  i: integer;
begin
  for i := 0 to objIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');

    Gerador.wCampo(tcInt, '', 'tpInsc',     1,  1, 1, eSTpInscricaoToStr(objIdeEstabLot.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',     1, 15, 1, objIdeEstabLot.Items[i].nrInsc);
    Gerador.wCampo(tcStr, '', 'codLotacao', 1, 30, 1, objIdeEstabLot.Items[i].codLotacao);

    GerarRemunPer(objIdeEstabLot.Items[i].remunPerAnt, nomeRemunPer);

    Gerador.wGrupo('/ideEstabLot');
  end;

  if objIdeEstabLot.Count > 99 then
    Gerador.wAlerta('', 'ideEstabLot', 'Lista de Estabelecimento/Lotação', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtRemun.GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
var
  i: integer;
begin
  for i := 0 to objIdePeriodo.Count - 1 do
  begin
    Gerador.wGrupo('idePeriodo');

    Gerador.wCampo(tcStr, '', 'perRef', 7, 7, 1, objIdePeriodo.Items[i].perRef);

    GerarIdeEstabLot2(objIdePeriodo.Items[i].ideEstabLot, 'remunPerAnt');

    Gerador.wGrupo('/idePeriodo');
  end;

  if objIdePeriodo.Count > 180 then
    Gerador.wAlerta('', 'idePeriodo', 'Lista de Periodos', ERR_MSG_MAIOR_MAXIMO + '180');
end;

procedure TEvtRemun.GerarIdeTrabalhador;
begin
  Gerador.wGrupo('ideTrabalhador');

  Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, ideTrabalhador.cpfTrab);

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nisTrab',  1, 11, 0, ideTrabalhador.nisTrab);

  if (ideTrabalhador.infoMVInst()) then
    GerarInfoMV(ideTrabalhador.infoMV);

  if (ideTrabalhador.infoComplemInst()) then
    GerarInfoComplem;

  if (ideTrabalhador.procJudTrabInst()) then
    GerarProcJudTrab(ideTrabalhador.procJudTrab);

  if (VersaoDF >= ve02_04_02) and (ideTrabalhador.infoIntermInst()) then
    GerarInfoInterm(ideTrabalhador.infoInterm);

  Gerador.wGrupo('/ideTrabalhador');
end;

procedure TEvtRemun.GerarSucessaoVinc;
begin
  Gerador.wGrupo('sucessaoVinc');

  if VersaoDF >= veS01_00_00 then
  begin

    Gerador.wCampo(tcInt, '', 'tpInsc', 1, 1, 1, eSTpInscricaoToStr(ideTrabalhador.infoComplem.sucessaoVinc.tpInscAnt));
    Gerador.wCampo(tcStr, '', 'nrInsc', 14,  14, 1, ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt);

  end
  else
  begin

    if VersaoDF >= ve02_05_00 then
      Gerador.wCampo(tcInt, '', 'tpInscAnt', 1, 1, 1, eSTpInscricaoToStr(ideTrabalhador.infoComplem.sucessaoVinc.tpInscAnt));

    Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt', 14,  14, 1, ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt);

  end;

  Gerador.wCampo(tcStr, '', 'matricAnt',      1,  30, 0, ideTrabalhador.infoComplem.sucessaoVinc.matricAnt);
  Gerador.wCampo(tcDat, '', 'dtAdm',         10,  10, 1, ideTrabalhador.infoComplem.sucessaoVinc.dtAdm);
  Gerador.wCampo(tcStr, '', 'observacao',     1, 255, 0, ideTrabalhador.infoComplem.sucessaoVinc.observacao);

  Gerador.wGrupo('/sucessaoVinc');
end;

procedure TEvtRemun.GerarInfoComplem;
begin
  Gerador.wGrupo('infoComplem');

  Gerador.wCampo(tcStr, '', 'nmTrab',        1, 70, 1, ideTrabalhador.infoComplem.nmTrab);
  Gerador.wCampo(tcDat, '', 'dtNascto',     10, 10, 1, ideTrabalhador.infoComplem.dtNascto);

  if VersaoDF < ve02_04_02 then
  begin
    Gerador.wCampo(tcStr, '', 'codCBO',        1,  6, 1, ideTrabalhador.infoComplem.codCBO);

    if ideTrabalhador.infoComplem.natAtividade <> navNaoInformar then
      Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 0, eSNatAtividadeToStr(ideTrabalhador.infoComplem.natAtividade));

    Gerador.wCampo(tcInt, '', 'qtdDiasTrab',   1,  2, 0, ideTrabalhador.infoComplem.qtdDiasTrab);
  end;

  if ideTrabalhador.infoComplem.sucessaoVincInst() then
    GerarSucessaoVinc;

  Gerador.wGrupo('/infoComplem');
end;

procedure TEvtRemun.GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
begin
  Gerador.wGrupo('infoPerAnt');

  GerarIdeADC(pInfoPerAnt.ideADC);

  Gerador.wGrupo('/infoPerAnt');
end;

procedure TEvtRemun.GerarDmDev;
var
  i: integer;
begin
  for i := 0 to dmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');
    Gerador.wCampo(tcStr, '', 'ideDmDev', 1, 30, 1, dmDev[i].ideDmDev);
    Gerador.wCampo(tcInt, '', 'codCateg', 1,  3, 1, dmDev[i].codCateg);

    if VersaoDF >= veS01_01_00 then
    begin
      if (dmDev[i].indRRA = snfSim) and (dmDev[i].infoRRAInst()) then
      begin
        Gerador.wCampo(tcStr, '', 'indRRA', 1,  1, 1, eSSimNaoFacultativoToStr(dmDev[i].indRRA));

        if (dmDev[i].infoRRAInst()) then
          GerarInfoRRA(dmDev[i].infoRRA);
      end;
    end;

    if (dmDev[i].infoPerApurInst()) then
      GerarInfoPerApur(dmDev[i].infoPerApur);

    if (dmDev[i].infoPerAntInst()) and
       (dmDev[i].infoPerAnt.ideADC.Count > 0) then
      GerarInfoPerAnt(dmDev[i].infoPerAnt);

    if (VersaoDF < ve02_04_02) and (dmDev[i].infoTrabIntermInst()) then
      GerarInfoTrabInterm(dmDev[i].infoTrabInterm);

    if (VersaoDF >= ve02_04_02) and (dmDev[i].infoComplContInst()) then
      GerarInfoComplCont(dmDev[i].infoComplCont);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 999 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demostrativo de Valores', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtRemun.GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
begin
  Gerador.wGrupo('infoPerApur');

  GerarIdeEstabLot(pInfoPerApur.ideEstabLot);

  Gerador.wGrupo('/infoPerApur');
end;

procedure TEvtRemun.GerarInfoTrabInterm(
  pInfoTrabInterm: TInfoTrabIntermCollection);
var
  i: integer;
begin
  for i := 0 to pInfoTrabInterm.Count - 1 do
  begin
    Gerador.wGrupo('infoTrabInterm');

    Gerador.wCampo(tcStr, '', 'codConv', 1, 30, 1, pInfoTrabInterm.Items[i].codConv);

    Gerador.wGrupo('/infoTrabInterm');
  end;

  if pInfoTrabInterm.Count > 99 then
    Gerador.wAlerta('', 'infoTrabInterm', 'Lista de Informações de Convocação', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtRemun.GerarRemunPer(objRemunPer: TRemunPer1200Collection; const nomeRemunPer: string = 'remunPerApur');
var
  i: Integer;
begin
  for i := 0 to objRemunPer.Count - 1 do
  begin
    Gerador.wGrupo(nomeRemunPer);

    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, objRemunPer.Items[i].matricula);

    if objRemunPer.Items[i].indSimples <> idsNenhum then
      Gerador.wCampo(tcStr, '', 'indSimples', 1, 1, 0, eSIndSimplesToStr(objRemunPer.Items[i].indSimples));

    GerarItensRemun(objRemunPer.Items[i].itensRemun, 'itensRemun');

    if (nomeRemunPer = 'remunPerApur') and (VersaoDF <= ve02_05_00) then
    begin
      if objRemunPer.Items[i].infoSaudeColetInst() then
        GerarInfoSaudeColet(objRemunPer.Items[i].infoSaudeColet);
    end;

    if (objRemunPer.Items[i].infoAgNocivoInst()) then
      GerarInfoAgNocivo(objRemunPer.Items[i].infoAgNocivo);

    if VersaoDF <= ve02_05_00 then
      GerarInfoTrabInterm(objRemunPer.Items[i].infoTrabInterm);

    Gerador.wGrupo('/' + nomeRemunPer);
  end;

  if objRemunPer.Count > 8 then
    Gerador.wAlerta('', nomeRemunPer, 'Lista de ' + nomeRemunPer, ERR_MSG_MAIOR_MAXIMO + '8');
end;

function TEvtRemun.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtRemun');
    Gerador.wGrupo('evtRemun Id="' + Self.Id + '"');

    GerarIdeEvento3(self.ideEvento);
    GerarIdeEmpregador(self.ideEmpregador);
    GerarIdeTrabalhador;
    GerarDmDev;

    Gerador.wGrupo('/evtRemun');
    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtRemun');

//    Validar(schevtRemun);
  except on e: Exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

procedure TEvtRemun.GerarInfoInterm(obj: TinfoIntermCollection);
var
  i: integer;
begin
  if obj.Count > 0 then
  begin
    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wGrupo('infoInterm');

      Gerador.wCampo(tcInt, '', 'qtdDiasInterm', 1, 2, 1, obj[0].qtdDiasInterm);

      Gerador.wGrupo('/infoInterm');
    end
    else
    begin
      for i := 0 to obj.Count - 1 do
      begin
        Gerador.wGrupo('infoInterm');

        Gerador.wCampo(tcInt, '', 'dia', 1, 2, 1, obj[i].dia);

        Gerador.wGrupo('/infoInterm');
      end;
    end;

    if obj.Count > 31 then
      Gerador.wAlerta('', 'infoInterm', 'Informações relativas ao trabalho intermitente', ERR_MSG_MAIOR_MAXIMO + '31');
  end;
end;

procedure TEvtRemun.GerarInfoComplCont(pInfoComplCont: TInfoComplCont);
begin
  Gerador.wGrupo('infoComplCont');

  Gerador.wCampo(tcStr, '', 'codCBO', 1, 6, 1, pInfoComplCont.codCBO);

  if pInfoComplCont.natAtividade <> navNaoInformar then
    Gerador.wCampo(tcStr, '', 'natAtividade', 1, 1, 0, eSNatAtividadeToStr(pInfoComplCont.natAtividade));

  Gerador.wCampo(tcInt, '', 'qtdDiasTrab',  1, 2, 0, pInfoComplCont.qtdDiasTrab);

  Gerador.wGrupo('/infoComplCont');
end;

{ TS1200CollectionItem }

constructor TS1200CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1200;
  FEvtRemun   := TEvtRemun.Create(AOwner);
end;

destructor TS1200CollectionItem.Destroy;
begin
  FEvtRemun.Free;

  inherited;
end;

{ TS1200Collection }

function TS1200Collection.Add: TS1200CollectionItem;
begin
  Result := Self.New;
end;

function TS1200Collection.GetItem(Index: integer): TS1200CollectionItem;
begin
  Result := TS1200CollectionItem(inherited Items[Index]);
end;

procedure TS1200Collection.SetItem(Index: integer; Value: TS1200CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1200Collection.New: TS1200CollectionItem;
begin
  Result := TS1200CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TInfoTrabIntermCollection }

function TInfoTrabIntermCollection.Add: TInfoTrabIntermCollectionItem;
begin
  Result := Self.New;
end;

function TInfoTrabIntermCollection.GetItem(
  Index: integer): TInfoTrabIntermCollectionItem;
begin
  Result := TInfoTrabIntermCollectionItem(inherited Items[Index]);
end;

procedure TInfoTrabIntermCollection.SetItem(Index: integer;
  Value: TInfoTrabIntermCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoTrabIntermCollection.New: TInfoTrabIntermCollectionItem;
begin
  Result := TInfoTrabIntermCollectionItem.Create;
  Self.Add(Result);
end;

function TEvtRemun.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok, Existe: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRemun';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideTrabalhador';
      ideTrabalhador.cpfTrab := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrabalhador.nisTrab := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);

      sSecao := 'infoMV';
      if INIRec.ReadString(sSecao, 'indMV', '') <> ''then
      begin
        ideTrabalhador.infoMV.indMV := eSStrToIndMV(Ok, INIRec.ReadString(sSecao, 'indMV', '1'));

        I := 1;
        while true do
        begin
          // de 01 até 10
          sSecao := 'remunOutrEmpr' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideTrabalhador.infoMV.remunOutrEmpr.New do
          begin
            tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
            nrInsc     := sFim;
            codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
            vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
          end;

          Inc(I);
        end;
      end;

      sSecao := 'infoComplem';
      if INIRec.ReadString(sSecao, 'nmTrab', '') <> '' then
      begin
        ideTrabalhador.infoComplem.nmTrab       := INIRec.ReadString(sSecao, 'nmTrab', '');
        ideTrabalhador.infoComplem.dtNascto     := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      end;

      sSecao := 'sucessaoVinc';
      if INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '') <> '' then
      begin
        ideTrabalhador.infoComplem.sucessaoVinc.tpInscAnt     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscAnt', '1'));
        ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt := INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '');
        ideTrabalhador.infoComplem.sucessaoVinc.matricAnt     := INIRec.ReadString(sSecao, 'matricAnt', '');
        ideTrabalhador.infoComplem.sucessaoVinc.dtAdm         := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
        ideTrabalhador.infoComplem.sucessaoVinc.observacao    := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      if INIRec.ReadString(sSecao, 'nrInsc', '') <> '' then
      begin
        ideTrabalhador.infoComplem.sucessaoVinc.tpInsc        := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        ideTrabalhador.infoComplem.sucessaoVinc.nrInsc        := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
        ideTrabalhador.infoComplem.sucessaoVinc.matricAnt     := INIRec.ReadString(sSecao, 'matricAnt', '');
        ideTrabalhador.infoComplem.sucessaoVinc.dtAdm         := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
        ideTrabalhador.infoComplem.sucessaoVinc.observacao    := INIRec.ReadString(sSecao, 'observacao', '');
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrabalhador.procJudTrab.New do
        begin
          tpTrib     := eSStrToTpTributo(Ok, INIRec.ReadString(sSecao, 'tpTrib', '1'));
          nrProcJud  := sFim;
          codSusp    := INIRec.ReadString(sSecao, 'codSusp', '');
        end;

        Inc(I);
      end;

      sSecao := 'infoInterm';
      if INIRec.SectionExists(sSecao) then
      begin
        if INIRec.ReadString(sSecao, 'qtdDiasInterm', '') <> '' then
          with ideTrabalhador.infoInterm.New do
            qtdDiasInterm := INIRec.ReadInteger(sSecao, 'qtdDiasInterm', 0);

      end;

      // Bloco referente a versão Simplificada
      I := 1;
      while true do
      begin
        // de 01 até 31
        sSecao := 'infoInterm' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'dia', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrabalhador.infoInterm.New do
        begin
          dia := INIRec.ReadInteger(sSecao, 'dia', 0);
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 001 até 999
        sSecao := 'dmDev' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with dmDev.New do
        begin
          ideDmDev := sFim;
          codCateg := INIRec.ReadInteger(sSecao, 'codCateg', 0);
          indRRA   := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indRRA', EmptyStr));

          sSecao := 'infoRRA' + IntToStrZero(I, 3);
          with infoRRA do
          begin
            tpProcRRA := eSStrToTpProcRRA(Ok, sFim);
            nrProcRRA := INIRec.ReadString(sSecao, 'nrProcRRA', EmptyStr);
            descRRA := INIRec.ReadString(sSecao, 'descRRA', EmptyStr);
            qtdMesesRRA := INIRec.ReadInteger(sSecao, 'qtdMesesRRA', 0);
            despProcJud.vlrDespCustas := INIRec.ReadFloat(sSecao, 'vlrDespCustas',0);
            despProcJud.vlrDespAdvogados := INIRec.ReadFloat(sSecao, 'vlrDespAdvogados',0);

            J := 1;
            while True do
            begin
              // de 01 até 99
              sSecao := 'ideAdv' + IntToStrZero(I, 3) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with ideAdv.New do
              begin
                tpInsc := eSStrToTpInscricao(Ok, sFim);
                nrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
                vlrAdv := INIRec.ReadFloat(sSecao, 'vlrAdv',0);
              end;

              Inc(J);
            end;
          end;

          J := 1;
          while true do
          begin
            // de 001 até 500
            sSecao := 'ideEstabLot' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerApur.ideEstabLot.New do
            begin
              tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
              nrInsc     := sFim;
              codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');
              qtdDiasAv  := INIRec.ReadInteger(sSecao, 'qtdDiasAv', 0);

              K := 1;
              while true do
              begin
                sSecao := 'itensRemun' + IntToStrZero(I, 3) +
                            IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                            IntToStrZero(1, 3);

                Existe := INIRec.SectionExists(sSecao);

                sSecao := 'detOper' + IntToStrZero(I, 3) +
                            IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                            IntToStrZero(1, 2);

                Existe := Existe or INIRec.SectionExists(sSecao);

                sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) +
                            IntToStrZero(J, 3) + IntToStrZero(K, 1);

                Existe := Existe or INIRec.SectionExists(sSecao);

                if not Existe then
                  break;

                // de 1 até 8
                sSecao := 'remunPerApur' + IntToStrZero(I, 3) + IntToStrZero(J, 3) +
                             IntToStrZero(K, 1);

                with remunPerApur.New do
                begin
                  matricula  := INIRec.ReadString(sSecao, 'matricula', 'FIM');
                  indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

                  L := 1;
                  while true do
                  begin
                    // de 001 até 200
                    sSecao := 'itensRemun' + IntToStrZero(I, 3) +
                                IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with itensRemun.New do
                    begin
                      codRubr    := sFim;
                      ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                      qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                      fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                      vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                      vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                      indApurIR  := eSStrToTpindApurIR(ok, INIRec.ReadString(sSecao, 'indApurIR', EmptyStr));
                    end;

                    Inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detOper' + IntToStrZero(I, 3) +
                                IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with infoSaudeColet.detOper.New do
                    begin
                      cnpjOper := sFim;
                      regANS   := INIRec.ReadString(sSecao, 'regANS', '');
                      vrPgTit  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrPgTit', ''), 0);

                      M := 1;
                      while true do
                      begin
                        // de 01 até 99
                        sSecao := 'detPlano' + IntToStrZero(I, 3) +
                                    IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                    IntToStrZero(L, 2) + IntToStrZero(M, 2);
                        sFim   := INIRec.ReadString(sSecao, 'nmDep', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with detPlano.New do
                         begin
                          tpDep    := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '00'));
                          cpfDep   := INIRec.ReadString(sSecao, 'cpfDep', '');
                          nmDep    := sFim;
                          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                          vlrPgDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgDep', ''), 0);
                        end;

                        Inc(M);
                      end;

                    end;

                    Inc(L);
                  end;

                  sSecao := 'infoAgNocivo' + IntToStrZero(I, 3) +
                                    IntToStrZero(J, 3) + IntToStrZero(K, 1);
                  if INIRec.ReadString(sSecao, 'grauExp', '') <> '' then
                    infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));
                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 1 até 8
            sSecao := 'ideADC' + IntToStrZero(I, 3) + IntToStrZero(J, 1);
            sFim   := INIRec.ReadString(sSecao, 'dtAcConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerAnt.ideADC.New do
            begin
              dtAcConv   := StringToDateTime(sFim);
              tpAcConv   := eSStrToTpAcConv(Ok, INIRec.ReadString(sSecao, 'tpAcConv', 'A'));
              compAcConv := INIRec.ReadString(sSecao, 'compAcConv', '');
              dtEfAcConv := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfAcConv', '0'));
              dsc        := INIRec.ReadString(sSecao, 'dsc', '');
              remunSuc   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'remunSuc', 'S'));

              K := 1;
              while true do
              begin
                // de 001 até 180
                sSecao := 'idePeriodo' + IntToStrZero(I, 3) + IntToStrZero(J, 1) +
                   IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with idePeriodo.New do
                begin
                  perRef := sFim;

                  L := 1;
                  while true do
                  begin
                    // de 001 até 500
                    sSecao := 'ideEstabLot' + IntToStrZero(I, 3) + IntToStrZero(J, 1) +
                       IntToStrZero(K, 3) + IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with ideEstabLot.New do
                    begin
                      tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
                      nrInsc     := sFim;
                      codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');

                      M := 1;
                      while true do
                      begin
                        // de 1 até 8
                        sSecao := 'remunPerAnt' + IntToStrZero(I, 3) + IntToStrZero(J, 1) +
                           IntToStrZero(K, 3) + IntToStrZero(L, 3) + IntToStrZero(M, 1);
                        sFim   := INIRec.ReadString(sSecao, 'matricula', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with remunPerAnt.New do
                        begin
                          matricula  := sFim;
                          indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

                          N := 1;
                          while true do
                          begin
                            // de 001 até 200
                            sSecao := 'itensRemun' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 1) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 3) + IntToStrZero(M, 1) +
                                        IntToStrZero(N, 3);
                            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                            if (sFim = 'FIM') or (Length(sFim) <= 0) then
                              break;

                            with itensRemun.New do
                            begin
                              codRubr    := sFim;
                              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                              qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                              fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                              vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                              indApurIR  := eSStrToTpindApurIR(ok, INIRec.ReadString(sSecao, 'indApurIR', '0'));  //09/02/2023
                            end;

                            Inc(N);
                          end;

                          sSecao := 'infoAgNocivo' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 1) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 3) + IntToStrZero(M, 1);
                          if INIRec.ReadString(sSecao, 'grauExp', '') <> '' then
                            infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));
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

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'infoTrabInterm' + IntToStrZero(I, 3) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoTrabInterm.New do
            begin
              codConv := sFim;
            end;

            Inc(J);
          end;

          // de 0 até 1
          sSecao := 'infoComplCont' + IntToStrZero(I, 3);
          if INIRec.SectionExists(sSecao) then
          begin
            sFim := INIRec.ReadString(sSecao, 'codCBO', '');
            if sFim <> '' then
            begin
              infoComplCont.codCBO := sFim;
              infoComplCont.natAtividade := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '0'));
              infoComplCont.qtdDiasTrab := INIRec.ReadInteger(sSecao, 'qtdDiasTrab', 0);
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

end.
