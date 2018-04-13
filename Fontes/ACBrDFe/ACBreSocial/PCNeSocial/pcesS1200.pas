{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit pcesS1200;

interface

uses
  SysUtils, Classes, Dialogs, Controls,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TRemunPer1200Collection = class;
  TRemunPer1200CollectionItem = class;
  TIdeEstabLotCollection = class;
  TIdeEstabLotCollectionItem = class;
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
  TDMDevCollection = class;
  TDMDevCollectionItem = class;
  TSucessaoVinc = class;
  TInfoTrabIntermCollectionItem = class;
  TInfoTrabIntermCollection = class;
  TinfoInterm = class;
  TInfoComplCont = class;

  TS1200Collection = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TS1200CollectionItem;
    procedure SetItem(Index: integer; Value: TS1200CollectionItem);
  public
    function Add: TS1200CollectionItem;
    property Items[Index: integer]: TS1200CollectionItem read GetItem write SetItem; default;
  end;

  TS1200CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtRemun: TEvtRemun;
    procedure setEvtRemun(const Value: TEvtRemun);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRemun: TEvtRemun read FEvtRemun write setEvtRemun;
  end;

  TDMDevCollection = class(TCollection)
  private
    function GetItem(Index: integer): TDMDevCollectionItem;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItem);
  public
    constructor Create(); reintroduce;

    function Add: TDMDevCollectionItem;
    property Items[Index: integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDMDevCollectionItem = class(TCollectionItem)
  private
    FIdeDmDev: string;
    FCodCateg: integer;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;
    FinfoTrabInterm: TinfoTrabIntermCollection;
    FinfoComplCont: TInfoComplCont;

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function infoPerApurInst(): boolean;
    function infoPerAntInst(): boolean;
    function infoComplContInst(): boolean;
    function infoTrabIntermInst(): boolean;
  published
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property codCateg: integer read FCodCateg write FCodCateg;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
    property infoComplCont: TInfoComplCont read FinfoComplCont write FinfoComplCont;
  end;

  TEvtRemun = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TeS1200IdeTrabalhador;
    FDMDev: TDMDevCollection;
    FACBreSocial: TObject;

    { Geradores específicos desta classe }
    procedure GerarIdeEstabLot(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerApur');
    procedure GerarIdeEstabLot2(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerAnt');
    procedure GerarRemunPer(objRemunPer: TRemunPer1200Collection; nomeRemunPer: string = 'remunPerApur');
    procedure GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
    procedure GerarIdeADC(objIdeADC: TIdeADCCollection);

    procedure GerarIdeTrabalhador;
    procedure GerarInfoComplem;
    procedure GerarSucessaoVinc;
    procedure GerarDmDev;
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
    procedure GerarInfoTrabInterm(pInfoTrabInterm: TInfoTrabIntermCollection);
    procedure GerarInfoInterm;
    procedure GerarInfoComplCont(pInfoComplCont: TInfoComplCont);
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideTrabalhador: TeS1200IdeTrabalhador read FIdeTrabalhador write FIdeTrabalhador;
    property dmDev: TDMDevCollection read FDMDev write FDMDev;
  end;

  TRemunPer1200Collection = class(TCollection)
  private
    FNomeGrupoXML: string;
    function GetItem(Index: integer): TRemunPer1200CollectionItem;
    procedure SetItem(Index: integer; Value: TRemunPer1200CollectionItem);
  public
    constructor Create(); reintroduce;
    function Add: TRemunPer1200CollectionItem;
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

  TIdeEstabLotCollection = class(TCollection)
  private
    function GetItem(Index: integer): TIdeEstabLotCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeEstabLotCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TIdeEstabLotCollectionItem;
    property Items[Index: integer]: TIdeEstabLotCollectionItem read GetItem write SetItem;
  end;

  TIdeEstabLotCollectionItem = class(TCollectionItem)
  private
    FTpInsc: TpTpInsc;
    FNrInsc: string;
    FCodLotacao: string;
    FQtdDiasAv: integer;
    FRemunPerApur: TRemunPer1200Collection;
    FRemunPerAnt: TRemunPer1200Collection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property tpInsc: TpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codLotacao: string read FCodLotacao write FCodLotacao;
    property qtdDiasAv: integer read FQtdDiasAv write FQtdDiasAv;
    property remunPerApur: TRemunPer1200Collection read FRemunPerApur write FRemunPerApur;
    property remunPerAnt: TRemunPer1200Collection read FRemunPerAnt write FRemunPerAnt;
  end;

  TIdePeriodoCollection = class(TCollection)
  private
    function GetItem(Index: integer): TIdePeriodoCollectionItem;
    procedure SetItem(Index: integer; Value: TIdePeriodoCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TIdePeriodoCollectionItem;
    property Items[Index: integer]: TIdePeriodoCollectionItem read GetItem write SetItem;
  end;

  TIdePeriodoCollectionItem = class(TCollectionItem)
  private
    FPerRef: string;
    FIdeEstabLot: TIdeEstabLotCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property perRef: string read FPerRef write FPerRef;
    property ideEstabLot: TIdeEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TIdeADCCollection = class(TCollection)
  private
    function GetItem(Index: integer): TIdeADCCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeADCCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TIdeADCCollectionItem;
    property Items[Index: integer]: TIdeADCCollectionItem read GetItem write SetItem;
  end;

  TIdeADCCollectionItem = class(TCollectionItem)
  private
    FDtAcConv: TDate;
    FTpAcConv: tpTpAcConv;
    FCompAcConv: String;
    FDtEfAcConv: TDate;
    FDSC: string;
    FRemunSuc: tpSimNao;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create; reintroduce;

    property dtAcConv: TDate read FDtAcConv write FDtAcConv;
    property tpAcConv: tpTpAcConv read FTpAcConv write FTpAcConv;
    property compAcConv: String read FCompAcConv write FCompAcConv;
    property dtEfAcConv: TDate read FDtEfAcConv write FDtEfAcConv;
    property dsc: string read FDSC write FDSC;
    property remunSuc: tpSimNao read FRemunSuc write FRemunSuc;
    property idePeriodo: TIdePeriodoCollection read FIdePeriodo write FIdePeriodo;
  end;

  TInfoPerAnt = class(TPersistent)
  private
    FIdeADC: TIdeADCCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property ideADC: TIdeADCCollection read FIdeADC write FIdeADC;
  end;

  TInfoPerApur = class(TPersistent)
  private
    FIdeEstabLot: TIdeEstabLotCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property ideEstabLot: TIdeEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TeS1200IdeTrabalhador = class(TideTrabalhador2) // S-1200
  private
    FInfoMV: TInfoMV;
    FInfoComplem: TInfoComplem;
    FProcJudTrab: TProcJudTrabCollection;
    FinfoInterm: TinfoInterm;

    function getInfoComplem: TInfoComplem;
    function getInfoMV: TInfoMV;
    function getInfoProcJudTrab: TProcJudTrabCollection;
    function getInfoInterm: TinfoInterm;
  public
    constructor Create;
    destructor Destroy; override;
    
    function infoMVInst(): boolean;
    function infoComplemInst(): boolean;
    function procJudTrabInst(): boolean;
    function infoIntermInst(): boolean;

    property infoMV: TInfoMV read getInfoMV write FInfoMV;
    property infoComplem: TInfoComplem read getInfoComplem write FInfoComplem;
    property procJudTrab: TProcJudTrabCollection read getInfoProcJudTrab write FProcJudTrab;
    property infoInterm: TinfoInterm read getInfoInterm write FinfoInterm;
  end;

  TSucessaoVinc = class(TPersistent)
  private
    FCnpjEmpregAnt: string;
    FMatricAnt: string;
    FdtAdm: TDateTime;
    FObservacao: string;
  public
    property cnpjEmpregAnt: string read FCnpjEmpregAnt write FCnpjEmpregAnt;
    property matricAnt: string read FMatricAnt write FMatricAnt;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property observacao: string read FObservacao write FObservacao;
  end;

  TInfoComplem = class(TPersistent)
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

  TInfoTrabIntermCollection = class(TCollection)
  private
    function GetItem(Index: integer): TInfoTrabIntermCollectionItem;
    procedure SetItem(Index: integer; Value: TInfoTrabIntermCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TInfoTrabIntermCollectionItem;
    property Items[Index: integer]: TInfoTrabIntermCollectionItem read GetItem write SetItem; default;
  end;

  TInfoTrabIntermCollectionItem = class(TCollectionItem)
  private
    FcodConv: string;
  published
    property codConv: string read FcodConv write FcodConv;
  end;

  TinfoInterm = class(TPersistent)
  private
    FqtdDiasInterm: Integer;
  public
    property qtdDiasInterm: Integer read FqtdDiasInterm write FqtdDiasInterm;
  end;

  TInfoComplCont = class(TPersistent)
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
  ACBreSocial, ACBrDFeUtil;

{ TRemunPer1200CollectionItem }

constructor TRemunPer1200CollectionItem.Create;
begin
  FInfoAgNocivo := nil;
  FinfoTrabInterm := TinfoTrabIntermCollection.Create;

  inherited
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
  Result := TRemunPer1200CollectionItem(inherited Add);
  Result.Create;
end;

constructor TRemunPer1200Collection.Create();
begin
  inherited Create(TRemunPer1200CollectionItem);
end;

function TRemunPer1200Collection.GetItem(Index: integer): TRemunPer1200CollectionItem;
begin
  Result := TRemunPer1200CollectionItem(inherited GetItem(Index));
end;

procedure TRemunPer1200Collection.SetItem(Index: integer; Value: TRemunPer1200CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdeEstabLotCollectionItem }

constructor TIdeEstabLotCollectionItem.Create;
begin
  FRemunPerApur := TRemunPer1200Collection.Create;
  FRemunPerAnt := TRemunPer1200Collection.Create;
end;

destructor TIdeEstabLotCollectionItem.Destroy;
begin
  FRemunPerApur.Free;
  FRemunPerAnt.Free;

  inherited;
end;

{ TIdeEstabLotCollection }
function TIdeEstabLotCollection.Add: TIdeEstabLotCollectionItem;
begin
  Result := TIdeEstabLotCollectionItem(inherited Add);
  Result.Create;
end;

constructor TIdeEstabLotCollection.Create;
begin
  inherited Create(TIdeEstabLotCollectionItem);
end;

function TIdeEstabLotCollection.GetItem(Index: integer): TIdeEstabLotCollectionItem;
begin
  Result := TIdeEstabLotCollectionItem(inherited GetItem(Index));
end;

procedure TIdeEstabLotCollection.SetItem(Index: integer; Value: TIdeEstabLotCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdePeriodoCollectionItem }
constructor TIdePeriodoCollectionItem.Create;
begin
  FIdeEstabLot := TIdeEstabLotCollection.Create;
end;

destructor TIdePeriodoCollectionItem.Destroy;
begin
  FIdeEstabLot.Free;

  inherited;
end;

{ TIdePeriodoCollection }
function TIdePeriodoCollection.Add: TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem(inherited Add);
  Result.Create;
end;

constructor TIdePeriodoCollection.Create;
begin
  inherited Create(TIdePeriodoCollectionItem);
end;

function TIdePeriodoCollection.GetItem(Index: integer): TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem(inherited GetItem(Index));
end;

procedure TIdePeriodoCollection.SetItem(Index: integer; Value: TIdePeriodoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdeADCCollectionItem }
constructor TIdeADCCollectionItem.Create;
begin
  FIdePeriodo := TIdePeriodoCollection.Create;
end;

{ TIdeADCCollection }
function TIdeADCCollection.Add: TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem(inherited Add);
  Result.Create;
end;

constructor TIdeADCCollection.Create;
begin
  inherited Create(TIdeADCCollectionItem);
end;

function TIdeADCCollection.GetItem(Index: integer): TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem(inherited GetItem(Index));
end;

procedure TIdeADCCollection.SetItem(Index: integer; Value: TIdeADCCollectionItem);
begin
  inherited SetItem(Index, Value);
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

  FIdeEstabLot := TIdeEstabLotCollection.Create;
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
  FInfoMV := nil;
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

function TeS1200IdeTrabalhador.getInfoProcJudTrab: TProcJudTrabCollection;
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

function TeS1200IdeTrabalhador.getInfoInterm: TinfoInterm;
begin
  if not(Assigned(FinfoInterm)) then
    FinfoInterm := TinfoInterm.Create;
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

{ TDMDevCollection }

constructor TDMDevCollection.Create;
begin
  inherited Create(TDMDevCollectionItem);
end;

function TDMDevCollection.Add: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Add());
  Result.Create;
end;

function TDMDevCollection.GetItem(Index: integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited GetItem(Index));
end;

procedure TDMDevCollection.SetItem(Index: integer; Value: TDMDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  FinfoTrabInterm := TinfoTrabIntermCollection.Create;

  FInfoPerApur := nil;
  FInfoPerAnt := nil;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
  FinfoTrabInterm.Free;

  inherited;
end;

function TDMDevCollectionItem.getInfoPerApur: TInfoPerApur;
begin
  if not(Assigned(FInfoPerApur)) then
    FInfoPerApur := TInfoPerApur.Create;
  Result := FInfoPerApur;
end;

function TDMDevCollectionItem.infoPerApurInst: boolean;
begin
  Result := Assigned(FInfoPerApur);
end;

function TDMDevCollectionItem.infoTrabIntermInst: boolean;
begin
  Result := Assigned(FinfoTrabInterm);
end;

function TDMDevCollectionItem.getInfoPerAnt: TInfoPerAnt;
begin
  if not(Assigned(FInfoPerAnt)) then
    FInfoPerAnt := TInfoPerAnt.Create;
  Result := FInfoPerAnt;
end;

function TDMDevCollectionItem.infoComplContInst: boolean;
begin
  Result := Assigned(FinfoComplCont);
end;

function TDMDevCollectionItem.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

{ TEvtRemun }
constructor TEvtRemun.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TeS1200IdeTrabalhador.Create;
  FDMDev := TDMDevCollection.Create;
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
    Gerador.wCampo(tcStr, '', 'compAcConv',  7,   7, 0, objIdeADC.Items[i].compAcConv);
    Gerador.wCampo(tcDat, '', 'dtEfAcConv', 10,  10, 0, objIdeADC.Items[i].dtEfAcConv);
    Gerador.wCampo(tcStr, '', 'dsc',         1, 255, 1, objIdeADC.Items[i].dsc);
    Gerador.wCampo(tcStr, '', 'remunSuc',    1,   1, 1, eSSimNaoToStr(objIdeADC.Items[i].remunSuc));

    GerarIdePeriodo(objIdeADC.Items[i].idePeriodo);

    Gerador.wGrupo('/ideADC');
  end;

  if objIdeADC.Count > 8 then
    Gerador.wAlerta('', 'ideADC', 'Lista de Identificação de Instrumentos', ERR_MSG_MAIOR_MAXIMO + '8');
end;

procedure TEvtRemun.GerarIdeEstabLot(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerApur');
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

procedure TEvtRemun.GerarIdeEstabLot2(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerAnt');
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
  Gerador.wCampo(tcStr, '', 'nisTrab',  1, 11, 0, ideTrabalhador.nisTrab);

  if (ideTrabalhador.infoMVInst()) then
    GerarInfoMV(ideTrabalhador.infoMV);

  if (ideTrabalhador.infoComplemInst()) then
    GerarInfoComplem;

  if (ideTrabalhador.procJudTrabInst()) then
    GerarProcJudTrab(ideTrabalhador.procJudTrab);

  if (VersaoDF >= ve02_04_02) and (ideTrabalhador.infoIntermInst()) then
    GerarInfoInterm;

  Gerador.wGrupo('/ideTrabalhador');
end;

procedure TEvtRemun.GerarSucessaoVinc;
begin
  Gerador.wGrupo('sucessaoVinc');

  Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt', 14,  14, 1, ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt);
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

    if (dmDev[i].infoPerApurInst()) then
      GerarInfoPerApur(dmDev[i].infoPerApur);

    if (dmDev[i].infoPerAntInst()) then
      GerarInfoPerAnt(dmDev[i].infoPerAnt);

    if (VersaoDF < ve02_04_02) and (dmDev[i].infoTrabIntermInst()) then
      GerarInfoTrabInterm(dmDev[i].infoTrabInterm);

    if (VersaoDF >= ve02_04_02) and (dmDev[i].infoComplContInst()) then
      GerarInfoComplCont(dmDev[i].infoComplCont);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 99 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demostrativo de Valores', ERR_MSG_MAIOR_MAXIMO + '99');
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

procedure TEvtRemun.GerarRemunPer(objRemunPer: TRemunPer1200Collection; nomeRemunPer: string = 'remunPerApur');
var
  i: integer;
begin
  for i := 0 to objRemunPer.Count - 1 do
  begin
    Gerador.wGrupo(nomeRemunPer);

    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, objRemunPer.Items[i].matricula);

    if ord(objRemunPer.Items[i].indSimples) > 0 then
      Gerador.wCampo(tcStr, '', 'indSimples', 1, 1, 0, eSIndSimplesToStr(objRemunPer.Items[i].indSimples));

    GerarItensRemun(objRemunPer.Items[i].itensRemun, 'itensRemun');

    if (nomeRemunPer = 'remunPerApur') then
    begin
      if objRemunPer.Items[i].infoSaudeColetInst() then
        GerarInfoSaudeColet(objRemunPer.Items[i].infoSaudeColet);
    end;

    if (objRemunPer.Items[i].infoAgNocivoInst()) then
      GerarInfoAgNocivo(objRemunPer.Items[i].infoAgNocivo);

    if VersaoDF >= ve02_04_02 then
      GerarInfoTrabInterm(objRemunPer.Items[i].infoTrabInterm);

    Gerador.wGrupo('/' + nomeRemunPer);
  end;

  if objRemunPer.Count > 8 then
    Gerador.wAlerta('', nomeRemunPer, 'Lista de ' + nomeRemunPer, ERR_MSG_MAIOR_MAXIMO + '8');
end;

function TEvtRemun.GerarXML: boolean;
begin
  try
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

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtRemun');

    Validar(schevtRemun);
  except
    on e: Exception do
      raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtRemun.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRemun';
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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
      ideTrabalhador.infoMV.indMV := eSStrToIndMV(Ok, INIRec.ReadString(sSecao, 'indMV', '1'));

      I := 1;
      while true do
      begin
        // de 01 até 10
        sSecao := 'remunOutrEmpr' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrabalhador.infoMV.remunOutrEmpr.Add do
        begin
          tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          nrInsc     := sFim;
          codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
          vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
        end;

        Inc(I);
      end;

      sSecao := 'infoComplem';
      ideTrabalhador.infoComplem.nmTrab       := INIRec.ReadString(sSecao, 'nmTrab', '');
      ideTrabalhador.infoComplem.dtNascto     := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      ideTrabalhador.infoComplem.codCBO       := INIRec.ReadString(sSecao, 'codCBO', '');
      ideTrabalhador.infoComplem.natAtividade := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', '1'));
      ideTrabalhador.infoComplem.qtdDiasTrab  := INIRec.ReadInteger(sSecao, 'qtdDiasTrab', 0);

      sSecao := 'sucessaoVinc';
      ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt := INIRec.ReadString(sSecao, 'cnpjEmpregAnt', '');
      ideTrabalhador.infoComplem.sucessaoVinc.matricAnt     := INIRec.ReadString(sSecao, 'matricAnt', '');
      ideTrabalhador.infoComplem.sucessaoVinc.dtAdm         := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
      ideTrabalhador.infoComplem.sucessaoVinc.observacao    := INIRec.ReadString(sSecao, 'observacao', '');

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrabalhador.procJudTrab.Add do
        begin
          tpTrib     := eSStrToTpTributo(Ok, INIRec.ReadString(sSecao, 'tpTrib', '1'));
          nrProcJud  := sFim;
          codSusp    := INIRec.ReadInteger(sSecao, 'codSusp', 0);
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with dmDev.Add do
        begin
          ideDmDev := sFim;
          codCateg := INIRec.ReadInteger(sSecao, 'codCateg', 0);

          J := 1;
          while true do
          begin
            // de 001 até 500
            sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerApur.ideEstabLot.Add do
            begin
              tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
              nrInsc     := sFim;
              codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');
              qtdDiasAv  := INIRec.ReadInteger(sSecao, 'qtdDiasAv', 0);

              K := 1;
              while true do
              begin
                // de 1 até 8
                sSecao := 'remunPerApur' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                             IntToStrZero(K, 1);
                sFim   := INIRec.ReadString(sSecao, 'matricula', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with remunPerApur.Add do
                begin
                  matricula  := sFim;
                  indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

                  L := 1;
                  while true do
                  begin
                    // de 001 até 200
                    sSecao := 'itensRemun' + IntToStrZero(I, 2) +
                                IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with itensRemun.Add do
                    begin
                      codRubr    := sFim;
                      ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                      qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                      fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                      vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                      vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                    end;

                    Inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detOper' + IntToStrZero(I, 2) +
                                IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with infoSaudeColet.detOper.Add do
                    begin
                      cnpjOper := sFim;
                      regANS   := INIRec.ReadString(sSecao, 'regANS', '');
                      vrPgTit  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrPgTit', ''), 0);

                      M := 1;
                      while true do
                      begin
                        // de 01 até 99
                        sSecao := 'detPlano' + IntToStrZero(I, 2) +
                                    IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                    IntToStrZero(L, 2) + IntToStrZero(M, 2);
                        sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with detPlano.Add do
                         begin
                          tpDep    := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '00'));
                          cpfDep   := sFim;
                          nmDep    := INIRec.ReadString(sSecao, 'nmDep', '');
                          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                          vlrPgDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgDep', ''), 0);
                        end;

                        Inc(M);
                      end;

                    end;

                    Inc(L);
                  end;

                  sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) +
                                    IntToStrZero(J, 3) + IntToStrZero(K, 1);
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
            sSecao := 'ideADC' + IntToStrZero(I, 2) + IntToStrZero(J, 1);
            sFim   := INIRec.ReadString(sSecao, 'dtAcConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerAnt.ideADC.Add do
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
                sSecao := 'idePeriodo' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                   IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with idePeriodo.Add do
                begin
                  perRef := sFim;

                  L := 1;
                  while true do
                  begin
                    // de 001 até 500
                    sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                       IntToStrZero(K, 3) + IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with ideEstabLot.Add do
                    begin
                      tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
                      nrInsc     := sFim;
                      codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');

                      M := 1;
                      while true do
                      begin
                        // de 1 até 8
                        sSecao := 'remunPerAnt' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                           IntToStrZero(K, 3) + IntToStrZero(L, 3) + IntToStrZero(M, 1);
                        sFim   := INIRec.ReadString(sSecao, 'matricula', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with remunPerAnt.Add do
                        begin
                          matricula  := sFim;
                          indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

                          N := 1;
                          while true do
                          begin
                            // de 001 até 200
                            sSecao := 'itensRemun' + IntToStrZero(I, 2) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                        IntToStrZero(L, 3) + IntToStrZero(M, 1) +
                                        IntToStrZero(N, 3);
                            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                            if (sFim = 'FIM') or (Length(sFim) <= 0) then
                              break;

                            with itensRemun.Add do
                            begin
                              codRubr    := sFim;
                              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                              qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                              fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                              vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                            end;

                            Inc(N);
                          end;

                          sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 1) +
                                        IntToStrZero(L, 3) + IntToStrZero(M, 1);
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
            sSecao := 'infoTrabInterm' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codConv', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoTrabInterm.Add do
            begin
              codConv := sFim;
            end;

            Inc(J);
          end;

        end;

        Inc(I);
      end;
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

procedure TEvtRemun.GerarInfoInterm;
begin
  Gerador.wGrupo('infoInterm');

  Gerador.wCampo(tcInt, '', 'qtdDiasInterm', 1, 2, 1, ideTrabalhador.infoInterm.qtdDiasInterm);

  Gerador.wGrupo('/infoInterm');
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
  FTipoEvento := teS1200;
  FEvtRemun := TEvtRemun.Create(AOwner);
end;

destructor TS1200CollectionItem.Destroy;
begin
  FEvtRemun.Free;

  inherited;
end;

procedure TS1200CollectionItem.setEvtRemun(const Value: TEvtRemun);
begin
  FEvtRemun.Assign(Value);
end;

{ TS1200Collection }

function TS1200Collection.Add: TS1200CollectionItem;
begin
  Result := TS1200CollectionItem(inherited Add);
  Result.Create(TComponent(self.Owner));
end;

function TS1200Collection.GetItem(Index: integer): TS1200CollectionItem;
begin
  Result := TS1200CollectionItem(inherited GetItem(Index));
end;

procedure TS1200Collection.SetItem(Index: integer; Value: TS1200CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoTrabInternCollection }

function TInfoTrabIntermCollection.Add: TInfoTrabIntermCollectionItem;
begin
  Result := TInfoTrabIntermCollectionItem(inherited Add());
end;

constructor TInfoTrabIntermCollection.Create;
begin
  inherited Create(TInfoTrabIntermCollectionItem);
end;

function TInfoTrabIntermCollection.GetItem(
  Index: integer): TInfoTrabIntermCollectionItem;
begin
  Result := TInfoTrabIntermCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTrabIntermCollection.SetItem(Index: integer;
  Value: TInfoTrabIntermCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
