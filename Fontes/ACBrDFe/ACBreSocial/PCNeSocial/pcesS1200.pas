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

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function infoPerApurInst(): boolean;
    function infoPerAntInst(): boolean;
  published
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property codCateg: integer read FCodCateg write FCodCateg;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
  end;

  TEvtRemun = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TeS1200IdeTrabalhador;
    FDMDev: TDMDevCollection;
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
    function getInfoAgNocivo: TInfoAgNocivo;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function infoAgNocivoInst(): boolean;

    property indSimples: tpIndSimples read FIndSimples write FIndSimples;
    property infoAgNocivo: TInfoAgNocivo read getInfoAgNocivo write FInfoAgNocivo;
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

    function getInfoComplem: TInfoComplem;
    function getInfoMV: TInfoMV;
    function getInfoProcJudTrab: TProcJudTrabCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function infoMVInst(): boolean;
    function infoComplemInst(): boolean;
    function procJudTrabInst(): boolean;

    property infoMV: TInfoMV read getInfoMV write FInfoMV;
    property infoComplem: TInfoComplem read getInfoComplem write FInfoComplem;
    property procJudTrab: TProcJudTrabCollection read getInfoProcJudTrab write FProcJudTrab;
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

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TRemunPer1200CollectionItem }

constructor TRemunPer1200CollectionItem.Create;
begin
  FInfoAgNocivo := nil;
  inherited
end;

destructor TRemunPer1200CollectionItem.Destroy;
begin
  FreeAndNil(FInfoAgNocivo);
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
  FProcJudTrab := TProcJudTrabCollection.Create;
end;

destructor TeS1200IdeTrabalhador.Destroy;
begin
  FreeAndNil(FInfoMV);
  FreeAndNil(FInfoComplem);
  FProcJudTrab.Free;

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

function TDMDevCollectionItem.getInfoPerAnt: TInfoPerAnt;
begin
  if not(Assigned(FInfoPerAnt)) then
    FInfoPerAnt := TInfoPerAnt.Create;
  Result := FInfoPerAnt;
end;

function TDMDevCollectionItem.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

{ TEvtRemun }
constructor TEvtRemun.Create(AACBreSocial: TObject);
begin
  inherited;

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
  Gerador.wCampo(tcStr, '', 'codCBO',        1,  6, 1, ideTrabalhador.infoComplem.codCBO);

  if ideTrabalhador.infoComplem.natAtividade <> navNaoInformar then
    Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 0, eSNatAtividadeToStr(ideTrabalhador.infoComplem.natAtividade));

  Gerador.wCampo(tcInt, '', 'qtdDiasTrab',   1,  2, 0, ideTrabalhador.infoComplem.qtdDiasTrab);

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

    GerarInfoTrabInterm(dmDev[i].infoTrabInterm);
    
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

    Gerador.wGrupo('/' + nomeRemunPer);
  end;

  if objRemunPer.Count > 8 then
    Gerador.wAlerta('', nomeRemunPer, 'Lista de ' + nomeRemunPer, ERR_MSG_MAIOR_MAXIMO + '8');
end;

function TEvtRemun.GerarXML: boolean;
begin
  try
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
  I: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      // Falta Implementar
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
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
