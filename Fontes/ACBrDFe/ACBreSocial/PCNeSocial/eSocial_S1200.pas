{ ****************************************************************************** }
{ Projeto: Componente ACBreSocial }
{ Biblioteca multiplataforma de componentes Delphi para envio dos eventos do }
{ eSocial - http://www.esocial.gov.br/ }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto }
{ Daniel Simoes de Almeida }
{ André Ferreira de Moraes }

{ Colaboradores nesse arquivo: }

{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr }

{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }

{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }

{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }

{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
  |*  - Doação do componente para o Projeto ACBr
  |* 01/03/2016: Guilherme Costa
  |*  - Alterações para validação com o XSD
  ****************************************************************************** }
{$I ACBr.inc}
unit eSocial_S1200;

interface

uses
  SysUtils, Classes, Dialogs, Controls,
  pcnConversao, ACBrUtil,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

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

    procedure GerarIdeTrabalhador();
    procedure GerarInfoComplem();
    procedure GerarSucessaoVinc();
    procedure GerarDmDev();
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;

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

implementation

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
  FInfoPerApur := nil;
  FInfoPerAnt := nil;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
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
  iIdeADC: integer;
begin
  for iIdeADC := 0 to objIdeADC.Count - 1 do
  begin
    Gerador.wGrupo('ideADC');
    Gerador.wCampo(tcDat, '', 'dtAcConv', 0, 0, 0, objIdeADC.Items[iIdeADC].dtAcConv);
    Gerador.wCampo(tcStr, '', 'tpAcConv', 0, 0, 0, eSTpAcConvToStr(objIdeADC.Items[iIdeADC].tpAcConv));
    Gerador.wCampo(tcStr, '', 'compAcConv', 0, 0, 0, objIdeADC.Items[iIdeADC].compAcConv);
    Gerador.wCampo(tcDat, '', 'dtEfAcConv', 0, 0, 0, objIdeADC.Items[iIdeADC].dtEfAcConv);
    Gerador.wCampo(tcStr, '', 'dsc', 0, 0, 0, objIdeADC.Items[iIdeADC].dsc);
    Gerador.wCampo(tcStr, '', 'remunSuc', 0, 0, 0, eSSimNaoToStr(objIdeADC.Items[iIdeADC].remunSuc));
    GerarIdePeriodo(objIdeADC.Items[iIdeADC].idePeriodo);
    Gerador.wGrupo('/ideADC');
  end;
end;

procedure TEvtRemun.GerarIdeEstabLot(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerApur');
var
  iIdeEstabLot: integer;
begin
  for iIdeEstabLot := 0 to objIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');
    Gerador.wCampo(tcInt, '', 'tpInsc', 0, 0, 0, eSTpInscricaoToStr(objIdeEstabLot.Items[iIdeEstabLot].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, objIdeEstabLot.Items[iIdeEstabLot].nrInsc);
    Gerador.wCampo(tcStr, '', 'codLotacao', 0, 0, 0, objIdeEstabLot.Items[iIdeEstabLot].codLotacao);
    Gerador.wCampo(tcInt, '', 'qtdDiasAv', 0, 0, 0, objIdeEstabLot.Items[iIdeEstabLot].qtdDiasAv);
    GerarRemunPer(objIdeEstabLot.Items[iIdeEstabLot].remunPerApur, nomeRemunPer);
    Gerador.wGrupo('/ideEstabLot');
  end;
end;

procedure TEvtRemun.GerarIdeEstabLot2(objIdeEstabLot: TIdeEstabLotCollection; nomeRemunPer: string = 'remunPerAnt');
var
  iIdeEstabLot: integer;
begin
  for iIdeEstabLot := 0 to objIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');
    Gerador.wCampo(tcInt, '', 'tpInsc', 0, 0, 0, eSTpInscricaoToStr(objIdeEstabLot.Items[iIdeEstabLot].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, objIdeEstabLot.Items[iIdeEstabLot].nrInsc);
    Gerador.wCampo(tcStr, '', 'codLotacao', 0, 0, 0, objIdeEstabLot.Items[iIdeEstabLot].codLotacao);
    GerarRemunPer(objIdeEstabLot.Items[iIdeEstabLot].remunPerAnt, nomeRemunPer);
    Gerador.wGrupo('/ideEstabLot');
  end;
end;

procedure TEvtRemun.GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
var
  iIdePeriodo: integer;
begin
  for iIdePeriodo := 0 to objIdePeriodo.Count - 1 do
  begin
    Gerador.wGrupo('idePeriodo');
    Gerador.wCampo(tcStr, '', 'perRef', 0, 0, 0, objIdePeriodo.Items[iIdePeriodo].perRef);
    GerarIdeEstabLot2(objIdePeriodo.Items[iIdePeriodo].ideEstabLot, 'remunPerAnt');
    Gerador.wGrupo('/idePeriodo');
  end;
end;

procedure TEvtRemun.GerarIdeTrabalhador;
begin
  Gerador.wGrupo('ideTrabalhador');
  Gerador.wCampo(tcStr, '', 'cpfTrab', 0, 0, 0, ideTrabalhador.cpfTrab);
  Gerador.wCampo(tcStr, '', 'nisTrab', 0, 0, 0, ideTrabalhador.nisTrab);
  if (ideTrabalhador.infoMVInst()) then
    GerarInfoMV(ideTrabalhador.infoMV);
  if (ideTrabalhador.infoComplemInst()) then
    GerarInfoComplem();
  if (ideTrabalhador.procJudTrabInst()) then
    GerarProcJudTrab(ideTrabalhador.procJudTrab);
  Gerador.wGrupo('/ideTrabalhador');
end;

procedure TEvtRemun.GerarSucessaoVinc;
begin
  Gerador.wGrupo('sucessaoVinc');
  Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt', 0, 0, 0, ideTrabalhador.infoComplem.sucessaoVinc.cnpjEmpregAnt);
  Gerador.wCampo(tcStr, '', 'matricAnt', 0, 0, 0, ideTrabalhador.infoComplem.sucessaoVinc.matricAnt);
  Gerador.wCampo(tcDat, '', 'dtAdm', 0, 0, 0, ideTrabalhador.infoComplem.sucessaoVinc.dtAdm);
  Gerador.wCampo(tcStr, '', 'observacao', 0, 0, 0, ideTrabalhador.infoComplem.sucessaoVinc.observacao);
  Gerador.wGrupo('/sucessaoVinc');
end;

procedure TEvtRemun.GerarInfoComplem;
begin
  Gerador.wGrupo('infoComplem');
  Gerador.wCampo(tcStr, '', 'nmTrab', 0, 0, 0, ideTrabalhador.infoComplem.nmTrab);
  Gerador.wCampo(tcDat, '', 'dtNascto', 0, 0, 0, ideTrabalhador.infoComplem.dtNascto);
  Gerador.wCampo(tcStr, '', 'codCBO', 0, 0, 0, ideTrabalhador.infoComplem.codCBO);
  Gerador.wCampo(tcStr, '', 'natAtividade', 0, 0, 0, eSNatAtividadeToStr(ideTrabalhador.infoComplem.natAtividade));
  Gerador.wCampo(tcInt, '', 'qtdDiasTrab', 0, 0, 0, ideTrabalhador.infoComplem.qtdDiasTrab);
  if ideTrabalhador.infoComplem.sucessaoVincInst() then
    GerarSucessaoVinc();
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
    Gerador.wCampo(tcStr, '', 'ideDmDev', 0, 0, 0, dmDev[i].ideDmDev);
    Gerador.wCampo(tcInt, '', 'codCateg', 0, 0, 0, dmDev[i].codCateg);
    if (dmDev[i].infoPerApurInst()) then
      GerarInfoPerApur(dmDev[i].infoPerApur);
    if (dmDev[i].infoPerAntInst()) then
      GerarInfoPerAnt(dmDev[i].infoPerAnt);
    Gerador.wGrupo('/dmDev');
  end;
end;

procedure TEvtRemun.GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
begin
  Gerador.wGrupo('infoPerApur');
  GerarIdeEstabLot(pInfoPerApur.ideEstabLot);
  Gerador.wGrupo('/infoPerApur');
end;

procedure TEvtRemun.GerarRemunPer(objRemunPer: TRemunPer1200Collection; nomeRemunPer: string = 'remunPerApur');
var iRemunPer: integer;
begin
  for iRemunPer := 0 to objRemunPer.Count - 1 do
  begin
    Gerador.wGrupo(nomeRemunPer);
    Gerador.wCampo(tcStr, '', 'matricula', 0, 0, 0, objRemunPer.Items[iRemunPer].matricula);
    if ord(objRemunPer.Items[iRemunPer].indSimples) > 0 then
      Gerador.wCampo(tcStr, '', 'indSimples', 0, 0, 0, eSIndSimplesToStr(objRemunPer.Items[iRemunPer].indSimples));
    GerarItensRemun(objRemunPer.Items[iRemunPer].itensRemun, 'itensRemun');
    if (nomeRemunPer = 'remunPerApur') then
    begin
      if objRemunPer.Items[iRemunPer].infoSaudeColetInst() then
        GerarInfoSaudeColet(objRemunPer.Items[iRemunPer].infoSaudeColet);
    end;
    if (objRemunPer.Items[iRemunPer].infoAgNocivoInst()) then
      GerarInfoAgNocivo(objRemunPer.Items[iRemunPer].infoAgNocivo);
    Gerador.wGrupo('/' + nomeRemunPer);
  end;
end;

function TEvtRemun.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtRemun');
    Gerador.wGrupo('evtRemun Id="' + GerarChaveEsocial(now, self.ideEmpregador.nrInsc, 0) + '"');
    GerarIdeEvento3(self.ideEvento);
    GerarIdeEmpregador(self.ideEmpregador);
    GerarIdeTrabalhador();
    GerarDmDev();
    Gerador.wGrupo('/evtRemun');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtRemun');
    Validar('evtRemun');
  except
    on e: Exception do
      raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
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

end.
