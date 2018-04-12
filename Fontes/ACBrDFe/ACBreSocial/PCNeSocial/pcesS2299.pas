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
|* 01/03/2016: Guilherme Costa
|*  - Alterações para validação com o XSD
|* 15/02/2018 - EdmarFrazão
|*  Alterações  Declaração e Campo Opcional ftransfTit
******************************************************************************}
{$I ACBr.inc}

unit pcesS2299;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2299Collection = class;
  TS2299CollectionItem = class;
  TEvtDeslig = class;
  TInfoDeslig = class;
  TVerbasRescS2299 = class;
  TDmDevCollection = class;
  TDMDevCollectionItem = class;
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

  TS2299Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2299CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2299CollectionItem);
  public
    function Add: TS2299CollectionItem;
    property Items[Index: Integer]: TS2299CollectionItem read GetItem write SetItem; default;
  end;

  TS2299CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtDeslig : TEvtDeslig;
    procedure setEvtDeslig(const Value: TEvtDeslig);
  public
    constructor Create(AOwner: Tcomponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtDeslig: TEvtDeslig read FEvtDeslig write setEvtDeslig;
  end;

  TEvtDeslig = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoDeslig: TInfoDeslig;
    FACBreSocial: TObject;

    procedure GerarInfoDeslig(obj: TInfoDeslig);
    procedure GerarSucessaoVinc(obj: TSucessaoVinc);
    procedure GerarVerbasResc(obj: TVerbasRescS2299);
    procedure GerarDmDev(pDmDev: TDmDevCollection);
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
    procedure GerarIdeADC(pIdeADC: TIdeADCCollection);
    procedure GerarIdePeriodo(pIdePeriodo: TIdePeriodoCollection);
    procedure GerarconsigFGTS(obj: TConsigFGTSCollection);
    procedure GerarTransfTit(obj: TtransfTit);
    procedure GerarInfoTrabInterm(obj: TInfoTrabIntermCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoDeslig: TInfoDeslig read FInfoDeslig write FInfoDeslig;
  end;

  TInfoDeslig = class(TPersistent)
  private
    FmtvDeslig: String;
    FdtDeslig: TDateTime;
    FindPagtoAPI : tpSimNao;
    FdtProjFimAPI : TDateTime;
    FPensAlim: tpPensaoAlim;
    FpercAliment: Double;
    FVrAlim: Double;
    FnrCertObito : String;
    FnrProcTrab : String;
    FIndCumprParc: tpCumprParcialAviso;
    FObservacao : String; // Descontinuado na versão 2.4.02
    Fobservacoes: TobservacoesCollection;
    FSucessaoVinc : TSucessaoVinc;
    FVerbasResc : TVerbasRescS2299;
    FQuarentena: TQuarentena;
    FconsigFGTS: TConsigFGTSCollection;
    FInfoASO: TInfoASO;
    FtransfTit: TtransfTit;
    FQtdDiasInterm: Integer;

    function getVerbasResc: TVerbasRescS2299;
  public
    constructor Create;
    destructor  Destroy; override;

    function verbasRescInst: boolean;

    property mtvDeslig: String read FmtvDeslig write FmtvDeslig;
    property dtDeslig: TDateTime read FdtDeslig write FdtDeslig;
    property indPagtoAPI : tpSimNao read FindPagtoAPI write FindPagtoAPI;
    property dtProjFimAPI : TDateTime read FdtProjFimAPI write FdtProjFimAPI;
    property percAliment : Double read FpercAliment write FpercAliment;
    property vrAlim: Double read FVrAlim write FVrAlim;
    property pensAlim: tpPensaoAlim read FPensAlim write FPensAlim;
    property nrCertObito : String read FnrCertObito write FnrCertObito;
    property nrProcTrab : String read FnrProcTrab write FnrProcTrab;
    property indCumprParc: tpCumprParcialAviso read FIndCumprParc write FIndCumprParc;
    property Observacao : String read FObservacao write FObservacao;
    property observacoes: TobservacoesCollection read Fobservacoes write Fobservacoes;
    property SucessaoVinc : TSucessaoVinc read FSucessaoVinc write FSucessaoVinc;
    property VerbasResc : TVerbasRescS2299 read getVerbasResc write FVerbasResc;
    property Quarentena: TQuarentena read FQuarentena write FQuarentena;
    property consigFGTS: TConsigFGTSCollection read FconsigFGTS write FconsigFGTS;
    property InfoASO : TInfoASO read FInfoASO write FInfoASO;
    property transfTit: TtransfTit read FtransfTit write FtransfTit;
    property QtdDiasInterm: Integer read FQtdDiasInterm write FQtdDiasInterm;
  end;

  TIdePeriodoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdePeriodoItem;
    procedure SetItem(Index: Integer; Value: TIdePeriodoItem);
  public
    constructor Create; reintroduce;
    function Add: TIdePeriodoItem;
    property Items[Index: Integer]: TIdePeriodoItem read GetItem write SetItem; default;
  end;

  TIdePeriodoItem = class(TCollectionItem)
  private
    FPerRef: string;
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property perRef: string read FPerRef write FPerRef;
    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TIdeADCCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeADCItem;
    procedure SetItem(Index: Integer; Value: TIdeADCItem);
  public
    constructor Create; reintroduce;

    function Add: TIdeADCItem;
    property Items[Index: Integer]: TIdeADCItem read GetItem write SetItem; default;
  end;

  TIdeADCItem = class(TCollectionItem)
  private
    FDtAcConv: TDateTime;
    FTpAcConv: tpTpAcConv;
    FcompAcConv: String;
    FDtEfAcConv: TDateTime;
    FDsc: String;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property dtAcConv: TDateTime read FDtAcConv write FDtAcConv;
    property tpAcConv: tpTpAcConv read FTpAcConv write FTpAcConv;
    property compAcConv: String read FcompAcConv write FcompAcConv;
    property dtEfAcConv: TDateTime read FdtEfAcConv write FdtEfAcConv;
    property dsc: string read FDsc write FDsc;
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
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TDmDevCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDMDevCollectionItem;
    procedure SetItem(Index: Integer; Value: TDMDevCollectionItem);
  public
    constructor Create; reintroduce;

    function Add: TDMDevCollectionItem;
    property Items[Index: Integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDMDevCollectionItem = class(TCollectionItem)
  private
    FIdeDmDev: string;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;
    FinfoTrabInterm: TinfoTrabIntermCollection;

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function infoPerApurInst: boolean;
    function infoPerAntInst: boolean;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
    property infoTrabInterm: TinfoTrabIntermCollection read FinfoTrabInterm write FinfoTrabInterm;
  end;

  TVerbasRescS2299 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollection;
  public
    constructor Create; reintroduce;

    property dmDev: TDmDevCollection read FDmDev write FDmDev;
  end;

  TConsigFGTSCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TConsigFGTSItem;
    procedure SetItem(Index: Integer; Value: TConsigFGTSItem);
  public
    constructor Create; reintroduce;
    function Add: TConsigFGTSItem;
    property Items[Index: Integer]: TConsigFGTSItem read GetItem write SetItem; default;
  end;

  TConsigFGTSItem = class(TCollectionItem)
  private
    FidConsig: tpSimNao;
    FinsConsig: string;
    FnrContr: string;
  public
    property idConsig: tpSimNao read FidConsig write FidConsig;
    property insConsig: string read FinsConsig write FinsConsig;
    property nrContr: string read FnrContr write FnrContr;
  end;

  TtransfTit = class(TPersistent)
  private
    FcpfSubstituto: string;
    FdtNascto: TDateTime;
  public
    property cpfSubstituto: string read FcpfSubstituto write FcpfSubstituto;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
  end;

  TInfoTrabIntermCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoTrabIntermItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabIntermItem);
  public
    constructor Create; reintroduce;
    function Add: TInfoTrabIntermItem;
    property Items[Index: Integer]: TInfoTrabIntermItem read GetItem write SetItem; default;
  end;

  TInfoTrabIntermItem = class(TCollectionItem)
  private
    FcodConv: string;
  public
    property codConv: string read FcodConv write FcodConv;
  end;

implementation

uses
  IniFiles,StrUtils,
  ACBreSocial, ACBrDFeUtil;

{ TS2299Collection }

function TS2299Collection.Add: TS2299CollectionItem;
begin
  Result := TS2299CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2299Collection.GetItem(Index: Integer): TS2299CollectionItem;
begin
  Result := TS2299CollectionItem(inherited GetItem(Index));
end;

procedure TS2299Collection.SetItem(Index: Integer; Value: TS2299CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoDeslig }

constructor TInfoDeslig.Create;
begin
  inherited;
  FSucessaoVinc := TSucessaoVinc.Create;
  FVerbasResc   := nil;
  FQuarentena   := TQuarentena.Create;
  FInfoASO      := TInfoASO.Create;
  FtransfTit    := TtransfTit.Create;
  Fobservacoes  := TobservacoesCollection.Create;
  FconsigFGTS   := TConsigFGTSCollection.Create;
end;

destructor TInfoDeslig.destroy;
begin
  FSucessaoVinc.Free;
  FreeAndNil(FVerbasResc);
  FQuarentena.Free;
  FInfoASO.Free;
  FtransfTit.Free;
  Fobservacoes.Free;
  FreeAndNil(FconsigFGTS);
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

{ TS2299CollectionItem }

constructor TS2299CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento  := teS2299;
  FEvtDeslig := TEvtDeslig.Create(AOwner);
end;

destructor TS2299CollectionItem.Destroy;
begin
  FEvtDeslig.Free;

  inherited;
end;

procedure TS2299CollectionItem.setEvtDeslig(const Value: TEvtDeslig);
begin
  FEvtDeslig.Assign(Value);
end;

{ TDmDevCollection }

constructor TDmDevCollection.Create;
begin
  inherited Create(TDMDevCollectionItem);
end;

function TDmDevCollection.Add: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Add);
  Result.Create;
end;

function TDmDevCollection.GetItem(Index: Integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited GetItem(Index));
end;

procedure TDmDevCollection.SetItem(Index: Integer; Value: TDMDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  FInfoPerApur := nil;
  FInfoPerAnt := nil;
  FInfoTrabInterm := TInfoTrabIntermCollection.Create;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
  FInfoTrabInterm.Free;

  inherited;
end;

function TDMDevCollectionItem.getInfoPerApur: TInfoPerApur;
begin
  if not Assigned(FInfoPerApur) then
    FInfoPerApur := TInfoPerApur.Create;
  result := FInfoPerApur;
end;

function TDMDevCollectionItem.getInfoPerAnt: TInfoPerAnt;
begin
  if not Assigned(FInfoPerAnt) then
    FInfoPerAnt := TInfoPerAnt.Create;
  result := FInfoPerAnt;
end;

function TDMDevCollectionItem.infoPerApurInst: boolean;
begin
  result := Assigned(FInfoPerApur);
end;

function TDMDevCollectionItem.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

{ TInfoPerApur }

constructor TInfoPerApur.Create;
begin
  FIdeEstabLot := TideEstabLotCollection.Create;
end;

destructor TInfoPerApur.Destroy;
begin
  FIdeEstabLot.Free;
end;

{ TInfoPerAnt }

constructor TInfoPerAnt.Create;
begin
  FIdeADC := TIdeADCCollection.Create;
end;

destructor TInfoPerAnt.Destroy;
begin
  FIdeADC.Free;
end;

{ TIdeADCCollection }

constructor TIdeADCCollection.Create;
begin
  inherited Create(TIdeADCItem);
end;

function TIdeADCCollection.Add: TIdeADCItem;
begin
  Result := TIdeADCItem(inherited Add);
  Result.Create;
end;

function TIdeADCCollection.GetItem(Index: Integer): TIdeADCItem;
begin
  Result := TIdeADCItem(inherited GetItem(Index));
end;

procedure TIdeADCCollection.SetItem(Index: Integer; Value: TIdeADCItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdeADCItem }

constructor TIdeADCItem.Create;
begin
  FIdePeriodo := TIdePeriodoCollection.Create;
end;

destructor TIdeADCItem.Destroy;
begin
  FIdePeriodo.Free;

  inherited;
end;

{ TIdePeriodoCollection }

constructor TIdePeriodoCollection.Create;
begin
  inherited Create(TIdePeriodoItem);
end;

function TIdePeriodoCollection.Add: TIdePeriodoItem;
begin
  Result := TIdePeriodoItem(inherited Add);
  Result.Create;
end;

function TIdePeriodoCollection.GetItem(Index: Integer): TIdePeriodoItem;
begin
  Result := TIdePeriodoItem(inherited GetItem(Index));
end;

procedure TIdePeriodoCollection.SetItem(Index: Integer; Value: TIdePeriodoItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdePeriodoItem }

constructor TIdePeriodoItem.Create;
begin
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
  inherited;

  FDmDev := TDmDevCollection.Create;
end;

{ TInfoTrabIntermCollection }

function TInfoTrabIntermCollection.Add: TInfoTrabIntermItem;
begin
  Result := TInfoTrabIntermItem(inherited Add);
//  Result.Create;
end;

constructor TInfoTrabIntermCollection.Create;
begin
  inherited Create(TInfoTrabIntermItem);
end;

function TInfoTrabIntermCollection.GetItem(
  Index: Integer): TInfoTrabIntermItem;
begin
  Result := TInfoTrabIntermItem(inherited GetItem(Index));
end;

procedure TInfoTrabIntermCollection.SetItem(Index: Integer;
  Value: TInfoTrabIntermItem);
begin
  inherited SetItem(Index, Value);
end;

{ TConsigFGTSCollection }

function TConsigFGTSCollection.Add: TConsigFGTSItem;
begin
  Result := TConsigFGTSItem(inherited Add);
end;

constructor TConsigFGTSCollection.Create;
begin
  inherited Create(TConsigFGTSItem);
end;

function TConsigFGTSCollection.GetItem(Index: Integer): TConsigFGTSItem;
begin
  Result := TConsigFGTSItem(inherited GetItem(Index));
end;

procedure TConsigFGTSCollection.SetItem(Index: Integer; Value: TConsigFGTSItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEvtDeslig }

constructor TEvtDeslig.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVInculo := TIdeVinculo.Create;
  FInfoDeslig := TInfoDeslig.Create;
end;

destructor TEvtDeslig.destroy;
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
  Gerador.wCampo(tcStr, '', 'indPagtoAPI',  1,  1, 1, eSSimNaoToStr(obj.indPagtoAPI));

  if (obj.indPagtoAPI=tpSim) then
    Gerador.wCampo(tcDat, '', 'dtProjFimAPI', 10, 10, 0, obj.dtProjFimAPI);

  Gerador.wCampo(tcStr, '', 'pensAlim',    1,  1, 1, obj.pensAlim);
  Gerador.wCampo(tcDe2, '', 'percAliment', 1,  5, 0, obj.percAliment);
  Gerador.wCampo(tcDe2, '', 'vrAlim',      1, 14, 0, obj.vrAlim);

  if ((obj.mtvDeslig='09') or (obj.mtvDeslig='10')) then
    Gerador.wCampo(tcStr, '', 'nrCertObito', 1, 32, 0, obj.nrCertObito);

  if (obj.mtvDeslig='17') then
    Gerador.wCampo(tcStr, '', 'nrProcTrab', 1, 20, 0, obj.nrProcTrab);

  Gerador.wCampo(tcStr, '', 'indCumprParc', 1,   1, 1, eSTpCumprParcialAvisoToStr(obj.indCumprParc));

  If (obj.QtdDiasInterm > 0) And (VersaoDF <> ve02_04_01) Then
     Gerador.wCampo(tcInt, '', 'qtdDiasInterm', 1,   2, 0, obj.QtdDiasInterm);

  if (VersaoDF = ve02_04_01) then
    Gerador.wCampo(tcStr, '', 'observacao',   1, 255, 0, obj.Observacao)
  else
    GerarObservacoes(obj.observacoes);
  if (StrToIntDef(obj.mtvDeslig,0) in [11, 12, 13, 25, 28, 29, 30]) then
     GerarSucessaoVinc(obj.SucessaoVinc);
  if (obj.transfTit.cpfSubstituto <> '') And (obj.mtvDeslig='34') then
    GerarTransfTit(obj.transfTit);

  if obj.verbasRescInst then
    GerarVerbasResc(obj.VerbasResc);

  GerarQuarentena(obj.Quarentena);
  GerarconsigFGTS(obj.consigFGTS);

  Gerador.wGrupo('/infoDeslig');
end;

procedure TEvtDeslig.GerarSucessaoVinc(obj: TSucessaoVinc);
begin
  if obj.CnpjEmpSucessora <> EmptyStr then
  begin
    Gerador.wGrupo('sucessaoVinc');

    Gerador.wCampo(tcStr, '', 'cnpjSucessora', 14, 14, 1, obj.CnpjEmpSucessora);

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

    Gerador.wCampo(tcDat, '', 'dtAcConv',   10,  10, 1, pIdeADC[i].dtAcConv);
    Gerador.wCampo(tcStr, '', 'tpAcConv',    1,   1, 1, eSTpAcConvToStr(pIdeADC[i].tpAcConv));
    Gerador.wCampo(tcStr, '', 'compAcConv',  7,   7, 0, pIdeADC[i].compAcConv);
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

procedure TEvtDeslig.GerarDmDev(pDmDev: TDmDevCollection);
var
  i: integer;
begin
  for i := 0 to pDmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcStr, '', 'ideDmDev', 1, 30, 1, pDmDev[i].ideDmDev);
    if pDmDev[i].infoPerApurInst then
      GerarInfoPerApur(pDmDev[i].infoPerApur);

    if pDmDev[i].infoPerAntInst then
      GerarInfoPerAnt(pDmDev[i].infoPerAnt);

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

  Gerador.wGrupo('/verbasResc');
end;

function TEvtDeslig.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtDeslig');
    Gerador.wGrupo('evtDeslig Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoDeslig(Self.InfoDeslig);
              
    Gerador.wGrupo('/evtDeslig');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtDeslig');

    Validar(schevtDeslig);
  except on e:exception do
    raise Exception.Create(e.Message);
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

    if obj.Count > 9 then
      Gerador.wAlerta('', 'consigFGTS', 'Informações sobre operação de crédito consignado com garantia de FGTS', ERR_MSG_MAIOR_MAXIMO + '9')
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

function TEvtDeslig.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtDeslig';
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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
      infoDeslig.dtDeslig     := StringToDateTime(INIRec.ReadString(sSecao, 'dtDeslig', '0'));
      infoDeslig.indPagtoAPI  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPagtoAPI', 'S'));
      infoDeslig.dtProjFimAPI := StringToDateTime(INIRec.ReadString(sSecao, 'dtProjFimAPI', '0'));
      infoDeslig.pensAlim     := eSStrToTpPensaoAlim(Ok, INIRec.ReadString(sSecao, 'pensAlim', '0'));
      infoDeslig.percAliment  := StringToFloatDef(INIRec.ReadString(sSecao, 'percAliment', ''), 0);
      infoDeslig.vrAlim       := StringToFloatDef(INIRec.ReadString(sSecao, 'vrAlim', ''), 0);
      infoDeslig.nrCertObito  := INIRec.ReadString(sSecao, 'nrCertObito', EmptyStr);
      infoDeslig.nrProcTrab   := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      infoDeslig.indCumprParc := eSStrToTpCumprParcialAviso(Ok, INIRec.ReadString(sSecao, 'indCumprParc', '0'));
      infoDeslig.Observacao   := INIRec.ReadString(sSecao, 'observacao', EmptyStr);

      sSecao := 'sucessaoVinc';
      infoDeslig.sucessaoVinc.CnpjEmpSucessora := INIRec.ReadString(sSecao, 'cnpjEmpSucessora', EmptyStr);

      sSecao := 'transfTit';
      infoDeslig.transfTit.cpfSubstituto := INIRec.ReadString(sSecao, 'cpfSubstituto', EmptyStr);
      infoDeslig.transfTit.dtNascto      := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));

      I := 1;
      while true do
      begin
        // de 01 até 50
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.VerbasResc.dmDev.Add do
        begin
          ideDmDev := sFim;

          J := 1;
          while true do
          begin
            // de 01 até 24
            sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerApur.ideEstabLot.Add do
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

                with detVerbas.Add do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
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

                with infoSaudeColet.detOper.Add do
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

                    with detPlano.Add do
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
              infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));

              sSecao := 'infoSimples' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

              K := 1;
              while true do
              begin
                // de 1 até 8
                sSecao := 'ideADC' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                            IntToStrZero(K, 1);
                sFim   := INIRec.ReadString(sSecao, 'dtAcConv', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoPerAnt.ideADC.Add do
                begin
                  dtAcConv   := StringToDateTime(sFim);
                  tpAcConv   := eSStrToTpAcConv(Ok, INIRec.ReadString(sSecao, 'tpAcConv', 'A'));
                  compAcConv := INIRec.ReadString(sSecao, 'compAcConv', EmptyStr);
                  dtEfAcConv := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfAcConv', '0'));
                  dsc        := INIRec.ReadString(sSecao, 'dsc', EmptyStr);

                  L := 1;
                  while true do
                  begin
                    // de 001 até 180
                    sSecao := 'idePeriodo' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                IntToStrZero(K, 1) + IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with idePeriodo.Add do
                    begin
                      perRef := sFim;

                      M := 1;
                      while true do
                      begin
                        // de 01 até 24
                        sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                    IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                    IntToStrZero(M, 1);
                        sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with ideEstabLot.Add do
                        begin
                          tpInsc     := eSStrToTpInscricao(Ok, sFim);
                          NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
                          codLotacao := INIRec.ReadString(sSecao, 'codLotacao', EmptyStr);

                          N := 1;
                          while true do
                          begin
                            // de 001 até 200
                            sSecao := 'detVerbas' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                    IntToStrZero(K, 1) + IntToStrZero(L, 3) +
                                    IntToStrZero(M, 1) + IntToStrZero(N, 3);
                            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                            if (sFim = 'FIM') or (Length(sFim) <= 0) then
                              break;

                            with detVerbas.Add do
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

                        end;

                        Inc(M);
                      end;

                    end;

                    Inc(L);
                  end;

                end;

                Inc(K);
              end;

              sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));

              sSecao := 'infoSimples' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 00 até 99
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

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpTrib', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.VerbasResc.procJudTrab.Add do
        begin
          tpTrib    := eSStrToTpTributo(Ok, sFim);
          nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);
          codSusp   := INIRec.ReadInteger(sSecao, 'codSusp', 0);
        end;

        Inc(I);
      end;

      sSecao := 'infoMV';
      infoDeslig.VerbasResc.infoMV.indMV := eSStrToIndMV(Ok, INIRec.ReadString(sSecao, 'indMV', '1'));

      I := 1;
      while true do
      begin
        // de 01 até 10
        sSecao := 'remunOutrEmpr' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.VerbasResc.infoMV.remunOutrEmpr.Add do
        begin
          TpInsc     := eSStrToTpInscricao(Ok, sFim);
          NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
          codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
          vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
        end;

        Inc(I);
      end;

      sSecao := 'quarentena';
      infoDeslig.quarentena.dtFimQuar := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimQuar', '0'));

      I := 1;
      while true do
      begin
        // de 01 até 10
        sSecao := 'consigFGTS' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'idConsig', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoDeslig.consigFGTS.Add do
        begin
          idConsig  := eSStrToSimNao(Ok, sFim);
          insConsig := INIRec.ReadString(sSecao, 'insConsig', EmptyStr);
          nrContr   := INIRec.ReadString(sSecao, 'nrContr', EmptyStr);
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

end.
