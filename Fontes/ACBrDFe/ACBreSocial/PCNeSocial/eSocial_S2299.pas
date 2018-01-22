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
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S2299;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

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
  TconsigFGTS = class;

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
      procedure GerarInfoDeslig(obj: TInfoDeslig);
      procedure GerarSucessaoVinc(obj: TSucessaoVinc);
      procedure GerarVerbasResc(obj: TVerbasRescS2299);
      procedure GerarDmDev(pDmDev: TDmDevCollection);
      procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
      procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
      procedure GerarIdeADC(pIdeADC: TIdeADCCollection);
      procedure GerarIdePeriodo(pIdePeriodo: TIdePeriodoCollection);
      procedure GerarconsigFGTS(obj: TconsigFGTS);
    public
      constructor Create(AACBreSocial: TObject);overload;
      destructor  Destroy; override;

      function GerarXML: boolean; override;

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
      FObservacao : String;
      FSucessaoVinc : TSucessaoVinc;
      FVerbasResc : TVerbasRescS2299;
      FQuarentena: TQuarentena;
    FconsigFGTS: TconsigFGTS;
      FInfoASO: TInfoASO;

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
      property SucessaoVinc : TSucessaoVinc read FSucessaoVinc write FSucessaoVinc;
      property VerbasResc : TVerbasRescS2299 read getVerbasResc write FVerbasResc;
      property Quarentena: TQuarentena read FQuarentena write FQuarentena;
      property consigFGTS: TconsigFGTS read FconsigFGTS write FconsigFGTS;
      property InfoASO : TInfoASO read FInfoASO write FInfoASO;
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
    FDtEfAcConv: TDateTime;
    FDsc: String;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property dtAcConv: TDateTime read FDtAcConv write FDtAcConv;
    property tpAcConv: tpTpAcConv read FTpAcConv write FTpAcConv;
    property dtEfAcConv: TDateTime read FDtAcConv write FDtAcConv;
    property dsc: string read FDsc write FDsc;
    property idePeriodo: TIdePeriodoCollection read FIdePeriodo write FIdePeriodo;
  end;

  TInfoPerAnt = class(TPersistent)
  private
    FIdeADC: TIdeADCCollection;
  public
    constructor Create;
    destructor Destroy;

    property ideADC: TIdeADCCollection read FIdeADC write FIdeADC;
  end;

  TInfoPerApur = class(TPersistent)
  private
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create;
    destructor Destroy;

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
  end;

  TVerbasRescS2299 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollection;
  public
    constructor Create; reintroduce;

    property dmDev: TDmDevCollection read FDmDev write FDmDev;
  end;

  TconsigFGTS = class(TPersistent)
  private
    FidConsig: tpSimNao;
    FinsConsig: string;
    FnrContr: string;
  public
    property idConsig: tpSimNao read FidConsig write FidConsig;
    property insConsig: string read FinsConsig write FinsConsig;
    property nrContr: string read FnrContr write FnrContr;
  end;

implementation

uses
  eSocial_NaoPeriodicos;

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

{ TEvtDeslig }

constructor TEvtDeslig.Create(AACBreSocial: TObject);
begin
  inherited;
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
    Gerador.wCampo(tcStr, '', 'mtvDeslig', 0,0,0, obj.mtvDeslig);
    Gerador.wCampo(tcDat, '', 'dtDeslig', 0,0,0, obj.dtDeslig);
    Gerador.wCampo(tcStr, '', 'indPagtoAPI', 0,0,0, eSSimNaoToStr(obj.indPagtoAPI));
    if (obj.indPagtoAPI=tpSim) then
      Gerador.wCampo(tcDat, '', 'dtProjFimAPI', 0,0,0, obj.dtProjFimAPI);
    Gerador.wCampo(tcStr, '', 'pensAlim', 0, 0, 0, obj.pensAlim);
    Gerador.wCampo(tcDe2, '', 'percAliment', 0, 0, 0, obj.percAliment);
    Gerador.wCampo(tcDe2, '', 'vrAlim', 0, 0, 0, obj.vrAlim);
    if ((obj.mtvDeslig='09') or (obj.mtvDeslig='10')) then
      Gerador.wCampo(tcStr, '', 'nrCertObito', 0,0,0, obj.nrCertObito);
    if (obj.mtvDeslig='17') then
      Gerador.wCampo(tcStr, '', 'nrProcTrab', 0,0,0, obj.nrProcTrab);
    Gerador.wCampo(tcStr, '', 'indCumprParc', 0, 0, 0, eSTpCumprParcialAvisoToStr(obj.indCumprParc));
    Gerador.wCampo(tcStr, '', 'observacao', 0,0,0, obj.Observacao);
    GerarSucessaoVinc(obj.SucessaoVinc);
    if obj.verbasRescInst then
      GerarVerbasResc(obj.VerbasResc);

  GerarQuarentena(obj.Quarentena);
  GerarconsigFGTS(obj.consigFGTS);
  Gerador.wGrupo('/infoDeslig');
end;

procedure TEvtDeslig.GerarSucessaoVinc(obj: TSucessaoVinc);
begin
  if obj.cnpjEmpregAnt <> EmptyStr then
  begin
    Gerador.wGrupo('sucessaoVinc');
      Gerador.wCampo(tcStr, '', 'cnpjSucessora', 0,0,0, obj.cnpjEmpregAnt);
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
      Gerador.wCampo(tcStr, '', 'perRef', 0,0,0, pIdePeriodo[i].perRef);
      GerarIdeEstabLot(pIdePeriodo[i].ideEstabLot);
    Gerador.wGrupo('/idePeriodo');
  end;
end;

procedure TEvtDeslig.GerarIdeADC(pIdeADC: TIdeADCCollection);
var
  i: integer;
begin
  for i := 0 to pIdeADC.Count - 1 do
  begin
    Gerador.wGrupo('ideADC');
      Gerador.wCampo(tcDat, '', 'dtAcConv', 0,0,0, pIdeADC[i].dtAcConv);
      Gerador.wCampo(tcStr, '', 'tpAcConv', 0,0,0, eSTpAcConvToStr(pIdeADC[i].tpAcConv));
      Gerador.wCampo(tcDat, '', 'dtEfAcConv', 0,0,0, pIdeADC[i].dtEfAcConv);
      Gerador.wCampo(tcStr, '', 'dsc', 0,0,0, pIdeADC[i].dsc);
      GerarIdePeriodo(pIdeADC[i].idePeriodo);
    Gerador.wGrupo('/ideADC');
  end;
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
      Gerador.wCampo(tcStr, '', 'ideDmDev', 0,0,0, pDmDev[i].ideDmDev);
      if pDmDev[i].infoPerAntInst then
        GerarInfoPerApur(pDmDev[i].infoPerApur);
      if pDmDev[i].infoPerAntInst then
        GerarInfoPerAnt(pDmDev[i].infoPerAnt);
    Gerador.wGrupo('/dmDev');
  end;
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
    GerarCabecalho('evtDeslig');
      Gerador.wGrupo('evtDeslig Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');//versao="'+Self.versao+'"
        //gerarIdVersao(self);
        gerarIdeEvento2(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        gerarIdeVinculo(self.IdeVinculo);
        GerarInfoDeslig(Self.InfoDeslig);
      Gerador.wGrupo('/evtDeslig');
    GerarRodape;
    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtDeslig');
    Validar('evtDeslig');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;
  Result := (Gerador.ArquivoFormatoXML <> '')
end;

procedure TEvtDeslig.GerarconsigFGTS(obj: TconsigFGTS);
begin
  Gerador.wGrupo('consigFGTS');
  Gerador.wCampo(tcStr, '', 'idConsig',  1,  1, 1, eSSimNaoToStr(obj.idConsig));
  Gerador.wCampo(tcStr, '', 'insConsig', 0,  5, 0, obj.insConsig);
  Gerador.wCampo(tcStr, '', 'nrContr',   0, 15, 0, obj.nrContr);
  Gerador.wGrupo('/consigFGTS');
end;

{ TInfoDeslig }

constructor TInfoDeslig.Create;
begin
  inherited;
    FSucessaoVinc := TSucessaoVinc.Create;
    FVerbasResc := nil;
    FQuarentena := TQuarentena.Create;
    FconsigFGTS := TconsigFGTS.Create;
    FInfoASO := TInfoASO.Create;
end;

destructor TInfoDeslig.destroy;
begin
  FSucessaoVinc.Free;
  FreeAndNil(FVerbasResc);
  FQuarentena.Free;
  FconsigFGTS.Free;
  FInfoASO.Free;
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
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);
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


end.
