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

unit eSocial_S2399;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS2399Collection = class;
  TS2399CollectionItem = class;
  TEvtTSVTermino = class;
  TInfoTSVTermino = class;
  TVerbasRescS2399 = class;
  TDmDevCollectionItem = class;
  TDmDevCollection = class;


  TS2399Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2399CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2399CollectionItem);
  public
    function Add: TS2399CollectionItem;
    property Items[Index: Integer]: TS2399CollectionItem read GetItem write SetItem; default;
  end;

  TS2399CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVTermino : TEvtTSVTermino;
    procedure setEvtTSVTermino(const Value: TEvtTSVTermino);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVTermino: TEvtTSVTermino read FEvtTSVTermino write setEvtTSVTermino;
  end;

  TEvtTSVTermino = class(TeSocialEvento)
    private
      FIdeEvento: TIdeEvento2;
      FIdeEmpregador: TIdeEmpregador;
      FIdeTrabSemVInc : TideTrabSemVinc;
      FInfoTSVTermino: TInfoTSVTermino;
      procedure GerarInfoTSVTermino(obj: TInfoTSVTermino);
      procedure GerarVerbasResc(obj: TVerbasRescS2399);
      procedure GerarIdeTrabSemVinc(obj: TIdeTrabSemVinc);
      procedure GerarDmDev(pDmDev: TDmDevCollection);
     public
      constructor Create(AACBreSocial: TObject);overload;
      destructor Destroy; override;

      function GerarXML: boolean; override;

      property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
      property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
      property IdeTrabSemVInc: TideTrabSemVinc read FIdeTrabSemVInc write FIdeTrabSemVInc;
      property InfoTSVTermino: TInfoTSVTermino read FInfoTSVTermino write FInfoTSVTermino;
  end;

  TinfoTSVTermino = class(TPersistent)
    private
      FdtTerm : TDateTime;
      FmtvDesligTSV : string;
      FverbasResc : TVerbasRescS2399;
      Fquarentena : TQuarentena;
    public
      constructor Create;
      destructor  Destroy; override;

      property dtTerm : TDateTime read FdtTerm write FdtTerm;
      property mtvDesligTSV : string read FmtvDesligTSV write FmtvDesligTSV;
      property verbasResc : TVerbasRescS2399 read FverbasResc write FverbasResc;
      property quarentena : TQuarentena read Fquarentena write Fquarentena;
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

  TDmDevCollectionItem = class(TCollectionItem)
  private
    FIdeDmDev: string;
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create; reintroduce;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TVerbasRescS2399 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollection;
  public
    constructor Create; reintroduce;

    property dmDev: TDmDevCollection read FDmDev write FDmDev;
  end;

implementation

uses
  eSocial_NaoPeriodicos;

{ TS2399Collection }

function TS2399Collection.Add: TS2399CollectionItem;
begin
  Result := TS2399CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2399Collection.GetItem(Index: Integer): TS2399CollectionItem;
begin
  Result := TS2399CollectionItem(inherited GetItem(Index));
end;

procedure TS2399Collection.SetItem(Index: Integer; Value: TS2399CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2399CollectionItem }

constructor TS2399CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2399;
  FEvtTSVTermino := TEvtTSVTermino.Create(AOwner);
end;

destructor TS2399CollectionItem.Destroy;
begin
  FEvtTSVTermino.Free;
  inherited;
end;

procedure TS2399CollectionItem.setEvtTSVTermino(const Value: TEvtTSVTermino);
begin
  FEvtTSVTermino.Assign(Value);
end;

{ TEvtTSVTermino }

constructor TEvtTSVTermino.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabSemVInc := TideTrabSemVinc.Create;
  FInfoTSVTermino := TInfoTSVTermino.Create;
end;

destructor TEvtTSVTermino.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabSemVInc.Free;
  FInfoTSVTermino.Free;
  inherited;
end;

procedure TEvtTSVTermino.GerarIdeTrabSemVinc(obj: TIdeTrabSemVinc);
begin
  Gerador.wGrupo('ideTrabSemVinculo');
    Gerador.wCampo(tcStr, '', 'cpfTrab', 0,0,0, obj.cpfTrab);
    Gerador.wCampo(tcStr, '', 'nisTrab', 0,0,0, obj.nisTrab);
    Gerador.wCampo(tcStr, '', 'codCateg', 0,0,0, obj.codCateg);
  Gerador.wGrupo('/ideTrabSemVinculo');
end;

procedure TEvtTSVTermino.GerarInfoTSVTermino(obj: TInfoTSVTermino);
begin
  Gerador.wGrupo('infoTSVTermino');
    Gerador.wCampo(tcDat, '', 'dtTerm', 0,0,0, obj.dtTerm);
    Gerador.wCampo(tcStr, '', 'mtvDesligTSV', 0,0,0, obj.mtvDesligTSV);
    GerarVerbasResc(obj.verbasResc);
    GerarQuarentena(obj.quarentena);
  Gerador.wGrupo('/infoTSVTermino');
end;

procedure TEvtTSVTermino.GerarDmDev(pDmDev: TDmDevCollection);
var
  i: integer;
begin
  for i := 0 to pDmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');
      Gerador.wCampo(tcStr, '', 'ideDmDev', 0,0,0, pDmDev[i].ideDmDev);
      GerarIdeEstabLot(pDmDev[i].ideEstabLot);
    Gerador.wGrupo('/dmDev');
  end;
end;

procedure TEvtTSVTermino.GerarVerbasResc(obj: TVerbasRescS2399);
begin
  if (obj.dmDev.Count > 0) or (obj.ProcJudTrab.Count > 0) then
  begin
    Gerador.wGrupo('verbasResc');
      GerarDmDev(obj.dmDev);
      GerarProcJudTrab(obj.ProcJudTrab);
      if obj.infoMVInst then
        GerarInfoMV(obj.infoMV);
    Gerador.wGrupo('/verbasResc');
  end;
end;

function TEvtTSVTermino.GerarXML: boolean;
begin
  try
  GerarCabecalho('evtTSVTermino');
    Gerador.wGrupo('evtTSVTermino Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');//versao="'+Self.versao+'"
      //gerarIdVersao(self);
      gerarIdeEvento2(self.IdeEvento);
      gerarIdeEmpregador(self.IdeEmpregador);
      GerarIdeTrabSemVinc(self.IdeTrabSemVInc);
      GerarInfoTSVTermino(Self.InfoTSVTermino);
    Gerador.wGrupo('/evtTSVTermino');
  GerarRodape;
  XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVTermino');
  Validar('evtTSVTermino');
except on e:exception do
  raise Exception.Create(e.Message);
end;
Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TinfoTSVTermino }

constructor TinfoTSVTermino.Create;
begin
  inherited;
  FverbasResc := TVerbasRescS2399.Create;
  Fquarentena := TQuarentena.Create;
end;

destructor TinfoTSVTermino.Destroy;
begin
  FverbasResc.Free;
  Fquarentena.Free;
  inherited;
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
  FIdeEstabLot := TideEstabLotCollection.Create;
end;

{ TVerbasRescS2399 }

constructor TVerbasRescS2399.Create;
begin
  inherited;
  FDmDev := TDmDevCollection.Create;
end;


end.
