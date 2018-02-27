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

unit pcesS5001;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TS5001 = class;
  TInfoCpCalcCollection = class;
  TInfoCpCalcCollectionItem = class;
  TInfoCp = class;
  TIdeEstabLotCollection = class;
  TIdeEstabLotCollectionItem = class;
  TInfoCategIncidCollection = class;
  TInfoCategIncidCollectionItem = class;
  TInfoBaseCSCollection = class;
  TInfoBaseCSCollectionItem = class;
  TCalcTercCollection = class;
  TCalcTercCollectionItem = class;
  TEvtBasesTrab = class;

  TS5001 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtBasesTrab: TEvtBasesTrab;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtBasesTrab(const Value: TEvtBasesTrab);

  public
    constructor Create;
    destructor Destroy; override;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtBasesTrab: TEvtBasesTrab read FEvtBasesTrab write setEvtBasesTrab;

  end;

  TInfoCpCalcCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoCpCalcCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCpCalcCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TInfoCpCalcCollectionItem;
    property Items[Index: Integer]: TInfoCpCalcCollectionItem read GetItem write SetItem;
  end;

  TInfoCpCalcCollectionItem = class(TCollectionItem)
  private
    FtpCR: string;
    FvrCpSeg: Double;
    FvrDescSeg: Double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrCpSeg: Double read FvrCpSeg write FvrCpSeg;
    property vrDescSeg: Double read FvrDescSeg write FvrDescSeg;
  end;

  TInfoCp = class(TPersistent)
  private
    FIdeEstabLot: TIdeEstabLotCollection;
  public
    constructor Create(AOwner: TEvtBasesTrab);
    destructor Destroy; override;

    property IdeEstabLot: TIdeEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TIdeEstabLotCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeEstabLotCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeEstabLotCollectionItem);
  public
    constructor Create(AOwner: TInfoCp);
    function Add: TIdeEstabLotCollectionItem;
    property Items[Index: Integer]: TIdeEstabLotCollectionItem read GetItem write SetItem;
  end;

  TIdeEstabLotCollectionItem = class(TCollectionItem)
  private
    FNrInsc: string;
    FCodLotacao: string;
    FTpInsc: TpTpInsc;
    FInfoCategIncid: TInfoCategIncidCollection;
    procedure SetInfoCategIncid(const Value: TInfoCategIncidCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property tpInsc: TpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codLotacao: string read FCodLotacao write FCodLotacao;
    property InfoCategIncid: TInfoCategIncidCollection read FInfoCategIncid write SetInfoCategIncid;
  end;

  TInfoCategIncidCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoCategIncidCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCategIncidCollectionItem);
  public
    constructor Create(AOwner: TIdeEstabLotCollectionItem);
    function Add: TInfoCategIncidCollectionItem;
    property Items[Index: Integer]: TInfoCategIncidCollectionItem read GetItem write SetItem;
  end;

  TInfoCategIncidCollectionItem = class(TCollectionItem)
  private
    FMatricula: string;
    FcodCateg: Integer;
    FindSimples: tpIndSimples;
    FInfoBaseCS: TInfoBaseCSCollection;
    FCalcTerc: TCalcTercCollection;
    procedure SetInfoBaseCS(const Value: TInfoBaseCSCollection);
    procedure SetCalcTerc(const Value: TCalcTercCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property matricula: string read FMatricula write FMatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property indSimples: tpIndSimples read FindSimples write FindSimples;
    property InfoBaseCS: TInfoBaseCSCollection read FInfoBaseCS write SetInfoBaseCS;
    property CalcTerc: TCalcTercCollection read FCalcTerc write SetCalcTerc;
  end;

  TInfoBaseCSCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoBaseCSCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoBaseCSCollectionItem);
  public
    constructor Create(AOwner: TInfoCategIncidCollectionItem);
    function Add: TInfoBaseCSCollectionItem;
    property Items[Index: Integer]: TInfoBaseCSCollectionItem read GetItem write SetItem;
  end;

  TInfoBaseCSCollectionItem = class(TCollectionItem)
  private
    Find13: Integer;
    FtpValor: Integer;
    Fvalor: Double;
  public
    property ind13: Integer read Find13 write Find13;
    property tpValor: Integer read FtpValor write FtpValor;
    property valor: Double read Fvalor write Fvalor;
  end;

  TCalcTercCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCalcTercCollectionItem;
    procedure SetItem(Index: Integer; Value: TCalcTercCollectionItem);
  public
    constructor Create(AOwner: TInfoCategIncidCollectionItem);
    function Add: TCalcTercCollectionItem;
    property Items[Index: Integer]: TCalcTercCollectionItem read GetItem write SetItem;
  end;

  TCalcTercCollectionItem = class(TCollectionItem)
  private
    FtpCR: Integer;
    FvrCsSegTerc: Double;
    FvrDescTerc: Double;
  public
    property tpCR: Integer read FtpCR write FtpCR;
    property vrCsSegTerc: Double read FvrCsSegTerc write FvrCsSegTerc;
    property vrDescTerc: Double read FvrDescTerc write FvrDescTerc;
  end;

  TEvtBasesTrab = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TIdeTrabalhador3;
    FInfoCpCalc: TInfoCpCalcCollection;
    FInfoCp: TInfoCp;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TIdeTrabalhador3 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoCpCalc: TInfoCpCalcCollection read FInfoCpCalc write FInfoCpCalc;
    property InfoCp: TInfoCp read FInfoCp write FInfoCp;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

implementation

{ TS5001 }

constructor TS5001.Create;
begin
  FTipoEvento := teS5001;
  FEvtBasesTrab := TEvtBasesTrab.Create;
end;

destructor TS5001.Destroy;
begin
  FEvtBasesTrab.Free;

  inherited;
end;

function TS5001.GetXml : string;
begin
  Result := FEvtBasesTrab.XML;
end;

procedure TS5001.SetXml(const Value: string);
begin
  if Value = FEvtBasesTrab.XML then Exit;

  FEvtBasesTrab.XML := Value;
  FEvtBasesTrab.Leitor.Arquivo := Value;
  FEvtBasesTrab.LerXML;

end;

function TS5001.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TS5001.SetEvtBasesTrab(const Value: TEvtBasesTrab);
begin
  FEvtBasesTrab.Assign(Value);
end;

{ TEvtBasesTrab }

constructor TEvtBasesTrab.Create();
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TIdeTrabalhador3.Create;
  FInfoCpCalc := TInfoCpCalcCollection.Create;
  FInfoCp := TInfoCp.Create(Self);
end;

destructor TEvtBasesTrab.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FInfoCpCalc.Free;
  FInfoCp.Free;

  inherited;
end;

{ TInfoCpCalcCollection }

function TInfoCpCalcCollection.Add: TInfoCpCalcCollectionItem;
begin
  Result := TInfoCpCalcCollectionItem(inherited Add);
end;

constructor TInfoCpCalcCollection.Create;
begin
  inherited create(TInfoCpCalcCollectionItem);
end;

function TInfoCpCalcCollection.GetItem(
  Index: Integer): TInfoCpCalcCollectionItem;
begin
  Result := TInfoCpCalcCollectionItem(inherited GetItem(Index));
end;

procedure TInfoCpCalcCollection.SetItem(Index: Integer;
  Value: TInfoCpCalcCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoCp }

constructor TInfoCp.Create;
begin
  FIdeEstabLot := TIdeEstabLotCollection.Create(Self);
end;

destructor TInfoCp.Destroy;
begin
  FIdeEstabLot.Free;

  inherited;
end;

{ TIdeEstabLotCollection }

function TIdeEstabLotCollection.Add: TIdeEstabLotCollectionItem;
begin
  Result := TIdeEstabLotCollectionItem(inherited Add);
  Result.create;
end;

constructor TIdeEstabLotCollection.Create(AOwner: TInfoCp);
begin
  inherited create(TIdeEstabLotCollectionItem);
end;

function TIdeEstabLotCollection.GetItem(
  Index: Integer): TIdeEstabLotCollectionItem;
begin
  Result := TIdeEstabLotCollectionItem(inherited GetItem(Index));
end;

procedure TIdeEstabLotCollection.SetItem(Index: Integer;
  Value: TIdeEstabLotCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdeEstabLotCollectionItem }

constructor TIdeEstabLotCollectionItem.Create;
begin
  FInfoCategIncid := TInfoCategIncidCollection.Create(Self);
end;

destructor TIdeEstabLotCollectionItem.Destroy;
begin
  FInfoCategIncid.Free;

  inherited;
end;

procedure TIdeEstabLotCollectionItem.SetInfoCategIncid(
  const Value: TInfoCategIncidCollection);
begin
  FInfoCategIncid := Value;
end;

{ TInfoCategIncidCollection }

function TInfoCategIncidCollection.Add: TInfoCategIncidCollectionItem;
begin
  Result := TInfoCategIncidCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfoCategIncidCollection.Create;
begin
  inherited create(TInfoCategIncidCollectionItem);
end;

function TInfoCategIncidCollection.GetItem(
  Index: Integer): TInfoCategIncidCollectionItem;
begin
  Result := TInfoCategIncidCollectionItem(inherited GetItem(Index));
end;

procedure TInfoCategIncidCollection.SetItem(Index: Integer;
  Value: TInfoCategIncidCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoCategIncidCollectionItem }

constructor TInfoCategIncidCollectionItem.Create;
begin
  FInfoBaseCS := TInfoBaseCSCollection.Create(Self);
  FCalcTerc := TCalcTercCollection.Create(Self);
end;

destructor TInfoCategIncidCollectionItem.Destroy;
begin
  FInfoBaseCS.Free;
  FCalcTerc.Free;

  inherited;
end;

procedure TInfoCategIncidCollectionItem.SetCalcTerc(
  const Value: TCalcTercCollection);
begin
  FCalcTerc := Value;
end;

procedure TInfoCategIncidCollectionItem.SetInfoBaseCS(
  const Value: TInfoBaseCSCollection);
begin
  FInfoBaseCS := Value;
end;

{ TInfoBaseCSCollection }

function TInfoBaseCSCollection.Add: TInfoBaseCSCollectionItem;
begin
  Result := TInfoBaseCSCollectionItem(inherited Add);
end;

constructor TInfoBaseCSCollection.Create(
  AOwner: TInfoCategIncidCollectionItem);
begin
  inherited create(TInfoBaseCSCollectionItem);
end;

function TInfoBaseCSCollection.GetItem(
  Index: Integer): TInfoBaseCSCollectionItem;
begin
  Result := TInfoBaseCSCollectionItem(inherited GetItem(Index));
end;

procedure TInfoBaseCSCollection.SetItem(Index: Integer;
  Value: TInfoBaseCSCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCalcTercCollection }

function TCalcTercCollection.Add: TCalcTercCollectionItem;
begin
  Result := TCalcTercCollectionItem(inherited Add);
end;

constructor TCalcTercCollection.Create(
  AOwner: TInfoCategIncidCollectionItem);
begin
  inherited create(TCalcTercCollectionItem);
end;

function TCalcTercCollection.GetItem(
  Index: Integer): TCalcTercCollectionItem;
begin
  Result := TCalcTercCollectionItem(inherited GetItem(Index));
end;

procedure TCalcTercCollection.SetItem(Index: Integer;
  Value: TCalcTercCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TEvtBasesTrab.LerXML: boolean;
var
  ok: Boolean;
  i, j, k: Integer;
begin
  Result := False;
  try
    XML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtBasesTrab') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        IdeEvento.IndApuracao  := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));
        IdeEvento.perApur      := leitor.rCampo(tcStr, 'perApur');
      end;

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideTrabalhador') <> '' then
      begin
        IdeTrabalhador.cpfTrab := leitor.rCampo(tcStr, 'cpfTrab');

        i := 0;
        while Leitor.rExtrai(3, 'procJudTrab', '', i + 1) <> '' do
        begin
          IdeTrabalhador.procJudTrab.Add;
          IdeTrabalhador.procJudTrab.Items[i].nrProcJud := leitor.rCampo(tcStr, 'nrProcJud');
          IdeTrabalhador.procJudTrab.Items[i].codSusp   := leitor.rCampo(tcStr, 'codSusp');
          inc(i);
        end;
      end;

      i := 0;
      while Leitor.rExtrai(2, 'infoCpCalc', '', i + 1) <> '' do
      begin
        infoCpCalc.Add;
        infoCpCalc.Items[i].tpCR      := leitor.rCampo(tcStr, 'tpCR');
        infoCpCalc.Items[i].vrCpSeg   := leitor.rCampo(tcDe2, 'vrCpSeg');
        infoCpCalc.Items[i].vrDescSeg := leitor.rCampo(tcDe2, 'vrDescSeg');
        inc(i);
      end;

      if leitor.rExtrai(2, 'infoCp') <> '' then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'ideEstabLot', '', i + 1) <> '' do
        begin
          infoCp.IdeEstabLot.Add;
          infoCp.IdeEstabLot.Items[i].tpInsc     := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
          infoCp.IdeEstabLot.Items[i].nrInsc     := leitor.rCampo(tcStr, 'nrInsc');
          infoCp.IdeEstabLot.Items[i].codLotacao := leitor.rCampo(tcStr, 'codLotacao');

          j := 0;
          while Leitor.rExtrai(4, 'infoCategIncid', '', j + 1) <> '' do
          begin
            infoCp.IdeEstabLot.Items[i].InfoCategIncid.Add;
            infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].matricula  := leitor.rCampo(tcStr, 'matricula');
            infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].codCateg   := leitor.rCampo(tcInt, 'codCateg');
            infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].indSimples := eSStrToIndSimples(ok, leitor.rCampo(tcStr, 'indSimples'));

            k := 0;
            while Leitor.rExtrai(5, 'infoBaseCS', '', k + 1) <> '' do
            begin
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].InfoBaseCS.Add;
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].InfoBaseCS.Items[k].ind13   := leitor.rCampo(tcInt, 'ind13');
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].InfoBaseCS.Items[k].tpValor := leitor.rCampo(tcInt, 'tpValor');
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].InfoBaseCS.Items[k].valor   := leitor.rCampo(tcDe2, 'valor');
              inc(k);
            end;

            k := 0;
            while Leitor.rExtrai(5, 'calcTer', '', k + 1) <> '' do
            begin
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].CalcTerc.Add;
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].CalcTerc.Items[k].tpCR        := leitor.rCampo(tcInt, 'tpCR');
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].CalcTerc.Items[k].vrCsSegTerc := leitor.rCampo(tcDe2, 'vrCsSegTerc');
              infoCp.IdeEstabLot.Items[i].InfoCategIncid.Items[j].CalcTerc.Items[k].vrDescTerc  := leitor.rCampo(tcDe2, 'vrDescTerc');
              inc(k);
            end;

            inc(j);
          end;

          inc(i);
        end;

      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
