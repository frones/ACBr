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

{$I ACBr.inc}

unit pcesS5003;

interface

uses
  SysUtils, Classes, pcnConversao, pcnLeitor, ACBrUtil, pcesCommon, pcesConversaoeSocial, Dialogs;

type
  TS5003 = class;
  TEvtBasesFGTS = class;

  TInfoFGTS = class;
  TIdeEstabLot2Collection = class;
  TIdeEstabLot2CollectionItem = class;
  TInfoTrabFGTSCollection = class;
  TInfoTrabFGTSCollectionItem = class;
  TInfoBaseFGTS = class;
  TBasePerApurCollection = class;
  TBasePerApurCollectionItem = class;
  TInfoDpsFGTS = class;
  TInfoTrabDpsCollection = class;
  TInfoTrabDpsCollectionItem = class;
  TDpsPerApurCollection = class;
  TDpsPerApurCollectionItem = class;

  TS5003 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtBasesFGTS: TEvtBasesFGTS;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtBasesFGTS(const Value: TEvtBasesFGTS);

  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtBasesFGTS: TEvtBasesFGTS read FEvtBasesFGTS write setEvtBasesFGTS;
  end;
  
  TDpsPerApurCollectionItem = class(TCollectionItem)
  private
    FtpDps: Integer;
    FdpsFGTS: Currency;
  public
    property tpDps: Integer read FtpDps write FtpDps;
    property dpsFGTS: Currency read FdpsFGTS write FdpsFGTS;
  end;

  TDpsPerApurCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDpsPerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
  public
    constructor Create(AOwner: TInfoTrabDpsCollectionItem);
    function Add: TDpsPerApurCollectionItem;
    property Items[Index: Integer]: TDpsPerApurCollectionItem read GetItem write SetItem;
  end;

  TInfoTrabDpsCollectionItem = class(TCollectionItem)
  private
    Fmatricula: String;
    FcodCateg: Integer;
    FDpsPerApur: TDpsPerApurCollection;

    procedure SetDpsPerApur(const Value: TDpsPerApurCollection);
    function GetDpsPerApur: TDpsPerApurCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property dpsPerApur: TDpsPerApurCollection read GetDpsPerApur write SetDpsPerApur;
  end;

  TInfoTrabDpsCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
  public
    constructor Create(AOwner: TInfoDpsFGTS);
    function Add: TInfoTrabDpsCollectionItem;
    property Items[Index: Integer]: TInfoTrabDpsCollectionItem read GetItem write SetItem;
  end;

  TInfoDpsFGTS = class(TPersistent)
  private
    FInfoTrabDps: TInfoTrabDpsCollection;

    procedure SetInfoTrabDps(const Value: TInfoTrabDpsCollection);
    function GetInfoTrabDps: TInfoTrabDpsCollection;
  public
    constructor Create(AOwner: TInfoFGTS);
    destructor Destroy; override;
    property infoTrabDps: TInfoTrabDpsCollection read GetInfoTrabDps write SetInfoTrabDps;
  end;  

  TBasePerApurCollectionItem = class(TCollectionItem)
  private
    FtpValor: Integer;
    FremFGTS: Currency;
  public
    property tpValor: Integer read FtpValor write FtpValor;
    property remFGTS: Currency read FremFGTS write FremFGTS;
  end;

  TBasePerApurCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TBasePerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
  public
    constructor Create(AOwner: TInfoBaseFGTS);
    function Add: TBasePerApurCollectionItem;
    property Items[Index: Integer]: TBasePerApurCollectionItem read GetItem write SetItem;
  end;

  TInfoBaseFGTS = class(TPersistent)
  private
    FBasePerApur: TBasePerApurCollection;
  public
    constructor Create(AOwner: TInfoTrabFGTSCollectionItem);
    destructor Destroy; override;
    property basePerApur: TBasePerApurCollection read FBasePerApur write FBasePerApur;
  end;

  TInfoTrabFGTSCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
  public
    constructor Create(AOwner: TIdeEstabLot2CollectionItem);
    function Add: TInfoTrabFGTSCollectionItem;
    property Items[Index: Integer]: TInfoTrabFGTSCollectionItem read GetItem write SetItem;
  end;

  TInfoTrabFGTSCollectionItem = class(TCollectionItem)
  private
    Fmatricula: String;
    FcodCateg: Integer;
    FdtAdm: TDateTime;
    FdtDeslig: TDateTime;
    FdtInicio: TDateTime;
    FmtvDeslig: String;
    FdtTerm: TDateTime;
    FmtvDesligTSV: String;
    FInfoBaseFGTS: TInfoBaseFGTS;

    procedure SetInfoBaseFGTS(const Value: TInfoBaseFGTS);
    function GetInfoBaseFGTS: TInfoBaseFGTS;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property dtDeslig: TDateTime read FdtDeslig write FdtDeslig;
    property dtInicio: TDateTime read FdtInicio write FdtInicio;
    property mtvDeslig: String read FmtvDeslig write FmtvDeslig;
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV: String read FmtvDesligTSV write FmtvDesligTSV;
    property InfoBaseFGTS: TInfoBaseFGTS read GetInfoBaseFGTS write SetInfoBaseFGTS;
  end;

  TIdeEstabLot2Collection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeEstabLot2CollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeEstabLot2CollectionItem);
  public
    constructor Create(AOwner: TInfoFGTS);
    function Add: TIdeEstabLot2CollectionItem;
    property Items[Index: Integer]: TIdeEstabLot2CollectionItem read GetItem write SetItem;
  end;

  TIdeEstabLot2CollectionItem = class(TCollectionItem)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: String;
    FcodLotacao: String;
    FInfoTrabFGTS: TInfoTrabFGTSCollection;

    procedure SetInfoTrabFGTS(const Value: TInfoTrabFGTSCollection);
    function GetInfoTrabFGTS: TInfoTrabFGTSCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
    property codLotacao: String read FcodLotacao write FcodLotacao;
    property InfoTrabFGTS: TInfoTrabFGTSCollection read GetInfoTrabFGTS write SetInfoTrabFGTS;
  end;

  TInfoFGTS = class(TPersistent)
  private
    FdtVenc: TDateTime;
    FIdeEstabLot: TIdeEstabLot2Collection;
    FinfoDpsFGTS: TInfoDpsFGTS;

    procedure SetIdeEstabLot(const Value: TIdeEstabLot2Collection);
    procedure SetInfoDpsFGTS(const Value: TInfoDpsFGTS);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property dtVenc: TDateTime read FdtVenc write FdtVenc;
    property IdeEstabLot: TIdeEstabLot2Collection read FIdeEstabLot write SetIdeEstabLot;
    property infoDpsFGTS: TInfoDpsFGTS read FinfoDpsFGTS write SetInfoDpsFGTS;
  end;

  TEvtBasesFGTS = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TideTrabalhador2;
    FinfoFGTS: TInfoFGTS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TideTrabalhador2 read FIdeTrabalhador write FIdeTrabalhador;
    property infoFGTS: TInfoFGTS read FinfoFGTS write FinfoFGTS;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

implementation

uses
  IniFiles;

{ TS5003 }

constructor TS5003.Create;
begin
  FTipoEvento := teS5003;
  FEvtBasesFGTS := TEvtBasesFGTS.Create;
end;

destructor TS5003.Destroy;
begin
  FEvtBasesFGTS.Free;

  inherited;
end;

function TS5003.GetEvento : TObject;
begin
  Result := self;
end;

function TS5003.GetXml : string;
begin
  Result := FEvtBasesFGTS.XML;
end;

procedure TS5003.SetXml(const Value: string);
begin
  if Value = FEvtBasesFGTS.XML then Exit;

  FEvtBasesFGTS.FXML := Value;
  FEvtBasesFGTS.Leitor.Arquivo := Value;
  FEvtBasesFGTS.LerXML;

end;

function TS5003.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TS5003.SetEvtBasesFGTS(const Value: TEvtBasesFGTS);
begin
  FEvtBasesFGTS.Assign(Value);
end;

{ TEvtBasesFGTS }

constructor TEvtBasesFGTS.Create();
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TideTrabalhador2.Create;
  FinfoFGTS := TInfoFGTS.Create;
end;

destructor TEvtBasesFGTS.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FinfoFGTS.Free;

  inherited;
end;

function TEvtBasesFGTS.LerXML: boolean;
var
  ok: Boolean;
  i, j, k: Integer;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtBasesFGTS') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
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
        IdeTrabalhador.nisTrab := leitor.rCampo(tcStr, 'nisTrab');
      end;

      if leitor.rExtrai(2, 'infoFGTS') <> '' then
      begin
        infoFGTS.dtVenc := leitor.rCampo(tcDat, 'dtVenc');

        i := 0;
        while Leitor.rExtrai(3, 'ideEstabLot', '', i + 1) <> '' do
        begin
          infoFGTS.IdeEstabLot.Add;
          infoFGTS.IdeEstabLot.Items[i].TpInsc     := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
          infoFGTS.IdeEstabLot.Items[i].NrInsc     := leitor.rCampo(tcStr, 'nrInsc');
          infoFGTS.IdeEstabLot.Items[i].codLotacao := leitor.rCampo(tcStr, 'codLotacao');

          j := 0;
          while Leitor.rExtrai(4, 'infoTrabFGTS', '', j + 1) <> '' do
          begin
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Add;
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].matricula    := leitor.rCampo(tcStr, 'matricula');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].codCateg     := leitor.rCampo(tcInt, 'codCateg');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].dtAdm        := leitor.rCampo(tcDat, 'dtAdm');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].dtDeslig     := leitor.rCampo(tcDat, 'dtDeslig');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].dtInicio     := leitor.rCampo(tcDat, 'dtInicio');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].mtvDeslig    := leitor.rCampo(tcStr, 'mtvDeslig');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].dtTerm       := leitor.rCampo(tcDat, 'dtTerm');
            infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].mtvDesligTSV := leitor.rCampo(tcStr, 'mtvDesligTSV');

            k := 0;
            while Leitor.rExtrai(5, 'basePerApur', '', k + 1) <> '' do
            begin
              infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].InfoBaseFGTS.basePerApur.Add;
              infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].InfoBaseFGTS.basePerApur.Items[k].tpValor := leitor.rCampo(tcInt, 'tpValor');
              infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].InfoBaseFGTS.basePerApur.Items[k].remFGTS := leitor.rCampo(tcDe2, 'remFGTS');

              inc(k);
            end;

            inc(j);
          end;

          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'infoDpsFGTS', '', i + 1) <> '' do
        begin
          j := 0;
          while Leitor.rExtrai(4, 'infoTrabDps', '', j + 1) <> '' do
          begin
            infoFGTS.infoDpsFGTS.infoTrabDps.Add;
            infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].matricula := leitor.rCampo(tcStr, 'matricula');
            infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].codCateg  := leitor.rCampo(tcInt, 'codCateg');

            k := 0;
            while Leitor.rExtrai(5, 'dpsPerApur', '', k + 1) <> '' do
            begin
              infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.Add;
              infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.Items[k].tpDps   := leitor.rCampo(tcInt, 'tpDps');
              infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.Items[k].dpsFGTS := leitor.rCampo(tcDe2, 'dpsFGTS');

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
    on e: Exception do  begin
      ShowMessage(e.Message);
      Result := False;
    end;
  end;
end;

function TEvtBasesFGTS.SalvarINI: boolean;
var
  AIni: TMemIniFile;
//  sSecao: String;
//  i, j, k: Integer;
begin
  Result := False;

  AIni := TMemIniFile.Create('');
  try
    Result := True;

    with Self do
    begin



    end;
  finally
    AIni.Free;
  end;
end;

{ TideEstabLotCollection }

function TIdeEstabLot2Collection.Add: TIdeEstabLot2CollectionItem;
begin
  Result := TIdeEstabLot2CollectionItem(inherited Add);
end;

constructor TIdeEstabLot2Collection.Create(AOwner: TInfoFGTS);
begin
  inherited create(TIdeEstabLot2CollectionItem);
end;

function TIdeEstabLot2Collection.GetItem(Index: Integer): TIdeEstabLot2CollectionItem;
begin
  Result := TIdeEstabLot2CollectionItem(inherited GetItem(Index));
end;

procedure TIdeEstabLot2Collection.SetItem(Index: Integer; Value: TIdeEstabLot2CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoFGTS }

constructor TInfoFGTS.Create;
begin
  FIdeEstabLot := TIdeEstabLot2Collection.Create(Self);
  FInfoDpsFGTS := TInfoDpsFGTS.Create(Self);
end;

destructor TInfoFGTS.Destroy;
begin
  FIdeEstabLot.Free;
  FinfoDpsFGTS.Free;
  inherited;
end;

procedure TInfoFGTS.SetIdeEstabLot(const Value: TIdeEstabLot2Collection);
begin
  FIdeEstabLot := Value;
end;

procedure TInfoFGTS.SetInfoDpsFGTS(const Value: TInfoDpsFGTS);
begin
  FInfoDpsFGTS := Value;
end;

{ TIdeEstabLotCollectionItem }

constructor TIdeEstabLot2CollectionItem.Create;
begin
  FInfoTrabFGTS := TInfoTrabFGTSCollection.Create(Self);
end;

destructor TIdeEstabLot2CollectionItem.Destroy;
begin
  FInfoTrabFGTS.Free;
  inherited;
end;

function TIdeEstabLot2CollectionItem.GetInfoTrabFGTS: TInfoTrabFGTSCollection;
begin
  if not(Assigned(FInfoTrabFGTS)) then
     FInfoTrabFGTS := TInfoTrabFGTSCollection.Create(Self);
  Result := FInfoTrabFGTS;   
end;

procedure TIdeEstabLot2CollectionItem.SetInfoTrabFGTS(const Value: TInfoTrabFGTSCollection);
begin
  FInfoTrabFGTS := Value;
end;

{ TInfoTrabFGTSCollection }

function TInfoTrabFGTSCollection.Add: TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem(inherited Add);
//  Result.Create(self);
end;

constructor TInfoTrabFGTSCollection.Create(AOwner: TIdeEstabLot2CollectionItem);
begin
  inherited create(TInfoTrabFGTSCollectionItem);
end;

function TInfoTrabFGTSCollection.GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTrabFGTSCollection.SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
begin
  inherited SetItem(Index, Value);
end;
{ TInfoBaseFGTS }

constructor TInfoBaseFGTS.Create(AOwner: TInfoTrabFGTSCollectionItem);
begin
  FBasePerApur := TBasePerApurCollection.Create(Self);
end;

destructor TInfoBaseFGTS.Destroy;
begin
  FBasePerApur.Free;
  inherited;
end;

{ TInfoTrabFGTSCollectionItem }

constructor TInfoTrabFGTSCollectionItem.Create;
begin
  FInfoBaseFGTS := TInfoBaseFGTS.Create(Self);
end;

destructor TInfoTrabFGTSCollectionItem.Destroy;
begin
  FInfoBaseFGTS.Free;
  inherited;
end;

function TInfoTrabFGTSCollectionItem.GetInfoBaseFGTS: TInfoBaseFGTS;
begin
  if not(Assigned(FInfoBaseFGTS)) then
     FInfoBaseFGTS := TInfoBaseFGTS.Create(Self);
  Result := FInfoBaseFGTS;
end;

procedure TInfoTrabFGTSCollectionItem.SetInfoBaseFGTS(const Value: TInfoBaseFGTS);
begin
  FInfoBaseFGTS := Value;
end;

{ TBasePerApurCollection }

function TBasePerApurCollection.Add: TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem(inherited Add);
  Result.Create(self);
end;

constructor TBasePerApurCollection.Create(AOwner: TInfoBaseFGTS);
begin
  inherited create(TBasePerApurCollectionItem);
end;

function TBasePerApurCollection.GetItem(Index: Integer): TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem(inherited GetItem(Index));
end;

procedure TBasePerApurCollection.SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoDpsFGTS }

constructor TInfoDpsFGTS.Create;
begin
  FInfoTrabDps := TInfoTrabDpsCollection.Create(Self);
end;

destructor TInfoDpsFGTS.Destroy;
begin
  FInfoTrabDps.Free;
  inherited;
end;

function TInfoDpsFGTS.GetInfoTrabDps: TInfoTrabDpsCollection;
begin             
  if not(Assigned(FInfoTrabDps)) then
     FInfoTrabDps := TInfoTrabDpsCollection.Create(Self);
  Result := FInfoTrabDps;
end;

procedure TInfoDpsFGTS.SetInfoTrabDps(const Value: TInfoTrabDpsCollection);
begin
  FInfoTrabDps := Value;
end;

{ TInfoTrabDpsCollection }

function TInfoTrabDpsCollection.Add: TInfoTrabDpsCollectionItem;
begin
  Result := TInfoTrabDpsCollectionItem(inherited Add);
end;

constructor TInfoTrabDpsCollection.Create(AOwner: TInfoDpsFGTS);
begin
  inherited create(TInfoTrabDpsCollectionItem);
end;

function TInfoTrabDpsCollection.GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
begin
  Result := TInfoTrabDpsCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTrabDpsCollection.SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoTrabDpsCollectionItem }

constructor TInfoTrabDpsCollectionItem.Create;
begin            
  FDpsPerApur := TDpsPerApurCollection.Create(Self);
end;

destructor TInfoTrabDpsCollectionItem.Destroy;
begin
  FDpsPerApur.Free;
  inherited;
end;

function TInfoTrabDpsCollectionItem.GetDpsPerApur: TDpsPerApurCollection;
begin
  if not(Assigned(FDpsPerApur)) then
     FDpsPerApur := TDpsPerApurCollection.Create(Self);
  Result := FDpsPerApur;
end;

procedure TInfoTrabDpsCollectionItem.SetDpsPerApur(const Value: TDpsPerApurCollection);
begin
  FDpsPerApur := Value;
end;

{ TDpsPerApurCollection }

function TDpsPerApurCollection.Add: TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited Add);
  Result.Create(Self);
end;

constructor TDpsPerApurCollection.Create(AOwner: TInfoTrabDpsCollectionItem);
begin
  inherited create(TDpsPerApurCollectionItem);
end;

function TDpsPerApurCollection.GetItem(Index: Integer): TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited GetItem(Index));
end;

procedure TDpsPerApurCollection.SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
