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
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnLeitor, ACBrUtil, pcesCommon, pcesConversaoeSocial, Dialogs;

type
  TEvtBasesFGTS = class;
  TIdeEstabLot2CollectionItem = class;
  TInfoTrabFGTSCollectionItem = class;
  TInfoBaseFGTS = class;
  TBasePerApurCollectionItem = class;
  TInfoTrabDpsCollectionItem = class;
  TDpsPerApurCollectionItem = class;

  TS5003 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtBasesFGTS: TEvtBasesFGTS;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtBasesFGTS: TEvtBasesFGTS read FEvtBasesFGTS write FEvtBasesFGTS;
  end;
  
  TDpsPerApurCollectionItem = class(TObject)
  private
    FtpDps: Integer;
    FdpsFGTS: Currency;
  public
    property tpDps: Integer read FtpDps write FtpDps;
    property dpsFGTS: Currency read FdpsFGTS write FdpsFGTS;
  end;

  TDpsPerApurCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TDpsPerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
  public
    function Add: TDpsPerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDpsPerApurCollectionItem;
    property Items[Index: Integer]: TDpsPerApurCollectionItem read GetItem write SetItem;
  end;

  TInfoTrabDpsCollectionItem = class(TObject)
  private
    Fmatricula: String;
    FcodCateg: Integer;
    FDpsPerApur: TDpsPerApurCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property dpsPerApur: TDpsPerApurCollection read FDpsPerApur write FDpsPerApur;
  end;

  TInfoTrabDpsCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
  public
    function Add: TInfoTrabDpsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoTrabDpsCollectionItem;
    property Items[Index: Integer]: TInfoTrabDpsCollectionItem read GetItem write SetItem;
  end;

  TInfoDpsFGTS = class(TObject)
  private
    FInfoTrabDps: TInfoTrabDpsCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property infoTrabDps: TInfoTrabDpsCollection read FInfoTrabDps write FInfoTrabDps;
  end;  

  TBasePerApurCollectionItem = class(TObject)
  private
    FtpValor: Integer;
    FremFGTS: Currency;
  public
    property tpValor: Integer read FtpValor write FtpValor;
    property remFGTS: Currency read FremFGTS write FremFGTS;
  end;

  TBasePerApurCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TBasePerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
  public
    function Add: TBasePerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TBasePerApurCollectionItem;
    property Items[Index: Integer]: TBasePerApurCollectionItem read GetItem write SetItem;
  end;

  TInfoBaseFGTS = class(TObject)
  private
    FBasePerApur: TBasePerApurCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property basePerApur: TBasePerApurCollection read FBasePerApur write FBasePerApur;
  end;

  TInfoTrabFGTSCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
  public
    function Add: TInfoTrabFGTSCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoTrabFGTSCollectionItem;
    property Items[Index: Integer]: TInfoTrabFGTSCollectionItem read GetItem write SetItem;
  end;

  TInfoTrabFGTSCollectionItem = class(TObject)
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
  public
    constructor Create;
    destructor Destroy; override;

    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property dtDeslig: TDateTime read FdtDeslig write FdtDeslig;
    property dtInicio: TDateTime read FdtInicio write FdtInicio;
    property mtvDeslig: String read FmtvDeslig write FmtvDeslig;
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV: String read FmtvDesligTSV write FmtvDesligTSV;
    property InfoBaseFGTS: TInfoBaseFGTS read FInfoBaseFGTS write FInfoBaseFGTS;
  end;

  TIdeEstabLot2Collection = class(TObjectList)
  private
    function GetItem(Index: Integer): TIdeEstabLot2CollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeEstabLot2CollectionItem);
  public
    function Add: TIdeEstabLot2CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabLot2CollectionItem;
    property Items[Index: Integer]: TIdeEstabLot2CollectionItem read GetItem write SetItem;
  end;

  TIdeEstabLot2CollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: String;
    FcodLotacao: String;
    FInfoTrabFGTS: TInfoTrabFGTSCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
    property codLotacao: String read FcodLotacao write FcodLotacao;
    property InfoTrabFGTS: TInfoTrabFGTSCollection read FInfoTrabFGTS write FInfoTrabFGTS;
  end;

  TInfoFGTS = class(TObject)
  private
    FdtVenc: TDateTime;
    FIdeEstabLot: TIdeEstabLot2Collection;
    FinfoDpsFGTS: TInfoDpsFGTS;

    procedure SetIdeEstabLot(const Value: TIdeEstabLot2Collection);
    procedure SetInfoDpsFGTS(const Value: TInfoDpsFGTS);
  public
    constructor Create;
    destructor Destroy; override;
    property dtVenc: TDateTime read FdtVenc write FdtVenc;
    property IdeEstabLot: TIdeEstabLot2Collection read FIdeEstabLot write SetIdeEstabLot;
    property infoDpsFGTS: TInfoDpsFGTS read FinfoDpsFGTS write SetInfoDpsFGTS;
  end;

  TEvtBasesFGTS = class(TObject)
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

{ TEvtBasesFGTS }

constructor TEvtBasesFGTS.Create();
begin
  inherited Create;
  FLeitor         := TLeitor.Create;
  FIdeEvento      := TIdeEvento5.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTrabalhador := TideTrabalhador2.Create;
  FinfoFGTS       := TInfoFGTS.Create;
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
//var
//  AIni: TMemIniFile;
//  sSecao: String;
//  i, j, k: Integer;
begin
  Result := True;

//  AIni := TMemIniFile.Create('');
//  try
//    with Self do
//    begin
//
//
//
//    end;
//  finally
//    AIni.Free;
//  end;
end;

{ TideEstabLotCollection }

function TIdeEstabLot2Collection.Add: TIdeEstabLot2CollectionItem;
begin
  Result := Self.New;
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
  inherited Create;
  FIdeEstabLot := TIdeEstabLot2Collection.Create;
  FInfoDpsFGTS := TInfoDpsFGTS.Create;
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
  inherited Create;
  FInfoTrabFGTS := TInfoTrabFGTSCollection.Create;
end;

destructor TIdeEstabLot2CollectionItem.Destroy;
begin
  FInfoTrabFGTS.Free;
  inherited;
end;

{ TInfoTrabFGTSCollection }

function TInfoTrabFGTSCollection.Add: TInfoTrabFGTSCollectionItem;
begin
  Result := Self.New;
end;

function TInfoTrabFGTSCollection.GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTrabFGTSCollection.SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
begin
  inherited SetItem(Index, Value);
end;
function TInfoTrabFGTSCollection.New: TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoBaseFGTS }

constructor TInfoBaseFGTS.Create;
begin
  inherited Create;
  FBasePerApur := TBasePerApurCollection.Create;
end;

destructor TInfoBaseFGTS.Destroy;
begin
  FBasePerApur.Free;
  inherited;
end;

{ TInfoTrabFGTSCollectionItem }

constructor TInfoTrabFGTSCollectionItem.Create;
begin
  inherited Create;
  FInfoBaseFGTS := TInfoBaseFGTS.Create;
end;

destructor TInfoTrabFGTSCollectionItem.Destroy;
begin
  FInfoBaseFGTS.Free;
  inherited;
end;

{ TBasePerApurCollection }

function TBasePerApurCollection.Add: TBasePerApurCollectionItem;
begin
  Result := Self.New;
end;

function TBasePerApurCollection.GetItem(Index: Integer): TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem(inherited GetItem(Index));
end;

procedure TBasePerApurCollection.SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TBasePerApurCollection.New: TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoDpsFGTS }

constructor TInfoDpsFGTS.Create;
begin
  inherited Create;
  FInfoTrabDps := TInfoTrabDpsCollection.Create;
end;

destructor TInfoDpsFGTS.Destroy;
begin
  FInfoTrabDps.Free;
  inherited;
end;

{ TInfoTrabDpsCollection }

function TInfoTrabDpsCollection.Add: TInfoTrabDpsCollectionItem;
begin
  Result := Self.New;
end;

function TInfoTrabDpsCollection.GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
begin
  Result := TInfoTrabDpsCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTrabDpsCollection.SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TInfoTrabDpsCollection.New: TInfoTrabDpsCollectionItem;
begin
  Result := TInfoTrabDpsCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoTrabDpsCollectionItem }

constructor TInfoTrabDpsCollectionItem.Create;
begin
  inherited Create;
  FDpsPerApur := TDpsPerApurCollection.Create;
end;

destructor TInfoTrabDpsCollectionItem.Destroy;
begin
  FDpsPerApur.Free;
  inherited;
end;

{ TDpsPerApurCollection }

function TDpsPerApurCollection.Add: TDpsPerApurCollectionItem;
begin
  Result := Self.New;
end;

function TDpsPerApurCollection.GetItem(Index: Integer): TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited GetItem(Index));
end;

procedure TDpsPerApurCollection.SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TIdeEstabLot2Collection.New: TIdeEstabLot2CollectionItem;
begin
  Result := TIdeEstabLot2CollectionItem.Create;
  Self.Add(Result);
end;

function TDpsPerApurCollection.New: TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem.Create;
  Self.Add(Result);
end;

end.
