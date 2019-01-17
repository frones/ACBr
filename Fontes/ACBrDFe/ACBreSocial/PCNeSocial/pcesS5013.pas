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
|* 16/01/2016: Arce
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit pcesS5013;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, ACBrUtil,
  pcesCommon, pcesConversaoeSocial;

type
  TS5013 = class;
  TInfoFGTS = class;
  TInfoBaseFGTS = class;
  TBasePerApurCollection = class;
  TBasePerApurCollectionItem = class;
  TInfoDpsFGTS = class;
  TDpsPerApurCollection = class;
  TDpsPerApurCollectionItem = class;

  TEvtFGTS = class;

  TS5013 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtFGTS: TEvtFGTS;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtFGTS(const Value: TEvtFGTS);

  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtFGTS: TEvtFGTS read FEvtFGTS write SetEvtFGTS;
  end;

  TDpsPerApurCollectionItem = class(TCollectionItem)
  private
    FtpDps: Integer;
    FvrFGTS: Currency;
  public
    property tpDps: Integer read FtpDps write FtpDps;
    property vrFGTS: Currency read FvrFGTS write FvrFGTS;
  end;

  TDpsPerApurCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDpsPerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
  public
    constructor Create(AOwner: TInfoDpsFGTS);
    function Add: TDpsPerApurCollectionItem;
    property Items[Index: Integer]: TDpsPerApurCollectionItem read GetItem write SetItem;
  end;

  TInfoDpsFGTS = class(TPersistent)
  private
    FDpsPerApur: TDpsPerApurCollection;

    procedure SetDpsPerApur(const Value: TDpsPerApurCollection);
  public
    constructor Create(AOwner: TInfoFGTS);
    destructor Destroy; override;
    property dpsPerApur: TDpsPerApurCollection read FDpsPerApur write SetDpsPerApur;
  end;

  TBasePerApurCollectionItem = class(TCollectionItem)
  private
    FtpValor: Integer;
    FbaseFGTS: Currency;
  public
    property tpValor: Integer read FtpValor write FtpValor;
    property baseFGTS: Currency read FbaseFGTS write FbaseFGTS;
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
    FInfoDpsFGTS: TDpsPerApurCollection;

    procedure SetBasePerApur(const Value: TBasePerApurCollection);
  public
    constructor Create(AOwner: TInfoFGTS);
    destructor Destroy; override;
    property basePerApur: TBasePerApurCollection read FBasePerApur write SetBasePerApur;
  end;

  TInfoFGTS = class(TPersistent)
  private
    FnrRecArqBase: String;
    FindExistInfo: Integer;
    FInfoBaseFGTS: TInfoBaseFGTS;
    FInfoDpsFGTS: TInfoDpsFGTS;

    procedure SetInfoBaseFGTS(const Value: TInfoBaseFGTS);
    procedure SetInfoDpsFGTS(const Value: TInfoDpsFGTS);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property indExistInfo: Integer read FindExistInfo;
    property infoBaseFGTS: TInfoBaseFGTS read FInfoBaseFGTS write SetInfoBaseFGTS;
    property infoDpsFGTS: TInfoDpsFGTS read FInfoDpsFGTS write SetInfoDpsFGTS;
  end;

  TEvtFGTS = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FInfoIRRF: TInfoFGTS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: Boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoFGTS: TInfoFGTS read FInfoIRRF write FInfoIRRF;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

implementation

uses
  IniFiles;

{ TS5013 }

constructor TS5013.Create;
begin
  FTipoEvento := teS5013;
  FEvtFGTS := TEvtFGTS.Create;
end;

destructor TS5013.Destroy;
begin
  FEvtFGTS.Free;

  inherited;
end;

function TS5013.GetEvento : TObject;
begin
  Result := self;
end;

function TS5013.GetXml : string;
begin
  Result := FEvtFGTS.XML;
end;

procedure TS5013.SetXml(const Value: string);
begin
  if Value = FEvtFGTS.XML then Exit;

  FEvtFGTS.FXML := Value;
  FEvtFGTS.Leitor.Arquivo := Value;
  FEvtFGTS.LerXML;

end;

function TS5013.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TS5013.SetEvtFGTS(const Value: TEvtFGTS);
begin
  FEvtFGTS.Assign(Value);
end;

{ TEvtFGTS }

constructor TEvtFGTS.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoIRRF := TInfoFGTS.Create;
end;

destructor TEvtFGTS.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoIRRF.Free;

  inherited;
end;

{ TInfoFGTS }

constructor TInfoFGTS.Create;
begin
  FInfoBaseFGTS := TInfoBaseFGTS.Create(Self);
  FInfoDpsFGTS := TInfoDpsFGTS.Create(Self);
end;

destructor TInfoFGTS.Destroy;
begin
  FInfoBaseFGTS.Free;

  inherited;
end;

procedure TInfoFGTS.SetInfoBaseFGTS(const Value: TInfoBaseFGTS);
begin
  FInfoBaseFGTS := Value;
end;

procedure TInfoFGTS.SetInfoDpsFGTS(const Value: TInfoDpsFGTS);
begin
  FInfoDpsFGTS := Value;
end;

function TEvtFGTS.LerXML: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtFGTS') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'infoFGTS') <> '' then
      begin
        InfoFGTS.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        InfoFGTS.FindExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        if leitor.rExtrai(3, 'infoBaseFGTS') <> '' then
        begin
           i := 0;
           while Leitor.rExtrai(4, 'basePerApur', '', i + 1) <> '' do
           begin
             InfoFGTS.infoBaseFGTS.basePerApur.Add;
             InfoFGTS.infoBaseFGTS.basePerApur.Items[i].tpValor  := leitor.rCampo(tcInt, 'tpValor');
             InfoFGTS.infoBaseFGTS.basePerApur.Items[i].baseFGTS := leitor.rCampo(tcDe2, 'remFGTS');
             inc(i);
           end;
        end;

        if leitor.rExtrai(3, 'infoDpsFGTS') <> '' then
        begin
           i := 0;
           while Leitor.rExtrai(4, 'dpsPerApur', '', i + 1) <> '' do
           begin
             InfoFGTS.infoDpsFGTS.dpsPerApur.Add;
             InfoFGTS.infoDpsFGTS.dpsPerApur.Items[i].tpDps  := leitor.rCampo(tcInt, 'tpDps');
             InfoFGTS.infoDpsFGTS.dpsPerApur.Items[i].vrFGTS := leitor.rCampo(tcDe2, 'vrFGTS');
             inc(i);
           end;
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtFGTS.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i: Integer;
begin
  Result := False;

  AIni := TMemIniFile.Create('');
  try
    Result := True;

  finally
    AIni.Free;
  end;
end;

{ TInfoBaseFGTS }

constructor TInfoBaseFGTS.Create(AOwner: TInfoFGTS);
begin
  FBasePerApur := TBasePerApurCollection.Create(Self);
end;

destructor TInfoBaseFGTS.Destroy;
begin
  FBasePerApur.Free;
  inherited;
end;

procedure TInfoBaseFGTS.SetBasePerApur(const Value: TBasePerApurCollection);
begin
  FBasePerApur := Value;
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

{ TInfoDpsFGTSCollection }

function TDpsPerApurCollection.Add: TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited Add);
  Result.Create(self);
end;

constructor TDpsPerApurCollection.Create(AOwner: TInfoDpsFGTS);
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

{ TInfoDpsFGTS }

constructor TInfoDpsFGTS.Create(AOwner: TInfoFGTS);
begin
  FDpsPerApur := TDpsPerApurCollection.Create(Self);
end;

destructor TInfoDpsFGTS.Destroy;
begin
  FDpsPerApur.Free;
  inherited;
end;

procedure TInfoDpsFGTS.SetDpsPerApur(const Value: TDpsPerApurCollection);
begin
  FDpsPerApur := Value;
end;

end.

