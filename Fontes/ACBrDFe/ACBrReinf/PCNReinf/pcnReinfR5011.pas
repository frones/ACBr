{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

{$I ACBr.inc}

unit pcnReinfR5011;

interface

uses
  Classes, Sysutils, pcnGerador, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf;

type
  TR5011 = class;
  TEvtTotalContrib = class;
  TInfoRecEv = class;
  TInfoTotalContribCollection = class;
  TInfoTotalContribCollectionItem = class;
  TRTomCollection = class;
  TRTomCollectionItem = class;
  TRPrestCollection = class;
  TRPrestCollectionItem = class;
  TRRecRepADCollection = class;
  TRRecRepADCollectionItem = class;
  TRComl = class;
  TRCPRBCollection = class;
  TRCPRBCollectionItem = class;

  TR5011 = class(TInterfacedObject, IEventoReinf)
  private
    FTipoEvento: TTipoEvento;
    FEvtTotalContrib: TEvtTotalContrib;

    function GetXml: string;
    procedure SetXml(const Value: string);
    function GetTipoEvento: TTipoEvento;
    procedure SetEvtTotalContrib(const Value: TEvtTotalContrib);
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento: TObject;
  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtTotalContrib: TEvtTotalContrib read FEvtTotalContrib write setEvtTotalContrib;
  end;

  TEvtTotalContrib = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento1;
    FIdeContrib: TIdeContrib;
    FIdeStatus: TIdeStatus;
    FInfoRecEv: TInfoRecEv;
    FInfoTotalContrib: TInfoTotalContribCollection;

    procedure SetInfoTotalContrib(const Value: TInfoTotalContribCollection);
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotalContrib: TInfoTotalContribCollection read FInfoTotalContrib write SetInfoTotalContrib;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

  TInfoRecEv = class
  private
    FnrProtEntr: String;
    FdhProcess: TDateTime;
    FtpEv: String;
    FidEv: String;
    Fhash: String;
  public
    property nrProtEntr: String read FnrProtEntr;
    property dhProcess: TDateTime read FdhProcess;
    property tpEv: String read FtpEv;
    property idEv: String read FidEv;
    property hash: String read Fhash;
  end;

  TInfoTotalContribCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoTotalContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTotalContribCollectionItem);
  public
    constructor Create(AOwner: TEvtTotalContrib);
    function Add: TInfoTotalContribCollectionItem;
    property Items[Index: Integer]: TInfoTotalContribCollectionItem read GetItem write SetItem;
  end;

  TInfoTotalContribCollectionItem = class(TCollectionItem)
  private
    FnrRecArqBase: String;
    FindExistInfo: TindExistInfo;
    FRTom: TRTomCollection;
    FRPrest: TRPrestCollection;
    FRRecRepAD: TRRecRepADCollection;
    FRComl: TRComl;
    FRCPRB: TRCPRBCollection;

    procedure SetRTom(const Value: TRTomCollection);
    procedure SetRPrest(const Value: TRPrestCollection);
    procedure SetRRecRepAD(const Value: TRRecRepADCollection);
    procedure SetRCPRB(const Value: TRCPRBCollection);

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property indExistInfo: TindExistInfo read FindExistInfo;
    property RTom: TRTomCollection read FRTom;
    property RPrest: TRPrestCollection read FRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD;
    property RComl: TRComl read FRComl;
    property RCPRB: TRCPRBCollection read FRCPRB;
  end;

  TRTomCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRTomCollectionItem;
    procedure SetItem(Index: Integer; Value: TRTomCollectionItem);
  public
    constructor Create(AOwner: TInfoTotalContribCollectionItem);
    function Add: TRTomCollectionItem;
    property Items[Index: Integer]: TRTomCollectionItem read GetItem write SetItem;
  end;

  TRTomCollectionItem = class(TCollectionItem)
  private
    FcnpjPrestador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalNRetAdic: Double;
  public
    property cnpjPrestador: String read FcnpjPrestador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic;
  end;

  TRPrestCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRPrestCollectionItem;
    procedure SetItem(Index: Integer; Value: TRPrestCollectionItem);
  public
    constructor Create(AOwner: TInfoTotalContribCollectionItem);
    function Add: TRPrestCollectionItem;
    property Items[Index: Integer]: TRPrestCollectionItem read GetItem write SetItem;
  end;

  TRPrestCollectionItem = class(TCollectionItem)
  private
    FtpInscTomador: TtpInsc;
    FnrInscTomador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalNRetAdic: Double;
  public
    property tpInscTomador: TtpInsc read FtpInscTomador;
    property nrInscTomador: String read FnrInscTomador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic;
  end;

  TRRecRepADCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRRecRepADCollectionItem;
    procedure SetItem(Index: Integer; Value: TRRecRepADCollectionItem);
  public
    constructor Create(AOwner: TInfoTotalContribCollectionItem);
    function Add: TRRecRepADCollectionItem;
    property Items[Index: Integer]: TRRecRepADCollectionItem read GetItem write SetItem;
  end;

  TRRecRepADCollectionItem = class(TCollectionItem)
  private
    FcnpjAssocDesp: string;
    FvlrTotalRep: Double;
    FvlrTotalRet: Double;
    FvlrTotalNRet: Double;
  public
    property cnpjAssocDesp: string read FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet;
  end;

  TRComl = class(TPersistent)
  private
    FvlrCPApur: Double;
    FvlrRatApur: Double;
    FvlrSenarApur: Double;
    FvlrCPSusp: Double;
    FvlrRatSusp: Double;
    FvlrSenarSusp: Double;
  public
    property vlrCPApur: Double read FvlrCPApur;
    property vlrRatApur: Double read FvlrRatApur;
    property vlrSenarApur: Double read FvlrSenarApur;
    property vlrCPSusp: Double read FvlrCPSusp;
    property vlrRatSusp: Double read FvlrRatSusp;
    property vlrSenarSusp: Double read FvlrSenarSusp;
  end;

  TRCPRBCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRCPRBCollectionItem;
    procedure SetItem(Index: Integer; Value: TRCPRBCollectionItem);
  public
    constructor Create(AOwner: TInfoTotalContribCollectionItem);
    function Add: TRCPRBCollectionItem;
    property Items[Index: Integer]: TRCPRBCollectionItem read GetItem write SetItem;
  end;

  TRCPRBCollectionItem = class(TCollectionItem)
  private
    FcodRec: Integer;
    FvlrCPApurTotal: Double;
    FvlrCPRBSusp: Double;
  public
    property codRec: Integer read FcodRec;
    property vlrCPApurTotal: Double read FvlrCPApurTotal;
    property vlrCPRBSusp: Double read FvlrCPRBSusp;
  end;

implementation

uses
  IniFiles, pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR5011 }

constructor TR5011.Create;
begin
  FTipoEvento := teR5011;
  FEvtTotalContrib := TEvtTotalContrib.Create;
end;

destructor TR5011.Destroy;
begin
  FEvtTotalContrib.Free;

  inherited;
end;

function TR5011.GetXml : string;
begin
  Result := FEvtTotalContrib.XML;
end;

procedure TR5011.SetXml(const Value: string);
begin
  if Value = FEvtTotalContrib.XML then
    Exit;

  FEvtTotalContrib.FXML := Value;
  FEvtTotalContrib.Leitor.Arquivo := Value;
  FEvtTotalContrib.LerXML;
end;

function TR5011.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TR5011.SetEvtTotalContrib(const Value: TEvtTotalContrib);
begin
  FEvtTotalContrib.Assign(Value);
end;

function TR5011.GetEvento: TObject;
begin
  Result := Self;
end;

{ TInfoTotalContribCollection }

function TInfoTotalContribCollection.Add: TInfoTotalContribCollectionItem;
begin
  Result := TInfoTotalContribCollectionItem(inherited Add);
  Result.Create;
end;

constructor TInfoTotalContribCollection.Create(AOwner: TEvtTotalContrib);
begin
  inherited create(TInfoTotalContribCollectionItem);
end;

function TInfoTotalContribCollection.GetItem(
  Index: Integer): TInfoTotalContribCollectionItem;
begin
  Result := TInfoTotalContribCollectionItem(inherited GetItem(Index));
end;

procedure TInfoTotalContribCollection.SetItem(Index: Integer;
  Value: TInfoTotalContribCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoTotalContribCollectionItem }

constructor TInfoTotalContribCollectionItem.Create;
begin
  FRTom      := TRTomCollection.Create(Self);
  FRPrest    := TRPrestCollection.Create(Self);
  FRRecRepAD := TRRecRepADCollection.Create(Self);
  FRComl     := TRComl.Create;
  FRCPRB     := TRCPRBCollection.Create(Self);
end;

destructor TInfoTotalContribCollectionItem.Destroy;
begin
  FRTom.Free;
  FRPrest.Free;
  FRRecRepAD.Free;
  FRComl.Free;
  FRCPRB.Free;

  inherited;
end;

procedure TInfoTotalContribCollectionItem.SetRCPRB(
  const Value: TRCPRBCollection);
begin
  FRCPRB := Value;
end;

procedure TInfoTotalContribCollectionItem.SetRPrest(
  const Value: TRPrestCollection);
begin
  FRPrest := Value;
end;

procedure TInfoTotalContribCollectionItem.SetRRecRepAD(
  const Value: TRRecRepADCollection);
begin
  FRRecRepAD := Value;
end;

procedure TInfoTotalContribCollectionItem.SetRTom(
  const Value: TRTomCollection);
begin
  FRTom := Value;
end;

{ TRTomCollection }

function TRTomCollection.Add: TRTomCollectionItem;
begin
  Result := TRTomCollectionItem(inherited Add);
//  Result.Create;
end;

constructor TRTomCollection.Create(AOwner: TInfoTotalContribCollectionItem);
begin
  inherited create(TRTomCollectionItem);
end;

function TRTomCollection.GetItem(Index: Integer): TRTomCollectionItem;
begin
  Result := TRTomCollectionItem(inherited GetItem(Index));
end;

procedure TRTomCollection.SetItem(Index: Integer;
  Value: TRTomCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRPrestCollection }

function TRPrestCollection.Add: TRPrestCollectionItem;
begin
  Result := TRPrestCollectionItem(inherited Add);
//  Result.Create;
end;

constructor TRPrestCollection.Create(AOwner: TInfoTotalContribCollectionItem);
begin
  inherited create(TRPrestCollectionItem);
end;

function TRPrestCollection.GetItem(Index: Integer): TRPrestCollectionItem;
begin
  Result := TRPrestCollectionItem(inherited GetItem(Index));
end;

procedure TRPrestCollection.SetItem(Index: Integer;
  Value: TRPrestCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRRecRepADCollection }

function TRRecRepADCollection.Add: TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem(inherited Add);
//  Result.Create;
end;

constructor TRRecRepADCollection.Create(AOwner: TInfoTotalContribCollectionItem);
begin
  inherited create(TRRecRepADCollectionItem);
end;

function TRRecRepADCollection.GetItem(
  Index: Integer): TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem(inherited GetItem(Index));
end;

procedure TRRecRepADCollection.SetItem(Index: Integer;
  Value: TRRecRepADCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRCPRBCollection }

function TRCPRBCollection.Add: TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited Add);
//  Result.Create;
end;

constructor TRCPRBCollection.Create(AOwner: TInfoTotalContribCollectionItem);
begin
  inherited create(TRCPRBCollectionItem);
end;

function TRCPRBCollection.GetItem(Index: Integer): TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited GetItem(Index));
end;

procedure TRCPRBCollection.SetItem(Index: Integer;
  Value: TRCPRBCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEvtTotalContrib }

constructor TEvtTotalContrib.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeEvento        := TIdeEvento1.Create;
  FIdeContrib       := TIdeContrib.Create;
  FIdeStatus        := TIdeStatus.Create;
  FInfoRecEv        := TInfoRecEv.Create;
  FInfoTotalContrib := TInfoTotalContribCollection.Create(Self);
end;

destructor TEvtTotalContrib.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeContrib.Free;
  FIdeStatus.Free;
  FInfoRecEv.Free;
  FInfoTotalContrib.Free;

  inherited;
end;

procedure TEvtTotalContrib.SetInfoTotalContrib(
  const Value: TInfoTotalContribCollection);
begin
  FInfoTotalContrib := Value;
end;

function TEvtTotalContrib.LerXML: boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtTotalContrib') <> '' then
    begin
      FId := Leitor.rAtributo('id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideContri') <> '' then
      begin
        IdeContrib.TpInsc := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeContrib.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideRecRetorno') <> '' then
      begin
        if leitor.rExtrai(3, 'ideStatus') <> '' then
        begin
          IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
          IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

          i := 0;
          while Leitor.rExtrai(4, 'regOcorrs', '', i + 1) <> '' do
          begin
            IdeStatus.regOcorrs.Add;
            IdeStatus.regOcorrs.Items[i].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
            IdeStatus.regOcorrs.Items[i].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
            IdeStatus.regOcorrs.Items[i].codResp        := leitor.rCampo(tcStr, 'codResp');
            IdeStatus.regOcorrs.Items[i].dscResp        := leitor.rCampo(tcStr, 'dscResp');
            
            inc(i);
          end;
        end;
      end;

      if leitor.rExtrai(2, 'infoRecEv') <> '' then
      begin
        infoRecEv.FnrProtEntr := leitor.rCampo(tcStr, 'nrProtEntr');
        infoRecEv.FdhProcess  := leitor.rCampo(tcDat, 'dhProcess');
        infoRecEv.FtpEv       := leitor.rCampo(tcStr, 'tpEv');
        infoRecEv.FidEv       := leitor.rCampo(tcStr, 'idEv');
        infoRecEv.Fhash       := leitor.rCampo(tcStr, 'hash');
      end;

      i := 0;
      while Leitor.rExtrai(2, 'infoTotalContrib', '', i + 1) <> '' do
      begin
        infoTotalContrib.Add;
        infoTotalContrib.Items[i].FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoTotalContrib.Items[i].FindExistInfo := StrToindExistInfo(Ok, leitor.rCampo(tcStr, 'indExistInfo'));

        j := 0;
        while Leitor.rExtrai(3, 'RTom', '', j + 1) <> '' do
        begin
          infoTotalContrib.Items[i].RTom.Add;

          InfoTotalContrib.Items[i].RTom.Items[j].FcnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
          InfoTotalContrib.Items[i].RTom.Items[j].FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          InfoTotalContrib.Items[i].RTom.Items[j].FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          InfoTotalContrib.Items[i].RTom.Items[j].FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          InfoTotalContrib.Items[i].RTom.Items[j].FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          InfoTotalContrib.Items[i].RTom.Items[j].FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

          inc(j);
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RPrest', '', j + 1) <> '' do
        begin
          infoTotalContrib.Items[i].RPrest.Add;

          InfoTotalContrib.Items[i].RPrest.Items[j].FtpInscTomador     := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInscTomador'));
          InfoTotalContrib.Items[i].RPrest.Items[j].FnrInscTomador     := leitor.rCampo(tcStr, 'nrInscTomador');
          InfoTotalContrib.Items[i].RPrest.Items[j].FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          InfoTotalContrib.Items[i].RPrest.Items[j].FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          InfoTotalContrib.Items[i].RPrest.Items[j].FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          InfoTotalContrib.Items[i].RPrest.Items[j].FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          InfoTotalContrib.Items[i].RPrest.Items[j].FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

          inc(j);
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RRecRepAD', '', j + 1) <> '' do
        begin
          InfoTotalContrib.Items[i].RRecRepAD.Add;
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].FcnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].FvlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].FvlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].FvlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');

          inc(j);
        end;

        if leitor.rExtrai(3, 'RComl') <> '' then
        begin
          InfoTotalContrib.Items[i].RComl.FvlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
          InfoTotalContrib.Items[i].RComl.FvlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
          InfoTotalContrib.Items[i].RComl.FvlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
          InfoTotalContrib.Items[i].RComl.FvlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
          InfoTotalContrib.Items[i].RComl.FvlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
          InfoTotalContrib.Items[i].RComl.FvlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RCPRB', '', j + 1) <> '' do
        begin
          InfoTotalContrib.Items[i].RCPRB.Add;
          InfoTotalContrib.Items[i].RCPRB.Items[j].FcodRec         := leitor.rCampo(tcInt, 'codRec');
          InfoTotalContrib.Items[i].RCPRB.Items[j].FvlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          InfoTotalContrib.Items[i].RCPRB.Items[j].FvlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');
          inc(i);
        end;

        inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtTotalContrib.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j: Integer;
begin
  Result := False;

  AIni := TMemIniFile.Create('');
  try
    Result := True;

    with Self do
    begin
      sSecao := 'evtTotal';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'perApur', IdeEvento.perApur);

      sSecao := 'ideContri';
      AIni.WriteString(sSecao, 'tpInsc', TpInscricaoToStr(IdeContrib.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeContrib.nrInsc);

      sSecao := 'ideStatus';
      AIni.WriteString(sSecao, 'cdRetorno', ideStatus.cdRetorno);
      AIni.WriteString(sSecao, 'descRetorno', ideStatus.descRetorno);

      for i := 0 to ideStatus.regOcorrs.Count -1 do
      begin
        sSecao := 'regOcorrs' + IntToStrZero(I, 3);

        AIni.WriteInteger(sSecao, 'tpOcorr',       ideStatus.regOcorrs.Items[i].tpOcorr);
        AIni.WriteString(sSecao, 'localErroAviso', ideStatus.regOcorrs.Items[i].localErroAviso);
        AIni.WriteString(sSecao, 'codResp',        ideStatus.regOcorrs.Items[i].codResp);
        AIni.WriteString(sSecao, 'dscResp',        ideStatus.regOcorrs.Items[i].dscResp);
      end;

      sSecao := 'infoRecEv';
      AIni.WriteString(sSecao, 'nrProtEntr', infoRecEv.nrProtEntr);
      AIni.WriteString(sSecao, 'dhProcess',  DateToStr(infoRecEv.dhProcess));
      AIni.WriteString(sSecao, 'tpEv',       infoRecEv.tpEv);
      AIni.WriteString(sSecao, 'idEv',       infoRecEv.idEv);
      AIni.WriteString(sSecao, 'hash',       infoRecEv.hash);

      for i := 0 to infoTotalContrib.Count -1 do
      begin
        sSecao := 'infoTotalContrib' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'nrRecArqBase', infoTotalContrib.Items[i].nrRecArqBase);
        AIni.WriteString(sSecao, 'indExistInfo', indExistInfoToStr(infoTotalContrib.Items[i].indExistInfo));

        with infoTotalContrib.Items[i] do
        begin
          for j := 0 to RTom.Count -1 do
          begin
            sSecao := 'RTom' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

            AIni.WriteString(sSecao, 'cnpjPrestador',    RTom.Items[j].cnpjPrestador);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RTom.Items[j].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RTom.Items[j].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RTom.Items[j].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RTom.Items[j].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RTom.Items[j].vlrTotalNRetAdic);
          end;

          for j := 0 to RPrest.Count -1 do
          begin
            sSecao := 'RPrest' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

            AIni.WriteString(sSecao, 'tpInscTomador',    TpInscricaoToStr(RPrest.Items[j].tpInscTomador));
            AIni.WriteString(sSecao, 'nrInscTomador',    RPrest.Items[j].nrInscTomador);
            AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RPrest.Items[j].vlrTotalBaseRet);
            AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RPrest.Items[j].vlrTotalRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RPrest.Items[j].vlrTotalRetAdic);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RPrest.Items[j].vlrTotalNRetPrinc);
            AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RPrest.Items[j].vlrTotalNRetAdic);
          end;

          for j := 0 to RRecRepAD.Count -1 do
          begin
            sSecao := 'RRecRepAD' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

            AIni.WriteString(sSecao, 'cnpjAssocDesp', RRecRepAD.Items[j].cnpjAssocDesp);
            AIni.WriteFloat(sSecao, 'vlrTotalRep',    RRecRepAD.Items[j].vlrTotalRep);
            AIni.WriteFloat(sSecao, 'vlrTotalRet',    RRecRepAD.Items[j].vlrTotalRet);
            AIni.WriteFloat(sSecao, 'vlrTotalNRet',   RRecRepAD.Items[j].vlrTotalNRet);
          end;

          sSecao := 'RComl' + IntToStrZero(I, 3);
          AIni.WriteFloat(sSecao, 'vlrCPApur',    RComl.vlrCPApur);
          AIni.WriteFloat(sSecao, 'vlrRatApur',   RComl.vlrRatApur);
          AIni.WriteFloat(sSecao, 'vlrSenarApur', RComl.vlrSenarApur);
          AIni.WriteFloat(sSecao, 'vlrCPSusp',    RComl.vlrCPSusp);
          AIni.WriteFloat(sSecao, 'vlrRatSusp',   RComl.vlrRatSusp);
          AIni.WriteFloat(sSecao, 'vlrSenarSusp', RComl.vlrSenarSusp);

          for j := 0 to RCPRB.Count -1 do
          begin
            sSecao := 'RCPRB' + IntToStrZero(I, 3) + IntToStrZero(j, 1);

            AIni.WriteInteger(sSecao, 'codRec',       RCPRB.Items[j].codRec);
            AIni.WriteFloat(sSecao, 'vlrCPApurTotal', RCPRB.Items[j].vlrCPApurTotal);
            AIni.WriteFloat(sSecao, 'vlrCPRBSusp',    RCPRB.Items[j].vlrCPRBSusp);
          end;
        end;
      end;
    end;
  finally
    AIni.Free;
  end;
end;

end.
