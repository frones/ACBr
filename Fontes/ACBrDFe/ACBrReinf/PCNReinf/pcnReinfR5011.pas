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

unit pcnReinfR5011;

interface

uses
  Classes, Sysutils, pcnGerador, pcnLeitor,
  pcnConversaoReinf, pcnReinfClasses, ACBrReinfEventosBase;

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

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtTotalContrib(const Value: TEvtTotalContrib);
  public
    constructor Create;
    destructor Destroy; override;

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

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotalContrib: TInfoTotalContribCollection read FInfoTotalContrib write SetInfoTotalContrib;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

  TInfoRecEv = class(TPersistent)
  private
    FnrProtEntr: String;
    FdhProcess: TDateTime;
    FtpEv: String;
    FidEv: String;
    Fhash: String;
  public
    property nrProtEntr: String read FnrProtEntr write FnrProtEntr;
    property dhProcess: TDateTime read FdhProcess write FdhProcess;
    property tpEv: String read FtpEv write FtpEv;
    property idEv: String read FidEv write FidEv;
    property hash: String read Fhash write Fhash;
  end;

  TInfoTotalContribCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoTotalContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTotalContribCollectionItem);
  public
    constructor Create; reintroduce;
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
    constructor Create(AOwner: TEvtTotalContrib); reintroduce;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property indExistInfo: TindExistInfo read FindExistInfo write FindExistInfo;
    property RTom: TRTomCollection read FRTom write SetRTom;
    property RPrest: TRPrestCollection read FRPrest write SetRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD write SetRRecRepAD;
    property RComl: TRComl read FRComl write FRComl;
    property RCPRB: TRCPRBCollection read FRCPRB write SetRCPRB;
  end;

  TRTomCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRTomCollectionItem;
    procedure SetItem(Index: Integer; Value: TRTomCollectionItem);
  public
    constructor Create; reintroduce;
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
    property cnpjPrestador: String read FcnpjPrestador write FcnpjPrestador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
  end;

  TRPrestCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRPrestCollectionItem;
    procedure SetItem(Index: Integer; Value: TRPrestCollectionItem);
  public
    constructor Create; reintroduce;
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
    property tpInscTomador: TtpInsc read FtpInscTomador write FtpInscTomador;
    property nrInscTomador: String read FnrInscTomador write FnrInscTomador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
  end;

  TRRecRepADCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRRecRepADCollectionItem;
    procedure SetItem(Index: Integer; Value: TRRecRepADCollectionItem);
  public
    constructor Create; reintroduce;
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
    property cnpjAssocDesp: string read FcnpjAssocDesp write FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep write FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet write FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet write FvlrTotalNRet;
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
    property vlrCPApur: Double read FvlrCPApur write FvlrCPApur;
    property vlrRatApur: Double read FvlrRatApur write FvlrRatApur;
    property vlrSenarApur: Double read FvlrSenarApur write FvlrSenarApur;
    property vlrCPSusp: Double read FvlrCPSusp write FvlrCPSusp;
    property vlrRatSusp: Double read FvlrRatSusp write FvlrRatSusp;
    property vlrSenarSusp: Double read FvlrSenarSusp write FvlrSenarSusp;
  end;

  TRCPRBCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRCPRBCollectionItem;
    procedure SetItem(Index: Integer; Value: TRCPRBCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TRCPRBCollectionItem;
    property Items[Index: Integer]: TRCPRBCollectionItem read GetItem write SetItem;
  end;

  TRCPRBCollectionItem = class(TCollectionItem)
  private
    FcodRec: Integer;
    FvlrCPApurTotal: Double;
    FvlrCPRBSusp: Double;
  public
    property codRec: Integer read FcodRec write FcodRec;
    property vlrCPApurTotal: Double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPRBSusp: Double read FvlrCPRBSusp write FvlrCPRBSusp;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

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
  if Value = FEvtTotalContrib.XML then Exit;

  FEvtTotalContrib.XML := Value;
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

{ TEvtTotalContrib }

constructor TEvtTotalContrib.Create();
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento1.Create;
  FIdeContrib := TIdeContrib.Create;
  FIdeStatus := TIdeStatus.Create;
  FInfoRecEv := TInfoRecEv.Create;
  FInfoTotalContrib := TInfoTotalContribCollection.Create;
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
    XML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtTotalContrib') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

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
        infoRecEv.nrProtEntr := leitor.rCampo(tcStr, 'nrProtEntr');
        infoRecEv.dhProcess  := leitor.rCampo(tcDatHor, 'dhProcess');
        infoRecEv.tpEv       := leitor.rCampo(tcStr, 'tpEv');
        infoRecEv.idEv       := leitor.rCampo(tcStr, 'idEv');
        infoRecEv.hash       := leitor.rCampo(tcStr, 'hash');
      end;

      i := 0;
      while Leitor.rExtrai(2, 'infoTotalContrib', '', i + 1) <> '' do
      begin
        infoTotalContrib.Add;
        infoTotalContrib.Items[i].nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoTotalContrib.Items[i].indExistInfo := StrToindExistInfo(Ok, leitor.rCampo(tcStr, 'indExistInfo'));

        j := 0;
        while Leitor.rExtrai(3, 'RTom', '', j + 1) <> '' do
        begin
          infoTotalContrib.Items[i].RTom.Add;

          InfoTotalContrib.Items[i].RTom.Items[j].cnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
          InfoTotalContrib.Items[i].RTom.Items[j].vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          InfoTotalContrib.Items[i].RTom.Items[j].vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          InfoTotalContrib.Items[i].RTom.Items[j].vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          InfoTotalContrib.Items[i].RTom.Items[j].vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          InfoTotalContrib.Items[i].RTom.Items[j].vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

          inc(j);
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RPrest', '', j + 1) <> '' do
        begin
          infoTotalContrib.Items[i].RPrest.Add;

          InfoTotalContrib.Items[i].RPrest.Items[j].tpInscTomador     := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInscTomador'));
          InfoTotalContrib.Items[i].RPrest.Items[j].nrInscTomador     := leitor.rCampo(tcStr, 'nrInscTomador');
          InfoTotalContrib.Items[i].RPrest.Items[j].vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          InfoTotalContrib.Items[i].RPrest.Items[j].vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          InfoTotalContrib.Items[i].RPrest.Items[j].vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          InfoTotalContrib.Items[i].RPrest.Items[j].vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          InfoTotalContrib.Items[i].RPrest.Items[j].vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

          inc(j);
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RRecRepAD', '', j + 1) <> '' do
        begin
          InfoTotalContrib.Items[i].RRecRepAD.Add;
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].cnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].vlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].vlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
          InfoTotalContrib.Items[i].RRecRepAD.Items[j].vlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');

          inc(j);
        end;

        if leitor.rExtrai(3, 'RComl') <> '' then
        begin
          InfoTotalContrib.Items[i].RComl.vlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
          InfoTotalContrib.Items[i].RComl.vlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
          InfoTotalContrib.Items[i].RComl.vlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
          InfoTotalContrib.Items[i].RComl.vlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
          InfoTotalContrib.Items[i].RComl.vlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
          InfoTotalContrib.Items[i].RComl.vlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');
        end;

        j := 0;
        while Leitor.rExtrai(3, 'RCPRB', '', j + 1) <> '' do
        begin
          InfoTotalContrib.Items[i].RCPRB.Add;
          InfoTotalContrib.Items[i].RCPRB.Items[j].codRec         := leitor.rCampo(tcInt, 'codRec');
          InfoTotalContrib.Items[i].RCPRB.Items[j].vlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          InfoTotalContrib.Items[i].RCPRB.Items[j].vlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');
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

(*
{ TRCPRBCollection }

function TRCPRBCollection.Add: TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited Add);
end;

constructor TRCPRBCollection.Create(AOwner: TInfoTotalContrib);
begin
  inherited create(TRCPRBCollectionItem);
end;

function TRCPRBCollection.GetItem(
  Index: Integer): TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited GetItem(Index));
end;

procedure TRCPRBCollection.SetItem(Index: Integer;
  Value: TRCPRBCollectionItem);
begin
  inherited SetItem(Index, Value);
end;
*)

{ TInfoTotalContribCollection }

function TInfoTotalContribCollection.Add: TInfoTotalContribCollectionItem;
begin
  Result := TInfoTotalContribCollectionItem(inherited Add);
end;

constructor TInfoTotalContribCollection.Create;
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

constructor TInfoTotalContribCollectionItem.Create(
  AOwner: TEvtTotalContrib);
begin
  FRTom := TRTomCollection.Create;
  FRPrest := TRPrestCollection.Create;
  FRRecRepAD := TRRecRepADCollection.Create;
  FRComl := TRComl.Create;
  FRCPRB := TRCPRBCollection.Create;
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
end;

constructor TRTomCollection.Create;
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
end;

constructor TRPrestCollection.Create;
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
end;

constructor TRRecRepADCollection.Create;
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
end;

constructor TRCPRBCollection.Create;
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

end.
