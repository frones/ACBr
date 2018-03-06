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

unit pcnReinfR5001;

interface

uses
  Classes, Sysutils, pcnGerador, pcnLeitor,
  pcnConversaoReinf, pcnReinfClasses, ACBrReinfEventosBase;

type

  TR5001 = class;
  TEvtTotal = class;
  TRRecRepADCollection = class;
  TRRecRepADCollectionItem = class;
  TRCPRBCollection = class;
  TRCPRBCollectionItem = class;

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

  TRTom = class(TPersistent)
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

  TRPrest = class(TPersistent)
  private
    FtpInscTomador: tpTpInsc;
    FnrInscTomador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalNRetAdic: Double;
  public
    property tpInscTomador: tpTpInsc read FtpInscTomador write FtpInscTomador;
    property nrInscTomador: String read FnrInscTomador write FnrInscTomador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
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

  TRRecEspetDest = class(TPersistent)
  private
    FvlrReceitaTotal: Double;
    FvlrCPApurTotal: Double;
    FvlrCPSuspTotal: Double;
  public
    property vlrReceitaTotal: Double read FvlrReceitaTotal write FvlrReceitaTotal;
    property vlrCPApurTotal: Double read FvlrCPApurTotal write FvlrCPApurTotal;
    property vlrCPSuspTotal: Double read FvlrCPSuspTotal write FvlrCPSuspTotal;
  end;

  TInfoTotal = class(TPersistent)
  private
    FnrRecArqBase: String;
    FRTom: TRTom;
    FRPrest: TRPrest;
    FRRecRepAD: TRRecRepADCollection;
    FRComl: TRComl;
    FRCPRB: TRCPRBCollection;
    FRRecEspetDest: TRRecEspetDest;

    procedure SetRRecRepAD(const Value: TRRecRepADCollection);
    procedure SetRCPRB(const Value: TRCPRBCollection);
  public
    constructor Create(AOwner: TEvtTotal);
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property RTom: TRTom read FRTom write FRTom;
    property RPrest: TRPrest read FRPrest write FRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD write SetRRecRepAD;
    property RComl: TRComl read FRComl write FRComl;
    property RCPRB: TRCPRBCollection read FRCPRB write SetRCPRB;
    property RRecEspetDest: TRRecEspetDest read FRRecEspetDest write FRRecEspetDest;
  end;

  TR5001 = class(TInterfacedObject, IEventoReinf)
  private
    FTipoEvento: TTipoEvento;
    FEvtTotal: TEvtTotal;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtTotal(const Value: TEvtTotal);
  public
    constructor Create;
    destructor Destroy; override;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtTotal: TEvtTotal read FEvtTotal write setEvtTotal;
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

  TRCPRBCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRCPRBCollectionItem;
    procedure SetItem(Index: Integer; Value: TRCPRBCollectionItem);
  public
    constructor Create(AOwner: TInfoTotal);
    function Add: TRCPRBCollectionItem;
    property Items[Index: Integer]: TRCPRBCollectionItem read GetItem write SetItem;
  end;

  TRCPRBCollectionItem = class(TCollectionItem)
  private
    FcodRec: Integer;
    FvlrCPAPApurTotal: Double;
    FvlrCPPRBSusp: Double;
  public
    property codRec: Integer read FcodRec write FcodRec;
    property vlrCPAPApurTotal: Double read FvlrCPAPApurTotal write FvlrCPAPApurTotal;
    property vlrCPPRBSusp: Double read FvlrCPPRBSusp write FvlrCPPRBSusp;
  end;

  TEvtTotal = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento1;
    FIdeContrib: TIdeContrib;
    FIdeStatus: TIdeStatus;
    FInfoRecEv: TInfoRecEv;
    FInfoTotal: TInfoTotal;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotal: TInfoTotal read FInfoTotal write FInfoTotal;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR5001 }

constructor TR5001.Create;
begin
  FTipoEvento := teR5001;
  FEvtTotal := TEvtTotal.Create;
end;

destructor TR5001.Destroy;
begin
  FEvtTotal.Free;

  inherited;
end;

function TR5001.GetXml : string;
begin
  Result := FEvtTotal.XML;
end;

procedure TR5001.SetXml(const Value: string);
begin
  if Value = FEvtTotal.XML then Exit;

  FEvtTotal.XML := Value;
  FEvtTotal.Leitor.Arquivo := Value;
  FEvtTotal.LerXML;
end;

function TR5001.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TR5001.SetEvtTotal(const Value: TEvtTotal);
begin
  FEvtTotal.Assign(Value);
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

constructor TRCPRBCollection.Create(AOwner: TInfoTotal);
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

{ TEvtTotal }

constructor TEvtTotal.Create();
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento1.Create;
  FIdeContrib := TIdeContrib.Create;
  FIdeStatus := TIdeStatus.Create;
  FInfoRecEv := TInfoRecEv.Create;
  FInfoTotal := TInfoTotal.Create(Self);
end;

destructor TEvtTotal.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeContrib.Free;
  FIdeStatus.Free;
  FInfoRecEv.Free;
  FInfoTotal.Free;

  inherited;
end;

{ TInfoTotal }

constructor TInfoTotal.Create(AOwner: TEvtTotal);
begin
  FRTom := TRTom.Create;
  FRPrest := TRPrest.Create;
  FRRecRepAD := TRRecRepADCollection.Create;
  FRComl := TRComl.Create;
  FRCPRB := TRCPRBCollection.Create(Self);
  FRRecEspetDest := TRRecEspetDest.Create;
end;

destructor TInfoTotal.Destroy;
begin
  FRTom.Free;
  FRPrest.Free;
  FRRecRepAD.Free;
  FRComl.Free;
  FRCPRB.Free;
  FRRecEspetDest.Free;

  inherited;
end;

procedure TInfoTotal.SetRCPRB(const Value: TRCPRBCollection);
begin
  FRCPRB := Value;
end;

procedure TInfoTotal.SetRRecRepAD(const Value: TRRecRepADCollection);
begin
  FRRecRepAD := Value;
end;

function TEvtTotal.LerXML: boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    XML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtTotal') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideContrib') <> '' then
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

      if leitor.rExtrai(2, 'infoTotal') <> '' then
      begin
        infoTotal.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

        if leitor.rExtrai(3, 'RTom') <> '' then
        begin
          infoTotal.RTom.cnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
          infoTotal.RTom.vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          infoTotal.RTom.vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          infoTotal.RTom.vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          infoTotal.RTom.vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          infoTotal.RTom.vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');
        end;

        if leitor.rExtrai(3, 'RPrest') <> '' then
        begin
          infoTotal.RPrest.tpInscTomador := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInscTomador'));
          infoTotal.RPrest.nrInscTomador := leitor.rCampo(tcStr, 'nrInscTomador');
          infoTotal.RPrest.vlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          infoTotal.RPrest.vlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          infoTotal.RPrest.vlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          infoTotal.RPrest.vlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          infoTotal.RPrest.vlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RRecRepAD', '', i + 1) <> '' do
        begin
          infoTotal.RRecRepAD.Add;
          infoTotal.RRecRepAD.Items[i].cnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
          infoTotal.RRecRepAD.Items[i].vlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
          infoTotal.RRecRepAD.Items[i].vlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
          infoTotal.RRecRepAD.Items[i].vlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');
          inc(i);
        end;

        if leitor.rExtrai(3, 'RComl') <> '' then
        begin
          infoTotal.RComl.vlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
          infoTotal.RComl.vlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
          infoTotal.RComl.vlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
          infoTotal.RComl.vlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
          infoTotal.RComl.vlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
          infoTotal.RComl.vlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RCPRB', '', i + 1) <> '' do
        begin
          infoTotal.RCPRB.Add;
          infoTotal.RCPRB.Items[i].codRec           := leitor.rCampo(tcInt, 'codRec');
          infoTotal.RCPRB.Items[i].vlrCPAPApurTotal := leitor.rCampo(tcDe2, 'vlrCPAPApurTotal');
          infoTotal.RCPRB.Items[i].vlrCPPRBSusp     := leitor.rCampo(tcDe2, 'vlrCPPRBSusp');
          inc(i);
        end;

        if leitor.rExtrai(3, 'RRecEspetDest') <> '' then
        begin
          infoTotal.RRecEspetDest.vlrReceitaTotal := leitor.rCampo(tcDe2, 'vlrReceitaTotal');
          infoTotal.RRecEspetDest.vlrCPApurTotal  := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          infoTotal.RRecEspetDest.vlrCPSuspTotal  := leitor.rCampo(tcDe2, 'vlrCPSuspTotal');
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
