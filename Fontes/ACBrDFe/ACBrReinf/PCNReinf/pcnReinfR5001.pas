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

unit pcnReinfR5001;

interface

uses
  Classes, Sysutils, pcnGerador, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf;

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
    property nrProtEntr: String read FnrProtEntr;
    property dhProcess: TDateTime read FdhProcess;
    property tpEv: String read FtpEv;
    property idEv: String read FidEv;
    property hash: String read Fhash;
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
    property cnpjPrestador: String read FcnpjPrestador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic;
  end;

  TRPrest = class(TPersistent)
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

  TRRecEspetDesp = class(TPersistent)
  private
    FvlrReceitaTotal: Double;
    FvlrCPApurTotal: Double;
    FvlrCPSuspTotal: Double;
  public
    property vlrReceitaTotal: Double read FvlrReceitaTotal;
    property vlrCPApurTotal: Double read FvlrCPApurTotal;
    property vlrCPSuspTotal: Double read FvlrCPSuspTotal;
  end;

  TInfoTotal = class(TPersistent)
  private
    FnrRecArqBase: String;
    FRTom: TRTom;
    FRPrest: TRPrest;
    FRRecRepAD: TRRecRepADCollection;
    FRComl: TRComl;
    FRCPRB: TRCPRBCollection;
    FRRecEspetDesp: TRRecEspetDesp;

    procedure SetRRecRepAD(const Value: TRRecRepADCollection);
    procedure SetRCPRB(const Value: TRCPRBCollection);
  public
    constructor Create(AOwner: TEvtTotal);
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property RTom: TRTom read FRTom write FRTom;
    property RPrest: TRPrest read FRPrest write FRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD write SetRRecRepAD;
    property RComl: TRComl read FRComl write FRComl;
    property RCPRB: TRCPRBCollection read FRCPRB write SetRCPRB;
    property RRecEspetDesp: TRRecEspetDesp read FRRecEspetDesp write FRRecEspetDesp;
  end;

  TR5001 = class(TInterfacedObject, IEventoReinf)
  private
    FTipoEvento: TTipoEvento;
    FEvtTotal: TEvtTotal;

    function GetXml: string;
    procedure SetXml(const Value: string);
    function GetTipoEvento: TTipoEvento;
    procedure SetEvtTotal(const Value: TEvtTotal);
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento: TObject;
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
    property cnpjAssocDesp: string read FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet;
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
    FvlrCPApurTotal: Double;
    FvlrCPRBSusp: Double;
  public
    property codRec: Integer read FcodRec;
    property vlrCPApurTotal: Double read FvlrCPApurTotal;
    property vlrCPRBSusp: Double read FvlrCPRBSusp;
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
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotal: TInfoTotal read FInfoTotal write FInfoTotal;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

implementation

uses
  IniFiles, pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

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

  FEvtTotal.FXML := Value;
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

function TR5001.GetEvento: TObject;
begin
  Result := Self;
end;

{ TRRecRepADCollection }

function TRRecRepADCollection.Add: TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem(inherited Add);
//  Result.Create;
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
//  Result.Create;
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

  FIdeEvento  := TIdeEvento1.Create;
  FIdeContrib := TIdeContrib.Create;
  FIdeStatus  := TIdeStatus.Create;
  FInfoRecEv  := TInfoRecEv.Create;
  FInfoTotal  := TInfoTotal.Create(Self);
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
  FRTom          := TRTom.Create;
  FRPrest        := TRPrest.Create;
  FRRecRepAD     := TRRecRepADCollection.Create;
  FRComl         := TRComl.Create;
  FRCPRB         := TRCPRBCollection.Create(Self);
  FRRecEspetDesp := TRRecEspetDesp.Create;
end;

destructor TInfoTotal.Destroy;
begin
  FRTom.Free;
  FRPrest.Free;
  FRRecRepAD.Free;
  FRComl.Free;
  FRCPRB.Free;
  FRRecEspetDesp.Free;

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
    FXML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtTotal') <> '' then
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

      if leitor.rExtrai(2, 'infoTotal') <> '' then
      begin
        infoTotal.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

        if leitor.rExtrai(3, 'RTom') <> '' then
        begin
          infoTotal.RTom.FcnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
          infoTotal.RTom.FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          infoTotal.RTom.FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          infoTotal.RTom.FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          infoTotal.RTom.FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          infoTotal.RTom.FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');
        end;

        if leitor.rExtrai(3, 'RPrest') <> '' then
        begin
          infoTotal.RPrest.FtpInscTomador := StrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInscTomador'));
          infoTotal.RPrest.FnrInscTomador := leitor.rCampo(tcStr, 'nrInscTomador');
          infoTotal.RPrest.FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
          infoTotal.RPrest.FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
          infoTotal.RPrest.FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
          infoTotal.RPrest.FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
          infoTotal.RPrest.FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RRecRepAD', '', i + 1) <> '' do
        begin
          infoTotal.RRecRepAD.Add;
          infoTotal.RRecRepAD.Items[i].FcnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
          infoTotal.RRecRepAD.Items[i].FvlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
          infoTotal.RRecRepAD.Items[i].FvlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
          infoTotal.RRecRepAD.Items[i].FvlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');
          inc(i);
        end;

        if leitor.rExtrai(3, 'RComl') <> '' then
        begin
          infoTotal.RComl.FvlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
          infoTotal.RComl.FvlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
          infoTotal.RComl.FvlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
          infoTotal.RComl.FvlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
          infoTotal.RComl.FvlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
          infoTotal.RComl.FvlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RCPRB', '', i + 1) <> '' do
        begin
          infoTotal.RCPRB.Add;
          infoTotal.RCPRB.Items[i].FcodRec         := leitor.rCampo(tcInt, 'codRec');
          infoTotal.RCPRB.Items[i].FvlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          infoTotal.RCPRB.Items[i].FvlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');
          inc(i);
        end;

        if leitor.rExtrai(3, 'RRecEspetDesp') <> '' then
        begin
          infoTotal.RRecEspetDesp.FvlrReceitaTotal := leitor.rCampo(tcDe2, 'vlrReceitaTotal');
          infoTotal.RRecEspetDesp.FvlrCPApurTotal  := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          infoTotal.RRecEspetDesp.FvlrCPSuspTotal  := leitor.rCampo(tcDe2, 'vlrCPSuspTotal');
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtTotal.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i: Integer;
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

      sSecao := 'infoTotal';
      AIni.WriteString(sSecao, 'nrRecArqBase', infoTotal.nrRecArqBase);

      sSecao := 'RTom';
      AIni.WriteString(sSecao, 'cnpjPrestador',    infoTotal.RTom.cnpjPrestador);
      AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   infoTotal.RTom.vlrTotalBaseRet);
      AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  infoTotal.RTom.vlrTotalRetPrinc);
      AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   infoTotal.RTom.vlrTotalRetAdic);
      AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', infoTotal.RTom.vlrTotalNRetPrinc);
      AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  infoTotal.RTom.vlrTotalNRetAdic);

      sSecao := 'RPrest';
      AIni.WriteString(sSecao, 'tpInscTomador',    TpInscricaoToStr(infoTotal.RPrest.tpInscTomador));
      AIni.WriteString(sSecao, 'nrInscTomador',    infoTotal.RPrest.nrInscTomador);
      AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   infoTotal.RPrest.vlrTotalBaseRet);
      AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  infoTotal.RPrest.vlrTotalRetPrinc);
      AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   infoTotal.RPrest.vlrTotalRetAdic);
      AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', infoTotal.RPrest.vlrTotalNRetPrinc);
      AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  infoTotal.RPrest.vlrTotalNRetAdic);

      for i := 0 to InfoTotal.RRecRepAD.Count -1 do
      begin
        sSecao := 'RRecRepAD' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'cnpjAssocDesp', InfoTotal.RRecRepAD.Items[i].cnpjAssocDesp);
        AIni.WriteFloat(sSecao, 'vlrTotalRep',    InfoTotal.RRecRepAD.Items[i].vlrTotalRep);
        AIni.WriteFloat(sSecao, 'vlrTotalRet',    InfoTotal.RRecRepAD.Items[i].vlrTotalRet);
        AIni.WriteFloat(sSecao, 'vlrTotalNRet',   InfoTotal.RRecRepAD.Items[i].vlrTotalNRet);
      end;

      sSecao := 'RComl';
      AIni.WriteFloat(sSecao, 'vlrCPApur',    infoTotal.RComl.vlrCPApur);
      AIni.WriteFloat(sSecao, 'vlrRatApur',   infoTotal.RComl.vlrRatApur);
      AIni.WriteFloat(sSecao, 'vlrSenarApur', infoTotal.RComl.vlrSenarApur);
      AIni.WriteFloat(sSecao, 'vlrCPSusp',    infoTotal.RComl.vlrCPSusp);
      AIni.WriteFloat(sSecao, 'vlrRatSusp',   infoTotal.RComl.vlrRatSusp);
      AIni.WriteFloat(sSecao, 'vlrSenarSusp', infoTotal.RComl.vlrSenarSusp);

      for i := 0 to InfoTotal.RCPRB.Count -1 do
      begin
        sSecao := 'RCPRB' + IntToStrZero(I, 1);

        AIni.WriteInteger(sSecao, 'codRec',       InfoTotal.RCPRB.Items[i].codRec);
        AIni.WriteFloat(sSecao, 'vlrCPApurTotal', InfoTotal.RCPRB.Items[i].vlrCPApurTotal);
        AIni.WriteFloat(sSecao, 'vlrCPRBSusp',    InfoTotal.RCPRB.Items[i].vlrCPRBSusp);
      end;

      sSecao := 'RRecEspetDesp';
      AIni.WriteFloat(sSecao, 'vlrReceitaTotal', infoTotal.RRecEspetDesp.vlrReceitaTotal);
      AIni.WriteFloat(sSecao, 'vlrCPApurTotal',  infoTotal.RRecEspetDesp.vlrCPApurTotal);
      AIni.WriteFloat(sSecao, 'vlrCPSuspTotal',  infoTotal.RRecEspetDesp.vlrCPSuspTotal);
    end;
  finally
    AIni.Free;
  end;
end;

end.
