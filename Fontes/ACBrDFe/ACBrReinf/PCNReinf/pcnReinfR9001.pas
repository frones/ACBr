{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit pcnReinfR9001;

interface

uses
  Classes, Sysutils, Contnrs, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf;

type

  TR9001 = class;
  TinfoTotal = class;
  TEvtTotal = class;
  TRRecRepADCollection = class;
  TRRecRepADCollectionItem = class;
  TRCPRBCollection = class;
  TRCPRBCollectionItem = class;
  TinfoCRTomCollection = class;
  TinfoCRTomCollectionItem = class;
  TRComlCollection = class;
  TRComlCollectionItem = class;

  TInfoRecEv = class(TObject)
  private
    FnrRecArqBase: String;
    FnrProtEntr: String;
    FdhProcess: TDateTime;
    FdhRecepcao: TDateTime;
    FtpEv: String;
    FidEv: String;
    Fhash: String;
  public
    property nrRecArqBase: String read FnrRecArqBase;
    property nrProtEntr: String read FnrProtEntr;
    property dhProcess: TDateTime read FdhProcess;
    property dhRecepcao: TDateTime read FdhRecepcao;
    property tpEv: String read FtpEv;
    property idEv: String read FidEv;
    property hash: String read Fhash;
  end;

  TRTom = class(TObject)
  private
    FcnpjPrestador: String;
    FvlrTotalBaseRet: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalNRetAdic: Double;
    FinfoCRTom: TinfoCRTomCollection;

    procedure SetinfoCRTom(const Value: TinfoCRTomCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjPrestador: String read FcnpjPrestador;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic;
    property infoCRTom: TinfoCRTomCollection read FinfoCRTom write SetinfoCRTom;
  end;

  TRPrest = class(TObject)
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

  TRComlCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRComlCollectionItem;
    procedure SetItem(Index: Integer; Value: TRComlCollectionItem);
  public
    function Add: TRComlCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRComlCollectionItem;

    property Items[Index: Integer]: TRComlCollectionItem read GetItem write SetItem;
  end;

  TRComlCollectionItem = class(TObject)
  private
    FvlrCPApur: Double;
    FvlrRatApur: Double;
    FvlrSenarApur: Double;
    FvlrCPSusp: Double;
    FvlrRatSusp: Double;
    FvlrSenarSusp: Double;
    FCRComl: String;
    FvlrCRComl: Double;
    FvlrCRComlSusp: Double;
  public
    property vlrCPApur: Double read FvlrCPApur;
    property vlrRatApur: Double read FvlrRatApur;
    property vlrSenarApur: Double read FvlrSenarApur;
    property vlrCPSusp: Double read FvlrCPSusp;
    property vlrRatSusp: Double read FvlrRatSusp;
    property vlrSenarSusp: Double read FvlrSenarSusp;
    property CRComl: String read FCRComl;
    property vlrCRComl: Double read FvlrCRComl;
    property vlrCRComlSusp: Double read FvlrCRComlSusp;
  end;

  TRRecEspetDesp = class(TObject)
  private
    FvlrReceitaTotal: Double;
    FvlrCPApurTotal: Double;
    FvlrCPSuspTotal: Double;
    FCRRecEspetDesp: String;
    FvlrCRRecEspetDesp: Double;
    FvlrCRRecEspetDespSusp: Double;
  public
    property vlrReceitaTotal: Double read FvlrReceitaTotal;
    property vlrCPApurTotal: Double read FvlrCPApurTotal;
    property vlrCPSuspTotal: Double read FvlrCPSuspTotal;
    property CRRecEspetDesp: String read FCRRecEspetDesp;
    property vlrCRRecEspetDesp: Double read FvlrCRRecEspetDesp;
    property vlrCRRecEspetDespSusp: Double read FvlrCRRecEspetDespSusp;
  end;

  TInfoTotal = class(TObject)
  private
    FnrRecArqBase: String;
    FRTom: TRTom;
    FRPrest: TRPrest;
    FRRecRepAD: TRRecRepADCollection;
    FRComl: TRComlCollection;
    FRCPRB: TRCPRBCollection;
    FRRecEspetDesp: TRRecEspetDesp;

    procedure SetRRecRepAD(const Value: TRRecRepADCollection);
    procedure SetRCPRB(const Value: TRCPRBCollection);
    procedure SetRComl(const Value: TRComlCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property RTom: TRTom read FRTom write FRTom;
    property RPrest: TRPrest read FRPrest write FRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD write SetRRecRepAD;
    property RComl: TRComlCollection read FRComl write SetRComl;
    property RCPRB: TRCPRBCollection read FRCPRB write SetRCPRB;
    property RRecEspetDesp: TRRecEspetDesp read FRRecEspetDesp write FRRecEspetDesp;
  end;

  TR9001 = class(TInterfacedObject, IEventoReinf)
  private
    FTipoEvento: TTipoEvento;
    FEvtTotal: TEvtTotal;

    function GetXml: string;
    procedure SetXml(const Value: string);
    function GetTipoEvento: TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento: TObject;

    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtTotal: TEvtTotal read FEvtTotal write FEvtTotal;
  end;

  TRRecRepADCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRRecRepADCollectionItem;
    procedure SetItem(Index: Integer; Value: TRRecRepADCollectionItem);
  public
    function Add: TRRecRepADCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRRecRepADCollectionItem;

    property Items[Index: Integer]: TRRecRepADCollectionItem read GetItem write SetItem;
  end;

  TRRecRepADCollectionItem = class(TObject)
  private
    FcnpjAssocDesp: string;
    FvlrTotalRep: Double;
    FvlrTotalRet: Double;
    FvlrTotalNRet: Double;
    FCRRecRepAD: String;
    FvlrCRRecRepAD: Double;
    FvlrCRRecRepADSusp: Double;
  public
    property cnpjAssocDesp: string read FcnpjAssocDesp;
    property vlrTotalRep: Double read FvlrTotalRep;
    property vlrTotalRet: Double read FvlrTotalRet;
    property vlrTotalNRet: Double read FvlrTotalNRet;
    property CRRecRepAD: String read FCRRecRepAD;
    property vlrCRRecRepAD: Double read FvlrCRRecRepAD;
    property vlrCRRecRepADSusp: Double read FvlrCRRecRepADSusp;
  end;

  TRCPRBCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRCPRBCollectionItem;
    procedure SetItem(Index: Integer; Value: TRCPRBCollectionItem);
  public
    function Add: TRCPRBCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRCPRBCollectionItem;

    property Items[Index: Integer]: TRCPRBCollectionItem read GetItem write SetItem;
  end;

  TRCPRBCollectionItem = class(TObject)
  private
    FcodRec: Integer;
    FvlrCPApurTotal: Double;
    FvlrCPRBSusp: Double;
    FCRCPRB: String;
    FvlrCRCPRB: Double;
    FvlrCRCPRBSusp: Double;
  public
    property codRec: Integer read FcodRec;
    property vlrCPApurTotal: Double read FvlrCPApurTotal;
    property vlrCPRBSusp: Double read FvlrCPRBSusp;
    property CRCPRB: String read FCRCPRB;
    property vlrCRCPRB: Double read FvlrCRCPRB;
    property vlrCRCPRBSusp: Double read FvlrCRCPRBSusp;
  end;

  TinfoCRTomCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TinfoCRTomCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoCRTomCollectionItem);
  public
    function Add: TinfoCRTomCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoCRTomCollectionItem;

    property Items[Index: Integer]: TinfoCRTomCollectionItem read GetItem write SetItem;
  end;

  TinfoCRTomCollectionItem = class(TObject)
  private
    FCRTom: string;
    FVlrCRTom: Double;
    FVlrCRTomSusp: Double;
  public
    property CRTom: string read FCRTom;
    property VlrCRTom: Double read FVlrCRTom;
    property VlrCRTomSusp: Double read FVlrCRTomSusp;
  end;

  TEvtTotal = class(TObject)
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

    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  pcnConversao, DateUtils;

{ TR9001 }

constructor TR9001.Create;
begin
  FTipoEvento := teR9001;
  FEvtTotal := TEvtTotal.Create;
end;

destructor TR9001.Destroy;
begin
  FEvtTotal.Free;

  inherited;
end;

function TR9001.GetXml : string;
begin
  Result := FEvtTotal.XML;
end;

procedure TR9001.SetXml(const Value: string);
begin
  if Value = FEvtTotal.XML then Exit;

  FEvtTotal.FXML := Value;
  FEvtTotal.Leitor.Arquivo := Value;
  FEvtTotal.LerXML;
end;

function TR9001.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

function TR9001.GetEvento: TObject;
begin
  Result := Self;
end;

{ TRRecRepADCollection }

function TRRecRepADCollection.Add: TRRecRepADCollectionItem;
begin
  Result := Self.New;
end;

function TRRecRepADCollection.GetItem(
  Index: Integer): TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem(inherited GetItem(Index));
end;

function TRRecRepADCollection.New: TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRRecRepADCollection.SetItem(Index: Integer;
  Value: TRRecRepADCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRCPRBCollection }

function TRCPRBCollection.Add: TRCPRBCollectionItem;
begin
  Result := Self.New;
end;

function TRCPRBCollection.GetItem(
  Index: Integer): TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited GetItem(Index));
end;

function TRCPRBCollection.New: TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRCPRBCollection.SetItem(Index: Integer;
  Value: TRCPRBCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfoCRTomCollection }

function TinfoCRTomCollection.Add: TinfoCRTomCollectionItem;
begin
  Result := Self.New;
end;

function TinfoCRTomCollection.GetItem(
  Index: Integer): TinfoCRTomCollectionItem;
begin
  Result := TinfoCRTomCollectionItem(inherited GetItem(Index));
end;

function TinfoCRTomCollection.New: TinfoCRTomCollectionItem;
begin
  Result := TinfoCRTomCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoCRTomCollection.SetItem(Index: Integer;
  Value: TinfoCRTomCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRComlCollection }

function TRComlCollection.Add: TRComlCollectionItem;
begin
  Result := Self.New;
end;

function TRComlCollection.GetItem(Index: Integer): TRComlCollectionItem;
begin
  Result := TRComlCollectionItem(inherited GetItem(Index));
end;

function TRComlCollection.New: TRComlCollectionItem;
begin
  Result := TRComlCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRComlCollection.SetItem(Index: Integer; Value: TRComlCollectionItem);
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
  FInfoTotal  := TInfoTotal.Create;
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

constructor TInfoTotal.Create;
begin
  inherited;
  FRTom          := TRTom.Create;
  FRPrest        := TRPrest.Create;
  FRRecRepAD     := TRRecRepADCollection.Create;
  FRComl         := TRComlCollection.Create;
  FRCPRB         := TRCPRBCollection.Create;
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

procedure TInfoTotal.SetRComl(const Value: TRComlCollection);
begin
  FRComl := Value;
end;

procedure TInfoTotal.SetRCPRB(const Value: TRCPRBCollection);
begin
  FRCPRB := Value;
end;

procedure TInfoTotal.SetRRecRepAD(const Value: TRRecRepADCollection);
begin
  FRRecRepAD := Value;
end;

{ TRTom }

constructor TRTom.Create;
begin
  inherited Create;
  FinfoCRTom := TinfoCRTomCollection.Create;
end;

destructor TRTom.Destroy;
begin
  FinfoCRTom.Free;

  inherited;
end;

procedure TRTom.SetinfoCRTom(const Value: TinfoCRTomCollection);
begin
  FinfoCRTom := Value;
end;

function TEvtTotal.LerXML: boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := True;
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
            IdeStatus.regOcorrs.New;
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
        infoRecEv.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoRecEv.FnrProtEntr := leitor.rCampo(tcStr, 'nrProtEntr');
        infoRecEv.FdhProcess  := leitor.rCampo(tcDatHor, 'dhProcess');
        infoRecEv.FdhRecepcao := leitor.rCampo(tcDatHor, 'dhRecepcao');
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

          // Versão 1.03.02
          i := 0;
          while Leitor.rExtrai(4, 'infoCRTom', '', i + 1) <> '' do
          begin
            infoTotal.RTom.infoCRTom.New;
            infoTotal.RTom.infoCRTom.Items[i].FCRTom        := leitor.rCampo(tcStr, 'CRTom');
            infoTotal.RTom.infoCRTom.Items[i].FVlrCRTom     := leitor.rCampo(tcDe2, 'VlrCRTom');
            infoTotal.RTom.infoCRTom.Items[i].FVlrCRTomSusp := leitor.rCampo(tcDe2, 'VlrCRTomSusp');

            inc(i);
          end;
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
          infoTotal.RRecRepAD.New;
          infoTotal.RRecRepAD.Items[i].FcnpjAssocDesp     := leitor.rCampo(tcStr, 'cnpjAssocDesp');
          infoTotal.RRecRepAD.Items[i].FvlrTotalRep       := leitor.rCampo(tcDe2, 'vlrTotalRep');
          infoTotal.RRecRepAD.Items[i].FvlrTotalRet       := leitor.rCampo(tcDe2, 'vlrTotalRet');
          infoTotal.RRecRepAD.Items[i].FvlrTotalNRet      := leitor.rCampo(tcDe2, 'vlrTotalNRet');

          // Versão 1.03.02
          infoTotal.RRecRepAD.Items[i].FCRRecRepAD        := leitor.rCampo(tcStr, 'CRRecRepAD');
          infoTotal.RRecRepAD.Items[i].FvlrCRRecRepAD     := leitor.rCampo(tcDe2, 'vlrCRRecRepAD');
          infoTotal.RRecRepAD.Items[i].FvlrCRRecRepADSusp := leitor.rCampo(tcDe2, 'vlrCRRecRepADSusp');

          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RComl', '', i + 1) <> '' do
        begin
          infoTotal.RComl.New;
          infoTotal.RComl.Items[i].FvlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
          infoTotal.RComl.Items[i].FvlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
          infoTotal.RComl.Items[i].FvlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
          infoTotal.RComl.Items[i].FvlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
          infoTotal.RComl.Items[i].FvlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
          infoTotal.RComl.Items[i].FvlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');

          // Versão 1.03.02
          infoTotal.RComl.Items[i].FCRComl        := leitor.rCampo(tcStr, 'CRComl');
          infoTotal.RComl.Items[i].FvlrCRComl     := leitor.rCampo(tcDe2, 'vlrCRComl');
          infoTotal.RComl.Items[i].FvlrCRComlSusp := leitor.rCampo(tcDe2, 'vlrCRComlSusp');

          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'RCPRB', '', i + 1) <> '' do
        begin
          infoTotal.RCPRB.New;
          infoTotal.RCPRB.Items[i].FcodRec         := leitor.rCampo(tcInt, 'codRec');
          infoTotal.RCPRB.Items[i].FvlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          infoTotal.RCPRB.Items[i].FvlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');

          // Versão 1.03.02
          infoTotal.RCPRB.Items[i].FCRCPRB        := leitor.rCampo(tcStr, 'CRCPRB');
          infoTotal.RCPRB.Items[i].FvlrCRCPRB     := leitor.rCampo(tcDe2, 'vlrCRCPRB');
          infoTotal.RCPRB.Items[i].FvlrCRCPRBSusp := leitor.rCampo(tcDe2, 'vlrCRCPRBSusp');

          inc(i);
        end;

        if leitor.rExtrai(3, 'RRecEspetDesp') <> '' then
        begin
          infoTotal.RRecEspetDesp.FvlrReceitaTotal := leitor.rCampo(tcDe2, 'vlrReceitaTotal');
          infoTotal.RRecEspetDesp.FvlrCPApurTotal  := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
          infoTotal.RRecEspetDesp.FvlrCPSuspTotal  := leitor.rCampo(tcDe2, 'vlrCPSuspTotal');

          // Versão 1.03.02
          infoTotal.RRecEspetDesp.FCRRecEspetDesp        := leitor.rCampo(tcStr, 'CRRecEspetDesp');
          infoTotal.RRecEspetDesp.FvlrCRRecEspetDesp     := leitor.rCampo(tcDe2, 'vlrCRRecEspetDesp');
          infoTotal.RRecEspetDesp.FvlrCRRecEspetDespSusp := leitor.rCampo(tcDe2, 'vlrCRRecEspetDespSusp');
        end;
      end;
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
  Result := True;

  AIni := TMemIniFile.Create('');
  try
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
      AIni.WriteString(sSecao, 'nrRecArqBase', infoRecEv.nrRecArqBase);
      AIni.WriteString(sSecao, 'nrProtEntr', infoRecEv.nrProtEntr);
      AIni.WriteString(sSecao, 'dhProcess',  DateToStr(infoRecEv.dhProcess));
      AIni.WriteString(sSecao, 'dhRecepcao', DateToStr(infoRecEv.dhRecepcao));
      AIni.WriteString(sSecao, 'tpEv',       infoRecEv.tpEv);
      AIni.WriteString(sSecao, 'idEv',       infoRecEv.idEv);
      AIni.WriteString(sSecao, 'hash',       infoRecEv.hash);

      with InfoTotal do
      begin
        sSecao := 'infoTotal';
        AIni.WriteString(sSecao, 'nrRecArqBase', nrRecArqBase);

        sSecao := 'RTom';
        AIni.WriteString(sSecao, 'cnpjPrestador',    RTom.cnpjPrestador);
        AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RTom.vlrTotalBaseRet);
        AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RTom.vlrTotalRetPrinc);
        AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RTom.vlrTotalRetAdic);
        AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RTom.vlrTotalNRetPrinc);
        AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RTom.vlrTotalNRetAdic);

        // Versão 1.03.02
        for i := 0 to RTom.infoCRTom.Count -1 do
        begin
          sSecao := 'infoCRTom' + IntToStrZero(I, 1);

          AIni.WriteString(sSecao, 'CRTom',       RTom.infoCRTom.Items[i].CRTom);
          AIni.WriteFloat(sSecao, 'VlrCRTom',     RTom.infoCRTom.Items[i].VlrCRTom);
          AIni.WriteFloat(sSecao, 'VlrCRTomSusp', RTom.infoCRTom.Items[i].VlrCRTomSusp);
        end;

        sSecao := 'RPrest';
        AIni.WriteString(sSecao, 'tpInscTomador',    TpInscricaoToStr(RPrest.tpInscTomador));
        AIni.WriteString(sSecao, 'nrInscTomador',    RPrest.nrInscTomador);
        AIni.WriteFloat(sSecao, 'vlrTotalBaseRet',   RPrest.vlrTotalBaseRet);
        AIni.WriteFloat(sSecao, 'vlrTotalRetPrinc',  RPrest.vlrTotalRetPrinc);
        AIni.WriteFloat(sSecao, 'vlrTotalRetAdic',   RPrest.vlrTotalRetAdic);
        AIni.WriteFloat(sSecao, 'vlrTotalNRetPrinc', RPrest.vlrTotalNRetPrinc);
        AIni.WriteFloat(sSecao, 'vlrTotalNRetAdic',  RPrest.vlrTotalNRetAdic);

        for i := 0 to RRecRepAD.Count -1 do
        begin
          sSecao := 'RRecRepAD' + IntToStrZero(I, 3);

          AIni.WriteString(sSecao, 'cnpjAssocDesp', RRecRepAD.Items[i].cnpjAssocDesp);
          AIni.WriteFloat(sSecao, 'vlrTotalRep',    RRecRepAD.Items[i].vlrTotalRep);
          AIni.WriteFloat(sSecao, 'vlrTotalRet',    RRecRepAD.Items[i].vlrTotalRet);
          AIni.WriteFloat(sSecao, 'vlrTotalNRet',   RRecRepAD.Items[i].vlrTotalNRet);

          // Versão 1.03.02
          AIni.WriteString(sSecao, 'CRRecRepAD',       RRecRepAD.Items[i].CRRecRepAD);
          AIni.WriteFloat(sSecao, 'vlrCRRecRepAD',     RRecRepAD.Items[i].vlrCRRecRepAD);
          AIni.WriteFloat(sSecao, 'vlrCRRecRepADSusp', RRecRepAD.Items[i].vlrCRRecRepADSusp);
        end;

        for i := 0 to RComl.Count -1 do
        begin
          sSecao := 'RComl' + IntToStrZero(I, 1);

          AIni.WriteFloat(sSecao, 'vlrCPApur',    RComl.Items[i].vlrCPApur);
          AIni.WriteFloat(sSecao, 'vlrRatApur',   RComl.Items[i].vlrRatApur);
          AIni.WriteFloat(sSecao, 'vlrSenarApur', RComl.Items[i].vlrSenarApur);
          AIni.WriteFloat(sSecao, 'vlrCPSusp',    RComl.Items[i].vlrCPSusp);
          AIni.WriteFloat(sSecao, 'vlrRatSusp',   RComl.Items[i].vlrRatSusp);
          AIni.WriteFloat(sSecao, 'vlrSenarSusp', RComl.Items[i].vlrSenarSusp);

          // Versão 1.03.02
          AIni.WriteString(sSecao, 'CRComl',       RComl.Items[i].CRComl);
          AIni.WriteFloat(sSecao, 'vlrCRComl',     RComl.Items[i].vlrCRComl);
          AIni.WriteFloat(sSecao, 'vlrCRComlSusp', RComl.Items[i].vlrCRComlSusp);
        end;

        for i := 0 to RCPRB.Count -1 do
        begin
          sSecao := 'RCPRB' + IntToStrZero(I, 1);

          AIni.WriteInteger(sSecao, 'codRec',       RCPRB.Items[i].codRec);
          AIni.WriteFloat(sSecao, 'vlrCPApurTotal', RCPRB.Items[i].vlrCPApurTotal);
          AIni.WriteFloat(sSecao, 'vlrCPRBSusp',    RCPRB.Items[i].vlrCPRBSusp);

          // Versão 1.03.02
          AIni.WriteString(sSecao, 'CRCPRB',       RCPRB.Items[i].CRCPRB);
          AIni.WriteFloat(sSecao, 'vlrCRCPRB',     RCPRB.Items[i].vlrCRCPRB);
          AIni.WriteFloat(sSecao, 'vlrCRCPRBSusp', RCPRB.Items[i].vlrCRCPRBSusp);
        end;

        sSecao := 'RRecEspetDesp';
        AIni.WriteFloat(sSecao, 'vlrReceitaTotal', RRecEspetDesp.vlrReceitaTotal);
        AIni.WriteFloat(sSecao, 'vlrCPApurTotal',  RRecEspetDesp.vlrCPApurTotal);
        AIni.WriteFloat(sSecao, 'vlrCPSuspTotal',  RRecEspetDesp.vlrCPSuspTotal);

        // Versão 1.03.02
        AIni.WriteString(sSecao, 'CRRecEspetDesp',       RRecEspetDesp.CRRecEspetDesp);
        AIni.WriteFloat(sSecao, 'vlrCRRecEspetDesp',     RRecEspetDesp.vlrCRRecEspetDesp);
        AIni.WriteFloat(sSecao, 'vlrCRRecEspetDespSusp', RRecEspetDesp.vlrCRRecEspetDespSusp);
      end;
    end;
  finally
    AIni.Free;
  end;
end;

end.
