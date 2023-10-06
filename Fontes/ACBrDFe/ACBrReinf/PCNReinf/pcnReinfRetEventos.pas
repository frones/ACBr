{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit pcnReinfRetEventos;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf,
  pcnReinfR9005;

type
  TRetEnvioLote = class;
  TeventoCollection = class;
  TeventoCollectionItem = class;
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

  TRComlCollection = class(TACBrObjectList)
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

  TRAquisCollectionItem = class(TObject)
  private
    FCRAquis: String;
    FvlrCRAquis: Double;
    FvlrCRAquisSusp: Double;
  public
    property CRAquis: String read FCRAquis;
    property vlrCRAquis: Double read FvlrCRAquis;
    property vlrCRAquisSusp: Double read FvlrCRAquisSusp;
  end;
    
  TRAquisCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRAquisCollectionItem;
    procedure SetItem(Index: Integer; Value: TRAquisCollectionItem);
  public
    function Add: TRAquisCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRAquisCollectionItem;

    property Items[Index: Integer]: TRAquisCollectionItem read GetItem write SetItem;
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
    FRAquis: TRAquisCollection;
    FRCPRB: TRCPRBCollection;
    FRRecEspetDesp: TRRecEspetDesp;

    procedure SetRRecRepAD(const Value: TRRecRepADCollection);
    procedure SetRCPRB(const Value: TRCPRBCollection);
    procedure SetRComl(const Value: TRComlCollection);
    procedure SetRAquis(const Value: TRAquisCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property RTom: TRTom read FRTom write FRTom;
    property RPrest: TRPrest read FRPrest write FRPrest;
    property RRecRepAD: TRRecRepADCollection read FRRecRepAD write SetRRecRepAD;
    property RComl: TRComlCollection read FRComl write SetRComl;
    property RAquis: TRAquisCollection read FRAquis write SetRAquis;
    property RCPRB: TRCPRBCollection read FRCPRB write SetRCPRB;
    property RRecEspetDesp: TRRecEspetDesp read FRRecEspetDesp write FRRecEspetDesp;
  end;

  TRRecRepADCollection = class(TACBrObjectList)
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

  TRCPRBCollection = class(TACBrObjectList)
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

  TinfoCRTomCollection = class(TACBrObjectList)
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

    property IdeEvento: TIdeEvento1 read FIdeEvento write FIdeEvento;
    property IdeContrib: TIdeContrib read FIdeContrib write FIdeContrib;
    property IdeStatus: TIdeStatus read FIdeStatus write FIdeStatus;
    property InfoRecEv: TInfoRecEv read FInfoRecEv write FInfoRecEv;
    property InfoTotal: TInfoTotal read FInfoTotal write FInfoTotal;

    property Id: String  read FId;
    property XML: String read FXML;
  end;

  { TeventoCollection }
  TeventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TeventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TeventoCollectionItem);
  public
    function Add: TeventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TeventoCollectionItem;

    property Items[Index: Integer]: TeventoCollectionItem read GetItem write SetItem;
  end;

  TeventoCollectionItem = class(TObject)
  private
    FId: string;
    FArquivoReinf: string;
    FevtTotal: TEvtTotal;
    FevtRet: TevtRet;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: string read FId write FId;
    property ArquivoReinf: string read FArquivoReinf write FArquivoReinf;
    property evtTotal: TEvtTotal read FevtTotal write FevtTotal;
    property evtRet: TevtRet read FevtRet write FevtRet;
  end;

  { TdadosRecepcaoLote }

  TdadosRecepcaoLote = class(TObject)
  private
    FdhRecepcao: TDateTime;
    FversaoAplicativoRecepcao: String;
    FprotocoloEnvio: String;
  public
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicativoRecepcao: String read FversaoAplicativoRecepcao write FversaoAplicativoRecepcao;
    property protocoloEnvio: String read FprotocoloEnvio write FprotocoloEnvio;
  end;

  { TdadosProcessamentoLote }

  TdadosProcessamentoLote = class(TObject)
  private
    FversaoAplicativoProcessamentoLote: String;
  public
    property versaoAplicativoProcessamentoLote: String read FversaoAplicativoProcessamentoLote write FversaoAplicativoProcessamentoLote;
  end;

  { TRetEnvioLote }

  TRetEnvioLote = class(TObject)
  private
    FLeitor: TLeitor;

    FId: String;
    FIdeTransmissor: TIdeTransmissor;
    FStatus: TStatus;
    Fevento: TeventoCollection;
    FIdeContribuinte: TideContrib;
    FdadosRecepcaoLote: TdadosRecepcaoLote;
    FdadosProcessamentoLote: TdadosProcessamentoLote;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
    function LerXmlRetornadoWebService: boolean;
    function LerXmlRetornadoAPIRest: boolean;

    procedure LerRetornoEventos;

    function SalvarINI(nCont: Integer): boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;

    property Id: String read FId write FId;
    property IdeTransmissor: TIdeTransmissor read FIdeTransmissor write FIdeTransmissor;
    property IdeContribuinte: TideContrib read FIdeContribuinte write FIdeContribuinte;
    property Status: TStatus read FStatus write FStatus;
    property dadosRecepcaoLote: TdadosRecepcaoLote read FdadosRecepcaoLote write FdadosRecepcaoLote;
    property dadosProcessamentoLote: TdadosProcessamentoLote read FdadosProcessamentoLote write FdadosProcessamentoLote;
    property evento: TeventoCollection read Fevento write Fevento;
  end;

implementation

uses
  IniFiles, ACBrUtil.Base, ACBrUtil.Strings, DateUtils;

{ TRRecRepADCollection }

function TRRecRepADCollection.Add: TRRecRepADCollectionItem;
begin
  Result := Self.New;
end;

function TRRecRepADCollection.GetItem(
  Index: Integer): TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem(inherited Items[Index]);
end;

function TRRecRepADCollection.New: TRRecRepADCollectionItem;
begin
  Result := TRRecRepADCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRRecRepADCollection.SetItem(Index: Integer;
  Value: TRRecRepADCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRCPRBCollection }

function TRCPRBCollection.Add: TRCPRBCollectionItem;
begin
  Result := Self.New;
end;

function TRCPRBCollection.GetItem(
  Index: Integer): TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem(inherited Items[Index]);
end;

function TRCPRBCollection.New: TRCPRBCollectionItem;
begin
  Result := TRCPRBCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRCPRBCollection.SetItem(Index: Integer;
  Value: TRCPRBCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoCRTomCollection }

function TinfoCRTomCollection.Add: TinfoCRTomCollectionItem;
begin
  Result := Self.New;
end;

function TinfoCRTomCollection.GetItem(
  Index: Integer): TinfoCRTomCollectionItem;
begin
  Result := TinfoCRTomCollectionItem(inherited Items[Index]);
end;

function TinfoCRTomCollection.New: TinfoCRTomCollectionItem;
begin
  Result := TinfoCRTomCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoCRTomCollection.SetItem(Index: Integer;
  Value: TinfoCRTomCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRComlCollection }

function TRComlCollection.Add: TRComlCollectionItem;
begin
  Result := Self.New;
end;

function TRComlCollection.GetItem(Index: Integer): TRComlCollectionItem;
begin
  Result := TRComlCollectionItem(inherited Items[Index]);
end;

function TRComlCollection.New: TRComlCollectionItem;
begin
  Result := TRComlCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRComlCollection.SetItem(Index: Integer; Value: TRComlCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRAquisCollection }

function TRAquisCollection.Add: TRAquisCollectionItem;
begin
  Result := Self.New;
end;

function TRAquisCollection.GetItem(Index: Integer): TRAquisCollectionItem;
begin
  Result := TRAquisCollectionItem(inherited Items[Index]);
end;

function TRAquisCollection.New: TRAquisCollectionItem;
begin
  Result := TRAquisCollectionItem.Create;
  Self.Add(Result);
end;

procedure TRAquisCollection.SetItem(Index: Integer; Value: TRAquisCollectionItem);
begin
  inherited Items[Index] := Value;
end;


{ TInfoTotal }

constructor TInfoTotal.Create;
begin
  FRTom          := TRTom.Create;
  FRPrest        := TRPrest.Create;
  FRRecRepAD     := TRRecRepADCollection.Create;
  FRComl         := TRComlCollection.Create;
  FRAquis        := TRAquisCollection.Create;
  FRCPRB         := TRCPRBCollection.Create;
  FRRecEspetDesp := TRRecEspetDesp.Create;
end;

destructor TInfoTotal.Destroy;
begin
  FRTom.Free;
  FRPrest.Free;
  FRRecRepAD.Free;
  FRComl.Free;
  FRAquis.Free;
  FRCPRB.Free;
  FRRecEspetDesp.Free;

  inherited;
end;

procedure TInfoTotal.SetRComl(const Value: TRComlCollection);
begin
  FRComl := Value;
end;

procedure TInfoTotal.SetRAquis(const Value: TRAquisCollection);
begin
  FRAquis := Value;
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

{ TeventoCollection }

function TeventoCollection.Add: TeventoCollectionItem;
begin
  Result := Self.New;
end;

function TeventoCollection.GetItem(
  Index: Integer): TeventoCollectionItem;
begin
  Result := TeventoCollectionItem(inherited Items[Index]);
end;

function TeventoCollection.New: TeventoCollectionItem;
begin
  Result := TeventoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TeventoCollection.SetItem(Index: Integer;
  Value: TeventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TeventoCollectionItem }

constructor TeventoCollectionItem.Create;
begin
  FevtTotal := TEvtTotal.Create;
  FevtRet := TevtRet.Create;
  FId := EmptyStr;
end;

destructor TeventoCollectionItem.destroy;
begin
  FevtTotal.Free;
  FevtRet.Free;

  inherited;
end;

{ TEvtTotal }

constructor TEvtTotal.Create();
begin
  FIdeEvento  := TIdeEvento1.Create;
  FIdeContrib := TIdeContrib.Create;
  FIdeStatus  := TIdeStatus.Create;
  FInfoRecEv  := TInfoRecEv.Create;
  FInfoTotal  := TInfoTotal.Create;
end;

destructor TEvtTotal.Destroy;
begin
  FIdeEvento.Free;
  FIdeContrib.Free;
  FIdeStatus.Free;
  FInfoRecEv.Free;
  FInfoTotal.Free;

  inherited;
end;

{ TRetEnvioLote }

constructor TRetEnvioLote.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeTransmissor := TIdeTransmissor.Create;
  FIdeContribuinte := TideContrib.Create;
  FStatus := TStatus.Create;
  FdadosRecepcaoLote := TdadosRecepcaoLote.Create;
  FdadosProcessamentoLote := TdadosProcessamentoLote.Create;
  Fevento := TeventoCollection.Create;
end;

destructor TRetEnvioLote.Destroy;
begin
  FLeitor.Free;

  FIdeTransmissor.Free;
  FIdeContribuinte.Free;
  FStatus.Free;
  FdadosRecepcaoLote.Free;
  FdadosProcessamentoLote.Free;
  Fevento.Free;

  inherited;
end;

procedure TRetEnvioLote.LerRetornoEventos;
var
  i, j: Integer;
  Ok: Boolean;
begin
  if leitor.rExtrai(2, 'retornoEventos') <> '' then
  begin
    i := 0;
    while Leitor.rExtrai(3, 'evento', '', i + 1) <> '' do
    begin
      evento.New;
      evento.Items[i].Id           := FLeitor.rAtributo('id', 'evento');
      evento.Items[i].ArquivoReinf := RetornarConteudoEntre(Leitor.Grupo, '>', '</evento');

      if leitor.rExtrai(4, 'evtRet') <> '' then
      begin
        evento.Items[i].evtRet.Leitor.Arquivo := evento.Items[i].ArquivoReinf;
        evento.Items[i].evtRet.LerXML;
      end
      else
      begin
        with evento.Items[i].evtTotal do
        begin
          if leitor.rExtrai(4, 'evtTotal') <> '' then
          begin
            FId := Leitor.rAtributo('id=');

            if leitor.rExtrai(5, 'ideEvento') <> '' then
              IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

            if leitor.rExtrai(5, 'ideContri') <> '' then
            begin
              IdeContrib.TpInsc := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInsc'));
              IdeContrib.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
            end;

            if leitor.rExtrai(5, 'ideRecRetorno') <> '' then
            begin
              if leitor.rExtrai(6, 'ideStatus') <> '' then
              begin
                IdeStatus.cdRetorno   := leitor.rCampo(tcStr, 'cdRetorno');
                IdeStatus.descRetorno := leitor.rCampo(tcStr, 'descRetorno');

                j := 0;
                while Leitor.rExtrai(7, 'regOcorrs', '', j + 1) <> '' do
                begin
                  IdeStatus.regOcorrs.New;
                  IdeStatus.regOcorrs.Items[j].tpOcorr        := leitor.rCampo(tcInt, 'tpOcorr');
                  IdeStatus.regOcorrs.Items[j].localErroAviso := leitor.rCampo(tcStr, 'localErroAviso');
                  IdeStatus.regOcorrs.Items[j].codResp        := leitor.rCampo(tcStr, 'codResp');
                  IdeStatus.regOcorrs.Items[j].dscResp        := leitor.rCampo(tcStr, 'dscResp');

                  inc(j);
                end;
              end;
            end;

            if leitor.rExtrai(5, 'infoRecEv') <> '' then
            begin
              infoRecEv.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
              infoRecEv.nrProtEntr := leitor.rCampo(tcStr, 'nrProtEntr');
              infoRecEv.dhProcess  := leitor.rCampo(tcDatHor, 'dhProcess');
              infoRecEv.dhRecepcao := leitor.rCampo(tcDatHor, 'dhRecepcao');
              infoRecEv.tpEv       := leitor.rCampo(tcStr, 'tpEv');
              infoRecEv.idEv       := leitor.rCampo(tcStr, 'idEv');
              infoRecEv.hash       := leitor.rCampo(tcStr, 'hash');
            end;

            if leitor.rExtrai(5, 'infoTotal') <> '' then
            begin
              with InfoTotal do
              begin
                FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

                if leitor.rExtrai(6, 'RTom') <> '' then
                begin
                  RTom.FcnpjPrestador     := leitor.rCampo(tcStr, 'cnpjPrestador');
                  RTom.FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
                  RTom.FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
                  RTom.FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
                  RTom.FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
                  RTom.FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');

                  // Versão 1.03.02
                  j := 0;
                  while Leitor.rExtrai(7, 'infoCRTom', '', j + 1) <> '' do
                  begin
                    RTom.infoCRTom.New;
                    RTom.infoCRTom.Items[j].FCRTom        := leitor.rCampo(tcStr, 'CRTom');
                    RTom.infoCRTom.Items[j].FVlrCRTom     := leitor.rCampo(tcDe2, 'VlrCRTom');
                    RTom.infoCRTom.Items[j].FVlrCRTomSusp := leitor.rCampo(tcDe2, 'VlrCRTomSusp');

                    inc(j);
                  end;
                end;

                if leitor.rExtrai(6, 'RPrest') <> '' then
                begin
                  RPrest.FtpInscTomador := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInscTomador'));
                  RPrest.FnrInscTomador := leitor.rCampo(tcStr, 'nrInscTomador');
                  RPrest.FvlrTotalBaseRet   := leitor.rCampo(tcDe2, 'vlrTotalBaseRet');
                  RPrest.FvlrTotalRetPrinc  := leitor.rCampo(tcDe2, 'vlrTotalRetPrinc');
                  RPrest.FvlrTotalRetAdic   := leitor.rCampo(tcDe2, 'vlrTotalRetAdic');
                  RPrest.FvlrTotalNRetPrinc := leitor.rCampo(tcDe2, 'vlrTotalNRetPrinc');
                  RPrest.FvlrTotalNRetAdic  := leitor.rCampo(tcDe2, 'vlrTotalNRetAdic');
                end;

                j := 0;
                while Leitor.rExtrai(6, 'RRecRepAD', '', j + 1) <> '' do
                begin
                  RRecRepAD.New;
                  RRecRepAD.Items[j].FcnpjAssocDesp := leitor.rCampo(tcStr, 'cnpjAssocDesp');
                  RRecRepAD.Items[j].FvlrTotalRep   := leitor.rCampo(tcDe2, 'vlrTotalRep');
                  RRecRepAD.Items[j].FvlrTotalRet   := leitor.rCampo(tcDe2, 'vlrTotalRet');
                  RRecRepAD.Items[j].FvlrTotalNRet  := leitor.rCampo(tcDe2, 'vlrTotalNRet');

                  // Versão 1.03.02
                  RRecRepAD.Items[j].FCRRecRepAD        := leitor.rCampo(tcStr, 'CRRecRepAD');
                  RRecRepAD.Items[j].FvlrCRRecRepAD     := leitor.rCampo(tcDe2, 'vlrCRRecRepAD');
                  RRecRepAD.Items[j].FvlrCRRecRepADSusp := leitor.rCampo(tcDe2, 'vlrCRRecRepADSusp');

                  inc(j);
                end;

                j := 0;
                while Leitor.rExtrai(6, 'RComl', '', j + 1) <> '' do
                begin
                  RComl.New;
                  RComl.Items[j].FvlrCPApur    := leitor.rCampo(tcDe2, 'vlrCPApur');
                  RComl.Items[j].FvlrRatApur   := leitor.rCampo(tcDe2, 'vlrRatApur');
                  RComl.Items[j].FvlrSenarApur := leitor.rCampo(tcDe2, 'vlrSenarApur');
                  RComl.Items[j].FvlrCPSusp    := leitor.rCampo(tcDe2, 'vlrCPSusp');
                  RComl.Items[j].FvlrRatSusp   := leitor.rCampo(tcDe2, 'vlrRatSusp');
                  RComl.Items[j].FvlrSenarSusp := leitor.rCampo(tcDe2, 'vlrSenarSusp');

                  // Versão 1.03.02
                  RComl.Items[j].FCRComl        := leitor.rCampo(tcStr, 'CRComl');
                  RComl.Items[j].FvlrCRComl     := leitor.rCampo(tcDe2, 'vlrCRComl');
                  RComl.Items[j].FvlrCRComlSusp := leitor.rCampo(tcDe2, 'vlrCRComlSusp');

                  inc(j);
                end;

                j := 0;
                while Leitor.rExtrai(6, 'RAquis', '', j + 1) <> '' do
                begin
                  RAquis.New;
                  RAquis.Items[j].FCRAquis        := leitor.rCampo(tcStr, 'CRAquis');
                  RAquis.Items[j].FvlrCRAquis     := leitor.rCampo(tcDe2, 'vlrCRAquis');
                  RAquis.Items[j].FvlrCRAquisSusp := leitor.rCampo(tcDe2, 'vlrCRAquisSusp');

                  inc(j);
                end;

                j := 0;
                while Leitor.rExtrai(6, 'RCPRB', '', j + 1) <> '' do
                begin
                  RCPRB.New;
                  RCPRB.Items[j].FcodRec         := leitor.rCampo(tcInt, 'codRec');
                  RCPRB.Items[j].FvlrCPApurTotal := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
                  RCPRB.Items[j].FvlrCPRBSusp    := leitor.rCampo(tcDe2, 'vlrCPRBSusp');

                  // Versão 1.03.02
                  RCPRB.Items[j].FCRCPRB        := leitor.rCampo(tcStr, 'CRCPRB');
                  RCPRB.Items[j].FvlrCRCPRB     := leitor.rCampo(tcDe2, 'vlrCRCPRB');
                  RCPRB.Items[j].FvlrCRCPRBSusp := leitor.rCampo(tcDe2, 'vlrCRCPRBSusp');

                  inc(j);
                end;

                if leitor.rExtrai(6, 'RRecEspetDesp') <> '' then
                begin
                  RRecEspetDesp.FvlrReceitaTotal := leitor.rCampo(tcDe2, 'vlrReceitaTotal');
                  RRecEspetDesp.FvlrCPApurTotal  := leitor.rCampo(tcDe2, 'vlrCPApurTotal');
                  RRecEspetDesp.FvlrCPSuspTotal  := leitor.rCampo(tcDe2, 'vlrCPSuspTotal');

                  // Versão 1.03.02
                  RRecEspetDesp.FCRRecEspetDesp        := leitor.rCampo(tcStr, 'CRRecEspetDesp');
                  RRecEspetDesp.FvlrCRRecEspetDesp     := leitor.rCampo(tcDe2, 'vlrCRRecEspetDesp');
                  RRecEspetDesp.FvlrCRRecEspetDespSusp := leitor.rCampo(tcDe2, 'vlrCRRecEspetDespSusp');
                end;
              end;
            end;
          end;
        end;
      end;

      inc(i);
    end;
  end;
end;

function TRetEnvioLote.LerXml: boolean;
begin
  if ( ( Pos('retornoLoteEventosAssincrono',Leitor.Arquivo) > 0 ) or
       ( Pos('retornoRecibosChaveEvento',Leitor.Arquivo) > 0 ) ) then
    Result := LerXmlRetornadoAPIRest
  else
    Result := LerXmlRetornadoWebService;
end;

function TRetEnvioLote.LerXmlRetornadoAPIRest: boolean;
var
  i: Integer;
  Ok: Boolean;
begin
  Result := True;

  try
    Leitor.Grupo := Leitor.Arquivo;

    if leitor.rExtrai(1, 'retornoLoteEventosAssincrono') <> '' then
    begin
      if leitor.rExtrai(2, 'ideContribuinte') <> '' then
      begin
        IdeContribuinte.TpInsc := StrToTpInscricao(Ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeContribuinte.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'status') <> '' then
      begin
        Status.cdstatus := leitor.rCampo(tcStr, 'cdResposta');
        Status.descRetorno := leitor.rCampo(tcStr, 'descResposta');

        if leitor.rExtrai(3, 'ocorrencias') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'ocorrencia', '', i + 1) <> '' do
          begin
            Status.Ocorrencias.New;

            Status.Ocorrencias.Items[i].Codigo := FLeitor.rCampo(tcInt, 'codigo');
            Status.Ocorrencias.Items[i].Descricao := FLeitor.rCampo(tcStr, 'descricao');
            Status.Ocorrencias.Items[i].Tipo := FLeitor.rCampo(tcInt, 'tipo');
            Status.Ocorrencias.Items[i].Localizacao := FLeitor.rCampo(tcStr, 'localizacao');
            inc(i);
          end;
        end;
      end;

      if leitor.rExtrai(2, 'dadosRecepcaoLote') <> '' then
      begin
        dadosRecepcaoLote.dhRecepcao := leitor.rCampo(tcDatHor, 'dhRecepcao');
        dadosRecepcaoLote.versaoAplicativoRecepcao := leitor.rCampo(tcStr, 'versaoAplicativoRecepcao');
        dadosRecepcaoLote.protocoloEnvio := leitor.rCampo(tcStr, 'protocoloEnvio');
      end;

      if leitor.rExtrai(2, 'dadosProcessamentoLote') <> '' then
      begin
        dadosProcessamentoLote.versaoAplicativoProcessamentoLote := leitor.rCampo(tcStr, 'versaoAplicativoProcessamentoLote');
      end;

      LerRetornoEventos;
    end
    else if leitor.rExtrai(1, 'Reinf') <> '' then
    begin
      if leitor.rExtrai(2, 'ideStatus') <> '' then
      begin
        Status.cdstatus := leitor.rCampo(tcStr, 'cdRetorno');
        Status.descRetorno := leitor.rCampo(tcStr, 'descRetorno');
      end;

      LerRetornoEventos;
    end;
  except
    Result := False;
  end;
end;

function TRetEnvioLote.LerXmlRetornadoWebService: boolean;
var
  i: Integer;
begin
  Result := True;

  try
    Leitor.Grupo := Leitor.Arquivo;

    if leitor.rExtrai(1, 'retornoLoteEventos') <> '' then
    begin
      FId := Leitor.rAtributo('id=');

      if leitor.rExtrai(2, 'ideTransmissor') <> '' then
        IdeTransmissor.IdTransmissor := FLeitor.rCampo(tcStr, 'IdTransmissor');

      if leitor.rExtrai(2, 'status') <> '' then
      begin
        Status.cdStatus    := Leitor.rCampo(tcInt, 'cdStatus');
        Status.descRetorno := Leitor.rCampo(tcStr,'descRetorno');

        if leitor.rExtrai(3, 'dadosRegistroOcorrenciaLote') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'ocorrencias', '', i + 1) <> '' do
          begin
            Status.Ocorrencias.New;
            Status.Ocorrencias.Items[i].Tipo        := FLeitor.rCampo(tcInt, 'tipo');
            Status.Ocorrencias.Items[i].Localizacao := FLeitor.rCampo(tcStr, 'localizacaoErroAviso');
            Status.Ocorrencias.Items[i].Codigo      := FLeitor.rCampo(tcInt, 'codigo');
            Status.Ocorrencias.Items[i].Descricao   := FLeitor.rCampo(tcStr, 'descricao');
            inc(i);
          end;
        end;
      end;

      LerRetornoEventos;
    end;
  except
    Result := False;
  end;
end;

function TRetEnvioLote.SalvarINI(nCont: Integer): boolean;
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
      with evento.Items[nCont].evtTotal do
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
          sSecao := 'regOcorrs' + IntToStrZero(i, 3);

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
            sSecao := 'infoCRTom' + IntToStrZero(i, 1);

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
            sSecao := 'RRecRepAD' + IntToStrZero(i, 3);

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
            sSecao := 'RComl' + IntToStrZero(i, 1);

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

          for i := 0 to RAquis.Count -1 do
          begin
            sSecao := 'RAquis' + IntToStrZero(i, 1);

            AIni.WriteString(sSecao, 'CRAquis',       RAquis.Items[i].CRAquis);
            AIni.WriteFloat(sSecao, 'vlrCRAquis',     RAquis.Items[i].vlrCRAquis);
            AIni.WriteFloat(sSecao, 'vlrCRAquisSusp', RAquis.Items[i].vlrCRAquisSusp);
          end;

          for i := 0 to RCPRB.Count -1 do
          begin
            sSecao := 'RCPRB' + IntToStrZero(i, 1);

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
    end;
  finally
    AIni.Free;
  end;
end;

end.
