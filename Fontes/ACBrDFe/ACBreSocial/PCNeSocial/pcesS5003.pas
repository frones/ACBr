{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS5003;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcesCommon, pcesConversaoeSocial;

type
  TEvtBasesFGTS = class;
  TIdeEstabLot2Collection = class;
  TIdeEstabLot2CollectionItem = class;
  TInfoTrabFGTSCollection = class;
  TInfoTrabFGTSCollectionItem = class;
  TInfoBaseFGTS = class;
  TInfoFGTS = class;
  TBasePerApurCollection = class;
  TBasePerApurCollectionItem = class;
  TInfoTrabDpsCollection = class;
  TInfoTrabDpsCollectionItem = class;
  TDpsPerApurCollection = class;
  TDpsPerApurCollectionItem = class;
  TIdeEstabCollection = class;
  TIdeEstabCollectionItem = class;
  TideLotacaoCollection = class;
  TideLotacaoCOllectionItem = class;
  TsucessaoVinc = class;
  TdetRubrSuspCollection = class;
  TdetRubrSuspCollectionItem = class;
  TideProcessoFGTSCollection = class;
  TideProcessoFGTSCollectionItem = class;
  TinfoBasePerAntECollection = class;
  TinfoBasePerAntECollectionItem = class;
  TBasePerAntECollection = class;
  TBasePerAntECollectionItem = class;
  TProcCS = class;
  
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
  
  TSucessaoVinc = class
  private
    FtpInsc   : tpTpInsc;
    FnrInsc   : string;
    FmatricAnt: string;
    FdtAdm    : TDateTime;
  public
    property tpInsc   : tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc   : string read FnrInsc write FnrInsc;
    property matricAnt: string read FmatricAnt write FmatricAnt;
    property dtAdm    : TDateTime read FdtAdm write FdtAdm;
  end;

  TDpsPerApurCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDpsPerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
  public
    function Add: TDpsPerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDpsPerApurCollectionItem;
    property Items[Index: Integer]: TDpsPerApurCollectionItem read GetItem write SetItem;
  end;
  
  TDpsPerApurCollectionItem = class(TObject)
  private
    FtpDps: Integer;
    FdpsFGTS: Double;
  public
    property tpDps: Integer read FtpDps write FtpDps;
    property dpsFGTS: Double read FdpsFGTS write FdpsFGTS;
  end;

  TIdeEstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeEstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeEstabCollectionItem);
  public
    function Add: TIdeEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabCollectionItem;
    property Items[Index: Integer]: TIdeEstabCollectionItem read GetItem write SetItem;
  end;
  
  TIdeEstabCollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: String;
    FideLotacao: TIdeLotacaoCollection;

    function getIdeLotacao(): TIdeLotacaoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function IdeLotacaoInst(): Boolean;

    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
    property ideLotacao: TIdeLotacaoCollection read getIdeLotacao write FideLotacao;
end;

  TideLotacaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideLotacaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TideLotacaoCollectionItem);
  public
    function Add: TideLotacaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideLotacaoCollectionItem;
    property Items[Index: Integer]: TideLotacaoCollectionItem read GetItem write SetItem;
  end;
  
  TideLotacaoCollectionItem = class(TObject)
  private
    FcodLotacao   : String;
    FtpLotacao    : String;
    FtpInsc       : tpTpInsc;
    FnrInsc       : String;
    FinfoTrabFGTS : TInfoTrabFGTSCollection;

    function getinfoTrabFGTS(): TInfoTrabFGTSCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoTrabFGTSInst(): Boolean;

    property codLotacao  : String read FcodLotacao write FcodLotacao;
    property tpLotacao   : String read FtpLotacao write FtpLotacao;
    property tpInsc      : tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc      : String read FnrInsc write FnrInsc;
    property infoTrabFGTS: TinfoTrabFGTSCollection read getInfoTrabFGTS write FinfoTrabFGTS;
  end;

  TInfoTrabDpsCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
  public
    function Add: TInfoTrabDpsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoTrabDpsCollectionItem;
    property Items[Index: Integer]: TInfoTrabDpsCollectionItem read GetItem write SetItem;
  end;
  
  TInfoTrabDpsCollectionItem = class(TObject)
  private
    Fmatricula: String;
    FcodCateg: Integer;
    FDpsPerApur: TDpsPerApurCollection;

    function getDpsPerApur(): TDpsPerApurCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dpsPerApurInst(): Boolean;

    property matricula: String read Fmatricula write Fmatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property dpsPerApur: TDpsPerApurCollection read getDpsPerApur write FDpsPerApur;
  end;

  TInfoDpsFGTS = class(TObject)
  private
    FInfoTrabDps: TInfoTrabDpsCollection;
    
    function getInfoTrabDps(): TInfoTrabDpsCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function InfoTrabDpsInst(): Boolean;

    property infoTrabDps: TInfoTrabDpsCollection read getInfoTrabDps write FInfoTrabDps;
  end;  

  TBasePerApurCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TBasePerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
  public
    function Add: TBasePerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TBasePerApurCollectionItem;
    property Items[Index: Integer]: TBasePerApurCollectionItem read GetItem write SetItem;
  end;

  TBasePerApurCollectionItem = class(TObject)
  private
    FtpValor    : Integer;
    FindIncid   : Integer;
    FremFGTS    : Double;
    FdpsFGTS    : Double;
    FdetRubrSusp: TdetRubrSuspCollection;
    
    function getDetRubrSusp(): TdetRubrSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function detRubrSuspInst(): Boolean;
    
    property tpValor    : Integer read FtpValor write FtpValor;
    property indIncid   : Integer read FindIncid write FindIncid;
    property remFGTS    : Double read FremFGTS write FremFGTS;
    property dpsFGTS    : Double read FdpsFGTS write FdpsFGTS;
    property detRubrSusp: TdetRubrSuspCollection read getDetRubrSusp write FdetRubrSusp;
  end;

  TInfoBaseFGTS = class(TObject)
  private
    FBasePerApur    : TBasePerApurCollection;
    FInfoBasePerAntE: TinfoBasePerAntECollection;
    
    function getBasePerApur()    : TBasePerApurCollection;
    function getInfoBasePerAntE(): TinfoBasePerAntECollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function BasePerApurInst(): Boolean;
    function InfoBasePerAntEInst(): Boolean;
    
    property basePerApur: TBasePerApurCollection read getBasePerApur write FBasePerApur;
    property infoBasePerAntE: TinfoBasePerAntECollection read getInfoBasePerAntE write FInfoBasePerAntE;
  end;

  TInfoTrabFGTSCollection = class(TACBrObjectList)
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
    FcategOrig: Integer;
    FtpRegTrab: tpTpRegTrab;
    FremunSuc: tpSimNaoFacultativo;
    FdtAdm: TDateTime;
    FdtDeslig: TDateTime;
    FdtInicio: TDateTime;
    FmtvDeslig: String;
    FdtTerm: TDateTime;
    FmtvDesligTSV: String;
    FProcCS: TProcCS;
    FInfoBaseFGTS: TInfoBaseFGTS;
    FsucessaoVinc: TSucessaoVinc;
  public
    constructor Create;
    destructor Destroy; override;

    property matricula      : String read Fmatricula write Fmatricula;
    property codCateg       : Integer read FcodCateg write FcodCateg;
    property categOrig      : Integer read FcategOrig write FcategOrig;
    property tpRegTrab      : tpTpRegTrab read FtpRegTrab write FtpRegTrab;
    property remunSuc       : tpSimNaoFacultativo read FremunSuc write FremunSuc;
    property dtAdm          : TDateTime read FdtAdm write FdtAdm;
    property dtDeslig       : TDateTime read FdtDeslig write FdtDeslig;
    property dtInicio       : TDateTime read FdtInicio write FdtInicio;
    property mtvDeslig      : String read FmtvDeslig write FmtvDeslig;
    property dtTerm         : TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV   : String read FmtvDesligTSV write FmtvDesligTSV;
    property InfoBaseFGTS   : TInfoBaseFGTS read FInfoBaseFGTS write FInfoBaseFGTS;
    property sucessaoVinc   : TSucessaoVinc read FsucessaoVinc write FsucessaoVinc;
    property ProcCS         : TProcCS read FProcCS write FProcCS;
  end;

  TIdeEstabLot2Collection = class(TACBrObjectList)
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
    FtpInsc      : tpTpInsc;
    FnrInsc      : String;
    FcodLotacao  : String;
    FInfoTrabFGTS: TInfoTrabFGTSCollection;
    
    function getInfoTrabFGTS(): TInfoTrabFGTSCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function InfoTrabFGTSInst(): Boolean;
    
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
    property codLotacao: String read FcodLotacao write FcodLotacao;
    property InfoTrabFGTS: TInfoTrabFGTSCollection read getInfoTrabFGTS write FInfoTrabFGTS;
  end;

  TInfoFGTS = class(TObject)
  private
    FdtVenc     : TDateTime;
    FIdeEstabLot: TIdeEstabLot2Collection;
    FinfoDpsFGTS: TInfoDpsFGTS;
    FIdeEstab   : TIdeEstabCollection;

    function getIdeEstabLot(): TIdeEstabLot2Collection;
    function getInfoDpsFGTS(): TInfoDpsFGTS;
    function getIdeEstab(): TIdeEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function IdeEstabLotInst(): Boolean;
    function InfoDpsFGTSInst(): Boolean;
    function IdeEstabInst(): Boolean;
        
    property dtVenc      : TDateTime read FdtVenc write FdtVenc;
    property IdeEstabLot : TIdeEstabLot2Collection read getIdeEstabLot write FIdeEstabLot;
    property infoDpsFGTS : TInfoDpsFGTS read getInfoDpsFGTS write FInfoDpsFGTS;
    property IdeEstab    : TIdeEstabCollection read getIdeEstab write FIdeEstab;
  end;

  TEvtBasesFGTS = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;
    FVersaoDF: TVersaoeSocial;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TideTrabalhador2;
    FinfoFGTS: TInfoFGTS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento      : TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador  : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador : TideTrabalhador2 read FIdeTrabalhador write FIdeTrabalhador;
    property infoFGTS       : TInfoFGTS read FinfoFGTS write FinfoFGTS;
    property Leitor         : TLeitor read FLeitor write FLeitor;
    property Id             : String      read FId;
    property XML            : String     read FXML;
    property VersaoDF       : TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TdetRubrSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetRubrSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetRubrSuspCollectionItem);
  public
    function Add: TdetRubrSuspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetRubrSuspCollectionItem;
    property Items[Index: Integer]: TdetRubrSuspCollectionItem read GetItem write SetItem;
  end;
  
  TdetRubrSuspCollectionItem = class(TObject)
  private
    FcodRubr        : String;
    FideTabRubr     : String;
    FvrRubr         : Double;
    FideProcessoFGTS: TideProcessoFGTSCollection;

    function getideProcessoFGTS(): TideProcessoFGTSCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function ideProcessoFGTSInst(): Boolean;

    property codRubr        : String read FcodRubr write FcodRubr;
    property ideTabRubr     : String read FideTabRubr write FideTabRubr;
    property vrRubr         : Double read FvrRubr write FvrRubr;
    property ideProcessoFGTS: TideProcessoFGTSCollection read getideProcessoFGTS write FideProcessoFGTS;
  end;

  TideProcessoFGTSCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideProcessoFGTSCollectionItem;
    procedure SetItem(Index: Integer; Value: TideProcessoFGTSCollectionItem);
  public
    function Add: TideProcessoFGTSCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideProcessoFGTSCollectionItem;
    property Items[Index: Integer]: TideProcessoFGTSCollectionItem read GetItem write SetItem;
  end;
  
  TideProcessoFGTSCollectionItem = class(TObject)
  private
    FnrProc: String;
  public
    property nrProc: String read FnrProc write FnrProc;
  end;

  TinfoBasePerAntECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoBasePerAntECollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoBasePerAntECollectionItem);
  public
    function Add: TinfoBasePerAntECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoBasePerAntECollectionItem;
    property Items[Index: Integer]: TinfoBasePerAntECollectionItem read GetItem write SetItem;
  end;
  
  TinfoBasePerAntECollectionItem = class(TObject)
  private
    FperRef     : String;
    FtpAcConv   : string;
    FbasePerAntE: TbasePerAntECollection;

    function getBasePerAntE(): TbasePerAntECollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function basePerAntEInst(): Boolean;
    
    property perRef     : String read FperRef write FperRef;
    property tpAcConv   : string read FtpAcConv write FtpAcConv;
    property basePerAntE: TbasePerAntECollection read getbasePerAntE write FbasePerAntE;
  end;
  
  TBasePerAntECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TBasePerAntECollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerAntECollectionItem);
  public
    function Add: TBasePerAntECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TBasePerAntECollectionItem;
    property Items[Index: Integer]: TBasePerAntECollectionItem read GetItem write SetItem;
  end;

  TBasePerAntECollectionItem = class(TObject)
  private
    FtpValorE   : Integer;
    FindIncidE  : Integer;
    FremFGTSE   : Double;
    FdpsFGTSE   : Double;
    FdetRubrSusp: TdetRubrSuspCollection;
    
    function getDetRubrSusp(): TdetRubrSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function detRubrSuspInst(): Boolean;
    
    property tpValorE   : Integer read FtpValorE write FtpValorE;
    property indIncidE  : Integer read FindIncidE write FindIncidE;
    property remFGTSE   : Double read FremFGTSE write FremFGTSE;
    property dpsFGTSE   : Double read FdpsFGTSE write FdpsFGTSE;
    property detRubrSusp: TdetRubrSuspCollection read getDetRubrSusp write FdetRubrSusp;
  end;

  TProcCS = class(TObject)
  private
   FNrProcJud: String;
  public
   property nrProcJud: String read FNrProcJud write FNrProcJud;
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
  i, j, k, l, m, n, o: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtBasesFGTS/', FXML)+18, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtBasesFGTS') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        IdeEvento.IndApuracao := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));
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

        if VersaoDF <= ve02_05_00 then
          IdeTrabalhador.nisTrab := leitor.rCampo(tcStr, 'nisTrab');
      end;

      if leitor.rExtrai(2, 'infoFGTS') <> '' then
      begin
        infoFGTS.dtVenc := leitor.rCampo(tcDat, 'dtVenc');

        if VersaoDF <= ve02_05_00 then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'ideEstabLot', '', i + 1) <> '' do
          begin
            infoFGTS.IdeEstabLot.New;
            infoFGTS.IdeEstabLot.Items[i].TpInsc     := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
            infoFGTS.IdeEstabLot.Items[i].NrInsc     := leitor.rCampo(tcStr, 'nrInsc');
            infoFGTS.IdeEstabLot.Items[i].codLotacao := leitor.rCampo(tcStr, 'codLotacao');

            j := 0;
            while Leitor.rExtrai(4, 'infoTrabFGTS', '', j + 1) <> '' do
            begin
              infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.New;
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
                infoFGTS.IdeEstabLot.Items[i].InfoTrabFGTS.Items[j].InfoBaseFGTS.basePerApur.New;
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
              infoFGTS.infoDpsFGTS.infoTrabDps.New;
              infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].matricula := leitor.rCampo(tcStr, 'matricula');
              infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].codCateg  := leitor.rCampo(tcInt, 'codCateg');

              k := 0;
              while Leitor.rExtrai(5, 'dpsPerApur', '', k + 1) <> '' do
              begin
                infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.New;
                infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.Items[k].tpDps   := leitor.rCampo(tcInt, 'tpDps');
                infoFGTS.infoDpsFGTS.infoTrabDps.Items[j].dpsPerApur.Items[k].dpsFGTS := leitor.rCampo(tcDe2, 'dpsFGTS');

                inc(k);
              end;

              inc(j);
            end;

            inc(i);
          end;
        end
        else
        begin
          i := 0;
          while Leitor.rExtrai(3, 'ideEstab', '', i + 1) <> '' do
          begin
            infoFGTS.IdeEstab.New;
            infoFGTS.IdeEstab.Items[i].tpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
            infoFGTS.IdeEstab.Items[i].nrInsc := leitor.rCampo(tcStr, 'nrInsc');

            j := 0;
            while Leitor.rExtrai(4, 'ideLotacao', '', j + 1) <> '' do
            begin
              infoFGTS.IdeEstab.Items[i].IdeLotacao.New;
              infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].codLotacao := leitor.rCampo(tcStr, 'codLotacao');
              infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].tpLotacao  := leitor.rCampo(tcStr, 'tpLotacao');

              if (leitor.rCampo(tcStr, 'tpLotacao') <> EmptyStr) then
              begin
                 if (StrToInt(InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].tpLotacao) in [2, 3, 4, 5, 6, 7, 8, 9]) then
                 begin
                   infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].tpInsc   := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                   infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].nrInsc   := leitor.rCampo(tcStr, 'nrInsc');
                 end;
              end;

              k := 0;
              while Leitor.rExtrai(5, 'infoTrabFGTS', '', k + 1) <> '' do
              begin
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.New;
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].matricula    := leitor.rCampo(tcStr, 'matricula');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].codCateg     := leitor.rCampo(tcInt, 'codCateg');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].categOrig    := leitor.rCampo(tcInt, 'categOrig');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].tpRegTrab    := eSStrToTpRegTrab(ok, leitor.rCampo(tcStr, 'tpRegTrab'));
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].remunSuc     := eSStrToSimNaoFacultativo(ok, leitor.rCampo(tcStr, 'remunSuc'));
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].dtDeslig     := leitor.rCampo(tcDat, 'dtDeslig');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].mtvDeslig    := leitor.rCampo(tcStr, 'mtvDeslig');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].dtTerm       := leitor.rCampo(tcDat, 'dtTerm');
                infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].mtvDesligTSV := leitor.rCampo(tcStr, 'mtvDesligTSV');
                
                if leitor.rExtrai(6, 'sucessaoVinc') <> '' then
                begin
                  infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].sucessaoVinc.tpInsc    := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                  infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].sucessaoVinc.nrInsc    := leitor.rCampo(tcStr, 'nrInsc');
                  infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].sucessaoVinc.matricAnt := leitor.rCampo(tcStr, 'matricAnt');
                  infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].sucessaoVinc.dtAdm     := leitor.rCampo(tcDat, 'dtAdm');
                end;

                if Leitor.rExtrai(6, 'infoBaseFGTS') <> '' then
                begin
                  l := 0;
                  while Leitor.rExtrai(7, 'basePerApur', '', l + 1) <> '' do
                  begin
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.New;
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].tpValor  := leitor.rCampo(tcInt, 'tpValor');
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].indIncid := leitor.rCampo(tcInt, 'indIncid');
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].remFGTS  := leitor.rCampo(tcDe2, 'remFGTS');
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].dpsFGTS  := leitor.rCampo(tcDe2, 'dpsFGTS');

                    m := 0;
                    while Leitor.rExtrai(8, 'detRubrSusp', '', m + 1) <> '' do
                    begin
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.New;
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.Items[m].codRubr    := leitor.rCampo(tcStr, 'codRubr');
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.Items[m].ideTabRubr := leitor.rCampo(tcStr, 'ideTabRubr');
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.Items[m].vrRubr     := leitor.rCampo(tcDe2, 'vrRubr');

                      n := 0;
                      while Leitor.rExtrai(9, 'ideProcessoFGTS', '', n + 1) <> '' do
                      begin
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.Items[m].IdeProcessoFGTS.New;
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.BasePerApur.Items[l].detRubrSusp.Items[m].IdeProcessoFGTS.Items[n].nrProc := leitor.rCampo(tcStr, 'nrProc');

                        inc(n);
                      end;

                      inc(m);
                    end;

                    inc(l);
                  end;

                  l := 0;
                  while Leitor.rExtrai(7, 'infoBasePerAntE', '', l + 1) <> '' do
                  begin
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.infoBasePerAntE.New;
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.infoBasePerAntE.Items[l].perRef := leitor.rCampo(tcStr, 'perRef');
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].InfoBaseFGTS.InfoBasePerAntE.Items[l].tpAcConv := leitor.rCampo(tcStr, 'tpAcConv');
                    m := 0;
                    while Leitor.rExtrai(8, 'basePerAntE', '', m + 1) <> '' do
                    begin
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.infoBasePerAntE.Items[l].basePerAntE.New;
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.infoBasePerAntE.Items[l].basePerAntE.Items[m].tpValorE  := leitor.rCampo(tcInt, 'tpValorE');
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].indIncidE := leitor.rCampo(tcInt, 'indIncidE');
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].remFGTSE  := leitor.rCampo(tcDe2, 'remFGTSE');
                      infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].dpsFGTSE  := leitor.rCampo(tcDe2, 'dpsFGTSE');
                      
                      n := 0;
                      while Leitor.rExtrai(9, 'detRubrSusp', '', n + 1) <> '' do
                      begin
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.New;
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.Items[n].codRubr    := leitor.rCampo(tcStr, 'codRubr');
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.Items[n].ideTabRubr := leitor.rCampo(tcStr, 'ideTabRubr');
                        infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.Items[n].vrRubr     := leitor.rCampo(tcDe2, 'vrRubr'); 

                        o := 0;
                        while Leitor.rExtrai(10, 'ideProcessoFGTS', '', o + 1) <> '' do
                        begin
                          infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.Items[n].ideProcessoFGTS.New;
                          infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].detRubrSusp.Items[n].ideProcessoFGTS.Items[o].nrProc := leitor.rCampo(tcStr, 'nrProc');
                          inc(o);
                        end;

                        inc(n);
                      end;

                      inc(m);
                    end;

                    inc(l);
                  end;

                  if Leitor.rExtrai(6, 'procCS') <> '' then
                    infoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoTrabFGTS.Items[k].procCS.nrProcJud := leitor.rCampo(tcStr, 'nrProcJud');
                end;

                inc(k);
              end;

              inc(j);
            end;
            
            inc(i);
          end;  
        end;
      end;

      Result := True;
    end;

  except
    on e: Exception do  begin
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

{ TideEstabLot2Collection }

function TIdeEstabLot2Collection.New: TIdeEstabLot2CollectionItem;
begin
  Result := TIdeEstabLot2CollectionItem.Create;
  Self.Add(Result);
end;

function TIdeEstabLot2Collection.Add: TIdeEstabLot2CollectionItem;
begin
  Result := Self.New;
end;

function TIdeEstabLot2Collection.GetItem(Index: Integer): TIdeEstabLot2CollectionItem;
begin
  Result := TIdeEstabLot2CollectionItem(inherited Items[Index]);
end;

procedure TIdeEstabLot2Collection.SetItem(Index: Integer; Value: TIdeEstabLot2CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TIdeEstabLotCollectionItem }

constructor TIdeEstabLot2CollectionItem.Create;
begin
  inherited Create;

  FInfoTrabFGTS := nil;
end;

destructor TIdeEstabLot2CollectionItem.Destroy;
begin
  if InfoTrabFGTSInst() then
    FInfoTrabFGTS.Free;

  inherited;
end;

function TIdeEstabLot2CollectionItem.getInfoTrabFGTS(): TInfoTrabFGTSCollection;
begin
  if not(InfoTrabFGTSInst()) then
    FInfoTrabFGTS := TInfoTrabFGTSCollection.Create;
  Result := FInfoTrabFGTS;
end;

function TIdeEstabLot2CollectionItem.InfoTrabFGTSInst(): Boolean;
begin
  Result := Assigned(FInfoTrabFGTS);
end;

{ TInfoFGTS }

constructor TInfoFGTS.Create;
begin
  inherited Create;

  FIdeEstabLot := nil;
  FInfoDpsFGTS := nil;
  FIdeEstab    := nil;
end;

destructor TInfoFGTS.Destroy;
begin
  if IdeEstabLotInst() then
    FIdeEstabLot.Free;
  if InfoDpsFGTSInst() then
    FinfoDpsFGTS.Free;
  if IdeEstabInst() then
    FideEstab.Free;

  inherited;
end;

function TInfoFGTS.getIdeEstabLot(): TIdeEstabLot2Collection;
begin
  if not(IdeEstabLotInst()) then
    FIdeEstabLot := TIdeEstabLot2Collection.Create;
  Result := FIdeEstabLot;
end;

function TInfoFGTS.getInfoDpsFGTS(): TInfoDpsFGTS;
begin
  if not(InfoDpsFGTSInst()) then
    FInfoDpsFGTS := TInfoDpsFGTS.Create;
  Result := FInfoDpsFGTS;
end;

function TInfoFGTS.getIdeEstab(): TIdeEstabCollection;
begin
  if not(IdeEstabInst()) then
    FIdeEstab := TIdeEstabCollection.Create;
  Result := FIdeEstab;
end;

function TInfoFGTS.IdeEstabLotInst(): Boolean;
begin
  Result := Assigned(FIdeEstabLot);
end;

function TInfoFGTS.InfoDpsFGTSInst(): Boolean;
begin
  Result := Assigned(FinfoDpsFGTS);
end;

function TInfoFGTS.IdeEstabInst(): Boolean;
begin
  Result := Assigned(FIdeEstab);
end;

{ TInfoTrabFGTSCollection }

function TInfoTrabFGTSCollection.Add: TInfoTrabFGTSCollectionItem;
begin
  Result := Self.New;
end;

function TInfoTrabFGTSCollection.GetItem(Index: Integer): TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem(inherited Items[Index]);
end;

procedure TInfoTrabFGTSCollection.SetItem(Index: Integer; Value: TInfoTrabFGTSCollectionItem);
begin
  inherited Items[Index] := Value;
end;
function TInfoTrabFGTSCollection.New: TInfoTrabFGTSCollectionItem;
begin
  Result := TInfoTrabFGTSCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoTrabFGTSCollectionItem }

constructor TInfoTrabFGTSCollectionItem.Create;
begin
  inherited Create;
  
  FInfoBaseFGTS := TInfoBaseFGTS.Create;
  FSucessaoVinc := TSucessaoVinc.Create;
  FProcCS       := TProcCS.Create;
end;

destructor TInfoTrabFGTSCollectionItem.Destroy;
begin
  FInfoBaseFGTS.Free;
  FSucessaoVinc.Free;
  FProcCS.Free;

  inherited;
end;

{ TInfoBaseFGTS }

constructor TInfoBaseFGTS.Create;
begin
  inherited Create;

  FBasePerApur := nil;
  FInfoBasePerAntE := nil;
end;

destructor TInfoBaseFGTS.Destroy;
begin
  if BasePerApurInst() then
    FBasePerApur.Free;
  if InfoBasePerAntEInst() then
    FInfoBasePerAntE.Free;

  inherited;
end;

function TInfoBaseFGTS.getBasePerApur(): TBasePerApurCollection;
begin
  if not(BasePerApurInst()) then
    FBasePerApur := TBasePerApurCollection.Create;
  Result := FBasePerApur;
end;

function TInfoBaseFGTS.InfoBasePerAntEInst(): Boolean;
begin
  Result := Assigned(FInfoBasePerAntE);
end;

function TInfoBaseFGTS.getInfoBasePerAntE(): TInfoBasePerAntECollection;
begin
  if not(InfoBasePerAntEInst()) then
    FInfoBasePerAntE := TInfoBasePerAntECollection.Create;
  Result := FInfoBasePerAntE;
end;

function TInfoBaseFGTS.BasePerApurInst(): Boolean;
begin
  Result := Assigned(FBasePerApur);
end;

{ TBasePerApurCollection }

function TBasePerApurCollection.Add: TBasePerApurCollectionItem;
begin
  Result := Self.New;
end;

function TBasePerApurCollection.GetItem(Index: Integer): TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem(inherited Items[Index]);
end;

procedure TBasePerApurCollection.SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TBasePerApurCollection.New: TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem.Create;
  Self.Add(Result);
end;

{ TBasePerApurCollectionItem }

constructor TBasePerApurCollectionItem.Create;
begin
  inherited;
  
  FdetRubrSusp := nil;
end;

destructor TBasePerApurCollectionItem.Destroy;
begin
 if detRubrSuspInst() then
   FdetRubrSusp.Free;
   
 inherited;
end;

function TBasePerApurCollectionItem.getdetRubrSusp(): TdetRubrSuspCollection;
begin
  if not(detRubrSuspInst()) then
    FdetRubrSusp := TdetRubrSuspCollection.Create;
  Result := FdetRubrSusp;
end;

function TBasePerApurCollectionItem.detRubrSuspInst(): Boolean;
begin
  Result := Assigned(FdetRubrSusp);
end;

{ TInfoDpsFGTS }

constructor TInfoDpsFGTS.Create;
begin
  inherited Create;

  FInfoTrabDps := nil;
end;

destructor TInfoDpsFGTS.Destroy;
begin
  if InfoTrabDpsInst() then
    FInfoTrabDps.Free;

  inherited;
end;

function TInfoDpsFGTS.getInfoTrabDps(): TInfoTrabDpsCollection;
begin
  if not(InfoTrabDpsInst()) then
    FInfoTrabDps := TInfoTrabDpsCollection.Create;
  Result := FInfoTrabDps;
end;

function TInfoDpsFGTS.InfoTrabDpsInst(): Boolean;
begin
  Result := Assigned(FInfoTrabDps);
end;

{ TInfoTrabDpsCollection }

function TInfoTrabDpsCollection.Add: TInfoTrabDpsCollectionItem;
begin
  Result := Self.New;
end;

function TInfoTrabDpsCollection.GetItem(Index: Integer): TInfoTrabDpsCollectionItem;
begin
  Result := TInfoTrabDpsCollectionItem(inherited Items[Index]);
end;

procedure TInfoTrabDpsCollection.SetItem(Index: Integer; Value: TInfoTrabDpsCollectionItem);
begin
  inherited Items[Index] := Value;
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

  FDpsPerApur := nil
end;

destructor TInfoTrabDpsCollectionItem.Destroy;
begin
  if dpsPerApurInst() then
    FDpsPerApur.Free;

  inherited;
end;

function TInfoTrabDpsCollectionItem.getDpsPerApur(): TDpsPerApurCollection;
begin
  if not(dpsPerApurInst()) then
    FDpsPerApur := TDpsPerApurCollection.Create;
  Result := FDpsPerApur;
end;

function TInfoTrabDpsCollectionItem.dpsPerApurInst(): Boolean;
begin
  Result := Assigned(FDpsPerApur);
end;

{ TIdeEstabCollection }

function TIdeEstabCollection.Add: TIdeEstabCollectionItem;
begin
  Result := Self.New;
end;

function TIdeEstabCollection.GetItem(Index: Integer): TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem(inherited Items[Index]);
end;

procedure TIdeEstabCollection.SetItem(Index: Integer; Value: TIdeEstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeEstabCollection.New: TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeEstabCollectionItem }

constructor TIdeEstabCollectionItem.Create;
begin
  inherited;
  
  FIdeLotacao := nil;
end;

destructor TIdeEstabCollectionItem.Destroy;
begin
  if IdeLotacaoInst() then
    FIdeLotacao.Free;
  
  inherited;  
end;

function TIdeEstabCollectionItem.IdeLotacaoInst(): Boolean;
begin
  Result := Assigned(FideLotacao);
end;

function TIdeEstabCollectionItem.getIdeLotacao: TIdeLotacaoCollection;
begin
  if not(IdeLotacaoInst()) then
    FIdeLotacao := TIdeLotacaoCollection.Create;
  Result := FIdeLotacao;
end;

{ TIdeLotacaoCollection }

function TIdeLotacaoCollection.Add: TIdeLotacaoCollectionItem;
begin
  Result := Self.New;
end;

function TIdeLotacaoCollection.GetItem(Index: Integer): TIdeLotacaoCollectionItem;
begin
  Result := TIdeLotacaoCollectionItem(inherited Items[Index]);
end;

procedure TIdeLotacaoCollection.SetItem(Index: Integer;
  Value: TIdeLotacaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeLotacaoCollection.New: TIdeLotacaoCollectionItem;
begin
  Result := TIdeLotacaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeLotacaoColletionItem }

constructor TIdeLotacaoCollectionItem.Create;
begin
  inherited;
  
  FinfoTrabFGTS := nil;
end;

destructor TIdeLotacaoCollectionItem.Destroy;
begin
  if infoTrabFGTSInst() then
    FinfoTrabFGTS.Free;

  inherited;
end;

function TIdeLotacaoCollectionItem.infoTrabFGTSInst(): Boolean;
begin
  Result := Assigned(FinfoTrabFGTS);
end;

function TIdeLotacaoCollectionItem.getInfoTrabFGTS: TinfoTrabFGTSCollection;
begin
  if not(infoTrabFGTSInst()) then
    FinfoTrabFGTS := TinfoTrabFGTSCollection.Create;
  Result := FinfoTrabFGTS;
end;

{ TDpsPerApurCollection }

function TDpsPerApurCollection.Add: TDpsPerApurCollectionItem;
begin
  Result := Self.New;
end;

function TDpsPerApurCollection.GetItem(Index: Integer): TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited Items[Index]);
end;

procedure TDpsPerApurCollection.SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDpsPerApurCollection.New: TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem.Create;
  Self.Add(Result);
end;

{ TdetRubrSuspCollection }

function TdetRubrSuspCollection.Add: TdetRubrSuspCollectionItem;
begin
  Result := Self.New;
end;

function TdetRubrSuspCollection.GetItem(Index: Integer): TdetRubrSuspCollectionItem;
begin
  Result := TdetRubrSuspCollectionItem(inherited Items[Index]);
end;

procedure TdetRubrSuspCollection.SetItem(Index: Integer; Value: TdetRubrSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdetRubrSuspCollection.New: TdetRubrSuspCollectionItem;
begin
  Result := TdetRubrSuspCollectionItem.Create;
  Self.Add(Result);
end;

{ TdetRubrSuspCollectionItem }

constructor TdetRubrSuspCollectionItem.Create;
begin
  inherited;
  
  FideProcessoFGTS := nil;
end;

destructor TdetRubrSuspCollectionItem.Destroy;
begin
  if IdeProcessoFGTSinst() then
    FideProcessoFGTS.Free;

  inherited;
end;

function TdetRubrSuspCollectionItem.getIdeProcessoFGTS(): TIdeProcessoFGTSCollection;
begin
  if not(IdeProcessoFGTSInst()) then
    FIdeProcessoFGTS := TIdeProcessoFGTSCollection.Create;
  Result := FIdeProcessoFGTS;
end;

function TdetRubrSuspCollectionItem.IdeProcessoFGTSInst(): Boolean;
begin
  Result := Assigned(FIdeProcessoFGTS);
end;

{ TIdeProcessoFGTSCollection }

function TIdeProcessoFGTSCollection.Add: TIdeProcessoFGTSCollectionItem;
begin
  Result := Self.New;
end;

function TIdeProcessoFGTSCollection.GetItem(Index: Integer): TIdeProcessoFGTSCollectionItem;
begin
  Result := TIdeProcessoFGTSCollectionItem(inherited Items[Index]);
end;

procedure TIdeProcessoFGTSCollection.SetItem(Index: Integer; Value: TIdeProcessoFGTSCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeProcessoFGTSCollection.New: TIdeProcessoFGTSCollectionItem;
begin
  Result := TIdeProcessoFGTSCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoBasePerAntECollection }

function TinfoBasePerAntECollection.Add: TinfoBasePerAntECollectionItem;
begin
  Result := Self.New;
end;

function TinfoBasePerAntECollection.GetItem(Index: Integer): TinfoBasePerAntECollectionItem;
begin
  Result := TinfoBasePerAntECollectionItem(inherited Items[Index]);
end;

procedure TinfoBasePerAntECollection.SetItem(Index: Integer; Value: TinfoBasePerAntECollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoBasePerAntECollection.New: TinfoBasePerAntECollectionItem;
begin
  Result := TinfoBasePerAntECollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoBasePerAntECollectionItem }

constructor TinfoBasePerAntECollectionItem.Create;
begin
  inherited;
  
  FbasePerAntE := nil;
end;

destructor TinfoBasePerAntECollectionItem.Destroy;
begin
  if basePerAntEinst() then
    FbasePerAntE.Free;

  inherited;
end;

function TinfoBasePerAntECollectionItem.getBasePerAntE(): TbasePerAntECollection;
begin
  if not(BasePerAntEInst()) then
    FbasePerAntE := TbasePerAntECollection.Create;
  Result := FbasePerAntE;
end;

function TinfoBasePerAntECollectionItem.BasePerAntEInst(): Boolean;
begin
  Result := Assigned(FbasePerAntE);
end;

{ TBasePerAntECollection }

function TBasePerAntECollection.Add: TBasePerAntECollectionItem;
begin
  Result := Self.New;
end;

function TBasePerAntECollection.GetItem(Index: Integer): TBasePerAntECollectionItem;
begin
  Result := TBasePerAntECollectionItem(inherited Items[Index]);
end;

procedure TBasePerAntECollection.SetItem(Index: Integer; Value: TBasePerAntECollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TBasePerAntECollection.New: TBasePerAntECollectionItem;
begin
  Result := TBasePerAntECollectionItem.Create;
  Self.Add(Result);
end;

{ TBasePerAntECollectionItem }

constructor TBasePerAntECollectionItem.Create;
begin
  inherited;
  
  FdetRubrSusp := nil;
end;

destructor TBasePerAntECollectionItem.Destroy;
begin
  if detRubrSuspInst() then
    FdetRubrSusp.Free;

  inherited;
end;

function TBasePerAntECollectionItem.getDetRubrSusp(): TdetRubrSuspCollection;
begin
  if not(detRubrSuspInst()) then
    FdetRubrSusp := TdetRubrSuspCollection.Create;
  Result := FdetRubrSusp;
end;

function TBasePerAntECollectionItem.detRubrSuspInst(): Boolean;
begin
  Result := Assigned(FdetRubrSusp);
end;

end.
