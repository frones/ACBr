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

unit pcesS5011;

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
  pcesCommon, pcesConversaoeSocial;

type
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;
  TInfoTercSuspCollection = class;
  TInfoTercSuspCollectionItem = class;
  TbasesremunCollection = class;
  TbasesremunCollectionItem = class;
  TinfoSubstPatrOpPortCollection = class;
  TinfoSubstPatrOpPortCollectionItem = class;
  TbasesAquisCollection = class;
  TbasesAquisCollectionItem = class;
  TbasesComercCollection = class;
  TbasesComercCollectionItem = class;
  TinfoCREstabCollection = class;
  TinfoCREstabCollectionItem = class;

  TEvtCS = class;

  TS5011 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtCS: TEvtCS;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtCS: TEvtCS read FEvtCS write FEvtCS;

  end;

  TInfoCPSeg = class(TObject)
  private
    FvrDescCP: Double;
    FvrCpSeg: Double;
  public
    property vrDescCP: Double read FvrDescCP write FvrDescCP;
    property vrCpSeg: Double read FvrCpSeg write FvrCpSeg;
  end;

  TInfoAtConc = class(TObject)
  private
    FfatorMes: Double;
    Ffator13: Double;
  public
    property fatorMes: Double read FfatorMes;
    property fator13: Double read Ffator13;
  end;

  TInfoPJ = class(TObject)
  private
    FindCoop: Integer;
    FindConstr: Integer;
    FindSubstPatr: Integer;
    FpercRedContrib: Double;
    FpercTransf: tpPercTransf;
    FinfoAtConc: TinfoAtConc;
  public
    constructor Create;
    destructor Destroy; override;

    property indCoop: Integer read FindCoop;
    property indConstr: Integer read FindConstr;
    property indSubstPatr: Integer read FindSubstPatr;
    property percRedContrib: Double read FpercRedContrib;
    property infoAtConc: TinfoAtConc read FinfoAtConc write FinfoAtConc;
    property percTransf: tpPercTransf read FPercTransf write FPercTransf;
  end;

  TInfoContrib = class(TObject)
  private
    FclassTrib: String;
    FinfoPJ: TInfoPJ;
  public
    constructor Create;
    destructor Destroy; override;

    property classTrib: String read FclassTrib;
    property infoPJ: TInfoPJ read FinfoPJ write FinfoPJ;
  end;

  TInfoComplObra = class(TObject)
  private
    FindSubstPatrObra: Integer;
  public
    property indSubstPatrObra: Integer read FindSubstPatrObra;
  end;

  TInfoEstab = class(TObject)
  private
    FFap: Double;
    FAliqRatAjust: Double;
    FcnaePrep: String;
    FcnpjResp: String;
    FAliqRat: tpAliqRat;
    FinfoComplObra: TInfoComplObra;
  public
    constructor Create;
    destructor Destroy; override;

    property cnaePrep: String read FcnaePrep;
    property cnpjResp: String read FcnpjResp;
    property AliqRat: tpAliqRat read FAliqRat;
    property fap: Double read FFap;
    property aliqRatAjust: Double read FAliqRatAjust;
    property infoComplObra: TInfoComplObra read FinfoComplObra write FinfoComplObra;
  end;

  TinfoTercSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoTercSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoTercSuspCollectionItem);
  public
    function Add: TinfoTercSuspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
	  function New: TinfoTercSuspCollectionItem;
    property Items[Index: Integer]: TinfoTercSuspCollectionItem read GetItem write SetItem;
  end;

  TinfoTercSuspCollectionItem = class(TObject)
  private
    FcodTerc: String;
  public
    property codTerc: String read FcodTerc;
  end;

  TInfoEmprParcial = class(TObject)
  private
    FtpinscContrat: Integer;
    FnrInscContrat: String;
    FtpInscProp: Integer;
    FnrInscProp: String;
    FcnoObra: String;
  public
    property tpInscContrat: Integer read FtpinscContrat;
    property nrInscContrat: String read FnrInscContrat;
    property tpInscProp: Integer read FtpInscProp;
    property nrInscProp: String read FnrInscProp;
    property cnoObra: String read FcnoObra;
  end;

  TdadosOpPort = class(TObject)
  private
    FFap: Double;
    FAliqRatAjust: Double;
    FcnpjOpPortuario: String;
    FAliqRat: tpAliqRat;
  public
    property cnpjOpPortuario: String read FcnpjOpPortuario;
    property AliqRat: tpAliqRat read FAliqRat;
    property fap: Double read FFap;
    property aliqRatAjust: Double read FAliqRatAjust;
  end;

  TbasesCp = class(TObject)
  private
    FvrBcCp00: Double;
    FvrBcCp15: Double;
    FvrBcCp20: Double;
    FvrBcCp25: Double;
    FvrSuspBcCp00: Double;
    FvrSuspBcCp15: Double;
    FvrSuspBcCp20: Double;
    FvrSuspBcCp25: Double;
    FvrBcCp00VA: Double;
    FvrBcCp15VA: Double;
    FvrBcCp20VA: Double;
    FvrBcCp25VA: Double;
    FvrSuspBcCp00VA: Double;
    FvrSuspBcCp15VA: Double;
    FvrSuspBcCp20VA: Double;
    FvrSuspBcCp25VA: Double;
    FvrDescSest: Double;
    FvrCalcSest: Double;
    FvrDescSenat: Double;
    FvrCalcSenat: Double;
    FvrSalFam: Double;
    FvrSalMat: Double;
    FvrBcCpSM: Double;
  public
    property vrBcCp00: Double read FvrBcCp00;
    property vrBcCp15: Double read FvrBcCp15;
    property vrBcCp20: Double read FvrBcCp20;
    property vrBcCp25: Double read FvrBcCp25;
    property vrSuspBcCp00: Double read FvrSuspBcCp00;
    property vrSuspBcCp15: Double read FvrSuspBcCp15;
    property vrSuspBcCp20: Double read FvrSuspBcCp20;
    property vrSuspBcCp25: Double read FvrSuspBcCp25;
    property vrBcCp00VA: Double read FvrBcCp00VA;
    property vrBcCp15VA: Double read FvrBcCp15VA;
    property vrBcCp20VA: Double read FvrBcCp20VA;
    property vrBcCp25VA: Double read FvrBcCp25VA;
    property vrSuspBcCp00VA: Double read FvrSuspBcCp00VA;
    property vrSuspBcCp15VA: Double read FvrSuspBcCp15VA;
    property vrSuspBcCp20VA: Double read FvrSuspBcCp20VA;
    property vrSuspBcCp25VA: Double read FvrSuspBcCp25VA;
    property vrDescSest: Double read FvrDescSest;
    property vrCalcSest: Double read FvrCalcSest;
    property vrDescSenat: Double read FvrDescSenat;
    property vrCalcSenat: Double read FvrCalcSenat;
    property vrSalFam: Double read FvrSalFam;
    property vrSalMat: Double read FvrSalMat;
    property vrBcCpSM: Double read FvrBcCpSM;
  end;

  TbasesRemunCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasesRemunCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesRemunCollectionItem);
  public
    function Add: TbasesRemunCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbasesRemunCollectionItem;
    property Items[Index: Integer]: TbasesRemunCollectionItem read GetItem write SetItem;
  end;

  TbasesRemunCollectionItem = class(TObject)
  private
    FindIncid: Integer;
    FcodCateg: Integer;
    FbasesCp: TbasesCp;
  public
    constructor Create;
    destructor Destroy; override;

    property indIncid: Integer read FindIncid;
    property codCateg: Integer read FcodCateg;
    property basesCp: TbasesCp read FbasesCp write FbasesCp;
  end;

  TbasesAvNport = class(TObject)
  private
    FvrBcCp25: Double;
    FvrBcCp15: Double;
    FvrBcCp00: Double;
    FvrBcCp20: Double;
    FvrBcCp13: Double;
    FvrDescCP: Double;
    FvrBcFgts: Double;
  public
    property vrBcCp00: Double read FvrBcCp00;
    property vrBcCp15: Double read FvrBcCp15;
    property vrBcCp20: Double read FvrBcCp20;
    property vrBcCp25: Double read FvrBcCp25;
    property vrBcCp13: Double read FvrBcCp13;
    property vrBcFgts: Double read FvrBcFgts;
    property vrDescCP: Double read FvrDescCP;
  end;

  TinfoSubstPatrOpPortCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoSubstPatrOpPortCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoSubstPatrOpPortCollectionItem);
  public
    function Add: TinfoSubstPatrOpPortCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoSubstPatrOpPortCollectionItem;
    property Items[Index: Integer]: TinfoSubstPatrOpPortCollectionItem read GetItem write SetItem;
  end;

  TinfoSubstPatrOpPortCollectionItem = class(TObject)
  private
    FcnpjOpPortuario: String;
  public
    property cnpjOpPortuario: String read FcnpjOpPortuario;
  end;

  TideLotacaoCollectionItem = class(TObject)
  private
    Ffpas: Integer;
    FcodLotacao: String;
    FcodTercs: String;
    FcodTercsSusp: String;
    FinfoTercSusp: TinfoTercSuspCollection;
    FInfoEmprParcial: TInfoEmprParcial;
    FcnoObra: String;
    FdadosOpPort: TdadosOpPort;
    Fbasesremun: TbasesremunCollection;
    FbasesAvNport: TbasesAvNport;
    FinfoSubstPatrOpPort: TinfoSubstPatrOpPortCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property codLotacao: String read FcodLotacao;
    property fpas: Integer read Ffpas;
    property codTercs: String read FcodTercs;
    property codTercsSusp: String read FcodTercsSusp;
    property infoTercSusp: TinfoTercSuspCollection read FinfoTercSusp write FinfoTercSusp;
    property InfoEmprParcial: TInfoEmprParcial read FInfoEmprParcial write FInfoEmprParcial;
    property cnoObra: String read FcnoObra;
    property dadosOpPort: TdadosOpPort read FdadosOpPort write FdadosOpPort;
    property basesremun: TbasesremunCollection read Fbasesremun write Fbasesremun;
    property basesAvNPort: TbasesAvNport read FbasesAvNport write FbasesAvNport;
    property infoSubstPatrOpPort: TinfoSubstPatrOpPortCollection read FinfoSubstPatrOpPort write FinfoSubstPatrOpPort;
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

  TbasesAquisCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasesAquisCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesAquisCollectionItem);
  public
    function Add: TbasesAquisCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbasesAquisCollectionItem;
    property Items[Index: Integer]: TbasesAquisCollectionItem read GetItem write SetItem;
  end;

  TbasesAquisCollectionItem = class(TObject)
  private
    FvrSenarCalc: Double;
    FvrCPNRet: Double;
    FvrRatDescPR: Double;
    FvrRatCalcPR: Double;
    FvrCPDescPR: Double;
    FvrSenarDesc: Double;
    FvrSenarNRet: Double;
    FvrCPCalcPR: Double;
    FvrRatNRet: Double;
    FvlrAquis: Double;
    FindAquis: Integer;
  public
    property indAquis: Integer read FindAquis;
    property vlrAquis: Double read FvlrAquis;
    property vrCPDescPR: Double read FvrCPDescPR;
    property vrCPNRet: Double read FvrCPNRet;
    property vrRatNRet: Double read FvrRatNRet;
    property vrSenarNRet: Double read FvrSenarNRet;
    property vrCPCalcPR: Double read FvrCPCalcPR;
    property vrRatDescPR: Double read FvrRatDescPR;
    property vrRatCalcPR: Double read FvrRatCalcPR;
    property vrSenarDesc: Double read FvrSenarDesc;
    property vrSenarCalc: Double read FvrSenarCalc;
  end;

  TbasesComercCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasesComercCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesComercCollectionItem);
  public
    function Add: TbasesComercCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
		function New: TbasesComercCollectionItem;
    property Items[Index: Integer]: TbasesComercCollectionItem read GetItem write SetItem;
  end;

  TbasesComercCollectionItem = class(TObject)
  private
    FvrCPSusp: Double;
    FvrRatSusp: Double;
    FvrSenarSusp: Double;
    FvrBcComPr: Double;
    FindComerc: Integer;
  public
    property indComerc: Integer read FindComerc;
    property vrBcComPr: Double read FvrBcComPr;
    property vrCPSusp: Double read FvrCPSusp;
    property vrRatSusp: Double read FvrRatSusp;
    property vrSenarSusp: Double read FvrSenarSusp;
  end;

  TinfoCREstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoCREstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoCREstabCollectionItem);
  public
    function Add: TinfoCREstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
	function New: TinfoCREstabCollectionItem;
    property Items[Index: Integer]: TinfoCREstabCollectionItem read GetItem write SetItem;
  end;

  TinfoCREstabCollectionItem = class(TObject)
  private
    FvrSuspCR: Double;
    FvrCR: Double;
    FtpCR: String;
  public
    property tpCR: String read FtpCR;
    property vrCR: Double read FvrCR;
    property vrSuspCR: Double read FvrSuspCR;
  end;

  TideEstabCollectionItem = class(TObject)
  private
    FNrInsc: string;
    FTpInsc: tpTpInsc;
    FinfoEstab: TinfoEstab;
    FideLotacao: TideLotacaoCollection;
    FbasesAquis: TbasesAquisCollection;
    FbasesComerc: TbasesComercCollection;
    FinfoCREstab: TinfoCREstabCollection;

  public
    constructor Create;
    destructor Destroy; override;

    property TpInsc: tpTpInsc read FTpInsc;
    property NrInsc: string read FNrInsc;
    property infoEstab: TinfoEstab read FinfoEstab write FinfoEstab;
    property ideLotacao: TideLotacaoCollection read FideLotacao write FideLotacao;
    property basesAquis: TbasesAquisCollection read FbasesAquis write FbasesAquis;
    property basesComerc: TbasesComercCollection read FbasesComerc write FbasesComerc;
    property infoCREstab: TinfoCREstabCollection read FinfoCREstab write FinfoCREstab;
  end;

  TideEstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideEstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TideEstabCollectionItem);
  public
    function Add: TideEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideEstabCollectionItem;
    property Items[Index: Integer]: TideEstabCollectionItem read GetItem write SetItem;
  end;

  TInfoCRContribCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    function Add: TInfoCRContribCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
	function New: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem;
  end;

  TInfoCRContribCollectionItem = class(TObject)
  private
    FtpCR: String;
    FvrCR: Double;
    FvrCRSusp: Double;
  public
    property tpCR: String read FtpCR;
    property vrCR: Double read FvrCR;
    property vrCRSusp: Double read FvrCRSusp;
  end;

  TInfoCS = class(TObject)
  private
    FnrRecArqBase: String;
    FindExistInfo: Integer;
    FInfoCpSeg: TInfoCpSeg;
    FInfoContrib: TInfoContrib;
    FideEstab: TideEstabCollection;
    FinfoCRContrib: TinfoCRContribCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase;
    property indExistInfo: Integer read FindExistInfo;
    property InfoCpSeg: TInfoCpSeg read FInfoCpSeg write FInfoCpSeg;
    property InfoContrib: TInfoContrib read FInfoContrib write FInfoContrib;
    property ideEstab: TideEstabCollection read FideEstab write FideEstab;
    property infoCRContrib: TinfoCRContribCollection read FinfoCRContrib write FinfoCRContrib;
  end;

  TEvtCS = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;
    FVersaoDF: TVersaoeSocial;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TIdeTrabalhador3;
    FInfoCS: TInfoCS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TIdeTrabalhador3 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoCS: TInfoCS read FInfoCS write FInfoCS;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId;
    property XML: String     read FXML;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base;

{ TS5011 }

constructor TS5011.Create;
begin
  inherited Create;
  FTipoEvento := teS5011;
  FEvtCS      := TEvtCS.Create;
end;

destructor TS5011.Destroy;
begin
  FEvtCS.Free;

  inherited;
end;

function TS5011.GetEvento : TObject;
begin
  Result := self;
end;

function TS5011.GetXml : string;
begin
  Result := FEvtCS.XML;
end;

procedure TS5011.SetXml(const Value: string);
begin
  if Value = FEvtCS.XML then Exit;

  FEvtCS.FXML := Value;
  FEvtCS.Leitor.Arquivo := Value;
  FEvtCS.LerXML;

end;

function TS5011.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TEvtCS }

constructor TEvtCS.Create;
begin
  inherited Create;
  FLeitor         := TLeitor.Create;
  FIdeEvento      := TIdeEvento5.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTrabalhador := TIdeTrabalhador3.Create;
  FInfoCS         := TInfoCS.Create;
end;

destructor TEvtCS.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FInfoCS.Free;

  inherited;
end;

function TEvtCS.LerXML: boolean;
var
  ok: Boolean;
  i, j, k: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtCS/', FXML)+11, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtCS') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        IdeEvento.IndApuracao := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));
        IdeEvento.perApur     := leitor.rCampo(tcStr, 'perApur');
      end;

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'infoCS') <> '' then
      begin
        infoCS.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoCS.FindExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        if leitor.rExtrai(3, 'infoCPSeg') <> '' then
        begin
          infoCS.InfoCpSeg.vrDescCP := leitor.rCampo(tcDe2, 'vrDescCP');
          infoCS.InfoCpSeg.vrCpSeg  := leitor.rCampo(tcDe2, 'vrCpSeg');
        end;

        if leitor.rExtrai(3, 'infoContrib') <> '' then
        begin
          infoCS.InfoContrib.FclassTrib := leitor.rCampo(tcStr, 'classTrib');

          if leitor.rExtrai(4, 'infoPJ') <> '' then
          begin
            infoCS.InfoContrib.infoPJ.FindCoop        := leitor.rCampo(tcInt, 'indCoop');
            infoCS.InfoContrib.infoPJ.FindConstr      := leitor.rCampo(tcInt, 'indConstr');
            infoCS.InfoContrib.infoPJ.FindSubstPatr   := leitor.rCampo(tcInt, 'indSubstPatr');
            infoCS.InfoContrib.infoPJ.FpercRedContrib := leitor.rCampo(tcDe2, 'percRedContrib');
            infoCS.InfoContrib.infoPJ.FPercTransf     := eSStrToTpPercTransf(ok, leitor.rCampo(tcStr, 'percTransf'));

            if leitor.rExtrai(5, 'infoAtConc') <> '' then
            begin
              infoCS.InfoContrib.infoPJ.infoAtConc.FfatorMes := leitor.rCampo(tcDe4, 'fatorMes');
              infoCS.InfoContrib.infoPJ.infoAtConc.Ffator13  := leitor.rCampo(tcDe2, 'fator13');
            end;
          end;
        end;

        i := 0;
        while Leitor.rExtrai(3, 'ideEstab', '', i + 1) <> '' do
        begin
          infoCS.ideEstab.New;
          infoCS.ideEstab.Items[i].FTpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
          infoCS.ideEstab.Items[i].FNrInsc := leitor.rCampo(tcStr, 'nrInsc');

          if leitor.rExtrai(4, 'infoEstab') <> '' then
          begin
            infoCS.ideEstab.Items[i].infoEstab.FcnaePrep     := leitor.rCampo(tcStr, 'cnaePrep');
            infoCS.ideEstab.Items[i].infoEstab.FcnpjResp     := leitor.rCampo(tcStr, 'cnpjResp');
            infoCS.ideEstab.Items[i].infoEstab.FAliqRat      := eSStrToAliqRat(ok, leitor.rCampo(tcStr, 'AliqRat'));
            infoCS.ideEstab.Items[i].infoEstab.Ffap          := leitor.rCampo(tcDe4, 'fap');
            infoCS.ideEstab.Items[i].infoEstab.FaliqRatAjust := leitor.rCampo(tcDe4, 'aliqRatAjust');

            if leitor.rExtrai(5, 'infoComplObra') <> '' then
              infoCS.ideEstab.Items[i].infoEstab.infoComplObra.FindSubstPatrObra := leitor.rCampo(tcInt, 'indSubstPatrObra');
          end;

          j := 0;
          while Leitor.rExtrai(4, 'ideLotacao', '', j + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].ideLotacao.New;
            infoCS.ideEstab.Items[i].ideLotacao.Items[j].FcodLotacao   := leitor.rCampo(tcStr, 'codLotacao');
            infoCS.ideEstab.Items[i].ideLotacao.Items[j].Ffpas         := leitor.rCampo(tcInt, 'fpas');
            infoCS.ideEstab.Items[i].ideLotacao.Items[j].FcodTercs     := leitor.rCampo(tcStr, 'codTercs');
            infoCS.ideEstab.Items[i].ideLotacao.Items[j].FcodTercsSusp := leitor.rCampo(tcStr, 'codTercsSusp');

            k := 0;
            while Leitor.rExtrai(5, 'infoTercSusp', '', k + 1) <> '' do
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoTercSusp.New;
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoTercSusp.Items[k].FcodTerc := leitor.rCampo(tcStr, 'codTerc');
              inc(k);
            end;

            if leitor.rExtrai(5, 'infoEmprParcial') <> '' then
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.FtpInscContrat := leitor.rCampo(tcInt, 'tpInscContrat');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.FnrInscContrat := leitor.rCampo(tcStr, 'nrInscContrat');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.FtpInscProp    := leitor.rCampo(tcInt, 'tpInscProp');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.FnrInscProp    := leitor.rCampo(tcStr, 'nrInscProp');

              if VersaoDF > ve02_05_00 then
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.FcnoObra     := leitor.rCampo(tcStr, 'cnoObra');
            end;

            if VersaoDF <= ve02_05_00 then
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].FcnoObra := leitor.rCampo(tcStr, 'cnoObra');

            if leitor.rExtrai(5, 'dadosOpPort') <> '' then
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.FcnpjOpPortuario := leitor.rCampo(tcStr, 'cnpjOpPortuario');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.FAliqRat         := eSStrToAliqRat(ok, leitor.rCampo(tcStr, 'AliqRat'));
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.Ffap             := leitor.rCampo(tcDe4, 'fap');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.FaliqRatAjust    := leitor.rCampo(tcDe4, 'aliqRatAjust');
            end;

            k := 0;
            while Leitor.rExtrai(5, 'basesRemun', '', k + 1) <> '' do
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.New;
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].FindIncid := leitor.rCampo(tcInt, 'indIncid');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].FcodCateg := leitor.rCampo(tcInt, 'codCateg');

              if leitor.rExtrai(6, 'basesCp') <> '' then
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp00 := leitor.rCampo(tcDe2, 'vrBcCp00');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp15 := leitor.rCampo(tcDe2, 'vrBcCp15');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp20 := leitor.rCampo(tcDe2, 'vrBcCp20');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp25 := leitor.rCampo(tcDe2, 'vrBcCp25');

                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp00 := leitor.rCampo(tcDe2, 'vrSuspBcCp00');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp15 := leitor.rCampo(tcDe2, 'vrSuspBcCp15');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp20 := leitor.rCampo(tcDe2, 'vrSuspBcCp20');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp25 := leitor.rCampo(tcDe2, 'vrSuspBcCp25');

                if VersaoDF > ve02_05_00 then
                begin
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp00VA := leitor.rCampo(tcDe2, 'vrBcCp00VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp15VA := leitor.rCampo(tcDe2, 'vrBcCp15VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp20VA := leitor.rCampo(tcDe2, 'vrBcCp20VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCp25VA := leitor.rCampo(tcDe2, 'vrBcCp25VA');
  
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp00VA := leitor.rCampo(tcDe2, 'vrSuspBcCp00VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp15VA := leitor.rCampo(tcDe2, 'vrSuspBcCp15VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp20VA := leitor.rCampo(tcDe2, 'vrSuspBcCp20VA');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSuspBcCp25VA := leitor.rCampo(tcDe2, 'vrSuspBcCp25VA');
                end;
                
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrDescSest  := leitor.rCampo(tcDe2, 'vrDescSest');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrCalcSest  := leitor.rCampo(tcDe2, 'vrCalcSest');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrDescSenat := leitor.rCampo(tcDe2, 'vrDescSenat');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrCalcSenat := leitor.rCampo(tcDe2, 'vrCalcSenat');

                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSalFam := leitor.rCampo(tcDe2, 'vrSalFam');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrSalMat := leitor.rCampo(tcDe2, 'vrSalMat');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.FvrBcCpSM := leitor.rCampo(tcDe2, 'vrBcCpSM');
              end;

              inc(k);
            end;

            if leitor.rExtrai(5, 'basesAvNPort') <> '' then
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcCp00 := leitor.rCampo(tcDe2, 'vrBcCp00');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcCp15 := leitor.rCampo(tcDe2, 'vrBcCp15');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcCp20 := leitor.rCampo(tcDe2, 'vrBcCp20');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcCp25 := leitor.rCampo(tcDe2, 'vrBcCp25');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcCp13 := leitor.rCampo(tcDe2, 'vrBcCp13');

              if VersaoDF <= ve02_05_00 then
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrBcFgts := leitor.rCampo(tcDe2, 'vrBcFgts');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.FvrDescCP := leitor.rCampo(tcDe2, 'vrDescCP');
              end;
            end;

            k := 0;
            while Leitor.rExtrai(5, 'infoSubstPatrOpPort', '', k + 1) <> '' do
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoSubstPatrOpPort.New;
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoSubstPatrOpPort.Items[k].FcnpjOpPortuario := leitor.rCampo(tcStr, 'cnpjOpPortuario');
              inc(k);
            end;

            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(4, 'basesAquis', '', j + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].basesAquis.New;
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FindAquis    := leitor.rCampo(tcInt, 'indAquis');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvlrAquis    := leitor.rCampo(tcDe2, 'vlrAquis');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrCPDescPR  := leitor.rCampo(tcDe2, 'vrCPDescPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrCPNRet    := leitor.rCampo(tcDe2, 'vrCPNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrRatNRet   := leitor.rCampo(tcDe2, 'vrRatNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrSenarNRet := leitor.rCampo(tcDe2, 'vrSenarNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrCPCalcPR  := leitor.rCampo(tcDe2, 'vrCPCalcPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrRatDescPR := leitor.rCampo(tcDe2, 'vrRatDescPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrRatCalcPR := leitor.rCampo(tcDe2, 'vrRatCalcPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrSenarDesc := leitor.rCampo(tcDe2, 'vrSenarDesc');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].FvrSenarCalc := leitor.rCampo(tcDe2, 'vrSenarCalc');
            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(4, 'basesComerc', '', j + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].basesComerc.New;
            infoCS.ideEstab.Items[i].basesComerc.Items[j].FindComerc   := leitor.rCampo(tcInt, 'indComerc');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].FvrBcComPr   := leitor.rCampo(tcDe2, 'vrBcComPr');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].FvrCPSusp    := leitor.rCampo(tcDe2, 'vrCPSusp');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].FvrRatSusp   := leitor.rCampo(tcDe2, 'vrRatSusp');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].FvrSenarSusp := leitor.rCampo(tcDe2, 'vrSenarSusp');
            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(4, 'infoCREstab', '', j + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].infoCREstab.New;
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].FtpCR     := leitor.rCampo(tcStr, 'tpCR');
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].FvrCR     := leitor.rCampo(tcDe2, 'vrCR');
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].FvrSuspCR := leitor.rCampo(tcDe2, 'vrSuspCR');
            inc(j);
          end;

          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'infoCRContrib', '', i + 1) <> '' do
        begin
          infoCS.infoCRContrib.New;
          infoCS.infoCRContrib.Items[i].FtpCR     := leitor.rCampo(tcStr, 'tpCR');
          infoCS.infoCRContrib.Items[i].FvrCR     := leitor.rCampo(tcDe2, 'vrCR');
          infoCS.infoCRContrib.Items[i].FvrCRSusp := leitor.rCampo(tcDe2, 'vrCRSusp');
          inc(i);
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TEvtCS.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j, k: Integer;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'evtCS';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'IndApuracao', eSIndApuracaoToStr(IdeEvento.IndApuracao));
      AIni.WriteString(sSecao, 'perApur',     IdeEvento.perApur);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'infoCS';
      AIni.WriteString(sSecao, 'nrRecArqBase',  infoCS.nrRecArqBase);
      AIni.WriteInteger(sSecao, 'indExistInfo', infoCS.indExistInfo);

      sSecao := 'infoCPSeg';
      AIni.WriteFloat(sSecao, 'vrDescCP', infoCS.infoCPSeg.vrDescCP);
      AIni.WriteFloat(sSecao, 'vrCpSeg',  infoCS.infoCPSeg.vrCpSeg);

      sSecao := 'infoContrib';
      AIni.WriteString(sSecao, 'classTrib', infoCS.infoContrib.classTrib);

      sSecao := 'infoPJ';
      AIni.WriteInteger(sSecao, 'indCoop',      infoCS.infoContrib.infoPJ.indCoop);
      AIni.WriteInteger(sSecao, 'indConstr',    infoCS.infoContrib.infoPJ.indConstr);
      AIni.WriteInteger(sSecao, 'indSubstPatr', infoCS.infoContrib.infoPJ.indSubstPatr);
      AIni.WriteFloat(sSecao, 'percRedContrib', infoCS.infoContrib.infoPJ.percRedContrib);

      sSecao := 'infoAtConc';
      AIni.WriteFloat(sSecao, 'fatorMes', infoCS.infoContrib.infoPJ.infoAtConc.fatorMes);
      AIni.WriteFloat(sSecao, 'fator13',  infoCS.infoContrib.infoPJ.infoAtConc.fator13);

      for i := 0 to infoCS.ideEstab.Count -1 do
      begin
        sSecao := 'ideEstab' + IntToStrZero(I, 4);

        AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(infoCS.ideEstab.Items[i].TpInsc));
        AIni.WriteString(sSecao, 'nrInsc', infoCS.ideEstab.Items[i].nrInsc);

        sSecao := 'infoEstab' + IntToStrZero(I, 4);

        AIni.WriteString(sSecao, 'cnaePrep',    infoCS.ideEstab.Items[i].infoEstab.cnaePrep);
        AIni.WriteString(sSecao, 'aliqRat',     eSAliqRatToStr(infoCS.ideEstab.Items[i].infoEstab.AliqRat));
        AIni.WriteFloat(sSecao, 'fap',          infoCS.ideEstab.Items[i].infoEstab.fap);
        AIni.WriteFloat(sSecao, 'aliqRatAjust', infoCS.ideEstab.Items[i].infoEstab.aliqRatAjust);

        sSecao := 'infoComplObra' + IntToStrZero(I, 4);

        AIni.WriteInteger(sSecao, 'indSubstPatrObra', infoCS.ideEstab.Items[i].infoEstab.infoComplObra.indSubstPatrObra);

        with infoCS.ideEstab.Items[i] do
        begin
          for j := 0 to ideLotacao.Count -1 do
          begin
            sSecao := 'ideLotacao' + IntToStrZero(I, 4) + IntToStrZero(j, 2);

            AIni.WriteString(sSecao, 'codLotacao',   ideLotacao.Items[j].codLotacao);
            AIni.WriteInteger(sSecao, 'fpas',        ideLotacao.Items[j].fpas);
            AIni.WriteString(sSecao, 'codTercs',     ideLotacao.Items[j].codTercs);
            AIni.WriteString(sSecao, 'codTercsSusp', ideLotacao.Items[j].codTercsSusp);
            AIni.WriteString(sSecao, 'cnoObra',      ideLotacao.Items[j].cnoObra);

            with ideLotacao.Items[j] do
            begin
              for k := 0 to infoTercSusp.Count -1 do
              begin
                with infoTercSusp.Items[k] do
                begin
                  sSecao := 'infoTercSusp' + IntToStrZero(I, 4) + IntToStrZero(j, 2) +
                          IntToStrZero(k, 2);

                  AIni.WriteString(sSecao, 'codTerc', codTerc);
                end;
              end;

              sSecao := 'infoEmprParcial' + IntToStrZero(I, 4) + IntToStrZero(j, 2);

              AIni.WriteInteger(sSecao, 'tpInscContrat', infoEmprParcial.tpInscContrat);
              AIni.WriteString(sSecao, 'nrInscContrat',  infoEmprParcial.nrInscContrat);
              AIni.WriteInteger(sSecao, 'tpInscProp',    infoEmprParcial.tpInscProp);
              AIni.WriteString(sSecao, 'nrInscProp',     infoEmprParcial.nrInscProp);

              sSecao := 'dadosOpPort' + IntToStrZero(I, 4) + IntToStrZero(j, 2);

              AIni.WriteString(sSecao, 'cnpjOpPortuario', dadosOpPort.cnpjOpPortuario);
              AIni.WriteString(sSecao, 'nrInscContrat',   eSAliqRatToStr(dadosOpPort.AliqRat));
              AIni.WriteFloat(sSecao, 'fap',              dadosOpPort.fap);
              AIni.WriteFloat(sSecao, 'aliqRatAjust',     dadosOpPort.aliqRatAjust);

              for k := 0 to basesremun.Count -1 do
              begin
                with basesremun.Items[k] do
                begin
                  sSecao := 'basesRemun' + IntToStrZero(I, 4) + IntToStrZero(j, 2) +
                          IntToStrZero(k, 2);

                  AIni.WriteInteger(sSecao, 'indincid', indincid);
                  AIni.WriteInteger(sSecao, 'codCateg',  codCateg);

                  sSecao := 'basesCp' + IntToStrZero(I, 4) + IntToStrZero(j, 2) +
                          IntToStrZero(k, 2);

                  with basesCp do
                  begin
                    AIni.WriteFloat(sSecao, 'vrBcCp00',     vrBcCp00);
                    AIni.WriteFloat(sSecao, 'vrBcCp15',     vrBcCp15);
                    AIni.WriteFloat(sSecao, 'vrBcCp20',     vrBcCp20);
                    AIni.WriteFloat(sSecao, 'vrBcCp25',     vrBcCp25);
                    AIni.WriteFloat(sSecao, 'vrSuspBcCp00', vrSuspBcCp00);
                    AIni.WriteFloat(sSecao, 'vrSuspBcCp15', vrSuspBcCp15);
                    AIni.WriteFloat(sSecao, 'vrSuspBcCp20', vrSuspBcCp20);
                    AIni.WriteFloat(sSecao, 'vrSuspBcCp25', vrSuspBcCp25);
                    AIni.WriteFloat(sSecao, 'vrDescSest',   vrDescSest);
                    AIni.WriteFloat(sSecao, 'vrCalcSest',   vrCalcSest);
                    AIni.WriteFloat(sSecao, 'vrDescSenat',  vrDescSenat);
                    AIni.WriteFloat(sSecao, 'vrCalcSenat',  vrCalcSenat);
                    AIni.WriteFloat(sSecao, 'vrSalFam',     vrSalFam);
                    AIni.WriteFloat(sSecao, 'vrSalMat',     vrSalMat);
                    AIni.WriteFloat(sSecao, 'vrBcCpSM',     vrBcCpSM);
                  end;
                end;
              end;

              sSecao := 'basesAvNPort' + IntToStrZero(I, 4) + IntToStrZero(j, 2);

              with basesAvNPort do
              begin
                AIni.WriteFloat(sSecao, 'vrBcCp00', vrBcCp00);
                AIni.WriteFloat(sSecao, 'vrBcCp15', vrBcCp15);
                AIni.WriteFloat(sSecao, 'vrBcCp20', vrBcCp20);
                AIni.WriteFloat(sSecao, 'vrBcCp25', vrBcCp25);
                AIni.WriteFloat(sSecao, 'vrBcCp13', vrBcCp13);
                AIni.WriteFloat(sSecao, 'vrBcFgts', vrBcFgts);
                AIni.WriteFloat(sSecao, 'vrDescCP', vrDescCP);
              end;

              for k := 0 to infoSubstPatrOpPort.Count -1 do
              begin
                with infoSubstPatrOpPort.Items[k] do
                begin
                  sSecao := 'infoSubstPatrOpPort' + IntToStrZero(I, 4) + IntToStrZero(j, 2) +
                          IntToStrZero(k, 3);

                  AIni.WriteString(sSecao, 'cnpjOpPortuario', cnpjOpPortuario);
                end;
              end;
            end;
          end;

          for j := 0 to basesAquis.Count -1 do
          begin
            with basesAquis.Items[j] do
            begin
              sSecao := 'basesAquis' + IntToStrZero(I, 4) + IntToStrZero(j, 1);

              AIni.WriteInteger(sSecao, 'indAquis',  indAquis);
              AIni.WriteFloat(sSecao, 'vlrAquis',    vlrAquis);
              AIni.WriteFloat(sSecao, 'vrCPDescPR',  vrCPDescPR);
              AIni.WriteFloat(sSecao, 'vrCPNRet',    vrCPNRet);
              AIni.WriteFloat(sSecao, 'vrRatNRet',   vrRatNRet);
              AIni.WriteFloat(sSecao, 'vrSenarNRet', vrSenarNRet);
              AIni.WriteFloat(sSecao, 'vrCPCalcPR',  vrCPCalcPR);
              AIni.WriteFloat(sSecao, 'vrRatDescPR', vrRatDescPR);
              AIni.WriteFloat(sSecao, 'vrRatCalcPR', vrRatCalcPR);
              AIni.WriteFloat(sSecao, 'vrSenarDesc', vrSenarDesc);
              AIni.WriteFloat(sSecao, 'vrSenarCalc', vrSenarCalc);
            end;
           end;

          for j := 0 to basesComerc.Count -1 do
          begin
            with basesComerc.Items[j] do
            begin
              sSecao := 'basesComerc' + IntToStrZero(I, 4) + IntToStrZero(j, 1);

              AIni.WriteInteger(sSecao, 'indComerc', indComerc);
              AIni.WriteFloat(sSecao, 'vrBcComPR',   vrBcComPr);
              AIni.WriteFloat(sSecao, 'vrCPSusp',    vrCPSusp);
              AIni.WriteFloat(sSecao, 'vrRatSusp',   vrRatSusp);
              AIni.WriteFloat(sSecao, 'vrSenarSusp', vrSenarSusp);
            end;
           end;

          for j := 0 to infoCREstab.Count -1 do
          begin
            with infoCREstab.Items[j] do
            begin
              sSecao := 'infoCREstab' + IntToStrZero(I, 4) + IntToStrZero(j, 2);

              AIni.WriteString(sSecao, 'tpCR',   tpCR);
              AIni.WriteFloat(sSecao, 'vrCR',     vrCR);
              AIni.WriteFloat(sSecao, 'vrSuspCR', vrSuspCR);
            end;
           end;

        end;
      end;

      for i := 0 to infoCS.infoCRContrib.Count -1 do
      begin
        sSecao := 'infoCRContrib' + IntToStrZero(I, 2);

        AIni.WriteString(sSecao, 'tpCR',   infoCS.infoCRContrib.Items[i].tpCR);
        AIni.WriteFloat(sSecao, 'vrCR',     infoCS.infoCRContrib.Items[i].vrCR);
        AIni.WriteFloat(sSecao, 'vrCRSusp', infoCS.infoCRContrib.Items[i].vrCRSusp);
      end;
    end;
  finally
    AIni.Free;
  end;
end;

{ TInfoCS }

constructor TInfoCS.Create;
begin
  inherited Create;
  
  FInfoCpSeg     := TInfoCpSeg.Create;
  FInfoContrib   := TInfoContrib.Create;
  FideEstab      := TideEstabCollection.Create;
  FinfoCRContrib := TinfoCRContribCollection.Create;
end;

destructor TInfoCS.Destroy;
begin
  FInfoCpSeg.Free;
  FInfoContrib.Free;
  FideEstab.Free;
  FinfoCRContrib.Free;

  inherited;
end;

{ TInfoContrib }

constructor TInfoContrib.Create;
begin
  inherited Create;
  FInfoPJ := TInfoPJ.Create;
end;

destructor TInfoContrib.Destroy;
begin
  FInfoPJ.Free;

  inherited;
end;

{ TInfoPJ }

constructor TInfoPJ.Create;
begin
  inherited Create;
  FinfoAtConc := TinfoAtConc.Create;
end;

destructor TInfoPJ.Destroy;
begin
  FinfoAtConc.Free;
  
  inherited;
end;

{ TideEstabCollection }

function TideEstabCollection.Add: TideEstabCollectionItem;
begin
  Result := Self.New;
end;

function TideEstabCollection.GetItem(
  Index: Integer): TideEstabCollectionItem;
begin
  Result := TideEstabCollectionItem(inherited Items[Index]);
end;

procedure TideEstabCollection.SetItem(Index: Integer;
  Value: TideEstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TideEstabCollection.New: TideEstabCollectionItem;
begin
  Result := TideEstabCollectionItem.Create;
  Self.Add(Result);
end;

{ TbasesAquisCollection }

function TbasesAquisCollection.Add: TbasesAquisCollectionItem;
begin
  Result := Self.New;
end;

function TbasesAquisCollection.GetItem(
  Index: Integer): TbasesAquisCollectionItem;
begin
  Result := TbasesAquisCollectionItem(inherited Items[Index]);
end;

procedure TbasesAquisCollection.SetItem(Index: Integer;
  Value: TbasesAquisCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbasesAquisCollection.New: TbasesAquisCollectionItem;
begin
  Result := TbasesAquisCollectionItem.Create;
  Self.Add(Result);
end;

{ TbasesComercCollection }

function TbasesComercCollection.Add: TbasesComercCollectionItem;
begin
  Result := Self.New;
end;

function TbasesComercCollection.GetItem(
  Index: Integer): TbasesComercCollectionItem;
begin
  Result := TbasesComercCollectionItem(inherited Items[Index]);
end;

procedure TbasesComercCollection.SetItem(Index: Integer;
  Value: TbasesComercCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbasesComercCollection.New: TbasesComercCollectionItem;
begin
  Result := TbasesComercCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoCREstabCollection }

function TinfoCREstabCollection.Add: TinfoCREstabCollectionItem;
begin
  Result := Self.New;
end;

function TinfoCREstabCollection.GetItem(
  Index: Integer): TinfoCREstabCollectionItem;
begin
  Result := TinfoCREstabCollectionItem(inherited Items[Index]);
end;

procedure TinfoCREstabCollection.SetItem(Index: Integer;
  Value: TinfoCREstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoCREstabCollection.New: TinfoCREstabCollectionItem;
begin
  Result := TinfoCREstabCollectionItem.Create;
  Self.Add(Result);
end;

{ TideEstabCollectionItem }

constructor TideEstabCollectionItem.Create;
begin
  inherited Create;

  FInfoEstab   := TInfoEstab.Create;
  FideLotacao  := TideLotacaoCollection.Create;
  FbasesAquis  := TbasesAquiscollection.Create;
  FbasesComerc := TbasesComercCollection.Create;
  FinfoCREstab := TinfoCREstabCollection.Create;
end;

destructor TideEstabCollectionItem.Destroy;
begin
  FInfoEstab.Free;
  FideLotacao.Free;
  FbasesAquis.Free;
  FbasesComerc.Free;
  FinfoCREstab.Free;

  inherited;
end;

{ TideLotacaoCollection }

function TideLotacaoCollection.Add: TideLotacaoCollectionItem;
begin
  Result := Self.New;
end;

function TideLotacaoCollection.GetItem(
  Index: Integer): TideLotacaoCollectionItem;
begin
  Result := TideLotacaoCollectionItem(inherited Items[Index]);
end;

procedure TideLotacaoCollection.SetItem(Index: Integer;
  Value: TideLotacaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TideLotacaoCollection.New: TideLotacaoCollectionItem;
begin
  Result := TideLotacaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoTercSuspCollection }

function TinfoTercSuspCollection.Add: TinfoTercSuspCollectionItem;
begin
  Result := Self.New;
end;

function TinfoTercSuspCollection.GetItem(
  Index: Integer): TinfoTercSuspCollectionItem;
begin
  Result := TinfoTercSuspCollectionItem(inherited Items[Index]);
end;

procedure TinfoTercSuspCollection.SetItem(Index: Integer;
  Value: TinfoTercSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoTercSuspCollection.New: TinfoTercSuspCollectionItem;
begin
  Result := TInfoTercSuspCollectionItem.Create;
  Self.Add(Result);
end;

{ TbasesRemunCollection }

function TbasesRemunCollection.Add: TbasesRemunCollectionItem;
begin
  Result := Self.New;
end;

function TbasesRemunCollection.GetItem(
  Index: Integer): TbasesRemunCollectionItem;
begin
  Result := TbasesRemunCollectionItem(inherited Items[Index]);
end;

procedure TbasesRemunCollection.SetItem(Index: Integer;
  Value: TbasesRemunCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbasesRemunCollection.New: TbasesRemunCollectionItem;
begin
  Result := TbasesremunCollectionItem.Create;
  Self.Add(Result);
end;

{ TbasesRemunCollectionItem }

constructor TbasesRemunCollectionItem.Create;
begin
  inherited Create;
  FbasesCp := TbasesCp.Create;
end;

destructor TbasesRemunCollectionItem.Destroy;
begin
  FbasesCp.Free;

  inherited;
end;

{ TinfoSubstPatrOpPortCollection }

function TinfoSubstPatrOpPortCollection.Add: TinfoSubstPatrOpPortCollectionItem;
begin
  Result := Self.New;
end;

function TinfoSubstPatrOpPortCollection.GetItem(
  Index: Integer): TinfoSubstPatrOpPortCollectionItem;
begin
  Result := TinfoSubstPatrOpPortCollectionItem(inherited Items[Index]);
end;

procedure TinfoSubstPatrOpPortCollection.SetItem(Index: Integer;
  Value: TinfoSubstPatrOpPortCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoSubstPatrOpPortCollection.New: TinfoSubstPatrOpPortCollectionItem;
begin
  Result := TinfoSubstPatrOpPortCollectionItem.Create;
  Self.Add(Result);
end;

{ TideLotacaoCollectionItem }

constructor TideLotacaoCollectionItem.Create;
begin
  inherited Create;

  FinfoTercSusp        := TinfoTercSuspCollection.Create;
  FInfoEmprParcial     := TInfoEmprParcial.Create;
  FdadosOpPort         := TdadosOpPort.Create;
  Fbasesremun          := TbasesremunCollection.Create;
  FbasesAvNport        := TbasesAvNport.Create;
  FinfoSubstPatrOpPort := TinfoSubstPatrOpPortCollection.Create;
end;

destructor TideLotacaoCollectionItem.Destroy;
begin
  FinfoTercSusp.Free;
  FInfoEmprParcial.Free;
  FdadosOpPort.Free;
  Fbasesremun.Free;
  FbasesAvNport.Free;
  FinfoSubstPatrOpPort.Free;

  inherited;
end;

{ TInfoEstab }

constructor TInfoEstab.Create;
begin
  inherited Create;
  FinfoComplObra := TinfoComplObra.Create;
end;

destructor TInfoEstab.Destroy;
begin
  FinfoComplObra.Free;

  inherited;
end;

{ TInfoCRContribCollection }

function TInfoCRContribCollection.Add: TInfoCRContribCollectionItem;
begin
  Result := Self.New;
end;

function TInfoCRContribCollection.GetItem(
  Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited Items[Index]);
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer;
  Value: TInfoCRContribCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCRContribCollection.New: TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem.Create;
  Self.Add(Result);
end;

end.
