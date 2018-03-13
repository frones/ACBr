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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit pcesS5011;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TS5011 = class;

  TInfoCS = class;
  TInfoCPSeg = class;
  TInfoContrib = class;
  TideEstabCollection = class;
  TideEstabCollectionItem = class;
  TideLotacaoCollection = class;
  TideLotacaoCollectionItem = class;
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;
  TInfoTercSuspCollection = class;
  TInfoTercSuspCollectionItem = class;
  TbasesRemunCollection = class;
  TbasesremunCollectionItem = class;
  TinfoSubstPartOpPortCollection = class;
  TinfoSubstPartOpPortCollectionItem = class;
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
    procedure SetEvtCS(const Value: TEvtCS);

  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtCS: TEvtCS read FEvtCS write setEvtCS;

  end;

  TInfoCPSeg = class(TPersistent)
  private
    FvrDescCP: Double;
    FvrCpSeg: Double;
  public
    property vrDescCP: Double read FvrDescCP write FvrDescCP;
    property vrCpSeg: Double read FvrCpSeg write FvrCpSeg;
  end;

  TInfoAtConc = class(TPersistent)
  private
    FfatorMes: Double;
    Ffator13: Double;
  public
    property fatorMes: Double read FfatorMes write FfatorMes;
    property fator13: Double read Ffator13 write Ffator13;
  end;

  TInfoPJ = class(TPersistent)
  private
    FindCoop: Integer;
    FindConstr: Integer;
    FindSubstPart: Integer;
    FpercRedContrib: Double;
    FinfoAtConc: TinfoAtConc;
  public
    constructor Create(AOwner: TInfoContrib); reintroduce;
    destructor Destroy; override;

    property indCoop: Integer read FindCoop write FindCoop;
    property indConstr: Integer read FindConstr write FindConstr;
    property indSubstPart: Integer read FindSubstPart write FindSubstPart;
    property percRedContrib: Double read FpercRedContrib write FpercRedContrib;
    property infoAtConc: TinfoAtConc read FinfoAtConc write FinfoAtConc;
  end;

  TInfoContrib = class(TPersistent)
  private
    FclassTrib: String;
    FinfoPJ: TInfoPJ;
  public
    constructor Create(AOwner: TInfoCS); reintroduce;
    destructor Destroy; override;

    property classTrib: String read FclassTrib write FclassTrib;
    property infoPJ: TInfoPJ read FinfoPJ write FinfoPJ;
  end;

  TInfoComplObra = class(TPersistent)
  private
    FindSubstPartObra: Integer;
  public
    property indSubstPartObra: Integer read FindSubstPartObra write FindSubstPartObra;
  end;

  TInfoEstab = class(TPersistent)
  private
    FFap: Double;
    FAliqRatAjust: Double;
    FcnaePrep: String;
    FAliqRat: tpAliqRat;
    FinfoComplObra: TInfoComplObra;
  public
    constructor Create(AOwner: TideEstabCollectionItem); reintroduce;
    destructor Destroy; override;

    property cnaePrep: String read FcnaePrep write FcnaePrep;
    property AliqRat: tpAliqRat read FAliqRat write FAliqRat;
    property fap: Double read FFap write FFap;
    property aliqRatAjust: Double read FAliqRatAjust write FAliqRatAjust;
    property infoComplObra: TInfoComplObra read FinfoComplObra write FinfoComplObra;
  end;

  TinfoTercSuspCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoTercSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoTercSuspCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TinfoTercSuspCollectionItem;
    property Items[Index: Integer]: TinfoTercSuspCollectionItem read GetItem write SetItem;
  end;

  TinfoTercSuspCollectionItem = class(TCollectionItem)
  private
    FcodTerc: String;
  public
    property codTerc: String read FcodTerc write FcodTerc;
  end;

  TideLotacaoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TideLotacaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TideLotacaoCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TideLotacaoCollectionItem;
    property Items[Index: Integer]: TideLotacaoCollectionItem read GetItem write SetItem;
  end;

  TInfoEmprParcial = class(TPersistent)
  private
    FtpinscContrat: Integer;
    FnrInscContrat: String;
    FtpInscProp: Integer;
    FnrInscProp: String;
  public
    property tpInscContrat: Integer read FtpinscContrat write FtpinscContrat;
    property nrInscContrat: String read FnrInscContrat write FnrInscContrat;
    property tpInscProp: Integer read FtpInscProp write FtpInscProp;
    property nrInscProp: String read FnrInscProp write FnrInscProp;
  end;

  TdadosOpPort = class(TPersistent)
  private
    FFap: Double;
    FAliqRatAjust: Double;
    FcnpjOpPortuario: String;
    FAliqRat: tpAliqRat;
  public
    property cnpjOpPortuario: String read FcnpjOpPortuario write FcnpjOpPortuario;
    property AliqRat: tpAliqRat read FAliqRat write FAliqRat;
    property fap: Double read FFap write FFap;
    property aliqRatAjust: Double read FAliqRatAjust write FAliqRatAjust;
  end;

  TbasesCp = class(TPersistent)
  private
    FvrBcCp25: Double;
    FvrBcCp15: Double;
    FvrBcCp20: Double;
    FvrBcCp00: Double;
    FvrSuspBcCp00: Double;
    FvrSuspBcCp25: Double;
    FvrSuspBcCp15: Double;
    FvrSuspBcCp20: Double;
    FvrCalcSest: Double;
    FvrSalFam: Double;
    FvrDescSenat: Double;
    FvrCalcSenat: Double;
    FvrSalMat: Double;
    FvrDescSest: Double;
  public
    property vrBcCp00: Double read FvrBcCp00 write FvrBcCp00;
    property vrBcCp15: Double read FvrBcCp15 write FvrBcCp15;
    property vrBcCp20: Double read FvrBcCp20 write FvrBcCp20;
    property vrBcCp25: Double read FvrBcCp25 write FvrBcCp25;
    property vrSuspBcCp00: Double read FvrSuspBcCp00 write FvrSuspBcCp00;
    property vrSuspBcCp15: Double read FvrSuspBcCp15 write FvrSuspBcCp15;
    property vrSuspBcCp20: Double read FvrSuspBcCp20 write FvrSuspBcCp20;
    property vrSuspBcCp25: Double read FvrSuspBcCp25 write FvrSuspBcCp25;
    property vrDescSest: Double read FvrDescSest write FvrDescSest;
    property vrCalcSest: Double read FvrCalcSest write FvrCalcSest;
    property vrDescSenat: Double read FvrDescSenat write FvrDescSenat;
    property vrCalcSenat: Double read FvrCalcSenat write FvrCalcSenat;
    property vrSalFam: Double read FvrSalFam write FvrSalFam;
    property vrSalMat: Double read FvrSalMat write FvrSalMat;
  end;

  TbasesRemunCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TbasesRemunCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesRemunCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TbasesRemunCollectionItem;
    property Items[Index: Integer]: TbasesRemunCollectionItem read GetItem write SetItem;
  end;

  TbasesRemunCollectionItem = class(TCollectionItem)
  private
    FindIncid: Integer;
    FcodCateg: Integer;
    FbasesCp: TbasesCp;
  public
    constructor Create(AOwner: TideLotacaoCollectionItem); reintroduce;
    destructor Destroy; override;

    property indIncid: Integer read FindIncid write FindIncid;
    property codCateg: Integer read FcodCateg write FcodCateg;
    property basesCp: TbasesCp read FbasesCp write FbasesCp;
  end;

  TbasesAvNport = class(TPersistent)
  private
    FvrBcCp25: Double;
    FvrBcCp15: Double;
    FvrBcCp00: Double;
    FvrBcCp20: Double;
    FvrBcCp13: Double;
    FvrDescCP: Double;
    FvrBcFgts: Double;
  public
    property vrBcCp00: Double read FvrBcCp00 write FvrBcCp00;
    property vrBcCp15: Double read FvrBcCp15 write FvrBcCp15;
    property vrBcCp20: Double read FvrBcCp20 write FvrBcCp20;
    property vrBcCp25: Double read FvrBcCp25 write FvrBcCp25;
    property vrBcCp13: Double read FvrBcCp13 write FvrBcCp13;
    property vrBcFgts: Double read FvrBcFgts write FvrBcFgts;
    property vrDescCP: Double read FvrDescCP write FvrDescCP;
  end;

  TinfoSubstPartOpPortCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoSubstPartOpPortCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoSubstPartOpPortCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TinfoSubstPartOpPortCollectionItem;
    property Items[Index: Integer]: TinfoSubstPartOpPortCollectionItem read GetItem write SetItem;
  end;

  TinfoSubstPartOpPortCollectionItem = class(TCollectionItem)
  private
    FcnpjOpPortuario: String;
  public
    property cnpjOpPortuario: String read FcnpjOpPortuario write FcnpjOpPortuario;
  end;

  TideLotacaoCollectionItem = class(TCollectionItem)
  private
    Ffpas: Integer;
    FcodLotacao: String;
    FcodTercs: String;
    FcodTercsSusp: String;
    FinfoTercSusp: TinfoTercSuspCollection;
    FInfoEmprParcial: TInfoEmprParcial;
    FdadosOpPort: TdadosOpPort;
    Fbasesremun: TbasesremunCollection;
    FbasesAvNport: TbasesAvNport;
    FinfoSubstPartOpPort: TinfoSubstPartOpPortCollection;
  public
    constructor Create(AOwner: TideEstabCollectionItem); reintroduce;
    destructor Destroy; override;

    property codLotacao: String read FcodLotacao write FcodLotacao;
    property fpas: Integer read Ffpas write Ffpas;
    property codTercs: String read FcodTercs write FcodTercs;
    property codTercsSusp: String read FcodTercsSusp write FcodTercsSusp;
    property infoTercSusp: TinfoTercSuspCollection read FinfoTercSusp write FinfoTercSusp;
    property InfoEmprParcial: TInfoEmprParcial read FInfoEmprParcial write FInfoEmprParcial;
    property dadosOpPort: TdadosOpPort read FdadosOpPort write FdadosOpPort;
    property basesremun: TbasesremunCollection read Fbasesremun write Fbasesremun;
    property basesAvNPort: TbasesAvNport read FbasesAvNport write FbasesAvNport;
    property infoSubstPartOpPort: TinfoSubstPartOpPortCollection read FinfoSubstPartOpPort write FinfoSubstPartOpPort;
  end;

  TideEstabCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TideEstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TideEstabCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TideEstabCollectionItem;
    property Items[Index: Integer]: TideEstabCollectionItem read GetItem write SetItem;
  end;

  TbasesAquisCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TbasesAquisCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesAquisCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TbasesAquisCollectionItem;
    property Items[Index: Integer]: TbasesAquisCollectionItem read GetItem write SetItem;
  end;

  TbasesAquisCollectionItem = class(TCollectionItem)
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
    property indAquis: Integer read FindAquis write FindAquis;
    property vlrAquis: Double read FvlrAquis write FvlrAquis;
    property vrCPDescPR: Double read FvrCPDescPR write FvrCPDescPR;
    property vrCPNRet: Double read FvrCPNRet write FvrCPNRet;
    property vrRatNRet: Double read FvrRatNRet write FvrRatNRet;
    property vrSenarNRet: Double read FvrSenarNRet write FvrSenarNRet;
    property vrCPCalcPR: Double read FvrCPCalcPR write FvrCPCalcPR;
    property vrRatDescPR: Double read FvrRatDescPR write FvrRatDescPR;
    property vrRatCalcPR: Double read FvrRatCalcPR write FvrRatCalcPR;
    property vrSenarDesc: Double read FvrSenarDesc write FvrSenarDesc;
    property vrSenarCalc: Double read FvrSenarCalc write FvrSenarCalc;
  end;

  TbasesComercCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TbasesComercCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesComercCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TbasesComercCollectionItem;
    property Items[Index: Integer]: TbasesComercCollectionItem read GetItem write SetItem;
  end;

  TbasesComercCollectionItem = class(TCollectionItem)
  private
    FvrCPSusp: Double;
    FvrRatSusp: Double;
    FvrSenarSusp: Double;
    FvrBcComPr: Double;
    FindComerc: Integer;
  public
    property indComerc: Integer read FindComerc write FindComerc;
    property vrBcComPr: Double read FvrBcComPr write FvrBcComPr;
    property vrCPSusp: Double read FvrCPSusp write FvrCPSusp;
    property vrRatSusp: Double read FvrRatSusp write FvrRatSusp;
    property vrSenarSusp: Double read FvrSenarSusp write FvrSenarSusp;
  end;

  TinfoCREstabCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoCREstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoCREstabCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TinfoCREstabCollectionItem;
    property Items[Index: Integer]: TinfoCREstabCollectionItem read GetItem write SetItem;
  end;

  TinfoCREstabCollectionItem = class(TCollectionItem)
  private
    FvrSuspCR: Double;
    FvrCR: Double;
    FtpCR: Integer;
  public
    property tpCR: Integer read FtpCR write FtpCR;
    property vrCR: Double read FvrCR write FvrCR;
    property vrSuspCR: Double read FvrSuspCR write FvrSuspCR;
  end;

  TideEstabCollectionItem = class(TCollectionItem)
  private
    FNrInsc: string;
    FTpInsc: tpTpInsc;
    FinfoEstab: TinfoEstab;
    FideLotacao: TideLotacaoCollection;
    FbasesAquis: TbasesAquisCollection;
    FbasesComerc: TbasesComercCollection;
    FinfoCREstab: TinfoCREstabCollection;
  public
    constructor Create(AOwner: TInfoCS); reintroduce;
    destructor Destroy; override;

    property TpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property NrInsc: string read FNrInsc write FNrInsc;
    property infoEstab: TinfoEstab read FinfoEstab write FinfoEstab;
    property ideLotacao: TideLotacaoCollection read FideLotacao write FideLotacao;
    property basesAquis: TbasesAquisCollection read FbasesAquis write FbasesAquis;
    property basesComerc: TbasesComercCollection read FbasesComerc write FbasesComerc;
    property infoCREstab: TinfoCREstabCollection read FinfoCREstab write FinfoCREstab;
  end;

  TInfoCRContribCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem;
  end;

  TInfoCRContribCollectionItem = class(TCollectionItem)
  private
    FtpCR: string;
    FvrCR: Double;
    FvrCRSusp: Double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrCR: Double read FvrCR write FvrCR;
    property vrCRSusp: Double read FvrCRSusp write FvrCRSusp;
  end;

  TInfoCS = class(TPersistent)
  private
    FnrRecArqBase: String;
    FindExistInfo: Integer;
    FInfoCpSeg: TInfoCpSeg;
    FInfoContrib: TInfoContrib;
    FideEstab: TideEstabCollection;
    FinfoCRContrib: TinfoCRContribCollection;
  public
    constructor Create(AOwner: TEvtCS);
    destructor Destroy; override;

    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property indExistInfo: Integer read FindExistInfo write FindExistInfo;
    property InfoCpSeg: TInfoCpSeg read FInfoCpSeg write FInfoCpSeg;
    property InfoContrib: TInfoContrib read FInfoContrib write FInfoContrib;
    property ideEstab: TideEstabCollection read FideEstab write FideEstab;
    property infoCRContrib: TinfoCRContribCollection read FinfoCRContrib write FinfoCRContrib;
  end;

  TEvtCS = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TIdeTrabalhador3;
    FInfoCS: TInfoCS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TIdeTrabalhador3 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoCS: TInfoCS read FInfoCS write FInfoCS;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

implementation

{ TS5011 }

constructor TS5011.Create;
begin
  FTipoEvento := teS5011;
  FEvtCS := TEvtCS.Create;
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

  FEvtCS.XML := Value;
  FEvtCS.Leitor.Arquivo := Value;
  FEvtCS.LerXML;

end;

function TS5011.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TS5011.SetEvtCS(const Value: TEvtCS);
begin
  FEvtCS.Assign(Value);
end;

{ TEvtCS }

constructor TEvtCS.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TIdeTrabalhador3.Create;
  FInfoCS := TInfoCS.Create(Self);
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

{ TInfoCS }

constructor TInfoCS.Create;
begin
  FInfoCpSeg := TInfoCpSeg.Create;
  FInfoContrib := TInfoContrib.Create(Self);
  FideEstab := TideEstabCollection.Create;
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

constructor TInfoContrib.Create(AOwner: TInfoCS);
begin
  FInfoPJ := TInfoPJ.Create(Self);
end;

destructor TInfoContrib.Destroy;
begin
  FInfoPJ.Free;

  inherited;
end;

{ TInfoPJ }

constructor TInfoPJ.Create(AOwner: TInfoContrib);
begin
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
  Result := TideEstabCollectionItem(inherited Add);
end;

constructor TideEstabCollection.Create;
begin
  inherited create(TideEstabCollectionItem);
end;

function TideEstabCollection.GetItem(
  Index: Integer): TideEstabCollectionItem;
begin
  Result := TideEstabCollectionItem(inherited GetItem(Index));
end;

procedure TideEstabCollection.SetItem(Index: Integer;
  Value: TideEstabCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TbasesAquisCollection }

function TbasesAquisCollection.Add: TbasesAquisCollectionItem;
begin
  Result := TbasesAquisCollectionItem(inherited Add);
end;

constructor TbasesAquisCollection.Create;
begin
  inherited create(TbasesAquisCollectionItem);
end;

function TbasesAquisCollection.GetItem(
  Index: Integer): TbasesAquisCollectionItem;
begin
  Result := TbasesAquisCollectionItem(inherited GetItem(Index));
end;

procedure TbasesAquisCollection.SetItem(Index: Integer;
  Value: TbasesAquisCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TbasesComercCollection }

function TbasesComercCollection.Add: TbasesComercCollectionItem;
begin
  Result := TbasesComercCollectionItem(inherited Add);
end;

constructor TbasesComercCollection.Create;
begin
  inherited create(TbasesComercCollectionItem);
end;

function TbasesComercCollection.GetItem(
  Index: Integer): TbasesComercCollectionItem;
begin
  Result := TbasesComercCollectionItem(inherited GetItem(Index));
end;

procedure TbasesComercCollection.SetItem(Index: Integer;
  Value: TbasesComercCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfoCREstabCollection }

function TinfoCREstabCollection.Add: TinfoCREstabCollectionItem;
begin
  Result := TinfoCREstabCollectionItem(inherited Add);
end;

constructor TinfoCREstabCollection.Create;
begin
  inherited create(TinfoCREstabCollectionItem);
end;

function TinfoCREstabCollection.GetItem(
  Index: Integer): TinfoCREstabCollectionItem;
begin
  Result := TinfoCREstabCollectionItem(inherited GetItem(Index));
end;

procedure TinfoCREstabCollection.SetItem(Index: Integer;
  Value: TinfoCREstabCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TideEstabCollectionItem }

constructor TideEstabCollectionItem.Create(AOwner: TInfoCS);
begin
  FInfoEstab := TInfoEstab.Create(Self);
  FideLotacao := TideLotacaoCollection.Create;
  FbasesAquis := TbasesAquiscollection.Create;
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
  Result := TideLotacaoCollectionItem(inherited Add);
end;

constructor TideLotacaoCollection.Create;
begin
  inherited create(TideLotacaoCollectionItem);
end;

function TideLotacaoCollection.GetItem(
  Index: Integer): TideLotacaoCollectionItem;
begin
  Result := TideLotacaoCollectionItem(inherited GetItem(Index));
end;

procedure TideLotacaoCollection.SetItem(Index: Integer;
  Value: TideLotacaoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfoTercSuspCollection }

function TinfoTercSuspCollection.Add: TinfoTercSuspCollectionItem;
begin
  Result := TinfoTercSuspCollectionItem(inherited Add);
end;

constructor TinfoTercSuspCollection.Create;
begin
  inherited create(TinfoTercSuspCollectionItem);
end;

function TinfoTercSuspCollection.GetItem(
  Index: Integer): TinfoTercSuspCollectionItem;
begin
  Result := TinfoTercSuspCollectionItem(inherited GetItem(Index));
end;

procedure TinfoTercSuspCollection.SetItem(Index: Integer;
  Value: TinfoTercSuspCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TbasesRemunCollection }

function TbasesRemunCollection.Add: TbasesRemunCollectionItem;
begin
  Result := TbasesRemunCollectionItem(inherited Add);
end;

constructor TbasesRemunCollection.Create;
begin
  inherited create(TbasesRemunCollectionItem);
end;

function TbasesRemunCollection.GetItem(
  Index: Integer): TbasesRemunCollectionItem;
begin
  Result := TbasesRemunCollectionItem(inherited GetItem(Index));
end;

procedure TbasesRemunCollection.SetItem(Index: Integer;
  Value: TbasesRemunCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TbasesRemunCollectionItem }

constructor TbasesRemunCollectionItem.Create(
  AOwner: TideLotacaoCollectionItem);
begin
  FbasesCp := TbasesCp.Create;
end;

destructor TbasesRemunCollectionItem.Destroy;
begin
  FbasesCp.Free;

  inherited;
end;

{ TinfoSubstPartOpPortCollection }

function TinfoSubstPartOpPortCollection.Add: TinfoSubstPartOpPortCollectionItem;
begin
  Result := TinfoSubstPartOpPortCollectionItem(inherited Add);
end;

constructor TinfoSubstPartOpPortCollection.Create;
begin
  inherited create(TinfoSubstPartOpPortCollectionItem);
end;

function TinfoSubstPartOpPortCollection.GetItem(
  Index: Integer): TinfoSubstPartOpPortCollectionItem;
begin
  Result := TinfoSubstPartOpPortCollectionItem(inherited GetItem(Index));
end;

procedure TinfoSubstPartOpPortCollection.SetItem(Index: Integer;
  Value: TinfoSubstPartOpPortCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TideLotacaoCollectionItem }

constructor TideLotacaoCollectionItem.Create(
  AOwner: TideEstabCollectionItem);
begin
  FinfoTercSusp := TinfoTercSuspCollection.Create;
  FInfoEmprParcial := TInfoEmprParcial.Create;
  FdadosOpPort := TdadosOpPort.Create;
  Fbasesremun := TbasesremunCollection.Create;
  FbasesAvNport := TbasesAvNport.Create;
  FinfoSubstPartOpPort := TinfoSubstPartOpPortCollection.Create;
end;

destructor TideLotacaoCollectionItem.Destroy;
begin
  FinfoTercSusp.Free;
  FInfoEmprParcial.Free;
  FdadosOpPort.Free;
  Fbasesremun.Free;
  FbasesAvNport.Free;
  FinfoSubstPartOpPort.Free;

  inherited;
end;

{ TInfoEstab }

constructor TInfoEstab.Create(AOwner: TideEstabCollectionItem);
begin
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
  Result := TInfoCRContribCollectionItem(inherited Add);
end;

constructor TInfoCRContribCollection.Create;
begin
  inherited create(TInfoCRContribCollectionItem);
end;

function TInfoCRContribCollection.GetItem(
  Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited GetItem(Index));
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer;
  Value: TInfoCRContribCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TEvtCS.LerXML: boolean;
var
  ok: Boolean;
  i, j, k: Integer;
begin
  Result := False;
  try
    XML := Leitor.Arquivo;

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
        infoCS.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoCS.indExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        if leitor.rExtrai(3, 'infoCPSeg') <> '' then
        begin
          infoCS.InfoCpSeg.vrDescCP := leitor.rCampo(tcDe2, 'vrDescCP');
          infoCS.InfoCpSeg.vrCpSeg  := leitor.rCampo(tcDe2, 'vrCpSeg');
        end;

        if leitor.rExtrai(3, 'infoContrib') <> '' then
        begin
          infoCS.InfoContrib.classTrib := leitor.rCampo(tcStr, 'classTrib');

          if leitor.rExtrai(4, 'infoPJ') <> '' then
          begin
            infoCS.InfoContrib.infoPJ.indCoop        := leitor.rCampo(tcInt, 'indCoop');
            infoCS.InfoContrib.infoPJ.indConstr      := leitor.rCampo(tcInt, 'indConstr');
            infoCS.InfoContrib.infoPJ.indSubstPart   := leitor.rCampo(tcInt, 'indSubstPart');
            infoCS.InfoContrib.infoPJ.percRedContrib := leitor.rCampo(tcDe2, 'percRedContrib');

            if leitor.rExtrai(5, 'infoAtConc') <> '' then
            begin
              infoCS.InfoContrib.infoPJ.infoAtConc.fatorMes := leitor.rCampo(tcDe4, 'fatorMes');
              infoCS.InfoContrib.infoPJ.infoAtConc.fator13  := leitor.rCampo(tcDe2, 'fator13');
            end;
          end;
        end;

        i := 0;
        while Leitor.rExtrai(3, 'ideEstab', '', i + 1) <> '' do
        begin
          infoCS.ideEstab.Add;
          infoCS.ideEstab.Items[i].TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
          infoCS.ideEstab.Items[i].NrInsc := leitor.rCampo(tcStr, 'nrInsc');

          if leitor.rExtrai(4, 'infoEstab') <> '' then
          begin
            infoCS.ideEstab.Items[i].infoEstab.cnaePrep     := leitor.rCampo(tcStr, 'cnaePrep');
            infoCS.ideEstab.Items[i].infoEstab.AliqRat      := eSStrToAliqRat(ok, leitor.rCampo(tcStr, 'AliqRat'));
            infoCS.ideEstab.Items[i].infoEstab.fap          := leitor.rCampo(tcDe4, 'fap');
            infoCS.ideEstab.Items[i].infoEstab.aliqRatAjust := leitor.rCampo(tcDe4, 'aliqRatAjust');

            if leitor.rExtrai(5, 'infoComplObra') <> '' then
              infoCS.ideEstab.Items[i].infoEstab.infoComplObra.indSubstPartObra := leitor.rCampo(tcInt, 'indSubstPartObra');

            j := 0;
            while Leitor.rExtrai(5, 'ideLotacao', '', i + 1) <> '' do
            begin
              infoCS.ideEstab.Items[i].ideLotacao.Add;
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].codLotacao   := leitor.rCampo(tcStr, 'codLotacao');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].fpas         := leitor.rCampo(tcInt, 'fpas');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].codTercs     := leitor.rCampo(tcStr, 'codTercs');
              infoCS.ideEstab.Items[i].ideLotacao.Items[j].codTercsSusp := leitor.rCampo(tcStr, 'codTercsSusp');

              k := 0;
              while Leitor.rExtrai(6, 'infoTercSusp', '', i + 1) <> '' do
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoTercSusp.Add;
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoTercSusp.Items[k].codTerc := leitor.rCampo(tcStr, 'codTerc');
                inc(k);
              end;

              if leitor.rExtrai(6, 'infoEmprParcial') <> '' then
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.tpInscContrat := leitor.rCampo(tcInt, 'tpInscContrat');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.nrInscContrat := leitor.rCampo(tcStr, 'nrInscContrat');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.tpInscProp    := leitor.rCampo(tcInt, 'tpInscProp');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].InfoEmprParcial.nrInscProp    := leitor.rCampo(tcStr, 'nrInscProp');
              end;

              if leitor.rExtrai(6, 'dadosOpPort') <> '' then
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.cnpjOpPortuario := leitor.rCampo(tcStr, 'cnpjOpPortuario');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.AliqRat         := eSStrToAliqRat(ok, leitor.rCampo(tcStr, 'AliqRat'));
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.fap             := leitor.rCampo(tcDe4, 'fap');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].dadosOpPort.aliqRatAjust    := leitor.rCampo(tcDe4, 'aliqRatAjust');
              end;

              k := 0;
              while Leitor.rExtrai(6, 'basesRemun', '', i + 1) <> '' do
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Add;
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].indIncid := leitor.rCampo(tcInt, 'indIncid');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].codCateg := leitor.rCampo(tcInt, 'codCateg');

                if leitor.rExtrai(7, 'basesCp') <> '' then
                begin
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrBcCp00 := leitor.rCampo(tcDe2, 'vrBcCp00');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrBcCp15 := leitor.rCampo(tcDe2, 'vrBcCp15');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrBcCp20 := leitor.rCampo(tcDe2, 'vrBcCp20');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrBcCp25 := leitor.rCampo(tcDe2, 'vrBcCp25');

                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSuspBcCp00 := leitor.rCampo(tcDe2, 'vrSuspBcCp00');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSuspBcCp15 := leitor.rCampo(tcDe2, 'vrSuspBcCp15');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSuspBcCp20 := leitor.rCampo(tcDe2, 'vrSuspBcCp20');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSuspBcCp25 := leitor.rCampo(tcDe2, 'vrSuspBcCp25');

                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrDescSest  := leitor.rCampo(tcDe2, 'vrDescSest');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrCalcSest  := leitor.rCampo(tcDe2, 'vrCalcSest');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrDescSenat := leitor.rCampo(tcDe2, 'vrDescSenat');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrCalcSenat := leitor.rCampo(tcDe2, 'vrCalcSenat');

                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSalFam := leitor.rCampo(tcDe2, 'vrSalFam');
                  infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesRemun.Items[k].basesCp.vrSalMat := leitor.rCampo(tcDe2, 'vrSalMat');
                end;

                inc(k);
              end;

              if leitor.rExtrai(6, 'basesAvNPort') <> '' then
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcCp00 := leitor.rCampo(tcDe2, 'vrBcCp00');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcCp15 := leitor.rCampo(tcDe2, 'vrBcCp15');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcCp20 := leitor.rCampo(tcDe2, 'vrBcCp20');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcCp25 := leitor.rCampo(tcDe2, 'vrBcCp25');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcCp13 := leitor.rCampo(tcDe2, 'vrBcCp13');

                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrBcFgts := leitor.rCampo(tcDe2, 'vrBcFgts');
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].basesAvNPort.vrDescCP := leitor.rCampo(tcDe2, 'vrDescCP');
              end;

              k := 0;
              while Leitor.rExtrai(6, 'infoSubstPartOpPort', '', i + 1) <> '' do
              begin
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoSubstPartOpPort.Add;
                infoCS.ideEstab.Items[i].ideLotacao.Items[j].infoSubstPartOpPort.Items[k].cnpjOpPortuario := leitor.rCampo(tcStr, 'cnpjOpPortuario');
                inc(k);
              end;

              inc(j);
            end;
          end;

          j := 0;
          while Leitor.rExtrai(4, 'basesAquis', '', i + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].basesAquis.Add;
            infoCS.ideEstab.Items[i].basesAquis.Items[j].indAquis    := leitor.rCampo(tcInt, 'indAquis');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vlrAquis    := leitor.rCampo(tcDe2, 'vlrAquis');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrCPDescPR  := leitor.rCampo(tcDe2, 'vrCPDescPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrCPNRet    := leitor.rCampo(tcDe2, 'vrCPNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrRatNRet   := leitor.rCampo(tcDe2, 'vrRatNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrSenarNRet := leitor.rCampo(tcDe2, 'vrSenarNRet');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrCPCalcPR  := leitor.rCampo(tcDe2, 'vrCPCalcPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrRatDescPR := leitor.rCampo(tcDe2, 'vrRatDescPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrRatCalcPR := leitor.rCampo(tcDe2, 'vrRatCalcPR');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrSenarDesc := leitor.rCampo(tcDe2, 'vrSenarDesc');
            infoCS.ideEstab.Items[i].basesAquis.Items[j].vrSenarCalc := leitor.rCampo(tcDe2, 'vrSenarCalc');
            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(4, 'basesComerc', '', i + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].basesComerc.Add;
            infoCS.ideEstab.Items[i].basesComerc.Items[j].indComerc   := leitor.rCampo(tcInt, 'indComerc');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].vrBcComPr   := leitor.rCampo(tcDe2, 'vrBcComPr');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].vrCPSusp    := leitor.rCampo(tcDe2, 'vrCPSusp');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].vrRatSusp   := leitor.rCampo(tcDe2, 'vrRatSusp');
            infoCS.ideEstab.Items[i].basesComerc.Items[j].vrSenarSusp := leitor.rCampo(tcDe2, 'vrSenarSusp');
            inc(j);
          end;

          j := 0;
          while Leitor.rExtrai(4, 'infoCREstab', '', i + 1) <> '' do
          begin
            infoCS.ideEstab.Items[i].infoCREstab.Add;
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].tpCR     := leitor.rCampo(tcInt, 'tpCR');
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].vrCR     := leitor.rCampo(tcDe2, 'vrCR');
            infoCS.ideEstab.Items[i].infoCREstab.Items[j].vrSuspCR := leitor.rCampo(tcDe2, 'vrSuspCR');
            inc(j);
          end;

          inc(i);
        end;

        i := 0;
        while Leitor.rExtrai(3, 'infoCRContrib', '', i + 1) <> '' do
        begin
          infoCS.infoCRContrib.Add;
          infoCS.infoCRContrib.Items[i].tpCR     := leitor.rCampo(tcStr, 'tpCR');
          infoCS.infoCRContrib.Items[i].vrCR     := leitor.rCampo(tcDe2, 'vrCR');
          infoCS.infoCRContrib.Items[i].vrCRSusp := leitor.rCampo(tcDe2, 'vrCRSusp');
          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
