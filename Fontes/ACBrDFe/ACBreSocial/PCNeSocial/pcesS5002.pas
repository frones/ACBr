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

unit pcesS5002;

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
  TInfoIrrfCollectionItem = class;
  TbasesIrrfCollection = class;
  TbasesIrrfCollectionItem = class;
  TirrfCollection = class;
  TirrfCollectionItem = class;
  TideTrabalhador4 = class;
  TdmDevCollection = class;
  TdmDevCollectionItem = class;
  TinfoirCollection = class;
  TinfoirCollectionItem = class;
  TidePgtoExt = class;
  TEvtIrrfBenef = class;
  TtotApurMenCollection = class;
  TtotApurMenCollectionItem = class;
  TtotApurDiaCollection = class;
  TtotApurDiaCollectionItem = class;
  TInfoPgtoExt = class;
  TInfoProcJudRubCollection = class;
  TInfoProcJudRubCollectionItem = class;
  TinfoIRComplem = class;
  TideDepCollection = class;
  TideDepCollectionItem = class;
  TinfoIRCRCollection = class;
  TinfoIRCRCollectionItem = class;
  TdedDepenCollection = class;
  TdedDepenCollectionItem = class;
  TpenAlimCollection = class;
  TpenAlimCollectionItem = class;
  TprevidComplCollection = class;
  TprevidComplCollectionItem = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;
  TinfoValoresCollection = class;
  TinfoValoresCollectionItem = class;
  TdedSuspCollection = class;
  TdedSuspCollectionItem = class;
  TbenefPenCollection = class;
  TbenefPenCollectionItem = class;
  TplanSaudeCollection = class;
  TplanSaudeCollectionItem = class;
  TinfoDepSauCollection = class;
  TinfoDepSauCollectionItem = class;
  TInfoReembMedCollection = class;
  TInfoReembMedCollectionItem = class;
  TdetReembTitCollection = class;
  TdetReembTitCollectionItem = class;
  TinfoReembDepCollection = class;
  TinfoReembDepCollectionItem = class;
  TdetReembDepCollection = class;
  TdetReembDepCollectionItem = class;

  TS5002 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtirrfBenef: TEvtirrfBenef;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento write FTipoEvento;
    property EvtirrfBenef: TEvtirrfBenef read FEvtirrfBenef write FEvtirrfBenef;
  end;

  TInfoDep = class(TObject)
  private
    FvrDedDep: Double;
  public
    property vrDedDep: Double read FvrDedDep;
  end;

  TInfoIrrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoIrrfCollectionItem);
  public
    function Add: TInfoIrrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoIrrfCollectionItem;
    property Items[Index: Integer]: TInfoIrrfCollectionItem read GetItem write SetItem;
  end;

  TInfoIrrfCollectionItem = class(TObject)
  private
    FCodCateg: integer;
    FindResBr: String;
    FbasesIrrf: TbasesIrrfCollection;
    Firrf: TirrfCollection;
    FidePgtoExt: TidePgtoExt;

    procedure SetbasesIrrf(const Value: TbasesIrrfCollection);
    procedure Setirrf(const Value: TirrfCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property CodCateg: integer read FCodCateg;
    property indResBr: String read FindResBr;
    property basesIrrf: TbasesIrrfCollection read FbasesIrrf write SetbasesIrrf;
    property irrf: TirrfCollection read Firrf write Setirrf;
    property idePgtoExt: TidePgtoExt read FidePgtoExt;
  end;

  TinfoIRCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoIRCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoIRCollectionItem);
  public
    function Add: TinfoIRCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoIRCollectionItem;
    property Items[Index: Integer]: TinfoIRCollectionItem read GetItem write SetItem;
  end;

  TinfoIRCollectionItem = class(TObject)
  private
    FtpInfoIR: Integer;
    Fvalor: Double;
    FinfoProcJudRub: TinfoProcJudRubCollection;

    function getInfoProcJudRub(): TinfoProcJudRubCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoProcJudRubInst(): Boolean;

    property tpInfoIR: Integer read FtpInfoIR;
    property valor: Double read Fvalor;
    property infoProcJudRub: TinfoProcJudRubCollection read getInfoProcJudRub write FinfoProcJudRub;
  end;

  TdmDevCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdmDevCollectionItem;
    procedure SetItem(Index: Integer; Value: TdmDevCollectionItem);
  public
    function Add: TdmDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdmDevCollectionItem;
    property Items[Index: Integer]: TdmDevCollectionItem read GetItem write SetItem;
  end;

  TdmDevCollectionItem = class(TObject)
  private
    FperRef    : String;
    FideDmDev  : String;
    FtpPgto    : Integer;
    FdtPgto    : TDateTime;
    FcodCateg  : Integer;
    FinfoIR    : TinfoIRCollection;
    FtotApurMen: TtotApurMenCollection;
    FtotApurDia: TtotApurDiaCollection;
    FinfoRRA   : TInfoRRA;
    FInfoPgtoExt : TInfoPgtoExt;

    function getInfoIR(): TInfoIRCollection;
    function getTotApurMen(): TTotApurMenCollection;
    function getTotApurDia(): TTotApurDiaCollection;
    function getInfoRRA(): TInfoRRA;
    function getInfoPgtoExt(): TInfoPgtoExt;
  public
    constructor Create;
    destructor Destroy; override;

    function infoIRInst(): Boolean;
    function totApurMenInst(): Boolean;
    function totApurDiaInst(): Boolean;
    function infoRRAInst(): Boolean;
    function infoPgtoExtInst(): Boolean;

    property perRef     : String read FperRef;
    property ideDmDev   : String read FideDmDev;
    property tpPgto     : Integer read FtpPgto;
    property dtPgto     : TDateTime read FdtPgto;
    property codCateg   : Integer read FcodCateg;
    property infoIR     : TInfoIRCollection read getInfoIR write FinfoIR;
    property totApurMen : TtotApurMenCollection read getTotApurMen write FtotApurMen;
    property totApurDia : TtotApurDiaCollection read getTotApurDia write FtotApurDia;
    property infoRRA    : TInfoRRA read getInfoRRA write FinfoRRA;
    property infoPgtoExt: TInfoPgtoExt read getInfoPgtoExt write FInfoPgtoExt;
  end;

  TideTrabalhador4 = class(TIdeTrabalhador3)
  private
    FcpfBenef: string;
    FdmDev: TdmDevCollection;
    FinfoIRComplem: TinfoIRComplem;

    function getDmDev: TDmDevCollection;
    function getInfoIRComplem: TInfoIRComplem;
  public
    constructor Create;
    destructor Destroy; override;

    function dmDevInst(): boolean;
    function infoIRComplemInst(): boolean;

    property cpfBenef: string read FcpfBenef;
    property dmDev: TdmDevCollection read getDmDev write FdmDev;
    property infoIRComplem: TinfoIRComplem read getInfoIRComplem write FinfoIRComplem;
  end;

  TbasesIrrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbasesIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesIrrfCollectionItem);
  public
    function Add: TbasesIrrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbasesIrrfCollectionItem;
    property Items[Index: Integer]: TbasesIrrfCollectionItem read GetItem write SetItem;
  end;

  TbasesIrrfCollectionItem = class(TObject)
  private
    Fvalor: Double;
    FtpValor: Integer;
  public
    property tpValor: Integer read FtpValor;
    property valor: Double read Fvalor;
  end;

  TirrfCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TirrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TirrfCollectionItem);
  public
    function Add: TirrfCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TirrfCollectionItem;
    property Items[Index: Integer]: TirrfCollectionItem read GetItem write SetItem;
  end;

  TirrfCollectionItem = class(TObject)
  private
    FtpCR: string;
    FvrIrrfDesc: Double;
  public
    property tpCR: string read FtpCR;
    property vrIrrfDesc: Double read FvrIrrfDesc;
  end;

  TidePgtoExt = class(TObject)
  private
    FidePais: TidePais;
    FendExt: TendExt;
  public
    constructor Create;
    destructor Destroy; override;

    property idePais: TidePais read FidePais write FidePais;
    property endExt: TendExt read FendExt write FendExt;
  end;

  TTotApurMenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTotApurMenCollectionItem;
    procedure SetItem(Index: Integer; Value: TTotApurMenCollectionItem);
  public
    function New: TTotApurMenCollectionItem;
    property Items[Index: Integer]: TTotApurMenCollectionItem read GetItem write SetItem;
  end;

  TTotApurMenCollectionItem = class
  private
   FCRMen: string;
   FVlrCRMen: Double;
   FVlrCRMenSusp: Double;
  public
   property CRMen: string read FCRMen;
   property vlrCRMen: Double read FVlrCRMen;
   property vlrCRMenSusp: Double read FVlrCRMenSusp;
  end;

  TTotApurDiaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTotApurDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TTotApurDiaCollectionItem);
  public
    function New: TTotApurDiaCollectionItem;
    property Items[Index: Integer]: TTotApurDiaCollectionItem read GetItem write SetItem;
  end;

  TTotApurDiaCollectionItem = class
  private
   FPerApurDia: Integer;
   FCRDia: string;
   FVlrCRDia: Double;
   FVlrCRDiaSusp: Double;
  public
   property perApurDia: Integer read FPerApurDia;
   property CRDia: string read FCRDia;
   property vlrCRDia: Double read FVlrCRDia;
   property vlrCRDiaSusp: Double read FVlrCRDiaSusp;
  end;

  TEvtIrrfBenef = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento      : TIdeEvento5;
    FIdeEmpregador  : TIdeEmpregador;
    FIdeTrabalhador : TIdeTrabalhador4;
    FInfoDep        : TInfoDep;
    FInfoIrrf       : TInfoIrrfCollection;
    FVersaoDF       : TVersaoeSocial;

    procedure SetInfoIrrf(const Value: TInfoIrrfCollection);
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento      : TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador  : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador : TIdeTrabalhador4 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoDep        : TInfoDep read FInfoDep write FInfoDep;
    property InfoIrrf       : TInfoIrrfCollection read FInfoIrrf write SetInfoIrrf;
    property Leitor         : TLeitor read FLeitor write FLeitor;
    property Id             : String read FId;
    property XML            : String read FXML;
    property VersaoDF       : TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TInfoPgtoExt = class(TObject)
  private
    FpaisResidExt: integer;
    FindNIF: tpIndNIF;
    FnifBenef: string;
    FfrmTribut: Integer;
    FEndExtV110: TEndExtV110;
  public
    constructor Create;
    destructor  Destroy; override;

    property paisResidExt: integer read FpaisResidExt;
    property indNIF: tpIndNIF read FindNIF;
    property nifBenef: string read FnifBenef;
    property frmTribut: Integer read FfrmTribut;
    property endExt: TEndExtV110 read FEndExtV110 write FEndExtV110;
  end;

  TInfoProcJudRubCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoProcJudRubCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoProcJudRubCollectionItem);
  public
    function New: TInfoProcJudRubCollectionItem;
    property Items[Index: Integer]: TInfoProcJudRubCollectionItem read GetItem write SetItem;
  end;

  TInfoProcJudRubCollectionItem = class(TObject)
  private
   FnrProc: string;
   FufVara: string;
   FcodMunic: integer;
   FidVara: integer;
  public
   property nrProc: string read FnrProc;
   property ufVara: string read FufVara;
   property codMunic: integer read FcodMunic;
   property idVara: integer read FidVara;
  end;

  TinfoIRComplem = class(TObject)
  private
    FdtLaudo: TDateTime;
    FideDep: TideDepCollection;
    FinfoIRCR: TinfoIRCRCollection;
    FplanSaude: TplanSaudeCollection;
    FinfoReembMed: TinfoReembMedCollection;

    function getIdeDep: TideDepCollection;
    function getInfoIRCR: TinfoIRCRCollection;
    function getPlanSaude: TplanSaudeCollection;
    function getInfoReembMed: TinfoReembMedCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function ideDepInst: boolean;
    function infoIRCRInst: boolean;
    function planSaudeInst: boolean;
    function infoReembMedInst: boolean;

    property dtLaudo: TDateTime read FdtLaudo;
    property ideDep: TideDepCollection read getIdeDep write FideDep;
    property infoIRCR: TinfoIRCRCollection read getInfoIRCR write FinfoIRCR;
    property planSaude: TplanSaudeCollection read getPlanSaude write FplanSaude;
    property infoReembMed: TinfoReembMedCollection read getInfoReembMed write FinfoReembMed;
  end;

  TideDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeDepCollectionItem;
    procedure SetItem(Index: Integer; const Value: TIdeDepCollectionItem);
  public
    function New: TIdeDepCollectionItem;
    property Items[Index: Integer]: TIdeDepCollectionItem read GetItem write SetItem; default;
  end;

  TideDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FdtNascto: TDateTime;
    Fnome: string;
    FdepIRRF: tpSimNaoFacultativo;
    FtpDep: tpTpDep;
    FdescrDep: string;
  public
    property cpfDep: string read FcpfDep;
    property dtNascto: TDateTime read FdtNascto;
    property nome: string read Fnome;
    property depIRRF: tpSimNaoFacultativo read FdepIRRF;
    property tpDep: tpTpDep read FtpDep;
    property descrDep: string read FdescrDep;
  end;

  TinfoIRCRCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoIRCRCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoIRCRCollectionItem);
  public
    function New: TinfoIRCRCollectionItem;
    property Items[Index: Integer]: TinfoIRCRCollectionItem read GetItem write SetItem; default;
  end;

  TinfoIRCRCollectionItem = class(TObject)
  private
    FtpCR: string;
    FdedDepen: TdedDepenCollection;
    FpenAlim: TpenAlimCollection;
    FprevidCompl: TprevidComplCollection;
    FinfoProcRet: TinfoProcRetCollection;

    function getDedDepen: TdedDepenCollection;
    function getPenAlim: TpenAlimCollection;
    function getPrevidCompl: TprevidComplCollection;
    function getInfoProcRet: TinfoProcRetCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dedDepenInst: boolean;
    function penAlimInst: boolean;
    function previdComplInst: boolean;
    function infoProcRetInst: boolean;

    property tpCR: string read FtpCR;
    property dedDepen: TdedDepenCollection read getDedDepen write FdedDepen;
    property penAlim: TpenAlimCollection read getPenAlim write FpenAlim;
    property previdCompl: TprevidComplCollection read getPrevidCompl write FprevidCompl;
    property infoProcRet: TinfoProcRetCollection read getInfoProcRet write FinfoProcRet;
  end;

  TdedDepenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdedDepenCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdedDepenCollectionItem);
  public
    function New: TdedDepenCollectionItem;
    property Items[Index: Integer]: TdedDepenCollectionItem read GetItem write SetItem; default;
  end;

  TdedDepenCollectionItem = class(TObject)
  private
    FtpRend: integer;
    FcpfDep: string;
    FvlrDedDep: double;
  public
    property tpRend: integer read FtpRend;
    property cpfDep: string read FcpfDep;
    property vlrDedDep: double read FvlrDedDep;
  end;

  TpenAlimCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TpenAlimCollectionItem;
    procedure SetItem(Index: Integer; const Value: TpenAlimCollectionItem);
  public
    function New: TpenAlimCollectionItem;
    property Items[Index: Integer]: TpenAlimCollectionItem read GetItem write SetItem; default;
  end;

  TpenAlimCollectionItem = class(TObject)
  private
    FtpRend: integer;
    FcpfDep: string;
    FvlrDedPenAlim: double;
  public
    property tpRend: integer read FtpRend;
    property cpfDep: string read FcpfDep;
    property vlrDedPenAlim: double read FvlrDedPenAlim;
  end;

  TprevidComplCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TprevidComplCollectionItem;
    procedure SetItem(Index: Integer; const Value: TprevidComplCollectionItem);
  public
    function New: TprevidComplCollectionItem;
    property Items[Index: Integer]: TprevidComplCollectionItem read GetItem write SetItem; default;
  end;

  TprevidComplCollectionItem = class(TObject)
  private
    FtpPrev: tpTpPrev;
    FcnpjEntidPC: string;
    FvlrDedPC: double;
    FvlrPatrocFunp: double;
  public
    property tpPrev: tpTpPrev read FtpPrev;
    property cnpjEntidPC: string read FcnpjEntidPC;
    property vlrDedPC: double read FvlrDedPC;
    property vlrPatrocFunp: double read FvlrPatrocFunp;
  end;

  TinfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoProcRetCollectionItem);
  public
    function New: TinfoProcRetCollectionItem;
    property Items[Index: Integer]: TinfoProcRetCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: tpTpProcRet;
    FnrProcRet: string;
    FcodSusp: integer;
    FinfoValores: TinfoValoresCollection;

    function getInfoValores: TinfoValoresCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoValoresInst: boolean;

    property tpProcRet: tpTpProcRet read FtpProcRet;
    property nrProcRet: string read FnrProcRet;
    property codSusp: integer read FcodSusp;
    property infoValores: TinfoValoresCollection read getInfoValores write FinfoValores;
  end;

  TinfoValoresCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoValoresCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoValoresCollectionItem);
  public
    function New: TinfoValoresCollectionItem;
    property Items[Index: Integer]: TinfoValoresCollectionItem read GetItem write SetItem; default;
  end;

  TinfoValoresCollectionItem = class(TObject)
  private
    FindApuracao: tpIndApuracao;
    FvlrNRetido: double;
    FvlrDepJud: double;
    FvlrCmpAnoCal: double;
    FvlrCmpAnoAnt: double;
    FvlrRendSusp: double;
    FdedSusp: TdedSuspCollection;

    function getDedSusp: TdedSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function dedSuspInst: boolean;

    property indApuracao: tpIndApuracao read FindApuracao;
    property vlrNRetido: double read FvlrNRetido;
    property vlrDepJud: double read FvlrDepJud;
    property vlrCmpAnoCal: double read FvlrCmpAnoCal;
    property vlrCmpAnoAnt: double read FvlrCmpAnoAnt;
    property vlrRendSusp: double read FvlrRendSusp;
    property dedSusp: TdedSuspCollection read getDedSusp write FdedSusp;
  end;

  TdedSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdedSuspCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdedSuspCollectionItem);
  public
    function New: TdedSuspCollectionItem;
    property Items[Index: Integer]: TdedSuspCollectionItem read GetItem write SetItem; default;
  end;

  TdedSuspCollectionItem = class(TObject)
  private
    FindTpDeducao: tpIndTpDeducao;
    FvlrDedSusp: double;
    FcnpjEntidPC: string;
    FvlrPatrocFunp: double;
    FbenefPen: TbenefPenCollection;

   function getBenefPen: TbenefPenCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function benefPenInst: boolean;

    property indTpDeducao: tpIndTpDeducao read FindTpDeducao;
    property vlrDedSusp: double read FvlrDedSusp;
    property cnpjEntidPC: string read FcnpjEntidPC;
    property vlrPatrocFunp: double read FvlrPatrocFunp;
    property benefPen: TbenefPenCollection read getBenefPen write Fbenefpen;
  end;

  TbenefPenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbenefPenCollectionItem;
    procedure SetItem(Index: Integer; const Value: TbenefPenCollectionItem);
  public
    function New: TbenefPenCollectionItem;
    property Items[Index: Integer]: TbenefPenCollectionItem read GetItem write SetItem; default;
  end;

  TbenefPenCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrDepenSusp: double;
  public
    property cpfDep: string read FcpfDep;
    property vlrDepenSusp: double read FvlrDepenSusp;
  end;

  TplanSaudeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TplanSaudeCollectionItem;
    procedure SetItem(Index: Integer; const Value: TplanSaudeCollectionItem);
  public
    function New: TplanSaudeCollectionItem;
    property Items[Index: Integer]: TplanSaudeCollectionItem read GetItem write SetItem; default;
  end;

  TplanSaudeCollectionItem = class(TObject)
  private
    FcnpjOper: string;
    FregANS: string;
    FvlrSaudeTit: double;
    FinfoDepSau: TinfoDepSauCollection;

    function getInfoDepSau: TinfoDepSauCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoDepSauInst: boolean;

    property cnpjOper: string read FcnpjOper;
    property regANS: string read FregANS;
    property vlrSaudeTit: double read FvlrSaudeTit;
    property infoDepSau: TinfoDepSauCollection read getInfoDepSau write FinfoDepSau;
  end;

  TinfoDepSauCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoDepSauCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoDepSauCollectionItem);
  public
    function New: TinfoDepSauCollectionItem;
    property Items[Index: Integer]: TinfoDepSauCollectionItem read GetItem write SetItem; default;
  end;

  TinfoDepSauCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrSaudeDep: double;
  public
    property cpfDep: string read FcpfDep;
    property vlrSaudeDep: double read FvlrSaudeDep;
  end;

  TInfoReembMedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoReembMedCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoReembMedCollectionItem);
  public
    function New: TinfoReembMedCollectionItem;
    property Items[Index: Integer]: TinfoReembMedCollectionItem read GetItem write SetItem; default;
  end;

  TInfoReembMedCollectionItem = class(TObject)
  private
    FindOrgReemb: string;
    FcnpjOper: string;
    FregANS: string;
    FdetReembTit: TdetReembTitCollection;
    FinfoReembDep: TinfoReembDepCollection;

    function getDetReembTit: TdetReembTitCollection;
    function getInfoReembDep: TinfoReembDepCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function detReembTitInst: boolean;
    function infoReembDepInst: boolean;

    property indOrgReemb: string read FindOrgReemb;
    property cnpjOper: string read FcnpjOper;
    property regANS: string read FregANS;
    property detReembTit: TdetReembTitCollection read getDetReembTit write FdetReembTit;
    property infoReembDep: TinfoReembDepCollection read getInfoReembDep write FinfoReembDep;
  end;

  TdetReembTitCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetReembTitCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdetReembTitCollectionItem);
  public
    function New: TdetReembTitCollectionItem;
    property Items[Index: Integer]: TdetReembTitCollectionItem read GetItem write SetItem; default;
  end;

  TdetReembTitCollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FvlrReemb: double;
    FvlrReembAnt: double;
  public
    property tpInsc: tpTpInsc read FtpInsc;
    property nrInsc: string read FnrInsc;
    property vlrReemb: double read FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt;
  end;

  TinfoReembDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoReembDepCollectionItem;
    procedure SetItem(Index: Integer; const Value: TinfoReembDepCollectionItem);
  public
    function New: TinfoReembDepCollectionItem;
    property Items[Index: Integer]: TinfoReembDepCollectionItem read GetItem write SetItem; default;
  end;

  TinfoReembDepCollectionItem = class(TObject)
  private
    FcpfBenef: string;
    FdetReembDep: TdetReembDepCollection;

    function getDepReembDep: TdetReembDepCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function detReembDepInst: boolean;

    property cpfBenef: string read FcpfBenef;
    property detReembDep: TdetReembDepCollection read getDepReembDep write FdetReembDep;
  end;

  TdetReembDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetReembDepCollectionItem;
    procedure SetItem(Index: Integer; const Value: TdetReembDepCollectionItem);
  public
    function New: TdetReembDepCollectionItem;
    property Items[Index: Integer]: TdetReembDepCollectionItem read GetItem write SetItem; default;
  end;

  TdetReembDepCollectionItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FvlrReemb: double;
    FvlrReembAnt: double;
  public
    property tpInsc: tpTpInsc read FtpInsc;
    property nrInsc: string read FnrInsc;
    property vlrReemb: double read FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base;

{ TS5002 }

constructor TS5002.Create;
begin
  inherited Create;
  FTipoEvento   := teS5002;
  FEvtIrrfBenef := TEvtIrrfBenef.Create;
end;

destructor TS5002.Destroy;
begin
  FEvtIrrfBenef.Free;

  inherited;
end;

function TS5002.GetEvento : TObject;
begin
  Result := self;
end;

function TS5002.GetXml : string;
begin
  Result := FEvtIrrfBenef.XML;
end;

procedure TS5002.SetXml(const Value: string);
begin
  if Value = FEvtIrrfBenef.XML then Exit;

  FEvtIrrfBenef.FXML := Value;
  FEvtIrrfBenef.Leitor.Arquivo := Value;
  FEvtIrrfBenef.LerXML;

end;

function TS5002.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TInfoIrrfCollection }

function TInfoIrrfCollection.Add: TInfoIrrfCollectionItem;
begin
  Result := Self.New;
end;

function TInfoIrrfCollection.GetItem(
  Index: Integer): TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem(inherited Items[Index]);
end;

procedure TInfoIrrfCollection.SetItem(Index: Integer;
  Value: TInfoIrrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoIrrfCollection.New: TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoIrrfCollectionItem }

constructor TInfoIrrfCollectionItem.Create;
begin
  inherited Create;
  FbasesIrrf  := TbasesIrrfCollection.Create;
  Firrf       := TirrfCollection.Create;
  FidePgtoExt := TidePgtoExt.Create;
end;

destructor TInfoIrrfCollectionItem.Destroy;
begin
  FbasesIrrf.Free;
  Firrf.Free;
  FidePgtoExt.Free;

  inherited;
end;

procedure TInfoIrrfCollectionItem.SetbasesIrrf(const Value: TbasesIrrfCollection);
begin
  FbasesIrrf := Value;
end;

procedure TInfoIrrfCollectionItem.Setirrf(const Value: TirrfCollection);
begin
  Firrf := Value;
end;

{ TbaseIrrfCollection }

function TbasesIrrfCollection.Add: TbasesIrrfCollectionItem;
begin
  Result := Self.New;
end;

function TbasesIrrfCollection.GetItem(
  Index: Integer): TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem(inherited Items[Index]);
end;

procedure TbasesIrrfCollection.SetItem(Index: Integer;
  Value: TbasesIrrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDmDevCollection }

function TDmDevCollection.Add: TDmDevCollectionItem;
begin
  Result := Self.New;
end;

function TDmDevCollection.GetItem(Index: Integer): TDmDevCollectionItem;
begin
  Result := TDmDevCollectionItem(inherited Items[Index]);
end;

procedure TDmDevCollection.SetItem(Index: Integer;
  Value: TDmDevCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDmDevCollection.New: TDmDevCollectionItem;
begin
  Result := TDmDevCollectionItem.Create;
  Self.Add(Result);
end;

{ TDmDevColletionItem }

constructor TDmDevCollectionItem.Create;
begin
  inherited;

  FinfoIR      := nil;
  FtotApurMen  := nil;
  FtotApurDia  := nil;
  FinfoRRA     := nil;
  FinfoPgtoExt := nil;
end;

destructor TDmDevCollectionItem.Destroy;
begin
  if infoIRInst() then
    FreeAndNil(FinfoIR);
  if totApurMenInst() then
    FreeAndNil(FtotApurMen);
  if totApurDiaInst() then
    FreeAndNil(FtotApurDia);
  if infoRRAInst() then
    FreeAndNil(FinfoRRA);
  if infoPgtoExtInst() then
    FreeAndNil(FinfoPgtoExt);
  inherited;
end;

function TDmDevCollectionItem.infoIRInst(): Boolean;
begin
  Result := Assigned(FinfoIR);
end;

function TDmDevCollectionItem.getInfoIR: TInfoIRCollection;
begin
  if not(Assigned(FinfoIR)) then
    FinfoIR := TinfoIRCollection.Create;
  Result := FinfoIR;
end;

function TDmDevCollectionItem.totApurMenInst(): Boolean;
begin
  Result := Assigned(FtotApurMen);
end;

function TDmDevCollectionItem.getTotApurMen: TtotApurMenCollection;
begin
  if not(Assigned(FtotApurMen)) then
    FtotApurMen := TtotApurMenCollection.Create;
  Result := FtotApurMen;
end;

function TDmDevCollectionItem.totApurDiaInst(): Boolean;
begin
  Result := Assigned(FtotApurDia);
end;

function TDmDevCollectionItem.getTotApurDia: TtotApurDiaCollection;
begin
  if not(Assigned(FtotApurDia)) then
    FtotApurDia := TtotApurDiaCollection.Create;
  Result := FtotApurDia;
end;

function TDmDevCollectionItem.getInfoRRA: TInfoRRA;
begin
  if not(Assigned(FinfoRRA)) then
    FinfoRRA := TinfoRRA.Create;
  Result := FinfoRRA;
end;

function TDmDevCollectionItem.infoRRAInst(): Boolean;
begin
  Result := Assigned(FinfoRRA);
end;

function TDmDevCollectionItem.getInfoPgtoExt: TInfoPgtoExt;
begin
  if not (Assigned(FInfoPgtoExt)) then
    FInfoPgtoExt := TInfoPgtoExt.Create;
  Result := FInfoPgtoExt;
end;

function TDmDevCollectionItem.infoPgtoExtInst: Boolean;
begin
  Result := Assigned(FInfoPgtoExt);
end;

{ TInfoPgtoExt }

constructor TInfoPgtoExt.Create;
begin
  inherited Create;

  FEndExtV110 := TEndExtV110.Create;
end;

destructor TInfoPgtoExt.Destroy;
begin
  FreeAndNil(FEndExtV110);

  inherited;
end;

{ TinfoIRCollection }

function TinfoIRCollection.Add: TinfoIRCollectionItem;
begin
  Result := Self.New;
end;

function TinfoIRCollection.GetItem(Index: Integer): TinfoIRCollectionItem;
begin
  Result := TinfoIRCollectionItem(inherited Items[Index]);
end;

procedure TinfoIRCollection.SetItem(Index: Integer;
  Value: TinfoIRCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoIRCollection.New: TinfoIRCollectionItem;
begin
  Result := TinfoIRCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoIRCollectionItem }

constructor TinfoIRCollectionItem.Create;
begin
  inherited Create;

  FinfoProcJudRub := nil;
end;

destructor TinfoIRCollectionItem.Destroy;
begin
  if infoProcJudRubInst() then
    FreeAndNil(FinfoProcJudRub);

  inherited;
end;

function TinfoIRCollectionItem.getInfoProcJudRub: TinfoProcJudRubCollection;
begin
  if not (Assigned(FinfoProcJudRub)) then
    FinfoProcJudRub := TinfoProcJudRubCollection.Create;
  Result := FinfoProcJudRub;
end;

function TinfoIRCollectionItem.infoProcJudRubInst: Boolean;
begin
  Result := Assigned(FinfoProcJudRub);
end;

{ TinfoProcJudRubCollection }

function TinfoProcJudRubCollection.GetItem(Index: Integer): TinfoProcJudRubCollectionItem;
begin
  Result := TinfoProcJudRubCollectionItem(inherited Items[Index]);
end;

procedure TinfoProcJudRubCollection.SetItem(Index: Integer;
  Value: TinfoProcJudRubCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoProcJudRubCollection.New: TinfoProcJudRubCollectionItem;
begin
  Result := TinfoProcJudRubCollectionItem.Create;
  Self.Add(Result);
end;

{ TirrfCollection }

function TirrfCollection.Add: TirrfCollectionItem;
begin
  Result := Self.New;
end;

function TirrfCollection.GetItem(Index: Integer): TirrfCollectionItem;
begin
  Result := TirrfCollectionItem(inherited Items[Index]);
end;

procedure TirrfCollection.SetItem(Index: Integer;
  Value: TirrfCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TirrfCollection.New: TirrfCollectionItem;
begin
  Result := TirrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TidePgtoExt }

constructor TidePgtoExt.Create;
begin
  inherited Create;
  FidePais := TidePais.Create;
  FendExt  := TendExt.Create;
end;

destructor TidePgtoExt.Destroy;
begin
  FidePais.Free;
  FendExt.Free;

  inherited;
end;

function TbasesIrrfCollection.New: TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem.Create;
  Self.Add(Result);
end;

{ TTotApurMenCollection }

function TTotApurMenCollection.GetItem(
  Index: Integer): TTotApurMenCollectionItem;
begin
  Result := TTotApurMenCollectionItem(inherited Items[Index]);
end;

procedure TTotApurMenCollection.SetItem(Index: Integer;
  Value: TTotApurMenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TTotApurMenCollection.New: TTotApurMenCollectionItem;
begin
  Result := TTotApurMenCollectionItem.Create;
  Self.Add(Result);
end;

{ TTotApurDiaCollection }

function TTotApurDiaCollection.GetItem(
  Index: Integer): TTotApurDiaCollectionItem;
begin
  Result := TTotApurDiaCollectionItem(inherited Items[Index]);
end;

procedure TTotApurDiaCollection.SetItem(Index: Integer;
  Value: TTotApurDiaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TTotApurDiaCollection.New: TTotApurDiaCollectionItem;
begin
  Result := TTotApurDiaCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoIRComplem }

constructor TinfoIRComplem.Create;
begin
  inherited Create;

  FideDep := nil;
  FinfoIRCR := nil;
  FplanSaude := nil;
  FinfoReembMed := nil;
end;

destructor TinfoIRComplem.Destroy;
begin
  if ideDepInst() then
    FreeAndNil(FideDep);

  if infoIRCRInst() then
    FreeAndNil(FinfoIRCR);

  if planSaudeInst() then
    FreeAndNil(FplanSaude);

  if infoReembMedInst() then
    FreeAndNil(FinfoReembMed);

  inherited;
end;

function TinfoIRComplem.getIdeDep: TideDepCollection;
begin
  if not Assigned(FideDep) then
    FideDep := TideDepCollection.Create;
  Result := FideDep;
end;

function TinfoIRComplem.getInfoIRCR: TinfoIRCRCollection;
begin
  if not Assigned(FinfoIRCR) then
    FinfoIRCR := TinfoIRCRCollection.Create;
  Result := FinfoIRCR;
end;

function TinfoIRComplem.getPlanSaude: TplanSaudeCollection;
begin
  if not Assigned(FPlanSaude) then
    FPlanSaude := TPlanSaudeCollection.Create;
  Result := FPlanSaude;
end;

function TinfoIRComplem.getInfoReembMed: TinfoReembMedCollection;
begin
  if not Assigned(FinfoReembMed) then
    FinfoReembMed := TinfoReembMedCollection.Create;
  Result := FinfoReembMed;
end;

function TinfoIRComplem.ideDepInst: boolean;
begin
  Result := Assigned(FideDep);
end;

function TinfoIRComplem.infoIRCRInst: boolean;
begin
  Result := Assigned(FinfoIRCR);
end;

function TinfoIRComplem.planSaudeInst: boolean;
begin
  Result := Assigned(FplanSaude);
end;

function TinfoIRComplem.infoReembMedInst: boolean;
begin
  Result := Assigned(FinfoReembMed);
end;

{ TideDepCollection }

function TideDepCollection.GetItem(Index: Integer): TideDepCollectionItem;
begin
  Result := TideDepCollectionItem(inherited Items[Index]);
end;

procedure TideDepCollection.SetItem(Index: Integer; const Value: TideDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TideDepCollection.New: TideDepCollectionItem;
begin
  Result := TideDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoIRCRCollection }

function TinfoIRCRCollection.GetItem(Index: Integer): TinfoIRCRCollectionItem;
begin
  Result := TinfoIRCRCollectionItem(inherited Items[Index]);
end;

procedure TinfoIRCRCollection.SetItem(Index: Integer; const Value: TinfoIRCRCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoIRCRCollection.New: TinfoIRCRCollectionItem;
begin
  Result := TinfoIRCRCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoIRCRCollectionItem }

constructor TinfoIRCRCollectionItem.Create;
begin
  inherited Create;

  FdedDepen := nil;
  FpenAlim := nil;
  FprevidCompl := nil;
  FinfoProcRet := nil;
end;

destructor TinfoIRCRCollectionItem.Destroy;
begin
  if dedDepenInst() then
    FreeAndNil(FdedDepen);

  if penAlimInst() then
    FreeAndNil(FpenAlim);

  if previdComplInst() then
    FreeAndNil(FprevidCompl);

  if infoProcRetInst() then
    FreeAndNil(FinfoProcRet);

  inherited;
end;

function TinfoIRCRCollectionItem.getDedDepen: TdedDepenCollection;
begin
  if not Assigned(FdedDepen) then
    FdedDepen := TdedDepenCollection.Create;
  Result := FdedDepen;
end;

function TinfoIRCRCollectionItem.getPenAlim: TpenAlimCollection;
begin
  if not Assigned(FpenAlim) then
    FpenAlim := TpenAlimCollection.Create;
  Result := FpenAlim;
end;

function TinfoIRCRCollectionItem.getPrevidCompl: TprevidComplCollection;
begin
  if not Assigned(FprevidCompl) then
    FprevidCompl := TprevidComplCollection.Create;
  Result := FprevidCompl;
end;

function TinfoIRCRCollectionItem.getInfoProcRet: TinfoProcRetCollection;
begin
  if not Assigned(FinfoProcRet) then
    FinfoProcRet := TinfoProcRetCollection.Create;
  Result := FinfoProcRet;
end;

function TinfoIRCRCollectionItem.dedDepenInst: boolean;
begin
  Result := Assigned(FdedDepen);
end;

function TinfoIRCRCollectionItem.penAlimInst: boolean;
begin
  Result := Assigned(FpenAlim);
end;

function TinfoIRCRCollectionItem.previdComplInst: boolean;
begin
  Result := Assigned(FprevidCompl);
end;

function TinfoIRCRCollectionItem.infoProcRetInst: boolean;
begin
  Result := Assigned(FinfoProcRet);
end;

{ TdedDepenCollection }

function TdedDepenCollection.GetItem(Index: Integer): TdedDepenCollectionItem;
begin
  Result := TdedDepenCollectionItem(inherited Items[Index]);
end;

procedure TdedDepenCollection.SetItem(Index: Integer; const Value: TdedDepenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdedDepenCollection.New: TdedDepenCollectionItem;
begin
  Result := TdedDepenCollectionItem.Create;
  Self.Add(Result);
end;

{ TpenAlimCollection }

function TpenAlimCollection.GetItem(Index: Integer): TpenAlimCollectionItem;
begin
  Result := TpenAlimCollectionItem(inherited Items[Index]);
end;

procedure TpenAlimCollection.SetItem(Index: Integer; const Value: TpenAlimCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TpenAlimCollection.New: TpenAlimCollectionItem;
begin
  Result := TpenAlimCollectionItem.Create;
  Self.Add(Result);
end;

{ TprevidComplCollection }

function TprevidComplCollection.GetItem(Index: Integer): TprevidComplCollectionItem;
begin
  Result := TprevidComplCollectionItem(inherited Items[Index]);
end;

procedure TprevidComplCollection.SetItem(Index: Integer; const Value: TprevidComplCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TprevidComplCollection.New: TprevidComplCollectionItem;
begin
  Result := TprevidComplCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoProcRetCollection }

function TinfoProcRetCollection.GetItem(Index: Integer): TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem(inherited Items[Index]);
end;

procedure TinfoProcRetCollection.SetItem(Index: Integer; const Value: TinfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoProcRetCollection.New: TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoProcRetCollectionItem }

constructor TinfoProcRetCollectionItem.Create;
begin
  inherited Create;

  FinfoValores := nil;
end;

destructor TinfoProcRetCollectionItem.Destroy;
begin
  if infoValoresInst() then
    FreeAndNil(FinfoValores);

  inherited;
end;

function TinfoProcRetCollectionItem.getInfoValores: TinfoValoresCollection;
begin
  if not Assigned(FinfoValores) then
    FinfoValores := TinfoValoresCollection.Create;
  Result := FinfoValores;
end;

function TinfoProcRetCollectionItem.infoValoresInst: boolean;
begin
  Result := Assigned(FinfoValores);
end;

{ TinfoValoresCollection }

function TinfoValoresCollection.GetItem(Index: Integer): TinfoValoresCollectionItem;
begin
  Result := TinfoValoresCollectionItem(inherited Items[Index]);
end;

procedure TinfoValoresCollection.SetItem(Index: Integer; const Value: TinfoValoresCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoValoresCollection.New: TinfoValoresCollectionItem;
begin
  Result := TinfoValoresCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoValoresCollectionItem }

constructor TinfoValoresCollectionItem.Create;
begin
  inherited Create;

  FdedSusp := nil;
end;

destructor TinfoValoresCollectionItem.Destroy;
begin
  if dedSuspInst() then
    FreeAndNil(FdedSusp);

  inherited;
end;

function TinfoValoresCollectionItem.getDedSusp: TdedSuspCollection;
begin
  if not Assigned(FdedSusp) then
    FdedSusp := TdedSuspCollection.Create;
  Result := FdedSusp;
end;

function TinfoValoresCollectionItem.dedSuspInst: boolean;
begin
  Result := Assigned(FdedSusp);
end;

{ TdedSuspCollection }

function TdedSuspCollection.GetItem(Index: Integer): TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem(inherited Items[Index]);
end;

procedure TdedSuspCollection.SetItem(Index: Integer; const Value: TdedSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdedSuspCollection.New: TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem.Create;
  Self.Add(Result);
end;

{ TdedSuspCollectionItem }

constructor TdedSuspCollectionItem.Create;
begin
  inherited Create;

  FbenefPen := nil;
end;

destructor TdedSuspCollectionItem.Destroy;
begin
  if benefPenInst() then
    FreeAndNil(FbenefPen);

  inherited;
end;

function TdedSuspCollectionItem.getBenefPen: TbenefPenCollection;
begin
  if not Assigned(FbenefPen) then
    FbenefPen := TbenefPenCollection.Create;
  Result := FbenefPen;
end;

function TdedSuspCollectionItem.benefPenInst: boolean;
begin
  Result := Assigned(FbenefPen);
end;

{ TbenefPenCollection }

function TbenefPenCollection.GetItem(Index: Integer): TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem(inherited Items[Index]);
end;

procedure TbenefPenCollection.SetItem(Index: Integer; const Value: TbenefPenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TbenefPenCollection.New: TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem.Create;
  Self.Add(Result);
end;

{ TplanSaudeCollection }

function TplanSaudeCollection.GetItem(Index: Integer): TplanSaudeCollectionItem;
begin
  Result := TplanSaudeCollectionItem(inherited Items[Index]);
end;

procedure TplanSaudeCollection.SetItem(Index: Integer; const Value: TplanSaudeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TplanSaudeCollection.New: TplanSaudeCollectionItem;
begin
  Result := TplanSaudeCollectionItem.Create;
  Self.Add(Result);
end;

{ TplanSaudeCollectionItem }

constructor TplanSaudeCollectionItem.Create;
begin
  inherited Create;

  FinfoDepSau := nil;
end;

destructor TplanSaudeCollectionItem.Destroy;
begin
  if infoDepSauInst() then
    FreeAndNil(FinfoDepSau);

  inherited;
end;

function TplanSaudeCollectionItem.getInfoDepSau: TinfoDepSauCollection;
begin
  if not Assigned(FinfoDepSau) then
    FinfoDepSau := TinfoDepSauCollection.Create;
  Result := FinfoDepSau;
end;

function TplanSaudeCollectionItem.infoDepSauInst: boolean;
begin
  Result := Assigned(FinfoDepSau);
end;

{ TinfoDepSauCollection }

function TinfoDepSauCollection.GetItem(Index: Integer): TinfoDepSauCollectionItem;
begin
  Result := TinfoDepSauCollectionItem(inherited Items[Index]);
end;

procedure TinfoDepSauCollection.SetItem(Index: Integer; const Value: TinfoDepSauCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoDepSauCollection.New: TinfoDepSauCollectionItem;
begin
  Result := TinfoDepSauCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoReembMedCollection }

function TinfoReembMedCollection.GetItem(Index: Integer): TinfoReembMedCollectionItem;
begin
  Result := TinfoReembMedCollectionItem(inherited Items[Index]);
end;

procedure TinfoReembMedCollection.SetItem(Index: Integer; const Value: TinfoReembMedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoReembMedCollection.New: TinfoReembMedCollectionItem;
begin
  Result := TinfoReembMedCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoReembMedCollectionItem }

constructor TinfoReembMedCollectionItem.Create;
begin
  inherited Create;

  FdetReembTit := nil;
  FinfoReembDep := nil;
end;

destructor TinfoReembMedCollectionItem.Destroy;
begin
  if detReembTitInst() then
    FreeAndNil(FdetReembTit);

  if infoReembDepInst() then
    FreeAndNil(FinfoReembDep);

  inherited;
end;

function TinfoReembMedCollectionItem.getDetReembTit: TdetReembTitCollection;
begin
  if not Assigned(FdetReembTit) then
    FdetReembTit := TdetReembTitCollection.Create;
  Result := FdetReembTit;
end;

function TinfoReembMedCollectionItem.detReembTitInst: boolean;
begin
  Result := Assigned(FdetReembTit);
end;

function TinfoReembMedCollectionItem.getInfoReembDep: TinfoReembDepCollection;
begin
  if not Assigned(FinfoReembDep) then
    FinfoReembDep := TinfoReembDepCollection.Create;
  Result := FinfoReembDep;
end;

function TinfoReembMedCollectionItem.infoReembDepInst: boolean;
begin
  Result := Assigned(FinfoReembDep);
end;

{ TdetReembTitCollection }

function TdetReembTitCollection.GetItem(Index: Integer): TdetReembTitCollectionItem;
begin
  Result := TdetReembTitCollectionItem(inherited Items[Index]);
end;

procedure TdetReembTitCollection.SetItem(Index: Integer; const Value: TdetReembTitCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdetReembTitCollection.New: TdetReembTitCollectionItem;
begin
  Result := TdetReembTitCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoReembDepCollection }

function TinfoReembDepCollection.GetItem(Index: Integer): TinfoReembDepCollectionItem;
begin
  Result := TinfoReembDepCollectionItem(inherited Items[Index]);
end;

procedure TinfoReembDepCollection.SetItem(Index: Integer; const Value: TinfoReembDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoReembDepCollection.New: TinfoReembDepCollectionItem;
begin
  Result := TinfoReembDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfoReembDepCollectionItem }

constructor TinfoReembDepCollectionItem.Create;
begin
  inherited Create;

  FdetReembDep := nil;
end;

destructor TinfoReembDepCollectionItem.Destroy;
begin
  if detReembDepInst() then
    FreeAndNil(FdetReembDep);

  inherited;
end;

function TinfoReembDepCollectionItem.getDepReembDep: TdetReembDepCollection;
begin
  if not Assigned(FdetReembDep) then
    FdetReembDep := TdetReembDepCollection.Create;
  Result := FdetReembDep;
end;

function TinfoReembDepCollectionItem.detReembDepInst: boolean;
begin
  Result := Assigned(FdetReembDep);
end;

{ TdetReembDepCollection }

function TdetReembDepCollection.GetItem(Index: Integer): TdetReembDepCollectionItem;
begin
  Result := TdetReembDepCollectionItem(inherited Items[Index]);
end;

procedure TdetReembDepCollection.SetItem(Index: Integer; const Value: TdetReembDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdetReembDepCollection.New: TdetReembDepCollectionItem;
begin
  Result := TdetReembDepCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeTrabalhador4 }

constructor TIdeTrabalhador4.Create;
begin
 inherited Create;

 FdmDev := nil;
 FinfoIRComplem := nil;
end;

destructor TIdeTrabalhador4.Destroy;
begin
 if dmDevInst() then
   FreeAndNil(FdmDev);
 if infoIRComplemInst() then
   FreeAndNil(FinfoIRComplem);
 inherited;
end;

function TIdeTrabalhador4.getDmDev: TDmDevCollection;
begin
  if not(Assigned(FDmDev)) then
    FDmDev := TDmDevCollection.Create;
  Result := FDmDev;
end;

function TIdeTrabalhador4.dmDevInst(): boolean;
begin
  Result := Assigned(FDmDev);
end;

function TIdeTrabalhador4.getInfoIRComplem: TinfoIRComplem;
begin
  if not(Assigned(FinfoIRComplem)) then
    FinfoIRComplem := TinfoIRComplem.Create;
  Result := FinfoIRComplem;
end;

function TIdeTrabalhador4.infoIRComplemInst: boolean;
begin
  Result := Assigned(FinfoIRComplem);
end;

{ TEvtIrrfBenef }

constructor TEvtIrrfBenef.Create;
begin
  inherited Create;
  FLeitor          := TLeitor.Create;
  FIdeEvento       := TIdeEvento5.Create;
  FIdeEmpregador   := TIdeEmpregador.Create;
  FIdeTrabalhador  := TIdeTrabalhador4.Create;
  FInfoDep         := TInfoDep.Create;
  FInfoIrrf        := TInfoIrrfCollection.Create;
end;

destructor TEvtIrrfBenef.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FInfoDep.Free;
  FInfoIrrf.Free;

  inherited;
end;

procedure TEvtIrrfBenef.SetInfoIrrf(const Value: TInfoIrrfCollection);
begin
  FInfoIrrf := Value;
end;

function TEvtIrrfBenef.LerXML: boolean;
var
  ok: Boolean;
  i, j, k, l, m, n: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtIrrfBenef/', FXML)+18, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtIrrfBenef') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(1, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');

        if VersaoDF <= ve02_05_00 then
          IdeEvento.IndApuracao  := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));

        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');
      end; { ideEvento }

      if leitor.rExtrai(1, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end; { ideEmpregador }

      if leitor.rExtrai(1, 'ideTrabalhador') <> '' then
      begin
        if VersaoDF <= ve02_05_00 then
          IdeTrabalhador.cpfTrab := leitor.rCampo(tcStr, 'cpfTrab')
        else
          IdeTrabalhador.FcpfBenef := leitor.rCampo(tcStr, 'cpfBenef');
      end; { ideTrabalhador }

      if VersaoDF <= ve02_05_00 then
      begin
        if leitor.rExtrai(2, 'infoDep') <> '' then
          infoDep.FvrDedDep := leitor.rCampo(tcDe2, 'vrDedDep');

        i := 0;
        while Leitor.rExtrai(2, 'infoIrrf', '', i + 1) <> '' do
        begin
          with InfoIrrf do
          begin
            New;
            Items[i].FCodCateg := leitor.rCampo(tcInt, 'codCateg');
            Items[i].FindResBr := leitor.rCampo(tcStr, 'indResBr');
          end;

          j := 0;
          while Leitor.rExtrai(3, 'basesIrrf', '', j + 1) <> '' do
          begin
            with InfoIrrf.Items[i].basesIrrf do
            begin
              New;
              Items[j].FtpValor := leitor.rCampo(tcInt, 'tpValor');
              Items[j].Fvalor   := leitor.rCampo(tcDe2, 'valor');
            end;

            inc(j);
          end; { basesIrrf }

          j := 0;
          while Leitor.rExtrai(3, 'irrf', '', j + 1) <> '' do
          begin
            with InfoIrrf.Items[i].irrf do
            begin
              New;
              Items[j].FtpCR       := leitor.rCampo(tcStr, 'tpCR');
              Items[j].FvrIrrfDesc := leitor.rCampo(tcDe2, 'vrIrrfDesc');
            end;

            inc(j);
          end; { irrf }

          if leitor.rExtrai(3, 'idePgtoExt') <> '' then
          begin
            if leitor.rExtrai(4, 'idePais') <> '' then
            begin
              with InfoIrrf.Items[i].idePgtoExt.idePais do
              begin
                codPais  := leitor.rCampo(tcStr, 'codPais');
                indNIF   := eSStrToIndNIF(ok, leitor.rCampo(tcStr, 'indNIF'));
                nifBenef := leitor.rCampo(tcStr, 'nifBenef');
              end;
            end; { idePais }

            if leitor.rExtrai(4, 'endExt') <> '' then
            begin
              with InfoIrrf.Items[i].idePgtoExt.endExt do
              begin
                dscLograd := leitor.rCampo(tcStr, 'dscLograd');
                nrLograd  := leitor.rCampo(tcStr, 'nrLograd');
                complem   := leitor.rCampo(tcStr, 'complem');
                bairro    := leitor.rCampo(tcStr, 'bairro');
                nmCid     := leitor.rCampo(tcStr, 'nmCid');
                codPostal := leitor.rCampo(tcStr, 'codPostal');
              end;
            end; { endExt }
          end; { idePgtoExt }

          inc(i);
        end;
      end  { infoIrrf }
      else
      begin
        i := 0;
        while Leitor.rExtrai(2, 'dmDev', '', i + 1) <> '' do
        begin
          with IdeTrabalhador.DmDev do
          begin
            New;
            Items[i].FperRef   := leitor.rCampo(tcStr, 'perRef');
            Items[i].FideDmDev := leitor.rCampo(tcStr, 'ideDmDev');
            Items[i].FtpPgto   := leitor.rCampo(tcInt, 'tpPgto');
            Items[i].FdtPgto   := leitor.rCampo(tcDat, 'dtPgto');
            Items[i].FcodCateg := leitor.rCampo(tcInt, 'codCateg');
          end;

          j := 0;
          while Leitor.rExtrai(3, 'infoIR', '', j + 1) <> '' do
          begin
            with IdeTrabalhador.DmDev.Items[i].infoIR do
            begin
              New;
              Items[j].FtpInfoIR := leitor.rCampo(tcInt, 'tpInfoIR');
              Items[j].Fvalor    := leitor.rCampo(tcDe2, 'valor');
            end;

            if VersaoDF >= veS01_02_00 then
            begin
              k := 0;
              while Leitor.rExtrai(4, 'infoProcJudRub', '', k + 1) <> '' do
              begin
                with IdeTrabalhador.DmDev.Items[i].infoIR.Items[j].infoProcJudRub do
                begin
                  New;
                  Items[k].FnrProc   := leitor.rCampo(tcStr, 'nrProc');
                  Items[k].FufVara   := leitor.rCampo(tcStr, 'ufVara');
                  Items[k].FcodMunic := leitor.rCampo(tcInt, 'codMunic');
                  Items[k].FidVara   := leitor.rCampo(tcInt, 'idVara');
                end;

                inc(k);
              end; { infoProcJudRub }
            end;

            inc(j);
          end; { infoIR }

          j := 0;
          while Leitor.rExtrai(3, 'totApurMen', '', j + 1) <> '' do
          begin
            with IdeTrabalhador.DmDev.Items[i].totApurMen do
            begin
              New;
              Items[j].FCRMen          := leitor.rCampo(tcStr, 'CRMen');
              Items[j].FvlrCRMen       := leitor.rCampo(tcDe2, 'vlrCRMen');

              if VersaoDF < veS01_02_00 then
                Items[j].FvlrCRMenSusp := leitor.rCampo(tcDe2, 'vlrCRMenSusp');
            end;

            inc(j);
          end; { totApurMen }

          j := 0;
          while Leitor.rExtrai(3, 'totApurDia', '', j + 1) <> '' do
          begin
            with IdeTrabalhador.DmDev.Items[i].totApurDia do
            begin
              New;
              Items[j].FperApurDia     := leitor.rCampo(tcInt, 'perApurDia');
              Items[j].FCRDia          := leitor.rCampo(tcStr, 'CRDia');
              Items[j].FvlrCRDia       := leitor.rCampo(tcDe2, 'vlrCRDia');

              if VersaoDF < veS01_02_00 then
                Items[j].FvlrCRDiaSusp := leitor.rCampo(tcDe2, 'vlrCRDiaSusp');
            end;

            inc(j);
          end; { totApurDia }

          if VersaoDF >= veS01_02_00 then
          begin
            if leitor.rExtrai(3, 'infoRRA') <> '' then
            begin
              with IdeTrabalhador.DmDev.Items[i].infoRRA do
              begin
                tpProcRRA   := eSStrToTpProcRRA(ok, leitor.rCampo(tcStr, 'tpProcRRA'));
                nrProcRRA   := leitor.rCampo(tcStr, 'nrProcRRA');
                descRRA     := leitor.rCampo(tcStr, 'descRRA');
                qtdMesesRRA := leitor.rCampo(tcDe1, 'qtdMesesRRA');
              end;

              if leitor.rExtrai(4, 'despProcJud') <> '' then
              begin
                with IdeTrabalhador.DmDev.Items[i].infoRRA.despProcJud do
                begin
                  vlrDespCustas    := leitor.rCampo(tcDe2, 'vlrDespCustas');
                  vlrDespAdvogados := leitor.rCampo(tcDe2, 'vlrDespAdvogados');
                end;
              end; { despProcJud }

              j := 0;
              while Leitor.rExtrai(4, 'ideAdv', '', j + 1) <> '' do
              begin
                with IdeTrabalhador.DmDev.Items[i].infoRRA.ideAdv do
                begin
                  New;
                  Items[j].tpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                  Items[j].nrInsc := leitor.rCampo(tcStr, 'nrInsc');
                  Items[j].vlrAdv := leitor.rCampo(tcDe2, 'vlrAdv');
                end;

                inc(j);
              end; { ideADV }
            end; { infoRRA }

            if leitor.rExtrai(3, 'infoPgtoExt') <> '' then
            begin
              with IdeTrabalhador.DmDev do
              begin
                Items[i].infoPgtoExt.FpaisResidExt := leitor.rCampo(tcInt, 'paisResidExt');
                Items[i].infoPgtoExt.FindNIF       := eSStrToIndNIF(ok, leitor.rCampo(tcStr, 'indNIF'));
                Items[i].infoPgtoExt.FnifBenef     := leitor.rCampo(tcStr, 'nifBenef');
                Items[i].infoPgtoExt.FfrmTribut    := leitor.rCampo(tcInt, 'frmTribut');
              end;

              if leitor.rExtrai(4, 'endExt') <> '' then
              begin
                with IdeTrabalhador.DmDev.Items[i].infoPgtoExt.endExt do
                begin
                  endDscLograd := leitor.rCampo(tcStr, 'endDscLograd');
                  endNrLograd  := leitor.rCampo(tcStr, 'endNrLograd');
                  endComplem   := leitor.rCampo(tcStr, 'endComplem');
                  endBairro    := leitor.rCampo(tcStr, 'endBairro');
                  endCidade    := leitor.rCampo(tcStr, 'endCidade');
                  endEstado    := leitor.rCampo(tcStr, 'endEstado');
                  endCodPostal := leitor.rCampo(tcStr, 'endCodPostal');
                  telef        := leitor.rCampo(tcStr, 'telef');
                end;
              end; { endExt }
            end; { infoPgtoExt }

            if leitor.rExtrai(3, 'infoIRComplem') <> '' then
            begin
              IdeTrabalhador.infoIRComplem.FdtLaudo := leitor.rCampo(tcDat, 'dtLaudo');

              j := 0;
              while Leitor.rExtrai(4, 'ideDep', '', j + 1) <> '' do
              begin
                with IdeTrabalhador.infoIRComplem.ideDep do
                begin
                  New;
                  Items[j].FcpfDep   := leitor.rCampo(tcStr, 'cpfDep');
                  Items[j].FdepIRRF  := eSStrToSimNaoFacultativo(ok, leitor.rCampo(tcStr, 'depIRRF'));

                  // terminar
                  //Items[j].FdtNascto := leitor.rCampo(tcDat, 'dtNascto');
                  Items[j].Fnome     := leitor.rCampo(tcStr, 'nome');
                  Items[j].FtpDep    := eSStrToTpDep(ok, leitor.rCampo(tcStr, 'tpDep'));
                  Items[j].FdescrDep := leitor.rCampo(tcStr, 'descrDep');
                end;

                inc(j);
              end; { ideDep }

              j := 0;
              while Leitor.rExtrai(4, 'infoIRCR', '', j + 1) <> '' do
              begin
                with IdeTrabalhador.infoIRComplem.infoIRCR do
                begin
                  New;
                  Items[j].FtpCR := leitor.rCampo(tcStr, 'tpCR');
                end;

                k := 0;
                while Leitor.rExtrai(5, 'dedDepen', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].dedDepen do
                  begin
                    New;
                    Items[k].FtpRend    := leitor.rCampo(tcInt, 'tpRend');
                    Items[k].FcpfDep    := leitor.rCampo(tcStr, 'cpfDep');
                    Items[k].FvlrDedDep := leitor.rCampo(tcDe2, 'vlrDedDep');
                  end;

                  inc(k);
                end; { dedDepen }

                k := 0;
                while Leitor.rExtrai(5, 'penAlim', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].penAlim do
                  begin
                    New;
                    Items[k].FtpRend        := leitor.rCampo(tcInt, 'tpRend');
                    Items[k].FcpfDep        := leitor.rCampo(tcStr, 'cpfDep');
                    Items[k].FvlrDedPenAlim := leitor.rCampo(tcDe2, 'vlrDedPenAlim');
                  end;

                  inc(k);
                end; { penAlim }

                k := 0;
                while Leitor.rExtrai(5, 'previdCompl', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].previdCompl do
                  begin
                    New;
                    Items[k].FtpPrev        := eSStrTotpTpPrev(ok, leitor.rCampo(tcStr, 'tpPrev'));
                    Items[k].FcnpjEntidPC   := leitor.rCampo(tcStr, 'cnpjEntidPC');
                    Items[k].FvlrDedPC      := leitor.rCampo(tcDe2, 'vlrDedPC');
                    Items[k].FvlrPatrocFunp := leitor.rCampo(tcDe2, 'vlrPatrocFunp');
                  end;

                  inc(k);
                end; { previdCompl }

                k := 0;
                while Leitor.rExtrai(5, 'infoProcRet', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].infoProcRet do
                  begin
                    New;
                    Items[k].FtpProcRet := eSStrTotpTpProcRet(ok, leitor.rCampo(tcStr, 'tpProcRet'));
                    Items[k].FnrProcRet := leitor.rCampo(tcStr, 'nrProcRet');
                    Items[k].FcodSusp   := leitor.rCampo(tcInt, 'codSusp');
                  end;

                  l := 0;
                  while Leitor.rExtrai(6, 'infoValores', '', l + 1) <> '' do
                  begin
                    with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].infoProcRet.Items[k].infoValores do
                    begin
                      New;
                      Items[l].FindApuracao  := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'indApuracao'));
                      Items[l].FvlrNRetido   := leitor.rCampo(tcDe2, 'vlrNRetido');
                      Items[l].FvlrDepJud    := leitor.rCampo(tcDe2, 'vlrDepJud');
                      Items[l].FvlrCmpAnoCal := leitor.rCampo(tcDe2, 'vlrCmpAnoCal');
                      Items[l].FvlrCmpAnoAnt := leitor.rCampo(tcDe2, 'vlrCmpAnoAnt');
                      Items[l].FvlrRendSusp  := leitor.rCampo(tcDe2, 'vlrRendSusp');
                    end;

                    m := 0;
                    while Leitor.rExtrai(6, 'dedSusp', '', m + 1) <> '' do
                    begin
                      with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].infoProcRet.Items[k].infoValores.Items[l].dedSusp do
                      begin
                        New;
                        Items[m].FindTpDeducao  := eSStrTotpIndTpDeducao(ok, leitor.rCampo(tcStr, 'indTpDeducao'));
                        Items[m].FvlrDedSusp    := leitor.rCampo(tcDe2, 'vlrDedSusp');
                        Items[m].FcnpjEntidPC   := leitor.rCampo(tcStr, 'cnpjEntidPC');
                        Items[m].FvlrPatrocFunp := leitor.rCampo(tcDe2, 'vlrPatrocFunp');
                      end;

                      n := 0;
                      while Leitor.rExtrai(7, 'benefPen', '', n + 1) <> '' do
                      begin
                        with IdeTrabalhador.infoIRComplem.infoIRCR.Items[j].infoProcRet.Items[k].infoValores.Items[l].dedSusp.Items[m].benefPen do
                        begin
                          New;
                          Items[n].FcpfDep       := leitor.rCampo(tcStr, 'cpfDep');
                          Items[n].FvlrDepenSusp := leitor.rCampo(tcDe2, 'vlrDepenSusp');
                        end;

                        inc(n);
                      end; { benefPen }

                      inc(m);
                    end; { dedSusp }

                    inc(l);
                  end; { infoValores }

                  inc(k);
                end; { infoProcRet }

                inc(j);
              end; { infoIRCR }

              j := 0;
              while Leitor.rExtrai(4, 'planSaude', '', j + 1) <> '' do
              begin
                with IdeTrabalhador.infoIRComplem.planSaude do
                begin
                  New;
                  Items[j].FcnpjOper    := leitor.rCampo(tcStr, 'cnpjOper');
                  Items[j].FregANS      := leitor.rCampo(tcStr, 'regANS');
                  Items[j].FvlrSaudeTit := leitor.rCampo(tcDe2, 'vlrSaudeTit');
                end;

                k := 0;
                while Leitor.rExtrai(5, 'infoDepSau', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.planSaude.Items[j].infoDepSau do
                  begin
                    New;
                    Items[k].FcpfDep      := leitor.rCampo(tcStr, 'cpfDep');
                    Items[k].FvlrSaudeDep := leitor.rCampo(tcDe2, 'vlrSaudeDep');
                  end;

                  inc(k);
                end; { infoDepSau }

                inc(j);
              end; { planSaude }

              j := 0;
              while Leitor.rExtrai(4, 'infoReembMed', '', j + 1) <> '' do
              begin
                with IdeTrabalhador.infoIRComplem.infoReembMed do
                begin
                  New;
                  Items[j].FindOrgReemb := leitor.rCampo(tcStr, 'indOrgReemb');
                  Items[j].FcnpjOper    := leitor.rCampo(tcStr, 'cnpjOper');
                  Items[j].FregANS      := leitor.rCampo(tcStr, 'regANS');
                end;

                k := 0;
                while Leitor.rExtrai(5, 'detReembTit', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoReembMed.Items[j].detReembTit do
                  begin
                    New;
                    Items[k].FtpInsc      := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                    Items[k].FnrInsc      := leitor.rCampo(tcStr, 'nrInsc');
                    Items[k].FvlrReemb    := leitor.rCampo(tcDe2, 'vlrReemb');
                    Items[k].FvlrReembAnt := leitor.rCampo(tcDe2, 'vlrReembAnt');
                  end;

                  inc(k);
                end; { detReembTit }

                k := 0;
                while Leitor.rExtrai(5, 'infoReembDep', '', k + 1) <> '' do
                begin
                  with IdeTrabalhador.infoIRComplem.infoReembMed.Items[j].infoReembDep do
                  begin
                    New;
                    Items[k].FcpfBenef := leitor.rCampo(tcStr, 'cpfBenef');
                  end;

                  l := 0;
                  while Leitor.rExtrai(6, 'detReembDep', '', l + 1) <> '' do
                  begin
                    with IdeTrabalhador.infoIRComplem.infoReembMed.Items[j].infoReembDep.Items[k].detReembDep do
                    begin
                      New;
                      Items[l].FtpInsc      := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                      Items[l].FnrInsc      := leitor.rCampo(tcStr, 'nrInsc');
                      Items[l].FvlrReemb    := leitor.rCampo(tcDe2, 'vlrReemb');
                      Items[l].FvlrReembAnt := leitor.rCampo(tcDe2, 'vlrReembAnt');
                    end;

                    inc(l);
                  end; { detReembDep }

                  inc(k);
                end; { infoReembDep }

                inc(j);
              end; { infoReembMed }
            end; { infoIRComplem }
          end;

          inc(i);
        end; { dmDev }
      end;

      Result := True;
    end; { evtIrrfBenef }
  except
    Result := False;
  end;
end;

function TEvtIrrfBenef.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j, k, l, m: Integer;
  infoIR: TinfoIRCollectionItem;
  infoProcJudRub: TInfoProcJudRubCollectionItem;
  infoValores: TinfoValoresCollectionItem;
  detReembDep: TdetReembDepCollectionItem;
begin
  Result := True;

  AIni := TMemIniFile.Create('');
  try
    with Self do
    begin
      sSecao := 'evtIrrfBenef';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'nrRecArqBase', IdeEvento.nrRecArqBase);
      AIni.WriteString(sSecao, 'perApur',      IdeEvento.perApur);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'ideTrabalhador';
      AIni.WriteString(sSecao, 'cpfBenef', ideTrabalhador.cpfBenef);

      for i := 0 to ideTrabalhador.dmDev.Count -1 do
      begin
        sSecao := 'dmDev' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'perRef', ideTrabalhador.dmDev.Items[i].perRef);
        AIni.WriteString(sSecao, 'indResBr', ideTrabalhador.dmDev.Items[i].ideDmDev);
        AIni.WriteInteger(sSecao, 'tpPgto', ideTrabalhador.dmDev.Items[i].tpPgto);
        AIni.WriteDate(sSecao, 'dtPgto', ideTrabalhador.dmDev.Items[i].dtPgto);
        AIni.WriteInteger(sSecao, 'codCateg', ideTrabalhador.dmDev.Items[i].codCateg);

        for j := 0 to ideTrabalhador.dmDev.Items[i].infoIR.Count -1 do
        begin
          sSecao := 'infoIR' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

          infoIR := ideTrabalhador.dmDev.Items[i].infoIR.Items[j];
          AIni.WriteInteger(sSecao, 'tpInfoIR', infoIR.tpInfoIR);
          AIni.WriteFloat(sSecao, 'valor', infoIR.valor);

          for k := 0 to infoIR.infoProcJudRub.Count -1 do
          begin
            sSecao := 'infoIR' + IntToStrZero(I, 3) + IntToStrZero(j, 3) + IntToStrZero(k, 3);

            infoProcJudRub := infoIR.infoProcJudRub.Items[k];
            AIni.WriteString(sSecao, 'nrProc', infoProcJudRub.nrProc);
            AIni.WriteString(sSecao, 'ufVara', infoProcJudRub.ufVara);
            AIni.WriteInteger(sSecao, 'codMunic', infoProcJudRub.codMunic);
            AIni.WriteInteger(sSecao, 'idVara', infoProcJudRub.idVara);
          end;
        end;

        for j := 0 to ideTrabalhador.dmDev.Items[i].totApurMen.Count -1 do
        begin
          sSecao := 'totApurMen' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

          AIni.WriteString(sSecao, 'CRMen', ideTrabalhador.dmDev.Items[i].totApurMen.Items[j].CRMen);
          AIni.WriteFloat(sSecao, 'vlrCRMen', ideTrabalhador.dmDev.Items[i].totApurMen.Items[j].vlrCRMen);
        end;

        for j := 0 to ideTrabalhador.dmDev.Items[i].totApurDia.Count -1 do
        begin
          sSecao := 'totApurDia' + IntToStrZero(I, 3) + IntToStrZero(j, 3);

          AIni.WriteInteger(sSecao, 'perApurDia', ideTrabalhador.dmDev.Items[i].totApurDia.Items[j].perApurDia);
          AIni.WriteString(sSecao, 'CRDia', ideTrabalhador.dmDev.Items[i].totApurDia.Items[j].CRDia);
          AIni.WriteFloat(sSecao, 'vlrCRDia', ideTrabalhador.dmDev.Items[i].totApurDia.Items[j].vlrCRDia);
        end;

        sSecao := 'infoRRA' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'tpProcRRA', eSTpProcRRAToStr(ideTrabalhador.dmDev.Items[i].infoRRA.tpProcRRA));
        AIni.WriteString(sSecao, 'nrProcRRA', ideTrabalhador.dmDev.Items[i].infoRRA.nrProcRRA);
        AIni.WriteString(sSecao, 'descRRA', ideTrabalhador.dmDev.Items[i].infoRRA.descRRA);
        AIni.WriteFloat(sSecao, 'qtdMesesRRA', ideTrabalhador.dmDev.Items[i].infoRRA.qtdMesesRRA);

        sSecao := 'despProcJud' + IntToStrZero(I, 3);

        AIni.WriteFloat(sSecao, 'vlrDespCustas', ideTrabalhador.dmDev.Items[i].infoRRA.despProcJud.vlrDespCustas);
        AIni.WriteFloat(sSecao, 'vlrDespAdvogados', ideTrabalhador.dmDev.Items[i].infoRRA.despProcJud.vlrDespAdvogados);

        for j := 0 to ideTrabalhador.dmDev.Items[i].infoRRA.ideAdv.Count -1 do
        begin
          sSecao := 'ideAdv' + IntToStrZero(I, 3) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(ideTrabalhador.dmDev.Items[i].infoRRA.ideAdv.Items[j].tpInsc));
          AIni.WriteString(sSecao, 'nrInsc', ideTrabalhador.dmDev.Items[i].infoRRA.ideAdv.Items[j].nrInsc);
          AIni.WriteFloat(sSecao, 'vlrAdv', ideTrabalhador.dmDev.Items[i].infoRRA.ideAdv.Items[j].vlrAdv);
        end;

        sSecao := 'infoPgtoExt' + IntToStrZero(I, 3);

        AIni.WriteInteger(sSecao, 'paisResidExt', ideTrabalhador.dmDev.Items[i].infoPgtoExt.paisResidExt);
        AIni.WriteString(sSecao, 'indNIF', eSIndNIFToStr(ideTrabalhador.dmDev.Items[i].infoPgtoExt.indNIF));
        AIni.WriteString(sSecao, 'nifBenef', ideTrabalhador.dmDev.Items[i].infoPgtoExt.nifBenef);
        AIni.WriteInteger(sSecao, 'frmTribut', ideTrabalhador.dmDev.Items[i].infoPgtoExt.frmTribut);

        sSecao := 'endExt' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'endDscLograd', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endDscLograd);
        AIni.WriteString(sSecao, 'endNrLograd', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endNrLograd);
        AIni.WriteString(sSecao, 'endComplem', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endComplem);
        AIni.WriteString(sSecao, 'endBairro', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endBairro);
        AIni.WriteString(sSecao, 'endCidade', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endCidade);
        AIni.WriteString(sSecao, 'endEstado', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endEstado);
        AIni.WriteString(sSecao, 'endCodPostal', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.endCodPostal);
        AIni.WriteString(sSecao, 'telef', ideTrabalhador.dmDev.Items[i].infoPgtoExt.endExt.telef);
      end;

      sSecao := 'infoIRComplem';
      AIni.WriteDate(sSecao, 'dtLaudo', ideTrabalhador.infoIRComplem.dtLaudo);

      for i := 0 to ideTrabalhador.infoIRComplem.ideDep.Count -1 do
      begin
        sSecao := 'ideDep' + IntToStrZero(I, 3);

        AIni.WriteString(sSecao, 'cpfDep', ideTrabalhador.infoIRComplem.ideDep.Items[i].cpfDep);
        AIni.WriteString(sSecao, 'depIRRF', eSSimNaoFacultativoToStr(ideTrabalhador.infoIRComplem.ideDep.Items[i].depIRRF));
        AIni.WriteDate(sSecao, 'dtNascto', ideTrabalhador.infoIRComplem.ideDep.Items[i].dtNascto);
        AIni.WriteString(sSecao, 'nome', ideTrabalhador.infoIRComplem.ideDep.Items[i].nome);
        AIni.WriteString(sSecao, 'tpDep', eStpDepToStr(ideTrabalhador.infoIRComplem.ideDep.Items[i].tpDep));
        AIni.WriteString(sSecao, 'descrDep', ideTrabalhador.infoIRComplem.ideDep.Items[i].descrDep);
      end;

      for i := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Count -1 do
      begin
        sSecao := 'infoIRCR' + IntToStrZero(I, 2);

        AIni.WriteString(sSecao, 'cpfDep', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].tpCR);

        for j := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Items[i].dedDepen.Count -1 do
        begin
          sSecao := 'dedDepen' + IntToStrZero(I, 2) + IntToStrZero(j, 3);

          AIni.WriteInteger(sSecao, 'tpRend', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].dedDepen.Items[j].tpRend);
          AIni.WriteString(sSecao, 'cpfDep', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].dedDepen.Items[j].cpfDep);
          AIni.WriteFloat(sSecao, 'vlrDedDep', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].dedDepen.Items[j].vlrDedDep);
        end;

        for j := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Items[i].penAlim.Count -1 do
        begin
          sSecao := 'penAlim' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteInteger(sSecao, 'tpRend', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].penAlim.Items[j].tpRend);
          AIni.WriteString(sSecao, 'cpfDep', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].penAlim.Items[j].cpfDep);
          AIni.WriteFloat(sSecao, 'vlrDedPenAlim', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].penAlim.Items[j].vlrDedPenAlim);
        end;

        for j := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Items[i].previdCompl.Count -1 do
        begin
          sSecao := 'previdCompl' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpPrev', eStpTpPrevToStr(ideTrabalhador.infoIRComplem.infoIRCR.Items[i].previdCompl.Items[j].tpPrev));
          AIni.WriteString(sSecao, 'cnpjEntidPC', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].previdCompl.Items[j].cnpjEntidPC);
          AIni.WriteFloat(sSecao, 'vlrDedPC', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].previdCompl.Items[j].vlrDedPC);
          AIni.WriteFloat(sSecao, 'vlrPatrocFunp', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].previdCompl.Items[j].vlrPatrocFunp);
        end;

        for j := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Count -1 do
        begin
          sSecao := 'infoProcRet' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpProcRet', eStpTpProcRetToStr(ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Items[j].tpProcRet));
          AIni.WriteString(sSecao, 'nrProcRet', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Items[j].nrProcRet);
          AIni.WriteInteger(sSecao, 'codSusp', ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Items[j].codSusp);

          for k := 0 to ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Items[j].infoValores.Count -1 do
          begin
            sSecao := 'infoProcRet' + IntToStrZero(I, 2) + IntToStrZero(j, 2) + IntToStrZero(k, 1);

            infoValores := ideTrabalhador.infoIRComplem.infoIRCR.Items[i].infoProcRet.Items[j].infoValores.Items[k];
            AIni.WriteString(sSecao, 'indApuracao', eSIndApuracaoToStr(infoValores.indApuracao));
            AIni.WriteFloat(sSecao, 'vlrNRetido', infoValores.vlrNRetido);
            AIni.WriteFloat(sSecao, 'vlrDepJud', infoValores.vlrDepJud);
            AIni.WriteFloat(sSecao, 'vlrCmpAnoCal', infoValores.vlrCmpAnoCal);
            AIni.WriteFloat(sSecao, 'vlrCmpAnoAnt', infoValores.vlrCmpAnoAnt);
            AIni.WriteFloat(sSecao, 'vlrRendSusp', infoValores.vlrRendSusp);

            for l := 0 to infoValores.dedSusp.Count -1 do
            begin
              sSecao := 'infoValores' + IntToStrZero(I, 2) + IntToStrZero(j, 2) + IntToStrZero(k, 1) +
                                        IntToStrZero(l, 2);

              AIni.WriteString(sSecao, 'indTpDeducao', eStpTpIndTpDeducaoToStr(infoValores.dedSusp.Items[l].indTpDeducao));
              AIni.WriteFloat(sSecao, 'vlrDedSusp', infoValores.dedSusp.Items[l].vlrDedSusp);
              AIni.WriteString(sSecao, 'cnpjEntidPC', infoValores.dedSusp.Items[l].cnpjEntidPC);
              AIni.WriteFloat(sSecao, 'vlrPatrocFunp', infoValores.dedSusp.Items[l].vlrPatrocFunp);

              for m := 0 to infoValores.dedSusp.Items[l].benefPen.Count -1 do
              begin
                sSecao := 'infoProcRet' + IntToStrZero(I, 2) + IntToStrZero(j, 2) + IntToStrZero(k, 1) +
                                          IntToStrZero(l, 2) + IntToStrZero(m, 2);

                AIni.WriteString(sSecao, 'cpfDep', infoValores.dedSusp.Items[l].benefPen.Items[m].cpfDep);
                AIni.WriteFloat(sSecao, 'vlrDepenSusp', infoValores.dedSusp.Items[l].benefPen.Items[m].vlrDepenSusp);
              end;
            end;
          end;
        end;
      end;

      for i := 0 to ideTrabalhador.infoIRComplem.planSaude.Count -1 do
      begin
        sSecao := 'planSaude' + IntToStrZero(I, 2);

        AIni.WriteString(sSecao, 'cnpjOper', ideTrabalhador.infoIRComplem.planSaude.Items[i].cnpjOper);
        AIni.WriteString(sSecao, 'regANS', ideTrabalhador.infoIRComplem.planSaude.Items[i].regANS);
        AIni.WriteFloat(sSecao, 'vlrSaudeTit', ideTrabalhador.infoIRComplem.planSaude.Items[i].vlrSaudeTit);

        for j := 0 to ideTrabalhador.infoIRComplem.planSaude.Items[i].infoDepSau.Count -1 do
        begin
          sSecao := 'infoDepSau' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'cpfDep', ideTrabalhador.infoIRComplem.planSaude.Items[i].infoDepSau.Items[j].cpfDep);
          AIni.WriteFloat(sSecao, 'vlrSaudeDep', ideTrabalhador.infoIRComplem.planSaude.Items[i].infoDepSau.Items[j].vlrSaudeDep);
        end;
      end;

      for i := 0 to ideTrabalhador.infoIRComplem.infoReembMed.Count -1 do
      begin
        sSecao := 'infoReembMed' + IntToStrZero(I, 2);

        AIni.WriteString(sSecao, 'indOrgReemb', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].indOrgReemb);
        AIni.WriteString(sSecao, 'cnpjOper', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].cnpjOper);
        AIni.WriteString(sSecao, 'regANS', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].regANS);

        for j := 0 to ideTrabalhador.infoIRComplem.infoReembMed.Items[i].detReembTit.Count -1 do
        begin
          sSecao := 'detReembTit' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(ideTrabalhador.infoIRComplem.infoReembMed.Items[i].detReembTit.Items[j].tpInsc));
          AIni.WriteString(sSecao, 'nrInsc', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].detReembTit.Items[j].nrInsc);
          AIni.WriteFloat(sSecao, 'vlrReemb', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].detReembTit.Items[j].vlrReemb);
          AIni.WriteFloat(sSecao, 'vlrReembAnt', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].detReembTit.Items[j].vlrReembAnt);
        end;

        for j := 0 to ideTrabalhador.infoIRComplem.infoReembMed.Items[i].infoReembDep.Count -1 do
        begin
          sSecao := 'infoReembDep' + IntToStrZero(I, 2) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'cpfBenef', ideTrabalhador.infoIRComplem.infoReembMed.Items[i].infoReembDep.Items[j].cpfBenef);

          for k := 0 to ideTrabalhador.infoIRComplem.infoReembMed.Items[i].infoReembDep.Items[j].detReembDep.Count -1 do
          begin
            sSecao := 'infoReembDep' + IntToStrZero(I, 2) + IntToStrZero(j, 2) + IntToStrZero(k, 2);

            detReembDep := ideTrabalhador.infoIRComplem.infoReembMed.Items[i].infoReembDep.Items[j].detReembDep.Items[k];
            AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(detReembDep.tpInsc));
            AIni.WriteString(sSecao, 'nrInsc', detReembDep.nrInsc);
            AIni.WriteFloat(sSecao, 'vlrReemb', detReembDep.vlrReemb);
            AIni.WriteFloat(sSecao, 'vlrReembAnt', detReembDep.vlrReembAnt);
          end;
        end;
      end;
    end;
  finally
    AIni.Free;
  end;
end;

end.
