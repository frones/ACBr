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

unit pcesS1210;

interface

uses
  SysUtils, Classes, Controls,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnGerador, pcnConsts,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TPgtoExt = class;
  TInfoPgtoExt = class;
  TIdeBenef = class;
  TInfoPgtoCollection = class;
  TInfoPgtoItem = class;
  TdetPgtoFlCollection = class;
  TdetPgtoFlItem = class;
  TEvtPgtos = class;
  TS1210CollectionItem = class;
  TDeps = class;
  TDetPgtoBenPr = class;
  TDetPgtoFerCollection = class;
  TDetPgtoFerItem = class;
  TRubricasComPensaoCollection = class;
  TRubricasComPensaoItem = class;
  TDetPgtoAntCollection = class;
  TDetPgtoAntItem = class;
  TInfoPgtoAntCollection = class;
  TInfoPgtoAntItem = class;
  TinfoIRComplem = class;
  TinfoDepCollection = class;
  TinfoDepCollectionItem = class;
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

  TS1210Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1210CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1210CollectionItem);
  public
    function Add: TS1210CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1210CollectionItem;
    property Items[Index: Integer]: TS1210CollectionItem read GetItem write SetItem; default;
  end;

  TS1210CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtPgtos: TEvtPgtos;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtPgtos: TEvtPgtos read FEvtPgtos write FEvtPgtos;
  end;

  TEvtPgtos = class(TeSocialEvento)
  private
    FIdeEvento : TIdeEvento3;
    FIdeEmpregador : TIdeEmpregador;
    FIdeBenef : TIdeBenef;

    {Geradores da classe}
    procedure GerarIdeBenef(objIdeBenef: TIdeBenef);
    procedure GerarInfoIRComplem(obj: TInfoIRComplem);
    procedure GerarInfoDep(obj: TinfoDepCollection);
    procedure GerarInfoIRCR(obj: TinfoIRCRCollection);
    procedure GerarDedDepen(obj: TdedDepenCollection);
    procedure GerarPenAlim(obj: TpenAlimCollection);
    procedure GerarPrevidCompl(obj: TprevidComplCollection);
    procedure GerarInfoProcRet(obj: TinfoProcRetCollection);
    procedure GerarInfoValores(obj: TinfoValoresCollection);
    procedure GerarDedSusp(obj: TdedSuspCollection);
    procedure GerarBenefPen(obj: TbenefPenCollection);
    procedure GerarPlanSaude(obj: TplanSaudeCollection);
    procedure GerarInfoDepSau(obj: TinfoDepSauCollection);
    procedure GerarInfoReembMed(obj: TinforeembMedCollection);
    procedure GerarDetReembTit(obj: TdetReembTitCollection);
    procedure GerarInfoReembDep(obj: TinfoReembDepCollection);
    procedure GerarDetReembDep(obj: TdetReembDepCollection);
    procedure GerarInfoPgto(objInfoPgto: TInfoPgtoCollection);
    procedure GerardetPgtoFl(objdetPgtofl: TdetPgtoFlCollection);
    procedure GerarRubricasComPensao(pRubricasComPensao: TRubricasComPensaoCollection; const GroupName: String = 'retPgtoTot');
    procedure GerarDetPgtoBenPr(pDetPgtoBenPr: TDetPgtoBenPr);
    procedure GerarDetPgtoFer(pDetPgtoFer: TDetPgtoFerCollection);
    procedure GeraridePgtoExt(objPgtoExt: TPgtoExt);
    procedure GerarInfoPgtoExt(objInfoPgtoExt: TInfoPgtoExt);
    procedure GerarDeps(pDeps: TDeps);
    procedure GerarCamposRubricas(pRubrica: TRubricaCollectionItem);
    procedure GerarDetPgtoAnt(pDetPgtoAnt: TDetPgtoAntCollection);
    procedure GerarInfoPgtoAnt(pInfoPgtoAnt: TInfoPgtoAntCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento : TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeBenef : TIdeBenef read FIdeBenef write FIdeBenef;
  end;

  TIdeBenef = class(TObject)
  private
    FCpfBenef : String;
    FDeps: TDeps;
    FInfoPgto : TInfoPgtoCollection;
    FinfoIRComplem: TInfoIRComplem;

    function getDeps: TDeps;
    function getInfoPgto : TInfoPgtoCollection;
    function getInfoIRComplem : TInfoIRComplem;
  public
    constructor Create;
    destructor  Destroy; override;

    function depsInst: boolean;
    function infoIRComplemInst: boolean;

    property CpfBenef : String read FCpfBenef write FCpfBenef;
    property deps: TDeps read getDeps write FDeps;
    property InfoPgto : TInfoPgtoCollection read getInfoPgto write FInfoPgto;
    property infoIRComplem: TinfoIRComplem read getInfoIRComplem write FInfoIRComplem;
  end;

  TDeps = class(TObject)
  private
    FVrDedDep: Double;
  public
    property vrDedDep: Double read FVrDedDep write FVrDedDep;
  end;

  TInfoPgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoPgtoItem;
    procedure SetItem(Index: Integer; const Value: TInfoPgtoItem);
  public
    function Add: TInfoPgtoItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoPgtoItem;
    property Items[Index: Integer]: TInfoPgtoItem read GetItem write SetItem;
  end;

  TInfoPgtoItem = class(TObject)
  private
    FDtPgto      : TDateTime;
    FTpPgto      : tpTpPgto;
    FIndResBr    : tpSimNao;
    FdetPgtoFl   : TdetPgtoFlCollection;
    FdetPgtoBenPr: TdetPgtoBenPr;
    FDetPgtoFer  : TDetPgtoFerCollection;
    FDetPgtoAnt  : TDetPgtoAntCollection;
    FIdePgtoExt  : TPgtoExt;
    FInfoPgtoExt : TInfoPgtoExt;
    FPerRef      : String;
    FIdeDmDev    : string;
    FVrLiq       : Double;
    FPaisResidExt: string;

    function GetdetPgtoFl : TdetPgtoFlCollection;
    function GetIdePgtoExt : TPgtoExt;
    function getInfoPgtoExt : TInfoPgtoExt;
    function getDetPgtoBenPr: TdetPgtoBenPr;
    function getDetPgtoFer: TDetPgtoFerCollection;
    function getDetPgtoAnt: TDetPgtoAntCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function detPgtoFlInst(): Boolean;
    function detidePgtoExtInst(): Boolean;
    function InfoPgtoExtInst(): Boolean;
    function detPgtoBenPrInst(): Boolean;
    function detPgtoFerInst(): Boolean;
    function detPgtoAntInst(): Boolean;

    property DtPgto : TDateTime read FDtPgto write FDtPgto;
    property IndResBr : TpSimNao read FIndResBr write FIndResBr;
    property TpPgto : tpTpPgto read FTpPgto write FTpPgto;
    property detPgtoFl: TdetPgtoFlCollection read GetdetPgtoFl write FdetPgtoFl;
    property detPgtoBenPr: TdetPgtoBenPr read getDetPgtoBenPr write FdetPgtoBenPr;
    property detPgtoFer: TDetPgtoFerCollection read getDetPgtoFer write FDetPgtoFer;
    property detPgtoAnt: TDetPgtoAntCollection read getDetPgtoAnt write FDetPgtoAnt;
    property IdePgtoExt : TPgtoExt read GetIdePgtoExt write FIdePgtoExt;
    property infoPgtoExt : TInfoPgtoExt read GetInfoPgtoExt write FInfoPgtoExt;
    property perRef: string read FPerRef write FPerRef;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property vrLiq: Double read FVrLiq write FVrLiq;
    property paisResidExt: string read FPaisResidExt write FPaisResidExt;
  end;

  TDetPgtoBenPr = class(TObject)
  private
    FPerRef: String;
    FIdeDmDev: string;
    FIndPgtoTt: tpSimNao;
    FVrLiq: Double;
    FRetPgtoTot: TRubricaCollection;
    FInfoPgtoParc: TRubricaCollection;

    function getRetPgtoTot: TRubricaCollection;
    function getInfoPgtoParc: TRubricaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function retPgtoTotInst: boolean;
    function infoPgtoParcInst: boolean;

    property perRef: string read FPerRef write FPerRef;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property indPgtoTt: tpSimNao read FIndPgtoTt write FIndPgtoTt;
    property vrLiq: Double read FVrLiq write FVrLiq;
    property retPgtoTot: TRubricaCollection read getRetPgtoTot write FRetPgtoTot;
    property infoPgtoParc: TRubricaCollection read getInfoPgtoParc write FInfoPgtoParc;
  end;

  TRubricasComPensaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRubricasComPensaoItem;
    procedure SetItem(Index: Integer; const Value: TRubricasComPensaoItem);
  public
    function Add: TRubricasComPensaoItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRubricasComPensaoItem;
    property Items[Index: integer]: TRubricasComPensaoItem read GetItem write SetItem;
      default;
  end;

  TRubricasComPensaoItem = class(TRubricaCollectionItem)
  private
    FPenAlim: TPensaoAlimCollection;

    function getpenAlim: TPensaoAlimCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function pensaoAlimInst: boolean;
    property penAlim: TPensaoAlimCollection read getpenAlim write FPenAlim;
  end;

  TdetPgtoFlCollection = class(TACBrObjectList)
  private
    function GetITem(Index: Integer): TdetPgtoFlItem;
    procedure SetItem(Index: Integer; const Value: TdetPgtoFlItem);
  public
    function Add: TdetPgtoFlItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetPgtoFlItem;
    property Items[Index: Integer]: TdetPgtoFlItem read GetItem write SetItem;
  end;

  TdetPgtoFlItem = class(TObject)
  private
    FperRef : String;
    FIdeDmDev: string;
    FIndPagtoTt: tpSimNao;
    FVrLiq: Double;
    FNrRecArq: string;
    FRetPagtoTot: TRubricasComPensaoCollection;
    FInfoPgtoParc: TRubricaCollection;

    function getRetPagtoTot: TRubricasComPensaoCollection;
    function getInfoPgtoParc: TRubricaCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function retPagtoToInst: boolean;
    function infoPgtoFlInst: boolean;

    property perRef : String read FperRef write FperRef;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property indPagtoTt: tpSimNao read FIndPagtoTt write FIndPagtoTt;
    property vrLiq: Double read FVrLiq write FVrLiq;
    property nrRecArq: string read FNrRecArq write FNrRecArq;
    property retPagtoTot: TRubricasComPensaoCollection read getRetPagtoTot write FRetPagtoTot;
    property infoPgtoParc: TRubricaCollection read getInfoPgtoParc write FInfoPgtoParc;
  end;

  TPgtoExt = class(TObject)
  private
    FidePais : TIdePais;
    FEndExt : TEndExt;
  public
    constructor Create;
    destructor  Destroy; override;

    property idePais: TIdePais read FIdePais write FIdePais;
    property endExt: TEndExt read FEndExt write FEndExt;
  end;

  TInfoPgtoExt = class(TObject)
  private
    FindNIF: tpIndNIF;
    FnifBenef: string;
    FfrmTribut: Integer;
    FEndExtV110: TEndExtV110;
  public
    constructor Create;
    destructor  Destroy; override;

    property indNIF: tpIndNIF read FindNIF write FindNIF;
    property nifBenef: string read FnifBenef write FnifBenef;
    property frmTribut: Integer read FfrmTribut write FfrmTribut;
    property endExt: TEndExtV110 read FEndExtV110 write FEndExtV110;
  end;

  TDetPgtoFerCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetPgtoFerItem;
    procedure SetItem(Index: Integer; const Value: TDetPgtoFerItem);
  public
    function Add: TDetPgtoFerItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetPgtoFerItem;
    property Items[Index: Integer]: TDetPgtoFerItem read GetItem write SetITem;
      default;
  end;

  TDetPgtoFerItem = class(TObject)
  private
    FCodCateg: integer;
    FDtIniGoz: TDateTime;
    FQtDias: Integer;
    FVrLiq: Double;
    FDetRubrFer: TRubricasComPensaoCollection;
    Fmatricula: String;
  public
    constructor Create;
    destructor Destroy; override;

    property codCateg: integer read FCodCateg write FCodCateg;
    property matricula: String read Fmatricula write Fmatricula;
    property dtIniGoz: TDateTime read FDtIniGoz write FDtIniGoz;
    property qtDias: integer read FQtDias write FQtDias;
    property vrLiq: Double read FVrLiq write FVrLiq;
    property detRubrFer: TRubricasComPensaoCollection read FDetRubrFer write FDetRubrFer;
  end;

  TDetPgtoAntCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetPgtoAntItem;
    procedure SetItem(Index: Integer; const Value: TDetPgtoAntItem);
  public
    function Add: TDetPgtoAntItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetPgtoAntItem;
    property Items[Index: Integer]: TDetPgtoAntItem read GetItem write SetITem;
      default;
  end;

  TDetPgtoAntItem = class(TObject)
  private
    FCodCateg: Integer;
    FInfoPgtoAnt: TInfoPgtoAntCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property codCateg: Integer read FCodCateg write FCodCateg;
    property infoPgtoAnt: TInfoPgtoAntCollection read FInfoPgtoAnt write FInfoPgtoAnt;
  end;

  TInfoPgtoAntCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoPgtoAntItem;
    procedure SetItem(Index: Integer; const Value: TInfoPgtoAntItem);
  public
    function Add: TInfoPgtoAntItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoPgtoAntItem;
    property Items[Index: Integer]: TInfoPgtoAntItem read GetItem write SetITem;
      default;
  end;

  TInfoPgtoAntItem = class(TObject)
  private
    FTpBcIRRF: tpCodIncIRRF;
    FVrBcIRRF: Double;
  public
    property tpBcIRRF: tpCodIncIRRF read FTpBcIRRF write FTpBcIRRF;
    property vrBcIRRF: Double read FVrBcIRRF write FVrBcIRRF;
  end;

  TinfoIRComplem = class(TObject)
  private
    FdtLaudo: TDateTime;
    FinfoDep: TinfoDepCollection;
    FinfoIRCR: TinfoIRCRCollection;
    FplanSaude: TplanSaudeCollection;
    FinfoReembMed: TinfoReembMedCollection;

    function getInfoDep: TinfoDepCollection;
    function getInfoIRCR: TinfoIRCRCollection;
    function getPlanSaude: TplanSaudeCollection;
    function getInfoReembMed: TinfoReembMedCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoDepInst: boolean;
    function infoIRCRInst: boolean;
    function planSaudeInst: boolean;
    function infoReembMedInst: boolean;

    property dtLaudo: TDateTime read FdtLaudo write FdtLaudo;
    property infoDep: TinfoDepCollection read getInfoDep write FinfoDep;
    property infoIRCR: TinfoIRCRCollection read getInfoIRCR write FinfoIRCR;
    property planSaude: TplanSaudeCollection read getPlanSaude write FplanSaude;
    property infoReembMed: TinfoReembMedCollection read getInfoReembMed write FinfoReembMed;
  end;

  TinfoDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoDepCollectionItem;
    procedure SetItem(Index: Integer; const Value: TInfoDepCollectionItem);
  public
    function New: TInfoDepCollectionItem;
    property Items[Index: Integer]: TInfoDepCollectionItem read GetItem write SetItem; default;
  end;

  TinfoDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FdtNascto: TDateTime;
    Fnome: string;
    FdepIRRF: tpSimNaoFacultativo;
    FtpDep: tpTpDep;
    FdescrDep: string;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property nome: string read Fnome write Fnome;
    property depIRRF: tpSimNaoFacultativo read FdepIRRF write FdepIRRF;
    property tpDep: tpTpDep read FtpDep write FtpDep;
    property descrDep: string read FdescrDep write FdescrDep;
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

    property tpCR: string read FtpCR write FtpCR;
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
    property tpRend: integer read FtpRend write FtpRend;
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDedDep: double read FvlrDedDep write FvlrDedDep;
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
    property tpRend: integer read FtpRend write FtpRend;
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDedPenAlim: double read FvlrDedPenAlim write FvlrDedPenAlim;
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
    property tpPrev: tpTpPrev read FtpPrev write FtpPrev;
    property cnpjEntidPC: string read FcnpjEntidPC write FcnpjEntidPC;
    property vlrDedPC: double read FvlrDedPC write FvlrDedPC;
    property vlrPatrocFunp: double read FvlrPatrocFunp write FvlrPatrocFunp;
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
    FcodSusp: string;
    FinfoValores: TinfoValoresCollection;

    function getInfoValores: TinfoValoresCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function infoValoresInst: boolean;

    property tpProcRet: tpTpProcRet read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: string read FcodSusp write FcodSusp;
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

    property indApuracao: tpIndApuracao read FindApuracao write FindApuracao;
    property vlrNRetido: double read FvlrNRetido write FvlrNRetido;
    property vlrDepJud: double read FvlrDepJud write FvlrDepJud;
    property vlrCmpAnoCal: double read FvlrCmpAnoCal write FvlrCmpAnoCal;
    property vlrCmpAnoAnt: double read FvlrCmpAnoAnt write FvlrCmpAnoAnt;
    property vlrRendSusp: double read FvlrRendSusp write FvlrRendSusp;
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

    property indTpDeducao: tpIndTpDeducao read FindTpDeducao write FindTpDeducao;
    property vlrDedSusp: double read FvlrDedSusp write FvlrDedSusp;
    property cnpjEntidPC: string read FcnpjEntidPC write FcnpjEntidPC;
    property vlrPatrocFunp: double read FvlrPatrocFunp write FvlrPatrocFunp;
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
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDepenSusp: double read FvlrDepenSusp write FvlrDepenSusp;
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

    property cnpjOper: string read FcnpjOper write FcnpjOper;
    property regANS: string read FregANS write FregANS;
    property vlrSaudeTit: double read FvlrSaudeTit write FvlrSaudeTit;
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
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrSaudeDep: double read FvlrSaudeDep write FvlrSaudeDep;
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

    property indOrgReemb: string read FindOrgReemb write FindOrgReemb;
    property cnpjOper: string read FcnpjOper write FcnpjOper;
    property regANS: string read FregANS write FregANS;
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
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vlrReemb: double read FvlrReemb write FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt write FvlrReembAnt;
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

    property cpfBenef: string read FcpfBenef write FcpfBenef;
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
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vlrReemb: double read FvlrReemb write FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt write FvlrReembAnt;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS1210Collection }

function TS1210Collection.Add: TS1210CollectionItem;
begin
  Result := Self.New;
end;

function TS1210Collection.GetItem(Index: Integer): TS1210CollectionItem;
begin
  Result := TS1210CollectionItem(inherited Items[Index]);
end;

procedure TS1210Collection.SetItem(Index: Integer; Value: TS1210CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1210Collection.New: TS1210CollectionItem;
begin
  Result := TS1210CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1210CollectionItem }

constructor TS1210CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1210;
  FEvtPgtos   := TEvtPgtos.Create(AOwner);
end;

destructor TS1210CollectionItem.Destroy;
begin
  FEvtPgtos.Free;

  inherited;
end;

{ TIdeBenef }

constructor TIdeBenef.create;
begin
  inherited;

  FDeps := nil;
  FInfoPgto := TInfoPgtoCollection.Create;
  FinfoIRComplem := nil;
end;

destructor TIdeBenef.destroy;
begin
  if depsInst() then
    FreeAndNil(FDeps);

  FInfoPgto.Free;

  if infoIRComplemInst() then
    FreeAndNil(FinfoIRComplem);

  inherited;
end;

function TIdeBenef.getDeps: TDeps;
begin
  if not Assigned(FDeps) then
    FDeps := TDeps.Create;
  Result := FDeps;
end;

function TIdeBenef.depsInst: boolean;
begin
  result := Assigned(FDeps);
end;

function TIdeBenef.getInfoPgto: TInfoPgtoCollection;
begin
  if (not Assigned(FInfoPgto)) then
    FInfoPgto := TInfoPgtoCollection.Create;
  Result := FInfoPgto;
end;

function TIdeBenef.getInfoIRComplem: TinfoIRComplem;
begin
  if not Assigned(FinfoIRComplem) then
    FinfoIRComplem := TinfoIRComplem.Create;
  Result := FinfoIRComplem;
end;

function TIdeBenef.InfoIRComplemInst: boolean;
begin
  result := Assigned(FinfoIRComplem);
end;

{ TInfoPgtoAntCollection }

function TInfoPgtoAntCollection.Add: TInfoPgtoAntItem;
begin
  Result := Self.New;
end;

function TInfoPgtoAntCollection.GetItem(Index: Integer): TInfoPgtoAntItem;
begin
  Result := TInfoPgtoAntItem(inherited Items[Index]);
end;

procedure TInfoPgtoAntCollection.SetItem(Index: Integer; const Value: TInfoPgtoAntItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoPgtoAntCollection.New: TInfoPgtoAntItem;
begin
  Result := TInfoPgtoAntItem.Create;
  Self.Add(Result);
end;

{ TInfoPgtoCollection }

function TInfoPgtoCollection.Add: TInfoPgtoItem;
begin
  Result := Self.New;
end;

function TInfoPgtoCollection.GetItem(Index: Integer): TInfoPgtoItem;
begin
  Result := TInfoPgtoItem(inherited Items[Index]);
end;

procedure TInfoPgtoCollection.SetItem(Index: Integer; const Value: TInfoPgtoItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoPgtoCollection.New: TInfoPgtoItem;
begin
  Result := TInfoPgtoItem.Create;
  Self.Add(Result);
end;

{ TInfoPgtoItem }

constructor TInfoPgtoItem.Create;
begin
  inherited Create;
  FdetPgtoFl    := TdetPgtoFlCollection.Create;
  FIdePgtoExt   := TPgtoExt.Create;
  FdetPgtoBenPr := nil;
  FDetPgtoFer   := nil;
  FDetPgtoAnt   := nil;
  FInfoPgtoExt  := nil;
end;

destructor TInfoPgtoItem.Destroy;
begin
  FdetPgtoFl.Free;
  FIdePgtoExt.Free;
  FreeAndNil(FdetPgtoBenPr);
  FreeAndNil(FDetPgtoFer);
  FreeAndNil(FDetPgtoAnt);
  FreeAndNil(FInfoPgtoExt);
  inherited;
end;

function TInfoPgtoItem.detPgtoAntInst: boolean;
begin
  Result := Assigned(FDetPgtoAnt);
end;

function TInfoPgtoItem.detPgtoFerInst: boolean;
begin
  Result := Assigned(FDetPgtoFer);
end;

function TInfoPgtoItem.detidePgtoExtInst: Boolean;
begin
  Result := Assigned(FidePgtoExt);
end;

function TInfoPgtoItem.InfoPgtoExtInst: Boolean;
begin
  Result := Assigned(FInfoPgtoExt);
end;

function TInfoPgtoItem.detPgtoFlInst: Boolean;
begin
  Result := Assigned(FdetPgtoFl);
end;

function TInfoPgtoItem.detPgtoBenPrInst: boolean;
begin
  Result := Assigned(FdetPgtoBenPr);
end;

function TInfoPgtoItem.GetDetPgtoAnt: TDetPgtoAntCollection;
begin
  if not Assigned(FDetPgtoAnt) then
    FDetPgtoAnt := TDetPgtoAntCollection.Create;
  Result := FDetPgtoAnt;
end;

function TInfoPgtoItem.GetdetPgtoFl: TdetPgtoFlCollection;
begin
  if not (Assigned(FdetPgtoFl)) then
    FdetPgtoFl := TdetPgtoFlCollection.Create;
  Result := FdetPgtoFl;
end;

function TInfoPgtoItem.getDetPgtoBenPr: TDetPgtoBenPr;
begin
  if not Assigned(FdetPgtoBenPr) then
    FdetPgtoBenPr := TDetPgtoBenPr.Create;
  Result := FdetPgtoBenPr;
end;

function TInfoPgtoItem.GetIdePgtoExt: TPgtoExt;
begin
  if not (Assigned(FIdePgtoExt)) then
    FIdePgtoExt := TPgtoExt.Create;
  Result := FIdePgtoExt;
end;

function TInfoPgtoItem.GetInfoPgtoExt: TInfoPgtoExt;
begin
  if not (Assigned(FInfoPgtoExt)) then
    FInfoPgtoExt := TInfoPgtoExt.Create;
  Result := FInfoPgtoExt;
end;

function TInfoPgtoItem.getDetPgtoFer: TDetPgtoFerCollection;
begin
  if not Assigned(FDetPgtoFer) then
    FDetPgtoFer := TDetPgtoFerCollection.Create;
  Result := FDetPgtoFer;
end;

{ TPgtoExt }

constructor TPgtoExt.Create;
begin
  inherited Create;
  FIdePais := TIdePais.Create;
  FEndExt  := TEndExt.Create;
end;

destructor TPgtoExt.destroy;
begin
  FIdePais.Free;
  FEndExt.Free;

  inherited;
end;

{ TRubricasComPensaoCollection }

function TRubricasComPensaoCollection.add: TRubricasComPensaoItem;
begin
  Result := Self.New;
end;

function TRubricasComPensaoCollection.GetItem(Index: Integer): TRubricasComPensaoItem;
begin
  Result := TRubricasComPensaoItem(inherited Items[Index]);
end;

procedure TRubricasComPensaoCollection.SetItem(Index: Integer; const Value: TRubricasComPensaoItem);
begin
  inherited Items[Index] := Value;
end;

function TRubricasComPensaoCollection.New: TRubricasComPensaoItem;
begin
  Result := TRubricasComPensaoItem.Create;
  Self.Add(Result);
end;

{ TRubricasComPensaoItem }

constructor TRubricasComPensaoItem.Create;
begin
  inherited;

  FPenAlim := nil;
end;

destructor TRubricasComPensaoItem.Destroy;
begin
  FreeAndNil(FPenAlim);

  inherited;
end;

function TRubricasComPensaoItem.pensaoAlimInst: boolean;
begin
  result := Assigned(FPenAlim);
end;

function TRubricasComPensaoItem.getpenAlim: TPensaoAlimCollection;
begin
  if not Assigned(FPenAlim) then
    FPenAlim := TPensaoAlimCollection.Create;
  Result := FPenAlim;
end;

{ TdetPgtoFlCollection }

function TdetPgtoFlCollection.Add: TdetPgtoFlItem;
begin
  Result := Self.New;
end;

function TdetPgtoFlCollection.GetItem(Index: Integer): TdetPgtoFlItem;
begin
  Result := TdetPgtoFlItem(inherited Items[Index]);
end;

procedure TdetPgtoFlCollection.SetItem(Index: Integer; const Value: TdetPgtoFlItem);
begin
  inherited Items[Index] := Value;
end;

function TdetPgtoFlCollection.New: TdetPgtoFlItem;
begin
  Result := TdetPgtoFlItem.Create;
  Self.Add(Result);
end;

{ TdetPgtoFlItem }

constructor TdetPgtoFlItem.create;
begin
  inherited Create;
  FRetPagtoTot  := nil;
  FInfoPgtoParc := nil;
end;

destructor TdetPgtoFlItem.destroy;
begin
  FreeAndNil(FRetPagtoTot);
  FreeAndNil(FInfoPgtoParc);

  inherited;
end;

function TdetPgtoFlItem.getRetPagtoTot: TRubricasComPensaoCollection;
begin
  if not Assigned(FRetPagtoTot) then
    FRetPagtoTot := TRubricasComPensaoCollection.Create;
  Result := FRetPagtoTot;
end;

function TdetPgtoFlItem.retPagtoToInst: boolean;
begin
  Result := Assigned(FRetPagtoTot);
end;

function TdetPgtoFlItem.getInfoPgtoParc: TRubricaCollection;
begin
  if not Assigned(FInfoPgtoParc) then
    FInfoPgtoParc := TRubricaCollection.Create;
  Result := FInfoPgtoParc;
end;

function TdetPgtoFlItem.infoPgtoFlInst: boolean;
begin
  Result := Assigned(FInfoPgtoParc);
end;

{ TDetPgtoAntCollection }

function TDetPgtoAntCollection.Add: TDetPgtoAntItem;
begin
  Result := Self.New;
end;

function TDetPgtoAntCollection.GetItem(Index: Integer): TDetPgtoAntItem;
begin
  result := TDetPgtoAntItem(inherited Items[Index]);
end;

procedure TDetPgtoAntCollection.SetItem(Index: Integer; const Value: TDetPgtoAntItem);
begin
  inherited Items[Index] := Value;
end;

function TDetPgtoAntCollection.New: TDetPgtoAntItem;
begin
  Result := TDetPgtoAntItem.Create;
  Self.Add(Result);
end;

{ TDetPgtoAntItem }

constructor TDetPgtoAntItem.Create;
begin
  inherited Create;
  FInfoPgtoAnt := TInfoPgtoAntCollection.Create;
end;

destructor TDetPgtoAntItem.Destroy;
begin
  FInfoPgtoAnt.Free;

  inherited;
end;

{ TDetPgtoFerCollection }

function TDetPgtoFerCollection.Add: TDetPgtoFerItem;
begin
  Result := Self.New;
end;

function TDetPgtoFerCollection.GetItem(Index: Integer): TDetPgtoFerItem;
begin
  result := TDetPgtoFerItem(inherited Items[Index]);
end;

procedure TDetPgtoFerCollection.SetITem(Index: Integer; const Value: TDetPgtoFerItem);
begin
  inherited Items[Index] := Value;
end;

function TDetPgtoFerCollection.New: TDetPgtoFerItem;
begin
  Result := TDetPgtoFerItem.Create;
  Self.Add(Result);
end;

{ TDetPgtoFerItem }

constructor TDetPgtoFerItem.Create;
begin
  inherited Create;
  FDetRubrFer := TRubricasComPensaoCollection.Create;
end;

destructor TDetPgtoFerItem.Destroy;
begin
  inherited;

  FDetRubrFer.Free;
end;

{ TDetPgtoBenPr }

constructor TDetPgtoBenPr.Create;
begin
  inherited;

  FRetPgtoTot   := nil;
  FInfoPgtoParc := nil;
end;

destructor TDetPgtoBenPr.Destroy;
begin
  FreeAndNil(FRetPgtoTot);
  FreeAndNil(FInfoPgtoParc);

  inherited;
end;

function TDetPgtoBenPr.getInfoPgtoParc: TRubricaCollection;
begin
  if not Assigned(FInfoPgtoParc) then
    FInfoPgtoParc := TRubricaCollection.Create;
  Result := FInfoPgtoParc;
end;

function TDetPgtoBenPr.getRetPgtoTot: TRubricaCollection;
begin
  if not Assigned(FRetPgtoTot) then
    FRetPgtoTot := TRubricaCollection.Create;
  Result := FRetPgtoTot;
end;

function TDetPgtoBenPr.infoPgtoParcInst: boolean;
begin
  Result := Assigned(FInfoPgtoParc);
end;

function TDetPgtoBenPr.retPgtoTotInst: boolean;
begin
  Result := Assigned(FRetPgtoTot);
end;

{ TEvtPgtos }

constructor TEvtPgtos.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
end;

destructor TEvtPgtos.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;

  inherited;
end;

procedure TEvtPgtos.GerarCamposRubricas(pRubrica: TRubricaCollectionItem);
begin
  if (VersaoDF >= ve02_04_02) then
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, pRubrica.matricula);

  Gerador.wCampo(tcStr, '', 'codRubr',    1, 30, 1, pRubrica.codRubr);
  Gerador.wCampo(tcStr, '', 'ideTabRubr', 1,  8, 1, pRubrica.ideTabRubr);
  Gerador.wCampo(tcDe2, '', 'qtdRubr',    1,  6, 0, pRubrica.qtdRubr);
  Gerador.wCampo(tcDe2, '', 'fatorRubr',  1,  5, 0, pRubrica.fatorRubr);
  Gerador.wCampo(tcDe2, '', 'vrUnit',     1, 14, 0, pRubrica.vrUnit);
  Gerador.wCampo(tcDe2, '', 'vrRubr',     1, 14, 1, pRubrica.vrRubr);
end;

procedure TEvtPgtos.GerarRubricasComPensao(
  pRubricasComPensao: TRubricasComPensaoCollection;
  const GroupName: string = 'retPgtoTot');
var
  i: Integer;
begin
  for i := 0 to pRubricasComPensao.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);

    GerarCamposRubricas(pRubricasComPensao[i]);

    if pRubricasComPensao[i].pensaoAlimInst() then
      GerarPensaoAlim(pRubricasComPensao[i].penAlim, 'penAlim');

    Gerador.wGrupo('/'+GroupName);
  end;

  if pRubricasComPensao.Count > 99 then
    Gerador.wAlerta('', GroupName, 'Lista de ' + GroupName, ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerardetPgtoFl(objdetPgtofl: TdetPgtoFlCollection);
var
  i: Integer;
begin
  for i := 0 to objdetPgtofl.Count - 1 do
  begin
    Gerador.wGrupo('detPgtoFl');

    Gerador.wCampo(tcStr, '', 'perRef',    7,  7, 0, objdetPgtofl.Items[i].perRef);
    Gerador.wCampo(tcStr, '', 'ideDmDev',  1, 30, 1, objdetPgtofl.Items[i].ideDmDev);
    Gerador.wCampo(tcStr, '', 'indPgtoTt', 1,  1, 1, eSSimNaoToStr(objdetPgtofl.Items[i].indPagtoTt));
    Gerador.wCampo(tcDe2, '', 'vrLiq',     1, 14, 1, objdetPgtoFl.items[i].vrLiq);
    Gerador.wCampo(tcStr, '', 'nrRecArq',  1, 40, 0, objdetPgtofl.Items[i].nrRecArq);

    if objdetPgtofl.Items[i].retPagtoToInst() then
      GerarRubricasComPensao(objdetPgtofl.Items[i].retPagtoTot);

    if objdetPgtofl.Items[i].infoPgtoFlInst then
      GerarItensRemun(objdetPgtofl.Items[i].infoPgtoParc, 'infoPgtoParc');

    Gerador.wGrupo('/detPgtoFl');
  end;

  if objdetPgtofl.Count > 200 then
    Gerador.wAlerta('', 'detPgtoFl', 'Lista de Detalhamento de Pagamento', ERR_MSG_MAIOR_MAXIMO + '200');
end;

procedure TEvtPgtos.GerarDeps(pDeps: TDeps);
begin
  Gerador.wGrupo('deps');

  Gerador.wCampo(tcDe2, '', 'vrDedDep', 1, 14, 1, pDeps.vrDedDep);

  Gerador.wGrupo('/deps');
end;

procedure TEvtPgtos.GerarIdeBenef(objIdeBenef : TIdeBenef);
begin
  Gerador.wGrupo('ideBenef');
  Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, objIdeBenef.cpfBenef);

  if VersaoDF <= ve02_05_00 then
    if objIdeBenef.depsInst() then
      GerarDeps(objIdeBenef.deps);

  GerarInfoPgto(objIdeBenef.InfoPgto);

  if VersaoDF >= veS01_02_00 then
    if objIdeBenef.infoIRComplemInst() then
      GerarInfoIRComplem(objIdeBenef.infoIRComplem);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtPgtos.GerarInfoIRComplem(obj: TinfoIRComplem);
begin
  if not obj.infoDepInst() and 
     not obj.infoIRCRInst() and
     not obj.planSaudeInst() and 
     not obj.infoReembMedInst() then
    exit;

  Gerador.wGrupo('infoIRComplem');
  
  if obj.dtLaudo > 0 then
    Gerador.wCampo(tcDat, '', 'dtLaudo', 10, 10, 1, obj.dtLaudo);

  if obj.infoDepInst() then
    GerarInfoDep(obj.infoDep);

  if obj.infoIRCRInst() then
    GerarInfoIRCR(obj.infoIRCR);

  if obj.planSaudeInst() then
    GerarPlanSaude(obj.planSaude);

  if obj.infoReembMedInst() then
    GerarInfoReembMed(obj.infoReembMed);

  Gerador.wGrupo('/infoIRComplem');
end;

procedure TEvtPgtos.GerarInfoDep(obj: TinfoDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoDep');

    Gerador.wCampo(tcStr, '', 'cpfDep',     11, 11,  1, obj[i].cpfDep);
    
    if obj[i].dtNascto > 0 then
      Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10,  1, obj[i].dtNascto);

    Gerador.wCampo(tcStr, '', 'nome',        0, 70,  0, obj[i].nome);

    if obj[i].depIRRF = snfSim then
      Gerador.wCampo(tcStr, '', 'depIRRF',   1,  1,  1, eSSimNaoFacultativoToStr(obj[i].depIRRF));
    
    if obj[i].tpDep <> tdNenhum then
      Gerador.wCampo(tcInt, '', 'tpDep',     2,  2,  0, eStpDepToStr(obj[i].tpDep));

    Gerador.wCampo(tcStr, '', 'descrDep',    0, 100, 0, obj[i].descrDep);

    Gerador.wGrupo('/infoDep');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'infoDep', 'Informações de dependentes não cadastrados pelos eventos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtPgtos.GerarInfoIRCR(obj: TinfoIRCRCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoIRCR');

    Gerador.wCampo(tcStr, '', 'tpCR', 6, 6, 1, obj[i].tpCR);

    if obj[i].dedDepenInst() then
      GerarDedDepen(obj[i].dedDepen);

    if obj[i].penAlimInst() then
      GerarPenAlim(obj[i].penAlim);

    if obj[i].previdComplInst() then
      GerarPrevidCompl(obj[i].previdCompl);

    if obj[i].infoProcRetInst() then
      GerarInfoProcRet(obj[i].infoProcRet);

    Gerador.wGrupo('/infoIRCR');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoIRCR', 'Informações de Imposto de Renda, por Código de Receita - CR', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarDedDepen(obj: TdedDepenCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dedDepen');

    Gerador.wCampo(tcInt, '', 'tpRend',     2,  2, 1, obj[i].tpRend);
    Gerador.wCampo(tcStr, '', 'cpfDep',    11, 11, 1, obj[i].cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrDedDep',  1, 14, 1, obj[i].vlrDedDep);

    Gerador.wGrupo('/dedDepen');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'dedDepen', 'Dedução do rendimento tributável relativa a dependentes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtPgtos.GerarPenAlim(obj: TpenAlimCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('penAlim');

    Gerador.wCampo(tcInt, '', 'tpRend',         2,  2, 1, obj[i].tpRend);
    Gerador.wCampo(tcStr, '', 'cpfDep',        11, 11, 1, obj[i].cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrDedPenAlim',  1, 14, 1, obj[i].vlrDedPenAlim);

    Gerador.wGrupo('/penAlim');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'penAlim', 'Informação dos beneficiários da pensão alimentícia', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarPrevidCompl(obj: TprevidComplCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('previdCompl');

    Gerador.wCampo(tcStr, '', 'tpPrev',        1,  1, 1, eStpTpPrevToStr(obj[i].tpPrev));
    Gerador.wCampo(tcStr, '', 'cnpjEntidPC',  11, 11, 1, obj[i].cnpjEntidPC);
    Gerador.wCampo(tcDe2, '', 'vlrDedPC',      1, 14, 1, obj[i].vlrDedPC);
    Gerador.wCampo(tcDe2, '', 'vlrPatrocFunp', 0, 14, 0, obj[i].vlrPatrocFunp);

    Gerador.wGrupo('/previdCompl');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'previd', 'Informações relativas a planos de previdência complementar', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoProcRet(obj: TinfoProcRetCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoProcRet');

    Gerador.wCampo(tcStr, '', 'tpProcRet',  1,  1, 1, eStpTpProcRetToStr(obj[i].tpProcRet));
    Gerador.wCampo(tcStr, '', 'nrProcRet',  1, 17, 1, obj[i].nrProcRet);
    Gerador.wCampo(tcInt, '', 'codSusp',    0, 14, 0, obj[i].codSusp);

    if obj[i].infoValoresInst then
      GerarInfoValores(obj[i].infoValores);

    Gerador.wGrupo('/infoProcRet');
  end;

  if obj.Count > 50 then
    Gerador.wAlerta('', 'infoProcRet', 'Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtPgtos.GerarInfoValores(obj: TinfoValoresCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoValores');

    Gerador.wCampo(tcStr, '', 'indApuracao',  1,  1, 1, eSIndApuracaoToStr(obj[i].indApuracao));
    Gerador.wCampo(tcDe2, '', 'vlrNRetido',   0, 14, 0, obj[i].vlrNRetido);
    Gerador.wCampo(tcDe2, '', 'vlrDepJud',    0, 14, 0, obj[i].vlrDepJud);
    Gerador.wCampo(tcDe2, '', 'vlrCmpAnoCal', 0, 14, 0, obj[i].vlrCmpAnoCal);
    Gerador.wCampo(tcDe2, '', 'vlrCmpAnoAnt', 0, 14, 0, obj[i].vlrCmpAnoAnt);
    Gerador.wCampo(tcDe2, '', 'vlrRendSusp',  0, 14, 0, obj[i].vlrRendSusp);

    if obj[i].dedSuspInst() then
      GerarDedSusp(obj[i].dedSusp);

    Gerador.wGrupo('/infoValores');
  end;

  if obj.Count > 2 then
    Gerador.wAlerta('', 'infoValores', 'Informações de valores relacionados a não retenção de tributos ou a depósitos judiciais', ERR_MSG_MAIOR_MAXIMO + '2');
end;

procedure TEvtPgtos.GerarDedSusp(obj: TdedSuspCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dedSusp');

    Gerador.wCampo(tcStr, '', 'indTpDeducao',  1,  1, 1, eStpTpIndTpDeducaoToStr(obj[i].indTpDeducao));
    Gerador.wCampo(tcDe2, '', 'vlrDedSusp',    0, 14, 0, obj[i].vlrDedSusp);
    Gerador.wCampo(tcStr, '', 'cnpjEntidPC',   0, 11, 0, obj[i].cnpjEntidPC);
    Gerador.wCampo(tcDe2, '', 'vlrPatrocFunp', 0, 14, 0, obj[i].vlrPatrocFunp);

    if obj[i].benefPenInst() then
      GerarBenefPen(obj[i].benefPen);

    Gerador.wGrupo('/dedSusp');
  end;

  if obj.Count > 25 then
    Gerador.wAlerta('', 'dedSusp', 'Detalhamento das deduções com exigibilidade suspensa', ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TEvtPgtos.GerarBenefPen(obj: TbenefPenCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('benefPen');

    Gerador.wCampo(tcStr, '', 'cpfDep',       11, 11, 1, obj[i].cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrDepenSusp',  1, 14, 1, obj[i].vlrDepenSusp);

    Gerador.wGrupo('/benefPen');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'benefPen', 'Informação das deduções suspensas por dependentes e beneficiários da pensão alimentícia', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarPlanSaude(obj: TplanSaudeCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('planSaude');

    Gerador.wCampo(tcStr, '', 'cnpjOper',    14, 14, 1, obj[i].cnpjOper);
    Gerador.wCampo(tcStr, '', 'regANS',       0,  6, 0, obj[i].regANS);
    Gerador.wCampo(tcDe2, '', 'vlrSaudeTit',  1, 14, 1, obj[i].vlrSaudeTit);

    if obj[i].infoDepSauInst() then
      GerarInfoDepSau(obj[i].infoDepSau);

    Gerador.wGrupo('/planSaude');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'planSaude', 'Plano de saúde coletivo', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoDepSau(obj: TinfoDepSauCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoDepSau');

    Gerador.wCampo(tcStr, '', 'cpfDep',      11, 11, 1, obj[i].cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrSaudeDep',  1, 14, 1, obj[i].vlrSaudeDep);

    Gerador.wGrupo('/infoDepSau');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoDepSau', 'Informações de dependente de plano de saúde coletivo empresarial', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoReembMed(obj: TinfoReembMedCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoReembMed');

    Gerador.wCampo(tcStr, '', 'indOrgReemb', 1,  1, 1, obj[i].indOrgReemb);
    if obj[i].indOrgReemb = '1' then
    begin
      Gerador.wCampo(tcStr, '', 'cnpjOper',    0, 14, 0, obj[i].cnpjOper);
      Gerador.wCampo(tcStr, '', 'regANS',      0,  6, 0, obj[i].regANS);
    end;

    if obj[i].detReembTitInst() then
      GerarDetReembTit(obj[i].detReembTit);

    if obj[i].infoReembDepInst() then
      GerarInfoReembDep(obj[i].infoReembDep);

    Gerador.wGrupo('/infoReembMed');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoReembMed', 'Informações relativas a reembolsos efetuados no período de apuração (perApur) pelo empregador ao trabalhador referente a despesas médicas ou odontológicas', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarDetReembTit(obj: TdetReembTitCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('detReembTit');

    Gerador.wCampo(tcStr, '', 'tpInsc',      1,  1, 1, eSTpInscricaoToStr(obj[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',      1, 15, 1, obj[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',    0, 14, 0, obj[i].vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt', 0, 14, 0, obj[i].vlrReembAnt);

    Gerador.wGrupo('/detReembTit');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'detReembTit', 'Informação de reembolso do titular do plano de saúde coletivo empresarial.', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoReembDep(obj: TInfoReembDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoReembDep');

    Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, obj[i].cpfBenef);

    if obj[i].detReembDepInst() then
      GerarDetReembDep(obj[i].detReembDep);

    Gerador.wGrupo('/infoReembDep');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'inforeembMed', 'Informações relativas a reembolsos efetuados no período de apuração (perApur) pelo empregador ao trabalhador referente a despesas médicas ou odontológicas', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarDetReembDep(obj: TdetReembDepCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('detReembDep');

    Gerador.wCampo(tcStr, '', 'tpInsc',      1,  1, 1, eSTpInscricaoToStr(obj[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',      1, 15, 1, obj[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',    0, 14, 0, obj[i].vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt', 0, 14, 0, obj[i].vlrReembAnt);

    Gerador.wGrupo('/detReembDep');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'detReembDep', 'Detalhamento dos reembolsos efetuados em perApur pelo empregador ao trabalhador referente a despesas médicas ou odontológicas', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GeraridePgtoExt(objPgtoExt: TPgtoExt);
begin
  Gerador.wGrupo('idePgtoExt');

  GerarIdePais(objPgtoExt.idePais);
  GerarEndExt(objPgtoExt.endExt);

  Gerador.wGrupo('/idePgtoExt');
end;

procedure TEvtPgtos.GerarInfoPgtoExt(objInfoPgtoExt: TInfoPgtoExt);
begin
  Gerador.wGrupo('infoPgtoExt');

  Gerador.wCampo(tcStr, '', 'indNIF'      ,  1,  1,  1, eSIndNIFToStr(objInfoPgtoExt.indNIF));
  Gerador.wCampo(tcStr, '', 'nifBenef'    ,  1, 30,  0, objInfoPgtoExt.nifBenef);
  Gerador.wCampo(tcStr, '', 'frmTribut'   ,  1,  2,  1, objInfoPgtoExt.frmTribut);
  
  if objInfoPgtoExt.endExt.endDscLograd <> '' then
  begin
    Gerador.wGrupo('endExt');

    Gerador.wCampo(tcStr, '', 'endDscLograd',  1, 80,  0, objInfoPgtoExt.endExt.endDscLograd);
    Gerador.wCampo(tcStr, '', 'endNrLograd' ,  1, 10,  0, objInfoPgtoExt.endExt.endNrLograd);
    Gerador.wCampo(tcStr, '', 'endComplem'  ,  1, 30,  0, objInfoPgtoExt.endExt.endComplem);
    Gerador.wCampo(tcStr, '', 'endBairro'   ,  1, 60,  0, objInfoPgtoExt.endExt.endBairro);
    Gerador.wCampo(tcStr, '', 'endCidade'   ,  1, 40,  0, objInfoPgtoExt.endExt.endCidade);
    Gerador.wCampo(tcStr, '', 'endEstado'   ,  1, 40,  0, objInfoPgtoExt.endExt.endEstado);
    Gerador.wCampo(tcStr, '', 'endCodPostal',  1, 12,  0, objInfoPgtoExt.endExt.endCodPostal);
    Gerador.wCampo(tcStr, '', 'telef'       ,  1, 15,  0, objInfoPgtoExt.endExt.telef);
    
    Gerador.wGrupo('/endExt');
  end;

  Gerador.wGrupo('/infoPgtoExt');
end;

procedure TEvtPgtos.GerarDetPgtoBenPr(pDetPgtoBenPr: TDetPgtoBenPr);
begin
  Gerador.wGrupo('detPgtoBenPr');

  Gerador.wCampo(tcStr, '', 'perRef',    7,  7, 1, pDetPgtoBenPr.perRef);
  Gerador.wCampo(tcStr, '', 'ideDmDev',  1, 30, 1, pDetPgtoBenPr.ideDmDev);
  Gerador.wCampo(tcStr, '', 'indPgtoTt', 1,  1, 1, eSSimNaoToStr(pDetPgtoBenPr.indPgtoTt));
  Gerador.wCampo(tcDe2, '', 'vrLiq',     1, 14, 1, pDetPgtoBenPr.vrLiq);

  if pDetPgtoBenPr.retPgtoTotInst() then
    GerarItensRemun(pDetPgtoBenPr.retPgtoTot, 'retPgtoTot');

  if pDetPgtoBenPr.infoPgtoParcInst() then
    GerarItensRemun(pDetPgtoBenPr.retPgtoTot, 'infoPgtoParc');

  Gerador.wGrupo('/detPgtoBenPr');
end;

procedure TEvtPgtos.GerarDetPgtoFer(pDetPgtoFer: TDetPgtoFerCollection);
var
  i: integer;
begin
  for i := 0 to pDetPgtoFer.Count - 1 do
  begin
    Gerador.wGrupo('detPgtoFer');

    Gerador.wCampo(tcInt, '', 'codCateg',  1,  3, 1, pDetPgtoFer[i].codCateg);

    if (VersaoDF >= ve02_04_02) then
      Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, pDetPgtoFer[i].matricula);

    Gerador.wCampo(tcDat, '', 'dtIniGoz', 10, 10, 1, pDetPgtoFer[i].dtIniGoz);
    Gerador.wCampo(tcInt, '', 'qtDias',    1,  2, 1, pDetPgtoFer[i].qtDias);
    Gerador.wCampo(tcDe2, '', 'vrLiq',     1, 14, 1, pDetPgtoFer[i].vrLiq);

    GerarRubricasComPensao(pDetPgtoFer[i].detRubrFer, 'detRubrFer');

    Gerador.wGrupo('/detPgtoFer');
  end;

  if pDetPgtoFer.Count > 5 then
    Gerador.wAlerta('', 'detPgtoFer', 'Lista de Detalhamento de Pagamento', ERR_MSG_MAIOR_MAXIMO + '5');
end;

procedure TEvtPgtos.GerarDetPgtoAnt(pDetPgtoAnt: TDetPgtoAntCollection);
var
  i: Integer;
begin
  for i := 0 to pDetPgtoAnt.Count - 1 do
  begin
    Gerador.wGrupo('detPgtoAnt');

    Gerador.wCampo(tcInt, '', 'codCateg', 1, 3, 1, pDetPgtoAnt[i].codCateg);

    GerarInfoPgtoAnt(pDetPgtoAnt[i].infoPgtoAnt);

    Gerador.wGrupo('/detPgtoAnt');
  end;

  if pDetPgtoAnt.Count > 99 then
    Gerador.wAlerta('', 'detPgtoAnt', 'Lista de Detalhamento de Pagamento Anterior', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoPgtoAnt(pInfoPgtoAnt: TInfoPgtoAntCollection);
var
  i: Integer;
begin
  for i := 0 to pInfoPgtoAnt.Count - 1 do
  begin
    Gerador.wGrupo('infoPgtoAnt');

    Gerador.wCampo(tcStr, '', 'tpBcIRRF', 2,  2, 1, eSCodIncIRRFToStr(pInfoPgtoAnt[i].tpBcIRRF));
    Gerador.wCampo(tcDe2, '', 'vrBcIRRF', 1, 14, 1, pInfoPgtoAnt[i].vrBcIRRF);

    Gerador.wGrupo('/infoPgtoAnt');
  end;

  if pInfoPgtoAnt.Count > 99 then
    Gerador.wAlerta('', 'infoPgtoAnt', 'Lista de Detalhamento de Pagamento', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtPgtos.GerarInfoPgto(objInfoPgto: TInfoPgtoCollection);
var
  i: integer;
begin
  for i := 0 to objInfoPgto.Count - 1 do
  begin
    Gerador.wGrupo('infoPgto');

    Gerador.wCampo(tcDat, '', 'dtPgto',   10, 10, 1, objInfoPgto.Items[i].dtPgto);
    Gerador.wCampo(tcStr, '', 'tpPgto',    1,  2, 1, eSTpTpPgtoToStr(objInfoPgto.Items[i].tpPgto));

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'indResBr',  1,  1, 1, eSSimNaoToStr(objInfoPgto.Items[i].indResBr));

      if (objInfoPgto.Items[i].tpPgto in [tpPgtoRemun1200, tpPgtoResc2299, tpPgtoResc2399, tpPgtoRemun1202]) then
        if (objInfoPgto.Items[i].detPgtoFlInst()) then
          GerardetPgtoFl(objInfoPgto.Items[i].detPgtoFl);

      if objInfoPgto.Items[i].detPgtoBenPrInst() then
        GerarDetPgtoBenPr(objInfoPgto.Items[i].detPgtoBenPr);

      if objInfoPgto.Items[i].detPgtoFerInst() then
        GerarDetPgtoFer(objInfoPgto.Items[i].detPgtoFer);

      if objInfoPgto.Items[i].detPgtoAntInst() then
        GerarDetPgtoAnt(objInfoPgto.Items[i].detPgtoAnt);

      if (objInfoPgto.Items[i].indResBr = tpNao) then
        if (objInfoPgto.Items[i].detidePgtoExtInst) then
          GeraridePgtoExt(objInfoPgto.Items[i].idePgtoExt);
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'perRef',    7,  7, 0, objInfoPgto.Items[i].perRef);
      Gerador.wCampo(tcStr, '', 'ideDmDev',  1, 30, 1, objInfoPgto.Items[i].ideDmDev);
      Gerador.wCampo(tcDe2, '', 'vrLiq',     1, 14, 1, objInfoPgto.items[i].vrLiq);
    end;
   
    if VersaoDF >= veS01_01_00 then
    begin   
      if (StrToIntDef(objInfoPgto.Items[i].paisResidExt, 0) > 0) and (StrToInt(objInfoPgto.Items[i].paisResidExt) > 105) and
         ((StrToInt(Copy(Self.ideEvento.perApur,1,4))*100)+StrToInt(Copy(Self.ideEvento.perApur,6,2)) >= 202303) then
      begin
        Gerador.wCampo(tcStr, '', 'paisResidExt',  1,  3, 1, objInfoPgto.Items[i].paisResidExt);

        if (objInfoPgto.Items[i].InfoPgtoExtInst()) then
          GerarInfoPgtoExt(objInfoPgto.Items[i].InfoPgtoExt);
      end;
    end;

    Gerador.wGrupo('/infoPgto');
  end;

  if objInfoPgto.Count > 60 then
    Gerador.wAlerta('', 'infoPgto', 'Lista de Informações de Pagamento', ERR_MSG_MAIOR_MAXIMO + '60');
end;

function TEvtPgtos.GerarXML: Boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtPgtos');
    Gerador.wGrupo('evtPgtos Id="' + Self.Id + '"');

    if VersaoDF <= ve02_05_00 then
      GerarIdeEvento3(Self.ideEvento, True, True, False)
    else
      GerarIdeEvento3(Self.ideEvento, True, False, True);

    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeBenef(Self.ideBenef);

    Gerador.wGrupo('/evtPgtos');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtPgtos');

//    Validar(schevtPgtos);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtPgtos.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
  zerosInfoPgto: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtPgtos';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      IdeEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideBenef';
      ideBenef.cpfBenef := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);

      sSecao := 'deps';
      if INIRec.ReadString(sSecao, 'vrDedDep', '') <> '' then
        ideBenef.deps.vrDedDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vrDedDep', ''), 0);

      sFim := INIRec.ReadString('infoPgto01', 'dtPgto', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        zerosInfoPgto := 3
      else
        zerosInfoPgto := 2;

      I := 1;
      while true do
      begin
        // de 01 até 999
        sSecao := 'infoPgto' + IntToStrZero(I, zerosInfoPgto);
        sFim   := INIRec.ReadString(sSecao, 'dtPgto', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideBenef.InfoPgto.New do
        begin
          DtPgto   := StringToDateTime(sFim);
          TpPgto   := eSStrTotpTpPgto(Ok, INIRec.ReadString(sSecao, 'tpPgto', '1'));
          IndResBr := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indResBr', 'S'));

          PerRef   := INIRec.ReadString(sSecao, 'PerRef', EmptyStr);
          IdeDmDev := INIRec.ReadString(sSecao, 'IdeDmDev', EmptyStr);
          VrLiq    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrLiq', ''), 0);
          paisResidExt := INIRec.ReadString(sSecao, 'paisResidExt', EmptyStr);

          J := 1;
          while true do
          begin
            // de 001 até 200
            sSecao := 'detPgtoFl' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

            if (sFim = 'FIM') then
              break;

            with detPgtoFl.New do
            begin
              perRef     := sFim;
              ideDmDev   := INIRec.ReadString(sSecao, 'ideDmDev', EmptyStr);
              indPagtoTt := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPagtoTt', 'S'));
              vrLiq      := StringToFloatDef(INIRec.ReadString(sSecao, 'vrLiq', ''), 0);
              nrRecArq   := INIRec.ReadString(sSecao, 'nrRecArq', EmptyStr);

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'retPgtoTot' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with retPagtoTot.New do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'penAlim' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 3) +
                                   IntToStrZero(K, 2) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cpfBenef', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with penAlim.New do
                    begin
                      cpfBenef      := sFim;
                      dtNasctoBenef := StringToDateTime(INIRec.ReadString(sSecao, 'dtNasctoBenef', '0'));
                      nmBenefic     := INIRec.ReadString(sSecao, 'nmBenefic', EmptyStr);
                      vlrPensao     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPensao', ''), 0);
                    end;

                    Inc(L);
                  end;

                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'infoPgtoParc' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoPgtoParc.New do
                begin
                  matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          sSecao := 'detPgtoBenPr' + IntToStrZero(I, zerosInfoPgto);
          if INIRec.ReadString(sSecao, 'perRef', '') <> '' then
          begin
            detPgtoBenPr.perRef    := INIRec.ReadString(sSecao, 'perRef', EmptyStr);
            detPgtoBenPr.ideDmDev  := INIRec.ReadString(sSecao, 'ideDmDev', EmptyStr);
            detPgtoBenPr.indPgtoTt := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPgtoTt', 'S'));
            detPgtoBenPr.vrLiq     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrLiq', ''), 0);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'retPgtoTot' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with detPgtoBenPr.retPgtoTot.New do
            begin
              codRubr    := sFim;
              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
              qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
              fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
              vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'infoPgtoParc' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with detPgtoBenPr.infoPgtoParc.New do
            begin
              codRubr    := sFim;
              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
              qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
              fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
              vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 1 até 5
            sSecao := 'detPgtoFer' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 1);
            sFim   := INIRec.ReadString(sSecao, 'codCateg', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with detPgtoFer.New do
            begin
              codCateg  := StrToInt(sFim);
              matricula := INIRec.ReadString(sSecao, 'matricula', '');
              dtIniGoz  := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniGoz', '0'));
              qtDias    := INIRec.ReadInteger(sSecao, 'qtDias', 0);
              vrLiq     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrLiq', ''), 0);

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'detRubrFer' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 1) +
                               IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with detRubrFer.New do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'penAlim' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 1) +
                                   IntToStrZero(K, 2) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cpfBenef', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with penAlim.New do
                    begin
                      cpfBenef      := sFim;
                      dtNasctoBenef := StringToDateTime(INIRec.ReadString(sSecao, 'dtNasctoBenef', '0'));
                      nmBenefic     := INIRec.ReadString(sSecao, 'nmBenefic', EmptyStr);
                      vlrPensao     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPensao', ''), 0);
                    end;

                    Inc(L);
                  end;

                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'detPgtoAnt' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codCateg', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with detPgtoAnt.New do
            begin
              codCateg := StrToInt(sFim);

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'infoPgtoAnt' + IntToStrZero(I, zerosInfoPgto) + IntToStrZero(J, 2) +
                               IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpBcIRRF', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoPgtoAnt.New do
                begin
                  tpBcIRRF := eSStrToCodIncIRRF(Ok, sFim);
                  vrBcIRRF := StringToFloatDef(INIRec.ReadString(sSecao, 'vrBcIRRF', ''), 0);
                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          sSecao := 'idePgtoExt' + IntToStrZero(I, zerosInfoPgto);
          if INIRec.ReadString(sSecao, 'codPais', '') <> '' then
          begin
            idePgtoExt.idePais.codPais  := INIRec.ReadString(sSecao, 'codPais', EmptyStr);
            idePgtoExt.idePais.indNIF   := eSStrToIndNIF(Ok, INIRec.ReadString(sSecao, 'indNIF', '1'));
            idePgtoExt.idePais.nifBenef := INIRec.ReadString(sSecao, 'nifBenef', EmptyStr);

            idePgtoExt.endExt.dscLograd := INIRec.ReadString(sSecao, 'dscLograd', EmptyStr);
            idePgtoExt.endExt.nrLograd  := INIRec.ReadString(sSecao, 'nrLograd', EmptyStr);
            idePgtoExt.endExt.complem   := INIRec.ReadString(sSecao, 'complem', EmptyStr);
            idePgtoExt.endExt.bairro    := INIRec.ReadString(sSecao, 'bairro', EmptyStr);
            idePgtoExt.endExt.nmCid     := INIRec.ReadString(sSecao, 'nmCid', EmptyStr);
            idePgtoExt.endExt.codPostal := INIRec.ReadString(sSecao, 'codPostal', EmptyStr);
          end;

          sSecao := 'infoPgtoExt' + IntToStrZero(I, zerosInfoPgto);
          infoPgtoExt.indNIF    := eSStrToIndNIF(Ok, INIRec.ReadString(sSecao, 'indNIF', '1'));
          infoPgtoExt.nifBenef  := INIRec.ReadString(sSecao, 'nifBenef', EmptyStr);
          infoPgtoExt.frmTribut := INIRec.ReadInteger(sSecao, 'frmTribut', 0);

          sSecao := 'endExt' + IntToStrZero(I, zerosInfoPgto);
          infoPgtoExt.endExt.endDscLograd := INIRec.ReadString(sSecao, 'endDscLograd', EmptyStr);
          infoPgtoExt.endExt.endNrLograd  := INIRec.ReadString(sSecao, 'endNrLograd', EmptyStr);
          infoPgtoExt.endExt.endComplem   := INIRec.ReadString(sSecao, 'endComplem', EmptyStr);
          infoPgtoExt.endExt.endBairro    := INIRec.ReadString(sSecao, 'endBairro', EmptyStr);
          infoPgtoExt.endExt.endCidade    := INIRec.ReadString(sSecao, 'endCidade', EmptyStr);
          infoPgtoExt.endExt.endEstado    := INIRec.ReadString(sSecao, 'endEstado', EmptyStr);
          infoPgtoExt.endExt.endCodPostal := INIRec.ReadString(sSecao, 'endCodPostal', EmptyStr);
          infoPgtoExt.endExt.telef        := INIRec.ReadString(sSecao, 'telef', EmptyStr);
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // infoIRComplem.dtLaudo não é obrigatória, verificar se exite o primeiro reg em alguma das filhas
        sSecao := 'infoDep' + IntToStrZero(I, 2) + '001';
        sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
        begin
          sSecao := 'infoIRCR' + IntToStrZero(I, 2) + '01';
          sFim   := INIRec.ReadString(sSecao, 'tpCR', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
          begin
            sSecao := 'planSaude' + IntToStrZero(I, 2) + '01';
            sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
            begin
              sSecao := 'infoReembMed' + IntToStrZero(I, 2) + '01';
              sFim   := INIRec.ReadString(sSecao, 'indOrgReemb', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;
            end;
          end;
        end;

        // de 0 até 1
        sSecao := 'infoIRComplem' + IntToStrZero(I, 2);

        with ideBenef.infoIRComplem do
        begin
          dtLaudo := StringToDateTime(INIRec.ReadString(sSecao, 'dtLaudo', '0'));

          J := 1;
          while true do
          begin
            // de 001 até 999
            sSecao := 'infoDep' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

            if (sFim = 'FIM') then
              break;

            with InfoDep.New do
            begin
              cpfDep := sFim;
              dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
              nome := INIRec.ReadString(sSecao, 'nome', '');
              depIRRF := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'depIRRF', ''));
              tpDep := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '01'));
              descrDep := INIRec.ReadString(sSecao, 'descrDep', '');
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'infoIRCR' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpCR', 'FIM');

            if (sFim = 'FIM') then
              break;

            with infoIRCR.New do
            begin
              tpCR := sFim;

              K := 1;
              while true do
              begin
                // de 001 até 999
                sSecao := 'dedDepen' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                       IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'tpRend', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with DedDepen.New do
                begin
                  tpRend := StrToIntDef(sFim,1);
                  cpfDep := INIRec.ReadString(sSecao, 'cpfDep', '');
                  vlrDedDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedDep', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'penAlim' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                      IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpRend', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with PenAlim.New do
                begin
                  tpRend := StrToIntDef(sFim,1);
                  cpfDep := INIRec.ReadString(sSecao, 'cpfDep', '');
                  vlrDedPenAlim := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedPenAlim', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'previdCompl' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                      IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpPrev', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with PrevidCompl.New do
                begin
                  tpPrev := eSStrTotpTpPrev(Ok, sFim);
                  cnpjEntidPC := INIRec.ReadString(sSecao, 'cnpjEntidPC', '');
                  vlrDedPC := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedPC', ''), 0);
                  vlrPatrocFunp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPatrocFunp', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 50
                sSecao := 'infoProcRet' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                      IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpProcRet', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with InfoProcRet.New do
                begin
                  tpProcRet := eSStrTotpTpProcRet(Ok, sFim);
                  nrProcRet := INIRec.ReadString(sSecao, 'nrProcRet', '');
                  codSusp := INIRec.ReadString(sSecao, 'codSusp', '');

                  L := 1;
                  while true do
                  begin
                    // de 01 até 02
                    sSecao := 'infoValores' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                   IntToStrZero(K, 2) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'indApuracao', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with InfoValores.New do
                    begin
                      indApuracao := eSStrToIndApuracao(Ok, sFim);
                      vlrNRetido := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRetido', ''), 0);
                      vlrDepJud := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepJud', ''), 0);
                      vlrCmpAnoCal := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoCal', ''), 0);
                      vlrCmpAnoAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoAnt', ''), 0);
                      vlrRendSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendSusp', ''), 0);

                      M := 1;
                      while true do
                      begin
                        // de 01 até 25
                        sSecao := 'dedSusp' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                       IntToStrZero(K, 2) + IntToStrZero(L, 2) +
                                       IntToStrZero(M, 2);

                        sFim   := INIRec.ReadString(sSecao, 'indTpDeducao', 'FIM');

                        if (sFim = 'FIM') then
                          break;

                        with DedSusp.New do
                        begin
                          indTpDeducao := eSStrTotpIndTpDeducao(Ok, sFim);
                          vlrDedSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedSusp', ''), 0);
                          cnpjEntidPC := INIRec.ReadString(sSecao, 'cnpjEntidPC', '');
                          vlrPatrocFunp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPatrocFunp', ''), 0);

                          N := 1;
                          while true do
                          begin
                            // de 01 até 99
                            sSecao := 'benefPen' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                           IntToStrZero(K, 2) + IntToStrZero(L, 2) +
                                           IntToStrZero(M, 2) + IntToStrZero(N, 2);

                            sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                            if (sFim = 'FIM') then
                              break;

                            with BenefPen.New do
                            begin
                              cpfDep := sFim;
                              vlrDepenSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepenSusp', ''), 0);
                            end;

                            Inc(N);
                          end;
                        end;

                        Inc(M);
                      end;
                    end;

                    Inc(L);
                  end;
                end;

                Inc(K);
              end;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'planSaude' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

            if (sFim = 'FIM') then
              break;

            with PlanSaude.New do
            begin
              cnpjOper := sFim;
              regANS := INIRec.ReadString(sSecao, 'regANS', '');
              vlrSaudeTit := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSaudeTit', ''), 0);

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'infoDepSau' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                      IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                if (sFim = 'FIM') then
                  break;

                with InfoDepSau.New do
                begin
                  cpfDep := sFim;
                  vlrSaudeDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSaudeDep', ''), 0);
                end;

                Inc(K);
              end;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'infoReembMed' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'indOrgReemb', 'FIM');

            if (sFim = 'FIM') then
              break;

            with InfoReembMed.New do
            begin
              indOrgReemb := sFim;
              cnpjOper := INIRec.ReadString(sSecao, 'cnpjOper', '');
              regANS := INIRec.ReadString(sSecao, 'regANS', '');

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'detReembTit' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                          IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                if (sFim = 'FIM') then
                  break;

                DetReembTit.Clear;
                with DetReembTit.New do
                begin
                  tpInsc := eSStrToTpInscricao(Ok, sFim);
                  nrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
                  vlrReemb := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReemb', ''), 0);
                  vlrReembAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReembAnt', ''), 0);
                end;

                Inc(K);
              end;

              K := 1;
              while true do
              begin
                // de 01 até 99
                sSecao := 'infoReembDep' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                           IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'cpfBenef', 'FIM');

                if (sFim = 'FIM') then
                  break;

                InfoReembDep.Clear;
                with InfoReembDep.New do
                begin
                  cpfBenef := sFim;

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detReembDep' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                                              IntToStrZero(K, 2) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                    if (sFim = 'FIM') then
                      break;

                    DetReembDep.Clear;
                    with DetReembDep.New do
                    begin
                      tpInsc := eSStrToTpInscricao(Ok, sFim);
                      nrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');
                      vlrReemb := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReemb', ''), 0);
                      vlrReembAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReembAnt', ''), 0);
                    end;

                    Inc(L);
                  end;
                end;

                Inc(K);
              end;
            end;

            Inc(J);
          end;

          Inc(I);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
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

{ TinfoIRComplem }

constructor TinfoIRComplem.Create;
begin
  inherited Create;

  FinfoDep := nil;
  FinfoIRCR := nil;
  FplanSaude := nil;
  FinfoReembMed := nil;
end;

destructor TinfoIRComplem.Destroy;
begin
  if infoDepInst() then
    FreeAndNil(FinfoDep);

  if infoIRCRInst() then
    FreeAndNil(FinfoIRCR);

  if planSaudeInst() then
    FreeAndNil(FplanSaude);

  if infoReembMedInst() then
    FreeAndNil(FinfoReembMed);

  inherited;
end;

function TinfoIRComplem.getInfoDep: TinfoDepCollection;
begin
  if not Assigned(FinfoDep) then
    FinfoDep := TinfoDepCollection.Create;
  Result := FinfoDep;
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

function TinfoIRComplem.infoDepInst: boolean;
begin
  Result := Assigned(FinfoDep);
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

{ TinfoDepCollection }

function TinfoDepCollection.GetItem(Index: Integer): TinfoDepCollectionItem;
begin
  Result := TinfoDepCollectionItem(inherited Items[Index]);
end;

procedure TinfoDepCollection.SetItem(Index: Integer; const Value: TinfoDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfoDepCollection.New: TinfoDepCollectionItem;
begin
  Result := TinfoDepCollectionItem.Create;
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

end.
