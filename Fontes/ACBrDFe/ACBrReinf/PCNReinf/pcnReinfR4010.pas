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

unit pcnReinfR4010;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.DateTime,
  ACBrDFeConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  {Classes específicas deste evento}

  TR4010Collection = class;
  TR4010CollectionItem = class;
  TevtRetPF = class;
  TideEstab = class;
  TideBenef = class;
  TideDepCollection = class;
  TideDepCollectionItem = class;
  TidePgtoCollection = class;
  TidePgtoCollectionItem = class;
  TinfoPgtoCollection = class;
  TinfoPgtoCollectionItem = class;
  TdetDedCollection = class;
  TdetDedCollectionItem = class;
  TbenefPenCollection = class;
  TbenefPenCollectionItem = class;
  TrendIsentoCollection = class;
  TrendIsentoCollectionItem = class;
  TinfoProcRetCollection = class;
  TinfoProcRetCollectionItem = class;
  TdedSuspCollection = class;
  TdedSuspCollectionItem = class;
  TbenefPenSuspCollection = class;
  TbenefPenSuspCollectionItem = class;
  TinfoRRA = class;
  TdespProcJud = class;
  TideAdvCollection = class;
  TideAdvCollectionItem = class;
  TinfoProcJud = class;
  TinfoPgtoExt = class;
  TendExt = class;
  TideOpSaudeCollection = class;
  TideOpSaudeCollectionItem = class;
  TinfoReembCollection = class;
  TinfoReembCollectionItem = class;
  TinfoDependPlCollection = class;
  TinfoDependPlCollectionItem = class;

  { TR4010Collection }
  TR4010Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR4010CollectionItem;
    procedure SetItem(Index: Integer; Value: TR4010CollectionItem);
  public
    function Add: TR4010CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR4010CollectionItem;

    property Items[Index: Integer]: TR4010CollectionItem read GetItem write SetItem; default;
  end;

  { TR4010CollectionItem }
  TR4010CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtRetPF: TevtRetPF;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRetPF: TevtRetPF read FevtRetPF write FevtRetPF;
  end;

  { TevtRetPF }
  TevtRetPF = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoComplContri: TinfoComplContri;
    FideEstab: TideEstab;

    {Geradores específicos desta classe}
    procedure GerarideEstab;
    procedure GerarideBenef;
    procedure GerarideDep(Lista: TideDepCollection);
    procedure GeraridePgto(Lista: TidePgtoCollection);
    procedure GerarinfoPgto(Lista: TinfoPgtoCollection);
    procedure GerardetDed(Lista: TdetDedCollection);
    procedure GerarbenefPen(Lista: TbenefPenCollection);
    procedure GerarrendIsento(Lista: TrendIsentoCollection);
    procedure GerarinfoProcRet(Lista: TinfoProcRetCollection);
    procedure GerardedSusp(Lista: TdedSuspCollection);
    procedure GerarbenefPenSusp(Lista: TbenefPenSuspCollection);
    procedure GerarinfoRRA(item: TinfoRRA);
    procedure GerardespProcJud(item: TdespProcJud);
    procedure GerarideAdv(Lista: TideAdvCollection);
    procedure GerarinfoProcJud(item: TinfoProcJud);
    procedure GerarinfoPgtoExt(item: TinfoPgtoExt);
    procedure GerarendExt(item: TendExt);
    procedure GerarideOpSaude(Lista: TideOpSaudeCollection);
    procedure GerarinfoReemb(Lista: TinfoReembCollection);
    procedure GerarinfoDependPl(Lista: TinfoDependPlCollection);
    procedure GerarinfoReembDep(Lista: TinfoReembCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideEstab: TideEstab read FideEstab write FideEstab;
  end;

  { TideEstab }
  TideEstab = class(TObject)
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;

    FideBenef: TideBenef;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property ideBenef: TideBenef read FideBenef write FideBenef;
  end;

  { TideBenef }
  TideBenef = class(TObject)
  private
    FcpfBenef: string;
    FnmBenef: string;
    FideEvtAdic: string;
    FideDep: TideDepCollection;
    FidePgto: TidePgtoCollection;
    FideOpSaude: TideOpSaudeCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cpfBenef: string read FcpfBenef write FcpfBenef;
    property nmBenef: string read FnmBenef write FnmBenef;
    property ideEvtAdic: string read FideEvtAdic write FideEvtAdic;
    property ideDep: TideDepCollection read FideDep write FideDep;
    property idePgto: TidePgtoCollection read FidePgto write FidePgto;
    property ideOpSaude: TideOpSaudeCollection read FideOpSaude write FideOpSaude;
  end;

  { TideDepCollection }
  TideDepCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideDepCollectionItem;
    procedure SetItem(Index: Integer; Value: TideDepCollectionItem);
  public
    function Add: TideDepCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideDepCollectionItem;

    property Items[Index: Integer]: TideDepCollectionItem read GetItem write SetItem; default;
  end;

  { TideDepCollectionItem }
  TideDepCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FrelDep: TtpDependente;
    FdescrDep: string;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property relDep: TtpDependente read FrelDep write FrelDep;
    property descrDep: string read FdescrDep write FdescrDep;
  end;

  { TidePgtoCollection }
  TidePgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TidePgtoCollectionItem;
    procedure SetItem(Index: Integer; Value: TidePgtoCollectionItem);
  public
    function Add: TidePgtoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TidePgtoCollectionItem;

    property Items[Index: Integer]: TidePgtoCollectionItem read GetItem write SetItem; default;
  end;

  { TidePgtoCollectionItem }
  TidePgtoCollectionItem = class(TObject)
  private
    FnatRend: string;
    Fobserv: string;
    FinfoPgto: TinfoPgtoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property natRend: string read FnatRend write FnatRend;
    property observ: string read Fobserv write Fobserv;
    property infoPgto: TinfoPgtoCollection read FinfoPgto write FinfoPgto;
  end;

  { TinfoPgtoCollection }
  TinfoPgtoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoPgtoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoPgtoCollectionItem);
  public
    function Add: TinfoPgtoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoPgtoCollectionItem;

    property Items[Index: Integer]: TinfoPgtoCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoPgtoCollectionItem }
  TinfoPgtoCollectionItem = class(TObject)
  private
    FdtFG: TDateTime;
    FcompFP: TDateTime;
    FindDecTerc: string;
    FvlrRendBruto: double;
    FvlrRendTrib: double;
    FvlrIR: double;
    FindRRA: string;
    FindFciScp: string;
    FnrInscFciScp: string;
    FpercSCP: double;
    FindJud: string;
    FpaisResidExt: string;
    FdtEscrCont: TDateTime;
    Fobserv: string;
    FdetDed: TdetDedCollection;
    FrendIsento: TrendIsentoCollection;
    FinfoProcRet: TinfoProcRetCollection;
    FinfoRRA: TinfoRRA;
    FinfoProcJud: TinfoProcJud;
    FinfoPgtoExt: TinfoPgtoExt;
  public
    constructor Create;
    destructor Destroy; override;

    property dtFG: TDateTime read FdtFG write FdtFG;
    property compFP: TDateTime read FcompFP write FcompFP;
    property indDecTerc: string read FindDecTerc write FindDecTerc;
    property vlrRendBruto: double read FvlrRendBruto write FvlrRendBruto;
    property vlrRendTrib: double read FvlrRendTrib write FvlrRendTrib;
    property vlrIR: double read FvlrIR write FvlrIR;
    property indRRA: string read FindRRA write FindRRA;
    property indFciScp: string read FindFciScp write FindFciScp;
    property nrInscFciScp: string read FnrInscFciScp write FnrInscFciScp;
    property percSCP: double read FpercSCP write FpercSCP;
    property indJud: string read FindJud write FindJud;
    property paisResidExt: string read FpaisResidExt write FpaisResidExt;
    property dtEscrCont: TDateTime read FdtEscrCont write FdtEscrCont;
    property observ: string read Fobserv write Fobserv;
    property detDed: TdetDedCollection read FdetDed write FdetDed;
    property rendIsento: TrendIsentoCollection read FrendIsento write FrendIsento;
    property infoProcRet: TinfoProcRetCollection read FinfoProcRet write FinfoProcRet;
    property infoRRA: TinfoRRA read FinfoRRA write FinfoRRA;
    property infoProcJud: TinfoProcJud read FinfoProcJud write FinfoProcJud;
    property infoPgtoExt: TinfoPgtoExt read FinfoPgtoExt write FinfoPgtoExt;
  end;

  { TdetDedCollection }
  TdetDedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetDedCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetDedCollectionItem);
  public
    function Add: TdetDedCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetDedCollectionItem;

    property Items[Index: Integer]: TdetDedCollectionItem read GetItem write SetItem; default;
  end;

  { TdetDedCollectionItem }
  TdetDedCollectionItem = class(TObject)
  private
    FindTpDeducao: TindTpDeducao;
    FvlrDeducao: double;
    FinfoEntid: string;
    FnrInscPrevComp: string;
    FvlrPatrocFunp: double;
    FbenefPen: TbenefPenCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indTpDeducao: TindTpDeducao read FindTpDeducao write FindTpDeducao;
    property vlrDeducao: double read FvlrDeducao write FvlrDeducao;
    property infoEntid: string read FinfoEntid write FinfoEntid;
    property nrInscPrevComp: string read FnrInscPrevComp write FnrInscPrevComp;
    property vlrPatrocFunp: double read FvlrPatrocFunp write FvlrPatrocFunp;
    property benefPen: TbenefPenCollection read FbenefPen write FbenefPen;
  end;

  { TbenefPenCollection }
  TbenefPenCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbenefPenCollectionItem;
    procedure SetItem(Index: Integer; Value: TbenefPenCollectionItem);
  public
    function Add: TbenefPenCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbenefPenCollectionItem;

    property Items[Index: Integer]: TbenefPenCollectionItem read GetItem write SetItem; default;
  end;

  { TbenefPenCollectionItem }
  TbenefPenCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrDepen: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDepen: double read FvlrDepen write FvlrDepen;
  end;

  { TrendIsentoCollection }
  TrendIsentoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TrendIsentoCollectionItem;
    procedure SetItem(Index: Integer; Value: TrendIsentoCollectionItem);
  public
    function Add: TrendIsentoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TrendIsentoCollectionItem;

    property Items[Index: Integer]: TrendIsentoCollectionItem read GetItem write SetItem; default;
  end;

  { TrendIsentoCollectionItem }
  TrendIsentoCollectionItem = class(TObject)
  private
    FtpIsencao: TtpIsencao;
    FvlrIsento: double;
    FdescRendimento: string;
    FdtLaudo: TDateTime;
  public
    property tpIsencao: TtpIsencao read FtpIsencao write FtpIsencao;
    property vlrIsento: double read FvlrIsento write FvlrIsento;
    property descRendimento: string read FdescRendimento write FdescRendimento;
    property dtLaudo: TDateTime read FdtLaudo write FdtLaudo;
  end;

  { TinfoProcRetCollection }
  TinfoProcRetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetCollectionItem);
  public
    function Add: TinfoProcRetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcRetCollectionItem;

    property Items[Index: Integer]: TinfoProcRetCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoProcRetCollectionItem }
  TinfoProcRetCollectionItem = class(TObject)
  private
    FtpProcRet: TtpProc;
    FnrProcRet: string;
    FcodSusp: string;
    FvlrNRetido: double;
    FvlrDepJud: double;
    FvlrCmpAnoCal: double;
    FvlrCmpAnoAnt: double;
    FvlrRendSusp: double;
    FdedSusp: TdedSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpProcRet: TtpProc read FtpProcRet write FtpProcRet;
    property nrProcRet: string read FnrProcRet write FnrProcRet;
    property codSusp: string read FcodSusp write FcodSusp;
    property vlrNRetido: double read FvlrNRetido write FvlrNRetido;
    property vlrDepJud: double read FvlrDepJud write FvlrDepJud;
    property vlrCmpAnoCal: double read FvlrCmpAnoCal write FvlrCmpAnoCal;
    property vlrCmpAnoAnt: double read FvlrCmpAnoAnt write FvlrCmpAnoAnt;
    property vlrRendSusp: double read FvlrRendSusp write FvlrRendSusp;
    property dedSusp: TdedSuspCollection read FdedSusp write FdedSusp;
  end;

  { TdedSuspCollection }
  TdedSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdedSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TdedSuspCollectionItem);
  public
    function Add: TdedSuspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdedSuspCollectionItem;

    property Items[Index: Integer]: TdedSuspCollectionItem read GetItem write SetItem; default;
  end;

  { TdedSuspCollectionItem }
  TdedSuspCollectionItem = class(TObject)
  private
    FindTpDeducao: TindTpDeducao;
    FvlrDedSusp: double;
    FbenefPenSusp: TbenefPenSuspCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indTpDeducao: TindTpDeducao read FindTpDeducao write FindTpDeducao;
    property vlrDedSusp: double read FvlrDedSusp write FvlrDedSusp;
    property benefPenSusp: TbenefPenSuspCollection read FbenefPenSusp write FbenefPenSusp;
  end;

  { TbenefPenSuspCollection }
  TbenefPenSuspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TbenefPenSuspCollectionItem;
    procedure SetItem(Index: Integer; Value: TbenefPenSuspCollectionItem);
  public
    function Add: TbenefPenSuspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TbenefPenSuspCollectionItem;

    property Items[Index: Integer]: TbenefPenSuspCollectionItem read GetItem write SetItem; default;
  end;

  { TbenefPenSuspCollectionItem }
  TbenefPenSuspCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrDepenSusp: double;
  public
    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrDepenSusp: double read FvlrDepenSusp write FvlrDepenSusp;
  end;

  { TinfoRRA }
  TinfoRRA = class(TObject)
  private
    FtpProcRRA: TtpProc;
    FnrProcRRA: string;
    FindOrigRec: TindOrigemRecursos;
    FdescRRA: string;
    FqtdMesesRRA: double;
    FcnpjOrigRecurso: string;
    FdespProcJud: TdespProcJud;
  public
    constructor Create;
    destructor Destroy; override;

    property tpProcRRA: TtpProc read FtpProcRRA write FtpProcRRA;
    property nrProcRRA: string read FnrProcRRA write FnrProcRRA;
    property indOrigRec: TindOrigemRecursos read FindOrigRec write FindOrigRec;
    property descRRA: string read FdescRRA write FdescRRA;
    property qtdMesesRRA: double read FqtdMesesRRA write FqtdMesesRRA;
    property cnpjOrigRecurso: string read FcnpjOrigRecurso write FcnpjOrigRecurso;
    property despProcJud: TdespProcJud read FdespProcJud write FdespProcJud;
  end;

  { TdespProcJud }
  TdespProcJud = class(TObject)
  private
    FvlrDespCustas: double;
    FvlrDespAdvogados: double;
    FideAdv: TideAdvCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property vlrDespCustas: double read FvlrDespCustas write FvlrDespCustas;
    property vlrDespAdvogados: double read FvlrDespAdvogados write FvlrDespAdvogados;
    property ideAdv: TideAdvCollection read FideAdv write FideAdv;
  end;

  { TideAdvCollection }
  TideAdvCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideAdvCollectionItem;
    procedure SetItem(Index: Integer; Value: TideAdvCollectionItem);
  public
    function Add: TideAdvCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideAdvCollectionItem;

    property Items[Index: Integer]: TideAdvCollectionItem read GetItem write SetItem; default;
  end;

  { TideAdvCollectionItem }
  TideAdvCollectionItem = class(TObject)
  private
    FtpInscAdv: TtpInsc;
    FnrInscAdv: string;
    FvlrAdv: double;
  public
    property tpInscAdv: TtpInsc read FtpInscAdv write FtpInscAdv;
    property nrInscAdv: string read FnrInscAdv write FnrInscAdv;
    property vlrAdv: double read FvlrAdv write FvlrAdv;
  end;

  { TinfoProcJud }
  TinfoProcJud = class(TObject)
  private
    FnrProc: string;
    FindOrigRec: TindOrigemRecursos;
    FcnpjOrigRecurso: string;
    Fdesc: string;
    FdespProcJud: TdespProcJud;
  public
    constructor Create;
    destructor Destroy; override;

    property nrProc: string read FnrProc write FnrProc;
    property indOrigRec: TindOrigemRecursos read FindOrigRec write FindOrigRec;
    property cnpjOrigRecurso: string read FcnpjOrigRecurso write FcnpjOrigRecurso;
    property desc: string read Fdesc write Fdesc;
    property despProcJud: TdespProcJud read FdespProcJud write FdespProcJud;
  end;

  { TinfoPgtoExt }
  TinfoPgtoExt = class(TObject)
  private
    FindNIF: TindNIF;
    FnifBenef: string;
    FfrmTribut: string;
    FendExt: TendExt;
  public
    constructor Create;
    destructor Destroy; override;

    property indNIF: TindNIF read FindNIF write FindNIF;
    property nifBenef: string read FnifBenef write FnifBenef;
    property frmTribut: string read FfrmTribut write FfrmTribut;
    property endExt: TendExt read FendExt write FendExt;
  end;

  { TendExt }
  TendExt = class(TObject)
  private
    FdscLograd: string;
    FnrLograd: string;
    Fcomplem: string;
    Fbairro: string;
    Fcidade: string;
    Festado: string;
    FcodPostal: string;
    Ftelef: string;
  public
    property dscLograd: string read FdscLograd write FdscLograd;
    property nrLograd: string read FnrLograd write FnrLograd;
    property complem: string read Fcomplem write Fcomplem;
    property bairro: string read Fbairro write Fbairro;
    property cidade: string read Fcidade write Fcidade;
    property estado: string read Festado write Festado;
    property codPostal: string read FcodPostal write FcodPostal;
    property telef: string read Ftelef write Ftelef;
  end;

  { TideOpSaudeCollection }
  TideOpSaudeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideOpSaudeCollectionItem;
    procedure SetItem(Index: Integer; Value: TideOpSaudeCollectionItem);
  public
    function Add: TideOpSaudeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideOpSaudeCollectionItem;

    property Items[Index: Integer]: TideOpSaudeCollectionItem read GetItem write SetItem; default;
  end;

  { TideOpSaudeCollectionItem }
  TideOpSaudeCollectionItem = class(TObject)
  private
    FnrInsc: string;
    FregANS: string;
    FvlrSaude: double;
    FinfoReemb: TinfoReembCollection;
    FinfoDependPl: TinfoDependPlCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property nrInsc: string read FnrInsc write FnrInsc;
    property regANS: string read FregANS write FregANS;
    property vlrSaude: double read FvlrSaude write FvlrSaude;
    property infoReemb: TinfoReembCollection read FinfoReemb write FinfoReemb;
    property infoDependPl: TinfoDependPlCollection read FinfoDependPl write FinfoDependPl;
  end;

  { TinfoReembCollection }
  TinfoReembCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoReembCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoReembCollectionItem);
  public
    function Add: TinfoReembCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoReembCollectionItem;

    property Items[Index: Integer]: TinfoReembCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoReembCollectionItem }
  TinfoReembCollectionItem = class(TObject)
  private
    FtpInsc: TtpInsc;
    FnrInsc: string;
    FvlrReemb: double;
    FvlrReembAnt: double;
  public
    property tpInsc: TtpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vlrReemb: double read FvlrReemb write FvlrReemb;
    property vlrReembAnt: double read FvlrReembAnt write FvlrReembAnt;
  end;

  { TinfoDependPlCollection }
  TinfoDependPlCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoDependPlCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoDependPlCollectionItem);
  public
    function Add: TinfoDependPlCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoDependPlCollectionItem;

    property Items[Index: Integer]: TinfoDependPlCollectionItem read GetItem write SetItem; default;
  end;

  { TinfoDependPlCollectionItem }
  TinfoDependPlCollectionItem = class(TObject)
  private
    FcpfDep: string;
    FvlrSaude: double;
    FinfoReembDep: TinfoReembCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cpfDep: string read FcpfDep write FcpfDep;
    property vlrSaude: double read FvlrSaude write FvlrSaude;
    property infoReembDep: TinfoReembCollection read FinfoReembDep write FinfoReembDep;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR4010Collection }

function TR4010Collection.Add: TR4010CollectionItem;
begin
  Result := Self.New;
end;

function TR4010Collection.GetItem(Index: Integer): TR4010CollectionItem;
begin
  Result := TR4010CollectionItem(inherited Items[Index]);
end;

function TR4010Collection.New: TR4010CollectionItem;
begin
  Result := TR4010CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR4010Collection.SetItem(Index: Integer; Value: TR4010CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR4010CollectionItem }

constructor TR4010CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teR4010;
  FevtRetPF := TevtRetPF.Create(AOwner);
end;

destructor TR4010CollectionItem.Destroy;
begin
  inherited;

  FevtRetPF.Free;
end;

{ TevtRetPF }

constructor TevtRetPF.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri       := TideContri.Create;
  FIdeEvento       := TIdeEvento2.Create;
  FinfoComplContri := TinfoComplContri.Create;
  FideEstab        := TideEstab.Create;
end;

destructor TevtRetPF.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoComplContri.Free;
  FideEstab.Free;

  inherited;
end;

{ TideEstab }

constructor TideEstab.Create;
begin
  FideBenef := TideBenef.Create;
end;

destructor TideEstab.Destroy;
begin
  FideBenef.Free;

  inherited;
end;

{ TideBenef }

constructor TideBenef.Create;
begin
  FideDep := TideDepCollection.Create;
  FidePgto := TidePgtoCollection.Create;
  FideOpSaude := TideOpSaudeCollection.Create;
end;

destructor TideBenef.Destroy;
begin
  FideDep.Free;
  FidePgto.Free;
  FideOpSaude.Free;

  inherited;
end;

procedure TevtRetPF.GerarideEstab;
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.ideEstab.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.ideEstab.nrInscEstab);

  GerarideBenef;

  Gerador.wGrupo('/ideEstab');
end;

procedure TevtRetPF.GerarideBenef;
begin
  Gerador.wGrupo('ideBenef');

  with Self.ideEstab do
  begin
    Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, ideBenef.cpfBenef);
    Gerador.wCampo(tcStr, '', 'nmBenef',   1, 70, 0, ideBenef.nmBenef);
    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
      Gerador.wCampo(tcStr, '', 'ideEvtAdic',1,  8, 0, ideBenef.ideEvtAdic);

    GerarideDep(ideBenef.ideDep);
    GeraridePgto(ideBenef.idePgto);
    GerarideOpSaude(ideBenef.ideOpSaude);
  end;

  Gerador.wGrupo('/ideBenef');
end;

procedure TevtRetPF.GerarideDep(Lista: TideDepCollection);
var
  item: TideDepCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideDep');

    Gerador.wCampo(tcStr, '', 'cpfDep',   1,  11, 1, item.cpfDep);
    Gerador.wCampo(tcStr, '', 'relDep',   1,   1, 1, TpDependenteToStr(item.relDep));
    Gerador.wCampo(tcStr, '', 'descrDep', 1,  30, 0, item.descrDep);

    Gerador.wGrupo('/ideDep');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'ideDep', 'Lista de Dependentes', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtRetPF.GeraridePgto(Lista: TidePgtoCollection);
var
  item: TidePgtoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('idePgto');

    Gerador.wCampo(tcStr, '', 'natRend',  5,   5, 1, item.natRend);
    Gerador.wCampo(tcStr, '', 'observ',   1,  30, 0, item.observ);

    GerarinfoPgto(item.infoPgto);

    Gerador.wGrupo('/idePgto');
  end;

  if Lista.Count > 100 then
    Gerador.wAlerta('', 'idePgto', 'Identificação do rendimento', ERR_MSG_MAIOR_MAXIMO + '100');
end;

procedure TevtRetPF.GerarinfoPgto(Lista: TinfoPgtoCollection);
var
  item: TinfoPgtoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoPgto');
    Gerador.wCampo(tcDat, '', 'dtFG', 10,  10, 1, item.dtFG);

    if(item.compFP > 0)then
    begin
      if item.indDecTerc = 'S' then
        Gerador.wCampo(tcStr, '', 'compFP', 4,  4, 0, FormatDateTime('yyyy',item.compFP))
      else
        Gerador.wCampo(tcStr, '', 'compFP', 7,  7, 0, FormatDateTime('yyyy-mm',item.compFP));
    end;
    Gerador.wCampo(tcStr, '', 'indDecTerc',     1,   1,  0, item.indDecTerc);
    Gerador.wCampo(tcDe2, '', 'vlrRendBruto',   1,  14,  1, item.vlrRendBruto);
    Gerador.wCampo(tcDe2, '', 'vlrRendTrib',    1,  14,  0, item.vlrRendTrib);
    Gerador.wCampo(tcDe2, '', 'vlrIR',          1,  14,  0, item.vlrIR);
    Gerador.wCampo(tcStr, '', 'indRRA',         1,   1,  0, item.indRRA);
    Gerador.wCampo(tcStr, '', 'indFciScp',      1,   1,  0, item.indFciScp);
    Gerador.wCampo(tcStr, '', 'nrInscFciScp',  14,  14,  0, item.nrInscFciScp);
    Gerador.wCampo(tcDe1, '', 'percSCP',        1,   4,  0, item.percSCP);
    Gerador.wCampo(tcStr, '', 'indJud',         1,   1,  0, item.indJud);
    Gerador.wCampo(tcStr, '', 'paisResidExt',   1,   3,  0, item.paisResidExt);

    if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
    begin
      Gerador.wCampo(tcDat, '', 'dtEscrCont', 1,  10, 0, item.dtEscrCont);
      Gerador.wCampo(tcStr, '', 'observ',     1, 200, 0, item.observ);
    end;

    GerardetDed(item.detDed);
    GerarrendIsento(item.rendIsento);
    GerarinfoProcRet(item.infoProcRet);
    GerarinfoRRA(item.infoRRA);
    GerarinfoProcJud(item.infoProcJud);
    GerarinfoPgtoExt(item.infoPgtoExt);

    Gerador.wGrupo('/infoPgto');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoPgto',
                    'Informações relativas ao rendimento pago/creditado',
                    ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtRetPF.GerardetDed(Lista: TdetDedCollection);
var
  item: TdetDedCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('detDed');
    Gerador.wCampo(tcStr, '', 'indTpDeducao',     1,   2,  1, indTpDeducaoToStr(item.indTpDeducao));
    Gerador.wCampo(tcDe2, '', 'vlrDeducao',       1,  14,  1, item.vlrDeducao);
    Gerador.wCampo(tcStr, '', 'infoEntid',        1,   1,  0, item.infoEntid);
    Gerador.wCampo(tcStr, '', 'nrInscPrevComp',  14,  14,  0, item.nrInscPrevComp);
    Gerador.wCampo(tcDe2, '', 'vlrPatrocFunp',    1,  14,  0, item.vlrPatrocFunp);

    GerarbenefPen(item.benefPen);

    Gerador.wGrupo('/detDed');
  end;

  if Lista.Count > 25 then
    Gerador.wAlerta('', 'detDed',
                    'Informações relativas às deduções do rendimento tributável',
                    ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TevtRetPF.GerarbenefPen(Lista: TbenefPenCollection);
var
  item: TbenefPenCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('benefPen');
    Gerador.wCampo(tcStr, '', 'cpfDep',   11,  11,  1, item.cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrDepen',  1,  14,  1, item.vlrDepen);
    Gerador.wGrupo('/benefPen');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'benefPen',
                    'Informação dos dependentes e beneficiários da pensão alimentícia',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarrendIsento(Lista: TrendIsentoCollection);
var
  item: TrendIsentoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('rendIsento');
    Gerador.wCampo(tcStr, '', 'tpIsencao',       1,    2, 1, tpIsencaoToStr(item.tpIsencao));
    Gerador.wCampo(tcDe2, '', 'vlrIsento',       1,   14, 1, item.vlrIsento);
    Gerador.wCampo(tcStr, '', 'descRendimento',  1,  100, 0, item.descRendimento);

    // Informação exclusiva se {tpIsencao} = [6]
    if item.tpIsencao = tiPensaoAposentadoria then
      Gerador.wCampo(tcDat, '', 'dtLaudo',        10,   10, 0, item.dtLaudo);

    Gerador.wGrupo('/rendIsento');
  end;

  if Lista.Count > 25 then
    Gerador.wAlerta('', 'rendIsento', 'Rendimentos isentos ou não tributáveis', ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TevtRetPF.GerarinfoProcRet(Lista: TinfoProcRetCollection);
var
  item: TinfoProcRetCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcRet');
    Gerador.wCampo(tcStr, '', 'tpProcRet',     1,    1, 1, TpProcToStr(item.tpProcRet));
    Gerador.wCampo(tcStr, '', 'nrProcRet',     1,   21, 1, item.nrProcRet);
    Gerador.wCampo(tcStr, '', 'codSusp',       1,   14, 0, item.codSusp);
    Gerador.wCampo(tcDe2, '', 'vlrNRetido',    1,   14, 0, item.vlrNRetido);
    Gerador.wCampo(tcDe2, '', 'vlrDepJud',     1,   14, 0, item.vlrDepJud);
    Gerador.wCampo(tcDe2, '', 'vlrCmpAnoCal',  1,   14, 0, item.vlrCmpAnoCal);
    Gerador.wCampo(tcDe2, '', 'vlrCmpAnoAnt',  1,   14, 0, item.vlrCmpAnoAnt);
    Gerador.wCampo(tcDe2, '', 'vlrRendSusp',   1,   14, 0, item.vlrRendSusp);

    GerardedSusp(item.dedSusp);

    Gerador.wGrupo('/infoProcRet');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcRet',
                    'Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais',
                    ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TevtRetPF.GerardedSusp(Lista: TdedSuspCollection);
var
  item: TdedSuspCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('dedSusp');
    Gerador.wCampo(tcStr, '', 'indTpDeducao',  1,    1, 1, indTpDeducaoToStr(item.indTpDeducao));
    Gerador.wCampo(tcDe2, '', 'vlrDedSusp',    1,   14, 0, item.vlrDedSusp);

    GerarbenefPenSusp(item.benefPenSusp);

    Gerador.wGrupo('/dedSusp');
  end;

  if Lista.Count > 25 then
    Gerador.wAlerta('', 'dedSusp',
                    'Detalhamento das deduções com exigibilidade suspensa',
                    ERR_MSG_MAIOR_MAXIMO + '25');
end;

procedure TevtRetPF.GerarbenefPenSusp(Lista: TbenefPenSuspCollection);
var
  item: TbenefPenSuspCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('benefPen');
    Gerador.wCampo(tcStr, '', 'cpfDep',       11,  11,  1, item.cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrDepenSusp',  1,  14,  1, item.vlrDepenSusp);
    Gerador.wGrupo('/benefPen');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'benefPen',
                    'Informação das deduções suspensas por dependentes e beneficiários da pensão alimentícia',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarinfoRRA(item: TinfoRRA);
begin
  if item.qtdMesesRRA > 0 then
  begin
    Gerador.wGrupo('infoRRA');
    Gerador.wCampo(tcStr, '', 'tpProcRRA',        1,   1,  1, TpProcToStr(item.tpProcRRA));
    Gerador.wCampo(tcStr, '', 'nrProcRRA',        1,  21,  0, item.nrProcRRA);
    Gerador.wCampo(tcStr, '', 'indOrigRec',       1,   1,  1, indOrigemRecursosToStr(item.indOrigRec));
    Gerador.wCampo(tcStr, '', 'descRRA',          1,  50,  0, item.descRRA);
    Gerador.wCampo(tcDe1, '', 'qtdMesesRRA',      1,   4,  1, item.qtdMesesRRA);
    Gerador.wCampo(tcStr, '', 'cnpjOrigRecurso', 14,  14,  0, item.cnpjOrigRecurso);

    GerardespProcJud(item.despProcJud);

    Gerador.wGrupo('/infoRRA');
  end;  
end;

procedure TevtRetPF.GerardespProcJud(item: TdespProcJud);
begin
  if item.vlrDespCustas > 0 then
  begin
    Gerador.wGrupo('despProcJud');
    Gerador.wCampo(tcDe2, '', 'vlrDespCustas',     1,  14,  1, item.vlrDespCustas);
    Gerador.wCampo(tcDe2, '', 'vlrDespAdvogados',  1,  14,  1, item.vlrDespAdvogados);

    GerarideAdv(item.ideAdv);

    Gerador.wGrupo('/despProcJud');
  end;
end;

procedure TevtRetPF.GerarideAdv(Lista: TideAdvCollection);
var
  item: TideAdvCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideAdv');
    Gerador.wCampo(tcStr, '', 'tpInscAdv',  1,   1,  1, TpInscricaoToStr(item.tpInscAdv));
    Gerador.wCampo(tcStr, '', 'nrInscAdv', 11,  14,  1, item.nrInscAdv);
    Gerador.wCampo(tcDe2, '', 'vlrAdv',     1,  14,  0, item.vlrAdv);
    Gerador.wGrupo('/ideAdv');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'ideAdv', 'Identificação do advogado', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarinfoProcJud(item: TinfoProcJud);
begin
  if item.nrProc <> '' then
  begin
    Gerador.wGrupo('infoProcJud');
    Gerador.wCampo(tcStr, '', 'nrProc',            1,  21,  1, item.nrProc);
    Gerador.wCampo(tcStr, '', 'indOrigRec',        1,   1,  1, indOrigemRecursosToStr(item.indOrigRec));
    Gerador.wCampo(tcStr, '', 'cnpjOrigRecurso',  14,  14,  0, item.cnpjOrigRecurso);
    Gerador.wCampo(tcStr, '', 'desc',              1,  50,  0, item.desc);

    GerardespProcJud(item.despProcJud);

    Gerador.wGrupo('/infoProcJud');
  end;
end;

procedure TevtRetPF.GerarinfoPgtoExt(item: TinfoPgtoExt);
begin
  if item.frmTribut <> '' then
  begin
    Gerador.wGrupo('infoPgtoExt');
    Gerador.wCampo(tcStr, '', 'indNIF',     1,   1,  1, indNIFToStr(item.indNIF));
    Gerador.wCampo(tcStr, '', 'nifBenef',   1,  30,  0, item.nifBenef);
    Gerador.wCampo(tcStr, '', 'frmTribut',  2,   2,  1, item.frmTribut);

    GerarendExt(item.endExt);

    Gerador.wGrupo('/infoPgtoExt');
  end;
end;

procedure TevtRetPF.GerarendExt(item: TendExt);
begin
  // Nenhum campo é obrigatório, validados Logradouro e codigo postal
  if (item.dscLograd <> '') or
     (item.codPostal <> '') then
  begin
    Gerador.wGrupo('endExt');
    Gerador.wCampo(tcStr, '', 'dscLograd',    1,  80,  0, item.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',     1,  10,  0, item.nrLograd);
    Gerador.wCampo(tcStr, '', 'complem',      1,  30,  0, item.complem);
    Gerador.wCampo(tcStr, '', 'bairro',       1,  60,  0, item.bairro);
    Gerador.wCampo(tcStr, '', 'cidade',       1,  40,  0, item.cidade);
    Gerador.wCampo(tcStr, '', 'estado',       1,  40,  0, item.estado);
    Gerador.wCampo(tcStr, '', 'codPostal',    1,  12,  0, item.codPostal);
    Gerador.wCampo(tcStr, '', 'telef',        1,  15,  0, item.telef);
    Gerador.wGrupo('/endExt');
  end;
end;

procedure TevtRetPF.GerarideOpSaude(Lista: TideOpSaudeCollection);
var
  item: TideOpSaudeCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideOpSaude');
    Gerador.wCampo(tcStr, '', 'nrInsc',   14,  14,  1, item.nrInsc);
    Gerador.wCampo(tcStr, '', 'regANS',    1,   6,  0, item.regANS);
    Gerador.wCampo(tcDe2, '', 'vlrSaude',  1,  14,  1, item.vlrSaude);

    GerarinfoReemb(item.infoReemb);
    GerarinfoDependPl(item.infoDependPl);

    Gerador.wGrupo('/ideOpSaude');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'ideOpSaude',
                    'Identificação da operadora do plano privado coletivo empresarial de assistência à saúde',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarinfoReemb(Lista: TinfoReembCollection);
var
  item: TinfoReembCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoReemb');
    Gerador.wCampo(tcStr, '', 'tpInsc',       1,   1,  1, TpInscricaoToStr(item.tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',      11,  14,  1, item.nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',     1,  14,  0, item.vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt',  1,  14,  0, item.vlrReembAnt);
    Gerador.wGrupo('/infoReemb');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'infoReemb',
                    'Informação de reembolso do titular do plano de saúde coletivo empresarial',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarinfoDependPl(Lista: TinfoDependPlCollection);
var
  item: TinfoDependPlCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoDependPl');
    Gerador.wCampo(tcStr, '', 'cpfDep',    11,  11,  1, item.cpfDep);
    Gerador.wCampo(tcDe2, '', 'vlrSaude',  1,  14,  1, item.vlrSaude);

    GerarinfoReembDep(item.infoReembDep);

    Gerador.wGrupo('/infoDependPl');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'infoDependPl',
                    'Informações de dependente do plano de saúde coletivo empresarial',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TevtRetPF.GerarinfoReembDep(Lista: TinfoReembCollection);
var
  item: TinfoReembCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoReembDep');
    Gerador.wCampo(tcStr, '', 'tpInsc',       1,   1,  1, TpInscricaoToStr(item.tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',      11,  14,  1, item.nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrReemb',     1,  14,  0, item.vlrReemb);
    Gerador.wCampo(tcDe2, '', 'vlrReembAnt',  1,  14,  0, item.vlrReembAnt);
    Gerador.wGrupo('/infoReembDep');
  end;

  if Lista.Count > 99 then
    Gerador.wAlerta('', 'infoReemb',
                    'Informação de reembolso do dependente do plano de saúde coletivo empresarial',
                    ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TevtRetPF.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evt4010PagtoBeneficiarioPF');
    Gerador.wGrupo('evtRetPF id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarideEstab;

    Gerador.wGrupo('/evtRetPF');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtRetPF.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, I2, I3, I4, I5: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRetPF';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.TpInsc := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoComplContri';
      ideContri.infoComplContri.natJur := INIRec.ReadString(sSecao, 'natJur', EmptyStr);

      sSecao := 'ideEstab';
      ideEstab.tpInscEstab := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      ideEstab.nrInscEstab := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);

      with ideEstab do
      begin
        sSecao := 'ideBenef';
        ideBenef.cpfBenef := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);
        ideBenef.nmBenef  := INIRec.ReadString(sSecao, 'nmBenef', EmptyStr);
        if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
          ideBenef.ideEvtAdic := INIRec.ReadString(sSecao, 'ideEvtAdic', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 999
          sSecao := 'ideDep' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideBenef.ideDep.New do
          begin
            cpfDep   := sFim;
            relDep   := StrToTpDependente(ok,INIRec.ReadString(sSecao, 'relDep', ''));
            descrDep := INIRec.ReadString(sSecao, 'descrDep', '');
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 01 até 100
          sSecao := 'idePgto' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'natRend', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideBenef.idePgto.New do
          begin
            natRend := sFim;
            observ  := INIRec.ReadString(sSecao, 'observ', '');

            I2 := 1;
            while true do
            begin
              // de 1 até 999
              sSecao := 'infoPgto' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtFG', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoPgto.New do
              begin
                dtFG := StringToDateTime(sFim);

                sFim := INIRec.ReadString(sSecao, 'compFP', '');

                if sFim <> '' then
                begin
                  if Length(sFim) = 4 then
                    sFim := '01/' + sFim;

                  sFim := '01/' + sFim;

                  compFP := StringToDateTime(sFim);
                end;

                indDecTerc   := INIRec.ReadString(sSecao, 'indDecTerc', '');
                vlrRendBruto := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendBruto', ''), 0);
                vlrRendTrib  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendTrib', ''), 0);
                vlrIR        := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIR', ''), 0);
                indRRA       := INIRec.ReadString(sSecao, 'indRRA', '');
                indFciScp    := INIRec.ReadString(sSecao, 'indFciScp', '');
                nrInscFciScp := INIRec.ReadString(sSecao, 'nrInscFciScp', '');
                percSCP      := StringToFloatDef(INIRec.ReadString(sSecao, 'percSCP', ''), 0);
                indJud       := INIRec.ReadString(sSecao, 'indJud', '');
                paisResidExt := INIRec.ReadString(sSecao, 'paisResidExt', '');

                if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF >= v2_01_02 then
                begin
                  dtEscrCont := StringToDateTime(INIRec.ReadString(sSecao, 'dtEscrCont', ''));
                  observ     := INIRec.ReadString(sSecao, 'observ', '');
                end;

                I3 := 1;
                while true do
                begin
                  // de 1 até 25
                  sSecao := 'detDed' + IntToStrZero(I, 3) +
                                       IntToStrZero(I2, 3) +
                                       IntToStrZero(I3, 3);

                  sFim := INIRec.ReadString(sSecao, 'indTpDeducao', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with detDed.New do
                  begin
                    indTpDeducao   := StrToIndTpDeducao(Ok, sFim);
                    vlrDeducao     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDeducao', ''), 0);
                    infoEntid      := INIRec.ReadString(sSecao, 'infoEntid', '');
                    nrInscPrevComp := INIRec.ReadString(sSecao, 'nrInscPrevComp', '');
                    vlrPatrocFunp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPatrocFunp', ''), 0);

                    I4 := 1;
                    while true do
                    begin
                      // de 1 até 25
                      sSecao := 'benefPen' + IntToStrZero(I, 3) +
                                             IntToStrZero(I2, 3) +
                                             IntToStrZero(I3, 3) +
                                             IntToStrZero(I4, 3);

                      sFim := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                      if (sFim = 'FIM') or (Length(sFim) <= 0) then
                        break;

                      with benefPen.New do
                      begin
                        cpfDep   := sFim;
                        vlrDepen := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepen', ''), 0);
                      end;

                      Inc(I4);
                    end;
                  end;

                  Inc(I3);
                end;

                I3 := 1;
                while true do
                begin
                  // de 1 até 25
                  sSecao := 'rendIsento' + IntToStrZero(I, 3) +
                                           IntToStrZero(I2, 3) +
                                           IntToStrZero(I3, 3);

                  sFim := INIRec.ReadString(sSecao, 'tpIsencao', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with rendIsento.New do
                  begin
                    tpIsencao      := StrTotpIsencao(Ok, sFim);
                    vlrIsento      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIsento', ''), 0);
                    descRendimento := INIRec.ReadString(sSecao, 'descRendimento', '');
                    dtLaudo        := StringToDateTime(INIRec.ReadString(sSecao, 'dtLaudo', ''));
                  end;

                  Inc(I3);
                end;

                I3 := 1;
                while true do
                begin
                  // de 1 até 50
                  sSecao := 'infoProcRet' + IntToStrZero(I, 3) +
                                            IntToStrZero(I2, 3) +
                                            IntToStrZero(I3, 3);

                  sFim := INIRec.ReadString(sSecao, 'tpProcRet', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoProcRet.New do
                  begin
                    tpProcRet    := StrToTpProc(Ok, sFim);
                    nrProcRet    := INIRec.ReadString(sSecao, 'nrProcRet', '');
                    codSusp      := INIRec.ReadString(sSecao, 'codSusp', '');
                    vlrNRetido   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRetido', ''), 0);
                    vlrDepJud    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepJud', ''), 0);
                    vlrCmpAnoCal := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoCal', ''), 0);
                    vlrCmpAnoAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCmpAnoAnt', ''), 0);
                    vlrRendSusp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendSusp', ''), 0);

                    I4 := 1;
                    while true do
                    begin
                      // de 1 até 25
                      sSecao := 'dedSusp' + IntToStrZero(I, 3) +
                                            IntToStrZero(I2, 3) +
                                            IntToStrZero(I3, 3) +
                                            IntToStrZero(I4, 3);

                      sFim := INIRec.ReadString(sSecao, 'indTpDeducao', 'FIM');

                      if (sFim = 'FIM') or (Length(sFim) <= 0) then
                        break;

                      with dedSusp.New do
                      begin
                        indTpDeducao := StrToindTpDeducao(Ok, sFim);
                        vlrDedSusp   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDedSusp', ''), 0);

                        I5 := 1;
                        while true do
                        begin
                          // de 1 até 99
                          sSecao := 'benefPenSusp' + IntToStrZero(I, 3) +
                                                     IntToStrZero(I2, 3) +
                                                     IntToStrZero(I3, 3) +
                                                     IntToStrZero(I4, 3) +
                                                     IntToStrZero(I5, 3);

                          sFim := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                          if (sFim = 'FIM') or (Length(sFim) <= 0) then
                            break;

                          with benefPenSusp.New do
                          begin
                            cpfDep       := sFim;
                            vlrDepenSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepenSusp', ''), 0);
                          end;

                          Inc(I5);
                        end;
                      end;

                      Inc(I4);
                    end;
                  end;

                  Inc(I3);
                end;

                sSecao := 'infoRRA' + IntToStrZero(I, 3) +
                                      IntToStrZero(I2, 3);

                with infoRRA do
                begin
                  tpProcRRA       := StrToTpProc(Ok, INIRec.ReadString(sSecao, 'tpProcRRA', ''));
                  nrProcRRA       := INIRec.ReadString(sSecao, 'nrProcRRA', '');
                  indOrigRec      := StrToindOrigemRecursos(Ok, INIRec.ReadString(sSecao, 'indOrigRec', ''));
                  descRRA         := INIRec.ReadString(sSecao, 'descRRA', '');
                  qtdMesesRRA     := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdMesesRRA', ''), 0);
                  cnpjOrigRecurso := INIRec.ReadString(sSecao, 'cnpjOrigRecurso', '');

                  sSecao := 'infoRRA.despProcJud' + IntToStrZero(I, 3) +
                                                    IntToStrZero(I2, 3);

                  with despProcJud do
                  begin
                    vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''),0);
                    vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''),0);

                    I3 := 1;
                    while true do
                    begin
                      // de 1 até 99
                      sSecao := 'infoRRA.ideAdv' + IntToStrZero(I, 3) +
                                                   IntToStrZero(I2, 3) +
                                                   IntToStrZero(I3, 3);

                      sFim := INIRec.ReadString(sSecao, 'tpInscAdv', '');

                      if (sFim = 'FIM') or (Length(sFim) <= 0) then
                        break;

                      with ideAdv.New do
                      begin
                        tpInscAdv := StrToTpInscricao(ok,sFim);
                        nrInscAdv := INIRec.ReadString(sSecao, 'nrInscAdv', '');
                        vlrAdv    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdv', ''), 0);
                      end;

                      Inc(I3);
                    end;
                  end;
                end;

                sSecao := 'infoProcJud' + IntToStrZero(I, 3) +
                                          IntToStrZero(I2, 3);

                with infoProcJud do
                begin
                  nrProc          := INIRec.ReadString(sSecao, 'nrProc', '');
                  indOrigRec      := StrToindOrigemRecursos(Ok, INIRec.ReadString(sSecao, 'indOrigRec', ''));
                  cnpjOrigRecurso := INIRec.ReadString(sSecao, 'cnpjOrigRecurso', '');
                  desc            := INIRec.ReadString(sSecao, 'desc', '');

                  sSecao := 'infoProcJud.despProcJud' + IntToStrZero(I, 3) +
                                                        IntToStrZero(I2, 3);

                  with despProcJud do
                  begin
                    vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''),0);
                    vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''),0);

                    I3 := 1;
                    while true do
                    begin
                      // de 1 até 99
                      sSecao := 'infoProcJud.ideAdv' + IntToStrZero(I, 3) +
                                                       IntToStrZero(I2, 3) +
                                                       IntToStrZero(I3, 3);

                      sFim := INIRec.ReadString(sSecao, 'tpInscAdv', 'FIM');

                      if (sFim = 'FIM') or (Length(sFim) <= 0) then
                        break;

                      with ideAdv.New do
                      begin
                        tpInscAdv := StrToTpInscricao(ok,sFim);
                        nrInscAdv := INIRec.ReadString(sSecao, 'nrInscAdv', '');
                        vlrAdv    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdv', ''), 0);
                      end;

                      Inc(I3);
                    end;
                  end;
                end;

                sSecao := 'infoPgtoExt' + IntToStrZero(I, 3) +
                                          IntToStrZero(I2, 3);

                with infoPgtoExt do
                begin
                  indNIF    := StrToindNIF(Ok, INIRec.ReadString(sSecao, 'indNIF', ''));
                  nifBenef  := INIRec.ReadString(sSecao, 'nifBenef', '');
                  frmTribut := INIRec.ReadString(sSecao, 'frmTribut', '');

                  sSecao := 'endExt' + IntToStrZero(I, 3) +
                                       IntToStrZero(I2, 3);

                  with endExt do
                  begin
                    dscLograd := INIRec.ReadString(sSecao, 'dscLograd', '');
                    nrLograd := INIRec.ReadString(sSecao, 'nrLograd', '');
                    complem := INIRec.ReadString(sSecao, 'complem', '');
                    bairro := INIRec.ReadString(sSecao, 'bairro', '');
                    cidade := INIRec.ReadString(sSecao, 'cidade', '');
                    estado := INIRec.ReadString(sSecao, 'estado', '');
                    codPostal := INIRec.ReadString(sSecao, 'codPostal', '');
                    telef := INIRec.ReadString(sSecao, 'telef', '');
                  end;
                end;
              end;
              Inc(I2);
            end;
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'ideOpSaude' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideBenef.ideOpSaude.New do
          begin
            nrInsc   := sFim;
            regANS   := INIRec.ReadString(sSecao, 'regANS', '');
            vlrSaude := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSaude', ''), 0);

            I2 := 1;
            while true do
            begin
              // de 1 até 99
              sSecao := 'infoReemb' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoReemb.New do
              begin
                tpInsc      := StrToTpInscricao(Ok, sFim);
                nrInsc      := INIRec.ReadString(sSecao, 'nrInsc', '');
                vlrReemb    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReemb', ''), 0);
                vlrReembAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReembAnt', ''), 0);
              end;

              Inc(I2);
            end;

            I2 := 1;
            while true do
            begin
              // de 1 até 99
              sSecao := 'infoDependPl' + IntToStrZero(I, 3) + IntToStrZero(I2, 3);
              sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoDependPl.New do
              begin
                cpfDep   := sFim;
                vlrSaude := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrSaude', ''), 0);

                I3 := 1;
                while true do
                begin
                  // de 1 até 99
                  sSecao := 'infoReembDep' + IntToStrZero(I, 3) +
                                             IntToStrZero(I2, 3) +
                                             IntToStrZero(I3, 3);
                  sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoReembDep.New do
                  begin
                    tpInsc      := StrToTpInscricao(Ok, sFim);
                    nrInsc      := INIRec.ReadString(sSecao, 'nrInsc', '');
                    vlrReemb    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReemb', ''), 0);
                    vlrReembAnt := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrReembAnt', ''), 0);
                  end;

                  Inc(I3);
                end;
              end;

              Inc(I2);
            end;
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

{ TideDepCollection }

function TideDepCollection.Add: TideDepCollectionItem;
begin
  Result := Self.New;
end;

function TideDepCollection.GetItem(Index: Integer): TideDepCollectionItem;
begin
  Result := TideDepCollectionItem(inherited Items[Index]);
end;

function TideDepCollection.New: TideDepCollectionItem;
begin
  Result := TideDepCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideDepCollection.SetItem(Index: Integer;
  Value: TideDepCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TidePgtoCollection }

function TidePgtoCollection.Add: TidePgtoCollectionItem;
begin
  Result := Self.New;
end;

function TidePgtoCollection.GetItem(
  Index: Integer): TidePgtoCollectionItem;
begin
  Result := TidePgtoCollectionItem(inherited Items[Index]);
end;

function TidePgtoCollection.New: TidePgtoCollectionItem;
begin
  Result := TidePgtoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TidePgtoCollection.SetItem(Index: Integer;
  Value: TidePgtoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TidePgtoCollectionItem }

constructor TidePgtoCollectionItem.Create;
begin
  FinfoPgto := TinfoPgtoCollection.Create;
end;

destructor TidePgtoCollectionItem.Destroy;
begin
  FinfoPgto.Free;

  inherited;
end;

{ TinfoPgtoCollection }

function TinfoPgtoCollection.Add: TinfoPgtoCollectionItem;
begin
  Result := Self.New;
end;

function TinfoPgtoCollection.GetItem(
  Index: Integer): TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem(inherited Items[Index]);
end;

function TinfoPgtoCollection.New: TinfoPgtoCollectionItem;
begin
  Result := TinfoPgtoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoPgtoCollection.SetItem(Index: Integer;
  Value: TinfoPgtoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoPgtoCollectionItem }

constructor TinfoPgtoCollectionItem.Create;
begin
  FdetDed := TdetDedCollection.Create;
  FrendIsento := TrendIsentoCollection.Create;
  FinfoProcRet := TinfoProcRetCollection.Create;
  FinfoRRA := TinfoRRA.Create;
  FinfoProcJud := TinfoProcJud.Create;
  FinfoPgtoExt := TinfoPgtoExt.Create;
end;

destructor TinfoPgtoCollectionItem.Destroy;
begin
  FdetDed.Free;
  FrendIsento.Free;
  FinfoProcRet.Free;
  FinfoRRA.Free;
  FinfoProcJud.Free;
  FinfoPgtoExt.Free;

  inherited;
end;

{ TdetDedCollection }

function TdetDedCollection.Add: TdetDedCollectionItem;
begin
  Result := Self.New;
end;

function TdetDedCollection.GetItem(Index: Integer): TdetDedCollectionItem;
begin
  Result := TdetDedCollectionItem(inherited Items[Index]);
end;

function TdetDedCollection.New: TdetDedCollectionItem;
begin
  Result := TdetDedCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdetDedCollection.SetItem(Index: Integer;
  Value: TdetDedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TdetDedCollectionItem }

constructor TdetDedCollectionItem.Create;
begin
  FbenefPen := TbenefPenCollection.Create;
end;

destructor TdetDedCollectionItem.Destroy;
begin
  FbenefPen.Free;

  inherited;
end;

{ TbenefPenCollection }

function TbenefPenCollection.Add: TbenefPenCollectionItem;
begin
  Result := Self.New;
end;

function TbenefPenCollection.GetItem(
  Index: Integer): TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem(inherited Items[Index]);
end;

function TbenefPenCollection.New: TbenefPenCollectionItem;
begin
  Result := TbenefPenCollectionItem.Create;
  Self.Add(Result);
end;

procedure TbenefPenCollection.SetItem(Index: Integer;
  Value: TbenefPenCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TrendIsentoCollection }

function TrendIsentoCollection.Add: TrendIsentoCollectionItem;
begin
  Result := Self.New;
end;

function TrendIsentoCollection.GetItem(
  Index: Integer): TrendIsentoCollectionItem;
begin
  Result := TrendIsentoCollectionItem(inherited Items[Index]);
end;

function TrendIsentoCollection.New: TrendIsentoCollectionItem;
begin
  Result := TrendIsentoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TrendIsentoCollection.SetItem(Index: Integer;
  Value: TrendIsentoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcRetCollection }

function TinfoProcRetCollection.Add: TinfoProcRetCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcRetCollection.GetItem(
  Index: Integer): TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem(inherited Items[Index]);
end;

function TinfoProcRetCollection.New: TinfoProcRetCollectionItem;
begin
  Result := TinfoProcRetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcRetCollection.SetItem(Index: Integer;
  Value: TinfoProcRetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcRetCollectionItem }

constructor TinfoProcRetCollectionItem.Create;
begin
  FdedSusp := TdedSuspCollection.Create;
end;

destructor TinfoProcRetCollectionItem.Destroy;
begin
  FdedSusp.Free;

  inherited;
end;

{ TdedSuspCollection }

function TdedSuspCollection.Add: TdedSuspCollectionItem;
begin
  Result := Self.New;
end;

function TdedSuspCollection.GetItem(
  Index: Integer): TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem(inherited Items[Index]);
end;

function TdedSuspCollection.New: TdedSuspCollectionItem;
begin
  Result := TdedSuspCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdedSuspCollection.SetItem(Index: Integer;
  Value: TdedSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TdedSuspCollectionItem }

constructor TdedSuspCollectionItem.Create;
begin
  FbenefPenSusp := TbenefPenSuspCollection.Create;
end;

destructor TdedSuspCollectionItem.Destroy;
begin
  FbenefPenSusp.Free;

  inherited;
end;

{ TbenefPenSuspCollection }

function TbenefPenSuspCollection.Add: TbenefPenSuspCollectionItem;
begin
  Result := Self.New;
end;

function TbenefPenSuspCollection.GetItem(
  Index: Integer): TbenefPenSuspCollectionItem;
begin
  Result := TbenefPenSuspCollectionItem(inherited Items[Index]);
end;

function TbenefPenSuspCollection.New: TbenefPenSuspCollectionItem;
begin
  Result := TbenefPenSuspCollectionItem.Create;
  Self.Add(Result);
end;

procedure TbenefPenSuspCollection.SetItem(Index: Integer;
  Value: TbenefPenSuspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoRRA }

constructor TinfoRRA.Create;
begin
  FdespProcJud := TdespProcJud.Create;
end;

destructor TinfoRRA.Destroy;
begin
  FdespProcJud.Free;

  inherited;
end;

{ TdespProcJud }

constructor TdespProcJud.Create;
begin
  FideAdv := TideAdvCollection.Create;
end;

destructor TdespProcJud.Destroy;
begin
  FideAdv.Free;

  inherited;
end;

{ TideAdvCollection }

function TideAdvCollection.Add: TideAdvCollectionItem;
begin
  Result := Self.New;
end;

function TideAdvCollection.GetItem(Index: Integer): TideAdvCollectionItem;
begin
  Result := TideAdvCollectionItem(inherited Items[Index]);
end;

function TideAdvCollection.New: TideAdvCollectionItem;
begin
  Result := TideAdvCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideAdvCollection.SetItem(Index: Integer;
  Value: TideAdvCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcJud }

constructor TinfoProcJud.Create;
begin
  FdespProcJud := TdespProcJud.Create;
end;

destructor TinfoProcJud.Destroy;
begin
  FdespProcJud.Free;

  inherited;
end;

{ TinfoPgtoExt }

constructor TinfoPgtoExt.Create;
begin
  FendExt := TendExt.Create;
end;

destructor TinfoPgtoExt.Destroy;
begin
  FendExt.Free;

  inherited;
end;

{ TideOpSaudeCollection }

function TideOpSaudeCollection.Add: TideOpSaudeCollectionItem;
begin
  Result := Self.New;
end;

function TideOpSaudeCollection.GetItem(
  Index: Integer): TideOpSaudeCollectionItem;
begin
  Result := TideOpSaudeCollectionItem(inherited Items[Index]);
end;

function TideOpSaudeCollection.New: TideOpSaudeCollectionItem;
begin
  Result := TideOpSaudeCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideOpSaudeCollection.SetItem(Index: Integer;
  Value: TideOpSaudeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TideOpSaudeCollectionItem }

constructor TideOpSaudeCollectionItem.Create;
begin
  FinfoReemb := TinfoReembCollection.Create;
  FinfoDependPl := TinfoDependPlCollection.Create;
end;

destructor TideOpSaudeCollectionItem.Destroy;
begin
  FinfoReemb.Free;
  FinfoDependPl.Free;

  inherited;
end;

{ TinfoReembCollection }

function TinfoReembCollection.Add: TinfoReembCollectionItem;
begin
  Result := Self.New;
end;

function TinfoReembCollection.GetItem(
  Index: Integer): TinfoReembCollectionItem;
begin
  Result := TinfoReembCollectionItem(inherited Items[Index]);
end;

function TinfoReembCollection.New: TinfoReembCollectionItem;
begin
  Result := TinfoReembCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoReembCollection.SetItem(Index: Integer;
  Value: TinfoReembCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoDependPlCollection }

function TinfoDependPlCollection.Add: TinfoDependPlCollectionItem;
begin
  Result := Self.New;
end;

function TinfoDependPlCollection.GetItem(
  Index: Integer): TinfoDependPlCollectionItem;
begin
  Result := TinfoDependPlCollectionItem(inherited Items[Index]);
end;

function TinfoDependPlCollection.New: TinfoDependPlCollectionItem;
begin
  Result := TinfoDependPlCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoDependPlCollection.SetItem(Index: Integer;
  Value: TinfoDependPlCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoDependPlCollectionItem }

constructor TinfoDependPlCollectionItem.Create;
begin
  FinfoReembDep := TinfoReembCollection.Create;
end;

destructor TinfoDependPlCollectionItem.Destroy;
begin
  FinfoReembDep.Free;

  inherited;
end;

end.

