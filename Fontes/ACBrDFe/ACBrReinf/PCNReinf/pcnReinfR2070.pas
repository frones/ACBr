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

unit pcnReinfR2070;

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
  TR2070Collection = class;
  TR2070CollectionItem = class;
  TevtPgtosDivs = class;

  {Classes específicas deste evento}
  TideBenef = class;
  TinfoResidExt = class;
  TinfoEnder = class;
  TinfoFiscal = class;
  TinfoMolestia = class;
  TideEstabCollection = class;
  TideEstabCollectionItem = class;
  TpgtoPFCollection = class;
  TpgtoPFCollectionItem = class;
  TdetDeducaoCollection = class;
  TdetDeducaoCollectionItem = class;
  TrendIsentoCollection = class;
  TrendIsentoCollectionItem = class;
  TdetCompetCollection = class;
  TdetCompetCollectionItem = class;
  TcompJud = class;
  TinfoRRACollection = class;
  TinfoRRACollectionItem = class;
  TdespProcJud = class;
  TideAdvogadoCollection = class;
  TideAdvogadoCollectionItem = class;
  TinfoProcJudCollection = class;
  TinfoProcJudCollectionItem = class;
  TorigemRecursos = class;
  TdepJudicial = class;
  TpgtoPJCollection = class;
  TpgtoPJCollectionItem = class;
  TpgtoResidExt = class;

  TR2070Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2070CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2070CollectionItem);
  public
    function Add: TR2070CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2070CollectionItem;

    property Items[Index: Integer]: TR2070CollectionItem read GetItem write SetItem; default;
  end;

  TR2070CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtPgtosDivs: TevtPgtosDivs;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtPgtosDivs: TevtPgtosDivs read FevtPgtosDivs write FevtPgtosDivs;
  end;

  TevtPgtosDivs = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FideBenef: TideBenef;

    {Geradores específicos desta classe}
    procedure GerarideBenef;
    procedure GerarinfoResidExt;
    procedure GerarinfoEnder;
    procedure GerarinfoFiscal;
    procedure GerarinfoMolestia;
    procedure GerarideEstab(Lista: TideEstabCollection);
    procedure GerarpgtoPF(Lista: TpgtoPFCollection);
    procedure GerardetDeducao(Lista: TdetDeducaoCollection);
    procedure GerarrendIsento(Lista: TrendIsentoCollection);
    procedure GerardetCompet(Lista: TdetCompetCollection);
    procedure GerarcompJud(Item: TCompJud);
    procedure GerarinfoRRA(Lista: TinfoRRACollection);
    procedure GerardespProcJud(Item: TdespProcJud);
    procedure GerarideAdvogado(Lista: TideAdvogadoCollection);
    procedure GerarinfoProcJud(Lista: TinfoProcJudCollection);
    procedure GerarorigemRecursos(Item: TorigemRecursos);
    procedure GerardepJudicial(Item: TdepJudicial);
    procedure GerarpgtoPJ(Lista: TpgtoPJCollection);
    procedure GerarpgtoResidExt(Item: TpgtoResidExt);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property ideBenef: TideBenef read FideBenef write FideBenef;
  end;

  { TideBenef }
  TideBenef = class(TObject)
  private
    FcodPgto: String;
    FtpInscBenef: TtpInsc;
    FnrInscBenef: String;
    FnmRazaoBenef: String;
    FinfoResidExt: TinfoResidExt;
    FinfoMolestia: TinfoMolestia;
    FideEstab: TideEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property codPgto: String read FcodPgto write FcodPgto;
    property tpInscBenef: TtpInsc read FtpInscBenef write FtpInscBenef;
    property nrInscBenef: String read FnrInscBenef write FnrInscBenef;
    property nmRazaoBenef: String read FnmRazaoBenef write FnmRazaoBenef;
    property infoResidExt: TinfoResidExt read FinfoResidExt write FinfoResidExt;
    property infoMolestia: TinfoMolestia read FinfoMolestia write FinfoMolestia;
    property ideEstab: TideEstabCollection read FideEstab write FideEstab;
  end;

  { TinfoResidExt }
  TinfoResidExt = class(TObject)
  private
    FinfoEnder: TinfoEnder;
    FinfoFiscal: TinfoFiscal;
  public
    constructor Create;
    destructor Destroy; override;

    property infoEnder: TinfoEnder read FinfoEnder write FinfoEnder;
    property infoFiscal: TinfoFiscal read FinfoFiscal write FinfoFiscal;
  end;

  { TinfoEnder }
  TinfoEnder = class(TObject)
  private
    FpaisResid: String;
    FdscLograd: String;
    FnrLograd: String;
    Fcomplem: String;
    Fbairro: String;
    Fcidade: String;
    FcodPostal: String;
  public
    property paisResid: String read FpaisResid write FpaisResid;
    property dscLograd: String read FdscLograd write FdscLograd;
    property nrLograd: String read FnrLograd write FnrLograd;
    property complem: String read Fcomplem write Fcomplem;
    property bairro: String read Fbairro write Fbairro;
    property cidade: String read Fcidade write Fcidade;
    property codPostal: String read FcodPostal write FcodPostal;
  end;

  { TinfoFiscal }
  TinfoFiscal = class(TObject)
  private
    FindNIF: TindNIF;
    FnifBenef: String;
    FrelFontePagad: String;
  public
    property indNIF: TindNIF read FindNIF write FindNIF;
    property nifBenef: String read FnifBenef write FnifBenef;
    property relFontePagad: String read FrelFontePagad write FrelFontePagad;
  end;

  { TinfoMolestia }
  TinfoMolestia = class(TObject)
  private
    FdtLaudo: TDateTime;
  public
    property dtLaudo: TDateTime read FdtLaudo write FdtLaudo;
  end;

  TideEstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideEstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TideEstabCollectionItem);
  public
    function Add: TideEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideEstabCollectionItem;

    property Items[Index: Integer]: TideEstabCollectionItem read GetItem write SetItem; default;
  end;

  TideEstabCollectionItem = class(TObject)
  private
    FtpInsc: TtpInsc;
    FnrInsc: string;
    FpgtoPF: TpgtoPFCollection;
    FpgtoPJ: TpgtoPJCollection;
    FpgtoResidExt: TpgtoResidExt;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: TtpInsc read FtpInsc write FtpInsc default tiCNPJ;
    property nrInsc: string read FnrInsc write FnrInsc;
    property pgtoPF: TpgtoPFCollection read FpgtoPF write FpgtoPF;
    property pgtoPJ: TpgtoPJCollection read FpgtoPJ write FpgtoPJ;
    property pgtoResidExt: TpgtoResidExt read FpgtoResidExt write FpgtoResidExt;
  end;

  TpgtoPFCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TpgtoPFCollectionItem;
    procedure SetItem(Index: Integer; Value: TpgtoPFCollectionItem);
  public
    function Add: TpgtoPFCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TpgtoPFCollectionItem;

    property Items[Index: Integer]: TpgtoPFCollectionItem read GetItem write SetItem; default;
  end;

  TpgtoPFCollectionItem = class(TObject)
  private
    FdtPgto: TDateTime;
    FindSuspExig: TtpSimNao;
    FindDecTerceiro: TtpSimNao;
    FvlrRendTributavel: double;
    FvlrIRRF: double;
    FdetDeducao: TdetDeducaoCollection;
    FrendIsento: TrendIsentoCollection;
    FdetCompet: TdetCompetCollection;
    FcompJud: TcompJud;
    FinfoRRA: TinfoRRACollection;
    FinfoProcJud: TinfoProcJudCollection;
    FdepJudicial: TdepJudicial;
  public
    constructor Create;
    destructor Destroy; override;

    property dtPgto: TDateTime read FdtPgto write FdtPgto;
    property indSuspExig: TtpSimNao read FindSuspExig write FindSuspExig;
    property indDecTerceiro: TtpSimNao read FindDecTerceiro write FindDecTerceiro;
    property vlrRendTributavel: double read FvlrRendTributavel write FvlrRendTributavel;
    property vlrIRRF: double read FvlrIRRF write FvlrIRRF;
    property detDeducao: TdetDeducaoCollection read FdetDeducao write FdetDeducao;
    property rendIsento: TrendIsentoCollection read FrendIsento write FrendIsento;
    property detCompet: TdetCompetCollection read FdetCompet write FdetCompet;
    property compJud: TcompJud read FcompJud write FcompJud;
    property infoRRA: TinfoRRACollection read FinfoRRA write FinfoRRA;
    property infoProcJud: TinfoProcJudCollection read FinfoProcJud write FinfoProcJud;
    property depJudicial: TdepJudicial read FdepJudicial write FdepJudicial;
  end;

  TdetDeducaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetDeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetDeducaoCollectionItem);
  public
    function Add: TdetDeducaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetDeducaoCollectionItem;

    property Items[Index: Integer]: TdetDeducaoCollectionItem read GetItem write SetItem; default;
  end;

  TdetDeducaoCollectionItem = class(TObject)
  private
    FindTpDeducao: TindTpDeducao;
    FvlrDeducao: double;
  public
    property indTpDeducao: TindTpDeducao read FindTpDeducao write FindTpDeducao;
    property vlrDeducao: double read FvlrDeducao write FvlrDeducao;
  end;

  TrendIsentoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TrendIsentoCollectionItem;
    procedure SetItem(Index: Integer; Value: TrendIsentoCollectionItem);
  public
    function Add: TrendIsentoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TrendIsentoCollectionItem;

    property Items[Index: Integer]: TrendIsentoCollectionItem read GetItem write SetItem; default;
  end;

  TrendIsentoCollectionItem = class(TObject)
  private
    FtpIsencao: TtpIsencao;
    FvlrIsento: double;
    FdescRendimento: String;
  public
    property tpIsencao: TtpIsencao read FtpIsencao write FtpIsencao;
    property vlrIsento: double read FvlrIsento write FvlrIsento;
    property descRendimento: String read FdescRendimento write FdescRendimento;
  end;

  TdetCompetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetCompetCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetCompetCollectionItem);
  public
    function Add: TdetCompetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetCompetCollectionItem;

    property Items[Index: Integer]: TdetCompetCollectionItem read GetItem write SetItem; default;
  end;

  TdetCompetCollectionItem = class(TObject)
  private
    FindPerReferencia: TindPerReferencia;
    FperRefPagto: String;
    FvlrRendTributavel: double;
  public
    property indPerReferencia: TindPerReferencia read FindPerReferencia write FindPerReferencia;
    property perRefPagto: String read FperRefPagto write FperRefPagto;
    property vlrRendTributavel: double read FvlrRendTributavel write FvlrRendTributavel;
  end;

  { TcompJud }
  TcompJud = class(TObject)
  private
    FvlrCompAnoCalend: double;
    FvlrCompAnoAnt: double;
  public
    property vlrCompAnoCalend: double read FvlrCompAnoCalend write FvlrCompAnoCalend;
    property vlrCompAnoAnt: double read FvlrCompAnoAnt write FvlrCompAnoAnt;
  end;

  TinfoRRACollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoRRACollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoRRACollectionItem);
  public
    function Add: TinfoRRACollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoRRACollectionItem;

    property Items[Index: Integer]: TinfoRRACollectionItem read GetItem write SetItem; default;
  end;

  TinfoRRACollectionItem = class(TObject)
  private
    FtpProcRRA: TtpProc;
    FnrProcRRA: String;
    FcodSusp: String;
    FnatRRA: String;
    FqtdMesesRRA: Integer;
    FdespProcJud: TdespProcJud;
  public
    constructor Create;
    destructor Destroy; override;

    property tpProcRRA: TtpProc read FtpProcRRA write FtpProcRRA;
    property nrProcRRA: String read FnrProcRRA write FnrProcRRA;
    property codSusp: String read FcodSusp write FcodSusp;
    property natRRA: String read FnatRRA write FnatRRA;
    property qtdMesesRRA: Integer read FqtdMesesRRA write FqtdMesesRRA;
    property despProcJud: TdespProcJud read FdespProcJud write FdespProcJud;
  end;

  { TdespProcJud }
  TdespProcJud = class(TObject)
  private
    FvlrDespCustas: double;
    FvlrDespAdvogados: double;
    FideAdvogado: TideAdvogadoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property vlrDespCustas: double read FvlrDespCustas write FvlrDespCustas;
    property vlrDespAdvogados: double read FvlrDespAdvogados write FvlrDespAdvogados;
    property ideAdvogado: TideAdvogadoCollection read FideAdvogado write FideAdvogado;
  end;

  TideAdvogadoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideAdvogadoCollectionItem;
    procedure SetItem(Index: Integer; Value: TideAdvogadoCollectionItem);
  public
    function Add: TideAdvogadoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideAdvogadoCollectionItem;

    property Items[Index: Integer]: TideAdvogadoCollectionItem read GetItem write SetItem; default;
  end;

  TideAdvogadoCollectionItem = class(TObject)
  private
    FtpInscAdvogado: TtpInsc;
    FnrInscAdvogado: String;
    FvlrAdvogado: double;
  public
    property tpInscAdvogado: TtpInsc read FtpInscAdvogado write FtpInscAdvogado;
    property nrInscAdvogado: String read FnrInscAdvogado write FnrInscAdvogado;
    property vlrAdvogado: double read FvlrAdvogado write FvlrAdvogado;
  end;

  TinfoProcJudCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcJudCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcJudCollectionItem);
  public
    function Add: TinfoProcJudCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcJudCollectionItem;

    property Items[Index: Integer]: TinfoProcJudCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcJudCollectionItem = class(TObject)
  private
    FnrProcJud: String;
    FcodSusp: String;
    FindOrigemRecursos: TindOrigemRecursos;
    FdespProcJud: TdespProcJud;
    ForigemRecursos: TorigemRecursos;
  public
    constructor Create;
    destructor Destroy; override;

    property nrProcJud: String read FnrProcJud write FnrProcJud;
    property codSusp: String read FcodSusp write FcodSusp;
    property indOrigemRecursos: TindOrigemRecursos read FindOrigemRecursos write FindOrigemRecursos;
    property despProcJud: TdespProcJud read FdespProcJud write FdespProcJud;
    property origemRecursos: TorigemRecursos read ForigemRecursos write ForigemRecursos;
  end;

  { TorigemRecursos }
  TorigemRecursos = class(TObject)
  private
    FcnpjOrigemRecursos: String;
  public
    property cnpjOrigemRecursos: String read FcnpjOrigemRecursos write FcnpjOrigemRecursos;
  end;

  { TdepJudicial }
  TdepJudicial = class(TObject)
  private
    FvlrDepJudicial: double;
  public
    property vlrDepJudicial: double read FvlrDepJudicial write FvlrDepJudicial;
  end;

  TpgtoPJCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TpgtoPJCollectionItem;
    procedure SetItem(Index: Integer; Value: TpgtoPJCollectionItem);
  public
    function Add: TpgtoPJCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TpgtoPJCollectionItem;

    property Items[Index: Integer]: TpgtoPJCollectionItem read GetItem write SetItem; default;
  end;

  TpgtoPJCollectionItem = class(TObject)
  private
    FdtPagto: TDateTime;
    FvlrRendTributavel: double;
    FvlrRet: double;
    FinfoProcJud: TinfoProcJudCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtPagto: TDateTime read FdtPagto write FdtPagto;
    property vlrRendTributavel: double read FvlrRendTributavel write FvlrRendTributavel;
    property vlrRet: double read FvlrRet write FvlrRet;
    property infoProcJud: TinfoProcJudCollection read FinfoProcJud write FinfoProcJud;
  end;

  { TpgtoResidExt }
  TpgtoResidExt = class(TObject)
  private
    FdtPagto: TDateTime;
    FtpRendimento: String;
    FformaTributacao: String;
    FvlrPgto: double;
    FvlrRet: double;
  public
    property dtPagto: TDateTime read FdtPagto write FdtPagto;
    property tpRendimento: String read FtpRendimento write FtpRendimento;
    property formaTributacao: String read FformaTributacao write FformaTributacao;
    property vlrPgto: double read FvlrPgto write FvlrPgto;
    property vlrRet: double read FvlrRet write FvlrRet;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2070Collection }

function TR2070Collection.Add: TR2070CollectionItem;
begin
  Result := Self.New;
end;

function TR2070Collection.GetItem(Index: Integer): TR2070CollectionItem;
begin
  Result := TR2070CollectionItem(inherited Items[Index]);
end;

function TR2070Collection.New: TR2070CollectionItem;
begin
  Result := TR2070CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2070Collection.SetItem(Index: Integer; Value: TR2070CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2070CollectionItem }

constructor TR2070CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento   := teR2070;
  FevtPgtosDivs := TevtPgtosDivs.Create(AOwner);
end;

destructor TR2070CollectionItem.Destroy;
begin
  inherited;

  FevtPgtosDivs.Free;
end;

{ TevtPgtosDivs }

constructor TevtPgtosDivs.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri := TideContri.Create;
  FIdeEvento := TIdeEvento2.Create;
  FideBenef  := TideBenef.Create;
end;

destructor TevtPgtosDivs.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FideBenef.Free;

  inherited;
end;

{ TideBenef }

constructor TideBenef.Create;
begin
  FinfoResidExt := TinfoResidExt.Create;
  FinfoMolestia := TinfoMolestia.Create;
  FideEstab     := TideEstabCollection.Create;
end;

destructor TideBenef.Destroy;
begin
  FinfoResidExt.Free;
  FinfoMolestia.Free;
  FideEstab.Free;

  inherited;
end;

{ TinfoResidExt }

constructor TinfoResidExt.Create;
begin
  FinfoEnder  := TinfoEnder.Create;
  FinfoFiscal := TinfoFiscal.Create;
end;

destructor TinfoResidExt.Destroy;
begin
  FinfoEnder.Free;
  FinfoFiscal.Free;

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

function TideEstabCollection.New: TideEstabCollectionItem;
begin
  Result := TideEstabCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideEstabCollection.SetItem(Index: Integer;
  Value: TideEstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TideEstabCollectionItem }

constructor TideEstabCollectionItem.Create;
begin
  FpgtoPF       := TpgtoPFCollection.Create;
  FpgtoPJ       := TpgtoPJCollection.Create;
  FpgtoResidExt := TpgtoResidExt.Create;
end;

destructor TideEstabCollectionItem.Destroy;
begin
  FpgtoPF.Free;
  FpgtoPJ.Free;
  FpgtoResidExt.Free;

  inherited;
end;

{ TpgtoPFCollection }

function TpgtoPFCollection.Add: TpgtoPFCollectionItem;
begin
  Result := Self.New;
end;

function TpgtoPFCollection.GetItem(
  Index: Integer): TpgtoPFCollectionItem;
begin
  Result := TpgtoPFCollectionItem(inherited Items[Index]);
end;

function TpgtoPFCollection.New: TpgtoPFCollectionItem;
begin
  Result := TpgtoPFCollectionItem.Create;
  Self.Add(Result);
end;

procedure TpgtoPFCollection.SetItem(Index: Integer;
  Value: TpgtoPFCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TpgtoPFCollectionItem }

constructor TpgtoPFCollectionItem.create;
begin
  FdetDeducao  := TdetDeducaoCollection.Create;
  FrendIsento  := TrendIsentoCollection.Create;
  FdetCompet   := TdetCompetCollection.Create;
  FcompJud     := TcompJud.Create;
  FinfoRRA     := TinfoRRACollection.Create;
  FinfoProcJud := TinfoProcJudCollection.Create;
  FdepJudicial := TdepJudicial.Create;
end;

destructor TpgtoPFCollectionItem.Destroy;
begin
  FdetDeducao.Free;
  FrendIsento.Free;
  FdetCompet.Free;
  FcompJud.Free;
  FinfoRRA.Free;
  FinfoProcJud.Free;
  FdepJudicial.Free;

  inherited;
end;

{ TdetDeducaoCollection }

function TdetDeducaoCollection.Add: TdetDeducaoCollectionItem;
begin
  Result := Self.New;
end;

function TdetDeducaoCollection.GetItem(
  Index: Integer): TdetDeducaoCollectionItem;
begin
  Result := TdetDeducaoCollectionItem(inherited Items[Index]);
end;

function TdetDeducaoCollection.New: TdetDeducaoCollectionItem;
begin
  Result := TdetDeducaoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdetDeducaoCollection.SetItem(Index: Integer;
  Value: TdetDeducaoCollectionItem);
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

{ TdetCompetCollection }

function TdetCompetCollection.Add: TdetCompetCollectionItem;
begin
  Result := Self.New;
end;

function TdetCompetCollection.GetItem(
  Index: Integer): TdetCompetCollectionItem;
begin
  Result := TdetCompetCollectionItem(inherited Items[Index]);
end;

function TdetCompetCollection.New: TdetCompetCollectionItem;
begin
  Result := TdetCompetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdetCompetCollection.SetItem(Index: Integer;
  Value: TdetCompetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoRRACollection }

function TinfoRRACollection.Add: TinfoRRACollectionItem;
begin
  Result := Self.New;
end;

function TinfoRRACollection.GetItem(
  Index: Integer): TinfoRRACollectionItem;
begin
  Result := TinfoRRACollectionItem(inherited Items[Index]);
end;

function TinfoRRACollection.New: TinfoRRACollectionItem;
begin
  Result := TinfoRRACollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoRRACollection.SetItem(Index: Integer;
  Value: TinfoRRACollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoRRACollectionItem }

constructor TinfoRRACollectionItem.Create;
begin
  despProcJud := TdespProcJud.Create;
end;

destructor TinfoRRACollectionItem.Destroy;
begin
  despProcJud.Free;

  inherited;
end;

{ TdespProcJud }

constructor TdespProcJud.Create;
begin
  FideAdvogado := TideAdvogadoCollection.Create;
end;

destructor TdespProcJud.Destroy;
begin
  FideAdvogado.Free;

  inherited;
end;

{ TideAdvogadoCollection }

function TideAdvogadoCollection.Add: TideAdvogadoCollectionItem;
begin
  Result := Self.New;
end;

function TideAdvogadoCollection.GetItem(
  Index: Integer): TideAdvogadoCollectionItem;
begin
  Result := TideAdvogadoCollectionItem(inherited Items[Index]);
end;

function TideAdvogadoCollection.New: TideAdvogadoCollectionItem;
begin
  Result := TideAdvogadoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TideAdvogadoCollection.SetItem(Index: Integer;
  Value: TideAdvogadoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcJudCollection }

function TinfoProcJudCollection.Add: TinfoProcJudCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcJudCollection.GetItem(
  Index: Integer): TinfoProcJudCollectionItem;
begin
  Result := TinfoProcJudCollectionItem(inherited Items[Index]);
end;

function TinfoProcJudCollection.New: TinfoProcJudCollectionItem;
begin
  Result := TinfoProcJudCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcJudCollection.SetItem(Index: Integer;
  Value: TinfoProcJudCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcJudCollectionItem }

constructor TinfoProcJudCollectionItem.Create;
begin
  FdespProcJud    := TdespProcJud.Create;
  ForigemRecursos := TorigemRecursos.Create;
end;

destructor TinfoProcJudCollectionItem.Destroy;
begin
  FdespProcJud.Free;
  ForigemRecursos.Free;

  inherited;
end;

{ TpgtoPJCollection }

function TpgtoPJCollection.Add: TpgtoPJCollectionItem;
begin
  Result := Self.New;
end;

function TpgtoPJCollection.GetItem(
  Index: Integer): TpgtoPJCollectionItem;
begin
  Result := TpgtoPJCollectionItem(inherited Items[Index]);
end;

function TpgtoPJCollection.New: TpgtoPJCollectionItem;
begin
  Result := TpgtoPJCollectionItem.Create;
  Self.Add(Result);
end;

procedure TpgtoPJCollection.SetItem(Index: Integer;
  Value: TpgtoPJCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TpgtoPJCollectionItem }

constructor TpgtoPJCollectionItem.Create;
begin
  FinfoProcJud := TinfoProcJudCollection.Create;
end;

destructor TpgtoPJCollectionItem.Destroy;
begin
  FinfoProcJud.Free;

  inherited;
end;

procedure TevtPgtosDivs.GerarideBenef;
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'codPgto',      1,   4, 1, Self.FideBenef.codPgto);
  Gerador.wCampo(tcStr, '', 'tpInscBenef',  1,   1, 0, TpInscricaoToStr( Self.FideBenef.tpInscBenef ));
  Gerador.wCampo(tcStr, '', 'nrInscBenef',  1,  14, 0, Self.FideBenef.nrInscBenef);
  Gerador.wCampo(tcStr, '', 'nmRazaoBenef', 1, 150, 1, Self.FideBenef.nmRazaoBenef);

  GerarinfoResidExt;
  GerarinfoMolestia;
  GerarideEstab(Self.FideBenef.ideEstab);

  Gerador.wGrupo('/ideBenef');
end;

procedure TevtPgtosDivs.GerarinfoResidExt;
begin
  Gerador.wGrupo('infoResidExt');

  GerarinfoEnder;
  GerarinfoFiscal;

  Gerador.wGrupo('/infoResidExt');
end;

procedure TevtPgtosDivs.GerarinfoEnder;
begin
  Gerador.wGrupo('infoEnder');

  Gerador.wCampo(tcStr, '', 'paisResid', 1,  3, 1, Self.FideBenef.infoResidExt.infoEnder.paisResid);
  Gerador.wCampo(tcStr, '', 'dscLograd', 1, 80, 1, Self.FideBenef.infoResidExt.infoEnder.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',  1, 10, 0, Self.FideBenef.infoResidExt.infoEnder.nrLograd);
  Gerador.wCampo(tcStr, '', 'complem',   1, 30, 0, Self.FideBenef.infoResidExt.infoEnder.complem);
  Gerador.wCampo(tcStr, '', 'bairro',    1, 60, 0, Self.FideBenef.infoResidExt.infoEnder.bairro);
  Gerador.wCampo(tcStr, '', 'cidade',    1, 30, 0, Self.FideBenef.infoResidExt.infoEnder.cidade);
  Gerador.wCampo(tcStr, '', 'codPostal', 1, 12, 0, Self.FideBenef.infoResidExt.infoEnder.codPostal);

  Gerador.wGrupo('/infoEnder');
end;

procedure TevtPgtosDivs.GerarinfoFiscal;
begin
  Gerador.wGrupo('infoFiscal');

  Gerador.wCampo(tcStr, '', 'indNIF',        1,  1, 1, indNIFToStr( Self.FideBenef.infoResidExt.infoFiscal.indNIF ) );
  Gerador.wCampo(tcStr, '', 'nifBenef',      1, 20, 0, Self.FideBenef.infoResidExt.infoFiscal.nifBenef);
  Gerador.wCampo(tcStr, '', 'relFontePagad', 1,  3, 0, Self.FideBenef.infoResidExt.infoFiscal.relFontePagad);

  Gerador.wGrupo('/infoFiscal');
end;

procedure TevtPgtosDivs.GerarinfoMolestia;
begin
  if ( Self.FideBenef.infoMolestia.dtLaudo <> 0 ) then
  begin
    Gerador.wGrupo('infoMolestia');

    Gerador.wCampo(tcDat, '', 'dtLaudo', 10, 10, 1, Self.FideBenef.infoMolestia.dtLaudo);

    Gerador.wGrupo('/infoMolestia');
  end;
end;

procedure TevtPgtosDivs.GerarideEstab(Lista: TideEstabCollection);
var
  item: TideEstabCollectionItem;
  i: Integer;
begin
  Gerador.wGrupo('infoPgto');

  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideEstab');

    Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, TpInscricaoToStr(item.tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 14, 1, item.nrInsc);

    if item.pgtoPF.Count > 0 then
    begin
      Gerador.wGrupo('pgtoResidBR');

      GerarpgtoPF(item.pgtoPF);
      GerarpgtoPJ(item.pgtoPJ);

      Gerador.wGrupo('/pgtoResidBR');
    end;

    GerarpgtoResidExt(item.pgtoResidExt);

    Gerador.wGrupo('/ideEstab');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'ideEstab', 'Lista de Estabelecimentos', ERR_MSG_MAIOR_MAXIMO + '999');

  Gerador.wGrupo('/infoPgto');
end;

procedure TevtPgtosDivs.GerarpgtoPF(Lista: TpgtoPFCollection);
var
  item: TpgtoPFCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('pgtoPF');

    Gerador.wCampo(tcDat, '', 'dtPgto',            10, 10, 1, item.dtPgto);
    Gerador.wCampo(tcStr, '', 'indSuspExig',        1,  1, 1, SimNaoToStr(item.indSuspExig));
    Gerador.wCampo(tcStr, '', 'indDecTerceiro',     1,  1, 1, SimNaoToStr(item.indDecTerceiro));
    Gerador.wCampo(tcDe2, '', 'vlrRendTributavel',  1, 14, 1, item.vlrRendTributavel);
    Gerador.wCampo(tcDe2, '', 'vlrIRRF',            1, 14, 1, item.vlrIRRF);

    GerardetDeducao(item.detDeducao);
    GerarrendIsento(item.rendIsento);
    GerardetCompet(item.detCompet);
    GerarcompJud(item.compJud);
    GerarinfoRRA(item.infoRRA);
    GerarinfoProcJud(item.infoProcJud);
    GerardepJudicial(item.depJudicial);

    Gerador.wGrupo('/pgtoPF');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'pgtoPF', 'Lista de Beneficiários PF', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerardetDeducao(Lista: TdetDeducaoCollection);
var
  item: TdetDeducaoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('detDeducao');

    Gerador.wCampo(tcStr, '', 'indTpDeducao', 1,  1, 1, indTpDeducaoToStr( item.indTpDeducao ));
    Gerador.wCampo(tcDe2, '', 'vlrDeducao',   1, 14, 1, item.vlrDeducao);

    Gerador.wGrupo('/detDeducao');
  end;

  if Lista.Count > 6 then
    Gerador.wAlerta('', 'detDeducao', 'Lista de Detalhamento de Deduções', ERR_MSG_MAIOR_MAXIMO + '6');
end;

procedure TevtPgtosDivs.GerarrendIsento(Lista: TrendIsentoCollection);
var
  item: TrendIsentoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('rendIsento');

    Gerador.wCampo(tcStr, '', 'tpIsencao',      1,   2, 1, tpIsencaoToStr( item.tpIsencao ));
    Gerador.wCampo(tcDe2, '', 'vlrIsento',      1,  14, 1, item.vlrIsento);
    Gerador.wCampo(tcStr, '', 'descRendimento', 1, 100, 0, item.descRendimento);

    Gerador.wGrupo('/rendIsento');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'rendIsento', 'Lista de Rendimentos Isentos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerardetCompet(Lista: TdetCompetCollection);
var
  item: TdetCompetCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('detCompet');

    Gerador.wCampo(tcStr, '', 'indPerReferencia',  1,  1, 1, indPerReferenciaToStr( item.indPerReferencia ));
    Gerador.wCampo(tcStr, '', 'perRefPagto',       7,  7, 1, item.perRefPagto);
    Gerador.wCampo(tcDe2, '', 'vlrRendTributavel', 1, 14, 1, item.vlrRendTributavel);

    Gerador.wGrupo('/detCompet');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'detCompet', 'Lista de Detalhamento de Competências', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerarcompJud(Item: TCompJud);
begin
  if ( ( Item.vlrCompAnoCalend <> 0 ) or ( Item.vlrCompAnoAnt <> 0 ) ) then
  begin
    Gerador.wGrupo('compJud');

    Gerador.wCampo(tcDe2, '', 'vlrCompAnoCalend', 1, 14, 0, Item.vlrCompAnoCalend);
    Gerador.wCampo(tcDe2, '', 'vlrCompAnoAnt',    1, 14, 0, Item.vlrCompAnoAnt);

    Gerador.wGrupo('/compJud');
  end;
end;

procedure TevtPgtosDivs.GerarinfoRRA(Lista: TinfoRRACollection);
var
  item: TinfoRRACollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoRRA');

    Gerador.wCampo(tcStr, '', 'tpProcRRA',   1,  1, 0, TpProcToStr( item.tpProcRRA ));
    Gerador.wCampo(tcStr, '', 'nrProcRRA',   1, 21, 0, item.nrProcRRA);
    Gerador.wCampo(tcStr, '', 'codSusp',     1, 14, 0, item.codSusp);
    Gerador.wCampo(tcStr, '', 'natRRA',      1, 50, 0, item.natRRA);
    Gerador.wCampo(tcInt, '', 'qtdMesesRRA', 1,  5, 0, item.qtdMesesRRA);

    GerardespProcJud(item.despProcJud);

    Gerador.wGrupo('/infoRRA');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoRRA', 'Lista de Informações RRA', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerardespProcJud(Item: TdespProcJud);
begin
  if ( ( Item.vlrDespCustas <> 0 ) or ( Item.vlrDespAdvogados <> 0 ) ) then
  begin
    Gerador.wGrupo('despProcJud');

    Gerador.wCampo(tcDe2, '', 'vlrDespCustas',    1, 14, 1, Item.vlrDespCustas);
    Gerador.wCampo(tcDe2, '', 'vlrDespAdvogados', 1, 14, 1, Item.vlrDespAdvogados);

    GerarideAdvogado(Item.ideAdvogado);

    Gerador.wGrupo('/despProcJud');
  end;
end;

procedure TevtPgtosDivs.GerarideAdvogado(Lista: TideAdvogadoCollection);
var
  item: TideAdvogadoCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('ideAdvogado');

    Gerador.wCampo(tcStr, '', 'tpInscAdvogado', 1,  1, 1, TpInscricaoToStr( item.tpInscAdvogado ));
    Gerador.wCampo(tcStr, '', 'nrInscAdvogado', 1, 14, 1, item.nrInscAdvogado);
    Gerador.wCampo(tcDe2, '', 'vlrAdvogado',    1, 14, 1, item.vlrAdvogado);

    Gerador.wGrupo('/ideAdvogado');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'ideAdvogado', 'Lista de Identificação de Advogados', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerarinfoProcJud(Lista: TinfoProcJudCollection);
var
  item: TinfoProcJudCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcJud');

    Gerador.wCampo(tcStr, '', 'nrProcJud',         1, 21, 1, item.nrProcJud);
    Gerador.wCampo(tcStr, '', 'codSusp',           1, 14, 0, item.codSusp);
    Gerador.wCampo(tcStr, '', 'indOrigemRecursos', 1,  1, 1, indOrigemRecursosToStr( item.indOrigemRecursos ));

    GerardespProcJud(item.despProcJud);
    GerarorigemRecursos(item.origemRecursos);

    Gerador.wGrupo('/infoProcJud');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'infoProcJud', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerarorigemRecursos(Item: TorigemRecursos);
begin
  if ( Item.cnpjOrigemRecursos <> '' ) then
  begin
    Gerador.wGrupo('origemRecursos');

    Gerador.wCampo(tcStr, '', 'cnpjOrigemRecursos', 14, 14, 1, Item.cnpjOrigemRecursos);

    Gerador.wGrupo('/origemRecursos');
  end;
end;

procedure TevtPgtosDivs.GerardepJudicial(Item: TdepJudicial);
begin
  if ( Item.vlrDepJudicial <> 0 ) then
  begin
    Gerador.wGrupo('depJudicial');

    Gerador.wCampo(tcDe2, '', 'vlrDepJudicial', 1, 14, 0, Item.vlrDepJudicial);

    Gerador.wGrupo('/depJudicial');
  end;
end;

procedure TevtPgtosDivs.GerarpgtoPJ(Lista: TpgtoPJCollection);
var
  item: TpgtoPJCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('pgtoPJ');

    Gerador.wCampo(tcDat, '', 'dtPagto',           10, 10, 1, item.dtPagto);
    Gerador.wCampo(tcDe2, '', 'vlrRendTributavel',  1, 14, 1, item.vlrRendTributavel);
    Gerador.wCampo(tcDe2, '', 'vlrRet',             1, 14, 1, item.vlrRet);

    GerarinfoProcJud(item.infoProcJud);

    Gerador.wGrupo('/pgtoPJ');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'pgtoPJ', 'Lista de Beneficiarios PJ', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtPgtosDivs.GerarpgtoResidExt(Item: TpgtoResidExt);
begin
  if ( ( Item.dtPagto <> 0 ) or ( Item.tpRendimento <> '' ) or
       ( Item.formaTributacao <> '' ) or ( Item.vlrPgto <> 0 ) or
       ( Item.vlrRet <> 0 ) ) then
  begin
    Gerador.wGrupo('pgtoResidExt');

    Gerador.wCampo(tcDat, '', 'dtPagto',         10, 10, 1, Item.dtPagto);
    Gerador.wCampo(tcStr, '', 'tpRendimento',     1,  3, 1, Item.tpRendimento);
    Gerador.wCampo(tcStr, '', 'formaTributacao',  1,  2, 1, Item.formaTributacao);
    Gerador.wCampo(tcDe2, '', 'vlrPgto',          1, 14, 1, Item.vlrPgto);
    Gerador.wCampo(tcDe2, '', 'vlrRet',           1, 14, 1, Item.vlrRet);

    Gerador.wGrupo('/pgtoResidExt');
  end;
end;

function TevtPgtosDivs.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtPgtosDivs');
    Gerador.wGrupo('evtPgtosDivs id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    GerarideBenef;

    Gerador.wGrupo('/evtPgtosDivs');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtPgtosDivs');

//    Validar(schevtPgtosDivs);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtPgtosDivs.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtPgtosDivs';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideBenef';
      ideBenef.codPgto      := INIRec.ReadString(sSecao, 'codPgto', EmptyStr);
      ideBenef.tpInscBenef  := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscBenef', '1'));
      ideBenef.nrInscBenef  := INIRec.ReadString(sSecao, 'nrInscBenef', EmptyStr);
      ideBenef.nmRazaoBenef := INIRec.ReadString(sSecao, 'nmRazaoBenef', EmptyStr);

      sSecao := 'infoEnder';
      if INIRec.ReadString(sSecao, 'paisResid', '') <> '' then
      begin
        ideBenef.infoResidExt.infoEnder.paisResid := INIRec.ReadString(sSecao, 'paisResid', EmptyStr);
        ideBenef.infoResidExt.infoEnder.dscLograd := INIRec.ReadString(sSecao, 'dscLograd', EmptyStr);
        ideBenef.infoResidExt.infoEnder.nrLograd  := INIRec.ReadString(sSecao, 'nrLograd', EmptyStr);
        ideBenef.infoResidExt.infoEnder.complem   := INIRec.ReadString(sSecao, 'complem', EmptyStr);
        ideBenef.infoResidExt.infoEnder.bairro    := INIRec.ReadString(sSecao, 'bairro', EmptyStr);
        ideBenef.infoResidExt.infoEnder.cidade    := INIRec.ReadString(sSecao, 'cidade', EmptyStr);
        ideBenef.infoResidExt.infoEnder.codPostal := INIRec.ReadString(sSecao, 'codPostal', EmptyStr);

        sSecao := 'infoFiscal';
        ideBenef.infoResidExt.infoFiscal.indNIF        := StrToindNIF(Ok, INIRec.ReadString(sSecao, 'indNIF', '1'));
        ideBenef.infoResidExt.infoFiscal.nifBenef      := INIRec.ReadString(sSecao, 'nifBenef', EmptyStr);
        ideBenef.infoResidExt.infoFiscal.relFontePagad := INIRec.ReadString(sSecao, 'relFontePagad', EmptyStr);
      end;

      sSecao := 'infoMolestia';
      if INIRec.ReadString(sSecao, 'dtLaudo', '') <> '' then
        ideBenef.infoMolestia.dtLaudo := StringToDateTime(INIRec.ReadString(sSecao, 'dtLaudo', '0'));

      with ideBenef do
      begin
        I := 1;
        while true do
        begin
          // de 001 até 999
          sSecao := 'ideEstab' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideEstab.New do
          begin
            tpInsc := StrToTpInscricao(Ok, sFim);
            nrInsc := INIRec.ReadString(sSecao, 'nrInsc', '');

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'pgtoPF' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtPgto', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with pgtoPF.New do
              begin
                dtPgto            := StringToDateTime(sFim);
                indSuspExig       := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'indSuspExig', 'S'));
                indDecTerceiro    := StrToSimNao(Ok, INIRec.ReadString(sSecao, 'indDecTerceiro', 'S'));
                vlrRendTributavel := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendTributavel', ''), 0);
                vlrIRRF           := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIRRF', ''), 0);

                K := 1;
                while true do
                begin
                  // de 0 até 6
                  sSecao := 'detDeducao' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 1);
                  sFim   := INIRec.ReadString(sSecao, 'indTpDeducao', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with detDeducao.New do
                  begin
                    indTpDeducao := StrToindTpDeducao(Ok, sFim);
                    vlrDeducao   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDeducao', ''), 0);
                  end;

                  Inc(K);
                end;

                K := 1;
                while true do
                begin
                  // de 000 até 999
                  sSecao := 'rendIsento' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim   := INIRec.ReadString(sSecao, 'tpIsencao', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with rendIsento.New do
                  begin
                    tpIsencao      := StrTotpIsencao(Ok, sFim);
                    vlrIsento      := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrIsento', ''), 0);
                    descRendimento := INIRec.ReadString(sSecao, 'descRendimento', '');
                  end;

                  Inc(K);
                end;

                K := 1;
                while true do
                begin
                  // de 000 até 999
                  sSecao := 'detCompet' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim   := INIRec.ReadString(sSecao, 'indPerReferencia', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with detCompet.New do
                  begin
                    indPerReferencia  := StrToindPerReferencia(Ok, sFim);
                    perRefPagto       := INIRec.ReadString(sSecao, 'perRefPagto', '');
                    vlrRendTributavel := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendTributavel', ''), 0);
                  end;

                  Inc(K);
                end;

                sSecao := 'compJud' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
                if INIRec.ReadString(sSecao, 'vlrCompAnoCalend', '') <> '' then
                begin
                  compJud.vlrCompAnoCalend := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCompAnoCalend', ''), 0);
                  compJud.vlrCompAnoAnt    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrCompAnoAnt', ''), 0);
                end;

                K := 1;
                while true do
                begin
                  // de 000 até 999
                  sSecao := 'infoRRA' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim   := INIRec.ReadString(sSecao, 'tpProcRRA', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoRRA.New do
                  begin
                    tpProcRRA   := StrToTpProc(Ok, sFim);
                    nrProcRRA   := INIRec.ReadString(sSecao, 'nrProcRRA', '');
                    codSusp     := INIRec.ReadString(sSecao, 'codSusp', '');
                    natRRA      := INIRec.ReadString(sSecao, 'natRRA', '');
                    qtdMesesRRA := INIRec.ReadInteger(sSecao, 'qtdMesesRRA', 0);

                    sSecao := 'infoRRA_despProcJud' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3);
                    if INIRec.ReadString(sSecao, 'vlrDespCustas', '') <> '' then
                    begin
                      despProcJud.vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''), 0);
                      despProcJud.vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''), 0);

                      L := 1;
                      while true do
                      begin
                        // de 000 até 999
                        sSecao := 'infoRRA_ideAdvogado' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 3);
                        sFim   := INIRec.ReadString(sSecao, 'tpInscAdvogado', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with despProcJud.ideAdvogado.New do
                        begin
                          tpInscAdvogado := StrToTpInscricao(Ok, sFim);
                          nrinscAdvogado := INIRec.ReadString(sSecao, 'nrinscAdvogado', '');
                          vlrAdvogado    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdvogado', ''), 0);
                        end;

                        Inc(L);
                      end;
                    end;
                  end;

                  Inc(K);
                end;

                K := 1;
                while true do
                begin
                  // de 000 até 999
                  sSecao := 'pgtoPF_infoProcJud' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoProcJud.New do
                  begin
                    nrProcJud         := sFim;
                    codSusp           := INIRec.ReadString(sSecao, 'codSusp', '');
                    indOrigemRecursos := StrToindOrigemRecursos(Ok, INIRec.ReadString(sSecao, 'indOrigemRecursos', ''));

                    sSecao := 'pgtoPF_infoProcJud_despProcJud' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3);
                    if INIRec.ReadString(sSecao, 'vlrDespCustas', '') <> '' then
                    begin
                      despProcJud.vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''), 0);
                      despProcJud.vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''), 0);

                      L := 1;
                      while true do
                      begin
                        // de 000 até 999
                        sSecao := 'pgtoPF_infoProcJud_ideAdvogado' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 3);
                        sFim   := INIRec.ReadString(sSecao, 'tpInscAdvogado', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with despProcJud.ideAdvogado.New do
                        begin
                          tpInscAdvogado := StrToTpInscricao(Ok, sFim);
                          nrinscAdvogado := INIRec.ReadString(sSecao, 'nrinscAdvogado', '');
                          vlrAdvogado    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdvogado', ''), 0);
                        end;

                        Inc(L);
                      end;
                    end;

                    sSecao := 'pgtoPF_infoProcJud_origemRecursos' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3);
                    if INIRec.ReadString(sSecao, 'cnpjOrigemRecursos', '') <> '' then
                      origemRecursos.cnpjOrigemRecursos := INIRec.ReadString(sSecao, 'cnpjOrigemRecursos', '');
                  end;

                  Inc(K);
                end;

                sSecao := 'depJudicial' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3);
                if INIRec.ReadString(sSecao, 'vlrDepJudicial', '') <> '' then
                  depJudicial.vlrDepJudicial := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDepJudicial', ''), 0);
              end;

              Inc(J);
            end;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'pgtoPJ' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'dtPagto', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with pgtoPJ.New do
              begin
                dtPagto           := StringToDateTime(sFim);
                vlrRendTributavel := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRendTributavel', ''), 0);
                vlrRet            := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRet', ''), 0);

                K := 1;
                while true do
                begin
                  // de 000 até 999
                  sSecao := 'pgtoPJ_infoProcJud' + IntToStrZero(I, 3) +
                                  IntToStrZero(J, 3) + IntToStrZero(K, 3);
                  sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with infoProcJud.New do
                  begin
                    nrProcJud         := sFim;
                    codSusp           := INIRec.ReadString(sSecao, 'codSusp', '');
                    indOrigemRecursos := StrToindOrigemRecursos(Ok, INIRec.ReadString(sSecao, 'indOrigemRecursos', ''));

                    sSecao := 'pgtoPJ_infoProcJud_despProcJud' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3);
                    if INIRec.ReadString(sSecao, 'vlrDespCustas', '') <> '' then
                    begin
                      despProcJud.vlrDespCustas    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', ''), 0);
                      despProcJud.vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', ''), 0);

                      L := 1;
                      while true do
                      begin
                        // de 000 até 999
                        sSecao := 'pgtoPJ_infoProcJud_ideAdvogado' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 3);
                        sFim   := INIRec.ReadString(sSecao, 'tpInscAdvogado', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with despProcJud.ideAdvogado.New do
                        begin
                          tpInscAdvogado := StrToTpInscricao(Ok, sFim);
                          nrinscAdvogado := INIRec.ReadString(sSecao, 'nrinscAdvogado', '');
                          vlrAdvogado    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdvogado', ''), 0);
                        end;

                        Inc(L);
                      end;
                    end;

                    sSecao := 'pgtoPJ_infoProcJud_origemRecursos' + IntToStrZero(I, 3) +
                                        IntToStrZero(J, 3) + IntToStrZero(K, 3);
                    if INIRec.ReadString(sSecao, 'cnpjOrigemRecursos', '') <> '' then
                      origemRecursos.cnpjOrigemRecursos := INIRec.ReadString(sSecao, 'cnpjOrigemRecursos', '');
                  end;

                  Inc(K);
                end;

              end;

              Inc(J);
            end;

            sSecao := 'pgtoResidExt' + IntToStrZero(I, 3);
            if INIRec.ReadString(sSecao, 'dtPagto', '') <> '' then
            begin
              pgtoResidExt.dtPagto         := StringToDateTime(INIRec.ReadString(sSecao, 'dtPagto', '0'));
              pgtoResidExt.tpRendimento    := INIRec.ReadString(sSecao, 'tpRendimento', EmptyStr);
              pgtoResidExt.formaTributacao := INIRec.ReadString(sSecao, 'formaTributacao', '');
              pgtoResidExt.vlrPgto         := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgto', ''), 0);
              pgtoResidExt.vlrRet          := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRet', ''), 0);
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

end.
