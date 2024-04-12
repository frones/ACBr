{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcesS2500;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2500Collection = class;
  TS2500CollectionItem = class;
  TEvtProcTrab = class;
  TIdeEmpregadorS2500 = class;
  TIdeResp = class;
  TInfoProcesso = class;
  TDadosCompl = class;
  TInfoProcJud = class;
  TInfoCCP = class;
  TIdeTrab = class;
  TDependenteCollectionS2500 = class;
  TDependenteCollectionItemS2500 = class;
  TInfoContrCollection = class;
  TInfoContrCollectionItem = class;
  TInfoCompl = class;
  TRemuneracaoCollection = class;
  TRemuneracaoCollectionItem = class;
  TInfoVinc = class;
  TInfoDeslig = class;
  TInfoTerm = class;
  TMudCategAtivCollection = class;
  TMudCategAtivCollectionItem = class;
  TUnicContrCollection = class;
  TUnicContrCollectionItem = class;
  TIdeEstab = class;
  TInfoVlr = class;
  TAbonoCollection = class;
  TAbonoCollectionItem = class;
  TIdePeriodoCollection = class;
  TIdePeriodoCollectionItem = class;
  TbaseCalculo = class;
  TInfoFGTS = class;
  TBaseMudCateg = class;

  TS2500Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2500CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2500CollectionItem);
  public
    function New: TS2500CollectionItem;
    property Items[Index: Integer]: TS2500CollectionItem read GetItem write SetItem; default;
  end;

  TS2500CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtProcTrab: TEvtProcTrab;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtProcTrab: TEvtProcTrab read FEvtProcTrab write FEvtProcTrab;
  end;

  TEvtProcTrab = class(TESocialEvento)
  private
    FideEvento: TIdeEvento2;
    FideEmpregador: TIdeEmpregadorS2500;
    FideTrab: TIdeTrab;
    FinfoProcesso: TInfoProcesso;

    procedure GerarIdeEmpregador(obj: TIdeEmpregadorS2500);
    procedure GerarIdeResp(obj: TIdeResp);
    procedure GerarInfoProcesso(obj: TinfoProcesso);
    procedure GerarIdeTrab(Obj: TideTrab);
    procedure GerarInfoContr(obj: TinfoContrCollection);
    procedure GerarDependente(obj: TDependenteCollectionS2500);
    procedure GerarDadosCompl(Obj: TdadosCompl);
    procedure GerarInfoCompl(obj: TinfoCompl);
    procedure GerarInfoProcJud(Obj: TInfoProcJud);
    procedure GerarInfoCCP(Obj: TInfoCCP);
    procedure GerarRemuneracao(obj: TRemuneracaoCollection);
    procedure GerarInfoVinc(obj: TinfoVinc);
    procedure GerarInfoTerm(obj: TinfoTerm);
    procedure GerarSucessaoVinc(obj: TsucessaoVinc);
    procedure GerarInfoDeslig(obj: TinfoDeslig);
    procedure GerarMudCategAtiv(obj: TMudCategAtivCollection);
    procedure GerarUnicContr(obj: TUnicContrCollection);
    procedure GerarIdeEstab(obj: TIdeEstab);
    procedure GerarInfoVlr(obj: TInfoVlr);
    procedure GerarAbono(obj: TAbonoCollection);
    procedure GerarIdePeriodo(obj: TIdePeriodoCollection);
    procedure GerarBaseCalculo(obj: TbaseCalculo);
    procedure GerarInfoFGTS(obj: TinfoFGTS);
    procedure GerarBaseMudCateg(obj: TbaseMudCateg);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregadorS2500 read FIdeEmpregador write FIdeEmpregador;
    property ideTrab: TIdeTrab read FIdeTrab write FIdeTrab;
    property infoProcesso: TInfoProcesso read FInfoProcesso write FInfoProcesso;
  end;

  TIdeEmpregadorS2500 = class(TInscricao)
  private
    FiDeResp: TIdeResp;

    function getIdeResp(): TIdeResp;
  public
    constructor Create;
    destructor  Destroy; override;

    function instIdeResp(): Boolean;

    property ideResp: TIdeResp read getIdeResp write FIdeResp;
  end;

  TIdeResp = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
  public
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
  end;

  TInfoProcesso = class(TObject)
  private
    Forigem: tpOrigemProc;
    FnrProcTrab: string;
    FobsProcTrab: string;
    FdadosCompl: TDadosCompl;
  public
    constructor Create;
    destructor  Destroy; override;

    property origem: tpOrigemProc read Forigem write Forigem;
    property nrProcTrab: string read FNrProcTrab write FNrProcTrab;
    property obsProcTrab: string read FObsProcTrab write FObsProcTrab;
    property dadosCompl: TDadosCompl read FDadosCompl write FDadosCompl;
  end;

  TDadosCompl = class(TObject)
  private
    FinfoProcJud: TInfoProcJud;
    FinfoCCP: TInfoCCP;

    function getInfoProcJud(): TInfoProcJud;
    function getInfoCCP(): TInfoCCP;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoProcJud(): boolean;
    function instInfoCCP(): boolean;

    property infoCCP: TInfoCCP read getInfoCCP write FInfoCCP;
    property infoProcJud: TInfoProcJud read getInfoProcJud write FInfoProcJud;
  end;

  TInfoProcJud = class(TObject)
  private
    FdtSent: TDateTime;
    FufVara: string;
    FcodMunic: Integer;
    FidVara: Integer;
  public
    property dtSent: TDateTime read FDtSent write FDtSent;
    property ufVara: string read FUfVara write FUfVara;
    property codMunic: Integer read FCodMunic write FCodMunic;
    property idVara: Integer read FIdVara write FIdVara;
  end;

  TInfoCCP = class(TObject)
  private
    FdtCCP: TDateTime;
    FtpCCP: tpTpCCP;
    FcnpjCCP: string;
  public
    property dtCCP: TDateTime read FdtCCP write FdtCCP;
    property tpCCP: tpTpCCP read FtpCCP write FtpCCP;
    property cnpjCCP: string read FcnpjCCP write FcnpjCCP;
  end;

  TIdeTrab = class(TObject)
  private
    FcpfTrab: string;
    FnmTrab: string;
    FdtNascto: TDateTime;
    Fdependente: TDependenteCollectionS2500;
    FinfoContr: TInfoContrCollection;

    function getDependenteS2500(): TDependenteCollectionS2500;
    function getInfoContr(): TInfoContrCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instDependenteS2500(): boolean;
    function instInfoContr(): boolean;

    property cpfTrab: string read FCpfTrab write FCpfTrab;
    property nmTrab: string read FNmTrab write FNmTrab;
    property dtNascto: TDateTime read FDtNascto write FDtNascto;
    property dependente: TDependenteCollectionS2500 read getDependenteS2500 write FDependente;
    property infoContr: TInfoContrCollection read getInfoContr write FInfoContr;
  end;

  TDependenteCollectionS2500 = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDependenteCollectionItemS2500;
    procedure SetItem(Index: Integer; Value: TDependenteCollectionItemS2500);
  public
    function New: TDependenteCollectionItemS2500;
    property Items[Index: Integer]: TDependenteCollectionItemS2500 read GetItem write SetItem; default;
  end;

  TDependenteCollectionItemS2500 = class(TObject)
  private
    FcpfDep: string;
    FtpDep: tpTpDep;
    FdescDep: string;
  public
    property tpDep: tpTpDep read FTpDep write FTpDep;
    property cpfDep: string read FCpfDep write FCpfDep;
    property descDep: string read FDescDep write FDescDep;
  end;

  TInfoContrCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoContrCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoContrCollectionItem);
  public
    function New: TInfoContrCollectionItem;
    property Items[Index: Integer]: TInfoContrCollectionItem read GetItem write SetItem; default;
  end;

  TInfoContrCollectionItem = class(TObject)
  private
    FtpContr: tpTpContrS2500;
    FindContr: tpSimNao;
    FdtAdmOrig: TDateTime;
    FindReint: tpSimNaoFacultativo;
    FindCateg: tpSimNao;
    FindNatAtiv: tpSimNao;
    FindMotDeslig: tpSimNao;
    FindUnic: tpSimNaoFacultativo;
    Fmatricula: string;
    FcodCateg: Integer;
    FdtInicio: TDateTime;
    FinfoCompl: TInfoCompl;
    FmudCategAtiv: TMudCategAtivCollection;
    FunicContr: TUnicContrCollection;
    FideEstab: TIdeEstab;

    function getMudCategAtiv(): TMudCategAtivCollection;
    function getInfoCompl(): TInfoCompl;
    function getUnicContr(): TUnicContrCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instMudCategAtiv(): boolean;
    function instInfoCompl(): boolean;
    function instUnicContr(): boolean;

    property tpContr: tpTpContrS2500 read FTpContr write FTpContr;
    property indContr: tpSimNao read FIndContr write FIndContr;
    property dtAdmOrig: TDateTime read FDtAdmOrig write FDtAdmOrig;
    property indReint: tpSimNaoFacultativo read FIndReint write FIndReint;
    property indCateg: tpSimNao read FIndCateg write FIndCateg;
    property indNatAtiv: tpSimNao read FIndNatAtiv write FIndNatAtiv;
    property indMotDeslig: tpSimNao read FIndMotDeslig write FIndMotDeslig;
    property indUnic: tpSimNaoFacultativo read FIndUnic write FIndUnic;
    property matricula: string read FMatricula write FMatricula;
    property codCateg: Integer read FCodCateg write FCodCateg;
    property dtInicio: TDateTime read FDtInicio write FDtInicio;
    property infoCompl: TInfoCompl read getInfoCompl write FInfoCompl;
    property mudCategAtiv: TMudCategAtivCollection read getMudCategAtiv write FMudCategAtiv;
    property unicContr: TUnicContrCollection read getUnicContr write FUnicContr;
    property ideEstab: TIdeEstab read FIdeEstab write FIdeEstab;
  end;

  TInfoCompl = class(TObject)
  private
    FcodCBO: string;
    FnatAtividade: tpNatAtividade;
    Fremuneracao: TRemuneracaoCollection;
    FinfoVinc: TInfoVinc;
    FinfoTerm: TInfoTerm;

    function getRemuneracao(): TRemuneracaoCollection;
    function getInfoVinc(): TInfoVinc;
    function getInfoTerm(): TInfoTerm;
  public
    constructor Create;
    destructor  Destroy; override;

    function instRemuneracao(): boolean;
    function instInfoVinc(): boolean;
    function instInfoTerm(): boolean;

    property codCBO: string read FCodCBO write FCodCBO;
    property natAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property remuneracao: TRemuneracaoCollection read getRemuneracao write FRemuneracao;
    property infoVinc: TInfoVinc read getInfoVinc write FInfoVinc;
    property infoTerm: TInfoTerm read getInfoTerm write FInfoTerm;
  end;

  TRemuneracaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRemuneracaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRemuneracaoCollectionItem);
  public
    function New: TRemuneracaoCollectionItem;
    property Items[Index: Integer]: TRemuneracaoCollectionItem read GetItem write SetItem; default;
  end;

  TRemuneracaoCollectionItem = class(TRemuneracao)
  private
    FdtRemun: TDateTime;
    FvrSalFx: Double;
    FundSalFixo: tpUndSalFixo;
    FdscSalVar: string;
  public
    property dtRemun: TDateTime read FDtRemun write FDtRemun;
    property vrSalFx: Double read FvrSalFx write FvrSalFx;
    property undSalFixo: tpUndSalFixo read FundSalFixo write FundSalFixo;
    property dscSalVar: string read FdscSalVar write FdscSalVar;
  end;

  TInfoVinc = class(TObject)
  private
    FtpRegTrab: tpTpRegTrab;
    FtpRegPrev: tpTpRegPrev;
    FdtAdm: TDateTime;
    FtmpParc: tpTmpParc;
    Fduracao: TDuracao;
    Fobservacoes: TObservacoesCollection;
    FsucessaoVinc: TSucessaoVinc;
    FinfoDeslig: TInfoDeslig;

    function getDuracao(): TDuracao;
    function getObservacoes(): TObservacoesCollection;
    function getSucessaoVinc(): TSucessaoVinc;
    function getInfoDeslig(): TInfoDeslig;
  public
    constructor Create;
    destructor  Destroy; override;

    function instDuracao(): boolean;
    function instObservacoes(): boolean;
    function instSucessaoVinc(): boolean;
    function instInfoDeslig(): boolean;

    property tpRegTrab: tpTpRegTrab read FTpRegTrab write FTpRegTrab;
    property tpRegPrev: tpTpRegPrev read FTpRegPrev write FTpRegPrev;
    property dtAdm: TDateTime read FdtAdm write FdtAdm;
    property tmpParc: tpTmpParc read FtmpParc write FtmpParc;
    property duracao: TDuracao read getDuracao write FDuracao;
    property sucessaoVinc: TSucessaoVinc read getSucessaoVinc write FSucessaoVinc;
    property infoDeslig: TInfoDeslig read getInfoDeslig write FInfoDeslig;
    property observacoes: TObservacoesCollection read getObservacoes write FObservacoes;
  end;

  TInfoDeslig = class(TObject)
  private
    FdtDeslig: TDateTime;
    FmtvDeslig: string;
    FdtProjFimAPI: TDateTime;
    FpensAlim: tpPensaoAlim;
    FpercAliment: Double;
    FvrAlim: Double;
  public
    property dtDeslig: TDateTime read FdtDeslig write FdtDeslig;
    property mtvDeslig: string read FmtvDeslig write FmtvDeslig;
    property dtProjFimAPI: TDateTime read FdtProjFimAPI write FdtProjFimAPI;
    property pensAlim: tpPensaoAlim read FpensAlim write FpensAlim;
    property percAliment: Double read FpercAliment write FpercAliment;
    property vrAlim: Double read FvrAlim write FvrAlim;
  end;

  TInfoTerm = class(TObject)
  private
    FdtTerm: TDateTime;
    FmtvDesligTSV: string;
  public
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV: string read FmtvDesligTSV write FmtvDesligTSV;
  end;

  TMudCategAtivCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TMudCategAtivCollectionItem;
    procedure SetItem(Index: Integer; Value: TMudCategAtivCollectionItem);
  public
    function New: TMudCategAtivCollectionItem;
    property Items[Index: Integer]: TMudCategAtivCollectionItem read GetItem write SetItem; default;
  end;

  TMudCategAtivCollectionItem = class(TObject)
  private
    FcodCateg: Integer;
    FnatAtividade: tpNatAtividade;
    FdtMudCategAtiv: TDateTime;
  public
    property codCateg: Integer read FcodCateg write FcodCateg;
    property natAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property dtMudCategAtiv: TDateTime read FdtMudCategAtiv write FdtMudCategAtiv;
  end;

  TUnicContrCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TUnicContrCollectionItem;
    procedure SetItem(Index: Integer; Value: TUnicContrCollectionItem);
  public
    function New: TUnicContrCollectionItem;
    property Items[Index: Integer]: TUnicContrCollectionItem read GetItem write SetItem; default;
  end;

  TUnicContrCollectionItem = class(TObject)
  private
    FmatUnic: string;
    FcodCateg: Integer;
    FdtInicio: TDateTime;
  public
    property matUnic: string read FmatUnic write FmatUnic;
    property codCateg: integer read FcodCateg write FcodCateg;
    property dtInicio: TDateTime read FdtInicio write FdtInicio;
  end;

  TIdeEstab = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FinfoVlr: TInfoVlr;
  public
    constructor Create;
    destructor  Destroy; override;

    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property infoVlr: TInfoVlr read FInfoVlr write FInfoVlr;
  end;

  TInfoVlr = class(TObject)
  private
    FcompIni: string;
    FcompFim: string;
    FrepercProc: tpRepercProc;
    FvrRemun: double;
    FvrAPI: double;
    Fvr13API: double;
    FvrInden: double;
    FvrBaseIndenFGTS: double;
    FpagDiretoResc: tpSimNaoFacultativo;
    FindReperc: tpIndReperc;
    FindenSD: tpSimFacultativo;
    FindenAbono: tpSimFacultativo;
    Fabono: TAbonoCollection;
    FidePeriodo: TIdePeriodoCollection;

    function getAbono(): TAbonoCollection;
    function getIdePeriodo(): TIdePeriodoCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function instAbono(): boolean;
    function instIdePeriodo(): boolean;

    property compIni: string read FcompIni write FcompIni;
    property compFim: string read FcompFim write FcompFim;
    property repercProc: tpRepercProc read FrepercProc write FrepercProc;
    property vrRemun: double read FvrRemun write FvrRemun;
    property vrAPI: double read FvrAPI write FvrAPI;
    property vr13API: double read Fvr13API write Fvr13API;
    property vrInden: double read FvrInden write FvrInden;
    property vrBaseIndenFGTS: double read FvrBaseIndenFGTS write FvrBaseIndenFGTS;
    property pagDiretoResc: tpSimNaoFacultativo read FpagDiretoResc write FpagDiretoResc;
    property indReperc: tpIndReperc read FindReperc write FindReperc;
    property abono: TAbonoCollection read getAbono write Fabono;
    property indenSD: tpSimFacultativo read FindenSD write FindenSD;
    property indenAbono: tpSimFacultativo read FindenAbono write FindenAbono;
    property idePeriodo: TIdePeriodoCollection read getIdePeriodo write FIdePeriodo;
  end;

  TAbonoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TAbonoCollectionItem;
    procedure SetItem(Index: Integer; Value: TAbonoCollectionItem);
  public
    function New: TAbonoCollectionItem;
    property Items[Index: Integer]: TAbonoCollectionItem read GetItem write SetItem; default;
  end;

  TAbonoCollectionItem = class(TObject)
  private
    FanoBase: string;
  public
    constructor Create;
    destructor  Destroy; override;
    property anoBase: string read FanoBase write FanoBase;
  end;

  TIdePeriodoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdePeriodoCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdePeriodoCollectionItem);
  public
    function New: TIdePeriodoCollectionItem;
    property Items[Index: Integer]: TIdePeriodoCollectionItem read GetItem write SetItem; default;
  end;

  TIdePeriodoCollectionItem = class(TObject)
  private
    FperRef: string;
    FbaseCalculo: TBaseCalculo;
    FinfoFGTS: TInfoFGTS;
    FbaseMudCateg: TBaseMudCateg;

    function getInfoFGTS(): TInfoFGTS;
    function getBaseMudCateg(): TBaseMudCateg;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoFGTS(): boolean;
    function instBaseMudCateg(): boolean;

    property perRef: string read FperRef write FperRef;
    property baseCalculo: TBaseCalculo read FbaseCalculo write FbaseCalculo;
    property infoFGTS: TInfoFgts read getInfoFGTS write FInfoFGTS;
    property baseMudCateg: TBaseMudCateg read getBaseMudCateg write FbaseMudCateg;
  end;

  TBaseCalculo = class(TObject)
  private
    FvrBcCpMensal: double;
    FvrBcCp13: double;
    FvrBcFgts: double;
    FvrBcFgts13: double;
    FinfoAgNocivo: TinfoAgNocivo;

    function getInfoAgNocivo(): TinfoAgNocivo;
  public
    constructor Create;
    destructor  Destroy; override;

    function instInfoAgNocivo(): boolean;

    property vrBcCpMensal: double read FvrBcCpMensal write FvrBcCpMensal;
    property vrBcCp13: double read FvrBcCp13 write FvrBcCp13;
    property vrBcFgts: double read FvrBcFgts write FvrBcFgts;
    property vrBcFgts13: double read FvrBcFgts13 write FvrBcFgts13;
    property infoAgNocivo: TinfoAgNocivo read getInfoAgNocivo write FinfoAgNocivo;
  end;

  TInfoFGTS = class(TObject)
  private
    FvrBcFgtsGuia: double;
    FvrBcFgts13Guia: double;
    FpagDireto: tpSimNao;
    FvrBcFGTSProcTrab: Double;
    FvrBcFGTSSefip: Double;
    FvrBcFGTSDecAnt: Double;
  public
    property vrBcFgtsGuia: double read FvrBcFgtsGuia write FvrBcFgtsGuia;
    property vrBcFgts13Guia: double read FvrBcFgts13Guia write FvrBcFgts13Guia;
    property pagDireto: tpSimNao read FpagDireto write FpagDireto;
    property vrBcFGTSProcTrab: double read FvrBcFGTSProcTrab write FvrBcFGTSProcTrab;
    property vrBcFGTSSefip: double read FvrBcFGTSSefip write FvrBcFGTSSefip;
    property vrBcFGTSDecAnt: double read FvrBcFGTSDecAnt write FvrBcFGTSDecAnt;
  end;

  TBaseMudCateg = class(TObject)
  private
   FcodCateg: integer;
   FvrBcCPrev: double;
  public
   property codCateg: integer read FcodCateg write FcodCateg;
   property vrBcCPrev: double read FvrBcCPrev write FvrBcCPrev;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2500Collection }

function TS2500Collection.GetItem(Index: Integer): TS2500CollectionItem;
begin
  Result := TS2500CollectionItem(inherited Items[Index]);
end;

procedure TS2500Collection.SetItem(Index: Integer;
  Value: TS2500CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2500Collection.New: TS2500CollectionItem;
begin
  Result := TS2500CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2500CollectionItem }

constructor TS2500CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento  := teS2500;
  FEvtProcTrab := TEvtProcTrab.Create(AOwner);
end;

destructor TS2500CollectionItem.Destroy;
begin
  FEvtProcTrab.Free;

  inherited;
end;

{ TIdeEmpregadorS2500 }

constructor TIdeEmpregadorS2500.Create;
begin
  inherited Create;

  FIdeResp := nil;
end;

destructor TIdeEmpregadorS2500.Destroy;
begin
  if instIdeResp() then
    FreeAndNil(FIdeResp);

  inherited;
end;

function TIdeEmpregadorS2500.instIdeResp: Boolean;
begin
  Result := Assigned(FIdeResp);
end;

function TIdeEmpregadorS2500.getIdeResp: TIdeResp;
begin
  if not Assigned(FIdeResp) then
    FIdeResp := TIdeResp.Create;
  Result := FIdeResp;
end;

{ TDependenteCollectionS2500 }

function TDependenteCollectionS2500.GetItem(Index: Integer): TDependenteCollectionItemS2500;
begin
  Result := TDependenteCollectionItemS2500(inherited Items[Index]);
end;

procedure TDependenteCollectionS2500.SetItem(Index: Integer; Value: TDependenteCollectionItemS2500);
begin
  inherited Items[Index] := Value;
end;

function TDependenteCollectionS2500.New: TDependenteCollectionItemS2500;
begin
  Result := TDependenteCollectionItemS2500.Create;
  Self.Add(Result);
end;

{ TIdeTrab }

constructor TIdeTrab.Create;
begin
  inherited Create;

  FDependente := nil;
  FInfoContr := nil;
end;

destructor TIdeTrab.Destroy;
begin
  if instDependenteS2500() then
    FreeAndNil(FDependente);
  if instInfoContr() then
    FreeAndNil(FInfoContr);

  inherited;
end;

function TIdeTrab.InstDependenteS2500: boolean;
begin
  Result := Assigned(FDependente);
end;

function TIdeTrab.GetDependenteS2500: TDependenteCollectionS2500;
begin
  if not Assigned(FDependente) then
    FDependente := TDependenteCollectionS2500.Create;
  Result := FDependente;
end;

function TIdeTrab.InstInfoContr: boolean;
begin
  Result := Assigned(FInfoContr);
end;

function TIdeTrab.getInfoContr: TInfoContrCollection;
begin
  if not Assigned(FInfoContr) then
    FInfoContr := TInfoContrCollection.Create;
  Result := FInfoContr;
end;

{ TInfoContrCollection }

function TInfoContrCollection.GetItem(Index: Integer): TInfoContrCollectionItem;
begin
  Result := TInfoContrCollectionItem(inherited Items[Index]);
end;

procedure TInfoContrCollection.SetItem(Index: Integer; Value: TInfoContrCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoContrCollection.New: TInfoContrCollectionItem;
begin
  Result := TInfoContrCollectionItem.Create;
  Self.Add(Result);
end;

{TInfoContrCollectionItem}

constructor TInfoContrCollectionItem.Create;
begin
  inherited Create;

  FInfoCompl := nil;
  FMudCategAtiv := nil;
  FUnicContr := nil;
  FIdeEstab := TIdeEstab.Create;
end;

destructor TInfoContrCollectionItem.Destroy;
begin
  if instInfoCompl() then
    FreeAndNil(FInfoCompl);
  if instMudCategAtiv() then
    FreeAndNil(FMudCategAtiv);
  if instUnicContr() then
    FreeAndNil(FUnicContr);
  FreeAndNil(FIdeEstab);

  inherited;
end;

function TInfoContrCollectionItem.getUnicContr(): TUnicContrCollection;
begin
  if not Assigned(FUnicContr) then
    FUnicContr := TUnicContrCollection.Create;
  Result := FUnicContr;
end;

function TInfoContrCollectionItem.instUnicContr(): boolean;
begin
  Result := Assigned(FUnicContr);
end;

function TInfoContrCollectionItem.getMudCategAtiv(): TMudCategAtivCollection;
begin
  if not Assigned(FMudCategAtiv) then
    FMudCategAtiv := TMudCategAtivCollection.Create;
  Result := FMudCategAtiv;
end;

function TInfoContrCollectionItem.instMudCategAtiv(): boolean;
begin
  Result := Assigned(FMudCategAtiv);
end;

function TInfoContrCollectionItem.getInfoCompl(): TInfoCompl;
begin
  if not Assigned(FInfoCompl) then
    FInfoCompl := TInfoCompl.Create;
  Result := FInfoCompl;
end;

function TInfoContrCollectionItem.instInfoCompl(): boolean;
begin
  Result := Assigned(FInfoCompl);
end;

{ TRemuneracaoCollection }

function TRemuneracaoCollection.GetItem(Index: Integer): TRemuneracaoCollectionItem;
begin
  Result := TRemuneracaoCollectionItem(inherited Items[Index]);
end;

procedure TRemuneracaoCollection.SetItem(Index: Integer; Value: TRemuneracaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRemuneracaoCollection.New: TRemuneracaoCollectionItem;
begin
  Result := TRemuneracaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoCompl }

constructor TInfoCompl.Create;
begin
  inherited Create;

  FRemuneracao := nil;
  FInfoVinc := nil;
  FInfoTerm := nil;
end;

destructor TInfoCompl.Destroy;
begin
  if instRemuneracao() then
    FreeAndNil(FRemuneracao);
  if instInfoVinc() then
    FreeAndNil(FInfoVinc);
  if instInfoTerm() then
    FreeAndNil(FInfoTerm);

  inherited;
end;

function TInfoCompl.instRemuneracao(): boolean;
begin
  Result := Assigned(FRemuneracao);
end;

function TInfoCompl.getRemuneracao(): TRemuneracaoCollection;
begin
  if not Assigned(FRemuneracao) then
    FRemuneracao := TRemuneracaoCollection.Create;
  Result := FRemuneracao;
end;

function TInfoCompl.getInfoVinc(): TInfoVinc;
begin
  if not Assigned(FInfoVinc) then
    FInfoVinc := TInfoVinc.Create;
  Result := FInfoVinc;
end;

function TInfoCompl.instInfoVinc(): boolean;
begin
  Result := Assigned(FInfoVinc);
end;

function TInfoCompl.getInfoTerm(): TInfoTerm;
begin
  if not Assigned(FInfoTerm) then
    FInfoTerm := TInfoTerm.Create;
  Result := FInfoTerm;
end;

function TInfoCompl.instInfoTerm(): boolean;
begin
  Result := Assigned(FInfoTerm);
end;

{ TInfoVinc }

constructor TInfoVinc.Create;
begin
  inherited Create;

  FDuracao := nil;
  FObservacoes := nil;
  FSucessaoVinc := nil;
  FInfoDeslig := nil;
end;

destructor TInfoVinc.Destroy;
begin
  if instDuracao() then
    FreeAndNil(FDuracao);
  if instObservacoes() then
    FreeAndNil(FObservacoes);
  if instSucessaoVinc() then
    FreeAndNil(FSucessaoVinc);
  if instInfoDeslig() then
    FreeAndNil(FInfoDeslig);

  inherited;
end;

function TInfoVinc.getDuracao(): TDuracao;
begin
  if not Assigned(FDuracao) then
    FDuracao := TDuracao.Create;
  Result := FDuracao;
end;

function TInfoVinc.instDuracao(): boolean;
begin
  Result := Assigned(FDuracao);
end;

function TInfoVinc.instObservacoes(): boolean;
begin
  Result := Assigned(FObservacoes);
end;

function TInfoVinc.getObservacoes(): TObservacoesCollection;
begin
  if not Assigned(FObservacoes) then
    FObservacoes := TObservacoesCollection.Create;
  Result := FObservacoes;
end;

function TInfoVinc.getSucessaoVinc(): TSucessaoVinc;
begin
  if not Assigned(FSucessaoVinc) then
    FSucessaoVinc := TSucessaoVinc.Create;
  Result := FSucessaoVinc;
end;

function TInfoVinc.instSucessaoVinc(): boolean;
begin
  Result := Assigned(FSucessaoVinc);
end;

function TInfoVinc.getInfoDeslig(): TInfoDeslig;
begin
  if not Assigned(FInfoDeslig) then
    FInfoDeslig := TInfoDeslig.Create;
  Result := FInfoDeslig;
end;

function TInfoVinc.instInfoDeslig(): boolean;
begin
  Result := Assigned(FInfoDeslig);
end;

{ TInfoProcesso }

constructor TInfoProcesso.Create;
begin
  inherited Create;

  FDadosCompl := TDadosCompl.Create;
end;

destructor TInfoProcesso.Destroy;
begin
  FreeAndNil(FDadosCompl);

  inherited;
end;

{ TDadosCompl }

constructor TDadosCompl.Create;
begin
  inherited Create;

  FInfoProcJud := nil;
  FInfoCCP := nil;
end;

destructor TDadosCompl.Destroy;
begin
  if instInfoProcJud() then
    FreeAndNil(FInfoProcJud);
  if instInfoCCP() then
    FreeAndNil(FInfoCCP);

  inherited;
end;

function TDadosCompl.instInfoProcJud(): Boolean;
begin
  Result := Assigned(FInfoProcJud);
end;

function TDadosCompl.getInfoProcJud(): TInfoProcJud;
begin
  if not Assigned(FInfoProcJud) then
    FInfoProcJud := TInfoProcJud.Create;
  Result := FInfoProcJud;
end;

function TDadosCompl.instInfoCCP(): Boolean;
begin
  Result := Assigned(FInfoCCP);
end;

function TDadosCompl.getInfoCCP(): TInfoCCP;
begin
  if not Assigned(FInfoCCP) then
    FInfoCCP := TInfoCCP.Create;
  Result := FInfoCCP;
end;

{ TMudCategAtivCollection }

function TMudCategAtivCollection.GetItem(Index: Integer): TMudCategAtivCollectionItem;
begin
  Result := TMudCategAtivCollectionItem(inherited Items[Index]);
end;

procedure TMudCategAtivCollection.SetItem(Index: Integer; Value: TMudCategAtivCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TMudCategAtivCollection.New: TMudCategAtivCollectionItem;
begin
  Result := TMudCategAtivCollectionItem.Create;
  Self.Add(Result);
end;

{ TUnicContrCollection }

function TUnicContrCollection.GetItem(Index: Integer): TUnicContrCollectionItem;
begin
  Result := TUnicContrCollectionItem(inherited Items[Index]);
end;

procedure TUnicContrCollection.SetItem(Index: Integer; Value: TUnicContrCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TUnicContrCollection.New: TUnicContrCollectionItem;
begin
  Result := TUnicContrCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeEstab }

constructor TIdeEstab.Create;
begin
  inherited Create;

  FInfoVlr := TInfoVlr.Create;
end;

destructor TIdeEstab.Destroy;
begin
  FreeAndNil(FInfoVlr);

  inherited;
end;

{ TAbonoCollection }

function TAbonoCollection.GetItem(Index: Integer): TAbonoCollectionItem;
begin
  Result := TAbonoCollectionItem(inherited Items[Index]);
end;

procedure TAbonoCollection.SetItem(Index: Integer; Value: TAbonoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TAbonoCollection.New: TAbonoCollectionItem;
begin
  Result := TAbonoCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdePeriodoCollection }

function TIdePeriodoCollection.GetItem(Index: Integer): TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem(inherited Items[Index]);
end;

procedure TIdePeriodoCollection.SetItem(Index: Integer; Value: TIdePeriodoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdePeriodoCollection.New: TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem.Create;
  Self.Add(Result);
end;

{ TAbonoCollectionItem }

constructor TAbonoCollectionItem.Create;
begin
  inherited Create;
end;

destructor TAbonoCollectionItem.Destroy;
begin
  inherited;
end;

{ TIdePeriodoCollectionItem }

constructor TIdePeriodoCollectionItem.Create;
begin
  inherited Create;

  FbaseCalculo := TBaseCalculo.Create;
  FInfoFGTS := nil;
  FBaseMudCateg := nil;
end;

destructor TIdePeriodoCollectionItem.Destroy;
begin
  FreeAndNil(FbaseCalculo);

  if instInfoFGTS() then
    FreeAndNil(FInfoFGTS);
  if instBaseMudCateg() then
    FreeAndNil(FBaseMudCateg);

  inherited;
end;

function TIdePeriodoCollectionItem.getInfoFGTS(): TInfoFGTS;
begin
  if not Assigned(FInfoFGTS) then
    FInfoFGTS := TInfoFGTS.Create;
  Result := FInfoFGTS;
end;

function TIdePeriodoCollectionItem.instInfoFGTS(): boolean;
begin
  Result := Assigned(FInfoFGTS);
end;

function TIdePeriodoCollectionItem.getBaseMudCateg(): TBaseMudCateg;
begin
  if not Assigned(FBaseMudCateg) then
    FBaseMudCateg := TBaseMudCateg.Create;
  Result := FBaseMudCateg;
end;

function TIdePeriodoCollectionItem.instBaseMudCateg(): boolean;
begin
  Result := Assigned(FBaseMudCateg);
end;

{ TInfoVlr }

constructor TInfoVlr.Create;
begin
  inherited Create;

  FAbono := nil;
  FIdePeriodo := nil;
end;

destructor TInfoVlr.Destroy;
begin
  if instAbono() then
    FreeAndNil(FAbono);

  if instIdePeriodo() then
    FreeAndNil(FIdePeriodo);

  inherited;
end;

function TInfoVlr.instAbono(): boolean;
begin
  Result := Assigned(FAbono);
end;

function TInfoVlr.instIdePeriodo(): boolean;
begin
  Result := Assigned(FIdePeriodo);
end;

function TInfoVlr.getAbono(): TAbonoCollection;
begin
  if not Assigned(FAbono) then
    FAbono := TAbonoCollection.Create;
  Result := FAbono;
end;

function TInfoVlr.getIdePeriodo(): TIdePeriodoCollection;
begin
  if not Assigned(FIdePeriodo) then
    FIdePeriodo := TIdePeriodoCollection.Create;
  Result := FIdePeriodo;
end;

{ TBaseCalculo }

constructor TBaseCalculo.Create;
begin
  inherited Create;

  FinfoAgNocivo := nil;
end;

destructor TBaseCalculo.Destroy;
begin
  if instInfoAgNocivo() then
    FreeAndNil(FinfoAgNocivo);

  inherited;
end;

function TBaseCalculo.instInfoAgNocivo(): boolean;
begin
  Result := Assigned(FinfoAgNocivo);
end;

function TBaseCalculo.getInfoAgNocivo(): TinfoAgNocivo;
begin
  if not Assigned(FinfoAgNocivo) then
    FinfoAgNocivo := TinfoAgNocivo.Create;
  Result := FinfoAgNocivo;
end;

{ TEvtProcTrab }

constructor TEvtProcTrab.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregadorS2500.Create;
  FIdeTrab       := TIdeTrab.Create;
  FInfoProcesso  := TInfoProcesso.Create;
end;

destructor TEvtProcTrab.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrab.Free;
  FInfoProcesso.Free;

  inherited;
end;

procedure TEvtProcTrab.GerarIdeResp(obj: TIdeResp);
begin
  if obj.nrInsc <> '' then
  begin
    Gerador.wGrupo('ideResp');

    Gerador.wCampo(tcStr, '', 'tpInsc',  1,  1, 1, eSTpInscricaoToStr(obj.tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 14, 14, 1, obj.nrInsc);

    Gerador.wGrupo('/ideResp');
  end;
end;

procedure TEvtProcTrab.GerarIdeEmpregador(obj: TIdeEmpregadorS2500);
begin
  Gerador.wGrupo('ideEmpregador');

  Gerador.wCampo(tcStr, '', 'tpInsc', 1, 1, 1, eSTpInscricaoToStr(obj.tpInsc));

  if (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador in [tePessoaFisica,
                                                                        teOrgaoPublicoExecutivoFederal, teOrgaoPublicoLegislativoFederal,
                                                                        teOrgaoPublicoJudiciarioFederal, teOrgaoPublicoAutonomoFederal]) then
    Gerador.wCampo(tcStr, '', 'nrInsc', 14, 14, 1, obj.nrInsc)
  else
    Gerador.wCampo(tcStr, '', 'nrInsc',  8,  8, 1, Copy(obj.nrInsc, 1, 8));

  GerarIdeResp(obj.IdeResp);

  Gerador.wGrupo('/ideEmpregador');
end;

procedure TEvtProcTrab.GerarInfoProcJud(Obj: TInfoProcJud);
begin
  Gerador.wGrupo('infoProcJud');

  Gerador.wCampo(tcDat, '', 'dtSent',   10, 10, 0, obj.dtSent);
  Gerador.wCampo(tcStr, '', 'ufVara',    2,  2, 0, obj.ufVara);
  Gerador.wCampo(tcInt, '', 'codMunic',  7,  7, 1, obj.codMunic);
  Gerador.wCampo(tcInt, '', 'idVara',    4,  4, 1, obj.idVara);

  Gerador.wGrupo('/infoProcJud');
end;

procedure TEvtProcTrab.GerarInfoCCP(obj: TInfoCCP);
begin
  Gerador.wGrupo('infoCCP');

  Gerador.wCampo(tcDat, '', 'dtCCP',   10, 10, 0, obj.dtCCP);
  Gerador.wCampo(tcStr, '', 'tpCCP',    1,  1, 1, eSTpTpCCPToStr(obj.tpCCP));
  Gerador.wCampo(tcStr, '', 'cnpjCCP', 14, 14, 0, obj.cnpjCCP);

  Gerador.wGrupo('/infoCCP');
end;

procedure TEvtProcTrab.GerarDadosCompl(obj: TdadosCompl);
begin
  Gerador.wGrupo('dadosCompl');

  if obj.instInfoProcJud() then
    GerarInfoProcJud(obj.infoProcJud);

  if obj.instInfoCCP() then
    GerarInfoCCP(obj.infoCCP);

  Gerador.wGrupo('/dadosCompl');
end;

procedure TEvtProcTrab.GerarInfoProcesso(obj: TinfoProcesso);
begin
  Gerador.wGrupo('infoProcesso');

  Gerador.wCampo(tcStr, '', 'origem'     ,  1,   1, 1, eSTpTpOrigemProcToStr(obj.origem));
  Gerador.wCampo(tcStr, '', 'nrProcTrab' , 15,  15, 1, obj.nrProcTrab);
  Gerador.wCampo(tcStr, '', 'obsProcTrab',  0, 900, 0, obj.obsProcTrab);

  GerarDadosCompl(obj.dadosCompl);

  Gerador.wGrupo('/infoProcesso');
end;

procedure TEvtProcTrab.GerarDependente(obj: TDependenteCollectionS2500);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('dependente');

    Gerador.wCampo(tcStr, '', 'cpfDep',  11, 11, 0, obj.Items[i].cpfDep);
    Gerador.wCampo(tcStr, '', 'tpDep',    1,  2, 1, eStpDepToStr(obj.Items[i].tpDep));
    Gerador.wCampo(tcStr, '', 'descDep',  0, 30, 0, obj.Items[i].descDep);

    Gerador.wGrupo('/dependente');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'dependente', 'Lista de Dependentes', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtProcTrab.GerarRemuneracao(obj: TRemuneracaoCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('remuneracao');

    Gerador.wCampo(tcDat, '', 'dtRemun',    10, 10, 0, obj.Items[i].dtRemun);
    Gerador.wCampo(tcDe2, '', 'vrSalFx',     1, 14, 1, obj.Items[i].vrSalFx);
    Gerador.wCampo(tcStr, '', 'undSalFixo',  1,  1, 1, eSUndSalFixoToStr(obj.Items[i].undSalFixo));

    if (obj.Items[i].undSalFixo = sfPorTarefa) or (obj.Items[i].undSalFixo = sfNaoaplicavel) then
      Gerador.wCampo(tcStr, '', 'dscSalVar', 0, 255, 0, obj.Items[i].dscSalVar);

    Gerador.wGrupo('/remuneracao');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'remuneracao', 'Informações da remuneração e periodicidade de pagamento', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtProcTrab.GerarSucessaoVinc(obj: TsucessaoVinc);
begin
  if obj.nrInsc = '' then
    exit;

  Gerador.wGrupo('sucessaoVinc');

  Gerador.wCampo(tcStr, '', 'tpInsc',      1,   1, 1, eSTpInscricaoToStr(obj.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',     14,  14, 1, obj.nrInsc);
  Gerador.wCampo(tcStr, '', 'matricAnt',   0,  30, 0, obj.matricAnt);
  Gerador.wCampo(tcDat, '', 'dtTransf',   10,  10, 1, obj.dtTransf);

  Gerador.wGrupo('/sucessaoVinc');
end;

procedure TEvtProcTrab.GerarInfoDeslig(obj: TinfoDeslig);
begin
  Gerador.wGrupo('infoDeslig');

  Gerador.wCampo(tcDat, '', 'dtDeslig',       10, 10, 0, obj.dtDeslig);
  Gerador.wCampo(tcStr, '', 'mtvDeslig',       1,  2, 1, obj.mtvDeslig);

  if obj.dtProjFimAPI > 0 then
    Gerador.wCampo(tcDat, '', 'dtProjFimAPI', 10, 10, 0, obj.dtProjFimAPI);

  if VersaoDF > veS01_01_00 then
  begin
    Gerador.wCampo(tcStr, '', 'pensAlim',    1,  1, 0, eSTpPensaoAlimToStr(obj.pensAlim));
    Gerador.wCampo(tcDe2, '', 'percAliment', 1,  5, 0, obj.percAliment);
    Gerador.wCampo(tcDe2, '', 'vrAlim',      1, 14, 0, obj.vrAlim);
  end;

  Gerador.wGrupo('/infoDeslig');
end;

procedure TEvtProcTrab.GerarInfoVinc(obj: TinfoVinc);
begin
  if (obj.tpRegTrab <> trNenhum) or
     (obj.instDuracao()) or
     (obj.instObservacoes()) or
     (obj.instSucessaoVinc()) or
     (obj.instInfoDeslig())
  then
  begin
    Gerador.wGrupo('infoVinc');
    if obj.tpRegTrab <> trNenhum then
      Gerador.wCampo(tcStr, '', 'tpRegTrab',  1,  1, 1, eSTpRegTrabToStr(obj.tpRegTrab));
    if obj.tpRegPrev <> rpNenhum then
      Gerador.wCampo(tcStr, '', 'tpRegPrev',  1,  1, 1, eSTpRegPrevToStr(obj.tpRegPrev));
    if obj.dtAdm > 0 then
      Gerador.wCampo(tcDat, '', 'dtAdm',     10, 10, 0, obj.dtAdm);
    if obj.tmpParc <> tpNenhum then
      Gerador.wCampo(tcStr, '', 'tmpParc',    1,  1, 1, tpTmpParcToStr(obj.tmpParc));

    if obj.instDuracao() then
      GerarDuracao(obj.duracao, StrToInt(eSTpRegTrabToStr(obj.tpRegTrab)));

    if obj.instObservacoes() then
      GerarObservacoes(obj.observacoes);

    if obj.instSucessaoVinc() then
      GerarSucessaoVinc(obj.sucessaoVinc);

    if obj.instInfoDeslig() then
      GerarInfoDeslig(obj.infoDeslig);

    Gerador.wGrupo('/infoVinc');
  end;
end;

procedure TEvtProcTrab.GerarInfoTerm(obj: TinfoTerm);
begin
  Gerador.wGrupo('infoTerm');

  Gerador.wCampo(tcDat, '', 'dtTerm', 10, 10, 0, obj.dtTerm);

  if obj.mtvDesligTSV <> '' then
    Gerador.wCampo(tcStr, '', 'mtvDesligTSV', 2, 2, 1, obj.mtvDesligTSV);

  Gerador.wGrupo('/infoTerm');
end;

procedure TEvtProcTrab.GerarInfoCompl(obj: TinfoCompl);
begin
  if (obj.codCBO <> '') or
     (obj.natAtividade <> navNaoInformar) or
     (obj.instRemuneracao()) or
     (obj.InfoVinc.tpRegTrab <> trNenhum) or
     (obj.InfoVinc.FtpRegPrev <> rpNenhum) or
     (obj.InfoVinc.FdtAdm > 0) or
     (obj.InfoVinc.FtmpParc <> tpNenhum) or
     (obj.infoVinc.instDuracao()) or
     (obj.infoVinc.instObservacoes()) or
     (obj.infoVinc.instSucessaoVinc()) or
     (obj.infoVinc.instInfoDeslig()) or
     (obj.instInfoTerm())
  then
  begin
    Gerador.wGrupo('infoCompl');

    if obj.codCBO <> '' then
      Gerador.wCampo(tcStr, '', 'codCBO',    1,  6, 1, obj.codCBO);

    if obj.natAtividade <> navNaoInformar then
      Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 1, eSNatAtividadeToStr(obj.natAtividade));

    if obj.instRemuneracao() then
      GerarRemuneracao(obj.remuneracao);

    if obj.instInfoVinc() then
      GerarInfoVinc(obj.infoVinc);

    if obj.instInfoTerm() then
      GerarInfoTerm(obj.infoTerm);

    Gerador.wGrupo('/infoCompl');
  end;
end;

procedure TEvtProcTrab.GerarMudCategAtiv(obj: TMudCategAtivCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('mudCategAtiv');

    Gerador.wCampo(tcInt, '', 'codCateg',    3,  3, 1, obj.Items[i].codCateg);

    if obj.Items[i].natAtividade <> navNaoInformar then
      Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 1, eSNatAtividadeToStr(obj.Items[i].natAtividade));

    Gerador.wCampo(tcDat, '', 'dtMudCategAtiv', 10, 10, 0, obj.Items[i].dtMudCategAtiv);

    Gerador.wGrupo('/mudCategAtiv');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'mudCategAtiv', 'Informação do novo código de categoria e/ou da nova natureza da atividade', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtProcTrab.GerarUnicContr(obj: TUnicContrCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('unicContr');

    Gerador.wCampo(tcStr, '', 'matUnic',    1, 30, 1, obj.Items[i].matUnic);

    if obj.Items[i].codCateg > 0 then
      Gerador.wCampo(tcInt, '', 'codCateg', 3,  3, 1, obj.Items[i].codCateg);

    Gerador.wCampo(tcDat, '', 'dtInicio',  10, 10, 0, obj.Items[i].dtInicio);

    Gerador.wGrupo('/unicContr');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'unicContr', 'Informações dos vínculos/contratos incorporados', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtProcTrab.GerarInfoContr(obj: TinfoContrCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('infoContr');

    Gerador.wCampo(tcStr, '', 'tpContr',       1,  1, 1, eSTpContrS2500ToStr(obj.Items[i].tpContr));
    Gerador.wCampo(tcStr, '', 'indContr',      1,  1, 1, eSSimNaoToStr(obj.Items[i].indContr));
    Gerador.wCampo(tcDat, '', 'dtAdmOrig',    10, 10, 0, obj.Items[i].dtAdmOrig);

    if obj.Items[i].indReint <> snfNada then
      Gerador.wCampo(tcStr, '', 'indReint',    1,  1, 1, eSSimNaoFacultativoToStr(obj.Items[i].indReint));

    Gerador.wCampo(tcStr, '', 'indCateg',      1,  1, 1, eSSimNaoToStr(obj.Items[i].indCateg));
    Gerador.wCampo(tcStr, '', 'indNatAtiv',    1,  1, 1, eSSimNaoToStr(obj.Items[i].indNatAtiv));
    Gerador.wCampo(tcStr, '', 'indMotDeslig',  1,  1, 1, eSSimNaoToStr(obj.Items[i].indMotDeslig));

    if (obj.Items[i].indUnic <> snfNada) and (VersaoDF <= veS01_01_00) then
      Gerador.wCampo(tcStr, '', 'indUnic',     1,  1, 1, eSSimNaoFacultativoToStr(obj.Items[i].indUnic));

    Gerador.wCampo(tcStr, '', 'matricula',     0, 30, 0, obj.Items[i].matricula);

    if obj.Items[i].codCateg > 0 then
      Gerador.wCampo(tcInt, '', 'codCateg',    3,  3, 1, obj.Items[i].codCateg);

    Gerador.wCampo(tcDat, '', 'dtInicio',     10, 10, 0, obj.Items[i].dtInicio);

    if obj.Items[i].instInfoCompl() then
      GerarInfoCompl(obj.Items[i].infoCompl);

    if obj.Items[i].instMudCategAtiv() then
      GerarMudCategAtiv(obj.Items[i].mudCategAtiv);

    if obj.Items[i].instUnicContr() then
      GerarUnicContr(obj.Items[i].unicContr);

    GerarIdeEstab(obj.Items[i].ideEstab);

    Gerador.wGrupo('/infoContr');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'infoContr', 'Informações do contrato de trabalho', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtProcTrab.GerarIdeEstab(obj: TIdeEstab);
begin
  Gerador.wGrupo('ideEstab');

  Gerador.wCampo(tcStr, '', 'tpInsc',      1,   1, 1, eSTpInscricaoToStr(obj.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',     14,  14, 1, obj.nrInsc);

  GerarInfoVlr(obj.infoVlr);

  Gerador.wGrupo('/ideEstab');
end;

procedure TEvtProcTrab.GerarInfoVlr(Obj: TInfoVlr);
begin
  Gerador.wGrupo('infoVlr');

  Gerador.wCampo(tcStr, '', 'compIni',         7,  7, 1, obj.compIni);
  Gerador.wCampo(tcStr, '', 'compFim',         7,  7, 1, obj.compFim);

  if VersaoDF > veS01_01_00 then
  begin
    Gerador.wCampo(tcStr, '', 'indReperc',       1,  1, 1, eSTpTpIndRepercToStr(obj.indReperc));
    Gerador.wCampo(tcStr, '', 'indenSD',         0,  1, 0, eSSimFacultativoToStr(obj.indenSD));
    Gerador.wCampo(tcStr, '', 'indenAbono',      0,  1, 0, eSSimFacultativoToStr(obj.indenAbono));
  end
  else
  begin
    Gerador.wCampo(tcStr, '', 'repercProc',      1,  1, 1, eSTpTpRepercProcToStr(obj.repercProc));
    Gerador.wCampo(tcDe2, '', 'vrRemun',         1, 14, 1, obj.vrRemun);
    Gerador.wCampo(tcDe2, '', 'vrAPI',           1, 14, 1, obj.vrAPI);
    Gerador.wCampo(tcDe2, '', 'vr13API',         1, 14, 1, obj.vr13API);
    Gerador.wCampo(tcDe2, '', 'vrInden',         1, 14, 1, obj.vrInden);
    Gerador.wCampo(tcDe2, '', 'vrBaseIndenFGTS', 0, 14, 0, obj.vrBaseIndenFGTS);
    Gerador.wCampo(tcStr, '', 'pagDiretoResc',   0,  1, 0, eSSimNaoFacultativoToStr(obj.pagDiretoResc));
  end;

  if ((obj.instAbono()) and (VersaoDF > veS01_01_00)) then
    GerarAbono(obj.abono);

  if obj.instIdePeriodo() then
    GerarIdePeriodo(obj.idePeriodo);

  Gerador.wGrupo('/infoVlr');
end;

procedure TEvtProcTrab.GerarAbono(Obj: TAbonoCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('abono');

    Gerador.wCampo(tcStr, '', 'anoBase', 4, 4, 1, obj.Items[i].anoBase);

    Gerador.wGrupo('/abono');
  end;

  if obj.Count > 9 then
    Gerador.wAlerta('', 'abono', 'Identificação do(s) ano(s)-base em que houve indenização substitutiva de abono salarial',
                    ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TEvtProcTrab.GerarBaseCalculo(obj: TbaseCalculo);
var
  NrOcorrBcCp: Integer;
begin
  if ( (VersaoDF >= veS01_02_00) and (obj.vrBcCpMensal = 0) and (obj.vrBcCp13 = 0)) then
    exit;

  if (VersaoDF >= veS01_02_00) then
    NrOcorrBcCp := 0
  else
    NrOcorrBcCp := 1;

  Gerador.wGrupo('baseCalculo');

  Gerador.wCampo(tcDe2, '', 'vrBcCpMensal',    1, 14, 1, obj.vrBcCpMensal);
  Gerador.wCampo(tcDe2, '', 'vrBcCp13',        1, 14, NrOcorrBcCp, obj.vrBcCp13);
  if VersaoDF <= veS01_01_00 then
  begin
    Gerador.wCampo(tcDe2, '', 'vrBcFgts',        1, 14, 1, obj.vrBcFgts);
    Gerador.wCampo(tcDe2, '', 'vrBcFgts13',      1, 14, 1, obj.vrBcFgts13);
  end;

  if obj.instInfoAgNocivo() then
    GerarInfoAgNocivo(obj.infoAgNocivo);

  Gerador.wGrupo('/baseCalculo');
end;

procedure TEvtProcTrab.GerarInfoFGTS(obj: TinfoFGTS);
begin
  Gerador.wGrupo('infoFGTS');

  if VersaoDF > veS01_01_00 then
  begin
    Gerador.wCampo(tcDe2, '', 'vrBcFGTSProcTrab', 1, 14, 1, obj.vrBcFGTSProcTrab);
    Gerador.wCampo(tcDe2, '', 'vrBcFGTSSefip',    1, 14, 0, obj.vrBcFGTSSefip);
    Gerador.wCampo(tcDe2, '', 'vrBcFGTSDecAnt',   1, 14, 0, obj.vrBcFGTSDecAnt);
  end
  else
  begin
    Gerador.wCampo(tcDe2, '', 'vrBcFgtsGuia',     1, 14, 1, obj.vrBcFgtsGuia);
    Gerador.wCampo(tcDe2, '', 'vrBcFgts13Guia',   1, 14, 1, obj.vrBcFgts13Guia);
    Gerador.wCampo(tcStr, '', 'pagDireto',        1,  1, 1, eSSimNaoToStr(obj.pagDireto));
  end;

  Gerador.wGrupo('/infoFGTS');
end;

procedure TEvtProcTrab.GerarBaseMudCateg(obj: TbaseMudCateg);
begin
  Gerador.wGrupo('baseMudCateg');

  Gerador.wCampo(tcInt, '', 'codCateg',        3,  3, 1, obj.codCateg);
  Gerador.wCampo(tcDe2, '', 'vrBcCPrev',       1, 14, 1, obj.vrBcCPrev);

  Gerador.wGrupo('/baseMudCateg');
end;

procedure TEvtProcTrab.GerarIdePeriodo(obj: TIdePeriodoCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('idePeriodo');

    Gerador.wCampo(tcStr, '', 'perRef', 7, 7, 1, obj.Items[i].perRef);

    GerarBaseCalculo(obj.Items[i].baseCalculo);

    if obj.Items[i].instInfoFGTS() then
      GerarInfoFGTS(obj.Items[i].infoFGTS);

    if obj.Items[i].instBaseMudCateg() then
      GerarBaseMudCateg(obj.Items[i].baseMudCateg);

    Gerador.wGrupo('/idePeriodo');
  end;

  if obj.Count > 999 then
    Gerador.wAlerta('', 'idePeriodo', 'Identificação do período ao qual se referem as bases de cálculo',
                    ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtProcTrab.GerarIdeTrab(Obj: TideTrab);
begin
  Gerador.wGrupo('ideTrab');

  Gerador.wCampo(tcStr, '', 'cpfTrab',  11, 11, 1, obj.cpfTrab);
  Gerador.wCampo(tcStr, '', 'nmTrab' ,   0, 70, 1, obj.nmTrab);
  Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 0, obj.dtNascto);

  if obj.instDependenteS2500() and (VersaoDF <= veS01_01_00) then
    GerarDependente(obj.dependente);

  if obj.instInfoContr () then
    GerarInfoContr(obj.infoContr);

  Gerador.wGrupo('/ideTrab');
end;

function TEvtProcTrab.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(Now, Self.ideEmpregador.NrInsc, Self.Sequencial);

    GerarCabecalho('evtProcTrab');
    Gerador.wGrupo('evtProcTrab Id="' + Self.Id + '"');

    GerarIdeEvento2(Self.ideEvento);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarInfoProcesso(Self.infoProcesso);
    GerarIdeTrab(Self.ideTrab);

    Gerador.wGrupo('/evtProcTrab');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtProcTrab');

//    Validar(schevtProcTrab);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtProcTrab.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtProcTrab';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi  := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideResp';
      ideEmpregador.ideResp.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.ideResp.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoProcesso';
      infoProcesso.origem      := eSStrToTpOrigemProc(Ok, INIRec.ReadString(sSecao, 'origem', '0'));
      infoProcesso.nrProcTrab  := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      infoProcesso.obsProcTrab := INIRec.ReadString(sSecao, 'obsProcTrab', EmptyStr);

      sSecao := 'infoProcJud';
      infoProcesso.dadosCompl.infoProcJud.dtSent   := StringToDateTime(INIRec.ReadString(sSecao, 'dtSent', '0'));
      infoProcesso.dadosCompl.infoProcJud.ufVara   := INIRec.ReadString(sSecao, 'ufVara', EmptyStr);
      infoProcesso.dadosCompl.infoProcJud.codMunic := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      infoProcesso.dadosCompl.infoProcJud.idVara   := INIRec.ReadInteger(sSecao, 'idVara', 0);

      sSecao := 'infoCCP';
      sFim := INIRec.ReadString(sSecao, 'dtCCP', '');

      if sFim <> '' then
      begin
        infoProcesso.dadosCompl.infoCCP.dtCCP   := StringToDateTime(sFim);
        infoProcesso.dadosCompl.infoCCP.tpCCP   := eSStrToTpTpCCP(Ok, INIRec.ReadString(sSecao, 'tpCCP', EmptyStr));
        infoProcesso.dadosCompl.infoCCP.cnpjCCP := INIRec.ReadString(sSecao, 'cnpjCCP', EmptyStr);
      end;

      sSecao := 'ideTrab';
      ideTrab.cpfTrab  := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrab.nmTrab   := INIRec.ReadString(sSecao, 'nmTrab', EmptyStr);
      ideTrab.dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'dependente' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrab.dependente.New do
        begin
          cpfDep  := sFim;
          tpDep   := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', EmptyStr));
          descDep := INIRec.ReadString(sSecao, 'descDep', EmptyStr);
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'infoContr' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'indContr', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrab.infoContr.New do
        begin
          tpContr      := eSStrToTpContrS2500(Ok, INIRec.ReadString(sSecao, 'tpContr', EmptyStr));
          indContr     := eSStrToSimNao(Ok, sFim);
          dtAdmOrig    := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdmOrig', '0'));
          indReint     := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indReint', EmptyStr));
          indCateg     := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indCateg', EmptyStr));
          indNatAtiv   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indNatAtiv', EmptyStr));
          indMotDeslig := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indMotDeslig', EmptyStr));
          indUnic      := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indUnic', EmptyStr));
          matricula    := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
          codCateg     := INIRec.ReadInteger(sSecao, 'codCateg', 0);
          dtInicio     := StringToDateTime(INIRec.ReadString(sSecao, 'dtInicio', '0'));

          sSecao := 'infoCompl' + IntToStrZero(I, 2);
          infoCompl.codCBO := INIRec.ReadString(sSecao, 'codCBO', EmptyStr);
          infoCompl.natAtividade := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', EmptyStr));

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'remuneracao' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'dtRemun', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoCompl.remuneracao.New do
            begin
              dtRemun    := StringToDateTime(INIRec.ReadString(sSecao, 'dtRemun', '0'));
              vrSalFx    := StrToFloat(INIRec.ReadString(sSecao, 'vrSalFx', EmptyStr));
              undSalFixo := eSStrToUndSalFixo(Ok, INIRec.ReadString(sSecao, 'undSalFixo', EmptyStr));
              dscSalVar  := INIRec.ReadString(sSecao, 'dscSalVar', EmptyStr);

            end;

            Inc(J);
          end;

          sSecao := 'infovinc' + IntToStrZero(I, 2);
          infoCompl.infoVinc.tpRegTrab := eSStrToTpRegTrab(Ok, INIRec.ReadString(sSecao, 'tpRegTrab', EmptyStr));
          infoCompl.infoVinc.tpRegPrev := eSStrTotpRegPrev(Ok, INIRec.ReadString(sSecao, 'tpRegPrev', EmptyStr));
          infoCompl.infoVinc.dtAdm     := StringToDateTime(INIRec.ReadString(sSecao, 'dtAdm', '0'));
          infoCompl.infoVinc.tmpParc   := StrTotpTmpParc(Ok, INIRec.ReadString(sSecao, 'tmpParc', EmptyStr));

          sSecao := 'duracao' + IntToStrZero(I, 2);
          infoCompl.infoVinc.duracao.TpContr   := eSStrToTpContr(Ok, INIRec.ReadString(sSecao, 'TpContr', EmptyStr));
          infoCompl.infoVinc.duracao.dtTerm    := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
          infoCompl.infoVinc.duracao.clauAssec := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'clauAssec', EmptyStr));
          infoCompl.infoVinc.duracao.objDet    := INIRec.ReadString(sSecao, 'objDet', EmptyStr);

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'observacoes' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'observacao', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoCompl.infoVinc.observacoes.New do
            begin
              observacao := INIRec.ReadString(sSecao, 'observacao', EmptyStr);
            end;

            Inc(J);
          end;

          sSecao := 'sucessaoVinc' + IntToStrZero(I, 2);
          infoCompl.infoVinc.sucessaoVinc.tpInsc    := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', EmptyStr));
          infoCompl.infoVinc.sucessaoVinc.nrInsc    := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
          infoCompl.infoVinc.sucessaoVinc.matricAnt := INIRec.ReadString(sSecao, 'matricAnt', EmptyStr);
          infoCompl.infoVinc.sucessaoVinc.dtTransf  := StringToDateTime(INIRec.ReadString(sSecao, 'dtTransf', '0'));

          sSecao := 'infoDeslig' + IntToStrZero(I, 2);
          infoCompl.infoVinc.infoDeslig.dtDeslig     := StringToDateTime(INIRec.ReadString(sSecao, 'dtDeslig', '0'));
          infoCompl.infoVinc.infoDeslig.mtvDeslig    := INIRec.ReadString(sSecao, 'mtvDeslig', EmptyStr);
          infoCompl.infoVinc.infoDeslig.dtProjFimAPI := StringToDateTime(INIRec.ReadString(sSecao, 'dtProjFimAPI', '0'));

          if VersaoDF >= veS01_02_00 then
          begin
            infoCompl.infoVinc.infoDeslig.pensAlim     := eSStrToTpPensaoAlim(Ok, INIRec.ReadString(sSecao, 'pensAlim', EmptyStr));
            infoCompl.infoVinc.infoDeslig.percAliment  := StringToFloatDef(INIRec.ReadString(sSecao, 'percAliment', ''), 0);
            infoCompl.infoVinc.infoDeslig.vrAlim       := StringToFloatDef(INIRec.ReadString(sSecao, 'vrAlim', ''), 0);
          end;

          sSecao := 'infoTerm' + IntToStrZero(I, 2);
          infoCompl.infoTerm.dtTerm       := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
          infoCompl.infoTerm.mtvDesligTSV := INIRec.ReadString(sSecao, 'mtvDesligTSV', EmptyStr);

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'mudCategAtiv' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codCateg', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with mudCategAtiv.New do
            begin
              codCateg       := StrToInt(sFIM);
              natAtividade   := eSStrToNatAtividade(Ok, INIRec.ReadString(sSecao, 'natAtividade', EmptyStr));
              dtMudCategAtiv := StringToDateTime(INIRec.ReadString(sSecao, 'dtMudCategAtiv', '0'));

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'unicContr' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'codCateg', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with unicContr.New do
            begin
              matUnic  := INIRec.ReadString(sSecao, 'matUnic', EmptyStr);
              codCateg := StrToInt(sFIM);
              dtInicio := StringToDateTime(INIRec.ReadString(sSecao, 'dtInicio', '0'));

            end;

            Inc(J);
          end;

          sSecao := 'ideEstab' + IntToStrZero(I, 2);
          ideEstab.TpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          ideEstab.NrInsc := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

          sSecao := 'infoVlr' + IntToStrZero(I, 2);
          ideEstab.infoVlr.compIni         := INIRec.ReadString(sSecao, 'compIni', EmptyStr);
          ideEstab.infoVlr.compFim         := INIRec.ReadString(sSecao, 'compFim', EmptyStr);
          ideEstab.infoVlr.repercProc      := eSStrToTpRepercProc(Ok, INIRec.ReadString(sSecao, 'compFim', EmptyStr));
          ideEstab.infoVlr.vrRemun         := StrToFloat(INIRec.ReadString(sSecao, 'vrRemun', EmptyStr));
          ideEstab.infoVlr.vrAPI           := StrToFloat(INIRec.ReadString(sSecao, 'vrAPI', EmptyStr));
          ideEstab.infoVlr.vr13API         := StrToFloat(INIRec.ReadString(sSecao, 'vr13API', EmptyStr));
          ideEstab.infoVlr.vrInden         := StrToFloat(INIRec.ReadString(sSecao, 'vrInden', EmptyStr));
          ideEstab.infoVlr.vrBaseIndenFGTS := StrToFloat(INIRec.ReadString(sSecao, 'vrBaseIndenFGTS', EmptyStr));
          ideEstab.infoVlr.pagDiretoResc   := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'pagDiretoResc', EmptyStr));

          J := 1;
          while true do
          begin
            // de 00 até 09
            sSecao := 'abono' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'anoBase', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideEstab.infoVlr.abono.New do
            begin
              anoBase := sFIM;
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 00 até 999
            sSecao := 'idePeriodo' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideEstab.infoVlr.idePeriodo.New do
            begin
              perRef := sFIM;

              sSecao := 'baseCalculo' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              baseCalculo.vrBcCpMensal := StringToFloat(INIRec.ReadString(sSecao, 'vrBcCpMensal', '0'));
              baseCalculo.vrBcCp13     := StringToFloat(INIRec.ReadString(sSecao, 'vrBcCp13', '0'));
              baseCalculo.vrBcFgts     := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFgts', '0'));
              baseCalculo.vrBcFgts13   := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFgts13', '0'));

              sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              baseCalculo.infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', EmptyStr));

              sSecao := 'infoFGTS' + IntToStrZero(I, 2) + IntToStrZero(J, 3);

              if VersaoDF >= veS01_02_00 then
              begin
                infoFGTS.vrBcFGTSProcTrab := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFGTSProcTrab', '0'));
                infoFGTS.vrBcFGTSSefip    := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFGTSSefip', '0'));
                infoFGTS.vrBcFGTSDecAnt   := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFGTSDecAnt', '0'));
              end
              else
              begin
                infoFGTS.vrBcFgtsGuia     := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFgtsGuia', '0'));
                infoFGTS.vrBcFgts13Guia   := StringToFloat(INIRec.ReadString(sSecao, 'vrBcFgts13Guia', '0'));
                infoFGTS.pagDireto        := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'pagDireto', EmptyStr));
              end;

              sSecao := 'baseMudCateg' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              baseMudCateg.codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);
              baseMudCateg.vrBcCPrev := StringToFloat(INIRec.ReadString(sSecao, 'vrBcCPrev', '0'));

            end;

            Inc(J);
          end;


        end;

        Inc(I);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;


end.
