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
|* 01/03/2016: Guilherme Costa
|*  - Alterações para validação com o XSD
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}

unit pcesCommon;

interface

uses
  SysUtils, Classes, Controls,
  pcesConversaoeSocial;

const
  dDataBrancoNula = '30/12/1899';

type
  {Classes existentes nesta unit}
  TeSocial = class;
  TAliqGilRat = class;
  TAlvaraJudicial = class;
  TAposentadoria = class;
  TBrasil = class;
  TCNH = class;
  TContato = class;
  TContatoTrabalhador = class;
  TInfoContrato = class;
  TCTPS = class;
  TDependenteCollection = class;
  TDependenteCollectionItem = class;
  TDescAtividadeCollection = class;
  TDescAtividadeCollectionItem = class;
  TDocumentos = class;
  TDuracao = class;
  TEndereco = class;
  TEpiCollection = class;
  TEpiCollectionItem = class;
  TExterior = class;
  TFGTS = class;
  TFiliacaoSindical = class;
  THorarioCollection = class;
  THorarioCollectionItem = class;
  THorarioIntervaloCollectionItem = class;
  THorarioIntervaloCollection = class;
  THorContratual = class;
  TIdeEmpregador = class;
  TIdeEvento = class;
  TIdePeriodo = class;
  TIdeEstabVinc = class;
  TIdeTomadorServ = class;
  TIdeTrabSubstituidoCollection = class;
  TIdeTrabSubstituidoCollectionItem = class;
  TIdeVinculo = class;
  TInfoAtivDesemp = class;
  TInfoDeficiencia = class;
  TLocalTrabalho = class;
  TNascimento = class;
  TProcesso = class;
  TProcAdmJudFap = class;
  TProcAdmJudRat = class;
  TRemuneracao = class;
  TRG = class;
  TRNE = class;
  TSucessaoVinc = class;
  TOC = class;
  TRIC = class;
  TTrabalhador = class;
  TTrabEstrangeiro = class;
  TTrabTemporario = class;
  TVinculo = class;
  TideTrabalhador = class;
  TideTrabalhador2 = class;
  TideTrabSemVinc = class;
  TIdeFolhaPagto = class;
  TEmitente = class;
  TEndExt = class;
  TIdePais = class;
  TInfoAgNocivo = class;
  TRubricaCollectionItem = class;
  TRubricaCollection = class;
  TRecPgtosCollectionItem = class;
  TRecPgtosCollection = class;
  TInfoASO = class;
  TLocalTrabGeral = class;
  TLocalTrabDom = class;
  TInfoCeletista = class;
  TInfoEstatutario = class;
  TInfoRegimeTrab = class;
  TAfastamento = class;
  TDesligamento = class;
  TInfoAmbCollection = class;
  TInfoAmbItem = class;
  TInfoAtiv = class;
  TFatRiscoCollection = class;
  TFatRiscoItem = class;
  TcargoFuncao = class;
  TinfoEstagiario = class;
  TinstEnsino = class;
  TageIntegracao = class;
  TsupervisorEstagio = class;
  TVerbasResc = class;
  TQuarentena = class;
  TInfoProcJudCollection = class;
  TInfoProcJudItem = class;
  TideEstabLotCollection = class;
  TideEstabLotItem = class;
  TinfoSimples = class;
  TdetVerbasCollection = class;
  TdetVerbasItem = class;
  TProcJudTrabCollectionItem = class;
  TProcJudTrabCollection = class;
  TPensaoAlimCollectionItem = class;
  TPensaoAlimCollection = class;
  TDetPlanoCollectionItem = class;
  TDetPlanoCollection = class;
  TDetOperCollectionItem = class;
  TDetOperCollection = class;
  TInfoSaudeColet = class;
  TRemunPerCollectionItem = class;
  TNfsItem = class;
  TNfsColecao = class;
  TFiliacaoSindicalItem = class;
  TEpcEpi = class;
  TEpcCollection = class;
  TEpcCollectionItem = class;
  TRemunOutrEmprCollectionItem = class;
  TRemunOutrEmprCollection = class;
  TInfoMV = class;
  TIdeRespInf = class;
  TObservacoesCollectionItem = class;
  TObservacoesCollection = class;
  TtransfDom = class;
  IEventoeSocial = Interface;

  TeSocial = class(TPersistent)
  private
    FId: string;
    FSequencial: Integer;
  published
    property Id: string read FId write FId;
    property Sequencial: Integer read FSequencial write FSequencial;
  end;

  TIdeFolhaPagto = class(TPersistent)
  private
    FindApuracao: tpIndApuracao;
    FperApur: string;
  published
    property indApuracao: tpIndApuracao read FindApuracao write FindApuracao;
    property perApur: string read FperApur write FperApur;
  end;

  TeSocialCollection = class(TCollection)

  end;

  TAliqGilRat = class(TPersistent)
  private
    FAliqRat: tpAliqRat;
    FFap: Double;
    FAliqRatAjust: Double;
    FProcAdmJudRat: TProcAdmJudRat;
    FProcAdmJudFap: TProcAdmJudFap;
    function getProcAdmJudRat(): TProcAdmJudRat;
    function getProcAdmJudFat(): TProcAdmJudFap;
  public
    constructor Create;
    destructor Destroy; override;
    function procAdmJudRatInst(): Boolean;
    function procAdmJudFapInst(): Boolean;

    property AliqRat: tpAliqRat read FAliqRat write FAliqRat;
    property Fap: Double read FFap write FFap;
    property AliqRatAjust: Double read FAliqRatAjust write FAliqRatAjust;
    property ProcAdmJudRat: TProcAdmJudRat read getProcAdmJudRat write FProcAdmJudRat;
    property ProcAdmJudFap: TProcAdmJudFap read getProcAdmJudFat write FProcAdmJudFap;
  end;

  TAlvaraJudicial = class
  private
    FNrProcJud: string;
  public
    property NrProcJud: string read FNrProcJud write FNrProcJud;
  end;

  TAposentadoria = class
  private
    FTrabAposent: tpSimNao;
  public
    property TrabAposent: tpSimNao read FTrabAposent write FTrabAposent;
  end;

  TBrasil = class
  private
    FTpLograd: string;
    FDscLograd: string;
    FNrLograd: string;
    FComplemento: string;
    FBairro: string;
    FCep: string;
    FCodMunic: integer;
    FUF: tpuf;
  public
    property TpLograd: string read FTpLograd write FTpLograd;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property CodMunic: integer read FCodMunic write FCodMunic;
    property UF: tpuf read FUF write FUF;
  end;

  TCNH = class
  private
    FnrRegCnh: string;
    FDtExped: TDateTime;
    FufCnh: tpuf;
    FDtValid: TDateTime;
    FdtPriHab: TDateTime;
    FcategoriaCnh: tpCnh;
  public
    property nrRegCnh: string read FnrRegCnh write FnrRegCnh;
    property DtExped: TDateTime read FDtExped write FDtExped;
    property ufCnh: tpuf read FufCnh write FufCnh;
    property DtValid: TDateTime read FDtValid write FDtValid;
    property dtPriHab: TDateTime read FdtPriHab write FdtPriHab;
    property categoriaCnh: tpCnh read FcategoriaCnh write FcategoriaCnh;
  end;

  TContato = class(TPersistent)
  private
    FNmCtt: string;
    FCpfCtt: string;
    FFoneFixo: string;
    FFoneCel: string;
    FEmail: string;
  public
    property NmCtt: string read FNmCtt write FNmCtt;
    property CpfCtt: string read FCpfCtt write FCpfCtt;
    property FoneFixo: string read FFoneFixo write FFoneFixo;
    property FoneCel: string read FFoneCel write FFoneCel;
    property Email: string read FEmail write FEmail;
  end;

  TContatoTrabalhador = class
  private
    FFonePrinc: string;
    FFoneAlternat: string;
    FEmailPrinc: string;
    FEmailAlternat: string;
  public
    property FonePrinc: string read FFonePrinc write FFonePrinc;
    property FoneAlternat: string read FFoneAlternat write FFoneAlternat;
    property EmailPrinc: string read FEmailPrinc write FEmailPrinc;
    property EmailAlternat: string read FEmailAlternat write FEmailAlternat;
  end;

  TInfoContrato = class
  private
    FCodCargo: string;
    FCodFuncao: string;
    FCodCateg: integer;
    FCodCarreira: string;
    FDTIngrCarr: TDate;

    FRemuneracao: TRemuneracao;
    FDuracao: TDuracao;
    FLocalTrabalho: TLocalTrabalho;
    FHorContratual: THorContratual;
    FInfoAtivDesemp: TInfoAtivDesemp;
    FFiliacaoSindical: TFiliacaoSindical;
    FAlvaraJudicial: TAlvaraJudicial;
    Fobservacoes: TobservacoesCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property CodCargo: string read FCodCargo write FCodCargo;
    property CodFuncao: string read FCodFuncao write FCodFuncao;
    property CodCateg: integer read FCodCateg write FCodCateg;
    property codCarreira: string read FCodCarreira write FCodCarreira;
    property dtIngrCarr: TDate read FDTIngrCarr write FDTIngrCarr;
    property Remuneracao: TRemuneracao read FRemuneracao write FRemuneracao;
    property Duracao: TDuracao read FDuracao write FDuracao;
    property LocalTrabalho: TLocalTrabalho read FLocalTrabalho write FLocalTrabalho;
    property HorContratual: THorContratual read FHorContratual write FHorContratual;
    property InfoAtivDesemp: TInfoAtivDesemp read FInfoAtivDesemp write FInfoAtivDesemp;
    property FiliacaoSindical: TFiliacaoSindical read FFiliacaoSindical write FFiliacaoSindical;
    property AlvaraJudicial: TAlvaraJudicial read FAlvaraJudicial write FAlvaraJudicial;
    property observacoes: TobservacoesCollection read Fobservacoes write Fobservacoes;
  end;

  TCTPS = class
  private
    FNrCtps: string;
    FSerieCtps: string;
    FUfCtps: string;
  public
    property NrCtps: string read FNrCtps write FNrCtps;
    property SerieCtps: string read FSerieCtps write FSerieCtps;
    property UfCtps: string read FUfCtps write FUfCtps;
  end;

  TDependenteCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDependenteCollectionItem;
    procedure SetItem(Index: Integer; Value: TDependenteCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TDependenteCollectionItem;
    property Items[Index: Integer]: TDependenteCollectionItem read GetItem write SetItem; default;
  end;

  TDependenteCollectionItem = class(TCollectionItem)
  private
    FtpDep: tpTpDep;
    FnmDep: string;
    FdtNascto: TDateTime;
    FcpfDep: string;
    FdepIRRF: tpSimNao;
    FdepSF: tpSimNao;
//    FDepPlan: tpSimNao;
    FIncTrab: tpSimNao;
  published
    constructor create; reintroduce;
    property tpDep: tpTpDep read FtpDep write FtpDep;
    property nmDep: string read FnmDep write FnmDep;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property cpfDep: string read FcpfDep write FcpfDep;
    property depIRRF: tpSimNao read FdepIRRF write FdepIRRF;
    property depSF: tpSimNao read FdepSF write FdepSF;
//    property depPlan: tpSimNao read FDepPlan write FDepPlan;
    property incTrab: tpSimNao read FIncTrab write FIncTrab;
  end;

  TDescAtividadeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDescAtividadeCollectionItem;
    procedure SetItem(Index: Integer; Value: TDescAtividadeCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TDescAtividadeCollectionItem;
    property Items[Index: Integer]: TDescAtividadeCollectionItem read GetItem write SetItem; default;
  end;

  TDescAtividadeCollectionItem = class(TCollectionItem)
  private
    FdescAtivDesemp: string;
  public
    constructor create; reintroduce;
    property descAtivDesemp: string read FdescAtivDesemp write FdescAtivDesemp;
  end;

  TDocumentos = class
  private
    FCTPS: TCTPS;
    FRIC: TRIC;
    FRG: TRG;
    FRNE: TRNE;
    FOC: TOC;
    FCNH: TCNH;
  public
    constructor Create;
    destructor Destroy; override;

    property CTPS: TCTPS read FCTPS write FCTPS;
    property RIC: TRIC read FRIC write FRIC;
    property RG: TRG read FRG write FRG;
    property RNE: TRNE read FRNE write FRNE;
    property OC: TOC read FOC write FOC;
    property CNH: TCNH read FCNH write FCNH;
  end;

  TDuracao = class
  private
    FTpContr: tpTpContr;
    FdtTerm: TDateTime;
    FclauAssec: tpSimNao;
  public
    property TpContr: tpTpContr read FTpContr write FTpContr;
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
    property clauAssec: tpSimNao read FclauAssec write FclauAssec;
  end;

  TEndereco = class
  private
    FBrasil: TBrasil;
    FExterior: TExterior;
  public
    constructor Create;
    destructor Destroy; override;

    property Brasil: TBrasil read FBrasil write FBrasil;
    property Exterior: TExterior read FExterior write FExterior;
  end;

  TEpiCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TEpiCollectionItem;
    procedure SetItem(Index: Integer; Value: TEpiCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TEpiCollectionItem;
    property Items[Index: Integer]: TEpiCollectionItem read GetItem write SetItem; default;
  end;

  TEpiCollectionItem = class(TCollectionItem)
  private
    FcaEPI: string;
    FeficEpi : tpSimNao;
    FperiodicTroca: tpSimNao;
    FcondFuncto: tpSimNao;
    Fhigienizacao: tpSimNao;
    FmedProtecao: tpSimNao;
    FprzValid: tpSimNao;
  public
    constructor create; reintroduce;
    property caEPI: string read FcaEPI write FcaEPI;
    property eficEpi : tpSimNao read FeficEpi write FeficEpi;
    property medProtecao: tpSimNao read FmedProtecao write FmedProtecao;
    property condFuncto: tpSimNao read FcondFuncto write FcondFuncto;
    property przValid : tpSimNao read FprzValid write FprzValid;
    property periodicTroca : tpSimNao read FperiodicTroca write FperiodicTroca;
    property higienizacao : tpSimNao read Fhigienizacao write Fhigienizacao;
  end;

  TExterior = class
  private
    FPaisResid: string;
    FDscLograd: string;
    FNrLograd: string;
    FComplemento: string;
    FBairro: string;
    FNmCid: string;
    FCodPostal: string;
  public
    property PaisResid: string read FPaisResid write FPaisResid;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property NmCid: string read FNmCid write FNmCid;
    property CodPostal: string read FCodPostal write FCodPostal;
  end;

  TFGTS = class
  private
    FOpcFGTS: tpOpcFGTS;
    FDtOpcFGTS: TDateTime;
  public
    property OpcFGTS: tpOpcFGTS read FOpcFGTS write FOpcFGTS;
    property DtOpcFGTS: TDateTime read FDtOpcFGTS write FDtOpcFGTS;
  end;

  TFiliacaoSindical = class(TCollection)
  private
    function GetItem(Index: Integer): TFiliacaoSindicalItem;
    procedure SetItem(Index: Integer; const Value: TFiliacaoSindicalItem);
  public
    constructor Create; reintroduce;
    function Add: TFiliacaoSindicalItem;
    property Items[Index: Integer]: TFiliacaoSindicalItem read GetItem write SetItem; default;
  end;

  TFiliacaoSindicalItem = class(TCollectionItem)
  private
    FCnpjSindTrab: string;
  public
    property CnpjSindTrab: string read FCnpjSindTrab write FCnpjSindTrab;
  end;

  THorarioCollection = class(TCollection)
  private
    function GetItem(Index: Integer): THorarioCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: THorarioCollectionItem;
    property Items[Index: Integer]: THorarioCollectionItem read GetItem write SetItem; default;
  end;

  THorarioCollectionItem = class(TCollectionItem)
  private
    FDia: tpTpDia;
    FCodHorContrat: string;
  public
    constructor create; reintroduce;
    property Dia: tpTpDia read FDia write FDia;
    property CodHorContrat: string read FCodHorContrat write FCodHorContrat;
  end;

  THorarioIntervaloCollection = class(TCollection)
  private
    function GetItem(Index: Integer): THorarioIntervaloCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioIntervaloCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: THorarioIntervaloCollectionItem;
    property Items[Index: Integer]: THorarioIntervaloCollectionItem read GetItem write SetItem;
  end;

  THorarioIntervaloCollectionItem = class(TCollectionItem)
  private
    FTpInterv : tpTpIntervalo;
    FDurInterv: integer;
    FIniInterv: string;
    FTermInterv : string;
  public
    constructor create; reintroduce;

    property tpInterv: tpTpIntervalo read FTpInterv write FTpInterv;
    property durInterv: integer read FDurInterv write FDurInterv;
    property iniInterv: string read FIniInterv write FIniInterv;
    property termInterv: string read FTermInterv write FTermInterv;
  end;

  THorContratual = class(TPersistent)
  private
    FQtdHrsSem: Double;
    FTpJornada: tpTpJornada;
    FDscTpJorn: string;
    FTMPParc: tpTmpParc;
    FHorario: THorarioCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property QtdHrsSem: Double read FQtdHrsSem write FQtdHrsSem;
    property TpJornada: tpTpJornada read FTpJornada write FTpJornada;
    property DscTpJorn: string read FDscTpJorn write FDscTpJorn;
    property tmpParc: tpTmpParc read FTMPParc write FTMPParc;
    property horario: THorarioCollection read FHorario write FHorario;
  end;

  TInscricao = class(TPersistent)
  protected
    FTpInsc: tpTpInsc;
    FNrInsc: string;
  public
    property TpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property NrInsc: string read FNrInsc write FNrInsc;
  end;

  TIdeEmpregador = class(TInscricao)
  private
    FOrgaoPublico: Boolean;
  public
    procedure AfterConstruction; override;
    property OrgaoPublico: Boolean read FOrgaoPublico write FOrgaoPublico;
  end;

  TIdeTransmissor = class(TIdeEmpregador);

  TIdeEvento = class(TPersistent)
  private
    FTpAmb: TpTpAmb;
    FProcEmi: TpProcEmi;
    FVerProc: string;
  public
    property TpAmb: TpTpAmb read FTpAmb write FTpAmb;
    property ProcEmi: TpProcEmi read FProcEmi write FProcEmi;
    property VerProc: string read FVerProc write FVerProc;
  end;

  TIdeEvento2 = class(TideEvento)
  private
    FIndRetif: tpIndRetificacao;
    FNrRecibo: string;
  public
    property indRetif: tpIndRetificacao read FIndRetif write FIndRetif;
    property NrRecibo: string read FNrRecibo write FNrRecibo;
  end;

  TIdeEvento3 = class(TideEvento2)
  private
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
  public
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
  end;

  TIdeEvento4 = class(TPersistent)
  private
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
    FTpAmb: TpTpAmb;
    FProcEmi: TpProcEmi;
    FVerProc: string;
  public
    property TpAmb: TpTpAmb read FTpAmb write FTpAmb;
    property ProcEmi: TpProcEmi read FProcEmi write FProcEmi;
    property VerProc: string read FVerProc write FVerProc;
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
  end;

  TIdeEvento5 = class(TPersistent)
  private
    FnrRecArqBase: string;
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
  public
    property nrRecArqBase: string read FnrRecArqBase write FnrRecArqBase;
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
  end;

  TIdePeriodo = class(TPersistent)
  private
    FIniValid: string;
    FFimValid: string;
  public
    property IniValid: string read FIniValid write FIniValid;
    property FimValid: string read FFimValid write FFimValid;
  end;

  TIdeEstabVinc = class(TInscricao)
  end;

  TIdeTomadorServ = class(TInscricao)
  private
    FIdeEstabVinc: TIdeEstabVinc;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstabVinc: TIdeEstabVinc read FIdeEstabVinc write FIdeEstabVinc;
  end;

  TIdeTrabSubstituidoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeTrabSubstituidoCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeTrabSubstituidoCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TIdeTrabSubstituidoCollectionItem;
    property Items[Index: Integer]: TIdeTrabSubstituidoCollectionItem read GetItem write SetItem; default;
  end;

  TIdeTrabSubstituidoCollectionItem = class(TCollectionItem)
  private
    FCpfTrabSubst:  string;
  public
    constructor Create; reintroduce;
    property CpfTrabSubst:  string read FCpfTrabSubst write FCpfTrabSubst;
  end;

  TIdeVinculo = class
  private
    FCpfTrab: string;
    FNisTrab: string;
    FMatricula: string;
    FcodCateg: Integer;
  public
    property cpfTrab: string read FcpfTrab write FcpfTrab;
    property nisTrab: string read FNisTrab write FNisTrab;
    property matricula: string read FMatricula write FMatricula;
    property codCateg: Integer read FcodCateg write FcodCateg;
  end;

  TInfoAtivDesemp = class(TPersistent)
  private
    FDescAtividade: TDescAtividadeCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property DescAtividade: TDescAtividadeCollection read FDescAtividade write FDescAtividade;
  end;

  TInfoDeficiencia = class
  private
    FDefMotora: tpSimNao;
    FDefVisual: tpSimNao;
    FDefMental: tpSimNao;
    FDefIntelectual: tpSimNao;
    FDefAuditiva: tpSimNao;
    FDefFisica: tpSimNao;
    FReabReadap: tpSimNao;
    FInfoCota: tpSimNao;
    FObservacao: string;
  public
    property DefFisica: tpSimNao read FDefFisica  write FDefFisica;
    property DefMental: tpSimNao read FDefMental  write FDefMental;
    property DefIntelectual: tpSimNao read FDefIntelectual write FDefIntelectual;
    property DefMotora: tpSimNao read FDefMotora write FDefMotora;
    property DefVisual: tpSimNao read FDefVisual write FDefVisual;
    property DefAuditiva: tpSimNao read FDefAuditiva write FDefAuditiva;
    property ReabReadap: tpSimNao read FReabReadap write FReabReadap;
    property infoCota: tpSimNao read FInfoCota write FInfoCota;
    property Observacao: string read FObservacao write FObservacao;
  end;

  TLocalTrabGeral = class
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FDescComp: string;
  public
    property TpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property NrInsc: string read FNrInsc write FNrInsc;
    property DescComp: string read FDescComp write FDescComp;
  end;

  TLocalTrabDom = class
  private
    FTpLograd: String;
    FDscLograd: string;
    FNrLograd: string;
    FComplemento: string;
    FBairro: string;
    FCep: string;
    FCodMunic: integer;
    FUf: tpuf;
  public
    property TpLograd: String read FTpLograd write FTpLograd;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property CodMunic: integer read FCodMunic write FCodMunic;
    property Uf: tpuf read FUf write FUf;
  end;

  TLocalTrabalho = class
  private
    FLocalTrabGeral: TLocalTrabGeral;
    FLocalTrabDom: TLocalTrabDom;
  public
    constructor Create;
    destructor Destroy; override;

    property LocalTrabGeral: TLocalTrabGeral read FLocalTrabGeral write FLocalTrabGeral;
    property LocalTrabDom: TLocalTrabDom read FLocalTrabDom write FLocalTrabDom;
  end;

  TNascimento = class
  private
    FDtNascto: TDateTime;
    FCodMunic: integer;
    FUF: string;
    FPaisNascto: string;
    FPaisNac: string;
    FNmMae: string;
    FNmPai: string;
  public
    property dtNascto: TDateTime read FDtNascto write FDtNascto;
    property codMunic: integer read FCodMunic write FCodMunic;
    property UF: string read FUF write FUF;
    property PaisNascto: string read FPaisNascto write FPaisNascto;
    property PaisNac: string read FPaisNac write FPaisNac;
    property NmMae: string read FNmMae write FNmMae;
    property NmPai: string read FNmPai write FNmPai;
  end;

  TProcesso = class(TCollectionItem)
  protected
    FNrProc: String;
    FCodSusp: String;
  public
    constructor create; reintroduce;
    property nrProc: string read FNrProc write FNrProc;
    property codSusp: String read FCodSusp write FCodSusp;
  end;

  TProcAdmJudFap = class(TProcesso)
  private
    FTpProc: tpTpProc;
  public
    property tpProc: tpTpProc read FTpProc write FTpProc;
  end;

  TProcAdmJudRat = class(TProcesso)
  private
    FTpProc: tpTpProc;
  public
    property tpProc: tpTpProc read FTpProc write FTpProc;
  end;

  TRemuneracao = class
  private
    FVrSalFx: double;
    FUndSalFixo: tpUndSalFixo;
    FDscSalVar: string;
  public
    property VrSalFx: double read FVrSalFx write FVrSalFx;
    property UndSalFixo: tpUndSalFixo read FUndSalFixo write FUndSalFixo;
    property DscSalVar: string read FDscSalVar write FDscSalVar;
  end;

  TRG = class
  private
    FNrRg: string;
    FOrgaoEmissor: string;
    FDtExped: TDateTime;
  public
    property NrRg: string read FNrRg write FNrRg;
    property OrgaoEmissor: string read FOrgaoEmissor write FOrgaoEmissor;
    property DtExped: TDateTime read FDtExped write FDtExped;
  end;

  TRNE = class
  private
    FNrRne: string;
    FOrgaoEmissor: string;
    FDtExped: TDateTime;
  public
    property NrRne: string read FNrRne write FNrRne;
    property OrgaoEmissor: string read FOrgaoEmissor write FOrgaoEmissor;
    property DtExped: TDateTime read FDtExped write FDtExped;
  end;

  TSucessaoVinc = class
  private
    FCnpjEmpregAnt: string;
    FMatricAnt: string;
    FdtTransf: TDateTime;
    FObservacao: string;
    FCnpjEmpSucessora: string;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjEmpregAnt: string read FCnpjEmpregAnt write FCnpjEmpregAnt;
    property CnpjEmpSucessora: string read FCnpjEmpSucessora write FCnpjEmpSucessora;
    property MatricAnt: string read FMatricAnt write FMatricAnt;
    property dtTransf: TDateTime read FdtTransf write FdtTransf;
    property Observacao: string read FObservacao write FObservacao;
  end;

  TOC = class
  private
    FNrOc: string;
    FOrgaoEmissor: string;
    FDtExped: TDateTime;
    FDtValid: TDateTime;
  public
    property NrOc: string read FNrOc write FNrOc;
    property OrgaoEmissor: string read FOrgaoEmissor write FOrgaoEmissor;
    property DtExped: TDateTime read FDtExped write FDtExped;
    property DtValid: TDateTime read FDtValid write FDtValid;
  end;

  TRIC = class
  private
    FNrRic: string;
    FOrgaoEmissor: string;
    FDtExped: TDateTime;
  public
    property NrRic: string read FNrRic write FNrRic;
    property OrgaoEmissor: string read FOrgaoEmissor write FOrgaoEmissor;
    property DtExped: TDateTime read FDtExped write FDtExped;
  end;

  TTrabalhador = class(TPersistent)
  private
    FCpfTrab: string;
    FNisTrab: string;
    FNmTrab: string;
    FSexo: string;
    FRacaCor: integer;
    FEstCiv: integer;
    FGrauInstr: string;
    FNmSoc: string;
    FIndPriEmpr: tpSimNao;

    FNascimento: TNascimento;
    FDocumentos: TDocumentos;
    FEndereco: TEndereco;
    FTrabEstrangeiro: TTrabEstrangeiro;
    FInfoDeficiencia: TInfoDeficiencia;
    FDependente: TDependenteCollection;
    FAposentadoria: TAposentadoria;
    FContato: TContatoTrabalhador;
    FExtrangeiroSN : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property CpfTrab: string read FCpfTrab write FCpfTrab;
    property NisTrab: string read FNisTrab write FNisTrab;
    property NmTrab: string read FNmTrab write FNmTrab;
    property Sexo: string read FSexo write FSexo;
    property RacaCor: integer read FRacaCor write FRacaCor;
    property EstCiv: integer read FEstCiv write FEstCiv;
    property GrauInstr: string read FGrauInstr write FGrauInstr;
    property nmSoc: string read FNmSoc write FNmSoc;
    property IndPriEmpr: tpSimNao read FIndPriEmpr write FIndPriEmpr;
    property Nascimento: TNascimento read FNascimento write FNascimento;
    property Documentos: TDocumentos read FDocumentos write FDocumentos;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property TrabEstrangeiro: TTrabEstrangeiro read FTrabEstrangeiro write FTrabEstrangeiro;
    property InfoDeficiencia: TInfoDeficiencia read FInfoDeficiencia write FInfoDeficiencia;
    property Dependente: TDependenteCollection read FDependente write FDependente;
    property Aposentadoria: TAposentadoria read FAposentadoria write FAposentadoria;
    property Contato: TContatoTrabalhador read FContato write FContato;
    property ExtrangeiroSN: Boolean read FExtrangeiroSN write FExtrangeiroSN;
  end;

  TTrabEstrangeiro = class
  private
    FDtChegada: TDateTime;
    FClassTrabEstrang: tpClassTrabEstrang;
    FCasadoBr: string;
    FFilhosBr: string;
  public
    property DtChegada: TDateTime read FDtChegada write FDtChegada;
    property ClassTrabEstrang: tpClassTrabEstrang read FClassTrabEstrang write FClassTrabEstrang;
    property CasadoBr: string read FCasadoBr write FCasadoBr;
    property FilhosBr: string read FFilhosBr write FFilhosBr;
  end;

  TTrabTemporario = class(TPersistent)
  private
    FHipLeg: integer;
    FJustContr: string;
    FTpInclContr: tpInclContr;
    FJustProrr: string;

    FIdeTomadorServ: TIdeTomadorServ;
    FIdeTrabSubstituido: TIdeTrabSubstituidoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property hipLeg: integer read FHipLeg write FHipLeg;
    property justContr: string read FJustContr write FJustContr;
    property tpinclContr: tpinclContr read FTpInclContr write FTpInclContr;
    property justProrr: string read FJustProrr write FJustProrr;
    property IdeTomadorServ: TIdeTomadorServ read FIdeTomadorServ write FIdeTomadorServ;
    property IdeTrabSubstituido: TIdeTrabSubstituidoCollection read FIdeTrabSubstituido write FIdeTrabSubstituido;
  end;

  TAprend = class(TInscricao)
  end;

  TInfoCeletista = class
  private
    FDtAdm: TDate;
    FTpAdmissao: tpTpAdmissao;
    FIndAdmissao: tpTpIndAdmissao;
    FTpRegJor: tpTpRegJor;
    FNatAtividade: tpNatAtividade;
    FdtBase: Integer;
    FcnpjSindCategProf: string;

    FFGTS: TFGTS;
    FTrabTemporario: TTrabTemporario;
    FAprend: TAprend;
  public
    constructor Create;
    destructor Destroy; override;

    property DtAdm: TDate read FDtAdm write FDtAdm;
    property TpAdmissao: tpTpAdmissao read FTpAdmissao write FTpAdmissao;
    property IndAdmissao: tpTpIndAdmissao read FIndAdmissao write FIndAdmissao;
    property TpRegJor: tpTpRegJor read FTpRegJor write FTpRegJor;
    property NatAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property dtBase: Integer read FdtBase write FdtBase;
    property  cnpjSindCategProf: string read FcnpjSindCategProf write FcnpjSindCategProf;

    property FGTS: TFGTS read FFGTS write FFGTS;
    property TrabTemporario: TTrabTemporario read FTrabTemporario write FTrabTemporario;
    property aprend: TAprend read FAprend write FAprend;
  end;

  TInfoDecJud = class
  private
    FNrProcJud: string;
  public
    property nrProcJud: string read FNrProcJud write FNrProcJud;
  end;

  TInfoEstatutario = class
  private
    FIndProvim: tpIndProvim;
    FTpProv: tpTpProv;
    FDtNomeacao: TDate;
    FDtPosse: TDate;
    FDtExercicio: TDate;
    FTpPlanRP: tpPlanRP;
    FInfoDecJud: TInfoDecJud;
  public
    constructor Create;
    destructor Destroy; override;

    property IndProvim: tpIndProvim read FIndProvim write FIndProvim;
    property TpProv: tpTpProv read FTpProv write FTpProv;
    property DtNomeacao: TDate read FDtNomeacao write FDtNomeacao;
    property DtPosse: TDate read FDtPosse write FDtPosse;
    property DtExercicio: TDate read FDtExercicio write FDtExercicio;
    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property infoDecJud: TInfoDecJud read FInfoDecJud write FInfoDecJud;
  end;

  TInfoRegimeTrab = class
  private
    FInfoCeletista: TInfoCeletista;
    FInfoEstatutario: TInfoEstatutario;
  public
    constructor Create;
    destructor Destroy; override;

    property InfoCeletista: TInfoCeletista read FInfoCeletista write FInfoCeletista;
    property InfoEstatutario: TInfoEstatutario read FInfoEstatutario write FInfoEstatutario;
  end;

  TInfoASO = class
  private
    FDtAso: TDate;
    FNrCRM: string;
    FUfCRM: tpuf;
  public
    property DtAso: TDate read FDtAso write FDtAso;
    property NrCRM: string read FNrCRM write FNrCRM;
    property UfCRM: tpuf read FUfCRM write FUfCRM;
  end;

  TAfastamento = class
  private
    FDtIniAfast: TDate;
    FcodMotAfast: tpMotivosAfastamento;
  public
    property DtIniAfast: TDate read FDtIniAfast write FDtIniAfast;
    property codMotAfast : tpMotivosAfastamento read FcodMotAfast write FcodMotAfast;
  end;

  TDesligamento = class
  private
    FDtDeslig: TDate;
  public
    property DtDeslig: TDate read FDtDeslig write FDtDeslig;
  end;

  TtransfDom = class
  private
    FcpfSubstituido: String;
    FmatricAnt: String;
    FdtTransf: TDate;
  public
    property cpfSubstituido: String read FcpfSubstituido write FcpfSubstituido;
    property matricAnt: String read FmatricAnt write FmatricAnt;
    property dtTransf: TDate read FdtTransf write FdtTransf;
  end;

  TVinculo = class
  private
    FMatricula: string;
    FTpRegTrab: tpTpRegTrab;
    FTpRegPrev: tpTpRegPrev;
    FNrRecInfPrelim: string;
    FcadIni: tpSimNao;

    FInfoRegimeTrab: TInfoRegimeTrab;
    FInfoContrato: TInfoContrato;
    FSucessaoVinc: TSucessaoVinc;
    FtransfDom: TtransfDom;
    FAfastamento: TAfastamento;
    FDesligamento: TDesligamento;
    FInfoASO: TInfoASO;
  public
    constructor Create;
    destructor Destroy; override;

    property Matricula: string read FMatricula write FMatricula;
    property TpRegTrab: tpTpRegTrab read FTpRegTrab write FTpRegTrab;
    property TpRegPrev: tpTpRegPrev read FTpRegPrev write FTpRegPrev;
    property NrRecInfPrelim: string read FNrRecInfPrelim write FNrRecInfPrelim;
    property cadIni: tpSimNao read FcadIni write FcadIni;

    property InfoRegimeTrab: TInfoRegimeTrab read FInfoRegimeTrab write FInfoRegimeTrab;
    property InfoContrato: TInfoContrato read FInfoContrato write FInfoContrato;
    property SucessaoVinc: TSucessaoVinc read FSucessaoVinc write FSucessaoVinc;
    property transfDom: TtransfDom read FtransfDom write FtransfDom;

    property Afastamento: TAfastamento read FAfastamento write FAfastamento;
    property Desligamento: TDesligamento read FDesligamento write FDesligamento;
    property InfoASO: TInfoASO read FInfoASO write FInfoASO;
  end;

  TideTrabalhador = class(TPersistent)  //S-2205;
  private
    FCpfTrab: string;
  public
    property cpfTrab: string read FCpfTrab write FCpfTrab;
  end;

  TideTrabalhador2 = class(TideTrabalhador) //S-2210;S-3000;
  private
    FNisTrab: string;
  public
    property nisTrab: string read FNisTrab write FNisTrab;
  end;

  TideTrabalhador3 = class(TideTrabalhador) 
  private
    FprocJudTrab: TprocJudTrabCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property procJudTrab: TprocJudTrabCollection read FprocJudTrab write FprocJudTrab;
  end;

  TideTrabSemVinc = class(TideTrabalhador2)
  private
    FcodCateg : Integer;
  public
    property codCateg : Integer read FcodCateg write FcodCateg;
  end;

  TEmitente = class(TPersistent)
  private
    FnmEmit: string;
    FideOC: tpIdeOC;
    FnrOc: string;
    FufOC: tpuf;
  public
    property nmEmit: string read FnmEmit write FnmEmit;
    property ideOC: tpIdeOC read FideOC write FideOC;
    property nrOc: string read FnrOc write FnrOc;
    property ufOC: tpuf read FufOC write FufOC;
  end;

  TEndExt = class(TPersistent)
  private
    FDscLograd: string;
    FNrLograd: string;
    FComplem: string;
    FBairro: string;
    FNmCid: string;
    FCodPostal: string;
  public
    property dscLograd: string read FDscLograd write FDscLograd;
    property nrLograd: string read FNrLograd write FNrLograd;
    property complem: string read FComplem write FComplem;
    property bairro: string read FBairro write FBairro;
    property nmCid: string read FNmCid write FNmCid;
    property codPostal: string read FCodPostal write FCodPostal;
  end;

  TIdePais = class(TPersistent)
  private
    FCodPais: string;
    FIndNIF: tpIndNIF;
    FNifBenef: string;
  public
    property codPais: string read FCodPais write FCodPais;
    property indNIF: tpIndNIF read FIndNIF write FIndNIF;
    property nifBenef: string read FNifBenef write FNifBenef;
  end;

  TInfoAgNocivo = class(TPersistent)
  private
    FGrauExp: tpGrauExp;
  public
    property grauExp: tpGrauExp read FGrauExp write FGrauExp;
  end;

  TRubricaCollection = class(TCollection)
  private
  public
    function GetItem(Index: Integer): TRubricaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRubricaCollectionItem);
  public
    constructor Create;
    function Add: TRubricaCollectionItem;
    property Items[Index: Integer]: TRubricaCollectionItem read GetItem write SetItem;
      default;
  end;

  TRubricaCollectionItem = class(TCollectionItem)
  protected
    Fmatricula: string;
    FCodRubr: string;
    FIdeTabRubr: string;
    FQtdRubr: Double;
    FFatorRubr: Double;
    FVrUnit: Double;
    FVrRubr: Double;
  public
    constructor create; reintroduce;

    property matricula: string read Fmatricula write Fmatricula;
    property codRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property qtdRubr: Double read FQtdRubr write FQtdRubr;
    property fatorRubr: Double read FFatorRubr write FFatorRubr;
    property vrUnit: Double read FVrUnit write FVrUnit;
    property vrRubr: Double read FVrRubr write FVrRubr;
  end;

  TReciboPagamento = class(TCollectionItem) // s1200
  private
    FIdeRecPgto: Integer;
    FvrRec : Double;
  public
    constructor create; reintroduce;
    property ideRecPgto: Integer read FIdeRecPgto write FIdeRecPgto;
    property vrRec : Double read FvrRec write FvrRec;
  end;

  TRecPgtosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRecPgtosCollectionItem;
    procedure SetItem(Index: Integer; Value: TRecPgtosCollectionItem);
  public
    constructor create; reintroduce;
    function Add: TRecPgtosCollectionItem;
    property Items[Index: Integer]: TRecPgtosCollectionItem read GetItem write SetItem;
  end;

  TRecPgtosCollectionItem = class(TReciboPagamento) // s2299 ; s2399
  private
    FIdeRecPgto : Integer;
    FVlrPgto: Double;
    FideEstabLot : TIdeEstabLotCollection;
  public
    constructor Create;
    property vlrPgto: Double read FVlrPgto write FVlrPgto;
    property IdeRecPgto: Integer read FIdeRecPgto write FIdeRecPgto;
    property ideEstabLot : TideEstabLotCollection read FideEstabLot write FideEstabLot;
  end;

  TideEstabLotCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TideEstabLotItem;
    procedure SetItem(Index: Integer; Value: TideEstabLotItem);
  public
    constructor create; reintroduce;
    function Add: TideEstabLotItem;
    property Items[Index: Integer]: TideEstabLotItem read GetItem write SetItem; default;
  end;

  TideEstabLotItem = class(TCollectionItem)
  private
    FtpInsc : tpTpInsc;
    FnrInsc : String;
    FcodLotacao : String;
    FdetVerbas : TRubricaCollection;
    FInfoSaudeColet: TInfoSaudeColet;
    FinfoAgNocivo : TInfoAgNocivo;
    FinfoSimples : TinfoSimples;

    function getInfoSaudeColect: TInfoSaudeColet;
    function getInfoAgNocivo: TInfoAgNocivo;
    function getInfoSimples: TinfoSimples;
  public
    constructor Create; reintroduce;
    function infoSaudeColetInst: boolean;
    function infoAgNocivoInst: boolean;
    function infoSimplesInst: Boolean;

    property tpInsc : tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc : string read FnrInsc write FnrInsc;
    property codLotacao : string read FcodLotacao write FcodLotacao;
    property detVerbas : TRubricaCollection read FdetVerbas write FdetVerbas;
    property infoSaudeColet: TInfoSaudeColet read getInfoSaudeColect write FInfoSaudeColet;
    property infoAgNocivo : TInfoAgNocivo read getInfoAgNocivo write FinfoAgNocivo;
    property infoSimples : TinfoSimples read getInfoSimples write FinfoSimples;
  end;

  TInfoAmbCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoAmbItem;
    procedure SetItem(Index: Integer; Value: TInfoAmbItem);
  public
    constructor create; reintroduce;
    function Add: TInfoAmbItem;
    property Items[Index: Integer]: TInfoAmbItem read GetItem write SetItem;
  end;

  TInfoAmbItem = class(TCollectionItem)
  private
    FcodAmb : String;
    FInfoAtiv: TInfoAtiv;
    FFatRisco: TFatRiscoCollection;
    FEPI : TEpiCollection;

    procedure setInfoAtiv(const Value: TInfoAtiv);
    procedure setFatRisco(const Value: TFatRiscoCollection);
    procedure setEPI(const Value: TEpiCollection);
  public
    constructor create; reintroduce;

    property codAmb : String read FcodAmb write FcodAmb;
    property InfoAtiv : TInfoAtiv read FInfoAtiv write setInfoAtiv;
    property FatRisco : TFatRiscoCollection read FFatRisco write setFatRisco;
    property EPI : TEpiCollection read FEPI write setEPI;
  end;

  TInfoAtiv = class(TPersistent)
  private
    FdscAtivDes : String;
  public
    property dscAtivDes : String read FdscAtivDes write FdscAtivDes;
  end;

  TEpcCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TEpcCollectionItem;
    procedure SetItem(Index: Integer; Value: TEpcCollectionItem);
  public
    constructor create; reintroduce;

    function Add: TEpcCollectionItem;
    property Items[Index: Integer]: TEpcCollectionItem read GetItem write SetItem; default;
  end;

  TEpcCollectionItem = class(TCollectionItem)
  private
    FDscEpc: string;
    FEficEpc: tpSimNao;
  public
    property dscEpc: String read FDscEpc write FDscEpc;
    property eficEpc: tpSimNao read FEficEpc write FEficEpc;
  end;

  TEpcEpi = class(TPersistent)
  private
    FUtilizEPC: tpUtilizEPC;
    FUtilizEPI: tpUtilizEPI;
    FEPC: TEpcCollection;
    FEpi: TEpiCollection;

    function getEpc: TEpcCollection;
    function getEpi: TEpiCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function epcInst: boolean;
    function epiInst: boolean;

    property utilizEPC: tpUtilizEPC read FUtilizEPC write FUtilizEPC;
    property utilizEPI: tpUtilizEPI read FUtilizEPI write FUtilizEPI;
    property epc: TEpcCollection read getEpc write FEPC;
    property epi: TEpiCollection read getEpi write FEpi;
  end;

  TFatRiscoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TFatRiscoItem;
    procedure SetItem(Index: Integer; Value: TFatRiscoItem);
  public
    constructor create; reintroduce;

    function Add: TFatRiscoItem;
    property Items[Index: Integer]: TFatRiscoItem read GetItem write SetItem;
  end;

  TFatRiscoItem = class(TCollectionItem)
  private
    FcodFatRis : String;
    FintConc : String;
    FtecMedicao : String;
    FEpcEpi: TEpcEpi;
  public
    constructor create; reintroduce;

    property codFatRis: String read FcodFatRis write FcodFatRis;
    property intConc : String read FintConc write FintConc;
    property tecMedicao: String read FtecMedicao write FtecMedicao;
    property epcEpi: TEpcEpi read FEpcEpi write FEpcEpi;
  end;

  TcargoFuncao = class(TPersistent)
  private
    FcodCargo : String;
    FcodFuncao : String;
  public
    property codCargo: String read FcodCargo write FcodCargo;
    property codFuncao: String read FcodFuncao write FcodFuncao;
  end;

  TProcJudTrabCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TProcJudTrabCollectionItem;
    procedure SetItem(Index: Integer; Value: TProcJudTrabCollectionItem);
  public
    constructor create; reintroduce;
    function Add: TProcJudTrabCollectionItem;
    property Items[Index: Integer]: TProcJudTrabCollectionItem read GetItem write SetItem;
  end;

  TProcJudTrabCollectionItem = class(TCollectionItem)
  private
    FTpTrib: tpTpTributo;
    FNrProcJud: string;
    FCodSusp: integer;
  published
    constructor create; reintroduce;
    property tpTrib: tpTpTributo read FTpTrib write FTpTrib;
    property nrProcJud: string read FNrProcJud write FNrProcJud;
    property codSusp: Integer read FCodSusp write FCodSusp;
  end;

  TinfoEstagiario = class(TPersistent)
  private
    FnatEstagio : TpNatEstagio;
    FnivEstagio : TpNivelEstagio;
    FareaAtuacao : String;
    FnrApol : String;
    FvlrBolsa : Double;
    FdtPrevTerm : TDate;
    FinstEnsino : TinstEnsino;
    FageIntegracao : TageIntegracao;
    FsupervisorEstagio : TsupervisorEstagio;
  public
    constructor Create;
    destructor  Destroy; override;

    property natEstagio : TpNatEstagio read FnatEstagio write FnatEstagio;
    property nivEstagio : TpNivelEstagio read FnivEstagio write FnivEstagio;
    property areaAtuacao : String read FareaAtuacao write FareaAtuacao;
    property nrApol : String read FnrApol write FnrApol;
    property vlrBolsa : Double read FvlrBolsa write FvlrBolsa;
    property dtPrevTerm : TDate read FdtPrevTerm write FdtPrevTerm;
    property instEnsino : TinstEnsino read FinstEnsino write FinstEnsino;
    property ageIntegracao : TageIntegracao read FageIntegracao write FageIntegracao;
    property supervisorEstagio : TsupervisorEstagio read FsupervisorEstagio write FsupervisorEstagio;
  end;

  TinstEnsino = class(TPersistent)
  private
    FcnpjInstEnsino : String;
    FnmRazao : String;
    FdscLograd : String;
    FnrLograd : String;
    Fbairro : String;
    FCep : String;
    FcodMunic : Integer;
    FUf : tpUf;
  public
    property cnpjInstEnsino : String read FcnpjInstEnsino write FcnpjInstEnsino;
    property nmRazao : String read FnmRazao write FnmRazao;
    property dscLograd : String read FdscLograd write FdscLograd;
    property nrLograd : String read FnrLograd write FnrLograd;
    property bairro : String read Fbairro write Fbairro;
    property Cep : String read FCep write FCep;
    property codMunic : Integer read FcodMunic write FcodMunic;
    property Uf : tpUf read FUf write FUf;
  end;

  TageIntegracao = class(TPersistent)
  private
    FcnpjAgntInteg : String;
    FnmRazao : String;
    FdscLograd : String;
    FnrLograd : String;
    Fbairro : String;
    FCep : String;
    FcodMunic : Integer;
    FUf : tpUf;
  public
    property cnpjAgntInteg : String read FcnpjAgntInteg write FcnpjAgntInteg;
    property nmRazao : String read FnmRazao write FnmRazao;
    property dscLograd : String read FdscLograd write FdscLograd;
    property nrLograd : String read FnrLograd write FnrLograd;
    property bairro : String read Fbairro write Fbairro;
    property Cep : String read FCep write FCep;
    property codMunic : Integer read FcodMunic write FcodMunic;
    property Uf : tpUf read FUf write FUf;
  end;

  TsupervisorEstagio = class(TPersistent)
  private
    FcpfSupervisor : String;
    FnmSuperv : String;
  public
    property cpfSupervisor : String read FcpfSupervisor write FcpfSupervisor;
    property nmSuperv : String read FnmSuperv write FnmSuperv;
  end;

  TQuarentena = class(TPersistent)
  private
    FdtFimQuar : TDate;
  public
    property dtFimQuar: TDate read FdtFimQuar write FdtFimQuar;
  end;

  TVerbasResc = class(TPersistent)
  private
    FProcJudTrab : TProcJudTrabCollection;
    FInfoMV: TInfoMV;

    function getProcJudTrab: TProcJudTrabCollection;
    function getInfoMV: TInfoMV;
  public
    constructor Create;
    destructor Destroy; override;

    function procJudTrabInst: boolean;
    function infoMVInst: boolean;

    property procJudTrab: TProcJudTrabCollection read getProcJudTrab write FProcJudTrab;
    property infoMV: TInfoMV read getInfoMV write FInfoMV;
  end;

  TdetVerbasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdetVerbasItem;
    procedure SetItem(Index: Integer; Value: TdetVerbasItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TdetVerbasItem;
    property Items[Index: Integer]: TdetVerbasItem read GetItem write SetItem; default;
  end;

  TdetVerbasItem = class(TCollectionItem)
  protected
    FcodRubr : String;
    FideTabRubr: String;
    FqtdRubr: Double;
    FvrUnit: Double;
    FvrRubr: Double;
  public
    property codRubr: String read FcodRubr write FcodRubr;
    property ideTabRubr: String read FideTabRubr write FideTabRubr;
    property qtdRubr: Double read FqtdRubr write FqtdRubr;
    property vrUnit: Double read FvrUnit write FvrUnit;
    property vrRubr: DOuble read FvrRubr write FvrRubr;
  end;

  TinfoSimples = class(TPersistent)
  private
    FindSimples : tpIndSimples;
  public
    property indSimples : tpIndSimples read FindSimples write FindSimples;
  end;

  TInfoProcJudCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoProcJudItem;
    procedure SetItem(Index: Integer; Value: TInfoProcJudItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfoProcJudItem;
    property Items[Index: Integer]: TInfoProcJudItem read GetItem write SetItem; default;
  end;

  TInfoProcJudItem=class(TCollectionItem)
  private
    FtpProc : tpTpProc;
    FtpTrib : tpTpTributo;
    FnrProcJud: string;//em S1250 o campo é nrProcJUD e em S1260 é apenas nrProc - deixado nrProcJud para reutilização da classe
    FCodSusp: Integer;
    FvrCPNRet: Double;
    FvrRatNRet: Double;
    FvrSenarNRet: Double;
    FvrCPSusp: Double;
    FvrRatSusp: Double;
    FvrSenarSusp: Double;
  public
    property tpProc: tpTpProc read FtpProc write FtpProc;
    property tpTrib: tpTpTributo read FtpTrib write FtpTrib;
    property nrProcJud: string read FnrProcJud write FnrProcJud;
    property codSusp: Integer read FCodSusp write FCodSusp;
    property vrCPNRet: Double read FvrCPNRet write FvrCPNRet;
    property vrRatNRet: Double read FvrRatNRet write FvrRatNRet;
    property vrSenarNRet: Double read FvrSenarNRet write FvrSenarNRet;
    property vrCPSusp: Double read FvrCPSusp write FvrCPSusp;
    property vrRatSusp: Double read FvrRatSusp write FvrRatSusp;
    property vrSenarSusp: Double read FvrSenarSusp write FvrSenarSusp;
  end;

  TPensaoAlimCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPensaoAlimCollectionItem;
    procedure SetItem(Index: Integer; Value: TPensaoAlimCollectionItem);
  public
    constructor create; reintroduce;
    function Add: TPensaoAlimCollectionItem;
    property Items[Index: Integer]: TPensaoAlimCollectionItem read GetItem write SetItem;
  end;

  TPensaoAlimCollectionItem = class(TCollectionItem)
  private
    FCpfBenef: string;
    FDtNasctoBenef: TDate;
    FNmBenefic: string;
    FVlrPensao: Double;
  public
    constructor create; reintroduce;

    property cpfBenef: string read FCpfBenef write FCpfBenef;
    property dtNasctoBenef: TDate read FDtNasctoBenef write FDtNasctoBenef;
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property vlrPensao: Double read FVlrPensao write FVlrPensao;
  end;

  TDetPlanoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDetPlanoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetPlanoCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TDetPlanoCollectionItem;
    property Items[Index: Integer]: TDetPlanoCollectionItem read GetItem write SetItem;
  end;

  TDetPlanoCollectionItem = class(TCollectionItem)
  private
    FTpDep: tpTpDep;
    FCpfDep: string;
    FDtNascto: TDate;
    FNmDep: string;
    FVlrPgDep: Double;
  public
    constructor create; reintroduce;

    property tpDep: tpTpDep read FTpDep write FTpDep;
    property cpfDep: string read FCpfDep write FCpfDep;
    property dtNascto: TDate read FDtNascto write FDtNascto;
    property nmDep: string read FNmDep write FNmDep;
    property vlrPgDep: Double read FVlrPgDep write FVlrPgDep;
  end;

  TDetOperCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDetOperCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetOperCollectionItem);
  public
    constructor create; reintroduce;
    function Add: TDetOperCollectionItem;
    property Items[Index: Integer]: TDetOperCollectionItem read GetItem write SetItem;
  end;

  TDetOperCollectionItem = class(TCollectionItem)
  private
    FCnpjOper: string;
    FRegANS: string;
    FVrPgTit: Double;
    FDetPlanoCollection: TDetPlanoCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property cnpjOper: string read FCnpjOper write FCnpjOper;
    property regANS: string read FRegANS write FRegANS;
    property vrPgTit: Double read FVrPgTit write FVrPgTit;
    property detPlano: TDetPlanoCollection read FDetPlanoCollection write FDetPlanoCollection;
  end;

  TInfoSaudeColet = class(TPersistent)
  private
    FDetOper: TDetOperCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property detOper: TDetOperCollection read FDetOper write FDetOper;

  end;

  TRemunPerCollectionItem = class(TCollectionItem)
  private
    FMatricula: string;
    FItensRemun: TRubricaCollection;
    FInfoSaudeColet: TInfoSaudeColet;
    function getInfoSaudeColet: TInfoSaudeColet;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function infoSaudeColetInst(): boolean;

    property matricula: string read FMatricula write FMatricula;
    property itensRemun: TRubricaCollection read FItensRemun write FItensRemun;
    property infoSaudeColet: TInfoSaudeColet read getInfoSaudeColet
      write FInfoSaudeColet;
  end;

  TNfsColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TNfsItem;
    procedure SetItem(Index: Integer; const Value: TNfsItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TNfsItem;
    property Items[Index: Integer]: TNfsItem read GetItem write SetItem;
  end;

  TNfsItem = class(TCollectionItem)
  private
    Fserie: string;
    FnrDocto: string;
    FdtEmisNF: TDateTime;
    FvlrBruto: Double;
    FvrCPDescPR: Double;
    FvrRatDescPR: Double;
    FvrSenarDesc: Double;
  public
    property serie: string read Fserie write Fserie;
    property nrDocto: string read FnrDocto write FnrDocto;
    property dtEmisNF: TDateTime read FdtEmisNF write FdtEmisNF;
    property vlrBruto: Double read FvlrBruto write FvlrBruto;
    property vrCPDescPR: Double read FvrCPDescPR write FvrCPDescPR;
    property vrRatDescPR: Double read FvrRatDescPR write FvrRatDescPR;
    property vrSenarDesc: Double read FvrSenarDesc write FvrSenarDesc;
  end;

  TRemunOutrEmprCollection = class(TCollection)
  private
    function GetItem(Index: integer): TRemunOutrEmprCollectionItem;
    procedure SetItem(Index: integer; Value: TRemunOutrEmprCollectionItem);
  public
    constructor Create; reintroduce;
    function add: TRemunOutrEmprCollectionItem;
    property Items[Index: integer]: TRemunOutrEmprCollectionItem
      read GetItem write SetItem;
  end;

  TRemunOutrEmprCollectionItem = class(TCollectionItem)
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FCodCateg: integer;
    FVlrRemunOE: double;
  public
    constructor Create; reintroduce;
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codCateg: integer read FCodCateg write FCodCateg;
    property vlrRemunOE: double read FVlrRemunOE write FVlrRemunOE;
  end;

  TInfoMV = class(TPersistent)
  private
    FIndMV: tpIndMV;
    FRemunOutrEmpr: TRemunOutrEmprCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property indMV: tpIndMV read FIndMV write FIndMV;
    property remunOutrEmpr: TRemunOutrEmprCollection
      read FRemunOutrEmpr write FRemunOutrEmpr;
  end;

  TIdeRespInf = class
  private
    FnmResp: string;
    FcpfResp: string;
    Ftelefone: string;
    Femail: string;
  public
    property nmResp: string read FnmResp write FnmResp;
    property cpfResp: string read FcpfResp write FcpfResp;
    property telefone: string read Ftelefone write Ftelefone;
    property email: string read Femail write Femail;
  end;

  TObservacoesCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TObservacoesCollectionItem;
    procedure SetItem(Index: Integer; Value: TObservacoesCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TObservacoesCollectionItem;
    property Items[Index: Integer]: TObservacoesCollectionItem read GetItem write SetItem; default;
  end;

  TObservacoesCollectionItem = class(TCollectionItem)
  private
    Fobservacao: string;
  published
    constructor create; reintroduce;

    property observacao: string read Fobservacao write Fobservacao;
  end;

  IEventoeSocial = Interface(IInterface)
    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    function GetEvento : TObject;

    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
  end;

implementation

{ TAliqRat }
constructor TAliqGilRat.Create;
begin
  inherited;
  FProcAdmJudRat := nil;
  FProcAdmJudFap := nil;
end;

destructor TAliqGilRat.Destroy;
begin
  FreeAndNil(FProcAdmJudRat);
  FreeAndNil(FProcAdmJudFap);
  inherited;
end;

function TAliqGilRat.getProcAdmJudFat: TProcAdmJudFap;
begin
  if Not(Assigned(FProcAdmJudFap)) then
    FProcAdmJudFap := TProcAdmJudFap.Create;
  Result := FProcAdmJudFap;
end;

function TAliqGilRat.getProcAdmJudRat: TProcAdmJudRat;
begin
  if Not(Assigned(FProcAdmJudRat)) then
    FProcAdmJudRat := TProcAdmJudRat.Create;
  Result := FProcAdmJudRat;
end;

function TAliqGilRat.procAdmJudFapInst: Boolean;
begin
  Result := Assigned(FProcAdmJudFap);
end;

function TAliqGilRat.procAdmJudRatInst: Boolean;
begin
  Result := Assigned(FProcAdmJudRat);
end;

{ TDocumentos }
constructor TDocumentos.Create;
begin
  inherited;
  FCTPS:= TCTPS.Create;
  FRIC:= TRIC.Create;
  FRG:= TRG.Create;
  FRNE:= TRNE.Create;
  FOC:= TOC.Create;
  FCNH:= TCNH.Create;
end;

destructor TDocumentos.Destroy;
begin
  FCTPS.Free;
  FRIC.Free;
  FRG.Free;
  FRNE.Free;
  FOC.Free;
  FCNH.Free;
  inherited;
end;

{ TEndereco }
constructor TEndereco.Create;
begin
  FBrasil:= TBrasil.Create;
  FExterior:= TExterior.Create;
end;

destructor TEndereco.Destroy;
begin
  FBrasil.Free;
  FExterior.Free;
  inherited;
end;

{ TDependenteCollection }
function TDependenteCollection.Add: TDependenteCollectionItem;
begin
  Result := TDependenteCollectionItem(inherited Add);
  Result.Create;
end;

constructor TDependenteCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TDependenteCollectionItem);
end;

function TDependenteCollection.GetItem(
  Index: Integer): TDependenteCollectionItem;
begin
  Result := TDependenteCollectionItem(inherited GetItem(Index));
end;

procedure TDependenteCollection.SetItem(Index: Integer;
  Value: TDependenteCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTrabalhador }
constructor TTrabalhador.Create;
begin
  inherited;
  FNascimento:= TNascimento.Create;
  FDocumentos:= TDocumentos.Create;
  FEndereco:= TEndereco.Create;
  FTrabEstrangeiro:= TTrabEstrangeiro.Create;
  FInfoDeficiencia:= TInfoDeficiencia.Create;
  FDependente:= TDependenteCollection.Create(Self);
  FAposentadoria:= TAposentadoria.Create;
  FContato:= TContatoTrabalhador.Create;
end;

destructor TTrabalhador.Destroy;
begin
  FNascimento.Free;
  FDocumentos.Free;
  FEndereco.Free;
  FTrabEstrangeiro.Free;
  FInfoDeficiencia.Free;
  FDependente.Free;
  FAposentadoria.Free;
  FContato.Free;
  inherited;
end;

{ TTrabTemporario }
constructor TTrabTemporario.Create;
begin
  inherited;
  FIdeTomadorServ:= TIdeTomadorServ.Create;
  FIdeTrabSubstituido:= TIdeTrabSubstituidoCollection.Create(self);
end;

destructor TTrabTemporario.Destroy;
begin
  FIdeTomadorServ.Free;
  FIdeTrabSubstituido.Free;
  inherited;
end;

{ TIdeTomadorServ }
constructor TIdeTomadorServ.Create;
begin
  FIdeEstabVinc := TIdeEstabVinc.Create;
end;

destructor TIdeTomadorServ.Destroy;
begin
  FIdeEstabVinc.Free;
  inherited;
end;

{ THorarioCollection }
function THorarioCollection.Add: THorarioCollectionItem;
begin
  Result := THorarioCollectionItem(inherited Add);
  Result.Create;
end;

constructor THorarioCollection.Create(AOwner: TPersistent);
begin
  inherited Create(THorarioCollectionItem);
end;

function THorarioCollection.GetItem(Index: Integer): THorarioCollectionItem;
begin
  Result := THorarioCollectionItem(inherited GetItem(Index));
end;

procedure THorarioCollection.SetItem(Index: Integer;
  Value: THorarioCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ THorarioIntervaloCollectionItem }

constructor THorarioIntervaloCollectionItem.create;
begin

end;

{ THorarioIntervaloCollection }

function THorarioIntervaloCollection.Add: THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem(inherited Add);
  Result.create;
end;

constructor THorarioIntervaloCollection.Create;
begin
  inherited create(THorarioIntervaloCollectionItem);
end;

function THorarioIntervaloCollection.GetItem(
  Index: Integer): THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem(inherited GetItem(Index));
end;

procedure THorarioIntervaloCollection.SetItem(Index: Integer;
  Value: THorarioIntervaloCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ THorContratual }
constructor THorContratual.Create;
begin
  inherited;
  FHorario:= THorarioCollection.Create(Self);
end;

destructor THorContratual.Destroy;
begin
  FHorario.Free;
  inherited;
end;

{ TDescAtividadeCollection }
function TDescAtividadeCollection.Add: TDescAtividadeCollectionItem;
begin
  Result := TDescAtividadeCollectionItem(inherited Add);
  Result.Create;
end;

constructor TDescAtividadeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TDescAtividadeCollectionItem);
end;

function TDescAtividadeCollection.GetItem(
  Index: Integer): TDescAtividadeCollectionItem;
begin
  Result := TDescAtividadeCollectionItem(inherited GetItem(Index));
end;

procedure TDescAtividadeCollection.SetItem(Index: Integer;
  Value: TDescAtividadeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoAtivDesemp }
constructor TInfoAtivDesemp.Create;
begin
  inherited;
  FDescAtividade:= TDescAtividadeCollection.Create(Self);
end;

destructor TInfoAtivDesemp.Destroy;
begin
  FDescAtividade.Free;
  inherited;
end;

{ TContrato }
constructor TInfoContrato.Create;
begin
  FRemuneracao:= TRemuneracao.Create;
  FDuracao:= TDuracao.Create;
  FLocalTrabalho:= TLocalTrabalho.Create;
  FHorContratual:= THorContratual.Create;
  FInfoAtivDesemp:= TInfoAtivDesemp.Create;
  FFiliacaoSindical:= TFiliacaoSindical.Create;
  FAlvaraJudicial:= TAlvaraJudicial.Create;
  Fobservacoes := TobservacoesCollection.Create;
end;

destructor TInfoContrato.Destroy;
begin
  FRemuneracao.Free;
  FDuracao.Free;
  FLocalTrabalho.Free;
  FHorContratual.Free;
  FInfoAtivDesemp.Free;
  FFiliacaoSindical.Free;
  FAlvaraJudicial.Free;
  Fobservacoes.Free;
  inherited;
end;

{ TSucessaoVinc }

constructor TSucessaoVinc.Create;
begin

end;

destructor TSucessaoVinc.Destroy;
begin

end;

{ TVinculo }
constructor TVinculo.Create;
begin
  FInfoRegimeTrab := TInfoRegimeTrab.Create;
  FInfoContrato:= TInfoContrato.Create;
  FSucessaoVinc:= TSucessaoVinc.Create;
  ftransfDom := TtransfDom.Create;
  FAfastamento := TAfastamento.Create;
  FDesligamento := TDesligamento.Create;
  FInfoASO := TInfoASO.Create;
end;

destructor TVinculo.Destroy;
begin
  FInfoRegimeTrab.Free;
  FInfoContrato.Free;
  FSucessaoVinc.Free;
  FtransfDom.Free;
  FAfastamento.Free;
  FDesligamento.Free;
  FInfoASO.Free;
  inherited;
end;

{ TEpcCollection }

function TEpcCollection.Add: TEpcCollectionItem;
begin
  Result := TEpcCollectionItem(inherited Add);
end;

constructor TEpcCollection.Create;
begin
  inherited Create(TEpcCollectionItem);
end;

function TEpcCollection.GetItem(Index: Integer): TEpcCollectionItem;
begin
  Result := TEpcCollectionItem(inherited GetItem(Index));
end;

procedure TEpcCollection.SetItem(Index: Integer; Value: TEpcCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEpiCollection }
function TEpiCollection.Add: TEpiCollectionItem;
begin
  Result := TEpiCollectionItem(inherited Add);
  Result.Create;
end;

constructor TEpiCollection.Create;
begin
  inherited Create(TEpiCollectionItem);
end;

function TEpiCollection.GetItem(Index: Integer): TEpiCollectionItem;
begin
  Result := TEpiCollectionItem(inherited GetItem(Index));
end;

procedure TEpiCollection.SetItem(Index: Integer;
  Value: TEpiCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRubricaCollectionItem }
constructor TRubricaCollectionItem.create;
begin

end;

{ TRubricaCollection }
function TRubricaCollection.Add: TRubricaCollectionItem;
begin
  Result := TRubricaCollectionItem(inherited Add);
  Result.create;
end;

constructor TRubricaCollection.Create;
begin
 inherited Create(TRubricaCollectionItem);
end;

function TRubricaCollection.GetItem(Index: Integer): TRubricaCollectionItem;
begin
  Result := TRubricaCollectionItem(inherited GetItem(Index));
end;

procedure TRubricaCollection.SetItem(Index: Integer;
  Value: TRubricaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TReciboPagamento }
constructor TReciboPagamento.create;
begin

end;

{ TRecPgtosCollection }
function TRecPgtosCollection.Add: TRecPgtosCollectionItem;
begin
  Result := TRecPgtosCollectionItem(inherited Add);
  Result.create;
end;

constructor TRecPgtosCollection.create;
begin
  inherited create(TRecPgtosCollectionItem);
end;

function TRecPgtosCollection.GetItem(Index: Integer): TRecPgtosCollectionItem;
begin
  Result := TRecPgtosCollectionItem(inherited GetItem(Index));
end;

procedure TRecPgtosCollection.SetItem(Index: Integer;
  Value: TRecPgtosCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoRegimeTrab }
constructor TInfoRegimeTrab.Create;
begin
  FInfoCeletista := TInfoCeletista.Create;
  FInfoEstatutario := TInfoEstatutario.Create;
end;

destructor TInfoRegimeTrab.Destroy;
begin
  FInfoCeletista.Free;
  FInfoEstatutario.Free;
  inherited;
end;

{ TInfoEstatutario }

constructor TInfoEstatutario.Create;
begin
  FInfoDecJud := TInfoDecJud.Create;
end;

destructor TInfoEstatutario.Destroy;
begin
  FInfoDecJud.Free;
  inherited;
end;

{ TInfoCeletista }
constructor TInfoCeletista.Create;
begin
  FFGTS := TFGTS.Create;
  FTrabTemporario := TTrabTemporario.Create;
  FAprend := TAprend.Create;
end;

destructor TInfoCeletista.Destroy;
begin
  FFGTS.Free;
  FTrabTemporario.Free;
  FAprend.Free;
  inherited;
end;

{ TLocalTrabalho }
constructor TLocalTrabalho.Create;
begin
  FLocalTrabGeral := TLocalTrabGeral.Create;
  FLocalTrabDom := TLocalTrabDom.Create;
end;

destructor TLocalTrabalho.Destroy;
begin
  FLocalTrabGeral.Free;
  FLocalTrabDom.Free;
  inherited;
end;

{ TFatRiscoCollection }

function TFatRiscoCollection.Add: TFatRiscoItem;
begin
  Result := TFatRiscoItem(inherited Add);
  Result.Create;
end;

constructor TFatRiscoCollection.create;
begin
  inherited Create(TFatRiscoItem);
end;

function TFatRiscoCollection.GetItem(Index: Integer): TFatRiscoItem;
begin
  Result := TFatRiscoItem(inherited GetItem(index));
end;

procedure TFatRiscoCollection.SetItem(Index: Integer; Value: TFatRiscoItem);
begin
  inherited SetItem(Index, Value);
end;

{ TFatRiscoItem }

constructor TFatRiscoItem.create;
begin
  FEpcEpi := TEpcEpi.Create;
end;

{ TInfoAmbCollection }

function TInfoAmbCollection.Add: TInfoAmbItem;
begin
  Result := TInfoAmbItem(inherited Add);
  Result.Create;
end;

constructor TInfoAmbCollection.create;
begin
  inherited Create(TInfoAmbItem);
end;

function TInfoAmbCollection.GetItem(Index: Integer): TInfoAmbItem;
begin
  Result := TInfoAmbItem(inherited GetItem(index));
end;

procedure TInfoAmbCollection.SetItem(Index: Integer; Value: TInfoAmbItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoAmbItem }

constructor TInfoAmbItem.create;
begin
  FInfoAtiv := TInfoAtiv.Create;
  FFatRisco := TFatRiscoCollection.Create;
  FEPI      := TEpiCollection.Create;
end;

procedure TInfoAmbItem.setEPI(const Value: TEpiCollection);
begin
  FEPI.Assign(Value);
end;

procedure TInfoAmbItem.setFatRisco(const Value: TFatRiscoCollection);
begin
  FFAtRisco.Assign(Value);
end;

procedure TInfoAmbItem.setInfoAtiv(const Value: TInfoAtiv);
begin
  FInfoAtiv.Assign(Value);
end;

{ TinfoEstagiario }

constructor TinfoEstagiario.Create;
begin
  inherited;
  FinstEnsino := TinstEnsino.Create;
  FageIntegracao := TageIntegracao.Create;
  FsupervisorEstagio := TsupervisorEstagio.Create;
end;

destructor TinfoEstagiario.Destroy;
begin
  FinstEnsino.Free;
  FageIntegracao.Free;
  FsupervisorEstagio.Free;
  inherited;
end;

{ TVerbasResc }

constructor TVerbasResc.Create;
begin
  FProcJudTrab := nil;
  FInfoMV := nil;
end;

destructor TVerbasResc.Destroy;
begin
  FreeAndNil(FProcJudTrab);
  FreeAndNil(FInfoMV);
end;

function TVerbasResc.getProcJudTrab: TProcJudTrabCollection;
begin
  if not assigned(FProcJudTrab) then
    FProcJudTrab := TProcJudTrabCollection.Create;
  Result := FProcJudTrab;
end;

function TVerbasResc.getInfoMV: TInfoMV;
begin
  if not assigned(FInfoMV) then
    FInfoMV := TInfoMV.Create;
  Result := FInfoMV;
end;

function TVerbasResc.procJudTrabInst: boolean;
begin
  result := Assigned(FProcJudTrab);
end;

function TVerbasResc.infoMVInst: boolean;
begin
  result := Assigned(FInfoMV);
end;

{ TInfoProcJudCollection }

function TInfoProcJudCollection.Add: TInfoProcJudItem;
begin
  Result:= TinfoProcJudItem(inherited Add);
  Result.Create(Self);
end;

constructor TInfoProcJudCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TinfoProcJudItem);
end;

function TInfoProcJudCollection.GetItem(Index: Integer): TInfoProcJudItem;
begin
  Result := TInfoProcJudItem(inherited GetItem(Index));
end;

procedure TInfoProcJudCollection.SetItem(Index: Integer; Value: TInfoProcJudItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRecPgtosCollectionItem }

constructor TRecPgtosCollectionItem.Create;
begin
  inherited;
  FideEstabLot := TideEstabLotCollection.Create;
end;

{ TdetVerbasCollection }

function TdetVerbasCollection.Add: TdetVerbasItem;
begin
  Result := TdetVerbasItem(inherited Add);
  Result.Create(Self);
end;

constructor TdetVerbasCollection.Create(AOwner: TPersistent);
begin
   inherited Create(TdetVerbasItem);
end;

function TdetVerbasCollection.GetItem(Index: Integer): TdetVerbasItem;
begin
  Result := TdetVerbasItem(inherited GetItem(Index));
end;

procedure TdetVerbasCollection.SetItem(Index: Integer; Value: TdetVerbasItem);
begin
  inherited SetItem(Index, Value);
end;

{ TideEstabLotCollection }

function TideEstabLotCollection.Add: TideEstabLotItem;
begin
  Result := TideEstabLotItem(inherited Add);
  Result.Create;
end;

constructor TideEstabLotCollection.create;
begin
  inherited Create(TideEstabLotItem);
end;

function TideEstabLotCollection.GetItem(Index: Integer): TideEstabLotItem;
begin
  Result := TideEstabLotItem(inherited GetItem(index));
end;

procedure TideEstabLotCollection.SetItem(Index: Integer; Value: TideEstabLotItem);
begin
  inherited SetItem(Index, Value);
end;

{ TideEstabLotItem }

constructor TideEstabLotItem.Create;
begin
  FdetVerbas := TRubricaCollection.Create;
  FInfoSaudeColet := nil;
  FinfoAgNocivo := nil;
  FinfoSimples := nil;
end;

function TideEstabLotItem.getInfoAgNocivo: TInfoAgNocivo;
begin
  if not assigned(FinfoAgNocivo) then
    FinfoAgNocivo := TInfoAgNocivo.Create;
  result := FinfoAgNocivo;
end;

function TideEstabLotItem.getInfoSimples: TinfoSimples;
begin
  if not assigned(FinfoSimples) then
    FinfoSimples := TinfoSimples.Create;
  Result := FinfoSimples;
end;

function TideEstabLotItem.getInfoSaudeColect: TInfoSaudeColet;
begin
  if not Assigned(FInfoSaudeColet) then
    FInfoSaudeColet := TInfoSaudeColet.Create;
  result := FInfoSaudeColet;
end;

function TideEstabLotItem.infoSaudeColetInst: boolean;
begin
  result := Assigned(FInfoSaudeColet);
end;

function TideEstabLotItem.infoAgNocivoInst: boolean;
begin
  result := assigned(FinfoAgNocivo);
end;

function TideEstabLotItem.infoSimplesInst: boolean;
begin
  result := Assigned(FinfoSimples);
end;

{ TProcJudTrab }
constructor TProcJudTrabCollectionItem.create;
begin
end;

{ TProcJudTrabCollection }
function TProcJudTrabCollection.Add: TProcJudTrabCollectionItem;
begin
  Result := TProcJudTrabCollectionItem(inherited Add);
  Result.create;
end;

constructor TProcJudTrabCollection.create;
begin
  inherited create(TProcJudTrabCollectionItem);
end;

function TProcJudTrabCollection.GetItem(
  Index: Integer): TProcJudTrabCollectionItem;
begin
  Result := TProcJudTrabCollectionItem(inherited GetItem(Index));
end;

procedure TProcJudTrabCollection.SetItem(Index: Integer;
  Value: TProcJudTrabCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TPensaoAlimCollection }
function TPensaoAlimCollection.Add: TPensaoAlimCollectionItem;
begin
  Result := TPensaoAlimCollectionItem(inherited Add);
  Result.create;
end;

constructor TPensaoAlimCollection.create;
begin
  inherited create(TPensaoAlimCollectionItem)
end;

function TPensaoAlimCollection.GetItem(
  Index: Integer): TPensaoAlimCollectionItem;
begin
  Result := TPensaoAlimCollectionItem(inherited GetItem(Index));
end;

procedure TPensaoAlimCollection.SetItem(Index: Integer;
  Value: TPensaoAlimCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TPensaoAlimCollectionItem }
constructor TPensaoAlimCollectionItem.create;
begin
end;

{ TDetPlano }
function TDetPlanoCollection.Add: TDetPlanoCollectionItem;
begin
  Result := TDetPlanoCollectionItem(inherited Add);
  Result.create;
end;

constructor TDetPlanoCollection.create;
begin
  inherited create(TDetPlanoCollectionItem);
end;

function TDetPlanoCollection.GetItem(Index: Integer): TDetPlanoCollectionItem;
begin
  Result := TDetPlanoCollectionItem(inherited GetItem(Index));
end;

procedure TDetPlanoCollection.SetItem(Index: Integer;
  Value: TDetPlanoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDetPlanoCollectionItem }
constructor TDetPlanoCollectionItem.create;
begin

end;

{ TDetOperCollectionItem }
constructor TDetOperCollectionItem.create;
begin
  FDetPlanoCollection := TDetPlanoCollection.create;
end;

destructor TDetOperCollectionItem.destroy;
begin
  FDetPlanoCollection.Free;
  inherited;
end;

{ TDetOperCollection }
function TDetOperCollection.Add: TDetOperCollectionItem;
begin
  Result := TDetOperCollectionItem(inherited Add);
  Result.create;
end;

constructor TDetOperCollection.create;
begin
  inherited create(TDetOperCollectionItem)
end;

function TDetOperCollection.GetItem(Index: Integer): TDetOperCollectionItem;
begin
  Result := TDetOperCollectionItem(inherited GetItem(Index));
end;

procedure TDetOperCollection.SetItem(Index: Integer;
  Value: TDetOperCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoSaudeColet }
constructor TInfoSaudeColet.create;
begin
  FDetOper := TDetOperCollection.create;
end;

destructor TInfoSaudeColet.destroy;
begin
  FDetOper.Free;
end;

{TIdeTrabSubstituidoCollection}

function TIdeTrabSubstituidoCollection.Add: TIdeTrabSubstituidoCollectionItem;
begin
  Result := TIdeTrabSubstituidoCollectionItem(inherited Add);
  Result.Create;
end;

constructor TIdeTrabSubstituidoCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TIdeTrabSubstituidoCollectionItem);
end;

function TIdeTrabSubstituidoCollection.GetItem(Index: Integer): TIdeTrabSubstituidoCollectionItem;
begin
  Result := TIdeTrabSubstituidoCollectionItem(inherited GetItem(Index));
end;

procedure TIdeTrabSubstituidoCollection.SetItem(Index: Integer;
  Value: TIdeTrabSubstituidoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDependenteCollectionItem }

constructor TDependenteCollectionItem.create;
begin

end;

{ TDescAtividadeCollectionItem }

constructor TDescAtividadeCollectionItem.create;
begin

end;

{ TEpiCollectionItem }

constructor TEpiCollectionItem.create;
begin

end;

{ THorarioCollectionItem }

constructor THorarioCollectionItem.create;
begin

end;

{ TIdeTrabSubstituidoCollectionItem }


constructor TIdeTrabSubstituidoCollectionItem.Create;
begin

end;

{ TProcesso }
constructor TProcesso.Create;
begin

end;

{ TRemunPerCollectionItem }
constructor TRemunPerCollectionItem.Create;
begin
  FItensRemun := TRubricaCollection.Create;
  FInfoSaudeColet := nil;
end;

destructor TRemunPerCollectionItem.Destroy;
begin
  FItensRemun.Free;
  FreeAndNil(FInfoSaudeColet);
  inherited;
end;

function TRemunPerCollectionItem.getInfoSaudeColet: TInfoSaudeColet;
begin
  if not (Assigned(FInfoSaudeColet)) then
    FInfoSaudeColet := TInfoSaudeColet.Create;
  Result := FInfoSaudeColet;
end;

function TRemunPerCollectionItem.infoSaudeColetInst: boolean;
begin
  Result := Assigned(FInfoSaudeColet);
end;

{ TNfsColecao }
function TNfsColecao.Add: TNfsItem;
begin
  Result := TNfsItem(inherited Add);
  //Result.Create;
end;

constructor TNfsColecao.Create(AOwner: TPersistent);
begin
  inherited Create(TNfsItem);
end;

function TNfsColecao.GetItem(Index: Integer): TNfsItem;
begin
  Result := TNfsItem(inherited GetItem(Index));
end;

procedure TNfsColecao.SetItem(Index: Integer; const Value: TNfsItem);
begin
  inherited SetItem(Index, Value);
end;

{ TFiliacaoSindical }

function TFiliacaoSindical.Add: TFiliacaoSindicalItem;
begin
  result := TFiliacaoSindicalItem(inherited Add);
  //Result.Create;
end;

constructor TFiliacaoSindical.Create;
begin
  inherited Create(TFiliacaoSindicalItem)
end;

function TFiliacaoSindical.GetItem(Index: Integer): TFiliacaoSindicalItem;
begin
  Result := TFiliacaoSindicalItem(inherited GetItem(Index));
end;

procedure TFiliacaoSindical.SetItem(Index: Integer; const Value: TFiliacaoSindicalItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEpcEpi }

constructor TEpcEpi.Create;
begin
  FEPC := nil;
  FEpi := nil;
end;

destructor TEpcEpi.Destroy;
begin
  FreeAndNil(FEPC);
  FreeAndNil(FEpi);
end;

function TEpcEpi.getEpc: TEpcCollection;
begin
  if not Assigned(FEPC) then
    FEPC := TEpcCollection.Create;
  Result := FEPC;
end;

function TEpcEpi.epcInst: boolean;
begin
  result := Assigned(FEPC);
end;

function TEpcEpi.getEpi: TEpiCollection;
begin
  if not Assigned(FEpi) then
    FEpi := TEpiCollection.Create;
  result := FEpi;
end;

function TEpcEpi.epiInst: boolean;
begin
  result := Assigned(FEpi);
end;

{ TRemunOutrEmpr }
constructor TRemunOutrEmprCollectionItem.Create;
begin

end;

{ TRemunOutrEmprCollection }
function TRemunOutrEmprCollection.add: TRemunOutrEmprCollectionItem;
begin
  Result := TRemunOutrEmprCollectionItem(inherited add);
  Result.Create;
end;

constructor TRemunOutrEmprCollection.Create;
begin
  inherited Create(TRemunOutrEmprCollectionItem);
end;

function TRemunOutrEmprCollection.GetItem(Index: integer):
TRemunOutrEmprCollectionItem;
begin
  Result := TRemunOutrEmprCollectionItem(inherited GetItem(Index));
end;

procedure TRemunOutrEmprCollection.SetItem(Index: integer;
  Value: TRemunOutrEmprCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoMV }
constructor TInfoMV.Create;
begin
  FRemunOutrEmpr := TRemunOutrEmprCollection.Create;
end;

destructor TInfoMV.Destroy;
begin
  FRemunOutrEmpr.Free;
  inherited;
end;

{ TIdeEmpregador }

procedure TIdeEmpregador.AfterConstruction;
begin
  inherited;
  FOrgaoPublico := False;
end;

{ TObservacoesCollectionItem }

constructor TObservacoesCollectionItem.create;
begin

end;

{ TObservacoesCollection }

function TObservacoesCollection.Add: TObservacoesCollectionItem;
begin
  Result := TObservacoesCollectionItem(inherited Add);
end;

constructor TObservacoesCollection.Create();
begin
  inherited Create(TObservacoesCollectionItem);
end;

function TObservacoesCollection.GetItem(
  Index: Integer): TObservacoesCollectionItem;
begin
  Result := TObservacoesCollectionItem(inherited GetItem(Index));
end;

procedure TObservacoesCollection.SetItem(Index: Integer;
  Value: TObservacoesCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TideTrabalhador3 }

constructor TideTrabalhador3.Create;
begin
  FprocJudTrab := TprocJudTrabCollection.create;
end;

destructor TideTrabalhador3.Destroy;
begin
  FprocJudTrab.Free;

  inherited;
end;

end.
