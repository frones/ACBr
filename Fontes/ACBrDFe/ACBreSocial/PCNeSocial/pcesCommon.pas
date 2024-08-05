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

unit pcesCommon;

interface

uses
  SysUtils, Classes, Controls,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcesConversaoeSocial;

const
  dDataBrancoNula = '30/12/1899';

type
  {Classes existentes nesta unit}
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
  TEndExtV110 = class;
  TIdePais = class;
  TInfoAgNocivo = class;
  TRubricaCollectionItem = class;
  TRubricaCollection = class;
  TRecPgtosCollectionItem = class;
  TRecPgtosCollection = class;
  TInfoASO = class;
  TLocalTrabGeral = class;
  TLocalTrabDom = class;
  TLocalTempDom = class;
  TInfoCeletista = class;
  TInfoEstatutario = class;
  TInfoRegimeTrab = class;
  TAfastamento = class;
  TDesligamento = class;
  TcargoFuncao = class;
  TinfoEstagiario = class;
  TinstEnsino = class;
  TageIntegracao = class;
  TsupervisorEstagio = class;
  TVerbasResc = class;
  TQuarentena = class;
  TInfoProcJudCollection = class;
  TInfoProcJCollection = class;
  TInfoProcJudItem = class;
  TInfoProcJItem = class;
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
  TRemunOutrEmprCollectionItem = class;
  TRemunOutrEmprCollection = class;
  TInfoMV = class;
  TIdeRespInf = class;
  TObservacoesCollectionItem = class;
  TObservacoesCollection = class;
  TtransfDom = class;
  TinfoRegCTPS = class;
  TtrabImig = class;
  TtreiCapCollection = class;
  TtreiCapCollectionItem = class;
  Tcessao = class;
  TInfoRRA = class;
  TDespProcJud = class;
  TIdeAdvCollection = class;
  TIdeAdvCollectionItem = class;

  IEventoeSocial = Interface;

  TeSocial = class(TObject)
  private
    FId: string;
    FSequencial: Integer;
  public
    property Id: string read FId write FId;
    property Sequencial: Integer read FSequencial write FSequencial;
  end;

  TIdeFolhaPagto = class(TObject)
  private
    FindApuracao: tpIndApuracao;
    FperApur: string;
  public
    property indApuracao: tpIndApuracao read FindApuracao write FindApuracao;
    property perApur: string read FperApur write FperApur;
  end;

  TeSocialCollection = class(TACBrObjectList)
  public
    FACBreSocial: TComponent;
    constructor Create(AACBreSocial: TComponent); reintroduce; virtual;
  end;

  TAliqGilRat = class(TObject)
  private
    FAliqRat: tpAliqRat;
    FFap: Double;
    FAliqRatAjust: Double;
    FProcAdmJudRat: TProcAdmJudRat;
    FProcAdmJudFap: TProcAdmJudFap;
    function getProcAdmJudRat(): TProcAdmJudRat;
    function getProcAdmJudFap(): TProcAdmJudFap;
  public
    constructor Create;
    destructor Destroy; override;
    function procAdmJudRatInst(): Boolean;
    function procAdmJudFapInst(): Boolean;

    property AliqRat: tpAliqRat read FAliqRat write FAliqRat;
    property Fap: Double read FFap write FFap;
    property AliqRatAjust: Double read FAliqRatAjust write FAliqRatAjust;
    property ProcAdmJudRat: TProcAdmJudRat read getProcAdmJudRat write FProcAdmJudRat;
    property ProcAdmJudFap: TProcAdmJudFap read getProcAdmJudFap write FProcAdmJudFap;
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
    FUF: string;
  public
    property TpLograd: string read FTpLograd write FTpLograd;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property CodMunic: integer read FCodMunic write FCodMunic;
    property UF: string read FUF write FUF;
  end;

  TCNH = class
  private
    FnrRegCnh: string;
    FDtExped: TDateTime;
    FufCnh: string;
    FDtValid: TDateTime;
    FdtPriHab: TDateTime;
    FcategoriaCnh: tpCnh;
  public
    property nrRegCnh: string read FnrRegCnh write FnrRegCnh;
    property DtExped: TDateTime read FDtExped write FDtExped;
    property ufCnh: string read FufCnh write FufCnh;
    property DtValid: TDateTime read FDtValid write FDtValid;
    property dtPriHab: TDateTime read FdtPriHab write FdtPriHab;
    property categoriaCnh: tpCnh read FcategoriaCnh write FcategoriaCnh;
  end;

  TContato = class(TObject)
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
    // Até a versão ve2.05.00
    FCodCargo: string;
    FCodFuncao: string;
    FCodCarreira: string;
    FDTIngrCarr: TDate;
    // A partir da versão veS.01.00.00
    FnmCargo: string;
    FCBOCargo: string;
    FdtIngrCargo: TDate;
    FnmFuncao: string;
    FCBOFuncao: string;
    FacumCargo: tpSimNaoFacultativo;

    FCodCateg: integer;

    FRemuneracao: TRemuneracao;
    FDuracao: TDuracao;
    FLocalTrabalho: TLocalTrabalho;
    FHorContratual: THorContratual;
    FInfoAtivDesemp: TInfoAtivDesemp;
    FFiliacaoSindical: TFiliacaoSindical;
    FAlvaraJudicial: TAlvaraJudicial;
    Fobservacoes: TobservacoesCollection;
    FtreiCap: TtreiCapCollection;

    function getTreiCap: TtreiCapCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function treiCapInst(): boolean;

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
    property nmCargo: string read FnmCargo write FnmCargo;
    property CBOCargo: string read FCBOCargo write FCBOCargo;
    property dtIngrCargo: TDate read FdtIngrCargo write FdtIngrCargo;
    property nmFuncao: string read FnmFuncao write FnmFuncao;
    property CBOFuncao: string read FCBOFuncao write FCBOFuncao;
    property acumCargo: tpSimNaoFacultativo read FacumCargo write FacumCargo;
    property treiCap: TtreiCapCollection read getTreiCap write FtreiCap;
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

  TDependenteCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDependenteCollectionItem;
    procedure SetItem(Index: Integer; Value: TDependenteCollectionItem);
  public
    function Add: TDependenteCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDependenteCollectionItem;
    property Items[Index: Integer]: TDependenteCollectionItem read GetItem write SetItem; default;
  end;

  TDependenteCollectionItem = class(TObject)
  private
    FtpDep: tpTpDep;
    FnmDep: string;
    FdtNascto: TDateTime;
    FcpfDep: string;
    FdepIRRF: tpSimNao;
    FdepSF: tpSimNao;
    FsexoDep: string;
    FIncTrab: tpSimNao;
    FDescrDep: string;
  public
    property tpDep: tpTpDep read FtpDep write FtpDep;
    property nmDep: string read FnmDep write FnmDep;
    property dtNascto: TDateTime read FdtNascto write FdtNascto;
    property cpfDep: string read FcpfDep write FcpfDep;
    property depIRRF: tpSimNao read FdepIRRF write FdepIRRF;
    property depSF: tpSimNao read FdepSF write FdepSF;
    property sexoDep: string read FsexoDep write FsexoDep;
    property incTrab: tpSimNao read FIncTrab write FIncTrab;
    property incFisMen: tpSimNao read FIncTrab write FIncTrab;
    property descrDep: string read FDescrDep write FDescrDep;
  end;

  TDescAtividadeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDescAtividadeCollectionItem;
    procedure SetItem(Index: Integer; Value: TDescAtividadeCollectionItem);
  public
    function Add: TDescAtividadeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDescAtividadeCollectionItem;
    property Items[Index: Integer]: TDescAtividadeCollectionItem read GetItem write SetItem; default;
  end;

  TDescAtividadeCollectionItem = class(TObject)
  private
    FdescAtivDesemp: string;
  public
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
    FobjDet: string;
  public
    constructor Create;

    property TpContr: tpTpContr read FTpContr write FTpContr;
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
    property clauAssec: tpSimNao read FclauAssec write FclauAssec;
    property objDet: string read FobjDet write FobjDet;
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

  THorarioCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): THorarioCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioCollectionItem);
  public
    function Add: THorarioCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: THorarioCollectionItem;
    property Items[Index: Integer]: THorarioCollectionItem read GetItem write SetItem; default;
  end;

  THorarioCollectionItem = class(TObject)
  private
    FDia: tpTpDia;
    FCodHorContrat: string;
  public
    property Dia: tpTpDia read FDia write FDia;
    property CodHorContrat: string read FCodHorContrat write FCodHorContrat;
  end;

  THorarioIntervaloCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): THorarioIntervaloCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioIntervaloCollectionItem);
  public
    function Add: THorarioIntervaloCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: THorarioIntervaloCollectionItem;
    property Items[Index: Integer]: THorarioIntervaloCollectionItem read GetItem write SetItem;
  end;

  THorarioIntervaloCollectionItem = class(TObject)
  private
    FDurInterv: integer;
    FIniInterv: string;
    FTermInterv : string;
  public
    property durInterv: integer read FDurInterv write FDurInterv;
    property iniInterv: string read FIniInterv write FIniInterv;
    property termInterv: string read FTermInterv write FTermInterv;
  end;

  THorContratual = class(TObject)
  private
    fqtdHrsSem: Double;
    ftpJornada: tpTpJornada;
    fdscTpJorn: string;
    ftmpParc: tpTmpParc;
    fhorario: THorarioCollection;
    fdscJorn: string;
    fhorNoturno: tpSimNao;
  public
    constructor Create;
    destructor Destroy; override;

    property qtdHrsSem: Double read fqtdHrsSem write fqtdHrsSem;
    property tpJornada: tptpJornada read ftpJornada write ftpJornada;
    property dscTpJorn: string read fdscTpJorn write fdscTpJorn;
    property tmpParc: tptmpParc read ftmpParc write ftmpParc;
    property horario: thorarioCollection read fhorario write fhorario;
    property dscJorn: string read fdscJorn write fdscJorn;
    property horNoturno: tpSimNao read fhorNoturno write fhorNoturno;
  end;

  TInscricao = class(TObject)
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

  (*
  +---------------------------------------------------------------------------------------------------------------+
  |                                          Classes de Geração do IdeEvento                                      |
  +----------+--------+---------- +-------+-----------------------------------------------------------------------+
  |  Classe  |IndRetif|IndApuracao|IndGuia|                              Eventos                                  |
  +----------+--------+-----------+-------+-----------------------------------------------------------------------+
  |          |        |           |       |2190, 2200, 2205, 2206, 2210, 2220, 2221, 2230, 2231, 2240, 2245, 2250,|
  |IdeEvento2|    X   |           |   X   |2260, 2298, 2299, 2300, 2306, 2399, 2400, 2405, 2410, 2416, 2418, 2420 |
  +----------+--------+-----------+-------+-----------------------------------------------------------------------+
  |IdeEvento3|    X   |     X     |   X   |1200, 1202, 1207, 1210, 1250, 1260, 1270, 1280, 1300                   |
  +----------+--------+-----------+-------+-----------------------------------------------------------------------+
  |IdeEvento4|        |     X     |   X   |1295, 1298, 1299                                                       |
  +----------+--------+-----------+-------+-----------------------------------------------------------------------+

  +--------------------------------------------------------+
  |       Valores padrão nas procedures de geração         |
  +---------------+------------+---------------+-----------+
  |   procedure   |GeraIndRetif|GeraIndApuracao|GeraIndGuia|
  +---------------+------------+---------------+-----------+
  |GerarIdeEvento2|    True    |               |   False   |
  +---------------+------------+---------------+-----------+
  |GerarIdeEvento3|    True    |      True     |    True   |
  +---------------+------------+---------------+-----------+
  |GerarIdeEvento4|    True    |               |    True   |
  +---------------+------------+---------------+-----------+
  //O campo indRetif é gerado se: (GeraIndRetif = True).
  //O campo indApuracao é gerado se: (GeraIndApuracao = True).
  //O campo indGuia é gerado se: (GeraIndGuia = True) e (Leiaute >= S1.0) e (indGuia <> '').

  +-----------------------------------------------------------------------------------------------------------+
  |                                  Campos informados na geração dos eventos                                 |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |       Eventos         |    Classe   |Leiaute|   procedure   |indRetif|nrRecibo|indApuracao|perApur|indGuia|
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento |        |        |           |       |       |
  |         2190          | TIdeEvento2 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento2|    X   |    X   |           |       |       |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |2200, 2205, 2206, 2210,|             |       |               |        |        |           |       |       |
  |2220, 2221, 2230, 2231,|             |  2.5  |GerarIdeEvento2|    X   |    X   |           |       |       |
  |2240, 2245, 2250, 2260,|             |       |               |        |        |           |       |       |
  |2298, 2300, 2306, 2400,| TIdeEvento2 +-------+---------------+--------+--------+-----------+-------+-------+
  |2405, 2410, 2416, 2418,|             |       |               |        |        |           |       |       |
  |2420                   |             | S1.0  |GerarIdeEvento2|    X   |    X   |           |       |       |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento2|    X   |    X   |           |       |       |
  |      2299, 2399       | TIdeEvento2 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento2|    X   |    X   |           |       |   X   |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento3|    X   |    X   |     X     |   X   |       |
  |      1200, 1280       | TIdeEvento3 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento3|    X   |    X   |     X     |   X   |   X   |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento3|    X   |    X   |     X     |   X   |       |
  |      1202, 1207       | TIdeEvento3 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento3|    X   |    X   |     X     |   X   |       |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento3|    X   |    X   |     X     |   X   |       |
  |    1210, 1260, 1270   | TIdeEvento3 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento3|    X   |    X   |           |   X   |   X   |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |      1250, 1300       | TIdeEvento3 |  2.5  |GerarIdeEvento3|    X   |    X   |     X     |   X   |       |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |         1295          | TIdeEvento4 |  2.5  |GerarIdeEvento4|        |        |     X     |   X   |       |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  |                       |             |  2.5  |GerarIdeEvento4|        |        |     X     |   X   |       |
  |      1298, 1299       | TIdeEvento4 +-------+---------------+--------+--------+-----------+-------+-------|
  |                       |             | S1.0  |GerarIdeEvento4|        |        |     X     |   X   |   X   |
  +-----------------------+-------------+-------+---------------+--------+--------+-----------+-------+-------+
  *)

  TIdeEvento = class(TObject)
  private
    FProcEmi: TpProcEmi;
    FVerProc: string;
  public
    property ProcEmi: TpProcEmi read FProcEmi write FProcEmi;
    property VerProc: string read FVerProc write FVerProc;
  end;

  TIdeEvento2 = class(TideEvento)
  private
    FIndRetif: tpIndRetificacao;
    FNrRecibo: string;
    FindGuia: string;
  public
    property indRetif: tpIndRetificacao read FIndRetif write FIndRetif;
    property NrRecibo: string read FNrRecibo write FNrRecibo;
    property indGuia: string read FindGuia write FindGuia;
  end;

  TIdeEvento3 = class(TIdeEvento2)
  private
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
  public
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
  end;

  TIdeEvento4 = class(TIdeEvento)
  private
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
    FindGuia: string;
  public
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
    property indGuia: string read FindGuia write FindGuia;
  end;

  TIdeEvento5 = class(TObject)
  private
    FnrRecArqBase: string;
    FIndApuracao: tpIndApuracao;
    FPerApur: string;
  public
    property nrRecArqBase: string read FnrRecArqBase write FnrRecArqBase;
    property IndApuracao: tpIndApuracao read FIndApuracao write FIndApuracao;
    property perApur: string read FPerApur write FPerApur;
  end;

  TIdePeriodo = class(TObject)
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

  TIdeTrabSubstituidoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeTrabSubstituidoCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeTrabSubstituidoCollectionItem);
  public
    function Add: TIdeTrabSubstituidoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeTrabSubstituidoCollectionItem;
    property Items[Index: Integer]: TIdeTrabSubstituidoCollectionItem read GetItem write SetItem; default;
  end;

  TIdeTrabSubstituidoCollectionItem = class(TObject)
  private
    FCpfTrabSubst:  string;
  public
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

  TInfoAtivDesemp = class(TObject)
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
    FInfoCota: tpSimNaoFacultativo;
    FObservacao: string;
  public
    property DefFisica: tpSimNao read FDefFisica  write FDefFisica;
    property DefMental: tpSimNao read FDefMental  write FDefMental;
    property DefIntelectual: tpSimNao read FDefIntelectual write FDefIntelectual;
    property DefMotora: tpSimNao read FDefMotora write FDefMotora;
    property DefVisual: tpSimNao read FDefVisual write FDefVisual;
    property DefAuditiva: tpSimNao read FDefAuditiva write FDefAuditiva;
    property ReabReadap: tpSimNao read FReabReadap write FReabReadap;
    property infoCota: tpSimNaoFacultativo read FInfoCota write FInfoCota;
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
    FUf: string;
  public
    property TpLograd: String read FTpLograd write FTpLograd;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property CodMunic: integer read FCodMunic write FCodMunic;
    property Uf: string read FUf write FUf;
  end;

  TLocalTempDom = class
  private
    FTpLograd: String;
    FDscLograd: string;
    FNrLograd: string;
    FComplemento: string;
    FBairro: string;
    FCep: string;
    FCodMunic: integer;
    FUf: string;
  public
    property TpLograd: String read FTpLograd write FTpLograd;
    property DscLograd: string read FDscLograd write FDscLograd;
    property NrLograd: string read FNrLograd write FNrLograd;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cep: string read FCep write FCep;
    property CodMunic: integer read FCodMunic write FCodMunic;
    property Uf: string read FUf write FUf;
  end;

  TLocalTrabalho = class
  private
    FLocalTrabGeral: TLocalTrabGeral;
    FLocalTrabDom: TLocalTrabDom;
    FLocalTempDom: TLocalTempDom;
  public
    constructor Create;
    destructor Destroy; override;

    property LocalTrabGeral: TLocalTrabGeral read FLocalTrabGeral write FLocalTrabGeral;
    property LocalTrabDom: TLocalTrabDom read FLocalTrabDom write FLocalTrabDom;
    property LocalTempDom: TLocalTempDom read FLocalTempDom write FLocalTempDom;
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

  TProcesso = class(TObject)
  protected
    FNrProc: String;
    FCodSusp: String;
  public
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
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FmatricAnt: string;
    FdtTransf: TDateTime;
    Fobservacao: string;
  public
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property tpInscAnt: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property cnpjEmpregAnt: string read FnrInsc write FnrInsc;
    property matricAnt: string read FmatricAnt write FmatricAnt;
    property dtTransf: TDateTime read FdtTransf write FdtTransf;
    property observacao: string read Fobservacao write Fobservacao;
  end;

  TSucessaoVinc2 = class
  private
    FnrInsc: string;
    FtpInsc: tpTpInsc;
  public
    property tpInscSuc: tpTpInsc read FtpInsc write FtpInsc;
    property cnpjSucessora: string read FnrInsc write FnrInsc;
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
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

  TTrabalhador = class(TObject)
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
    FPaisNac: string;

    FNascimento: TNascimento;
    FDocumentos: TDocumentos;
    FEndereco: TEndereco;
    FTrabEstrangeiro: TTrabEstrangeiro;
    FInfoDeficiencia: TInfoDeficiencia;
    FDependente: TDependenteCollection;
    FAposentadoria: TAposentadoria;
    FContato: TContatoTrabalhador;
    FExtrangeiroSN : Boolean;
    FtrabImig: TtrabImig;
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
    property trabImig: TtrabImig read FtrabImig write FtrabImig;
    property PaisNac: string read FPaisNac write FPaisNac;
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

  TTrabTemporario = class(TObject)
  private
    FHipLeg: integer;
    FJustContr: string;
    FJustProrr: string;
    FIdeEstabVinc: TIdeEstabVinc;
    FIdeTomadorServ: TIdeTomadorServ;
    FIdeTrabSubstituido: TIdeTrabSubstituidoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property hipLeg: integer read FHipLeg write FHipLeg;
    property justContr: string read FJustContr write FJustContr;
    property justProrr: string read FJustProrr write FJustProrr;
    property IdeTomadorServ: TIdeTomadorServ read FIdeTomadorServ write FIdeTomadorServ;
    property IdeTrabSubstituido: TIdeTrabSubstituidoCollection read FIdeTrabSubstituido write FIdeTrabSubstituido;
    property IdeEstabVinc: TIdeEstabVinc read FIdeEstabVinc write FIdeEstabVinc;
  end;

  TAprend = class(TInscricao)
    FindAprend: tpIndAprend;
    FcnpjEntQual: string;
    FcnpjPrat: string;
  public
    property indAprend: tpIndAprend read FindAprend write FindAprend;
    property cnpjEntQual: string read FcnpjEntQual write FcnpjEntQual;
    property cnpjPrat: string read FcnpjPrat write FcnpjPrat;
  end;

  TInfoCeletista = class
  private
    FDtAdm: TDate;
    FTpAdmissao: tpTpAdmissao;
    FIndAdmissao: tpTpIndAdmissao;
    FNrProcTrab: String;
    FTpRegJor: tpTpRegJor;
    FNatAtividade: tpNatAtividade;
    FdtBase: Integer;
    FcnpjSindCategProf: string;
    FFGTS: TFGTS;
    FTrabTemporario: TTrabTemporario;
    FAprend: TAprend;
    FmatAnotJud: string;
  public
    constructor Create;
    destructor Destroy; override;

    property DtAdm: TDate read FDtAdm write FDtAdm;
    property TpAdmissao: tpTpAdmissao read FTpAdmissao write FTpAdmissao;
    property IndAdmissao: tpTpIndAdmissao read FIndAdmissao write FIndAdmissao;
    property nrProcTrab: string read FNrProcTrab write FNrProcTrab;
    property TpRegJor: tpTpRegJor read FTpRegJor write FTpRegJor;
    property NatAtividade: tpNatAtividade read FNatAtividade write FNatAtividade;
    property dtBase: Integer read FdtBase write FdtBase;
    property cnpjSindCategProf: string read FcnpjSindCategProf write FcnpjSindCategProf;
    property FGTS: TFGTS read FFGTS write FFGTS;
    property TrabTemporario: TTrabTemporario read FTrabTemporario write FTrabTemporario;
    property aprend: TAprend read FAprend write FAprend;
    property matAnotJud: string read FmatAnotJud write FmatAnotJud;
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
    FindTetoRGPS: tpSimNaoFacultativo;
    FindAbonoPerm: tpSimNaoFacultativo;
    FdtIniAbono: TDate;
  public
    constructor Create;
    destructor Destroy; override;

    property indProvim: tpIndProvim read FIndProvim write FIndProvim;
    property tpProv: tpTpProv read FTpProv write FTpProv;
    property dtNomeacao: TDate read FDtNomeacao write FDtNomeacao;
    property dtPosse: TDate read FDtPosse write FDtPosse;
    property dtExercicio: TDate read FDtExercicio write FDtExercicio;
    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property infoDecJud: TInfoDecJud read FInfoDecJud write FInfoDecJud;
    property indTetoRGPS: tpSimNaoFacultativo read FindTetoRGPS write FindTetoRGPS;
    property indAbonoPerm: tpSimNaoFacultativo read FindAbonoPerm write FindAbonoPerm;
    property dtIniAbono: TDate read FdtIniAbono write FdtIniAbono;
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
    FUfCRM: string;
  public
    property DtAso: TDate read FDtAso write FDtAso;
    property NrCRM: string read FNrCRM write FNrCRM;
    property UfCRM: string read FUfCRM write FUfCRM;
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

  TMudancaCPF = class
  private
    FcpfAnt: String;
    FmatricAnt: String;
    FdtAltCPF: TDate;
    Fobservacao: String;
  public
    property cpfAnt: String read FcpfAnt write FcpfAnt;
    property matricAnt: String read FmatricAnt write FmatricAnt;
    property dtAltCPF: TDate read FdtAltCPF write FdtAltCPF;
    property observacao: String read Fobservacao write Fobservacao;
  end;

  TMudancaCPF2 = class
  private
    FcpfAnt: String;
    FdtAltCPF: TDate;
    Fobservacao: String;
    FmatricAnt: String;
  public
    property cpfAnt: String read FcpfAnt write FcpfAnt;
    property dtAltCPF: TDate read FdtAltCPF write FdtAltCPF;
    property observacao: String read Fobservacao write Fobservacao;
    property matricAnt: String read FmatricAnt write FmatricAnt;
  end;

  TMudancaCPF3 = class
  private
    FnovoCPF: String;
  public
    property novoCPF: String read FnovoCPF write FnovoCPF;
  end;

  TVinculo = class
  private
    Fmatricula: string;
    FtpRegTrab: tpTpRegTrab;
    FtpRegPrev: tpTpRegPrev;
    FnrRecInfPrelim: string;
    FcadIni: tpSimNao;

    FInfoRegimeTrab: TInfoRegimeTrab;
    FInfoContrato: TInfoContrato;
    FSucessaoVinc: TSucessaoVinc;
    FtransfDom: TtransfDom;
    FAfastamento: TAfastamento;
    FDesligamento: TDesligamento;
    FInfoASO: TInfoASO;
    FMudancaCPF: TMudancaCPF;
    Fcessao: Tcessao;

  public
    constructor Create;
    destructor Destroy; override;

    property matricula: string read FMatricula write FMatricula;
    property tpRegTrab: tpTpRegTrab read FTpRegTrab write FTpRegTrab;
    property tpRegPrev: tpTpRegPrev read FTpRegPrev write FTpRegPrev;
    property nrRecInfPrelim: string read FNrRecInfPrelim write FNrRecInfPrelim;
    property cadIni: tpSimNao read FcadIni write FcadIni;

    property infoRegimeTrab: TInfoRegimeTrab read FInfoRegimeTrab write FInfoRegimeTrab;
    property infoContrato: TInfoContrato read FInfoContrato write FInfoContrato;
    property sucessaoVinc: TSucessaoVinc read FSucessaoVinc write FSucessaoVinc;
    property transfDom: TtransfDom read FtransfDom write FtransfDom;

    property mudancaCPF: TMudancaCPF read FMudancaCPF write FMudancaCPF;
    property afastamento: TAfastamento read FAfastamento write FAfastamento;
    property desligamento: TDesligamento read FDesligamento write FDesligamento;
    property infoASO: TInfoASO read FInfoASO write FInfoASO;
    property cessao: Tcessao read Fcessao write Fcessao;
  end;

  TideTrabalhador = class(TObject)  //S-2205;
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
    Fmatricula : String;
  public
    property codCateg : Integer read FcodCateg write FcodCateg;
    property matricula : String read Fmatricula write Fmatricula;
  end;

  TEmitente = class(TObject)
  private
    FnmEmit: string;
    FideOC: tpIdeOC;
    FnrOc: string;
    FufOC: string;
  public
    property nmEmit: string read FnmEmit write FnmEmit;
    property ideOC: tpIdeOC read FideOC write FideOC;
    property nrOc: string read FnrOc write FnrOc;
    property ufOC: string read FufOC write FufOC;
  end;

  TEndExt = class(TObject)
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

  TEndExtV110 = class
  private
    FEndDscLograd: string;
    FEndNrLograd: string;
    FEndComplem: string;
    FEndBairro: string;
    FEndCidade: string;
    FEndEstado: string;
    FEndCodPostal: string;
    Ftelef: string;
  public
    property endDscLograd: string read FEndDscLograd write FEndDscLograd;
    property endNrLograd: string read FEndNrLograd write FEndNrLograd;
    property endComplem: string read FEndComplem write FEndComplem;
    property endBairro: string read FEndBairro write FEndBairro;
    property endCidade: string read FEndCidade write FEndCidade;
    property endEstado: string read FEndEstado write FEndEstado;
    property endCodPostal: string read FEndCodPostal write FEndCodPostal;
    property telef: string read Ftelef write Ftelef;
  end;

  TIdePais = class(TObject)
  private
    FCodPais: string;
    FIndNIF: tpIndNIF;
    FNifBenef: string;
  public
    property codPais: string read FCodPais write FCodPais;
    property indNIF: tpIndNIF read FIndNIF write FIndNIF;
    property nifBenef: string read FNifBenef write FNifBenef;
  end;

  TInfoAgNocivo = class(TObject)
  private
    FGrauExp: tpGrauExp;
  public
    property grauExp: tpGrauExp read FGrauExp write FGrauExp;
  end;

  TRubricaCollection = class(TACBrObjectList)
  private
  public
    function GetItem(Index: Integer): TRubricaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRubricaCollectionItem);
  public
    function Add: TRubricaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRubricaCollectionItem;
    property Items[Index: Integer]: TRubricaCollectionItem read GetItem write SetItem;
      default;
  end;

  TRubricaCollectionItem = class(TObject)
  protected
    Fmatricula: string;
    FCodRubr: string;
    FIdeTabRubr: string;
    FQtdRubr: Double;
    FFatorRubr: Double;
    FVrUnit: Double;
    FVrRubr: Double;
    FindApurIR : tpindApurIR;
  public
    property matricula: string read Fmatricula write Fmatricula;
    property codRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property qtdRubr: Double read FQtdRubr write FQtdRubr;
    property fatorRubr: Double read FFatorRubr write FFatorRubr;
    property vrUnit: Double read FVrUnit write FVrUnit;
    property vrRubr: Double read FVrRubr write FVrRubr;
    property indApurIR: tpindApurIR read FindApurIR write FindApurIR;
  end;

  TReciboPagamento = class(TObject) // s1200
  private
    FIdeRecPgto: Integer;
    FvrRec : Double;
  public
    property ideRecPgto: Integer read FIdeRecPgto write FIdeRecPgto;
    property vrRec : Double read FvrRec write FvrRec;
  end;

  TRecPgtosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRecPgtosCollectionItem;
    procedure SetItem(Index: Integer; Value: TRecPgtosCollectionItem);
  public
    function Add: TRecPgtosCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRecPgtosCollectionItem;
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

  TideEstabLotCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TideEstabLotItem;
    procedure SetItem(Index: Integer; Value: TideEstabLotItem);
  public
    function Add: TideEstabLotItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TideEstabLotItem;
    property Items[Index: Integer]: TideEstabLotItem read GetItem write SetItem; default;
  end;

  TideEstabLotItem = class(TObject)
  private
    FtpInsc : tpTpInsc;
    FnrInsc : String;
    FcodLotacao : String;
    FdetVerbas : TRubricaCollection;
    FInfoSaudeColet: TInfoSaudeColet;
    FinfoAgNocivo : TInfoAgNocivo;
    FinfoSimples : TinfoSimples;

    function getinfoSaudeColet: TInfoSaudeColet;
    function getInfoAgNocivo: TInfoAgNocivo;
    function getInfoSimples: TinfoSimples;
  public
    constructor Create;
    destructor Destroy; override;
    function infoSaudeColetInst: boolean;
    function infoAgNocivoInst: boolean;
    function infoSimplesInst: Boolean;

    property tpInsc : tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc : string read FnrInsc write FnrInsc;
    property codLotacao : string read FcodLotacao write FcodLotacao;
    property detVerbas : TRubricaCollection read FdetVerbas write FdetVerbas;
    property infoSaudeColet: TInfoSaudeColet read getinfoSaudeColet write FInfoSaudeColet;
    property infoAgNocivo : TInfoAgNocivo read getInfoAgNocivo write FinfoAgNocivo;
    property infoSimples : TinfoSimples read getInfoSimples write FinfoSimples;
  end;

  TcargoFuncao = class(TObject)
  private
    FcodCargo : String;
    FcodFuncao : String;
    FnmCargo: string;
    FCBOCargo: string;
    FnmFuncao: string;
    FCBOFuncao: string;
  public
    property codCargo: String read FcodCargo write FcodCargo;
    property codFuncao: String read FcodFuncao write FcodFuncao;
    property nmCargo: string read FnmCargo write FnmCargo;
    property CBOCargo: string read FCBOCargo write FCBOCargo;
    property nmFuncao: string read FnmFuncao write FnmFuncao;
    property CBOFuncao: string read FCBOFuncao write FCBOFuncao;
  end;

  TProcJudTrabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProcJudTrabCollectionItem;
    procedure SetItem(Index: Integer; Value: TProcJudTrabCollectionItem);
  public
    function Add: TProcJudTrabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TProcJudTrabCollectionItem;
    property Items[Index: Integer]: TProcJudTrabCollectionItem read GetItem write SetItem;
  end;

  TProcJudTrabCollectionItem = class(TObject)
  private
    FTpTrib: tpTpTributo;
    FNrProcJud: string;
    FCodSusp: string;
  public
    property tpTrib: tpTpTributo read FTpTrib write FTpTrib;
    property nrProcJud: string read FNrProcJud write FNrProcJud;
    property codSusp: string read FCodSusp write FCodSusp;
  end;

  TinfoEstagiario = class(TObject)
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

  TinstEnsino = class(TObject)
  private
    FcnpjInstEnsino : String;
    FnmRazao : String;
    FdscLograd : String;
    FnrLograd : String;
    Fbairro : String;
    FCep : String;
    FcodMunic : Integer;
    FUf : String;
  public
    property cnpjInstEnsino : String read FcnpjInstEnsino write FcnpjInstEnsino;
    property nmRazao : String read FnmRazao write FnmRazao;
    property dscLograd : String read FdscLograd write FdscLograd;
    property nrLograd : String read FnrLograd write FnrLograd;
    property bairro : String read Fbairro write Fbairro;
    property Cep : String read FCep write FCep;
    property codMunic : Integer read FcodMunic write FcodMunic;
    property Uf : String read FUf write FUf;
  end;

  TageIntegracao = class(TObject)
  private
    FcnpjAgntInteg : String;
    FnmRazao : String;
    FdscLograd : String;
    FnrLograd : String;
    Fbairro : String;
    FCep : String;
    FcodMunic : Integer;
    FUf : string;
  public
    property cnpjAgntInteg : String read FcnpjAgntInteg write FcnpjAgntInteg;
    property nmRazao : String read FnmRazao write FnmRazao;
    property dscLograd : String read FdscLograd write FdscLograd;
    property nrLograd : String read FnrLograd write FnrLograd;
    property bairro : String read Fbairro write Fbairro;
    property Cep : String read FCep write FCep;
    property codMunic : Integer read FcodMunic write FcodMunic;
    property Uf : string read FUf write FUf;
  end;

  TsupervisorEstagio = class(TObject)
  private
    FcpfSupervisor : String;
    FnmSuperv : String;
  public
    property cpfSupervisor : String read FcpfSupervisor write FcpfSupervisor;
    property nmSuperv : String read FnmSuperv write FnmSuperv;
  end;

  TQuarentena = class(TObject)
  private
    FdtFimQuar : TDate;
  public
    property dtFimQuar: TDate read FdtFimQuar write FdtFimQuar;
  end;

  TVerbasResc = class(TObject)
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

  TdetVerbasCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetVerbasItem;
    procedure SetItem(Index: Integer; Value: TdetVerbasItem);
  public
    function Add: TdetVerbasItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetVerbasItem;
    property Items[Index: Integer]: TdetVerbasItem read GetItem write SetItem; default;
  end;

  TdetVerbasItem = class(TObject)
  protected
    FcodRubr : String;
    FideTabRubr : String;
    FqtdRubr : Double;
    FvrUnit : Double;
    FvrRubr : Double;
    FindApurIR : tpindApurIR;
  public
    property codRubr : String read FcodRubr write FcodRubr;
    property ideTabRubr : String read FideTabRubr write FideTabRubr;
    property qtdRubr : Double read FqtdRubr write FqtdRubr;
    property vrUnit : Double read FvrUnit write FvrUnit;
    property vrRubr : DOuble read FvrRubr write FvrRubr;
    property indApurIR : tpindApurIR read FindApurIR write FindApurIR;
  end;

  TinfoSimples = class(TObject)
  private
    FindSimples : tpIndSimples;
  public
    property indSimples : tpIndSimples read FindSimples write FindSimples;
  end;

  TInfoProcJudCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoProcJudItem;
    procedure SetItem(Index: Integer; Value: TInfoProcJudItem);
  public
    function Add: TInfoProcJudItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoProcJudItem;
    property Items[Index: Integer]: TInfoProcJudItem read GetItem write SetItem; default;
  end;

  TInfoProcJudItem = class(TObject)
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

  TInfoProcJCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoProcJItem;
    procedure SetItem(Index: Integer; Value: TInfoProcJItem);
  public
    function Add: TInfoProcJItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoProcJItem;
    property Items[Index: Integer]: TInfoProcJItem read GetItem write SetItem; default;
  end;

  TInfoProcJItem = class(TObject)
  private
    FnrProcJud: string;
    FCodSusp: Integer;
    FvrCPNRet: Double;
    FvrRatNRet: Double;
    FvrSenarNRet: Double;
  public
    property nrProcJud: string read FnrProcJud write FnrProcJud;
    property codSusp: Integer read FCodSusp write FCodSusp;
    property vrCPNRet: Double read FvrCPNRet write FvrCPNRet;
    property vrRatNRet: Double read FvrRatNRet write FvrRatNRet;
    property vrSenarNRet: Double read FvrSenarNRet write FvrSenarNRet;
  end;

  TPensaoAlimCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPensaoAlimCollectionItem;
    procedure SetItem(Index: Integer; Value: TPensaoAlimCollectionItem);
  public
    function Add: TPensaoAlimCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TPensaoAlimCollectionItem;
    property Items[Index: Integer]: TPensaoAlimCollectionItem read GetItem write SetItem;
  end;

  TPensaoAlimCollectionItem = class(TObject)
  private
    FCpfBenef: string;
    FDtNasctoBenef: TDate;
    FNmBenefic: string;
    FVlrPensao: Double;
  public
    property cpfBenef: string read FCpfBenef write FCpfBenef;
    property dtNasctoBenef: TDate read FDtNasctoBenef write FDtNasctoBenef;
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property vlrPensao: Double read FVlrPensao write FVlrPensao;
  end;

  TDetPlanoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetPlanoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetPlanoCollectionItem);
  public
    function Add: TDetPlanoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetPlanoCollectionItem;
    property Items[Index: Integer]: TDetPlanoCollectionItem read GetItem write SetItem;
  end;

  TDetPlanoCollectionItem = class(TObject)
  private
    FTpDep: tpTpDep;
    FCpfDep: string;
    FDtNascto: TDate;
    FNmDep: string;
    FVlrPgDep: Double;
  public
    property tpDep: tpTpDep read FTpDep write FTpDep;
    property cpfDep: string read FCpfDep write FCpfDep;
    property dtNascto: TDate read FDtNascto write FDtNascto;
    property nmDep: string read FNmDep write FNmDep;
    property vlrPgDep: Double read FVlrPgDep write FVlrPgDep;
  end;

  TDetOperCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetOperCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetOperCollectionItem);
  public
    function Add: TDetOperCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetOperCollectionItem;
    property Items[Index: Integer]: TDetOperCollectionItem read GetItem write SetItem;
  end;

  TDetOperCollectionItem = class(TObject)
  private
    FCnpjOper: string;
    FRegANS: string;
    FVrPgTit: Double;
    FDetPlanoCollection: TDetPlanoCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property cnpjOper: string read FCnpjOper write FCnpjOper;
    property regANS: string read FRegANS write FRegANS;
    property vrPgTit: Double read FVrPgTit write FVrPgTit;
    property detPlano: TDetPlanoCollection read FDetPlanoCollection write FDetPlanoCollection;
  end;

  TInfoSaudeColet = class(TObject)
  private
    FDetOper: TDetOperCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property detOper: TDetOperCollection read FDetOper write FDetOper;

  end;

  TRemunPerCollectionItem = class(TObject)
  private
    FMatricula: string;
    FItensRemun: TRubricaCollection;
    FInfoSaudeColet: TInfoSaudeColet;
    function getInfoSaudeColet: TInfoSaudeColet;
  public
    constructor Create;
    destructor Destroy; override;
    function infoSaudeColetInst(): boolean;

    property matricula: string read FMatricula write FMatricula;
    property itensRemun: TRubricaCollection read FItensRemun write FItensRemun;
    property infoSaudeColet: TInfoSaudeColet read getInfoSaudeColet
      write FInfoSaudeColet;
  end;

  TNfsColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNfsItem;
    procedure SetItem(Index: Integer; const Value: TNfsItem);
  public
    function Add: TNfsItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TNfsItem;
    property Items[Index: Integer]: TNfsItem read GetItem write SetItem;
  end;

  TNfsItem = class(TObject)
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

  TRemunOutrEmprCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TRemunOutrEmprCollectionItem;
    procedure SetItem(Index: integer; Value: TRemunOutrEmprCollectionItem);
  public
    function Add: TRemunOutrEmprCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRemunOutrEmprCollectionItem;
    property Items[Index: integer]: TRemunOutrEmprCollectionItem
      read GetItem write SetItem;
  end;

  TRemunOutrEmprCollectionItem = class(TObject)
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FCodCateg: integer;
    FVlrRemunOE: double;
  public
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property codCateg: integer read FCodCateg write FCodCateg;
    property vlrRemunOE: double read FVlrRemunOE write FVlrRemunOE;
  end;

  TInfoMV = class(TObject)
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

  TObservacoesCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TObservacoesCollectionItem;
    procedure SetItem(Index: Integer; Value: TObservacoesCollectionItem);
  public
    function Add: TObservacoesCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TObservacoesCollectionItem;
    property Items[Index: Integer]: TObservacoesCollectionItem read GetItem write SetItem; default;
  end;

  TObservacoesCollectionItem = class(TObject)
  private
    Fobservacao: string;
  public
    property observacao: string read Fobservacao write Fobservacao;
  end;

  TinfoRegCTPS = class(TObject)
  private
    FCBOcargo: string;
    FVrSalFx: double;
    FUndSalFixo: tpUndSalFixo;
    FTpContr: tpTpContr;
    FdtTerm: TDateTime;
  public
    property CBOCargo: string read FCBOcargo write FCBOcargo;
    property vrSalFx: double read FVrSalFx write FVrSalFx;
    property undSalFixo: tpUndSalFixo read FUndSalFixo write FUndSalFixo;
    property tpContr: tpTpContr read FTpContr write FTpContr;
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
  end;

  TtrabImig = class(TObject)
  private
   FtmpResid: tpTmpResid;
   FcondIng: tpCondIng;
  public
   property tmpResid: tpTmpResid read FtmpResid write FtmpResid;
   property condIng: tpCondIng read FcondIng write FcondIng;
  end;

  TtreiCapCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TtreiCapCollectionItem;
    procedure SetItem(Index: Integer; Value: TtreiCapCollectionItem);
  public
    function Add: TtreiCapCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TtreiCapCollectionItem;
    property Items[Index: Integer]: TtreiCapCollectionItem read GetItem write SetItem; default;
  end;

  TtreiCapCollectionItem = class(TObject)
  private
   FcodTreiCap: Integer;
  public
   property codTreiCap: Integer read FcodTreiCap write FcodTreiCap;
  end;

  Tcessao = class(TObject)
  private
   FdtIniCessao: TDate;
  public
   property dtIniCessao: TDate read FdtIniCessao write FdtIniCessao;
  end;

  TInfoRRA = class(TObject)
  private
    FtpProcRRA: tpTpProcRRA;
    FnrProcRRA: string;
    FdescRRA: string;
    FqtdMesesRRA: Double;
    FDespProcJud: TDespProcJud;
    FIdeAdv: TIdeAdvCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    function getIdeAdv(): TIdeAdvCollection;
    function instIdeAdv(): boolean;
    function getDespProcJud(): TDespProcJud;
    function instDespProcJud(): boolean;

    property tpProcRRA: tpTpProcRRA read FtpProcRRA write FtpProcRRA;
    property nrProcRRA: string read FnrProcRRA write FnrProcRRA;
    property descRRA: string read FdescRRA write FdescRRA;
    property qtdMesesRRA: Double read FqtdMesesRRA write FqtdMesesRRA;
    property despProcJud: TDespProcJud read getDespProcJud write FDespProcJud;
    property ideAdv: TIdeAdvCollection read getIdeAdv write FideAdv;
  end;

  TDespProcJud = class(TObject)
  private
    FvlrDespCustas: Double;
    FvlrDespAdvogados: Double;
  public
    property vlrDespCustas: Double read FvlrDespCustas write FvlrDespCustas;
    property vlrDespAdvogados: Double read FvlrDespAdvogados write FvlrDespAdvogados;
  end;

  TIdeAdvCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeAdvCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeAdvCollectionItem);
  public
    function New: TIdeAdvCollectionItem;
    property Items[Index: Integer]: TIdeAdvCollectionItem read GetItem write SetItem; default;
  end;

  TIdeAdvCollectionItem = class(TObject)
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FvlrAdv: Double;
  public
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property vlrAdv: Double read FvlrAdv write FvlrAdv;
  end;

  IEventoeSocial = Interface(IInterface)
    ['{93160D81-FE11-454A-ACA7-DA357D618F82}']
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

function TAliqGilRat.getProcAdmJudFap: TProcAdmJudFap;
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
  Result := Self.New;
end;

function TDependenteCollection.GetItem(
  Index: Integer): TDependenteCollectionItem;
begin
  Result := TDependenteCollectionItem(inherited Items[Index]);
end;

procedure TDependenteCollection.SetItem(Index: Integer;
  Value: TDependenteCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDependenteCollection.New: TDependenteCollectionItem;
begin
  Result := TDependenteCollectionItem.Create;
  Self.Add(Result);
end;

{ TTrabalhador }
constructor TTrabalhador.Create;
begin
  inherited;
  FNascimento      := TNascimento.Create;
  FDocumentos      := TDocumentos.Create;
  FEndereco        := TEndereco.Create;
  FTrabEstrangeiro := TTrabEstrangeiro.Create;
  FInfoDeficiencia := TInfoDeficiencia.Create;
  FDependente      := TDependenteCollection.Create;
  FAposentadoria   := TAposentadoria.Create;
  FContato         := TContatoTrabalhador.Create;
  FtrabImig        := TtrabImig.Create;
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
  FtrabImig.Free;

  inherited;
end;

{ TTrabTemporario }
constructor TTrabTemporario.Create;
begin
  inherited;
  FIdeTomadorServ     := TIdeTomadorServ.Create;
  FIdeTrabSubstituido := TIdeTrabSubstituidoCollection.Create;
  FIdeEstabVinc       := TIdeEstabVinc.Create;;
end;

destructor TTrabTemporario.Destroy;
begin
  FIdeTomadorServ.Free;
  FIdeTrabSubstituido.Free;
  FIdeEstabVinc.Free;
  inherited;
end;

{ TIdeTomadorServ }
constructor TIdeTomadorServ.Create;
begin
  inherited Create;
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
  Result := Self.New;
end;

function THorarioCollection.GetItem(Index: Integer): THorarioCollectionItem;
begin
  Result := THorarioCollectionItem(inherited Items[Index]);
end;

procedure THorarioCollection.SetItem(Index: Integer;
  Value: THorarioCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function THorarioCollection.New: THorarioCollectionItem;
begin
  Result := THorarioCollectionItem.Create;
  Self.Add(Result);
end;

{ THorarioIntervaloCollection }

function THorarioIntervaloCollection.Add: THorarioIntervaloCollectionItem;
begin
  Result := Self.New;
end;

function THorarioIntervaloCollection.GetItem(
  Index: Integer): THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem(inherited Items[Index]);
end;

procedure THorarioIntervaloCollection.SetItem(Index: Integer;
  Value: THorarioIntervaloCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function THorarioIntervaloCollection.New: THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem.Create;
  Self.Add(Result);
end;

{ THorContratual }
constructor THorContratual.Create;
begin
  inherited;
  FHorario:= THorarioCollection.Create;
end;

destructor THorContratual.Destroy;
begin
  FHorario.Free;
  inherited;
end;

{ TDescAtividadeCollection }
function TDescAtividadeCollection.Add: TDescAtividadeCollectionItem;
begin
  Result := Self.New;
end;

function TDescAtividadeCollection.GetItem(Index: Integer): TDescAtividadeCollectionItem;
begin
  Result := TDescAtividadeCollectionItem(inherited Items[Index]);
end;

procedure TDescAtividadeCollection.SetItem(Index: Integer; Value: TDescAtividadeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDescAtividadeCollection.New: TDescAtividadeCollectionItem;
begin
  Result := TDescAtividadeCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoAtivDesemp }
constructor TInfoAtivDesemp.Create;
begin
  inherited;
  FDescAtividade:= TDescAtividadeCollection.Create;
end;

destructor TInfoAtivDesemp.Destroy;
begin
  FDescAtividade.Free;
  inherited;
end;

{ TContrato }
constructor TInfoContrato.Create;
begin
  inherited Create;
  FRemuneracao      := TRemuneracao.Create;
  FDuracao          := TDuracao.Create;
  FLocalTrabalho    := TLocalTrabalho.Create;
  FHorContratual    := THorContratual.Create;
  FInfoAtivDesemp   := TInfoAtivDesemp.Create;
  FFiliacaoSindical := TFiliacaoSindical.Create;
  FAlvaraJudicial   := TAlvaraJudicial.Create;
  Fobservacoes      := TobservacoesCollection.Create;
  FtreiCap          := nil;
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
  FreeAndNil(FtreiCap);

  inherited;
end;

function TInfoContrato.treiCapInst: boolean;
begin
  Result := Assigned(FtreiCap);
end;

function TInfoContrato.getTreiCap: TtreiCapCollection;
begin
  if not(Assigned(FtreiCap)) then
    FtreiCap := TtreiCapCollection.Create;
  Result := FtreiCap;
end;

{ TVinculo }
constructor TVinculo.Create;
begin
  FInfoRegimeTrab := TInfoRegimeTrab.Create;
  FInfoContrato := TInfoContrato.Create;
  FSucessaoVinc := TSucessaoVinc.Create;
  ftransfDom := TtransfDom.Create;
  FMudancaCPF := TMudancaCPF.Create;
  FAfastamento := TAfastamento.Create;
  FDesligamento := TDesligamento.Create;
  FInfoASO := TInfoASO.Create;
  Fcessao := Tcessao.Create;
end;

destructor TVinculo.Destroy;
begin
  FInfoRegimeTrab.Free;
  FInfoContrato.Free;
  FSucessaoVinc.Free;
  FtransfDom.Free;
  FMudancaCPF.Free;
  FAfastamento.Free;
  FDesligamento.Free;
  FInfoASO.Free;
  Fcessao.Free;
  inherited;
end;

{ TRubricaCollection }
function TRubricaCollection.Add: TRubricaCollectionItem;
begin
  Result := Self.New;
end;

function TRubricaCollection.GetItem(Index: Integer): TRubricaCollectionItem;
begin
  Result := TRubricaCollectionItem(inherited Items[Index]);
end;

procedure TRubricaCollection.SetItem(Index: Integer;
  Value: TRubricaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRubricaCollection.New: TRubricaCollectionItem;
begin
  Result := TRubricaCollectionItem.Create;
  Self.Add(Result);
end;

{ TRecPgtosCollection }
function TRecPgtosCollection.Add: TRecPgtosCollectionItem;
begin
  Result := Self.New;
end;

function TRecPgtosCollection.GetItem(Index: Integer): TRecPgtosCollectionItem;
begin
  Result := TRecPgtosCollectionItem(inherited Items[Index]);
end;

procedure TRecPgtosCollection.SetItem(Index: Integer;
  Value: TRecPgtosCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRecPgtosCollection.New: TRecPgtosCollectionItem;
begin
  Result := TRecPgtosCollectionItem.Create;
  Self.Add(Result);
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
  FTpPlanRP := prpNenhum;
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
  FLocalTempDom := TLocalTempDom.Create;
end;

destructor TLocalTrabalho.Destroy;
begin
  FLocalTrabGeral.Free;
  FLocalTrabDom.Free;
  FLocalTempDom.Free;
  inherited;
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
  inherited Create;
  FProcJudTrab := nil;
  FInfoMV      := nil;
end;

destructor TVerbasResc.Destroy;
begin
  FreeAndNil(FProcJudTrab);
  FreeAndNil(FInfoMV);
  inherited;
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
  Result := Self.New;
end;

function TInfoProcJudCollection.GetItem(Index: Integer): TInfoProcJudItem;
begin
  Result := TInfoProcJudItem(inherited Items[Index]);
end;

procedure TInfoProcJudCollection.SetItem(Index: Integer; Value: TInfoProcJudItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoProcJudCollection.New: TInfoProcJudItem;
begin
  Result := TInfoProcJudItem.Create;
  Self.Add(Result);
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
  Result := Self.New;
end;

function TdetVerbasCollection.GetItem(Index: Integer): TdetVerbasItem;
begin
  Result := TdetVerbasItem(inherited Items[Index]);
end;

procedure TdetVerbasCollection.SetItem(Index: Integer; Value: TdetVerbasItem);
begin
  inherited Items[Index] := Value;
end;

function TdetVerbasCollection.New: TdetVerbasItem;
begin
  Result := TdetVerbasItem.Create;
  Self.Add(Result);
end;

{ TideEstabLotCollection }

function TideEstabLotCollection.Add: TideEstabLotItem;
begin
  Result := Self.New;
end;

function TideEstabLotCollection.GetItem(Index: Integer): TideEstabLotItem;
begin
  Result := TideEstabLotItem(inherited Items[Index]);
end;

procedure TideEstabLotCollection.SetItem(Index: Integer; Value: TideEstabLotItem);
begin
  inherited Items[Index] := Value;
end;

function TideEstabLotCollection.New: TideEstabLotItem;
begin
  Result := TideEstabLotItem.Create;
  Self.Add(Result);
end;

{ TideEstabLotItem }

constructor TideEstabLotItem.Create;
begin
  inherited Create;
  FdetVerbas      := TRubricaCollection.Create;
  FInfoSaudeColet := nil;
  FinfoAgNocivo   := nil;
  FinfoSimples    := nil;
end;

destructor TideEstabLotItem.Destroy;
begin
  FdetVerbas.Free;

  if Assigned(FinfoAgNocivo) then
    FreeAndNil(FInfoAgNocivo);

  inherited;
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

function TideEstabLotItem.getinfoSaudeColet: TInfoSaudeColet;
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

{ TProcJudTrabCollection }
function TProcJudTrabCollection.Add: TProcJudTrabCollectionItem;
begin
  Result := Self.New;
end;

function TProcJudTrabCollection.GetItem(
  Index: Integer): TProcJudTrabCollectionItem;
begin
  Result := TProcJudTrabCollectionItem(inherited Items[Index]);
end;

procedure TProcJudTrabCollection.SetItem(Index: Integer;
  Value: TProcJudTrabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TProcJudTrabCollection.New: TProcJudTrabCollectionItem;
begin
  Result := TProcJudTrabCollectionItem.Create;
  Self.Add(Result);
end;

{ TPensaoAlimCollection }
function TPensaoAlimCollection.Add: TPensaoAlimCollectionItem;
begin
  Result := Self.New;
end;

function TPensaoAlimCollection.GetItem(
  Index: Integer): TPensaoAlimCollectionItem;
begin
  Result := TPensaoAlimCollectionItem(inherited Items[Index]);
end;

procedure TPensaoAlimCollection.SetItem(Index: Integer;
  Value: TPensaoAlimCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TPensaoAlimCollection.New: TPensaoAlimCollectionItem;
begin
  Result := TPensaoAlimCollectionItem.Create;
  Self.Add(Result);
end;

{ TDetPlanoCollection }
function TDetPlanoCollection.Add: TDetPlanoCollectionItem;
begin
  Result := Self.New;
end;

function TDetPlanoCollection.GetItem(Index: Integer): TDetPlanoCollectionItem;
begin
  Result := TDetPlanoCollectionItem(inherited Items[Index]);
end;

procedure TDetPlanoCollection.SetItem(Index: Integer;
  Value: TDetPlanoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetPlanoCollection.New: TDetPlanoCollectionItem;
begin
  Result := TDetPlanoCollectionItem.Create;
  Self.Add(Result);
end;

{ TDetOperCollectionItem }
constructor TDetOperCollectionItem.create;
begin
  inherited Create;
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
  Result := Self.New;
end;

function TDetOperCollection.GetItem(Index: Integer): TDetOperCollectionItem;
begin
  Result := TDetOperCollectionItem(inherited Items[Index]);
end;

procedure TDetOperCollection.SetItem(Index: Integer;
  Value: TDetOperCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetOperCollection.New: TDetOperCollectionItem;
begin
  Result := TDetOperCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoSaudeColet }
constructor TInfoSaudeColet.Create;
begin
  inherited Create;
  FDetOper := TDetOperCollection.Create;
end;

destructor TInfoSaudeColet.destroy;
begin
  FDetOper.Free;
  inherited;
end;

{TIdeTrabSubstituidoCollection}

function TIdeTrabSubstituidoCollection.Add: TIdeTrabSubstituidoCollectionItem;
begin
  Result := Self.New;
end;

function TIdeTrabSubstituidoCollection.GetItem(Index: Integer): TIdeTrabSubstituidoCollectionItem;
begin
  Result := TIdeTrabSubstituidoCollectionItem(inherited Items[Index]);
end;

procedure TIdeTrabSubstituidoCollection.SetItem(Index: Integer; Value: TIdeTrabSubstituidoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeTrabSubstituidoCollection.New: TIdeTrabSubstituidoCollectionItem;
begin
  Result := TIdeTrabSubstituidoCollectionItem.Create;
  Self.Add(Result);
end;

{ TRemunPerCollectionItem }
constructor TRemunPerCollectionItem.Create;
begin
  inherited Create;
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
  Result := Self.New;
end;

function TNfsColecao.GetItem(Index: Integer): TNfsItem;
begin
  Result := TNfsItem(inherited Items[Index]);
end;

procedure TNfsColecao.SetItem(Index: Integer; const Value: TNfsItem);
begin
  inherited Items[Index] := Value;
end;

function TNfsColecao.New: TNfsItem;
begin
  Result := TNfsItem.Create;
  Self.Add(Result);
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
  Result := TFiliacaoSindicalItem(inherited Items[Index]);
end;

procedure TFiliacaoSindical.SetItem(Index: Integer; const Value: TFiliacaoSindicalItem);
begin
  inherited Items[Index] := Value;
end;

{ TRemunOutrEmprCollection }

function TRemunOutrEmprCollection.Add: TRemunOutrEmprCollectionItem;
begin
  Result := Self.New;
end;

function TRemunOutrEmprCollection.GetItem(Index: integer):
TRemunOutrEmprCollectionItem;
begin
  Result := TRemunOutrEmprCollectionItem(inherited Items[Index]);
end;

procedure TRemunOutrEmprCollection.SetItem(Index: integer;
  Value: TRemunOutrEmprCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRemunOutrEmprCollection.New: TRemunOutrEmprCollectionItem;
begin
  Result := TRemunOutrEmprCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoMV }

constructor TInfoMV.Create;
begin
  inherited Create;
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

{ TObservacoesCollection }

function TObservacoesCollection.Add: TObservacoesCollectionItem;
begin
  Result := Self.New;
end;

function TObservacoesCollection.GetItem(
  Index: Integer): TObservacoesCollectionItem;
begin
  Result := TObservacoesCollectionItem(inherited Items[Index]);
end;

procedure TObservacoesCollection.SetItem(Index: Integer;
  Value: TObservacoesCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TObservacoesCollection.New: TObservacoesCollectionItem;
begin
  Result := TObservacoesCollectionItem.Create;
  Self.Add(Result);
end;

{ TideTrabalhador3 }

constructor TideTrabalhador3.Create;
begin
  inherited Create;

  FprocJudTrab := TprocJudTrabCollection.create;
end;

destructor TideTrabalhador3.Destroy;
begin
  FprocJudTrab.Free;

  inherited;
end;

{ TInfoProcJCollection }

function TInfoProcJCollection.Add: TInfoProcJItem;
begin
  Result := Self.New;
end;

function TInfoProcJCollection.GetItem(Index: Integer): TInfoProcJItem;
begin
  Result := TInfoProcJItem(inherited Items[Index]);
end;

procedure TInfoProcJCollection.SetItem(Index: Integer; Value: TInfoProcJItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoProcJCollection.New: TInfoProcJItem;
begin
  Result := TInfoProcJItem.Create;
  Self.Add(Result);
end;

{ TeSocialCollection }

constructor TeSocialCollection.Create(AACBreSocial: TComponent);
begin
  inherited Create;
  FACBreSocial := AACBreSocial;
end;

{ TtreiCapCollection }

function TtreiCapCollection.Add: TtreiCapCollectionItem;
begin
  Result := Self.New;
end;

function TtreiCapCollection.GetItem(Index: Integer): TtreiCapCollectionItem;
begin
  Result := TtreiCapCollectionItem(inherited Items[Index]);
end;

procedure TtreiCapCollection.SetItem(Index: Integer; Value: TtreiCapCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TtreiCapCollection.New: TtreiCapCollectionItem;
begin
  Result := TtreiCapCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoRRA }

constructor TInfoRRA.Create;
begin
  inherited Create;

  FDespProcJud := nil;
  FIdeAdv := nil;
end;

destructor TInfoRRA.Destroy;
begin
  if instDespProcJud () then
    FreeAndNil(FDespProcJud);

  if Assigned(FIdeAdv) then
    FreeAndNil(FIdeAdv);

  inherited;
end;

function TInfoRRA.getIdeAdv: TIdeAdvCollection;
begin
  if not Assigned(FIdeAdv) then
    FIdeAdv := TIdeAdvCollection.Create;
  Result := FIdeAdv;
end;

function TInfoRRA.instIdeAdv: boolean;
begin
  Result := Assigned(FideAdv);
end;

function TInfoRRA.getDespProcJud: TDespProcJud;
begin
  if not Assigned(FDespProcJud) then
    FDespProcJud := TDespProcJud.Create;
  Result := FDespProcJud;
end;

function TInfoRRA.InstDespProcJud: boolean;
begin
  Result := Assigned(FDespProcJud);
end;

{ TIdeAdvCollection }

function TIdeAdvCollection.GetItem(Index: Integer): TIdeAdvCollectionItem;
begin
  Result := TIdeAdvCollectionItem(inherited Items[Index]);
end;

procedure TIdeAdvCollection.SetItem(Index: Integer; Value: TIdeAdvCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeAdvCollection.New: TIdeAdvCollectionItem;
begin
  Result := TIdeAdvCollectionItem.Create;
  Self.Add(Result);
end;

{ TDuracao }

constructor TDuracao.Create;
begin
  FTpContr := PrazoNaoAplicavel;
end;

end.
