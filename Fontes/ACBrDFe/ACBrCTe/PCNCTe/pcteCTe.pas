////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/cte                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_cte/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
// Desenvolvimento                                                            //
//         de CTe: Wiliam Zacarias da Silva Rosa                              //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteCTe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnConversao, pcteProcCTe, pcteSignature, pcteConversaoCTe, pcnGerador;

{$IFDEF PL_103}
 {$I pcteCTe_V103.inc}
{$ENDIF}

{$IFDEF PL_104}
 {$I pcteCTe_V104.inc}
{$ENDIF}

{$IFDEF PL_200}
// {$I pcteCTe_V200.inc}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              Definição de classes para a versão 2.00                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type
  TCTe    = class;
  TInfCTe = class;
  TIde    = class;

  TToma03   = class;
  TToma4    = class;
  TEndereco = class;

  TinfPercursoCollection       = class;
  TinfPercursoCollectionItem   = class;

  TCompl = class;

  TFluxo              = class;
  TPassCollection     = class;
  TPassCollectionItem = class;

  TEntrega   = class;
  TSemData   = class;
  TComData   = class;
  TNoPeriodo = class;
  TSemHora   = class;
  TComHora   = class;
  TNoInter   = class;

  TObsContCollection      = class;
  TObsContCollectionItem  = class;
  TObsFiscoCollection     = class;
  TObsFiscoCollectionItem = class;

  TEmit      = class;
  TEnderEmit = class;

  TToma      = class;

  TRem       = class;
  TLocColeta = class;

  TExped = class;

  TReceb = class;

  TDest   = class;
  TLocEnt = class;

  TVPrest             = class;
  TCompCollection     = class;
  TCompCollectionItem = class;

  TImp         = class;
  TICMS        = class;
  TCST00       = class;
  TCST20       = class;
  TCST45       = class;
  TCST60       = class;
  TCST90       = class;
  TICMSOutraUF = class;
  TICMSSN      = class;
  TICMSUFFim   = class;
  TinfTribFed  = class;

////////////////////////////////////////////////////////////////////////////////
// Informações de um CT-e Normal
////////////////////////////////////////////////////////////////////////////////
  TInfCTeNorm = class;

  TinfServico              = class;
  TinfDocRefCollection     = class;
  TinfDocRefCollectionItem = class;

  TInfCarga           = class;
  TInfQCollection     = class;
  TInfQCollectionItem = class;

  TInfDoc = class;

  TInfNFCollection           = class;
  TInfNFCollectionItem       = class;
  TinfUnidTranspNFCollection = class;
  TinfUnidCargaNFCollection  = class;

  TinfUnidTranspCollectionItem = class;
  TlacUnidTranspCollection     = class;
  TlacUnidTranspCollectionItem = class;
  TinfUnidCargaCollection      = class;
  TinfUnidCargaCollectionItem  = class;
  TlacUnidCargaCollection      = class;
  TlacUnidCargaCollectionItem  = class;

  TInfNFeCollection           = class;
  TInfNFeCollectionItem       = class;
  TinfUnidTranspNFeCollection = class;
  TinfUnidCargaNFeCollection  = class;

  TInfOutrosCollection           = class;
  TInfOutrosCollectionItem       = class;
  TinfUnidTranspOutrosCollection = class;
  TinfUnidCargaOutrosCollection  = class;

  TDocAnt                    = class;
  TEmiDocAntCollection       = class;
  TEmiDocAntCollectionItem   = class;
  TIdDocAntCollection        = class;
  TIdDocAntCollectionItem    = class;
  TIdDocAntPapCollection     = class;
  TIdDocAntPapCollectionItem = class;
  TIdDocAntEleCollection     = class;
  TIdDocAntEleCollectionItem = class;

  TSegCollection     = class;
  TSegCollectionItem = class;

  TinfGlobalizado = class;

  TinfServVinc                   = class;
  TinfCTeMultimodalCollection     = class;
  TinfCTeMultimodalCollectionItem = class;

////////////////////////////////////////////////////////////////////////////////

  // Informações do modal Rodoviário
  TRodo                  = class;
  TOccCollection         = class;
  TOccCollectionItem     = class;
  TEmiOCC                = class;
  TValePedCollection     = class;
  TValePedCollectionItem = class;
  TVeicCollection        = class;
  TVeicCollectionItem    = class;
  TProp                  = class;
  TLacRodoCollection     = class;
  TLacRodoCollectionItem = class;
  TMotoCollection        = class;
  TMotoCollectionItem    = class;

  TRodoOS                = class;
  TVeicOS                = class;
  TPropOS                = class;

  // Informações do modal Aéreo
  TAereo    = class;
  TTarifa   = class;
  TNatCarga = class;
  TpInfManuCollection     = class;
  TpInfManuCollectionItem = class;

  // Informações do modal Aquaviário
  TAquav                 = class;
  TBalsaCollection       = class;
  TBalsaCollectionItem   = class;
  TdetContCollection     = class;
  TdetContCollectionItem = class;
  TLacreCollection       = class;
  TLacreCollectionItem   = class;

  TInfDocAquav = class;

  TInfNFAquavCollection      = class;
  TInfNFAquavCollectionItem  = class;
  TInfNFeAquavCollection     = class;
  TInfNFeAquavCollectionItem = class;

  // Informações do modal Ferroviário
  TFerrov                 = class;
  TTrafMut                = class;
  TFerroEnvCollection     = class;
  TFerroEnvCollectionItem = class;
  TEnderFerro             = class;
  TDetVagCollection       = class;
  TDetVagCollectionItem   = class;

  // Informações do modal Dutoviário
  TDuto = class;

  // Informações do Multimodal
  TMultimodal = class;

////////////////////////////////////////////////////////////////////////////////

  TPeriCollection     = class;
  TPeriCollectionItem = class;

  TVeicNovosCollection     = class;
  TVeicNovosCollectionItem = class;

  TCobr              = class;
  TFat               = class;
  TDupCollection     = class;
  TDupCollectionItem = class;

  // Informações do CT-e de substituição
  TInfCteSub   = class;
  TTomaICMS    = class;
  TRefNF       = class;
  TTomaNaoICMS = class;

////////////////////////////////////////////////////////////////////////////////
// Informações de um CT-e Complemento de Valores
////////////////////////////////////////////////////////////////////////////////
  TInfCteComp             = class;
  TVPresComp              = class;
  TCompCompCollection     = class;
  TCompCompCollectionItem = class;
  TImpComp                = class;
  TICMSComp               = class;

////////////////////////////////////////////////////////////////////////////////
// Informações de um CT-e de Anulação
////////////////////////////////////////////////////////////////////////////////
  TInfCteAnu = class;

////////////////////////////////////////////////////////////////////////////////
  TautXMLCollection     = class;
  TautXMLCollectionItem = class;

  ////////////////////////////////////////////////////////////////////////////////

  TCTe = class(TPersistent)
  private
    FinfCTe: TInfCTe;
    Fide: TIde;
    Fcompl: TCompl;

    Femit: TEmit;
    Ftoma: TToma;
    Frem: TRem;
    Fexped: TExped;
    Freceb: TReceb;
    Fdest: TDest;

    FvPrest: TVPrest;
    Fimp: TImp;

    FinfCTeNorm: TInfCTeNorm;
    FinfCteComp: TInfCteComp;
    FInfCteAnu: TInfCteAnu;
    FautXML: TautXMLCollection;

    FProcCTe: TProcCTe;
    FSignature: TSignature;

    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property infCTe: TInfCTe read FinfCTe write FinfCTe;
    property ide: TIde       read Fide    write Fide;
    property compl: TCompl   read Fcompl  write Fcompl;

    property emit: TEmit     read Femit   write Femit;
    property toma: TToma     read Ftoma   write Ftoma;
    property rem: TRem       read Frem    write Frem;
    property exped: TExped   read Fexped  write Fexped;
    property receb: TReceb   read Freceb  write Freceb;
    property dest: TDest     read Fdest   write Fdest;

    property vPrest: TVPrest read FvPrest write FvPrest;
    property imp: TImp       read Fimp    write Fimp;

    property infCTeNorm: TInfCTeNorm read FinfCTeNorm write FinfCTeNorm;
    property infCteComp: TInfCteComp read FinfCteComp write FinfCteComp;
    property infCteAnu: TInfCteAnu   read FinfCteAnu  write FinfCteAnu;

    property autXML: TautXMLCollection read FautXML write SetautXML;

    property procCTe: TProcCTe     read FProcCTe   write FProcCTe;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

  TInfCTe = class(TPersistent)
  private
    FId : String;
    FVersao : Double;
    function GetVersaoStr: String;
  published
    property Id: String     read FId     write FId;
    property versao: Double read FVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
  end;

  TIde = class(TPersistent)
  private
    FcUF        : Integer;
    FcCT        : Integer;
    FCFOP       : Integer;
    FnatOp      : String;
    FforPag     : TpCTeFormaPagamento;
    Fmodelo     : Integer;
    Fserie      : Integer;
    FnCT        : Integer;
    FdhEmi      : TDateTime;
    FtpImp      : TpcnTipoImpressao;
    FtpEmis     : TpcnTipoEmissao;
    FcDV        : Integer;
    FtpAmb      : TpcnTipoAmbiente;
    FtpCTe      : TpcteTipoCTe;
    FprocEmi    : TpcnProcessoEmissao;
    FverProc    : String;
    FindGlobalizado: TIndicador;
    FrefCTe     : String;
    FcMunEnv    : Integer;
    FxMunEnv    : String;
    FUFEnv      : String;
    Fmodal      : TpcteModal;
    FtpServ     : TpcteTipoServico;
    FcMunIni    : Integer;
    FxMunIni    : String;
    FUFIni      : String;
    FcMunFim    : Integer;
    FxMunFim    : String;
    FUFFim      : String;
    Fretira     : TpcteRetira;
    Fxdetretira : String;
    FindIEToma: TpcnindIEDest;

    FToma03 : TToma03;
    FToma4  : TToma4;
    FinfPercurso: TinfPercursoCollection;

    FdhCont : TDateTime;
    FxJust  : String;
    
    procedure SetinfPercurso(Value: TinfPercursoCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property cUF: Integer                 read FcUF            write FcUF;
    property cCT: Integer                 read FcCT            write FcCT;
    property CFOP: Integer                read FCFOP           write FCFOP;
    property natOp : String               read FnatOp          write FnatOp;
    property forPag : TpcteFormaPagamento read FforPag         write FforPag;
    property modelo: Integer              read Fmodelo         write Fmodelo;
    property serie: Integer               read Fserie          write Fserie;
    property nCT: Integer                 read FnCT            write FnCT;
    property dhEmi: TDateTime             read FdhEmi          write FdhEmi;
    property tpImp: TpcnTipoImpressao     read FtpImp          write FtpImp;
    property tpEmis: TpcnTipoEmissao      read FtpEmis         write FtpEmis;
    property cDV: Integer                 read FcDV            write FcDV;
    property tpAmb: TpcnTipoAmbiente      read FtpAmb          write FtpAmb;
    property tpCTe: TpcteTipoCTe          read FtpCTe          write FtpCTe;
    property procEmi: TpcnProcessoEmissao read FprocEmi        write FprocEmi;
    property verProc: String              read FverProc        write FverProc;
    property indGlobalizado: TIndicador   read FindGlobalizado write FindGlobalizado default tiNao;
    property refCTe: String               read FrefCTe         write FrefCTe;
    property cMunEnv: Integer             read FcMunEnv        write FcMunEnv;
    property xMunEnv: String              read FxMunEnv        write FxMunEnv;
    property UFEnv: String                read FUFEnv          write FUFEnv;
    property modal: TpcteModal            read Fmodal          write Fmodal;
    property tpServ: TpcteTipoServico     read FtpServ         write FtpServ;
    property cMunIni: Integer             read FcMunIni        write FcMunIni;
    property xMunIni: String              read FxMunIni        write FxMunIni;
    property UFIni: String                read FUFIni          write FUFIni;
    property cMunFim: Integer             read FcMunFim        write FcMunFim;
    property xMunFim: String              read FxMunFim        write FxMunFim;
    property UFFim: String                read FUFFim          write FUFFim;
    property retira: TpcteRetira          read Fretira         write Fretira;
    property xDetRetira: String           read Fxdetretira     write Fxdetretira;
    property indIEToma: TpcnindIEDest     read FindIEToma      write FindIEToma;

    property toma03: TToma03 read FToma03 write FToma03;
    property toma4: TToma4   read FToma4  write FToma4;

    property infPercurso: TinfPercursoCollection     read FinfPercurso   write SetinfPercurso;

    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: String     read FxJust  write FxJust;
  end;

  TToma03 = class(TPersistent)
  private
    Ftoma : TpcteTomador;
  published
    property Toma: TpcteTomador read Ftoma write Ftoma;
  end;

  TToma4 = class(TPersistent)
  private
    Ftoma      : TpcteTomador;
    FCNPJCPF   : String;
    FIE        : String;
    FxNome     : String;
    FxFant     : String;
    Ffone      : String;
    FEnderToma : TEndereco;
    Femail     : String;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property toma: TpcteTomador   read Ftoma      write Ftoma;
    property CNPJCPF: String      read FCNPJCPF   write FCNPJCPF;
    property IE: String           read FIE        write FIE;
    property xNome: String        read FxNome     write FxNome;
    property xFant: String        read FxFant     write FxFant;
    property fone: String         read Ffone      write Ffone;
    property enderToma: TEndereco read FEnderToma write FEnderToma;
    property email: String        read Femail     write Femail;
  end;

  TinfPercursoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfPercursoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfPercursoCollectionItem);
  public
    constructor Create(AOwner: Tide);
    function Add: TinfPercursoCollectionItem;
    property Items[Index: Integer]: TinfPercursoCollectionItem read GetItem write SetItem; default;
  end;

  TinfPercursoCollectionItem = class(TCollectionItem)
  private
    FUFPer: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property UFPer: String read FUFPer write FUFPer;
  end;

  TToma = class(TPersistent)
  private
    FCNPJCPF: String;
    FIE: String;
    FxNome: String;
    FxFant: String;
    Ffone: String;
    FEnderToma: TEndereco;
    Femail: String;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String          read FCNPJCPF   write FCNPJCPF;
    property IE: String               read FIE        write FIE;
    property xNome: String            read FxNome     write FxNome;
    property xFant: String            read FxFant     write FxFant;
    property fone: String             read Ffone      write Ffone;
    property enderToma: TEndereco     read FEnderToma write FEnderToma;
    property email: String            read Femail     write Femail;
  end;

  TEndereco = class(TPersistent)
  private
    FxLgr    : String;
    Fnro     : String;
    FxCpl    : String;
    FxBairro : String;
    FcMun    : Integer;
    FxMun    : String;
    FCEP     : Integer;
    FUF      : String;
    FcPais   : Integer;
    FxPais   : String;
  published
    property xLgr: String    read FxLgr    write FxLgr;
    property nro: String     read Fnro     write Fnro;
    property xCpl: String    read FxCpl    write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: String    read FxMun    write FxMun;
    property CEP: Integer    read FCEP     write FCEP;
    property UF: String      read FUF      write FUF;
    property cPais: Integer  read FcPais   write FcPais;
    property xPais: String   read FxPais   write FxPais;
  end;

  TCompl = class(TPersistent)
  private
    FxCaracAd  : String;
    FxCaracSer : String;
    FxEmi      : String;
    Ffluxo     : TFluxo;
    FEntrega   : TEntrega;
    ForigCalc  : String;
    FdestCalc  : String;
    FxObs      : String;

    FObsCont  : TObsContCollection;
    FObsFisco : TObsFiscoCollection;

    procedure SetObsCont(Value: TObsContCollection);
    procedure SetObsFisco(Value: TObsFiscoCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property xCaracAd: String  read FxCaracAd  write FxCaracAd;
    property xCaracSer: String read FxCaracSer write FxCaracSer;
    property xEmi: String      read FxEmi      write FxEmi;
    property fluxo: TFluxo     read Ffluxo     write Ffluxo;
    property Entrega: TEntrega read FEntrega   write FEntrega;
    property origCalc: String  read ForigCalc  write ForigCalc;
    property destCalc: String  read FdestCalc  write FdestCalc;
    property xObs: String      read FxObs      write FxObs;

    property ObsCont: TObsContCollection   read FObsCont  write SetObsCont;
    property ObsFisco: TObsFiscoCollection read FObsFisco write SetObsFisco;
  end;

  TFluxo = class(TPersistent)
  private
    FxOrig  : String;
    Fpass   : TPassCollection;
    FxDest  : String;
    FxRota  : String;

    procedure SetPass(Value: TPassCollection);
  public
    constructor Create(AOwner: TCompl);
    destructor Destroy; override;
  published
    property xOrig: String         read FxOrig write FxOrig;
    property pass: TPassCollection read Fpass  write SetPass;
    property xDest: String         read FxDest write FxDest;
    property xRota: String         read FxRota write FxRota;
  end;

  TPassCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPassCollectionItem;
    procedure SetItem(Index: Integer; Value: TPassCollectionItem);
  public
    constructor Create(AOwner: TFluxo);
    function Add: TPassCollectionItem;
    property Items[Index: Integer]: TPassCollectionItem read GetItem write SetItem; default;
  end;

  TPassCollectionItem = class(TCollectionItem)
  private
    FxPass : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xPass: String read FxPass write FxPass;
  end;

  TEntrega = class(TPersistent)
  private
    FTipoData : TpcteTipoDataPeriodo;
    FTipoHora : TpcteTipoHorarioIntervalo;

    FsemData   : TSemData;
    FcomData   : TComData;
    FnoPeriodo : TNoPeriodo;
    FsemHora   : TSemHora;
    FcomHora   : TComHora;
    FnoInter   : TNoInter;
  public
    constructor Create(AOwner: TCompl);
    destructor Destroy; override;
  published
    property TipoData: TpcteTipoDataPeriodo      read FTipoData write FTipoData;
    property TipoHora: TpcteTipoHorarioIntervalo read FTipoHora write FTipoHora;

    property semData: TSemData     read FsemData   write FsemData;
    property comData: TComData     read FcomData   write FcomData;
    property noPeriodo: TNoPeriodo read FnoPeriodo write FnoPeriodo;
    property semHora: TSemHora     read FsemHora   write FsemHora;
    property comHora: TComHora     read FcomHora   write FcomHora;
    property noInter: TNoInter     read FnoInter   write FnoInter;
  end;

  TSemData = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
  published
    property tpPer: TpcteTipoDataPeriodo read FtpPer write FtpPer;
  end;

  TComData = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
   FdProg : TDateTime;
  published
    property tpPer: TpcteTipoDataPeriodo read FtpPer write FtpPer;
    property dProg: TDateTime            read FdProg write FdProg;
  end;

  TNoPeriodo = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
   FdIni  : TDateTime;
   FdFim  : TDateTime;
  published
    property tpPer: TpcteTipoDataPeriodo read FtpPer write FtpPer;
    property dIni: TDateTime             read FdIni  write FdIni;
    property dFim: TDateTime             read FdFim  write FdFim;
  end;

  TSemHora = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
  published
    property tpHor: TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
  end;

  TComHora = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
   FhProg : TDateTime;
  published
    property tpHor: TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
    property hProg: TDateTime                 read FhProg write FhProg;
  end;

  TNoInter = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
   FhIni  : TDateTime;
   FhFim  : TDateTime;
  published
    property tpHor: TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
    property hIni: TDateTime                  read FhIni  write FhIni;
    property hFim: TDateTime                  read FhFim  write FhFim;
  end;

  TObsContCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TObsContCollectionItem;
    procedure SetItem(Index: Integer; Value: TObsContCollectionItem);
  public
    constructor Create(AOwner: TCompl);
    function Add: TObsContCollectionItem;
    property Items[Index: Integer]: TObsContCollectionItem read GetItem write SetItem; default;
  end;

  TObsContCollectionItem = class(TCollectionItem)
  private
    FxCampo : String;
    FxTexto : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TObsFiscoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TObsFiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TObsFiscoCollectionItem);
  public
    constructor Create(AOwner: TCompl);
    function Add: TObsFiscoCollectionItem;
    property Items[Index: Integer]: TObsFiscoCollectionItem read GetItem write SetItem; default;
  end;

  TObsFiscoCollectionItem = class(TCollectionItem)
  private
    FxCampo : String;
    FxTexto : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TEmit = class(TPersistent)
  private
    FCNPJ      : String;
    FIE        : String;
    FIEST      : string;
    FxNome     : String;
    FxFant     : String;
    FEnderEmit : TEnderEmit;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJ: String          read FCNPJ      write FCNPJ;
    property IE: String            read FIE        write FIE;
    property IEST: String          read FIEST      write FIEST;
    property xNome: String         read FxNome     write FxNome;
    property xFant: String         read FxFant     write FxFant;
    property enderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
  end;

  TEnderEmit = class(TPersistent)
  private
    FxLgr    : String;
    Fnro     : String;
    FxCpl    : String;
    FxBairro : String;
    FcMun    : Integer;
    FxMun    : String;
    FCEP     : Integer;
    FUF      : String;
    Ffone    : String;
  published
    property xLgr: String    read FxLgr    write FxLgr;
    property nro: String     read Fnro     write Fnro;
    property xCpl: String    read FxCpl    write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: String    read FxMun    write FxMun;
    property CEP: Integer    read FCEP     write FCEP;
    property UF: String      read FUF      write FUF;
    property fone: String    read Ffone    write Ffone;
  end;

  TRem = class(TPersistent)
  private
    FCNPJCPF   : String;
    FIE        : String;
    FxNome     : String;
    FxFant     : String;
    Ffone      : String;
    FEnderReme : TEndereco;
    Femail     : String;
    FlocColeta : TLocColeta;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String       read FCNPJCPF   write FCNPJCPF;
    property IE: String            read FIE        write FIE;
    property xNome: String         read FxNome     write FxNome;
    property xFant: String         read FxFant     write FxFant;
    property fone: String          read Ffone      write Ffone;
    property enderReme: TEndereco  read FEnderReme write FEnderReme;
    property email: String         read Femail     write Femail;
    property locColeta: TLocColeta read FlocColeta write FlocColeta;
  end;

  TLocColeta = class(TPersistent)
  private
    FCNPJCPF : String;
    FxNome   : String;
    FxLgr    : String;
    Fnro     : String;
    FxCpl    : String;
    FxBairro : String;
    FcMun    : Integer;
    FxMun    : String;
    FUF      : String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: String read FxNome write FxNome;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
  end;

  TExped = class(TPersistent)
  private
    FCNPJCPF    : String;
    FIE         : String;
    FxNome      : String;
    Ffone       : String;
    FEnderExped : TEndereco;
    Femail      : String;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String       read FCNPJCPF    write FCNPJCPF;
    property IE: String            read FIE         write FIE;
    property xNome: String         read FxNome      write FxNome;
    property fone: String          read Ffone       write Ffone;
    property enderExped: TEndereco read FEnderExped write FEnderExped;
    property email: String         read Femail      write Femail;
  end;

  TReceb = class(TPersistent)
  private
    FCNPJCPF    : String;
    FIE         : String;
    FxNome      : String;
    Ffone       : String;
    FEnderReceb : TEndereco;
    Femail      : String;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String       read FCNPJCPF    write FCNPJCPF;
    property IE: String            read FIE         write FIE;
    property xNome: String         read FxNome      write FxNome;
    property fone: String          read Ffone       write Ffone;
    property enderReceb: TEndereco read FEnderReceb write FEnderReceb;
    property email: String         read Femail      write Femail;
  end;

  TDest = class(TPersistent)
  private
    FCNPJCPF   : String;
    FIE        : String;
    FxNome     : String;
    Ffone      : String;
    FISUF      : String;
    FEnderDest : TEndereco;
    Femail     : String;
    FlocEnt    : TLocEnt;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String      read FCNPJCPF   write FCNPJCPF;
    property IE: String           read FIE        write FIE;
    property xNome: String        read FxNome     write FxNome;
    property fone: String         read Ffone      write Ffone;
    property ISUF: String         read FISUF      write FISUF;
    property enderDest: TEndereco read FEnderDest write FEnderDest;
    property email: String        read Femail     write Femail;
    property locEnt: TLocEnt      read FlocEnt    write FlocEnt;
  end;

  TLocEnt = class(TPersistent)
  private
    FCNPJCPF : String;
    FxNome   : String;
    FxLgr    : String;
    Fnro     : String;
    FxCpl    : String;
    FxBairro : String;
    FcMun    : Integer;
    FxMun    : String;
    FUF      : String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: String   read FxNome   write FxNome;
    property xLgr: String    read FxLgr    write FxLgr;
    property nro: String     read Fnro     write Fnro;
    property xCpl: String    read FxCpl    write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: String    read FxMun    write FxMun;
    property UF: String      read FUF      write FUF;
  end;

  TVPrest = class(TPersistent)
  private
    FvTPrest : Currency;
    FvRec    : Currency;
    FComp    : TCompCollection;

    procedure SetCompItem(const Value: TCompCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property vTPrest: Currency     read FvTPrest write FvTPrest;
    property vRec: Currency        read FvRec    write FvRec;
    property Comp: TCompCollection read FComp    write SetCompItem;
  end;

  TCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompCollectionItem);
  public
    constructor Create(AOwner: TVPrest);
    function Add: TCompCollectionItem;
    property Items[Index: Integer]: TCompCollectionItem read GetItem write SetItem; default;
  end;

  TCompCollectionItem = class(TCollectionItem)
  private
    FxNome : String;
    FvComp : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: String   read FxNome write FxNome;
    property vComp: Currency read FvComp write FvComp;
  end;

  TImp = class(TPersistent)
  private
    FICMS       : TICMS;
    FvTotTrib   : Currency;
    FInfAdFisco : String;
    FICMSUFFim: TICMSUFFim;
    FinfTribFed: TinfTribFed;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property ICMS: TICMS             read FICMS        write FICMS;
    property vTotTrib: Currency      read FvTotTrib    write FvTotTrib;
    property infAdFisco: String      read FInfAdFisco  write FInfAdFisco;
    property ICMSUFFim: TICMSUFFim   read FICMSUFFim   write FICMSUFFim;
    property infTribFed: TinfTribFed read FinfTribFed  write FinfTribFed;
  end;

  TICMS = class(TPersistent)
  private
    FSituTrib    : TpcnCSTIcms;
    FCST00       : TCST00;
    FCST20       : TCST20;
    FCST45       : TCST45;
    FCST60       : TCST60;
    FCST90       : TCST90;
    FICMSOutraUF : TICMSOutraUF;
    FICMSSN      : TICMSSN;
  public
    constructor Create(AOwner: TImp);
    destructor Destroy; override;
  published
    property SituTrib: TpcnCSTIcms     read FSituTrib    write FSituTrib;
    property ICMS00: TCST00            read FCST00       write FCST00;
    property ICMS20: TCST20            read FCST20       write FCST20;
    property ICMS45: TCST45            read FCST45       write FCST45;
    property ICMS60: TCST60            read FCST60       write FCST60;
    property ICMS90: TCST90            read FCST90       write FCST90;
    property ICMSOutraUF: TICMSOutraUF read FICMSOutraUF write FICMSOutraUF;
    property ICMSSN: TICMSSN           read FICMSSN      write FICMSSN;
  end;

  TCST00 = class(TPersistent)
  private
    FCST   : TpcnCSTIcms;
    FvBC   : Currency;
    FpICMS : Currency;
    FvICMS : Currency;
  published
    property CST: TpcnCSTIcms read FCST   write FCST default cst00;
    property vBC: Currency    read FvBC   write FvBC;
    property pICMS: Currency  read FpICMS write FpICMS;
    property vICMS: Currency  read FvICMS write FvICMS;
  end;

  TCST20 = class(TPersistent)
  private
    FCST    : TpcnCSTIcms;
    FpRedBC : Currency;
    FvBC    : Currency;
    FpICMS  : Currency;
    FvICMS  : Currency;
  published
    property CST: TpcnCSTIcms read FCST    write FCST default cst20;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency    read FvBC    write FvBC;
    property pICMS: Currency  read FpICMS  write FpICMS;
    property vICMS: Currency  read FvICMS  write FvICMS;
  end;

  TCST45 = class(TPersistent)
  private
    FCST : TpcnCSTIcms;
  published
    property CST: TpcnCSTIcms read FCST write FCST;
  end;

  TCST60 = class(TPersistent)
  private
    FCST        : TpcnCSTIcms;
    FvBCSTRet   : Currency;
    FvICMSSTRet : Currency;
    FpICMSSTRet : Currency;
    FvCred      : Currency;
  published
    property CST: TpcnCSTIcms     read FCST        write FCST default cst60;
    property vBCSTRet: Currency   read FvBCSTRet   write FvBCSTRet;
    property vICMSSTRet: Currency read FvICMSSTRet write FvICMSSTRet;
    property pICMSSTRet: Currency read FpICMSSTRet write FpICMSSTRet;
    property vCred: Currency      read FvCred      write FvCred;
  end;

  TCST90 = class(TPersistent)
  private
    FCST    : TpcnCSTIcms;
    FpRedBC : Currency;
    FvBC    : Currency;
    FpICMS  : Currency;
    FvICMS  : Currency;
    FvCred  : Currency;
  published
    property CST: TpcnCSTIcms read FCST    write FCST default cst90;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency    read FvBC    write FvBC;
    property pICMS: Currency  read FpICMS  write FpICMS;
    property vICMS: Currency  read FvICMS  write FvICMS;
    property vCred: Currency  read FvCred  write FvCred;
  end;

  TICMSOutraUF = class(TPersistent)
  private
    FCST           : TpcnCSTIcms;
    FpRedBCOutraUF : Currency;
    FvBCOutraUF    : Currency;
    FpICMSOutraUF  : Currency;
    FvICMSOutraUF  : Currency;
  published
    property CST: TpcnCSTIcms        read FCST           write FCST default cst90;
    property pRedBCOutraUF: Currency read FpRedBCOutraUF write FpRedBCOutraUF;
    property vBCOutraUF: Currency    read FvBCOutraUF    write FvBCOutraUF;
    property pICMSOutraUF: Currency  read FpICMSOutraUF  write FpICMSOutraUF;
    property vICMSOutraUF: Currency  read FvICMSOutraUF  write FvICMSOutraUF;
  end;

  TICMSSN = class(TPersistent)
  private
    FindSN : Integer;
  published
    property indSN: Integer read FindSN write FindSN default 1;
  end;

  TICMSUFFim = class(TPersistent)
  private
    FvBCUFFim: Currency;
    FpFCPUFFim: Currency;
    FpICMSUFFim: Currency;
    FpICMSInter: Currency;
    FpICMSInterPart: Currency;
    FvFCPUFFim: Currency;
    FvICMSUFFim: Currency;
    FvICMSUFIni: Currency;
  published
    property vBCUFFim: Currency       read FvBCUFFim       write FvBCUFFim;
    property pFCPUFFim: Currency      read FpFCPUFFim      write FpFCPUFFim;
    property pICMSUFFim: Currency     read FpICMSUFFim     write FpICMSUFFim;
    property pICMSInter: Currency     read FpICMSInter     write FpICMSInter;
    property pICMSInterPart: Currency read FpICMSInterPart write FpICMSInterPart;
    property vFCPUFFim: Currency      read FvFCPUFFim      write FvFCPUFFim;
    property vICMSUFFim: Currency     read FvICMSUFFim     write FvICMSUFFim;
    property vICMSUFIni: Currency     read FvICMSUFIni     write FvICMSUFIni;
  end;

  TinfTribFed = class(TPersistent)
  private
    FvPIS: Currency;
    FvCOFINS: Currency;
    FvIR: Currency;
    FvINSS: Currency;
    FvCSLL: Currency;
  published
    property vPIS: Currency    read FvPIS    write FvPIS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vIR: Currency     read FvIR     write FvIR;
    property vINSS: Currency   read FvINSS   write FvINSS;
    property vCSLL: Currency   read FvCSLL   write FvCSLL;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfCTeNorm = class(TPersistent)
  private
    FinfCarga: TInfCarga;
    FinfDoc: TInfDoc;
    FdocAnt: TDocAnt;
    Fseg: TSegCollection;

    Frodo: TRodo;             // Informações do modal Rodoviário
    FrodoOS: TRodoOS;         // Informações do modal Rodoviário Outros Serviços
    Faereo: TAereo;           // Informações do modal Aéreo
    Faquav: TAquav;           // Informações do modal Aquaviário
    Fferrov: TFerrov;         // Informações do modal Ferroviário
    Fduto: TDuto;             // Informações do modal Dutoviário
    Fmultimodal: TMultimodal; // Informações do Multimodal

    Fperi: TPeriCollection;
    FveicNovos: TVeicNovosCollection;
    FCobr: TCobr;
    FinfCteSub: TInfCteSub;
    FinfGlobalizado: TinfGlobalizado;
    FinfServVinc: TinfServVinc;
    FinfServico: TinfServico;
    FinfDocRef: TinfDocRefCollection;
    FrefCTeCanc: String;

    procedure SetSeg(const Value: TSegCollection);
    procedure SetPeri(const Value: TPeriCollection);
    procedure SetVeicNovos(const Value: TVeicNovosCollection);
    procedure SetinfDocRef(const Value: TinfDocRefCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property infCarga: TInfCarga read FInfCarga write FInfCarga;
    property infDoc: TInfDoc     read FinfDoc   write FinfDoc;
    property docAnt: TDocAnt     read FdocAnt   write FdocAnt;
    property seg: TSegCollection read Fseg      write SetSeg;

    property rodo: TRodo             read Frodo       write Frodo;
    property rodoOS: TRodoOS         read FrodoOS     write FrodoOS;

    property aereo: TAereo           read Faereo      write Faereo;
    property aquav: TAquav           read Faquav      write Faquav;
    property ferrov: TFerrov         read Fferrov     write Fferrov;
    property duto: TDuto             read Fduto       write Fduto;
    property multimodal: TMultimodal read Fmultimodal write Fmultimodal;

    property peri: TPeriCollection           read Fperi           write SetPeri;
    property veicNovos: TVeicNovosCollection read FveicNovos      write SetVeicNovos;
    property cobr: TCobr                     read FCobr           write FCobr;
    property infCteSub: TInfCteSub           read FinfCteSub      write FinfCteSub;
    property infGlobalizado: TinfGlobalizado read FinfGlobalizado write FinfGlobalizado;
    property infServVinc: TinfServVinc       read FinfServVinc    write FinfServVinc;

    property infServico: TinfServico         read FinfServico write FinfServico;
    property infDocRef: TinfDocRefCollection read FinfDocRef  write SetinfDocRef;
    property refCTeCanc: String              read FrefCTeCanc write FrefCTeCanc;
  end;

  TInfServico = class(TPersistent)
  private
    FxDescServ: String;
    FqCarga: Currency;
  published
    property xDescServ: String read FxDescServ write FxDescServ;
    property qCarga: Currency read FqCarga write FqCarga;
  end;

  TinfDocRefCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfDocRefCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfDocRefCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TinfDocRefCollectionItem;
    property Items[Index: Integer]: TinfDocRefCollectionItem read GetItem write SetItem; default;
  end;

  TinfDocRefCollectionItem = class(TCollectionItem)
  private
    FnDoc: String;
    Fserie: String;
    Fsubserie: String;
    FdEmi: TDateTime;
    FvDoc: Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nDoc: String     read FnDoc     write FnDoc;
    property serie: String    read Fserie    write Fserie;
    property subserie: String read Fsubserie write Fsubserie;
    property dEmi: TDateTime  read FdEmi     write FdEmi;
    property vDoc: Currency   read FvDoc     write FvDoc;
  end;

  TInfGlobalizado = class(TPersistent)
  private
    FxObs: String;
  published
    property xObs: String     read FxObs   write FxObs;
  end;

  TInfServVinc = class(TPersistent)
  private
    FinfCTeMultimodal: TinfCTeMultimodalCollection;

    procedure SetinfCTeMultimodal(const Value: TinfCTeMultimodalCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property infCTeMultimodal: TinfCTeMultimodalCollection read FinfCTeMultimodal write SetinfCTeMultimodal;
  end;

  TInfCTeMultimodalCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfCTeMultimodalCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfCTeMultimodalCollectionItem);
  public
    constructor Create(AOwner: TInfServVinc);
    function Add: TInfCTeMultimodalCollectionItem;
    property Items[Index: Integer]: TInfCTeMultimodalCollectionItem read GetItem write SetItem; default;
  end;

  TInfCTeMultimodalCollectionItem = class(TCollectionItem)
  private
    FchCTeMultimodal: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chCTeMultimodal: String read FchCTeMultimodal write FchCTeMultimodal;
  end;

  TInfCarga = class(TPersistent)
  private
    FvCarga  : Currency;
    FproPred : String;
    FxOutCat : String;
    FinfQ    : TInfQCollection;
    FvCargaAverb: Currency;

    procedure SetInfQ(const Value: TInfQCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property vCarga: Currency      read FvCarga      write FvCarga;
    property proPred: String       read FproPred     write FproPred;
    property xOutCat: String       read FxOutCat     write FxOutCat;
    property infQ: TInfQCollection read FinfQ        write SetInfQ;
    property vCargaAverb: Currency read FvCargaAverb write FvCargaAverb;
  end;

  TInfQCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfQCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfQCollectionItem);
  public
    constructor Create(AOwner: TInfCarga);
    function Add: TInfQCollectionItem;
    property Items[Index: Integer]: TInfQCollectionItem read GetItem write SetItem; default;
  end;

  TInfQCollectionItem = class(TCollectionItem)
  private
    FcUnid  : TUnidMed;
    FtpMed  : String;
    FqCarga : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cUnid: TUnidMed  read FcUnid  write FcUnid;
    property tpMed: String    read FtpMed  write FtpMed;
    property qCarga: Currency read FqCarga write FqCarga;
  end;

  TInfDoc = class(TPersistent)
  private
    FinfNF     : TInfNFCollection;
    FinfNFe    : TInfNFeCollection;
    FinfOutros : TInfOutrosCollection;

    procedure SetInfNF(const Value: TInfNFCollection);
    procedure SetInfNFe(const Value: TInfNFeCollection);
    procedure SetInfOutros(const Value: TInfOutrosCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property infNF: TInfNFCollection         read FinfNF     write SetInfNF;
    property infNFe: TInfNFeCollection       read FinfNFe    write SetInfNFe;
    property infOutros: TInfOutrosCollection read FinfOutros write SetInfOutros;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfNFCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFCollectionItem);
  public
    constructor Create(AOwner: TInfDoc);
    function Add: TInfNFCollectionItem;
    property Items[Index: Integer]: TInfNFCollectionItem read GetItem write SetItem; default;
  end;

  TInfNFCollectionItem = class(TCollectionItem)
  private
    FnRoma         : String;
    FnPed          : String;
    Fmodelo        : TpcteModeloNF;
    Fserie         : String;
    FnDoc          : String;
    FdEmi          : TDateTime;
    FvBC           : Currency;
    FvICMS         : Currency;
    FvBCST         : Currency;
    FvST           : Currency;
    FvProd         : Currency;
    FvNF           : Currency;
    FnCFOP         : Integer;
    FnPeso         : Currency;
    FPIN           : String;
    FdPrev         : TDateTime;
    FinfUnidTransp : TinfUnidTranspNFCollection;
    FinfUnidCarga  : TinfUnidCargaNFCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspNFCollection);
    procedure SetinfUnidCarga(const Value: TinfUnidCargaNFCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nRoma: String                             read FnRoma         write FnRoma;
    property nPed: String                              read FnPed          write FnPed;
    property modelo: TpcteModeloNF                     read Fmodelo        write Fmodelo;
    property serie: String                             read Fserie         write Fserie;
    property nDoc: String                              read FnDoc          write FnDoc;
    property dEmi: TDateTime                           read FdEmi          write FdEmi;
    property vBC: Currency                             read FvBC           write FvBC;
    property vICMS: Currency                           read FvICMS         write FvICMS;
    property vBCST: Currency                           read FvBCST         write FvBCST;
    property vST: Currency                             read FvST           write FvST;
    property vProd: Currency                           read FvProd         write FvProd;
    property vNF: Currency                             read FvNF           write FvNF;
    property nCFOP: Integer                            read FnCFOP         write FnCFOP;
    property nPeso: Currency                           read FnPeso         write FnPeso;
    property PIN: String                               read FPIN           write FPIN;
    property dPrev: TDateTime                          read FdPrev         write FdPrev;
    property infUnidTransp: TinfUnidTranspNFCollection read FinfUnidTransp write SetinfUnidTransp;
    property infUnidCarga: TinfUnidCargaNFCollection   read FinfUnidCarga  write SetinfUnidCarga;
  end;

  TinfUnidTranspNFCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfNFCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaNFCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaCollectionItem);
  public
    constructor Create(AOwner: TinfNFCollectionItem);
    function Add: TinfUnidCargaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

////////////////////////////////////////////////////////////////////////////////

  TinfUnidTranspCollectionItem = class(TCollectionItem)
  private
    FtpUnidTransp  : TpcnUnidTransp;
    FidUnidTransp  : String;
    FlacUnidTransp : TlacUnidTranspCollection;
    FinfUnidCarga  : TinfUnidCargaCollection;
    FqtdRat        : Double;

    procedure SetlacUnidTransp(const Value: TlacUnidTranspCollection);
    procedure SetinfUnidCarga(const Value: TinfUnidCargaCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpUnidTransp: TpcnUnidTransp            read FtpUnidTransp  write FtpUnidTransp;
    property idUnidTransp: String                    read FidUnidTransp  write FidUnidTransp;
    property lacUnidTransp: TlacUnidTranspCollection read FlacUnidTransp write SetlacUnidTransp;
    property infUnidCarga: TinfUnidCargaCollection   read FinfUnidCarga  write SetinfUnidCarga;
    property qtdRat: Double                          read FqtdRat        write FqtdRat;
  end;

  TlacUnidTranspCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfUnidTranspCollectionItem);
    function Add: TlacUnidTranspCollectionItem;
    property Items[Index: Integer]: TlacUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TlacUnidTranspCollectionItem = class(TCollectionItem)
  private
    FnLacre : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TinfUnidCargaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaCollectionItem);
  public
    constructor Create(AOwner: TinfUnidTranspCollectionItem);
    function Add: TinfUnidCargaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaCollectionItem = class(TCollectionItem)
  private
    FtpUnidCarga  : TpcnUnidCarga;
    FidUnidCarga  : String;
    FlacUnidCarga : TlacUnidCargaCollection;
    FqtdRat       : Double;

    procedure SetlacUnidCarga(const Value: TlacUnidCargaCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpUnidCarga: TpcnUnidCarga            read FtpUnidCarga  write FtpUnidCarga;
    property idUnidCarga: String                   read FidUnidCarga  write FidUnidCarga;
    property lacUnidCarga: TlacUnidCargaCollection read FlacUnidCarga write SetlacUnidCarga;
    property qtdRat: Double                        read FqtdRat       write FqtdRat;
  end;

  TlacUnidCargaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacUnidCargaCollectionItem);
  public
    constructor Create(AOwner: TinfUnidCargaCollectionItem);
    function Add: TlacUnidCargaCollectionItem;
    property Items[Index: Integer]: TlacUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

  TlacUnidCargaCollectionItem = class(TCollectionItem)
  private
    FnLacre : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFeCollectionItem);
  public
    constructor Create(AOwner: TInfDoc);
    function Add: TInfNFeCollectionItem;
    property Items[Index: Integer]: TInfNFeCollectionItem read GetItem write SetItem; default;
  end;

  TInfNFeCollectionItem = class(TCollectionItem)
  private
    Fchave         : String;
    FPIN           : String;
    FdPrev         : TDateTime;
    FinfUnidTransp : TinfUnidTranspNFeCollection;
    FinfUnidCarga  : TinfUnidCargaNFeCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspNFeCollection);
    procedure SetinfUnidCarga(const Value: TinfUnidCargaNFeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chave: String                              read Fchave         write Fchave;
    property PIN: String                                read FPIN           write FPIN;
    property dPrev: TDateTime                           read FdPrev         write FdPrev;
    property infUnidTransp: TinfUnidTranspNFeCollection read FinfUnidTransp write SetinfUnidTransp;
    property infUnidCarga: TinfUnidCargaNFeCollection   read FinfUnidCarga  write SetinfUnidCarga;
  end;

  TinfUnidTranspNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfNFeCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaCollectionItem);
  public
    constructor Create(AOwner: TinfNFeCollectionItem);
    function Add: TinfUnidCargaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfOutrosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfOutrosCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfOutrosCollectionItem);
  public
    constructor Create(AOwner: TInfDoc);
    function Add: TInfOutrosCollectionItem;
    property Items[Index: Integer]: TInfOutrosCollectionItem read GetItem write SetItem; default;
  end;

  TInfOutrosCollectionItem = class(TCollectionItem)
  private
    FtpDoc         : TpcteTipoDocumento;
    FdescOutros    : String;
    FnDoc          : String;
    FdEmi          : TdateTime;
    FvDocFisc      : Currency;
    FdPrev         : TDateTime;
    FinfUnidTransp : TinfUnidTranspOutrosCollection;
    FinfUnidCarga  : TinfUnidCargaOutrosCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspOutrosCollection);
    procedure SetinfUnidCarga(const Value: TinfUnidCargaOutrosCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpDoc: TpcteTipoDocumento                     read FtpDoc         write FtpDoc;
    property descOutros: String                            read FdescOutros    write FdescOutros;
    property nDoc: String                                  read FnDoc          write FnDoc;
    property dEmi: TdateTime                               read FdEmi          write FdEmi;
    property vDocFisc: Currency                            read FvDocFisc      write FvDocFisc;
    property dPrev: TDateTime                              read FdPrev         write FdPrev;
    property infUnidTransp: TinfUnidTranspOutrosCollection read FinfUnidTransp write SetinfUnidTransp;
    property infUnidCarga: TinfUnidCargaOutrosCollection   read FinfUnidCarga  write SetinfUnidCarga;
  end;

  TinfUnidTranspOutrosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfOutrosCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaOutrosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaCollectionItem);
  public
    constructor Create(AOwner: TinfOutrosCollectionItem);
    function Add: TinfUnidCargaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

////////////////////////////////////////////////////////////////////////////////

  TDocAnt = class(TPersistent)
  private
    FemiDocAnt : TEmiDocAntCollection;

    procedure SetEmiDocAnt(const Value: TEmiDocAntCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property emiDocAnt: TEmiDocAntCollection read FemiDocAnt write SetEmiDocAnt;
  end;

  TEmiDocAntCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TEmiDocAntCollectionItem;
    procedure SetItem(Index: Integer; Value: TEmiDocAntCollectionItem);
  public
    constructor Create(AOwner: TDocAnt);
    function Add: TEmiDocAntCollectionItem;
    property Items[Index: Integer]: TEmiDocAntCollectionItem read GetItem write SetItem; default;
  end;

  TEmiDocAntCollectionItem = class(TCollectionItem)
  private
    FCNPJCPF  : String;
    FIE       : String;
    FUF       : String;
    FxNome    : String;
    FidDocAnt : TIdDocAntCollection;

    procedure SetIdDocAnt(const Value: TIdDocAntCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJCPF: String               read FCNPJCPF  write FCNPJCPF;
    property IE: String                    read FIE       write FIE;
    property UF: String                    read FUF       write FUF;
    property xNome: String                 read FxNome    write FxNome;
    property idDocAnt: TIdDocAntCollection read FidDocAnt write SetIdDocAnt;
  end;

  TIdDocAntCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdDocAntCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdDocAntCollectionItem);
  public
    constructor Create(AOwner: TEmiDocAntCollectionItem);
    function Add: TIdDocAntCollectionItem;
    property Items[Index: Integer]: TIdDocAntCollectionItem read GetItem write SetItem; default;
  end;

  TIdDocAntCollectionItem = class(TCollectionItem)
  private
    FidDocAntPap : TIdDocAntPapCollection;
    FidDocAntEle : TIdDocAntEleCollection;

    procedure SetIdDocAntPap(const Value: TIdDocAntPapCollection);
    procedure SetIdDocAntEle(const Value: TIdDocAntEleCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property idDocAntPap: TIdDocAntPapCollection read FidDocAntPap write SetIdDocAntPap;
    property idDocAntEle: TIdDocAntEleCollection read FidDocAntEle write SetIdDocAntEle;
  end;

  TIdDocAntPapCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdDocAntPapCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdDocAntPapCollectionItem);
  public
    constructor Create(AOwner: TIdDocAntCollectionItem);
    function Add: TIdDocAntPapCollectionItem;
    property Items[Index: Integer]: TIdDocAntPapCollectionItem read GetItem write SetItem; default;
  end;

  TIdDocAntPapCollectionItem = class(TCollectionItem)
  private
    FtpDoc  : TpcteTipoDocumentoAnterior;
    Fserie  : String;
    Fsubser : String;
    FnDoc   : Integer;
    FdEmi   : TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpDoc: TpcteTipoDocumentoAnterior read FtpDoc  write FtpDoc;
    property serie: String                     read Fserie  write Fserie;
    property subser: String                    read Fsubser write Fsubser;
    property nDoc: Integer                     read FnDoc   write FnDoc;
    property dEmi: TDateTime                   read FdEmi   write FdEmi;
  end;

  TIdDocAntEleCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdDocAntEleCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdDocAntEleCollectionItem);
  public
    constructor Create(AOwner: TIdDocAntCollectionItem);
    function Add: TIdDocAntEleCollectionItem;
    property Items[Index: Integer]: TIdDocAntEleCollectionItem read GetItem write SetItem; default;
  end;

  TIdDocAntEleCollectionItem = class(TCollectionItem)
  private
    Fchave : String;
    FchCTe : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chave: String read Fchave write Fchave;
    property chCTe: String read FchCTe write FchCTe;
  end;

  TSegCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSegCollectionItem;
    procedure SetItem(Index: Integer; Value: TSegCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TSegCollectionItem;
    property Items[Index: Integer]: TSegCollectionItem read GetItem write SetItem; default;
  end;

  TSegCollectionItem = class(TCollectionItem)
  private
    FrespSeg : TpcteRspSeg;
    FxSeg    : String;
    FnApol   : String;
    FnAver   : String;
    FvCarga  : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property respSeg: TpcteRspSeg read FrespSeg write FrespSeg;
    property xSeg: String         read FxSeg    write FxSeg;
    property nApol: String        read FnApol   write FnApol;
    property nAver: String        read FnAver   write FnAver;
    property vCarga: Currency     read FvCarga  write FvCarga;
  end;

////////////////////////////////////////////////////////////////////////////////
// Modal Rodoviário
////////////////////////////////////////////////////////////////////////////////

  TRodo = class(TPersistent)
  private
    FRNTRC   : String;
    FdPrev   : tDateTime;
    FLota    : TpcteLotacao;
    FCIOT    : String;
    Focc     : TOccCollection;
    FvalePed : TValePedCollection;
    Fveic    : TVeicCollection;
    FlacRodo : TLacRodoCollection;
    Fmoto    : TMotoCollection;

    procedure SetOcc(const Value: TOccCollection);
    procedure SetValePed(const Value: TValePedCollection);
    procedure SetVeic(const Value: TVeicCollection);
    procedure SetLacRodo(const Value: TLacRodoCollection);
    procedure SetMoto(const Value: TMotoCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property RNTRC: String               read FRNTRC   write FRNTRC;
    property dPrev: TDateTime            read FdPrev   write FdPrev;
    property lota: TpcteLotacao          read FLota    write FLota;
    property CIOT: String                read FCIOT    write FCIOT;
    property occ: TOccCollection         read Focc     write SetOcc;
    property valePed: TValePedCollection read FvalePed write SetValePed;
    property veic: TVeicCollection       read Fveic    write SetVeic;
    property lacRodo: TLacRodoCollection read FlacRodo write SetLacRodo;
    property moto: TMotoCollection       read Fmoto    write SetMoto;
  end;

  TOccCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOccCollectionItem;
    procedure SetItem(Index: Integer; Value: TOccCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TOccCollectionItem;
    property Items[Index: Integer]: TOccCollectionItem read GetItem write SetItem; default;
  end;

  TOccCollectionItem = class(TCollectionItem)
  private
    Fserie  : String;
    FnOcc   : Integer;
    FdEmi   : TDateTime;
    FemiOCC : TEmiOCC;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property serie: String   read Fserie  write Fserie;
    property nOcc: Integer   read FnOcc   write FnOcc;
    property dEmi: TDateTime read FdEmi   write FdEmi;
    property emiOcc: TEmiOCC read FemiOCC write FemiOCC;
  end;

  TEmiOCC = class(TPersistent)
  private
    FCNPJ : String;
    FcInt : String;
    FIE   : String;
    FUF   : String;
    Ffone : String;
  published
    property CNPJ: String read FCNPJ write FCNPJ;
    property cInt: String read FcInt write FcInt;
    property IE: String   read FIE   write FIE;
    property UF: String   read FUF   write FUF;
    property fone: String read Ffone write Ffone;
  end;

  TValePedCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TValePedCollectionItem;
    procedure SetItem(Index: Integer; Value: TValePedCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TValePedCollectionItem;
    property Items[Index: Integer]: TValePedCollectionItem read GetItem write SetItem; default;
  end;

  TValePedCollectionItem = class(TCollectionItem)
  private
    FCNPJForn : String;
    FnCompra  : String;
    FCNPJPg   : String;
    FvValePed : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJForn: String   read FCNPJForn write FCNPJForn;
    property nCompra: String    read FnCompra  write FnCompra;
    property CNPJPg: String     read FCNPJPg   write FCNPJPg;
    property vValePed: Currency read FvValePed write FvValePed;
  end;

  TVeicCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TVeicCollectionItem;
    procedure SetItem(Index: Integer; Value: TVeicCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TVeicCollectionItem;
    property Items[Index: Integer]: TVeicCollectionItem read GetItem write SetItem; default;
  end;

  TVeicCollectionItem = class(TCollectionItem)
  private
    FcInt    : String;
    FRENAVAM : String;
    Fplaca   : String;
    Ftara    : Integer;
    FcapKG   : Integer;
    FcapM3   : Integer;
    FtpProp  : TpcteTipoPropriedade;
    FtpVeic  : TpcteTipoVeiculo;
    FtpRod   : TpcteTipoRodado;
    FtpCar   : TpcteTipoCarroceria;
    FUF      : String;
    Fprop    : TProp;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cInt: String                 read FcInt    write FcInt;
    property RENAVAM: String              read FRENAVAM write FRENAVAM;
    property placa: String                read Fplaca   write Fplaca;
    property tara: Integer                read Ftara    write Ftara;
    property capKG: Integer               read FcapKG   write FcapKG;
    property capM3: Integer               read FcapM3   write FcapM3;
    property tpProp: TpcteTipoPropriedade read FtpProp  write FtpProp;
    property tpVeic: TpcteTipoVeiculo     read FtpVeic  write FtpVeic;
    property tpRod: TpcteTipoRodado       read FtpRod   write FtpRod;
    property tpCar: TpcteTipoCarroceria   read FtpCar   write FtpCar;
    property UF: String                   read FUF      write FUF;
    property Prop: TProp                  read Fprop    write Fprop;
  end;

  TProp = class(TPersistent)
  private
    FCNPJCPF : String;
    FRNTRC   : String;
    FxNome   : String;
    FIE      : String;
    FUF      : String;
    FtpProp  : TpcteProp;
  published
    property CNPJCPF: String   read FCNPJCPF write FCNPJCPF;
    property RNTRC: String     read FRNTRC   write FRNTRC;
    property xNome: String     read FxNome   write FxNome;
    property IE: String        read FIE      write FIE;
    property UF: String        read FUF      write FUF;
    property tpProp: TpcteProp read FtpProp  write FtpProp;
  end;

  TLacRodoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLacRodoCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacRodoCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TLacRodoCollectionItem;
    property Items[Index: Integer]: TLacRodoCollectionItem read GetItem write SetItem; default;
  end;

  TLacRodoCollectionItem = class(TCollectionItem)
  private
    FnLacre : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TMotoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMotoCollectionItem;
    procedure SetItem(Index: Integer; Value: TMotoCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TMotoCollectionItem;
    property Items[Index: Integer]: TMotoCollectionItem read GetItem write SetItem; default;
  end;

  TMotoCollectionItem = class(TCollectionItem)
  private
    FxNome : String;
    FCPF   : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: String read FxNome write FxNome;
    property CPF: String   read FCPF   write FCPF;
  end;

  TRodoOS = class(TPersistent)
  private
    FNroRegEstadual: String;
    FTAF: String;
    Fveic: TVeicOS;
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property TAF: String            read FTAF            write FTAF;
    property NroRegEstadual: String read FNroRegEstadual write FNroRegEstadual;
    property veic: TVeicOS          read Fveic           write Fveic;
  end;

  TVeicOS = class(TPersistent)
  private
    Fplaca: String;
    FRENAVAM: String;
    FUF: String;
    Fprop: TPropOS;
  public
    constructor Create(AOwner: TRodoOS);
    destructor Destroy; override;
  published
    property placa: String   read Fplaca   write Fplaca;
    property RENAVAM: String read FRENAVAM write FRENAVAM;
    property prop: TPropOS   read Fprop    write Fprop;
    property UF: String      read FUF      write FUF;
  end;

  TPropOS = class(TPersistent)
  private
    FCNPJCPF: String;
    FTAF: String;
    FNroRegEstadual: String;
    FxNome: String;
    FIE: String;
    FUF: String;
    FtpProp: TpcteProp;
  published
    property CNPJCPF: String        read FCNPJCPF        write FCNPJCPF;
    property TAF: String            read FTAF            write FTAF;
    property NroRegEstadual: String read FNroRegEstadual write FNroRegEstadual;
    property xNome: String          read FxNome          write FxNome;
    property IE: String             read FIE             write FIE;
    property UF: String             read FUF             write FUF;
    property tpProp: TpcteProp      read FtpProp         write FtpProp;
  end;

////////////////////////////////////////////////////////////////////////////////
// Modal Aéreo
////////////////////////////////////////////////////////////////////////////////

  TAereo = class(TPersistent)
  private
    FnMinu      : Integer;
    FnOCA       : String;
    FdPrevAereo : tDateTime;
    FxLAgEmi    : String;
    FIdT        : String;
    Ftarifa     : TTarifa;
    FnatCarga   : TNatCarga;
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property nMinu: Integer        read FnMinu      write Fnminu;
    property nOCA: String          read FnOCA       write FnOCA;
    property dPrevAereo: TDateTime read FdPrevAereo write FdPrevAereo;
    property xLAgEmi: String       read FxLAgEmi    write FxLAgEmi;
    property IdT: String           read FIdT        write FIdT;
    property tarifa: TTarifa       read Ftarifa     write Ftarifa;
    property natCarga: TNatCarga   read FnatCarga   write FnatCarga;
  end;

  TTarifa = class(TPersistent)
  private
    FCL   : String;
    FcTar : String;
    FvTar : Currency;
  public
    property CL: String     read FCL   write FCL;
    property cTar: String   read FcTar write FcTar;
    property vTar: Currency read FvTar write FvTar;
  end;

  { TNatCarga }

  TNatCarga = class(TPersistent)
  private
    FxDime    : String;
    FcinfManu : TpInfManuCollection;
    FcIMP     : String;  // Alterar para ser uma lista

    procedure SetcinfManu(const Value: TpInfManuCollection);
  public
    constructor Create(AOwner: TAereo);
    destructor Destroy; override;

    property xDime:    String              read FxDime    write FxDime;
    property cinfManu: TpInfManuCollection read FcinfManu write SetcinfManu;
    property cIMP:     String              read FcIMP     write FcIMP;
  end;

  TpInfManuCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TpInfManuCollectionItem;
    procedure SetItem(Index: Integer; Value: TpInfManuCollectionItem);
  public
    constructor Create(AOwner: TNatCarga);

    function Add: TpInfManuCollectionItem;
    property Items[Index: Integer]: TpInfManuCollectionItem read GetItem write SetItem; default;
  end;

  TpInfManuCollectionItem = class(TCollectionItem)
  private
    FnInfManu : TpInfManu;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nInfManu : TpInfManu  read FnInfManu   write FnInfManu;
  end;

////////////////////////////////////////////////////////////////////////////////
// Modal Aquaviário
////////////////////////////////////////////////////////////////////////////////

  TAquav = class(TPersistent)
  private
    FvPrest   : Currency;
    FvAFRMM   : Currency;
    FnBooking : String;
    FnCtrl    : String;
    FxNavio   : String;
    Fbalsa    : TBalsaCollection;
    FnViag    : String;
    Fdirec    : TpcteDirecao;
    FprtEmb   : String;
    FprtTrans : String;
    FprtDest  : String;
    FtpNav    : TTipoNavegacao;
    Firin     : String;
    FdetCont  : TdetContCollection;

    procedure SetBalsa(const Value: TbalsaCollection);
    procedure SetdetCont(const Value: TdetContCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property vPrest: Currency            read FvPrest   write FvPrest;
    property vAFRMM: Currency            read FvAFRMM   write FvAFRMM;
    property nBooking: String            read FnBooking write FnBooking;
    property nCtrl: String               read FnCtrl    write FnCtrl;
    property xNavio: String              read FxNavio   write FxNavio;
    property balsa: TBalsaCollection     read Fbalsa    write Setbalsa;
    property nViag: String               read FnViag    write FnViag;
    property direc: TpcteDirecao         read Fdirec    write Fdirec;
    property prtEmb: String              read FprtEmb   write FprtEmb;
    property prtTrans: String            read FprtTrans write FprtTrans;
    property prtDest: String             read FprtDest  write FprtDest;
    property tpNav: TTipoNavegacao       read FtpNav    write FtpNav;
    property irin: String                read Firin     write Firin;
    property detCont: TdetContCollection read FdetCont  write SetdetCont;
  end;

  TBalsaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TBalsaCollectionItem;
    procedure SetItem(Index: Integer; Value: TBalsaCollectionItem);
  public
    constructor Create(AOwner: TAquav);
    function Add: TBalsaCollectionItem;
    property Items[Index: Integer]: TBalsaCollectionItem read GetItem write SetItem; default;
  end;

  TBalsaCollectionItem = class(TCollectionItem)
  private
    FxBalsa : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xBalsa: String read FxBalsa write FxBalsa;
  end;

  TdetContCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdetContCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetContCollectionItem);
  public
    constructor Create(AOwner: TAquav);
    function Add: TdetContCollectionItem;
    property Items[Index: Integer]: TdetContCollectionItem read GetItem write SetItem; default;
  end;

  TdetContCollectionItem = class(TCollectionItem)
  private
    FnCont  : String;
    FLacre  : TLacreCollection;
    FinfDoc : TinfDocAquav;

    procedure SetLacre(const Value: TLacreCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nCont: String           read FnCont  write FnCont;
    property Lacre: TLacreCollection read FLacre  write SetLacre;
    property infDoc: TinfDocAquav    read FinfDoc write FinfDoc;
  end;

  TLacreCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLacreCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacreCollectionItem);
  public
    constructor Create(AOwner: TdetContCollectionItem);
    function Add: TLacreCollectionItem;
    property Items[Index: Integer]: TLacreCollectionItem read GetItem write SetItem; default;
  end;

  TLacreCollectionItem = class(TCollectionItem)
  private
    FnLacre : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TInfDocAquav = class(TPersistent)
  private
    FinfNF  : TInfNFAquavCollection;
    FinfNFe : TInfNFeAquavCollection;

    procedure SetInfNFAquav(const Value: TInfNFAquavCollection);
    procedure SetInfNFeAquav(const Value: TInfNFeAquavCollection);
  public
    constructor Create(AOwner: TdetContCollectionItem);
    destructor Destroy; override;
  published
    property infNF: TInfNFAquavCollection   read FinfNF  write SetInfNFAquav;
    property infNFe: TInfNFeAquavCollection read FinfNFe write SetInfNFeAquav;
  end;

  TInfNFAquavCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFAquavCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFAquavCollectionItem);
  public
    constructor Create(AOwner: TInfDocAquav);
    function Add: TInfNFAquavCollectionItem;
    property Items[Index: Integer]: TInfNFAquavCollectionItem read GetItem write SetItem; default;
  end;

  TInfNFAquavCollectionItem = class(TCollectionItem)
  private
    Fserie   : String;
    FnDoc    : String;
    FunidRat : Double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property serie: String   read Fserie   write Fserie;
    property nDoc: String    read FnDoc    write FnDoc;
    property unidRat: Double read FunidRat write FunidRat;
  end;

  TInfNFeAquavCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFeAquavCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFeAquavCollectionItem);
  public
    constructor Create(AOwner: TInfDocAquav);
    function Add: TInfNFeAquavCollectionItem;
    property Items[Index: Integer]: TInfNFeAquavCollectionItem read GetItem write SetItem; default;
  end;

  TInfNFeAquavCollectionItem = class(TCollectionItem)
  private
    Fchave   : String;
    FunidRat : Double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chave: String   read Fchave   write Fchave;
    property unidRat: Double read FunidRat write FunidRat;
  end;

////////////////////////////////////////////////////////////////////////////////
// Modal Ferroviário
////////////////////////////////////////////////////////////////////////////////

  TFerrov = class(TPersistent)
  private
    FtpTraf   : TpcteTipoTrafego;
    FtrafMut  : TTrafMut;
    Ffluxo    : String;
    FidTrem   : String;
    FvFrete   : Currency;
    FferroEnv : TFerroEnvCollection;
    FdetVag   : TDetVagCollection;

    procedure SetFerroEnv(const Value: TFerroEnvCollection);
    procedure SetDetVag(const Value: TDetVagCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property tpTraf: TpcteTipoTrafego      read FtpTraf   write FtpTraf;
    property trafMut: TTrafMut             read FtrafMut  write FTrafMut;
    property fluxo: String                 read Ffluxo    write Ffluxo;
    property idTrem: String                read FidTrem   write FidTrem;
    property vFrete: Currency              read FvFrete   write FvFrete;
    property ferroEnv: TFerroEnvCollection read FferroEnv write SetferroEnv;
    property detVag: TDetVagCollection     read FdetVag   write SetdetVag;
  end;

  TTrafMut = class(TPersistent)
  private
    FrespFat : TpcteTrafegoMutuo;
    FferrEmi : TpcteTrafegoMutuo;
  published
    property respFat: TpcteTrafegoMutuo read FrespFat write FrespFat;
    property ferrEmi: TpcteTrafegoMutuo read FferrEmi write FferrEmi;
  end;

  TFerroEnvCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TFerroEnvCollectionItem;
    procedure SetItem(Index: Integer; Value: TFerroEnvCollectionItem);
  public
    constructor Create(AOwner: TFerrov);
    function Add: TFerroEnvCollectionItem;
    property Items[Index: Integer]: TFerroEnvCollectionItem read GetItem write SetItem; default;
  end;

  TFerroEnvCollectionItem = class(TCollectionItem)
  private
    FxNome      : String;
    FIE         : String;
    FCNPJ       : String;
    FcInt       : String;
    FenderFerro : TEnderFerro;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJ: String            read FCNPJ       write FCNPJ;
    property cInt: String            read FcInt       write FcInt;
    property IE: String              read FIE         write FIE;
    property xNome: String           read FxNome      write FxNome;
    property enderFerro: TEnderFerro read FenderFerro write FenderFerro;
  end;

  TEnderFerro = class(TPersistent)
  private
    FxLgr    : String;
    Fnro     : String;
    FxCpl    : String;
    FxBairro : String;
    FcMun    : Integer;
    FxMun    : String;
    FCEP     : Integer;
    FUF      : String;
  published
    property xLgr: String    read FxLgr    write FxLgr;
    property nro: String     read Fnro     write Fnro;
    property xCpl: String    read FxCpl    write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: String    read FxMun    write FxMun;
    property CEP: Integer    read FCEP     write FCEP;
    property UF: String      read FUF      write FUF;
  end;

  TDetVagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDetVagCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetVagCollectionItem);
  public
    constructor Create(AOwner: TFerrov);
    function Add: TDetVagCollectionItem;
    property Items[Index: Integer]: TDetVagCollectionItem read GetItem write SetItem; default;
  end;

  TDetVagCollectionItem = class(TCollectionItem)
  private
    FnVag   : Integer;
    Fcap    : Currency;
    FtpVag  : String;
    FpesoR  : Currency;
    FpesoBC : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nVag: Integer    read FnVag   write FnVag;
    property cap: Currency    read Fcap    write Fcap;
    property tpVag: String    read FtpVag  write FtpVag;
    property pesoR: Currency  read FpesoR  write FpesoR;
    property pesoBC: Currency read FpesoBC write FpesoBC;
  end;

////////////////////////////////////////////////////////////////////////////////
// Modal Dutoviário
////////////////////////////////////////////////////////////////////////////////

  TDuto = class(TPersistent)
  private
    FvTar : Currency;
    FdIni : TDateTime;
    FdFim : TDateTime;
  published
    property vTar: Currency  read FvTar write FvTar;
    property dIni: TDateTime read FdIni write FdIni;
    property dFim: TDateTime read FdFim write FdFim;
  end;

////////////////////////////////////////////////////////////////////////////////
// Multimodal
////////////////////////////////////////////////////////////////////////////////

  TMultimodal = class(TPersistent)
  private
    FCOTM: String;
    FindNegociavel: TpcnindNegociavel;
    FxSeg: string;
    FCNPJ: string;
    FnApol: string;
    FnAver: string;
  published
    property COTM: String  read FCOTM write FCOTM;
    property indNegociavel: TpcnindNegociavel read FindNegociavel write FindNegociavel;
    property xSeg: String  read FxSeg  write FxSeg;
    property CNPJ: String  read FCNPJ  write FCNPJ;
    property nApol: String read FnApol write FnApol;
    property nAver: String read FnAver write FnAver;
  end;

////////////////////////////////////////////////////////////////////////////////

  TPeriCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPeriCollectionItem;
    procedure SetItem(Index: Integer; Value: TPeriCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TPeriCollectionItem;
    property Items[Index: Integer]: TPeriCollectionItem read GetItem write SetItem; default;
  end;

  TPeriCollectionItem = class(TCollectionItem)
  private
    FnONU        : String;
    FxNomeAE     : String;
    FxClaRisco   : String;
    FgrEmb       : String;
    FqTotProd    : String;
    FqVolTipo    : String;
    FpontoFulgor : String;
    FqTotEmb: String;
    FuniAP: TpUniMed;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nONU: String        read FnONU        write FnONU;
    property xNomeAE: String     read FxNomeAE     write FxNomeAE;
    property xClaRisco: String   read FxClaRisco   write FxClaRisco;
    property grEmb: String       read FgrEmb       write FgrEmb;
    property qTotProd: String    read FqTotProd    write FqTotProd;
    property qVolTipo: String    read FqVolTipo    write FqVolTipo;
    property pontoFulgor: String read FpontoFulgor write FpontoFulgor;
    property qTotEmb: String     read FqTotEmb     write FqTotEmb;
    property uniAP: TpUniMed     read FuniAP       write FuniAP;
  end;

  TVeicNovosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TVeicNovosCollectionItem;
    procedure SetItem(Index: Integer; Value: TVeicNovosCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TVeicNovosCollectionItem;
    property Items[Index: Integer]: TVeicNovosCollectionItem read GetItem write SetItem; default;
  end;

  TVeicNovosCollectionItem = class(TCollectionItem)
  private
    Fchassi : String;
    FcCor   : String;
    FxCor   : String;
    FcMod   : String;
    FvUnit  : Currency;
    FvFrete : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chassi: String   read Fchassi write Fchassi;
    property cCor: String     read FcCor   write FcCor;
    property xCor: String     read FxCor   write FxCor;
    property cMod: String     read FcMod   write FcMod;
    property vUnit: Currency  read FvUnit  write FvUnit;
    property vFrete: Currency read FvFrete write FvFrete;
  end;

  TCobr = class(TPersistent)
  private
    Ffat : TFat;
    Fdup : TDupCollection;

    procedure SetDup(Value: TDupCollection);
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property fat: TFat           read Ffat write Ffat;
    property dup: TDupCollection read Fdup write SetDup;
  end;

  TFat = class(TPersistent)
  private
    FnFat  : String;
    FvOrig : Currency;
    FvDesc : Currency;
    FvLiq  : Currency;
  published
    property nFat: String    read FnFat  write FnFat;
    property vOrig: Currency read FvOrig write FvOrig;
    property vDesc: Currency read FvDesc write FvDesc;
    property vLiq: Currency  read FvLiq  write FvLiq;
  end;

  TDupCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDupCollectionItem;
    procedure SetItem(Index: Integer; Value: TDupCollectionItem);
  public
    constructor Create(AOwner: TCobr);
    function Add: TDupCollectionItem;
    property Items[Index: Integer]: TDupCollectionItem read GetItem write SetItem; default;
  end;

  TDupCollectionItem = class(TCollectionItem)
  private
    FnDup  : String;
    FdVenc : TDateTime;
    FvDup  : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nDup: String     read FnDup  write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: Currency   read FvDup  write FvDup;
  end;

  TInfCteSub = class(TPersistent)
  private
    FchCte: String;
    FrefCteAnu: String;
    FtomaICMS: TTomaICMS;
    FtomaNaoICMS: TTomaNaoICMS;
    FindAlteraToma: TIndicador;
  public
    constructor Create(AOwner: TInfCTeNorm);
    destructor Destroy; override;
  published
    property chCte: String             read FchCte         write FchCte;
    property refCteAnu: String         read FrefCteAnu     write FrefCteAnu;
    property tomaICMS: TTomaICMS       read FtomaICMS      write FtomaICMS;
    property tomaNaoICMS: TTomaNaoICMS read FtomaNaoICMS   write FtomaNaoICMS;
    property indAlteraToma: TIndicador read FindAlteraToma write FindAlteraToma default tiNao;
  end;

  TTomaICMS = class(TPersistent)
  private
    FrefNFe : String;
    FrefNF  : TRefNF;
    FrefCte : String;
  public
    constructor Create(AOwner: TInfCteSub);
    destructor Destroy; override;
  published
    property refNFe: String read FrefNFe write FrefNFe;
    property refNF: TRefNF  read FrefNF  write FrefNF;
    property refCte: String read FrefCte write FrefCte;
  end;

  TRefNF = class(TPersistent)
  private
    FCNPJCPF  : String;
    Fmod      : String;
    Fserie    : Integer;
    Fsubserie : Integer;
    Fnro      : Integer;
    Fvalor    : Currency;
    FdEmi     : TDateTime;
  published
    property CNPJCPF: String   read FCNPJCPF  write FCNPJCPF;
    property modelo: String    read Fmod      write Fmod;
    property serie: Integer    read Fserie    write Fserie;
    property subserie: Integer read Fsubserie write Fsubserie;
    property nro: Integer      read Fnro      write Fnro;
    property valor: Currency   read Fvalor    write Fvalor;
    property dEmi: TDateTime   read FdEmi     write FdEmi;
  end;

  TTomaNaoICMS = class(TPersistent)
  private
    FrefCteAnu : String;
  published
    property refCteAnu: String read FrefCteAnu write FrefCteAnu;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfCteComp = class(TPersistent)
  private
    FChave     : String;
//    FvPresComp : TVPresComp;
//    FimpComp   : TImpComp;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property chave: String         read FChave     write FChave;
//    property vPresComp: TVPresComp read FvPresComp write FvPresComp;
//    property impComp: TImpComp     read FimpComp   write FimpComp;
  end;

  TVPresComp = class(TPersistent)
  private
    FvTPrest  : Currency;
    FcompComp : TCompCompCollection;

    procedure SetCompCompItem(const Value: TCompCompCollection);
  public
    constructor Create(AOwner: TInfCteComp);
    destructor Destroy; override;
  published
    property vTPrest: Currency             read FvTPrest  write FvTPrest;
    property compComp: TCompCompCollection read FcompComp write SetCompCompItem;
  end;

  TCompCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCompCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompCompCollectionItem);
  public
    constructor Create(AOwner: TVPresComp);
    function Add: TCompCompCollectionItem;
    property Items[Index: Integer]: TCompCompCollectionItem read GetItem write SetItem; default;
  end;

  TCompCompCollectionItem = class(TCollectionItem)
  private
    FxNome : String;
    FvComp : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: String   read FxNome write FxNome;
    property vComp: Currency read FvComp write FvComp;
  end;

  TImpComp = class(TPersistent)
  private
    FICMSComp   : TICMSComp;
    FvTotTrib   : Currency;
    FinfAdFisco : String;
  public
    constructor Create(AOwner: TInfCteComp);
    destructor Destroy; override;
  published
    property ICMSComp: TICMSComp read FICMSComp   write FICMSComp;
    property vTotTrib: Currency  read FvTotTrib   write FvTotTrib;
    property infAdFisco: String  read FinfAdFisco write FinfAdFisco;
  end;

  TICMSComp = class(TPersistent)
  private
    FSituTrib    : TpcnCSTIcms;

    FCST00       : TCST00;
    FCST20       : TCST20;
    FCST45       : TCST45;
    FCST60       : TCST60;
    FCST90       : TCST90;
    FICMSOutraUF : TICMSOutraUF;
    FICMSSN      : TICMSSN;
  public
    constructor Create(AOwner: TImpComp);
    destructor Destroy; override;
  published
    property SituTrib: TpcnCSTIcms     read FSituTrib    write FSituTrib;

    property ICMS00: TCST00            read FCST00       write FCST00;
    property ICMS20: TCST20            read FCST20       write FCST20;
    property ICMS45: TCST45            read FCST45       write FCST45;
    property ICMS60: TCST60            read FCST60       write FCST60;
    property ICMS90: TCST90            read FCST90       write FCST90;
    property ICMSOutraUF: TICMSOutraUF read FICMSOutraUF write FICMSOutraUF;
    property ICMSSN: TICMSSN           read FICMSSN      write FICMSSN;
  end;

////////////////////////////////////////////////////////////////////////////////

  TInfCteAnu = class(TPersistent)
  private
    FchCTe : String;
    FdEmi  : TDateTime;
  published
    property chCTe: String   read FchCTe write FchCTe;
    property dEmi: TDateTime read FdEmi  write FdEmi;
  end;

  ////////////////////////////////////////////////////////////////////////////////

  TautXMLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TautXMLCollectionItem = class(TCollectionItem)
  private
    FCNPJCPF: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

const
  CMUN_EXTERIOR : Integer = 9999999;
  XMUN_EXTERIOR : String  = 'EXTERIOR';
  UF_EXTERIOR   : String  = 'EX';

implementation

Uses
  ACBrUtil;

{ TCTe }

constructor TCTe.Create;
begin
  FinfCTe := TInfCTe.Create;
  Fide    := TIde.Create(Self);
  Fcompl  := TCompl.Create(Self);

  Femit  := TEmit.Create(Self);
  FToma  := TToma.Create(Self);
  Frem   := TRem.Create(Self);;
  Fexped := TExped.Create(Self);
  Freceb := TReceb.Create(Self);
  Fdest  := TDest.Create(Self);

  FvPrest := TVPrest.Create(Self);
  Fimp    := TImp.Create(Self);

  FinfCTeNorm := TInfCTeNorm.Create(Self);
  FinfCTeComp := TInfCteComp.Create(Self);
  FinfCteAnu  := TInfCteAnu.Create;
  FautXML     := TautXMLCollection.Create(Self);

  FProcCTe   := TProcCTe.create;
  Fsignature := Tsignature.create;
end;

destructor TCTe.Destroy;
begin
  FinfCTe.Free;
  Fide.Free;
  Fcompl.Free;

  Femit.Free;
  Ftoma.Free;
  Frem.Free;
  Fexped.Free;
  Freceb.Free;
  Fdest.Free;

  FvPrest.Free;
  Fimp.Free;

  FinfCTeNorm.Free;
  FInfCTeComp.Free;
  FInfCTeAnu.Free;
  FautXML.Free;
  
  FProcCTe.Free;
  Fsignature.Free;

  inherited Destroy;
end;

procedure TCTe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

{ TInfCTe }

function TInfCTe.GetVersaoStr: String;
begin
  if FVersao <= 0 then
     Result := V2_00
  else
     Result := 'versao="'+FloatToString(FVersao,'.','#0.00')+'"';
end;

{ TIde }

constructor TIde.Create(AOwner: TCTe);
begin
  inherited Create;
  FToma03 := TToma03.Create;
  FToma4  := TToma4.Create( AOwner );
  FinfPercurso := TinfPercursoCollection.Create(Self);
  FindGlobalizado := tiNao;
end;

destructor TIde.Destroy;
begin
  FToma03.Free;
  FToma4.Free;
  FinfPercurso.Free;
  inherited;
end;

procedure TIde.SetinfPercurso(Value: TinfPercursoCollection);
begin
  FinfPercurso.Assign(Value);
end;

{ TToma4 }

constructor TToma4.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderToma := TEndereco.Create;
end;

destructor TToma4.Destroy;
begin
  FEnderToma.Free;
  inherited;
end;

{ TinfPercursoCollection }

function TinfPercursoCollection.Add: TinfPercursoCollectionItem;
begin
  Result := TinfPercursoCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfPercursoCollection.Create(AOwner: Tide);
begin
  inherited Create(TinfPercursoCollectionItem);
end;

function TinfPercursoCollection.GetItem(
  Index: Integer): TinfPercursoCollectionItem;
begin
  Result := TinfPercursoCollectionItem(inherited GetItem(Index));
end;

procedure TinfPercursoCollection.SetItem(Index: Integer;
  Value: TinfPercursoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfPercursoCollectionItem }

constructor TinfPercursoCollectionItem.Create;
begin

end;

destructor TinfPercursoCollectionItem.Destroy;
begin

  inherited;
end;

{ TCompl }

constructor TCompl.Create(AOwner: TCTe);
begin
  inherited Create;
  Ffluxo    := TFluxo.Create(Self);
  FEntrega  := TEntrega.Create(Self);
  FObsCont  := TObsContCollection.Create(Self);
  FObsFisco := TObsFiscoCollection.Create(Self);
end;

destructor TCompl.Destroy;
begin
  Ffluxo.Free;
  FEntrega.Free;
  FObsCont.Free;
  FObsFisco.Free;
  inherited;
end;

procedure TCompl.SetObsCont(Value: TObsContCollection);
begin
 FObsCont.Assign(Value);
end;

procedure TCompl.SetObsFisco(Value: TObsFiscoCollection);
begin
 FObsFisco.Assign(Value);
end;

{ TFluxo }

constructor TFluxo.Create(AOwner: TCompl);
begin
  inherited Create;
  Fpass := TPassCollection.Create(Self);
end;

destructor TFluxo.Destroy;
begin
  Fpass.Free;
  inherited;
end;

procedure TFluxo.SetPass(Value: TPassCollection);
begin
  Fpass.Assign(Value);
end;

{ TPassCollection }

function TPassCollection.Add: TPassCollectionItem;
begin
  Result := TPassCollectionItem(inherited Add);
  Result.create;
end;

constructor TPassCollection.Create(AOwner: TFluxo);
begin
  inherited Create(TPassCollectionItem);
end;

function TPassCollection.GetItem(Index: Integer): TPassCollectionItem;
begin
  Result := TPassCollectionItem(inherited GetItem(Index));
end;

procedure TPassCollection.SetItem(Index: Integer;
  Value: TPassCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TPassCollectionItem }

constructor TPassCollectionItem.Create;
begin

end;

destructor TPassCollectionItem.Destroy;
begin

  inherited;
end;

{ TEntrega }

constructor TEntrega.Create(AOwner: TCompl);
begin
  inherited Create;
  FsemData   := TSemData.Create;
  FcomData   := TComData.Create;
  FnoPeriodo := TNoPeriodo.Create;
  FsemHora   := TSemHora.Create;
  FcomHora   := TComHora.Create;
  FnoInter   := TNoInter.Create;
end;

destructor TEntrega.Destroy;
begin
  FsemData.Free;
  FcomData.Free;
  FnoPeriodo.Free;
  FsemHora.Free;
  FcomHora.Free;
  FnoInter.Free;
  inherited;
end;

{ TObsContCollection }

function TObsContCollection.Add: TObsContCollectionItem;
begin
  Result := TObsContCollectionItem(inherited Add);
  Result.create;
end;

constructor TObsContCollection.Create(AOwner: TCompl);
begin
  inherited Create(TObsContCollectionItem);
end;

function TObsContCollection.GetItem(
  Index: Integer): TObsContCollectionItem;
begin
  Result := TObsContCollectionItem(inherited GetItem(Index));
end;

procedure TObsContCollection.SetItem(Index: Integer;
  Value: TObsContCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TObsContCollectionItem }

constructor TObsContCollectionItem.Create;
begin

end;

destructor TObsContCollectionItem.Destroy;
begin

  inherited;
end;

{ TObsFiscoCollection }

function TObsFiscoCollection.Add: TObsFiscoCollectionItem;
begin
  Result := TObsFiscoCollectionItem(inherited Add);
  Result.create;
end;

constructor TObsFiscoCollection.Create(AOwner: TCompl);
begin
  inherited Create(TObsFiscoCollectionItem);
end;

function TObsFiscoCollection.GetItem(
  Index: Integer): TObsFiscoCollectionItem;
begin
  Result := TObsFiscoCollectionItem(inherited GetItem(Index));
end;

procedure TObsFiscoCollection.SetItem(Index: Integer;
  Value: TObsFiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TObsFiscoCollectionItem }

constructor TObsFiscoCollectionItem.Create;
begin

end;

destructor TObsFiscoCollectionItem.Destroy;
begin

  inherited;
end;

{ TEmit }

constructor TEmit.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderEmit := TEnderEmit.Create;
end;

destructor TEmit.Destroy;
begin
  FEnderEmit.Free;
  inherited;
end;

{ TRem }

constructor TRem.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderReme := TEndereco.Create;
  FlocColeta := TlocColeta.Create;
end;

destructor TRem.Destroy;
begin
  FEnderReme.Free;
  FlocColeta.Free;
  inherited;
end;

{ TExped }

constructor TExped.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderExped := TEndereco.Create;
end;

destructor TExped.Destroy;
begin
  FEnderExped.Free;
  inherited;
end;

{ TReceb }

constructor TReceb.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderReceb := TEndereco.Create;
end;

destructor TReceb.Destroy;
begin
  FEnderReceb.Free;
  inherited;
end;

{ TDest }

constructor TDest.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderDest := TEndereco.Create;
  FlocEnt    := TlocEnt.Create;
end;

destructor TDest.Destroy;
begin
  FEnderDest.Free;
  FlocEnt.Free;
  inherited;
end;

{ TVPrest }

constructor TVPrest.Create(AOwner: TCTe);
begin
  inherited Create;
  FComp := TCompCollection.Create(Self);
end;

destructor TVPrest.Destroy;
begin
  FComp.Free;
  inherited;
end;

procedure TVPrest.SetCompItem(const Value: TCompCollection);
begin
  Fcomp.Assign(Value);
end;

{ TCompCollection }

function TCompCollection.Add: TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited Add);
  Result.create;
end;

constructor TCompCollection.Create(AOwner: TVPrest);
begin
  inherited Create(TCompCollectionItem);
end;

function TCompCollection.GetItem(Index: Integer): TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited GetItem(Index));
end;

procedure TCompCollection.SetItem(Index: Integer;
  Value: TCompCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCompCollectionItem }

constructor TCompCollectionItem.Create;
begin

end;

destructor TCompCollectionItem.Destroy;
begin

  inherited;
end;

{ TImp }

constructor TImp.Create(AOwner: TCTe);
begin
  FICMS := TICMS.Create(Self);
  FICMSUFFim := TICMSUFFim.Create;
  FinfTribFed := TinfTribFed.Create;
end;

destructor TImp.Destroy;
begin
  FICMS.free;
  FICMSUFFim.free;
  FinfTribFed.Free;
  inherited;
end;

{ TICMS }

constructor TICMS.Create(AOwner: TImp);
begin
  inherited Create;
  FCST00       := TCST00.create;
  FCST20       := TCST20.create;
  FCST45       := TCST45.create;
  FCST60       := TCST60.create;
  FCST90       := TCST90.create;
  FICMSOutraUF := TICMSOutraUF.Create;
  FICMSSN      := TICMSSN.Create;
end;

destructor TICMS.Destroy;
begin
  FCST00.Free;
  FCST20.Free;
  FCST45.Free;
  FCST60.Free;
  FCST90.Free;
  FICMSOutraUF.Free;
  FICMSSN.Free;
  inherited;
end;

{ TInfCTeNorm }

constructor TInfCTeNorm.Create(AOwner: TCTe);
begin
  FinfCarga := TInfCarga.Create(Self);
  FinfDoc   := TInfDoc.Create(Self);
  FdocAnt   := TDocAnt.Create(Self);
  Fseg      := TSegCollection.Create(Self);

  Frodo       := TRodo.Create(Self);
  FrodoOS     := TRodoOS.Create(Self);
  Faereo      := TAereo.Create(Self);
  Faquav      := TAquav.Create(Self);
  Fferrov     := TFerrov.Create(Self);
  Fduto       := TDuto.Create;
  Fmultimodal := TMultimodal.Create;

  Fperi      := TPeriCollection.Create(Self);
  FveicNovos := TVeicNovosCollection.Create(Self);
  Fcobr      := TCobr.Create(Self);
  FinfCteSub := TInfCteSub.Create(Self);

  FinfGlobalizado := TinfGlobalizado.Create;
  FinfServVinc := TinfServVinc.Create(Self);
  FinfDocRef := TinfDocRefCollection.Create(Self);
  FinfServico := TinfServico.Create;
end;

destructor TInfCTeNorm.Destroy;
begin
  FinfCarga.Free;
  FinfDoc.Free;
  FdocAnt.Free;
  Fseg.Free;

  Frodo.Free;
  FrodoOS.Free;
  Faereo.Free;
  Faquav.Free;
  Fferrov.Free;
  Fduto.Free;
  Fmultimodal.Free;

  Fperi.Free;
  FveicNovos.Free;
  Fcobr.Free;
  FinfCteSub.Free;
  FinfGlobalizado.Free;
  FinfServVinc.Free;
  FinfDocRef.Free;
  FinfServico.Free;

  inherited;
end;

procedure TInfCTeNorm.SetSeg(const Value: TSegCollection);
begin
  Fseg := Value;
end;

procedure TInfCTeNorm.SetPeri(const Value: TPeriCollection);
begin
  Fperi := Value;
end;

procedure TInfCTeNorm.SetVeicNovos(const Value: TVeicNovosCollection);
begin
  FveicNovos := Value;
end;

procedure TInfCTeNorm.SetinfDocRef(const Value: TinfDocRefCollection);
begin
  FinfDocRef := Value;
end;

{ TInfCarga }

constructor TInfCarga.Create(AOwner: TInfCTeNorm);
begin
  FinfQ := TInfQCollection.Create(self);
end;

destructor TInfCarga.Destroy;
begin
  FinfQ.Free;
  inherited;
end;

procedure TInfCarga.SetInfQ(const Value: TInfQCollection);
begin
  FinfQ.Assign(Value);
end;

{ TInfQCollection }

function TInfQCollection.Add: TInfQCollectionItem;
begin
  Result := TInfQCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfQCollection.Create(AOwner: TInfCarga);
begin
  inherited Create(TInfQCollectionItem);
end;

function TInfQCollection.GetItem(Index: Integer): TInfQCollectionItem;
begin
  Result := TInfQCollectionItem(inherited GetItem(Index));
end;

procedure TInfQCollection.SetItem(Index: Integer;
  Value: TInfQCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfQCollectionItem }

constructor TInfQCollectionItem.Create;
begin

end;

destructor TInfQCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfDoc }

constructor TInfDoc.Create(AOwner: TInfCTeNorm);
begin
  FinfNF     := TInfNFCollection.Create(Self);
  FinfNFe    := TInfNFeCollection.Create(Self);
  FinfOutros := TInfOutrosCollection.Create(Self);
end;

destructor TInfDoc.Destroy;
begin
  FinfNF.Free;
  FinfNFe.Free;
  FinfOutros.Free;
  inherited;
end;

procedure TInfDoc.SetInfNF(const Value: TInfNFCollection);
begin
  FinfNF.Assign(Value);
end;

procedure TInfDoc.SetInfNFe(const Value: TInfNFeCollection);
begin
  FinfNFe.Assign(Value);
end;

procedure TInfDoc.SetInfOutros(const Value: TInfOutrosCollection);
begin
  FinfOutros.Assign(Value);
end;

{ TInfNFCollection }

function TInfNFCollection.Add: TInfNFCollectionItem;
begin
  Result := TInfNFCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFCollection.Create(AOwner: TInfDoc);
begin
  inherited Create(TInfNFCollectionItem);
end;

function TInfNFCollection.GetItem(Index: Integer): TInfNFCollectionItem;
begin
  Result := TInfNFCollectionItem(inherited GetItem(Index));
end;

procedure TInfNFCollection.SetItem(Index: Integer;
  Value: TInfNFCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFCollectionItem }

constructor TInfNFCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspNFCollection.Create(Self);
  FinfUnidCarga  := TInfUnidCargaNFCollection.Create(Self);
end;

destructor TInfNFCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FinfUnidCarga.Free;
  inherited;
end;

procedure TInfNFCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspNFCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TInfNFCollectionItem.SetinfUnidCarga(
  const Value: TinfUnidCargaNFCollection);
begin
  FinfUnidCarga := Value;
end;

{ TinfUnidTranspNFCollection }

function TinfUnidTranspNFCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspNFCollection.Create(
  AOwner: TinfNFCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspNFCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspNFCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

{ TinfUnidTranspCollectionItem }

constructor TinfUnidTranspCollectionItem.Create;
begin
  FlacUnidTransp := TlacUnidTranspCollection.Create(Self);
  FinfUnidCarga  := TinfUnidCargaCollection.Create(Self);
end;

destructor TinfUnidTranspCollectionItem.Destroy;
begin
  FlacUnidTransp.Free;
  FinfUnidCarga.Free;
  inherited;
end;

procedure TinfUnidTranspCollectionItem.SetlacUnidTransp(
  const Value: TlacUnidTranspCollection);
begin
  FlacUnidTransp := Value;
end;

procedure TinfUnidTranspCollectionItem.SetinfUnidCarga(
  const Value: TinfUnidCargaCollection);
begin
  FinfUnidCarga := Value;
end;

{ TlacUnidTranspCollection }

function TlacUnidTranspCollection.Add: TlacUnidTranspCollectionItem;
begin
  Result := TlacUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacUnidTranspCollection.Create(
  AOwner: TinfUnidTranspCollectionItem);
begin
  inherited Create(TlacUnidTranspCollectionItem);
end;

function TlacUnidTranspCollection.GetItem(
  Index: Integer): TlacUnidTranspCollectionItem;
begin
  Result := TlacUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TlacUnidTranspCollection.SetItem(Index: Integer;
  Value: TlacUnidTranspCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TlacUnidTranspCollectionItem }

constructor TlacUnidTranspCollectionItem.Create;
begin

end;

destructor TlacUnidTranspCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfUnidCargaCollection }

function TinfUnidCargaCollection.Add: TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidCargaCollection.Create(
  AOwner: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidCargaCollectionItem);
end;

function TinfUnidCargaCollection.GetItem(
  Index: Integer): TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidCargaCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfUnidCargaCollectionItem }

constructor TinfUnidCargaCollectionItem.Create;
begin
  FlacUnidCarga := TlacUnidCargaCollection.Create(Self);
end;

destructor TinfUnidCargaCollectionItem.Destroy;
begin
  FlacUnidCarga.Free;
  inherited;
end;

procedure TinfUnidCargaCollectionItem.SetlacUnidCarga(
  const Value: TlacUnidCargaCollection);
begin
  FlacUnidCarga := Value;
end;

{ TlacUnidCargaCollection }

function TlacUnidCargaCollection.Add: TlacUnidCargaCollectionItem;
begin
  Result := TlacUnidCargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacUnidCargaCollection.Create(
  AOwner: TinfUnidCargaCollectionItem);
begin
  inherited Create(TlacUnidCargaCollectionItem);
end;

function TlacUnidCargaCollection.GetItem(
  Index: Integer): TlacUnidCargaCollectionItem;
begin
  Result := TlacUnidCargaCollectionItem(inherited GetItem(Index));
end;

procedure TlacUnidCargaCollection.SetItem(Index: Integer;
  Value: TlacUnidCargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TlacUnidCargaCollectionItem }

constructor TlacUnidCargaCollectionItem.Create;
begin

end;

destructor TlacUnidCargaCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfUnidCargaNFCollection }

function TinfUnidCargaNFCollection.Add: TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidCargaNFCollection.Create(AOwner: TinfNFCollectionItem);
begin
  inherited Create(TinfUnidCargaCollectionItem);
end;

function TinfUnidCargaNFCollection.GetItem(
  Index: Integer): TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidCargaNFCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFeCollection }

function TInfNFeCollection.Add: TInfNFeCollectionItem;
begin
  Result := TInfNFeCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFeCollection.Create(AOwner: TInfDoc);
begin
  inherited Create(TInfNFeCollectionItem);
end;

function TInfNFeCollection.GetItem(Index: Integer): TInfNFeCollectionItem;
begin
  Result := TInfNFeCollectionItem(inherited GetItem(Index));
end;

procedure TInfNFeCollection.SetItem(Index: Integer;
  Value: TInfNFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFeCollectionItem }

constructor TInfNFeCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspNFeCollection.Create(Self);
  FinfUnidCarga  := TInfUnidCargaNFeCollection.Create(Self);
end;

destructor TInfNFeCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FinfUnidCarga.Free;
  inherited;
end;

procedure TInfNFeCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspNFeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TInfNFeCollectionItem.SetinfUnidCarga(
  const Value: TinfUnidCargaNFeCollection);
begin
  FinfUnidCarga := Value;
end;

{ TinfUnidTranspNFeCollection }

function TinfUnidTranspNFeCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspNFeCollection.Create(
  AOwner: TinfNFeCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspNFeCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspNFeCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

{ TinfUnidCargaNFeCollection }

function TinfUnidCargaNFeCollection.Add: TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidCargaNFeCollection.Create(AOwner: TinfNFeCollectionItem);
begin
  inherited Create(TinfUnidCargaCollectionItem);
end;

function TinfUnidCargaNFeCollection.GetItem(
  Index: Integer): TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidCargaNFeCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfOutrosCollection }

function TInfOutrosCollection.Add: TInfOutrosCollectionItem;
begin
  Result := TInfOutrosCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfOutrosCollection.Create(AOwner: TInfDoc);
begin
  inherited Create(TInfOutrosCollectionItem);
end;

function TInfOutrosCollection.GetItem(
  Index: Integer): TInfOutrosCollectionItem;
begin
  Result := TInfOutrosCollectionItem(inherited GetItem(Index));
end;

procedure TInfOutrosCollection.SetItem(Index: Integer;
  Value: TInfOutrosCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfOutrosCollectionItem }

constructor TInfOutrosCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspOutrosCollection.Create(Self);
  FinfUnidCarga  := TInfUnidCargaOutrosCollection.Create(Self);
end;

destructor TInfOutrosCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FinfUnidCarga.Free;
  inherited;
end;

procedure TInfOutrosCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspOutrosCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TInfOutrosCollectionItem.SetinfUnidCarga(
  const Value: TinfUnidCargaOutrosCollection);
begin
  FinfUnidCarga := Value;
end;

{ TinfUnidTranspOutrosCollection }

function TinfUnidTranspOutrosCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspOutrosCollection.Create(
  AOwner: TinfOutrosCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspOutrosCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspOutrosCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

{ TinfUnidCargaNFeCollection }

function TinfUnidCargaOutrosCollection.Add: TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidCargaOutrosCollection.Create(AOwner: TinfOutrosCollectionItem);
begin
  inherited Create(TinfUnidCargaCollectionItem);
end;

function TinfUnidCargaOutrosCollection.GetItem(
  Index: Integer): TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidCargaOutrosCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDocAnt }

constructor TDocAnt.Create(AOwner: TInfCTeNorm);
begin
  FemiDocAnt := TEmiDocAntCollection.Create(Self);
end;

destructor TDocAnt.Destroy;
begin
  FemiDocAnt.Free;
  inherited;
end;

procedure TDocAnt.SetEmiDocAnt(const Value: TEmiDocAntCollection);
begin
 FEmiDocAnt.Assign(Value);
end;

{ TEmiDocAntCollection }

function TEmiDocAntCollection.Add: TEmiDocAntCollectionItem;
begin
  Result := TemiDocAntCollectionItem(inherited Add);
  Result.create;
end;

constructor TEmiDocAntCollection.Create(AOwner: TDocAnt);
begin
  inherited Create(TemiDocAntCollectionItem);
end;

function TEmiDocAntCollection.GetItem(
  Index: Integer): TEmiDocAntCollectionItem;
begin
  Result := TemiDocAntCollectionItem(inherited GetItem(Index));
end;

procedure TEmiDocAntCollection.SetItem(Index: Integer;
  Value: TEmiDocAntCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TEmiDocAntCollectionItem }

constructor TEmiDocAntCollectionItem.Create;
begin
 FidDocAnt := TidDocAntCollection.Create(Self);
end;

destructor TEmiDocAntCollectionItem.Destroy;
begin
  FidDocAnt.Free;
  inherited;
end;

procedure TEmiDocAntCollectionItem.SetIdDocAnt(
  const Value: TIdDocAntCollection);
begin
  FidDocAnt.Assign(Value);
end;

{ TIdDocAntCollection }

function TIdDocAntCollection.Add: TIdDocAntCollectionItem;
begin
  Result := TidDocAntCollectionItem(inherited Add);
  Result.create;
end;

constructor TIdDocAntCollection.Create(AOwner: TEmiDocAntCollectionItem);
begin
  inherited Create(TidDocAntCollectionItem);
end;

function TIdDocAntCollection.GetItem(
  Index: Integer): TIdDocAntCollectionItem;
begin
  Result := TidDocAntCollectionItem(inherited GetItem(Index));
end;

procedure TIdDocAntCollection.SetItem(Index: Integer;
  Value: TIdDocAntCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdDocAntCollectionItem }

constructor TIdDocAntCollectionItem.Create;
begin
 FidDocAntPap := TidDocAntPapCollection.Create(Self);
 FidDocAntEle := TidDocAntEleCollection.Create(Self);
end;

destructor TIdDocAntCollectionItem.Destroy;
begin
  FidDocAntPap.Free;
  FidDocAntEle.Free;
  inherited;
end;

procedure TIdDocAntCollectionItem.SetIdDocAntPap(
  const Value: TIdDocAntPapCollection);
begin
 FidDocAntPap.Assign(Value);
end;

procedure TIdDocAntCollectionItem.SetIdDocAntEle(
  const Value: TIdDocAntEleCollection);
begin
 FidDocAntEle.Assign(Value);
end;

{ TIdDocAntPapCollection }

function TIdDocAntPapCollection.Add: TIdDocAntPapCollectionItem;
begin
  Result := TidDocAntPapCollectionItem(inherited Add);
  Result.create;
end;

constructor TIdDocAntPapCollection.Create(AOwner: TIdDocAntCollectionItem);
begin
  inherited Create(TidDocAntPapCollectionItem);
end;

function TIdDocAntPapCollection.GetItem(
  Index: Integer): TIdDocAntPapCollectionItem;
begin
  Result := TidDocAntPapCollectionItem(inherited GetItem(Index));
end;

procedure TIdDocAntPapCollection.SetItem(Index: Integer;
  Value: TIdDocAntPapCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdDocAntPapCollectionItem }

constructor TIdDocAntPapCollectionItem.Create;
begin

end;

destructor TIdDocAntPapCollectionItem.Destroy;
begin

  inherited;
end;

{ TIdDocAntEleCollection }

function TIdDocAntEleCollection.Add: TIdDocAntEleCollectionItem;
begin
  Result := TidDocAntEleCollectionItem(inherited Add);
  Result.create;
end;

constructor TIdDocAntEleCollection.Create(AOwner: TIdDocAntCollectionItem);
begin
  inherited Create(TidDocAntEleCollectionItem);
end;

function TIdDocAntEleCollection.GetItem(
  Index: Integer): TIdDocAntEleCollectionItem;
begin
  Result := TidDocAntEleCollectionItem(inherited GetItem(Index));
end;

procedure TIdDocAntEleCollection.SetItem(Index: Integer;
  Value: TIdDocAntEleCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdDocAntEleCollectionItem }

constructor TIdDocAntEleCollectionItem.Create;
begin

end;

destructor TIdDocAntEleCollectionItem.Destroy;
begin

  inherited;
end;

{ TSegCollection }

function TSegCollection.Add: TSegCollectionItem;
begin
  Result := TSegCollectionItem(inherited Add);
  Result.create;
end;

constructor TSegCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TSegCollectionItem);
end;

function TSegCollection.GetItem(Index: Integer): TSegCollectionItem;
begin
  Result := TSegCollectionItem(inherited GetItem(Index));
end;

procedure TSegCollection.SetItem(Index: Integer;
  Value: TSegCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TSegCollectionItem }

constructor TSegCollectionItem.Create;
begin

end;

destructor TSegCollectionItem.Destroy;
begin

  inherited;
end;

{ TRodo }

constructor TRodo.Create(AOwner: TInfCTeNorm);
begin
  Focc     := TOccCollection.Create(Self);
  FvalePed := TValePedCollection.Create(Self);
  Fveic    := TVeicCollection.Create(Self);
  FlacRodo := TLacRodoCollection.Create(Self);
  Fmoto    := TMotoCollection.Create(Self);
end;

destructor TRodo.Destroy;
begin
  Focc.Free;
  FvalePed.Free;
  Fveic.Free;
  FlacRodo.Free;
  Fmoto.Free;
  inherited;
end;

procedure TRodo.SetOcc(const Value: TOccCollection);
begin
 Focc.Assign(Value);
end;

procedure TRodo.SetValePed(const Value: TValePedCollection);
begin
 FvalePed.Assign(Value);
end;

procedure TRodo.SetVeic(const Value: TVeicCollection);
begin
 Fveic.Assign(Value);
end;

procedure TRodo.SetLacRodo(const Value: TLacRodoCollection);
begin
  FlacRodo.Assign(Value);
end;

procedure TRodo.SetMoto(const Value: TMotoCollection);
begin
 Fmoto.Assign(Value);
end;

{ TOccCollection }

function TOccCollection.Add: TOccCollectionItem;
begin
  Result := TOccCollectionItem(inherited Add);
  Result.create;
end;

constructor TOccCollection.Create(AOwner: TRodo);
begin
  inherited Create(TOccCollectionItem);
end;

function TOccCollection.GetItem(Index: Integer): TOccCollectionItem;
begin
  Result := TOccCollectionItem(inherited GetItem(Index));
end;

procedure TOccCollection.SetItem(Index: Integer;
  Value: TOccCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TOccCollectionItem }

constructor TOccCollectionItem.Create;
begin
  FemiOCC := TEmiOCC.Create;
end;

destructor TOccCollectionItem.Destroy;
begin
  FemiOCC.Free;
  inherited;
end;

{ TValePedCollection }

function TValePedCollection.Add: TValePedCollectionItem;
begin
  Result := TValePedCollectionItem(inherited Add);
end;

constructor TValePedCollection.Create(AOwner: TRodo);
begin
  inherited Create(TValePedCollectionItem);
end;

function TValePedCollection.GetItem(
  Index: Integer): TValePedCollectionItem;
begin
  Result := TValePedCollectionItem(inherited GetItem(Index));
end;

procedure TValePedCollection.SetItem(Index: Integer;
  Value: TValePedCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TValePedCollectionItem }

constructor TValePedCollectionItem.Create;
begin

end;

destructor TValePedCollectionItem.Destroy;
begin

  inherited;
end;

{ TVeicCollection }

function TVeicCollection.Add: TVeicCollectionItem;
begin
  Result := TVeicCollectionItem(inherited Add);
  Result.create;
end;

constructor TVeicCollection.Create(AOwner: TRodo);
begin
  inherited Create(TVeicCollectionItem);
end;

function TVeicCollection.GetItem(Index: Integer): TVeicCollectionItem;
begin
  Result := TVeicCollectionItem(inherited GetItem(Index));
end;

procedure TVeicCollection.SetItem(Index: Integer;
  Value: TVeicCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TVeicCollectionItem }

constructor TVeicCollectionItem.Create;
begin
  Fprop := TProp.Create;
end;

destructor TVeicCollectionItem.Destroy;
begin
  Fprop.Free;
  inherited;
end;

{ TLacRodoCollection }

function TLacRodoCollection.Add: TLacRodoCollectionItem;
begin
  Result := TLacRodoCollectionItem(inherited Add);
  Result.create;
end;

constructor TLacRodoCollection.Create(AOwner: TRodo);
begin
  inherited Create(TLacRodoCollectionItem);
end;

function TLacRodoCollection.GetItem(
  Index: Integer): TLacRodoCollectionItem;
begin
  Result := TLacRodoCollectionItem(inherited GetItem(Index));
end;

procedure TLacRodoCollection.SetItem(Index: Integer;
  Value: TLacRodoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TLacRodoCollectionItem }

constructor TLacRodoCollectionItem.Create;
begin

end;

destructor TLacRodoCollectionItem.Destroy;
begin

  inherited;
end;

{ TMotoCollection }

function TMotoCollection.Add: TMotoCollectionItem;
begin
  Result := TMotoCollectionItem(inherited Add);
  Result.create;
end;

constructor TMotoCollection.Create(AOwner: TRodo);
begin
  inherited Create(TMotoCollectionItem);
end;

function TMotoCollection.GetItem(Index: Integer): TMotoCollectionItem;
begin
  Result := TMotoCollectionItem(inherited GetItem(Index));
end;

procedure TMotoCollection.SetItem(Index: Integer;
  Value: TMotoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TMotoCollectionItem }

constructor TMotoCollectionItem.Create;
begin

end;

destructor TMotoCollectionItem.Destroy;
begin

  inherited;
end;

{ TAereo }

constructor TAereo.Create(AOwner: TInfCTeNorm);
begin
  Ftarifa   := TTarifa.Create;
  FnatCarga := TNatCarga.Create(Self);
end;

destructor TAereo.Destroy;
begin
  Ftarifa.Free;
  FnatCarga.Free;
  inherited;
end;

constructor TNatCarga.Create(AOwner: TAereo);
begin
   FcinfManu := TpInfManuCollection.Create(Self);
end;

destructor TNatCarga.Destroy;
begin
  FcinfManu.Free;
  inherited Destroy;
end;

procedure TNatCarga.SetcinfManu(const Value: TpInfManuCollection);
begin
  FcInfManu.Assign(Value);
end;

{ TpInfManuCollection }

function TpInfManuCollection.Add: TpInfManuCollectionItem;
begin
  Result := TpInfManuCollectionItem(inherited Add);
  Result.create;
end;

constructor TpInfManuCollection.Create(AOwner: TNatCarga);
begin
  inherited Create(TpInfManuCollectionItem);
end;

function TpInfManuCollection.GetItem(Index: Integer): TpInfManuCollectionItem;
begin
  Result := TpInfManuCollectionItem(inherited GetItem(Index));
end;

procedure TpInfManuCollection.SetItem(Index: Integer; Value: TpInfManuCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TpInfManuCollectionItem }

constructor TpInfManuCollectionItem.Create;
begin

end;

destructor TpInfManuCollectionItem.Destroy;
begin

  inherited;
end;

{ TAquav }

constructor TAquav.Create(AOwner: TInfCTeNorm);
begin
 Fbalsa   := TBalsaCollection.Create(Self);
 FdetCont := TdetContCollection.Create(Self);
end;

destructor TAquav.Destroy;
begin
  Fbalsa.Free;
  FdetCont.Free;
  inherited;
end;

procedure TAquav.SetBalsa(const Value: TbalsaCollection);
begin
  Fbalsa.Assign(Value);
end;

procedure TAquav.SetdetCont(const Value: TdetContCollection);
begin
  FdetCont := Value;
end;

{ TBalsaCollection }

function TBalsaCollection.Add: TBalsaCollectionItem;
begin
  Result := TBalsaCollectionItem(inherited Add);
  Result.create;
end;

constructor TBalsaCollection.Create(AOwner: TAquav);
begin
  inherited Create(TBalsaCollectionItem);
end;

function TBalsaCollection.GetItem(Index: Integer): TBalsaCollectionItem;
begin
  Result := TBalsaCollectionItem(inherited GetItem(Index));
end;

procedure TBalsaCollection.SetItem(Index: Integer;
  Value: TBalsaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TBalsaCollectionItem }

constructor TBalsaCollectionItem.Create;
begin

end;

destructor TBalsaCollectionItem.Destroy;
begin

  inherited;
end;

{ TdetContCollection }

function TdetContCollection.Add: TdetContCollectionItem;
begin
  Result := TdetContCollectionItem(inherited Add);
  Result.create;
end;

constructor TdetContCollection.Create(AOwner: TAquav);
begin
  inherited Create(TdetContCollectionItem);
end;

function TdetContCollection.GetItem(
  Index: Integer): TdetContCollectionItem;
begin
  Result := TdetContCollectionItem(inherited GetItem(Index));
end;

procedure TdetContCollection.SetItem(Index: Integer;
  Value: TdetContCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdetContCollectionItem }

constructor TdetContCollectionItem.Create;
begin
 FLacre  := TLacreCollection.Create(Self);
 FinfDoc := TInfDocAquav.Create(Self);
end;

destructor TdetContCollectionItem.Destroy;
begin
  FLacre.Free;
  FinfDoc.Free;
  inherited;
end;

procedure TdetContCollectionItem.SetLacre(const Value: TLacreCollection);
begin
  FLacre := Value;
end;

{ TLacreCollection }

function TLacreCollection.Add: TLacreCollectionItem;
begin
  Result := TLacreCollectionItem(inherited Add);
  Result.create;
end;

constructor TLacreCollection.Create(AOwner: TdetContCollectionItem);
begin
  inherited Create(TLacreCollectionItem);
end;

function TLacreCollection.GetItem(Index: Integer): TLacreCollectionItem;
begin
  Result := TLacreCollectionItem(inherited GetItem(Index));
end;

procedure TLacreCollection.SetItem(Index: Integer;
  Value: TLacreCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TLacreCollectionItem }

constructor TLacreCollectionItem.Create;
begin

end;

destructor TLacreCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfDocAquav }

constructor TInfDocAquav.Create(AOwner: TdetContCollectionItem);
begin
 FinfNF  := TinfNFAquavCollection.Create(Self);
 FinfNFe := TinfNFeAquavCollection.Create(Self);
end;

destructor TInfDocAquav.Destroy;
begin
  FinfNF.Free;
  FinfNFe.Free;
  inherited;
end;

procedure TInfDocAquav.SetInfNFAquav(const Value: TInfNFAquavCollection);
begin
  FinfNF := Value;
end;

procedure TInfDocAquav.SetInfNFeAquav(const Value: TInfNFeAquavCollection);
begin
  FinfNFe := Value;
end;

{ TInfNFAquavCollection }

function TInfNFAquavCollection.Add: TInfNFAquavCollectionItem;
begin
  Result := TinfNFAquavCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFAquavCollection.Create(AOwner: TInfDocAquav);
begin
  inherited Create(TinfNFAquavCollectionItem);
end;

function TInfNFAquavCollection.GetItem(
  Index: Integer): TInfNFAquavCollectionItem;
begin
  Result := TinfNFAquavCollectionItem(inherited GetItem(Index));
end;

procedure TInfNFAquavCollection.SetItem(Index: Integer;
  Value: TInfNFAquavCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFAquavCollectionItem }

constructor TInfNFAquavCollectionItem.Create;
begin

end;

destructor TInfNFAquavCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfNFeAquavCollection }

function TInfNFeAquavCollection.Add: TInfNFeAquavCollectionItem;
begin
  Result := TinfNFeAquavCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFeAquavCollection.Create(AOwner: TInfDocAquav);
begin
  inherited Create(TinfNFeAquavCollectionItem);
end;

function TInfNFeAquavCollection.GetItem(
  Index: Integer): TInfNFeAquavCollectionItem;
begin
  Result := TinfNFeAquavCollectionItem(inherited GetItem(Index));
end;

procedure TInfNFeAquavCollection.SetItem(Index: Integer;
  Value: TInfNFeAquavCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFeAquavCollectionItem }

constructor TInfNFeAquavCollectionItem.Create;
begin

end;

destructor TInfNFeAquavCollectionItem.Destroy;
begin

  inherited;
end;

//////////////////////////////////////////////////////////////////////////////
{ TFerrov }

constructor TFerrov.Create(AOwner: TInfCTeNorm);
begin
  inherited Create;
  FtrafMut  := TTrafMut.Create;
  FferroEnv := TFerroEnvCollection.Create(self);
  FdetVag   := TDetVagCollection.Create(Self);
end;

destructor TFerrov.Destroy;
begin
  FtrafMut.Free;
  FferroEnv.Free;
  FdetVag.Free;
  inherited;
end;

procedure TFerrov.SetFerroEnv(const Value: TFerroEnvCollection);
begin
 FferroEnv.Assign(Value);
end;

procedure TFerrov.SetDetVag(const Value: TDetVagCollection);
begin
 FdetVag.Assign(Value);
end;

{ TFerroEnvCollection }

function TFerroEnvCollection.Add: TFerroEnvCollectionItem;
begin
  Result := TFerroEnvCollectionItem(inherited Add);
  Result.create;
end;

constructor TFerroEnvCollection.Create(AOwner: TFerrov);
begin
  inherited Create(TFerroEnvCollectionItem);
end;

function TFerroEnvCollection.GetItem(
  Index: Integer): TFerroEnvCollectionItem;
begin
  Result := TFerroEnvCollectionItem(inherited GetItem(Index));
end;

procedure TFerroEnvCollection.SetItem(Index: Integer;
  Value: TFerroEnvCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TFerroEnvCollectionItem }

constructor TFerroEnvCollectionItem.Create;
begin
  FenderFerro := TEnderFerro.Create;
end;

destructor TFerroEnvCollectionItem.Destroy;
begin
  FenderFerro.Free;
  inherited;
end;

{ TDetVagCollection }

function TDetVagCollection.Add: TDetVagCollectionItem;
begin
  Result := TDetVagCollectionItem(inherited Add);
  Result.create;
end;

constructor TDetVagCollection.Create(AOwner: TFerrov);
begin
  inherited Create(TDetVagCollectionItem);
end;

function TDetVagCollection.GetItem(Index: Integer): TDetVagCollectionItem;
begin
  Result := TDetVagCollectionItem(inherited GetItem(Index));
end;

procedure TDetVagCollection.SetItem(Index: Integer;
  Value: TDetVagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDetVagCollectionItem }

constructor TDetVagCollectionItem.Create;
begin

end;

destructor TDetVagCollectionItem.Destroy;
begin

  inherited;
end;

{ TPeriCollection }

function TPeriCollection.Add: TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited Add);
  Result.create;
end;

constructor TPeriCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TPeriCollectionItem);
end;

function TPeriCollection.GetItem(Index: Integer): TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited GetItem(Index));
end;

procedure TPeriCollection.SetItem(Index: Integer;
  Value: TPeriCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TPeriCollectionItem }

constructor TPeriCollectionItem.Create;
begin

end;

destructor TPeriCollectionItem.Destroy;
begin

  inherited;
end;

{ TVeicNovosCollection }

function TVeicNovosCollection.Add: TVeicNovosCollectionItem;
begin
  Result := TVeicNovosCollectionItem(inherited Add);
  Result.create;
end;

constructor TVeicNovosCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TVeicNovosCollectionItem);
end;

function TVeicNovosCollection.GetItem(
  Index: Integer): TVeicNovosCollectionItem;
begin
  Result := TVeicNovosCollectionItem(inherited GetItem(Index));
end;

procedure TVeicNovosCollection.SetItem(Index: Integer;
  Value: TVeicNovosCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TVeicNovosCollectionItem }

constructor TVeicNovosCollectionItem.Create;
begin

end;

destructor TVeicNovosCollectionItem.Destroy;
begin

  inherited;
end;

{ TCobr }

constructor TCobr.Create(AOwner: TInfCTeNorm);
begin
  inherited Create;
  Ffat := TFat.Create;
  Fdup := TDupCollection.Create(self);
end;

destructor TCobr.Destroy;
begin
  Ffat.Free;
  Fdup.Free;
  inherited;
end;

procedure TCobr.SetDup(Value: TDupCollection);
begin
  Fdup.Assign(Value);
end;

{ TDupCollection }

function TDupCollection.Add: TDupCollectionItem;
begin
  Result := TDupCollectionItem(inherited Add);
  Result.create;
end;

constructor TDupCollection.Create(AOwner: TCobr);
begin
  inherited Create(TDupCollectionItem);
end;

function TDupCollection.GetItem(Index: Integer): TDupCollectionItem;
begin
  Result := TDupCollectionItem(inherited GetItem(Index));
end;

procedure TDupCollection.SetItem(Index: Integer;
  Value: TDupCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDupCollectionItem }

constructor TDupCollectionItem.Create;
begin

end;

destructor TDupCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfCteSub }

constructor TInfCteSub.Create(AOwner: TInfCTeNorm);
begin
  FtomaICMS    := TTomaICMS.Create(Self);
  FtomaNaoICMS := TTomaNaoICMS.Create;
  FindAlteraToma := tiNao;
end;

destructor TInfCteSub.Destroy;
begin
  FtomaICMS.Free;
  FtomaNaoICMS.Free;
  inherited;
end;

{ TTomaICMS }

constructor TTomaICMS.Create(AOwner: TInfCteSub);
begin
  FrefNF := TRefNF.Create;
end;

destructor TTomaICMS.Destroy;
begin
  FrefNF.Free;
  inherited;
end;

{ TInfCteComp }

constructor TInfCteComp.Create(AOwner: TCTe);
begin
//  FvPresComp := TVPresComp.Create(Self);
//  FimpComp   := TImpComp.Create(Self);
end;

destructor TInfCteComp.Destroy;
begin
//  FvPresComp.Free;
//  FimpComp.Free;
  inherited;
end;

{ TVPresComp }

constructor TVPresComp.Create(AOwner: TInfCteComp);
begin
 FcompComp := TCompCompCollection.Create(Self);
end;

destructor TVPresComp.Destroy;
begin
  FcompComp.Free;
  inherited;
end;

procedure TVPresComp.SetCompCompItem(const Value: TCompCompCollection);
begin
  FcompComp.Assign(Value);
end;

{ TCompCompCollection }

function TCompCompCollection.Add: TCompCompCollectionItem;
begin
  Result := TCompCompCollectionItem(inherited Add);
  Result.create;
end;

constructor TCompCompCollection.Create(AOwner: TVPresComp);
begin
  inherited Create(TCompCompCollectionItem);
end;

function TCompCompCollection.GetItem(
  Index: Integer): TCompCompCollectionItem;
begin
  Result := TCompCompCollectionItem(inherited GetItem(Index));
end;

procedure TCompCompCollection.SetItem(Index: Integer;
  Value: TCompCompCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCompCompCollectionItem }

constructor TCompCompCollectionItem.Create;
begin

end;

destructor TCompCompCollectionItem.Destroy;
begin

  inherited;
end;

{ TImpComp }

constructor TImpComp.Create(AOwner: TInfCteComp);
begin
  FICMSComp := TICMSComp.Create(Self);
end;

destructor TImpComp.Destroy;
begin
  FICMSComp.Free;
  inherited;
end;

{ TICMSComp }

constructor TICMSComp.Create(AOwner: TImpComp);
begin
  inherited Create;
  FCST00       := TCST00.create;
  FCST20       := TCST20.create;
  FCST45       := TCST45.create;
  FCST60       := TCST60.create;
  FCST90       := TCST90.create;
  FICMSOutraUF := TICMSOutraUF.Create;
  FICMSSN      := TICMSSN.Create;
end;

destructor TICMSComp.Destroy;
begin
  FCST00.Free;
  FCST20.Free;
  FCST45.Free;
  FCST60.Free;
  FCST90.Free;
  FICMSOutraUF.Free;
  FICMSSN.Free;
  inherited;
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Add);
  Result.create;
end;

constructor TautXMLCollection.Create(AOwner: TCTe);
begin
  inherited Create(TautXMLCollectionItem);
end;

function TautXMLCollection.GetItem(Index: Integer): TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited GetItem(Index));
end;

procedure TautXMLCollection.SetItem(Index: Integer;
  Value: TautXMLCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TautXMLCollectionItem }

constructor TautXMLCollectionItem.Create;
begin

end;

destructor TautXMLCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfCTeMultimodalCollectionItem }

constructor TInfCTeMultimodalCollectionItem.Create;
begin

end;

destructor TInfCTeMultimodalCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfCTeMultimodalCollection }

function TInfCTeMultimodalCollection.Add: TInfCTeMultimodalCollectionItem;
begin
  Result := TInfCTeMultimodalCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfCTeMultimodalCollection.Create(AOwner: TInfServVinc);
begin
  inherited Create(TInfCTeMultimodalCollectionItem);
end;

function TInfCTeMultimodalCollection.GetItem(
  Index: Integer): TInfCTeMultimodalCollectionItem;
begin
  Result := TInfCTeMultimodalCollectionItem(inherited GetItem(Index));
end;

procedure TInfCTeMultimodalCollection.SetItem(Index: Integer;
  Value: TInfCTeMultimodalCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfServVinc }

constructor TInfServVinc.Create(AOwner: TInfCTeNorm);
begin
  FinfCTeMultimodal := TinfCTeMultimodalCollection.Create(Self);
end;

destructor TInfServVinc.Destroy;
begin
  FinfCTeMultimodal.Free;
  inherited;
end;

procedure TInfServVinc.SetinfCTeMultimodal(
  const Value: TinfCTeMultimodalCollection);
begin
  FinfCTeMultimodal := Value;
end;

{ TToma }

constructor TToma.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderToma := TEndereco.Create;
end;

destructor TToma.Destroy;
begin
  FEnderToma.Free;
  inherited;
end;

{ TinfDocRefCollectionItem }

constructor TinfDocRefCollectionItem.Create;
begin

end;

destructor TinfDocRefCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfDocRefCollection }

function TinfDocRefCollection.Add: TinfDocRefCollectionItem;
begin
  Result := TinfDocRefCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfDocRefCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TinfDocRefCollectionItem);
end;

function TinfDocRefCollection.GetItem(
  Index: Integer): TinfDocRefCollectionItem;
begin
  Result := TinfDocRefCollectionItem(inherited GetItem(Index));
end;

procedure TinfDocRefCollection.SetItem(Index: Integer;
  Value: TinfDocRefCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TVeicOS }

constructor TVeicOS.Create(AOwner: TRodoOS);
begin
  Fprop := TPropOS.Create;
end;

destructor TVeicOS.Destroy;
begin
  Fprop.Free;

  inherited;
end;

{ TRodoOS }

constructor TRodoOS.Create(AOwner: TInfCTeNorm);
begin
  Fveic := TVeicOS.Create(Self);
end;

destructor TRodoOS.Destroy;
begin
  Fveic.Free;

  inherited;
end;

{$ENDIF}

end.

